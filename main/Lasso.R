# Master lasso class
# used for training and evaluating the lasso

Lasso <- function(DF, var_list, num_lambdas=50,
    testing_months=3, training_months=12,
    verbose=FALSE)
{
    require(glmnet)
    require(glmnetUtils)

    # define a range of lambdas to test different regularization
    lambdas <- lambda_sequence(num_lambdas)

    # unpack variable list
    tar_var <- var_list$tar_var
    agg_vars <- var_list$agg_vars
    lag_vars <- var_list$lag_vars
    date_vars <- var_list$date_vars
    model_vars <- c(tar_var, date_vars, lag_vars, agg_vars)

    # split up training and test locations
    location_splits <- split_locations(DF$location_key, 1)
    num_locs <- length(location_splits)

    # Determine the number of sliding windows to capture results from
    dates <- ymd(DF$date_key)
    min_date <- min(dates)
    max_date <- max(dates)
    num_windows <- count_sliding_windows(min_date, max_date, training_months, testing_months)
    if (verbose) {
        cat(paste0("\nTraining LASSO on ", training_months, " months of data.\n"))
        cat(paste0("Predictions from ", min_date %m+% months(training_months), " to ", max_date,
            " split into ", num_windows,
            " windows, each ", testing_months," months\n"))
        cat(paste(rep("-", 80), collapse=""))
    }

    # initialize containers to store results
    all_rmse <- vector("list", num_locs)
    all_results <- vector("list", num_locs)
    for (loc in 1:num_locs) {
        # split data into test and training based on the location
        train_start <- min_date
        known_df <- DF[location_splits[[loc]],]
        unknown_df <- DF[-location_splits[[loc]],]

        if (verbose)
            cat(paste0("\n\nLOCATION ", loc, "."))

        # for each window store the average prediction error
        hourly_results <- vector("list", num_windows)
        for (window in 1:num_windows) {
            if (verbose) {
                test_start <- train_start %m+% months(training_months)
                test_end <- test_start %m+% months(testing_months)
                cat(paste0("\n  WINDOW ", window,
                    ". Train: [", train_start, ", ", test_start,
                    "). Test: [", test_start, ", ", test_end, ")"))
            }

            # split the training dataset into the data to model, and the cross-validation set
            known_splits <- split_dates(ymd(known_df$date_key),
                train_start, training_months, testing_months)
            model_set <- known_df[known_splits$in_train, model_vars]
            future_set <- known_df[known_splits$in_test, model_vars]

            # pull out future data from unknow location
            unknown_splits <- split_dates(ymd(unknown_df$date_key),
                train_start, training_months, testing_months)
            unknown_set <- unknown_df[unknown_splits$in_test, model_vars]

            # save location ids
            future_loc_keys <- known_df[known_splits$in_test, "location_key"]
            unknown_loc_keys <- unknown_df[unknown_splits$in_test, "location_key"]
            future_dt_keys <- known_df[known_splits$in_test, "datetime_key"]
            unknown_dt_keys <- unknown_df[unknown_splits$in_test, "datetime_key"]

            # train model on all combinations of interactions
            lasso_formula <- as.formula(
                  paste0(tar_var, " ~ "
                      , ifelse(is.null(date_vars), "", paste(date_vars, collapse="+"))
                      , ifelse(is.null(lag_vars), "",  paste0("+(", paste(lag_vars, collapse="+"), ")^2"))
                      , ifelse(is.null(agg_vars), "",  paste0("+(", paste(agg_vars, collapse="+"), ")^2"))))
            lasso <- glmnet(formula=lasso_formula,
                data=model_set, family="gaussian", lambda=lambdas)
            null_model <- mean(model_set[,tar_var])

            # predict on the future window for training locations (i.e. future set)
            future_predict <- predict(lasso, newdata=future_set, s=lambdas, type="response")
            future_error <- future_set[,tar_var] - future_predict
            future_error_null <- future_set[,tar_var] - null_model

            future_results <- cbind(
                data.frame(location=future_loc_keys, datetime_key=future_dt_keys, window,
                    set="future",
                    actual=future_set[,tar_var]),
                future_predict,
                null_model,
                future_error,
                future_error_null
            )
            names(future_results) <- c("location", "datetime_key", "window", "set", "actual",
                paste0("predict", 1:num_lambdas),
                "predict_null",
                paste0("error", 1:num_lambdas),
                "error_null")
            # predict on furure window for unknown location (unknown set)
            unknown_predict <- predict(lasso, newdata=unknown_set, s=lambdas, type="response")
            unknown_error <- unknown_set[,tar_var] - unknown_predict
            unknown_error_null <- unknown_set[,tar_var] - null_model
            unknown_results <- cbind(
                data.frame(location=unknown_loc_keys, datetime_key=unknown_dt_keys, window,
                    set="unknown",
                    actual=unknown_set[,tar_var]),
                unknown_predict,
                null_model,
                unknown_error,
                unknown_error_null
            )
            names(unknown_results) <- c("location", "datetime_key", "window", "set", "actual",
                paste0("predict", 1:num_lambdas),
                "predict_null",
                paste0("error", 1:num_lambdas),
                "error_null")
            # combine results
            hourly_results[[window]] <- rbind(future_results, unknown_results)

            # increment the start date
            train_start <- train_start %m+% months(testing_months)
        }

        # combine results from each window
        all_results[[loc]] <- bind_rows(hourly_results)
    }

    # hourly level data:
    all_hourly_df  <- bind_rows(all_results)
    rm(all_results)

    # find rmse for each lambda
    error_names <- c(paste0("error", 1:num_lambdas), "error_null")
    temp <- all_hourly_df[,c("set", error_names)]
    rmses <- temp %>% group_by(set) %>% summarise_each(funs(rmse))
    # join the lambda value to each rmse
    rmse_long <- melt(rmses)
    names(rmse_long) <- c("set", "error_id", "rmse")
    rmse_long$lambda_id <- str_replace(rmse_long$error_id, fixed("error"), "lambda")
    lambda_df <- data.frame(lambda_id=paste0("lambda", 1:num_lambdas), lambda_val=lambdas)
    rmse_df <- left_join(x=rmse_long, y=lambda_df, by="lambda_id")
    rmse_df$error_id <- NULL

    # use rmse to find best lambda
    best_lambda <- rmse_df[rmse_df$rmse == min(rmse_df$rmse) &
            rmse_df$lambda_id != "lambda_null" &
            rmse_df$set == "future", "lambda_val"]
    rval <- list(rmse_df=rmse_df,
        all_hourly_df=all_hourly_df,
        best_lambda=best_lambda,
        num_lambdas=num_lambdas)
    class(rval) <- "Lasso"
    return(rval)
}


plot_error_curve <- function(x, set) {
    UseMethod("plot_error_curve", x)
}

plot_error_curve.Lasso <- function(object, set, ...) {
    require(ggplot2)
    require(dplyr)
    require(stringr)

    if (class(object) != "Lasso")
        stop("must provide object of type Lasso")

    # unpack the Lasso object
    rmse_df <- object$rmse_df

    # pull out the correct set
    rmse_set <- rmse_df[rmse_df$set == set & rmse_df$lambda_id != "lambda_null",]
    null_set <- rmse_df[rmse_df$set == set & rmse_df$lambda_id == "lambda_null",]

    # identify the best value of lambda for the specified set
    best_lambda <- rmse_set[rmse_set$rmse == min(rmse_set$rmse), "lambda_val"]
    rmse_set$highlight <- factor(ifelse(rmse_set$lambda_val == best_lambda,
        "Optimal",
        "Sub-Optimal"))
    plot_title <- paste0("RMSE Across Windows and Locations\nFor Each Lambda of ",
        str_to_title(set), " Set")

    # plot the mean absolute rmse
    mycolours <- c("Optimal" = "red", "Sub-Optimal" = "grey50")
    null_label <- paste0("Null RMSE: ", round(null_set$rmse,2))
    rval <- ggplot(rmse_set, aes(x=lambda_val, y=rmse)) +
#         geom_hline(aes(yintercept=rmse, group=1, lty="foo"), null_set, show_guide=TRUE) +
        annotate("text", x=quantile(rmse_set$lambda_val, probs=.25),
            y=quantile(rmse_set$rmse, probs=.99),
            label=null_label) +
        geom_point(aes(colour=highlight), size=2.5) +
        geom_line() +
        scale_color_manual("Status: ", values=mycolours) +
        scale_x_log10(breaks=c(0.001, .01, .1, 1)) +
        coord_trans(y = "log10") +
        labs(x="Lambda",
            y="RMSE",
            title=plot_title) +
        theme_bw() +
        theme(legend.position="bottom")
    return(rval)
}


plot_error_distribution <- function(x, split_by) {
    UseMethod("plot_error_distribution", x)
}
plot_error_distribution.Lasso <- function(object, split_by, ...) {
    if (class(object) != "Lasso")
        stop("must provide object of type Lasso")

    # unpack the Lasso object
    all_hourly_df <- object$all_hourly_df
    num_lambdas <- object$num_lambdas

    rmse <- all_hourly_df[, c("location", "window", "set", paste0("error", 1:num_lambdas))] %>%
        group_by(location, window, set) %>%
        summarise_each(funs(rmse))
    # join the lambda value to each rmse
    rmse_long <- melt(rmse, id.vars=c("location", "window", "set"))
    rmse_long$lambda_id <- str_replace(rmse_long$variable, fixed("error"), "lambda")
    lambda_df <- data.frame(lambda_id=paste0("lambda", 1:num_lambdas),
        lambda_val=lambda_sequence(num_lambdas))
    rmse_df <- left_join(x=rmse_long, y=lambda_df, by="lambda_id")
    best_errors <- rmse_df[rmse_df$lambda_val == object$best_lambda,]
    best_errors[,split_by] <- factor(best_errors[,split_by])

    plot_title <- paste0("Distribution of Error Across ", str_to_title(split_by))
    rval <- ggplot(best_errors, aes_string(x="value", colour=split_by)) +
        geom_density() +
        labs(x="RMSE",
            y="Density",
            title="Distribution of Error Across Locations") +
        theme_bw() +
        theme(legend.position="none") +
        facet_wrap(~set, scales="free_y", nrow=2)
    return(rval)
}


plot_spatial_correlation_Lasso <- function(best_errors, set) {
    # put the errors in wide format
    temp <- best_errors[best_errors$set == set, c("location", "datetime_key", "error")]
    temp <- temp %>% group_by(location, datetime_key) %>% summarise(RMSE = rmse(error))
    wide_error <- dcast(temp, datetime_key~location, value.var="RMSE")
    # if there wasn't a prediction made at a certain time for a location
    # drop the the row from all other locations too
    wide_error <- wide_error[complete.cases(wide_error), ]
    wide_error$datetime_key <- NULL

    error_cor <- cor(wide_error)
    cor_df <- melt(error_cor)
    cor_df$Location_X <- factor(cor_df$Var1)
    cor_df$Location_Y <- factor(cor_df$Var2)
    n_breaks <- 10
    cor_df$Correlation <- as.character(cut(cor_df$value,
            breaks=quantile(cor_df$value, probs=seq(0, 1 , length.out=n_breaks)),
        right=FALSE))
    cor_df$Correlation <- factor(ifelse(is.na(cor_df$Correlation), "1.0", cor_df$Correlation))

    mypalette <- c(brewer.pal(length(levels(cor_df$Correlation)) - 1,"Blues"), "grey50")
    rval <- ggplot(data = cor_df, aes(x=Location_X, y=Location_Y, fill=Correlation)) +
        geom_tile(colour="white") +
        theme_bw() +
        scale_fill_manual("Correlation", values=mypalette) +
        labs(title="Correlation of Errors: Validation Set",
            x="Location",
            y="Location")
    return(rval)
}


