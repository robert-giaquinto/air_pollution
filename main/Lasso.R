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
    all_results <- vector("list", num_windows)
    train_start <- min_date
    for (window in 1:num_windows) {
        if (verbose) {
            test_start <- train_start %m+% months(training_months)
            test_end <- test_start %m+% months(testing_months)
            cat(paste0("\n  WINDOW ", window,
                ". Train: [", train_start, ", ", test_start,
                "). Test: [", test_start, ", ", test_end, ")"))
        }

        # split the training dataset into the data to model, and the cross-validation set
        date_splits <- split_dates(ymd(DF$date_key), train_start, training_months, testing_months)
        model_set <- DF[date_splits$in_train, model_vars]
        future_set <- DF[date_splits$in_test, model_vars]

        # save location ids
        future_keys <- DF[date_splits$in_test, c("location_key", "datetime_key")]

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
        future_results <- cbind(future_keys,
            data.frame(window=rep(window, nrow(future_predict)),
                actual=future_set[,tar_var]),
            future_predict,
            null_model,
            future_error,
            future_error_null)
        names(future_results) <- c("location_key", "datetime_key", "window", "actual",
            paste0("predict", 1:num_lambdas),
            "predict_null",
            paste0("error", 1:num_lambdas),
            "error_null")
        all_results[[window]] <- future_results

        # increment the start date
        train_start <- train_start %m+% months(testing_months)
    }
    # combine all windows
    all_hourly_df  <- bind_rows(all_results)
    rm(all_results)

    # summarize RMSE based on locations and prediction windows
    error_names <- c(paste0("error", 1:num_lambdas), "error_null")
    rmses <- all_hourly_df[,c("location_key", "window", error_names)] %>%
        group_by(location_key, window) %>%
        summarise_each(funs(rmse))
    # join the lambda value to each rmse
    rmse_long <- melt(rmses, id.vars=c("location_key", "window"))
    names(rmse_long) <- c("location_key", "window", "error_id", "rmse")
    rmse_long$lambda_id <- str_replace(rmse_long$error_id, fixed("error"), "lambda")
    lambda_df <- data.frame(lambda_id=paste0("lambda", 1:num_lambdas),
        lambda_val=lambda_sequence(num_lambdas))
    window_rmse <- left_join(x=rmse_long, y=lambda_df, by="lambda_id")
    window_rmse$error_id <- NULL

    # find overall rmse aggregated by lambdas
    temp <- all_hourly_df[,error_names]
    temp$set <- "future"
    rmses <- temp %>% group_by(set) %>% summarise_each(funs(rmse))
    # join the lambda value to each rmse
    rmse_long <- melt(rmses)
    names(rmse_long) <- c("set", "error_id", "rmse")
    rmse_long$set <- NULL
    rmse_long$lambda_id <- str_replace(rmse_long$error_id, fixed("error"), "lambda")
    rmse_df <- left_join(x=rmse_long, y=lambda_df, by="lambda_id")
    rmse_df$error_id <- NULL

    # use rmse to find best lambda
    null_rmse <-  rmse_df[rmse_df$lambda_id == "lambda_null", "rmse"]
    best_rmse <- min(rmse_df$rmse)
    best_lambda <- rmse_df[rmse_df$rmse == best_rmse &
            rmse_df$lambda_id != "lambda_null", "lambda_val"]
    rval <- list(rmse_df=rmse_df,
        window_rmse=window_rmse,
        all_hourly_df=all_hourly_df,
        best_lambda=best_lambda,
        best_rmse=best_rmse,
        null_rmse=null_rmse,
        num_lambdas=num_lambdas)
    class(rval) <- "Lasso"
    return(rval)
}


plot_error_curve <- function(x) {
    UseMethod("plot_error_curve", x)
}
plot_error_curve.Lasso <- function(object, ...) {
    require(ggplot2)
    require(dplyr)
    require(stringr)

    if (class(object) != "Lasso")
        stop("must provide object of type Lasso")

    # unpack the Lasso object
    rmse_df <- object$rmse_df

    # split full and null models
    rmse_set <- rmse_df[rmse_df$lambda_id != "lambda_null",]
    null_set <- rmse_df[rmse_df$lambda_id == "lambda_null",]

    # identify the best value of lambda
    rmse_set$highlight <- factor(ifelse(rmse_set$lambda_val == object$best_lambda, "Optimal", "Sub-Optimal"))

    # plot the mean absolute rmse
    mycolours <- c("Optimal" = "red", "Sub-Optimal" = "grey50")
    null_label <- paste0("Null RMSE: ", round(null_set$rmse,2))
    rval <- ggplot(rmse_set, aes(x=lambda_val, y=rmse)) +
        annotate("text", x=quantile(rmse_set$lambda_val, probs=.25),
            y=quantile(rmse_set$rmse, probs=.99),
            label=null_label) +
        geom_point(aes(colour=highlight), size=2.5) +
        geom_line() +
        scale_color_manual("Overall Performance: ", values=mycolours) +
        scale_x_log10(breaks=c(0.001, .01, .1, 1)) +
        coord_trans(y = "log10") +
        labs(x="Lambda",
            y="RMSE",
            title="RMSE Across Moving Windows\nFor Each Lambda") +
        theme_bw() +
        theme(legend.position="bottom")
    return(rval)
}


plot_error_distribution <- function(x, conditional) {
    UseMethod("plot_error_distribution", x)
}
plot_error_distribution.Lasso <- function(object, conditional, ...) {
    if (class(object) != "Lasso")
        stop("must provide object of type Lasso")
    plot_df <- object$window_rmse
    plot_df[,conditional] <- factor(plot_df[,conditional])
    plot_title <- paste0("Distribution of Error By ", conditional)
    out_plot <- ggplot(plot_df, aes_string(x="rmse", colour=conditional)) +
        geom_density() +
        labs(x="RMSE",
            y="Density",
            title=plot_title) +
        theme_bw() +
        theme(legend.position="none")
    plot_caption <- paste0("Color of lines represents distribution of RMSE for each prediction ",
        str_replace_all(conditional, fixed("_"), "\\_"),
        ". Variability comes from different RMSE's in each ",
        ifelse(conditional == "window", "location", "window"), ".")
    rval <- list(out_plot=out_plot, plot_caption=plot_caption)
    return(rval)
}



plot_spatial_correlation <- function(x, n_breaks) {
    UseMethod("plot_spatial_correlation", x)
}
plot_spatial_correlation <- function(object, n_breaks=10, ...) {
    key_vars <- c("location_key", "datetime_key", "window", "actual")
    best_lambda_index <- which(object$best_lambda == lambda_sequence(num_lambdas))
    keep_vars <- c(key_vars, paste0("error", best_lambda_index))
    best_errors <- as.data.frame(object$all_hourly_df[,keep_vars])
    names(best_errors) <- c(key_vars, "error")

    # put the errors in wide format
    temp <- best_errors %>% group_by(location_key, datetime_key) %>% summarise(RMSE = rmse(error))
    wide_error <- dcast(temp, datetime_key~location_key, value.var="RMSE")
    # if there wasn't a prediction made at a certain time for a location_key
    # drop the the row from all other locations too
    wide_error <- wide_error[complete.cases(wide_error), ]
    wide_error$datetime_key <- NULL

    error_cor <- cor(wide_error)
    cor_df <- melt(error_cor)
    cor_df$Location_X <- factor(cor_df$Var1)
    cor_df$Location_Y <- factor(cor_df$Var2)
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


