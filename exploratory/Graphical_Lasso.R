# Master graphical lasso class
# used for training and evaluating the graphical lasso
Graphical_Lasso <- function(DF, var_list, num_lambdas=50,
    testing_months=3, training_months=12, verbose=FALSE, cov_methods=c("pearson", "spearman"))
{
    require(glasso)
    # define a range of lambdas to test different regularization
    lambdas <- lambda_sequence(num_lambdas, decreasing=FALSE)

    # unpack variable list
    tar_var <- var_list$tar_var
    agg_vars <- var_list$agg_vars
    lag_vars <- var_list$lag_vars
    date_vars <- var_list$date_vars
    model_vars <- c(date_vars, lag_vars, agg_vars, tar_var)

    # split up training and test locations
    location_splits <- split_locations(DF$location_key, 1)
    num_locs <- length(location_splits)

    # Determine the number of sliding windows to capture results from
    dates <- ymd(DF$date_key)
    min_date <- min(dates)
    max_date <- max(dates)
    num_windows <- count_sliding_windows(min_date, max_date, training_months, testing_months)
    if (verbose) {
        cat(paste0("\nTraining GLASSO on ", training_months, " months of data.\n"))
        cat(paste0("Predictions from ", min_date %m+% months(training_months), " to ", max_date,
            " split into ", num_windows,
            " windows, each ", testing_months," months\n"))
        cat(paste(rep("-", 80), collapse=""))
    }

    # initialize containers to store results
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

            # split the training dataset from known locations to training and future datasets
            known_splits <- split_dates(ymd(known_df$date_key),
                train_start, training_months, testing_months)
            model_set <- as.matrix(known_df[known_splits$in_train, model_vars])
            future_set <- as.matrix(known_df[known_splits$in_test, model_vars])
            p <- ncol(model_set) - 1

            # pull out future data from unknow location
            unknown_splits <- split_dates(ymd(unknown_df$date_key),
                train_start, training_months, testing_months)
            unknown_set <- as.matrix(unknown_df[unknown_splits$in_test, model_vars])

            # save location ids
            future_loc_keys <- known_df[known_splits$in_test, "location_key"]
            unknown_loc_keys <- unknown_df[unknown_splits$in_test, "location_key"]
            future_dt_keys <- known_df[known_splits$in_test, "datetime_key"]
            unknown_dt_keys <- unknown_df[unknown_splits$in_test, "datetime_key"]

            # train model on all combinations of interactions
            graphical_lasso <- train_glasso(x=model_set[,1:p], y=model_set[,p+1],
                lambdas,
                cov_methods=cov_methods,
                rescale=TRUE, standardize=TRUE)
            null_model <- mean(model_set[,p+1])

            # predict on the future window for training locations (i.e. future set)
            future_predict <- predict(graphical_lasso, newx=future_set[,1:p])
            future_list <- lapply(seq(dim(future_predict)[3]), function(x) {
                temp_predicts <- as.data.frame(future_predict[ , , x])
                names(temp_predicts) <- paste0("predict", 1:num_lambdas)
                temp_predicts$predict_null <- null_model

                temp_errors <- as.data.frame(future_set[,p+1] - future_predict[ , , x])
                names(temp_errors) <- paste0("error", 1:num_lambdas)
                temp_errors$error_null <- future_set[,p+1] - null_model

                temp_keys <- data.frame(location = future_loc_keys,
                    datetime_key = future_dt_keys,
                    window,
                    set = "future",
                    cov_method = cov_methods[x],
                    actual = future_set[,tar_var])
                rval <- cbind(temp_keys, temp_predicts, temp_errors)
                return(rval)
            })
            future_results <- bind_rows(future_list)

            # predict on the future window for test locations (i.e. unknown set)
            unknown_predict <- predict(graphical_lasso, newx=unknown_set[,1:p])
            unknown_list <- lapply(seq(dim(unknown_predict)[3]), function(x) {
                temp_predicts <- as.data.frame(unknown_predict[ , , x])
                names(temp_predicts) <- paste0("predict", 1:num_lambdas)
                temp_predicts$predict_null <- null_model

                temp_errors <- as.data.frame(unknown_set[,p+1] - unknown_predict[ , , x])
                names(temp_errors) <- paste0("error", 1:num_lambdas)
                temp_errors$error_null <- unknown_set[,p+1] - null_model

                temp_keys <- data.frame(location = unknown_loc_keys,
                    datetime_key = unknown_dt_keys,
                    window,
                    set = "unknown",
                    cov_method = cov_methods[x],
                    actual = unknown_set[,tar_var])
                rval <- cbind(temp_keys, temp_predicts, temp_errors)
                return(rval)
            })
            unknown_results <- bind_rows(unknown_list)

            # combine results
            hourly_results[[window]] <- rbind(future_results, unknown_results)

            # increment the start date and model id
            train_start <- train_start %m+% months(testing_months)
        }

        # combine results from each window
        all_results[[loc]] <- bind_rows(hourly_results)
    }

    # hourly level data:
    all_hourly_df  <- bind_rows(all_results)
    rm(all_results)

    # find rmse for each lambda by set and covariance method
    error_names <- c(paste0("error", 1:num_lambdas), "error_null")
    temp <- all_hourly_df[,c("set", "cov_method", error_names)]
    temp[,error_names] <- temp[,error_names]^2
    rmse <- temp %>% group_by(set, cov_method) %>% summarise_each(funs(sqrt_mean))
    # join the lambda value to each rmse
    rmse_long <- melt(rmse)
    names(rmse_long) <- c("set", "cov_method", "error_id", "rmse")
    rmse_long$lambda_id <- str_replace(rmse_long$error_id, fixed("error"), "lambda")
    lambda_df <- data.frame(lambda_id=paste0("lambda", 1:num_lambdas), lambda_val=lambdas)
    rmse_df <- left_join(x=rmse_long, y=lambda_df, by="lambda_id")
    rmse_df$error_id <- NULL

    # use rmse to find best lambda and covariance method
    best_parameters <- rmse_df[rmse_df$rmse == min(rmse_df$rmse) &
            rmse_df$lambda_id != "lambda_null" &
            rmse_df$set == "future", c("lambda_val", "cov_method")]
    best_lambda <- best_parameters[1,1]
    best_cov_method <- best_parameters[1,2]

    # package results
    rval <- list(rmse_df=rmse_df,
        all_hourly_df=all_hourly_df,
        best_lambda=best_lambda,
        best_cov_method=best_cov_method,
        num_lambdas=num_lambdas)
    class(rval) <- "Graphical_Lasso"
    return(rval)
}

# fit method for graphical lasso
train_glasso <- function(x, y, lambdas,
    cov_methods=c("pearson","spearman"), rescale=TRUE, standardize=TRUE, ...)
{
    require(glasso)
    # TODO:
    # check that x and y are valid

    # TODO:
    # check that lamdas are valid and increasing

    # center and scale the data
    meany <- mean(y)
    meanx <- colMeans(x)
    if(standardize){
        sdy <- sd(y)
        sdx <- apply(x, 2, sd)
        x <- scale(x, center=TRUE, scale=TRUE)
    } else {
        # center by don't scale
        x <- scale(x, center=TRUE, scale=FALSE)
        sdx <- rep(1, ncol(x))
        sdy <- 1
    }
    y <- (y - meany) / sdy

    num_lambdas <- length(lambdas)
    betamat <- array(NA, dim=c(ncol(x), num_lambdas, length(cov_methods)))
    for (i in 1:length(cov_methods)) {
        cat(paste0("\nTraining using method=", cov_methods[i], ":\nLambda: "))
        covx <- cov(x, method=cov_methods[i])
        for (j in 1:num_lambdas) {
            cat(paste0(j, ", "))
            model <- NULL
            if (j == 1 || is.null(model$w) || is.null(model$wi)) {
                if (lambdas[j] != 0) model <- glasso::glasso(covx, rho=lambdas[j])
                if (lambdas[j] == 0) model <- list(w=covx, wi=NULL)
            } else if (j != 1 && !is.null(model$w) && !is.null(model$wi)) {
                model <- glasso::glasso(covx,
                    rho=lambdas[j],
                    start="warm",
                    w.init=model$w,
                    wi.init=model$wi)
            }
            beta <- model$wi %*% cov(x,y, method=cov_methods[i])
            # rescale the betas
            if (rescale && sum(abs(beta)) != 0) {
                beta <- beta * lsfit(x %*% beta, y, intercept=FALSE)$coef
            }
            # save betas
            betamat[, j, i] <- beta
        }
    }

    # calculate intercepts
    interceptmat <- matrix(meany, nrow=length(lambdas) , ncol=length(cov_methods))
    for (m in 1:length(lambdas)) {
        for (n in 1:length(cov_methods)) {
            interceptmat[m, n] <- interceptmat[m, n] - sum((sdy * meanx / sdx) * betamat[,m,n])
        }
    }
    betamat <- sweep(betamat, 1, sdy / sdx, "*")
    rval <- list(intercepts=interceptmat,
        coefficients=betamat,
        lambdas=lambdas,
        cov_methods=cov_methods)
    class(rval) <- "train_glasso"
    return(rval)
}

# prediction method for the fit model
# TODO: this should predict on any type of glasso object (evaluation, training, final, etc)
predict.train_glasso <- function(object, newx, ...) {
    if(class(object) != "train_glasso")
        stop("Must pass train_glasso object.")

    if (!is.matrix(newx))
        stop("newx must be a matrix")

    graphical_lasso <- object
    lambdas <- graphical_lasso$lambdas
    num_lambdas <- length(lambdas)
    interceptmat <- graphical_lasso$intercepts
    betamat <- graphical_lasso$coefficients
    cov_methods <- graphical_lasso$cov_methods

    yhat <- array(dim=c(nrow(newx), num_lambdas, length(cov_methods)))
    for (k in 1:length(cov_methods)) {
        for (j in 1:num_lambdas) {
            yhat[,j,k] <- interceptmat[j,k] + newx %*% betamat[,j,k]
        }
    }
    return(yhat)
}


# METHODS FOR EVALUATING

# TODO: refit final glasso with optimal parameters

# TODO predict method for final model




plot_error_curve.Graphical_Lasso <- function(object, set, ...) {
    require(ggplot2)
    require(dplyr)
    require(stringr)

    if (class(object) != "Graphical_Lasso")
        stop("must provide object of type Graphical_Lasso")

    # unpack the Graphical_Lasso object
    rmse_df <- object$rmse_df

    # pull out the correct set
    rmse_set <- rmse_df[rmse_df$set == set & rmse_df$lambda_id != "lambda_null",]
    null_rmse <- round(unique(rmse_df[rmse_df$set == set & rmse_df$lambda_id == "lambda_null", "rmse"]),2)

    # identify the best value of lambda for the specified set
    best_lambda <- rmse_set[rmse_set$rmse == min(rmse_set$rmse), "lambda_val"]
    if (length(best_lambda) > 1) best_lambda <- best_lambda[1]
    best_cov_method <- rmse_set[rmse_set$rmse == min(rmse_set$rmse), "cov_method"]
    if (length(best_cov_method) > 1) best_cov_method <- best_cov_method[1]
    rmse_set$highlight <- factor(ifelse(rmse_set$lambda_val == best_lambda &
            rmse_set$cov_method == best_cov_method,
        "Optimal",
        "Sub-Optimal"))
    plot_title <- paste0("RMSE Across Windows and Locations\nFor Each Lambda and Method of Covariance Used for ",
        str_to_title(set), " Set")

    # plot the mean absolute rmse
    mycolours <- c("Optimal" = "red", "Sub-Optimal" = "grey50")
    null_label <- paste0("Null RMSE: ", null_rmse)
    rval <- ggplot(rmse_set, aes(x=lambda_val, y=rmse)) +
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
        theme(legend.position="bottom") +
        facet_wrap(~cov_method)
    return(rval)
}




plot_error_distribution.Graphical_Lasso <- function(object, split_by, ...) {
    if (class(object) != "Graphical_Lasso")
        stop("must provide object of type Graphical_Lasso")

    # unpack the Graphical_Lasso object
    all_hourly_df <- object$all_hourly_df
    num_lambdas <- object$num_lambdas

    temp <- all_hourly_df[, c("location", "window", "set", "cov_method", paste0("error", 1:num_lambdas))]
    temp[,paste0("error", 1:num_lambdas)] <- temp[,paste0("error", 1:num_lambdas)]^2
    rmse <- temp %>% group_by(location, window, set, cov_method) %>% summarise_each(funs(sqrt_mean))
    # join the lambda value to each rmse
    rmse_long <- melt(rmse, id.vars=c("location", "window", "set", "cov_method"))
    rmse_long$lambda_id <- str_replace(rmse_long$variable, fixed("error"), "lambda")
    lambda_df <- data.frame(lambda_id=paste0("lambda", 1:num_lambdas),
        lambda_val=lambda_sequence(num_lambdas))
    rmse_df <- left_join(x=rmse_long, y=lambda_df, by="lambda_id")
    best_errors <- rmse_df[rmse_df$lambda_val == object$best_lambda &
            rmse_df$cov_method == object$best_cov_method,]
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


plot_spatial_correlation_Glasso <- function(best_errors, set, n_breaks=10) {
    # put the errors in wide format
    temp <- best_errors[best_errors$set == set, c("location", "datetime_key", "error")]
    temp <- temp %>% group_by(location, datetime_key) %>% summarise(rmse = sqrt_mean(error^2))
    wide_error <- dcast(temp, datetime_key~location, value.var="rmse")
    # if there wasn't a prediction made at a certain time for a location
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



