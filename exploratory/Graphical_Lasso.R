# Master graphical lasso class
# used for training and evaluating the graphical lasso
Graphical_Lasso <- function(DF, var_list, num_lambdas=50,
    testing_months=3, training_months=12, verbose=FALSE, cov_methods=c("pearson", "spearman"))
{
    require(glasso)
    # unpack variable list
    tar_var <- var_list$tar_var
    agg_vars <- var_list$agg_vars
    lag_vars <- var_list$lag_vars
    date_vars <- var_list$date_vars
    model_vars <- c(date_vars, lag_vars, agg_vars, tar_var)
    # locations <- sort(unique(DF$location_key))
    # num_locs <- length(locations)

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
    train_start <- min_date # train start will be incremented

    # save the lambda values used for each window and covariance method
    lambda_df <- expand.grid(window=1:num_windows, cov_methods)
    lambda_df <- cbind(lambda_df, matrix(rep(NA, nrow(lambda_df) * num_lambdas), nrow=nrow(lambda_df)))
    names(lambda_df) <- c("window", "cov_methods", paste0("lambda", 1:num_lambdas))

    # for each window store the average prediction error
    window_results <- vector("list", num_windows)
    for (window in 1:num_windows) {
        if (verbose) {
            test_start <- train_start %m+% months(training_months)
            test_end <- test_start %m+% months(testing_months)
            cat(paste0("\nWINDOW ", window,
                ". Train: [", train_start, ", ", test_start,
                "). Test: [", test_start, ", ", test_end, ")"))
        }

        # split the training dataset from known locations to training and future datasets
        known_splits <- split_dates(ymd(DF$date_key),
            train_start, training_months, testing_months)
        model_set <- as.matrix(DF[known_splits$in_train, model_vars])
        future_set <- as.matrix(DF[known_splits$in_test, model_vars])
        p <- ncol(model_set) - 1

        # save location and date ids
        future_keys <- DF[known_splits$in_test, c("datetime_key", "location_key", "Latitude", "Longitude")]

        # train model on all combinations of interactions
        graphical_lasso <- train_glasso_rank(x=model_set[,1:p], y=model_set[,p+1],
            num_lambdas=num_lambdas,
            cov_methods=cov_methods,
            rescale_betas=TRUE, standardize_input=TRUE)
        # save lambdas
        for (cm in 1:length(cov_methods)) {
            lambda_df[lambda_df$window == window &
                    lambda_df$cov_methods == cov_methods[cm],
                (ncol(lambda_df)-num_lambdas+1):ncol(lambda_df)] <- graphical_lasso$lambda_list[[cm]]
        }
        # save null model
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
            temp_keys <- data.frame(window, cov_methods = cov_methods[x], actual = future_set[,tar_var])
            rval <- cbind(future_keys, temp_keys, temp_predicts, temp_errors)
            return(rval)
        })
        window_results[[window]] <- bind_rows(future_list)

        # increment the start date and model id
        train_start <- train_start %m+% months(testing_months)
    }

    # combine results from each window
    all_hourly_df <- bind_rows(window_results)
    rm(window_results)

    # find rmse for each lambda by covariance method
    error_names <- c(paste0("error", 1:num_lambdas), "error_null")
    temp <- all_hourly_df[, c("cov_methods", "window", error_names)]
    temp[,error_names] <- temp[,error_names]
    # aggregate error to find rmse for each of the training+test runs
    rmses <- temp %>% group_by(cov_methods, window) %>% summarise_each(funs(rmse))
    # join the lambda value to each rmse
    rmse_long <- melt(rmses, id.vars=c("cov_methods", "window"))
    names(rmse_long) <- c("cov_methods", "window", "error_id", "rmse")
    rmse_long$lambda_id <- str_replace(rmse_long$error_id, fixed("error"), "lambda")
    # transform lambda_df into a long df for easier merging
    lambda_long <- melt(lambda_df, id.vars=c("window", "cov_methods"))
    names(lambda_long) <- c("window", "cov_methods", "lambda_id", "lambda_val")
    temp <- sapply(lambda_long, is.factor)
    lambda_long[temp] <- lapply(lambda_long[temp], as.character)
    rmse_df <- left_join(x=rmse_long, y=lambda_long, by=c("window", "cov_methods", "lambda_id"))
    rmse_df$error_id <- NULL

    # use rmse to find best lambda and covariance method across windows
    rmse_agg <- rmse_df %>% group_by(cov_methods, lambda_id) %>%
        summarise(Avg_RMSE=mean(rmse), StDev_RMSE=sd(rmse), Lambda=mean(lambda_val))

    best_error <- min(rmse_agg[rmse_agg$lambda_id != "lambda_null", "Avg_RMSE"])
    best_parameters <- rmse_agg[rmse_agg$Avg_RMSE == best_error &
            rmse_agg$lambda_id != "lambda_null", c("Lambda", "cov_methods", "lambda_id")]
    best_lambda <- best_parameters$Lambda
    best_cov_method <- best_parameters$cov_methods
    best_lambda_id <- as.numeric(str_replace(best_parameters$lambda_id, "lambda", ""))

    # package results
    rval <- list(rmse_df=rmse_df,
        all_hourly_df=all_hourly_df,
        rmse_agg=rmse_agg,
        best_lambda=best_lambda,
        best_cov_method=best_cov_method,
        best_lambda_id=best_lambda_id,
        lambdas=lambda_long,
        num_lambdas=num_lambdas)
    class(rval) <- "Graphical_Lasso"
    return(rval)
}




train_glasso_rank <- function(x, y, num_lambdas,
    cov_methods=c("pearson","spearman"),
    rescale_betas=TRUE, standardize_input=TRUE, verbose=TRUE)
{
    require(huge)
    # center and scale the data
    meany <- mean(y)
    meanx <- colMeans(x)
    if(standardize_input){
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
    betamat <- array(NA, dim=c(ncol(x), num_lambdas, length(cov_methods)))
    lambda_list <- vector(length(cov_methods), mode="list")
    for (i in 1:length(cov_methods)) {
        if (verbose) {
            cat(paste0("\nTraining using method=", cov_methods[i],
                "\nFinding Betas for lambda sequence:\n"))
        }
        cov_x <- cov(x, method=cov_methods[i])
        if (cov_methods[i] == "pearson") {
            lambdas <- lambda_sequence(num_lambdas, decreasing=TRUE)
            model <- huge(cov_x, lambda=lambdas, cov.output=TRUE,  method="glasso", verbose=FALSE)
        } else {
            model <- huge(cov_x, nlambda=num_lambdas, cov.output=TRUE,  method="glasso", verbose=FALSE)
        }
        lambda_list[[i]] <- model$lambda
        for (j in 1:num_lambdas) {
            if (verbose) cat(paste0(j, " "))
            cov_xy <- cov(x, y, method=cov_methods[i])
            beta <- model$icov[[j]] %*% cov_xy
            # rescale the betas
            if (rescale_betas && sum(abs(beta)) != 0) {
                beta <- beta * lsfit(x %*% beta, y, intercept=FALSE)$coef
            }
            # save betas
            betamat[, j, i] <- as.matrix(beta)
        }
    }

    # calculate intercepts
    interceptmat <- matrix(meany, nrow=num_lambdas, ncol=length(cov_methods))
    for (m in 1:num_lambdas) {
        for (n in 1:length(cov_methods)) {
            interceptmat[m, n] <- interceptmat[m, n] - sum((sdy * meanx / sdx) * betamat[,m,n])
        }
    }
    betamat <- sweep(betamat, 1, sdy / sdx, "*")
    rval <- list(intercepts=interceptmat,
        coefficients=betamat,
        num_lambdas=num_lambdas,
        cov_methods=cov_methods,
        lambda_list=lambda_list)
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
    num_lambdas <- graphical_lasso$num_lambdas
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


plot_error_curve.Graphical_Lasso <- function(object, ...) {
    require(ggplot2)
    require(grid)
    require(gridExtra)
    require(dplyr)
    require(stringr)

    if (class(object) != "Graphical_Lasso")
        stop("must provide object of type Graphical_Lasso")

    # unpack the Graphical_Lasso object
    rmse_agg <- object$rmse_agg

    # find null performance
    null_rmse <- unique(rmse_agg[rmse_agg$lambda_id == "lambda_null",c("Avg_RMSE", "StDev_RMSE")])
    null_label <- paste0("Null RMSE: ", round(null_rmse$Avg_RMSE, 2), " (+/-", round(null_rmse$StDev_RMSE,2),")")

    # extract avg rmse by lambda and label the best results
    rmse_set <- rmse_agg[rmse_agg$lambda_id != "lambda_null",]
    rmse_set$highlight <- factor(ifelse(rmse_set$Lambda == object$best_lambda &
            rmse_set$cov_methods == object$best_cov_method,
        "Optimal",
        "Sub-Optimal"))

    # plot
    mycolours <- c("Optimal" = "red", "Sub-Optimal" = "grey50")
    plot_title <- paste0("RMSE Averaged Over Sliding Windows and Locations\n",
        "For Each Lambda and Method of Covariance Calculation")
    cov_methods <- unique(rmse_set$cov_methods)
    plot_list <- vector(ceiling(sqrt(length(cov_methods))), mode="list")
    p1 <- ggplot(rmse_set, aes(x=Lambda, y=Avg_RMSE)) +
        geom_errorbar(aes(ymax = Avg_RMSE + StDev_RMSE, ymin=Avg_RMSE - StDev_RMSE, colour=highlight)) +
        geom_point(aes(colour=highlight), size=2.5) +
        geom_line() +
        scale_color_manual("Status: ", values=mycolours) +
        labs(x="Model Complexity",
            y="RMSE",
            title=plot_title) +
        theme_bw() +
        theme(legend.position="bottom", axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
        facet_wrap(~cov_methods, scales="free_x")
    rval <- list(p1=p1, null_label=null_label)
    return(rval)
}

squared_distance <- function(lat_dif, long_dif) {
    return(sqrt(lat_dif^2 + long_dif^2))
}

gaussian_kernel <- function(x, sigma) {
    return(1/(sqrt(2 * pi) * sigma) * exp(-1 * x^2 / (2 * sigma^2)))
}



plot_error_distribution.Graphical_Lasso <- function(object, ...)
{
    if (class(object) != "Graphical_Lasso")
        stop("must provide object of type Graphical_Lasso")
    rval <- ggplot(object$rmse_df, aes(x=rmse, colour=factor(window))) +
        geom_density() +
        labs(x="RMSE",
            y="Density",
            title="Distribution of Error By Sliding Windows") +
        theme_bw() +
        theme(legend.position="none") +
        facet_wrap(~cov_methods)
    return(rval)
}


plot_spatial_correlation.Graphical_Lasso <- function(object, n_breaks=10, ...) {
    best_error_key <- paste0("error", object$best_lambda_id)
    keep_vars <- c("location_key", "datetime_key", best_error_key)
    all_hourly_df <- object$all_hourly_df[,keep_vars]
    names(all_hourly_df) <- c(keep_vars[1:2], "error")

    # calculate rms by place and time
    temp <- all_hourly_df %>% group_by(location_key, datetime_key) %>%
        summarise(RMSE = rmse(error))
    # put the errors in wide format
    wide_error <- dcast(temp, datetime_key~location_key, value.var="RMSE")
    # if there wasn't a prediction made at a certain time for a location_key,
    # then drop the the row from all other locations too
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



