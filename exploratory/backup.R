

<<glasso, eval=FALSE>>=

    lambda1 <- round(seq(.03, .55, length.out=10)^2, 3)
    lambda2 <- round(seq(.316, .45, length.out=10)^2, 3)
    num_lambdas = length(lambda1) * length(lambda2)

    # initialize containers to store results
    loc_results <- vector("list", num_locs)
    all_errors <- vector("list", num_locs)

    for (loc in 1:num_locs) {
        train_df <- DF[train_locs[[loc]], ]
        test_df <- DF[-train_locs[[loc]], ]

        dates <- ymd(train_df$date_key)
        start_date <- min(dates)
        end_date <- max(dates) %m-% months(testing_months + training_months)

        # Determine the number of sliding windows to capture results from
        sliding_window <- new_interval(start_date %m+% months(training_months), max(dates))
        window_duration <- as.period(sliding_window, units="months")
        window_duration_month <- month(window_duration) + round(day(window_duration)/30)
        num_windows <- ceiling(window_duration_month / testing_months)

        if (print_results)
            print(paste0("Location ", loc, ". Predict over:", sliding_window,
                " broken into ", num_windows,
                " segments of ", testing_months," months"))

        # for each window store the average prediction error
        window_results <- vector("list", num_windows)
        for (i in 1:num_windows) {

            if (print_results)
                print(paste0("Running segment ", i, ": ",
                    start_date, " - ", start_date %m+% months(testing_months)))

            # split the training dataset into the data to model, and the cross-validation set
            train_dates <- split_dates(dates, start_date, training_months, testing_months)
            model_df <- train_df[train_dates$in_train, c(date_vars, lag_vars, tar_var)]
            valid_df <- train_df[train_dates$in_validation, c(date_vars, lag_vars, tar_var)]
            p <- ncol(model_df) - 1
            n <- nrow(model_df)

            # put data in a matrix
            model_mat <- as.matrix(model_df)
            valid_mat <- as.matrix(valid_df)
            rm(model_df, valid_df)

            # train model on all combinations of interactions
            #         glasso_cv <- cv.scout(x=model_mat[,1:p], y=model_mat[,p+1], p1=1, p2=1)
            #         gmodel <- scout(x=model_mat[,1:p], y=model_mat[,p+1],
            #             p1=1, p2=1, lam1s=glasso_cv$bestlam1,lam2s=glasso_cv$bestlam2)
            gmodel <- scout(x=model_mat[,1:p], y=model_mat[,p+1],
                p1=1, p2=1,
                lam1s=lambda1,
                lam2s=lambda2,
                trace=FALSE)

            # save coefficients
            #         cf <- melt(gmodel$coefficients)
            #         names(cf) <- c("lambda1", "lambda2", "row_id", "estimate")

            # predict on the validation set
            valid_array <- predict(gmodel, newx=valid_mat[,1:p])
            valid_scores <- melt(valid_array)
            names(valid_scores) <- c("lambda1", "lambda2", "row_id", "value")
            valid_scores$lambda1 <- paste0("lambda1_", valid_scores$lambda1)
            valid_scores$lambda2 <- paste0("and_lambda2_", valid_scores$lambda2)
            valid_scores <- dcast(valid_scores, row_id ~ lambda1 + lambda2)
            valid_scores$row_id <- NULL
            valid_error <- sqrt(colMeans(valid_scores - valid_mat[,p+1])^2)

            if (print_results)
                print(paste0("    Best RMSE = ", round(min(valid_error), 2),
                    ", Mean of RMSEs = ", round(mean(valid_error), 2),
                    ", Stdev of RMSEs = ", round(sd(valid_error), 2)))

            rmse_df <- data.frame(cbind(i, t(valid_error)))
            names(rmse_df)[1] <- "window"
            window_results[[i]] <- rmse_df
            # increment the start date
            start_date <- start_date %m+% months(testing_months)
        }

        # summarize the window results for this location
        # average over each of the moving windows (1 number per lambda)
        all_window <- bind_rows(window_results)
        agg_window <- colMeans(all_window[,-1])
        loc_results[[loc]] <- data.frame(cbind(loc, t(agg_window)))

        if (print_results)
            print(paste0("Location ", loc,
                ": Best RMSE Averaged over all windows = ", round(min(agg_window), 4),
                ". Mean = ", round(mean(agg_window), 4),
                ". Stdev = ", round(sd(agg_window), 4)))
        # save all scores too
        all_window$loc <- loc
        all_errors[[loc]] <- all_window
    }


    @




        <<glasso_plot_error, eval=FALSE>>=

        all_rmse_df <- bind_rows(all_errors)
    rmse_df <- melt(all_rmse_df,
        id.vars=c("loc", "window"),
        variable.name="lambda_keys",
        value.name="error")
    rmse_df$lambda1_id <- as.numeric(str_split_fixed(rmse_df$lambda_keys, '_', 5)[,2])
    rmse_df$lambda2_id <- as.numeric(str_split_fixed(rmse_df$lambda_keys, '_', 5)[,5])
    rmse_df$lambda1_val <- lambda1[rmse_df$lambda1_id]
    rmse_df$lambda2_val <- lambda2[rmse_df$lambda2_id]
    rmse_df$lambda1_id <- NULL
    rmse_df$lambda2_id <- NULL
    rmse_df$lambda_keys <- NULL

    # identify the best pair of lambdas
    agg_df <- rmse_df %>%
        group_by(lambda1_val, lambda2_val) %>%
        summarise(mean_error = mean(error))
    best_lambda1 <- agg_df$lambda1_val[which(agg_df$mean_error == min(agg_df$mean_error))]
    best_lambda2 <- agg_df$lambda2_val[which(agg_df$mean_error == min(agg_df$mean_error))]
    agg_df$error_group <- as.character(cut(agg_df$mean_error,
        breaks=quantile(agg_df$mean_error, probs=seq(0, 1 , length.out=8))))
    agg_df$error_group <- factor(ifelse(is.na(agg_df$error_group), "optimal", agg_df$error_group))
    agg_df$error_group <- relevel(agg_df$error_group, ref="optimal")

    mypalette <- c("red2", brewer.pal(length(levels(agg_df$error_group)) - 1,"Blues"))
    ggplot(agg_df, aes(x=factor(lambda1_val), y=factor(lambda2_val), fill=error_group)) +
        geom_tile(colour="white") +
        scale_fill_manual(values=mypalette) +
        theme_bw() +
        labs(x="Covariance Regularization",
            y="Regression Coefficient Regularization",
            title="RMSE Averaged Across Moving Windows",
            fill="RMSE")


    @


        <<glasso_coef, results='asis', eval=FALSE>>=
        # pull out the coefficients for the best lambda (of the last model made)
        # TODO: save coefficients of all models, so the a better picture can be seen
        cf <- coef(glasso, s=best_lambda)
    cf <- data.frame(coef = rownames(cf)[which(cf > 0)], est = round(cf[which(cf > 0)], 4))
    print(xtable(cf, digits=4), include.rownames=FALSE)
    @


        Plot distribution of error across locations.
    <<glasso_location_plots, out.width='.49\\textwidth', eval=FALSE>>=
        best_errors <- rmse_df[rmse_df$lambda1_val == best_lambda1 &
                rmse_df$lambda2_val == best_lambda2,]

    ggplot(best_errors, aes(x=error, colour=factor(loc))) +
        geom_density() +
        labs(x="Error",
            y="Density",
            title="Distribution of Error Across Locations") +
        theme_bw() +
        theme(legend.position="none")
    @


        Plot error distributions varied by window time periods
    <<glasso_window_plots, out.width='.49\\textwidth', eval=FALSE>>=
        ggplot(best_errors, aes(x=error, colour=factor(window))) +
        geom_density() +
        labs(x="Error",
            y="Density",
            title="Distribution of Error Across Time Periods") +
        theme_bw() +
        theme(legend.position="none")

    @


        Analyze the error:
        <<glasso_location_error_table, results='asis', eval=FALSE>>=
        # what is the predicted error at each location (for optimal lambda)
        loc_error <- best_errors %>% group_by(loc) %>%
        summarise(mean_error = mean(error))
    loc_error <- loc_error[order(loc_error$mean_error, decreasing=FALSE),]
    print(xtable(loc_error, digits=4), include.rownames=FALSE)
    rm(loc_error)
    @

        <<glasso_window_error_table, results='asis', eval=FALSE>>=
        # what is the predicted error at each location (for optimal lambda)
        window_error <- best_errors %>% group_by(window) %>%
        summarise(mean_error = mean(error))
    window_error <- window_error[order(window_error$mean_error, decreasing=FALSE),]
    print(xtable(window_error, digits=4), include.rownames=FALSE)
    rm(window_error)

    @









        \subsection{Hierarchical Models}
    No hyperparameters to search for, so don't leave out a location. Instead use location as random effect. Predictions and evaluation still made on the future points.

    <<mixed_effects, eval=FALSE>>=
    library(lme4)

    # initialize containers to store results
    loc_results <- vector("list", num_locs)
    all_errors <- vector("list", num_locs)

    for (loc in 1:num_locs) {
    train_df <- DF[train_locs[[loc]], ]
    test_df <- DF[-train_locs[[loc]], ]

    dates <- ymd(train_df$date_key)
    start_date <- min(dates)
    end_date <- max(dates) %m-% months(testing_months + training_months)

    # Determine the number of sliding windows to capture results from
    sliding_window <- new_interval(start_date %m+% months(training_months), max(dates))
    window_duration <- as.period(sliding_window, units="months")
    window_duration_month <- month(window_duration) + round(day(window_duration)/30)
    num_windows <- ceiling(window_duration_month / testing_months)

    if (print_results)
    print(paste0("Location ", loc, ". Predict over: ", sliding_window,
    " broken into ", num_windows,
    " segments of ", testing_months," months"))

    # for each window store the average prediction error
    window_results <- vector("list", num_windows)
    for (i in 1:num_windows) {

    if (print_results)
    print(paste0("Running segment ", i, ": ",
    start_date, " - ", start_date %m+% months(testing_months)))

    # split the training dataset into the data to model, and the cross-validation set
    train_dates <- split_dates(dates, start_date, training_months, testing_months)
    model_df <- train_df[train_dates$in_train, c(date_vars, lag_vars, tar_var, "day_of_week")]
    valid_df <- train_df[train_dates$in_validation, c(date_vars, lag_vars, tar_var, "day_of_week")]
    model_df$day_of_week <- factor(model_df$day_of_week)
    valid_df$day_of_week <- factor(valid_df$day_of_week)


    # train model
    mixed_model <- lmer(pm25 ~ rushhour + quarter_cycle + pm25_lag1 + (1 | day_of_week),
    REML=FALSE,
    data = model_df)

    # save coefficients
    #         cf <- melt(gmodel$coefficients)
    #         names(cf) <- c("lambda1", "lambda2", "row_id", "estimate")

    # predict on the validation set
    valid_array <- predict(gmodel, newx=valid_mat[,1:p])
    valid_scores <- melt(valid_array)
    names(valid_scores) <- c("lambda1", "lambda2", "row_id", "value")
    valid_scores$lambda1 <- paste0("lambda1_", valid_scores$lambda1)
    valid_scores$lambda2 <- paste0("and_lambda2_", valid_scores$lambda2)
    valid_scores <- dcast(valid_scores, row_id ~ lambda1 + lambda2)
    valid_scores$row_id <- NULL
    valid_error <- sqrt(colMeans(valid_scores - valid_mat[,p+1])^2)

    if (print_results)
    print(paste0("    Best RMSE = ", round(min(valid_error), 2),
    ", Mean of RMSEs = ", round(mean(valid_error), 2),
    ", Stdev of RMSEs = ", round(sd(valid_error), 2)))

    rmse_df <- data.frame(cbind(i, t(valid_error)))
    names(rmse_df)[1] <- "window"
    window_results[[i]] <- rmse_df
    # increment the start date
    start_date <- start_date %m+% months(testing_months)
    }

    # summarize the window results for this location
    # average over each of the moving windows (1 number per lambda)
    all_window <- bind_rows(window_results)
    agg_window <- colMeans(all_window[,-1])
    loc_results[[loc]] <- data.frame(cbind(loc, t(agg_window)))

    if (print_results)
    print(paste0("Location ", loc,
    ": Best RMSE Averaged over all windows = ", round(min(agg_window), 4),
    ". Mean = ", round(mean(agg_window), 4),
    ". Stdev = ", round(sd(agg_window), 4)))
    # save all scores too
    all_window$loc <- loc
    all_errors[[loc]] <- all_window
    }


    @


