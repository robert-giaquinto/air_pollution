
Unpooled_Glasso <- function(DF, var_list, num_lambdas=50,
    testing_months=3, training_months=12,
    cov_methods=c("pearson", "spearman"), spatial_smoothing=c("none", "distance", "kernel"),
    verbose=FALSE)
{
    # performs grid search, and returns results from all combinations of grid parameters
    # trained for an Unpooled GLASSO
    grid_search <- expand.grid(cov_methods=cov_methods, spatial_smoothing=spatial_smoothing, stringsAsFactors=FALSE)
    all_results_list <- vector("list", nrow(grid_search))
    all_lambdas_list <- vector("list", nrow(grid_search))
    all_windows_list <- vector("list", nrow(grid_search))
    for (i in 1:nrow(grid_search)) {
        results <- Train_Unpooled_Glasso(DF=DF,
            var_list=var_list,
            num_lambdas=num_lambdas,
            testing_months=testing_months,
            training_months=training_months,
            cov_method=grid_search[i,"cov_methods"], spatial_smoothing=grid_search[i,"spatial_smoothing"],
            verbose=verbose)
        all_results_list[[i]] <- results$all_results
        all_lambdas_list[[i]] <- results$lambda_df
        all_windows_list[[i]] <- results$window_df
    }
    all_results <- bind_rows(all_results_list)
    all_lambdas <- bind_rows(all_lambdas_list)
    all_windows <- unique(bind_rows(all_windows_list))
    rmse_results <- compute_rmses(all_results, all_lambdas)
    rval <- list(all_results=all_results, all_lambdas=all_lambdas, all_windows=all_windows,
        rmse_df=rmse_results$rmse_df, rmse_agg=rmse_results$rmse_agg,
        best_error=rmse_results$best_error,
        best_cov_method=rmse_results$best_cov_method,
        best_spatial_smoothing=rmse_results$best_spatial_smoothing,
        best_lambda=rmse_results$best_lambda,
        best_lambda_id=rmse_results$best_lambda_id)
    class(rval) <- "Unpooled_Glasso"
    return(rval)
}


Train_Unpooled_Glasso <- function(DF, var_list, num_lambdas=50,
    testing_months=3, training_months=12,
    cov_method="pearson", spatial_smoothing="none",
    verbose=FALSE)
{
    # evaluate the unpooled glasso for one set of parameters
    # unpack variable list
    tar_var <- var_list$tar_var
    agg_vars <- var_list$agg_vars
    lag_vars <- var_list$lag_vars
    date_vars <- var_list$date_vars
    model_vars <- c(date_vars, lag_vars, agg_vars, tar_var)

    # make sure DF keys are strings
    DF$location_key <- as.character(DF$location_key)
    locations <- sort(unique(DF$location_key))
    num_locations <- length(locations)

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
    lambda_df <- expand.grid(1:num_windows, locations, cov_method, spatial_smoothing, stringsAsFactors=FALSE)
    lambda_df <- cbind(lambda_df,
        matrix(rep(NA, nrow(lambda_df) * num_lambdas), nrow=nrow(lambda_df)))
    names(lambda_df) <- c("window", "location_key", "cov_method", "spatial_smoothing", paste0("lambda", 1:num_lambdas))
    # also save dates associated with each window
    window_df <- data.frame(window=1:num_windows,
        train_start=rep(NA,num_windows),
        test_start=rep(NA,num_windows),
        test_end=rep(NA,num_windows))

    window_results <- vector("list", num_windows)
    window_betas <- vector("list", num_windows)
    for (window in 1:num_windows) {
        test_start <- train_start %m+% months(training_months)
        test_end <- test_start %m+% months(testing_months)
        window_df[window_df$window==window,"train_start"] <- train_start
        window_df[window_df$window==window,"test_start"] <- test_start
        window_df[window_df$window==window,"test_end"] <- test_end
        if (verbose) {
            cat(paste0("\nWINDOW ", window,
                ". Train: [", train_start, ", ", test_start,
                "). Test: [", test_start, ", ", test_end, ")"))
        }

        # split the training dataset from known locations to training and future datasets
        known_splits <- split_dates(ymd(DF$date_key),
            train_start, training_months, testing_months)

        # save location and date ids
        model_keys <- DF[known_splits$in_train, c("datetime_key", "location_key", "Latitude", "Longitude")]
        future_keys <- DF[known_splits$in_test, c("datetime_key", "location_key", "Latitude", "Longitude")]

        # build model training matrix by including interactions in the data
        model_df <- DF[known_splits$in_train, model_vars]
        future_df <- DF[known_splits$in_test, model_vars]
        # transform DFs into a wider DFs with interaction features
        model_formula <- as.formula(
            paste0(" ~ "
                , ifelse(is.null(date_vars), "", paste(date_vars, collapse="+"))
                , ifelse(is.null(lag_vars), "",  paste0("+(", paste(lag_vars, collapse="+"), ")^2"))
                , ifelse(is.null(agg_vars), "",  paste0("+(", paste(agg_vars, collapse="+"), ")^2 - 1"))))
        model_set <- cbind(model.matrix(model_formula, data=model_df), model_df[,tar_var])
        future_set <- cbind(model.matrix(model_formula, data=future_df), future_df[,tar_var])
        num_features <- ncol(model_set) - 1

        # train a model for each location
        models <- vector("list", num_locations)
        names(models) <- locations
        for (location in locations) {
            # fit a model for a specific location with given parameters
            model = glasso_fit(x=model_set[model_keys$location_key == location, 1:num_features],
                y=model_set[model_keys$location_key == location, num_features+1],
                num_lambdas=num_lambdas,
                cov_method=cov_method,
                standardize_input=TRUE,
                rescale_betas=FALSE)
            models[[location]] <- model

            # save lambda used for later aggregations
            lambda_df[lambda_df$window == window &
                    lambda_df$location_key == location,
                (ncol(lambda_df)-num_lambdas+1):ncol(lambda_df)] <- model$lambdas
        }

        # now that all locations and methods are computed, can combine (if requested) and predict
        location_betas <- vector("list", num_locations)
        names(location_betas) <- locations
        distance_matrix <- compute_distance_matrix(future_keys)
        for (location in locations) {
            # initialize beta matrix (rows=features, cols=lambdas) for this location
            # nrows includes intercept value
            beta_matrix <- matrix(NA, nrow=num_features+1, ncol=num_lambdas)
            for (lam in 1:num_lambdas) {
                # pull out appropriate x and y depending on if the smooting will be applied
                if (spatial_smoothing == "none") {
                    x <- model_set[model_keys$location_key == location, 1:num_features]
                    y <- model_set[model_keys$location_key == location, num_features+1]
                    icov <- models[[location]]$icov[[lam]]
                    cov_xy <- models[[location]]$cov_xy
                } else {
                    # use x and y from all locations for rescaling beta
                    # (note all needed from x y is sd and mean, can optimize here)
                    x <- model_set[,1:num_features]
                    y <- model_set[, num_features+1]
                    covariances <- smooth_covariance(models, distance_matrix, location, lam, spatial_smoothing)
                    icov <- covariances$icov
                    cov_xy <- covariances$cov_xy
                }
                beta_matrix[,lam] <- compute_beta(icov, cov_xy, x, y, standardize_input=TRUE)
            }
            location_betas[[location]] <- beta_matrix
        }
        window_betas[[window]] <- location_betas

        # Combine predictions, actuals, errors, and keys for this training window
        location_results <- vector("list", num_locations)
        names(location_results) <- locations
        for (location in locations) {
            # use betas associated with each location to predict future pm25
            betas <- location_betas[[location]]
            future_x <- future_set[future_keys$location_key == location, 1:num_features]
            yhats <- predict(betas, future_x)
            location_results[[location]] <- .compute_errors(yhats,
                y=future_set[future_keys$location_key == location, num_features+1],
                keys=future_keys[future_keys$location_key == location,],
                window=window,
                location=location,
                cov_method=cov_method,
                spatial_smoothing=spatial_smoothing)
        }
        window_results[[window]] <- bind_rows(location_results)

        # increment the start date and model id
        train_start <- train_start %m+% months(testing_months)
    }
    # combine the results from all windows
    all_results <- bind_rows(window_results)

    # package results
    rval <- list(all_results=all_results,
        location_betas=location_betas,
        lambda_df=lambda_df,
        window_df=window_df,
        cov_method=cov_method,
        spatial_smoothing=spatial_smoothing)
    class(rval) <- "Train_Unpooled_Glasso"
    return(rval)
}




glasso_fit <- function(x, y, num_lambdas,
    cov_method="pearson", standardize_input=TRUE, rescale_betas=FALSE, ...)
{
    # this is the algorithm that does the if fitting of a specific model
    # specifically, given the inputs, it finds a sparse inverse covariance matrix
    # for the data matrix x for predicting the real valued outputs y
    require(huge)
    # calculate statistics for rescaling beta for later
    sd_y <- sd(y)
    sd_x <- sd(x)
    mean_x <- mean(x)
    mean_y <- mean(y)
    # center and scale the data
    if(standardize_input){
        x <- scale(x, center=TRUE, scale=TRUE)
    } else {
        # center by don't scale
        x <- scale(x, center=TRUE, scale=FALSE)
    }
    # always standardize y
    y <- (y - mean_y) / sd_y

    cov_xy <- cov(x, y, method=cov_method)
    cov_x <- cov(x, method=cov_method)
    if (cov_method == "pearson") {
        # use the same lambda sequence each time
        lambdas <- lambda_sequence(num_lambdas, decreasing=TRUE)
        model <- huge(cov_x, lambda=lambdas, cov.output=TRUE,  method="glasso", verbose=FALSE, ...)
    } else {
        # TODO create a lambda sequence function that works when using spearman correlation
        # for now, rely on the built in lambda sequence generator
        model <- huge(cov_x, nlambda=num_lambdas, cov.output=TRUE,  method="glasso", verbose=FALSE, ...)
    }
    if (rescale_betas) {
        rval <- list(icov=model$icov, cov_xy=cov_xy, lambdas=model$lambda,
            standardize_input=standardize_input, mean_y=mean_y, sd_y=sd_y, mean_x=mean_x, sd_x=sd_x,
            x=x, y=y)
    } else {
        rval <- list(icov=model$icov, cov_xy=cov_xy, lambdas=model$lambda,
            standardize_input=standardize_input, mean_y=mean_y, sd_y=sd_y, mean_x=mean_x, sd_x=sd_x,
            x=NULL, y=NULL)
    }
    class(rval) <- "glasso_fit"
    return(rval)
}


compute_beta.glasso_fit <- function(object, ...) {
    # interface for computing betas directly from the glasso fit object
    if (object$rescale_betas) {
        rval <- .compute_beta(icov=object$icov, cov_xy=object$cov_xy,
            mean_y=object$mean_y, mean_x=object$mean_x, sd_x=object$sd_x, sd_y=object$sd_y,
            rescale_betas=object$rescale_betas, standardize_input=object$standardize_input,
            x=object$x, y=object$y)
    } else {
        rval <- .compute_beta(icov=object$icov, cov_xy=object$cov_xy,
            mean_y=object$mean_y, mean_x=object$mean_x, sd_x=object$sd_x, sd_y=object$sd_y,
            rescale_betas=object$rescale_betas, standardize_input=object$standardize_input)
    }
    return(rval)
}


compute_beta <- function(icov, cov_xy, x, y, standardize_input, ...) {
    # this is the primary interface for computing betas
    # x and y required so that the betas can be scaled to a particular training set
    # this interface assumes x and y must be given so that betas will be rescaled
    if (standardize_input) {
        sd_y <- sd(y)
        sd_x <- apply(x, 2, sd)
        x <- scale(x, center=TRUE, scale=TRUE)
    } else {
        x <- scale(x, center=TRUE, scale=FALSE)
        sd_x <- rep(1, ncol(x))
        sd_y <- 1
    }
    mean_y <- mean(y)
    mean_x <- colMeans(x)
    # always standardize y
    y <- (y - mean_y) / sd_y
    rval <- .compute_beta(icov=icov, cov_xy=cov_xy,
        mean_y=mean_y, mean_x=mean_x, sd_x=sd_x, sd_y=sd_y,
        rescale_betas=TRUE,
        x=x, y=y)
    return(rval)
}

.compute_beta <- function(icov, cov_xy, mean_x, mean_y, sd_x, sd_y,
    rescale_betas, x=NULL, y=NULL) {
    # Generic function to compute betas given:
    # a sparse inverse covariance matrix of features x,
    # covariance between x and y,
    # and, if re-scaling, the x and y training data
    beta <- icov %*% cov_xy

    # make sure correct input is give is rescaling betas is requested
    if (rescale_betas) {
        if (is.null(x) & is.null(y)) {
            stop("x and y must be provided to rescale beta")
        } else {
            # do the rescaling
            if (sum(abs(beta)) != 0){
                beta <- as.matrix(beta * lsfit(x %*% beta, y, intercept=FALSE)$coef)
            }
        }
    } else if (is.null(mean_x) & is.null(mean_y) & is.null(sd_x) & is.null(sd_y)) {
        stop("mean_x, mean_y, sd_x, sd_y must be provided to calculate intecept")
    }

    # calculate intercept
    intercept <- mean_y - sum((sd_y * mean_x / sd_x) * beta)
    # uncenter and scale betas
    beta <- beta * (sd_y / sd_x)
    rval <- c(intercept, beta)
    return(rval)
}

compute_distance_matrix <- function(coordinates) {
    # in case there is noise in coordinates, find center of each location
    mean_locations <- coordinates %>% group_by(location_key) %>%
        summarise(Latitude=mean(Latitude), Longitude=mean(Longitude))
    num_locations <- length(mean_locations$location_key)
    # use locations for find distance matrix
    lats <- mean_locations$Latitude
    names(lats) <- mean_locations$location_key
    lat_dist <- as.matrix(dist(lats, diag=TRUE, upper=TRUE))
    longs <- mean_locations$Longitude
    names(longs) <- mean_locations$location_key
    long_dist <- as.matrix(dist(longs, diag=TRUE, upper=TRUE))
    distance_matrix <- matrix(rep(0, num_locations^2), nrow=num_locations)
    for (m in 1:num_locations) {
        for (n in 1:num_locations) {
            distance_matrix[m,n] <- squared_distance(lat_dist[m,n], long_dist[m,n])
        }
    }
    return(distance_matrix)
}

smooth_covariance <- function(models, distance_matrix, location, lambda, method, sigma=0.5)
{
    # given a set of inverse covariances, and covariances between x and y (all contained in models)
    # then this function will use the distance matrix between locations to smooth the covariances
    locations <- names(models)
    num_locations <- length(locations)

    # use distance matrix to determine weight assigned to each location
    if (method == "distance") {
        # use inverse distance weighting
        # assume origin has 50% weight?
        origin_weight <- 0.50
        weight_matrix <- distance_matrix / rowSums(distance_matrix)
        weight_matrix <- weight_matrix * (1 - origin_weight)
        diag(weight_matrix) <- rep(origin_weight, ncol(weight_matrix))
    } else if (method == "kernel") {
        kernel_matrix <- gaussian_kernel(distance_matrix, sigma=sigma)
        row_sums = rowSums(kernel_matrix)
        weight_matrix <- kernel_matrix / row_sums
    } else {
        stop("method must be weight distance or kernel")
    }
    weight_vector <- weight_matrix[which(location == locations),]

    # apply weighting
    num_features <- nrow(models[[locations[1]]]$icov[[1]])
    icov <- matrix(0, nrow=num_features, ncol=num_features)
    cov_xy <- rep(0, num_features)
    for (loc in locations) {
        weight_index <- which(loc == locations)
        icov <- icov + (models[[loc]]$icov[[lambda]] * weight_vector[weight_index])
        cov_xy <- cov_xy + (models[[loc]]$cov_xy * weight_vector[weight_index])
    }
    rval <- list(icov=icov, cov_xy=cov_xy)
    return(rval)
}


# prediction method for the fit model
# TODO: this should predict on any type of glasso object (evaluation, training, final, etc)
predict <- function(betas, newx, add_intercept=TRUE, ...) {
    # this is a general predict method, it just applies X*b
    if (!is.matrix(newx))
        stop("newx must be a matrix")
    # assume intercept is first row of betas, so append a 1 before newx
    if (add_intercept) newx <- cbind(1, newx)
    rval <- newx %*% betas
    return(rval)
}


.compute_errors <- function(yhats, y, keys, window, location, cov_method, spatial_smoothing)
{
    # this is a hidden function for combining predictions, actuals, errors, and keys
    # into a single dataframe
    num_lambdas <- ncol(yhats)
    temp_predicts <- as.data.frame(yhats)
    names(temp_predicts) <- paste0("predict", 1:num_lambdas)
    temp_predicts$predict_null <- mean(y)

    temp_errors <- as.data.frame(y - yhats)
    names(temp_errors) <- paste0("error", 1:num_lambdas)
    temp_errors$error_null <- y - mean(y)

    temp_keys <- data.frame(window=window, location=location,
        cov_method=cov_method, spatial_smoothing=spatial_smoothing, actual=y)
    rval <- cbind(keys, temp_keys, temp_predicts, temp_errors)
}


compute_rmses <- function(all_results, lambda_df) {
    # finds RMSE statistics given the training and prediction results
    # aggregate error to find rmse for each of the training+test runs
    key_vars <-  c("window", "location_key", "cov_method", "spatial_smoothing")
    error_vars <- names(all_results)[which(str_detect(names(all_results), "error[0-9]+|error\\_null"))]
    rmses <- all_results[, c(key_vars, error_vars)] %>%
        group_by(window, location_key, cov_method, spatial_smoothing) %>%
        summarise_each(funs(rmse))

    # join the lambda value to each rmse
    rmse_long <- melt(rmses, id.vars=key_vars)
    names(rmse_long) <- c(key_vars, "error_id", "rmse")
    rmse_long$lambda_id <- str_replace(rmse_long$error_id, fixed("error"), "lambda")

    # transform lambda_df into a long df for easier merging
    lambda_long <- melt(lambda_df, id.vars=key_vars)
    names(lambda_long) <- c(key_vars, "lambda_id", "lambda_val")
    temp <- sapply(lambda_long, is.factor)
    lambda_long[temp] <- lapply(lambda_long[temp], as.character)
    rmse_df <- left_join(x=rmse_long, y=lambda_long, by=c(key_vars, "lambda_id"))
    rmse_df$error_id <- NULL

    # use rmse to find best lambda and covariance method across windows
    rmse_agg <- rmse_df %>% group_by(cov_method, spatial_smoothing, lambda_id) %>%
        summarise(Avg_RMSE=mean(rmse), StDev_RMSE=sd(rmse), Lambda=mean(lambda_val))

    best_error <- min(rmse_agg[rmse_agg$lambda_id != "lambda_null", "Avg_RMSE"])
    best_parameters <- rmse_agg[rmse_agg$Avg_RMSE == best_error &
            rmse_agg$lambda_id != "lambda_null", c("Lambda", "cov_method", "spatial_smoothing", "lambda_id")]
    best_lambda <- best_parameters$Lambda
    best_cov_method <- best_parameters$cov_method
    best_spatial_smoothing <- best_parameters$spatial_smoothing
    best_lambda_id <- as.numeric(str_replace(best_parameters$lambda_id, "lambda", ""))
    rval <- list(rmse_df=rmse_df, rmse_agg=rmse_agg,
        best_error=best_error,
        best_lambda=best_lambda,
        best_cov_method=best_cov_method,
        best_spatial_smoothing=best_spatial_smoothing,
        best_lambda_id=best_lambda_id)
    return(rval)
}





plot_error_curve <- function(x) {
    UseMethod("plot_error_curve", x)
}
plot_error_curve.Unpooled_Glasso <- function(object, ...) {
    require(ggplot2)
    require(grid)
    require(gridExtra)
    require(dplyr)
    require(stringr)

    if (class(object) != "Unpooled_Glasso")
        stop("must provide object of type Unpooled_Glasso")

    # unpack the Unpooled_Glasso object
    rmse_agg <- object$rmse_agg

    # find null performance
    null_rmse <- unique(rmse_agg[rmse_agg$lambda_id == "lambda_null",c("Avg_RMSE", "StDev_RMSE")])
    plot_caption <- paste0("Null RMSE: ", round(null_rmse$Avg_RMSE, 2), " (+/-", round(null_rmse$StDev_RMSE,2),"). ",
        "Error bars represent variability in RMSE across locations and prediction windows.")

    # extract avg rmse by lambda and label the best results
    rmse_set <- rmse_agg[rmse_agg$lambda_id != "lambda_null",]
    rmse_set$highlight <- factor(ifelse(rmse_set$lambda_id == paste0("lambda", object$best_lambda_id) &
            rmse_set$cov_method == object$best_cov_method &
            rmse_set$spatial_smoothing == object$best_spatial_smoothing,
        "Optimal",
        "Sub-Optimal"))

    # plot
    mycolours <- c("Optimal" = "red", "Sub-Optimal" = "grey50")
    p1 <- ggplot(rmse_set, aes(x=Lambda, y=Avg_RMSE)) +
        geom_errorbar(aes(ymax=Avg_RMSE+StDev_RMSE, ymin=Avg_RMSE-StDev_RMSE,
            colour=highlight), width=.1) +
        geom_point(aes(colour=highlight), size=2.5) +
        geom_line() +
        facet_grid(spatial_smoothing~cov_method, scales="free_x") +
        theme_bw() +
        scale_color_manual("Overall Performance: ", values=mycolours) +
        scale_x_log10() +
        labs(x="Model Complexity",
            y="RMSE",
            title="RMSE Across All Hyperparameters:\nCovariance Method (Columns) and Weighting of Covariances (Rows)") +
        theme(legend.position="bottom", axis.ticks.x = element_blank(), axis.text.x = element_blank())
    rval <- list(p1=p1, plot_caption=plot_caption)
    return(rval)
}




plot_error_distribution <- function(x, conditional) {
    UseMethod("plot_error_distribution", x)
}
plot_error_distribution.Unpooled_Glasso <- function(object, conditional, ...)
{
    if (class(object) != "Unpooled_Glasso")
        stop("must provide object of type Unpooled_Glasso")
    if (!(conditional %in% c("window", "location_key")))
        stop("conditional parameter must be either window or location_key")
    plot_df <- object$rmse_df
    plot_df[,conditional] <- factor(plot_df[,conditional])
    out_plot <- ggplot(plot_df, aes_string(x="rmse", colour=conditional)) +
        geom_density() +
        labs(x="RMSE",
            y="Density",
            title="Distribution of Error By Sliding Windows") +
        theme_bw() +
        theme(legend.position="none") +
        facet_grid(spatial_smoothing~cov_method, scales="free_y")
    plot_caption <- paste0("Color of lines represents distribution of RMSE for each prediction ",
        str_replace_all(conditional, fixed("_"), "\\_"),
        ". Variability comes from different RMSE's in each ",
        ifelse(conditional == "window", "location", "window"),
        ". Note, optimal combination of parameters requires calculating covariance using ",
        object$best_cov_method, "'s method, and ",
        ifelse(object$best_spatial_smoothing == "none",
            "not combining covariances",
            paste0("combing covariances from each location based on a ",
                object$best_spatial_smoothing, " weighting.")))
    rval <- list(out_plot=out_plot, plot_caption=plot_caption)
    return(rval)
}


plot_spatial_correlation <- function(x) {
    UseMethod("plot_spatial_correlation", x)
}
plot_spatial_correlation.Unpooled_Glasso <- function(object, n_breaks=10, ...) {
    if (class(object) != "Unpooled_Glasso")
        stop("must provide object of type Unpooled_Glasso")
    best_error_key <- paste0("error", object$best_lambda_id)
    keep_vars <- c("location_key", "datetime_key", best_error_key)
    all_results <- object$all_results[,keep_vars]
    names(all_results) <- c(keep_vars[1:2], "error")

    # calculate rms by place and time
    temp <- all_results %>% group_by(location_key, datetime_key) %>%
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

