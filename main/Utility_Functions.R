# split the data into training and test sets
# need two functions:
# 1. split locations into test and train
split_locations <- function(location_key, num_test_locs=1, repeated=TRUE) {
    # location key: vector of unique location identifiers
    # num_test_locs: number of locations to be kept in test set (default=1)
    # return: a list of length equal to number of unique locations. Each
    #         element of the list contains indices of the training rows.
    # example:
    #location_splits <- split_locations(DF$location_key, 1)
    #num_locs <- length(location_splits)

    uniq_locs <- unique(location_key)
    num_locs <- length(uniq_locs)
    in_train = vector("list", num_locs)
    for (i in 1:num_locs) {
        if (repeated) {
            training_locs <- uniq_locs[-1*i]
        } else {
            training_locs <- sample(uniq_locs, size=num_locs - num_test_locs)
        }
        in_train[[i]] <- which(location_key %in% training_locs)
    }
    return(in_train)
}

# 2. a function to split out the cross-validation sections
# i.e. a sliding window based on date
split_dates <- function(dates, train_start, training_months=12, testing_months=3) {
    test_start <- train_start %m+% months(training_months)
    test_end <- test_start %m+% months(testing_months)
    rval <- list(in_train=which(dates >= train_start & dates < test_start),
        in_test=which(dates >= test_start & dates < test_end))
    return(rval)
}

lambda_sequence <- function(num_lambdas, decreasing=TRUE) {
    if (decreasing)
        rval <- exp(seq(-0.1, -7, length.out=num_lambdas))
    else
        rval <- exp(seq(-7, -0.1, length.out=num_lambdas))

    return(rval)
}

count_sliding_windows <- function(train_start, max_date, training_months, testing_months) {
    sliding_window <- new_interval(train_start %m+% months(training_months), max_date)
    window_duration <- as.period(sliding_window, units="months")
    window_duration_month <- month(window_duration) + round(day(window_duration)/30)
    num_windows <- ceiling(window_duration_month / testing_months)
    return(num_windows)
}

# sqrt_mean <- function(x) sqrt(mean(x))

rmse <- function(x) sqrt(mean(x^2))

squared_distance <- function(lat_dif, long_dif) {
    return(sqrt(lat_dif^2 + long_dif^2))
}

gaussian_kernel <- function(x, sigma) {
    return(1/(sqrt(2 * pi) * sigma) * exp(-1 * x^2 / (2 * sigma^2)))
}
