
data_dir <- '/Users/robert/documents/umn/air_pollution/data/'

# read in a few rows of data to determine column types
temp <- read.csv(file=paste0(data_dir, "houston_features.csv"), nrow=1000)

# if column types are logical (from poor guessing of type) change to double
col_types <- sapply(temp, class)
col_types <- ifelse(col_types %in% c('logical', 'numeric'), 'd', str_sub(col_types,1,1))
col_types <- paste(col_types, collapse='')

DF <- read_csv(file=paste0(data_dir, "houston_features.csv"),
    progress=FALSE,
    col_types=col_types)
# remove temporary items
rm(temp, col_types)

# One location moved slightly in 2013
DF$location_key <- ifelse(DF$location_key == 201000058, 201000026, DF$location_key)
# to speed up exploration, only use data from 2010 forward
full_locations <- c(167001034L, 201000024L, 201000026L, 201001034L,
    201001035L, 201001039L, 201001042L, 245000022L) # add this one if modeling post 2011: 201001050L
DF <- as.data.frame(DF[DF$date_key >= 20110000,]) #& DF$location_key %in% full_locations,])
DF$County_Code <- NULL
DF$State_Code <- NULL

# Create a few custom variables
DF$am_rushhour <- ifelse(DF$Time_Local %in% c(6,7,8), 1, 0)
DF$pm_rushhour <- ifelse(DF$Time_Local %in% c(18,19,20,21), 1, 0)
DF$rushhour <- ifelse(DF$am_rushhour > 0 | DF$pm_rushhour > 0, 1, 0)

# define variables to refer to groups of variables
tar_var <- 'pm25'

key_vars <- c("date_key", "datetime_key", "location_key"
    #     , "Date_Local"
    , "Latitude"
    , "Longitude"
    #     , "Site_Num"
    , "Time_Local"
)

date_vars <- c("day_of_week_cycle",
    "day_of_week_0", "day_of_week_1", "day_of_week_2",
    "day_of_week_3", "day_of_week_4", "day_of_week_5", "day_of_week_6",
    "day_of_year_cycle", "week_of_year_cycle",
    "month_cycle",
    "month_1", "month_2", "month_3", "month_4", "month_5", "month_6", "month_7",
    "month_8", "month_9", "month_10", "month_11", "month_12",
    "day_of_month_cycle",
    "quarter_cycle", "quarter_1", "quarter_2", "quarter_3", "quarter_4",
    "time_cycle",
    "am_rushhour", "pm_rushhour", "rushhour"
)

lag_root_names <- c("pm25_lag", "wind_direction_sin_lag", "wind_direction_cos_lag",
    # "wind_direction_NE_lag", "wind_direction_SE_lag",
    # "wind_direction_NW_lag", "wind_direction_SW_lag",
    "wind_knots_lag", "temperature_lag",
    "pct_humidity_lag", "dewpoint_lag",
    "ozone_lag", "so2_lag", "co_lag", "no2_lag")

lag_vars <- c(paste0(lag_root_names, "1")
    #     , paste0(lag_root_names, "2")
    #     , paste0(lag_root_names, "3")
)

agg_vars <- c(paste0(lag_root_names, "1", "_agg1")
    #     , paste0(lag_root_names, "2", "_agg1")
    #     , paste0(lag_root_names, "3", "_agg1")
)

# keep only the variables used above for modeling
DF <- DF[,c(key_vars, tar_var, date_vars, lag_vars, agg_vars)]

# impute remainder of missings
for (n in names(DF)) {
    if (n %in% c(lag_vars, agg_vars)) {
        DF[is.na(DF[,n]), n] <- mean(DF[,n], na.rm=TRUE)
    }
}


# plot_df <- DF[,c("location_key", "date_key")] %>% group_by(location_key, date_key) %>%
#     summarise(ct = length(date_key))
# plot_df$date_key <- ymd(plot_df$date_key)
# ggplot(plot_df, aes(x=date_key, y=ct)) +
#     geom_point() +
#     facet_wrap(~location_key)
