# library(GHCNr)

# daily_ts <- daily(
#   "CA003076680",
#   paste("2010", "01", "01", sep = "-"),
#   paste("2015", "12", "31", sep = "-"),
#   variables = "tmax"
# )
# daily_ts
# write.csv(daily_ts, "/tmp/daily.csv")
# daily_ts <- read.csv("/tmp/daily.csv")

# class(daily_ts)

# monthly_ts <- monthly(daily_ts)
# monthly_ts
# class(monthly_ts)
# annual_ts <- annual(daily_ts)
# annual_ts
# class(annual_ts)

# # daily_ts <- remove_flagged(daily_ts)
# # daily_ts
# # class(daily_ts)

# # station_coverage <- coverage(daily_ts)