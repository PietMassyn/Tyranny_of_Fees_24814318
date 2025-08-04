library(xts)
library(tbl2xts)


convert_daily_to_weekly <- function(df){


    xts_df <- tbl_xts(df)

    weekly_TRI_xts <- apply.weekly(xts_df, last)

    weekly_TRI <- xts_tbl(weekly_TRI_xts)

    return(weekly_TRI)

}


#df2 <- convert_daily_to_weekly(df = df)
