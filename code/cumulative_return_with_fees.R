library(tidyverse)
# there needs to be an existing returns column in the df

cumulative_return_with_fees <- function(df, bps, time_frame){

    if( !"returns" %in% names(df)) stop("... for Calc to work, provide returns column called 'returns'")

    net <- paste0("net_return_bps", bps)
    fees <- paste0("fees_bps", bps)


    monthly_fee <- (bps / 10000) / time_frame

    df <- df %>% mutate(!!net := coalesce(returns,0) - monthly_fee,
                        !!fees := cumprod(1 + !!sym(net)))


    }

#weekly_df <- cumulative_return_with_fees(df = weekly_df, bps = 10, time_frame = 52)

