final_value_of_1m_investment <- function(df, bps_val){
    x <- df %>%
        filter(bps == bps_val)%>%
        pull(cumulative_returns)

    return(x)

}


#bps_10 <- final_value_of_1m_investment(df = difference_in_cum_value,bps_val =  "fees_bps10")


