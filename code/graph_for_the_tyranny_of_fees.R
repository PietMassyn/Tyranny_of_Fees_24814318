graph_for_the_tyranny_of_fees <- function(year, cum_diff, tidy_dat, text_box_location_x, text_box_location_y){

    final_value_of_1m_investment <- function(df, bps_val){
        x <- df %>%
            filter(bps == bps_val)%>%
            pull(cumulative_returns)

        return(x)

    }

    bps_10 <- final_value_of_1m_investment(df = cum_diff, bps_val = "fees_bps10")
    bps_250 <- final_value_of_1m_investment(df = cum_diff, bps_val = "fees_bps250")
    bps_350 <- final_value_of_1m_investment(df = cum_diff, bps_val = "fees_bps350")
    label_text <- glue("R1m invested in January {year} \n\u2022 10 BPS: R{round(bps_10,2)}m \n\u2022 250 BPS: R{round(bps_250,2)}m \n\u2022 350 BPS : R{round(bps_350,2)}m")
    subtitle_text <- glue("Investor Cumulative Returns Given Different Fees since {year}")

    p <- ggplot(tidy_dat)+
        geom_line(aes(x = date, y = cumulative_returns, color = bps), linewidth = 0.8, alpha = 0.7) +
        geom_text(data = cum_diff,
                  aes(x = date, y = cumulative_returns, label = difference, color = bps),
                  hjust = -0.1, size = 2, fontface = "bold") +
        annotate(
            "label",
            x = as.Date(text_box_location_x),
            y = text_box_location_y,
            label = label_text,
            hjust = 0,
            vjust = 1,
            size = 3,
            fill = "white",
            color = "black",
            label.size = 0.1
            ) +
        fmxdat::theme_fmx(title.size = ggpts(30),
                      subtitle.size = ggpts(28),
                      caption.size = ggpts(25),
                      CustomCaption = T) +
        fmxdat::fmx_cols() +
        labs(x = "", y = "Cumulative Returns Given Fees", caption = "Note:\nData from fmxdat package",
             title = "Effect of Fees on Returns",
             subtitle = subtitle_text)

    fmxdat::finplot(p, x.vert = T, x.date.type = "%Y", x.date.dist = "1 years")

    return(p)


}


#graph_for_the_tyranny_of_fees(year = 2010, cum_diff = difference_in_cum_value_2010, tidy_dat = tidy_df_2010, text_box_location_x = "2010-01-01", text_box_location_y = 3)

