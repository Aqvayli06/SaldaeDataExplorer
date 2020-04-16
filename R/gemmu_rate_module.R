
#' basic growth rate
#' @author Farid Azouaou
#' @param x numeric vector
#' @param asurif differentiation lag
#' @return numeric vector containing differentiated values
basic_growth_rate <- function(x, asurif = 1) {
  linear_Develop <- x - lag(x, asurif)
  linear_growth_rate <- round((x - lag(x, asurif)) / lag(x, asurif) * 100, 2)
  log_growth_rate <- round((log(x) - log(lag(x, asurif))) * 100, 2)
  gr <- dplyr::tbl_df(data.frame(linear_Develop, linear_growth_rate, log_growth_rate))
  return(gr)
}
#'Saldae Detect possible growth rates
#' @description based on time unit detect possible growth rates
#' @author Farid Azouaou
#' @param base_unit time unit
#' @param gemmu_iswi target growth rate
#' @return possible growth rates
#' @export

gemmu_detect_frequency <- function(base_unit = NULL, gemmu_iswi = NULL) {
  if (base_unit == "minutes" & gemmu_iswi == "Hourly") {
    return("minute")
  }
  if (base_unit == "hours" & gemmu_iswi == "Daily") {
    return("hour")
  }
  if (base_unit == "days" & gemmu_iswi == "Weekly") {
    return("wday")
  }
  if (base_unit == "days" & gemmu_iswi == "Monthly") {
    return("mday")
  }
  if (base_unit == "days" & gemmu_iswi == "Yearly") {
    return("yday")
  }
  if (base_unit == "months" & gemmu_iswi == "Quarterly") {
    return("mquarter")
  }
  if (base_unit == "months" & gemmu_iswi == "Yearly") {
    return("month")
  }
  if (base_unit == "quarters" & gemmu_iswi == "Yearly") {
    return("quarter")
  }
}

#' Detect all possible growth rates based on Time Unit
#' @author Farid Azouaou
#' @param base_unit time unit
#' @return a vector containing possible growth rates.
#' @export
gemmu_yellan_f <- function(base_unit = NULL) {
  if (base_unit == "minutes") {
    return(c("Minutes", "Hourly"))
  }
  if (base_unit == "minutes") {
    return(c("Minutes", "Hourly"))
  }
  if (base_unit == "hours") {
    return(c("Hourly", "Daily"))
  }
  if (base_unit == "days") {
    return(c("Daily", "Weekly", "Monthly"))
  }
  if (base_unit == "weeks") {
    return(c("Weekly", "Monthly", "Quarterly"))
  }
  if (base_unit == "months") {
    return(c("Monthly", "Quarterly", "Yearly"))
  }
  if (base_unit == "quarters") {
    return(c("Quarterly", "Yearly"))
  }
  if (base_unit == "years") {
    return(c("Yearly"))
  }
}
tezmer_i_gemmu <- function(x) {
  return(!any(na.omit(x) <= 0))
}

#' Growth Rate
#' @author Farid Azouaou
#' @param tisefka_report Data exploration report
#' @param gemmu_iswi target growth rate
#' @param d_tirni exhaustive or grouped by
#' @return  data frame containing growth rate
#' @export

rate_n_gemmu_f <- function(tisefka_report = NULL, gemmu_iswi = NULL, d_tirni = NULL) {
  tisefka <- tisefka_report$tisefka
  target_variable <- tisefka_report$target_ts
  base_unit <- tisefka_report$time_unit
  gemmu <- tezmer_i_gemmu(x = tisefka[, target_variable])
  akka_ukuden <- c("Seconds", "Minutes", "Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly")
  names(akka_ukuden) <- c("seconds", "minutes", "hours", "days", "weeks", "months", "quarters", "years")
  if (akka_ukuden[base_unit] != gemmu_iswi) {
    tisefka_n_gemmu <- dplyr::tbl_df(data.frame(date = seq(from = min(tisefka$date), to = max(tisefka$date), by = base_unit)))
    tisefka_n_gemmu <- tisefka_n_gemmu %>% dplyr::left_join(tisefka, by = "date")
    gemmu_ig_zemren <- c("minute", "hour", "day", "wday", "mday", "month", "quarter", "mquarter", "year")
    gemmu <- dplyr::tbl_df(timetk::tk_get_timeseries_signature(tisefka_n_gemmu$date))
    gemmu <- gemmu %>% dplyr::mutate(mquarter = (month %% 4) + 1)
    gemmu <- gemmu[, gemmu_ig_zemren]
    gemmu <- gemmu %>% dplyr::select_if(~ length(unique(.)) > 1)
    tisefka_n_gemmu <- dplyr::bind_cols(tisefka_n_gemmu, gemmu)
    #------------------------
    zuzer_s <- gemmu_detect_frequency(base_unit = base_unit, gemmu_iswi = gemmu_iswi)
    gemmu <- tisefka_n_gemmu %>% tidyr::spread(!!zuzer_s, !!target_variable)
    asurif <- length(unique(tisefka_n_gemmu %>% dplyr::pull(!!zuzer_s)))
    gemmu <- gemmu %>% dplyr::select(c((ncol(gemmu) - asurif + 1):ncol(gemmu)))
    gemmu <- apply(gemmu, 2, function(x) basic_growth_rate(x, asurif = asurif))

    gemmu_yellan <- colnames(gemmu[[1]])
    gemmu <- dplyr::tbl_df(do.call(cbind, gemmu))
    gemmu <- sapply(gemmu_yellan, function(x) rowSums(gemmu[, grepl(x, colnames(gemmu))], na.rm = TRUE))
    gemmu <- dplyr::tbl_df(gemmu)
    gemmu <- dplyr::bind_cols(tisefka_n_gemmu, gemmu)
    gemmu <- gemmu %>% dplyr::filter(!is.na(target_variable))
    gemmu <- gemmu[, c("date", gemmu_yellan)]
  } else {
    tisefka_n_gemmu <- dplyr::tbl_df(data.frame(date = seq(from = min(tisefka$date), to = max(tisefka$date), by = base_unit)))
    tisefka_n_gemmu <- tisefka_n_gemmu %>% dplyr::left_join(tisefka, by = "date")
    # tisefka_n_gemmu <- tisefka%>%
    #   complete(date = seq(min(date), max(date), by=base_unit))
    gemmu <- basic_growth_rate(tisefka_n_gemmu %>% dplyr::pull(!!target_variable))
    gemmu_yellan <- colnames(gemmu)
    gemmu <- dplyr::bind_cols(tisefka_n_gemmu, gemmu)
    gemmu <- gemmu %>% dplyr::filter(!is.na(target_variable))
    gemmu <- gemmu[, c("date", gemmu_yellan)]
  }
  return(gemmu)
}
#' Display Growth rate results in chart.
#' @author Farid Azouaou
#' @param gemu_tisefka data frame containing growth rate
#' @return plotly object
#' @export

sekned_gemmu_f <- function(gemu_tisefka = NULL) {
  #----------------------------
  gemmu_tisefka <- reshape2::melt(gemu_tisefka, id.vars = "date")
  p <- gemmu_tisefka %>%
    plotly::plot_ly(x = ~date, y = ~value, type = "scatter", mode = "lines", color = ~variable) %>%
    plotly::layout(
      xaxis = base::list(title = paste("Time"), tickangle = -45),
      yaxis = base::list(title = "Growth Rate"),
      margin = base::list(b = 100),
      barmode = "stack", legend = list(orientation = "h", x = 0.35, y = 100)
    )
  p <- p %>% plotly::config(displaylogo = F)
  return(p)
  #----------------------------
}
