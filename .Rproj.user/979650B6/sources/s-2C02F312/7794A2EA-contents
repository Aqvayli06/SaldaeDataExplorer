
#' Data Aggregation based on a given time unit and grouping pattern
#' @author Farid Azouaou
#' @param  tisefka  data frame including date variable
#' @param  tisefka_report exploratory report of raw data
#' @param  sdukkel group by a discrete variable
#' @param  base_unit time unit of the raw data
#' @param  time_unit time unit of the aggregated data
#' @param  aggregation_metric aggregation metric (sum mean max min)
#' @return list containing aggregated data
data_aggregation_f <- function(tisefka = NULL, tisefka_report = NULL, time_unit = NULL, base_unit = NULL, target_ts = NULL, sdukkel = NULL) {
  report_output <- base::list()
  if (time_unit != base_unit) {
    #-----------------------------------------------
    if (time_unit == "seconds") {
      #--------- unit which helps to determine on which frequency to summarize
      unit_format <- "%Y-%m-%d %H:%M:%S"
      #----------tasetta is when the upper unit does not have enough data for the last point (10 months in the last year)
    }
    if (time_unit == "minutes") {
      #--------- unit which helps to determine on which frequency to summarize
      unit_format <- "%Y-%m-%d %H:%M"
    }
    if (time_unit == "hours") {
      unit_format <- "%Y-%m-%d %H"
    }
    if (time_unit == "days") {
      unit_format <- "%Y-%m-%d"
    }
    if (time_unit == "months") {
      unit_format <- "%Y-%m"
    }
    if (time_unit == "years") {
      unit_format <- "%Y"
    }
    #--------------------- average
    if (!time_unit %in% c("weeks", "quarters")) {
      time_vect_group <- format(as.POSIXct(rownames(tisefka), tz = "CET"), unit_format)
    } else if (time_unit == "quarters") {
      time_vect_group <- paste(format(as.POSIXct(rownames(tisefka), tz = "CET"), "%Y"), lubridate::quarter(rownames(tisefka)))
    } else if (time_unit == "weeks") {
      time_vect_group <- paste(format(as.POSIXct(rownames(tisefka), tz = "CET"), "%Y"), lubridate::week(rownames(tisefka)))
    } else {
      stop("time unit not found")
    }
    tasetta <- detect_number_basis_units_in_upper_unit(upper_unit = time_unit, basis_unit = base_unit, last_date = tail(as.POSIXct(rownames(tisefka), tz = "CET"), 1))

    if (length(unique(tail(time_vect_group, tasetta))) > 1) {
      tasetta_dat <- tail(time_vect_group, tasetta)
      incomplete <- length(which(tasetta_dat == tail(tasetta_dat, 1)))
      time_vect_group <- head(time_vect_group, -incomplete)
      tisefka <- head(tisefka, -incomplete)
    }

    #-----------------------------
    aqerru <- detect_number_basis_units_in_upper_unit(upper_unit = time_unit, basis_unit = base_unit, last_date = head(as.POSIXct(rownames(tisefka), tz = "CET"), 1))
    if (length(unique(head(time_vect_group, aqerru))) > 1) {
      aqerru_dat <- head(time_vect_group, aqerru)
      incomplete <- length(which(aqerru_dat == head(aqerru_dat, 1)))
      time_vect_group <- tail(time_vect_group, -incomplete)
      tisefka <- head(tisefka, -incomplete)
    }
    #------------------------------
    date_by_unit <- data.frame(time_vect_group, date = rownames(tisefka)) %>% dplyr::distinct(time_vect_group, .keep_all = TRUE)

    target_dat <- tisefka[, target_ts, drop = F]
    colnames(target_dat) <- "target_variable"
    if (!is.null(sdukkel)) {
      target_dat$sdukkel <- tisefka[, sdukkel]
    } else {
      target_dat$sdukkel <- target_ts
      sdukkel <- target_ts
    }
    target_dat["date"] <- rownames(tisefka)
    upper_summary_mean <- target_dat %>%
      dplyr::mutate(upper_unit = time_vect_group) %>%
      dplyr::group_by(upper_unit, sdukkel) %>% # group by the day column
      dplyr::summarise(m = mean(target_variable, na.rm = TRUE)) %>%
      tidyr::spread(sdukkel, m)
    upper_summary_mean <- dplyr::tbl_df(data.frame(upper_summary_mean, check.names = FALSE)) %>%
      dplyr::mutate(date = as.POSIXct(date_by_unit$date, tz = "CET"))
    #--------------------- Sum --------------
    upper_summary_sum <- target_dat %>%
      dplyr::mutate(upper_unit = time_vect_group) %>%
      dplyr::group_by(upper_unit, sdukkel) %>% # group by the day column
      dplyr::summarise(m = sum(target_variable, na.rm = TRUE)) %>%
      tidyr::spread(sdukkel, m)
    upper_summary_sum <- dplyr::tbl_df(data.frame(upper_summary_sum, check.names = FALSE)) %>%
      dplyr::mutate(date = as.POSIXct(date_by_unit$date, tz = "CET"))

    #--------------------- maximum
    upper_summary_max <- target_dat %>%
      dplyr::mutate(upper_unit = time_vect_group) %>%
      dplyr::group_by(upper_unit, sdukkel) %>% # group by the day column
      dplyr::summarise(m = max(target_variable, na.rm = TRUE)) %>%
      tidyr::spread(sdukkel, m)
    upper_summary_max <- dplyr::tbl_df(data.frame(upper_summary_max, check.names = FALSE)) %>%
      dplyr::mutate(date = as.POSIXct(date_by_unit$date, tz = "CET"))

    #--------------------- min
    upper_summary_min <- target_dat %>%
      dplyr::mutate(upper_unit = time_vect_group) %>%
      dplyr::group_by(upper_unit, sdukkel) %>% # group by the day column
      dplyr::summarise(m = min(target_variable, na.rm = TRUE)) %>%
      tidyr::spread(sdukkel, m)
    upper_summary_min <- dplyr::tbl_df(data.frame(upper_summary_min, check.names = FALSE)) %>%
      dplyr::mutate(date = as.POSIXct(date_by_unit$date, tz = "CET"))


    # colnames(upper_summary_sum) <-colnames(upper_summary_mean)  <- c(time_unit ,target_ts,"date")
    # colnames(upper_summary_max) <-colnames(upper_summary_min) <- c(time_unit ,target_ts,"date")
    report_output[["ts_aggregated_by_sum"]] <- upper_summary_sum
    report_output[["ts_aggregated_by_average"]] <- upper_summary_mean
    report_output[["ts_aggregated_by_max"]] <- upper_summary_max
    report_output[["ts_aggregated_by_min"]] <- upper_summary_min
    #----------------------
    report_output <- lapply(report_output, function(x) x[, -1])
    #------------------------------
  } else {
    tisefka[, "date"] <- as.POSIXct(rownames(tisefka))
    if (!is.null(sdukkel)) {
      target_dat <- tisefka[, c(target_ts, "date")]
      colnames(target_dat) <- c("target_variable", "date")
      target_dat$sdukkel <- tisefka[, sdukkel]
      target_dat <- dplyr::tbl_df(target_dat)
      report_output$tisefka_ticeqfin <- target_dat %>%
        tidyr::spread(unique(sdukkel), target_variable)
    }
    report_output$tisefka <- tisefka[, c(target_ts, "date")]
  }
  return(report_output)
}
#---------------------------------------


#' Data Exploration Main wrapper function
#' @author Farid Azouaou
#' @param  tisefka  data frame including date variable
#' @param  tisefka_report exploratory report of raw data
#' @param  sdukkel group by a discrete variable
#' @param  sdukkel_ih logical variable stating whether to group or no
#' @param  base_unit time unit of the raw data
#' @param  time_unit time unit of the aggregated data
#' @param  aggregation_metric aggregation metric (sum mean max min)
#' @return list containing aggregated data with seasonally adjusted and anomaly detection infrmation

data_exloration_aqerru <- function(tisefka = NULL, tisefka_report = NULL, sdukkel = NULL,
                                   sdukkel_ih = NULL, aggregation_metric = NULL, time_unit = NULL, base_unit = NULL, target_ts = NULL, ts_actions = NULL) {
  if (is.null(tisefka)) {
    return(NULL)
  }
  report_output <- base::list()
  #----------------------
  if (is.null(sdukkel_ih)) {
    sdukkel <- NULL
  } else if (sdukkel_ih == FALSE) {
    sdukkel <- NULL
  }
  if (time_unit == base_unit) aggregation_metric <- NULL
  report_output_data <- data_aggregation_f(
    tisefka = tisefka,
    tisefka_report = tisefka_report,
    sdukkel = sdukkel,
    time_unit = time_unit,
    base_unit = base_unit, target_ts = target_ts
  )
  #----------------------------------------------


  if (is.null(aggregation_metric)) {
    if (!is.null(sdukkel)) tisefka <- report_output_data$tisefka_ticeqfin
    if (is.null(sdukkel)) tisefka <- report_output_data$tisefka
  } else {
    tisefka <- report_output_data[[paste0("ts_aggregated_by_", tolower(aggregation_metric))]]
  }
  #----------------------seasonal adjustement
  season_adjust <- FALSE
  if (is.null(sdukkel)) {
    report_output$seasonal_adjustement <- season_adjust_f(ts_x = tisefka[, target_ts, drop = T], time_unit = time_unit)
    season_adjust <- TRUE %in% grepl("seas_adjust_", names(report_output$seasonal_adjustement))
  }
  #----------------------
  if (season_adjust == TRUE) {
    seaso_indx <- grepl("seas_adjust_", names(report_output$seasonal_adjustement))
    tisefka <- dplyr::tbl_df(tisefka)
    tisefka <- tisefka %>% dplyr::mutate(Seas_Adju = report_output$seasonal_adjustement[seaso_indx][[1]])
  }
  report_output$tella_anomaly <- FALSE
  if (nrow(tisefka) < 50000) anomaly_detection <- TRUE
  if (anomaly_detection == TRUE & is.null(sdukkel)) {
    tisefka <- anomaly_detection_nnegh(tisefka = tisefka, anomaly_mode = "anomalize", target_ts = target_ts)
    report_output$tella_anomaly <- "Anomalies" %in% colnames(tisefka)
  }
  report_output$tisefka <- tisefka
  report_output$group_by <- sdukkel
  report_output$time_unit <- time_unit
  report_output$target_ts <- target_ts

  report_output$season_adjust <- season_adjust
  report_output$aggregation_metric <- aggregation_metric

  #---------------------------------------------
  report_output$data_aggregated <- report_output_data
  # report_output$data_aggregated_plot <- sekned_tisefka(tisefka = tisefka,aggregation=aggregation_metric ,time_unit= time_unit, include_smoothing = smoothing,season_adjust = season_adjust, color_by = sdukkel,plot_theme=NULL)
  #----------------------------------------------
  return(report_output)
}
#---------------------------- data exploration (boxplot)-----------------

#' Display box plot

#' @author Farid Azouaou
#' @param  tisefka  data frame including date variable
#' @param  x explaining variable
#' @param  y explained variable
#' @param  aggregation_metric aggregation metric (sum mean max min)
#' @return a boxplot plotly object


data_exloration_box <- function(tisefka = NULL, x = NULL, y = NULL, aggregation_metric = "Sum") {
  #-------------------------------
  if (is.null(x)) {
    p <- plotly::plot_ly(x = ~ tisefka[, y], type = "box", text = y, name = y)
    p <- p %>% plotly::layout(title = paste(y, "Statistics Overview"), xaxis = base::list(title = ""), yaxis = base::list(title = "y"))
  } else {
    tisefka$x_temporary <- as.factor(tisefka[, x])
    tisefka$y_temporary <- tisefka[, y]

    p <- plotly::plot_ly(tisefka, y = ~y_temporary, color = ~x_temporary, type = "box")
    p <- p %>%
      plotly::layout(
        title = paste(y, "statistics overview  group by", x),
        yaxis = base::list(title = y)
      )
  }

  return(p)
}

#' Time based Seasonality frequencies
#' @description detect based on time unit possible frequencies
#' @author Farid Azouaou
#' @param  time_unit time unit used to detect frequency
#' @return a vector of existing frequencies for that time unit

time_based_seasonality <- function(time_unit = NULL) {
  #----------------------
  if (time_unit == "seconds") {
    return(c(10, 60))
  }
  if (time_unit == "minutes") {
    return(c(10, 60))
  }
  if (time_unit == "hours") {
    return(c(4, 6, 8, 12, 24))
  }
  if (time_unit == "days") {
    return(c(5, 7, 30))
  }
  if (time_unit == "weeks") {
    return(c(4))
  }
  if (time_unit == "months") {
    return(c(3, 6, 12))
  }
  if (time_unit == "quarters") {
    return(4)
  }
  if (time_unit == "years") {
    return(10)
  }
  #----------------------
}
#' Seasonal Adjustement
#' @description extract from time series the seasonal , trend and remainder components based on a time frequency that we extract from time unit
#' @author Farid Azouaou
#' @param  ts_x  target time series
#' @param  time_unit time unit used to detect frequency
#' @return a list containing frequency and seasonally adjusted time serie

season_adjust_f <- function(ts_x = NULL, time_unit = NULL) {
  #-----------------------------
  output_season <- base::list()
  ts_x <- interp_na_value(ts_x = ts_x, mode = "spline")
  tuzyat <- time_based_seasonality(time_unit = time_unit)
  #-------------------
  tuzyat <- c(tuzyat, forecast::findfrequency(x = ts_x))
  output_season$ts_orig <- ts_x
  #----------------------------
  tuzyat <- unique(tuzyat)
  for (tuzya in tuzyat) {
    tes3a_ddurt <- FALSE
    if (tuzya > 1 & length(ts_x) / tuzya > 3) {
      tes3a_ddurt <- seastests::isSeasonal(ts(ts_x, frequency = tuzya), test = "wo", freq = NA)
      ts_temp <- forecast::seasadj(stats::stl(x = ts(ts_x, frequency = tuzya), s.window = 2, s.degree = 1))
      output_season[[paste0("seas_adjust_", tuzya)]] <- as.numeric(ts_temp)
    } else {
      output_season[[paste0("seas_adjust_", tuzya)]] <- NULL
    }
    output_season[[paste0("freq_", tuzya)]] <- tes3a_ddurt
  }
  #-------------------
  return(output_season)
  #-------------------
}
#'Display raw data including additional insights such seasonally adjusted , outliers(anomalies) and quadratic fit
#' @author Farid Azouaou
#' @param tisefka_report list containing exploratory report
#' @param ts_actions
#' @param sekned_settings: display settings
#' @param graph_type Graph type whether a "marker" , "line" , "marker+line", "historgram", "density"
#' @return plolty reactive object

sekned_tisefka <- function(tisefka_report = NULL, ts_actions = TRUE, graph_type = "Lines",plot_settings= NULL) {
  # Base plot with date axis
  if (is.null(tisefka_report)) {
    return(tisefka_report)
  }
  tisefka <- tisefka_report$tisefka
  color_by <- tisefka_report$group_by
  time_unit <- tisefka_report$time_unit
  # season_adjust  <-tisefka_report$season_adjust
  aggregation <- tisefka_report$aggregation_metric
  target_ts <- tisefka_report$target_ts
  season_adjust <- include_smoothing <- svegned_anomaly <- FALSE

  if (length(ts_actions) > 0) {
    include_smoothing <- "Smoothing" %in% ts_actions
    season_adjust <- ("Season. Adjust" %in% ts_actions) & (tisefka_report$season_adjust)
    svegned_anomaly <- ("Anomaly" %in% ts_actions) & (tisefka_report$tella_anomaly)
  }
  if (season_adjust != TRUE) {
    tisefka$Seas_Adju <- NULL
  }

  x_axis <- paste("Time in ", time_unit)
  graph_type <- base::tolower(graph_type)
  if (is.null(color_by)) {
    cn <- colnames(tisefka)
    if (is.null(aggregation)) {
      cn_new <- c("target_variable", "date")
      if (season_adjust == TRUE) {
        cn_new <- c(cn_new, "Seas_Adju")
      }
      if (svegned_anomaly == TRUE) {
        cn_new <- c(cn_new, "Anomalies")
      }
      y_ax <- cn[1]
    } else {
      cn_new <- c("target_variable", "date")
      if (season_adjust == TRUE) {
        cn_new <- c(cn_new, "Seas_Adju")
      }
      if (svegned_anomaly == TRUE) {
        cn_new <- c(cn_new, "Anomalies")
      }

      y_ax <- cn[1]
      #-------------------------
    }

    colnames(tisefka) <- cn_new
    graph_fill <- bar_type <- graph_mode <- NULL
    if(is.null(plot_settings)){
      RAW_col      <- "#00AFBB"
      SA_col       <- "brown"
      Anomaly_col  <- "orange"
    }else{
      RAW_col      <- plot_settings$colors_inu[1]
      SA_col       <- plot_settings$colors_inu[2]
      Anomaly_col  <- plot_settings$colors_inu[3]
    }
    if (graph_type %in% c("markers")) {
      graph_type <- "scatter"
      graph_mode <- "markers"
      RAW_col <- NULL
    }
    if (graph_type %in% c("lines", "lines+markers")) {
      graph_mode <- graph_type
      graph_type <- "scatter"
    }
    if (graph_type %in% c("filled")) {
      graph_mode <- "lines"
      graph_type <- "scatter"
      graph_fill <- "tozeroy"
    }
    if (graph_type %in% c("bar1")) {
      graph_type <- "bar"
      bar_type <- NULL
    }
    if (graph_type %in% c("density")) {
      tisefka <- stats::density(tisefka[, "target_variable", drop = T])
      tisefka <- data.frame(date = tisefka$x, target_variable = tisefka$y)
      graph_mode <- "lines"
      graph_type <- "scatter"
      graph_fill <- "tozeroy"
      x_axis <- y_ax
      y_ax <- "probability"
      season_adjust <- include_smoothing <- svegned_anomaly <- FALSE
    }

    p <- plotly::plot_ly(tisefka) %>%
      plotly::add_trace(
        x = ~date, y = ~target_variable, type = graph_type, mode = graph_mode, fill = graph_fill, name = y_ax, yaxis = "y",
        line = base::list(color = RAW_col),
        text = ~ paste(y_ax)
      ) %>%
      plotly::layout(
        title = NULL,
        xaxis = base::list(title = x_axis),
        yaxis = base::list(title = y_ax, side = "left", showgrid = FALSE, zeroline = FALSE)
      )
    if (svegned_anomaly == TRUE) {
      p <- p %>%
        plotly::add_trace(x = ~date, y = ~Anomalies, type = "scatter", mode = "markers", name = "Anomalies", yaxis = "y"
                          )
    }
    if (season_adjust == TRUE) {
      p <- p %>%
        plotly::add_trace(
          x = ~date, y = ~Seas_Adju, type = "scatter", mode = "lines", name = "SA", yaxis = "y",
          line = base::list(color = SA_col)
        )
    }
    if (include_smoothing == TRUE) {
      xx <- 1:nrow(tisefka)
      smoothing <- stats::loess(target_variable ~ xx, data = tisefka)
      smoothing.pred <- predict(smoothing, se = TRUE)
      ll.df <- data.frame(
        x = smoothing$x, fit = smoothing$fit,
        lb = smoothing$fit - (1.96 * smoothing$s),
        ub = smoothing$fit + (1.96 * smoothing$s)
      )

      p <- plotly::add_lines(p, x = ~date, y = smoothing.pred$fit, name = "Smoothing", line = base::list(color = "#E7B800", width = 2))
      p <- plotly::add_ribbons(p,
        x = ~date, ymin = ll.df$lb, ymax = ll.df$ub, name = "95% CI", line = base::list(opacity = 0.4, width = 0, color = "#E7B800"),
        fillcolor = "rgba(7, 164, 181, 0.2)"
      )
      p <- plotly::layout(p, y_axis = y_ax)
    }
    p <- plotly::layout(p, legend = list(orientation = "h", x = 0.35, y = 100))
    return(p)
  }
  if (graph_type == "density") {
    date_den <- tisefka %>%
      dplyr::select(-date) %>%
      dplyr::select(1)
    date_den <- stats::density(date_den[, 1, drop = T], na.rm = TRUE)$x
    tisefka <- apply(tisefka %>% dplyr::select(-date), 2, function(x) stats::density(x, na.rm = TRUE)$y)
    tisefka <- data.frame(date = date_den, tisefka, check.names = FALSE)
  }
  tisefka <- dplyr::tbl_df(reshape2::melt(tisefka, id = "date"))
  if (graph_type == "lines") {
    p <- tisefka %>%
      plotly::plot_ly(x = ~date, y = ~value, type = "scatter", mode = "lines", color = ~variable) %>%
      plotly::layout(
        xaxis = base::list(title = paste("Time in", time_unit), tickangle = -45),
        yaxis = base::list(title = target_ts),
        margin = base::list(b = 100),
        barmode = "stack"
      )
  }

  if (graph_type == "stack") {
    p <- tisefka %>%
      plotly::plot_ly(x = ~date, y = ~value, type = "bar", color = ~variable) %>%
      plotly::layout(
        xaxis = base::list(title = paste("Time in", time_unit), tickangle = -45),
        yaxis = base::list(title = target_ts),
        margin = base::list(b = 100),
        barmode = "stack"
      )
  }
  if (graph_type == "bar") {
    p <- tisefka %>%
      plotly::plot_ly(x = ~date, y = ~value, type = "bar", color = ~variable) %>%
      plotly::layout(
        xaxis = base::list(title = paste("Time in", time_unit), tickangle = -45),
        yaxis = base::list(title = target_ts),
        margin = base::list(b = 100),
        barmode = "group"
      )
  }
  if (graph_type == "scatter") {
    p <- tisefka %>%
      plotly::plot_ly(x = ~date, y = ~value, type = "scatter", color = ~variable) %>%
      plotly::layout(
        xaxis = base::list(title = paste("Time in", time_unit), tickangle = -45),
        yaxis = base::list(title = target_ts),
        margin = base::list(b = 100)
      )
  }
  if (graph_type == "density") {
    p <- tisefka %>%
      plotly::plot_ly(x = ~date, y = ~value, type = "scatter", color = ~variable, fill = "tozeroy", mode = "line") %>%
      plotly::layout(
        xaxis = base::list(title = paste("Time in", time_unit), tickangle = -45),
        yaxis = base::list(title = target_ts),
        margin = base::list(b = 100)
      )
  }
  p <- plotly::layout(p, title = NULL, legend = list(orientation = "h", x = 0.35, y = 100))
  return(p)
}


annayen_n_tumura_f <- function() {
  flags <- c(
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/au.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/jp.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/se.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fr.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg"
  )
  names(flags) <- c("AUS", "GER", "UK", "JAP", "SWD", "FRA", "USA")
  warning("wer3ad tfuk tagi n w annayen!!!")
  return(flags)
}
data_exploration_pie <- function(tisefka = NULL, x = NULL, y = NULL, aggregation_metric = "sum", bgrnd_color = "black") {

  # Add label position
  if (is.null(x)) {
    return(NULL)
  }

  tisefka$x_temporary <- as.factor(tisefka[, x])
  tisefka$y_temporary <- tisefka[, y]
  tisefka <- dplyr::tbl_df(tisefka)
  tisefka <- tisefka %>%
    dplyr::group_by(x_temporary) %>%
    dplyr::summarise(count = n())

  p <- tisefka %>%
    plotly::plot_ly(labels = ~x_temporary, values = ~count) %>%
    plotly::add_pie(hole = 0.6) %>%
    plotly::layout(
      title = paste("Categorical variables overview:", x), showlegend = T,
      paper_bgcolor = bgrnd_color,
      plot_bgcolor = bgrnd_color,
      xaxis = base::list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = base::list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    ) %>%
    plotly::config(displaylogo = F)

  return(p)
}

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
#'Detect
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

#' rearrange into Data frame to read it from DT functions
sekned_tisefka_DT <- function(tisefka = NULL, gzem = 3) {
  rownames(tisefka) <- tisefka$date
  tisefka$date <- NULL
  tisefka <- round(tisefka, gzem)
  return(tisefka)
}
#' Reactive Data Exploration including statistics data quality and graphic output.
#' @param tisefka Raw data(row names as date)
#' @author Farid Azouaou
#' @param numeric_variables the numerical variables
#' @param tisefka_report Data report including outliers , statistics and quality information
#' @return  datahandontable object including start end date , statistics outliers information and charts(boxplot,distribution and raw plot)
#' @export

Handson_exploration <- function(tisefka = NULL, tisefka_report = NULL, numeric_variables = NULL) {
  tisefka <- tisefka[, numeric_variables, drop = F]
  tisefka_density <- apply(tisefka, 2, function(x) stats::density(na.omit(x))$y)
  relevant_variables <- c("variables", "outliers_cnt")
  DF <- tisefka_report$outliers[, relevant_variables]
  DF <- cbind(DF, ukud_tilisa_f(tisefka = tisefka))
  DF_stat <- data.frame(tisefka_report$beschreibung[, c("n", "na", "mean", "sd")])
  DF <- cbind(DF, DF_stat)

  # DF <- DF[numeric_variables, ]
  DF$Chart <- sapply(
    1:ncol(tisefka),
    function(x) {
      jsonlite::toJSON(list(
        values = tisefka[, x],
        options = list(type = "line",col="green")
      ),
      na="null")
    }
  )
  DF$Density <- sapply(
    1:ncol(tisefka),
    function(x) {
      jsonlite::toJSON(list(
        values = tisefka_density[, x],
        options = list(type = "line")
      ))
    }
  )
  DF$Box <- sapply(
    1:ncol(tisefka),
    function(x) {
      jsonlite::toJSON(list(
        values = tisefka[, x],
        options = list(type = "box")
      ),na="null")
    }
  )

  rh_plot <- rhandsontable::rhandsontable(DF, rowHeaders = NULL, width = 1000, height = 300) %>%
    rhandsontable::hot_col("Chart", renderer = htmlwidgets::JS("renderSparkline")) %>%
    rhandsontable::hot_col("Density", renderer = htmlwidgets::JS("renderSparkline")) %>%
    rhandsontable::hot_col("Box", renderer = htmlwidgets::JS("renderSparkline"))
  # rhandsontable(DF, rowHeaders = NULL, width = 550, height = 300) %>%
  #   rhandsontable::hot_col("chart", renderer = htmlwidgets::JS("renderSparkline"))
  return(rh_plot)
}
