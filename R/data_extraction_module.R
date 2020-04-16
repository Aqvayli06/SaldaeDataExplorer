#' Read data from inputfile (CSV)
#' @author Farid Azouaou
#' @param input_file input file
#' @return tbl_df object

ghred_tisefka_csv <- function(input_file = NULL) {
  tisefka_csv <- readr::read_csv(
    file = input_file$datapath,
    # header = TRUE,
    # delim = ",",
    na = c("", "NA","ND", " ", "-", ".", "#")
  )
  tisefka_csv <- data.frame(tisefka_csv, check.names = FALSE)
  return(tisefka_csv)
}


#' Read data from inputfile (Excel)
#' @author Farid Azouaou
#' @param input_file inputfile object
#' @param tawriqt excel sheet name
#' @return tbl_df object

ghred_tisefka_excel <- function(input_file = NULL, tawriqt = NULL) {
  tisefka_excel <- readxl::read_excel(
    path = input_file$datapath,
    # header = TRUE,
    # delim = ",",
    sheet = tawriqt,
    na = c("", "NA","ND", " ", "-", ".", "#")
  )
  tisefka_excel <- data.frame(tisefka_excel, check.names = FALSE)
  return(tisefka_excel)
}

#' Read data main function
#' @author Farid Azouaou
#' @param input_file input file
#' @param tala data source (CSV/Excel/Database)
#' @param tawriqt excel sheet name
#' @return tbl_df object
#' @export

ghred_tisefka_aqerru <- function(input_file = NULL, tala = NULL, tawriqt = NULL) {
  if (tala == "CSV") {
    tisefka <- ghred_tisefka_csv(input_file = input_file)
  }
  if (tala == "EXCEL") {
    tisefka <- ghred_tisefka_excel(input_file = input_file, tawriqt = tawriqt)
  }
  return(tisefka)
}

#' Saldae : is Date
#' @description Check if the intput element is a date based on a given date format
#' @author Farid Azouaou
#' @param mydate a character containing date information
#' @param SA_date_format date format (YYYY/mm/dd , YY-dd-mm)
#' @return idk


IsDate <- function(mydate, SA_date_format) {
  tryCatch(!is.na(base::as.POSIXct(mydate, "", format = SA_date_format)),
    error = function(err) {
      FALSE
    }
  )
}

#' Saldae duplicated dates in raw data
#' @description check if there are duplicated dates in raw data and if possible create new variables based on that
#' @author Farid Azouaou
#' @param tisefka raw data
#' @param date_index variable to use as a date
#' @return logical statement

tisefka_spread_yella <- function(tisefka = NULL, date_index = NULL) {
  if (is.null(tisefka)) {
    return(NULL)
  }
  date_index <- which(date_index == colnames(tisefka))
  if (date_index == 0)return(NULL)
  tisefka_diagnosis <- dlookr::diagnose(.data = tisefka)
  tisefka_diagnosis <- data.frame(tisefka_diagnosis, check.names = FALSE)
  rownames(tisefka_diagnosis) <- tisefka_diagnosis$variables
  if (tisefka_diagnosis[date_index, "unique_rate"] == 1) {
    return(NULL)
  }
  tisefka_diagnosis <- tisefka_diagnosis[-date_index, ]
  if (min(tisefka_diagnosis[, "unique_count"]) > 10) {
    return(NULL)
  }
  tisefka_diagnosis <- tisefka_diagnosis[tisefka_diagnosis[, "unique_count"] <= 10, ]
  if (max(tisefka_diagnosis[, "unique_count"]) == 1) {
    return(NULL)
  }
  return(tisefka_diagnosis[tisefka_diagnosis[, "unique_count"] <= 10, "variables"])
}

#' Saldae prepare data
#' @description Prepare raw data using date_variable as row names
#' @author Farid Azouaou
#' @param tisefka raw data(uploaded from inputfile)
#' @param date_variable variable to use as date
#' @param SA_date_format date format
#' @param spread_value value to spread data
#' @param spread_key   group category to use to spread data
#' @return raw data ready to explore and to analyze
#' @export

sbed_tisefka <- function(tisefka = NULL, date_variable = NULL, SA_date_format = "YYYY-MM-DD", spread_value = NULL, spread_key = NULL) {
  SA_date_format <- paste(SA_date_format, "H:M:S")
  if (is.null(tisefka)) {
    return(NULL)
  }
  DATE_index <- which(date_variable == colnames(tisefka))
  Date_telha <- IsDate(mydate = head(tisefka[, DATE_index]), SA_date_format = SA_date_format)
  if (!FALSE %in% Date_telha) {
    colnames(tisefka)[DATE_index] <- "date"
  } else {
    return(NULL)
  }
  if (!is.null(spread_value) & !is.null(spread_key)) {
    # print("duplicated data available")
    # tisefka2<<-tisefka
    # tisefka <- tisefka %>%tidyr::spread(key = spread_key,value = spread_value)
    # tisefka3<<-tisefka
  }

  tisefka <- tisefka %>% dplyr::distinct(date, .keep_all = TRUE)
  warning("There is high risk, that duplicated data are overwritten!!!!")
  rownames(tisefka) <- tisefka[, "date"]
  return(tisefka)
}

#' Saldae available date format
#' @author Farid Azouaou
#' @param ukud raw data
#' @return vector of date format
#' @export
SA_date_format_yellan <- function(ukud = NULL) {
  date_format_yellan <- c(
    "YYYY-MM-DD", "YYYY/MM/DD", "YYYY.MM.DD", "YYYYMMDD",
    "DD-MM-YYYY", "DD/MM/YYYY", "DD.MM.YYYY", "MM-DD-YYYY"
  )
  return(date_format_yellan)
}

#' Saldae arrange  data
#' @author Farid Azouaou
#' @param tifeka raw data
#' @return tbl_df object
fren_tisefka <- function(tifeka = NULL) {
  # tisefka    <- tisefka %>% dplyr::distinct(date, .keep_all = TRUE)
  tisefka <- tisefka %>% dplyr::arrange(as.POSIXct(date, tz = "CET"), .by_group = TRUE)
}


#' Time interval
#' @author Farid Azouaou
#' @param x time vector

ukud_aciwen <- function(x = NULL) {
  anda_tebda <- length(x) + 1 - length(zoo::na.trim(x, sides = "left"))
  anda_tfuk <- length(zoo::na.trim(x, sides = "right"))
  return(c(anda_tebda, anda_tfuk))
}


#' Saldae  Time interval
#' @author Farid Azouaou
#' @param tisefka data frame with dates as row names
#' @return time intervals
#' @export

ukud_tilisa_f <- function(tisefka = NULL) {
  ukud_tilisa <- t(apply(tisefka, 2, function(x) ukud_aciwen(x)))
  ukud_tilisa <- data.frame("Start Date" = rownames(tisefka)[ukud_tilisa[, 1]], "End Date" = rownames(tisefka)[ukud_tilisa[, 2]], check.names = FALSE)
  rownames(ukud_tilisa) <- colnames(tisefka)
  return(ukud_tilisa)
}
