#' Anomaly Detection Pool (View)
#' @author  Farid Azouaou
#' @param anomalies_set a list of dataframes containing anomaly detection results
#' @return a DT table
#' @export

anomaly_to_DT_insight <- function(anomalies_set = NULL){
  my_date <- anomalies_set[[1]][,"date"]
  target_variables <- names(anomalies_set)
  anomalies_set <- anomalies_set%>%purrr::map(~dplyr::mutate(.x,corridor = upper_bound - lower_bound,anomaly_bin = as.numeric(!is.na(Anomalies)))%>%
                                                dplyr::mutate(deviation = anomaly_bin *(observed-observed_cleaned)/corridor)%>%
                                                dplyr::select(observed,anomaly,deviation))

  anomalies_set <- names(anomalies_set)%>%purrr::map(~anomalies_set[[.x]]%>%rename_all( function(.){paste(.x,c("observed","anomaly","deviation"),sep="_")} ))


  anomalies_set<-anomalies_set%>%dplyr::bind_cols()
  anomalies_set<- my_date%>%dplyr::bind_cols(anomalies_set)

  colnames(anomalies_set)<- gsub("_observed","",colnames(anomalies_set))
  value_columns  <- anomalies_set%>%dplyr::select(dplyr::contains("_deviation"))%>%colnames()
  columns2hide   <-grep("_deviation|anomaly",colnames(anomalies_set))
  anomaly_DT <- anomalies_set%>%DT::datatable(extensions = 'Scroller',options=list(deferRender = TRUE, scrollY = 200, scroller = TRUE,columnDefs = list(list(visible=FALSE, targets=columns2hide))))%>%
    DT::formatStyle(
    columns =target_variables ,
    valueColumns =value_columns,
    color = DT::styleInterval(c(-0.01, 0.01), c('white', 'black', 'white')),
    backgroundColor = DT::styleInterval(c(-0.01, 0.01), c('orange', 'lightgreen', 'brown'))    )
  return(anomaly_DT)
}
# target_variables <- colnames(economics)[-1]
# anomalies_set <-SaldaeDataExplorer::anomaly_detection_nnegh(tisefka = economics,target_ts = target_variables)
# aa <- anomaly_to_DT_insight(anomalies_set)
