#' @title Calculation of daily statistics for dendrometer data
#'
#' @description This function calculates various statistics of dendrometer data on a daily basis. The daily statistics includes the daily maximum and minimum with their corresponding times and daily amplitude (difference between daily maximum and minimum). See King et al. (2013) for details.
#'
#' @references King G, Fonti P, Nievergelt D, Büntgen U, Frank D (2013) Climatic drivers of hourly to yearly tree radius variations along a 6°C natural warming gradient. Agricultural and Forest Meteorology 168:36–46. \doi{10.1016/j.agrformet.2012.08.002}
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.
#'
#' @param TreeNum numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.
#'
#' @return A dataframe with the daily statistics of the dendrometer data that contains:
#' \tabular{llll}{
#' \strong{Columns}\tab\tab   \strong{Description}\cr
#' \code{DATE}\tab\tab    The day of year in "yyyy-mm-dd".\cr
#' \code{Min}\tab\tab    The minimum value record for the corresponding day.\cr
#' \code{Time_min}\tab\tab    The time when minimum value recorded for the corresponding day.\cr
#' \code{Max}\tab\tab    The maximum value record for the corresponding day.\cr
#' \code{Time_max}\tab\tab    The time when maximum value recorded for the corresponding day.\cr
#' \code{mean}\tab\tab    The daily average value of the dendrometer reading.\cr
#' \code{median}\tab\tab    The daily median value of the dendrometer reading.\cr
#' \code{amplitude}\tab\tab   The difference between daily maximum and daily minimum.\cr
#' \code{Remarks}\tab\tab   "*" if Time_max > Time_min otherwise "".\cr
#' }
#'
#' @importFrom stats approx median na.exclude na.omit sd
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when summarize all_of
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(nepa17)
#' daily_stats<-daily.data(df=nepa17, TreeNum=1)
#' head(daily_stats,10)}
#'
#' @export
daily.data<-function(df, TreeNum){
  if(TreeNum > ncol(df)-1){
    stop("The tree number is not valid for provided dataset.")
  }
  TIME <- DATE <- dm <- time <- Max <- Min<- Time_max <- Time_min <- NULL
  tn <- TreeNum+1
  if (!inherits(df[[1]], 'Date') && !inherits(df[[1]], 'POSIXct')) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  df <- tibble(df)%>%
    select(c(1,all_of(tn)))
  df <- df%>%
    rename(TIME = 1,
           dm = 2)%>%
    mutate(DATE = as.Date(TIME),
           time = format(TIME, "%H:%M:%S"))
  result <- df%>%
    group_by(DATE) %>%
    summarize(
      Min = min(dm, na.rm = T),
      Time_min = time[which.min(dm)],
      Max = max(dm, na.rm = T),
      Time_max = time[which.max(dm)],
      mean = mean(dm, na.rm = T),
      median = median(dm, na.rm=T),
      amplitude = Max-Min,
      Remarks = ifelse(Time_max>Time_min, '*', '')
    )
  return(result)
}


