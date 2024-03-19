#' @title Truncation of the dendrometer data
#'
#' @description This function is helpful to truncate dendrometer data for a user-defined period.
#'
#' @param df dataframe with the first column named date and time in the format \code{yyyy-mm-dd HH:MM:SS}.
#'
#' @param CalYear numerical value or array of two elements for the desired year of calculation.
#'
#' @param DOY numerical value or array of two elements representing the day of year. If we provide an array instead of a single value for \code{CalYear} and a single value for \code{DOY}, it truncates data from the \code{DOY} of the first \code{CalYear} to the same \code{DOY} of the second \code{CalYear}.  Conversely, if we provide one value for \code{CalYear} and an array of two elements for \code{DOY} truncates the data form first \code{DOY} to second \code{DOY} within the same \code{CalYear}. Finally, if we provide an array with two values for both \code{DOY} and \code{CalYear}, it truncates data from the first \code{DOY} of the first \code{CalYear} to the second \code{DOY} of second \code{CalYear}.
#'
#' @return A dataframe with the truncated data for the defined periods.
#'
#' @importFrom stats approx median na.exclude na.omit sd
#' @importFrom lubridate ymd_hms ymd year yday
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(nepa)
#' #Extracting data from doy 20 to 50 in 2017.
#' trunc1<-dendro.truncate(df=nepa, CalYear=2017, DOY=c(20,50))
#' head(trunc1,10)}
#'
#' @export
dendro.truncate<-function (df, CalYear,DOY){
  TIME <- yr.1 <- da.1 <- NULL
  cy<-CalYear
  doy<-DOY
  if (is.null(cy)|is.null(doy)){
    stop('Either "CalYear" and(or) "DOY" are empty.')
  }
  if(length(doy)>2|length(cy)>2){
    stop("The length of input 'CalYear' or 'DOY' is > 3.")
  }

  if (!inherits(df[[1]], 'Date') && !inherits(df[[1]], 'POSIXct')) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  data <- tibble(df)%>%
    rename(TIME = 1)

  yr=NULL
  data<-data %>%
    mutate(yr.1 = year(TIME),
          da.1 = yday(TIME))
  yr1 <- unique(data$yr.1)
  for(i in cy){
    if (i %in% yr1 == FALSE) {
      stop("Provided year does not exist in dataset.")
    }
  }
  if(length(doy)==2&length(cy==1)){
      mn.p<-min(row.names(data)[data$yr.1==cy[1] & data$da.1==doy[1]])
      mx.p<-max(row.names(data)[data$yr.1==cy[1] & data$da.1==doy[2]])
      year1 <- data[mn.p:mx.p,]
    }
  if(length(doy)==1&length(cy==1)){
      year1<-data  %>%
        filter(yr.1==cy[1], da.1 == doy[1])
    }
  if(length(cy)==2&length(doy)==1){
    mn.p<-min(row.names(data)[data$yr.1==cy[1] & data$da.1==doy[1]])
    mx.p<-max(row.names(data)[data$yr.1==cy[2] & data$da.1==doy[1]])
    year1 <- data[mn.p:mx.p,]
  }
  if(length(cy)==2 & length(doy)==2){
      mn.p<-min(row.names(data)[data$yr.1==cy[1] & data$da.1==doy[1]])
      mx.p<-max(row.names(data)[data$yr.1==cy[2] & data$da.1==doy[2]])
      year1 <- data[mn.p:mx.p,]
    }
  year1 <- year1%>%
    mutate(yr.1 = NULL,
           da.1 = NULL)#[,c('yr.1','da.1')] <- NULL
  return(year1)
}
