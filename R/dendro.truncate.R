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
#' @examples library(dendRoAnalyst)
#' data(nepa)
#' #Extracting data from doy 20 to 50 in 2017.
#' trunc1<-dendro.truncate(df=nepa, CalYear=2017, DOY=c(20,50))
#' head(trunc1,10)
#'
#' @importFrom stats approx median na.exclude na.omit sd
#'
#' @export
dendro.truncate<-function (df, CalYear,DOY)
{
  data <- df
  cy<-CalYear
  doy<-DOY
  yr <- NULL
  temp1 <- data.frame(timestamp = as.POSIXct(strptime(data[,1],format = "%Y-%m-%d %H:%M:%S"), tz = "UTC"))
  if (is.na(as.POSIXct(temp1$timestamp[1], format = "%Y-%m-%d %H:%M:%S"))) {
    stop("Date not in the right format")
  }
  temp1[, 2:ncol(data)] <- data[, 2:ncol(data)]
  temp1$yr <- as.numeric(format(temp1$timestamp, "%Y"))
  temp1$da <- as.numeric(format(temp1$timestamp, "%j"))
  yr1 <- unique(temp1$yr)
  if(is.null(doy)==F){
    if(length(doy)>2|length(cy)>2){
      stop("The length of input 'CalYear' or 'DOY' is > 3.")
    }
    if(length(doy)==2&length(cy==1)){
      if (cy %in% yr1 == FALSE) {
        stop("Provided year does not exist in dataset.")
      }
      year1<-subset(temp1, temp1$yr==cy&temp1$da>=doy[1]&temp1$da<=doy[2])
    }
    if(length(doy)==1&length(cy==1)){
      if (cy %in% yr1 == FALSE) {
        stop("Provided year does not exist in dataset.")
      }
      year1<-subset(temp1, temp1$yr==cy&temp1$da==doy)
    }
    if(length(cy)==2&length(doy)==1){
      if (cy[1] %in% yr1 == FALSE|cy[2] %in% yr1 == FALSE) {
        stop("Provided year does not exist in dataset.")
      }
      an1<-subset(temp1, temp1$yr==cy[1]&temp1$da>=doy)
      an2<-subset(temp1, temp1$yr==cy[2]&temp1$da<=doy)
      year1<-rbind(an1,an2)
    }
    if(length(cy)==2&length(doy)==2){
      cy1<-cy[1]
      cy2<-cy[2]
      if (cy1 %in% yr1 == FALSE) {
        stop("Provided year does not exist in dataset.")
      }
      if (cy2 %in% yr1 == FALSE) {
        stop("Provided year does not exist in dataset.")
      }
      an11<-subset(temp1, temp1$yr==cy1)
      an1<-subset(an11,an11$da>=doy[1])
      an21<-subset(temp1, temp1$yr==cy2)
      an2<-subset(an21, an21$da<=doy[2])
      year1<-rbind(an1,an2)
    }

  }else{
    if (cy %in% yr1 == FALSE) {
      stop("Provided year does not exist in dataset.")
    }
    year1 <- subset(temp1, temp1$yr == cy)
  }
  year2 <- year1[, 1:ncol(data)]
  colnames(year2) <- colnames(data)
  row.names(year2) <- 1:nrow(year2)
  return(year2)
}
