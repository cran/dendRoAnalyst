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
#' \code{DOY}\tab\tab    The day of year.\cr
#' \code{min}\tab\tab    The minimum value record for the corresponding day.\cr
#' \code{Time_min}\tab\tab    The time when minimum value recorded for the corresponding day.\cr
#' \code{max}\tab\tab    The maximum value record for the corresponding day.\cr
#' \code{Time_max}\tab\tab    The time when maximum value recorded for the corresponding day.\cr
#' \code{mean}\tab\tab    The daily average value of the dendrometer reading.\cr
#' \code{median}\tab\tab    The daily median value of the dendrometer reading.\cr
#' \code{amplitude}\tab\tab   The difference between daily maximum and daily minimum.\cr
#' }
#' @examples library(dendRoAnalyst)
#' data(nepa17)
#' daily_stats<-daily.data(df=nepa17, TreeNum=1)
#' head(daily_stats,10)
#'
#' @importFrom stats approx median na.exclude na.omit sd
#'
#' @export
daily.data<-function(df, TreeNum){
  data<-df
  Year<-NULL
  days<-NULL
  temp1<-data.frame(timestamp=as.POSIXct(strptime(data[,1], format = '%Y-%m-%d %H:%M:%S'), tz='UTC'))
  if(is.na(as.POSIXct(temp1$timestamp[1], format = '%Y-%m-%d %H:%M:%S'))){
    stop('Date not in the right format')
  }
  temp1[,2]<-data[,(TreeNum+1)]
  temp1$Year<-as.numeric(format(temp1$timestamp, '%Y'))
  temp1$Months<-as.numeric(format(temp1$timestamp, '%m'))
  temp1$days<-as.numeric(format(temp1$timestamp, '%j'))
  yr<-as.integer(unique(temp1$Year))
  #if (CalYear%in%yr==FALSE) {
   # stop(print("The provided year is not valid for the dataset."))
  #}
  if (TreeNum>ncol(data)-1) {
    stop("The tree number is not valid for provided dataset.")
  }
  #mnth<-sort(as.integer(unique(temp1$Months)))
  #doy<-as.integer(unique(temp1$days))

  x<-2
  ####################################################
  daily1<-c()
  for(y in yr){
    year1<-subset(temp1, Year==y)
    max1<-c()
    min1<-c()
    med1<-c()
    mean1<-c()
    DOY<-c()
    min_time<-c()
    max_time<-c()
    for(i in min(year1$days):max(year1$days)){
      DOY<-c(DOY,i)
      day<-subset(year1, days==i)
      a<-max(day[,x], na.rm = T)
      b<-min(day[,x], na.rm = T)
      max1<-c(max1,max(day[,x], na.rm = T))
      min1<-c(min1,min(day[,x], na.rm = T))
      med1<-c(med1,median(day[,x], na.rm = T))
      mean1<-c(mean1,mean(day[,x], na.rm = T))
      c<-day$timestamp[which(day[,x]==b)][1]
      d<-day$timestamp[which(day[,x]==a)][1]
      min_time<-c(min_time,format(c, '%H:%M:%S'))
      max_time<-c(max_time,format(d, '%H:%M:%S'))
    }
    daily<-data.frame(DOY)
    daily$min<-min1
    daily$Time_Minimum<-min_time
    daily$max<-max1
    daily$Time_Maximum<-max_time
    daily$mean<-mean1
    daily$median<-med1
    daily$amplitude<-daily$max-daily$min
    daily1<-rbind.data.frame(daily1,daily)
  }
  ####################################################
  #print(daily)
  return(daily1)
}


