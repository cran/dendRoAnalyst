#' @title Resampling temporal resolution of dendrometer data
#'
#' @description This function is designed to change the temporal resolution of data. Depending on the objective, the user can define either maximum, minimum, or mean values to resample data in hourly, daily, weekly or monthly frequency.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS}.
#'
#' @param by either \emph{H, D, W} or \emph{M} to resample data into hourly, daily, weekly or monthly resolution.
#'
#' @param value either \emph{max, min} or \emph{mean} for the resampling value.
#'
#' @return Dataframe with resampled data.
#'
#' @examples library(dendRoAnalyst)
#' data(nepa17)
#' # To resample monthly with maximum value
#' resample_M<-dendro.resample(df=nepa17[,1:2], by='M', value='max')
#' head(resample_M,10)
#'
#' @importFrom stats approx median na.exclude na.omit sd
#'
#'
#' @export
dendro.resample<-function(df, by, value){
  data<-df
  if(ncol(data)<3){
    data[,3]=data[,2]
  }
  yr<-NULL
  da<-NULL
  wk<-NULL
  mn<-NULL
  temp1<-data.frame(timestamp=as.POSIXct(strptime(data[,1], format = '%Y-%m-%d %H:%M:%S'), tz='UTC'))
  if(is.na(as.POSIXct(temp1$timestamp[1], format = '%Y-%m-%d %H:%M:%S'))){
    stop('Date not in the right format')
  }
  temp1[,2:ncol(data)]<-data[,2:ncol(data)]
  temp1$yr<-as.numeric(format(temp1$timestamp, '%Y'))
  temp1$mn<-as.numeric(format(temp1$timestamp, '%m'))
  temp1$wk<-as.numeric(format(temp1$timestamp, '%W'))
  temp1$wk[temp1$wk==0]<-as.numeric(1)
  temp1$da<-as.numeric(format(temp1$timestamp, '%j'))
  temp1$hr<-as.numeric(format(temp1$timestamp, '%H'))
  temp1$ymd<-as.character(format(temp1$timestamp, '%Y-%m-%d'))
  yr1<-unique(temp1$yr)
  if(by%in%c('D','W','M', 'H')==FALSE){
    stop("You must provide arguement 'by' with either 'H' for Hourly, 'D' for Daily,'W' for weekly or 'M' for monthly")
  }
  if(value%in%c('max','min','mean')==FALSE){
    stop("You must provide arguement 'value' with either 'max' for maximum,'min' for minimum or 'mean' for mean")
  }

  DOY1<-c()
  time1<-c()
  resamp1<-c()
  for(year in yr1){
    year1<-subset(temp1, yr==year)
    DOY<-c()
    time<-c()
    a<-c()
    #By daily basis
    if(by=='D'){
      x<-matrix(ncol=ncol(data)-1)
      if(value=='max'){
        for(i in min(year1$da):max(year1$da)){
          day<-subset(year1, da==i)
          time<-c(time,format(day$timestamp[1], '%Y-%m-%d'))
          #DOY<-c(DOY,i)
          a1<-apply(day[,2:ncol(data)], 2, max, na.rm=T)
          a<-rbind.data.frame(a,a1)
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }
      if(value=='min'){
        for(i in min(year1$da):max(year1$da)){
          day<-subset(year1, da==i)
          time<-c(time,format(day$timestamp[1], '%Y-%m-%d'))
          #DOY<-c(DOY,i)
          a1<-apply(day[,2:ncol(data)], 2, min, na.rm=T)
          a<-rbind.data.frame(a,a1)
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }
      if(value=='mean'){
        for(i in min(year1$da):max(year1$da)){
          day<-subset(year1, da==i)
          time<-c(time,format(day$timestamp[1], '%Y-%m-%d'))
          #DOY<-c(DOY,i)
          a1<-apply(day[,2:ncol(data)], 2, mean, na.rm=T)
          a<-rbind.data.frame(a,a1)
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }

    }
    #############################################################
    ####################################################################
    ####################################################################


    # By weekly basis
    if(by=='W'){
      w<-c()
      x<-matrix(ncol=ncol(data)-1)
      if(value=='max'){
        for(i in min(year1$wk):max(year1$wk)){
          week<-subset(year1, wk==i)
          time<-c(time,format(week$timestamp[1], '%Y-%m-%d'))
          #w<-c(w,i)
          a1<-apply(week[,2:ncol(data)], 2, max, na.rm=T)
          a<-rbind.data.frame(a,a1)
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }
      if(value=='min'){
        for(i in min(year1$wk):max(year1$wk)){
          week<-subset(year1, wk==i)
          time<-c(time,format(week$timestamp[1], '%Y-%m-%d'))
          #w<-c(w,i)
          a1<-apply(week[,2:ncol(data)], 2, min, na.rm=T)
          a<-rbind.data.frame(a,a1)
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }
      if(value=='mean'){
        for(i in min(year1$wk):max(year1$wk)){
          week<-subset(year1, wk==i)
          time<-c(time,format(week$timestamp[1], '%Y-%m-%d'))
          #w<-c(w,i)
          a1<-apply(week[,2:ncol(data)], 2, mean, na.rm=T)
          a<-rbind.data.frame(a,a1)
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }

    }

    ############################################################################
    ###############################################################################
    ###############################################################################
    if(by=='M'){
      m<-c()
      x<-matrix(ncol=ncol(data)-1)
      if(value=='max'){
        for(i in min(year1$mn):max(year1$mn)){
          month<-subset(year1, mn==i)
          time<-c(time,format(month$timestamp[1], '%Y-%m-%d'))
          #m<-c(m,i)
          a1<-apply(month[,2:ncol(data)], 2, max, na.rm=T)
          a<-rbind.data.frame(a,a1)
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }
      if(value=='min'){
        for(i in min(year1$mn):max(year1$mn)){
          month<-subset(year1, mn==i)
          time<-c(time,format(month$timestamp[1], '%Y-%m-%d'))
          #m<-c(m,i)
          a1<-apply(month[,2:ncol(data)], 2, min, na.rm=T)
          a<-rbind.data.frame(a,a1)
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }
      if(value=='mean'){
        for(i in min(year1$mn):max(year1$mn)){
          month<-subset(year1, mn==i)
          time<-c(time,format(month$timestamp[1], '%Y-%m-%d'))
          #m<-c(m,i)
          a1<-apply(month[,2:ncol(data)], 2, mean, na.rm=T)
          a<-rbind.data.frame(a,a1)
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }

    }
    #########################################################
    ############# Hourly resample ###########################
    if(by=='H'){
      if(value=='max'){
        for(i in min(year1$da):max(year1$da)){
          day<-subset(year1, da==i)
          for(h in unique(day$hr)){
            hours<-subset(day, day$hr==h)
            hms<-paste( sprintf("%02d", as.numeric(h)),':00:00', sep = '')
            ymdhms<-paste(as.character(hours$ymd[1]),hms, sep =' ')
            time<-c(time,ymdhms)
            a1<-apply(hours[,2:ncol(data)], 2, max, na.rm=T)
            a<-rbind.data.frame(a,a1)
          }
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }
      if(value=='min'){
        for(i in min(year1$da):max(year1$da)){
          day<-subset(year1, da==i)
          for(h in unique(day$hr)){
            hours<-subset(day, day$hr==h)
            hms<-paste(h,':00:00', sep = '')
            ymdhms<-paste(hours$ymd[1],hms, sep =' ')
            time<-c(time,ymdhms)
            a1<-apply(hours[,2:ncol(data)], 2, min, na.rm=T)
            a<-rbind.data.frame(a,a1)
          }
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }
      if(value=='mean'){
        for(i in min(year1$da):max(year1$da)){
          day<-subset(year1, da==i)
          for(h in unique(day$hr)){
            hours<-subset(day, day$hr==h)
            hms<-paste(h,':00:00', sep = '')
            ymdhms<-paste(hours$ymd[1],hms, sep =' ')
            time<-c(time,ymdhms)
            a1<-apply(hours[,2:ncol(data)], 2, mean, na.rm=T)
            a<-rbind.data.frame(a,a1)
          }
        }
        resamp<-data.frame(time,a)
        names(resamp)<-c('Time',colnames(data[2:ncol(data)]))
        #return(resamp)
      }

    }

    #########################################################
    resamp1<-rbind.data.frame(resamp1,resamp)
  }
  rownames(resamp1)<-1:nrow(resamp1)
  for(i in 2:ncol(resamp1)){
    loc.inf<-which(resamp1[,i]=='-Inf')
    resamp1[loc.inf,i]<-NA
  }
  if(ncol(df)<3){
    resamp1=resamp1[,1:2]
    names(resamp1)<-colnames(df)
  }
  resamp1[,1]=as.character(resamp1[,1])
  return(resamp1)
}
