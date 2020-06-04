#' @title Detection and spline interpolation of missing values in dendrometer data.
#'
#' @description This function detects gap(s) in time series, inserts the missing rows based on the provided temporal resolution and assings \code{NA} values to the corresponding value. If required the \code{NA} values can be replaced by spline interpolation using \code{\link[zoo:na.approx]{na.spline}} of the package \pkg{zoo}.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and following columns with dendrometer data for the same temporal resolution and time period.
#'
#' @param resolution integer, indicating the resolution of dendrometer data in \strong{minutes}.
#'
#' @param fill logical, if \code{TRUE} it fills the \code{NA} values using spline interpolation. Default is \code{FALSE}.
#'
#' @return A dataframe containing the dendrometer data including gaps filled with either \code{NA} or interpolated values.
#'
#' @examples library(dendRoAnalyst)
#' data(nepa17)
#' gf_nepa17<-spline.interpolation(df=nepa17, resolution=60)
#' head(gf_nepa17,10)
#'
#' @importFrom stats approx median na.exclude na.omit sd
#'
#' @importFrom zoo na.spline
#'
#' @export
spline.interpolation<-function(df, resolution, fill=FALSE){
  dataset<-df
  if(ncol(df)<=2){
    dataset$x<-dataset[,2]
  }
  b<-resolution
  dataset1<-dataset
  names(dataset1)<-c('TIME',colnames(dataset)[2:ncol(dataset)])
  a1<-ncol(dataset)+1
  dataset2<-data.frame(dataset1, row.names = dataset1$TIME)
  dataset2$time<-as.POSIXct(strptime(dataset1$TIME, "%Y-%m-%d %H:%M:%S"), tz='UTC')
  if(is.na(as.POSIXct(dataset2$time[1], format = '%Y-%m-%d %H:%M:%S'))){
    stop('Date not in the right format')
  }
  #dataset2$time=dataset2[,1]
  reference<-dataset2$time[1]
  dataset2$time_min<-as.integer(difftime(dataset2$time,reference, units = "mins"))
  x<-c()
  y<-c()
  ms.tm<-c()
  for (i in 1:length(dataset2$time_min)){
    x[i]<- dataset2$time_min[i+1]- dataset2$time_min[i]
    if(isTRUE(x[i]>b)==TRUE){
      a<-x[i]/b
      y<-c(y,a-1)
      #cat('\n Missing time after', as.character(dataset2$time[i]))
      #cat('. Number of missing values =', a-1)
      ms.tm<-c(ms.tm,as.character(dataset2$time[i]))
    }
  }
  if(length(ms.tm)==0){
    na.pos<-c()
    for(i in 2:ncol(dataset)){
      na.pos<-c(na.pos, which(is.na(dataset[,i])==T))
    }
    if(length(na.pos)==0){
      message("There are no gap in recording time or any internal 'NA' value.\n")
      return(dataset)
    }else{
      message("There are no gap in recording time but there may be 'NA' values in reading.\n")
      if(fill==T){
        print("Filling the internal 'NA' values.\n")
        for(i in 2:ncol(dataset)){
          f.loc<-which(is.na(dataset[,i])==T)
          f.sp<-zoo::na.spline(dataset[,i])
          dataset[f.loc,i]<-f.sp[f.loc]
        }
        dataset[,1]<-as.POSIXct(strptime(dataset[,1], "%Y-%m-%d %H:%M:%S"), tz='UTC')
        message("Done!!!\n")
        return(dataset)
      }else{
        dataset[,1]<-as.POSIXct(strptime(dataset[,1], "%Y-%m-%d %H:%M:%S"), tz='UTC')
        message("Done!!!\n")
        return(dataset)
      }
    }
  }else{
    ms<-data.frame('Missing location-after'=ms.tm, 'Number of consecutive missings'=y)
    print(ms)
    #x1=as.POSIXct(strptime(dataset2$time[1], "%Y-%m-%d %H:%M:%S"), tz='UTC')
    x1<-dataset2$time[1]#############
    #x2<-as.POSIXct(strptime(dataset2$time[length(dataset2$time)], "%Y-%m-%d %H:%M:%S"), tz='UTC')
    x2<-dataset2$time[length(dataset2$time)]
    x3<-seq(from = as.POSIXct(x1, tz = 'UTC'),to = as.POSIXct(x2, tz = 'UTC'), by=paste(b, 'min', sep = ' '))
    dataset5<-data.frame(matrix(data = NA, nrow=length(x3), ncol=1), row.names = x3)
    names(dataset5)<-colnames(dataset2)[1]
    dataset5[,1]<-as.POSIXct(strptime(x3, "%Y-%m-%d %H:%M:%S"), tz='UTC')
    #rownames(dataset2)=dataset2$time
    #rownames(dataset5)=dataset5[,1]
    dataset6<-merge(dataset5, dataset2, by=0, all=T)
    dataset6[,2:3]<-NULL
    dataset6[,a1:ncol(dataset6)]<-NULL
    names(dataset6)<-colnames(df)
    if(fill==T){
      if(max(y, na.rm = T)>=(1440/b)){warning('WARNING: There is gap in dataset lasting for more than 24 hours. The filling may not be reliable.\n')}else{message('Filling the gap ..... ....\n')}
      for(i in 2:ncol(dataset6)){
        f.loc<-which(is.na(dataset6[,i])==T)
        f.sp<-zoo::na.spline(dataset6[,i])
        dataset6[f.loc,i]<-f.sp[f.loc]
      }
    }else{
      dataset6
    }
    dataset6[,1]<-as.character(as.POSIXct(strptime(x3, "%Y-%m-%d %H:%M:%S"), tz='UTC'))
    cat("Done!!!\n")
    if(ncol(df)<=2){
      dataset6$x<-NULL
    }
    return(dataset6)
  }

}
