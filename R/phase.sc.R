#' @title Application of the stem-cycle approach to calculate different phases, their duration and to plot them.
#'
#' @description This function analyses the dendrometer data using Stem-cycle approach (Downs et al. 1999; Deslauriers et al. 2011). A function that defines three phases: 1) Shrinkage, when the dendrometer reading is less than previous reading, 2) Expansion, when current reading is more than previous reading and 3) Increment, when current reading is higher than the previous maximum. Additionally, it calculates various statistics for each phase.
#'
#' @references Deslauriers A, Rossi S, Turcotte A, Morin H, Krause C (2011) A three-step procedure in SAS to analyze the time series from automatic dendrometers. Dendrochronologia 29:151–161. \doi{10.1016/j.dendro.2011.01.008}
#'
#' @references Downes G, Beadle C, Worledge D (1999) Daily stem growth patterns in irrigated Eucalyptus globulus and E. nitens in relation to climate. Trees 14:102–111. \doi{10.1007/PL00009752}
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS}. It should contain data with constant temporal resolution for best results.
#'
#' @param TreeNum numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.
#'
#' @param smoothing numerical value from 1 to 12 which indicates the length of the smoothing spline, i.e. 1 = 1 hour and 12 = 12 hours. Default is \code{NULL} for no smoothing.The function \code{\link[pspline:smooth.Pspline]{smooth.Pspline}} is used for smoothing the data.
#'
#' @param outputplot  logical, to \code{plot} the phase diagram.
#'
#' @param days array with initial and final day for plotting. E.g. \emph{c(a,b)}, where a = initial date and b = final date.
#'
#' @param cols array with three elements representing colors for shrinking, expanding and increasing phases respectively.
#'
#' @param phNames array with three elements for three different phases. Default is \strong{"Shrinkage", "Expansion" and "Increment"}.
#'
#' @param cex numeric, for the size of the points. Default is \code{NULL}.
#'
#' @param cex.axis numeric, for the size of the axis tick labels. Default is \code{NULL}.
#'
#' @param cex.legend numeric, for the size of the legend labels. Default is \code{NULL}.
#'
#' @param font.axis numeric, for the font type of the axis tick labels. Default is \code{NULL}.
#'
#' @param col.axis color names, for the color of the axis tick labels. Default is \code{NULL}.
#'
#' @param ... other graphical parameters.
#'
#' @return A list of two dataframes. The first dataframe \emph{SC_cycle} with cyclic phases along with various statistics and the second dataframe \emph{SC_phase} with assigned phases for each data point.The dataframe \emph{SC_cycle} contains the beginning, end, duration, magnitude and rate of each phase. The dataframe \emph{SC_phase} contains time and corresponding phases during that time.
#' The contents of \emph{SC_cycle} are:
#' \tabular{llll}{
#' \strong{Columns}\tab\tab   \strong{Description}\cr
#' \code{Phase}\tab\tab	Cyclic phases. 1, 2, and 3 for Shrinkage, Expansion, and Increment respectively.\cr
#' \code{start}\tab\tab	Time when the corresponding phase starts.\cr
#' \code{end}\tab\tab	Time when the corresponding phase ends.\cr
#' \code{Duration_h}\tab\tab	Duration of the corresponding phase in hours.\cr
#' \code{Duration_m}\tab\tab	Duration of the corresponding phase in minutes.\cr
#' \code{Magnitude}\tab\tab	Radial/circumferential change during the corresponding phase in millimeters.\cr
#' \code{rate}\tab\tab	Rate of Radial/circumferential change in micrometers per hour.\cr
#' \code{DOY}\tab\tab	Day of year for the corresponding phase.

#' }
#'
#' @examples library(dendRoAnalyst)
#' data(gf_nepa17)
#' sc.phase<-phase.sc(df=gf_nepa17, TreeNum=1, smoothing=12, outputplot=TRUE, days=c(150,160))
#' head(sc.phase[[1]],10)
#' head(sc.phase[[2]],10)
#'
#' @importFrom grDevices rgb
#'
#' @importFrom graphics abline axis axis.POSIXct box legend lines mtext par points polygon rect text plot
#'
#' @importFrom stats approx median na.exclude na.omit sd
#'
#' @importFrom pspline smooth.Pspline
#'
#' @export
phase.sc<-function(df, TreeNum, smoothing=NULL, outputplot=FALSE, days, cols=c('#fee8c8','#fdbb84','#e34a33'),phNames = c('Shrinkage','Expansion','Increment'), cex=NULL, cex.axis=NULL,cex.legend=NULL, font.axis=NULL, col.axis=NULL, ...){
  data<-df
  #a<-NULL
  #b<-NULL
  temp<-data.frame(timestamp=as.POSIXct(data[,1], format = '%Y-%m-%d %H:%M:%S', tz='UTC'))
  if(unique(is.na(as.POSIXct(temp$timestamp, format = '%Y-%m-%d %H:%M:%S')))){
    stop('Date not in the right format')
  }
  data$doy<-as.integer(format(as.POSIXct(temp[,1], format = '%Y-%m-%d %H:%M:%S'), '%j'))
  data$yr<-as.integer(format(as.POSIXct(temp[,1], format = '%Y-%m-%d %H:%M:%S'), '%y'))
  y1<-unique(data$yr)
  if(length(y1)>1){
    data$doy2<-data$doy
    d<-max(which(data$doy==ifelse(y1[1]%%4==0,366,365)))
    data$doy2[(d+1):nrow(data)]<-data$doy2[(d+1):nrow(data)]+data$doy2[d]
  }
  dm1<-TreeNum+1
  sf<-smoothing
  #if(sf>12|sf<1){
  #  stop('smoothing must be between 1 to 12.')
  #}
  ############################
  reso_den<-function(input_time){
    time1<-input_time
    reference<-time1[1]
    time_min<-as.integer(difftime(time1,reference, units = "mins"))
    diff_time<-diff(time_min)
    diff2_time<-unique(diff_time)
    reso<-mean(diff2_time)
    if(isTRUE(length(diff2_time>1))==T){
      print(diff2_time)
      warning('Warning: The temporal resolution of dendrometer data is not consistent, For better result, please use dendrometer data with consistent resolution. There may be NA values in the dataset.')
      #cat(paste('Mean temporal resolution is :', round(reso),' minutes.'))
    }else{
      reso<-mean(diff2_time)
      #cat(paste('Temporal resolution is :', round(reso),' minutes.'))
      return(round(reso))
    }
  }
  #####################################################
  #########################
  r.denro<-reso_den(temp[,1])
  if(is.null(sf)==T){
    dm<-dm1
  }else{
    if(sf>12|sf<1){
      stop('smoothing must be between 1 to 12.')
    }else{
      spar2<-(60/r.denro)*12
      spar3<-(spar2/12)*sf
      sm<-pspline::smooth.Pspline(1:nrow(data),data[,dm1],spar=spar3, w=spar3*0.1)$ysmth
      data$sm<-sm[,1]
      dm<- which(colnames(data)=='sm')
    }
  }

  y<-c()
  for(i in 1:nrow(data)){
    if(isTRUE(data[,dm][i+1]-data[,dm][i]>0)==TRUE & isTRUE(max(data[,dm][1:i], na.rm = T)<data[,dm][i+1])==TRUE){
      y[i+1]<-3
    }else{
      if(isTRUE(data[,dm][i+1]-data[,dm][i]>0)==TRUE){
        y[i+1]<-2
      }else{
        y[i+1]<-1
      }

    }
  }
  data$y<-y[1:length(y)-1]
  ##########################################################
  ##########################################################
  ########### to calculate different phases ################
  ##########################################################
  ph<-c()
  doy<-c()
  magn<-c()
  for(i in 1:nrow(data)){
    if(isTRUE(data$y[i]==data$y[i+1])==TRUE & isTRUE((i+1)-i==1)==TRUE){
      next
    }
    else{
      doy<-c(doy, i)
      ph<-c(ph,data$y[i])
      magn<-c(magn,data[,dm1][i])#####
    }
  }
  abc<-data.frame(doy)
  strt_time<-c()
  for (i in 1:nrow(abc)){
    strt_time[i]<-strftime(data[,1][abc$doy[i]], format = '%Y-%m-%d %H:%M:%S')
  }
  abc$doy<-NULL
  abc$Phases<-ph
  abc<-na.omit(abc)
  abc$start<-strt_time[1:length(strt_time)-1]
  abc$end<-strt_time[2:(length(strt_time))]
  abc$Duration_m<-as.integer(difftime(strptime(abc$end, format = '%Y-%m-%d %H:%M:%S'), strptime(abc$start, format = '%Y-%m-%d %H:%M:%S'), units = 'mins'))
  abc$Duration_h<-round(as.numeric(difftime(strptime(abc$end, format = '%Y-%m-%d %H:%M:%S'), strptime(abc$start, format = '%Y-%m-%d %H:%M:%S'), units = 'hours')),1)
  abc$magnitude<-as.numeric(round(diff(magn),8))
  abc$rate<-(abc$magnitude/abc$Duration_h)*1000
  abc$DOY<-as.integer(format(strptime(abc$start, format = '%Y-%m-%d %H:%M:%S'), '%j'))

  #####################################################################
  ##################### For Plotting Phase data #######################
  #####################################################################
  if(outputplot==TRUE){
    if(days[2]>days[1]){
      a1<-as.numeric(which(data$doy==days[1]&data$yr==y1[1]))
      b1<-as.numeric(which(data$doy==days[2]&data$yr==y1[1]))
    }else{
      if(length(y1)<=1|length(y1)>=3){
        warning('WARNING: days[1] > days[2] not valid in this case. The plot is not possible.')
      }else{
        a1<-as.numeric(which(data$doy==days[1]&data$yr==y1[1]))############
        b1<-as.numeric(which(data$doy2==(days[2]+data$doy[d])&data$yr==y1[2]))
      }
    }
    #a1<-as.numeric(which(data$doy==days[1]))
    #b1<-as.numeric(which(data$doy==days[2]))
    a1_mn<-min(a1)
    b1_mx<-max(b1)
    data2<-data[a1_mn:b1_mx,]
    c1<-days[2]-days[1]
    xloc<-seq(a1_mn,b1_mx,(1440/r.denro))
    xloc2<-c()
    xloc4<-c()
    for(i in 1:length(xloc)){
      xloc2<-c(xloc2,data$doy[xloc[i]])
      xloc4<-c(xloc4,data$yr[xloc[i]])
    }
    opar <- par(no.readonly =TRUE)
    on.exit(par(opar))
    par(mar=c(6, 4.1, 5, 4.1), xpd=TRUE)
    plot(row.names(data2), data2[,dm1], type='l', col='grey', xlab = 'DOY', ylab = 'Stem size variation [mm]', xaxt='none', cex.axis=ifelse(is.null(cex.axis),1,cex.axis), font.axis=ifelse(is.null(font.axis),1,font.axis), col.axis=ifelse(is.null(col.axis),'black',col.axis), ...)###
    for(i in a1_mn:b1_mx){
      if(isTRUE(data$y[i]==1)==T){
        points(i, data[,dm1][i], col=cols[1], pch=16, cex = ifelse(is.null(cex),1,cex))###
      }else{
        if(isTRUE(data$y[i]==2)==T){
          points(i, data[,dm1][i], col=cols[2], pch=16, cex = ifelse(is.null(cex),1,cex))###
        }else{
          points(i, data[,dm1][i], col=cols[3], pch=16, cex = ifelse(is.null(cex),1,cex))###
        }
      }
    }
    legend('top',inset=c(0,-0.15*ifelse(is.null(cex.legend),1,cex.legend)), legend = phNames, col = cols, pch = 16, ncol = 3, box.lty = 0, text.font = 2,pt.cex = ifelse(is.null(cex),1,cex), cex=ifelse(is.null(cex.legend),1,cex.legend), bg = F)
    axis(1, at = xloc, xloc2, las=3, cex.axis=ifelse(is.null(cex.axis),1,cex.axis),font.axis=ifelse(is.null(font.axis),1,font.axis), col.axis=ifelse(is.null(col.axis),'black',col.axis))
    axis(3, at = xloc, xloc4, las=1, cex.axis=ifelse(is.null(cex.axis),1,cex.axis),font.axis=ifelse(is.null(font.axis),1,font.axis), col.axis=ifelse(is.null(col.axis),'black',col.axis), line = -1, tick = F)
  }
  data3<-data.frame('Time'=data[,1],'Phases'=data$y)
  return(list(SC_cycle=abc, SC_phase=data3))
}
