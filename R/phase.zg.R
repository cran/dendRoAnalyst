#' @title Application of the zero-growth approach to calculate different phases, their duration and to plot them.
#'
#' @description This function analyses data using the zero-growth approach. Initially, it divides the data in two categories: 1) Tree water deficiency (TWD), i.e. the reversible shrinkage and expansion of the tree stem when the current reading is below the previous maximum and, 2) Increment (GRO), the irreversible expansion of the stem when the current reading is above the previous maximum. Then it calculates the TWD for each data point as the difference between the modelled "growth line" and the observed measurement. See \href{https://doi.org/10.1111/nph.13995}{Zweifel et. al.,(2016) } for details.
#'
#' @references Zweifel R, Haeni M, Buchmann N, Eugster W (2016) Are trees able to grow in periods of stem shrinkage? New Phytol 211:839–849. https://doi.org/10.1111/nph.13995
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS}. It should contain data with constant temporal resolution for best results.
#'
#' @param TreeNum numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.
#'
#' @param outputplot  logical, to \code{plot} the phase diagram.
#'
#' @param days array with initial and final day for plotting. E.g. \emph{c(a,b)}, where a = initial date and b = final date.
#'
#' @param linearCol color for the modelled curve.
#'
#' @param twdCol color for the TWD curve.
#'
#' @param twdFill filling method for the area under the TWD curve. Equivalent to \code{density} argument of the \code{\link[graphics:polygon]{polygon}} function in the \pkg{graphics} package of R. Default value is \code{NULL}.
#'
#' @param twdFillCol color to fill the area under the TWD curve.
#'
#' @param xlab string, x label of the \code{plot}.
#'
#' @param ylab1 string, y label of the upper \code{plot}.
#'
#' @param ylab2 string, y label of the lower \code{plot}.
#'
#' @param twdYlim numeric, to define the limit of the y-axis of the lower plot. Default is \code{NULL}, which automatically adjusts the y-axis limit.
#'
#' @param cex.lab numeric, for the size of the axis labels. Default is \code{NULL}.
#'
#' @param cex.axis numeric, for the size of the axis tick labels. Default is \code{NULL}.
#'
#' @param font.lab numeric, for the font type of the axis labels. Default is \code{NULL}.
#'
#' @param font.axis numeric, for the font type of the axis tick labels. Default is \code{NULL}.
#'
#' @param col.axis color names, for the color of the axis tick labels. Default is \code{NULL}.
#'
#' @param col.lab color names, for the color of the axis labels. Default is \code{NULL}.
#'
#'
#' @return A list of two dataframes. The first dataframe \emph{ZG_cycle} contains the cyclic phases along with various statistics and the second dataframe \emph{ZG_phase} with assigned phases for each data point.The contents of \emph{ZG_cycle} are:
#' \tabular{llll}{
#' \strong{Columns}\tab\tab   \strong{Description}\cr
#' \code{DOY}\tab\tab	Day of year for the corresponding phase.\cr
#' \code{Phase}\tab\tab	TWD for tree water deficit and GRO for irreversible expansion.\cr
#' \code{start}\tab\tab	The time when the corresponding phase starts.\cr
#' \code{end}\tab\tab	The time when the corresponding phase ends.\cr
#' \code{Duration_h}\tab\tab	Duration of the corresponding phase in hours.\cr
#' \code{Magnitude}\tab\tab	The radial/circumferential change during the corresponding ‘GRO’ phase in millimeters.\cr
#' \code{rate}\tab\tab	The rate of radial/circumferential change during the corresponding ‘GRO’ phase measured in micrometers per hour.\cr
#' \code{Max.twd}\tab\tab	The maximum TWD recorded for the corresponding TWD phase.\cr
#' \code{Max.twd.time}\tab\tab	The time of occurrence of maximum TWD value for each corresponding TWD phase.\cr
#' \code{Avg.twd}\tab\tab	Average of TWD values for each TWD phase.\cr
#' \code{STD.twd}\tab\tab	The standard deviation of TWD values for each TWD phase.
#' }
#'
#' @examples library(dendRoAnalyst)
#' data(gf_nepa17)
#' zg.phase<-phase.zg(df=gf_nepa17, TreeNum=1, outputplot=TRUE, days=c(150,160))
#' head(zg.phase[[1]],10)
#' head(zg.phase[[2]],10)
#'
#' @importFrom grDevices rgb
#'
#' @importFrom graphics abline axis axis.POSIXct box legend lines mtext par points polygon rect text plot
#'
#' @importFrom stats approx median na.exclude na.omit sd
#'
#' @export
phase.zg<-function(df, TreeNum, outputplot, days, linearCol='#2c7fb8',twdCol='#636363',twdFill=NULL,twdFillCol='#f03b20',xlab='DOY',ylab1='Stem size variation [mm]',ylab2='TWD [mm]',twdYlim=NULL, cex.axis=NULL, cex.lab=NULL, font.lab=NULL, col.lab=NULL, font.axis=NULL, col.axis=NULL){
  temp13<-df
  dm<-TreeNum+1
  data<-temp13[,c(1,dm)]
  dm<-2
  a<-NULL
  b<-NULL
  temp<-as.POSIXct(strptime(data[,1], "%Y-%m-%d %H:%M:%S"), tz='UTC')
  if(is.na(as.POSIXct(temp[1], format = '%Y-%m-%d %H:%M:%S'))){
    stop('Date not in the right format')
  }
  data$doy<-as.integer(format(strptime(temp, format = '%Y-%m-%d %H:%M:%S'), '%j'))
  data$yr<-as.integer(format(strptime(temp, format = '%Y-%m-%d %H:%M:%S'), '%y'))
  y1<-unique(data$yr)
  if(length(y1)>1){
    data$doy2<-data$doy
    d<-max(which(data$doy==ifelse(y1[1]%%4==0,366,365)))
    data$doy2[(d+1):nrow(data)]<-data$doy2[(d+1):nrow(data)]+data$doy2[d]
  }

  #require(pspline)
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
  r.denro<-reso_den(temp)
  ############################
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
  data$phs<-data$y
  zg_ph<-data$y
  zg_ph[zg_ph==2|zg_ph==1]<-'TWD'
  zg_ph[zg_ph==3]<-'GRO'
  data$y<-zg_ph
  ##########################################
  x<-which(data$y=='GRO')
  dp.3.time<-data[,dm][x]
  y.3<-na.exclude(data[,dm][x])

  ap.all<-approx(c(1,x[1]),c(y.3[1],y.3[1]),xout =1:(x[1]-1))$y
  for(i in 2:length(x)){
    ap<-approx(c(x[(i-1)],x[i]),c(y.3[(i-1)],y.3[i]),xout =x[(i-1)]:(x[i]-1))
    #lines(x[i]:(x[i+1]-1), ap$y)
    ap.all<-c(ap.all,ap$y[1:length(ap$y)])
  }
  if(x[length(x)]!=nrow(data)){
    ap.all2<-approx(c(x[length(x)],nrow(data)),c(y.3[length(y.3)],y.3[length(y.3)]),xout =x[length(x)]:nrow(data))$y
  }else{
    #ap.all2=approx(c(x[length(x)-1],nrow(data)),c(y.3[length(y.3)-1],y.3[length(y.3)]),xout =x[length(x)-1]:nrow(data))$y
    n<-which(zg_ph=='TWD')
    n1<-n[length(n)]
    ap.all2<-data[,dm][(n1+1):nrow(data)]
  }

  ap.all<-c(ap.all, ap.all2)
  data$twd<-data[,dm]-ap.all
  data$strght.line<-ap.all
  #Calculating different phases in ZG method
  gr.ph<-c()
  doy<-c()
  magn<-c()
  for(i in 1:nrow(data)){
    if(isTRUE(data$y[i]==data$y[i+1])==TRUE & isTRUE((i+1)-i==1)==TRUE){
      next
    }
    else{
      doy<-c(doy, i)
      gr.ph<-c(gr.ph,data$y[i])
      magn<-c(magn,data[,dm][i])
    }
  }
  mx.twd1<-c()
  avg.twd1<-c()
  s.twd1<-c()
  md.twd1<-c()
  mn.twd1<-c()
  d.min1<-c()
  d.max1<-c()

  for(i in 2:length(x)){
    df<-data[x[i-1]:x[(i)],]
    mx.twd1<-c(mx.twd1, max(df$twd,na.rm = T))
    mn.twd1<-c(mn.twd1, min(df$twd,na.rm = T))
    avg.twd1<-c(avg.twd1, mean(df$twd, na.rm = T))
    s.twd1<-c(s.twd1, sd(df$twd, na.rm = T))
    md.twd1<-c(md.twd1, median(df$twd, na.rm = T))
    t.mn<-subset(df,df$twd==min(df$twd, na.rm = T))
    d.min1<-c(d.min1, strftime(t.mn[1,1], format = '%Y-%m-%d %H:%M:%S'))
    t.mx<-subset(df,df$twd==max(df$twd, na.rm = T))
    d.max1<-c(d.max1, strftime(t.mx[1,1], format = '%Y-%m-%d %H:%M:%S'))
  }
  df2<-data[(x[length(x)]+1):nrow(data),]
  mx.twd<-c(mx.twd1, max(df2$twd,na.rm = T))
  mn.twd<-c(mn.twd1, min(df2$twd,na.rm = T))
  avg.twd<-c(avg.twd1, mean(df2$twd, na.rm = T))
  s.twd<-c(s.twd1, sd(df2$twd, na.rm = T))
  md.twd<-c(md.twd1, median(df2$twd, na.rm = T))
  t.mn<-subset(df2,df2$twd==min(df2$twd, na.rm = T))
  d.min<-c(d.min1, strftime(t.mn[1,1], format = '%Y-%m-%d %H:%M:%S'))
  t.mx<-subset(df2,df2$twd==max(df2$twd, na.rm = T))
  d.max<-c(d.max1, strftime(t.mx[1,1], format = '%Y-%m-%d %H:%M:%S'))

  xyz<-data.frame(d.max,mx.twd, d.min, mn.twd, avg.twd, s.twd, md.twd)
  s<-subset(xyz, xyz$mn.twd!=0)
  abc<-data.frame(doy)
  strt_time<-c()
  for (i in 1:nrow(abc)){
    strt_time[i]<-strftime(data[,1][abc$doy[i]], format = '%Y-%m-%d %H:%M:%S')
  }
  abc$doy<-NULL
  abc$DOY<-as.integer(format(strptime(strt_time, format = '%Y-%m-%d %H:%M:%S'), '%j'))
  abc$Phases<-gr.ph
  abc<-na.omit(abc)
  abc$start<-strt_time[1:length(strt_time)-1]
  abc$end<-strt_time[2:(length(strt_time))]
  abc$Duration_h<-round(as.numeric(difftime(strptime(abc$end, format = '%Y-%m-%d %H:%M:%S'), strptime(abc$start, format = '%Y-%m-%d %H:%M:%S'), units = 'hours')),1)
  abc$magnitude<-as.numeric(round(diff(magn),8))
  abc$rate<-(abc$magnitude/abc$Duration_h)*1000
  ########################################################
  data$tm=data[,1]
  twd12=data$twd
  tp<-abc$start[abc$Phases=='TWD']
  ep<-abc$end[abc$Phases=='TWD']
  twd.loc=which(abc$Phases=='TWD')
  max.t=c()
  max.tm=c()
  mn.t=c()
  sd.t=c()
  for(q in 1:length(tp)){
    r= as.numeric(which(data$tm==tp[q]))
    t= as.numeric(which(data$tm==ep[q]))
    f=data[r:t,]
    max.t=c(max.t, min(f$twd))
    d=which(f$twd==min(f$twd))
    max.tm=c(max.tm,strftime(f[,1][d[length(d)]], format = '%Y-%m-%d %H:%M:%S') )
    mn.t=c(mn.t,mean(f$twd, na.rm=T))
    sd.t=c(sd.t,sd(f$twd, na.rm=T))
  }
  #print(f)
  abc$Max.twd<-NA
  abc$Max.twd[twd.loc]<- -max.t
  abc$Max.twd.time<-NA
  abc$Max.twd.time[twd.loc]<-strftime(max.tm, format = '%Y-%m-%d %H:%M:%S')
  abc$Avg.twd<-NA
  abc$Avg.twd[twd.loc]<- -mn.t
  abc$STD.twd<-NA
  abc$STD.twd[twd.loc]<- sd.t

  #######################################################
  #abc2<-abc[order(abc$Phases, decreasing = T),]
  #abc2$Max.twd<-NA
  #abc2$Max.twd[1:length(s$mx.twd)]<- -s$mn.twd
  #abc2$Max.twd.time<-NA
  #abc2$Max.twd.time[1:length(s$mx.twd)]<-strftime(s$d.min, format = '%Y-%m-%d %H:%M:%S')
  #abc2$Avg.twd<-NA
  #abc2$Avg.twd[1:length(s$mx.twd)]<- -s$avg.twd
  #abc2$STD.twd<-NA
  #abc2$STD.twd[1:length(s$mx.twd)]<- s$s.twd
  #abc<-abc2[order(as.numeric(row.names(abc2)), decreasing = F),]
  abc$rate[abc$Phases=='TWD']<-NA
  abc$magnitude[abc$Phases=='TWD']<-NA
  ###################################################################
  ################Plotting#########################################
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
    #a1<-which(data$doy==days[1])
    #b1<-which(data$doy==days[2])
    a1_mn<-min(a1)
    b1_mx<-max(b1)
    c1<-days[2]-days[1]
    data2<-data[a1_mn:b1_mx,]
    xloc<-seq(a1_mn,b1_mx,(1440/r.denro))
    xloc2<-c()
    xloc4<-c()
    for(i in 1:length(xloc)){
      xloc2<-c(xloc2,data$doy[xloc[i]])
      xloc4<-c(xloc4,data$yr[xloc[i]])
    }
    tw.mn<-ifelse(is.null(twdYlim),min(data2$twd, na.rm = T),twdYlim)
    opar <- par(no.readonly =TRUE)
    on.exit(par(opar))
    par(mfrow=c(2,1))
    par(mar=c(0, 4.1, 5, 4.1), xpd=F)
    plot(x=row.names(data2), y=data2[,dm], type='l', col='grey25', xlab = '', ylab = ylab1, xaxt='none',cex.lab=ifelse(is.null(cex.lab),1,cex.lab), cex.axis=ifelse(is.null(cex.axis),1,cex.axis),font.axis=ifelse(is.null(font.axis),1,font.axis), col.axis=ifelse(is.null(col.axis),'black',col.axis), font.lab=ifelse(is.null(font.lab),1,font.lab), col.lab=ifelse(is.null(col.lab),'black',col.lab))
    #abline(v=xloc, col='lightgray',lty=2, lwd=0.5)
    lines(row.names(data2), data2$strght.line, col=linearCol, lwd=1.25)
    axis(3, at = xloc, xloc2, las=3, cex.axis=ifelse(is.null(cex.axis),1,cex.axis),font.axis=ifelse(is.null(font.axis),1,font.axis), col.axis=ifelse(is.null(col.axis),'black',col.axis))
    par(mar=c(5.1, 4.1, 0, 4.1))

    plot(x=row.names(data2), y=data2$twd, col='red', type='n', ylab = '', xlab = xlab,yaxt='n',xaxt='n', ylim = c(tw.mn,0),cex.lab=ifelse(is.null(cex.lab),1,cex.lab), cex.axis=ifelse(is.null(cex.axis),1,cex.axis),font.axis=ifelse(is.null(font.axis),1,font.axis), col.axis=ifelse(is.null(col.axis),'black',col.axis), font.lab=ifelse(is.null(font.lab),1,font.lab), col.lab=ifelse(is.null(col.lab),'black',col.lab))
    polygon(c(row.names(data2)[1], row.names(data2), row.names(data2)[length(row.names(data2))]), c(0, data2$twd, 0), col=twdFillCol, border = twdCol, density = twdFill)
    lines(row.names(data2), data2$twd)
    #lines()
    abline(h=0, col='blue', lwd=1)
    axis(4, at = seq(0,tw.mn,(tw.mn*0.25)), round(seq(0,-tw.mn,-tw.mn*0.25),2), las=3, cex.axis=ifelse(is.null(cex.axis),1,cex.axis),font.axis=ifelse(is.null(font.axis),1,font.axis), col.axis=ifelse(is.null(col.axis),'black',col.axis))
    axis(1, at = xloc, xloc2, las=3, cex.axis=ifelse(is.null(cex.axis),1,cex.axis),font.axis=ifelse(is.null(font.axis),1,font.axis), col.axis=ifelse(is.null(col.axis),'black',col.axis))
    mtext(ylab2, side = 4, line = 2.5,cex=ifelse(is.null(cex.lab),1,cex.lab),font=ifelse(is.null(font.lab),1,font.lab), col=ifelse(is.null(col.lab),'black',col.lab))
    box()
    #legend('top',inset=c(0,-0.1), legend = c('Shrinkage','Expansion','Increment'), col = c('red','blue','green'), pch = 16, ncol = 3, box.lty = 0)

  }
  ################################################################
  data3<-data.frame('Time'=data[,1],'Phases'=data$y,'TWD'=-data$twd)
  return(list(ZG_cycle=abc, ZG_phase=data3))
}
