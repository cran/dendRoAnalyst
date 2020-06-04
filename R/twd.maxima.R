#' @title Locating the maxima of TWD periods
#'
#' @description This function detects the TWD phases, including their beginning (TWDb), using the phase.zg function. Then it calculates the number, time of occurance (Tm) and value of every local maximum within each TWD phase. In addition it calculates the time difference between 'TWDb' and each 'Tm' within each TWD phase.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS}. It should contain data with constant temporal resolution for best results.
#'
#' @param TreeNum numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.
#'
#' @param smoothing numerical value from 1 to 12 which indicates the length of the smoothing spline, i.e. 1 = 1 hour and 12 = 12 hours. Default is 5.
#'
#' @param showPlot logical, if \code{TRUE}, it generates a plot. Default is \code{TRUE}.
#'
#' @param days array with initial and final day for plotting. E.g. \emph{c(a,b)}, where a=initial date and b=final date. Default is \emph{c(150,160)}.
#'
#' @param ... additional graphical parameter incuded in \code{\link[dendRoAnalyst:phase.zg]{phase.zg}}.
#'
#' @return A dataframe with statistics of maxima in each TWD phase.
#'
#' @examples library(dendRoAnalyst)
#' data(gf_nepa17)
#' df1=gf_nepa17[2500:3500,]
#' twd_max<-twd.maxima(df=df1, TreeNum=2, showPlot=FALSE)
#' head(twd_max,10)
#'
#' @importFrom grDevices rgb
#'
#' @importFrom graphics abline axis axis.POSIXct box legend lines mtext par points polygon rect text plot
#'
#' @importFrom stats approx median na.exclude na.omit sd
#'
#' @importFrom zoo na.spline
#'
#' @export
twd.maxima<-function(df, TreeNum, smoothing=5, showPlot=T, days=c(150,160), ...){
  n<-TreeNum
  sc<-phase.sc(df=df, TreeNum=n, smoothing=smoothing, outputplot=F)
  zg<-phase.zg(df=df, TreeNum=n, outputplot=showPlot, days = days, ...)
  sc1<-sc[[1]]
  #sc2<-sc[[2]]
  zg1<-zg[[1]]
  zg2<-zg[[2]]
  sc1.1<-subset(sc1, sc1$Phases==1)
  #sc1.2<-subset(sc1, sc1$Phases==2)
  #sc1.3<-subset(sc1, sc1$Phases==3)
  zg1.1<-subset(zg1, zg1$Phases=='TWD')
  #zg1.2<-subset(zg1, zg1$Phases=='GRO')
  strt.sh<-sc1.1$start
  end.p<-c()
  for(i in sc1.1$start){
    st.loc<-which(sc1$start==i)
    en.loc<-st.loc+1
    end.p<-c(end.p,sc1$end[en.loc])
  }
  #######################################
  twd.mx<-c()
  loc.mx<-c()
  time.mx<-c()
  for(loc in 1:length(strt.sh)){
    strt.loc<-which(zg2$Time==strt.sh[loc])
    end.loc<-which(zg2$Time==end.p[loc])
    cr.twd<-zg2[strt.loc:end.loc,]
    mx<-max(cr.twd$TWD, na.rm = T)[1]
    loc.max<-which(zg2$TWD==mx)[1]
    t.mx<-as.character(zg2$Time[loc.max])
    time.mx<-c(time.mx,t.mx)
    twd.mx<-c(twd.mx,mx)
    loc.mx<-c(loc.mx,loc.max)
  }
  ################################################
  zg2$mtwd<-0
  zg2$mtwd[loc.mx]<-1
  if(isTRUE(showPlot)){
    lines(loc.mx,twd.mx, col='black',type = 'h', lwd=2.5)
  }
  mins3<-c()
  for(twd in 1:nrow(zg1.1)){
    twd.st<-which(zg2$Time==zg1.1$start[twd])
    twd.end<-which(zg2$Time==zg1.1$end[twd])
    temp1<-zg2[twd.st:twd.end,]
    temp2<-temp1[temp1$mtwd==1,]
    y<-as.integer(difftime(strptime(temp2$Time, format = '%Y-%m-%d %H:%M:%S'), strptime(zg1.1$start[twd], format = '%Y-%m-%d %H:%M:%S'), units = 'mins'))
    x1<-rep(zg2$Time[twd.st], nrow(temp2))
    x2<-rep(zg2$Time[twd.end], nrow(temp2))
    twd.n<-rep(twd, nrow(temp2))
    x<-data.frame('start.time'=x1,'end.time'=x2,temp2[,c(1,3)],'duration_from_start'=y, 'twd.number'=twd.n)
    mins3<-rbind.data.frame(mins3,x)
  }
  return(mins3)
}
