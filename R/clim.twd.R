#' @title Calculating relative growth change during no-rain periods.
#'
#' @description This function calculates the number and the location of climatically adverse periods within a climate time series. The user can define a duration and threshold of these conditions. The function also provides the relative radial/circumferencial change during each adverse period for the original or normalized data. See \href{https://doi.org/10.3389/fpls.2019.00342}{Raffelsbauer et al., (2019)} for more details.
#'
#' @references Raffelsbauer V, Spannl S, Peña K, Pucha-Cofrep D, Steppe K, Bräuning A (2019) Tree Circumference Changes and Species-Specific Growth Recovery After Extreme Dry Events in a Montane Rainforest in Southern Ecuador. Front Plant Sci 10:342. https://doi.org/10.3389/fpls.2019.00342
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.
#'
#' @param Clim dataframe with the first column containing \code{doy} and second column containing corresponding climate data.
#'
#' @param dailyValue either \emph{'max', 'min'}, or \emph{'mean'} for selecting the daily resampled value. Default is \emph{'max'}. See \code{\link[dendRoAnalyst:dendro.resample]{dendro.resample}} for details.
#'
#' @param thresholdClim numeric, the theshold for the respective climatic parameter. E.g. if climatic data is precipitation then days, where precipitation is below or equal to this value, are considered as adverse climate. Dafault is 0.
#'
#' @param thresholdDays numeric, the minimum number of consecutive adverse days to be considered for analysis. For example, \code{thresholdDays}=2 means the relative radial/circumferential change is calculated for adverse periods lasting for more than 2 days. Default is 2.
#'
#' @param norm logical, if \code{TRUE} the function uses normalized data instead of original dataset. Default is \code{FALSE}.
#'
#' @param showPlot logical, if \code{TRUE}, generates plots.
#'
#' @return A dataframe containing the respective periods, relative radial/circumference change for each tree, the ID for each period and their beginning and end.
#'
#' @examples library(dendRoAnalyst)
#' data(gf_nepa17)
#' data(ktm_rain17)
#' relative_dry_growth<-clim.twd(df=gf_nepa17, Clim=ktm_rain17, dailyValue='max', showPlot=TRUE)
#' 1
#'
#' head(relative_dry_growth,10)
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
clim.twd <- function(df,Clim,dailyValue='max',thresholdClim=0,thresholdDays=2,norm=F,showPlot){
  temp <- df
  if(ncol(df)<=2){
    temp[,3]<-temp[,2]
    colnames(temp)=c(colnames(df),'x')
  }else{
    temp<-temp}
  #n <- treeNum+1
  rn <- Clim
  da <- dendRoAnalyst::dendro.resample(temp,'D',dailyValue)
  nm=colnames(da)
  if(isTRUE(norm)){
    for(col in 2:ncol(da)){
      da[,col]=(da[,col]-mean(da[,col],na.rm = T))/sd(da[,col],na.rm = T)
    }
    colnames(da)=nm
  }
  x <- c(ifelse(rn[,2]<=thresholdClim,'y','n'))
  doy <- rn[,1]
  doy2 <- as.numeric(format(as.POSIXct(strptime(da[,1], format = '%Y-%m-%d')), '%j'))
  sl <- which(x=='y')
  el<-which(x=='n')
  if(isTRUE(sl[1]>el[1])){
    el<-c(el[sl[1]-1],el[el>=sl[1]])
  }else{
    el<-c(sl[1],el)
    }
  if(isTRUE(sl[length(sl)]==length(doy))){
    el<-c(el,sl[length(sl)])
  }
  ##############################################
  srt<-c()
  en<-c()
  d2<-c()
  for(i in 2:length(el)){
    x1<-sl[(el[i-1])<sl& sl<el[i]]
    y<-x1-el[i-1]
    z1<-matrix(nrow=length(x1),ncol = ncol(da)-1)
    for(j in 2:ncol(da)){
      a<-c()
      for(k in 1:length(x1)){
        a<-c(a,which(doy2==doy[x1[k]]))
      }
      b<-which(doy2==doy[el[i-1]])
      z<-da[a,j]-da[b,j]
      z1[,j-1]<-z
    }
    colnames(z1)<-paste('rel_change_',colnames(da)[2:ncol(da)], sep = '')
    #srt<-c(srt,doy[x1[1]])
    #en<-c(en,doy[x1[length(x1)]])
    d2<-rbind.data.frame(d2,data.frame('Time_lag'=y, z1,
                                      'period_id'=rep(i-1,length(x1)),
                                      'strt'=rep(doy[x1[1]],length(x1)),
                                      'end'=rep(doy[x1[length(x1)]],length(x1))))
  }

  #################################################
  if(isTRUE(showPlot)){
    l<-c()
    ue<-c()
    id<-c()
    un<-unique(d2$period_id)
    for (i in 1:length(un)){
      df1<-subset(d2,d2$period_id==un[i])
      if(nrow(df1)>thresholdDays){
        l<-c(l,unique(df1$strt))
        ue<-c(ue,unique(df1$end))
        id<-c(id,un[i])
      }
    }
    ###################################################
    x_lab<-c(1,32,60,91,121,152,182,213,244,274,305,335,365)
    opar <- par(no.readonly =TRUE)
    on.exit(par(opar))
    par(mar=c(5,1,10,1), xpd=F)
    plot(rn[,1],rn[,2],cex.lab=1.5,
         cex.axis=1.25, xlab = names(rn)[1],
         ylab='',col='white', ylim = c(0,max(rn[,2], na.rm=T)),
         xaxt='none', yaxt='none')
    abline(v=(l+ue)/2)
    for(i in 1:nrow(rn)){
      if(x[i]=='y'){
        rect(xleft = doy[i]-0.5, xright = doy[i]+0.5,
             ybottom = -2, ytop = max(rn[,2], na.rm = T),
             col='gray',border = 'grey')
      }else{
        rect(xleft = doy[i]-0.5, xright = doy[i]+0.5,
             ybottom = -2, ytop = max(rn[,2], na.rm = T),
             col='steelblue',border = 'steelblue')
      }
    }
    axis(side=3,at=(l+ue)/2,labels = id, las=2, cex.axis = 0.6, font = 2)
    axis(side=1,at=x_lab,labels = x_lab, las=1, cex.axis = 1, font = 2)
    #########################################################

    output<-data.frame('start_doy'=l, 'end_doy'=ue, 'Id'=id)
    print('The adverse climatic periods are listed below:')
    print(output)
    print("Please choose periods' id from the list. Please press enter two times when you are done.")
    perd<-c()
    perd<-scan()
    if(length(perd)==0){
      perd<-id[2:4]
    }
    clrs<-c('red','blue','green','purple','goldenrod','maroon','black','mediumorchid1','red','blue','green','yellow','purple','goldenrod','maroon','black','mediumorchid1','red','blue','green','yellow','purple','goldenrod','maroon','black','mediumorchid1')
    mn2<-c()
    mx2<-c()
    mn3<-c()
    for(k in 1:length(perd)){
      df3<-subset(d2, d2$period_id==perd[k])
      mn1<-min(df3[,2:(ncol(df3)-3)])
      mx1<-max(df3[,2:(ncol(df3)-3)])
      mn2<-c(mn2,mn1)
      mx2<-c(mx2,mx1)
      mn3<-c(mn3,nrow(df3))
    }
    y_mx<-max(mx2)
    y_mn<-min(mn2)
    x_mx<-max(mn3)
    cols<-clrs[1:length(perd)]
    opar <- par(no.readonly =TRUE)
    on.exit(par(opar))
    par(mar=c(5,6,8,8), xpd=T)
    plot(1,1, xlim = c(1,x_mx),ylim = c(y_mn,y_mx), col='white', xlab = 'Consecutive days', ylab = ifelse(isTRUE(norm),'Normalized relative growth variation (mm)','Relative growth variation (mm)'), cex.axis=1.25, cex.lab=1.5, xaxt='n')
    for(k in 1:length(perd)){
      df3<-subset(d2, d2$period_id==perd[k])
      if(isTRUE(ncol(df)<=2)){
        lines(df3[,1],df3[,2], col=cols[k], type='b', lwd=2,pch=1,bg=cols[k])
        legend('topright', inset=c(-0.25,0),legend = colnames(df)[2], col = 'black', pch = 1,ncol = 1, pt.bg = 'black', bty = 'n', title = 'Trees')
        legend('topright', inset=c(-0.25,0.2),legend = paste('ID_',perd,sep = ''), col = cols,lty = 1,ncol = 1, lwd = 2, bty='n',title = 'Clim Ids')
        axis(1, at=seq(1,x_mx,1), labels = seq(1,x_mx,1), cex.axis=1.25, las=2)
        axis(3, at=seq(1,x_mx,1), labels = seq(1,x_mx,1)+(df3$strt[1]-1), cex.axis=1.25, las=2)
        mtext('DOY',3,3,cex = 1.25)
      }else{
        for(a in 1:ncol(df3[,2:(ncol(df3)-3)])){
          lines(df3[,1],df3[,(a+1)], col=cols[k], type='b', lwd=2,pch=a,bg=cols[k])
          #points(df3[,1],df3[,(a+1)], col=cols[k],pch=a, cex=1.25)
          legend('topright', inset=c(-0.25,0),legend = colnames(temp[,2:(ncol(temp))]), col = 'black', pch = 1:ncol(df3[,2:(ncol(df3)-3)]),ncol = 1, pt.bg = 'black', bty = 'n', title = 'Trees', pt.cex = 1, cex = 0.75)
          legend('topright', inset=c(-0.25,0.1*(ncol(df)-1)),legend = paste('ID_',perd,sep = ''), col = cols,lty = 1,ncol = 1, lwd = 2, bty='n',title = 'Clim Ids', pt.cex = 1, cex = 0.75, text.col = cols)
          axis(1, at=seq(1,x_mx,1), labels = seq(1,x_mx,1), cex.axis=1.25, las=2)
          if(length(perd)==1){
            axis(3, at=seq(1,x_mx,1), labels = seq(1,x_mx,1)+(df3$strt[1]-1), cex.axis=1.25, las=2)
            mtext('DOY',3,3,cex = 1.25)
          }
      }
      }
    }
  }
  if(ncol(df)<=2){
    d3<-d2
    d3[,3]<-NULL
    return(d3)
  }else{
    return(d2)
  }
}
