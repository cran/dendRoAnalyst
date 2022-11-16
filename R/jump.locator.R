#' @title Removing artefacts due to manual adjustments of dendrometers automatically for more than one dendrometers
#'
#' @description Dendrometers generally have limited memory capacity beyond which it stops recording. To keep the measurement ongoing, they should be adjusted periodically, which can cause positive or negative jumps in the data. This function locates these artefacts and adjusts them.Unlike in \code{\link[dendRoAnalyst:i.jump.locator]{i.jump.locator}}, it can handle dataset with more than one dendrometers.
#'
#' @return A dataframe containing jump-free dendrometer data.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.
#'
#' @param v numerical value which is considered as artefact. E.g. \code{v}=1 implies that if the difference to the consecutive data point is more than 1 or less than -1, it will be considered as an artefact.
#'
#' @importFrom grDevices rgb
#'
#' @importFrom graphics abline axis axis.POSIXct box legend lines mtext par points polygon rect text plot
#'
#' @importFrom stats approx median na.exclude na.omit sd
#'
#' @examples library(dendRoAnalyst)
#' data(nepa)
#' jump_free_nepa<-jump.locator(df=nepa ,v=1)
#' head(jump_free_nepa,10)
#'
#' @export
jump.locator<-function(df, v){
  temp<-df
  ju<-v
  if(unique(is.na(as.POSIXct(temp[,1], format = '%Y-%m-%d %H:%M:%S', tz='UTC')))){
    stop('Date not in the right format')
  }
  x<-as.POSIXct(temp[,1], "%Y-%m-%d %H:%M:%S", tz='UTC')
  x2<-paste(x)
  temp2<-data.frame('Time'=format(x))
  d.names<-colnames(temp)
  for(cols in 2:ncol(temp)){
    y<-temp[,cols]
    y_d<-diff(y)
    y_d2<-c(0,y_d)
    pos.ju<-y_d2[y_d2>= ju]
    neg.ju<-y_d2[y_d2<= -ju]
    all.ju<-y_d2[y_d2>=ju|y_d2<= -ju]
    j.loc<-which(y_d2>=ju|y_d2<= -ju)
    if(length(j.loc)==0){
      print(paste('There is no jump in dendrometer:',d.names[cols], sep = ' '))
      temp2[,cols]=y
    }else{
      #par(xpd=F)
      #plot(x,y, type='l',main = 'Data with all jump(s)', xlab = '',ylab = 'Increment (mm)',xaxt = 'n')
      #abline(v=x[j.loc], lwd=3, col=rgb(255,0,0, maxColorValue=255, alpha = 127))
      #axis.POSIXct(1, at=seq(as.POSIXct(x[1]), as.POSIXct(x[length(x)]), by="month"), format= "%b-%y", las= 2,cex.axis = 1)#############
      #for(i in j.loc){
      #  text(x[i],mean(y),labels = round(y_d2[i],2), srt=90, cex =1)
      #}
      jump_data<-matrix(1,nrow = length(j.loc), ncol = 3)
      jump_data[,1]<-as.numeric(j.loc)
      jump_data[,2]<-noquote(x2[j.loc])
      jump_data[,3]<-as.numeric(round(y_d2[j.loc],3))
      row.names(jump_data)<-1:length(j.loc)
      colnames(jump_data)<-c('Location','Time','Difference')
      print(paste('The jump(s) in dendrometer ', d.names[cols],':', sep = ''))
      print(jump_data)
      #invisible(readline(prompt="Press [enter] to continue"))
      for(i in j.loc){
        #plot(x[(i-10):(i+10)],y[(i-10):(i+10)], type='l', xlab = 'Time', ylab = 'Increment', main = paste('Jump at',x[i]), yaxt='n', ylim = c(min(y[(i-10):(i+10)]),max(y[(i-10):(i+10)])))
        #rect(x[(i-1)],min(y[(i-10):(i+10)]),x[(i+1)],max(y[(i-10):(i+10)]), col=rgb(255,0,0, maxColorValue = 255, alpha = 127), border = NA)
        #axis(2,seq(min(y[(i-10):(i+10)]),max(y[(i-10):(i+10)]),(max(y[(i-10):(i+10)])-min(y[(i-10):(i+10)]))/4))
        #invisible(readline(prompt="Press [enter] to adjust this jump"))
        y[i:length(y)]<-y[i:length(y)]-(y_d2[i])
        #plot(x[(i-10):(i+10)],y[(i-10):(i+10)], type='l', xlab = 'Time', ylab = 'Increment', main = paste('Removal of jump at',x[i]), yaxt='n', ylim = c(min(y[(i-10):(i+10)]),max(y[(i-10):(i+10)])))
        #rect(x[(i-1)],min(y[(i-10):(i+10)]),x[(i+1)],max(y[(i-10):(i+10)]), col=rgb(0,255,0, maxColorValue=255, alpha = 127), border = NA)
        #axis(2,seq(min(y[(i-10):(i+10)]),max(y[(i-10):(i+10)]),(max(y[(i-10):(i+10)])-min(y[(i-10):(i+10)]))/4))
        #invisible(readline(prompt="Press [enter] to continue"))
      }
      #plot(x,y,type = 'l', main = 'Data after removal of all jump(s)', xlab = '',ylab = 'Increment (mm)',xaxt = 'n')
      #abline(v=x[j.loc], lwd=3, col=rgb(0,255,0, maxColorValue=255, alpha = 127))
      #axis.POSIXct(1, at=seq(as.POSIXct(x[1]), as.POSIXct(x[length(x)]), by="month"), format= "%b-%y", las= 2,cex.axis = 1)#####
      #temp2<-data.frame('Time'=format(x),'corrected.data'=y)
      temp2[,cols]<-y

    }

  }
  colnames(temp2)<-colnames(df)
  cat('Done!!!')
  return(temp2)
}
