#' @title Interpolation of NA values using the dendrometer network
#'
#' @description A function to interpolate the missing data of a dendrometer with the help of other dendrometers from the same site, provided they have the same measurement period and temporal resolution.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and dendrometer data in the second column and onward. The data gaps must be filled with \code{NA} using the gap.interpolation function.
#'
#' @param referenceDF dataframe with other dendrometers to be used as reference for the interpolation. The more dendrometers are included, the more robust will be the interpolation.
#'
#' @param niMethod string, either \emph{'linear'} or \emph{'proportional'} for interpolation method.
#'
#' @return A dataframe with \code{NA} values replaced by interpolated data.
#'
#' @examples library(dendRoAnalyst)
#' data("gf_nepa17")
#' df1<-gf_nepa17
#' # Creating an artificial reference dataset.
#' df2<-cbind(gf_nepa17,gf_nepa17[,2:3],gf_nepa17[,2:3])
#' # Creating gaps in dataset by replacing some of the reading with NA in dataset.
#' df1[40:50,3]<-NA
#' # Using proportional interpolation method.
#' df1_NI<-network.interpolation(df=df1, referenceDF=df2, niMethod='proportional')
#' head(df1_NI,10)
#'
#' @importFrom stats approx median na.exclude na.omit sd lm
#'
#' @importFrom boot boot boot.ci
#'
#' @export
network.interpolation<-function(df,referenceDF,niMethod){
  #temp1 <- data.frame(timestamp = as.POSIXct(strptime(df[,1],format = "%Y-%m-%d %H:%M:%S")))
  #if (is.na(as.POSIXct(temp1$timestamp[1], format = "%Y-%m-%d %H:%M:%S"))) {
    #stop("Date not in the right format")
  #}
  df1<-df
  #df1[,1]=temp1
  df2<-referenceDF
  meth<-niMethod
  cd<-2:ncol(df1)
  for(i in cd){
    if(length(which(is.na(df1[,i])))==0){
      next
    }else{
      f.loc<-which(is.na(df1[,i]))
      r.sq<-c()
      tm<-c()
      inp.v<-c()
      ul<-c()
      ll<-c()
      for(j in f.loc){
        ref.j<-c(t(df2[j,2:ncol(df2)]))
        ref.pj<-c(t(df2[(j-1),2:ncol(df2)]))
        ref.pj[ref.pj==0]<-0.001
        if(meth=='linear'){
          xy<-function(df,i){
            df2<-df[i,]
            lml<-lm(df2)
            d<-c(summary(lml)$adj.r.squared,lml$coefficients[2], lml$coefficients[1])
            return(d)
          }
          lmc<-function(x){
            A1<-boot::boot(data.frame(ref.pj,ref.j),statistic =xy,R=500)
            return(A1$t0)
          }
          A2<-lmc(data.frame(ref.pj,ref.j))
          #A1<-boot::boot(data.frame(ref.pj,ref.j),statistic =xy,R=500)
          r.sq<-c(r.sq,A2[1])
          df1[j,i]<-df1[(j-1),i]*A2[2]+A2[3]
          tm<-c(tm,df1[j,1])
          inp.v<-c(inp.v,df1[j,i])

        }else{
          if(meth=='proportional'){
            fc <- function(d, i){
              d2 <- d[i]
              return(mean(d2, na.rm = T))
            }
            mnc<-function(data){
              A<-boot::boot(p_ref.d,statistic = fc,R=500)
              B<-boot::boot.ci(A, conf=0.95, type="bca")
              return(c(A$t0, B$bca[4], B$bca[5]))
            }
            ref.d<-as.numeric((ref.j-ref.pj)/ref.pj)
            p_ref.d<-df1[(j-1),i]+(df1[(j-1),i]*ref.d)
            #A<-boot::boot(p_ref.d,statistic = fc,R=500)
            #B<-boot::boot.ci(A, conf=0.95, type="bca")
            st=mnc(p_ref.d)
            #print(mean(ref.d))
            df1[j,i]<-mean(st[1], na.rm=T)
            tm<-c(tm,df1[j,1])
            inp.v<-c(inp.v,df1[j,i])
            ul<-c(ul, st[3])
            ll<-c(ll, st[2])
          }else{
            stop("Invalid niMethod provided. Please choose either 'linear' or 'proportional'.")
          }

        }
      }
      print(paste('Interpolation in', colnames(df1)[i]))
      if(meth=='proportional'){
        print(data.frame('Time'=tm, 'Interpolated_value'=inp.v, 'LCI95'=ll, 'UCI95'=ul))
      }else{
        print(data.frame('Time'=tm,  'Interpolated_value'=inp.v, 'rsquared'=r.sq))
      }
    }
  }
  return(df1)
}
