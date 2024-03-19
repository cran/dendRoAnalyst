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
#' @importFrom stats lm
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data("gf_nepa17")
#' df1<-gf_nepa17
#' # Creating an artificial reference dataset.
#' df2<-cbind(gf_nepa17,gf_nepa17[,2:3],gf_nepa17[,2:3])
#' colnames(df2) <- c('Time','T1','T2','T3','T4','T5','T6')
#' # Creating gaps in dataset by replacing some of the reading with NA in dataset.
#' df1[40:50,3]<-NA
#' # Using proportional interpolation method.
#' df1_NI<-network.interpolation(df=df1, referenceDF=df2, niMethod='proportional')
#' head(df1_NI,10)}
#'
#' @export
network.interpolation<-function(df, referenceDF, niMethod){
  ################################################
  bootstrap_mean <- function(data, n_bootstrap) {
    bootstrap_means <- numeric(n_bootstrap)
    for (i in 1:n_bootstrap) {
      bootstrap_sample <- sample(data, size = length(data), replace = TRUE)
      bootstrap_means[i] <- mean(bootstrap_sample)
    }
    original_mean <- mean(data)
    bootstrap_se <- sd(bootstrap_means)
    return(list(bootstrap_mean = mean(bootstrap_means), bootstrap_se = bootstrap_se))
}
  ################################################
  if(!(niMethod%in%c('proportional','linear'))){
    stop("The 'niMethod' must be either 'proportional' or 'linear'")
  }
  if (!inherits(df[[1]], 'Date') && !inherits(df[[1]], 'POSIXct')) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  if (!inherits(referenceDF[[1]], 'Date') && !inherits(referenceDF[[1]], 'POSIXct')) {
    referenceDF[[1]] <- ymd_hms(referenceDF[[1]])
  }
  TIME <- NULL
  #referenceDF[,1]<-as_datetime(referenceDF[,1])
  df<- tibble(df)%>%
    rename(TIME = 1)
  df_r<-tibble(referenceDF)%>%
    rename(TIME = 1)
  ctime <- intersect(df$TIME,df_r$TIME)
  if(length(ctime)==0){
    stop("'df' and 'referenceDF' must have time overlap.")
  }else{
    if((length(ctime)==nrow(df)) & (length(ctime)==nrow(df_r))){
      df<-df
      df_r<-df_r
    }else{
      warning("The temporal coverage of 'df' and 'referenceDF' is not same. The output data will be shortened to the common time period.")
      common_t<-as.POSIXct(ctime, origin='1970-01-01', tz='UTC')
      df<-df%>%filter(TIME%in%common_t)
      df_r<-df_r%>%filter(TIME%in%common_t)
    }

  }
  #print(df)
  filled <- sapply(df[-1], function(cl){
    if(length(which(is.na(cl)))==0){
      return(cl)
    }else{
      na.loc<-which(is.na(cl))
      for(i in na.loc){
        a_p <- ifelse(cl[i-1]==0,0.001,cl[i-1])
        b_p <- unlist(df_r[-1]%>%slice(i-1))
        b <- unlist(df_r[-1]%>%slice(i))
        if(niMethod=='proportional'){
          percent_ch<-(b-b_p)/b_p
          bp<-bootstrap_mean(percent_ch, n_bootstrap = 500)
          cl[i] <- a_p+a_p*bp[[1]]
        }else{
          lm1<-lm(b~b_p)
          #d<-c(lml$coefficients[2], lml$coefficients[1])
          cl[i] <- a_p*lm1$coefficients[2]+lm1$coefficients[1]
          #print(cl[i])
        }
      }
      return(cl)
    }
  })
  df <- bind_cols(df[1],filled)
  return(df)
}
