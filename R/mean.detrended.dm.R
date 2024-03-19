#' @title Calculate mean of detrended dendrometer data.
#'
#' @description This function calculate the mean detrended series of dendrometer data. It is usefull to make a single averaged detrended dendrometer series of a species in a site. Further, it provides option for removing first order autocorrelation from dendrometer series using \code{auto.arima} function of "forecast" package.
#'
#' @param detrended.dm dataframe output data frame of \code{dm.detrend} function.
#'
#' @param ac1.remove logical if TRUE removes first order autocorrelation.
#'
#' @param robust.mean logical if TRUE calculates robust mean.
#'
#' @return A data frame with the mean of detrended dendrometer series.
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(gf_nepa17)
#' detrended<-dm.detrend(df=nepa17, method='f_diff', CalYear=2017)
#' m_detre <- mean_detrended.dm(detrended)
#' head(m_detre,10)}
#'
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when
#' @importFrom tibble as_tibble tibble
#' @importFrom stats residuals
#' @export
mean_detrended.dm<- function(detrended.dm, ac1.remove=T, robust.mean=T){
  #######################################################
  robust_mean <- function(data){
    avg <- mean(data, trim = 0.15)
    return(avg)
  }
  ac1_remove <- function(data){
    ac1_model <- forecast::auto.arima(data)
    ac1_res <- residuals(ac1_model)
    return(ac1_res)
  }
  #######################################################
  if (!inherits(detrended.dm[[1]], 'Date') && !inherits(detrended.dm[[1]], 'POSIXct')) {
    detrended.dm[[1]] <- ymd_hms(detrended.dm[[1]])
  }
  result <- apply(detrended.dm[-1], MARGIN = 1, FUN = ifelse(robust.mean, robust_mean,'mean'))
  df_result <- detrended.dm[1]%>%mutate('STD_DDM' = result)
  if (ac1.remove){
    result <- ac1_remove(result)
    result <- result+abs(min(result))
    result <- result/mean(result)
    df_result <- df_result%>%mutate('RES_DDM' = result)
  }
  return(df_result)
}
