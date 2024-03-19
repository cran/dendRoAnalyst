#' @title Detrend the dendrometer data
#'
#' @description This function detrends the dendrometer data either using first difference or using gam function from \code{\link[mgcv]{mgcv}} package or the Gompertz function.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.
#'
#' @param method either 'f_diff', 'gam' or 'gomp' indicating the method to detrend the dendrometer data column in \emph{df}.
#'
#' @param CalYear numeric for year of calculation. If df has more than one year, assigning CalYear truncates the data of only that year.
#'
#' @return A dataframe with the detrended dendrometer series.
#'
#' @importFrom stats approx median na.exclude na.omit sd predict
#' @importFrom lubridate ymd_hms ymd year yday
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when bind_cols
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text
#' @importFrom tidyr pivot_longer
#' @importFrom mgcv gam
#' @importFrom minpack.lm nlsLM
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(gf_nepa17)
#' detrended<-dm.detrend(df=nepa17, method='f_diff', CalYear=2017)
#' head(detrended,10)}
#'
#' @export
dm.detrend<-function(df, method='gam', CalYear){
  if(!(method%in%c('gam','f_diff','gomp'))){
    stop('Method must be either gam or f_diff or gomp.')
  }
  if(!(length(CalYear)==1)){
    stop('CalYear must be a single value.')
  }
  if (!inherits(df[[1]], "Date") && !inherits(df[[1]], "POSIXct")) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  TIME <- NULL
  df <- as_tibble(df)%>%
    rename(TIME = 1)
  if(!(CalYear%in%unique(year(df$TIME)))){
    stop('Provided CalYear does not exist in df')
  }
  df <- df%>%
    filter(year(TIME)%in%CalYear)
  df <- dendro.resample(df = df, by='D', value='mean')
  df <- df %>% mutate(across(where(is.numeric), ~ . - first(.)))
  x_val <- yday(df$TIME)
  res <- sapply(df[-1], function(col){
    y <- col
    cons <- ifelse(min(y)<0, abs(min(y)), 1)
    y_adj <- y+cons
    if(method == 'gam'){
      gam_fun <- gam(y~s(x_val, k = 10))
      y_pred <- predict(gam_fun)
      y_pred_adj <- y_pred+cons
      res <- y_adj/y_pred_adj
    }else{
      if(method == 'gomp'){
        a <- max(y)
        gom_mod <- nlsLM(y~a*exp(-exp(b-k*doy)),
                                     start = list(b=0.5, k=0.005))
        y_pred <- predict(gom_mod)
        y_pred_adj <- y_pred+cons
        res <- y_adj/y_pred_adj
      }else{
        res <- c(0, diff(y))
        res <- res+abs(min(res))
        res <- res/mean(res)
      }
    }
    return(res)
  })
  df <- bind_cols(df[1],res)
  return(df)
}
