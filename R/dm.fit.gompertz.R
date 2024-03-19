#' @title Fitting gompertz function on annual dendrometer data
#'
#' @description This function modells the annual growth of dendrometer data using gompertz function.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.
#'
#' @param CalYear numeric for year of calculation. If df has more than one year, assigning CalYear truncates the data of only that year.
#'
#' @param TreeNum numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.
#'
#' @param f_derivative logical if yes returns first derivative of gompertz curve.
#
#' @return A dataframe with the modelled dendrometer series.
#'
#' @importFrom stats approx median na.exclude na.omit sd predict
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom minpack.lm nlsLM
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(gf_nepa17)
#' gomp_fitted<-dm.fit.gompertz(df=gf_nepa17, TreeNum = 1, CalYear=2017)
#' head(gomp_fitted,10)}
#'
#' @export
dm.fit.gompertz<- function(df, CalYear, TreeNum, f_derivative=F){
  #################functions#############################
  gompertz_fit<-function(x,y,f.d){
    a <- max(y)
    gom_mod <- minpack.lm::nlsLM(y~a*exp(-exp(b-k*x_val)),start = list(b=0.5, k=0.005))
    para <- summary(gom_mod)
    b<- para$parameters[1,1]
    k<-para$parameters[2,1]
    if(f.d){
      y_pred <- der_gompertz(x,a,b,k)
    }else{
      y_pred <- predict(gom_mod)
    }
    return(y_pred)
  }
  der_gompertz<-function(doy,a,b,k){
    y.d<-exp(b-k*doy-exp(b-k*doy))
    return(y.d)
  }
  #######################################################
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
    filter(year(TIME) == CalYear)
  df <- dendro.resample(df = df, by='D', value='mean')
  df <- df %>% mutate(across(where(is.numeric), ~ . - first(.)))
  if(TreeNum =='all'){
    df <- df
  }else{
    df <- df%>%
      select(c(1,TreeNum+1))
  }
  x_val <- yday(df$TIME)

  res <- apply(df[-1], 2, function(col) gompertz_fit(x_val,col,f_derivative))
  df <- bind_cols(df[1],res)
  return(df)
}
