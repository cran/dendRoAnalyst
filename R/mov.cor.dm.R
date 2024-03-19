#' @title Calculating running correlation between dendrometer data and daily climate.
#'
#' @description This function calculates running correlation between dendrometer series and provided climate variables. Users can choose methods such as pearson, kendall and spearman, see \code{cor.test} for further information.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.
#'
#' @param Clim dataframe with the first column containing \code{Date in yyyy-mm-dd} and second column containing corresponding climate data.
#'
#' @param TreeNum numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.
#'
#' @param win_size numerical, the running days windows of which the correlation is to be calculated. The minimum value is 18.
#'
#' @param cor_method string, method to be applied during correlation calculation. One of the following: pearson, kendall and spearman
#'
#' @return A list of dataframes containing the correlation and significant value between dendrometer series and provided climates.
#'
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when pull
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom stats cor.test
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(gf_nepa17)
#' data(ktm_rain17)
#' out_corr<-mov.cor.dm(df=gf_nepa17, Clim=ktm_rain17, TreeNum=1, win_size=21)
#' head(out_corr[1],10)}
#' @export
mov.cor.dm<-function(df, Clim, TreeNum, win_size, cor_method='pearson'){
  if(!(cor_method%in%c('pearson', 'kendall', 'spearman')))
  if (!inherits(df[[1]], "Date") && !inherits(df[[1]], "POSIXct")) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  if (!inherits(Clim[[1]], "Date") && !inherits(Clim[[1]], "POSIXct")) {
    Clim[[1]] <- ymd(Clim[[1]])
  }
  TIME <- NULL

  df <- dendro.resample(df = df, by = 'D', value= 'mean')%>%
    mutate(TIME = as.Date(TIME))
  Clim <- as_tibble(Clim)%>%
    rename(TIME = 1)
  x <- intersect(Clim$TIME, df$TIME)
  if (length(x) == 0) {
    stop("'df' and 'Clim' must have time overlap.")
  }
  else {
    if ((length(x) == nrow(df)) & (length(x) == nrow(Clim))) {
      df <- df
      Clim <- Clim
    }
    else {
      warning("The temporal coverage of 'df' and 'Clim' is not same. The output data will be shortened to the common time period.")
      x <- as.Date(x, origin='1970-01-01', tz='UTC')
      df <- df %>% filter(TIME %in% x)
      Clim <- Clim %>% filter(TIME %in% x)
    }
  }
  ##########################################################################
  wl <- ifelse(win_size%%2==0, win_size+1, win_size) #win.length
  dm_col <- pull(df,TreeNum+1)
  n_row <- nrow(Clim)
  wl2<-(wl-1)
  wl3 <- wl2/2
  out<- lapply(Clim[-1], function(cls){
    estimate<-as.data.frame(matrix(ncol = 3, nrow = n_row))
    estimate[,1]<-as.Date(x, origin='1970-01-01', tz='UTC')
    for( n in seq(wl, n_row)){
      val_1 <- cls[(n-wl2):n]
      val_2 <- dm_col[(n-wl2):n]
      a=cor.test(val_1, val_2, methods=cor_method)
      estimate[n-wl3,2]<-a$estimate
      estimate[n-wl3,3]<-a$p.value
    }
    colnames(estimate)=c('TIME','corr','p_val')
    return(as_tibble(estimate))
  })
  class(out) <- "mov.cor"
  return(out)
}
