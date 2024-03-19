#' @title Detection and interpolation of missing values in dendrometer data.
#'
#' @description This function detects gap(s) in time series, inserts the missing rows based on the provided temporal resolution and assings \code{NA} values to the corresponding value. If required the \code{NA} values can be replaced by spline interpolation using \code{\link[zoo:na.approx]{na.spline}} of the package \pkg{zoo} or seasonal interpolation considering the seasonality of the daily pattern using \code{\link[forecast:na.interp]{na.interp}} of the package \pkg{forecast}.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and following columns with dendrometer data for the same temporal resolution and time period.
#'
#' @param resolution integer, indicating the resolution of dendrometer data in \strong{minutes}.
#'
#' @param fill logical, if \code{TRUE} it fills the \code{NA} values using spline interpolation. Default is \code{FALSE}.
#'
#' @param method string, 'spline' for the spline interpolation or 'seasonal' for the seasonal interpolation.
#'
#' @return A dataframe containing the dendrometer data including gaps filled with either \code{NA} or interpolated values.
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(nepa17)
#' gf_nepa17<-dm.na.interpolation(df=nepa17, resolution=60)
#' head(gf_nepa17,10)}
#'
#' @importFrom stats approx median na.exclude na.omit sd ts
#' @importFrom lubridate ymd_hms ymd isoweek
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when first lead last left_join
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom zoo na.spline
#' @importFrom forecast na.interp
#'
#' @export
dm.na.interpolation <- function(df, resolution, fill = FALSE, method='spline') {
  ##############################################################################
  replace_na_with_spline <- function(column) {
    if(is.numeric(column)) {
      sp <- na.spline(column)
      column[is.na(column)] <- sp[is.na(column)]
    }
    return(column)
  }
  replace_na_with_seasonality<- function(date,column, s, gap.info){
    df <- tibble(date, column)%>%
      rename(date = 1,
             dm = 2)%>%
      mutate('yr' = year(date), 'wd' = isoweek(date))
    yr2 <- year(gap.info$TIME)
    wd2 <- isoweek(gap.info$TIME)
    for(i in 1:length(wd2)){
      df11 <- df%>%filter(yr==yr2[i] & wd%in%c(wd2[i]-1,wd2[i],wd2[i]+1))
      #print(df11$wd)
      #print(df11$dm)
      value <- ts(df11$dm, frequency = s)
      value <- na.interp(value)
      column[df$date%in%df11$date] <- value
    }
    return(column)
    }
  ##############################################################################
  TIME<-yr<-wd<-time_min<-diff_time <-Gap.num <- NULL
  if(!(method%in%c('spline','seasonal'))){
    stop("The method must be either 'spline' or ''seasonal")
  }
  if (!inherits(df[[1]], 'Date') && !inherits(df[[1]], 'POSIXct')) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  df <- tibble(df) %>%
    rename(TIME = 1) %>%
    mutate(time_min = as.integer(difftime(TIME, first(TIME), units = "mins")),
           diff_time = lead(time_min)-time_min)
  gap.info <- df %>%
    filter(diff_time > resolution) %>%
    mutate(Gap.num = diff_time/resolution-1)%>%
    select(TIME,Gap.num)
  df<-df%>%
    mutate(time_min = NULL,
           diff_time = NULL)
  if(nrow(gap.info) == 0){
    na_check <- colSums(is.na(df))[-1]
    if(all(na_check == 0)){
      message("There are no gap in recording time or any internal 'NA' value.\n")
      df <- df
    }else{
      message("There are no gap in recording time but there may be 'NA' values in reading.\n")
      if (fill) {
        message("Filling the internal 'NA' values.\n")
        if(method=='spline'){
          result <- sapply(df[-1], replace_na_with_spline)
        }else{
          result <- sapply(df[-1], function(col){
            dt <- ts(col, frequency = 1440/resolution)
            dt <- na.interp(dt)
            return(dt)
          })
        }
        df <- bind_cols(df[1], result)
      }
    }
    return(df)
  }else{
    print(gap.info)
    x1 <- first(df$TIME)
    x2 <- last(df$TIME)
    df_fill <- tibble(TIME = seq(from=x1, to=x2, by = paste(resolution,'mins')))%>%
      left_join(df, by = c("TIME" = "TIME"))

    if (fill) {
      if (max(gap.info$Gap.num, na.rm = TRUE) >= (1440 / resolution)) {
        warning("WARNING: There is a gap in dataset lasting for more than 24 hours. The filling may not be reliable.\n")
      } else {
        message("Filling the gap ..... ....\n")
        if(method=='spline'){
          df_fill2 <- sapply(df_fill[-1], replace_na_with_spline)
        }else{
          df_fill2 <-apply(df_fill[-1], 2, function(col) replace_na_with_seasonality(df_fill[1],col,1440/resolution, gap.info))
        }
        df_fill <- bind_cols(df_fill[1], df_fill2)
      }
    }
    # df_fill <- df_fill%>%
    #   mutate(time_min = NULL,
    #          diff_time = NULL)
    return(df_fill)
  }
}
