#' @title Calculating relative growth change during no-rain periods.
#'
#' @description This function calculates the number and the location of climatically adverse periods within a climate time series. The user can define a duration and threshold of these conditions. The function also provides the relative radial/circumferencial change during each adverse period for the original or normalized data. See Raffelsbauer et al., (2019) for more details.
#'
#' @references Raffelsbauer V, Spannl S, Peña K, Pucha-Cofrep D, Steppe K, Bräuning A (2019) Tree Circumference Changes and Species-Specific Growth Recovery After Extreme Dry Events in a Montane Rainforest in Southern Ecuador. Front Plant Sci 10:342. \doi{10.3389/fpls.2019.00342}
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.
#'
#' @param Clim dataframe with the first column containing \code{Date in yyyy-mm-dd} and second column containing corresponding climate data.
#'
#' @param dailyValue either \emph{'max', 'min', 'mean'}, or \emph{'sum'} for selecting the daily resampled value. Default is \emph{'max'}. See \code{\link[dendRoAnalyst:dendro.resample]{dendro.resample}} for details.
#'
#' @param thresholdClim string, the theshold for the respective climatic parameter. E.g. if climatic data is precipitation then days, where precipitation is below or equal to this value, are considered as adverse climate. Dafault is '<10'.
#'
#' @param thresholdDays string, the minimum number of consecutive adverse days to be considered for analysis. For example, \code{thresholdDays}=2 means the relative radial/circumferential change is calculated for adverse periods lasting for more than 2 days. Default is '>5'.
#'
#' @param showPlot logical, if \code{TRUE}, generates plots.
#'
#' @return A dataframe containing the respective periods, relative radial/circumference change for each tree, the ID for each period and their beginning and end.
#'
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text
#' @importFrom tidyr pivot_longer
#' @examples
#' \donttest{
#' library(dendRoAnalyst)
#' data(gf_nepa17)
#' data(ktm_rain17)
#' relative_dry_growth<-clim.twd(df=gf_nepa17, Clim=ktm_rain17, dailyValue='max', showPlot=TRUE)
#' 1
#'
#' head(relative_dry_growth,10)
#'
#'}
#' @export
clim.twd <- function(df,Clim,dailyValue='max',thresholdClim = '<10',thresholdDays ='>5', showPlot=TRUE){
  if (!inherits(df[[1]], 'Date') && !inherits(df[[1]], 'POSIXct')) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  if (!inherits(Clim[[1]], 'Date') && !inherits(Clim[[1]], 'POSIXct')) {
    Clim[[1]] <- ymd(Clim[[1]])
  }
  TIME <- DATE <-IDs <-climate <-condition <- Value <- Variable <- NULL
  df <- dendro.resample(df=df, by='D', value=dailyValue)
  df <- df%>%
    mutate('TIME' = as.Date(TIME))
  #df$TIME <- yday(df$TIME)
  clim <- as_tibble(Clim)
  clim <- clim%>%
    rename('DATE' = 1,
           'climate' = 2)
  ctime <- intersect(df$TIME, clim$DATE)
  if(length(ctime)==0){
    stop("The dendrometer and climate data do not have a common time period.")
  }else{
    if((length(ctime)==nrow(df)) & (length(ctime)==nrow(clim))){
      df <- df
      clim <- clim
    }else{
      message("The analyses period is reduced to common time period.")
      common_t<-as.Date(ctime, origin='1970-01-01', tz='UTC')
      df<-df%>%filter(TIME%in%common_t)
      clim<-clim%>%filter(DATE%in%common_t)
    }
  }
  operator<-substr(thresholdClim, 1, 1)
  value <- as.numeric(substr(thresholdClim, 2, nchar(thresholdClim)))
  operator_fun <- match.fun(operator)
  x <- ifelse(operator_fun(clim[[2]],value), 1,0)
  st <- c(x[1], diff(x)) == 1
  cond <- cumsum(st)*x

  operator_c<-substr(thresholdDays, 1, 1)
  value_c <- as.numeric(substr(thresholdDays, 2, nchar(thresholdDays)))
  operator_fun_c <- match.fun(operator_c)

  if(!(value_c%in%c(0,1))){
    cond2 <- cond[cond>0]
    cond3 <-unique(cond2)
    for(i in seq_along(cond3)){
      cond[cond==cond3[i]] <- ifelse(operator_fun_c(length(cond[cond==cond3[i]]), value_c), i, 0)
    }
    cond[cond>0] = 1
    st <- c(cond[1], diff(cond)) == 1
    cond <- cumsum(st)*cond
  }

  df <- df %>%
    mutate('IDs' = cond)

  clim <- clim %>%
    mutate('condition' = st,
           'IDs' = cond)
  result <- df %>%
    group_by(IDs) %>%
    mutate(across(where(is.numeric), ~ . - first(.))) %>%
    ungroup()%>%
    filter(IDs>0)
  if(showPlot){
  #plotting the precipitation data
    p1 <- ggplot(clim, aes(x = DATE, y = climate, group=1)) +
      geom_area(fill = 'blue') +
      geom_rect(data = subset(clim, condition),
                aes(xmin = DATE, xmax = DATE+value_c , ymin = -Inf, ymax = Inf),
                fill = "red", alpha = 0.2) +
      geom_text(data=subset(clim, condition), aes(x=DATE,label = IDs), nudge_y = max(clim$climate))+
      #scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey")) +
      theme_minimal() +
      labs(title = "Condition-based climate Plot", x = "Date", y = "Climate value")
    print(p1)

    #################################################################################################
    print('The adverse climatic periods are listed below:')
    print(unique(result$IDs))
    print("Please choose periods' id from the list. Please press enter two times when you are done.")
    perd<-c()
    perd<-scan()
    if(length(perd)==0){
      perd<-1
    }
    fdf<- result %>%
      filter(IDs%in%perd)
    data_long <- pivot_longer(fdf, cols = c(-1,-ncol(result)), names_to = "Variable", values_to = "Value")
    p2 <- ggplot(data_long, aes(x = TIME, y = Value, color = Variable)) +
      geom_line() +
      geom_point()
      if(length(perd)>1){
        p2 <- p2+facet_wrap(~ IDs, scales = "free", ncol = 1)
      }
      p2<-p2+labs(title = "Relative growth change in adverse climate",
           x = "Time",
           y = "Relative growth change",
           color = "Trees") +
      theme_minimal() + theme(legend.position = "bottom") +theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
    print(p2)
  }
  return(result)
}
