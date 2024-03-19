#' @title Locating the maxima of TWD periods
#'
#' @description This function detects the TWD phases, including their beginning (TWDb), using the phase.zg function. Then it calculates the number, time of occurance (Tm) and value of every local maximum within each TWD phase. In addition it calculates the time difference between 'TWDb' and each 'Tm' within each TWD phase.
#'
#' @param df data frame with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS}. It should contain data with constant temporal resolution for best results.
#'
#' @param TreeNum numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.
#'
#' @param smoothing numerical value from 1 to 12 which indicates the length of the smoothing spline, i.e. 1 = 1 hour and 12 = 12 hours. Default is 5.
#'
#' @return A data frame with statistics of maxima in each TWD phase.
#'
#' @importFrom lubridate ymd_hms ymd
#' @importFrom tidyverse tidyverse_packages
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when bind_rows
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(gf_nepa17)
#' df1=gf_nepa17[2500:3500,]
#' twd_max<-twd.maxima(df=df1, TreeNum=2)
#' head(twd_max,10)}
#'
#' @export
twd.maxima<-function(df, TreeNum, smoothing=5){
  TIME <- Phases <- mtwd <- NULL
  n<-TreeNum
  sc<-phase.sc(df=df, TreeNum=n, smoothing=smoothing)
  zg<-phase.zg(df=df, TreeNum=n)
  sc1<-sc[[1]]
  zg1<-zg[[1]]
  zg2<-zg[[2]]
  sc1.1<-sc1 %>%
    filter(Phases == 1)
  zg1.1<- zg1 %>%
    filter(Phases == 1)
  strt.sh<-sc1.1$Start
  end.p <- sapply(sc1.1$Start, function(i){
    return(sc1$End[which(sc1$Start==i)+1])
  })
  #######################################
  res <- sapply(seq_along(strt.sh), function(loc){
    cr.twd<-zg2 %>%
      slice(which(TIME==strt.sh[loc]):which(TIME==end.p[loc]))
    mx <- first(max(cr.twd$TWD, na.rm = T))
    loc.mx <- first(which(zg2$TWD==mx))
    return(loc.mx)
  })
  ################################################
  zg2$mtwd<-0
  zg2$mtwd[res]<-1
  mins3<-lapply(seq_along(zg1.1[[1]]), function(twd){
    twd.st<-which(zg2$TIME==zg1.1$Start[twd])
    twd.end<-which(zg2$TIME==zg1.1$End[twd])
    temp1<-zg2%>%
      slice(twd.st:twd.end)%>%
      filter(mtwd == 1) %>%
      mutate(Duration_from_start = difftime(TIME, zg1.1$Start[twd], units = 'mins'),
             Start.time = zg2$TIME[twd.st],
             End.time = zg2$TIME[twd.end],
             twd.number = twd
             )
    temp2 <- temp1%>%
      select(c(8,9,1,3,4,7,ncol(temp1)))
    return(temp2)
  })
  return(bind_rows(mins3))
}
