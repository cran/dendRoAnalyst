#' @title Application of the zero-growth approach to calculate different phases, their duration and to plot them.
#'
#' @description This function analyses data using the zero-growth approach. Initially, it divides the data in two categories: 1) Tree water deficiency (TWD), i.e. the reversible shrinkage and expansion of the tree stem when the current reading is below the previous maximum and, 2) Increment (GRO), the irreversible expansion of the stem when the current reading is above the previous maximum. Then it calculates the TWD for each data point as the difference between the modelled "growth line" and the observed measurement. See Zweifel et. al.,(2016) for details.
#'
#' @references Zweifel R, Haeni M, Buchmann N, Eugster W (2016) Are trees able to grow in periods of stem shrinkage? New Phytol 211:839–849. \doi{10.1111/nph.13995}
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS}. It should contain data with constant temporal resolution for best results.
#'
#' @param TreeNum numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.
#'
#' @return A list of two dataframes. The first dataframe \emph{ZG_cycle} contains the cyclic phases along with various statistics and the second dataframe \emph{ZG_phase} with assigned phases for each data point.The contents of \emph{ZG_cycle} are:
#' \tabular{llll}{
#' \strong{Columns}\tab\tab   \strong{Description}\cr
#' \code{DOY}\tab\tab	Day of year for the corresponding phase.\cr
#' \code{Phase}\tab\tab	TWD for tree water deficit and GRO for irreversible expansion.\cr
#' \code{start}\tab\tab	Time when the corresponding phase starts.\cr
#' \code{end}\tab\tab	Time when the corresponding phase ends.\cr
#' \code{Duration_h}\tab\tab	Duration of the corresponding phase in hours.\cr
#' \code{Magnitude}\tab\tab	Radial/circumferential change in millimeters.\cr
#' \code{rate}\tab\tab	Rate of Radial/circumferential change in micrometers per hour.\cr
#' \code{Max.twd}\tab\tab	Maximum TWD recorded for the corresponding TWD phase.\cr
#' \code{twd.severity}\tab\tab	The severity of the individual TWD period (see description below).\cr
#' \code{Max.twd.time}\tab\tab	Time of occurrence of maximum TWD value for each TWD phase.\cr
#' \code{Avg.twd}\tab\tab	Average of TWD values for each TWD phase.\cr
#' \code{STD.twd}\tab\tab	Standard deviation of TWD values for each TWD phase.
#' }
#'
#'@description The severity value of each TWD was introduced in version 0.1.4 of the package.
#'
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(gf_nepa17)
#' zg.phase<-phase.zg(df=gf_nepa17[1:600,], TreeNum=1)
#' head(zg.phase[[1]],10)
#' head(zg.phase[[2]],10)}
#'
#' @importFrom stats approx median na.exclude na.omit sd
#'
#' @export
phase.zg<-function (df, TreeNum){
  ############################################################################
  Duration_h <- NULL
  phase_cal_zg <- function(y){
    all_max <- cummax(y)
    phases<- vector('integer', length(y))
    phases[all_max-y==0] <- 2
    phases[all_max>y] <-1
    return(phases)
  }
  normalize <- function(x) {
    return ((x - min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T)))
  }
  phase_sats_zg<-function(dm_data, y){
    ph_diff<-c(0,diff(y))
    gro_crv<-cummax(dm_data$dm)
    twd_crv<-gro_crv-dm_data$dm
    dy<-yday(c(dm_data$TIME[1], dm_data$TIME[ph_diff!=0]))
    ph<-c(y[1],y[ph_diff!=0])
    magn<-c(dm_data$dm[1], dm_data$dm[ph_diff!=0], dm_data$dm[nrow(dm_data)])
    magn<-diff(magn)
    magn[ph==1] <- NA
    strt_t<-c(dm_data$TIME[1], dm_data$TIME[ph_diff!=0])
    #strt_t<-strt_t[1:length(strt_t)-1]
    end_t <- c(strt_t[2:length(strt_t)], dm_data$TIME[nrow(dm_data)])
    dur_h<-as.numeric(difftime(end_t, strt_t), units='hours')
    st_loc<-c(1, which(ph_diff!=0))
    end_loc<-c(st_loc[2:length(st_loc)], length(y))
    max_values <- sapply(1:length(st_loc), function(i) {
      max(twd_crv[st_loc[i]:end_loc[i]])
    })
    max_time <- sapply(1:length(st_loc), function(i) {
      twd_<-twd_crv[st_loc[i]:end_loc[i]]
      t<-dm_data$TIME[st_loc[i]:end_loc[i]]
      last(t[twd_==max(twd_)])
    })
    avg_twd <- sapply(1:length(st_loc), function(i) {
      mean(twd_crv[st_loc[i]:end_loc[i]], na.rm=T)
    })
    std_twd <- sapply(1:length(st_loc), function(i) {
      sd(twd_crv[st_loc[i]:end_loc[i]], na.rm=T)
    })
    max_time <- as.POSIXct(max_time, origin = "1970-01-01", tz = "UTC")
    st_max_diff<-as.numeric(difftime(max_time,strt_t, units = 'hours'))
    st_max_diff[ph==2]<-NA
    st_max_diff_norm<-normalize(st_max_diff)
    end_max_diff<-as.numeric(difftime(end_t,max_time, units = 'hours'))
    end_max_diff[ph==2]<-NA
    end_max_diff_norm<-normalize(end_max_diff)
    max_values[ph==2]<-NA
    max_values_norm<-normalize(max_values)
    sev<-0.3*st_max_diff_norm+0.3*end_max_diff_norm+0.4*max_values_norm
    max_time[ph==2]<-NA
    avg_twd[ph==2]<-NA
    std_twd[ph==2]<-NA
    out <- tibble(
      'Phases' = ph,
      'Start' = strt_t,
      'End' = end_t,
      'Duration_h' = dur_h,
      'Magnitude' = magn,
      'rate' = magn*1000/dur_h,
      'max.twd' = max_values,
      'Max.twd.time' = max_time,
      'twd.severity' = sev,
      'Avg.twd' = avg_twd,
      'STD.twd' = std_twd,
      'DOY' = dy
    )%>%
      filter(Duration_h > 0)
    dm_data <- dm_data %>%
      mutate('Phases' = y,
             'TWD' = twd_crv,
             'GRO' = gro_crv)
    return(list(out, tibble(dm_data)))
  }
  #############################################################################
  if (!inherits(df[[1]], 'Date') && !inherits(df[[1]], 'POSIXct')) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  dm_data <- df%>%
    select(c(1,TreeNum+1))%>%
    rename(TIME = 1,
           dm = 2)
  #colnames(dm_data) <- c('Time', 'dm')
  y<-phase_cal_zg(dm_data$dm)
  ot<-phase_sats_zg(dm_data, y)
  out<-list(ZG_cycle = ot[[1]], ZG_phase = ot[[2]])
  class(out) <- "ZG_output"
  return(out)
}
