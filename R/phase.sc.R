#' @title Application of the stem-cycle approach to calculate different phases, their duration and to plot them.
#'
#' @description This function analyses the dendrometer data using Stem-cycle approach (Downs et al. 1999; Deslauriers et al. 2011). A function that defines three phases: 1) Shrinkage, when the dendrometer reading is less than previous reading, 2) Expansion, when current reading is more than previous reading and 3) Increment, when current reading is higher than the previous maximum. Additionally, it calculates various statistics for each phase.
#'
#' @references Deslauriers A, Rossi S, Turcotte A, Morin H, Krause C (2011) A three-step procedure in SAS to analyze the time series from automatic dendrometers. Dendrochronologia 29:151–161. \doi{10.1016/j.dendro.2011.01.008}
#'
#' @references Downes G, Beadle C, Worledge D (1999) Daily stem growth patterns in irrigated Eucalyptus globulus and E. nitens in relation to climate. Trees 14:102–111. \doi{10.1007/PL00009752}
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS}. It should contain data with constant temporal resolution for best results.
#'
#' @param TreeNum numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.
#'
#' @param smoothing numerical value from 1 to 12 which indicates the length of the smoothing spline, i.e. 1 = 1 hour and 12 = 12 hours. Default is \code{NULL} for no smoothing.The function \code{\link[pspline:smooth.Pspline]{smooth.Pspline}} is used for smoothing the data.
#'
#' @return A list of two dataframes. The first dataframe \emph{SC_cycle} with cyclic phases along with various statistics and the second dataframe \emph{SC_phase} with assigned phases for each data point.The dataframe \emph{SC_cycle} contains the beginning, end, duration, magnitude and rate of each phase. The dataframe \emph{SC_phase} contains time and corresponding phases during that time.
#' The contents of \emph{SC_cycle} are:
#' \tabular{llll}{
#' \strong{Columns}\tab\tab   \strong{Description}\cr
#' \code{Phase}\tab\tab	Cyclic phases. 1, 2, and 3 for Shrinkage, Expansion, and Increment respectively.\cr
#' \code{start}\tab\tab	Time when the corresponding phase starts.\cr
#' \code{end}\tab\tab	Time when the corresponding phase ends.\cr
#' \code{Duration_h}\tab\tab	Duration of the corresponding phase in hours.\cr
#' \code{Duration_m}\tab\tab	Duration of the corresponding phase in minutes.\cr
#' \code{Magnitude}\tab\tab	Radial/circumferential change during the corresponding phase in millimeters.\cr
#' \code{rate}\tab\tab	Rate of Radial/circumferential change in micrometers per hour.\cr
#' \code{DOY}\tab\tab	Day of year for the corresponding phase.

#' }
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(gf_nepa17)
#' sc.phase<-phase.sc(df=gf_nepa17, TreeNum=1, smoothing=12)
#' head(sc.phase[[1]],10)
#' head(sc.phase[[2]],10)}
#'
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom pspline smooth.Pspline
#'
#' @export
phase.sc<-function(df, TreeNum, smoothing=NULL){
  ##################### function to calculate phases################
  phase_cal <- function(y){
    all_max <- cummax(y)
    all_max_diff <-diff(all_max)
    #all_max_diff <-all_max_diff[2:length(all_max_diff)]
    phases<- vector('integer', length(y)-1)
    # phases[all_max_diff > 0] <- 3
    y_diff <- diff(y)
    # phases[all_max_diff = 0 & y_diff>=0] <- 2
    # phases[all_max_diff = 0 & y_diff<0] <- 1
    phases<-sapply(seq_along(all_max_diff), function(i){
      if(all_max_diff[i]>0){
        ph <- 3
      }else{
        if(y_diff[i]>=0){
          ph <- 2
        }else{
          ph <- 1
        }
      }
      #print(length(phases))
      return(ph)
    })
    return(phases)
  }
  #####################function to cal phase stats #################
  phase_sats<-function(dm_data, y){
    ph_diff<-c(0,diff(y))
    dy<-yday(c(dm_data$TIME[1], dm_data$TIME[ph_diff!=0]))
    ph<-c(y[1],y[ph_diff!=0])
    magn<-c(dm_data$dm[1], dm_data$dm[ph_diff!=0], dm_data$dm[nrow(dm_data)])
    magn<-diff(magn)
    strt_t<-c(dm_data$TIME[1], dm_data$TIME[ph_diff!=0])
    #strt_t<-strt_t[1:length(strt_t)-1]
    end_t <- c(strt_t[2:length(strt_t)], dm_data$TIME[nrow(dm_data)])
    dur_h<-as.numeric(difftime(end_t, strt_t), units='hours')
    dur_m<-as.numeric(difftime(end_t, strt_t), units='mins')
    out <- tibble(
      'Phases' = ph,
      'Start' = strt_t,
      'End' = end_t,
      'Duration_h' = dur_h,
      'Duration_m' = dur_m,
      'Magnitude' = magn,
      'rate' = magn*1000/dur_h,
      'DOY' = dy
    )
    return(out)
  }
  #####################function to calculate resolution ############
  reso_den<-function(input_time){
    time1<-input_time
    reference<-time1[1]
    time_min<-as.integer(difftime(time1,reference, units = "mins"))
    diff_time<-diff(time_min)
    diff2_time<-unique(diff_time)
    reso<-mean(diff2_time)
    if(isTRUE(length(diff2_time>1))==T){
      print(diff2_time)
      warning('Warning: The temporal resolution of dendrometer data is not consistent, For better result, please use dendrometer data with consistent resolution. There may be NA values in the dataset.')
      #cat(paste('Mean temporal resolution is :', round(reso),' minutes.'))
    }else{
      reso<-mean(diff2_time)
      #cat(paste('Temporal resolution is :', round(reso),' minutes.'))
      return(round(reso))
    }
  }
  ##################################################################
  if (!inherits(df[[1]], 'Date') && !inherits(df[[1]], 'POSIXct')) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  dm_data <- as_tibble(df)%>%
    select(c(1,TreeNum+1))
  sf<-smoothing
  dm_data <- dm_data %>%
    rename(TIME = 1,
           dm = 2)
  r.denro<-reso_den(dm_data$TIME)
  if(is.null(sf)==T){
    y_sm <- dm_data$dm
  }else{
    if(sf>12|sf<1){
      stop('smoothing must be between 1 to 12.')
    }else{
      spar2<-(60/r.denro)*12
      spar3<-(spar2/12)*sf
      sm<-smooth.Pspline(1:nrow(dm_data),dm_data$dm,spar=spar3, w=spar3*0.1)$ysmth
      y_sm<-sm[,1]
    }
  }
  y<-phase_cal(y_sm)
  ph_st<-phase_sats(dm_data, y)
  dm_data <- dm_data%>%
    mutate('Phases' = c(NA,y))
  out<-list(SC_cycle = ph_st, SC_phase = tibble(dm_data))
  class(out) <- "SC_output"
  return(out)
}

