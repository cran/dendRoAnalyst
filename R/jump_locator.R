#' @title Removing artefacts due to manual adjustments of dendrometers automatically for more than one dendrometers
#'
#' @description Dendrometers generally have limited memory capacity beyond which it stops recording. To keep the measurement ongoing, they should be adjusted periodically, which can cause positive or negative jumps in the data. This function locates these artefacts and adjusts them.Unlike in \code{\link[dendRoAnalyst:i.jump.locator]{i.jump.locator}}, it can handle dataset with more than one dendrometers.
#'
#' @return A dataframe containing jump-free dendrometer data.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.
#'
#' @param v numerical value which is considered as artefact. E.g. \code{v}=1 implies that if the difference to the consecutive data point is more than 1 or less than -1, it will be considered as an artefact.
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(nepa)
#' jump_free_nepa<-jump.locator(df=nepa ,v=1)
#' head(jump_free_nepa,10)}
#'
#' @export
jump.locator<-function(df, v){
  if (!inherits(df[[1]], 'Date') && !inherits(df[[1]], 'POSIXct')) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  df <- tibble(df)%>%
    rename(TIME = 1)
  df2 <- df[-1]
  c_name <- colnames(df2)
  locs <- sapply(seq_along(df2), function(cl){
    column <- df2[[cl]]
    jd<-c(0,diff(column, na.rm=T))
    jl<- which(abs(jd)>=v)
    if(length(jl)==0){
      print(paste('There is no jump in ',c_name[cl]))
      return(column)
    }else{
      l <- vector('double', length(jl))
      t <- vector(length = length(jl))
      v1 <- vector('double', length = length(jl))
      for(i in 1:length(jl)){
        t[i] <- df$TIME[jl[i]]
        l[i] <- jl[i]
        v1[i] <- jd[jl[i]]
        column[jl[i]:length(column)] <- column[jl[i]:length(column)] - jd[jl[i]]
      }
      print(paste('Jump information in ',c_name[cl]))
      print(tibble(
        'Index' = l,
        'Jump_time' = as.POSIXct(t, origin = "1970-01-01", tz = "UTC"),
        'Jump_value' = v1
      ))
      return(column)
    }
  })
  colnames(locs)<-c_name
  df <- bind_cols(df[1],locs)
  return(df)
}
