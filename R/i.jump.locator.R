#' @title Removing artefacts due to manual adjustments of dendrometers interactively
#'
#' @description Dendrometers generally have limited memory capacity beyond which it stops recording. To keep the measurement ongoing, they should be adjusted periodically, which can cause positive or negative jumps in the data. This function locates these artefacts and interactively adjusts them one by one.
#'
#' @return A dataframe containing jump-free dendrometer data.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.
#'
#' @param TreeNum numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.
#'
#' @param v numerical value which is considered as artefact. E.g. \code{v}=1 implies that if the difference to the consecutive data point is more than 1 or less than -1, it will be considered as an artefact.
#'
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when slice
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text geom_vline xlab ylab ggtitle
#' @importFrom tidyr pivot_longer
#' @export

i.jump.locator<-function(df, TreeNum, v){
  ##########################################
  plot_jump <- function(df, jl, st, t, title){
    df <- df%>%
      rename(dm = 2)
    p<-ggplot(data=df, mapping = aes(TIME, dm))+
      geom_line(color='black')+
      geom_vline(xintercept = as.numeric(t), color = ifelse(st=="a","red","green"), lwd = 3, alpha=0.5)+
      theme_minimal()+xlab('Time')+ylab('Stem size variation')+ggtitle(title)
    print(p)
  }
  ##########################################
  TIME <- dm <- . <- NULL
  if(TreeNum>ncol(df)-1){
    stop('Tree number does not exist in the provided dataset')
  }
  if (!inherits(df[[1]], 'Date') && !inherits(df[[1]], 'POSIXct')) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  df <- as_tibble(df)%>%
    rename(TIME = 1)%>%
    select(c(1,TreeNum+1))%>%
    mutate(diff_dm = c(0, diff(.[[2]])))
  jl <- which(abs(df$diff_dm)>=v)
  if(length(jl)==0){
    message('There is no jump in the dendrometer.')
    df<-df%>%
      select(1:2)
    return(df)
  }else{
    jump.info<-df%>%
      slice(jl)%>%
      select(c(1,3))%>%
      rename(Jump_at = 1,
             Jump_by = 2)
    print(jump.info)
    plot_jump(df, jl, st="a", t=jump.info$Jump_at, title='Overview of jump(s)')
    data<-df[[2]]
    for(i in seq_along(jl)){
      fl<-jl[i]-20
      fm<-jl[i]+20
      print(fl)
      df_sl<-df%>%
        slice(fl:fm)%>%
        rename(dm=2)
      # Loop until a valid response is received
      user_input <- ''
      while(!user_input %in% c("yes", "no")) {
        user_input <- readline(prompt = paste("Do you want to move forward with jump adjustment at",jump.info$Jump_at[i], "(yes/no): "))
        user_input <- tolower(user_input)
        plot_jump(df=df_sl, jl=jl[i], st='a', t=jump.info$Jump_at[i], title=paste(jump.info$Jump_at[i], ', before adjustment'))
        if(user_input == "yes") {
          invisible(readline(prompt="Press [enter] to continue"))
          print(paste("Adjusting jump at",jump.info$Jump_at[i],"...."))
          data[jl[i]:length(data)] <- data[jl[i]:length(data)]-jump.info$Jump_by[i]
          df_sl$dm <- data[fl:fm]
          plot_jump(df=df_sl, jl=jl[i], st='b', t=jump.info$Jump_at[i], title=paste(jump.info$Jump_at[i], ', after adjustment'))
          invisible(readline(prompt="Press [enter] to continue"))
          print(paste("Adjusting jump at",jump.info$Jump_at[i],"...."))
        } else if(user_input == "no") {
          print(paste("Skipping adjusting jump at",jump.info$Jump_at[i],"...."))
          next
        } else {
          print("Invalid input. Please enter 'yes' or 'no'.")
          # Optionally, you might want to loop back and ask again until a valid response is received
        }
      }
    }
    df[[2]] = data
    df$diff_dm <-NULL
    plot_jump(df, jl, st="b", t=jump.info$Jump_at, title='Overview after adjustment of jump(s)')
    return(df)
  }
}
