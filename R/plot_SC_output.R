#' @title Plotting output of stem cycle approach (i.e., output of phase.sc function).
#'
#' @description This function plots the stem cycle of dendrometer data.
#'
#' @param SC_output list the output of \code{phase.sc} function.
#'
#' @param DOY array with initial and final day for plotting. E.g. \emph{c(a,b)}, where a = initial date and b = final date.
#'
#' @param Year array for indicating year for plotting.
#'
#' @param cols array of three elements: colour for each phases.
#'
#' @param phNames array with three elements for three different phases. Default is \strong{"Shrinkage", "Expansion" and "Increment"}.
#'
#' @return A plot with different phases.
#'
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text scale_color_manual theme_bw
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \donttest{
#' library(dendRoAnalyst)
#' data(gf_nepa17)
#' sc.phase<-phase.sc(df=gf_nepa17, TreeNum=1, smoothing=12)
#' plot_SC_output(SC_output=sc.phase,DOY=c(50,60), Year=2017)
#' }
#'
#' @importFrom ggplot2 ggplot
#'
#' @export
plot_SC_output<-function(SC_output, DOY, Year, cols=c('#fee8c8','#fdbb84','#e34a33'),phNames = c('Shrinkage','Expansion','Increment')){
  if(!inherits(SC_output ,"SC_output")){
    stop("The input list's class must be 'SC_output'.")
  }
  if(is.numeric(phNames)){
    stop("'phName' must be a character vector")
  }
  #############truncate data for specified periods#######################
  TIME <- dm <- Phases <- NULL
  td<-dendro.truncate(SC_output[[2]], CalYear = Year, DOY = DOY)
  td<-td %>%
    rename(TIME = 1,
           dm= 2,
           Phases = 3)
  td$Phases[td$Phases==1]<-phNames[1]
  td$Phases[td$Phases==2]<-phNames[2]
  td$Phases[td$Phases==3]<-phNames[3]
  ############# plotting the data #######################################
  p<-suppressWarnings(ggplot(td, aes(x = as.POSIXct(TIME), y = dm)) +
    geom_line(color='grey') +
    geom_point(aes(color=as.factor(Phases)), size = 3) +
    scale_color_manual(values = cols, na.value = "grey", name = 'Phases',
                       limits=phNames) +
    theme_minimal() +
    labs(title = "DM Data Over Time by Phase",
         x = "Time",
         y = "Stem size variation")+theme_bw()+
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)))
  return(p)
}
