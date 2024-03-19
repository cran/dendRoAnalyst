#' @title Plotting output of zero-growth approach (i.e., output of phase.zg function).
#'
#' @description This function plots the GRO and TWD of dendrometer data for a defined time period.
#'
#' @param ZG_output list the output of \code{phase.zg} function.
#'
#' @param DOY array with initial and final day for plotting. E.g. \emph{c(a,b)}, where a = initial date and b = final date.
#'
#' @param Year array for indicating year for plotting.
#'
#' @return A plot with GRO and TWD in two different subplots.
#'
#' @examples
#' \donttest{
#' library(dendRoAnalyst)
#' data(gf_nepa17)
#' zg.phase<-phase.zg(df=gf_nepa17, TreeNum=1)
#' plot_ZG_output(ZG_output=zg.phase,DOY=c(50,51), Year=2017)
#' }
#'
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when if_else arrange
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text scale_color_manual theme_bw scale_fill_manual
#' @importFrom tidyr pivot_longer
#'
#' @export
plot_ZG_output<-function(ZG_output, DOY, Year){
  if(!inherits(ZG_output ,"ZG_output")){
    stop("The input list's class must be 'ZG_output'.")
  }
  GRO <- TWD <- Variable <- Value <- TIME <- NULL
  td<-dendro.truncate(ZG_output[[2]], CalYear = Year, DOY = DOY)
  td<-td %>%
    rename(TIME = 1)
  data_long <- pivot_longer(td, c(GRO, TWD), names_to = "Variable", values_to = "Value")

  # Adjust TWD to be negative directly in the data_long dataframe
  data_long <- data_long %>%
    mutate(Value = if_else(Variable == "TWD", Value * -1, Value))

  # Ensure the data is ordered, especially after adding new rows
  data_long <- arrange(data_long, TIME)
  p<- suppressWarnings(ggplot(data_long, aes(x = TIME, y = Value)) +
    geom_line(data = filter(data_long, Variable == "GRO"), aes(color = Variable)) +
    geom_area(data = filter(data_long, Variable == "TWD"), aes(fill = Variable, group = 1), alpha = 0.5) +
    scale_fill_manual(values = c("GRO" = "blue", "TWD" = "red")) +
    scale_color_manual(values = c("GRO" = "blue", "TWD" = "red")) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
    labs(x = "Time", y = "Value") +
    theme_minimal() +
    theme(legend.position = "top")+
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)))
  return(p)
}
