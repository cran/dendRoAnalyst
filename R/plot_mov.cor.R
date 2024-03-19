#' @title Plotting moving correlation with climate.
#'
#' @description This function plots the moving correlation i.e., output of \code{mov.cor.dm}.
#'
#' @param mov.cor.output list the output of \code{mov.cor.dm} function.
#'
#' @param sig.only logical TRUE to plot only significant correlation.
#'
#' @param ci numeric confidence interval
#'
#' @param clim_vars array of climate variables or "all" for all climate variables.
#'
#' @return A plot with correlation.
#'
#' @examples
#' \donttest{
#' library(dendRoAnalyst)
#' data(gf_nepa17)
#' data(ktm_rain17)
#' out_corr<-mov.cor.dm(df=gf_nepa17, Clim=ktm_rain17, TreeNum=1, win_size=21)
#' plot_mov.cor(mov.cor.output=out_corr, sig.only=TRUE, ci=0.95)}
#'
#'
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when bind_rows
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text geom_raster scale_fill_gradient2 scale_x_date geom_tile
#' @importFrom tidyr pivot_longer
#'
#' @export
plot_mov.cor<-function(mov.cor.output, sig.only=T, ci=0.95, clim_vars='all'){
  if(!inherits(mov.cor.output, "mov.cor")){
    stop("The class of input must be 'mov.cor'")
  }
  para <- TIME <- corr <- NULL
  varnames <- names(mov.cor.output)
  if(!(clim_vars%in%c('all',varnames))){
    stop("'clim_vars' must be 'all' for all the climate variables or an array of name of climate variables as the names in input dataset")
  }
  for(vn in seq_along(varnames)){
    mov.cor.output[[varnames[vn]]]$para <- toupper(varnames[vn])
  }
  class(mov.cor.output) <- NULL
  corrs <- bind_rows(mov.cor.output)
  if (!inherits(corrs$TIME, "Date") && !inherits(corrs$TIME, "POSIXct")) {
    corrs$TIME <- ymd(corrs$TIME)
  }
  if(clim_vars=='all'){
    corrs <- corrs
  }else{
    corrs <- as_tibble(corrs)%>%filter(para%in%clim_vars)
  }

  p<-suppressWarnings(
    ggplot(data=corrs, aes(x=TIME, y=para, fill=corr))+
    geom_raster(aes(fill=corr))+
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                         limits = c(-1, 1), na.value = 'grey79') +
    labs(title=paste('Runnig correlation with climate'),
         x = "Time", y = "Climate variable", fill = "r") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_date(date_labels = "%b-%y", date_breaks = "1 month")
  )
  if(sig.only){
    ci <- 1-ci
    corrs2 <- na.exclude(corrs[corrs$p_val>=ci, ])
    p<-suppressWarnings(p+geom_tile(data = corrs2, aes(x = TIME, y = para), color = 'white', fill = NA, linewidth = 1))
  }
  return(p)
}
