#' @title Resampling temporal resolution of dendrometer  and climate data
#'
#' @description This function is designed to change the temporal resolution of data. Depending on the objective, the user can define either maximum, minimum, or mean values to resample data in hourly, daily, weekly or monthly frequency.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS}.
#'
#' @param by either \emph{H, D, W} or \emph{M} to resample data into hourly, daily, weekly or monthly resolution.
#'
#' @param value either \emph{max, min}, \emph{mean} or \emph{sum} for the resampling value.
#'
#' @return Dataframe with resampled data.
#'
#' @examples
#' \donttest{library(dendRoAnalyst)
#' data(nepa17)
#' # To resample monthly with maximum value
#' resample_M<-dendro.resample(df=gf_nepa17, by='M', value='max')
#' head(resample_M,10)}
#'
#' @importFrom stats approx median na.exclude na.omit sd
#' @importFrom lubridate ymd_hms ymd
#' @importFrom dplyr mutate filter group_by summarise ungroup %>% rename across select where case_when everything
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 ggplot geom_area geom_rect geom_text aes theme_minimal labs geom_line geom_point facet_wrap theme element_text
#' @importFrom tidyr pivot_longer
#'
#'
#' @export
dendro.resample <- function(df, by, value) {
  TIME <- period <- NULL
  # Convert timestamp to POSIXct if not already
  # Convert the first column to datetime if not already
  if (!inherits(df[[1]], 'Date') && !inherits(df[[1]], 'POSIXct')) {
    df[[1]] <- ymd_hms(df[[1]])
  }
  df <- as_tibble(df)%>%
    rename(TIME = 1)
  # Determine the grouping variable based on 'by'
  period_col <- case_when(
    by == 'D' ~ as.Date(df$TIME),
    by == 'W' ~ floor_date(df$TIME, "week"),
    by == 'M' ~ floor_date(df$TIME, "month"),
    by == 'H' ~ df$TIME - minutes(minute(df$TIME)) - seconds(second(df$TIME)),
    TRUE ~ as.Date(df$TIME) # Fallback to daily if 'by' is unrecognized
  )

  # Create a new dataframe with the period column and the timestamp
  data <- df %>%
    mutate(period = period_col) %>%
    select(-TIME, everything()) # Move timestamp to the end if needed

  # List of circumference columns, assuming all except 'period'
  circ_columns <- setdiff(names(df[-1]), "TIME")

  # Apply the chosen summarization function to each circumference column
  summarization_fun <- match.fun(value)
  data_summarized <- data %>%
    group_by(period) %>%
    summarise(across(all_of(circ_columns), summarization_fun, .names = "{.col}"),
              .groups = 'drop')
  df <- data_summarized %>%
    rename(TIME = 1)
  return(df)
}
