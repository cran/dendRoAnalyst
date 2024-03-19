#' @title Reading dendrometer data.
#'
#' @description This function reads dendrometer data from .csv or .txt or .xlsx files. This function automatically recognizes the date time format of the first column of the data frame and changes it to "yyyy-mm-dd HH:MM:SS format".
#'
#' @param file string file name or path of the file.
#'
#' @param sep string the separator of the files. Only if they are different than the standard separators such as  tab for .txt file and comma for .csv file.
#'
#' @param dec the character used in the file for decimal points.
#'
#' @return A dataframe with the dendrometer data:
#'
#' @importFrom lubridate ymd_hms dmy_hms mdy_hms
#' @importFrom readxl read_excel
#' @importFrom utils read.table read.csv
#' @importFrom stringr str_count
#'
#' @export
read.dendrometer<-function(file, sep=NULL, dec=NULL){
  ###############################################
  recognize_datetime_format <- function(datetime_str) {
    # Check for common date and time formats and return the corresponding format string
    if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4} \\d{1,2}:\\d{1,2}:\\d{1,2}", datetime_str)) {
      return("%m/%d/%Y %H:%M:%S")  # Month/Day/Year Hours:Minutes:Seconds (24-hour clock)
    } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4} \\d{1,2}:\\d{1,2}", datetime_str)){
      return("%m/%d/%Y %H:%M")  # Month/Day/Year Hours:Minutes (24-hour clock)
    } else if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2} \\d{1,2}:\\d{1,2}:\\d{1,2}", datetime_str)) {
      return("%Y-%m-%d %H:%M:%S")  # Year-Month-Day Hours:Minutes:Seconds (24-hour clock)
    } else if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2} \\d{1,2}:\\d{1,2}", datetime_str)) {
      return("%Y-%m-%d %H:%M")  # Year-Month-Day Hours:Minutes:Seconds (24-hour clock)
    } else if (grepl("^\\d{4}.\\d{1,2}.\\d{1,2} \\d{1,2}:\\d{1,2}:\\d{1,2}", datetime_str)) {
      return("%Y.%m.%d %H:%M:%S")  # Year-Month-Day Hours:Minutes:Seconds (24-hour clock)
    } else if (grepl("^\\d{4}.\\d{1,2}.\\d{1,2} \\d{1,2}:\\d{1,2}", datetime_str)) {
      return("%Y.%m.%d %H:%M")  # Year-Month-Day Hours:Minutes:Seconds (24-hour clock)
    } else if (grepl("^\\d{1,2}.\\d{1,2}.\\d{4} \\d{1,2}:\\d{1,2}:\\d{1,2}", datetime_str)) {
      return("%d.%m.%Y %H:%M:%S")  # Year-Month-Day Hours:Minutes:Seconds (24-hour clock)
    } else if (grepl("^\\d{1,2}.\\d{1,2}.\\d{4} \\d{1,2}:\\d{1,2}", datetime_str)) {
      return("%d.%m.%Y %H:%M")  # Year-Month-Day Hours:Minutes:Seconds (24-hour clock)
    } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4} \\d{1,2}:\\d{1,2} [AP]M", datetime_str)) {
      return("%m/%d/%Y %I:%M %p")  # Month/Day/Year Hours:Minutes AM/PM (12-hour clock)
    } else if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2} \\d{1,2}:\\d{1,2} [AP]M", datetime_str)) {
      return("%Y-%m-%d %I:%M %p")  # Year-Month-Day Hours:Minutes AM/PM (12-hour clock)
    } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4} \\d{1,2}:\\d{1,2} [AP]M$", datetime_str)) {
      return("%m/%d/%Y %I:%M %p") # U.S. format with 12-hour clock
    } else if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4} \\d{1,2}:\\d{1,2}:\\d{1,2}$", datetime_str)) {
      return("%d-%m-%Y %H:%M:%S") # European format with day first and 24-hour clock, no seconds
    } else if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4} \\d{1,2}:\\d{1,2}$", datetime_str)) {
      return("%d-%m-%Y %H:%M") # European format with day first and 24-hour clock, no seconds
    } else if (grepl("^\\d{4}/\\d{1,2}/\\d{1,2} \\d{1,2}:\\d{1,2}:\\d{1,2}$", datetime_str)) {
      return("%Y/%m/%d %H:%M:%S") # ISO 8601 format with slashes
    } else if (grepl("^\\d{4}/\\d{1,2}/\\d{1,2} \\d{1,2}:\\d{1,2}$", datetime_str)) {
      return("%Y/%m/%d %H:%M") # ISO 8601 format with slashes and no seconds
    } else if (grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{4} \\d{1,2}:\\d{1,2}$", datetime_str)) {
      return("%d.%m.%Y %H:%M") # European format with dots and no seconds
    } else if (grepl("^\\d{1,2} \\d{1,2} \\d{4} \\d{1,2}:\\d{1,2}:\\d{1,2}$", datetime_str)) {
      return("%m %d %Y %H:%M:%S") # Space-separated format
    } else {
      return("Time format not recognized")
    }
  }
  ###############################################
  file2 <- basename(file)
  fname_split<-strsplit(file2, split = '\\.')[[1]]
  ext<-fname_split[2]
  #print(ext)
  if(!ext %in% c('csv','txt','xlsx')){
    stop("The file extension should be either 'csv', 'txt' or 'xlsx'.")
  }else{
    if(ext %in% c('csv','txt')){
      if (ext=='csv'){
        df<-read.csv(file, header=TRUE, sep = ifelse(is.null(sep),',',sep), dec = ifelse(is.null(dec),'.',dec))
      }else{df<-read.table(file, header=TRUE, sep = ifelse(is.null(sep), '\t',sep), dec = ifelse(is.null(dec),'.',dec))}
      #check the first datetime length to see if it starts at 00:00:00
      if (str_count(df[1,1])<11){
        c_count <- str_count(df[2,1])
      }else{
        c_count <- str_count(df[1,1])
      }
      ## Lets add %H:%M:%S to the time at midnight
      if (c_count <17){
        df[which(str_count(df[,1])<11), 1] <- paste(df[which(str_count(df[,1])<11), 1], '00:00')
      }else{
        df[which(str_count(df[,1])<11), 1] <- paste(df[which(str_count(df[,1])<11), 1], '00:00:00')
      }
      datetime_format<-sapply(df[c(1, nrow(df)/2, nrow(df)),1], recognize_datetime_format)
      datetime_format<- unique(datetime_format)
      if (length(datetime_format)>1){
        stop('The time format is not consistent')
      }else{
        message("The datetime format of dataset was '", datetime_format, "' and converted to '%Y-%m-%d %H:%M:%S'")
      }
      df[,1]<-as.POSIXct(df[,1], format = datetime_format, tz='UTC')

    }else{
      df <- readxl::read_excel(file)
    }
  }
  return(df)
}
