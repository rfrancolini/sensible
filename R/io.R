#' retrieve example type hobotemp file name
#'
#' @export
#' @return filename
example_filename_temp <- function(){
  system.file("exampledata/little_drisko_hobo.csv",
              package="sensible")
}

#' retreive example type tiltometer file name
#'
#' @export
#' @return filename
example_filename_tilt <- function(){
  system.file("exampledata/2102053_LittleDrisko_TCM_Current.zip",
              package="sensible")
}

#' retrieve example type wavelogger directory path
#'
#' @export
#' @return character
example_filepath_wave <- function(){
  system.file("exampledata/2021_OWHL_LittleDris_Small.zip",
              package = "sensible")
}

#' retreive example type parXtreem file name
#'
#' @export
#' @return filename
example_filename_par <- function(){
  system.file("exampledata/Little_Drisko_PAR.csv",
              package="sensible")
}


#' clip hobotemp table by date
#'
#' @export
#' @param x tibble, hobotemp
#' @param startstop POSIXt vector of two values or NA, only used if clip = "user"
#' @return tibble
clip_hobotemp <- function(x,
                          startstop = NA) {

  if (is.na(startstop)[1]) {
     x <- x %>% dplyr::mutate (Date = as.Date(.data$DateTime, tz = "UTC"),
                              DateNum = as.numeric(.data$DateTime))

     ix <- which(diff(x$Date) != 0)[1]  + 1
     firstday <- as.numeric(difftime(x$DateTime[ix], x$DateTime[1]))

        if (firstday < 23) {
          x <- x[-(1:(ix-1)),]
        }

     iix <- dplyr::last(which(diff(x$Date) != 0))  + 1
     lastday <- as.numeric(difftime(dplyr::last(x$DateTime),x$DateTime[iix]))

        if (lastday < 23) {
          x <- x[-((iix+1):nrow(x)),]
        }

     x <- x %>% dplyr::select(-.data$Date, -.data$DateNum)
  }


  if (!is.na(startstop)[1]) {
    x <- x %>%
      dplyr::filter(.data$DateTime >= startstop[1]) %>%
      dplyr::filter(.data$DateTime <= startstop[2])
  }

  x
}


#' clip tiltometer table by date
#'
#' @export
#' @param x tibble, tiltometer
#' @param startstop POSIXt vector of two values or NA, only used if clip = "user"
#' @return tibble
clip_tiltometer <- function(x,
                            startstop = NA) {
  
  if (is.na(startstop)[1]) {
    x <- x %>% dplyr::mutate (Date = as.Date(.data$DateTime, tz = "UTC"),
                              DateNum = as.numeric(.data$DateTime))
    
    ix <- which(diff(x$Date) != 0)[1]  + 1
    firstday <- as.numeric(difftime(x$DateTime[ix], x$DateTime[1]))
    
    if (firstday < 23) {
      x <- x[-(1:(ix-1)),]
    }
    
    iix <- dplyr::last(which(diff(x$Date) != 0))  + 1
    lastday <- as.numeric(difftime(dplyr::last(x$DateTime),x$DateTime[iix]))
    
    if (lastday < 23) {
      x <- x[-((iix+1):nrow(x)),]
    }
    
    x <- x %>% dplyr::select(-.data$Date, -.data$DateNum)
  }
  
  
  if (!is.na(startstop)[1]) {
    x <- x %>%
      dplyr::filter(.data$DateTime >= startstop[1]) %>%
      dplyr::filter(.data$DateTime <= startstop[2])
  }
  
  x
}


#' clip wavelogger table by date
#'
#' @export
#' @param x tibble, waveloger
#' @param startstop POSIXt vector of two values in UTC or NA, only used if clip = "user"
#' @return tibble
clip_wavelogger <- function(x,
                            startstop = NA) {
  
  if (is.na(startstop)[1]) {
    x <- x %>% dplyr::mutate (Date = as.Date(.data$DateTime, tz = "UTC"),
                              DateNum = as.numeric(.data$DateTime))
    
    ix <- which(diff(x$Date) != 0)[1]  + 1
    firstday <- as.numeric(difftime(x$DateTime[ix], x$DateTime[1]))
    
    if (firstday < 23) {
      x <- x[-(1:(ix-1)),]
    }
    
    iix <- dplyr::last(which(diff(x$Date) != 0))  + 1
    lastday <- as.numeric(difftime(dplyr::last(x$DateTime),x$DateTime[iix]))
    
    if (lastday < 23) {
      x <- x[-((iix+1):nrow(x)),]
    }
    
    x <- x %>% dplyr::select(-.data$Date, -.data$DateNum)
  }
  
  
  if (!is.na(startstop)[1]) {
    x <- x %>%
      dplyr::filter(.data$DateTime >= startstop[1]) %>%
      dplyr::filter(.data$DateTime <= startstop[2])
  }
  
  x
}


#' clip parXtreem table by date
#'
#' @export
#' @param x tibble, parXtreem
#' @param startstop POSIXt vector of two values or NA, only used if clip = "user"
#' @return tibble
clip_parXtreem <- function(x,
                           startstop = NA) {
  
  if (is.na(startstop)[1]) {
    x <- x %>% dplyr::mutate (Date = as.Date(.data$DateTime, tz = "UTC"),
                              DateNum = as.numeric(.data$DateTime))
    
    ix <- which(diff(x$Date) != 0)[1]  + 1
    firstday <- as.numeric(difftime(x$DateTime[ix], x$DateTime[1]))
    
    if (firstday < 23) {
      x <- x[-(1:(ix-1)),]
    }
    
    iix <- dplyr::last(which(diff(x$Date) != 0))  + 1
    lastday <- as.numeric(difftime(dplyr::last(x$DateTime),x$DateTime[iix]))
    
    if (lastday < 23) {
      x <- x[-((iix+1):nrow(x)),]
    }
    
    x <- x %>% dplyr::select(-.data$Date, -.data$DateNum)
  }
  
  
  if (!is.na(startstop)[1]) {
    x <- x %>%
      dplyr::filter(.data$DateTime >= startstop[1]) %>%
      dplyr::filter(.data$DateTime <= startstop[2])
  }
  
  x
}

#' read hobotemp data file header
#'
#' @export
#' @param filename character, the name of the file
#' @param skip numeric, number of lines to skip - default 1
#' @return named list
read_hobo_cols <- function(filename = example_filename_temp(),
                                 skip = 1){

  x <- readLines(filename)[skip+2] %>%
   # stringr::str_split('("[^"]*),') %>%
    stringr::str_split(stringr::fixed(","), n = Inf) %>%
    `[[`(1)

  N <- length(x)
  #x <- x[length(x) >0]

  r = c("icnn", rep("-",N-4)) %>% paste(collapse = "")

  n = c("Reading", "DateTime", "Temp", "Intensity", LETTERS[seq_len(N-4)])

  r <- list(col_names = n, col_types = r)

  return(r)
}


#' read wavelogger data file
#'
#' @export
#' @param filepath character, the name of the directory - full path needed
#' @param clipped character, if auto, removed partial start/end days. if user, uses supplied startstop days. if none, does no date trimming
#' @param startstop POSIXt vector of two values or NA, only used if clip = "user"
#' @return tibble

# adapted from postprocessing workflow: http://owhl.org/post-processing-information/
read_wavelogger <- function(filepath = example_filepath_wave(),
                            clipped = c("auto", "user", "none")[1],
                            startstop = NA){
  
  stopifnot(inherits(filepath, "character"))
  #stopifnot(file.exists(filepath[1]))  Removed this - possible zipped file, check diff way?
  
  myTimeZone = "UTC" #default setting on owhl
  
  if (grepl(".zip", filepath) == FALSE) {
    
    filenames <- list.files(path=filepath, pattern = '*.csv', full.names=TRUE)
    x = owhlR::joinOWHLfiles(filenames, timezone = myTimeZone, verbose = FALSE)
    
  } else {
    
    filelist <- unzip(zipfile = filepath, list = TRUE)
    filenames <- as.vector(filelist$Name)
    
    tempd <- tempdir()
    unzip(filepath, exdir = tempd)
    filenames <- file.path(tempd, filenames)
    x = owhlR::joinOWHLfiles(filenames, timezone = myTimeZone, verbose = FALSE)
    unlink(tempd)
    
  }
  
  x <- switch(tolower(clipped[1]),
              "auto" = clip_wavelogger(x, startstop = NA),
              "user" = clip_wavelogger(x, startstop = startstop),
              "none" = x,
              stop("options for clipped are auto, user, or none. what is ", clipped, "?")
  )
  
  return(dplyr::as_tibble(x) %>% dplyr::select(-.data$POSIXt, -.data$frac.seconds))
  
}



#' retrieve example type air pressure data
#'
#' @export
#' @return character
example_airpressure <- function(){
  x <- read.csv(system.file("exampledata/KRKD_MesoWest_LittleDris.csv",
                            package = "sensible"))
  x <- na.omit(x)
  x$DateTime = as.POSIXct(x$DateTime, format = "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
  return(x)
}

#' retrieve air pressure data from mesowest database
#'
#' @export
#' @param api_key character, your api key for mesowest
#' @param wavelogger tibble, wavelogger data
#' @param output character, filename to save raw airpressure data
#' @return tibble

read_airpressure <- function(api_key = NA,
                             wavelogger = read_wavelogger(),
                             output = NA)
{
  stopifnot(inherits(api_key, "character"))
  suppressMessages(mesowest::requestToken(api_key))
  
  #use mesowest function to grab air pressure data
  #uses dates of interest based on wavelogger data
  
  starttime <- format(wavelogger$DateTime[1], "%Y%m%d%H%M")
  stoptime <- format(dplyr::last(wavelogger$DateTime), "%Y%m%d%H%M")
  
  meso <- mesowest::mw(service = "timeseries",
                       stid = "KRKD",
                       vars = "sea_level_pressure",
                       start = starttime,
                       end = stoptime,
                       units = "english",
                       jsonsimplify = TRUE)
  
  x <- data.frame(lapply(meso$STATION$OBSERVATIONS, unlist))
  
  x <- x %>%
    dplyr::select(-2) %>%
    dplyr::rename(DateTime = .data$date_time) %>%
    dplyr::rename(sea_pressure.mbar = .data$sea_level_pressure_set_1d)
  
  x$DateTime = as.POSIXct(x$DateTime, format = "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
  x <- na.omit(x)
  
  if (!is.na(output)) {
    readr::write_csv(x, file = output) }
  
  return(x)
  
}


#' interpolate air pressure to match owhl, calculate seawater pressure
#'
#' @export
#' @param wavelogger tibble, wavelogger data
#' @param airpressure tibble, airpressure data
#' @return tibble

interp_swpressure <- function(wavelogger = read_wavelogger(),
                              airpressure = read_airpressure())
{
  ix <- findInterval(wavelogger$DateTime, airpressure$DateTime)
  
  wavelogger <- wavelogger %>%
    dplyr::mutate(airpressure = airpressure$sea_pressure.mbar[ix],
                  swpressure = .data$Pressure.mbar - .data$airpressure)
  
  # Convert data to tsibbles
  #air <- tsibble::as_tsibble(airpressure, index = date)
  #wave <- tsibble::as_tsibble(wavelogger, index = DateTime)
  
  return(wavelogger)
}




#' convert pressure to sea surface elevation, correct for signal attenuation
#'
#' @export
#' @param wavelogger tibble, wavelogger data
#' @param latitude numeric, approx latitude of deployment - degrees north, default 44.5
#' @return tibble

mbar_to_elevation <- function(wavelogger = interp_swpressure(),
                              latitude = 44.5)
{
  
  wavelogger <- wavelogger %>%
    dplyr::mutate(swdepth = owhlR::millibarToSeawater(wavelogger$swpressure,
                                                      latitude = latitude),
                  swdepth = oceanwaves::prCorr(.data$swdepth,
                                               Fs = 4,
                                               zpt = 0.2))
  
  return(wavelogger)
}



#' convert pressure to sea surface elevation, correct for signal attenuation
#'
#' @export
#' @param wavelogger tibble, wavelogger data
#' @param burst numeric, time in minutes to calculate wave stats
#' @param site character, name of the site
#' @param output character, name of file to store file data in
#' @param ... other
#' @return tibble

wave_stats <- function(wavelogger = mbar_to_elevation(),
                       burst = 30,
                       site = NA,
                       output = NA,
                       ...)
{
  
  waves_spec <- owhlR::processBursts(Ht = wavelogger$swdepth,
                                     times = wavelogger$DateTime,
                                     burstLength = burst,
                                     Fs = 4,
                                     ...)
  
  if (!is.na(site)) {waves_spec <- waves_spec %>% dplyr::mutate(Site = site)}
  
  if (!is.na(output)) {
    readr::write_csv(waves_spec, file = output) }
  
  return(waves_spec)
}



#' read raw hobotemp data file, QA/QC it, write as a new file
#'
#' @export
#' @param filename character, the name of the file
#' @param output character, the name for the outputted QAQC file
#' @param site character, the name of the site, if NA the code will use filename without special character
#' @param clipped character, if auto, removed partial start/end days. if user, uses supplied startstop days. if none, does no date trimming
#' @param startstop POSIXt vector of two values or NA, only used if clip = "user"
#' @param skip numeric, number of rows to skip when reading, default 1
#' @return tibble
read_hobotemp <- function(filename = example_filename_temp(),
                          output = NA,
                          site = NA,
                          clipped = c("auto", "user", "none")[1],
                          startstop = NA,
                          skip = 1){
  stopifnot(inherits(filename, "character"))
  stopifnot(file.exists(filename[1]))

  columns <- read_hobo_cols(filename[1])

  x <- readr::read_csv(filename,
                       #col_names = columns[['col_names']],
                       col_types = columns[["col_types"]],
                       skip = skip,
                       quote = '"')

  colnames(x) <- columns[["col_names"]][1:4]

  #define site name to be filled in column
  if (!is.na(site)) {
    siteName <- site
  } else {
  #extract site name from first line of file
    siteName <- readLines(filename, 1) %>%
      stringr::str_extract_all("(?<=: ).+(?=\")") %>%
      `[[`(1)  %>%
      stringr::str_replace_all("[^[:alnum:]]", "")
  }

  #assign sitename to the column
  x <- x %>% dplyr::mutate(Site = siteName)

  #convert date/time to POSIXct format
  x$DateTime = as.POSIXct(x$DateTime, format = "%m/%d/%y %I:%M:%S %p", tz = "US/Eastern")

  #convert date/time to UTC
  x <- x %>% dplyr::mutate(DateTime = lubridate::with_tz(x$DateTime, tzone = "UTC"))

  x <- switch(tolower(clipped[1]),
              "auto" = clip_hobotemp(x, startstop = NA),
              "user" = clip_hobotemp(x, startstop = startstop),
              "none" = x,
              stop("options for clipped are auto, user, or none. what is ", clipped, "?")
              )

  #Remove na's
  x <- na.omit(x)

  if (!is.na(output)) {
  readr::write_csv(x, file = output) }

  return(x)

}


#' read tiltometer data file
#'
#' @export
#' @param filename character, the name of the file
#' @param site character, site being read in
#' @param output character, the name for the outputted QAQC file
#' @param clipped character, if auto, removed partial start/end days. if user, uses supplied startstop days. if none, does no date trimming
#' @param startstop POSIXt vector of two values in UTC or NA, only used if clip = "user"
#' @return tibble
read_tiltometer <- function(filename = example_filename_tilt(),
                            site = NA,
                            output = NA,
                            clipped = c("auto", "user", "none")[1],
                            startstop = NA){
  stopifnot(inherits(filename, "character"))
  stopifnot(file.exists(filename[1]))
  x <- suppressMessages(readr::read_csv(filename[1], locale = readr::locale(tz = "Etc/GMT-4")))
  #cleaning up the header
  h <- colnames(x)
  lut <- c("ISO 8601 Time" = "DateTime",
           "Speed (cm/s)" = "speed",
           "Heading (degrees)" = "dir",
           "Velocity-N (cm/s)" = "v",
           "Velocity-E (cm/s)" = "u")
  colnames(x) <- lut[h]
  #adapted from: https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
  attr(x, "units") <- stringr::str_extract(h, "(?<=\\().*?(?=\\))")
  attr(x, "filename") <- filename[1]
  #use the spec attr for original colnames
  #attr(x, "original_colnames") <- h
  
  x <- x %>% dplyr::mutate(DateTime = lubridate::with_tz(x$DateTime, tzone = "UTC"))
  
  
  x <- switch(tolower(clipped[1]),
              "auto" = clip_tiltometer(x, startstop = NA),
              "user" = clip_tiltometer(x, startstop = startstop),
              "none" = x,
              stop("options for clipped are auto, user, or none. what is ", clipped, "?")
  )
  
  if (!is.na(site)) {x <- x %>% dplyr::mutate(Site = site)}
  
  #omit NAs from data
  x <- na.omit(x)
  
  if (!is.na(output)) {
    readr::write_csv(x, file = output) }
  
  return(x)
  
}




#' read tiltometer temperature data file
#'
#' @export
#' @param filename character, the name of the file
#' @param site character, site being read in
#' @param output character, the name for the outputted QAQC file
#' @param clipped character, if auto, removed partial start/end days. if user, uses supplied startstop days. if none, does no date trimming
#' @param startstop POSIXt vector of two values in UTC or NA, only used if clip = "user"
#' @return tibble
read_tiltometer_temp <- function(filename = example_filename_tilt(),
                                 site = NA,
                                 output = NA,
                                 clipped = c("auto", "user", "none")[1],
                                 startstop = NA){
  stopifnot(inherits(filename, "character"))
  stopifnot(file.exists(filename[1]))
  x <- suppressMessages(readr::read_csv(filename[1], locale = readr::locale(tz = "Etc/GMT-4")))
  #cleaning up the header
  h <- colnames(x)
  lut <- c("ISO 8601 Time" = "DateTime",
           "Temperature (C)" = "Temp")
  colnames(x) <- lut[h]
  #adapted from: https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
  attr(x, "filename") <- filename[1]
  #use the spec attr for original colnames
  #attr(x, "original_colnames") <- h
  
  x <- x %>% dplyr::mutate(DateTime = lubridate::with_tz(x$DateTime, tzone = "UTC"))
  
  
  x <- switch(tolower(clipped[1]),
              "auto" = clip_tiltometer(x, startstop = NA),
              "user" = clip_tiltometer(x, startstop = startstop),
              "none" = x,
              stop("options for clipped are auto, user, or none. what is ", clipped, "?")
  )
  
  if (!is.na(site)) {x <- x %>% dplyr::mutate(Site = site)}
  
  #omit NAs from data
  x <- na.omit(x)
  
  if (!is.na(output)) {
    readr::write_csv(x, file = output) }
  
  return(x)
  
}



#' read parXtreem data file
#'
#' @export
#' @param filename character, the name of the file
#' @param site character, site being read in
#' @param output character, the name for the outputted QAQC file
#' @param clipped character, if auto, removed partial start/end days. if user, uses supplied startstop days. if none, does no date trimming
#' @param startstop POSIXt vector of two values or NA, only used if clip = "user"
#' @return tibble
read_parXtreem <- function(filename = example_filename_par(),
                           site = NA,
                           output = NA,
                           clipped = c("auto", "user", "none")[1],
                           startstop = NA){
  stopifnot(inherits(filename, "character"))
  stopifnot(file.exists(filename[1]))
  x <- suppressMessages(readr::read_csv(filename[1]))
  #cleaning up the header
  #PPFD = photosynthetic photon flux density
  h <- colnames(x)
  lut <- c("data2" = "Temp",
           "data1" = "PAR",
           "loggerUid" = "ID",
           "logDateTime" = "logDateTime",
           "dateTime" = "DateTime")
  colnames(x) <- lut[h]
  #adapted from: https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
  #attr(x, "units") <- stringr::str_extract(h, "(?<=\\().*?(?=\\))")
  #attr(x, "filename") <- filename[1]
  #use the spec attr for original colnames
  #attr(x, "original_colnames") <- h
  
  #convert date/time to POSIXct format
  x$DateTime = as.POSIXct(x$DateTime, format = "%d-%m-%Y %H:%M")
  
  
  x <- switch(tolower(clipped[1]),
              "auto" = clip_parXtreem(x, startstop = NA),
              "user" = clip_parXtreem(x, startstop = startstop),
              "none" = x,
              stop("options for clipped are auto, user, or none. what is ", clipped, "?")
  )
  
  if (!is.na(site)) {x <- x %>% dplyr::mutate(Site = site)}
  
  #omit NAs from data
  x <- na.omit(x)
  
  if (!is.na(output)) {
    readr::write_csv(x, file = output) }
  
  return(x)
  
}


#' print out summary of hobotemp data
#'
#' @export
#' @param x tibble, tibble of hobotemp data
#' @return tibble
summarize_hobotemp <- function(x = read_hobotemp()){

  #remove any NA's before summarizing

  x <- na.omit(x)

  s <- x %>% dplyr::group_by(.data$Site) %>%
             dplyr::summarise(mean.temp = mean(.data$Temp),
                              first.day = dplyr::first(.data$DateTime),
                              last.day = dplyr::last(.data$DateTime),
                              max.temp = max(.data$Temp),
                              max.temp.date = .data$DateTime[which.max(.data$Temp)],
                              min.temp = min(.data$Temp),
                              min.temp.date = .data$DateTime[which.min(.data$Temp)])

  return(s)
}




#' print out summary of tiltmeter data
#'
#' @export
#' @param x tibble, tibble of tiltmeter data
#' @return tibble
summarize_tiltometer<- function(x = read_tiltometer()){
  
  #remove any NA's before summarizing
  #remove all 0 PAR items when calculating mean
  
  x <- na.omit(x)
  
  s <- x %>% dplyr::group_by(.data$Site) %>%
    dplyr::summarise(mean.speed = mean(.data$speed),
                     first.day = dplyr::first(.data$DateTime),
                     last.day = dplyr::last(.data$DateTime),
                     max.speed = max(.data$speed),
                     max.speed.bearing = .data$dir[which.max(.data$speed)],
                     max.speed.date = .data$DateTime[which.max(.data$speed)])
  
  return(s)
}




#' print out summary of parXtreem data
#'
#' @export
#' @param x tibble, tibble of PAR data
#' @return tibble
summarize_parXtreem <- function(x = read_parXtreem()){
  
  #remove any NA's before summarizing
  #remove all 0 PAR items when calculating mean
  
  x <- na.omit(x)
  
  s <- x %>% dplyr::group_by(.data$Site) %>%
    dplyr::summarise(mean.par = mean(.data$PAR[.data$PAR!=0]),
                     first.day = dplyr::first(.data$DateTime),
                     last.day = dplyr::last(.data$DateTime),
                     max.PAR = max(.data$PAR),
                     max.PAR.date = .data$DateTime[which.max(.data$PAR)])
  
  return(s)
}



############# MODEL CODE ################

#' retreive example type sensor file name
#'
#' @export
#' @return filename
example_filename_sensor <- function(){
  system.file("exampledata/CapeSmallSensorHOBO.csv",
              package="sensible")
}

#' retreive example type sensor file name
#'
#' @export
#' @return filename
example_filename_sat <- function(){
  system.file("exampledata/CapeSmallSat.csv",
              package="sensible")
}

#' read in sensor and satellite data, generate dataset to create model off of
#' 
#' @export
#' @param sensordata character, the name of the file with sensor data
#' @param satellitedata character, the name of the file with satellite data
#' @param param character, the type of data being measured, default "temp"
#' @param output character, name for outputted dataset to be written as
#' @return dataframe for model creation

create_model_data <- function(sensordata = example_filename_sensor(),
                         satellitedata = example_filename_sat(),
                         param = "temp",
                         output = NA
                         ){
  
  stopifnot(inherits(sensordata, "character"))
  stopifnot(file.exists(sensordata[1]))
  
  stopifnot(inherits(satellitedata, "character"))
  stopifnot(file.exists(satellitedata[1]))
  
  #read in csv files provided
  sensordata <- readr::read_csv(sensordata[1])
  satdata <- readr::read_csv(satellitedata[1])
  
  #normalize csv column names as safeguard, add "sat" versus "sensor" column
  names(sensordata)[1] <- "DateTime"
  names(sensordata)[2] <- param
  
  names(satdata)[1] <- "DateTime"
  names(satdata)[2] <- param
  
  sensordata <- sensordata %>% dplyr::mutate(Method = "sensor")
  satdata <- satdata %>% dplyr::mutate(Method = "satellite")
  
  #convert date/time to POSIXct format in case its not
  sensordata$DateTime = as.POSIXct(sensordata$DateTime, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
  satdata$DateTime = as.POSIXct(satdata$DateTime, format = "%m/%d/%Y", tz = "UTC")
  
  #turn sensordata into daily measurements in case its not already
  second_col <- names(sensordata)[2]
  
  sensordata_daily <- sensordata %>%
    dplyr::mutate(DateTime = format(as.POSIXct(.data$DateTime, format = "%Y/%m/%d %H:%M:%S"),
                             format = "%m/%d/%Y")) %>%
    dplyr::group_by(.data$DateTime) %>%
    dplyr::summarise(!!param := mean(.data[[second_col]])) %>% 
    dplyr::mutate(Method = "sensor") %>%
    dplyr::ungroup()
  
  sensordata_daily$DateTime <- as.POSIXct(sensordata_daily$DateTime, format = "%m/%d/%Y", tz = "UTC")
  
  ##merge all SENSOR and SAT data together
  all_data_daily <- rbind(sensordata_daily, satdata)
  
  ###reformat data so temperature for each method is in different column
  x <- all_data_daily %>% tidyr::pivot_wider(names_from = .data$Method, 
                                      values_from = !!sym(param))
  
  if (!is.na(output)) {
    readr::write_csv(x, file = output) }
  
  return(x)
}


#' read in model dataset, generate model
#' 
#' @export
#' @param modeldata tibble, tibble of model data with columns DateTime, sensor, satellite
#' @param output character, output filename for RDS object to be saved as
#' @return R object of model

create_model <- function(modeldata = create_model_data(),
                              output = NA
                              ){
  
  modeldata.narm <- modeldata %>% stats::na.omit() #remove na
  modeldata.Train <- dplyr::sample_frac(modeldata.narm, size = 0.8) #create model based on 80% of data
  modeldata.Test <- setdiff(modeldata.narm, modeldata.Train) #test model on remaining 20% of data
  
  #Create model from train data, test on test data
  mod <- stats::lm(sensor ~ satellite, data = modeldata.Train)
  summary(mod)
  plot(mod)
  model.Prediction <- as.vector(stats::predict(mod, modeldata.Test))
  
  if (!is.na(output)) {
    saveRDS(mod, file = output) }
  
  return(mod) #return model
  
}


#' read in model, predict days of interest
#' 
#' @export
#' @param model model, generated by create_model
#' @param modeldata tibble, tibble of model data with columns DateTime, sensor, satellite
#' @param output character, output filename for final
#' @return dataframe of outputted model values alongside original input values

predict_data <- function(model = create_model(),
                         modeldata = create_model_data(),
                              output = NA
                              ){

  predicted <- as.vector(stats::predict(model, modeldata)) 
  alldata <- data.frame(modeldata, predicted)
  
  if (!is.na(output)) {
    readr::write_csv(alldata, file = output) }
  
  return(alldata)
  
}






