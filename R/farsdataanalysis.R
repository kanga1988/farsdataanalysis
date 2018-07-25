#' fars_read
#' This function reads data from the  US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System (FARS).
#' It proceed by reading the filename of the data povided as argument and return a data as a tibble.
#'
#'@param filename A character string pointing to the filename of the FARS data.
#' @return This function returns a tibble containing the FARS data. The function will stop if invalide filename is provided.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @note this package comes with three example files that can be accessed using
#' fullname <- system.file('extdata', 'accident_2013.csv.bz2', package = 'farsdataanalysis')
#' fars_read(filename = fullname)
#' @examples
#' \dontrun{
#' fars_read("accident_2015.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
  kanga <- system.file('extdata', 'accident_2015.csv.bz2',package = 'farsdataanalysis')
  filename <- kanga
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' make_filename.
#'
#' \code{make_filename} makes a file name by adding the given year as per  FARS filename standard
#'
#' This function takes a year as input argument and produces a valid FARS filename as output.
#' in this format: "accident_" + year + ".csb.bz2"
#'
#' @param year An integer,a string or a numeric data type that can be coerced to an integer
#'   But this function can be improve by adding argument validation test, that will check if the input argument
#'   can be coerce to integer. If yes continue with coercion. Else ,request the users to provide a valid input argument.
#'
#' @return this function returns a string that is the proper FARS data
#'    filename for the given year.  An erronous file name can be produce the param year was not validate.
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#' fars_read_years
#'
#' \code{fars_read_years} produces a list of tibbles of FARS data as per input vector of years.
#'
#' This function takes a vector of years and produces a list of tibbles,
#'   where each tibble is that year's FARS file year and MONTH observations.
#'   This function produce an output that retain only year and month from the original FARS dataset.
#'   To work, this function require a valid year as input argument. otherwise it will return a list with element value is NULL.
#'
#' @param years Vector of years' FARS files to open.
#'    Valid vector element will be integer,string or numeric that can be coerced to integer.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @return This function returns a list of tibbles with two variables : MONTH and year
#' Error handling using tryCatch to validate year input and return NULL for invalid year
#'
#' @examples
#' \dontrun{
#' fars_read_years(years = c(2013, 2014, 2015))
#' }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select_(~ MONTH, ~ year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' fars_summarize_years
#'
#' Produce a Summary of FARS Files: number of fatalities per mounth per year.
#'
#' \code{fars_summarize_years} produces a summary tibble of FARS years and
#'   months given a vector of years.
#'
#' This function takes a vector of years, extract the FARS data for the corresponding years.
#'   Bind the tibbles in the data list together. the function dplyr::bind_rows() allow to combine the elements (tibbles) in dat_list
#'   with differing number of variables together to create just one combined dataset.
#'   Produce the summary of combined dataset(tribble) showing the number of observations as per combination MONTH/year.
#'   This function require a valid input argument: i.e. a valid years vector as requiered for FARS data.
#'
#' @param years Vector of years' FARS files to open.
#'    Valid vector element will be integer,string or numeric that can be coerced to integer.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @return This function returns a summureze tibble in a wide form
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(years = c(2013,2014,2015))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_(~ year, ~ MONTH) %>%
    dplyr::summarize_(n = ~ n()) %>%
    tidyr::spread_('year', 'n')
}


#' fars_map_state
#' Map State Motor Vehicle Fatalities.
#'
#' \code{fars_map_state} maps state motor vehicle fatalities given a year and
#'   state id number.
#'
#' This function takes a state number and a year, and draws
#'   a state outline with dots to represent the location of motor vehicle
#'   fatalities for that year.  An error message will be displayed if an invalid
#'   state number is chosen or the chosen year's data does not exist.
#'
#'  It require to load maps library (which will load also all the dependacies:mapproj, mapdata, sp, maptools,rnaturalearth)
#'  It require to load also the graphics library with it's dependacies
#'
#' @param state.num is a numerical code for US state.
#' @param year  as an integer
#' @return plot of Selected US STATE with all the fatalities in the graphic window
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(12, 2014)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~ STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

