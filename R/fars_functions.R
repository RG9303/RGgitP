#' Read the FARS file
#'
#' @description The function \code{fars_read} read a csv file if it exists and forwards the argument a data frame.
#'
#' @usage fars_read(filename)
#'
#' @param filename to enter a database with format csv.
#'
#' @return if file exists, this function read the file and return a database as a data frame. If the extension
#' is diferent to csv, it can not read the file.
#'
#' @source Data source: US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS),
#'         which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.
#'         For more information, please see \url{https://crashstats.nhtsa.dot.gov/#/DocumentTypeList/23}.
#'
#' @examples \dontrun{fars_read("accident_2013.csv.bz2")}
#'
#' @importFrom readr read_csv
#' @import dplyr
#'
#' @note To generate a file name, use \code{make_filename}.
#'
#' @seealso \code{\link{make_filename}}
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Create a filename
#'
#' @description The function transform a variable as a integer and print a character vector
#' containing a formatted combination of text and variable value.
#'
#' @usage make_filename(year)
#'
#' @param year as a variable to tranform it in an integer value.
#'
#' @return a character vector containing a formatted combination of text and variable value.
#'
#' @examples make_filename(2013)
#'
#' @note This function will only create a filename, to read in the file, use \code{fars_read}.
#'
#' @seealso \code{\link{fars_read}}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#' Read the FARS data with given year
#'
#' @description The function save the name of a specific data base according to a year,
#' read the ddatabase and transmute it drops existing variables as year.
#'
#' @usage fars_read_years(years)
#'
#' @param years as a variable to tranform it in an integer value. It will be used
#' when it generate the name of a file with the last function.
#'
#' @return if the year is in the set: 2013, 2014 or 2015, it will transmute a database in a new data frame depending
#' on a specific month. Otherwise it will print a warning.
#'
#' @source Data source: US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS),
#'         which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.
#'
#' @examples \dontrun{fars_read_years(2013)}
#' @examples \dontrun{fars_read_years(c(2013, 2014))}
#'
#' @import dplyr
#'
#' @seealso \code{\link{fars_read}}
#' @seealso \code{\link{make_filename}}
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}



#' Summarize FARS data by year
#'
#' @description The function transmute the data frame by group, summarize and spread a key-value pair
#' across the variables year and n.
#'
#' @usage fars_summarize_years(years)
#'
#' @param years as a variable to read the other functions and transmute the data frame.
#'
#' @return a data frame by group of year and month, and summarize by count. It will print the head of the database.
#'
#' @source Data source: US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS),
#'         which is a nationwide census providing the American public yearly data regarding fatal injuries suffered
#'         in motor vehicle traffic crashes.
#'
#' @examples \dontrun{fars_summarize_years(2013)}
#' @examples \dontrun{fars_summarize_years(c(2013, 2014))}
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Map displaying location of fatal injuries for selected state and year
#'
#' @description The functiontransform the principal database and add conditionals for some variables.
#' Plot a map with a specific lat and long.
#'
#' @usage fars_map_state(state.num, year)
#'
#' @param state.num as a variable that represent a state.
#' @param year as a variable to tranform it in an integer value.
#'
#' @return if number of a state is unique and it is contained in the variable STATE of the data
#' it will make a data frame with this, with conditionals to transform NAs in the
#' variables LONGITUD AND LATITUDE and print a map with this location. Otherwise print a
#' message "no accidents to plot" and return an invisible object.
#'
#' @source Data source: US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS),
#'         which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.
#'
#' @note State code:
#'   01: Alabama,
#'   02: Alaska,
#'   04: Arizona,
#'   05: Arkansas,
#'   06: California,
#'   08: Colorado,
#'   09: Connecticut,
#'   10: Delaware,
#'   11: District of Columbia,
#'   12: Florida,
#'   13: Georgia,
#'   15: Hawaii,
#'   16: Idaho,
#'   17: Illinois,
#'   18: Indiana,
#'   19: Iowa,
#'   20: Kansas,
#'   21: Kentucky,
#'   22: Louisiana,
#'   23: Maine,
#'   24: Maryland,
#'   25: Massachusetts,
#'   26: Michigan,
#'   27: Minnesota,
#'   28: Mississippi,
#'   29: Missouri,
#'   30: Montana,
#'   31: Nebraska,
#'   32: Nevada,
#'   33: New Hampshire,
#'   34: New Jersey,
#'   35: New Mexico,
#'   36: New York,
#'   37: North Carolina,
#'   38: North Dakota,
#'   39: Ohio,
#'   40: Oklahoma,
#'   41: Oregon,
#'   42: Pennsylvania,
#'   43: Puerto Rico,
#'   44: Rhode Island,
#'   45: South Carolina,
#'   46: South Dakota,
#'   47: Tennessee,
#'   48: Texas,
#'   49: Utah,
#'   50: Vermont,
#'   51: Virginia,
#'   52: Virgin Islands,
#'   53: Washington,
#'   54: West Virginia,
#'   55: Wisconsin,
#'   56: Wyoming.
#'
#' @examples \dontrun{fars_map_state(40, 2013)}
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @import dplyr
#'
#' @seealso \code{\link{fars_summarize_years}}
#' @seealso  \code{\link{make_filename}}
#' @seealso  \code{\link{fars_read}}
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
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
