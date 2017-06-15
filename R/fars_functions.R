# setwd("C:/Users/mmora/OneDrive/061 Coursera/spec_MasteringSoftwareDevInR/03_BuildingRPackages/w2_assignment")
# setwd("C:/Users/marco/OneDrive/061 Coursera/spec_MasteringSoftwareDevInR/03_BuildingRPackages/w4_package/farsdata")
# abc 2

#' Read in the "Fatality Analysis Reporting System" data file
#'
#' \code{fars_read} reads the data file from the "Fatality Analysis Reporting System.
#'
#' @param filename data file with csv format
#'
#' @return \code{fars_read} looks for the filename within the current path directory. The function stops, when
#' it doesn't find the specified file. If the file exists, the function loads the csv file and returns a data frame
#' (side note: it ensures that it is a data frame via a dplyr function)
#'
#' @examples
#' fars_read("./data/accident_2015.csv.bz2")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
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


#' Creates the string for a filename, according to a given year. Only valid for the "Fatality
#' Analysis Reporting System", respectively their files
#'
#' \code{make_filename} takes as input a year and gives out the name of the file as a string
#'
#' @param year input year, can be numeric or character
#'
#' @return \code{make_filename} gives you back the file name as a string
#'
#' @examples
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}





#' \code{fars_read_years} reads multiple files in, based on the data given by the "Fatality
#' Analysis Reporting System"
#'
#' @param years The years relating to the file names, to be read in
#'
#' @return \code{fars_read_years} will search for the file names based on the input \code{years}
#' For example, if \code{years} is specified as \code{2016:2017}, the function will search for
#' the following files:
#'   \itemize{
#'     \item "accident_2016.csv.bz2"
#'     \item "accident_2017.csv.bz2"
#'   }
#'   If the files exist: a list containing the requested data will be returned.
#'   If the files do not exist: an error will be returned stating the invalid year(s).
#'
#' @seealso \code{\link{make_filename}} for naming convention
#'
#' @examples
#' fars_read_years(2015:2016)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
fars_read_years <- function(years) {
        require(magrittr)

        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select_( ~MONTH, ~year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}



#' \code{fars_summarize_years} summarizes the data (given by the "Fatality Analysis Reporting System")
#' according to the years, which is being defined.
#'
#' @param years The years to be summarized
#'
#' @return \code{fars:summarize_years} gives the summarized data in a wide format
#'
#' @seealso \code{\link{fars_read_years}} to get more information
#'
#' @examples
#' fars_summarize_years(2015:2016)
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
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


#' \code{fars_map_state} will plot the accidents on a map via the maps package. As inputs we have
#' the state number and the year of interest
#'
#' @param state.num State number
#' @param years The year of interest
#'
#' @examples
#' fars_map_state(2, 2016)
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
