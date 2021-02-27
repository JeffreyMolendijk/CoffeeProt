#' Import a table in .csv, .txt, .tsv, .xls or .xlsx file formats
#'
#' \code{cp_fileimport} automatically detects the file extension of the input file and performs the file reading accordingly.
#'
#' @param file The file to be loaded. Input should be a path to the file
#' 
#' @return A data frame.
#'
#' @examples
#' cp_fileimport("tablename.csv")
#' 
cp_fileimport <- function(file){

if(sub("^.*\\.","", file$datapath) == "csv"){
  tryCatch(
    {
      df <- read.csv(file$datapath, sep = ",", na.strings = c("NA", "Na", "NaN", "NAN", "na", "nan"))
    },
    error = function(e) {
      stop(safeError(e))
    }
  )
  
} else if(sub("^.*\\.","", file$datapath) == "txt" | sub("^.*\\.","", file$datapath) == "tsv") {
  tryCatch(
    {
      df <- read.csv(file$datapath, sep = "\t", na.strings = c("NA", "Na", "NaN", "NAN", "na", "nan"))
    },
    error = function(e) {
      stop(safeError(e))
    }
  )
} else if(sub("^.*\\.","", file$datapath) == "xls" | sub("^.*\\.","", file$datapath) == "xlsx") {
  tryCatch(
    {
      df <- readxl::read_excel(path = file$datapath, guess_max = 21474836, na = c("NA", "Na", "NaN", "NAN", "na", "nan"))
    },
    error = function(e) {
      stop(safeError(e))
    }
  )
}
  return(df)
}