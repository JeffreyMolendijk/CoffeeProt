#' Download table as .csv file in shiny
#'
#' \code{cp_dl_table_csv} returns a downloadHandler in shiny.
#'
#' @param table The table to be downloaded.
#' @param filename A string to be used as the filename.
#' 
#' @return An exported table in .csv format.
#'
#' @examples
#' cp_dl_table_csv(table, "tablename.csv")
#' 
cp_dl_table_csv <- function(table, filename){
  downloadHandler(
    filename = function() { filename },
    content = function(file) {write.csv(table, file, row.names = FALSE)}
  )
}



#' Download table as .csv file, zip compressed in shiny
#'
#' \code{cp_dl_table_csv} returns a downloadHandler in shiny.
#'
#' @param table The table to be downloaded.
#' @param filename A string to be used as the filename.
#' 
#' @return An exported table in .csv format, zip compressed.
#'
#' @examples
#' cp_dl_table_csv(table, "tablename.csv")
#' 
cp_dl_table_zip <- function(table, filename){
  downloadHandler(
  filename = function() { paste0( filename, ".zip") },
  content <- function(file) {
    
    fs <- c()
    tmpdir <- tempdir()
    print(tmpdir)
    path <- paste0(tmpdir, "\\", filename, ".csv")
    fs <- c(fs, path)
    
    write.csv(table, path, row.names = FALSE)
    zip(zipfile=file, files=fs, flags = "-j")
    
    }
  )
  }



