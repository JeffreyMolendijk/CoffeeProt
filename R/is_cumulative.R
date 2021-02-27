#' Test whether SNP or gene locations are cumulative per chromosome, or non-cumulative
#'
#' \code{cp_dl_table_csv} returns a downloadHandler in shiny.
#'
#' @param data The table to be downloaded. Must contain a chromosome and location column.
#' @param chr_col The gene or SNP chromosome column name, as a string
#' @param loc_col The gene or SNP location column name, as a string
#' 
#' @return A boolean, TRUE / FALSE indicating whether the data is cumulative
#'
#' @examples
#' cp_dl_table_csv(table, "chromosome_col", "location_col")
#' 
cp_is_cumulative <- function(data, chr_col, loc_col){
  
  ### TEST IF SNP_BP is cumulative
  x = data %>% group_by(!! rlang::sym(chr_col)) %>% summarise(min = min(as.numeric(!! rlang::sym(loc_col))), max = max(as.numeric(!! rlang::sym(loc_col)))) %>% arrange(min)
  c = outer(x$max, x$min, ">")
  d = outer(x$min, x$max, "<")
  is_cumulative <- x %>% mutate(Overlap = apply(c & d, 1, sum) > 1) %>% select(Overlap) %>% colSums() %>% as.vector() == 0
 
  return(is_cumulative)
  
}