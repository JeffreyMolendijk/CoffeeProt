#' Create a SNP / Gene location plot
#'
#' \code{cp_circos_plot} returns a SNP / Gene location plot.
#'
#' @param qtldata A pQTL table.
#' 
#' @return A ggplot2 / patchwork plot visualizing SNP and gene locations.
#'
#' @examples
#' cp_circos_create(qtldata, c("gene1", "gene2","gene3"))
#' 
cp_donut_plot <- function(qtldata, type){
  
  qtlcolnames <- colnames(qtldata)
  
  if(type == "proxy"){
    col_select <- qtlcolnames[9]
    plotlabel <- "Proxy"
    
  } else if(type == "impact"){
    col_select <- "CP_Variant_Impact"
    plotlabel <- "Variant effect impact"
    
  } else {
    col_select <- "CP_Variant_Effect"
    plotlabel <- "Variant effect"
    
  }
  
  proxydonut <- qtldata %>% select(qtlcolnames[1], !! rlang::sym(col_select))
  
  proxydonut <- proxydonut %>% group_by(!! rlang::sym(col_select)) %>% summarise(count = n()) %>% arrange(-count) %>% 
    mutate(fraction = count / sum(count) * 100) %>% 
    mutate(ymax = cumsum(fraction)) %>% mutate(ymin = c(0, head(ymax, n=-1))) %>% 
    mutate(labpos = (ymax + ymin) / 2) %>% mutate(datatype = "data") 
  
  
  proxydonut = proxydonut %>% mutate(plotlabel = !! rlang::sym(col_select)) %>% mutate(pclabel = fraction)
  
  if((proxydonut$plotlabel %>% unique %>% length) >= 9){
    proxydonut = proxydonut %>% mutate(plotlabel = case_when(count < proxydonut[9, "count"] %>% as.numeric() ~ "rest", TRUE  ~ plotlabel)) %>% mutate(pclabel = case_when(count >= proxydonut[9, "count"] %>% as.numeric() ~ fraction, count == (proxydonut[floor(((nrow(proxydonut)  / 2))), "count"] %>% as.numeric()) ~ (100 - proxydonut[10, "ymax"] %>% as.numeric()), TRUE  ~ NA_real_))
  }
  
  proxydonut$pclabel <- tidyr::replace_na(proxydonut$pclabel, " ")
  
  plot_qtl_proxydonut <- ggplot(proxydonut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=plotlabel)) +
    geom_rect() +
    geom_text( x=2.5, aes(y=labpos, label=paste0((stringr::str_extract(pclabel, "^.{3}") %>% sub("\\.$","",.)),"%"), color=plotlabel), size=3) + 
    annotate("text", col = "gray",  x = -1, y = 0, vjust = 0.5, label = c(paste0("annotated: \n", nrow(qtldata) - (qtldata %>% select(!! rlang::sym(col_select)) %>% is.na() %>% sum), " \\ ", nrow(qtldata)))) +
    coord_polar(theta="y") +
    xlim(c(-1, 4))  + 
    scale_fill_discrete(name = plotlabel) +
    scale_color_discrete(name = plotlabel) + 
    theme_void()
  
  return(plot_qtl_proxydonut)
  
  
}