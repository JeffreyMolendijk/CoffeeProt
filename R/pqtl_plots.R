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
cp_pqtl_locplot <- function(qtldata){
  
  qtlcolnames <- colnames(qtldata)
  
  x = qtldata %>% mutate(gene_loc = ((!! rlang::sym(qtlcolnames[5]) + !! rlang::sym(qtlcolnames[6])) / 2))
  snp_labs = x %>% group_by(!! rlang::sym(qtlcolnames[3])) %>% summarise(meanchr = mean(!! rlang::sym(qtlcolnames[2]))) %>% as.data.frame()
  gene_labs = x %>% group_by(!! rlang::sym(qtlcolnames[7])) %>% summarise(meanchr = mean(gene_loc)) %>% as.data.frame()
  
  p1 = ggplot(x, aes(x = !! rlang::sym(qtlcolnames[2]), y = gene_loc)) + geom_bin2d(bins = 200) + theme(axis.text=element_text(size=6), axis.title=element_text(size=8),  legend.position = "none" ,panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "SNP location", y = "Gene location") + scale_fill_gradient(low="gray93", high="darkblue") + scale_x_continuous(breaks = snp_labs$meanchr, labels = snp_labs$snp_chr) + scale_y_continuous(breaks = gene_labs$meanchr, labels = gene_labs$gene_chr)
  
  p2 = ggplot(x, aes(x = !! rlang::sym(qtlcolnames[2]))) + stat_density(fill = "lightblue", alpha = 0.7, adjust = 0.3) + cowplot::theme_nothing() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "SNP location", y = "Gene location") + scale_fill_gradient(low="gray95",high="darkblue")
  
  p3 = ggplot(x, aes(y = gene_loc))+ stat_density(fill = "lightblue", alpha = 0.7, adjust = 0.3) + cowplot::theme_nothing() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "SNP location", y = "Gene location") + scale_fill_gradient(low="gray95",high="darkblue")
  
  plot_qtl_snploc <- p2 + plot_spacer() + p1 + p3 + plot_layout(widths = c(8, 1), heights = c(1, 8))
  
  return(plot_qtl_snploc)
  
}

#' Create a cp_circos object
#'
#' \code{cp_circos_create} returns a circos plot.
#'
#' @param qtldata A pQTL table.
#' @param circos_select Concatenated strings containing gene names to be plotted.
#' 
#' @return A cp_circos object, to be used in \code{cp_circos_plot}
#'
#' @examples
#' cp_circos_create(qtldata, c("gene1", "gene2","gene3"))
#' 
cp_circos_create <- function(qtldata, circos_select){
  
  qtlcolnames <- colnames(qtldata)
  
  #Create a table containing the start and end of each chromosome (in the RSID and Gene columns)
  chrlocs <- rbind(qtldata[,c(1,2,3)] %>% `colnames<-`(qtlcolnames[1:3]), 
                   qtldata[,c(1,5,7)] %>% `colnames<-`(qtlcolnames[1:3]), 
                   qtldata[,c(1,6,7)] %>% `colnames<-`(qtlcolnames[1:3]) )  
  
  chrlocs <- chrlocs %>% mutate(snp_chr = !! rlang::sym(qtlcolnames[3])) %>% group_by(snp_chr) %>% mutate(chrstart = as.numeric(min(!! rlang::sym(qtlcolnames[2]))), chrend = as.numeric(max(!! rlang::sym(qtlcolnames[2])))) %>% ungroup %>% distinct(snp_chr, chrstart, chrend) %>% arrange(snp_chr) %>% mutate(snp_chr = paste0("chr", as.character(snp_chr))) %>% mutate(gene_symbol = snp_chr)
  
  #A chromosome where the start and end are the same cause problems, must be removed.
  chrlocs <- chrlocs %>% filter(chrstart != chrend)
  
  #For normalizetodataframe to work a track needs to be initialized first
  circos.genomicInitialize(chrlocs, plotType = NULL)
  
  bed_genomesize <- chrlocs %>% group_by(snp_chr) %>% summarize(chr_len = max(chrend) - min(chrstart)) %>% select(chr_len) %>% colSums() %>% as.numeric()
  
  target <- circos_select
  
  
  #Use the circlize normalizetodataframe function before making links > seems to cause errors.
  bed <- qtldata %>% filter(!! rlang::sym(qtlcolnames[4]) %in% target) %>% mutate(gene_symbol = !! rlang::sym(qtlcolnames[4]), snp_chr = paste0("chr", as.character(!! rlang::sym(qtlcolnames[3]))), gene_chr = paste0("chr", as.character(!! rlang::sym(qtlcolnames[7]))), snp_bp_abs = !! rlang::sym(qtlcolnames[2]),  snp_bp_abs_end = !! rlang::sym(qtlcolnames[2]), pvalue = !! rlang::sym(qtlcolnames[8]), gene_start_abs = !! rlang::sym(qtlcolnames[5]), gene_end_abs = !! rlang::sym(qtlcolnames[6])) %>% select(snp_chr, snp_bp_abs, snp_bp_abs_end, gene_start_abs, gene_end_abs, pvalue, everything()) 
  bed <- bed %>% mutate(snp_bp_abs_end = (snp_bp_abs_end + (bed_genomesize / 1000)))
  bed <- bed %>% mutate(gene_end_abs = case_when(gene_end_abs - gene_start_abs < (bed_genomesize / 1000) ~  (gene_start_abs + (bed_genomesize / 1000)), TRUE ~ gene_end_abs) )
  bed <- bed %>% circlize:::validate_data_frame()
  bed <- bed %>% circlize:::normalizeToDataFrame()
  bed1 <- bed %>% select(snp_chr, snp_bp_abs, snp_bp_abs_end, pvalue) %>% mutate(gene_symbol = snp_chr)
  bed2 <- bed %>% select(gene_chr, gene_start_abs, gene_end_abs, pvalue, gene_symbol)
  
  #Create colors for plot, if there is only 1 unique pvalue make the color gray, else make gradient.
  if(bed$pvalue %>% unique() %>% length() < 2){
    linkcolors <- paste0("gray", "80")
  } else {
    ii <- cut(-log10(bed$pvalue), breaks = seq(min(-log10(bed$pvalue)), max(-log10(bed$pvalue)), len = 100), include.lowest = TRUE)
    linkcolors <- colorRampPalette(c("gray", "darkblue"))(99)[ii]
    linkcolors <- paste0(linkcolors, "80")
  }
  
  chrlabels <- bed2 %>% select(-pvalue) %>% `colnames<-`(colnames(chrlocs)) %>% distinct(snp_chr, chrstart, .keep_all = TRUE) %>% mutate(labelname = gene_symbol, labcol = "black") %>% bind_rows(chrlocs %>% mutate(chrstart = ((chrstart + chrend) / 2), chrend = chrstart, labelname = snp_chr, labcol = "darkgray"))
  
  cp_circos <- list()
  cp_circos$chrlocs <- chrlocs
  cp_circos$chrlabels <- chrlabels
  cp_circos$bed1 <- bed1
  cp_circos$bed2 <- bed2
  cp_circos$linkcolors <- linkcolors
  
  return(cp_circos)
  
}


#' Create a circos plot
#'
#' \code{cp_circos_plot} returns a circos plot.
#'
#' @param cp_circos A cp_circos object produced by \code{cp_circos_create}
#' 
#' @return A circos plot object visualizing the links between gene and SNP locations.
#'
#' @examples
#' cp_circos_plot(cp_circos)
#' 
cp_circos_plot <- function(cp_circos){

  circos.genomicInitialize(cp_circos$chrlocs, plotType = NULL)
  circos.genomicLabels(cp_circos$chrlabels, labels.column = 4, side = "outside", col = cp_circos$chrlabels$labcol)
  circos.track(ylim = c(0, 1), panel.fun = function(x, y) circos.genomicAxis(h ="top", labels = NULL, labels.col = "#FF000000"), bg.col = rand_color(nrow(cp_circos$chrlocs), transparency = 0.5), bg.border = NA, track.height = 0.05)
  circos.genomicRainfall(cp_circos$bed1, track.height = 0.1, pch = 16, cex = 0.4, col = c("#FF000080", "#0000FF80"))
  circos.genomicLink(cp_circos$bed1, cp_circos$bed2, col = cp_circos$linkcolors, border = NA, lwd = 1)

}
