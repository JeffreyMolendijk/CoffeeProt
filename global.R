# Libraries ----
suppressPackageStartupMessages({
#library(stringr)
#library(reshape2)
#library(AnnotationDbi)
#library(readxl)
#library(svglite)
#library(org.Hs.eg.db)
#library(org.Mm.eg.db)
#library(config)
#library(future)
#library(promises)
#library(js2graphic)
#library(shinyBS)
#library(fst)
#library(tidyr)
#library(RPostgres)
#library(DT)
#library(imputeLCMD)
#library(WGCNA)
library(ggrepel)
library(DBI)
library(circlize)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(data.table)
library(ggplot2)
library(patchwork)
library(shinydashboardPlus)
library(shinycssloaders)
library(networkD3)
})

# Set the color for the plot loading bars (from the shinycssloaders package)
options(spinner.color="#0dc5c1")

# Load database access credentials if config.yml is present
# Otherwise indicate that datawarehouse has not been loaded
if("config.yml" %in% list.files()){
  dw <- config::get("datawarehouse")
} else {
  dw <- list()
  dw$exists = FALSE
}

# Read example table for the Welcome page
demotable_q <- read.csv("data/demotable/demotable_qtl.csv", header = TRUE, check.names = FALSE)
demotable_p <- read.csv("data/demotable/demotable_protein.csv", header = TRUE, check.names = FALSE)
demotable_pheno <- read.csv("data/demotable/demotable_lqtl.csv", header = TRUE, check.names = FALSE)

# Read GWAS Catalog study table for users to select the study to download (molQTL tab)
gwas_catalog <- readRDS("data/gwas_catalog/gwascatalog_short.Rds") %>% as.data.frame()

# Read Variant effect - variant impact mapping table
ve_impact_mapping <- read.csv(file = "database/variant_effect_impact.csv", header = TRUE, stringsAsFactors = FALSE)

# Read Drug-Gene Interaction database
db_dgidb <- readRDS(file = "database/DGIdb_genename_drugname.Rds") %>% select(drug_name, gene_name, interaction_claim_source, interaction_types) %>% mutate(gene_name = tolower(gene_name))
db_dgidb_source <- db_dgidb %>% group_by(interaction_claim_source) %>% count() %>% arrange(interaction_claim_source) %>% mutate(dgi_source = paste0(interaction_claim_source, " (", n, ")"))