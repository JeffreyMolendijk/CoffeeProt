# SERVER ----
server <- function(input, output, session) {
  
###################################################################################################################################################
#### Global options and setup
###################################################################################################################################################
  
  # Options to increase the upload filesize (900MB) and those the app when the session ends
  options(shiny.maxRequestSize = 900*1024^2)
  session$onSessionEnded(stopApp)

  # Load the Sweet Alert dependencies from shinyWidgets package
  useSweetAlert()
  
  # Create an object called "forout_reactive" to store reactive values
  forout_reactive <- reactiveValues()
  
  # Divert messages to the output for logging purposes
  if (!interactive()) sink(stderr(), type = "output")

#  # Periodically clear ram
#  observe({
#    # periodically collect, every 30 seconds?
#    invalidateLater(30000,session)
#    message("ram cleared")
#    gc()
#  })
  
  # Log message to identify the time CoffeeProt was accessed
  message(paste0("Status: CoffeeProt Version 1.0 (28/02/2021) is accessed at ", date()))
  

  # Generate the ticks that indicate whether the required datafiles have been loaded.
  # If a file is not loaded an X will be displayed, otherwise a tick mark
  output$tick_summary <- renderUI({
    tagList(
      if(is.null(forout_reactive$table_complex)){tags$div(HTML('<i class="fa fa-times" style = "color:#222d32;"></i> Protein/transcript data processed'))} else {tags$div(HTML('<i class="fa fa-check" style = "color:#222d32;"></i> Protein/transcript data processed'))},
      br())})
  
  output$tick_database <- renderUI({
    tagList(
      if(is.null(forout_reactive$table_complex)){tags$div(HTML('<i class="fa fa-times" style = "color:#222d32;"></i> Protein/transcript data processed'))} else {tags$div(HTML('<i class="fa fa-check" style = "color:#222d32;"></i> Protein/transcript data processed'))},
      br())})
  
  output$tick_network <- renderUI({
    tagList(
      if(is.null(forout_reactive$table_complex)){tags$div(HTML('<i class="fa fa-times" style = "color:#222d32;"></i> Protein/transcript data processed'))} else {tags$div(HTML('<i class="fa fa-check" style = "color:#222d32;"></i> Protein/transcript data processed'))},
      if(is.null(forout_reactive$table_qtl_processed)){tags$div(HTML('<i class="fa fa-times" style = "color:#222d32;"></i> pQTL/eQTL data processed (Optional)'))} else {tags$div(HTML('<i class="fa fa-check" style = "color:#222d32;"></i> pQTL/eQTL data processed'))},
      if(is.null(forout_reactive$table_pheno_processed)){tags$div(HTML('<i class="fa fa-times" style = "color:#222d32;"></i> GWAS/molQTL data processed (Optional)'))} else {tags$div(HTML('<i class="fa fa-check" style = "color:#222d32;"></i> GWAS/molQTL data processed'))},
      br())})
  
  output$tick_baitnetwork <- renderUI({
    tagList(
      if(is.null(forout_reactive$table_qtl_processed)){tags$div(HTML('<i class="fa fa-times" style = "color:#222d32;"></i> pQTL/eQTL data processed'))} else {tags$div(HTML('<i class="fa fa-check" style = "color:#222d32;"></i> pQTL/eQTL data processed'))},
      if(is.null(forout_reactive$table_pheno_processed)){tags$div(HTML('<i class="fa fa-times" style = "color:#222d32;"></i> GWAS/molQTL data processed'))} else {tags$div(HTML('<i class="fa fa-check" style = "color:#222d32;"></i> GWAS/molQTL data processed'))},
      br())})
  
  output$tick_qtlprot <- renderUI({
    tagList(
      if(is.null(forout_reactive$table_complex)){tags$div(HTML('<i class="fa fa-times" style = "color:#222d32;"></i> Protein/transcript data processed'))} else {tags$div(HTML('<i class="fa fa-check" style = "color:#222d32;"></i> Protein/transcript data processed'))},
      if(is.null(forout_reactive$table_qtl_processed)){tags$div(HTML('<i class="fa fa-times" style = "color:#222d32;"></i> pQTL/eQTL data processed'))} else {tags$div(HTML('<i class="fa fa-check" style = "color:#222d32;"></i> pQTL/eQTL data processed'))},
      br())})
  
  
  
  # disable the downdload button on page load
  shinyjs::disable("download_plot")
  shinyjs::disable("download_plot_zip")
  shinyjs::disable("download_table_zip")

  # Enable plot download if plots are present
  observe({
    if(length((c(names(forout_reactive) %>% grep("^plot_", ., value = TRUE) %>% sort() ))) > 0) {
      
      # enable the download button
      shinyjs::enable("download_plot")
      shinyjs::enable("download_plot_zip")
      
    }
  })
  
  # Enable table download if tables are present
  observe({
    if(length((c(names(forout_reactive) %>% grep("^table_", ., value = TRUE) %>% sort() ))) > 0) {

      # enable the download button
      shinyjs::enable("download_table_zip")

    }
  })
  
  # Disable download_circos, but enable if circos has been processed
  observe({
    if(length((c(names(forout_reactive) %>% grep("^cp_circos$", ., value = TRUE) %>% sort() ))) == 0) {

      # enable the download button
      shinyjs::disable("download_circos")
      
    } else if(length((c(names(forout_reactive) %>% grep("^cp_circos$", ., value = TRUE) %>% sort() ))) > 0) {
      shinyjs::enable("download_circos")
      
    }
  })
  

###################################################################################################################################################
#### Demo data tables, downloader and example data loading
###################################################################################################################################################
  
  # Demo tables ----
  # Rendered tables displayed on the Welcome page
  output$demotable_protein <- DT::renderDT({return(DT::datatable(read.csv("data/demotable/demotable_protein.csv", header = TRUE, check.names = FALSE), rownames = FALSE, class = "compact stripe", caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: Example proteomics table. ", "</b>","<em>","Protein data must be uploaded as a matrix in which the first column contains protein identifiers and the remaining columns contain quantitative measurements. The accepted protein identifiers are gene names, Uniprot identifiers and ENSEMBL gene identifiers.","</em>"))), options = list(scrollX = TRUE, pageLength = 10, dom = 't')))}, server = FALSE)
  output$demotable_qtl <- DT::renderDT({return(DT::datatable(read.csv("data/demotable/demotable_qtl.csv", header = TRUE, check.names = FALSE), rownames = FALSE, class = "compact stripe", caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: Example pQTL table. ", "</b>","<em>","For the QTL data the uploaded matrix should contain separate columns containing (1) rsIDs, (2) SNP location, (3) SNP chromosome, (4) gene names, (5) gene start location, (6) gene end location, (7) gene chromosome, (8) a measure of significance and (9) a proxy or grouping column.","</em>"))), options = list(scrollX = TRUE, pageLength = 10, dom = 't')))}, server = FALSE)
  output$demotable_pheno <- DT::renderDT({return(DT::datatable(read.csv("data/demotable/demotable_lqtl.csv", header = TRUE, check.names = FALSE), rownames = FALSE, class = "compact stripe", caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: Example phenotype table. ", "</b>","<em>","For the QTL data the uploaded matrix should contain separate columns containing (1) rsIDs, (2) gene names, (3) SNP location, (4) SNP chromosome, (5) a measure of significance and (6) a proxy or grouping column.","</em>"))), options = list(scrollX = TRUE, pageLength = 10, dom = 't')))}, server = FALSE)
  output$demotable_ld <- DT::renderDT({return(DT::datatable(read.csv("data/demotable/demotable_ld.csv", header = TRUE, check.names = FALSE), rownames = FALSE, class = "compact stripe", caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: Haplotype / LD block table. ", "</b>","<em>","For the haplotype data the uploaded matrix should contain separate columns containing (1) LD block chromosome, (2) LD block start position and (3) LD block end position. The start and end position format should match the format used in the pQTL/eQTL data","</em>"))), options = list(scrollX = TRUE, pageLength = 10, dom = 't')))}, server = FALSE)
  
  
  # Demoset text ----
  # Text description of the downloadable Parker study demo files, displayed on the Welcome page
  output$demoref <- renderUI({
    if(input$demoset == "parker"){
      HTML('
    <table cellspacing=5>
    <tr><td style="padding-right: 10px">Publication:</td><td><a href="https://doi.org/10.1038/s41586-019-0984-y" target="_blank">An integrative systems genetic analysis of mammalian lipid metabolism</a></td></tr>
    <tr><td style="padding-right: 10px">Number of samples:</td><td>306</td></tr>
    <tr><td style="padding-right: 10px">Number of proteins:</td><td>8,370</td></tr>
    <tr><td style="padding-right: 10px">Number of pQTLs</td><td>140,104 - 571,205</td></tr>
    <tr><td style="padding-right: 10px">Number of molQTLs</td><td>8,032</td></tr>
    <tr><td style="padding-right: 10px">Filesize:</td><td>12.7 MB</td></tr></table><br>')
    } })
  
  
  # Downloadbutton demoset ----
  # Downloadbutton for the zipped demo studies included in CoffeeProt (currently only Parker et al.)
  output$dl_demoset_ui <- renderUI({
    if(input$demoset != "")
      downloadButton("downloadData", label = "Download Demo Dataset")
  })
  
  
  # Download demo dataset handler ----
  # Downloadhandler for the zipped demo studies included in CoffeeProt (currently only Parker et al.)
  output$downloadData <- downloadHandler(
    filename <- function() {paste(input$demoset, "zip", sep=".")},
    
    content =function(file) {message("Action: User downloaded the demo dataset")
      file.copy(paste0("data/",input$demoset,".zip"), file)},
    contentType = "application/zip",
  )
  
  # Automated demo file loading ----
  # Load the Parker demofiles (from /data/parker.zip) on buttonpress (blue button)
  # Loaded files include the proteomics (protein.csv), pQTL (pQTL_1e-4_.csv) and lipidQTL (lQTL.csv) data
  observeEvent(input$demobutton_files, {
    
    sendSweetAlert(session = session, title = "Loading datafiles!", text = "Please be patient, this will just take a minute...", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
    
    # Protein data
    df <- read.csv(unz("./data/parker.zip", "protein.csv"))
    
    names(df)[1] <- "ID"
    
    forout_reactive$protdf <- df
    
    # pQTL data
    df <- read.csv(unz("./data/parker.zip", "pQTL_1e-4.csv"))
    
    names(df)[1] <- "ID"
    
    forout_reactive$qtlcolnames <- colnames(df)
    
    # Confirm that columns 2, 5, 6, 8 are numeric, integer or double columns.
    if((sapply(df, class)[2] %in% c("numeric", "integer", "double")) & (sapply(df, class)[5] %in% c("numeric", "integer", "double")) & (sapply(df, class)[6] %in% c("numeric", "integer", "double")) & (sapply(df, class)[8] %in% c("numeric", "integer", "double"))){
      
      forout_reactive$qtldf <- df
    } else {
      
      sendSweetAlert(session = session, title = "Processing problem.", text = "Please confirm that columns 2, 5, 6, 8 are numeric.", type = "error")
      forout_reactive$qtldf <- df
      
    }
    
    # molQTL data
    df <- read.csv(unz("./data/parker.zip", "lQTL.csv"))
    
    names(df)[1] <- "ID"
    
    forout_reactive$phenocolnames <- colnames(df)
    
    # Confirm that columns 3 and 5 are numeric, integer or double columns.
    if((sapply(df, class)[3] %in% c("numeric", "integer", "double")) & (sapply(df, class)[5] %in% c("numeric", "integer", "double"))){
      
      forout_reactive$phenodf <- df
    } else {
      
      sendSweetAlert(session = session, title = "Processing problem.", text = "Please confirm that columns 3 and 5 are numeric.", type = "error")
      forout_reactive$phenodf <- df
      
    }
    
    # haplotype data
    df <- read.csv(unz("./data/parker.zip", "haplotype_cumulative_mm10.csv"))
    
    #Detect whether column 10 (LD block) contains information
    if(ncol(df) == 3 &  df[,2] %>% class %in% c("numeric", "double", "integer") &  df[,3] %>% class %in% c("numeric", "double", "integer")){
      forout_reactive$haplotype_valid <- TRUE

      #Remove chromosome entries that start with chr* or chromosome* 
      df <- df %>% mutate(!! rlang::sym(colnames(df)[1]) := stringr::str_remove_all(!! rlang::sym(colnames(df)[1]), stringr::regex("chromosome_|chr_|chromosome-|chr-|chromosome |chr |chromosome|chr", ignore_case = TRUE)))
      df[[colnames(df)[1]]] <- factor(df[[colnames(df)[1]]], levels=c(1:22,"X","Y","MT")) %>% droplevels()
      
      df <- df %>% mutate(LD_name = paste0("LD_Chr_", !! rlang::sym(colnames(df)[1]), "_", !! rlang::sym(colnames(df)[2]), "_", !! rlang::sym(colnames(df)[3])))
      
    } else {
      forout_reactive$haplotype_valid <- FALSE
    }
    
    forout_reactive$haplotypedf <- df
    
    print(paste0("Valid haplotype data is present?", forout_reactive$haplotype_valid))
    
    print(head(forout_reactive$haplotypedf))
    
    # Notify user that demo data has been loaded
    sendSweetAlert(session = session, title = "Demo files loaded", html = TRUE, text = HTML("Proteomic, pQTL, haplotype and molQTL datasets have been loaded. Please proceed to process the loaded data, but ignore the fileuploaders. The data can be processed on the <code>Protein/transcript data</code>, <code>pQTL/eQTL data</code> and <code>GWAS/molQTL</code> tabs shown in the navigation bar on the left."), type = "success")
    message(paste0("Action: User loaded the demo datafiles"))
    
  }, ignoreInit = TRUE)
  
  
  
###################################################################################################################################################
#### Protein data processing
###################################################################################################################################################
  
  
  # Protein data - input - renderUI_correlation ----
  output$correlation_ui <- renderUI({
    req(forout_reactive$protanno)
    box(title = "Correlation Parameters", status = "primary", solidHeader = FALSE, width = 12, 
        HTML(paste0('<p align="justify">', "Please select a correlation and p-value adjustment method, followed by clicking the 'Correlate!' button. After performing the protein-protein or transcript-transcript correlation, CoffeeProt will check the presence of all correlated pairs in the CORUM, BioPlex and STRING databases. Next, all protein/transcript localizations are added to the correlation data to determine whether the correlated proteins/transcripts are known to be located in the same organelle. This process is expected to take around 2-3 minutes.", "<p>")), br(),
        selectInput("cor_type", label = ("Correlation / Co-regulation method"), choices = list("Biweight midcorrelation" = "bicor", "Pearson's correlation" = "pearson", "Spearman's correlation" = "spearman"), selected = 1),
        selectInput("fdr_type", label = ("Adjusted p-value method"), choices = list("Benjamini-Hochberg" = "BH", "Bonferroni" = "bonferroni"), selected = 1),
        actionButton(inputId = "correlate", label = "Correlate!"))  })
  
  
  # Protein data - input - renderUI_correlation_sensitivity ----
  output$sensitivity_ui <- renderUI({
    req(forout_reactive$table_complex)
    
    if(is.null(forout_reactive$plot_sensitivity) == TRUE){
      box(title = "Correlation sensitivity analysis", status = "primary", solidHeader = FALSE, width = 12, 
          HTML("A sensitivity analysis will indicate which p-value and correlation coefficient cut-offs affect the percentage of correlated pairs found in the reference databases. Clicking the 'Perform sensitivity analysis!' will perform the analysis on many combinations of correlation coefficients and q-values. <b>This analysis takes about 2-3 minutes</b>. When exporting sensitivity plots on the <code>Plot & Table export</code> tab, please select the Extra-Large and Extra-Wide dimensions to retain the correct aspect ratio and readable numbers."), br(), br(),
          actionButton(inputId = "sensitivity_analysis", label = "Perform sensitivity analysis!"))
    } else {
      box(title = "Correlation sensitivity analysis", status = "primary", solidHeader = FALSE, width = 12, 
          HTML("A sensitivity analysis will indicate which p-value and correlation coefficient cut-offs affect the percentage of correlated pairs found in the reference databases. Clicking the 'Perform sensitivity analysis!' will perform the analysis on many combinations of correlation coefficients and q-values. <b>This analysis takes about 2-3 minutes</b>. When exporting sensitivity plots on the <code>Plot & Table export</code> tab, please select the Extra-Large and Extra-Wide dimensions to retain the correct aspect ratio and readable numbers."), br(), br(),
          plotOutput("sensitivityplot"),
          HTML(paste("<b>", "Figure: Correlation sensitivity analysis. ", "</b>",
                     "<em>", "Combinations of correlation coefficients and q-values input parameters are tested to determine the effect on the correlated pair enrichment in the database analyses. 
        The colours and numbers indicate the percentage of correlated pairs found in that database. Plots are made for 3 databases, for the 'correlated' and 'non-correlated' pairs.", "</em>")), br(), br(),
          actionButton(inputId = "sensitivity_analysis", label = "Perform sensitivity analysis!"))
    }
  })
  
  
  # Protein data - input - renderUI_protfile ----
  output$protfile_ui <- renderUI({
    tagList(
    tags$div(title="Upload your protein/transcript data in .csv, .xls or .xlsx format", fileInput("protfile", "Choose protein/transcript file", multiple = FALSE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xls",".xlsx"))),
    
    #abab Code for manual annotations: currently being tested
    #tags$div(title="Upload an gene-annotation mapping file  in .csv, .xls or .xlsx format (optional)", fileInput("protfile_manual", "Choose gene-annotation mapping file (optional)", multiple = FALSE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xls",".xlsx"))),
    )
    })
  
  
  # Protein data - input - renderUI_protid ----
  output$prot_select_ui <- renderUI({
    req(forout_reactive$protdf)
    tagList(
      tags$div(title="Proteins with more missing values than the selected percentage are removed",
      sliderInput("protNA", "Percentage missing values allowed", 0, 100, 20)),
      actionButton(inputId = "protannotate", label = "Process proteins/transcripts!") )})
  
  
  # Protein data - fileinput ----
  observeEvent((input$protfile), {
    
    df <- cp_fileimport(input$protfile)
    
    names(df)[1] <- "ID"
    
    message(paste0("Action: User uploaded the protein file: ", input$protfile))
    message(paste0("This data has ", nrow(df), " rows, ", ncol(df), " cols"))
    
    forout_reactive$protdf <- df
    
  })
  
  
  # Protein data - annotation ----
  observeEvent(input$protannotate, {
    
    sendSweetAlert(session = session, title = "Commencing Processing!", text = "Please be patient, this will just take a minute...", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
    
    if(is.null(forout_reactive$protdf) == TRUE){
      sendSweetAlert(session = session, title = "Error! Please upload a valid protein file", type = "error")
    } else if((sum( sapply( forout_reactive$protdf[,2:ncol(forout_reactive$protdf)], class) == "numeric" ) ==  ncol(forout_reactive$protdf) - 1) == FALSE) { 
      sendSweetAlert(session = session, title = "Error! Please upload a valid protein file", text = "All columns except for the first should contain numeric data. Missing values could be a blank cell, or one of the following strings: 'NA', 'Na', 'NaN', 'NAN', 'na', 'nan'", type = "error")
      } else {
      
  
      # Load Human Protein Atlas, but edit to include ancestor localization terms for more accurate overlap
      db_hpa <- readRDS(file = "database/db_hpa.Rds")
      ancestors <- readxl::read_excel(path = 'database/localization_ancestors.xlsx' ) %>% 
        filter(!is.na(Ancestors)) %>% 
        mutate(term_ancestor = paste0(Term, ";", Ancestors))
      
      db_hpa$CP_loc <- db_hpa$`IF main protein location`
      
      for(i in 1:nrow(ancestors)){
        db_hpa$CP_loc <- db_hpa$CP_loc %>% sub(ancestors$Term[i], ancestors$term_ancestor[i], .)
      }
      
      db_hpa <- db_hpa %>% mutate(CP_loc = sapply(strsplit(db_hpa$CP_loc, ";"), function(x) paste(unique(x), collapse = ";"))) %>% select(-ENSG, -Uniprot, -`HyperLOPIT location`, -Reliability) %>% rename("HPA_IF_protein_location" = `IF main protein location`)

      db_hpa_locoverlap <<- db_hpa %>% select(3) %>% mutate(loc2 = CP_loc) %>% `colnames<-`(c("loc1", "loc2")) %>% distinct %>% tidyr::expand(loc1 = .$loc1, loc2 = .$loc2) %>% mutate(overlap.loc = mapply(function(x, y) paste(intersect(x, y), collapse=";"), strsplit(.$loc1, ";"), strsplit(.$loc2, ";")) ) 
      
      # Confirm that the protein data and protNA are present
      req(forout_reactive$protdf, input$protNA)

      # Save the number of values prior to filtering in a reactive value (for plotting purposes)
      forout_reactive$protnum <- nrow(forout_reactive$protdf)
      
      # Filtering missing values based on user input
      df <- forout_reactive$protdf %>% filter(rowSums(is.na(.)) <= (ncol(.) * (input$protNA/100))) %>% mutate(varID = paste0("x",1:nrow(.))) %>% select(varID, everything())
      names(df) <- gsub(" ", "_", names(df))
      
      forout_reactive$protfilter <- df
      
      # Converting ID's to gene names (GREPL finds UNIPROT IDs https://www.uniprot.org/help/accession_numbers)
      # The cp_idtype function detects the identifier type in the ID column
      df <- cp_idconvert(df, cp_idtype(df$ID))
      
      
      # Add drug-gene interaction database annotations by joining on gene names
      df <- left_join(df %>% mutate(ID = tolower(.$ID)), db_hpa, by = c("ID" = "Gene")) %>% mutate(inDGIdb = tolower(ID) %in% tolower(db_dgidb$gene_name)) %>% select(varID, ID, HPA_IF_protein_location:CP_loc, inDGIdb, everything())
      
      # If manual annotations have been uploaded, add them to df here... (Currently in development)
      if(isTruthy(input$protfile_manual)){
        print("manual anno file found")
        protanno_manual <- cp_fileimport(input$protfile_manual) %>% select(2,1) %>% `colnames<-`(c("manual_annotation", "ID")) %>% mutate(ID = tolower(.$ID))
        print(head(protanno_manual))
        
        # If manual annotation file contains IDs that are not genes, convert them.
        protanno_manual <- cp_idconvert(protanno_manual, cp_idtype(protanno_manual$ID))
        
        df <- left_join(df, protanno_manual) %>% select(varID, ID, HPA_IF_protein_location:CP_loc, inDGIdb, manual_annotation, everything())
        
      }
      
      # Save the resulting annotated data and report that the processing has been completed
      forout_reactive$protanno <- df

      message(paste0("Action: User annotated the protein data"))
      
      sendSweetAlert(session = session, title = "Annotation Success!", text = "Please proceed by performing protein correlation", type = "success")
    
      }})
  
  
  # Protein data - correlate ----
  observeEvent(input$correlate, {
    
    if(is.null(forout_reactive$protdf) == TRUE){
      sendSweetAlert(session = session, title = "Error!", text = "Please upload a valid protein file", type = "error")
    } else {
      
      req(forout_reactive$protdf)
      sendSweetAlert(session = session, title = "Running correlation (1/2)", text = "Please be patient, this will just take a few minutes...", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
      
      # Loading required databases
      db_string <- fst::read_fst("database/db_string_hs_100.fst")
      db_bioplex3 <- readRDS(file = "database/db_bioplex3.Rds")
      db_bioplex3_4pc <<- db_bioplex3 %>% filter(pInt >= quantile(x = db_bioplex3$pInt, 0.90))
      db_corum_gp <- readRDS(file = "database/db_corum_gp.Rds")
      
      # Impute missing values prior to performing correlation analyses
      df <- imputeLCMD::impute.MinDet(forout_reactive$protfilter %>% dplyr::select(3:ncol(.))) %>% `colnames<-`(paste0("X", colnames(.))) %>% `row.names<-`(forout_reactive$protfilter$varID)
      
      # Perform correlation based on the selected method (bicor, pearson, spearman)
      # Created a cor dataframe containing protein 1 and 2, correlation coefficient, p-value, q-value columns
      if(input$cor_type == "bicor"){
        
        message("Action: User selected: bicor")
        cor <- WGCNA::bicorAndPvalue(df %>% t)
        names(cor)[1] <- "cor"
        
        cor <- bind_cols(cor$cor %>% reshape2::melt() %>% as.data.table() %>% `colnames<-`(., c("p1", "p2", "cor")) %>% 
                           filter(as.character(p1) < as.character(p2)),
                         cor$p %>% reshape2::melt() %>% as.data.table() %>% `colnames<-`(., c("p1", "p2", "pval")) %>% 
                           filter(as.character(p1) < as.character(p2)) %>% select(pval)) %>% mutate(qval = p.adjust(p = .$pval, method = input$fdr_type))
        
      } else if(input$cor_type == "pearson") {
        message("Action: User selected: pearson")  
        cor <- WGCNA::corAndPvalue(df %>% t)
        
        cor <- bind_cols(cor$cor %>% reshape2::melt() %>% as.data.table() %>% `colnames<-`(., c("p1", "p2", "cor")) %>% 
                           filter(as.character(p1) < as.character(p2)),
                         cor$p %>% reshape2::melt() %>% as.data.table() %>% `colnames<-`(., c("p1", "p2", "pval")) %>% 
                           filter(as.character(p1) < as.character(p2)) %>% select(pval)) %>% mutate(qval = p.adjust(p = .$pval, method = input$fdr_type))
        
      } else if(input$cor_type == "spearman") {
        message("Action: User selected: spearman")  
        cor <- WGCNA::corAndPvalue(df %>% t, use = "pairwise.complete.obs", method = c("spearman"))
        
        cor <- bind_cols(cor$cor %>% reshape2::melt() %>% as.data.table() %>% `colnames<-`(., c("p1", "p2", "cor")) %>% 
                           filter(as.character(p1) < as.character(p2)),
                         cor$p %>% reshape2::melt() %>% as.data.table() %>% `colnames<-`(., c("p1", "p2", "pval")) %>% 
                           filter(as.character(p1) < as.character(p2)) %>% select(pval)) %>% mutate(qval = p.adjust(p = .$pval, method = input$fdr_type))
        
      } else {
        message("You have not selected bicor, pearson or spearman")  
        
      }
      
      # Editing cor into complex and paircount ----
      sendSweetAlert(session = session, title = "Annotating correlation results (2/2)", text = "Please be patient, this will just take a few minutes...", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
      
      
      # Join annotations onto the cor dataframe for the first and second protein per row
      cor <- cor %>% 
        left_join(., forout_reactive$protanno %>% dplyr::select(varID, ID) %>% `colnames<-`(., c("varID", "varID1")), by = c("p1" = "varID")) %>% 
        left_join(., forout_reactive$protanno %>% dplyr::select(varID, ID) %>% `colnames<-`(., c("varID", "varID2")), by = c("p2" = "varID")) %>% as.data.frame()
      
      
      # Create a concatenated Variable-Variable column (VarVar) indicating the two proteins that are correlated
      cor$VarVar <- paste(pmin(as.character(cor$varID1),as.character(cor$varID2)), pmax(as.character(cor$varID1),as.character(cor$varID2)), sep = "_")
      
      
      # Test database presence of the VarVar column in CORUM, BioPlex etc.
      complex <- cor %>% mutate(VarVar = tolower(VarVar)) %>% left_join(., db_corum_gp %>% mutate(Genepair = tolower(Genepair)) %>% dplyr::select(ComplexID:ComplexName), by = c("VarVar" = "Genepair")) %>% select(-p1, -p2) %>% mutate(inCORUM = is.na(ComplexID) == FALSE) %>% left_join(., db_bioplex3_4pc %>% mutate(VarVar = tolower(VarVar)) %>% dplyr::select(VarVar, pW), by = "VarVar") %>% mutate(inBioplex = is.na(pW) == FALSE) %>% left_join(., db_string) %>% distinct(VarVar, .keep_all = TRUE)
      
      
      # Check whether the two correlated proteins share a subcellular localization and report the overlap (slow computation)
      complex <- complex %>% left_join(., forout_reactive$protanno %>% dplyr::select(ID, CP_loc) %>% `colnames<-`(c("ID", "loc1")), by = c("varID1" = "ID")) %>% 
        left_join(., forout_reactive$protanno %>% dplyr::select(ID, CP_loc) %>% `colnames<-`(c("ID", "loc2")), by = c("varID2" = "ID")) 
      
      complex <- left_join(complex, db_hpa_locoverlap) %>% mutate(overlap.loc = tidyr::replace_na(overlap.loc, "")) %>% mutate(share.loc = (overlap.loc != ""))
      
      # Assigning results to reactive values and report progress to the user
      forout_reactive$table_complex <- complex
      updateProgressBar(session = session, id = "pb2", value = 100)
      
      
      
      sendSweetAlert(session = session, title = "Correlation Success!", text = "Please proceed to the QTL upload (optional) or Analysis tabs", type = "success")
      message("Status: Protein correlation success")  

    }})
  
  
  # renderUI for annotation gauge plot ----
  output$prot_annogauge_ui <- renderUI({
    req(forout_reactive$protanno)
    box(title = "", width = 12, plotOutput("anno_gauge") %>% withSpinner(),
        HTML(paste("<b>", "Figure: Protein/transcript filtering and annotation summary. ", "</b>",
                   "<em>", "(a.) Percentage of proteins/transcripts filtered by user-specified missing value cut-off. 
                   (b.) Percentage of proteins/transcripts annotated with Cell Atlas protein localization as determined by immunofluorescent staining.
                   (c.) Percentage of proteins/transcripts annotated DGIdb drug-gene interactions.", "</em>")))
  })  
  
  # renderUI for annotation tables ----
  output$cortable_ui <- renderUI({
    req(forout_reactive$table_complex)
    tagList(tabBox(title = "", width = 12,
                   tabPanel("Protein/transcript table (preview)", DT::DTOutput("protannotable"), downloadButton("download_protannotable","Download table")),
                   tabPanel("Correlation table (preview)", DT::DTOutput("cortable"), sliderInput("complextable_dl_filter", "Export p-value filter (log10 scale)", min = -10, max = 0, value = 0, step = 1), downloadButton("download_complextable","Download annotated correlation table"), HTML("<em><sup>* Currently has a 1.000.000 row limit</sup></em>")) ) )
      })  
  

###################################################################################################################################################
#### QTL data processing
###################################################################################################################################################

  # QTL data - input - renderUI_qtlfile ----
  output$qtl_file_ui <- renderUI({
    tagList(
    fileInput("qtlfile", "Choose pQTL/eQTL file", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv",".xls",".xlsx")),
    tags$div(title="Upload a haplotype mapping file  in .csv, .xls or .xlsx format (optional)", fileInput("haplotype", "Choose haplotype mapping file (optional)", multiple = FALSE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xls",".xlsx"))))})
  
  
  # Analysis - select_qtlfiltertype ----
  output$qtl_filtertype_ui <- renderUI({
    req(forout_reactive$qtldf)
    radioGroupButtons(inputId = "qtl_filtertype", label = "QTL filter type", choices = c('Single cut-off' = "single", 'Proxy cut-off' = "proxy"), selected = "single", justified = TRUE) })
  
  
  # Analysis - select_qtlfiltertype ----
  output$qtl_quanttype_ui <- renderUI({
    req(forout_reactive$qtldf)
    radioGroupButtons(inputId = "qtl_quanttype", label = "Significance data type", choices = c('p-value' = "pval", 'LOD' = "lod"), selected = "pval", justified = TRUE) })
  
  
  # Analysis - qtl_anno_ve ----
  output$qtl_anno_ve_ui <- renderUI({
    req(forout_reactive$qtldf)
    if(dw$exists){
      selectInput("qtl_anno_species", "Select species for variant effect annotation", choices = c("No annotation","Homo sapiens", "Mus musculus"), selected = 1, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
    } else {
      selectInput("qtl_anno_species", "Select species for variant effect annotation", choices = c("No annotation"), selected = 1, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
    }})
  
  
  # QTL data - input - qtlpvalUI ----
  output$qtl_pval_ui <- renderUI({
    req(input$qtl_filtertype, input$qtl_quanttype, is.numeric(forout_reactive$qtldf[[ rlang::sym(forout_reactive$qtlcolnames[8]) ]]))
    
    if(input$qtl_quanttype == "pval"){
      
      maxpval <- forout_reactive$qtldf[[ rlang::sym(forout_reactive$qtlcolnames[8]) ]] %>% -(log10(.)) %>% .[is.finite(.)] %>% max() %>% floor()

      if(input$qtl_filtertype == "single"){
        forout_reactive$proxylist_qtl <- forout_reactive$qtldf %>% select(!! rlang::sym(forout_reactive$qtlcolnames[9])) %>% unique %>% unlist()
        sliderInput("qtlpval", "p-value filter (log10 scale)", min = -maxpval, max = 0, value = 0, step = 1)
      } else {
        forout_reactive$proxylist_qtl <- forout_reactive$qtldf %>% select(!! rlang::sym(forout_reactive$qtlcolnames[9])) %>% unique %>% unlist()

        lapply(seq(forout_reactive$proxylist_qtl), function(i) {
          sliderInput(inputId = paste0("proxy_", forout_reactive$proxylist_qtl[i]), label = paste0("Filter p-value (log10 scale) by proxy: ", forout_reactive$proxylist_qtl[i]), min = -maxpval, max = 0, value = 0, step = 1)
        })
      } } else {
        
        maxlod <- forout_reactive$qtldf[[ rlang::sym(forout_reactive$qtlcolnames[8]) ]] %>% .[is.finite(.)] %>% max() %>% floor()
        minlod <- forout_reactive$qtldf[[ rlang::sym(forout_reactive$qtlcolnames[8]) ]] %>% .[is.finite(.)] %>% min() %>% floor()
        
        if(input$qtl_filtertype == "single"){
          forout_reactive$proxylist_qtl <- forout_reactive$qtldf %>% select(!! rlang::sym(forout_reactive$qtlcolnames[9])) %>% unique %>% unlist()
          sliderInput("qtlpval", "LOD filter", min = minlod, max = maxlod, value = 0, step = 1)
        } else {
          forout_reactive$proxylist_qtl <- forout_reactive$qtldf %>% select(!! rlang::sym(forout_reactive$qtlcolnames[9])) %>% unique %>% unlist()

          lapply(seq(forout_reactive$proxylist_qtl), function(i) {
            sliderInput(inputId = paste0("proxy_", forout_reactive$proxylist_qtl[i]), label = paste0("Filter LOD by proxy: ", forout_reactive$proxylist_qtl[i]), min = minlod, max = maxlod, value = 0, step = 1)
          }) } } })
  
  
  # QTL data - input - renderUI_proxyselect ----
  output$qtl_uploadfinish_ui <- renderUI({
    req(input$qtl_filtertype, forout_reactive$qtldf)
    actionButton(inputId = "qtl_final_upload", label = "Process pQTLs/eQTLs!") })
  
  
  # QTL data - fileinput ----
  observeEvent(input$qtlfile, {
    
    df <- cp_fileimport(input$qtlfile)
    
    names(df)[1] <- "ID"
    
    forout_reactive$qtlcolnames <- colnames(df)
    
    message(paste0("Action: User uploaded the pQTL file: ", input$qtlfile))
    message(paste0("This data has ", nrow(df), " rows, ", ncol(df), " cols, and has the following colnames:"))
    message(colnames(df))
    
    message(paste0("The columns are the following types: "))
    message(sapply(df, class))
    
    # Confirm that columns 2, 5, 6, 8 are numeric, integer or double columns.
    if((sapply(df, class)[2] %in% c("numeric", "integer", "double")) & (sapply(df, class)[5] %in% c("numeric", "integer", "double")) & (sapply(df, class)[6] %in% c("numeric", "integer", "double")) & (sapply(df, class)[8] %in% c("numeric", "integer", "double"))){
      
      # If there is no proxy data (e.g. all missing / NA) then paste the contents of that column
      if(df %>% select(!! rlang::sym(forout_reactive$qtlcolnames[9])) %>% unique %>% length < 2){
        print("proxy only has 1 group")
        df <- df %>% mutate(!! rlang::sym(forout_reactive$qtlcolnames[9]) := paste(!! rlang::sym(forout_reactive$qtlcolnames[9])))
      }
      
      forout_reactive$qtldf <- df
    } else {
      
      sendSweetAlert(session = session, title = "Processing problem.", text = "Please confirm that columns 2, 5, 6, 8 are numeric.", type = "error")
      forout_reactive$qtldf <- df
      
    }
  })
  
  
  # Haplotype data - processing ----
  observeEvent(input$haplotype, {
    
    df <- cp_fileimport(input$haplotype)
    
    #Detect whether column 10 (LD block) contains information
    if(ncol(df) == 3 &  df[,2] %>% class %in% c("numeric", "double", "integer") &  df[,3] %>% class %in% c("numeric", "double", "integer")){
      forout_reactive$haplotype_valid <- TRUE
      sendSweetAlert(session = session, title = "Haplotype uploaded", text = "File format appears to be valid.", type = "success")
      
      #Remove chromosome entries that start with chr* or chromosome* 
      df <- df %>% mutate(!! rlang::sym(colnames(df)[1]) := stringr::str_remove_all(!! rlang::sym(colnames(df)[1]), stringr::regex("chromosome_|chr_|chromosome-|chr-|chromosome |chr |chromosome|chr", ignore_case = TRUE)))
      df[[colnames(df)[1]]] <- factor(df[[colnames(df)[1]]], levels=c(1:22,"X","Y","MT")) %>% droplevels()
      
      df <- df %>% mutate(LD_name = paste0("LD_Chr_", !! rlang::sym(colnames(df)[1]), "_", !! rlang::sym(colnames(df)[2]), "_", !! rlang::sym(colnames(df)[3])))
      
    } else {
      forout_reactive$haplotype_valid <- FALSE
      sendSweetAlert(session = session, title = "Haplotype uploaded", text = "File format appears to be invalid.", type = "error")
    }
    
    forout_reactive$haplotypedf <- df

    print(paste0("Valid haplotype data is present?", forout_reactive$haplotype_valid))
    
    print(head(forout_reactive$haplotypedf))

  })
  
  
  # QTL data - processing ----
  observeEvent(input$qtl_final_upload, {
    
    req(forout_reactive$qtldf)
    
    if(ncol(forout_reactive$qtldf) == 9 & (sapply(forout_reactive$qtldf, class)[2] %in% c("numeric", "integer", "double")) & (sapply(forout_reactive$qtldf, class)[5] %in% c("numeric", "integer", "double")) & (sapply(forout_reactive$qtldf, class)[6] %in% c("numeric", "integer", "double")) & (sapply(forout_reactive$qtldf, class)[8] %in% c("numeric", "integer", "double"))){
      
    sendSweetAlert(session = session, title = "Commencing Processing!", text = "Please be patient, this will just take a minute...", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
      
    if(input$qtl_quanttype == "pval"){
      
      forout_reactive$qtl_quanttype <- "pval"
    
      if(input$qtl_filtertype == "single"){
      df <- forout_reactive$qtldf %>% select(forout_reactive$qtlcolnames[1], forout_reactive$qtlcolnames[2], forout_reactive$qtlcolnames[3], forout_reactive$qtlcolnames[4], forout_reactive$qtlcolnames[5], forout_reactive$qtlcolnames[6], forout_reactive$qtlcolnames[7], forout_reactive$qtlcolnames[8], forout_reactive$qtlcolnames[9]) %>% filter(!! rlang::sym(forout_reactive$qtlcolnames[8]) < 10^(input$qtlpval))
      } else {
      df <- forout_reactive$qtldf %>% select(forout_reactive$qtlcolnames[1], forout_reactive$qtlcolnames[2], forout_reactive$qtlcolnames[3], forout_reactive$qtlcolnames[4], forout_reactive$qtlcolnames[5], forout_reactive$qtlcolnames[6], forout_reactive$qtlcolnames[7], forout_reactive$qtlcolnames[8], forout_reactive$qtlcolnames[9])  
      for(i in 1:length(forout_reactive$proxylist_qtl)){
        df <- df %>% filter( !(!! rlang::sym(forout_reactive$qtlcolnames[9]) == forout_reactive$proxylist_qtl[i] & !! rlang::sym(forout_reactive$qtlcolnames[8]) > 10^(input[[paste0("proxy_", forout_reactive$proxylist_qtl[i])]])))
      }
      } } else {
        
        forout_reactive$qtl_quanttype <- "lod"
        
        if(input$qtl_filtertype == "single"){
          df <- forout_reactive$qtldf %>% select(forout_reactive$qtlcolnames[1], forout_reactive$qtlcolnames[2], forout_reactive$qtlcolnames[3], forout_reactive$qtlcolnames[4], forout_reactive$qtlcolnames[5], forout_reactive$qtlcolnames[6], forout_reactive$qtlcolnames[7], forout_reactive$qtlcolnames[8], forout_reactive$qtlcolnames[9]) %>% filter(!! rlang::sym(forout_reactive$qtlcolnames[8]) > input$qtlpval)
        } else {
          df <- forout_reactive$qtldf %>% select(forout_reactive$qtlcolnames[1], forout_reactive$qtlcolnames[2], forout_reactive$qtlcolnames[3], forout_reactive$qtlcolnames[4], forout_reactive$qtlcolnames[5], forout_reactive$qtlcolnames[6], forout_reactive$qtlcolnames[7], forout_reactive$qtlcolnames[8], forout_reactive$qtlcolnames[9])  
          for(i in 1:length(forout_reactive$proxylist_qtl)){
            df <- df %>% filter( !(!! rlang::sym(forout_reactive$qtlcolnames[9]) == forout_reactive$proxylist_qtl[i] & !! rlang::sym(forout_reactive$qtlcolnames[8]) < input[[paste0("proxy_", forout_reactive$proxylist_qtl[i])]]))
          }
        }         
        
      }
    
    # Pvalues of exactly zero are problematic when log transformed. Pvalue of zero are replaced with the lowest non-zero pvalue
    pvalrank <- df %>% select(!! rlang::sym(forout_reactive$qtlcolnames[8])) %>% distinct() %>% arrange(!! rlang::sym(forout_reactive$qtlcolnames[8])) %>% head()
    if(pvalrank[[1]][1] == 0){df <- df %>% mutate(!! rlang::sym(forout_reactive$qtlcolnames[8]) := case_when( (!! rlang::sym(forout_reactive$qtlcolnames[8]) == 0) ~ pvalrank[[1]][2], TRUE ~ !! rlang::sym(forout_reactive$qtlcolnames[8])))}
    
    # Determine is a gene is intragenic or not
    df <- df %>% mutate(CP_Intragenic_QTL = case_when( (!! rlang::sym(forout_reactive$qtlcolnames[2]) >= !! rlang::sym(forout_reactive$qtlcolnames[5]) & !! rlang::sym(forout_reactive$qtlcolnames[2]) <= !! rlang::sym(forout_reactive$qtlcolnames[6])) ~ "TRUE", TRUE ~ "FALSE"))
    
    # Remove chromosome entries that start with chr* or chromosome* 
    df <- df %>% mutate(!! rlang::sym(forout_reactive$qtlcolnames[3]) := stringr::str_remove_all(!! rlang::sym(forout_reactive$qtlcolnames[3]), stringr::regex("chromosome_|chr_|chromosome-|chr-|chromosome |chr |chromosome|chr", ignore_case = TRUE)))
    df <- df %>% mutate(!! rlang::sym(forout_reactive$qtlcolnames[7]) := stringr::str_remove_all(!! rlang::sym(forout_reactive$qtlcolnames[7]), stringr::regex("chromosome_|chr_|chromosome-|chr-|chromosome |chr |chromosome|chr", ignore_case = TRUE)))
    
    df[[forout_reactive$qtlcolnames[3]]] <- factor(df[[forout_reactive$qtlcolnames[3]]], levels=c(1:22,"X","Y","MT")) %>% droplevels()
    df[[forout_reactive$qtlcolnames[7]]] <- factor(df[[forout_reactive$qtlcolnames[7]]], levels=c(1:22,"X","Y","MT")) %>% droplevels()
    
    
    # Added code to process haplotype data if present...
    if(!isTruthy(forout_reactive$haplotypedf)){
      message("processing without haplotypes")
      forout_reactive$ld_processed <- FALSE
    } else if(isTruthy(forout_reactive$haplotypedf) & forout_reactive$haplotype_valid == TRUE){
      message("haplotype uploaded and valid")
      
      # Perform a fuzzy join with the haplotype data to add LD block information to the pQTL data
      df <- as.data.table(df)[as.data.table(forout_reactive$haplotypedf), on=c(paste0(colnames(df)[3],"==", colnames(forout_reactive$haplotypedf)[1]), paste0(colnames(df)[2],">", colnames(forout_reactive$haplotypedf)[2]), paste0(colnames(df)[2],"<",colnames(forout_reactive$haplotypedf)[3])), names(forout_reactive$haplotypedf)[4] := mget(paste0("i.", names(forout_reactive$haplotypedf)[4]))] %>% as.data.frame()
      
      print(head(df))
      
      forout_reactive$ld_processed <- TRUE
    } else {
      message("haplotype uploaded but invalid")
      forout_reactive$ld_processed <- FALSE
    }
    

    
    ### TEST IF SNP_BP and gene_bp are cumulative
    snploccumulative  <- cp_is_cumulative(df, forout_reactive$qtlcolnames[3], forout_reactive$qtlcolnames[2])
    geneloccumulative <- cp_is_cumulative(df, forout_reactive$qtlcolnames[7], forout_reactive$qtlcolnames[5])
    
    # Print message if the SNP and gene location data are in different formats (cumulative / non-cumulative)
    if(snploccumulative != geneloccumulative){
      message("Status: The SNP location and gene location columns are different (cumulative / non-cumulative)")}
    
    # Convert SNP and gene locations to cumulative positions if required
    if(snploccumulative == FALSE){
     message("Status: Your snp_loc and gene_loc are not cumulative")
      
      df_chrlen <- rbind(df[,c(1,2,3)] %>% `colnames<-`(forout_reactive$qtlcolnames[1:3]), 
                  df[,c(1,5,7)] %>% `colnames<-`(forout_reactive$qtlcolnames[1:3]), 
                  df[,c(1,6,7)] %>% `colnames<-`(forout_reactive$qtlcolnames[1:3]) ) %>% 
      
      # Compute chromosome size
      group_by(!! rlang::sym(forout_reactive$qtlcolnames[3])) %>% 
      summarise(chr_len=max(!! rlang::sym(forout_reactive$qtlcolnames[2]))) %>% 
      
      # Calculate cumulative position of each chromosome
      mutate(tot=cumsum(as.numeric(chr_len))-as.numeric(chr_len)) %>%
      select(-chr_len)
      
      # Add this info to the initial dataset
      df <- left_join(df, df_chrlen) %>%
      
      # Add a cumulative position of each SNP
      arrange(!! rlang::sym(forout_reactive$qtlcolnames[3]), !! rlang::sym(forout_reactive$qtlcolnames[2])) %>%
      mutate(!! rlang::sym(forout_reactive$qtlcolnames[2]) := (!! rlang::sym(forout_reactive$qtlcolnames[2])+tot)) %>% ungroup() %>% select(-tot)
        
      # Add this info to the initial dataset
      df <-  left_join(df, df_chrlen %>% `colnames<-`(c(forout_reactive$qtlcolnames[7], "tot"))) %>%
        
      # Add a cumulative position of each SNP
      arrange(!! rlang::sym(forout_reactive$qtlcolnames[7]), !! rlang::sym(forout_reactive$qtlcolnames[6])) %>%
      mutate(!! rlang::sym(forout_reactive$qtlcolnames[5]) := (!! rlang::sym(forout_reactive$qtlcolnames[5])+tot)) %>%
      mutate(!! rlang::sym(forout_reactive$qtlcolnames[6]) := (!! rlang::sym(forout_reactive$qtlcolnames[6])+tot)) %>% ungroup() %>% select(-tot)}
    
    # Reorder the SNP and gene column factors
    df[[forout_reactive$qtlcolnames[3]]] <- factor(df[[forout_reactive$qtlcolnames[3]]], levels=c(1:22,"X","Y","MT")) %>% droplevels()
    df[[forout_reactive$qtlcolnames[7]]] <- factor(df[[forout_reactive$qtlcolnames[7]]], levels=c(1:22,"X","Y","MT")) %>% droplevels()

    
    # CONVERTING ID TO GENE NAME
    if(cp_idtype(df %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unlist) == "uniprot"){
      
      uniKeys <- (AnnotationDbi::keys(org.Hs.eg.db::org.Hs.eg.db, keytype="SYMBOL")) %>%  c(., AnnotationDbi::keys(org.Mm.eg.db::org.Mm.eg.db, keytype="SYMBOL")) #Take all gene symbols from DB 
      Hs_g <- AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, keys=uniKeys, columns="UNIPROT", keytype="SYMBOL") %>% bind_rows(., AnnotationDbi::select(org.Mm.eg.db::org.Mm.eg.db, keys=uniKeys, columns="UNIPROT", keytype="SYMBOL"))
      
      df <- left_join(df, Hs_g, by = setNames("UNIPROT", forout_reactive$qtlcolnames[4]))
      df <- df %>% mutate(!! rlang::sym(forout_reactive$qtlcolnames[4]) := SYMBOL) %>% select(-SYMBOL) %>% filter(is.na(!! rlang::sym(forout_reactive$qtlcolnames[4])) == FALSE)

      message("Status: Converted Uniprot to Gene names")
    } else if(cp_idtype(df %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unlist) == "ensembl") {
      
      uniKeys <- (AnnotationDbi::keys(org.Hs.eg.db::org.Hs.eg.db, keytype="SYMBOL")) %>%  c(., AnnotationDbi::keys(org.Mm.eg.db::org.Mm.eg.db, keytype="SYMBOL")) #Take all gene symbols from DB 
      Hs_g <- AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, keys=uniKeys, columns="ENSEMBL", keytype="SYMBOL") %>% bind_rows(., AnnotationDbi::select(org.Mm.eg.db::org.Mm.eg.db, keys=uniKeys, columns="ENSEMBL", keytype="SYMBOL"))
      
      df <- left_join(df, Hs_g, by = setNames("ENSEMBL", forout_reactive$qtlcolnames[4]))
      df <- df %>% mutate(!! rlang::sym(forout_reactive$qtlcolnames[4]) := SYMBOL) %>% select(-SYMBOL) %>% filter(is.na(!! rlang::sym(forout_reactive$qtlcolnames[4])) == FALSE)
      
      message("Status: Converted Ensembl Gene to Gene names")
    } else {
      message("Status: No gene name conversion needed")
    }
    

    #### ANNOTATION FOR VARIANT EFFECTS
    if(input$qtl_anno_species == "No annotation"){
      forout_reactive$table_qtl_processed <- df 
      forout_reactive$qtl_annotated <- FALSE
      sendSweetAlert(session = session, title = "Processing Success!", text = paste0("QTLs remaining after filtering: ", nrow(df), "/", nrow(forout_reactive$qtldf), ". Upload other data types or proceed to the analysis tabs"), type = "success")
      
      
    } else if(input$qtl_anno_species == "Homo sapiens" & nrow(df) < 500000){
      sendSweetAlert(session = session, title = "Annotation Started!", text = "Please wait.", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
      message("Status: Selected Homo sapiens for annotation")
      q <- 'SELECT * FROM hs_unique WHERE rsid = ANY(VALUES %s )'
      
      query <- paste0("('", df %>%  select(!! rlang::sym(forout_reactive$qtlcolnames[1])) %>% unlist, "')")
      
      con <- DBI::dbConnect(RPostgres::Postgres(), user = dw$user, password = dw$password, dbname = dw$dbname, host = dw$host)
      
      start = Sys.time()
      res <- dbGetQuery(con, sprintf(q,paste(query,collapse = ",")))
      end = Sys.time()
      message(start - end)
      
      res <- df %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1])) %>% `colnames<-`(c("rsid")) %>% left_join(., res)

      
      # If no SNPs were annotated, proceed as if no annotation was performed
      if(is.na(res$ve) %>% sum() == nrow(df)){
        forout_reactive$table_qtl_processed <- df 
        forout_reactive$qtl_annotated <- FALSE
        
        sendSweetAlert(session = session, title = "Processing completed without annotation.", text = paste0("QTLs remaining after filtering: ", nrow(df), "/", nrow(forout_reactive$qtldf), ". No human SNPs were detected in the data."), type = "success")
      } else {
        
        df$CP_Variant_Effect <- res$ve %>% sub('^c\\(.', "",.) %>% sub('\"', "",.)
        df <- left_join(df, ve_impact_mapping %>% select(SO.term, CP_Variant_Impact), by = c("CP_Variant_Effect" = "SO.term"))
        
        forout_reactive$table_qtl_processed <- df
        forout_reactive$qtl_annotated <- TRUE
        
        sendSweetAlert(session = session, title = "Processing Success!", text = paste0("QTLs remaining after filtering: ", nrow(df), "/", nrow(forout_reactive$qtldf), ". Upload other data types or proceed to the analysis tabs"), type = "success")
      }
      
      
      
    } else if(input$qtl_anno_species == "Mus musculus" & nrow(df) < 500000){
      sendSweetAlert(session = session, title = "Annotation Started!", text = "Please wait.", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
      message("Status: Selected Mus musculus for annotation")
      q <- 'SELECT * FROM mm_unique_2 WHERE rsid = ANY(VALUES %s )'
      
      query <- paste0("('", df %>%  select(!! rlang::sym(forout_reactive$qtlcolnames[1])) %>% unlist, "')")
      
      con <- DBI::dbConnect(RPostgres::Postgres(), user = dw$user, password = dw$password, dbname = dw$dbname, host = dw$host)
      
      start = Sys.time()
      res <- dbGetQuery(con, sprintf(q,paste(query,collapse = ",")))
      end = Sys.time()
      message(start - end)
      
      res <- df %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1])) %>% `colnames<-`(c("rsid")) %>% left_join(., res)
      
      # If no SNPs were annotated, proceed as if no annotation was performed
      if(is.na(res$ve) %>% sum() == nrow(df)){
        forout_reactive$table_qtl_processed <- df 
        forout_reactive$qtl_annotated <- FALSE
        
        sendSweetAlert(session = session, title = "Processing completed without annotation.", text = paste0("QTLs remaining after filtering: ", nrow(df), "/", nrow(forout_reactive$qtldf), ". No mouse SNPs were detected in the data."), type = "success")
      } else {
        df$CP_Variant_Effect <- res$ve %>% sub('^c\\(.', "",.) %>% sub('\"', "",.)
        df <- left_join(df, ve_impact_mapping %>% select(SO.term, CP_Variant_Impact), by = c("CP_Variant_Effect" = "SO.term"))
        
        forout_reactive$table_qtl_processed <- df
        forout_reactive$qtl_annotated <- TRUE
        
        sendSweetAlert(session = session, title = "Processing Success!", text = paste0("QTLs remaining after filtering: ", nrow(df), "/", nrow(forout_reactive$qtldf), ". Upload other data types or proceed to the analysis tabs."), type = "success")
      }      
      
    } else {
      forout_reactive$table_qtl_processed <- df
      forout_reactive$qtl_annotated <- FALSE
      sendSweetAlert(session = session, title = "Processing completed without annotation!", text = paste0("QTLs remaining after filtering: ", nrow(df), "/", nrow(forout_reactive$qtldf), ". Data contained > 500,000 rows, please filter the data"), type = "info")
      
    }
    
      updateProgressBar(session = session, id = "pb3", value = 100)
      
    } else {
      
      sendSweetAlert(session = session, title = "Processing problem.", text = "Please confirm that the datafile contains 9 columns where columns 2, 5, 6 & 8 are numeric.", type = "error")
      
    }

    })
  

  # Sensitivity analysis
  observeEvent(input$sensitivity_analysis, {
    req(forout_reactive$table_complex)
    
    sendSweetAlert(session = session, title = "Commencing Processing!", text = "Please be patient, this will just take about 2 minutes...", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
    
    df_filter <- forout_reactive$table_complex
    
    # Define the ggplot theme used for the plots
    mytheme <- theme(axis.text=element_text(size=10), axis.title=element_text(size=12),  legend.position = "none" ,panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust=0.5), plot.tag=element_text(angle=-90), plot.tag.position="right")
    
    # these cut-offs needs to be looped over
    results <- list()
    corparam = c(rep(0, 9), rep(0.1, 9), rep(0.2, 9), rep(0.3, 9), rep(0.4, 9), rep(0.5, 9), rep(0.6, 9), rep(0.7, 9), rep(0.8, 9), rep(0.9, 9))
    qvalparam = c(rep(c(1, 1e-6, 1e-12, 1e-18, 1e-24, 1e-30, 1e-36, 1e-42, 1e-48), 10))
    
    for(i in 1:length(corparam)){
      
      df_filter <- df_filter %>% mutate(correlated = (cor > corparam[i] & qval < qvalparam[i]))
      
      results[[i]] <- list(corparam[i], qvalparam[i],
                           (df_filter %>% filter(correlated == TRUE & inCORUM == TRUE) %>% nrow() / df_filter %>% filter(correlated == TRUE & inCORUM == FALSE) %>% nrow()) * 100,
                           (df_filter %>% filter(correlated == FALSE & inCORUM == TRUE) %>% nrow() / df_filter %>% filter(correlated == FALSE & inCORUM == FALSE) %>% nrow()) * 100,
                           
                           (df_filter %>% filter(correlated == TRUE & inBioplex == TRUE) %>% nrow() / df_filter %>% filter(correlated == TRUE & inBioplex == FALSE) %>% nrow()) * 100,
                           (df_filter %>% filter(correlated == FALSE & inBioplex == TRUE) %>% nrow() / df_filter %>% filter(correlated == FALSE & inBioplex == FALSE) %>% nrow()) * 100,
                           
                           (df_filter %>% filter(correlated == TRUE & share.loc == TRUE) %>% nrow() / df_filter %>% filter(correlated == TRUE & share.loc == FALSE) %>% nrow()) * 100,
                           (df_filter %>% filter(correlated == FALSE & share.loc == TRUE) %>% nrow() / df_filter %>% filter(correlated == FALSE & share.loc == FALSE) %>% nrow()) * 100,
                           
                           df_filter %>% filter(correlated == TRUE) %>% nrow(),
                           df_filter %>% filter(correlated == FALSE) %>% nrow()
      )
      
    }
    
    df <- data.frame(matrix(unlist(results), nrow=length(results), byrow=T)) %>% `colnames<-`(c("cor", "qval", "CORUM_C", "CORUM_NC", "BioPlex_C", "BioPlex_NC", "shareloc_C", "shareloc_NC", "correlated_C", "correlated_NC"))
    
    # Generate the correlated (c) and not correlated (nc) plots for CORUM, BioPlex and shared locations
    corum_c <- ggplot(df, aes(x = cor, y = -log10(qval), fill = CORUM_C)) + geom_tile() + geom_text(aes(label = round(CORUM_C, 1)), size = 3) + mytheme + scale_x_continuous(expand = c(0, 0.01)) + scale_y_continuous(expand = c(0.01, 0.01)) + labs(x = "") + ggtitle(label = "% in CORUM")
    corum_nc <-ggplot(df, aes(x = cor, y = -log10(qval), fill = CORUM_NC)) + geom_tile() + geom_text(aes(label = round(CORUM_NC, 1)), size = 3) + theme(legend.position = "null") + mytheme + scale_x_continuous(expand = c(0, 0.01)) + scale_y_continuous(expand = c(0.01, 0.01))
    
    bioplex_c <-ggplot(df, aes(x = cor, y = -log10(qval), fill = BioPlex_C)) + geom_tile() + geom_text(aes(label = round(BioPlex_C, 1)), size = 3) + theme(legend.position = "null") + mytheme + scale_x_continuous(expand = c(0, 0.01)) + scale_y_continuous(expand = c(0.01, 0.01)) + labs(x = "", y = "") + ggtitle(label = "% in BioPlex")
    bioplex_nc <-ggplot(df, aes(x = cor, y = -log10(qval), fill = BioPlex_NC)) + geom_tile() + geom_text(aes(label = round(BioPlex_NC, 2)), size = 3) + theme(legend.position = "null") + mytheme + scale_x_continuous(expand = c(0, 0.01)) + scale_y_continuous(expand = c(0.01, 0.01)) + labs(y = "")
    
    shareloc_c <-ggplot(df, aes(x = cor, y = -log10(qval), fill = shareloc_C)) + geom_tile() + geom_text(aes(label = round(shareloc_C, 1)), size = 3) + theme(legend.position = "null") + mytheme + scale_x_continuous(expand = c(0, 0.01)) + scale_y_continuous(expand = c(0.01, 0.01)) + labs(x = "", y = "", tag = "Correlated \n") + ggtitle(label = "% shared localization")
    shareloc_nc <-ggplot(df, aes(x = cor, y = -log10(qval), fill = shareloc_NC)) + geom_tile() + geom_text(aes(label = round(shareloc_NC, 1)), size = 3) + theme(legend.position = "null") + mytheme + scale_x_continuous(expand = c(0, 0.01)) + scale_y_continuous(expand = c(0.01, 0.01)) + labs(y = "", tag = "Not correlated \n")
    
    correlated_c <-ggplot(df, aes(x = cor, y = -log10(qval), fill = log10(correlated_C))) + geom_tile() + geom_text(aes(label = formatC(correlated_C, format = "e", digits = 1)), size = 3) + theme(legend.position = "null") + mytheme + scale_x_continuous(expand = c(0, 0.01)) + scale_y_continuous(expand = c(0.01, 0.01)) + labs(x = "", y = "", tag = "Correlated \n") + ggtitle(label = "Number of proteins")
    correlated_nc <-ggplot(df, aes(x = cor, y = -log10(qval), fill = log10(correlated_NC))) + geom_tile() + geom_text(aes(label = formatC(correlated_NC, format = "e", digits = 1)), size = 3) + theme(legend.position = "null") + mytheme + scale_x_continuous(expand = c(0, 0.01)) + scale_y_continuous(expand = c(0.01, 0.01)) + labs(y = "", tag = "Not correlated \n")
    
    
    sendSweetAlert(session = session, title = "Plot completed!", text = "", type = "success")
    
    # Make a combined plot panel using patchwork
    forout_reactive$plot_sensitivity <- corum_c + bioplex_c + shareloc_c + correlated_c + corum_nc + bioplex_nc + shareloc_nc + correlated_nc + plot_layout(nrow = 2)
    
  })
  
  
  #Create circos on buttonclick ----
  observeEvent(input$circos_bttn, {
    req(forout_reactive$table_qtl_processed, input$circos_select)
    
    forout_reactive$cp_circos <- cp_circos_create(forout_reactive$table_qtl_processed, input$circos_select, forout_reactive$qtl_quanttype)
    
  })
  
  
  # Analysis - circos ----
  output$qtl_circos <- renderUI( {
    req(forout_reactive$cp_circos)
    
    s <- svglite::svgstring(standalone=FALSE)
    cp_circos_plot(forout_reactive$cp_circos)
    dev.off()
    
    forout_reactive$circosplot <- s()
    
    tagList(
    HTML(forout_reactive$circosplot),
    HTML(paste("<b>", "Figure: pQTL/eQTL Circos plot. ", "</b>", "<em>", "The pQTL/eQTL Circos plot visualizes the locations of SNPs affecting the user-specified protein targets. The outer labels show the protein targets (black) and chromosome labels (gray). The central dot plots indicate the density of SNPs at any location, where points closer to the center of the plot indicate a high density of SNPs. The edges between proteins and SNPs highlight the relative p-values among the QTLs where the lowest p-values are coloured blue, and the higher p-values are coloured gray.", "</em>"))
    )
  })  
  
  
  # Download circos ----
  output$download_circos <- downloadHandler(
    filename = function() { paste("Circos_", input$network_complexselect %>% unlist, ".svg", sep = "") },
    content = function(file) {
      
      gc()
      
      req(forout_reactive$cp_circos)
      
      svg(filename = file)
      cp_circos_plot(forout_reactive$cp_circos)
      dev.off()
      
    })
  
  
  # renderUI_circos ----
  output$qtl_circos_ui <- renderUI({
    req(forout_reactive$table_qtl_processed)
    box(title = "pQTL/eQTL Circos", width = 12, isolate(selectizeInput("circos_select", "Select Gene/Protein", choices = c(forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% arrange(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unique %>% as.vector()), selected = NULL, multiple = TRUE, options = list(maxItems = 10))),
        actionButton(inputId = "circos_bttn", label = "Make Circos!"), downloadButton("download_circos", "Download Circos"),
        uiOutput("qtl_circos") %>% withSpinner())
  }) 
  
  
  # renderUI_proxydonut ----
  # UI for QTL summary box on the pQTL/eQTL upload page
  output$qtl_proxydonut_ui <- renderUI({
    req(forout_reactive$table_qtl_processed)
    
    if(forout_reactive$qtl_annotated == FALSE){
      tagList(tabBox(title = "", width = 12,
                     tabPanel("Proxy annotation", plotOutput("qtl_proxy_donut") %>% withSpinner(), HTML(paste("<b>", "Figure: pQTL/eQTL annotation summary. ", "</b>", "<em>", "Percentage of QTLs annotated by proxy.", "</em>"))),
                     tabPanel("QTL location",  plotOutput("qtl_snploc") %>% withSpinner(), HTML(paste("<b>", "Figure: pQTL/eQTL genomic location. ", "</b>", "<em>", "The 2-dimensional density of genomic locations is shown in the main panel, where high density is shown in blue, and low density as grey. Separate density plots are produced for the SNP locations (x-axis) and gene locations (y-axis).", "</em>")))))
    } else {
      
      tagList(tabBox(title = "", width = 12,
                     tabPanel("Proxy annotation", plotOutput("qtl_proxy_donut") %>% withSpinner(), HTML(paste("<b>", "Figure: pQTL/eQTL annotation summary. ", "</b>", "<em>", "Percentage of QTLs annotated by proxy.", "</em>"))),
                     tabPanel("Impact annotation", plotOutput("qtl_impact_donut") %>% withSpinner(), HTML(paste("<b>", "Figure: pQTL/eQTL annotation summary. ", "</b>", "<em>", "Percentage of QTLs annotated by variant effect impact ratings", "</em>"))),
                     tabPanel("Variant effect annotation", plotOutput("qtl_anno_donut") %>% withSpinner(), HTML(paste("<b>", "Figure: pQTL/eQTL annotation summary. ", "</b>", "<em>", "Percentage of QTLs annotated by variant effects", "</em>"))),
                     tabPanel("QTL location",  plotOutput("qtl_snploc") %>% withSpinner(), HTML(paste("<b>", "Figure: pQTL/eQTL genomic location. ", "</b>", "<em>", "The 2-dimensional density of genomic locations is shown in the main panel, where high density is shown in blue, and low density as grey. Separate density plots are produced for the SNP locations (x-axis) and gene locations (y-axis).", "</em>")))) )  }
  })  
  
  
  # renderUI_qtl_tables ----
  # UI for QTL result tables
  output$qtl_tables_ui <- renderUI({
    req(input$qtl_final_upload)
    tagList(tabBox(title = "", width = 12,
                   tabPanel("p/eQTL table", DT::DTOutput("qtltable"), downloadButton("download_qtltable","Download table")),
                   tabPanel("p/eQTL gene tally", DT::DTOutput("qtltallytable"), downloadButton("download_qtltallytable","Download table")),
                   tabPanel("Intragenic/intergenic interactions", DT::DTOutput("intraintertable"), downloadButton("download_intraintertable","Download table"), downloadButton("download_intrainter_summarized_table","Download table (summarized)")) ) )
  })  
  

  

###################################################################################################################################################
#### Phenotype (molQTL/GWAS) data processing
###################################################################################################################################################
  
  # pheno data - input - renderUI_phenofile ----
  output$pheno_file_ui <- renderUI({
    fileInput("phenofile", "Choose molQTL file", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv",".xls",".xlsx")) })
  
  # Analysis - select_phenofiltertype ----
  output$pheno_filtertype_ui <- renderUI({
    req(forout_reactive$phenodf)
    radioGroupButtons(inputId = "pheno_filtertype", label = "pheno filter type", choices = c('Single cut-off' = "single", 'Proxy cut-off' = "proxy"), selected = "single", justified = TRUE) })
  
  # Analysis - select_phenofiltertype ----
  output$pheno_quanttype_ui <- renderUI({
    req(forout_reactive$phenodf)
    radioGroupButtons(inputId = "pheno_quanttype", label = "Significance data type", choices = c('p-value' = "pval", 'LOD' = "lod"), selected = "pval", justified = TRUE) })
  
  # pheno data - input - phenopvalUI ----
  output$pheno_pval_ui <- renderUI({
    req(input$pheno_filtertype, input$pheno_quanttype, is.numeric(forout_reactive$phenodf[[ rlang::sym(forout_reactive$phenocolnames[5]) ]]))
    
    if(input$pheno_quanttype == "pval"){
      
      # Define the maximum pvalue for the p-value input slider
      maxpval <- forout_reactive$phenodf[[ rlang::sym(forout_reactive$phenocolnames[5]) ]] %>% -(log10(.)) %>% .[is.finite(.)] %>% max() %>% floor()
      
      if(input$pheno_filtertype == "single"){
        forout_reactive$proxylist_pheno <- forout_reactive$phenodf %>% select(forout_reactive$phenocolnames[6]) %>% unique %>% unlist()
        sliderInput("phenopval", "p-value filter (log10 scale)", min = -maxpval, max = 0, value = 0, step = 1)
        
      } else {
        forout_reactive$proxylist_pheno <- forout_reactive$phenodf %>% select(forout_reactive$phenocolnames[6]) %>% unique %>% unlist()
        message(forout_reactive$proxylist_pheno)
        
        lapply(seq(forout_reactive$proxylist_pheno), function(i) {
          sliderInput(inputId = paste0("proxy_", forout_reactive$proxylist_pheno[i]), label = paste0("Filter p-value (log10 scale) by proxy: ", forout_reactive$proxylist_pheno[i]), min = -maxpval, max = 0, value = 0, step = 1)
        })
      } } else {
        
        maxlod <- forout_reactive$phenodf[[ rlang::sym(forout_reactive$phenocolnames[5]) ]] %>% .[is.finite(.)] %>% max() %>% floor()
        minlod <- forout_reactive$phenodf[[ rlang::sym(forout_reactive$phenocolnames[5]) ]] %>% .[is.finite(.)] %>% min() %>% floor()
        
        
        if(input$pheno_filtertype == "single"){
          forout_reactive$proxylist_pheno <- forout_reactive$phenodf %>% select(forout_reactive$phenocolnames[6]) %>% unique %>% unlist()
          sliderInput("phenopval", "LOD filter", min = minlod, max = maxlod, value = 0, step = 1)
        } else {
          forout_reactive$proxylist_pheno <- forout_reactive$phenodf %>% select(!! rlang::sym(forout_reactive$phenocolnames[6])) %>% unique %>% unlist()
          message(forout_reactive$proxylist_pheno)
          
          lapply(seq(forout_reactive$proxylist_pheno), function(i) {
            sliderInput(inputId = paste0("proxy_", forout_reactive$proxylist_pheno[i]), label = paste0("Filter LOD by proxy: ", forout_reactive$proxylist_pheno[i]), min = minlod, max = maxlod, value = 0, step = 1)
          }) } } })
  
  # pheno data - input - renderUI_proxyselect ----
  output$pheno_uploadfinish_ui <- renderUI({
    req(input$pheno_filtertype, forout_reactive$phenodf)
    actionButton(inputId = "pheno_final_upload", label = "Process molQTLs!") })
  
  
  # pheno data - fileinput ----
  observeEvent(input$phenofile, {
    
    df <- cp_fileimport(input$phenofile)
    
    names(df)[1] <- "ID"
    
    forout_reactive$phenocolnames <- colnames(df)
    
    message(paste0("Action: User uploaded the molQTL file: ", input$phenofile))
    message(paste0("This data has ", nrow(df), " rows, ", ncol(df), " cols, and has the following colnames:"))
    message(colnames(df))
    
    message(paste0("The columns are the following types: "))
    message(sapply(df, class))
    
    # Confirm that columns 2, 5, 6, 8 are numeric, integer or double columns.
    if((sapply(df, class)[3] %in% c("numeric", "integer", "double")) & (sapply(df, class)[5] %in% c("numeric", "integer", "double"))){
      
      forout_reactive$phenodf <- df
    } else {
      
      sendSweetAlert(session = session, title = "Processing problem.", text = "Please confirm that columns 3 and 5 are numeric.", type = "error")
      forout_reactive$phenodf <- df
      
    }
    
  })

  
  # pheno data - processing ----
  observeEvent(input$pheno_final_upload, {
    
    req(forout_reactive$phenodf)
    
    if(ncol(forout_reactive$phenodf) == 6 & (sapply(forout_reactive$phenodf, class)[3] %in% c("numeric", "integer", "double")) & (sapply(forout_reactive$phenodf, class)[5] %in% c("numeric", "integer", "double"))){
    
    sendSweetAlert(session = session, title = "Commencing Processing!", text = "Please be patient, this will just take a minute...", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
    

    if(input$pheno_quanttype == "pval"){
      
      if(input$pheno_filtertype == "single"){
        df <- forout_reactive$phenodf %>% select(forout_reactive$phenocolnames[1], forout_reactive$phenocolnames[2], forout_reactive$phenocolnames[3], forout_reactive$phenocolnames[4], forout_reactive$phenocolnames[5], forout_reactive$phenocolnames[6]) %>% filter(!! rlang::sym(forout_reactive$phenocolnames[5]) < 10^(input$phenopval))
      } else {
        df <- forout_reactive$phenodf %>% select(forout_reactive$phenocolnames[1], forout_reactive$phenocolnames[2], forout_reactive$phenocolnames[3], forout_reactive$phenocolnames[4], forout_reactive$phenocolnames[5], forout_reactive$phenocolnames[6])  
        for(i in 1:length(forout_reactive$proxylist_pheno)){
          df <- df %>% filter( !(!! rlang::sym(forout_reactive$phenocolnames[6]) == forout_reactive$proxylist_pheno[i] & !! rlang::sym(forout_reactive$phenocolnames[5]) > 10^(input[[paste0("proxy_", forout_reactive$proxylist_pheno[i])]])))
        }
      } } else {
        
        if(input$pheno_filtertype == "single"){
          df <- forout_reactive$phenodf %>% select(forout_reactive$phenocolnames[1], forout_reactive$phenocolnames[2], forout_reactive$phenocolnames[3], forout_reactive$phenocolnames[4], forout_reactive$phenocolnames[5], forout_reactive$phenocolnames[6]) %>% filter(!! rlang::sym(forout_reactive$phenocolnames[5]) > input$phenopval)
        } else {
          df <- forout_reactive$phenodf %>% select(forout_reactive$phenocolnames[1], forout_reactive$phenocolnames[2], forout_reactive$phenocolnames[3], forout_reactive$phenocolnames[4], forout_reactive$phenocolnames[5], forout_reactive$phenocolnames[6])  
          for(i in 1:length(forout_reactive$proxylist_pheno)){
            df <- df %>% filter( !(!! rlang::sym(forout_reactive$phenocolnames[6]) == forout_reactive$proxylist_pheno[i] & !! rlang::sym(forout_reactive$phenocolnames[5]) < input[[paste0("proxy_", forout_reactive$proxylist_pheno[i])]]))
          }
        }         
        
      }
    
    #Delete SNP_loc and SNP_chr rows where either is NA
    df <- df %>% filter(is.na(!! rlang::sym(forout_reactive$phenocolnames[3])) == FALSE | is.na(!! rlang::sym(forout_reactive$phenocolnames[3])) == FALSE)
    
    #Catch errors where all rows are removed
    if(nrow(df) == 0){sendSweetAlert(session = session, title = "Error, dataset contains 0 rows after filtering", text = "Please select less stringent cut-offs", type = "error")} 
    validate(need(nrow(df)!=0, "Error, dataset contains 0 rows after filtering. "))
    
    
    #Pvalues of exactly zero are problematic when log transformed. Pvalue of zero are replaced with the lowest non-zero pvalue
    pvalrank <- df %>% select(!! rlang::sym(forout_reactive$phenocolnames[5])) %>% distinct() %>% arrange(!! rlang::sym(forout_reactive$phenocolnames[5])) %>% head()
    if(pvalrank[[1]][1] == 0){df <- df %>% mutate(!! rlang::sym(forout_reactive$phenocolnames[5]) := case_when( (!! rlang::sym(forout_reactive$phenocolnames[5]) == 0) ~ pvalrank[[1]][2], TRUE ~ !! rlang::sym(forout_reactive$phenocolnames[5])))}
    
    #Remove chr* and chromosome* strings from chromosome column
    df <- df %>% mutate(!! rlang::sym(forout_reactive$phenocolnames[4]) := stringr::str_remove_all(!! rlang::sym(forout_reactive$phenocolnames[4]), stringr::regex("chrosomesome_|chr_|chr-|chromosome|chr", ignore_case = TRUE)))
    
    df[[forout_reactive$phenocolnames[4]]] <- factor(df[[forout_reactive$phenocolnames[4]]], levels=c(1:22,"X","Y","MT"))
    
 
      forout_reactive$table_pheno_processed <- df 
      forout_reactive$pheno_annotated <- FALSE
      sendSweetAlert(session = session, title = "Processing Success!", text = paste0("QTLs remaining after filtering: ", nrow(df), "/", nrow(forout_reactive$phenodf), ". Upload other data types or proceed to the analysis tabs"), type = "success")
   
      updateProgressBar(session = session, id = "pb4", value = 100)
    
    } else {
      
      sendSweetAlert(session = session, title = "Processing problem.", text = "Please confirm that the datafile contains 6 columns where columns 3 & 5 are numeric.", type = "error")

    }
    
  })
  
  
  # renderUI_proxydonut ----
  output$pheno_proxydonut_ui <- renderUI({
    req(forout_reactive$table_pheno_processed)
    box(title = "", width = 12, plotOutput("pheno_proxy_donut") %>% withSpinner(),
        HTML(paste("<b>", "Figure: molQTL annotation summary. ", "</b>",
                   "<em>", "Percentage of molQTLs annotated by the grouping variable.", "</em>")))
  })  
  
  
  # renderUI_phenotables ----
  output$pheno_tables_ui <- renderUI({
    req(input$pheno_final_upload)
    tagList(tabBox(title = "", width = 12,
                   tabPanel("molQTL table", DT::DTOutput("phenotable"), downloadButton("download_phenotable","Download table")),
                   tabPanel("molQTL gene tally", DT::DTOutput("phenotallytable"), downloadButton("download_phenotallytable","Download table")) ) )
  })  
  
  
  # pheno data metabolite annotator - input - renderUI_phenofile ----
  output$pheno_file_anno_ui <- renderUI({
    fileInput("phenofile_anno", "Choose file for annotation", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv",".xls",".xlsx")) })
  
  # pheno data metabolite annotator - input - renderUI_phenofile ----
  output$download_pheno_anno_metabolite <- downloadHandler(
    filename = function() { paste("molQTL_annotated.csv") },
    content = function(file) {
      
      gc()
      
      df <- cp_fileimport(input$phenofile_anno)
      phenocolnames <- colnames(df)
      
      db_chemrich = readxl::read_excel(path = "./database/chemrich_identifier_list.xlsx", skip = 2) %>% select(`name variant`, CID, `Class For ChemRICH`) %>% mutate(`name variant` = tolower(`name variant`)) %>% distinct()
      db_chemrich = left_join(db_chemrich %>% select(`name variant`, CID), db_chemrich[,2:3] %>% distinct() %>% aggregate(`Class For ChemRICH`~CID, data = . ,paste0,sep="")) %>% select(-CID) %>% distinct()
      
      names(db_chemrich)[1] <- phenocolnames[2]
      
      df <- fuzzyjoin::stringdist_left_join(df, db_chemrich, max_dist = 0)
      
      print(df)
      
      write.csv(x = df, file = file, row.names = FALSE)
      
    })
  

###################################################################################################################################################
#### RenderUI
###################################################################################################################################################
  

  # Analysis - select_proteinComplex - renderUI_protcomplex ----
  output$protqtlplot_ui1 <- renderUI({
    req(forout_reactive$table_complex, forout_reactive$table_qtl_processed, db_hpa_locoverlap)
    
    maxpval <- forout_reactive$table_complex[[ "pval" ]] %>% -(log10(.)) %>% .[is.finite(.)] %>% max() %>% floor()
    
      tagList(sliderInput("param_qtlprot_qval", "q-value cut-off (log10 scale)", min = -maxpval, max = 0, value = 0, step = 1),
              sliderInput(inputId = "param_qtlprot_cor", "Correlation cut-off", min = -1, max = 1, value = c(-1, 0.5), step = 0.01),
              selectInput("protqtlplot_chrselect", "Select Chromosome", (c("All Chromosomes", forout_reactive$table_qtl_processed %>% arrange(!! rlang::sym(forout_reactive$qtlcolnames[3])) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[3])) %>% unique %>% unlist %>% as.vector())), selected = "All Chromosomes", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
              selectInput("select_organelle_protqtl", "Select organelles to include", choices = c(db_hpa_locoverlap %>% distinct() %>% select(loc1) %>% unlist %>% unique() %>% strsplit(., ";") %>% unlist %>% unique %>% sort(., decreasing = FALSE, na.last = TRUE), NA), selected = c(db_hpa_locoverlap %>% distinct() %>% select(loc1) %>% unlist %>% unique() %>% strsplit(., ";") %>% unlist %>% unique %>% sort(., decreasing = TRUE, na.last = TRUE), NA), multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
              selectInput(inputId = "prottype", label = "Selecting protein source", choices = c('CORUM' = "corum", 'BioPlex 3.0' = "bioplex", "All proteins with QTL" = "protwqtl", "All / individual proteins" = "all"), selected = "CORUM", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
      
      )  
  })
  
  output$protqtlplot_ui2 <- renderUI({
    req(forout_reactive$table_complex, forout_reactive$table_qtl_processed, input$prottype)
    
    if(input$prottype == "corum"){
      
      if(forout_reactive$qtl_annotated == FALSE){
        tagList(selectInput("protqtlplot_complexselect", "Current selection", (c("All complexes", forout_reactive$table_complex %>% filter((cor > input$param_qtlprot_cor[2] | cor < input$param_qtlprot_cor[1]) & qval < 10^(input$param_qtlprot_qval) & inCORUM == TRUE) %>% group_by(ComplexID) %>% add_tally(name = "complexsize") %>% ungroup() %>% filter(complexsize >= 2) %>% arrange(-complexsize) %>% select(ComplexName) %>% unique %>% unlist %>% as.vector())), selected = "All complexes", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                radioGroupButtons(inputId = "edgetype", label = "Select edge type", choices = c('Proxy' = "proxy", 'Intragenic' = 'intragenic'), selected = "proxy", justified = TRUE))  
      } else {
        tagList(selectInput("protqtlplot_complexselect", "Current selection", (c("All complexes", forout_reactive$table_complex %>% filter((cor > input$param_qtlprot_cor[2] | cor < input$param_qtlprot_cor[1]) & qval < 10^(input$param_qtlprot_qval) & inCORUM == TRUE) %>% group_by(ComplexID) %>% add_tally(name = "complexsize") %>% ungroup() %>% filter(complexsize >= 2) %>% arrange(-complexsize) %>% select(ComplexName) %>% unique %>% unlist %>% as.vector())), selected = "All complexes", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                radioGroupButtons(inputId = "edgetype", label = "Select edge type", choices = c('Proxy' = "proxy", 'Intragenic' = 'intragenic', 'Variant effect' = "ve", "Variant impact" = "impact"), selected = "proxy", justified = TRUE))
      }
      
    } else if (input$prottype == "bioplex"){
      
      if(forout_reactive$qtl_annotated == FALSE){
        tagList(selectInput("protqtlplot_complexselect", "Current selection", (c("All complexes", db_bioplex3_4pc %>% mutate(VarVar = tolower(VarVar)) %>% filter(VarVar %in% (forout_reactive$table_complex %>% filter((cor > input$param_qtlprot_cor[2] | cor < input$param_qtlprot_cor[1]) & qval < 10^(input$param_qtlprot_qval)) %>% select(VarVar) %>% unlist)) %>% select(SymbolA, SymbolB) %>% c(.$SymbolA, .$SymbolB) %>% unique %>% unlist %>% as.vector())), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                radioGroupButtons(inputId = "edgetype", label = "Select edge type", choices = c('Proxy' = "proxy", 'Intragenic' = 'intragenic'), selected = "proxy", justified = TRUE))  
      } else {
        tagList(selectInput("protqtlplot_complexselect", "Current selection", (c("All complexes", db_bioplex3_4pc %>% mutate(VarVar = tolower(VarVar)) %>% filter(VarVar %in% (forout_reactive$table_complex %>% filter((cor > input$param_qtlprot_cor[2] | cor < input$param_qtlprot_cor[1]) & qval < 10^(input$param_qtlprot_qval)) %>% select(VarVar) %>% unlist)) %>% select(SymbolA, SymbolB) %>% c(.$SymbolA, .$SymbolB) %>% unique %>% unlist %>% as.vector())), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                radioGroupButtons(inputId = "edgetype", label = "Select edge type", choices = c('Proxy' = "proxy", 'Intragenic' = 'intragenic', 'Variant effect' = "ve", "Variant impact" = "impact"), selected = "proxy", justified = TRUE))
      }
      
    } else if (input$prottype == "all"){
      
      if(forout_reactive$qtl_annotated == FALSE){
        tagList(selectizeInput("protqtlplot_complexselect", "Current selection", (c("All proteins", forout_reactive$table_complex %>% filter((cor > input$param_qtlprot_cor[2] | cor < input$param_qtlprot_cor[1]) & qval < 10^(input$param_qtlprot_qval))  %>% select(varID1, varID2) %>% c(.$varID1, .$varID2) %>% unique %>% unlist %>% as.vector() %>% sort(., decreasing = FALSE))), selected = "All proteins", multiple = TRUE, options = list(maxItems = 10)),
                radioGroupButtons(inputId = "edgetype", label = "Select edge type", choices = c('Proxy' = "proxy", 'Intragenic' = 'intragenic'), selected = "proxy", justified = TRUE))  
      } else {
        tagList(selectizeInput("protqtlplot_complexselect", "Current selection", (c("All proteins", forout_reactive$table_complex %>% filter((cor > input$param_qtlprot_cor[2] | cor < input$param_qtlprot_cor[1]) & qval < 10^(input$param_qtlprot_qval))  %>% select(varID1, varID2) %>% c(.$varID1, .$varID2) %>% unique %>% unlist %>% as.vector() %>% sort(., decreasing = FALSE))), selected = "All proteins", multiple = TRUE, options = list(maxItems = 10)),
                radioGroupButtons(inputId = "edgetype", label = "Select edge type", choices = c('Proxy' = "proxy", 'Intragenic' = 'intragenic', 'Variant effect' = "ve", "Variant impact" = "impact"), selected = "proxy", justified = TRUE))
      }
      
    } else if (input$prottype == "protwqtl"){
      
      if(forout_reactive$qtl_annotated == FALSE){
        tagList(selectInput("protqtlplot_complexselect", "Current selection", (c("All proteins with QTLs")), selected = "All proteins with QTLs", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                radioGroupButtons(inputId = "edgetype", label = "Select edge type", choices = c('Proxy' = "proxy", 'Intragenic' = 'intragenic'), selected = "proxy", justified = TRUE))  
      } else {
        tagList(selectInput("protqtlplot_complexselect", "Current selection", (c("All proteins with QTLs")), selected = "All proteins with QTLs", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                radioGroupButtons(inputId = "edgetype", label = "Select edge type", choices = c('Proxy' = "proxy", 'Intragenic' = 'intragenic', 'Variant effect' = "ve", "Variant impact" = "impact"), selected = "proxy", justified = TRUE))
      }
      
    }

  })
  
  # QTLPROT - Select proxy ----
  output$edgecol_ui <- renderUI({
    req(input$edgetype)
    
    if(forout_reactive$qtl_annotated == FALSE){
      
      if(input$edgetype == "proxy"){
      tagList(selectInput("edgeproxy", "Select QTLs to include ", choices = c(forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[9])) %>% unique %>% as.vector()), selected = c(forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[9])) %>% unique %>% as.vector()), multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
                tags$div(title="e.g. only keep edges of SNPs which contain a cis and a trans connection", checkboxInput("edgeproxy_intersect_rsid", label = "Only keep SNPs with all selected proxies?", value = FALSE)),
                tags$div(title="e.g. only keep edges of proteins which contain a cis and a trans connection.", checkboxInput("edgeproxy_intersect", label = "Only keep proteins with all selected proxies?", value = FALSE)),
                actionButton(inputId = "protqtl_bttn", label = "Make plot!"))
        
      } else if(input$edgetype == "intragenic"){
      tagList(selectInput("edgeproxy", "Select QTLs to include", choices = c(forout_reactive$table_qtl_processed %>% select(CP_Intragenic_QTL) %>% unique %>% as.vector()), selected = c(forout_reactive$table_qtl_processed %>% select(CP_Intragenic_QTL) %>% unique %>% as.vector()), multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
                tags$div(title="e.g. only keep edges of SNPs which contain a cis and a trans connection", checkboxInput("edgeproxy_intersect_rsid", label = "Only keep SNPs with all selected proxies?", value = FALSE)),
                tags$div(title="e.g. only keep edges of proteins which contain a cis and a trans connection.", checkboxInput("edgeproxy_intersect", label = "Only keep proteins with all selected proxies?", value = FALSE)),
                actionButton(inputId = "protqtl_bttn", label = "Make plot!"))
      }
      
    } else {
    if(input$edgetype == "proxy"){
      tagList(selectInput("edgeproxy", "Select QTLs to include", choices = c(forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[9])) %>% unique %>% as.vector()), selected = c(forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[9])) %>% unique %>% as.vector()), multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
              tags$div(title="e.g. only keep edges of SNPs which contain a cis and a trans connection", checkboxInput("edgeproxy_intersect_rsid", label = "Only keep SNPs with all selected proxies?", value = FALSE)),
              tags$div(title="e.g. only keep edges of proteins which contain a cis and a trans connection.", checkboxInput("edgeproxy_intersect", label = "Only keep proteins with all selected proxies?", value = FALSE)),
              actionButton(inputId = "protqtl_bttn", label = "Make plot!"))
    } else if(input$edgetype == "intragenic"){
      tagList(selectInput("edgeproxy", "Select QTLs to include", choices = c(forout_reactive$table_qtl_processed %>% select(CP_Intragenic_QTL) %>% unique %>% as.vector()), selected = c(forout_reactive$table_qtl_processed %>% select(CP_Intragenic_QTL) %>% unique %>% as.vector()), multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
              tags$div(title="e.g. only keep edges of SNPs which contain a cis and a trans connection", checkboxInput("edgeproxy_intersect_rsid", label = "Only keep SNPs with all selected proxies?", value = FALSE)),
              tags$div(title="e.g. only keep edges of proteins which contain a cis and a trans connection.", checkboxInput("edgeproxy_intersect", label = "Only keep proteins with all selected proxies?", value = FALSE)),
              actionButton(inputId = "protqtl_bttn", label = "Make plot!"))
      } else if(input$edgetype == "ve"){
      tagList(selectInput("edgeproxy", "Select QTLs to include", choices = c(forout_reactive$table_qtl_processed %>% select(CP_Variant_Effect) %>% unique %>% as.vector()), selected = c(forout_reactive$table_qtl_processed %>% select(CP_Variant_Effect) %>% unique %>% as.vector()), multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
              tags$div(title="e.g. only keep edges of SNPs which contain a cis and a trans connection", checkboxInput("edgeproxy_intersect_rsid", label = "Only keep SNPs with all selected proxies?", value = FALSE)),
              tags$div(title="e.g. only keep edges of proteins which contain a cis and a trans connection.", checkboxInput("edgeproxy_intersect", label = "Only keep proteins with all selected proxies?", value = FALSE)),
              actionButton(inputId = "protqtl_bttn", label = "Make plot!"))
    } else {
      tagList(selectInput("edgeproxy", "Select QTLs to include", choices = c(forout_reactive$table_qtl_processed %>% select(CP_Variant_Impact) %>% unique %>% as.vector()), selected = c(forout_reactive$table_qtl_processed %>% select(CP_Variant_Impact) %>% unique %>% as.vector()), multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
              tags$div(title="e.g. only keep edges of SNPs which contain a cis and a trans connection", checkboxInput("edgeproxy_intersect_rsid", label = "Only keep SNPs with all selected proxies?", value = FALSE)),
              tags$div(title="e.g. only keep edges of proteins which contain a cis and a trans connection.", checkboxInput("edgeproxy_intersect", label = "Only keep proteins with all selected proxies?", value = FALSE)),
              actionButton(inputId = "protqtl_bttn", label = "Make plot!"))
    } }
  })
  
  
  output$snp_interaction_table_ui <- renderUI({
    
    req(forout_reactive$table_qtl_processed, forout_reactive$table_pheno_processed)
    
    box(title = "SNP interaction table", status = "primary", width = 12,
        HTML("<p>A table containing all SNPs that are associated to 1) a protein/transcript and 2) a trait can be generated to aid the interpretation of the bait network plots. This table can be exported with separate rows for individual traits, by combining all traits associated with a SNP-gene combination (e.g. <code>SNP1, gene1, trait1;trait2;trait3</code>) or by combining all genes associated with a SNP-trait combination (e.g. <code>SNP1, trait1, gene1;gene2;gene3</code>).</p>"), br(),
        selectInput("interactiontable_type", "Select the format of the interaction table", choices = c("Single trait per row" = "single", "Combine traits" = "traits", "Combine genes" = "genes"), selected = 1, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
        downloadButton("download_interactiontable", "Download interactions"))
    
  })
  
  output$parambox_baitnw_ui <- renderUI({
    
    req(forout_reactive$table_qtl_processed, forout_reactive$table_pheno_processed)
    
    if(!isTruthy(forout_reactive$temp_baitnetwork)){
      
      box(title = "Plot parameters", status = "primary", width = 12, radioGroupButtons(inputId = "baittype", label = "Bait type", choices = c("Gene / Protein" = "qtl", Phenotype = "pheno"), selected = "qtl", justified = TRUE), 
          uiOutput("baitselect_ui1"), uiOutput("baitselect_ui2"),
          selectInput("nodelabels", "Select node labels to display", c("All", "No SNP labels", "None"), selected = "All", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
          selectInput(inputId = "baitnetwork_dgi", label = "Add drug-gene interactions from DGIdb?", choices = c('None', 'All DGIdb interactions', db_dgidb_source$dgi_source), selected = "None", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
          actionButton(inputId = "bait_bttn", label = "Make plot!"))
      
    } else {
      
      box(title = "Plot parameters", status = "primary", width = 12, radioGroupButtons(inputId = "baittype", label = "Bait type", choices = c("Gene / Protein" = "qtl", Phenotype = "pheno"), selected = "qtl", justified = TRUE), 
          uiOutput("baitselect_ui1"), uiOutput("baitselect_ui2"),
          selectInput("nodelabels", "Select node labels to display", c("All", "No SNP labels", "None"), selected = "All", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
          selectInput(inputId = "baitnetwork_dgi", label = "Add drug-gene interactions from DGIdb?", choices = c('None', 'All DGIdb interactions', db_dgidb_source$dgi_source), selected = "None", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
          actionButton(inputId = "bait_bttn", label = "Make plot!"), 
          downloadButton("download_baitnetwork", "Download network"),
          downloadButton("download_baitnetworktable", "Download network as table"))
      
    }
    
  })
  
  output$resultbox_baitnw_ui <- renderUI({
    
    req(forout_reactive$bait_links)
    
    box(title = "Protein network plot", width = 12, forceNetworkOutput("network_bait", width = "100%", height = "70vh") %>% withSpinner(), plotOutput("network_bait_legend") %>% withSpinner(), HTML(paste("<b>", "Figure: Interactive network plot. ", "</b>","<em>", "Network plot created using the igraph and networkD3 R packages. Edge colours indicate interaction types.", "</em>")))    
  
  })
  
  
  output$parambox_nw_ui <- renderUI({
    
    req(forout_reactive$table_complex)
    
    box(title = "Plot parameters", status = "primary", width = 12, uiOutput("ui_param_network1"), uiOutput("ui_param_network2"), uiOutput("ui_param_network3"), uiOutput("ui_param_network4"), uiOutput("ui_param_network5"))
  })  
  
  output$resultbox_nw_ui <- renderUI({
    
    req(forout_reactive$interactive_plot_network)
    
    box(title = "Protein network plot", width = 12, forceNetworkOutput("network_plot_interactive", width = "100%", height = "70vh") %>% withSpinner(), plotOutput("network_legend") %>% withSpinner(), HTML(paste("<b>", "Figure: Interactive network plot. ", "</b>","<em>", "Network plot created using the igraph and networkD3 R packages. Edge colours indicate interaction types.", "</em>")))    
 
  })
  
  
  output$parambox_qtlprot_ui <- renderUI({
    
    req(forout_reactive$table_qtl_processed, forout_reactive$table_complex)
    
    box(title = "Plot parameters", status = "primary", width = 12, uiOutput("protqtlplot_ui1"), uiOutput("protqtlplot_ui2"), uiOutput("edgecol_ui"))

  })
  
  output$resultbox_qtlprot_ui <- renderUI({
    
    req(forout_reactive$plot_manhattan, forout_reactive$plot_edge, forout_reactive$plot_arcdiagram)
    
    box(title = "SNP-Protein interaction", width = 12, plotOutput("plot_manhattan") %>% withSpinner(), plotOutput("plot_edge") %>% withSpinner(), plotOutput("plot_arcdiagram") %>% withSpinner(),
        HTML(paste("<b>", "Figure: SNP-Protein interactions. ", "</b>","<em>", "The SNP-protein interaction plot is constructed from three separate plots. A Manhattan plot (top) highlights the QTL p-values per chromosome. Edges are drawn (center) connecting QTL and protein data, where edge color indicates the QTL type. Protein-Protein interactions are shown using arc-diagrams, proteins are ordered by complex size and number of connections.", "</em>")))    
  })
  

  
  
  
  # Analysis - ui_param_summary ----
  output$parambox_summary_ui <- renderUI({
    req(forout_reactive$table_complex)
    maxpval <- forout_reactive$table_complex[[ "pval" ]] %>% -(log10(.)) %>% .[is.finite(.)] %>% max() %>% floor()
    
    
      box(title = "Plot parameters", status = "primary", solidHeader = FALSE, width = 12,
          sliderInput("param_summary_qval", "q-value cut-off (log10 scale)", min = -maxpval, max = 0, value = 0, step = 1),
          sliderInput(inputId = "param_summary_cor", "Correlation cut-off", min = -1, max = 1, value = c(-0.5, 0.5), step = 0.01),
          actionButton(inputId = "summary_bttn", label = "Make plot!"))
  })
  
  # Analysis - ui_param_database ----
  output$parambox_db_ui <- renderUI({
    req(forout_reactive$table_complex)
    maxpval <- forout_reactive$table_complex[[ "pval" ]] %>% -(log10(.)) %>% .[is.finite(.)] %>% max() %>% floor()
    
    box(title = "Plot parameters", status = "primary", solidHeader = FALSE, width = 12,
        sliderInput("param_database_qval", "q-value cut-off (log10 scale)", min = -maxpval, max = 0, value = 0, step = 1),
        sliderInput(inputId = "param_database_cor", "Correlation cut-off", min = -1, max = 1, value = 0.5, step = 0.01),
        actionButton(inputId = "database_bttn", label = "Make plot!"))
  })
  
  # Analysis - ui_param_network ----
  output$ui_param_network1 <- renderUI({
    req(forout_reactive$table_complex, db_hpa_locoverlap)
    
    maxpval <- forout_reactive$table_complex[[ "pval" ]] %>% -(log10(.)) %>% .[is.finite(.)] %>% max() %>% floor()
    
    if(is.null(forout_reactive$table_qtl_processed) == TRUE){
      tagList(
        sliderInput("param_network_qval", "q-value cut-off (log10 scale)", min = -maxpval, max = 0, value = 0, step = 1),
        sliderInput(inputId = "param_network_cor", "Correlation cut-off", min = -1, max = 1, value = c(-1, 0.5), step = 0.01),
        selectInput("select_organelle", "Select organelles to include", choices = c(db_hpa_locoverlap %>% distinct() %>% select(loc1) %>% unlist %>% unique() %>% strsplit(., ";") %>% unlist %>% unique %>% sort(., decreasing = FALSE, na.last = TRUE), NA), selected = c(db_hpa_locoverlap %>% distinct() %>% select(loc1) %>% unlist %>% unique() %>% strsplit(., ";") %>% unlist %>% unique %>% sort(., decreasing = TRUE, na.last = TRUE), NA), multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
        selectInput(inputId = "prottype_network", label = "Selecting protein source", choices = c('CORUM' = "corum", 'BioPlex 3.0' = "bioplex", "All / individual proteins" = "all"), selected = "CORUM", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
      )
    } else {
      tagList(
        sliderInput("param_network_qval", "q-value cut-off (log10 scale)", min = -maxpval, max = 0, value = 0, step = 1),
        sliderInput(inputId = "param_network_cor", "Correlation cut-off", min = -1, max = 1, value = c(-1, 0.5), step = 0.01),
        selectInput("select_organelle", "Select organelles to include", choices = c(db_hpa_locoverlap %>% distinct() %>% select(loc1) %>% unlist %>% unique() %>% strsplit(., ";") %>% unlist %>% unique %>% sort(., decreasing = FALSE, na.last = TRUE), NA), selected = c(db_hpa_locoverlap %>% distinct() %>% select(loc1) %>% unlist %>% unique() %>% strsplit(., ";") %>% unlist %>% unique %>% sort(., decreasing = TRUE, na.last = TRUE), NA), multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
        selectInput(inputId = "prottype_network", label = "Selecting protein source", choices = c('CORUM' = "corum", 'BioPlex 3.0' = "bioplex", "All proteins with QTL" = "protwqtl", "All / individual proteins" = "all"), selected = "CORUM", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
      )
    }

  })
  
  # Analysis - ui_param_network ----
  output$ui_param_network2 <- renderUI({
    req(forout_reactive$table_complex, input$prottype_network, forout_reactive$protanno)
    
    if(input$prottype_network == "corum"){tagList(selectInput("network_complexselect", "Current selection", (c("All complexes", forout_reactive$table_complex %>% filter((cor > input$param_network_cor[2] | cor < input$param_network_cor[1]) & qval < 10^(input$param_network_qval) & inCORUM == TRUE) %>% group_by(ComplexID) %>% add_tally(name = "complexsize") %>% ungroup() %>% filter(complexsize >= 2) %>% arrange(-complexsize) %>% select(ComplexName) %>% unique %>% unlist %>% as.vector())), selected = "All complexes", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL))
      
    } else if (input$prottype_network == "bioplex"){tagList(selectInput("network_complexselect", "Current selection", (c("All complexes", db_bioplex3_4pc %>% mutate(VarVar = tolower(VarVar)) %>% filter(VarVar %in% (forout_reactive$table_complex %>% filter((cor > input$param_network_cor[2] | cor < input$param_network_cor[1]) & qval < 10^(input$param_network_qval)) %>% select(VarVar) %>% unlist)) %>% select(SymbolA, SymbolB) %>% c(.$SymbolA, .$SymbolB) %>% unique %>% unlist %>% sort(., decreasing = FALSE) %>% as.vector())), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL))
    
    } else if (input$prottype_network == "all"){tagList(selectInput("network_complexselect", "Current selection", (c("All proteins", forout_reactive$protanno$ID %>% unique %>% unlist %>% as.vector() %>% sort(., decreasing = FALSE) %>% as.data.table())), selected = "All proteins", multiple = TRUE, selectize = TRUE, width = NULL, size = NULL))
      
    } else {tagList(selectInput("network_complexselect", "Current selection", (c("All proteins with QTLs")), selected = "All proteins with QTLs", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL))
      
    }
  })
  
  # Analysis - ui_param_network ----
  output$ui_param_network3 <- renderUI({
    req(forout_reactive$table_complex, input$prottype_network)
    
    if(is.null(forout_reactive$table_qtl_processed) == TRUE){
    tagList(
      shinyjs::hidden(selectInput("network_edgeselect", "Select edge colour", c("Proxy", "Intragenic"), selected = "Proxy", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)))
  } else if(is.null(forout_reactive$table_qtl_processed) == FALSE & forout_reactive$qtl_annotated == FALSE) {
    tagList(
      selectInput("network_edgeselect", "Select edge colour", c("Proxy", "Intragenic"), selected = "Proxy", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL))
  } else if(is.null(forout_reactive$table_qtl_processed) == FALSE & forout_reactive$qtl_annotated == TRUE) {
    tagList(
      selectInput("network_edgeselect", "Select edge colour", c("Proxy", "Intragenic", "Variant Effect", "Variant Impact"), selected = "Proxy", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)) 
  }
  })
  
  # Analysis - ui_param_network ----
  output$ui_param_network4 <- renderUI({
    req(forout_reactive$table_complex, input$prottype_network)
    
    if(is.null(forout_reactive$table_qtl_processed) == FALSE & is.null(forout_reactive$table_pheno_processed) == FALSE){
      tagList(radioGroupButtons(inputId = "network_qtlselect", label = "Select datatype", choices = c('Proteomics only' = "proteomics", 'Include pQTL' = "pqtl", 'Include pQTL & molQTL' = "pqtlmqtl"), selected = "proteomics", justified = TRUE))
    } else if(is.null(forout_reactive$table_qtl_processed) == FALSE & is.null(forout_reactive$table_pheno_processed) == TRUE){
      tagList(radioGroupButtons(inputId = "network_qtlselect", label = "Select datatype", choices = c('Proteomics only' = "proteomics", 'Include pQTL' = "pqtl"), selected = "proteomics", justified = TRUE)) 
    } else {
      tagList(shinyjs::hidden(radioGroupButtons(inputId = "network_qtlselect", label = "Select datatype", choices = c('Proteomics only' = "proteomics", 'Include pQTL' = "pqtl", 'Include pQTL & molQTL' = "pqtlmqtl"), selected = "proteomics", justified = TRUE)))    
    }  
  })
  
  
  # Analysis - ui_param_network ----
  output$ui_param_network5 <- renderUI({
    req(forout_reactive$table_complex, input$prottype_network)
    
    if(is.null(forout_reactive$table_qtl_processed) == TRUE){
      tagList(
        shinyjs::hidden(radioGroupButtons(inputId = "network_qtlsummarize", label = "Summarize QTLs?", choices = c('Individual QTLs' = "individual", 'Per chromosome' = "chromosome"), selected = "individual", justified = TRUE)),
        selectInput("nodelabelsnw", "Select node labels to display", c("All", "No SNP labels", "None"), selected = "All", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
        selectInput(inputId = "network_dgi", label = "Add drug-gene interactions from DGIdb?", choices = c('None', 'All DGIdb interactions', db_dgidb_source$dgi_source), selected = "None", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
        actionButton(inputId = "network_bttn", label = "Make plot!"),
        downloadButton("download_network", "Download network"),
        downloadButton("download_networktable", "Download network as table"))    
      } else if(forout_reactive$ld_processed == TRUE){
      tagList(
        radioGroupButtons(inputId = "network_qtlsummarize", label = "Summarize QTLs?", choices = c('Individual QTLs' = "individual", 'Per chromosome' = "chromosome", "Per LD" = "LD"), selected = "individual", justified = TRUE),
        selectInput("nodelabelsnw", "Select node labels to display", c("All", "No SNP labels", "None"), selected = "All", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
        selectInput(inputId = "network_dgi", label = "Add drug-gene interactions from DGIdb?", choices = c('None', 'All DGIdb interactions', db_dgidb_source$dgi_source), selected = "None", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
        actionButton(inputId = "network_bttn", label = "Make plot!"),
        downloadButton("download_network", "Download network"),
        downloadButton("download_networktable", "Download network as table"))
    } else {
      tagList(
        radioGroupButtons(inputId = "network_qtlsummarize", label = "Summarize QTLs?", choices = c('Individual QTLs' = "individual", 'Per chromosome' = "chromosome"), selected = "individual", justified = TRUE),
        selectInput("nodelabelsnw", "Select node labels to display", c("All", "No SNP labels", "None"), selected = "All", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
        selectInput(inputId = "network_dgi", label = "Add drug-gene interactions from DGIdb?", choices = c('None', 'All DGIdb interactions', db_dgidb_source$dgi_source), selected = "None", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
        actionButton(inputId = "network_bttn", label = "Make plot!"),
        downloadButton("download_network", "Download network"),
        downloadButton("download_networktable", "Download network as table")) 
    }
  })
  
  
  
  # Analysis - ui_param_network_bait ----
  output$baitselect_ui1 <- renderUI({
    req(input$baittype, forout_reactive$table_qtl_processed, forout_reactive$table_pheno_processed)
    
    if(input$baittype == "qtl" & forout_reactive$qtl_annotated == TRUE){
      
    tagList(
      isolate(selectizeInput("baitselect", "Select Bait Gene/Protein", choices = c(forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% arrange(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unique %>% as.vector()), selected = NULL, multiple = TRUE, options = list(maxItems = 15))),
      selectInput("baitnetwork_edgeselect", "Select edge colour", c("Proxy", "Intragenic", "Variant Effect", "Variant Impact"), selected = "Proxy", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)) 
      
      } else if(input$baittype == "qtl" & forout_reactive$qtl_annotated == FALSE){
        
        tagList(
          isolate(selectizeInput("baitselect", "Select Bait Gene/Protein", choices = c(forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% arrange(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unique %>% as.vector()), selected = NULL, multiple = TRUE, options = list(maxItems = 15))),
          selectInput("baitnetwork_edgeselect", "Select edge colour", c("Proxy", "Intragenic"), selected = "Proxy", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)) 
        
      } else if(input$baittype == "pheno" & forout_reactive$qtl_annotated == TRUE){
        
        tagList(
          isolate(selectizeInput("baitselect", "Select Bait Phenotype", choices = c(forout_reactive$table_pheno_processed %>% select(!! rlang::sym(forout_reactive$phenocolnames[2])) %>% arrange(!! rlang::sym(forout_reactive$phenocolnames[2])) %>% unique %>% as.vector()), selected = NULL, multiple = TRUE, options = list(maxItems = 15))),
          selectInput("baitnetwork_edgeselect", "Select edge colour", c("Proxy", "Intragenic", "Variant Effect", "Variant Impact"), selected = "Proxy", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL))
        
      } else if(input$baittype == "pheno" & forout_reactive$qtl_annotated == FALSE){
        
        tagList(
          isolate(selectizeInput("baitselect", "Select Bait Phenotype", choices = c(forout_reactive$table_pheno_processed %>% select(!! rlang::sym(forout_reactive$phenocolnames[2])) %>% arrange(!! rlang::sym(forout_reactive$phenocolnames[2])) %>% unique %>% as.vector()), selected = NULL, multiple = TRUE, options = list(maxItems = 15))),
          selectInput("baitnetwork_edgeselect", "Select edge colour", c("Proxy", "Intragenic"), selected = "Proxy", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL))
        
      }
  })
  
  
  # Analysis - ui_param_network_bait ----
  output$baitselect_ui2 <- renderUI({
    req(forout_reactive$table_qtl_processed, forout_reactive$table_pheno_processed)
    
    if(forout_reactive$ld_processed == FALSE){
      
      tagList(radioGroupButtons(inputId = "baitnetwork_qtlsummarize", label = "Summarize QTLs?", choices = c('Individual QTLs' = "individual", 'Per chromosome' = "chromosome"), selected = "individual", justified = TRUE)) 
      
    } else if(forout_reactive$ld_processed == TRUE){
      
      tagList(radioGroupButtons(inputId = "baitnetwork_qtlsummarize", label = "Summarize QTLs?", choices = c('Individual QTLs' = "individual", 'Per chromosome' = "chromosome", "Per LD" = "LD"), selected = "individual", justified = TRUE)) 
      
    } 
  })
  
  
  # analysis - bait_network on buttonpress ----
  observeEvent(input$bait_bttn, {
    req(forout_reactive$table_qtl_processed, forout_reactive$table_pheno_processed, input$baitselect, input$baittype, input$baitnetwork_edgeselect)
    
    message("Action: Creating baitnetwork plot")
    
    if(input$baitnetwork_edgeselect == "Proxy"){      
      selection = forout_reactive$qtlcolnames[9]
    } else if(input$baitnetwork_edgeselect == "Intragenic"){
      selection = "CP_Intragenic_QTL"
    } else if(input$baitnetwork_edgeselect == "Variant Effect"){
      selection = "CP_Variant_Effect"
    } else if(input$baitnetwork_edgeselect == "Variant Impact"){
      selection = "CP_Variant_Impact"
    }
    
    if(input$baitnetwork_qtlsummarize != "LD") {
      # Run the following if data sHOULD NOT be summarized by LD

    if(input$baittype == "qtl"){
      
      bait_qtl <- forout_reactive$table_qtl_processed %>% filter(tolower(!! rlang::sym(forout_reactive$qtlcolnames[4])) %in% tolower(input$baitselect)) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(forout_reactive$qtlcolnames[3]), !! rlang::sym(selection)) %>% mutate(datatype = "qtl")
      bait_list <- bait_qtl %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1])) %>% unlist()
      bait_pheno <- forout_reactive$table_pheno_processed %>% filter(!! rlang::sym(forout_reactive$phenocolnames[1]) %in% bait_list) %>% select(!! rlang::sym(forout_reactive$phenocolnames[1]), !! rlang::sym(forout_reactive$phenocolnames[2]), !! rlang::sym(forout_reactive$phenocolnames[4]), !! rlang::sym(forout_reactive$phenocolnames[6])) %>% mutate(datatype = "Phenotype")

    } else {
      
      bait_pheno <- forout_reactive$table_pheno_processed %>% filter(!! rlang::sym(forout_reactive$phenocolnames[2]) %in% input$baitselect) %>% select(!! rlang::sym(forout_reactive$phenocolnames[1]), !! rlang::sym(forout_reactive$phenocolnames[2]), !! rlang::sym(forout_reactive$phenocolnames[4])) %>% mutate(edge = "Phenotype", datatype = "Phenotype")
      bait_list <- bait_pheno %>% select(!! rlang::sym(forout_reactive$phenocolnames[1])) %>% unlist()
      bait_qtl <- forout_reactive$table_qtl_processed %>% filter(!! rlang::sym(forout_reactive$qtlcolnames[1]) %in% bait_list) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(forout_reactive$qtlcolnames[3]), !! rlang::sym(selection)) %>% mutate(datatype = "qtl")

    }
    
    } else {
      # Run the following if data SHOULD be summarized by LD
      # first make a rsid / LD mapping from QTL data > Then join onto pheno, before mutating datatype > Then the rest should work as intended
      
      ld_table <- forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1]), LD_name) %>% `colnames<-`(c("varID1", "LD_name"))
      
      if(input$baittype == "qtl"){
        
        bait_qtl <- forout_reactive$table_qtl_processed %>% filter(tolower(!! rlang::sym(forout_reactive$qtlcolnames[4])) %in% tolower(input$baitselect)) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(forout_reactive$qtlcolnames[3]), !! rlang::sym(selection), LD_name) %>% mutate(datatype = "qtl")
        bait_list <- bait_qtl %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1])) %>% unlist()
        
        bait_pheno <- forout_reactive$table_pheno_processed %>% filter(!! rlang::sym(forout_reactive$phenocolnames[1]) %in% bait_list) %>% select(!! rlang::sym(forout_reactive$phenocolnames[1]), !! rlang::sym(forout_reactive$phenocolnames[2]), !! rlang::sym(forout_reactive$phenocolnames[4]), !! rlang::sym(forout_reactive$phenocolnames[6])) %>% `colnames<-`(c("varID1", "varID2", "chromosome", "edge")) %>% left_join(., ld_table) %>% mutate(datatype = "Phenotype")
        
      } else {
        
        bait_pheno <- forout_reactive$table_pheno_processed %>% filter(!! rlang::sym(forout_reactive$phenocolnames[2]) %in% input$baitselect) %>% select(!! rlang::sym(forout_reactive$phenocolnames[1]), !! rlang::sym(forout_reactive$phenocolnames[2]), !! rlang::sym(forout_reactive$phenocolnames[4])) %>% mutate(edge = "Phenotype") %>% `colnames<-`(c("varID1", "varID2", "chromosome", "edge")) %>% left_join(., ld_table) %>% mutate( datatype = "Phenotype")
        bait_list <- bait_pheno %>% select(varID1) %>% unlist()
        bait_qtl <- forout_reactive$table_qtl_processed %>% filter(!! rlang::sym(forout_reactive$qtlcolnames[1]) %in% bait_list) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(forout_reactive$qtlcolnames[3]), !! rlang::sym(selection), LD_name) %>% mutate(datatype = "qtl")
        
      }
      
    }

    
    if(input$baitnetwork_qtlsummarize == "individual"){
      bait_edge <- rbind(bait_pheno, bait_qtl %>% `colnames<-`(colnames(bait_pheno))) %>% `colnames<-`(c("varID1", "varID2", "chromosome", "edge","datatype")) %>% mutate(edge = edge %>% tidyr::replace_na(., "Not annotated"))
      
    } else if(input$baitnetwork_qtlsummarize == "chromosome"){
      bait_edge <- rbind(bait_pheno, bait_qtl %>% `colnames<-`(colnames(bait_pheno))) %>% `colnames<-`(c("varID1", "varID2", "chromosome", "edge","datatype")) %>% mutate(varID1 = paste0("Chr_", chromosome)) %>% mutate(edge = edge %>% tidyr::replace_na(., "Not annotated"))
      
    } else if(input$baitnetwork_qtlsummarize == "LD") {
      
      bait_edge <- rbind(bait_pheno, bait_qtl %>% `colnames<-`(colnames(bait_pheno))) %>% `colnames<-`(c("varID1", "varID2", "chromosome","edge", "LD_name", "datatype")) %>% mutate(varID1 = case_when(is.na(LD_name) ~ as.character(varID1), TRUE ~ LD_name)) %>% select(-LD_name) %>% mutate(edge = edge %>% tidyr::replace_na(., "Not annotated"))
      
      print(head(bait_edge))
      
    }
    
    if(input$baitnetwork_dgi != "None"){
      
      if(input$baitnetwork_dgi == "All DGIdb interactions"){
        drug <- db_dgidb %>% select(drug_name, gene_name)
      } else {
        drug <- db_dgidb %>% filter(dgi_interaction_claim_source == input$baitnetwork_dgi %>% sub(" .*", "", .)) %>% select(drug_name, gene_name)
      }
      
      bait_edge <- rbind(bait_edge, drug %>% filter(gene_name %in% (bait_qtl %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unlist %>% tolower())) %>% mutate(chromosome = "", edge = "Drug-gene interaction", datatype = "Drug-gene interaction") %>% `colnames<-`(colnames(bait_edge)) ) %>% mutate(varID2 = case_when(tolower(as.character(varID2)) %in% (bait_qtl %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unlist %>% tolower) ~ tolower(as.character(varID2)), TRUE ~ as.character(varID2)) ) 
    }
    
    bait_vertices <- unlist(list(bait_edge[,1] %>% as.character %>% unlist, bait_edge[,2] %>% as.character %>% unlist)) %>% as.data.frame() %>% `colnames<-`(c("var")) %>% group_by(var) %>% summarize(n=n()) %>% mutate(nodetype = case_when(tolower(var) %in% tolower(bait_qtl[[forout_reactive$qtlcolnames[4]]]) == TRUE ~ "Protein", var %in% db_dgidb$drug_name == TRUE ~ "Drug", var %in% bait_edge$varID1 == TRUE ~ "SNP", var %in% bait_pheno[[forout_reactive$phenocolnames[2]]] == TRUE ~ "Phenotype"))
    
    
    if(nrow(bait_vertices) > 0){
      nodes <- bait_vertices %>% mutate(numid = 0:(nrow(bait_vertices)-1))
    } else {
      nodes <- bait_vertices %>% mutate(numid = 0)
    }
    
    if(nrow(bait_vertices) > 2000){
      sendSweetAlert(session = session, title = "Error, too many network nodes (>2000)", text = "Please select a stricter correlation cut-off", type = "error")
    } else {  
    
    links <- bait_edge %>% mutate(varID1 = as.factor(varID1), varID2 = as.factor(varID2)) %>% left_join(., nodes %>% select(var, numid) %>% `colnames<-`(c("varID1", "source"))) %>% left_join(., nodes %>% select(var, numid) %>% `colnames<-`(c("varID2", "target"))) %>% select(source, target, everything())
    
    F2 <- colorRampPalette(c("#FFAE42", "#094183"), bias = nrow(links), space = "rgb", interpolate = "linear")
    colCodes <- F2(length(unique(links$edge)))
    links <- links %>% mutate(edgecol = sapply(links$edge, function(x) colCodes[which(sort(unique(links$edge)) == x)])) %>% group_by(source, target) %>% distinct() %>% ungroup()
    
    if(input$nodelabels == "None"){nodes <- nodes %>% mutate(var = NA)}
    if(input$nodelabels == "No SNP labels"){nodes <- nodes %>% mutate(var = case_when(nodetype %in% c("SNP") ~ "", TRUE ~ nodes$var))}
    
    forout_reactive$bait_nodes <- nodes
    forout_reactive$bait_links <- links
    
    forout_reactive$temp_baitnetwork <- TRUE
    
    updateProgressBar(session = session, id = "pb8", value = 100)

    }
  })
  
  
  # Analysis - network_plot_interactive ----
  output$network_bait <- renderForceNetwork( {
    req(forout_reactive$bait_links, forout_reactive$bait_nodes)
    
        forceNetwork(Links = forout_reactive$bait_links, Nodes = forout_reactive$bait_nodes, Source = 'source', Target = 'target', NodeID = 'var', Nodesize = 'n', Group = 'nodetype', linkColour = forout_reactive$bait_links$edgecol, charge = input$nodecharge, opacity = 1, fontSize = 12, zoom = TRUE, opacityNoHover = 0.6, legend = TRUE)
    
  })
  
  
 
  
  # Analysis - network_plot_interactive ----
  output$network_bait_legend <- renderPlot( {
    req(forout_reactive$bait_links)
    
    linklegend <- forout_reactive$bait_links %>% select(edge, edgecol) %>% distinct()
    legend <- ggplot(linklegend, aes(x = edge, y = 1, col = edgecol)) + geom_line(size = 3) + theme_bw() + scale_color_manual(name = "Link type", values = linklegend$edgecol %>% as.character(), labels = linklegend$edge) +theme(legend.position="bottom", legend.text=element_text(size=15), legend.title = element_text(size=15))
    legend <- ggpubr::get_legend(legend)
    
    ggpubr::as_ggplot(legend)

  })
  

  # output - fileinput - qtltable ----  
  output$qtltable <- DT::renderDT({
    req(forout_reactive$table_qtl_processed)
    return(forout_reactive$table_qtl_processed %>% head(., 20) %>% DT::datatable(., rownames = FALSE, 
                                      caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: pQTL/eQTL summary table. ", "</b>","<em>","Table containing the pQTLs/eQTLs after filtering" , "</em>"))),
                                      options = list(scrollX = TRUE, pageLength = 5, dom = 'tip')))
  }, server = FALSE)
  
  
  # output - fileinput - qtltable ----  
  output$qtltallytable <- DT::renderDT({
    req(forout_reactive$table_qtl_processed, forout_reactive$proxylist_qtl)
    
    tallytable = forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unique()
    total = forout_reactive$table_qtl_processed %>% group_by(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% tally(name = "n_total")
    
      for(i in 1:length(forout_reactive$proxylist_qtl)){
        tallytable <- tallytable %>% left_join(., forout_reactive$table_qtl_processed %>% filter(!! rlang::sym(forout_reactive$qtlcolnames[9]) == forout_reactive$proxylist_qtl[i]) %>% group_by(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% tally(name = paste0("n_",forout_reactive$proxylist_qtl[i])))
      }

    tallytable <- tallytable %>% left_join(., total)
    
    forout_reactive$table_qtl_tally <- tallytable %>% mutate_all(funs(tidyr::replace_na(., 0))) %>% arrange(-n_total)
    return(forout_reactive$table_qtl_tally %>% DT::datatable(., rownames = FALSE, caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: pQTL/eQTL gene tally table. ", "</b>","<em>","A tally of the number of pQTLs/eQTLs per protein/transcript. The total number, and the number for each of the provided groupings/proxies is shown. The table is ordered from most prevalent to least prevalent" , "</em>"))),
                                                             options = list(scrollX = TRUE, pageLength = 5, dom = 'tip')))
    
  }, server = FALSE)
  
  
  # output - intrainterable ----  
  output$intraintertable <- DT::renderDT({
    req(forout_reactive$table_qtl_processed)
    
    # Find all SNPs that are intragenic in one gene and intergenic to another
    snplist <- intersect(forout_reactive$table_qtl_processed %>% filter(CP_Intragenic_QTL == TRUE) %>% select(ID) %>% unlist,
                         forout_reactive$table_qtl_processed %>% filter(CP_Intragenic_QTL == FALSE) %>% select(ID) %>% unlist)
    

    interactiontable <- left_join(forout_reactive$table_qtl_processed %>% filter(ID %in% snplist & CP_Intragenic_QTL == TRUE) %>% select(ID, !! rlang::sym(forout_reactive$qtlcolnames[4])) %>% `colnames<-`(c("ID", "Intragenic_gene")),
                                                   forout_reactive$table_qtl_processed %>% filter(ID %in% snplist & CP_Intragenic_QTL == FALSE) %>% select(ID, !! rlang::sym(forout_reactive$qtlcolnames[4])) %>% `colnames<-`(c("ID", "Intergenic_gene")), by = "ID") 
    forout_reactive$table_intra_inter <- aggregate(Intergenic_gene ~ .,data= interactiontable %>% as.matrix(),FUN=paste0, collapse = ";")
    forout_reactive$table_intra_inter_summarized <- aggregate(Intergenic_gene ~ .,data= interactiontable%>% select(Intragenic_gene, Intergenic_gene) %>% distinct() %>% as.matrix(),FUN=paste0, collapse = ";")
    

    return(forout_reactive$table_intra_inter %>% head(., 20) %>% DT::datatable(., rownames = FALSE, 
                                                                                 caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: Intragenic/intergenic interactions. ", "</b>","<em>","Table containing genes with intragenic SNPs that also affect intergenic genes. This table only shows the first 20 rows of the results, but the full table can be downloaded. The downloadable summarized table contains all unique intragenic/intergenic gene interactions, without the SNP column." , "</em>"))),
                                                                                 options = list(scrollX = TRUE, pageLength = 5, dom = 'tip')))
  }, server = FALSE)
  
  
  # output - phenotable ----  
  output$phenotable <- DT::renderDT({
    req(forout_reactive$table_pheno_processed)
    return(forout_reactive$table_pheno_processed %>% head(., 20) %>% DT::datatable(., rownames = FALSE,
                                                                                    caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: molQTL summary table. ", "</b>","<em>","Table containing the molQTLs after filtering" , "</em>"))),
                                                                                    options = list(scrollX = TRUE, pageLength = 5, dom = 'tip')))
  }, server = FALSE)
  
  
  # output - phenotallytable ----  
  output$phenotallytable <- DT::renderDT({
    req(forout_reactive$table_pheno_processed, forout_reactive$proxylist_pheno)
    
    tallytable = forout_reactive$table_pheno_processed %>% select(!! rlang::sym(forout_reactive$phenocolnames[2])) %>% unique()
    total = forout_reactive$table_pheno_processed %>% group_by(!! rlang::sym(forout_reactive$phenocolnames[2])) %>% tally(name = "n_total")
    
    for(i in 1:length(forout_reactive$proxylist_pheno)){
      tallytable <- tallytable %>% left_join(., forout_reactive$table_pheno_processed %>% filter(!! rlang::sym(forout_reactive$phenocolnames[6]) == forout_reactive$proxylist_pheno[i]) %>% group_by(!! rlang::sym(forout_reactive$phenocolnames[2])) %>% tally(name = paste0("n_",forout_reactive$proxylist_pheno[i])))
    }
    
    tallytable <- tallytable %>% left_join(., total)
    
    forout_reactive$table_pheno_tally <- tallytable %>% mutate_all(funs(tidyr::replace_na(., 0))) %>% arrange(-n_total)
    return(forout_reactive$table_pheno_tally %>% DT::datatable(., rownames = FALSE, caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: molQTL gene tally table. ", "</b>","<em>","A tally of the number of molQTLs per trait. The total number, and the number for each of the provided groupings/proxies is shown. The table is ordered from most prevalent to least prevalent" , "</em>"))),
                                                               options = list(scrollX = TRUE, pageLength = 5, dom = 'tip')))
    
  }, server = FALSE)

  # output - protannotable ----    
  output$protannotable <- DT::renderDT({
    req(forout_reactive$protanno)
    
    if(ncol(forout_reactive$protanno) > 100){
      return(forout_reactive$protanno %>% head(., 100) %>% select(1:100) %>% DT::datatable(., rownames = FALSE, 
                                                        caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: Protein/transcript annotation results. ", "</b>","<em>","Each protein/transcript ID is assigned a unique variable ID (varID) and is annotated with Cell Atlas localizations. Only the first 100 rows are displayed. Please download the table to view all data.","</em>"))),
                                                        options = list(scrollX = TRUE, pageLength = 5, dom = 'tip')))      
    } else {
      return(forout_reactive$protanno %>% head(., 100) %>% DT::datatable(., rownames = FALSE,
                                                        caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: Protein/transcript annotation results. ", "</b>","<em>","Each protein/transcript ID is assigned a unique variable ID (varID) and is annotated with Cell Atlas localizations. Only the first 100 rows are displayed. Please download the table to view all data.","</em>"))),
                                                        options = list(scrollX = TRUE, pageLength = 5, dom = 'tip')))     
    }
    
  }, server = FALSE)
  

  # output - cortable ----    
  output$cortable <- DT::renderDT({
    
    return(forout_reactive$table_complex %>% head(20) %>% select(varID1, varID2, VarVar, everything()) %>% DT::datatable(., rownames = FALSE,
                                                               caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: Correlation results. ", "</b>","<em>","Protein-protein or transcript-transcript correlations are performed using user-specified parameters to discover co-regulated pairs For each correlated pair (p1 & p2) a correlation score (cor), p-value (pval) and q-value (qval) are displayed. The following columns indicate the CORUM complex identifiers, complex names or presence in the database (ComplexID - inCORUM). After that the BioPlex pW score is shown, a column indicating whether the protein pair was found in BioPlex (inBioPlex, top 10 percent), and the STRINGdb linkscore. Finally, the subcellular localizations of the first and second protein/transcript are shown (loc1, loc2), followed by the overlap (overlap.loc) and column indicating whether any localizations where the same (share.loc).","</em>"))),
                                                               options = list(scrollX = TRUE, pageLength = 5, dom = 'tip')) %>% DT::formatRound(columns=c('cor'), digits=2) %>% DT::formatSignif(columns = c('pval', 'qval'), digits = 2))
  }, server = FALSE)
  

    

    
  # analysis - histogram qval on buttonpress ----
  observeEvent(input$summary_bttn, {
    req(forout_reactive$table_complex,  isolate(input$param_summary_cor))
    
    sendSweetAlert(session = session, title = "Creating summary plots", text = "", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
    message("Action: Creating summary plot")
    
    p1 <- ggplot(forout_reactive$table_complex, aes(abs(qval), fill = abs(qval) < isolate(10^(input$param_summary_qval)))) + geom_histogram(bins = 300) + theme(legend.position = c(.95, .30), legend.justification = c("right", "top"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                  panel.background = element_blank(), axis.line = element_line(colour = "black")) + coord_cartesian(expand = FALSE) + labs(fill = "Correlation")  + scale_fill_manual(breaks = c(TRUE, FALSE),values = c("#094183","darkgray"))
    
    
    p2 <- ggplot(forout_reactive$table_complex %>% filter(qval < 0.005), aes(abs(qval), fill = abs(qval) < isolate(10^(input$param_summary_qval)))) + geom_histogram(bins = 100) + annotate("text", x = c(0.0049), y = Inf, vjust = 2.5, hjust = 1, label = c(paste0("Correlated pairs: ", (abs(forout_reactive$table_complex$qval) < isolate(10^(input$param_summary_qval))) %>% sum %>% formatC(., format = "e", digits = 2), "\n Not correlated: ", (abs(forout_reactive$table_complex$qval) > isolate(10^(input$param_summary_qval))) %>% sum %>% formatC(., format = "e", digits = 2)))) + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none", legend.box.just = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + coord_cartesian(expand = FALSE)  + scale_fill_manual(breaks = c(TRUE, FALSE),values = c("#094183","darkgray"))
    layout <- c(area(t = 1, l = 1, b = 6, r = 6), area(t = 1, l = 3, b = 3, r = 6))
    
    # Correlation q-value histogram
    forout_reactive$plot_summary_histogram_qval <- p1 + p2 + plot_layout(design = layout)
    
    
    p1 <- ggplot(forout_reactive$table_complex, aes(abs(pval))) + geom_histogram(bins = 300) + theme(legend.position = c(.95, .30), legend.justification = c("right", "top"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + coord_cartesian(expand = FALSE) + labs(fill = "Correlation")
    
    p2 <- ggplot(forout_reactive$table_complex %>% filter(pval < 0.005), aes(abs(pval))) + geom_histogram(bins = 100)  + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none", legend.box.just = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + coord_cartesian(expand = FALSE)  + scale_fill_manual(breaks = c(TRUE, FALSE),values = c("#094183","darkgray"))
    layout <- c(area(t = 1, l = 1, b = 6, r = 6), area(t = 1, l = 3, b = 3, r = 6))
    
    # Correlation p-value histogram
    forout_reactive$plot_summary_histogram_pval <- p1 + p2 + plot_layout(design = layout)
    
    # Correlation coefficient histogram
    forout_reactive$plot_summary_histogram_cor <- ggplot(forout_reactive$table_complex, aes(cor, fill = cor > isolate(input$param_summary_cor[2]) | cor < isolate(input$param_summary_cor[1]))) + geom_histogram(bins = 200) + geom_vline(xintercept = c(isolate(input$param_summary_cor[1]), isolate(input$param_summary_cor[2])), colour = "darkgray", linetype = "dashed")  + annotate("text", x = c(0.9), y = Inf, vjust = 2.5, hjust = 1, label = c(paste0("Correlated pairs: ", (forout_reactive$table_complex$cor > isolate(input$param_summary_cor[2]) | forout_reactive$table_complex$cor < isolate(input$param_summary_cor[1])) %>% sum %>% formatC(., format = "e", digits = 2), "\n Not correlated: ", (!(forout_reactive$table_complex$cor > isolate(input$param_summary_cor[2]) | forout_reactive$table_complex$cor < isolate(input$param_summary_cor[1]))) %>% sum %>% formatC(., format = "e", digits = 2)))) + theme(legend.position = c(.95, .30), legend.justification = c("right", "top"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + coord_cartesian(expand = FALSE) + labs(fill = "Correlation")  + scale_fill_manual(breaks = c(TRUE, FALSE),values = c("#094183","darkgray"))
    
    
    
    
    #Interaction pairs of correlated proteins > This should be done before left-join with corum as it introduces duplicates
    paircount <- forout_reactive$table_complex %>% filter(qval < 10^(input$param_summary_qval)) %>% filter(cor > input$param_summary_cor[2]) %>% dplyr::distinct(., VarVar) %>% tidyr::separate(., VarVar, sep = "_", into = c("v1","v2")) %>% as.matrix() %>% as.vector() %>% table() %>% reshape2::melt() %>% `colnames<-`(c("ID", "value"))
    
    if(nrow(paircount) == 0){sendSweetAlert(session = session, title = "Error, dataset contains 0 rows after filtering", text = "Please select less stringent cut-offs", type = "error")}
    validate(need(nrow(paircount)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    
    
    paircount <- forout_reactive$protanno %>% dplyr::select(varID, ID) %>% left_join(., paircount, by = c("ID" = "ID"))
    
    paircount <- paircount %>% mutate(paircount_bin = case_when(is.na(value) == TRUE ~ "0",
                                                                value > 0 & value < 6 ~ "1-5",
                                                                value > 5 & value < 21 ~ "6-20",
                                                                value > 20 & value < 51 ~ "21-50",
                                                                value > 50 & value < 101 ~ "51-100",
                                                                value > 100 ~ ">100",
                                                                TRUE ~ "missing"))
    
    paircount <- paircount %>% mutate(paircount_bin = factor(paircount_bin, levels = c(">100","51-100","21-50","6-20","1-5","0","missing")))
    
    
    forout_reactive$plot_summary_paircount <- ggplot(paircount, aes(y = paircount_bin)) + geom_histogram(stat = "count", width = 0.6, fill = "darkgray") + 
      geom_text(stat = "count" ,aes(label = paircount_bin), hjust = -0.5, position = position_dodge(width = 1)) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), axis.text.y = element_blank()) + 
      labs(x = "", y = "Co-regulation partners per protein/transcript")
    
    
    sendSweetAlert(session = session, title = "Summary plots created", text = "Rendering now...", type = "success")
    
    updateProgressBar(session = session, id = "pb5", value = 100)
    
  })
  

  


  # Analysis - databases ----
  observeEvent(input$database_bttn, {
    
    # Analysis - inCORUM ----
    req(forout_reactive$table_complex, input$param_database_cor, input$param_database_qval)
    
    df_filter <- forout_reactive$table_complex %>% mutate(correlated = cor > input$param_database_cor & qval < 10^(input$param_database_qval))
    
    if(nrow(df_filter %>% filter(correlated == TRUE)) ==0){sendSweetAlert(session = session, title = "There are no correlated rows in the dataset after filtering", text = "Please select less stringent cut-offs", type = "error")}
    validate(need(nrow(df_filter %>% filter(correlated == TRUE))!=0, "There are no correlated rows in the dataset. Try removing or relaxing one or more filters."))
    
    if(nrow(df_filter %>% filter(correlated == FALSE)) ==0){sendSweetAlert(session = session, title = "Error, all rows are 'correlated'", text = "Please select more stringent cut-offs", type = "error")}
    validate(need(nrow(df_filter %>% filter(correlated == FALSE))!=0, "There are no non-correlated rows in the dataset. Please select more stringent cut-offs."))
    
    
    sendSweetAlert(session = session, title = "Creating database plots", text = "", type = "success")
    message("Action: Creating database plot")
    
    # Analysis - inCORUM ----
    chisq <- matrix(c(df_filter %>% filter(correlated == TRUE & inCORUM == TRUE) %>% nrow(),
                      df_filter %>% filter(correlated == TRUE & inCORUM == FALSE) %>% nrow() ,
                      df_filter %>% filter(correlated == FALSE & inCORUM == TRUE) %>% nrow() ,
                      df_filter %>% filter(correlated == FALSE & inCORUM == FALSE) %>% nrow() ), 
                    nrow = 2, 
                    dimnames = list(inCORUM = c("CORUM", "Unknown"),
                                    Correlated = c("Correlated", "Not-Correlated")))
    
    chisq_result <- chisq.test(chisq)
    if(chisq_result$p.value < 0.001){chisq_result$p.value <- "<0.001"}
    chisq_annotation <- (paste0("p-value: ",chisq_result$p.value %>% formatC(., format = "e", digits = 2),"  |  fold change: ", ((chisq[1,1] / chisq[2,1]) / (chisq[1,2] / chisq[2,2])) %>% round(., digits = 1)))
    
    chisqplot <- data.frame( c("TRUE", "FALSE"), c((chisq[1,1] / chisq[2,1]) * 100, (chisq[1,2] / chisq[2,2]) * 100)) %>% `colnames<-`(c("Correlation", "value"))
    forout_reactive$plot_database_inCORUM <- ggplot(chisqplot, aes(x = Correlation, y = value)) + geom_bar(stat = "identity", fill = "darkgray", width = 0.6) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Co-regulated", y = "Correlated pairs found in CORUM (%)") + ggpubr::stat_pvalue_manual(data.frame("FALSE", "TRUE",chisq_annotation) %>% `colnames<-`(c("group1", "group2", "p")), y.position = max(chisqplot$value) * 1.03, tip.length = 0)
    
    # Analysis - inBioplex ----
    chisq <- matrix(c(df_filter %>% filter(correlated == TRUE & inBioplex == TRUE) %>% nrow(),
                      df_filter %>% filter(correlated == TRUE & inBioplex == FALSE) %>% nrow() ,
                      df_filter %>% filter(correlated == FALSE & inBioplex == TRUE) %>% nrow() ,
                      df_filter %>% filter(correlated == FALSE & inBioplex == FALSE) %>% nrow() ), 
                    nrow = 2, 
                    dimnames = list(inBioplex = c("Bioplex", "Unknown"),
                                    Correlated = c("Correlated", "Not-Correlated")))
    
    chisq_result <- chisq.test(chisq)
    if(chisq_result$p.value < 0.001){chisq_result$p.value <- "<0.001"}
    chisq_annotation <- (paste0("p-value: ",chisq_result$p.value %>% formatC(., format = "e", digits = 2),"  |  fold change: ", ((chisq[1,1] / chisq[2,1]) / (chisq[1,2] / chisq[2,2])) %>% round(., digits = 1)))
    
    chisqplot <- data.frame( c("TRUE", "FALSE"), c((chisq[1,1] / chisq[2,1]) * 100, (chisq[1,2] / chisq[2,2]) * 100)) %>% `colnames<-`(c("Correlation", "value"))
    
    forout_reactive$plot_database_inBioplex <- ggplot(chisqplot, aes(x = Correlation, y = value)) + geom_bar(stat = "identity", fill = "darkgray", width = 0.6) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Co-regulated", y = "Correlated pairs found in BioPlex 3.0 (%)") + ggpubr::stat_pvalue_manual(data.frame("FALSE", "TRUE",chisq_annotation) %>% `colnames<-`(c("group1", "group2", "p")), y.position = max(chisqplot$value) * 1.03, tip.length = 0)
    
    # Analysis - shareloc ----
    chisq <- matrix(c(df_filter %>% filter(correlated == TRUE & share.loc == TRUE) %>% nrow(),
                      df_filter %>% filter(correlated == TRUE & share.loc == FALSE) %>% nrow() ,
                      df_filter %>% filter(correlated == FALSE & share.loc == TRUE) %>% nrow() ,
                      df_filter %>% filter(correlated == FALSE & share.loc == FALSE) %>% nrow() ), 
                    nrow = 2, 
                    dimnames = list(shareloc = c("Shared_Location", "Not_shared"),
                                    Correlated = c("Correlated", "Not-Correlated")))
    
    
    chisq_result <- chisq.test(chisq)
    if(chisq_result$p.value < 0.001){chisq_result$p.value <- "<0.001"}
    chisq_annotation <- (paste0("p-value: ",chisq_result$p.value %>% formatC(., format = "e", digits = 2),"  |  fold change: ", ((chisq[1,1] / chisq[2,1]) / (chisq[1,2] / chisq[2,2])) %>% round(., digits = 1)))
    
    chisqplot <- data.frame( c("TRUE", "FALSE"), c((chisq[1,1] / chisq[2,1]) * 100, (chisq[1,2] / chisq[2,2]) * 100)) %>% `colnames<-`(c("Correlation", "value"))
    
    forout_reactive$plot_database_shareloc <- ggplot(chisqplot, aes(x = Correlation, y = value)) + geom_bar(stat = "identity", fill = "darkgray", width = 0.6) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Co-regulated", y = "Correlated pairs with shared localization (%)") + ggpubr::stat_pvalue_manual(data.frame("FALSE", "TRUE",chisq_annotation) %>% `colnames<-`(c("group1", "group2", "p")), y.position = max(chisqplot$value) * 1.03, tip.length = 0)
    
    #StringDB linkplot
    forout_reactive$plot_database_stringdb <- ggplot(data = forout_reactive$table_complex, aes(x = cor, y = linkscore)) + geom_smooth(method = "gam") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Correlation", y = "STRINGdb Linkscore") + scale_fill_gradient(low="white",high="darkblue")
    
    updateProgressBar(session = session, id = "pb6", value = 100)
  })    
  
  
  #### Render plots ----
  output$linkscore <- renderPlot({
    req(forout_reactive$plot_database_stringdb)
    forout_reactive$plot_database_stringdb})
  output$inCORUM <- renderPlot({
    req(forout_reactive$plot_database_inCORUM)
    forout_reactive$plot_database_inCORUM})
  output$inBioplex <- renderPlot( {
    req(forout_reactive$plot_database_inBioplex)
    forout_reactive$plot_database_inBioplex})
  output$shareloc <- renderPlot( {
    req(forout_reactive$plot_database_shareloc)
    forout_reactive$plot_database_shareloc})
  output$plot_manhattan <- renderPlot( {
    req(forout_reactive$plot_manhattan)
    forout_reactive$plot_manhattan})  
  output$plot_edge <- renderPlot( {
    req(forout_reactive$plot_edge)
    forout_reactive$plot_edge})  
  output$plot_arcdiagram <- renderPlot( {
    req(forout_reactive$plot_arcdiagram)
    forout_reactive$plot_arcdiagram}) 
  output$network_plot_interactive <- renderForceNetwork({
    req(forout_reactive$interactive_plot_network)
    forout_reactive$interactive_plot_network})
  output$paircount <- renderPlot( {
    req(forout_reactive$plot_summary_paircount)
    forout_reactive$plot_summary_paircount})   
  output$histogramqval <- renderPlot( {
    req(forout_reactive$plot_summary_histogram_qval)
    forout_reactive$plot_summary_histogram_qval})  
  output$histogrampval <- renderPlot( {
    req(forout_reactive$plot_summary_histogram_pval)
    forout_reactive$plot_summary_histogram_pval})  
  output$histogramcor <- renderPlot( {
    req(forout_reactive$plot_summary_histogram_cor)
    forout_reactive$plot_summary_histogram_cor}) 
  output$sensitivityplot <- renderPlot( {
    req(forout_reactive$plot_sensitivity)
    forout_reactive$plot_sensitivity}) 
  
  
  
  output$resultbox_cor_ui <- renderUI({
    req(forout_reactive$plot_summary_histogram_qval, forout_reactive$plot_summary_histogram_cor, forout_reactive$plot_summary_paircount)
    
    tabBox(title = "Summary plots", width = 12,
           tabPanel("Correlation histogram", plotOutput("histogramcor") %>% withSpinner(), HTML(paste("<b>", "Figure: Co-regulated protein/transcript pairs. ", "</b>","<em>", "The number of co-regulated pairs after filtering for user-specified correlation coefficient cut-offs. The number of protein/transcript pairs that meets the criteria is displayed.", "</em>")) ),
           tabPanel("q-value histogram", plotOutput("histogramqval") %>% withSpinner(), HTML(paste("<b>", "Figure: Co-regulated protein/transcript pairs. ", "</b>","<em>", "The number of co-regulated pairs after filtering for user-specified q-value cut-offs. The number of protein/transcript pairs that meets the criteria is displayed.", "</em>")) ),
           tabPanel("p-value histogram", plotOutput("histogrampval") %>% withSpinner(), HTML(paste("<b>", "Figure: Co-regulated protein/transcript pairs. ", "</b>","<em>", "The number of co-regulated pairs after filtering for user-specified p-value cut-offs. The number of protein/transcript pairs that meets the criteria is displayed.", "</em>")) ),
           tabPanel("Correlated partners", plotOutput("paircount") %>% withSpinner(), HTML(paste("<b>", "Figure: Co-regulation partners per protein/transcript ", "</b>","<em>", "The number of co-regulation partners per protein/transcript has been determined based on the user-specified correlation coefficient and q-value cut-offs. The data is binned to group proteins/transcripts with 0, 1-5, 6-20, 21-50, 51-100 and >100 partners.", "</em>")) ))
  })
  
  output$resultbox_db_ui <- renderUI({
    req(forout_reactive$plot_database_stringdb, forout_reactive$plot_database_inCORUM, forout_reactive$plot_database_inBioplex, forout_reactive$plot_database_shareloc)
    
    tabBox(title = "Enrichment analysis", width = 12,
           tabPanel("CORUM", plotOutput("inCORUM") %>% withSpinner(), HTML(paste("<b>", "Figure: Protein-protein interaction enrichment. ", "</b>","<em>", "Enrichment of previously reported protein-protein interactions as reported in the CORUM database. A Chi-squared test was performed to determine the significance of enrichments.", "</em>")) ),
           tabPanel("BioPlex 3.0", plotOutput("inBioplex") %>% withSpinner(), HTML(paste("<b>", "Figure: Protein-protein interaction enrichment. ", "</b>","<em>", "Enrichment of previously reported protein-protein interactions as reported in Bioplex 3.0 database. A Chi-squared test was performed to determine the significance of enrichments.", "</em>")) ),
           tabPanel("STRINGdb", plotOutput("linkscore") %>% withSpinner(), HTML(paste("<b>", "Figure: Protein-protein interaction enrichment. ", "</b>","<em>", "Enrichment of previously reported protein-protein interactions as reported in the STRING database.", "</em>")) ),
           tabPanel("Localization", plotOutput("shareloc") %>% withSpinner(), HTML(paste("<b>", "Figure: Shared localization of protein pairs. ", "</b>","<em>", "Enrichment of protein pair co-localization was determined based on Cell Atlas localization determined by immunofluorescent staining. A Chi-squared test was performed to determine the significance of enrichments.", "</em>")) ),
           tabPanel("Top Corum complexes", DT::DTOutput("complextable"), downloadButton("download_corumcomplextable","Download table")) )
    
    
    })
  
  # output - results - complextable ----    
  output$complextable <- DT::renderDT({
    
    req(forout_reactive$table_complex, input$param_database_cor)
    forout_reactive$table_complex_tally <- forout_reactive$table_complex %>% filter(cor > input$param_database_cor & qval < 10^(input$param_database_qval)) %>% filter(ComplexName != "NA" & is.null(ComplexName) == FALSE) %>% group_by(ComplexName) %>% tally %>% arrange(-n) %>% mutate(ComplexName = ComplexName %>% sub(" \\(.*", "", .))
    
    return((forout_reactive$table_complex_tally %>% DT::datatable(., rownames = FALSE, caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;','Table: ', htmltools::em('Top CORUM protein complexes. Tally of CORUM complex names among protein pairs with correlation above user-specified cut-off.')), options = list(scrollX = TRUE, pageLength = 10, dom = 'tip'))) )
    
  })
  
  
  
  # Analysis - network_plot ----
  observeEvent(input$network_bttn, {
    
    req(forout_reactive$table_complex, input$network_complexselect, input$network_qtlselect, input$network_edgeselect, input$network_qtlsummarize, input$param_network_cor, input$param_network_qval, input$prottype_network)
    
    sendSweetAlert(session = session, title = "Creating network plot", text = "", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
    message("Action: Creating network plot")

    if(input$network_complexselect == "All proteins"){
      
      nw_edge <- forout_reactive$table_complex %>% filter((cor > input$param_network_cor[2] | cor < input$param_network_cor[1]) & qval < 10^(input$param_network_qval)) %>% select(varID1, varID2) %>% mutate(connection = "Protein-protein interaction", datatype = "protein")
      
    } else if(input$prottype_network == "all"){
      
      nw_edge <- forout_reactive$table_complex %>% filter((cor > input$param_network_cor[2] | cor < input$param_network_cor[1]) & qval < 10^(input$param_network_qval)) %>% select(varID1, varID2) %>% filter((varID1 %in% input$network_complexselect) | (varID2 %in% input$network_complexselect)) %>% mutate(connection = "Protein-protein interaction", datatype = "protein")
      
    } else if(input$network_complexselect == "All proteins with QTLs"){
      
      req(forout_reactive$table_qtl_processed)
      nw_edge <- forout_reactive$table_complex %>% filter((cor > input$param_network_cor[2] | cor < input$param_network_cor[1]) & qval < 10^(input$param_network_qval)) %>% filter(varID1 %in% (forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unique %>% unlist %>% as.vector() %>% tolower) | varID2 %in% (forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unique %>% unlist %>% as.vector() %>% tolower) ) %>% select(varID1, varID2) %>% mutate(connection = "Protein-protein interaction", datatype = "protein")
      
    } else if (input$prottype_network == "corum" & (input$network_complexselect %in% c("All complexes"))){
      
      nw_edge <- forout_reactive$table_complex %>% filter((cor > input$param_network_cor[2] | cor < input$param_network_cor[1]) & qval < 10^(input$param_network_qval) & inCORUM == TRUE) %>% select(varID1, varID2) %>% mutate(connection = "Protein-protein interaction", datatype = "protein")
      
    } else if (input$prottype_network == "corum" & !(input$network_complexselect %in% c("All complexes"))){
      
      nw_edge <- forout_reactive$table_complex %>% filter((cor > input$param_network_cor[2] | cor < input$param_network_cor[1]) & qval < 10^(input$param_network_qval) & inCORUM == TRUE  & ComplexName == input$network_complexselect) %>% select(varID1, varID2) %>% mutate(connection = "Protein-protein interaction", datatype = "protein")
      
    } else if (input$prottype_network == "bioplex" & (input$network_complexselect %in% c("All complexes"))){
      
      bioplextarget <- db_bioplex3_4pc %>% select(SymbolA, SymbolB) %>% c(.$SymbolA, .$SymbolB) %>% unlist %>% as.vector() %>% tolower
      nw_edge <- forout_reactive$table_complex %>% filter((cor > input$param_network_cor[2] | cor < input$param_network_cor[1]) & qval < 10^(input$param_network_qval)) %>% filter(varID1 %in% bioplextarget | varID2 %in% bioplextarget) %>% select(varID1, varID2) %>% mutate(connection = "Protein-protein interaction", datatype = "protein")
      
    } else if (input$prottype_network == "bioplex" & !(input$network_complexselect %in% c("All complexes"))){
      
      bioplextarget <- db_bioplex3_4pc %>% filter(SymbolA == input$network_complexselect | SymbolB == input$network_complexselect) %>% select(SymbolA, SymbolB) %>% c(.$SymbolA, .$SymbolB) %>% unlist %>% as.vector() %>% tolower
      nw_edge <- forout_reactive$table_complex %>% filter((cor > input$param_network_cor[2] | cor < input$param_network_cor[1]) & qval < 10^(input$param_network_qval)) %>% filter(varID1 %in% bioplextarget | varID2 %in% bioplextarget) %>% select(varID1, varID2) %>% mutate(connection = "Protein-protein interaction", datatype = "protein")
      
    }
    
    # Filter nw_edge by organelles in input$select_organelle
    if(length(input$select_organelle > 0)){
      
      # Take the organelle filter and replace the string "NA" with NA
      select_organelle <- input$select_organelle
      select_organelle[ select_organelle == "NA" ] <- NA
      
      # Create a list of proteins that should be retained
      # unlist CP_loc and check whether any entry is present in select_organelle
      # filter nw_edge by the created genelist
      select_organelle_genes <- forout_reactive$protanno %>% select(ID, CP_loc) %>% rowwise() %>% filter(strsplit(CP_loc, ";") %>% unlist %in% select_organelle %>% any()) %>% select(ID) %>% unlist %>% unique
      
      nw_edge <- nw_edge %>% filter(varID1 %in% select_organelle_genes & varID2 %in% select_organelle_genes)
      
    }

    

    if(nrow(nw_edge) == 0){sendSweetAlert(session = session, title = "Error, dataset contains 0 rows after filtering", text = "Please select less stringent cut-offs", type = "error")}
    validate(need(nrow(nw_edge)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    
  
    #abab nw dgidb 
    if(input$network_dgi != "None"){
      
      if(input$network_dgi == "All DGIdb interactions"){
        drug <- db_dgidb %>% select(drug_name, gene_name)
        } else {
        drug <- db_dgidb %>% filter(dgi_interaction_claim_source == input$network_dgi %>% sub(" .*", "", .)) %>% select(drug_name, gene_name)
      }
        
      nw_edge <- rbind(nw_edge, drug %>% filter(gene_name %in% (c(nw_edge$varID1, nw_edge$varID2) %>% unique %>% tolower())) %>% mutate(connection = "Drug-gene interaction", datatype = "Drug-gene interaction") %>% `colnames<-`(colnames(nw_edge)) )
      }
    
    
    
    if(input$network_edgeselect == "Proxy"){
      selection = forout_reactive$qtlcolnames[9]
    } else if(input$network_edgeselect == "Intragenic"){
      selection = "CP_Intragenic_QTL"
    } else if(input$network_edgeselect == "Variant Effect"){
      selection = "CP_Variant_Effect"
    } else if(input$network_edgeselect == "Variant Impact"){
      selection = "CP_Variant_Impact"
    }
      
    
    #plotting
    if((input$network_qtlselect == "pqtl") & (is.null(forout_reactive$table_qtl_processed) == FALSE)){
      
      if(input$network_qtlsummarize == "individual"){
        nw_qtl <- forout_reactive$table_qtl_processed %>% filter(tolower(!! rlang::sym(forout_reactive$qtlcolnames[4])) %in% (c(nw_edge$varID1, nw_edge$varID2) %>% unique)) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(selection)) %>% `colnames<-`(c("varID1", "varID2", "connection")) %>% mutate(datatype = "SNP", varID1 = tolower(varID1), varID2 = tolower(varID2))
      
      } else if(input$network_qtlsummarize == "LD"){
        # Select same columns as individual but add LD_name, then use case_when to mutate snps in LD blocks, them remove LD_name col
        nw_qtl <- forout_reactive$table_qtl_processed %>% filter(tolower(!! rlang::sym(forout_reactive$qtlcolnames[4])) %in% (c(nw_edge$varID1, nw_edge$varID2) %>% unique)) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(selection), LD_name) %>% `colnames<-`(c("varID1", "varID2", "connection", "LD_name")) %>% mutate(varID1 = case_when(is.na(LD_name) ~ as.character(varID1), TRUE ~ LD_name)) %>% select(-LD_name) %>% mutate(datatype = "SNP", varID1 = tolower(varID1), varID2 = tolower(varID2))
      } else {
        nw_qtl <- forout_reactive$table_qtl_processed %>% filter(tolower(!! rlang::sym(forout_reactive$qtlcolnames[4])) %in% (c(nw_edge$varID1, nw_edge$varID2) %>% unique)) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[3]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(selection)) %>% `colnames<-`(c("varID1", "varID2", "connection")) %>% mutate(datatype = "SNP", varID1 = paste0("Chr_",varID1), varID2 = tolower(varID2))
      }
      
      nw_vertices <- c(nw_edge$varID1, nw_edge$varID2, nw_qtl$varID1, nw_qtl$varID2) %>% as.data.frame() %>% `colnames<-`(c("var")) %>% group_by(var) %>% summarize(n=n()) %>% mutate(nodetype = case_when(var %in% nw_qtl$varID1 == TRUE ~ "SNP", var %in% db_dgidb$drug_name == TRUE ~ "Drug", var %in% nw_qtl$varID1 == FALSE ~ "Protein"))
      nw_edge <- bind_rows(nw_qtl, nw_edge)
      

      
    } else if((input$network_qtlselect == "pqtlmqtl") & (is.null(forout_reactive$table_qtl_processed) == FALSE) & (is.null(forout_reactive$table_pheno_processed) == FALSE)){
      
      if(input$network_qtlsummarize == "individual"){
        nw_pqtl <- forout_reactive$table_qtl_processed %>% filter(tolower(!! rlang::sym(forout_reactive$qtlcolnames[4])) %in% (c(nw_edge$varID1, nw_edge$varID2) %>% unique)) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(selection)) %>% `colnames<-`(c("varID1", "varID2", "connection")) %>% mutate(datatype = "SNP", varID1 = tolower(varID1), varID2 = tolower(varID2))
        nw_mqtl <- forout_reactive$table_pheno_processed %>% filter(tolower(!! rlang::sym(forout_reactive$phenocolnames[1])) %in% (c(nw_pqtl$varID1, nw_pqtl$varID2) %>% unique)) %>% select(!! rlang::sym(forout_reactive$phenocolnames[1]), !! rlang::sym(forout_reactive$phenocolnames[2]), !! rlang::sym(forout_reactive$phenocolnames[6])) %>% `colnames<-`(c("varID1", "varID2", "connection")) %>% mutate(datatype = "Phenotype", varID1 = tolower(varID1))
        nw_qtl <- bind_rows(nw_pqtl, nw_mqtl)
      } else if(input$network_qtlsummarize == "LD"){
        # Use the same code as the "individual SNPs" but replace rsIDs with LD names where possible
        # Copy the snp-ld mapping from nw_pqtl and join this to nw_mqtl, then overwrite rsIDs with LD_block names
        nw_pqtl <- forout_reactive$table_qtl_processed %>% filter(tolower(!! rlang::sym(forout_reactive$qtlcolnames[4])) %in% (c(nw_edge$varID1, nw_edge$varID2) %>% unique)) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(selection), LD_name) %>% `colnames<-`(c("varID1", "varID2", "connection", "LD_name")) %>% mutate(varID1 = case_when(is.na(LD_name) ~ as.character(varID1), TRUE ~ LD_name)) %>% select(-LD_name) %>% mutate(datatype = "SNP", varID1 = tolower(varID1), varID2 = tolower(varID2))
        
        ld_table <- forout_reactive$table_qtl_processed %>% filter(tolower(!! rlang::sym(forout_reactive$qtlcolnames[4])) %in% (c(nw_edge$varID1, nw_edge$varID2) %>% unique)) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[1]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(selection), LD_name) %>% `colnames<-`(c("varID1", "varID2", "connection", "LD_name")) %>% select(varID1, LD_name)
        
        nw_mqtl <- forout_reactive$table_pheno_processed %>% filter(tolower(!! rlang::sym(forout_reactive$phenocolnames[1])) %in% (c(nw_pqtl$varID1, nw_pqtl$varID2) %>% unique)) %>% select(!! rlang::sym(forout_reactive$phenocolnames[1]), !! rlang::sym(forout_reactive$phenocolnames[2]), !! rlang::sym(forout_reactive$phenocolnames[6])) %>% `colnames<-`(c("varID1", "varID2", "connection")) %>% mutate(datatype = "Phenotype", varID1 = tolower(varID1)) %>% left_join(., ld_table) %>% mutate(varID1 = case_when(is.na(LD_name) ~ as.character(varID1), TRUE ~ LD_name)) %>% select(-LD_name)
        nw_qtl <- bind_rows(nw_pqtl, nw_mqtl)
        
      } else {
        nw_pqtl <- forout_reactive$table_qtl_processed %>% filter(tolower(!! rlang::sym(forout_reactive$qtlcolnames[4])) %in% (c(nw_edge$varID1, nw_edge$varID2) %>% unique)) %>% select(!! rlang::sym(forout_reactive$qtlcolnames[3]), !! rlang::sym(forout_reactive$qtlcolnames[4]), !! rlang::sym(selection)) %>% `colnames<-`(c("varID1", "varID2", "connection")) %>% mutate(datatype = "SNP", varID1 = paste0("Chr_",varID1), varID2 = tolower(varID2))
        nw_mqtl <- forout_reactive$table_pheno_processed %>% filter(tolower(!! rlang::sym(forout_reactive$phenocolnames[4])) %in% (c(nw_pqtl$varID1, nw_pqtl$varID2) %>% sub("Chr_","",.) %>% unique)) %>% select(!! rlang::sym(forout_reactive$phenocolnames[4]), !! rlang::sym(forout_reactive$phenocolnames[2]), !! rlang::sym(forout_reactive$phenocolnames[6])) %>% `colnames<-`(c("varID1", "varID2", "connection")) %>% mutate(datatype = "Phenotype", varID1 = paste0("Chr_",varID1))
        nw_qtl <- bind_rows(nw_pqtl, nw_mqtl)
      }
      
      nw_vertices <- c(nw_edge$varID1, nw_edge$varID2, nw_qtl$varID1, nw_qtl$varID2) %>% as.data.frame() %>% `colnames<-`(c("var")) %>% group_by(var) %>% summarize(n=n()) %>% mutate(nodetype = case_when(var %in% nw_mqtl$varID2 == TRUE ~ "Phenotype", var %in% nw_pqtl$varID1 == TRUE ~ "SNP", var %in% db_dgidb$drug_name == TRUE ~ "Drug", var %in% nw_pqtl$varID1 == FALSE ~ "Protein"))
      nw_edge <- bind_rows(nw_qtl, nw_edge)
      
    } else {
      
      nw_vertices <- c(nw_edge$varID1, nw_edge$varID2) %>% as.data.frame() %>% `colnames<-`(c("var")) %>% group_by(var) %>% summarize(n=n()) %>% mutate(connection = "Protein-protein interaction") %>% mutate(nodetype = case_when(var %in% db_dgidb$drug_name == TRUE ~ "Drug", TRUE ~ "Protein"))
      }
 
    
    # First check the number of connections in the data
    # If there are more than 10,000 > refuse plotting, filter data again
    # If > 2000 and < 10,000  > Refuse rendering, but allow plot download
    # If < 2000   > Render and download available
    if(nrow(nw_vertices) > 10000){
      sendSweetAlert(session = session, title = "Error, too many network nodes to create the plot (>10,000)", text = "Please select a stricter correlation cut-off", type = "error")
    } else {
      
      F2 <- colorRampPalette(c("#FFAE42", "#094183"), bias = nrow(nw_edge), space = "rgb", interpolate = "linear")
      colCodes <- F2(length(unique(nw_edge$connection)))
      nw_edge <- nw_edge %>% mutate(edgecol = sapply(nw_edge$connection, function(x) colCodes[which(sort(unique(nw_edge$connection)) == x)])) %>% group_by(varID1, varID2) %>% distinct() %>% ungroup()
    
    ###Making the interactive plot
    nodes <- nw_vertices %>% mutate(numid = 0:(nrow(nw_vertices)-1))
    links <- nw_edge %>% left_join(., nodes %>% select(var, numid) %>% `colnames<-`(c("varID1", "source"))) %>% left_join(., nodes %>% select(var, numid) %>% `colnames<-`(c("varID2", "target"))) %>% select(source, target, varID1, varID2, everything())
    
    if(input$nodelabelsnw == "None"){nodes <- nodes %>% mutate(var = NA)}
    if(input$nodelabelsnw == "No SNP labels"){nodes <- nodes %>% mutate(var = case_when(nodetype %in% c("SNP") ~ "", TRUE ~ as.character(nodes$var)))}
    

    # if more than 2000, don't render, but allow download
    if(nrow(nw_vertices) > 2000){
      sendSweetAlert(session = session, title = "Too many network nodes to render the plot (>2000), but download available", text = "Please select a stricter correlation cut-off, or directly download the network", type = "info")
      forout_reactive$interactive_plot_network_dl <-  forceNetwork(Links = links, Nodes = nodes, Source = 'source', Target = 'target', NodeID = 'var', Nodesize = 'n', Group = 'nodetype', charge = input$nodechargenw, linkColour = links$edgecol, opacity = 1, fontSize = 12, zoom = TRUE, opacityNoHover = 0.6, legend = TRUE)
    
      forout_reactive$network_links <- links
      updateProgressBar(session = session, id = "pb7", value = 100)
      
      } else {
        
      forout_reactive$interactive_plot_network_dl <-  forceNetwork(Links = links, Nodes = nodes, Source = 'source', Target = 'target', NodeID = 'var', Nodesize = 'n', Group = 'nodetype', charge = input$nodechargenw, linkColour = links$edgecol, opacity = 1, fontSize = 12, zoom = TRUE, opacityNoHover = 0.6, legend = TRUE)
      forout_reactive$interactive_plot_network <-  forceNetwork(Links = links, Nodes = nodes, Source = 'source', Target = 'target', NodeID = 'var', Nodesize = 'n', Group = 'nodetype', charge = input$nodechargenw, linkColour = links$edgecol, opacity = 1, fontSize = 12, zoom = TRUE, opacityNoHover = 0.6, legend = TRUE)
      
      forout_reactive$network_links <- links
      updateProgressBar(session = session, id = "pb7", value = 100)
      sendSweetAlert(session = session, title = "Network rendered", text = "", type = "success")
      
      
      }
    
    }
      
    
    
  })
  
  # Analysis - network_plot_interactive ----
  output$network_legend <- renderPlot( {
    req(forout_reactive$network_links, forout_reactive$interactive_plot_network)
    
    linklegend <- forout_reactive$network_links %>% select(connection, edgecol) %>% distinct()
    legend <- ggplot(linklegend, aes(x = connection, y = 1, col = edgecol)) + geom_line(size = 2.5) + theme_bw() + scale_color_manual(name = "Link type", values = linklegend$edgecol %>% as.character(), labels = linklegend$connection) +theme(legend.position="bottom", legend.text=element_text(size=15), legend.title = element_text(size=15))
    legend <- ggpubr::get_legend(legend)
    
    ggpubr::as_ggplot(legend)
    
  })
  
  
  # pQTL Gene / SNP location plot ----
  output$qtl_snploc <- renderPlot( {

    req(forout_reactive$table_qtl_processed)
    
    forout_reactive$plot_qtl_snploc <- cp_pqtl_locplot(forout_reactive$table_qtl_processed)
    
    forout_reactive$plot_qtl_snploc
    
  })
  

  # Annotation - qtl_proxy_donut ----
  output$qtl_proxy_donut <- renderPlot( {
      
      req(forout_reactive$table_qtl_processed)
      
      forout_reactive$plot_qtl_proxydonut <- cp_donut_plot(forout_reactive$table_qtl_processed, "proxy")
      
      forout_reactive$plot_qtl_proxydonut
      
    })
    
  # Annotation - qtl_anno_donut ----
  output$qtl_anno_donut <- renderPlot( {
      
      req(forout_reactive$qtl_annotated == TRUE)

      forout_reactive$plot_qtl_annodonut <- cp_donut_plot(forout_reactive$table_qtl_processed, "ve")
      
      forout_reactive$plot_qtl_annodonut
      
    }) 
  
  # Annotation - qtl_impact_donut ----
  output$qtl_impact_donut <- renderPlot( {
    
    req(forout_reactive$qtl_annotated == TRUE)
    
    forout_reactive$plot_qtl_impactdonut <- cp_donut_plot(forout_reactive$table_qtl_processed, "impact")
    
    forout_reactive$plot_qtl_impactdonut
    
  }) 
  
  # Annotation - pheno_proxy_donut ----
  output$pheno_proxy_donut <- renderPlot( {
    
    req(forout_reactive$table_pheno_processed)
    
    proxydonut <- forout_reactive$table_pheno_processed %>% select(!! rlang::sym(forout_reactive$phenocolnames[1]), !! rlang::sym(forout_reactive$phenocolnames[6]))
    
    proxydonut <- proxydonut %>% group_by(!! rlang::sym(forout_reactive$phenocolnames[6])) %>% summarise(count = n()) %>% arrange(-count) %>% 
      mutate(fraction = count / sum(count) * 100) %>% 
      mutate(ymax = cumsum(fraction)) %>% mutate(ymin = c(0, head(ymax, n=-1))) %>% 
      mutate(labpos = (ymax + ymin) / 2) %>% mutate(datatype = "data") 
    
    
    proxydonut = proxydonut %>% mutate(plotlabel = !! rlang::sym(forout_reactive$phenocolnames[6])) %>% mutate(pclabel = fraction)
    
    if((proxydonut$plotlabel %>% unique %>% length) >= 9){
      proxydonut = proxydonut %>% mutate(plotlabel = case_when(count < proxydonut[9, "count"] %>% as.numeric() ~ "rest", TRUE  ~ plotlabel)) %>% mutate(pclabel = case_when(count >= proxydonut[9, "count"] %>% as.numeric() ~ fraction, count == (proxydonut[floor(((nrow(proxydonut)  / 2))), "count"] %>% as.numeric()) ~ (100 - proxydonut[10, "ymax"] %>% as.numeric()), TRUE  ~ NA_real_))
    }
    
    proxydonut$pclabel <- tidyr::replace_na(proxydonut$pclabel, " ")
    
    
    forout_reactive$plot_pheno_proxydonut <- ggplot(proxydonut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=plotlabel)) +
      geom_rect() +
      geom_text( x=2.5, aes(y=labpos, label=paste0((stringr::str_extract(pclabel, "^.{3}") %>% sub("\\.$","",.)),"%"), color=plotlabel), size=3) + 
      annotate("text", col = "gray",  x = -1, y = 0, vjust = 0.5, label = c(paste0("annotated: \n", nrow(forout_reactive$table_pheno_processed) - (forout_reactive$table_pheno_processed %>% select(!! rlang::sym(forout_reactive$phenocolnames[6])) %>% is.na() %>% sum), " \\ ", nrow(forout_reactive$table_pheno_processed)))) +
      coord_polar(theta="y") +
      xlim(c(-1, 4))  + 
      scale_fill_discrete(name = "Proxy") +
      scale_color_discrete(name = "Proxy") + 
      theme_void()
    
    forout_reactive$plot_pheno_proxydonut
    
  })
  
  
  # Annotation - anno_gauge ----
  output$anno_gauge <- renderPlot( {
    
    req(forout_reactive$protanno)
    
    df <- data.frame(matrix(nrow=3, ncol = 4))
    
    names(df) <- c("variable", "percentage","n","total")
    df$variable <- c("1_TotalProtein","ProteinLocation", "3_DGIdb")
    df$percentage <- c(formatC(nrow(forout_reactive$protanno) / forout_reactive$protnum, digits = 2) %>% as.numeric(),
                       formatC((nrow(forout_reactive$protanno) - forout_reactive$protanno %>% dplyr::select(CP_loc) %>% is.na() %>% sum() ) / nrow(forout_reactive$protanno), digits = 2) %>% as.numeric(),
                       formatC(nrow(forout_reactive$protanno %>% filter(inDGIdb == TRUE)) / nrow(forout_reactive$protanno), digits = 2) %>% as.numeric()
    )
    
    df$n <- c(nrow(forout_reactive$protanno),
                  (nrow(forout_reactive$protanno) - forout_reactive$protanno %>% dplyr::select(CP_loc) %>% is.na() %>% sum()),
              (nrow(forout_reactive$protanno %>% filter(inDGIdb == TRUE))) )
    
    df$total <- c(forout_reactive$protnum,
                       nrow(forout_reactive$protanno),
                  nrow(forout_reactive$protanno))
    
    
    df <- df %>% mutate(group=ifelse(percentage <0.6, "red",
                                     ifelse(percentage>=0.6 & percentage<0.8, "orange","green")),
                        label=paste0(percentage*100, "%"),
                        title=dplyr::recode(variable, `1_TotalProtein`="a. Protein/transcript \n number after filtering", `ProteinLocation`="b. Protein localization \n by immunofluorescence", `3_DGIdb`="c. Protein/transcript \n found in DGIdb"),
                        fraction=paste0("\n \n (", n," / ", total,")"))
    
    
  #Gauge plot code is adapted from https://pomvlad.blog/2018/05/03/gauges-ggplot2/
   forout_reactive$plot_annogauge <- ggplot(df, aes(fill = group, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
      geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
      geom_rect() + 
      coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
      geom_text(aes(x = 0, y = 0, label = label, colour=group), size=8) +
      geom_text(aes(x = 0, y = 0, label = fraction, colour=group), size=6) +
      geom_text(aes(x=1.5, y=1.5, label=title), size=5) + 
      facet_wrap(~title, ncol = 5) +
      theme_void() +
      scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
      scale_colour_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank()) +
      guides(fill=FALSE) +
      guides(colour=FALSE)
   
   forout_reactive$plot_annogauge
    
  }) 
  
  
  # Analysis - ProtQTL plot ----
  observeEvent(input$protqtl_bttn, {
    
    req(forout_reactive$table_complex, forout_reactive$table_qtl_processed, isolate(input$protqtlplot_chrselect), isolate(input$param_qtlprot_cor), isolate(input$param_qtlprot_qval))
    
    sendSweetAlert(session = session, title = "Creating SNP-Protein plots", text = "Started processing", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
    message("Action: Creating SNP-Prot plot")
    
    if(nrow(forout_reactive$table_complex %>% filter((cor > isolate(input$param_qtlprot_cor[2]) | cor < isolate(input$param_qtlprot_cor[1])) & qval < 10^(isolate(input$param_qtlprot_qval)))) == 0){sendSweetAlert(session = session, title = "Error, dataset contains 0 rows after filtering", text = "Please select less stringent cut-offs", type = "error")}
    validate(need(nrow(forout_reactive$table_complex %>% filter((cor > isolate(input$param_qtlprot_cor[2]) | cor < isolate(input$param_qtlprot_cor[1])) & qval < 10^(isolate(input$param_qtlprot_qval))))!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    
    if(!isTruthy(isolate(input$protqtlplot_complexselect))){sendSweetAlert(session = session, title = "Error, Please select a protein or complex", text = "Please select at least one protein or complex", type = "error")}
    validate(need(isTruthy(isolate(input$protqtlplot_complexselect)), "Please select at least one protein or complex."))
    
    
    if(isolate(input$protqtlplot_chrselect) == "All Chromosomes"){
      forout_reactive$qtl_chrom <- forout_reactive$table_qtl_processed  %>% mutate(pc_x = ((!! rlang::sym(forout_reactive$qtlcolnames[2]) / max(!! rlang::sym(forout_reactive$qtlcolnames[2]))) * 100 )) %>% mutate(minpval = min(-log10(!! rlang::sym(forout_reactive$qtlcolnames[8]))), maxpval = max(-log10(!! rlang::sym(forout_reactive$qtlcolnames[8])))) %>% dplyr::group_by(!! rlang::sym(forout_reactive$qtlcolnames[3])) %>% mutate(mean_chr = mean(pc_x)) %>% ungroup()
    } else {
      forout_reactive$qtl_chrom <- forout_reactive$table_qtl_processed %>% filter(!! rlang::sym(forout_reactive$qtlcolnames[3]) == isolate(input$protqtlplot_chrselect))  %>% mutate(pc_x = (((!! rlang::sym(forout_reactive$qtlcolnames[2]) - min(!! rlang::sym(forout_reactive$qtlcolnames[2])) ) / (max(!! rlang::sym(forout_reactive$qtlcolnames[2])) - min(!! rlang::sym(forout_reactive$qtlcolnames[2]))) ) * 100 )) %>% mutate(minpval = min(-log10(!! rlang::sym(forout_reactive$qtlcolnames[8]))), maxpval = max(-log10(!! rlang::sym(forout_reactive$qtlcolnames[8])))) %>% dplyr::group_by(!! rlang::sym(forout_reactive$qtlcolnames[3])) %>% mutate(mean_chr = mean(pc_x)) %>% ungroup()
    }
    
    if(forout_reactive$qtl_quanttype == "pval"){
      ymax = forout_reactive$qtl_chrom %>% select(!! rlang::sym(forout_reactive$qtlcolnames[8])) %>% -log10(.) %>% max %>% floor
      ymin = forout_reactive$qtl_chrom %>% select(!! rlang::sym(forout_reactive$qtlcolnames[8])) %>% -log10(.) %>% min %>% ceiling
      yint = c(1,5,10,25,50,100)
      yint = yint[which.min(abs(yint-(ymax/5)))]
      yseq = c(ymin, seq(from = yint, to = ymax, by = yint))
      forout_reactive$plot_manhattan <- ggplot(forout_reactive$qtl_chrom, aes(x = pc_x, y = -log10(!! rlang::sym(forout_reactive$qtlcolnames[8])), col = as.factor(!! rlang::sym(forout_reactive$qtlcolnames[3])))) + geom_point(size = 0.1, shape = 20) + annotate(geom = "text", x = 105, y = yseq, label = yseq, alpha = 0.5) + annotate(geom = "text", x = 107, y = (ymin + ((ymax-ymin) / 2)), label = (forout_reactive$qtlcolnames[8]), alpha = 0.5, angle = 90) + annotate(geom = "segment", x = 101, xend = 103, y = yseq, yend = yseq, alpha = 0.3) + theme(legend.position = "null") + cowplot::theme_nothing() + scale_color_manual(values = rep(c("#094183", "darkgray"), forout_reactive$qtl_chrom %>% group_by(!! rlang::sym(forout_reactive$qtlcolnames[3])) %>% tally() %>% nrow )) + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0,0)) + coord_cartesian(xlim = c(0,110)) + annotate("text", x = forout_reactive$qtl_chrom["mean_chr"] %>% unlist %>% unique, y = max(-log10(forout_reactive$qtl_chrom[forout_reactive$qtlcolnames[8]])), label = forout_reactive$qtl_chrom[forout_reactive$qtlcolnames[3]] %>% unlist %>% unique, size = 4, hjust = 0.5, vjust = 1) + geom_segment(data = forout_reactive$qtl_chrom %>% distinct(!! rlang::sym(forout_reactive$qtlcolnames[3]), mean_chr, minpval, maxpval), aes(x = mean_chr, y = minpval, xend = mean_chr, yend = maxpval), alpha = 0.1)
    } else {
      ymax = forout_reactive$qtl_chrom %>% select(!! rlang::sym(forout_reactive$qtlcolnames[8])) %>% max %>% floor
      ymin = forout_reactive$qtl_chrom %>% select(!! rlang::sym(forout_reactive$qtlcolnames[8])) %>% min %>% ceiling
      yint = c(1,5,10,25,50,100)
      yint = yint[which.min(abs(yint-(ymax/5)))]
      yseq = c(ymin, seq(from = yint, to = ymax, by = yint))
      forout_reactive$plot_manhattan <- ggplot(forout_reactive$qtl_chrom, aes(x = pc_x, y = (!! rlang::sym(forout_reactive$qtlcolnames[8])), col = as.factor(!! rlang::sym(forout_reactive$qtlcolnames[3])))) + geom_point(size = 0.1, shape = 20) + annotate(geom = "text", x = 105, y = yseq, label = yseq, alpha = 0.5)  + annotate(geom = "text", x = 107, y = (ymin + ((ymax-ymin) / 2)), label = (forout_reactive$qtlcolnames[8]), alpha = 0.5, angle = 90) + annotate(geom = "segment", x = 101, xend = 103, y = yseq, yend = yseq, alpha = 0.3) + theme(legend.position = "null") + cowplot::theme_nothing() + scale_color_manual(values = rep(c("#094183", "darkgray"), forout_reactive$qtl_chrom %>% group_by(!! rlang::sym(forout_reactive$qtlcolnames[3])) %>% tally() %>% nrow )) + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0,0)) + coord_cartesian(xlim = c(0,110)) + annotate("text", x = forout_reactive$qtl_chrom["mean_chr"] %>% unlist %>% unique, y = max((forout_reactive$qtl_chrom[forout_reactive$qtlcolnames[8]])), label = forout_reactive$qtl_chrom[forout_reactive$qtlcolnames[3]] %>% unlist %>% unique, size = 4, hjust = 0.5, vjust = 1) + geom_segment(data = forout_reactive$qtl_chrom %>% distinct(!! rlang::sym(forout_reactive$qtlcolnames[3]), mean_chr, minpval, maxpval), aes(x = mean_chr, y = min((forout_reactive$qtl_chrom[forout_reactive$qtlcolnames[8]])), xend = mean_chr, yend = max((forout_reactive$qtl_chrom[forout_reactive$qtlcolnames[8]]))), alpha = 0.1)
}
    
    #Filter data using correlation and q-value cut-offs, save object for arc diagram plot
    arc_diag <- forout_reactive$table_complex %>% filter((cor > isolate(input$param_qtlprot_cor[2]) | cor < isolate(input$param_qtlprot_cor[1])) & qval < 10^(isolate(input$param_qtlprot_qval)))
    
    if(isolate(input$protqtlplot_complexselect) == "All proteins with QTLs"){
    arc_diag <- arc_diag %>% filter(varID1 %in% (forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unique %>% unlist %>% as.vector() %>% tolower) | varID2 %in% (forout_reactive$table_qtl_processed %>% select(!! rlang::sym(forout_reactive$qtlcolnames[4])) %>% unique %>% unlist %>% as.vector() %>% tolower) )
    validate(need(nrow(arc_diag)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    } else if (isolate(input$prottype) == "corum" & !(isolate(input$protqtlplot_complexselect) %in% c("All proteins", "All proteins with QTLs"))){
    arc_diag <- arc_diag %>% filter(inCORUM == TRUE)       
    } else if (isolate(input$prottype) == "bioplex" & !(isolate(input$protqtlplot_complexselect) %in% c("All proteins", "All proteins with QTLs"))){
    bioplextarget <- db_bioplex3_4pc %>% select(SymbolA, SymbolB) %>% c(.$SymbolA, .$SymbolB) %>% unlist %>% as.vector() %>% tolower
    arc_diag <- arc_diag %>% filter(varID1 %in% bioplextarget | varID2 %in% bioplextarget)      
    } 
    
    
    # add organelle filtering here based on input$select_organelle_protqtl
    if(length(isolate(input$select_organelle_protqtl) > 0)){
      
      # Take the organelle filter and replace the string "NA" with NA
      select_organelle <- isolate(input$select_organelle_protqtl)
      select_organelle[ select_organelle == "NA" ] <- NA
      
      # Create a list of proteins that should be retained
      # unlist CP_loc and check whether any entry is present in select_organelle
      # filter nw_edge by the created genelist
      select_organelle_genes <- forout_reactive$protanno %>% select(ID, CP_loc) %>% rowwise() %>% filter(strsplit(CP_loc, ";") %>% unlist %in% select_organelle %>% any()) %>% select(ID) %>% unlist %>% unique
      
      arc_diag <- arc_diag %>% filter(varID1 %in% select_organelle_genes & varID2 %in% select_organelle_genes)
      
    }
    
    
    arc_diag <- bind_rows(arc_diag %>% mutate(varIDx = varID1, varIDy = varID2) %>% select(-varID1, - varID2), arc_diag %>% mutate(varIDx = varID2, varIDy = varID1) %>% select(-varID1, - varID2))
    
    arc_diag <- arc_diag %>% group_by(ComplexID) %>% add_tally(name = "complexsize") %>% group_by(ComplexID, varIDx) %>% add_tally(name = "complexprotnum") %>% left_join(., (arc_diag %>% select(ComplexID, varIDx) %>% unique %>% group_by(ComplexID) %>% count(name = "complexprotuniq"))) %>% ungroup()
    
    ###Optional filter for single complex
    if(isolate(input$prottype) == "corum" & !(isolate(input$protqtlplot_complexselect) %in% c("All complexes", "All proteins", "All proteins with QTLs"))){
      arc_diag <- arc_diag %>% filter(ComplexName == isolate(input$protqtlplot_complexselect))
    }
    
    if(isolate(input$prottype) == "bioplex" & !(isolate(input$protqtlplot_complexselect) %in% c("All complexes", "All proteins", "All proteins with QTLs"))){
      arc_bioplextarget <- db_bioplex3_4pc %>% filter(SymbolA == isolate(input$protqtlplot_complexselect) | SymbolB == isolate(input$protqtlplot_complexselect)) %>% select(SymbolA, SymbolB) %>% c(.$SymbolA, .$SymbolB) %>% unlist %>% as.vector() %>% tolower
      arc_diag <- arc_diag %>% filter(varIDx %in% arc_bioplextarget | varIDy %in% arc_bioplextarget)
    }
    
    if(isolate(input$prottype) == "all" & !(isolate(input$protqtlplot_complexselect) %in% c("All complexes", "All proteins", "All proteins with QTLs"))){
      arc_diag <- arc_diag %>% filter(varIDx %in% isolate(input$protqtlplot_complexselect) | varIDy %in% isolate(input$protqtlplot_complexselect))
    }
    
    #The assignment of a protein location for the arc_diagram depends on the protein source used. 
    #For Corum the proteins are first ordered by complex size, then by number of connections
    #For other plots 
    if(isolate(input$prottype) == "corum"){
      arc_loc <- arc_diag %>% dplyr::select(varIDx, ComplexID, ComplexName, complexprotuniq, complexprotnum) %>% unique %>% ungroup() %>% arrange(-complexprotuniq, -ComplexID, -complexprotnum) %>% mutate(loc.prot = ( (1:nrow(.) / nrow(.)) * 100) ) %>% select(-complexprotuniq, -complexprotnum)
      arc_loc <- left_join(arc_loc, arc_loc %>% group_by(ComplexID) %>% summarise(loc.complex = mean(loc.prot))) %>% ungroup()
      arc_diag <- arc_diag %>% left_join(., arc_loc, by = c("ComplexID" = "ComplexID", "varIDx" = "varIDx")) %>% left_join(., arc_loc, by = c("ComplexID" = "ComplexID", "varIDy" = "varIDx")) %>% filter(loc.prot.x < loc.prot.y)
    } else if(isolate(input$prottype) == "all" | !(isolate(input$protqtlplot_complexselect) %in% c("All complexes", "All proteins", "All proteins with QTLs"))){
      arc_loc <- bind_rows(arc_diag %>% select(varIDx) %>% `colnames<-`(c("varIDx")), arc_diag %>% select(varIDy) %>% `colnames<-`(c("varIDx"))) %>% unique %>% arrange(varIDx) %>% mutate(loc.prot = ( (1:nrow(.) / nrow(.)) * 100) ) %>% mutate(user_selection = varIDx %in% isolate(input$protqtlplot_complexselect))
      arc_diag <- arc_diag %>% left_join(., arc_loc, by = c("varIDx" = "varIDx")) %>% left_join(., arc_loc, by = c("varIDy" = "varIDx")) %>% filter(loc.prot.x < loc.prot.y)
      
    } else {
      arc_loc <- bind_rows(arc_diag %>% select(varIDx) %>% `colnames<-`(c("varIDx")), arc_diag %>% select(varIDy) %>% `colnames<-`(c("varIDx"))) %>% unique %>% arrange(varIDx) %>% mutate(loc.prot = ( (1:nrow(.) / nrow(.)) * 100) )
      arc_diag <- arc_diag %>% left_join(., arc_loc, by = c("varIDx" = "varIDx")) %>% left_join(., arc_loc, by = c("varIDy" = "varIDx")) %>% filter(loc.prot.x < loc.prot.y)
      
    }
    
    #Join QTL data on VarIDx and VarIDy
    if(forout_reactive$qtl_annotated == FALSE){
      qtl_edges <- rbind(arc_diag, arc_diag %>% mutate(varIDx = arc_diag$varIDy, varIDy = arc_diag$varIDx, loc.prot.x = arc_diag$loc.prot.y, loc.prot.y = arc_diag$loc.prot.x, user_selection.x = arc_diag$user_selection.y, user_selection.y = arc_diag$user_selection.x)) %>% left_join(., forout_reactive$qtl_chrom %>% mutate(gene_symbol = tolower(!! rlang::sym(forout_reactive$qtlcolnames[4]))) %>% select(gene_symbol, !! rlang::sym(forout_reactive$qtlcolnames[9]), !! rlang::sym(forout_reactive$qtlcolnames[8]), pc_x, CP_Intragenic_QTL), by = c("varIDx" = "gene_symbol")) %>% filter(is.na(pc_x) == FALSE)
    } else {
      qtl_edges <- rbind(arc_diag, arc_diag %>% mutate(varIDx = arc_diag$varIDy, varIDy = arc_diag$varIDx, loc.prot.x = arc_diag$loc.prot.y, loc.prot.y = arc_diag$loc.prot.x, user_selection.x = arc_diag$user_selection.y, user_selection.y = arc_diag$user_selection.x)) %>% left_join(., forout_reactive$qtl_chrom %>% mutate(gene_symbol = tolower(!! rlang::sym(forout_reactive$qtlcolnames[4]))) %>% select(gene_symbol, !! rlang::sym(forout_reactive$qtlcolnames[9]), !! rlang::sym(forout_reactive$qtlcolnames[8]), pc_x, CP_Intragenic_QTL, CP_Variant_Effect, CP_Variant_Impact), by = c("varIDx" = "gene_symbol")) %>% filter(is.na(pc_x) == FALSE)
      
    }
    
    
    #### Filter proxies or VE depending on selected edgetype
    if(isolate(input$edgeproxy_intersect) == TRUE & length(isolate(input$edgeproxy)) > 1){
      
      intersectlist = list()
      
      if(isolate(input$edgetype) == "proxy"){
        for(i in 1:length(isolate(input$edgeproxy))){intersectlist[i] <- qtl_edges %>% filter(!! rlang::sym(forout_reactive$qtlcolnames[9]) == isolate(input$edgeproxy)[i]) %>% select(VarVar) %>% unique}
      } else if(isolate(input$edgetype) == "intragenic"){
        for(i in 1:length(isolate(input$edgeproxy))){intersectlist[i] <- qtl_edges %>% filter(CP_Intragenic_QTL == isolate(input$edgeproxy)[i]) %>% select(VarVar) %>% unique}
        } else if(isolate(input$edgetype) == "ve"){
        for(i in 1:length(isolate(input$edgeproxy))){intersectlist[i] <- qtl_edges %>% filter(CP_Variant_Effect == isolate(input$edgeproxy)[i]) %>% select(VarVar) %>% unique}
      } else {
        for(i in 1:length(isolate(input$edgeproxy))){intersectlist[i] <- qtl_edges %>% filter(CP_Variant_Impact == isolate(input$edgeproxy)[i]) %>% select(VarVar) %>% unique}
      }
      

      if(length(Reduce(intersect, intersectlist) %>% unlist) > 0){
      qtl_edges <- qtl_edges %>% filter(VarVar %in% (Reduce(intersect, intersectlist) %>% unlist))
      } else {
      qtl_edges <- qtl_edges %>% filter(VarVar %in% (Reduce(intersect, intersectlist) %>% unlist))
      }
      

    } else if(isolate(input$edgeproxy_intersect) == FALSE){

      if(isolate(input$edgetype) == "proxy"){
        if(isTruthy(isolate(input$edgeproxy))){qtl_edges <- qtl_edges %>% filter(!! rlang::sym(forout_reactive$qtlcolnames[9]) == isolate(input$edgeproxy))}
      } else if(isolate(input$edgetype) == "intragenic"){
        if(isTruthy(isolate(input$edgeproxy))){qtl_edges <- qtl_edges %>% filter(CP_Intragenic_QTL == isolate(input$edgeproxy))}
      } else if(isolate(input$edgetype) == "ve"){
        if(isTruthy(isolate(input$edgeproxy))){qtl_edges <- qtl_edges %>% filter(CP_Variant_Effect == isolate(input$edgeproxy))}
      } else {
        if(isTruthy(isolate(input$edgeproxy))){qtl_edges <- qtl_edges %>% filter(CP_Variant_Impact == isolate(input$edgeproxy))}}
    
    } 
    
    #Keep only QTLs intersecting with selected types
    if(isolate(input$edgeproxy_intersect_rsid) == TRUE & length(isolate(input$edgeproxy)) > 1){
      intersectlist = list()
      
      if(isolate(input$edgetype) == "proxy"){
        for(i in 1:length(isolate(input$edgeproxy))){intersectlist[i] <- qtl_edges %>% filter(!! rlang::sym(forout_reactive$qtlcolnames[9]) == isolate(input$edgeproxy)[i]) %>% select(pc_x) %>% unique}
      } else if(isolate(input$edgetype) == "intragenic"){
        for(i in 1:length(isolate(input$edgeproxy))){intersectlist[i] <- qtl_edges %>% filter(CP_Intragenic_QTL == isolate(input$edgeproxy)[i]) %>% select(pc_x) %>% unique}
      } else if(isolate(input$edgetype) == "ve"){
        for(i in 1:length(isolate(input$edgeproxy))){intersectlist[i] <- qtl_edges %>% filter(CP_Variant_Effect == isolate(input$edgeproxy)[i]) %>% select(pc_x) %>% unique}
      } else {
        for(i in 1:length(isolate(input$edgeproxy))){intersectlist[i] <- qtl_edges %>% filter(CP_Variant_Impact == isolate(input$edgeproxy)[i]) %>% select(pc_x) %>% unique}
      }
      
      if(length(Reduce(intersect, intersectlist) %>% unlist) > 0){
        qtl_edges <- qtl_edges %>% filter(pc_x %in% (Reduce(intersect, intersectlist) %>% unlist))
      } else {
        qtl_edges <- qtl_edges %>% filter(pc_x %in% (Reduce(intersect, intersectlist) %>% unlist))
      }
    }
      

    if(nrow(qtl_edges) == 0){
      forout_reactive$plot_edge <- ggplot(qtl_edges) + cowplot::theme_nothing() + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) 
    } else {
      if(input$edgetype == "proxy"){
      forout_reactive$plot_edge <- ggplot(qtl_edges) + geom_curve(aes(x = pc_x, y = 1, xend = loc.prot.x, yend = 0, col = !! rlang::sym(forout_reactive$qtlcolnames[9])), curvature = 0.03, alpha = 1, size = 0.1) + cowplot::theme_nothing() + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + coord_cartesian(xlim = c(0,110)) + scale_y_continuous(expand=c(0.02,0.02)) + theme(legend.position = c(0.95, .50)) + guides(color = guide_legend(title = NULL, ncol = 3, label.position = "bottom",label.hjust = 0.5,label.vjust = 1,label.theme = element_text(angle = 90), override.aes = list(size = 2)))
      } else if(input$edgetype == "intragenic"){
        forout_reactive$plot_edge <- ggplot(qtl_edges) + geom_curve(aes(x = pc_x, y = 1, xend = loc.prot.x, yend = 0, col = CP_Intragenic_QTL), curvature = 0.03, alpha = 1, size = 0.1) + cowplot::theme_nothing() + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + coord_cartesian(xlim = c(0,110)) + scale_y_continuous(expand=c(0.02,0.02)) + theme(legend.position = c(0.95, .50)) + guides(color = guide_legend(title = NULL, ncol = 3, label.position = "bottom",label.hjust = 0.5,label.vjust = 1,label.theme = element_text(angle = 90), override.aes = list(size = 2)))
      } else if(input$edgetype == "ve"){
      forout_reactive$plot_edge <- ggplot(qtl_edges) + geom_curve(aes(x = pc_x, y = 1, xend = loc.prot.x, yend = 0, col = CP_Variant_Effect), curvature = 0.03, alpha = 1, size = 0.1) + cowplot::theme_nothing() + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + coord_cartesian(xlim = c(0,110)) + scale_y_continuous(expand=c(0.02,0.02)) + theme(legend.position = c(0.95, .50)) + guides(color = guide_legend(title = NULL, ncol = 3, label.position = "bottom",label.hjust = 0.5,label.vjust = 1,label.theme = element_text(angle = 90), override.aes = list(size = 2)))
      } else {
      forout_reactive$plot_edge <- ggplot(qtl_edges) + geom_curve(aes(x = pc_x, y = 1, xend = loc.prot.x, yend = 0, col = CP_Variant_Impact), curvature = 0.03, alpha = 1, size = 0.1) + cowplot::theme_nothing() + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + coord_cartesian(xlim = c(0,110)) + scale_y_continuous(expand=c(0.02,0.02)) + theme(legend.position = c(0.95, .50)) + guides(color = guide_legend(title = NULL, ncol = 3, label.position = "bottom",label.hjust = 0.5,label.vjust = 1,label.theme = element_text(angle = 90), override.aes = list(size = 2)))
      }
    }
    
    
    if(!(isolate(input$protqtlplot_complexselect) %in% c("All complexes", "All proteins", "All proteins with QTLs"))){
    if(isolate(input$prottype) == "all"){
      #use user_selection to colour selected proteins
      forout_reactive$plot_arcdiagram <- ggplot(arc_diag) + geom_curve(aes(x = loc.prot.x, y = 0, xend = loc.prot.y, yend = 0), col = "darkgray", curvature = 0.3, alpha = 0.6, size = 0.3, ncp = 50) + geom_curve(data = arc_loc, aes(x = loc.prot, y = -0.1, xend = loc.prot, yend = rep(c(-0.8, -0.7), length.out = nrow(arc_loc)), col = user_selection, alpha = user_selection, size = user_selection), curvature = 0) + scale_alpha_discrete(range = c(0.1, 0.6)) + scale_size_discrete(range = c(0.3, 1)) + annotate("text", x = arc_loc$loc.prot, y = rep(c(-0.8, -0.7), length.out = nrow(arc_loc)), label = arc_loc$varIDx, angle = 90, size = 4, hjust = 1) + geom_point(shape = 20, size = 0.3, stroke = 0.1, aes(x = loc.prot.x, y = 0)) + geom_point(shape = 20, size = 0.3, stroke = 0.1, aes(x = loc.prot.y, y = 0)) + coord_cartesian(xlim = c(0,110), ylim = c(-1,0)) + cowplot::theme_nothing() + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0,0)) + theme(legend.position = c(0.95, .50))
    } else {
      forout_reactive$plot_arcdiagram <- ggplot(arc_diag) + geom_curve(aes(x = loc.prot.x, y = 0, xend = loc.prot.y, yend = 0), col = "darkgray", curvature = 0.3, alpha = 0.6, size = 0.3, ncp = 50) + geom_curve(data = arc_loc, aes(x = loc.prot, y = -0.1, xend = loc.prot, yend = rep(c(-0.8, -0.7), length.out = nrow(arc_loc))), curvature = 0, alpha = 0.1, size = 0.3) + annotate("text", x = arc_loc$loc.prot, y = rep(c(-0.8, -0.7), length.out = nrow(arc_loc)), label = arc_loc$varIDx, angle = 90, size = 4, hjust = 1) + geom_point(shape = 20, size = 0.3, stroke = 0.1, aes(x = loc.prot.x, y = 0)) + geom_point(shape = 20, size = 0.3, stroke = 0.1, aes(x = loc.prot.y, y = 0)) + coord_cartesian(xlim = c(0,110), ylim = c(-1,0)) + cowplot::theme_nothing() + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0,0)) + theme(legend.position = c(0.95, .50))
      
    }

      } else if(isolate(input$protqtlplot_complexselect) == c("All complexes")){
      
      if(isolate(input$prottype) == "bioplex"){
        #Code for all complexes
        forout_reactive$plot_arcdiagram <- ggplot(arc_diag) + geom_curve(aes(x = loc.prot.x, y = 0, xend = loc.prot.y, yend = 0), col = "darkgray", curvature = 0.3, alpha = 0.6, size = 0.3, ncp = 50) + geom_curve(data = arc_loc, aes(x = loc.prot, y = -0.1, xend = loc.prot, yend = rep(c(-0.8, -0.7), length.out = nrow(arc_loc))), curvature = 0, alpha = 0.1, size = 0.3) + annotate("text", x = arc_loc$loc.prot, y = rep(c(-0.8, -0.7), length.out = nrow(arc_loc)), label = arc_loc$varIDx, angle = 90, size = 4, hjust = 1) + geom_point(shape = 20, size = 0.3, stroke = 0.1, aes(x = loc.prot.x, y = 0)) + geom_point(shape = 20, size = 0.3, stroke = 0.1, aes(x = loc.prot.y, y = 0)) + coord_cartesian(xlim = c(0,110), ylim = c(-1,0)) + cowplot::theme_nothing() + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0,0))  + theme(legend.position = c(0.95, .50)) + guides(color = guide_legend(title = NULL,label.position = "bottom",label.hjust = 0.5,label.vjust = 1,label.theme = element_text(angle = 90), override.aes = list(size = 2)))
        
      } else {
        #Code for all complexes
        forout_reactive$plot_arcdiagram <- ggplot(arc_diag) + 
          geom_curve(aes(x = loc.prot.x, y = 0, xend = loc.prot.y, yend = 0, col = as.factor(ComplexID)), curvature = 1, alpha = 0.8, size = 0.3, ncp = 50) + 
          geom_curve(data = arc_loc %>% distinct(ComplexID, ComplexName, loc.complex), aes(x = loc.complex, y = -0.01, xend = loc.complex, yend = -0.3), curvature = 0, alpha = 0.1, size = 0.3) + 
          annotate("text", x = arc_loc %>% distinct(ComplexID, ComplexName, loc.complex)  %>% select(loc.complex) %>% unlist, y = -0.31, label = arc_loc %>% distinct(ComplexID, ComplexName, loc.complex)  %>% select(ComplexName) %>% as.matrix(), angle = 70, size = 3, hjust = 1) + 
          geom_point(shape = 20, size = 0.3, stroke = 0.1, aes(x = loc.prot.x, y = 0, col = as.factor(ComplexID))) + geom_point(shape = 20, size = 0.3, stroke = 0.1, aes(x = loc.prot.y, y = 0, col = as.factor(ComplexID))) + theme(legend.position = "null") + coord_cartesian(xlim = c(0,110), ylim = c(-1,0)) + cowplot::theme_nothing() + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0,0))
        
      }
      } else {
      forout_reactive$plot_arcdiagram <- ggplot(arc_diag) + geom_curve(aes(x = loc.prot.x, y = 0, xend = loc.prot.y, yend = 0), col = "darkgray", curvature = 0.3, alpha = 0.6, size = 0.3, ncp = 50) + geom_curve(data = arc_loc, aes(x = loc.prot, y = -0.1, xend = loc.prot, yend = rep(c(-0.8, -0.7), length.out = nrow(arc_loc))), curvature = 0, alpha = 0.1, size = 0.3) + annotate("text", x = arc_loc$loc.prot, y = rep(c(-0.8, -0.7), length.out = nrow(arc_loc)), label = arc_loc$varIDx, angle = 90, size = 4, hjust = 1) + geom_point(shape = 20, size = 0.3, stroke = 0.1, aes(x = loc.prot.x, y = 0)) + geom_point(shape = 20, size = 0.3, stroke = 0.1, aes(x = loc.prot.y, y = 0)) + coord_cartesian(xlim = c(0,110), ylim = c(-1,0)) + cowplot::theme_nothing() + theme(plot.margin=unit(c(0,0,0,0), "null"), panel.margin=unit(c(0,0,0,0), "null"), axis.ticks.length = unit(0, "pt")) + scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0,0))  + theme(legend.position = c(0.95, .50)) + guides(color = guide_legend(title = NULL,label.position = "bottom",label.hjust = 0.5,label.vjust = 1,label.theme = element_text(angle = 90), override.aes = list(size = 2)))
      
      }
    
    sendSweetAlert(session = session, title = "Creating SNP-Protein plots", text = "Processing completed, rendering started", type = "success")
    
    
    updateProgressBar(session = session, id = "pb9", value = 100)
    
  })
  

  
###################################################################################################################################################
#### Report and exporting
###################################################################################################################################################
    
  
    # Report - Select plot ----
    output$downloadplotselect_ui <- renderUI({
      req(forout_reactive)
      selectInput("downloadplotselect", "Select plot", (c(names(forout_reactive) %>% grep("^plot_", ., value = TRUE) %>% sort() )), selected = 1, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
    })
    
  
  # Download interactive network handler ----
  output$download_network <- downloadHandler(
    filename = function() { paste("network_", input$network_complexselect, ".html", sep = "") },
    content = function(file) {
      
      gc()
      
      saveNetwork(forout_reactive$interactive_plot_network_dl, file, selfcontained = TRUE)
    })
  
  # Download network handler ----
  output$download_networktable <- downloadHandler(
    filename = function() { paste("network_", input$network_complexselect, ".csv", sep = "") },
    content = function(file) {
      
      gc()
      
      if("Drug-gene interaction" %in% forout_reactive$network_links$connection){
        write.csv(x = forout_reactive$network_links %>% left_join(., db_dgidb, by = c("varID1" = "drug_name", "varID2" = "gene_name")), file = file, row.names = FALSE)
      } else {
        write.csv(x = forout_reactive$network_links, file = file, row.names = FALSE)
      }
    })

  
  # Download SNP interaction table handler ----
  output$download_interactiontable <- downloadHandler(
    filename = function() { paste("SNP_interactions_", input$interactiontable_type, ".csv", sep = "") },
    content = function(file) {
      
      gc()
      
      snplist <- intersect(forout_reactive$table_qtl_processed$ID, forout_reactive$table_pheno_processed$ID)
      interactiontable <- left_join(forout_reactive$table_qtl_processed %>% filter(ID %in% snplist), forout_reactive$table_pheno_processed %>% filter(ID %in% snplist), by = "ID")
      colnames <- colnames(interactiontable)
      
      print(colnames)
      
      interactiontable <- interactiontable %>% rename(gene = colnames[4]) %>% rename(CP_trait_name = colnames[length(colnames)-4])
      
      print(colnames)
      
      if(input$interactiontable_type == "single"){
        
        write.csv(x = interactiontable, file = file, row.names = FALSE)
        
      } else if(input$interactiontable_type == "traits"){
        
        write.csv(x = aggregate(CP_trait_name ~ .,data=interactiontable %>% select(ID, gene, CP_trait_name) %>% as.matrix,FUN=paste0, collapse = ";"), file = file, row.names = FALSE)
        
      } else {
        
        write.csv(x = aggregate(gene ~ .,data=interactiontable %>% select(ID, gene, CP_trait_name) %>% as.matrix,FUN=paste0, collapse = ";"), file = file, row.names = FALSE)
        
      } })
    
  # Download bait network handler ----
  output$download_baitnetwork <- downloadHandler(
    filename = function() { paste("baitnetwork_", input$baitselect, ".html", sep = "") },
    content = function(file) {
      
      gc()
      
    saveNetwork(forceNetwork(Links = forout_reactive$bait_links, Nodes = forout_reactive$bait_nodes, Source = 'source', Target = 'target', NodeID = 'var', Nodesize = 'n', Group = 'nodetype', linkColour = forout_reactive$bait_links$edgecol, charge = input$nodecharge, opacity = 1, fontSize = 12, zoom = TRUE, opacityNoHover = 0.6, legend = TRUE), file, selfcontained = TRUE)
    })
  
  # Download bait network handler ----
  output$download_baitnetworktable <- downloadHandler(
    filename = function() { paste("baitnetwork_", input$baitselect, ".csv", sep = "") },
    content = function(file) {
      
      gc()
      
      if("Drug-gene interaction" %in% forout_reactive$bait_links$edge){
        
        write.csv(x = forout_reactive$bait_links %>% left_join(., db_dgidb, by = c("varID1" = "drug_name", "varID2" = "gene_name")), file = file, row.names = FALSE)
        
      } else {
        
        write.csv(x = forout_reactive$bait_links, file = file, row.names = FALSE)
        
      }
        
    })
  
  # Download plot handler ----
  output$download_plot <- downloadHandler(
    filename = function() { paste(input$downloadplotselect, '.', input$downloadplotfiletype, sep='') },
    content = function(file) {
      
      gc()
      
      sendSweetAlert(session = session, title = "Starting download!", text = "The download will start as soon as the plot is rendered.", type = "success")
      
      ggsave(file, plot = forout_reactive[[input$downloadplotselect]], device = input$downloadplotfiletype, width = as.numeric(input$plotdims[1]), height = (as.numeric(input$plotdims[1]) / as.numeric(input$plotdims[2])))
    })
  
  # Download plot handler in zip format (all files) ----
  output$download_plot_zip <- downloadHandler(
    filename = function() { paste("CoffeeProt_all_plots.zip") },
 
      content =function(file) {
        
        gc()
        
        if(length((c(names(forout_reactive) %>% grep("^plot_", ., value = TRUE) %>% sort() ))) > 0){
        
        sendSweetAlert(session = session, title = "Starting download!", text = "The download will start as soon as the plots are rendered.", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
        
        fs <- c()
        tmpdir <- tempdir()
        print(tmpdir)
        for (i in 1:length((c(names(forout_reactive) %>% grep("^plot_", ., value = TRUE) %>% sort() )))) {
          path <- paste0(tmpdir, "\\", (c(names(forout_reactive) %>% grep("^plot_", ., value = TRUE) %>% sort() ))[i], ".svg")
          fs <- c(fs, path)
          ggsave(path, plot = forout_reactive[[(c(names(forout_reactive) %>% grep("^plot_", ., value = TRUE) %>% sort() ))[i]]], device = "svg", width = 8, height = 8)
        } 
        zip(zipfile=file, files=fs, flags = "-j")
        
        sendSweetAlert(session = session, title = "Plots rendered!", text = "The download will start now", type = "success")
        
      } }
  )
  
  # Download table handler in zip format (all files) ----
  output$download_table_zip <- downloadHandler(
    filename = function() { paste("CoffeeProt_all_tables.zip") },
    
    content =function(file) {
      
      gc()
      
      if(length((c(names(forout_reactive) %>% grep("^table_", ., value = TRUE) %>% sort() ))) > 0){
        
        sendSweetAlert(session = session, title = "Starting download!", text = "The download will start as soon as the tables are prepared", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
        
        fs <- c()
        tmpdir <- tempdir()
        print(tmpdir)
        for (i in 1:length((c(names(forout_reactive) %>% grep("^table_", ., value = TRUE) %>% sort() )))) {
          path <- paste0(tmpdir, "\\", (c(names(forout_reactive) %>% grep("^table_", ., value = TRUE) %>% sort() ))[i], ".csv")
          fs <- c(fs, path)
          write.csv(x = forout_reactive[[(c(names(forout_reactive) %>% grep("^table_", ., value = TRUE) %>% sort() ))[i]]], file = path, row.names = FALSE)
          } 
        zip(zipfile=file, files=fs, flags = "-j")
        
        sendSweetAlert(session = session, title = "Tables prepared!", text = "The download will start now", type = "success")
        
      } }
  )
  
  #Generate GWAS Catalog table
  observeEvent(input$generate_gwascatalog, {
    
    if(!isTruthy(input$gwas_catalog_row_last_clicked)){
      sendSweetAlert(session = session, title = "Please select a study to retrieve.", text = "Click on any row in the table above...", type = "error")
      
    } else {

    message(paste("gwas_", gwas_catalog %>% select(-reported_trait, -study_id) %>% group_by(pubmed_id) %>% distinct() %>% .[input$gwas_catalog_row_last_clicked,1] %>% unlist %>% as.vector(), ".csv", sep = ""))
    
    sendSweetAlert(session = session, title = "Downloading data from GWAS Catalog", text = "Please be patient, this will just take a minute...", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
    
    id <- gwas_catalog %>% filter(pubmed_id == gwas_catalog %>% select(-reported_trait, -study_id) %>% group_by(pubmed_id) %>% distinct() %>% .[input$gwas_catalog_row_last_clicked,1] %>% unlist %>% as.vector()) %>% select(study_id) %>% unlist %>% as.vector()
    
    output <- list()
    
    #Perform search for each study id in pubmed id
    for(i in 1:length(id)){
      
      message(id[i])
      
      association_gwas <- gwasrapidd::get_associations(study_id = id[i])
      
      if(length(association_gwas@associations$pvalue) > 0){
        variant_gwas <- gwasrapidd::get_variants(study_id = id[i])
        
        output_table <- data.frame(variant_id = association_gwas@risk_alleles$variant_id, trait = gwas_catalog %>% filter(study_id == id[i]) %>% select(reported_trait) %>% unlist, pvalue = association_gwas@associations$pvalue, pvalue_description = association_gwas@associations$pvalue_description) %>% left_join(., variant_gwas@variants, by = "variant_id") %>% select(variant_id, trait, chromosome_position, chromosome_name, pvalue, functional_class, pvalue_description)
        
        output[[length(output) + 1]] <- output_table
      } 
    }
    
    message("Status: completed accessing GWAS catalog data")
    
    #Combine tables for export
    if(length(output) == 0){
      sendSweetAlert(session = session, title = "Study contained no associations.", text = "Please try a different study...", type = "error")
      
    } else {
      
      output <- do.call(rbind, output)
      
      sendSweetAlert(session = session, title = "Processing complete!", text = "Generating table...", type = "success")
      
      #Assign pubmed ID and table to reactive values
      forout_reactive$gwas_catalog_temp_pubmed_id <- gwas_catalog %>% select(-reported_trait, -study_id) %>% group_by(pubmed_id) %>% distinct() %>% .[input$gwas_catalog_row_last_clicked,1] %>% unlist %>% as.vector()
      forout_reactive$gwas_catalog_temp <- output
      
      }
    }
  })

  # Download table handler ----
  output$download_gwascatalog <- downloadHandler(
    filename = function() { paste("gwas_", forout_reactive$gwas_catalog_temp_pubmed_id, ".csv", sep = "") },
    content = function(file) { 
      
      gc()
      
      if(!isTruthy(forout_reactive$gwas_catalog_temp)){
        
        sendSweetAlert(session = session, title = "Please generate a table first.", text = "First select a study of interest, followed by hitting the 'Retrieve and generate GWAS table' button", type = "error")
        
      } else {
      
      if(input$tick_gwascatalog == FALSE){
        write.csv(forout_reactive$gwas_catalog_temp, file, row.names = FALSE)
      } else {
        write.csv(forout_reactive$gwas_catalog_temp %>% select(variant_id, pvalue_description, chromosome_position, chromosome_name, pvalue, functional_class, trait), file, row.names = FALSE)
      } }
       }
  )
  


  # GWAS catalog tables ----
  output$gwas_catalog <- DT::renderDT({return(DT::datatable(gwas_catalog %>% select(-reported_trait, -study_id) %>% group_by(pubmed_id) %>% distinct(), rownames = FALSE, class = "compact stripe", filter = "top", selection = "single", options = list(dom = 'ltpr', pageLength = 15, lengthChange = FALSE), caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: GWAS Catalog published studies. ", "</b>","<em>","To download any of the studies from the table first click on the row of the study of interest (1) and click the download button below the table (2).","</em>")))))}, server = FALSE)
  output$gwas_catalog_temp <- DT::renderDT({return(DT::datatable(
    
    if(input$tick_gwascatalog == FALSE){
      forout_reactive$gwas_catalog_temp
    } else {
      forout_reactive$gwas_catalog_temp %>% select(variant_id, pvalue_description, chromosome_position, chromosome_name, pvalue, functional_class, trait)
    }
    , rownames = FALSE, class = "compact stripe", filter = "top", selection = "single", options = list(dom = 'ltpr'), caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: justify;', HTML(paste("<b>","Table: GWAS Catalog published studies. ", "</b>","<em>","To download any of the studies from the table first click on the row of the study of interest (1) and click the download button below the table (2). In CoffeeProt, the second column in the user-uploaded data is used as the mapped trait in the QTL. In the GWAS Catalog most studies report the mapped traits in the <em>trait</em> column but some show more detailed information in the <em>pvalue_description</em> column. In some cases it may be preferred to use the latter column as the mapped trait.","</em>")))))}, server = FALSE)
  
  
  # Download table handlers ----
  output$download_protannotable       <- cp_dl_table_csv(forout_reactive$protanno, "protein_annotated.csv")
  output$download_qtltallytable       <- cp_dl_table_csv(forout_reactive$table_qtl_tally, "pqtl_tally.csv")
  output$download_qtltable            <- cp_dl_table_csv(forout_reactive$table_qtl_processed, "pqtl_table.csv")
  output$download_intraintertable     <- cp_dl_table_csv(forout_reactive$table_intra_inter, "pqtl_intragenic_intergenic_table.csv")
  output$download_intrainter_summarized_table     <- cp_dl_table_csv(forout_reactive$table_intra_inter_summarized, "pqtl_intragenic_intergenic_summarized_table.csv")
  output$download_phenotallytable     <- cp_dl_table_csv(forout_reactive$table_pheno_tally, "phenotype_tally.csv")
  output$download_phenotable          <- cp_dl_table_csv(forout_reactive$table_pheno_processed, "phenotype_table.csv")
  output$download_corumcomplextable   <- cp_dl_table_csv(forout_reactive$table_complex_tally, "corum_complex_tally.csv")
  
  # Download complex table
  output$download_complextable        <- downloadHandler(
    filename = function() { paste("protein_complex.zip") },
    
    content = function(file) {
      
      sendSweetAlert(session = session, title = "Starting download!", text = "The download will start as soon as the table is prepared", type = "info", btn_labels = NA, closeOnClickOutside = FALSE)
      gc()
      
      if(exists("tmpdir")){
        unlink(tmpdir)
        exists("tmpdir")
        print("deleting tempdir")
      }
      
      fs <- c()
      tmpdir <<- tempdir()
      print(tmpdir)
      
      path <- paste0(tmpdir, "\\", "protein_complex.csv")
      fs <- c(fs, path)
      
      if(nrow(forout_reactive$table_complex %>% filter(log10(pval) < input$complextable_dl_filter)) > 1000000){
        write.csv(x = forout_reactive$table_complex %>% filter(log10(pval) < input$complextable_dl_filter) %>% slice_min(., pval, n = 1000000) %>% select(varID1, varID2, VarVar, everything()), file = path, row.names = FALSE)
      } else {
        write.csv(x = forout_reactive$table_complex %>% filter(log10(pval) < input$complextable_dl_filter) %>% select(varID1, varID2, VarVar, everything()), file = path, row.names = FALSE)
      }
      
      
      zip(zipfile=file, files=fs, flags = "-j")
      
      sendSweetAlert(session = session, title = "Table prepared!", text = "The download will start now", type = "success")
      
    }
  )
}