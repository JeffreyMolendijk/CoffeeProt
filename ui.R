# Packages and databases are loaded in the global.R file
source("./global.R")

### UI ----
ui <- dashboardPage(
  
  # App title and header ----
  title = "CoffeeProt",
  
  # The CoffeeProt logo in the header is read from the coffeeprot.svg file in the www folder
  # Clicking the logo opens a link to the CoffeeProt BiorXiv pre-print
  dashboardHeader(title = tags$a(href='https://doi.org/10.1101/2020.10.02.323246', target = '_blank',
                                 tags$img(src=paste0("coffeeprot.svg"), height = "70%", width = "auto", align = "middle"))),
  
  # Sidebar menu with subitems ----
  dashboardSidebar(
    sidebarMenu(menuItem("Welcome", tabName = "landing", icon = icon("book-open")),
                menuItem("Protein/transcript data", tabName = "ProtWorkflow", icon = icon("fingerprint")),
                menuItem("pQTL/eQTL data", tabName = "QTLWorkflow", icon = icon("dna")),
                menuItem("GWAS/molQTL", tabName = "PhenoWorkflow", icon = icon("users")),
                menuItem("Analysis", startExpanded = FALSE ,icon = icon("chart-bar"),
                         menuSubItem("Correlation Summary", tabName = "Analysis_Summary"),
                         menuSubItem("Database", tabName = "Analysis_Database"),
                         menuSubItem("SNP-Protein", tabName = "Analysis_ProteinQTL"),
                         menuSubItem("Network", tabName = "Analysis_Network"),
                         menuSubItem("Bait Network", tabName = "Analysis_Network_Bait")),
                menuItem("Plot & Table export", tabName = "Report", icon = icon("file-download"))), br(), br(), br(),
    h5("Progress"),
   progressBar(id = "pb2", title = "Processing: Protein/transcript", value = 0, status = "success", size = "xs"),
   progressBar(id = "pb3", title = "Processing: pQTL/eQTL", value = 0, status = "success", size = "xs"),
   progressBar(id = "pb4", title = "Processing: molQTL", value = 0, status = "success", size = "xs"),
   progressBar(id = "pb5", title = "Analysis: Correlation summary", value = 0, status = "success", size = "xs"),
   progressBar(id = "pb6", title = "Analysis: Database", value = 0, status = "success", size = "xs"),
   progressBar(id = "pb9", title = "Analysis: SNP-Protein", value = 0, status = "success", size = "xs"),
   progressBar(id = "pb7", title = "Analysis: Network", value = 0, status = "success", size = "xs"),
   progressBar(id = "pb8", title = "Analysis: Baitnetwork", value = 0, status = "success", size = "xs")),
    
  # Main panel for analysis inputs and visualizations ----
  dashboardBody(
    
    # The global options for the UI and meta tags are defined here
    # This includes the custom.css stylesheet into the body.
    # The meta tags affect the search enginge results when looking for CoffeeProt
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), 
              tags$meta(name = "description", content = "CoffeeProt is an easy to use and interactive tool to analyze proteomics and GWAS (pQTL / molQTL) data. Discover functional protein - QTL - phenotype associations."),
              tags$meta(name = "keywords", content = "protein, proteomics, genomics, GWAS, QTL, pQTL, molQTL, phenotype, analysis, visualization")),
    
    shinyjs::useShinyjs(),
    
    tabItems(
      
      # The tabs below contains the UI layout for the app
      
      # Tabitem Welcome ----
      tabItem(tabName = "landing",
              div(class = "jumbotron", HTML("<center><h1>Welcome to CoffeeProt!</h1></center>"), HTML("<center><p>For the integration of QTL and Protein/transcript data.</p></center>")),
              fluidRow(
                column(6, fluidRow(div(class = "col-sm-12", div(class = "box box-primary", style = "padding-right: 5%; padding-left: 5%; font-size:110%", NULL, div(class = "box-body", shiny::includeMarkdown("README.md")))),
                )),
                column(6, fluidRow(box(title = "Demo datasets", status = "primary", solidHeader = FALSE, width = 12, actionButton(inputId = "demobutton_files", label = "Load demo data (Parker): Proteomics, pQTL, molQTL", icon = icon("play-circle"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), br(), br(), "OR", br(), br(), "Download demo data", selectizeInput("demoset", label = NULL, choices = list("" ,"Parker HMDP" = "parker"), selected = "parker", options = list(placeholder = 'Select dataset')) ,htmlOutput("demoref"), uiOutput("dl_demoset_ui"))),
                       fluidRow(tabBox(title = "Demo tables", width = 12,
                                       tabPanel("Protein", DT::DTOutput("demotable_protein")),
                                       tabPanel("pQTL", DT::DTOutput("demotable_qtl")),
                                       tabPanel("molQTL / GWAS", DT::DTOutput("demotable_pheno")) )),
                       fluidRow(tabBox(title = "Example plots", width = 12,
                                       tabPanel("correlation", br(), img(src = "images/plot_summary_histogram_cor.png", height = "80%", width = "80%"), HTML(paste("<p style='text-align:justify'>","<b>","Figure: Correlation coefficient histogram. ", "</b>","<em>","Correlation analyses can be performed using protein/transcript data to identify correlated protein or transcript pairs. This plot indicates the number of pairs that would be considered to be 'correlated' at the user-selected correlation coefficient cut-off of 0.5. These plots are useful to judge appropriate cut-offs for other analyses that use the same parameters, such as the network analyses.","</em>","</p>"))),
                                       tabPanel("database", br(), img(src = "images/plot_database_inCORUM.png", height = "80%", width = "80%"), HTML(paste("<p style='text-align:justify'>","<b>","Figure: Correlation database enrichment plot. ", "</b>","<em>","The protein or transcript pairs which are considered to be correlated, based on the user-defined cutoffs (correlation coefficient and 1-value), can be further analyzed using enrichment plots. In this example, the fraction of correlated (true) and non-correlated pairs (false) found in the CORUM protein-protein interaction database are shown. As expected, the correlation pairs detected using CoffeeProt are in agreement with the database, leading to a fold change enrichment of <50. Please note that the relatively low percentages on the y-axis are due to the large number of potential correlation pairs (several million) compared to the protein pairs in the database (~30.000).","</em>","</p>"))),
                                       tabPanel("QTL-protein", br(), img(src = "images/plot_manhattan.png", height = "80%", width = "80%"), img(src = "images/plot_edge.png", height = "80%", width = "80%"), img(src = "images/plot_arcdiagram.png", height = "80%", width = "80%"), br(), br(), HTML(paste("<p style='text-align:justify'>","<b>","Figure: SNP-Protein interactions. ", "</b>","<em>","The SNP-protein interaction plot is constructed from three separate plots. A Manhattan plot (top) highlights the QTL p-values per chromosome. Edges are drawn (center) connecting QTL and protein data, where edge color indicates the QTL type. Protein-Protein interactions are shown using arc-diagrams, proteins are ordered by complex size and number of connections.","</em>","</p>"))),
                                       tabPanel("network", br(), HTML(paste("<p style='text-align:justify'>","<b>","Correlation network plot. ", "</b>","<em>","Interactive correlation networks are produced based on the user-uploaded protein/transcript data. These networks are highly customizable, allowing for the addition of pQTLs/eQTLs and molQTLs, colouring the links by a variable of interest (variant effect, intragenic SNPs). Additionally, known drug-gene interaction data can be added to aid the identification of proteins/transcripts with known drug-interactions.","</em>","</p>")), br(), tags$button(tags$a(href='images/network_corum_06cor.html', target='blank', 'Click here to open the example network in a new browser tab.')) ),
                                       tabPanel("bait network", br(), HTML(paste("<p style='text-align:justify'>","<b>","Correlation bait network plot. ", "</b>","<em>","Interactive correlation bait networks are produced based on the user-uploaded protein/transcript data. Users can select a single, or multiple proteins/transcripts of interest, to visualize all interacting targets associated traits. Alternatively, users can select traits of interest to show all associated proteins/transcripts. In this example, a bait network was made for the trait CE (cholesterol ester lipid measurement), highlighting interactions with proteins including Tmem97 and Aldh1a7. These networks are highly customizable, allowing for the addition of drug-gene interactions or colouring the links by a variable of interest (variant effect, intragenic SNPs). ","</em>","</p>")), br(), tags$button(tags$a(href='images/baitnetwork_CE.html', target='blank', 'Click here to open the bait network in a new browser tab.')) ))),
                       ))),
      
      
      # Tabitem ProtWorkflow ----
      tabItem(tabName = "ProtWorkflow", 
              
              fluidRow(column(6, fluidRow(box(title = "Upload protein/transcript data", status = "primary", solidHeader = FALSE, width = 12,
                                        HTML(paste0('<p align="justify">', "Please start by preparing protein/transcript data files as described on the <code><b>Welcome page</b></code>. Your dataset should contain identifiers in the first column, which could either be gene names, UniProt IDs or ENSEMBL IDs. All other columns should contain numeric data, corresponding to the protein/transcript abundance per sample. CoffeeProt will convert all IDs to gene names after the data is uploaded.", "<p>")), 
                                        uiOutput("protfile_ui"),
                                        uiOutput("prot_select_ui")),
                                        uiOutput("correlation_ui"))),
                      column(6, (uiOutput("prot_annogauge_ui")), fluidRow(uiOutput("cortable_ui"))))),
      
      # Tabitem QTLWorkflow ----
      tabItem(tabName = "QTLWorkflow", 
              
              fluidRow(column(6, fluidRow(box(title = "Upload SNPs associated to a protein/transcript", status = "primary", solidHeader = FALSE, width = 12,
                                       HTML(paste0('<p align="justify">', "Please start by preparing pQTL/eQTL data files as described on the <code><b>Welcome page</b></code>. pQTL/eQTL data files require the columns with information related to the SNP, the affected protein/transcript and a measure of the association.", "<p>")), 
                                       uiOutput("qtl_file_ui"), 
                                       uiOutput("qtl_filtertype_ui"),
                                       uiOutput("qtl_quanttype_ui"),
                                       uiOutput("qtl_pval_ui"),
                                       uiOutput("qtl_anno_ve_ui"),
                                       uiOutput("qtl_uploadfinish_ui")))),
                      column(6, fluidRow(uiOutput("qtl_circos_ui"), uiOutput("qtl_proxydonut_ui"), uiOutput("qtl_tables_ui"))) )),
      
      # Tabitem PhenoWorkflow ----
      tabItem(tabName = "PhenoWorkflow", 
              
              fluidRow(box(title = "GWAS Catalog publicly available datasets", status = "primary", solidHeader = FALSE, width = 12, collapsible = TRUE, collapsed = TRUE,
                           DT::DTOutput("gwas_catalog"), br(), br(), DT::DTOutput("gwas_catalog_temp"), fluidRow(column(width = 4, actionButton("generate_gwascatalog","Retrieve and generate GWAS table"), downloadButton("download_gwascatalog","Download generated table")), column(width = 8, checkboxInput("tick_gwascatalog", label = "Use P-value description column as trait?", value = FALSE)) )),

                column(6, fluidRow(#box(title = "Trait annotation (optional)", status = "primary", solidHeader = FALSE, width = 12, collapsible = TRUE, collapsed = TRUE,
                                   #"Use this tab to annotate traits", uiOutput("pheno_file_anno_ui"), downloadButton("download_pheno_anno_metabolite","Annotate metabolites")),
                                   box(title = "Upload SNPs associated to a phenotype or molecular trait", status = "primary", solidHeader = FALSE, width = 12,
                                       HTML(paste0('<p align="justify">', "Please start by preparing molQTL/GWAS data files as described on the <code><b>Welcome page</b></code>. The GWAS/molQTL format is similar to the pQTL/eQTL files, but only needs the following 6 columns: rsID, phenotype, SNP location, SNP chromosome, p-value and grouping.", "<p>")), 
                                       uiOutput("pheno_file_ui"), 
                                       uiOutput("pheno_filtertype_ui"),
                                       uiOutput("pheno_quanttype_ui"),
                                       uiOutput("pheno_pval_ui"),
                                       uiOutput("pheno_anno_ve_ui"),
                                       uiOutput("pheno_uploadfinish_ui")))),
                column(6, fluidRow(uiOutput("pheno_proxydonut_ui"), uiOutput("pheno_tables_ui"))) )),
      
      # Tabitems Analysis ----
      tabItem(tabName = "Analysis_Summary", 
              fluidRow(column(6,
              fluidRow(box(title = "Instructions", status = "primary", solidHeader = FALSE, width = 12, uiOutput("tick_summary"), HTML("<p align='justify'>This tab displays a summary of the protein-protein correlation analysis. Prior to producing the plots, co-regulation is defined by the user by setting correlation coefficient and q-value cut-offs. The histograms visualize the number of protein-protein interactions that meet these criteria. For each protein, the number of co-regulation partners is determined based on the user-specified criteria.</p>")),
                       uiOutput("parambox_summary_ui"))),
              column(6,
                     fluidRow(uiOutput("resultbox_cor_ui")) ) )),
                     

      tabItem(tabName = "Analysis_Database",
              fluidRow(column(6, fluidRow(box(title = "Instructions", width = 12, status = "primary", solidHeader = FALSE, uiOutput("tick_database"), HTML("<p align='justify'>Analyses are performed after annotating co-regulated protein pairs to determine the extend of overlapping annotations. Protein-protein interaction databases (STRING, CORUM & BioPlex 3.0) are searched to identify previously discovered protein pairs. It is expected that a larger percentage of co-regulated protein pairs is found in these databases, compared to the non co-regulated pairs. It is recommended to adjust the co-regulation criteria if no enrichment is detected.</p>")),
                       uiOutput("parambox_db_ui")) ),
              column(6, fluidRow(uiOutput("resultbox_db_ui")) )),
              
              fluidRow(column(12, fluidRow(uiOutput("sensitivity_ui"))),
              )),
              

      tabItem(tabName = "Analysis_Network", fluidRow(column(6, box(title = "Instructions", status = "primary", width = 12, solidHeader = FALSE, uiOutput("tick_network"), HTML("<p align='justify'>Network plots are used to visualize interactions between co-regulated proteins in interactive plots. The user can produce networks for 1) All protein interactions, 2) all protein interactions involved in QTLs, 3) protein interactions in the CORUM database or 4) protein interactions in the BioPlex 3.0 database. If QTLs have been uploaded they can be added directly to the network plots. Finally, the nodes and edges in the interactive plot can be colored by nodetypes (protein / SNP) and the user-uploaded proxies or annotations. The interactive plot allows zooming in on, moving and highlighting sections of the network. <br> <br> <b>Downloading network plots: </b> Network plots can be downloaded as interactive HTML files using the download button next to the plot button. These HTML files can be opened in most browsers and saved to a PDF file using the save to PDF option (control + p). On some browsers the network plots can be directly saved to a SVG or PDF file (Right click network plot > save as ...)</p>")) ),
                                                     column(6, fluidRow(shinyjs::hidden(box(id = "hiddenbox_nw", title = "Plot parameters (optional)", status = "primary", width = 12, collapsible = TRUE, collapsed = TRUE, sliderInput("nodechargenw", "Node distance parameter", -50, -1, -30), selectInput("nodelabelsnw", "Select node labels to display", c("All", "No SNP labels", "None"), selected = "All", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)) ), 
                                                                         uiOutput("parambox_nw_ui")) )),
                                            fluidRow(column(12, uiOutput("resultbox_nw_ui") ) ) ),
      
      tabItem(tabName = "Analysis_Network_Bait", fluidRow(column(6, box(title = "Instructions", width = 12, status = "primary", solidHeader = FALSE, uiOutput("tick_baitnetwork"), HTML("<p align='justify'>Bait network plots are used to visualize interactions between co-regulated proteins in interactive plots. The bait refers to a single, or list of, proteins or phenotypes of interest.The nodes and edges in the interactive plot can be colored by nodetypes (protein / SNP) and the user-uploaded proxies. The interactive plot allows zooming in on, moving and highlighting sections of the network. <br> <br> <b>Downloading network plots: </b> Network plots can be downloaded as interactive HTML files using the download button next to the plot button. These HTML files can be opened in most browsers and saved to a PDF file using the save to PDF option (control + p). On some browsers the network plots can be directly saved to a SVG or PDF file (Right click network plot > save as ...)</p>")) ),
                                                          column(6, fluidRow(shinyjs::hidden( box(id = "hiddenbox_baitnw", title = "Plot parameters (optional)", status = "primary", width = 12, collapsible = TRUE, collapsed = TRUE, sliderInput("nodecharge", "Node distance parameter", -50, -1, -30), selectInput("nodelabels", "Select node labels to display", c("All", "No SNP labels", "None"), selected = "All", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)) ), uiOutput("parambox_baitnw_ui") ) )),
                                                 fluidRow(column(12, uiOutput("resultbox_baitnw_ui")) ),
                                                 fluidRow(column(12, uiOutput("nw_baitselect_table_links") )) ),
      
      
      tabItem(tabName = "Analysis_ProteinQTL",  fluidRow(column(6, fluidRow(box(title = "Instructions", status = "primary", solidHeader = FALSE, width = 12, uiOutput("tick_qtlprot"), HTML("<p align='justify'>The SNP-Protein plot summarizes the interactions in the uploaded data by combining several visualizations. A Manhattan plot (top) highlights the QTL p-values per chromosome. Edges are drawn (center) connecting QTL and protein data, where edge color indicates the QTL type. Protein-Protein interactions are shown using arc-diagrams, proteins are ordered by complexsize and number of connections. The user can alter the plots by selecting a single chromosome or proteincomplex of interest.</p>")))),
                                                         column(6, fluidRow(uiOutput("parambox_qtlprot_ui")))), 
              fluidRow(column(12, fluidRow(uiOutput("resultbox_qtlprot_ui") )))),
      
  
      # Tabitem Report ----
      tabItem(tabName = "Report",
              fluidRow(box(title = "Export all plots & tables", status = "primary", width = 6, HTML("<p align='justify'>Use this tab to download <b>all plots </b> or <b>all tables</b> compressed in a Zip folder. The plots are exported at the default square ratio and in .svg file format. To export plots in different dimensions or file types, use the individual plot export option.</p>"), downloadButton("download_plot_zip", "Download all plots (Zip)"), downloadButton("download_table_zip", "Download all tables (Zip)"))),
              fluidRow(box(title = "Export individual plots", status = "primary", width = 6,  uiOutput("downloadplotselect_ui"), selectInput("downloadplotfiletype", "Select filetype", choices = c("svg", "pdf", "png", "eps", "tiff", "bmp"), selected = "svg"), pickerInput("plotdims", "Select plot dimensions", choices = list(Size = c("Large" = 10, "Medium" = 8, "Small" = 5, "Extra small" = 3), Shape = c("Wide"=1.6, "Square"=1)), selected = list(8, 1.6), multiple = T, options = list('max-options-group' = 1)),downloadButton("download_plot", "Download plot"))))
)))
