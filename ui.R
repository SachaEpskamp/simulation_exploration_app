#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
navbarPage("Network estimation simulations", theme = shinytheme("cosmo"),
           tabPanel("Introduction",
                    sidebarLayout(
                      

                      sidebarPanel(
                        p("Welcome to the network estimation simulation app. This app was made by Adela Isvoranu and Sacha Epskamp, and is aimed to present results from large-scale simulaton studies on the performance of psychological network estimation from continuous and ordered categorical data. The generating models were GGMs. We generated normal data as well as ordered categorical data (uniformly and skewed) with four levels."),
                        p("Panels in this app:"),
                        tags$ul(
                          tags$li(tags$b("Introduction (this panel):"),"Overview of the app and network models used in the simulation study."), 
                          tags$li(tags$b("Recommender:"),"Can be used to recommend estimators based on given criteria (e.g., specificity > 0.9)."), 
                          tags$li(tags$b("Estimators:"), "Can be used select which estimators are included in results on the remaining tabs."), 
                          tags$li(tags$b("Radar plots (preset):"),"Creates radar plots based on pre-defined groups of outcome measures."), 
                          tags$li(tags$b("Radar plots (custum):"),"Creates radar plots based on manually chosen outcome measures."), 
                          tags$li(tags$b("Line plots (preset):"),"Creates line plots based on pre-defined groups of outcome measures as a function of sample size."), 
                          tags$li(tags$b("Line plots (custum):"),"Creates line plots based on manually chosen outcome measures as a function of sample size."),
                          tags$li(tags$b("Summary tables:"),"Can be used to create a summary table of a specific condition.")
                        ),

                        selectInput('dataSource',
                                    label = "Explore data:",
                                    choices = levels(Data$truegraph),
                                    selected =   "MAGNA"),
                        htmlOutput("graphdescriptives")
                        
                      ),
                      mainPanel(
                        withSpinner(plotOutput("truenetwork", width = "600px", height = "600px"))
                        # withSpinner(plotOutput("truecentrality", height = "400px", width = "600px"))                        
                      )
                    )
           ),
           
           # tabPanel("Explanation",
           #          # p("Welcome to the network estimation simulation app. This app was made by Adela Isvoranu and Sacha Epskamp, and is aimed to present results from three simulaton studies on the performance of network estimation in various settings."),
           #          # p("On the next tab (network and estimators), you can select the simulation study and estimators you wish to investigate. The three simulation studies are based on the following datasets:"),
           #          # tags$ol(
           #          #   tags$li("A synthetic GGM in which two groups of 9 nodes are connected with chain graphs, which are interconnected with four bridge edges. All edge weights were set to 0.3."), 
           #          #   tags$li("The Depression, Anxiety and Stress short-scale (DASS), obtained from the Open Source Psychometrics Project (openpsychometrics.org). The true model was a Gaussian graphical model (GGM), which was estimated using psychonetrics with weighted least squares and stepup estimation. The network contains three subscales: depression items, anxiety items, and stress items; 'bridge edges' indicate edges between nodes of different subscales"), 
           #          #   tags$li("The Genetic Risk and Outcome of Psychosis Project (GROUP), as previously studied in https://doi.org/10.1017/S003329171900045X. The GROUP dataset contains measures of the Community Assessment of Psychic Experiences (CAPE) scale in addition to a polygenetic risk score (PRS) item. Bridge edges are edges between PRS and other nodes in the network. The network structure used to simulate under is the same structure as reported by https://doi.org/10.1017/S003329171900045X (a GGM), which was estimated using the ggmModSelect algorithm. "), 
           #          #   # tags$li("The Changing Lives of Older Couples (CLOC) study, as previously studed in http://dx.doi.org/10.1037/abn0000028. This is a dataset of binary items from the 11-item Center for Epidemiologic Studies Depression Scale (CES-D), as well as a binary item indicating loss of spouse. The model was an Ising model, which was estimated with IsingFit. The parameters from this model were supplied by the first author of http://dx.doi.org/10.1037/abn0000028, Eiko Fried.")
           #          # ),
           #          # p("The generating model were GGMs. We generated normal data as well as ordered categorical data (uniformly and skewed) with four levels. Next, we applied various GGM estimators, as further discussed in the paper.")
           #          # 
           #          p("Welcome to the network estimation simulation app. This app was made by Adela Isvoranu and Sacha Epskamp, and is aimed to present results from three simulaton studies on the performance of network estimation in various settings."),
           #          p("On the next tab (network and estimators), you can select the simulation study and estimators you wish to investigate."),
           #          # tags$ol(
           #          #   tags$li("A synthetic GGM in which two groups of 9 nodes are connected with chain graphs, which are interconnected with four bridge edges. All edge weights were set to 0.3."), 
           #          #   tags$li("The Depression, Anxiety and Stress short-scale (DASS), obtained from the Open Source Psychometrics Project (openpsychometrics.org). The true model was a Gaussian graphical model (GGM), which was estimated using psychonetrics with weighted least squares and stepup estimation. The network contains three subscales: depression items, anxiety items, and stress items; 'bridge edges' indicate edges between nodes of different subscales"), 
           #          #   tags$li("The Genetic Risk and Outcome of Psychosis Project (GROUP), as previously studied in https://doi.org/10.1017/S003329171900045X. The GROUP dataset contains measures of the Community Assessment of Psychic Experiences (CAPE) scale in addition to a polygenetic risk score (PRS) item. Bridge edges are edges between PRS and other nodes in the network. The network structure used to simulate under is the same structure as reported by https://doi.org/10.1017/S003329171900045X (a GGM), which was estimated using the ggmModSelect algorithm. "), 
           #          #   # tags$li("The Changing Lives of Older Couples (CLOC) study, as previously studed in http://dx.doi.org/10.1037/abn0000028. This is a dataset of binary items from the 11-item Center for Epidemiologic Studies Depression Scale (CES-D), as well as a binary item indicating loss of spouse. The model was an Ising model, which was estimated with IsingFit. The parameters from this model were supplied by the first author of http://dx.doi.org/10.1037/abn0000028, Eiko Fried.")
           #          # ),
           #          p("The generating models were GGMs. We generated normal data as well as ordered categorical data (uniformly and skewed) with four levels. Next, we applied various GGM estimators, as further discussed in the paper.")
           #          
           # ),
           tabPanel("Recommender",
                    sidebarLayout(
                      sidebarPanel(
                        pickerInput(
                          inputId = "recommender_datasets",
                          label = "Datasets", 
                          choices = levels(Data$truegraph),
                          selected =   "MAGNA",
                          multiple = TRUE
                        ),
                        sliderTextInput(
                          "recommender_samplesize",
                          "Sample size",
                          choices = sort(unique(Data$sampleSize)),
                          grid = FALSE,selected = 1000
                        ),
                        prettyRadioButtons(
                          inputId = "recommender_datatype",
                          label = "Data type:", 
                          choices = c("normal" , "skewed","uniform ordered", "skewed ordered"),
                          inline = TRUE, 
                          # status = "danger",
                          selected = 'normal',
                          fill = TRUE
                        ),
                        # First rule:
                        # fluidRow(
                        #   column(3,
                        #          selectInput(
                        #            inputId = "recomendor_stattype_1",
                        #            label = "Type:", 
                        #            choices = c("mean","median","Q05","Q25","Q75","Q95"),
                        #            selected = "mean"
                        #          )),
                        #   column(4,
                        #          selectInput(
                        #            inputId = "recomendor_stat_1",
                        #            label = "Statistic:", 
                        #            choices = metricLabels,
                        #            selected = "sensitivity"
                        #          )
                        #   ),
                        #   column(5,
                        #          sliderInput(
                        #            inputId = "recomendor_range_1",
                        #            label = "Range:", 
                        #            min = 0,
                        #            max = 1,
                        #            value = c(0,1)
                        #            
                        #          )
                        #   )
                        # ),
                        
                        tags$div(id = 'placeholder'),
                        actionButton('insertBtn', '+',style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
                        actionButton('removeBtn', '-',style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
                        
                        
                        # uiOutput("recommenderRules"),
                        # Add rule:
                        # actionButton("remove_rule", "-"),
                        # actionButton("add_rule", "+"),
                        actionButton(
                          inputId = "give_recommendation",
                          label = "Recommendation"
                        )
                      ),
                      mainPanel(
                        withSpinner(dataTableOutput("recommendationTable"))
                      )
                    )
           ),
           # tabPanel("Dataset explorer",
           #          sidebarLayout(
           #            sidebarPanel(
           #              selectInput('dataSource',
           #                          label = "Data used:",
           #                          choices = levels(Data$truegraph),
           #                          selected =   "MAGNA"),
           #              htmlOutput("graphdescriptives")
           #              
           #            ),
           #            mainPanel(
           #              withSpinner(plotOutput("truenetwork", width = "600px", height = "600px"))
           #              # withSpinner(plotOutput("truecentrality", height = "400px", width = "600px"))                        
           #            )
           #          )
           # ),
           tabPanel("Estimators",
                    sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("estimators",
                                           label="Estimators:",
                                           choices =   allMethods,
                                           selected =   defMethods),
                        radioButtons("colorset","Color palette (for radar and line plots):",choices = c("paired","pastel","ggplot"), selected = "paired")
                        
                      ),
                      mainPanel(
                        p("The following estimators are used in this study:"),
                        tags$ul(
                         tags$li(tags$b("EBICglasso:"),"Estimates 100 regularized networks using the graphical LASSO, and returns the network that optimizes EBIC. Uses the EBICglasso function from the qgraph package.") ,
                         tags$li(tags$b("ggmModSelect (stepwise = FALSE, gamma = 0):"),"Same as the EBICglasso, but uses non-regularized re-estimates of the network structures in model selection and returns the best non-regularized network. Uses the ggmModSelect function from the qgraph package.") ,
                         tags$li(tags$b("ggmModSelect (stepwise = TRUE, gamma = 0):"),"Starts with non-stepwise ggmModSelect, but continues with iterative stepwise estimation, adding and removing edges until fit is optimized."),
                         tags$li(tags$b("FIML prune (alpha = 0.01):"),"Removes edges non-significant at a given alpha level and re-estimates parameters with the removed edges constrained to zero. Uses the ggm, runmodel and prune functions from the psychonetrics package."),
                         tags$li(tags$b("FIML prune -> modelsearch (alpha = 0.01):"),"Same as above, but follows up with extensive stepwise model search to optimize fit. Uses the modelsearch function from psychonetrics."),
                         
                         tags$li(tags$b("WLS prune (alpha = 0.01):"),"Same as FIML prune, but uses the WLS estimator rather than the FIML estimator. The WLS weights matrix is obtained from the data. Ordered categorical data is modeled through polychoric correlations."),
                         tags$li(tags$b("WLS prune -> stepup (alpha = 0.01):"),"Same as above, but follows up with step-up model search to optimize fit. Uses the stepup function from psychonetrics."),
                         
                         tags$li(tags$b("GGM_bootstrap (alpha = 0.01):"),"Thresholds edges that are not significant based on a bootstrapped p-value. Uses GGM_bootstrap from the GGMnonreg package."),
                         tags$li(tags$b("GGM_regression (alpha = 0.01):"),"Performs nodewise stepwise model search. Uses GGM_regression from the GGMnonreg package."),
                         
                         tags$li(tags$b("BGGM (estimate; alpha = 0.05):"),"Uses Bayesian estimation (posterior mean) and thresholds edges for which 0 is in the credibility interval. Uses the estimate and select functions from the BGGM package."),
                         tags$li(tags$b("BGGM (explore; alpha = 0.05):"),"Uses Bayesian estimation (posterior mean) and thresholds edges based on the Bayes Factor. Uses the explore and select functions from the BGGM package."),
                         
                         tags$li(tags$b("mgm (CV; 10 folds):"),"Nodewise regularized mixed graphical model estimation using 10-fold cross-validation. Uses the mgm function from the mgm package."),
                         tags$li(tags$b("mgm (EBIC; gamma = 0.25):"),"Nodewise regularized mixed graphical model estimation using EBIC model selection. Uses the mgm function from the mgm package.")
                        ),
                        p("The GGMnonreg methods are deselected by default because these are not on CRAN. The 'paired' color set uses similar colors for subsequent pairs of estimators with a maximum of 12 (1-2, 3-4, 5-6, 7-8, 9-10, and 11-12).")
                      )
                      
                    )
           ),
           
           tabPanel("Radar plots (preset)",
                    sidebarLayout(
                      sidebarPanel(
                        actionButton("draw_radar_preset", "Create plot"),
                        hr(),
                        selectInput('dataSource_radar_preset',
                                    label = "Data used:",
                                    choices = levels(Data$truegraph),
                                    # selected =   "MAGNA",
                                    selected = levels(Data$truegraph),
                                    multiple=TRUE),
                        selectInput(inputId = "sampleSize_preset", 
                                    label = "Sample size:", 
                                    choices = sort(unique(Data$sampleSize)),
                                    selected = 1000, #sort(unique(Data$sampleSize))[2],
                                    multiple=TRUE),
                        selectInput(inputId = "data_preset", 
                                    label = "Data type:", 
                                    choices = c("normal" , "skewed","uniform ordered", "skewed ordered"),
                                    selected = c("normal"),multiple=TRUE),
                        selectInput(inputId = "transformation_preset", 
                                    label = "Transformation:", 
                                    choices = sort(unique(Data$transformation)),
                                    selected = "no transformation", multiple = TRUE),
                        selectInput(inputId = "metrics_preset", 
                                    label = "Currently included metrics:", 
                                    choices = c("Edge detection","Edge weights & node strength","closeness & betweenness","bridge edge detection","edge replication","centrality replication"),
                                    selected = "Edge detection"),
                        selectInput("plot_preset","What to plot:",
                                    choices =  c("mean","median","Q05","Q25","Q75","Q95"),
                                    selected = "mean"),
                        selectInput("arrangement_preset",label = "Arrangement (main x - sub x - y):",
                                    choices = arrangementOptions, selected = arrangementOptions[1]),
                        hr(),
                        downloadLink('download_radar_preset', 'Download plot as PDF file'),
                        numericInput('width_radar_preset', 'Width', value = 15, min = 1),
                        numericInput('height_radar_preset', 'Height', value = 10, min = 1),
                        
                        width = 2
                      ), 
                      mainPanel(
                        withSpinner(plotOutput("presetRadar_plot", height = "1000px", width = "1000px"))
                      )
                    )
           ),
           
           tabPanel("Radar plots (custom)",
                    sidebarLayout(
                      sidebarPanel(
                        actionButton("draw_radar_custom", "Create plot"),
                        hr(),
                        selectInput('dataSource_radar_custom',
                                    label = "Data used:",
                                    choices = levels(Data$truegraph),
                                    # selected =   "MAGNA",
                                    selected = levels(Data$truegraph),
                                    multiple=TRUE),
                        selectInput(inputId = "sampleSize_custom", 
                                    label = "Sample size:", 
                                    choices = sort(unique(Data$sampleSize)), #sort(unique(Data$sampleSize)),
                                    selected = 1000, multiple = TRUE),
                        selectInput(inputId = "data_custom", 
                                    label = "Data type:", 
                                    choices = c("normal" , "skewed","uniform ordered", "skewed ordered"),
                                    selected = c("normal"),multiple=TRUE),
                        selectInput(inputId = "transformation_custom", 
                                    label = "Transformation:", 
                                    choices = sort(unique(Data$transformation)),
                                    multiple = TRUE,
                                    selected = "no transformation"),
                        selectInput(inputId = "metrics_custom", 
                                    label = "Currently included metrics:", 
                                    choices = metricLabels,
                                    selected = metricDefault,multiple=TRUE),
                        selectInput("plot_custom","What to plot:",
                                    choices =  c("mean","median","Q05","Q25","Q75","Q95"),
                                    selected = "mean"),
                        selectInput("arrangement_custom",label = "Arrangement (main x - sub x - y):",
                                    choices = arrangementOptions, selected = arrangementOptions[1]),
                        hr(),
                        downloadLink('download_radar_custom', 'Download plot as PDF file'),
                        numericInput('width_radar_custom', 'Width', value = 15, min = 1),
                        numericInput('height_radar_custom', 'Height', value = 10, min = 1),
                        
                        width = 2
                        
                        
                      ), 
                      mainPanel(
                        withSpinner(plotOutput("customRadar_plot", height = "1000px", width = "1000px"))
                      )
                      
                    )
           ),
           tabPanel("Line plots (preset)",
                    sidebarLayout(
                      sidebarPanel(
                        actionButton("draw_line_preset", "Create plot"),
                        hr(),
                        selectInput('dataSource_line_preset',
                                    label = "Data used:",
                                    choices = levels(Data$truegraph),
                                    # selected =   "MAGNA",
                                    selected = levels(Data$truegraph),
                                    multiple=TRUE),
                        selectInput(inputId = "data_line_preset", 
                                    label = "Data type:", 
                                    choices = c("normal" , "skewed","uniform ordered", "skewed ordered"),
                                    selected = c("normal"),multiple=TRUE),
                        selectInput(inputId = "transformation_line_preset", 
                                    label = "Transformation:", 
                                    choices = sort(unique(Data$transformation)),
                                    selected = "no transformation", multiple = TRUE),
                        selectInput(inputId = "metrics_preset_line", 
                                    label = "Currently included metrics:", 
                                    choices = c(
                                      "sensitivity",
                                      "specificity & precision",
                                      "edge weight accuracy",
                                      "node strength",
                                      "closeness",
                                      "betweenness",
                                      "bridge sensitivity",
                                      "bridge specificity & precision",
                                      "edge replication",
                                      "bridge replication",
                                      "centrality replication"
                                    ),
                                    selected = "Edge detection"),
                        selectInput("plot_preset_line","What to plot:",
                                    choices =  c("mean","median","Q05","Q25","Q75","Q95"),
                                    selected = "mean"),
                        hr(),
                        downloadLink('download_line_preset', 'Download plot as PDF file'),
                        numericInput('width_preset_line', 'Width', value = 15, min = 1),
                        numericInput('height_preset_line', 'Height', value = 8, min = 1),
                        
                        width = 2
                      ), 
                      mainPanel(
                        withSpinner(plotOutput("presetLine_plot", height = "1000px", width = "1000px"))
                      )
                    )
                    
           ),
           tabPanel("Line plots (custom)",
                    sidebarLayout(
                      sidebarPanel(
                        actionButton("draw_line_custom", "Create plot"),
                        hr(),
                        selectInput('dataSource_line_custom',
                                    label = "Data used:",
                                    choices = levels(Data$truegraph),
                                    # selected =   "MAGNA",
                                    selected = levels(Data$truegraph),
                                    multiple=TRUE),
                        selectInput(inputId = "data_line_custom", 
                                    label = "Data type:", 
                                    choices = c("normal" , "skewed","uniform ordered", "skewed ordered"),
                                    selected = c("normal"),multiple=TRUE),
                        selectInput(inputId = "transformation_line_custom", 
                                    label = "Transformation:", 
                                    choices = sort(unique(Data$transformation)),
                                    selected = "no transformation", multiple = TRUE),
                        selectInput(inputId = "metrics_custom_line", 
                                    label = "Currently included metrics:", 
                                    choices = metricLabels,
                                    selected = metricDefault_line,multiple=TRUE),
                        selectInput("plot_custom_line","What to plot:",
                                    choices =  c("mean","median","Q05","Q25","Q75","Q95"),
                                    selected = "mean"),
                        hr(),
                        downloadLink('download_line_custom', 'Download plot as PDF file'),
                        numericInput('width_custom_line', 'Width', value = 15, min = 1),
                        numericInput('height_custom_line', 'Height', value = 8, min = 1),
                        
                        width = 2
                        
                      ), 
                      mainPanel(
                        
                        withSpinner(plotOutput("customLine_plot", height = "1000px", width = "1000px"))
                        
                      )
                    )
           ),
           tabPanel("Summary tables",
                    
                    
                    fluidRow(
                      column(3,
                             
                             selectInput(inputId = "sampleSize_summaryTable", 
                                         label = "Sample size:", 
                                         choices = sort(unique(Data$sampleSize)),
                                         selected = 1000),
                             selectInput(inputId = "data_summaryTable", 
                                         label = "Data type:", 
                                         choices = c("normal" , "skewed","uniform ordered", "skewed ordered"),
                                         selected = c("normal"),multiple=FALSE),
                             actionButton("create_table", "Create table")
                      ),
                      column(3, offset = 1,
                             selectInput(inputId = "transformation_summaryTable", 
                                         label = "Transformation:", 
                                         choices = sort(unique(Data$transformation)),
                                         selected = "no transformation",multiple=FALSE),
                             selectInput(inputId = "metrics_summaryTable", 
                                         label = "Metric:", 
                                         choices = metricLabels,
                                         selected = c("correlation"),multiple=FALSE)
                             # selectInput('dataSource_summaryTable',
                             #             label = "Data used:",
                             #             choices = levels(Data$truegraph),
                             #             selected =   "Synthetic sparse")
                      ) #,
                      # column(2,offset = 2,
                      #        downloadLink('download_radar_custom', 'Download plot as PDF file'),
                      #        numericInput('width_radar_custom', 'Width', value = 15, min = 1),
                      #        numericInput('height_radar_custom', 'Height', value = 10, min = 1))
                    ),
                    hr(),
                    dataTableOutput("summaryTable")
                    # Table here!
           )
           
)