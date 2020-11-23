#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Data source:
  useData <- reactive({
    # if (input$dataSource == "DASS"){
      # out <- Data_long %>% filter(truegraph == input$dataSource)
    # } else {
      # out <- Data_long %>% filter(dataSource == "GROUP")
    # }

    Data_long %>% filter(method %in% input$estimators)
  })
  

    output$truenetwork <- renderPlot({
    
      
      truegraph <- Graphs[[input$dataSource]]

      if (ncol(truegraph) == nrow(truegraph)){
        truegraph <- as.matrix(truegraph)
        cluster <- rep("singlecluster",ncol(truegraph))
      } else {
        cluster <- truegraph[[ncol(truegraph)]]
        truegraph <- as.matrix(truegraph[,-ncol(truegraph)])
      }
      
      qgraph(truegraph, layout = "spring", theme = "colorblind",
                    cut = 0,groups = cluster, legend = FALSE, vsize = 7, labels = FALSE)
      
      
      # if (input$dataSource == "DASS"){
      #   # Read true network:
      #   DASSnet <- as.matrix(read.csv("DASSnetwork.csv"))
      #   items <- read.csv("itemDescriptions.csv", stringsAsFactors = FALSE)
      #   
      #   
      #   # Names;
      #   names <- items$description[!is.na(items$shortid)]
      #   labs <- items$id[!is.na(items$shortid)]
      #   groups <- items$scale[!is.na(items$shortid)]
      #   item <- items$shortid[!is.na(items$shortid)]
      #   
      #   names <- c("I was aware of dryness of my mouth.", 
      #              "I couldn't seem to experience any positive feeling\nat all.", 
      #              "I experienced breathing difficulty", 
      #              "I tended to over-react to situations.", 
      #              "I found it difficult to relax.", 
      #              "I felt that I had nothing to look forward to.", 
      #              "I felt that I was using a lot of nervous energy.", 
      #              "I felt I wasn't worth much as a person.", 
      #              "I felt that I was rather touchy.", 
      #              "I felt scared without any good reason.", 
      #              "I found it hard to wind down.", 
      #              "I was aware of the action of my heart in the absence\nof physical exertion", 
      #              "I felt down-hearted and blue.", 
      #              "I felt I was close to panic.", 
      #              "I was unable to become enthusiastic about anything.", 
      #              "I was intolerant of anything that kept me from getting\non with what I was doing.", 
      #              "I felt that life was meaningless.", 
      #              "I found myself getting agitated.", 
      #              "I was worried about situations in which I might panic\nand make a fool of myself.", 
      #              "I experienced trembling", 
      #              "I found it difficult to work up the initiative to do\nthings."
      #   )
      #   
      #   
      #   qgraph(DASSnet[order(item),order(item)], layout = "spring", theme = "colorblind",
      #                       cut = 0,groups = groups[order(item)], legend = TRUE, vsize = 7, labels = sort(item),
      #                       nodeNames = names, legend.cex = 0.3)
      #   
      # } else if (input$dataSource == "GROUP"){
      #   
      #   
      #   ### PRS:
      #   load("NetworkModSelect.Rfile")
      #   trueNet <- as.matrix(NetworkModSelect$graph)
      #   
      #   
      #   ## Rename node names ##
      #   nodenames <- c("Feeling sad", "Other people say things with double meaning", "Lack of enthusiasm",
      #                  "Not talkative when with other people", "Messages on TV have special meaning",
      #                  "People are not what they seem (False appearance)", "Persecution", "Lack of emotions",
      #                  "Feeling pessimistic", "Conspiracy", "Important person", "No future", "Special person",
      #                  "Suicidal", "Telepathy", "No interest in others", "Influenced by devices", "Lack of motivation",
      #                  "Crying", "Voodoo", "Lack of energy", "Odd look", "Empty mind", "Thought withdrawal",
      #                  "Lack of activity", "Thought insertion", "Blunted affect", "Thought broadcasting",
      #                  "Lack of spontaneity", "Thought echo", "External control", "Blunted emotions",
      #                  "Hallucinations", "Voices conversing", "Lack of personal hygiene", "Unable to terminate",
      #                  "Lack of hobbies", "Guilty", "Failure", "Feeling tense", "Capgras", "Visual hallucinations",
      #                  "PRS")
      #   
      #   
      #   ## Create groups of nodes ##
      #   groups <- list("Positive Symptoms" = c(2,5,6,7,10,11,13,15,17,20,22,24,26,28,30,31,33,34,41,42),
      #                  "Negative Symptoms" = c(3,4,8,16,18,21,23,25,27,29,32,35,36,37),
      #                  "Depressive Symptoms" = c(1,9,12,14,19,38,39,40),
      #                  "Polygenic Risk Score" = c(43))
      #   
      #   
      #   ### Color function (lighten colors) ###
      #   lighten <- function(x, bright = 0.2){
      #     col <- c(col2rgb(x))/255
      #     col <- 1-((1-bright)*(1-col))
      #     rgb(col[1],col[2],col[3])  
      #   }
      #   
      #   ### Select the color of the nodes ###
      #   color <- c(lighten("rosybrown3"), "#39BEB1", lighten('peachpuff1'), "#ACA4E2")
      #   
      #   L <- structure(c(0.609462046534191, -0.0423455973586461, 0.702316073837142, 
      #                    0.385396166935755, -0.422366867483547, -0.0407156881057192, -0.217440424307598, 
      #                    0.327355673389826, 0.301892679885363, -0.22901654969962, -0.283772696145424, 
      #                    0.250727595279525, -0.331110748874779, 0.114147162607393, -0.398861458140665, 
      #                    1, -0.358600542956156, 0.757377108402333, 0.335918953740378, 
      #                    -0.614920540826671, 0.454148981989655, 0.246207387492337, 0.389972612860693, 
      #                    -0.65731936628128, 0.951140112117755, -0.795160008977542, 0.128086733767918, 
      #                    -0.459904844734162, 0.680147362980305, -0.290730383065296, -0.714477385945526, 
      #                    0.126608433820951, -0.843732274315117, -1, 0.73353878267248, 
      #                    0.872138349896754, 0.947746708274309, 0.626453407173028, 0.438912516275512, 
      #                    0.758264151816831, -0.173017848105056, -0.788338081584589, -0.0355380101305027, 
      #                    -0.334607522478488, -0.676876503012396, 0.520691985667036, 0.431236332421573, 
      #                    0.0213970381619291, -0.97494377070803, -0.0657440532449058, 1, 
      #                    -0.148017200306618, 0.144663586282009, -0.242944082909084, 0.154866239340205, 
      #                    -0.472784329116897, -0.11084879106544, -0.785489448243628, -0.100156242979336, 
      #                    0.598671677979919, 0.252218652203709, -1, -0.681570853141563, 
      #                    0.0704561828917976, -0.647792696760904, 0.738072360389149, 0.671307541362617, 
      #                    0.152881491201338, 0.407654079724319, 0.936184974557405, 0.299796492970212, 
      #                    0.744064559528061, 0.417486901795919, 0.151558758322605, 0.65668566100404, 
      #                    -0.0645426993902926, 0.00833386755391885, -0.0962730382155111, 
      #                    -0.395958589847299, 0.44238547485217, -0.84520391292937, -0.485585684547482, 
      #                    -0.614290029318846, 0.65783051135347, -0.331353606053276, 0.225605557135213), .Dim = c(43L, 2L))
      #   
      #   
      #   labelcol <- rep("black",ncol(NetworkModSelect$graph))
      #   labelcol[groups$`Positive Symptoms`] <- "white"
      #   labelcol[groups$`Negative Symptoms`] <- "white"
      #   
      #    qgraph(NetworkModSelect$graph, layout = L,# fade = fade,
      #                        theme="colorblind", cut=0, nodeNames = nodenames, vsize=4,
      #                        details=FALSE, groups = groups, palette="pastel",
      #                        legend.cex=0.3, label.cex=1)
      # } else {
      #   
      #   library("igraph")
      #   chain <- as.matrix(get.adjacency(watts.strogatz.game(1,9,1,0)))
      #   O <- chain
      #   O[] <- 0
      #   fullNetwork <- rbind(
      #     cbind(chain,O),
      #     cbind(O, chain)
      #   )
      #   
      #   fullNetwork[1,13] <-
      #     fullNetwork[2,12] <- 
      #     fullNetwork[3,11] <- 
      #     fullNetwork[4,10] <- 
      #     fullNetwork[13,1] <-
      #     fullNetwork[12,2] <- 
      #     fullNetwork[11,3] <- 
      #     fullNetwork[10,4] <- 1
      #   
      #   fullNetwork <- 0.3 * fullNetwork
      #   
      #   groups <- rep(c("A","B"),each=9)
      #   
      #  
      #   # load("fried.RData")
      #   # trueNet <- trueNetwork
      #   # groups <- ifelse(Symptoms == "loss","loss","symptom")
      #   # 
      #   qgraph(fullNetwork, layout = "spring",
      #          theme="colorblind", cut=0, vsize=8,
      #          details=FALSE, groups = groups, palette="pastel",
      #          legend.cex=0.3, label.cex=1, legend = FALSE,
      #          labels = FALSE)
      # }
      
      # 
      # qgraph(trueNet, layout = "spring", theme = "colorblind",
      #        cut = 0,groups = groups, legend = TRUE, vsize = 7, labels = item,
      #        nodeNames = names, legend.cex = 0.35)
    })
    
    # output$truecentrality <- renderPlot({
    #   centralityPlot(trueNet, scale = "raw", include = c("Strength","Closeness","Betweenness")) + theme_bw(14)
    # })
    

    ### Preset radar plot:
    presetRadar_reactive <- eventReactive(input$draw_radar_preset,{
      
 
      # Which metrics:
      if (input$metrics_preset == "Edge detection"){
        metrics <- c(  "sensitivity",  "sensitivity\n(signed)",  "sensitivity\n(top 50%)", "sensitivity\n(top 25%)", 
                       "sensitivity\n(top 10%)",
                       "specificity", "precision", "precision\n(top 50%)", "precision\n(top 25%)", "precision\n(top 10%)")
      } else if (input$metrics_preset == "Edge weights & node strength"){
        metrics <-  c("correlation", "correlation\n(absolute)", "1 - bias", "1 - bias\n(true edges)",  "fade maximum\nfalse edge",
                      
                      "node strength\ncorrelation",  "node strengh\nKendall cor",    "node Strength\ntop 1",  "node Strength\ntop 3",  "node Strength\ntop 5")
      }  else if (input$metrics_preset == "closeness & betweenness"){
        metrics <-  c( "closeness\ncorrelation",  "closeness\nKendall cor",    "closeness\ntop 1",  "closeness\ntop 3",  "closeness\ntop 5",
                       
                       
                       "betweenness\ncorrelation",  "betweenness\nKendall cor",    "betweenness\ntop 1",  "betweenness\ntop 3",  "betweenness\ntop 5"   )
      }  else if (input$metrics_preset == "bridge edge detection"){
        metrics <-  c(  "sensitivity\nbridge", "sensitivity\nbridge (signed)",   "sensitivity\nbridge (top\n50%)", "sensitivity\nbridge (top\n25%)", 
                        "sensitivity\nbridge (top\n10%)", 
                        
                        "specificity\nbridge", "precision\nbridge", 
                        "precision\nbridge (top\n50%)", "precision\nbridge (top\n25%)", "precision\nbridge (top\n10%)")
      } else if (input$metrics_preset == "edge replication"){
        metrics <-  c(   "correlation\nreplication ", "replicated\nedges", "replicated\nedges (top 50%)", "replicated\nedges (top 25%)", "replicated\nedges (top 10%)", 
                         "replicated\nzeroes",
                         
                         
                         
                         "replicated\nbridge edges", "replicated\nbridge edges\n(top 50%)", "replicated\nbridge edges\n(top 25%)", "replicated\nbridge edges\n(top 10%)", 
                         "replicated\nbridge zeroes")
      } else if (input$metrics_preset == "edge replication"){
        metrics <-  c(   "correlation\nreplication ", "replicated\nedges", "replicated\nedges (top 50%)", "replicated\nedges (top 25%)", "replicated\nedges (top 10%)", 
                         "replicated\nzeroes",
                         
                         
                         
                         "replicated\nbridge edges", "replicated\nbridge edges\n(top 50%)", "replicated\nbridge edges\n(top 25%)", "replicated\nbridge edges\n(top 10%)", 
                         "replicated\nbridge zeroes")
      } else if (input$metrics_preset == "centrality replication"){
        metrics <-  c(  "node strength\nreplication cor", 
                        "node strength\nreplication Kendall cor",
                        "node strength\nreplicated strongest", 
                        
                        "closeness\nreplication cor", 
                        "closeness\nreplication Kendall cor",
                        "closeness\nreplicated strongest", 
                        
                        "betweenness\nreplication cor", 
                        "betweenness\nreplication Kendall cor",
                        "betweenness\nreplicated strongest")
      } 
      
      plotter_radial(useData() %>% filter(truegraph %in% input$dataSource_radar_preset), metrics, sampleSize = input$sampleSize_preset, data = input$data_preset, 
                     plot = input$plot_preset, transformation = input$transformation_preset, ylim = c(0,1), 
                     arrangement = input$arrangement_preset,
                     palette = input$colorset)
    })
    
    
    
    # Plot:
    output$presetRadar_plot <- renderPlot({
      presetRadar_reactive()
    })
        
    ### Custom radar plot:
    customRadar_reactive <- eventReactive(input$draw_radar_custom,{
      plotter_radial(useData() %>% filter(truegraph%in%input$dataSource_radar_custom), input$metrics_custom, sampleSize = input$sampleSize_custom, data = input$data_custom, 
                     plot = input$plot_custom, transformation = input$transformation_custom, ylim = c(0,1), arrangement = input$arrangement_custom,
                     palette = input$colorset)
    })
    
    # Plot:
    output$customRadar_plot <- renderPlot({
      customRadar_reactive()
    })
    
    # Download plot:
    output$download_radar_preset <- downloadHandler(
      filename ="simulationresult%03d.pdf",
      content = function(file) {
        pdf(file, width = as.numeric(input$width_radar_preset),
            height = as.numeric(input$height_radar_preset))
        print(presetRadar_reactive())
        dev.off()
      }
    )
    
    output$download_radar_custom <- downloadHandler(
      filename ="simulationresult%03d.pdf",
      content = function(file) {
        pdf(file, width = as.numeric(input$width_radar_custom),
            height = as.numeric(input$height_radar_custom))
        print(customRadar_reactive())
        dev.off()
      }
    )
    
    
    ### preset line plot:
    presetLine_reactive <- eventReactive(input$draw_line_preset,{
      
      # Which metrics:
      if (input$metrics_preset_line == "sensitivity"){
        metrics <- c(  "sensitivity",  "sensitivity\n(signed)",  "sensitivity\n(top 50%)", "sensitivity\n(top 25%)", 
                       "sensitivity\n(top 10%)"
                      )
      } else if (input$metrics_preset_line ==  "specificity & precision"){
        metrics <- c( 
                       "specificity", "precision", "precision\n(top 50%)", "precision\n(top 25%)", "precision\n(top 10%)")
    
      } else if (input$metrics_preset_line == "edge weight accuracy"){
        metrics <-  c("correlation", "correlation\n(absolute)", "1 - bias", "1 - bias\n(true edges)",  "fade maximum\nfalse edge")
      }  else if (input$metrics_preset_line ==  "node strength"){
        metrics <-  c("node strength\ncorrelation",  "node strengh\nKendall cor",    "node Strength\ntop 1",  "node Strength\ntop 3",  "node Strength\ntop 5")
     
        
     } else if (input$metrics_preset_line == "closeness"){
       
        metrics <-  c( "closeness\ncorrelation",  "Node strengh\nKendall cor",    "closeness\ntop 1",  "closeness\ntop 3",  "closeness\ntop 5"
                         )
      }  else if (input$metrics_preset_line == "betweenness"){
        
        metrics <-  c( "betweenness\ncorrelation",  "Node strengh\nKendall cor",    "betweenness\ntop 1",  "betweenness\ntop 3",  "betweenness\ntop 5"   )
        
        
      } else if (input$metrics_preset_line ==  "bridge sensitivity"){
        metrics <-  c(  "sensitivity\nbridge", "sensitivity\nbridge (signed)",   "sensitivity\nbridge (top\n50%)", "sensitivity\nbridge (top\n25%)", 
                        "sensitivity\nbridge (top\n10%)")
      }  else if (input$metrics_preset_line ==   "bridge specificity & precision"){
        metrics <-  c( "specificity\nbridge", "precision\nbridge", 
                        "precision\nbridge (top\n50%)", "precision\nbridge (top\n25%)", "precision\nbridge (top\n10%)")
      } else if (input$metrics_preset_line == "edge replication"){
        metrics <-  c(   "correlation\nreplication ", "replicated\nedges", "replicated\nedges (top 50%)", "replicated\nedges (top 25%)", "replicated\nedges (top 10%)", 
                         "replicated\nzeroes")
        
      } else if (input$metrics_preset_line == "bridge replication"){
        metrics <-  c( "replicated\nbridge edges", "replicated\nbridge edges\n(top 50%)", "replicated\nbridge edges\n(top 25%)", "replicated\nbridge edges\n(top 10%)", 
                         "replicated\nbridge zeroes")
      } else if (input$metrics_preset_line == "centrality replication"){
        metrics <-  c(  "node strength\nreplication cor", 
                        "node strength\nreplicated strongest", 
                        
                        "closeness\nreplication cor", 
                        "closeness\nreplicated strongest", 
                        
                        "betweenness\nreplication cor", 
                        "betweenness\nreplicated strongest")
      } 
      
      plotter_line(useData() %>% filter(truegraph %in% input$dataSource_line_preset),  metrics = input$metrics_preset_line, ylim = c(0,1),palette = input$colorset, data = input$data_line_preset, transformation = input$transformation_line_preset)
    })
    
    # Plot:
    output$presetLine_plot <- renderPlot({
      presetLine_reactive()
    })

    
    # Download:
    output$download_line_preset <- downloadHandler(
      filename ="simulationresult%03d.pdf",
      content = function(file) {
        pdf(file, width = as.numeric(input$width_preset_line),
            height = as.numeric(input$height_preset_line))
        print(presetLine_reactive())
        dev.off()
      }
    )
    
    
    ### custom line plot:
    customLine_reactive <- eventReactive(input$draw_line_custom,{
      plotter_line(useData() %>% filter(truegraph %in% input$dataSource_line_custom), input$metrics_custom_line, data = input$data_line_custom, transformation = input$transformation_line_custom, ylim = c(0,1),palette = input$colorset)
    })
    
    # Plot:
    output$customLine_plot <- renderPlot({
      customLine_reactive()
    })
    
    
    # Download:
    output$download_line_custom <- downloadHandler(
      filename ="simulationresult%03d.pdf",
      content = function(file) {
        pdf(file, width = as.numeric(input$width_custom_line),
            height = as.numeric(input$height_custom_line))
        print(customLine_reactive())
        dev.off()
      }
    )
    
    # Summary table:
    
    reactiveTable <- eventReactive(input$create_table,{
      
      useData2 <- useData()
      
      # Only relevant data type and sample size:
      gathered_sub <- useData2[useData2$data %in% input$data_summaryTable & useData2$sampleSize %in% input$sampleSize_summaryTable &
                                 useData2$transformation %in% input$transformation_summaryTable,]
      
      gathered_sub <- gathered_sub %>% filter(metric %in% input$metrics_summaryTable)
      # Drop some columns:
      gathered_sub <- gathered_sub %>% dplyr::select(data,method,sampleSize,transformation,value)
      
      summarized <- gathered_sub %>% dplyr::group_by(method) %>%
        dplyr::summarize(
          mean = round(mean(value, na.rm=TRUE),2),
          median = round(median(value, na.rm=TRUE),2),
          Q05 = round(quantile(value, 0.05, na.rm = TRUE),2),
          Q25 = round(quantile(value, 0.25, na.rm = TRUE),2),
          Q75 = round(quantile(value, 0.75, na.rm = TRUE),2),
          Q95 = round(quantile(value, 0.95, na.rm = TRUE),2)
        )
      
      summarized
    })
    
    output$summaryTable <- renderDataTable({
      
      reactiveTable()
    })
    
    
    # Recommender rules:
    
    ## keep track of elements inserted and not yet removed
    inserted <- c()
    
    observeEvent(input$insertBtn, {
      btn <- input$insertBtn
      id <- paste0('rule_', btn)
      insertUI(
        selector = '#placeholder',
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
          # tags$p(paste('Element number', btn)),
          fluidRow(
            column(3,
                   selectInput(
                     inputId = paste0("recomendor_stattype_",id),
                     label = "",
                     choices = c("mean","median","Q05","Q25","Q75","Q95"),
                     selected = "mean"
                   )),
            column(5,
                   selectInput(
                     inputId = paste0("recomendor_stat_",id),
                     label = "",
                     choices = metricLabels_nonewline,
                     selected = "sensitivity"
                   )
            ),
            # column(5,
            #        sliderInput(
            #          inputId = paste0("recomendor_range_",id),
            #          label = "", 
            #          min = 0,
            #          max = 1,
            #          value = c(0,1)
            #          
            #        )
            # )
            column(4,
                   numericInput(
                     inputId = paste0("recomendor_range_",id),
                     label = "", 
                     min = 0,
                     max = 1,
                     value = 0.75
                     
                   )
            )
          ),
          id = id
        )
      )
      inserted <<- c(id, inserted)
    })
    
    observeEvent(input$removeBtn, {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', inserted[1])
      )
      inserted <<- inserted[-1]
    })
    
    
    # Recommendor data:
    useData_recommender <-  eventReactive(input$give_recommendation,{
      
      # Stats to use:
      stats <- character(0)
      for (i in seq_along(inserted)){
        tmp <- input[[paste0("recomendor_stat_",inserted[i])]]
        stats[i] <- metricCols[match(tmp,metricLabels_nonewline)]
      }
      
      baseData <-  Data %>% 
        filter(sampleSize %in% as.numeric(input$recommender_samplesize),
               data %in% input$recommender_datatype,
               truegraph %in% input$recommender_datasets) %>% 
        group_by(sampleSize,nLevels,truegraph,data,missing,sampleadjust,transformation,
                 method) %>% 
        select_(.dots = stats)
     
      # Mean:
     means <- baseData %>% 
        summarize_at(.vars = stats, mean, na.rm=TRUE)
      
      names(means)[names(means)%in%stats] <- paste0("mean_",stats)
      
      # median
      medians <- baseData %>% 
        summarize_at(.vars = stats, median, na.rm=TRUE)
      
      names(medians)[names(medians)%in%stats] <- paste0("median_",stats)
      
      # Q05:
      Q05s <- baseData %>% 
        summarize_at(.vars = stats, quantile,  probs = 0.05, na.rm=TRUE)
      
      names(Q05s)[names(Q05s)%in%stats] <- paste0("Q05_",stats)
      
      # Q25:
      Q25s <- baseData %>% 
        summarize_at(.vars = stats, quantile,  probs = 0.25, na.rm=TRUE)
      
      names(Q25s)[names(Q25s)%in%stats] <- paste0("Q25_",stats)
      
      # Q75:
      Q75s <- baseData %>% 
        summarize_at(.vars = stats, quantile,  probs = 0.75, na.rm=TRUE)
      
      names(Q75s)[names(Q75s)%in%stats] <- paste0("Q75_",stats)
      
      # Q95:
      Q95s <- baseData %>% 
        summarize_at(.vars = stats, quantile,  probs = 0.95, na.rm=TRUE)
      
      names(Q95s)[names(Q95s)%in%stats] <- paste0("Q95_",stats)
      
      # Join all:
      joined <- means %>% 
        left_join(medians)  %>% 
        left_join(Q05s)  %>% 
        left_join(Q25s)  %>% 
        left_join(Q75s)  %>% 
        left_join(Q95s)
      
      

      return(joined)
    })
      
    #                  .funs = list(
    #                  mean = round(mean(., na.rm=TRUE),2),
    #                  median = round(median(., na.rm=TRUE),2),
    #                  Q05 = round(quantile(., 0.05, na.rm = TRUE),2),
    #                  Q25 = round(quantile(., 0.25, na.rm = TRUE),2),
    #                  Q75 = round(quantile(., 0.75, na.rm = TRUE),2),
    #                  Q95 = round(quantile(., 0.95, na.rm = TRUE),2)
    #     ))
    # })
    
    
    ### Recommendation table:
    reactiveRecommendationTable <- eventReactive(input$give_recommendation,{
      
      statements <- list()
     
      # Data:
      curData_recommender <- useData_recommender()
     
      # Construct statements:
      for (i in seq_along(inserted)){
        stattype <- input[[paste0("recomendor_stattype_",inserted[i])]]
        stat <- input[[paste0("recomendor_stat_",inserted[i])]]
        stat <- metricCols[match(stat,metricLabels_nonewline)]
        
        statid <- paste0(stattype,"_",stat)
        
        range <-  input[[paste0("recomendor_range_",inserted[i])]]
        
        # curData_recommender <- curData_recommender[
        #   curData_recommender[[statid]] <= range[2] & 
        #   curData_recommender[[statid]] >= range[1],
        #   ]
        curData_recommender <- curData_recommender[
            curData_recommender[[statid]] >= range,
          ]
        
        
        # 
        # statements[[i]] <- as.formula(
        #   paste0(
        #     statid, "<=", range[1], " & ", statid, ">=", range[2]
        #   )
        
      }
    
      # Select only relevant columns:
      curData_recommender <- curData_recommender %>% ungroup %>% dplyr::select(
        truegraph,method,transformation
      ) %>% group_by(method, transformation) %>%
        summarize(datasets = n(),
                  `total datasets` = paste0(" / ",length(input$recommender_datasets)),
                  graphs = paste(truegraph, collapse = "; ")) %>%
        arrange(method,transformation)
      
      return(na.omit(curData_recommender))
    })
    
    output$recommendationTable <- renderDataTable({
      
      reactiveRecommendationTable()
    })
    
    output$graphdescriptives <- renderUI({
      source <- input$dataSource
      
     tags$div(
        "Graph: ",source,tags$br(),
        "Number of nodes: ",descriptives$graph_nNode[descriptives$truegraph==source],tags$br(),
        "Number of clusters: ",descriptives$graph_nCluster[descriptives$truegraph==source],tags$br(),
        "Sparsity: ",round(descriptives$graph_sparsity[descriptives$truegraph==source],2),tags$br(),
        "Average absolute edge-weight: ",round(descriptives$graph_avg_abs_weight[descriptives$truegraph==source],2),tags$br(),
        "Average shortest path-length: ",round(descriptives$smallworld[descriptives$truegraph==source],2),tags$br(),
        "Smallworldness: ",round(descriptives$smallworld[descriptives$truegraph==source],2),tags$br()
        
     )

      
    })
    
})


