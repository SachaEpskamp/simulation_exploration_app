# Install packages if needed:
install_if_needed <- function(pkg){
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dep = TRUE)
    library(pkg, character.only = TRUE)
  }
}


install_if_needed('shinycssloaders')
install_if_needed("shinyWidgets")
install_if_needed("shinythemes")
install_if_needed("shiny")
install_if_needed("ggplot2")
install_if_needed("dplyr")
install_if_needed("qgraph")
install_if_needed("RColorBrewer")
install_if_needed("tidyr")
install_if_needed("REdaS")

# From Github:
if (!require("ggh4x")){
  install_if_needed("devtools")
  devtools::install_github("teunbrand/ggh4x")
  library("ggh4x")
}

# Spinners:
 options(
   spinner.type = 5,
   spinner.color = brewer.pal(10,"Paired")[10]
 ) 


# Read graphs:
graphfiles <- list.files(path = "weights/", pattern = "_weights",full.names = TRUE)
Graphs <- graphfiles %>% lapply(read.csv)
names(Graphs) <- graphfiles %>% gsub("weights//?","",.) %>% gsub("\\_weights\\.csv","",.)
 
# Read data:
# files <- list.files(path = "data/", pattern = "lisa_v13",full.names = TRUE)
# bind_rows(lapply(files,read.table,header=TRUE)) # %>% filter(missing < 0.5)

Data <- readRDS("simresults.RDS")

# Nicer labels for data sused:
Data$truegraph <- factor(Data$truegraph, 
  levels = c("MAGNA_PTSD", "DASS_ggmModSelect_stepwise", "psychTools_bfi_ggmModSelect_stepwise"),
  labels = c("MAGNA", "DASS21", "BFI")
  )

names(Graphs) <- c("MAGNA", "DASS21", "BFI")[match(names(Graphs),  c("MAGNA_PTSD", "DASS_ggmModSelect_stepwise", "psychTools_bfi_ggmModSelect_stepwise"))]


# Read graph descriptives:
descriptives <- read.csv("graph_descriptives.csv", stringsAsFactors = FALSE, sep = ";")
descriptives$truegraph <- factor(descriptives$truegraph, 
                                 levels = c("MAGNA_PTSD", "DASS_ggmModSelect_stepwise", "psychTools_bfi_ggmModSelect_stepwise"),
                                 labels = c("MAGNA", "DASS21", "BFI")
)
descriptives <- descriptives %>% filter(!is.na(truegraph))

# Fix for maxfade:
Data$maxfade_false_edge[!is.finite(Data$maxfade_false_edge)] <- NA

# Make nice factor for missings:
# Data$missingFactor <- factor(Data$missing, levels = sort(unique(Data$missing)),
                             # labels = paste0(100*sort(unique(Data$missing)),"% N/A"))

# Transformation labels:
Data$transformation <- factor(Data$transformation, 
                              levels = c("none", "rank", "npn", "polychoric/categorical"),
                              labels = c("no transformation", "rank / Spearman", "non-paranormal", "polychoric / categorical"))

# Reorder levels in data:
Data$data <- factor(Data$data, levels = 
                      c("normal", "skewed", "uniform ordered", "skewed ordered") )

# Nicer labels:
Data$method <- factor(Data$method,
       levels = c("ggmModSelect", "ggmModSelect_stepwise", 
                  "FIML_prune", "FIML_prune_modelsearch",
                  "WLS_prune", "WLS_prune_stepup",
                  "mgm_CV", "mgm_EBIC", 
                  "GGM_bootstrap","GGM_regression",
                  "BGGM_estimate", "BGGM_explore", "EBICglasso"),
       labels = c("ggmModSelect (stepwise = FALSE, gamma = 0)", "ggmModSelect (stepwise = TRUE, gamma = 0)", 
                  "FIML prune (alpha = 0.01)", "FIML prune -> modelsearch  (alpha = 0.01)",
                  "WLS prune  (alpha = 0.01)", "WLS prune -> stepup  (alpha = 0.01)",
                  "mgm (CV; 10 folds)", "mgm (EBIC; gamma = 0.25)", 
                  "GGM_bootstrap (alpha = 0.01)","GGM_regression",
                 "BGGM (estimate; alpha = 0.05)",
                  "BGGM (explore; BF = 3)",
                 "EBICglasso (gamma = 0.5)"))

# All methods (for ordering):
allMethods <-  c("ggmModSelect (stepwise = FALSE, gamma = 0)", "ggmModSelect (stepwise = TRUE, gamma = 0)", 
                 "FIML prune (alpha = 0.01)", "FIML prune -> modelsearch  (alpha = 0.01)",
                 "WLS prune  (alpha = 0.01)", "WLS prune -> stepup  (alpha = 0.01)",
                 "mgm (CV; 10 folds)", "mgm (EBIC; gamma = 0.25)", 
                 "GGM_bootstrap (alpha = 0.01)","GGM_regression",
                 "BGGM (estimate; alpha = 0.05)",
                 "BGGM (explore; BF = 3)",
                 "EBICglasso (gamma = 0.5)")

Data$method <- factor(Data$method, levels = allMethods)

# Default methods:
defMethods <-  c("ggmModSelect (stepwise = FALSE, gamma = 0)", "ggmModSelect (stepwise = TRUE, gamma = 0)", 
                 "FIML prune (alpha = 0.01)", "FIML prune -> modelsearch  (alpha = 0.01)",
                 "WLS prune  (alpha = 0.01)", "WLS prune -> stepup  (alpha = 0.01)",
                 "mgm (CV; 10 folds)", "mgm (EBIC; gamma = 0.25)", 
                 # "GGM_bootstrap (alpha = 0.01)","GGM_regression",
                 "BGGM (estimate; alpha = 0.05)",
                 "BGGM (explore; BF = 3)",
                 "EBICglasso (gamma = 0.5)")



# Now combine with variant:
# 
Data$sampleSizeFactor <- factor(Data$sampleSize, levels = sort(unique(Data$sampleSize)),
                             labels = paste0("n = ",sort(unique(Data$sampleSize))))

# Rescale opacity:
Data$oneMinOpacity <- 1 - Data$maxfade_false_edge

# Rescale bias:
Data$OneMinBias <- 1 - Data$bias
Data$OneMinBias_true_edges <- 1 - Data$bias_true_edges
Data$OneMinBias_bridge <- 1 - Data$bias_bridge
Data$OneMinBias_true_edges_bridge <- 1 - Data$bias_true_edges_bridge

# Rescale error:
Data$success <- !Data$error

# All the condition cols:
conditionCols <- c("sampleSize", "nLevels", "data", "method", 
                   "sampleSizeFactor","truegraph","transformation")

# All the relevant columns in order:
metricCols <- c("sensitivity",  "sensitivity_signed",  "sensitivity_top50", "sensitivity_top25", 
                "sensitivity_top10",
                
                "specificity", "precision", "precision_top50", "precision_top25", "precision_top10",
                
                "correlation", "abs_cor", "OneMinBias", "OneMinBias_true_edges",  "oneMinOpacity",
                
                "strength_correlation",  "strength_correlation_kendall",    "strength_top1",  "strength_top3",  "strength_top5",
                
                "closeness_correlation", "closeness_correlation_kendall",   "closeness_top1", "closeness_top3", "closeness_top5", 
                
                
                "betweenness_correlation",  "betweenness_correlation_kendall",  "betweenness_top1",  "betweenness_top3", "betweenness_top5", 
                
                "sensitivity_bridge", "sensitivity_signed_bridge",   "sensitivity_top50_bridge", "sensitivity_top25_bridge", 
                "sensitivity_top10_bridge", 
                
                "specificity_bridge", "precision_bridge", 
                "precision_top50_bridge", "precision_top25_bridge", "precision_top10_bridge",
                
                "correlation_replication", "replicated_edges", "replicated_top50", "replicated_top25", "replicated_top10", 
                "replicated_zeroes",
                
                "replicated_edges_bridge", 
                "replicated_top50_bridge", "replicated_top25_bridge", "replicated_top10_bridge", 
                "replicated_zeroes_bridge", 
                
                "strength_correlation_replication", 
                "strength_correlation_kendall_replication",
                "strength_top1_replication", 
                
                "closeness_correlation_replication", 
                "closeness_correlation_kendall_replication", 
                "closeness_top1_replication",
                
                "betweenness_correlation_replication", 
                "betweenness_correlation_kendall_replication", 
                "betweenness_top1_replication", 
                
                "difftime", "success")

metricLabels <- c("sensitivity",  "sensitivity\n(signed)",  "sensitivity\n(top 50%)", "sensitivity\n(top 25%)", 
                "sensitivity\n(top 10%)",
                
                "specificity", "precision", "precision\n(top 50%)", "precision\n(top 25%)", "precision\n(top 10%)",
                
                "correlation", "correlation\n(absolute)", "1 - bias", "1 - bias\n(true edges)",  "fade maximum\nfalse edge",
                
                "node strength\ncorrelation",  "node strengh\nKendall cor",    "node Strength\ntop 1",  "node Strength\ntop 3",  "node Strength\ntop 5",
                
                "closeness\ncorrelation",  "closeness\nKendall cor",    "closeness\ntop 1",  "closeness\ntop 3",  "closeness\ntop 5",
                
                
                "betweenness\ncorrelation",  "betweenness\nKendall cor",    "betweenness\ntop 1",  "betweenness\ntop 3",  "betweenness\ntop 5",
                
                "sensitivity\nbridge", "sensitivity\nbridge (signed)",   "sensitivity\nbridge (top\n50%)", "sensitivity\nbridge (top\n25%)", 
                "sensitivity\nbridge (top\n10%)", 
                
                "specificity\nbridge", "precision\nbridge", 
                "precision\nbridge (top\n50%)", "precision\nbridge (top\n25%)", "precision\nbridge (top\n10%)",
                
                
                "correlation\nreplication ", "replicated\nedges", "replicated\nedges (top 50%)", "replicated\nedges (top 25%)", "replicated\nedges (top 10%)", 
                "replicated\nzeroes",
                
                
                
                "replicated\nbridge edges", "replicated\nbridge edges\n(top 50%)", "replicated\nbridge edges\n(top 25%)", "replicated\nbridge edges\n(top 10%)", 
                "replicated\nbridge zeroes",
                
                "node strength\nreplication cor", 
                "node strength\nreplication Kendall cor",
                "node strength\nreplicated strongest", 
                
                "closeness\nreplication cor", 
                "closeness\nreplication Kendall cor",
                "closeness\nreplicated strongest", 
                
                "betweenness\nreplication cor", 
                "betweenness\nreplication Kendall cor",
                "betweenness\nreplicated strongest", 
                
                
                "Computation time", "Successful\ncomputations")

metricLabels_nonewline <- c("sensitivity",  "sensitivity (signed)",  "sensitivity (top 50%)", "sensitivity (top 25%)", 
                  "sensitivity (top 10%)",
                  
                  "specificity", "precision", "precision (top 50%)", "precision (top 25%)", "precision (top 10%)",
                  
                  "correlation", "correlation (absolute)", "1 - bias", "1 - bias (true edges)",  "fade maximum false edge",
                  
                  "node strength correlation",  "node strengh Kendall cor",    "node Strength top 1",  "node Strength top 3",  "node Strength top 5",
                  
                  "closeness correlation",  "closeness Kendall cor",    "closeness top 1",  "closeness top 3",  "closeness top 5",
                  
                  
                  "betweenness correlation",  "betweenness Kendall cor",    "betweenness top 1",  "betweenness top 3",  "betweenness top 5",
                  
                  "sensitivity bridge", "sensitivity bridge (signed)",   "sensitivity bridge (top 50%)", "sensitivity bridge (top 25%)", 
                  "sensitivity bridge (top 10%)", 
                  
                  "specificity bridge", "precision bridge", 
                  "precision bridge (top 50%)", "precision bridge (top 25%)", "precision bridge (top 10%)",
                  
                  
                  "correlation replication ", "replicated edges", "replicated edges (top 50%)", "replicated edges (top 25%)", "replicated edges (top 10%)", 
                  "replicated zeroes",
                  
                  
                  
                  "replicated bridge edges", "replicated bridge edges (top 50%)", "replicated bridge edges (top 25%)", "replicated bridge edges (top 10%)", 
                  "replicated bridge zeroes",
                  
                  "node strength replication cor", 
                  "node strength replication Kendall cor",
                  "node strength replicated strongest", 
                  
                  "closeness replication cor", 
                  "closeness replication Kendall cor",
                  "closeness replicated strongest", 
                  
                  "betweenness replication cor", 
                  "betweenness replication Kendall cor",
                  "betweenness replicated strongest", 
                  
                  
                  "Computation time", "Successful computations")

# Default custom metrics:
metricDefault <- c("sensitivity", "sensitivity\n(top 25%)", "specificity", "precision", "precision\n(top 25%)", 
                   "correlation",  "fade maximum\nfalse edge",  "sensitivity\nbridge", "sensitivity\nbridge (top\n25%)",
                   "specificity\nbridge", "precision\nbridge", "precision\nbridge (top\n25%)",
                   "Successful\ncomputations")

metricDefault_line <- c("sensitivity", "specificity",  
                   "correlation",   "sensitivity\nbridge", 
                   "specificity\nbridge")

# To long format data:
Data_long <- Data %>% select_(.dots=c(conditionCols,metricCols)) %>%  gather_("metric","value",metricCols)

# Label everything nicely:
Data_long$metric <- factor(Data_long$metric, levels = metricCols, labels = metricLabels)

### Function for the radial plots:
plotter_radial <- function(gathered, metrics, sampleSize, transformation, data, plot = c("mean","median","Q05","Q25","Q75","Q95"), ylim = c(0,1),
                           arrangement = "data type - sample size - transformation",
                           palette = "paired"){
  # handle arrangement:
  arrangement <- strsplit(arrangement, spl = " - ")[[1]]
  arrangement <- sapply(arrangement,switch,
                        "data type" = "data",
                        "sample size" = "sampleSizeFactor",
                        "transformation" = "transformation")
                                     
  formula <- as.formula(paste0(arrangement[3], "~", arrangement[1]," + ",arrangement[2]))
  facet <- facet_nested(formula)
                                     
  plot <- match.arg(plot)

  # Only relevant data type and sample size:
  gathered <- gathered[gathered$data %in% data & gathered$sampleSize %in% sampleSize & gathered$transformation %in% transformation,]
  
  # Only relevant metrics:
  gathered <- gathered %>% filter(metric %in% metrics)

  summarized <- gathered %>% dplyr::group_by(sampleSize,method,metric,sampleSizeFactor,transformation,data) %>%
    dplyr::summarize(
      mean = mean(value, na.rm=TRUE),
      median = median(value, na.rm=TRUE),
      Q05 = quantile(value, 0.05, na.rm = TRUE),
      Q25 = quantile(value, 0.25, na.rm = TRUE),
      Q75 = quantile(value, 0.75, na.rm = TRUE),
      Q95 = quantile(value, 0.95, na.rm = TRUE)
    )
  
  # Make a dummy x variable:
  summarized$xDummy <- as.numeric(factor(summarized$metric,levels=metrics))
  
  # Arrange:
  summarized <- summarized %>% arrange(xDummy)
  
  # Make one more row for first:
  # summarized <- bind_rows(summarized,summarized %>% filter(xDummy == 1))
  
  # Metric data set:
  metricsDF <- summarized %>% group_by(xDummy) %>% 
    summarize(x = unique(xDummy), label = unique(metric), 
              angle = 90 - unique(rad2deg(2*pi*(xDummy/max(summarized$xDummy))))) %>%
   mutate(
    hjust = ifelse(angle < -90 & angle > -270, 1, 0)
  ) %>% mutate(
    angle = ifelse(angle < -90 & angle > -270,angle + 180,angle)
  )
  
  # Append the entire data frame with a second first metric (a bit silly but works):
  # summarized <- bind_rows(summarized,summarized%>%filter(xDummy==1))
  #https://stackoverflow.com/questions/42562128/ggplot2-connecting-points-in-polar-coordinates-with-a-straight-line-2
  coord_radar <- function (theta = "x", start = 0, direction = 1) {
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") "y" else "x"
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
            direction = sign(direction),
            is_linear = function(coord) TRUE)
  }
  
  # Silly border line:
  borderLineDF <- data.frame(x=seq(0,max(summarized$xDummy), length = 1000), y = 1)
  borderLineDF_25 <- data.frame(x=seq(0,max(summarized$xDummy), length = 1000), y = 0.25)
  borderLineDF_50 <- data.frame(x=seq(0,max(summarized$xDummy), length = 1000), y = 0.5)
  borderLineDF_75 <- data.frame(x=seq(0,max(summarized$xDummy), length = 1000), y = 0.75)

  # Vertical Lines:
  vertLines <- bind_rows(lapply(sort(unique(summarized$xDummy)), function(x){
    data.frame(x=x,y=c(0,1))
  }))

  
  g <- ggplot(na.omit(summarized), aes_string(x = "xDummy", y = plot, colour = "method", label = "metric"))  + 
    geom_line(data=borderLineDF,aes(x=x,y=y,colour = "black", label = ""), colour = "black") + 
    geom_line(data=borderLineDF_50,aes(x=x,y=y,colour = "black", label = ""), colour = "black", alpha = 0.5) + 
    geom_line(data=borderLineDF_25,aes(x=x,y=y,colour = "black", label = ""), colour = "black", alpha = 0.25) + 
    geom_line(data=borderLineDF_75,aes(x=x,y=y,colour = "black", label = ""), colour = "black", alpha = 0.25)  +
    geom_point(data=data.frame(x=0,y=0),aes(x=x,y=y,colour = "black", label = ""), colour = "black", cex = 2 ) + 
    facet +
    geom_line(data=vertLines,aes(x=x,y=y,group=x, label = NULL),color = "black", alpha = 0.25) +
    #  geom_line(alpha = 0.75, lwd = 1.25) +
    geom_polygon(fill = NA, lwd = 1.25,show.legend=FALSE, alpha = 0.75) +
    geom_point(cex = 3) +
    # facet_nested(missingFactor ~ data + sampleSizeFactor) +
    theme_bw(14) +
    xlab("")+
    ylab("") +
    scale_x_continuous(limits = c(0,max(summarized$xDummy)), labels = c(metrics,metrics[1]),
                       breaks = 0:max(summarized$xDummy)) +
    scale_y_continuous(limits=c(0,1.25), breaks = c(0,0.25,0.5,0.75,1), minor_breaks=NULL) +
    scale_fill_discrete("") + scale_colour_discrete("") +
    theme(legend.position = "top") +
    coord_radar()  + 
    geom_text(data=metricsDF,aes(x=x, label = label, angle=angle,
                                 y=1.05, hjust = hjust), colour = "black", cex = 4) +
    theme(axis.text.x = element_blank(), panel.border = element_blank(),
          panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
          legend.title = element_text( size = 10),
          legend.text = element_text( size = 9)) + 
    # scale_colour_brewer("",palette = "Paired") +
  guides(colour=guide_legend(nrow=4)) 
  
  
  
  
  if (palette == "paired"){
    g <- g + scale_colour_brewer("",palette = "Paired")
  } else if (palette == "pastel"){
    g <- g + scale_colour_brewer("",palette = "Set3")
  }
  
  return(g)
}

# arrangement options:

arrangementOptions <- c("data type - sample size - transformation", "data type - transformation - sample size", 
                        "transformation - data type - sample size", "transformation - sample size - data type", 
                        "sample size - transformation - data type", "sample size - data type - transformation"
)

# 
# incl <- c("sensitivity",  "sensitivity\n(signed)",  "sensitivity\n(top 50%)", "sensitivity\n(top 25%)", 
#           "sensitivity\n(top 10%)")
# plotter_radial(Data_long, incl, sampleSize = 500, data = "normal", plot = "mean", missing = 0, ylim = c(0,1))


### Function for the line plots:
plotter_line <- function(gathered, metrics, transformation, data, plot = c("mean","median","Q05","Q25","Q75","Q95"), ylim = c(0,1),
                         palette = "paired"){
  plot <- match.arg(plot)
  
  # Only relevant metrics:
  gathered <- gathered[gathered$metric %in% metrics & gathered$data %in% data & gathered$transformation %in% transformation,]
  
  summarized <- gathered %>% group_by(sampleSize,method,metric,transformation,data) %>% 
    summarize(
      mean = mean(value, na.rm=TRUE),
      median = median(value, na.rm=TRUE),
      Q05 = quantile(value, 0.05, na.rm = TRUE),
      Q25 = quantile(value, 0.25, na.rm = TRUE),
      Q75 = quantile(value, 0.75, na.rm = TRUE),
      Q95 = quantile(value, 0.95, na.rm = TRUE)
    ) 
  
  
  g <- ggplot(summarized, aes_string(x = "as.numeric(as.factor(sampleSize))", y = plot, colour = "method")) + 
    geom_line(alpha = 0.75, lwd = 1.1) + 
    geom_point(cex = 1.5) + 
    facet_nested(metric ~   data + transformation) +
    theme_bw(14) + 
    xlab("Sample size")+
    ylab("") + 
    scale_fill_discrete("") + scale_colour_discrete("") + 
    theme(legend.position = "top") + 
    ggplot2::scale_y_continuous(limits=c(ylim[1],ylim[2]),breaks=seq(ylim[1],ylim[2],length=5)) +
    scale_x_continuous(breaks = seq_along(unique(summarized$sampleSize)), labels = sort(unique(summarized$sampleSize))) + 
    # theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme( panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
    # geom_vline(xintercept=seq(1.5, length(unique(Data$sampleSize))-0.5, 1), 
    # lwd=0.5, colour="black", alpha = 0.25) +
    # scale_colour_brewer("",palette = "Paired") + 
    guides(colour=guide_legend(nrow=4)) + theme(legend.title = element_text(size = 9), 
                                                legend.text = element_text(size = 9)) + 
    theme(axis.text.x = element_text(angle = 90),
          legend.title = element_text( size = 12),
          legend.text = element_text( size = 10)) 
  
  
  if (palette == "paired"){
    g <- g + scale_colour_brewer("",palette = "Paired")
  } else if (palette == "pastel"){
    g <- g + scale_colour_brewer("",palette = "Set3")
  }
  
  return(g)
  
}

# incl <- c("sensitivity",  "sensitivity\n(signed)",  "sensitivity\n(top 50%)", "sensitivity\n(top 25%)",
#           "sensitivity\n(top 10%)")
# plotter_line(Data_long, incl)
# 
