##################################################################
# Graph Statistics MODULE OF APP 
# App for Project 
# @Author Daniel Bodden
##################################################################


#load libraries
require(shiny, quietly = TRUE)
require(igraph, quietly = TRUE, warn.conflicts = FALSE)
require(visNetwork, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(DT, quietly = TRUE)
require(plotly, quietly = TRUE)

options(shiny.trace = FALSE)

########
#global variables

# Read files in DIR DATA
graphFilename = list.files( path = "data/", include.dirs = FALSE ) 
#dummy variable to create a graph
g <- make_ring(10) %>% set_vertex_attr("name", value = LETTERS[1:10])
#######


####################
# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("SNA App", 
                         
        ######                 
        tabPanel("Dataset & Stats",
             fluidPage(
               fluidRow(
                 actionButton("loadgraphdata",
                            "Load selected graph Data"),
                 selectInput("graphdata",
                           "Choose Graph data:",
                           graphFilename),
                 uiOutput("nodesedges"),
                 uiOutput("uistats")
 
             ) 
            )
        ),
        
        ####
       tabPanel("Graph",
                fluidPage(
                  fluidRow(
                    uiOutput("uitbloverviewrankeddegbet")
                  ),
                  fluidRow(
                    #show plot
                    column(
                      8,
                      visNetworkOutput("graphplot", width = "100%", height = "400px")
                      
                    ),
                    # controls and stats
                    column(
                      4,
                      dataTableOutput("tbloverviewcentrality"),
                      
                      selectInput("selectcentralitymeasureforgraph", 
                                  "Centrality Measure to Plot on Graph:",
                                  c("None"  = "none",
                                    "Degree" = "degree",
                                    "Betweennes" = "betweennes",
                                    "Pagerank" = "pagerank"))
                    )# end column
                    
                  )#end fluidrow
                  
                )#end fluidpage
               )#end tabpanel
              ,
       
       tabPanel("Paths",
                fluidPage(
                  fluidRow(
                    # show stats
                    fluidRow(
                      uiOutput("uistatdistances")
                    )#end fluidrow
                  ),
                  fluidRow(
                    #show controls to select a path
                    uiOutput("uipathselect")
                  )
                 ,
                  fluidRow(
                    # show plot of shortest path between 2 nodes
                      uiOutput("uigraphshortestpath")
                  )
                  
                )),
       tabPanel("Communities",
              mainPanel(
                  #fluidrows
                  fluidRow(
                    uiOutput("uicommunityselect")
                  )),
                  ###end fluidrow
                  #fluidrows
                  fluidRow(
                    uiOutput("uitlbmodularityscores")
                  ),
                  ###end fluidrow
                  fluidRow(
                    column(
                      8,
                    uiOutput("uigraphcommunity")
                    ),
                    column(
                      4,
                      uiOutput("uitlbmembershipofcom")
                    )
                  ),
                  ###end fluidrow
                  #fluidrows
                  fluidRow(
                    column(
                      4,
                      uiOutput("uitlbsizeofcom")
                    )# end column
                    ,
                    # Plot denrogram
                    column(
                      8,
                      uiOutput("uiplotdenrogram")
                    )#end column 
                  )
                  ###end fluidrow
                #)
        )
      
))
### END UI 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ########################################
  # Definitioon of reactive values
  rv <- reactiveValues(
    graph=NULL,
    nredges=c(),
    nrnodes=c(),
    edgedensity=c(),
    degree = data.frame(ID= c(), degree = c() ),
    degreeranked = data.frame(ID= c(), degree = c(),rank=c() ),
    dgDist = c(),
    betw = data.frame(ID= c(), between = c() ), 
    betwranked = data.frame(ID= c(), between = c(), rank=c() ), 
    pagerk = data.frame(ID= c(), pagerankvalue = c() ), 
    combineddegreebetw =c(),
    longestpath = c(),
    meandistance =c(),
    highestbtwid = c(),
    highestdgid  = c(),
    highestpgrid = c(),
    overviewhighestcentrality = data.frame(ID= c(), score = c(), centralitymeasure = c() ),
    ## Var for community Detection
    lpn = c(),
    lpnmodularity =c(),
    fgy = c(),
    fgymodularity =c(),
    ebn = c(), 
    ebnmodularity =c(),
    wlp = c(), 
    wlpmodularity =c(),
    lvn = c(),
    lvnmodularity =c(),
    mpe = c(),
    mpemodularity =c(),
    lev = c(), 
    levmodularity = c(),
    sgs = c(),
    sgsmodularity = c(),
    overviewcommresults = data.frame(algoritme= c(), modularity = c() ),
    membershipofcomtbl = data.frame(),
    sizeofcomtbl = data.frame()
  )
  ########################################
  
  ########################################
  #Event push button load graph data
  observeEvent(
    input$loadgraphdata,
    {
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Loading Data", value = 0)
      
      #set dir of file
      dir <- "data/"
      fulldir <- paste(dir,input$graphdata,sep="") 
      
      #find format of graph data
      formatfile <- NULL
      formatsplit <- strsplit(
        as.character(input$graphdata),".", fixed=TRUE)[[1]][2]
      
      if(formatsplit == "net"){
        formatfile <- "pajek" 
      }
      if(formatsplit =="gml"){
        formatfile <- "gml"
      }
      if(formatsplit =="edges"){
        formatfile <- "csv"
      }
      
      
      ######## READ INTO REACTIVE VALUE
      if (!is.null(formatfile))
        {
        #read graph data
        
        if(formatfile == "csv"){
          
          #read file
          dataset <- read.csv(
            fulldir, 
            header = FALSE, 
            sep = " ", 
            col.names = c("Source", "Target"))
          
          #transform to Igraph  object
          dataset <- graph.data.frame(dataset, directed = T)
          rv$graph <- connect(
            dataset, 
            100, 
            mode = c("all", "out", "in", "total"))
          
          #set nodes names of the ID
          V(rv$graph)$id <- as.numeric(V(rv$graph)$name)
        }
        else{
          # Read graph object
          rv$graph <- read_graph(
            file=fulldir,
            format=formatfile
          )
        }
      
        ###############
        # CLEAN GRAPH
        
        #Simplify, remove self-loops
        rv$graph <- simplify(rv$graph)
        
        ####
        # Count Nodes
        rv$nredges <- vcount(rv$graph)
        ####
        
        ####
        # count Edges
        rv$nrnodes <- ecount(rv$graph)
        ####
        
        ####
        # Calculate Edge Density
        # density of a graph is the ratio of the number of edges and the number of possible edges.
        rv$gpdensity <- edge_density(
          rv$graph, 
          loops = TRUE)
        
        ####
        output$nodesedges <- renderUI({
          str1 <- paste("Nr of Nodes", rv$nrnodes )
          str2 <- paste("Nr of Edges", rv$nredges )
          str3 <- paste("Edge Density", rv$gpdensity)
          
          HTML(paste(str1, str2, str3, sep = '<br/>'))
        })
        ####
        
        ########################################
        progress$set(message = "Calculate Stats", value = 0.3)
        
        #calculate degree en degree distribution
        
        #calc degree
        rv$deg = data.frame(
          V(rv$graph)$id,
          degree(rv$graph))
        
        rv$deg <-setNames(
          rv$deg, 
          c("NodeID","DegreeValue"))
        
        #calc distribution
        rv$dgDist <-  degree_distribution(
          graph = rv$graph,
          cumulative = FALSE,
          mode="all")
        
        #plot distribution
        output$plotdegreedist <- renderPlot(
          plot( 
            x=0:max(rv$deg$DegreeValue), 
            y=1- rv$dgDist, 
            pch=19, 
            cex=1.2, 
            col="orange",
            xlab="Degree", 
            ylab="Cumulative Frequency")
        )
        
        
        ########################################
        
        #Table degree
        #######################################
        
        output$tbldegree = renderDT(
          rv$deg[order(rv$deg$DegreeValue, decreasing=TRUE),],
          options = list(
            searching = FALSE,
            lengthChange = FALSE,       
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"),
            pageLength = 5
          ),
          rownames= FALSE,
          selection = 'none'
        )
        
        #######################################
        
        #######################################
        # BETWEENNESS
        rv$betw <- data.frame(
          V(rv$graph)$id,
          betweenness(rv$graph, v = V(rv$graph), 
                      directed = TRUE))
        
        rv$betw<-setNames(
          rv$betw,
          c("NodeID","BetweennesValue"))
        
        output$tblbetweennes = renderDT(
          rv$betw[order(rv$betw$BetweennesValue, decreasing=TRUE),],
          options = list(
            searching = FALSE,
            lengthChange = FALSE,       
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"),
            pageLength = 5
          ),
          rownames= FALSE,
          selection = 'none'
        )
        #######################################
        
        #######################################
        progress$set(message = "Rendering Plots", value = 0.5)
        
        #plot between vs degree
        #merge
        tbldegbet <- merge(
          rv$deg,
          rv$betw, 
          by.x="NodeID", 
          by.y="NodeID"   )
        
        #plot
        output$plotdegbetw <- renderPlotly({
          plot_ly(
            tbldegbet, 
            x = ~DegreeValue,
            y = ~BetweennesValue, 
            key = ~NodeID,
            type = 'scatter') 
          
        }
        )
        
        #######################################
        #Page RANK
        
        # calc
        rv$pagerk <- data.frame(
          V(rv$graph)$id,
          (page.rank(
            rv$graph, 
            vids = V(rv$graph), 
            directed = TRUE))$vector)
        
        #set names
        rv$pagerk <-setNames(
          rv$pagerk,
          c("NodeID","PageRankValue"))
        
        #create table
        output$tblpagerank = renderDT(
          rv$pagerk[order( rv$pagerk$PageRankValue, decreasing=TRUE),],
          options = list(
            searching = FALSE,
            lengthChange = FALSE,       
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"),
            pageLength = 5
          ),
          rownames= FALSE,
          selection = 'none'
        )
        
        #######################################
        
        
        #######################################
        #Render STATS UI       
        output$uistats <- renderUI(
          tabsetPanel(
            tabPanel("Degree vs Betweennes",   
                     plotlyOutput("plotdegbetw"),
                     
                     #row with options to cut a part of the network on Degree Values
                     fluidRow(
                       column(
                         3,
                         actionButton("cutgraphondegree",
                                      "Prune Graph on Degree")
                       )# End column
                       ,
                       column(4,
                              sliderInput("cutnodeondegree", 
                                          "Degree Value to Prune Nodes ",
                                          min = min(rv$deg$DegreeValue),
                                          max = max(rv$deg$DegreeValue),
                                          value =min(rv$deg$DegreeValue) )
                       )#end column
                     )#end fluidrow
                     ,
                     #row with options to cut a part of the network on Betweennes Values
                     fluidRow(
                       column(
                         3,  
                         actionButton("cutgraphonbtw",
                                      "Prune Graph on Betweenness")
                       )#end column
                       ,
                       column(
                         4, 
                         sliderInput("cutnodeonbtw", 
                                     "Betweenness Value to Prune Nodes ",
                                     min = min(rv$betw$BetweennesValue),
                                     max = max(rv$betw$BetweennesValue),
                                     value =min(rv$betw$BetweennesValue))
                       )#end column
                     )#end fluidrow
                     
            ),
            tabPanel("Degree Distribution",  plotOutput("plotdegreedist")),
            tabPanel("Degree Values",  dataTableOutput("tbldegree")),
            tabPanel("Betweennes Values",  dataTableOutput("tblbetweennes")),
            tabPanel("PageRank Values",  dataTableOutput("tblpagerank"))
            
          )# end tabsetPanel
        )
        #######################################
        
        
        ########################################
        ####### GRAPH 
        progress$set(message = "Plot Graph", value = 0.6)
        
        # Get highest Degree, Betweennes and PAgeRank Nodes
        rv$highestbtwid <- rv$betw[which.max(rv$betw$BetweennesValue),]
        rv$highestbtwid$CentralityMeasure <- "Betweennes"
        colnames(rv$highestbtwid) <- c("NodeID", "Score", "CentralityMeasure")
        
        rv$highestdgid  <- rv$deg[which.max(rv$deg$DegreeValue),]
        rv$highestdgid$CentralityMeasure <- "Degree"
        colnames(rv$highestdgid) <- c("NodeID", "Score", "CentralityMeasure")
        
        rv$highestpgrid <- rv$pagerk[which.max(rv$pagerk$PageRankValue),]
        rv$highestpgrid$CentralityMeasure <- "Pagerank"
        colnames(rv$highestpgrid) <- c("NodeID", "Score", "CentralityMeasure")
        
        #clear
        rv$overviewhighestcentrality <- NULL
        
        #create table of these values  
        rv$overviewhighestcentrality = rbind(
          rv$overviewhighestcentrality,
          rv$highestbtwid, stringsAsFactors=FALSE)
        
        rv$overviewhighestcentrality= rbind(
          rv$overviewhighestcentrality,
          rv$highestdgid, stringsAsFactors=FALSE)
        
        rv$overviewhighestcentrality= rbind(
          rv$overviewhighestcentrality,
          rv$highestpgrid, stringsAsFactors=FALSE)
        
        colnames(rv$overviewhighestcentrality) <-
          c("NodeID", "Score", "CentralityMeasure")
        
        output$tbloverviewcentrality = renderDT(
          rv$overviewhighestcentrality,
          options = list(
            searching = FALSE,
            lengthChange = FALSE,       
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"),
            pageLength = 5
          ),
          caption = "Centrality Measures: Node with Highest Value",
          rownames= FALSE,
          selection = 'none'
        )
        
        ####
        # Plot Graph data
        output$graphplot = renderVisNetwork({
          visIgraph(
            rv$graph, 
            physics=FALSE) %>%
            visOptions(nodesIdSelection = TRUE) %>%
            visPhysics(enabled = FALSE) %>%
            visEdges( smooth = FALSE) %>%
            visNodes(
              color = list(background = "blue", highlight = 'red'),
              label = as.character(V(rv$graph)$ID))
        })
        ####
        
        ####
        # SHOW RANKED DEGREE AND BETWEENNESS TO INDICATE INTERESTING CASES
        
        
        
        #rank Degree
        rv$degreeranked <- rv$deg[
          order( rv$deg$DegreeValue, 
                 decreasing=TRUE),]
        
        colnames(rv$degreeranked) <-
          c("NodeID", "DegreeValue")
        
        rv$degreeranked <- transform(rv$degreeranked, 
                                     rankDegree = ave (DegreeValue, 
                                                       FUN = function(x)
                                                         rank (-x, ties.method ="min")))
        colnames(rv$degreeranked) <-
          c("NodeID", "DegreeValue", "rankDegree")
        
        
        rv$degreeranked$rankDegree <- factor(rv$degreeranked$rankDegree,
                                             labels =seq.int((unique(rv$degreeranked$rankDegree))) )
        
        colnames(rv$degreeranked) <-
          c("NodeID", "DegreeValue", "rankDegree")
        
        
        #rank Betweennes
        rv$betwranked <- rv$betw[
          order( rv$betw$BetweennesValue, 
                 decreasing=TRUE),]
        
        colnames(rv$betwranked) <-
          c("NodeID", "BetweennesValue")
        
        rv$betwranked <- transform(rv$betwranked, 
                                   rankBetweennes = ave (BetweennesValue, 
                                                         FUN = function(x)
                                                           rank (-x, ties.method ="min")))
        colnames(rv$betwranked) <-
          c("NodeID", "BetweennesValue", "rankBetweennes")
        
        rv$betwranked$rankBetweennes <- factor(rv$betwranked$rankBetweennes,
                                               labels =seq.int((unique(rv$betwranked$rankBetweennes))) )
        
        colnames(rv$betwranked) <-
          c("NodeID", "BetweennesValue", "rankBetweennes")
        
        
        
        #Combine
        rv$combineddegreebetw <- merge(rv$degreeranked, rv$betwranked, by.x = "NodeID", by.y = "NodeID")
        
        rv$combineddegreebetw <- rv$combineddegreebetw[
          order( rv$combineddegreebetw$rankDegree,
                 decreasing=FALSE),]
        
        
        #create Table
        output$tbloverviewrankeddegbet = renderDT(
          rv$combineddegreebetw,
          options = list(
            searching = FALSE,
            lengthChange = FALSE,       
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"),
            pageLength = 5
          ),
          caption = "Compare RANKED Degree and Betweenness Scores",
          rownames= FALSE,
          selection = 'none'
        )
        
        #create Ui
        output$uitbloverviewrankeddegbet <- renderUI({
          dataTableOutput("tbloverviewrankeddegbet")
        })
        
        ########################################
        
        
        ########################################
        ####### PATHS
        
        #######
        #Longest Path
        #calc
        rv$longestpath <- NULL
        rv$longestpath <- farthest_vertices( 
          rv$graph,
          directed = FALSE,
          unconnected = TRUE,
          weights = NULL)
        
        # Average Path length
        #calc
        rv$meandistance <- NULL
        rv$meandistance <- mean_distance(
          rv$graph, 
          directed = FALSE, 
          unconnected = TRUE)
        
        #Create UI of distance Stats
        output$uistatdistances <- renderUI({
          str1 <- paste(
            "Longest distance", 
            rv$longestpath$distance )
          str2 <- paste(
            "Longest Distance from NodeID: ", 
            as.character( rv$longestpath$vertices[1]), 
            "to NodeID: ",
            as.character( rv$longestpath$vertices[2])
          )
          str3 <- paste("Average Distance", rv$meandistance)
          
          HTML(paste(str1, str2, str3, sep = '<br/>'))
          #HTML(paste(str1, str2, str3, sep = '<br/>'))
          
        })
        ####
        
        #######
        
        ### All paths
        
        ### Distribution between Algoritmes
        
        ####### 
        output$uipathselect <- renderUI(
          # options to display Path between 2 nodes
          fluidRow(
            column(4,
                   selectInput("pathidfirst",
                               "Choose Node",
                               c(V(rv$graph)))
            )#end column
            ,
            column(4,
                   selectInput("pathidsecond",
                               "Choose another Node:",
                               c(V(rv$graph)))
            )#end column
            ,
            column(4,
                   actionButton("calcpathfromto",
                                "Calculate Shortest Path")   
            )#end column
            
          )# end fluidrow
          
        )
        ####### 
        
        ########################################
        
        
        ########################################
        ####### COMMUNITY
        ###### 
        output$uicommunityselect <- renderUI(
          fluidRow(
            column(8, align="left",
                   actionButton("calccommunityalgo",
                                "Calculate Communities")     
            )
          )
        )
        ####### 
        
        ########################################
        
       
      }
      ######## 
      progress$set(message = "Finished", value = 1)
    })
  #############################################
  
  
  ########################################
  # PRUNE GRAPH ON DEGREE
  observeEvent({
    input$cutgraphondegree
  },
  {

    #find nodes to Prune based on the degree given 
    bad.network<-V(rv$graph)[degree(rv$graph)<=input$cutnodeondegree] 
    
    #Prune the graph
    rv$graph <-delete.vertices(rv$graph, bad.network)

  })
  ########################################
  

  
  ########################################
  # PRUNE GRAPH ON Betweennes
  observeEvent({
    input$cutgraphonbtw
  },
  {
    cat(input$cutnodeonbtw)
    
    #find nodes to Prune based on the degree given 
    bad.network <- V(rv$graph)[betweenness(rv$graph)<=input$cutnodeonbtw] 
    
    #Prune the graph
    rv$graph <- delete.vertices(rv$graph, bad.network)
  })
  ########################################
  
  
  ########################################
  # Dynamically plot Graph based on the centrality measure selected
  observeEvent(
    input$selectcentralitymeasureforgraph,                                     ##        eventExpr
    {
      if(input$selectcentralitymeasureforgraph == "none"){
        cat("1: None")
        
        V(rv$graph)$size <- 25
        
        output$graphplot = renderVisNetwork({
          visIgraph(rv$graph, idToLabel = TRUE, physics=FALSE) %>%
            visOptions(nodesIdSelection = TRUE) %>%
            visPhysics(stabilization = FALSE) %>%
            visEdges(smooth = FALSE) %>%
            visIgraphLayout(layout = 'layout.fruchterman.reingold') %>%
            visNodes(color = list(background = "blue", highlight = 'red'))
        })
        
      }
      else if(input$selectcentralitymeasureforgraph == "degree")
        {
        cat("2: degree")
        
        # 12+ deg$DegreeValue 
        V(rv$graph)$size <- 12+  rv$deg$DegreeValue # Node size
      
        output$graphplot = renderVisNetwork({
          visIgraph(rv$graph, idToLabel = TRUE, physics=FALSE) %>%
            visOptions(nodesIdSelection = TRUE) %>%
            visPhysics(stabilization = FALSE) %>%
            visEdges(smooth = FALSE) %>%
            visIgraphLayout(layout = 'layout.fruchterman.reingold') %>%
            visNodes(color = list(background = "blue", highlight = 'red'))
        })
        
      }
      else if(input$selectcentralitymeasureforgraph == "betweennes")
      {
        cat("3: betweennes")
        # 12 +(betw$between/vcount(dataset))
        V(rv$graph)$size <- 12  +  ( (rv$betw$BetweennesValue*2.0) / rv$nrnodes )
        
        output$graphplot = renderVisNetwork({
          visIgraph(rv$graph, idToLabel = TRUE, physics=FALSE) %>%
            visOptions(nodesIdSelection = TRUE) %>%
            visPhysics(stabilization = FALSE) %>%
            visEdges(smooth = FALSE) %>%
            visIgraphLayout(layout = 'layout.fruchterman.reingold') %>%
            visNodes(color = list(background = "blue", highlight = 'red'))
        })
        
      }
      else if(input$selectcentralitymeasureforgraph == "pagerank"){
        cat("4: pagerank")
        # 12 + pagerk$PageRankValue*100
        
        V(rv$graph)$size <- 12 + (rv$pagerk$PageRankValue*100) 
        
        
        output$graphplot = renderVisNetwork({
          visIgraph(rv$graph, idToLabel = TRUE, physics=FALSE) %>%
            visOptions(nodesIdSelection = TRUE) %>%
            visPhysics(stabilization = FALSE) %>%
            visEdges(smooth = FALSE) %>%
            visIgraphLayout(layout = 'layout.fruchterman.reingold') %>%
            visNodes(color = list(background = "blue", highlight = 'red'))
        })
        
      }
      
     }, 
    ## handlerExpr
    ignoreInit = TRUE
  )
  ########################################

  
  
  #############################################
  #Observe event 
  observeEvent({
    input$calcpathfromto
  },
  {

    #check if both nodes are different
    if(input$pathidfirst != input$pathidsecond)
    {
      
      # Find the shortest path between specific nodes.
      pathfromto <- shortest_paths(
        rv$graph,
        from =  as.numeric(input$pathidfirst),
        to  =  as.numeric(input$pathidsecond),
        output = "both") # both path nodes and edges
      
      # Generate edge color variable to plot the paths
      ecol <- rep("gray80", ecount(rv$graph))
      ecol[unlist(pathfromto$epath)] <- "orange"
      
      # Generate edge width variable to plot the path:
      ew <- rep(1, ecount(rv$graph))
      ew[unlist(pathfromto$epath)] <- 10
      
      # Generate node color variable to plot the path:
      vcol <- rep("gray40", vcount(rv$graph))
      vcol[unlist(pathfromto$vpath)] <- "gold"
      
      #plot
      output$plotpath <- renderPlot(
        plot.igraph(
          rv$graph,
          vertex.color=vcol, 
          edge.color=ecol, 
          edge.width=ew, 
          edge.arrow.mode=0,
          layout= layout.fruchterman.reingold)
      )
   
      #Generate UI output
      output$uigraphshortestpath <- renderUI({
        plotOutput("plotpath",width = "100%", height = "600px")
        
      } )

    }# end if
    else{
      showModal(modalDialog(
        title = "Wrong input!",
        "Select 2 different nodes.",
        easyClose = TRUE,
        footer = NULL
      ))
    }

    #########

    
  })# End observer Button From To PATH
  ########################################
  
  #############################################
  #Observe event 
  observeEvent({
    input$calccommunityalgo
  },
  {
    
    #empty content of dataframe
    rv$overviewcommresults <- rv$overviewcommresults[0,]
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Loading Data", value = 0)
    
    #calc clustering for Map Equation with Infomap as search algoritm
    rv$mpe <- cluster_infomap(
      rv$graph, 
      e.weights = NULL, 
      v.weights = NULL, 
      nb.trials = 10,
      modularity = TRUE)
    
    #calc modularity of the community of Map Equation
    rv$mpemodularity <- modularity(
      rv$graph, 
      membership(rv$mpe))
    
    mpe.row <- list("Map Equation", rv$mpemodularity, length(rv$mpe), as.character(is_hierarchical(rv$mpe)))
    
    #calc clustering for Louvian
    rv$lvn <- cluster_louvain(
      as.undirected(rv$graph) , 
      weights = NULL)
    
    rv$lvnmodularity <- modularity(
      as.undirected(rv$graph) , 
      membership(rv$lvn))
    
    lvn.row <- list("Louvian", rv$lvnmodularity, length(rv$lvn), as.character(is_hierarchical(rv$lvn)))
    
    #calc clustering for Waltrap
    rv$wlp <- cluster_walktrap(
      rv$graph, 
      steps=200, 
      modularity=TRUE)
    
    rv$wlpmodularity <- modularity(
      rv$graph, 
      membership(rv$wlp))
    
    wlp.row <-list("Walktrap", rv$wlpmodularity, length(rv$wlp), as.character(is_hierarchical(rv$wlp)))
    
    
    #calc clustering for Edge Betweenness
    rv$ebn <- cluster_edge_betweenness(
      rv$graph, 
      directed=F)
    
    rv$ebnmodularity <- modularity(
      rv$graph, 
      membership(rv$ebn))
    
    ebn.row <-list("Edge Betweenness", rv$ebnmodularity, length(rv$ebn), as.character(is_hierarchical(rv$ebn)))
    
    
    #calc clustering for Fast Greedy
    rv$fgy <- cluster_fast_greedy(  as.undirected(rv$graph) )
    
    rv$fgymodularity  <- modularity(
      as.undirected(rv$graph) , 
      membership(rv$fgy))
    
    fgy.row <-list("Fast Greedy", rv$fgymodularity, length(rv$fgy), as.character(is_hierarchical(rv$fgy)))
    
    
    #calc clustering for Label Propogation
    rv$lpn <- cluster_label_prop(rv$graph)

    rv$lpnmodularity <- modularity(
      rv$graph, 
      membership(rv$lpn))
    
    lpn.row <-list("Label Propogation", rv$lpnmodularity, length(rv$lpn), as.character(is_hierarchical(rv$lpn)))
    
    #calc clustering for Leading eigenvector
    rv$lev <- cluster_leading_eigen( rv$graph)
    
    rv$levmodularity <- modularity(
      rv$graph, 
      membership(rv$lev))
    
    lev.row <-list("Leading Eigenvector", rv$levmodularity, length(rv$lev), as.character(is_hierarchical(rv$lev)))
    
    #calc clustering for Springclass
    #rv$sgs <- cluster_spinglass(rv$graph)
    #rv$sgsmodularity <- modularity(
    #  rv$graph, 
    #  membership(rv$sgs))
    
    #sgs.row <-list("Springclass", rv$sgsmodularity, length(rv$sgs), as.character(is_hierarchical(rv$sgs)))
    
    
    #######################################
    ### combine results to table
    
    ########################################
    progress$set(message = "Generate Table", value = 0.3)
    
    rv$overviewcommresults = rbind(rv$overviewcommresults,mpe.row, stringsAsFactors=FALSE)
    rv$overviewcommresults = rbind(rv$overviewcommresults,lvn.row, stringsAsFactors=FALSE)
    rv$overviewcommresults = rbind(rv$overviewcommresults,wlp.row, stringsAsFactors=FALSE)
    rv$overviewcommresults = rbind(rv$overviewcommresults,ebn.row, stringsAsFactors=FALSE)
    rv$overviewcommresults = rbind(rv$overviewcommresults,fgy.row, stringsAsFactors=FALSE)
    rv$overviewcommresults = rbind(rv$overviewcommresults,lpn.row, stringsAsFactors=FALSE)
    rv$overviewcommresults = rbind(rv$overviewcommresults,lev.row, stringsAsFactors=FALSE)
    #rv$overviewcommresults = rbind(rv$overviewcommresults,sgs.row, stringsAsFactors=FALSE)
   
    colnames(rv$overviewcommresults) <- c("Algoritme", "ModularityScore", "NrOfCommunities", "Hierachical")
    
    #reorder
    rv$overviewcommresults <- rv$overviewcommresults[
      order(rv$overviewcommresults$ModularityScore, 
        decreasing=TRUE),]
    
    #Table 
    output$tblmodularityscores = renderDT(
      rv$overviewcommresults,
      options = list(
        searching = FALSE,
        lengthChange = FALSE,       
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"),
        pageLength = 8
      ),
      caption = "Comparison of Community Detection Algoritmes",
      rownames= FALSE,
      selection = 'single'
    )
    
    #cat(rv$overviewcommresults)
    
    #uiOutput  Modularity Scores
    output$uitlbmodularityscores <- renderUI({
        dataTableOutput("tblmodularityscores")
    })
    #######################################
    
    progress$set(message = "Done", value = 1)
    
    
  })
  #############################################
  
  
  #############################################
  #Observe event 
  observeEvent({
    !is.null(input$tblmodularityscores_rows_selected)
  },
  {
    selectedmethod <- NULL 
    
    #get method from selected row
    selectedmethod <- rv$overviewcommresults[
      input$tblmodularityscores_rows_selected,c("Algoritme")] 

    
    #create plot with selected community detection algoritme
    ##################Springclass ###########################
    if(
      as.character(selectedmethod)=="Springclass" &&
      length(selectedmethod)>0 && 
      !is.null(selectedmethod)
      ){
      #Create plot
      output$plotcommunitygraph <- renderPlot({
        V(rv$graph)$color=membership(rv$sgs)
        plot.igraph(
          rv$graph,
          vertex.size=8,
          edge.arrow.mode=0,
          layout =  layout.fruchterman.reingold)
      })

      #Generate UI output
      output$uigraphcommunity <- renderUI({
        plotOutput("plotcommunitygraph",width = "100%", height = "600px")
      } )
      
      # Create Table with Members of the communities
      rv$membershipofcomtbl <- cbind(V(rv$graph),rv$sgs$membership) 
      colnames( rv$membershipofcomtbl) <- c("Node", "MemberOfCommunity")
      #Table 
      output$tblmembershipcom = renderDT(
        rv$membershipofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 10
        ),
        caption = "Node Community Membership",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Membership of Communities
      output$uitlbmembershipofcom <- renderUI({
        dataTableOutput("tblmembershipcom")
      })
      #######################################
      
      # Create Table with the Sizes of the communities
      rv$sizeofcomtbl <- as.data.frame(sizes(rv$sgs), stringsAsFactors=FALSE)
      colnames( rv$sizeofcomtbl) <- c("Communities", "NrofMembers")
      
      #Table 
      output$tblsizecom = renderDT(
        rv$sizeofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 5
        ),
        caption = "Size of Communities",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Size of Communities
      output$uitlbsizeofcom <- renderUI({
        dataTableOutput("tblsizecom")
      })
      #######################################
      
      #######################################
      # dendrogram ( NOT POSSIBLE BECAUSE NOT HIERARCHICAL)
      
      #create ui of dendrogram ( DUMMY MESSAGE)
      output$uiplotdenrogram <- renderUI({
        str1 <- paste("Algoritme is not hierarchical" )
        HTML(paste(str1, sep = '<br/>'))
        
      })
      #######################################
      
    }
    ##########################################################
    
    ##################Louvian ###########################
    else if(
      selectedmethod=="Louvian" &&
      length(selectedmethod)>0 && 
      !is.null(selectedmethod)
      ){
      #Create plot
      output$plotcommunitygraph <- renderPlot({
        V(rv$graph)$color=membership(rv$lvn)
        plot.igraph(
          rv$graph,
          vertex.size=8,
          edge.arrow.mode=0,
          layout =  layout.fruchterman.reingold)
      })
      
      #Generate UI output
      output$uigraphcommunity <- renderUI({
        plotOutput("plotcommunitygraph",width = "100%", height = "600px")
      } )
      
      # Create Table with Members of the communities
      rv$membershipofcomtbl <- cbind(V(rv$graph),rv$lvn$membership) 
      colnames( rv$membershipofcomtbl) <- c("Node", "MemberOfCommunity")
      #Table 
      output$tblmembershipcom = renderDT(
        rv$membershipofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 10
        ),
        caption = "Node Community Membership",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Membership of Communities
      output$uitlbmembershipofcom <- renderUI({
        dataTableOutput("tblmembershipcom")
      })
      #######################################
      
      # Create Table with the Sizes of the communities
      rv$sizeofcomtbl <- as.data.frame(sizes(rv$lvn), stringsAsFactors=FALSE)
      colnames( rv$sizeofcomtbl) <- c("Communities", "NrofMembers")
      
      #Table 
      output$tblsizecom = renderDT(
        rv$sizeofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 5
        ),
        caption = "Size of Communities",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Size of Communities
      output$uitlbsizeofcom <- renderUI({
        dataTableOutput("tblsizecom")
      })
      #######################################
      
      #######################################
      # dendrogram ( NOT POSSIBLE BECAUSE NOT HIERARCHICAL)
      
      #create ui of dendrogram ( DUMMY MESSAGE)
      output$uiplotdenrogram <- renderUI({
        str1 <- paste("Algoritme is not hierarchical" )
        HTML(paste(str1, sep = '<br/>'))
        
      })
      #######################################
      
    }
    #####################################################
    
    ################## Map Equation ###########################
    else if(
      selectedmethod=="Map Equation" &&
      length(selectedmethod)>0 && 
      !is.null(selectedmethod)
      ){
      
      #Create plot
      output$plotcommunitygraph <- renderPlot({
        V(rv$graph)$color=membership(rv$mpe)
        plot.igraph(
          rv$graph,
          vertex.size=8,
          edge.arrow.mode=0,
          layout =  layout.fruchterman.reingold)
      })
      
      #Generate UI output
      output$uigraphcommunity <- renderUI({
        plotOutput("plotcommunitygraph",width = "100%", height = "600px")
      } )
      
      # Create Table with Members of the communities
      rv$membershipofcomtbl <- cbind(V(rv$graph),rv$mpe$membership) 
      colnames( rv$membershipofcomtbl) <- c("Node", "MemberOfCommunity")
      #Table 
      output$tblmembershipcom = renderDT(
        rv$membershipofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 10
        ),
        caption = "Node Community Membership",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Membership of Communities
      output$uitlbmembershipofcom <- renderUI({
        dataTableOutput("tblmembershipcom")
      })
      #######################################
      
      # Create Table with the Sizes of the communities
      rv$sizeofcomtbl <- as.data.frame(sizes(rv$mpe), stringsAsFactors=FALSE)
      colnames( rv$sizeofcomtbl) <- c("Communities", "NrofMembers")
      
      #Table 
      output$tblsizecom = renderDT(
        rv$sizeofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 5
        ),
        caption = "Size of Communities",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Size of Communities
      output$uitlbsizeofcom <- renderUI({
        dataTableOutput("tblsizecom")
      })
      #######################################
      
      #######################################
      # dendrogram ( NOT POSSIBLE BECAUSE NOT HIERARCHICAL)
      
      #create ui of dendrogram ( DUMMY MESSAGE)
      output$uiplotdenrogram <- renderUI({
        str1 <- paste("Algoritme is not hierarchical" )
        HTML(paste(str1, sep = '<br/>'))
        
      })
      #######################################
      
    }
    ###########################################################
    
    ################## Edge Betweenness #######################
    else if(
      selectedmethod=="Edge Betweenness" &&
      length(selectedmethod)>0 && 
      !is.null(selectedmethod)
      ){
      
      #Create plot
      output$plotcommunitygraph <- renderPlot({
        V(rv$graph)$color=membership(rv$ebn)
        plot.igraph(
          rv$graph,
          vertex.size=8,
          edge.arrow.mode=0,
          layout =  layout.fruchterman.reingold)
      })
      
      #Generate UI output
      output$uigraphcommunity <- renderUI({
        plotOutput("plotcommunitygraph",width = "100%", height = "600px")
      } )
      
      # Create Table with Members of the communities
      rv$membershipofcomtbl <- cbind(V(rv$graph),rv$ebn$membership) 
      colnames( rv$membershipofcomtbl) <- c("Node", "MemberOfCommunity")
      #Table 
      output$tblmembershipcom = renderDT(
        rv$membershipofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 10
        ),
        caption = "Node Community Membership",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Membership of Communities
      output$uitlbmembershipofcom <- renderUI({
        dataTableOutput("tblmembershipcom")
      })
      #######################################
      
      # Create Table with the Sizes of the communities
      rv$sizeofcomtbl <- as.data.frame(sizes(rv$ebn), stringsAsFactors=FALSE)
      #rv$sizeofcomtbl <- cbind(names = rownames( rv$sizeofcomtbl),  rv$sizeofcomtbl)
      colnames( rv$sizeofcomtbl) <- c("Communities", "NrofMembers")
      #Table 
      output$tblsizecom = renderDT(
        rv$sizeofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 5
        ),
        caption = "Size of Communities",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Size of Communities
      output$uitlbsizeofcom <- renderUI({
        dataTableOutput("tblsizecom")
      })
      #######################################
      
      #######################################
      # dendrogram
     
      #create plot
      output$plotdenrogram  <- renderPlot({
        # using dendrogram objects
        hebn = as.dendrogram(rv$ebn)
        plot(hebn)
      })
      
      #create ui of dendrogram
      output$uiplotdenrogram <- renderUI({
        plotOutput("plotdenrogram")
      })
      #######################################
    }
    ##############################################################
    
    
    ################## Leading Eigenvector #######################
    else if(
      selectedmethod=="Leading Eigenvector" &&
      length(selectedmethod)>0 && 
      !is.null(selectedmethod)
      ){
      
      #Create plot
      output$plotcommunitygraph <- renderPlot({
        V(rv$graph)$color=membership(rv$lev)
        plot.igraph(
          rv$graph,
          vertex.size=8,
          edge.arrow.mode=0,
          layout =  layout.fruchterman.reingold)
      })
      
      #Generate UI output
      output$uigraphcommunity <- renderUI({
        plotOutput("plotcommunitygraph",width = "100%", height = "600px")
      } )
      
      # Create Table with Members of the communities
      rv$membershipofcomtbl <- cbind(V(rv$graph),rv$lev$membership) 
      colnames( rv$membershipofcomtbl) <- c("Node", "MemberOfCommunity")
      #Table 
      output$tblmembershipcom = renderDT(
        rv$membershipofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 10
        ),
        caption = "Node Community Membership",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Membership of Communities
      output$uitlbmembershipofcom <- renderUI({
        dataTableOutput("tblmembershipcom")
      })
      #######################################
      
      # Create Table with the Sizes of the communities
      rv$sizeofcomtbl <- as.data.frame(sizes(rv$lev), stringsAsFactors=FALSE)
      colnames( rv$sizeofcomtbl) <- c("Communities", "NrofMembers")
      
      #Table 
      output$tblsizecom = renderDT(
        rv$sizeofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 5
        ),
        caption = "Size of Communities",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Size of Communities
      output$uitlbsizeofcom <- renderUI({
        dataTableOutput("tblsizecom")
      })
      #######################################
      
      #######################################
      # dendrogram
      
      #create plot
      output$plotdenrogram  <- renderPlot({
        # using dendrogram objects
        hlev = as.dendrogram(rv$lev)
        plot(hlev)
      })
      
      #create ui of dendrogram
      output$uiplotdenrogram <- renderUI({
        plotOutput("plotdenrogram")
      })
      #######################################
      
    }     
    ######################################################

    
    ################## Fast Greedy #######################
    else if(
      selectedmethod=="Fast Greedy" &&
      length(selectedmethod)>0 && 
      !is.null(selectedmethod)
      ){
      
      #Create plot
      output$plotcommunitygraph <- renderPlot({
        V(rv$graph)$color=membership(rv$fgy)
        plot.igraph(
          rv$graph,
          vertex.size=8,
          edge.arrow.mode=0,
          layout =  layout.fruchterman.reingold)
      })
      
      #Generate UI output
      output$uigraphcommunity <- renderUI({
        plotOutput("plotcommunitygraph",width = "100%", height = "600px")
      } )
      
      # Create Table with Members of the communities
      rv$membershipofcomtbl <- cbind(V(rv$graph),rv$fgy$membership) 
      colnames( rv$membershipofcomtbl) <- c("Node", "MemberOfCommunity")
      #Table 
      output$tblmembershipcom = renderDT(
        rv$membershipofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 10
        ),
        caption = "Node Community Membership",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Membership of Communities
      output$uitlbmembershipofcom <- renderUI({
        dataTableOutput("tblmembershipcom")
      })
      #######################################
      
      # Create Table with the Sizes of the communities
      rv$sizeofcomtbl <- as.data.frame(sizes(rv$fgy), stringsAsFactors=FALSE)
      colnames( rv$sizeofcomtbl) <- c("Communities", "NrofMembers")
      
      #Table 
      output$tblsizecom = renderDT(
        rv$sizeofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 5
        ),
        caption = "Size of Communities",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Size of Communities
      output$uitlbsizeofcom <- renderUI({
        dataTableOutput("tblsizecom")
      })
      #######################################
      
      #######################################
      # dendrogram
      
      #create plot
      output$plotdenrogram  <- renderPlot({
        # using dendrogram objects
        hfgy = as.dendrogram(rv$fgy)
        plot(hfgy)
      })
      
      #create ui of dendrogram
      output$uiplotdenrogram <- renderUI({
        plotOutput("plotdenrogram")
      })
      #######################################
      
    }
    #####################################################
    
    
    ################## Walktrap #######################
    else if(
      selectedmethod=="Walktrap" &&
      length(selectedmethod)>0 && 
      !is.null(selectedmethod)
      ){
      
      #Create plot
      output$plotcommunitygraph <- renderPlot({
        V(rv$graph)$color=membership(rv$wlp)
        plot.igraph(
          rv$graph,
          vertex.size=8,
          edge.arrow.mode=0,
          layout =  layout.fruchterman.reingold)
      })
      
      #Generate UI output
      output$uigraphcommunity <- renderUI({
        plotOutput("plotcommunitygraph",width = "100%", height = "600px")
      } )
      
      # Create Table with Members of the communities
      rv$membershipofcomtbl <- cbind(V(rv$graph),rv$wlp$membership) 
      colnames( rv$membershipofcomtbl) <- c("Node", "MemberOfCommunity")
      
      #Table 
      output$tblmembershipcom = renderDT(
        rv$membershipofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 10
        ),
        caption = "Node Community Membership",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Membership of Communities
      output$uitlbmembershipofcom <- renderUI({
        dataTableOutput("tblmembershipcom")
      })
      #######################################
      
      # Create Table with the Sizes of the communities
      rv$sizeofcomtbl <- as.data.frame(sizes(rv$wlp), stringsAsFactors=FALSE)
      colnames( rv$sizeofcomtbl) <- c("Communities", "NrofMembers")
      
      #Table 
      output$tblsizecom = renderDT(
        rv$sizeofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 5
        ),
        caption = "Size of Communities",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Size of Communities
      output$uitlbsizeofcom <- renderUI({
        dataTableOutput("tblsizecom")
      })
      #######################################
      
      #######################################
      # dendrogram
      
      #create plot
      output$plotdenrogram  <- renderPlot({
        # using dendrogram objects
        hwlp = as.dendrogram(rv$wlp)
        plot(hwlp)
      })
      
      #create ui of dendrogram
      output$uiplotdenrogram <- renderUI({
        plotOutput("plotdenrogram")
      })
      #######################################
      
      
    }
    ####################################################
    
    ################## Label Propogation ###############
    else if(
      selectedmethod=="Label Propogation" &&
      length(selectedmethod)>0 && 
      !is.null(selectedmethod)
      ){
      
      #Create plot
      output$plotcommunitygraph <- renderPlot({
        V(rv$graph)$color=membership(rv$lpn)
        plot.igraph(
          rv$graph,
          vertex.size=8,
          edge.arrow.mode=0,
          layout =  layout.fruchterman.reingold)
      })
      
      #Generate UI output
      output$uigraphcommunity <- renderUI({
        plotOutput("plotcommunitygraph",width = "100%", height = "600px")
      } )
      
      # Create Table with Members of the communities
      rv$membershipofcomtbl <- cbind(V(rv$graph),rv$lpn$membership) 
      colnames( rv$membershipofcomtbl) <- c("Node", "MemberOfCommunity")
      
      #Table 
      output$tblmembershipcom = renderDT(
        rv$membershipofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 10
        ),
        caption = "Node Community Membership",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Membership of Communities
      output$uitlbmembershipofcom <- renderUI({
        dataTableOutput("tblmembershipcom")
      })
      #######################################
      
      # Create Table with the Sizes of the communities
      rv$sizeofcomtbl <- as.data.frame(sizes(rv$lpn), stringsAsFactors=FALSE)
      colnames( rv$sizeofcomtbl) <- c("Communities", "NrofMembers")
      
      #Table 
      output$tblsizecom = renderDT(
        rv$sizeofcomtbl,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,       
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 5
        ),
        caption = "Size of Communities",
        rownames= FALSE,
        selection = 'none'
      )
      
      #uiOutput  Size of Communities
      output$uitlbsizeofcom <- renderUI({
        dataTableOutput("tblsizecom")
      })
      #######################################

      #######################################
      # dendrogram ( NOT POSSIBLE BECAUSE NOT HIERARCHICAL)
      
      #create ui of dendrogram ( DUMMY MESSAGE)
      output$uiplotdenrogram <- renderUI({
        str1 <- paste("Algoritme is not hierarchical" )
        HTML(paste(str1, sep = '<br/>'))
        
      })
      #######################################
    
    }
    #########
  
  })
  #############################################
  
  
} # end server function
#######################################

# Run the application 
shinyApp(ui = ui, server = server)

