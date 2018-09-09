#### Load necessary packages and data ####
library(shiny)
library(igraph)
library(zoo)
library(dplyr)
library(gtools)
library(plyr)
library(rcytoscapejs)
library(networkD3)
library(pracma)
library(devtools)
library("RColorBrewer")
require(pracma)
library(network)
library(reshape2)
library(plotly)
require(visNetwork, quietly = TRUE)
#### LOAD ####


load("lassodataMYCsign2017-11-08.rdata")
load("lscodataMYCsign2017-11-08.rdata")

# ### LOAD PLOT DATA ###
load("lassoMYCsignlinkplotdata2017-02-08.rdata")
load("lscoMYCsignlinkplotdata2017-02-14.rdata")


ddd<-list(lassodataMYCsign=lassodataMYCsign,lscodataMYCsign=lscodataMYCsign)
eee<-list(lassoMYCsignlinkplotdata=lassoMYCsignlinkplotdata,lscoMYCsignlinkplotdata=lscoMYCsignlinkplotdata)
fff<-list(lassonameMYCsign=lassonameMYCsign,lsconameMYCsign=lsconameMYCsign)
ggg<-list(lassoMYCsignlinkcutdata=lassoMYCsignlinkcutdata,lscoMYCsignlinkcutdata=lscoMYCsignlinkcutdata)


#### Server ####
server <- function(input, output) {
  output$contents <- renderTable({
    if(input$demo==TRUE){
      wee<-paste("lassodataMYCsign")
      datum<-ddd[[wee]]
    }else{
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
    }
    
    edgeList<-datum[[input$sparsity]][1:6]
    colnames(edgeList) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
    if(input$self==FALSE){
      edgeList<-edgeList%>%filter_(~SourceName!=as.character(TargetName))####REMOVE SELF LINKS -- works on test network file
    }
    edgeList
  })
  
  
  
  
  output$forcelasso <- renderForceNetwork({
    if(input$demo==TRUE){
      wee<-paste("lassodataMYCsign")#paste("lassodataMYCsign")
      datum<-ddd[[wee]]}else{
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
        # datum<-wee
      }
    
    edgeList<-datum[[input$sparsity]][1:6]
    colnames(edgeList) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
    if(input$self==FALSE){
      edgeList<-edgeList%>%filter_(~SourceName!=as.character(TargetName))####REMOVE SELF LINKS -- works on test network file
    }
    
    gD<-simplify(graph.data.frame(edgeList,directed=TRUE))
    nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)),nName = igraph::V(gD)$name)
    getNodeID <- function(x){which(x == igraph::V(gD)$name) - 1}
    
    edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName", "Weight"), 
                            function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                    TargetID = getNodeID(x$TargetName)))
    nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))
    betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
    betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
    nodeList <- cbind(nodeList, nodeBetweenness=100*betAll.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
    rm(betAll, betAll.norm)
    
    dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")
    
    F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
    edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", "Weight", "SourceID", "TargetID"), 
                            function(x) data.frame(F1(x)))
    
    rm(dsAll, F1, getNodeID, gD)
    
    F2 <- colorRampPalette(c("#0000FF", "#FF0000"), bias = nrow(edgeList), space = "rgb", interpolate = "linear")
    colCodes <- F2(length(unique(edgeList$Weight)))
    edges_col <- sapply(edgeList$Weight, function(x) colCodes[which(sort(unique(edgeList$Weight)) == x)])
    
    rm(colCodes, F2)
    ############################################################################################
    # Let's create a network
    # with a simple click action - make the circles bigger when clicked
    MyClickScript <- 
      '      d3.select(this).select("circle").transition()
    .duration(750)
    .attr("r", 30)'
    D3_network_LM <- networkD3::forceNetwork(Links = edgeList, # data frame that contains info about edges
                                             Nodes = nodeList, # data frame that contains info about nodes
                                             Source = "SourceID", # ID of source node 
                                             Target = "TargetID", # ID of target node
                                             Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                             NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                             Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
                                             Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
                                             height = 500, # Size of the plot (vertical)
                                             width = 1000,  # Size of the plot (horizontal)
                                             fontSize = 15, # Font size
                                             linkDistance = networkD3::JS("function(d) { return 100*d.value; }"), 
                                             linkWidth = networkD3::JS("function(d) { return 2*d.value; }"),
                                             opacity = 0.85, # opacity
                                             zoom = TRUE, # ability to zoom when click on the node
                                             opacityNoHover = 0.1, # opacity of labels when static
                                             legend=FALSE,arrows=TRUE,
                                             linkColour = edges_col, bounded=FALSE #clickAction = MyClickScript
    ) # edge colors
    D3_network_LM
    
  })
  output$vizNet <- renderVisNetwork({
    # nodes <- data.frame(id = 1:3)
    # edges <- data.frame(from = c(1,2), to = c(1,3))
    
    wee<-paste("lassodataMYCsign")
    datum<-ddd[[wee]]
    
    edges<-datum[[input$sparsity]][1:3]
    colnames(edges) <- c("from", "to","width")#,"Link2","Sign", "Weight")
    
    gD<-simplify(graph.data.frame(edges,directed=TRUE))
    nodes <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)),nName = igraph::V(gD)$name)
    getNodeID <- function(x){which(x == igraph::V(gD)$name) - 1}
    
    edges <- plyr::ddply(edges, .variables = c("from", "to", "width"), 
                         function (x) data.frame(SourceID = getNodeID(x$from), 
                                                 TargetID = getNodeID(x$to)))
    nodes <- cbind(nodes, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))
    betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
    betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
    nodes <- cbind(nodes, nodeBetweenness=100*betAll.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
    rm(betAll, betAll.norm)
    colnames(nodes) <- c("id", "label","group","font.size")#,"Sign", "Weight")
    dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")
    
    F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
    edges <- plyr::ddply(edges, .variables=c("from", "to", "width", "SourceID", "TargetID"), 
                         function(x) data.frame(F1(x)))
    
    rm(dsAll, F1, getNodeID, gD)
    
    F2 <- colorRampPalette(c("#0000FF", "#FF0000"), bias = nrow(nodes), space = "rgb", interpolate = "linear")
    colCodes <- F2(length(unique(edges$diceSim)))
    edges_col <- sapply(edges$Weight, function(x) colCodes[which(sort(unique(edges$Weight)) == x)])
    colnames(edges) <- c("label", "too","width","from","to", "Weight")
    rm(colCodes, F2)
    
    visNetwork(nodes, edges,width="100%") %>% visLegend() %>%
      # viNodes(label=NULL)
      visEdges(shadow = FALSE,
               arrows ="from",#edges$from, list(to = list(enabled = TRUE, scaleFactor = 2)),
               color = list(color = "lightblue", highlight = "red")) %>%
      visLayout(randomSeed = 12) %>% # to have always the same network
      visOptions(manipulation = TRUE)%>%
      visClusteringByColor(colors=c("red") ) %>%
      visClusteringByGroup(groups )
  })
  output$text1 <- renderPrint({ 
    if(input$demo==TRUE){
      qee<-paste("lassonameMYCsign")#paste("lassodataMYCsign")
      aa<-fff[[qee]][[input$sparsity]]}else{
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        # aa <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
        inFile$name[[input$sparsity]]
      }
    
  })
  output$text2 <- renderPrint({ 
    if(input$demo==TRUE){
      wee<-paste("lassodataMYCsign")#paste("lassodataMYCsign")
      datum<-ddd[[wee]]}else{
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
        # datum<-wee
      }
    dd<-data.frame(datum[input$sparsity])
    net1<-network(dd[1:2],directed=TRUE,loops=TRUE)
    
    c(cat("size:",network.size(net1),"\n"),cat("links:",network.edgecount(net1),"\n"),cat("density:",network.density(net1),"\n"))
  })
  
  output$CytoscapeJS <-renderRcytoscapejs({
    if(input$demo==TRUE){
      wee<-paste("lassodataMYCsign")#paste("lassodataMYCsign")
      datum<-ddd[[wee]]}else{
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
        # datum<-wee
      }
    edgeData<-datum[[input$sparsity]][1:6]
    
    
    colnames(edgeData) <- c("sourceName", "targetName","color","link2","link", "weight")
    if(input$self==FALSE){
      edgeData<-edgeData%>%filter_(~sourceName!=as.character(targetName))####REMOVE SELF LINKS -- works on test network file
    }
    
    
    edgeData$edgeTargetShape<-edgeData$color
    edgeData$edgeTargetShape<-gsub(-1,"tee",edgeData$edgeTargetShape)
    edgeData$edgeTargetShape<-gsub(1,"triangle",edgeData$edgeTargetShape)
    # edgeData$edgeSourceShape<-edgeData$edgeTargetShape
    
    edgeData$color<-gsub(-1,"#FF0000",edgeData$color)
    edgeData$color<-gsub(1,"#0000FF",edgeData$color)
    
    
    gD<-simplify(graph.data.frame(edgeData,directed=TRUE))
    nodeData <- data.frame(id = c(0:(igraph::vcount(gD) - 1)),name = igraph::V(gD)$name)
    getNodeID <- function(x){which(x == igraph::V(gD)$name) - 1}
    
    edgeData <- plyr::ddply(edgeData, .variables = c("sourceName", "targetName", "weight","color","edgeTargetShape"),
                            function (x) data.frame(source = getNodeID(x$source),
                                                    target = getNodeID(x$target)))
    edgeData$targetShape<-edgeData$edgeTargetShape
    edgeData$targetShape<-edgeData$edgeTargetShape
    
    nodeData$shape <- "ellipse"
    # nodeData$selector<-"green"
    nodeData <- cbind(nodeData, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))
    
    network <- createCytoscapeJsNetwork(nodeData, edgeData)
    rcytoscapejs(network$nodes, network$edges, layout=input$clay)
    
  })
  plotInput <-function(){
    wee<-paste("lassodataMYCsign")
    datum<-ddd[[wee]]
    edgeData<-datum[[input$sparsity]][1:6]
    
    
    colnames(edgeData) <- c("sourceName", "targetName","color","link2","link", "weight")
    df<-edgeData[1:2]
    edgeData$D<-apply(df,1,function(x)
      ifelse(length(unique(x[!is.na(x)]))==1,x[!is.na(x)][NaN],1))
    edgeData<-na.omit(edgeData)
    edgeData$D<-NULL
    edgeData$edgeTargetShape<-NULL
    
    edgeData$edgeTargetShape<-edgeData$color
    edgeData$edgeTargetShape<-gsub(-1,"tee",edgeData$edgeTargetShape)
    edgeData$edgeTargetShape<-gsub(1,"triangle",edgeData$edgeTargetShape)
    edgeData$edgeSourceShape<-edgeData$edgeTargetShape
    
    edgeData$color<-gsub(-1,"#FF0000",edgeData$color)
    edgeData$color<-gsub(1,"#0000FF",edgeData$color)
    
    
    gD<-simplify(graph.data.frame(edgeData,directed=TRUE))
    nodeData <- data.frame(id = c(0:(igraph::vcount(gD) - 1)),name = igraph::V(gD)$name)
    getNodeID <- function(x){which(x == igraph::V(gD)$name) - 1}
    
    edgeData <- plyr::ddply(edgeData, .variables = c("sourceName", "targetName", "weight","color","edgeTargetShape"),
                            function (x) data.frame(source = getNodeID(x$source),
                                                    target = getNodeID(x$target)))
    edgeData$targetShape<-edgeData$edgeTargetShape
    edgeData$targetShape<-edgeData$edgeTargetShape
    
    nodeData$shape <- "ellipse"
    nodeData <- cbind(nodeData, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))
    
    network <- createCytoscapeJsNetwork(nodeData, edgeData)
    rcytoscapejs(network$nodes, network$edges, layout=input$clay)
    
  }
  
  output$CyTb <- renderDataTable({
    wee<-paste("lassodataMYCsign")
    datum<-ddd[[wee]]
    edgeData<-datum[[input$sparsity]][1:6]
    
    colnames(edgeData) <- c("sourceName", "targetName","link","link2","color", "weight")
    edgeData<- data.frame(edgeData)
    edgeData$color<-sign(edgeData$color)
    edgeData$color<-gsub(-1,"#FF0000",edgeData$color)
    edgeData$color<-gsub(1,"#00FF00",edgeData$color)
    
    
    gD<-simplify(graph.data.frame(edgeData,directed=TRUE))
    nodeData <- data.frame(id = c(0:(igraph::vcount(gD) - 1)),name = igraph::V(gD)$name)
    getNodeID <- function(x){which(x == igraph::V(gD)$name) - 1}
    
    edgeData <- plyr::ddply(edgeData, .variables = c("sourceName", "targetName", "weight","color"),
                            function (x) data.frame(source = getNodeID(x$source),
                                                    target = getNodeID(x$target)))
    nodeData <- cbind(nodeData, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))
    
    network <- createCytoscapeJsNetwork(nodeData, edgeData)
    rcytoscapejs(network$nodes, network$edges, showPanzoom=TRUE)
    
    
    datatable(edgeData, options = list(pageLength = 5))
    
  })
  
  
  #datatable(edgeData, options = list(pageLength = 5))
  output$overlapPLOT <- renderPlotly({
    if(input$demo==TRUE){
      wee<-paste("lassoMYCsignlinkplotdata")#paste("lassodataMYCsign")
      datum<-eee[[wee]]}else{
        inFile <- input$file2
        if (is.null(inFile))
          return(NULL)
        datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep2)
        # datum<-wee
      }
    
    data<-(datum[[input$sparsity]])
    if ((identical(input$data, "MYCsign" )) ){
      data[1:(dim(data)[1]-3),11]<-(as.numeric(as.character(data[1:(dim(data)[1]-3),9]))+(max(as.numeric(as.character(data[1:(dim(data)[1]-3),6])))-max(as.numeric(as.character(data[1:(dim(data)[1]-3),9])))))
      # data[1:(dim(data)[1]-3),11]<-scale(as.numeric(data[1:(dim(data)[1]-3),6]),center=F,scale=(0,max(as.numeric(data[1:(dim(data)[1]-3),9])))
      colnames(data)[colnames(data)=="V11"] <- "overlap_MYC"
      data<-data.frame(data[1:(dim(data)[1]-3),c(2,7,8,6,11)])}
    else{
      data<-data.frame(data[1:(dim(data)[1]-3),c(2,3,4,6,5)])}
    if(input$demo==TRUE){
      zee<-paste("lassoMYCsignlinkcutdata")#paste("lassodataMYCsign")
      
      cutoffs<-ggg[[zee]]}else{
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        cutoffs <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
        # cutoffs<-wee
      }
    cutoff<-(cutoffs[[input$sparsity]])
    cutoff<-data.frame(cutoff)
    long <- melt(data, id.vars = c("bins"))
    long$bins<-as.numeric(long$bins)
    long$value<-as.numeric(long$value)
    intercept1=cutoff$support.at.cross2[2]
    if(input$demo==TRUE){
      qee<-paste("lassonameMYCsign")#paste("lassodataMYCsign")
      aa<-fff[[qee]][[input$sparsity]]}else{
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        aa <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
        # datum<-wee
      }
    
    # aa<-fff[[qee]][[input$sparsity]]
    bb<-strsplit(aa,"_")
    cc<-sapply(bb,"[[",5)
    
    
    
    if (identical(input$data, "MYCsign")){
      cutoff$CUT<-cutoff$support.at.cross
      dd<-substr(cc,8,nchar(cc))
      gg=1010
      ff=10
      ccc=max(long[c(10:(dim(data)[1]),(dim(data)[1]):(((dim(data)[1])+1)*2)),]$value)
      hh=(dim(data)[1])
      ee=as.numeric(dd)/100}
    else if(identical(input$data, "BSUB")){
      cutoff$CUT<-(cutoff$support.at.cross)*10
      ccc=max(long[c(1:(dim(data)[1]),(dim(data)[1]+1):(((dim(data)[1])+1)*2)),]$value)
      
      ff=1
      hh=(dim(data)[1])
      gg=(dim(data)[1])+1
      dd<-substr(cc,8,nchar(cc))
      ee=as.numeric(dd)/100
    }else{
      cutoff$CUT<-(cutoff$support.at.cross)*10
      ccc=max(long[c(1:(dim(data)[1]),(dim(data)[1]+1):(((dim(data)[1])+1)*2)),]$value)
      
      ff=1
      hh=(dim(data)[1])
      gg=(dim(data)[1])+1
      dd<-substr(cc,5,nchar(cc))
      ee=as.numeric(dd)/10
    }
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "Frequency",
      range = c(0, ccc),
      showline = FALSE,
      showgrid = FALSE
    )
    a<-vector()
    b<-vector()
    for (i in 1:(dim(data)[1])){
      a[i]<-trapz(long$bins[c(i:(dim(data)[1]))]/(dim(data)[1]), long$value[c(i:(dim(data)[1]))]/max(long$value[c(i:(dim(data)[1]))]))
      b[i]<-trapz(long$bins[c(((hh+1)+i):(hh*2))]/(dim(data)[1]), long$value[c(((hh+1)+i):(hh*2))]/max(long$value[c(i:(dim(data)[1]))]))
    }
    # ab<-((1002-(length(na.omit(b/a)))))
    ww<-plot_ly() %>%
      add_lines(x = long$bins[c(ff:(dim(data)[1]))]/(dim(data)[1]), y = (long$value[c(ff:(dim(data)[1]))]/max(long$value[c(ff:(dim(data)[1]))])), name = "Measured",line = list(color = 'rgb(22, 96, 167)')) %>%
      add_lines(x = long$bins[c(gg:(hh*2))]/(dim(data)[1]), y = (long$value[c(gg:(hh*2))]/max(long$value[c(ff:(dim(data)[1]))])), name = "Randomized", line = list(color = 'rgb(205, 12, 24)'))%>%
      add_lines(x = long$bins[(1+hh*3):(hh*4)]/(dim(data)[1]), y = (long$value[(1+hh*3):(hh*4)]/max(long$value[(1+hh*3):(hh*4)]))*ccc, name = "Overlap, Measured", yaxis = "y2",fill = 'tozeroy',line = list(color = 'rgba(55, 15, 255,0.1)'),fillcolor = list(color = 'rgba(55, 15, 255,0.01)')) %>%
      add_lines(x = long$bins[(1+hh*2):(hh*3)]/(dim(data)[1]), y = (long$value[(1+hh*2):(hh*3)]/max(long$value[(1+hh*2):(hh*3)]))*ccc, name = "Overlap, Randomized", yaxis = "y2",fill = 'tozeroy',line = list(color = 'rgba(207, 114, 129,0.1)'),fillcolor = list(color = 'rgba(207, 114, 129,0.01)')) %>%
      # add_lines(x=c(1:(length(na.omit(b/a)))/(dim(data)[1])),y=na.omit(b/a), name = "FDR",line = list(color = 'grey',dash = "dash")) %>%
      add_lines(x=c(1:(dim(data)[1]))/(dim(data)[1]),y=na.locf(b/a)/2, name = "FDR",line = list(color = 'grey',dash = "dash")) %>%
      add_trace(x = as.numeric(ee), y = c(0,1),line = list(dash = "dash",color = "orange"),type='scatter',mode='lines',name="support at cutoff") %>%
      # add_trace(x = (cutoff$CUT[1])-.02, y = c(0,1),line = list(dash = "dash",color = "orange"),type='scatter',mode='lines',name="support at cutoff") %>%
      layout(margin = list(l=100, r=50, b=50, t=50, pad=0),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             yaxis2 = ay,yaxis=list(range=c(0,1),title="Overlap",showline = FALSE,showgrid = FALSE), 
             xaxis = list(title="Support", y = 0.05,showline = FALSE,showgrid = FALSE),
             legend = list(x = 0.1, y = -0.2,orientation = 'h')
      )
    
    
  })
  
  output$summary <- renderPrint({
    wee<-paste("lassodataMYCsign")
    datum<-ddd[[wee]]
    
    edgeList<-datum[[input$sparsity]][1:6]
    colnames(edgeList) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
    
    gD<-simplify(graph.data.frame(edgeList,directed=TRUE))
    nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)),nName = igraph::V(gD)$name)
    getNodeID <- function(x){which(x == igraph::V(gD)$name) - 1}
    
    edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName", "Weight"), 
                            function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                    TargetID = getNodeID(x$TargetName)))
    nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))
    betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
    betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
    nodeList <- cbind(nodeList, nodeBetweenness=100*betAll.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
    rm(betAll, betAll.norm)
    
    dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")
    
    F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
    edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", "Weight", "SourceID", "TargetID"), 
                            function(x) data.frame(F1(x)))
    edgeList
    
  })
  
  
  output$downloadData <- downloadHandler({
    if(input$demo==TRUE){
      wee<-paste("lassoMYCsignlinkplotdata")#paste("lassodataMYCsign")
      datum<-eee[[wee]]}else{
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
        # datum<-wee
      }
    # datum<-ddd[[wee]]
    
    edgeList<-datum[[input$sparsity]][1:6]
    colnames(edgeList) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
    
    gD<-simplify(graph.data.frame(edgeList,directed=TRUE))
    nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)),nName = igraph::V(gD)$name)
    getNodeID <- function(x){which(x == igraph::V(gD)$name) - 1}
    
    edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName", "Weight"), 
                            function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                    TargetID = getNodeID(x$TargetName)))
    nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))
    betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
    betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
    nodeList <- cbind(nodeList, nodeBetweenness=100*betAll.norm) 
    rm(betAll, betAll.norm)
    
    dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")
    
    F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
    edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", "Weight", "SourceID", "TargetID"), 
                            function(x) data.frame(F1(x)))
    
    filename = function() { paste(wee,'.tsv', sep='')}
    content = function(file) {
      write.csv(edgeList, file)}})
  
  
}
#### UI ####
ui <- shinyUI(fluidPage(
  titlePanel("NestBoot-Viz"),
  # tabsetPanel(
  # tabPanel("Upload",
  sidebarLayout(
    sidebarPanel(
      
      fileInput('file1', 'Network file(s) to upload',multiple = TRUE),
      radioButtons('sep', 'Separator',c(Tab='\t',Comma=','),'\t'),
      tags$div(class="header", checked=NA,
               tags$p("Examples for:"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-ed/L1000", "L1000,"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-ed/MYC", "MYC,"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-ed/Arrieta-Ortiz", "Arrieta-Ortiz,"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-ed/Lorenz", "Lorenz,"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-ed/Gardner", "Gardner,")
      ),
      tags$hr(),
      fileInput('file2', 'Overlap file(s) to upload',multiple = TRUE),
      radioButtons('sep2', 'Separator',c(Tab='\t',Comma=','),'\t'),
      tags$div(class="header", checked=NA,
               tags$p("Examples for:"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-ed/L1000", "L1000,"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-ed/MYC", "MYC,"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-ed/Arrieta-Ortiz", "Arrieta-Ortiz,"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-ed/Lorenz", "Lorenz,"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-ed/Gardner", "Gardner")
      ),
      tags$hr(),
      
      sliderInput("sparsity", "Sparsity",1, min = 1,max=length(lassodataMYCsign), step = 1),#max = dim("contents")[[1]][1], 
      verbatimTextOutput("text1"), br(),verbatimTextOutput("text2"),
      tags$hr(),
      checkboxInput('demo', 'Demo', FALSE),
      checkboxInput('self', 'Self-Loop', TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Input",tableOutput("contents")),
        tabPanel("forceSign", forceNetworkOutput("forcelasso",height='800px')),
        tabPanel("CytoscapeJS",rcytoscapejsOutput("CytoscapeJS",height='800px'),selectInput("clay",label="Layout:",c("CoSE"="cose","Cola"="cola","Concentric"="concentric","Circle"="circle","Dagre"="dagre","Grid"="grid","arbor"="arbor","markov"="cytoscape-markov-cluster"))),
        tabPanel("overlap",plotlyOutput("overlapPLOT"))#,downloadButton('downloadData', 'download'))
      )))
  
  
))

#### Run ####
shinyApp(ui = ui, server = server)