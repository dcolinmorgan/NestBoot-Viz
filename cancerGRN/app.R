

##improve file grep via github API https://developer.github.com/v3/repos/#list-all-topics-for-a-repository

#### Load necessary packages and data ####
library(shiny)
library(igraph)
library(zoo)
library(dplyr)
library(gtools)
#library(shinythemes)
library(plyr)
library('phangorn')
library(magrittr)
library(rcytoscapejs)
library(networkD3)
library(pracma)
library(devtools)
library("RColorBrewer")
require(pracma)
library(network)
library(reshape2)
library(plotly)
library(radarchart)
require(visNetwork, quietly = TRUE)
source("https://bioconductor.org/biocLite.R")
options(repos = BiocInstaller::biocinstallRepos())
getOption("repos")
library(ggtree)
library(treeio)
#### LOAD ####


load("lassodataMYCsign2017-11-08.rdata")
load("lscodataMYCsign2017-11-08.rdata")
# load("tlscodataMYCsign2018-12-21.rdata")
load("tlscodataMYCsign2019-02-14.rdata")

# ### LOAD PLOT DATA ###
load("lassoMYCsignlinkplotdata2017-02-08.rdata")
load("lscoMYCsignlinkplotdata2017-02-14.rdata")
load("tlscoMYCsignlinkplotdata2019-02-14.rdata")



ddd<-list(lassodataMYCsign=lassodataMYCsign,lscodataMYCsign=lscodataMYCsign,tlscodataMYCsign=tlscodataMYCsign)#tlscodataMYCsign)
eee<-list(lassoMYCsignlinkplotdata=lassoMYCsignlinkplotdata,lscoMYCsignlinkplotdata=lscoMYCsignlinkplotdata,tlscoMYCsignlinkplotdata=tlscoMYCsignlinkplotdata)
fff<-list(lassonameMYCsign=lassonameMYCsign,lsconameMYCsign=lsconameMYCsign,tlsconameMYCsign=tlsconameMYCsign)
ggg<-list(lassoMYCsignlinkcutdata=lassoMYCsignlinkcutdata,lscoMYCsignlinkcutdata=lscoMYCsignlinkcutdata,tlscoMYCsignlinkcutdata=tlscoMYCsignlinkcutdata)

#### Server ####
server <- function(input, output) {
  # v <- reactiveValues(data = NULL)
  output$table <- renderTable({
    datasetInput()
    
    
    # if(input$demo==TRUE | input$demo2==TRUE|input$demo3==TRUE){
    # observeEvent(input$demo, {
    #   # wee<-paste("lassodataMYCsign")
    #   v$data<-ddd[[paste("lassodataMYCsign")]]
    # })
    # observeEvent(input$demo2, {
    #   # wee<-paste("lscodataMYCsign")
    #   v$data<-ddd[[paste("lscodataMYCsign")]]
    # }) 
    # observeEvent(input$demo3, {
    #   # wee<-paste("tlscodataMYCsign")
    #   v$data<-ddd[[paste("tlscodataMYC")]]
    # }) 
    # 
    # observeEvent(input$reset, {
    #   v$data <- NULL
    #   inFile <- input$file1
    #   if (is.null(inFile)) return(NULL)
    #   v$data <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
    # })
    # if (is.null(v$data)) return()
    # edgeList<-v$data[[input$sparsity]][1:6]
    # colnames(edgeList) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
    # if(input$self==FALSE){
    # edgeList<-edgeList%>%filter_(~SourceName!=as.character(TargetName))####REMOVE SELF LINKS -- works on test network file
    # }
    
    
  })
  datasetInput <- reactive({
    if(input$raw==FALSE){
      wee<-paste(input$data,"dataMYCsign",sep="")
      datum<-ddd[[wee]]}else{
        # }else if(input$demo4==TRUE){
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
    edgeList})
  
  # datasetName <- reactive({
  #   if(input$raw==FALSE){
  #     qee<-paste(input$data,"nameMYCsign",sep="")
  #     name<-fff[[paste(input$data,"nameMYCsign",sep="")]][[input$sparsity]]}else{return()}
  #   name})
  
  output$downloadData <- downloadHandler(
    # if(input$raw==FALSE){
    #   wee<-paste(input$data,"nameMYCsign",sep="")
    #   name<-fff[[wee]]
    #   }else{return()}
    # cc=datasetName
    filename = function() { paste(fff[[paste(input$data,"nameMYCsign",sep="")]][[input$sparsity]], input$filetype, sep = ".") },
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      write.table(datasetInput(), file, sep = sep,row.names=FALSE) })
  
  output$downloadPlot <- downloadHandler(
    # filename = function() { paste(fff[[paste(input$data,"nameMYCsign",sep="")]][[input$sparsity]], input$filetype, sep = ".") },
    # content = function(file) {
    #   ggsave(file, plot = FORCE(), device = "png")
    filename = "Shinyplot.png",
    content = function(file) {
      # png(file,width=12,height=8)
      FORCE()%>%saveNetwork(file = filename)
      # dev.off()
      # }) 
    }
  )
  
  output$forcelasso <- renderForceNetwork({
    FORCE()
  })
  
  FORCE<- function(){
    # v <- reactiveValues(data = NULL)
    if(input$raw==FALSE){
      wee<-paste(input$data,"dataMYCsign",sep="")
      datum<-ddd[[wee]]}else{
        # }else if(input$demo4==TRUE){
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
    
  }
  
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
    if(input$raw==FALSE){
      qee<-paste(input$data,"nameMYCsign",sep="")
      fff[[qee]][[input$sparsity]]}else{validate(
        need(input$raw == FALSE, "cannot (yet) display multiple GRN names."))
        # inFile <- input$file1
        # if (is.null(inFile))
        #   return(NULL)
        # # aa <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
        # inFile$name[[input$sparsity]]
      }
    # invisible()
    
  })
  
  output$text2 <- renderPrint({ 
    if(input$raw==FALSE){
      wee<-paste(input$data,"dataMYCsign",sep="")
      datum<-ddd[[wee]]}else{validate(
        need(input$raw == FALSE, "cannot (yet) analyze multiple GRN."))
        # }else if(input$demo4==TRUE){
        # inFile <- input$file1
        # if (is.null(inFile))
        #   return(NULL)
        # datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
      }
       
    # dd<-data.frame(datum[input$sparsity])

    net1<-data.frame(datum[input$sparsity],stringsAsFactors = FALSE)
    # net1<-data.frame(net1)
    names(net1) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
    if(input$self==FALSE){
      net1<-net1%>%filter_(~as.character(SourceName)!=as.character(TargetName))####REMOVE SELF LINKS -- works on test network file
    }
    # net2<-net1
    net1<-network(net1[1:2],directed=TRUE,loops=TRUE)
    
    c(cat("N(odes):",network.size(net1),"\n"),cat("L(inks):",network.edgecount(net1),"\n"),cat("Sparsity:",network.edgecount(net1)/network.size(net1),"L/N"))
    invisible()
  })
  
  output$CytoscapeJS <-renderRcytoscapejs({
    if(input$raw==FALSE){
      wee<-paste(input$data,"dataMYCsign",sep="")
      datum<-ddd[[wee]]
      # datum<-lapply(datum, function(df){df[order(size(df,2)),]})
    }else{
      # }else if(input$demo4==TRUE){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
    }
    
    edgeData<-datum[[input$sparsity]][1:6]
    colnames(edgeData) <- c("sourceName", "targetName","link1","link2","link", "weight")
    if(input$self==FALSE){
      edgeData<-edgeData%>%filter_(~as.character(sourceName)!=as.character(targetName))####REMOVE SELF LINKS -- works on test network file
    }
    
    
    edgeData$edgeTargetShape<-edgeData$link1
    edgeData$edgeTargetShape<-gsub(-1,"tee",edgeData$edgeTargetShape)
    edgeData$edgeTargetShape<-gsub(1,"triangle",edgeData$edgeTargetShape)
    # edgeData$edgeSourceShape<-edgeData$edgeTargetShape
    
    edgeData$color<-edgeData$link1
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
  
  output$overlapPLOT <- renderPlotly({
    
    
    
    if(input$raw==FALSE){
      wee<-paste(input$data,"MYCsignlinkplotdata",sep="")#paste("lassodataMYCsign")
      datum<-eee[[wee]]
      data<-(datum[[input$sparsity]])
      zee<-paste(input$data,"MYCsignlinkcutdata",sep="")#paste("lassodataMYCsign")
      cutoffs<-ggg[[zee]]
      qee<-paste(input$data,"nameMYCsign",sep="")#paste("lassodataMYCsign")
      aa<-fff[[qee]][[input$sparsity]]
      if(identical(input$data,"tlsco")){validate( need(input$data != "tlsco", "not available"))}
    }else{validate( need(input$raw == FALSE, "cannot (yet) analyze uploaded GRN overlap"))
        # inFile <- input$file2
        # if (is.null(inFile))
        #   return(NULL)
        # datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep2,comment="#")
        # data<-(datum[[input$sparsity]])
        # cutoffs<-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep2,skip=dim(data[[1]])[1]-3,comment="#")
        # # data[(dim(lassoMYCsignlinkplotdata[[1]])[1]-2):dim(lassoMYCsignlinkplotdata[[1]])[1],]
        # # cutoffs[1]<-NULL
        # # colnames(cutoffs)<-cutoffs[1,]
        # 
        # aa <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep2)
        # # cutoffs<-datum
        # # datum<-wee
      }
    
    # data<-(datum[[input$sparsity]])
    # if ((identical(input$data, "MYCsign" )) )
    if(input$raw==FALSE){
      data[1:(dim(data)[1]-3),11]<-(as.numeric(as.character(data[1:(dim(data)[1]-3),9]))+(max(as.numeric(as.character(data[1:(dim(data)[1]-3),6])))-max(as.numeric(as.character(data[1:(dim(data)[1]-3),9])))))
      # data[1:(dim(data)[1]-3),11]<-scale(as.numeric(data[1:(dim(data)[1]-3),6]),center=F,scale=(0,max(as.numeric(data[1:(dim(data)[1]-3),9])))
      colnames(data)[colnames(data)=="V11"] <- "overlap_MYC"
      # data<-data.frame(data[1:(dim(data)[1]-3),c(2,7,8,6,11)])
      data<-data.frame(data[1:(dim(data)[1]-3),c(2,3,4,6,5)])
      }else{
      data<-data.frame(data[1:(dim(data)[1]-3),c(2,3,4,6,5)])
      colnames(data)<-c("bins","Data","shuffle","overlap","overlap_shuffle")
      cutoff<-(cutoffs[[input$sparsity]])
      cutoff<-data.frame(cutoffs)
      colnames(cutoff)<-c("","yrange","sparsity","","","","","","","support at cross","support at cross2")
    }
    
    # if(input$demo==TRUE){
    #   zee<-paste("lassoMYCsignlinkcutdata")#paste("lassodataMYCsign")
    #   
    #   cutoffs<-ggg[[zee]]}else{
    #     inFile <- input$file1
    #     if (is.null(inFile))
    #       return(NULL)
    #     cutoffs <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
    #     cutoffs[(dim(lassoMYCsignlinkplotdata[[1]])[1]-3):dim(lassoMYCsignlinkplotdata[[1]])[1],]
    #     # cutoffs<-wee
    #   }
    
    cutoff<-(cutoffs[[input$sparsity]])
    cutoff<-data.frame(cutoffs)
    long <- melt(data, id.vars = c("bins"))
    long$bins<-as.numeric(long$bins)
    long$value<-as.numeric(long$value)
    intercept1=cutoff$support.at.cross2[2]
    
    # if(input$demo==TRUE){
    #   qee<-paste("lassonameMYCsign")#paste("lassodataMYCsign")
    #   aa<-fff[[qee]][[input$sparsity]]}else{
    #     inFile <- input$file1
    #     if (is.null(inFile))
    #       return(NULL)
    #     aa <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
    #     # datum<-wee
    #   }
    
    # aa<-fff[[qee]][[input$sparsity]]
    bb<-strsplit(aa,"_")
    cc<-sapply(bb,"[[",5)
    
    
    
    if (input$raw==FALSE){
      cutoff$CUT<-cutoff$support.at.cross
      dd<-substr(cc,8,nchar(cc))
      gg=1010
      ff=10
      ccc=max(long[c(10:(dim(data)[1]),(dim(data)[1]):(((dim(data)[1])+1)*2)),]$value)
      hh=(dim(data)[1])
      ee=(as.numeric(dd))/100
      
    # else if(identical(input$data, "BSUB")){
    #   cutoff$CUT<-(cutoff$support.at.cross)*10
    #   ccc=max(long[c(1:(dim(data)[1]),(dim(data)[1]+1):(((dim(data)[1])+1)*2)),]$value)
    #   
    #   ff=1
    #   hh=(dim(data)[1])
    #   gg=(dim(data)[1])+1
    #   dd<-substr(cc,8,nchar(cc))
    #   ee=as.numeric(dd)/100
    # }
    }else{
      cutoff$CUT<-(cutoff$support.at.cross)*10
      ccc=max(long[c(1:(dim(data)[1]),(dim(data)[1]+1):(((dim(data)[1])+1)*2)),]$value)
      
      ff=1
      hh=(dim(data)[1])
      gg=(dim(data)[1])+1
      dd<-substr(cc,5,nchar(cc))
      # ee=as.numeric(dd)/10
      ee=as.numeric(cutoff$CUT)
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
      add_lines(x = long$bins[c(gg:(hh*2))]/(dim(data)[1]), y = (long$value[c(gg:(hh*2))]/max(long$value[c(ff:(dim(data)[1]))])), name = "Shuffled", line = list(color = 'rgb(205, 12, 24)'))%>%
      add_lines(x = long$bins[(1+hh*3):(hh*4)]/(dim(data)[1]), y = (long$value[(1+hh*3):(hh*4)]/max(long$value[(1+hh*3):(hh*4)]))*ccc, name = "Overlap, Measured", yaxis = "y2",fill = 'tozeroy',line = list(color = 'rgba(55, 15, 255,0.1)'),fillcolor = list(color = 'rgba(55, 15, 255,0.01)')) %>%
      add_lines(x = long$bins[(1+hh*2):(hh*3)]/(dim(data)[1]), y = (long$value[(1+hh*2):(hh*3)]/max(long$value[(1+hh*2):(hh*3)]))*ccc, name = "Overlap, Shuffled", yaxis = "y2",fill = 'tozeroy',line = list(color = 'rgba(207, 114, 129,0.1)'),fillcolor = list(color = 'rgba(207, 114, 129,0.01)')) %>%
      # add_lines(x=c(1:(length(na.omit(b/a)))/(dim(data)[1])),y=na.omit(b/a), name = "FDR",line = list(color = 'grey',dash = "dash")) %>%
      add_lines(x=c(1:(dim(data)[1]))/(dim(data)[1]),y=na.locf(b/a)/3, name = "FDR",line = list(color = 'grey',dash = "dash")) %>%
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
  
  
  
  output$jaccard <- renderPlot({
    
    if(input$raw==FALSE){validate(
      need(input$raw == TRUE, "please upload multiple, preferably dissimilar GRN."))
      # wee<-paste(input$data,"dataMYCsign",sep="")#paste("lassodataMYCsign")
      # datum<-ddd[[wee]]
      # # data<-(datum[[input$sparsity]])
      # # zee<-paste(input$data,"MYCsignlinkcutdata",sep="")#paste("lassodataMYCsign")
      # # cutoffs<-ggg[[zee]]
      # qee<-paste(input$data,"nameMYCsign",sep="")#paste("lassodataMYCsign")
      # cellline<-fff[[qee]]#[[input$sparsity]]
    }else{
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
      cellline<-inFile$name
    }
    
    JAKS<-matrix(0,length(cellline),length(cellline))
    if(input$FL==0){
      jj<-strsplit(cellline,"_")
      cellline<-sapply(jj,"[[",1)}else{}
    rownames(JAKS)<-cellline
    colnames(JAKS)<-cellline
    
    
    for(i in 1:length(cellline)){
      # myFiles<-datum
      
      # net1<-datum[i]
      
      net1<-datum[i]#[[input$sparsity]][1:6]
      net1<-data.frame(net1)
      names(net1) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
      if(input$self==FALSE){
        net1<-net1%>%filter_(~SourceName!=as.character(TargetName))####REMOVE SELF LINKS -- works on test network file
      }
      
      for(k in 1:length(cellline)){
        # net2<-datum[k]
        net2<-datum[k]#[[input$sparsity]][1:6]
        net2<-data.frame(net2)
        
        names(net2) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
        if(input$self==FALSE){
          net2<-net2%>%filter_(~SourceName!=as.character(TargetName))####REMOVE SELF LINKS -- works on test network file
        }
        a<-intersect(net1,net2)
        b<-union(net1,net2)
        JSim<-dim(a)[1]/dim(b)[1]
        JAKS[i,k]<-JSim
        # diag(JAKS)<-0
      }}
    # if(input$SIM==0){
    # c<-upgma(as.matrix(JAKS),method=input$uplay)}else{
    if (input$root==FALSE){jj<-'none'}else{jj<-FALSE}
    c<-upgma(as.matrix(1-JAKS),method=input$uplay)#}
    ggtree(c,layout=input$dendlay,branch.length=jj)+ geom_tiplab(size=6, color="black")#+ geom_nodepoint(color="red", alpha=1/4, size=10)#+geom_text(aes(x=c,label=round(c$edge.length, digits=2)))#,frame="none", adj=c(.5, -.75)))
    # plot(c)
    # edgelabels(round(c$edge.length, digits=2),frame="none", adj=c(.5, -.75))
    
    
  })
  
  output$JacTable <- renderTable({
    
    if(input$raw==FALSE){validate(
      need(input$raw == TRUE, "please upload multiple, preferably dissimilar GRN."))
      # wee<-paste(input$data,"dataMYCsign",sep="")#paste("lassodataMYCsign")
      # datum<-ddd[[wee]]
      # # data<-(datum[[input$sparsity]])
      # # zee<-paste(input$data,"MYCsignlinkcutdata",sep="")#paste("lassodataMYCsign")
      # # cutoffs<-ggg[[zee]]
      # qee<-paste(input$data,"nameMYCsign",sep="")#paste("lassodataMYCsign")
      # cellline<-fff[[qee]]#[[input$sparsity]]
    }else{
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
      cellline<-inFile$name
    }
    
    JAKS<-matrix(0,length(cellline),length(cellline))
    if(input$FL==0){
      jj<-strsplit(cellline,"_")
      cellline<-sapply(jj,"[[",1)}else{}
    rownames(JAKS)<-cellline
    colnames(JAKS)<-cellline
    
    
    for(i in 1:length(cellline)){
      # myFiles<-datum
      
      # net1<-datum[i]
      
      net1<-datum[i]#[[input$sparsity]][1:6]
      net1<-data.frame(net1)
      names(net1) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
      if(input$self==FALSE){
        net1<-net1%>%filter_(~SourceName!=as.character(TargetName))####REMOVE SELF LINKS -- works on test network file
      }
      
      for(k in 1:length(cellline)){
        # net2<-datum[k]
        net2<-datum[k]#[[input$sparsity]][1:6]
        net2<-data.frame(net2)
        
        names(net2) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
        if(input$self==FALSE){
          net2<-net2%>%filter_(~SourceName!=as.character(TargetName))####REMOVE SELF LINKS -- works on test network file
        }
        a<-intersect(net1,net2)
        b<-union(net1,net2)
        JSim<-dim(a)[1]/dim(b)[1]
        JAKS[i,k]<-JSim
        # diag(JAKS)<-0
      }}
    # if(input$SIM==0){
    #   c<-upgma(as.matrix(JAKS),method=input$uplay)}else{c<-upgma(as.matrix(1-JAKS),method=input$uplay)}
    # plot(c)
    # edgelabels(round(c$edge.length, digits=2),frame="none", adj=c(.5, -.75))
    # if(input$SIM==0){
    # if(input$show==TRUE){
    # JAKS}else{}}else{
    if(input$show==TRUE){
      1-JAKS}else{}#}
    # rownames = TRUE
    
  })
  
  
  output$radar <- renderChartJSRadar({
    
    if(input$raw==FALSE){validate(
      need(input$raw == TRUE, "please upload multiple, preferably dissimilar GRN."))
      # wee<-paste(input$data,"dataMYCsign",sep="")#paste("lassodataMYCsign")
      # datum<-ddd[[wee]]
      # # data<-(datum[[input$sparsity]])
      # # zee<-paste(input$data,"MYCsignlinkcutdata",sep="")#paste("lassodataMYCsign")
      # # cutoffs<-ggg[[zee]]
      # qee<-paste(input$data,"nameMYCsign",sep="")#paste("lassodataMYCsign")
      # cellline<-fff[[qee]]#[[input$sparsity]]
    }else{
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      datum <-lapply(rev(mixedsort(inFile$datapath)), read.csv, header=FALSE,sep = input$sep)
      cellline<-inFile$name
    }
    
    JAKS<-matrix(0,length(cellline),length(cellline))
    if(input$FL2==0){
      jj<-strsplit(cellline,"_")
      cellline<-sapply(jj,"[[",1)}else{}
    rownames(JAKS)<-cellline
    colnames(JAKS)<-cellline
    
    for(i in 1:length(cellline)){
      # myFiles<-datum
      
      net1<-datum[i]
      net1<-data.frame(net1)
      names(net1) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
      if(input$self==FALSE){
        net1<-net1%>%filter_(~SourceName!=as.character(TargetName))####REMOVE SELF LINKS -- works on test network file
      }
      for(k in 1:length(cellline)){
        net2<-datum[k]
        net2<-data.frame(net2)
        names(net2) <- c("SourceName", "TargetName","Link","Link2","Sign", "Weight")
        if(input$self==FALSE){
          net2<-net2%>%filter_(~SourceName!=as.character(TargetName))####REMOVE SELF LINKS -- works on test network file
        }
        a<-intersect(net1,net2)
        b<-union(net1,net2)
        JSim<-dim(a)[1]/dim(b)[1]
        JAKS[i,k]<-JSim
        # diag(JAKS)<-0
      }}
    # if(input$SIM2==1){
    chartJSRadar(data.frame(1-JAKS),labs=cellline,maxScale = 1,showToolTipLabel=TRUE,showLegend=input$zzz,polyAlpha=.001,scaleStepWidth = .1,scaleStartValue = input$aaa)
    # }else{    chartJSRadar(data.frame(JAKS),labs=cellline,showToolTipLabel=TRUE,showLegend=input$zzz,polyAlpha=.001,scaleStepWidth = .1,maxScale = 1-input$aaa)
    # }
    
  })
  
}
#### UI ####
ui <- shinyUI(fluidPage(#theme = shinytheme("united"),
  titlePanel("CancerGRN"),
  # tabsetPanel(
  # tabPanel("Upload",
  sidebarLayout(
    sidebarPanel(
      # actionButton("demo", "LASSO networks"),
      # actionButton("demo2", "LSCO networks"),
      radioButtons("data", label="Method",c("LASSO"="lasso", "LSCO"="lsco","TLSCO"="tlsco")),
      h4("networks inferred in:"),h5("'Perturbation-based gene regulatory network inference to unravel oncogenic mechanisms.'"), tags$a(href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE125958", " Raw data: GSE125958",target="_blank"),
      
      # actionButton("demo3", "MYC-TLSCO"),
      
      checkboxInput('self', 'Self-Loop', FALSE),
      # if(input$demo==TRUE){
      sliderInput("sparsity", "Sparsity",1, min = 1,max=30, step = 1),#max = dim("contents")[[1]][1], 
      # }else if(input$demo2==TRUE){
      #   sliderInput("sparsity", "Sparsity",1, min = 1,max=length(lscodataMYCsign), step = 1),#max = dim("contents")[[1]][1], 
      # }else if(input$demo3==TRUE){
      #   sliderInput("sparsity", "Sparsity",1, min = 1,max=length(tlscodataMYCsign), step = 1),#max = dim("contents")[[1]][1], 
      # }else(input$clear==TRUE){
      #   sliderInput("sparsity", "Sparsity",1, min = 1,max=length(sparsity), step = 1),#max = dim("contents")[[1]][1], 
      # }
      verbatimTextOutput("text1"), br(),verbatimTextOutput("text2"),
      tags$hr(),
      h4("Other Projects"),
      checkboxInput('raw', 'Upload Data', FALSE),
      
      # actionButton("reset", "Clear"),
      fileInput('file1', 'Network file(s) to upload',multiple = TRUE),
      radioButtons('sep', 'Separator',c(Tab='\t',Comma=','),'\t',inline=T),
      # tags$hr(),
      # fileInput('file2', 'Overlap file(s) to upload',multiple = TRUE), ##COMMENT OUT OVERLAP PLOT FOR PUBLICATION RE:SCALE ISSUES
      # radioButtons('sep2', 'Separator',c(Tab='\t',Comma=','),'\t',inline=T),
      
      tags$div(class="header", checked=NA,
               tags$p("Examples for each file type:"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/L1000_comparison", "L1000_comp,",target="_blank"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/L1000NestBoot_Aug2018", "L1000_full,",target="_blank"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/MycNestBoot_May2017", "MYC,",target="_blank"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/arrieta-ortizNestBoot_Feb2018", "Arrieta-Ortiz,",target="_blank"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/lorenzNestBoot_OCT2017", "Lorenz,",target="_blank"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/gardnerNestBoot_OCT2017", "Gardner",target="_blank")
      ),tags$br(),
      tags$div(class="header", checked=NA,
               tags$a(href="https://bitbucket.org/sonnhammergrni/genespider/src/BFECV/%2BMethods/BalanceFitError.m", "Inference code on Bitbucket",target="_blank")),
      tags$div(class="header", checked=NA,
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz", "Shiny app on Github",target="_blank")),
      tags$hr()
    ),
    mainPanel(
      tabsetPanel(
        
        tabPanel("CytoscapeJS",h4("This tab displays the seleceted GRN where blue links reflect up regulation while red reflect negative down regulation."),tags$hr(),selectInput("clay",label="Layout:",c("CoSE"="cose","Cola"="cola","Concentric"="concentric","Circle"="circle","Dagre"="dagre","Grid"="grid","arbor"="arbor","markov"="cytoscape-markov-cluster")),rcytoscapejsOutput("CytoscapeJS",height='800px')),
        tabPanel("forceSign",h4("This tab displays the seleceted GRN where node size and color represents overall, degree blue links reflect up regulation while red reflect negative down regulation."),tags$hr(),forceNetworkOutput("forcelasso",height='800px')),
        
        tabPanel("overlap",h4("This tab displays the entire bootstrap support range from 0 to 1, as well as overlap between all bootstrap GRNs for measured (blue) and shuffled (red) data. The FDR is estimated via a null background model based on networks inferred from shuffled data. This is done to restrict inclusion of false links by setting FDR e.g. to 5%. The dashed orange line represents the cutoff where this is reached, The dashed grey line shows how the FDR behaves as a function of the bootstrap support."),tags$hr(),plotlyOutput("overlapPLOT"))#,downloadButton('downloadData', 'download'))
        ,
        # tabPanel("UPGMA",h4("This tab displays the jaccard distance between uploaded networks in a link by link manner, as a rooted or unrooted tree."),tags$hr(),radioButtons('FL', 'Name',c(First='0',Full='1'),'0',inline=T),checkboxInput('root', 'Root', TRUE),selectInput("uplay",label="Layout:",c("average"="average","single"="single","complete"="complete","ward"="ward","mcquitty"="mcquitty","median"="median","centroid"="centroid")),selectInput("dendlay",label="Layout:",c("daylight"="daylight","circular"="circular","fan"="fan","equal_angle"="equal_angle","salnted"="slanted","rectangular"="rectangular")),plotOutput("jaccard",height='800px',width ='1000px' ),checkboxInput('show', 'ShowTable', FALSE),tableOutput("JacTable")),
        # tabPanel("radar",h4("This tab displays the jaccard distance between uploaded networks in a link by link manner, as a radar plot where all GRN are compared to one another. Jaccard distances are plotted along intersections with a GRN radar line permeating from the center."),tags$hr(),radioButtons('FL2', 'Name',c(First='0',Full='1'),'0',inline=T),checkboxInput('zzz', 'Labels', FALSE),sliderInput("aaa", "Zoom",0, min = 0,max=1, step = .01),chartJSRadarOutput("radar",height='500px')),
        tabPanel("raw",downloadButton('downloadData', 'Download'),radioButtons("filetype", "File type:",
                                                                               choices = c("csv", "tsv")),tableOutput("table"))
      )))
  
  
))

#### Run ####
shinyApp(ui = ui, server = server)

