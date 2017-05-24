if(!"shiny" %in% installed.packages()) 
{ 
    install.packages("shiny") 
}
library(shiny)

if(!"qgraph" %in% installed.packages()) 
{ 
    install.packages("qgraph") 
}
library(qgraph)

if(!"xlsx" %in% installed.packages()) 
{ 
    install.packages("xlsx") 
}
library(xlsx)

if(!"gridExtra" %in% installed.packages()) 
{ 
    install.packages("gridExtra") 
}
library(gridExtra)

if(!"shinythemes" %in% installed.packages()) 
{ 
    install.packages("shinythemes") 
}
library(shinythemes)

if(!"shinyjs" %in% installed.packages()) 
{ 
    install.packages("shinyjs") 
}
library(shinyjs)

ui <- navbarPage(title = "Literature Network app",
                 theme = shinytheme("cerulean"),
                 
                 tabPanel(title = "Analysis",
                          
                          strong(id = "text", 
                                 span("Created by "),
                                 a("Koen Derks", href = "https://www.linkedin.com/in/koen-derks-283273124/"),
                                 HTML("&bull;"),
                                 span("Code"),
                                 a("on GitHub", href = "https://github.com/koenderks/LiteratureNetworksApp")
                          ),
                          
                          sidebarLayout(
                          sidebarPanel(id = "sidebar", position = "right",
                                       
                                       h3("Explanation"),
                                       
                                       p('This app serves the purpose of visualizing and analyzing a literature search. It plots a network visualization (co-occurrence and/or citation network) of the literature search. Its output contains ranks for all articles. These ranks are indicators for the relevance of the article on the subject.'),
                                       
                                       strong('Note: You do not have to specify both files. It is possible to run an analysis on one or both of the networks.'),
                                       
                                       br(),
                                       br(),
                                       
                                       tags$a(href = "https://www.dropbox.com/sh/48i4t1hc63aufw0/AADUGGFkSkE3TXPYo6eaqLICa?dl=0", "Example datasets"),
                                       
                                       br(),
                                       br(),
                                       
                                       radioButtons(inputId = "sort",
                                                    label = "I want to see a:",
                                                    choices = c("Full analysis" = "Full analysis",
                                                                "Descriptives table" = "Descriptives"),
                                                    selected = "Full analysis"),
                                       
                                       br(),
                                       
                                       useShinyjs(),
                                       checkboxInput(inputId = "show",label = "Show co-occurrence data requirements"),
                                       
                                       h4(id = "line1", 'The data you select for a co-occurrence network should look like this:'),
                                       
                                       p(id = "line2", 'Column 1: ID of the raters (1,2,3 etc.).'),
                                       
                                       p(id = "line3", 'Column 2: Grade given by the rater (1-10).'),
                                       
                                       p(id = "line4", 'Column 3: General name of the article (e.g. Miller_Chapman_2001).'),
                                       
                                       radioButtons(inputId = "typedata", label = "My co-occurence data is of type:",
                                                    choices = c(".csv" = ".csv",
                                                                ".txt" = ".txt",
                                                                ".xlsx" = ".xlsx"),
                                                    selected = ".csv"),
                                       
                                       fileInput(inputId = 'data',
                                                 label = 'Choose datafile for co-occurence network'),
                                       
                                       checkboxInput(inputId = "show2",label = "Show citation data requirements"),
                                       
                                       h4(id = "line5",'The data you select for a citation network should look like this:'),
                                       
                                       p(id = "line6",'Column 1: General name of the source article (e.g. Miller_Chapman_2001).'),
                                       
                                       p(id = "line7",'Column 2: General name of the target article (e.g. Evans_Anastasio_1968).'),
                                       
                                       p(id = "line8",'Column 3: A logical indicating a citation (1 = yes, 0 = no).'),
                                       
                                       radioButtons(inputId = "typedata2", label = "My citation data is of type:",
                                                    choices = c(".csv" = ".csv",
                                                                ".txt" = ".txt",
                                                                ".xlsx" = ".xlsx"),
                                                    selected = ".csv"),
                                       
                                       fileInput('data2',
                                                 label = 'Choose datafile for citation network'),
                                       
                                       numericInput('length',
                                                    label = '(Full anaysis only) I want to see a top:',
                                                    value = 10,
                                                    min = 1,
                                                    max = 20),
                                       
                                       sliderInput(inputId = "samples", 
                                                   label = '(Full anaysis only) I want to take this many samples in the latent data augmentation:',
                                                   value = 100,
                                                   min = 1,
                                                   max = 5000),
                                       
                                       actionButton(inputId = 'action',label = 'Run analysis', icon = icon("line-chart")),
                                       
                                       br(),
                                       br(),
                                       
                                       strong('Author:'),
                                       
                                       p("Koen Derks"),
                                       
                                       p("koen-derks@student.uva.nl")
                                       
                          ),
                          
                          mainPanel(
                              
                              titlePanel("Results"),
                              
                              tableOutput(outputId = 'table'),
                              
                              plotOutput(outputId = 'plot'),
                              
                              plotOutput(outputId = 'plot2'),
                              
                              downloadButton('downloadnetwork', 'Download networks as PDF')
                              
                          )
                          
                 )
                 
                 )
                 
)

server <- function(input, output) {
    
    source("Functions_literatureNetworksApp.R")
    
    observe({
        toggle("line1",anim = TRUE,condition = input$show)
        toggle("line2",anim = TRUE,condition = input$show)
        toggle("line3",anim = TRUE,condition = input$show)
        toggle("line4",anim = TRUE,condition = input$show)
    })
    
    observe({
        toggle("line5",anim = TRUE,condition = input$show2)
        toggle("line6",anim = TRUE,condition = input$show2)
        toggle("line7",anim = TRUE,condition = input$show2)
        toggle("line8",anim = TRUE,condition = input$show2)
    })
    
    # start action button 
    observeEvent(input$action, {
        
        # co-occurrence file provided
        
        if(is.null(input$data)==FALSE & is.null(input$data2)==TRUE){
            
            progress <- shiny::Progress$new()
            
            on.exit(progress$close())
            
            progress$set(message = "Analysing", value = 0)
            
            file <- input$data
            
            if(input$sort=='Full analysis'){
                
                progress$inc(0.10, detail = 'Reading co-occurrence data')
                
            } else if (input$sort=='Descriptives'){
                
                progress$inc(0.50, detail = 'Reading co-occurrence data')
                
            }
            
            if (input$typedata == '.csv'){
                
                dat <- read.csv(file$datapath,sep=';')
                
            } else if (input$typedata == '.xlsx'){
                
                dat <- read.xlsx(file$datapath,sheetIndex = 1)
                
            } else if(input$typedata == '.txt'){
                
                dat <- read.table(file$datapath)
            }
            
            rater_dat <- dat
            
            x<-order(rater_dat[,3],decreasing = FALSE)
            
            y<-unique(rater_dat[,3][x])
            
            y<-na.omit(y)
            
            rater_dat<-data.frame(
                
                'Rater'=rater_dat[,1][x],
                
                'Grade'=rater_dat[,2][x],
                
                'Name'=rater_dat[,3][x])
            
            if(input$sort=='Full analysis'){
                
                progress$inc(0.10, detail = 'Analysing descriptives')
                
            } else if (input$sort=='Descriptives'){
                
                progress$inc(0.50, detail = 'Analysing descriptives')
                
            }
            
            stat<-matrix(NA,length(y),3)
            
            colnames(stat)<-c('Raters','Grade_mean','SD')
            
            rownames(stat)<-y
            
            for(i in 1:length(y)){
                
                p<-as.character(y[i])
                
                set<-subset(rater_dat,rater_dat$Name==p)
                
                grades<-as.numeric(as.vector(set$Grade))
                
                grades <- grades[!is.na(grades)]
                
                m<-round(mean(grades,na.rm = TRUE),digits = 1)
                
                s<-round(sd(grades),digits=1)
                
                mat<-matrix(grades,1,length(grades))
                
                stat[i,1]<-length(mat[1,])
                
                stat[i,2]<-m
                
                stat[i,3]<-s
                
            }
            
            stat<-as.data.frame(stat)
            
            stat<-subset(stat,stat$Raters!=0)
            
            stat<-stat[order(stat$Raters,decreasing = TRUE),]
            
            if(input$sort=='Full analysis'){
                
                progress$inc(0.30, detail = 'Creating adjacency matrix')
                
                graph_data <- dat[,c(1,3)]
                
                graph<-matrix(0,length(y),length(y))
                
                rownames(graph) <- y
                
                colnames(graph) <- y
                
                for (rater in sort(unique(dat[,1]))){
                    
                    set <- as.character(subset(graph_data,graph_data[,1] == rater)[,2])
                    
                    for(j in 1:length(set)){
                        
                        for(i in 1:length(set)){
                            
                            pin_1 <- set[i]
                            
                            pin_2 <- set[j]
                            
                            pin_1 <- which(y == pin_1)
                            
                            pin_2 <- which(y == pin_2)
                            
                            if (pin_1 != pin_2){
                                
                                graph[pin_1,pin_2] <- graph[pin_1,pin_2]+1
                                
                            } else {
                                
                                graph[pin_1,pin_2] <- 0
                                
                            }
                            
                        }
                        
                    }
                    
                }
                
                net<-qgraph(graph,
                            layout='spring',
                            cut=2,
                            title='Co-occurrence Network',
                            labels = 1:nrow(graph))
                
                cent <- centrality_auto(net)$node.centrality
                
                rownames(cent) <- y
                
                progress$inc(0.20, detail = 'Computing centrality measures')
                
                # Ratings based on betweenness occurance
                cent<-cent[order(-cent$Betweenness),]
                
                Rank_Betweenness_occurance<-rank(-cent$Betweenness,ties.method = 'average')
                
                cent<-cbind(cent,Rank_Betweenness_occurance)
                
                # Ratings based on closeness occurance
                cent<-cent[order(-cent$Closeness),]
                
                Rank_Closeness_occurance<-rank(-cent$Closeness,ties.method = 'average')
                
                cent<-cbind(cent,Rank_Closeness_occurance)
                
                # Ratings based on strength occurance
                cent<-cent[order(-cent$Strength),]
                
                Rank_Strength_occurance<-rank(-cent$Strength,ties.method = 'average')
                
                cent<-cbind(cent,Rank_Strength_occurance)
                
                colnames(cent) <- c('Betweenness',
                                    'Closeness',
                                    'Strength',
                                    'Rank_Betweenness',
                                    'Rank_Closeness',
                                    'Rank_Strength'
                )
                
                cent <- cent[ order(row.names(cent)), ]
                
                final <- merge(stat,cent,by=0)
                
                rownames(final)<-final$Row.names
                
                final<-final[,-1]
                
                # Compute overall rank using gibbs sampler
                
                require(MASS)
                require(mvtnorm)
                
                # Co-occurrence network
                
                progress$inc(0.20, detail = 'Computing overall ranks')
                
                XY1<-PolyCorFisherBivNorm(xranks = final$Rank_Betweenness,
                                          yranks= final$Rank_Closeness,
                                          nSamples = input$samples)
                
                ZY1<-PolyCorFisherBivNorm(xranks = final$Rank_Closeness,
                                          yranks = final$Rank_Strength,
                                          nSamples = input$samples)
                
                XZ1<-PolyCorFisherBivNorm(xranks = final$Rank_Betweenness,
                                          yranks= final$Rank_Strength,
                                          nSamples = input$samples)
                
                xy1rank<-apply(XY1$xSamples,MARGIN = 2,FUN = median)
                
                xy2rank<-apply(XY1$ySamples,MARGIN = 2,FUN = median)
                
                xyrank<-(xy1rank+xy1rank)/2
                
                zy1rank<-apply(ZY1$xSamples,MARGIN = 2,FUN = median)
                
                zy2rank<-apply(ZY1$ySamples,MARGIN = 2,FUN = median)
                
                zyrank<-(zy1rank+zy2rank)/2
                
                xz1rank<-apply(XZ1$xSamples,MARGIN = 2,FUN = median)
                
                xz2rank<-apply(XZ1$ySamples,MARGIN = 2,FUN = median)
                
                xzrank<-(xz1rank+xz2rank)/2
                
                Overall_rank<-rank((xzrank+xyrank+zyrank)/3)
                
                final<-cbind(final,Overall_rank)
                
                final<-final[order(final$Overall_rank),]
                
                progress$inc(0.10, detail = 'Returning data')
                
                # return data
                ret<-data.frame(
                    'Article'=rownames(head(final,n=input$length)),
                    'Rank'=head(final$Overall_rank,n=input$length))
                
                output$plot <- renderPlot(
                    qgraph(graph,
                           layout='spring',
                           cut=2,
                           title='Co-occurrence network',
                           labels = 1:nrow(graph))
                )
                
                output$table <- renderTable(ret)
                
                output$downloadnetwork <- downloadHandler(
                    
                    filename = function()
                    {
                        paste("network", class = ".pdf", sep = "") 
                    },
                    
                    content = function(file) 
                    {
                        pdf(file)
                        qgraph(graph,
                               layout='spring',
                               cut=2,
                               title='Co-occurrence network',
                               labels = 1:nrow(graph))
                        dev.off()
                    })
                
            } else if(input$sort=='Descriptives'){
                
                output$table<-renderTable(stat,rownames=TRUE)
                
            }
            
        }
        
        if(is.null(input$data2)==FALSE & is.null(input$data)==TRUE){
            
            file<-input$data2
            
            progress <- shiny::Progress$new()
            
            on.exit(progress$close())
            
            progress$set(message = "Analysing", value = 0)
            
            progress$inc(0.20, detail = 'Reading edgelist')
            
            if (input$typedata2 == '.csv'){
                
                edgelist <- read.csv(file$datapath,sep=';')
                
            } else if (input$typedata2 == '.xlsx'){
                
                edgelist <- read.xlsx(file$datapath,sheetIndex = 1)
                
            } else if(input$typedata2 == '.txt'){
                
                edgelist <- read.table(file$datapath)
            }
            
            if(input$sort=='Full analysis'){
                
                progress$inc(0.20, detail = 'Creating citation network')
                
                cit<-qgraph(edgelist, 
                            layout="spring", 
                            vsize=7, 
                            edge.color="darkblue", 
                            repulsion =0.6)
                
                progress$inc(0.20, detail = 'Computing centrality')
                
                cent<-centrality_auto(cit)$node.centrality
                
                progress$inc(0.20, detail = 'Computing ranks')
                
                XY1<-PolyCorFisherBivNorm(xranks = rank(-cent$Betweenness,ties.method = 'average'),
                                          yranks= rank(-cent$InDegree,ties.method = 'average'),
                                          nSamples = input$samples)
                
                ZY1<-PolyCorFisherBivNorm(xranks = rank(-cent$InDegree,ties.method = 'average'),
                                          yranks = rank(-cent$OutDegree,ties.method = 'average'),
                                          nSamples = input$samples)
                
                XZ1<-PolyCorFisherBivNorm(xranks = rank(-cent$OutDegree,ties.method = 'average'),
                                          yranks= rank(-cent$Betweenness,ties.method = 'average'),
                                          nSamples = input$samples)
                
                xy1rank<-apply(XY1$xSamples,MARGIN = 2,FUN = median)
                
                xy2rank<-apply(XY1$ySamples,MARGIN = 2,FUN = median)
                
                xyrank<-(xy1rank+xy1rank)/2
                
                zy1rank<-apply(ZY1$xSamples,MARGIN = 2,FUN = median)
                
                zy2rank<-apply(ZY1$ySamples,MARGIN = 2,FUN = median)
                
                zyrank<-(zy1rank+zy2rank)/2
                
                xz1rank<-apply(XZ1$xSamples,MARGIN = 2,FUN = median)
                
                xz2rank<-apply(XZ1$ySamples,MARGIN = 2,FUN = median)
                
                xzrank<-(xz1rank+xz2rank)/2
                
                overall<-rank((xzrank+xyrank+zyrank)/3,ties.method = 'average')
                
                progress$inc(0.20, detail = 'Rendering output')
                
                output$plot<- renderPlot(qgraph(edgelist, 
                                                layout="spring", 
                                                vsize=7, 
                                                edge.color="darkblue", 
                                                repulsion =0.6,
                                                title='Citation network'))
                
                res<-data.frame('Article'=rownames(cent),
                                'Rank'=overall)
                
                res<-res[order(res$Rank),]
                
                
                output$table<-renderTable(head(res,n=input$length))
                
                output$downloadnetwork <- downloadHandler(
                    
                    filename = function()
                    {
                        paste("network", class = ".pdf", sep = "") 
                    },
                    
                    content = function(file) 
                    {
                        pdf(file)
                        
                        qgraph(edgelist, 
                               layout="spring", 
                               vsize=7, 
                               edge.color="darkblue", 
                               repulsion =0.6,
                               title='Citation network')
                        dev.off()
                        
                    })
                
            } else if (input$sort =='Descriptives'){
                
                citacen<-qgraph(edgelist, 
                                layout="spring", 
                                vsize=7, 
                                edge.color="darkblue", 
                                repulsion =0.6,
                                title='Citation network')
                
                cita<-centrality_auto(citacen)$node.centrality
                
                cita<-cbind(rownames(cita),cita)
                
                cita<-cita[,c(1,4)]
                
                colnames(cita)<-c('Article','Cited')
                
                cita<-cita[order(-cita$Cited),]
                
                output$table<-renderTable(cita)
                
            }
            
        } # end citation network
        
        if(is.null(input$data)==FALSE & is.null(input$data2)==FALSE){
            
            progress <- shiny::Progress$new()
            
            on.exit(progress$close())
            
            progress$set(message = "Analysing", value = 0)
            
            file <- input$data
            
            progress$inc(0.10, detail = 'Reading co-occurrence data')
            
            if (input$typedata == '.csv'){
                
                dat <- read.csv(file$datapath,sep=';')
                
            } else if (input$typedata == '.xlsx'){
                
                dat <- read.xlsx(file$datapath,sheetIndex = 1)
                
            } else if(input$typedata == '.txt'){
                
                dat <- read.table(file$datapath)
            }
            
            rater_dat <- dat
            
            x<-order(rater_dat[,3],decreasing = FALSE)
            
            y<-unique(rater_dat[,3][x])
            
            y<-na.omit(y)
            
            rater_dat<-data.frame(
                
                'Rater'=rater_dat[,1][x],
                
                'Grade'=rater_dat[,2][x],
                
                'Name'=rater_dat[,3][x])
            
            if(input$sort=='Full analysis'){
                
                progress$inc(0.20, detail = 'Analysing descriptives')
                
            } else if(input$sort=='Descriptives'){
                
                progress$inc(0.20, detail = 'Analysing descriptives')
                
            } 
            
            stat<-matrix(NA,length(y),3)
            
            colnames(stat)<-c('Raters','Grade_mean','SD')
            
            rownames(stat)<-y
            
            for(i in 1:length(y)){
                
                p<-as.character(y[i])
                
                set<-subset(rater_dat,rater_dat$Name==p)
                
                grades<-as.numeric(as.vector(set$Grade))
                
                grades <- grades[!is.na(grades)]
                
                m<-round(mean(grades,na.rm = TRUE),digits = 1)
                
                s<-round(sd(grades),digits=1)
                
                mat<-matrix(grades,1,length(grades))
                
                stat[i,1]<-length(mat[1,])
                
                stat[i,2]<-m
                
                stat[i,3]<-s
                
            }
            
            stat<-as.data.frame(stat)
            
            stat<-subset(stat,stat$Raters!=0)
            
            stat<-stat[order(stat$Raters,decreasing = TRUE),]
            
            progress$inc(0.10, detail = 'Creating adjacency matrix')
            
            graph_data <- dat[,c(1,3)]
            
            graph<-matrix(0,length(y),length(y))
            
            rownames(graph) <- y
            
            colnames(graph) <- y
            
            for (rater in sort(unique(dat[,1]))){
                
                set <- as.character(subset(graph_data,graph_data[,1] == rater)[,2])
                
                for(j in 1:length(set)){
                    
                    for(i in 1:length(set)){
                        
                        pin_1 <- set[i]
                        
                        pin_2 <- set[j]
                        
                        pin_1 <- which(y == pin_1)
                        
                        pin_2 <- which(y == pin_2)
                        
                        if (pin_1 != pin_2){
                            
                            graph[pin_1,pin_2] <- graph[pin_1,pin_2]+1
                            
                        } else {
                            
                            graph[pin_1,pin_2] <- 0
                            
                        }
                        
                    }
                    
                }
                
            }
            
            net<-qgraph(graph,
                        layout='spring',
                        cut=2,
                        title='Co-occurrence Network',
                        nodeNames=rownames(graph_data),
                        legend=FALSE,
                        labels = 1:nrow(graph))
            
            file2<-input$data2
            
            progress$inc(0.20, detail = 'Reading edgelist')
            
            if (input$typedata2 == '.csv'){
                
                edgelist <- read.csv(file2$datapath,sep=';')
                
            } else if (input$typedata2 == '.xlsx'){
                
                edgelist <- read.xlsx(file2$datapath,sheetIndex = 1)
                
            } else if(input$typedata2 == '.txt'){
                
                edgelist <- read.table(file2$datapath)
            }
            
            if(input$sort=='Descriptives'){
                
                progress$inc(0.30, detail = 'Plotting citation network')
                
            } else if (input$sort=='Full analysis'){
                
                progress$inc(0.10, detail = 'Plotting citation network')
                
            }
            
            cit<-qgraph(edgelist, 
                        layout="spring", 
                        vsize=7, 
                        edge.color="darkblue", 
                        repulsion =0.6)
            
            if(input$sort=='Full analysis'){
                
                progress$inc(0.10, detail = 'Computing co-occurrence ranks')
                
                cent<-centrality_auto(net)$node.centrality
                
                rownames(cent)<-rownames(graph)
                
                cent2<-centrality_auto(cit)$node.centrality
                
                cent<-cent[order(rownames(cent)),]
                
                cent2<-cent2[order(rownames(cent2)),]
                
                cent$Betweenness<-rank(-cent$Betweenness)
                
                cent$Closeness<-rank(-cent$Closeness)
                
                cent$Strength<-rank(-cent$Strength)
                
                cent2$Betweenness<-rank(-cent2$Betweenness)
                
                cent2$InDegree<-rank(-cent2$InDegree)
                
                cent2$OutDegree<-rank(-cent2$OutDegree)
                
                cent2<-cent2[,-2]
                
                cent<-merge(cent,cent2,by = 0)
                
                rownames(cent)<-cent$Row.names
                
                cent<-cent[,-1]
                
                XY1<-PolyCorFisherBivNorm(xranks = rank(cent[,1],ties.method = 'average'),
                                          yranks= rank(cent[,2],ties.method = 'average'),
                                          nSamples = input$samples)
                
                ZY1<-PolyCorFisherBivNorm(xranks = rank(cent[,2],ties.method = 'average'),
                                          yranks = rank(cent[,3],ties.method = 'average'),
                                          nSamples = input$samples)
                
                XZ1<-PolyCorFisherBivNorm(xranks = rank(cent[,3],ties.method = 'average'),
                                          yranks= rank(cent[,1],ties.method = 'average'),
                                          nSamples = input$samples)
                
                xy1rank<-apply(XY1$xSamples,MARGIN = 2,FUN = median)
                
                xy2rank<-apply(XY1$ySamples,MARGIN = 2,FUN = median)
                
                xyrank<-(xy1rank+xy1rank)/2
                
                zy1rank<-apply(ZY1$xSamples,MARGIN = 2,FUN = median)
                
                zy2rank<-apply(ZY1$ySamples,MARGIN = 2,FUN = median)
                
                zyrank<-(zy1rank+zy2rank)/2
                
                xz1rank<-apply(XZ1$xSamples,MARGIN = 2,FUN = median)
                
                xz2rank<-apply(XZ1$ySamples,MARGIN = 2,FUN = median)
                
                xzrank<-(xz1rank+xz2rank)/2
                
                co_occurrence_rank<-rank((xzrank+xyrank+zyrank)/3,ties.method = 'average')
                
                progress$inc(0.10, detail = 'Computing citation ranks')
                
                XY1<-PolyCorFisherBivNorm(xranks = rank(cent[,4],ties.method = 'average'),
                                          yranks= rank(cent[,5],ties.method = 'average'),
                                          nSamples = input$samples)
                
                ZY1<-PolyCorFisherBivNorm(xranks = rank(cent[,5],ties.method = 'average'),
                                          yranks = rank(cent[,6],ties.method = 'average'),
                                          nSamples = input$samples)
                
                XZ1<-PolyCorFisherBivNorm(xranks = rank(cent[,4],ties.method = 'average'),
                                          yranks= rank(cent[,6],ties.method = 'average'),
                                          nSamples = input$samples)
                
                xy1rank<-apply(XY1$xSamples,MARGIN = 2,FUN = median)
                
                xy2rank<-apply(XY1$ySamples,MARGIN = 2,FUN = median)
                
                xyrank<-(xy1rank+xy1rank)/2
                
                zy1rank<-apply(ZY1$xSamples,MARGIN = 2,FUN = median)
                
                zy2rank<-apply(ZY1$ySamples,MARGIN = 2,FUN = median)
                
                zyrank<-(zy1rank+zy2rank)/2
                
                xz1rank<-apply(XZ1$xSamples,MARGIN = 2,FUN = median)
                
                xz2rank<-apply(XZ1$ySamples,MARGIN = 2,FUN = median)
                
                xzrank<-(xz1rank+xz2rank)/2
                
                citation_rank<-rank((xzrank+xyrank+zyrank)/3,ties.method = 'average')
                
                progress$inc(0.10, detail = 'Returning data')
                
                overall_rank<-rank(((co_occurrence_rank+citation_rank)/2))
                
                res<-data.frame(
                    'Article'=rownames(cent),
                    'Overall_rank'=overall_rank,
                    'Co_occurrence_Rank'=co_occurrence_rank,
                    'Citation_Rank'=citation_rank)
                
                res<-res[order(res$Overall_rank),]
                
            } # end full analysis
            
            #output
            output$plot<-renderPlot(qgraph(graph,
                                           layout='spring',
                                           cut=2,
                                           title='Co-occurrence Network',
                                           labels = 1:nrow(graph)))
            
            output$plot2<-renderPlot(qgraph(edgelist, 
                                            layout="spring", 
                                            vsize=7, 
                                            edge.color="darkblue", 
                                            repulsion =0.6,
                                            title='Citation network'))
            
            if(input$sort=='Full analysis'){
                
                output$table<-renderTable(head(res,n=input$length),rownames = TRUE)
                
            } else if(input$sort=='Descriptives'){
                
                output$table<-renderTable(stat,rownames=TRUE)
                
            }
            
            output$downloadnetwork <- downloadHandler(
                
                filename = function()
                {
                    paste("LiteratureNetworks", class = ".pdf", sep = "") 
                },
                
                content = function(file) 
                {
                    pdf(file)
                    
                    grid.table(head(res,n=input$length))
                    
                    qgraph(graph,
                           layout='spring',
                           cut=2,
                           title='Co-occurrence Network',
                           labels = 1:nrow(graph))
                    
                    qgraph(edgelist, 
                           layout="spring", 
                           vsize=7, 
                           edge.color="darkblue", 
                           repulsion =0.6,
                           title='Citation network')
                    
                    dev.off()
                })
            
        } # end both networks
        
    }) # end action button
    
} # end server

# Run the application 
shinyApp(ui = ui, server = server)

