library(shiny)
library(shinyWidgets)
library(shinyBS)
library(DT)
library(sqldf)
library(ggplot2)

scores <- read.csv(file.path("data.csv"), stringsAsFactors=FALSE)
runtimes <- read.csv(file.path("runtime.csv"), stringsAsFactors=FALSE)
scoresAll <- read.csv(file.path("dataAll.csv"), stringsAsFactors=FALSE)
runtimesAll <- read.csv(file.path("runtimeAll.csv"), stringsAsFactors=FALSE)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
    tags$head(tags$style(
      HTML('
           #sidebar {
              background-color: #ffffff;
          }
  
          body, label, input, button, select { 
            font-family: "Arial";
          }')
    )),
  
   # Application title
   #titlePanel("Visualization Tool (Niedner)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(id="sidebar",
        img(src = "ubc.png", width = "25%"),
        br(),
        br(),
        hr(),
        #implementing radio buttons

        
        bsCollapse(id = "sidebarCollapse", multiple = FALSE, open = NULL,
                   bsCollapsePanel("Discrete Plots", value = 1,
                           radioButtons("dist", "Select One",
                                         c("Accuracy" = "Accuracy",
                                           "F1-Score" = "F1.score")),
                           selectInput("p", 
                                       "Accuracy OR F1 Score:",
                                       c("Naive Bayes"='nv',
                                         "Linear Classifier"='lc',
                                         "Support Vector Machine"='svm',
                                         "Extreme Gradient Boosting"='egb',
                                         "Shallow Neural Network"='snn',
                                         "CNN"='cnn',
                                         "RCNN"='rcnn',
                                         "RNN-LSTM"='rnnl',
                                         "RNN-GRU"='rnng',
                                         "Bidirectional-RNN"='brnn')
                           ),
                           selectInput("q", 
                                       "Runtime:",
                                       c("Naive Bayes"='nv',
                                         "Linear Classifier"='lc',
                                         "Support Vector Machine"='svm',
                                         "Extreme Gradient Boosting"='egb',
                                         "Shallow Neural Network"='snn',
                                         "CNN"='cnn',
                                         "RCNN"='rcnn',
                                         "RNN-LSTM"='rnnl',
                                         "RNN-GRU"='rnng',
                                         "Bidirectional-RNN"='brnn')
                           ),
                           style = "info"),
                   bsCollapsePanel("Undivided Plot",  value = 2,
                           radioButtons("dist2", "Select Accuracy or F1-Score",
                                         c("Accuracy" = "Accuracy",
                                           "F1-Score" = "F1.score")),
                           radioButtons("ad", "Select Preference",
                                         c("Ascending" = "a",
                                           "descending" = "d",
                                           "No Preference" = "n")),                                   
                                   style = "info"),
                   bsCollapsePanel("Dendogram Visualization",  value = 3,
                                   shiny::actionButton(inputId='ab1', label="Show Diagram", 
                                                       icon = icon("th"), 
                                                       onclick ="window.open('http://localhost/ReviewViz/dendoTree.html', '_blank')"),
                                   style = "info"),
                   bsCollapsePanel("Topic Modeling Result",  value = 4,
                                   shiny::actionButton(inputId='ab1', label="Show Diagram", 
                                                       icon = icon("th"), 
                                                       onclick ="window.open('http://localhost/ReviewViz/circPacking.html', '_blank')"),
                                   style = "info")
        ),
        width = 4
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id = "tabs",
          # tabPanel("Plot", plotOutput("distPlot")), #, textOutput("selected_var")
          tabPanel("Generated Plots", value = 1,
                   bsCollapse(id = "plots", multiple = TRUE, open = c(1,2),
                   bsCollapsePanel("Accuracy OR F1 Score",  value = 1, # "This is a panel demo data plot",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, offset =3, plotOutput("distPlot"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Runtime", value = 2, # "This is a panel with just demo image ",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, offset =3, plotOutput("distPlot2"))
                                     )
                                   ),
                                   style = "primary"))
                   ),
          tabPanel("Undivided Plots", value = 2,
                   bsCollapse(id = "plot", multiple = TRUE, open = c(1,2),
                   bsCollapsePanel("Accuracy OR F1 Score",  value = 1, # "This is a panel demo data plot",
                                   fluidPage( 
                                     fluidRow(
                                       column(12, plotOutput("distPlot3"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Runtime", value = 2, # "This is a panel with just demo image ",
                                   fluidPage( 
                                     fluidRow(
                                       column(12, plotOutput("distPlot4"))
                                     )
                                   ),
                                   style = "primary"))
                   )
        )
      )
   )
))



# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
    cdata <- session$clientData
    
    observeEvent(input$tabs, ({
      updateCollapse(session, "sidebarCollapse", open = input$tabs)
    }))
    
    observeEvent(input$sidebarCollapse, ({
      choice <- input$sidebarCollapse
      updateTabsetPanel(session, "tabs", selected = choice)
    }))
    
    output$clientdataText <- renderText({
      paste(      session$clientData$output_myImage2_1_width,
                  session$clientData$output_myImage2_1_height,
                  session$clientData$output_myImage2_2_width,
                  session$clientData$output_myImage2_2_height,
                  session$clientData$output_myImage2_3_width,
                  session$clientData$output_myImage2_3_height,
                  input$sidebarCollapse ,sep = ' , ')
    })
    
    output$distPlot <- renderPlot({
    color.names = c("black","grey40","grey80","white")
     if(input$p=='nv'){
            if(input$dist == 'Accuracy'){
              capture <- subset(scores, (Model == "Naive Bayes"), select=c(Feature, Accuracy))
            } else {
              capture <- subset(scores, (Model == "Naive Bayes"), select=c(Feature, F1.score))
            }
     }
     if(input$p=='lc'){
            if(input$dist == 'Accuracy'){
              capture <- subset(scores, (Model == "Linear Classifier"), select=c(Feature, Accuracy))
            } else {
              capture <- subset(scores, (Model == "Linear Classifier"), select=c(Feature, F1.score))
            }
     }
     if(input$p=='svm'){
            if(input$dist == 'Accuracy'){
              capture <- subset(scores, (Model == "Support Vector Machine"), select=c(Feature, Accuracy))
            } else {
              capture <- subset(scores, (Model == "Support Vector Machine"), select=c(Feature, F1.score))
            }
     }
     if(input$p=='egb'){
            if(input$dist == 'Accuracy'){
              capture <- subset(scores, (Model == "Extreme Gradient Boosting"), select=c(Feature, Accuracy))
            } else {
              capture <- subset(scores, (Model == "Extreme Gradient Boosting"), select=c(Feature, F1.score))
            }
     }
     if(input$p=='snn'){
            if(input$dist == 'Accuracy'){
              capture <- subset(scores, (Model == "Shallow Neural Network"), select=c(Feature, Accuracy))
            } else {
              capture <- subset(scores, (Model == "Shallow Neural Network"), select=c(Feature, F1.score))
            }
     }
     if(input$p=='cnn'){
            if(input$dist == 'Accuracy'){
              capture <- subset(scores, (Model == "CNN (Transfer Learning)"), select=c(Feature, Accuracy))
            } else {
              capture <- subset(scores, (Model == "CNN (Transfer Learning)"), select=c(Feature, F1.score))
            }
     }
     if(input$p=='rcnn'){
            if(input$dist == 'Accuracy'){
              capture <- subset(scores, (Model == "RCNN (Transfer Learning)"), select=c(Feature, Accuracy))
            } else {
              capture <- subset(scores, (Model == "RCNN (Transfer Learning)"), select=c(Feature, F1.score))
            }
     }
     if(input$p=='rnnl'){
            if(input$dist == 'Accuracy'){
              capture <- subset(scores, (Model == "RNN_LSTM (Transfer Learning)"), select=c(Feature, Accuracy))
            } else {
              capture <- subset(scores, (Model == "RNN_LSTM (Transfer Learning)"), select=c(Feature, F1.score))
            }
     }
     if(input$p=='rnng'){
            if(input$dist == 'Accuracy'){
              capture <- subset(scores, (Model == "RNN_GRU (Transfer Learning)"), select=c(Feature, Accuracy))
            } else {
              capture <- subset(scores, (Model == "RNN_GRU (Transfer Learning)"), select=c(Feature, F1.score))
            }
     }
     if(input$p=='brnn'){
            if(input$dist == 'Accuracy'){
              capture <- subset(scores, (Model == "Bidirectional_RNN (Transfer Learning)"), select=c(Feature, Accuracy))
            } else {
              capture <- subset(scores, (Model == "Bidirectional_RNN (Transfer Learning)"), select=c(Feature, F1.score))
            }
     }

    if(input$dist == 'Accuracy'){
      ggplot(data=capture, aes(x=Feature, y=Accuracy)) + geom_bar(stat="identity", fill="steelblue") + geom_text(angle=90, aes(label=Accuracy), vjust=0.5, hjust = 1.1, color="white", size=5) + coord_cartesian(ylim = c(0.7, 1)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    } else {
      ggplot(data=capture, aes(x=Feature, y=F1.score)) + geom_bar(stat="identity", fill="steelblue") + geom_text(angle=90, aes(label=F1.score), vjust=0.5, hjust = 1.1, color="white", size=5)+ coord_cartesian(ylim = c(0.7, 1)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    })

    output$distPlot2 <- renderPlot({
    color.names = c("black","grey40","grey80","white")
     if(input$p=='nv'){
            capture <- subset(runtimes, (Model == "Naive Bayes"), select=c(Feature, RunTime))
     }
     if(input$p=='lc'){
            capture <- subset(runtimes, (Model == "Linear Classifier"), select=c(Feature, RunTime))
     }
     if(input$p=='svm'){
            capture <- subset(runtimes, (Model == "Support Vector Machine"), select=c(Feature, RunTime))
     }
     if(input$p=='egb'){
            capture <- subset(runtimes, (Model == "Extreme Gradient Boosting"), select=c(Feature, RunTime))
     }
     if(input$p=='snn'){
            capture <- subset(runtimes, (Model == "Shallow Neural Network"), select=c(Feature, RunTime))
     }
     if(input$p=='cnn'){
            capture <- subset(runtimes, (Model == "CNN (Transfer Learning)"), select=c(Feature, RunTime))
     }
     if(input$p=='rcnn'){
            capture <- subset(runtimes, (Model == "RCNN (Transfer Learning)"), select=c(Feature, RunTime))
     }
     if(input$p=='rnnl'){
            capture <- subset(runtimes, (Model == "RNN_LSTM (Transfer Learning)"), select=c(Feature, RunTime))
     }
     if(input$p=='rnng'){
            capture <- subset(runtimes, (Model == "RNN_GRU (Transfer Learning)"), select=c(Feature, RunTime))
     }
     if(input$p=='brnn'){
            capture <- subset(runtimes, (Model == "Bidirectional_RNN (Transfer Learning)"), select=c(Feature, RunTime))
     }

      ggplot(data=capture, aes(x=Feature, y=RunTime)) + geom_bar(stat="identity", fill="steelblue") + geom_text(angle=90, aes(label=RunTime), vjust=0.5, hjust = 1.1, color="white", size=5) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    })

    output$distPlot3 <- renderPlot({
     if(input$dist2=='Accuracy'){
            capture <- subset(scoresAll, select=c(ModelFeature,Accuracy))
            if(input$ad == "n"){
              ggplot(data = capture, aes(x=ModelFeature, y=Accuracy, color=Accuracy, group=1)) +
                                  geom_line(size = 2, alpha = 0.8) +
                                  geom_point(size =4, alpha = 0.9) +
                                  labs(x="Model+Feature",y="Accuracy")+
                                  #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                                  #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                  #theme_classic()
            }
            else if(input$ad == "a"){
              ggplot(data = capture, aes(x=reorder(ModelFeature,Accuracy), y=Accuracy, color=Accuracy, group=1)) +
                                  geom_line(size = 2, alpha = 0.8) +
                                  geom_point(size =4, alpha = 0.9) +
                                  labs(x="Model+Feature",y="Accuracy")+
                                  #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                                  #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                  #theme_classic()
            }
            else if(input$ad == "d"){
              ggplot(data = capture, aes(x=reorder(ModelFeature,-Accuracy), y=Accuracy, color=Accuracy, group=1)) +
                                  geom_line(size = 2, alpha = 0.8) +
                                  geom_point(size =4, alpha = 0.9) +
                                  labs(x="Model+Feature",y="Accuracy")+
                                  #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                                  #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                  #theme_classic()
            }
     } else{
            capture <- subset(scoresAll, select=c(ModelFeature,F1.score))
            
            if(input$ad == "n"){
                  ggplot(data = capture, aes(x=ModelFeature, y=F1.score, color=F1.score, group=1)) +
                          geom_line(size = 2, alpha = 0.75) +
                          geom_point(size =4, alpha = 0.9) +
                          labs(x="Model+Feature",y="F1-Score")+
                          #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                          #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                          #theme_classic()
            }            
            else if(input$ad == "a"){
                  ggplot(data = capture, aes(x=reorder(ModelFeature,F1.score), y=F1.score, color=F1.score, group=1)) +
                          geom_line(size = 2, alpha = 0.75) +
                          geom_point(size =4, alpha = 0.9) +
                          labs(x="Model+Feature",y="F1-Score")+
                          #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                          #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                          #theme_classic()
            }
            else if(input$ad == "d"){
                  ggplot(data = capture, aes(x=reorder(ModelFeature,-F1.score), y=F1.score, color=F1.score, group=1)) +
                          geom_line(size = 2, alpha = 0.75) +
                          geom_point(size =4, alpha = 0.9) +
                          labs(x="Model+Feature",y="F1-Score")+
                          #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                          #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                          #theme_classic()
            }
     }
    })

    output$distPlot4 <- renderPlot({
            capture <- subset(runtimesAll, select=c(ModelFeature,RunTime))

            if(input$ad == "n"){
                ggplot(data = capture, aes(x=ModelFeature, y=RunTime, color=RunTime, group=1)) +
                                    geom_line(size = 2, alpha = 0.8) +
                                    geom_point(size =4, alpha = 0.9) +
                                    labs(x="Model+Feature",y="RunTime")+
                                    #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                                    #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                                    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                    #theme_classic()
            }
            else if(input$ad == "a"){
                ggplot(data = capture, aes(x=reorder(ModelFeature,RunTime), y=RunTime, color=RunTime, group=1)) +
                                    geom_line(size = 2, alpha = 0.8) +
                                    geom_point(size =4, alpha = 0.9) +
                                    labs(x="Model+Feature",y="RunTime")+
                                    #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                                    #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                                    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                    #theme_classic()
            }
            else if(input$ad == "d"){
                ggplot(data = capture, aes(x=reorder(ModelFeature,-RunTime), y=RunTime, color=RunTime, group=1)) +
                                    geom_line(size = 2, alpha = 0.8) +
                                    geom_point(size =4, alpha = 0.9) +
                                    labs(x="Model+Feature",y="RunTime")+
                                    #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                                    #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                                    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                    #theme_classic()
            }
    })
})

# Run the application 
shinyApp(ui = ui, server = server)