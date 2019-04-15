# pckg <- c("caret","caretEnsemble","e1071","shiny","shinydashboard","plotly","ggplot2","gridExtra","grid","gtable",
#           "plyr","qcc","qcc","DT","shinyjs","gtools","V8","corrplot","tabplot","vcd","randomForest","rpart","xgboost",
#           "gbm","glmnetUtils","cluster","clustMixType","klaR","mclust","shinyBS","factoextra","caTools")
# install.packages(pckg)

library(caret)
library(caretEnsemble)
library(e1071)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(plyr)
library(qcc)
library(DT)
library(shinyjs)
library(gtools)
library(V8)
library(VIM)
library(corrplot)
library(tabplot)
library(vcd)
library(randomForest)
library(kernlab)
library(rpart)
library(xgboost)
library(gbm)
library(glmnetUtils)
library(cluster)
library(clustMixType)
library(klaR)
library("mclust")
library(shinyBS)
library(factoextra)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" 
options(shiny.maxRequestSize=10000*1024^2)


library(shiny)
library(caTools)

ui <- fluidPage(
  sidebarMenu(),
  mainPanel(
    tags$style(
      type="text/css",
      ".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
    tabsetPanel(
      tabPanel("Input", uiOutput("InputUpload"),
               uiOutput("FileUpload1")),
      tabPanel("Explore", uiOutput("summaryip"), actionButton("Go3", label = "Process"),
               dataTableOutput("summary", height = "auto", width = "100%")),# uiOutput("Explore")),
      tabPanel("Plots", uiOutput("PlotTypes"),
               uiOutput("HistsParams"),
               uiOutput("XaxisTypes"),
               uiOutput("YaxisTypes"),
               uiOutput("var_choose_correlation"),
               uiOutput("YaxisTypesB"),
               uiOutput("XaxisTypesB"),
               actionButton("plot", "Plot"),
               bsModal("stat_plot", title = "Statistical Plot", trigger = "plot", size = "large",
                       plotOutput("Plots_2"))),
      #             plotOutput("Plots_2")),
      tabPanel("Data Cleaning", uiOutput("Numclean"),
               uiOutput("VartobeCleanedtype"),
               uiOutput("VartobeCleaned"),
               uiOutput("CleanMethod"), 
               uiOutput("missingvalueinput"), 
               uiOutput("outlierinput"),
               uiOutput("outlierUpper"),
               uiOutput("outlierLower"),
               actionButton("clean", label = "Clean"),
               #             actionButton("check", label = "check"),
               #             verbatimTextOutput("xyz"),
               verbatimTextOutput("cp")),
      tabPanel("Variable Transformation", uiOutput("choose_file_trans"),
               uiOutput("FileUpload6"),
               uiOutput("choose_var_trans"),
               uiOutput("choose_trans_method"),
               actionButton("trans", "Transform"),
               verbatimTextOutput("trans2")),
      tabPanel("Train", 
               uiOutput("GuideTrain"),
               uiOutput("choosefiletrain"),
               uiOutput("FileUpload7"),
               uiOutput("Targets"),
               uiOutput("col_conv"),
               uiOutput("NumPredictors"),
               uiOutput("Predictors"),
               uiOutput("EvalTech"),
               uiOutput("MLTS"),
               uiOutput("Tunings"),
               uiOutput("MTRYYY"),
               uiOutput("NTREE"),
               uiOutput("SHRINKAGEEEE"),
               uiOutput("INTDEPTHHH"),
               uiOutput("NTREeSSs"),
               uiOutput("MINOBSSS"),
               uiOutput("GAMMMAAA"),
               uiOutput("CCCCC"),
               uiOutput("COMPLEXITYPARAMETER"),
               uiOutput("output_type_train"),
               actionButton("Go","Process"),
               #            actionButton("stop", "Stop Processing"),
               verbatimTextOutput("model")
      ),
      tabPanel("Predict", uiOutput("FileUpload2"),
               actionButton("Go2","Process"),
               dataTableOutput("predict")),#, uiOutput("Test"))
      tabPanel("Segmentation", uiOutput("fileEntry"),
               uiOutput("entryinput"),
               uiOutput("FileUpload5"),
               uiOutput("hierarchy_nonh"),
               uiOutput("hier_met"),
               uiOutput("hier_dist_met"),
               uiOutput("hier_clust_method"),
               uiOutput("varchooseseg_hcy"),
               uiOutput("segAlgo1"),
               uiOutput("varchoosekmeanspl"),
               uiOutput("varchoosekprotopl"),
               tags$script(HTML("$(document).ready(function() {
                                if(window.location.hash != '') {
                                $('div:not(#newWindowContent)').hide();
                                $('#newWindowContent').show();
                                $('#newWindowContent').appendTo('body');
                                }
                                })
                                ")),
               conditionalPanel(
                 condition = ("output.ishcy" == 1 ),#("input$ishcy" == 1),
                 actionButton("Plot_dend", "Plot", size = "large")
               ),
               bsModal("dend_plot", title = "Plot to help you assist you choose the number of clusters", trigger = "Plot_dend",
                       plotOutput("dend")),
               #         plotOutput("dend"),
               uiOutput("treecut"),
               uiOutput("op_tp_hc"),
               uiOutput("segAlgo"),
               uiOutput("varchooseseg"), 
               uiOutput("isscale"),
               uiOutput("isnstartk"),
               uiOutput("nstrt"),
               uiOutput("kclus"),
               uiOutput("kmod"),
               uiOutput("lamb"),
               uiOutput("kprot"),
               uiOutput("lambyes"),
               uiOutput("opsegm"),
               uiOutput("kfanny"),
               uiOutput("distFanny"),
               actionButton("Segment","Run"),
               dataTableOutput("segmentationrun"),
               dataTableOutput("create_hier_cluster"))#,#, uiOutput("Test"))
      
      
      )))


server <- function(input, output, session) {
  loadfile1 <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    S <- input$file1
    
    if(is.null(S))
      return(NULL)
    
    temp = read.csv(S$datapath, header = TRUE, stringsAsFactors = TRUE)
  })
  
  
  output$FileUpload1 <- renderUI(fileInput('file1', 'Upload DatASET', 
                                           accept = c('text/csv','text/comma-separated-values,text/plain', 
                                                      '.csv')))
  
  loadfile2 <- reactive({
    
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Dataset Loading..Kindly wait")
    
    S <- input$file2
    
    
    if (is.null(S))
      return(NULL)
    
    temp = read.csv (S$datapath, header=TRUE,stringsAsFactors = FALSE)
    
  }) #End of Load File Reactive
  
  
  loadfile3 <- reactive({
    temp <- read.csv("cleanfile.csv", header = TRUE, stringsAsFactors = FALSE)
  })
  
  # File Upload
  
  output$FileUpload2 = renderUI(
    fileInput('file2', 'Upload Testing Dataset (CSV File)',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv'))
  )
  
  
  output$InputGuidelines <- renderText({
    print(paste("The input file should have clean data ie no missing values. If not so, then please input 
                another file in the Exploration tab"))
  })
  
  output$InputUpload = renderUI({
    
    #  x <- cat("NOTE:\n 1. There should not be missing data in the file uplodaded in 'Input' 
    #           tab \n This is because few models do not accept missing values")
    # print(paste(x))
    x <- paste("Note:")
    y <- paste("File type should be only csv as only csv file will be accepted")
    #    z <- paste("2. This is because few Models does not accept missing values")
    
    HTML(paste(x,y, sep = '<br/>'))
    
    
    
  })   
  
  
  output$GuideTrain = renderUI({
    
    #  x <- cat("NOTE:\n 1. There should not be missing data in the file uplodaded in 'Input' 
    #           tab \n This is because few models do not accept missing values")
    # print(paste(x))
    x <- paste("Note:")
    y <- paste("1.There should not be missing data in the file uploaded in 'Input' tab")
    z <- paste("2. This is because few Models does not accept missing values")
    
    HTML(paste(x,y,z, sep = '<br/>'))
  })   
  
  output$choosefiletrain <- renderUI({
    selectInput( inputId = "trainFile_Choose", label = "Which file do you want to train the model on?", 
                 choices = list("File from input tab"=1, "Clean File from Data cleaning tab"=2, "New File"=3,
                                "Variable Transformed file"=4), selected = 1, 
                 multiple = FALSE)
  })
  
  #for model training tab new dataset
  loadfile7 <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    S <- input$file7
    
    if(is.null(S))
      return(NULL)
    
    temp = read.csv(S$datapath, header = TRUE, stringsAsFactors = TRUE)
  })
  
  output$FileUpload7 = renderUI(if(input$trainFile_Choose == 3){
    fileInput('file7', 'Upload Testing Dataset (CSV File)',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv'))
    
  }
  )
  
  
  loadfile8 <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    x <- read.csv("transformed_dataset.csv", fileEncoding = "UTF-8-BOM")
  })
  
  output$Targets = renderUI({
    if(input$trainFile_Choose == 1){
      df <- loadfile1()
    } else if(input$trainFile_Choose == 2){
      df <- read.csv("input_file.csv", header = TRUE, stringsAsFactors = TRUE)
    } else if(input$trainFile_Choose == 3){
      df <- loadfile7()
    } else if(input$trainFile_Choose == 4){
      df <- read.csv("transformed_dataset.csv", header = TRUE, stringsAsFactors = TRUE)
    }
    df
    # x <- names(df)
    selectInput(
      "Target", 
      label = "Choose the Target Variable",
      "",selectize=TRUE,multiple=FALSE,choices=names(df)
    )
  })
  
  
  output$NumPredictors = renderUI({
    radioButtons("NumPredictor", label = "Include all the predictors",
                 choices = list("Yes" = 1, "No" = 2), 
                 selected = 1)
  })
  
  #  output$MLTS = renderUI({b
  #    if(is.numeric(DF[,input$Target]) | is.integer(DF[,input$Target])){
  #      radioButtons("MLT", label = "Machine Learning Technique",
  #                   choices = list("Linear Regression" = 1, "Random Forest"=3), 
  #                   selected = 1)
  #    }else{radioButtons("MLT", label = "Machine Learning Technique",
  #                       choices = list("Logistic Regression"=2,"Random Forest"=3), 
  #                       selected = 1)}
  #  })  
  #input label name- MLT,  get the input for the machine learning technique
  
  #input tvariable name-cc, check if input variable is of binary but reading it as integer
  output$col_conv <- renderUI({
    radioButtons("cc", label = "Is your concerned target variable of integer type which is being used for classification?",
                 choices = list("Yes"=1, "No"=2), selected = 2)
  })
  
  
  output$MLTS = renderUI({
    if(input$trainFile_Choose == 1){
      DF <- loadfile1()
    } else if(input$trainFile_Choose == 2){
      DF <- read.csv("input_file.csv", header = TRUE, stringsAsFactors = TRUE)
    } else if(input$trainFile_Choose == 3){
      DF <- loadfile7()
    } else if(input$trainFile_Choose == 4){
      DF <- read.csv("transformed_dataset.csv", header = TRUE, stringsAsFactors = TRUE)
    }
    DF
    index <- which(names(DF) %in% input$Target)
    if(input$cc==1){
      DF[,input$Target] <- as.factor(DF[,input$Target])
    }else{}
    if(is.numeric(DF[,input$Target]) | is.integer(DF[,input$Target])){
      radioButtons("MLT", label = "Machine Learning Technique",
                   choices = list("Linear Regression" = 1, "Random Forest"=3, "Gradient Boosting"=4,
                                  "Decision Tree"=5, "Support Vector Machine"=6), 
                   selected = 3)
    }else{radioButtons("MLT", label = "Machine Learning Technique",
                       choices = list("Logistic Regression"=2,"Random Forest"=3, "Gradient Boosting" = 4,
                                      "Decision Tree"=5,"Support Vector Machine"=6), 
                       selected = 3)}
  }) 
  
  
  output$Predictors = renderUI(if(input$NumPredictor==2){
    if(input$trainFile_Choose == 1){
      df <- loadfile1()
    } else if(input$trainFile_Choose == 2){
      df <- read.csv("input_file.csv", header = TRUE, stringsAsFactors = TRUE)
    } else if(input$trainFile_Choose == 3){
      df <- loadfile7()
    } else if(input$trainFile_Choose == 4){
      df <- read.csv("transformed_dataset.csv", header = TRUE, stringsAsFactors = TRUE)
    }
    x <- names(df)
    
    selectInput(
      "Predictor", 
      label = "Choose Predictors",
      "",selectize=TRUE,multiple=TRUE,choices=x)
    #    )
  })
  
  
  output$EvalTech <- renderUI(
    {
      if(input$trainFile_Choose == 1){
        df <- loadfile1()
      } else if(input$trainFile_Choose == 2){
        df <- read.csv("input_file.csv", header = TRUE, stringsAsFactors = TRUE)
      } else if(input$trainFile_Choose == 3){
        df <- loadfile7()
      } else if(input$trainFile_Choose == 4){
        df <- read.csv("transformed_dataset.csv", header = TRUE, stringsAsFactors = TRUE)
      }
      x <- names(df)
      index <- which(x %in% input$Target )#%in% x)
      if(input$cc == 1){
        df[,index] <- as.factor(df[,index])
      }else{
        df[,index] <- df[,index]
      }
      if(is.factor(df[,index])){
        selectInput(
          "MetricType", 
          label = "Select Metric",
          "",selectize=TRUE,multiple=FALSE,choices=c("Accuracy","Kappa")
        )
      } else{
        selectInput(
          "MetricType", 
          label = "Select Metric",
          "",selectize=TRUE,multiple=FALSE,choices=c("RMSE","Rsquared")
        )
        
      }    #"RMSE","Rsquared",
      
    }
  )
  
  #input variable name- IP, check if summary wanted is for categorical or numerical
  output$summaryip <- renderUI({
    radioButtons("IP", label = "Select type of variable whose summary is required",
                 choices = list("Numeric" = 1, "Categorical" = 2, "Correlational"=3), selected = 1)
  }) 
  
  #summary tab
  output$summary <- renderDataTable(if(input$Go3){
    df <- loadfile1()  
    num_var <- sapply(df, is.numeric)
    cat_var <- !sapply(df, is.numeric)
    
    mystats <- function(x){
      if(class(x)=="numeric"){
        mini <- round(min(x, na.rm = T))
        nmiss <- sum(is.na(x))
        p1 <- round(quantile(x, 0.01, na.rm = T))
        p2 <- round(quantile(x, 0.05, na.rm = T))
        p3 <- round(quantile(x, 0.10, na.rm = T))
        
        q1 <- round(quantile(x,0.25, na.rm = T))
        q2 <- round(quantile(x,0.5, na.rm = T))
        q3 <- round(quantile(x,0.75, na.rm = T))
        p1 <- round(quantile(x,0.01, na.rm = T))
        p5 <- round(quantile(x,0.05, na.rm = T))
        p50 <- round(quantile(x,0.50, na.rm = T))
        p75 <- round(quantile(x,0.75, na.rm = T))
        p90 <- round(quantile(x,0.90, na.rm = T))
        p95 <- round(quantile(x,0.95, na.rm = T))
        p99 <- round(quantile(x,0.99, na.rm = T))
        skw <- round(skewness(x, na.rm = T, 3))
        y <- na.omit(x)
        lm <- length(unique(y))
        maxi <- round(max(x, na.rm = T))
        miss_perc <- (nmiss/count(x)) * 100
        return(c(minimum=mini, missing_number=nmiss, unique_values = lm, p=p1, p=p2,p=p3,p=q1,p=q2,p=q3,p=p90,
                 p=p95,p=p99,maximum=maxi, skewness=skw))
      }else {
        nmiss <- sum(is.na(x))
        cnt <- nlevels(x)#ifelse(class(x) == "factor",nlevels(x),0)
        #unq <- unique(x)
        #        non_miss <- nrow(x) - nmiss
        #return(c(counting = cnt))
        return(c(nmiss=nmiss, levelss = cnt))#, count_=cnt, non_missing_values = non_miss))
      }
    }
    
    if(input$IP==1){
      num_stats <- t(data.frame(apply(df[num_var],2,mystats)))
      num_stats
    } else if(isolate(input$IP==2)){
      nmiss <- sum(is.na(df[,cat_var]))
      cat_stats <- t(data.frame(sapply(df[cat_var],mystats)))
      cat_stats
    } else if(input$IP == 3){
      df1 <- na.omit(df[num_var])
      x <- cor(df1)
      x
    }
  })
  
  
  output$Tunings <- renderUI(if(input$MLT>0){
    radioButtons("tuning", label = "Do you want to tune your model", choices = list("Yes"=2, "No"=1),
                 selected = 1)
  })
  
  output$MTRYYY <- renderUI(if(input$tuning==2 & input$MLT == 3){
    if(input$trainFile_Choose == 1){
      df <- loadfile1()
    } else if(input$trainFile_Choose == 2){
      df <- read.csv("input_file.csv", header = TRUE, stringsAsFactors = TRUE)
    } else if(input$trainFile_Choose == 3){
      df <- loadfile7()
    } else if(input$trainFile_Choose == 4){
      df <- read.csv("transformed_dataset.csv", header = TRUE, stringsAsFactors = TRUE)
    }
    
    index <- which(names(df) %in% input$Target)
    if(input$cc == 1){
      df[,index] <- as.factor(df[,index])
    }
    
    y <- 1
    for (i in 1:length(input$Predictor)){
      y <- y+1
    }
    
    #    y <- count(input$Predictor)
    #    as.numeric(unlist(y))
    x <- if(is.factor(df[,index])){
      x <- floor(sqrt(y))
    } else { x <- floor(sqrt(y))}
    
    numericInput("mty", label = "enter the value for mtry", min=1, max = ncol(df), value = round(x))
  })
  
  output$NTREE <- renderUI(if(input$tuning==2 & input$MLT == 3){
    if(input$trainFile_Choose == 1){
      df <- loadfile1()
    } else if(input$trainFile_Choose == 2){
      df <- read.csv("input_file.csv", header = TRUE, stringsAsFactors = TRUE)
    } else if(input$trainFile_Choose == 3){
      df <- loadfile7()
    } else if(input$trainFile_Choose == 4){
      df <- read.csv("transformed_dataset.csv", header = TRUE, stringsAsFactors = TRUE)
    }
    index <- which(names(df) %in% input$Target)
    if(input$cc == 1){
      df[,index] <- as.factor(df[,index])
    }
    
    numericInput("ntree", label = "enter the value for ntree", min = 1, max = 20000, value = 501)
    
  })
  
  #input label shrink
  output$SHRINKAGEEEE <- renderUI(if(input$tuning==2 & input$MLT == 4){
    numericInput("shrink", label = "enter the learning rate", min = 0, max = 1, value = 0.001)
  })
  
  
  #input label intDepth
  output$INTDEPTHHH <- renderUI(if(input$tuning == 2 & input$MLT == 4){
    
    numericInput("intDepth", label = "Enter the number of leaves ie interaction depth value",
                 min=1, max = 2000, value = 1)
    
  })
  
  #input label NTRees
  output$NTREeSSs <- renderUI(if(input$tuning == 2 & input$MLT == 4){
    
    numericInput("NTReees", label = "Enter the number of trees",
                 min=1, max = 200000, value = 100)
    
  })
  
  
  #input label minObs
  output$MINOBSSS <- renderUI(if(input$tuning == 2 & input$MLT == 4){
    numericInput("minObs", label = "Enter the minimum number of samples in tree terminal nodes",
                 min=5, max = 10000, value = 10)
    
  })
  
  
  output$GAMMMAAA <- renderUI(if(input$tuning == 2 & input$MLT == 6){
    numericInput("gama", label = "Enter the value for gamma", min = 0, max = 2, value = 0.1)
  })
  
  output$CCCCC <- renderUI(if(input$tuning == 2 & input$MLT == 6){
    numericInput("ccc", label = "Enter the value for C", min = 0, max = 150, value = 1)
  })
  
  output$COMPLEXITYPARAMETER <- renderUI(if(input$tuning == 2 & input$MLT == 5){
    numericInput("c_p", label = "Enter the complexity parameter value", min = 0, max = 100000, value = 0.01)
  })
  
  
  DataTypeConversion <-reactive({
    
    
    DF<-loadfile1()
    DF[input$FactorLists] <- sapply(DF[input$FactorLists],as.factor)
    DF[input$NumericLists] <- sapply(DF[input$NumericLists],as.numeric)
    DF
    
    
    
  })
  
  # CHOOSING THE VARIABLES ON WHICH THE CORRELATION PLOT HAS TO BE CREATED
  output$var_choose_correlation <- renderUI(if(input$PlotType == "Correlation"){
    df <- loadfile1()
    nv <- sapply(df, is.numeric)
    numericcols<-as.list(colnames(df[,nv]))
    selectInput(inputId = "cor_var", label = "Choose the variables on which you want to plot the correlation plot",
                choices = numericcols, multiple = TRUE)
    
  })
  
  # output$ifplot <- renderUI(if(input$plot){
  #   plotOutput("Plots_2")
  # })
  
  #PLOTS PRINTING BASED ON INPUTS
  output$Plots_2 <- renderPlot(if(input$plot){#if(input$plot){
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing is going on..Kindly wait")
    
    df <- loadfile1()
    num_var <- sapply(df, is.numeric)
    if(isolate(input$PlotType) == "Histogram"){
      index <- which(names(df) %in% input$HistParam)
      x <- df[,index]
      plotOutput(hist(x))
      hist(x)    
    } else if(isolate(input$PlotType) == "Correlation"){
      x <- c(isolate(input$cor_var))
      df1 <- as.matrix(df[x])
      df1 <- na.omit(df1)
      x <- cor(df1)
      corrplot(x)
    } else if(isolate(input$PlotType) == "Scatterplot"){
      xplot <- which(names(df) %in% input$Xaxis)
      yplot <- which(names(df) %in% input$Yaxis)
      x <- df[,xplot]
      y <- df[,yplot]
      #      ggplot(df, aes_string(x=input$Xaxis, y=input$Yaxis)) +
      #        geom_jitter(size=2)+ xlab(paste(input$Xaxis))+ylab(input$Yaxis)+geom_smooth(method = lm )+
      #        ggtitle(paste(input$Xaxis," Vs ",input$Yaxis))
      plot.default(df[,xplot], df[,yplot], type = "p")
    } else if(isolate(input$PlotType == "Boxplot")){
      x <- which(names(df) %in% isolate(input$XaxisB))
      y <- which(names(df) %in% isolate(input$YaxisB))
      boxplot(df[,y] ~ df[,x], data = df)
    }
  })
  
  
  #CHOOSE THE COLUMN ON WHICH HISTOGRAM HAS TO BE CREATED
  output$HistsParams = renderUI(if(input$PlotType=="Histogram"){
    nums <- sapply(loadfile1(), is.numeric)
    numericcols<-as.list(colnames(loadfile1()[,nums]))
    selectInput(
      "HistParam", 
      label = "Plot Histogram",
      "",selectize=TRUE,multiple=FALSE,choices=numericcols
    )
  })
  
  #CHOOSE PLOTS BETWEEN HISTOGRAM, SCATTERPLOT, CORRELATION PLOT AND BOXPLOT
  output$PlotTypes = renderUI({
    selectInput(
      "PlotType", 
      label = "Select Plot",
      "",selectize=TRUE,multiple=FALSE,choices=c("Histogram","Scatterplot","Correlation","Boxplot")
    )
  }) 
  
  
  #dropdown to select x-axis for scatter plot  
  output$XaxisTypes = renderUI(if(input$PlotType=="Scatterplot"){
    nums <- sapply(loadfile1(), is.numeric)
    numericcols<-as.list(colnames(loadfile1()[,nums]))
    selectInput(
      "Xaxis", 
      label = "Select Xaxis",
      "",selectize=TRUE,multiple=FALSE,choices=numericcols
    )
  }) 
  
  # Dropdown to select y-axis for scatter plot
  
  output$YaxisTypes = renderUI(if(input$PlotType=="Scatterplot"){
    nums <- sapply(loadfile1(), is.numeric)
    numericcols<-as.list(colnames(loadfile1()[,nums]))
    selectInput(
      "Yaxis", 
      label = "Select Yaxis",
      "",selectize=TRUE,multiple=FALSE,choices=numericcols
    )
  })
  
  #dropdown to select y-axis for boxplot  
  output$XaxisTypesB = renderUI(if(input$PlotType=="Boxplot"){
    nums <- sapply(loadfile1(), is.numeric)
    numericcols<-as.list(colnames(loadfile1()))
    selectInput(
      "XaxisB", 
      label = "Select Xaxis",
      "",selectize=TRUE,multiple=FALSE,choices=numericcols
    )
  }) 
  
  # Dropdown to select y-axis for boxplot
  
  output$YaxisTypesB = renderUI(if(input$PlotType=="Boxplot"){
    nums <- sapply(loadfile1(), is.numeric)
    numericcols<-as.list(colnames(loadfile1()[,nums]))
    selectInput(
      "YaxisB", 
      label = "Select Yaxis",
      "",selectize=TRUE,multiple=FALSE,choices=numericcols
    )
  })
  
  
  output$Numclean = renderUI({
    radioButtons("NumClean", label = "Include all the variables",
                 choices = list("Yes" = 1, "No" = 2), 
                 selected = 1)
  })
  
  #choose the variables that need to be cleaned
  output$VartobeCleaned <- renderUI(if(input$NumClean == 2){
    df <- loadfile1()
    num_var <- sapply(df, is.numeric)
    char_var <- !sapply(df, is.numeric)
    
    if(input$varCleantype == 1){
      x <- names(df[num_var])
    }else {
      x <- names(df[char_var])
    }
    
    selectInput(
      "varClean", 
      label = "Choose the Target Variable",
      selectize=TRUE,multiple=FALSE,choices=(x)
    )
  })
  
  #choose the data type of the variables that need to be cleaned
  output$VartobeCleanedtype <- renderUI(if(input$NumClean == 2){
    radioButtons(
      "varCleantype", 
      label = "Choose the data type of the variables to be cleaned",
      choices= list("Numeric" = 1, "Categorical" = 2), selected = 1
    )
  })
  
  
  
  
  #input type of cleaning-outlier or missing value
  output$CleanMethod <- renderUI({
    if(input$NumClean == 1){
      selectInput(
        "cleanType", 
        label = "Choose the cleaning type",
        selectize=TRUE,multiple=FALSE,choices=list("missing value treatment" = 1,"outlier treatment"=2), selected = 1
      )
      
    }else if(input$NumClean == 2 & input$varCleantype == 1){
      selectInput(
        "cleanType", 
        label = "Choose the cleaning type",
        selectize=TRUE,multiple=FALSE,choices=list("missing value treatment" = 1,"outlier treatment"=2), selected = 1
      )
      
    }
  })
  
  #input the method to clean missing values
  output$missingvalueinput <- renderUI(if((input$cleanType) == 1){
    selectInput("cleanMethod", label = "Choose the missing value treatment method ",  selectize = TRUE, multiple = FALSE,
                choices = list("Mean"=1, "Median"=2, "Mode"=3, "Replace with zero value" = 4), selected = 1)
  })
  
  #input the where do you want to cap the outlier- upper or lower
  output$outlierinput <- renderUI(if((input$cleanType) == 2 ){
    selectInput("outliertype", label = "Choose the outlier treatment method ",  selectize = TRUE, multiple = FALSE,
                choices = list("Upper Bound"=1, "Lower Bound"=2), selected = 1)
  })
  
  #input the upper bound outlier treatment method- 90 perrcentile, 95 percentile, 99 percentile, standard deviation method or the inter-quartile range   
  output$outlierUpper <- renderUI(if((input$cleanType) == 2 & (input$outliertype==1)   ){#& input$cleanType == 2){
    selectInput("upperoutlier", label = "Choose the upper bound outlier treatment method",  selectize = TRUE, multiple = FALSE,
                choices = list("90 percentile"=1, "95 percentile"=2, "99 percentile"=3, "Standard deviation"=4, "inter-quartile range"=5,
                               "2*99 percentile" = 6),
                selected = 1)
  })
  
  #input the upper bound outlier treatment method- 1 perrcentile, 5 percentile, 10 percentile, standard deviation method or the inter-quartile range  
  output$outlierLower <- renderUI(if((input$outliertype) ==2 & (input$cleanType) == 2){
    selectInput("loweroutlier", label = "Choose the lower bound outlier treatment method", selectize = TRUE, multiple = FALSE,
                choices = list("1 percentile"=1, "5 percentile"=2, "10 percentile"=3, "Standard deviation"=4, "inter-quartile range"=5),
                selected = 2)
  })
  
  #  counter <- reactiveValues(countervalue = 0)
  
  #OPTION BETWEEN VARIABLE IMPORTANCE AND STATISTICAL MEASURE
  output$output_type_train <- renderUI({
    selectInput(inputId = "tot", label = "What is the type of data output do you want to see? ",
                choices = list("Statistical measures" = 1, "Variable Importance" = 2), selected = 1, multiple = FALSE)
  })
  
  ct <- reactiveValues(counter = 0)
  
  # FUNCTION TO START THE CLEANING PROCESS
  my_clean <- eventReactive(input$clean, ignoreNULL = FALSE, {
    ct$counter <- ct$counter + 1
    
    df <- loadfile1()
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing......")
    if(ct$counter == 1){
      write.csv(df, "input_file.csv", row.names = FALSE)
    }else if(ct$counter > 1) {
      df <- read.csv("input_file.csv")}
    
    index <- which(names(df) %in% input$varClean)
    varis <- input$varClean 
    
    modes <- function(x){
      ux <- na.omit(unique(x))
      ux[which.max(tabulate(match(x,ux)))]
    }
    
    num_var <- sapply(df, is.numeric)
    char_var <- !sapply(df, is.numeric)
    
    #include all variables
    if(input$NumClean == 1){
      #data type of variable is numerical
      {
        y <- ncol(df)
        for (i in 1:y) {
          if(isolate(input$cleanType == 1 & isolate(input$cleanMethod)==1 )){
            if(is.numeric(df[,i])){
              x <- df[,i]
              ab <- mean(x, na.rm = T)
              x[is.na(x)] <-ab
            } else if(is.factor(df[,i])){
              x <- df[,i]
              x[x == ""] <- 'NA'
              val <- unique(x[!is.na(x)])
              y <- val[which.max(tabulate(match(x, val)))]
              x[is.na(x)] <- y
            }
            df[,i] <- x
          } else if(isolate(input$cleanType == 1 & isolate(input$cleanMethod)==2 )){
            if(is.numeric(df[,i])){
              x <- df[,i]
              ab <- median(x, na.rm = T)
              x[is.na(x)] <-ab
            } else {
              x <- df[,i]
              x[x == ""] <- 'NA'
              y <- modes(x)
              x[is.na(x)] <- y
            }
            df[,i] <- x
          } else if(isolate(input$cleanType == 1 & isolate(input$cleanMethod)==3 )){
            if(is.numeric(df[,i])){
              x <- df[,i]
              y <- modes(x)
              x[is.na(x)] <- y
            } else {
              x <- df[,i]
              x[x == ""] <- 'NA'
              y <- modes(x)
              x[is.na(x)] <- y
            }
            df[,i] <- x
          } else if(isolate(input$cleanType == 1 & isolate(input$cleanMethod)==4 )){
            if(is.numeric(df[,i])){
              x <- df[,i]
              x[is.na(x)] <- 0
            } else {
              x <- df[,i]
              x[x == ""] <- 'NA'
              y <- modes(x)
              x[is.na(x)] <- y
            }
            df[,i] <- x
          } else  if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 1){
            if(is.numeric(df[,i])){
              x <- df[,i]
              y <- quantile(x,0.90, na.rm = TRUE)
              x[x > y ] <- y
            } else {
              x <- df[,i]
              x[x == ""] <- 'NA'
              y <- modes(x)
              x[is.na(x)] <- y
            }
            df[,i] <- x
          }else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 2){
            if(is.numeric(df[,i])){
              x <- df[,i]
              y <- quantile(x,0.95, na.rm = TRUE)
              x[x > y ] <- y
            }else{
              x <- df[,i]
              x[x == ""] <- 'NA'
              y <- modes(x)
              x[is.na(x)] <- x
            }
            df[,i] <- x
          }else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 3){
            if(is.numeric(df[,i])){
              x <- df[,i]
              y <- quantile(x,0.99, na.rm = TRUE)
              x[x > y ] <- y
            }else{
              x <- df[,i]
              y <- modes(x)
              x[is.na(x)] <- x
            }
            df[,i] <- x
            
          } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 4){
            if(is.numeric(df[,i])){
              a <- sd(df[,i])
              b <- (3*a) + mean(df[,i])
              x <- df[,i]
              x[x > b] <- b
            } else {
              x <- df[,i]
              x[x == ""] <- 'NA'
              y <- modes(x)
              x[is.na(x)] <- x
            }
            df[,i] <- x
          } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 5){
            if(is.numeric(df[,i])){
              a <- sd(df[,i])
              b <- (3*x) + mean(df[,i])
              x <- df[,i]
              y <- quantile(x,0.75, na.rm = TRUE) + (IQR(x, na.rm = TRUE) * 1.5)
              x[x > y] <- y
            }else{
              x<- df[,i]
              x[x == ""] <- 'NA'
              y <- modes(x)
              x[is.na(x)] <- y
            }
            df[,i] <- x
          } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 6){
            if(is.numeric(df[,i])){
              x <- df[,i]
              y <- 2*quantile(x,0.99, na.rm = TRUE) 
              x[x > y] <- y
            } else {
              x  <- df[,i]
              x[x == ""] <- 'NA'
              y <- modes(x)
              x[is.na(x)] <- y
            }
            df[,i] <- x
          } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 1){
            if(is.numeric(df[,i])){
              x <- df[,i]
              y <- quantile(x,0.01, na.rm = TRUE)
              x[x < y] <- y
            } else {
              x <- df[,i]
              x[x == ""] <- 'NA'
            }
            df[,i] <- x
          } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 2){
            if(is.numeric(df[,i])){
              x <- df[,i]
              y <- quantile(x,0.05, na.rm = TRUE)
              x[x < y] <- y
            }else{
              x <- df[,i]
              x[x == ""] <- 'NA'
            }
            df[,i] <- x
          } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 3){
            if(is.numeric(df[,i])){
              x <- df[,i]
              y <- quantile(x,0.10, na.rm = TRUE)
              x[x < y] <- y
              
            }else{
              x <- df[,i]
              x[x == ""] <- 'NA'
            }
            df[,i] <- x
          } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 4){
            if(is.numeric(df[,i])){
              a <- sd(df[,i])
              b <- (3*x) - mean(df[,i])
              x <- df[,i]
              x[x < b] <- b
            }else{
              x <- df[,i]
              x[x == ""] <- 'NA'
            }
            df[,i] <- x
          } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 5){
            if(is.numeric(df[,i])){
              
              a <- sd(df[,i])
              b <- (3*x) + mean(df[,i])
              x <- df[,index]
              y <- quantile(x,0.25, na.rm = TRUE) - (IQR(x, na.rm = TRUE) * 1.5)
              x[x < y] <- y
            }else{
              x <- df[,i]
              x[x == ""] <- 'NA'
            }
            df[,i] <- x
          }
        }
        
      }}else if(input$NumClean ==2){
        chng_nm <- list(c(isolate(input$varClean)))
        {
          index <- which(names(df) %in% input$varClean)
          if(is.numeric(df[,index])){
            if(isolate(input$cleanType) == 1 & isolate(input$cleanMethod)==1){
              x <- df[,index]
              #x[x=="NA"] <- 'NA'
              ab <- mean(x, na.rm = T)
              x[is.na(x)] <-ab
              
            }else if(isolate(input$cleanType) == 1 & isolate(input$cleanMethod)==2){
              x <- df[,index]
              #x[x=="NA"] <- 'NA'
              ab <- median(x, na.rm = T)
              x[is.na(x)] <-ab
              
            }else if(isolate(input$cleanType) == 1 & isolate(input$cleanMethod)==2){
              x <- df[,index]
              #x[x=="NA"] <- 'NA'
              ab <- modes(x)
              x[is.na(x)] <-ab
              
            }else if(isolate(input$cleanType) == 1 & isolate(input$cleanMethod)==2){
              x <- df[,index]
              #x[x=="NA"] <- 'NA'
              x[is.na(x)] <- 0
              
            }else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 1){
              x <- df[,index]
              
              y <- quantile(x,0.90, na.rm = TRUE)
              x[x > y ] <- y
              
            }else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 2){
              x <- df[,index]
              #x[x=="NA"] <- 'NA'
              y <- quantile(x,0.95, na.rm = TRUE)
              x[x > y ] <- y
              
            }else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 3){
              x <- df[,index]
              x[x=="NA"] <- 'NA'
              y <- quantile(x,0.99, na.rm = TRUE)
              x[x > y ] <- y
              
            } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 4){
              a <- sd(df[,index])
              b <- (3*a) + mean(df[,index])
              x <- df[,index]
              x[x > b] <- b
              
            } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 5){
              a <- sd(df[,index])
              b <- (3*x) + mean(df[,index])
              x <- df[,index]
              y <- quantile(x,0.75, na.rm = TRUE) + (IQR(x, na.rm = TRUE) * 1.5)
              x[x > y] <- y
              
            } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 6){
              x <- df[,index]
              y <- 2*quantile(x,0.99, na.rm = TRUE) 
              x[x > y] <- y
              
            }    else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 1){
              x <- df[,index]
              y <- quantile(x,0.01, na.rm = TRUE)
              x[x < y] <- y
            } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 2){
              x <- df[,index]
              y <- quantile(x,0.05, na.rm = TRUE)
              x[x < y] <- y
            } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 3){
              x <- df[,index]
              y <- quantile(x,0.10, na.rm = TRUE)
              x[x < y] <- y
            } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 4){
              a <- sd(df[,index])
              b <- (3*x) - mean(df[,index])
              x <- df[,index]
              x[x < b] <- b
            } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 5){
              a <- sd(df[,index])
              b <- (3*x) + mean(df[,index])
              x <- df[,index]
              y <- quantile(x,0.25, na.rm = TRUE) - (IQR(x, na.rm = TRUE) * 1.5)
              x[x < y] <- y
            }
          }
          else if(!is.numeric(df[,index])){
            x <- df[,index]
            x[x == ""] <- 'NA'
            y <- modes(x)
            x[is.na(x)] <- y
          }
          df[,index] <- x
        }
      }
    
    #write the data into a new file
    write.csv(df, "input_file.csv", row.names= FALSE)
    print("done")
    print(ct$counter)
  })
  
  # FUNCTION THAT CALLS THE CLEANING PROCESS
  output$cp <- renderPrint({
    
    print("start")
    Sys.sleep(5)
    my_clean()
    print("done")
    
  })
  
  
  output$xyz <- renderPrint(if(input$check){
    print("")
    chng <- list(c(isolate(input$varClean)))
    print(chng)
    print(str(chng))
  })
  
  #function to clean the data
  output$cleanProcess <- renderPrint(if(input$clean){
    counter$countervalue <- counter$countervalue + 1
    print("start")
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing......")
    
    #load the file from input tab
    if(counter$countervalue == 1){
      df <- loadfile1()
      write.csv(df, "input_file.csv")
    } else {
      df <- read.csv("input_file.csv")
    }
    
    index <- isolate(which(names(df) %in% input$varClean))
    varis <- input$varClean
    
    var <- c(df[,index])
    Outliers <- c()
    
    #function to find out the mode
    modes <- function(x){
      ux <- na.omit(unique(x))
      ux[which.max(tabulate(match(x,ux)))]
    }
    
    if(isolate(input$cleanType) == 1 & isolate(input$cleanMethod) == 1){
      x <- df[,index]
      y <- mean(x, na.rm = T)
      x[is.na(x)] <- y
      #      impute(x, mean)
    } else if(isolate(input$cleanType) == 1 & isolate(input$cleanMethod) == 2){
      
      x <- df[,index]
      y <- median(x, na.rm = T)
      x[is.na(x)] <- y
    } else if(isolate(input$cleanType) == 1 & isolate(input$cleanMethod) == 3){
      x <- df[,index]
      y <- modes(x)
      x[is.na(x)] <- y
      
      #     impute(x, mode)
    }else if(isolate(input$cleanType) == 1 & isolate(input$cleanMethod) == 4){
      
      x <- df[,index]
      y <- median(x, na.rm = TRUE)
      x[is.na(x)] <- 0
      #      impute(x, median)
    } #upper bound capping
    else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 1){
      x <- df[,index]
      y <- quantile(x,0.90, na.rm = TRUE)
      x[x > y ] <- y
      
    } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 2){
      x <- df[,index]
      y <- quantile(x,0.95, na.rm = TRUE)
      x[df[,index] > y] <- y
    } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 3){
      x <- df[,index]
      y <- quantile(x,0.99, na.rm = TRUE)
      x[x > y] <- y
    } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 4){
      a <- sd(df[,index])
      b <- (3*a) + mean(df[,index])
      x <- df[,index]
      x[x > b] <- b
    } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 5){
      a <- sd(df[,index])
      b <- (3*x) + mean(df[,index])
      x <- df[,index]
      y <- quantile(x,0.75, na.rm = TRUE) + (IQR(x, na.rm = TRUE) * 1.5)
      x[x > y] <- y
    } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 1 & isolate(input$upperoutlier) == 6){
      x <- df[,index]
      y <- 2*quantile(x,0.99, na.rm = TRUE) 
      x[x > y] <- y
    } #lower bound capping
    else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 1){
      x <- df[,index]
      y <- quantile(x,0.01, na.rm = TRUE)
      x[x < y] <- y
    } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 2){
      x <- df[,index]
      y <- quantile(x,0.05, na.rm = TRUE)
      x[x < y] <- y
    } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 3){
      x <- df[,index]
      y <- quantile(x,0.10, na.rm = TRUE)
      x[x < y] <- y
    } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 4){
      a <- sd(df[,index])
      b <- (3*x) - mean(df[,index])
      x <- df[,index]
      x[x < b] <- b
    } else if(isolate(input$cleanType) == 2 & isolate(input$outliertype) == 2 & isolate(input$loweroutlier) == 5){
      a <- sd(df[,index])
      b <- (3*x) + mean(df[,index])
      x <- df[,index]
      y <- quantile(x,0.25, na.rm = TRUE) - (IQR(x, na.rm = TRUE) * 1.5)
      x[x < y] <- y
    }
    
    df[,index] <- x
    
    #write the data into a new file
    write.csv(df, "input_file.csv")
    print("done")
  })
  
  # SPINAL CORD OF THE WHOLE SYSTEM- MODEL BUILDING
  output$model <- renderPrint(if(input$Go){
    
    print("model")
    if(input$trainFile_Choose == 1){
      df <- loadfile1()
    } else if(input$trainFile_Choose == 2){
      df <- read.csv("input_file.csv", header = TRUE, stringsAsFactors = TRUE)
    } else if(input$trainFile_Choose == 3){
      df <- loadfile7()
    } else if(input$trainFile_Choose == 4){
      df <- read.csv("transformed_dataset.csv", header = TRUE, stringsAsFactors = TRUE)
    }
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing.......")
    
    index <- which(names(df) %in% isolate(input$Target))
    Metric <- isolate(input$MetricType)
    
    if(input$cc==1){
      df[,index] = as.factor(df[,index])
    }
    
    if (input$NumPredictor==1){
      Formula<-as.formula(paste(colnames(df)[index], paste(colnames(df)[-c(index)], sep = "", 
                                                           collapse = " + "), sep = " ~ "))
    }    else{
      Formula<-as.formula(paste(colnames(df)[index], paste(isolate(input$Predictor), sep = "", 
                                                           collapse = " + "), sep = " ~ "))
    }
    
    formula_check <- Formula
    set.seed(12345)
    df1 = sample(seq_len(nrow(df)), size = 0.7 * (nrow(df)))
    training_set <- df[df1,]
    testing_set <- df[-df1,]
    
    #training_set <- subset(df, df$flag == TRUE)
    #testing_set <- subset(df, df$flag == FALSE)
    #train_set <- training_set
    #test_set <- testing_set
    x <- ifelse(nrow(df)>5000,3,5)
    tc11 <- trainControl(method = "cv", number = x)
    varchar <- df[,index]
    
    if(isolate(input$tuning) == 1){
      if(isolate(input$MLT)==1){
        fit.lm <<- train(Formula, data = df, method = "lm", metric = Metric, 
                         preProcess = c("center","scale"), trControl = tc11)
        
        results <- resamples(list(LinearRegression=fit.lm, LinearRegression=fit.lm))
        if(input$tot == 1){
          print(fit.lm)
          xyz <- summary(results)
        }else if(input$tot == 2){
          xy <- varImp(fit.lm)
          xy1 <- xy$importance
          xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          xyz <- xy2[1:5, , drop = FALSE]
        }
        xyz
        #      print(varImp(fit.lm))
        #      results
      } else if(isolate(input$MLT)==2){
        if (input$NumPredictor==1){
          Formula1<-as.formula(paste(as.factor(colnames(df)[index]), paste(colnames(df)[-c(index)], sep = "", 
                                                               collapse = " + "), sep = " ~ "))
        }    else{
          Formula1<-as.formula(paste(as.factor(colnames(df)[index]), paste(isolate(input$Predictor), sep = "", 
                                                               collapse = " + "), sep = " ~ "))
        }
        
        fit.glm <<- train(Formula1, data = df, method = "glm", 
                          preProcess = c("center","scale"), trControl = trainControl(method = "cv", number = x))
        # , classProbs = TRUE
        results <- resamples(list(LogisticRegression=fit.glm, LogisticRegression=fit.glm))
        df$pred <- predict(fit.glm, df)
        print(fit.glm)
        #        library(InformationValue)
        #        y <- optimalCutoff(df$pred)
        if(input$tot == 1){
          xyz <- table(df[,index], df$pred)
          
        }else if(input$tot == 2){
          # xy <- varImp(fit.glm, scale = FALSE)
          # xy1 <- xy$importance
          # xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          # xyz <- xy2[1:5, , drop = FALSE]
          xyz <- varImp(fit.glm)
        }
        
        xyz
        #      print(fit.rf)
        #     print(varImp(fit.rf))
        #      results
      } else if(isolate(input$MLT)==3){
        fit.rf <<- train(Formula, data = training_set, method = "rf", metric = Metric,
                         preProcess = c("center","scale"), importance = T, trControl = tc11)
        
        results <- resamples(list(RandomForest=fit.rf, RandomForest=fit.rf))
        testing_set$pred <- predict(fit.rf, testing_set)
        #       fit.rf       
        if(input$tot == 1){
          if(is.factor(df[,index])){
            xyz <- confusionMatrix(testing_set[,index],testing_set$pred)
          }else {xyz <- fit.rf}
          
        }else if(input$tot == 2){
          # xy <- varImp(fit.rf,scale = FALSE)
          # xy1 <- xy$importance
          # xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          xyz <- varImp(fit.rf)
          # xy <- xy$Importance
          # xyz <- xy1
        }
        
        
        #      print(fit.gbm)
        
        (xyz)
        
      } else if(isolate(input$MLT)==4){
        fit.gbm <<- train(Formula, data = df, method = "gbm", metric = Metric,
                          preProcess = c("center","scale"), trControl = tc11)
        df$pred <- predict(fit.gbm,df)
        results <- resamples(list(GradientBoost=fit.gbm, GradientBoost=fit.gbm))
        
        if(input$tot == 1){
          if(is.factor(df[,index])){
            xyz <- confusionMatrix(df[,index],df$pred)
          }else {xyz <- fit.gbm}
          
        }else if(input$tot == 2){
          xy <- varImp(fit.gbm,scale = FALSE)
          xy1 <- xy$importance
          xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          xyz <- xy2[1:5, , drop = FALSE]
        }
        
        
        #      print(fit.gbm)
        
        (xyz)
        #summary(results)
      } 
      else if(isolate(input$MLT)==5){
        fit.dt <<- train(Formula, data = df, method = "rpart", metric = Metric,
                         preProcess = c("center","scale"), trControl = tc11)
        df$pred <- predict(fit.dt, df)
        results <- resamples(list(DecisionTree=fit.dt, DecisionTree=fit.dt))
        
        if(input$tot == 1){
          if(is.factor(df[,index])){
            xyz <- confusionMatrix(df[,index], df$pred)
          }else {xyz <- fit.dt}
          
        }else if(input$tot == 2){
          xy <- varImp(fit.dt,scale = FALSE)
          xy1 <- xy$importance
          xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          xyz <- xy2[1:5, , drop = FALSE]
        }
        
        #xyz
        #      print(fit.dt)
        print(xyz)
        summary(results)
      }     else if(isolate(input$MLT)==6){
        fit.svm <<- train(Formula, data = df, method = "svmRadial", metric = Metric,
                          preProcess = c("center","scale"), trControl = tc11)
        df$pred <- predict(fit.svm, df)
        results <- resamples(list(SupportVectorMachine=fit.svm, SupportVectorMachine=fit.svm))
        
        
        if(input$tot == 1){
          if(is.factor(df[,index])){
            xyz <- confusionMatrix(df[,index], df$pred)
          }else {xyz <- fit.svm}
          
        }else if(input$tot == 2){
          # xy <- varImp(fit.svm,scale = FALSE)
          # xy1 <- xy$importance
          # xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          xyz <- varImp(fit.svm, scale = FALSE)
          # xy2[1:5, , drop = FALSE]
        }
        
        
        
        #xyz
        #      print(fit.svm)
        print(xyz)
        summary(results)
      }
    }    else {
      if(isolate(input$MLT)==1){
        fit.lm <<- train(Formula, data = df, method = "lm", metric = Metric,
                         preProcess = c("center","scale"), trControl = tc11)
        
        results <- resamples(list(LinearRegression=fit.lm, LinearRegression=fit.lm))
        
        if(input$tot == 1){
          xyz <- print(fit.lm)
          
        }else if(input$tot == 2){
          xy <- varImp(fit.svm,scale = FALSE)
          xy1 <- xy$importance
          xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          xyz <- xy2[1:5, , drop = FALSE]
        }
        
        xyz
        summary(results)
        
        #      print(varImp(fit.lm))
        #      results
      } else if(isolate(input$MLT)==2){
        fit.glm <<- train(Formula, data = df, method = "glm", metric = "ROC",
                          preProcess = c("center","scale"), trControl = trainControl(method = "cv", number = x, classProbs = TRUE))
        
        results <- resamples(list(LogisticRegression=fit.glm, LogisticRegression=fit.glm))
        df$pred <- predict(fit.glm, df)
        if(input$tot == 1){
          xyz <- print(fit.glm)
          confusionMatrix(df[,index], df$pred)
        }else if(input$tot == 2){
          xy <- varImp(fit.glm,scale = FALSE)
          xy1 <- xy$importance
          xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          xyz <- xy2[1:5, , drop = FALSE]
        }
        
        xyz
        # confusionMatrix(df[,index], df$pred)
        
        #      print(fit.rf)
        #     print(varImp(fit.rf))
        #      results
      }else if(isolate(input$MLT) == 3){
        rfgrid <<- expand.grid(.mtry = c(as.numeric(input$mty)))
        nt <- as.numeric(input$ntree)
        fit.rf <<- train(Formula, data = training_set, method = "rf", metric = Metric,
                         preProcess = c("center","scale"), importance = T, trControl = tc11, tuneGrid = rfgrid, ntree=nt)
        testing_set$pred <- predict(fit.rf, testing_set)
        results <- resamples(list(RandomForest=fit.rf, RandomForest=fit.rf))
        #       fit.rf       
        
        
        if(input$tot == 1){
          if(is.factor(df[,index])){
            xyz <- confusionMatrix(testing_set[,index], testing_set$pred)
          } else {xyz <- fit.rf}
        }else if(input$tot == 2){
          # xy <- varImp(fit.rf,scale = FALSE)
          # xy1 <- xy$importance
          # xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          # xyz <- xy2[1:5, , drop = FALSE]
          xyz <- varImp(fit.rf, scale = FALSE)
        }
        
        
        
        #xyz
        #       print(fit.rf)
        print(xyz)
        summary(results)
      } else if(isolate(input$MLT) == 4){
        
        
        #       gbmgrid <- expand.grid(interaction.depth = c(1,5,9), n.trees = (1:15)*100, shrinkage = 0.1,
        #                              n.minobsinnode = 20)
        
        gbmgrid <- expand.grid(interaction.depth = as.numeric(input$intDepth), n.trees = as.numeric(input$NTReees),
                               shrinkage = as.numeric(input$shrink), n.minobsinnode = as.numeric(input$minObs))
        #       gbmgrid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),
        #                              n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))
        fit.gbm <<- train(Formula, data = df, method = "gbm", metric = Metric,
                          preProcess = c("center","scale"), trControl = tc11, tuneGrid = gbmgrid)
        df$pred <- predict(fit.gbm, df)
        results <- resamples(list(GradientBoost=fit.gbm, GradientBoost=fit.gbm))
        
        if(input$tot == 1){
          if(is.factor(df[,index])){
            xyz <- confusionMatrix(df[,index], df$pred)
          } else {xyz <- fit.gbm}
        }else if(input$tot == 2){
          xy <- varImp(fit.gbm,scale = FALSE)
          xy1 <- xy$importance
          xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          xyz <- xy2[1:5, , drop = FALSE]
        }
        
        #       print(fit.gbm)
        print(xyz)
        summary(results)       
      } else if(isolate(input$MLT) == 6){
        svmgrid <<- expand.grid(sigma = as.numeric(input$gama), C = as.numeric(input$ccc))
        
        fit.svm <<- train(Formula, data = df, method = "svmRadial", metric = Metric,
                          preProcess = c("center"), trControl = tc11, tuneGrid = svmgrid)
        df$pred <- predict(fit.svm, df) 
        results <- resamples(list(SupportVectorMachine=fit.svm, SupportVectorMachine=fit.svm))
        #print(fit.svm)
        
        #      x <- confusionMatrix(fit.svm)
        #      y <- varImp(fit.svm)
        #      print(x,y )
        #      confusionMatrix(fit.svm)
        #      varImp(fit.svm)
        #      confusionMatrix(fit.svm)
        #      fit.svm
        
        if(input$tot == 1){
          if(is.factor(df[,index])){
            xyz <- confusionMatrix(df[,index], df$pred)
          } else {xyz <- fit.svm}
        }else if(input$tot == 2){
          # xy <- varImp(fit.svm,scale = FALSE)
          # xy1 <- xy$importance
          # xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          xyz <- varImp(fit.svm, scale = FALSE)
        }
        
        #xyz
        #      print(fit.svm)
        print(xyz)
        # summary(results)
      } else if(isolate(input$MLT) == 5){
        dtgrid <<- expand.grid(cp = input$c_p)
        
        fit.dt <<- train(Formula, data = df, method = "rpart", metric = Metric,
                         preProcess = c("center","scale"), trControl = tc11, tuneGrid = dtgrid)
        
        df$pred <- predict(fit.dt, df)
        results <- resamples(list(DecisionTree=fit.dt, DecisionTree=fit.dt))
        #print(fit.svm)
        
        #      x <- confusionMatrix(fit.svm)
        #      y <- varImp(fit.svm)
        #      print(x,y )
        #       confusionMatrix(fit.dt)
        #       varImp(fit.dt)
        #       fit.dt
        
        if(input$tot == 1){
          if(is.factor(df[,index])){
            xyz <- confusionMatrix(df[,index], df$pred)
          } else {xyz <- fit.dt}
        }else if(input$tot == 2){
          xy <- varImp(fit.dt,scale = FALSE)
          xy1 <- xy$importance
          xy2 <- xy1[order(xy1$Overall, decreasing = TRUE), , drop = FALSE]
          xyz <- xy2[1:5, , drop = FALSE]
        }
        
        #       print(fit.dt)
        print(xyz)
        summary(results)
      }
      
    }
  }
  )
  
  # PREDICTION TAB
  output$predict <- renderDataTable(if(input$Go2){
    
    df <- loadfile2()
    index <- which(names(df) %in% input$Target)
    Metric <- input$MetricType
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing.......")
    
    #  fit.rf <- train(Formula, data = test_set, method = "rf", metric = Metric,
    #                  preProcess = c("center","scale"), importance = T)
    if(isolate(input$MLT)==1){
      df$predicted <- predict(fit.lm, df)
      df
    }  else if(isolate(input$MLT)==2){
      df$predicted <- predict(fit.glm, df)
      df
    } else if(isolate(input$MLT)==3){
      df$predicted <- predict(fit.rf, df)
      df
    } else if(isolate(input$MLT)==4){
      df$predicted <- predict(fit.gbm, df)
      df
    } else if(isolate(input$MLT)==5){
      df$predicted <- predict(fit.dt, df)
      df
    } else if(isolate(input$MLT)==6){
      df$predicted <- predict(fit.svm, df)
      df
    }
    
    write.csv(df,"predicted.csv", row.names = FALSE)
    df
    
    
  })
  
  # CHOOSING THE FILE ON WHICH SEGMENTATION HAS TO BE RUN
  output$fileEntry <- renderUI({
    selectInput(inputId = "entry", label = "Do you want to use predicted dataset or want to upload a new one?",
                choices = list("Predicted" = 1, "New Dataset"=2), selected = 1, multiple = FALSE)
  })
  
  #load predicted data file
  loadfile4 <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    temp = read.csv("predicted.csv", header = TRUE, stringsAsFactors = TRUE)
  })
  
  #for segmentation tab
  loadfile5 <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    S <- input$file5
    
    if(is.null(S))
      return(NULL)
    
    temp = read.csv(S$datapath, header = TRUE, stringsAsFactors = TRUE)
  })
  
  #cacll for uplodaing the dataset
  output$FileUpload5 <- renderUI(if(input$entry == 2){
    fileInput('file5', 'Upload DatASET', 
              accept = c('text/csv','text/comma-separated-values,text/plain', 
                         '.csv'))
  })
  
  
  #upload new file if new file option is chosen  
  output$entryinput <- renderUI(if(input$entry == 2){
    df <- FileUpload5()
  })
  
  
  #option between hierarchial and non-hierarchial clustering
  output$hierarchy_nonh <- renderUI({
    selectInput(inputId = "ishcy", label = "Do you want to run hierarchial clustering or non-hierarchial clustering?",
                choices = list("Yes" = 1, "No"=2), selected = 2, multiple = FALSE)
  })
  
  #option betweeen hierARCHIAL AND NON-HIERARCHIAL CLUSTERING
  output$hier_met <- renderUI(if(input$ishcy == 1){
    selectInput(inputId = "hier_method", label = "Which one of the following hierarchial algorithms do you
                want to run?", choices = list("Agglomerative" = 1, "Divisive" = 2), selected = 1, multiple = FALSE)
  })
  
  #OPTION BETWEEN THE DISTANCE MEASURES FOR CREATING THE DISTANCE MATRIX
  output$hier_dist_met <- renderUI(if(input$ishcy == 1){
    if( input$hier_method == 1){
      selectInput(inputId = "hier_distn_met", label = "Choose the method for computing the distances",
                  choices = list("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                  selected = "euclidean")
    } else {
      selectInput(inputId = "hier_distn_met", label = "Choose the method for computing the distances",
                  choices = list("euclidean", "manhattan"),
                  selected = "euclidean")
    }
  })
  
  output$hier_clust_method <- renderUI(if(input$ishcy == 1 & input$hier_method == 1){
    selectInput(inputId = "hchy_clust_method", label = "Choose the clustering method",
                choices = list("average", "single", "complete", "ward"), selected = "average", multiple = FALSE)
  })
  
  # CHOOSING THE VARIABLES FOR HIERARCHIAL CLUSTERING
  output$varchooseseg_hcy <- renderUI({
    if((input$entry) == 1){
      df <- loadfile4()
    }else{
      df <- loadfile5()
    }
    
    #choose the variables
    if((input$ishcy) == 1){
      nums <- unlist(lapply(df, is.numeric))  
      df1 <- df[,nums]
      x <- names(df1)
      selectInput(inputId = "segvar_hcy", label = "Choose the variables on which segmentation needs to be run","",
                  selectize = TRUE, multiple = TRUE, choices = x)
    }
    
  })
  
  #choose the clustering algorithm
  output$segAlgo1 <- renderUI(if(input$ishcy == 2){
    if(input$entry == 1){
      df <- loadfile4()
    }else{
      df <- loadfile5()
    }
    
    selectInput(inputId = "SegmentationAlgo1", label = "Choose the segmentation algorithm",
                choices = list("K-Means" = 1, "K-Proto" = 3, "Fuzzy Clustering" = 4), selected = 1)
    
  })
  
  
  # CHOOSING THE VARIABLES FOR NON-HIERARCHIAL CLUSTERING KMODES
  output$varchoosekprotopl <- renderUI(if(input$ishcy == 2 & input$SegmentationAlgo1 == 3){
    if((input$entry) == 1){
      df <- loadfile4()
    }else{
      df <- loadfile5()
    }
    
    #choose the variables
    if((input$SegmentationAlgo1) == 3){
      x <- names(df)
      selectInput(inputId = "kprotvar", label = "Choose the variables on which you want to see silhouette plot","",
                  selectize = TRUE, multiple = TRUE, choices = x)
    }

    
  })
  
  
  # CHOOSING THE VARIABLES FOR NON-HIERARCHIAL CLUSTERING KMEANS AND FUZZY
  output$varchoosekmeanspl <- renderUI(if(input$ishcy == 2){
    if((input$entry) == 1){
      df <- loadfile4()
    }else{
      df <- loadfile5()
    }
    
    #choose the variables
    if((input$SegmentationAlgo1) == 1 | input$SegmentationAlgo1 == 4){
      nums <- unlist(lapply(df, is.numeric))  
      df1 <- df[,nums]
      x <- names(df1)
      selectInput(inputId = "kmnvar", label = "Choose the variables on which segmentation needs to be run","",
                  selectize = TRUE, multiple = TRUE, choices = x)
    }
    # else{
    #   x <- names(df)
    #   selectInput(inputId = "segvar", label = "Choose the variables on which segmentation needs to be run","",
    #               selectize = TRUE, multiple = TRUE, choices = x)
    #   
    # }
    
  })
  
  
  
  #plotting the dendogram
  output$dend <- renderPlot(if(input$Plot_dend){
    if(input$entry == 1){
      df <- loadfile4()
    }else{
      df <- loadfile5()
    }
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing.......")
    
    if(input$ishcy == 1){
      x <- isolate(input$segvar_hcy)
      df1 <- df[x]
      
      d <- scale(df1)
      
      if(input$hier_method == 1){
        
        y <- isolate(input$hchy_clust_method)
        d1 <- dist(d, method = isolate(input$hier_distn_met))
        hc1 <- hclust(d1, method = y)
        
      } else if(isolate(input$hier_method) == 2) {
        y <- isolate(input$hier_distn_met)
        hc1 <- diana(d, metric = y)
      }
      
      plot(hc1, cex = 0.6)
      
    } else if(input$ishcy == 2){
      if(input$SegmentationAlgo1 == 1 |input$SegmentationAlgo1 == 4  ){
        x <- isolate(input$kmnvar)
        df_new <- df[x]
        d_scaled <- scale(df_new)
        set.seed(12345)
        

        fviz_nbclust(d_scaled, kmeans, method = "wss")
      } else if(input$SegmentationAlgo1 == 3){
        x <- isolate(input$kprotvar)
        df_new <- df[x]
        kmax <- 15
        df_new_omit <- na.omit(df_new)
        wss <- sapply(1:kmax, function(k){kproto(df_new_omit, k)$tot.withinss})
        plot(1:kmax, wss,
             type="b", pch = 19, frame = FALSE)
      }
    } 
  })
  
  # CHOOSE THE POINT AT WHICH THE CLUSTERS HAVE TO BE CREATED
  output$treecut <- renderUI(if(input$ishcy == 1 & input$Plot_dend){
    numericInput(inputId = "cut", label = "Enter the value where the clusters need to be split",
                 min = 2, max = 100, value = 3)
  })
  
  # GET INPUT TO SEE TYPE OF OUTPUT OF HIERARCHIAL CLUSTERING
  output$op_tp_hc <- renderUI(if(input$ishcy == 1 & input$Plot_dend){
    selectInput(inputId = "ot_t", label = "What is output you intend to see?", 
                choices = list("Cluster count" = 1, "Data frame" = 2), selected = 2)
  })
  #CREATING THE CLUSTERS AND SAVING IT IN A CSV FILE
  output$create_hier_cluster <- renderDataTable(if(input$Segment){
    if(input$entry == 1){
      df <- loadfile4()
    }else{
      df <- loadfile5()
    }
    
    x <- isolate(input$segvar_hcy)
    df1 <- df[x]
    
    d <- scale(df1)
    
    if(input$hier_method == 1){
      
      y <- isolate(input$hchy_clust_method)
      d1 <- dist(d, method = isolate(input$hier_distn_met))
      hc1 <- hclust(d1, method = y)
      
    } else if(isolate(input$hier_method) == 2) {
      y <- isolate(input$hier_distn_met)
      hc1 <- diana(d, metric = y)
    }
    
    if(isolate(input$hier_method == 1)){
      sb <- cutree(hc1, isolate(input$cut))
      df <- cbind(df, sb)
    } else if(isolate(input$hier_method == 2)){
      sb <- cutree(hc1, isolate(input$cut))
      df <- cbind(df,sb)
    }
    
    if(isolate(input$ot_t == 1)){
      xb <- table(df$sb)
      xb <- as.data.frame(xb)
    } else{xb <- df}
    write.csv(df, "hierarchial_clustering.csv", row.names = FALSE)
    xb
  })
  
  
  #choose the clustering algorithm
  output$segAlgo <- renderUI(if(input$ishcy == 2){
    if(input$entry == 1){
      df <- loadfile4()
    }else{
      df <- loadfile5()
    }
    
    selectInput(inputId = "SegmentationAlgo", label = "Choose the segmentation algorithm",
                choices = list("K-Means" = 1, "K-Modes" = 2, "K-Proto" = 3, "Fuzzy Clustering" = 4), selected = 1)
    
  })
  
  # CHOOSING THE VARIABLES FOR NON-HIERARCHIAL CLUSTERING
  output$varchooseseg <- renderUI(if(input$ishcy == 2){
    if((input$entry) == 1){
      df <- loadfile4()
    }else{
      df <- loadfile5()
    }
    
    #choose the variables
    if((input$SegmentationAlgo) == 1 | input$SegmentationAlgo ==4){
      nums <- unlist(lapply(df, is.numeric))  
      df1 <- df[,nums]
      x <- names(df1)
      selectInput(inputId = "segvar", label = "Choose the variables on which segmentation needs to be run","",
                  selectize = TRUE, multiple = TRUE, choices = x)
    }else{
      x <- names(df)
      selectInput(inputId = "segvar", label = "Choose the variables on which segmentation needs to be run","",
                  selectize = TRUE, multiple = TRUE, choices = x)
      
    }
    
  })
  
  #do you need to scale the data
  output$isscale <- renderUI(if(input$ishcy == 2 & ((input$SegmentationAlgo) == 1 | (input$SegmentationAlgo) == 2
                                                    | (input$SegmentationAlgo) == 4)){
    selectInput(inputId = "scaleup", label = "Do you want to scale the data?", choices = list("Yes" = 1, "No"=2),
                multiple = FALSE, selected = 1)
  })
  
  #enter nstart and k manually
  output$isnstartk <- renderUI(if((input$ishcy == 2) &(input$SegmentationAlgo) == 1){
    selectInput(inputId = "isnk", label = "Do you want to enter the values manually for nstart and number of clusters?",
                choices = list("Yes" = 1, "No" = 2), selected = 2)
  })
  
  
  #enter value for nstart
  output$nstrt <- renderUI(if((input$ishcy == 2) & (input$SegmentationAlgo) == 1 & (input$isnk) == 1){
    numericInput(inputId = "nstart", label = "Enter the value of nstart", value = 25)  
  })
  
  
  #enter value for k
  output$kclus <- renderUI(if((input$ishcy == 2) & (input$SegmentationAlgo) == 1 & (input$isnk) == 1){
    numericInput(inputId = "kclusters", label = "Enter the number of clusters", value = 3,
                 min = 2)  
  })
  
  #enter value of k for kmodes
  output$kmod <- renderUI(if((input$ishcy == 2) & (input$SegmentationAlgo) == 2){
    numericInput(inputId = "kmodess", label = "Enter the number of clusters", value = 2, min = 2)
  })
  
  #choose between entering lambda for kproto or use optimal one
  output$lamb <- renderUI(if((input$ishcy == 2) & (input$ishcy == 2) & (input$SegmentationAlgo) == 3){
    selectInput("yeslamb", label = "Do you want to enter the value for lambda or use optimal one?", 
                choices = list("Yes" = 1, "No" = 2), selected = 2)
  })
  
  #enter the number of clusters for kproto
  output$kprot <- renderUI(if((input$ishcy == 2) & (input$SegmentationAlgo) == 3){
    numericInput(inputId = "kpro", label = "Enter the number of clusters", value = 3, min = 1)
  })
  
  #enter the value for lambda if lamb is true
  output$lambyes <- renderUI(if((input$ishcy == 2) & (input$SegmentationAlgo) == 3 & (input$yeslamb) == 1){
    numericInput(inputId = "lambda", label = "Enter the value for lambda", value = 1,min = 1)
  }) 
  
  output$kfanny <- renderUI(if((input$ishcy == 2) & input$SegmentationAlgo == 4){
    numericInput(inputId = "kfuzz", label = "Enter the number of clusters", min = 2, max = 100, value = 3)
  })
  
  output$opsegm <- renderUI(if(input$ishcy == 2){
    selectInput(inputId = "optypeseg", label = "What is the output type do you want to be displayed?",
                choices = list("Cluster division" = 1, "Data frame clustered" = 2),#, "Profiling of the segments" = 3),
                selected = 2)
  })
  
  output$distFanny <- renderUI(if((input$ishcy == 2) & input$SegmentationAlgo == 4){
    selectInput(inputId = "distfuzz", label = "Choose the metric for running fanny",
                choices = list("euclidean", "manhattan", "SqEuclidean"), multiple = FALSE)
  })
  
  #running segmentation function
  output$segmentationrun <- renderDataTable(if((input$ishcy == 2) & input$Segment){
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing.......")
    
    if(input$entry == 1){
      df <- loadfile4()
    }else{
      df <- loadfile5()
    }
    
    
    
    #k-means algo
    if(isolate(input$SegmentationAlgo) == 1){
      
      x <- c(isolate(input$segvar))#paste(quote(isolate(input$segvar)), sep = "", collapse = ",")
      df_new <- df[x]
      
      if(isolate(input$scaleup) == 1){
        df_scaled <- as.matrix(scale(df_new))
      } else {
        df_scaled <- as.matrix(df_new)
      }
      
      #      x <- c(isolate(input$segvar))#paste(quote(isolate(input$segvar)), sep = "", collapse = ",")
      df_scaled #<- df[x]
      #      df_new <- df[x]
      
      if(input$isnk == 1){
        k <- isolate(input$kclusters)
        nst <- isolate(input$nstart)
      }else{
        d_clust <- Mclust(as.matrix(df_scaled), G=1:15,
                          modelNames = mclust.options("emModelNames"))
        
        k <- d_clust$G
        nst <- 25
      }
      
      kmm <- kmeans(df_scaled,k,nstart = nst)
      
      df <- cbind(df, cluster_n <- kmm$cluster)
      df$cluster_n <- as.factor(df$cluster_n)
      
      require(tables)
      #      profiling <- tabular(1 + paste(isolate(input$segvar), sep = "",collapse = "+") ~ mean + mean * cluster_n, data = df)
      #     profile_1 <- as.matrix(profiling)
      #      profile_1 <- as.data.frame(profile_1)
    } else if(isolate(input$SegmentationAlgo) == 2){
      k <- isolate(input$kmodess)
      
      x <- c(isolate(input$segvar))#paste(quote(isolate(input$segvar)), sep = "", collapse = ",")
      df_new <- df[x]
      
      num_var <- (sapply(df_new, is.numeric))
      char_var <- (!sapply(df_new, is.numeric))
      
      dfx <- df_new[num_var]
      dfd <- df_new[char_var]
      
      if(isolate(input$scaleup) == 1){
        df_scaled <- as.matrix(scale(dfx))
        dfs <- as.matrix(dfd)
      } else {
        df_scaled <- as.matrix(dfx)
        dfs <- as.matrix(dfd)
      }
      
      dffd <- cbind(df_scaled, dfs)
      
      kmo <- kmodes(dffd, k)
      df <- cbind(df, cluster_n <- kmo$cluster)
      df$cluster_n <- as.factor(df$cluster_n)
    } else if(isolate(input$SegmentationAlgo) == 3){
      x <- c(isolate(input$segvar))#paste(quote(isolate(input$segvar)), sep = "", collapse = ",")
      df_new <- df[x]
      k <- isolate(input$kpro)
      if(isolate(input$yeslamb) == 1){
        a <- isolate(input$lambda)
      }else{
        a <- lambdaest(df)
      }
      
      kp <- kproto(df_new, k, lambda = a)
      df <- cbind(df, cluster_n <- kp$cluster)
      df$cluster_n <- as.factor(df$cluster_n)
    } else if(input$SegmentationAlgo == 4){
      k <- isolate(input$kfuzz)
      x <- isolate(input$segvar)
      df_new <- df[x]
      met <- isolate(input$distfuzz)
      if(isolate(input$scaleup) == 1){
        df_scaled <- as.matrix(scale(df_new))
      } else {
        df_scaled <- as.matrix(df_new)
      }
      
      kfuz <- fanny(df_scaled, k, metric = met)
      df <- cbind(df, cluster_n <- kfuz$clustering )
      df<- cbind(df, kfuz$membership)
      df$cluster_n <- as.factor(df$cluster_n)
    }
    
    if(isolate(input$optypeseg) == 1){
      xb <- table(df$cluster_n)
      xb <- as.data.frame(xb)
    } else if(isolate(input$optypeseg) == 2){
      xb <- df
    } 
    
    df
    #   profile_1
    print(k)
    
    write.csv(df,"clusters_created.csv", row.names = FALSE)
    xb
    
  })
  
  # options for which while in which you want to transform the column
  output$choose_file_trans <- renderUI({
    selectInput(inputId = "choose_file_seg", label = "Choose the file from which you want to transform the data",
                choices = list("File from 'Input' tab" = 1, "File from data cleaning tab" = 2, "New File" = 3,
                               "Previously Transformed file" = 4),
                selected = 1, multiple = FALSE)
  })
  
  
  
  #for Variable transformation tab
  loadfile6 <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    S <- input$file6
    
    if(is.null(S))
      return(NULL)
    
    temp = read.csv(S$datapath, header = TRUE, stringsAsFactors = TRUE)
  })
  
  #cacll for uplodaing the dataset
  output$FileUpload6 <- renderUI(if(input$choose_file_seg == 3){
    fileInput('file6', 'Upload DatASET', 
              accept = c('text/csv','text/comma-separated-values,text/plain', 
                         '.csv'))
  })
  
  output$choose_var_trans <- renderUI({
    if(input$choose_file_seg == 1){
      df <- loadfile1()
    } else if(input$choose_file_seg == 2){
      df <- read.csv("input_file.csv")
    } else if(input$choose_file_seg == 3){
      df <- loadfile6()
    } else if(input$choose_file_seg == 4){
      df <- read.csv("transformed_dataset.csv")
    }
    
    y <- sapply(df, is.numeric)
    df_new <- df[y]
    x <- names(df_new)
    selectInput(inputId = "var_trans", label = "Choose the variable you want to transform",
                choices = x, multiple = TRUE)
  })
  
  
  output$choose_trans_method <- renderUI({
    selectInput(inputId = "trans_method", label = "Choose the transformation method",
                choices = list("Log transformation" = 1, "Square-root transformation" = 2, "Z-Score transformation" = 3,
                               "Exponential" = 4, "Squared" = 5),
                selected = 1, multiple = TRUE)
  })
  
  
  ct1 <- reactiveValues(counter = 0)
  
  
  output$trans2 <- renderPrint(if(input$trans){
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing.......")
    
    
    #    ct1$counter <- ct1$counter + 1
    if(isolate(input$choose_file_seg) == 1){
      df <- loadfile1()
    } else if(isolate(input$choose_file_seg) == 2){
      df <- read.csv("input_file.csv")
    } else if(isolate(input$choose_file_seg) == 3){
      df <- loadfile6()
    }else if(isolate(input$choose_file_seg) == 4){
      df <- read.csv("transformed_dataset.csv")
    }
    
    #    if(ct1$counter == 1){
    #      write.csv(df, "transformed_dataset.csv", row.names = FALSE)
    #    }else{
    #      df <- read.csv("transformed_dataset.csv")
    #    }
    
    # obtain the length and the elements of the variables to be transformed 
    vart <- c()
    y1 <- c()
    x <- length(isolate(input$var_trans))
    for (i in 1:x) {
      y <- isolate(input$var_trans)
      vart[i] <- y
    }
    
    for (i in 1:x) {
      y <- isolate(input$var_trans)
      y1[i] <- y
    }
    
    
    k <- ncol(df) + 1
    # obtain the number of transformation methods and their names
    cm <- c()
    x <- length(isolate(input$trans_method))
    for (i in 1:x) {
      y <- isolate(input$trans_method[i])
      cm[i] <- y
    }
    x1 <- length(vart)
    if(length(vart) == 1){
      if(length(cm) > 1){
        for (i in 1:length(cm)) {
          if(cm[i] == "1"){
            index <- which(names(df) %in% vart[1])
            df[,k] <- log(df[,index])
            names(df)[k] <- paste(names(df)[index],"log",sep = "_")
            k <- k + 1
          } else if(cm[i] == "2"){
            index <- which(names(df) %in% vart[1])
            df[,k] <- sqrt(df[,index])
            names(df)[k] <- paste(names(df)[index],"sqrt",sep = "_")
            k <- k + 1
          } else if(cm[i] == "3"){
            index <- which(names(df) %in% vart[1])
            x <- df[,index]
            y <- sd(x)*sqrt((length(x)-1)/(length(x)))
            y1 <- mean(x)
            z <- (x - y1)/y
            df[,k] <- z
            names(df)[k] <- paste(names(df)[index], "zScore", sep = "_")
            k <- k + 1
          } else if(cm[i] == "4"){
            index <- which(names(df) %in% vart[1])
            df[,k] <- exp(df[,index])
            names(df)[k] <- paste(names(df)[index], "exponential", sep = "_")
            k <- k + 1
          }else if(cm[i] == "5"){
            index <- which(names(df) %in% vart[1])
            x <- df[,index]
            df[,k] <- df[,index] * df[,index]
            names(df)[k] <- paste(names(df)[index], "squared", sep = "_")
            k <- k + 1
          }
        }
      } else {
        if(cm[1] == "1"){
          index <- which(names(df) %in% vart[1])
          df[,k] <- log(df[,index])
          names(df)[k] <- paste(names(df)[index],"log",sep = "_")
          k <- k + 1
        } else if(cm[1] == "2"){
          index <- which(names(df) %in% vart[1])
          df[,k] <- sqrt(df[,index])
          names(df)[k] <- paste(names(df)[index],"sqrt",sep = "_")
          k <- k + 1
        } else if(cm[1] == "3"){
          index <- which(names(df) %in% vart[1])
          x <- df[,index]
          y <- sd(x)*sqrt((length(x)-1)/(length(x)))
          y1 <- mean(x)
          z <- (x - y1)/y
          df[,k] <- z
          names(df)[k] <- paste(names(df)[index], "zScore", sep = "_")
          k <- k + 1
        } else if(cm[1] == "4"){
          index <- which(names(df) %in% vart[1])
          df[,k] <- exp(df[,index])
          names(df)[k] <- paste(names(df)[index], "exponential", sep = "_")
          k <- k + 1
        }else if(cm[1] == "5"){
          index <- which(names(df) %in% vart[1])
          x <- df[,index]
          df[,k] <- df[,index] * df[,index]
          names(df)[k] <- paste(names(df)[index], "squared", sep = "_")
          k <- k + 1
        }
      }
    } else {
      
      for (i in 1:length(y1)) {
        # for (i in 1:length(y1)) {
        #   index <- which(names(df) %in% y1[i])
        #   df[,k] <- log(df[,index])
        #   names(df)[k] <- paste(names(df)[index], "log", sep = "_")
        #   k  <- k + 1
        #   # s <- sum(df[,index])
        #   # print(s)
        # }
        # 
        if(length(cm) > 1){
          for (j in 1:length(cm)) {
            if(cm[j] == "1"){
              # index <- which(names(df) %in% y1[i])
              # df[,k] <- log(df[,index])
              # names(df)[k] <- paste(names(df)[index], "log", sep = "_")
              # k  <- k + 1
              # 
              index <- which(names(df) %in% y1[i])
              df[,k] <- log(df[,index])
              names(df)[k] <- paste(names(df)[index],"log",sep = "_")
              k <- k + 1
            } else if(cm[j] == "2"){
              index <- which(names(df) %in% y1[i])
              df[,k] <- sqrt(df[vart[i]])
              names(df)[k] <- paste(vart[i],"sqrt",sep = "_")
              k <- k + 1
            } else if(cm[j] == "3"){
              index <- which(names(df) %in% y1[i])
              x <- df[vart[i]]
              y <- sd(x)*sqrt((length(x)-1)/(length(x)))
              y1 <- mean(x)
              z <- (x - y1)/y
              df[,k] <- z
              names(df)[k] <- paste(vart[i], "zScore", sep = "_")
              k <- k + 1
            } else if(cm[j] == "4"){
              index <- which(names(df) %in% y1[i])
              df[,k] <- exp(df[vart[i]])
              names(df)[k] <- paste(vart[i], "exponential", sep = "_")
              k <- k + 1
            }else if(cm[j] == "5"){
              index <- which(names(df) %in% y1[i])
              x <- df[,index]
              df[,k] <- df[y1[i]] * df[y1[i]]
              names(df)[k] <- paste(y1[i], "squared", sep = "_")
              k <- k + 1
            }
          }
        } else {
          if(cm[1] == "1"){
            index <- which(names(df) %in% y1[i])
            df[,k] <- log(df[,index])
            names(df)[k] <- paste(y1[i],"log",sep = "_")
            k <- k + 1
          } else if(cm[1] == "2"){
            index <- which(names(df) %in% y1[i])
            df[,k] <- sqrt(df[,index])
            names(df)[k] <- paste(y1[i],"sqrt",sep = "_")
            k <- k + 1
          } else if(cm[1] == "3"){
            index <- which(names(df) %in% y1[i])
            x <- df[,index]
            y <- sd(x)*sqrt((length(x)-1)/(length(x)))
            y1 <- mean(x)
            z <- (x - y1)/y
            df[,k] <- z
            names(df)[k] <- paste(y1[i], "zScore", sep = "_")
            k <- k + 1
          } else if(cm[1] == "4"){
            index <- which(names(df) %in% y1[i])
            df[,k] <- exp(df[,index])
            names(df)[k] <- paste(y1[i], "exponential", sep = "_")
            k <- k + 1
          }else if(cm[1] == "5"){
            index <- which(names(df) %in% y1[i])
            x <- df[,index]
            df[,k] <- df[,index] * df[,index]
            names(df)[k] <- paste(y1[i], "squared", sep = "_")
            k <- k + 1
          }
        }
        
      }
    }
    
    write.csv(df, "transformed_dataset.csv", row.names = FALSE)
    print("Done!")
  })
  
  
  output$trans <- renderUI(if(input$trans){
    if(input$choose_file_seg == 1){
      df <- loadfile1()
    } else if(input$choose_file_seg == 2){
      df <- read.csv("input_file.csv")
    } else if(input$choose_file_seg == 3){
      df <- loadfile6()
    }
    
    index <- which(names(df) %in% input$var_trans)
    k <- ncol(df) + 1
    
    if(input$trans_method == 1){
      df[,k] <- log(df[,index])
      names(df)[k] <- paste(names(df)[index],"log", sep = "_")
    }else if(input$trans_method == 2){
      df[,k] <- sqrt(df[,k])
      names(df)[k] <- paste(names(df)[index],"sqrt", sep = "_")
    }else if(input$trans_method == 3){
      x <- df[,index]
      y <- sd(x)*sqrt((length(x)-1)/(length(x)))
      y1 <- mean(df[,index])
      z <- (x - y1)/y
      df[,k] <- z
      names(df)[k] <- paste(names(df)[index], "Z_score", sep = "_")
    }
    
    write.csv(df, "transformed_dataset.csv", row.names = FALSE)
    
  })
  
  
  
  }

shinyApp(ui, server)