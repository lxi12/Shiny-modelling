# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("Shiny - Modelling"),
    tabsetPanel(
      tabPanel("Summary",icon=icon("info"),     
               htmlOutput(outputId = "Summary")),
      
      tabPanel("Raw Data",icon=icon("table"),
                DT::dataTableOutput(outputId="table")),
      
      tabPanel("Mosaic Plot",icon=icon("chart-bar"),
               selectizeInput(inputId = "VariablesA", 
                              label = "Categorical Variables", 
                              choices = choicesA, multiple = TRUE, 
                              selected = choicesA_default),
               plotOutput(outputId = "Mosaic")),
      
      tabPanel("Pairs Plot",icon=icon("chart-line"),
               selectizeInput(inputId="VariablesD",
                              label="Categorical Variables",
                              choices=choicesA,multiple=TRUE,
                              selected=choicesA_default),
               selectizeInput(inputId="VariablesE",
                              label="Numeric Variables",
                              choices=choicesB,multiple=TRUE,
                              selected=choicesB_default),
               selectizeInput(inputId="VariablesF",
                              label="Colour Variable",
                              choices=choicesA,multiple=FALSE,
                              selected="HEALTHCARE_BASIS"),
               withSpinner(
                 plotOutput(outputId = "Pairs"))),
      
      tabPanel("Correlation",icon=icon("border-none"),
               checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
               selectInput(inputId = "CorrMeth", label = "Correlation method", 
                           choices = c("pearson","spearman","kendall"), selected = "pearson"),
               selectInput(inputId = "Group", label = "Grouping method", 
                           choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), 
                           selected = "OLO"),
               withSpinner(
                 plotOutput(outputId = "Corrgram"))),
      
      tabPanel("Homogeneity",icon=icon("chart-area"),
               plotOutput(outputId = "Tabplot")),
      
      tabPanel("Gaps",icon=icon("chart-line"),
               checkboxInput(inputId = "scale", label = "Scale", value = FALSE),
               checkboxInput(inputId = "centre", label="Centre", value=FALSE),
               checkboxInput(inputId = "legend", label="Show Legend", value=FALSE),
               selectizeInput(inputId="VariablesB",label="Numeric Variables",
                              choices=choicesB,multiple=TRUE,selected=choicesB_default),
               plotOutput(outputId = "Rising")),
      
      tabPanel("Matplots",icon=icon("chart-area"),
               checkboxInput(inputId = "scale_m", label = "Scale", value = FALSE),
               checkboxInput(inputId = "centre_m", label = "Centre", value = FALSE),
               checkboxInput(inputId = "legend_m", label = "Show Legend", value = FALSE),
               selectizeInput(inputId="VariablesC",label="Numeric Variables",
                              choices=choicesB,multiple=TRUE,selected=choicesB_default),
               plotOutput(outputId = "Matplot")),
      
      tabPanel("Missing",icon=icon("barcode"),
               sidebarPanel(
                 checkboxInput(inputId = "cluster", label = "Cluster Missingness", value = FALSE),
                 checkboxInput(inputId = "sort", label="Sort Variables", value=FALSE),
                 sliderInput(inputId = "VarThresh", label = "Threshold of Variable Missingness", 
                             min = 0, max =100, step = 5, value = 100, post="%"),
                 sliderInput(inputId = "ObsThresh", label = "Threshold of Observation Missingness", 
                             min = 0, max =100, step = 5, value = 100, post="%")),
               mainPanel(
                 withSpinner(
                 plotOutput(outputId = "Missing")),
                 hr(),
                 withSpinner(
                 textOutput(outputId = "text1")),
                 hr(),
                 withSpinner(
                 textOutput(outputId="text2")),
                 hr(),
                 withSpinner(
                 plotOutput(outputId = "Misscor")),
                 hr(),
                 withSpinner(
                 plotOutput(outputId="Pattern")))),
      
      tabPanel("Outliers",icon=icon("chart-bar"),
               sidebarPanel(
                 checkboxInput(inputId = "standardise", label = "Show Standardized", 
                               value = FALSE),
                 checkboxInput(inputId = "outliers", label = "Show Outliers", 
                               value = TRUE),
                 sliderInput(inputId = "range", label = "IQR Multiplier", 
                             min = 0, max = 5, step = 0.1, value = 1.5)),
               mainPanel(
                 plotOutput(outputId = "Boxplot1"),
                 plotOutput(outputId="Boxplot2"),
                 plotOutput(outputId = "Cooks"))),
      
      tabPanel("Glmnet Model",icon=icon("chart-line"),
               p("Data after clean, Predictions for the test data"),
               p("Missing thresholds the same as missing plot"),
               p("Outliers are based on Cook's distance"),
               sidebarPanel(
                 selectInput(inputId = "ImpMethod", label = "Imputation method", 
                 choices = c("KNN", "Partial Del","Median"), selected = "KNN"),
                 checkboxInput(inputId = "remove", label="Remove Outliers", value=FALSE),
                 actionButton(inputId = "Go", label = "Predict", icon = icon("play"))),
               mainPanel(
                 withSpinner(
                 plotOutput(outputId = "Scatter")),
                 hr(),
                 withSpinner(
                 textOutput("text3")),
                 hr(),
                 withSpinner(
                 textOutput("text4")))),
      
      tabPanel("Residual Box-plot", icon=icon("chart-area"),
               p("Data after clean and imputation"),
               sliderInput(inputId = "range_r", label = "IQR Multiplier", 
                           min = 0, max = 5, step = 0.1, value = 1.5),
               withSpinner(
                 plotOutput(outputId = "Residual"))
               )
               
      
  )))
      
        

    
