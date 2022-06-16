# Define server logic required to draw plots
shinyServer(function(input, output,session) {
  onSessionEnded(function() {
    stopApp()})
  output$Summary <- renderUI({
    print(dfSummary(dat, varnumbers=TRUE, graph.magnif=0.8),
          method="render",
          heading=FALSE,
          bootstrap.css=FALSE)})
  
  output$table <- renderDataTable({DT::datatable(data = as.data.frame(dat[,-c(16)]))})
  
  output$Mosaic <- renderPlot({
    formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
    mosaic(formula, data = catsdata,
           main = "Mosaic Plot of Discrete Variables", shade = TRUE, legend = TRUE)})
  
  output$Pairs <- renderPlot({
    cat_cols <- c(input$VariablesD)
    num_cols <- c(input$VariablesE)
    colour_1 <- c(input$VariablesF)
    data_mixed <- data.frame(dat[,cat_cols], dat[,num_cols])
    ggpairs(data = data_mixed,  mapping = ggplot2::aes(colour = dat[,colour_1]), 
            columnLabels = c(cat_cols, num_cols), 
            title = "Numeric and Factor Pairs Plot")})
  
  output$Corrgram <- renderPlot({
    corrgram(numsdata, 
             order = input$Group, 
             abs = input$abs, 
             cor.method = input$CorrMeth,
             text.panel = panel.txt,
             main = "Correlation of Numeric Variables")})
  
  output$Tabplot<-renderPlot({
    tabplot::tableplot(dat, sortcol="row", decreasing=FALSE, 
                       title="Tabplot of Data")})
  
  output$Rising <- renderPlot({
    cols <- c(input$VariablesB)
    d <- data.frame(numsdata[,cols])
    for (col in 1:ncol(d)){
      d[,col] <- d[order(d[,col]),col]    #sort each column in ascending order
    }
    d <- scale(x=d, center=input$centre, scale=input$scale)
    mypalette <- rainbow(ncol(d))
    matplot(x = seq(1, 100, length.out = nrow(d)), y = d, 
            type = "l", xlab = "Percentile (%)", ylab = "Values", 
            lty = 1, lwd = 1, col = mypalette, main = "Rising Value Chart")
    legend(legend = colnames(d), x = "topleft", plot=input$legend,  
           lty = 1, lwd = 1, cex=0.65, col = mypalette, ncol = 2)})
  
  output$Matplot <- renderPlot({
    cols <- c(input$VariablesC)
    d <- data.frame(numsdata[,cols])
    d <- scale(d, center = input$centre_m, scale = input$scale_m) 
    matplot(d, type = "l", col = rainbow(ncol(d)), xlab = "Observations in Sequence", 
            ylab = "Value", main="Homogeneity Chart")
    legend(legend = colnames(d), x = "topleft", plot=input$legend_m,  
           lty = 1, lwd = 1, cex=0.65, col = rainbow(ncol(d)), ncol = 2)})

  getCleanData <- reactive({
    d <- dat
    vRatio <- apply(d, 2, pMiss)          #process columns
    d <- d[, vRatio < input$VarThresh]
    oRatio <- apply(d, 1, pMiss)          #process rows
    d <- d[oRatio < input$ObsThresh, ]
    if (input$remove) {
      d <- d[-influ_obs,]
    }
    d
  })
  
  output$Missing <- renderPlot({
    vis_miss(getCleanData(), cluster = input$cluster, sort_miss=input$sort)+ 
             labs(title = "Missingness Values")})
  
  output$text1 <- renderText({
    paste("Original data dimensions are: ", dim(dat)[1], dim(dat)[2])})
  
  output$text2 <- renderText({
    paste("Clean data dimensions are: ", dim(getCleanData())[1], dim(getCleanData())[2])})
  
  output$Misscor <- renderPlot({
    m <- is.na(dat) +0
    cm <- colMeans(m)
    m <- m[, cm>0 & cm<1, drop=FALSE]
    corrgram(cor(m), order = "OLO", abs = TRUE, 
             main = "Variables missing value correlation before clean data")})
  
  output$Pattern <- renderPlot({
    dat$MISSINGNESS <- apply(X=is.na(dat), MARGIN=1, FUN=sum)
    tree <- train(MISSINGNESS~. -CODE-OBS_TYPE, data=dat, method="rpart", na.action=na.rpart)
    rpart.plot(tree$finalModel, main="Predicting the number of missing variables per observation before clean data",
               roundint=TRUE, clip.facs=TRUE)})
  
  output$Boxplot1 <- renderPlot({
    data <- as.matrix(numsdata[,-c(12)])     #not including num_shadow in Boxplots
    data <- scale(data, center = input$standardise, scale = input$standardise)
    Boxplot(y = data, ylab = NA, use.cols = TRUE, notch = FALSE, 
            varwidth = FALSE,  horizontal = FALSE, outline = input$outliers, 
            col = brewer.pal(n = dim(numsdata)[2], name = "RdBu"),
            range = input$range, main = "Boxplots of Numeric Variables",  
            id = ifelse(input$outliers, list(n = Inf, location = "avoid"), FALSE))})
  
  output$Boxplot2 <- renderPlot({
    d <- dat
    Boxplot(DEATH_RATE ~ cut(HEALTHCARE_COST, breaks=c(-Inf,4000, 10000, Inf), labels=c("Low", "Middle","High")),
            data=d, outline = input$outliers,main="Boxplots for Death_Rate vs Healthcare_Cost Level", 
            col = brewer.pal(n = dim(d)[2], name = "RdBu"),ce.axis=0.5,
            range = input$range, xlab="HEALTHCARE_COST Level",
            id = ifelse(input$outliers, list(n = Inf, location = "avoid"),FALSE))})
  
  output$Cooks <- renderPlot({
    plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
    abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
    text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="blue", pos=4)})   
  
  # develop a recipe-based processing pipeline
  getRecipe <- reactive({
    rec <- recipe(DEATH_RATE ~., getCleanData())  %>%
      update_role("CODE", new_role = "id")        %>%      #id is not a predictor
      update_role("OBS_TYPE", new_role = "split") %>%      #obs_type is not a predictor
      step_center(all_numeric(), -has_role("outcome"))%>% 
      step_scale(all_numeric(), -has_role("outcome")) %>%
      step_dummy(all_predictors(), -all_numeric())    
    if (input$ImpMethod == "KNN") {
      rec <- step_impute_knn(rec, all_predictors(), neighbors = 5)
    } else if (input$ImpMethod == "Median") {
      rec <- step_impute_median(rec, all_numeric_predictors())
    } else if (input$ImpMethod == "Partial Del") {
      rec <- step_naomit(rec, all_predictors(), skip = TRUE)   # row wise partial deletion
    }
    rec
  })
  
  getModel <- reactive({
    req(input$Go)          #req(), requires to exit early, efficient
    d <- getCleanData()
    train <- d[d$OBS_TYPE == "Train",]
    isolate({train(getRecipe(), data = train, method = "glmnet", metric="RMSE")})})
  
  output$Scatter <- renderPlot({
    d <- getCleanData()
    test <- d[d$OBS_TYPE == "Test",]
    #train the glmnet model and predict for the test data
    predictions <- predict(getModel(), newdata=test)
    rang <- range(test$DEATH_RATE,predictions)
    ggplot(data=test)+geom_point(mapping=aes(x=DEATH_RATE,y=predictions))+
      geom_abline(slope=1, col="blue")+xlim(rang)+ylim(rang)+
      labs(title="Predicted vs Actual",x="Actual DEATH_RATE", y="Predicted")+
      coord_fixed(ratio=1)})
  
  output$text3 <- renderText({
    paste("Thresholds VarMiss:", input$VarThresh,"%", "  ObsMiss:", input$ObsThresh,"%")})
  
  output$text4 <- renderText({
    d <- getCleanData()
    test <- d[d$OBS_TYPE == "Test",]
    predictions <- predict(getModel(), newdata=test)
    paste('test-RMSE is: ', round(RMSE(test$DEATH_RATE, predictions),4))})

  output$Residual <- renderPlot({
    coef <- input$range_r
    d <- getCleanData()
    mtrain <- d[d$OBS_TYPE == "Train",]
    mtest <- d[d$OBS_TYPE == "Test",]
    
    # create a new col indicating dataset
    d$dataset <- "All"
    mtrain$dataset <- "train"
    mtest$dataset <- "test"
    data <- bind_rows(d,mtrain,mtest) #make a mix data frame
    
    predall <- predict(getModel(), newdata=data)
    data$residual <- data$DEATH_RATE - predall
    rdata <- subset(data, select=c(CODE,residual,dataset))
    
    Boxplot(residual~dataset, data=rdata, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
            horizontal = FALSE, outline = TRUE, 
            col = brewer.pal(n = dim(rdata)[2], name = "RdBu"),
            range = coef, main = paste("Residual boxplots at IQR multiplier of", coef),
            id = list(n = Inf, labels=rdata$CODE,location = "avoid"))})
  })
