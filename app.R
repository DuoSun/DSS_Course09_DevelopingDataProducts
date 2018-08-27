#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Iris Data Feature Selection"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(3,
           wellPanel(     
             h3("1. Introduction"),
             h5(paste0(
               "The performance of five popular algorithms (LDA, KNN, SVM, Radom Forest, and CART) ",
               " are compared on Iris data for prediction of iris species (setosa, versicolor, and virginica). "
             )),
             h5(paste0(
               "Iris data set is split into training and validation data sets. ",
              " 10-folder cross-validation is used on training dataset to train the models and estimate the prediction accuracy. ",
               "The built models are also tested on validation data."
             )),
             h3("2. Select Predictors"),
             h5(paste0(
               "User can select different predictors (one, some, or all of four) for model building."
             )),
             checkboxInput("checkboxSL","Sepal Length",value = TRUE),
             checkboxInput("checkboxSW","Sepal Width",value = TRUE),
             checkboxInput("checkboxPL","Petal Length",value = TRUE),
             checkboxInput("checkboxPW","Petal Width",value = TRUE),
             h3("3. Select Seed"),
             h5(paste0(
               "User can set different seed value to create different training/validation datasets."
             )),
             numericInput("numeric", "Seed", value = 1, min = 1, max = 100, step = 1),
             submitButton("Build / Update Prediction Models")
           )
    ),
    column(9,
           wellPanel(
             h4("Figure. 10-fold cross-validation training results of the five algorithms"),
             plotOutput("ResultDotPlot"),
             h4("Table. estimated predication accuracy vs measured accuracy on validation data"),
             tableOutput("ResultAccuracy")
           )
    )
  )
)

server <- function(input, output) {
  
  # Load library and iris data
  library(caret)
  library(e1071)
  library(randomForest)
  data(iris)
  dataset_all <- iris
  
  # Split out validation dataset
  validation_index <- reactive({
    set.seed(input$numeric)
    createDataPartition(dataset_all$Species, p=0.80, list=FALSE)
  })
  validation <- reactive({
    dataset_all[-validation_index(),]
  })
  dataset_training <- reactive({
    dataset_all[validation_index(),]
  })
  
  # Select predictors  
  dataset <- reactive({
    predictors <- NULL
    if (input$checkboxSL) predictors <- c(predictors,"Sepal.Length")
    if (input$checkboxSW) predictors <- c(predictors,"Sepal.Width")
    if (input$checkboxPL) predictors <- c(predictors,"Petal.Length")
    if (input$checkboxPW) predictors <- c(predictors,"Petal.Width")
    dataset <- dataset_training()[,c(predictors,"Species")]
  })
  
  # Run algorithms using 10-fold cross validation
  control <- trainControl(method="cv", number=10); metric <- "Accuracy" 
  
  # Train algorithms
  fit.lda <- reactive({
    set.seed(0);fit.lda <- train(Species~., data=dataset(), method="lda", metric=metric, trControl=control)  
  })
  fit.cart <- reactive({
    set.seed(0);fit.cart <- train(Species~., data=dataset(), method="rpart", metric=metric, trControl=control)  
  })
  fit.knn <- reactive({
    set.seed(0);fit.knn <- train(Species~., data=dataset(), method="knn", metric=metric, trControl=control)  
  })
  fit.svm <- reactive({
    set.seed(0);fit.svm <- train(Species~., data=dataset(), method="svmRadial", metric=metric, trControl=control)  
  })
  fit.rf <- reactive({
    set.seed(0);fit.rf <- train(Species~., data=dataset(), method="rf", metric=metric, trControl=control)  
  })
  
  # Compare algorithms based on estimated prediction performance
  results <- reactive({
    results <- resamples(list(lda=fit.lda(), cart=fit.cart(), knn=fit.knn(), svm=fit.svm(), rf=fit.rf()))
  })
  
  # Display training results
  output$ResultDotPlot <- renderPlot({
    dotplot(results())
  })
  
  # Display accurancy performance on validation data and compare with estimated values
  output$ResultAccuracy <- renderTable({
    predictions.lda <- predict(fit.lda(), newdata=validation())
    accuracy.lda    <- confusionMatrix(predictions.lda, validation()$Species)$overall["Accuracy"]
    predictions.cart<- predict(fit.cart(), newdata=validation())
    accuracy.cart   <- confusionMatrix(predictions.cart, validation()$Species)$overall["Accuracy"]
    predictions.knn <- predict(fit.knn(), newdata=validation())
    accuracy.knn    <- confusionMatrix(predictions.knn, validation()$Species)$overall["Accuracy"]
    predictions.svm <- predict(fit.svm(), newdata=validation())
    accuracy.svm    <- confusionMatrix(predictions.svm, validation()$Species)$overall["Accuracy"]
    predictions.rf  <- predict(fit.rf(), newdata=validation())
    accuracy.rf     <- confusionMatrix(predictions.rf, validation()$Species)$overall["Accuracy"]
    #summary(results())$values
    #print(fit.lda)
    data.frame("Algorithm"=c("lda","cart","knn","svm","rf"),"Estimated Accuracy"=sapply(results()$values[,c(2,4,6,8,10)],mean),
               "Validation Accuracy" = c(accuracy.lda,accuracy.cart,accuracy.knn,accuracy.svm,accuracy.rf))
  },align = 'c')
}

# Run the application 
shinyApp(ui = ui, server = server)