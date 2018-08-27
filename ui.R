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
