#
# This is the user-interface definition of a Shiny web application
#
# This function is designed to take a three-word phrase as an input, and 
# return a prediction of the next word based on a corpus
#

library(shiny); library(shinybusy)

# Define UI for application that predicts the next word based on an input phrase
fluidPage(

    # Including a progress bar
    add_busy_spinner(spin = "flower", color = "lightblue", 
                     timeout = 100, position = "full-page"),

    # Application title
    titlePanel("Next word prediction"),

    # Sidebar with a text box input for predictor phrase
    sidebarLayout(
        sidebarPanel(
                textInput("predictor",
                        "Enter prediction phrase (of at least three words):"),
                submitButton("Predict!")
        ),

        # Show the predicted next word
        mainPanel(
                h5("The predicted next word is:"),
                textOutput("prediction")
        )
    )
)