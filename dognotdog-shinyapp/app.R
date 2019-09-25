##Written  by Roshan Noronha
##September 17, 2019
##A R shiny app that classifies and image as "dog" or "not dog"

#install packages if missing
if (!require(shiny)) {
  install.packages("shiny")
}

if (!require(imager)) {
  install.packages("imager")
}

if (!require(reticulate)) {
  install.packages("reticulate")
}

if (!require(markdown)) {
  install.packages("markdown")
}

#load required packages
library(shiny)
library(imager)
library(reticulate)
library(markdown)

#create virtual env
virtualenv_install("env", packages = c("Pillow", "keras", "tensorflow"), ignore_installed = FALSE)

#import python libraries
PIL <- import(module = "PIL")
keras <- import("keras")

#load weight file
trainedModel <- keras$models$load_model("V3-transferlearningcatsanddogs.h5")

#define UI
ui <- fluidPage(
  
  titlePanel("Dog Not a Dog"),
  
  navbarPage("", 
             tabPanel("App",
                sidebarLayout(
                  sidebarPanel(
                    fileInput("file", "Upload your Picture", accept = c('image/png', 'image/jpeg'))
                  ),
                  
                  mainPanel(
                    
                    plotOutput("image"),
                    textOutput("text")
                  )
                )),
             
             tabPanel("About",
                      includeMarkdown("about.md"))
  )
)

server <- function(input, output) {
  
  observeEvent(input$file, {
    
    #don't remove
    userImage <- input$file
    
    output$image <- renderPlot({
      
      plot(load.image(userImage$datapath), axes = FALSE)
    })
    
    output$text <- renderText(defDog(userImage$datapath))
  })
  
  defDog <- function(imagePath) {
    
    #need to convert R vector to a python list since load_img is a python function
    testImage <- keras$preprocessing$image$load_img(path=imagePath, target_size = r_to_py(c(224L, 224L)))
    testImage <- keras$preprocessing$image$img_to_array(testImage, data_format = "channels_last")
    testImage <- keras$backend$expand_dims(testImage, axis = 0L)
    
    prediction <- trainedModel$predict(testImage, steps = 1L)
    
    if(which(prediction == max(prediction)) == 1) {
      return ("Not a dog")
      
    } else {
      
      return ("This is a dog!")
    }
  }
}

shinyApp(ui = ui, server = server)
