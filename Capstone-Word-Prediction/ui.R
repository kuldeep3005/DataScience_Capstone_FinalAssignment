suppressWarnings(library(shiny))
suppressWarnings(library(markdown))

# Next Word Prediction Application UI

shinyUI(navbarPage("Capstone: Final Assignment- Prediction Model",
                   tabPanel("Predict Next Word",
                            HTML("<strong>Author: Kuldeep Singh Meena</strong>"),
                            br(),
                            HTML("<strong>Date: 05/09/2019</strong>"),
                            br(),
                            img(src = "Coursera.png"),
                            br(),
                            br(),
                  #title for left side of page
                            titlePanel("Text Prediction: User Interface"),
                  # Sidebar for user to enter part of a sentence 
                            sidebarLayout(
                                sidebarPanel(
                                    helpText("This is prototype model of Neuro Linguistic Programming techniques to predict next word"),
                                    textInput("inputString", "Please Enter a word, Text or a sentence",value = ""),
                                    br(),
                                    br(),
                                    img(src = "Swiftkey.png"),
                                    br(),
                                    br(),
                                    br()
                                ),
                                # Main panel to display the results of the word prediction
                                mainPanel(
                                    h2("Next Word Prediction"),
                                    verbatimTextOutput("prediction"),
                                    strong("Here is what the user has entered:"),
                                    tags$style(type='text/css', '#text1 {background-color: rgba(102,255,102,0.50); color: black;}'),
                                    textOutput('text1'),
                                    br(),
                                    strong("Here is how the next word was Predicted:"),
                                    tags$style(type='text/css', '#text2 {background-color: rgba(130,255,180,0.30); color: black;}'),
                                    textOutput('text2'),
                                    br(),
                                    br(),
                                    br()
                                )
                            )
                            
                   ),
                   tabPanel("About",
                            mainPanel(
                                img(src = "Coursera.png"),
                                br(),
                                img(src = "Swiftkey.png"),
                                img(src = "R_Studio.jpg",  height = 70, width = 210),
                                br(),
                                includeMarkdown("./about.md")
                            )
                   )
)
)