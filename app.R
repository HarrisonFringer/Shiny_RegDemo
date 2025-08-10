library(shiny)
library(bslib)
library(MASS)
library(ggplot2)
# Define UI for application
ui <- fluidPage(theme = bs_theme(
  version = 5,
  bootswatch = "darkly",  # Default themes from lib
  primary = "#0066CC",    # Overrides default color
  base_font = font_google("Roboto"), #fonts of interest
  heading_font = font_google("Bebas Neue") #Otha fonts
),
  navlistPanel(id = "tabset",
               "Regression Exploration",
               tabPanel("Personal Interests", "Prior Experience", p("In my undergraduate years, I spent around a semester working on a project that focused on data simulation. This was later expected to extend to a themed setting (i.e. healthcare outcomes), where the primary goal was to explore different assumptions involved in linear regression. Originally coded in SAS, the simulation setup has been transformed into an interactive shiny app!"), p("To view the work on this project, please click on the following tab labelled 'Clinical Simulation (LR Analysis)'.")),
               
               tabPanel("LR Experiment", "Clinical Studies", 
                        p("Assume you are a researcher who has been investigating a number of features that are believed to significantly identify how wealthy an individual will become! A small sample of the data can be viewed in the table shown below:"), tableOutput("fewobs"), 
                        p("Your first assignment is to diagnose which variables should be utilized to model wealth! As a hint, there are 3 different variables being utilized in this model, and the R-Squared value you are trying to achieve is:",
                        textOutput("ModRSquare")), 
                 fluidRow(column(4, selectInput("pred1", "First Predictor", choices = NULL)), column(4, selectInput("pred2", "Second Predictor", choices = NULL)), column(4, selectInput("pred3", "Third Predictor", choices = NULL))
        ),
                        textOutput("ModelPicked"),
                        actionButton("linmod", "Test Model!"),
                        textOutput("UserRSquare")
      ),
               tabPanel("LR Experiment - Part 2", "Data Modification",
                        p("As mentioned earlier, it is clear that the model was made intentionally simple so that you can intuitively look at how certain variables have different impacts upon the model. That being said, let's make the data a little more messy."),
                        p("It is common for data to have outliers, so we have set up a graph below that allows for you to randomly impute outliers to the dataset. Simply click the button below to modify the data accordingly!"),
                fluidRow(column(4, selectInput("xax", "X-Axis Variable", choices = NULL)),
                         column(8, plotOutput("plot1"))),
                fluidRow(column(2, actionButton("outView", "Add Outliers!")),
                         column(2, actionButton("hetButt", "Heteroskedastize!!")),
                         column(8, textOutput("OutRSquare"))),
                plotOutput("plot2")
                )
    )
  )

# Define server logic
server <- function(input, output, session) {
  
#REACTIVE VARIABLE SECTION
ClinVars = reactive({
  df = as.data.frame(mvrnorm(n = 100, 
                        matrix(data = c(57.17,161.23,60.19,4.83,4.26,73.64,23.11), ncol = 1), 
                        matrix(data = c(251.9942608,-29.1350802,-18.6839261,1.0861618,-0.0042945,-14.5993255,1.0377663,-29.1350802,67.2870847,49.0798668,0.0189653,0.1820476,-6.2200159,-0.9748199,-18.6839261,49.0798668,141.2841523,0.4055821,0.5244046,-16.5727033,40.1839770,1.0861618,0.0189653,0.4055821,0.0252470,0.0174510,0.2129830,0.1502846,-0.0042945,0.1820476,0.5244046,0.0174510,0.0232640,0.3133657,0.1475357,-14.5993255,-6.2200159,-16.5727033,0.2129830,0.3133657,115.3234050,-4.5375690,1.0377663,-0.9748199,40.1839770,0.1502846,0.1475357,-4.5375690,16.0344349), nrow = 7, ncol = 7)
  )) #note n value
  df$V0 = 120 + 30*df$V1 + 10*df$V2 + 60*df$V7 + rnorm(nrow(df), 0, 100)
  colnames(df) = c("Age","Height","Weight","Ln_SBP","Ln_DBP","HR_BPM","BMI","Wealth")
  df
})
vals = reactiveValues(dat = NULL, xvar = "")
usermod = eventReactive(input$linmod,{
                    as.formula(paste0("Wealth ~ ",input$pred1,"+",input$pred2,"+",input$pred3))
                   })

modelSum = reactive({
  lm(as.formula(paste0("Wealth ~ ",input$xax)), data = vals$dat)
  }) 

testMod = reactive({
  lm(as.formula(paste0("Wealth ~ ",input$pred1,"+",input$pred2,"+",input$pred3)), data = vals$dat)
})
#This approach appears to better work within this context than the above approach
#OBSERVE SECTION
observe({
  vals$dat = ClinVars()
})
observe({
  updateSelectInput(session, "pred1", choices = colnames(ClinVars()[,-8]))
})
observe({
  updateSelectInput(session, "pred2", choices = colnames(ClinVars()[,-8]))
})
observe({
  updateSelectInput(session, "pred3", choices = colnames(ClinVars()[,-8]))
})
observe({
  updateSelectInput(session, "xax", choices = colnames(ClinVars()[,-8]))
})

observeEvent(input$outView,{
    df = vals$dat
    df$Wealth[floor(runif(1,1,100))] = df$Wealth[floor(runif(1,1,100))] + 3*sd(df$Wealth) #Note this will get worse each iteration if not prefactored, again this can be expanded upon successful integration
  vals$dat = df
})

observeEvent(input$hetButt,{
    df = vals$dat
    for (i in 1:100){#again, the 100 is "n"
      df$Wealth[i] = df$Wealth[i] + df$Age[i]*(15)*sample(c(-1,1), 1, replace = T, prob = c(0.5, 0.5))
    }
    vals$dat = df
})

#Name assignment necessary for shiny operation

#OUTPUT SECTION
output$fewobs = renderTable(head(ClinVars()))
output$ModRSquare = renderText(summary(lm(Wealth ~ Age + Height + BMI, data = ClinVars()))$r.squared)
output$ModelPicked = renderText(paste0("Selected Model: ",input$pred1,"+",input$pred2,"+",input$pred3))
output$UserRSquare = renderText(paste0("Model R-Square: ",summary(lm(usermod(), data = ClinVars()))$r.squared))
output$plot1 = renderPlot({ggplot(data = ClinVars(), aes(x = .data[[input$xax]], y = Wealth)) +
    geom_point() +
    geom_abline(intercept = summary(modelSum())$coefficients[1,1], slope = summary(modelSum())$coefficients[2,1])
  })
output$OutRSquare = renderText(paste0("Model R-Square: ", summary(modelSum())$r.squared))
output$plot2 = renderPlot({ggplot(testMod(), aes(x = .fitted, y = .resid)) + 
    geom_point()})
}


# Run the application 
shinyApp(ui = ui, server = server)
