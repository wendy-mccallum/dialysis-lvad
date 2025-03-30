##################
## Risk Prediction of Dialysis after LVAD ## 3?27/29


#source("server.R")

# Load necessary libraries
library(shiny)
library(survival)
library(shinyjs)



# Load your Cox regression model (assuming it's saved as "cox_model.rds")
load(paste0( "H:/Research/CTSI/Fall 2024/intermacs biolincc descriptions/calculator/app/app/shrinkage.Rdata"))
#saveRDS(form.bw, "H:/Research/CTSI/Fall 2024/intermacs biolincc descriptions/calculator/form.bw.rds")

#form.bw <- readRDS("H:/Research/CTSI/Fall 2024/intermacs biolincc descriptions/calculator/form.bw.rds")



# Define the UI
ui <- fluidPage(
  useShinyjs(),  # Optional, if using shinyjs for interactivity
  titlePanel("Risk Prediction Calculator"),
  
  sidebarLayout(
  sidebarPanel(
    HTML("<h3>Input parameters</h3>"),
    
    numericInput("AGE_DEIDENT", label = "Age (years)", 
                 min = 18, max = 90,
                 value = 60),
    selectInput("MALE",label = "Sex",
                choices = list("Male"="1", "Female"="0"),
                selected = "1"),
    selectInput("DEVICE_TY",label = "Device Type",
                choices = list("LVAD"="1", "BIVAD"="3"),
                selected = "1"),
    selectInput("w_profile",label = "Intermacs Profile",
                choices = list("Profile 1"="1", "Profile 2"="2", "Profile 3"="3",
                               "Profile 4"="4","Profile 5-7"="5"),
                selected = "1"),
    numericInput("W_GFR", label = "eGFR (ml/min/1.73 m2)", 
                 min = 0, max = 120,
                 value = 60),
    numericInput("BUN_MG_DL", label = "BUN (mg/dl)", 
                 min = 0, max = 160,
                 value = 30),
    numericInput("HEMOGLOBIN_G_DL", label = "Hemoglobin (g/dl)", 
                 min = 0, max = 30,
                 value = 12),
    numericInput("ALBUMIN_G_DL", label = "Albumin (g/dl)", 
                 min = 0, max = 10,
                 value = 4),
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
    
    mainPanel(
      h3("Predicted Risk"),
      verbatimTextOutput("riskOutput")
    )
  )
)


# Define the server function
server <- function(input, output, session) {
  
  # Function to make prediction based on user input
  observeEvent(input$submitbutton, {
    
    # Gather the inputs from the user
    new_data <- data.frame(
      AGE_DEIDENT = input$AGE_DEIDENT,
      MALE = input$MALE,
      DEVICE_TY = input$DEVICE_TY,
      w_profile = input$w_profile,
      W_GFR = input$W_GFR,
      BUN_MG_DL = input$BUN_MG_DL,
      HEMOGLOBIN_G_DL = input$HEMOGLOBIN_G_DL,
      ALBUMIN_G_DL = input$ALBUMIN_G_DL
    )
    
    output$riskOutput<-renderText({
      # Predict the risk using the Cox model (use predict() function)
      # Assuming your Cox model produces a linear predictor, which is a risk score
predicted_risk <- predict(bw.model, newdata = new_data, type = "lp")  # Returns a numeric vector

# Compute baseline survival
baseline_survival <- basehaz(bw.model, centered = FALSE)

# Extract the hazard value at the desired time point
h0.i <- baseline_survival$hazard[which.min(abs(baseline_survival$time - horizon))]

# Calculate survival probability
p.RFS <- exp(-h0.i * exp(predicted_risk))  # Correctly apply risk

# Display result
paste(
  "The predicted risk score is:", round(predicted_risk, 2), "\n",
  "The predicted dialysis-free survival probability at", horizon, "months is:", round(p.RFS, 2)
)
    })
  })
}    
    

# Run the application
shinyApp(ui = ui, server = server)




