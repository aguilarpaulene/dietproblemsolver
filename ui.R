library(shiny)
library(readxl)
library(openxlsx)

#Aguilar, Paulene A.
#Section B4L
#CMSC 150: Final Project (Diet Problem Solver)

food_data <- read.xlsx("food_data.xlsx")    #data frame
food_mat = data.matrix(food_data)
#print(food_mat)
#define ui
ui <- fluidPage(
  titlePanel("Diet Problem Solver"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("food_items", "Select Food Items:", choices = food_data$Foods),
      actionButton("reset", "Reset"),
      actionButton("selectAll", "All"),
      actionButton("preselect", "Pre-select")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Food Items and Nutritional Values",
                 DT::dataTableOutput("food_table"),
        ),
        tabPanel("Results",
                 tags$p("Your selected food/s are:"),
                 tableOutput("selected_foods"),
                 tags$p("Final Solution"),
                 tableOutput("final_solution"),
                 textOutput("objective_value"),
                 tags$p("The solution and cost breakdown by food:"),
                 tableOutput("food_servings_cost")
        ),
        tabPanel("Tableau",
                 textOutput("tableau_i"),
                 tableOutput("tableau_iterations"),
                 textOutput("basic_sol_label"),
                 tableOutput("basic_solution")
        )
      )
    )
  )
   
)