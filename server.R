library(shiny)
library(readxl)
library(openxlsx)

#Aguilar, Paulene A.
#Section B4L
#CMSC 150: Final Project (Diet Problem Solver)

food_data <- read.xlsx("food_data.xlsx")    #data frame
food_mat = data.matrix(food_data)

#perform simplex method (minimization) from selected inputs
simplex <- function(selected) {

  #set-up initial tableau
  n = nrow(selected)
  #cat("Number of selected foods:",n,"\n")
  
  #food price and nutritional values
  food<-selected[,1] #class = character
  price<-selected[,2]
  servings<-selected[,3]
  calories<-selected[,4]
  cholesterol<-selected[,5]
  total_fat<-selected[,6]
  sodium<-selected[,7]
  carbohydrates<-selected[,8]
  fiber<-selected[,9]
  protein<-selected[,10]
  vit_a<-selected[,11]
  vit_c<-selected[,12]
  calcium<-selected[,13]
  iron<-selected[,14]
  
  #set-up constraints
  min<-c(-2000,0,0,0,0,-25,-50,-5000,-50,-800,-10) #minimum nutritional values
  max<-c(2250,300,65,2400,300,100,100,50000,20000,1600,30) #maximum nutritional values (initialize as negative for simpler set-up for minimization)
  minserving<-0
  maxserving<-10
  
  #set-up matrix for initial tableau
  objectiveMat<-matrix(c(price,0), nrow = 1) #in objective function
  
  nutrientsMat = matrix(data = c(calories,cholesterol,total_fat, sodium, carbohydrates,
                                 fiber,protein,vit_a,vit_c,calcium,iron), nrow=nrow(selected), ncol=11)
  nutrients = matrix(as.numeric(nutrientsMat), nrow=nrow(nutrientsMat), ncol=ncol(nutrientsMat)) #convert matrix array into numeric
  
  #initialize matrix for nutrients constraints
  nutrientsconstraints <- matrix(nrow = 0, ncol=nrow(nutrients)+1)
  for(i in 1:length(min)){
    nutrientsconstraints <- rbind(nutrientsconstraints, c(nutrients[,i], min[i]))
    nutrientsconstraints <- rbind(nutrientsconstraints, c(-nutrients[,i], max[i]))
  }
  
  #initialize matrix for servings constraints
  servingsconstraints <- matrix(0, nrow = n, ncol = n+1)
  for(i in 1:n){
    servingsconstraints[i,i] = -1
    servingsconstraints[i,ncol(servingsconstraints)] = 10
  }
  
  #initialize x_i 
  x = diag(nrow = nrow(selected)+1)
  
  #combined all constraints
  allconstraints = rbind(nutrientsconstraints, servingsconstraints, x, objectiveMat)
  
  #transpose matrix for dual problem
  tMat = t(allconstraints)
  tMat = matrix(as.numeric(tMat), nrow = nrow(tMat), ncol = ncol(tMat))
  
  #matrices for checking of values
  matrices = list(objective_matrix = objectiveMat, nutrients_matrix = nutrients, nutrientsconstraints = nutrientsconstraints, 
                  servings_constraints = servingsconstraints, initial_tableau = tMat)
  #print(matrices)

  #set column names for initial tableau
  nutrientsCol = ncol(nutrients)
  nutrientsName = paste("S", 1:((nutrientsCol*2)+n), sep="")  #for nutrients and servings constraints column names 
  servingsName = paste("x", 1:n, sep = "") #for servings 
  z_solution = c("Z", "Solution") #for Z and Solution
  
  #initialize variables
  t = matrix(as.numeric(tMat), nrow = nrow(tMat), ncol = ncol(tMat), dimnames = list(NULL, c(nutrientsName, servingsName, z_solution))) #matrix to be implemented
  solve_t = TRUE    #set to TRUE until condition is met
  iteration = 1     #number of tableaus
  basicSolutions = list()  #list of basic solution for very iteration
  tableaus = list()        #list of tableaus for every iteration
  feasible = TRUE          #boolean to hold if the problem is feasible or not
  tableaus[[1]] = t        #store initial tableau first in list
  
  while (solve_t) {
    #pivot column (most negative in bottom row)
    PC = which.min(t[nrow(t), 1:(ncol(t) - 1)])
    if (t[nrow(t), PC] >= 0) { #check if negative
      cat("No more negative values in bottom row.\n")
      break
    }
    
    #test ratios for pivot row
    right = t[1:(nrow(t)-1), ncol(t)] #rightmost column
    pivot = t[1:(nrow(t)-1), PC] #pivot column
    test_ratio = rep(Inf, times = length(right)) #initialize test ratio to Inf to set for negative values
    for (i in 1:(nrow(t)-1)) {
      if(pivot[i] > 0){ #if pivot element is positive
        test_ratio[i] = right[i] / pivot[i]  #store test ratio
      }
    }
    #print(test_ratio)
    PR= which.min(test_ratio)
    if (is.infinite(test_ratio[PR])) {
      feasible = FALSE
      cat("Unable to solve problem because pivot row is not valid.\n")
      break
    }
    
    # Pivot element and normalize pivot row
    PE = t[PR, PC]
    if(PE == 0){        #if pivot element is 0
      feasible = FALSE
      cat("Unable to solve problem because pivot element is zero.\n")
    }
    
    t[PR, ] = t[PR, ] / PE #normalization
    
    #row operations to turn other elements in pivot column to zero
    for (i in 1:nrow(t)) {
      if (i != PR) {
        t[i, ] = t[i, ] - t[i, PC] * t[PR, ]   #row elimination
      }
    }
    
    # store tableau in list of tableaus
    tableaus[[iteration+1]] = t
    
    # extract basic solution from tableau
    basicMat = matrix(c(0), nrow = 1, ncol = ncol(t) - 1, dimnames = list(NULL, c(nutrientsName, servingsName, "Z")))
    for (j in 1:ncol(basicMat)) {
      col = t[, j]
      if (sum(col == 1) == 1 && sum(col == 0) == (nrow(t) - 1)) { #check if the column has exactly one 1, and the rest is zero
        basicMat[1,j]=t[col == 1, ncol(t)]
      }
    }
    basicSolutions[[iteration]] = basicMat
    
    # Check optimality
    if (all(t[nrow(t), 1:(ncol(t) - 1)] >= 0)) { #if no negative values in bottom row
      cat("Optimal solution found.\n")
      solve_t = FALSE
    }
    iteration <- iteration + 1
  }
  
  #since dual problem the final solution will be the last row per column of the last tableau
  final_solution = matrix(data = t[nrow(t), 1:(ncol(t)-2)], nrow=1, ncol=ncol(t)-2, 
                          dimnames = list(NULL, c(nutrientsName, servingsName))) 
  Z = t[nrow(t), ncol(t)] #the final answer for Z is the value at the last row and last column of final tableau
  final_solution = cbind(final_solution, Z)
  
  #for cost breakdown by food and servings
  serving = final_solution[,servingsName]
  index = which(serving > 0, arr.ind = TRUE) #get index in which servings of food is not equal to 0
  cost_breakdown = matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("Foods", "Servings", "Cost($)")))
  for (i in index) {
    cost = serving[i] * as.numeric(price[i])
    cost = round(cost, digits = 4)
    serving[i] = round(serving[i], digits = 4)
    cost_breakdown <- rbind(cost_breakdown, c(food[i], serving[i], cost))
  }
  #print(cost_breakdown)
  
  # Return results
  result <- list(
    feasibility = feasible,
    n = iteration-1,
    finalTableau = t,
    tableaus = tableaus,
    finalSolution = final_solution, 
    basicSolutions = basicSolutions,
    optimalValue = t[nrow(t), ncol(t)],
    costBreakdown = cost_breakdown
  )
  #print(result)
  return(result)
}

#define server logic
server<-function(input, output, session){
  
  #for tabpanel "Food Items and Nutritional Value"
  output$food_table<- DT::renderDataTable({ #display food data table
    DT::datatable(food_data)
  })
  observeEvent(
    input$selectAll,
    {updateCheckboxGroupInput(session,"food_items",selected = food_data$Foods)
  })
  observeEvent(
    input$reset, 
    {updateCheckboxGroupInput(session, "food_items", selected = 0)
  })
  observeEvent(
    input$preselect,
    {updateCheckboxGroupInput(session, "food_items", selected = food_data$Foods[1:20])}
  )
  food<-reactive({          #select foods from table
    selected_food = food_data[food_data$Foods%in%input$food_items, ] #food in matrix
    return(selected_food)
  })
  result <-reactive({      #pass selected foods to simplex function
    food_selected<-food()
    print(food_selected)
    l_result = simplex(food_selected)
    if(l_result$feasibility == FALSE){
      validate("The problem is infeasible. 
               It is not possible to meet the nutritional constraints with the foods that you have selected. 
               The possible reason is that the pivot element is 0.\n")
    }else{
      good = l_result
    }
  })
  
  #for tabpanel "Results"
  output$selected_foods<-renderTable({
    selected_food<-food() #class = matrix, array
  })
  output$final_solution<-renderTable({
    r = result()
    r$finalSolution
  })
  output$food_servings_cost<-renderTable({
    r = result()
    r$costBreakdown
  })
  output$objective_value<-renderText({
    r = result()
    paste("The cost of this optimal diet is $", round(r$optimalValue, digits = 2), " per day.\n")
  })
  
  #for tabpanel "Tableau"
  output$tableau_i<-renderText({
    r = result()
    paste("Tableau iterations from 0 to ", r$n, "\n")  
  })
  output$tableau_iterations<-renderTable({
    r = result()
    r$tableaus
  })
  output$basic_sol_label<-renderText({
    r = result()
    paste("Basic solutions from tableau 1 to ", r$n,"\n")
  })
  output$basic_solution<-renderTable({
    r = result()
    r$basicSolutions
  })
}