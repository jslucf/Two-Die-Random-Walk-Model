#setup a dice
dice=1:6

#Function that takesthe number of dice, the target number(s) to stop the walk, and the number of iterations to run
random.walk = function(dice.n, target, iter){
  
  #setup a vector to hold results of random walk
  count_rolls=c()
  
  #simulate 60 times
  for(n in 1:iter){
    
    #turn counter to 1 for first roll
    count_rolls[n] = 0
    
    #this is a test to see if a four was rolled on either dice. set to 0 before first random walk
    four=0
    
    #this is the random walk which will keep running as long as a 4 is not rolled
    while(four == 0){
      
      #roll a dice twice
      dice_roll = sample(dice, dice.n, replace=T)
      
      #if either dice is a 4, then end the random walk to start the next one
      if(any(dice_roll[1:dice.n] %in% target)){
        count_rolls[n] = count_rolls[n]+1
        four=1
        
        #if neither is a 4, then add 1 to the count for this random walk and then roll again
      } else{
        count_rolls[n] = count_rolls[n]+1
        four=0
      }
    } #while
  } #for
  
  return(data.frame(count_rolls))
} #function



library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Random Walk with Dice Simulator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("dice.n",
                     "Number of dice tossed per simulation (1 - 6): ",
                     min = 1,
                     max = 6,
                     value = 2),
         
         checkboxGroupInput("target", label = "Select target number(s) for the random walk: ", choices = 1:6, 
                            selected = 4),
      
      
      numericInput("iter", "Select the number of simulations to run (1 - 10,000): ", 
                  min = 1, max = 10000, value = 60, step = 10)
      
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("directions"),
        
         plotOutput("distPlot"),
         
        verbatimTextOutput("summary"),
        
        verbatimTextOutput("true.value")
      )
   )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  walk = reactive({  #perform the random walk function
    
    random.walk(input$dice.n, input$target, input$iter)
    
    })
  
  output$directions = renderPrint({
cat("This app creates a simulation of a random walk using dice. A random walk will roll a dice as many times as needed before it rolls its target (in the case of multiple die, until one of the die rolls 
the target number, or in the case of multiple targets, it will keep going until one of the target numbers is rolled).
This process is then repeated to create a distribution of how many times it typically would take for the random walk 
to reach its target. \n \n 
For example, the default settings rolled two dice until one of them rolled a 4, 
and then repeated this 60 times.", sep="\n")
    })

    #histogram of random walk
   output$distPlot = renderPlot({ 
     ggplot(walk(), aes(count_rolls)) + 
       geom_histogram(bins = max(walk() )  ) +
       xlab("Number of Rolls Needed") + 
       scale_x_continuous(breaks = 1:max(walk() ) )
       })
   
   #prints the summary table
   output$summary = renderPrint({  
     summary(walk() )
     
     })
   
   output$true.value = renderPrint({
     
     true.value =  1 - ((6 - length(input$target)) / 6)^input$dice.n
     fraction = paste( (6^input$dice.n  - (6 - length(input$target))^input$dice.n), 
                       6^input$dice.n, sep="/")
     
     paste("The true probability of rolling at least one ", 
           paste(input$target, collapse= " or "), 
           " using ",
           input$dice.n, " dice is ",  
           round(true.value, 3), 
           " (",
           fraction,
           ")",
           ", or 1-in-every-", round(1/true.value, 2), 
           " turns.",
           sep="")
     
     
   })
     
} #server

# Run the application 
shinyApp(ui = ui, server = server)

