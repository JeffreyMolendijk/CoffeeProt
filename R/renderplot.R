#Function to render plots in shiny. Combined the function call, the req() and the plot return.
#The plot parameter should be the plot object
cp_renderplot <- function(plot){
  
  renderedplot <- renderPlot({
    req(plot)
    plot})
  
  return(renderedplot)
}