
#Try out the examples
# library(shiny)
# runExample('07_widgets')
# 



library(shiny)

my_ui <- fluidPage(
 #adding simple text
 h1('hello CEU'),
 div(h2('to center'), align="center"),
 hr(),
 tags$code('hellooo') , 
 
 div(img(src = "https://rstudio.com/assets/img/og/shiny-og-fb.jpg", height = 140, width = 400)),
 
 div(h3('change collor'), style = "color:blue")
 #includeHTML('')

)

my_server <- function(input, output) {
  
}

shinyApp(ui = my_ui, server = my_server)
