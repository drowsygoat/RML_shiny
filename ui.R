

library(shinythemes)
library(shinysky)
library(shinyjs)
library(shinyWidgets)
library(shiny)

#################### Loading Files ###############################################################################

# logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput

function logifySlider (sliderId, sci = false) {
if (sci) {


// scientific style

$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
})

} else {

// regular number style

$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) { return (Math.pow(10, num)); }

})
}
}"

# call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
// wait a few ms to allow other scripts to execute
setTimeout(function() {
// include call for each slider
logifySlider('FDR_dot_plot', sci = true)
}, 5)})
"
##########Java (Key Press) ######################################################################################
js <- '
$(document).on("keyup", function(e) {
if(e.keyCode == 13){
Shiny.onInputChange("keyPressed", Math.random());
}
});
'
##############INACTIVITY TIMER   JS ################################################################################
# 
inactivity <- 'function idleTimer() {
var t = setTimeout(logout, 200000);
window.onmousemove = resetTimer;
window.onkeypress = resetTimer;

function logout() {
Shiny.onInputChange("nieaktywny", Math.random());
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 200000); // time is in milliseconds
}
}
idleTimer();'

############################################################################################################

# tags$head(tags$style(".shiny-plot-output{height:100vh !important;}"))

shinyUI(
  tagList(
    tags$head(
      # tags$style(
      # HTML(
      #   ".dt-button.buttons-columnVisibility {
      #         background: #FF0000 !important;
      #         color: white !important;
      #         opacity: 0.5;
      #      }
      #   .dt-button.buttons-columnVisibility.active {
      #         background: black !important;
      #         color: black !important;
      #         opacity: 1;
      #      }"
      # )
      # ),
      tags$style(
        type = "text/css",
        ".shiny-output-error-custom { font-size: 24px; color: grey; }",
        # ".shiny-output-error { visibility: hidden; }",
        # ".shiny-output-error:before { visibility: hidden; }",
        ".navbar {background-color: #262626;
                  max-height:3px !important;
                  font-family: Arial;
                  font-weight: Bold;
                  font-size: 16px;
                  color: #717d7e;
                  opacity: 0.8;
                  border-color: transparent}"
        # ".well { border-color: transparent; opacity: 0.95; background-color: brown}",
      )
    ),
    
    tags$style(
      HTML(
        "
    .tabbable > .nav > li > a                  {background-color: #C0C0C0;  color:black; margin-top: -10px}
    .tabbable > .nav > li[class=active]    > a {background-color: grey; color:white; margin-top: -10px} "
      )
    ),
    
    
    tags$script(HTML(js)),
    tags$script(HTML(inactivity)),
    tags$script(HTML(JS.logify)),
    tags$script(HTML(JS.onload)),
    
    tags$style(type = "text/css", "body { padding-top: 70px; }"),
    tags$style(type = "text/css", "label { font-size: 18x; color: #717d7e; }"),
    tags$style(
      type = "text/css",
      ".selectize-input { font-size: 14px; color: #717d7e; line-height: 22px; }
                .selectize-dropdown { font-size: 14px; color: #717d7e;  line-height: 22px; }"
    ),
    
    tags$style(
      type = "text/css",
      "body {padding-top: 70px;}
       label { font-size: 15; color: #717d7e;}
       #RiboTag_Entry_e .selectize-input { font-size: 12px; color: #717d7e; line-height: 20px;  width: 50px; }
       selectize-dropdown { font-size: 15px; color: #717d7e;  line-height: 20px, width: 250px; }
       .checkbox-input{color: #717d7e; }
       .tabbable > .nav > li > a {background-color: white;  color:black}
       .tabbable > .nav > li[class=active] > a {background-color: grey; color:black}"
      
    ),
    # tags$style(type = "text/css", "label.control-label, .js-range-slider {text-align: center; vertical-align: middle; } .form-group { display: table-row;}"),
    
    tags$style(
      type = "text/css",
      ".irs-to {background: green;}
       .irs-from {background: green;}
       .irs-bar {background: grey; height: 5px;}
       .irs-bar-edge {background: grey; height: 5px; width: 5px;}
       .irs-line {border: 1px solid black; height: 6px;}
       .irs-single {background: grey}
       .irs-double {background: grey;}
       .irs-max {display: none;}
       .irs-min {display: none;}
       .irs-slider {width: 7px; height: 12px; top: 22px; background: black;}"
    ),
    
    navbarPage(
      title = "Scrapie RML RiboTag",
      position = "fixed-top",
      collapsible = TRUE,
      fluid = TRUE,
      selected = "Information",
      
      # theme = shinytheme("paper"),
      # setBackgroundImage(src = "astro.jpg"),
      #
      # setBackgroundColor(
      #  color = "white"),
      # gradient = c("linear", "radial"),
      # direction = c("bottom", "top", "right", "left"),
      # shinydashboard = FALSE
      
      tabPanel("Single gene look-up",
               fluidPage(
                 sidebarLayout(
                   sidebarPanel(width = 2, fluidRow(
                     column(
                       12,
                       awesomeRadio(
                         inputId = "search_key_mRNA",
                         label = div(style = "font-size: 14px", "Search genes by:"),
                         choices = c("Gene Symbol" = 1, "Ensembl ID" = 2),
                         selected =  1,
                         inline = FALSE
                       ),
                       checkboxInput(
                         inputId = "fix_axis_single_plot",
                         label = "Fixed scales",
                         value = TRUE
                       ),
                       conditionalPanel(
                         "input.search_key_mRNA == 1",
                         selectizeInput(
                           inputId = "RiboTag_Entry",
                           label = div(style = "font-size: 14px", "Gene Symbol"),
                           multiple = FALSE,
                           size = 20,
                           choices = NULL,
                           options = list(
                             placeholder = 'eg. Gad1',
                             onInitialize = I('function() { this.setValue("");}')
                           )
                         )
                       ),
                       conditionalPanel(
                         "input.search_key_mRNA == 2",
                         selectizeInput(
                           inputId = "RiboTag_Entry_e",
                           label = div(style = "font-size: 14px", "Gene Symbol"),
                           multiple = FALSE,
                           size = 20,
                           choices = NULL,
                           width = "800px",
                           options = list(
                             placeholder = 'eg. ENSMUS...',
                             onInitialize = I('function() { this.setValue(""); }')
                           )
                         )
                       )
                     )
                   )),
                   mainPanel(width = 10,
                             fluidRow(fluidRow(
                               column(
                                 12,
                                 busyIndicator("", wait = 0),
                                 plotOutput(outputId = "RiboTagPlot", width = "800px"),
                                 plotOutput(outputId = "RiboTagPlot_RML", width = "800px")
                               )
                             )))
                 )
               )),
      tabPanel("Dot plot",
               fluidRow(column(
                 12,
                 sidebarLayout(
                   sidebarPanel(
                     width = 2,
                     fluidRow(column(
                       12,
                       sliderInput(
                         inputId = "FDR_dot_plot",
                         label = div(style = "font-size: 16px", "FDR threshold"),
                         min = -5,
                         max = -1,
                         value = -1,
                         step = 0.25,
                         ticks = TRUE,
                         width = "200px"
                       )
                     )),
                     fluidRow(column(
                       12,
                       checkboxInput("fix_axis_dot_plot",
                                     label = "Fixed scales",
                                     value = FALSE),
                       checkboxInput("wpi_18_include",
                                     label = "Include 18 WPI",
                                     value = FALSE)
                     )),
                     fluidRow(column(12,
                                     actionButton(
                                       "dot_reset", "Reset"
                                     )))
                   ),
                   mainPanel(width = 10,
                             fluidRow(column(
                               12,
                               busyIndicator("", wait = 0),
                               plotOutput(
                                 outputId = "DotPlotEnrichment",
                                 height = 500,
                                 click = "dot_plot_click",
                                 brush = brushOpts(id = "dot_plot_brush")
                               )
                             )))
                 )
               )),
               fluidRow(column(
                 12,
                 DT::dataTableOutput("table_click_1")
               ))),
      # -------------------------------------------------------------
      tabPanel("Information",
               fluidRow(column(
                 5,
                 tags$iframe(
                   src = "informacje.html",
                   seamless = NA,
                   scrolling = "no",
                   width = 1200,
                   height = 1000
                 )
               )))
    )
  )
)
