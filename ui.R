library(shiny)
library(shinyjs)
library("shinyWidgets")

shinyUI(pageWithSidebar(
  headerPanel('Event Study Workbench'),
  sidebarPanel(
    tags$script(HTML("
     $(document).ready(function() {
                     $('.btn').on('click', function(){$(this).blur()});
                     })
                     ")),
    fileInput("inputReturnData", "Upload return data", multiple = FALSE, accept = NULL),
    fileInput("inputpardata", "Upload parameter data", multiple = FALSE, accept = NULL),
    actionButton("startbutton", "Start"),
    # You can open the modal server-side, you have to put this in the ui :
    tags$script("Shiny.addCustomMessageHandler('launch-modal', function(d) {$('#' + d).modal().focus();})"),
    tags$script("Shiny.addCustomMessageHandler('remove-modal', function(d) {$('#' + d).modal('hide');})"),
    # Code for creating a modal
    tags$div(
      id = "my-modal",
      class="modal fade", tabindex="-1", `data-backdrop`="static", `data-keyboard`="false",
      tags$div(
        class="modal-dialog",
        tags$div(
          class = "modal-content",
          tags$div(class="modal-header", tags$h4(class="modal-title", "Calculation in progress")),
          tags$div(
            class="modal-body",
            shinyWidgets::progressBar(id = "pb", value = 0, display_pct = TRUE)
          ),
          tags$div(class="modal-footer", tags$button(type="button", class="btn btn-default", `data-dismiss`="modal", "Dismiss"))
        )
      )
    ),
    actionButton("reset_input", "Reset parameters"),
    uiOutput('resetable_input')
  ),
  
  
  mainPanel(useShinyjs(),
            tabsetPanel(type = "tabs", id = "inTabset",
                        tabPanel("Plot", h3("Mean Cumulative Return"), 
                                 actionButton("erase_plot", "Erase All"),
                                 div(id = "plot-container",
                                     #tags$img(src = "spinner.gif",
                                     #         id = "loading-spinner"),
                                     plotOutput("meancumreturnplot")
                                 ),
                                 textOutput("alert1"),
                                 tags$head(tags$style("#alert1{color: red;
                               font-size: 30px;
                                            font-style: italic;
                                            }"
                                 )),
                                 textOutput("filteredempty"),
                                 tags$head(tags$style("#filteredempty{color: red;
                               font-size: 30px;
                                            font-style: italic;
                                            }"
                                 ))), 
                        tabPanel("Summary of Cumulative Return",tableOutput("summary")
                        ))
            
  )
    
))
