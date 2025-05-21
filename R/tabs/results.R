resultTab <- f7Tab(
  tabName = "Diagnosis",
  title = "Diagnosis",
  hidden = TRUE,
  # icon = f7Icon("graph_circle"),
  icon = emoji::emoji('chart increasing'),
  active = FALSE,
  tags$head(
    tags$style(sass::sass(sass::sass_file('www/css/main.scss'))),
    tags$script(src='js/main.js'),
    htmltools::htmlDependency(name = "f7-icons", version = "3.0.0", 
                              src = c(file = "f7icons"), package = "shinyMobile", stylesheet = "css/framework7-icons.css", 
                              all_files = TRUE)
  ),
  # f7Flex(
    f7Block(shiny.fluent::PrimaryButton.shinyInput(inputId = 'send_back', text = 'Back'), style = 'position:relative; top:20px; left:20px; width: 150px; padding: 10px'),
  # ),
  uiOutput('scenario_options'),
  shiny.fluent::PrimaryButton.shinyInput(inputId = 'submit', text = 'Re-estimate with scenario'),
  
  
  # shiny.fluent::Dropdown.shinyInput(
  #   inputId = 'scenario',
  #   value = 0,
  #   options = list(
  #     list(key = 0, text = "No test"),
  #     list(key = 1, text = "Smear (-)"),
  #     list(key = 2, text = "Xpert (-)"),
  #     list(key = 3, text = "Smear (-), MGIT (-)"),
  #     list(key = 4, text = "Smear (-), Xpert (-)"),
  #     list(key = 5, text = "All tests (-)")
  #   )
  # ),
  
  f7Card(
    title = "Risk of TBM",
    
    shinycssloaders::withSpinner(uiOutput('theta_text'), type=7, size=.2, hide.ui=FALSE),
    f7Accordion(
      f7AccordionItem(
        title = '',
        shinycssloaders::withSpinner(plotOutput('theta_areasPlot'))
      )
    )
  ),
  
  
  f7Card(
    title = "Average Bacillary Burden (given TBM Positive)",
    shinycssloaders::withSpinner(uiOutput('re_text'), type=7, size=.2, hide.ui = FALSE),
    f7Accordion(
      f7AccordionItem(
        title = '',
        shinycssloaders::withSpinner(plotOutput('re_areasPlot'))
      )
    )
  ),
  
  
  f7Block(
    f7BlockHeader('Probability of positive confirmation tests'),
    uiOutput('test_text'),
    
    div(style='display:block; clear:both; height: 50px'),
    f7Accordion(
      id = 'test_plots',
      multiCollapse = TRUE,
      f7AccordionItem(
        title = 'Smear',
        f7Block(
          uiOutput('smear_text'),
          shinycssloaders::withSpinner(plotOutput('smear_areasPlot')),
        )
      ),
      f7AccordionItem(
        title = 'Mgit Culture',
        f7Block(
          uiOutput('mgit_text'),
          shinycssloaders::withSpinner(plotOutput('mgit_areasPlot')),
        )
      ),
      f7AccordionItem(
        title = 'GeneXpert',
        f7Block(
          uiOutput('xpert_text'),
          shinycssloaders::withSpinner(plotOutput('xpert_areasPlot')),
        )
      ),
    )
  )
)