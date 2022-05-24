resultTab <- f7Tab(
  tabName = "Diagnosis",
  hidden = TRUE,
  # icon = f7Icon("graph_circle"),
  icon = emo::ji('chart increasing'),
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
    
    uiOutput('theta_text'),
    shinycssloaders::withSpinner(plotOutput('theta_areasPlot'))
  ),
  
  
  f7Card(
    title = "Average Bacillary Burden (given TBM Positive)",
    uiOutput('re_text'),
    shinycssloaders::withSpinner(plotOutput('re_areasPlot'))
  ),
  
  
  f7Block(
    f7BlockHeader('Proability of positive confirmation tests'),
    uiOutput('test_text'),
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