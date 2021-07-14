library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(apexcharter)

fluentNumericInput = function(inputId, ...){
  tagList(
    shiny.fluent::TextField.shinyInput(inputId = inputId,
                                       ...),
    tags$script(paste0("$('input#",inputId,"').on('change paste keyup', function(){
        if (!/^\\.?\\d*\\.?\\d*$/.test($(this).val())){
          newstring = $(this).val().slice(0,-1);
          $(this).val(newstring);
        }
      });"))
  )
}

ui <- f7Page(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    text = "
    shinyjs.sendBackFirstTab = () => $('.toolbar-inner > :first-child')[0].click();
  ", functions = c("sendBackFirstTab")),
  f7TabLayout(
    navbar = f7Navbar(
      title = "TBM-LCA Diagnosis Model",
      hairline = TRUE,
      shadow = FALSE,
      leftPanel = FALSE,
      rightPanel = FALSE
    ),
    f7Tabs(
      animated = TRUE,
      # Data ----
      f7Tab(
        tabName = 'Information',
        icon = f7Icon('person_crop_circle'),
        active = TRUE,
        
        f7Flex(
          f7Block(
            f7BlockTitle('Patient History'),
            fluentNumericInput(inputId = 'age',
                               label = 'Patient Age',
                               placeholder = '(years)',
                               description = 'Patient\'s Age in years'),
            shiny.fluent::Toggle.shinyInput(inputId = 'hiv_status',
                                            label = 'HIV Positive'),
            shiny.fluent::Toggle.shinyInput(inputId = 'clin_contact_tb',
                                            label = 'Past TB Contact')
          ),
          f7Block(
            f7BlockTitle('Clinical Symptoms'),
            shiny.fluent::Toggle.shinyInput(inputId = 'clin_symptoms',
                                            label = 'General TB Syndrome'),
            shiny.fluent::Toggle.shinyInput(inputId = 'clin_motor_palsy',
                                            label = 'Local neuro-deficit'),
            shiny.fluent::Toggle.shinyInput(inputId = 'clin_nerve_palsy',
                                            label = 'Cranial nerve palsy'),
            fluentNumericInput(inputId = 'clin_gcs',
                               label = 'Glasgow Coma Score',
                               placeholder = '3-15'),
            fluentNumericInput(inputId = 'clin_illness_day',
                               label = 'Days since first symptoms',
                               placeholder = '(days)',
                               description = 'Number of days since first symptoms to admission'),
          ),
          f7Block(
            f7BlockTitle('X-Ray Signs'),
            shiny.fluent::Toggle.shinyInput(inputId = 'xray_pul_tb',
                                            label = 'Pulmonary non-miliary TB'),
            shiny.fluent::Toggle.shinyInput(inputId = 'xray_mil_tb',
                                            label = 'Miliary TB')
          ),
          f7Block(
            f7BlockTitle('Hematological Tests'),
            fluentNumericInput(inputId = 'csf_lympho',
                               label = 'CSF Lymphocyte Count',
                               placeholder = '(cells/mcL)'),
            fluentNumericInput(inputId = 'csf_neutro',
                               label = 'CSF Neutrophils Count',
                               placeholder = '(cells/mcL)'),
            fluentNumericInput(inputId = 'csf_eos',
                               label = 'CSF Eosinophils Count',
                               placeholder = '(cells/mcL)'),
            fluentNumericInput(inputId = 'csf_rbc',
                               label = 'CSF Red Cell Count',
                               placeholder = '(cells/mcL)')
          ),
          f7Block(
            f7BlockTitle('Biochemical Tests'),
            fluentNumericInput(inputId = 'csf_glucose',
                               label = 'CSF Glucose',
                               placeholder = '(mg/100 mL)'),
            fluentNumericInput(inputId = 'bld_glucose',
                               label = 'Corresponding Blood Glucose',
                               placeholder = '(mg/100 mL)'),
            fluentNumericInput(inputId = 'csf_protein',
                               label = 'CSF Protein',
                               placeholder = '(g/L)'),
            fluentNumericInput(inputId = 'csf_lactate',
                               label = 'CSF Lactate',
                               placeholder = '(g/L)')
          ),
          f7Block(
            f7BlockTitle('Others'),
            shiny.fluent::Toggle.shinyInput(inputId = 'crypto',
                                            label = 'Cryptococcal Antigen/Indian Ink')
          )
        )
        
        
      ),
      # Results ----
      f7Tab(
        tabName = "Diagnosis",
        icon = f7Icon("graph_circle"),
        active = FALSE,
        f7Select(
          inputId = "Model",
          label = "Model choice", 
          choices = paste("Model", 1:5)
        ),
        div(
          f7Block(shiny.fluent::PrimaryButton.shinyInput(inputId = 'submit', text = 'Run')),
          style = 'width:100px;'
        ),
        # div(style="clear:both"),
        
        tags$head(
          tags$style(sass::sass(sass::sass_file('www/css/main.scss'))),
          tags$script(src='js/main.js')
        ),
        
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            title = "Chance of TBM",
            uiOutput('theta_text'),
            shinycssloaders::withSpinner(plotOutput('theta_areasPlot'))
          ),
        ),
        
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            title = "Average Bacillary Burden",
            uiOutput('re_text'),
            shinycssloaders::withSpinner(plotOutput('re_areasPlot'))
          )
        ),
        
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Block(
            f7BlockHeader('Chances of postive confirmation tests'),
            uiOutput('test_text'),
            f7Accordion(
              id = 'test_plots',
              multiCollapse = TRUE,
              f7AccordionItem(
                title = 'Smear'
                f7Block(
                  uiOutput('smear_text'),
                  shinycssloaders::withSpinner(plotOutput('smear_areasPlot')),
                )
              ),
              f7AccordionItem(
                title = 'Mgit Culture'
                f7Block(
                  uiOutput('mgit_text'),
                  shinycssloaders::withSpinner(plotOutput('mgit_areasPlot')),
                )
              ),
              f7AccordionItem(
                title = 'GeneXpert'
                f7Block(
                  uiOutput('xpert_text'),
                  shinycssloaders::withSpinner(plotOutput('xpert_areasPlot')),
                )
              ),
            )
          )
        )
      ),
      f7Tab(
        tabName = "Info",
        icon = f7Icon("info_circle"),
        active = FALSE,
        f7Block(
          div(
            style = 'text-align:center',
            uiOutput('oucru_logo'),
            h1('TBM-LCA Supplementary Web App'),
            p('Decision helper for TBM Diagnosis'),
            p('Intended for Academic Use'),
            p('2021'),
            p('Contact: trinhdhk@oucru.org')
          )
        )
      )
    )
  )
)