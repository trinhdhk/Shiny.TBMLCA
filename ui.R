library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(apexcharter)
options('spinner.type' = 8)

fluentNumericInput = function(inputId, ..., warn.validator = NULL){
  tagList(
    shiny.fluent::TextField.shinyInput(inputId = inputId,
                                       ...),
    tags$script(paste0("$('input#",inputId,"').on('change paste keyup', function(){
        if (!/^\\.?\\d*\\.?\\d*$/.test($(this).val())){
          newstring = $(this).val().slice(0,-1);
          $(this).val(newstring);
        }
      });")),
    
    if (length(warn.validator)) {
      tags$script(
        HTML(glue::glue(
          "
          $('input#[inputId]').on('change paste keyup', function(){
            validator = [warn.validator];
            status = validator($(this).val());
            if (status == 0 || !$(this).val()) {
              $(this).attr('data-warn', '0');
            }
            if (status == 1) $(this).attr('data-warn', '1');
            if (status == 2) $(this).attr('data-warn', '2');
          }).focusout(function(){
            if ($(this).attr('data-warn') === '2'){
               Shiny.setInputValue('errInput', {
                inputId : '[inputId]',
                callId : Math.random()
              });
              $(this).attr('data-warn', '0');
            }
            if ($(this).val() == '') $(this).attr('data-warn', '0');
          });
          ",
          .open = '[', .close = ']'
        ))
      )
    }
  )
}

ui <- f7Page(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    text = "
    shinyjs.sendBackFirstTab = () => $('.toolbar-inner > :first-child')[0].click();
  ", functions = c("sendBackFirstTab")),
  shinyjs::extendShinyjs(
    text = "shinyjs.setInput = (input) => Shiny.setInputValue(input[0], input[1]);",
    functions = "setInput"
  ),
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
                               description = 'Patient\'s Age in years',
                               warn.validator = 'x => x < 18 || x > 100 ? 1 : 0'),
            shiny.fluent::Toggle.shinyInput(inputId = 'hiv_status',
                                            label = 'HIV Status*',
                                            offText = 'Confirmed HIV negative',
                                            onText = 'Confirmed HIV positive'),
            shiny.fluent::Toggle.shinyInput(inputId = 'clin_contact_tb',
                                            label = 'Past TB Contact*',
                                            offText = 'No known contact',
                                            onText = 'Have known contact')
          ),
          f7Block(
            f7BlockTitle('Clinical Symptoms'),
            shiny.fluent::Toggle.shinyInput(inputId = 'clin_symptoms',
                                            label = 'General TB Syndrome'),
            shiny.fluent::Toggle.shinyInput(inputId = 'clin_motor_palsy',
                                            label = 'Local neuro-deficit*'),
            shiny.fluent::Toggle.shinyInput(inputId = 'clin_nerve_palsy',
                                            label = 'Cranial nerve palsy'),
            fluentNumericInput(inputId = 'clin_gcs',
                               label = 'Glasgow Coma Score',
                               placeholder = '3-15',
                               warn.validator = 'x => x < 3 || x > 15 ? 2 : 0'),
            fluentNumericInput(inputId = 'clin_illness_day',
                               label = 'Days from first symptoms*',
                               placeholder = '(days)',
                               description = 'Number of days from first symptoms to admission',
                               warn.validator = 'x => x > 30 ? 1 : 0'),
          ),
          f7Block(
            f7BlockTitle('X-Ray Signs'),
            shiny.fluent::Toggle.shinyInput(inputId = 'xray_pul_tb',
                                            label = 'Pulmonary non-miliary TB*'),
            shiny.fluent::Toggle.shinyInput(inputId = 'xray_mil_tb',
                                            label = 'Miliary TB*')
          ),
          f7Block(
            f7BlockTitle('Hematological Tests'),
            fluentNumericInput(inputId = 'csf_lympho',
                               label = 'CSF Lymphocyte Count*',
                               placeholder = '(cells/μL)',
                               warn.validator = 'x => x > 10000 ? 1 : 0'),
            fluentNumericInput(inputId = 'csf_neutro',
                               label = 'CSF Neutrophils Count*',
                               placeholder = '(cells/μL)',
                               warn.validator = 'x => x > 10000 ? 1 : 0'),
            fluentNumericInput(inputId = 'csf_eos',
                               label = 'CSF Eosinophils Count*',
                               placeholder = '(cells/μL)',
                               warn.validator = 'x => x > 1000 ? 1 : 0'),
            fluentNumericInput(inputId = 'csf_rbc',
                               label = 'CSF Red Cell Count*',
                               placeholder = '(cells/μL)')
          ),
          f7Block(
            f7BlockTitle('Biochemical Tests'),
            fluentNumericInput(inputId = 'csf_glucose',
                               label = 'CSF Glucose*',
                               placeholder = '(mmol/L)',
                               warn.validator = 'x => x > 20 ? 2 : x > 10 ? 1 : 0'),
            fluentNumericInput(inputId = 'bld_glucose',
                               label = 'Paired Blood Glucose*',
                               placeholder = '(mmol/L)',
                               warn.validator = 'x => x > 20 ? 2 : x > 10 ? 1 : 0'),
            fluentNumericInput(inputId = 'csf_protein',
                               label = 'CSF Protein*',
                               placeholder = '(g/L)',
                               warn.validator = 'x => x > 20 ? 2 : x > 10 ? 1 : 0'),
            fluentNumericInput(inputId = 'csf_lactate',
                               label = 'CSF Lactate*',
                               placeholder = '(mmol/L)',
                               warn.validator = 'x => x > 20 ? 2 : x > 10 ? 1 : 0')
          ),
          f7Block(
            f7BlockTitle('Others'),
            shiny.fluent::Toggle.shinyInput(inputId = 'crypto',
                                            label = 'Cryptococcal Meningitis',
                                            onText = 'Positive Antigen/Indian Ink',
                                            offText = 'Negative Antigen/Indian Ink'),
            f7BlockFooter('Model was not trained with yellow-flagged values and thus might not work correctly. Reg-flagged values are out of possible range and will be removed.')
          ),
        )
        
        
      ),
      # Results ----
      f7Tab(
        tabName = "Diagnosis",
        icon = f7Icon("graph_circle"),
        active = FALSE,
        uiOutput('modelSelector'),
        
        div(
          f7Block(shiny.fluent::PrimaryButton.shinyInput(inputId = 'submit', text = 'Run')),
          style = 'width:400px'
        ),
        # div(style="clear:both"),
        
        tags$head(
          tags$style(sass::sass(sass::sass_file('www/css/main.scss'))),
          tags$script(src='js/main.js')
        ),
        
        
        f7Card(
          title = "Chance of TBM",
          uiOutput('theta_text'),
          shinycssloaders::withSpinner(plotOutput('theta_areasPlot'))
        ),
        
        
        f7Card(
          title = "Average Bacillary Burden (given TBM Positve)",
          uiOutput('re_text'),
          shinycssloaders::withSpinner(plotOutput('re_areasPlot'))
        ),
        
        
        f7Block(
          f7BlockHeader('Chances of postive confirmation tests'),
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