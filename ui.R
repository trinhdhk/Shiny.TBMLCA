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
  shinyjs::extendShinyjs(
    text = "shinyjs.disableDiag = () => {
    $($('.toolbar-inner a')[1]).addClass('disabled');
    $($('#welcome-page ul li a.item-link')[0]).addClass('disabled');
    }",
    functions = "disableDiag"
  ),
  shinyjs::extendShinyjs(
    text = "shinyjs.enableDiag = () => {
    $($('.toolbar-inner a')[1]).removeClass('disabled');
    $($('#welcome-page ul li a.item-link')[0]).removeClass('disabled');
    }",
    functions = "enableDiag"
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
        p("You can choose to upload your database on calculate TBM risk case-wise.", 
          style = 'padding-left: 20px'),
        p("The Diagnosis tab is only accessible with case-wise mode.",
          style = 'padding-left: 20px'),
        f7Accordion(
          id = "welcome-page",
          f7AccordionItem(
            title = "I want to calculate the risk for one patient",
            # open = TRUE,
            f7Flex(
              f7Block(
                f7BlockTitle('Patient profile'),
                shiny.fluent::Toggle.shinyInput(inputId = 'hiv',
                                                label = 'Is the patient tested positive with HIV or on Anti-retroviral therapy?',
                                                offText = 'Confirmed HIV negative',
                                                onText = 'Confirmed HIV positive'),
                shiny.fluent::Toggle.shinyInput(inputId = 'contact_tb',
                                                label = 'Any past noticeable contact with individual who was then diagnosed with TB in the past 12 months?',
                                                offText = 'No known contact',
                                                onText = 'Have known contact')
              ),
              f7Block(
                f7BlockTitle('Clinical Symptoms'),
                shiny.fluent::Toggle.shinyInput(inputId = 'tb_symptoms',
                                                label = 'Does the patient have either: weight loss, night sweating, or coughing > 2 weeks>?',
                                                offText = 'No',
                                                onText = 'Yes'),
                shiny.fluent::Toggle.shinyInput(inputId = 'focal_neuro_deficit',
                                                label = 'Does the patient have any focal neurological deficit?',
                                                offText = 'No',
                                                onText = 'Yes'),
                shiny.fluent::Toggle.shinyInput(inputId = 'cranial_nerve_palsy',
                                                label = 'Does the patient have any other cranial nerve palsy (expect for the focal neurological deficit)?',
                                                offText = 'No',
                                                onText = 'Yes'),
                misc$fluentNumericInput(inputId = 'gcs',
                                        label = 'Glasgow Coma Score',
                                        placeholder = '3-15',
                                        warn.validator = 'x => x < 3 || x > 15 ? 2 : 0'),
                misc$fluentNumericInput(inputId = 'illness_day',
                                        label = 'Days from first symptoms',
                                        placeholder = '(days)',
                                        description = 'Number of days from first symptoms to admission',
                                        warn.validator = 'x => x > 30 ? 1 : 0')
              ),
              f7Block(
                f7BlockTitle('X-Ray Signs'),
                shiny.fluent::Toggle.shinyInput(inputId = 'xray_pul_tb',
                                                label = 'Pulmonary non-miliary TB (excluding Miliary TB)'),
                shiny.fluent::Toggle.shinyInput(inputId = 'xray_mil_tb',
                                                label = 'Miliary TB')
              ),
              f7Block(
                f7BlockTitle('Hematological Tests'),
                f7BlockHeader('All features must be corrected for traumatic lumbar puncture'),
                fluentNumericInput(inputId = 'csf_wbc',
                                   label = 'CSF White cell count',
                                   placeholder = '(cells/μL)',
                                   warn.validator = 'x => x > 10000 ? 1 : 0'),
                fluentNumericInput(inputId = 'csf_lym',
                                   label = 'CSF Lymphocyte count',
                                   placeholder = '(cells/μL)',
                                   warn.validator = 'x => x > 10000 ? 1 : 0'),
                fluentNumericInput(inputId = 'csf_eos',
                                   label = 'CSF Eosinophils count',
                                   placeholder = '(cells/μL)',
                                   warn.validator = 'x => x > 1000 ? 1 : 0'),
                fluentNumericInput(inputId = 'csf_rbc',
                                   label = 'CSF Red Cell count',
                                   placeholder = '(cells/μL)')
              ),
              f7Block(
                f7BlockTitle('Biochemical Tests'),
                f7BlockHeader('All features must be corrected for traumatic lumbar puncture'),
                fluentNumericInput(inputId = 'csf_glu',
                                   label = 'CSF Glucose',
                                   placeholder = '(mmol/L)',
                                   warn.validator = 'x => x > 20 ? 2 : x > 10 ? 1 : 0'),
                fluentNumericInput(inputId = 'bld_glu',
                                   label = 'Paired Blood Glucose',
                                   placeholder = '(mmol/L)',
                                   warn.validator = 'x => x > 20 ? 2 : x > 10 ? 1 : 0'),
                fluentNumericInput(inputId = 'csf_pro',
                                   label = 'CSF Protein',
                                   placeholder = '(g/dL)',
                                   warn.validator = 'x => x > 20 ? 2 : x > 10 ? 1 : 0'),
                fluentNumericInput(inputId = 'csf_lac',
                                   label = 'CSF Lactate',
                                   placeholder = '(mmol/L)',
                                   warn.validator = 'x => x > 20 ? 2 : x > 10 ? 1 : 0')
              ),
              f7Block(
                f7BlockTitle('Was the patient diagnosed with other?'),
                shiny.fluent::Toggle.shinyInput(inputId = 'cryptococ',
                                                label = 'Cryptococcal Meningitis with Antigen or Indian Ink',
                                                onText = 'Yes',
                                                offText = 'No'),
                shiny.fluent::Toggle.shinyInput(inputId = 'gram',
                                                label = 'Other bacterial infection, with Gram stain',
                                                onText = 'Yes',
                                                offText = 'No'),
                f7BlockFooter('Model was not trained with yellow-flagged values and thus might not work correctly. Reg-flagged values are out of possible range and will be removed.')
              ),
            )
          ),
          f7AccordionItem(
            title = "I want to calculate TBM risk for a list of patients in my data",
            p("In this mode you will download the template, fill your data and reupload it.", style = 'padding-left: 20px'),
            p(strong("1. First download the template and scroll down for the dictionary"), style = 'padding-left: 20px'),
            f7Block(f7DownloadButton('template', 'Download template'), 
                    style='height=100%'),
            div( tableOutput('data_dict'),
                 style = "height: 150px; overflow-y: auto; margin-left: 5px; margin-right:5px"),
            p(" - Note that only smear, mgit, and xpert are allowed to be missing and all CSF biomarkers should be corrected for traumatic lumbar puncture (although CSF Red cell count is still needed).",
               style = 'padding-left: 20px'),
            p(strong("2. Upload dataset, make sure it is from the template."),
              style = 'padding-left: 20px; margin-top:10px'),
            f7Block(
              f7File(inputId = 'custom_data',
                     label = NULL,
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
              ),
              style = 'margin-top:5px'
            ),
            p(strong("3. The output will appear below when done."), style = 'padding-left: 20px; margin-top:10px'),
            
            uiOutput('download_results')
          )
          
        )
        
       
        
        
      ),
      # Results ----
      f7Tab(
        tabName = "Diagnosis",
        icon = f7Icon("graph_circle"),
        active = FALSE,
        div(
          "You can select a scenario in which some confirmatory test results have been retrieved.",
          "All probabilities of test positive are for future tests",
          style = "float:left; width: calc(100% - 500px); padding: 30px 10px 10px 30px"
        ),
        
        div(
          f7SmartSelect(
            inputId = 'scenario',
            label = "Current scenario",
            selected = "No test",
            choices = list(
              "No test" = '0',
              "Smear (-)" = 'a',
              "Xpert (-)" = 'b',
              "Smear (-) + MGIT (-)" = 'c',
              "Smear (-) + Xpert (-)" = 'd',
              "All tests (-)" = 'e'
              # `0` = "No test",
              # `1` = "Smear (-)",
              # `2` = "Xpert (-)",
              # `3` = "Smear (-), MGIT (-)",
              # `4` = "Smear (-), Xpert (-)",
              # `5` = "All tests (-)"
            ),
            openIn='popover',
            searchbar=FALSE
          ),
          f7Block(shiny.fluent::PrimaryButton.shinyInput(inputId = 'submit', text = 'Re-estimate with scenario')),
          style = 'width:400px; float:right'
        ),
        div(style="clear:both; width:100%"),
        
       
        # div(style="clear:both"),
        
        tags$head(
          tags$style(sass::sass(sass::sass_file('www/css/main.scss'))),
          tags$script(src='js/main.js')
        ),
        
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