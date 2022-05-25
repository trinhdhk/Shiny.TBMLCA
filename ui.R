library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(apexcharter)
options('spinner.type' = 8)
tabs <- new.env()
for (tab in list.files(file.path('R', 'tabs'), '.R', full.names=TRUE)) source(tab, local=tabs)

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
  title = "TBM-LCA Dx App",
  options = list(dark = TRUE, iosTranslucentBars = TRUE),
  # allowPWA = TRUE,
  tags$head(
    tags$link(rel="icon", type="image/x-icon", href='assets/hex.svg'),
    tags$link(rel='manifest', href='manifest.json'),
    tags$script(
      type = 'module',
      src = 'js/sw-register.js'
    ),
    # tags$link(rel="apple-touch-icon", type="image/png", sizes="180x180", href=paste('data:image/png;base64,', readLines('www/assets/apple-touch-icon', warn=FALSE))),
    tags$link(rel="apple-touch-icon", type="image/png", sizes="180x180", href='assets/tbmlca_hex.png'),
    tags$meta(name="apple-mobile-web-app-status-bar-style", content="black-translucent")
    # tags$meta(name="apple-mobile-web-app-capable",content="yes"),
    # tags$meta(name="viewport", content="viewport-fit=cover")

  ),
  tags$link(rel="preconnect",href="https://fonts.googleapis.com"),
  tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=TRUE),
  tags$link(href="https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@300;400&display=swap",rel="stylesheet"),
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
  shinyjs::extendShinyjs(
    text = "shinyjs.submit = () => $('button#submit').click();",
    functions = "submit"
  ),
  f7TabLayout(
    navbar = f7Navbar(

      title = div(img(src='assets/logo.svg', class="logo"),
                 span("TBM-LCA Diagnosis App"), style="height:100%; width:100%; user-select: none"),
      hairline = TRUE,
      shadow = FALSE,
      leftPanel = FALSE,
      rightPanel = FALSE
    ),
    f7Tabs(
      id = 'mainTabs',
      animated = TRUE,
      # Data ----
      tabs$onePatientTab,
      tabs$uploadDataTab,
      # Results ----
      tabs$resultTab,
      tabs$infoTab
    )
  )
)
