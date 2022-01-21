library(shiny)
library(ggplot2)
library(thematic)
ggplot2::theme_set(ggplot2::theme_bw())
thematic_shiny()

server = function(input, output, session) {
  # Invalid values clerance
  observeEvent(input$errInput, {
    shiny.fluent::updateTextField.shinyInput(inputId = input$errInput$inputId, value = '')
    # updateTextInput(inputId='errInput', value=NULL)
  })
  # Color-scheme aware
  observeEvent(input$color_scheme,{
    file <- paste0('www/assets/oucru_', input$color_scheme)
    output$oucru_logo <- renderUI(tags$a(href = 'https://www.oucru.org', target = '_blank', tags$img(alt = 'OUCRU logo', style = 'max-width: 250px; border-radius:10px; border: 1px solid #fff', src = paste('data:image/jpeg;base64,', readLines(file, warn=FALSE)))))
    plot_theme <- switch(input$color_scheme, light = theme_bw, dark = theme_dark)
    bayesplot::bayesplot_theme_set(plot_theme())
    
    # bayesplot::bayesplot_theme_update(
    # )
    if (input$color_scheme=='dark') {
      bayesplot::color_scheme_set("brightblue")
      bayesplot::bayesplot_theme_update(
        plot.background = element_rect(fill='transparent',color='transparent'),
        panel.background = element_rect(fill = "transparent"),
        axis.text = element_text(color = 'white',size = 15,family = 'sans'))
    } else{
      bayesplot::color_scheme_set("blue")
      bayesplot::bayesplot_theme_update(
        plot.background = element_rect(fill='transparent',color='transparent'),
        panel.background = element_rect(fill = "transparent"),
        axis.text = element_text(color = 'black',size = 15,family = 'sans'))
    }
  })
  
  model_files <- gsub('.RDS$', '', list.files('fits'))
  full_models <- grep('full', model_files, value = TRUE)
  simplified_models <- grep('simplified', model_files, value = TRUE)
  # Get model
  output$modelSelector <- 
    renderUI(
      f7Select(
        inputId = "Model",
        label = "Model choice", 
        width = '400px',
        choices = model_files,
        selected = 'Model 8 (b58) full'
      )
    )

  params <- reactiveVal()
  predicts <- reactiveValues()
  observeEvent(input$submit, {
   
    `%|%` <- \(L, R) if (isTRUE(is.null(L) || is.na(L))) R else L
    
    optionalPredictors <- list(
      age = (log2(as.numeric(input$age)) - 5.2632) %|% 0,
      clin_symptoms = as.numeric(input$clin_symptoms) %|% 0,
      clin_nerve_palsy = as.numeric(input$clin_nerve_palsy) %|% 0,
      clin_gcs = (15 - as.numeric(input$clin_gcs)) %|% 0 
    )
    tryCatch(
      X <- rbind(
        input$hiv_status,
        optionalPredictors$clin_symptoms,
        input$clin_motor_palsy,
        optionalPredictors$clin_nerve_palsy,
        input$clin_contact_tb,
        input$xray_pul_tb,
        input$xray_mil_tb,
        input$crypto,
        optionalPredictors$age,
        log2(as.numeric(input$clin_illness_day)),
        log10(as.numeric(input$bld_glucose)) - .8233,
        log10(as.numeric(input$csf_glucose)+1) - .5845,
        log10(as.numeric(input$csf_lympho)+1) - 1.7791,
        log10(as.numeric(input$csf_protein)) - .0160,
        log10(as.numeric(input$csf_lactate)) - .6072,
        log10(as.numeric(input$csf_neutro)+1) - 1.6306,
        optionalPredictors$clin_gcs,
        log10(as.numeric(input$csf_eos)+1) - .0747,
        log10(as.numeric(input$csf_rbc)+1) - 1.4402,
        (log10(as.numeric(input$csf_neutro)+1) - 1.6306)^2
      ),
      error = function(err) shinyWidgets::sendSweetAlert(session, 'Error', text=err, type='error')
    )
    # browser()
    forcedSimplified <- any(sapply(list(input$crypto, 
                                   input$bld_glucose, input$csf_glucose,
                                   input$csf_lympho, input$csf_protein, 
                                   input$csf_lactate, input$csf_neutro,
                                   input$csf_eos, input$csf_rbc), 
                                   function(x) {
                                     is.na(as.numeric(x))
                                   }))
    if (forcedSimplified){
      browser()
      shinyMobile::updateF7Select(session = session,
                                  inputId = 'Model',
                                  selected = 'Model 8 (b58) simplified'
      )
      shinyjs::js$setInput(list('Model', 'Model 8 (b58) simplified'))
      params(readRDS(file.path('fits', paste0(input$Model, '.RDS')))$coef)
    }
      
    else{
      shinyMobile::updateF7Select(session = session,
                                  inputId = 'Model',
                                  selected = 'Model 8 (b58) full'
      )
      shinyjs::js$setInput(list('Model', 'Model 8 (b58) full'))
      params(readRDS(file.path('fits', paste0(input$Model, '.RDS')))$coef)
    }
      
     #TODO select model. now default
    if (input$Model %in% simplified_models){
      theta = params()$d %*% X[c(1,2,3,4,5,6,7,9,10,17)] + as.matrix(params()$d0)
    } else {
      theta = params()$a %*% X + as.matrix(params()$a0)
      bvars = 8 + switch(
        input$Model,
        'Model m8_b568' = c(5, 6, 8),
        'Model m8_b4568' = c(4, 5, 6, 8))
      bacillary = 
        as.matrix(params()$b_HIV) * X[1] +
        as.matrix(params()$b_cs) * X[2] + 
        params()$b %*% X[bvars, drop=FALSE]
      p_Smear = qlogis(as.matrix(plogis(params()$z_Smear[,1, drop=FALSE])*(1-plogis(theta)) + plogis(params()$z_Smear[,2, drop=FALSE] + params()$b_RE[,1] * bacillary)*plogis(theta)))
      p_Mgit= qlogis(as.matrix(plogis(params()$z_Mgit[,1, drop=FALSE])*(1-plogis(theta)) + plogis(params()$z_Mgit[,2, drop=FALSE] + params()$b_RE[,2] * bacillary)*plogis(theta)))
      p_Xpert = qlogis(as.matrix(plogis(params()$z_Xpert[,1, drop=FALSE])*(1-plogis(theta)) + plogis(params()$z_Xpert[,2, drop=FALSE] + params()$b_RE[,3] * bacillary)*plogis(theta)))
      # browser()
    }
    
    if (any(is.na(theta))){
      shinyWidgets::sendSweetAlert(
        session, 
        "Error", 
        text="Some mandatory predictors might be missing or invalid.", 
        type="error"
      )
      shinyjs::js$sendBackFirstTab()
    } else {
      plot_text <- function(x, fn=plogis, of){
        mean = fn(mean(x))
        lower.ci = fn(quantile(x, .025))
        upper.ci = fn(quantile(x, .975))
        renderUI(
          HTML(glue::glue('
          Estimated {of} (coloured vertical line) is: 
          {strong(format(mean,digits=3))},
          with 95% Credible Interval (solid area) is 
          [{strong(format(lower.ci, digits=3))}, {strong(format(upper.ci, digits=3))}]')
          ))
      }
      colnames(theta) = "Probability"
      predicts$theta = theta
      
      # Theta ----
      output$theta_text = plot_text(theta, of = 'probability of having TBM')
      output$theta_areasPlot = renderPlot({
        q <- quantile(theta, c(.005, .025, .25, .75, .975, .995))
        q <- round(c(q, mean(theta)), digits=3)
        l <- format(round(plogis(q),digits=3), scientific=FALSE)
        # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
        bayesplot::mcmc_areas(theta, prob = .95, prob_outer = .995,  point_est = 'mean') + 
          scale_x_continuous(breaks=q, label=l)
      })
      
      if (input$Model %in% simplified_models){
        output$re_text <- output$test_text <- renderUI(p('Not supported in simplified model'))
      } else {
        colnames(bacillary) = "Bacillary Burden"
        predicts$bacillary = bacillary
        colnames(p_Smear) <- colnames(p_Mgit) <- colnames(p_Xpert) <- 'Probability'
        predicts$p_Smear = p_Smear
        predicts$p_Mgit = p_Mgit
        predicts$p_Xpert = p_Xpert
        
        # RE
        output$re_text = plot_text(bacillary, fn = c, of = 'average bacillary burden')
        output$re_areasPlot = renderPlot(
          bayesplot::mcmc_areas(bacillary, prob=.95,  prob_outer = .995, point_est = 'mean')
        )
        
        output$test_text = renderText("Below plots show estimation of each confimation test's chance of positive.")
        # Tests
        output$smear_text = plot_text(p_Smear, of = 'probability of positive Smear')
        output$smear_areasPlot = renderPlot({
          q <- quantile(p_Smear, c(.005, .025, .25, .75, .975, .995))
          q <- round(c(q, mean(p_Smear)), digits=3)
          l <- format(round(plogis(q),digits=3), scientific=FALSE)
          # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
          bayesplot::mcmc_areas(p_Smear, prob = .95, prob_outer = .995,  point_est = 'mean') + 
            scale_x_continuous(breaks=q, label=l)
        })
        output$mgit_text = plot_text(p_Mgit, of = 'probability of positive Mgit')
        output$mgit_areasPlot = renderPlot({
          q <- quantile(p_Mgit, c(.005, .025, .25, .75, .975, .995))
          q <- round(c(q, mean(p_Mgit)), digits=3)
          l <- format(round(plogis(q),digits=3), scientific=FALSE)
          # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
          bayesplot::mcmc_areas(p_Mgit, prob = .95, prob_outer = .995,  point_est = 'mean') + 
            scale_x_continuous(breaks=q, label=l)
        })
        output$xpert_text = plot_text(p_Xpert, of = 'probability of positive GeneXpert')
        output$xpert_areasPlot = renderPlot({
          q <- quantile(p_Xpert, c(.005, .025, .25, .75, .975, .995))
          q <- round(c(q, mean(p_Xpert)), digits=3)
          l <- format(round(plogis(q),digits=3), scientific=FALSE)
          # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
          bayesplot::mcmc_areas(p_Xpert, prob = .95, prob_outer = .995,  point_est = 'mean') + 
            scale_x_continuous(breaks=q, label=l)
        })
        
        # output$tests_areasPlot = renderPlot(
        #   bayesplot::mcmc_areas(p_Smear, prob=.95, prob_outer = .995, point_est = 'mean') + ggtitle('Smear') +
        #     bayesplot::mcmc_areas(p_Mgit, prob=.95, point_est = 'mean') + ggtitle('Mgit') + 
        #     bayesplot::mcmc_areas(p_Xpert, prob=.95, point_est = 'mean') + ggtitle('Xpert')
        # )
      }
     
    }
  })
  
  # send the theme to javascript
  observe({
    session$sendCustomMessage(
      type = "ui-tweak",
      message = list(os = input$theme, skin = input$color)
    )
  })
  
}
