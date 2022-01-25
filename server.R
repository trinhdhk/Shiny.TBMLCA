library(shiny)
library(ggplot2)
library(thematic)
ggplot2::theme_set(ggplot2::theme_bw())
thematic_shiny()

misc = new.env()
source('R/misc.R', local=misc)
server = function(input, output, session) {
  # shinyjs::js$disableDiag()
  output$data_dict = renderTable(
    tibble::tribble(
      ~ Variable, ~ Meaning,
      "hiv"     , "Is the patient tested positive with HIV or on Anti-retroviral therapy?",
      "contact_tb", "Any past noticeable contact with individual who was then diagnosed with TB in the past 12 months?",
      "tb_symptoms", "Does the patient have either: weight loss, night sweating, or coughing > 2 weeks>?",
      "focal_neuro_deficit", "Does the patient have any focal neurological deficit?",
      "cranial_nerve_palsy", "Does the patient have any other cranial nerve palsy (expect for the focal neurological deficit)?",
      "gcs", "Glasgow Coma Score",
      "illness_day", "Days from first symptoms (days)",
      'xray_pul_tb', 'Pulmonary non-miliary TB (excluding Miliary TB)',
      'xray_mil_tb', 'Miliary TB',
      'csf_wbc', 'CSF White cell count (cells/μL)',
      'csf_lym', 'CSF Lymphocyte count (cells/μL)',
      'csf_eos', 'CSF Eosinophils count (cells/μL)', 
      'csf_rbc', 'CSF Red Cell count (cells/μL)',
      'csf_glu', 'CSF Glucose (mmol/L)',
      'bld_glu', 'Paired Blood Glucose (mmol/L)',
      'csf_pro', 'CSF Protein (g/dL)',
      'csf_lac', 'CSF Lactate (mmol/L)',
      'cryptococ', 'Cryptococcal Meningitis with Antigen or Indian Ink',
      'gram', 'Other bacterial infection, with Gram stain',
      'smear', 'ZN Smear test result (left blank if not retrieved yet)',
      'mgit', 'MGIT culture result (left blank if not retrieved yet)',
      'xpert', 'GeneXpert result (left blank if not retrieved yet)'
    )
  )
  output$template = downloadHandler(
    filename = 'template.csv',
    content = function(file) {
      dt = cbind(misc$template_dt, data.frame(smear=vector(), mgit=vector(), xpert=vector()))
      write.csv(dt, file, row.names = FALSE)
    },
    contentType = 'text/csv'
  )
  
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
  
  # model_files <- gsub('.RDS$', '', list.files('fits'))
  # full_models <- grep('full', model_files, value = TRUE)
  # simplified_models <- grep('simplified', model_files, value = TRUE)
  # Get model

 
  # params <- reactiveVal()
  # predicts <- reactiveValues()
  observeEvent(c(input$submit, input$custom_data), {
    
   
    `%|%` <- \(L, R) if (isTRUE(is.null(L) || is.na(L))) R else L
    # browser()
    data = misc$create_data(input, session)
    if (!is.data.frame(data)) return(1)
    mode = list(misc$create_recipe(data, input, session), nrow(data) == 1)
    if (mode[[2]] == 1) shinyjs::js$enableDiag() else shinyjs::js$disableDiag()
    if (mode[[1]] == ''){
      # shinyWidgets::sendSweetAlert(
      #   session, 
      #   "Error", 
      #   text="Some mandatory predictors might be missing or invalid.", 
      #   type="error"
      # )
      f7Dialog(
        id = 'missing-var',
        title = 'Missing essential features',
        type = 'alert',
        text = 'Some essential variables are missing.'
      )
      shinyjs::js$sendBackFirstTab()
      
    } else
    params = 
      if (mode[[1]] == 'full')
        list(
          a = read.csv('fits/m3/a.csv', check.names = FALSE) |> as.matrix(),
          b = read.csv('fits/m3/b.csv', check.names = FALSE) |> as.matrix(),
          z = read.csv('fits/m3/z.csv', check.names = FALSE) |> as.matrix(),
          scales = jsonlite::read_json('fits/m3/scales.json', simplifyVector = TRUE)
        )
      else 
        list(
          a = read.csv('fits/s1/a.csv')
        )
    if (mode[[1]] == 'full'){
      # browser()
      if (mode[[2]]==1) {
        # print(data[1,])
        if (any(is.na(data[1,]))){
          f7Dialog(
            id = 'missing-var',
            title = 'Missing essential features',
            type = 'alert',
            text = 'Some essential variables are missing.'
          )
          shinyjs::js$sendBackFirstTab()
    
        } else {
          # browser()
          X = misc$create_model_matrix(misc$rescale_data(data, params$scales))
          
          bvars = c(3,4,5,6,7,8) + 9 + 1 #nXc=9, intercept=1
          
          ztheta = params$a %*% t(X)
          theta = plogis(ztheta)
          bacillary = 
            as.matrix(params$b[,'b_HIV', drop=FALSE]) %*% t(X[,2, drop=FALSE]) +
            params$b[,paste0('b[',1:6,']'), drop=FALSE] %*% t(X[,bvars, drop=FALSE])
          
          fpr = cbind(
            plogis(params$z[,'z_Smear[1]', drop=FALSE]),
            plogis(params$z[,'z_Mgit[1]', drop=FALSE]),
            plogis(params$z[,'z_Xpert[1]', drop=FALSE])
          )
          tpr = cbind(
            plogis(params$z[,'z_Smear[2]', drop=FALSE] + params$b[,'b_RE[1]'] * bacillary),
            plogis(params$z[,'z_Mgit[2]',  drop=FALSE] + params$b[,'b_RE[2]'] * bacillary),
            plogis(params$z[,'z_Xpert[2]', drop=FALSE] + params$b[,'b_RE[3]'] * bacillary)
          )
          scenarios = list(
            a = misc$test_prob(theta, fpr[,1, drop=FALSE], tpr[,1, drop=FALSE], neg=TRUE),
            b = misc$test_prob(theta, fpr[,3, drop=FALSE], tpr[,3, drop=FALSE], neg=TRUE),
            c = misc$test_prob(theta, fpr[,1:2], tpr[,1:2], neg=TRUE),
            d = misc$test_prob(theta, fpr[,c(1,3)], tpr[,c(1,3)], neg=TRUE),
            e = misc$test_prob(theta, fpr, tpr, neg=TRUE)
          )
          
          # if scenario == 0
          # theta   = theta
          
          if (input$scenario != '0'){
            theta_orig = theta
            scenario = scenarios[[input$scenario]]
            theta = theta * scenario$tpr / scenario$prob
          }
          
          p_Smear = (as.matrix(plogis(params$z[,'z_Smear[1]', drop=FALSE])*(1-(theta)) + plogis(params$z[,'z_Smear[2]', drop=FALSE] + params$b[,'b_RE[1]'] * bacillary)*(theta)))
          p_Mgit  = (as.matrix(plogis(params$z[,'z_Mgit[1]',  drop=FALSE])*(1-(theta)) + plogis(params$z[,'z_Mgit[2]',  drop=FALSE] + params$b[,'b_RE[2]'] * bacillary)*(theta)))
          p_Xpert = (as.matrix(plogis(params$z[,'z_Xpert[1]', drop=FALSE])*(1-(theta)) + plogis(params$z[,'z_Xpert[2]', drop=FALSE] + params$b[,'b_RE[3]'] * bacillary)*(theta)))
          
          plot_text <- function(x, fn=\(x) x, of){
            mean = mean(fn(x))
            lower.ci = quantile(fn(x),.025)
            upper.ci = quantile(fn(x),.975)
            renderUI(
              HTML(glue::glue('
          Estimated {of} (coloured vertical line) is: 
          {strong(format(mean,digits=3))},
          with 95% Credible Interval (solid area) is 
          [{strong(format(lower.ci, digits=3))}, {strong(format(upper.ci, digits=3))}]')
              ))
          }
          colnames(theta) = "Probability"
          colnames(ztheta) = "Score"
          predicts$theta = theta
          
          # Theta ----
          output$theta_text = plot_text(theta, of = 'probability of TBM')
          output$theta_areasPlot = renderPlot({
            q <- quantile(theta, c(.025, .25, .75, .975))
            q <- round(c(q, mean(theta)), digits=4)
            l <- format(round((q),digits=4), scientific=FALSE,  drop0trailing=TRUE)
            # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
            bayesplot::mcmc_areas(theta, prob = .95, prob_outer = .995,  point_est = 'mean') + 
              scale_x_continuous(breaks=q, label=l) + 
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          })
          
          colnames(bacillary) = "Bacillary Burden"
          predicts$bacillary = bacillary
          colnames(p_Smear) <- colnames(p_Mgit) <- colnames(p_Xpert) <- 'Probability'
          predicts$p_Smear = p_Smear
          predicts$p_Mgit = p_Mgit
          predicts$p_Xpert = p_Xpert
          
          # RE
          output$re_text = plot_text(bacillary, fn = c, of = 'average bacillary burden')
          output$re_areasPlot = renderPlot(
            bayesplot::mcmc_areas(bacillary, prob=.95,  prob_outer = .995, point_est = 'mean') + 
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          )
          
          output$test_text = renderText("Below plots show estimation of each confimation test's chance of positive.")
          # Tests
          output$smear_text = plot_text(p_Smear, of = 'probability of positive Smear')
          output$smear_areasPlot = renderPlot({
            q <- quantile(p_Smear, c(.005, .025, .975, .995))
            q <- round(c(q, mean(p_Smear)), digits=4)
            l <- format(round((q),digits=4), scientific=FALSE,  drop0trailing=TRUE)
            # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
            bayesplot::mcmc_areas(p_Smear, prob = .95, prob_outer = .995,  point_est = 'mean') + 
              scale_x_continuous(breaks=q, label=l)+ 
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          })
          output$mgit_text = plot_text(p_Mgit, of = 'probability of positive Mgit')
          output$mgit_areasPlot = renderPlot({
            q <- quantile(p_Mgit, c(.005, .025, .975, .995))
            q <- round(c(q, mean(p_Mgit)), digits=4)
            l <- format(round((q),digits=4), scientific=FALSE,  drop0trailing=TRUE)
            # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
            bayesplot::mcmc_areas(p_Mgit, prob = .95, prob_outer = .995,  point_est = 'mean') + 
              scale_x_continuous(breaks=q, label=l)+ 
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          })
          output$xpert_text = plot_text(p_Xpert, of = 'probability of positive GeneXpert')
          output$xpert_areasPlot = renderPlot({
            q <- quantile(p_Xpert, c(.005, .025, .25, .75, .975, .995))
            q <- round(c(q, mean(p_Xpert)), digits=3)
            l <- format(round(plogis(q),digits=3), scientific=FALSE,  drop0trailing=TRUE)
            # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
            bayesplot::mcmc_areas(p_Xpert, prob = .95, prob_outer = .995,  point_est = 'mean') + 
              scale_x_continuous(breaks=q, label=l)+ 
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          })
          
        } 
      } else {
        
        # browser()
        X = misc$create_model_matrix(misc$rescale_data(data, params$scales))
        bvars = c(3,4,5,6,7,8) + 9 + 1 #nXc=9, intercept=1
        ztheta = params$a %*% t(X)
        theta = plogis(ztheta)
        bacillary = 
          as.matrix(params$b[,'b_HIV', drop=FALSE]) %*% t(X[,2, drop=FALSE]) +
          params$b[,paste0('b[',1:6,']'), drop=FALSE] %*% t(X[,bvars, drop=FALSE])
        
        p_Smear_neg_all = plogis(params$z[,'z_Smear[1]', drop=FALSE])
        p_Mgit_neg_all = plogis(params$z[,'z_Mgit[1]', drop=FALSE])
        p_Xpert_neg_all = plogis(params$z[,'z_Xpert[1]', drop=FALSE])
        
        p_Smear <- p_Mgit <- p_Xpert <- matrix(nrow=nrow(theta), ncol=ncol(theta))
        for (n in seq_len(nrow(data))){
          # browser()
          p_Smear_pos = misc$test_prob2(data[n, 'smear'], plogis(params$z[,'z_Smear[2]', drop=FALSE] + params$b[,'b_RE[1]'] * bacillary[,n]))
          p_Mgit_pos = misc$test_prob2(data[n, 'mgit'],   plogis(params$z[,'z_Mgit[2]', drop=FALSE] + params$b[,'b_RE[2]'] * bacillary[,n]))
          p_Xpert_pos = misc$test_prob2(data[n, 'xpert'], plogis(params$z[,'z_Xpert[2]', drop=FALSE] + params$b[,'b_RE[3]'] * bacillary[,n]))
          
          p_Smear_neg = misc$test_prob2(data[n, 'smear'], p_Smear_neg_all)
          p_Mgit_neg  = misc$test_prob2(data[n, 'mgit'],   p_Mgit_neg_all)
          p_Xpert_neg = misc$test_prob2(data[n, 'xpert'], p_Xpert_neg_all)
          
          # browser()
          theta[,n] = theta[,n] * (p_Smear_pos * p_Mgit_pos * p_Xpert_pos) / (theta[,n] * (p_Smear_pos * p_Mgit_pos * p_Xpert_pos) + (1-theta[,n]) * (p_Smear_neg * p_Mgit_neg * p_Xpert_neg))
          p_Smear[,n] = (as.matrix(plogis(params$z[,'z_Smear[1]', drop=FALSE])*(1-(theta[,n])) + plogis(params$z[,'z_Smear[2]', drop=FALSE] + params$b[,'b_RE[1]'] * bacillary[,n])*(theta[,n])))
          p_Mgit[,n]  = (as.matrix(plogis(params$z[,'z_Mgit[1]',  drop=FALSE])*(1-(theta[,n])) + plogis(params$z[,'z_Mgit[2]',  drop=FALSE] + params$b[,'b_RE[2]'] * bacillary[,n])*(theta[,n])))
          p_Xpert[,n] = (as.matrix(plogis(params$z[,'z_Xpert[1]', drop=FALSE])*(1-(theta[,n])) + plogis(params$z[,'z_Xpert[2]', drop=FALSE] + params$b[,'b_RE[3]'] * bacillary[,n])*(theta[,n])))
        }
        #-----
        # browser()
        sample_array = list(theta, bacillary, p_Smear, p_Mgit, p_Xpert)
        names(sample_array) = c('p_tbm', 'bacillary_burden', 'p_Smear', 'p_Mgit', 'p_Xpert')
        
        sample_summary =
          sapply(sample_array, function(arr) {
            data.frame(
              mean = apply(arr, 2, mean),
              median = apply(arr, 2, quantile, .5),
              lower.ci=apply(arr,2,quantile, .25),
              upper.ci=apply(arr,2,quantile, .25)
              )
          }, simplify = FALSE)
        output$sample_array = downloadHandler(
          filename = 'posterior_est.json',
          content =  function(file) {
            dt = sample_array
            jsonlite::write_json(dt, file)
          },
          contentType = 'text/json'
        )
        output$sample_summary = downloadHandler(
          filename = 'posterior_summary.json',
          content = function(file) {
            dt = sample_summary
            jsonlite::write_json(dt, file)
          },
          contentType = 'text/json'
        )
        
        output$download_results = renderUI(
          f7Flex(
            f7Block(
              f7DownloadButton('sample_array',
                                     label = 'Download posterior samples'),
              f7BlockFooter("Sample file contains MCMC samples which work best for future model.")),
            f7Block(
              f7DownloadButton('sample_summary',
                               label = 'Download posterior summary'),
              f7BlockFooter("Summary file contains point estimates and credible intervals which work best for result report."))
          )
        )
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
