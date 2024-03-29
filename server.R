library(shiny)
library(ggplot2)
library(thematic)
ggplot2::theme_set(ggplot2::theme_bw())
thematic_shiny()

plot_text <- function(x, fn=\(x) x, of, ci=FALSE, pct = FALSE){
  factor = if (pct) 100 else 1
  digits = if (pct) 3 else 1
  unit = if (pct) '%' else ''
  mean = mean(fn(x)) * factor
  lower.ci = quantile(fn(x),.025) * factor
  upper.ci = quantile(fn(x),.975) * factor
  ci_text = if (ci) glue::glue(
    ', with 90% Credible Interval is [{strong(format(lower.ci, digits=digits))}, {strong(format(upper.ci, digits=digits))}] {unit}
    '
  ) else ''
  renderUI(
    span(HTML(glue::glue('
          Expected {of} is: 
          {strong(format(mean,digits=digits))} {unit} {ci_text}.')),
         class='click-through'
    ))
}

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
      'xray_pul_tb', 'Pulmonary non-miliary TB',
      'xray_mil_tb', 'Miliary TB',
      'csf_wbc', 'CSF White cell count (cells/μL)',
      'csf_lym', 'CSF Lymphocyte count (cells/μL)',
      'csf_eos', 'CSF Eosinophils count (cells/μL)', 
      'csf_rbc', 'CSF Red Cell count (cells/μL)',
      'csf_glu', 'CSF Glucose (mmol/L)',
      'bld_glu', 'Paired Blood Glucose (mmol/L)',
      'csf_pro', 'CSF Protein (g/L)',
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
  predicts <- reactiveValues()
  .last_val <- reactiveValues()
  .last_val$impute=0
  .last_val$impute2=0
  .last_val$scenario = '0'
  observeEvent(input$submit_home, {
    shinyMobile::updateF7Tabs('mainTabs', selected = 'Diagnosis', session=session)
    shinyjs::js$submit()
  })
  observeEvent(input$send_back, {
    shinyjs::js$sendBackFirstTab()
  })
  observeEvent(c(input$submit, input$submit2, input$custom_data), {
    # browser()
    # .last_val$scenario <- input$scenario
   
    # .last_val$impute <- NULL
    # shinyjs::js$setInput('missingConfirm', NULL)
    `%|%` <- \(L, R) if (isTRUE(is.null(L) || is.na(L))) R else L
    # browser()
    data = misc$create_data(input, session)
    if (!is.data.frame(data)) return(1)
   
    
    mode = list(misc$create_recipe(data, input, session), nrow(data) == 1)
    # browser()
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
          a = read.csv('fits/s1/a.csv', check.names = FALSE) |> as.matrix(),
          scales = jsonlite::read_json('fits/m3/scales.json', simplifyVector = TRUE)
        )
    if (mode[[1]] == 'full'){
      # browser()
      if (mode[[2]]==1) {
        # Legacy
        
        
        if (any(is.na(data[1,]))){
          if ((is.null(input$missingConfirm))){
            f7Dialog(
              id = 'missingConfirm',
              title = 'Missing some laboratory features',
              type = 'confirm',
              text = 'Some laboratory variables are missing and going to be singly imputed. The imputation is a black-box XGBoost model based on our training dataset. Imputation might reduce the reliability of the model. This is a once-per-session caution. Do you want to continue?'
            )
            if (isFALSE(input$missingConfirm)) shinyjs::js$sendBackFirstTab()
          } else if (!(input$missingConfirm)){
            f7Dialog(
              id = 'missing-var',
              title = 'Missing essential features',
              type = 'alert',
              text = 'Some laboratoy variables are missing and you chose not to impute. Please refresh the session if you want to reverse your decision.'
            )
            shinyjs::js$sendBackFirstTab()
          }
        } 
        
        observeEvent(input$missingConfirm, {
          # if (.last_val$impute != input$missingConfirm) {
            .last_val$impute <-  input$missingConfirm
          # }
        })
          
          # (if (any(is.na(data[1,])) && if (!is.null(input$missingConfirm)) !input$missingConfirm else T) {
          #   # .last_val$impute2 <- .last_val$impute
          # 
          # } else {
        observeEvent(c(input$missingConfirm, data), if (!any(is.na(data[1,])) | if (!is.null(input$missingConfirm)) input$missingConfirm else F){
          # .last_val$impute2 <- .last_val$impute
         
          # browser()
          if (length(.last_val$scenario)) updateF7SmartSelect(inputId = 'scenario', selected = .last_val$scenario)
          if (identical(data,.last_val$data) & (identical(input$scenario, .last_val$scenario) | is.null(input$submit2))) return()
          .last_val$data = data
          if(!is.null(input$scenario)) .last_val$scenario = input$scenario
         
          
          X = misc$create_model_matrix(misc$rescale_data(data, params$scales))
          # browser()
          missX = colnames(X)[is.na(X[1,])]
          # print(missX)
          if (any(is.na(X[1,]))){
            # browser()
            X2 = X[,c(-1,-ncol(X)),drop=F] |> as.data.frame()
            colnames(X2) = paste0('V', 1:ncol(X2))
            X3 = X2[, colnames(imp_model$imputed.data[[1]])]
            X3 = rbind(0,X3)
            X3 = mixgb::impute_new(imp_model, X3)[[1]][-1,]
            X2[,colnames(imp_model$imputed.data[[1]])] = X3
            X2 = cbind(X[,1], X2, X[, ncol(X)])
            colnames(X2) = colnames(X)
            X = X2
            print(X)
          }
          
          select_list <-   list(
            "No test" = '0',
            "Smear (-)" = 'a',
            "Xpert (-)" = 'b',
            "Smear (-) + MGIT (-)" = 'c',
            "Smear (-) + Xpert (-)" = 'd',
            "All tests (-)" = 'e'
          )
          
          print(.last_val$scenario)
          output$scenario_options <- 
            renderUI(
              f7Flex(
                  f7Block(
                  # div(
                  "You can select a scenario in which some confirmatory test results have been retrieved.",
                  "All probabilities of test positive are for future tests",
                  
                  if (length(missX)) glue::glue('<br>{missX} is imputed with standardised value = {X[1,missX]|>round(2)}') |> as.list() |> do.call(HTML, args=_),
                  
                  id = 'scenario-instructions'
                  # style = "width: calc(100% - 500px); padding: 30px 10px 10px 30px"
                  # ),
                ),
                
        
                
                f7Block(
                  id = 'scenario-options',
                  f7SmartSelect(
                    inputId = 'scenario',
                    label = "Current scenario",
                    # selected = names(select_list)[which(select_list==.last_val$scenario)],
                    selected = .last_val$scenario,
                    choices = select_list,
                    openIn='popover',
                    searchbar=FALSE
                  ),
                  f7Block(shiny.fluent::PrimaryButton.shinyInput(inputId = 'submit2', text = 'Re-estimate with scenario')),
                  # style = 'width:400px;'
                )
              )
            )
          
          # browser()
          
          # observeEvent(req(input$scenario), {
          #   .last_input$scenario = input$scenario
          # })
          
                  
          bvars = c(3,4,5,6,7,8) + 10 + 1 #nXd=10, intercept=1
          # browser()
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
            plogis(params$z[,'z_Smear[2]', drop=FALSE] + params$b[,'b_RE[1]', drop=FALSE] * bacillary),
            plogis(params$z[,'z_Mgit[2]',  drop=FALSE] + params$b[,'b_RE[2]', drop=FALSE] * bacillary),
            plogis(params$z[,'z_Xpert[2]', drop=FALSE] + params$b[,'b_RE[3]', drop=FALSE] * bacillary)
          )
          scenarios = list(
            a = misc$test_prob(theta, fpr[,1, drop=FALSE], tpr[,1, drop=FALSE], neg=TRUE),
            b = misc$test_prob(theta, fpr[,3, drop=FALSE], tpr[,3, drop=FALSE], neg=TRUE),
            c = misc$test_prob(theta, fpr[,1:2], tpr[,1:2], neg=TRUE),
            d = misc$test_prob(theta, fpr[,c(1,3)], tpr[,c(1,3)], neg=TRUE),
            e = misc$test_prob(theta, fpr, tpr, neg=TRUE)
          )
          
          if (length(input$scenario)) if (input$scenario != '0'){
            theta_orig = theta
            scenario = scenarios[[input$scenario]]
            theta = theta * scenario$tpr / scenario$prob
          }
          
          p_Smear = (as.matrix(plogis(params$z[,'z_Smear[1]', drop=FALSE])*(1-(theta)) + plogis(params$z[,'z_Smear[2]', drop=FALSE] + params$b[,'b_RE[1]'] * bacillary)*(theta)))
          p_Mgit  = (as.matrix(plogis(params$z[,'z_Mgit[1]',  drop=FALSE])*(1-(theta)) + plogis(params$z[,'z_Mgit[2]',  drop=FALSE] + params$b[,'b_RE[2]'] * bacillary)*(theta)))
          p_Xpert = (as.matrix(plogis(params$z[,'z_Xpert[1]', drop=FALSE])*(1-(theta)) + plogis(params$z[,'z_Xpert[2]', drop=FALSE] + params$b[,'b_RE[3]'] * bacillary)*(theta)))
          
          
          colnames(theta) = "Probability"
          colnames(ztheta) = "Score"
          predicts$theta = theta
          
          # Theta ----
          output$theta_text = plot_text(theta, of = 'probability of TBM', pct=T)
          output$theta_areasPlot = renderPlot({
            q <- quantile(theta, c(.1, .25, .75, .9))
            q <- round(c(q, mean(theta)), digits=2)
            l <- format(round((q),digits=2), scientific=FALSE,  drop0trailing=TRUE)
            # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
            bayesplot::mcmc_areas(theta, prob = .50, prob_outer = .90,  point_est = 'mean', adjust = 2) +
              scale_x_continuous(breaks=q, label=l) + 
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
            # ggplot() + stat_ecdf(aes(x=theta, xend=after_stat(ecdf))) + 
            #   geom_segment(aes(y=mean(theta)), linewidth=1, color = )
            #   geom_segment(aes(y=mean(theta), yend=mean(theta), x=1)) + 
            #   bayesplot::theme_default()
            
          })
          
          colnames(bacillary) = "Bacillary burden"
          predicts$bacillary = bacillary
          colnames(p_Smear) <- colnames(p_Mgit) <- colnames(p_Xpert) <- 'Probability'
          predicts$p_Smear = p_Smear
          predicts$p_Mgit = p_Mgit
          predicts$p_Xpert = p_Xpert
          
          # RE
          output$re_text = plot_text(bacillary, fn = c, of = 'average bacillary burden')
          output$re_areasPlot = renderPlot({
            q <- quantile(bacillary, c(.1, .25, .75, .9))
            q <- round(c(q, mean(bacillary)), digits=2)
            l <- format(round((q),digits=2), scientific=FALSE,  drop0trailing=TRUE)
            bayesplot::mcmc_areas(bacillary, prob=.5,  prob_outer = 1, point_est = 'mean', adjust = 2) +
              scale_x_continuous(breaks=q, label=l) 
            # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          }
            
          )
          
          output$test_text = renderText("The plots below show estimates of each confimation test's possibility of positive.")
          # Tests
          output$smear_text = plot_text(p_Smear, of = 'probability of positive Smear')
          output$smear_areasPlot = renderPlot({
            q <- quantile(p_Smear, c(.1, .025, .975, .9))
            q <- round(c(q, mean(p_Smear)), digits=2)
            l <- format(round((q),digits=2), scientific=FALSE,  drop0trailing=TRUE)
            # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
            bayesplot::mcmc_areas(p_Smear, prob = .5, prob_outer = .95,  point_est = 'mean', adjust = 2) + 
              scale_x_continuous(breaks=q, label=l)+ 
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          })
          output$mgit_text = plot_text(p_Mgit, of = 'probability of positive Mgit')
          output$mgit_areasPlot = renderPlot({
            q <- quantile(p_Mgit, c(.1, .25, .75, .9))
            q <- round(c(q, mean(p_Mgit)), digits=2)
            l <- format(round((q),digits=2), scientific=FALSE,  drop0trailing=TRUE)
            # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
            bayesplot::mcmc_areas(p_Mgit, prob = .5, prob_outer = .95,  point_est = 'mean', adjust = 2) + 
              scale_x_continuous(breaks=q, label=l)+ 
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          })
          output$xpert_text = plot_text(p_Xpert, of = 'probability of positive GeneXpert')
          output$xpert_areasPlot = renderPlot({
            q <- quantile(p_Xpert, c(.1, .25, .75, .9))
            q <- round(c(q, mean(p_Xpert)), digits=2)
            l <- format(round(plogis(q),digits=2), scientific=FALSE,  drop0trailing=TRUE)
            # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
            bayesplot::mcmc_areas(p_Xpert, prob = .5, prob_outer = .95,  point_est = 'mean', adjust = 2) + 
              scale_x_continuous(breaks=q, label=l)+ 
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          })
          
        }) 
      } else {

        
        # browser()
        X = misc$create_model_matrix(misc$rescale_data(data, params$scales))
        if (any(is.na(X[1,]))){
          # browser()
          X2 = X[,c(-1,-ncol(X)),drop=F] |> as.data.frame()
          colnames(X2) = paste0('V', 1:ncol(X2))
          X3 = X2[, colnames(imp_model$imputed.data[[1]])]
          X3 = rbind(0,X3)
          X3 = mixgb::impute_new(imp_model, X3)[[1]][-1,]
          X2[,colnames(imp_model$imputed.data[[1]])] = X3
          X2 = cbind(X[,1], X2, X[, ncol(X)])
          colnames(X2) = colnames(X)
          X = X2
          print(X)
        }
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
              mean = apply(arr, 2, mean, na.rm=T),
              median = apply(arr, 2, quantile, .5, na.rm=T),
              lower.ci=apply(arr,2,quantile, .025, na.rm=T),
              upper.ci=apply(arr,2,quantile, .975, na.rm=T)
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
    } else if (mode[[1]] == 'simplified') {
      if (mode[[2]]==1) {
        # browser() 
        if (identical(data,.last_val$data)) return()
        .last_val$data = data
        output$scenario_options = renderUI(f7Block('Model runs in simplified mode as required CSF biomarkers are not provided. Only TBM risk is available.'))
        # browser()
        dt = data
        scale_Xc = params$scales
        dt$illness_day = (log2(dt$illness_day) - scale_Xc$id$`scaled:center`) / scale_Xc$id$`scaled:scale` 
        dt$gcs = (12-dt$gcs - scale_Xc$gcs$`scaled:center`) / scale_Xc$gcs$`scaled:scale` 
        
        X = cbind(1, dt[, c(misc$ess_var[1:(length(misc$ess_var)-2)], misc$add_var, misc$ess_var[-1:0+length(misc$ess_var)])])
        # a = params$a[1:3000,]
        
        ztheta = params$a %*% t(X)
        theta = plogis(ztheta)
        
        colnames(theta) = "Probability"
        
        # Theta ----
        output$theta_text = plot_text(theta, of = 'probability of TBM', pct=T)
        output$theta_areasPlot = renderPlot({
          q <- quantile(theta, c(.1, .25, .75, .9))
          q <- round(c(q, mean(theta)), digits=2)
          l <- format(round((q),digits=2), scientific=FALSE,  drop0trailing=TRUE)
          # qlogis(c(.0001,.001,.01, seq(.1,.9,.2),.99)
          bayesplot::mcmc_areas(theta, prob = .5, prob_outer = .95,  point_est = 'mean', adjust = 2) + 
            scale_x_continuous(breaks=q, label=l) + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        })
        output$re_text <- output$smear_text <- output$mgit_text <- output$xpert_text <- renderText('')
        
        output$re_areasPlot <- output$smear_areasPlot <- output$mgit_areasPlot <- output$xpert_areasPlot <- 
          renderPlot(ggplot2::ggplot() + ggplot2:::annotate('text', x = 3, y= 3, label = "Not available in simplified mode") + 
                       ggplot2::theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) 
                     + xlim(2,4) + ylim(2,4))
      } else {
        dt = data
        scale_Xc = params$scales
        dt$illness_day = (log2(dt$illness_day) - scale_Xc$id$`scaled:center`) / scale_Xc$id$`scaled:scale` 
        dt$gcs = (12-dt$gcs - scale_Xc$gcs$`scaled:center`) / scale_Xc$gcs$`scaled:scale` 
        
        X = cbind(1, dt[, c(misc$ess_var[1:(length(misc$ess_var)-2)], misc$add_var, misc$ess_var[-1:0+length(misc$ess_var)])])
        # a = params$a[1:3000,]
        
        ztheta = a %*% t(X)
        theta = plogis(ztheta)
        
        sample_array = list(theta)
        names(sample_array) = c('p_tbm')
        
        sample_summary =
          sapply(sample_array, function(arr) {
            data.frame(
              mean = apply(arr, 2, mean, na.rm=T),
              median = apply(arr, 2, quantile, .5, na.rm=T),
              lower.ci=apply(arr,2,quantile, .025, na.rm=T),
              upper.ci=apply(arr,2,quantile, .975, na.rm=T)
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
            f7BlockFooter('Not all required CSF biomarkers are provided. Model runs in simplified mode, only p_TBM is available.'),
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
  
  waiter::waiter_hide()
}
