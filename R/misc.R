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

compile_sass=function() sass::sass('www/css/main.scss' |> sass::sass_file(), output = 'www/css/main.css')

template_dt <- 
  data.frame(
    # Clinical
    hiv = vector(),
    tb_symptoms = vector(),
    cranial_nerve_palsy = vector(),
    focal_neuro_deficit = vector(),
    contact_tb = vector(),
    gcs = vector(),
    illness_day = vector(),
    headache = vector(),
    psychosis = vector(),
    
    #Xray 
    xray_pul_tb = vector(),
    xray_mil_tb = vector(),
    
    #CSF
    csf_glu = vector(),
    bld_glu = vector(),
    csf_rbc = vector(),
    csf_wbc = vector(),
    csf_lym = vector(),
    csf_eos = vector(),
    csf_pro = vector(),
    csf_lac = vector(),
    
    # Other
    gram = vector(),
    cryptococ = vector()
  )

ess_var = c('hiv', 'tb_symptoms','focal_neuro_deficit', 'cranial_nerve_palsy',  
            'contact_tb', 'xray_pul_tb', 'xray_mil_tb', 'illness_day', 'gcs')
add_var = c('headache', 'psychosis')

create_data = 
  function(input, session){
    if (length(input$custom_data)) {
      dt = read.csv(req(input$custom_data$datapath), check.names = FALSE)
      # browser()
      if (all.equal(names(dt), names(template_dt))[[1]] != TRUE) {
        f7Dialog(id = "invalid-file", 
                 title = "Invalid file",
                 text = "This file is not following the template. Please re-check!")
        # input$custom_data = NULL
        return(NA)
      }
      return(dt)
    }
    dt = template_dt
    dt = as.list(dt)
    for (var in names(dt)) dt[[var]] = as.numeric(isolate(input[[var]]))
    
    as.data.frame(dt)
  }


create_recipe = 
  function(dt, input, session){
    if (any(is.na(dt[1,ess_var]))) { model = ''}
    else if (any(is.na(dt[1,names(template_dt)]))){
      # if (any(is.na(dt[1,add_var]))) model = ''
      # else 
      model = 'simplified'
    } else model = 'full'
    
    model
  }

rescale_data = function(dt, scale_Xc){
  dt$illness_day = (log2(dt$illness_day) - scale_Xc$id$`scaled:center`) / scale_Xc$id$`scaled:scale` 
  dt$gcs = (15-dt$gcs - scale_Xc$gcs$`scaled:center`) / scale_Xc$gcs$`scaled:scale` 
  dt$csf_glu = (log2(dt$csf_glu+.1) - scale_Xc$csfglu$`scaled:center`) / scale_Xc$csfglu$`scaled:scale` 
  dt$bld_glu = (log2(dt$bld_glu) - scale_Xc$glu$`scaled:center`) / scale_Xc$glu$`scaled:scale` 
  dt$csf_rbc = (log10(dt$csf_rbc+1) - scale_Xc$csfred$`scaled:center`) / scale_Xc$csfred$`scaled:scale` 
  dt$csf_wbc = (log10(dt$csf_wbc+1) - scale_Xc$csfwbc$`scaled:center`) / scale_Xc$csfwbc$`scaled:scale` 
  dt$csf_lym = (log10(dt$csf_lym+1) - scale_Xc$csflym$`scaled:center`) / scale_Xc$csflym$`scaled:scale` 
  dt$csf_eos = (log10(dt$csf_eos+1) - scale_Xc$csfeos$`scaled:center`) / scale_Xc$csfeos$`scaled:scale` 
  dt$csf_pro = (log2(dt$csf_pro) - scale_Xc$csfpro$`scaled:center`) / scale_Xc$csfpro$`scaled:scale` 
  dt$csf_lac = (log2(dt$csf_lac) - scale_Xc$csflac$`scaled:center`) / scale_Xc$csflac$`scaled:scale` 
  dt$csfeos_pos = dt$csf_eos > 0
  
  dt
}

create_model_matrix = function(dt){
  with(dt,
       cbind(1, hiv, tb_symptoms, focal_neuro_deficit, cranial_nerve_palsy, contact_tb,
             xray_pul_tb, xray_mil_tb, cryptococ, gram, csfeos_pos,
             illness_day, bld_glu, csf_glu, csf_wbc, csf_lym, csf_pro, csf_lac,
             gcs, csf_eos, csf_rbc, csf_wbc^2))
}

test_prob = function(theta, fpr, tpr, neg=FALSE) {
  stopifnot(length(fpr) == length(tpr)) 
  if (neg) {
    fpr = 1-fpr
    tpr = 1-tpr
  }
  FPR = apply(fpr, 1, prod) |> as.matrix(ncol=1)
  TPR = apply(tpr,1, prod) |> as.matrix(ncol=1)
  prob = (1-theta) * FPR + (theta) * TPR
  list(fpr=FPR, tpr=TPR, prob=prob)
}
    
test_prob2 = function(val, p){
  if (is.na(val)) return(1)
  if (val == 1) return(p)
  1-p
}


