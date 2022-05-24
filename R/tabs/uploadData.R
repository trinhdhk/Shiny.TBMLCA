uploadDataTab <-
  f7Tab(
    tabName = 'Dataset Analysis',
    icon = span(emo::ji('card_index_dividers'), class = 'my-icon'),
    
    p(strong("1. First download the template and scroll down for the dictionary"), style = 'padding-left: 20px; padding-right: 20px'),
    f7Block(f7DownloadButton('template', 'Download template'), 
            style='height=100%'),
    div( tableOutput('data_dict'),
         style = "height: 200px; overflow-y: auto; margin-left: 5px; margin-right:5px"),
    p(" - Note that only smear, mgit, and xpert are allowed to be missing and all CSF biomarkers should be corrected for traumatic lumbar puncture (although CSF Red cell count is still needed).",
      style = 'padding-left: 20px; padding-right: 20px'),
    p(strong("2. Upload dataset, make sure it is from the template."),
      style = 'padding-left: 20px; padding-right: 20px; margin-top:10px'),
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
    p(strong("3. The output will appear below when done."), style = 'padding-left: 20px; padding-right: 20px; margin-top:10px'),
    
    uiOutput('download_results')
  )