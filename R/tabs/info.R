infoTab <-
  f7Tab(
    tabName = "Info",
    # icon = f7Icon("info_circle"),
    icon = emo::ji('information'),
    active = FALSE,
    f7Block(
      div(
        style = 'text-align:center',
        uiOutput('oucru_logo'),
        h1('TBM-LCA Supplementary Web App'),
        p('Decision helper for TBM Diagnosis'),
        p('Intended for Academic Use'),
        p('2022'),
        p('Author: Trinh Dong Huu Khanh'),
        p('Original paper: (somewhere)'),
        p('Contact: trinhdhk@oucru.org | trinh.dong@kcl.ac.uk')
      )
    )
  )