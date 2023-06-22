infoTab <-
  f7Tab(
    tabName = "Info",
    title = 'Info',
    # icon = f7Icon("info_circle"),
    icon = emo::ji('information'),
    active = FALSE,
    f7Block(
      div(
        style = 'text-align:center',
        uiOutput('oucru_logo'),
        h1('TBM-LCA Complementary Web App'),
        p('Decision helper for TBM Diagnosis'),
        p('Intended for Academic Use'),
        p('Author: Trinh Huu Khanh Dong'),
        p('Original paper: (somewhere)'),
        p('Contact: trinhdhk@oucru.org | trinh.dong@kcl.ac.uk'), 
        p('The OUCRU brand, OUCRU logo, and the OUCRU letter sequence are belonged to the Oxford University Clinical Research Unit'),
        p('PWA was made possible by ', a(target='_blank', href='https://www.pwabuilder.com/', "PWABuilder"))
      )
    )
  )