infoTab <-
  f7Tab(
    tabName = "Info",
    title = 'Info',
    # icon = f7Icon("info_circle"),
    icon = emoji::emoji('information'),
    active = FALSE,
    f7Block(
      div(
        style = 'text-align:center',
        uiOutput('oucru_logo'),
        h1('TBM-LCA Complementary Web App'),
        p('Decision helper for TBM Diagnosis'),
        p('Intended for Academic Use'),
        p('Author: Trinh Huu Khanh Dong'),
        p('Please cite:', a(href = 'https://doi.org/10.1186/s12879-024-08992-z', target="_blank", 'Orginal paper')),
        div(class='code-box', 'Dong, T.H.K., Donovan, J., Ngoc, N.M. et al. A novel diagnostic model for tuberculous meningitis using Bayesian latent class analysis. BMC Infect Dis 24, 163 (2024). https://doi.org/10.1186/s12879-024-08992-z'),
        p('Contact: trinhdhk@oucru.org | trinh.dong@kcl.ac.uk | trinhdhk@outlook.com.vn'), 
        p('The OUCRU brand, OUCRU logo, and the OUCRU letter sequence are belonged to the Oxford University Clinical Research Unit'),
        p('PWA was made possible by ', a(target='_blank', href='https://www.pwabuilder.com/', "PWABuilder"))
      )
    )
  )
