##
# Source all ui files
##
ui_files <- c('pf_rpt_dashbd','pf_rpt_expsumm','pf_rpt_rntsumm','pf_rpt_taxtble',
              'pf_ipt_par','pf_ipt_trn','pf_ipt_ppm',
              'pf_fle_ul','pf_fle_dl',
              'pf_dat_exp',
              'pf_res_nppty')
lapply(ui_files, function(f){
  source(paste0("./iUI/", f, ".R"), local = FALSE)
})

##
# Shiny ui
##
mainUI <- fluidPage(theme = shinythemes::shinytheme("journal"),
  
  # css style
  tags$head(
    includeCSS("fh_style.css")
  ),
  
  # main output
  navbarPage(
    "Financial Health",
    tabPanel(
      "Personal",
      navlistPanel(
        "Report",
        widths = c(2,10),
        pf_rpt_dashbd_tp,
        pf_rpt_expsumm_tp,
        pf_rpt_rntsumm_tp,
        pf_rpt_taxtble_tp,
        "Input",
        pf_ipt_par_tp,
        pf_ipt_trn_tp,
        pf_ipt_ppm_tp,
        "Files",
        pf_fle_ul_tp,
        pf_fle_dl_tp,
        'Resource',
        pf_res_nppty_tp,
        pf_dat_exp_tp
      )
    ),
    tabPanel(
      "Business"
    )
  )

)
