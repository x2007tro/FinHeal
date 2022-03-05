##
# tab
pf_ipt_par_tp <- tabPanel(
  "Parameters",
  
  fluidRow(
    column(
      12,
      
      tags$div(
        class = 'block_outter_frame',
        fluidRow(
          column(
            12,
            tags$div(class = "par_div", selectInput(paste0("fp_rpt_taxtble_ty"), label = "tax year", 
                                                     selected = lubridate::year(Sys.Date()), choices = unique(taxtbl_show$year), width = entry_wid_m)),    
            tags$div(class = "par_div", dateInput("pf_ipt_par_begdt","begin date", value = lubridate::floor_date(Sys.Date(),"month") - months(1), width = entry_wid_m)),
            tags$div(class = "par_div", dateInput("pf_ipt_par_enddt","end date", value = lubridate::ceiling_date(Sys.Date(),"month") - months(1) - days(1), width = entry_wid_m))
          )
        )
      ),
      
    )
  )
)