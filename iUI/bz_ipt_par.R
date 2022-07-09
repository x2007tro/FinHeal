##
# tab
bz_ipt_par_tp <- tabPanel(
  "Parameters",
  
  fluidRow(
    column(
      12,
      
      tags$div(
        class = 'block_outter_frame',
        fluidRow(
          column(
            12,
            tags$div(
              class = 'block_inner_frame',
              tags$h4(class = 'block_title', "Parameters"),
              tags$div(class = "par_div", dateInput("bz_ipt_par_begdt","begin date", value = as.Date('2022-06-01'), width = entry_wid_m)),
              tags$div(class = "par_div", dateInput("bz_ipt_par_enddt","end date", value = as.Date('2023-05-31'), width = entry_wid_m))
            )
          )
        )
      )
      
      
    )
  )
)