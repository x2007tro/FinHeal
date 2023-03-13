##
# tab
pf_res_nppty_tp <- tabPanel(
  "Mortgage and Property Tax",
  
  fluidRow(
    column(
      12,
      tags$div(
        class = 'block_outter_frame',
        uiOutput('pf_res_nppty_ui') 
      )
    )
  )
)