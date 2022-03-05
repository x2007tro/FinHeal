##
# tab
pf_rpt_expsumm_tp <- tabPanel(
  "Expense",
  
  fluidRow(
    column(
      12,
      tags$div(
        class = 'block_outter_frame',
        
        fluidRow(
          column(
            6,
            tags$div(
              class = 'block_inner_frame',
              tags$h3("Current Month"),
              plotOutput("pf_rpt_expsumm_p1")
            )
          ),
          column(
            6,
            tags$div(
              class = 'block_inner_frame',
              tags$h3("Current Year"),
              plotOutput("pf_rpt_expsumm_p2")
            )
          )
        ),
        fluidRow(
          column(
            6,
            tags$div(
              class = 'block_inner_frame',
              tags$h3("Past Six Month"),
              plotOutput("pf_rpt_expsumm_p3")
            )
          ),
          column(
            6,
            tags$div(
              class = 'block_inner_frame',
              tags$h3("Past Six Years"),
              plotOutput("pf_rpt_expsumm_p4")
            )
          )
        ) 
        
        
      )
    )
  )
)