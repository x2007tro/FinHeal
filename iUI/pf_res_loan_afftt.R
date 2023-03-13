##
# tab
pf_res_loan_afftt_tp <- tabPanel(
  "Property Investment Tools",
  
  tabsetPanel(
    tabPanel(
      "Profitability Test",
      
      fluidRow(
        column(
          12,
          
          tags$div(
            class = 'block_otter_frame',
            
            fluidRow(
              column(
                12,
                
                tags$div(
                  class = 'block_inner_frame',
                  
                  uiOutput('pf_res_loan_afftt_pfttst')
                )
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Afforability Test",
      
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
                  uiOutput('pf_res_loan_afftt_ipt')
                )
                
              )
            ),
            fluidRow(
              column(
                12,
                uiOutput('pf_res_loan_afftt_opt')
              )
            )
          )
          
        )
      )
    )
  )

)