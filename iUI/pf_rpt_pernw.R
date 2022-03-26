##
# tab
pr_rpt_pernw_tp <- tabPanel(
  "Net Worth",
  
  fluidRow(
    column(
      12,
      tags$div(
        class = 'block_outter_frame',
        
        fluidRow(
          column(
            6,
            tags$h4('Asset'),
            
            tags$div(
              class = 'block_inner_frame',
              
              fluidRow(
                column(
                  6,
                  
                  fluidRow(
                    column(
                      12,
                      uiOutput('pr_rpt_pernw_ast_prop')
                    )
                  ),
                  
                  fluidRow(
                    column(
                      12,
                      uiOutput('pr_rpt_pernw_ast_land')
                    )
                  )
                  
                ),
                column(
                  6,
                  
                  fluidRow(
                    column(
                      12,
                      uiOutput('pr_rpt_pernw_ast_oth')
                    )
                  )
                  
                )
              )
              
            )
          ),
          
          column(
            3,
            tags$h4('Liability'),
            
            tags$div(
              class = 'block_inner_frame',
              fluidRow(
                column(
                  12,
                  uiOutput('pr_rpt_pernw_liab')
                )
              ) 
            )
            
          ),
          
          column(
            3,
            tags$h4('Equity'),
            
            tags$div(
              class = 'block_inner_frame',
              fluidRow(
                column(
                  12,
                  uiOutput('pr_rpt_pernw_netw')
                )
              ) 
              
            )
          )
        
        )
        
      )
    )
  )
)