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
            4,
            
            fluidRow(
              column(
                12,
                
                tags$div(
                  class = 'block_inner_frame',
                  tags$h4(class = 'block_title', 'Net Worth'),
                  fluidRow(
                    column(
                      12,
                      uiOutput('pr_rpt_pernw_netw')
                    )
                  ) 
                  
                )
                
              )
            ),
            
            fluidRow(
              column(
                12,
                
                tags$div(
                  class = 'block_inner_frame',
                  tags$h4(class = 'block_title', 'Liability'),
                  fluidRow(
                    column(
                      12,
                      uiOutput('pr_rpt_pernw_liab')
                    )
                  ) 
                )
              )
            )
            
          ),
          
          column(
            8,
            
            tags$div(
              class = 'block_inner_frame',
              tags$h4(class = 'block_title', 'Asset'),
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
          )
        ),
        
        fluidRow(
          column(
            12,
            
            tags$div(
              class = 'block_inner_frame',
              tags$h4(class = 'block_title', 'Net Worth Curve'),
              plotOutput('pr_rpt_pernw_hist')
            )
          )
        )
        
      )
    )
  )
)