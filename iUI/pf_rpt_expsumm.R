##
# tab
pf_rpt_expsumm_tp <- tabPanel(
  "Expense",
  
  tabsetPanel(
    tabPanel(
      "Overall",
      
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
                  tags$h4(class = 'block_title', "Report Month"),
                  plotOutput("pf_rpt_expsumm_p1")
                )
              ),
              column(
                6,
                tags$div(
                  class = 'block_inner_frame',
                  tags$h4(class = 'block_title', "Report Year"),
                  plotOutput("pf_rpt_expsumm_p2")
                )
              )
            ),
            fluidRow(
              column(
                6,
                tags$div(
                  class = 'block_inner_frame',
                  tags$h4(class = 'block_title', "Past Five Months"),
                  plotOutput("pf_rpt_expsumm_p3")
                )
              ),
              column(
                6,
                tags$div(
                  class = 'block_inner_frame',
                  tags$h4(class = 'block_title', "Past Five Years"),
                  plotOutput("pf_rpt_expsumm_p4")
                )
              )
            ) 
            
            
          )
        )
      )
    ),
    tabPanel(
      "Credit Card",
      
      fluidRow(
        column(
          12,
          
          tags$div(
            class = 'block_outter_frame',
            
            uiOutput("pf_rpt_cc_summ")
          )
        )
      )
    )
    
  )
  
  
)