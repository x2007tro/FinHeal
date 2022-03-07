##
# tab
pf_rpt_dashbd_tp <- tabPanel(
  "Dashboard",
  
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
              plotOutput("pf_rpt_dashbd_p1")
            )
          ),
          
          column(
            6,
            tags$div(
              class = 'block_inner_frame',
              tags$h4(class = 'block_title', "Report Year"),
              plotOutput("pf_rpt_dashbd_p2")
            )
            
          )
        ),
        fluidRow(
          column(
            6,
            tags$div(
              class = 'block_inner_frame',
              tags$h4(class = 'block_title', "Report Month"),
              tags$div(
                class = 'dt_table',
                DT::dataTableOutput("pf_rpt_dashbd_t1")
              ) 
            )
          ),
          
          column(
            6,
            tags$div(
              class = 'block_inner_frame',
              tags$h4(class = 'block_title', "Previous Year"),
              plotOutput("pf_rpt_dashbd_p3")
            )
            
          )
        )
      )
      
      
    )
  )
)