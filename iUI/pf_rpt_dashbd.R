##
# tab
pf_rpt_dashbd_tp <- tabPanel(
  "Dashboard",
  
  fluidRow(
    column(
      12,
      
      fluidRow(
        column(
          6,
          tags$div(
            class = 'block_outter_frame',
            tags$h3("Current Month"),
            plotOutput("pf_rpt_dashbd_p1")
          )
        ),
        
        column(
          6,
          tags$div(
            class = 'block_outter_frame',
            tags$h3("Current Year"),
            plotOutput("pf_rpt_dashbd_p2")
          )
          
        )
      ),
      fluidRow(
        column(
          5,
          tags$div(
            class = 'block_outter_frame',
            tags$h3("Current Month"),
            DT::dataTableOutput("pf_rpt_dashbd_t1")
          )
        ),
        column(
          1,
          tags$div(
            class = 'seperater'
          )
        ),
        
        column(
          6,
          tags$div(
            class = 'block_outter_frame',
            tags$h3("Previous Year"),
            plotOutput("pf_rpt_dashbd_p3")
          )
          
        )
      ),
      
    )
  )
)