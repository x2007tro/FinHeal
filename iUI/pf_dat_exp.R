##
# tab
pf_dat_exp_tp <- tabPanel(
  "Raw Transactions",
  
  tabsetPanel(
    tabPanel(
      "Current Monh",
      
      fluidRow(
        column(
          12,
          tags$div(
            class = 'block_outter_frame',
            tags$div(
              class = 'block_inner_frame',
              DT::dataTableOutput("pf_dat_exp_cm_trans")
            )
          )
          
        )
      )
      
    ),
    tabPanel(
      "Year-to-date",
      
      fluidRow(
        column(
          12,
          tags$div(
            class = 'block_outter_frame',
            tags$div(
              class = 'block_inner_frame',
              DT::dataTableOutput("pf_dat_exp_ytd_trans")
            )
          )
          
        )
      )
    ),
    tabPanel(
      "Max",
      
      fluidRow(
        column(
          12,
          tags$div(
            class = 'block_outter_frame',
            tags$div(
              class = 'block_inner_frame',
              DT::dataTableOutput("pf_dat_exp_max_trans")
            )
          )
          
        )
      )
    )
  )
)