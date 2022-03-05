##
# tab
pf_dat_exp_tp <- tabPanel(
  "Transactions",
  
  tabsetPanel(
    tabPanel(
      "Current Monh",
      
      fluidRow(
        column(
          12,
          DT::dataTableOutput("pf_dat_exp_cm_trans")
        )
      )
      
    ),
    tabPanel(
      "Year-to-date",
      
      fluidRow(
        column(
          12,
          DT::dataTableOutput("pf_dat_exp_ytd_trans")
        )
      )
    ),
    tabPanel(
      "Max",
      
      fluidRow(
        column(
          12,
          DT::dataTableOutput("pf_dat_exp_max_trans")
        )
      )
    )
  )
)