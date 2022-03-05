##
# tab
pf_ipt_trn_tp <- tabPanel(
  "Transactions",
  
  fluidRow(
    column(
      12,
      
      do.call(tabsetPanel, c(id = "pf_ipt_trn_tab", lapply(1:nrow(accounts_show), function(i){
        
        tabPanel(
          accounts_show$name[i],
          uiOutput(accounts_show$id[i])
        )
        
      })))
      
    )
  )
)