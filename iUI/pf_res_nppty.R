##
# tab
pf_res_nppty_tp <- tabPanel(
  "Property Acquisition",
  
  fluidRow(
    column(
      12,
      tags$div(
        class = 'block_outter_frame',
        
        
        do.call(tabsetPanel, c(id = "pf_res_nppty_", lapply(1:nrow(intrt_show), function(i){
          
          tabPanel(
            paste0("Interest Rate ",  intrt_show$id[i]),
            uiOutput(paste0("pf_res_nppty_", intrt_show$id[i]))
          )
          
        })))
        
        
      )
    )
  )
)