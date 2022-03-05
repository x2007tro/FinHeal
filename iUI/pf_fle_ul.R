##
# tab
pf_fle_ul_tp <- tabPanel(
  "Upload",
  
  fluidRow(
    column(
      12,
      
      fileInput(
        "pf_mint_trans", 
        "Please upload MINT transactions file to the server: ",
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
    )
  )
)