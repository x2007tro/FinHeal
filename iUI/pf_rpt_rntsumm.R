##
# tab
pf_rpt_rntsumm_tp <- tabPanel(
  "Property",
  
  fluidRow(
    column(
      12,
      
      do.call(tabsetPanel, c(id = "pf_rpt_rntsumm_tab", lapply(1:nrow(property_show), function(i){

        tabPanel(
          property_show$name[i],
          uiOutput(paste0("pf_rpt_rntsumm_", property_show$id[i]))
        )

      })))
    )
  )
)