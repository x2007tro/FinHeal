##
# tab
pf_ipt_ppm_tp <- tabPanel(
  "Property Management",
  
  fluidRow(
    column(
      12,
      
      do.call(tabsetPanel, c(id = "pf_ipt_ppm_tab", lapply(1:nrow(property_shownact), function(i){
        
        tabPanel(
          property_shownact$name[i],
          uiOutput(paste0("pf_ipt_ppm_", property_shownact$id[i]))
        )
        
      })))
    )
  )
)