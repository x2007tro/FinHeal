##
# tab
pf_ipt_ppm_tp <- tabPanel(
  "Property Management",
  
  fluidRow(
    column(
      12,
      
      tags$div(
        do.call(tabsetPanel, c(id = "pf_ipt_ppm_tab", lapply(1:nrow(property_shownact), function(i){
          
          tabPanel(
            paste(property_shownact$name[i], "(",property_shownact$operation_type[i],")"),
            uiOutput(paste0("pf_ipt_ppm_", property_shownact$id[i]))
          )
          
        })))
      )
      
    )
  )
)