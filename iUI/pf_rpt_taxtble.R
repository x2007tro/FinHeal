##
# tab
pf_rpt_taxtble_tp <- tabPanel(
  "Tax Tables",
  
  fluidRow(
    column(
      12,
      tags$div(
        class = 'block_outter_frame',
        fluidRow(
          lapply(1:nrow(demogra_show), function(i){
            column(
              12/nrow(demogra_show),
              tags$div(
                class = 'block_inner_frame',
                tags$h3(paste0(demogra_show$name[i])),
                uiOutput(paste0("pf_rpt_taxtble_demo_", demogra_show$id[i]))
              )
            )
          })
        ),
        fluidRow(
          lapply(1:nrow(property_shownact), function(i){
            column(
              12/nrow(property_shownact),
              tags$div(
                class = 'block_inner_frame',
                tags$h3(paste0(property_shownact$name[i])),
                DT::dataTableOutput(paste0("pf_rpt_taxtble_ppty_", property_shownact$id[i]))
              )
            )
          })
        ),
      )
      
    )
  )
)