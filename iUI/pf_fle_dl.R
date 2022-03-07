##
# tab
pf_fle_dl_tp <- tabPanel(
  "Download",
  
  fluidRow(
    column(
      12,
      
      tags$div(
        class = 'block_outter_frame',
        
        fluidRow(
          
          tags$div(
            class = 'block_inner_frame',
            column(
              3,
              
              tags$div(
                class = 'block_inner_frame',
                
                tags$h4(class = 'block_title', 'data pick'),
                
                selectInput("pf_fle_dl_dspick", "Choose a dataset:", choices = c("current month", "ytd", "max"), width = entry_wid_m),
                
                # Button
                downloadButton(class = 'btn-danger', "pf_fle_dl_dsdl", "Download")
                
              )
              
            ),
            column(
              3,
              
              tags$div(
                class = 'block_inner_frame',
                
                tags$h4(class = 'block_title', 'data feature'),
                
                uiOutput('pf_fle_dl_dsfeat1')
                
              )
              
            ),
            column(
              3,
              
              tags$div(
                class = 'block_inner_frame',
                
                tags$h4(class = 'block_title', 'data feature'),
                
                uiOutput('pf_fle_dl_dsfeat2')
                
              )
              
            ),
            column(
              3,
              
              tags$div(
                class = 'block_inner_frame',
                
                tags$h4(class = 'block_title', 'data feature'),
                
                uiOutput('pf_fle_dl_dsfeat3')
                
              )
              
            ) 
          )
        
        ),
        fluidRow(
          column(
            12,
            
            tags$div(
              class = 'block_inner_frame',
              
              tags$h4(class = 'block_title', 'data snippet'),
              DT::dataTableOutput('pf_fle_dl_dssnip')
              
            )
            
          )
        )
      )
    
    )
  )
)