#
# handling economic indicators
#
output$pf_res_nppty_ui <- renderUI({
  
  do.call(tabsetPanel, c(id = "pf_res_nppty_", lapply(1:nrow(intrt_show()), function(i){
    
    tabPanel(
      paste0("Interest Rate ",  scales::percent(intrt_show()$rate[i], accuracy = 0.01)),
      uiOutput(paste0("pf_res_nppty_", intrt_show()$id[i]))
    )
    
  })))
  
})

observe({
  lapply(1:nrow(intrt_show()), function(i){
    
    curr_intrt_id <- intrt_show()$id[i]
    curr_intrt_rate <- intrt_show()$rate[i]
    
    output[[paste0("pf_res_nppty_",curr_intrt_id)]] <- renderUI({
      
      tagList(
        fluidRow(
          column(
            6,
            tags$div(
              class = "block_inner_frame",
              tags$h4(class = 'block_title', "Mortgage Monthly Payment"),
              DT::dataTableOutput(paste0("pf_res_nppty_amor_tbl_", curr_intrt_id))
            )
          ),
          column(
            6,
            tags$div(
              class = "block_inner_frame",
              tags$h4(class = 'block_title', "Property Tax Rate"),
              DT::dataTableOutput(paste0("pf_res_nppty_ptax_tbl_", curr_intrt_id))
            )
          )
        )
      )
      
    })
    
    output[[paste0("pf_res_nppty_amor_tbl_", curr_intrt_id)]] <- DT::renderDataTable({
      house_prcs <- seq(from = 500000, to = 2000000, by = 100000)
      res <- AmortTableConstr(house_prcs, curr_intrt_rate)
      DT::datatable(
        res$ft,
        options = list(
          pageLength = 10,
          orderClasses = FALSE,
          searching = TRUE,
          paging = TRUE
        )
      ) %>% 
        DT::formatRound('house_price', digits = 0) %>% 
        DT::formatRound('down_payment_required', digits = 0) %>% 
        DT::formatRound('loan_amount', digits = 0) %>% 
        DT::formatPercentage('interest_rate', digits = 2) %>% 
        DT::formatRound('monthly_payment', digits = 0)
    })
    
    output[[paste0("pf_res_nppty_ptax_tbl_", curr_intrt_id)]] <- DT::renderDataTable({
      pptytaxr_show <- pptytaxr_show() %>% 
        dplyr::filter(year == lubridate::year(input$pf_ipt_par_begdt)) %>% 
        dplyr::select(-source)
      DT::datatable(
        pptytaxr_show
      ) %>% 
        DT::formatPercentage('tax_rate', digits = 5) 
      
    })
    
  })
})

