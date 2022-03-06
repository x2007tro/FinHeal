#
# handling economic indicators
#
lapply(1:nrow(intrt_show), function(i){
  
  curr_intrt_id <- intrt_show$id[i]
  curr_intrt_rate <- intrt_show$rate[i]
  
  output[[paste0("pf_res_nppty_",curr_intrt_id)]] <- renderUI({
    
   tagList(
     fluidRow(
       column(
         6,
         tags$div(
           class = "block_inner_frame",
           tags$h3("Block 1"),
           DT::dataTableOutput(paste0("pf_res_nppty_amor_tbl_", curr_intrt_id))
         )
       ),
       column(
         6,
         tags$div(
           class = "block_inner_frame",
           tags$h3("Block 2"),
           DT::dataTableOutput(paste0("pf_res_nppty_ptax_tbl_", curr_intrt_id))
         )
       )
     )
   )
    
  })
  
  output[[paste0("pf_res_nppty_amor_tbl_", curr_intrt_id)]] <- DT::renderDataTable({
    house_prcs <- seq(from = 500000, to = 2000000, by = 100000)
    res <- AmortTableConstr(house_prcs, curr_intrt_rate*100)
    res$ft
  })
  
  output[[paste0("pf_res_nppty_ptax_tbl_", curr_intrt_id)]] <- DT::renderDataTable({
    pptytaxr_show <- pptytaxr_show %>% 
      dplyr::filter(year == lubridate::year(input$pf_ipt_par_begdt)) %>% 
      dplyr::select(-source)
    pptytaxr_show
  })
  
})