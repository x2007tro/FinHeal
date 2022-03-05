##
# render data from DB
output$pf_dat_exp_cm_trans <- DT::renderDataTable({
  df <- transdata_full %>% 
    dplyr::filter(transaction_date >= input$pf_ipt_par_begdt) %>% 
    dplyr::filter(transaction_date <= input$pf_ipt_par_enddt)
  df
})

output$pf_dat_exp_ytd_trans <- DT::renderDataTable({
  df <- transdata_full %>% 
    dplyr::filter(lubridate::year(transaction_date) == lubridate::year(input$pf_ipt_par_begdt))
  df 
})

output$pf_dat_exp_max_trans <- DT::renderDataTable({
  df <- transdata_full
  df
})