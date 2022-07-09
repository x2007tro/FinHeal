#
# handling economic indicators
#
exp4buz_tax_rpt_actnshow <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    records_full <- ReadDataFromSS(db_obj, '* Input 52 : Business Expense *')
    records_full <- records_full %>% 
      dplyr::filter(active == 1 & show == 1) %>% 
      #dplyr::filter(operation_type == 'p') %>% 
      dplyr::select(-dplyr::one_of('active','order','show','entry_datetime'))
  })
})

