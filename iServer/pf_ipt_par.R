#
# handling economic indicators
#
transdata_full <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    transdata_full <- ReadDataFromSS(db_obj, '* Input 02 : Transactions *')
    transdata_full <- transdata_full %>% 
      dplyr::filter(active == 1) %>% 
      #dplyr::filter(operation_type == 'p') %>% 
      dplyr::select(-dplyr::one_of('id','active','order','show','entry_datetime')) %>% 
      dplyr::arrange(transaction_date, hyper_category)
  })
})

transdata_cm <- reactive({
  df <- transdata_full() %>% 
    dplyr::filter(transaction_date >= input$pf_ipt_par_begdt) %>% 
    dplyr::filter(transaction_date <= input$pf_ipt_par_enddt) %>% 
    dplyr::arrange(transaction_date, hyper_category)
  df
})

transdata_ytd <- reactive({
  df <- transdata_full() %>% 
    dplyr::filter(lubridate::year(transaction_date) == lubridate::year(input$pf_ipt_par_begdt)) %>% 
    dplyr::arrange(transaction_date, hyper_category)
  df
})

accounts_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    accounts_full <- ReadDataFromSS(db_obj, '* Frame 01 : Account *')
    accounts_show <- accounts_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order) %>% 
      dplyr::select(id, name)
  })
})

creditcards_shownact <- reactive({
  
  withProgress(message = 'Getting credit card transaction details ...', {
    accounts_full <- ReadDataFromSS(db_obj, '* Frame 01 : Account *')
    accounts_show <- accounts_full %>% 
      dplyr::filter(card_type == 1) %>% 
      dplyr::arrange(order) %>% 
      dplyr::select(name, spending_limit)
  })
})

creditcards_trans_summ <- reactive({
  
  withProgress(message = 'Getting credit card transaction summary ...', {
    cc <- creditcards_shownact()
    transf <- transdata_full()
    
    by_month_cat <- cc %>% 
      dplyr::left_join(transf, by = c('name' = 'account')) %>% 
      dplyr::filter(transaction_date >= Sys.Date() - years(3)) %>% 
      dplyr::mutate(transaction_year = lubridate::year(transaction_date), transaction_month = lubridate::month(transaction_date)) %>% 
      dplyr::group_by(transaction_year, transaction_month, name, category, hyper_category) %>% 
      dplyr::summarise(amt = sum(amount))
    
    by_year_cat <- by_month_cat %>% 
      dplyr::group_by(transaction_year, name, category, hyper_category) %>% 
      dplyr::summarise(amt = sum(amt))
    
    by_month <- by_month_cat %>% 
      dplyr::group_by(transaction_year, transaction_month, name) %>% 
      dplyr::summarise(amt = sum(amt))
    
    by_year <- by_year_cat %>% 
      dplyr::group_by(transaction_year, name) %>% 
      dplyr::summarise(amt = sum(amt))
    
    tmp <- list(
      by_month_cat = by_month_cat,
      by_year_cat = by_year_cat,
      by_month = by_month,
      by_year = by_year
    )
    
    tmp
  })

})

opertype_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    opertype_full <- ReadDataFromSS(db_obj, '* Frame 03 : Operation Type *')
    opertype_show <- opertype_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order) %>% 
      dplyr::select(id, operation_type)
  })
})

transcat_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    transcat_full <- ReadDataFromSS(db_obj, '* Frame 04 : Transaction Category *')
    transcat_show <- transcat_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order) %>% 
      dplyr::select(id, name, hyper_category)
  })
})

dattbl_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    dattbl_full <- ReadDataFromSS(db_obj, '* Frame 05 : Data Table *')
    dattbl_show <- dattbl_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order) %>% 
      dplyr::select(id, name)
  })
})

autocat_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    autocat_full <- ReadDataFromSS(db_obj, '* Input 10 : Autocat *')
    autocat_show <- autocat_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order) %>% 
      dplyr::mutate(recurring = as.logical(recurring)) %>% 
      dplyr::select(string, category, recurring, property, multiplier, operation_type, data_table)
  })
})

property_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    property_full <- ReadDataFromSS(db_obj, '* Frame 11 : Property *')
    property_show <- property_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order)
  })
})

property_shownact <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    property_shownact <- property_show() %>% 
      dplyr::filter(active == 1)
  })
})

pptyval_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    pptyval_full <- ReadDataFromSS(db_obj, '* Frame 12 : Property Value *')
    pptyval_show <- pptyval_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order) %>% 
      dplyr::select(property_id, assessment_year, assessment_value, appraisal_value)
  })
})

pptytaxr_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    pptytaxr_full <- ReadDataFromSS(db_obj, '* Frame 13 : Property Tax Rates *')
    pptytaxr_show <- pptytaxr_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order) %>% 
      dplyr::select(year, ownership, province, area, tax_rate, source)
  })
})

pptytax_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    
    pptytax_show <- property_show() %>% 
      dplyr::select(id, ownership, province, area) %>% 
      dplyr::left_join(pptytaxr_show(), by = c('ownership','province','area')) %>% 
      dplyr::rename(property_id = id, assessment_year = year) %>% 
      dplyr::left_join(pptyval_show(), by = c("property_id","assessment_year")) %>% 
      dplyr::mutate(annual_amount = round(assessment_value * tax_rate))
  })
})

amort_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    amort_full <- ReadDataFromSS(db_obj, '* Frame 14 : Amortization *')
    amort_show <- amort_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order) %>% 
      dplyr::mutate(pmt_date = as.Date(pmt_date)) %>% 
      dplyr::select(property_id, pmt_date, principal, interest, end_balance)
  })
})

demogra_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    demogra_full <- ReadDataFromSS(db_obj, '* Input 05 : Demographics *')
    demogra_show <- demogra_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order)
  })
})

taxpar_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    taxpar_full <- ReadDataFromSS(db_obj, '* Input 06 : Tax Parameters *')
    taxpar_show <- taxpar_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order)
  })
})

taxtbl_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    taxtbl_full <- ReadDataFromSS(db_obj, '* Input 07 : Personal Income Tax Table *')
    taxtbl_show <- taxtbl_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order)
  })
})

intrt_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    intrt_full <- ReadDataFromSS(db_obj, '* Frame 20 : Interest Rates *')
    intrt_show <- intrt_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order)
  })
})

land_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    land_full <- ReadDataFromSS(db_obj, '* Frame 15 : Land *')
    land_show <- land_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order)
  })
})

lafftt_input_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    lafftt_input_full <- ReadDataFromSS(db_obj, '* Frame 16 : Loan afforability test input *')
    lafftt_input_show <- lafftt_input_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order)
  })
})

lafftt_output_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    lafftt_output_full <- ReadDataFromSS(db_obj, '* Frame 17 : Loan afforability test output *')
    lafftt_output_show <- lafftt_output_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order)
  })
})

pernw_show <- reactive({
  
  withProgress(message = 'Getting transaction details ...', {
    pernw_full <- ReadDataFromSS(db_obj, '* Input 08 : Net Worth *')
    pernw_show <- pernw_full %>% 
      dplyr::filter(show == 1) %>% 
      dplyr::arrange(order)
  })
})

autoInvalidate <- reactiveTimer(1000*60*15)

observe({
  
  autoInvalidate()
  
  updateDateInput(session, 'pf_ipt_par_begdt', value = lubridate::floor_date(Sys.Date(),"month") - months(1))
  updateDateInput(session, 'pf_ipt_par_enddt', value = lubridate::ceiling_date(Sys.Date(),"month") - months(1) - days(1))
  
})

cache_loan_mrtg_pymts <- reactive({
  
  meat <- lafftt_input_show()
  house_prcs <- meat$default_value[meat$subcategory == 'new loan']/0.8
  ids <- meat$id[meat$subcategory == 'new loan']
  
  mrtg_tbl <- AmortTableConstr(house_prcs, 570000, 0.04*100)
  res <- mrtg_tbl$ft$monthly_payment
  names(res) <- ids
  
  return(res)
  
})