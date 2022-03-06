##
# Global parameter
##
theme_set(theme_bw())
options(scipen = 66666)

entry_wid_s <- "105px"
entry_wid_m <- "180px"
entry_wid_l <- "275px"
entry_wid_xl <- "500px"
brewed_colors <- rep(RColorBrewer::brewer.pal(n = 9, name = "Set3"), 100)

#
# connection for database
#
db_obj <- list(
  srv = "192.168.2.200",
  prt = 3307,
  dbn = "FinHeal",
  id = "dspeast2",
  pwd = "yuheng"
)

transdata_full <- ReadDataFromSS(db_obj, '* Input 02 : Transactions *')
transdata_full <<- transdata_full %>% dplyr::filter(active == 1)

tasks <- list(100)

accounts_full <- ReadDataFromSS(db_obj, '* Frame 01 : Account *')
accounts_show <- accounts_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order) %>% 
  dplyr::select(id, name)

transcat_full <- ReadDataFromSS(db_obj, '* Frame 04 : Transaction Category *')
transcat_show <- transcat_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order) %>% 
  dplyr::select(id, name, hyper_category)

dattbl_full <- ReadDataFromSS(db_obj, '* Frame 05 : Data Table *')
dattbl_show <- dattbl_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order) %>% 
  dplyr::select(id, name)

autocat_full <- ReadDataFromSS(db_obj, '* Input 10 : Autocat *')
autocat_show <- autocat_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order) %>% 
  dplyr::mutate(recurring = as.logical(recurring)) %>% 
  dplyr::select(string, category, recurring, property, multiplier ,data_table)

property_full <- ReadDataFromSS(db_obj, '* Frame 11 : Property *')
property_show <- property_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order)
property_shownact <- property_show %>% 
  dplyr::filter(active == 1)

pptyval_full <- ReadDataFromSS(db_obj, '* Frame 12 : Property Value *')
pptyval_show <- pptyval_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order) %>% 
  dplyr::select(property_id, assessment_year, assessment_value, appraisal_value)

pptytaxr_full <- ReadDataFromSS(db_obj, '* Frame 13 : Property Tax Rates *')
pptytaxr_show <- pptytaxr_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order) %>% 
  dplyr::select(year, ownership, province, area, tax_rate, source)

pptytax_show <- property_show %>% 
  dplyr::select(id, ownership, province, area) %>% 
  dplyr::left_join(pptytaxr_show, by = c('ownership','province','area')) %>% 
  dplyr::rename(property_id = id, assessment_year = year) %>% 
  dplyr::left_join(pptyval_show, by = c("property_id","assessment_year")) %>% 
  dplyr::mutate(annual_amount = round(assessment_value * tax_rate))

amort_full <- ReadDataFromSS(db_obj, '* Frame 14 : Amortization *')
amort_show <- amort_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order) %>% 
  dplyr::mutate(pmt_date = as.Date(pmt_date)) %>% 
  dplyr::select(property_id, pmt_date, principal, interest, end_balance)

demogra_full <- ReadDataFromSS(db_obj, '* Input 05 : Demographics *')
demogra_show <- demogra_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order)

taxpar_full <- ReadDataFromSS(db_obj, '* Input 06 : Tax Parameters *')
taxpar_show <- taxpar_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order)

taxtbl_full <- ReadDataFromSS(db_obj, '* Input 07 : Personal Income Tax Table *')
taxtbl_show <- taxtbl_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order)

intrt_full <- ReadDataFromSS(db_obj, '* Frame 20 : Interest Rates *')
intrt_show <- intrt_full %>% 
  dplyr::filter(show == 1) %>% 
  dplyr::arrange(order)
