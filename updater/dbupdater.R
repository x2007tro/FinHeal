##
# Helper function for updating results in DB
##
source('./helper/dbhelper.R')
library('readxl')

##
# Construct account table
##
frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame01 account')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 01 : Account *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame02 card type')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 02 : Card Type *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame03 operation type')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 03 : Operation Type *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame04 transaction category')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 04 : Transaction Category *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame05 data table')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 05 : Data Table *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame11 property')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 11 : Property *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame12 property value')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 12 : Property Value *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame13 property tax rates')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 13 : Property Tax Rates *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame14 amortization')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 14 : Amortization *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame15 land')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 15 : Land *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'frame20 interest rates')
WriteDataToSS(db_obj, frame_account_tbl, '* Frame 20 : Interest Rates *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'input02 transactions')
WriteDataToSS(db_obj, frame_account_tbl, '* Input 02 : Transactions *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'input05 personal')
WriteDataToSS(db_obj, frame_account_tbl, '* Input 05 : Demographics *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'input06 tax parameters')
WriteDataToSS(db_obj, frame_account_tbl, '* Input 06 : Tax Parameters *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'input07 income tax brackets')
WriteDataToSS(db_obj, frame_account_tbl, '* Input 07 : Personal Income Tax Table *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'input08 net worth')
WriteDataToSS(db_obj, frame_account_tbl, '* Input 08 : Net Worth *')

frame_account_tbl <- readxl::read_excel('./updater/source.xlsx', sheet = 'input10 autocat')
WriteDataToSS(db_obj, frame_account_tbl, '* Input 10 : Autocat *')
