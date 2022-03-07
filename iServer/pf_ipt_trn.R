#
# handling economic indicators
#
lapply(1:nrow(accounts_show), function(i){
  
  curr_acct_id <- accounts_show$id[i]
  curr_acct_nm <- accounts_show$name[i]
  
  output[[curr_acct_id]] <- renderUI({
    
    withProgress(message = 'Retrieving transaction details ...', {
      
      req(input$pf_mint_trans)
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$pf_mint_trans$datapath, header = TRUE, stringsAsFactors = FALSE)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      temp <- df %>%
        dplyr::mutate(Date = as.Date(Date, format = '%m/%d/%Y')) %>% 
        dplyr::filter(Date >= input$pf_ipt_par_begdt) %>% 
        dplyr::filter(Date <= input$pf_ipt_par_enddt) %>% 
        dplyr::filter(Account.Name == curr_acct_nm) %>% 
        dplyr::mutate(NewAmount = ifelse(Transaction.Type == 'debit', Amount, -Amount)) %>% 
        dplyr::select('Date','Original.Description','NewAmount','Category','Account.Name') %>% 
        dplyr::mutate(recurring = FALSE, property = 'n/a', operation_type = 'p', data_table = dattbl_show$name[1]) %>% 
        dplyr::rename(date = Date, description = Original.Description, amount = NewAmount, category = Category, account = Account.Name)
      
      ##
      # Pre-processing some expense categories to each table
      for(z in 1:nrow(autocat_show)){
        temp <- temp %>% 
          dplyr::mutate(category = ifelse(grepl(autocat_show$string[z], description, ignore.case = TRUE), autocat_show$category[z], category)) %>% 
          dplyr::mutate(recurring = ifelse(grepl(autocat_show$string[z], description, ignore.case = TRUE), autocat_show$recurring[z], recurring)) %>% 
          dplyr::mutate(property = ifelse(grepl(autocat_show$string[z], description, ignore.case = TRUE), autocat_show$property[z], property)) %>% 
          dplyr::mutate(operation_type = ifelse(grepl(autocat_show$string[z], description, ignore.case = TRUE), autocat_show$operation_type[z], operation_type)) %>%
          dplyr::mutate(data_table = ifelse(grepl(autocat_show$string[z], description, ignore.case = TRUE), autocat_show$data_table[z], data_table)) %>% 
          dplyr::mutate(amount = ifelse(grepl(autocat_show$string[z], description, ignore.case = TRUE), autocat_show$multiplier[z]*amount, amount))
      }
      
      tasks[[i]] <<- temp %>% dplyr::arrange(category)
      
      tagList(
        fluidRow(
          column(
            12,
            tags$div(
              class = 'block_outter_frame',
              ##
              # task output
              lapply(1:nrow(tasks[[i]]), function(k){
                
                ##
                # Process data, create year and month data
                date_k <- tasks[[i]]$date[k]
                desc_k <- tasks[[i]]$description[k]
                amnt_k <- tasks[[i]]$amount[k]
                ctgr_k <- tasks[[i]]$category[k]
                recu_k <- tasks[[i]]$recurring[k]
                ppty_k <- tasks[[i]]$property[k]
                dtbl_k <- tasks[[i]]$data_table[k]
                
                fluidRow(
                  column(
                    width = 12,
                    
                    tags$div(
                      class = "block_inner_frame",
                      style = "border-bottom:0px solid gray;",
                      
                      ##
                      # Main task section
                      fluidRow(
                        column(
                          width = 12,
                          
                          tags$div(
                            
                            tagList(
                              tags$div(class = "task_div", dateInput(paste0("fp_ipt_trn_",curr_acct_id,"_date",k), label = "date", value = date_k, width = entry_wid_s)),
                              tags$div(class = "task_div", textInput(paste0("fp_ipt_trn_",curr_acct_id,"_desc",k), label = "description", value = desc_k, width = entry_wid_l)),
                              tags$div(class = "task_div", textInput(paste0("fp_ipt_trn_",curr_acct_id,"_amnt",k), label = "amount", value = amnt_k, width = entry_wid_s)),
                              tags$div(class = "task_div", selectInput(paste0("fp_ipt_trn_",curr_acct_id,"_ppty",k), label = "property", choices = c('n/a', property_show$name), selected = ppty_k, width = entry_wid_s)),
                              tags$div(class = "task_div", selectInput(paste0("fp_ipt_trn_",curr_acct_id,"_ctgr",k), label = "category", choices = transcat_show$name, selected = ctgr_k, width = entry_wid_m)),
                              tags$div(class = "task_div", selectInput(paste0("fp_ipt_trn_",curr_acct_id,"_optp",k), label = "b/p", choices = unique(opertype_show$id), selected = ctgr_k, width = entry_wid_ty)),
                              tags$div(class = "task_div", selectInput(paste0("fp_ipt_trn_",curr_acct_id,"_dtbl",k), label = "data table",  choices = dattbl_show$name, selected = dtbl_k, width = entry_wid_m)),
                              tags$div(class = "task_div", textInput(paste0("fp_ipt_trn_",curr_acct_id,"_cmts",k), label = "comments", value = "", width = entry_wid_m)),
                              tags$div(class = "task_div", style = 'padding-top:23px;margin-top:0px;', checkboxInput(paste0("fp_ipt_trn_",curr_acct_id,"_recu",k), label = "recurring", value = recu_k, width = entry_wid_s))
                              # add more tag here
                            )
                            
                          )
                        )
                      )
                      
                    )
                  )
                )
                
              }),
              
              fluidRow(
                column(
                  12,
                  
                  tags$div(
                    class = "block_inner_frame",
                    actionButton(class = "btn-primary", paste0("fp_ipt_trn_",curr_acct_id,"_enter"), "Enter into DB"),
                    actionButton(class = "btn-success", paste0("fp_ipt_trn_",curr_acct_id,"_clear"), "Clear from DB")
                  )
                  
                )
              )
            )
          )
        )
        
      )
    
    })
    
  })
  
  observeEvent(input[[paste0("fp_ipt_trn_", curr_acct_id, "_clear")]], {
    showNotification("Transactions are being removed from  DB...", type = 'message')
    sql_str <- paste0("DELETE FROM `* Input 02 : Transactions *` WHERE account = '", curr_acct_nm, 
                      "' AND transaction_date >= '", input$pf_ipt_par_begdt, 
                      "' AND transaction_date <= '", input$pf_ipt_par_enddt,"'")
    GetQueryResFromSS(db_obj, sql_str)
    
  })
  
  observeEvent(input[[paste0("fp_ipt_trn_",curr_acct_id,"_enter")]], {
    
    showNotification("Transactions are being entered to DB...", type = 'error')
    
    dates <- do.call("c", lapply(1:nrow(tasks[[i]]), function(i){input[[paste0("fp_ipt_trn_",curr_acct_id,"_date",i)]]}))
    desc <- do.call("c", lapply(1:nrow(tasks[[i]]), function(i){input[[paste0("fp_ipt_trn_",curr_acct_id,"_desc",i)]]}))
    amnt <- do.call("c", lapply(1:nrow(tasks[[i]]), function(i){input[[paste0("fp_ipt_trn_",curr_acct_id,"_amnt",i)]]}))
    ppty <- do.call("c", lapply(1:nrow(tasks[[i]]), function(i){input[[paste0("fp_ipt_trn_",curr_acct_id,"_ppty",i)]]}))
    ctgr <- do.call("c", lapply(1:nrow(tasks[[i]]), function(i){input[[paste0("fp_ipt_trn_",curr_acct_id,"_ctgr",i)]]}))
    hyctgt <- sapply(1:length(ctgr), function(x){ transcat_show$hyper_category[transcat_show$name == ctgr[x]]  })
    optp <- do.call("c", lapply(1:nrow(tasks[[i]]), function(i){input[[paste0("fp_ipt_trn_",curr_acct_id,"_optp",i)]]}))
    dtbl <- do.call("c", lapply(1:nrow(tasks[[i]]), function(i){input[[paste0("fp_ipt_trn_",curr_acct_id,"_dtbl",i)]]}))
    recu <- do.call("c", lapply(1:nrow(tasks[[i]]), function(i){input[[paste0("fp_ipt_trn_",curr_acct_id,"_recu",i)]]}))
    cmts <- do.call("c", lapply(1:nrow(tasks[[i]]), function(i){input[[paste0("fp_ipt_trn_",curr_acct_id,"_cmts",i)]]}))
    
    df <- data.frame(
      id = 0,
      transaction_date = dates,
      description = desc,
      amount = amnt,
      category = ctgr,
      hyper_category = hyctgt,
      property = ppty,
      account = curr_acct_nm,
      best_alt_account = curr_acct_nm,
      operation_type = optp,
      costco = 0,
      essential = 0,
      refundable = 0,
      receipt_kept = 0,
      recurring = as.numeric(recu),
      active = 1,
      order = 999,
      show = 1,
      entry_datetime = Sys.time(),
      comments = cmts,
      data_table = dtbl,
      stringsAsFactors = FALSE
    )
    
    df <- df %>% dplyr::filter(data_table != '* Excluded *') %>% dplyr::select(-data_table)
    if(nrow(df) > 0) WriteDataToSS(db_obj, df, '* Input 02 : Transactions *', apd = TRUE)
    
  })
  
})


