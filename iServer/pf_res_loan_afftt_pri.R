##
# tab

output$pf_res_loan_afftt_pfttst <- renderUI({
  
  prvs <- unique(pfttst_show()$province)
  
  tagList(
    fluidRow(
     lapply(1:length(prvs), function(i){
       itms_per_prv <- pfttst_show() %>% dplyr::filter(province == prvs[i])
       col_width <- itms_per_prv$column_width[1]
       
       column(
         col_width,
         
         fluidRow(
           column(
             12,
             
             tags$div(
               class = 'block_inner_frame',
               
               tags$h4(class = 'block_title', prvs[i]),  
                
               lapply(1:nrow(itms_per_prv), function(j){
                 
                 pptyval_j <- itms_per_prv$property_value[j]
                 lnval_j <- itms_per_prv$loan_value[j]
                 itrt_j <- itms_per_prv$interest_rate[j]
                 amrprd_j <- itms_per_prv$amortization_period[j]
                 ownsip_j <- itms_per_prv$ownership[j]
                 prv_j <- itms_per_prv$province[j]
                 area_j <- itms_per_prv$area[j]
                 
                 # mortgage payment
                 mrtg_tbl <- AmortTableConstr(pptyval_j, 0, itrt_j*100, amrprd_j)
                 mrtg_pymt <- mrtg_tbl$ft$monthly_payment
                 
                 # property tax
                 ppty_tax_master <- pptytaxr_show() %>%
                   dplyr::filter(year == lubridate::year(input$pf_ipt_par_begdt)) %>%
                   dplyr::filter(ownership == ownsip_j) %>%
                   dplyr::filter(province == prv_j) %>%
                   dplyr::filter(area == area_j)
                 ppty_tax <- pptyval_j * ppty_tax_master$tax_rate[1]/12
                 tot_exp <- mrtg_pymt + ppty_tax
                 
                 fluidRow(
                   column(
                     12,
                     
                     tags$div(
                       tagList(
                         tags$div(class = "task_div", textInput(paste0("pfttst_",i,"_pptyval",j), label = "property value", value = scales::comma(pptyval_j, accuracy = 1), width = entry_wid_s)),
                         tags$div(class = "task_div", textInput(paste0("pfttst_",i,"_lnval",j), label = "loan amount", value = scales::comma(lnval_j, accuracy = 1), width = entry_wid_s)),
                         tags$div(class = "task_div", textInput(paste0("pfttst_",i,"_itrt",j), label = "interest rate", value = scales::percent(itrt_j, accuracy = 0.01), width = entry_wid_s)),
                         tags$div(class = "task_div", textInput(paste0("pfttst_",i,"_amrprd",j), label = "amortization (yrs)", value = amrprd_j, width = entry_wid_s)),
                         tags$div(class = "task_div", textInput(paste0("pfttst_",i,"_mtgpymt",j), label = "mortgage", value = scales::comma(mrtg_pymt, accuracy = 1), width = entry_wid_s)),
                         tags$div(class = "task_div", textInput(paste0("pfttst_",i,"_ppttax",j), label = "property tax", value = scales::comma(ppty_tax, accuracy = 1), width = entry_wid_s)),
                         tags$div(class = "task_div", textInput(paste0("pfttst_",i,"_totpymt",j), label = "total expense", value = scales::comma(tot_exp, accuracy = 1), width = entry_wid_s))
                         # add more tag here
                       )
                     )
                   )
                 )
                 
                 
               })
               
             )
           )
         )
       )
       
     }) 
    )
  )
  
})


output$pf_res_loan_afftt_ipt <- renderUI({
  
  cats <- unique(lafftt_input_show()$category)
  
  tagList(
    fluidRow(
      
      lapply(1:length(cats), function(i){
        itms_per_cat <- lafftt_input_show() %>% dplyr::filter(category == cats[i])
        col_width <- itms_per_cat$column_width[1]
        subcats <- unique(itms_per_cat$subcategory)
        
        column(
          col_width,
          
          fluidRow(
            column(
              12,
              
              tags$div(
                class = 'block_inner_frame',
                
                tags$h4(class = 'block_title', cats[i]),
                
                fluidRow(
                  lapply(1:length(subcats), function(j){
                    itms_per_subcat <- itms_per_cat %>% dplyr::filter(subcategory == subcats[j])
                    ips_ids <- itms_per_subcat$id
                    ips_names <- itms_per_subcat$item
                    ips_defval <- itms_per_subcat$default_value
                    ips_used_names <- paste0(ips_names, " ($", scales::comma(ips_defval, accuracy = 1) , ")")
                    ips_defswt <- itms_per_subcat$default_switch
                    ips_freq <- itms_per_subcat$freq
                    ips_furp <- itms_per_subcat$further_processing
                    ips_button <- itms_per_subcat$button[1]
                    
                    column(
                      12/length(subcats),
                      
                      if(ips_button == 'checkbox') {
                        checkboxGroupInput(paste0('pf_res_loan_afftt_ipt_', subcats[j]), subcats[j], selected = ips_ids[as.logical(ips_defswt)],
                                           choiceNames = ips_used_names, choiceValues = ips_ids)
                      } else if (ips_button == 'radio') {
                        radioButtons(paste0('pf_res_loan_afftt_ipt_', subcats[j]), subcats[j], selected = ips_ids[as.logical(ips_defswt)],
                                     choiceNames = ips_used_names, choiceValues = ips_ids)
                      } else {
                        # nothing yet
                      }
                      
                    )
                    
                    
                  })
                )
              )
            )
          )
          
        )
      })
      
    ),
    
    fluidRow(
      column(
        8,
        actionButton(class = 'btn-info', 'pf_res_loan_afftt_ipt_run_test', label = 'run test', width = entry_wid_m)
      )
    )
  )

  
})

observeEvent(input$pf_res_loan_afftt_ipt_run_test, {
  
  output$pf_res_loan_afftt_opt <- renderUI({
    # get values
    tax_rate <- 0.40
    ipt_vals <- lafftt_input_show()$default_value
    names(ipt_vals) <- lafftt_input_show()$id
    # browser()
    
    # income
    opt_gpi_ann <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'personal')]]])
    opt_gpi <- opt_gpi_ann/12
    opt_npi <- opt_gpi * (1 - tax_rate)
    opt_ri <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'rental')]]])
    ri_rd <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'rent inl ratio')]]])
    opt_ri_rd <- opt_ri * ri_rd
    opt_gti <- opt_gpi + opt_ri_rd
    opt_nti <- opt_npi + opt_ri
    
    # loan
    opt_ps <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'item')]]])
    opt_loan <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'new loan')]]])
    opt_npv <- opt_loan/0.8
    opt_dp <- opt_npv - opt_loan
    opt_rb <- opt_ps
    opt_mrgy_pymt_existing <- 0
    opt_mrgt_pymt <- cache_loan_mrtg_pymts()[input[[paste0('pf_res_loan_afftt_ipt_', 'new loan')]]]
    
    ppty_tax_master <- pptytaxr_show() %>%
      dplyr::filter(year == lubridate::year(input$pf_ipt_par_begdt)) %>%
      dplyr::filter(ownership == 'residential') %>%
      dplyr::filter(province == 'BC') %>%
      dplyr::filter(area == 'sannich')
    opt_ppty_tax <- opt_npv * ppty_tax_master$tax_rate[1]/12
    opt_ne <- opt_mrgy_pymt_existing + opt_mrgt_pymt + opt_ppty_tax
    
    # old expenses
    opt_exp_stmary <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'st mary')]]])
    opt_exp_delora <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'delora')]]])
    opt_exp_archangel <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'archangel')]]])
    opt_exp_human <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'human')]]])
    opt_ee_actual <- opt_exp_stmary + opt_exp_delora + opt_exp_archangel + opt_exp_human
    opt_ee_bank <- opt_exp_stmary*0.8 + opt_exp_delora*0.85 + opt_exp_archangel*0.7 + opt_exp_human*0.75
   
    # summarize expense
    opt_te_actual <- opt_mrgy_pymt_existing + opt_mrgt_pymt + opt_ppty_tax + opt_ee_actual
    opt_te_bank <- opt_mrgy_pymt_existing + opt_mrgt_pymt + opt_ppty_tax + opt_ee_bank
    #print(opt_te_bank)
    
    # ratio
    opt_atdsr_actual <- opt_te_actual/opt_gti
    opt_atdsr_bank <- opt_te_bank/opt_gti
    opt_nsp <- opt_nti - opt_te_actual
    tres <- c(opt_atdsr_bank, opt_nsp)
    
    # testing output
    res <- 'pass'
    ab_lbl <- 'btn-warning'
    
    # setup structure
    itms <- lafftt_output_show()$item
    deps <- lafftt_output_show()$description
    fmls <- lafftt_output_show()$formula
    tgts <- lafftt_output_show()$target
    names(tres) <- itms
    
    fluidRow(
      column(
        12,
        
        tags$div(
          class = 'block_inner_frame',
          
          fluidRow(
            column(
              3,
              tagList(
                tags$h4(class = 'block_title', 'existing expense'),
                tags$h5(class = 'block_summary', 'property'),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'st mary')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee_stb"), label = NULL, value = scales::comma(opt_exp_stmary, accuracy = 1))))
                  )
                ),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'delora')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee_delora"), label = NULL, value = scales::comma(opt_exp_delora, accuracy = 1))))
                  )
                ),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'archangel')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee_archangel"), label = NULL, value = scales::comma(opt_exp_archangel, accuracy = 1))))
                  )
                ),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'human')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee_human"), label = NULL, value = scales::comma(opt_exp_human, accuracy = 1))))
                  )
                ),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'total actual expense')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee_tae"), label = NULL, value = scales::comma(opt_ee_actual, accuracy = 1))))
                  )
                ),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'total mortgage expense')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee_tme"), label = NULL, value = scales::comma(opt_ee_bank, accuracy = 1))))
                  )
                )
              )
            ),
            column(
              3,
              tagList(
                tags$h4(class = 'block_title', 'new loan expense'),
                tags$h5(class = 'block_summary', 'mortgage and property tax'),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'cash balance')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_rb"), label = NULL, value = scales::comma(opt_rb, accuracy = 1))))
                  )
                ),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'down payment')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_dp"), label = NULL, value = scales::comma(opt_dp, accuracy = 1))))
                  )
                ),
                
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'new loan amount')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_la"), label = NULL, value = scales::comma(opt_loan, accuracy = 1))))
                  )
                ),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'new property market value')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_npv"), label = NULL, value = scales::comma(opt_npv, accuracy = 1))))
                  )
                ),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'new loan mortgage (7% I.R.)')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_nlm"), label = NULL, value = scales::comma(opt_mrgt_pymt, accuracy = 1))))
                  )
                ),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'new loan property tax')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_nlpt"), label = NULL, value = scales::comma(opt_ppty_tax, accuracy = 1))))
                  )
                ),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'new loan total expense')),
                            tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ne"), label = NULL, value = scales::comma(opt_ne, accuracy = 1))))
                  )
                )
              )
            ),
            column(
              6,
              fluidRow(
                
                lapply(1:length(itms), function(i){
                  
                  column(
                    12/length(itms),
                    
                    tags$h4(class = 'block_title', deps[i]),
                    tags$h5(class = 'block_summary', fmls[i]),
                    
                    if(itms[i] == 'tdsr_bk'){
                      if(tres[itms[i]] <= tgts[i]){
                        res <- c('pass','fail')[1]
                        ab_lbl <- 'btn-success'
                      } else {
                        res <- c('pass','fail')[2]
                        ab_lbl <- 'btn-primary'
                      }
                      tagList(
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'gross personal income')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_gpi",i), label = NULL, value = scales::comma(opt_gpi, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", paste0('rental income (', scales::percent(ri_rd, accuracy = 1),')'))),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ri",i), label = NULL, value = scales::comma(opt_ri_rd, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'total gross income')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ti",i), label = NULL, value = scales::comma(opt_gti, accuracy = 1))))
                          )
                        ), tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'new loan expense')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ne"), label = NULL, value = scales::comma(opt_ne, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'existing expense')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee",i), label = NULL, value = scales::comma(opt_ee_bank, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'total expense')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_te",i), label = NULL, value = scales::comma(opt_te_bank, accuracy = 1))))
                          )
                        ), tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'actual ratio')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_atdsr",i), label = NULL, value = scales::percent(opt_atdsr_bank))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'maximum ratio')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ttdsr",i), label = NULL, value = scales::percent(tgts[i]))))
                          )
                        ),tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", '')),
                                    tags$td(width = "50%", actionButton(class = ab_lbl, paste0("pf_res_loan_afftt_opt_res",i), label = res, width = entry_wid_m)))
                          )
                        )
                      )
                      
                    } else if (itms[i] == 'tdsr_actl') {
                      if(tres[itms[i]] <= tgts[i]){
                        res <- c('pass','fail')[1]
                        ab_lbl <- 'btn-success'
                      } else {
                        res <- c('pass','fail')[2]
                        ab_lbl <- 'btn-primary'
                      }
                      tagList(
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'gross personal income')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_gpi",i), label = NULL, value = scales::comma(opt_gpi, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", paste0('rental income (', scales::percent(ri_rd, accuracy = 1),')'))),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ri",i), label = NULL, value = scales::comma(opt_ri_rd, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'total gross income')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ti",i), label = NULL, value = scales::comma(opt_gti, accuracy = 1))))
                          )
                        ), tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'new loan expense')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ne",i), label = NULL, value = scales::comma(opt_ne, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'existing expense (housing)')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee",i), label = NULL, value = scales::comma(opt_ee_actual, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'total expense (housing)')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_te",i), label = NULL, value = scales::comma(opt_te_actual, accuracy = 1))))
                          )
                        ), tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'actual ratio')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_agdsr",i), label = NULL, value = scales::percent(opt_atdsr_actual))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'maximum ratio')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_tgdsr",i), label = NULL, value = scales::percent(tgts[i]))))
                          )
                        ),tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", '')),
                                    tags$td(width = "50%", actionButton(class = ab_lbl, paste0("pf_res_loan_afftt_opt_res",i), label = res, width = entry_wid_m)))
                          )
                        )
                      )
                      
                    } else if (itms[i] == 'nsp') {
                      if(tres[itms[i]] >= tgts[i]){
                        res <- c('pass','fail')[1]
                        ab_lbl <- 'btn-success'
                      } else {
                        res <- c('pass','fail')[2]
                        ab_lbl <- 'btn-primary'
                      }
                      tagList(
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'net personal income (60%)')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_npi",i), label = NULL, value = scales::comma(opt_npi, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'rental income')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ri",i), label = NULL, value = scales::comma(opt_ri, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'total net income')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ti",i), label = NULL, value = scales::comma(opt_nti, accuracy = 1))))
                          )
                        ), tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'new loan expense')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ne",i), label = NULL, value = scales::comma(opt_ne, accuracy = 1))))
                          )
                        ), 
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'existing expense')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee",i), label = NULL, value = scales::comma(opt_ee_actual, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'total expense')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_te",i), label = NULL, value = scales::comma(opt_te_actual, accuracy = 1))))
                          )
                        ), tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'actual spending power')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ansp",i), label = NULL, value = scales::comma(opt_nsp, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'minimum spending power')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_tnsp",i), label = NULL, value = scales::comma(tgts[i], accuracy = 1))))
                          )
                        ),tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", '')),
                                    tags$td(width = "50%", actionButton(class = ab_lbl, paste0("pf_res_loan_afftt_opt_res",i), label = res, width = entry_wid_m)))
                          )
                        )
                      )
                      
                    } else {
                      # nothing
                    }
                    
                  )
                  
                  
                })
                
              )
            )
          )
          
        )
      )
    )
    
    
    
  })
})















