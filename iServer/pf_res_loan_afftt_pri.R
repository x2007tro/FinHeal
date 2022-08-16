##
# tab
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
    opt_ps <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'incoming')]]])
    opt_cb <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'outgoing')]]])
    opt_loan <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'new loan')]]])
    opt_npv <- 450000
    opt_dp <- opt_npv - opt_loan - 49000  # down payment already made
    opt_rb <- opt_ps + opt_cb - opt_dp
    opt_mrgy_pymt_existing <- 0
    opt_mrgt_pymt <- cache_loan_mrtg_pymts()[input[[paste0('pf_res_loan_afftt_ipt_', 'new loan')]]]
    
    
    ppty_tax_master <- pptytaxr_show() %>%
      dplyr::filter(year == lubridate::year(input$pf_ipt_par_begdt)) %>%
      dplyr::filter(ownership == 'residential_owner_occupied') %>%
      dplyr::filter(province == 'NB') %>%
      dplyr::filter(area == 'LSD')
    opt_ppty_tax <- opt_npv * ppty_tax_master$tax_rate[1]/12
    opt_ne <- opt_mrgy_pymt_existing + opt_mrgt_pymt + opt_ppty_tax
    
    # old expenses
    opt_ee <- sum(ipt_vals[names(ipt_vals) %in% input[[paste0('pf_res_loan_afftt_ipt_', 'existing')]]])
    opt_ee_no_veh <- opt_ee - ipt_vals['expense_existing_car']
    opt_te <- opt_mrgy_pymt_existing + opt_mrgt_pymt + opt_ppty_tax + opt_ee
    opt_te_no_veh <- opt_mrgy_pymt_existing + opt_mrgt_pymt + opt_ppty_tax + opt_ee_no_veh
    
    # ratio
    opt_atdsr <- opt_te/opt_gti
    opt_agdsr <- opt_te_no_veh/opt_gti
    opt_nsp <- opt_nti - opt_te
    tres <- c(opt_atdsr, opt_agdsr, opt_nsp)
    
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
                tags$h4(class = 'block_title', 'new loan'),
                tags$h5(class = 'block_summary', 'mortgage and property tax'),
                tags$div(
                  class = 'pf_res_loan_afftt_opt_div',
                  tags$table(
                    tags$tr(width = "100%",
                            tags$td(width = "50%", div(style = "", 'reno budget')),
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
                            tags$td(width = "50%", div(style = "", 'new loan mortgage (2.735% I.R.)')),
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
              9,
              fluidRow(
                
                lapply(1:length(itms), function(i){
                  
                  column(
                    12/length(itms),
                    
                    tags$h4(class = 'block_title', deps[i]),
                    tags$h5(class = 'block_summary', fmls[i]),
                    
                    if(itms[i] == 'tdsr'){
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
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee",i), label = NULL, value = scales::comma(opt_ee, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'total expense')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_te",i), label = NULL, value = scales::comma(opt_te, accuracy = 1))))
                          )
                        ), tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'actual ratio')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_atdsr",i), label = NULL, value = scales::percent(opt_atdsr))))
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
                      
                    } else if (itms[i] == 'gdsr') {
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
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee",i), label = NULL, value = scales::comma(opt_ee_no_veh, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'total expense (housing)')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_te",i), label = NULL, value = scales::comma(opt_te_no_veh, accuracy = 1))))
                          )
                        ), tags$br(),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'actual ratio')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_agdsr",i), label = NULL, value = scales::percent(opt_agdsr))))
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
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_ee",i), label = NULL, value = scales::comma(opt_ee, accuracy = 1))))
                          )
                        ),
                        tags$div(
                          class = 'pf_res_loan_afftt_opt_div',
                          tags$table(
                            tags$tr(width = "100%",
                                    tags$td(width = "50%", div(style = "", 'total expense')),
                                    tags$td(width = "50%", textInput(paste0("pf_res_loan_afftt_opt_te",i), label = NULL, value = scales::comma(opt_te, accuracy = 1))))
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















