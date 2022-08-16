#
# Shiny server
#
mainServer <- function(input, output, session) {
  
  ##
  # Load input files
  ##
  source("./iServer/pf_rpt_pernw.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_ipt_par.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_rpt_dashbd.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_rpt_expsumm.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_rpt_rntsumm.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_rpt_taxtble.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_ipt_trn.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_ipt_ppm.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_fle_dl.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_dat_exp.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_res_nppty.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/pf_res_loan_afftt_pri.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/bz_ipt_par.R", local = TRUE)
  
  ##
  # Load input files
  ##
  source("./iServer/bz_rpt_exp4buz.R", local = TRUE)
}
