

#install.packages("usmap")  # only if not installed
data(statepop, package = "usmap")
packages <- c("ggplot2", "readxl", "readr", "dplyr", "tidyr", "reshape2","grid", 
              "ggpubr", "gridBase", "gridExtra", "cdlTools", "stringr", "ggrepel")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])}

# Packages loading
lapply(packages, library, character.only = TRUE)


date_variable <- format(Sys.Date(), "%m.%d.%y")
startbc <- 1908   # starting birth cohort 
endbc <- 2100     # ending birth cohort
endyear <- 2200   # final calendar year, extra 100 years needed for AC-AP conversion
cohyears <- endbc-startbc+1     # number of cohort years
totalyears<-length(startbc:endyear)
v_calyears <- (startbc-startbc+1):(endbc-startbc+1) 
v_stdbirths<- rep(1000000, times = 193)
v_statefips <- c('01','02','04','05','06','08','09','10','11','12','13','15','16',
                 '17','18','19','20','21','22','23','24','25','26','27','28','29',
                 '30','31','32','33','34','35','36','37','38','39','40','41','42',
                 '44','45','46','47','48','49','50','51','53','54','55','56')

fips_to_abbr <- function(fips2) {
  fips2 <- sprintf("%02d", as.integer(fips2))
  lut <- setNames(usmap::statepop$abbr, usmap::statepop$fips)
  abbr <- unname(lut[fips2])
  if (is.na(abbr)) stop("Unknown state FIPS: ", fips2)
  abbr
}

#===============================================================================
# cohort life-course smoking transitions, output prevalence
#===============================================================================

generate_prevs <- function(startbc, gender, m_init.policy_AC, m_cess.policy_AC, m_initAC, 
                          m_cessAC, p_mortNS_AC, p_mortCS_AC, p_mortYSQ_AC, v_stdbirths){
  
  # file to store output with multi former smoker compartments
  
  v_namesoutput <- c("gender","cohort","age","year","baseline_initiation_rate",
                     "baseline_cessation_rate", "initiation_rate","cessation_rate",
                     "survivors","alive_smokers","smoking_prevalence", "former_prevalence", 
                     paste0("former_smokers_YSQ",1:40))
  
  m_output <- matrix(0,100*(endbc-startbc+1),length(v_namesoutput))
  colnames(m_output) <- v_namesoutput
  
  # Initialize matrices to store population by smoking status for all cohorts
  m_NSprevAC <- m_CSprevAC <- m_popAC <- matrix(0,100,endbc-startbc+1)
  colnames(m_NSprevAC) <- colnames(m_CSprevAC) <- colnames(m_popAC) <- as.character(startbc:endbc)
  a_FSprevAC <- array(0,dim=c(100,endbc-startbc+1,40))
  
  COUNT=1  # counter for output file
  for (cohort in startbc:endbc){  # loop over cohorts
    numcoh <- as.numeric(cohort)
    charcoh <- as.character(cohort)
    
    v_p_smkinitpol <- m_init.policy_AC[,charcoh]   # initiation probabilities for current cohort (policy)
    v_p_smkcesspol <- m_cess.policy_AC[,charcoh]   # cessation probabilities for current cohort (policy)
    v_p_smkinitbase <- m_initAC[,charcoh]    # initiation probabilities for current cohort (baseline)
    v_p_smkcessbase <- m_cessAC[,charcoh]    # cessation probabilities for current cohort (baseline)
    
    ### death probabilities for current cohort
    v_p_mort.NS <- p_mortNS_AC[p_mortNS_AC[,1]==numcoh,5]       # death probabilities for Never Smokers
    v_p_mort.CS <- p_mortCS_AC[p_mortCS_AC[,1]==numcoh,5]        # death probabilities for Current Smokers
    v_p_mort.YSQ <- p_mortYSQ_AC[p_mortYSQ_AC[,1,1]==numcoh,5,]       # death probabilities for Former Smokers by YSQ
    
    # Matrices to store population by age for current cohort
    m_NS <- matrix(0,100,1)                    # matrix for Never Smokers
    m_CS <- matrix(0,100,1)                    # matrix for Current Smokers
    m_FS_YSQ <- matrix(0,100,40)               # matrix for Former Smokers by YSQ
    
    ## Fill output for age 0
    m_NS[1] <- v_stdbirths[cohort-startbc+1]
    m_output[COUNT,] <- c(gender,cohort,0,cohort,0,0,0,0,m_NS[1],0,0,0,0*(1:40))
    COUNT <- COUNT+1
    
    for (i in 2:100) {  # loop over age
      m_NS[i] <- m_NS[i-1]*(1-v_p_smkinitpol[i-1])*(1-v_p_mort.NS[i-1])
      
      m_CS[i] <- m_NS[i-1]*v_p_smkinitpol[i-1]*(1-v_p_mort.NS[i-1])+m_CS[i-1]*(1-v_p_smkcesspol[i-1])*(1-v_p_mort.CS[i-1]) 
      
      m_FS_YSQ[i,1] <- m_CS[i-1]*(v_p_smkcesspol[i-1])*(1-v_p_mort.CS[i-1])
      
      for (j in 2:39){
        m_FS_YSQ[i,j] <- m_FS_YSQ[i-1,j-1]*(1-v_p_mort.YSQ[i-1,j-1]) 
      }
      m_FS_YSQ[i,40] <- m_FS_YSQ[i-1,39]*(1-v_p_mort.YSQ[i-1,39]) + m_FS_YSQ[i-1,40]*(1-v_p_mort.YSQ[i-1,40]) 
      
      # store populations by smoking status and prevalence and initiation/cessation rates in output file
      survivors <- m_NS[i]+m_CS[i]+sum(m_FS_YSQ[i,])
      m.FS_YSQsum <- sum(m_FS_YSQ[i,]) # using multiple former compartments
      
      m_output[COUNT,] <- c(gender,cohort,i-1,cohort+i-1,v_p_smkinitbase[i],v_p_smkcessbase[i],v_p_smkinitpol[i],v_p_smkcesspol[i],survivors,m_CS[i],m_CS[i]/survivors,m.FS_YSQsum/survivors,m_FS_YSQ[i,])
      COUNT <- COUNT+1
    }
    
    # total population
    m_totalpopbc <- m_NS + m_CS+rowSums(m_FS_YSQ)
    
    # store prevalence for current cohort
    m_NSprevAC[,charcoh] <- m_NS/m_totalpopbc
    m_CSprevAC[,charcoh] <- m_CS/m_totalpopbc
    m_popAC[,charcoh] <- m_totalpopbc
    
    for (j in 1:40){
      a_FSprevAC[,cohort-startbc+1,j] <- m_FS_YSQ[,j]/m_totalpopbc
    }
  }
  
  m_popAP <- matrix(0,nrow=100,ncol=cohyears+100)
  m_CSprevAP <-  m_NSprevAP <-  matrix(0, nrow=100, ncol= (cohyears+100))
  a_FSprevAP <- array(0,dim=c(100,(cohyears+100),40))
  
  # format by calendar year (AP) instead of by cohort (AC)
  for (byr in 1:cohyears){
    for (age in 0:99){
      m_popAP[age+1,byr+age] <- m_popAC[age+1,byr]
      m_CSprevAP[age+1,byr+age] <- m_CSprevAC[age+1,byr]
      m_NSprevAP[age+1,byr+age] <- m_NSprevAC[age+1,byr]
      
      for (j in 1:40){
        a_FSprevAP[age+1,byr+age,j] <- a_FSprevAC[age+1,byr,j]
      }
    }
  }
  
  m_smokersAP <- m_CSprevAP*m_popAP
  colnames(m_popAP) <- colnames(m_CSprevAP) <- colnames(m_NSprevAP) <- colnames(a_FSprevAP) <- as.character (startbc:endyear)
  
  return(list(m_output= m_output, m_NSprevAC= m_NSprevAC, m_CSprevAC= m_CSprevAC,
              a_FSprevAC= a_FSprevAC, m_popAC= m_popAC, m_NSprevAP= m_NSprevAP,
              m_CSprevAP= m_CSprevAP, a_FSprevAP= a_FSprevAP, m_popAP= m_popAP, m_smokersAP= m_smokersAP)) 
}


#===============================================================================
# Calculate number of SADs, and YLL using output from generate_prev function
#===============================================================================

calculate_mort <- function(l_prev_outputs, m_p_mortNS_AP, m_p_mortCS_AP,
                           a_p_mortYSQ_AP, m_NS.LE, df_census_data) {
  
  m_popdist <- as.matrix(cbind(df_census_data, rep(df_census_data[cohyears], 100)))
  colnames(m_popdist) <- as.character(startbc:endyear)
  
  m_SAD_AP <- m_popdist[, v_calyears] * (l_prev_outputs$m_CSprevAP[, v_calyears] * (m_p_mortCS_AP - m_p_mortNS_AP))
  
  for (j in 1:40) {
    m_SAD_AP <- m_SAD_AP + m_popdist[, v_calyears] * l_prev_outputs$a_FSprevAP[, v_calyears, j] *
      (a_p_mortYSQ_AP[, , j] - m_p_mortNS_AP)
  }
  
  df_SAD_AP <- as.data.frame(m_SAD_AP)
  v_SADyear <- colSums(df_SAD_AP)
  
  df_YLL_AP <- df_SAD_AP * m_NS.LE
  v_YLLyear <- colSums(df_YLL_AP)
  
  m_SAD_AP <- as.matrix(df_SAD_AP)
  m_YLL_AP <- as.matrix(df_YLL_AP)
  
  n <- nrow(m_SAD_AP)
  m <- ncol(m_SAD_AP)
  
  m_SAD_AC <- matrix(NA, nrow = n, ncol = m)
  m_YLL_AC <- matrix(NA, nrow = n, ncol = m)
  
  for (i in 1:n) {
    for (j in 1:m) {
      m_SAD_AC[i, j - i + 1] <- m_SAD_AP[i, j]
      m_YLL_AC[i, j - i + 1] <- m_YLL_AP[i, j]
    }
  }
  
  colnames(m_SAD_AC) <- as.character(startbc:endbc)
  colnames(m_YLL_AC) <- as.character(startbc:endbc)
  rownames(m_SAD_AC) <- 0:99
  rownames(m_YLL_AC) <- 0:99
  
  return(list(
    df_SAD_AP = df_SAD_AP, v_SADyear = v_SADyear,
    df_YLL_AP = df_YLL_AP, v_YLLyear = v_YLLyear,
    m_SAD_AC = m_SAD_AC, m_YLL_AC = m_YLL_AC
  ))
}


#===============================================================================
# loop through all US states (each state)
#===============================================================================

runstates <- function(fipscode, m.initiation.effect, m.cessation.effect){
  # Load state-specific census populations (2010-2019), smoking parameters, mortality, life expectancy
  # by smoking status, birth cohort, calendar year
  # CENSUS DATA REQUIRES SOME CLEANING FOR ANNUAL BIRTHS BY GENDER
  state_inputs_dir <- if (exists("state_input_root", inherits = TRUE)) {
    get("state_input_root", inherits = TRUE)
  } else {
    "data/state_inputs"
  }

  load(file.path(state_inputs_dir, "mort_rates", paste0("p.mort_", fipscode, ".RData"))) #mortality
  load(file.path(state_inputs_dir, paste0("smk_", fipscode, ".RData"))) #smoking init/cess cast as AC
  load(file.path(state_inputs_dir, paste0("pop_", fipscode, ".RData"))) #census pop
  load(file.path(state_inputs_dir, paste0("le_", fipscode, ".RData"))) #life expectacies

  
  # RUN STATUS QUO MODEL
  # set initiation and cessation for policy to be the same as the baseline 
  # generate_prevs() determines prevalence and calculate_mort() determines mortality outcomes
  # generate_prevs inputs: (starting cohort, gender, initiation, cessation , policy_initiation, policy_cessation,
  # neversmoker_mortality, currentsmoker_mortality, formersmoker_mortality, mortality_by_YSQ, state_number_of_births)
  
  gender <- 'Men'
  l_M.base.prev <- generate_prevs(startbc, gender, m_M.initAC, m_M.cessAC, 
                          m_M.initAC, m_M.cessAC,m_p_M.mortNS_AC,
                          m_p_M.mortCS_AC, a_p_M.mortYSQ_AC, v_stdbirths)
  
  l_M.base.mort <- calculate_mort(l_M.base.prev, m_p_M.mortNS_AP, m_p_M.mortCS_AP, 
                               a_p_M.mortYSQ_AP, m_M.NS.LE, df_M.census_data)
  
  gender <- 'Women'
  l_F.base.prev <- generate_prevs(startbc, gender, m_F.initAC, m_F.cessAC,
                               m_F.initAC, m_F.cessAC, m_p_F.mortNS_AC,
                               m_p_F.mortCS_AC, a_p_F.mortYSQ_AC, v_stdbirths)
  
  l_F.base.mort <- calculate_mort(l_F.base.prev, m_p_F.mortNS_AP, m_p_F.mortCS_AP,
                               a_p_F.mortYSQ_AP, m_F.NS.LE, df_F.census_data)
  
  ### TO APPLY SCENARIO EFFECTS, DATA NEEDS TO BE REFORMATTED TO AGE-PERIOD (AP)
  ### REFORMATTING ALONG DIAGONAL FROM AC TO AP REQUIRES EXTENDING TO 2200 (CALYEARS) 
  
  m_F.init.base_AP <- matrix(NA,100,(cohyears+100)) # initiation rates for women, age-period for baseline
  m_M.init.base_AP <- matrix(NA,100,(cohyears+100))
  m_F.cess.base_AP <- matrix(NA,100,(cohyears+100)) # cessation rates for women, age-period for baseline
  m_M.cess.base_AP <- matrix(NA,100,(cohyears+100))
  
  colnames(m_F.init.base_AP) <- colnames(m_M.init.base_AP) <- as.character (startbc:endyear)
  colnames(m_F.cess.base_AP) <- colnames(m_M.cess.base_AP) <- as.character (startbc:endyear)

  ## REFORMAT: fill these base AP matrices from the original AC matrices
  for (byr in startbc:(endbc)){
    for (age in 0:99){
      calyr <- byr+age
      m_F.init.base_AP[age+1,calyr-startbc+1] <- m_F.initAC[age+1,byr-startbc+1]
      m_M.init.base_AP[age+1,calyr-startbc+1] <- m_M.initAC[age+1,byr-startbc+1]
      m_F.cess.base_AP[age+1,calyr-startbc+1] <- m_F.cessAC[age+1,byr-startbc+1]
      m_M.cess.base_AP[age+1,calyr-startbc+1] <- m_M.cessAC[age+1,byr-startbc+1]
    }
  }
  
  ## APPLY Policy Effects TO BASELINE INITIATION
  m_F.init.policy_AP <- m_F.init.base_AP * m.initiation.effect
  m_M.init.policy_AP <- m_M.init.base_AP * m.initiation.effect
  m_F.cess.policy_AP <- m_F.cess.base_AP * m.cessation.effect
  m_M.cess.policy_AP <- m_M.cess.base_AP * m.cessation.effect

  ## REFORMAT BACK TO AGE COHORT
  m_F.init.policy_AC <- matrix(NA, nrow = 100, ncol = 0)
  m_M.init.policy_AC <- matrix(NA, nrow = 100, ncol = 0)
  m_F.cess.policy_AC <- matrix(NA, nrow = 100, ncol = 0)
  m_M.cess.policy_AC <- matrix(NA, nrow = 100, ncol = 0)
  
  # AP-to-AC conversion
  for (bc in startbc:endbc) {
    m_F.init.policy_AC <- cbind(m_F.init.policy_AC, diag(m_F.init.policy_AP[ , (bc - (startbc - 1)) : ncol(m_F.init.policy_AP)]))
    m_M.init.policy_AC <- cbind(m_M.init.policy_AC, diag(m_M.init.policy_AP[ , (bc - (startbc - 1)) : ncol(m_M.init.policy_AP)]))
    m_F.cess.policy_AC <- cbind(m_F.cess.policy_AC, diag(m_F.cess.policy_AP[ , (bc - (startbc - 1)) : ncol(m_F.cess.policy_AP)]))
    m_M.cess.policy_AC <- cbind(m_M.cess.policy_AC, diag(m_M.cess.policy_AP[ , (bc - (startbc - 1)) : ncol(m_M.cess.policy_AP)]))
  }
  
  colnames(m_F.init.policy_AC) <- colnames(m_M.init.policy_AC) <- as.character(startbc:endbc)
  colnames(m_F.cess.policy_AC) <- colnames(m_M.cess.policy_AC) <- as.character(startbc:endbc)
  
  ### RUN POLICY SCENARIOS   
  gender <- 'Men'
  l_M.policy.prev <- generate_prevs(startbc, gender, m_M.init.policy_AC, m_M.cess.policy_AC,
                                  m_M.initAC, m_M.cessAC, m_p_M.mortNS_AC,
                                  m_p_M.mortCS_AC, a_p_M.mortYSQ_AC, v_stdbirths)
  
  l_M.policy.mort <- calculate_mort(l_M.policy.prev, m_p_M.mortNS_AP, m_p_M.mortCS_AP, 
                                    a_p_M.mortYSQ_AP, m_M.NS.LE, df_M.census_data)
  
  gender <- 'Women'
  l_F.policy.prev <- generate_prevs(startbc, gender, m_F.init.policy_AC, m_F.cess.policy_AC,
                                    m_F.initAC, m_F.cessAC, m_p_F.mortNS_AC,
                                    m_p_F.mortCS_AC, a_p_F.mortYSQ_AC, v_stdbirths)
  
  l_F.policy.mort <- calculate_mort(l_F.policy.prev, m_p_F.mortNS_AP, m_p_F.mortCS_AP, 
                                    a_p_F.mortYSQ_AP, m_F.NS.LE, df_F.census_data)
  
  #------------------- format prev for outputting -----------------------------------
  ##-------------- State-specific smoking prevalence based on census population data 
  df_CSprevs.by.state <- NULL
  
  # age groups to loop through
  v_minage <- c(18, 18, 25, 45, 65)
  v_maxage <- c(99, 24, 44, 64, 99)
  
  for (i in c(1:5)){
    minage <- v_minage[i]
    maxage <- v_maxage[i]
    
    m_M.CSprev <- l_M.policy.prev$m_CSprevAP
    m_F.CSprev <- l_F.policy.prev$m_CSprevAP
    
    # Create population matrices
    m.M.pop_AP <- as.matrix(cbind(df_M.census_data, rep(df_M.census_data[cohyears], 100))) # Assume constant population sizes in future
    m.F.pop_AP <- as.matrix(cbind(df_F.census_data, rep(df_F.census_data[cohyears], 100)))
    
    colnames( m.M.pop_AP) <- colnames(m.F.pop_AP) <- as.character(startbc:endyear)
    
    # Calculate prevalence for men
    v_M.prev.minmax <- colSums(m.M.pop_AP[(minage+1):(maxage+1), ] * m_M.CSprev[(minage+1):(maxage+1), ]) / colSums(m.M.pop_AP[(minage+1):(maxage+1), ])
    
    # Calculate prevalence for women
    v_F.prev.minmax <- colSums(m.F.pop_AP[(minage+1):(maxage+1), ] * m_F.CSprev[(minage+1):(maxage+1), ]) / colSums(m.F.pop_AP[(minage+1):(maxage+1), ])
    
    # Calculate combined prevalence for both men and women
    v_numerator <- colSums(m.M.pop_AP[(minage+1):(maxage+1), ] * m_M.CSprev[(minage+1):(maxage+1), ]) + colSums(m.F.pop_AP[(minage+1):(maxage+1), ] * m_F.CSprev[(minage+1):(maxage+1), ])
    v_denominator <- colSums(m.M.pop_AP[(minage+1):(maxage+1), ]) + colSums(m.F.pop_AP[(minage+1):(maxage+1), ])
    v_B.prev.minmax <- v_numerator / v_denominator
    
    # Combine data into a single data frame for the current age group
    df_CSprevbystate_temp <- as.data.frame(rbind(
      cbind(v_M.prev.minmax[1:cohyears], "Men"),
      cbind(v_F.prev.minmax[1:cohyears], "Women"),
      cbind(v_B.prev.minmax[1:cohyears], "Both")
    ))
    
    # Set column names
    colnames(df_CSprevbystate_temp) <- c("prev", "gender")
    
    # Add additional columns
    df_CSprevbystate_temp$age <- paste0(minage, ".", maxage)
    df_CSprevbystate_temp$year <- rep(names(v_M.prev.minmax[1:cohyears]), 3)
    df_CSprevbystate_temp$state <- fipscode
    df_CSprevbystate_temp$abbr <- fips_to_abbr(fipscode)
    
    # Combine with the final data frame
    df_CSprevs.by.state <- rbind(df_CSprevs.by.state, df_CSprevbystate_temp)
  }
  
  df_CSprevs.by.state$prev<- as.numeric(df_CSprevs.by.state$prev)
  df_CSprevs.by.state$year<- as.numeric(df_CSprevs.by.state$year)
  
  #--------------format prev for output ----------------------------------------
  #-----------------------------------------------------------------------------
  
  m_M.smokers <- l_M.policy.prev$m_smokersAP
  m_F.smokers <- l_F.policy.prev$m_smokersAP
  m_M.popAP <- l_M.policy.prev$m_popAP
  m_F.popAP <- l_F.policy.prev$m_popAP
  m_M.CSprevAC <- cbind(l_M.policy.prev$m_CSprevAC, 'Male')
  m_F.CSprevAC <- cbind(l_F.policy.prev$m_CSprevAC, 'Female')
  
  l_prev_out <- list(
    state = fipscode,
    
    # policy prevalence matrices
    m_M_smokers = l_M.policy.prev$m_smokersAP,
    m_F_smokers = l_F.policy.prev$m_smokersAP,
    m_M_popAP = l_M.policy.prev$m_popAP,
    m_F_popAP = l_F.policy.prev$m_popAP,
    
    # keep full prev objects (baseline and policy)
    male_prev_obj        = l_M.policy.prev,
    female_prev_obj      = l_F.policy.prev,
    male_base_prev_obj   = l_M.base.prev,
    female_base_prev_obj = l_F.base.prev,
    
    # store full mortality objects (baseline and policy)
    male_mort_obj        = l_M.policy.mort,
    female_mort_obj      = l_F.policy.mort,
    male_base_mort_obj   = l_M.base.mort,
    female_base_mort_obj = l_F.base.mort,
    
    # store metadata needed for cohort reconstruction
    startbc   = startbc,
    endbc     = endbc,
    endyear   = endyear
  )
  
  
  #---------------format policy YLL and SADs for outputting --------------------
  #annual sum YLL
  v_M.YLLyear <- l_M.policy.mort$v_YLLyear
  v_F.YLLyear <- l_F.policy.mort$v_YLLyear
  v_B.YLLyear <- v_M.YLLyear + v_F.YLLyear
  #cumulative sum YLL
  v_M.YLLcum <- cumsum(v_M.YLLyear)
  v_F.YLLcum <- cumsum(v_F.YLLyear)
  v_B.YLLcum <- cumsum(v_B.YLLyear)
  
  #annual sum SAD
  v_M.SADyear <- l_M.policy.mort$v_SADyear
  v_F.SADyear <- l_F.policy.mort$v_SADyear
  v_B.SADyear <- v_M.SADyear+ v_F.SADyear
  #cumulative sum SAD
  v_M.SADcum <- cumsum(v_M.SADyear)
  v_F.SADcum <- cumsum(v_F.SADyear)
  v_B.SADcum <- cumsum(v_B.SADyear)
  
  #---------------- LYG using YSQ ----------------------------------------------------------------------------
  
  df_M.LYG_AP <- l_M.base.mort$df_YLL_AP - l_M.policy.mort$df_YLL_AP
  df_F.LYG_AP <- l_F.base.mort$df_YLL_AP - l_F.policy.mort$df_YLL_AP
  #annual sum LYG
  v_M.LYGyear <- colSums(df_M.LYG_AP)
  v_F.LYGyear <- colSums(df_F.LYG_AP)
  v_B.LYGyear <- v_M.LYGyear + v_F.LYGyear
  #cumulative sum LYG
  v_M.LYGcum <- cumsum(v_M.LYGyear)
  v_F.LYGcum <- cumsum(v_F.LYGyear)
  v_B.LYGcum <- cumsum(v_B.LYGyear)
  
  #annual sum SADs averted 
  v_M.SADs_averted_year <- l_M.base.mort$v_SADyear - l_M.policy.mort$v_SADyear
  v_F.SADs_averted_year <- l_F.base.mort$v_SADyear - l_F.policy.mort$v_SADyear
  v_B.SADs_averted_year <- v_M.SADs_averted_year + v_F.SADs_averted_year
  #cumulative sum SADs averted
  v_M.SADs_avert_cum <- cumsum(v_M.SADs_averted_year)
  v_F.SADs_avert_cum <- cumsum(v_F.SADs_averted_year)  
  v_B.SADs_avert_cum <- cumsum(v_B.SADs_averted_year)
 
  ##---combine mortality outputs------------------------------------------------------
  m_M.mortout <- cbind(v_M.YLLyear, v_M.YLLcum, v_M.SADyear, v_M.SADcum, 
                    v_M.LYGyear, v_M.LYGcum, v_M.SADs_averted_year,
                    v_M.SADs_avert_cum,startbc:endbc, 'Male')
  
  m_F.mortout <- cbind(v_F.YLLyear, v_F.YLLcum, v_F.SADyear, v_F.SADcum, 
                      v_F.LYGyear, v_F.LYGcum, v_F.SADs_averted_year,
                      v_F.SADs_avert_cum,startbc:endbc, 'Female')
  
  m_B.mortout <- cbind(v_B.YLLyear, v_B.YLLcum, v_B.SADyear, v_B.SADcum, 
                      v_B.LYGyear, v_B.LYGcum, v_B.SADs_averted_year,
                      v_B.SADs_avert_cum, startbc:endbc, 'Both')
  
  df_mort.outputs <- as.data.frame(rbind(m_M.mortout,m_F.mortout,m_B.mortout))
  colnames(df_mort.outputs) <- c('YLL','YLLcum','SADs', 'SADcum', 'LYG', 'LYGcum',
                              'SADsAverted','SADsAvertedcum','year', 'gender' )
  df_mort.outputs$state <- fipscode
  df_mort.outputs$abbr <- fips_to_abbr(fipscode)
  df_mort.outputs <- df_mort.outputs %>% mutate(across(c('YLL','YLLcum','SADs',
                                                         'SADcum', 'LYG', 'LYGcum', 
                                                         'SADsAverted','SADsAvertedcum',
                                                         'year'), as.numeric))


  return(list(df_mort.outputs = df_mort.outputs, 
              l_prev_out = l_prev_out, 
              df_CSprevs.by.state = df_CSprevs.by.state))
   
}

#===============================================================================
# Cigarette Tax
#===============================================================================

tax_effectCalculation <- function(initprice,tax,
                                  startbc = 1908,endyear = 2200, policyYear,
                                  inidecay  = 0.0, cesdecay  = 0.2,iniagemod = 1,cesagemod = 1,
                                  apply_inflation_adjustment = FALSE,
                                  inflation_adjustment_rate = 0.97) {
  
  ages    <- 0:99
  periods <- startbc:endyear
  
  nAges    <- length(ages)
  nPeriods <- length(periods)
  
  # Define age-specific elasticities
  ageeffects <- data.frame(
    age = ages,
    
    # # Cessation elasticity by age group
    # cess_elasticities = c(
    #   rep(0, 18),           # Ages 0–17: no cessation assumed
    #   rep(0, 8),         # Ages 18–25
    #   rep(-1.11, 14),        # Ages 26–39
    #   rep(-1.11, 60)         # Ages 40–99
    # ),
    # 
    # # Initiation elasticity by age group
    # init_elasticities = c(
    #   rep(0, 18),           # Ages 0–17
    #   rep(-0.423, 8),       # Ages 18–25 (He et al. 2025)
    #   rep(0, 14),        # Ages 26–39 (Friedman & Pesko 2022) – treated as no reduction
    #   rep(0, 60)         # Ages 40–99 (Friedman & Pesko 2022) – assumed as constant
    # )
    
    # Cessation elasticity by age group
    cess_elasticities = c(rep(0, 10), rep(-2.00, 90)),  # after age 10
    init_elasticities = c(
      rep(0, 10),    # ages 0-9
      rep(-0.4, 8),  # ages 10-17
      rep(-0.3, 7),  # ages 18-24
      rep(-0.2, 20), # ages 25-44
      rep(0, 55)     # ages 45-99
    )
    
  
  )
  
  # Initialize effect matrices
  m.initiation.effect <- matrix(1, nrow = nAges, ncol = nPeriods)
  m.cessation.effect  <- matrix(1, nrow = nAges, ncol = nPeriods)
  
  colnames(m.initiation.effect) <- colnames(m.cessation.effect) <- as.character(periods)
  
  # Loop to fill matrices
  for (j in seq_len(nPeriods)) {
    currentYear <- periods[j]
    
    if (currentYear >= policyYear) {
      timeSincePolicy <- currentYear - policyYear
      inflation_adjustment_factor <- if (apply_inflation_adjustment) {
        inflation_adjustment_rate ^ timeSincePolicy
      } else {
        1
      }
      adjusted_tax <- tax * inflation_adjustment_factor
      newprice <- initprice + adjusted_tax
      pricechange <- (newprice - initprice) / ((newprice + initprice) / 2)
      
      for (i in seq_len(nAges)) {
        # Initiation elasticity
        initEl <- ageeffects$init_elasticities[i]
        initModifier <- 1 - (-pricechange * initEl)
        m.initiation.effect[i, j] <- initModifier
        
        # Cessation elasticity
        cessEl <- ageeffects$cess_elasticities[i]
        cesseffect <- (-pricechange * cessEl) 
        cessModifier <- 1 + cesseffect * (1 - cesdecay) ^ timeSincePolicy
        m.cessation.effect[i, j]  <- cessModifier
      }
    }
  }
  
  return(list(
    m.initiation.effect = m.initiation.effect,
    m.cessation.effect  = m.cessation.effect
  ))
}
