# Core functions

source("/Users/wangmengyao/Desktop/Github/tax-modeling/02_code/tax-model/00_config.R", local = TRUE)

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

  # Convert life expectancy from age-cohort to age-period.
  m_NS.LE_AP <- matrix(NA, nrow = 100, ncol = cohyears)
  for (i in startbc:endbc) {
    for (age in 0:99) {
      byr <- i - age
      if (byr < startbc) {
        m_NS.LE_AP[age + 1, i - startbc + 1] <- m_NS.LE[age + 1, 1]
      } else {
        m_NS.LE_AP[age + 1, i - startbc + 1] <-
          m_NS.LE[age + 1, byr - startbc + 1]
      }
    }
  }

  df_YLL_AP <- df_SAD_AP * m_NS.LE_AP
  v_YLLyear <- colSums(df_YLL_AP)
  
  m_SAD_AP <- as.matrix(df_SAD_AP)
  m_YLL_AP <- as.matrix(df_YLL_AP)
  
  n <- nrow(m_SAD_AP)
  m <- ncol(m_SAD_AP)
  
  m_SAD_AC <- matrix(NA, nrow = n, ncol = m)
  m_YLL_AC <- matrix(NA, nrow = n, ncol = m)
  
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      cohort_index <- j - i + 1L
      if (cohort_index >= 1L && cohort_index <= m) {
        m_SAD_AC[i, cohort_index] <- m_SAD_AP[i, j]
        m_YLL_AC[i, cohort_index] <- m_YLL_AP[i, j]
      }
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
  state_input_paths <- c(
    file.path(state_input_root, "mort_rates", paste0("p.mort_", fipscode, ".RData")),
    file.path(state_input_root, paste0("smk_", fipscode, ".RData")),
    file.path(state_input_root, paste0("pop_", fipscode, ".RData")),
    file.path(state_input_root, paste0("le_", fipscode, ".RData"))
  )

  load(state_input_paths[1]) # mortality
  load(state_input_paths[2]) # smoking initiation/cessation
  load(state_input_paths[3]) # census population
  load(state_input_paths[4]) # life expectancy

  
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
    df_CSprevbystate_temp$abbr <- fips_abbr(fipscode)
    
    # Combine with the final data frame
    df_CSprevs.by.state <- rbind(df_CSprevs.by.state, df_CSprevbystate_temp)
  }
  
  df_CSprevs.by.state$prev<- as.numeric(df_CSprevs.by.state$prev)
  df_CSprevs.by.state$year<- as.numeric(df_CSprevs.by.state$year)
  
  #--------------format prev for output ----------------------------------------
  #-----------------------------------------------------------------------------
  
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
  df_mort.outputs$abbr <- fips_abbr(fipscode)
  mortality_numeric_columns <- c(
    "YLL", "YLLcum", "SADs", "SADcum", "LYG", "LYGcum",
    "SADsAverted", "SADsAvertedcum", "year"
  )
  df_mort.outputs[mortality_numeric_columns] <- lapply(
    df_mort.outputs[mortality_numeric_columns],
    as.numeric
  )


  return(list(df_mort.outputs = df_mort.outputs, 
              l_prev_out = l_prev_out, 
              df_CSprevs.by.state = df_CSprevs.by.state))
   
}

#===============================================================================
# Cigarette Tax Function
tax_effectCalculation <- function(initprice, tax, startbc = 1908, endyear = 2200, policyYear,
                                  cesdecay = 0.2, apply_inflation_adjustment = FALSE,
                                  inflation_adjustment_rate = 0.97) {
  ages <- 0:99
  periods <- startbc:endyear
  
  cess_elasticities <- c(rep(0, 10), rep(-2.0, 90))
  init_elasticities <- c(
    rep(0, 10), rep(-0.4, 8), rep(-0.3, 7),
    rep(-0.2, 20), rep(0, 55)
  )
  
  m.initiation.effect <- m.cessation.effect <- matrix(
    1, nrow = length(ages), ncol = length(periods),
    dimnames = list(NULL, as.character(periods))
  )
  
  for (j in which(periods >= policyYear)) {
    time <- periods[j] - policyYear
    inflation <- if (apply_inflation_adjustment) inflation_adjustment_rate^time else 1
    newprice <- initprice + tax * inflation
    pricechange <- (newprice - initprice) / ((newprice + initprice) / 2)
    
    m.initiation.effect[, j] <- 1 + pricechange * init_elasticities
    m.cessation.effect[, j] <- 1 - pricechange * cess_elasticities * (1 - cesdecay)^time
  }
  
  list(
    m.initiation.effect = m.initiation.effect,
    m.cessation.effect = m.cessation.effect
  )
}

#===============================================================================
# Format model prevalence output for downstream analysis
prevalence_long_df <- function(prev_out, scenario_name, state_fips, state_abbr, policy_year,
                               tax_increase_dollar, year_min, year_max,
                               inflation_rate = inflation_adjustment_rate) {
  years <- as.integer(colnames(prev_out$m_M_popAP))
  keep <- years >= year_min & years <= year_max
  years <- years[keep]

  sex_df <- function(smokers, population, sex) {
    smokers <- smokers[, keep, drop = FALSE]
    population <- population[, keep, drop = FALSE]
    grid <- expand.grid(AGE = seq_len(nrow(population)) - 1L, Calendar_Year = years,
                        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

    grid$scenario <- scenario_name
    grid$policy_year <- policy_year
    grid$tax_increase_dollar <- tax_increase_dollar
    grid$inflation_adjustment_rate <- inflation_rate
    grid$state_fips <- sprintf("%02d", as.integer(state_fips))
    grid$state_abbr <- state_abbr
    grid$sex <- sex
    grid$START_YOB <- grid$Calendar_Year - grid$AGE
    grid$END_YOB <- grid$START_YOB
    grid$population <- as.vector(population)
    grid$smokers <- as.vector(smokers)
    grid$prevalence <- ifelse(grid$population > 0, grid$smokers / grid$population, NA_real_)

    grid[, c(
      "scenario", "policy_year", "tax_increase_dollar", "inflation_adjustment_rate",
      "state_fips", "state_abbr", "sex", "START_YOB", "END_YOB", "AGE",
      "Calendar_Year", "population", "smokers", "prevalence"
    )]
  }

  rbind(
    sex_df(prev_out$m_M_smokers, prev_out$m_M_popAP, "Male"),
    sex_df(prev_out$m_F_smokers, prev_out$m_F_popAP, "Female")
  )
}

#===============================================================================
# Format age-sex-year cigarette consumption category probabilities.
cpd_long_df <- function(df, state_df, year_min, year_max) {
  out <- df[df$per >= year_min & df$per <= year_max,
            c("st_fips", "coh", "age", "per", "sex", paste0("p_v1_cpd", 1:6)),
            drop = FALSE]

  out$st_fips <- sprintf("%02d", as.integer(out$st_fips))
  out$state_abbr <- state_df$state_abbr[match(out$st_fips, state_df$state_fips)]
  out$sex <- ifelse(out$sex == 1, "Male", "Female")
  out$START_YOB <- as.integer(out$coh)
  out$END_YOB <- out$START_YOB
  out$AGE <- as.integer(out$age)
  out$Calendar_Year <- as.integer(out$per)

  out <- out[order(as.integer(out$st_fips), out$sex, out$START_YOB,
                   out$AGE, out$Calendar_Year), , drop = FALSE]
  out <- out[, c(
    "st_fips", "state_abbr", "sex", "START_YOB", "END_YOB", "AGE",
    "Calendar_Year", paste0("p_v1_cpd", 1:6)
  )]
  names(out)[names(out) == "st_fips"] <- "state_fips"
  names(out)[match(paste0("p_v1_cpd", 1:6), names(out))] <- paste0("CAT", 1:6)
  out
}

#===============================================================================
# Format one population workbook sheet as age-sex-year long data.
pop_long_df <- function(workbook_path, sheet_name, lookup_df) {
  raw_df <- suppressMessages(readxl::read_excel(
    workbook_path,
    sheet = sheet_name,
    .name_repair = "minimal"
  ))

  sheet_parts <- strsplit(sheet_name, "-", fixed = TRUE)[[1]]
  state_abbr <- sheet_parts[1]
  sex_value <- sheet_parts[2]
  state_fips <- lookup_df$state_fips[match(state_abbr, lookup_df$state_abbr)][1]

  if (is.na(state_fips) || nrow(raw_df) == 0) return(NULL)

  year_cols <- names(raw_df)[grepl("^[0-9]{4}$", names(raw_df))]
  if (length(year_cols) == 0) return(NULL)

  age_values <- suppressWarnings(as.integer(raw_df[[1]]))
  raw_df <- raw_df[!is.na(age_values), , drop = FALSE]
  age_values <- age_values[!is.na(age_values)]
  population_wide_df <- raw_df[, year_cols, drop = FALSE]

  if (max(age_values, na.rm = TRUE) == 85L && sum(age_values == 85L) == 1L) {
    age_85_population <- population_wide_df[age_values == 85L, , drop = FALSE]
    expanded_85_population <- age_85_population[
      rep(1L, length(seer_age_85_plus_weights)),
      ,
      drop = FALSE
    ]
    expanded_85_population[] <- lapply(
      expanded_85_population,
      function(values) as.numeric(values) * seer_age_85_plus_weights
    )
    population_wide_df <- rbind(
      population_wide_df[age_values < 85L, , drop = FALSE],
      expanded_85_population
    )
    age_values <- c(
      age_values[age_values < 85L],
      85L + seq_along(seer_age_85_plus_weights) - 1L
    )
  }

  stacked_values <- stack(population_wide_df)
  data.frame(
    state_fips = sprintf("%02d", as.integer(state_fips)),
    state_abbr = state_abbr,
    sex = sex_value,
    AGE = rep(age_values, times = length(year_cols)),
    Calendar_Year = as.integer(as.character(stacked_values$ind)),
    population_update = as.numeric(stacked_values$values),
    stringsAsFactors = FALSE
  )
}

#===============================================================================
# Project population beyond the last supplied year.
pop_project <- function(df, start_year = 2025L, base_year = 2030L, end_year = 2035L) {
  id_cols <- c("state_fips", "state_abbr", "sex", "AGE")

  pop_year <- function(year, value_name) {
    result <- df[
      df$Calendar_Year == year,
      c(id_cols, "population_update"),
      drop = FALSE
    ]
    names(result)[names(result) == "population_update"] <- value_name
    result
  }

  growth_rates <- merge(
    pop_year(start_year, "population_start"),
    pop_year(base_year, "population_base"),
    by = id_cols,
    all = FALSE,
    sort = FALSE
  )
  growth_rates$annual_growth_rate <-
    log(growth_rates$population_base / growth_rates$population_start) /
    (base_year - start_year)

  projection_years <- seq.int(base_year + 1L, end_year)
  projected_population <- growth_rates[
    rep(seq_len(nrow(growth_rates)), each = length(projection_years)),
    ,
    drop = FALSE
  ]
  projected_population$Calendar_Year <- rep(projection_years, times = nrow(growth_rates))
  projected_population$population_update <- projected_population$population_base *
    exp(projected_population$annual_growth_rate *
          (projected_population$Calendar_Year - base_year))
  projected_population <- projected_population[, c(id_cols, "Calendar_Year", "population_update")]

  rbind(
    df[
      df$Calendar_Year <= base_year,
      c(id_cols, "Calendar_Year", "population_update"),
      drop = FALSE
    ],
    projected_population
  )
}

#===============================================================================
# Calculate cigarette consumption and tax revenue from smokers, CPD, and tax rate.
revenue_calc <- function(smokers, average_cpd, tax_rate) {
  packs_per_smoker <- average_cpd * 365 / 20
  total_packs <- smokers * packs_per_smoker

  list(
    packs_per_smoker = packs_per_smoker,
    total_packs = total_packs,
    revenue = tax_rate * total_packs
  )
}

#===============================================================================
# Calculate annual state-level factors that align modeled and observed consumption.
scale_factors <- function(df) {
  df |>
    dplyr::group_by(state_abbr, Calendar_Year) |>
    dplyr::summarise(
      pop = sum(population, na.rm = TRUE),
      packs_model = sum(packs_model, na.rm = TRUE),
      packs_pc_obs = dplyr::first(state_cig_pack_per_capita),
      packs_obs = dplyr::first(state_cig_sales_pack_in_million) * 1000000,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      packs_pc_model = ifelse(
        !is.na(pop) & pop > 0,
        packs_model / pop,
        NA_real_
      ),
      scale_factor = ifelse(
        !is.na(packs_pc_model) & packs_pc_model > 0,
        packs_pc_obs / packs_pc_model,
        NA_real_
      )
    ) |>
    dplyr::select(
      state_abbr, Calendar_Year, pop, packs_pc_obs, packs_obs,
      packs_model, packs_pc_model, scale_factor
    )
}
