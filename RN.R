RN = function(settle, MaturityDate_or_Term, yield, freq = 2, Recovery_Rate1, Recovery_Rate2, rf_Term, rf_rate, convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"))
{
  # settle = 43013
  # MaturityDate_or_Term = 28.57
  # yield = 3.03/100
  # Recovery_Rate1 = 0.372
  # Recovery_Rate2 = 0.669
  # rf_Term = rf_table$Term
  # rf_rate = rf_table$rf
  require(jrvFinance)
  settle = as.Date(settle, origin = "1899-12-30")
  if(MaturityDate_or_Term > 100){
    mature = as.Date(MaturityDate_or_Term,origin = "1899-12-30")
  } else{
    mature = as.Date(settle + MaturityDate_or_Term*365.25)}
  
  coupon = yield
  
  # This function creates the table of CFs the same as EY RN methodology. i.e. first couon in 6 months from settle and then going forward
  # the main bond.TCF considers last coupon 6 months before mature and comes back in time
  
  sia.bond.TCF = function (settle, mature, coupon, freq = 2 , RNPD, Recovery_Rate, rf_Term, rf_rate, convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E")) 
  {
    settle <- as.Date(settle)
    mature <- as.Date(mature)
    Ref_PD_per_Period =1- (1-RNPD)^(1/freq)
    # if first coupon paid 6 months after settle, start = 0.5; if last coupon 6 months before mature uncomment start
    # start <- yearFraction(settle, nextC, prevC, nextC, freq, convention)
    start = 0.5
    t <- seq(from = start, by = 1/freq, length.out = coupons.n(settle, mature, freq)-1)
    t = c(t, yearFraction(settle, mature,freq, convention))
    
    # cf: for the last one depending on the time inteval intrerest accrues
    cf <- rep(coupon * 100/freq, length(t))
    cf[length(cf)] <- 100 + coupon * 100 * (t[length(t)]-t[length(t)-1])
    
    Survival_Prob = (1-Ref_PD_per_Period)^(freq*t)
    PD = -diff(c(1,Survival_Prob))
    
    # cf in the vent of default
    cf2 <- rep(100 + coupon * 100/freq, length(t))
    cf2[length(cf2)] <- 100 + coupon * 100 * (t[length(t)]-t[length(t)-1])
    cf_given_Default = cf2 * Recovery_Rate
    
    Expected_cf = PD * cf_given_Default + Survival_Prob * cf
    
    rf = approx(x = rf_Term , y = rf_rate, xout = t, method = "linear", rule = 2)$y
    
    DCF = Expected_cf / ((1+rf/2)^(2*t))
    #list(t = t, cf = cf)
    return(sum(DCF))
  }
  
  RNPD_finder = function(RNPD){
    environment(RNPD_finder)  = globalenv()
    difference = sia.bond.TCF(settle = settle, mature = mature, coupon = coupon, freq = freq, convention = convention, RNPD = RNPD, Recovery_Rate = Recovery_Rate1, rf_Term, rf_rate) - 100
    return(difference)
  }
  
  RNPD = uniroot(RNPD_finder, c(0,0.3))$root
  
  sia.bond.TCF(settle, mature, coupon, freq, RNPD = RNPD, Recovery_Rate1, rf_Term, rf_rate) # check to be 100
  
  coupon_finder = function(coupon){
    environment(coupon_finder)  = globalenv()
    difference = sia.bond.TCF(settle = settle, mature = mature, coupon = coupon, freq= freq, convention = convention, RNPD = RNPD, Recovery_Rate = Recovery_Rate2, rf_Term, rf_rate) - 100
    return(difference)
  }
  
  coupon.subject = uniroot(coupon_finder, c(0,0.3))$root
  
  sia.bond.TCF(settle, mature, coupon = coupon.subject, freq, RNPD, Recovery_Rate2, rf_Term, rf_rate) # check to be 100
  
  #RNPD
  return(coupon.subject)
}

attr(RN, "description" ) <- list( 
  "This function adjusts the yield from RecoveryRate1 to RecoveryRate2", 
  Recovery_Rate1 = "1st Lien Bank Loan 0.6707; 2nd Lien Bank Loan 0.3038; Sr. Unsecured Bank Loan 0.4587; 1st Lien Bond 0.5362; 2nd Lien Bond 0.4518; Sr. Unsecured Bond 0.3774; Sr. Sub Bond 0.3110; Sub Bond  0.3205; Jr. Sub Bond 0.2279", 
  Recovery_Rate2 = "1st Lien Bank Loan 0.6707; 2nd Lien Bank Loan 0.3038; Sr. Unsecured Bank Loan 0.4587; 1st Lien Bond 0.5362; 2nd Lien Bond 0.4518; Sr. Unsecured Bond 0.3774; Sr. Sub Bond 0.3110; Sub Bond  0.3205; Jr. Sub Bond 0.2279", 
)