library(tidyverse); options(tibble_print.min=40); options(scipen = .10)
library(metafor)
library(metaSEM)


#<##################################################################################>
# Functions to Extract Meta-Analytical Results----
#<##################################################################################>

# The first function (meta_table) retrieves all relevant statistics from the three-level 
# metafor and meta3 objects and organizes them into a row vector. This is 
# printed (for verification) and copied to the clipboard. From there, it can be 
# pasted directly into Excel.

# The second function (mara_table) is applied to a meta-regression model / 
# mixed-effects model (using both metafor and meta3) and extracts 
# the coefficients as well as the R-squared values. The R-squared values of the two Taus 
# kmNs must be calculated separately


meta_table = function(es_data, metaforobject, meta3object){
  k = round(metaforobject$s.nlevels.f[1],0)
  m = round(metaforobject$s.nlevels.f[2],0)
  N = es_data %>% 
    summarise(N = sum(n[!duplicated(sample_id)]))
  # Estimates
  b = tibble(metaforobject$b[,1]) %>% 
    rename(estimate = `metaforobject$b[, 1]`) %>% 
    round(., 3) %>% as.numeric()
  se = tibble(metaforobject$se) %>% 
    rename(se = `metaforobject$se`) %>% 
    round(., 2) %>% as.numeric()
  b_se = sprintf("%g (%g)", b,se)
  ci_lb = tibble(metaforobject$ci.lb) %>% 
    rename(ci_lb = `metaforobject$ci.lb`) %>% 
    round(.,2)
  ci_ub = tibble(metaforobject$ci.ub) %>% 
    rename(ci_ub = `metaforobject$ci.ub`) %>% 
    round(.,2)
  pval = tibble(metaforobject$pval) %>% 
    rename(p_value = `metaforobject$pval`) %>% 
    round(., 3)
  # Q test, df
  Q = tibble(metaforobject$QE) %>% 
    rename(Q_test = `metaforobject$QE`) %>% 
    round(.,2)  %>% as.numeric()
  df = metaforobject$s.nlevels.f[[2]] - metaforobject$m %>% 
    as_tibble() %>% 
    rename(df = value) %>% as.numeric()
  # Tau und I-square 
  T_w = tibble(meta3object$mx.fit$algebras$Tau2$result[,1]) %>% 
    rename(T_w = `meta3object$mx.fit$algebras$Tau2$result[, 1]`) %>% 
    sqrt(.) %>% 
    round(., 2)  %>% as.numeric()
  I2_2 = tibble(meta3object$mx.fit$algebras$I2q_2$result[,1]) %>% 
    rename(I2_w = `meta3object$mx.fit$algebras$I2q_2$result[, 1]`) %>% 
    round(., 2) %>% as.numeric()
  TauI2_w = sprintf("%g (%g)", T_w,I2_2)
  T_b = tibble(meta3object$mx.fit$algebras$Tau3$result[,1]) %>% 
    rename(T_b = `meta3object$mx.fit$algebras$Tau3$result[, 1]`) %>% 
    sqrt(.) %>% 
    round(., 2) %>% as.numeric()
  I2_3 = tibble(meta3object$mx.fit$algebras$I2q_3$result[,1]) %>% 
    rename(I2_b = `meta3object$mx.fit$algebras$I2q_3$result[, 1]`) %>% 
    round(., 2) %>% as.numeric()
  TauI2_b = sprintf("%g (%g)", T_b,I2_3)
  
  table = tibble(k,m,N,b_se, pval, ci_lb,ci_ub,TauI2_w, TauI2_b)
  print(table)
  write.table(table, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
}





# Meta-Regression (MARA)
#<##################################################################################>

mara_table2 = function(metaforobject){
  b = tibble(metaforobject$b[,1]) %>% 
    rename(b = `metaforobject$b[, 1]`) %>% 
    round(., 3) 
  se = tibble(metaforobject$se) %>% 
    rename(se = `metaforobject$se`) %>% 
    round(., 2) 
  b_se = tibble(b,se) %>% 
    reframe(b_se = sprintf("%g (%g)", b,se))
  ci_lb = tibble(metaforobject$ci.lb) %>% 
    rename(ci_lb = `metaforobject$ci.lb`) %>% 
    round(.,2)
  ci_ub = tibble(metaforobject$ci.ub) %>% 
    rename(ci_ub = `metaforobject$ci.ub`) %>% 
    round(.,2)
  pval = tibble(metaforobject$pval) %>% 
    rename(p_value = `metaforobject$pval`) %>% 
    round(., 3)
   reg_table = tibble(b_se, pval, ci_lb, ci_ub)
  print(reg_table)
  write.table(reg_table, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
}



#<##################################################################################>
# Funktion zum Publication bias
#<##################################################################################>

# NOTES
# 1) Die Grundlage fuer die Funktion is ein twolevel random effects model
# 2) Daraufbasierend wird der reg-test, ranktest und trim&fill Prozedur gemacht
# 3) Die Funktion zieht sie die beiden p-Werte sowie den korrigierten Effekt und p-Wert
#    und printed sie in dei Konsole
# 4) Zudem wird WENN beide p-Werte < .05 sind, der Ausdruck
#    "YES (Corrected rho = 'B_corrected' , p = 'pval_corrected' )" in die Zwischenablage
#    kopiert. Wenn der p-Wert <.001 ist wird nicht der exakte p-Wert in die Zwischenablage
#    kopiert sondern der Ausdruck


pub_bias = function(twolevelmodel) {
  regtest = regtest(twolevelmodel) 
  ranktest = ranktest(twolevelmodel)
  trimfill = trimfill(twolevelmodel)
  regtest_pval = regtest$pval
  ranktest_pval = ranktest$pval
  trimfill = trimfill(twolevelmodel)  
  b_corr = tibble(trimfill$b[, 1]) %>% 
    rename(estimate = `trimfill$b[, 1]`) %>% 
    round(., 3) %>% as.numeric()
  pval_corr = tibble(trimfill$pval) %>% 
    rename(p_value = `trimfill$pval`) %>% 
    round(., 4)  # Increase the number of decimal places
  
  # Convert pval_corr to a character vector
  pval_corr <- as.character(pval_corr)
  
  # Replace p-values smaller than 0.001 with "<.001"
  pval_corr[pval_corr < "0.001"] <- "<.001"
  
  pub_bias_table = tibble(regtest_pval, ranktest_pval, b_corr, p_value = pval_corr)
  print(pub_bias_table)  
  # Check if both p-values are smaller than 0.05
  if (all(regtest_pval < 0.05) && all(ranktest_pval < 0.05)) {
    cat("YES (Corrected rho =", b_corr, ", p =", pval_corr, ")\n")
  }
  
  # Write to the clipboard
  cat("YES (Corrected rho =", b_corr, ", p =", pval_corr, ")\n", file = "clipboard")
}







