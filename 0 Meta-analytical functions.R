library(tidyverse); options(tibble_print.min=40); options(scipen = .10)
library(metafor)
library(metaSEM)
library(weightr) #for the publication bias function below


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

mara_table = function(metaforobject){
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







# Publication bias
#<##################################################################################>

# The function reports three tests of publication bias
# 1) PET: A three-level random effects model with SE as the predictor
# 2) PEESE: The same model but with SE^2 as the predictor  
# 3) The selection test based on aggregated ES 
# To compare the adjutments in these model, a normal three level model is also run and 
# compared to the three adjustments


# Selection model auf aggregierten Studienwerten
pubbias_table <- function(df,
  study_id = "sample_id",
  effect_id = "es_id",
  yi_var = "yi",
  vi_var = "vi",
  selection_steps = c(0.025, 1),
  seed = NULL) {

if (!is.null(seed)) set.seed(seed)

d <- df %>%
dplyr::rename(
study  = all_of(study_id),
effect = all_of(effect_id),
yi     = all_of(yi_var),
vi     = all_of(vi_var)
) %>%
dplyr::mutate(
study  = factor(study),
effect = factor(effect),
sei    = sqrt(vi)
)

# Studienweise Aggregation für Selection Model
d_study <- d %>%
dplyr::group_by(study) %>%
dplyr::summarise(
yi = stats::weighted.mean(yi, w = 1 / vi, na.rm = TRUE),
vi = 1 / sum(1 / vi, na.rm = TRUE),
.groups = "drop"
)

# Three-level unadjusted
mf_pub <- metafor::rma.mv(
yi, vi,
random = ~ 1 | study / effect,
data = d,
method = "ML"
)

# PET
egger_pet <- metafor::rma.mv(
yi, vi,
mods = ~ sei,
random = ~ 1 | study / effect,
data = d,
method = "ML"
)

# PEESE
egger_peese <- metafor::rma.mv(
yi, vi,
mods = ~ I(sei^2),
random = ~ 1 | study / effect,
data = d,
method = "ML"
)

# Selection Model auf aggregierten Studienwerten
sel_mod <- tryCatch(
suppressWarnings(
weightr::weightfunct(
effect = d_study$yi,
v      = d_study$vi,
steps  = selection_steps
)
),
error = function(e) NULL
)

sel_est <- sel_se <- sel_lrt_stat <- sel_lrt_p <- sel_p <- NA_real_

if (!is.null(sel_mod)) {

# Effektschätzer und SE aus dem adjustierten Modell (Index 2 = Intercept)
sel_est <- tryCatch(as.numeric(sel_mod$adj_est[2, 1]), error = function(e) NA_real_)
sel_se  <- tryCatch(as.numeric(sel_mod$adj_se[2, 1]),  error = function(e) NA_real_)

# LRT: value = negativer LL -> unadj - adj = korrekte Differenz
# df = Anzahl geschätzter Gewichte = nsteps - 1
sel_lrt_stat <- tryCatch(
2 * (sel_mod$output_unadj$value - sel_mod$output_adj$value),
error = function(e) NA_real_
)
df_lrt <- sel_mod$nsteps - 1
sel_lrt_p <- tryCatch(
as.numeric(pchisq(sel_lrt_stat, df = df_lrt, lower.tail = FALSE)),
error = function(e) NA_real_
)

# p-Wert des Effektschätzers
sel_p <- if (!is.na(sel_est) && !is.na(sel_se) && sel_se > 0)
2 * pnorm(-abs(sel_est / sel_se)) else NA_real_
}

out <- dplyr::bind_rows(
data.frame(
Method       = "Unadjusted (3-level)",
Bias_test_p  = NA_real_,
Sig_rate     = NA_real_,
Corrected_ES = round(as.numeric(mf_pub$b[1, 1]), 3),
SE           = round(as.numeric(mf_pub$se[1]), 3),
ES_p         = round(as.numeric(mf_pub$pval[1]), 3)
),
data.frame(
Method       = "PET",
Bias_test_p  = round(as.numeric(egger_pet$pval[2]), 3),
Sig_rate     = NA_real_,
Corrected_ES = round(as.numeric(egger_pet$b[1, 1]), 3),
SE           = round(as.numeric(egger_pet$se[1]), 3),
ES_p         = round(as.numeric(egger_pet$pval[1]), 3)
),
data.frame(
Method       = "PEESE",
Bias_test_p  = round(as.numeric(egger_peese$pval[2]), 3),
Sig_rate     = NA_real_,
Corrected_ES = round(as.numeric(egger_peese$b[1, 1]), 3),
SE           = round(as.numeric(egger_peese$se[1]), 3),
ES_p         = round(as.numeric(egger_peese$pval[1]), 3)
),
data.frame(
Method       = "Selection model",
Bias_test_p  = round(sel_lrt_p, 3),
Sig_rate     = round(as.numeric(!is.na(sel_lrt_p) && sel_lrt_p < .05), 2),
Corrected_ES = round(sel_est, 3),
SE           = round(sel_se, 3),
ES_p         = round(sel_p, 3)
)
)

print(out)
invisible(list(
table       = out,
mf_pub      = mf_pub,
egger_pet   = egger_pet,
egger_peese = egger_peese,
sel_mod     = sel_mod,
d           = d,
d_study     = d_study
))
}
