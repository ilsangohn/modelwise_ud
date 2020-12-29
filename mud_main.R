library(UncDecomp)
library(xtable)
source("mud_functions.r")


## Load Data
yongdam = read.csv("yongdam.csv")
stages = c("ES", "GCM", "BC", "HM")
K = length(stages)

## Define lq deviance functions
q = 10
U_q = function(x){
  f_m = function(m) mean(abs((x-m))^q)
  return(optimize( f_m , c(-5000,5000))$objective)
}
u_q =  function(x, m){
  return(abs((x-m))^q)
}
f_q = function(x){
  f_m = function(m) mean(abs((x-m))^q)
  return(optimize( f_m , c(-5000,5000))$minimum)
}


### Figure 3
mean_plot(yongdam, stages)


## Table 2
stage_table = data.frame(matrix(0, 3*K, 4))
stage_table[, 2] = rep(stages, 3)
stage_table[, 1] = c("MAD", rep("", K-1), "Variance",  rep("", K-1), "l10",  rep("", K-1))
j = 3
for(Umethod in c(UD_cum_stage, UD_bal_stage)){
  i = 0
  for(Umeasure in c(U_mad, U_var, U_q)){
    X = Umethod(yongdam, "Y", stages, Umeasure)
    stage_table[i*K + (1:K), j] =  percent_calc(X$unc)
    i = i + 1
  }
  j = j + 1
}
print(xtable(stage_table),include.rownames = FALSE)


## Table 3
ud_mad = UD_bal_model(yongdam, "Y", stages, u_mad, flist_mad)
ud_var = UD_bal_model(yongdam, "Y", stages)
ud_lq = UD_bal_model(yongdam, "Y", stages, u_q, list(f_q))
model_unc = cbind(model_ud_tab(ud_mad), model_ud_tab(ud_var)[,3], model_ud_tab(ud_lq)[,3])
colnames(model_unc) = c("Stage", "Model", "MAD", "Variance", "l_q")
print(xtable(model_unc), include.rownames=FALSE)


## Figure 4
for(k in 1:K){
  u_comp = calc_u_comp(yongdam, "Y", stages, k, u_mad, median)$u_table
  ggplot(u_comp, aes(y=u, x=factor(A, levels=levels(A)[order(nchar(as.character(levels(A))))]),  group=Model)) +
    geom_point(aes(shape=Model), size=3) + geom_line(aes(col=Model)) +
    ylab(substitute(U[a]^'cumul,scaled', list(a=paste0("A,",k,",x")))) + xlab("A") + theme_bw() + 
    ylim(c(0,0.24)) +
    theme(legend.position="bottom") + 
    theme(axis.text.x = element_text(size =12))
  #title = paste0("comp_", stages[k],".png" )
  #ggsave(title, width=16, height=10, units = "cm")
}


