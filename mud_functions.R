### Figure
library(ggplot2)
library(grid)
library(gridExtra)

mean_plot = function(.data, stage){
  plot_list <- list()
  for(k in 1:4){
    Proj = .data[, c(stage[k], "Y")]
    colnames(Proj) = c("x", "y")
    Proj$grp = do.call(paste, c(.data[, stage[-k]], sep = ""))
    meanProj = tapply(Proj$y, Proj$x, mean)
    medProj = tapply(Proj$y, Proj$x, median)
    meanProj_df = data.frame(x=names(meanProj), y=meanProj, grp=1)
    medProj_df = data.frame(x=names(medProj), y=medProj, grp=1)
    plot_list[[k]] <- ggplot(Proj, aes(x=x, y=y, group=grp)) + 
      geom_point(cex=2) + 
      geom_point(data=medProj_df, col="orange", pch=15, cex=4, alpha=0.9) +
      geom_point(data=meanProj_df, col="red", pch=18, cex=4) +
      labs(x=stage[k], y=expression("Streamflow projection (m"^3*"/s)"), size=15) + 
      geom_line(colour = "green") + 
      geom_hline(yintercept = mean(.data$Y), col="red", lwd=1, lty=2) + 
      geom_hline(yintercept = median(.data$Y), col="orange", lwd=1, lty=4) +
      theme_bw()
  }
  do.call(grid.arrange,  plot_list)
}


percent_calc = function(x, only.prop=T, add.total=F, digit=2, digit2=3){
  percent = round(x/sum(x)*100, digit)
  if(only.prop){
    xx = paste0(percent, "%")
  } else {
    xx = paste0(round(x, digit2), " (", paste0(percent, "%"), ")")
  }
  if(add.total){
    if(only.prop){
      xx = c(xx, "100%")
    } else {
      xx = c(xx, paste0(round(sum(x), digit2), " (100%)"))
    }
  }
  return(xx)
}


model_ud_tab = function(Uobj){
  X = Uobj$unc
  tab = data.frame(Stage = NA, Model = NA, Uncertainty = NA)
  
  for(k in 1:length(X)){
    u_k = X[[k]]
    n_k = length(u_k)
    tab_k = data.frame(Stage = c(names(X)[k], rep("", n_k-1)), 
                       Model = names(u_k), Uncertainty = rep("", n_k))
    tab = rbind(tab, tab_k)
  }
  #tab = rbind(tab, c("Total", "", ""))
  tab = tab[-1,]
  tab[,3] = percent_calc(unlist(X))
  return(tab)
}


calc_m = function(.data, vname, stages, A_vec, f){
  m_vec = c()
  n_x = nrow(.data)
  for(i in 1:nrow(.data)){
    sub_data = .data[sapply(1:n_x, function(j) all(.data[j, stages[!A_vec]]==.data[i, stages[!A_vec]])), ]
    m_vec[i] = f(sub_data[, vname])
  }
  m_vec
}


calc_u_comp = function(.data,  vname, stages, k, u, f){
  n_x = nrow(.data)
  K = length(stages)
  
  models_k = unique(.data[,stages[k]])
  models_k = models_k[order(models_k)]
  n_model = length(models_k)
  
  dimnamesA = rep(list(c(F, T)), K)
  A_list = as.matrix(expand.grid(dimnamesA))
  n_A = nrow(A_list)
  m_matrix = sapply(1:n_A, function(j) calc_m(.data, vname, stages, A_list[j,], f))
  
  A_list_k = A_list[A_list[,k]==T, ]
  A_list_k = A_list_k[ order(rowSums(A_list_k)),]
  n_Ak = nrow(A_list_k)
  r_vec = apply(A_list_k, 1, sum)
  
  u_A_x = matrix(0, n_model, n_Ak )
  for(j in 1:n_Ak){
    A =  A_list_k[j,]
    A0 = A; A0[k] = F
    A_idx = sapply(1:n_A, function(t) all(A_list[t,]==A))
    A0_idx = sapply(1:n_A, function(t) all(A_list[t,]==A0))
    
    diff = (u(.data[,vname ],m_matrix[, A_idx]) - u(.data[,vname],m_matrix[, A0_idx]))/(n_x*choose(K-1,sum(A)-1)*K) #
    
    for(q in 1:n_model){
      x = models_k[q]
      x_idx = (.data[,stages[k]]==x)
      u_A_x[q, j] = sum(diff[x_idx])
    }
  }
  rownames(u_A_x) = models_k
  colnames(u_A_x) = apply(A_list_k, 1, function(x) paste("{", paste((1:4)[x], collapse=","), "}",sep=""))
  u_A_x_long = data.frame(Model=rep(rownames(u_A_x), ncol(u_A_x)), 
                          A=factor(rep(colnames(u_A_x), each=nrow(u_A_x))), 
                          u=as.vector(u_A_x))
  
  #u_r = sapply(1:K, function(k) rowSums(as.matrix(u_A_x[, r_vec==k])) ) 
  return(list(u_table_wide=u_A_x, u_table=u_A_x_long))
}
