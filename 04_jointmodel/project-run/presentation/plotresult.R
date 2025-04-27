
pacman::p_load(HDInterval, ggpubr)

df1<-
  paste0("d1_",1:10)|>
  lapply(\(s){
    fit = read_rds(file = paste0("../01_code/main-result/s1/",s,".rds"))
    total<-
      fit$draws(c("beta[2]", "alpha_mark", "alpha_trt"), format = "matrix")|>
      {\(i) i[,1]*i[,2]+i[,3]}()|>
      {\(i) c(mean(i), hdi(i))}()|>
      {\(i) tibble(parameter = "total", mean = i[1], lower = i[2], upper = i[3]) }()
    
    
    fit$summary(c("beta[1]", "beta[2]", "b0_bar", "k_bar", "alpha_mark", "alpha_trt"), mean, hdi)|>
      mutate(variable = c("beta0", "beta1", "b0_mean", "b1_mean", "alpha2", "alpha1"))|>
      rename("parameter" = variable)|>
      bind_rows(total)|>
      mutate(Data = s, .before = 1)
  })|>
  reduce(bind_rows)

df2<-
  paste0("d2_",1:10)|>
  lapply(\(s){
    fit = read_rds(file = paste0("../01_code/main-result/s2/",s,".rds"))
    total<-
      fit$draws(c("beta[2]", "alpha_mark", "alpha_trt"), format = "matrix")|>
      {\(i) i[,1]*i[,2]+i[,3]}()|>
      {\(i) c(mean(i), hdi(i))}()|>
      {\(i) tibble(parameter = "total", mean = i[1], lower = i[2], upper = i[3]) }()
    
    
    fit$summary(c("beta[1]", "beta[2]", "b0_bar", "k_bar", "alpha_mark", "alpha_trt"), mean, hdi)|>
      mutate(variable = c("beta0", "beta1", "b0_mean", "b1_mean", "alpha2", "alpha1"))|>
      rename("parameter" = variable)|>
      bind_rows(total)|>
      mutate(Data = s, .before = 1)
  })|>
  reduce(bind_rows)


df3<-
  paste0("d3_",1:10)|>
  lapply(\(s){
    fit = read_rds(file = paste0("../01_code/main-result/s3/",s,".rds"))
    total<-
      fit$draws(c("beta[2]", "alpha_mark", "alpha_trt"), format = "matrix")|>
      {\(i) i[,1]*i[,2]+i[,3]}()|>
      {\(i) c(mean(i), hdi(i))}()|>
      {\(i) tibble(parameter = "total", mean = i[1], lower = i[2], upper = i[3]) }()
    
    
    fit$summary(c("beta[1]", "beta[2]", "b0_bar", "k_bar", "alpha_mark", "alpha_trt"), mean, hdi)|>
      mutate(variable = c("beta0", "beta1", "b0_mean", "b1_mean", "alpha2", "alpha1"))|>
      rename("parameter" = variable)|>
      bind_rows(total)|>
      mutate(Data = s, .before = 1)
  })|>
  reduce(bind_rows)


df4<-
  paste0("d4_",1:10)|>
  lapply(\(s){
    fit = read_rds(file = paste0("../01_code/main-result/s4/",s,".rds"))
    total<-
      fit$draws(c("beta[2]", "alpha_mark", "alpha_trt"), format = "matrix")|>
      {\(i) i[,1]*i[,2]+i[,3]}()|>
      {\(i) c(mean(i), hdi(i))}()|>
      {\(i) tibble(parameter = "total", mean = i[1], lower = i[2], upper = i[3]) }()
    
    
    fit$summary(c("beta[1]", "beta[2]", "b0_bar", "k_bar", "alpha_mark", "alpha_trt"), mean, hdi)|>
      mutate(variable = c("beta0", "beta1", "b0_mean", "b1_mean", "alpha2", "alpha1"))|>
      rename("parameter" = variable)|>
      bind_rows(total)|>
      mutate(Data = s, .before = 1)
  })|>
  reduce(bind_rows)

#############
##########################
#########################################

par_name = c("beta1", "b0_mean", "b1_mean", "alpha2", "alpha1", "total")
true_val = c(0.5, -1.2, 0, 2, 0.8, -0.5, 0.8*(-1.2) -0.5)|>
  `names<-`(par_name)


p_beta1<-
list(df1, df2, df3, df4)|>
  `names<-`(paste0("Scenario: ",1:4))|>
  imap(\(d,n){
      filter(d, parameter =="beta1")%>%
      ggplot(aes(x = Data, y = mean, ymin = lower, ymax = upper)) +
      geom_pointrange() +
      ylim(-7,5)+
      geom_hline(yintercept = -1.2, linetype = "dashed", color = "red")+
      coord_flip() +
      theme_bw() +
      labs(title = paste(n, ":", "beta1"))
  })
ggpubr::ggarrange(p_beta1[[1]], p_beta1[[2]], p_beta1[[3]], p_beta1[[4]], ncol = 2, nrow = 2)


####

p_tot<-
  list(df1, df2, df3, df4)|>
  `names<-`(paste0("Scenario: ",1:4))|>
  imap(\(d,n){
    filter(d, parameter =="total")%>%
      ggplot(aes(x = Data, y = mean, ymin = lower, ymax = upper)) +
      geom_pointrange() +
      ylim(-7,5)+
      geom_hline(yintercept = -1.46, linetype = "dashed", color = "red")+
      coord_flip() +
      theme_bw() +
      labs(title = paste(n, ":", "overall treatment effect"))
  })
ggpubr::ggarrange(p_tot[[1]], p_tot[[2]], p_tot[[3]], 
                  p_tot[[4]], ncol = 2, nrow = 2)



###

p_alpha2<-
  list(df1, df2, df3, df4)|>
  `names<-`(paste0("Scenario: ",1:4))|>
  imap(\(d,n){
    filter(d, parameter =="alpha2")%>%
      ggplot(aes(x = Data, y = mean, ymin = lower, ymax = upper)) +
      geom_pointrange() +
      ylim(-0.5,1.5)+
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "red")+
      coord_flip() +
      theme_bw() +
      labs(title = paste(n, ":", "alpha2"))
  })
ggpubr::ggarrange(p_alpha2[[1]], p_alpha2[[2]], 
                  p_alpha2[[3]], p_alpha2[[4]], ncol = 2, nrow = 2)

###

p_alpha1<-
  list(df1, df2, df3, df4)|>
  `names<-`(paste0("Scenario: ",1:4))|>
  imap(\(d,n){
    filter(d, parameter =="alpha1")%>%
      ggplot(aes(x = Data, y = mean, ymin = lower, ymax = upper)) +
      geom_pointrange() +
      ylim(-2.5,1)+
      geom_hline(yintercept = -0.5, linetype = "dashed", color = "red")+
      coord_flip() +
      theme_bw() +
      labs(title = paste(n, ":", "alpha1"))
  })
ggpubr::ggarrange(p_alpha1[[1]], p_alpha1[[2]], 
                  p_alpha1[[3]], p_alpha1[[4]], ncol = 2, nrow = 2)
####################################

















