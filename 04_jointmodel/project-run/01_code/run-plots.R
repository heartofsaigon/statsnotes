
library(tidyverse)
library(HDInterval)
library(ggpubr)

s1<-
lapply(1:10, \(i){
  d<- read_rds(file = paste0("project-run/01_code/main-result/s1/d1_",i,".rds"))
  rd<-
  d$draws(c("beta[2]", "alpha_mark", "alpha_trt"), format = "matrix")|>
    `colnames<-`(c("beta1", "alpha2", "alpha1"))|>
    as_tibble()|>
    mutate(indirect = beta1*alpha2 +alpha1)
  apply(rd,2, \(i) c(mean = mean(i), hdi(i)))|>
    t()|>
    as.data.frame()|>
    rownames_to_column(var = "parameter")|>
    as_tibble()|>
    mutate(dat = paste0("d",i), .before = 1)
  })|>
  bind_rows()

s2<-
  lapply(1:10, \(i){
    d<- read_rds(file = paste0("project-run/01_code/main-result/s2/d2_",i,".rds"))
    rd<-
      d$draws(c("beta[2]", "alpha_mark", "alpha_trt"), format = "matrix")|>
      `colnames<-`(c("beta1", "alpha2", "alpha1"))|>
      as_tibble()|>
      mutate(indirect = beta1*alpha2 +alpha1)
    apply(rd,2, \(i) c(mean = mean(i), hdi(i)))|>
      t()|>
      as.data.frame()|>
      rownames_to_column(var = "parameter")|>
      as_tibble()|>
      mutate(dat = paste0("d",i), .before = 1)
  })|>
  bind_rows()

s3<-
  lapply(1:10, \(i){
    d<- read_rds(file = paste0("project-run/01_code/main-result/s3/d3_",i,".rds"))
    rd<-
      d$draws(c("beta[2]", "alpha_mark", "alpha_trt"), format = "matrix")|>
      `colnames<-`(c("beta1", "alpha2", "alpha1"))|>
      as_tibble()|>
      mutate(indirect = beta1*alpha2 +alpha1)
    apply(rd,2, \(i) c(mean = mean(i), hdi(i)))|>
      t()|>
      as.data.frame()|>
      rownames_to_column(var = "parameter")|>
      as_tibble()|>
      mutate(dat = paste0("d",i), .before = 1)
  })|>
  bind_rows()

s4<-
  lapply(1:10, \(i){
    d<- read_rds(file = paste0("project-run/01_code/main-result/s4/d4_",i,".rds"))
    rd<-
      d$draws(c("beta[2]", "alpha_mark", "alpha_trt"), format = "matrix")|>
      `colnames<-`(c("beta1", "alpha2", "alpha1"))|>
      as_tibble()|>
      mutate(indirect = beta1*alpha2 +alpha1)
    apply(rd,2, \(i) c(mean = mean(i), hdi(i)))|>
      t()|>
      as.data.frame()|>
      rownames_to_column(var = "parameter")|>
      as_tibble()|>
      mutate(dat = paste0("d",i), .before = 1)
  })|>
  bind_rows()

property = c(
  "Scenario 1: n/2 = 100; J = 10",
  "Scenario 2: n/2 = 100; J = 70",
  "Scenario 3: n/2 = 150; J = 10",
  "Scenario 4: n/2 = 150; J = 70"
)



beta1.plot<-
  list(s1,s2,s3,s4)|>
  `names<-`(property)|>
  imap(\(s,n){
  df<- filter(s, parameter == "beta1")
  df<- mutate(df, dat = factor(dat, levels = dat))
  
  ggplot() +
    # Plot the group CIs with horizontal error bars and points
    geom_errorbarh(data = df, 
                   aes(xmin = lower, xmax = upper, y = dat),
                   height = 0.2, color = "black") +
    geom_point(data = df, 
               aes(x = mean, y = dat),
               size = 3, color = "black") +
    geom_vline(xintercept = -1.2, linetype = 2, color = "red")+
    xlim(c(-5,2.5))+
    ggtitle(n)
})

lift_dl(ggarrange)(beta1.plot, ncol = 2, nrow = 2)
write_rds(beta1.plot, file = "project-run/01_code/main-result/beta1_plot.rds")

#-----

indirect.plot<-
  list(s1,s2,s3,s4)|>
  `names<-`(property)|>
  imap(\(s,n){
    df<- filter(s, parameter == "indirect")
    df<- mutate(df, dat = factor(dat, levels = dat))
    ggplot() +
      # Plot the group CIs with horizontal error bars and points
      geom_errorbarh(data = df, 
                     aes(xmin = lower, xmax = upper, y = dat),
                     height = 0.2, color = "black") +
      geom_point(data = df, 
                 aes(x = mean, y = dat),
                 size = 3, color = "black") +
      geom_vline(xintercept = -1.46, linetype = 2, color = "red")+
      xlim(c(-4,2))+
      ggtitle(n)
  })

lift_dl(ggarrange)(indirect.plot, ncol = 2, nrow = 2)
write_rds(indirect.plot, file = "project-run/01_code/main-result/indirect_plot.rds")

#-----

alpha2.plot<-
  list(s1,s2,s3,s4)|>
  `names<-`(property)|>
  imap(\(s,n){
    df<- filter(s, parameter == "alpha2")
    df<- mutate(df, dat = factor(dat, levels = dat))
    ggplot() +
      # Plot the group CIs with horizontal error bars and points
      geom_errorbarh(data = df, 
                     aes(xmin = lower, xmax = upper, y = dat),
                     height = 0.2, color = "black") +
      geom_point(data = df, 
                 aes(x = mean, y = dat),
                 size = 3, color = "black") +
      geom_vline(xintercept = 0.8, linetype = 2, color = "red")+
      xlim(c(0,1))+
      ggtitle(n)
  })

lift_dl(ggarrange)(alpha2.plot, ncol = 2, nrow = 2)
write_rds(alpha2.plot, file = "project-run/01_code/main-result/alpha2_plot.rds")
#-----



alpha1.plot<-
  list(s1,s2,s3,s4)|>
  `names<-`(property)|>
  imap(\(s,n){
    df<- filter(s, parameter == "alpha1")
    df<- mutate(df, dat = factor(dat, levels = dat))
    ggplot() +
      # Plot the group CIs with horizontal error bars and points
      geom_errorbarh(data = df, 
                     aes(xmin = lower, xmax = upper, y = dat),
                     height = 0.2, color = "black") +
      geom_point(data = df, 
                 aes(x = mean, y = dat),
                 size = 3, color = "black") +
      geom_vline(xintercept = -0.5, linetype = 2, color = "red")+
      xlim(c(-2,1))+
      ggtitle(n)
  })

lift_dl(ggarrange)(alpha1.plot, ncol = 2, nrow = 2)
write_rds(alpha1.plot, file = "project-run/01_code/main-result/alpha1_plot.rds")












