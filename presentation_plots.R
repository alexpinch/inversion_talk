library(tidyverse)
all_data <- tibble()
files <- length(list.files("/Users/alexpinch/GitHub/private/jan30_presentation/data_101822/full_runs/"))
full_runs <- list.files("/Users/alexpinch/GitHub/private/jan30_presentation/data_101822/full_runs/")
for (i in 1:files) {
  run <- (full_runs[i])
  typeof(run)
  setwd(file.path("/Users/alexpinch/GitHub/private/jan30_presentation/data_101822/full_runs/",run))
  run_data <- read.csv(file = paste(run,".csv",sep=""),skip=1,header=F) %>%
    rename(gen=V1,pop=V2,sample=V3,fitness=V4,inv_genotype=V5)
  run_data <- run_data %>% mutate(sim_run=run)
  all_data <- rbind(all_data, run_data)
}
all_data %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                   inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                   inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                   inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                   TRUE ~ fitness)) %>%
  group_by(gen, pop, inv_genotype)%>%
  summarize(mean_fitness= mean(fixed_fitness,na.rm=T)) %>%
  ggplot(.,aes(x=gen,y=mean_fitness,group=inv_genotype,color=inv_genotype)) +
  geom_smooth(method="loess") +
  facet_wrap(~pop)

summarized_data <- all_data %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                   inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                   inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                   inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                   TRUE ~ fitness)) %>%
  filter(!is.na(inv_genotype)) %>%
  group_by(gen, sim_run, inv_genotype) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  group_by(gen,inv_genotype) %>%
  mutate(qnt_90 = quantile(mean_fit, 0.9),
         qnt_10 = quantile(mean_fit, 0.1),
         mean_sim_fit = quantile(mean_fit, 0.9)) ## 0.5 = median

summarized_data %>%
  ggplot(.) +
  geom_line(aes(x=gen,y=mean_sim_fit,group=inv_genotype,color=as.factor(inv_genotype))) +
  geom_ribbon(aes(x=gen,ymin=qnt_10,ymax=qnt_90,group=inv_genotype,fill=as.factor(inv_genotype)),alpha=0.2) # alpha = transparency

#Allele frequency change over time
all_data %>%
  group_by(gen,pop,inv_genotype) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(.,aes(x=gen,y=freq,color=as_factor(inv_genotype))) +
  geom_line() +
  facet_wrap(~pop)
