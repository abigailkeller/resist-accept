##############################################
# Create stationary policies -- supplemental #
##############################################

# load packages
library(tidyverse)
library(MDPtoolbox)
library(patchwork)
library(viridis)
library(ggpubr)
library(RColorBrewer)

# define state space (species density)
K <- 1 # carrying capacity
states <- seq(0,K,by=0.01)

# action space (units of effort, ranging from 0 to 1)
actions <- seq(0, 1, 0.01)

# Time horizon
Tmax <- 150

# transition matrices
transition_filenames <- c("20240104_TM_r0_imm0.3.0.09_gamma.rds",
                          "20240104_TM_r1_imm0.3.0.09_gamma.rds",
                          "20240104_TM_r1_imm0.45.0.33_gamma.rds",
                          "20240104_TM_r1_imm1.2.0.25_gamma.rds",
                          "20240104_TM_r1_imm2.0.2_gamma.rds",
                          "20240104_TM_r1_imm4.0.125_gamma.rds",
                          "20240104_TM_r1_imm6.0.1_gamma.rds",
                          "20240104_TM_r1_imm14.0.05_gamma.rds",
                          "20240104_TM_r1_imm30.0.03_gamma.rds",
                          "20240104_TM_r1_imm52.0.025_gamma.rds",
                          "20240104_TM_r1_imm80.0.02_gamma.rds"
                          )

# set ecological change functions (loss types) and removal cost functions (penaltypes)
losstypes <- c('sig','exp')
penaltypes <- c('hpenal','lpenal')

# function to get utility matrix filenames
get_utility_filenames <- function(filename){
  #split up filename
  sub1 <- str_split(filename,'_')
  #sub2 <- str_split(sub1[[1]][4],'[.]')
  
  if(length(sub1[[1]])==4){
    out <- paste0(sub1[[1]][1],"_UM_",sub1[[1]][3],
                  "_",sub1[[1]][4])
  } else {
    sub3 <- str_split(sub1[[1]][5],'[.]')
    out <- paste0(sub1[[1]][1],"_UM_",sub1[[1]][3],
                  "_",sub1[[1]][4],"_",sub3[[1]][1])
  }
  
  utility_filenames <- c()
  
  for(i in 1:length(losstypes)){
    for(j in 1:length(penaltypes)){
      utility_filenames <- append(utility_filenames,paste0(out,"_",penaltypes[j],"_",losstypes[i],".rds"))
    }
  }
  
  return(utility_filenames)
}


# create empty data frame
out_df <- as.data.frame(matrix(NA,nrow=0,ncol=length(states)+4))
colnames(out_df) <- c('r','imm','penal','loss',states)

# set discount factor
beta <- 0.99

# get policies
for(i in 1:length(transition_filenames)){
  
  #load transition matrix
  transition <- readRDS(paste0('data/transition_matrices/',transition_filenames[i]))
  
  #get associated utility filenames
  utility_filenames <- get_utility_filenames(transition_filenames[i])
  
  for(j in 1:length(utility_filenames)){
    
    #load utility matrix
    utility <- readRDS(paste0('data/utility_matrices/',utility_filenames[j]))
    
    ####
    #run MDP -- backward iteration
    ####
    #mdp_output <- mdp_value_iteration(transition,utility,discount=0.99)
    
    # Action value vector at tmax
    Vtmax <- numeric(length(states))
    
    # Action value vector at t and t+1
    Vt <- numeric(length(states))
    Vtplus <- numeric(length(states))
    
    # Optimal policy vector
    D <- numeric(length(states))
    
    # The backward iteration consists in storing action values in the vector Vt which is the maximum of
    # utility plus the future action values for all possible next states. Knowing the final action 
    # values, we can then backwardly reset the next action value Vtplus to the new value Vt. We start 
    # The backward iteration at time T-1 since we already defined the action #value at Tmax.
    for (t in (Tmax-1):1) {
      
      # We define a matrix Q that stores the updated action values for #all states (rows)
      # actions (columns)
      Q <- array(0, dim=c(length(states), length(actions)))
      
      for (a in 1:length(actions)) {
        
        # For each harvest rate we fill for all states values (row) #the ith column (Action) of matrix Q
        # The utility of the ith action recorded for all states is #added to the product of the transition matrix of the ith #action by the action value of all states 
        Q[,a] <- utility[,a] + beta*(transition[,,a] %*% Vtplus)
        
      } # end of the harvest loop
      
      # Find the optimal action value at time t is the maximum of Q
      Vt <- apply(Q, 1, max)
      
      # After filling vector Vt of the action values at all states, we #update the vector Vt+1 to Vt and we go to the next step standing #for previous time t-1, since we iterate backward
      Vtplus <- Vt
      
    } # end of the time loop
    
    for (k in seq_along(states)) {
      # We look for each state which column of Q corresponds to the #maximum of the last updated value 
      # of Vt (the one at time t+1). If the index vector is longer than 1 #(if there is more than one optimal value we chose the minimum #harvest rate)
      D[k] <- actions[(min(which(round(Q[k,],13) == round(Vt[k],13))))]
    }
    
    #get optimal policies
    #policy <- actions[mdp_output$policy]
    
    #get metadata from filenames
    out_r <- as.numeric(str_split(str_split(transition_filenames[i],'_')[[1]][3],'')[[1]][2])
    full_imm <- str_split(transition_filenames[i],'_')[[1]][4]
    #out_imm <- as.numeric(gsub("imm","",gsub(".rds","",full_imm)))
    out_penal <- ifelse(length(str_split(utility_filenames[j],'_')[[1]])==6,
                        str_split(utility_filenames[j],'_')[[1]][5],
                        str_split(utility_filenames[j],'_')[[1]][6])
    out_loss <- ifelse(length(str_split(utility_filenames[j],'_')[[1]])==6,
                       str_split(str_split(utility_filenames[j],'_')[[1]][6],'[.]')[[1]][1],
                       str_split(str_split(utility_filenames[j],'_')[[1]][7],'[.]')[[1]][1])
    
    #concatenate data and add to df
    out_df[nrow(out_df)+1,] <- c(out_r,full_imm,out_penal,out_loss,D)
  }
}



##########
# plotting

# create long df
out_df_long <- out_df %>% 
  pivot_longer(cols=!c(penal,loss,r,imm),
               values_to = 'policy', names_to = 'state')

# convert to numeric
out_df_long$state <- as.numeric(out_df_long$state)
out_df_long$policy <- as.numeric(out_df_long$policy)

#r = 0, mu = 0.03
mu0.03_r0 <- ggplot()+
  geom_line(data=out_df_long[c(out_df_long$imm=='imm0.3.0.09' &
                               out_df_long$r=='0'),],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='optimal removal effort',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('A. \u03B1 = 0.00, r = 0'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 0.03
mu0.03_r1 <- ggplot()+
  geom_line(data=out_df_long[c(out_df_long$imm=='imm0.3.0.09' &
                               out_df_long$r=='1'),],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('B. \u03B1 = 0.00, r = 1'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 0.15
mu0.15 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm0.45.0.33',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('C. \u03B1 = 0.02, r = 1'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 0.3
mu0.3 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm1.2.0.25',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('D. \u03B1 = 0.04, r = 1'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 0.4
mu0.4 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm2.0.2',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='optimal removal effort',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('E. \u03B1 = 0.06, r = 1'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 0.5
mu0.5 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm4.0.125',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('F. \u03B1 = 0.07, r = 1'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 0.6
mu0.6 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm6.0.1',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('G. \u03B1 = 0.12, r = 1'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 0.7
mu0.7 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm14.0.05',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('H. \u03B1 = 0.14, r = 1'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1
mu1 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm30.0.03',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='optimal removal effort',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('I. \u03B1 = 0.69, r = 1'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1.3
mu1.3 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm52.0.025',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('J. \u03B1 = 0.99, r = 1'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1.6
mu1.6 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm80.0.02',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('K. \u03B1 = 1.00, r = 1'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1.3, with legend
mu1.3_wlegend <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm52.0.025',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='',
       color='Removal cost function',
       linetype='Ecological change\nfunction')+
  ggtitle(paste0('mu = 1.3, phi = 30'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.text=element_text(size=9),
        legend.title=element_text(size=11),
        legend.key.width = unit(1,'cm'),
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))
legend <- get_legend(mu1.3_wlegend)

#define plot layout
layout <- "
AABBCCDD
AABBCCDD
EEFFGGHH
EEFFGGHH
IIJJKKLL
IIJJKKLL
"

final_stat_plot <- mu0.03_r0+mu0.03_r1+mu0.15+mu0.3+mu0.4+mu0.5+
  mu0.6+mu0.7+mu1+mu1.3+mu1.6+legend+plot_layout(design=layout)
ggsave('figures/supplemental_resist_accept_boundary.tiff',final_stat_plot,dpi=700,width=8,height=7)

