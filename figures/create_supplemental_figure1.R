###################################################
# create plots of alpha for supplemental figure 1 #
###################################################

library(tidyverse)
library(viridis)
library(patchwork)

k_vec <- c(0.2,0.5,2,4,15,30,50,75,125,500)
theta_vec <- c(0.025,0.1,0.1,0.125,0.05,1/30,0.025,0.02,0.016,0.01)

# create vector of available alpha
alpha_seq <- seq(0.01,6,by=0.01)

out <- as.data.frame(matrix(NA,nrow=0,ncol=5))
colnames(out) <- c('alpha','prob','shape','scale','mean')
for(i in 1:length(k_vec)){
  df <- data.frame(
    alpha = alpha_seq,
    prob = dgamma(alpha_seq,shape=k_vec[i],scale=theta_vec[i])/
      sum(dgamma(alpha_seq,shape=k_vec[i],scale=theta_vec[i])),
    shape = rep(k_vec[i],length(alpha_seq)),
    scale = rep(theta_vec[i],length(alpha_seq)),
    mean = rep(k_vec[i]*theta_vec[i],length(alpha_seq))
  )
  out <- rbind(out,df)
}

out$mean <- round(out$mean,3)

my_palette <- viridis(10)


plot1 <- ggplot(data=out[out$mean==0.005,])+
  geom_line(aes(x=alpha,y=prob,color=as.factor(mean)),
            linewidth=1)+
  scale_color_manual(values='#440154FF',labels='0.005')+
  labs(x='',y='probability',color='mean \u03B1 / K')+
  scale_x_continuous(breaks=c(0,0.1,0.2),labels=c('0','0.1','0.2'),
                     limits=c(0,0.25))+
  theme_minimal()+
  ggtitle('A.')+
  theme(text = element_text(size = 15))
plot2 <- ggplot(data=out %>% filter(mean!=0.005 & mean!=5))+
  geom_line(aes(x=alpha,y=prob,color=as.factor(mean)),
            linewidth=1)+
  scale_color_manual(values=c("#482878FF","#3E4A89FF","#31688EFF","#26828EFF",
                              "#1F9E89FF","#35B779FF","#6DCD59FF","#B4DE2CFF"),
                     labels=c('0.05','0.2','0.5','0.75',
                              '1','1.25','1.5','2'))+
  labs(x='',y='probability',color='mean \u03B1 / K')+
  scale_x_continuous(breaks=c(0,1,2),labels=c('0','1.0','2.0'),
                     limits=c(0,2.5))+
  theme_minimal()+
  ggtitle('B.')+
  theme(text = element_text(size = 15))
plot3 <- ggplot(data=out[out$mean==5,])+
  geom_line(aes(x=alpha,y=prob,color=as.factor(mean)),
            linewidth=1)+
  scale_color_manual(values='#FDE725FF',labels='5.0')+
  labs(x='scaled source\npopulation size\n(\u03B1 / K)',y='probability',color='mean \u03B1 / K')+
  scale_x_continuous(breaks=c(4,5,6),labels=c('4.0','5.0','6.0'),
                     limits=c(4,6))+
  theme_minimal()+
  ggtitle('C.')+
  theme(text = element_text(size = 15))

#define plot layout
layout <- "
AA
BB
CC
"

final_plot <- plot1+plot2+plot3+plot_layout(design=layout)
ggsave('figures/supplemental_alpha.tiff',final_plot,
       dpi=700,width=4,height=7)

