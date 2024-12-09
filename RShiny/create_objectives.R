create_eco_objective_plots <- function(ecotype,states,sig_loss_params,
                                       exp_loss_params,weight){
  
  data <- rbind(
    data.frame(
      state = states,
      reward = weight*reward_exp(states,exp_loss_params),
      type = 'Exponential'
    ),
    data.frame(
      state = states,
      reward = weight*reward_sig(states,sig_loss_params),
      type = 'Sigmoidal'
    )
  )
  
  if(ecotype=='Both'){
    plot <- ggplot()+
      geom_line(data=data,
                aes(x=state,y=reward,linetype=type),
                color='black',linewidth=1.75)+
      scale_linetype_manual(values = c('solid','dotted'),
                            labels=c('Exponential','Sigmoidal'))+
      scale_x_continuous(breaks=c(0,1),labels=c(0,'K'),
                         limits=c(0,1.1))+
      labs(x='state (EGC density)',
           y=bquote("weighted value (" * w[d] ~ V[d] * ")"),linetype='')+
      ggtitle('Ecological change')+
      theme_minimal()+
      theme(text = element_text(size=16),
            legend.key.width = unit(1.25,'cm'))
    
    
  } else if(ecotype=='Sigmoidal'){
    plot <- ggplot()+
      geom_line(data=data[data$type=='Sigmoidal',],
                aes(x=state,y=reward,linetype=type),
                color='black',linewidth=1.75)+
      scale_linetype_manual(values = c('dotted'),
                            labels=c('Sigmoidal'))+
      scale_x_continuous(breaks=c(0,1),labels=c(0,'K'),
                         limits=c(0,1.1))+
      labs(x='state (EGC density)',
           y=bquote("weighted value (" * w[d] ~ V[d] * ")"),linetype='')+
      ggtitle('Ecological change')+
      theme_minimal()+
      theme(text = element_text(size=16),
            legend.key.width = unit(1.25,'cm'))
    
  } else if(ecotype=='Exponential'){
    plot <- ggplot()+
      geom_line(data=data[data$type=='Exponential',],
                aes(x=state,y=reward,linetype=type),
                color='black',linewidth=1.75)+
      scale_linetype_manual(values = c('solid'),
                            labels=c('Exponential'))+
      scale_x_continuous(breaks=c(0,1),labels=c(0,'K'),
                         limits=c(0,1.1))+
      labs(x='state (EGC density)',
           y=bquote("weighted value (" * w[d] ~ V[d] * ")"),linetype='')+
      ggtitle('Ecological change')+
      theme_minimal()+
      theme(text = element_text(size=16),
            legend.key.width = unit(1.25,'cm'))
    
  }
  return(plot)
}

create_cost_objective_plots <- function(penaltype,actions,
                                        nonlinear_exp,weight){
  
  data <- rbind(
    data.frame(
      actions = actions,
      reward = -weight*actions^nonlinear_exp,
      type = 'Nonlinear'
    ),
    data.frame(
      actions = actions,
      reward = -weight*actions,
      type = 'Linear'
    )
  )
  
  if(penaltype == 'Both'){
    
    plot <- ggplot(data=data)+
      geom_line(aes(x=actions,y=reward,color=type),linewidth=1.75)+
      scale_color_manual(values = c('mediumorchid','green4'),
                         labels = c('Linear','Nonlinear'))+
      labs(x='action (removal effort)',
           y=bquote("weighted value (" * w[c] ~ V[c] * ")"),color='')+
      ggtitle('Removal cost')+
      theme_minimal()+
      theme(text = element_text(size=16),)
    
  } else if(penaltype == 'Nonlinear'){
    
    plot <- ggplot(data=data[data$type=='Nonlinear',])+
      geom_line(aes(x=actions,y=reward,color=type),linewidth=1.75)+
      scale_color_manual(values = c('green4'),
                         labels = c('Nonlinear'))+
      labs(x='action (removal effort)',
           y=bquote("weighted value (" * w[c] ~ V[c] * ")"),color='')+
      ggtitle('Removal cost')+
      theme_minimal()+
      theme(text = element_text(size=16),)
    
  } else if(penaltype == 'Linear'){
    
    plot <- ggplot(data=data[data$type=='Linear',])+
      geom_line(aes(x=actions,y=reward,color=type),linewidth=1.75)+
      scale_color_manual(values = c('mediumorchid'),
                         labels = c('Linear'))+
      labs(x='action (removal effort)',
           y=bquote("weighted value (" * w[c] ~ V[c] * ")"),color='')+
      ggtitle('Removal cost')+
      theme_minimal()+
      theme(text = element_text(size=16),)
  }
  
  return(plot)
}

# function for ecological change -- sigmoidal
reward_sig <- function(s,loss_params){
  reward <- loss_params$loss_a/(1+exp(-loss_params$loss_b*(s-loss_params$loss_c)))
  out <- reward/(max(reward)-min(reward))
  return(out)
}
# function for ecological change -- exponential
reward_exp <- function(s,loss_params){
  reward <- loss_params$loss_a*(exp(-loss_params$loss_b*s)-loss_params$loss_c)
  out <- reward/(max(reward)-min(reward))
  return(out)
}
