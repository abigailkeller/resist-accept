create_eco_objective_plots <- function(ecotype,states,sig_loss_params,
                                       exp_loss_params){
  
  data <- rbind(
    data.frame(
      state = states,
      reward = reward_exp(states,exp_loss_params),
      type = 'Exponential'
    ),
    data.frame(
      state = states,
      reward = reward_sig(states,sig_loss_params),
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
      labs(x='state (EGC density)',y='reward',linetype='')+
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
      labs(x='state (EGC density)',y='reward',linetype='')+
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
      labs(x='state (EGC density)',y='reward',linetype='')+
      ggtitle('Ecological change')+
      theme_minimal()+
      theme(text = element_text(size=16),
            legend.key.width = unit(1.25,'cm'))
    
  }
  return(plot)
}

create_cost_objective_plots <- function(penaltype,ratio,actions){
  
  action_intersect <- 0.75
  linear_scale <- ratio*0.2/action_intersect
  nonlinear_exp <- 4
  
  data <- rbind(
    data.frame(
      actions = actions,
      reward = -get_nonlinear_scale(linear_scale,action_intersect,nonlinear_exp)*actions^nonlinear_exp,
      type = 'Nonlinear'
    ),
    data.frame(
      actions = actions,
      reward = -linear_scale*actions,
      type = 'Linear'
    )
  )
  
  if(penaltype == 'Both'){
    
    plot <- ggplot(data=data)+
      geom_line(aes(x=actions,y=reward,color=type),linewidth=1.75)+
      scale_color_manual(values = c('mediumorchid','green4'),
                         labels = c('Linear','Nonlinear'))+
      labs(x='action (removal effort)',y='reward',color='')+
      ggtitle('Removal cost')+
      theme_minimal()+
      theme(text = element_text(size=16),)
    
  } else if(penaltype == 'Nonlinear'){
    
    plot <- ggplot(data=data[data$type=='Nonlinear',])+
      geom_line(aes(x=actions,y=reward,color=type),linewidth=1.75)+
      scale_color_manual(values = c('green4'),
                         labels = c('Nonlinear'))+
      labs(x='action (removal effort)',y='reward',color='')+
      ggtitle('Removal cost')+
      theme_minimal()+
      theme(text = element_text(size=16),)
    
  } else if(penaltype == 'Linear'){
    
    plot <- ggplot(data=data[data$type=='Linear',])+
      geom_line(aes(x=actions,y=reward,color=type),linewidth=1.75)+
      scale_color_manual(values = c('mediumorchid'),
                         labels = c('Linear'))+
      labs(x='action (removal effort)',y='reward',color='')+
      ggtitle('Removal cost')+
      theme_minimal()+
      theme(text = element_text(size=16),)
  }
  
  return(plot)
}

#ecological reward -- sigmoidal
reward_sig <- function(s,loss_params){
  loss_params$loss_a/(1+exp(-loss_params$loss_b*(s-loss_params$loss_c)))
}

#ecological reward -- exponential
reward_exp <- function(s,loss_params){
  loss_params$loss_a*(exp(-loss_params$loss_b*s)-loss_params$loss_c)
}

#reward -- penalize effort -- linear
reward_penalize_linear <- function(s,fun,loss_params,linear_scale,action){
  fun(s,loss_params)-linear_scale*action
}

#function to get nonlinear scale parameter, given linear scale parameter and action intersection
get_nonlinear_scale <- function(linear_scale,action_intersect,nonlinear_exp){
  nonlinear_scale <- linear_scale*action_intersect/(action_intersect^nonlinear_exp)
  return(nonlinear_scale)
}
