#' plotFunctions
#' Functions to plot identified charts

########## Gains ##########
##### Cumulative Gains Chart #####
gains_chart <- function(data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(data[[1]]$Cum.Response.Pct)
  if(length(data) > 1){
    for(i in 2:length(data)){
      plot_table <- cbind(plot_table, data[[i]]$Cum.Response.Pct)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(data)
  plot_table$Cum.Records.Pct <- data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table, id.vars='Cum.Records.Pct')

  # Create ggplot object(line chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
       geom_line(aes(y = value, colour = variable),
                 size = 0.6) +
       geom_line(data = data[[1]],
                 aes(x = Cum.Records.Pct, y = Random.Response.Pct, lty='Random'),
                 colour = "black",
                 lty = "longdash",
                 size = 0.6) +
       scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
       scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Cumulative Gains Chart")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("% Cumulative Response")
  }
  else{
    ylabel <- ylab(ylabel)
  }
  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}


##### Decile Gains Chart #####
decile_gains_chart <- function(decile_data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(decile_data[[1]]$Cum.Response.Pct)
  if(length(decile_data) > 1){
    for(i in 2:length(decile_data)){
      plot_table <- cbind(plot_table, decile_data[[i]]$Cum.Response.Pct)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(decile_data)
  plot_table$Cum.Records.Pct <- decile_data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table[-1,], id.vars='Cum.Records.Pct')

  # Create ggplot object(bar chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_bar(aes(fill = variable, y = value, x = Cum.Records.Pct),
             position="dodge",
             stat="identity") +
    geom_line(data = decile_data[[1]],
              aes(x = Cum.Records.Pct, y = Random.Response.Pct, lty = "Random"),
              colour = "black",
              lty = "longdash",
              size = 0.6) +
    scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100)) +
    scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Cumulative Decile Gains Chart")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("% Cumulative Response")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}


##### Profit Gains Chart #####
profit_gains_chart <- function(data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(data[[1]]$Cum.Profit)
  if(length(data) > 1){
    for(i in 2:length(data)){
      plot_table <- cbind(plot_table, data[[i]]$Cum.Profit)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16)
                   , panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(data)
  plot_table$Cum.Records.Pct <- data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table, id.vars='Cum.Records.Pct')

  # Create ggplot object(line chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_line(aes(y = value, colour = variable),
              size = 0.6) +
    geom_hline(aes(yintercept = 0), colour = "grey", lty = "dashed") +
    geom_line(data = data[[1]],
              aes(x = Cum.Records.Pct, y = Cum.Random.Profit, lty = "Random"),
              colour = "black",
              lty = "longdash",
              size = 0.6) +
    scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Cumulative Profit Gains Chart")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("Cumulative Profits")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}


##### Decile Profit Gains Chart #####
decile_profit_gains_chart <- function(decile_data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(decile_data[[1]]$Cum.Profit)
  if(length(decile_data) > 1){
    for(i in 2:length(decile_data)){
      plot_table <- cbind(plot_table, decile_data[[i]]$Cum.Profit)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(decile_data)
  plot_table$Cum.Records.Pct <- decile_data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table, id.vars='Cum.Records.Pct')

  # Create ggplot object(bar chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_bar(aes(fill = variable, y = value, x = Cum.Records.Pct),
             position="dodge",
             stat="identity") +
    scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Cumulative Decile Profit Gains Chart")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("Cumulative Profits")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}

##### Non-cumulative Decile Gains Chart #####
noncum_decile_gains_chart <- function(decile_data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(decile_data[[1]]$Response.Pct)
  if(length(decile_data) > 1){
    for(i in 2:length(decile_data)){
      plot_table <- cbind(plot_table, decile_data[[i]]$Response.Pct)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(decile_data)
  plot_table$Cum.Records.Pct <- decile_data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table[-1,], id.vars='Cum.Records.Pct')

  # Create ggplot object(bar chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_bar(aes(fill = variable, y = value, x = Cum.Records.Pct),
             position="dodge",
             stat="identity") +
    geom_hline(aes(yintercept = 10, colour = "black", lty = "Random"),
               colour = "black",
               lty = "longdash") +
    scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100)) +
    scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Non-Cumulative Decile Gains Chart")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("% Response")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}

##### Non-cumulative Decile Profit Gains Chart #####
noncum_decile.profit_gains_chart <- function(decile_data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(decile_data[[1]]$Profit)
  if(length(decile_data) > 1){
    for(i in 2:length(decile_data)){
      plot_table <- cbind(plot_table, decile_data[[i]]$Profit)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(decile_data)
  plot_table$Cum.Records.Pct <- decile_data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table, id.vars='Cum.Records.Pct')

  # Create ggplot object(bar chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_bar(aes(fill = variable, y = value, x = Cum.Records.Pct),
             position="dodge",
             stat="identity") +
    scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Non-Cumulative Decile Profit Gains Chart")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("Profits")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}


####### Lift #######
##### Lift Chart #####
lift_chart <- function(data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(data[[1]]$Lift)
  if(length(data) > 1){
    for(i in 2:length(data)){
      plot_table <- cbind(plot_table, data[[i]]$Lift)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(data)
  plot_table$Cum.Records.Pct <- data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table[-1,], id.vars='Cum.Records.Pct')

  # Create ggplot object(line chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_line(aes(y = value, colour = variable),
              size = 0.6) +
    geom_hline(aes(yintercept = 1, lty = "Random"),
               colour = "black",
               lty = "longdash") +
    scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Lift Chart")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("Lift")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}

##### Decile Lift Chart #####
decile_lift_chart <- function(decile_data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(decile_data[[1]]$Lift)
  if(length(decile_data) > 1){
    for(i in 2:length(decile_data)){
      plot_table <- cbind(plot_table, decile_data[[i]]$Lift)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(decile_data)
  plot_table$Cum.Records.Pct <- decile_data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table[-1,], id.vars='Cum.Records.Pct')

  # Create ggplot object(bar chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_bar(aes(fill = variable, y = value, x = Cum.Records.Pct),
             position="dodge",
             stat="identity") +
    geom_hline(aes(yintercept = 1, lty = "Random"),
               colour = "black",
               lty = "longdash",) +
    scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Decile Lift Chart")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("Lift")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}

##### Profit Lift Chart #####
profit_lift_chart <- function(data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(data[[1]]$Profit.Diff)
  if(length(data) > 1){
    for(i in 2:length(data)){
      plot_table <- cbind(plot_table, data[[i]]$Profit.Diff)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(data)
  plot_table$Cum.Records.Pct <- data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table, id.vars='Cum.Records.Pct')

  # Create ggplot object(line chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_hline(aes(yintercept = 0, lty = 'Random'),
               colour = "black",
               lty = "longdash") +
    geom_line(aes(y = value, colour = variable),
              size = 0.6) +
    scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Profit Lift Chart (Additive)")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("Incremental Profits")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}

##### Decile Profit Lift Chart #####
decile_profit_lift_chart <- function(decile_data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(decile_data[[1]]$Profit.Diff)
  if(length(decile_data) > 1){
    for(i in 2:length(decile_data)){
      plot_table <- cbind(plot_table, decile_data[[i]]$Profit.Diff)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(decile_data)
  plot_table$Cum.Records.Pct <- decile_data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table, id.vars='Cum.Records.Pct')

  # Create ggplot object(bar chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_bar(aes(fill = variable, y = value, x = Cum.Records.Pct),
             position="dodge",
             stat="identity") +
    scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Decile Profit Lift Chart (Additive)")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("Incremental Profits")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}

##### Non-cumulative Decile lift Chart #####
noncum_decile_lift_chart <- function(decile_data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(decile_data[[1]]$Noncum.Lift)
  if(length(decile_data) > 1){
    for(i in 2:length(decile_data)){
      plot_table <- cbind(plot_table, decile_data[[i]]$Noncum.Lift)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(decile_data)
  plot_table$Cum.Records.Pct <- decile_data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table[-1,], id.vars='Cum.Records.Pct')

  # Create ggplot object(bar chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_bar(aes(fill = variable, y = value, x = Cum.Records.Pct),
             position="dodge",
             stat="identity") +
    geom_hline(aes(yintercept = 10, colour = "black", lty = "Random"),
               colour = "black",
               lty = "longdash") +
    scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100)) +

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Non-Cumulative Decile Lift Chart")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("Lift")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}

##### Non-cumulative Decile Profit Lift Chart #####
noncum_decile_profit_lift_chart <- function(decile_data, title, xlabel, ylabel){
  # Create ploting table
  plot_table <- data.frame(decile_data[[1]]$Noncum.Profit.Diff)
  if(length(decile_data) > 1){
    for(i in 2:length(decile_data)){
      plot_table <- cbind(plot_table, decile_data[[i]]$Noncum.Profit.Diff)
    }
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank())
  }
  else{
    theme <- theme(plot.title = element_text(hjust = 0.5, face="bold", size= 16),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none")
  }
  colnames(plot_table) <- names(decile_data)
  plot_table$Cum.Records.Pct <- decile_data[[1]]$Cum.Records.Pct
  plot_table <- melt(plot_table, id.vars='Cum.Records.Pct')

  # Create ggplot object(bar chart)
  p <- ggplot(plot_table, aes(colour=variable, y = value, x = Cum.Records.Pct)) +
    geom_bar(aes(fill = variable, y = value, x = Cum.Records.Pct),
             position="dodge",
             stat="identity") +
    scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))

  # Create title
  if(is.null(title) == T){
    title <- ggtitle("Non-Cumulative Decile Profit Lift Chart (Additive)")
  }
  else{
    title <- ggtitle(title)
  }

  # Create xlab
  if(is.null(xlabel) == T){
    xlabel <- xlab("% Records")
  }
  else{
    xlabel <- xlab(xlabel)
  }

  # Create ylab
  if(is.null(ylabel) == T){
    ylabel <- ylab("Profits")
  }
  else{
    ylabel <- ylab(ylabel)
  }

  plot_obj <- p + theme + title + xlabel + ylabel
  print(plot_obj)
  return(toString(plot_obj))
}
