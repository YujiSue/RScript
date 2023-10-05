#addLabels<-function(plot, )



barChart<-function(data, x, y,
                    bar_identify="color", 
                    error_bar, 
                    raw_data="bee", 
                    x_label = "X", 
                    y_label = "Y",
                    legend_label = Factor
                    ) {
    if (bar_identify == "none") {
        plot <- ggplot(data=data, aes(y=y, x=x))
    }
    else if (bar_identify == "color") {
        plot <- ggplot(data=data, aes(y=values, x=factor, fill=factor))
    }
    plot <- plot + stat_summary(fun.y="mean",geom="bar",colour="black")
    if (error_bar == "sd") {
        plot <- plot + geom_errorbar(aes(x=factor, ymin=average-sd, ymax=average+sd), width=.25)
    }
    if (raw_data == "point") {
        plot <- plot + geom_point(alpha=0.5)
    }
    else if (raw_data == "bee") {
        plot <- plot + geom_beeswarm()
    }
    else if (raw_data == "jitter") {
        plot <- plot + geom_jitter(position=position_jitter())
    }
    plot <- plot + xlab(x_label)
    plot <- plot + ylab(y_label)
    if (bar_identify == "color") {
        plot <- plot + labs(fill=legend_label)
    }
    plot <- plot + theme(axis.text.x = element_text(size=text_font_size), axis.text.y = element_text(size=text_font_size), legend.text=element_text(size=text_font_size))
    plot <- plot + theme(axis.title = element_text(size=title_font_size), legend.title = element_text(size=title_font_size))
    plot <- plot + theme(legend.position = legend)
    return(plot)
}

stackedBarChart <-function(data, 
                           mode="raw",
                           x="Factor1",
                           y="Value",
                           fill="Factor2" 
                           x_label="X", 
                           y_label="Y", 
                           legend="none", 
                           legend_label="Factor", 
                           title_font_size=16, 
                           text_font_size=12) {
  
    loadLibraries(c("dplyr", "ggplot2", "ggsignif", "ggbeeswarm"))
    plot <- ggplot(data, aes(y=y, x=x, fill=fill))
    if (mode=="raw") {
        plot <- plot + geom_bar(position="stack", stat="identity")
    }
    else if (mode=="percentage") {
        plot <- plot + geom_bar(position="fill", stat="identity")
    }
    plot <- plot + xlab(x_label)
    plot <- plot + ylab(y_label)
    if (bar_identify == "color") {
        plot <- plot + labs(fill=legend_label)
    }
    plot <- plot + theme(axis.text.x = element_text(size=text_font_size), axis.text.y = element_text(size=text_font_size), legend.text=element_text(size=text_font_size))
    plot <- plot + theme(axis.title = element_text(size=title_font_size), legend.title = element_text(size=title_font_size))
    plot <- plot + theme(legend.position = legend)
    return(plot)
}

boxChart<-function(data, 
                    mode="raw",
                    x="Factor1",
                           y="Value",
                           fill="Factor2" 
                           x_label="X", 
                           y_label="Y", 
                           legend="none", 
                           legend_label="Factor", 
                           title_font_size=16, 
                           text_font_size=12) {
    if (bar_identify == "none") {
        plot <- ggplot(data=data, aes(y=values, x=factor))
    }
else if (bar_identify == "color") {
    plot <- ggplot(data=data, aes(y=values, x=factor, fill = factor))
}
    plot <- plot + geom_boxplot()
    if (raw_data == "point") plot <- plot + geom_point(alpha=0.5)
    else if (raw_data == "bee") plot <- plot + geom_beeswarm()
    else if (raw_data == "jitter") plot <- plot + geom_jitter(position=position_jitter())
plot <- plot + xlab(x_label)
plot <- plot + ylab(y_label)
plot <- plot + labs(fill=col_label)
plot <- plot + theme(axis.text.x = element_text(size=text_font_size), axis.text.y = element_text(size=text_font_size), legend.text=element_text(size=text_font_size))
plot <- plot + theme(axis.title = element_text(size=title_font_size), legend.title = element_text(size=title_font_size))
plot
}

exportPlot<-function(plot, name, format) {
}
