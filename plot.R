library(dplyr)
library(ggplot2)
library(grid)

name <- read.csv('name.csv', stringsAsFactors = F)
getName <- function(key, type='short',nd=name,unit=F){
    ret <- nd[nd$name==key,][[type]]
    if(unit) ret <- paste0(ret," ",nd[nd$name==key,][['unit']])
    return(ret)
}

color15 = rev(c('#df0c21','#e44227','#e86030','#ec793e','#f0904f',
            '#f0904f','#d8a248','#bdb150','#a1bd65','#86c682',
            '#69b85a','#00a481','#008aa0','#006ca6','#12498d'))
# color15 = topo.colors(15)
fig.width = 6
fig.height = 6
fig.dpi = 100
fig.unit = 'in'
fig.ext = '.png'
fig.cex.main = 20
fig.cex.lab = 12
fig.cex.axis = 12
fig.size = 1.5

####
# T-SNE
####
f <- read.csv('F_tsne_species.csv', stringsAsFactors = F)
km <- read.csv('F_tsne_species_km_labels.csv', stringsAsFactors = F)
f = merge(f, km, all.x = TRUE, by='Species')
f = merge(f, d, all.x = TRUE, by='Species')

tmp = unique(f[c('km_labels', 'new_km_labels')])
dict_old_new = list()
dict_old_new[tmp$km_labels+1] = tmp$new_km_labels+1
rm(tmp)

f$new_km_labels <- as.factor(f$new_km_labels+1)

center <- read.csv('km_centers.csv', stringsAsFactors = F)
center <- center[order(-center$center_y),]
center$km_labels <- as.factor(seq(1,15))

ggplot(f, aes(x=tsne.0, y=tsne.1, color=alt)) +
    geom_point(size=fig.size)+
    scale_color_gradientn(colours=c('#df4d0c','#f0904f','#86c682', '#69b85a','#006184','#12498d'))+
    ggtitle('t-SNE Visualization')+
    labs(x='t-SNE 1', y='t-SNE 2',color=paste0(getName('alt', type='cap_short'),"\n(meter)"))+
    theme_bw()+
    theme(
        plot.title =element_text(size=fig.cex.main,hjust = 0.5),
        legend.title = element_text(size=fig.cex.lab),
        legend.text = element_text(size=fig.cex.lab),
        axis.title = element_text(size=fig.cex.axis),
        axis.text = element_text(size=fig.cex.axis)
    )
ggsave(filename=paste0("plot/t-SNE_by_elevation",fig.ext),width = fig.width, height=fig.height, units=fig.unit)

library(ggrepel)
ggplot(f, aes(x=tsne.0, y=tsne.1, color=new_km_labels)) +
    geom_point(size=fig.size)+
    scale_color_manual(values=color15)+
    ggtitle('Species Assemblage on t-SNE')+
    labs(x='t-SNE 1', y='t-SNE 2', color='group')+
    theme_bw()+
    theme(
        plot.title =element_text(size=fig.cex.main,hjust = 0.5),
        legend.title = element_text(size=fig.cex.lab),
        legend.text = element_text(size=fig.cex.lab),
        legend.position = "none",
        axis.title = element_text(size=fig.cex.axis),
        axis.text = element_text(size=fig.cex.axis)
    )+
    geom_point(data=center, aes(x=center_x, y=center_y), color='black', shape=4, size=1, stroke=2)+
    geom_label_repel(data = center, aes(x=center_x, y=center_y, label = km_labels), color = 'black',
                     size = 4) 
ggsave(filename=paste0("plot/t-SNE_by_cluster",fig.ext),width = fig.width, height=fig.height, units=fig.unit)

###########
# species #
###########
d = read.csv('updated_species_regression_v2.csv', stringsAsFactors = F)
d$X <- NULL
d$v.ratio = d$v.body_fore/d$v.whole

df <- d %>% group_by(km_label) %>% summarize(v.ratio = mean(v.body_fore)/mean(v.whole),
                                             s.ratio = mean(s.body_fore)/mean(s.whole),
                                             srad = mean(srad),
                                             tavg = mean(tavg),
                                             prec = mean(prec),
                                             wind = mean(wind),
                                             vapr = mean(vapr),
                                             tmin = mean(tmin),
                                             tmax = mean(tmax),
                                             alt = mean(alt))
df$km_label <- as.factor(df$km_label)

# Create a text
plot.list <- list()
for(key in c('alt','tavg','tmax','tmin','srad','vapr','prec','wind')){
    print(getName(key, unit=T, type='cap_long'))
    cor <- cor.test(df[[key]], df[["v.ratio"]])
    if(cor$estimate>0){
        text <- paste0('\n   cor: ', round(cor$estimate,4),"\n   p-value: ", round(cor$p.value,4))
        hj = 0
        vj = 1
        xm = -Inf
        ym = Inf
    }else{
        text <- paste0('\ncor: ', round(cor$estimate,4),"   \np-value: ", round(cor$p.value,4),"   ")
        hj = 1
        vj = 1
        xm = Inf
        ym = Inf
    }
    # Plot
    sp <- ggplot(df, aes_string(x=key, y="v.ratio", color="km_label", label="km_label")) + 
        geom_smooth(method = "lm", se = FALSE,color='gray')+
        geom_point(size=fig.size*4)+
        geom_text(color='white') +
        ggtitle(paste0('Brightness along ', getName(key, type='cap_short')))+
        labs(x=getName(key, type='cap_short',unit = T), y='Relative Brightnesss', color='group')+
        scale_color_manual(values=color15)+
        theme_bw()+
        theme(
            plot.title =element_text(size=fig.cex.main,hjust = 0.3, vjust=0.3),
            legend.title = element_text(size=fig.cex.lab),
            legend.text = element_text(size=fig.cex.lab),
            legend.position="none",
            axis.title = element_text(size=fig.cex.axis),
            axis.text = element_text(size=fig.cex.axis)
        )+
        annotate("text", xm, ym, label = text , color="black", hjust = hj, vjust = vj)
    plot.list[[key]] <- sp
    ggsave(filename=paste0("plot/cluster_",getName(key),"_vs_",getName('rv'),fig.ext),width = fig.width, height=fig.height, units=fig.unit)
    
}
library(gridExtra)
png(paste0("plot/cluster_brightness.png"),width = fig.width*4, height=fig.height*2, res=512, units=fig.unit)
grid.arrange(plot.list[['alt']],
             plot.list[['tavg']],
             plot.list[['tmax']],
             plot.list[['tmin']],
             plot.list[['srad']],
             plot.list[['vapr']],
             plot.list[['prec']],
             plot.list[['wind']], nrow = 2)
dev.off()
# ggsave(filename=paste0("plot/cluster_brightness",fig.ext),width = fig.width*4, height=fig.height*2, units=fig.unit)


# Create a text
plot.list <- list()
for(key in c('alt','tavg','tmax','tmin','srad','vapr','prec','wind')){
    print(getName(key, unit=T, type='cap_long'))
    cor <- cor.test(df[[key]], df[["s.ratio"]])
    if(cor$estimate>0){
        text <- paste0('\n   cor: ', round(cor$estimate,4),"\n   p-value: ", round(cor$p.value,4))
        hj = 0
        vj = 1
        xm = -Inf
        ym = Inf
    }else{
        text <- paste0('\ncor: ', round(cor$estimate,4),"   \np-value: ", round(cor$p.value,4),"   ")
        hj = 1
        vj = 1
        xm = Inf
        ym = Inf
    }
    # Plot
    sp <- ggplot(df, aes_string(x=key, y="s.ratio", color="km_label", label="km_label")) + 
        geom_smooth(method = "lm", se = FALSE,color='gray')+
        geom_point(size=fig.size*4)+
        geom_text(color='white') +
        ggtitle(paste0('Saturation along ', getName(key, type='cap_short')))+
        labs(x=getName(key, type='cap_short', unit = T), y='Relative Saturation', color='group')+
        scale_color_manual(values=color15)+
        theme_bw()+
        theme(
            plot.title =element_text(size=fig.cex.main,hjust = 0.3, vjust=0.3),
            legend.title = element_text(size=fig.cex.lab),
            legend.text = element_text(size=fig.cex.lab),
            legend.position="none",
            axis.title = element_text(size=fig.cex.axis),
            axis.text = element_text(size=fig.cex.axis)
        )+
        annotate("text", xm, ym, label = text , color="black", hjust = hj, vjust = vj)
    plot.list[[key]] <- sp
    ggsave(filename=paste0("plot/cluster_",getName(key),"_vs_",getName('rs'),fig.ext),width = fig.width, height=fig.height, units=fig.unit)
    
}
library(gridExtra)
png(paste0("plot/cluster_saturation.png"),width = fig.width*4, height=fig.height*2, res=512, units=fig.unit)
grid.arrange(plot.list[['alt']],
             plot.list[['tavg']],
             plot.list[['tmax']],
             plot.list[['tmin']],
             plot.list[['srad']],
             plot.list[['vapr']],
             plot.list[['prec']],
             plot.list[['wind']], nrow = 2)
dev.off()
# ggsave(filename=paste0("plot/cluster_brightness",fig.ext),width = fig.width*4, height=fig.height*2, units=fig.unit)


######
# predictions
######

p <- read.csv("predictions.csv", stringsAsFactors = F)
cor <- cor.test(p$true, p$pred)
if(cor$estimate>0){
    text <- paste0('\n   cor: ', round(cor$estimate,4),"\n   p-value < 1e-5 ")
    hj = 0
    vj = 1
    xm = -Inf
    ym = Inf
}else{
    text <- paste0('\ncor: ', round(cor$estimate,4),"   \np-value < 1e-5 ")
    hj = 1
    vj = 1
    xm = Inf
    ym = Inf
}

ggplot(p, aes(x=true, y=pred))+
    geom_point(size=fig.size*0.5)+
    geom_smooth(method = "lm", se = FALSE, color='red')+
    ggtitle('Performance Evaluation')+
    labs(x=paste0('Actual ', getName('alt', type='cap_short',unit=T)),
         y=paste0('Predicted ', getName('alt', type='cap_short',unit=T)))+
    theme_bw()+
    theme(
        plot.title =element_text(size=fig.cex.main,hjust = 0.3, vjust=0.3),
        legend.title = element_text(size=fig.cex.lab),
        legend.text = element_text(size=fig.cex.lab),
        legend.position="none",
        axis.title = element_text(size=fig.cex.axis),
        axis.text = element_text(size=fig.cex.axis)
    )+
    annotate("text", xm, ym, label = text , color="black", hjust = hj, vjust = vj)
ggsave(filename=paste0("plot/predictions",fig.ext),width = fig.width, height=fig.height, units=fig.unit)


####
# color simulation
####

library(reshape2)
cl <- read.csv('color_exp_sample.csv', stringsAsFactors = F)
cl <- melt(cl, id.vars = c('High_or_Low','hue'),value.name = 'deviation')

cl$hue <- as.factor(cl$hue)
cl$High_or_Low <- factor(cl$High_or_Low, levels=c('Low group','High group'))
plot.list <- list()
for(h in c(0.0, 0.3, 0.6)){
    clplot = cl[cl$hue==h,]
    clplot$variable <- paste0(clplot$variable, " (h=",clplot$hue,")")
    clplot$variable <- factor(clplot$variable, levels=rev(sort(unique(clplot$variable))))
    pp <- ggplot(clplot, aes(x=High_or_Low, y=deviation, fill=High_or_Low))+
        stat_boxplot(geom = "errorbar", width = 0.1) +  
        geom_boxplot(width=0.1, coef=3)+
        facet_grid(~variable) +
        scale_fill_manual(values=c('#bdbdbd','#636363'))+
        labs(x='',y='', fill='')+
        coord_flip()+
        theme_bw()+
        theme(
            plot.title =element_text(size=fig.cex.main,hjust = 0.3, vjust=0.3),
            legend.title = element_text(size=fig.cex.lab),
            legend.text = element_text(size=fig.cex.lab),
            legend.position="none",
            axis.title = element_text(size=fig.cex.axis),
            axis.text = element_text(size=fig.cex.axis)
        )
    ggsave(filename=paste0("plot/color_simulation_h",as.character(h),fig.ext),width = fig.width*2.1, height=fig.height*0.7, units=fig.unit)
    plot.list[[as.character(h)]] <- pp
}

png(paste0("plot/color_simulation.png"),width = fig.width*1.5, height=fig.height*1.5, res=512, units=fig.unit)
grid.arrange(plot.list[['0']],
             plot.list[['0.3']],
             plot.list[['0.6']], nrow = 3)
dev.off()


di <- read.csv('cluster_color_pattern_diversity.csv', stringsAsFactors = F)
di$new_km_labels <- as.factor(di$new_km_labels)
ggplot(di, aes(x=new_km_labels, y=avg.diversity, fill=new_km_labels))+
    geom_bar(stat='identity')+
    scale_fill_manual(values=color15)+
    labs(x='t-SNE 1', y='t-SNE 2', color='group')+
    theme_bw()+
    theme(
        plot.title =element_text(size=fig.cex.main,hjust = 0.3, vjust=0.3),
        legend.title = element_text(size=fig.cex.lab),
        legend.text = element_text(size=fig.cex.lab),
        legend.position="none",
        axis.title = element_text(size=fig.cex.axis),
        axis.text = element_text(size=fig.cex.axis)
    )
    
