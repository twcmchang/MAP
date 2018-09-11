library(dplyr)
library(ggplot2)

# color15 = c('black','#4d4d4d','green','#ffff99','#b15928',
#             '#a6cee3','#1f78b4','#fb9a99','#e31a1c',
#             '#b2df8a','#33a02c','#cab2d6','#6a3d9a','#fdbf6f','#ff7f00')
# color15 = rev(color15)
# color15 = c(topo.colors(7), terrain.colors(8))

color15 = topo.colors(15)
fig.width = 6
fig.height = 6
fig.dpi = 100
fig.unit = 'in'
fig.ext = '.eps'
fig.cex.main = 20
fig.cex.lab = 12
fig.cex.axis = 14
fig.size = 2

d = read.csv('species_regression_v2.csv')
d$v.ratio = d$v.body_fore/d$v.whole
name <- read.csv('name.csv', stringsAsFactors = F)

for(key in c('ratio', 'whole','body_fore')){
    v = lm(data=d, formula=paste0('v.', key, '~srad'))
    
    v.residual = v$residuals
    
    t = lm(data=d, formula='tavg~srad')
    t.residual = t$residuals
    
    tmp = data.frame('y'=v.residual,
                     'x'=t.residual)
    
    m = lm(data=tmp, formula='y~x')
    r_squared = round(summary(m)$r.squared,4)
    cor = cor.test(tmp$x, tmp$y)$estimate %>% round(4)
    
    png(paste0('partial_srad_vs_',key, fig.ext), width = fig.width, height = fig.height, units = fig.unit)
    plot(t.residual, v.residual, pch=19, size=fig.size,las=2,
         cex.main = fig.cex.main, cex.lab = fig.cex.lab, cex.axis= fig.cex.axis,
         main=paste0(key, ' vs. relative value\nR-squared = ',r_squared,', cor = ', cor),
         col=color15[d$km_label+1])
    lines(lowess(t.residual, v.residual), col='red')
    legend('topleft', legend = seq(0,14), ncol=5, pch=19, col=)
    dev.off()
}

for(key in c('ratio', 'whole','body_fore')){
    v = lm(data=d, formula=paste0('v.', key, '~tavg'))
    
    v.residual = v$residuals
    
    s = lm(data=d, formula='srad~tavg')
    s.residual = s$residuals
    
    tmp = data.frame('y'=v.residual,
                     'x'=s.residual)
    
    m = lm(data=tmp, formula='y~x')
    r_squared = round(summary(m)$r.squared,4)
    cor = cor.test(tmp$x, tmp$y)$estimate %>% round(4)
    
    png(paste0('partial_tavg_vs_',key,fig.ext), width = fig.width, height = fig.height, units = fig.unit)
    plot(s.residual, v.residual, pch=19, size=fig.size,
         cex.main = fig.cex.main, cex.lab = fig.cex.lab, cex.axis= fig.cex.axis, 
         main=paste0(key, ' vs. relative value\nR-squared = ',r_squared,', cor = ', cor), 
         col=color15[d$km_label+1])
    lines(lowess(s.residual, v.residual), col='red')
    axis(2, at=seq(-0.4, 0.3, 0.1), labels=seq(0.7, 1.2, 0.1), las=2, cex.axis=fig.cex.axis)
    legend('topleft', legend = seq(0,14), ncol=5, pch=19, col=color15)
    dev.off()
}


my_group = F
for(key in c('tavg', 'srad', 'alt')){
    
    m = lm(data=d, formula=paste0('v.ratio~', key))
    r_squared = round(summary(m)$r.squared,4)
    cor = cor.test(d[[key]], d[['v.ratio']])$estimate %>% round(4)
    
    png(paste0(key, '_vs_ratio_individual',fig.ext), width = fig.width, height = fig.height, units = fig.unit)
    plot(d[[key]], d[['v.ratio']], col=color15[d$km_label+1], pch=19, size=fig.size,las=1,yaxt='n',
         cex.main = fig.cex.main, cex.lab = fig.cex.lab, cex.axis= fig.cex.axis,
         main=paste0(key, ' vs. relative value\nR-squared = ',r_squared,', cor = ', cor),
         xlab=key,
         ylab='v.ratio')
    axis(2, at=seq(0.7, 1.2, 0.1), labels=seq(0.7, 1.2, 0.1), las=2, cex.axis=fig.cex.axis)
    lines(lowess(d[[key]], d[['v.ratio']]), col='red', lwd=2)
    legend('topleft', legend = seq(0,14), ncol=5, pch=19, col=color15)
    dev.off()
}


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

center <- read.csv('km_centers.csv')

ggplot(f, aes(x=tsne.0, y=tsne.1, color=alt)) +
    geom_point(size=1.5)+
    #scale_color_continuous(low='#fdae61',breaks=seq(250,2500,250),high = '#4393c3')+
    #scale_color_gradientn(colours=c('#fdae61','#a6d96a','#4393c3'))+
    #scale_color_gradientn(colours=c('#fdb462','#b3de69','#80b1d3'))+
    scale_color_gradientn(colours=c('#df4d0c','#f0904f','#86c682', '#69b85a','#006184','#12498d'))+
    ggtitle('t-SNE Visualization')+
    labs(x='t-SNE 1', y='t-SNE 2')+
    theme_bw()+
    theme(
        plot.title =element_text(size=fig.cex.main,hjust = 0.5),
        legend.title = element_text(size=fig.cex.lab),
        legend.text = element_text(size=fig.cex.lab),
        axis.title = element_text(size=fig.cex.axis),
        axis.text = element_text(size=fig.cex.axis)
    )

ggsave(filename=paste0("plot/t-SNE_by_elevation",fig.ext),width = fig.width, height=fig.height, units=fig.unit)

ggplot(f, aes(x=tsne.0, y=tsne.1, color=factor(new_km_labels))) +
    geom_point(size=2, alpha=0.8)+
    scale_color_manual(values=color15)+
    ggtitle('t-SNE Visualization')+
    labs(x='t-SNE 1', y='t-SNE 2')+
    theme_bw()+
    theme(
        plot.title =element_text(size=fig.cex.main,hjust = 0.5),
        legend.title = element_text(size=fig.cex.lab),
        legend.text = element_text(size=fig.cex.lab),
        axis.title = element_text(size=fig.cex.axis),
        axis.text = element_text(size=fig.cex.axis)
    )

ggsave(filename=paste0("plot/t-SNE_by_cluster",fig.ext),width = fig.width, height=fig.height, units=fig.unit)

