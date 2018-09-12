library(dplyr)

# color15 = c('black','#4d4d4d','green','#ffff99','#b15928',
#              '#a6cee3','#1f78b4','#fb9a99','#e31a1c',
#              '#b2df8a','#33a02c','#cab2d6','#6a3d9a','#fdbf6f','#ff7f00')
# color15 = rev(color15)

color15 = rev(c('#df0c21','#e44227','#e86030','#ec793e','#f0904f',
                '#f0904f','#d8a248','#bdb150','#a1bd65','#86c682',
                '#69b85a','#00a481','#008aa0','#006ca6','#12498d'))

d = read.csv('updated_species_regression_v2.csv')
d$v.ratio = d$v.body_fore/d$v.whole
d$s.ratio = d$s.body_fore/d$s.whole

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
  
  png(paste0('partial_srad_vs_',key,'.png'), width = 1024, height = 1024, units = 'px')
  plot(t.residual, v.residual, pch=19, size=0.1,
       main=paste0(key, ' relative value (residual) vs. srad (residual)\nR-squared = ',r_squared,', cor = ', cor),
       col=color15[d$km_label+1])
  lines(lowess(t.residual, v.residual), col='red')
  legend('topleft', legend = seq(0,14), ncol=5, pch=19, col=color15)
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
  
  png(paste0('partial_tavg_vs_',key,'.png'), width = 1024, height = 1024, units = 'px')
  plot(s.residual, v.residual, pch=19, size=0.1, 
       main=paste0(key, ' relative value (residual) vs. tavg (residual)\nR-squared = ',r_squared,', cor = ', cor), 
       col=color15[d$km_label+1])
  lines(lowess(s.residual, v.residual), col='red')
  legend('topleft', legend = seq(0,14), ncol=5, pch=19, col=color15)
  dev.off()
}


my_group = F
par(mfrow=c(2,4))
for(key in c('alt','tavg','tmax','tmin','srad','vapr','prec','wind')){
  
  m = lm(data=d, formula=paste0('v.ratio~', key))
  r_squared = round(summary(m)$r.squared,4)
  cor = cor.test(d[[key]], d[['v.ratio']])$estimate %>% round(4)
  pval = cor.test(d[[key]], d[['s.ratio']])$p.value %>% round(10)
  
  #png(paste0(key, '_vs_ratio_value_individual.png'))
  plot(d[[key]], d[['v.ratio']], col=color15[d$km_label+1], pch=19, 
       main=paste0(key, ' vs. relative value\nR-squared = ',r_squared,', cor = ', cor,'\np-value = ', pval),
       xlab=key,
       ylab='v.ratio')
  lines(lowess(d[[key]], d[['v.ratio']]), col='black', lwd=2)
  #legend('topleft', legend = seq(0,14), ncol=5, pch=19, col=color15)
  #dev.off()
}

# png(paste0('climate_vs_relative_saturation.png'), width=1800, height=1024, dpi=100, units='px')
par(mfrow=c(2,4))
for(key in c('alt','tavg','tmax','tmin','srad','vapr','prec','wind')){
    m = lm(data=d, formula=paste0('s.ratio~', key))
    r_squared = round(summary(m)$r.squared,4)
    cor = cor.test(d[[key]], d[['s.ratio']])$estimate %>% round(4)
    pval = cor.test(d[[key]], d[['s.ratio']])$p.value %>% round(10)
    
    #png(paste0(key, '_vs_ratio_saturation_individual.png'))
    plot(d[[key]], d[['s.ratio']], col=color15[d$km_label+1], pch=19, 
         main=paste0(key, ' vs. relative saturation\nR-squared = ',r_squared,', cor = ', cor,'\np-value = ', pval),
         xlab=key,
         ylab='s.ratio')
    lines(lowess(d[[key]], d[['s.ratio']]), col='black', lwd=2)
    #legend('topleft', legend = seq(0,14), ncol=5, pch=19, col=color15)
    #dev.off()
}
# dev.off()

library("PerformanceAnalytics")
my_data <- d[,c('tavg','vapr','alt','prec','srad','wind')]
png('climate_correlation.png', width =2048, height = 2048, res=256,units = 'px')
chart.Correlation(my_data, histogram=TRUE, pch=19)
dev.off()
