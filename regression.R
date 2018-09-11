library(dplyr)

color15 = c('black','#4d4d4d','green','#ffff99','#b15928',
             '#a6cee3','#1f78b4','#fb9a99','#e31a1c',
             '#b2df8a','#33a02c','#cab2d6','#6a3d9a','#fdbf6f','#ff7f00')
color15 = rev(color15)

d = read.csv('updated_species_regression_v2.csv')
d$v.ratio = d$v.body_fore/d$v.whole

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
       main=paste0(key, ' vs. relative value\nR-squared = ',r_squared,', cor = ', cor),
       col=topo.colors(15)[d$km_label+1])
  lines(lowess(t.residual, v.residual), col='red')
  legend('topleft', legend = seq(0,14), ncol=5, pch=19, col=topo.colors(15))
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
       main=paste0(key, ' vs. relative value\nR-squared = ',r_squared,', cor = ', cor), 
       col=topo.colors(15)[d$km_label+1])
  lines(lowess(s.residual, v.residual), col='red')
  legend('topleft', legend = seq(0,14), ncol=5, pch=19, col=topo.colors(15))
  dev.off()
}


my_group = F
for(key in c('tavg', 'srad', 'alt')){
  
  m = lm(data=d, formula=paste0('v.ratio~', key))
  r_squared = round(summary(m)$r.squared,4)
  cor = cor.test(d[[key]], d[['v.ratio']])$estimate %>% round(4)
  
  png(paste0(key, '_vs_ratio_individual.png'))
  plot(d[[key]], d[['v.ratio']], col=topo.colors(15)[d$km_label+1], pch=19, 
       main=paste0(key, ' vs. relative value\nR-squared = ',r_squared,', cor = ', cor),
       xlab=key,
       ylab='v.ratio')
  lines(lowess(d[[key]], d[['v.ratio']]), col='red', lwd=2)
  legend('topleft', legend = seq(0,14), ncol=5, pch=19, col=topo.colors(15))
  dev.off()
}




# lm(data=data.frame('v.residual'=v.residual,'t.residual'=t.residual),
#    formula='v.residual~t.residual')
