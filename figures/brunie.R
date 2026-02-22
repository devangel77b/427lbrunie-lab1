library(ggplot2)
library(dplyr)

raw <- read.csv('position.csv',header=TRUE)
data <- tibble(raw)


# make figure 1
ypred <- function(t){
  -0.5*9.81*t^2+5
}
fig1 <- ggplot(data) +
    geom_point(aes(x=t_s,y=y_m,shape=type))+
    geom_function(fun=ypred,color="blue")+
    xlab('$t$, \\unit{\\second}')+
    ylab('$y$, \\unit{\\meter}')+
    theme_bw(base_size=8)+
    theme(legend.position="inside",
	legend.position.inside=c(0.95,0.95),
	legend.justification.inside=c("right","top"),
	legend.title=element_blank())
ggsave('fig1.svg',plot=fig1,width=3.4167,height=2,units='in')



# make figure 2
vdata <- tibble(read.csv('velocity.csv',header=TRUE))
vpred <- function(t){
  -9.81*t
}
fig2 <- ggplot(vdata) +
    geom_point(aes(x=t_s,y=v_ms,shape=type))+
    geom_function(fun=vpred,color="blue")+
    xlab('$t$, \\unit{\\second}')+
    ylab('$v_y$, \\unit{\\meter\\per\\second}')+
    theme_bw(base_size=8)+
    theme(legend.position="inside",
	legend.position.inside=c(0.95,0.95),
	legend.justification.inside=c("right","top"),
	legend.title=element_blank())
ggsave('fig2.svg',plot=fig2,width=3.4167,height=2,units='in')


# estimate g from velocity data
model1 <- lm(v_ms~t_s+0,vdata)
model2 <- lm(v_ms~t_s,vdata)
print(anova(model1,model2))
print(model2)
print(summary(model2))


# ttest on times
tdata <- tibble(read.csv('times.csv',header=TRUE))
print(t.test(t_s~type,tdata))


    