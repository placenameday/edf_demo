
ggplot(adt, aes(x=ztjl, fill=sex)) + geom_density(alpha=.3) + theme(text = element_text(family='Kai'))

g <- ggplot(adt, aes(statu))
g + geom_bar() + theme(text = element_text(family='Kai'))