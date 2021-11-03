# sand box

# exclude all error sequences

TRY <- WM_dat[grabErrors == 0 & fineMotorError == 0 & locationError == 0 & featuresWM > 0]

features <- aggregate(data = TRY, featuresWM ~ angle + distractor + sub, mean)

a <- aggregate(data = dat, searchTime ~ distractor, mean)

ggplot(a, aes(x = distractor, y = searchTime, fill = distractor)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_cartesian(ylim=c(0, 1500)) +
  #scale_colour_manual(values = c("#CFCCCE","#9E9A9C","#564E52","#251B21"))+
  scale_fill_manual(values = c("#9E9A9C","#564E52","#251B21"))+
  #scale_colour_manual(values = c("#9E9A9C","#251B21"))+
  theme_set(theme_gray(base_size = 24)) + guides(colour=FALSE) + guides(fill=FALSE) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank(),
        legend.position= c(.2, .8),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"))# + facet_grid(~sub)



aggregate(data = WM_dat, fineMotorError ~ distractor + angle, mean)

aggregate(data = WM_dat, grabErrors ~ distractor + angle, mean)

aggregate(data = WM_dat, locationError ~ distractor + angle, mean)


dat <- SEARCH_dat[findObject == T & isTarget == T & searchTime > 0.01 & ItemsfixatedCutoff > 0 ]

aggregate(data = dat, timePerObject ~ distractor, mean)

aggregate(data = dat, timePerTargetFixation ~ distractor, mean)

aggregate(data = dat, timePerDistractorFixation ~ distractor, mean)

aggregate(data = dat, searchTime ~ distractor, mean)

aggregate(data = dat, nDistractorsUnique ~ distractor, mean)
aggregate(data = dat, nTargetsUnique ~ distractor, mean)
