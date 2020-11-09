

library(mobr)


library(mobr)
data(tank_comm)

View(tank_comm)
data(tank_plot_attr)
View(tank_plot_attr)

tank_mob_in = make_mob_in(tank_comm, tank_plot_attr)



par(mfrow=c(1,1))
plot_rarefaction(tank_mob_in, 'group', 'spat')

par(mfrow=c(1,2))
plot_abu(tank_mob_in, 'group', 'rad')
plot_abu(tank_mob_in, 'group', 'sad', leg_loc = NA)


# dan plants


data(inv_comm)      # Community matrix
data(inv_plot_attr) # Plot attributes data.frame
View(inv_comm)
head(inv_plot_attr)

View(inv_plot_attr)

inv_mob_in <- make_mob_in(inv_comm, inv_plot_attr)
inv_mob_in


plot_rarefaction(inv_mob_in, 'group', 'spat', lwd=4)

par(mfrow=c(1,1))
plot_rarefaction(inv_mob_in, 'group', 'indiv', pooled=F, lwd=2,
                 leg_loc='topright')
plot_rarefaction(inv_mob_in, 'group', 'indiv', pooled=T, lwd=4,
                 leg_loc=NA)

