library(rstan);

data <- readRDS("gp-compare_sim-data.rds");

# do not use Nystr\"om approximation
data$M <- data$J;

# use Nystr\"om approximation
#data$M <- floor(data$J * 0.2);

options(mc.cores=2);
fit <- stan(
	file="gp-compare.stan",
	data=data, iter=500, chains=4
);

f.mcmc <- extract(fit, "f")[[1]];
f.mcmc.mean <- apply(f.mcmc, 2, mean);
f.mcmc.lower <- apply(f.mcmc, 2, function(z) quantile(z, 0.025));
f.mcmc.upper <- apply(f.mcmc, 2, function(z) quantile(z, 0.975));

with(data, {
	plot(NA, xlim=c(0, 12), ylim=c(-2, 3));
	points(x, f);
	points(x, f.mcmc.mean, col="grey60");
	lines(x, f.mcmc.lower, col="grey", type="l");
	lines(x, f.mcmc.upper, col="grey", type="l");
});

cor(data$f, f.mcmc.mean)
with(data, mean(f < f.mcmc.upper & f > f.mcmc.lower))

