set.seed(7)
nrs <- 1000
priors <- c(.3,.6,.65,.85,.95)
n <- 20
alphas <- n * priors
betas <- n - alphas
thetas <- mapply(rbeta, n=n, shape1=alphas, shape2=betas)
thetao <- as.vector(thetas)
ci <- apply(thetas, 2, function(x) quantile(x, probs = c(0.025, 0.50, 0.975)))
cio <- quantile(thetao, probs = c(0.025, 0.50, 0.975))
theta_density <- apply(thetas, 2, density)
overall_density <- density(thetao)
pdf("subgrouppriors.pdf", width=8, height=8)
plot(seq(0,1,.1), type='n',
     xlim=c(0,1),
     xaxs='i',
     yaxs='i',
     yaxt='n',
     main = '1000 random samples from subgroup priors',
     xlab = expression(theta), ylab = "", 
     ylim = c(0, max(sapply(theta_density, function(d) max(d$y)))))

for (i in 1:length(theta_density)) {
  polygon(theta_density[[i]], 
          col = rgb(1-i/length(theta_density), 0, i/length(theta_density), alpha = 0.75))
}
polygon(overall_density, col = rgb(1, 1, 1, alpha = 0.5), lwd = 1.5)

legend_entries <- sapply(1:length(alphas), function(i) {
  alpha_str <- sprintf("%2d", alphas[i])
  beta_str <- sprintf("%2d", betas[i])
  ci_range <- sprintf("[%.3f, %.3f, %.3f]", ci[1, i], ci[2,i], ci[3, i])
  m <- sprintf("%.2f",alphas[i] / (alphas[i] + betas[i]))
  bquote("n = 20." ~ theta[.(i)] ~ " ~ Beta" (alpha == .(alpha_str),
                                  beta == .(beta_str)) * ". E(" * theta[.(i)] * ") = " ~ .(m) * "." ~ .(ci_range) * ".")
})

oci <- sprintf("[%.3f, %.3f, %.3f]", cio[1], cio[2], cio[3])

legend("topleft", legend = c(legend_entries, bquote("N = 100. [0.25%, 50%, 97.5%] quantiles for " ~ theta * ":"~ .(oci) * ".")), 
       fill = c(sapply(1:length(theta_density), function(i) rgb(1-i / length(theta_density), 0,i / length(theta_density), alpha = 0.75)), rgb(1, 1, 1, alpha = 0.5)),
       bty = "n", text.font = 2, cex=.8)
dev.off()
