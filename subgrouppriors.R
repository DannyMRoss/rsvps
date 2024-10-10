set.seed(7)
rsn <- 1000
alphas <- c(5,10,15,17,19) + 1
betas <- (n + 2) - alphas
thetas <- mapply(rbeta, n=rsn, shape1=alphas, shape2=betas)
thetao <- as.vector(thetas)
ci <- apply(thetas, 2, function(x) quantile(x, probs = c(0.025, 0.50, 0.975)))
cio <- quantile(thetao, probs = c(0.025, 0.50, 0.975))
theta_density <- apply(thetas, 2, density)
overall_density <- density(thetao)
pdf("subgrouppriors.pdf", width=8, height=8)
par(mfrow = c(2,1)) 
par(mar = c(2,1,2,1))
plot(seq(0,1,.1), type='n',
     xlim=c(0,1),
     xaxs='i',
     yaxs='i',
     yaxt='n',
     main = '1000 random samples from subgroup priors',
     xlab = expression(theta), ylab = "", 
     ylim = c(0, max(sapply(theta_density, function(d) max(d$y)*1.01))))

for (i in 1:length(theta_density)) {
  polygon(theta_density[[i]], 
          col = rgb(1-i/length(theta_density), 0, i/length(theta_density), alpha = 0.75))
}
polygon(overall_density, col = rgb(1, 1, 1, alpha = 0.5), lwd = 1.5)

legend_entries <- sapply(1:length(alphas), function(i) {
  alpha_str <- sprintf("%2d", alphas[i])
  beta_str <- sprintf("%2d", betas[i])
  ci_range <- sprintf("[%.2f, %.2f, %.2f]", ci[1, i], ci[2,i], ci[3, i])
  m <- sprintf("%.2f",alphas[i] / (alphas[i] + betas[i]))
  bquote("n = 20." ~ theta[.(i)] ~ " ~ Beta" (alpha == .(alpha_str),
                                  beta == .(beta_str)) * ". E(" * theta[.(i)] * ") = " ~ .(m) * "." ~ .(ci_range) * ".")
})

oci <- sprintf("[%.2f, %.2f, %.2f]", cio[1], cio[2], cio[3])

legend("topleft", legend = c(legend_entries, bquote("N = 100. [0.25%, 50%, 97.5%] quantiles for " ~ theta * ":"~ .(oci) * ".")), 
       fill = c(sapply(1:length(theta_density), function(i) rgb(1-i / length(theta_density), 0,i / length(theta_density), alpha = 0.75)), rgb(1/5, 0, 1-1/5, alpha=.25)),
       bty = "n", text.font = 2, cex=.8)

ocdf <- environment(ecdf(thetao))
x <- ocdf$x
y <- ocdf$y
par(mar = c(4,1,1,1))
plot(x,
     type='n',
     xlim=c(0,1),
     xaxs='i',
     yaxs='i',
     yaxt='n',
     main = '',
     xlab = expression(theta), ylab='',
     ylim = c(0,1))


polygon(x=c(x,rev(x)), y=c(y, rep(0,length(y))),
        col = rgb(1/5, 0, 1-1/5, alpha=.25),
        lwd=1.5)
legend("topleft", legend=c(expression("Cumulative density of" ~ theta)), 
       fill=c(rgb(1/5, 0, 1-1/5, alpha=.25)), bty='n')

for (j in c(0.025,seq(0.1, 0.9, by = 0.1),.975)) {
  i <- which.min(y<=j)
  yp <- y[[i]]
  xp <- x[[i]]
  points(x=xp, y=yp, col = "black", pch = 19,cex=.5)
  text(x=xp, y=yp, labels = sprintf("%.2f", xp), pos = 4, col = "black", cex=.5, 
       srt=ifelse(j==0.025,0,-90))
  text(x=xp, y=yp, labels = paste0(j * 100, "%"), pos = 2, col = "black",cex=.5)
}
dev.off()
