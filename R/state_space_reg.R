library(RTMB)

dat_stock <- df %>% filter(stock == "2J3KL")
cv        <- 0.2
sd_log    <- sqrt(log(1 + cv^2))
rng       <- as.numeric(quantile(dat_stock$biomass, c(0.15, 0.85)))

dat <- list(logB_obs = log(dat_stock$biomass),
            y        = dat_stock$prod.rate,
            sd_log   = sd_log,
            delta    = 0.05 * diff(range(dat_stock$biomass)),
            qlo      = rng[1],
            qhi      = rng[2])

m0 <- lm(prod.rate ~ biomass, data = dat_stock)
summary(m0)
sm <- segmented(m0, seg.Z = ~ biomass)
summary(sm)

coef(sm)
sm$psi


b1_0  <- as.numeric(coef(sm)[2])
psi_0 <- as.numeric(sm$psi[2])
u_0   <- min(max((psi_0 - dat$qlo) / (dat$qhi - dat$qlo), 0.02), 0.98)  # keep off the boundary

par <- list(a         = as.numeric(coef(sm)[1]),
            b1        = b1_0,
            b2        = b1_0 + as.numeric(coef(sm)[3]),
            logit_psi = qlogis(u_0),
            log_sigma = log(sd(residuals(sm))),
            logB      = log(dat_stock$biomass))


nll <- function(p) {
  getAll(p, dat)
  B     <- exp(logB)
  psi   <- qlo + (qhi - qlo) * plogis(logit_psi)
  hinge <- delta * logspace_add(0 * B, (B - psi) / delta)
  mu    <- a + b1 * B + (b2 - b1) * hinge
  -sum(dnorm(logB_obs, logB, sd_log, log = TRUE)) -
    sum(dnorm(y, mu, exp(log_sigma), log = TRUE))
}

obj <- MakeADFun(nll, par, random = "logB",
                 map = list(log_sigma = factor(NA)),   # fix sigma; identifies the EIV model
                 silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr, control = list(iter.max = 1000, eval.max = 1000))
sdr <- summary(sdreport(obj, par.fixed = opt$par), "fixed")

opt$convergence
max(abs(obj$gr(opt$par)))
sdr
