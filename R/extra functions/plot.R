


dat <- data.frame(matrix(ncol=4, nrow = 1000))
names(dat) <- c("age", "pof","k", "c")
dat$age <- 1:100

dat$k[1:100] <- rep(0.0658,100)
dat$k[101:200] <- rep(0.0658,100)
dat$k[201:300] <- rep(0.0658,100)
dat$k[301:400] <- rep(0.0658,100)
dat$k[401:500] <- rep(0.0658,100)

dat$k[501:600] <- rep(0.0658,100)
dat$k[601:700] <- rep(0.0658,100)
dat$k[701:800] <- rep(0.0658,100)
dat$k[801:900] <- rep(0.0658,100)
dat$k[901:1000] <- rep(0.0658,100)


dat$c[1:500] <- rep(1.087,100)
dat$c[501:1000] <- rep(2,100)


i = 1
m = 1
n = 2

for(n in 1:2) {

  cval <- unique(dat$c)[n]

  for(m in 1:5) {

    kval <- unique(dat$k)[m]

    for(i in 1:100) {


      res <- pof_cables_04kv_pex(
        utilisation_pct = "Default",
        operating_voltage_pct = "Default",
        sheath_test = "Default",
        partial_discharge = "Default",
        fault_hist = "Default",
        reliability_factor = "Default",
        age = i,
        k_value = kval,
        c_value = cval,
        normal_expected_life = 80)




      dat$pof[which(dat$c %in% cval & dat$k %in% kval & dat$age == i)] <-  res

    }
  }
}

library(ggplot2)

dat$k <- as.character(dat$k)
dat$c <- as.character(dat$c)
#
# ggplot(dat, aes(x=age, y=pof, color=k)) +
#   geom_line()



ggplot(dat, aes(x=age, y=pof, colour=k, shape = c,
              group=interaction(k, c))) +
  geom_point() + geom_line() + ggtitle("PoF for 0.4kV cable, normal expected life = 80" ) # for the main title



