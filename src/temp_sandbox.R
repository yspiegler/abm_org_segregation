library(tidyverse)

out_nopref <- read_rds("out/100org_output61_nopref.rds")
out_61     <- read_rds("out/100org_output61.rds")
out_50     <- read_rds("out/100org_output50.rds")
out_40     <- read_rds("out/100org_output40.rds")


plot_batch_dissimilarity(out_nopref, "61/39. [no pref. models]")
plot_batch_dissimilarity(out_61, "61/39.")
plot_batch_dissimilarity(out_40, "40/60.")
plot_batch_dissimilarity(out_50, "50/50.")

plot_batch_theil(out_nopref, "61/39. [no pref. models]")
plot_batch_theil(out_61, "61/39.")
plot_batch_theil(out_40, "40/60.")
plot_batch_theil(out_50, "50/50.")

ggsave("plots/plot_test.png", plot = p, width = 10, height = 6, dpi = 300)




plot_batch_theil(out_nopref, "61/39", FALSE, FALSE)
plot_batch_theil(out_61, "61/39", TRUE, FALSE)
plot_batch_theil(out_50, "50/50", TRUE, FALSE)
plot_batch_theil(out_61, "40/60", TRUE, FALSE)

plot_batch_theil(out_nopref, "61/39", FALSE, TRUE)
plot_batch_theil(out_61, "61/39", TRUE, TRUE)
plot_batch_theil(out_50, "50/50", TRUE, TRUE)
plot_batch_theil(out_61, "40/60", TRUE, TRUE)



# combine plots ---------------------------------------------------------------------------------------------------

p_non <- plot_batch_theil(out_nopref, "61/39", FALSE, T)
p_61 <- plot_batch_theil(out_61, "61/39", TRUE, F)
p_50 <- plot_batch_theil(out_50, "50/50", TRUE, F)

p_non + p_50$layers

a <- tibble(mean = double(length = 100), 
            ci_low = double(length = 100), 
            ci_high = double(length = 100), 
            iteration = 1:100)

for (i in 2:100) {
  test_values <- map_dbl(out_61, ~ .x[i, 4])
  ci <- Hmisc::smean.cl.normal(test_values)
  a[i, 1] <- ci[[1]]
  a[i, 2] <- ci[[2]]
  a[i, 3] <- ci[[3]]
}

a[1,1] <- 0.0658
a[1,2] <- 0.064
a[1,3] <- 0.0676

b <- tibble(mean = double(length = 100), 
            ci_low = double(length = 100), 
            ci_high = double(length = 100), 
            iteration = 1:100)

for (i in 1:100) {
  test_values <- map_dbl(out_nopref, ~ .x[i, 4])
  ci <- Hmisc::smean.cl.normal(test_values)
  b[i, 1] <- ci[[1]]
  b[i, 2] <- ci[[2]]
  b[i, 3] <- ci[[3]]
}

b[1,1] <- 0.0651
b[1,2] <- 0.0632
b[1,3] <- 0.0669

p_61

ggplot(a, aes(x = iteration, y = mean)) + geom_line() + 
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "green") + 
  geom_line(data = b, aes(x = iteration, y = mean)) + 
  geom_ribbon(data = b, aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "red") + 
  theme_minimal()
# should random choice decrease segregation? ----------------------------------------------------------------------



