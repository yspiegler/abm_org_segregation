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
