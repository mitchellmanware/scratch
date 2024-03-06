# explore creating torch tensor with sample covariate data
# March 06, 2024

# import covariate data
hms <- readRDS("data/hms.RDS")
pop <- readRDS("data/pop.RDS")

# sample
s <- sample(hms$site_id, size = 10, replace = FALSE)
h <- hms[which(hms$site_id %in% s), ]
p <- pop[which(pop$site_id %in% s), ]

# same site locations
all(unique(h$site_id) == unique(p$site_id))
summary(h)
summary(p)




