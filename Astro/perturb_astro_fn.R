# Perturbing astro data function

################################################
# Front Matter
################################################

library(data.table)
library(dplyr)
library(tidyselect)

################################################
# Useful functions
################################################

# create colors from magnitude columns
create.colors <- function(dat){ # takes full clean set with adjusted mags
  dr12.recode <- dat %>%
    mutate(u_g = u_adj - g_adj,
           g_r = g_adj - r_adj,
           r_i = r_adj - i_adj,
           i_z = i_adj - z_adj)
  
  return(dr12.recode) # returns colors only
}

create.perturbed.dat <- function(dat){
  # prep column names
  mag.names <- c("u_adj", "g_adj", "r_adj", "i_adj", "z_adj")
  err.names <- c("psfMagErr_u", "psfMagErr_g", "psfMagErr_r", "psfMagErr_i", "psfMagErr_z")
  
  # pull only necessary columns from trains and test sets
  mags.df <- dat %>% select(all_of(mag.names))
  errors.df <- dat %>% select(all_of(err.names))

  pert <- mapply(FUN = function(mag, error, i, j){ mag + rnorm(1, 0, error) },
         mags.df,
         errors.df) %>% as.data.frame

  # create colors
  pert <- create.colors(pert) # add color columns
  
  # attach class column
  pert$class <- dat$class %>% as.factor
  
  return(pert)
}

