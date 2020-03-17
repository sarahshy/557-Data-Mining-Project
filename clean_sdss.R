# this function cleans SDSS data, creates the colors, and returns the full clean set
clean.sdss <- function(dat){ # takes raw data
  # Clean data, remove -9999 magnitudes
  
  dr12.clean <- dat
  dr12.clean[dr12.clean == -9999] <- NA
  dr12.clean <- dr12.clean %>% drop_na()
  dr12.clean$class <- as.factor(dr12.clean$class)

  return(dr12.clean) # returns data w/o NA
}

# adjust magnitudes for extinction
adj.mags <- function(dat){ # takes full clean set
  # adjust for extinction (dr12.adj)
  dr12.adj <- dat
  
  dr12.adj$u_adj <- (dat$psfMag_u - dat$extinction_u)
  dr12.adj$g_adj <- (dat$psfMag_g - dat$extinction_g)
  dr12.adj$r_adj <- (dat$psfMag_r - dat$extinction_r)
  dr12.adj$i_adj <- (dat$psfMag_i - dat$extinction_i)
  dr12.adj$z_adj <- (dat$psfMag_z - dat$extinction_z)
  
  return(dr12.adj) # returns full clean data with adjusted mags
}

# create colors (ratios)
create.colors <- function(dat){ # takes full clean set with adjusted mags
  dr12.recode <- dat
  dr12.recode$u_g <- dat$u_adj - dat$g_adj
  dr12.recode$g_r <- dat$g_adj - dat$r_adj
  dr12.recode$r_i <- dat$r_adj - dat$i_adj
  dr12.recode$i_z <- dat$i_adj - dat$z_adj
  
  return(dr12.recode) # returns colors only
}


# this function splits and returns train and test sets cut on the i-band
create.train.test <- function(dat, quant = 0.1){ # takes full clean data with adjusted mags and colors
  # Create 10/90 train/test sets
  # cut on 0.1 quantile of i-band
  
  quant.cut <- dat$i_adj %>% quantile(quant)
  sdss.id <- which(dat$i_adj < quant.cut)
  train <- dat %>% filter(dat$i_adj < quant.cut)
  test <- dat %>% filter(dat$i_adj >= quant.cut)

  return(list(train, test))
}

# run all functions together
deep.clean <- function(dat){ # takes raw data
  clean <- clean.sdss(dat)
  adj <- adj.mags(clean)
  col <- create.colors(adj)
  sets <- create.train.test(col)
  
  return(sets)
}
















