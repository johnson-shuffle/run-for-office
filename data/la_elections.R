load_tidy()

options(java.parameters = "-Xmx8000m") ;  library(xlsx)


# ----- preliminaries -----------------------------------------------------

# filename
fn <- 'la_elections_14'

# multi-parish workbooks
mul <- c('Multi-Parish(Parish)', 'Multi-Parish(Precinct)')

# read in data: list of parishes
par <- read.xlsx(
  str_c('raw/', fn, '.xlsx'),
  sheetName = mul[1],
  startRow = 7,
  endRow = 72,
  colIndex = 1
  )
par <- par[, 1]


# ----- multi-parish (parish) ---------------------------------------------

# read in data
xls <- read.xlsx(
  str_c('raw/', fn, '.xlsx'),
  sheetName = mul[1],
  startRow = 6,
  header = F
  )

# total votes
tvr <- grep('Total Votes', xls$X1)

# elections
ele <- xls[tvr - 2, 1]

# start/end
str <- tvr + 1
end <- tvr - 3
end <- c(end[-1], nrow(xls))

# loop
x <- list()
for (i in seq_along(str)) {
  
  # construct the data frame
  tmp <- xls[str[i]:end[i], ]
  names(tmp) <- c('Parish', t(xls[str[i] - 2, ])[, 1][-1])
  tmp <- tmp[names(tmp)[!is.na(names(tmp))]]
  
  # add election type
  tmp <- cbind(rep(ele[i], nrow(tmp)), tmp)
  names(tmp)[1] <- 'Election'
  
  # reshape
  tmp <- gather(tmp, Option, Votes, -1:-2)
  tmp <- spread(tmp, Parish, Votes)
  
  # store in list
  x[[i]] <- tmp ; rm(tmp)
  
}

# bind together
x <- do.call(plyr::rbind.fill, x)


# ----- parish workbooks --------------------------------------------------

# helper function
par_fun <- function(wb) {
  
  # read in data
  xls <- read.xlsx(
    str_c('raw/', fn, '.xlsx'),
    sheetName = wb,
    startRow = 6,
    header = F
    )

  # total votes
  tvr <- grep('Total Votes', xls$X1)
  
  # elections
  ele <- xls[tvr - 2, 1]
  
  # loop
  dat <- list()
  for (i in seq_along(tvr)) {
    
    # construct the data frame
    tmp <- xls[tvr[i], ]
    tmp[1, 1] <- wb
    names(tmp) <- c('Parish', t(xls[tvr[i] - 1, ])[, 1][-1])
    tmp <- tmp[names(tmp)[!is.na(names(tmp))]]
    
    # add election type
    tmp <- cbind(rep(ele[i], nrow(tmp)), tmp)
    names(tmp)[1] <- 'Election'
    
    # reshape
    tmp <- gather(tmp, Option, Votes, -1:-2)
    tmp <- spread(tmp, Parish, Votes)
    
    # store in list
    dat[[i]] <- tmp ; rm(tmp)
    
  }
  
  # bind together
  dat <- do.call(rbind, dat)
  
  return(dat)
  
}

# make safe
par_fun <- safely(par_fun)

# map the helper function
y <- map(par, par_fun)

# map the result
z <- map(y, ~.x$result)
z <- do.call(plyr::rbind.fill, z)


# ----- final bind --------------------------------------------------------

out <- plyr::rbind.fill(x, z)

write.xlsx(out, str_c('out/', fn, '_clean.xlsx'), row.names = F)
