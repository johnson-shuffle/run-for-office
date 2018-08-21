# ----- Preample ----------------------------------------------------------

options(java.parameters = "-Xmx8000m")

library(xlsx)
library(stringdist)


# ----- Preliminaries -----------------------------------------------------

# counties with codes
pag1 <- 'https://www.sos.wa.gov/elections/candidates/offices-open-for-election.aspx'
pag2 <- 'https://weiapplets.sos.wa.gov/elections/'

# read html, find correct nodes, filter based on href
htm <- read_html(pag1)
nds <- html_nodes(htm, 'a')
cds <- map_chr(nds, ~xml_attrs(.x)[[1]])
hrf_lgl <- str_detect(cds, str_c('^', pag2, 'officesOpenForElection?'))

# create tibble
cou <- tibble(
  county = xml_text(nds)[hrf_lgl][-1],
  code = str_extract(cds[hrf_lgl][-1], '\\=[A-z]{2}')
  )
cou$county %<>% str_replace('\\s', '')
cou$code %<>% str_replace('\\=', '')

# add king and yakima
cou %<>% add_row(county = c('King', 'Yakima'), code = c('KI', 'YA'))

# arrange
cou %<>% arrange(county)


# ----- Results -----------------------------------------------------------

# elections and extensions 
ele <- tribble(
  ~date, ~Var3,
  '20171107', '.csv',
  '20161108', '.csv',
  '20151103', '.csv',
  '20141104', '.csv',
  '20131105', '_20131210_0314.csv',
  '20121106', '_20121205_1451.csv',
  '20111108', '.csv',
  '20101102', '.csv',
  '20091103', '.csv',
  '20081104', '.csv'
  )

# searches
shs <- expand.grid(cou$county, ele$date, stringsAsFactors = F)
shs <- left_join(shs, ele, by = c('Var2' = 'date'))

# helper function
cou_fun <- function(county, date, extension, pb = NULL) {
  
  # progress
  if (!is.null(pb)) { pb$tick()$print() }
  
  # column types
  cty <- cols(
    Race = col_character(),
    Candidate = col_character(),
    Party = col_character(),
    Votes = col_integer(),
    PercentageOfTotalVotes = col_double(),
    JurisdictionName = col_character()
    )
  
  # csv download
  pag <- 'http://results.vote.wa.gov/results/'
  add <- str_c(pag, date, '/export/', date, '_', county, extension)
  csv <- read_csv(add, col_types = cty)
  
  csv$Race %<>% str_replace('(\\()(\\d+)(\\))', '\\2')
  
  csv$County <- county
  
  csv$Year <- str_sub(date, 1, 4) %>% as.numeric()
  
  return(csv)
  
}

# make safe
cou_fun <- safely(cou_fun)

# map the helper function
pb <- progress_estimated(nrow(shs))
x <- list(county = shs$Var1, date = shs$Var2, extension = shs$Var3)
wa_result <- pmap(x, cou_fun, pb = pb)
save(wa_result, file = 'out/wa_results_2012_2018.Rda')


# ----- Offices -----------------------------------------------------------

# searches
shs <- expand.grid(cou$code, 2008:2018)

# helper function
off_fun <- function(county, year, pb = NULL) {
  
  # progress
  if (!is.null(pb)) { pb$tick()$print() }
  
  # get table
  x <- GET(
    'https://weiapplets.sos.wa.gov/elections/OfficesOpenForElection/Index',
    query = list(
      electionyear = year,
      countycode = county
      )
    )
  x %<>% content(as = 'text')
  x %<>% read_html()
  x %<>% html_table()
  x <- x[[1]]
  names(x)[c(1:2)] <- c('Prefix', 'Office')
  
  # tidy up
  flag <- x$Prefix == ''
  x$Region <- x$Office
  x$Region[flag] <- ''
  for (i in 2:nrow(x)) {
    if (x$Prefix[i] == '') {
      x$Prefix[i] <- x$Prefix[i - 1]
      x$Region[i] <- x$Region[i - 1]
    }
  }
  dat <- x[flag, ]
  
  dat$Term <- str_extract(dat$Office, '\\(\\d-year.*\\)')
  dat$Term <- str_replace_all(dat$Term, '\\(|\\)', '')
  
  dat$Office %<>% str_replace('(\\()(\\d+)(\\))', '\\2')
  dat$Office %<>% str_replace('\\(.*\\)', '')
  dat$Office %<>% str_trim()
  
  dat$County <- cou$county[cou$code == county]
  
  dat$Year <- year
  
  return(dat)
  
}

# make safe
off_fun <- safely(off_fun)

# map
pb <- progress_estimated(nrow(shs))
wa_office <- map2(shs$Var1, shs$Var2, off_fun, pb = pb)
save(wa_office, file = 'out/wa_offices_2012_2018.Rda')


# ----- Tidy up results----------------------------------------------------

# load
load('out/wa_results_2012_2018.Rda')

# map result
res <- do.call(plyr::rbind.fill, map(wa_result, ~.x$result))

# remove non-offices from election results
res$Candidate %<>% str_replace_all('([A-z])(\\()([A-z])', '\\1 \\(\\3')
res$Candidate %<>% str_replace_all('([A-z])(\\))([A-z])', '\\1\\) \\3')
drops <- res$Candidate[!str_detect(res$Candidate, '\\s')] %>% unique()
drops <- drops[!drops %in% c('Raven', 'WRITE-IN')]
res %<>% filter(!Candidate %in% drops)

# lower case, remove double spaces
res$Race %<>% tolower()
res$Race %<>% str_replace_all('\\s{2}', ' ')

# collapse races
res %<>%
  group_by(Race, Candidate, Party, JurisdictionName, Year) %>%
  summarise(
    Votes = sum(Votes),
    Region = str_c(County, collapse = ', ') 
  ) %>%
  group_by(Race, Year) %>%
  mutate(
    PercentageOfTotalVotes = Votes / sum(Votes)
  )
res$County <- ''
res %<>%
  within({
    County[!str_detect(Region, ',')] <- Region[!str_detect(Region, ',')]
  })


# ----- Tidy up offices----------------------------------------------------

# load
load('out/wa_offices_2012_2018.Rda')

# map result
off <- do.call(rbind, map(wa_office, ~.x$result))

# lower case, remove double spaces, numbers wrapped in ()
off$Prefix %<>% tolower()
off$Prefix %<>% str_replace_all('\\s{2}', ' ')
off$Race <- str_c(off$Prefix, off$Office, sep = ' ') %>% tolower()
off$Race %<>% str_replace_all('\\s{2}', ' ')
off$Race %<>% str_replace_all('(\\()(\\d{0,2})(\\))', '\\2')

# keep one set of statewide offices
stw <- filter(off, `File With` == 'State') %>%
  select(-County) %>%
  distinct()
stw$County <- ''  
off <- rbind(filter(off, `File With` != 'State'), stw)

# expired terms
expire <- off$Year + as.numeric(str_sub(off$Term, 1, 1))
expire <- expire < 2018

# unexpired offices
unx <- off[!expire, ]

# compare active vs expired offices: 264 active offices w/ no match
table(unx$Office %in% off[expire, ]$Office)

# unmatched active offices
unm_off <- unx[!unx$Office %in% off[expire, ]$Office, ]


# ----- Compare -----------------------------------------------------------

# inital check
ck1 <- unx$Race %in% res$Race
ck2 <- unx$Incumbent %in% res$Candidate
table(ck1)
table(ck2)
table(ck1, ck2) # 740 active offices with no match

# unmatched active offices
unm_res <- unx[!(ck1 | ck2), ]

# compare unmatched offices
table(unm_res$Office %in% unm_off$Office)

# try string distance
unm_res$Match <- NA
unm_res$Distance <- NA
for (i in 1:nrow(unm_res)) {
  
  # filter by county
  l1 <- res$Year == unm_res$Year[i]
  l2 <- res$County %in% unm_res$County[i] | res$Region %in% unm_res$County[i]
  tmp <- res[l1 & l2, ]
  
  # skip if empty
  if (nrow(tmp) == 0) { next }
  
  # use string distance
  dis <- stringdist(unm_res$Race[i], tmp$Race, method = 'jw')
  unm_res$Match[i] <- tmp$Race[which.min(dis)]
  unm_res$Distance[i] <- dis[which.min(dis)]
  
}

# sort by distance
unm_res %<>% arrange(Distance)

# save
write.xlsx(unm_res, 'out/wa_unmatched.xlsx', row.names = F)
