#!/usr/bin/env Rscript
# %% ####################################################
rm(list = ls())
library(LalRUtils)

libreq(tidyverse, data.table, fst, fixest, rio, foreach, magrittr, glue, readxl,
  janitor, tictoc, patchwork, RPushbullet, IRdisplay, furrr)

theme_set(lal_plot_theme()) # add _d() for dark
options(repr.plot.width=12, repr.plot.height=9)
options(ggplot2.discrete.fill = RColorBrewer::brewer.pal(9, "Set1"))
options(ggplot2.discrete.colour = RColorBrewer::brewer.pal(9, "Set1"))
options(ggplot2.continuous.fill = "viridis"); options(ggplot2.continuous.colour = "viridis")
set.seed(42)
chr = function(...) as.character(...) %>% display_html()
plan(multisession, workers = 6)
# %% ####################################################
root = "/home/alal/Dropbox/_Data/Elections/Nepal_Electoral_Data"
elec_81 = file.path(root, "1981_and_1982_election")
elec_86 = file.path(root, "1986_and_1987_election")
elec_92 = file.path(root, "1992_Local_Election")
elec_97 = file.path(root, "1997_Local_Election")

# %%
cn = c("district", "district_code", "vdc", "vdc_code", "post", "ward",
  "last_name", "party", "age", "gender", "votes", "vote_percent", "elected",
  "n_votes_reg", "n_votes_cast")

# %%
# function to slice vdc file
vdc_result = function(path){
  df = readxl::read_excel(path) %>% setDT %>% remove_empty("rows")
  # first 2 cols are aggregates
  sumtab = df[1, 1:2]
  df = df[, 3:ncol(df)] %>% clean_names
  df[, eval(parse(text = glue("n_votes_reg  := {as.numeric(names(sumtab)[2])}")))]
  df[, eval(parse(text = glue("n_votes_cast := {as.numeric(sumtab[[2]])}")))]
  return(df)
}

# function to ingest all VDC results for a district, add voter count and cast
# columns, and stack
dist_stack = function(distpath, colNames = cn){
  # list all vdc files (avoid conflicted copy nonsense)
  vdc_files = list.files(distpath, full.names = TRUE, pattern = "VDC_[0-9][0-9].xlsx")
  if (length(vdc_files) == 0){
    cat(distpath, " no files \n")
    return(NULL)
  }
  else{
    # apply vdc reader to all village excel sheets
    vdcdfs = map(vdc_files, vdc_result)
    names(vdcdfs) = vdc_files
    # stack, keep file name
    vdc_stacked = rbindlist(vdcdfs, use.names = T, fill=TRUE, idcol = T)
    # some districts have extra rubbo columns - handle separately
    setnames(vdc_stacked, names(vdc_stacked)[1:16], c('filename', colNames))
    return(vdc_stacked)
  }
}

# %%
 #######   #######
##     ## ##     ##
##     ##        ##
 ########  #######
       ## ##
##     ## ##
 #######  #########

dirs = list.dirs(elec_92, full.names = TRUE, recursive = F) %>% str_subset("District.*") %>% sort
dirs %>% head %>% print
# %% populate list of tables with each district
tic()
dist_results = future_map(dirs, dist_stack)
toc()

# %% drop empty
dist_results = Filter(Negate(is.null), dist_results)

# %% count number of columns in each district table and fix manually
lapply(dist_results, dim)
# %% # fixes for minor discrepancies in some district datasets
dist_results[[6]][, x16 := NULL]
dist_results[[27]][is.na(vote_percent),`:=`(
  vote_percent = percent_voters,
  votes        = voters
)][, c("percent_voters", "voters") := NULL]
dist_results[[32]][, c('x16', 'x17', 'x0') := NULL]
dist_results[[32]][is.na(vote_percent), vote_percent := voters_percent][, voters_percent := NULL]
dist_results[[40]][is.na(vote_percent), vote_percent := voters_percent][, voters_percent := NULL]
dist_results[[43]][is.na(vote_percent), vote_percent := voters_percent][, voters_percent := NULL]
# duplicate names
dist_results[[71]] = clean_names(dist_results[[71]])
dist_results[[71]][is.na(vote_percent), vote_percent := vote_percent_2][, vote_percent_2 := NULL]
dist_results[[73]][is.na(vote_percent), vote_percent := percent_vote][, percent_vote := NULL]
# %% stack and final cleanups
vdc_92 = rbindlist(dist_results)
# district name check
vdc_92$district %>% unique %>% sort %>% print
# fix duplicate
vdc_92[district == "Solukhmbu", district := "Solukhumbu"]

# %%
vdc_92 = vdc_92[!is.na(post)]
# %% # post names cleanup
vdc_92[post == "Chariman",     post := "Chairman"]
vdc_92[post == "Members",      post := "Member"]
vdc_92[post == "Deputi Mayor", post := "Vice Chairman"]
vdc_92[post == "Mayor",        post := "Chairman"]
vdc_92[grepl("[vV]ice.*[Cc]h.*", post )] %>% tabyl(post)
vdc_92[grepl("[vV]ice.*[Cc]h.*", post ), post := "Vice Chairman"]
vdc_92 %>% tabyl(post)
# %% write
fwrite(vdc_92, file.path(root, "Electoral_Clean/VDC_1992_all.csv"))
# %%
 #######  ########
##     ## ##    ##
##     ##     ##
 ########    ##
       ##   ##
##     ##   ##
 #######    ##

# %%
dirs = list.dirs(elec_97, full.names = TRUE, recursive = F) %>% str_subset("District.*") %>% sort
dirs %>% head %>% print

# %%
tic()
dist_results = future_map(dirs, dist_stack)
toc()

# %%
dist_results = Filter(Negate(is.null), dist_results)
# %% minor fixes
dist_results[[19]][!is.na(x79), vdc := x79][, x79 := NULL]
# %% stack
vdc_97 = rbindlist(dist_results)
# district name check
vdc_97$district %>% unique %>% sort %>% print
# fix duplicate
vdc_97[district == "kabhreplanchok", district := "Kabhreplanchok"]
vdc_97[district %in% c("Sankhhuwasabha", "Sakhuwasabha"), district := "Sankhuwasabha"]
vdc_97[district %in% c("Kabilbastu", "Kabilpastu"), district := "Kapilbastu"]
vdc_97[district == "22", district := district_code]
# clean up district codes
vdc_97 = vdc_97[!is.na(district_code)]
vdc_97[district_code == "Dolakha", district_code := "22"]
vdc_97[, district_code := as.numeric(district_code)]
# %% check empty post values
vdc_97$post %>% tabyl
vdc_97[is.na(post), unique(filename)] %>% print

# vdc_97[is.na(post) & filename ==
# "/home/alal/Dropbox/_Data/Elections/Nepal_Electoral_Data/1997_Local_Election/District
# 57, 1997/VDC_32.xlsx"]

# %%
vdc_97 = vdc_97[!is.na(post)]
# %% # post names cleanup
vdc_97[post == "Mayor",        post := "Chairman"]
vdc_97[post == "Mayoyr",        post := "Chairman"]
vdc_97[post == "Members",      post := "Member"]
vdc_97[post == "Deputy Mayor", post := "Vice Chairman"]
vdc_97[grepl("[vV]ice.*[Cc]h.*", post )] %>% tabyl(post)
vdc_97[grepl("[vV]ice.*[Cc]h.*", post ), post := "Vice Chairman"]
# %% write
fwrite(vdc_97, file.path(root, "Electoral_Clean/VDC_1997_all.csv"))

# %%

 #######     ##
##     ##  ####
##     ##    ##
 #######     ##
##     ##    ##
##     ##    ##
 #######   ######

# %%
dirs = list.dirs(elec_81, full.names = TRUE, recursive = F) %>% str_subset("District.*") %>% sort
dirs %>% head %>% print

# %%
tic()
dist_results = future_map(dirs, dist_stack)
toc()
