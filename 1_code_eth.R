# %% ####################################################
rm(list = ls())
library(LalRUtils)
LalRUtils::libreq(tidyverse, data.table, fst, collapse, fixest, rio, foreach, magrittr, haven, collapse,
                  janitor, tictoc, modelsummary, hablar, patchwork, RPushbullet, IRdisplay, stringdist)
theme_set(lal_plot_theme()) # add _d() for dark
options(repr.plot.width=12, repr.plot.height=9)
options(ggplot2.discrete.fill = RColorBrewer::brewer.pal(9, "Set1"))
options(ggplot2.discrete.colour = RColorBrewer::brewer.pal(9, "Set1"))
options(ggplot2.continuous.fill = "viridis"); options(ggplot2.continuous.colour = "viridis")
set.seed(42)
chr = function(...) as.character(...) %>% display_html()
# %% ####################################################
root = "/home/alal/Dropbox/_Data/Elections/Nepal_Electoral_Data"
int = file.path(root, "Electoral_Clean")
tmp = file.path(root, "tmp")
# %%
mappings = read_dta(file.path(root, "local_election_results/1992 Local Election (clean)_caste.dta")) %>%
  setDT %>% remove_empty("rows") %>% haven::as_factor()
mappings[LastName  == "", unique(last_name)] # empty
mappings[last_name == "", .(last_name, LastName)] %>% dim # nonempty
# %%
mappings = mappings[LastName != ""]
mappings[, n := .N, LastName]
setorder(mappings, -n)
setcolorder(mappings, 'n')
keepCols = colnames(mappings) %>% str_subset("(^n|LastName|prob*|caste*|vollan*)")
ethCols = colnames(mappings) %>% str_subset("(caste*|vollan*)")
mappings[, (ethCols) := lapply(.SD, as.character), .SDcols = ethCols]
collapsed_mappings = mappings[,
  lapply(.SD, function(x) x %>% .[!is.na(.)] %>% unique() %>% .[1]),
  by = LastName,
  .SDcols = c(ethCols, 'n', str_subset(colnames(mappings), "^prob"))]

# %% clean up names
collapsed_mappings[, (ethCols) := lapply(.SD, function(x) str_replace_all(x, "(\\(included\\)|\\(excluded\\))", "")), .SDcols = ethCols]
# %% sort by share
collapsed_mappings[, share := n/sum(n)]
collapsed_mappings[, cumulShare := cumsum(share)]
setcolorder(collapsed_mappings, c("LastName", "n", "share", "cumulShare"))
# %%
grouper_table = map_dfr(collapsed_mappings$LastName, ~ {
    i <- which(stringdist(., collapsed_mappings$LastName, "jw") < 0.1)
    tibble(index = i, title = collapsed_mappings$LastName[i])
  }, .id = "group") %>%
    distinct(index, .keep_all = T) %>%
    mutate(group = as.integer(group))

# %%

# %%
df_all = merge(collapsed_mappings, grouper_table[, c("group", "title")], by.x = 'LastName', by.y = 'title')
# %%
setcolorder(df_all, c("LastName", "group", "n", "share"))
setorder(df_all, group, -share)
df_all[, groupName  := LastName[1], group]
df_all[, groupShare := sum(share), groupName]
df_all[, casteFinal := ""]
setcolorder(df_all, c("LastName", "groupName", "casteFinal", "n", "groupShare", "share", "cumulShare"))
df_all
# %% send to google sheets
df_all %>% fwrite(file.path(tmp, "nep_eth_coding.csv"))

# %%
