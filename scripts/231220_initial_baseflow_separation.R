
## quick script to separate baseflow and quickflow in NWM q estimates

require(pacman)
p_load(tidyverse,
       grwat)

nwm_path <- "/Users/regi350/Downloads/nwm_flow 2"

x <- list.files(nwm_path, full.names = T) %>% 
  map(read_csv) %>% 
  bind_rows

read_ks_file <- function(file){
  
  message(paste("reading", file))
  
  comid = stringr::str_extract_all(file, "\\d{8}")
  
  data <- read_csv(file) %>% 
    rename("q_cms" = cms) %>% 
    mutate(comid = first(comid)) %>% 
    mutate(q_base = gr_baseflow(q_cms, method = 'jakeman'))
  
  return(data)
}


df <- list.files(nwm_path, full.names = T) %>% 
  map(read_ks_file) %>% 
  bind_rows()

ggplot(df %>% filter(comid == 24125789), aes(date_utc, q_cms)) + 
  geom_line()
 
ggplot(df, aes(x = date_utc)) + 
  geom_area(aes(date_utc, q_cms), fill = 'steelblue', alpha = 0.5) +
  geom_area(aes(date_utc, q_base), fill = 'orangered', alpha = 0.5) + 
  facet_wrap(~comid, scales = "free")
#ggsave("figures")


hdata <- x %>% 
  group_by(comid) %>% 
  mutate(q_base = gr_baseflow(cms, method = 'jakeman'))

ggplot(hdata) + 
  geom_line(aes(date_utc, cms))


ggplot(hdata) +
  geom_area(aes(date_utc, cms), fill = 'steelblue', color = 'black') +
  geom_area(aes(date_utc, q_base), fill = 'orangered', color = 'black')


