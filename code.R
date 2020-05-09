library(readxl)
library(tidyverse)
df <-read_xlsx("tabula-2020.05.06.20092999v1.full.xlsx", col_names=c("rf", "unadjust", "adjust")) %>%
  select(-adjust) %>%
  separate(unadjust, c("hr", "ci"), sep = " ") %>%
  separate(ci, c("lci", "uci"), sep = "-") %>%
  mutate(lci=str_remove(lci, "[(]"), uci=str_remove(uci, "[)]")) %>%
  mutate(lci=str_remove(lci, "ref")) %>%
  mutate(lci=str_remove(lci, "[)]")) %>%
  mutate(hr=as.numeric(hr), lci=as.numeric(lci), uci=as.numeric(uci))
 
  
  
ggplot(slice(df,17:19), aes(x=hr, y=rf)) +
  geom_point() +
  geom_linerange(aes(xmin=lci, xmax=uci)) +
  theme(axis.text.y=element_text(size=10)) +
  geom_vline(xintercept = 1, linetype="dashed") +
  labs(title = "Smoking's association with COVID-19 hospital deaths, UK", subtitle="Source -https://www.medrxiv.org/content/10.1101/2020.05.06.20092999v1  ", 
       caption = "Age and sex adjusted") +
  ylab(" ") +
  xlab("Hazard Ratio")
ggsave("smoke.png")  
