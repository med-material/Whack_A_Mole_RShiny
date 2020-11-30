library(dplyr)
vis_moleTable <- function(df) {
  moleEvents = c("Mole Spawned", "Fake Mole Spawned", "Pointer Shoot", "Mole Hit", "Mole Missed","Fake Mole Hit")
  table = data.frame(moleEvents, rep(NA,length(moleEvents)))
  names(table) = c("Event", "Count")
  stats = df %>%
    group_by(Event) %>% 
    dplyr::summarise(Count = n()) %>%
    filter(Event %in% moleEvents) %>%
    mutate(Event = factor(Event, levels = moleEvents),
            ) %>%
    arrange(Event)
  table <- table %>%
    left_join(stats, by = "Event") %>%
    mutate(Count = as.integer(Count.y),
           Count.x = NULL,
           Count.y = NULL)
  return(table)
}
