library(TraMineR)
library(TraMineRextras)

## Here we can create an event sequence object, but we are more interested in state sequences ---------
bg_vet_job <- bg_vet_job[order(bg_vet_job$id), ]

bg_vet_job_seqe <- seqecreate(id = bg_vet_job$id, 
                              timestamp = bg_vet_job$startdate,
                              event = bg_vet_job$onet_job_zone)

plot(head(bg_vet_job_seqe, 10), title = "Index plot (first 10 sequences)",
         withlegend = TRUE)


## Here we are looking at going from TSE to STE object using example data -------
data(actcal.tse)
data(actcal)

actcal.seqe <- seqecreate(id = actcal.tse$id, timestamp = actcal.tse$time,
                          event = actcal.tse$event)

## For us to go TSE to STS with our data we need an event transition matix, I'm confused by that so I kind of abandoned this -------
events <- c(1, 2, 3, 4, 5)
dm <- matrix(FALSE, 3,3, dimnames=list(events, events))
stmList <- seqe2stm(events, dropList=list())

# States defined by last occurred event (forgetting all previous events).
stm <- seqe2stm(events, dropList=list("1"=events[-1],
                                      "2"=events[-2], "3"=events[-3],
                                      "4"=events[-4], "5"=events[-5]))

mysts <- head(bg_vet_job_change_short, 50) %>% 
  select("id", "job_position", "onet_job_zone") %>%
  as.matrix() %>%
  TSE_to_STS(id=1,
             timestamp=2,
             event=3,
             stm=NULL, 
             tmin=1, 
             tmax=9, 
             firstState="None")

## Here is the example dataset and code for going SPELL to STS ---------
data(bfspell)
# Not aligned
bf.sts.y <- seqformat(bfspell20, from = "SPELL", to = "STS",
                      id = "id", begin = "begin", end = "end", status = "states",
                      process = FALSE)
head(bf.sts.y)
# Aligned
bf.sts.a <- seqformat(bfspell20, from = "SPELL", to = "STS",
                      id = "id", begin = "begin", end = "end", status = "states",
                      process = TRUE, pdata = bfpdata20, pvar = c("id", "when15"),
                      limit = 16)
names(bf.sts.a) <- paste0("a", 15:30)
head(bf.sts.a)

## Here I am trying to go SPELL to STS with our dataset, I used BG job change short (commenting out lines to select variables and spread) because I was originally working with the corrected job_position variable, but you could probably use a different dataset with the id, startdate, enddate, and job zone as well
testing <- head(bg_vet_job, 1000) 
testing <- testing %>% 
  mutate(startyear = year(startdate), endyear = year(enddate)) %>%
  select("id", "startyear", "endyear", "onet_job_zone")
testing_first <- testing %>% 
  select("id", "startyear") %>% group_by(id) %>% transmute(enter = min(startyear)) %>% distinct() %>% ungroup()
b <- as.matrix(testing_first)
a <- as.data.frame(b)
id

sts.test <- seqformat(testing, from = "SPELL", to = "STS",
                      id = "id",  begin = "startyear", end = "endyear", 
                      status = "onet_job_zone",  process = TRUE,
                      pdata = a, pvar = c("id", "enter"))

seqstatl(sts.test)
length(seqstatl(sts.test[, -1]))
comp.seq <- seqdef(sts.test[,-1])
print(comp.seq[1:3, ])
seqiplot(comp.seq,border = NA, with.legend = "right", cpal = c("#E57200", "#62BB46", "#FDDA24", "#009FDF", "#EF3F6B"), xlim =c(0,30))

# In TraMineR users guide "converting from the spell format" starts on pg. 42
# Documentation on seqformat function http://traminer.unige.ch/doc/seqformat.html

??seqdef

