load.libraries <- c('ggplot2','dplyr','grid','ggpubr','car','moments','treemap','psych','plm')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)


#reading data
guns <- read.csv("guns.csv")

nrow(guns)
colnames(guns)
summary(guns)

##data treatment: converting state_id to character
guns$stateid <- as.character(guns$stateid)
guns$shall <- as.character(guns$shall)
#states are not identified by consecutive numbers
length(unique(guns$stateid))==max(guns$stateid)

##Hypothesis: every year should have all 51 states
guns %>% group_by(as.character(year)) %>% summarise(state=n_distinct(stateid)) %>% filter(state<51)
#data is balanced pool with n=51 states, t= 23 and N=1173

#number of states in which shall law has been introduced in the observed time period (77 to 99)
states_with_shall <- guns %>% filter(shall==1) 
length(unique(states_with_shall$stateid))
#only in 29 states out of 51 states shall law has been introduced

#when shall law started in each state
shall_start<-guns %>% filter(shall==1) %>% group_by(stateid) %>% summarise(minyear=min(year)) %>% arrange(as.numeric(minyear))
h<-hist(shall_start$minyear, breaks = seq(77,99,1))
#17 out of 29 states implemented the shall law after 1990
#Therefore this data may not be sufficient for understanding the delay effects of shall law on crime rate in states which introduced shall law in the later part of the observation period

#writing code for testing different hypothesis in further part of the analysis
guns2<- guns %>% 
  left_join(shall_start) %>% 
  mutate(shall_yesorno = ifelse(is.na(minyear),"no","yes"), 
         year_bin=ifelse(minyear<=80,"77-80",ifelse(minyear<=85,"81-85",ifelse(minyear<=90,"86-90",ifelse(minyear<=95,"91-95","96-99")))))

guns3<- guns2 %>% 
  group_by(year) %>% 
  summarise(avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))


guns4<- guns2 %>% 
  group_by(year,shall_yesorno) %>% 
  summarise(avg.mur.s = round(mean(mur),2), avg.rob.s = round(mean(rob),2), avg.vio.s = round(mean(vio),2)) 

guns5<- guns2 %>% filter(shall_yesorno=="yes") %>%
  group_by(year,shall) %>% 
  summarise(number_of_sates=n(),avg.mur.s_yes = round(mean(mur),2), avg.rob.s_yes = round(mean(rob),2), avg.vio.s_yes = round(mean(vio),2)) 

guns6<-dcast(setDT(guns4), year ~ shall_yesorno,  value.var=c("avg.mur.s","avg.rob.s","avg.vio.s")) 

guns7<-dcast(setDT(guns5), year ~ shall,  value.var=c("avg.mur.s_yes","avg.rob.s_yes","avg.vio.s_yes","number_of_sates"))

temp_7780 <- guns2 %>% filter(year_bin=="77-80") %>%
  group_by(year) %>% 
  summarise(n_7780=n(),avg.mur.s_7780 = round(mean(mur),2), avg.rob.s_7780 = round(mean(rob),2), avg.vio.s_7780 = round(mean(vio),2)) 

temp_8185 <- guns2 %>% filter(year_bin=="81-85") %>%
  group_by(year) %>% 
  summarise(n_8185=n(),avg.mur.s_8185 = round(mean(mur),2), avg.rob.s_8185 = round(mean(rob),2), avg.vio.s_8185 = round(mean(vio),2)) 
temp_8690 <- guns2 %>% filter(year_bin=="86-90") %>%
  group_by(year) %>% 
  summarise(n_8690=n(),avg.mur.s_8690 = round(mean(mur),2), avg.rob.s_8690 = round(mean(rob),2), avg.vio.s_8690 = round(mean(vio),2)) 
temp_9195 <- guns2 %>% filter(year_bin=="91-95") %>%
  group_by(year) %>% 
  summarise(n_9195=n(),avg.mur.s_9195 = round(mean(mur),2), avg.rob.s_9195 = round(mean(rob),2), avg.vio.s_9195 = round(mean(vio),2)) 
temp_9699 <- guns2 %>% filter(year_bin=="96-99") %>%
  group_by(year) %>% 
  summarise(n_9699=n(),avg.mur.s_9699 = round(mean(mur),2), avg.rob.s_9699 = round(mean(rob),2), avg.vio.s_9699 = round(mean(vio),2)) 


guns8 <- guns3 %>%
  left_join(guns6) %>%
  left_join(guns7) %>%
  left_join(temp_7780) %>%
  left_join(temp_8185) %>%
  left_join(temp_8690) %>%
  left_join(temp_9195) %>%
  left_join(temp_9699)

guns8[is.na(guns8)] <- 0


ggplot(guns8 , aes(x=year)) + geom_line(aes(y = avg.vio), size=2, col="blue") + labs(title = "Avg. vio rate across different years")

state <- guns %>% group_by(stateid) %>% summarise(avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))

treemap(state, 
        index=c("stateid"),  
        vSize = "avg.vio", 
        type="index" ,
        title="Avg. vio rate across different state", #Customize your title
        fontsize.title = 14 #Change the font size of the title
)


ggplot(guns8 , aes(x=year)) +
  geom_line(aes(y = avg.vio , colour ="overall avg. vio_rate"), size=2) +
  geom_line(aes(y = avg.vio.s_yes, colour ="shall states avg. vio_rate"), size=2) +
  geom_line(aes(y = avg.vio.s_no, colour ="no shall states avg. vio_rate"), size=2) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()) + scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")+
  labs(title = "Avg. vio rate for overall and shall/no-shall states", y="rate")

ggplot(guns8 , aes(x=year)) +
  geom_line(aes(y = avg.vio.s_yes, colour ="shall states ovareall avg. vio_rate"), size=2) +
  geom_line(aes(y = avg.vio.s_yes_0, colour ="shall states avg. vio_rate before shall intro"), size=2) +
  geom_line(aes(y = avg.vio.s_yes_1, colour ="shall states avg. vio_rate after shall intro"), size=2) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()) +
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")+
  labs(title = "Analyzing the before and after avg. vio rate for states where shall law has been introduced", y="rate")

ggplot(guns8 , aes(x=year)) +
  geom_line(aes(y = avg.vio.s_7780 , colour ="states that introduced shall law in 77-80"), size=2) +
  geom_line(aes(y = avg.vio.s_8185 , colour ="states that introduced shall law in 81-85"), size=2) +
  geom_line(aes(y = avg.vio.s_8690 , colour ="states that introduced shall law in 86-90"), size=2) +
  geom_line(aes(y = avg.vio.s_9195 , colour ="states that introduced shall law in 91-95"), size=2) +
  geom_line(aes(y = avg.vio.s_9699 , colour ="states that introduced shall law in 96-99"), size=2) +
  geom_vline(xintercept = 80, alpha=0.5, linetype="dashed") +
  geom_vline(xintercept = 85, alpha=0.5, linetype="dashed") +
  geom_vline(xintercept = 90, alpha=0.5, linetype="dashed") +
  geom_vline(xintercept = 95, alpha=0.5, linetype="dashed") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()) + scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")+
  labs(title = "Avg. vio rate for states that started shall law at different points in time", y="rate") 

state_7780<-unique((guns2 %>% filter(year_bin=="77-80"))$stateid)
state_8185<-unique((guns2 %>% filter(year_bin=="81-85"))$stateid)
state_8690<-unique((guns2 %>% filter(year_bin=="86-90"))$stateid)
state_9195<-unique((guns2 %>% filter(year_bin=="91-95"))$stateid)
state_9699<-unique((guns2 %>% filter(year_bin=="96-99"))$stateid)

state<-state_7780
t7780_y80 <- guns2 %>% filter(year==80) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
t7780_y85 <- guns2 %>% filter(year==85) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
t7780_y90 <- guns2 %>% filter(year==90) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
t7780_y95 <- guns2 %>% filter(year==95) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
t7780_y99 <- guns2 %>% filter(year==99) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))

state<-state_8185
t8185_y85 <- guns2 %>% filter(year==85) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
t8185_y90 <- guns2 %>% filter(year==90) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
t8185_y95 <- guns2 %>% filter(year==95) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
t8185_y99 <- guns2 %>% filter(year==99) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))

state<-state_8690
t8690_y90 <- guns2 %>% filter(year==90) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
t8690_y95 <- guns2 %>% filter(year==95) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
t8690_y99 <- guns2 %>% filter(year==99) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))

state<-state_9195
t9195_y95 <- guns2 %>% filter(year==95) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
t9195_y99 <- guns2 %>% filter(year==99) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))

state<-state_9699
t9699_y99 <- guns2 %>% filter(year==99) %>% filter(stateid %in% state) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))

not_y80 <- guns2 %>% filter(year==80) %>% filter(! stateid %in% state_7780) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
not_y85 <- guns2 %>% filter(year==85) %>% filter(! stateid %in% c(state_7780,state_8185)) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
not_y90 <- guns2 %>% filter(year==90) %>% filter(! stateid %in% c(state_7780,state_8185,state_8690)) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
not_y95 <- guns2 %>% filter(year==95) %>% filter(! stateid %in% c(state_7780,state_8185,state_8690,state_9195)) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))
not_y99 <- guns2 %>% filter(year==99) %>% filter(! stateid %in% c(state_7780,state_8185,state_8690,state_9195,state_9699)) %>% group_by(shall) %>% summarise(n=n(), avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))


y80 <- cbind(year="80",rbind(cbind(shall_year="7780",t7780_y80),cbind(shall_year="no",not_y80)))
y85 <- cbind(year="85",rbind(cbind(shall_year="7780",t7780_y85),cbind(shall_year="8185",t8185_y85),cbind(shall_year="no",not_y85)))
y90 <- cbind(year="90",rbind(cbind(shall_year="7780",t7780_y90),cbind(shall_year="8185",t8185_y90),cbind(shall_year="8690",t8690_y90),cbind(shall_year="no",not_y90)))
y95 <- cbind(year="95",rbind(cbind(shall_year="7780",t7780_y95),cbind(shall_year="8185",t8185_y95),cbind(shall_year="8690",t8690_y95),cbind(shall_year="9195",t9195_y95),cbind(shall_year="no",not_y95)))
y99 <- cbind(year="99",rbind(cbind(shall_year="7780",t7780_y99),cbind(shall_year="8185",t8185_y99),cbind(shall_year="8690",t8690_y99),cbind(shall_year="9195",t9195_y99),cbind(shall_year="9699",t9699_y99),cbind(shall_year="no",not_y99)))


all <- rbind(y80,y85,y90,y95,y99)

ggplot(all, aes(x=year)) +
  geom_point(aes(y = n), size=2) +
  facet_grid(~shall_year) +
  labs(title = "Number of states that started shall in different years") + theme(legend.position="none")

ggplot(all, aes(x=year)) +
  geom_point(aes(y = avg.vio, colour ="avg. vio_rate"), size=2) +
  facet_grid(~shall_year) +
  labs(title = "Avg. violent rate for states that started shall in different years") + theme(legend.position="none")


