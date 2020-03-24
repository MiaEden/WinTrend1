#getwd()
#"C:/Users/edenm/OneDrive - HP Inc/Documents"
setwd("C:/Users/edenm/winTrendGrpMngRHTMLVisual")


#source('./r_files/flatten_HTML.r')


# Input load. Please do not change #
#`dataset` = read.csv('C:/Users/edenm/REditorWrapper_c18ebe2a-b066-45b6-8b9c-dcd6948a7104/input_df_f661807d-d342-445c-8e8b-4d78a4c68af9.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #

# dataset <- data.frame(VW_People.Manager2, VW_People.Manager1, VW_People.EmployeeName, Opportunity Customer Organization Name, 
#       Opportunity Identifier, Series Description, CloseMonthNb, Opportunity Close Fiscal Year Quarter Code, m_ExtendedUSD_R, 
#       m_PRED_LostExtUSD_R, m_PRED_WinExtUSD_R, OppCloseStatus, WinOrLoss, CustomerFlag.GroupWinRate)
# dataset <- unique(dataset)


# Paste or type your script code here:

# Libraries
libraryRequireInstall("ggplot2")
libraryRequireInstall("plotly")
libraryRequireInstall("dplyr")
libraryRequireInstall("tidyr")
options("scipen" = 99, "digits"=0)

# power bi has old version of dplyr, mutate is not working properly
#df <- dataset %>%
#  select(month = CloseMonthNb, cgroup = CustomerFlag.GroupWinRate, win = m_PRED_WinExtUSD_R, loss = m_PRED_LostExtUSD_R) %>%
#  mutate(month = as.factor(month)) %>%
#  gather(key = "variable", value = "value", -c(month, cgroup)) 

df <- dataset
df <- df[,c("CloseMonthNb", "CustomerFlag.GroupWinRate", "VW_People.Manager2", "m_PRED_WinExtUSD_R", "m_PRED_LostExtUSD_R")]
names(df) <- c("month", "cgroup", "director", "win", "loss")
df$month <- as.factor(df$month)

df <- gather(df, key = "variable", value = "value", -c(month, cgroup,director)) 
levels(df$month) <- c("11" ,"12","1",  "2",  "3",  "4", "5", "6", "7","8", "9", "10")
df <- df[order(df$month),]

monthLabel <- unique(df$month)
monthLabel <- month.abb[monthLabel]

p <- #ggplot(df, aes(x = month, y = value, color = variable, group = variable)) + # use color for borders if wanted
  ggplot(df, aes(x = month, y = value, group = variable)) +
  #geom_line(size = 1, alpha = 0.5) +
  #geom_point() +
  geom_bar(stat = "identity",aes(fill= variable)) + 
  #geom_line(aes(color = variable), size = 1, alpha = 0.5) +
  #geom_point(aes(color = variable)) +
  scale_fill_manual(values = c("#CD6076","#0096D6")) +  # red (loss), blue (win)
  facet_grid(cgroup ~ director, scales = 'free',labeller = label_wrap_gen(width = 15, multi_line = TRUE)) + # free scale means each chart adjust its axis, labeller for wrapping labels)
  #facet_wrap(cgroup ~ director, scales = 'free') + 
  #ggtitle ("Win/Loss USD by Group and Month (Predicted or Actual)") + 
  theme(legend.title = element_blank())  +  
  scale_x_discrete(breaks = c(1:length(monthLabel)), labels = monthLabel) +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position="top" , legend.title = element_blank()) +
  theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=90)) +
  theme(plot.title = element_text(family = 'Times', face = 'bold', colour = '#4E575F', size = 14)) +
  theme(strip.background = element_rect(colour="black", fill="#8961AC",  size=1, linetype=1)) +
  theme(strip.text= element_text(size=15, color="white", face="bold")) 
# geom_text(data = df, aes(x = month, y = value, label = paste("$",format(value/1000, nsmall = 0, big.mark = ",", scientific = FALSE),"K")), color="black" ,  hjust=.5, size = 4)
#geom_text(label= format(round(sum(df[which(df$variable == "win"),"value"])), nsmall = 0, big.mark = ","), x=Inf, y=Inf, hjust=1, vjust=1.2)
#p




################### Actual code ####################
#g = qplot(`Petal.Length`, data = iris, fill = `Species`, main = Sys.time());
####################################################

############# Create and save widget ###############
g = ggplotly(p);
internalSaveWidget(g, 'out.html');
####################################################
