library(RODBC)
library(dplyr)

channel
channel = odbcConnect("scm",uid = "apoorv.anand" , pwd = "eru15ty047")
wwstr_buck = sqlQuery(channel,"select * from amer_raw_wwstr_master_3weeks_2;")

colnames(wwstr_buck)[3] <- "revenue"

wwstr_buck$revenuecut<-cut(wwstr_buck$Revenue,breaks = seq(0,max(wwstr_buck$Revenue),20000),labels = FALSE)


attach(wwstr_buck)
wwstr_buck<-wwstr_buck %>% group_by(revenuecut,seg_desc) %>% summarise(Units=sum(sys_qty)
                                                          ,Dealcount=n_distinct(Segment_Key_Custno_Brand))
detach(wwstr_buck)


write.csv(wwstr_buck,"wwstr_buck.csv")
sqlQuery(channel,"select distinct deal_key from amer_ads80k_master_deal_key;")

ads_201514 = sqlQuery(channel,"select * from ads_201514_0509")
ads_201515 = sqlQuery(channel,"select * from ads_201515_0509")
amer_ads_201514 = sqlQuery(channel,"select * from amer_ads_201514")

amer_ads_201514$fisc_week_val

#Waterfall charts - Deals
attach(ads_0909)
hyp1<-ads_0909 %>% group_by(indirect_L1_type_flag) %>%
  select(indirect_L1_type_flag) %>%  
  summarise(CountKeyDealFlag=n_distinct(deal_key))


hyp2= ads_0909 %.%
  group_by(indirect_L1_type_flag,key1_flag) %.%
  select(indirect_L1_type_flag,key1_flag) %.%
  summarise(CountKeyDealFlag=n_distinct(deal_key)) %.%
  filter(key1_flag == 1)



hyp3= ads_0909 %.%
  group_by(indirect_L1_type_flag,key1_flag,key2_flag) %.%
  select(indirect_L1_type_flag,key1_flag,key2_flag) %.%
  summarise(CountKeyDealFlag=n_distinct(deal_key)) %.%
  filter(key1_flag == 1 , key2_flag == 1)

hyp4= ads_0909 %.%
  group_by(indirect_L1_type_flag,key1_flag,key2_flag,key3_flag) %.%
  select(indirect_L1_type_flag,key1_flag,key3_flag) %.%
  summarise(CountKeyDealFlag=n_distinct(deal_key)) %.%
  filter(key1_flag == 1 , key2_flag == 1, key3_flag == 1)


hyp5= ads_0909 %.%
  group_by(indirect_L1_type_flag,key1_flag,key2_flag,key3_flag,key4_final) %.%
  select(indirect_L1_type_flag,key1_flag,key3_flag,key4_final) %.%
  summarise(CountKeyDealFlag=n_distinct(deal_key)) %.%
  filter(key1_flag == 1 , key2_flag == 1, key3_flag == 1,key4_final == 1)


hyp6= ads_0909 %.%
  group_by(indirect_L1_type_flag,key1_flag,key2_flag,key3_flag,key4_final,compliant_flag) %.%
  select(indirect_L1_type_flag,key1_flag,key3_flag,key4_final,compliant_flag) %.%
  summarise(CountKeyDealFlag=n_distinct(deal_key)) %.%
  filter(key1_flag == 1 , key2_flag == 1, key3_flag == 1,key4_final == 1 ,compliant_flag == 1)


#Waterfall charts - Units

hyp1u<-ads_0909 %>% group_by(indirect_L1_type_flag) %>%
  select(indirect_L1_type_flag) %>%  
  summarise(SumsysQty=sum(sys_qty))



hyp2u= ads_0909 %.%
  group_by(indirect_L1_type_flag,key1_flag) %.%
  select(indirect_L1_type_flag,key1_flag) %.%
  summarise(SumsysQty=sum(sys_qty)) %.%
  filter(key1_flag == 1)



hyp3u= ads_0909 %.%
  group_by(indirect_L1_type_flag,key1_flag,key2_flag) %.%
  select(indirect_L1_type_flag,key1_flag,key2_flag) %.%
  summarise(SumsysQty=sum(sys_qty)) %.%
  filter(key1_flag == 1 , key2_flag == 1)

hyp4u= ads_0909 %.%
  group_by(indirect_L1_type_flag,key1_flag,key2_flag,key3_flag) %.%
  select(indirect_L1_type_flag,key1_flag,key3_flag) %.%
  summarise(SumsysQty=sum(sys_qty)) %.%
  filter(key1_flag == 1 , key2_flag == 1, key3_flag == 1)


hyp5u= ads_0909 %.%
  group_by(indirect_L1_type_flag,key1_flag,key2_flag,key3_flag,key4_final) %.%
  select(indirect_L1_type_flag,key1_flag,key3_flag,key4_final) %.%
  summarise(SumsysQty=sum(sys_qty)) %.%
  filter(key1_flag == 1 , key2_flag == 1, key3_flag == 1,key4_final == 1)


hyp6u= ads_0909 %.%
  group_by(indirect_L1_type_flag,key1_flag,key2_flag,key3_flag,key4_final,compliant_flag) %.%
  select(indirect_L1_type_flag,key1_flag,key3_flag,key4_final,compliant_flag) %.%
  summarise(SumsysQty=sum(sys_qty)) %.%
  filter(key1_flag == 1 , key2_flag == 1, key3_flag == 1,key4_final == 1 ,compliant_flag == 1)







#Rest of the hypothesis

attach(ads_0909)
rev_bucket = ads_0909 %>% 
  group_by(deal_key) %>%  
  select(deal_key,revenue_USD,sys_qty) %>%
  summarise(
  revenue = sum(revenue_USD) ,
  sys_qty = sum(sys_qty)
)


