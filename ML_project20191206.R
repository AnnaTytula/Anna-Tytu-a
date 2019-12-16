#######################################################################################
# Problem and data source  
#######################################################################################
#Using data on loans create a model to predict if requested loan will be good or bad
#Data source:https://github.com/szelor/practical-machine-learning/tree/master/Chapter10
#Data labels source:https://www.kaggle.com/justjonathan/lendingclub/version/1
#--------------------------------------------------------------------------------------

######################################################################################
# Libraries  
######################################################################################
# installing/loading the package:
if(!require(tidyverse)) {
  install.packages('tidyverse'); require(tidyverse)} #load / install+load tidyverse
if(!require(funModeling)) {
  install.packages('funModeling'); require(funModeling)} #load / install+load funModeling
if(!require(reshape2)) {
  install.packages('reshape2'); require(reshape2)} #load / install+load reshape2
if(!require(ggplot2)) {
  install.packages('ggplot2'); require(ggplot2)} #load / install+load ggplot2


######################################################################################
# Functions 
######################################################################################
# get_NA_into_0 - function that replace NA by 0 in selected columns 
# get_month_into_number - function that replace character into date type in one date 
# get_str_into_date - wrapper function that replace character into date type in dates  
# get_date_into_diff - function that replace dates by difference between two dates in days this is a timeframe between selected date and issue date
# get_char_into_number - function that delete everything except the number for example: string like "2%" will be replaced by 2
# get_perchar_into_number - wrapper function that delete everything except the number
# get_NA_into_1 - function that replace NA by 1 in selected columns 

##############################################################
# function that replace NA by 0 in selected columns 
get_NA_into_0<-function(data_object,data_columns){
  data_object<-data_object %>%
    mutate_at(.vars = data_columns, .funs = funs(replace(., is.na(.), 0)))
  return(data_object)
}

##############################################################
# function that replace character into date type in one date 
get_month_into_number<-function(data_char){
  data_char<-data_char %>%
    str_replace_all(., 'Jan', '1-1')   %>%
    str_replace_all(., 'Feb', '1-2')   %>%
    str_replace_all(., 'Mar', '1-3')   %>%
    str_replace_all(., 'Apr', '1-4')   %>%
    str_replace_all(., 'May', '1-5')   %>%
    str_replace_all(., 'Jun', '1-6')   %>%
    str_replace_all(., 'Jul', '1-7')   %>%
    str_replace_all(., 'Aug', '1-8')   %>%
    str_replace_all(., 'Sep', '1-9')   %>%
    str_replace_all(., 'Oct', '1-10')  %>%
    str_replace_all(., 'Nov', '1-11')  %>%
    str_replace_all(., 'Dec', '1-12')
  
  data_char<-as.Date(data_char,"%d-%m-%Y")
  return(data_char)
}

################################################################
# wrapper function that replace character into date type in dates  
get_str_into_date<-function(data_object,data_columns){
  for(col in data_columns){
    data_object[,col]=lapply(data_object[,col],get_month_into_number)
  }
  return(data_object)
}

################################################################
# function that replace dates by difference between two dates in days
# this is a timeframe between selected date and issue date
get_date_into_diff<-function(data_object,data_columns){
  for(col in data_columns){
    data_object[,col]=data_object[,col]-data_object[,'issue_d']
  }
  return(data_object)
}

################################################################
# function that delete everything except the number
# for example: string like "2%" will be replaced by 2
get_char_into_number<-function(data_char){
  data_char<-data_char %>%
    str_replace_all(., '[^0-9.-]', '')  
  data_char<-as.integer(data_char)
  return(data_char)
}

################################################################
# wrapper function that delete everything except the number
get_perchar_into_number<-function(data_object,data_columns){
    for(col in data_columns){
      data_object[,col]=lapply(data_object[,col],get_char_into_number)
    }
    return(data_object)
}

################################################################
# function that replace NA by 1 in selected columns 
get_NA_into_1<-function(data_object,data_columns){
  data_object<-data_object %>%
    mutate_at(.vars = data_columns, .funs = funs(replace(., is.na(.), 1)))
  return(data_object)
}

####################################################################################
# Run 
####################################################################################

#------------------
# --- Load data ---
#------------------
dataQ1 <- readr::read_csv('LoanStats_2018Q1.csv', na = 'NULL')
dataQ2 <- readr::read_csv('LoanStats_2018Q2.csv', na = 'NULL')


#----------------------------
# --- Merge data together ---
#----------------------------
dataLoan<-rbind(dataQ1,dataQ2)


#--------------------------------
# --- make loan status binary ---
#--------------------------------
#use only rows where loan status is not empty
dataLoan<-dataLoan[which(!is.na(dataLoan[,'loan_status'])),]
#make loan status binary
dataLoan[which(dataLoan[,'loan_status']=='Current'),'loan_status']='good'
dataLoan[which(dataLoan[,'loan_status']=='Fully Paid'),'loan_status']='good'
dataLoan[which(dataLoan[,'loan_status']!='good'),'loan_status']='bad'


#-------------------
# --- Check data ---
#-------------------
# stats for 0, NA, inf, var type and nb of unique values for each column 
Loan_stats<-df_status(dataLoan,print_results = TRUE)


#-----------------------
# -- Replace NA by 0 ---
#-----------------------
#replace NA by 0 in columns with number of months since some event 
columns_get_NA_into_0<-c('mo_sin_old_il_acct',
                         'mo_sin_old_rev_tl_op',
                         'mo_sin_rcnt_rev_tl_op',
                         'mo_sin_rcnt_tl',
                         'mths_since_rcnt_il',
                         'mths_since_last_delinq',
                         'mths_since_last_major_derog',
                         'mths_since_last_record',
                         'mths_since_recent_bc',
                         'mths_since_recent_bc_dlq',
                         'mths_since_recent_inq',
                         'mths_since_recent_revol_delinq',
                         'sec_app_mths_since_last_major_derog')
dataLoan<-get_NA_into_0(dataLoan,columns_get_NA_into_0)


#-------------------------------------
# -- Convert date string into date ---
#-------------------------------------
columns_get_months_into_number<-c('issue_d',
                                  'earliest_cr_line',
                                  'last_pymnt_d',
                                  'next_pymnt_d',
                                  'last_credit_pull_d')
dataLoan<-get_str_into_date(dataLoan,columns_get_months_into_number)

#replace dates by difference between date and issue date
dataLoan<-get_date_into_diff(dataLoan,columns_get_months_into_number[-which(columns_get_months_into_number=='issue_d')])


#--------------------------------------
# -- change emp_length into numbers ---
#--------------------------------------
dataLoan[,'emp_length']=lapply(dataLoan[,'emp_length'],get_delete_nonumbers)


#--------------------------------------------
# --- replace percentage char into number ---
#--------------------------------------------
columns_get_perchar_into_number<-c('int_rate','revol_util')
dataLoan<-get_perchar_into_number(dataLoan,columns_get_perchar_into_number)


#------------------------------------
# -- search for columns to delete ---
#------------------------------------
# delete empty rows, because empty values are not useful
columns_to_delete<-colnames(dataLoan)[Loan_stats$unique==0]
dataLoan<-dataLoan[,-which(colnames(dataLoan) %in% columns_to_delete)]
Loan_stats<-df_status(dataLoan,print_results = FALSE)

# delete columns with more than 70% NA values
columns_to_delete<-(Loan_stats[,'variable'])[Loan_stats[,'p_na']>70]
dataLoan<-dataLoan[,-which(colnames(dataLoan) %in% columns_to_delete)]
Loan_stats<-df_status(dataLoan,print_results = FALSE)

# check which colnames have only one value
colnames(dataLoan)[Loan_stats$unique==1]
# columns to delete because of only one value
# - policy_code - publicly available policy_code=1, new products not publicly available policy_code=2
# - num_tl_120dpd_2m	- Number of accounts currently 120 days past due (updated in past 2 months)
columns_to_delete<-c(colnames(dataLoan)[Loan_stats$unique==1])

# Variables related to the future (result from the loan status)
# - last_pymnt_d - Last month payment was received   <--- if cretit is not fully-paid this give us information aboiut status: Late
# - last_credit_pull_d - The most recent month LC pulled credit for this loan
# - delinq_2yrs	- The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years
# - mths_since_last_delinq	- The number of months since the borrower's last delinquency.
# - acc_now_delinq	- The number of accounts on which the borrower is now delinquent.
# - delinq_amnt	- The past-due amount owed for the accounts on which the borrower is now delinquent.
# - mths_since_recent_revol_delinq	- Months since most recent revolving delinquency.
columns_to_delete<-c(columns_to_delete,
                     'last_pymnt_d',
                     'last_credit_pull_d',
                     'delinq_2yrs',
                     'mths_since_last_delinq',
                     'acc_now_delinq',
                     'delinq_amnt',
                     'mths_since_recent_revol_delinq')
                     
# columns that are used to combine a few variable in a new one 
# - dti is a ratio calculated using the borrower’s total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower’s self-reported monthly income.
columns_to_delete<-c(columns_to_delete,
                     'annual_inc')
# - revol_util	- Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
#total_rev_hi_lim - 	Total revolving high credit/credit limit
#revol_bal_joint - Sum of revolving credit balance of the co-borrowers, net of duplicate balances
columns_to_delete<-c(columns_to_delete,
                     'total_rev_hi_lim',
                     'revol_bal_joint')
# bc_util	is a ratio of total current balance to high credit/credit limit for all bankcard accounts.
# - total_bal_il	- Total current balance of all installment accounts
# - bc_open_to_buy	- Total open to buy on revolving bankcards.
columns_to_delete<-c(columns_to_delete,
                     'total_bal_il','bc_open_to_buy')


# emp_title is a character type variable with 66100 unique values
# emp_titlecoud be connected with income and the income is more important information
# therefore emp_title will be deleted
columns_to_delete<-c(columns_to_delete,
                     'emp_title')

#issue_d is a date when the loan was funded
columns_to_delete<-c(columns_to_delete,
                     'issue_d')

dataLoan<-dataLoan[,-which(colnames(dataLoan) %in% columns_to_delete)]
Loan_stats<-df_status(dataLoan,print_results = TRUE)

#-----------------------
# -- Replace NA by 1 ---
#-----------------------
# - revol_util - Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit
dataLoan<-get_NA_into_1(dataLoan,c('revol_util','dti'))


#-----------------------
# -- Replace NA by 0 ---
#-----------------------
dataLoan<-get_NA_into_0(dataLoan,
                        c('emp_length',
                          'next_pymnt_d',
                          'all_util',
                          'avg_cur_bal',
                          'percent_bc_gt_75',
                          'il_util',
                          'bc_util',
                          'pct_tl_nvr_dlq'))

Loan_stats<-df_status(dataLoan,print_results = TRUE)


#------------------------------
# --- leading with outliers ---
#------------------------------
# Bad loan is some king of outliners in loans
# therefore I decided to delete all outlines but only for good loan status
# to speed-up I used min and max values of boxplot and set these values for all points that were outside the boxplot
#   Q1<-quantile(c(unlist(dataLoan[,'revol_bal'])), c(.25)) 
#   Q3<-quantile(c(unlist(dataLoan[,'revol_bal'])), c(.75)) 
#   IQR <- Q3-Q1
#   minimum of boxplot is Q1-1.5*IQR=-0.5*Q1-1.5*Q3
#   maximum of boxplot is Q3+1.5*IQR=2.5*Q3-1.5*Q1
#filtering numeric columns
numeric_cols <- sapply(dataLoan, is.numeric)
numeric_col_names<-names(numeric_cols[which(numeric_cols==TRUE)])
#check how many columns are numeric
num_cols<-length(numeric_col_names)  
for( i in 1:num_cols){
  good_id<-which(dataLoan[,'loan_status']=='good')
  Q1<-quantile(c(unlist(dataLoan[good_id,numeric_col_names[i]])), c(.25)) 
  Q3<-quantile(c(unlist(dataLoan[good_id,numeric_col_names[i]])), c(.75)) 
  IQR <- Q3-Q1
  min_b<-0.5*Q1-1.5*Q3
  max_b<-2.5*Q3-1.5*Q1
  if(length(which(dataLoan[good_id,numeric_col_names[i]]<min_b))>0){
    dataLoan[intersect(which(dataLoan[,numeric_col_names[i]]<min_b),good_id),numeric_col_names[i]]=min_b
  }
  if(length(which(dataLoan[good_id,numeric_col_names[i]]>max_b))>0){
    dataLoan[intersect(which(dataLoan[,numeric_col_names[i]]>max_b),good_id),numeric_col_names[i]]=max_b
  }
}


#----------------------------------------
# -- variables density by loan status ---
#----------------------------------------
# when plots of variable densities are different for different loan status 
# then this variable has impact on loan status, 
# otherwise its impact on loan status is not significant

#filtering numeric columns
numeric_cols <- sapply(dataLoan, is.numeric)
#check how many columns are numeric
length(numeric_cols[which(numeric_cols==TRUE)])  
summary(numeric_cols[which(numeric_cols==TRUE)])

#turn the data into long format (key->value)
#columns 1-12 
loans.lng <- melt(cbind(dataLoan[,'loan_status'],(dataLoan[,numeric_cols])[,1:12]), id='loan_status')
#plot the distribution for 'loan_status' for each numeric column
ggplot(aes(x=value, group=loan_status, colour=loan_status), data=loans.lng) + geom_density() + facet_wrap(~variable, scales='free')
#ggsave('loans_1.jpg',height=30, width = 50, dpi=300,limitsize = FALSE)
#I decided to delete variables:
columns_to_delete<-c('mths_since_last_record',
                     'revol_bal')
#columns 13-24
loans.lng <- melt(cbind(dataLoan[,'loan_status'],(dataLoan[,numeric_cols])[,13:24]), id='loan_status')
ggplot(aes(x=value, group=loan_status, colour=loan_status), data=loans.lng) + geom_density() + facet_wrap(~variable, scales='free')
ggsave('loans_2.jpg',height=30, width = 50, dpi=300,limitsize = FALSE)
columns_to_delete<-c(columns_to_delete,
                     'total_acc',
                     'total_pymnt',
                     'total_pymnt_inv',
                     'total_rec_late_fee',
                     'recoveries',
                     'collection_recovery_fee',
                     'last_pymnt_amnt')
#columns 25-36
loans.lng <- melt(cbind(dataLoan[,'loan_status'],(dataLoan[,numeric_cols])[,25:36]), id='loan_status')
ggplot(aes(x=value, group=loan_status, colour=loan_status), data=loans.lng) + geom_density() + facet_wrap(~variable, scales='free')
ggsave('loans_3.jpg',height=30, width = 50, dpi=300,limitsize = FALSE)
columns_to_delete<-c(columns_to_delete,
                     'mths_since_last_major_derog',
                     'tot_coll_amt',
                     'tot_cur_bal',
                     'mths_since_rcnt_il')
#columns 37-48
loans.lng <- melt(cbind(dataLoan[,'loan_status'],(dataLoan[,numeric_cols])[,37:48]), id='loan_status')
ggplot(aes(x=value, group=loan_status, colour=loan_status), data=loans.lng) + geom_density() + facet_wrap(~variable, scales='free')
ggsave('loans_4.jpg',height=30, width = 50, dpi=300,limitsize = FALSE)
columns_to_delete<-c(columns_to_delete,
                     'max_bal_bc',
                     'avg_cur_bal',
                     'mo_sin_old_il_acc',
                     'mo_sin_old_rev_tl_op',
                     'mo_sin_rcnt_rev_tl_op')
#columns 49-60
loans.lng <- melt(cbind(dataLoan[,'loan_status'],(dataLoan[,numeric_cols])[,49:60]), id='loan_status')
ggplot(aes(x=value, group=loan_status, colour=loan_status), data=loans.lng) + geom_density() + facet_wrap(~variable, scales='free')
ggsave('loans_5.jpg',height=30, width = 50, dpi=300,limitsize = FALSE)
columns_to_delete<-c(columns_to_delete,
                     'mo_sin_rcnt_tl',
                     'mths_since_recent_bc',
                     'mths_since_recent_bc_dlq',
                     'num_il_tl')
#columns 61-75
loans.lng <- melt(cbind(dataLoan[,'loan_status'],(dataLoan[,numeric_cols])[,61:75]), id='loan_status')
ggplot(aes(x=value, group=loan_status, colour=loan_status), data=loans.lng) + geom_density() + facet_wrap(~variable, scales='free')
ggsave('loans_5.jpg',height=30, width = 50, dpi=300,limitsize = FALSE)
columns_to_delete<-c(columns_to_delete,
                     'num_rev_accts',
                     'num_sats',
                     'num_tl_90g_dpd_24m',
                     'pct_tl_nvr_dlq',
                     'tot_hi_cred_lim',
                     'total_bal_ex_mort',
                     'total_il_high_credit_limit',
                     'sec_app_mths_since_last_major_derog')

dataLoan<-dataLoan[,-which(colnames(dataLoan) %in% columns_to_delete)]
Loan_stats<-df_status(dataLoan,print_results = TRUE)

#-------------------------------
# --- save the final object  ---
#-------------------------------
write.csv(dataLoan,'dataLoan.csv',row.names=FALSE)


