#データの読み込み
bank_data<-read.csv("bank_marketing_train.csv")
head(bank_data)

#データ内容の確認
str(bank_data)
summary(bank_data)

#↓各種説明変数
# age 年齢
# job 職業
# marital 婚姻ステータス
# education 学歴
# default クレジット不履行
# housing 住宅ローンの有無
# loan 個人ローンの有無
# contact 過去のコンタクト方法
# month 過去のコンタクト月
# day_of_week 過去のコンタクト曜日
# duration 過去のコンタクト時の時間
# campaign 本キャンペーンのコンタクト数
# pdays 過去のコンタクトからの経過日数
# previous 本キャンペーン以前のコンタクト数
# poutcome 過去のキャンペーンの結果
# emp.var.rate 就職率
# cons.price.idx 消費者物価指数
# cons.conf.idx 消費者信頼感指数
# euribor3m 欧州銀行間取引金利
# nr.employed 社員数

#目的変数をyesであれば1, noであれば0にする
bank_data$y<-ifelse(bank_data$y=="yes", 1, 0)

#データをみてみる
plot(y ~ age, data=bank_data)
plot(y ~ job, data=bank_data)
plot(y ~ poutcome, data=bank_data)
plot(y ~ duration, data=bank_data)
plot(y ~ previous, data=bank_data)

job_retired <- bank_data$job == "retired"
plot(y ~ job_retired, data=bank_data)


#ダミー変数の設定
bank_data$y<-as.factor(bank_data$y)
bank_data$job<-as.factor(bank_data$job)
bank_data$marital<-as.factor(bank_data$marital)
bank_data$education<-as.factor(bank_data$education)
bank_data$default<-as.factor(bank_data$default)
bank_data$housing<-as.factor(bank_data$housing)
bank_data$loan<-as.factor(bank_data$loan)
bank_data$contact<-as.factor(bank_data$contact)
bank_data$month<-as.factor(bank_data$month)
bank_data$day_of_week<-as.factor(bank_data$day_of_week)
bank_data$poutcome<-as.factor(bank_data$poutcome)

#学習用とテスト用にデータを分ける
set.seed(1234)  # コードの再現性を保つためseedを固定
num_rows<-dim(bank_data)[1]
idx<-c(1:num_rows)
train_idx<-sample(idx, size = num_rows*0.7 )
train_data<-bank_data[train_idx, ]
validation_data<-bank_data[-train_idx, ]

#ロジスティック回帰分析
bank_data_lm<-glm(y~.-duration-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed, data=train_data, family = "binomial")
summary(bank_data_lm)
AIC(bank_data_lm)

#step関数
bank_data_lm2<-step(bank_data_lm)
summary(bank_data_lm2)
AIC(bank_data_lm2)

#step関数実行後のモデル
bank_data_lm3<-glm(y ~ job + default + contact  + month  + day_of_week + campaign + poutcome, family = "binomial", data = train_data)

#オッズ比
exp(bank_data_lm2$coefficients)

#↓オッズ比1以上の説明変数
#jobretired
#jobself-employed
#jobstudent
#monthdec
#monthjun
#monthmar
#monthoct
#day_of_weekthu
#day_of_weektue
#day_of_weekwed
#poutcomenonexistent
#poutcomesuccess

#vifのチェック
library(car)
vif(bank_data_lm2)

#ROIを最大化させるためのアタックリストを出力するアルゴリズムの作成
score<-predict(bank_data_lm2, validation_data, type = "response")
score

#平均リアクション率から閾値の設定
summary(train_data)
ypred_flag<-ifelse(score >  0.08211694052, 1, 0)
ypred_flag

#confusion matrix
conf_mat<-table(validation_data$y, ypred_flag )
conf_mat

#accuracy
accuracy<-(conf_mat[1] + conf_mat[4]) /(conf_mat[1] + conf_mat[2] + conf_mat[3] + conf_mat[4])
accuracy

#アタックリスト
attack_num<-conf_mat[3] + conf_mat[4]
attack_num

#1人の架電に500円かかる
your_cost <- attack_num * 500
your_cost

#成約者×2000円
conf_mat[4] 
expected_revenue<-conf_mat[4] * 2000
expected_revenue

#期待される収益
expected_revenue - your_cost

#閾値を変えてみる
ypred_flag<-ifelse(score > 0.215, 1, 0)
conf_mat<-table(validation_data$y, ypred_flag )
conf_mat

attack_num<-conf_mat[3] + conf_mat[4]
your_cost <- attack_num * 500
expected_revenue<-conf_mat[4] * 2000
expected_revenue
expected_revenue - your_cost

#testデータ
test_data<-read.csv("bank_marketing_test.csv")

test_data$y<-ifelse(test_data$y=="yes", 1, 0)
test_data$y<-as.factor(test_data$y)
test_data$job<-as.factor(test_data$job)
test_data$marital<-as.factor(test_data$marital)
test_data$education<-as.factor(test_data$education)
test_data$default<-as.factor(test_data$default)
test_data$housing<-as.factor(test_data$housing)
test_data$loan<-as.factor(test_data$loan)
test_data$contact<-as.factor(test_data$contact)
test_data$month<-as.factor(test_data$month)
test_data$day_of_week<-as.factor(test_data$day_of_week)
test_data$poutcome<-as.factor(test_data$poutcome)

#monthデータを抜く
bank_data_lm4<-glm(y ~ job + default + contact  + day_of_week + campaign + poutcome, family = "binomial", data = train_data)

score2<-predict(bank_data_lm4, test_data, type = "response")
ypred_flag2<-ifelse(score2 > 0.215, 1, 0)
conf_mat2<-table(test_data$y, ypred_flag2 )
conf_mat2

attack_num2<-conf_mat2[3] + conf_mat2[4]
your_cost2 <- attack_num2 * 500
expected_revenue2<-conf_mat2[4] * 2000
expected_revenue2
expected_revenue2 - your_cost2

summary(bank_data_lm4)
