#�f�[�^�̓ǂݍ���
bank_data<-read.csv("bank_marketing_train.csv")
head(bank_data)

#�f�[�^���e�̊m�F
str(bank_data)
summary(bank_data)

#���e������ϐ�
# age �N��
# job �E��
# marital �����X�e�[�^�X
# education �w��
# default �N���W�b�g�s���s
# housing �Z��[���̗L��
# loan �l���[���̗L��
# contact �ߋ��̃R���^�N�g���@
# month �ߋ��̃R���^�N�g��
# day_of_week �ߋ��̃R���^�N�g�j��
# duration �ߋ��̃R���^�N�g���̎���
# campaign �{�L�����y�[���̃R���^�N�g��
# pdays �ߋ��̃R���^�N�g����̌o�ߓ���
# previous �{�L�����y�[���ȑO�̃R���^�N�g��
# poutcome �ߋ��̃L�����y�[���̌���
# emp.var.rate �A�E��
# cons.price.idx ����ҕ����w��
# cons.conf.idx ����ҐM�����w��
# euribor3m ���B��s�Ԏ������
# nr.employed �Ј���

#�ړI�ϐ���yes�ł����1, no�ł����0�ɂ���
bank_data$y<-ifelse(bank_data$y=="yes", 1, 0)

#�f�[�^���݂Ă݂�
plot(y ~ age, data=bank_data)
plot(y ~ job, data=bank_data)
plot(y ~ poutcome, data=bank_data)
plot(y ~ duration, data=bank_data)
plot(y ~ previous, data=bank_data)

job_retired <- bank_data$job == "retired"
plot(y ~ job_retired, data=bank_data)


#�_�~�[�ϐ��̐ݒ�
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

#�w�K�p�ƃe�X�g�p�Ƀf�[�^�𕪂���
set.seed(1234)  # �R�[�h�̍Č�����ۂ���seed���Œ�
num_rows<-dim(bank_data)[1]
idx<-c(1:num_rows)
train_idx<-sample(idx, size = num_rows*0.7 )
train_data<-bank_data[train_idx, ]
validation_data<-bank_data[-train_idx, ]

#���W�X�e�B�b�N��A����
bank_data_lm<-glm(y~.-duration-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed, data=train_data, family = "binomial")
summary(bank_data_lm)
AIC(bank_data_lm)

#step�֐�
bank_data_lm2<-step(bank_data_lm)
summary(bank_data_lm2)
AIC(bank_data_lm2)

#step�֐����s��̃��f��
bank_data_lm3<-glm(y ~ job + default + contact  + month  + day_of_week + campaign + poutcome, family = "binomial", data = train_data)

#�I�b�Y��
exp(bank_data_lm2$coefficients)

#���I�b�Y��1�ȏ�̐����ϐ�
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

#vif�̃`�F�b�N
library(car)
vif(bank_data_lm2)

#ROI���ő剻�����邽�߂̃A�^�b�N���X�g���o�͂���A���S���Y���̍쐬
score<-predict(bank_data_lm2, validation_data, type = "response")
score

#���σ��A�N�V����������臒l�̐ݒ�
summary(train_data)
ypred_flag<-ifelse(score >  0.08211694052, 1, 0)
ypred_flag

#confusion matrix
conf_mat<-table(validation_data$y, ypred_flag )
conf_mat

#accuracy
accuracy<-(conf_mat[1] + conf_mat[4]) /(conf_mat[1] + conf_mat[2] + conf_mat[3] + conf_mat[4])
accuracy

#�A�^�b�N���X�g
attack_num<-conf_mat[3] + conf_mat[4]
attack_num

#1�l�̉˓d��500�~������
your_cost <- attack_num * 500
your_cost

#����ҁ~2000�~
conf_mat[4] 
expected_revenue<-conf_mat[4] * 2000
expected_revenue

#���҂������v
expected_revenue - your_cost

#臒l��ς��Ă݂�
ypred_flag<-ifelse(score > 0.215, 1, 0)
conf_mat<-table(validation_data$y, ypred_flag )
conf_mat

attack_num<-conf_mat[3] + conf_mat[4]
your_cost <- attack_num * 500
expected_revenue<-conf_mat[4] * 2000
expected_revenue
expected_revenue - your_cost

#test�f�[�^
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

#month�f�[�^�𔲂�
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