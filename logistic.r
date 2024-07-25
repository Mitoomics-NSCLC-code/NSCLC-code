rm(list=ls())
options(stringsAsFactors=F)
path<-'H:\\data_path'
setwd(path)

#读取训练&测试样本集
train<-read.table(file='train.txt',sep='\t',header=T) 
v1<-read.table(file='validation1.txt',sep='\t',header=T)  

#新生成一行label，作为模型输入
train$label = factor(1*(train$label=='NSCLC'))
v1$label = factor(1*(v1$label=='NSCLC'))
View(v1)    #看一下现在数据长什么样

#查看数据基本情况
summary(train)
summary(v1)
#用训练数据拟合模型
l_model = glm(label~.,data = v1, family = 'binomial')  #可以替换data=后面的数据集
summary(l_model)
lambda =0.5 
v1$predict = 1*(predict(l_model,v1)>lambda)
train$predict = 1*(predict(l_model,train)>lambda)
v1pred2 = exp(predict(l_model,v1)) / (1 + exp(predict(l_model,v1)) )  #用公式计算所得值
trainpred2 = exp(predict(l_model,train)) / (1 + exp(predict(l_model,train)) )

v1$pred2 = v1pred2
train$pred2 = trainpred2
View(v1)
#计算模型精度
predict_value = v1$predict  #预测值
true_value = as.numeric(v1$label)-1 #真实值
error=predict_value-true_value

#判断正确的数量占总数的比例 

accuracy=(nrow(v1)-sum(abs(error)))/nrow(v1)



#混淆矩阵中的量（混淆矩阵具体解释见下页）

#真实值预测值全为1 / 预测值全为1 --- 提取出的正确信息条数/提取出的信息条数 

precision=sum(true_value & predict_value)/sum(predict_value)

#真实值预测值全为1 / 真实值全为1 --- 提取出的正确信息条数 /样本中的信息条数 

recall=sum(predict_value & true_value)/sum(true_value)
#输出以上各结果 

print(accuracy) 

print(precision) 

print(recall) 

print(F_measure) 

#混淆矩阵，显示结果依次为TP、FN、FP、TN 

table(true_value,predict_value)
