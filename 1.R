library(rpart)
install.packages("randomForest")
library(randomForest)
install.packages("e1071")
library(e1071)
library(class)
install.packages("caret")
library(caret)

# 1번 문제
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)

ucla$admit = factor(ucla$admit)

# 2번 문제
n = nrow(ucla)
i = 1:n
train_list = sample(i, n*0.6)
test_list = setdiff(i, train_list)
ucla_train = ucla[train_list, ]
ucla_test = ucla[test_list, ]

#결정트리
r = rpart(admit~., data = ucla_train)
printcp(r)

p = predict(r, newdata = ucla_test, type = 'class')
table(p, ucla_test$admit)

# 랜덤포레스트 트리50개
f = randomForest(admit~., data = ucla_test, ntree= 50)
f

# 랜덤포레스트 트리1000개
thf = randomForest(admit~., data = ucla_test, ntree= 1000)
thf

# SVM(radial basis)
s = svm(admit~., ucla_train)
print(s)

table(predict(s,ucla_test),ucla_test$admit)

# SVM(polynimial)
ps = svm(admit~., ucla_train, kernel = 'polynomial')
ps

table(predict(ps, ucla_test),ucla_test$admit)

# knn
train = ucla_train
test = ucla_test
k = knn(train, test, train$admit, k=5)
confusionMatrix(k,ucla_test$admit)
