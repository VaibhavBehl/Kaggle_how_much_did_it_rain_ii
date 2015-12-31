'''
Created on Dec 5, 2015

@author: vaibhav
'''
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.externals.joblib import Memory
import pandas as pd
import numpy as np
from sklearn.preprocessing import Imputer
from sklearn.metrics import mean_absolute_error

mem = Memory("./mycache_can_delete")
@mem.cache
def get_data(dataFile):
    return pd.read_csv(dataFile)

xTr = get_data('C:/myD/workarea/Kaggle567Workarea/mlclass_567_kaggle_rain/python/src/root/regressors/xtr_only_7030_many_feat.csv')
xTe = get_data('C:/myD/workarea/Kaggle567Workarea/mlclass_567_kaggle_rain/python/src/root/regressors/xte_only_7030_many_feat.csv')
yTe = get_data('C:/myD/workarea/Kaggle567Workarea/mlclass_567_kaggle_rain/python/src/root/regressors/yte_only_7030_many_feat.csv')
yTr = get_data('C:/myD/workarea/Kaggle567Workarea/mlclass_567_kaggle_rain/python/src/root/regressors/ytr_only_7030_many_feat.csv')

xTrMat = xTr.as_matrix()
yTrVec = yTr.as_matrix()
xTeMat = xTe.as_matrix()
yTeVec = yTe.as_matrix()

imp = Imputer(missing_values='NaN', strategy='mean', axis=0)
xTrNew = imp.fit_transform(xTrMat)
xTeNew = imp.fit_transform(xTeMat)

est = GradientBoostingRegressor(n_estimators=300, learning_rate=0.01, max_depth=9, subsample=0.8, 
                                random_state=292, loss='lad',verbose=2)

est.fit(xTrNew, yTrVec)
yTeEst = est.predict(xTeNew)
print(mean_absolute_error(yTeVec,yTeEst))


# # should choose by random
# trainFileNo = [1,2,4,5,6,8,9]
# testFileNo = [3,7,10]
# trainFiles = list()
# testFiles = list()
# for i in range(0,len(trainFileNo)):
#     trainFiles.append("C:/myD/workarea/Kaggle567Workarea/mlclass_567_kaggle_rain/data_CSV/splits_10/train_new_split" + str(trainFileNo[i]) + ".csv")
# for i in range(0,len(testFileNo)):
#     testFiles.append("C:/myD/workarea/Kaggle567Workarea/mlclass_567_kaggle_rain/data_CSV/splits_10/train_new_split" + str(testFileNo[i]) + ".csv")
# 
# #trData <- rbindlist(lapply(trainFiles, fread, sep=","))
# #teData <- rbindlist(lapply(testFiles, fread, sep=","))
# 
# list_ = []
# for file_ in trainFiles:
#     df = pd.read_csv(file_,index_col=None, header=0)
#     list_.append(df)
# trFrame = pd.concat(list_)
# list_ = []
# for file_ in testFiles:
#     df = pd.read_csv(file_,index_col=None, header=0)
#     list_.append(df)
# teFrame = pd.concat(list_)
