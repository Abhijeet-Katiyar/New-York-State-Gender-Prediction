import pandas as pd
import numpy as np


TOT_POP=[]
PCT_U18=[]
PC_18_65=[]
PCT_O65=[]
MALE_FEM=[]

for i in range(len(in_data)):
    TOT_POP.append(in_data[i][0])

for i in range(len(in_data)):
    PCT_U18.append(in_data[i][1])

for i in range(len(in_data)):
    PC_18_65.append(in_data[i][2])

for i in range(len(in_data)):
    PCT_O65.append(in_data[i][3])

for i in range(len(in_data)):
    MALE_FEM.append(in_data[i][4])

Data=pd.DataFrame(index=range(1,712))


Data['TOT_POP']=pd.Series(TOT_POP,index=Data.index)
Data['PCT_U18']=pd.Series(PCT_U18,index=Data.index)
Data['PC_18_65']=pd.Series(PC_18_65,index=Data.index)
Data['PCT_O65']=pd.Series(PCT_O65,index=Data.index)
Data['MALE_FEM']=pd.Series(MALE_FEM,index=Data.index)


X=np.log(Data[['TOT_POP', 'PCT_U18', 'PC_18_65', 'PCT_O65']])
y=np.log(Data['MALE_FEM'])

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split( X, y, test_size=0.30, random_state=42)

from sklearn.linear_model import LinearRegression as LR
lm=LR(normalize=True,fit_intercept=True)
lm_fit=lm.fit(X_train,y_train)

Predicts=lm.predict(np.array(X_test))

import sklearn.metrics as po
print(po.r2_score(np.exp(y_test),np.exp(Predicts)))
