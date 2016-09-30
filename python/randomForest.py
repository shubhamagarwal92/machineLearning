import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import cross_val_score
# from numpy import genfromtxt, savetxt
from sklearn.feature_extraction import DictVectorizer
import cPickle
from sklearn.metrics import confusion_matrix


def transformData(train):
	# train[0] is the label
	# train['factorVariable'] converted to dummy variable
	countFeatures = train.shape[1]
	# print countFeatures
	d1 = pd.get_dummies(train['factorVariable'], prefix = 'factorVariable')
	train.drop('factorVariable', axis=1, inplace=True)
	countFeatures = train.shape[1]
	# print countFeatures
	train = pd.concat([train, d1], axis=1)
	# countFeatures = train.shape[1]
	# print countFeatures
	countFeatures = train.shape[1]-1
	x = train[train.columns[1:countFeatures]]
	y = train[train.columns[0]]
	names = x.columns.values
	print names
	# y, _ = pd.factorize(train['action'])
	x = x.values
	y = y.values
	return (x,y,names)

train = pd.read_csv('path-to-train.csv',sep='\t')
train['factor'].describe()
train.drop('dropVariable', axis=1, inplace=True)

x,y,names = transformData(train)
clf = RandomForestClassifier(n_estimators=100, n_jobs = 10)
model = clf.fit(x,y)

print sorted(zip(map(lambda x: round(x, 4), model.feature_importances_), names), 
             reverse=True)

with open('path-to-dump', 'wb') as f:
    cPickle.dump(model, f)

## Reading pickle
# with open('path-to-dump','rb') as f:
#     model= cPickle.load(f)

# Scikit automatically uses training data (63% training, rest valid)
# for validation too in Random Forest
test = pd.read_csv('path-to-test.csv',sep='\t')
test.drop('timestamp', axis=1, inplace=True)

x,y,names = transformData(test)
pred = model.predict(x)
scores = cross_val_score(model, x, y)
scores.mean()

cm = confusion_matrix(y, pred)
np.set_printoptions(precision=2)


from sklearn import metrics
# testing score
score = metrics.f1_score(y, pred, pos_label=list(set(y)))
pscore = metrics.accuracy_score(y, pred)

# training score
score_train = metrics.f1_score(y_train, pred_train, pos_label=list(set(y_train)))


#### Latest working code
from sklearn.metrics import classification_report
print classification_report(y,pred, target_names=list(set(y)))
print confusion_matrix(y, pred, labels=list(set(y)))


y_true = pd.Series(y)
y_pred = pd.Series(pred)
pd.crosstab(y_true, y_pred, rownames=['True'], colnames=['Predicted'], margins=True)

#### Saving the model using joblib instead of pickle
# from sklearn.externals import joblib
# joblib.dump(clf, 'filename.pkl') 

# #then your colleagues can load it

# clf = joblib.load('filename.pk1')
