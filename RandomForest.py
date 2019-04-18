from sklearn.metrics import classification_report
## Random forest base-line model
from imblearn.over_sampling import SMOTE, ADASYN
from imblearn.ensemble import BalancedRandomForestClassifier
from imblearn.pipeline import make_pipeline



rf=RandomForestClassifier(bootstrap=True,
            criterion='gini', max_depth=50, max_features=10,
            max_leaf_nodes=None, min_impurity_decrease=0.0,
            min_impurity_split=None, min_samples_leaf=1,
            min_samples_split=2, min_weight_fraction_leaf=0.0,
            n_estimators=200, n_jobs=-1, oob_score=True, random_state=17,
            verbose=1,warm_start=True)

from sklearn.model_selection import GridSearchCV

# Create the parameter grid based on the results of random search 
param_grid = {
    'bootstrap': [True],
    'max_depth': [50,100,150],
    'max_features': [3,4,5,6],
    'n_estimators': [100, 200, 300, 400]
}

# Instantiate the grid search model
grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, scoring='roc_auc', cv = 3, n_jobs = -1, verbose = 1, return_train_score=True)

best = grid_search.best_estimator_
pred= best.predict(X_test)
prob=best.predict_proba(X_test)

## performance report

print(classification_report(y_test, pred, target_names=["not delay","delay"]))

## compare different sampling algorithm
from imblearn.over_sampling import SMOTE
from imblearn.under_sampling import OneSidedSelection
from imblearn.ensemble import BalancedRandomForestClassifier


## SMOTE oversampleing 
smote = SMOTE(random_state=17,n_jobs=12)  
psmote_rf=make_pipeline(smote,rf)  


## one side selection undersampleing
oss=OneSidedSelection(random_state=17,n_jobs=12)
poss_rf=make_pipeline(oos,rf)

## balanced random forest
brf = BalancedRandomForestClassifier(max_depth=50, max_features=10,
            max_leaf_nodes=None, min_impurity_decrease=0.0,min_samples_leaf=1,
            min_samples_split=2, min_weight_fraction_leaf=0.0,
            n_estimators=200, n_jobs=-1, oob_score=True, random_state=17,
            verbose=1)


## summarize & evaluate results

from imblearn.metrics import geometric_mean_score


def plotcm(cm, classes, ax,normalize=False,
                          title='Confusion matrix',
                          cmap=plt.cm.Blues):
    print(cm)
    print('')

    ax.imshow(cm, interpolation='nearest', cmap=cmap)
    ax.set_title(title)
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.sca(ax)
    plt.yticks(tick_marks, classes)

    fmt = '.2f' if normalize else 'd'
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        ax.text(j, i, format(cm[i, j], fmt),
                horizontalalignment="center",
                color="white" if cm[i, j] > thresh else "black")

    ax.set_ylabel('True label')
    ax.set_xlabel('Predicted label')


def evaluate(y_test,pred,prob):
    auc= roc_auc_score(y_test, prob[:,1])
    p=average_precision_score(y_test,prob[:,1])
    f1=f1_score(y_test,pred,average="weighted")
    cm_rf = confusion_matrix(y_test, pred)
    print('Confusion Matrix : ')
    print(cm_rf)
    print('Balanced Model performance : ')
    print(classification_report(y_test, pred, target_names=["not delay","delay"]))
    print('imbalanced Model performance : ')
    print(classification_report_imbalanced(y_test, pred))
















