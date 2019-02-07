import numpy as np
from sklearn import svm

from sklearn.model_selection import KFold
import pandas as pd
import random


# Attention: You're not allowed to use the model_selection module in sklearn.
#            You're expected to implement it with your own code.
# from sklearn.model_selection import GridSearchCV

class SVMNovelClassifer:

    def __init__(self):
        random.seed(0)

    def load_data(self, csv_fpath):

        data = pd.read_csv(csv_fpath)

        col_names_x = ['word_counts_vec', 'sent_counts_vec', 'para_counts_vec', 'commas_vec', 'sent_comma_freq_vec',
                       'para_comma_freq_vec', 'words_per_sentence_vec', 'words_per_paragraph_vec', 'sents_per_paragraph_vec',
                       'consecutive_counts_vec', 'consecutive_repeat_freq_vec', 'syll_and_word_freq_vec',
                       'polsyll_and_word_freq_vec', 'syll_and_sent_freq_vec', 'polsyll_and_sent_freq_vec', 'unique_counts_vec',
                       'type_token_ratio_vec', 'mean_usage_frequency_vec','median_MATTR', 'object_freq', 'relationship_freq',
                       'time_freq', 'self_freq', 'perceive_freq', 'i_freq','top_ten_freq','dialogue_freq',
                       'question_vec', 'exclamation_vec','sentiment_vec', 'label2']

        data.columns = col_names_x

        y = data['label2']
        del data['label2']

        return data, y

    def train_and_select_model(self, training_csv):

        X, Y = self.load_data(training_csv)

        X = np.asarray(X)
        Y = np.asarray(Y)

        # Attention: Write your own hyper-parameter candidates.

        param_set = [
                     {'kernel': 'linear', 'C': 1.0},
                     {'kernel': 'rbf'},
                     {'kernel': 'poly', 'degree': 3.0},
                     {'kernel': 'linear', 'C': 10.0},
        ]

        kf = KFold(n_splits=5)

        kf.get_n_splits(X)

        soft_linear_scores = []
        hard_linear_scores = []
        rbf_scores = []
        poly_scores = []


        for train_index, test_index in kf.split(X):
            X_train, X_test = X[train_index], X[test_index]
            y_train, y_test = Y[train_index], Y[test_index]

            soft_linear_model = svm.SVC(kernel='linear', C=1.0).fit(X_train, y_train)
            hard_linear_model = svm.SVC(kernel='linear', C=10.0).fit(X_train, y_train)
            rbf = svm.SVC(kernel='rbf').fit(X_train, y_train)
            poly_model = svm.SVC(kernel='poly', C=1.0).fit(X_train, y_train)


            soft_linear_scores.append(soft_linear_model.score(X_test, y_test))
            hard_linear_scores.append(hard_linear_model.score(X_test, y_test))
            rbf_scores.append(rbf.score(X_test, y_test))
            poly_scores.append(poly_model.score(X_test, y_test))

        for score in soft_linear_scores:
            print score

        best_model = svm.SVC(kernel='linear', C=1.0).fit(X[0:40], y_train[0:40])

        return best_model, 2.0


if __name__ == '__main__':

    training_csv = "normalized.csv"

    clf = SVMNovelClassifer()
    # load_data(clf, training_csv)

    trained_model, cv_score = clf.train_and_select_model(training_csv)




