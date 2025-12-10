# Naive Bayes Classifier
# Andrew Cashner
# 2025/12/10
#
# Document topic classifier:
#   - Bag of words: Each document is a set of tokens
#   - Goal is to classify each document in one of the topics

from enum import Enum
import numpy as np


training_documents = [
    ["hey", "dog", "what", "up"],
    ["pet", "dog", "bark", "food"],
    ["tree", "bark", "tree", "forest"],
    ["dog", "grooming", "pet", "dog"],
    ["what", "going", "on", "today"],
    ["plant", "tree", "forest", "grow"],
    ["flower", "plant", "grow", "wild"],
]

Label = Enum('Label', [('CHITCHAT', 0), ('PETS', 1), ('NATURE', 2)])

categories = [
    Label.CHITCHAT, 
    Label.PETS,
    Label.NATURE,
    Label.PETS,
    Label.CHITCHAT,
    Label.NATURE,
    Label.NATURE
]

testing_documents = [
    ["field", "grow", "tree", "forest", "plant"],
    ["what", "up", "going", "today"],
    ["dog", "food", "cat", "dog", "pet"],
    ["dog", "pee", "on", "tree", "bark"],
    ["dog", "dog", "dog", "pet"],
    ["flower", "flower", "forest", "tree", "wild", "tree", "grow"],

]

class NaiveBayesClassifier:
    training_data: np.array
    training_labels: np.array

    vocabulary: np.array
    priors: np.array
    conditional_probabilities: np.array

    def __init__(self,
                 training_docs: list[list[str]], 
                 training_labels: list[Label]) -> None:

        self.vocabulary = self.extract_vocabulary(training_docs)
        self.training_data = self.docs_to_matrix(training_docs)
        self.training_labels = self.labels_to_matrix(training_labels)

        self.priors = np.zeros((len([l for l in Label])))
        self.conditional_probabilities = np.zeros((len(self.vocabulary),
                                                 self.priors.shape[0]))

        for label_type in Label:
            self.priors[label_type.value] = self.mle_estimate(
                    self.training_labels, label_type.value) 

            for i in range(0, len(self.vocabulary)):
                docs_in_class = self.training_data[
                        np.where(self.training_labels == label_type.value)]

                self.conditional_probabilities[i][label_type.value] = \
                    self.conditional_probability(docs_in_class, i)

    def __repr__(self):
        return '\n\n'.join([
            "Naive Bayes Classifier:",
            f"training data:\n{self.training_data}",
            f"training labels:\n{self.training_labels}",
            f"vocab:\n{self.vocabulary}",
            f"priors:\n{self.priors}",
            f"conditional probabilities:\n{self.conditional_probabilities}"
            ])
    
    def labels_to_matrix(self, categories : list[Label]) -> np.array:
        return np.array([category.value for category in categories])

    def extract_vocabulary(self, docs: list[list[str]]) -> np.array:
        words = []
        for doc in docs:
            for word in doc:
                if word not in words:
                    words.append(word)
        words.sort()
        return np.array(words)

    def docs_to_matrix(self, docs: list[str]) -> np.array:
        vocab = self.extract_vocabulary(docs)

        vectors = []
        for doc in docs:
            vector = np.zeros((vocab.shape[0]))
            for word in doc:
                word_index = np.where(vocab == word)
                vector[word_index] += 1
            vectors.append(vector)

        return np.array(vectors)

    def mle_estimate(self, labels: np.array, label: int) -> float:
        matches = [this_label for this_label in labels if this_label == label]
        count = len(matches)
        total = labels.shape[0]
        return count / total

    def conditional_probability(self, docs_in_class: np.array, 
                                word_index: int) -> float:
        frequency = np.sum(docs_in_class[:, word_index]) + 1
        complement = np.sum(np.sum(np.delete(docs_in_class, 
                                             word_index, axis=1) + 1))
        return frequency / complement

    def word_probability(self, word: str, label: Label) -> float:
        word_index = np.where(self.vocabulary == word)[0][0]
        return np.log(
                self.conditional_probabilities[word_index][label.value])

    def predict(self, testing_docs: list[list[str]]) -> list[Label]:
        scores = np.zeros((len(testing_docs), len([l for l in Label])))
        predicted_labels: list[Label] = []

        for doc_index in range(0, len(testing_docs)):
            for label_type in Label:
                scores[doc_index][label_type.value] = \
                        np.log(self.priors[label_type.value])

                word_scores = [self.word_probability(word, label_type) 
                               for word in testing_docs[doc_index]
                               if word in self.vocabulary]
                scores[doc_index][label_type.value] += sum(word_scores)

            predicted_labels.append(Label(np.argmax(scores[doc_index])))

        print(f"\nLabel probabilities per doc:\n{scores}")
        return predicted_labels


def main():
    classifier = NaiveBayesClassifier(training_documents, categories)
    print(classifier)

    predicted_labels = classifier.predict(testing_documents)
    print("\nPredictions for test data:")
    print([label.name.lower() for label in predicted_labels])


if __name__ == '__main__':
    main()
