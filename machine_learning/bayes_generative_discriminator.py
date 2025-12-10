# Generative Bayes Discriminators
# Gaussian discriminant analysis: Linear and Quadratic
#
# Andrew Cashner
# 2025/12/10
#
# Drawn from https://medium.com/the-quantastic-journal/a-gentle-introduction-to-gaussian-discriminant-analysis-gaussian-naive-bayes-linear-discriminant-e54d54f89c5a

import numpy as np

class LdaClassifier:
    training_data: np.array
    training_labels: np.array

    priors: np.array
    means: np.array
    covariance: np.array

    def __init__(self,
                 training_data: np.array,
                 training_labels: np.array) -> None:

        self.training_data = training_data
        self.training_labels = training_labels

        num_samples = training_data.shape[0]
        num_features = training_data.shape[1]
        classes = np.unique(training_labels)
        num_classes = len(classes)

        self.priors = np.zeros(num_classes)
        self.means = np.zeros((num_classes, num_features))
        self.covariance = np.zeros((num_features, num_features))
        scatter = np.zeros((num_features, num_features))

        for i, label in enumerate(classes):
            samples_in_class = self.training_data[
                    self.training_labels == label]
            num_class_samples = samples_in_class.shape[0]

            self.priors[i] = num_class_samples / num_samples

            self.means[i, :] = np.mean(samples_in_class, axis=0)

            distance = samples_in_class - self.means[i, :]
            scatter += np.dot(distance.T, distance)

        self.covariance = scatter / (num_samples - num_classes)

    def __repr__(self):
        return '\n\n'.join([
            "Linear Discriminant Analysis Classifier:",
            f"training data:\n{self.training_data}",
            f"training labels:\n{self.training_labels}",
            f"priors:\n{self.priors}",
            f"means:\n{self.means}",
            f"covariance:\n{self.covariance}"
            ])

    def predict(self, data: np.array) -> np.array:
        num_samples = data.shape[0]
        predicted_labels = np.zeros(num_samples)

        for sample_index in range(num_samples):
            this_data = data[sample_index]
            posteriors = []

            for class_index in range(len(self.priors)):
                prior = np.log(self.priors[class_index])
                inv_covariance = np.linalg.inv(self.covariance)
                mean_diff = this_data - self.means[class_index]
                likelihood = -0.5 * mean_diff.T @ inv_covariance @ mean_diff
                posterior = prior + likelihood
                posteriors.append(posterior)

            predicted_labels[sample_index] = np.argmax(posteriors)
 
        return predicted_labels

class QdaClassifier:
    training_data: np.array
    training_labels: np.array

    priors: np.array
    means: np.array
    covariances: np.array

    def __init__(self,
                 training_data: np.array,
                 training_labels: np.array) -> None:

        self.training_data = training_data
        self.training_labels = training_labels

        num_samples = training_data.shape[0]
        num_features = training_data.shape[1]
        classes = np.unique(training_labels)
        num_classes = len(classes)

        self.priors = np.zeros(num_classes)
        self.means = np.zeros((num_classes, num_features))
        self.covariances = np.zeros((num_classes, 
                                     num_features, 
                                     num_features))
        scatter = np.zeros((num_features, num_features))

        for i, label in enumerate(classes):
            samples_in_class = self.training_data[
                    self.training_labels == label]
            num_class_samples = samples_in_class.shape[0]

            self.priors[i] = num_class_samples / num_samples

            self.means[i, :] = np.mean(samples_in_class, axis=0)

            self.covariances[i, :, :] = np.cov(samples_in_class, 
                                               rowvar=False)

    def __repr__(self):
        return '\n\n'.join([
            "Quadratic Discriminant Analysis Classifier:",
            f"training data:\n{self.training_data}",
            f"training labels:\n{self.training_labels}",
            f"priors:\n{self.priors}",
            f"means:\n{self.means}",
            f"covariances:\n{self.covariances}"
            ])

    def predict(self, data: np.array) -> np.array:
        num_samples = data.shape[0]
        num_labels = len(np.unique(self.training_labels))
        posteriors = np.zeros((num_samples, num_labels))
        predicted_labels = np.zeros(num_samples)

        for sample_index in range(num_samples):
            this_data = data[sample_index]

            for class_index in range(len(self.priors)):
                prior = np.log(self.priors[class_index])
               
                this_covariance = self.covariances[class_index]
                inv_covariance = np.linalg.inv(this_covariance)
                mean_diff = this_data - self.means[class_index]
                
                likelihood = -0.5 * mean_diff.T @ inv_covariance @ mean_diff
                
                posterior = prior + likelihood
                posteriors[sample_index][class_index] = posterior

            predicted_labels[sample_index] = \
                    np.argmax(posteriors[sample_index])

        print(posteriors)
        print(predicted_labels)
 
        return predicted_labels


def main():
    # Temperature and humidity
    train_data = np.array([
        [65, 51],
        [99, 100],
        [80, 90],
        [60, 40]
    ])

    test_data = np.array([
        [80, 30],
        [70, 100],
        [100, 100],
        [90, 20]
    ])

    # Comfortable or not
    labels = np.array([1, 0, 0, 1])

    print("\nLINEAR DISCRIMINANT ANALYSIS\n")
    classifier = LdaClassifier(train_data, labels)
    print(classifier)
 
    predicted_labels = classifier.predict(test_data)
    print("\nPredictions for test data:")
    print(["Comfortable" if label == 1 else "Uncomfortable" 
           for label in predicted_labels])

    print("\n\nQUADRATIC DISCRIMINANT ANALYSIS\n")
    # Locations (concentric)
    train_data = np.array([
        [-10, 0],
        [-5, 5],
        [0, 10],
        [5, 5],
        [10, 0],
        [-1, 0],
        [-0.5, 0.5],
        [0, 1],
        [0.5, 0.5],
        [1, 0]
    ])

    # Near to (1) or far from (0) origin
    labels = np.array([
        1, 1, 1, 1, 1, 
        0, 0, 0, 0, 0
    ])

    test_data = np.array([
        [0.0, 0.6],
        [-0.7, 0.1],
        [0, 0.8],
        [10, 6],
        [6, 6],
        [0.1, 0.2],
    ])


    classifier = QdaClassifier(train_data, labels)
    print(classifier)

    print(f"Test data:\n{test_data}")
    predicted_labels = classifier.predict(test_data)
    print("\nPredictions for test data:")
    print(["Far" if label == 1 else "Near" 
           for label in predicted_labels])

if __name__ == '__main__':
    main()
