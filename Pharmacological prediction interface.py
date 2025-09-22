import os
import sys
import numpy as np
import pandas as pd
import cv2
import torch
import joblib
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score
from sklearn.calibration import CalibratedClassifierCV
from sklearn.decomposition import PCA



class CNSPredictor:
    def __init__(self, config):
        """Initialize the CNS prediction system with configuration"""
        self.config = config
        self.device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
        self.model = None
        np.set_printoptions(threshold=sys.maxsize)

    def load_and_preprocess_data(self):
        """Load and preprocess image and label data"""
        # Load metadata
        df = pd.read_excel(self.config['smile_dir'], sheet_name='Sheet1')

        # Process training data
        train_names = os.listdir(self.config['train_img_dir'])
        train_images, train_labels = self._process_images_and_labels(
            train_names, df, self.config['train_img_dir'], is_train=True)

        # Process test data
        test_names = os.listdir(self.config['test_img_dir'])
        test_images, test_labels = self._process_images_and_labels(
            test_names, df, self.config['test_img_dir'], is_train=False)

        # Apply dimensionality reduction
        x_train, x_test = self._apply_dimensionality_reduction(train_images, test_images)

        return x_train, train_labels, x_test, test_labels, test_names

    def _process_images_and_labels(self, img_names, df, img_dir, is_train=True):
        """Helper function to process images and extract labels"""
        img_list = []
        label_list = []

        for name in img_names:
            # Extract label
            label = self._extract_label(name, df, is_train)
            if label is not None:  # Skip samples with invalid labels
                label_list.append(label)

                # Load and preprocess image
                img_path = os.path.join(img_dir, name)
                img = cv2.imread(img_path)
                img = img.reshape(1, -1)
                img_list.append(img)

        return np.array(img_list), label_list

    def _extract_label(self, filename, df, is_train):
        """Extract label from filename or metadata"""
        if is_train and filename.split('.')[0][0].isalpha():
            return 1 if filename[0] == 'N' else 0

        try:
            table_index = filename.split('.')[0]
            row_num = df.index[df['TableIndex'] == int(table_index)].tolist()
            atc_code = df['ATC-Code'][row_num].tolist()[0]

            if atc_code != 0:
                return 1 if atc_code[0] == 'N' else 0
            if not is_train:
                return 0

        except Exception as e:
            print(f"Error processing {filename}: {str(e)}")
            return None if is_train else 0

    def _apply_dimensionality_reduction(self, train_images, test_images):
        """Apply PCA for dimensionality reduction"""
        train_images = train_images.reshape(len(train_images), -1)
        test_images = test_images.reshape(len(test_images), -1)

        pca = PCA(n_components=20)
        x_train = pca.fit_transform(train_images)
        x_test = pca.transform(test_images)

        return x_train.tolist(), x_test.tolist()

    def train_model(self, x_train, y_train):
        """Train and calibrate Random Forest model"""
        rf = RandomForestClassifier(
            n_estimators=28,
            random_state=10,
            max_depth=7,
            max_leaf_nodes=100,
            max_features=4
        )

        # Train and track accuracy
        accuracy_history = []
        for i in range(1, 29):
            rf.n_estimators = i
            rf.fit(x_train, y_train)
            y_pred = rf.predict(x_train)
            accuracy_history.append(accuracy_score(y_train, y_pred))

        # Calibrate classifier
        calibrated_rf = CalibratedClassifierCV(rf, method='sigmoid', cv='prefit')
        calibrated_rf.fit(x_train, y_train)

        self.model = calibrated_rf
        return accuracy_history

    def evaluate_model(self, model_path, x_test, y_test, test_names, threshold=0.65):
        """Evaluate model performance and generate predictions"""
        model = joblib.load(model_path)

        # Generate predictions
        y_pred = model.predict(x_test)
        y_score = model.predict_proba(x_test)

        # Apply threshold
        y_pred_thresh = [1 if score[1] > threshold else 0 for score in y_score]

        # Create results dictionary
        results = {
            'names': test_names,
            'predictions': y_pred_thresh,
            'scores': [score[1] for score in y_score],
            'true_labels': y_test
        }

        # Calculate accuracy
        accuracy = accuracy_score(y_test, y_pred_thresh)

        return results, accuracy

    def save_results(self, results, output_path):
        """Save prediction results to file"""
        df = pd.DataFrame({
            'Name': results['names'],
            'Prediction': results['predictions'],
            'Score': results['scores'],
            'True_Label': results['true_labels']
        })
        df.to_excel(output_path, index=False)


# Configuration dictionary
config = {
    'smile_dir': "data/test.xlsx",
    'train_img_dir': "data/train/",
    'test_img_dir': "data/test/",
    'model_path': "data/model/",
    'output_path': "data/output/"
}

# Example usage
if __name__ == "__main__":
    # Initialize predictor
    predictor = CNSPredictor(config)
    # Load and preprocess data
    x_train, y_train, x_test, y_test, test_names = predictor.load_and_preprocess_data()
    # Train model (optional)
    accuracy_history = predictor.train_model(x_train, y_train)
    # Evaluate model
    results, accuracy = predictor.evaluate_model(
        config['model_path'],
        x_test,
        y_test,
        test_names
    )
    # Save results
    predictor.save_results(results, config['output_path'])
    print(f"Model Accuracy: {accuracy:.4f}")
