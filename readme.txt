README
## 1. System Requirements
### Software Dependencies (with version numbers)
- R (version 4.3.2)
- R base package (version 4.3.2)
- R dataset package (version 4.3.2)
- randomForest
- ConsensusClusterPlus (version 1.66.0)
- EBImage (version 4.44.0)

### Operating Systems
- Windows 10 / 11 (tested)
- Ubuntu 20.04 (tested)
- macOS 12+ (tested)

### Versions the software has been tested on
- R 4.3.2 (primary tested version)

### Any required non-standard hardware
No non-standard hardware is required.

---

## 2. Installation Guide
### Installation Instructions
1. Install R 4.3.2 from https://cran.r-project.org/
2. Install required packages in R console:
install.packages("randomForest")
install.packages("BiocManager")
BiocManager::install("ConsensusClusterPlus")
BiocManager::install("EBImage")
### Typical install time on a "normal" desktop computer
Approximately 10–15 minutes.

---

## 3. Demo
### Instructions to run on data
Run the integrated script in R:
source("CNSDIP.R")
### Expected output
- Random forest model prediction results
- Consensus clustering analysis
- Image processing and feature extraction (EBImage)
- Model performance metrics
- Visualization of screening results

### Expected run time for demo
Approximately 5–10 minutes on a standard desktop computer.

---

## 4. Instructions for use
### How to run the software on your data
All algorithms are integrated into a single script.
Simply run CNSDIP.R in R:
source("CNSDIP.R")

Overview：
The CNDlP.R script encompasses the complete implementation of all core modules, including:
·BAM (Biological Activity Map) generation via the process_map function
·BAM functional clustering analysis (incorporating Consensus Clustering and Hypergeometric Test)
·R interface for invoking the Python-based pharmacological prediction model, enabling CNS (Central Nervous System) prediction score ranking.

Datasets：
The test_raw_data directory contains the following key components:
·BAM_test: Input data for the pharmacological prediction module, used to generate CNS prediction score rankings
·raw_data_test: Raw input for the process_map function to generate BAM for drug1
