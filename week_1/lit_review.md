# MIMIC Dataset Litreature Review
1. [Benchmark of Deep Learning Models on Large
Healthcare MIMIC Datasets](https://arxiv.org/abs/1710.08531)
    - Aim: Predict mortality, length of stay, and ICD-9 code groups using Deep Learning models, ensemble of machine learning models SAPS II and SOFA scores.
    - Results: Show that deep learning models consistently outperform all the other approaches especially when the `raw' clinical time series data is used as input features to the models.
    - Insights:
        - MIMIC-III contains data associated with 53 423 distinct hospital admissions for adult patients (aged 15 years or above) and 7870 neonates admitted to an ICU at the BIDMC. The data covers 38 597 distinct adult patients with 49 785 hospital admissions.
        - **Cohort Selection**: The authors identied all the adult patients by using the age recorded at the time of ICU admission. Following previous studies, in their work, all the patients whose age was > 15 years at the time of ICU admission is considered s an adult. Second, for each patient, they only use their first admission in their benchmark datasets, and dropped all their later admissions. This was done to prevent possible information leakage in the analysis. 
        - **Data Cleaning**: The data extracted from MIMIC-III database has lots of erroneous entries due to noise, missing values, outliers, duplicate or incorrect records, clerical mistakes etc.
            1. There is inconsistency in the recording (units) of certain variables. For example, some of the prescriptions are recorded in 'dose' and in 'mg' units; while some variables in chartevents and labevents tables are recorded in both numeric and string data type.
            2. Some variables have multiple values recorded at the same time. 
            3. For some variables the observation was recorded as a range rather than a single measurement.
        - **Feature Selection and Extraction**: The authors select three sets of features: 
            1. Feature Set A: feature set consists of the 17 features used in the calculation of the SAPS-II score.
            2. Feature set consists of the 20 features related to the 17 features used in SAPS-II score.
            3. Feature set consists of 135 raw features selected from the 5 tables (inputevents, outputevents, chartevents, labevents and prescriptions)
        - **Mortality Prediction**
            - In-hospital mortality prediction, Short-term mortality prediction and Long-term mortality prediction
        - **Length of Stay Prediction**: The authors predict the length of stay for each admission. They define the length of stay of an admission as total duration of hospital stay, i.e. the length of time interval between hospital admission and discharge from the hospital.

2. [Open-access MIMIC-II Database for Intensive Care Research](https://www.ncbi.nlm.nih.gov/pubmed/22256274)
    - The data harnessed in MIMIC-II was collected from the ICUs of Beth Israel Deaconess Medical Center from 2001 to 2008 and represent 26,870 adult hospital admissions (version 2.6). MIMIC-II consists of two major components: clinical data and physiological waveforms. The clinical data, which include patient demographics, intravenous medication drip rates, and laboratory test results, were organized into a relational database. The physiological waveforms, including 125 Hz signals recorded at bedside and corresponding vital signs, were stored in an open-source format. MIMIC-II data were also deidentified in order to remove protected health information.

3. [Using freely accessible databases for laboratory medicine
research: experience with MIMIC database](http://jlpm.amegroups.com/article/download/3685/4380) 
    - Limitations of research based on MIMIC database: 
        - The MIMIC database has some advantages, including the extremely large sample size and data available for each patient during hospitalization, as well as long-term follow up time. However, it has the following weaknesses: 
            - Although the data in MIMIC database are prospectively collected, all MIMIC based studies are of a retrospective design. 
            - Only routine laboratory tests are recorded in MIMIC database, thus it is impossible for the researcher to investigate the clinical value of novel biomarkers
            - The database only records the all-cause mortality and hospital mortality. It is impossible to investigate the association between laboratory tests and disease specific mortality

4. [Mapping Patient Trajectories using Longitudinal Extraction and Deep Learning in the
MIMIC-III Critical Care Database](https://www.biorxiv.org/content/biorxiv/early/2017/08/17/177428.full.pdf)
    - Health care providers often perform and record actions in small batches over time. The authors extract these care events and form a sequence providing a trajectory for a patient’s interactions with the health care system.
    - These care offer a basic heuristic for the level of attention a patient receives from health care providers. 
    - Thus, the authors show that is possible to learn meaningful embeddings from the care events using two deep learning techniques, unsupervised autoencoders and long short-term memory networks.
    - Insights:
        - **Extracting Care Events from MIMIC**: The authors divided the MIMIC database into 4 groups:
            1. Static data that does not change over the course of an admission (i.e. demographic data)
            2. Actions performed by health care providers that have a specific time associated with them (i.e. laboratory events)
            3. Actions performed by health care providers that only have a date associated with them (i.e.oral medications)
            4. Streaming data measured on a per-minute basis (i.e. heart rate)
        - **Predicting Survival Using Care Events**
5. [Predicting Hospital Length of Stay using Neural
Networks on MIMIC III Data](https://www.researchgate.net/profile/Thanos_Gentimis/publication/324177552_Predicting_Hospital_Length_of_Stay_Using_Neural_Networks_on_MIMIC_III_Data/links/5ad62f60458515c60f55f736/Predicting-Hospital-Length-of-Stay-Using-Neural-Networks-on-MIMIC-III-Data.pdf)
    - In this paper the authors explore the use of neural networks for predicting the total length of stay for patients with various diagnoses based on selected general characteristics. A neural network was trained to predict whether patient stay will be long (> 5 days), or short (≤ 5 days) as of the time the patient leaves the ICU unit. 
6. [Causal Regularization](https://arxiv.org/pdf/1702.02604.pdf)
    -  Solve clinical predictive modeling problems using large EHR datasets: one on heart failure onset prediction and another on mortality prediction using the publicly available MIMIC III dataset.


