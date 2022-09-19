import pandas as pd
import numpy as np

source_file_path = "masterdata\\dataset_readmissions_datathon_HUS_2022.csv"

master_data = pd.read_csv(source_file_path)

is_readmission_row = master_data[master_data['readmission'] == True]
not_readmission_row = master_data[master_data['readmission'] == False]

patient_ids_with_readmission = is_readmission_row['patientID'].unique()
patient_ids_without_readmission = master_data[~master_data['patientID'].isin(patient_ids_with_readmission)]['patientID'].unique()

print(f"Number of patients without any readmissions {patient_ids_without_readmission.shape[0]}")
print(f"Number of patients with one or more readmissions {patient_ids_with_readmission.shape[0]}")
print(f"sum of the above {patient_ids_without_readmission.shape[0] + patient_ids_with_readmission.shape[0]}")

print(f"Unique cols in master data {master_data['patientID'].unique().shape}")


test_data_patent_ids_with_readmission = np.random.choice(patient_ids_with_readmission, size=1000, replace=False)
test_data_patient_ids_without_readmission = np.random.choice(patient_ids_without_readmission, size=1000, replace=False)

test_data_with_readmission = master_data[master_data['patientID'].isin(test_data_patent_ids_with_readmission)]
test_data_without_readmission = master_data[master_data['patientID'].isin(test_data_patient_ids_without_readmission)]

development_data = master_data[
    (~master_data['patientID'].isin(test_data_patent_ids_with_readmission)) &
    (~master_data['patientID'].isin(test_data_patient_ids_without_readmission))
 ]

development_data.to_csv("development_data.csv")
