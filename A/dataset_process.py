import pandas as pd
import numpy as np
import json


def generate_dicts():
    all_data = pd.read_csv("masterdata\\dataset_readmissions_datathon_HUS_2022.csv")


    mainDiagICD10Chapter_dict = {s: i for i, s in enumerate(all_data['mainDiagICD10Chapter'].fillna('').unique())}
    print(mainDiagICD10Chapter_dict)

    mainDiagBlock_dict = {s: i for i, s in enumerate(all_data['mainDiagBlock'].fillna('').unique())}
    print(len(mainDiagBlock_dict))

    age_group_dict = {s: i for i, s in enumerate(all_data['age_group'].fillna('').unique())}
    print(age_group_dict)

    diagnosis_codes = pd.concat([
        all_data[f'diagnosis_code_{i}'].fillna('') for i in range(1, 10)], axis=0).unique()


    print(diagnosis_codes)

    diagnosis_codes_dict = {s: i for i, s in enumerate(diagnosis_codes)}


    print(diagnosis_codes_dict)


    with open("embedding_dicts.json", 'w') as ed:
        json.dump({
            'mainDiagICD10Chapter': mainDiagICD10Chapter_dict,
            'mainDiagBlock': mainDiagBlock_dict,
            'age_group': age_group_dict,
            'diagnosis_codes': diagnosis_codes_dict
        }, ed)


def embed_data(data_path):

    with open("embedding_dicts.json", 'r') as ed:
        embedding_dicts = json.load(ed)

    dataset = pd.read_csv(data_path)

    dataset['mainDiagICD10Chapter'] = [embedding_dicts['mainDiagICD10Chapter'][row['mainDiagICD10Chapter']] for _, row in dataset.fillna('').iterrows()]

    dataset['mainDiagBlock'] = [embedding_dicts['mainDiagBlock'][row['mainDiagBlock']] for _, row in dataset.fillna('').iterrows()]

    dataset['age_group'] = [embedding_dicts['age_group'][row['age_group']] for _, row in dataset.fillna('').iterrows()]

    for i in range(1, 10):
        print(f'id' + str(i))
        dataset[f'diagnosis_code_{i}'] = [embedding_dicts['diagnosis_codes'][row[f'diagnosis_code_{i}']] for _, row in dataset.fillna('').iterrows()]

    return dataset


def get_embedding_sizes():

    with open("embedding_dicts.json", 'r') as ed:
        embedding_dicts = json.load(ed)

        print('mainDiagICD10Chapter: ' + str(len(embedding_dicts['mainDiagICD10Chapter'])))
        print('mainDiagBlock: ' + str(len(embedding_dicts['mainDiagBlock'])))
        print('age_group: ' + str(len(embedding_dicts['age_group'])))
        print('diagnosis_codes: ' + str(len(embedding_dicts['diagnosis_codes'])))

def create_embeddings_csv():

    thedf = embed_data("development_data.csv")
    thedf.to_csv("development_data_embedded.csv")


def get_split_train_validation():
    dev_data = pd.read_csv("development_data_embedded.csv")

    unique_patients = dev_data['patientID'].unique()
    val_set_ids = np.random.choice(unique_patients, len(unique_patients)//10)

    val_set = dev_data[dev_data['patientID'].isin(val_set_ids)]
    train_set = dev_data[~dev_data['patientID'].isin(val_set_ids)]

    return train_set, val_set


if __name__ == '__main__':

    ts, vs = get_split_train_validation()

    print(ts)
    print(vs)

    
