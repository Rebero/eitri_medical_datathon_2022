import numpy as np
import tensorflow as tf
import keras
from dataset_process import get_split_train_validation
import random
import os
import pickle as pkl


EPOCH_SIZE = 500
BACTCH_SIZE = 32

num_mainDiagICD10Chapter = 23
num_mainDiagBlock = 240
num_age_group = 10
num_diagnosis_codes = 7779


class IFallData(keras.utils.Sequence):

    def __init__(self, is_validation=False):

        self.is_validation = is_validation

        if is_validation:
            _, self.dataframe = get_split_train_validation()
            cache_name = "validation_input_output_cache.pkl"
        else:
            self.dataframe, _ = get_split_train_validation()
            cache_name = "train_input_output_cache.pkl"

        self.dataframe.fillna(0, inplace=True)


        if not os.path.exists(cache_name):
            print("generating tuples")
            self.input_outputs = [self.gen_input_output(row) for _, row in self.dataframe.iterrows()]
            with open(cache_name, 'wb') as che:
                pkl.dump(self.input_outputs, che)
        else:
            print("reading tuples")
            with open(cache_name, 'rb') as che:
                self.input_outputs = pkl.load(che)


        self.is_readmission = [q for q in self.input_outputs if q[1] == 1]
        self.non_readmission = [q for q in self.input_outputs if q[1] == 0]

        print("num readmission examples: " + str(len(self.is_readmission)))
        print("num non readmission examples: " + str(len(self.non_readmission)))

    def gen_input_output(self, possible_readmission_row):

        patient_hist = self.dataframe[self.dataframe['patientID'] == possible_readmission_row['patientID']]

        patient_hist = patient_hist[patient_hist['timestamp_in'] > possible_readmission_row['timestamp_out']]


        last_entry = patient_hist['timestamp_in'].min()
        # print(last_entry)

        patient_hist = patient_hist[patient_hist['timestamp_out'] == last_entry]

        if patient_hist.shape[0] != 1:
            is_readmission = 0
        else:
            is_readmission = int(patient_hist['readmission'].max())


        # print(patient_hist)

        ret = (
            {
                'bed_day_dep': float(possible_readmission_row['bed_day_dep']), 
                'DRG': float(possible_readmission_row['DRG']), 
                'gender': int(possible_readmission_row['gender']), 
                'age_group': int(possible_readmission_row['age_group']),
                'degree_of_urgency': int(possible_readmission_row['degree_of_urgency']), 
                'mainDiagICD10Chapter': int(possible_readmission_row['mainDiagICD10Chapter']), 
                'diagnosis_code_1': int(possible_readmission_row['diagnosis_code_1']), 
            },
            is_readmission
        )

        return ret

    def get_all(self):
        ret_input = {
            'bed_day_dep': [], 
            'DRG': [], 
            'gender': [], 
            'age_group': [],
            'degree_of_urgency': [], 
            'mainDiagICD10Chapter': [], 
            'diagnosis_code_1': [], 
        }

        ret_truth = []

        for s in (self.is_readmission + self.non_readmission):
            ret_truth.append(s[1])

            for k, v in s[0].items():
                ret_input[k].append(v)

        ret_truth = np.array(ret_truth).astype('int32')
        ret_input['bed_day_dep'] = np.array(ret_input['bed_day_dep']).astype('float32')
        ret_input['DRG'] = np.array(ret_input['DRG']).astype('float32')
        ret_input['gender'] = np.array(ret_input['gender']).astype('int32')
        ret_input['age_group'] = np.array(ret_input['age_group']).astype('int32')
        ret_input['degree_of_urgency'] = np.array(ret_input['degree_of_urgency']).astype('float32')
        ret_input['mainDiagICD10Chapter'] = np.array(ret_input['mainDiagICD10Chapter']).astype('int32')
        ret_input['diagnosis_code_1'] = np.array(ret_input['diagnosis_code_1']).astype('int32')

        return ret_input, ret_truth


    def __getitem__(self, _):
        ret_input = {
            'bed_day_dep': [], 
            'DRG': [], 
            'gender': [], 
            'age_group': [],
            'degree_of_urgency': [], 
            'mainDiagICD10Chapter': [], 
            'diagnosis_code_1': [], 
        }

        ret_truth = []

        num_readmissions = BACTCH_SIZE//2

        samples = random.sample(self.is_readmission, num_readmissions) + random.sample(self.non_readmission, BACTCH_SIZE - num_readmissions)

        for s in samples:
            ret_truth.append(s[1])

            for k, v in s[0].items():
                ret_input[k].append(v)

        ret_truth = np.array(ret_truth).astype('int32')
        ret_input['bed_day_dep'] = np.array(ret_input['bed_day_dep']).astype('float32')
        ret_input['DRG'] = np.array(ret_input['DRG']).astype('float32')
        ret_input['gender'] = np.array(ret_input['gender']).astype('int32')
        ret_input['age_group'] = np.array(ret_input['age_group']).astype('int32')
        ret_input['degree_of_urgency'] = np.array(ret_input['degree_of_urgency']).astype('float32')
        ret_input['mainDiagICD10Chapter'] = np.array(ret_input['mainDiagICD10Chapter']).astype('int32')
        ret_input['diagnosis_code_1'] = np.array(ret_input['diagnosis_code_1']).astype('int32')

        return ret_input, ret_truth

# um_mainDiagICD10Chapter = 23
#num_mainDiagBlock = 240
#num_age_group = 10
#num_diagnosis_codes = 7779

    def __len__(self):
        return EPOCH_SIZE


if __name__ == '__main__':

    ifd = IFallData(is_validation=True)

    print(ifd.__getitem__(1))






