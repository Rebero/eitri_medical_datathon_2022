import tensorflow as tf
import keras 
from keras import layers, models, callbacks, optimizers, losses, metrics

import i_fall


def build_model():

    bed_day_dep = layers.Input(shape=(1, ), dtype='float32', name='bed_day_dep')  
    DRG = layers.Input(shape=(1, ), dtype='float32', name='DRG')
    gender = layers.Input(shape=(1, ), dtype='int32', name='gender')
    age_group = layers.Input(shape=(1, ), dtype='int32', name='age_group')
    degree_of_urgency = layers.Input(shape=(1, ), dtype='int32', name='degree_of_urgency')
    mainDiagICD10Chapter = layers.Input(shape=(1, ), dtype='int32', name='mainDiagICD10Chapter')
    diagnosis_code_1 = layers.Input(shape=(1, ), dtype='int32', name='diagnosis_code_1')

    gender_vec = layers.Embedding(3, 2)(gender)
    age_group_vec = layers.Embedding(i_fall.num_age_group, 10)(age_group)
    degree_of_urgency_vec = layers.Embedding(5, 5)(degree_of_urgency)
    mainDiagICD10Chapter_vec = layers.Embedding(i_fall.num_mainDiagICD10Chapter, i_fall.num_mainDiagICD10Chapter)(mainDiagICD10Chapter)
    diagnosis_code_1_vec = layers.Embedding(i_fall.num_diagnosis_codes, 64)(diagnosis_code_1)

    x = layers.Concatenate()([
        tf.stack([bed_day_dep], axis=1), 
        tf.stack([DRG], axis=1),
        gender_vec,
        age_group_vec,
        degree_of_urgency_vec,
        mainDiagICD10Chapter_vec,
        diagnosis_code_1_vec])

    x = layers.Flatten()(x)
    x = layers.Dense(units=x.shape[-1])(x)
    x = layers.LayerNormalization()(x)
    x = layers.Dropout(0.15)(x)

    y = x

    for u in [512, 512, 256, 128]:
        x = layers.Dense(units=u, activation='LeakyReLU')(x)
        x = x + layers.Dense(units=x.shape[-1])(y)
        x = layers.Dropout(0.15)(x)

    readmission_prediction = layers.Dense(units=1, activation='sigmoid', name='readmission_prediction')(x)

    i_fall_model = models.Model(
        inputs={
            'bed_day_dep': bed_day_dep, 
            'DRG': DRG, 
            'gender': gender, 
            'age_group': age_group,
            'degree_of_urgency': degree_of_urgency, 
            'mainDiagICD10Chapter': mainDiagICD10Chapter, 
            'diagnosis_code_1': diagnosis_code_1},
        outputs=readmission_prediction,
        name='i_fall'
    )

    i_fall_model.compile(
        optimizer=optimizers.Adam(),
        loss=losses.BinaryCrossentropy(),
        metrics=metrics.BinaryAccuracy()
    )

    i_fall_model.summary()

    train_data = i_fall.IFallData(is_validation=False)
    valid_data = i_fall.IFallData(is_validation=True)

    i_fall_model.fit(
        x=train_data,
        steps_per_epoch=i_fall.EPOCH_SIZE,
        epochs=100,
        validation_data=valid_data,
        callbacks=[
            callbacks.TensorBoard("logs\\run05"),
            callbacks.TerminateOnNaN(),
            callbacks.ModelCheckpoint("checkpoints\\model05")
        ]
    )


def gather_stats():
    model = models.load_model("checkpoints\\model05")
    model.summary()

    valid_data = i_fall.IFallData(is_validation=True).get_all()
    valid_data_result = model(valid_data[0], training=False)

    

    print(valid_data_result)


if __name__ == "__main__":

    gather_stats()

    # build_model()
