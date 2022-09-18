import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split
from sklearn.decomposition import PCA
from sklearn.metrics import accuracy_score
from sklearn import svm

def main():
    source_filename = r"C:\Users\datathonadmin\Documents\Datathon"

    df = pd.read_csv(
        source_filename + r"\raw2.csv",
        low_memory=False,
        sep=';'
    )

    
    ## Exploring raw data
    # print(df.head)
    # print(f"Dataset size: {df.size}")
    # print(f"Dataset size: {df.shape}")



    """ Checking for nan values / columns works in prelimineray"""

    # for column in df.columns:
    #     col_nans[f'{column}'] = df[f'{column}'].isna().sum()

    # # print(df.index)
    # for index in df.index:
    #     ind_nans[f'{index}'] = df.loc[[index]].isna().sum()

    # for k, v in ind_nans.items():
    #     print(f"{k}: {v}")
    # overview = pd.DataFrame.from_dict(col_nans)

    # overview[]

    # plt.plot(x = col_nans.values, y = col_nans.keys)
    # plt.plot(x = ind_nans.values, y = ind_nans.keys)



    #----------------------------------
    # Preparing lists of columns identified through expert opinion and their respective variables
    #----------------------------------

    identifiers = [ # comments describe what values are within some columns giving initial errors to fix them later where needed
        "newid",
        "tcvhinf",  
        "tcvich",   # nan, ja, nei
        "age",
        "tcvtia",
        "sex",      # female, male, nan
        "slagt",    # strings
        "tmrs", 
        "mrs", 
        "thjtinf",  # nan, ja, nei
        "tcarotis",
        "tpkar",    # nan, ja, nei
        "tht",      # nan, ja, nei
        "tdiab",    # nan, ja, nei
        "taflp",    # nan, ja, nei
        "taflk",    # nan, ja, nei
        "tdemens",
        "tkols",
        "tidiktk",
        "tidiktd",  # nan, dates
        "tsivil",   # nan, ja, nei
        "bivt",     # nan, ja, nei
        "bivtdt",   # nan, + date
        "bivtkl",
        "bia",      # nan, ja, nei
        "biadt",    # nan , date
        "biakl",
        "tidsykd",
        "tidsykk", 
        "trokasig",
        
    ]    
        
    nan_date = [
        "bivtdt",   # nan, date
        "biadt",    # nan, date
        "tidiktd",  # nan, date
    ]

    yes_no_nan = [
        "tpkar",    # nan, ja, nei
        "tht",      # nan, ja, nei
        "tdiab",    # nan, ja, nei
        "taflp",    # nan, ja, nei
        "taflk",    # nan, ja, nei
        "thjtinf",  # nan, ja, nei
        "tcvich",   # nan, ja, nei
        "tsivil",   # nan, ja, nei
        "bivt",     # nan, ja, nei
        "bia",      # nan, ja, nei
    ]




    #----------------------------------
    # creating the subset of data of interest
    #----------------------------------


    working_df = df[identifiers].copy(deep=True)
    del df

    numeric_columns = [col for col in working_df.columns if col not in yes_no_nan or col not in nan_date]
    
    numeric_columns.remove("newid")     # removed newid as this subject id is needed to check reoccurence and shouldn't be used in nan cleaning section
    
    count_subjects = len(working_df["newid"].unique())
    
    columns_with_object = []




    #----------------------------------
    # exploratory work
    #----------------------------------

    # # get columns with object data types

    # for col in working_df.columns:
    #     if working_df[col].dtypes == object:
    #         columns_with_object.append(col)
        

    # # print unique value of object columns 

    # for col in working_df:
    #     if col in columns_with_object:
    #         print()
    #         print(col)
    #         print()
    #         print(working_df[col].unique())


    # # Used to find what the unique col values are

    # for col in columns_with_object:
    #     print(col)
    #     print(working_df[col])
    #     working_df[col].unique()
    
    
    
    
    """ Change nan/non numeric values to useful values """
    
    print("before nan treatment shape:")
    print(working_df.head)
    print(working_df.shape)
    
    #----------------------------------
    #  columns with nan_yes_no
    #----------------------------------

    # defining dict for replacing yes/no values
    dict_replace_nan_yes_no = {
        "nei": 0, 
        "ja": 1
    }


    nan_to_0 = [  # columns where nan becomes 0
        "tpkar",
        "tht",
        "tdiab",
        "taflp",
        "taflk",
        "thjtinf",
        "tcvich", 
        "tsivil", 
        "tcvhinf",
        "bivt", 
        "bia", 
    ]


    # ensuring all str entries in these columns are lowercase
    for col in yes_no_nan:
        working_df[col].str.lower()

    # replacing values of columns in datasubset in yes_no_nan list based on values in dict_replace_nan_yes_no
    for col in yes_no_nan:
        working_df[col] = working_df[col].apply(lambda x: dict_replace_nan_yes_no.get(x, x))
        
        if col in nan_to_0:
            working_df[col] = working_df[col].fillna(0)

    # replacing values of columns in datasubset in nan_date list setting all unique values to 1, where there were missing values, values were set 0
    for col in nan_date:
        col_unique_vals = {key: 1 for  key in working_df[col].unique()}
        working_df[col] = working_df[col].apply(lambda x: col_unique_vals.get(x, x))
        
        working_df[col] = working_df[col].fillna(0)




    #----------------------------------
    # numeric values
    #----------------------------------

    # removing already processed columns from numeric columns
    duplicates = ["tcvich", "taflp", "taflk"]
    cols_where_empty_should_be_excluded = ["age", "sex", "slagt", "tmrs", "tidiktk", "tidiktd", "tsivil", "bivtdt", "bivtkl", "biatd", "biakl", "tidsykd", "tidsykk", "trokasig"]

    # remaking the numeric columns list not to include duplicates or columns where empty values should be excluded
    numeric_columns = [col for col in numeric_columns if col not in duplicates or col not in cols_where_empty_should_be_excluded or col != "newid"] # ensuring newid stays for target identification
    
    # replacing values of columns in datasubset in numeric colums list setting all unique values to 1, where there were missing values, values were set 0
    for col in numeric_columns:
        col_unique_vals = {key: 1 for  key in working_df[col].unique()}
        working_df[col] = working_df[col].apply(lambda x: col_unique_vals.get(x, x))
        working_df[col] = working_df[col].fillna(0)




    #----------------------------------
    # mrs values
    #----------------------------------

    # defining dict for replacing these values
    dict_mrs_values = {
        "0" : 'Ingen symptomer og ingen begrensninger i dagliglivet',
        "1" : 'Lette symptomer, men i stand til � utf�re alle vanlige aktiviteter',
        "2" : 'Begrensninger i sosiale aktiviteter, men uavhengig i ADL',
        "3" : 'Har behov for noe hjelp (instrumental ADL), men kan g� uten hjelp',
        "4" : 'Kan ikke g� uten hjelp, trenger hjelp i daglige aktiviteter (basic ADL)',
        "5" : 'Sengeliggende, inkontinent, avhengig av kontinuerlig hjelp',
        "6" : 'D�d'
        }

    # dict was in the wrong order for the function so used dict comprehension to reverse to save time as per mentors suggestions: Dirty code saves time
    dict_mrs_values_reversed = {k: int(v) for v, k in dict_mrs_values.items()} 

    # replacing values of columns in datasubset in numeric colums list setting all unique values to 1, where there were missing values, values were
    for col in ["mrs", "tmrs"]:
        working_df[col] = working_df[col].apply(lambda x: dict_mrs_values_reversed.get(x, x))

    # working_df = working_df.replace({"mrs": dict_mrs_values_reversed})




    #----------------------------------
    # Remove the remaining NAN (if any)
    #----------------------------------

    # remove nan from yes_no_nan columns    
    for name in yes_no_nan:
        working_df = working_df[working_df[name].notna()]
    
    # # remove nan from date columns    
    for name in nan_date:
        working_df = working_df[working_df[name].notna()]
    
    # remove nan from numeric columns    
    for name in numeric_columns:
        working_df = working_df[working_df[name].notna()]


    working_df = working_df[working_df['age'].notna()]
    working_df = working_df[working_df['tmrs'].notna()]
    working_df = working_df[working_df['sex'].notna()]
    working_df = working_df[working_df['slagt'].notna()]
    



    """ Method for replace string vars for slagt"""
    
    working_df['slagt'].replace("Hjerneinfarkt", 0, inplace=True)
    working_df['slagt'].replace("TIA", 1, inplace=True)
    working_df['slagt'].replace("Intracerebral bl�dning", 2, inplace=True)
    


    
    """ Outliers"""
    
    working_df = working_df[(abs(working_df['age']) <= 110) & (abs(working_df['age'] >= 0))] # removing people over 105 and 0 after ploting the values finding the outliers at  up to around abs(1500) 
    
    print("after nan treatment shape:")
    print(working_df.head)
    print(working_df.shape)
    

    # Checking that the count of subjects 
    count_subjects2 = len(working_df["newid"].unique())
    if count_subjects != count_subjects2:
        print("Number of participants has changed during cleaning!")
    
    # final dataframe before modelling done | resetting index
    working_df.reset_index(inplace=True)




    #----------------------------------
    # Checking which subject had reoccurence of stroke based on reoccurence of subject id in dataset
    #----------------------------------
    
    subjects_first_entry = []
    subjects_first_entry_index = []

    subjects_second_entry = []
    subjects_second_entry_index = []
    
    for idx, subject in enumerate(working_df["newid"]):
        if subject not in subjects_first_entry:
            subjects_first_entry.append(subject)
            subjects_first_entry_index.append(idx)
            working_df.loc[idx, "target"] = 0
        else:
            subjects_second_entry.append(subject)
            subjects_second_entry_index.append(idx)
            working_df.loc[idx, "target"] = 1

    print("first stroke count:")
    print(len(subjects_first_entry))
    
    print("more than one stroke count:")
    print(len(subjects_second_entry))

    del working_df["newid"]
    # print(working_df["target"])           #checking that targets were set 




    """ Plotting of Features, variables, and demographics ++"""

    # plot histogram of Age distribution
    Num_Bin=30	
    n, bins, patches = plt.hist(working_df['age'], Num_Bin, facecolor='#00ffd4', alpha=0.5,rwidth=0.85)
    plt.xlabel('Age')
    plt.ylabel('Number of Cases')
    plt.title(r'Histogram of Age Distribution')
    plt.subplots_adjust(left=0.15)
    plt.show()


    # plot histogram of MRS/tMRS distribution
    Num_Bin = 7
    plt.hist([working_df['tmrs'], working_df['mrs']], bins= Num_Bin, width = 0.3 ,color=['#5e97ff','#00ffd4'], alpha=0.5, label =['tMRS','MRS'])
    plt.xlabel('Modified Ranking Score')
    plt.ylabel('Number of Cases')
    plt.title(r'Histogram of tMRS/MRS Distribution')
    plt.subplots_adjust(left=0.15)     
    plt.legend(loc='upper right')
    plt.show()

    # plot correlations of MRS/SEX/AGE
    working_df = working_df[working_df['sex'].notna()] #remove nan values from gender values
    sns.stripplot(data= working_df, x="mrs", y="age", hue="sex", size=3)
    plt.legend(['Male','Female']) 
    plt.show()

        

    # plot correlations of MRS/AGE/Type of stroke
    working_df = working_df.replace({"mrs": dict_mrs_values_reversed})
    working_df = working_df[(abs(working_df['age']) <= 105) & (abs(working_df['age'] >= 5))] #outlier removal of age data
    working_df = working_df[working_df['age'].notna()] # remove nan values in age

    working_df = working_df[working_df['slagt'].notna()]
    working_df['slagt'].replace("Hjerneinfarkt", 0, inplace=True)
    working_df['slagt'].replace("TIA", 1, inplace=True)
    working_df['slagt'].replace("Intracerebral bl�dning", 2, inplace=True)
    sns.stripplot(data= working_df, x="mrs", y="age", hue="slagt", size=3)
    plt.show()




    """ MODELLING """

    Y = working_df["target"].values                     # Setting targets
    
    del working_df["target"]                            # Removing targets from test/train dataset
    
    norm = MinMaxScaler()                               
    X = norm.fit_transform(working_df)                  # Normalising the data

    num_pca_to_use = get_num_pca_to_run(X)              # finding the number of Principal components to use 
    pca = PCA(n_components=num_pca_to_use)
    X = pca.fit_transform(X)                            # performing the dimensional reduction (Principle  Component Analysis)

    
    # Splitting the dataset to test/train with 70% / 30% divition respectively
    X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size=0.3, random_state=42)

    # Training the Linear Support Vector Machine Model
    clf = svm.SVC(kernel='linear', C=1, random_state=0).fit(X_train, y_train)

    # Predicting on the Test data with the trained Model
    y_hat = clf.predict(X_test)
    
    # Finding the Accuracy score
    score = accuracy_score(y_test, y_hat)
    print(f"Score: {score}")

    # Plotting the results
    plot_results(y_hat, y_test) 


def plot_results(y_hat, y_test):
    """unused plot, wasn't nice. Displays results of svm on categorical data"""
    x = range(len(y_test))
    
    plt.scatter(x, y_test, color = "red")
    plt.scatter(x, y_hat, color = "blue")
    
    plt.show()
    

def plot_labeled_pca_3d(df, x, y, z, title):
    """Plot to explore and better comprehend the data"""
    fig = plt.figure()

    ax = fig.add_subplot(projection='3d')
    
    colors = np.where(df["sex"]=="Female",'red','blue')
    
    ax.scatter3D(df[x], df[y], df[z], color=colors)

    ax.set_xlabel(f'{x}')
    ax.set_ylabel(f'{y}')
    ax.set_zlabel(f'{z}')

    plt.title(f'{title}')


    plt.show()


def get_num_pca_to_run(table):
    """ Input: Table to be transformed by PCA. Applies pca to min(col, row) and sums cumulative explained variance ratio until threshold met 95%. 
    Output: the n_components crossing the 95% threshold """

    [x, y] = table.shape                                # get table shape
    pca = PCA(n_components=(min(x, y)))                 # run pca with min(col)
    varpca = pca.fit(table)

    cumsums = np.cumsum(pca.explained_variance_ratio_)  # getting the cumulative sum of explained variance
        
    n_components_to_use = 0
    for idx, i in enumerate(cumsums):
        n_components_to_use += 1
        
        if i >= 0.95:                                   # if 95% is met, stop and save the parameter
            break

    return n_components_to_use




if __name__ == "__main__":
    main()