library("data.table")
library("ggplot2")
library("stringr")

# input data
data <- fread("dataset_readmissions_datathon_HUS_2022.txt", sep = "\t")

# convert age groups into ordered factors
data[, age_group := factor(
    age_group,
    level = c(
        "0-9",
        "10-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80-89",
        ">90",
        ""
    )
)]
# convert gender signs 1/2 into male or female.
data[, gender := factor(c("male", "female"), level = c("male", "female"))[gender]]

# Patient based times of admission (unique hosp ID)
data[, times_admission := length(unique(hosp_unique_ID)), by = "patientID"]

# Here only consider patients has two or more admissions
data1 <- data[times_admision >= 2]
data1[, patient_readmission := sum(readmission, na.rm = T) >= 1, by = "patientID"]

# Only consider patients has readmission.
data2 <- data1[(patient_readmission)]

# Calculate how many department movings for one admission, and if current is the last record (dismission) of one admission.
data2[, movingaround := .N, by = "hosp_unique_ID"]
data2[, dismissed := (movingaround == 1:.N), by = "hosp_unique_ID"]

# Calculate the number of ICD codes for each admission
data2$Num_ICD_codes <- rowSums(data2[, 12:20] != "")
data2[dismissed == F, Num_ICD_codes := NA]
data2[mors == 1, Num_ICD_codes := NA]

# Plot the count of readmission, against the number of ICD codes of the dismission records
ggplot(data2[readmission == 1 &
        dismissed == T], aes(x = as.factor(Num_ICD_codes))) + geom_histogram(stat = "count")


## Patient based records. In data11-13, each patient has only one merged entry

# calculate times of readmission for each patient
data11 <- data[mors == 0, .("readmissiontimes" = sum(readmission)), 
    by = c("patientID", "gender", "age_group")]

# if the same patient has different age groups, the last one is used
data12 <- data11[, .(readmissiontimes = sum(readmissiontimes), age_group = age_group[.N]), 
    by = c("patientID", "gender")]

# Calculate the patient based readmission rate
data13 <-
    data12[, .(readmission_rate = sum(readmissiontimes > 0) / .N), by = c("age_group", "gender")]
# Simple plot
ggplot(data13, aes(x = age_group, y = readmission_rate, fill = gender)) + geom_bar(stat =
        "identity")
# Plot with better displays
ggplot(data13, aes(x = age_group, y = readmission_rate, fill = gender)) + geom_bar(stat =
        "identity", position = "dodge") +
    scale_fill_manual(values = c("royalblue", "maroon1")) + theme(text = element_text(size =
            20)) +
    scale_x_discrete(name = "age group") + scale_y_continuous(name = "Number of patients")


## Patient based records. Starting here, data21 is admission based data, meaning each unique hosp_unique_ID has one and only one record. 

# count the number of ICD codes
data$Num_ICD_codes <- rowSums(data[, 12:20] != "")

# age group, main ICD and number of ICD codes use the values in the last record (if multiple records because of department transfer) of the admission, degree of the urgency use the first record.
data21 <-
    data[mors == 0, .(
        age_group = age_group[.N],
        main_ICD = mainDiagICD10Chapter[.N],
        degree_of_urgency = degree_of_urgency[1],
        moving_times = .N,
        Num_ICD_codes = Num_ICD_codes[.N]
    ), by = c("patientID", "gender", "hosp_unique_ID", "readmission")]

# Simplify main ICD labels
data21[, main_ICD := str_extract(main_ICD, "\\(.+\\)")]

# Is current admission followed by a readmission?
data21[, followed_by_readmission := shift(readmission, type = "lead")]

# plot all non-readmission records by check if it is followed by a readmission or not and calculate the readmmission rate, against degree of urgency
ggplot(data21[readmission == 0], aes(x = factor(degree_of_urgency),
        fill = factor(followed_by_readmission))) +
    geom_bar(position = "fill") + 
    scale_fill_manual(values = c("transparent", "royalblue")) +
    scale_y_continuous(limits = c(0, 0.15), name = "rate of readmission") + 
    scale_x_discrete(labels = c("Emergency", "Elective"), name = "Degree of urgency of admission") +
    theme(legend.position = "none", text = element_text(size = 20))

# plot all non-readmission records, and check if it is followed by a readmission or not, against number of ICD codes
ggplot(data21[readmission == 0], aes(x = factor(Num_ICD_codes), 
        fill = factor(followed_by_readmission))) +
    geom_bar(position = "fill") + scale_fill_manual(values = c("transparent", "royalblue")) +
    scale_x_discrete(name = "number of ICD codes") + 
    scale_y_continuous(name = "rate of readmission", limits = c(0, 0.15)) + 
    theme(legend.position = "none", text = element_text(size = 20))

# plot all non-readmission records, and check if it is followed by a readmission or not, against number of department transfers
ggplot(data21[readmission == 0], aes(x = factor(moving_times), 
        fill = factor(followed_by_readmission))) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("transparent", "royalblue")) +
    scale_y_continuous(limits = c(0, 0.1), name = "rate of readmission") +
    scale_x_discrete(limits = c(1:10), labels = 0:9, name = "Number of department transfers") +
    theme(legend.position = "none", text = element_text(size = 20))

# plot all non-readmission records, and check if it is followed by a readmission or not, against main ICD code at the time of release
ggplot(data21[readmission == 0], aes(x = main_ICD, fill = factor(followed_by_readmission))) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("transparent", "royalblue")) +
    scale_y_continuous(limits = c(0, 0.25), name = "rate of readmission") +
    scale_x_discrete(name = "main ICD code at the time of release",) + theme(
        legend.position = "none",
        text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)
    )
