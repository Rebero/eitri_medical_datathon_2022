library(tidyverse)
library(data.table)
library(caret)
library(lares)
library(lubridate)

# Load dataset (a dummy dataset was created where all Norwegian Carachters were replaced)
stroke <- read_csv("DataStrokeNotNorwegianLetter.csv")

###############################################################################################
# Calculate missingness and remove variables where more than 99.9% of observations are missing
#
# We chose to use 99.9% to remove "empty" variables, but keep variables containing sparse, but 
# potentially important information.
###############################################################################################

# Calculate the missingness of all variables using the lares package
missing <- missingness(stroke)

# Create list of variables with high missingness
missing1 <- missing %>%
  filter(missingness > 99.9)

# Filter variables with high missingness from the dataset
stroke <- stroke[ , -which(names(stroke) %in% missing1$variable)]

# Remove temporary objects
rm(missing, missing1)

####################################################################################################
# Create outcome parameter
#
# We chose to use more than one observation for a patient as the outcome variable. This is a
# weakness with the dataset since patients may have had strokes previously (that are not registered
# in the variable for previous strokes) and we do not know for sure that they do not have another stroke
# after the observation (at home, in a different hospital district, fatal stroke)
#####################################################################################################

# Label patients with more than one observation (0 = no more observations, 1 = more than one observation)
stroke <- stroke %>%
  group_by(newid) %>%
  mutate(outcome = n()-1) %>%
  ungroup() %>%
  mutate(outcome = ifelse(outcome > 1, 1, outcome))

# Amend the outcome parameter so the last observation for each patient is labeled 0 since we do not
# know if they have further strokes after the final observation
stroke <- cbind(stroke, 1:nrow(stroke))  
colnames(stroke)[ncol(stroke)] <- 'recordID'
data2 <- stroke %>%
  group_by(newid) %>%
  filter(n()>1) %>%  summarise(max(recordID))

stroke[stroke$recordID %in% data2$"max(recordID)", "outcome"] <- 0

# Remove temporary objects
rm(data2)

#########################################################################################################
# Convert dates
#
# Dates were coded in different formats in the dataset, presenting a challenge when using dates and times
# to calculate times between events.
#########################################################################################################

# Create dataframe with all (most) date variables
strokedates <- stroke %>%
  select(ends_with("dt"))

# Format date variables to standard date formats 
myfun <- function(x) as.Date(x, format = "%d%b%Y")
strokedates1 <- data.frame(lapply(strokedates[,3:82], myfun))

strokedates1 <- strokedates1 %>%
  mutate(xtskjvdt = ymd(strokedates$xtskjvdt))

# Remove old date variables from dataset
stroke <- stroke %>%
  select(-ends_with("dt"))

# Add formated date variables to dataset
stroke <- stroke %>%
  cbind(strokedates1)

# Remove temporary objects
rm(strokedates, strokedates1)

###############################################
# Replace "ja"/"nei" and "Ja"/"Nei" with 1 and 0
###############################################

stroke <- data.frame(lapply(stroke, function(x) {gsub("ja", 1, x)}))
stroke <- data.frame(lapply(stroke, function(x) {gsub("nei", 0, x)}))
stroke <- data.frame(lapply(stroke, function(x) {gsub("Ja", 1, x)}))
stroke <- data.frame(lapply(stroke, function(x) {gsub("Nei", 0, x)}))

##############################################################################
# Calculate new time variables
#
# We wanted to create some new variables for times between events in minutes
#############################################################################

# Ictus date and time
stroke$tidiktd <- as.Date(stroke$tidiktd, format = "%d%b%Y")

tidikt <- data.frame(tidiktk = stroke$tidiktk,
                     tidiktd = stroke$tidiktd)

tidikt <- tidikt %>%
  mutate(hours = str_sub(tidiktk, -4, -3)) %>%
  mutate(min = str_sub(tidiktk, -2)) %>%
  mutate(time = sprintf("%s %s:%s", tidiktd, hours, min)) %>%
  mutate(datetime = ymd_hm(time))

# Admission date and time
tidsyk <- data.frame(tidsykk = stroke$tidsykk,
                     tidsykd = stroke$tidsykd)

tidsyk <- tidsyk %>%
  mutate(hours = str_sub(tidsykk, -4, -3)) %>%
  mutate(min = str_sub(tidsykk, -2)) %>%
  mutate(year = str_sub(tidsykd, -2)) %>%
  mutate(month = str_sub(tidsykd, -4, -3)) %>%
  mutate(date = str_sub(tidsykd, -6, -5)) %>%
  mutate(time = sprintf("%s-%s-%s %s:%s", year, month, date, hours, min)) %>%
  mutate(datetime = ymd_hm(time))

# Thromobolysis date and time
bivt <- data.frame(bivtkl = stroke$bivtkl,
                   bivtdt = stroke$bivtdt)

bivt <- bivt %>%
  mutate(hours = str_sub(bivtkl, -4, -3)) %>%
  mutate(min = str_sub(bivtkl, -2)) %>%
  mutate(time = sprintf("%s %s:%s", bivtdt, hours, min)) %>%
  mutate(datetime = ymd_hm(time))

# Thromobolysis or embolectomy date and time
bia <- data.frame(biakl = stroke$biakl,
                  biadt = stroke$biadt)

bia <- bia %>%
  mutate(hours = str_sub(biakl, -4, -3)) %>%
  mutate(min = str_sub(biakl, -2)) %>%
  mutate(time = sprintf("%s %s:%s", biadt, hours, min)) %>%
  mutate(datetime = ymd_hm(time))

# Acetylsalisylsyre date and time
basa <- data.frame(basakl = stroke$basakl,
                   basadt = stroke$basadt)

basa <- basa %>%
  mutate(hours = str_sub(basakl, -4, -3)) %>%
  mutate(min = str_sub(basakl, -2)) %>%
  mutate(time = sprintf("%s %s:%s", basadt, hours, min)) %>%
  mutate(datetime = ymd_hm(time))

# Plateletinhibitor date and time
bpl <- data.frame(bplkl = stroke$bplkl,
                  bpldt = stroke$bpldt)

bpl <- bpl %>%
  mutate(hours = str_sub(bplkl, -4, -3)) %>%
  mutate(min = str_sub(bplkl, -2)) %>%
  mutate(time = sprintf("%s %s:%s", bpldt, hours, min)) %>%
  mutate(datetime = ymd_hm(time))

# Heparin high dose date and time
bheph <- data.frame(bhephkl = stroke$bhephkl,
                    bhephdt = stroke$bhephdt)

bheph <- bheph %>%
  mutate(hours = str_sub(bhephkl, -4, -3)) %>%
  mutate(min = str_sub(bhephkl, -2)) %>%
  mutate(time = sprintf("%s %s:%s", bhephdt, hours, min)) %>%
  mutate(datetime = ymd_hm(time))

# Iktus to utdato
stroke$utdato <- as.Date(stroke$utdato, format = "%d%b%Y")

# Calculate new variables
times <- data.frame(IctusToAdmission = as.numeric(tidsyk$datetime - tidikt$datetime),
                    IctusToThrombolysis = as.numeric(bivt$datetime - tidikt$datetime),
                    IctusToIAThromOrEmbol = as.numeric(bia$datetime - tidikt$datetime) * 60,
                    IctusToAcetylSalisyl = as.numeric(basa$datetime - tidikt$datetime) * 60,
                    IctusToPlateletInhibitor = as.numeric(bpl$datetime - tidikt$datetime) * 60,
                    IctusToHeparinHighDose = as.numeric(bheph$datetime - tidikt$datetime) * 60 * 24,
                    IctusToDischarge = as.numeric(stroke$utdato - tidikt$tidiktd))

# Remove observations with obvious wrong data/dates (more than 14 days from Ictus to admission)
times <- times %>%
  mutate(IctusToAdmission = ifelse(IctusToAdmission > 20160, "", ifelse(IctusToAdmission < 0, "", IctusToAdmission)))

# Remove negative time values
times <- times %>% 
  mutate_all(funs(replace(., .<0, NA)))

# Add new time variables to dataset
stroke <- stroke %>%
  cbind(times)

# Remove temp dfs
rm(bheph, bia, bivt, bpl, tidikt, tidsyk, basa, times)

##########################################################################################
# Add cleaned medication variables
#
# Medication variables were cleaned up an one-hot encoded in a separate script (in folder)
# The resulting variables were saved as csv and imported into the dataset.
###########################################################################################

# Import variables from csv file
meds <- read_csv("meds.csv")

# Remove some uneeded variables
meds <- meds %>%
  select(-newid, -...1)

# Add new medication variables to dataset (old variables removed further down in code)
stroke <- stroke %>%
  cbind(meds)

# Remove temporary objects
rm(meds)

###########################################################################################
# Filter patients that died
#
# We did not want obeservations where we knew that patients had died be part of the dataset
###########################################################################################

stroke <- stroke %>%
  mutate(mors_1 = str_sub(mrs, -1)) %>%
  mutate(mors_2 = str_sub(kmors, -1)) %>%
  mutate(mors = ifelse(mors_1 == "d", 1, ifelse(mors_2 == "d", 1, 0))) %>%
  select(-mors_1, -mors_2, -kmors, -kmorsd) %>%
  filter(is.na(mors))


#######################################################################################################
# Clean up variables
#
# We manually went through all variables in the dataset and identified variables that either had little 
# value to the model (e.g. very sparse) or that where highly correlated with other 
# variables (e.g. sub-scores of scales)
#
# This section is unfortunately a little messy due to the input from all team-members as result of
# the massive team-effort to clean the dataset. We did not prioritize to clean up this part of the
# code in the limited time available.
######################################################################################################

# Remove all free-text fields 

fritekstfelt <- c("xtsig",  'xnorsass',  'tannetd',  'tannetd2',  'skjutf10',  'uekganpatb',  'ue24anp',  'uecorbsk1',
                  'uecorbsk2',  'md1n',  'md1g',  'md1atc',  'md2n',  'md2g',  'md2atc',  'md3n',  'md3g',  'md3atc',  'md4n',  'md4g',
                  'md4atc',  'md5n',  'md5g',  'md5atc',  'md6n',  'md6g',  'md6atc',  'md7n',  'md7g',   'md7atc',  'md8n',  'md8g',
                  'md8atc',  'md9n',  'md9g',  'md9atc',  'md10n',  'md10g',  'md10atc',  'md11n',  'md11g',  'md11atc',  'md12n',
                  'md12g',  'md12atc',  'md13n',  'md13g',  'md13atc',  'mdu1n',  'mdu1g',  'mdu1atc',  'mdu2n',  'mdu2g',  'mdu2atc',
                  'mdu3n',  'mdu3g',  'mdu3atc',  'mdu4n',  'mdu4g',  'mdu4atc',  'mdu5n',  'mdu5g',  'mdu5atc',  'mdu6n',  'mdu6g',
                  'mdu6atc',  'mdu7n',  'mdu7g',  'mdu7atc',  'mdu8n',  'mdu8g',  'mdu8atc',  'mdu9n',  'mdu9g',  'mdu9atc',  'mdu10n',
                  'mdu10g',  'mdu10atc',  'mdu11n',  'mdu11g',  'mdu11atc',  'mdu12n',  'mdu12g',  'mdu12atc',  'mdu13n',  'mdu13g',
                  'mdu13atc',  'skjutf20',  'samtsig',  'skjutf21')

stroke <- stroke[ , !(names(stroke) %in% fritekstfelt)]

# Remove all date fields

slettdatoer <- c('xtopphd','xtopphdx','tidiktd','tidiktdx','osvlgdt','obldt','uurindt','obartdt','bivtdt','biadt','bhemikrand','bhemevakd','bdrend','bcarotisd','bhypdt','bivdt','basadt','bpldt','bhepldt','bhephdt','kngpegdt','kngpegopdt','kurinretdt','kurininkdt','kdvtdt','klungdt','khjtinfdt','khsvkdt','khjinfdt','khjbloddt','kpneudt','kuvidt','kinfutdt','kepidt','kfraktdt','kfalldt','kliggesdt','kmaligndt','utdato','tcvtiasmdt','tkordt','hvdato','uctdt','umrdt','uakctufd','uakmrutfd','uahctufd','uahmrutfd','uulhdt','uekgdt','ue24dt','uecordt','samtd','blprd','bltegd','kmorsd')

stroke <- stroke[ , !(names(stroke) %in% slettdatoer)]

stroke <- stroke %>%
  select(-ends_with("dt"))

# Remove unneeded or highly correlated variables

to_remove <- c("bivtkl",    "biadt",    "biakl",    "bhemikrand",    "bhemevakd",    "bcarotisd",    "bhypdt",    "bhypkl",    "bivdt",    "bivkl",    "basadt",    "basakl",    "bpldt",    "bplkl",    "bhepldt",    "tcvhinfant",    "tcvhinfald",    "tcvhinfar",    "tcvhinfsm",    "tcvhinfsar",    "tcvichant",    "tcvichald",    "tcvichar",    "tcvichsm",    "tcvichsar",    "tcvtiaant",    "tcvtiaald",    "tcvtiar",    "tcvtiasm",    "tcvtiasm24",    "tcvtiasmdt",    "thjtinfald",    "thjtinfar",    "tanginaald",    "tanginar",    "tkorald",    "tkorar",    "tkordt",    "taoventil",    "taovent1",    "thjanald",    "thjanar",    "tcarotisar",    "tcaropar",    "tpkar",    "tpkarald",    "tpkarar",    "tpkaropar",    "tlembar",    "tdvtar",    "thtald",    "thtar",    "tdyslipar",    "tdiabald",    "tdiabar",    "taflpald",    "taflpar",    "taflkald",    "taflkar",    "tmigrald",    "tmigrar",    "tdemensald",    "tdemensar",    "tdeprald",    "tdeprar",    "tkolsar",    "tcancer",    "tcancerar",    "tannet",    "tannetd",    "tannetd2")

stroke <- stroke[ , !(names(stroke) %in% to_remove)]

stroke <- stroke %>%
  select(-starts_with("osv"), -starts_with("obl"), -starts_with("uur"), starts_with("uph"), starts_with("md"))

ymseklokkeslettogar <- c("autoid", 'bltegk','blprk','ue24kl','uekgkl','uahmrutfk','uahctufk','uakmrutfk','uakctufk','umrkl','uctkl','trokslutt','tcvichsar','tcvichsm','tcvichar','tcvhinfsar','bhephkl','bheplkl','bplkl','basakl','bivkl','bheplkl','bplkl','basakl','bivkl','bhypkl','biakl','bivtkl','tidvarig','tidlegk','tidlegd','tidsykk','tidsykd','tidiktk','tidiktdOLD','norich','nortest','autoid2015','pnr','pnrx','xtsig','xtskjvdt','xnorsys','xnorsass','xsykehus','norsass')
stroke <- stroke[ , !(names(stroke) %in% ymseklokkeslettogar)]

divslett <- c("skjutf10",	"skjsig10",	"tinfekuvi",	"tinfekann",	"tutmatfys",	"tutmatpsy",	"oar",	"skjutf2",	"skjsig2",	"logapraxi",	"logaprkon",	"uctutf",	"uhemdap",	"uhemdtrans",	"uhemdcracau",	"uhemetiol",	"umrutf",	"umrdiff",	"umrhemor",	"uloks",	"uakctm1h",	"uctleug",	"uakctutf",	"skjsig20",	"skjsig21",	"umrleug",	"uakmrm1h",	"uakmrph",	"uakmrpv",	"uactstenho",	"uactstenve",	"uulhutf",	"uulhutater",	"uulhstenho",	"uulhstenve",	"uekgutf",	"uakmrutf",	"uahctutf",	"uahmrutf",	"uekgryt",	"ue24utf",	"ue24an",	"ue24anp",	"uecorutf",	"uecortilg",	"ue24eryt",	"uecortilg",	"uembutf",	"uemb6t",	"uembant",	"uembrate",	"umrlacnonelac",	"cth",	"ocsplakemb",	"atriefl",	"intstenose",	"bakre",	"unge",	"tapa",	"tapainf",	"embolisk",	"toast",	"rankin",	"iskemisk",	"embolus",	"karokklu1",	"karokkl",	"alderstrinn",	"asa",	"marevan",	"marevaninn",	"asainn",	"occip",	"bothhem",	"nihssinn",	"systinn",	"diastinn",	"tempinn",	"sao2inn",	"metning",	"o2tilsk",	"dagerinn",	"iktusdag",	"nihssint",	"gtidikt",	"gtidsyk",	"gtidleg",	"gtidblpr",	
              "gtidnih1",	"gtidct",	"gumr",	"gtidiktblprm",	"tidinnint",	"gtidiktnih1",	"nihssut",	"diffnihss",	"statin",	"statininn",	"tiaklinikk",	"a2blokker",	"tiazid",	"deltanihss",	"delta",	"betablokker",	"cablokker",	"tidliskemi",	"cttidl",	"acehemmer",	"reninang",	"agek",	"_Itoast_2",	"_Itoast_3",	"_Itoast_4",	"_Itoast_5",	"smokers",	"neversmoker",	"aktivsmoker",	"stillsmoker",	"side",	"hbint",	"atero",	"kardio",	"mikro",	"ukjent",	"deltanihssk2",	"deltanihssk3",	"deltanihssk4",	"deltanihssk5",	"deltanihssk6",	"deltanihssk7",	"deltanihssk8",	"deltanihssk9",	"deltanihssd2",	"deltanihssd3",	"deltanihssd7",	"forverring1",	"forverring2",	"forverring3",	"forverring4",	"prepost",	"alive7",	"tapaisk",	"goppholdnr",	"nihssut1",	"diffnihss1",	"uloksanterior",	"uloksbakre",	"uloksmedia",	"uloksantpost",	"uloksfremre",	"ulokwater",	"ulokmultiple",	"ulokflere",	"umrcortmix",	"umrlakleu",	"totrisk",	"umrdifftype",	"ulok",	"troponin",	"tiddagiktus",	"mrisize",	"mediaokkl",	"inr2",	"mediaokklct",	"mcaokkl",	"mrcortsub",	
              "mrblandsub",	"recurrence",	"recidiv",	"natt",	"lacs",	"uctfersk",	"uctny",	"lakwoke",	"nonlakwoke",	"foeretter",	"timer2",	"antihyp",	"preven",	"totrec",	"mors7",	"dageriktinn",	"mrs01",	"allbetablokker",	"ctaintra",	"mraintra",	"nihss1arm",	"nihss1afasi",	"nihssutarm",	"timer1",	"timer2bl",	"nihsstri",	"lokalinf",	"agemonica",	"af24",	"nihss3",	"tidiktinn",	"dmut",	"dmalle",	"aflpk",	"progression",	"infeksjon",	"ulokA1",	"ulokA2",	"ulokAuk",	"ulokM1",	"ulokM2",	"ulokM3",	"ulokMuk",	"ulokMperf",	"ulokWam",	"ulokWmp",	"ulokWap",	"ulokBtemp",	"ulokBo",	"ulokBth",	"ulokBmes",	"ulokBmo",	"ulokBcb",	"ulokBpo",	"umrsubcsik",	"umrlaksik",	"umremblak",	"mrokkl",	"mratett",	"ekgaf",	"ekgvebl",	"ekghoebl",	"ekghyper",	"ekgstt",	"ekgnfvi",	"ekggfvi",	"ekgnnvi",	"ekggnvi",	"ekg24af",	"ekkohyper",	"ekkoaorta",	"ekkomitral",	"ekkopfo",	"ekkofvi",	"ekkonvi",	"ekkoaneu",	"ekkovtromb",	"ekkoatromb",	"ekkoveg",	"ekkoasaneu",	"mrtidl",	"antsig",	"afasi",	"ulydinternaipsmed",	"oppholdsnummer",	"opphold1",	"restroke",	"reslag",	"recvh",	"mediagren",	"startfrisk",	"taflkp",	"antidepressivaut",	"taflpk",	"ctam1okkl",	"mram1okkl",	"totrisk1",	"pradaxa",	"nihssakutt1",	"nihssakutt2",	"nihssakutt3",	"nihssakutt4",	"nihssakutt5",	"nihssakutt6",	"nihssakutt7",	"nihssakutt8",	"nihssakutt9",	"nihssakutt10",	"nihssakutt11",	"nihssakutt12",	"nihss3t",	"nihss36t",	"nihss69t",	"nihss912t",	"nihss1215t",	"nihss1821t",	"nihss1518t",	"nihss2124t",	"nihss2d",	"nihss3d",	"nihss7d",	"statin1",	"statininn1",	"xarelto",	"xareltoinn",	"pradaxainn",	"eliquisinn",	"eliquis",	"antikoaginn",	"antikoag",	"bedre4",	"intsympt",	"intasympt",	"mrlak1g",	"mrlak2g",	"mrlak3g",	"mrlak4g",	"mrscg",	"mrcog",	"mrblg",	"mrlig",	"mrstlg",	"mrstsg",	"rec",	"mrg",	"recmr",	"mrinfstum",	"ctlak1g",	"ctlak2g",	"ctlak3g",	"ctlak4g",	"ctscg",	"ctcog",	"ctblg",	"ctlig",	"ctstlg",	"ctstsg",	"ctg",	"recct",	"ctinfstum",	"intCTsympt",	"int50sympt",	"int50asympt",	"mrlakg",	"nihmin1",	"nihmin2",	"nihmin3",	"nihmin4",	"nihmin5",	"nihmin6",	"nihmin7",	"nihmin8",	"nihmin9",	"nihmin10",	"nihmin11",	"nihsstatt1",	"nihsstatt2",	"nihsstatt3",	"nihsstatt4",	"nihsstatt5",	"nihsstatt6",	"nihsstatt7",	"nihsstatt8",	"nihsstatt9",	"nihsssum",	"antideput",	"diuralinn",	"alder75",	"age6574",	"vask",	"chadsvasc",	"nihss2127t",	"gebet",	"gebetant",	"mrdager",	"thalamus",	"pleie",	"parox",	"kronaf",	"af24t",	"afekg",	"nyaf",	"afoldny",	"holter",	"ukjaf",	"mcasymptokkl",	"nihssutarm1",	"ukjentkrypt",	"month",	"year",	"fazct",	"fazmr",	"anna",	"_merge",	"reca",	"rec1",	"hus",	"blodvolum",	"iktusneedle",	"iktbtmott",	"datoskaar",	"klokkeskaar",	"diktskaar",	"hourol01",	"hourol02",	"hourol03",	"hourol04",	"hourol05",	"hourol011",	"hourol012",	"hourol013",	"hourol014",	"hourol015",	"amiktol0dk",	"miktol0dk",	"hourol06",	"qtiktol0min",	"tiktol0min",	"iktmott",	"iktusdoor",	"doorneedle",	"norsass",	"mrs3",	"xnorsass",	"nortest",	"norich",	"kfall",	"kfalldt",	"kligges",	"kliggesdt",	"kmalign",	"kmaligndt",	"uctspot",	"ulokstamme",	"uekgangrbl",	"uekgpace",	"blhba1cny",	"onihk10dt",	"onihk10kl",	"onihk10p1a",	
              "onihk10p1b",	"onihk10p1c",	"onihk10p2",	"onihk10p3",	"onihk10p4",	"onihk10p5",	"onihk10p6",	"onihk10p7",	"onihk10p8",	"onihk10p9",	"onihk10p10",	"onihk10p11",	"onihk10tot",	"onihk10sig",	"onihk11dt",	"onihk11kl",	"onihk11p1a",	"onihk11p1b",	"onihk11p1c",	"onihk11p2",	"onihk11p3",	"onihk11p4",	"onihk11p5",	"onihk11p6",	"onihk11p7",	"onihk11p8",	"onihk11p9",	"onihk11p10",	"onihk11p11",	"onihk11tot",	"onihk11sig",	"onihk12dt",	"onihk12kl",	"onihk12p1a",	"onihk12p1b",	"onihk12p1c",	"onihk12p2",	"onihk12p3",	"onihk12p4",	"onihk12p5",	"onihk12p6",	"onihk12p7",	"onihk12p8",	"onihk12p9",	"onihk12p10",	"onihk12p11",	"onihk12tot",	"onihk12sig",	"onihk13dt",	"onihk13kl",	"onihk13p1a",	"onihk13p1b",	"onihk13p1c",	"onihk13p2",	"onihk13p3",	"onihk13p4",	"onihk13p5",	"onihk13p6",	"onihk13p7",	"onihk13p8",	"onihk13p9",	"onihk13p10",	"onihk13p11",	"onihk13tot",	"onihk13sig",	"onihk14dt",	"onihk14kl",	"onihk14p1a",	"onihk14p1b",	"onihk14p1c",	"onihk14p2",	"onihk14p3",	"onihk14p4",	"onihk14p5",	"onihk14p6",	"onihk14p7",	
              "onihk14p8",	"onihk14p9",	"onihk14p10",	"onihk14p11",	"onihk14tot",	"onihk14sig")

stroke <- stroke[ , !(names(stroke) %in% divslett)]

# Clean up NIH scores
nih <- stroke %>%
  select(ends_with("tot"))

stroke <- stroke %>%
  select(-starts_with("onih"))

stroke <- stroke %>%
  cbind(nih)

rm(nih)


# Clean up Rankin score

total <- stroke %>%
  select(ends_with("total"))

stroke <- stroke %>%
  select(-starts_with("obart"))

stroke <- stroke %>%
  cbind(total)

rm(total)

# Remove all remaining categorical variables with text fields and remove patients outside of HUS area

stroke <- stroke %>%
  type.convert(as.is = TRUE) 

stroke <- stroke[,names(sort(unlist(lapply(stroke, class)), decreasing = T))]

stroke <- stroke %>%
  filter(huslok == 1 | is.na(huslok)) %>%
  select(-huslok)

cat_to_keep <- stroke %>%
  select(slagt, utoast, mrs, uttil, tpleie, tutd, locsp)

stroke <- stroke %>% 
  select(-where(is.character)) %>%
  select(newid, outcome, everything())

stroke <- sapply(stroke, as.numeric)

##############################################################################################
# One-hot encoding variables
#
# A few of the multi-level categorical variables were deemed important for the model
# and were one-hot encoded.
##############################################################################################

dmy <- dummyVars("~.", data = cat_to_keep)
cats <- data.frame(predict(dmy, newdata = cat_to_keep))

stroke <- as.data.frame(stroke)

stroke <- stroke %>%
  bind_cols(cats)

rm(dmy)

#######################################################################################
# Split the dataset
#
# We chose a 70%/30% split between training/test datasets
######################################################################################

set.seed(698552214) 
sample <- sample(c(TRUE, FALSE), nrow(stroke), replace=TRUE, prob=c(0.7,0.3))
train  <- as.data.frame(stroke[sample, ])
test   <- as.data.frame(stroke[!sample, ])
rm(sample)

######################################################################################
# caret::XGBoost
#
# In this section we train an XGBoost model using (almost) the defaults parameters
# We did not have time to do any hyper-parameter tuning using cross-validation
######################################################################################

# Amend training and test set by removing patientidentifying variables and outcome
xgb_train <- train %>% select(-outcome, -newid, -recordID, -autoid1, -autoid2, -autoid2014, -autoid2x, -autoida)
xgb_test <- test %>% select(-outcome, -newid, recordID, -autoid1, -autoid2, -autoid2014, -autoid2x, -autoida)

# Create outcome objects
xgb_tr_outcome <- train$outcome
xgb_te_outcome <- test$outcome

#+++++++++++++++++++++++++++++++++++++++++++++++++++++
# Base caret xgboost model - using default parameters
#+++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(6875)

# Defining parameters for the xgboost algorithm
grid_default <- expand.grid(
  nrounds = 500, # Default is 100, we wanted to reduce the eta so needed to increase nrounds
  max_depth = 6,
  eta = 0.1, # Default is 0.3, we wanted a lower learning rate and chose 0.1 since we did not tune
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# Define the control parameters for the xgboost algorithm
train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

# Train the model on the training dataset
xgb_base <- caret::train(
  x = xgb_train,
  y = xgb_tr_outcome,
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = TRUE,
  objective = "binary:logistic"
)

# Make predictions on test set
xgbpred <- predict(xgb_base,xgb_test)
xgbpreds <- ifelse (xgbpred > 0.015,1,0) # Cut-off amended to balance false-positive/true-positive

# Create a confusion matrix for results
cfm <- confusionMatrix (as.factor(xgbpreds), as.factor(xgb_te_outcome), positive = "1")
cfm

# Show the impact of variables in the model
varImp(xgb_base)

# Plot ROC curve
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
pr <- prediction(predictions = xgbpred, labels = xgb_te_outcome)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,
     colorize = TRUE,
     print.cutoffs.at= seq(0,1,0.05),
     text.adj=c(-0.2,1.7))
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

