install.packages('imputeTS')
install.packages('dplyr')
library(dplyr)
require(imputeTS)
data <- read.csv('C:\\Users\\datathonadmin\\Desktop\\Datathon1709.csv')

#################################
#select all diferents medications names from the dataset (in free text)
#################################

medications <- unique(data$md1n)
medications <- rbind(medications, unique(data$md2n))
medications <- rbind(medications, unique(data$md3n))
medications <- rbind(medications, unique(data$md4n))
medications <- rbind(medications, unique(data$md5n))
medications <- rbind(medications, unique(data$md6n))
medications <- rbind(medications, unique(data$md7n))
medications <- rbind(medications, unique(data$md8n))
medications <- rbind(medications, unique(data$md9n))
medications <- rbind(medications, unique(data$md10n))
medications <- unique(medications[medications != ""])
medications <- sort(medications)
print(medications)

#################################
#group them by active principle
#################################

acarbose <- c("Glucobay")

acetazolamide <- c("Diamox")

allopurinol <- c("zyloric", "Zyloric", "Allopur",  "allopur"  )

amiloride <- c("Moduretic mit", "Moduretic mite", "Mormorix mite", "Normorix",
 "Normorix mite","noromix mite","Noromix mite","Normirix mite"  )

amlodipine <- c( "amlodipin","Amlodipin","Amlodipine","Amlodpin", "Esfarge hct", "Exforge HCT", 
 "Exforge hct", "Exforge HTC","exforge", "Exforge", "amlodipine",
 "norvasc","Norvasc", "felodipin", "Lomir")

amitriptiline <- c("sarotex", "Sarotex", "Sarotex ret", "sarotex ret.", "Amitriptyline", "Amitriptylin")

anacetranip <- c("Anacetrapib")

antiparkinson <- c("Requip depot")

apicsaban <- c("Eliqsuis", "Eliquis", "ELiquis", "eliquis")

ASA <- c("Abilify", "ASA", "asa", "Asa", "Abyle","Alybl-E" ,"AbyleE","Acetylsalisylsy", "Acetylsalisyre",
 "Alby-E","Albye","Albyel","Albyl","albyl- e","Albyl- E","albyl-e","albyl-E","Albyl-e",      
 "Albyl-E","ALbyl-E","albyl e","Albyl e","Albyl E","albyle","Albyle","AlbylE","AlbyleE",
 "Aspirin",   "Asantin ret","Asasanitn ret", "Asasantin","asasantin ret",
 "Asasantin ret","Asasantin Ret","asasantin ret.", "Asasanton","Asasantrin ret",
 "Diprasorin", "Diprsorin","MagnylE") 

atenolol <- c("atenolol","Atenolol","Atnolol", "Tenorimin" , "tenormin","Tenormin",
  "Uniloc","uniloc")

atorvastatine <- c("Storvastatin", "atorvastatin","Atorvastatin", "Arotvastatin","lipitor","Lipitor",
 "Lipitor1","Liptitor", "Liptor", "Atozet", "atorvastatine"  )

azathiopine <- c("Imurel")

baclofen <- c("lioresal", "Lioresal")

benazepril <- c("Bentazepil")

bendroflumetiazid <- c("Centul mite","centyl","Centyl","Centyl K","Centyl kaliumkl",
 "Centyl KCI","Centyl m kalium", "Centyl m KCL","Centyl m/ KCI" , 
 "Centyl m/Kakl","Centyl m/kal","Centyl m/kch","Centyl m/KCI","centyl m/kcl",   
 "Centyl m/kcl","Centyl m/KCL","Centyl med Kacl", "Centyl med kcl",  "Centyl med KCL", 
 "centyl mite","Centyl mite","Centyl Mite","Centyl mite/kcl", "Centylk",        
 "CentylK","CentylM M/kaliu", "CentylMite") 

bisoprolol <- c( "Emconco","emconcor","Emconcor","Emconcor5","Emoconcor","Emoncor",
 "Biosprolol","bisoprolol","Bisoprolol","Bisprolol", "lodoz","Lodoz"  )

bumetanid <- c("burinex", "Burinex")

bupropion <- c("Welbutrin", "Wellbutrin", "Wellbutrin ret", "Buprion" )

buspirone <- c("Buspiron")

candesartan <- c("atacand",  "Atacance Plus","atacand","Atacand","atacand plus","Atacand plus",
 "Atacand Plus","Atacand plus mi","Atacand Pluss" ,"Atacane","Atacnad plus mi",
 "candersartan","Candersartan","candesartan", "Candesartan","Candesartan com",
 "candesartan hct","Candesartan hct","Candesartan HCT", "Candesartan/HCT",
 "Candesartan/HTC","Candesarten",  "candemax comp", "Candemox","Candemox comp",
 "Kandesartan")

captopril <- c("Capoten", "captopril", "Captopril")

carbamazepine <- c("tegretol","Tegretol","tegretol ret", "Tegretol ret","tegretol retard",
 "Tegretol retard", "Trileptal", "trileptal"  )

carbimazol <- c("Carbimazol")

carvedilol <- c("Carvdilol","carvedilol","Carvedilol"  )

ceterizine <- c("Cefirzin")

citalopram <- c("Cipramil", "cipramil", "Citalopram", "citalopram", "Citalopramm")

clobazam <- c("Frisium")

clomipramine <- c("Anafranil")

clonazepam <- c("Klonazepam", "rivotril","Rivotril"  )

clonidin <- c("Catapressan")

clopidogrel <- c("plavix","Plavix","Plavix|", "Clopidogrel" )

clozapin <- c("Leponex", "Clozapine")

chlorprothixene <- c("Truxal")

dabigatran <- c("Padaxa", "pradaxa","Pradaxa" )

dapagliflozin <- c("forxiga", "Forxiga","Foxiga")

desloratidine <- c("Aerius")

desmopressin <- c("Minirin")

desogestrel <- c("cerazette", "Cerazette", "Cerazette p-pil" )

diazepam <- c("Valium", "stesolid", "vival","Vivial", "Sobril")

digitoxine <- c("Digiotixin","Digiotoxin", "digitoxin","Digitoxin","digotoxin", "Digimerck","digitoxine" )

digoxine <- c("Lanoxin", "Digoxin", "digoxine")

diltiazem <- c("Cardizem","Cardizem dep", "cardizem ret","Cardizem ret","Cardizem retard",
 "Cardizem uno","Cardizem Uno", "diltiazem")

dipyridamol <- c("Persanin ret","Persantin","persantin ret","Persantin ret","Perantin ret",
 "Persantin Ret","persantin ret.","Persantin ret.","Pesantin", "Asantin ret",
 "Asasanitn ret", "Asasantin","asasantin ret", "Asasantin ret","Asasantin Ret",
 "asasantin ret.", "Asasanton","Asasantrin ret", "Diprasorin", "Diprsorin", "Dipyridamol",
 "Aponova", "dipiridamol", "Presantin ret")

disopyramide <- c("Durbis ret")

disulfiram <- c("Antabus")

donepezile <- c("aricept","Aricept", "Arizept", "Donepezil", "donepezile", "Donezepil")

doxazosine <- c( "Carduan", "carduran", "Carduran", "Carduran dep", "Carudran", "Doksazopin", 
 "Doxazosin")

doxepin <- c("Sinequan")

duloxetine <- c("cymbalta", "Cymbalta", "Duloxetin", "")

empagliflozin <- c("Jardiancde", "Jardiance","Synjardi","Synjardy")

enalapril <- c("Enalalpril","enalapril","Enalapril","enalapril comp","Enalapril comp",
 "Enalapril Hexal", "enelapril comp","Analapril", "Renitac","renitec","Renitec",
 "Renitec comp","renitec comp mi","Renitec comp mi","renitec comp",  "Zanipress",
 "Perindopril")

eplerenone <- c("Inspra", "Eplerenon")

eprosartan <- c("teveten", "Teveten", "Teveten comp")

escitalopram <- c("cipralex", "Cipralex", "escitalopram", "Escitalopram")

esomeprazol <- c("Esopral", "Esomeprazol", "Esoprel", "Vimovo", "nexium","Nexium"  )

espironolactone <- c("spironolactone")

estradiol <- c("Estradot", "Eviana", "Indivina", "loette", "Loette", "Marvelon", "Mercilon",
 "Microgynon", "Ovesterin","activelle","Activelle","Vagifem", "Progynova", "Diane",
 "Oestriol", "Yasmin p-pille", "p-piller","P-piller","P-ring","P-stav")

ezetimib <- c("Atozet", "ezetrol", "Ezetrol", "inegy", "Inegy", "Ezetimib", "ezetimib")

exenatide <- c("Bydurion")

felodipin <- c("Felodipin",  "plendil", "Plendil", "Plendil dep")

fenelzine <- c("Nardil", "fenelzine")

fenofibrate <- c("Fenofibrat")

flecainide <- c("Tambocol","tambocor","Tambocor","Tambocor reatrd")

fluoxetine <- c("Fontex")

fluvastatine <- c("Leschol", "Leschol","Lescol","Lescol dep", "Lescol Dep","Lescol Depot")

fluvoxamine <- c("Fevarin")

furosemide <- c("Lasix","lasix ret" , "Lasix ret","Lasix Ret","lasix ret.","lasix retard",
  "Lasix retard","Lasix Retard","furosemid","Furosemid","Furosemide","Furosemied", 
  "Dirual", "diural","Diural","DiuraÃ¸", "Furix", "furix", "Furix ret", "Furix retard",
  "furosemide")

gabapentin <- c("Neurontin", "Nerontin","Neurontin" )

glibenclamid <- c("Glibenclamid", "GLibenclamid")

gliclazide <- c("Gliclazide")

glimepirid <- c("Amaryl", "amaryl", "Amayrl", "Glimenperid", "Glimeperid","glimepirid","Glimepirid" )

glipizid <- c("mindiab",  "Mindiab")

haloperidol <- c("Haldol")

hidroclorotiazide <- c("hct","HCT","Hydrochlorthiaz","Hydroklortiazid", "coaprovel", " Co Aprovel",
 "Coaprovel", "CoAprovel","Renitec comp","renitec comp mi","Renitec comp mi","renitec comp",
 "candemax comp", "Candemox","Candemox comp", "Edidrex", " Esfarge hct", "esidrex",
 "Esidrex", "Exforge HCT", "Exforge hct", "Exforge HTC", "Teveten comp",
 "Mormorix mite",  "Zestoretic","zestoretic mite","Normorix",
 "Normorix mite","noromix mite","Noromix mite","Normirix mite", "Tiazid", 
 "bendroflumetiazid","Thiazide" )

hydralazin <- c("Apresolin")

hydroxyzine <- c("Atarax", "atarax" )

Hyoscyamin <- c("Egazil")

imipramine <- c("Impra")

insuline <- c("Insuln","Insulatard","Levemir","Insulatard flex","insulin", "Lantus","Insulin",
  "Novomix mite","Novorapid", "Inuslin", "insuline" )

irbesartan <- c("coaprovel", " Co Aprovel","Coaprovel", "CoAprovel", "Co Aprovel", "Approvel", "aprovel", "Aprovel",
  "Irbesartan","Irbesartan HCT","Irbesartan hydr", "Irbestatin")

isoprolol <- c("Isoprolol")

isosorbide <- c("imdur", "imdur","Imdur","Imdur dep","Imdur depot", "Imdur Depot", "Ismo","Ismo ret",
  "Isosobimononitr","Isosorbid", "Isomex", "monoket","Monoket", "Monoket OD depo", "Sorbangil"  )

labetalol <- c("trandate",  "Trandate")

lamotrigine <- c("lamictal", "Lamictal","Lamiktal"   )

lansoprazol <- c("Lansoprazol","Lanzo","lanzoprazol" )

lercanidipine <- c("Lerkandipin","lerkandipin", "Lerkanidin","lerkanidipin", 
 "Lerkanidipin", "zanidip", "Zanidip",  "Zanipress" )

levetiracetam <- c("keppra", "Keppra")

levodopa <- c("Stalevo")

levothyroxine <- c("Lavaxin", "levaxin","Levaxin", "Levaxine")

levomepromazine <- c("Nozinan")

liothyronin <- c("Liothyronin",  "Liothyroxin")

linagliptin <- c("Jentadueto", "trajenta",  "Trajenta", "Glyxambi")

lisinopril <- c( "lisinopril","Lisinopril","Lisinopril HCT","Lisionopril",
 "zestril","Zestril",  "Zestoretic","zestoretic mite"   )

lithium <- c("Lithionit", "Litionitt")

loratadin <- c("loratadin","Loratadin","Loratidin")

losartan <- c("losartan","Losartan","Losartan comp" , "Losartan hct","Losartan HCT","Losartan/HCT",
  "Coazar comp","cordarone","Cordarone","Cosartan","Cozaa", "Cozaaar comp","cozaar","Cozaar",
  "cozaar comp","Cozaar comp", "Cozaar Comp","Cozaar comp f","Cozaar comp fo","Cozaar comp for",
  "Cozaar Comp for", "Cozaar Comp For","Cozaar compfort","Cozaar Forte","Cozaar komp","Cozaarcompforte", "Cozzr comp")

lovastatin  <- c("Mevacor", "Lovastatin")

lmwh <- c("klexane", "Klexane", "enoxaparine", "Bemiparin", "fragmin", "Fragmin" )

meloxicam <- c("Mobic")

memantin <- c("ebixa", "Ebixa", "Ebixia", "Memantin", "memantin", "Nemdatine", "Nemtadine", "Nemdadatin")

mesalamine <- c("pentasa")

metformin <- c("Metfomrmin", "Metformine", "Meformin","Metfomrmin","metformin",
 "Metformin", "Avandamet", "ecreas", "eucreas","Eucreas", "Eucreas 50/1000", 
 "eukreas", "Eurcreas", "Glucopahge", "Glucophag","glucophage","Glucophage", "Janumet",
 "Jentadueto", "Fenormin")

metoprolol <- c("Sel-zok","Slo-zok","Sel-zok dep","Selo-Zoc","selo-zok", "Selo-zok","Selo-Zok",
  "Selo-zok dep","Selo-zok dep.","Selo-zok sep","Selo 20k","selo zok",
  "Selo zok","Slo-zok","Selo Zok","Seloken", "Selozo","Selozoc","SeloZoc","selozok",
  "Selozok", "SeloZok","Selozok2","Selzok", "metopolol","metoprolol",
  "Metoprolol","Metoprolol dep","Metoprolol depo","Metorpolol","Metorpolol dep",
  "metoprolol","Metroprolol","Sleozok" ,"Metpprolol","motoprolol",
  "Bloxadoc", "Bloxazoc", "Bloxazoc dep","Synjardi","Synjardy")



mianserin <- c("Tolvon", "tolvon")

mirtazapin <- c("Mirtazapin","Mirtazapin nexa","Mirtazepin","remeron","Remeron"  )

moklobemid <- c("Aurorix")

morphine <- c("Dolcontin", "Buprenorphine", "Subutex")

moxonidine <- c("moxonidin",  "Moxonidin","Moxonidon",  "Moxoridin", "Physiotens","physitens"  )

naproxen <- c("Vimovo")

nebivolol <- c("Hypoloc", "Nebicard", "Nebivolol")

nifedipine <- c("adalat", "Adalat","Adalat dep","adalat oros","Adalat oros","Adalat Oros",
  "Adalat oros dep","Adalt oros", "Nifedipin",  "Nifenova", "Dalat oros", "nifedipine")

nitrazepam <- c("Mogadon", "Apodorm")

normodipine <- c("Normodipin")

nortriptiline <- c("Noritren")

olanzapine <- c("Zyprexa")

olmesartan <- c("Olmetac comp","Olmetec", "Olmetec comp","Olmitec comp")

omeprazol <- c( "Omeprasol","omeprazol","Omeprazol","omeprazole", "Losec"  )

pantoprazol <- c("Somac")

paroxetine <- c("seroxat","Seroxat", "Paroxetin" )

phenelzine <- c("Nardil")

phenobarbital <- c("Fenemal")

phenytoin <- c("Epinat")

pioglitazone <- c("Actos", "Pioglitazon")

piracetam <- c("Nootropil")

pramipexol <- c("sifrol", "Sifrol")

pravastatin <- c("pravastatin", "Pravastatin", "Prevastatin", "pravachol","Pravachol","Pravachol 20",
 "Parvastatin")

prednisolone <- c("prednisolon", "Prednisolon","Prenisolon")

pregabalin <- c("lyrica","Lyrica")

primidon <- c("Liskantin")

propranolol <- c("Pranolol", "Pranolog", "Propal ret", "Propanolol", "Inderal",
 "Inderal depot","Inderal ret","Inderal retard")

quetiapine <- c("Quentiapin", "Quetapin", "Quetiapin", "Quetiapine", "Seroquel" )

ramipril <- c("Ramipiril","Ramipiril comp","Ramipirl","ramipiril","Ramiplus","ramipril","Ramipril", "Tiatec", 
  "Tiratec", "Triatec", "triatec")

ranitidine <- c("Ranitidin",  "ranitidin", "zantac","Zantac" )

rasagilin <- c("Azilect")

reboxetine <- c("edronax")

ribaroxaban <- c("Xarelto", "xarelto")

risperidone <- c("Risperdal")

rivastigmin <- c("Exelon", "Exelon dep", "Exelon plaster")

rosiglitazone <- c("Avandamet", "Avandia")

rosuvastatine <- c("crestor", "Crestor", "Krestor", "Rusuvastatin", "Rosuvastatin", "Rovartal")

ropinirol <- c("Adartrel", "adartrel")

saxagliptin <- c("Onglyza")

selegilin <- c("Eldepryl")

sertraline <- c("Sertralin",  "zoloft","Zoloft")

simvastatine <- c("Simastatin","Simvastalin","simvastatin","Simvastatin","Simvastatni",
  "Simvastin","Sivastatin", "Simvstatin", "Sivmastatin", "Simvastatine",
  "zocor","Zocor", "inegy", "Inegy", "Socor" )

sitagliptin <- c("Janumet", "januvia", "Januvia" )

sotalol<- c("Sotacor","sotalol","Sotalol")

spironolactone <- c("aldactone", "Aldactone", "Spironolakton", "Spririx", "Spririx", "Spirix")

sulfasalazine <- c("Salazopyrin")

sumatriptan <- c("Imigran")

tamsulosin <- c("Omnic")

telmisartan <- c("micardis","Micardis","micardis plus","Micardis plus","Micardis Plus",
 "Micardis pluss",  "Micradis Pluss", "Telmisartan","Telmisartan hct",
 "Telmisartan HCT", "Actelsar")

thyroxine <- c("Thyroxin")

ticagrelor <- c("Brilique", "brilique")

tolterodine <- c("Detrusitol", "Detusitol")

trimipramine <- c("surmontil", "Surmontil")

valproate <- c("orfiril", "Orfiril","Orfiril long","Orfiril ret","orifiril long",
 "Valproat","valproate")

valsartan <- c("diovan","Diovan","diovan comp","Diovan comp","Diovan Comp","Diovan copm","Diovan|",
 "Diovane comp","valsartan","Valsartan", "Valsartan comp","Valsartan HCT",
 "Valsartan sando","Valsartan/HCT","Valsartane","Valsertan","Vaslartan",
 "valsartan","Valsartan","Valsartan comp","Valsartan HCT","Valsartan sando",
 "Valsartan/HCT","Valsartane","Valsertan","Vaslartan", "Esfarge hct", "Exforge HCT", 
 "Exforge hct", "Exforge HTC","exforge", "Exforge", "Entresto")

venlafaxine <- c("Venalfaxin", "Venlafaksin","venlafaxin","Venlafaxin","Efexor",
 "Efexor dep","Efexor depo","Efexor depot","effexor","Effexor","Effexor dep")

verapamil <- c("Isoprin ret","isoptin","Isoptin","isoptin ret","Isoptin ret",
 "isoptin retard","Isoptin retard", "Veracard","Verakard", "Ioptin ret")

vildagliptin <- c("ecreas", "eucreas","Eucreas", "Eucreas 50/1000", "eukreas", "Eurcreas", "Galvus")

vortioxetin <- c("Brintellix")

warfarine <- c("maervan","Marean","Marevab","marevam", "marevan","Marevan","Marevane"  )

ziprasidone <- c("Zeldoks")

zuclopenthixol <- c("cisordinol")


#################################
#therapeutic groups of principles
#################################

a2ra <- c(valsartan,telmisartan, olmesartan, losartan, irbesartan, eprosartan, 
  candesartan)

ACEI <- c(enalapril, ramipril, lisinopril, captopril, benazepril)

altialzhei <- c(memantin, donepezile, rivastigmin)

antidepres <- c(reboxetine, phenelzine, paroxetine, nortriptiline, moklobemid, 
  mirtazapin, mianserin, imipramine, fluoxetine, fenelzine, 
  escitalopram, citalopram, duloxetine, doxepin, clomipramine,
  amitriptiline, vortioxetin, venlafaxine, trimipramine,sertraline )

antiepilep <- c(primidon, pregabalin, piracetam, phenytoin, phenobarbital, levetiracetam, 
  clonazepam,lamotrigine, gabapentin, fluvoxamine, carbamazepine, valproate)

antihist <- c(loratadin, hydroxyzine, desloratidine, ceterizine, ranitidine)

antiparkinson <- c(rasagilin, pramipexol,ropinirol, levodopa, Hyoscyamin, baclofen, antiparkinson, selegilin)

antipsycotic <- c(quetiapine, olanzapine, lithium, levomepromazine,haloperidol, 
  chlorprothixene, clozapin, zuclopenthixol, ziprasidone, risperidone)

bb <- c(sotalol,propranolol,nebivolol, metoprolol, labetalol, isoprolol,
  carvedilol, bisoprolol, atenolol)

benzo <- c(nitrazepam, diazepam, clobazam)

ccb <- c(amlodipine, verapamil, normodipine, nifedipine, lercanidipine, felodipin, diltiazem)

digitalis <- c(digoxine, digitoxine)

disulfiram <- c(disulfiram)

tiazide <- c(hidroclorotiazide)

estrog_progest <- c(estradiol, desogestrel)

fenofibrate <- c(fenofibrate)

flecainide <- c(flecainide)

furosemide <- c(furosemide)

insuline <- c(insuline)

isosorbide <- c(isosorbide)

lmwh <- c(lmwh)

otherAntiHBP <- c(moxonidine, hydralazin, desmopressin, clonidin)

nsaid <- c(naproxen, meloxicam, mesalamine)

opioid <- c(morphine)

oralAD <- c(vildagliptin,sitagliptin, saxagliptin, rosiglitazone, pioglitazone,
  metformin,linagliptin, glipizid, glimepirid, gliclazide,glibenclamid,
  exenatide, empagliflozin, dapagliflozin, acarbose )

otherAntiArrith <- c(disopyramide)

oralAntiCoag <- c(ribaroxaban, dabigatran, warfarine, apicsaban)

otherAntiPlat <- c(ticagrelor, dipyridamol, clopidogrel)

otherChol <- c(ezetimib, anacetranip)

otherDiuretic <- c(espironolactone, eplerenone, bumetanid, amiloride, acetazolamide)

ppi <- c(pantoprazol, omeprazol, lansoprazol, esomeprazol)

statines <- c(simvastatine, rosuvastatine, pravastatin, lovastatin, 
  fluvastatine, atorvastatine)

steroids <- c(prednisolone)

thiazid <- c(hidroclorotiazide, bendroflumetiazid)

thyroid <- c(liothyronin, levothyroxine, carbimazol, thyroxine)

#################################
#Detect all medications of a group in the different medications columns and
#show them in dicotomic columns 
#################################

medToCol <- function(data, medvector, i){
  data[data$md1n %in%  medvector, i]  <- 1
  data[data$md2n %in%  medvector, i]  <- 1
  data[data$md3n %in%  medvector, i]  <- 1
  data[data$md4n %in%  medvector, i]  <- 1
  data[data$md5n %in%  medvector, i]  <- 1
  data[data$md6n %in%  medvector, i]  <- 1
  data[data$md7n %in%  medvector, i]  <- 1
  data[data$md8n %in%  medvector, i]  <- 1
  data[data$md9n %in%  medvector, i]  <- 1
  data[data$md10n %in%  medvector, i] <- 1
  names(data)[i] <- deparse(substitute(medvector))
  return(data)
}
data <- medToCol(data, ASA, 1641)
data <- medToCol(data,a2ra , 1642)
data <- medToCol(data,ACEI , 1643)
data <- medToCol(data,altialzhei , 1644)
data <- medToCol(data,antidepres , 1645)
data <- medToCol(data,antiepilep , 1646)
data <- medToCol(data,antihist , 1647)
data <- medToCol(data,antiparkinson , 1648)
data <- medToCol(data,antipsycotic , 1649)
data <- medToCol(data,bb , 1650)
data <- medToCol(data,benzo , 1651)
data <- medToCol(data,ccb , 1652)
data <- medToCol(data,digitalis , 1653)
data <- medToCol(data,disulfiram , 1654)
data <- medToCol(data,tiazide , 1655)
data <- medToCol(data,estrog_progest , 1656)
data <- medToCol(data,fenofibrate , 1657)
data <- medToCol(data,flecainide , 1658)
data <- medToCol(data,furosemide , 1659)
data <- medToCol(data,insuline , 1660)
data <- medToCol(data,isosorbide , 1661)
data <- medToCol(data,lmwh , 1662)
data <- medToCol(data,otherAntiHBP , 1663)
data <- medToCol(data,nsaid , 1664)
data <- medToCol(data,opioid , 1665)
data <- medToCol(data,oralAD , 1666)
data <- medToCol(data,otherAntiArrith , 1667)
data <- medToCol(data,oralAntiCoag , 1668)
data <- medToCol(data,otherAntiPlat , 1669)
data <- medToCol(data,otherChol , 1670)
data <- medToCol(data,otherDiuretic , 1671)
data <- medToCol(data,ppi , 1672)
data <- medToCol(data,statines , 1673)
data <- medToCol(data,thyroid , 1674)
data <- medToCol(data,steroids , 1675)



#################################
#subset the new medications columns with the patient id to merge it with the original dataset
#################################

data2 <- data[, c(1640:1675)]

#################################
#Setting the variables of medications not present to 0
#################################

data2 <- imputeTS::na_replace(data2, 0)


write.csv(data2, 'C:\\Users\\datathonadmin\\Desktop\\meds.csv')

#################################
#code to set the outcome of the last stroke of patients with multiple strokes to 0, as they have not suffered a restroke after that one.
#################################
require(dplyr)
data <- cbind(data, 1:7578)
colnames(data)[1641] <- 'recordID'
data2 <- data %>%
  group_by(newid) %>%
  filter(n()>1) %>%  summarise(max(recordID))
data[recordID %in% data2$`max(recordID)`, outcome] <- 0