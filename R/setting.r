#'Setting some list or dataframe (diseaseList for comorbidity analysis, covariateSetting for PLP analysis)
#'@export
setting <- function(){
    diseaseList<<-list(diseaseId = c(1,
                                     #2,
                                     3,
                                     #3,4,
                                     5,6,7,8,9,10, #8disease
                                     11,12,13,14,15,16,
                                     #17,18,
                                     19,
                                     #,20, 21
                                     22,23,24,25,26,27
                                     ), #7disease
                       diseaseName = c("rhinosinusitis",#"pneumonia",
                                       "urticaria_angioedema",
                                       #"urticaria",
                                       #"angioedema",
                                       "atopicDermatitis",
                                       "otherSkinManigestation","otherAllergicDisease","immunodeficiency","eosinophilAssociatedDisease",
                                       "anaphylaxis",
                                       "hypertension","diabetesMellitus","osteoporosis","depression","arthritis","GERD",
                                       #"Pneumothorax","chronicHeartFailure",
                                       "ischemicHeartDisease",
                                       #,"atopy_total","CRS_total"
                                       "allergic_conjunctivitis","irritant_contact_dermatitis","other_dermatitis","psoriasis","Erythema_nodosum","Erythema_multiforme"
                                       
                       ),
                       conceptIdSet = list(c(25297,26711,28060,132932,134661,134668,139841,256439,257007,257012,259848,440140,441321,4230641,4280726,4305500),
                                           #c(252947,253234,316452,318789,433233,434975,435298,437588,438175,439853,443890,444084,4112673),
                                           c(132707,132983,135032,135618,138501,139100,139900,139902,140803,200169,432585,432870,437241,440372,441259,441264,442579,4023944,4064036,4066819,4066820,4101602,4137430,4146239,4194652,4277280,432791,433740,434219,441202,441488,4083784,4098626,4105886,43530807),
                                           #c(132707,132983,135032,135618,138501,139100,139900,139902,140803,200169,432585,432870,437241,440372,441259,441264,442579,4023944,4064036,4066819,4066820,4101602,4137430,4146239,4194652,4277280),
                                           #(432791,433740,434219,441202,441488,4083784,4098626,4105886,43530807),
                                           c(133834,134438,141648,442747,444272,444375,4031019,4064028,4064029,4064030,4064031,4066471,4066727,4066735,4286663,46269791,46270315,46270371),
                                           c(75614,81931,132702,133551,135892,137053,137193,137626,140168,141370,141651,141654,432585,432870,437241,440372,441259,441264,443754,443900,4004352,4031142,4032899,4061843,4064032,4066472,4066736,4100184,4101602,4137430,4146738,4174533,4216188,4242574,4269878,4284492,4291277,4292519,4297491,4307925,45766714,46269732),
                                           c(374036,374347,376422,376707,379019,380111,432791,434219,440905,441202,441488,4020296,4026136,4031019,4055224,4055225,4058694,4058695,4059297,4059298,4059299,4064028,4064029,4064030,4064031,4066471,4066735,4105886,4239004,4266230,4318502,43530807,46269791,46270315),
                                           c(27526,29783,141831,432587,433171,433740,434893,435228,438688,440072,440371,440982,4048208,4083784,4086243,4093002,4096098,4097998,4098622,4098625,4098626,4100976,4100979,4100980,4100982,4101448,4101452,4101453,4122335,4132570,4139034,4149583,4177007,4185547,4230184,4239314,4245623,4261471,4274184,40483560),
                                           c(192674,197596,200772,314381,320749,434008,434281,435775,4090099,4111856,4137770,4245409,4297886,4297887,4302954,4305666,4341633),
                                           c(432791,434219,440905,441202,441488,4020296,4105886,4239004,4266230,4318502,43530807),
                                           c(320128),
                                           c(192279,200687,201254,201826,318712,321822,376065,377821,435216,439770,442793,443412,443727,443729,443730,443731,443732,443733,443734,443735,443767,4008576,4096041,4096042,4096670,4096671,4099652,4193704,4221933,4224419,4224879,4327944,40480000),
                                           c(77365,80502,81390,4002133,4002134,4003479,4003482,4003483,4004623,4004624,4010333,4033089,4067765,4069306,40480160,45766159,45767040),
                                           c(433991,434911,435220,438406,440383,441534,4049623,4077577,4098302,4195572,4228802,4282316),
                                           c(72705,74125,80809,81097,81931,256197,319825,4025831,4035611,4064048,4079734,4083556,4083681,4083682,4114439,4114447,4115161,4115377,4116142,4116143,4116151,4116440,4116447,4117686,4117687,4132810,4142899,4218311,4247084,4253901,4257971,4271003,4291025,4344166,40483262,40490497,44782456,44783376,44784338,44784341),
                                           c(30437,4144111),
                                           #c(253796,255302,258787),
                                           #c(316139,319835,439846),
                                           c(312327,314666,315286,315296,316427,317576,319038,319844,321318,438168,438172,444406,4092936,4108215,4108217,4108218,4108219,4108220,4108677,4108678,4108679,4108680,4119953,4124683,4176969,4296653,45766075,45766116),
                                           #,c(1),c(2)
                                           c(374036,374347,376422,376707,379019,380111),
                                           c(443900,4004352,4061843,4064032,4066472,4066736,4291277,4292519,46269732),
                                           c(133551,135892,137193,141370,4146738,4174533,4216188,4242574,4269878,4297491,45766714),
                                           c(75614,81931,140168,4031142,4100184,4284492,4307925),
                                           c(140176),
                                           c(132702,141651,443754,4032899)
                       ))

    cohortList <<- list(diseaseId = c(1,2,3,4,5,
                                      51,52,53,54,
                                      100,101,
                                      300,301),
                        diseaseName = c("Asthma", "Non-Severe Asthma","Severe Asthma", "AERD","ATA",
                                        "AERDsubtype1","AERDsubtype2","AERDsubtype3","AERDsubtype4",
                                        "exacerbation","GINA_STEP_4/5",
                                        "exacerbation_new", "not_exacerbation_new" )
    )

    measurementId <<- list(maesurementConceptId = c(2,3,4,5,6,7,8,
                                                    3028930,4169578,44786758,4010492,3046594,2212469,
                                                    3011708,3006504,3005322,3005600,
                                                    3017501,3026514,3005791,3021940,
                                                    3011505,3013115,3018010,3022096),
                           measureName = c("EDN","Periostin","Eotaxin1","Eotaxin2","SP_D","DPP10","MBL",
                                           "TGFb1","Chitinase","TIMP1","OPN","IL8","MPO",
                                           "FEV1(%)", "eosinophil in blood (%)", "IgE","FVC (%)",
                                           "Neutrophil in blood manual count (/ul)","Neutrophil in sputum (%)", "Eosinophil cationic protein","Eosinophil in sputum (%)",
                                           "FEV1/FVC (%)","Eosinophil in blood manual count (/ul)", "Neutrophil in blood (%)","Basophil in blood (%)")
    )

    covariateSetting <<- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                    useDemographicsAge = TRUE,
                                                                    useDemographicsAgeGroup = FALSE,
                                                                    useObservationAnyTimePrior = TRUE,
                                                                    useProcedureOccurrenceAnyTimePrior = FALSE,
                                                                    useMeasurementValueAnyTimePrior = TRUE,
                                                                    useMeasurementRangeGroupAnyTimePrior = FALSE,
                                                                    includedCovariateConceptIds = c(), addDescendantsToInclude = FALSE,
                                                                    excludedCovariateConceptIds = c(),
                                                                    addDescendantsToExclude = FALSE,
                                                                    includedCovariateIds = c())
}
