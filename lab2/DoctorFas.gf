concrete DoctorFas of Doctor =
  open
    SyntaxPes,
    ParadigmsPes,
    Prelude
  in {

-- I've got for Fas as the abbreviation for Farsi, but I think in the resource grammar it's Pes for Persian
-------------------
-- the first part could be a functor

lincat
  Phrase = Utt ;
  Fact = Cl ;
  Action = VP ;
  Property = VP ;
  Profession = CN ;
  Person = NP ;
  Place = {at,to : Adv} ;
  Substance = NP ;
  Illness = NP ;

lin
  presPosPhrase fact = mkUtt (mkS fact) ;
  presNegPhrase fact = mkUtt (mkS negativePol fact) ;
  pastPosPhrase fact = mkUtt (mkS anteriorAnt fact) ;
  pastNegPhrase fact = mkUtt (mkS anteriorAnt negativePol fact) ;
  presQuestionPhrase fact = mkUtt (mkQS (mkQCl fact)) ;
  pastQuestionPhrase fact = mkUtt (mkQS anteriorAnt (mkQCl fact)) ;


  impPosPhrase action = mkUtt (mkImp action) ;
  impNegPhrase action = mkUtt negativePol (mkImp action) ;

  actionFact person action = mkCl person action ;
  propertyFact person property = mkCl person property ;

  isProfessionProperty profession = mkVP (mkNP a_Det profession) ;
  needProfessionProperty profession = mkVP need_V2 (mkNP a_Det profession) ;
  isAtPlaceProperty place = mkVP place.at ;
  haveIllnessProperty illness = mkVP have_V2 illness ;

  theProfessionPerson profession = mkNP the_Det profession ;

  iMascPerson = i_NP ;
  iFemPerson = i_NP ;
  youMascPerson = you_NP ;
  youFemPerson = you_NP ;
  hePerson = he_NP ;
  shePerson = she_NP ;

  goToAction place = mkVP (mkVP go_V) place.to ;
  stayAtAction place = mkVP (mkVP stay_V) place.at ;
  vaccinateAction person = mkVP vaccinate_V2 person ;
  examineAction person = mkVP examine_V2 person ;
  takeSubstanceAction substance = mkVP take_V2 substance ;

-- end of what could be a functor
--------------------------------

  coughAction = mkVP (mkV "سرفه کردن") ;
  breatheAction = mkVP (mkV "نفس کشیدن") ;
  vomitAction = mkVP (mkV "استفراغ کردن") ;
  sleepAction = mkVP (mkV "خوابیدن") ;
  undressAction = mkVP (mkVP take_V2 (mkNP thePl_Det (mkN "لباس"))) (pAdv "در بیار") ;
  dressAction = mkVP (mkVP put_V2 (mkNP thePl_Det (mkN "لباس"))) (pAdv "بپوش") ;
  eatAction = mkVP (mkV "خوردن") ;
  drinkAction = mkVP (mkV "نوشیدن") ;
  smokeAction = mkVP (mkV "سیگارکشیدن") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "اندازه گرفتن")) (mkNP the_Det (mkN "دمای بدن")) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "اندازه گرفتن")) (mkNP the_Det (mkN "فشار خون")) ;

  hospitalPlace = {at = pAdv "در بیمارستان" ; to = pAdv "به بیمارستان"} ;
  homePlace = {at = pAdv "در خانه" ; to = pAdv "خانه"} ;
  schoolPlace = {at = pAdv "در مدرسه" ; to = pAdv "به مدرسه"} ;
  workPlace = {at = pAdv "در کار" ; to = pAdv "به کار"} ;

  doctorProfession = mkCN (mkN "دکتر") ;
  nurseProfession = mkCN (mkN "پرستار") ;
  interpreterProfession = mkCN (mkN "مترجم") ;

  bePregnantProperty = mkVP (mkA "حامله") ;
  beIllProperty = mkVP (mkA "مریض") ;
  beWellProperty = mkVP (mkA "خوب") ;
  beDeadProperty = mkVP (mkA "مرده") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "آلرژی")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "درد")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "کودک" "کودکان")) ;

  feverIllness = mkNP a_Det (mkN "تب") ;
  fluIllness = mkNP a_Det (mkN "آنفلوآنزا") ;
  headacheIllness = mkNP a_Det (mkN "سردرد") ;
  diarrheaIllness = mkNP a_Det (mkN "اسهال") ;
  heartDiseaseIllness = mkNP a_Det (mkN "بیماری قلبی") ;
  lungDiseaseIllness = mkNP a_Det (mkN "بیماری ریه") ;
  hypertensionIllness = mkNP (mkN "فشار خون بالا") ;

  alcoholSubstance = mkNP (mkN "مشروب") ;
  medicineSubstance = mkNP a_Det (mkN "دارو") ;
  drugsSubstance = mkNP aPl_Det (mkN "مواد مخدر") ;

oper
  pAdv : Str -> Adv = ParadigmsPes.mkAdv ;

  go_V = mkV "برو" "رفت" "رفته" ;
  stay_V = mkV "بمان" ;
  need_V2 = mkV2 (mkV "نیاز داشتن") ;
  take_V2 = mkV2 (mkV "گرفتن" "گرفت" "گرفته شده") ;
  put_V2 = mkV2 (mkV "پوشیدن") ;
  vaccinate_V2 = mkV2 (mkV "واکسن زدن") ;
  examine_V2 = mkV2 (mkV "معاینه کردن") ;

}