resource MiniResFas = open Prelude in {

param
  Number = Sg | Pl ;
  Person = Per1 | Per2 | Per3 ;
  Tense = Pres | Past  ;  

  Agreement = Agr Number Person ;

  VForm = VTense Person Number

oper
  Noun : Type = {s : Number => Species => Str} ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl}
    } ;

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "ها") ;

  Adjective : Type = {s : Str} ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (vtense, person, number, inf : Str) -> Verb
    = \vt, p, n, inf -> {
    s = table {
      VTense => v ;
      Person => p ;
      Number => n ;
      Inf => inf ;
      }
    } ;

  personEnd : person -> number -> Str =\p,n ->
    case <p,n> of {
      <p1,sg> => "م" ;
      <p2,sg> => "ی" ;
      <p3,sg> => "د" ;
      <p1,pl> => "یم" ;
      <p2,pl> => "ید" ;
      <p3,pl> => "ند" ;
     } ;
       
  infVerb : Str -> Str = \inf ->
    case inf of {
      infpast = init inf
      infpres = { s = table {
          "کردن" => "کن"
          "بودن" => "باش"
          "دادن" => "ده"
          "رفتن" => "رو"
          "شکستن" => "شکن"
          "خریدن" => "خر"
          "آمدن" => "آ"
          "نوشیدن" => "نوش"
          "خوردن" => "خور"
          "دویدن" => "دو"
          "دیدن" => "بین"
          "پریدن" => "پر"
          "کشتن" => "کش"
          "خواندن" => "خوان"
          "خوابیدن" => "خواب"
          "فهمیدن" => "فهم"
        }
      }
    } ;
  
  tenseVerb : (tense, infverb, personEnd : Str) -> Verb
    = \tpres, tpast, infverb, personEnd -> {
      s = table {
        TPres => "می" + infVerb.inf ! infpres + personEnd ;
        TPast => infVerb.inf ! infpast + personEnd ;
      }
    } ;
  
  -- verbs in lexicon have infinitive stem
    -- infinitive past = remove 'n' 
    -- inf preset = no pattern, except for some  
  -- tense 
    -- present = prefix 'mi' + inf present stem + personal ending
    -- past = inf past stem + personal ending 


  negation : Bool -> Str = \b -> case b of {True => [] ; False => "ن" + mkVerb} ; -- this is a prefix



  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

}
