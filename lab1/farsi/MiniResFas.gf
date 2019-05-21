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

  -- i have no idea what i need for this mkVerb function
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
   
  -- verbs in lexicon have infinitive stem
  
  infVerb : Str -> Str = \inf ->
    case inf of {
      infpast = init inf  -- inf past stem is the inf stem minus the last letter (which is an n/ن)
      -- inf present stem doesn't follow much of a pattern. these are the inf pres stem for the verbs in the lexicon
      -- when i use the farsi keyboard it flips the line and the arrows
      infpres = { s = table {
         ; "کردن" => "کن"
         ; "بودن" => "باش"
         ; "دادن" => "ده"
         ; "رفتن" => "رو"
         ; "شکستن" => "شکن"
         ; "خریدن" => "خر"
         ; "آمدن" => "آ"
         ; "نوشیدن" => "نوش"
         ; "خوردن" => "خور"
         ; "دویدن" => "دو"
         ; "دیدن" => "بین"
         ; "پریدن" => "پر"
         ; "کشتن" => "کش"
         ; "خواندن" => "خوان"
         ; "خوابیدن" => "خواب"
         ; "فهمیدن" => "فهم"
        }
      }
    } ;
  
  tenseVerb : (personEnd : Str) -> Verb -> Verb
    = \personEnd, infVerb -> {
      s = table {
        TPres => "می" + infVerb.inf ! infpres + personEnd ;  -- present = prefix 'mi' + inf present stem + personal ending
        TPast => infVerb.inf ! infpast + personEnd ;  -- past = inf past stem + personal ending 
      }
    } ;
  
  negation : Bool -> Str = \b -> case b of {True => [] ; False => "ن" + mkVerb} ; -- the 'ن' is attached as a prefix to the verb

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

}
