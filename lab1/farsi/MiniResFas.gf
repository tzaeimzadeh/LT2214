resource MiniResFas = open Prelude in {

param
  Number = Sg | Pl ;
  Person = P1 | P2 | P3 ;
  InfTense = IPres | IPast  ;  

  Agreement = Agr Number Person ;

  VForm = VF VTense Person Number ;

oper
  Noun : Type = {s : Number => Species => Str} ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl}
    } ;

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "ها") ;

  Adjective : Type = {s : Str} ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : Str -> Str -> Verb = \infPast, infPres -> {
    s = table {
      IPast = init infPast ;
      IPres = infPres
      } ;
    } 

  personEnd : Person -> Number -> Str =\p,n ->
    case <p,n> of {
      <p1,sg> => "م" ;
      <p2,sg> => "ی" ;
      <p3,sg> => "د" ;
      <p1,pl> => "یم" ;
      <p2,pl> => "ید" ;
      <p3,pl> => "ند" ;
     } ;
   
  -- verbs in lexicon have infinitive stem
  -- infpast = init inf  -- inf past stem is the inf stem minus the last letter (which is an n/ن)
  -- inf present stem doesn't follow much of a pattern. these are the inf pres stem for the verbs in the lexicon
  -- when i use the farsi keyboard it flips the line and the arrows
  --infpres = ; "کردن" => "کن"; "بودن" => "باش";"دادن" => "ده"; "رفتن" => "رو"; "شکستن" => "شکن"; "خریدن" => "خر"; "آمدن" => "آ"; "نوشیدن" => "نوش";
  -- "خوردن" => "خور"; "دویدن" => "دو";"دیدن" => "بین"; "پریدن" => "پر"; "کشتن" => "کش"; "خواندن" => "خوان"; "خوابیدن" => "خواب"; "فهمیدن" => "فهم"

  
  tenseVerb : (personEnd, InfTesnse : Str) -> Verb -> Verb
    = \personEnd, InfTense -> {
      s = table {
        TPres => "می" + mkVerb ! IPres + personEnd ;  -- present = prefix 'mi' + inf present stem + personal ending
        TPast => mkVerb !IPast + personEnd ;  -- past = inf past stem + personal ending 
      }
    } ;
  
  negation : Bool -> Str = \b -> case b of {True => [] ; False => "ن" + mkVerb} ; -- the 'ن' is attached as a prefix to the verb

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

}
