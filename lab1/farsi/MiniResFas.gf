--# -path=.:../abstract
resource MiniResFas = open Prelude in {

param
  Number = Sg | Pl ;
  Person = P1 | P2 | P3 ;
  Tense = Pres | Past  ;  

  Agreement = Agr Person Number ;

  VForm = VF Tense Person Number ;

oper
  Noun : Type = {s : Number => Str} ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> { 
    s = table {
      Sg => sg ; 
      Pl => pl} 
    } ; 

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "ها") ;

  -- mkNoun : Str -> Noun = \sg -> mkNoun sg (sg + "ها") ;

  Adjective : Type = {s : Str} ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : Str -> Str -> Verb = \infPast, infPres -> {
    s = table {
      VF Past p n => init infPast + personEnd p n;  -- past = inf past stem + personal ending 
      VF Pres p n => "می" + infPres + personEnd p n -- present = prefix 'mi' + inf present stem + personal ending
      } 
    } ;

  personEnd : Person -> Number -> Str =\p,n ->
     case <p,n> of {
      <P1,Sg> => "م" ;
      <P2,Sg> => "ی" ;
      <P3,Sg> => "د" ;
      <P1,Pl> => "یم" ;
      <P2,Pl> => "ید" ;
      <P3,Pl> => "ند" 
     } ;
   
  -- verbs in lexicon have infinitive stem
  -- infpast = init inf  -- inf past stem is the inf stem minus the last letter (which is an n/ن)
  -- inf present stem doesn't follow much of a pattern. these are the inf pres stem for the verbs in the lexicon
  -- when i use the farsi keyboard it flips the line and the arrows
  --infpres = ; "کردن" => "کن"; "بودن" => "باش";"دادن" => "ده"; "رفتن" => "رو"; "شکستن" => "شکن"; "خریدن" => "خر"; "آمدن" => "آ"; "نوشیدن" => "نوش";
  -- "خوردن" => "خور"; "دویدن" => "دو";"دیدن" => "بین"; "پریدن" => "پر"; "کشتن" => "کش"; "خواندن" => "خوان"; "خوابیدن" => "خواب"; "فهمیدن" => "فهم"

  
  negation : Bool -> Str -> Str = \b,verb -> case b of {True => verb ; False => "ن" + verb} ; -- the 'ن' is attached as a prefix to the verb

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

}
