resource MiniResFas = open Prelude in {

param
  Number = Sg | Pl ;
  Person = Per1 | Per2 | Per3 ;
  VTense = Pres | Past  ;  

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
     }
  -- this are suffixes - how do i do that ?       
  mkVerb : Str -> Str 
    = \inf -> init inf
  
  -- e.g.
  N = {s: Number => Str
  mkN : Str -> N =
  \root =
  { sg => root; 
    pl => "bla" + root ;
  }
  }
  
  -- verbs in lexicon have infinitive stem
    -- infinitive past = remove 'n' 
    -- inf present = still working out the rule for this, as it's a bit more varied 
  -- tense 
    -- present = prefix 'mi' to inf present + personal ending
    -- past = infinitive stem + personal ending 


  negation : Bool -> Str = \b -> case b of {True => [] ; False => "ن"} ; -- this is a prefix

      
  --regVerb : (inf : Str) -> Verb = \inf ->
    --mkVerb inf (inf + "تن") (inf + "دن") ;
   


  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
     pl  +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
     cr  +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") ;
     lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed") (lov + "ing") ;
     kis + ("s"|"sh"|"x") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed") (inf + "ing") ;
     _ => regVerb inf
     } ;


  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  -- generalized verb, here just "be"
 param
   GVForm = VF VForm | PresSg1 | PresPl | PastPl ;

 oper
  GVerb : Type = {
     s : GVForm => Str ;
     isAux : Bool
     } ;

  be_GVerb : GVerb = {
     s = table {
       PresSg1 => "am" ;
       PresPl  => "are" ;
       PastPl  => "were" ;
       VF vf   => (mkVerb "be" "is" "was" "been" "being").s ! vf
       } ;
     isAux = True
     } ;

  -- in VP formation, all verbs are lifted to GVerb, but morphology doesn't need to know this
   verb2gverb : Verb -> GVerb = \v -> {s =
     table {
       PresSg1 => v.s ! Inf ;
       PresPl  => v.s ! Inf ;
       PastPl  => v.s ! Past ;
       VF vf   => v.s ! vf
       } ;
     isAux = False
     } ;

}
