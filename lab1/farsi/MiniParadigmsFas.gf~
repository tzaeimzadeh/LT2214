resource MiniParadigmsFas = open

  MiniGrammarFas,
  MiniResFas
  
in {

oper
  mkN = overload {
    mkN : Str -> Noun   -- predictable noun, e.g. "کتاب" -> "کتاب ها"
      = \n -> lin N (regNoun n) ;
    } ;

  mkPN : Str -> PN
    = \s -> lin PN {s = s} ;

  mkA : Str -> A
    = \s -> lin A {s = s} ;

  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb
      = \s -> lin V (tenseVerb s) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash" ?
      = \s   -> lin V2 (tenseVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for" ?
      = \s,p -> lin V2 (tenseVerb s ** {c = p}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink" ?
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}
