/* ======================================================
   Gramática Inglés - Oraciones Negativas
   ====================================================== */

:- module(bnf_negativo_en, [
    oracion_negativa_en//2,
    pregunta_negativa_en//4,      
    exclamacion_negativa_en//2
]).

:- use_module(bnf_en).
:- use_module(base_datos).

/* ==========================
   Oraciones declarativas negativas
   ========================== */

oracion_negativa_en(sujeto(Num, Per, Suj), predicado(verbo_objeto(Verbo, Comp))) -->
    bnf_en:sujeto_en(Num, Per, Suj),
    [Aux, not],
      { auxiliar_do_does_hacer(Num, Per, Aux);
         auxiliar_have_has_tener(Num, Per, Aux) },
    bnf_en:predicado_en(Num, Per, Verbo, Comp),
    { bnf_en:validar_coherencia_semantica_en(Verbo, Comp) }.

/* ==========================
   Preguntas negativas
   ========================== */

pregunta_negativa_en(auxiliar(Aux), sujeto(Num, Per, Suj), Verbo, Comp) -->
    [Aux],
    { auxiliar_do_does_hacer(Num, Per, Aux);
      auxiliar_have_has_tener(Num, Per, Aux) },
    bnf_en:sujeto_en(Num, Per, Suj),
    [not],
    bnf_en:verbo_en(Num, Per, Verbo),
    bnf_en:complemento_en(Comp),
    ["?"],
    { bnf_en:validar_coherencia_semantica_en(Verbo, Comp) }.

/* ==========================
   Exclamaciones negativas
   ========================== */

exclamacion_negativa_en(sujeto(Num, Per, Suj), predicado(verbo_objeto(Verbo, Comp))) -->
    bnf_en:sujeto_en(Num, Per, Suj),
    [Aux, not],
      { auxiliar_do_does_hacer(Num, Per, Aux);
         auxiliar_have_has_tener(Num, Per, Aux) },
    bnf_en:predicado_en(Num, Per, Verbo, Comp),
    { bnf_en:validar_coherencia_semantica_en(Verbo, Comp) },
    ["!"].