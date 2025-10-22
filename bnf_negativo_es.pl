/* ======================================================
   Gramática Español - Oraciones Negativas
   ====================================================== */

:- module(bnf_negativo_es, [
    oracion_negativa_es//2,
    pregunta_negativa_es//3,
    exclamacion_negativa_es//2
]).

:- use_module(bnf_es).
:- use_module(base_datos).

/* ==========================
   Oraciones declarativas negativas
   Ej: "el no come la manzana"
   ========================== */

oracion_negativa_es(sujeto(Num, Per, Suj), predicado(verbo_objeto(Verbo, Comp))) -->
    bnf_es:sujeto_es(Num, Per, Suj),
    [no],
    bnf_es:predicado_es(Num, Per, Verbo, Comp),
    { bnf_es:validar_coherencia_semantica(Verbo, Comp) }.

/* ==========================
   Preguntas negativas
   Ej: "¿ella no devora manzanas?"
   ========================== */

pregunta_negativa_es(sujeto(Num, Per, Suj), Verbo, Comp) -->
    ["¿"],
    bnf_es:sujeto_es(Num, Per, Suj),
    [no],
    bnf_es:verbo_es(Num, Per, Verbo),
    bnf_es:complemento_es(Comp),
    [?],
    { bnf_es:validar_coherencia_semantica(Verbo, Comp) }.

/* ==========================
   Exclamaciones negativas
   Ej: "¡ella no come manzanas!"
   ========================== */

exclamacion_negativa_es(sujeto(Num, Per, Suj), predicado(verbo_objeto(Verbo, Comp))) -->
    ["¡"],
    bnf_es:sujeto_es(Num, Per, Suj),
    [no],
    bnf_es:predicado_es(Num, Per, Verbo, Comp),
    { bnf_es:validar_coherencia_semantica(Verbo, Comp) },
    ["!"].

