/* ======================================================
   Gramática - Inglés (SIN ERRORES DE SINTAXIS)
   ====================================================== */

:- module(bnf_en, [
    oracion_en//2,
    pregunta_en//4,        % Para tests existentes (sí/no)
    pregunta_wh_en//4,     % Para wh-questions (corregido)
    exclamacion_en//3
]).

:- use_module(base_datos).

/* ==========================
   === Oraciones declarativas ===
   ========================== */

oracion_en(sujeto(Num, Per, S), predicado(verbo_objeto(Verbo, Comp))) -->
    sujeto_en(Num, Per, S),
    predicado_en(Num, Per, Verbo, Comp),
    { validar_coherencia_semantica_en(Verbo, Comp) }, !.

/* ========= Sujeto ========= */

sujeto_en(Num, Per, Pron) -->
    [Palabra],
    { pronombre(Num, Per, _, _, Palabra),
      Pron = Palabra }, !.

sujeto_en(singular, tercera, Sust) -->
    articulo_en(singular),
    sustantivo_en(Sust), !.

sujeto_en(plural, tercera, SustP) -->
    articulo_en(plural),
    sustantivo_plural_en(SustP), !.

/* ========= Predicado ========= */

predicado_en(Num, Per, Verbo, Comp) -->
    verbo_en(Num, Per, Verbo),
    complemento_en(Comp), !.

/* ========= Complementos ========= */

% El artículo "the" puede usarse con singular o plural
complemento_en(articulo_sustantivo(Sust)) -->
    [the],
    sustantivo_en(Sust), !.

complemento_en(articulo_sustantivo(SustP)) -->
    [the],
    sustantivo_plural_en(SustP), !.

% Artículos específicos (a, an, some)
complemento_en(articulo_sustantivo(Sust)) -->
    articulo_en(singular),
    sustantivo_en(Sust), !.

complemento_en(articulo_sustantivo(SustP)) -->
    articulo_en(plural),
    sustantivo_plural_en(SustP), !.

complemento_en(sustantivo_plural(SustP)) -->
    sustantivo_plural_en(SustP), !.

/* ========= Artículos / Sustantivos ========= */

articulo_en(Num) -->
    [Palabra],
    { articulo(_, Num, _, Palabra), Palabra \= the }, !.

sustantivo_en(Sust) -->
    [Palabra],
    { objeto(singular, Palabra, _),
      Sust = Palabra }, !.

sustantivo_plural_en(SustP) -->
    [Palabra],
    { objeto(plural, Palabra, _),
      SustP = Palabra }, !.

/* ========= Verbos (CON MANEJO ESPECIAL PARA 'eats') ========= */

verbo_en(singular, tercera, eats) -->
    [eats],
    { verbo(eat, singular, tercera, _) }, !.

verbo_en(Num, Per, Verbo) -->
    [Palabra],
    { verbo(Palabra, Num, Per, _),
      Verbo = Palabra }, !.

/* ============================
   === Validación Semántica (Inglés) ===
   ============================ */

validar_coherencia_semantica_en(Verbo, Comp) :-
    extraer_objeto_complemento_en(Comp, Objeto),
    obtener_raiz_verbo_en(Verbo, RaizVerbo),
    % Si el objeto tiene categoría, debe ser compatible
    \+ (categoria_semantica(Objeto, Categoria),
        \+ compatible_verbo_objeto(RaizVerbo, Categoria)).

extraer_objeto_complemento_en(articulo_sustantivo(Sust), Sust).
extraer_objeto_complemento_en(sustantivo_plural(Sust), Sust).

obtener_raiz_verbo_en(eats, eat) :- !.
obtener_raiz_verbo_en(Verbo, Verbo).

/* ==========================
   === PREGUNTAS SÍ/NO (4 args - Compatible con tests existentes) ===
   ========================== */

pregunta_en(auxiliar(Aux), sujeto(Num, Per, S), Verbo, Comp) -->
    [Aux],
    { member(Aux, [do, does]) },
    sujeto_en(Num, Per, S),
    verbo_en(Num, Per, Verbo),
    complemento_en(Comp),
    { validar_coherencia_semantica_en(Verbo, Comp) },
    [?], !.

/* ==========================
   === PREGUNTAS WH (4 args - Para nuevos tests) ===
   ========================== */

% What does she eat? → pregunta_wh_en(what, does, she, eat)
pregunta_wh_en(WhWord, Aux, Sujeto, Verbo) -->
    [WhWord],
    { interrogativo(_, WhWord) },
    [Aux],
    { member(Aux, [do, does]) },
    sujeto_en(singular, tercera, Sujeto),
    verbo_en(singular, tercera, Verbo),
    [?], !.

% Who eats apples? → pregunta_wh_en(who, eats, apples, _)
pregunta_wh_en(who, Verbo, Comp, _) -->
    [who],
    { interrogativo(_, who) },
    verbo_en(singular, tercera, Verbo),
    complemento_en(Comp),
    { validar_coherencia_semantica_en(Verbo, Comp) },
    [?], !.

/* ==========================
   === Exclamaciones ===
   ========================== */

exclamacion_en(sujeto(Num, Per, S), predicado(verbo_objeto(Verbo, Comp)), exclamacion) -->
    sujeto_en(Num, Per, S),
    verbo_en(Num, Per, Verbo),
    complemento_en(Comp),
    { validar_coherencia_semantica_en(Verbo, Comp) },
    ["!"], !.