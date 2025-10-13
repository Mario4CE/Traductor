/* ======================================================
   Gramática - Inglés
   Soporta:
   - Oraciones declarativas
   - Preguntas con do/does
   - Exclamaciones
   ====================================================== */

:- module(bnf_en, [
    oracion_en//2,
    pregunta_en//4,
    exclamacion_en//3
]).

:- use_module(base_datos).

/* ==========================
   === Oraciones declarativas ===
   ========================== */

oracion_en(sujeto(Num, Per, S), predicado(verbo_objeto(Verbo, Comp))) -->
    sujeto_en(Num, Per, S),
    predicado_en(Num, Per, Verbo, Comp), !.

/* ========= Sujeto ========= */

sujeto_en(Num, Per, Pron) -->
    [Palabra],
    { pronombre(Num, Per, _, _, Palabra),
      Pron = Palabra }, !.

sujeto_en(singular, tercera, Sust) -->
    articulo_en(_),
    sustantivo_en(Sust), !.

sujeto_en(plural, tercera, SustP) -->
    articulo_en(_),
    sustantivo_plural_en(SustP), !.

/* ========= Predicado ========= */

predicado_en(Num, Per, Verbo, Comp) -->
    verbo_en(Num, Per, Verbo),
    complemento_en(Comp), !.

/* ========= Complementos ========= */

complemento_en(articulo_sustantivo(Art, Sust)) -->
    articulo_en(Art),
    sustantivo_en(Sust), !.

complemento_en(sustantivo_plural(SustP)) -->
    sustantivo_plural_en(SustP), !.

/* ========= Artículos / Sustantivos ========= */

articulo_en(Art) -->
    [Palabra],
    { articulo(_, _, _, Palabra), Art = Palabra }, !.

sustantivo_en(Sust) -->
    [Palabra],
    { objeto(singular, Palabra, _), Sust = Palabra }, !.

sustantivo_plural_en(SustP) -->
    [Palabra],
    { objeto(plural, Palabra, _), SustP = Palabra }, !.

/* ========= Verbos ========= */

verbo_en(Num, Per, Verbo) -->
    [Palabra],
    { verbo(Palabra, Num, Per, _), Verbo = Palabra }, !.

/* ==========================
   === Preguntas ===
   ========================== */

% Ej: Does he eat apples ?
pregunta_en(auxiliar(Aux), sujeto(Num, Per, S), Verbo, sustantivo_plural(Complemento)) -->
    [Aux],
    { member(Aux, [do, does]),
      (Aux = does -> Num = singular, Per = tercera ; true) },
    sujeto_en(Num, Per, S),
    verbo_en(Num, Per, Verbo),
    sustantivo_plural_en(Complemento),
    [?], !.

/* ==========================
   === Exclamaciones ===
   ========================== */

% Ej: She eat apples !
exclamacion_en(sujeto(Num, Per, S), predicado(verbo_objeto(Verbo, sustantivo_plural(Comp))), exclamacion) -->
    sujeto_en(Num, Per, S),
    verbo_en(Num, Per, Verbo),
    sustantivo_plural_en(Comp),
    ["!"], !. % Los de exclamacion van con comillas ya que prolog no reconoce ¡ y ! lo toma como corte