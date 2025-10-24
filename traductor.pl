/* ======================================================
   Módulo: traductor.pl
   Traducción ES <-> EN usando las gramáticas DCG y el léxico.
   ====================================================== */

:- module(traductor, [
    traducir_es_en/2,           % traducir_es_en(+TokensES, -TokensEN)
    traducir_en_es/2,           % traducir_en_es(+TokensEN, -TokensES)
    traducir/2                  % traducir(+ListaTokens, -ListaTokens) (autodetección)
]).

:- use_module(base_datos).
:- use_module(bnf_es).
:- use_module(bnf_en).
:- use_module(bnf_negativo_es).
:- use_module(bnf_negativo_en).

/* ------------------------------------------------------
   Interfaz principal (autodetección simple)
   ------------------------------------------------------ */

traducir(Entrada, Salida) :-
    (   traducir_es_en(Entrada, Salida)
    ->  true
    ;   traducir_en_es(Entrada, Salida)
    ).

/* ------------------------------------------------------
   Español -> Inglés
   ------------------------------------------------------ */

% 1) Negativa: "tu no come la manzana" => "you do not eat the apple"
traducir_es_en(TokensES, TokensEN) :-
    phrase(bnf_negativo_es:oracion_negativa_es(sujeto(Num, Per, SujES),
                                               predicado(verbo_objeto(VerboES, CompES))),
           TokensES),
    map_sujeto_es_en(Num, Per, SujES, SujEN),
    map_verbo_es_en(Num, Per, VerboES, VerboEN),
    map_complemento_es_en(CompES, CompEN),
    phrase(bnf_negativo_en:oracion_negativa_en(sujeto(Num, Per, SujEN),
                                               predicado(verbo_objeto(VerboEN, CompEN))),
           TokensEN), !.

% 2) Afirmativa
traducir_es_en(TokensES, TokensEN) :-
    phrase(bnf_es:oracion_es(sujeto(Num, Per, SujES),
                             predicado(verbo_objeto(VerboES, CompES))),
           TokensES),
    map_sujeto_es_en(Num, Per, SujES, SujEN),
    map_verbo_es_en(Num, Per, VerboES, VerboEN),
    map_complemento_es_en(CompES, CompEN),
    phrase(bnf_en:oracion_en(sujeto(Num, Per, SujEN),
                             predicado(verbo_objeto(VerboEN, CompEN))),
           TokensEN).

/* ------------------------------------------------------
   Inglés -> Español  (afirmativas por ahora)
   ------------------------------------------------------ */

traducir_en_es(TokensEN, TokensES) :-
    phrase(bnf_en:oracion_en(sujeto(Num, Per, SujEN),
                             predicado(verbo_objeto(VerboEN, CompEN))),
           TokensEN),
    map_sujeto_en_es(Num, Per, SujEN, SujES),
    map_verbo_en_es(Num, Per, VerboEN, VerboES),
    map_complemento_en_es(CompEN, CompES),
    phrase(bnf_es:oracion_es(sujeto(Num, Per, SujES),
                             predicado(verbo_objeto(VerboES, CompES))),
           TokensES).

/* ------------------------------------------------------
   Mapeos léxicos / estructurales
   ------------------------------------------------------ */

% Sujetos
map_sujeto_es_en(Num, Per, SujES, SujEN) :-
    (   base_datos:pronombre(Num, Per, _, SujES, SujEN)
    ;   base_datos:pronombre_propio(Num, Per, _, SujES, SujEN)
    ;   ( Num = singular ->
            base_datos:objeto(singular, SujEN, SujES)
        ;   base_datos:objeto(plural,   SujEN, SujES)
        )
    ), !.

map_sujeto_en_es(Num, Per, SujEN, SujES) :-
    (   base_datos:pronombre(Num, Per, _, SujES, SujEN)
    ;   base_datos:pronombre_propio(Num, Per, _, SujES, SujEN)
    ;   ( Num = singular ->
            base_datos:objeto(singular, SujEN, SujES)
        ;   base_datos:objeto(plural,   SujEN, SujES)
        )
    ), !.

% Verbos (obtenemos raíz inglesa a partir de la forma española y viceversa)
map_verbo_es_en(Num, Per, VerboES, RaizEN) :-
    base_datos:verbo(RaizEN, Num, Per, VerboES).

map_verbo_en_es(Num, Per, RaizEN, VerboES) :-
    base_datos:verbo(RaizEN, Num, Per, VerboES).

% Complementos (objeto directo con/sin artículo)
map_complemento_es_en(articulo_sustantivo(SustES), articulo_sustantivo(SustEN)) :-
    base_datos:objeto(singular, SustEN, SustES), !.
map_complemento_es_en(sustantivo_plural(SustES), sustantivo_plural(SustEN)) :-
    base_datos:objeto(plural,   SustEN, SustES), !.

map_complemento_en_es(articulo_sustantivo(SustEN), articulo_sustantivo(SustES)) :-
    base_datos:objeto(singular, SustEN, SustES), !.
map_complemento_en_es(sustantivo_plural(SustEN), sustantivo_plural(SustES)) :-
    base_datos:objeto(plural,   SustEN, SustES), !.

% ------------------------------------------------------
% Utilidades de salida (opcionales)
% ------------------------------------------------------
unir_con_espacios([], "").
unir_con_espacios([X|Xs], S) :-
    unir_con_espacios(Xs, R),
    ( R = "" -> atomic_list_concat([X], '', S)
    ; atomic_list_concat([X,' ',R], S)).

capitalizar_primera(Cadena, Resultado) :-
    atom_chars(Cadena, [Prim|Resto]),
    upcase_atom(Prim, PrimMay),
    atom_chars(Resultado, [PrimMay|Resto]).

imprimir_tokens(Toks) :-
    unir_con_espacios(Toks, S0),
    capitalizar_primera(S0, S1),
    format('~w.~n', [S1]).
