:- dynamic(diccionario/1).

% Dado un nombre de archivo que contiene todas las palabras que se quieren
% agregar al diccionario (una por linea), vacia diccionario/1 y agrega las definiciones nuevas

cargar(NombreDeArchivo) :-
  retractall(diccionario(_)),
  atom_codes(NombreDeArchivo, Arch),
  open(Arch, read, Str),
  read_file(Str,_),
  close(Str).

read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
    read_file(Stream,L), !.


% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).

diccionario_lista(L):- diccionario(X), string_codes(X, L).

juntar_con([],_,[]).
juntar_con([X],_,X).
juntar_con([X,Y|XS],A,R):- append(X,[A|S],R), juntar_con([Y|XS],A,S).

palabras(S, P) :- juntar_con(P,espacio,S), not((member(L, P), member(espacio,L))).

% El predicado funciona aunque no se sepa que es la variable V asignada ya que prolog
% infiere un elemento en esa posicion cuando realiza el arbol de inferencia, para
% que luego pasen una de las dos cosas siguientes, o se unifique con un valor,
% o se devuelve asi como esta, que es lo que hace
% Un ejemplo similar es el predicado length(X,3) al cual se le "pide" una lista de longitud 3
% devuelve X = [_G6839, _G6842, _G6845].
asignar_var(A,[],[(A,_)]).
asignar_var(A,[(A,V)|T],[(A,V)|T]).
asignar_var(A,[(X,V)|T],[(X,V)|M]):-A \= X,asignar_var(A,T,M).

palabras_con_variables(P, V) :- pcvauxpal(P, [], V, _).

% (+P, +D, -V -D2): instancia en V la búsqueda de P en D, y en D2 el nuevo dict

pcvauxpal([], D, [], D).
pcvauxpal([P|PS], D, [V|VS], D3) :-
    pcvauxlet(P, D, V, D2), pcvauxpal(PS, D2, VS, D3).
    
pcvauxlet([], D, [], D).
pcvauxlet([P|PS], D, [V|VS], D3) :-
    buscar(P, D, V, D2), pcvauxlet(PS, D2, VS, D3).

buscar(P, D, V, D2) :- (asignar_var(P, D, D2)), member((P, V), D2).

quitar(_,[],[]).
quitar(A,[H|T],L):-A==H, quitar(A,T,L).
quitar(A,[H|T],[H|L]):-A\==H,quitar(A,T,L).

cant_distintos([],0).
cant_distintos([X|XS],S) :- quitar(X,XS,R), cant_distintos(R,S2), S is S2+1.

% EJERCICIO 8
incluido([],L).
incluido([X|XS],L) :- member(X,L), incluido(XS,L).
descifrar(S,M) :- palabras(S,P), palabras_con_variables(P,V),setof(L,diccionario_lista(L),Q), incluido(V,Q), juntar_con(V,32,N), string_codes(M,N).

mensajes_mas_parejos(S, L) :-
	descifrar_sin_espacios(S, M), member(L, M), largos(L, N), desvest(N, D),
	not((member(L2, M), largos(L2, N2), desvest(N2, D2), D2 > D)).

largos([],[]).
largos([L|LS], [N|NS]) :- length(L, N), largos(LS, NS).

%Instancia en D el desvest^2 de la lista de numeros L
desvest(L, D) :- promedio(L, P), length(L, N), desvestaux(L, P, N, D2), D is D2 / N.

desvestaux([], _, _, 0).
desvestaux([L|LS], P, N, Res) :-
	desvestaux(LS, P, N, Rec), Res is (L - P)*(L - P) + Rec.

promedio(L, N) :- sum_list(L, N2), length(L, N3), N is N2 / N3.
