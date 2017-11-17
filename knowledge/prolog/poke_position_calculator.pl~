:- module(poke_position_calculator,[calculate_poke_position/6, get_object_height/1]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_register_prefix(suturo_object, 'http://knowrob.org/kb/suturo_object.owl#').

:- rdf_meta 
	get_object_height(r),
	get(r,r,r).


calculate_poke_position(X, Y, Z, RX, RY, RZ) :- get_object_sizes(H,W,D),
												TH is H / 4,
												TD is 2*D,
												RX is X,
                                                RY is Y,
                                                RZ is Z.

get_object_height(H):- rdf(suturo_object:'Ice_Tea_Package', suturo_object:'bounded', B),
					   get(B, suturo_object:'height', H).

get_object_width(W):- rdf(suturo_object:'Ice_Tea_Package', suturo_object:'bounded', B),
					   get(B, suturo_object:'width', W).

get_object_depth(D):- rdf(suturo_object:'Ice_Tea_Package', suturo_object:'bounded', B),
					   get(B, suturo_object:'depth', D).

get_object_sizes(RH,RW,RD) :- get_object_height(RH),
								get_object_width(RW),
								get_object_depth(RD).


get(B, P, R):-
    rdf(B, P, literal(type('http://www.w3.org/2001/XMLSchema#float', T))),
    atom_number(T, R).
												
