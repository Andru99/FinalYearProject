% Constructie algoritm de rezolvare care se poate reconstruii folosiing ILP
% Gasit forma de Genotip adaugare Teleport and all the other shits
% Animatie + Implementare imagine
% Schimbare de la pygame la modulul de js sau gasire metoda de traducre a outputlui sub forma de animatie
% Idee pt GA:
%   Control System that manage tactics
%   ...


%:- module(artif,
%  [
%    open_file/0
%    write_position_Astar/0,
%    write_position_door_key/0
%  ]
%).

read_walls(Str) :-
    at_end_of_stream(Str), !.

read_walls(Str) :-
    readWord(Str,Wall1),!,
    atom_string(Wall1,Wall),
    convert_to(Wall,X,Y),
    P = p(X,Y),
    assertz(thing(1,P)),
    read_walls(Str).

open_file :-
    X = '/home/andru/Projects/ArmonieEuler/FinalYearProject/output.txt',
    open(X,read,Str),
    read_walls(Str),
    close(Str).

 readWord(InStream,W):-
         get_code(InStream,Char),
         checkCharAndReadRest(Char,Chars,InStream),
         atom_codes(W,Chars).


   checkCharAndReadRest(10,[],_):-  !.
   checkCharAndReadRest(32,[],_):-  !.
   checkCharAndReadRest(-1,[],_):-  !.
   checkCharAndReadRest(end_of_file,[],_):-  !.
   checkCharAndReadRest(Char,[Char|Chars],InStream):-
         get_code(InStream,NextChar),
         checkCharAndReadRest(NextChar,Chars,InStream).

 convert_to(V,X,Y):-
 split_string(V,"","p()",[H|_]),
 split_string(H,",","",[H1,H2]),
 number_string(X,H1),
 number_string(Y,H2).

 get_length(N):-
    thing(1,P),!,
    P = p(_,Y),
    N is Y.

 check_pos(Pos, OID) :-
 nonvar(Pos),
 ( thing(1,Pos) -> OID = thing
  ;otherwise -> OID = empty
 ).

 generate_cells(L):-
    get_length(N),
    findall(p(X,Y),(between(1,N,X),between(1,N,Y)),L).

teleporter_functionality(L,Pos):-
    %random empty positon on the board
    random_between(1,L,X1),
    random_between(1,L,Y1),
    (check_pos(p(X1,Y1),OID),OID==empty ->Pos=p(X1,Y1)
    ;otherwise -> teleporter_functionality(L,Pos)).

generate_teleporter(L):-
    random_between(1,L,X1),
    random_between(1,L,Y1),
    (check_pos(p(X1,Y1),OID),OID==empty ->Pos=p(X1,Y1),assert(teleporter(3,Pos))
    ;otherwise -> generate_teleporter(L)).

 map_adj(Pos,AdjPos,OID):-
   nonvar(Pos),
   internal_poss_step(Pos, _M, AdjPos, 1),
   check_pos(AdjPos, OID).

 compute_step(p(X,Y), M, p(X1,Y1), I) :-
   ( M = s -> X1 =  X,    Y1 is Y+I
   ; M = e -> X1 is X+I,  Y1 =  Y
   ; M = n -> X1 =  X,    Y1 is Y-I
   ; M = w -> X1 is X-I,  Y1 =  Y
   ).

 internal_poss_step(P0, M, PossPosition, I) :-
   member(M, [s,e,n,w]), % try moves in this order
   compute_step( P0, M, PossPosition, I).

 get_agent_position(Pos):-
    agent(2,Pos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%  A* search Algorithm  %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% BUBBLESORT to sort the list of adjacent moves when succeding
bubblesort(Rel, D, List, SortedList) :-
swap(Rel, D,List, NewList), !,
bubblesort(Rel, D, NewList, SortedList).
bubblesort(_, _, SortedList, SortedList).

swap(Rel, D, [A,B|List], [B,A|List]) :-
check(Rel, B, A, D).
swap(Rel, D, [A|List], [A|NewList]) :-
swap(Rel, D,List, NewList).

check(Rel, A, B, C) :-
Goal =.. [Rel,A,B,C],
call(Goal).

compare1(A,B,C):-
	dis(A,C,R1),
	dis(B,C,R2),
	R1 < R2.

%ManhatanDistance
dis(p(X,Y),p(Z,T),C):-
	C is (abs(X-Z) + abs(T-Y)).

%%%% A* Search
search_Astar(P,[Pos:RPath|Queue],Visited,Path) :-
        equals(P,Pos),reverse([Pos|RPath],Path),!;
        findall(NewPos:[Pos|RPath], (
                map_adj(Pos,NewPos,empty),
                \+ member(NewPos,Visited),
                \+ member(NewPos:_,Queue)
        ),Children),
    append(Queue,Children,NewQueue),
    bubblesort(compare1,p(31,31),NewQueue,SortedNewQueue),
    search_Astar(P,SortedNewQueue,[Pos|Visited],Path).



equals(P1,P2):-
    P1 = p(X,Y),
    P2 = p(X1,Y1),
    X = X1, Y = Y1.

iterate_and_write([],_):-!.
iterate_and_write([H|T],OS):-
     writeln(OS,H),
     iterate_and_write(T,OS).

%%%  door implementation
write_position_Astar:-
    get_length(L),
    search_Astar(p(L,L),[p(1,1):[]],[],Path),
    open('aStar.txt',write,OS),
    iterate_and_write(Path,OS),
    close(OS).

door_position:-
   get_length(L),
   L1 is L-1,
   assert(door(3,p(L1,L))),
   assert(thing(1,p(L,L1))).

generate_key(L):-
    random_between(1,L,X1),
    random_between(1,L,Y1),
    (check_pos(p(X1,Y1),OID),OID==empty ->Pos=p(X1,Y1),assert(key(4,Pos))
    ;otherwise -> generate_key(L)).

key_funct(Agent_pos):-
    key(4,Pos),
    (Agent_pos == Pos ->retract(key(4,Pos)),door(3,P1),retract(door(3,P1))
    ;otherwise -> true).

move_door_key_scenario(Path1):-
    assert_border,
    door_position,
    get_length(L),
    generate_key(L),
    key(4,Pos1),
    search_Astar(Pos1,[p(1,1):[]],[],Path),
    key_funct(Pos1),
    L1 is L-1,
    retract(thing(1,p(L,L1))),
    search_Astar(p(L,L),[Pos1:[]],[],Path2),
    append(Path,Path2,Path1).

append_wall_list([]):-!.
append_wall_list([H|T]):-
    assertz(thing(1,H)),
    append_wall_list(T).

assert_border:-
    get_length(Length),
    findall(p(0,Y),(between(1,Length,Y)),L2),
    append_wall_list(L2),
    findall(p(X,0),(between(1,Length,X)),L3),
    append_wall_list(L3),
    Len is Length + 1,
    findall(p(Len,Y),(between(1,Len,Y)),L4),
    append_wall_list(L4),
    findall(p(X,Len),(between(1,Len,X)),L5),
    append_wall_list(L5).

write_position_door_key:-
    move_door_key_scenario(Path1),
    writeln(Path1),
    open('key_door.txt',write,OS),
    iterate_and_write(Path1,OS),
    close(OS).

last_element(List,Elem):-
    reverse(List,NList),
    NList=[H|_],
    Elem is H.

%%%search_Astar/Teleporter func
astar_recursive(Goal,L,Start,Pz):-
   teleporter(3,Pos),
   (search_Astar(Pos,[Start:[]],[],P),append(P,Pz,Pz1),teleporter_functionality(L,P1),P1\=Goal -> astar_recursive(Goal,L,P1,Pz1)
   ;otherwise -> writeln(Pz)).

do_the_teleporter(Path):-
    assert_border,%%%Border of the maze
    get_length(L),L1 is L-1, assert(thing(1,p(L1,L))), assert(thing(1,p(L,L1))),%%%Teleporter Cond
    generate_teleporter(L),%%Generate Teleporter Pos
    %%%%% While not reaching end, go Teleporter using Astar,
    astar_recursive(p(L,L),L,p(1,1),Path),
    writeln(Path).
