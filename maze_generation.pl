#!/usr/bin/env swipl

:- initialization(main, main).

internal_1d_to_2d(Z, N, p(X,Y)) :-
  X is mod(Z,N)+1,
  Y is div(Z,N)+1.

make_maze(N) :-
  NCells is (N*N)-1,
  maze_walls(NCells,N),
      findall(Pos,
             (between(0,NCells,W), internal_isWall(W,N),internal_1d_to_2d(W,N,Pos)),
             Walls),
      findall([Pos],
             (between(0,NCells,C), internal_isCell(C,N),internal_1d_to_2d(C,N,Pos)),
             Cells),
      random_permutation(Walls,ShuffledWalls),
      kruskals(Cells,ShuffledWalls).

% Create a lattice where empty cells are all spaced 1 unit apart
maze_walls(0,_):-!.
maze_walls(Z,N) :-
  internal_1d_to_2d(Z,N,Pos),
  Pos = p(X,Y),
  ( (1 is mod(X,2), 1 is mod(Y,2)) -> true
  ; otherwise                      -> assert((thing(1,Pos)))
  ),
  Z1 is Z-1,
  maze_walls(Z1,N).

% True if the Z is a 1D coordinate adjacent to a empty cell
internal_isWall(Z,N) :-
  X is mod(Z,N)+1,
  Y is div(Z,N)+1,
  X1 is mod(X,2),
  Y1 is mod(Y,2),
  dif(X1,Y1).

    % True if Z is a 1D coordinate of an empty cell
internal_isCell(Z,N) :-
  X is mod(Z,N)+1,
  Y is div(Z,N)+1,
  1 is mod(X,2),
  1 is mod(Y,2).

% Implementation of randomised Kruskals to generate a maze by deleting walls without
% creating cycles
% kruskals(+CellSet,+Walls)

kruskals(_,[]).
kruskals(Cells,[Wall|Walls]):-
  map_adj(Wall,C1,empty),
  map_adj(Wall,C2,empty),
  C2 \= C1,
  member(CellSet1,Cells),
  member(C1,CellSet1),
  (member(C2,CellSet1) -> kruskals(Cells,Walls)
  ; otherwise          -> (member(CellSet2,Cells),
                           member(C2,CellSet2),
                           append(CellSet1,CellSet2,MergedCellSet),
                           append([MergedCellSet],Cells,NewCells),
                           delete(NewCells,CellSet1,NewCells2),
                           delete(NewCells2,CellSet2,NewCells3),
                           retract(thing(1,Wall)),
                           kruskals(NewCells3,Walls))).

%Given position and AdjPosition should return OID of the position - things/empty
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

check_pos(Pos, OID) :-
nonvar(Pos),
( thing(1,Pos) -> OID = thing
 ;otherwise -> OID = empty
).

write_walls(_):-
    not(thing(1,_)),!.
write_walls(OS):-
%    open('output.txt',append,OS),
    thing(1,Pos),
    writeln(OS,Pos),
    retract(thing(1,Pos)),
    write_walls(OS).
%    close(OS).

open_file:-
    open('output.txt',write,OS),
    write_walls(OS),
    close(OS).

main(Argv) :-
    [H|_] = Argv,
    atom_number(H,N),
    make_maze(N),
    open_file.

