%%Xhail.lp%%%
%%%%%%%%%%%%%

#display move/2.
#display isTeleporter/2.
#display isEmpty/2.
#display isWall/2.
#display isKey/2.
#display isDoor/2.

#domain maze(X,Y).
#domain agent(A).
#domain fluent(F).
#domain event(E).

%%%%%Event Caluculus DECLARATIONS
holdsAt(F,T+1)  :- holdsAt(F,T), not stopped(F,T).
holdsAt(F,T+1)  :- not holdsAt(F,T), started(F,T).
stopped(F,T)    :- happens(E,T), terminates(E,F,T).
started(F,T)    :- happens(E,T), initiates(E,F,T).

%%%%%Domain defintions
agent(a1;a2).

maze(X,Y):-isDoor(X,Y).
maze(X,Y):-isKey(X,Y).
maze(X,Y):-isTeleporter(X,Y).
