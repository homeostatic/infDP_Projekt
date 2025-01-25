%Aufgabe 2

%Lösung von DeepSeek
/* horseman(Heads, Feet, Men, Horses) :-
    add(Men, Horses, Heads),
    mult(Men, s(s(o)), LegsMen),
    mult(Horses, s(s(s(s(o)))), LegsHorses),
    add(LegsMen, LegsHorses, Feet). */





isPeano(o). %Basiselement
isPeano(s(N)) :- isPeano(N).

succ(X ,s(X)). % Bestimmung des Nachfolgers
pred(s(X),X ). % Bestimmung des Vorgängers

add(o ,Y,Y ) .
add(s(X),Y,s(Z)) :- add(X,Y,Z).

sub(X,Y,Z) :- add(Y,Z,X).

mult(o , _, o).
mult(s(N), M, K) :- mult(N,M,O), add(O,M,K).

leq(o , _ ).
leq(s(N), s(M)) :- leq(N,M).


%Die schwirigkeit der Aufgabe besteht darin die Zeilen solange zu vertauschen, bis das Programm terminiert. 
%Kp wie man da sonst rangehen soll
%Nach DeepSeek muss add(Men,Horses,Heads) die erste Zeile sein, weil diese Bedingung die Suche in den weiteren anfragen einschränkt.
horseman(Heads, Feet, Men, Horses) :-
    add(Men,Horses,Heads),
    mult(Men, s(s(o)), MFeet),
    mult(Horses,s(s(s(s(o)))), HFeet),
    add(MFeet,HFeet,Feet).
    
 % ?- horseman(s(s(s(s(s(s(s(s(o)))))))), s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))))), M, H).

 