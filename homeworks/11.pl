%Aufgabe 2

%von DeepSeek
/* horseman(Heads, Feet, Men, Horses) :-
    add(Men, Horses, Heads),          % Zuerst Köpfe einschränken
    mult(Men, s(s(o)), MFeet),        % Dann Beine berechnen
    mult(Horses, s(s(s(s(o)))), HFeet),
    add(MFeet, HFeet, Feet).          % Abschließend Beinsumme prüfen
     */





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



 % Aufgabe 3
fisch(Gewicht) :- Gewicht >= 0.
bruecke(Mobile1, Mobile2).

gewicht(Mobile, Gewicht) :- Mobile = fisch(Gewicht).
gewicht(Mobile, Gewicht) :- Mobile = bruecke(Mobile1, Mobile2), gewicht(Mobile1, G1), gewicht(Mobile2, G2), Gewicht is G1 + G2 + 1.

ausgeglichen(fisch(_)).
ausgeglichen(bruecke(Mobile1,Mobile2)) :- gewicht(Mobile1, Gewicht),  gewicht(Mobile2, Gewicht), ausgeglichen(Mobile1), ausgeglichen(Mobile2).

mobile(Mobile,Gewicht) :- ausgeglichen(Mobile), gewicht(Mobile,Gewicht).

%Was liefern die folgenden Anfragen?
%| ?- mobile(bruecke(fisch(1),fisch(1)),2).
%       false.

%| ?- mobile(bruecke(fisch(1),fisch(1)),3).
%       true.

%| mobile(X,3).
% Es kommt ein Fehler. Das liegt daran, dass is eine partielles prädikat ist. 
% Bei der Auswertung wird die 2. Regel von Gewicht sind Mobile1 und Mobile2 unbelegt. 
% Dadurch wird der is Operator mit zwei unbelegten Variablen aufgerufen, und es kommt zu der Fehlermeldung