% Author: Amina Anna Mahamane Ousmane
% Date: Mercredi 12 Avril, 2023
% Student 300227147
% Command: consult("RANSAC").

% --------- Creating a list containing tous all the points of the cloud ------------------

read_xyz_file(File, Points) :- open(File, read, Stream), 
read_xyz_points(Stream,Points), 
close(Stream). 

read_xyz_points(Stream, []) :- 
    at_end_of_stream(Stream). 

read_xyz_points(Stream, [Point|Points]) :- 
    \+ at_end_of_stream(Stream), 
    read_line_to_string(Stream,L), 
    split_string(L, "\t", "\s\t\n", XYZ),
    convert_to_float(XYZ,Point), 
    read_xyz_points(Stream, Points).

convert_to_float([],[]). 

convert_to_float([H|T],[HH|TT]) :- 
    atom_number(H, HH), 
    convert_to_float(T,TT). 

% -------------------- PREDICATES ----------------------

% random3points(Points, Point3)
% True if Point3 is a randomly selected triplet of points from the list of points Points
random3points(Points, Point3) :-
    random_select(X1, Points, RemainingPoints1),
    random_select(X2, RemainingPoints1, RemainingPoints2),
    random_select(X3, RemainingPoints2, []),
    Point3 = [X1, X2, X3].

% plane(Point3, Plane)
% True if Plane is the equation of the plane passing through the three points in Point3
% Plane is specified by the list [a,b,c,d] from the equation ax+by+cz=d.
% The list of points has the form [[x1,y1,z1], [x2,y2,z2], [x3,y3,z3]].
plane(Point3, Plane) :-
    nth0(0, Point3, [X1,Y1,Z1]),
    nth0(1, Point3, [X2,Y2,Z2]),
    nth0(2, Point3, [X3,Y3,Z3]),
    A is Y1*(Z2-Z3) + Y2*(Z3-Z1) + Y3*(Z1-Z2),
    B is Z1*(X2-X3) + Z2*(X3-X1) + Z3*(X1-X2),
    C is X1*(Y2-Y3) + X2*(Y3-Y1) + X3*(Y1-Y2),
    D is -X1*A - Y1*B - Z1*C,
    Plane = [A,B,C,D].

% support(Plane, Points, Eps, N)
% True if the support of the plane Plane is composed of N points from the list of points Points 
% when the distance Eps is used.
support(Plane, Points, Eps, N) :-
    findall(P, (member(P, Points), point_to_plane_distance(P, Plane, D), D < Eps), Inliers),
    length(Inliers, InlierCount),
    InlierCount >= N.

% ransac_number_of_iterations(Confidence, Percentage, N)
% True if N is the number of iterations required by RANSAC with the parameters Confidence and Percentage
% using the formula N = log(1-C)/log(1-p^3) where C is Confidence and P is Percentage.
ransac_number_of_iterations(Confidence, Percentage, N) :-
    P is Percentage,
    C is Confidence,
    P_cubed is P**3,
    (P_cubed =< 1 ->
        N is ceiling(log(1-C)/log(1-P_cubed))
    ;   N = 0
    ).
% point_to_plane_distance(Point, Plane, Distance)
% Computes the distance between a point and a plane using the formula Ax+By+Cz+D/sqrt(A^2+B^2+C^2).
point_to_plane_distance([X,Y,Z], [A,B,C,D], Distance) :-
    Distance is abs(A*X + B*Y + C*Z + D) / sqrt(A**2 + B**2 + C**2).


% ---------------- TESTS  ---------------------

%Tests for plane predicate
test(plane, 1) :- plane([[1,0,0], [0,1,0], [0,0,0]], Plane), Plane = [0,0,1,0].
test(plane, 2) :- plane([[0,0,0], [1,1,1], [1,0,0]], Plane), Plane = [-1,1,1,0].
test(plane, 3) :- plane([[1,0,0], [0,1,0], [0,0,1]], Plane), Plane = [1,1,1,0].
test(plane, 4) :- plane([[0,0,0], [1,0,0], [0,1,0]], [0,0,1,0]).
test(plane, 5) :- plane([[1,2,3],[4,5,6],[7,8,9]], [A,B,C,D]), 
                  A =:= -3, B =:= 6, C =:= -3, D =:= -12.

%Tests for support predicate
test(support, 1) :- support([0,0,1,0], [[1,0,0], [0,1,0], [0,0,0], [0,0,1]], 0.1, 3).
test(support, 2) :- support([-1,1,1,0], [[0,0,0], [1,1,1], [1,0,0], [0,1,0], [0,0,1], [1,1,0]], 0.01, 4).
test(support, 3) :- support([1,1,1,0], [[0,0,0], [1,1,1], [1,0,0], [0,1,0], [0,0,1], [1,1,0]], 0.5, 1).
test(support, 4) :- support([0,0,1,0], [[0,0,0], [1,0,0], [0,1,0], [0,0,1]], 0.5, 3).
test(support, 5) :- support([1,1,1,0], [[0,0,0],[1,1,1],[2,2,2],[3,3,3],[4,4,4],[5,5,5],[6,6,6],[7,7,7]], 1, 3).

%Tests for ranac_number_of_iterations predicate
test(ransac_number_of_iterations, 1) :- ransac_number_of_iterations(0.95, 0.5, N), N = 23.
test(ransac_number_of_iterations, 2) :- ransac_number_of_iterations(0.99, 0.1, N), N = 17.
test(ransac_number_of_iterations, 3) :- ransac_number_of_iterations(0.80, 0.8, N), N = 3.
test(ransac_number_of_iterations, 4) :- ransac_number_of_iterations(0.99, 0.5, 35).
test(ransac_number_of_iterations, 5) :- ransac_number_of_iterations(0.95, 0.5, N), N =:= 72.

% Test pour random3points. L'approche utiliser pour test l'aléatoire selection de ce prédicat est la suivante :
/* 1) Créer une liste de test avec un nombre suffisamment grand de points (par exemple, 1000 points).
   2) Appeler la fonction plusieurs fois (par exemple, 100 fois) avec la même liste en entrée et stocker les résultats dans une autre liste.
   3) Vérifier que chaque résultat est une liste de 3 éléments différents.
   4) Vérifier que chaque résultat est unique (c'est-à-dire qu'il n'y a pas de doublons dans la liste de résultats).
   5) Vérifier que les résultats sont distribués de manière équitable. */

:- use_module(library(random)).
:- use_module(library(lists)).

% Prédicat pour choisir 3 points aléatoires
choisir_trois_points(Liste, Points) :-
    random_select(X1, Liste, RemainingPoints1),
    random_select(X2, RemainingPoints1, RemainingPoints2),
    random_select(X3, RemainingPoints2, []),
    Points = [X1, X2, X3].

% Créer une liste de test avec 1000 points
points(Liste) :-
    numlist(1, 1000, Liste).

% Appeler le prédicat 100 fois et stocker les résultats dans une liste
resultats(Resultats) :-
    points(Liste),
    findall(Points, (between(1, 100, _), choisir_trois_points(Liste, Points)), Resultats).

% Vérifier que chaque résultat est une liste de 3 éléments différents
test1(Resultats) :-
    forall(member(R, Resultats), length(R, 3)),
    forall((member(R1, Resultats), member(R2, Resultats), R1 \= R2), \+ permutation(R1, R2)).

% Vérifier que chaque résultat est unique
test2(Resultats) :-
    length(Resultats, N),
    sort(Resultats, ResultatsTries),
    length(ResultatsTries, NTries),
    N == NTries.

% Prédicat pour calculer le nombre de combinaisons possibles
attendu(Nb) :-
    Nb is 1000*(1000-1)*(1000-2)//6//100.

% Prédicat pour compter les occurrences de chaque combinaison
compte(Resultats, Compte) :-
    maplist(sort, Resultats, ResultatsTries),
    msort(ResultatsTries, ResultatsTriesTries),
    compte(1, ResultatsTriesTries, [], Compte).

compte(_, [], Compte, Compte).
compte(N, [X|Xs], [], Compte) :-
    compte(N, Xs, [count(X, N)|Compte], Compte).
compte(N, [X|Xs], [count(X,M)|Compte], Res) :-
    !,
    O is N+1,
    compte(O, Xs, [count(X,M)|Compte], Res).
compte(N, [X|Xs], Compte, Res) :-
    compte(1, Xs, [count(X,N)|Compte], Res).

% Prédicat pour comparer une occurrence au nombre attendu
compare(Nb, count(_, Occ)) :-
    Ecart is abs(Occ - Nb),
    Ecart < 10.

% Test complet
test_random3pts :-
    resultats(Resultats),
    test1(Resultats),
    test2(Resultats).



  

