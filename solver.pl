% NOTE: solve/1 and map_blocks/4 predicates are heavily inspired by Prolog documentation
% at http://www.swi-prolog.org/pldoc/man?section=clpfd-sudoku

:- use_module(library(clpfd)).

% Main function which should be invoked from command line.  Input_file should be a string atom which is
% the path to the input file.
main(Input_file) :-
  % Create 16 Lists of unbound variables and bind given values to corresponding variables.  These lists
  % will become the puzzle.
  open(Input_file,read,Str),
  length(A, 16), read_line_to_int(Str, A),
  length(B, 16), read_line_to_int(Str, B),
  length(C, 16), read_line_to_int(Str, C),
  length(D, 16), read_line_to_int(Str, D),
  length(E, 16), read_line_to_int(Str, E),
  length(F, 16), read_line_to_int(Str, F),
  length(G, 16), read_line_to_int(Str, G),
  length(H, 16), read_line_to_int(Str, H),
  length(I, 16), read_line_to_int(Str, I),
  length(J, 16), read_line_to_int(Str, J),
  length(K, 16), read_line_to_int(Str, K),
  length(L, 16), read_line_to_int(Str, L),
  length(M, 16), read_line_to_int(Str, M),
  length(N, 16), read_line_to_int(Str, N),
  length(O, 16), read_line_to_int(Str, O),
  length(P, 16), read_line_to_int(Str, P),
  close(Str),

  Puzzle = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P],
  solve(Puzzle),
  write_puzzle(Puzzle).

% Search for solution by trying various bindings of unbound variables.
solve(Rows) :-
  append(Rows, Vs), Vs ins 1..16,
  maplist(all_distinct, Rows),
  transpose(Rows, Columns),
  maplist(all_distinct, Columns),
  Rows = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],
  map_blocks(A, B, C, D), map_blocks(E, F, G, H), map_blocks(I, J, K, L), map_blocks(M, N, O, P),
  maplist(label, Rows).

% Define blocks and ensure AllDiff among them.
map_blocks([], [], [], []).
map_blocks([A,B,C,D|Bs1], [E,F,G,H|Bs2], [I,J,K,L|Bs3], [M,N,O,P|Bs4]) :-
  all_distinct([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]),
  map_blocks(Bs1, Bs2, Bs3, Bs4).

% Read line of ASCII codes from Str and bind values given in input to appropriate variables in Hex_line.
read_line_to_int(Str, Hex_line) :-
  read_line_to_codes(Str, In_line),
  line_to_int(In_line, Hex_line).

% Recursively process list of ASCII characters (List1) and bind their integer values to corresponding
% variables in List2.
line_to_int([],[]).
line_to_int([H1|T1], [H2|T2]) :-
  convert(H1,H2),
  line_to_int(T1, T2).

% Reads an ACSII char as Input and binds its integer value with Output.  If Input is '.', takes no action
% thus leaving an unbound variable in that position.
convert(Input, Output) :-
  49 @=< Input, Input @=< 57 -> Output is Input - 48;
  97 @=< Input, Input @=< 102 -> Output is Input - 87;
  true.

% Print formatted puzzle.
write_puzzle([]).
write_puzzle([H|T]) :-
  write(H),
  nl,
  write_puzzle(T).
