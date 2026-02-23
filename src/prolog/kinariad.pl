% Авдєєнко Дмитро Максимович
% Гра "K в ряд (падіння)" — поле n x m (n>=5, m>=6), k=[3..5]
% Заборонені комірки, MinMax + Alpha-Beta відсікання
% SWI-Prolog

:- use_module(library(lists)).
:- use_module(library(apply)).

% Board structure: board(Rows, Cols, K, Forbidden, Cells)
% Cells — flat list of size Rows*Cols
% Cell values: empty / x / o / blocked

%% new_game(++Rows, ++Cols, ++K, ++Forbidden, --Board)
% Create new board. Rows>=5, Cols>=6, K in [3..5].
% Multimodality:
%   new_game(++,++,++,++,--) — create board (main use)
%   new_game(++,++,++,++,+)  — verify board correctness
new_game(Rows, Cols, K, Forbidden, board(Rows,Cols,K,Forbidden,Cells)) :-
    Rows >= 5, Cols >= 6, K >= 3, K =< 5,
    Size is Rows * Cols,
    length(Cells, Size),
    init_cells(Cells, Rows, Cols, Forbidden, 1, 1).

%% init_cells(++Cells, ++Rows, ++Cols, ++Forb, ++CR, ++CC)
% Fill cells with empty or blocked based on Forbidden list.
% Multimodality:
%   init_cells(++,++,++,++,++,++) — deterministic initialization
init_cells([], _, _, _, _, _).
init_cells([Cell|Rest], Rows, Cols, Forb, CR, CC) :-
    ( memberchk((CR,CC), Forb) -> Cell = blocked ; Cell = empty ),
    CC1 is CC + 1,
    ( CC1 > Cols -> NCC = 1, NCR is CR+1 ; NCC = CC1, NCR = CR ),
    init_cells(Rest, Rows, Cols, Forb, NCR, NCC).

%% cell_index(++R, ++C, ++Cols, --Idx)
% Convert (Row,Col) to 1-based linear index, or reverse.
% Multimodality:
%   cell_index(++,++,++,--) — compute index
%   cell_index(--,--,++,++) — recover coordinates from index
cell_index(R, C, Cols, Idx) :-
    ( var(Idx) -> Idx is (R-1)*Cols + C
    ; R is (Idx-1)//Cols + 1, C is (Idx-1) mod Cols + 1 ).

%% get_cell(++Board, ++R, ++C, --Val)
% Get cell value at (R,C).
% Multimodality:
%   get_cell(++,++,++,--) — read cell
%   get_cell(++,++,++,+)  — check cell value
get_cell(board(_,Cols,_,_,Cells), R, C, Val) :-
    cell_index(R, C, Cols, Idx),
    nth1(Idx, Cells, Val).

%% set_cell(++Board, ++R, ++C, ++Val, --NewBoard)
% Return new board with cell (R,C) set to Val.
% Multimodality:
%   set_cell(++,++,++,++,--) — update board
set_cell(board(Rows,Cols,K,Forb,Cells), R, C, Val, board(Rows,Cols,K,Forb,NC)) :-
    cell_index(R, C, Cols, Idx),
    replace_nth1(Idx, Cells, Val, NC).

%% replace_nth1(++Idx, ++List, ++Val, --NewList)
% Replace 1-based element in list.
% Multimodality:
%   replace_nth1(++,++,++,--) — replace element
replace_nth1(1, [_|T], Val, [Val|T]).
replace_nth1(I, [H|T], Val, [H|NT]) :-
    I > 1, I1 is I-1, replace_nth1(I1, T, Val, NT).

% ============================================================
% Gravity: piece falls to lowest free row in column
% ============================================================

%% drop_piece(++Board, ++Col, ++Player, --NewBoard, --Row)
% Drop Player's piece in Col. Fails if column is full/blocked.
% Multimodality:
%   drop_piece(++,++,++,--,--) — main: drop piece
%   drop_piece(++,++,++,--,+)  — also check landing row
drop_piece(Board, Col, Player, NewBoard, Row) :-
    Board = board(Rows,_,_,_,_),
    find_bottom(Board, Col, Rows, Row),
    set_cell(Board, Row, Col, Player, NewBoard).

%% find_bottom(++Board, ++Col, ++CurR, --Row)
% Scan from bottom up for first empty cell in column.
% Multimodality:
%   find_bottom(++,++,++,--) — find free row
find_bottom(Board, Col, CurR, Row) :-
    CurR >= 1,
    get_cell(Board, CurR, Col, Val),
    ( Val = empty -> Row = CurR
    ; CurR1 is CurR-1, find_bottom(Board, Col, CurR1, Row) ).

%% col_available(++Board, ++Col)
% True if column has at least one empty cell (top row is empty).
% Multimodality:
%   col_available(++,++) — deterministic check
col_available(Board, Col) :-
    Board = board(_,Cols,_,_,_),
    Col >= 1, Col =< Cols,
    get_cell(Board, 1, Col, empty).

%% available_cols(++Board, --Cols)
% List of all columns with available space.
% Multimodality:
%   available_cols(++,--) — enumerate legal moves
available_cols(Board, Cols) :-
    Board = board(_,MaxC,_,_,_),
    findall(C, (between(1,MaxC,C), col_available(Board,C)), Cols).

% ============================================================
% Win detection
% ============================================================

%% winner(++Board, ++R, ++C, --Player)
% True if piece at (R,C) completes K in a row for Player.
% Multimodality:
%   winner(++,++,++,--) — detect winner after move
%   winner(++,++,++,+)  — check specific player
winner(Board, R, C, Player) :-
    get_cell(Board, R, C, Player),
    Player \= empty, Player \= blocked,
    Board = board(_,_,K,_,_),
    ( check_dir(Board, R, C, Player, K, 0, 1)
    ; check_dir(Board, R, C, Player, K, 1, 0)
    ; check_dir(Board, R, C, Player, K, 1, 1)
    ; check_dir(Board, R, C, Player, K, 1,-1) ).

%% check_dir(++Board, ++R, ++C, ++Player, ++K, ++DR, ++DC)
% Count consecutive Player pieces through (R,C) in direction (DR,DC).
% Win if total (both directions) >= K.
% Multimodality:
%   check_dir(++,++,++,++,++,++,++) — deterministic win check
check_dir(Board, R, C, Player, K, DR, DC) :-
    count_dir(Board, R, C, Player, DR, DC, 0, N1),
    NDR is -DR, NDC is -DC,
    count_dir(Board, R, C, Player, NDR, NDC, 0, N2),
    Total is N1 + N2 - 1,  % -1: (R,C) counted twice
    Total >= K.

%% count_dir(++Board, ++R, ++C, ++Player, ++DR, ++DC, ++Acc, --Count)
% Count Player pieces starting at (R,C) in direction (DR,DC).
% Multimodality:
%   count_dir(++,++,++,++,++,++,++,--) — directional count
count_dir(Board, R, C, Player, DR, DC, Acc, Count) :-
    Board = board(MaxR,MaxC,_,_,_),
    R >= 1, R =< MaxR, C >= 1, C =< MaxC,
    get_cell(Board, R, C, Player), !,
    Acc1 is Acc+1,
    NR is R+DR, NC is C+DC,
    count_dir(Board, NR, NC, Player, DR, DC, Acc1, Count).
count_dir(_, _, _, _, _, _, Acc, Acc).

%% win_exists(++Board, ++Player)
% True if Player has K in a row anywhere on board.
% Multimodality:
%   win_exists(++,++) — board-level win check
win_exists(Board, Player) :-
    Board = board(Rows,Cols,_,_,_),
    between(1,Rows,R), between(1,Cols,C),
    winner(Board, R, C, Player), !.

% ============================================================
% Position evaluation (heuristic)
% ============================================================

%% opponent(++P, --Opp)
% Map player to opponent symbol.
% Multimodality:
%   opponent(++,--) — get opponent
%   opponent(--,++) — reverse lookup
%   opponent(++,+)  — verify pair
opponent(x, o).
opponent(o, x).

%% eval_board(++Board, ++Player, --Score)
% Heuristic score: Player's threat points minus opponent's.
% Multimodality:
%   eval_board(++,++,--) — compute heuristic
eval_board(Board, Player, Score) :-
    opponent(Player, Opp),
    count_threats(Board, Player, MyPts),
    count_threats(Board, Opp, OppPts),
    Score is MyPts - OppPts.

%% count_threats(++Board, ++Player, --Pts)
% Sum heuristic points over all K-windows for Player.
% Multimodality:
%   count_threats(++,++,--) — sum threat scores
count_threats(Board, Player, Pts) :-
    Board = board(Rows,Cols,K,_,_),
    findall(P, window_pts(Board,Player,K,Rows,Cols,P), List),
    sumlist(List, Pts).

window_pts(Board, Player, K, MaxR, MaxC, Pts) :-  % horizontal
    between(1,MaxR,R), MCS is MaxC-K+1, between(1,MCS,C),
    window_score(Board, Player, K, R, C, 0, 1, Pts).
window_pts(Board, Player, K, MaxR, MaxC, Pts) :-  % vertical
    MRS is MaxR-K+1, between(1,MRS,R), between(1,MaxC,C),
    window_score(Board, Player, K, R, C, 1, 0, Pts).
window_pts(Board, Player, K, MaxR, MaxC, Pts) :-  % diagonal down-right
    MRS is MaxR-K+1, MCS is MaxC-K+1,
    between(1,MRS,R), between(1,MCS,C),
    window_score(Board, Player, K, R, C, 1, 1, Pts).
window_pts(Board, Player, K, MaxR, MaxC, Pts) :-  % diagonal up-right
    MRS is MaxR-K+1, between(K,MaxC,C), between(1,MRS,R),
    window_score(Board, Player, K, R, C, 1, -1, Pts).

%% window_score(++Board, ++Player, ++K, ++R, ++C, ++DR, ++DC, --Pts)
% Score for one K-window: 0 if opponent has any piece here.
% Multimodality:
%   window_score(++,++,++,++,++,++,++,--) — single window score
window_score(Board, Player, K, R, C, DR, DC, Pts) :-
    window_count(Board, Player, K, R, C, DR, DC, 0, 0, Mine, OppN),
    ( OppN > 0 -> Pts = 0 ; score_for(Mine, Pts) ).

%% window_count(++B,++P,++K,++R,++C,++DR,++DC,++AM,++AO,--M,--O)
% Count Player and opponent pieces in a K-length window.
% Multimodality:
%   window_count(++,++,++,++,++,++,++,++,++,--,--) — window counting
window_count(_, _, 0, _, _, _, _, AM, AO, AM, AO) :- !.
window_count(Board, Player, K, R, C, DR, DC, AM, AO, Mine, Opp) :-
    K > 0,
    get_cell(Board, R, C, Val),
    opponent(Player, OppP),
    ( Val=Player -> NM is AM+1, NO is AO
    ; Val=OppP   -> NM is AM,   NO is AO+1
    ;               NM is AM,   NO is AO ),
    K1 is K-1, NR is R+DR, NC is C+DC,
    window_count(Board, Player, K1, NR, NC, DR, DC, NM, NO, Mine, Opp).

%% score_for(++N, --Score)
% Points for N own pieces in an uncontested K-window.
score_for(1, 1).
score_for(2, 10).
score_for(3, 100).
score_for(4, 1000).
score_for(N, 100000) :- N >= 5.

% ============================================================
% MinMax з Alpha-Beta відсіканням
% Depth measured in HALF-MOVES (плівходах/пів-ходах)
% ============================================================

%% minimax(++Board, ++Depth, ++Alpha, ++Beta, ++Player, ++IsMax, --Score)
% Alpha-Beta minimax. IsMax=true for maximizer, false for minimizer.
% Depth = half-moves remaining.
%
% Multimodality:
%   minimax(++,++,++,++,++,++,--) — evaluate game tree node
minimax(Board, _, _, _, Player, _, 100000) :-
    opponent(Player, Opp), win_exists(Board, Opp), !.
minimax(Board, 0, _, _, Player, _, Score) :-
    !, eval_board(Board, Player, Score).
minimax(Board, Depth, Alpha, Beta, Player, IsMax, Score) :-
    available_cols(Board, Cols),
    ( Cols = [] ->
        eval_board(Board, Player, Score)
    ;
        opponent(Player, NextP),
        D1 is Depth-1,
        ( IsMax = true ->
            expand_max(Board, D1, Alpha, Beta, Player, NextP, Cols, Alpha, Score)
        ;
            expand_min(Board, D1, Alpha, Beta, Player, NextP, Cols, Beta, Score)
        )
    ).

%% expand_max(++B,++D,++A,++Bt,++P,++NP,++Cols,++CurA,--Best)
% Maximizer node: update Alpha, beta-cutoff.
% Multimodality:
%   expand_max(++,++,++,++,++,++,++,++,--) — maximizer expansion
expand_max(_, _, _, _, _, _, [], CurA, CurA).
expand_max(Board, D, Alpha, Beta, Player, NextP, [C|Rest], CurA, Best) :-
    drop_piece(Board, C, Player, NB, _),
    minimax(NB, D, CurA, Beta, NextP, false, CS),
    NewA is max(CurA, CS),
    ( NewA >= Beta -> Best = NewA   % beta-cutoff
    ; expand_max(Board, D, Alpha, Beta, Player, NextP, Rest, NewA, Best) ).

%% expand_min(++B,++D,++A,++Bt,++P,++NP,++Cols,++CurB,--Best)
% Minimizer node: update Beta, alpha-cutoff.
% Multimodality:
%   expand_min(++,++,++,++,++,++,++,++,--) — minimizer expansion
expand_min(_, _, _, _, _, _, [], CurB, CurB).
expand_min(Board, D, Alpha, Beta, Player, NextP, [C|Rest], CurB, Best) :-
    drop_piece(Board, C, Player, NB, _),
    minimax(NB, D, Alpha, CurB, NextP, true, CS),
    NewB is min(CurB, CS),
    ( NewB =< Alpha -> Best = NewB  % alpha-cutoff
    ; expand_min(Board, D, Alpha, Beta, Player, NextP, Rest, NewB, Best) ).

%% best_move(++Board, ++Player, ++Depth, --Col)
% Find best column for Player using minimax at given half-move depth.
% Multimodality:
%   best_move(++,++,++,--) — AI move selection
best_move(Board, Player, Depth, BestCol) :-
    available_cols(Board, Cols),
    Cols \= [],
    opponent(Player, NextP),
    D1 is Depth-1,
    findall(S-C,
        ( member(C, Cols),
          drop_piece(Board, C, Player, NB, _),
          minimax(NB, D1, -10000000, 10000000, NextP, false, S)
        ),
        Scored),
    max_member(_-BestCol, Scored).

% ============================================================
% Game interface predicates
% ============================================================

%% human_move(++Board, ++Col, ++Player, --NewBoard)
% Execute human move in Col. Fails if column unavailable.
% Multimodality:
%   human_move(++,++,++,--) — validated human move
human_move(Board, Col, Player, NewBoard) :-
    col_available(Board, Col),
    drop_piece(Board, Col, Player, NewBoard, _).

%% prolog_move(++Board, ++Player, ++Depth, --NewBoard, --Col)
% Execute AI move using MinMax+Alpha-Beta at Depth half-moves.
% Multimodality:
%   prolog_move(++,++,++,--,--) — AI turn
prolog_move(Board, Player, Depth, NewBoard, Col) :-
    best_move(Board, Player, Depth, Col),
    drop_piece(Board, Col, Player, NewBoard, _).

% ============================================================
% Console display
% ============================================================

%% print_board(++Board)
% Print board to console.
% Multimodality:
%   print_board(++) — side-effect only (display)
print_board(Board) :-
    Board = board(Rows,Cols,K,Forb,_),
    format("~nBoard ~wx~w K=~w Forbidden=~w~n",[Rows,Cols,K,Forb]),
    print_col_nums(1, Cols),
    print_rows(Board, 1, Rows, Cols).

print_col_nums(C, MaxC) :- C > MaxC, !, nl.
print_col_nums(C, MaxC) :-
    format(" ~w  ", [C]), C1 is C+1, print_col_nums(C1, MaxC).

print_rows(_, R, MaxR, _) :- R > MaxR, !.
print_rows(Board, R, MaxR, MaxC) :-
    print_row_cells(Board, R, 1, MaxC), nl,
    R1 is R+1, print_rows(Board, R1, MaxR, MaxC).

print_row_cells(_, _, C, MaxC) :- C > MaxC, !.
print_row_cells(Board, R, C, MaxC) :-
    get_cell(Board, R, C, Val), cell_char(Val, Ch),
    format("[~w] ", [Ch]), C1 is C+1, print_row_cells(Board, R, C1, MaxC).

cell_char(empty,   ' ').
cell_char(blocked, '#').
cell_char(x,       'X').
cell_char(o,       'O').

% ============================================================
% Приклади запитів (формат SWI-Prolog):
%
% /** <examples>
% % Приклад 1: нова гра, хід людини, хід Prolog
% ?- new_game(5, 6, 4, [], B),
%    human_move(B, 3, x, B1),
%    prolog_move(B1, o, 6, B2, C),
%    format("Prolog played column ~w~n", [C]),
%    print_board(B2).
%
% Очікуваний результат: Prolog зіграє у стовпець (зазвичай центральний)
%
% % Приклад 2: заборонені клітини
% ?- new_game(6, 7, 4, [(3,3),(3,4)], B),
%    print_board(B).
%
% Очікуваний результат: '#' у позиціях (3,3) та (3,4)
%
% % Приклад 3: перевірка перемоги (вертикаль)
% ?- new_game(5, 6, 3, [], B0),
%    human_move(B0, 1, x, B1),
%    human_move(B1, 1, x, B2),
%    human_move(B2, 1, x, B3),
%    ( win_exists(B3, x) -> write('X won!') ; write('game on') ), nl.
%
% Очікуваний результат: X won!
%
% % Приклад 4: Prolog vs Prolog, 8 пів-ходів
% ?- new_game(6, 7, 4, [], B0),
%    prolog_move(B0, x, 8, B1, C1),
%    prolog_move(B1, o, 8, B2, C2),
%    format("x->~w  o->~w~n", [C1,C2]),
%    print_board(B2).
% */
% ============================================================
