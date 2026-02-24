% kinariad_tau.pl - K in a row (gravity)
% Tau-Prolog 0.3.4 compatible - pure ASCII comments
% Author: Avdieienko Dmytro Maksymovych
%
% NOTE: dynamic must be written as :- dynamic(Name/Arity). in Tau-Prolog consult.
% current_board/1 sentinel fact allows js_get_board before first js_init.

:- use_module(library(lists)).
:- dynamic(current_board/1).

% Sentinel - makes current_board/1 known to the DB so retractall won't throw.
% JS init code replaces this immediately via retractall+assertz.
current_board(none).

% Board: board(Rows, Cols, K, Forbidden, Cells)
% Cells - flat list Rows*Cols, values: e/x/o/b (empty/x/o/blocked)

% ============================================================
% Board construction
% ============================================================

% make_list(++N,--List) - create a list of N uninstantiated variables
% Replaces length/2 which requires lists module in Tau-Prolog.
% Multimodality:
%   make_list(++,--) - create list of length N
make_list(0, []) :- !.
make_list(N, [_|T]) :- N > 0, N1 is N-1, make_list(N1, T).

% new_game(++Rows,++Cols,++K,++Forb,--Board)
% Multimodality:
%   new_game(++,++,++,++,--) - create board (main use)
%   new_game(++,++,++,++,+)  - verify board parameters
new_game(Rows, Cols, K, Forb, board(Rows,Cols,K,Forb,Cells)) :-
    Size is Rows * Cols,
    make_list(Size, Cells),
    init_cells(Cells, Rows, Cols, Forb, 1, 1).

% init_cells(++Cells,++Rows,++Cols,++Forb,++CR,++CC)
% Multimodality:
%   init_cells(++,++,++,++,++,++) - deterministic initialization
init_cells([], _, _, _, _, _).
init_cells([Cell|Rest], Rows, Cols, Forb, CR, CC) :-
    ( member((CR,CC), Forb) -> Cell = b ; Cell = e ),
    CC1 is CC + 1,
    ( CC1 > Cols -> NCC = 1, NCR is CR + 1 ; NCC = CC1, NCR = CR ),
    init_cells(Rest, Rows, Cols, Forb, NCR, NCC).

% ============================================================
% Cell access
% ============================================================

% cell_idx(++R,++C,++Cols,--Idx) or cell_idx(--R,--C,++Cols,++Idx)
% Multimodality:
%   cell_idx(++,++,++,--) - compute 1-based index
%   cell_idx(--,--,++,++) - recover coordinates from index
cell_idx(R, C, Cols, Idx) :-
    ( var(Idx) ->
        Idx is (R-1)*Cols + C
    ;
        R is (Idx-1)//Cols + 1,
        C is (Idx-1) mod Cols + 1
    ).

% get_cell(++Board,++R,++C,--Val)
% Multimodality:
%   get_cell(++,++,++,--) - read cell value
%   get_cell(++,++,++,+)  - check cell value
get_cell(board(_,Cols,_,_,Cells), R, C, Val) :-
    cell_idx(R, C, Cols, Idx),
    nth1(Idx, Cells, Val).

% set_cell(++Board,++R,++C,++Val,--NewBoard)
% Multimodality:
%   set_cell(++,++,++,++,--) - return new board with updated cell
set_cell(board(Rows,Cols,K,Forb,Cells), R, C, Val,
         board(Rows,Cols,K,Forb,NC)) :-
    cell_idx(R, C, Cols, Idx),
    replace_nth1(Idx, Cells, Val, NC).

% replace_nth1(++Idx,++List,++Val,--NewList)
% Multimodality:
%   replace_nth1(++,++,++,--) - replace element at 1-based position
replace_nth1(1, [_|T], Val, [Val|T]).
replace_nth1(I, [H|T], Val, [H|NT]) :-
    I > 1, I1 is I-1,
    replace_nth1(I1, T, Val, NT).

% ============================================================
% Gravity: piece falls to lowest free row in column
% ============================================================

% drop_piece(++Board,++Col,++Player,--NewBoard,--Row)
% Multimodality:
%   drop_piece(++,++,++,--,--) - drop piece, get new board and landing row
%   drop_piece(++,++,++,--,+)  - also verify landing row
drop_piece(Board, Col, Player, NewBoard, Row) :-
    Board = board(Rows,_,_,_,_),
    find_bottom(Board, Col, Rows, Row),
    set_cell(Board, Row, Col, Player, NewBoard).

% find_bottom(++Board,++Col,++CurR,--Row)
% Multimodality:
%   find_bottom(++,++,++,--) - scan bottom-up for first empty cell
find_bottom(Board, Col, CurR, Row) :-
    CurR >= 1,
    get_cell(Board, CurR, Col, Val),
    ( Val = e ->
        Row = CurR
    ;
        CurR1 is CurR - 1,
        find_bottom(Board, Col, CurR1, Row)
    ).

% col_available(++Board,++Col)
% Multimodality:
%   col_available(++,++) - check if top cell is empty (can drop piece)
col_available(Board, Col) :-
    Board = board(_,Cols,_,_,_),
    Col >= 1, Col =< Cols,
    get_cell(Board, 1, Col, e).

% available_cols(++Board,--Cols)
% Returns columns sorted center-first for better alpha-beta cutoffs.
% Multimodality:
%   available_cols(++,--) - enumerate legal moves, center-first order
available_cols(Board, Sorted) :-
    Board = board(_,MaxC,_,_,_),
    Mid is (MaxC + 1) // 2,
    findall(C, (between(1,MaxC,C), col_available(Board,C)), Cols),
    sort_center(Cols, Mid, Sorted).

% sort_center(++Cols,++Mid,--Sorted)
% Sort columns by distance to center using insertion sort (avoids msort).
% Multimodality:
%   sort_center(++,++,--) - center-first column ordering
sort_center([], _, []).
sort_center([H|T], Mid, Sorted) :-
    sort_center(T, Mid, SortedT),
    DH is abs(H - Mid),
    insert_by_dist(H, DH, SortedT, Mid, Sorted).

% insert_by_dist(++Col,++Dist,++SortedList,++Mid,--Result)
% Multimodality:
%   insert_by_dist(++,++,++,++,--) - insertion step for center-first sort
insert_by_dist(C, _, [], _, [C]).
insert_by_dist(C, DC, [H|T], Mid, [C,H|T]) :-
    DH is abs(H - Mid),
    DC =< DH, !.
insert_by_dist(C, DC, [H|T], Mid, [H|Rest]) :-
    insert_by_dist(C, DC, T, Mid, Rest).

% ============================================================
% Win detection
% ============================================================

% opponent(++P,--Opp)
% Multimodality:
%   opponent(++,--) - get opponent symbol
%   opponent(--,++) - reverse lookup (symmetric)
%   opponent(++,+)  - verify pair
opponent(x, o).
opponent(o, x).

% winner(++Board,++R,++C,--Player)
% Multimodality:
%   winner(++,++,++,--) - detect winner after move at (R,C)
%   winner(++,++,++,+)  - check if specific player won at (R,C)
winner(Board, R, C, Player) :-
    get_cell(Board, R, C, Player),
    Player \= e, Player \= b,
    Board = board(_,_,K,_,_),
    ( check_dir(Board, R, C, Player, K, 0, 1)
    ; check_dir(Board, R, C, Player, K, 1, 0)
    ; check_dir(Board, R, C, Player, K, 1, 1)
    ; check_dir(Board, R, C, Player, K, 1, -1)
    ).

% check_dir(++Board,++R,++C,++Player,++K,++DR,++DC)
% Multimodality:
%   check_dir(++,++,++,++,++,++,++) - directional win check
check_dir(Board, R, C, Player, K, DR, DC) :-
    count_dir(Board, R, C, Player, DR, DC, 0, N1),
    NDR is -DR, NDC is -DC,
    count_dir(Board, R, C, Player, NDR, NDC, 0, N2),
    Total is N1 + N2 - 1,
    Total >= K.

% count_dir(++Board,++R,++C,++Player,++DR,++DC,++Acc,--Count)
% Multimodality:
%   count_dir(++,++,++,++,++,++,++,--) - directional piece count
count_dir(Board, R, C, Player, DR, DC, Acc, Count) :-
    Board = board(MaxR,MaxC,_,_,_),
    R >= 1, R =< MaxR, C >= 1, C =< MaxC,
    get_cell(Board, R, C, Player), !,
    Acc1 is Acc + 1,
    NR is R+DR, NC is C+DC,
    count_dir(Board, NR, NC, Player, DR, DC, Acc1, Count).
count_dir(_, _, _, _, _, _, Acc, Acc).

% win_exists(++Board,++Player)
% Multimodality:
%   win_exists(++,++) - check if player has K-in-a-row anywhere
win_exists(Board, Player) :-
    Board = board(Rows,Cols,_,_,_),
    between(1,Rows,R), between(1,Cols,C),
    winner(Board, R, C, Player), !.

% ============================================================
% Heuristic evaluation
% ============================================================

% eval_board(++Board,++Player,--Score)
% Multimodality:
%   eval_board(++,++,--) - compute heuristic score for Player
eval_board(Board, Player, Score) :-
    opponent(Player, Opp),
    count_threats(Board, Player, MyPts),
    count_threats(Board, Opp, OppPts),
    Score is MyPts - OppPts.

% count_threats(++Board,++Player,--Pts)
% Multimodality:
%   count_threats(++,++,--) - aggregate window scores for Player
count_threats(Board, Player, Pts) :-
    Board = board(Rows,Cols,K,_,_),
    findall(P, window_pts(Board,Player,K,Rows,Cols,P), List),
    sum_list_tau(List, Pts).

% sum_list_tau(++List,--Sum) - portable sum, avoids library dependency
% Multimodality:
%   sum_list_tau(++,--) - sum numeric list
sum_list_tau([], 0).
sum_list_tau([H|T], S) :- sum_list_tau(T, S1), S is S1 + H.

% window_pts - generate all K-windows in four directions
window_pts(Board, Player, K, MaxR, MaxC, Pts) :-
    between(1,MaxR,R), MCS is MaxC-K+1, between(1,MCS,C),
    window_score(Board, Player, K, R, C, 0, 1, Pts).
window_pts(Board, Player, K, MaxR, MaxC, Pts) :-
    MRS is MaxR-K+1, between(1,MRS,R), between(1,MaxC,C),
    window_score(Board, Player, K, R, C, 1, 0, Pts).
window_pts(Board, Player, K, MaxR, MaxC, Pts) :-
    MRS is MaxR-K+1, MCS is MaxC-K+1,
    between(1,MRS,R), between(1,MCS,C),
    window_score(Board, Player, K, R, C, 1, 1, Pts).
window_pts(Board, Player, K, MaxR, MaxC, Pts) :-
    MRS is MaxR-K+1, between(K,MaxC,C), between(1,MRS,R),
    window_score(Board, Player, K, R, C, 1, -1, Pts).

% window_score(++Board,++Player,++K,++R,++C,++DR,++DC,--Pts)
% Multimodality:
%   window_score(++,++,++,++,++,++,++,--) - single window heuristic
window_score(Board, Player, K, R, C, DR, DC, Pts) :-
    win_count(Board, Player, K, R, C, DR, DC, 0, 0, Mine, OppN),
    ( OppN > 0 -> Pts = 0 ; score_for(Mine, Pts) ).

% win_count(++B,++P,++K,++R,++C,++DR,++DC,++AM,++AO,--M,--O)
% Multimodality:
%   win_count(++,++,++,++,++,++,++,++,++,--,--) - window counting
win_count(_, _, 0, _, _, _, _, AM, AO, AM, AO) :- !.
win_count(Board, Player, K, R, C, DR, DC, AM, AO, Mine, Opp) :-
    K > 0,
    get_cell(Board, R, C, Val),
    opponent(Player, OppP),
    ( Val = Player -> NM is AM+1, NO is AO
    ; Val = OppP   -> NM is AM,   NO is AO+1
    ;                 NM is AM,   NO is AO ),
    K1 is K-1, NR is R+DR, NC is C+DC,
    win_count(Board, Player, K1, NR, NC, DR, DC, NM, NO, Mine, Opp).

% score_for(++N,--Score) - exponential threat scoring
% Multimodality:
%   score_for(++,--) - map piece count to heuristic value
score_for(1, 1).
score_for(2, 10).
score_for(3, 100).
score_for(4, 1000).
score_for(N, 100000) :- N >= 5.

% ============================================================
% MinMax with Alpha-Beta pruning
% Depth measured in HALF-MOVES (piv-khody)
% ============================================================

% minimax(++Board,++Depth,++Alpha,++Beta,++Player,++IsMax,--Score)
% Multimodality:
%   minimax(++,++,++,++,++,++,--) - evaluate game tree node, return score
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
        D1 is Depth - 1,
        ( IsMax = true ->
            expand_max(Board, D1, Alpha, Beta, Player, NextP, Cols, Alpha, Score)
        ;
            expand_min(Board, D1, Alpha, Beta, Player, NextP, Cols, Beta, Score)
        )
    ).

% expand_max(++B,++D,++A,++Bt,++P,++NP,++Cols,++CurA,--Best)
% Multimodality:
%   expand_max(++,++,++,++,++,++,++,++,--) - maximizer expansion
expand_max(_, _, _, _, _, _, [], CurA, CurA).
expand_max(Board, D, Alpha, Beta, Player, NextP, [C|Rest], CurA, Best) :-
    drop_piece(Board, C, Player, NB, _),
    minimax(NB, D, CurA, Beta, NextP, false, CS),
    NewA is max(CurA, CS),
    ( NewA >= Beta ->
        Best = NewA
    ;
        expand_max(Board, D, Alpha, Beta, Player, NextP, Rest, NewA, Best)
    ).

% expand_min(++B,++D,++A,++Bt,++P,++NP,++Cols,++CurB,--Best)
% Multimodality:
%   expand_min(++,++,++,++,++,++,++,++,--) - minimizer expansion
expand_min(_, _, _, _, _, _, [], CurB, CurB).
expand_min(Board, D, Alpha, Beta, Player, NextP, [C|Rest], CurB, Best) :-
    drop_piece(Board, C, Player, NB, _),
    minimax(NB, D, Alpha, CurB, NextP, true, CS),
    NewB is min(CurB, CS),
    ( NewB =< Alpha ->
        Best = NewB
    ;
        expand_min(Board, D, Alpha, Beta, Player, NextP, Rest, NewB, Best)
    ).

% best_move(++Board,++Player,++Depth,--BestCol)
% Multimodality:
%   best_move(++,++,++,--) - AI move selection (main entry point)
best_move(Board, Player, Depth, BestCol) :-
    available_cols(Board, [First|RestCols]),
    opponent(Player, NextP),
    D1 is Depth - 1,
    drop_piece(Board, First, Player, NB0, _),
    minimax(NB0, D1, -10000000, 10000000, NextP, false, S0),
    best_move_ab(Board, D1, S0, 10000000, Player, NextP, RestCols,
                 S0, First, _, BestCol).

% best_move_ab(++B,++D,++A,++Bt,++P,++NP,++Cols,++BestS,++BestC,--BS,--BC)
% Multimodality:
%   best_move_ab(++,++,++,++,++,++,++,++,++,--,--) - root alpha-beta
best_move_ab(_, _, _, _, _, _, [], BS, BC, BS, BC).
best_move_ab(Board, D, Alpha, Beta, Player, NextP, [C|Rest],
             CurBestS, CurBestC, FinalS, FinalC) :-
    drop_piece(Board, C, Player, NB, _),
    minimax(NB, D, Alpha, Beta, NextP, false, CS),
    ( CS > CurBestS ->
        NewBestS = CS, NewBestC = C,
        NewAlpha is max(Alpha, CS)
    ;
        NewBestS = CurBestS, NewBestC = CurBestC,
        NewAlpha = Alpha
    ),
    best_move_ab(Board, D, NewAlpha, Beta, Player, NextP, Rest,
                 NewBestS, NewBestC, FinalS, FinalC).

% ============================================================
% STATE via assert - board stored in Prolog DB between JS calls
% js_init/3 now receives forbidden list directly (no dynamic forbidden_list)
% ============================================================

% js_init(++Rows,++Cols,++K,++Forb)
% Create new game. Forb is a Prolog list of (R,C) pairs.
% Multimodality:
%   js_init(++,++,++,++) - initialize game state in DB
js_init(Rows, Cols, K, Forb) :-
    new_game(Rows, Cols, K, Forb, Board),
    retractall(current_board(_)),
    assertz(current_board(Board)).

% js_human(++Col,++Player,--RowOut,--Result)
% Execute human move. Result: ok/full/win/draw.
% Multimodality:
%   js_human(++,++,--,--) - human move with result reporting
js_human(Col, Player, RowOut, Result) :-
    current_board(Board),
    ( col_available(Board, Col) ->
        drop_piece(Board, Col, Player, NewBoard, Row),
        retractall(current_board(_)),
        assertz(current_board(NewBoard)),
        RowOut = Row,
        ( winner(NewBoard, Row, Col, Player) ->
            Result = win
        ; available_cols(NewBoard, []) ->
            Result = draw
        ;
            Result = ok
        )
    ;
        RowOut = -1,
        Result = full
    ).

% js_ai(++Player,++Depth,--ColOut,--RowOut,--Result)
% Execute AI move using MinMax+Alpha-Beta.
% Multimodality:
%   js_ai(++,++,--,--,--) - AI move with result reporting
js_ai(Player, Depth, ColOut, RowOut, Result) :-
    current_board(Board),
    ( available_cols(Board, []) ->
        ColOut = -1, RowOut = -1, Result = draw
    ;
        best_move(Board, Player, Depth, Col),
        drop_piece(Board, Col, Player, NewBoard, Row),
        retractall(current_board(_)),
        assertz(current_board(NewBoard)),
        ColOut = Col, RowOut = Row,
        ( winner(NewBoard, Row, Col, Player) ->
            Result = win
        ; available_cols(NewBoard, []) ->
            Result = draw
        ;
            Result = ok
        )
    ).

% js_get_board(--Rows,--Cols,--K,--CellList)
% Multimodality:
%   js_get_board(--,--,--,--) - read-only board access for JS renderer
js_get_board(Rows, Cols, K, Cells) :-
    current_board(board(Rows,Cols,K,_,Cells)).
