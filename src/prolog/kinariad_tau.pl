% kinariad_tau.pl - K in a row (gravity)
% Tau-Prolog 0.3.4 compatible - pure ASCII comments
% Author: Avdieienko Dmytro Maksymovych
%
% NOTE: dynamic must be written as :- dynamic(Name/Arity). in Tau-Prolog consult.
% current_board/1 sentinel fact allows js_get_board before first js_init.

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
% Multimodality:
%   make_list(++,--) - create list of length N
make_list(0, []) :- !.
make_list(N, [_|T]) :- N > 0, N1 is N-1, make_list(N1, T).

% list_member(++Elem,++List) - membership without library(lists)
% Multimodality:
%   list_member(++,++) - check membership
list_member(X, [X|_]) :- !.
list_member(X, [_|T]) :- list_member(X, T).

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
    ( list_member((CR,CC), Forb) -> Cell = b ; Cell = e ),
    CC1 is CC + 1,
    ( CC1 > Cols -> NCC = 1, NCR is CR + 1 ; NCC = CC1, NCR = CR ),
    init_cells(Rest, Rows, Cols, Forb, NCR, NCC).

% ============================================================
% Cell access - direct list walk, no nth1 overhead
% ============================================================

% get_cell(++Board,++R,++C,--Val)
% Multimodality:
%   get_cell(++,++,++,--) - read cell value (O(R*Cols+C) walk)
%   get_cell(++,++,++,+)  - check cell value
get_cell(board(_,Cols,_,_,Cells), R, C, Val) :-
    Idx is (R-1)*Cols + C,
    list_nth1(Idx, Cells, Val).

% list_nth1(++Idx,++List,--Val) - fast 1-based list access, no library call
% Multimodality:
%   list_nth1(++,++,--) - access element at 1-based position
list_nth1(1, [H|_], H) :- !.
list_nth1(N, [_|T], H) :- N > 1, N1 is N-1, list_nth1(N1, T, H).

% set_cell(++Board,++R,++C,++Val,--NewBoard)
% Multimodality:
%   set_cell(++,++,++,++,--) - return new board with updated cell
set_cell(board(Rows,Cols,K,Forb,Cells), R, C, Val,
         board(Rows,Cols,K,Forb,NC)) :-
    Idx is (R-1)*Cols + C,
    replace_nth1(Idx, Cells, Val, NC).

% replace_nth1(++Idx,++List,++Val,--NewList)
% Multimodality:
%   replace_nth1(++,++,++,--) - replace element at 1-based position
replace_nth1(1, [_|T], Val, [Val|T]) :- !.
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

% available_cols(++Board,--Sorted)
% Returns columns sorted center-first for better alpha-beta cutoffs.
% Multimodality:
%   available_cols(++,--) - enumerate legal moves, center-first order
available_cols(Board, Sorted) :-
    Board = board(_,MaxC,_,_,_),
    Mid is (MaxC + 1) // 2,
    collect_avail(Board, 1, MaxC, Cols),
    sort_center(Cols, Mid, Sorted).

% collect_avail(++Board,++C,++MaxC,--Cols)
% Multimodality:
%   collect_avail(++,++,++,--) - collect available columns without findall
collect_avail(_, C, MaxC, []) :- C > MaxC, !.
collect_avail(Board, C, MaxC, Result) :-
    C1 is C + 1,
    ( get_cell(Board, 1, C, e) ->
        Result = [C|Rest],
        collect_avail(Board, C1, MaxC, Rest)
    ;
        collect_avail(Board, C1, MaxC, Result)
    ).

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
% Fast heuristic: count pieces weighted by column centrality.
% Avoids full window scan for speed in Tau-Prolog browser env.
% Multimodality:
%   eval_board(++,++,--) - compute heuristic score for Player
eval_board(Board, Player, Score) :-
    Board = board(_,Cols,_,_,_),
    Mid is (Cols + 1) // 2,
    opponent(Player, Opp),
    center_score(Board, Player, Opp, Mid, 0, Score).

% center_score(++Board,++P,++Opp,++Mid,++Acc,--Score)
% Walk the flat cell list, accumulating center-weighted score.
% Multimodality:
%   center_score(++,++,++,++,++,--) - iterative score accumulation
center_score(board(Rows,Cols,_,_,Cells), Player, Opp, Mid, Acc, Score) :-
    center_score_list(Cells, Player, Opp, Mid, Rows, Cols, 1, 1, Acc, Score).

center_score_list([], _, _, _, _, _, _, _, Acc, Acc).
center_score_list([V|T], Player, Opp, Mid, Rows, Cols, R, C, Acc, Score) :-
    ( V = Player ->
        W is 3 - min(2, abs(C - Mid)),
        NAcc is Acc + W
    ; V = Opp ->
        W is 3 - min(2, abs(C - Mid)),
        NAcc is Acc - W
    ;
        NAcc = Acc
    ),
    C1 is C + 1,
    ( C1 > Cols -> NC = 1, NR is R + 1 ; NC = C1, NR = R ),
    center_score_list(T, Player, Opp, Mid, Rows, Cols, NR, NC, NAcc, Score).

% ============================================================
% MinMax with Alpha-Beta pruning
% Depth measured in HALF-MOVES (piv-khody).
% Win check uses only the last move's landing cell (R,C)
% instead of win_exists (full board scan).
% ============================================================

% minimax(++Board,++LastR,++LastC,++LastP,++Depth,++Alpha,++Beta,++Player,++IsMax,--Score)
% Multimodality:
%   minimax(++,++,++,++,++,++,++,++,++,--) - evaluate game tree node
minimax(Board, LastR, LastC, LastP, _, _, _, Player, _, 100000) :-
    LastP \= Player, winner(Board, LastR, LastC, LastP), !.
minimax(Board, LastR, LastC, LastP, 0, _, _, Player, _, Score) :-
    !,
    ( LastP = Player, winner(Board, LastR, LastC, LastP) ->
        Score = 100000
    ;
        eval_board(Board, Player, Score)
    ).
minimax(Board, _, _, _, Depth, Alpha, Beta, Player, IsMax, Score) :-
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
    drop_piece(Board, C, Player, NB, Row),
    minimax(NB, Row, C, Player, D, CurA, Beta, NextP, false, CS),
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
    drop_piece(Board, C, Player, NB, Row),
    minimax(NB, Row, C, Player, D, Alpha, CurB, NextP, true, CS),
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
    drop_piece(Board, First, Player, NB0, Row0),
    minimax(NB0, Row0, First, Player, D1, -10000000, 10000000, NextP, false, S0),
    best_move_ab(Board, D1, S0, 10000000, Player, NextP, RestCols,
                 S0, First, _, BestCol).

% best_move_ab(++B,++D,++A,++Bt,++P,++NP,++Cols,++BestS,++BestC,--BS,--BC)
% Multimodality:
%   best_move_ab(++,++,++,++,++,++,++,++,++,--,--) - root alpha-beta
best_move_ab(_, _, _, _, _, _, [], BS, BC, BS, BC).
best_move_ab(Board, D, Alpha, Beta, Player, NextP, [C|Rest],
             CurBestS, CurBestC, FinalS, FinalC) :-
    drop_piece(Board, C, Player, NB, Row),
    minimax(NB, Row, C, Player, D, Alpha, Beta, NextP, false, CS),
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
