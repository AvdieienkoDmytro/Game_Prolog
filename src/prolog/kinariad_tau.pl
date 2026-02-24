% kinariad_tau.pl - K in a row (gravity), Tau-Prolog 0.3.4
% Author: Avdieienko Dmytro Maksymovych
%
% STATELESS design: board passed as atom (flat string Rows*Cols chars).
% Cell chars: e=empty, x=player X, o=player O, b=blocked.
% All board state is kept in JS; Prolog only computes AI moves.
%
% Internally board is stored as list of rows (list of char lists)
% for O(R+C) cell access instead of O(R*C).

% ============================================================
% atom <-> row-list conversion
% ============================================================

% atom_to_board(++Atom,++Cols,--RowList)
atom_to_board(Atom, Cols, RowList) :-
    atom_chars(Atom, Flat),
    split_rows(Flat, Cols, RowList).

% board_to_atom(++RowList,--Atom)
board_to_atom(RowList, Atom) :-
    flatten_rows(RowList, Flat),
    atom_chars(Atom, Flat).

split_rows([], _, []) :- !.
split_rows(Flat, Cols, [Row|Rest]) :-
    take(Flat, Cols, Row, Tail),
    split_rows(Tail, Cols, Rest).

take(L, 0, [], L) :- !.
take([H|T], N, [H|R], Tail) :- N > 0, N1 is N-1, take(T, N1, R, Tail).

flatten_rows([], []).
flatten_rows([Row|Rs], Flat) :-
    flatten_rows(Rs, Rest),
    concat_row(Row, Rest, Flat).

concat_row([], L, L).
concat_row([H|T], L, [H|R]) :- concat_row(T, L, R).

% ============================================================
% Cell access on row-list: O(R + C)
% ============================================================

% get_cell_rl(++RowList,++R,++C,--Val)  1-based
get_cell_rl(RowList, R, C, Val) :-
    row_nth1(R, RowList, Row),
    col_nth1(C, Row, Val).

row_nth1(1, [R|_], R) :- !.
row_nth1(N, [_|T], R) :- N > 1, N1 is N-1, row_nth1(N1, T, R).

col_nth1(1, [V|_], V) :- !.
col_nth1(N, [_|T], V) :- N > 1, N1 is N-1, col_nth1(N1, T, V).

% set_cell_rl(++RowList,++R,++C,++Val,--NewRowList)
set_cell_rl(RowList, R, C, Val, NRL) :-
    set_row(RowList, R, C, Val, NRL).

set_row([Row|Rs], 1, C, Val, [NRow|Rs]) :- !,
    set_col(Row, C, Val, NRow).
set_row([Row|Rs], R, C, Val, [Row|NRs]) :-
    R > 1, R1 is R-1, set_row(Rs, R1, C, Val, NRs).

set_col([_|T], 1, Val, [Val|T]) :- !.
set_col([H|T], C, Val, [H|NT]) :- C > 1, C1 is C-1, set_col(T, C1, Val, NT).

% ============================================================
% Gravity
% ============================================================

opponent(x, o).
opponent(o, x).

% find_bottom_rl(++RowList,++Rows,++Col,--Row)
find_bottom_rl(RowList, Rows, Col, Row) :-
    find_bot(RowList, Rows, Col, Rows, Row).

find_bot(RowList, _, Col, CurR, Row) :-
    CurR >= 1,
    get_cell_rl(RowList, CurR, Col, Val),
    ( Val = e ->
        Row = CurR
    ;
        CurR1 is CurR - 1,
        find_bot(RowList, _, Col, CurR1, Row)
    ).

% drop_piece_rl(++RL,++Rows,++Col,++Player,--NewRL,--Row)
drop_piece_rl(RL, Rows, Col, Player, NewRL, Row) :-
    find_bottom_rl(RL, Rows, Col, Row),
    set_cell_rl(RL, Row, Col, Player, NewRL).

% col_available_rl(++RL,++Col)  - top row (row 1) must be e
col_available_rl(RL, Col) :-
    get_cell_rl(RL, 1, Col, e).

% available_cols_rl(++RL,++Cols,--SortedCols)
available_cols_rl(RL, Cols, Sorted) :-
    Mid is (Cols + 1) // 2,
    [TopRow|_] = RL,
    collect_top(TopRow, 1, Avail),
    sort_center(Avail, Mid, Sorted).

collect_top([], _, []).
collect_top([V|T], C, Result) :-
    C1 is C + 1,
    ( V = e ->
        Result = [C|Rest], collect_top(T, C1, Rest)
    ;
        collect_top(T, C1, Result)
    ).

sort_center([], _, []).
sort_center([H|T], Mid, Sorted) :-
    sort_center(T, Mid, ST),
    DH is abs(H - Mid),
    insert_by_dist(H, DH, ST, Mid, Sorted).

insert_by_dist(C, _, [], _, [C]).
insert_by_dist(C, DC, [H|T], Mid, [C,H|T]) :-
    DH is abs(H - Mid), DC =< DH, !.
insert_by_dist(C, DC, [H|T], Mid, [H|Rest]) :-
    insert_by_dist(C, DC, T, Mid, Rest).

% ============================================================
% Win detection
% ============================================================

% winner_rl(++RL,++Rows,++Cols,++K,++R,++C,++Player)
winner_rl(RL, Rows, Cols, K, R, C, Player) :-
    get_cell_rl(RL, R, C, Player),
    Player \= e, Player \= b,
    ( check_dir_rl(RL, Rows, Cols, K, R, C, Player, 0, 1)
    ; check_dir_rl(RL, Rows, Cols, K, R, C, Player, 1, 0)
    ; check_dir_rl(RL, Rows, Cols, K, R, C, Player, 1, 1)
    ; check_dir_rl(RL, Rows, Cols, K, R, C, Player, 1, -1)
    ).

check_dir_rl(RL, Rows, Cols, K, R, C, Player, DR, DC) :-
    count_dir_rl(RL, Rows, Cols, R, C, Player, DR, DC, 0, N1),
    NDR is -DR, NDC is -DC,
    count_dir_rl(RL, Rows, Cols, R, C, Player, NDR, NDC, 0, N2),
    Total is N1 + N2 - 1,
    Total >= K.

count_dir_rl(RL, Rows, Cols, R, C, Player, DR, DC, Acc, Count) :-
    R >= 1, R =< Rows, C >= 1, C =< Cols,
    get_cell_rl(RL, R, C, Player), !,
    Acc1 is Acc + 1,
    NR is R+DR, NC is C+DC,
    count_dir_rl(RL, Rows, Cols, NR, NC, Player, DR, DC, Acc1, Count).
count_dir_rl(_, _, _, _, _, _, _, _, Acc, Acc).

% ============================================================
% Heuristic evaluation - single pass over top row (fast)
% ============================================================

% eval_board_rl(++RL,++Cols,++Player,--Score)
eval_board_rl(RL, Cols, Player, Score) :-
    Mid is (Cols + 1) // 2,
    opponent(Player, Opp),
    score_rows(RL, Player, Opp, Mid, Cols, 0, Score).

score_rows([], _, _, _, _, Acc, Acc).
score_rows([Row|Rs], Player, Opp, Mid, Cols, Acc, Score) :-
    score_row(Row, Player, Opp, Mid, 1, Acc, Acc2),
    score_rows(Rs, Player, Opp, Mid, Cols, Acc2, Score).

score_row([], _, _, _, _, Acc, Acc).
score_row([V|T], Player, Opp, Mid, C, Acc, Score) :-
    ( V = Player ->
        W is 3 - min(2, abs(C - Mid)), NAcc is Acc + W
    ; V = Opp ->
        W is 3 - min(2, abs(C - Mid)), NAcc is Acc - W
    ;
        NAcc = Acc
    ),
    C1 is C + 1,
    score_row(T, Player, Opp, Mid, C1, NAcc, Score).

% ============================================================
% MinMax + Alpha-Beta
% ============================================================

% minimax(++RL,++Rows,++Cols,++K,++LR,++LC,++LP,++D,++A,++B,++Player,++IsMax,--Score)
minimax(RL, Rows, Cols, K, LR, LC, LP, _, _, _, Player, _, 100000) :-
    LP \= Player, winner_rl(RL, Rows, Cols, K, LR, LC, LP), !.
minimax(RL, Rows, Cols, K, LR, LC, LP, 0, _, _, Player, _, Score) :-
    !,
    ( LP = Player, winner_rl(RL, Rows, Cols, K, LR, LC, LP) ->
        Score = 100000
    ;
        eval_board_rl(RL, Cols, Player, Score)
    ).
minimax(RL, Rows, Cols, K, _, _, _, Depth, Alpha, Beta, Player, IsMax, Score) :-
    available_cols_rl(RL, Cols, Cols_),
    ( Cols_ = [] ->
        eval_board_rl(RL, Cols, Player, Score)
    ;
        opponent(Player, NextP),
        D1 is Depth - 1,
        ( IsMax = true ->
            expand_max(RL, Rows, Cols, K, D1, Alpha, Beta, Player, NextP, Cols_, Alpha, Score)
        ;
            expand_min(RL, Rows, Cols, K, D1, Alpha, Beta, Player, NextP, Cols_, Beta, Score)
        )
    ).

expand_max(_, _, _, _, _, _, _, _, _, [], CurA, CurA).
expand_max(RL, Rows, Cols, K, D, Alpha, Beta, Player, NextP, [C|Rest], CurA, Best) :-
    drop_piece_rl(RL, Rows, C, Player, NRL, Row),
    minimax(NRL, Rows, Cols, K, Row, C, Player, D, CurA, Beta, NextP, false, CS),
    NewA is max(CurA, CS),
    ( NewA >= Beta -> Best = NewA
    ; expand_max(RL, Rows, Cols, K, D, Alpha, Beta, Player, NextP, Rest, NewA, Best)
    ).

expand_min(_, _, _, _, _, _, _, _, _, [], CurB, CurB).
expand_min(RL, Rows, Cols, K, D, Alpha, Beta, Player, NextP, [C|Rest], CurB, Best) :-
    drop_piece_rl(RL, Rows, C, Player, NRL, Row),
    minimax(NRL, Rows, Cols, K, Row, C, Player, D, Alpha, CurB, NextP, true, CS),
    NewB is min(CurB, CS),
    ( NewB =< Alpha -> Best = NewB
    ; expand_min(RL, Rows, Cols, K, D, Alpha, Beta, Player, NextP, Rest, NewB, Best)
    ).

% best_move(++RL,++Rows,++Cols,++K,++Player,++Depth,--BestCol)
best_move(RL, Rows, Cols, K, Player, Depth, BestCol) :-
    available_cols_rl(RL, Cols, [First|RestCols]),
    opponent(Player, NextP),
    D1 is Depth - 1,
    drop_piece_rl(RL, Rows, First, Player, NRL0, Row0),
    minimax(NRL0, Rows, Cols, K, Row0, First, Player, D1, -10000000, 10000000, NextP, false, S0),
    best_move_ab(RL, Rows, Cols, K, D1, Player, NextP, RestCols, S0, First, BestCol).

best_move_ab(_, _, _, _, _, _, _, [], _, BC, BC).
best_move_ab(RL, Rows, Cols, K, D, Player, NextP, [C|Rest], CurBS, CurBC, FC) :-
    drop_piece_rl(RL, Rows, C, Player, NRL, Row),
    minimax(NRL, Rows, Cols, K, Row, C, Player, D, CurBS, 10000000, NextP, false, CS),
    ( CS > CurBS ->
        best_move_ab(RL, Rows, Cols, K, D, Player, NextP, Rest, CS, C, FC)
    ;
        best_move_ab(RL, Rows, Cols, K, D, Player, NextP, Rest, CurBS, CurBC, FC)
    ).

% ============================================================
% JS interface - stateless (board passed as atom each call)
% ============================================================

% js_ai(++BoardAtom,++Player,++Depth,++Rows,++Cols,++K,--ColOut,--RowOut,--NewBoardAtom,--Result)
js_ai(BoardAtom, Player, Depth, Rows, Cols, K, ColOut, RowOut, NewBoardAtom, Result) :-
    atom_to_board(BoardAtom, Cols, RL),
    ( available_cols_rl(RL, Cols, []) ->
        ColOut = -1, RowOut = -1, NewBoardAtom = BoardAtom, Result = draw
    ;
        best_move(RL, Rows, Cols, K, Player, Depth, Col),
        drop_piece_rl(RL, Rows, Col, Player, NewRL, Row),
        board_to_atom(NewRL, NewBoardAtom),
        ColOut = Col, RowOut = Row,
        ( winner_rl(NewRL, Rows, Cols, K, Row, Col, Player) ->
            Result = win
        ; available_cols_rl(NewRL, Cols, []) ->
            Result = draw
        ;
            Result = ok
        )
    ).

% js_human(++BoardAtom,++Col,++Player,++Rows,++Cols,++K,--RowOut,--NewBoardAtom,--Result)
js_human(BoardAtom, Col, Player, Rows, Cols, K, RowOut, NewBoardAtom, Result) :-
    atom_to_board(BoardAtom, Cols, RL),
    ( col_available_rl(RL, Col) ->
        drop_piece_rl(RL, Rows, Col, Player, NewRL, Row),
        board_to_atom(NewRL, NewBoardAtom),
        RowOut = Row,
        ( winner_rl(NewRL, Rows, Cols, K, Row, Col, Player) ->
            Result = win
        ; available_cols_rl(NewRL, Cols, []) ->
            Result = draw
        ;
            Result = ok
        )
    ;
        RowOut = -1, NewBoardAtom = BoardAtom, Result = full
    ).

% js_new_board(++Rows,++Cols,++Forb,--BoardAtom)
% Forb: Prolog list of (R,C) pairs, e.g. [(3,4),(3,5)]
js_new_board(Rows, Cols, Forb, BoardAtom) :-
    make_rowlist(1, Rows, Cols, Forb, RL),
    board_to_atom(RL, BoardAtom).

make_rowlist(R, Rows, _, _, []) :- R > Rows, !.
make_rowlist(R, Rows, Cols, Forb, [Row|Rest]) :-
    R =< Rows,
    make_row(1, Cols, R, Forb, Row),
    R1 is R + 1,
    make_rowlist(R1, Rows, Cols, Forb, Rest).

make_row(C, Cols, _, _, []) :- C > Cols, !.
make_row(C, Cols, R, Forb, [Cell|Rest]) :-
    C =< Cols,
    ( list_member((R,C), Forb) -> Cell = b ; Cell = e ),
    C1 is C + 1,
    make_row(C1, Cols, R, Forb, Rest).

list_member(X, [X|_]) :- !.
list_member(X, [_|T]) :- list_member(X, T).
