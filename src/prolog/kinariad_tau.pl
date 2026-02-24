% kinariad_tau.pl - K in a row (gravity), Tau-Prolog 0.3.4
% Author: Avdieienko Dmytro Maksymovych
%
% Prolog handles ONLY: board init, human move validation, win/draw detection.
% AI (Minimax + Alpha-Beta) is implemented in JavaScript for performance.
%
% Board: flat atom, rows*cols chars. e=empty x=X o=O b=blocked.
% Internally: list of rows (list of char lists) for O(R+C) access.

% ============================================================
% atom <-> row-list
% ============================================================

atom_to_board(Atom, Cols, RL) :-
    atom_chars(Atom, Flat),
    split_rows(Flat, Cols, RL).

board_to_atom(RL, Atom) :-
    flatten_rows(RL, Flat),
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
% Cell access O(R+C)
% ============================================================

get_cell(RL, R, C, Val) :-
    row_nth(R, RL, Row),
    col_nth(C, Row, Val).

row_nth(1, [R|_], R) :- !.
row_nth(N, [_|T], R) :- N > 1, N1 is N-1, row_nth(N1, T, R).

col_nth(1, [V|_], V) :- !.
col_nth(N, [_|T], V) :- N > 1, N1 is N-1, col_nth(N1, T, V).

set_cell(RL, R, C, Val, NRL) :-
    set_row(RL, R, C, Val, NRL).

set_row([Row|Rs], 1, C, Val, [NRow|Rs]) :- !,
    set_col(Row, C, Val, NRow).
set_row([Row|Rs], R, C, Val, [Row|NRs]) :-
    R > 1, R1 is R-1, set_row(Rs, R1, C, Val, NRs).

set_col([_|T], 1, Val, [Val|T]) :- !.
set_col([H|T], C, Val, [H|NT]) :- C > 1, C1 is C-1, set_col(T, C1, Val, NT).

% ============================================================
% Gravity
% ============================================================

find_bottom(RL, Rows, Col, Row) :- find_bot(RL, Rows, Col, Rows, Row).

find_bot(RL, _, Col, CurR, Row) :-
    CurR >= 1,
    get_cell(RL, CurR, Col, Val),
    ( Val = e -> Row = CurR
    ; CurR1 is CurR-1, find_bot(RL, _, Col, CurR1, Row)
    ).

drop_piece(RL, Rows, Col, Player, NewRL, Row) :-
    find_bottom(RL, Rows, Col, Row),
    set_cell(RL, Row, Col, Player, NewRL).

col_available(RL, Col) :- get_cell(RL, 1, Col, e).

available_cols(RL, Cols, Sorted) :-
    Mid is (Cols+1)//2,
    [TopRow|_] = RL,
    collect_top(TopRow, 1, Avail),
    sort_center(Avail, Mid, Sorted).

collect_top([], _, []).
collect_top([V|T], C, Result) :-
    C1 is C+1,
    ( V = e -> Result = [C|Rest], collect_top(T, C1, Rest)
    ; collect_top(T, C1, Result)
    ).

sort_center([], _, []).
sort_center([H|T], Mid, Sorted) :-
    sort_center(T, Mid, ST),
    DH is abs(H-Mid),
    ins_dist(H, DH, ST, Mid, Sorted).

ins_dist(C, _, [], _, [C]).
ins_dist(C, DC, [H|T], Mid, [C,H|T]) :- DH is abs(H-Mid), DC =< DH, !.
ins_dist(C, DC, [H|T], Mid, [H|R]) :- ins_dist(C, DC, T, Mid, R).

% ============================================================
% Win detection
% ============================================================

opponent(x, o).
opponent(o, x).

winner(RL, Rows, Cols, K, R, C, Player) :-
    get_cell(RL, R, C, Player),
    Player \= e, Player \= b,
    ( check_dir(RL, Rows, Cols, K, R, C, Player, 0, 1)
    ; check_dir(RL, Rows, Cols, K, R, C, Player, 1, 0)
    ; check_dir(RL, Rows, Cols, K, R, C, Player, 1, 1)
    ; check_dir(RL, Rows, Cols, K, R, C, Player, 1, -1)
    ).

check_dir(RL, Rows, Cols, K, R, C, Player, DR, DC) :-
    count_dir(RL, Rows, Cols, R, C, Player, DR, DC, 0, N1),
    NDR is -DR, NDC is -DC,
    count_dir(RL, Rows, Cols, R, C, Player, NDR, NDC, 0, N2),
    Total is N1+N2-1, Total >= K.

count_dir(RL, Rows, Cols, R, C, Player, DR, DC, Acc, Count) :-
    R >= 1, R =< Rows, C >= 1, C =< Cols,
    get_cell(RL, R, C, Player), !,
    Acc1 is Acc+1, NR is R+DR, NC is C+DC,
    count_dir(RL, Rows, Cols, NR, NC, Player, DR, DC, Acc1, Count).
count_dir(_, _, _, _, _, _, _, _, Acc, Acc).

% ============================================================
% JS interface
% ============================================================

% js_human(++BoardAtom,++Col,++Player,++Rows,++Cols,++K,--RowOut,--NewBoardAtom,--Result)
js_human(BoardAtom, Col, Player, Rows, Cols, K, RowOut, NewBoardAtom, Result) :-
    atom_to_board(BoardAtom, Cols, RL),
    ( col_available(RL, Col) ->
        drop_piece(RL, Rows, Col, Player, NewRL, Row),
        board_to_atom(NewRL, NewBoardAtom),
        RowOut = Row,
        ( winner(NewRL, Rows, Cols, K, Row, Col, Player) -> Result = win
        ; available_cols(NewRL, Cols, [])               -> Result = draw
        ; Result = ok
        )
    ;
        RowOut = -1, NewBoardAtom = BoardAtom, Result = full
    ).

% js_new_board(++Rows,++Cols,++Forb,--BoardAtom)
js_new_board(Rows, Cols, Forb, BoardAtom) :-
    make_rl(1, Rows, Cols, Forb, RL),
    board_to_atom(RL, BoardAtom).

make_rl(R, Rows, _, _, []) :- R > Rows, !.
make_rl(R, Rows, Cols, Forb, [Row|Rest]) :-
    R =< Rows,
    make_row(1, Cols, R, Forb, Row),
    R1 is R+1, make_rl(R1, Rows, Cols, Forb, Rest).

make_row(C, Cols, _, _, []) :- C > Cols, !.
make_row(C, Cols, R, Forb, [Cell|Rest]) :-
    C =< Cols,
    ( lmember((R,C), Forb) -> Cell = b ; Cell = e ),
    C1 is C+1, make_row(C1, Cols, R, Forb, Rest).

lmember(X, [X|_]) :- !.
lmember(X, [_|T]) :- lmember(X, T).
