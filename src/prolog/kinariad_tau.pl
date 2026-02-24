% kinariad_tau.pl - K in a row (gravity), Tau-Prolog 0.3.4
% Author: Avdieienko Dmytro Maksymovych
%
% STATELESS design: board passed as an atom (flat string of Rows*Cols chars).
% Cell chars: e=empty, x=player X, o=player O, b=blocked.
% All board state is kept in JS; Prolog only computes AI moves.
%
% JS encodes board as atom like 'eeeeeee...', 1-based index = (R-1)*Cols+C.
% Prolog converts atom <-> char list for internal use.

:- dynamic(current_board/1).
current_board(none).

% ============================================================
% atom <-> char list conversion (Tau-Prolog has atom_chars/2)
% ============================================================

% board_to_list(++BoardAtom,--CharList)
board_to_list(Atom, List) :- atom_chars(Atom, List).

% list_to_board(++CharList,--BoardAtom)
list_to_board(List, Atom) :- atom_chars(Atom, List).

% ============================================================
% Cell access on char list (flat, 1-based index)
% ============================================================

% cell_idx(++R,++C,++Cols,--Idx) - 1-based flat index
cell_idx(R, C, Cols, Idx) :- Idx is (R-1)*Cols + C.

% list_nth1(++N,++List,--Elem)
list_nth1(1, [H|_], H) :- !.
list_nth1(N, [_|T], H) :- N > 1, N1 is N-1, list_nth1(N1, T, H).

% list_set_nth1(++N,++List,++Val,--NewList)
list_set_nth1(1, [_|T], V, [V|T]) :- !.
list_set_nth1(N, [H|T], V, [H|NT]) :-
    N > 1, N1 is N-1, list_set_nth1(N1, T, V, NT).

% get_cell(++Cells,++R,++C,++Cols,--Val)
get_cell(Cells, R, C, Cols, Val) :-
    cell_idx(R, C, Cols, Idx),
    list_nth1(Idx, Cells, Val).

% set_cell(++Cells,++R,++C,++Cols,++Val,--NewCells)
set_cell(Cells, R, C, Cols, Val, NC) :-
    cell_idx(R, C, Cols, Idx),
    list_set_nth1(Idx, Cells, Val, NC).

% ============================================================
% Board struct: b(Rows,Cols,K,Cells)  where Cells = char list
% ============================================================

% ============================================================
% Gravity
% ============================================================

% find_bottom(++Cells,++Rows,++Cols,++Col,--Row)
find_bottom(Cells, Rows, Cols, Col, Row) :-
    find_bottom_(Cells, Rows, Cols, Col, Rows, Row).

find_bottom_(Cells, _Rows, Cols, Col, CurR, Row) :-
    CurR >= 1,
    get_cell(Cells, CurR, Col, Cols, Val),
    ( Val = e ->
        Row = CurR
    ;
        CurR1 is CurR - 1,
        find_bottom_(Cells, _Rows, Cols, Col, CurR1, Row)
    ).

% drop_piece(++Cells,++Rows,++Cols,++Col,++Player,--NewCells,--Row)
drop_piece(Cells, Rows, Cols, Col, Player, NewCells, Row) :-
    find_bottom(Cells, Rows, Cols, Col, Row),
    set_cell(Cells, Row, Col, Cols, Player, NewCells).

% col_available(++Cells,++Cols,++Col)
col_available(Cells, Cols, Col) :-
    Col >= 1, Col =< Cols,
    get_cell(Cells, 1, Col, Cols, e).

% available_cols(++Cells,++Rows,++Cols,--AvailSorted)
available_cols(Cells, _Rows, Cols, Sorted) :-
    Mid is (Cols + 1) // 2,
    collect_avail(Cells, Cols, 1, Cols, Avail),
    sort_center(Avail, Mid, Sorted).

collect_avail(_, _, C, MaxC, []) :- C > MaxC, !.
collect_avail(Cells, Cols, C, MaxC, Result) :-
    C1 is C + 1,
    ( get_cell(Cells, 1, C, Cols, e) ->
        Result = [C|Rest],
        collect_avail(Cells, Cols, C1, MaxC, Rest)
    ;
        collect_avail(Cells, Cols, C1, MaxC, Result)
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

opponent(x, o).
opponent(o, x).

% winner(++Cells,++Rows,++Cols,++K,++R,++C,++Player)
winner(Cells, Rows, Cols, K, R, C, Player) :-
    get_cell(Cells, R, C, Cols, Player),
    Player \= e, Player \= b,
    ( check_dir(Cells, Rows, Cols, K, R, C, Player, 0, 1)
    ; check_dir(Cells, Rows, Cols, K, R, C, Player, 1, 0)
    ; check_dir(Cells, Rows, Cols, K, R, C, Player, 1, 1)
    ; check_dir(Cells, Rows, Cols, K, R, C, Player, 1, -1)
    ).

check_dir(Cells, Rows, Cols, K, R, C, Player, DR, DC) :-
    count_dir(Cells, Rows, Cols, R, C, Player, DR, DC, 0, N1),
    NDR is -DR, NDC is -DC,
    count_dir(Cells, Rows, Cols, R, C, Player, NDR, NDC, 0, N2),
    Total is N1 + N2 - 1,
    Total >= K.

count_dir(Cells, Rows, Cols, R, C, Player, DR, DC, Acc, Count) :-
    R >= 1, R =< Rows, C >= 1, C =< Cols,
    get_cell(Cells, R, C, Cols, Player), !,
    Acc1 is Acc + 1,
    NR is R+DR, NC is C+DC,
    count_dir(Cells, Rows, Cols, NR, NC, Player, DR, DC, Acc1, Count).
count_dir(_, _, _, _, _, _, _, _, Acc, Acc).

% ============================================================
% Heuristic evaluation
% ============================================================

% eval_board(++Cells,++Rows,++Cols,++Player,--Score)
eval_board(Cells, _Rows, Cols, Player, Score) :-
    Mid is (Cols + 1) // 2,
    opponent(Player, Opp),
    score_cells(Cells, Player, Opp, Mid, Cols, 1, 1, 0, Score).

score_cells([], _, _, _, _, _, _, Acc, Acc).
score_cells([V|T], Player, Opp, Mid, Cols, R, C, Acc, Score) :-
    ( V = Player ->
        W is 3 - min(2, abs(C - Mid)), NAcc is Acc + W
    ; V = Opp ->
        W is 3 - min(2, abs(C - Mid)), NAcc is Acc - W
    ;
        NAcc = Acc
    ),
    C1 is C + 1,
    ( C1 > Cols -> NC = 1, NR is R + 1 ; NC = C1, NR = R ),
    score_cells(T, Player, Opp, Mid, Cols, NR, NC, NAcc, Score).

% ============================================================
% MinMax + Alpha-Beta (stateless, board = char list)
% ============================================================

% minimax(++Cells,++Rows,++Cols,++K,++LR,++LC,++LP,++D,++A,++B,++Player,++IsMax,--Score)
minimax(Cells, Rows, Cols, K, LR, LC, LP, _, _, _, Player, _, 100000) :-
    LP \= Player, winner(Cells, Rows, Cols, K, LR, LC, LP), !.
minimax(Cells, Rows, Cols, K, LR, LC, LP, 0, _, _, Player, _, Score) :-
    !,
    ( LP = Player, winner(Cells, Rows, Cols, K, LR, LC, LP) ->
        Score = 100000
    ;
        eval_board(Cells, Rows, Cols, Player, Score)
    ).
minimax(Cells, Rows, Cols, K, _, _, _, Depth, Alpha, Beta, Player, IsMax, Score) :-
    available_cols(Cells, Rows, Cols, Cols_),
    ( Cols_ = [] ->
        eval_board(Cells, Rows, Cols, Player, Score)
    ;
        opponent(Player, NextP),
        D1 is Depth - 1,
        ( IsMax = true ->
            expand_max(Cells, Rows, Cols, K, D1, Alpha, Beta, Player, NextP, Cols_, Alpha, Score)
        ;
            expand_min(Cells, Rows, Cols, K, D1, Alpha, Beta, Player, NextP, Cols_, Beta, Score)
        )
    ).

expand_max(_, _, _, _, _, _, _, _, _, [], CurA, CurA).
expand_max(Cells, Rows, Cols, K, D, Alpha, Beta, Player, NextP, [C|Rest], CurA, Best) :-
    drop_piece(Cells, Rows, Cols, C, Player, NC, Row),
    minimax(NC, Rows, Cols, K, Row, C, Player, D, CurA, Beta, NextP, false, CS),
    NewA is max(CurA, CS),
    ( NewA >= Beta -> Best = NewA
    ; expand_max(Cells, Rows, Cols, K, D, Alpha, Beta, Player, NextP, Rest, NewA, Best)
    ).

expand_min(_, _, _, _, _, _, _, _, _, [], CurB, CurB).
expand_min(Cells, Rows, Cols, K, D, Alpha, Beta, Player, NextP, [C|Rest], CurB, Best) :-
    drop_piece(Cells, Rows, Cols, C, Player, NC, Row),
    minimax(NC, Rows, Cols, K, Row, C, Player, D, Alpha, CurB, NextP, true, CS),
    NewB is min(CurB, CS),
    ( NewB =< Alpha -> Best = NewB
    ; expand_min(Cells, Rows, Cols, K, D, Alpha, Beta, Player, NextP, Rest, NewB, Best)
    ).

% best_move(++Cells,++Rows,++Cols,++K,++Player,++Depth,--BestCol)
best_move(Cells, Rows, Cols, K, Player, Depth, BestCol) :-
    available_cols(Cells, Rows, Cols, [First|RestCols]),
    opponent(Player, NextP),
    D1 is Depth - 1,
    drop_piece(Cells, Rows, Cols, First, Player, NC0, Row0),
    minimax(NC0, Rows, Cols, K, Row0, First, Player, D1, -10000000, 10000000, NextP, false, S0),
    best_move_ab(Cells, Rows, Cols, K, D1, Player, NextP, RestCols, S0, First, BestCol).

best_move_ab(_, _, _, _, _, _, _, [], _, BC, BC).
best_move_ab(Cells, Rows, Cols, K, D, Player, NextP, [C|Rest], CurBS, CurBC, FC) :-
    drop_piece(Cells, Rows, Cols, C, Player, NC, Row),
    minimax(NC, Rows, Cols, K, Row, C, Player, D, CurBS, 10000000, NextP, false, CS),
    ( CS > CurBS ->
        best_move_ab(Cells, Rows, Cols, K, D, Player, NextP, Rest, CS, C, FC)
    ;
        best_move_ab(Cells, Rows, Cols, K, D, Player, NextP, Rest, CurBS, CurBC, FC)
    ).

% ============================================================
% JS interface - stateless (board passed as atom each call)
% ============================================================

% js_ai(++BoardAtom,++Player,++Depth,++Rows,++Cols,++K,--ColOut,--RowOut,--NewBoardAtom,--Result)
% Multimodality:
%   js_ai(++,++,++,++,++,++,--,--,--,--) - compute AI move, return new board atom
js_ai(BoardAtom, Player, Depth, Rows, Cols, K, ColOut, RowOut, NewBoardAtom, Result) :-
    atom_chars(BoardAtom, Cells),
    ( available_cols(Cells, Rows, Cols, []) ->
        ColOut = -1, RowOut = -1, NewBoardAtom = BoardAtom, Result = draw
    ;
        best_move(Cells, Rows, Cols, K, Player, Depth, Col),
        drop_piece(Cells, Rows, Cols, Col, Player, NewCells, Row),
        atom_chars(NewBoardAtom, NewCells),
        ColOut = Col, RowOut = Row,
        ( winner(NewCells, Rows, Cols, K, Row, Col, Player) ->
            Result = win
        ; available_cols(NewCells, Rows, Cols, []) ->
            Result = draw
        ;
            Result = ok
        )
    ).

% js_human(++BoardAtom,++Col,++Player,++Rows,++Cols,++K,--RowOut,--NewBoardAtom,--Result)
% Multimodality:
%   js_human(++,++,++,++,++,++,--,--,--) - execute human move, return new board atom
js_human(BoardAtom, Col, Player, Rows, Cols, K, RowOut, NewBoardAtom, Result) :-
    atom_chars(BoardAtom, Cells),
    ( col_available(Cells, Cols, Col) ->
        drop_piece(Cells, Rows, Cols, Col, Player, NewCells, Row),
        atom_chars(NewBoardAtom, NewCells),
        RowOut = Row,
        ( winner(NewCells, Rows, Cols, K, Row, Col, Player) ->
            Result = win
        ; available_cols(NewCells, Rows, Cols, []) ->
            Result = draw
        ;
            Result = ok
        )
    ;
        RowOut = -1, NewBoardAtom = BoardAtom, Result = full
    ).

% js_new_board(++Rows,++Cols,++ForbAtom,--BoardAtom)
% ForbAtom: Prolog list of (R,C) pairs as atom string, e.g. '[(3,4),(3,5)]'
% Multimodality:
%   js_new_board(++,++,++,--) - create initial board atom
js_new_board(Rows, Cols, Forb, BoardAtom) :-
    Size is Rows * Cols,
    make_cells(1, 1, Rows, Cols, Size, Forb, Cells),
    atom_chars(BoardAtom, Cells).

% make_cells(++R,++C,++Rows,++Cols,++Rem,++Forb,--Cells)
make_cells(_, _, _, _, 0, _, []) :- !.
make_cells(R, C, Rows, Cols, Rem, Forb, [Cell|Rest]) :-
    Rem > 0,
    ( list_member((R,C), Forb) -> Cell = b ; Cell = e ),
    Rem1 is Rem - 1,
    C1 is C + 1,
    ( C1 > Cols -> NC = 1, NR is R + 1 ; NC = C1, NR = R ),
    make_cells(NR, NC, Rows, Cols, Rem1, Forb, Rest).

% list_member(++X,++List)
list_member(X, [X|_]) :- !.
list_member(X, [_|T]) :- list_member(X, T).
