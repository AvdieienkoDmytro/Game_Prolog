% kinariad_tau.pl - K in a row (gravity)
% Tau-Prolog 0.3.4 compatible - pure ASCII comments
% Author: Avdieienko Dmytro Maksymovych
%
% NOTE: dynamic must be written as :- dynamic(Name/Arity). in Tau-Prolog consult.
% Board is stored as list of rows (list of lists) for O(Cols) cell access.

:- dynamic(current_board/1).

% Sentinel
current_board(none).

% Board: board(Rows, Cols, K, Forbidden, RowList)
% RowList = [[c11,c12,...,c1m], [c21,...], ..., [cn1,...,cnm]]
% Cell values: e (empty), x, o, b (blocked)

% ============================================================
% Utilities
% ============================================================

% list_nth1(++N,++List,--Elem)
list_nth1(1, [H|_], H) :- !.
list_nth1(N, [_|T], H) :- N > 1, N1 is N-1, list_nth1(N1, T, H).

% list_set_nth1(++N,++List,++Val,--NewList)
list_set_nth1(1, [_|T], Val, [Val|T]) :- !.
list_set_nth1(N, [H|T], Val, [H|NT]) :-
    N > 1, N1 is N-1, list_set_nth1(N1, T, Val, NT).

% list_member(++X,++List)
list_member(X, [X|_]) :- !.
list_member(X, [_|T]) :- list_member(X, T).

% make_row_lr(++C,++Cols,++R,++Forb,--Row) - build row left-to-right
make_row_lr(1, Cols, R, Forb, [Cell]) :-
    !,
    ( list_member((R,Cols), Forb) -> Cell = b ; Cell = e ).
make_row_lr(C, Cols, R, Forb, [Cell|Rest]) :-
    C =< Cols,
    ( list_member((R,C), Forb) -> Cell = b ; Cell = e ),
    C1 is C + 1,
    make_row_lr(C1, Cols, R, Forb, Rest).

% make_rows(++R,++Rows,++Cols,++Forb,--RowList)
make_rows(R, Rows, _, _, []) :- R > Rows, !.
make_rows(R, Rows, Cols, Forb, [Row|Rest]) :-
    R =< Rows,
    make_row_lr(1, Cols, R, Forb, Row),
    R1 is R + 1,
    make_rows(R1, Rows, Cols, Forb, Rest).

% ============================================================
% Board construction
% ============================================================

% new_game(++Rows,++Cols,++K,++Forb,--Board)
% Multimodality:
%   new_game(++,++,++,++,--) - create new board
%   new_game(++,++,++,++,+)  - verify board parameters
new_game(Rows, Cols, K, Forb, board(Rows,Cols,K,Forb,RowList)) :-
    make_rows(1, Rows, Cols, Forb, RowList).

% ============================================================
% Cell access - O(R + C) via row list
% ============================================================

% get_cell(++Board,++R,++C,--Val)
% Multimodality:
%   get_cell(++,++,++,--) - read cell value
%   get_cell(++,++,++,+)  - check cell value
get_cell(board(_,_,_,_,RowList), R, C, Val) :-
    list_nth1(R, RowList, Row),
    list_nth1(C, Row, Val).

% set_cell(++Board,++R,++C,++Val,--NewBoard)
% Multimodality:
%   set_cell(++,++,++,++,--) - return new board with cell updated
set_cell(board(Rows,Cols,K,Forb,RowList), R, C, Val,
         board(Rows,Cols,K,Forb,NewRowList)) :-
    list_nth1(R, RowList, OldRow),
    list_set_nth1(C, OldRow, Val, NewRow),
    list_set_nth1(R, RowList, NewRow, NewRowList).

% ============================================================
% Gravity
% ============================================================

% drop_piece(++Board,++Col,++Player,--NewBoard,--Row)
% Multimodality:
%   drop_piece(++,++,++,--,--) - drop piece, return board and landing row
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
%   col_available(++,++) - check if column has room
col_available(Board, Col) :-
    Board = board(_,Cols,_,_,_),
    Col >= 1, Col =< Cols,
    get_cell(Board, 1, Col, e).

% available_cols(++Board,--Sorted)
% Multimodality:
%   available_cols(++,--) - legal moves sorted center-first
available_cols(Board, Sorted) :-
    Board = board(_,MaxC,_,_,RowList),
    list_nth1(1, RowList, TopRow),
    Mid is (MaxC + 1) // 2,
    collect_avail_row(TopRow, 1, AvailCols),
    sort_center(AvailCols, Mid, Sorted).

% collect_avail_row(++Row,++C,--AvailCols)
% Multimodality:
%   collect_avail_row(++,++,--) - collect available columns from top row
collect_avail_row([], _, []).
collect_avail_row([V|T], C, Result) :-
    C1 is C + 1,
    ( V = e ->
        Result = [C|Rest],
        collect_avail_row(T, C1, Rest)
    ;
        collect_avail_row(T, C1, Result)
    ).

% sort_center(++Cols,++Mid,--Sorted)
% Multimodality:
%   sort_center(++,++,--) - insertion sort by distance to center
sort_center([], _, []).
sort_center([H|T], Mid, Sorted) :-
    sort_center(T, Mid, SortedT),
    DH is abs(H - Mid),
    insert_by_dist(H, DH, SortedT, Mid, Sorted).

% insert_by_dist(++Col,++Dist,++List,++Mid,--Result)
% Multimodality:
%   insert_by_dist(++,++,++,++,--) - insertion step
insert_by_dist(C, _, [], _, [C]).
insert_by_dist(C, DC, [H|T], Mid, [C,H|T]) :-
    DH is abs(H - Mid), DC =< DH, !.
insert_by_dist(C, DC, [H|T], Mid, [H|Rest]) :-
    insert_by_dist(C, DC, T, Mid, Rest).

% ============================================================
% Win detection
% ============================================================

% opponent(++P,--Opp)
% Multimodality:
%   opponent(++,--) - get opponent
%   opponent(--,++) - reverse lookup
%   opponent(++,+)  - verify
opponent(x, o).
opponent(o, x).

% winner(++Board,++R,++C,++Player)
% Multimodality:
%   winner(++,++,++,--) - detect winner after move at (R,C)
%   winner(++,++,++,+)  - check if player won at (R,C)
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
%   check_dir(++,++,++,++,++,++,++) - check K-in-a-row in direction
check_dir(Board, R, C, Player, K, DR, DC) :-
    count_dir(Board, R, C, Player, DR, DC, 0, N1),
    NDR is -DR, NDC is -DC,
    count_dir(Board, R, C, Player, NDR, NDC, 0, N2),
    Total is N1 + N2 - 1,
    Total >= K.

% count_dir(++Board,++R,++C,++Player,++DR,++DC,++Acc,--Count)
% Multimodality:
%   count_dir(++,++,++,++,++,++,++,--) - count consecutive pieces
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
% Fast heuristic: center-weighted piece count.
% Multimodality:
%   eval_board(++,++,--) - compute heuristic score for Player
eval_board(Board, Player, Score) :-
    Board = board(_,Cols,_,_,RowList),
    Mid is (Cols + 1) // 2,
    opponent(Player, Opp),
    score_rows(RowList, Player, Opp, Mid, 0, Score).

% score_rows(++RowList,++P,++Opp,++Mid,++Acc,--Score)
% Multimodality:
%   score_rows(++,++,++,++,++,--) - sum scores across all rows
score_rows([], _, _, _, Acc, Acc).
score_rows([Row|Rest], Player, Opp, Mid, Acc, Score) :-
    score_row(Row, Player, Opp, Mid, 1, Acc, Acc2),
    score_rows(Rest, Player, Opp, Mid, Acc2, Score).

% score_row(++Row,++P,++Opp,++Mid,++C,++Acc,--Score)
% Multimodality:
%   score_row(++,++,++,++,++,++,--) - score one row
score_row([], _, _, _, _, Acc, Acc).
score_row([V|T], Player, Opp, Mid, C, Acc, Score) :-
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
    score_row(T, Player, Opp, Mid, C1, NAcc, Score).

% ============================================================
% MinMax with Alpha-Beta pruning
% Depth in HALF-MOVES. Win detected at landing cell only.
% ============================================================

% minimax(++Board,++LR,++LC,++LP,++Depth,++Alpha,++Beta,++Player,++IsMax,--Score)
% Multimodality:
%   minimax(++,++,++,++,++,++,++,++,++,--) - evaluate game tree node
minimax(Board, LR, LC, LP, _, _, _, Player, _, 100000) :-
    LP \= Player, winner(Board, LR, LC, LP), !.
minimax(Board, LR, LC, LP, 0, _, _, Player, _, Score) :-
    !,
    ( LP = Player, winner(Board, LR, LC, LP) ->
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
%   best_move(++,++,++,--) - AI move selection
best_move(Board, Player, Depth, BestCol) :-
    available_cols(Board, [First|RestCols]),
    opponent(Player, NextP),
    D1 is Depth - 1,
    drop_piece(Board, First, Player, NB0, Row0),
    minimax(NB0, Row0, First, Player, D1, -10000000, 10000000, NextP, false, S0),
    best_move_ab(Board, D1, S0, 10000000, Player, NextP, RestCols,
                 S0, First, _, BestCol).

% best_move_ab(++B,++D,++A,++Bt,++P,++NP,++Cols,++BS,++BC,--FS,--FC)
% Multimodality:
%   best_move_ab(++,++,++,++,++,++,++,++,++,--,--) - root alpha-beta loop
best_move_ab(_, _, _, _, _, _, [], BS, BC, BS, BC).
best_move_ab(Board, D, Alpha, Beta, Player, NextP, [C|Rest],
             CurBS, CurBC, FS, FC) :-
    drop_piece(Board, C, Player, NB, Row),
    minimax(NB, Row, C, Player, D, Alpha, Beta, NextP, false, CS),
    ( CS > CurBS ->
        NewBS = CS, NewBC = C,
        NewAlpha is max(Alpha, CS)
    ;
        NewBS = CurBS, NewBC = CurBC,
        NewAlpha = Alpha
    ),
    best_move_ab(Board, D, NewAlpha, Beta, Player, NextP, Rest,
                 NewBS, NewBC, FS, FC).

% ============================================================
% JS interface
% ============================================================

% js_init(++Rows,++Cols,++K,++Forb)
% Multimodality:
%   js_init(++,++,++,++) - initialize game
js_init(Rows, Cols, K, Forb) :-
    new_game(Rows, Cols, K, Forb, Board),
    retractall(current_board(_)),
    assertz(current_board(Board)).

% js_human(++Col,++Player,--RowOut,--Result)
% Result: ok / full / win / draw
% Multimodality:
%   js_human(++,++,--,--) - execute human move
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
% Multimodality:
%   js_ai(++,++,--,--,--) - execute AI move
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

% js_get_board(--Rows,--Cols,--K,--RowList)
% Returns list of rows; JS renderer flattens it.
% Multimodality:
%   js_get_board(--,--,--,--) - read board for rendering
js_get_board(Rows, Cols, K, RowList) :-
    current_board(board(Rows,Cols,K,_,RowList)).
