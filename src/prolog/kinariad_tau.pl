% kinariad_tau.pl - K in a row (gravity)
% Tau-Prolog 0.3.4 compatible - pure ASCII, no term_to_atom
% Author: Avdieienko Dmytro Maksymovych

:- dynamic current_board/1.

% Board: board(Rows, Cols, K, Forbidden, Cells)
% Cells - flat list Rows*Cols, values: e/x/o/b

% new_game(++Rows,++Cols,++K,++Forb,--Board)
new_game(Rows, Cols, K, Forb, board(Rows,Cols,K,Forb,Cells)) :-
    Size is Rows * Cols,
    length(Cells, Size),
    init_cells(Cells, Rows, Cols, Forb, 1, 1).

init_cells([], _, _, _, _, _).
init_cells([Cell|Rest], Rows, Cols, Forb, CR, CC) :-
    ( member((CR,CC), Forb) -> Cell = b ; Cell = e ),
    CC1 is CC + 1,
    ( CC1 > Cols -> NCC = 1, NCR is CR + 1 ; NCC = CC1, NCR = CR ),
    init_cells(Rest, Rows, Cols, Forb, NCR, NCC).

% cell_idx(++R,++C,++Cols,--Idx) or cell_idx(--R,--C,++Cols,++Idx)
cell_idx(R, C, Cols, Idx) :-
    ( var(Idx) ->
        Idx is (R-1)*Cols + C
    ;
        R is (Idx-1)//Cols + 1,
        C is (Idx-1) mod Cols + 1
    ).

% get_cell(++Board,++R,++C,--Val)
get_cell(board(_,Cols,_,_,Cells), R, C, Val) :-
    cell_idx(R, C, Cols, Idx),
    nth1(Idx, Cells, Val).

% set_cell(++Board,++R,++C,++Val,--NewBoard)
set_cell(board(Rows,Cols,K,Forb,Cells), R, C, Val,
         board(Rows,Cols,K,Forb,NC)) :-
    cell_idx(R, C, Cols, Idx),
    replace_nth1(Idx, Cells, Val, NC).

replace_nth1(1, [_|T], Val, [Val|T]).
replace_nth1(I, [H|T], Val, [H|NT]) :-
    I > 1, I1 is I-1,
    replace_nth1(I1, T, Val, NT).

% drop_piece(++Board,++Col,++Player,--NewBoard,--Row)
drop_piece(Board, Col, Player, NewBoard, Row) :-
    Board = board(Rows,_,_,_,_),
    find_bottom(Board, Col, Rows, Row),
    set_cell(Board, Row, Col, Player, NewBoard).

% find_bottom(++Board,++Col,++CurR,--Row)
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
col_available(Board, Col) :-
    Board = board(_,Cols,_,_,_),
    Col >= 1, Col =< Cols,
    get_cell(Board, 1, Col, e).

% available_cols(++Board,--Cols)
available_cols(Board, Cols) :-
    Board = board(_,MaxC,_,_,_),
    findall(C, (between(1,MaxC,C), col_available(Board,C)), Cols).

% opponent(++P,--Opp)
opponent(x, o).
opponent(o, x).

% winner(++Board,++R,++C,--Player)
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
check_dir(Board, R, C, Player, K, DR, DC) :-
    count_dir(Board, R, C, Player, DR, DC, 0, N1),
    NDR is -DR, NDC is -DC,
    count_dir(Board, R, C, Player, NDR, NDC, 0, N2),
    Total is N1 + N2 - 1,
    Total >= K.

% count_dir(++Board,++R,++C,++Player,++DR,++DC,++Acc,--Count)
count_dir(Board, R, C, Player, DR, DC, Acc, Count) :-
    Board = board(MaxR,MaxC,_,_,_),
    R >= 1, R =< MaxR, C >= 1, C =< MaxC,
    get_cell(Board, R, C, Player), !,
    Acc1 is Acc + 1,
    NR is R+DR, NC is C+DC,
    count_dir(Board, NR, NC, Player, DR, DC, Acc1, Count).
count_dir(_, _, _, _, _, _, Acc, Acc).

% win_exists(++Board,++Player)
win_exists(Board, Player) :-
    Board = board(Rows,Cols,_,_,_),
    between(1,Rows,R), between(1,Cols,C),
    winner(Board, R, C, Player), !.

% eval_board(++Board,++Player,--Score)
eval_board(Board, Player, Score) :-
    opponent(Player, Opp),
    count_threats(Board, Player, MyPts),
    count_threats(Board, Opp, OppPts),
    Score is MyPts - OppPts.

% count_threats(++Board,++Player,--Pts)
count_threats(Board, Player, Pts) :-
    Board = board(Rows,Cols,K,_,_),
    findall(P, window_pts(Board,Player,K,Rows,Cols,P), List),
    sum_list_kinariad(List, Pts).

sum_list_kinariad([], 0).
sum_list_kinariad([H|T], S) :- sum_list_kinariad(T, S1), S is S1 + H.

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
window_score(Board, Player, K, R, C, DR, DC, Pts) :-
    win_count(Board, Player, K, R, C, DR, DC, 0, 0, Mine, OppN),
    ( OppN > 0 -> Pts = 0 ; score_for(Mine, Pts) ).

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

score_for(1, 1).
score_for(2, 10).
score_for(3, 100).
score_for(4, 1000).
score_for(N, 100000) :- N >= 5.

% minimax(++Board,++Depth,++Alpha,++Beta,++Player,++IsMax,--Score)
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

% best_move(++Board,++Player,++Depth,--Col)
best_move(Board, Player, Depth, BestCol) :-
    available_cols(Board, Cols),
    Cols \= [],
    opponent(Player, NextP),
    D1 is Depth - 1,
    findall(S-C,
        ( member(C, Cols),
          drop_piece(Board, C, Player, NB, _),
          minimax(NB, D1, -10000000, 10000000, NextP, false, S)
        ),
        Scored),
    max_pair(Scored, _-BestCol).

max_pair([H], H).
max_pair([S-C|Rest], Best) :-
    max_pair(Rest, BS-BC),
    ( S >= BS -> Best = S-C ; Best = BS-BC ).

% ============================================================
% STATE via assert - board stored in Prolog DB between calls
% ============================================================

% js_init(++Rows,++Cols,++K) - create new game, store in DB
% Forbidden cells are added via js_add_forbidden before js_init
:- dynamic forbidden_list/1.
forbidden_list([]).

% js_set_forbidden(++ForbList) - set forbidden cells as Prolog list term
js_set_forbidden(Forb) :-
    retractall(forbidden_list(_)),
    assertz(forbidden_list(Forb)).

% js_init(++Rows,++Cols,++K) - init game using stored forbidden
js_init(Rows, Cols, K) :-
    forbidden_list(Forb),
    new_game(Rows, Cols, K, Forb, Board),
    retractall(current_board(_)),
    assertz(current_board(Board)).

% js_human(++Col,++Player,--RowOut,--Result)
% Result: ok/full/win/draw
% RowOut: row where piece landed (-1 if full)
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
% Returns board dimensions and flat cell list for rendering
js_get_board(Rows, Cols, K, Cells) :-
    current_board(board(Rows,Cols,K,_,Cells)).
