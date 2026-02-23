% kinariad_tau.pl - K in a row (gravity), Tau-Prolog compatible
% Author: Avdieienko Dmytro Maksymovych

% Board: board(Rows, Cols, K, Forbidden, Cells)
% Cells - flat list, size Rows*Cols, values: e/x/o/b (b=blocked)

% new_game(++Rows, ++Cols, ++K, ++Forbidden, --Board)
% Multiuse: new_game(++,++,++,++,--) - create board
new_game(Rows, Cols, K, Forb, board(Rows,Cols,K,Forb,Cells)) :-
    Rows >= 5, Cols >= 6, K >= 3, K =< 5,
    Size is Rows * Cols,
    length(Cells, Size),
    init_cells(Cells, Rows, Cols, Forb, 1, 1).

% init_cells(++Cells,++Rows,++Cols,++Forb,++CR,++CC)
% Multiuse: init_cells(++,++,++,++,++,++) - initialize cells
init_cells([], _, _, _, _, _).
init_cells([Cell|Rest], Rows, Cols, Forb, CR, CC) :-
    ( member((CR,CC), Forb) -> Cell = b ; Cell = e ),
    CC1 is CC + 1,
    ( CC1 > Cols -> NCC = 1, NCR is CR + 1 ; NCC = CC1, NCR = CR ),
    init_cells(Rest, Rows, Cols, Forb, NCR, NCC).

% cell_idx(++R,++C,++Cols,--Idx) - compute index
% cell_idx(--R,--C,++Cols,++Idx) - restore coords
% Multiuse: cell_idx - bidirectional index/coord conversion
cell_idx(R, C, Cols, Idx) :-
    ( var(Idx) ->
        Idx is (R-1)*Cols + C
    ;
        R is (Idx-1)//Cols + 1,
        C is (Idx-1) mod Cols + 1
    ).

% get_cell(++Board,++R,++C,--Val) - read cell
% get_cell(++Board,++R,++C,+Val)  - check value
% Multiuse: get_cell - read or verify cell value
get_cell(board(_,Cols,_,_,Cells), R, C, Val) :-
    cell_idx(R, C, Cols, Idx),
    nth1(Idx, Cells, Val).

% set_cell(++Board,++R,++C,++Val,--NewBoard)
% Multiuse: set_cell(++,++,++,++,--) - update board
set_cell(board(Rows,Cols,K,Forb,Cells), R, C, Val,
         board(Rows,Cols,K,Forb,NC)) :-
    cell_idx(R, C, Cols, Idx),
    replace_nth1(Idx, Cells, Val, NC).

% replace_nth1(++Idx,++List,++Val,--New)
% Multiuse: replace_nth1(++,++,++,--) - replace element
replace_nth1(1, [_|T], Val, [Val|T]).
replace_nth1(I, [H|T], Val, [H|NT]) :-
    I > 1, I1 is I-1,
    replace_nth1(I1, T, Val, NT).

% ============================================================
% Gravity
% ============================================================

% drop_piece(++Board,++Col,++Player,--NewBoard,--Row)
% Multiuse:
%   drop_piece(++,++,++,--,--) - drop piece (main use)
%   drop_piece(++,++,++,--,+)  - with row verification
drop_piece(Board, Col, Player, NewBoard, Row) :-
    Board = board(Rows,_,_,_,_),
    find_bottom(Board, Col, Rows, Row),
    set_cell(Board, Row, Col, Player, NewBoard).

% find_bottom(++Board,++Col,++CurR,--Row)
% Multiuse: find_bottom(++,++,++,--) - find lowest free row
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
% Multiuse: col_available(++,++) - check column availability
col_available(Board, Col) :-
    Board = board(_,Cols,_,_,_),
    Col >= 1, Col =< Cols,
    get_cell(Board, 1, Col, e).

% available_cols(++Board,--Cols)
% Multiuse: available_cols(++,--) - list available columns
available_cols(Board, Cols) :-
    Board = board(_,MaxC,_,_,_),
    findall(C, (between(1,MaxC,C), col_available(Board,C)), Cols).

% ============================================================
% Win detection
% ============================================================

% opponent(++P,--Opp) - get opponent
% opponent(--P,++Opp) - reverse
% opponent(++P,+Opp)  - verify pair
% Multiuse: opponent - bidirectional player mapping
opponent(x, o).
opponent(o, x).

% winner(++Board,++R,++C,--Player)
% Multiuse:
%   winner(++,++,++,--) - find winner after move
%   winner(++,++,++,+)  - verify specific player wins
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
% Multiuse: check_dir(++,++,++,++,++,++,++) - check direction for win
check_dir(Board, R, C, Player, K, DR, DC) :-
    count_dir(Board, R, C, Player, DR, DC, 0, N1),
    NDR is -DR, NDC is -DC,
    count_dir(Board, R, C, Player, NDR, NDC, 0, N2),
    Total is N1 + N2 - 1,
    Total >= K.

% count_dir(++Board,++R,++C,++Player,++DR,++DC,++Acc,--Count)
% Multiuse: count_dir(++,++,++,++,++,++,++,--) - count pieces in direction
count_dir(Board, R, C, Player, DR, DC, Acc, Count) :-
    Board = board(MaxR,MaxC,_,_,_),
    R >= 1, R =< MaxR, C >= 1, C =< MaxC,
    get_cell(Board, R, C, Player), !,
    Acc1 is Acc + 1,
    NR is R+DR, NC is C+DC,
    count_dir(Board, NR, NC, Player, DR, DC, Acc1, Count).
count_dir(_, _, _, _, _, _, Acc, Acc).

% win_exists(++Board,++Player)
% Multiuse: win_exists(++,++) - check if player has won
win_exists(Board, Player) :-
    Board = board(Rows,Cols,_,_,_),
    between(1,Rows,R), between(1,Cols,C),
    winner(Board, R, C, Player), !.

% ============================================================
% Heuristic evaluation
% ============================================================

% eval_board(++Board,++Player,--Score)
% Multiuse: eval_board(++,++,--) - heuristic score
eval_board(Board, Player, Score) :-
    opponent(Player, Opp),
    count_threats(Board, Player, MyPts),
    count_threats(Board, Opp, OppPts),
    Score is MyPts - OppPts.

% count_threats(++Board,++Player,--Pts)
% Multiuse: count_threats(++,++,--) - count threat windows
count_threats(Board, Player, Pts) :-
    Board = board(Rows,Cols,K,_,_),
    findall(P, window_pts(Board,Player,K,Rows,Cols,P), List),
    sum_list(List, Pts).

sum_list([], 0).
sum_list([H|T], S) :- sum_list(T, S1), S is S1 + H.

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
% Multiuse: window_score(++,++,++,++,++,++,++,--) - score one window
window_score(Board, Player, K, R, C, DR, DC, Pts) :-
    win_count(Board, Player, K, R, C, DR, DC, 0, 0, Mine, OppN),
    ( OppN > 0 -> Pts = 0 ; score_for(Mine, Pts) ).

% win_count(++B,++P,++K,++R,++C,++DR,++DC,++AM,++AO,--M,--O)
% Multiuse: win_count(++,++,++,++,++,++,++,++,++,--,--) - count in window
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

% score_for(++N,--Score) - points for N own pieces in free window
score_for(1, 1).
score_for(2, 10).
score_for(3, 100).
score_for(4, 1000).
score_for(N, 100000) :- N >= 5.

% ============================================================
% MinMax with Alpha-Beta (depth in half-moves)
% ============================================================

% minimax(++Board,++Depth,++Alpha,++Beta,++Player,++IsMax,--Score)
% Multiuse: minimax(++,++,++,++,++,++,--) - evaluate node
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
% Multiuse: expand_max(++,++,++,++,++,++,++,++,--) - maximizer node
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
% Multiuse: expand_min(++,++,++,++,++,++,++,++,--) - minimizer node
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
% Multiuse: best_move(++,++,++,--) - choose best AI move
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

% max_pair(++List,--Best) - find pair with maximum first element
% Multiuse: max_pair(++,--) - find maximum scored move
max_pair([H], H).
max_pair([S-C|Rest], Best) :-
    max_pair(Rest, BS-BC),
    ( S >= BS -> Best = S-C ; Best = BS-BC ).

% ============================================================
% Bridge predicates - called from JavaScript
% ============================================================

% js_new_game(++Rows,++Cols,++K,++ForbAtom,--BoardAtom)
js_new_game(Rows, Cols, K, ForbAtom, BoardAtom) :-
    term_to_atom(Forb, ForbAtom),
    new_game(Rows, Cols, K, Forb, Board),
    term_to_atom(Board, BoardAtom).

% js_human_move(++BoardAtom,++Col,++PlayerAtom,--NewBoardAtom,--Result)
% Result: ok / full / win / draw
js_human_move(BoardAtom, Col, PlayerAtom, NewBoardAtom, Result) :-
    term_to_atom(Board, BoardAtom),
    term_to_atom(Player, PlayerAtom),
    ( col_available(Board, Col) ->
        drop_piece(Board, Col, Player, NewBoard, Row),
        term_to_atom(NewBoard, NewBoardAtom),
        ( winner(NewBoard, Row, Col, Player) ->
            Result = win
        ; available_cols(NewBoard, []) ->
            Result = draw
        ;
            Result = ok
        )
    ;
        NewBoardAtom = BoardAtom,
        Result = full
    ).

% js_ai_move(++BoardAtom,++PlayerAtom,++Depth,--NewBoardAtom,--Col,--Result)
js_ai_move(BoardAtom, PlayerAtom, Depth, NewBoardAtom, Col, Result) :-
    term_to_atom(Board, BoardAtom),
    term_to_atom(Player, PlayerAtom),
    ( available_cols(Board, []) ->
        NewBoardAtom = BoardAtom,
        Col = -1,
        Result = draw
    ;
        best_move(Board, Player, Depth, Col),
        drop_piece(Board, Col, Player, NewBoard, Row),
        term_to_atom(NewBoard, NewBoardAtom),
        ( winner(NewBoard, Row, Col, Player) ->
            Result = win
        ; available_cols(NewBoard, []) ->
            Result = draw
        ;
            Result = ok
        )
    ).

% js_get_cells(++BoardAtom,--CellsAtom)
js_get_cells(BoardAtom, CellsAtom) :-
    term_to_atom(board(_,_,_,_,Cells), BoardAtom),
    term_to_atom(Cells, CellsAtom).

% js_board_info(++BoardAtom,--Rows,--Cols,--K)
js_board_info(BoardAtom, Rows, Cols, K) :-
    term_to_atom(board(Rows,Cols,K,_,_), BoardAtom).