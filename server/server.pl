% server.pl - SWI-Prolog HTTP server for K-in-a-row game
% Author: Avdieienko Dmytro Maksymovych
%
% Endpoints:
%   POST /ai     - compute AI move (Minimax + Alpha-Beta)
%   GET  /health - health check (for Render.com)
%
% Request JSON for /ai:
%   {"board":"eee...","player":"x","depth":3,"rows":6,"cols":7,"k":4}
% Response JSON:
%   {"col":4,"row":6,"board":"eee...x","result":"ok"}
%   result: "ok" | "win" | "draw"

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).

:- set_setting(http:cors, [*]).

:- http_handler(root(ai),     handle_ai,     [method(post)]).
:- http_handler(root(health), handle_health, []).
:- http_handler(root(.),      cors_preflight,[method(options), prefix]).

% ============================================================
% HTTP handlers
% ============================================================

cors_preflight(Request) :-
    cors_enable(Request, [methods([post, get, options])]),
    format("Content-type: text/plain~n~n").

handle_ai(Request) :-
    cors_enable(Request, [methods([post, options])]),
    http_read_json_dict(Request, D, []),
    atom_string(BoardAtom, D.board),
    atom_string(Player,    D.player),
    Depth = D.depth,
    Rows  = D.rows,
    Cols  = D.cols,
    K     = D.k,
    (   js_ai(BoardAtom, Player, Depth, Rows, Cols, K, Col, Row, NewBoard, Result)
    ->  atom_string(NewBoard, NBS),
        atom_string(Result, RS),
        reply_json_dict(json{col:Col, row:Row, board:NBS, result:RS})
    ;   reply_json_dict(json{error:"ai_failed"}, [status(500)])
    ).

handle_health(Request) :-
    cors_enable(Request, [methods([get, options])]),
    reply_json_dict(json{status:"ok"}).

% ============================================================
% atom <-> row-list  (board atom = flat string of R*C chars)
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
% Cell access  O(R + C)
% ============================================================

get_cell(RL, R, C, Val) :- row_nth(R, RL, Row), col_nth(C, Row, Val).

row_nth(1, [R|_], R) :- !.
row_nth(N, [_|T], R) :- N > 1, N1 is N-1, row_nth(N1, T, R).

col_nth(1, [V|_], V) :- !.
col_nth(N, [_|T], V) :- N > 1, N1 is N-1, col_nth(N1, T, V).

set_cell(RL, R, C, Val, NRL) :- set_row(RL, R, C, Val, NRL).

set_row([Row|Rs], 1, C, Val, [NRow|Rs]) :- !, set_col(Row, C, Val, NRow).
set_row([Row|Rs], R, C, Val, [Row|NRs]) :-
    R > 1, R1 is R-1, set_row(Rs, R1, C, Val, NRs).

set_col([_|T], 1, Val, [Val|T]) :- !.
set_col([H|T], C, Val, [H|NT]) :- C > 1, C1 is C-1, set_col(T, C1, Val, NT).

% ============================================================
% Gravity
% ============================================================

opponent(x, o).
opponent(o, x).

find_bottom(RL, Rows, Col, Row) :- find_bot(RL, Rows, Col, Rows, Row).

find_bot(RL, _, Col, CurR, Row) :-
    CurR >= 1,
    get_cell(RL, CurR, Col, Val),
    ( Val = e -> Row = CurR ; CurR1 is CurR-1, find_bot(RL, _, Col, CurR1, Row) ).

drop_piece(RL, Rows, Col, Player, NewRL, Row) :-
    find_bottom(RL, Rows, Col, Row),
    set_cell(RL, Row, Col, Player, NewRL).

% available_cols reads only top row — O(Cols)
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
    sort_center(T, Mid, ST), DH is abs(H-Mid), ins_dist(H, DH, ST, Mid, Sorted).

ins_dist(C, _, [], _, [C]).
ins_dist(C, DC, [H|T], Mid, [C,H|T]) :- DH is abs(H-Mid), DC =< DH, !.
ins_dist(C, DC, [H|T], Mid, [H|R]) :- ins_dist(C, DC, T, Mid, R).

% ============================================================
% Win detection — O(4*K) at landing cell only
% ============================================================

winner(RL, Rows, Cols, K, R, C, Player) :-
    get_cell(RL, R, C, Player), Player \= e, Player \= b,
    ( check_dir(RL, Rows, Cols, K, R, C, Player, 0,  1)
    ; check_dir(RL, Rows, Cols, K, R, C, Player, 1,  0)
    ; check_dir(RL, Rows, Cols, K, R, C, Player, 1,  1)
    ; check_dir(RL, Rows, Cols, K, R, C, Player, 1, -1)
    ).

check_dir(RL, Rows, Cols, K, R, C, Player, DR, DC) :-
    count_dir(RL, Rows, Cols, R, C, Player,  DR,  DC, 0, N1),
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
% Heuristic — single pass O(Rows*Cols)
% ============================================================

eval_board(RL, Cols, Player, Score) :-
    Mid is (Cols+1)//2, opponent(Player, Opp),
    score_rows(RL, Player, Opp, Mid, 0, Score).

score_rows([], _, _, _, Acc, Acc).
score_rows([Row|Rs], P, Opp, Mid, Acc, Score) :-
    score_row(Row, P, Opp, Mid, 1, Acc, Acc2),
    score_rows(Rs, P, Opp, Mid, Acc2, Score).

score_row([], _, _, _, _, Acc, Acc).
score_row([V|T], P, Opp, Mid, C, Acc, Score) :-
    ( V = P   -> W is 3-min(2,abs(C-Mid)), NAcc is Acc+W
    ; V = Opp -> W is 3-min(2,abs(C-Mid)), NAcc is Acc-W
    ; NAcc = Acc
    ),
    C1 is C+1, score_row(T, P, Opp, Mid, C1, NAcc, Score).

% ============================================================
% Minimax + Alpha-Beta pruning
%
% minimax(++RL, ++Rows, ++Cols, ++K,
%         ++LastR, ++LastC, ++LastPlayer,
%         ++Depth, ++Alpha, ++Beta,
%         ++MaxPlayer, ++IsMax,
%         --Score)
%
% Win check only at (LastR,LastC) — avoids full-board scan.
% Columns sorted center-first for better pruning.
% ============================================================

minimax(RL, Rows, Cols, K, LR, LC, LP, _, _, _, Player, _, 100000) :-
    LP \= Player, winner(RL, Rows, Cols, K, LR, LC, LP), !.
minimax(RL, Rows, Cols, K, LR, LC, LP, 0, _, _, Player, _, Score) :-
    !,
    ( LP = Player, winner(RL, Rows, Cols, K, LR, LC, LP)
    -> Score = 100000
    ;  eval_board(RL, Cols, Player, Score)
    ).
minimax(RL, Rows, Cols, K, _, _, _, Depth, Alpha, Beta, Player, IsMax, Score) :-
    available_cols(RL, Cols, Cols_),
    ( Cols_ = []
    -> eval_board(RL, Cols, Player, Score)
    ;  opponent(Player, NextP), D1 is Depth-1,
       ( IsMax = true
       -> expand_max(RL, Rows, Cols, K, D1, Alpha, Beta, Player, NextP, Cols_, Alpha, Score)
       ;  expand_min(RL, Rows, Cols, K, D1, Alpha, Beta, Player, NextP, Cols_, Beta,  Score)
       )
    ).

expand_max(_, _, _, _, _, _, _, _, _, [], CurA, CurA).
expand_max(RL, Rows, Cols, K, D, Alpha, Beta, Player, NextP, [C|Rest], CurA, Best) :-
    drop_piece(RL, Rows, C, Player, NRL, Row),
    minimax(NRL, Rows, Cols, K, Row, C, Player, D, CurA, Beta, NextP, false, CS),
    NewA is max(CurA, CS),
    ( NewA >= Beta -> Best = NewA
    ; expand_max(RL, Rows, Cols, K, D, Alpha, Beta, Player, NextP, Rest, NewA, Best)
    ).

expand_min(_, _, _, _, _, _, _, _, _, [], CurB, CurB).
expand_min(RL, Rows, Cols, K, D, Alpha, Beta, Player, NextP, [C|Rest], CurB, Best) :-
    drop_piece(RL, Rows, C, Player, NRL, Row),
    minimax(NRL, Rows, Cols, K, Row, C, Player, D, Alpha, CurB, NextP, true, CS),
    NewB is min(CurB, CS),
    ( NewB =< Alpha -> Best = NewB
    ; expand_min(RL, Rows, Cols, K, D, Alpha, Beta, Player, NextP, Rest, NewB, Best)
    ).

best_move(RL, Rows, Cols, K, Player, Depth, BestCol) :-
    available_cols(RL, Cols, [First|RestCols]),
    opponent(Player, NextP), D1 is Depth-1,
    drop_piece(RL, Rows, First, Player, NRL0, Row0),
    minimax(NRL0, Rows, Cols, K, Row0, First, Player, D1, -10000000, 10000000, NextP, false, S0),
    best_move_ab(RL, Rows, Cols, K, D1, Player, NextP, RestCols, S0, First, BestCol).

best_move_ab(_, _, _, _, _, _, _, [], _, BC, BC).
best_move_ab(RL, Rows, Cols, K, D, Player, NextP, [C|Rest], CurBS, CurBC, FC) :-
    drop_piece(RL, Rows, C, Player, NRL, Row),
    minimax(NRL, Rows, Cols, K, Row, C, Player, D, CurBS, 10000000, NextP, false, CS),
    ( CS > CurBS
    -> best_move_ab(RL, Rows, Cols, K, D, Player, NextP, Rest, CS,    C,     FC)
    ;  best_move_ab(RL, Rows, Cols, K, D, Player, NextP, Rest, CurBS, CurBC, FC)
    ).

% ============================================================
% Main predicate (called by JS via HTTP)
% ============================================================

% js_ai(++BoardAtom, ++Player, ++Depth, ++Rows, ++Cols, ++K,
%        --ColOut, --RowOut, --NewBoardAtom, --Result)
js_ai(BoardAtom, Player, Depth, Rows, Cols, K, ColOut, RowOut, NewBoardAtom, Result) :-
    atom_to_board(BoardAtom, Cols, RL),
    ( available_cols(RL, Cols, []) ->
        ColOut = -1, RowOut = -1, NewBoardAtom = BoardAtom, Result = draw
    ;
        best_move(RL, Rows, Cols, K, Player, Depth, Col),
        drop_piece(RL, Rows, Col, Player, NewRL, Row),
        board_to_atom(NewRL, NewBoardAtom),
        ColOut = Col, RowOut = Row,
        ( winner(NewRL, Rows, Cols, K, Row, Col, Player) -> Result = win
        ; available_cols(NewRL, Cols, [])               -> Result = draw
        ; Result = ok
        )
    ).

% ============================================================
% Server startup
% ============================================================

:- initialization(main, main).

main :-
    ( getenv('PORT', PS) -> atom_number(PS, Port) ; Port = 8080 ),
    format("K-in-a-row SWI-Prolog server starting on port ~w~n", [Port]),
    http_server(http_dispatch, [port(Port)]),
    format("Server ready. Waiting for requests...~n"),
    thread_get_message(_).  % block main thread forever
