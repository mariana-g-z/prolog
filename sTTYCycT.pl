solve :-
    start(State),
    length(Moves, N),
    dfs([State], Moves, Path), 
    !,
    show([start|Moves], Path),
    format('~nmoves = ~w~n', [N]).

% Búsqueda en profundidad (DFS)
dfs([State|States], [], Path) :-
    goal(State), 
    !, 
    reverse([State|States], Path).

dfs([State|States], [Move|Moves], Path) :-
    move(State, Next, Move),
    not(memberchk(Next, [State|States])), % Evitar ciclos
    dfs([Next,State|States], Moves, Path).

% Mostrar el estado del puzzle
show([], _).
show([Move|Moves], [State|States]) :-
    State = state(A, B, C, D, E, F, G, H, I),
    format('~n~w~n~n', [Move]),
    format('~w ~w ~w~n', [A, B, C]),
    format('~w ~w ~w~n', [D, E, F]),
    format('~w ~w ~w~n', [G, H, I]),
    show(Moves, States).

% Estado inicial y objetivo
start(state(*, 1, 3, 4, 2, 5, 7, 8, 6)).
goal(state(1, 2, 3, 4, 5, 6, 7, 8, *)).

% Movimientos posibles

% Movimiento hacia la derecha
move(state(*, B, C, D, E, F, G, H, J), state(B, *, C, D, E, F, G, H, J), right).
move(state(*, B, C, D, E, F, G, H, J), state(D, B, C, *, E, F, G, H, J), down).

% Movimiento hacia la izquierda
move(state(A, *, C, D, E, F, G, H, J), state(*, A, C, D, E, F, G, H, J), left).
move(state(A, *, C, D, E, F, G, H, J), state(A, C, *, D, E, F, G, H, J), right).
move(state(A, *, C, D, E, F, G, H, J), state(A, E, C, D, *, F, G, H, J), down).

% Movimiento hacia arriba
move(state(A, B, *, D, E, F, G, H, J), state(A, *, B, D, E, F, G, H, J), left).
move(state(A, B, *, D, E, F, G, H, J), state(A, B, F, D, E, *, G, H, J), down).
move(state(A, B, C, *, E, F, G, H, J), state(*, B, C, A, E, F, G, H, J), up).
move(state(A, B, C, *, E, F, G, H, J), state(A, B, C, E, *, F, G, H, J), right).
move(state(A, B, C, *, E, F, G, H, J), state(A, B, C, G, E, F, *, H, J), down).

% Movimiento hacia abajo
move(state(A, B, C, D, *, F, G, H, J), state(A, *, C, D, B, F, G, H, J), up).
move(state(A, B, C, D, *, F, G, H, J), state(A, B, C, D, F, *, G, H, J), right).
move(state(A, B, C, D, *, F, G, H, J), state(A, B, C, D, H, F, G, *, J), down).
move(state(A, B, C, D, *, F, G, H, J), state(A, B, C, *, D, F, G, H, J), left).

% Movimiento hacia arriba
move(state(A, B, C, D, E, *, G, H, J), state(A, B, *, D, E, C, G, H, J), up).
move(state(A, B, C, D, E, *, G, H, J), state(A, B, C, D, *, E, G, H, J), left).
move(state(A, B, C, D, E, *, G, H, J), state(A, B, C, D, E, J, G, H, *), down).

% Movimiento hacia la izquierda
move(state(A, B, C, D, E, F, *, H, J), state(A, B, C, D, E, F, H, *, J), left).
move(state(A, B, C, D, E, F, *, H, J), state(A, B, C, *, E, F, D, H, J), up).
move(state(A, B, C, D, E, F, G, *, J), state(A, B, C, D, E, F, *, G, J), left).
move(state(A, B, C, D, E, F, G, *, J), state(A, B, C, D, *, F, G, E, J), up).

% Movimiento hacia abajo
move(state(A, B, C, D, E, F, G, H, *), state(A, B, C, D, E, *, G, H, F), up).
move(state(A, B, C, D, E, F, G, H, *), state(A, B, C, D, E, F, G, *, H), left).

