% Definição dos vértices
:- dynamic vertice/1.
vertice(rj). % Rio de Janeiro
vertice(sp). % São Paulo
vertice(rp). % Ribeirão Preto
vertice(pp). % Petrópolis
vertice(op). % Ouro Preto
vertice(bn). % Bonito

% Definição das arestas (grafo direcionado)
:- dynamic aresta/3.
aresta(rj, pp, 20).
aresta(pp, bn, 350).
aresta(rj, rp, 160).
aresta(rj, sp, 60).
aresta(sp, rp, 30).
aresta(rp, op, 10).
aresta(sp, op, 50).
aresta(rp, bn, 40).


% Função para encontrar o menor caminho usando Dijkstra
dijkstra(Origem, Destino, Caminho, Custo) :-
  dijkstra_inicio(Origem, Destino, Caminho, Custo).

% Função auxiliar para inicializar os caminhos e custos
dijkstra_inicio(Origem, Destino, Caminho, Custo) :-
  dijkstra([0-Origem-[Origem]], [], Destino, CaminhoInvertido, Custo),
  reverse(CaminhoInvertido, [], Caminho).

% Caso base: destino alcançado
dijkstra([Custo-Destino-Caminho | _], _, Destino, Caminho, Custo).

% Recursão para encontrar o menor caminho
dijkstra([Custo-V-[V|T] | Resto], Visitados, Destino, Caminho, CustoFinal) :-
  findall(CustoNovo-W-[W,V|T],
          (aresta(V, W, Peso), \+ member(W, Visitados), CustoNovo is Custo + Peso),
          Novos),
  append(Resto, Novos, Total),
  sort(Total, TotalOrdenado),
  dijkstra(TotalOrdenado, [V|Visitados], Destino, Caminho, CustoFinal).

% Função auxiliar para inverter lista
reverse([], Acumulador, Acumulador).
reverse([H|T], Acumulador, Inversa) :-
  reverse(T, [H|Acumulador], Inversa).

% Função para encontrar todos os caminhos usando Dijkstra
dijkstra_todos_caminhos(Origem, Destino, Caminhos) :-
  findall(Caminho-Custo,
          dijkstra(Origem, Destino, Caminho, Custo),
          CaminhosNaoOrdenados),
  sort(2, @=<, CaminhosNaoOrdenados, Caminhos).

% Função para mostrar caminhos alternativos
mostrar_caminhos_alternativos(Origem, Destino) :-
  dijkstra_todos_caminhos(Origem, Destino, Caminhos),
  writeln('Caminhos do menor para o maior custo:'),
  mostrar_caminhos(Caminhos).

% Função auxiliar para mostrar a lista de caminhos
mostrar_caminhos([]).
mostrar_caminhos([Caminho-Custo | Resto]) :-
  format('Caminho: ~w, Custo: ~w~n', [Caminho, Custo]),
  mostrar_caminhos(Resto).

% Consulta para encontrar o caminho mais curto usando Dijkstra
consulta_dijkstra(Origem, Destino, Caminho, Custo) :-
  dijkstra(Origem, Destino, Caminho, Custo).

% Consulta para encontrar e mostrar caminhos alternativos
consulta_caminhos_alternativos(Origem, Destino) :-
  mostrar_caminhos_alternativos(Origem, Destino).


?- consulta_caminhos_alternativos('rio de janeiro', 'ouro preto').
?- mostrar_caminhos_alternativos('rio de janeiro', 'ouro preto').









% Diretiva discontiguous para mostrar_caminhos/1 e mostrar_caminhos_alternativos_bf/2
:- discontiguous mostrar_caminhos/1.
:- discontiguous mostrar_caminhos_alternativos_bf/2.

% Função para encontrar o menor caminho usando Bellman-Ford
bellman_ford(Origem, Destino, Caminho, Custo) :-
    inicializar_distancias(Origem),
    vertice_num(N),
    atualizar_distancias(N),
    \+ existe_ciclo_negativo,
    caminho_minimo(Origem, Destino, CaminhoInvertido, Custo),
    reverse(CaminhoInvertido, Caminho).

% Função para inicializar as distâncias
inicializar_distancias(Origem) :-
    retractall(distancia(_, _)),
    retractall(predecessor(_, _)),
    forall(vertice(V), assert(distancia(V, inf))),
    assert(distancia(Origem, 0)).

% Função para atualizar as distâncias (relaxamento das arestas)
atualizar_distancias(0) :- !.
atualizar_distancias(N) :-
    findall(_, (aresta(U, V, Peso), relaxar(U, V, Peso)), _),
    N1 is N - 1,
    atualizar_distancias(N1).

% Função para relaxar uma aresta
relaxar(U, V, Peso) :-
    distancia(U, DistU),
    DistU \= inf,
    distancia(V, DistV),
    NovoDistV is DistU + Peso,
    (NovoDistV < DistV ->
        retract(distancia(V, DistV)),
        assert(distancia(V, NovoDistV)),
        retractall(predecessor(V, _)),
        assert(predecessor(V, U))
    ; true).

% Verificação de ciclos negativos
existe_ciclo_negativo :-
    aresta(U, V, Peso),
    distancia(U, DistU),
    DistU \= inf,
    distancia(V, DistV),
    NovoDistV is DistU + Peso,
    NovoDistV < DistV.

% Função para encontrar o caminho mínimo
caminho_minimo(Origem, Destino, [Destino | Caminho], Custo) :-
    distancia(Destino, Custo),
    caminho_minimo_aux(Origem, Destino, Caminho).

caminho_minimo_aux(Origem, Origem, []).
caminho_minimo_aux(Origem, V, [U | Caminho]) :-
    predecessor(V, U),
    caminho_minimo_aux(Origem, U, Caminho).

% Função para contar o número de vértices
vertice_num(N) :-
    findall(V, vertice(V), Vertices),
    length(Vertices, N).

% Função para encontrar todos os caminhos alternativos usando Bellman-Ford
encontrar_caminhos_alternativos_bf(Origem, Destino, Caminhos) :-
    inicializar_distancias(Origem),
    vertice_num(N),
    atualizar_distancias(N),
    \+ existe_ciclo_negativo,
    findall(CaminhoInvertido-Custo,
            (distancia(Destino, Custo),
             caminho_minimo(Origem, Destino, CaminhoInvertido, Custo),
             \+ member(CaminhoInvertido, Caminhos)),
            CaminhosNaoOrdenados),
    sort(2, @=<, CaminhosNaoOrdenados, Caminhos).

% Função para mostrar caminhos alternativos com Bellman-Ford
mostrar_caminhos_alternativos_bf(Origem, Destino) :-
  encontrar_caminhos_alternativos_bf(Origem, Destino, Caminhos),
  writeln('Caminhos alternativos:'),
  mostrar_caminhos(Caminhos).



% Função auxiliar para mostrar a lista de caminhos
mostrar_caminhos([]).
mostrar_caminhos([Caminho-Custo | Resto]) :-
  reverse(Caminho, CaminhoInvertido),
  format('Caminho: ~w, Custo: ~w~n', [CaminhoInvertido, Custo]),
  mostrar_caminhos(Resto).

% Adicionar um vértice
adiciona_vertice(Vertice) :-
  \+ vertice(Vertice), % Verifica se o vértice não existe
  assert(vertice(Vertice)).

% Adicionar uma aresta
adiciona_aresta(Origem, Destino, Peso) :-
  vertice(Origem), % Verifica se o vértice de origem existe
  vertice(Destino), % Verifica se o vértice de destino existe
  \+ aresta(Origem, Destino, Peso), % Verifica se a aresta não existe
  assert(aresta(Origem, Destino, Peso)).

% Remover uma aresta
remove_aresta(Origem, Destino, Peso) :-
  retract(aresta(Origem, Destino, Peso)).

% Remover todas as arestas conectadas a um vértice
remove_arestas_vertice(Vertice) :-
  retractall(aresta(Vertice, _, _)),
  retractall(aresta(_, Vertice, _)).

% Remover um vértice e todas as suas arestas
remove_vertice(Vertice) :-
  retract(vertice(Vertice)),
  remove_arestas_vertice(Vertice).

