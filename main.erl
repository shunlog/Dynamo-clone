-module(main).
-compile(export_all).


random_element(List) ->
    N = length(List),
    Index = rand:uniform(N),  % Generates a random index (1 to N)
    lists:nth(Index, List).   % Gets the element at the random index



gossip_node() ->
    gossip_node(sets:from_list([self()], [{version, 2}])).

gossip_node(seed, Pid) ->
    gossip_node(sets:from_list([self(), Pid], [{version, 2}])).

%% Receives set of node Pids
gossip_node(Nodes) ->
    %% Read all the messages first
    receive
        {From, info} -> From ! Nodes;
        {members, SenderNodes} -> 
            Diff = sets:subtract(SenderNodes, Nodes),
            DiffSize = sets:size(Diff),
            if
                DiffSize =/= 0 ->
                    io:format("~p found ~p~n", [self(), Diff]);
                true -> ok
            end,
            gossip_node(sets:union(Nodes, SenderNodes))
    after 1000 ->
            ok
    end,
    %% Then try to connect to a node
    %% TODO: try-except instead of check
    Size = sets:size(Nodes),
    if 
        Size =/= 0 ->
            Pid = random_element(sets:to_list(Nodes)),
            Pid ! {members, Nodes};
        true ->ok
    end,
    gossip_node(Nodes).



main() ->
    Pid = spawn(main, gossip_node, []),
    NodesCount = 10,
    SpawnNode = fun(_) -> spawn(main, gossip_node, [seed, Pid]) end,
    lists:foreach(SpawnNode, lists:seq(1, NodesCount)).
  
