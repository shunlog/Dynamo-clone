-module(main).
-compile(export_all).


random_element(List) ->
    N = length(List),
    Index = rand:uniform(N),  % Generates a random index (1 to N)
    lists:nth(Index, List).   % Gets the element at the random index



gossip_node(Timeout) ->
    gossip_node(sets:from_list([self()], [{version, 2}]), Timeout).


%% Function for the node process.
%% Keeps track of a set of node Pids, including its own
%%
%% It listens to gossip from other nodes,
%% Receiving their list of nodes and reconciling it with its own.
%%
%% If there haven't been any messages in 1 second,
%% it tries to gossip to a random node its list of nodes.
gossip_node(Nodes, Timeout) ->
    %% Read all the messages first
    receive
        {From, info} -> From ! Nodes,
                        gossip_node(Nodes, Timeout);
        {join, NewPid} -> gossip_node(sets:add_element(NewPid, Nodes), Timeout);
        {members, SenderNodes} -> 
            Diff = sets:subtract(SenderNodes, Nodes),
            DiffSize = sets:size(Diff),
            if
                DiffSize =/= 0 ->
                    io:format("~p found ~p~n", [self(), Diff]);
                true -> ok
            end,
            node_send(sets:union(Nodes, SenderNodes), Timeout)
    after Timeout ->
            node_send(Nodes, Timeout)
    end.

node_send(Nodes, Timeout) ->
    %% Gossip to a random node
    Others = sets:del_element(self(), Nodes),
    Size = sets:size(Others),
    if 
        Size =/= 0 ->
            Pid = random_element(sets:to_list(Others)),
            
            Pid ! {members, Nodes};
        true ->ok
    end,
    gossip_node(Nodes, Timeout).



main() ->
    Seed = spawn(main, gossip_node, [3000]),
    NodesCount = 10,
    SpawnNode = fun(_) -> Node = spawn(main, gossip_node, [1000]),
                          Node ! {join, Seed}
                end,
    lists:foreach(SpawnNode, lists:seq(1, NodesCount)).
  
