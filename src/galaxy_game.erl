%% @doc
%% Implementation module for the galactic battle simulator.
%% The following example shows the expected behavior of the simulator:
%%
%% Planets=[mercury,uranus,venus, earth]
%% Shields=[mercury,uranus]
%% Alliances=[{mercury, uranus}, {venus, earth}]
%% Actions=[{nuclear,mercury},{laser,venus}, {laser, uranus}]
%%
%% ExpectedSurvivors = [uranus]
%% In order to produce this expected results, the following calls will be tested:
%% * ok = setup_universe(Planets, Shields, Alliances)
%% * [uranus] = simulate_attack(Planets, Actions)
%% * ok = teardown_universe(Planets)
%%
%% All the 3 calls will be tested in order to check they produce the expected
%% side effects (setup_universe/3 creates a process per planet, etc)
%% @end

-module(galaxy_game).

-include_lib("eunit/include/eunit.hrl").

-type planet()::atom().
-type shield()::planet().
-type alliance()::{planet(), planet()}.
-type attack()::{laser | nuclear, planet()}.

-export([setup_universe/3, teardown_universe/1, simulate_attack/2]).

%% @doc Set up a universe described by the input.
%% The imput is asumed to be minimal and non redundant (i.e. if there is an
%% alliance {a, b} there won't be an alliance {b, a}).
%% Once this function returns, the universe is expected to be fully ready,
%% shields, alliances and all.
-spec setup_universe([planet()], [shield()], [alliance()]) -> ok.
%% @end
setup_universe(Planets, Shields, Alliances) ->

	lists:map(
		fun(Planet) -> 
			PID = spawn(fun() -> spawn_planet() end),
			register(Planet, PID) 
		end, Planets),

	lists:map(
		fun(PlanetToShield) -> 
			PlanetToShield ! {shield, true} 
		end, Shields),

	lists:map(
		fun(Alliance) -> 
			{Planet1, Planet2} = Alliance, 
			Planet1 ! {alliance, Planet2} 
		end, Alliances),
	
	lists:map(
		fun(Planet) ->
			Planet ! {self(), ping},
			receive
				{_, pong} -> ok
			after
				5000 -> exit(timeout)
			end
		end, Planets),

    ok.

%% @doc Clean up a universe simulation.
%% This function will only be called after calling setup_universe/3 with the
%% same set of planets.
%% Once this function returns, all the processes spawned by the simulation
%% should be gone.
-spec teardown_universe([planet()]) -> ok.
%% @end
teardown_universe(Planets) ->
	lists:map(fun(Planet) -> 
		case whereis(Planet) of
			undefined -> already_dead;
			Pid ->
			 	case is_process_alive(Pid) of
			 		true -> Planet ! {self(),teardown},
						receive
							{_, dead} -> ok;
							_ -> exit(improper_shutdown)
						after
							5000 -> exit(timeout)
						end;
					false -> 
						exit(Pid, kill),
						unregister(Planet)
				end
		end
	end, Planets),
    ok.

%% @doc Simulate an attack.
%% This function will only be called after setting up a universe with the same
%% set of planets.
%% It returns the list of planets that have survived the attack
-spec simulate_attack([planet()], [attack()]) -> Survivors::[planet()].
%% @end
simulate_attack(Planets, Actions) ->
	register(simulation,self()),
	PlanetMap = lists:foldl(
		fun(Planet, Acc) -> 
			PID = whereis(Planet),
			MRef = monitor(process,PID),
			[{PID, {Planet,MRef}}|Acc]
		end, [], Planets
		),
	lists:map(
		fun(Action) -> 
			{Attack, Planet} = Action, 
			exit(whereis(Planet), Attack)
		end
		, Actions),
	%{_,_,HandleStart} = os:timestamp(),
	Victims = handle_combat_responses(PlanetMap, [], 1),
	%{_,_,HandleEnd} = os:timestamp(),
	%Diff = HandleEnd - HandleStart,
	%io:format("HandleCombatResponses: ~p Units ~n", [Diff]),
	Survivors = lists:subtract(Planets,Victims),
	unregister(simulation),
	lists:map(
		fun(Planet) ->
			{_,{_,Ref}} = Planet,
			demonitor(Ref,[flush])
		end, PlanetMap),
    Survivors.

handle_combat_responses(Planets, Victims, Timeout) ->
	receive
		{'DOWN', _, process, From, Reason} -> 
			{Name,_} = proplists:get_value(From, Planets),
			io:format("Planet ~p died because ~p ~n", [Name, Reason]),
			handle_combat_responses(Planets, [Name|Victims], 0)
	after
		Timeout ->
			Victims
	end.

spawn_planet() ->
	receive
		{Parent, ping} ->
			Parent ! {self(), pong};
		{shield, true} -> 
			io:format("Got shield message! ~n"),
			process_flag(trap_exit, true);
		{alliance, Ally} ->
			io:format("Got alliance message! ~n"),
			link(whereis(Ally));
		{From, teardown} ->
			io:format("Got exit signal. ~n"),
			From ! {self(), dead},
			exit(kill);
		{'EXIT', From, nuclear} ->
			case get_registered_name(From) of
				simulation ->
					io:format("Noes! Planet ~p got hit by a nuclear missile. ~n", [get_registered_name(self())]),
					exit(kill);
				_ ->
					io:format("Ally got hit :/ ~n")
			end;
		{'EXIT', From, laser} ->
			case get_registered_name(From) of
				simulation ->
					io:format("Direct laser hit! Luckily we have shields.~n");
				_ ->
					io:format("Ally ~p died because it got hit by a laser. Planet ~p has a shield. ~n",[From,get_registered_name(self())])
			end;			
		{'EXIT', _, Msg} ->
			io:format("Received and ignored exit type ~p ~n", [Msg]);
		Msg ->
			io:format("Got unknown message: ~p ~n", [Msg])
	end,
	spawn_planet().

get_registered_name(Pid) ->
	case process_info(Pid, registered_name) of
		{registered_name, Name} ->
			Name;
		undefined ->
			unknown_process
	end.