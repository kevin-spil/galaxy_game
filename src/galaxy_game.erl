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
	lists:map(fun(Planet) -> PID = spawn(fun() -> spawn_planet() end), register(Planet, PID)  end, Planets),
	lists:map(fun(PlanetToShield) -> PlanetToShield ! {shield, true} end, Shields),
	lists:map(fun(Alliance) -> {Planet1, Planet2} = Alliance, Planet1 ! {alliance, Planet2} end, Alliances),
	
	lists:map(fun(Planet) ->
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
			undefined -> ok;
			_ -> Planet ! {self(),teardown},
				receive
					{_, dead} -> ok
				after
					5000 -> exit(timeout)
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
	lists:map(fun(Action) -> {Attack, Planet} = Action, exit(whereis(Planet), Attack) end, Actions),
	timer:sleep(100),
	Survivors = lists:foldl(
		fun(Planet, Survivors) -> 
			case whereis(Planet) of 
				undefined -> io:format("Planet ~p died. ~n", [Planet]), Survivors;
				_ -> [Planet|Survivors] 
			end
		end, [], Planets),
    lists:reverse(Survivors).

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
		{'EXIT', FROM, nuclear} ->
			io:format("Nuked :( ~n"),
			exit(kill);
		{'EXIT', From, Msg} ->
			io:format("Received and ignored exit type ~p ~n", [Msg]);
		{Msg, Arg} ->
			io:format("Got unknown message: ~p with value: ~p ~n", [Msg, Arg]);
		_ ->
			io:format("Derp. ~n")
	end,
	spawn_planet().