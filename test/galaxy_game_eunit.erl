-module(galaxy_game_eunit).

-include_lib("eunit/include/eunit.hrl").

galaxy_game_default_test_() ->
    P = [mercury, venus, earth, mars],
    S = [venus, earth],
    As = [{mercury, venus}, {venus, earth}],
    A = [{nuclear, venus}, {laser, earth}],
    Exp = [earth, mars],
    
    [
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(P, S, As) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(P) end,
            %Tests
            fun (ok) ->
                    case is_valid_setup(P, S, As) of
                        {true, true, true} ->
                            ?_assertEqual(true, true);
                        V ->
                            validity_printout(V),
                            ?assertEqual(true, false)
                    end
            end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(P, S, As) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(P) end,
            %Tests
            fun (ok) -> [?_assertEqual(Exp, galaxy_game:simulate_attack(P, A))] end
        }
    ].
galaxy_game_teardown_test_() ->
    P = [mercury, venus, earth, mars],
    S = [venus, earth],
    As = [{mercury, venus}, {venus, earth}],
    [
        % Teardown tests
        {setup,
            fun () -> galaxy_game:setup_universe(P, S, As) end,
            fun (ok) -> ok end,
            fun (ok) -> 
                galaxy_game:teardown_universe(P),
                [?_assertEqual(false, valid_planets(P))] end
        },
        {setup,
            fun () -> galaxy_game:setup_universe([a], [], []) end,
            fun (ok) -> ok end,
            fun (ok) -> 
                galaxy_game:teardown_universe([a]),
                [?_assertEqual(false, valid_planets([a]))] end
        }
    ].
galaxy_game_basic_laser_test_() ->
    ABCPlanets = [a,b,c],
    [
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        }
    ].
galaxy_game_shielded_laser_test_() ->
    ABCPlanets = [a,b,c],
    [
        % Basic shielded laser tests
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        }
    ].
galaxy_game_alliance_laser_test_() ->
    ABCPlanets = [a,b,c],
    [
        % Basic alliance laser test
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        }
    ].
galaxy_game_alliance_one_shield_laser_test_() ->
    ABCPlanets = [a,b,c],
    [
        % Basic alliance laser test (As-B)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        % (A-Bs)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        %(As-C)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        %(A-Cs)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        %(Bs-C)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        %(B-Cs)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        }
    ].
galaxy_game_alliance_two_shield_laser_test_() ->
    ABCPlanets = [a,b,c],
    [
        % Basic alliance laser test
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b,c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b,c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b,c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{laser,a}]))] end
        }
    ].
galaxy_game_basic_nuclear_test_() ->
    ABCPlanets = [a,b,c],
    [
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        }
    ].
galaxy_game_shielded_nuclear_test_() ->
    ABCPlanets = [a,b,c],
    [
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,b,c], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,b,c], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,b,c], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        }
    ].
galaxy_game_alliance_nuclear_test_() ->
    ABCPlanets = [a,b,c],
    [
        % Basic alliance laser test
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        }
    ].
galaxy_game_alliance_one_shield_nuclear_test_() ->
    ABCPlanets = [a,b,c],
    [
        % (As-B)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        % (A-Bs)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        %(As-C)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        %(A-Cs)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        %(Bs-C)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        %(B-Cs)
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        }
    ].
galaxy_game_alliance_two_shield_nuclear_test_() ->
    ABCPlanets = [a,b,c],
    [
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,b], [{a,b}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [a,c], [{a,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b,c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,b], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,c}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b,c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([a,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,b}]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(ABCPlanets, [b,c], [{b,c}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(ABCPlanets) end,
            %Tests
            fun (ok) -> [?_assertEqual([b,c], galaxy_game:simulate_attack(ABCPlanets, [{nuclear,a}]))] end
        }
    ].
galaxy_game_misc_test_() ->
    P = [earth,mercury,uranus,venus],
    MP = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,bb,cc,dd,ee,ff,gg,hh,ii,jj,kk,ll,mm,nn,oo,pp,qq,rr,ss,tt,uu,vv,ww,xx,yy,zz],
    MS = [a,d,y,c,aa,ee,oo,uu,ii],
    MA = [{a,aa},{b,bb},{c,cc}],
    [
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(P, [mercury,uranus], [{mercury,uranus}, {venus,earth}]) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(P) end,
            %Tests
            fun (ok) -> [?_assertEqual([uranus], galaxy_game:simulate_attack(P, [{nuclear,mercury},{laser, venus},{laser,uranus} ]))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(P, [], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(P) end,
            %Tests
            fun (ok) -> [?_assertEqual(P, galaxy_game:simulate_attack(P, []))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe(MP, MS, MA) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe(MP) end,
            %Tests
            fun (ok) -> [?_assertEqual(MP, galaxy_game:simulate_attack(MP, []))] end
        },
        {setup, 
            %Setup
            fun () -> galaxy_game:setup_universe([a], [], []) end,
            %Teardown
            fun (ok) -> galaxy_game:teardown_universe([a]) end,
            %Tests
            fun (ok) -> [?_assertEqual([a], galaxy_game:simulate_attack([a], []))] end
        }
    ].

%%==============================================================================
%% Internal Functions
%%==============================================================================

is_valid_setup(Planets, Shields, Alliances) ->
    VPlanets = valid_planets(Planets),
    case VPlanets of
        true -> {VPlanets, planets_shielded(Shields), allied(Alliances)};
        false -> {false, untested, untested}
    end.

valid_planets(Planets) ->
    lists:all(fun (P) ->
                PPid = whereis(P),
                PPid /= undefined andalso erlang:is_process_alive(PPid)
        end, Planets).

planets_shielded(Shields) ->
    lists:all(fun (S) ->
                {trap_exit, true} == erlang:process_info(whereis(S), trap_exit)
        end, Shields).

allied(Alliances) ->
    lists:all(fun ({X, Y}) ->
                {links, Links} = erlang:process_info(whereis(X), links),
                YPid = whereis(Y),
                lists:member(YPid, Links)
        end, Alliances).

validity_printout(Validity) ->
    Printout = lists:zip([valid_planets, planets_shielded, planets_allied],
                         tuple_to_list(Validity)),
    ?debugFmt("Invalid Universe Setup:~n~p~n", [Printout]),
    false.