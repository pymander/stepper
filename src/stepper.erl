%% @author Erik L. Arneson <earneson@arnesonium.com>
%% @copyright 2017 Erik L. Arneson (MIT license)
%% @version 0.9.0
%% @doc
%% A Raspberry Pi GPIO library for the 28BYJ-48 5V stepper motor with a ULN2003 controller board.
%% @end

-module(stepper).

%% API exports
-export([init/5, release/1, clearMotor/1, forward/2, reverse/2]).

-record(motor,
        { delay = 10,
          pins }).

pin_sequence() -> [[1,0,0,1],
                   [1,0,0,0],
                   [1,1,0,0],
                   [0,1,0,0],
                   [0,1,1,0],
                   [0,0,1,0],
                   [0,0,1,1],
                   [0,0,0,1]].

%%====================================================================
%% API functions
%%====================================================================

init(Delay, Pin1, Pin2, Pin3, Pin4) ->
    #motor{delay = Delay,
           pins = lists:map(fun (P) ->
                                    gpio:init(P, out)
                            end,
                            [Pin1, Pin2, Pin3, Pin4])}.

release(Motor) ->
    lists:map(fun (P) ->
                      gpio:stop(P)
              end,
              Motor#motor.pins).

forward(Motor, Steps) -> loopSequence(Motor, Steps, pin_sequence()).

reverse(Motor, Steps) -> loopSequence(Motor, Steps, lists:reverse(pin_sequence())).

%%====================================================================
%% Internal functions
%%====================================================================

writeStep(Motor, Step) ->
    lists:zipwith(fun (P, V) ->
                        gpio:write(P, V)
                  end,
                  Motor#motor.pins, Step),
    timer:sleep(Motor#motor.delay),
    ok.

clearMotor(Motor) ->
    writeStep(Motor, [0,0,0,0]),
    ok.

writeSequence(Motor, Steps, [H|Seq]) when Steps > 0 ->
    writeStep(Motor, H),
    writeSequence(Motor, Steps - 1, Seq);
writeSequence(_Motor, Steps, []) ->
    Steps;
writeSequence(_Motor, 0, _) ->
    0.

loopSequence(Motor, Steps, Sequence) when Steps > 0 ->
    loopSequence(Motor, writeSequence(Motor, Steps, Sequence), Sequence);
loopSequence(Motor, 0, _) ->
    clearMotor(Motor).

    
    
    
    
