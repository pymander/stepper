% Stepper motor GPIO stuff.

-module(stepper).
-export([init/5, clearMotor/1, forward/2, reverse/2]).
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

init(Delay, Pin1, Pin2, Pin3, Pin4) ->
    #motor{delay = Delay,
           pins = lists:map(fun (P) ->
                                    gpio:init(P, out)
                            end,
                            [Pin1, Pin2, Pin3, Pin4])}.

writeStep(Motor, Step) ->
    lists:zipwith(fun (P, V) ->
                        gpio:write(P, V)
                  end,
                  Motor#motor.pins, Step),
    receive after Motor#motor.delay -> ok end,
    ok.

clearMotor(Motor) ->
    writeStep(Motor, [0,0,0,0]),
    ok.

forward(Motor, Steps) -> forwardSeq(Motor, Steps, pin_sequence()).

forwardSeq(Motor, Steps, [H|Seq]) when Steps > 0 ->
    writeStep(Motor, H),
    forwardSeq(Motor, Steps - 1, Seq);
forwardSeq(Motor, Steps, []) when Steps > 0 ->
    forwardSeq(Motor, Steps, pin_sequence());
forwardSeq(Motor, 0, _) ->
    clearMotor(Motor).

reverse(Motor, Steps) -> reverseSeq(Motor, Steps, lists:reverse(pin_sequence())).

reverseSeq(Motor, Steps, [H|Seq]) when Steps > 0 ->
    writeStep(Motor, H),
    reverseSeq(Motor, Steps - 1, Seq);
reverseSeq(Motor, Steps, []) when Steps > 0 ->
    reverseSeq(Motor, Steps, lists:reverse(pin_sequence()));
reverseSeq(Motor, 0, _) ->
    clearMotor(Motor).

    
    
    
