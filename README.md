# Stepper Motor GPIO Library for Raspberry Pi

This library requires the [πGPIO](https://github.com/paoloo/gpio) library. It has only been tested with the 28BYJ-48 5V stepper motor with a ULN2003 controller board. I would love to see it expanded to handle other stepper motors.

## Requirements

- **Raspbian**
- **erlang** 17+
- **rebar3** for library management

## Usage

Your **rebar.config** file should look like this:

``` erlang
{deps, [
    {stepper, {git, "git://github.com/pymander/stepper.git", { branch, "master" }}},
    ... % other dependencies goes here
]}.
```

You can then compile your project or whatever like this:

``` shell
rebar3 compile
```

Now from `rebar3 shell`, you can do things like this:

``` erlang-repl
24> Motor = stepper:init(5, 12, 13, 23, 24). 
{motor,5,[<0.204.0>,<0.208.0>,<0.212.0>,<0.216.0>]}
25> stepper:forward(Motor, 256).             
ok
26> stepper:reverse(Motor, 300).
ok
```

## License
[MIT License](LICENSE.md) © 2017 Erik L. Arneson
