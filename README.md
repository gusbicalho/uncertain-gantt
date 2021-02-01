# uncertain-gantt

[![Hackage](https://img.shields.io/hackage/v/uncertain-gantt.svg?logo=haskell)](https://hackage.haskell.org/package/uncertain-gantt)

In interactive mode, use `\` at the end of a line to enter multi-line input (necessary to input tasks).
```
$ cabal run uncertain-gantt
> resource TeamA 1
> task First\
|   TeamA
|   uniform 1 10
|   Do first things first
|
> print descriptions
Tasks:
First: Do first things first
>
```

In script mode, task parameters must be indented by `  ` (two spaces), but you don't need `\` at the end. See `resources/example.ug`.

```
$ cabal run uncertain-gantt resources/example.ug

Tasks:
Build A: Build some stuff
Build B: Build other stuff
Discovery: Find things out
Integrate: Build other stuff
Sell Stuff
User Test

Example run:
Discovery           TeamA #   ###############
Build A             TeamA #                  ###########
Sell Stuff          TeamA #                  #############################
Build B             TeamB *                  **
Integrate           TeamB *                             *******************************************
User Test           TeamA #                                                                        ########################################
Completes at: 109

Running 1000 simulations...
Completion time mean: 93.70899999999975
Completion time p5: 73.0
Completion time p10: 77.0
Completion time p25: 84.0
Completion time p50: 93.0
Completion time p75: 103.0
Completion time p90: 111.0
Completion time p95: 116.0
```
