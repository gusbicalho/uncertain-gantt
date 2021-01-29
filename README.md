# uncertain-gantt

[![Hackage](https://img.shields.io/hackage/v/uncertain-gantt.svg?logo=haskell)](https://hackage.haskell.org/package/uncertain-gantt)

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
Build A             TeamA #                  ####################
Sell Stuff          TeamA #                  ##################################
Build B             TeamB *                  *****
Integrate           TeamB *                                      ***************************************
User Test           TeamA #                                                                             #############################################
Completes at: 119

Running 1000 simulations...
Completion time p5: 72.0
Completion time p10: 76.0
Completion time p25: 83.0
Completion time p50: 91.0
Completion time p75: 100.0
Completion time p90: 108.0
Completion time p95: 112.0
```
