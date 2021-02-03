# uncertain-gantt

```
$ cabal run uncertain-gantt -- --help
Usage:
  uncertain-gantt -                 # reads from stdin in file mode
  uncertain-gantt --interactive     # (-i) interactive mode
  uncertain-gantt <script-file>     # reads from file
  uncertain-gantt <script-file> -i  # reads from file and enters interactive mode
```

In interactive mode, defining a task will enter multi-line input mode. To submit the multi-line input, just input an empty line.
```
$ cabal run uncertain-gantt -- -i
> resource TeamA 1
> task First
|   TeamA
|   uniform 1 10
|   Do first things first
|
> print tasks briefly
Tasks:
First: Do first things first
>
```

In script mode, task parameters must be indented by two spaces. See `resources/example.ug`.

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
