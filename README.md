# uncertain-gantt

See [resources/example.ug](resources/example.ug) (or the `example.ug` file in the packaged release) for details about the language.

If you're running from source, you can run the examples below with `cabal run uncertain-gantt --`.

```
$ ./uncertain-gantt --help
Usage:
  uncertain-gantt -                 # reads from stdin in file mode
  uncertain-gantt --interactive     # (-i) interactive mode
  uncertain-gantt <script-file>     # reads from file
  uncertain-gantt <script-file> -i  # reads from file and enters interactive mode
```

In interactive mode, defining a task will enter multi-line input mode. To submit the multi-line input, just input an empty line.
```
$ ./uncertain-gantt -- -i
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
$ ./uncertain-gantt example.ug
Up to date
Tasks:
Build A: Build some stuff
Build B: Build other stuff
Discovery: Find things out
Integrate: Build other stuff
Sell Stuff

Example run:
Discovery           TeamA #   #############
Build A             TeamA #                #########
Sell Stuff          TeamA #                ####################################
Build B             TeamB *                ****
Integrate           TeamB *                         ************************
User Test           TeamC >                                                 >>>>>>>>>>>>>>>>>
Completes at: 63

Running 1000 simulations...
Completion time mean: 77.9670000000001
Completion time p5: 57.0
Completion time p10: 61.0
Completion time p25: 68.0
Completion time p50: 76.0
Completion time p75: 86.0
Completion time p90: 96.0
Completion time p95: 103.0
```
