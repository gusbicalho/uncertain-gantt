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

[... Lots of output! ...]

Running 10000 simulations...
Completion time mean: 77.78970000000946
Completion time p5: 57.0
Completion time p10: 61.0
Completion time p25: 68.0
Completion time p50: 76.0
Completion time p75: 86.0
Completion time p90: 96.0
Completion time p95: 103.0
-Infinit    0.43%
    52.9  #  1.50%
    55.8  ###  3.36%
    58.7  ######  5.68%
    61.6  ########  8.21%
    64.5  ##########  10.11%
    67.4  ############  12.03%
    70.3  ############  12.28%
    73.2  ###########  10.92%
    76.1  ##########  9.75%
    79.0  ######  5.77%
    81.9  ######  5.89%
    84.8  ####  4.49%
87.69999  ###  3.37%
    90.6  ##  1.98%
    93.5  #  1.36%
    96.4  #  0.90%
    99.3  #  0.61%
102.1999    0.46%
   105.1    0.26%
   108.0  #  0.60%
```
