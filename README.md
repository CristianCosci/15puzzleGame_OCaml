# 15puzzleGame_OCaml
Implementation of A-star(A*) to solve 15-Puzzle game in OCaml


In the repo there is a report explaining the project and in the project folder there are some example instances (in the .txt) and the Ocaml file (the actual program).

Heuristics implemented:
1. **Hamming**
2. **Manhattan distance**
3. **Linear conflicts** + **Manhattan distance**

Both the A* algorithm and the Best First search (obviously less efficient) have been implemented.
For algorithm evaluations see the attached report.
To change the type of algorithm just change the distance function used to sort the priority queue, in this way the evaluation function **f(n)** can be chosen by the user.

It is possible to change the various instances to be solved by changing the variable Initial state in the file. At this point the main is able to check if the instance is valid and solvable and, if successful, to resolve it.

### Commands to compile and run the program:
```bash 
ocamlc puzzleGame.ml -o puzzleGame
./puzzleGame
```

### Credits:
* https://algorithmsinsight.wordpress.com/graph-theory-2/a-star-in-general/implementing-a-star-to-solve-n-puzzle/
