Introduction:
------------
Original Problem: http://en.wikipedia.org/wiki/Bridge_and_torch_problem

General Solution by Professor Roland Backhouse: www.cs.nott.ac.uk/~rcb/MPC/GeneralTorchProblem.ps

Installation and Usage: 
--------------------

    ~/GitHub/cabinet/General-Capacity-C-Torch-Problem-Solver $ cabal install
    
    Resolving dependencies...
    Configuring TorchSolver-0.1.0.0...
    Building TorchSolver-0.1.0.0...
    Preprocessing executable 'TorchSolver' for TorchSolver-0.1.0.0...
    Warning: No documentation was generated as this package does not contain a
    library. Perhaps you want to use the --executables flag.
    Installing executable(s) in
    ~/Library/Haskell/ghc-7.6.3/lib/TorchSolver-0.1.0.0/bin
    Installed TorchSolver-0.1.0.0
    Updating documentation index
    ~/Library/Haskell/doc/index.html

    ~/GitHub/cabinet/General-Capacity-C-Torch-Problem-Solver $ TorchSolver 
    
    Input Total Number of People:
    4
    Input Bridge capacity:
    2
    Input time taken for each person, seperated by 1 space:
    1 2 5 8
    Best Sequences: 
     
    [1,2] go(es) across...
    [1] return(s)...
    [5,8] go(es) across...
    [2] return(s)...
    [1,2] go(es) across...
     
    Crossing Time: 15 minutes.
 
