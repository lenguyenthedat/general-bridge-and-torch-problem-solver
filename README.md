Introduction:
------------
Original Problem on Wikipedia's [Bridge and Torch problem](http://en.wikipedia.org/wiki/Bridge_and_torch_problem).

Generalized Problem Definition & Solution by Professor [Roland Backhouse](http://www.cs.nott.ac.uk/~rcb/): 
- [Slides (pdf)](http://aps.cs.nott.ac.uk/wp-content/uploads/2008/05/capacity-c-torch-problem-aps-club.pdf)
- [Paper (ps)](http://www.cs.nott.ac.uk/~rcb/MPC/GeneralTorchProblem.ps)

Solution Design:
----------------
![Design](https://github.com/lenguyenthedat/general-bridge-and-torch-problem-solver/blob/master/Overview.png)

Installation and Usage: 
-----------------------

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
     
    [1,2] go across...
    [1] returns...
    [5,8] go across...
    [2] returns...
    [1,2] go across...

    Crossing Time: 15 minutes.
 
