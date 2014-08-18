yascc
=====

Yet Another Source Code Counter - Haskell software that counts lines of codes, ignoring commented lines. 

I started this project to improve my programming skills and Haskell knowledge. The original name of the project was **locc(lines of code counter)** but a similar project had a similar name so I changed it. The code is fairly simple and throughout the various iterations of it I'll try to clean, optimize it and if relevant, use more advanced Haskell techniques


### Install

    1. Download source code
    2. Compile with "ghc src/yascc.hs"
    3. ln -s src/yascc ./yascc
    4. sudo mv yascc /usr/local/bin

### Use

If you want the output to be **file: # of loc** just use:
    
    yascc <files/directory>

To output the number of lines of code per language use:

    yascc --total <files/directory>