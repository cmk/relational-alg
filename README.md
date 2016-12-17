Project idea I got after reading:
http://mfenwick100.blogspot.com/2012/10/a-haskell-library-for-relational-algebra.html

http://stackoverflow.com/questions/23766419/when-would-i-want-to-use-a-free-monad-interpreter-pattern

I'm trying to teach myself some stuff about monadic design in Haskell by implementing an interpreter for a simple SQL-like DSL. The DSL is typed and I'm using CSV files instead of a database so this just boils down to reading in CSV files, executing instructions, and then writing out CSV files.