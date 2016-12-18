I'm trying to teach myself some stuff about interpreter design in Haskell by implementing an interpreter for a simple SQL-like DSL. The DSL is typed and I'm using CSV files instead of a database so this just boils down to reading in CSV files, executing instructions, and then writing out CSV files.

The main point of entry to the project is Spec.hs in the test folder. There are three tests (`selectName`, `selectTable`, & `selectJoin`) that should give you an idea of how the DSL is supposed to function. The DSL itself is implemented inside Relation.hs and Expression.hs in the Select folder. 
