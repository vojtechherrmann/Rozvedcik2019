# Rozvedcik2019 - Draw

Takes two csv files in folder data:
- guests: list of guests and their internal category names (columns: Name, CategoryInternalName)
- teams: NxP list of teams composition (columns: TeamId, CategoryId1, CategoryId2, ..., CategoryIdP)

Both csv files must be comma separated, in UTF-8 encoding and with headers.

In the main.R script user can define three arguments:
- nr_redundant_symbols (int) - each players card will match exactly two other players with exactly one symbols (2 symbols on one card in total). This parameter says how many other symbols each player will have on his card.
- black_n_white (bool) - for testing on black and white printers a number is added to each symbol representing its color
- seed (int) (for replicating same random results in the future)

Running all parts of main.R loads data, calculates stuff and write output to the pics folder. In addition outputs.RData is saved with three administratively usefull datasets.
The output is one card for each player (with external category name above). The number the file name contains is random.

All of the other parameters (colors, symbols etc.) have to be modified inside the function.R script, which is not going to be documented other way then in-code comments.

There are 30 symbols and 8 colors prepared for usage. Two notes on that:
- if number of players exceeds 30 or number of teams exceeds 8, the script will run out of options when generating symbols (search symbols (colors) in the function.R script)
- even if the criteria are met, one can have unprinted some symbols in the cards due to system and environment settings