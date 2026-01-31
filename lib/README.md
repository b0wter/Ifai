

Parser
------
Each game mode has its own parser to make sure that it acts independently.
This way intent does not leak out of the game mode into other modes or anything global.

Parsers in the modules have the signature `string -> Intent option`.
In case they are not able to understn