This document represents the handin for assignement 2 in the course functional 
programming at LTH, by Björn Lennernäs and Linus Svensson. 

================================================================================

> module AutoComp where
> import Haskore

Music: 

> type BassStyle = [(Int,Dur)]

> silence = -1

> basic, calypso, boogie :: BassStyle
> basic = [(0,hn),(4,hn)]
> calypso = [(silence,qn),(0,en),(2,en),(silence,qn),(0,en),(2,en)]
> boogie = [(0,en),(4,en),(5,en),(4,en),
>	(0,en),(4,en),(5,en),(4,en)]


> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass _ _ _ = id

> type ChordProgression = [[Chord]]
