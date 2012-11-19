This document represents the handin for assignement 2 in the course functional 
programming at LTH, by Björn Lennernäs and Linus Svensson. 

================================================================================

> module AutoComp where
> import Haskore

Music: 

> type BassStyle = [(Int,Dur)]

> silence = -1

> basic, calypso, boogie :: BassStyle
> basic = [(1,hn),(5,hn)]
> calypso = [(silence,qn),(1,en),(3,en),(silence,qn),(1,en),(3,en)]
> boogie = [(1,en),(5,en),(6,en),(5,en),
>	(1,en),(5,en),(6,en),(5,en)]


> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass _ _ _ = id

> type ChordProgression = [[Chord]]
