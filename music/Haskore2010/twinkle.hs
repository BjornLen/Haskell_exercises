module Twinkle where
import AutoComp 
import Haskore hiding (Major,Minor)
 
 -- note updaters for mappings
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
lmap f l = line (map f l)

-- repeat something n times
times  1    m = m
times n m = m :+: (times (n - 1) m)

-- Defining the twinkle melody --
-- Line 1 in twinkle
twinkleMelody :: Music
oct = 5
p11 = lmap (fd qn) [c oct, c oct, g oct, g oct, a oct, a oct ]
p12 = lmap (fd hn) [g oct]
p13 = lmap (fd qn) [f oct, f oct, e oct, e oct, d oct, d oct]
p14 = lmap (fd hn) [c oct]
l1 = p11 :+: p12 :+: p13 :+: p14

-- Line 2 in twinkle
p21 = lmap (fd qn) [g oct, g oct, f oct, f oct, e oct, e oct ]
p22 = lmap (fd hn) [d oct]
l2 = p21 :+: p22 :+: p21 :+: p22

-- Line 3 in twinkle
l3 = l1

twinkleMelody = Instr "piano" (l1 :+: l2 :+: l3)

-- Defining the chordProgression of twinkle
twinkleChords :: ChordProgression
twinkleChords = [(C,wn),(F,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),
	(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),(G,hn),
	(C,wn),(F,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn)]

twinkleBass :: (PitchClass,HarmonicQuality) -> Music
twinkleBass key = Tempo 3 (autoBass boogie key twinkleChords)
chord_voicing = Tempo 5 (autoChord (C,Major) twinkleChords)

twinkle = Tempo 3 (twinkleMelody :=: autoComp calypso (F,Major) twinkleChords)
