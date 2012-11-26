module JingleBells where
import AutoComp 
import Haskore hiding (Major,Minor)
 
 -- note updaters for mappings
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
lmap f l = line (map f l)

-- Defining the melody for imperial march
jingleMelody :: Music
oct = 4
-- line 1
p11 = 	lmap (fd qn) [e oct, e oct] :+:
	lmap (fd hn) [e oct]
p13 =	lmap (fd qn) [e oct, g oct, c oct, d oct]
p14 =	lmap (fd hn) [e oct] :+:
	Rest hn
l1 = p11 :+: p11 :+: p13 :+: p14

-- line 2
p21 =	lmap (fd qn) [f oct, f oct, f oct, f oct]
p22 =	lmap (fd qn) [f oct, e oct, e oct, e oct]
p23 =	lmap (fd qn) [e oct, d oct, d oct, e oct]
p24 =	lmap (fd hn) [d oct, g oct]
l2 = p21 :+: p22 :+: p23 :+: p24

-- line 3
l3 = l1

-- line 4
p41 =	p21
p42 =	p22
p43 =	lmap (fd qn) [g oct, g oct, f oct, d oct]
p44 =	c oct hn v :+: Rest hn
l4 = p41 :+: p42 :+: p43 :+: p44

jingleMelody = Instr "piano" (l1 :+: l2 :+: l3 :+: l4)

-- Defining the chordProgression of twinkle
jingleChords :: ChordProgression
jingleChords = [(C,wn),(C,wn),(C,wn),(C,wn),
	(F,wn),(C,wn),(G,wn),(C,wn),
	(C,wn),(C,wn),(C,wn),(C,wn),
	(F,wn),(C,wn),(G,wn),(C,wn)]

jingle = Tempo 3 (jingleMelody :=: autoComp boogie (C,Major) jingleChords)
