module Twinkle where
import AutoComp
import Haskore
 
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
oct = 7 
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

twinkleMelody = l1 :+: l2 :+: l3

-- Defining the chordProgression of twinkle
twinkleChords :: ChordProgression
twinkleChords = [(C,wn),(F,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),
	(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),(G,hn),
	(C,wn),(F,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn)]

bbc = lmap (fd en) [c 3, g 3, a 3, g 3]
bbf = lmap (fd en) [f 3, c 4, d 4, c 4]
bbg = lmap (fd en) [g 3, d 4, e 4, d 4]

-- Bassline boogie bass twinkle, one var for each line
bl1 = foldr1 (:+:) [bbc, bbc, bbf, bbc, bbg, bbc, bbg, bbc]
bl2 = foldr1 (:+:) [bbc, bbg, bbc, bbg, bbc, bbg, bbc, bbg]

twinkleBass = autoBass boogie (C, major) twinkleChords

twinkle = Tempo 2 ((Instr "piano" twinkleMelody) :=: (Instr "Acoustic Bass" (bl1:+:bl2:+:bl1)))

cMajor = foldr1 (:=:) [ Note (x, 4) hn [Volume 80] | x <-[C, E, G] ]

testBajs = Tempo 3 (cMajor :+: Rest wn :+: (Note (C, 4) hn [Volume 80]))
