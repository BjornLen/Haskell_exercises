module Twinklehc where
import Haskore
 
 -- note updaters for mappings
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
lmap f l = line (map f l)
 
 -- repeat something n times
times  1    m = m
times n m = m :+: (times (n - 1) m)

bbc = lmap (fd qn) [c 3, g 3, a 3, g 3]
bbf = lmap (fd qn) [f 3, c 4, d 4, c 4]
bbg = lmap (fd qn) [g 3, d 4, e 4, d 4]

-- Bassline boogie bass twinkle, one var for each line
bl1 = foldr (:+:) [bbc, bbc, bbf, bbc, bbg, bbc, bbg, bbc]
bl2 = foldr (:+:) [bbc, bbg, bbc, bbg, bbc, bbg, bbc, bbg]
bl3 = bl1

-- Line 1 in twinkle
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

twinkle = ( Instr "piano" (Tempo 3 (l1:+:l2:+:l3)) ) :=: (Instr "Acoustic Bass" (Tempo 3 (bl1:+:bl2:+:bl3)) )

