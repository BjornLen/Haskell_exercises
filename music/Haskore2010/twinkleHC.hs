module Twinklehc where
import Haskore
 
 -- note updaters for mappings
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
lmap f l = line (map f l)
 
bbc = lmap (fd en) [c 3, g 3, a 3, g 3]
bbf = lmap (fd en) [f 3, c 4, d 4, c 4]
bbg = lmap (fd en) [g 3, d 4, e 4, d 4]

-- Bassline boogie bass twinkle, one var for each line
bl1 = foldr1 (:+:) [bbc, bbc, bbf, bbc, bbg, bbc, bbg, bbc]
bl2 = foldr1 (:+:) [bbc, bbg, bbc, bbg, bbc, bbg, bbc, bbg]
bl3 = bl1

twinkle = Instr "Acoustic Bass" (Tempo 3 (bl1:+:bl2:+:bl3))

