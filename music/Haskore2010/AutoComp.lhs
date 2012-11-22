This document represents the handin for assignement 2 in the course functional 
programming at LTH, by Björn Lennernäs and Linus Svensson. 
TODO: set up as latex document.
================================================================================

> module AutoComp where
> import Haskore

\section{Introduction}

This document contains a mix of theory, describing the workings of certain 
aspects of western music, and code which describes a way to utilize the
theory to automatically generate accompaniments for musical scores. 
TODO: Add disclaimer?? (this represents a halting comprehension etc..
also quite simplistic.) 

\section{The basics}
First things first, what is a musical score? A musical score is basically
music in printed form, the characteristic sheet of music is a musical score. 
The rules that govern how such scores are interpreted are quite complex 
and only a subset of them will be described and utilized here. For the 
purpose of this document however a musical score consists of notes, e.g. ♪,
and chords. The chords are written above the lines wherein the notes lie and 
consist of, at least in this document, one of the letters C,D,E,F,G,A.

TODO: Perhaps not use this line?
Which naturally leads to the next topic, what does C,D,E... represent? To
answer that it helps to first answer a more basic question and that is: What
is the relationship between a note and an actual physical sound? It must
somehow relate to the frequency of the sound, since different frequencys is
basically different sounds (although other factors, souch as volume, 
instrument etc. also play a role). The way notes works is that they belong to
different frequencys sections called octaves, numbered from from 1 and
upwards. So for a note to make sense it needs to be accompanied with its 
octave, e.g. Note = (C, 4) which denotes a C in the fourth octave. The 
letters C - A represent positions within an octave, and each octave is 
divided into twelve equidistant sections. 
TODO: Fix..

An import quality of octaves is that the frequency range they
represent double between each octave, where the first octave has a range
of frequenzy in approximately the range 30 to 60 hz. The highest octave
represented on a piano is typically the seventh.  

TODO: Add: Note need a duration. Pitchclasses. 

TODO: In this section define (in code) stuff like notes, octaves etc (if we need
them). I'm guessing that we need to represent notes as positions on in a sheet, 
from 0 and up. And given these positions and the note supply, below, we determine 
what pitchclass and octave that we assign the notes. 


\section{Keys and chords}
What is a key? Root + harmonic (C Major). 
Harmonic gives pattern - how to pick a note scale from the octave.
Different patterns

Chords belong to chord class. Chords class similar to key: root, harmonic, chord 
pattern (used to build chord, e.g. basic triad), chord scale - given by a 
function applied on the key of the song and the choords root and pattern 
tabular. 

TODO: Define keys and how to pick not supply from keys. How to 


================================================================================

This is the definition of a ChordProgression

> type ChordProgression = [(PitchClass,Dur)]

\section{Scale Patterns}
================================================================================

Definierar två variabler major o minor för harmonic quality för enkelhet

> type HarmonicQuality = Int;

> major, minor :: HarmonicQuality
> major = 1
> minor = -1

En listning av olika scalepatterns

> type ScalePattern = [Int]

> ionian, lydian, mixolydian, aeolian, dorian, phrygian :: ScalePattern
> ionian	= [0, 2, 4, 5, 7 ,9, 11]
> lydian	= [0, 2, 4, 6, 7, 9 ,11]
> mixolydian	= [0, 2, 4, 5, 7, 9, 10]
> aeolian	= [0, 2, 3, 5, 7, 8, 10]
> dorian	= [0, 2, 3, 5 ,7, 9, 10]
> phrygian	= [0, 1, 3, 5, 7, 8, 10]

Väljer ett scalepattern baserat på om man har major eller minor och vilken
position den aktuella noten har i grundskalan (typ C major)

> chooseScalePattern :: HarmonicQuality -> Int -> ScalePattern
> chooseScalePattern quality
>	| quality == major = chooseScalePatternMajor
>	| quality == minor = chooseScalePatternMinor

> -- Helper functions for chooseScalePattern
> chooseScalePatternMajor pos
>	| pos == 0 = ionian
>	| pos == 1 = mixolydian
>	| pos == 3 = lydian
>	| pos == 4 = mixolydian
>	| pos == 5 = aeolian
> chooseScalePatternMinor pos
>	| pos == 1 = dorian
>	| pos == 2 = phrygian

Skapar en notskala baserat på pitklassen (typ C eller D) och i vilken oktav
man vill att skalan ska börja i. Tänkte att man väljer 3 där för bass line.
Sen får man en lista med ABsPitches.

> noteSupply :: PitchClass -> Octave -> HarmonicQuality -> [AbsPitch]
> noteSupply pclass oct quality 
>	| quality == major = map ((+) (absPitch (pclass,oct))) ionian			-- <-- use .
>	| quality == minor = map ((+) (absPitch (pclass,oct))) aeolian			-- <-- use .

Tar reda på positionen för en specifik pitch i en given grundskala.

> notePosition :: [AbsPitch] -> Pitch -> Int
> notePosition scale pitch = 								-- DÅLIG OCH FUL KOD
>	[index | (index, e) <- zip [0..] scale, e == absPitch pitch ] !! 0

\section{Bass Lines}

Some properties for the bass line.

> bassVol = [Volume 60]
> bassOct = 3 -- the base octave in the bass line

De olika bass stylesen definierade.

> type BassStyle = [(Int,Dur)]

> silence = -1

> basic, calypso, boogie :: BassStyle
> basic = [(0,hn),(4,hn)]
> calypso = [(silence,qn),(0,en),(2,en),(silence,qn),(0,en),(2,en)]
> boogie = [(0,en),(4,en),(5,en),(4,en),
>	(0,en),(4,en),(5,en),(4,en)]


================================================================================

Nedanstående kod är under utveckling. 

> autoBass :: BassStyle -> (PitchClass, HarmonicQuality) -> ChordProgression -> Music
> autoBass style (pclass,quality) cprog = 
>	toMusic (bassLine (cycle style) quality (noteSupply pclass bassOct quality) cprog)

> bassLine :: BassStyle -> HarmonicQuality -> [AbsPitch] -> ChordProgression -> [(AbsPitch,Dur)]
> bassLine _ _ _ [] = []
 
 > bassLine _ _ _ = [((absPitch (C,3)),wn),(-1,wn),((absPitch (D,4)),wn)]

> bassLine ((st,sdur):sts) quality noteSupp ((ch,cdur):chs)
>	| sdur == 0 = bassLine sts quality noteSupp ((ch,cdur):chs)
>	| cdur == 0 = bassLine ((st,sdur):sts) quality noteSupp chs
>	| otherwise = play st dur : 
>		bassLine ((st,sdur-dur):sts) quality noteSupp ((ch,cdur-dur):chs)
>	where 
>		dur = min sdur cdur
>		play st dur
>			| st == -1 = (-1,dur)
>			| otherwise =
>				(map ((+) (absPitch (ch,bassOct)))((chooseScalePattern quality (notePosition noteSupp (ch,bassOct)))) !! 
>					st,dur)

> toMusic :: [(AbsPitch,Dur)] -> Music
> toMusic pitches =
>	foldr1 (:+:) (map toNote pitches)
>	where toNote (p,dur)
>		| p == silence	= Rest dur
>		| otherwise	= Note (pitch p) dur bassVol














