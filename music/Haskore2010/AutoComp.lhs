This document represents the handin for assignement 2 in the course functional 
programming at LTH, by Bj�rn Lennern�s and Linus Svensson. 
TODO: set up as latex document.
================================================================================

> module AutoComp where
> import Haskore hiding(Major,Minor,Key) -- We want to redefine these types

\section{Introduction}

This document contains a mix of theory, describing the workings of certain 
aspects of western music, and code which describes a way to utilize the
theory to automatically generate accompaniments for musical scores. 

\section{The basics}
First things first, what is a musical score? A musical score is basically
music in printed form, the characteristic sheet of music is a musical score. 
The rules that govern how such scores are interpreted are quite complex 
and only a subset of them will be described and utilized here. For the 
purpose of this document however a musical score consists of notes
and chords. The chords are written above the lines wherein the notes lie and 
consist of, at least in this document, one of the letters C,D,E,F,G,A.

\subsection{Notes}

Which naturally leads to the next topic, what does C,D,E... represent? To
answer that it helps to first answer a more basic question and that is: What
is the relationship between a note and an actual physical sound? It must
somehow relate to the frequency of the sound, since different frequencys are
basically different sounds (although other factors, souch as volume, 
instrument etc. also play a role). 

The way notes works is that they belong to different frequency sections 
called octaves, where octave 1 is the lowest found on pianos. So for a 
note to make sense it needs to be accompanied with its octave, e.g. 
Note $\in$ (C, 4) which denotes a C in the fourth octave. The letters C - A 
then represent different positions within an octave, called semitones,
and each octave is divided into twelve semitones. In this document the
distance between semitones is assumed to be equal.

Another import quality of octaves is that the frequency range they
represent double between each octave, where the first octave has an 
approximate frequenzy range of 30 to 60 hz. The highest octave 
represented on a piano is typically the seventh.  

To reiterate then, C,D,E,.. represent one position on an equidistant 
frequency scale, with twelve positions called semitones. To use that 
informatation to construct a sound we also need an octave, which is 
basically a multiple of octaves. So, given a positon and an octave we
have physical sound frequency, and to construct a sound from that all
we need is a duration, an instrument and a volume. Here the convention
is to call the combination of position in an octave and an octave a 
\emph{pitch}, the position is called a PitchClass in Haskore. The duration
is defined as ratio of whole note (<= 1), and the default value in 
Haskore is 30 whole notes per minute. Thus:

\begin{verbatim}

	>type Pitch = (PitchClass, Octave)
	>type PitchClass = C | D | E..
	>type Note = Pitch -> Dur -> [NoteAttribute] -> Music
	>:t Volume 
	Volume :: Float -> NoteAttribute
	>type Dur = Ration Int
	>:t wn 
	>wn :: Dur
	>:t (Note (G,4) wn [Volume 60])
	> (Note (G,4) wn [Volume 60]) :: Music

\end{verbatim}



\section{Keys and chords}
So how do we interpret a note sheet? An important concept is that of note 
supplies. As a rule only a subset of the pitchclasses are used in a song,
this subset is called the note supply for the piece and it is from these
pitchclasses that the Pitches in a melody are formed. To construct the 
note supple for a piece its key is needed, which is an object with two
properties - a root, which is a pitchclass, and a harmonic quality, which 
is either major or minor. Thus (C,Major) is a Key.

In this document this key is used in two ways to construct the note supply. 
First its harmonic quality is used to choose a pattern, basically positions
in an octave, and its root is used to align the pattern so that the note
supply starts at this section of the octave. There are several different
patterns to choose from for both major and minor, in this assignement
only the most common for major and minor are used and they are called
ionian and aeolian respectively (see below). Furthermore the note supply
also needs an octave to represent a set of unique pitches, in this assignement
the melody is played with this octave set to four.

So given a note supply pitches for the notes on the note sheet are constructed
from this supply by looking at their position in the sheet. The note that
starts two steps below the bottom line in the sheet is the first in the note 
supply and every subsequent step is are choosen from the pattern by  


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

> data HarmonicQuality = Major | Minor
>	deriving (Eq)

> type Key = (PitchClass,HarmonicQuality)

================================================================================

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
>	| quality == Major = chooseScalePatternMajor
>	| quality == Minor = chooseScalePatternMinor
>	where
>		chooseScalePatternMajor pos
>			| pos == 0 = ionian
>			| pos == 1 = mixolydian
>			| pos == 3 = lydian
>			| pos == 4 = mixolydian
>			| pos == 5 = aeolian
> 		chooseScalePatternMinor pos
>			| pos == 1 = dorian
>			| pos == 2 = phrygian

Skapar en notskala baserat på pitklassen (typ C eller D) och i vilken oktav
man vill att skalan ska börja i. Tänkte att man väljer 3 där för bass line.
Sen får man en lista med ABsPitches.

================================================================================

> noteSupply :: Pitch -> HarmonicQuality -> [AbsPitch]
> noteSupply pitch quality 
>	| quality == Major = map ((+) (absPitch pitch)) ionian
>	| quality == Minor = map ((+) (absPitch pitch)) aeolian

Tar reda på positionen för en specifik pitch i en given grundskala.
N�Nmn problem med att vi f�r ut sista index om den inte �terfinns.

> notePosition :: [AbsPitch] -> AbsPitch -> Int
> notePosition scale ab = pos (map (`mod` 12) scale) ab 0

> pos :: Eq a => [a] -> a -> Int -> Int
> pos [] _ i = i 			
> pos (x:xs) xr i 
>	| x == xr = i
>	| x /= xr = pos xs xr (i+1)

================================================================================

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

Den anropar bassLine som flätar ihop style och chordprogression och tillverkar grundskalan.

> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass style key prog = 
>	toMusic (bassLine (cycle style) quality (noteSupply ((fst key),bassOct) quality) prog) bassVol (:+:)
>		where quality = snd key

Flätar ihop bass style och chordprogression en duration i taget (lite influense av åkessons variant)

> bassLine :: BassStyle -> HarmonicQuality -> [AbsPitch] -> ChordProgression -> [(AbsPitch,Dur)]
> bassLine _ _ _ [] = []
> bassLine ((st,sdur):sts) quality noteSupp ((ch,cdur):chs)
>	| sdur == 0 = bassLine sts quality noteSupp ((ch,cdur):chs)
>	| cdur == 0 = bassLine ((st,sdur):sts) quality noteSupp chs
>	| otherwise = play st dur : 
>		bassLine ((st,sdur-dur):sts) quality noteSupp ((ch,cdur-dur):chs)
>	where 
>		dur = min sdur cdur
>		play st dur
>			| st == -1 = (silence,dur)
>			| otherwise = ((absPitch (ch,bassOct)) + ((chooseScalePattern quality pos ) !! st),dur)
>				where pos = notePosition noteSupp (absPitch (ch,0))


> toMusic :: [(AbsPitch,Dur)] -> [NoteAttribute] -> (Music -> Music -> Music) -> Music
> toMusic pitches vol f =
>	foldr1 f (map toNote pitches)
>	where toNote (p,dur)
>		| p == silence	= Rest dur
>		| otherwise	= Note (pitch p) dur vol


================================================================================

> autoChord :: Key -> ChordProgression -> Music
> autoChord key chords = genChord key chords initial

> genChord :: Key -> ChordProgression -> [(AbsPitch,Dur)] -> Music
> genChord key [] _ = Rest 0 
> genChord key (ch:chs) prev = (toMusic minimal chordVol (:=:)):+:(genChord key chs minimal)
> 	where minimal = minimize key ch prev

> minimize :: Key -> (PitchClass,Dur) -> [(AbsPitch,Dur)] -> [(AbsPitch,Dur)]
> minimize key cur prev = candidates !! (pos (score candidates prev) (minimum (score candidates prev)) 0)
>	where 	candidates = genCandidates key cur 

> score :: [[(AbsPitch,Dur)]] -> [(AbsPitch,dur)] -> [Int]
> score candidates prev = zipWith (+) (map (1*) (in_d cur)) (map (2*) (ex_d cur (map (fst) prev)))
>	where 	in_d pts  = [(maximum pt) - (minimum pt) |pt <- pts ]
>		cur = map (map (fst)) candidates
>		ex_d pts pre = [sum (map abs (zipWith (-) pt pre) ) | pt<-pts ]

> genCandidates :: Key -> (PitchClass,Dur) -> [[(AbsPitch,Dur)]]
> genCandidates key (pclass,dur) =  genValidsWithDur [genValids (map (`mod`12) (map (pattern !!) inv))| inv <- inversions] dur
>	where
>		pattern = map ((+) (absPitch (pclass,0) )) (chooseScalePattern (snd key) (notePosition noteSupp (absPitch (pclass,0) )))
>			where
>				noteSupp = noteSupply ((fst key),0) (snd key)

> genValidsWithDur :: [[[AbsPitch]]] -> Dur -> [[(AbsPitch,Dur)]]
> genValidsWithDur pts d = [zip pt durs | pt <-(concat pts)]
> 	where durs = [d,d,d]

> genValids :: [AbsPitch] -> [[AbsPitch]]
> genValids triad = [val | val <-permut, all (<= u_bound) val, all (>= l_bound) val ]
>	where 	u_bound = absPitch (G,5) 
>		l_bound = absPitch (E,4)
>		permut  = [[i,j,k] | i<-(map (+(triad !! 0)) o45),j<-(map (+(triad !! 1)) o45),k<-(map (+(triad !! 2))o45)]
>			where o45 = [48,60]

> initial = [(55,wn),(59,wn),(67,wn)] :: [(AbsPitch, Dur)] 

> chordVol = [Volume 35]

> inversions = [[0,2,4],[2,4,0],[4,0,2],[2,4,0],[4,0,2],[4,2,0]]


bassLine genererar en lista med absolut pitchar och durations som görs till noter här.
Om vi vill göra ackord istället för enskilda noter bör det gå att göra här.

================================================================================

> autoComp :: BassStyle -> Key -> ChordProgression -> Music
> autoComp style key chords =
>	(Instr "Acoustic Bass" bass) :=: (Instr "Church Organ" chord_v)
>	where 	bass = autoBass style key chords
>		chord_v = autoChord key chords









