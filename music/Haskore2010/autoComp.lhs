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

TODO: In this section define (in code) stuff like notes, octaves etc (if we need them). I'm guessing that we need to represent notes as positions on in a sheet, from 0 and up. And given these positions and the note supply, below, we determine what pitchclass and octave that we assign the notes. 


\section{Keys and chords}
What is a key? Root + harmonic (C Major). 
Harmonic gives pattern - how to pick a note scale from the octave.
Different patterns

Chords belong to chord class. Chords class similar to key: root, harmonic, chord pattern (used to build chord, e.g. basic triad), chord scale - given by a function applied on the key of the song and the choords root and pattern tabular. 

TODO: Define keys and how to pick not supply from keys. How to 








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
