Indirect Assassin
=================

> What was that? Did I just assassinate that guy? No, that can't be... I don't
> kill. Must have been his own fault.

You're a dodgy assassin (though you don't call yourself that) who never
*directly* kills people, but who's ok with *indirectly* doing so. In fact,
you're more than ok with it; it's your job.

Under your secret identity as "The Agent", you do jobs where the task is to
eliminate professors in dungeons. You care deeply about your contractor's wish
for silence regarding these jobs, so you don't know why these dungeon-examining
professors must die. And you're always surprised when they do, because you
never killed one...


Authors
-------

See the file AUTHORS.txt


Copyright
---------

See the file COPYRIGHT.txt


Version
-------

This is version 0.1.0 of Indirect Assassin.


Online
------

Indirect Assassin has a web page at
http://metanohi.name/projects/indirectassassin/ which always contains the
newest version of the program. Indirect Assassin is also on Haskell's Hackage;
see http://


Installation
------------

Indirect Assassin is written in Haskell. It should work with not-too-old
versions of the Glasgow Haskell Compiler (GHC). Indirect Assassin also uses the
Haskell SDL library.

Indirect Assassin has only been tested on GNU/Linux, but it should also run on
other systems such as MS Windows or Apple Mac.

You can download GHC and the Haskell Platform from
http://hackage.haskell.org/platform/ --- once you have downloaded and installed
those, you can use a newly installed program, "cabal", to download and install
Haskell SDL bindings:

    cabal install sdl sdl-image sdl-mixer sdl-ttf missingh

Once this is done, run (from the root directory of the extracted tarball):

    runhaskell Setup.lhs configure
    runhaskell Setup.lhs build
    runhaskell Setup.lhs install

and the game should be installed.


### With Cabal

Note that, if you have installed the Haskell Platform prior to downloading this
program, you can just run

    cabal install indirectassassin
    
to both download and install the newest version of this game and its SDL
dependencies. Note that you'll probably still want to keep a copy of this
README as it's essentially the documentation of the game.


How to play
-----------

### Step 0: Start it

To start the game, just run:

    indirectassassin

from a terminal. However, this will only let you play on the built-in dungeon
maps. To specify custom maps, run:

    indirectassassin map_0 map_1 ... map_n

Custom maps will be prepended to the list of built-in maps.

Indirect Assassin does not have a menu (programming a menu is boring, and using
the command-line is easier (this is, by the way, completely objective)).

At this time, the resolution is hardcoded at 768x576, i.e. 12x9 64x64 tiles,
and fullscreen is not possible. This is mainly due to slacking on the author's
part.


### Step 1: Play

The gameplay of Indirect Assassin is simple, but it does require learning
several rules and items, as well as the memorisation of a few keys on your
keyboard. On the plus side (again, completely objective), your mouse (if you
have any) is useless in this game.

Genre-wise, Indirect Assassin is probably closest to stealth games; it's a
turn-based stealth game.

Indirect Assassin consists of maps; there are a few built-in ones, and it's
easy to create your own.


#### Gameplay

You, The Agent, can see in the dark through the use of your high-tech
sunglasses, but the professors cannot see that you can see in the dark. Your
vision is limited to a few tiles forward, and not backward or to any
sides. Professors use flashlights to see in the dark. They too can only see a
few tiles ahead and not backwards or to the sides. You can see the light from
their flashlights. If you get covered in light so much that a professor can see
you, he kills you instantly, and you lose. You can see the light one tile
before the professor with the light source can see you, which might give you
time to escape it.

The tiles which are not visible due to either your night vision or the
professors' flashlights, are completely black, unless you have entered cheat
mode (aka debug mode) where all tiles are visible to you (but still not to the
professors).

When you start the game, you play on the first map from the list of maps. At
any time you can shift to another map (since the game is turn-based, you can
actually have games on many maps at the same time). You can also play the same
map at the same time across several frames.

When you win or lose on a map, you receive a message on the screen stating your
win/lose state. Accepting that state lets you play on that map again.


#### How to win

If you manage to kill (indirectly, of course) all the professors on a map, you
win. To indirectly kill a professor, you must pick up dangerous items and place
them so that the professors stumble upon them, and remove good items, so that
professors don't stumble over them. There are several types of items, and they
affect the professors in different ways.

An important part of playing on any map is thus to figure out how many
professors exist and which items are available. Unless the cheat mode is used,
this process requires luck.


#### Items

There are 7 items:

* Barrels
* Buckets
* Yellow bat
* Green bee
* Diamond
* Tomato
* Ice shield

Note that the game does not remind you which items you currently possess;
you'll have to remember that yourself.


#### Maps

Indirect Assassin comes with HOWMANY built-in maps. All maps are files in a
simple textual format. The included maps can be found in
datainstallationdir/maps/ (or, if you downloaded the tarball, in data/maps/).

##### How to create your own maps

It might be useful to look at the built-in maps for inspiration for creating
your own maps.

A small map might look like this:

    #####
    #!b #
    #  x#
    #####

[EXPLAIN]


#### Keys to remember

* Move <direction> if possible: <direction> arrow key
* Rotate to look in <direction>: Shift+<direction> arrow key
* Toggle cheat mode: Control+X
* Accept message: Enter
* Go to next map: Control+Right arrow key
* Go to previous map: Control+Left arrow key
* Play current map in a next, different frame: Control+Up arrow key
* Play current map in a previous, different frame: Control+Up arrow key
* Drop item in front of you: [Item key]
* Exit game: ESCAPE


Liberated Pixel Cup
-------------------

Indirect Assassin is an entry in the Liberated Pixel Cup 2012, a free software
and free culture contest. See http://lpc.opengameart.org/ for more information.


This document
-------------

To the extent possible under law, Niels G. W. Serup has waived all copyright
and related or neighboring rights to this document; see the Creative Commons
Zero 1.0 document for more information.
