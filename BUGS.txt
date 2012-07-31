Bugs and missing features
=========================

These things will hopefully have been fixed in the next release.


Strong flashlights
------------------

Flashlights shine through walls.


Stiff movement
--------------

Not all movement is as fluid as it should be.


Lack of speed
-------------

Efficiency was not my goal with the 0.1.0 release, which should be obvious when
running my game on a not-too-new computer. Since the code needs to be
refactored anyway, making it more efficient could be done at the same time.


Ugly code
---------

I consider it a bug that GameRunner.hs is so ugly. This is my first real
Haskell program, and I found Logic.hs more interesting to work on, meaning I
never got around to fixing GameRunner.hs. For one thing, there's a lot of code
duplication which is not nice. It would also be nice with some actual
commenting of code. My separation of code into modules is also a bit arbitrary
at times. And other things.


Few maps
--------

This game needs more maps. Fortunately, the format is simple, and people can
make their own maps. The included maps must be fun to play. The current maps
have been created without great care.


Winnability not ensured
-----------------------

The maps that come with this version of Indirect Asssassin have not been
checked for winnability. It might be a good idea to develop a tool to check for
winnability.


Lack of sound
-------------

I couldn't get Haskell's SDL-mixer working. I tried on two different machines,
and both gave me a floating point exception when trying to play an .ogg Vorbis
file, though loading it seemed to be no problem. It is possible that it's my
system which has incompatible libraries. Once I get around to updating my
entire system, I'll report it if I still experience it. Somewhat similarly, due
to SDLttf, I always get a segfault when the program exits; this might also have
something to do with my old computer and its non-upgraded distro.
