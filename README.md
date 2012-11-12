;;; "cl-puyopuyo" goes here. Hacks and glory await!


;;;;https://github.com/o-simon/ecl-smartphones
;;;;http://lists.common-lisp.net/pipermail/lisp-game-dev/2011-July/000153.html

;;;; puyopuyo.lisp
;;;; puyopuyo clone in common lisp
;;;;with SDL(lispbuilder) for gfx and input, and (maybe) sound

;;;;MIT License 
;;;;Author: Anes Lihovac: anes DOT lihovac AT googlemail DOT com

;;this is a "I want to learn Lisp" project, and to use slime and emacs
;;don't be dissapointed to find non-lispy things here
;;cause this is my first lisp project

;;everything tested with SBCL 1.0.55.0.debian + quicklisp


;;;;field width is 6 stones
;;;;field height is 12 stones

;;;;it kinda works like this:
;;;;in memory we hold an 6x12 array which is filled default with -1
;;;;meaning that all cells are empty and can be filled with a stone
;;;;in the game-loop we will

;;;;spwan a new random (col 0-3, blue=0, red=1, green=2, yellow=3) stone at 
;;;;position 0,3 and 0,4
;;;;if cells are -1 we can draw and proceed
;;;;if cells bellow our pos are -1, we can drop
;;;;if we hit anything, we start a backtrack to find if the cell
;;;;above, beneath, or left/right from us is the same color,
;;;;and if the summary of those conencted color cells is >=4 we can
;;;;erase these cells, meaning this could cause an avalanche effect
;;;;freeing up more and more cells

;;;;code wise we will
;;;;init sdl
;;;;init the playfiled -> array
;;;;load the stones
;;:;make a timed thread which will be triggered each second
;;;;to drop the stones
o


To start via slime: (asdf:load-system "cl-puyopuyo")