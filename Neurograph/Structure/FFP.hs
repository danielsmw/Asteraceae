module Neurograph.Structure.FFP where

import Neurograph.Time
import Neurograph.Node
import Neurograph.Brain
import Neurograph.Connect

import Neurograph.Structure.OCOS

{-|
	The prototypical FFP (Folded Feedback Pathway) structure. We create an OCOS
	and then connect 3 new nodes to it which consitute the feedback pathway. Note
	that FFP is, like OCOS, a graph-level structure, and so a 'Mask' of the type
	(*,*,*,0) should be provided to specify upper-level containment.

	By default, all the nodes start with a value of zero. These can be
	manipulated using the 'poke' and 'stimulate' functions. The local node
	labelling here is consistent with the labelling suggested in diagrams
	of FFP in /Bioinformation Processing/.
-}
str_FFP :: Mask -> [Node]
str_FFP mask =
	let
		p = position

		[n1,n2,n3,n4,n5,n6,n7,n8] = str_OCOS mask
		n9  = Node (maskPosition mask (0,0,0,9 )) [p n10] [     ] True 0
		n10 = Node (maskPosition mask (0,0,0,10)) [p n11] [p n9 ] True 0
		n11 = Node (maskPosition mask (0,0,0,11)) [     ] [p n10] True 0
	in
		map (maskNode mask) $ connect (p n9) (p n7)
			[n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11]
