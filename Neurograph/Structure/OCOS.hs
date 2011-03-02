module Neurograph.Structure.OCOS where

import Neurograph.Brain
import Neurograph.Node
import Neurograph.Connect

{-|
	The prototypical OCOS (On-Center/Off-Surround) structure. We create eight
	nodes and provide their links by hand. Since OCOS is a graph-level structure,
	a 'Mask' of the type (*,*,*,0) should be provided to specify upper-level
	containment.

	By default, all the nodes start with a value of zero. These can be
	manipulated using the 'poke' and 'stimulate' functions. The local node
	labelling here is consistent with the labelling suggested in diagrams
	of OCOS in /Bioinformation Processing/.
-}
str_OCOS :: Mask -> [Node]
str_OCOS mask =
	let
		p = position -- For convenience

		n1 = Node (maskPosition mask (0,0,0,1))
			[p n4          ] [                   ] True  0

		n2 = Node (maskPosition mask (0,0,0,2))
			[p n5,p n7,p n8] [                   ] True  0

		n3 = Node (maskPosition mask (0,0,0,3))
			[p n6          ] [                   ] True  0

		n4 = Node (maskPosition mask (0,0,0,4))
			[p n7          ] [p n1               ] False 0

		n5 = Node (maskPosition mask (0,0,0,5))
			[p n7          ] [p n2               ] False 0

		n6 = Node (maskPosition mask (0,0,0,6))
			[p n7          ] [p n3               ] False 0

		n7 = Node (maskPosition mask (0,0,0,7))
			[p n8          ] [p n2,p n4,p n5,p n6] True  0

		n8 = Node (maskPosition mask (0,0,0,8))
			[              ] [p n2,p n7          ] True  0
	in
		map (maskNode mask) [n1,n2,n3,n4,n5,n6,n7,n8]
