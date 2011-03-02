module Neurograph.Structure.Hinge where

import Neurograph.Node
import Neurograph.Brain
import Neurograph.Time
import Neurograph.Connect

str_Hinge :: Mask -> [Node]
str_Hinge mask =
	let
		p = position

		n1 = Node (maskPosition mask (0,0,0,1))
			[		]	[p n2,p n4	    ]	True	0
		
		n2 = Node (maskPosition mask (0,0,0,2))
			[p n1           ]	[p n3,p n4          ]	True	0
		
		n3 = Node (maskPosition mask (0,0,0,3))
			[p n2           ]	[p n4,p n5          ]	False	0
		
		n4 = Node (maskPosition mask (0,0,0,4))
			[p n1,p n2,p n3 ]	[p n5               ]	True	0
		
		n5 = Node (maskPosition mask (0,0,0,5))
			[p n3,p n4      ]	[p n6               ]	True	0
			
		n6 = Node (maskPosition mask (0,0,0,6))
			[p n5           ]       [                   ]   True	0
	in
		map (maskNode mask) [n1,n2,n3,n4,n5,n6]
