module Neurograph.Structure.Utilities where

import Neurograph.Brain
import Neurograph.Node
import Neurograph.Time
import Neurograph.Connect

{-|
	The prototypical clock structure consists of three nodes.
	Nodes 1, -1 feed each other so that they are always at a
	value of 1 (combatting time decay of node values), and they
	also each feed Node 0. Thus Node 0 increments by 1 at every
	timestep ( +2 from nodes, -1 from decay ), and so Node 0
	is called the Face Node of the clock.
-}
str_Clock :: Mask -> [Node]
str_Clock mask = 
	let
		p = position

		leftHand  = Node (maskPosition mask (0,0,0,0-1))  [p rightHand]
								  [p rightHand, p face] True 1
		rightHand = Node (maskPosition mask (0,0,0,0+1))  [p leftHand ]
								  [p leftHand,  p face] True 1
		face      = Node (maskPosition mask (0,0,0,0*0))  [p leftHand, p rightHand]
								  [] True 0
	in
		[leftHand,rightHand,face]

{-|
	The prototypical battery structure is identical to a clock
	except that only one node feeds the \"Face\" node. In this
	way, Node 0 maintains a constant value at every time step.

	Note that the default battery has zero charge, i.e. the value
	of Node 0 is 0. To create a battery with a charge, 'stimulate'
	this node (or just use the 'battery' function).

	Further note a battery of no initial charge is not useless;
	in fact, it makes a perfect sink or counter to have nodes
	dump into.
-}
str_Battery :: Mask -> [Node]
str_Battery mask =
	let
		p = position

		leftHand  = Node (maskPosition mask (0,0,0,0-1))  [p rightHand]
								  [p rightHand, p face] True 1
		rightHand = Node (maskPosition mask (0,0,0,0+1))  [p leftHand ]
								  [p leftHand ] True 1
		face      = Node (maskPosition mask (0,0,0,0*0))  [p leftHand ]
								  [] True 0
	in
		[leftHand,rightHand,face]

-- | Create a battery via 'str_Battery' with some initial charge.
battery :: Int -> Mask -> [Node]
battery charge mask = stimulate charge (str_Battery mask) (maskPosition mask (0,0,0,0))
