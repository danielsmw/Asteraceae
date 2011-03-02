{-|
	This module contains information about how to connect upper-level
	structures and basic structure manipulation (like arrow-flipping).

	The 'adjoin' function really should not be implemented outside this
	module, because it can cause unexpected results if you're not thinking
	about the way Haskell manages variables and list data (this has caused
	hours of needless debuging in the past!). If you're unsure, you should 
	probably use the 'posAdjoin' function instead, which connects structures
	by reference to positions, not nodes.

	A few basic conventions are also introduced, such as the adoption of
	the (0,0,*,*) subnet for external \"observation\" structures like
	clocks and external batteries.
-}
module Neurograph.Connect where

import Neurograph.Brain
import Neurograph.Node
import Neurograph.Time
import Data.List

-- TODO: Change these to (0,1,0,0), but it would mean updating the code in
-- our sample implementations... and I just don't feel like it at the moment.
clockSubnet = (0,0,1,0)
batterySubnet = (0,0,2,0)

{-|
	Adjoin structures by specifying their nodes\' 'Position's and connection
	lists along with their reference 'Brain'.

	This function implements (and should supercede) 'adjoin'.
-}
posAdjoin :: [Position] -> [Position] ->
		[(Position,Position)] -> [(Position,Position)] ->
		Brain -> Brain
posAdjoin f g a2b b2a oldBrain = 
	let
		a = map (nodeLookup oldBrain) f
		b = map (nodeLookup oldBrain) g
		graph = a++b
		brain = foldl (flip delete) oldBrain graph
		aTOb  = foldl (\z (n,m) -> connect n m z) graph a2b
		bTOa  = foldl (\z (n,m) -> connect n m z) aTOb  b2a
	in
		bTOa++brain
		

{-|
	Adjoin structures by specifying their 'Node's and connection
	lists along with their reference 'Brain'.

	This function /should not be used liberally/; use
	'posAdjoin' instead.
-}
adjoin :: [Node] -> [Node] ->
		[(Position,Position)] -> [(Position,Position)] ->
		Brain -> Brain
adjoin a b a2b b2a oldBrain = 
	let
		graph = a++b
		brain = foldl (flip delete) oldBrain graph
		aTOb  = foldl (\z (n,m) -> connect n m z) graph a2b
		bTOa  = foldl (\z (n,m) -> connect n m z) aTOb  b2a
	in
		bTOa++brain

{-|
	Apply a 'Mask' to a 'Position' by replacing zero-entries of the
	latter with the former, correspondingly.
-}
maskPosition :: Mask -> Position -> Position
maskPosition (m,a,s,k) (p,o,z,n) = 
	let ch = (\x y -> if (y==0) then x else y)
	in ((ch m p), (ch a o), (ch s z), (ch k n))

-- | Apply a 'Mask' to the 'Position' of a 'Node', and return the new 'Node'.
maskNode :: Mask -> Node -> Node
maskNode mask (Node pos inL outL exci val) = Node (maskPosition mask pos) inL outL exci val

-- | Flip the arrows of a structure by swapping in and out lists of its nodes.
invertArrows :: [Node] -> [Node]
invertArrows = map (\ n@(Node nodes ins outs excit val) -> Node nodes outs ins excit val )
