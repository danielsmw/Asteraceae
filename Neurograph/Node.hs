{-|
   This module implements the basic 'Node' data type and node-level functions.
   Functions are implemented for updating nodal connections, updating node
   properties individually, and performing node lookups from lists. [NOTE:
   this will need to be updated upon reimplementing Brain as a hashtable].

   The Position and Mask type synonyms, while only syntactic sugar, are
   introduced to keep track of the functional significance of the various
   four-tuples passed around the program. 
-}

module Neurograph.Node where

import Data.List
import Data.Maybe

{-|
   The Position type synonym uniquely identifies a particular node in
   state-space. The left-most number represents the local, subgraph-level
   numbering of the node; the next number, the graph's numbering in its
   cortical can; et cetera.

   Note that changing this to any size n-tuple will not actually affect
   any of the underlying "Neurograph" code, because positions should
   generally be referenced as the 'Position' data type rather than
   (Integer, Integer, Integer, Integer), and so changes here will be
   automatically reflected. The only places these changes will need to
   be made by hand are in hand-built structures like 'str_OCOS',
   'str_FFP', et cetera. This same directive applies to changing the size
   of 'Mask' (make sure you update 'Mask' if you update 'Position'!).
-}
type Position = (Integer,Integer,Integer,Integer)

{-|
   The Mask type synonym is, in fact, formally identical to 'Position';
   the implementation of a Mask, however, is found in the initialization
   of nodes in larger structures. Like an IP subnet, it effectively
   identifies more general information while allowing specific information
   to be handled by the client, i.e. 192.168.0.0/16. (In fact, an IP
   subnet mask acts the other way, i.e. 255.255.255.0 hides everything
   but the lower-level numbering... but the word Mask is super convenient,
   and the idea is similar.)
-}
type Mask = (Integer,Integer,Integer,Integer)

-- | The null mask is just (0,0,0,0).
nullMask :: Mask
nullMask = (0,0,0,0)

{-|
   The Node type is the singular ingredient which, replicated,
   forms a graph. It has a four-tuple 'Position', lists positions 
   of nodes in and nodes out, a boolean for excitory/inhibitory,
-} 
data Node = Node { position :: Position , nodesIn :: [Position] ,
		   nodesOut :: [Position] , excitory :: Bool , value :: Int }
		   deriving (Eq)

{-|
   We like storing excitoriness as a Bool, but in practice a 
   +/- 1 is more useful.
-}
sign :: Node -> Int
sign n = case (excitory n) of
		True -> 1
		False -> (-1)

-- | Update a node's in list with a new node
receive :: Node -> Node -> Node
receive n p = Node (position n) ((position p) : nodesIn n) (nodesOut n)
			(excitory n) (value n)

-- | Update a node's out list with a new node
goto :: Node -> Node -> Node
goto n p = Node (position n) (nodesIn n) ((position p) : nodesOut n)
			(excitory n) (value n)

-- | Update a node's in list with the removal of a node
shun :: Node -> Node -> Node
shun n p = Node (position n) (delete (position p) $ nodesIn n) (nodesOut n)
			(excitory n) (value n)

-- | Update a node's out list with the removal of a node
retreat :: Node -> Node -> Node
retreat n p = Node (position n) (nodesIn n) (delete (position p) $ nodesOut n)
			(excitory n) (value n)

{-|
   Given a collection of nodes, search by position and return
   the appropriate node. Error out if we can't find it.

   TODO: When the "Brain" type is reimplemented as a hashtable,
   that module will implement a constant-time lookup function.
   Then this linear-time lookup will be effectively deprecated.
-}
nodeLookup :: [Node] -> Position -> Node
nodeLookup brain pos = 
	let node = find (\n -> position n == pos) brain
	in case node of
		Nothing -> error "nodeLookup: Node wasn't in brain!"
		_ -> fromJust node

{-|
   Increment the value of a node at some position in the brain
   by one. This is just a special case of 'stimulate'
-}
poke :: [Node] -> Position -> [Node]
poke = stimulate 1

-- | Set the value of a node at a given position in a brain.
stimulate :: Int -> [Node] -> Position -> [Node]
stimulate s b pos =
	let
		node = nodeLookup b pos
		brain = delete node b
	in
		(Node (position node) (nodesIn node)
			(nodesOut node) (excitory node) ((+) s $ value node)):brain

-- | Toggle the excitory/inhibitory nature of a node in at a position in the brain.
toggle :: Position -> [Node] -> [Node]
toggle pos b = 
	let
		node = nodeLookup b pos
		brain = delete node b
	in
		(Node (position node) (nodesIn node)
			(nodesOut node) (not $ excitory node) (value node)):brain

-- | Deletes the node at some position in a brain and cleans up loose ends.
-- | /Warning/: This code is untested.
-- | /DOUBLE WARNING/: This code, in fact, does not work.
kill :: [Node] -> Position -> [Node]
kill b pos =
	let
		node = nodeLookup b pos
		substrate = map (nodeLookup b) $ (nodesIn node) ++ (nodesOut node)
		brain = foldl (\z n -> delete n z) (delete node b) substrate
		newSubstrate =
			(map (\(Node pos ins outs exc val) -> Node pos (delete pos ins) outs exc val) (map (nodeLookup b) $ nodesOut node)) ++
			(map (\(Node pos ins outs exc val) -> Node pos ins (delete pos outs) exc val) (map (nodeLookup b) $ nodesIn  node))
	in
		brain ++ newSubstrate	

--instance Show Node where
--	show n = (show $ position n) ++ "\t| " ++ (show $ value n) ++ "\n"
instance Show Node where
	show n = "\n" ++ "\t\t<--- " ++ (concat $ intersperse " " $ map myshow $ nodesIn n)
		      ++ "\n" ++ (myshow $ position n) ++ "\t ==  " ++ (show $ value n) ++ "\n"
		      ++ "\t\t---> " ++ (concat $ intersperse " " $ map myshow $ nodesOut n) ++ "\n"
		where myshow (a,b,c,d) =   "["++(show a)++"."
					      ++(show b)++"."
					      ++(show c)++"."
					      ++(show d)++"]"

