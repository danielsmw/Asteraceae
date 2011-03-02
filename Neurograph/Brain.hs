module Neurograph.Brain where

import Neurograph.Node
import Data.List

{-|
   A Brain is just a synonym for ['Node'], but useful syntactically
   to differentiate between a reference to a subgraph of the system
   or the entire system.
-}
type Brain = [Node]

{-|
   Given the positions of two nodes and a reference brain,
   modify the brain to reflect the connection of the first
   node to the second node.

   Connect specifically prohibits first-level feedback,
   but for no good reason other than that we wanted to.
-}
connect :: Position -> Position -> Brain -> Brain
connect x y b =
	let
		nx = nodeLookup b x
		ny = nodeLookup b y
		brain  = delete nx $ delete ny b
	in
		case nx==ny of
			False -> (goto nx ny):(receive ny nx):brain
			True -> error "No self-connections, please!"

{-|
   Given a 'Brain' and a 'Mask', extract the nodes that show
   through the mask; e.g. 
   
   @
   	extract myBrain (1,1,0,0)
   @

   Returns a list of all the nodes in module 1, can 1; i.e. those
   whose positions are prefixed by (1,1).
-}
extract :: Brain -> Mask -> [Node]
extract brain mask = 
	let 
		pred (m,a,s,k) (p,o,z,n) =
			( m == p || m == 0 ) &&
			( a == o || a == 0 ) &&
			( s == z || s == 0 ) &&
			( k == n || k == 0 )
	in
		-- Todo: change this filter to a mapped hashtable 
		-- lookup when that's implemented.
		filter (\n -> pred mask (position n)) brain
