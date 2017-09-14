# Block World Problem

This is an automated solver for reaching the goal state, given an initial state, in the block world. It shows the sequence of operations needed to be applied to the initial configuration to reach the final configuration.

The predicates are:
	
	ontable(X):	X is on the table.
	on(X, Y):	X is on Y.
	clear(X):	The top of X is clear.
	hold(X):	The robot arm is holding X.
	ae:			The robot arm is empty.

The operations are:
	
	stack(X, Y):	Put X on top of Y, given that robot arm was holding X.
	unstack(X, Y):	Remove Y from the top of X and the robot arm will then hold X.
	put_down(X):	The robot arm was holding X and will put it on the table.
	pick_up(X):		The robot arm will pick up X from the table and hold it.

The state is represented as a list of lists, where each list represents one pile/'tower' of blocks.

For example, [[y,x],[w,z,u],[v]] represents the state

	  	u	
	x	z	
	y	w	v

So, for example to go from state

		e
	a 	c 	f
	b 	d 	g

to state

		f
	a	c 	b
	g	d 	e

