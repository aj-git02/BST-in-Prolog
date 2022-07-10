% Akarsh Jain, 2020CS10318

ibt(empty).
ibt(node(N, L, R)):- integer(N), ibt(L), ibt(R).

size(empty,0).
size(node(_, L, R),N):-size(L,N1),size(R,N2),Y is N1+N2,N is Y+1.

height(empty,0).
height(node(_, L, R),N):-height(L,N1),
		height(R,N2),
		Y is N1,
		N1>=N2,
		N is Y+1.
height(node(_, L, R),N):-height(L,N1),
		height(R,N2),
		Y is N2,
		N2>N1,
		N is Y+1.
		
		
% append(L1,L2,L) is true if L=L1@L2
append([],L,L).
append([H|T],L2,[H|L3]) :- append(T,L2,L3).		


%takes inorder of left subtree and right subtree and appends them together with root in middle
inorder(empty,[]).
inorder(node(Z, L, R),List):- inorder(L,L1),
		inorder(R,L2),
		append(L1,[Z],L3),
		append(L3,L2,List).
		
preorder(empty,[]).
preorder(node(Z, L, R),List):- preorder(L,L1),
		preorder(R,L2),
		append([Z],L1,L3),
		append(L3,L2,List).
	
postorder(empty,[]).
postorder(node(Z, L, R),List):- postorder(L,L1),
		postorder(R,L2),
		append(L1,L2,L3),
		append(L3,[Z],List).
		

% to do this tail recursively, I have rotated the tree (as done in avl trees left rotation) to
% reduce the height of the left branch (since only if one branch is there, then it is easy to tail recurse)

trInorder(empty,[]).
trInorder(node(Z, empty, R),[Z|T]):- trInorder(R,T).
trInorder(node(Z, node(Z1, L1, R1), R),L):- 
		trInorder(node(Z1,L1,node(Z,R1,R)),L).
	

% this function takes the R tree and attaches it to the rightmost branch of left subtree
rightToLeft(empty,R,R).
rightToLeft(node(Z,L1,R1),R,node(Z,L1,R2)):-rightToLeft(R1,R,R2).
		
% if there is only left subtree, it is easy to tail recurse. Hence, if right subtree is there
% then it is attached to the left subtree at appropriate place
trPreorder(empty,[]).
trPreorder(node(Z,L,empty),[Z|T]):- trPreorder(L,T).
trPreorder(node(Z,L,R),List):-
		rightToLeft(L,R,BT),
		trPreorder(node(Z,BT,empty),List).

%true if tree is non-empty
notempty(node(_,_,_)).

% this function takes the L tree and attaches it to the leftmost branch of right subtree
lefttoRight(empty,L,L).
lefttoRight(node(Z,L1,R1),L,node(Z,L2,R1)):-lefttoRight(L1,L,L2).

% Helper function for trPostorder
% The list L1 contains the postfix of the postorder traversal, which increases in size as the tree is traversed
% at last when only the empty tree is left, it contains the entire postorder traversal.
% unfortunately, this is a iterative approach unlike others which are nearly pure declarative. This is
% because the declarative algorithm was exponential and was not able to process large inputs
iterPost(empty,List,L1):-append([],L1,List).
iterPost(node(Z,empty,R),List,L1):-
		iterPost(R,List,[Z|L1]).
iterPost(node(Z,L,R),List,L1):-
		lefttoRight(R,L,B),
		iterPost(B,List,[Z|L1]).

trPostorder(BT,List):-iterPost(BT,List,[]).

% recursive, appends the left euler tour, right euler tour with the current node appropriately
eulerTour(empty,[]).
eulerTour(node(Z,L,R),[Z|T]):-
		eulerTour(L,List1),
		append(List1,[Z],List2),
		eulerTour(R,List3),
		append(List2,List3,List4),
		append(List4,[Z],T).

% helper function for pre/post/inET
% middle(L,L1,Z,L2) checks if L=L1@[Z]@L2
middle([Z|L2],[],Z,L2).
middle([H|T],[H|T1],Z,L2):-middle(T,T1,Z,L2).

% given prehelp(Euler tour, Pre order traversal)
% splits the euler tour as EulerTour=[H]+L1+[H]+L3+[H] 
% recurses on L1 and L3 to get their preorder traversals namely L4 and L5
% constructs preorder by [H]+L4+L5
prehelp([],[]).
prehelp([H,H,H],[H]).
prehelp([H|List1],[H|List]):-
		middle(List1,L1,H,L2),
		append(L3,[H],L2),
		prehelp(L1,L4),
		prehelp(L3,L5),
		append(L4,L5,List).

preET(BT,List):-
		eulerTour(BT,List1),
		prehelp(List1,List).

posthelp([],[]).
posthelp([H,H,H],[H]).
posthelp([H|List1],List):-
		middle(List1,L1,H,L2),
		append(L3,[H],L2),
		posthelp(L1,L4),
		posthelp(L3,L5),
		append(L5,[H],L6),
		append(L4,L6,List).

postET(BT,List):-
		eulerTour(BT,List1),
		posthelp(List1,List).

inhelp([],[]).
inhelp([H,H,H],[H]).
inhelp([H|List1],List):-
		middle(List1,L1,H,L2),
		append(L3,[H],L2),
		inhelp(L1,L4),
		inhelp(L3,L5),
		append([H],L5,L6),
		append(L4,L6,List).

inET(BT,List):-
		eulerTour(BT,List1),
		inhelp(List1,List).
		

toString(empty,'()').
toString(node(Z,L,R),S):-
		toString(L,S1),
		toString(R,S2),
		atom_string(Z,S3),
		string_concat('(',S3,S4),
		string_concat(S4,', ',S5),
		string_concat(S5,S1,S6),
		string_concat(S6,', ',S7),
		string_concat(S7,S2,S8),
		string_concat(S8,')',S).

isBalanced(empty).
isBalanced(node(_,L,R)):-
		isBalanced(L),
		isBalanced(R),
		height(L,N1),
		height(R,N2),
		abs(N1-N2) < 2 .

% largest(BST,X) is true of X is the larger than any element in BST
largest(empty,_).
largest(node(Z,_,Y),M):- Z<M,largest(Y,M).

% smallest(BST,X) is true of X is the smaller than any element in BST
smallest(empty,_).
smallest(node(Z,X,_),M):- Z>M,smallest(X,M).

isBST(empty).
isBST(node(Z,L,R)):-
		largest(L,Z),
		smallest(R,Z),
		isBST(L),
		isBST(R).

lookup(N,node(N,_,_)).
lookup(N,node(Z,L,_)):-
		N<Z,
		lookup(N,L).
lookup(N,node(Z,_,R)):-
		N>Z,
		lookup(N,R).

%is true when the given node is not present in BST
notPresent(_,empty).
notPresent(N,node(Z,L,_)):-
		N=\=Z,
		N<Z,
		notPresent(N,L).
notPresent(N,node(Z,_,R)):-
		N=\=Z,
		N>Z,
		notPresent(N,R).


insert(N,empty,node(N,empty,empty)).
insert(N,node(Z,L,R),node(Z,L1,R)):-
		N=\=Z,
		N<Z,
		insert(N,L,L1).
insert(N,node(Z,L,R),node(Z,L,R1)):-
		N=\=Z,
		N>Z,
		insert(N,R,R1).
		
%finds the largest node value in the given non empty bst used in delete
large(node(Z,empty,empty),Z).
large(node(_,_,R),N):-
		large(R,N).

delete(N,node(N,empty,empty),empty).
delete(N,node(N,empty,R),R).	
% if deletion is to be done inside the tree then, the largest node from left subtree is put in its place
% to maintain BST invariant
delete(N,node(N,L,R),node(Z,L1,R)):-	
		large(L,Z),
		delete(Z,L,L1).
delete(N,node(Z,L,R),node(Z,L1,R)):-
		N=\=Z,
		N<Z,
		delete(N,L,L1).
delete(N,node(Z,L,R),node(Z,L,R1)):-
		N=\=Z,
		N>Z,
		delete(N,R,R1).

% special insertion for makeBST
% suppose we need to insert in the right tree and its height is greater than the left by 1 then, 
% we rotate the tree to reduce the height of the right tree so as to not violate in future the balanced condition
avlinsert(Z,empty,node(Z,empty,empty)).
avlinsert(Z,node(N,L,node(M,L1,R1)),BT):-
		Z>N,
		height(L,N1),
		height(node(M,L1,R1),N2),
		N2 is N1+1,
		avlinsert(Z,node(M,node(N,L,L1),R1),BT).
avlinsert(Z,node(N,L,R),node(N,L,R1)):-
		Z>N,
		avlinsert(Z,R,R1).
avlinsert(Z,node(N,node(M,L1,R1),R),BT):-
		Z<N,
		height(node(M,L1,R1),N1),
		height(R,N2),
		N1 is N2+1,
		avlinsert(Z,node(M,L1,node(N,R1,R)),BT).
avlinsert(Z,node(N,L,R),node(N,L1,R)):-
		Z<N,
		avlinsert(Z,L,L1).

makeBST([],empty).
makeBST([H|[]],node(H,empty,empty)).
makeBST(List,B):-
		sort(List,[H|T]),
		makeBST(T,B1),
		avlinsert(H,B1,B),
		isBST(B),
		isBalanced(B).

/*test cases
node(6,node(4,empty,empty),node(5,empty,empty))

node(6,node(4,node(3,empty,empty),node(2,empty,empty)),node(5,empty,empty))

node(4, node(2, node(1, empty, empty), node(3, empty, empty)), node(6, node(5, empty, empty), node(7, empty, empty)))
*/