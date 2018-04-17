### Graphs
This package contains a study of undirected graphs. 
Taken from the book Algorithms, Fourth edition by Sedgwick.
The core structure of analyzing an undirected graph is 
the Adjacency structure. This structure can be in one of many forms:
1. A matrix that contains n x n elements of all the vertices.
2. Some type of list structure containing the vertices of all elements
with each element referencing all the immediate edges of a vertex.
Formally speaking these structures are defined as:
## adjacency matrix
## array of edges
## array of adjacency lists
##
The implementation I have chosen is not documented in the book. The usual
implementation of choice is an array of adjacency lists.  In this implementation
you are typically given an array up front that contains all the valid vertices
of the graph that you are storing in memory. Each element of the array 
then contains a linked list of immediate neighbors to the given vertex. In the 
case of an undirected map, each vertice will appear twice in the linked list
since two connected vertices point to each other; as opposed to a directed graph 
in which each node only points in one direction:

directed:
a ---> b is given as input.
undirected:
a ---> b is given as input. However, a new vertex, b , is then inserted into the 
array of vertices for b:
b ---> a

Using Maps instead of Arrays:
I am using maps instead of Arrays ( or lists ) to represent this graph structure.
It seems only natural to use a dynamic map structure to replace the conventional
Array + LinkedList structure that is usually given as an example. 

The time and space complexity of the Adjacent List is as follows:

• Space usage proportional to V + E

• Constant time to add an edge

• Time proportional to the degree of v to iterate through vertices adjacent to v 
(constant time per adjacent vertex processed)

Map usage appears to be the same as above. The only immediate difference I see is
due to the dynamic nature of map creation. There is no need to store all the vertices
up front but instead allocate them as they are read in. 

There might be other experimental structures that might prove interesting. For example,
Instead of storing the edges in a list or map structure ( here I am using a map for edges also )
but .. instead storing them in a differenct type of data structures depending on 
the type of underlying application needs. For example, a bst or a heap might prove 
both interesting an useful.
