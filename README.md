Hydra is a simple template engine for inserting python scripts inside a latex document.
In brief, Hydra adds three new construct to latex:
* Insertion block: `<§...§>`
* Python block: `§:...:§`
* Capture block : `¤...¤`

# Insertion block
The python expression inside an insertion block is evaluated and
printed to the current environnement, in general the current latex file.
For instance,
``` latex
\begin{equation}
1 + 2 = <§ 1+2 §> 
\end{equation}
``` 
becomes
``` latex
\begin{equation}
1 + 2 = 3
\end{equation}
```
#Python block
The python block allows to write more lenghty block of code.
We can complexify the previous example into
``` latex
§:
def sum(k, n ) :
	return (sum( [ x**k for x in range(n+1) ] ) )
n=100
p=2
:§
\begin{equation}
\sum_{k=1}^{<§n§>} k^{<§p§>}  = <§ sum(p,n) §> 
\end{equation}
``` 
which becomes
``` latex
\begin{equation}
\sum_{k=1}^{100} k^{2}  =  328350 
\end{equation}
```

#Capture block
The capture block facilitate the use of insertion block inside 
the python block. More precisely, inside a python block a `¤...¤` block
represent a latex string which will capture any call to `<§...§>` inside the environnement
``` python
s = ¤ 1 + 2 = <§1+2!> ¤
```
is equivalent to the string
``` python
s = r'''1 + 2 = 3'''.
```
These blocks can be useful combined with the possibility to use insertion bloc inside a python block.
For instance
``` python
§:
def sum(k, n ) :
	return (sum( [ x**k for x in range(n+1) ] ) )
n=100
:§
\begin{align} §:
	for p in range(2):
		<§ ¤ \sum_{k=1}^{<§n§>} k^{<§p§>}  = <§ sum(p,n) §> \\ ¤  §>
:§ \end{align}
```
becomes 
``` latex
\begin{align}
\sum_{k=1}^{100} k^{0}  =  100 \\
\sum_{k=1}^{100} k^{1}  =  5050 \\
\sum_{k=1}^{100} k^{2}  =  328350 
\end{align}
```
 


