Hydra is a simple generic template engine with python and ocaml backend. 

In brief, Hydra can be used to add three new construct to latex:
* Insertion blocks: `<§...§>`
* Code blocks: `§:...:§`
* Capture blocks : `¤...¤`

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
#Code block
Code blocks allow to write more lenghty block of code.
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
Capture blocks simplifies the writing of long insertion block by capturing inner call to `<§...§>`.
More precisely, inside a code block a `¤...¤` block represent a latex string which will capture any call to `<§...§>`
``` python
¤ 1 + 2 = <§1+2§> ¤
```
is equivalent to
``` python
<§ "1+2={}".format(1+2) §>.
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
		¤ \sum_{k=1}^{<§n§>} k^{<§p§>}  = <§ sum(p,n) §> \\ ¤
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
 
#Installation
Using the opam package manager, it is sufficient to pin the repository using
```bash
opam pin /path/to/repository
```

#Ocaml backend
An alternative syntax is also available using the `--syntax generic` option. 
The ocaml backend is selected either by precising the `--mode ocaml_i` option or using hydra on a file with an `.hyml` extension.



