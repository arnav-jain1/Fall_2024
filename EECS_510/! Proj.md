# Part 1
 I am creating a language that parses and approves strings in what is called algebraic chess notation. Algebraic chess notation is a way of tracking moves being played allowing games to be recorded without the board itself. The most basic form of the notation involves the letter of the piece for the first character, then the location of where it will move to.
 For example Nf3 means knight to space f3 and e4 means pawn to space e4
 Note that these are very basic moves and my language will be able to parse more complex terms.

 I chose this language because I enjoy playing chess a lot and the chess world championships are currently going on right now.

The language will be represented by an NFA
Alphabet: {\[1-8], x, +, #, \[a-h], O-O, O-O-O, R, N, B, Q, K}
An acceptable string is as follows
(\[R, N, B, Q, K, $\lambda$]\[a-h, 1-8, $\lambda$]\[x, $\lambda$]\[a-h]\[1-8]\[+, #, $\lambda$])  |  O-O  |  O-O-O
For a quick breakdown, either you can castle king-side (O-O) or queen-side (O-O-O) or you can make an actual move. An actual move consists of a piece or lambda if its a pawn. Then, if and only if there are multiple pieces that can make the same move, you add either the rank (1-8) or file (a-h). After that, if a piece is being captured put an x. After that add the file and the rank in that order (mandatory) and finally, if it's a check then put +, if it is checkmate put #, and if it is neither then we are already done.

# Part 2 regular grammar
$$
\begin{aligned}
& S \rightarrow [R, N, B, Q, K]\alpha \space | \space \alpha \space | \space O-O \space | \space O-O-O\\
& \alpha \rightarrow [1-8]\beta \space | \space [a-h]\beta \space | \space \beta \\
& \beta \rightarrow x\gamma \space | \space \gamma \\
& \gamma \rightarrow [a-h][1-8]\delta \space | \space [a-h][1-8] \\
& \delta \rightarrow + \space | \space \# \space 
\end{aligned}
$$
```tikz
\usetikzlibrary{automata, positioning}

\begin{document}

\begin{tikzpicture}[shorten >=1pt, node distance=5cm, on grid, auto]

\node[state, initial] (S) {$S$};
\node[state] (alpha) [right=of S] {$\alpha$};
\node[state, accepting] (O1) [above right=of S] {$O1$};
\node[state, accepting] (O2) [below right =of S] {$O2$};
\node[state] (beta) [right=of alpha] {$\beta$};
\node[state] (gamma) [right=of beta] {$\gamma$};
\node[state, accepting] (delta) [right=of gamma] {$\delta$};
\node[state, accepting] (end) [right=of delta] {Accept};

\path[->]
(S)    edge node {$[R, N, B, Q, K], \lambda$} (alpha)
    edge node {$O-O$} (O1)
    edge node {$O-O-O$} (O2)
(alpha) edge node {$[1-8], [a-h], \lambda$} (beta)
(beta) edge node {$x, \lambda$} (gamma)
(gamma) edge node {$[a-h][1-8]$} (delta)
(delta) edge node {[$+$, $\#$]} (end);

\end{tikzpicture}

\end{document}
```




