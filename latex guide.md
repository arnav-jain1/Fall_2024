# LaTeX Guide for Equations and Proofs

LaTeX is widely used for typesetting mathematical documents due to its powerful handling of mathematical notation. Below are common elements used in writing equations and proofs:

Note: For new line latex use \$\$ to surround the equation

## Superscript and Subscript

- Superscript: `a^2` renders as  $a^2$
- Subscript: `a_2` renders as $a_2$
	- Use {} for multiple characters: `a_{5a1}` renders as $a_{5a1}$

## Functions and Logarithms

- `\sin`, `\cos`, `\log`, etc.: `\sin(x)` renders as $\sin(x)$
- Logarithms: `\log_{2}(x)` renders as $\log_{2}(x)$
- Fractions: `\frac{a}{b}` renders as $\frac{a}{b}$
- Square Roots: `\sqrt{x}` renders as $\sqrt{x}$
- nth Roots: `\sqrt[n]{x}` renders as $\sqrt[n]{x}$

## Set Notation

- Set of integers `\mathbb{Z}` renders as $\mathbb{Z}$
- Set of rational numbers `\mathbb{Q}` renders as $\mathbb{Q}$
- Set of real numbers `\mathbb{R}` renders as $\mathbb{R}$
- Set of positive real numbers `\mathbb{R}^+` renders as $\mathbb{R}^+$
- Set of complex numbers `\mathbb{C}` renders as $\mathbb{C}$
- Empty set `\emptyset` renders as $\emptyset$ 
- Set of numbers  `\{1, 2, 3, 4, 5\}` renders as $\{1, 2, 3, 4, 5 \}$
- Ellipses `\ldots` renders as $\ldots$  
	- `\cdots`, `\vdots`, and `\ddots` render as $\cdots$ $\vdots$ and $\ddots$ respectively
## Quantifiers

- Existential: `\exists` renders as $\exists$
- Universal: `\forall` renders as $\forall$
- In: `\in` renders as $\in$
- Not In: `\notin` renders as $\notin$
- Union: `\cup` renders as $\cup$
- Intersection: `\cap` renders as $\cap$
- Subset: `\subset` renders as $\subset$
- Superset: `\supset` renders as $\supset$
- Equivalent: `\equiv` renders as $\equiv$
- Implies: `\Rightarrow` renders as $\Rightarrow$
- If and Only If (Iff): `\Leftrightarrow` renders as $\Leftrightarrow$
- Infinity: `$\infty$` renders as $\infty$ 

## Operators

- Summation: `\sum_{i=1}^{n}` renders as $\sum_{i=1}^{n}$
- Product: `\prod_{i=1}^{n}` renders as $\prod_{i=1}^{n}$
- Limits: `\lim_{x \to \infty}` renders as $\lim_{x \to \infty}$
- Derivative: `\frac{d}{dx} f(x)` renders as $\frac{d}{dx} f(x)$
- Partial Derivative: `\frac{\partial}{\partial x} f(x,y)` renders as $\frac{\partial}{\partial x} f(x,y)$
- Definite Integral: `\int_{a}^{b} f(x) dx` renders as $\int_{a}^{b} f(x) \, dx$
- Indefinite Integral: `\int f(x) dx` renders as $\int f(x) \, dx$

## Relations

- Less than or equal to: `\leq` renders as $\leq$
- Greater than or equal to: `\geq` renders as $\geq$
- Not equal to: `\neq` renders as $\neq$

## Greek Letters

- Alpha: `\alpha` renders as $\alpha$
- Beta: `\beta` renders as $\beta$
- Gamma: `\gamma` renders as $\gamma$, etc.
- Pi: `\pi` renders as $\pi$
- Theta: `\theta` renders as $\theta$
- Lambda: `\lambda` renders as $\lambda$
- Sigma: `\sigma` renders as $\sigma$
- lowercase omega: `\omega` renders as $\omega$
- Uppercase Omega: `\Omega` renders as $\Omega$

## Logical Symbols

- And: `\land` renders as $\land$
- Or: `\lor` renders as $\lor$
- Not: `\neg` renders as $\neg$
- Therefore: `\therefore` renders as $\therefore$
- Because: `\because` renders as $\because$

## Matrices

Matrices can be created using the `matrix` environment:

```
latex
\begin{matrix}
a & b \\
c & d
\end{matrix}
```
$$
\begin{matrix}
a & b \\
c & d
\end{matrix}
$$
