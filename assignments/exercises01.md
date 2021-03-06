---
title: Exercise sheet 1
---

1. Prove that homotopy is an equivalence relation

2. Prove that if $Y$ is a convex subset of $\mathbb{R}^n$, then any two continuous functions from $X$ to $Y$ are homotopic. What if Y is star convex?

3. Prove that the relation of homotopy equivalence between two *spaces* is an equivalence relation.

3. Prove that the space $\mathbb{R}^n\setminus\{0\}$ is homotopically equivalent to $S^{n-1}$.

4. Prove that $\{1,1/2,1/3,\ldots\}\cup\{0\}$ can never be homotopically equivalent to $\{0,1,2,\ldots\}$ (both spaces are subsets of $\mathbb{R}$ and are given the subspace topology.)

5. Let $S_r := \{z \in \mathbb{C} \ |\ |z|<r\}$. Consider a polynomial $f(z) = a_0 + a_1 z + \cdots + a_n z^n$ of degree $n$, where $a_n\neq 0$. If $r$ is such that $f$ has has no root in $S_r$, then we can define the map $f_r : S_r \to \mathbb{C}\setminus\{0\}$, which is simply the restriction of $f$ to $S_r$.
	a) If $f(z) = (2z-1)(3z-1)(z-4)(z-5)$, then prove that its restriction, $f_1$, to the unit circle is homotopic to the restriction of the map $z \to z^2(z-4)^2$. In general, prove that if $f$ has $k$ roots whose modulus is less than 1, then its restriction, $f_1$, to the unit circle is homotopic to the restriction of the polynomial $z^k (z-2)^{n-k}$. *(For all these parts, remember that the domain is $\mathbb{C}\setminus\{0\}$ and not just $\mathbb{C}$.)*
	a) Prove that if $f$ does not have any root $\alpha$, such that $|\alpha|\leq r$, then $f_r$ is homotopic to a constant map.
	b) Prove that if $r$ is large enough, then $f_r :S_r \to \mathbb{C}\setminus \{0\}$ is homotopic to the map $\theta_n: S_r \to \mathbb{C}\setminus\{0\}$, where $\theta_n(z) = z^n$. *(Hint: Of course, if $r$ is large enough then $f$ will not have a root in $S_r$. However, you have to choose a homotopy so that the throughout the homotopy there is no root in $S_r$. Note that if $\alpha$ is a root of $f$, then $-a_n \alpha^n = a_0 + a_1 \alpha + \cdots a_{n-1}\alpha^{n-1}$. By taking the modulus on both sides, show that $|\alpha|$ must be less than $|a_0| + |a_1|+ \cdots + |a_n|$ if $\alpha$ is a root such that $|\alpha|>1$. How is this useful?*)
	c) Later in this course, will later prove that if $n>0$ then $\theta_n$ is not homotopic to a constant map. How will that help you to deduce the fundamental theorem of algebra?

6. A map is said to be null-homotopic if it is homotopic to a constant map.
	a) Prove that a space $X$ is contractible (homotopically equivalent to a point) if and only if the identity map $\mathrm{Id}_X$ is null-homotopic.
	b) Prove that if $Y$ is contractible, then any continuous map $f:X\to Y$ is null-homotopic (and therefore, any two maps are homotopic).
