---
title: Exercise sheet 4
---

1. Prove the following consequences of the Van Kampen theorem (try to prove each of them in detail using both versions of the theorem). In each case, we assume that $U \cap V$ is simply connected: 
	a. If $U$ and $V$ are simply connected open subsets of $X$ that cover $X$, then prove that $X$ is simply connected.
	a. If $U$ and $V$ are open subsets of $X$ that cover $X$, and $V$ is simply connected, then prove that the inclusion map $i: U \to X$ induces a surjection whose kernel is the smallest normal subgroup containing $j_*(\pi_1(U \cap V))$, where $j : U \cap V \to U$, is the inclusion map.
	b. If $U$ and $V$ are open subsets of $X$ that cover $X$, and $U\ \cap V$ is simply connected, then prove that $\pi_1(X)$ is isomorphic to $\pi_1(U) * \pi_1(V)$.

2. Use the previous question to compute the fundamental groups of:
	a. $S^n$, for $n\geq 2$.
	b. The projective plane realized as a closed 2-disk with the antipodal points on the boundary identified. *(Hint: Let $U$ be the projective plane minus a point and $V$ be a small disk containing that point. What does $U$ deformation retract to? Find a loop, $\gamma$, representing a generator of its fundamental group. How does $\gamma$ relate with a loop representating the generator of $\pi_1(U \cap V)$?)*
	c. The connected sum of two projective planes.

3. Can you construct a space whose fundamental group is $\mathbb{Z}/n\mathbb{Z}$ for any given natural number $n$? *(Hint: try generalizing the projective plane example)* How about constructing a space whose fundamental group is any given abelian group?

4. A space is said to be an $n$ dimensional manifold if each point on it has an open neighbourhood that is homeomorphic to $\mathbb{R}^n$.  Prove that the fundamental group of a manifold of dimension greater than or equal to 3 remains unchanged if you delete a point from the manifold.

5. Along with problem 6 from exercise set 2, this should help you to prove that if $A$ is a closed subset of $\mathbb{R}^2$ which is homeomorphic to $\mathbb{R}$, then $\mathbb{R}^2 \setminus A$ is disconnected:
	a. Let $\mathbb{R}^3_{+\epsilon}$ denote the subspace $\{(x, y, z) \in \mathbb{R}^3 \ |\ z > -\epsilon\}$ and $\mathbb{R}^3_{-\epsilon}$ denote the subspace $\{(x, y, z) \in \mathbb{R}^3 \ |\ z < \epsilon\}$. Prove that $U:=\mathbb{R}^3_{+\epsilon} \setminus (A \times (-\epsilon, 0])$ deformation retracts onto $\mathbb{R}^3_+ \setminus A$, $V:=\mathbb{R}^3_{-\epsilon} \setminus (A \times [0, \epsilon))$ deformation retracts onto $\mathbb{R}^3_-$ and $U \cap V$ deformation retracts onto $\mathbb{R}^2\setminus A$.
	b. Prove that if $\mathbb{R}^2 \setminus A$ is connected then so is $U \cap V$.
	c. Prove that if we assume that $\mathbb{R}^2\setminus A$ is connected, then the fundamental group of $\mathbb{R}^3\setminus A$ must be trivial. Use exercise 6 from exercise set 2. Why does this lead to a contradiction?

6. Use the previous exercise to prove that if $C$ is a closed subspace of $S^2$ which is homeomorphic to $S^1$, then $S^2 \setminus C$ is disconnected. *(Hint: $S^2$ minus a single point is homeomorphic to $\mathbb{R}^2$. By removing $C$, you have removed a lot more than one point!)*

