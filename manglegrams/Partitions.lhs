%% -*- LaTeX -*- 
\documentclass{tmr}

%% This file is simultaneously
%%   (a) a valid Haskell source file,
%%   (b) a valid LaTeX source file, and
%%   (c) valid input to the lhs2TeX program.
%% Is that neat, or what?

%include lhs2TeX.fmt
%options ghci

% possible packages to fix URL formatting: urlbst, breakurl?

\title{Generating Multiset Partitions}
\author{Brent Yorgey\email{byorgey@@gmail.com}}

\newcommand{\N}{\mathbb{N}}
\newcommand{\term}[1]{\emph{#1}}
\newcommand{\bell}[1]{\varpi_{#1}}
\newcommand{\vleq}{\trianglelefteq}
\newcommand{\nvleq}{\ntrianglelefteq}
\newcommand{\vgeq}{\trianglerighteq}

\begin{document}

\begin{introduction} 
  \emph{Multisets} are a generalization of sets that allow elements
  to occur more than once.  In this article, I present a general
  algorithmic framework in Haskell for generating the unique
  \emph{partitions} of multisets using time linear in the number of
  partitions.  The framework is based on the key observation that
  multisets can be represented using vectors in $\N^n$.  It is
  general enough that it also lends itself to efficient computation
  of a number of related combinatorial functions, including vector
  partitions, integer partitions, and power sets.

  This article is literate Haskell; the reader is encouraged to load
  it into their favorite interpreter and experiment while reading along.
  Every example shown in the text was actually computed by the listed
  code, through the magic of lhs2TeX~\cite{lhs2TeX}.
%  Also, wow, does the
%  italic numeral `2' have a fantastic curlicue, or what? 2 2 2.

%if style == newcode
\begin{code}
module Partitions where

import Data.List
import Control.Monad
import Control.Arrow

import Test.QuickCheck
\end{code}
%endif

\end{introduction}

\section{Introduction}

A few months ago, working on a Project Euler~\cite{euler} problem
(\#159, to be precise), I wanted to write a function to generate all
factorizations of a given integer $n \geq 2$.  A factorization of
$n$ is simply a nonempty list of integers, all greater than $1$,
whose product is $n$.  For example, $30$ has five distinct
factorizations: $30 = 15 \times 2 = 10 \times 3 = 6 \times 5 = 5
\times 3 \times 2$.  (Note that we include the trivial factorization
$30 = 30$ and consider factorizations equal up to permutation of the
factors, e.g.\ $15 \times 2$ and $2 \times 15$ are the same
factorization.) How can we compute all factorizations of a given
integer efficiently?

Consider representing integers by multisets of their prime
factors. For example, $30 = \{2,3,5\}$, and $12 = \{2,2,3\}$.
We must use multisets of prime factors rather than sets, since
we want to be able to distinguish between, say, $6 = \{2,3\}$ and $12
= \{2,2,3\}$.  Given an integer $n$ and its multiset of prime factors,
a factorization of $n$ corresponds to a particular grouping of the
prime factors into subsets.  For example, for $n=30$, the
factorization $10 \times 3$ corresponds to the grouping $\{\{2,5\},
\{3\}\}$.  

In general, such a grouping into subsets is known as a
\term{partition}.  Formally, a partition of a multiset $M$, or
\term{multipartition}, is a collection of sub-multisets (usually just
referred to as subsets) $T_i \subseteq M$ for which
\begin{equation*}
  \biguplus T_i = M,
\end{equation*}
where $\uplus$ denotes multiset union; for example, $\{1,2\} \uplus
\{1,3\} = \{1,1,2,3\}$. 

As an example, we can list all the multipartitions of $\{2,2,3\}$: \[
\{\{\{2,2,3\}\}, \{\{2,2\}, \{3\}\}, \{\{2\}, \{2,3\}\}, \{\{2\},
\{2\}, \{3\}\}\}. \] These correspond to the factorizations $12 = 4
\times 3 = 2 \times 6 = 2 \times 2 \times 3$, and in general it's not
hard to see that factorizations of an integer exactly correspond to
partitions of its multiset of prime factors.

So, I set out to write some code for generating multiset partitions.
This turned out to be rather tricky, and it took me a few days to come
up with a good solution. The solution I eventually found, however,
ended up being quite general, and serves as a nice case study in the
expressiveness of Haskell.  I found I was largely able to think
``through'' the language directly about the problem, without having to
do too much translation between my head and the code.  (Or perhaps
this only means that I think in Haskell, but that wouldn't be such a
bad thing either.)

Algorithms for generating multiset partitions have been published
previously, by, for example, Knuth~\cite{TAOCP} (who also notes the
correspondence between multisets and vectors in $\N^n$). However,
Knuth's implementation uses an imperative paradigm.  To my knowledge
there have been no published treatments of the multipartition problem
in a functional context, although I would gladly welcome correction on
this point.

\section{A na\"ive approach: set partitions}

Since multisets are a generalization of sets, let's start by
generating set partitions and see how far it takes us.  Given a set
$S$, we can generate its partitions as follows: first, choose an
arbitrary element $s \in S$; then generate the power set (set of all
subsets) of the remaining elements of $S$; each one of these subsets
can be combined with $s$ and a partition of the remaining elements
to form a partition of $S$.  That is, for each $T \subseteq (S
\setminus \{s\})$, recursively find the partitions of $S \setminus
(\{s\} \cup T)$, and add $\{s\} \cup T$ to each to form a partition
of $S$.

For example, let $S = \{2,3,5\}$ and choose $s = 2$, so the first
subset of every generated partition will contain $2$.  Next we
generate the power set of $S \setminus \{2\} = \{3, 5\}$, namely,
$\{\{3,5\}, \{3\}, \{5\}, \varnothing\}$.  Each one of these can be
combined with $s$.  Combining the first with $s$ yields our first
partition, $\{\{2\} \uplus \{3,5\}\} = \{\{2,3,5\}\}$, since there is
nothing left to recursively partition.  Combining $\{3\}$ with $s$
yields a first subset of $\{2,3\}$, and recursively partitioning the
remaining $\{5\}$ yields only itself, giving us $\{\{2,3\}, \{5\}\}$.
Combining $\{5\}$ with $s$ similarly gives us $\{\{2,5\}, \{3\}\}$.
Finally, we combine $\varnothing$ with $s$, giving $\{2\}$ as the
first subset; recursively partitioning the remaining $\{3,5\}$ gives
both $\{\{3,5\}\}$ and $\{\{3\}, \{5\}\}$, and prepending $\{2\}$
gives us the final two partitions, $\{\{2\}, \{3,5\}\}$ and
$\{\{2\},\{3\},\{5\}\}$.

Some thought (and/or trying more examples) should convince the reader
that this succeeds in generating each partition of $S$ exactly once.

This algorithm is implemented in Listing~\ref{lst:set-partitions},
using lists as a simple representation of sets.  (Incidentally, most
of the code in this article, including that in
Listing~\ref{lst:set-partitions}, was tested with
QuickCheck~\cite{QC}; the QuickCheck properties are not shown but can
readily be found in the source.)

\begin{listing}[htp]
\begin{code}
-- pSet s generates the power set of s, pairing each subset
-- with its complement.  
-- e.g. pSet [1,2] = [([1,2],[]),([1],[2]),([2],[1]),([],[1,2])].
pSet :: [a]   -> [([a],[a])]
pSet []     = [([],[])]
pSet (x:xs) = mapx first ++ mapx second where
   mapx which = map (which (x:)) $ pSet xs
   first f (x,y) = (f x, y)
   second f (x,y) = (x, f y)

-- setPartitions S generates a list of partitions of S.
setPartitions :: [a] -> [[[a]]]
setPartitions []     = [[]]
setPartitions (s:s') = do (sub,compl) <- pSet s'
                          let firstSubset = s:sub
                          map (firstSubset:) $ setPartitions compl
\end{code}
\caption{Computing set partitions. \label{lst:set-partitions}}
\end{listing}

% $ % (fix syntax highlighting in Emacs, which is very confused by the
%     mixture of LaTeX and Haskell =)

%if style == newcode
\begin{code}
-- compute Bell numbers using the Pierce triangle. 
-- See Knuth, TAOCP Vol.4, 7.2.1.5.
nextPierceRow :: [Integer] -> [Integer]
nextPierceRow r = next
    where next = last r : zipWith (+) r next

pierceTriangle = iterate nextPierceRow [1]
bellNumbers = map head pierceTriangle
bell = (bellNumbers !!)

-- test to make sure the right number of partitions are generated.
prop_bell :: [Int] -> Bool
prop_bell s = genericLength (setPartitions s) == bell (length s)

-- many of the properties exhibit combinatorial blow-up after only a
-- few tests, so implement a special testing function testN which
-- allows us to limit the number of tests performed.  Use
-- (testN n) just like test.
only :: Int -> Config
only n = defaultConfig { configMaxTest = n }

testN :: (Testable a) => Int -> a -> IO ()
testN = check . only
\end{code}
%endif
% $

For example, evaluating |setPartitions [2,3,5]| yields
\begin{quote}
\eval{setPartitions [2,3,5]},
\end{quote} %% ??? better way to do this (w/o horizontal offset)?
just as we computed before.  Now, given a suitable implementation of
|factor|, which returns a list of the prime factors of its argument,
generating factorizations is straightforward:

\begin{code}
factorizations2 :: (Integral a) => a -> [[a]]
factorizations2 = map (map product) . setPartitions . factor
\end{code}

Let's try |factorizations2 30|:
\begin{quote}
  \eval{factorizations2 30}.
\end{quote}

Lovely! However, there's a problem: as you may have guessed,
|setPartitions| doesn't do so well when given a multiset instead of a
set. For example, evaluating |setPartitions [2,2,3]| yields
\begin{quote}
  \eval{setPartitions [2,2,3]}.
\end{quote}
The two copies of |2| are treated as if they are distinct, and we end
up with both |[[2,3],[2]]| and |[[2],[2,3]]|, which are equivalent
when considered as multiset partitions.  This means that an expression
such as |factorizations2 12| will give duplicate results as well:
\begin{quote}
  \eval{factorizations2 12}.
\end{quote}

This certainly won't do.  We need to get rid of the duplications, but
how?  The easiest and most obvious way is to create a new function,
|msetPartitions|, which generates set partitions and then culls
duplicates:

\begin{code}
msetPartitions :: (Ord a) => [a] -> [[[a]]]
msetPartitions = nub . map (sort . map sort) . setPartitions
\end{code}

Normalizing the partitions with |map (sort . map sort)| before
applying |nub| is necessary, since otherwise |nub| will not consider
partitions such as |[[2,3],[2]]| and |[[2],[2,3]]| equal. Evaluating
|msetPartitions [2,2,3]| yields
\begin{quote}
\eval{msetPartitions [2,2,3]}
\end{quote}
as expected.  With a definition of |factorizations3| using
|msetPartitions| in place of |setPartitions|, we can now correctly
compute the factorizations of $12$:

%if style == newcode
\begin{code}
factorizations3 :: (Integral a) => a -> [[a]]
factorizations3 = map (map product) . msetPartitions . factor
\end{code}
%endif

\begin{quote}
  \eval{factorizations3 12}.
\end{quote}

Are we done?  Some degree of inefficiency is clearly involved here,
since throwing away duplicate partitions means wasted work to generate
them.  The question is, how much inefficiency?  The code certainly has
conciseness and elegance going for it, so we might be willing to
overlook a little inefficiency if it means we can have a concise
implementation that still works for all practical purposes.

Unfortunately (you probably saw this one coming), |msetPartitions|
turns out to be staggeringly inefficient. The worst case occurs when
we have the most possible overlap between elements, namely, a multiset
with $n$ copies of the same element. First, since |setPartitions|
never examines the elements of its argument, it clearly generates the
same number of partitions for such a list as it does for any other
$n$-element list, namely, the $n$th Bell number $\bell{n}$.  Bell
numbers \cite{mw_bell, concrete} count the number of distinct set
partitions of an $n$-element set, and satisfy the recurrence \[
\bell{0} = 1; \quad \bell{n+1} = \sum_{k=0}^n \binom{n}{k}
\bell{k}. \] (Extra credit: how does this recurrence relate to
|setPartitions|?)  Thus, the first few Bell numbers for $n \geq 0$ are
$1,1,2,5,15,52,203,877,4140,21147\dots$ \cite{oeis_bell}.

On the other hand, the number of distinct multipartitions of such a
worst-case multiset is the $n$th partition number $P_n$, which counts
the number of ways of writing $n$ as a sum of positive integers.
Partition numbers \cite{mw_partition, TAOCP} have the generating
function \[ (1 + z + z^2 + \dotsb)(1 + z^2 + z^4 + \dotsb)(1 + z^3 +
z^6 + \dotsb) \dots = \prod_{n \geq 1} \left( \sum_{k \geq 0} z^{nk}
\right) = \prod_{n \geq 1} \frac{1}{1 - z^n},
\] and also satisfy a recurrence discovered by Euler,
\begin{multline*}
 P_0 = 1; \quad P_n = P_{n-1} + P_{n-2} - P_{n-5} - P_{n-7} + P_{n-12}
 + P_{n-15} - \dotsb \\ = \sum_{\substack{-\infty < k < \infty\\k \neq
 0}} (-1)^{k+1} P_{n - (3k^2 + k)/2},
\end{multline*}
if we stipulate that $P_n = 0$ when $n < 0$.  The first few partition
numbers for $n \geq 0$ are $1,1,2,3,5,7,11,15,22,30\dots$
\cite{oeis_partitions}.

As you can see, Bell numbers grow far more quickly than partition
numbers. (This can be shown analytically; $\bell{n}$ grows as $(n/\log
n)^n$, whereas $P_n$ only grows as $A^{\sqrt{n}}/n$ for a certain
constant $A$ \cite{TAOCP}.) For example, |msetPartitions| correctly
computes the $P_{10} = 42$ unique multipartitions of |replicate 10 1|,
but it takes a few seconds to do so, since it must cull duplicates
from an initial list of $\bell{10} = 115975$.  And it isn't long
before things become completely hopeless: computing the modest $P_{30}
= 5604$ unique multipartitions of |(replicate 30 1)| in this way would
require culling duplicates from among the whopping $\bell{30} =
846749014511809332450147$ (that's $8.5 \times 10^{24}$) generated by
|setPartitions|!

So, for all its conciseness, |msetPartitions| won't work after all.
We need a way to directly generate all the unique partitions of a
multiset, using only time linear in the number of partitions
generated.

\section{Multiset partitions are vector partitions}

In order to directly generate partitions of a multiset $M$, we need to
impose some sort of ordering on the subsets of $M$, and to be able to
generate them in order.  Using such an ordering, we could guarantee
that the subsets in a partition always occur in order, thus ruling out
the possibility of partitions occurring multiple times with their
subsets arranged differently.  Indeed, this is how |setPartitions|
works.  Considering the elements of a set $S$ (represented by a
Haskell list) to be ``ordered'' by the order of their occurrence in
the list, the partitions generated by |setPartitions| always contain
strictly increasing subsets in increasing order (where subsets are
ordered by their first elements).

We could also order multiset subsets lexicographically, but it is
not clear how we could efficiently generate them in order.
Considering some elements equal to one another throws a big wrench
into things, since we can no longer simply recurse over a multiset
one element at a time, as |setPartitions| does.

Suppose we have a multiset $M$, which has $n$ unique elements.  The
key insight is that we can represent $M$ by a pair $(v,e)$, where $v$
is a vector in $\N^m$ (that is, an $m$-tuple of non-negative
integers), $e$ is an ordered list of length $m$, and $m \geq n$.  In
particular, $e$ is a list of distinct elements which are a superset of
the unique elements of $M$, and $v$ records the number of occurrences
of each element (possibly zero for elements not occurring in $M$).
For example, the multiset containing $\{6,5,7,4,4,5,4\}$ can be
represented by the vector $(3,2,0,1,1)$ together with the list of
elements $[4,5,29,6,7]$.

Using this representation, multiset union corresponds to componentwise
vector addition, which reduces the multiset partition problem to that
of finding all sets of vectors in $\N^n$ which sum to a given vector.
In other words, multiset partitions are vector partitions in disguise!

As an example, if we represent the multiset $M = \{2,2,3\}$ by the
vector $(2,1)$ and element list $[2,3]$, then the partitions of $M$
correspond to partitions of $v$, as shown below:
\begin{gather*}
\{\{\{2,2,3\}\}, \{\{2,2\}, \{3\}\}, \{\{2,3\}, \{2\}\},
\{\{2\}, \{2\}, \{3\}\} \} \\
\cong \\
\{\{(2,1)\}, \{(2,0), (0,1)\}, \{(1,1),(1,0)\},
\{(1,0),(1,0),(0,1)\}\}. 
\end{gather*}

This is good, because it is easy to impose an ordering on vectors,
and, as we will see, they can also be efficiently generated in order.

\section{Multisets}

We will represent vectors in Haskell as |[Int]|. This is a
simplification, but works fine for our purposes.  Multisets are
represented as discussed previously:

\begin{code}
type Vec = [Int]
data MultiSet a = MS [a] Vec deriving Show
\end{code}

Of course, this Haskell definition of |MultiSet| is too lax; in
particular, it doesn't preclude the element list and count vector
having different lengths.  We could correct this by using a list of
pairs instead of a pair of lists; however, most of the time we'll be
manipulating the element list and count vector separately, and the
headache of constantly zipping and unzipping the two lists simply
isn't worth it.

Converting between multisets and regular lists is straightforward
(Listing~\ref{lst:convert-list-ms}).  Although an |Eq| instance for
the element type is all that is needed for the conversion, an |Ord|
instance allows it to be done more efficiently, so we include both as
options.

\begin{listing}
\begin{code}
fromList :: (Ord a) => [a] -> MultiSet a             -- O(n lg n)
fromList = uncurry MS . unzip . map (head &&& length) 
             . group . sort

fromListUnord :: (Eq a) => [a] -> MultiSet a         -- O(n^2)
fromListUnord xs = MS nx counts 
  where nx     = nub xs             
        counts = map (\elt -> (length . filter (==elt) $ xs)) nx

toList :: MultiSet a -> [a]
toList (MS es cs) = concat $ zipWith replicate cs es
\end{code}
\caption{Converting between lists and multisets. \label{lst:convert-list-ms}}
\end{listing}

%if style == newcode
\begin{code}
prop_toFromList :: (Ord a) => [a] -> Bool
prop_toFromList xs = (toList . fromList $ xs) == sort xs

prop_toFromListUnordIdem :: (Eq a) => [a] -> Bool
prop_toFromListUnordIdem xs = (tfu . tfu $ xs) == (tfu xs) 
    where tfu = toList . fromListUnord
\end{code}
%endif

For example, here's |fromList [2,3,3,2,3,5]|:
\begin{quote}
  \eval{fromList [2,3,3,2,3,5]}.
\end{quote}

Now that we can easily convert between lists and our multiset
representation, we can get on with the real work: generating vector
partitions. 

\section{Vectors}

First, some notation: if $u,v \in \N^n$, then $u \vleq v$ indicates
that $u$ is componentwise less than or equal to $v$, and $u \leq v$
indicates that $u$ is lexicographically less than or equal to $v$.  In
other words, $u \vleq v$ means that every element of $u$ is no greater
than the corresponding element of $v$, whereas $u \leq v$ implies only
that $u$ is no greater than $v$ in the first element where they
differ. Therefore, $u \vleq v$ implies $u \leq v$, but the converse
does not hold.  For example, $(1,2,7) \leq (1,3,5)$ (since $2 \leq
3$), but $(1,2,7) \nvleq (1,3,5)$ (since $7 \nleq 5$).


A few simple utility functions for manipulating |Vec| values are shown
in Listing~\ref{lst:vec-impl}. Note that the |Ord| instance for lists
is lexicographical.  Thus, for |u,v :: Vec|, the Haskell expression |u
<= v| corresponds exactly to the notation $u \leq v$ introduced
earlier.  The relation $u \vleq v$ is implemented by the custom
|(<||=)| operator.

\begin{listing}[htp]
\begin{code}
-- recall that type Vec = [Int].

-- componentwise comparison of vectors.
(<|=) :: Vec -> Vec -> Bool
xs <|= ys = and $ zipWith (<=) xs ys

-- (vUnit v) produces a unit vector of the same length as v.
vUnit :: Vec -> Vec
vUnit []     = []
vUnit [_]    = [1]
vUnit (_:xs) = 0 : vUnit xs

-- (vZero v) produces a zero vector of the same length as v.
vZero :: Vec -> Vec
vZero = map (const 0)

-- test for the zero vector.
vIsZero :: Vec -> Bool
vIsZero = all (==0)

-- do vector arithmetic componentwise.
(.+), (.-) :: Vec -> Vec -> Vec
(.+) = zipWith (+)
(.-) = zipWith (-)

\end{code}
\caption{Implementation of |Vec|. \label{lst:vec-impl}}
\end{listing}
% $

\section{Generating vector partitions}

Given a vector $v$, the general method we will use to recursively
generate all its partitions is as follows: first, we include the
singleton set $\{v\}$ as a special case; then, for each smaller
vector $v'$ in some appropriate subset of $\{ u : u \vleq v\}$,
recursively generate all partitions $P'$ of $v - v'$, and combine to
form the partitions $\{v'\} \cup P'$.  Since the vectors in each
$P'$ sum to $v - v'$, adding $v'$ will create a partition of $v$.
All we have left to determine is what the ``appropriate subset'' of
$\{ u : u \vleq v \}$ should be.

First, each partition should contain vectors in lexicographically
non-decreasing order; as discussed before, this will guarantee that we
don't get duplicate partitions.  In order to enforce this restriction
as we recurse, we must keep track of a current lower limit $v_L$,
which is the largest (hence, most recent) vector in the current
partially built partition.  We will only choose vectors $v'$ for which
$v' \geq v_L$.

Second, we need not bother with vectors $v'$ for which $v - v' < v'$,
since in that case it would be impossible to complete a non-decreasing
partition starting with $v'$.  In other words, we only need to
consider vectors $v'$ which are less than or equal to ``half'' of $v$,
defined as the vector in the exact middle of the lexicographically
ordered list of vectors $u \vleq v$ (rounding down if the list has an
even number of elements).

In summary, we want to choose vectors $v'$ for which $v' \vleq v$
and $\frac{1}{2}v \geq v' \geq v_L$.  To realize this, we first need
a function to compute ``half'' of a vector $v$.  The implementation
of |vHalf| is straightforward, recursively splitting along each
dimension until finding one that splits evenly, then copying the
remainder of the vector.  We also need a function to generate a
lexicographically sorted list of vectors which fall within a certain
range. The function |withinFromTo| takes three vectors |m|, |s|, and
|e|, and generates the list of vectors $v$ for which $m \vgeq v$ and
$s \geq v \geq e$; that is, vectors falling \emph{within} the
$n$-dimensional box framed by the origin and $m$, starting
\emph{from} $s$, and running down \emph{to} $e$.
Listing~\ref{lst:vector-partitions} puts all of these ideas
together.

\begin{listing}
\begin{code}
vPartitions :: Vec -> [[Vec]]
vPartitions v = vPart v (vUnit v) where
  vPart v _ | vIsZero v = [[]]
  vPart v vL = [v] : [ v' : p' | v' <- withinFromTo v (vHalf v) vL,
                                 p' <- vPart (v .- v') v' ]

vHalf :: Vec -> Vec
vHalf [] = []
vHalf (x:xs) | (even x)  = (x `div` 2) : vHalf xs
             | otherwise = (x `div` 2) : xs

downFrom :: Int -> [Int]
downFrom n = [n,(n-1)..0]

-- (within m) generates a decreasing list of vectors v <|= m.
within :: Vec -> [Vec]
within = sequence . map downFrom

-- This is a lie.
withinFromTo' :: Vec -> Vec -> Vec -> [Vec]
withinFromTo' m s e = 
    takeWhile (>= e) . dropWhile (> s) . within $ m

\end{code}
\caption{Computing vector partitions. \label{lst:vector-partitions}}
\end{listing}
% $

%if style == newcode
\begin{code}
vSum :: [Vec] -> Vec
vSum = foldl' (.+) (repeat 0)

prop_vPartitions :: Vec -> Property
prop_vPartitions v = (not . null $ v) && (not . vIsZero $ v) ==> (all ((==v) . vSum) . vPartitions $ v)
\end{code}
%endif
% $

Let's try an example, and compute the partitions of the vector
$(2,1)$:
\begin{quote}
  \eval{vPartitions [2,1]}.
\end{quote}

Looking good!  We're almost done, but there is still one loose end to
tie up. The single quote at the end of |withinFromTo'|
(Listing~\ref{lst:vector-partitions}) is not a typo; in fact,
|withinFromTo'| is an impostor!  It only illustrates the intended
behavior of its quote-less cousin used in the definition of
|vPartitions|.  Although |withinFromTo'| elegantly matches its
intuitive definition described earlier, it is inefficient.  Just like
|msetPartitions|, it generates some vectors only to discard them.
Since our goal is to write ``productive'' code that doesn't do any
unnecessary work, we'll actually use a more efficient version at the
cost of some readability.

\section{Efficiently enumerating vectors}

The real |withinFromTo| is shown in Listing~\ref{lst:generating-vecs}.
The implementation of this function is tricky to get
right; careful attention must be paid to appropriate base cases,
especially the case when $s \nvleq m$.  In such a case we must first
``clip'' $s$ against $m$ before proceeding.  Otherwise we risk
generating vectors $v$ for which $v \leq s$ but $v \nvleq m$. In the
general case, we recurse over each dimension and ensure that the
generated vectors stay lexicographically between $s$ and $e$.

\begin{listing}[htp]
\begin{code}
clip :: Vec -> Vec -> Vec
clip = zipWith min

withinFromTo :: Vec -> Vec -> Vec -> [Vec]
withinFromTo m s e | not (s <|= m) = withinFromTo m (clip m s) e
withinFromTo m s e | e > s = []
withinFromTo m s e = wFT m s e True True
  where
    wFT [] _ _ _ _ = [[]]
    wFT (m:ms) (s:ss) (e:es) useS useE = 
        let start = if useS then s else m
            end   = if useE then e else 0
        in
          [x:xs | x  <- [start,(start-1)..end],
                  let useS' = useS && x==s,
                  let useE' = useE && x==e,
                  xs <- wFT ms ss es useS' useE' ]
\end{code}
\caption{A better implementation of |withinFromTo|. \label{lst:generating-vecs}}
\end{listing}

%if style == newcode
\begin{code}
prop_wNaive :: Vec -> Bool
prop_wNaive m = (within m) == (withinFromTo m m (vZero m))

prop_wftNaive :: Vec -> Vec -> Vec -> Property
prop_wftNaive m s e = (lengthsEqual [m,s,e]) ==> (withinFromTo m s e) == (withinFromTo' m s e)

-- ??? create a custom generator for multiple lists?

lengthsEqual :: [Vec] -> Bool
lengthsEqual = (==1) . length . nub . map length

prop_wftNonincr :: Vec -> Vec -> Vec -> Property
prop_wftNonincr m s e = (lengthsEqual [m,s,e]) ==> and $ zipWith (>=) vs (tail vs)
  where vs = withinFromTo m s e

prop_wftWithin :: Vec -> Vec -> Vec -> Property
prop_wftWithin m s e = (lengthsEqual [m,s,e]) ==> all (<|= m) (withinFromTo m s e)

prop_wftLEq :: Vec -> Vec -> Vec -> Property
prop_wftLEq m s e = (lengthsEqual [m,s,e]) ==> all (<= s) (withinFromTo m s e)

prop_wftGEq :: Vec -> Vec -> Vec -> Property
prop_wftGEq m s e = (lengthsEqual [m,s,e]) ==> all (>= e) (withinFromTo m s e)
\end{code}
%endif
% $

\section{A walk in the park}

Now that we've descended into the depths, tamed multisets, slain the
|Vec| beast, and bludgeoned |withinFromTo| into submission, the climb
back out will be nothing but a scenic stroll!  We have only to wave
one of our magic vector functions, and Things will Happen. Our vector
functions are so magical, in fact, that we can use them to compute not
just multiset partitions, but a number of other interesting functions
as well.

\begin{listing}[htp]
\begin{code}
-- Integer partitions.

intPartitions :: Int -> [[Int]]
intPartitions = map (map head) . vPartitions . return

-- Power sets.

mPowSet :: MultiSet a -> [MultiSet a]
mPowSet (MS elts v) = map (MS elts) $ within v

powSet :: (Ord a) => [a] -> [[a]]
powSet = map toList . mPowSet . fromList

powSetUnord :: (Eq a) => [a] -> [[a]]
powSetUnord = map toList . mPowSet . fromListUnord

-- Partitions.

mPartitions :: MultiSet a -> [[MultiSet a]]
mPartitions (MS elts v) = map (map (MS elts)) $ vPartitions v

partitions :: (Ord a) => [a] -> [[[a]]]
partitions = map (map toList) . mPartitions . fromList

partitionsUnord :: (Eq a) => [a] -> [[[a]]]
partitionsUnord = map (map toList) . mPartitions . fromListUnord
\end{code}
\caption{Casually waving some magic vector functions around. \label{lst:applications}}
\end{listing}
% $

%if style == newcode
\begin{code}
prop_partitions :: (Ord a) => [a] -> Bool
prop_partitions s = normalize (partitions s) == normalize (msetPartitions s)

normalize :: (Ord a) => [[[a]]] -> [[[a]]]
normalize = sort . map (sort . map sort)

-- compute partition numbers using Euler's recurrence.
genPentagonals :: [Integer]
genPentagonals = map (\n -> n * (3*n - 1) `div` 2) ([1..] >>= \n -> [n,-n])

select :: [Integer] -> [Integer] -> [Integer]
select ns xs = select' 1 ns xs
    where select' _ [] _ = []
          select' _ _ [] = []
          select' i nns@(n:ns) xxs@(x:xs) 
              | i < n  = select' (i+1) nns xs
              | i ==n  = x : select' (i+1) ns xs

nextPartitions :: [Integer] -> [Integer]
nextPartitions ps = p' : ps 
    where p' = sum $ zipWith (*) (concat $ repeat [1,1,-1,-1]) (select genPentagonals ps)

partitionNums = map head $ iterate nextPartitions [1]

-- make sure the right number of integer partitions are generated.
prop_partitionNums :: Int -> Property
prop_partitionNums n = (n > 0) ==> 
    (genericLength $ intPartitions n) == (partitionNums !! n)
\end{code}
%endif
% $

For one, consider the problem of splitting an integer $n$ into a
multiset of smaller integers whose sum is $n$. Such integer
partitions can be seen as degenerate vector partitions. In
particular, the partitions of the integer $n$ correspond exactly to
partitions of the 1-dimensional vector $(n)$. This is implemented by
the function |intPartitions|, shown in
Listing~\ref{lst:applications}. For example, |intPartitions 5|
yields
\begin{quote}
  \eval{intPartitions 5}.
\end{quote}
It seems to work! As a check, we can also (rather inefficiently!)
compute the partition numbers~\cite{oeis_partitions} with |map (length
. intPartitions) [1..]|:
\begin{quote}
  \eval{map (length . intPartitions) $ [1..19]}...
\end{quote}
% $

In particular, |length $ intPartitions 30| yields \eval{length $
  intPartitions 30} almost instantly.  This is clearly an improvement
  over |length $ msetPartitions (replicate 30 1)|, which theoretically
  produces the same value but probably wouldn't finish before the heat
  death of the universe.
% $

Computing power sets is easy, too (Listing~\ref{lst:applications}),
since there is a natural bijection between unique subsets of a
multiset $(e,v)$ and the set of vectors $u \vleq v$. For example,
here's |powSet [2,2,3,3]|:

\begin{quote}
  \eval{powSet [2,2,3,3]}.
\end{quote}

Last (but not least!), finding partitions of a multiset (also in
Listing~\ref{lst:applications}) is now a simple matter of finding
partitions of the representative count vector. Here's |partitions
[2,2,3]|:

\begin{quote}
  \eval{partitions [2,2,3]}
\end{quote}

No repeated partitions here!

\section{Factorizations, reloaded}

We can finally use the |partitions| function for its original intended
purpose, to efficiently generate integer factorizations.  In fact, we
can use |powSet| to efficiently generate divisors, as
well. Listing~\ref{lst:factorizations} shows how. (This certainly is
not the fastest or most robust factoring code possible, but it isn't
really the point.  Any implementation of |factor| could be
combined with |partitions| and |powSet| in this way.)

\begin{listing}[htp]
\begin{code}
primes :: (Integral a) => [a]
primes = 2 : 3 : [ p | p <- [5,7..], isPrime p ]

isPrime :: (Integral a) => a -> Bool
isPrime n = all ((/= 0) . mod n) $ upToSqrt n primes

upToSqrt :: (Integral a) => a -> [a] -> [a]
upToSqrt n = takeWhile (<= (round . sqrt . fromIntegral $ n))

factor :: (Integral a) => a -> [a]
factor 1 = []
factor n = factor' n [] primes
  where 
    factor' n fs ps@(p:pt) 
      | (fromIntegral p > (sqrt . fromIntegral $ n)) = n:fs
      | (n `mod` p == 0) = factor' (n `div` p) (p:fs) ps
      | otherwise = factor' n fs pt

divisors :: (Integral a) => a -> [a]
divisors = map product . powSet . factor 

factorizations :: (Integral a) => a -> [[a]]
factorizations = map (map product) . partitions . factor
\end{code}
\caption{Factorizations as multiset
  partitions. \label{lst:factorizations}}
\end{listing}
% $

For example, we can compute the factorizations of $30$:
\begin{quote}
  \eval{factorizations 30}.
\end{quote}
But that's easy, since $30$ has no repeated prime factors.  Let's try
something like $24$:
\begin{quote}
  \eval{factorizations 24}.
\end{quote}
Or how about |length $ factorizations 1073741824|?
\begin{quote}
  \eval{length $ factorizations 1073741824}
\end{quote}
Funny, I think I remember seeing that number somewhere recently\dots

As a final comment, the reader may note that we are actually taking
advantage of the fact that multisets and vectors also correspond
to monomials. For example, the multiset $\{1,1,2,3,3,3,3\}$
corresponds to the monomial $x_1^2 x_2 x_3^4$. Positive integers can
be viewed as monomials over the primes, $2^{\alpha_2} 3^{\alpha_3}
5^{\alpha_5} \dots$, which leads directly to the code in
Listing~\ref{lst:factorizations}.  Writing a function to compute
general monomial factorizations is left as an exercise for the
reader. 

\section{Acknowledgments}

Thanks to David Amos and Wouter Swierstra for their helpful comments
on a first draft of this article.

\section{About the author}

Brent Yorgey has a BA in Computer Science from Williams College in
Massachusetts, USA, and hopes to begin studying for a PhD in the fall
of 2008.  He spends entirely too much time chatting in \#haskell (as
byorgey) when he should be doing other things (such as editing this
article).

\bibliography{Partitions}

\end{document}
