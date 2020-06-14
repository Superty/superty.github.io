After writing this up I realized that I needed to assume the probability densities are continuous. Oh well.

Let `X` and `Y` be iid random variables drawn from a distribution `D`. We want to show that if `A = X + Y` and `B = X - Y` are independent, then `D` is a Gaussian.

Let `f`, `g`, and `h` be the probability densities of `X`, `X + Y`, and `X - Y` respectively. (of course, `f` is also the density of `Y`).

Let the probability density of the joint distribution of `X + Y` and `X - Y` be `d(a, b)`. But `d(x + y, x - y) = f(x) f(y)` since fixing the values of `X + Y` and `X - Y` totally determines the values of `X` and `Y`.

Now since `X + Y` and `X - Y` are independent we have

`f(x) f(y) = g(x + y) h(x - y)` ---- (1)

We want to express `g` and `h` in terms of `f`.

Setting `y = x`, we obtain `g(2x) h(0) = f(x)^2`. (2)

Setting `y = -x`, we obtain `h(2x) g(0) = f(x) f(-x)`. (3)

Setting `x = y = 0`, we obtain `f(0)^2 = g(0) h(0)`. (4)

Dividing the (2) and (3) by (4), we obtain `G(2x) = F(x)^2` and `H(2x) = F(x) F(-x)` where `F(x) = f(x)/f(0)`, `G(x) = g(x)/g(0)`, `H(x) = h(x)/h(0)`. Note that `F(0) = G(0) = H(0) = 1`.

Substituting `a = (x + y)/2` and `b = (x - y)/2` in (1) and dividing by (4), we get `G(2a) H(2b) = F(a + b) F(a - b)`. Substituting our equations for `G` and `H`, we get

`F(a)^2 F(b) F(-b) = F(a + b) F(a - b)` ---- (5).

We want to prove that `F` is even, i.e., `F(x) = F(-x)`, because once we show that the above equations simplifies nicely into

`F(a)^2 F(b)^2 = F(a + b) F(a - b)` ---- (6).

At that point, we will be able to continue making some substitutions until we get the result, as follows. Per usual with solving functional equations whose solutions we want to be exponentials, we'll express everything in terms of `F(1)`. Let's call `F(1)` as `c`.

Setting `b = 1` we get `F(a)^2 c^2 = F(a + 1) F(a - 1)`. In other words, `F(a + 1) / F(a) = c^2 F(a) / F(a - 1)`, i.e., the ratio between successive natural number values increases by `c^2` every time. The first ratio, `F(1)/F(0)` is `c` by definition. Each time it increases by two, so the ratios are `c^1, c^3, c^5, ...` and the values of `F` at `0, 1, 2 ...` are `c^1, c^4, c^9, ...` so we have `F(n) = c^(n^2)` for naturals `n`.

We can also support division by two. Setting `b = a` in (6) we obtain `F(a)^4 = F(2a)`, which is to say `F(a/2) = F(a)^(1/4)`. This along with the evenness of `F` is enough to get `F(x) = c^(x^2)` for all `x` of the form `i/2^k` for integers `i` and `k`.

Now, we need `F` to be continuous for this next part. I never learnt analysis, but from what I understood, any real number can be described by a Cauchy sequence consisting of only rationals of the form `i/2^k` so determining `F` at these points suffices to determine it for all real values. (stole this part from https://math.stackexchange.com/questions/505/can-there-be-two-distinct-continuous-functions-that-are-equal-at-all-rationals)

But it's left to prove that `F` is even if it satisfies `F(a)^2 F(b) F(-b) = F(a + b) F(a - b)`. (5)

We want to relate `F(x) and F(-x)`. Changing `b` to `-b` is useless in this equation so we are unable to use the `F(-b)` term to do this. But we _can_ use the `F(a - b)` term.

Swapping `a` and `b`, we get `F(b)^2 F(a) F(-a) = F(a + b) F(b - a)`. Dividing (5) by this we get `F(a) F(-b) / (F(b) F(-a)) = F(a - b) / F(b - a)`. Notice a pattern? Let `r(x) = F(x)/F(-x)`. Then by rearranging and setting `p = a - b` we have:

`r(p + b) = r(p) r(b)` ---- (7).

So `r` is an exponential! (proof omitted) Eventually we want to prove that `r` is always one, but this is a start. So we know that

`F(x) = F(-x) t^x` ---- (8).

Frustratingly, I couldn't find any elementary way of proceeding from here. It was only later that I realized `F(x) = e^x` satisfies (5), `F(a)^2 F(b) F(-b) = F(a + b) F(a - b)`. (in that case, `t = e^2`)

Of course, `e^x` is not a valid probability density. So we need to bring in more properties of the density to get rid of this pesky `e^x`. The total area under the curve must be finite and non-zero.

All integrals mentioned are definite integrals from -infinity to +infinity. We have that `∫F(x)dx`, `∫F(x)F(-x)dx`, and `∫F(x)^2dx` are finite and strictly positive since total probability for `f`, `g`, and `h` is finite and positive (but we can't say the integrals above have to be one since we normalized by `f(0)`, `g(0)`, `h(0)`).

For now let's assume that `F(x)` is also strictly positive. (proof that it can't be zero isn't cslear but I think it can be done.)



```
∫ F(x) dx
= ∫ F(-x) t^-x dx       from (8)
= ∫ F(x) t^(x) dx     (x -> -x and flipping the limits)
= ∫ F(-x) t^-x t^(x) dx
= ∫ F(-x) t^(-x) dx   

```