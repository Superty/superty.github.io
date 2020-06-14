
## Let `X, Y ~ D`; if `X + Y` and `X - Y` are independent, show `D` is a Gaussian

#### Or: how to waste the greater part of a weekend proving something in a completely obtuse and unenlightening but 'elementary' way

Let `X` and `Y` be iid random variables drawn from a distribution `D`. We want to show that if `X + Y` and `X - Y` are independent, then `D` is a Gaussian.

Let `f`, `g`, and `h` be the probability densities of `X`, `X + Y`, and `X - Y` respectively, fuctions from the reals to the non-negative reals with total area under the curve equal to 1. (Of course, `f` is also the density of `Y`). We will assume that `f` is continuous.

Let the probability density of the joint distribution of `X + Y` and `X - Y` be `d(a, b)`. But `d(x + y, x - y) = f(x) f(y)` since fixing the values of `X + Y` and `X - Y` totally determines the values of `X` and `Y`.

Now since `X + Y` and `X - Y` are independent we have

`f(x) f(y) = g(x + y) h(x - y)` ---- (1).

First, let's prove that `f` can never be zero. Suppose `f(a)` is zero, this gives us four lines `x = a`, `y = a` that must be zero in `D^2`. 

We want to express `g` and `h` in terms of `f`.

Setting `y = x`, we obtain `g(2x) h(0) = f(x)^2`. (2)

Setting `y = -x`, we obtain `h(2x) g(0) = f(x) f(-x)`. (3)

Setting `x = y = 0`, we obtain `f(0)^2 = g(0) h(0)`. (4)

Dividing the (2) and (3) by (4), we obtain `G(2x) = F(x)^2` and `H(2x) = F(x) F(-x)` where `F(x) = f(x)/f(0)`, `G(x) = g(x)/g(0)`, `H(x) = h(x)/h(0)`. Note that `F(0) = G(0) = H(0) = 1`.

Substituting `a = (x + y)/2` and `b = (x - y)/2` in (1) and dividing by (4), we get `G(2a) H(2b) = F(a + b) F(a - b)`. Substituting our equations for `G` and `H`, we get

`F(a)^2 F(b) F(-b) = F(a + b) F(a - b)` ---- (5).

At this point I wasted a lot of time trying to prove that `F` is even, i.e., `F(x) = F(-x)`, because once we show that the above equations simplify nicely and we can continue making some substitutions until we get the result. If I were to give up, now would've been the time, knowing that the proof along these lines was not going to be nice. Alas, I was too stubborn.

I got as far as `F(x) = F(-x) r^x` for some `r`, but could not prove `r = 1`. After a long time I realized that `e^x` is a solution to (5), ruled out only because it's not L1 (i.e., the "total probability" is infinite). After that I spent a whole lot of time trying to use the fact that the probability densities `f`, `g`, `h`, need to be L1, but this led nowhere. The above relation with `r` plus being L1 are not enough to prove anything useful, as those two conditions are not sufficient to enforce (5). So I went back to (5) and tried to extract more properties from it that I could use in the analysis.

At this point I ended up just proving it directly. It took embarassingly long for me to realize that I had actually solved it, and that `F` need not even be even (ha), and that `F(x) = exp(-x^2 - x)` is a solution, for example. It was then that I remembered that the Gaussian could be centered somewhere other than zero, so `F` need not be even.

Now we can make some substitutions and prove the result. What we need to prove is that `F(x)` is of the form `exp(px^2 + qx)` for real `p`, `q` and `p < 0`. This is the form of the Gaussian density function after normalizing by the value at zero.

As is usual when we want to prove that something is an exponential, we will try to show everything is a power of `F(1)`. Substituting `b = 1` in (5), we get

`F(a)^2 F(1) F(-1) = F(a + 1) F(a - 1)` ---- (6).

Okay, so let `c = F(1)` and `c*t = F(-1)`. Substituting and rearranging, we get

`t * c^2 * F(a)/F(a - 1) = F(a + 1) / F(a)` ---- (6).

So the ratio between successive integer values gets gets multiplied by `t c^2` each time. So we have

`F(1) / F(0) = c`,

`F(2) / F(1) = t c^3`,

`F(3) / F(2) = t^2 c^5`,

`F(4) / F(3) = t^3 c^7`,

so, since the sum of the first `n` odd numbers is `n^2` and the sum of the first `n - 1` naturals is `n(n - 1)/2`, we can see that `F(n) = t^(n(n - 1)/2) c^(n^2)`. Since both `c` and `t` are positive, we can rewrite them as `c = e^k` and `t = e^l` where `k` and `l` are reals, to obtain `F(n) = exp(n^2 (k + l/2) - n (l/2))`. Setting `p = k + l/2` and `q = -l/2`, we get

`F(n) = exp(p n^2 + q n)` for all naturals `n` ---- (7).

We can also support division by two. Substituting `b = a/2` in (5), we get

`F(a)^3 F(-a) = F(2a)`  ---- (8).

`F(-a)^3 F(a) = F(-2a)` ---- (9).

Dividing (8) by (9), we obtain

`F(a)/F(-a)  = sqrt(F(2a)/F(-2a))` ---- (10).

For any natural `n`, we know `F(n) / F(-n) = exp(2qn)`. Using (10), `F(n/2) / F(-n/2) = sqrt(exp(2qn)) = exp(2q(n/2))`. We can prove this for any `x = n/2^k` for natural `k`, by induction. Using this and (8), we have

`F(x/2)^4 = F(x) exp(qx)` for all `x = n/2^k` ---- (11).

From this, and (7), we have

```
F(n/2)^4 = exp(p n^2 + qn) exp(qn)
         = exp(p n^2 + 2qn)
F(n/2)   = exp(p (n/2)^2 + q (n/2)).
```

Again, by induction we can prove `F(x) = exp(px^2 + qx)` for all `x = n/2^k`. Now since we can represent any real number aribtrarily closely with a number of this form, this is sufficient to establish that this must hold for all real `x`, assuming `F` is continuous. (formally, there exists a Cauchy sequence consisting of only numbers of this form for any real). So we have

`F(x) = exp(px^2 + qx)` for all reals `x` ---- (12).

If `p >= 0`, then `F` would not be L1 and hence could not be a probability density. Therefore, `p < 0`, as desired.