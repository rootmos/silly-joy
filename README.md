# silly-joy
[![Build, test and push image](https://github.com/rootmos/silly-joy/actions/workflows/build-test-push.yaml/badge.svg)](https://github.com/rootmos/silly-joy/actions/workflows/build-test-push.yaml)

`silly-joy` is an interpreter for the concatenative programming
language Joy, implemented in Haskell using
[extensible-effects](https://hackage.haskell.org/package/extensible-effects).

## What's Joy?
Here are some resources:
* *Mathematical foundations of Joy* by *Manfred von Thun*
  ([archived here](https://web.archive.org/web/20111007025556/http://www.latrobe.edu.au/phimvt/joy/j02maf.html), the paper that sparked this project)
* On [Wikipedia](https://en.wikipedia.org/wiki/Joy_(programming_language))
* [Official page](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)
* Over at [http://www.concatenative.org](http://www.concatenative.org/wiki/view/Joy)

## Try it out!
Simplest way to try it out is by using Docker:
```
docker run -it rootmos/silly-joy --tui
```
You'll be greeted by a terminal UI with the REPL as well as
a window showing the current stack. Very convenient when learning stack-based
languages!

![demo](../master/demo.gif)


## Example session

```
> 1 2 +
> :stack
3
> 12 *
> print
36
> fact := [[pop 0 =] [pop pop 1] [ [dup 1 -] dip dup i *] ifte] dup i
> 10 fact
> :st
3628800
> ["foo" "bar" strcat] [strlen] b
> dup print
6
> 1 swap - print
-5
> even := 2 % 0 =
> odd := even not
> 3 odd 2 even and print
true
> prime := [dup even [3 <] dip or] [2 =] [prime_trial_division] ifte
> prime_trial_division := 3 [[pop % null] [pop =] [[2 +] dip x] ifte] x
> [2 3 4 5 6 7 8 9 10 11 12 13] [prime] map print
[true true false true false true false false false true false true]
> sum := 0 [+] fold
> div-3-5 := [dup [3 % null] dip 5 % null or] filter
> up-to := pred [[]] [cons] primrec
> 10 up-to div-3-5 sum print
23
> next-fib := dup [+] dip swap
> add-if-even := [even] [dup [rotate] dip + rotate] [] ifte
> 0 1 1 [100 >] [pop pop] [next-fib add-if-even] [i] genrec print
188
```
