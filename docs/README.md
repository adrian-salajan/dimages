# dimages
denotational image lib
 | [source code here](https://github.com/adrian-salajan/dimages)

### Intro
This is a self attempt to better understand FP concepts like functors, applicatives, monads, etc.
visually, by implementing them on images.

This project and explanations are not meant to be an introduction to these concepts, but a try to understand them in a more intuitive way by applying them on visual images.

This was inspired by [Conal Elliott - Denotational Design: From Meanings To Programs](https://www.youtube.com/watch?v=bmKYiUOEo2A)

### Defining images
This is very well explained in the presentation.

TLDR we can say an Image is a function from location (x, y coordinates)
to some value A. In order to have them rendered on the screen we need visual images which will be a function from location to a Color.

`class Image[A](val im: Loc => A)` where A is of type `Color`

### Functor

[bird]:https://github.com/adrian-salajan/dimages/blob/master/src/main/resources/bird.png?raw=true
[swapColors]:https://github.com/adrian-salajan/dimages/blob/master/png/functor/replaceColors.png?raw=true
[grayscale]:https://github.com/adrian-salajan/dimages/blob/master/png/functor/grayscale.png?raw=true
[invert]:https://github.com/adrian-salajan/dimages/blob/master/png/functor/invert.png?raw=true
[threshold]:https://github.com/adrian-salajan/dimages/blob/master/png/functor/threshold.png?raw=true
[ignore]:https://github.com/adrian-salajan/dimages/blob/master/png/functor/ignoreInputReplaceGreen.png?raw=true

A functor is composed of 1 thing

1. A map function that takes another function `f: A => B` as argument, which is applied on the `A` value inside a `F[A]` context. Map will give back a `F[B]`. This allows transformation of the value in the context,
while leaving the context untouched. Of course, map needs to apply `f` on some `F[A]`, so it has this argument as well:
`def map(a: F[A], f: A => B): F[B]`

#### Functor on images
So having implemented functor for images (check the code and presentation), what does this mean?

It means that given a source image, we can transform it into another, given a function `Color => Color`.

Notice that the function takes the original source Color and gives us another color based on that. So we can't create ANY image,
but only derived ones from our original.
If we want we can ignore the original Color but that limits us extremely, being then only able to return a constant Color.

So we take the image and with the help of `map` we transform it, pixel by pixel (since we
apply f to a Color, which is the value at a Location)

original:

![alt text][bird]

grayscale colors:

![grayscale][grayscale]

Replace colors - that specific dark red with green:

![swap colors][swapColors]

invert colors:

![invert colors][invert]

threshold - if brightness over some value V then put White else put Black:

![threshold][threshold]

ignore the input color and return Green:

![ignore][ignore]

The transformations above are just one application of map with a function `Color => Color`,
but there is nothing stopping us to transform color into something else, 
like `Color => Boolean`, it's just that we can't render booleans to screen (for our visualization purpose  it is not helpful), so we need
a second map transformation `Boolean => Color`, but then again due to function composition
we can compose the functions into a single one `Color => Color`

### Applicative

An applicative is composed of 2 things:

1. A function which can wrap any value `A` with the context `F`. `pure(a: A): F[A]`. So it must be that `pure` knows what the `F` context means. Very important.
2. One of the two functions which are equivalent to each other (can rewrite one in terms of the other + `pure`):
    * `map2(a: F[A], b: F[B], f: (A, B) => C): F[C]`
    * `apply(a: F[A], f: F[A => B]): F[B]`
    
#### Map2
`map2(a: F[A], b: F[B], f: (A, B) => C): F[C]`

This says it can merge 2 contexts. The `f` function knows how to merge A and B into a new value C,
but `map2` knows (just like `pure`) about what the context means, so it actually knows to merge 2 contexts (of the same type) together.
We can replace the word `context` with `effect` => `map2` knows how to merge 2 effects.

#### Apply
`apply(a: F[A], f: F[A => B]): F[B]`

This is more tricky to understand intuitively, but given a function/program wrapped in the context/effect `F`,
we can run the program with the value in `a: F[A]`.
Not to be confused with the functor's `map(a: F[A], f: A => B): F[B]` - there is an extra `F` over `f` in apply.

Because `apply` also gets as input 2 `F`s and returns only one it means that it also knows how to merge these `F`s (just like `map2`)

#### Map2 vs Apply
So what is the difference? Formally none because we can write one in terms of the other + `pure`

<pre>
def map2[F[_], A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): F[C] = {
  val one = ap(pure(f curried))(fa)
  ap(one)(fb)
}

def apply[A, B](ff: F[A => B])(fa: F[A]): F[B] =
  map2(ff, fa)((f, a) => f(a))
</pre>

So it's clear that both `map2` and `apply` know how to merge F contexts/effects, but what about the difference in signatures?

`ff: F[A => B]` from `apply` is the partial application of `f: (A, B) => C` from `map2` with `a: F[A]`,
it kinda holds a `F[A]` inside.

#### Applicative is also a Functor
<pre>
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      ap(pure(f))(fa)
</pre>

But functor is not an applicative, functor does not have `pure` so it does not know anything about what `F` means.

#### Applicative on images
[cy]:https://github.com/adrian-salajan/dimages/blob/master/src/main/resources/crayons.png?raw=true
[max]:https://github.com/adrian-salajan/dimages/blob/master/png/applicative/max.png?raw=true
[sw]:https://github.com/adrian-salajan/dimages/blob/master/png/applicative/seeThroughWhite.png?raw=true
[disolve]:https://github.com/adrian-salajan/dimages/blob/master/png/applicative/disolve.png?raw=true

Given F is an Image it means we can combine any 2 images.

Original image A:

![original image A][bird]

Original image B:

![original image B][cy]

Max between A and B

![max][max]

See through white

![sw][sw]

Disolve (creates a new images randomly taking color from A or B)

![disolve][disolve]



### Monad
[rotate]:https://github.com/adrian-salajan/dimages/blob/master/png/monad/rotate.png?raw=true
[swirl]:https://github.com/adrian-salajan/dimages/blob/master/png/monad/swirl.png?raw=true
[whiteStripes]:https://github.com/adrian-salajan/dimages/blob/master/png/monad/whiteStripes.png?raw=true
[bands]:https://github.com/adrian-salajan/dimages/blob/master/png/monad/bands.png?raw=true
[combine]:https://github.com/adrian-salajan/dimages/blob/master/png/monad/combine.png?raw=true

A monad is composed of 2 things:
1. A function which can wrap any value `A` with the context `F`:  `pure(a: A): F[A]` (A monad is also an applicative)
2. either of the functions:
 * `flatMap(fa: F[A], f: a => F[B]): F[B]`
 * map + `flatten(fa: F[F[A]]): F[A]`

#### FlatMap
`flatMap(fa: F[A], f: a => F[B]): F[B]`

This takes a value in a context and a function `f` from a value to a value in a context and returns its result.
It seems a bit strange that `f` returns a `F[B]` not just a `B` (as the functor's map), so unlike the functor this is not
just a simple transformation of `F[A]`, it's a transformation + a new `F` context effect.

If applicative lets us merge effects then `flatMap` provides the ability to chain new effects having the original wrapped value as input.

#### Flatten
`flatten(fa: F[F[A]]): F[A]`

This is merging effects. The difference from applicative, these are "serial" effects `F[F[A]]`
while applicative merges parallel effects in `map2(a: F[A], b: F[B], f: (A, B) => C): F[C]`

#### FlatMap vs Flatten

These are equivalent

<pre>
def flatMap[F[_], A, B](fa: F[A])(f: A => F[B]): F[B] = flatten(map(fa, f))

def flatten[F[_], A](ffa: F[F[A]]): F[A] = flatMap(ffa)(a => a)
</pre>

#### Monad on Images

Before I implemented Monad on Images it was very unclear how it would be useful.

with Image[Color] flapMap looks as
`flatMap(fa: Image[Color], f: color => Image[Color]): Image[Color]`
so given a color image, from each color build an image and somehow return a single image back? Well apparently yes (all the "intermediary" images are flattened).
<pre>
def flatMap[A, B](fa: Image[A])(f: A => Image[B]): Image[B] = new Image[B] ({
      loc =>
        val img: Image[B] = f(fa.im(loc))
        img.im(loc)
})
</pre>

The implementation returns a new image, which for each pixel/Location returns the color of f's Image[B] at that location,
and since f has the power to build new images, it can decide each location what color it has, it can be the original input color or completely different.

Functor and Applicative let us modify images by doing transformations only based on their colors,
while Monad provides the ability to do transformations based on both color and location (or color at a location).
This extra power comes through `f: Color => Image[Color]` and monads flattening of effects. 

This finally gives the power of doing geometric transformations on images, because we can access through location different parts of the image.

Geometric transformations: rotate + scale + translate

![rotate][rotate]

Geometric transformation: swirl

![swirl][swirl]

Combine two images based on location (middle x axis)

![combine][combine]

Return different colors based on location (every X pixels return white instead of the original color)

![whiteStripes][whiteStripes]

Return colors of the same image but from other locations

![bands][bands]


### Comonad
??? tbd















