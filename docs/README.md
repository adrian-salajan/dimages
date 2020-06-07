# dimages
denotational image lib

### Intro
This is an (self) attempt to better understand FP/math concepts like functors, applicatives, monads, etc.
visually by implementing them on images.

This was inspired by [Conal Elliott - Denotational Design: From Meanings To Programs](https://www.youtube.com/watch?v=bmKYiUOEo2A)

### Defining images
Whis is very well explained in the presentation, TLDR we can say an Image is a function from location (x, y coordinates)
to some value A. In order to have them rendered on the screen we need visual images which will be a function from location to a Color.

`class Image[A](val im: Loc => A)` where A is of type `Color`

### Functor

[bird]: https://github.com/adrian-salajan/dimages/blob/master/src/main/resources/bird.png?raw=true "Logo Title Text 2"

A functor is composed of 2 or 3 things, depending how you see it.
1. A context. In scala the context is a higher-kinded type with one whole `F[_]`
2. A generic value inside the context: the `A` in `F[A]`
3. A map function that takes another function `f: A => B` which is applied on the `A` value giving back a `F[B]`. This allows transformation of the value in the context,
while leaving the context untouched. Of course, map needs to apply `f` on some `F[A]`, so it has this argument as well:
`def map(a: F[A], f: A => B): F[B]`

So having implemented one for images (check the code and presentation), what does this mean?

It means that we can transform one image into another given a function Color => Color.
Notice that the function takes the old Color and gives us another color based on that. So we can't create ANY images,
only derived ones from our original. Optionally we can ignore the original images but that not allow us to do much, except return a constant solid Color.

So we take the bird image and with the help of `map` we transform it, pixel by pixel (since we
apply f to a Color, which is the value at a Location)

original: ![alt text][bird]






