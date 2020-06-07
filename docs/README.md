# dimages
denotational image lib

#### Intro
This is a self attempt to better understand FP concepts like functors, applicatives, monads, etc.
visually, by implementing them on images.

This project and explanations are not meant to be an introduction to these concepts, but a try to understand them in a more intuitive way by applying them on visual images.

This was inspired by [Conal Elliott - Denotational Design: From Meanings To Programs](https://www.youtube.com/watch?v=bmKYiUOEo2A)

#### Defining images
This is very well explained in the presentation.

TLDR we can say an Image is a function from location (x, y coordinates)
to some value A. In order to have them rendered on the screen we need visual images which will be a function from location to a Color.

`class Image[A](val im: Loc => A)` where A is of type `Color`

#### Functor

[bird]:https://github.com/adrian-salajan/dimages/blob/master/src/main/resources/bird.png?raw=true
[swapColors]:https://github.com/adrian-salajan/dimages/blob/master/png/functor/replaceColors.png?raw=true
[grayscale]:https://github.com/adrian-salajan/dimages/blob/master/png/functor/grayscale.png?raw=true
[invert]:https://github.com/adrian-salajan/dimages/blob/master/png/functor/invert.png?raw=true
[threshold]:https://github.com/adrian-salajan/dimages/blob/master/png/functor/threshold.png?raw=true
[ignore]:https://github.com/adrian-salajan/dimages/blob/master/png/functor/ignoreInputReplaceGreen.png?raw=true

A functor is composed of 2 things
1. A context/wrapper over a generic value A. In scala the context is a one-argument type constructor `F[A]`
2. A map function that takes another function `f: A => B` as argument, which is applied on the `A` value inside the context Map will give back a `F[B]`. This allows transformation of the value in the context,
while leaving the context untouched. Of course, map needs to apply `f` on some `F[A]`, so it has this argument as well:
`def map(a: F[A], f: A => B): F[B]`

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

swap colors:

![swap colors][swapColors]

invert colors:

![invert colors][invert]

threshold:

![threshold][threshold]

ignore the input color and return Green:

![ignore][ignore]

The transformations above are just one application of map with a function `Color => Color`,
but there is nothing stopping us to transform color into something else, 
like `Color => Boolean`, it's just that we can't render booleans to screen (for our visualization purpose  it is not helpful), so we need
a second map transformation `Boolean => Color`, but then again due to function composition
we can compose the functions into a single one `Color => Color`

#### Applicative

#### Monad






