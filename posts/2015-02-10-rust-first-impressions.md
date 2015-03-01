---
title: Rust: First Impressions
---

I recently decided to give the [Rust programming language][1] a try. Although
Rust had crossed my radar a few times before, I'd never gotten much further
than *Hello World!*. The language has undergone a lot of change over the last
few years, but things appear to finally be slowing down for its upcoming 1.0
release.

<!--more-->

### The Project

My latest encounter with Rust began when I set out to write a ray tracer
to get some experience with graphics programming. I initially considered a few
different languages. 

As a [GitHub search for "raytracer"][8] shows, the typical language for the job
is C++. And I think it makes sense. You have to recursively trace about
a million rays of light through the scene, so you need something fast (or
a book to read while you wait for your Ruby ray tracer to finish executing).
It's also a problem that maps well to the OOP paradigm. The "scene" will
naturally have different sorts of shapes (e.g., planes, spheres, cubes), each
with a mix of shared and unique behavior that can be modeled well with inheritance.
Finally, bindings to a decent graphics library are a must, which C++ also
accommodates.

It turns out that Rust also nicely meets these requirements. It's fast, more or
less supports OOP via its trait system, and has at least [one nice graphics
library][2]. So I gave it a try. What follows are my impressions of the
language as a Rust novice (so take my praises and criticisms with a grain of
salt).

### The Good

I had a pretty great experience with Rust, so there was a lot to like.

#### Ownership

Ownership is probably the most unique of Rust's features. It's also what
I struggled the most with when it came to getting my code to compile. 

The idea is that since Rust code isn't garbage collected, and the programmer
isn't managing memory, the compiler needs some way to keep track of the
*lifetime* of your data so that it can allocate and deallocate memory for you. In
theory, this is awesome. You get the performance benefits of managing your
own memory with all the safety of a garbage collected language.

In order for that to work, the compiler enforces a set of rules for ownership
and borrowing of pointers and other resources. Resources are said to be *owned*
by their variables. They can be borrowed by another variable, but just like
a bike you lend out, the owner can't use it while it's being borrowed. 

It does get a bit complicated and I'm not yet experienced enough to describe it
in more detail, but I'm really liking the ownership system so far. I'll qualify
that by saying, given the choice between manually managing my memory or letting
Rust do it, I'd choose Rust. Comparing Rust to a GC-ed language isn't quite
fair, as Rust is presumably being used because GC isn't an option.

While the "borrow checker" has been the biggest hindrance in getting my Rust
code to compile, it (theoretically) protects you from a whole class of
memory issues (like leaks and dangling pointers). Based on my experience so
far, I do think fighting the borrow checker is vastly preferable to hunting
down the cause of a segfault in a large C/C++ codebase.

#### Cargo

[Cargo][3], Rust's package manager, has been a pleasure to work with. The best
kind of package managers are the ones that just work, and so far that's been
for me with Cargo. The tool is reminiscent in many ways of Ruby's [Bundler][6],
and it turned out that wasn't a coincidence as [both were created by the same
people][4].

Cargo is similar to Bundler in that it provides a simple way to list out your
build dependencies---each from either [crates.io][3] (the rubygems.org
equivalent), a GitHub repo, or from the local filesystem. It then retrieves and
builds your dependencies, saving the version numbers or git commit SHAs in the
`Cargo.lock` file. With that file checked in to source control, it's easy to
get repeatable builds using *exactly* those same dependencies, so you don't suffer
from issues with others trying to build your project with slightly different
versions of the dependencies. Pretty neat.

**Cargo.toml**
```
[dependencies.image]
git = "https://github.com/PistonDevelopers/image"
```

**Cargo.lock**
```
[[package]]
name = "image"
version = "0.2.0-alpha.10"
source = "git+https://github.com/PistonDevelopers/image#5b589d98e53da920a28dbed8b3ea83452280cdd2"
dependencies = [
 "num 0.1.12 (git+https://github.com/rust-lang/num)",
]
```

#### Support for Testing

Rust has some really cool built-in support for unit testing. This is great
because every roadblock to writing tests makes them that much less likely to get
written. In Rust, tests can be tagged with a `#[test]` attribute and added
directly to the source file:

```rust
impl Vector {
    pub fn length(&self) -> f64 {
        (Float::powf(self.x, 2.0) + Float::powf(self.y, 2.0) + Float::powf(self.z, 2.0)).sqrt()
    }
}

#[test]
fn test_vector_length() {
    let subject = Vector{ x: 1.0, y: 1.0, z: 1.0 };
    assert_eq!(subject.length(), (3.0 as f64).sqrt());
}
```

Once you've set up your project with Cargo, tests are easy to run:

```bash
$ cargo test
Compiling rustray v0.0.1 (file:///Users/ndreynolds/repos/rustray)
Running target/rustray-7f062fec34db6a8c

running 1 test
test test_vector_length ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured
```

#### Safety vs. Flexibility

Every language seems to make its own trade-offs between safety (protecting the
programmer from him or herself) and flexibility (which allows you to do useful
things). The protections that prevent you from unsafe memory access or type
errors always come at the cost of some flexibility. 

With C, you're free to do whatever you want to your bits. If GCC doesn't like
the types you've assigned, you can always cast your way out of it---future
segmentation faults notwithstanding. Haskell is on the other end of the
spectrum. Its rigid type system protects you from all sorts of silly mistakes,
but its functional purity makes normally simple things like printing to stdout
or getting a random number considerably more difficult. 

Rust seems to aim for somewhere in the middle of the two extremes. I think one
of the clear goals of Rust is to provide a safer choice for the sort of systems
programming that C or C++ is typically used for. One way to achieve safety is
to prevent the programmer from doing anything useful, but I don't think that's
the case here. 

Rust encourages safety by providing reasonable defaults, but there's usually
an escape hatch if you need to do something not-so-safe (which can happen every
now and then in the real world). By default, variable bindings are immutable,
which means any mutation must be made explicit. 

```rust
// Variable bindings are immutable by default:
let x = 42;
// x += 1;   <-- Can't do this

// But if you later decide you need a mutable binding, you have that option:
let mut y = 42;
y += 1;        
```

Rust also forgoes null pointers and encourages an [Option][9] type (similar to
Swift's optional and Haskell's Maybe monad) to represent a value that may be
absent. In languages like Java, null pointers often mean null pointer
exceptions. With an optional type, the compiler can enforce that you handle
both scenarios (i.e., value presence *and* absence). In my project, one example
of this was checking for an intersection when tracing a ray. An intersection
only occurs if there's an object in the ray's traced path, so it may be absent:

```rust
// Matches on an Option<Intersection>
match self.find_intersection(ray) {
  Some(isect) => self.get_shade(isect, depth),
  None        => Pixel::black()
}

// You can also use `unwrap`, which fails if the value is None.
let isect : Intersection = self.find_intersection(ray).unwrap();
```

### The Bad

There were a few things I didn't like about Rust. I'm sure most of these are just
due to my inexperience with the language or Rust being a relatively new language.

#### Unstable APIs

Rust is still in alpha and much of the standard API is currently marked as
unstable. This means that---for now at least---every time you upgrade to the
latest nightly, there's a pretty good chance that your code will no longer
compile. I've gone through this a few times with my ray tracer and spent a few
hours moving to new APIs each time (for example, `serialize` to
`rustc-serialize`). The other issue this presents is that a lot of the
3rd-party tutorials and example code out there won't compile with the latest
compiler.

The good news is that this is supposed to change soon with the [1.0 release][5].

#### Steep Learning Curve

One of the trade-offs made in exchange for all the nice things about Rust is
that it takes a while to get off the ground. For me at least, it's not
a language like Python or Ruby that you can start making useful things with on
your first day. Using Rust effectively is predicated on having a good
understanding of the language. As a beginner, I struggled with compiler error
messages that seemed cryptic taken out of context (mostly due to borrowing or
ownership problems), but once I'd read more of the official book, they did
start to make more sense. Moreover, like Haskell, Rust has a powerful type
system and gives you a lot of rope to hang yourself with. I got stuck a few
times when trying to define and deserialize a polymorphic "shape" type.

#### Lifetime Syntax

I think Rust generally has pretty nice syntax. I like its Ruby-style closures
(`|x| x * x`). While I like the ownership paradigm, I'm not sold on the syntax
for defining lifetimes. The terse `a`, `b`, `c` identifiers combined with the
borrowed and mutable symbols can start to feel like reading and writing
hieroglyphics:

```rust
fn foobar<'a, 'b>(x1: &mut Foo<'a>, y1: &'a Bar, x2: &mut Foo<'b>, y2: &'b Bar) {
  // ...
}
```

Sure, I imagine this will get easier to read with time, but I wouldn't mind some
Java-style verbosity here.

### Wrapping Up

I was going to add a "The Ugly" section to complete the trifecta, but my
experience with Rust has actually been a really pleasant one. I'm sure the API
stability and documentation will only improve with time. 

With that said, I don't think Rust is a good general purpose programming
language. I don't plan to use Rust for web programming any time soon. When it
comes to rapid development, I think the extra cognitive overhead in dealing
with ownership and complexities like `String` vs. `&str` make it hard sell
against something like Ruby. But for problems in Rust's wheelhouse---like
system tools or operating systems or media decoders---where you need speed and
safety, it's a really great tool to have.

[1]: http://rust-lang.org
[2]: https://github.com/PistonDevelopers/image
[3]: https://crates.io/
[4]: https://mail.mozilla.org/pipermail/rust-dev/2014-March/009090.html
[5]: http://blog.rust-lang.org/2015/02/13/Final-1.0-timeline.html
[6]: http://bundler.io/
[7]: http://github.com/ndreynolds/rustray
[8]: https://github.com/search?utf8=%E2%9C%93&q=raytracer
[9]: http://doc.rust-lang.org/std/option/index.html
[10]: http://github.com/ndreynolds/rustray 
