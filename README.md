# signe

Signe is a minimal library that bridges the gap between the functional programming style of Clojure and stateful object oriented GUI libraries such as Java Swing. The goal is to eliminate the complexity and spaghetti code that object-oriented GUI programming can easily lead to, and separate the user interface from the domain model, such that the domain model is unaware of the user interface. The domain model can be a single immutable clojure data structure wrapped inside an atom. In practice, it implements the observer design pattern, so that whenever the domain model is replaced by a new version of itself, e.g. through an atomic swap, only functions that are connected with GUI elements that need to be refreshed are called. This library is not bound to a particular GUI library, but is designed to work well with Swing. For Swing, there is also the Seesaw library that provides a nice wrapper and syntactic sugar to hide the Java interop calls. Signe offers functionality orthogonal to that of Seesaw and will play well with or without it.

The functionality of this library is best appreciated by taking a look at the examples in [examples.clj](src/signe/examples.clj).

## Usage

FIXME

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
