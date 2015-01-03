# signe

Signe is a minimal library that bridges the gap between the functional programming style of Clojure and stateful object oriented GUI libraries such as Java Swing. The goal is to eliminate the complexity and spaghetti code that object-oriented GUI programming can easily lead to, and separate the user interface from the domain model, such that the domain model is unaware of the user interface. The domain model can be a single immutable clojure data structure wrapped inside an atom. In practice, it implements the observer design pattern, so that whenever the domain model is replaced by a new version of itself, e.g. through an atomic swap, only functions that are connected with GUI elements that need to be refreshed are called. This library is not bound to a particular GUI library, but is designed to work well with Swing. For Swing, there is also the [Seesaw](https://github.com/daveray/seesaw) library that provides a nice wrapper and syntactic sugar to hide the Java interop calls. Signe offers functionality orthogonal to that of Seesaw and will play well with or without it.

The functionality of this library is best appreciated by taking a look at the examples in [examples.clj](src/signe/examples.clj).

## Usage
Here are some very useful functions and macros. Since this is a very small library, the user will probably not need any other functions in order to use the library.

See [examples.clj](src/signe/examples.clj) for how they are used in practice.

### Useful Functions
```clojure
(make-monitor state-atom)
```
Creates a new instance of type Monitor from an atom that wraps the domain model.

```clojure
(only-register monitor funcall updater & args)
```
registers a function **updater** that will be called whenever the call chain defined by **funcall** evaluates to a different value than before, when applied to the model. **monitor** is the monitor object that monitors the model for changes. The **updater** function takes a record of type ModelChange which contains a state-field, and returns the next hidden state. The initial state is nil, or an optional value from args.

```clojure
(register-and-update monitor funcall updater & args)
```
Works like the function **only-register**, but also calls the **updater** function once. Usually, you will want to use this function over **only-register**, so that newly instantiated widgets get initialized.


### Useful Macros
```clojure
(call-chain expr)
```
A macro used to define a chain of function calls. Evaluates to a Funcall. If **expr** is a keyword **:obj**, it will evaluate to an empty Funcall object. Otherwise it will recursively build a function call chain.

## License

Copyright © 2014 Jonas Östlund

Distributed under the Eclipse Public License version 1.0.
