# scala-dynamic-typing

Proof of concept String interpolator macro for runtime failure of typechecking.

## Usage

### typechecks fine

```scala
case class Cat(name: String, age: Int)
val myCat = Cat("Fluf", 10)
val age = dyn"myCat.age"
age === 10
```

### fails typecheck at runtime with a compiler warning

```scala
case class Cat(name: String, age: Int)
val myCat = Cat("Fluf", 10)
val age = dyn"myCat.breed"
age === 10
```
