# Logos

Overview/Story:
Logos is designed to represent any logical or mathematical statement and to help prove theorems and run math processes.

Some of us have a Math minor and have always found that proving theorems helped us learn math. Now with the rise of AI, we saw that these language models were deficient in logic. Therefore, we wanted to represent statements in code so that a machine can find connections and generate new theorems. It has probably been done before, but hopefully this will be done more efficiently than ever before, by reducing math to its simplest forms.

Features:
- Primitive types are statements and values
- Data types also include sets, variables, and functions, which we will call function-objects
- Recursively defined function-objects
- Some type inference is needed (e.g. variables within set definitions) but it’s generally manifest static typing
- A good amount of syntactic sugar
- Sum and product types (including optionals)
- User-defined custom data types / classes (effectively sets)
- Boolean operations should be simplified automatically
- User-defined scope of variables and assumed-true statements
- Shallow binding and lazy evaluation
- A package (not built in) to include common math definitions, like real numbers and numbers like pi
- Variables are immutable but boolean truth-values can be mutated
- Variables are bound
- No shadowing
- Expression-oriented; everything returns a value
- Sequential flow
- Exceptions and optionals

## Examples:

```logos
// A class with a behavior (like an interface)
class Traceable {
  mod source(self) -> Any;
}
```
```logos
// An enum with multiple variant forms
enum Contradiction {
  direct: None,
  indirect: bool,
  bool, // equivalent to 0: bool
}
```
```logos
// A struct holding structured data
struct ContradictionSource {
  source: string,
  data: mut [bool; 4],
  notes: mut [string],
  contradiction: Contradiction,
}
```
```logos
// A constructor for the struct
mod ContradictionSource.new() {
  return Self {
    source: "root",
    data: mut [false; 4],
    notes: mut [],
    contradiction: Contradiction.direct(none),
  };
}
```
```logos
// A main function with mutation and logic
mod main(source: ContradictionSource, realSource: Traceable) {
  if source == none {
    x = ContradictionSource.new();
  }
  x.data.3 = true;
  print(str(x.data)); // Output: [false, false, false, true]
}
```

**Collaborators:**

### Rayane Tarazi 
A computer science enthusiast and gamer, always looking to leve up. 😎	
### Daniel Munoz 
Loves coding and cats. 🐈‍⬛
### Akash Beh 
A linux enthusiast who loves to einstein bagels, minecraft, and math.
### Nick Aurino
A computer enthusiast who enjoys games and his dog! 

**Repo Link:**
[Github Pages Hosting](https://rtarazi1105.github.io/Logos/)
