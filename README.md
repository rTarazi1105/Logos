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

**Examples:**
<ins>Importing:<\ins>
_use mod::axioms;

mod Main {
	axioms::Axioms;
	
	define var 2 = succ(1);
	
}_
<ins>Priority:<\ins>
_import here::basics;
use basics::ifthen;
use basics::Contradiction;

statement A : "It's raining";
statement B : "The ground is wet";
statement C : "It rained yesterday";

statement X : A and C ifthen B;		// Priority of operations over "and"

mod Examine(S: statement) { // given that the statement is true
	if S.truth != true throw Contradiction;
	match S {
		conjunction(C) => for I in C {
			assume I;
			Examine(I);
		},
		disjunction(D) => {
			T = none;
			all_false_except_one = true;
			for I in D
				if I.truth == none {
					if T != none break;
					T = I;
				} else if I.truth
					all_false_except_one = false;
			if all_false_except_one assume T;
		},
	}
}

mod Main {
	assume X;
	
	print B.truth;		// none
	
	assume C; Examine(X);
	print B.truth;		// true
}_
<ins>Structs:<\ins>
_struct Matrix {
	contents: [[Int; _]; _]
}

mod Matrix.cols -> Int {
	self.0.len
}
mod Matrix.rows -> Int {
	self.0[0].len
}

// inheritance
struct Matrix.Stochastic {
	sum: Int
}

mod createMatrix(cols: Int, rows: Int) {
	Matrix {
		contents: [[0; rows]; cols],
	}
}

mod createStochastic(cols: Int, rows: Int) {
	Matrix.Stochastic {
		contents: [[0; rows]; cols],
		sum: 100
	}
}__
<ins>Generator:<\ins>
_mod Range(n: Int) -> Collection {
	i = 0;
	while i != n {
		yield i;
		i = i + 1;
	}
}_
<ins>Axioms:<\ins>
_import here::basics;
use basics.data;
use basics::Set;

property is_injective f : f(x) = f(y) ifthen x = y;
// x and y are ∀ by default

var 0 : "zero";

// succession
func succ x : "custom" and succ(x) != 0 and is_injective(succ);

var 1 : 1 = succ(0);

Set Naturals(x : ∃succ(x));

mod Axioms {
	assume ∃succ(0); assume ∃succ(x) ifthen ∃succ(succ(x));
	if Naturals.Contains(succ(0)).truth print "success"
}_

**Collaborators:**
Rayane Tarazi, Daniel Munoz, Akash Beh, Nick Aurino

**Repo Link:**
https://github.com/rTarazi1105/Logos.git
