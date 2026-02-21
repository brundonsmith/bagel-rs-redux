/// Literals: nil, booleans, numbers (integer and decimal), strings (with escapes)
pub const LITERALS: &str = "
const a = nil
const b = true
const c = false
const d = 42
const e = 3.14
const f = 'hello'
const g = 'it\\'s escaped \\\\ done'
";

/// Arithmetic operators: +, -, *, /, and parenthesized grouping
pub const ARITHMETIC: &str = "
const a = 1 + 2
const b = 10 - 3
const c = 4 * 5
const d = 8 / 2
const e = (1 + 2) * 3
const f = 1 + 2 * 3 - 4 / 2
";

/// Comparison and logical operators: ==, !=, <, <=, >, >=, &&, ||, ??, !
pub const COMPARISON_AND_LOGIC: &str = "
const a = 1 == 2
const b = 1 != 2
const c = 1 < 2
const d = 1 <= 2
const e = 1 > 2
const f = 1 >= 2
const g = true && false
const h = true || false
const i = nil ?? 5
const j = !true
";

/// Const declarations with type annotations, including export
pub const DECLARATIONS: &str = "
const a: number = 42
const b: string = 'hello'
const c: boolean = true
const d: nil = nil
const e: unknown = 42
export const f: number = 1
export const g = 'exported'
";

/// Import declarations, including multiple imports and aliases
pub const IMPORTS: &str = "
from './foo.bgl' import { alpha }
from './bar.bgl' import { beta, gamma }
from './baz.bgl' import { delta as renamed }

const alpha = 'stuff'
";

/// Function expressions: various param styles, expression and block bodies
pub const FUNCTIONS: &str = "
const a = () => 42
const b = (x: number) => x + 1
const c = (x: number, y: number): number => x + y
const d = x => x
const e = () => {
    js.console.log('hello')
}
const f = (a: number, b: number) => {
    js.console.log(a)
    js.console.log(b)
}
";

/// Function type annotations and contextual typing
pub const FUNCTION_TYPES: &str = "
const a: () => number = () => 42
const b: (number) => number = (x) => x + 1
const c: (number, number) => number = (a, b) => a + b
const d: (x: number, y: number) => number = (a, b) => a + b
";

/// If-else expressions: simple, with else, and else-if chains; if without else
pub const IF_ELSE: &str = "
const a = if true { 1 } else { 2 }
const b = if false { 'yes' }
const c = if true { 1 } else if false { 2 } else { 3 }
const d = if true { 'a' } else if true { 'b' } else if false { 'c' } else { 'd' }
";

/// Array literals and object literals
pub const COLLECTIONS: &str = "
const a = [1, 2, 3]
const b = ['hello', 'world']
const c = {name: 'alice', age: 42}
const d = {x: 1, y: 2, z: 3}
const e = [{name: 'bob'}, {name: 'carol'}]
const f = {inner: {deep: true}}
";

/// Property access and invocations, including chaining
pub const PROPERTY_ACCESS: &str = "
const a = {x: 1, y: 2}
const b = a.x
const c = {inner: {deep: 42}}
const d = c.inner.deep
const e = js.console.log
";

/// Invocations: simple, chained, and as statements in blocks
pub const INVOCATIONS: &str = "
const log = js.console.log
const main = () => {
    js.console.log('hello')
    js.console.log(42)
}
";

/// Type annotations: primitives, ranges, arrays, typeof, parenthesized, function types
pub const TYPE_ANNOTATIONS: &str = "
const a: number = 1
const b: string = 'hi'
const c: boolean = true
const d: nil = nil
const e: unknown = 42
const f: 0..10 = 5
const g: 0.. = 100
const h: ..100 = 50
const i: number[] = [1, 2, 3]
const j: typeof a = 2
const k: (number) = 3
const l: number? = b * 3
const m: number? = nil
";

/// Type checking errors: mismatched types, wrong operator operands, and subtler cases
pub const TYPE_ERRORS: &str = "
const a: string = 42
const b: number = true
const c = 1 + true
const d = 'hello' - 1
const e = 1 && 2
const f = 1 < true
const g = !42
const h: boolean = nil ?? 5
const i: 0..10 = 15
const j: string[] = ['hello', 42]
const k: () => string = () => 42
const l: (string) => number = (x: number) => x
const m = if 1 { 'yes' } else { 'no' }
const n = {name: 'alice'}.age
const o: {name: string} = {name: 'alice', age: 42}
const p: {x: number, y: number} = {x: 1}

const fn = (a: number, b: number) => a + b
const resulta: 4 = fn(2, 2)
const resultb: 4 = (2)..fn(2)

const aaaaaa = fn()
const bbbbbb = fn(1, 2, 3)
";

/// Comments: line comments, block comments, inline comments, and blank lines
pub const COMMENTS: &str = "
// A line comment before a declaration
const a = 1

// A block of consecutive line comments
// spanning multiple lines
const b = 2

const c = 3 // inline comment

/* oasifjdsdfgkhu
    kjsdfghkdlsjfhg
*/
const d = 4

/* a block comment */
const e = 5
// comment directly after code, no blank line
const f = 6
";

/// Blank lines between declarations
pub const BLANK_LINES: &str = "
const a = 1

const b = 2



const c = 3
";

/// Pipe call (UFCS): subject..function(args) desugars to function(subject, args)
pub const PIPE_CALL: &str = "
const double = (x: number): number => x * 2
const add = (a: number, b: number): number => a + b
const result = 5..double()
const other = 5..add(3)
const stuff = (3 + 'foo')..double()
";

pub const BAD_SYNTAX: &str = "
const arr: number[] = [1,,.,3]
const foo = () => {
    (
}
const other = obj.
const otherfunc = () => {
    obja.()
    objb.
}
";

/// Module with unparseable junk that prevents parsing to the end
pub const UNPARSEABLE_JUNK: &str = "
const a =    1
@#$%^&
const b = 2
";

/// Tuple types: fixed-length typed arrays
pub const TUPLE_TYPES: &str = "
const a: [number, string] = [1, 'hello']
const b: [boolean, number, string] = [true, 42, 'hi']
const c: [number, number] = [1, 2]
const d: [] = []
const e: [number] = [42]
const f: [string, nil] = ['hello', nil]
";

/// Tuple type errors: mismatched element types, wrong length, non-tuple assignment
pub const TUPLE_TYPE_ERRORS: &str = "
const a: [number, string] = [1, 2]
const b: [number, number] = [1, 2, 3]
const c: [number, number, number] = [1, 2]
const d: [string] = [42]
const e: [boolean] = ['oops']
const f: [number, string] = 'not a tuple'
";

/// Union types: values that can be one of several types
pub const UNION_TYPES: &str = "
const a: number | string = 42
const b: number | string = 'hello'
const c: number | string | boolean = true
const d: number | nil = nil
const e: number | nil = 5
const f: string | number = if true { 'hi' } else { 42 }
const g: (number | string)[] = [1, 'two', 3]
const h: [number, string | boolean] = [1, true]
const i: [number, string | boolean] = [1, 'hello']
";

/// Union type errors: values not matching any variant
pub const UNION_TYPE_ERRORS: &str = "
const a: number | string = true
const b: number | boolean = 'nope'
const c: number | string = nil
const d: [number | string] = [true]
";

/// Named type declarations (type aliases) and their usage
pub const NAMED_TYPES: &str = "
type mynum = number
const a: mynum = 42

type mystr = string
const b: mystr = 'hello'

type pair = [number, string]
const c: pair = [1, 'hi']

type result = number | string
const d: result = 42
const e: result = 'error'

type callback = (number) => string
const f: callback = (x) => 'done'

type nested = [result, boolean]
const g: nested = [42, true]
const h: nested = ['err', false]
";

/// Named type errors: mismatches through aliases, undefined types
pub const NAMED_TYPE_ERRORS: &str = "
type mynum = number
const a: mynum = 'not a number'

type pair = [number, string]
const b: pair = [1, 2]
const c: pair = ['oops', 'bad']

type result = number | string
const d: result = true

type strict = [boolean, number]
const e: strict = [1, true]
";
