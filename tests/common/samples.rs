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
const l: number = b * 3
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
";

pub const BAD_SYNTAX: &str = "
const arr: number[] = [1,,.,3]
const foo = () => {
    (
}
";
