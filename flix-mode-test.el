;;; flix-mode-test.el --- Some tests for flix-mode -*- lexical-binding: t; -*-

;; Copyright (c) 2021  Jacob Harris Cryer Kragh

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'flix-mode)

(defun flix-mode--indent-test-helper (content)
  (with-temp-buffer
    (insert content)
    (flix-mode)
    (setq tab-width 4)
    (indent-region (point-min) (point-max))
    (should (equal content
                   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest flix-mode--indent-single-line-def-1 ()
  (flix-mode--indent-test-helper
   "def f(x: Int32): Int32 = x + 2"))

(ert-deftest flix-mode--indent-single-line-def-2 ()
  (flix-mode--indent-test-helper
   "
def f(x: Int32): Int32 = x + 2"))

(ert-deftest flix-mode--indent-single-line-def-3 ()
  (flix-mode--indent-test-helper
   "
def f(x: Int32): Int32 = x + 2

"))

(ert-deftest flix-mode--indent-single-line-enum-1 ()
  (flix-mode--indent-test-helper
   "enum Color { case Red, Blue, Green }"))

(ert-deftest flix-mode--indent-single-line-enum-2 ()
  (flix-mode--indent-test-helper
   "
enum Color { case Red, Blue, Green }"))

(ert-deftest flix-mode--indent-single-line-enum-3 ()
  (flix-mode--indent-test-helper
   "
enum Color { case Red, Blue, Green }

"))

(ert-deftest flix-mode--indent-single-line-alias-1 ()
  (flix-mode--indent-test-helper
   "type alias Foo = Int32"))

(ert-deftest flix-mode--indent-single-line-alias-2 ()
  (flix-mode--indent-test-helper
   "
type alias Foo = Int32"))

(ert-deftest flix-mode--indent-single-line-alias-3 ()
  (flix-mode--indent-test-helper
   "
type alias Foo = Int32

"))

(ert-deftest flix-mode--indent-single-namespace-1 ()
  (flix-mode--indent-test-helper
   "namespace Foo {}"))

(ert-deftest flix-mode--indent-single-namespace-2 ()
  (flix-mode--indent-test-helper
   "
namespace Foo {}"))

(ert-deftest flix-mode--indent-single-namespace-3 ()
  (flix-mode--indent-test-helper
   "
namespace Foo {}

"))

(ert-deftest flix-mode--indent-nested-namespaces-1 ()
  (flix-mode--indent-test-helper
   "
namespace Foo {
    namespace Bar {
        
    }
}"))

(ert-deftest flix-mode--indent-nested-namespaces-2 ()
  (flix-mode--indent-test-helper
   "
namespace Foo {
    namespace Bar {
        namespace Baz {
            
        }
    }
}"))

(ert-deftest flix-mode--indent-nested-namespaces-3 ()
  (flix-mode--indent-test-helper
   "
namespace Foo {
    namespace Bar {
        
    }

    namespace Baz {
        
    }
}"))

(ert-deftest flix-mode--indent-sibling-namespaces-1 ()
  (flix-mode--indent-test-helper
   "
namespace Foo {
    
}

namespace Bar {
    
}
"))

(ert-deftest flix-mode--indent-multi-line-call ()
  (flix-mode--indent-test-helper
   "def main(_args: Array[String]): Int32 & Impure =
    println(
        \"Hello World!\"
    );
    0
"))

(ert-deftest flix-mode--program-1 ()
  (flix-mode--indent-test-helper
   "
def main(_args: Array[String]): Int32 & Impure =
    g();
    0

def f(): Unit & Impure =
    match Console.readLine() {
        case None => ()
        case Some(s) =>
            println(s);
            f()
    }

def g(): Unit & Impure =
    let in = Console.readLine();
    println(\">>> ${in}\");
    match in {
        case Some(\"END\") => ()
        case _ => g()
    }
"))

(ert-deftest flix-mode--program-2 ()
  (flix-mode--indent-test-helper
   "
def main(_args: Array[String]): Int32 & Impure =
    println(fac(5)); // 120
    0

def fac(n: Int32): Int32 = facAcc(n, 1)

def facAcc(n: Int32, acc: Int32): Int32 = match n {
    case 0 => acc
    case _ => facAcc(n - 1, n * acc)
}
"))

(ert-deftest flix-mode--program-3 ()
  (flix-mode--indent-test-helper
   "
namespace Input {
    type alias Stdin = ##java.util.Scanner

    pub enum Bla {
        case Foo,
        case Bar
    }

    pub enum Baz {
        case Qux, Quux, Quuux
    }

    namespace Nested {
        pub def foo(): Int32 & Impure =
            println(\"Hi!\");
            0
    }

    /*
    {

    }
    */

    pub def openStdin(): Stdin & Impure =
        import new java.util.Scanner(##java.io.InputStream) as newScanner;
        import get java.lang.System:in as in;
        newScanner(in())

    pub def readLine(in: Stdin): Option[String] & Impure =
        import java.util.Scanner.hasNextLine();
        import java.util.Scanner.nextLine();
        if (hasNextLine(in)) // TODO: if then else
            Some(nextLine(in))
        else
            None
}

enum Aop { case Plus, Minus }

enum Aexp {
    case Literal(Int32),
    case Binary(Aexp, Aop, Aexp),
}

def eval(exp: Aexp): Int32 = match exp {
    case Literal(n) => n
    case Binary(left, op, right) =>
        let a = eval(left);
        let b = eval(right);
        match op {
            case Plus => a + b
            case Minus => a - b
        }
}

type alias Token = {
    text: String,
    start: Int32,
    end: Int32
}

def nextIntToken(line: String, start: Int32, idx: Int32, numeral: String): Token =
    if (idx >= String.length(line) or not Char.isDigit(String.charAt(idx, line)))
        { text = numeral, start = start, end = idx }
    else {
        let c = String.charAt(idx, line);
        nextIntToken(line, start, idx + 1, numeral + Char.toString(c))
    }

def nextToken(line: String, start: Int32): Option[Token] =
    if (start >= String.length(line))
        None
    else {
        let c = String.charAt(start, line);
        if (c == '+' or c == '-')
            Some({ text = \"${c}\", start = start, end = start + 1 })
        else
            Some(nextIntToken(line, start, start, \"\"))
    }

def tokenize(line: String, start: Int32): List[Token] =
    match nextToken(line, start) {
        case None => Nil
        case Some(tok) => tok :: tokenize(line, tok.end)
    }

def stringify(t: Token): String =
    \"Token { text = '${t.text}', start = ${t.start}, end = ${t.end} }\"

def main(_args: Array[String]): Int32 & Impure =
    let in = Input.openStdin();
    match Input.readLine(in) {
        case None => ()
        case Some(line) => {
            let tokens = tokenize(String.replace(\" \", \"\", line), 0);
            List.foreach(t -> println(stringify(t)), tokens)
        }
    };
    println(Input.readLine(in));
    let exp = Binary(Literal(2), Plus, Literal(2));
    foo(eval(exp));
    0

def _bar(x: Int32): Int32 & Impure =
    let y = 2 * x;
    let z = y + 1;
    foo(z);
    0

def foo(n: Int32): Unit & Impure =
    println(\"Hello!\");
    println(\"...\");
    println(\">>> ${n}\")

"))

(ert-deftest flix-mode--program-4 ()
  (flix-mode--indent-test-helper
   "def pow2(n: Int32): Int32 = pow2Aux(n, 1)

def pow2Aux(n: Int32, acc: Int32): Int32 = match n {
    case 0 => acc
    case _ => pow2Aux(n - 1, 2 * acc)
}

def main(_args: Array[String]): Int32 & Impure =
    println(pow2(10));
    0
"))

(ert-deftest flix-mode--program-5 ()
  (flix-mode--indent-test-helper
   "def printRands(count: Int32): Unit & Impure =
    import java.lang.Math:round(Float64);
    import java.lang.Math:random();
    let upperBound = 100.0;
    if (count == 0)
        ()
    else {
        println(round(random() * upperBound));
        printRands(count - 1)
    }

def main(_args: Array[String]): Int32 & Impure =
    printRands(5);
    let x = {
        println(\"hi\");
        5
    };
    println(x);
    0

"))

(ert-deftest flix-mode--program-6 ()
  (flix-mode--indent-test-helper
   "/* This is a comment */
// This is another comment
def main(_args: Array[String]): Int32 & Impure =
    solveIt(stdin());
    0

def solveIt(sc: Scanner): Unit & Impure =
    match readInt(sc) {
        case None => ()
        case Some(n) => {
            println(n);
            solveIt(sc)
        }
    }

/// ---

type alias Scanner = ##java.util.Scanner

def stdin(): Scanner & Impure =
    import new java.util.Scanner(##java.io.InputStream) as newScanner;
    import get java.lang.System:in as in;
    newScanner(in())

def readInt(sc: Scanner): Option[Int32] & Impure =
    import java.util.Scanner.hasNextInt();
    import java.util.Scanner.nextInt();
    if (hasNextInt(sc))
        Some(nextInt(sc))
    else
        None
"))

(ert-deftest flix-mode--program-7 ()
  (flix-mode--indent-test-helper
   "def f(): Int64 =
    let x = 2147483647i64;
    x

def main(_args: Array[String]): Int32 & Impure =
    let y = f() + 1i64;
    println(y);
    0
"))

(ert-deftest flix-mode--program-8 ()
  (flix-mode--indent-test-helper
   "
def main(_args: Array[String]): Int32 & Impure =
    println(f());
    println(Console.readLine());
    println(Console.readLine());
    0

def f(): Int32 & Pure = 42
"))

(ert-deftest flix-mode--program-9 ()
  (flix-mode--indent-test-helper
   "// def f(y: Int32): Int32 = y + 1

/* what */

def main(_args: Array[String]): Int32 & Impure =
    let x = 41;
    let f = y -> y + 1;
    let z = f(x);
    println(z);
    0
"))

(ert-deftest flix-mode--program-10 ()
  (flix-mode--indent-test-helper
   "
def secret(x: Int32): Bool = match x {
    case 42 => true
    case _ => if (x % 400 == 0) true else false
}

namespace Time {

    pub enum Weekday {
        case Monday,
        case Tuesday,
        case Wednesday,
        case Thursday,
        case Friday
    }

    // A comment

    pub def dayToInt(wd: Weekday): Int32 = match wd {
        case Monday => 0
        case Tuesday => 1
        case Wednesday => 2
        case Thursday => 3
        case Friday => 4
    }

    // Another comment

    pub def getMonday(): Weekday =
        Monday

    // This comment is indented correctly...
    
    pub def getFriday(): Weekday = Friday

    // This comment is indented incorrectly!
}

type alias Wd = Time.Weekday

def d2i(wd: Wd): Int32 = Time.dayToInt(wd)

// This comment is indented incorrectly!
def main(_args: Array[String]): Int32 & Impure =
    let m = Time.getMonday();
    let f = Time.getFriday();
    let delta = d2i(f) - d2i(m);
    println(\"Delta from monday to friday: ${delta}\");
    println(\"Secret? ${secret(d2i(f))}\");
    0

"))
