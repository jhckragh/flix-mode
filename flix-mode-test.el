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

;; (ert-deftest flix-mode--program-2 () ;; TODO: This doesn't pass because of the comment!
;;   (flix-mode--indent-test-helper
;;    "
;; def main(_args: Array[String]): Int32 & Impure =
;;     println(fac(5)); // 120
;;     0

;; def fac(n: Int32): Int32 = facAcc(n, 1)

;; def facAcc(n: Int32, acc: Int32): Int32 = match n {
;;     case 0 => acc
;;     case _ => facAcc(n - 1, n * acc)
;; }
;; "))
