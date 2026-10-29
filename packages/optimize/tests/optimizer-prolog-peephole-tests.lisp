;;;; packages/optimize/tests/optimizer-prolog-peephole-tests.lisp
;;;;
;;;; Relocated from packages/prolog/tests/prolog-peephole-tests.lisp and
;;;; prolog-peephole-tests-internal.lisp as part of the migration off cl-cc's
;;;; own homegrown Prolog engine onto the external cl-prolog-kit library — this
;;;; is genuine cl-cc functionality (the peephole optimizer), unlike the rest
;;;; of packages/prolog/tests, which tested the now-removed engine's own
;;;; unification/DCG/builtin-dispatch machinery (now the external cl-prolog-kit
;;;; library's responsibility to test, not cl-cc's). The two TYPE-OF/3 tests
;;;; from the original file exercised packages/prolog's dead (no production
;;;; caller) declarative type-inference rules and were dropped, not ported.

(in-package :cl-cc/test)


;;; %peephole-walk unit tests (internal helper)

