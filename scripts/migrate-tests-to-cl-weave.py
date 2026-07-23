#!/usr/bin/env python3
"""Transform cl-cc home-grown test forms to cl-weave native, preserving
comments and formatting outside the rewritten heads.

  (deftest name "doc" body...)        -> (it-sequential "name" body...)
  (deftest-each base "doc"
     :cases ((label v...) ...) (vars) b) -> per-case (it-sequential "base label"
                                              (destructuring-bind (vars) (list v...) b))
  (assert-true x)      -> (expect x :to-be-truthy)
  (assert-false x)     -> (expect x :to-be-falsy)
  (assert-null x)      -> (expect x :to-be-null)
  (assert-eq a b)      -> (expect b :to-be a)
  (assert-eql a b)     -> (expect b :to-be a)
  (assert-= a b)       -> (expect b :to-equal a)
  (assert-equal a b)   -> (expect b :to-equal a)
  (assert-equalp a b)  -> (expect b :to-equalp a)
  (assert-string= a b) -> (expect b :to-equal a)
  (assert-type ty v)   -> (expect (typep v 'ty) :to-be-truthy)
  (assert-signals c body...) -> (let ((#:s nil)) (handler-case (progn body...) (c () (setf #:s t))) (expect #:s :to-be-truthy))
  (defsuite ...) / (in-suite ...)      -> removed

Operates on raw text with a paren/​string/​comment-aware scanner so comments and
layout are preserved verbatim; only the recognized heads are rewritten.
"""
import sys, re

def skip_ws_comments(s, i):
    # not used for span extraction; kept simple
    return i

def read_form(s, i):
    """Return (form_text, end_index) for the sexp starting at s[i]=='('.
    Handles strings, char literals, line comments, block comments, |...|."""
    assert s[i] == '('
    depth = 0
    j = i
    n = len(s)
    while j < n:
        c = s[j]
        if c == '"':
            j += 1
            while j < n and s[j] != '"':
                if s[j] == '\\':
                    j += 1
                j += 1
            j += 1
            continue
        if c == ';':
            while j < n and s[j] != '\n':
                j += 1
            continue
        if c == '#' and j + 1 < n and s[j+1] == '\\':
            j += 3  # #\x  (char literal, may be #\Newline but 1 char ok for balance)
            continue
        if c == '#' and j + 1 < n and s[j+1] == '|':
            j += 2
            while j + 1 < n and not (s[j] == '|' and s[j+1] == '#'):
                j += 1
            j += 2
            continue
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
            if depth == 0:
                return s[i:j+1], j+1
        j += 1
    raise ValueError("unbalanced")

def split_args(inner):
    """Split the inner text of a form (without outer parens) into top-level
    sub-tokens (atoms or sexps), returning list of (text) preserving order."""
    toks = []
    i = 0
    n = len(inner)
    while i < n:
        c = inner[i]
        if c in ' \t\n':
            i += 1
            continue
        if c == ';':
            while i < n and inner[i] != '\n':
                i += 1
            continue
        # '#' dispatch reader macros: #(...) vector, #\x char, #'fn, #.form,
        # #xNN/#bNN/#:sym/#+feature ... — keep the whole datum as one token.
        if c == '#':
            pre = i
            i += 1  # past '#'
            if i < n and inner[i] == '(':          # #(...) vector
                _, j = read_form(inner, i); toks.append(inner[pre:j]); i = j; continue
            if i < n and inner[i] == '\\':         # #\x or #\Newline
                i += 1
                j = i + 1 if i < n else i
                while j < n and (inner[j].isalnum() or inner[j] == '-'):
                    j += 1
                toks.append(inner[pre:j]); i = j; continue
            if i < n and inner[i] in "'.+-":       # #'fn #.form #+feat #-feat
                i += 1
                while i < n and inner[i] in ' \t\n':
                    i += 1
                if i < n and inner[i] == '(':
                    _, j = read_form(inner, i); toks.append(inner[pre:j]); i = j
                else:
                    j = i
                    while j < n and inner[j] not in ' \t\n()";':
                        j += 1
                    toks.append(inner[pre:j]); i = j
                continue
            # #xNN #bNN #oNN #:sym #C(...) etc — read as atom (up to delimiter)
            j = i
            while j < n and inner[j] not in ' \t\n()";':
                j += 1
            toks.append(inner[pre:j]); i = j; continue
        # quote/quasiquote/unquote prefixes attach to the following datum
        if c in "'`" or c == ',':
            pre = i
            if c == ',' and i+1 < n and inner[i+1] == '@':
                i += 2
            else:
                i += 1
            while i < n and inner[i] in ' \t\n':
                i += 1
            if i < n and inner[i] == '(':
                _, j = read_form(inner, i)
                toks.append(inner[pre:j]); i = j
            elif i < n and inner[i] == '"':
                j = i+1
                while j < n and inner[j] != '"':
                    if inner[j] == '\\': j += 1
                    j += 1
                j += 1
                toks.append(inner[pre:j]); i = j
            else:
                j = i
                while j < n and inner[j] not in ' \t\n()";':
                    j += 1
                toks.append(inner[pre:j]); i = j
            continue
        if c == '(':
            f, j = read_form(inner, i)
            toks.append(f)
            i = j
            continue
        if c == '"':
            j = i + 1
            while j < n and inner[j] != '"':
                if inner[j] == '\\':
                    j += 1
                j += 1
            j += 1
            toks.append(inner[i:j])
            i = j
            continue
        # atom
        j = i
        while j < n and inner[j] not in ' \t\n()";':
            j += 1
        toks.append(inner[i:j])
        i = j
    return toks

GENSYM = [0]
def gensym():
    GENSYM[0] += 1
    return f"%%signaled{GENSYM[0]}"

def transform_asserts(text):
    """Rewrite assert-* forms inside TEXT (recursively) to expect forms."""
    out = []
    i = 0
    n = len(text)
    while i < n:
        c = text[i]
        if c == '"':
            j = i + 1
            while j < n and text[j] != '"':
                if text[j] == '\\':
                    j += 1
                j += 1
            j += 1
            out.append(text[i:j]); i = j; continue
        if c == ';':
            j = i
            while j < n and text[j] != '\n':
                j += 1
            out.append(text[i:j]); i = j; continue
        if c == '(':
            form, j = read_form(text, i)
            inner = form[1:-1]
            toks = split_args(inner)
            head = toks[0] if toks else ''
            rewritten = rewrite_head(head, toks)
            if rewritten is not None:
                out.append(rewritten)
            else:
                # recurse into the form body
                out.append('(' + transform_asserts(inner) + ')')
            i = j; continue
        out.append(c); i += 1
    return ''.join(out)

def rewrite_head(head, toks):
    a = toks[1:] if len(toks) > 1 else []
    def T(x):  # transform nested asserts inside an argument
        return transform_asserts(x)
    if head == 'assert-true' and len(a) >= 1:
        return f"(expect {T(a[0])} :to-be-truthy)"
    if head == 'assert-false' and len(a) >= 1:
        return f"(expect {T(a[0])} :to-be-falsy)"
    if head == 'assert-null' and len(a) >= 1:
        return f"(expect {T(a[0])} :to-be-null)"
    if head in ('assert-eq', 'assert-eql') and len(a) >= 2:
        return f"(expect {T(a[1])} :to-be {T(a[0])})"
    if head == 'assert-=' and len(a) >= 2:
        # numeric = crosses types (6 vs 6.0d0); :to-equal is type-strict equal.
        return f"(expect (= {T(a[0])} {T(a[1])}) :to-be-truthy)"
    if head in ('assert-equal', 'assert-string=') and len(a) >= 2:
        return f"(expect {T(a[1])} :to-equal {T(a[0])})"
    if head == 'assert-equalp' and len(a) >= 2:
        return f"(expect {T(a[1])} :to-equalp {T(a[0])})"
    if head == 'assert-type' and len(a) >= 2:
        return f"(expect (typep {T(a[1])} '{a[0]}) :to-be-truthy)"
    if head == 'assert-signals' and len(a) >= 2:
        g = gensym()
        cond = a[0]
        body = ' '.join(T(x) for x in a[1:])
        return (f"(let (({g} nil)) (handler-case (progn {body}) "
                f"({cond} () (setf {g} t))) (expect {g} :to-be-truthy))")
    return None

def name_to_string(tok):
    tok = tok.strip()
    if tok.startswith('"'):
        return tok
    return '"' + tok.lower() + '"'

def transform_toplevel(s):
    out = []
    i = 0
    n = len(s)
    while i < n:
        c = s[i]
        if c == ';':
            j = i
            while j < n and s[j] != '\n':
                j += 1
            out.append(s[i:j]); i = j; continue
        if c == '"':
            j = i + 1
            while j < n and s[j] != '"':
                if s[j] == '\\':
                    j += 1
                j += 1
            j += 1
            out.append(s[i:j]); i = j; continue
        if c == '(':
            form, j = read_form(s, i)
            toks = split_args(form[1:-1])
            head = toks[0] if toks else ''
            if head in ('defsuite',):
                # drop, but keep following newline handling minimal
                i = j
                # also swallow a trailing blank line
                while i < n and s[i] in ' \t':
                    i += 1
                if i < n and s[i] == '\n':
                    i += 1
                continue
            if head == 'in-suite':
                i = j
                while i < n and s[i] in ' \t':
                    i += 1
                if i < n and s[i] == '\n':
                    i += 1
                continue
            if head == 'deftest':
                out.append(transform_deftest(toks)); i = j; continue
            if head == 'deftest-each':
                out.append(transform_deftest_each(toks)); i = j; continue
            out.append(form); i = j; continue
        out.append(c); i += 1
    return ''.join(out)

def transform_deftest(toks):
    # toks: ('deftest' name [docstring] body...)
    name = toks[1]
    rest = toks[2:]
    if rest and rest[0].startswith('"'):
        rest = rest[1:]  # drop docstring
    body = '\n  '.join(transform_asserts(x) for x in rest)
    return f'(it-sequential {name_to_string(name)}\n  {body})'

def transform_deftest_each(toks):
    # (deftest-each base [doc] :cases (...) (vars) body...)
    base = toks[1]
    rest = toks[2:]
    if rest and rest[0].startswith('"'):
        rest = rest[1:]
    # find :cases
    assert rest[0] == ':cases', f"expected :cases, got {rest[0]}"
    cases_form = rest[1]              # (( "label" v...) ...)
    vars_form = rest[2]               # (vars)
    body = rest[3:]
    cases = split_args(cases_form[1:-1])  # each is a (label v...) sexp
    body_txt = ' '.join(transform_asserts(x) for x in body)
    out = []
    for case in cases:
        parts = split_args(case[1:-1])
        label = parts[0]
        vals = parts[1:]
        lbl = label.strip('"')
        tname = f'"{base.lower()} {lbl}"'
        vals_txt = ' '.join(vals)
        out.append(
            f'(it-sequential {tname}\n'
            f'  (destructuring-bind {vars_form} (list {vals_txt})\n'
            f'    {body_txt}))')
    return '\n\n'.join(out)

if __name__ == '__main__':
    src = open(sys.argv[1]).read()
    sys.stdout.write(transform_toplevel(src))
