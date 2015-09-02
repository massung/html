# HTML Rendering for Common Lisp

A simple HTML rendering package for Common Lisp. It properly encodes inner text and tag attributes. It also knows about singleton attributes (e.g. `<link>` and `<br>`) and language tags (e.g. `<style>` and `<script>`).

## Quickstart

Only a couple functions and a single macro are exported.

    (html-render form)

    (html-format stream form &optional colonp atp &rest args)

The `html-render` function converts a Lisp *form* (as HTML) into a string (optionally writing to a stream instead). Atoms are HTML encoded and written to the stream as-is. Lists are considered tags, and are expected to be in form:

    (tag-name &optional attributes &rest child-forms)

Let's try it...

    CL-USER > (html "Hello, world!")
    "Hello, world!"

    CL-USER > (html '(:h1 ((:class "<wow>")) "This & That"))
    "<H1 CLASS='&lt;wow&gt;'>This &amp; That</H1>"

The `html-format` function is designed to be callable from within a `format` call using [`~/`](http://www.lispworks.com/documentation/HyperSpec/Body/22_ced.htm). In fact, `html-render` simply wraps a call to `html-format`.

## Making It Easier

Putting together s-expressions to generate HTML can be a bit cumbersome, and usually devolves into backquoting and comma-evaluating. But, the HTML package allows for (almost) HTML-like generation of the s-expressions for you using tag functions.

For example:

    CL-USER > (<html>
               (<head>
                (<title> "Title")
                (<link> :href "style.css" :rel "stylesheet"))
               (<body>
                (<div> :class "example" "Cool, eh?")))
    (HTML NIL (HTML::HEAD NIL (HTML::TITLE NIL "Title") (HTML::LINK ((:HREF "style.css") (:REL "stylesheet")))) (HTML::BODY NIL (HTML::DIV ((:CLASS "example")) "Cool, eh?")))

And these expressions can be passed right along to `html-render`...

    CL-USER > (html-render *)
    "<HTML><HEAD><TITLE>Title</TITLE><LINK HREF='style.css' REL='stylesheet'></HEAD><BODY><DIV CLASS='example'>Cool, eh?</DIV></BODY></HTML>"

After the function, any keyword arguments are asssumed to be attributes, followed by a value. If the value is NIL, then the attribute is written as a singleton attribute. For example:

    CL-USER > (html-render (<td> :nowrap nil))
    "<TD NOWRAP></TD>"

Every HTML5 tag is automatically supported, but you can also create your own tags as well using the `define-html-tag` macro:

    (define-html-tag name)

The *name* argument should be the tag that is output to the s-expression, and so should be a symbol or string. A function with the tag name wrapped in angle-brackets (<>) is automatically written, which handles parsing of attribute keywords and content forms for you.

    CL-USER > (define-html-tag foo)
    <FOO>

    CL-USER > (html-render (<foo> :bar "baz" "Look, ma, my own tag!"))
    "<FOO BAR='baz'>Look, ma, my own tag!</FOO>"

That's it!
