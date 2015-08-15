# HTML Rendering for Common Lisp

A simple HTML rendering package for Common Lisp. It properly encodes inner text and tag attributes. It also knows about singleton attributes (e.g. `<link>` and `<br>`) and language tags (e.g. `<style>` and `<script>`).

## Quickstart

Only three (3) functions are exported:

    (html stream &optional form)

This is the function you'll use most often.

It writes a Lisp *form* (as HTML) to *stream*. If *stream* is `nil` then it will output the form to a string.

List forms are considered to be tags, and are expected to be in form:

    (tag-name &optional attributes &rest child-forms)

Let's try it...

    CL-USER > (html nil "Hello, world!")
    "Hello, world!"

    CL-USER > (html nil '(:h1 ((:class "big")) "This & That"))
    "<H1 CLASS='big'>This &amp; That</H1>"

    CL-USER > (html nil `(:body ()
                           (:ul ((:class "list"))
                             (:li () 1)
                             (:li () 2))
                           (:br)))
    "<BODY<UL CLASS='list'><LI>1</LI><LI>2</LI></UL><BR></BODY>"

A helper function to `html` that is also exported is `html-format`. This function is defined so that it may be used with `~/` in `format`.

    (html-format stream form &optional colonp atp &rest args)

It formats tags, attributes, and other Lisp forms as well into a stream. If the colon switch is used, then it forces HTML encoding, otherwise it does its best to determine (by tag name) whether or not to encode attributes and inner text.

Aside from the added arguments, its usage is exactly the same as `html`. But you can use it inside `format` as well:

    CL-USER > (format t "~/html-format/" '(:br))
    <BR>

Finally is `html-encode`, which is used to encode attributes and inner-text. All ASCII characters in the range 32 to 127 (excluding HTML special characters) are output directly, otherwise they are encoded.

    (html-encode stream string)

Give it a whirl...

    CL-USER > (html-encode nil "<This & That>")o
    "&lt;This &amp; That&gt;"

That's it!
