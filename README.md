# HTML Rendering for Common Lisp

A simple HTML rendering package for Common Lisp. It properly encodes inner text and tag attributes. It also knows about singleton attributes (e.g. `<link>` and `<br>`) and language tags (e.g. `<style>` and `<script>`).

## Quickstart

Only a few functions are exported:

    (html form &optional stream)
    (html-parse string &optional source)

The `html` function converts a Lisp *form* (as HTML) into a string (optionally writing to a stream instead). While the `html-parse` function will take an HTML string and parse it into a Lisp form.

List forms are considered to be tags, and are expected to be in form:

    (tag-name &optional attributes &rest child-forms)

Let's try it...

    CL-USER > (html nil "Hello, world!")
    "Hello, world!"

    CL-USER > (html nil '(:h1 ((:class "big")) "This & That"))
    "<H1 CLASS='big'>This &amp; That</H1>"

Additionally, the values of attributes are formatted!

    CL-USER > (html nil `(:body ()
                (:ul ((:class "~:[dark~;light~]-theme" t))
                  (:li () 1)
                  (:li () 2))
                (:br)))
    "<BODY<UL CLASS='light-theme'><LI>1</LI><LI>2</LI></UL><BR></BODY>"

    CL-USER > (html-parse *)
    ("BODY" NIL ("UL" (("CLASS" "light-theme")) ("LI" NIL "1") ("LI" NIL "2")) ("BR" NIL))

That's it!
