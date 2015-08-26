# HTML Rendering for Common Lisp

A simple HTML rendering package for Common Lisp. It properly encodes inner text and tag attributes. It also knows about singleton attributes (e.g. `<link>` and `<br>`) and language tags (e.g. `<style>` and `<script>`).

## Quickstart

Only a few functions are exported:

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

Additionally, the values of attributes are formatted!

    CL-USER > (html nil `(:body ()
                           (:ul ((:class "~:[dark~;light~]-theme" t))
                             (:li () 1)
                             (:li () 2))
                           (:br)))
    "<BODY<UL CLASS='light-theme'><LI>1</LI><LI>2</LI></UL><BR></BODY>"

While `html` is used to generate the HTML for a single tag/form, you can use the `html-page` function to generate simple HTML for an entire page:

    (html-page stream title &key meta scripts stylesheets body)

Thie will generate an `<HTML>` tag, with a `<HEAD>` and `<BODY>` for you. The `<HEAD>` tag will contain `<TITLE>`, `<META>`, `<LINK>`, and `<SCRIPT>` tags as denoted by the arguments. The *body* - if provided, should be a list of forms.

    CL-USER > (html-page nil
                         "Test"
                         :meta '(("charset" "utf-8"))
                         :scripts '("jquery.js")
                         :stylesheets '("dark.css")
                         :body '((:h1 () "Hello,world!")))
    "<HTML><HEAD><TITLE>Test</TITLE><META charset='utf-8'><SCRIPT SRC='jquery.js' TYPE='text/javascript'></SCRIPT><LINK HREF='dark.cc' REL='stylesheet'></HEAD><BODY><H1>Hello, world!</H1></BODY></HTML>"

Finally, if you just need to encode a string to be property HTML encoded, you can do that with `html-encode`.

    (html-encode stream string)

Give it a whirl...

    CL-USER > (html-encode nil "<This & That>")
    "&lt;This &amp; That&gt;"

That's it!
