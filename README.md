# HTML Rendering for Common Lisp

A simple HTML rendering package for Common Lisp. It properly encodes inner text and tag attributes. It also knows about singleton attributes (e.g. `<link>` and `<br>`) and language tags (e.g. `<style>` and `<script>`).

## Quickstart

The most common function you'll use to render HTML is the `html-render` function:

    (html-render form &optional stream)

It renders an arbitrary Lisp form into HTML. If no stream is provided, then the output is to a string. Many different Lisp types are supported, and sequences are recursively rendered.

    CL-USER > (html-render "Hello")
    "Hello"

    CL-USER > (html-render "This & That")
    "This &amp; That"

    CL-USER > (html-render '(a b c))
    "ABC"

At a lower-level, `html-render` works by calling into `html-format`: a generic method which handles encoding entities and generating the output for tags, DOCTYPE declarations, and CDATA.

    (html-format stream form &optional colonp atp &rest args)

The `html-format` function is designed to be callable from within a `format` call using [`~/`](http://www.lispworks.com/documentation/HyperSpec/Body/22_ced.htm). The only optional argument that is used is *colonp*, which (if T) will force encoding of HTML entities. You should almost never have to worry about this, as the code should just "do the right thing".

## HTML Tags

Internal to the `html` package are three classes which handle rendering of all HTML elements:

* HTML-TAG
* HTML-DOCTYPE
* HTML-CDATA

While you do not construct these directly (with `make-instance`), they can be constructed using tag functions which are declared with the `define-html-tag` macro.

    (define-html-tag name)

The *name* should be the tag name that is output via `html-render`, but otherwise is just a symbol. A function will be created that wraps your tag in angle brackets (<>), which can then be called to construct a tag with attributes and elements.

*All HTML5 tags are already defined for you.*

For example:

    CL-USER > (<img> :src "lolcat.png")
    #<HTML-TAG "IMG">

Now, let's render it.

    CL-USER > (html-render *)
    "<IMG SRC='lolcat.png'>"

One thing to notice is that the `html` package is aware that the "IMG" tag is a singleton HTML tag (it has no close tag) and renders it appropriately.

When calling a tag function, all keywords passed in are assumed to be attributes with the value immediately following it (which can be NIL for singleton attributes). Any other value is an element in the tag. There is no requirement for attribute to preceed child elements.

Let's try another example:

    CL-USER > (html-render (<ul> :class "ex-1" (loop for i below 3 collect (<li> i))))
    "<UL CLASS='ex-1'><LI>0</LI><LI>1</LI><LI>2</LI></UL>

## DOCTYPE and CDATA

In addition to the HTML5 tag functions, there are two additional functions you can use to create HTML elements:

    (<!doctype> &optional root)

This will create an `html-doctype` element that will render the <!DOCTYPE> declaration.

    (<!cdata> &optional data)

This will create an `html-cdata` element that will properly render <![CDATA[..]]> sections for you with unencoded data.

    CL-USER > (html-render (<body> (<!cdata> "<This & That>")))
    "<BODY><![CDATA[<This & That>]]></BODY>"

The doctype root and cdata data are optional parameters. If nil, the DOCTYPE declaration will not render and neither will the CDATA block. However, you can use these function to create an empty object that you can then modify for use later.

## Accessor Methods

`html-tag` accessors

    (html-tag-name tag)           ;=> string
    (html-tag-attributes tag)     ;=> associative-list
    (html-tag-elements tag)       ;=> form list

`html-doctype` accessors

    (html-doctype-root doctype)   ;=> string

`html-cdata` accessors

    (html-cdata-data cdata)       ;=> string

That's it!
