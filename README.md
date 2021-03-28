# Loading OntoLisp using ql:quickload
OntoLisp uses [Quicklisp](https://www.quicklisp.org/) to load dependencies.

Add the following block to your `~/.sbclrc` file or equivalent
or `~/.config/common-lisp/source-registry.conf`.

```lisp
(asdf:initialize-source-registry
 '(:source-registry
   (:tree #p"/path/to/OntoLisp/")
   :inherit-configuration))
```

From a repl you can then run
```lisp
(ql:quickload :ontolisp)
(owlapi::owlapi-test)
(owl-syntaxes::owl-syntaxes-test)
```
If compilation is trigger on multiple calls to `ql:quickload`
you can call `(asdf:compile-system :ontolisp)` which should prevent
the need to compile the system on each load.

# OntoLisp
A Common Lisp Framework for the Semantic Web

## Contributors

[Tom Gillespi](https://github.com/tgbugs) - thanks for the QuickLisp and ASDF changes! 

## About 

(Old README.TXT) 

```
OntoLisp (NOSA): A Semantic Web Framework for OWL 2 and OWLlink in Common Lisp

by Michael Wessel 

Version 0.9 

OntoLisp (NOSA) is a Common Lisp library providing:

- an ontology management framework inspired by the Java OWLAPI (v2.2)

  http://owlapi.sourceforge.net/

- OWl 2 parsers for OWL 2 functional and OWL 2 XML syntax
  
  http://www.w3.org/TR/owl2-syntax/

- OWL 2 renderers for OWL 2 functional, OWL 2 XML, and OWL 2 RDF syntax

- an OWLlink implementation 

  http://www.owllink.org/
   
  featuring the OWLlink XML, OWLlink functional, 
  and OWLlink S-Exression syntax, as well as a converter for these
  different syntaxes.   

OntoLisp has been tested with Allegro Common Lisp (ACL) 8.2, Lispworks
(LW) 5.2, and Steel Bank Common Lisp (SBCL) 1.0.40, using Linux and
Mac OS-X.


OntoLisp is under the Lisp Lesser GNU Public License

http://opensource.franz.com/preamble.html


OntoLisp uses the following third-party libraries / files: 

- ASDF - Another System Definition Facility by Daniel Barlow et al.

  http://common-lisp.net/project/asdf/

  as the system definition facility (ontolisp:ontolisp.asd)

- Wilbur2 by Ora Lassila

  http://www.lassila.org/

  is currently used only for the XML parsing
  (soon for RDF as well, when OWL 2 RDF support will be added in a 
  future version)

  I have made some changes to Wilbur2, please consult 
  wilbur2:wilbur.asd. 

- S-HTTP-Client by Sven Van Caekenberghe

  http://homepage.mac.com/svc/s-http-client/

  is used for s-http-client:do-http-request 

  Alternatively, CL-HTTP (http://www.cl-http.org:8001/cl-http/)
  or AllegroServe (http://allegroserve.sourceforge.net/) could 
  be used (see ontolisp:http-stream.lisp)

- the other included systems (puri, s-utils, s-sysdeps, s-base64)
  are required by S-HTTP-Client and are included here for
  convenience (if you don't use S-HTTP-CLIENT, these directories
  can be deleted) 

- the ontolisp:test; library contains (original and modified version) of
  the OWL 2 primer in different syntaxes for testing purposes, 
  
  http://www.w3.org/TR/owl2-primer/

  as well as the famous people & pets ontology in functional syntax
  by Sean Bechhofer . These ontologies are public and can be found 
  in various ontology repositories on the web, e.g.,
  
  http://owl.cs.manchester.ac.uk/repository/



In order to load OntoLisp, simply evaluate

(load "/home/<yourhomedir>/ontolisp/ontolisp.lisp") 

Please note that "~" will not work under SBCL, and evaluating the
ontolisp.lisp buffer from emacs will not work - the file has to be
loaded, due to the utilized *load-pathname*. If loading doesn't work
for you, simply edit ontolisp.lisp (at the position of the break).

Sourcecode / API documentation is currently not provided. Please work
through the provided examples in order to see what OntoLisp can do for
you. Start inspecting ontolisp:owlapi-test.lisp and evaluate

(owlapi::owlapi-test)

in your Listener to become familiar with the OWLAPI of OntoLisp.

Then, to learn about the OWL 2 parsing and rendering facilities as
well as the OWLlink processor and converter, inspect
ontolisp:owl-syntaxes-test.lisp and evaluate

(owl-syntaxes::owl-syntaxes-test)

in your Listener.

Please note that OntoLisp currently does not come with a DL reasoner
and hence cannot really perform reasoning. Rather. a set of dummy
functions is provided as the bridge to the reasoner which produce some
output.  I will add a DL reasoner in a future version of OntoLisp.

Hence, the framework currently only addresses the syntactic aspects /
management of OWL 2 and OWL ontologies. It is rather straight-forward
to connect OntoLisp to an existing KRSS-compliant DL reasoner. Please
consult the file ontolisp:dummy-functions.lisp. These bridge functions
have to be implemented in order to attach a real DL reasoner to
OntoLisp. For illustration purposes, I have provided the reasoner
bridge implementation of RacerPro with the files
ontolisp:reasoner-bridge-for-owlapi.lisp and
ontolisp:owllink-reasoner-bridge.lisp. Of course, this can only give
you some hints on how these functions would look like for a different
KRSS-compliant reasoner.

OntoLisp was designed in a way to depend as less as possible on
third-party libraries. Consequently, aspects like URI representation
and handling may seem impoverished int he code, compared to, say,
PURI. Also the OWL 2 functional parsers etc. are hand written. But I
prefer to have things under my control and work out things for myself
rather than relying / depending on some library that I don't fully
understand. I would argue that the provided solutions are "good
enough" for what OntoLisp is trying to do, but of course, things could
be improved quite a bit. Still I hope the library contains useful
functionality and code fragments to get your OWL 2 or OWLlink project
started. The provided examples should give some hints and contain more
explanations on how to go on. Otherwise drop me a mail if you have
specific questions / problems.

Future plans:

- add OWL 2 RDF parser using Wilbur2 
- add the MiDeLoRa DL system 


Regards,

Michael Wessel, Hamburg / Germany, 2010-07-03 

mailto:michael_wessel@gmx.de
mailto:wessel@racer-systems-com 
http://www.michael-wessel.info
http://www.racer-systems.com

```
