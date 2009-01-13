;; fowler_dsl.nu
;;
;; INTRODUCTION
;;
;; This is a Nu implementation of a Domain Specific Language example that
;; appeared in the following article by Martin Fowler:
;;   "Language Workbenches: The Killer-App for Domain Specific Languages?"
;;   http://martinfowler.com/articles/languageWorkbench.html
;;
;; This Nu version is adapted from a nicely-done Common Lisp version of
;; the example by Rainer Joswig.  After Rainer saw the Java and C# versions
;; that Martin Fowler and others had done in the above article, he knew he
;; could do better.
;;
;; Rainer has an article explaining his take on the example problem:
;;   http://lispm.dyndns.org/news?ID=NEWS-2005-07-08-1
;;
;; Even better, watch Rainer's screencast in which he develops
;; the solution from scratch in about 15 minutes using Common Lisp.
;;   http://www.cl-http.org:8002/mov/dsl-in-lisp.mov
;;
;;
;; I ported Rainer's Common Lisp version to Nu as part of testing out
;; the new macro-1 features in Nu 0.4.  I added a simple dispatching
;; mechanism and some sample debugging output as part of the
;; code-generation in the macro.
;;
;;
;; SYSTEM REQUIREMENTS
;;
;; This code requires Nu 0.4 or above.
;;
;;
;; AUTHOR
;;
;; The original Common Lisp code was written by Rainer Joswig.
;; This Nu version was written by Jeff Buck.


;; ------------------------------------------------------------------
;; The original version of this code used Common Lisp's built-in
;; dispatching mechanism to dispatch on class type.
;;
;; We'll make our own simple dispatcher that returns the appropriate
;; class name for a given record type.
(set $mappingDispatcher ((NSMutableDictionary alloc) init))


;; The first 4 characters of the data describes the type of record,
;; which will be what we dispatch on.
(set $dispatchFieldRange '(0 4))

;; ------------------------------------------------------------------
;; The "defmapping" macro does most of the heavy lifting.  We generate
;; a class for each record type.
;;
;; The main method in the class is initWithData.
;; initWithData is passed a string of data that corresponds to a single
;; record.  Each of the fixed length fields in the data string is parsed
;; out according to the mapping spec (held in the *description parameter).
;; An ivar is then set for each parsed field.
;;
;; The dump method is just a simple debugging tool that prints out
;; a representation of the class.
;;
;; The last thing the macro does is add an entry for the class to the
;; dispatching dictionary.
;;
;; The macro parameters:
;;   name - the name of the class to generate
;;   type - the record type that we'll dispatch on
;;   description - the mappings for a fixed-field record
;;
(macro-1 defmapping (name type *description)
     `(progn
            (class ,name is NSObject
                 (ivars)
                 (ivar-accessors)
                 
                 ;; create an class instance that will be
                 ;; the representation of the passed-in string
                 ;; data
                 (- (id) initWithData: (id) data is
                    (super init)
                    (',*description each:
                     (do (v)
                         (self setValue:(data substringWithRange:
                                              (list (first v) (+ 1 (- (second v) (first v)))))
                               forIvar:((third v) stringValue))))
                    self)
                 
                 ;; generate a dump function for debug output
                 (- (void) dump is
                    (puts (+ "<Instance of: " ,name " Type: " ,type ">"))
                    ((quote ,*description) each:
                     (do (v)
                         (print (+ ((third v) stringValue) ":  "))
                         (print (self valueForIvar:((third v) stringValue)))
                         (puts "")))
                    (puts "")))
            
            ;; Add an entry for this mapping class to the dispatcher
            ($mappingDispatcher setValue:,name forKey:,type)))


;; Given a string of records (one per line), create an array
;; of objects of the appropriate type.
;;
(function parseRecords (data)
     ((data lines) map:
      (do (l)
          (set dispatchclass
               ($mappingDispatcher valueForKey:(l substringWithRange:$dispatchFieldRange)))
          ;; skip mappings that we don't have a dispatch class for
          (if (dispatchclass)
              (then ((dispatchclass alloc) initWithData:l))))))


;; ------------------------------------------------------------------
;; The example mappings from Martin Fowler's article...
;;
(defmapping serviceCall "SVCL"
     (4 18 customerName)
     (19 23 customerID)
     (24 27 callTypeCode)
     (28 35 dateOfCallString))

(defmapping usage "USGE"
     (4 8 customerID)
     (9 22 customerName)
     (30 30 cycle)
     (31 36 readDate))


;; The example data from Martin Fowler's article.
;;
(set exampleData <<-END
SVCLFOWLER         10101MS0120050313.........................
SVCLHOHPE          10201DX0320050315........................
SVCLTWO           x10301MRP220050329..............................
BOGUS
USGE10301TWO          x50214..7050329...............................
END
)


;; ------------------------------------------------------------------
;; Parse the data and generate the objects
(set records (parseRecords exampleData))

;; Dump out the classes just as a test
(records each: (do (c) (c dump)))


