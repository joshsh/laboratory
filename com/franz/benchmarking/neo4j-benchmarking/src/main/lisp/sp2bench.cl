(require :agraph "/home/josh/opt/_agraph/agraph-4.8-linuxamd64.64-client-lisp/agraph4.fasl")

(in-package :db.agraph.user)
(enable-!-reader)
(enable-print-decoded t)
(setf *default-ag-http-port* 10042)

(open-triple-store "sp2bench-50k" :catalog "testing" :read-only t)
(open-triple-store "sp2bench-1m" :catalog "testing" :read-only t)

(triple-count)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq q1 "PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX dc:      <http://purl.org/dc/elements/1.1/>
          PREFIX dcterms: <http://purl.org/dc/terms/>
          PREFIX bench:   <http://localhost/vocabulary/bench/>
          PREFIX xsd:     <http://www.w3.org/2001/XMLSchema#>

          SELECT ?yr
          WHERE {
            ?journal rdf:type bench:Journal .
            ?journal dc:title 'Journal 1 (1940)'^^xsd:string .
            ?journal dcterms:issued ?yr
          }")

(setq q2 "PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
          PREFIX swrc:    <http://swrc.ontoware.org/ontology#>
          PREFIX foaf:    <http://xmlns.com/foaf/0.1/>
          PREFIX bench:   <http://localhost/vocabulary/bench/>
          PREFIX dc:      <http://purl.org/dc/elements/1.1/>
          PREFIX dcterms: <http://purl.org/dc/terms/>

          SELECT ?inproc ?author ?booktitle ?title
                 ?proc ?ee ?page ?url ?yr ?abstract
          WHERE {
            ?inproc rdf:type bench:Inproceedings .
            ?inproc dc:creator ?author .
            ?inproc bench:booktitle ?booktitle .
            ?inproc dc:title ?title .
            ?inproc dcterms:partOf ?proc .
            ?inproc rdfs:seeAlso ?ee .
            ?inproc swrc:pages ?page .
            ?inproc foaf:homepage ?url .
            ?inproc dcterms:issued ?yr
            OPTIONAL {
              ?inproc bench:abstract ?abstract
            }
          }
          ORDER BY ?yr")

(setq q3a "PREFIX rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           PREFIX bench: <http://localhost/vocabulary/bench/>
           PREFIX swrc:  <http://swrc.ontoware.org/ontology#>

           SELECT ?article
           WHERE {
             ?article rdf:type bench:Article .
             ?article ?property ?value
             FILTER (?property=swrc:pages)
           }")

(setq q3b "PREFIX rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           PREFIX bench: <http://localhost/vocabulary/bench/>
           PREFIX swrc:  <http://swrc.ontoware.org/ontology#>

           SELECT ?article
           WHERE {
             ?article rdf:type bench:Article .
             ?article ?property ?value
             FILTER (?property=swrc:month)
           }")

(setq q3c "PREFIX rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           PREFIX swrc:  <http://swrc.ontoware.org/ontology#>
           PREFIX bench: <http://localhost/vocabulary/bench/>

           SELECT ?article
           WHERE {
             ?article rdf:type bench:Article .
             ?article ?property ?value
             FILTER (?property=swrc:isbn)
           }")

(setq q4 "PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX bench:   <http://localhost/vocabulary/bench/>
          PREFIX dc:      <http://purl.org/dc/elements/1.1/>
          PREFIX dcterms: <http://purl.org/dc/terms/>
          PREFIX foaf:    <http://xmlns.com/foaf/0.1/>
          PREFIX swrc:    <http://swrc.ontoware.org/ontology#>

          SELECT DISTINCT ?name1 ?name2
          WHERE {
            ?article1 rdf:type bench:Article .
            ?article2 rdf:type bench:Article .
            ?article1 dc:creator ?author1 .
            ?author1 foaf:name ?name1 .
            ?article2 dc:creator ?author2 .
            ?author2 foaf:name ?name2 .
            ?article1 swrc:journal ?journal .
            ?article2 swrc:journal ?journal
            FILTER (?name1<?name2)
          }")

(setq q5a "PREFIX rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           PREFIX foaf:  <http://xmlns.com/foaf/0.1/>
           PREFIX bench: <http://localhost/vocabulary/bench/>
           PREFIX dc:    <http://purl.org/dc/elements/1.1/>

           SELECT DISTINCT ?person ?name
           WHERE {
             ?article rdf:type bench:Article .
             ?article dc:creator ?person .
             ?inproc rdf:type bench:Inproceedings .
             ?inproc dc:creator ?person2 .
             ?person foaf:name ?name .
             ?person2 foaf:name ?name2
             FILTER (?name=?name2)
           }")

(setq q5b "PREFIX rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           PREFIX foaf:  <http://xmlns.com/foaf/0.1/>
           PREFIX bench: <http://localhost/vocabulary/bench/>
           PREFIX dc:    <http://purl.org/dc/elements/1.1/>

           SELECT DISTINCT ?person ?name
           WHERE {
             ?article rdf:type bench:Article .
             ?article dc:creator ?person .
             ?inproc rdf:type bench:Inproceedings .
             ?inproc dc:creator ?person .
             ?person foaf:name ?name
           }")

(setq q6 "PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
          PREFIX foaf:    <http://xmlns.com/foaf/0.1/>
          PREFIX dc:      <http://purl.org/dc/elements/1.1/>
          PREFIX dcterms: <http://purl.org/dc/terms/>

          SELECT ?yr ?name ?document
          WHERE {
            ?class rdfs:subClassOf foaf:Document .
            ?document rdf:type ?class .
            ?document dcterms:issued ?yr .
            ?document dc:creator ?author .
            ?author foaf:name ?name
            OPTIONAL {
              ?class2 rdfs:subClassOf foaf:Document .
              ?document2 rdf:type ?class2 .
              ?document2 dcterms:issued ?yr2 .
              ?document2 dc:creator ?author2
              FILTER (?author=?author2 && ?yr2<?yr)
            } FILTER (!bound(?author2))
          }")

(setq q7 "PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
          PREFIX foaf:    <http://xmlns.com/foaf/0.1/>
          PREFIX dc:      <http://purl.org/dc/elements/1.1/>
          PREFIX dcterms: <http://purl.org/dc/terms/>

          SELECT DISTINCT ?title
          WHERE {
            ?class rdfs:subClassOf foaf:Document .
            ?doc rdf:type ?class .
            ?doc dc:title ?title .
            ?bag2 ?member2 ?doc .
            ?doc2 dcterms:references ?bag2
            OPTIONAL {
              ?class3 rdfs:subClassOf foaf:Document .
              ?doc3 rdf:type ?class3 .
              ?doc3 dcterms:references ?bag3 .
              ?bag3 ?member3 ?doc
              OPTIONAL {
                ?class4 rdfs:subClassOf foaf:Document .
                ?doc4 rdf:type ?class4 .
                ?doc4 dcterms:references ?bag4 .
                ?bag4 ?member4 ?doc3
              } FILTER (!bound(?doc4))
            } FILTER (!bound(?doc3))
          }")

(setq q8 "PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>
          PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX dc:   <http://purl.org/dc/elements/1.1/>

          SELECT DISTINCT ?name
          WHERE {
            ?erdoes rdf:type foaf:Person .
            ?erdoes foaf:name 'Paul Erdoes'^^xsd:string .
            {
              ?document dc:creator ?erdoes .
              ?document dc:creator ?author .
              ?document2 dc:creator ?author .
              ?document2 dc:creator ?author2 .
              ?author2 foaf:name ?name
              FILTER (?author!=?erdoes &&
                      ?document2!=?document &&
                      ?author2!=?erdoes &&
                      ?author2!=?author)
            } UNION {
              ?document dc:creator ?erdoes.
              ?document dc:creator ?author.
              ?author foaf:name ?name
              FILTER (?author!=?erdoes)
            }
          }")

(setq q9 "PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>

          SELECT DISTINCT ?predicate
          WHERE {
            {
              ?person rdf:type foaf:Person .
              ?subject ?predicate ?person
            } UNION {
              ?person rdf:type foaf:Person .
              ?person ?predicate ?object
            }
          }")

(setq q10 "PREFIX person: <http://localhost/persons/>

           SELECT ?subject ?predicate
           WHERE {
             ?subject ?predicate person:Paul_Erdoes
           }")

(setq q11 "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

           SELECT ?ee
           WHERE {
             ?publication rdfs:seeAlso ?ee
           }
           ORDER BY ?ee
           LIMIT 10
           OFFSET 50")

(setq q12a "PREFIX rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX foaf:  <http://xmlns.com/foaf/0.1/>
            PREFIX bench: <http://localhost/vocabulary/bench/>
            PREFIX dc:    <http://purl.org/dc/elements/1.1/>

            ASK {
              ?article rdf:type bench:Article .
              ?article dc:creator ?person1 .
              ?inproc  rdf:type bench:Inproceedings .
              ?inproc  dc:creator ?person2 .
              ?person1 foaf:name ?name1 .
              ?person2 foaf:name ?name2
              FILTER (?name1=?name2)
            }")

(setq q12b "PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>
            PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>
            PREFIX dc:   <http://purl.org/dc/elements/1.1/>

            ASK {
              ?erdoes rdf:type foaf:Person .
              ?erdoes foaf:name 'Paul Erdoes'^^xsd:string .
              {
                ?document dc:creator ?erdoes .
                ?document dc:creator ?author .
                ?document2 dc:creator ?author .
                ?document2 dc:creator ?author2 .
                ?author2 foaf:name ?name
                FILTER (?author!=?erdoes &&
                        ?document2!=?document &&
                        ?author2!=?erdoes &&
                        ?author2!=?author)
              } UNION {
                ?document dc:creator ?erdoes .
                ?document dc:creator ?author .
                ?author foaf:name ?name
                FILTER (?author!=?erdoes)
              }
            }")

(setq q12c "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX person: <http://localhost/persons/>
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>

            ASK {
              person:John_Q_Public rdf:type foaf:Person.
            }")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-select-query (query-str)
    (let ((query (sparql.parser::parse-sparql query-str)))
        (setq count 0)
        (let ((result (sparql:run-sparql query :results-format :alists)))
            (loop for r in result do (setq count (+ 1 count))))
        count))

(defun evaluate-ask-query (query-str)
    (let ((query (sparql.parser::parse-sparql query-str)))
        (sparql:run-sparql query)))

(defun time-query (query-str iters iterate)
    (let ((query (sparql.parser::parse-sparql query-str)))
        (let ((real1 (get-internal-real-time)))
            (loop for i from 1 to iters do
                (let ((result (sparql:run-sparql query :results-format :alists)))
                    (if iterate (loop for r in result do (setq nothing nil)))))
        (let ((real2 (get-internal-real-time)))
		    (- real2 real1)))))

(defun time-query-n (query-str iters sets iterate)
    (setq result ())
    (loop for i from 1 to sets do
        (let ((tm (time-query query-str iters iterate)))
            (setq result (cons tm result))))
    (reverse result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(count-select-query q1)    ;; 1
(count-select-query q2)    ;; 965
(count-select-query q3a)   ;; 3647
(count-select-query q3b)   ;; 25
(count-select-query q3c)   ;; 0
(count-select-query q4)    ;; 104746
(count-select-query q5a)   ;; 1085
(count-select-query q5b)   ;; 1085
(count-select-query q6)    ;;
(count-select-query q7)    ;;
(count-select-query q8)    ;;
(count-select-query q9)    ;;
(count-select-query q10)   ;;
(count-select-query q11)   ;;

(evaluate-ask-query q12a)  ;;
(evaluate-ask-query q12b)  ;;
(evaluate-ask-query q12c)  ;;


(time-query-n q1 100 10 t)
(time-query-n q2 100 10 t)
(time-query-n q3a 100 10 t)
(time-query-n q3b 100 10 t)
(time-query-n q3c 100 10 t)
(time-query-n q4 100 10 t)
(time-query-n q5a 100 10 t)
(time-query-n q5b 100 10 t)



(time-query-n q1 100 10 nil)


