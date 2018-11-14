---
title: Validate JSON Schema with Org mode
author: creichert
date: 2018-11-14
layout: post
---

Recently, I helped implement a [JSON Schema Validation
API](https://assertible.com/blog/automatically-import-openapi-v3-response-json-schema-assertions)
for Assertible that is free to use.

I manage a large portion of my agenda from `org-mode`, and part of my job on the
front-line of support and customer interaction is to field requests about JSON
Schema. Naturally, it would make sense if I could seamlessly work with JSON
Schema documents from Org mode.

Here are some snippets I use to validate [JSON Schema](https://json-schema.org)
documents for assiting [Assertible](https://assertible.com) users and my own
testing:


- **Validate JSON literals**
  ```org-mode
  #+NAME: org-src-json-schema-literals
  #+BEGIN_SRC shell :results pp
  curl -s https://assertible.com/json -d '{
    "schema": {},
    "json": {}
    }' | jq .
  #+END_SRC
  
  #+RESULTS: org-src-json-schema-literals
  : {
  :   "errors": [],
  :   "valid": true
  : }
  ```
- **Validating JSON Schema files**
  ```org-mode
  #+NAME: org-src-json-schema-files
  #+BEGIN_SRC shell :results pp
  SCHEMA=$(cat $HOME/dev/rbros/assertible/test/data/simplyrets-api-json-schema.json)
  JSON=$(cat $HOME/dev/rbros/assertible/test/data/simplyrets-api-response-json-schema.json)
  curl -s https://assertible.com/json -d "{
      \"schema\": $SCHEMA,
      \"json\": $JSON
    }" | jq .
  #+END_SRC

  #+RESULTS: org-src-json-schema-files
  : {
  :   "errors": [],
  :   "valid": true
  : }
  ```
- **Good validation errors**
  The Assertible JSON Schema Validation API gives back decent errors too
  ```org-mode
  #+NAME: org-src-json-schema-errors
  #+BEGIN_SRC shell :results pp
  curl -s https://assertible.com/json -d '{
    "schema": {"type":"object"},
    "json": null
    }' | jq .errors[]
  #+END_SRC

  #+RESULTS: org-src-json-schema-errors
  : "failed to validate type - `null` is not a object"
  ```
