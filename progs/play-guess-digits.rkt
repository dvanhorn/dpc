#lang class/1
(require class/universe)
(require "guess-client-digits.rkt")
(require "guess-server1.rkt")

(launch-many-worlds
  (big-bang (new no-guess%))
  (universe (new thinking-of% (random 1000))))