#!/bin/bash

cd ~/clojure/qudos
lein clean
lein cljsbuild once min
rsync -av resources/public/ gmp26@webuu1.maths.cam.ac.uk:/www/drupal/sites/understandinguncertainty.org/files/animations/for-qudos

