#!/bin/bash

cd ~/clojure/qudos
lein clean
lein cljsbuild once min
rsync -av resources/public/ gmp26@pan.maths.org:/www/cms/drupal/sites/understandinguncertainty.org/files/animations/for-qudos

