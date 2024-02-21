(ns threading.doo-test
  (:require [cljs.test :as test]
            [doo.runner :refer-macros [doo-tests]]
            [threading.core-test]))

(enable-console-print!)
(doo-tests 'threading.core-test)
