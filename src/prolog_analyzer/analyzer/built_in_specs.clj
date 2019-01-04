(ns prolog-analyzer.analyzer.built-in-specs)

{:pre_specs {:built-ins
             {"member" {2 [[{:spec :any} {:spec :list :type {:spec :any}}
                            {:spec :var} {:spec :list :type {:spec :any}}]
                           [{:spec :any} {:spec :var}]
                           [{:spec :var} {:spec :var}]]}}}
 :post_specs {:builts-ins
              {"member" {2 [[[{:spec :nonvar} {:spec :list :type {:spec :nonvar}}] [{:spec :nonvar} {:spec :list :type {:spec :nonvar}}]]
                            [[{:spec :var} {:spec :list :type {:spec :named-any :name "X"}}] [{:spec :named-any :name "X"} {:spec :list :type {:spec :named-any :name "X"}}]]
                            [[{:spec :named-any :name "X"} {:spec :var}] [{:spec :named-any :name "X"} {:spec :list :type {:spec :or :arglist [{:spec :named-any :name "X"} {:spec :var}]}}]]
                            [[{:spec :var} {:spec :var}] [{:spec :var} {:spec :list :type {:spec :var}}]]]}}}}
