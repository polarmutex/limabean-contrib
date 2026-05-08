(ns beancount.plugins.check-commodity
  (:require [limabean.plugin :as plugin]))

(defn- posting-occurrences [posting]
  (cond-> [[(:acc posting) (:cur posting)]]
    (:cost posting)  (conj [(:acc posting) (get-in posting [:cost :cur])])
    (:price posting) (conj [(:acc posting) (get-in posting [:price :cur])])))

(defn- dct-occurrences [dct]
  (case (:dct dct)
    :open    (for [c (:currencies dct)] [(:acc dct) c])
    :txn     (mapcat posting-occurrences (:postings dct))
    :balance [[(:acc dct) (:cur dct)]]
    :price   [["Price Directive Context" (:cur dct)]
              ["Price Directive Context" (get-in dct [:price :cur])]]
    nil))

(defn booked-xf
  "Validates that all commodities used have a corresponding Commodity directive.

  Port of https://github.com/beancount/beancount/blob/master/beancount/plugins/check_commodity.py"
  [{:keys [config options]}]
  (fn [rf]
    (let [state (volatile! {:directives [] :commodity-map {} :occurrences #{}})]
      (fn
        ([] (rf))
        ([result]
         (let [{:keys [directives commodity-map occurrences]} @state
               missing (some (fn [[ctx cur]]
                               (when-not (contains? commodity-map cur)
                                 [ctx cur]))
                             (sort occurrences))]
           (if missing
             (let [[ctx cur] missing]
               (plugin/error! (first directives)
                              (str "Missing Commodity directive for '" cur
                                   "' in '" ctx "'")))
             (rf (reduce rf result directives)))))
        ([result dct]
         (when (= (:dct dct) :commodity)
           (vswap! state update :commodity-map assoc (:cur dct) dct))
         (when-let [occs (seq (dct-occurrences dct))]
           (vswap! state update :occurrences into occs))
         (vswap! state update :directives conj dct)
         result)))))
