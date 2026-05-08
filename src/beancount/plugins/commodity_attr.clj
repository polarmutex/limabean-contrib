(ns beancount.plugins.commodity-attr
  (:require [clojure.string :as str]
            [limabean.plugin :as plugin]))

(defn- meta-value
  "Return the raw value for metadata attribute `attr` on directive `dct`, or nil."
  [dct attr]
  (when-let [mv (get (:metadata dct) attr)]
    (first (vals mv))))

(defn raw-xf
  "Validates that all Commodity directives have required attributes with valid values.

  Config is an EDN map from attribute keyword to a vector of valid string values,
  or nil to accept any value (just requiring the attribute is present):

    {:sector [\"Technology\" \"Financials\" \"Energy\"]
     :name nil}

  Port of https://github.com/beancount/beancount/blob/master/beancount/plugins/commodity_attr.py"
  [{:keys [config]}]
  (let [validmap (into {}
                       (map (fn [[attr values]]
                              [attr (when values (set values))]))
                       config)]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result dct]
         (when (= (:dct dct) :commodity)
           (doseq [[attr valid-values] validmap]
             (let [value (meta-value dct attr)]
               (cond
                 (nil? value)
                 (plugin/error! dct
                   (str "Missing attribute '" (name attr)
                        "' for Commodity directive " (:cur dct)))

                 (and valid-values (not (contains? valid-values value)))
                 (plugin/error! dct
                   (str "Invalid value '" value "' for attribute " (name attr)
                        ", Commodity directive " (:cur dct)
                        "; valid options: " (str/join ", " (sort valid-values))))))))
         (rf result dct))))))
