(ns beancount.leafonly.leafonly
  (:require [clojure.string :as str]
            [limabean.plugin :as plugin]))

(defn- non-leaf-accounts
  "Return the set of account names that have at least one child account"
  [accounts]
  (into #{}
        (filter (fn [a]
                  (some (fn [b]
                          (and (not= a b)
                               (str/starts-with? b (str a ":"))))
                        accounts)))
        accounts))

(defn- first-non-leaf-posting
  "Return [txn-dct account] for the first posting to a non-leaf account, or nil"
  [directives non-leaf]
  (some (fn [dct]
          (when (= (:dct dct) :txn)
            (some (fn [p]
                    (when (contains? non-leaf (:acc p))
                      [dct (:acc p)]))
                  (:postings dct))))
        directives))

(defn booked-xf
  "Validates that no transaction has postings to a non-leaf account.
  A non-leaf account is one that has at least one child account.

  Port of https://github.com/beancount/beancount/blob/master/beancount/plugins/leafonly.py"
  [{:keys [config options]}]
  (fn [rf]
    (let [state (volatile! {:directives [] :accounts #{}})]
      (fn
        ([] (rf))
        ([result]
         (let [{:keys [directives accounts]} @state
               non-leaf (non-leaf-accounts accounts)]
           (if-let [[dct acc] (first-non-leaf-posting directives non-leaf)]
             (plugin/error! dct (str "Non-leaf account '" acc "' has postings on it"))
             (rf (reduce rf result directives)))))
        ([result dct]
         (when (= (:dct dct) :open)
           (vswap! state update :accounts conj (:acc dct)))
         (vswap! state update :directives conj dct)
         result)))))
