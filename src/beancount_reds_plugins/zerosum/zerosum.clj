(ns beancount-reds-plugins.zerosum.zerosum
  (:require [clojure.string :as str]))

(def ^:private DEFAULT-TOLERANCE 0.0099M)

(defn- posting-opposite?
  "True if posting `p` is an equal-and-opposite match for `posting` in `zs-account`."
  [p posting zs-account tolerance]
  (and (= (:acc p) zs-account)
       (:units p)
       (:units posting)
       (= (:cur p) (:cur posting))
       (< (abs (+ (:units p) (:units posting))) tolerance)))

(defn- find-match
  "Scan forward from local-i in txns-with-idx for a posting opposite to the given one.
   Returns [global-idx posting-idx] or nil. Excludes [local-i skip-pi] (self-match)
   and any postings already in the `matched` set."
  [txns-with-idx local-i skip-pi posting zs-account date-range tolerance matched]
  (let [max-date (.plusDays ^java.time.LocalDate (:date (second (nth txns-with-idx local-i)))
                             (long date-range))]
    (loop [j local-i]
      (when (< j (count txns-with-idx))
        (let [[global-j txn-j] (nth txns-with-idx j)]
          (if (.isAfter ^java.time.LocalDate (:date txn-j) max-date)
            nil
            (or (first
                  (keep-indexed
                    (fn [pi p]
                      (when (and (not (and (= j local-i) (= pi skip-pi)))
                                 (not (contains? matched [global-j pi]))
                                 (posting-opposite? p posting zs-account tolerance))
                        [global-j pi]))
                    (:postings txn-j)))
                (recur (inc j)))))))))

(defn- match-zerosum-account
  "Returns [modified-directives new-account-names] after matching postings for one account."
  [directives zs-account target-account date-range tolerance]
  (let [txns-with-idx (vec (keep-indexed
                             (fn [i dct]
                               (when (and (= :txn (:dct dct))
                                          (some #(= (:acc %) zs-account) (:postings dct)))
                                 [i dct]))
                             directives))
        matched  (volatile! #{})
        mods     (volatile! {})
        new-accs (volatile! #{})]
    (doseq [[local-i [global-i txn]] (map-indexed vector txns-with-idx)
            [pi posting]             (map-indexed vector (:postings txn))
            :when (and (= (:acc posting) zs-account)
                       (not (contains? @matched [global-i pi])))]
      (when-let [[match-gi match-pi]
                 (find-match txns-with-idx local-i pi posting
                             zs-account date-range tolerance @matched)]
        (vswap! matched conj [global-i pi] [match-gi match-pi])
        (vswap! mods update global-i
                #(assoc-in (or % txn) [:postings pi :acc] target-account))
        (vswap! mods update match-gi
                #(assoc-in (or % (nth directives match-gi)) [:postings match-pi :acc] target-account))
        (vswap! new-accs conj target-account)))
    (let [directives' (if (empty? @mods)
                        directives
                        (reduce #(assoc %1 (key %2) (val %2)) (vec directives) @mods))]
      [directives' @new-accs])))

(defn- new-open-directives
  [new-accounts directives]
  (let [open-accs  (into #{} (comp (filter #(= :open (:dct %))) (map :acc)) directives)
        first-date (some :date directives)]
    (for [acc new-accounts :when (not (contains? open-accs acc))]
      {:dct :open :acc acc :date first-date :currencies #{} :metadata {:auto nil}})))

(defn booked-xf
  "Matches pairs of opposite postings in designated zero-sum accounts and moves them
   to a target account. Unmatched postings remain in the source account.

   Config (EDN map):
     {:zerosum-accounts {\"Assets:ZSA:Transfers\" [\"Assets:ZSA-Matched:Transfers\" 30]}
      :account-name-replace [\"ZeroSum-Accounts:\" \"ZSA-Matched:\"]
      :tolerance 0.0099
      :flag-unmatched false}

   `account-name-replace` is used to derive target-account from zs-account when the
   target entry in `zerosum-accounts` is blank.

   Port of https://github.com/redstreet/beancount_reds_plugins/tree/main/beancount_reds_plugins/zerosum
   Differences from Python:
   - Config is EDN instead of Python dict literal
   - Currency equality is required for a match (Python only checks numeric value)
   - match_metadata and link_transactions options are not ported"
  [{:keys [config]}]
  (fn [rf]
    (let [state (volatile! [])]
      (fn
        ([] (rf))
        ([result]
         (let [{zs-accs   :zerosum-accounts
                name-repl :account-name-replace
                tolerance :tolerance
                flag-unm  :flag-unmatched} config
               [from-str to-str] (or name-repl ["" ""])
               tol (bigdec (or tolerance DEFAULT-TOLERANCE))
               [directives' new-accs]
               (reduce (fn [[ds accs] [zs-acc [target-acc date-range]]]
                         (let [tgt (if (str/blank? target-acc)
                                     (str/replace zs-acc from-str to-str)
                                     target-acc)
                               [ds' new] (match-zerosum-account ds zs-acc tgt date-range tol)]
                           [ds' (into accs new)]))
                       [(vec @state) #{}]
                       zs-accs)
               directives'' (if flag-unm
                              (let [zs-acc-set (set (keys zs-accs))]
                                (mapv (fn [dct]
                                        (if (and (= :txn (:dct dct))
                                                 (some #(contains? zs-acc-set (:acc %))
                                                       (:postings dct)))
                                          (assoc dct :flag "!")
                                          dct))
                                      directives'))
                              directives')
               opens (new-open-directives new-accs directives'')]
           (rf (reduce rf (reduce rf result opens) directives''))))
        ([result dct]
         (vswap! state conj dct)
         result)))))
