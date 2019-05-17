(ns special-palindrome.core
  (:gen-class))

(defn remove-letter [letter-frequencies letter-to-decrement]
  (let [frequency-of-letter (get letter-frequencies letter-to-decrement)]
    ; skip removing if the letter-frequency is nil
    (if (not frequency-of-letter) letter-frequencies
    (cond (= frequency-of-letter 1) (dissoc letter-frequencies letter-to-decrement)
          :else
          (update letter-frequencies letter-to-decrement - 1)))))

(defn get-permutations
  ([letters-to-add]
   (let [letters-to-add-frequencies (frequencies letters-to-add)]
     (get-permutations letters-to-add-frequencies (list) "")))

  ([letter-frequencies-from-last-call built-string letter-to-remove]
   (let [letters-to-add (remove-letter letter-frequencies-from-last-call letter-to-remove)]
     (cond (empty? letters-to-add)
        (list (str built-string)) ; turn the built list into a string
        :else
        (do
        (flatten (reduce conj
                (map #(get-permutations letters-to-add (conj built-string %)  %) (keys letters-to-add)))))))))

(defn -main [& args]
  (let [input "asasd"
        as-array (seq (char-array input))
        permutations (get-permutations as-array)]
    (println as-array)
    (println (type as-array))
    (println permutations)
    ))

