; (require 'lz77 :reload)
; user=> (lz77/compress "abracadabra")
; [[0 0 \a] [0 0 \b] [0 0 \r] [3 1 \c] [2 1 \d] [7 4 nil]]
; user=> (lz77/compress "sir_sid_eastman")
; [[0 0 \s] [0 0 \i] [0 0 \r] [0 0 \_] [4 2 \d] [4 1 \e] [0 0 \a] [6 1 \t] [0 0 \m] [4 1 \n]]
(ns lz77)

(defn search
  [buffer search]
  (let [matches (map
                  #(vector % (interleave (drop (- (count buffer) %) buffer) search))
                  (range (count buffer) 0 -1))

        matches (map
                  (fn [[offset match]]
                    (->> match
                         (partition 2)
                         (take-while (partial apply =))
                         (reduce (fn [s [f _]] (conj s f)) [])
                         (vector offset)))
                  matches)

        matches (filter
                  (fn [[offset match]]
                    (> (count match) 0))
                  matches)

        max-match (apply max-key
                         (fn [[_ match]]
                           (count match))
                         (conj matches [0 nil]))
        ]
    max-match))

(defn encode
  [i window]
  (let [search-buffer (take i window)
        lookahead-buffer (drop i window)
        [offset match] (search search-buffer lookahead-buffer)
        match-len (count match)
        next-i (+ i match-len)
        next-char (get window next-i)]
    [(+ next-i 1) [offset match-len next-char]]))

(defn compress
  [text]
  (loop [table []
         [next-idx [offset len ch :as enc]] (encode 0 text)]
    (if (or (nil? ch)
            (>= next-idx (count text)))
      (conj table enc)
      (recur (conj table enc)
             (encode next-idx text)))))
