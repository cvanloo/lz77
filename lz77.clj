; (require 'lz77 :reload)
;
; user=> (lz77/compress-normal "abracadabra")
; [[0 0 \a] [0 0 \b] [0 0 \r] [3 1 \c] [2 1 \d] [7 4 nil]]
; user=> (lz77/compress-normal "sir_sid_eastman")
; [[0 0 \s] [0 0 \i] [0 0 \r] [0 0 \_] [4 2 \d] [4 1 \e] [0 0 \a] [6 1 \t] [0 0 \m] [4 1 \n]]
; user=> (lz77/compress-normal "abababcd")
; [[0 0 \a] [0 0 \b] [2 2 \a] [2 1 \c] [0 0 \d]]
; user=> (lz77/compress-with-rle "abababcd")
; [[0 0 \a] [0 0 \b] [2 4 \c] [0 0 \d]]
; user=> (lz77/compress-with-rle "ababacd")
; [[0 0 \a] [0 0 \b] [2 3 \c] [0 0 \d]]
; user=> (lz77/compress-with-rle "abdababacf")
; [[0 0 \a] [0 0 \b] [0 0 \d] [3 2 \a] [2 2 \c] [0 0 \f]]
;
; user=> (apply str (lz77/decode [[0 0 \s] [0 0 \i] [0 0 \r] [0 0 \_] [4 2 \d] [4 1 \e] [0 0 \a] [6 1 \t] [0 0 \m] [4 1 \n]]))
; "sir_sid_eastman"
; user=> (apply str (lz77/decode [[0 0 \a] [0 0 \b] [2 3 \c] [0 0 \d]]))
; "ababacd"
; user=> (apply str (lz77/decode [[0 0 \a] [0 0 \b] [2 4 \c] [0 0 \d]]))
; "abababcd"
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
    [(inc next-i) [offset match-len next-char]]))

(defn rle
  [match text]
  (if (or (nil? match)
          (empty? match))
    0
    (->> (repeat match)
         (apply concat)
         (interleave text)
         (partition 2)
         (take-while (partial apply =))
         (#(/ (count %) (count match))))))

(defn encode-with-rle
  [i window]
  (let [search-buffer (take i window)
        lookahead-buffer (drop i window)
        [offset match] (search search-buffer lookahead-buffer)
        match-len (count match)
        run-len (if (= match-len offset)
                  (rle match lookahead-buffer)
                  1)
        total-len (int (Math/ceil (* run-len match-len)))
        next-i (+ i total-len)
        next-char (get window next-i)]
    [(inc next-i) [offset total-len next-char]]))

(defn compress
  [encode-f text]
  (loop [table []
         [next-idx [offset len ch :as enc]] (encode-f 0 text)]
    (if (or (nil? ch)
            (>= next-idx (count text)))
      (conj table enc)
      (recur (conj table enc)
             (encode-f next-idx text)))))

(def compress-normal (partial compress encode))
(def compress-with-rle (partial compress encode-with-rle))

(defn decode
  [compressed]
  (reduce
    (fn [decoded [offset length ch]]
      (let [s (take length
                    (drop (- (count decoded)
                             offset)
                          decoded))
            s (take length (apply concat (repeat s)))]
        (concat decoded s [ch])))
    []
    compressed))

(defn decompress
  [compressed]
  (apply str (decode compressed)))
