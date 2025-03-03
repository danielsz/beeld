(ns beeld.core
  (:require [clojure.java.io :as io]
            [clj-http.client :as client]
            [kryptos.core :as crypto])
  (:import [javax.imageio ImageIO]
           [java.net URI URL]
           [java.io File]))


(defprotocol Beeld
  (file-size [x]))

(extend-protocol Beeld
  File
  (file-size [x]
    (.length x))
  URI
  (file-size [x]
    (Long/parseLong (get-in (client/head (str x)) [:headers "Content-Length"])))
  URL
  (file-size [x]
    (Long/parseLong (get-in (client/head (str x)) [:headers "Content-Length"])))
  String
  (file-size [x]
    (cond
      (.exists (io/file x)) (file-size (io/file x))
      (URL. x) (file-size (URL. x)))))

(defn file->bytes [path]
  (with-open [in (io/input-stream path)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn url->bytes [url]
  (:body (client/get url {:as :byte-array})))

(defn bytes->base64 [b]
  (crypto/encode-base64 b))

(defn detect-image-format [data]
  (let [in (ImageIO/createImageInputStream data)
        iis (ImageIO/getImageReaders in)]
    (for [reader (iterator-seq iis)]
      (.getFormatName reader))))


