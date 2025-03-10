(ns beeld.core
  (:require [clojure.java.io :as io]
            [clj-http.client :as client]
            [kryptos.core :as crypto])
  (:import [javax.imageio ImageIO]
           [java.net URI URL]
           [java.io File]
           [java.io BufferedInputStream]
           [java.nio.file Files]))

(defprotocol Beeld
  (file-size [x])
  (->bytes [x])
  (->base64 [x])
  (detect-image-format [x])
  (mime-type [x]))

(extend-protocol Beeld
  File
  (file-size [x]
    (.length x))
  (->bytes [x]
    (->bytes (io/input-stream x)))
  (->base64 [x]
    (->base64 (->bytes x)))
  (detect-image-format [x]
    (detect-image-format (io/input-stream x)))
  (mime-type [x]
    (Files/probeContentType (.toPath x)))
  URI
  (file-size [x]
    (Long/parseLong (get-in (client/head (str x)) [:headers "Content-Length"])))
  URL
  (file-size [x]
    (Long/parseLong (get-in (client/head (str x)) [:headers "Content-Length"])))
  (->bytes [x]
    (:body (client/get (str x) {:as :byte-array})))
  String
  (file-size [x]
    (cond
      (.exists (io/file x)) (file-size (io/file x))
      (URL. x) (file-size (URL. x))))
  (->bytes [x]
    (cond
      (.exists (io/file x)) (->bytes (io/input-stream x))
      (URL. x) (->bytes (URL. x))))
  (->base64 [x]
    (->base64 (->bytes x)))
  (detect-image-format [x]
    (detect-image-format (io/input-stream x)))
  (mime-type [x]
    (when (.exists (io/file x)) (Files/probeContentType (.toPath (io/file x)))))
  byte/1
  (->base64 [x]
    (crypto/encode-base64 x))
  (detect-image-format [x]
    (detect-image-format (io/input-stream x)))
  BufferedInputStream
  (->bytes [x]
    (with-open [in x
                out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
      (.toByteArray out)))
  (detect-image-format [x]
    (let [in (ImageIO/createImageInputStream x)
        iis (ImageIO/getImageReaders in)]
    (for [reader (iterator-seq iis)]
      (.getFormatName reader)))))

