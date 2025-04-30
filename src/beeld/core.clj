(ns beeld.core
  (:require [clojure.java.io :as io]
            [clj-http.client :as client]
            [kryptos.core :as crypto]
            [image-resizer.resize :refer [resize-fn]]
            [image-resizer.scale-methods :refer [speed]])
  (:import [javax.imageio ImageIO]
           [java.net URI URL]
           [java.io File]
           [java.io BufferedInputStream ByteArrayOutputStream]
           [java.nio.file Files]
           [java.awt.image BufferedImage]))

(defprotocol Beeld
  (filename [x])
  (filesize [x])
  (->bytes [x])
  (->base64 [x])
  (detect-image-format [x])
  (mime-type [x])
  (write [x] [x folder])
  (scale [x] [x width height] [x width height quality])
  (clone [is n]))

(extend-protocol Beeld
  File
  (filesize [x]
    (.length x))
  (->bytes [x]
    (->bytes (io/input-stream x)))
  (->base64 [x]
    (->base64 (->bytes x)))
  (detect-image-format [x]
    (detect-image-format (io/input-stream x)))
  (mime-type [x]
    (Files/probeContentType (.toPath x)))
  (filename [x] (.getName x))
  (write
    ([x] (write x (System/getProperty "java.io.tmpdir")))
    ([x dest]
     (let [name (filename x)]
       (write (->bytes x) (str dest "/" name)))))
  (scale
    ([x]
     (scale x 750 750))
    ([x width height]
     (scale x width height speed))
    ([x width height quality]
     (scale (io/input-stream x) width height quality) ))
  URI
  (filesize [x]
    (Long/parseLong (get-in (client/head (str x)) [:headers "Content-Length"])))
  (filename [x] (filename (io/file (.getPath x))))
  URL
  (filesize [x]
    (Long/parseLong (get-in (client/head (str x)) [:headers "Content-Length"])))
  (filename [x] (filename (io/file (.getPath x))))
  (->bytes [x]
    (:body (client/get (str x) {:as :byte-array})))
  (write
    ([x] (write x (System/getProperty "java.io.tmpdir")))
    ([x folder] (let [name (filename x)]
                  (write (->bytes x) (str folder "/" name)))))
  (scale
    ([x]
     (scale x 750 750))
    ([x width height]
     (scale x width height speed))
    ([x width height quality]
     (scale (:body (client/get (str x) {:as :byte-array})) width height quality)))
  String
  (filesize [x]
    (cond
      (.exists (io/file x)) (filesize (io/file x))
      (URL. x) (filesize (URL. x))))
  (filename [x]
    (cond
      (.exists (io/file x)) (filename (io/file x))
      (URL. x) (filename (URL. x))))
  (->bytes [x]
    (cond
      (.exists (io/file x)) (->bytes (io/input-stream x))
      (URL. x) (->bytes (URL. x))))
  (scale
    ([x]
     (scale x 750 750))
    ([x width height]
     (scale x width height speed))
    ([x width height quality]
     (cond
       (.exists (io/file x)) (scale (io/file x) width height quality)
       (URL. x) (scale (URL. x) width height quality))))
  (->base64 [x]
    (->base64 (->bytes x)))
  (detect-image-format [x]
    (detect-image-format (io/input-stream x)))
  (write
    ([x] (write x (System/getProperty "java.io.tmpdir")))
    ([x dest]
     (cond
       (.exists (io/file x)) (write (->bytes (io/file x)) (str dest "/" (filename x)))
       (URL. x) (write (URL. x) dest))))
  (mime-type [x]
    (when (.exists (io/file x)) (Files/probeContentType (.toPath (io/file x)))))
  byte/1
  (->base64 [x]
    (crypto/encode-base64 x))
  (detect-image-format [x]
    (detect-image-format (io/input-stream x)))
  (write
    ([x] (write x (System/getProperty "java.io.tmpdir")))
    ([x dest]
     (with-open [w (io/output-stream dest)]
       (.write w x))))
  (scale
    ([x]
     (scale x 750 750))
    ([x width height]
     (scale x width height speed))
    ([x width height quality]
     (scale (io/input-stream x) width height quality)))
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
      (.getFormatName reader))))
  (clone [is n]
    (let [baos (ByteArrayOutputStream.)]
      (.transferTo is baos)
      (take n (repeatedly #(io/input-stream (.toByteArray baos))))))
  (scale
    ([x]
     (scale x 750 750))
    ([x width height]
     (scale x width height speed))
    ([x width height quality]
     (let [[a b] (clone x 2)
           image-format (first (detect-image-format a))
           resize-f (resize-fn width height quality)
           ^BufferedImage resized-image (resize-f b)]
       (with-open [baos (java.io.ByteArrayOutputStream.)]
         (ImageIO/write resized-image image-format baos)
         (.toByteArray baos)))))
  (write
    ([x] (write x (System/getProperty "java.io.tmpdir")))
    ([x dest]
     (write (->bytes x) dest))))

