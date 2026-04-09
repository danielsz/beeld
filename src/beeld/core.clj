(ns beeld.core
  (:require [clojure.java.io :as io]
            [clj-http.client :as client]
            [image-resizer.resize :refer [resize-fn]]
            [image-resizer.rotate :refer [rotate-90-counter-clockwise-fn]]
            [image-resizer.scale-methods :refer [speed]]
            [beeld.metadata :refer [orientation]])
  (:import [javax.imageio ImageIO]
           [java.net URI URL]
           [java.io File]
           [java.io BufferedInputStream ByteArrayOutputStream]
           [java.nio.file Files]
           [java.util Base64]
           [java.awt.image BufferedImage]))

(defprotocol Beeld
  (filename [x])
  (filesize [x])
  (->bytes [x])
  (->base64 [x])
  (detect-image-format [x])
  (mime-type [x])
  (write [x] [x dest] [x n dest])
  (scale [x] [x width height] [x width height quality])
  (clone [is n]))


(defn url-string? [x]
  (try
    (let [url (URL. x)]
      (and (seq (.getProtocol url))
           (seq (.getHost url))))
    (catch java.net.MalformedURLException _ false)))


(extend-protocol Beeld
  File
  (filesize [x] (.length x))
  (->bytes [x] (->bytes (io/input-stream x)))
  (->base64 [x] (->base64 (->bytes x)))
  (detect-image-format [x] (detect-image-format (io/input-stream x)))
  (mime-type [x] (Files/probeContentType (.toPath x)))
  (filename [x] (.getName x))
  (write
    ([x] (write x (System/getProperty "java.io.tmpdir")))
    ([x dest] (write x (filename x) dest))
    ([x n dest] (write (->bytes x) (str dest "/" n))))
  (scale
    ([x] (scale x 750 750))
    ([x width height] (scale x width height speed))
    ([x width height quality] (scale (io/input-stream x) width height quality)))

  URI
  (filesize [x] (Long/parseLong (get-in (client/head (str x)) [:headers "Content-Length"])))
  (filename [x] (filename (io/file (.getPath x))))
  (->bytes [x] (->bytes (.toURL x)))
  (->base64 [x] (->base64 (.toURL x)))
  (detect-image-format [x] (detect-image-format (.toURL x)))
  (mime-type [x] (mime-type (.toURL x)))
  (write
    ([x] (write x (System/getProperty "java.io.tmpdir")))
    ([x dest] (write x (filename x) dest))
    ([x n dest] (write (->bytes x) (str dest "/" n))))
  (scale
    ([x] (scale x 750 750))
    ([x width height] (scale x width height speed))
    ([x width height quality] (scale (.toURL x) width height quality)))
  URL
  (filesize [x] (Long/parseLong (get-in (client/head (str x)) [:headers "Content-Length"])))
  (filename [x] (filename (io/file (.getPath x))))
  (->bytes [x] (:body (client/get (str x) {:as :byte-array})))
  (mime-type [x] (-> x .openConnection .getContentType))
  (write
    ([x] (write x (System/getProperty "java.io.tmpdir")))
    ([x dest] (write x (filename x) dest))
    ([x n dest] (write (->bytes x) (str dest "/" n))))
  (scale
    ([x] (scale x 750 750))
    ([x width height] (scale x width height speed))
    ([x width height quality] (scale (:body (client/get (str x) {:as :byte-array})) width height quality)))

  String
  (filesize [x]
    (cond
      (.exists (io/file x)) (filesize (io/file x))
      (url-string? x) (filesize (URL. x))
      :else (throw (ex-info "Cannot determine filesize" {:input x}))))
  (filename [x]
    (cond
      (.exists (io/file x)) (filename (io/file x))
      (url-string? x) (filename (URL. x))
      :else (throw (ex-info "Cannot determine filename" {:input x}))))
  (->bytes [x]
    (cond
      (.exists (io/file x)) (->bytes (io/input-stream x))
      (url-string? x) (->bytes (URL. x))
      :else (throw (ex-info "Cannot determine bytes" {:input x}))))
  (->base64 [x] (->base64 (->bytes x)))
  (detect-image-format [x] (detect-image-format (io/input-stream x)))
  (mime-type [x]
    (when (.exists (io/file x)) (Files/probeContentType (.toPath (io/file x)))))
  (write
    ([x] (write x (System/getProperty "java.io.tmpdir")))
    ([x dest] (write x (filename x) dest))
    ([x n dest]
     (cond
       (.exists (io/file x)) (write (->bytes (io/file x)) (str dest "/" n))
       (url-string? x) (write (URL. x) n dest)
       :else (throw (ex-info "Cannot write" {:input x})))))
  (scale
    ([x] (scale x 750 750))
    ([x width height] (scale x width height speed))
    ([x width height quality]
     (cond
       (.exists (io/file x)) (scale (io/file x) width height quality)
       (url-string? x) (scale (URL. x) width height quality)
       :else (throw (ex-info "Cannot scale" {:input x})))))

  byte/1
  (->base64 [x] (.encodeToString (Base64/getEncoder) x))
  (detect-image-format [x] (detect-image-format (io/input-stream x)))
  (write
    ([x] (throw (ex-info "Cannot write byte array with 1-arity: no filename available. Use (write x dest) or (write x n dest)" {})))
    ([x dest]
     (with-open [w (io/output-stream dest)]
       (.write w x)))
    ([x n dest]
     (with-open [w (io/output-stream (str dest "/" n))]
       (.write w x))))
  (scale
    ([x] (scale x 750 750))
    ([x width height] (scale x width height speed))
    ([x width height quality] (scale (io/input-stream x) width height quality)))

  BufferedInputStream
  (->bytes [x]
    (with-open [in x
                out (java.io.ByteArrayOutputStream.)]
      (io/copy in out)
      (.toByteArray out)))
  (detect-image-format [x]
  (with-open [in (ImageIO/createImageInputStream x)]
    (doall
      (for [reader (iterator-seq (ImageIO/getImageReaders in))]
        (.getFormatName reader)))))
  (clone [is n]
    (let [baos (ByteArrayOutputStream.)]
      (.transferTo is baos)
      (take n (repeatedly #(io/input-stream (.toByteArray baos))))))
  (write
    ([x] (throw (ex-info "Cannot write BufferedInputStream with 1-arity: no filename available. Use (write x dest) or (write x n dest)" {})))
    ([x dest] (write (->bytes x) dest))
    ([x n dest] (write (->bytes x) n dest)))
  (scale
    ([x] (scale x 750 750))
    ([x width height] (scale x width height speed))
    ([x width height quality]
     (let [[a b c] (clone x 3)
           image-format      (first (detect-image-format a))
           transformation-f  (case (orientation c)
                               "Left side, bottom (Rotate 270 CW)"      (comp (rotate-90-counter-clockwise-fn) (resize-fn width height quality))
                               "Top, left side (Horizontal / normal)"   (resize-fn width height quality)
                               (resize-fn width height quality))
           ^BufferedImage resized-image (transformation-f b)]
       (with-open [baos (java.io.ByteArrayOutputStream.)]
         (ImageIO/write resized-image image-format baos)
         (.toByteArray baos))))))


