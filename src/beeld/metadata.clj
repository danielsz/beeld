(ns beeld.metadata
  (:require [beeld.metadata-extractor.metadata :as extractor :refer [metadata description]]
            [clojure.java.io :as io]
            [clojure.walk :refer [postwalk]])
  (:import [com.drew.metadata.exif ExifIFD0Directory ExifSubIFDDirectory GpsDirectory]
           [com.drew.metadata.exif.makernotes FujifilmMakernoteDirectory]
           [com.drew.metadata.iptc IptcDirectory]
           [com.drew.metadata.jpeg JpegDirectory]
           [com.drew.metadata.xmp XmpDirectory]
           [com.drew.metadata.file FileTypeDirectory]
           [com.drew.metadata.file FileSystemDirectory]
           [com.drew.metadata Tag]))

(defmacro exif-tag
  "x can be any argument that clojure.java.io/input-stream supports, ie. InputStream, File, URI, URL, Socket, byte array, and String arguments. If the argument is a String, it tries to resolve it first as a URI, then as a local file name.  URIs with a 'file' protocol are converted to local file names."
  [x directory tag]
  `(let [metadata# (metadata (io/input-stream ~x))]
    (when-let [directory# (.getFirstDirectoryOfType metadata# ~directory)]
      (description metadata# (class directory#) ~tag))))

(defn mime-type [x]
  (exif-tag x FileTypeDirectory FileTypeDirectory/TAG_DETECTED_FILE_MIME_TYPE))

(defn filename [x]
  (exif-tag x FileSystemDirectory FileSystemDirectory/TAG_FILE_NAME))

(defn simulation [x]
  (exif-tag x FujifilmMakernoteDirectory FujifilmMakernoteDirectory/TAG_FILM_MODE))

(defn compression [x]
  (exif-tag x ExifIFD0Directory ExifIFD0Directory/TAG_COMPRESSION))

(defn orientation [x]
  (exif-tag x ExifIFD0Directory ExifIFD0Directory/TAG_ORIENTATION))

(defn make [x]
  (exif-tag x ExifIFD0Directory ExifIFD0Directory/TAG_MAKE))

(defn model [x]
  (exif-tag x ExifIFD0Directory ExifIFD0Directory/TAG_MODEL))

(defn lens [x]
  (exif-tag x ExifSubIFDDirectory ExifSubIFDDirectory/TAG_LENS))

(defn lens-make [x]
  (exif-tag x ExifSubIFDDirectory ExifSubIFDDirectory/TAG_LENS_MAKE))

(defn lens-model [x]
  (exif-tag x ExifSubIFDDirectory ExifSubIFDDirectory/TAG_LENS_MODEL))

(defn aperture [x]
  (exif-tag x ExifSubIFDDirectory ExifSubIFDDirectory/TAG_APERTURE))

(defn fnumber [x]
  (exif-tag x ExifSubIFDDirectory ExifSubIFDDirectory/TAG_FNUMBER))

(defn shutter-speed [x]
  (exif-tag x ExifSubIFDDirectory ExifSubIFDDirectory/TAG_SHUTTER_SPEED))

(defn exposure-time [x]
  (exif-tag x ExifSubIFDDirectory ExifSubIFDDirectory/TAG_EXPOSURE_TIME))

(defn focal-length [x]
  (exif-tag x ExifSubIFDDirectory ExifSubIFDDirectory/TAG_FOCAL_LENGTH))

(defn lens-specification [x]
  (exif-tag x ExifSubIFDDirectory ExifSubIFDDirectory/TAG_LENS_SPECIFICATION))

(defn iso [x]
  (exif-tag x ExifSubIFDDirectory ExifSubIFDDirectory/TAG_ISO_EQUIVALENT))

(defn original-date [x]
  (when-let [directory (.getFirstDirectoryOfType (metadata x) ExifSubIFDDirectory)]
    (.getDate directory ExifSubIFDDirectory/TAG_DATETIME_ORIGINAL)))

(defn description-exif [x]
  (exif-tag x ExifIFD0Directory ExifIFD0Directory/TAG_IMAGE_DESCRIPTION))

(defn compresion-type [x]
  (exif-tag x JpegDirectory JpegDirectory/TAG_COMPRESSION_TYPE))

(defn data-precision [x]
  (exif-tag x JpegDirectory JpegDirectory/TAG_DATA_PRECISION))

(defn caption [x]
  (exif-tag x IptcDirectory IptcDirectory/TAG_CAPTION))

(defn geolocation [x]
  (when-let [directory (.getFirstDirectoryOfType (metadata (io/input-stream x)) GpsDirectory)]
    (.getGeoLocation directory)))

(defn gps-date [x]
  (when-let [directory (.getFirstDirectoryOfType (metadata (io/input-stream x)) GpsDirectory)]
    (.getGpsDate directory)))

(defn description-xmp [x]
  (when-let [directory (.getFirstDirectoryOfType (metadata (io/input-stream x)) XmpDirectory)]
    (let [path (filter #(= "dc:description[1]" (.getPath %)) (iterator-seq (.iterator (.getXMPMeta directory))))]
      (when (seq path)
        (.getValue (first path))))))

(defn number-of-tags [x]
  (let [metadata (metadata (io/input-stream x))]
    (reduce (fn [x y] (+ x (count y))) 0 (extractor/tags metadata))))

(defn tags* [x]
  (let [metadata (metadata (io/input-stream x))]
    (mapcat seq (extractor/tags metadata))))

(defn tags [x]
  (let [coll (->> (tags* x)
                (group-by Tag/.getDirectoryName))
        f (fn [node] (if (instance? Tag node)
                      {(Tag/.getTagName node) (Tag/.getDescription node)}
                      node))]
    (postwalk f coll)))
