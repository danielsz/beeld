(ns beeld.metadata-extractor.metadata
  (:import [com.drew.imaging ImageMetadataReader]           
           [com.drew.metadata.xmp XmpDirectory]
           [clojure.lang Reflector]
           [java.io InputStream]))

(defn metadata
  "Accepts file or input-stream; upstream function is overloaded"
  [x]
  (let [metadata (ImageMetadataReader/readMetadata x)]
    (when (instance? java.io.InputStream x) (.close x))
    metadata))

(defn directories [metadata]
  (doseq [directory (.getDirectories metadata)]
    (println directory)))

(defn directory [metadata klass]
  (.getFirstDirectoryOfType metadata klass))

(defn descriptor [metadata descriptor-klass directory-klass]
  (Reflector/invokeConstructor descriptor-klass (to-array [(directory metadata directory-klass)])))

(defn tags [metadata]
  (for [directory (.getDirectories metadata)]
    (.getTags directory)))

(defn description
  "example: (description metadata FujifilmMakernoteDirectory FujifilmMakernoteDirectory/TAG_IMAGE_NUMBER) "
  [metadata klass tag]
  (let [directory (directory metadata klass)]
    (.getDescription directory tag)))

;; Exception for the above is XMPdirectory
;; https://github.com/drewnoakes/metadata-extractor/wiki/XMP

(defn description-xmp [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) XmpDirectory)]
    (doseq [x (iterator-seq (.iterator (.getXMPMeta directory)))]
           (println (.getPath x) (.getValue x)))))
