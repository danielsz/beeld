(ns beeld.metadata-extractor.metadata
  (:require [clojure.java.io :as io]
            [beeld.metadata-extractor.metadata :as extractor])
  (:import [com.drew.imaging ImageMetadataReader]           
           [com.drew.metadata Directory Tag]
           [com.drew.metadata.xmp XmpDirectory]
           [clojure.lang Reflector]))

(defn metadata
  "Accepts same arguments as io/input-stream"
  [x]
  (with-open [is (io/input-stream x)]
    (ImageMetadataReader/readMetadata is)))

(defn directories [metadata]
  (.getDirectories metadata))

(defn directory [metadata klass]
  (.getFirstDirectoryOfType metadata klass))

(defn descriptor [metadata descriptor-klass directory-klass]
  (Reflector/invokeConstructor descriptor-klass (to-array [(directory metadata directory-klass)])))

(defn tags [metadata]
  (for [directory (directories metadata)]
    (.getTags directory)))

(defn tags-with-extractor [metadata]
  (for [^Directory directory (directories metadata)
        :let [extractor (fn [^Tag tag] (.getObject directory (.getTagType tag)))]
        ^Tag tag (.getTags directory)]
    {:tag tag
     :extractor extractor}))

(defn find-tags-by-name [x val]
  (let [tags (tags-with-extractor (metadata x))]
    (filter (fn [{:keys [tag]}] (= val (.getTagName tag))) tags)))

(defn get-tag-by-name [x val]
  (let [tags (find-tags-by-name x val)]
    (first (map (fn [{:keys [tag extractor]}] (extractor tag)) tags))))

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

