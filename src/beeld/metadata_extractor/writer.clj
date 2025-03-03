(ns beeld.metadata-extractor.writer
  (:require [clojure.java.io :as io])
  (:import [org.apache.commons.imaging.formats.jpeg.iptc JpegIptcRewriter PhotoshopApp13Data IptcType IptcTypes IptcRecord JpegIptcRewriter]
           [org.apache.commons.imaging.formats.jpeg.exif ExifRewriter]
           [org.apache.commons.imaging.formats.jpeg.xmp JpegXmpRewriter]
           [org.apache.commons.imaging Imaging]
           [org.apache.commons.imaging.formats.tiff.constants ExifTagConstants TiffTagConstants TiffDirectoryType]
           [org.apache.commons.imaging.formats.tiff.write TiffOutputSet]
           [org.apache.commons.imaging.formats.tiff.taginfos TagInfoAscii]
           [java.util ArrayList]))


(defn write-iptc [file new-file records blocks]
  (with-open [os (io/output-stream new-file)]
      (doto (JpegIptcRewriter.)
        (.writeIptc file os (PhotoshopApp13Data. records blocks)))))

(defn write-iptc-fields [file new-file xs]
  (let [[records blocks] (if-let [metadata (Imaging/getMetadata file)]
                           (if-let [photoshop (.getPhotoshop metadata)]
                             (let [data (.-photoshopApp13Data photoshop)]
                               [(.getRecords data) (.getRawBlocks data)])
                             [(ArrayList.) (ArrayList.)])
                           [(ArrayList.) (ArrayList.)])]
    (doseq [[k v] xs
            :let [new-record (IptcRecord. k v)]]
      (.add records new-record))
    (write-iptc file new-file records blocks)))

(defn write-iptc-caption [file new-file caption]
  (write-iptc-fields file new-file [[IptcTypes/CAPTION_ABSTRACT caption]]))

(defn write-iptc-caption-and-keywords [file new-file caption keywords] ; "#gabser, #ineleven"
  (write-iptc-fields file new-file [[IptcTypes/CAPTION_ABSTRACT caption]
                                    [IptcTypes/KEYWORDS keywords]]))

(defn write-exif-description [file new-file description]
  (let [output-set (if-let [metadata (Imaging/getMetadata file)]
                     (.getOutputSet (.getExif metadata))
                     (TiffOutputSet.))        
        exif-directory (.getOrCreateRootDirectory output-set)
        ; sub-exif-directory (.getOrCreateExifDirectory output-set)
        new-tag-description (TagInfoAscii. "ImageDescription" 0x010E -1 TiffDirectoryType/EXIF_DIRECTORY_IFD0)]
    (.removeField exif-directory 270)
    (.add exif-directory new-tag-description (into-array String [description]))
    (with-open [os (io/output-stream new-file)]
      (doto (ExifRewriter.)
        (.updateExifMetadataLossy file os output-set)))))

(defn write-xmp [file new-file]
  (let [xml (or (Imaging/getXmpXml file) "")]
    (with-open [os (io/output-stream new-file)]
      (doto (JpegXmpRewriter.)
        (.updateXmpXml file os xml)))))
