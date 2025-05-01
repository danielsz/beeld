(ns beeld.metadata
  (:require [beeld.metadata-extractor.metadata :refer [metadata tags description]])
  (:import [com.drew.metadata.exif ExifIFD0Directory ExifSubIFDDirectory GpsDirectory]
           [com.drew.metadata.exif.makernotes FujifilmMakernoteDirectory]
           [com.drew.metadata.iptc IptcDirectory]
           [com.drew.metadata.jpeg JpegDirectory]
           [com.drew.metadata.xmp XmpDirectory]
           [com.drew.metadata.file FileTypeDirectory]
           [com.drew.metadata.file FileSystemDirectory]))

(defmacro exif-tag [file-or-is directory tag]
  `(let [metadata# (metadata ~file-or-is)]
    (when-let [directory# (.getFirstDirectoryOfType metadata# ~directory)]
      (description metadata# (class directory#) ~tag))))

(defn mime-type [file-or-is]
  (exif-tag file-or-is FileTypeDirectory FileTypeDirectory/TAG_DETECTED_FILE_MIME_TYPE))

(defn filename [file-or-is]
  (exif-tag file-or-is FileSystemDirectory FileSystemDirectory/TAG_FILE_NAME))

(defn simulation [file-or-is]
  (exif-tag file-or-is FujifilmMakernoteDirectory FujifilmMakernoteDirectory/TAG_FILM_MODE))

(defn compression [file-or-is]
  (exif-tag file-or-is ExifIFD0Directory ExifIFD0Directory/TAG_COMPRESSION))

(defn orientation [file-or-is]
  (exif-tag file-or-is ExifIFD0Directory ExifIFD0Directory/TAG_ORIENTATION))

(defn make [file-or-is]
  (exif-tag file-or-is ExifIFD0Directory ExifIFD0Directory/TAG_MAKE))

(defn model [file-or-is]
  (exif-tag file-or-is ExifIFD0Directory ExifIFD0Directory/TAG_MODEL))

(defn lens [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_LENS))

(defn lens-make [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_LENS_MAKE))

(defn lens-model [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_LENS_MODEL))

(defn aperture [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_APERTURE))

(defn fnumber [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_FNUMBER))

(defn shutter-speed [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_SHUTTER_SPEED))

(defn exposure-time [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_EXPOSURE_TIME))

(defn focal-length [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_FOCAL_LENGTH))

(defn lens-specification [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_LENS_SPECIFICATION))

(defn iso [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_ISO_EQUIVALENT))

(defn original-date [file-or-is]
  (exif-tag file-or-is ExifSubIFDDirectory ExifSubIFDDirectory/TAG_DATETIME_ORIGINAL))

(defn description-exif [file-or-is]
  (exif-tag file-or-is ExifIFD0Directory ExifIFD0Directory/TAG_IMAGE_DESCRIPTION))

(defn compresion-type [file-or-is]
  (exif-tag file-or-is JpegDirectory JpegDirectory/TAG_COMPRESSION_TYPE))

(defn data-precision [file-or-is]
  (exif-tag file-or-is JpegDirectory JpegDirectory/TAG_DATA_PRECISION))

(defn caption [file-or-is]
  (exif-tag file-or-is IptcDirectory IptcDirectory/TAG_CAPTION))

(defn geolocation [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) GpsDirectory)]
    (.getGeoLocation directory)))

(defn gps-date [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) GpsDirectory)]
    (.getGpsDate directory)))

(defn description-xmp [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) XmpDirectory)]
    (let [path (filter #(= "dc:description[1]" (.getPath %)) (iterator-seq (.iterator (.getXMPMeta directory))))]
      (when (seq path)
        (.getValue (first path))))))

(defn number-of-tags [file]
  (let [metadata (metadata file)]
    (reduce (fn [x y] (+ x (count y))) 0 (tags metadata))))
