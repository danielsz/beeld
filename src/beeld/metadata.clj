(ns beeld.metadata
  (:require [beeld.metadata-extractor.metadata :refer [metadata tags description]])
  (:import [com.drew.metadata.exif ExifIFD0Directory ExifSubIFDDirectory GpsDirectory]
           [com.drew.metadata.exif.makernotes FujifilmMakernoteDirectory]
           [com.drew.metadata.iptc IptcDirectory]
           [com.drew.metadata.jpeg JpegDirectory]
           [com.drew.metadata.xmp XmpDirectory]
           [com.drew.metadata.file FileTypeDirectory]
           [com.drew.metadata.file FileSystemDirectory]))

(defn mime-type [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) FileTypeDirectory)]
    (description (metadata file) (class directory) FileTypeDirectory/TAG_DETECTED_FILE_MIME_TYPE)))

(defn filename [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) FileSystemDirectory)]
    (description (metadata file) (class directory) FileSystemDirectory/TAG_FILE_NAME)))

(defn simulation [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) FujifilmMakernoteDirectory)]
    (description (metadata file) (class directory) FujifilmMakernoteDirectory/TAG_FILM_MODE)))

(defn compression [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifIFD0Directory)]
    (description (metadata file) (class directory) ExifIFD0Directory/TAG_COMPRESSION)))

(defn orientation [file-or-is]
  (let [metadata (metadata file-or-is)]
    (when-let [directory (.getFirstDirectoryOfType metadata ExifIFD0Directory)]
      (description metadata (class directory) ExifIFD0Directory/TAG_ORIENTATION))))

(defn make [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifIFD0Directory)]
    (description (metadata file) (class directory) ExifIFD0Directory/TAG_MAKE)))

(defn model [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifIFD0Directory)]
    (description (metadata file) (class directory) ExifIFD0Directory/TAG_MODEL)))

(defn lens [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (description (metadata file) (class directory) ExifSubIFDDirectory/TAG_LENS)))

(defn lens-make [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (description (metadata file) (class directory) ExifSubIFDDirectory/TAG_LENS_MAKE)))

(defn lens-model [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (description (metadata file) (class directory) ExifSubIFDDirectory/TAG_LENS_MODEL)))

(defn aperture [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (description (metadata file) (class directory) ExifSubIFDDirectory/TAG_APERTURE)))

(defn fnumber [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (description (metadata file) (class directory) ExifSubIFDDirectory/TAG_FNUMBER)))

(defn shutter-speed [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (description (metadata file) (class directory) ExifSubIFDDirectory/TAG_SHUTTER_SPEED)))

(defn exposure-time [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (description (metadata file) (class directory) ExifSubIFDDirectory/TAG_EXPOSURE_TIME)))

(defn focal-length [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (description (metadata file) (class directory) ExifSubIFDDirectory/TAG_FOCAL_LENGTH)))

(defn lens-specification [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (description (metadata file) (class directory) ExifSubIFDDirectory/TAG_LENS_SPECIFICATION)))

(defn compresion-type [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) JpegDirectory)]
    (description (metadata file) (class directory) JpegDirectory/TAG_COMPRESSION_TYPE)))

(defn data-precision [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) JpegDirectory)]
    (description (metadata file) (class directory) JpegDirectory/TAG_DATA_PRECISION)))

(defn iso [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (description (metadata file) (class directory) ExifSubIFDDirectory/TAG_ISO_EQUIVALENT)))

(defn caption [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) IptcDirectory)]
    (description (metadata file) (class directory) IptcDirectory/TAG_CAPTION)))

(defn description-exif [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifIFD0Directory)]
    (description (metadata file) (class directory) ExifIFD0Directory/TAG_IMAGE_DESCRIPTION)))

(defn original-date [file]
  (when-let [directory (.getFirstDirectoryOfType (metadata file) ExifSubIFDDirectory)]
    (.getDate directory ExifSubIFDDirectory/TAG_DATETIME_ORIGINAL)))

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
