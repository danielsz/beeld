(ns beeld.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [image-resizer.scale-methods :refer [speed]]
            [beeld.core :as sut])
  (:import [java.io File BufferedInputStream ByteArrayOutputStream]
           [java.net URI URL]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def test-dir (str (System/getProperty "java.io.tmpdir") "/beeld-test/"))

(defn test-dir-fixture [f]
  (.mkdirs (File. test-dir))
  (f))

(use-fixtures :once test-dir-fixture)

(defn create-test-png []
  (let [bi   (BufferedImage. 100 100 BufferedImage/TYPE_INT_ARGB)
        baos (ByteArrayOutputStream.)]
    (ImageIO/write bi "png" baos)
    (.toByteArray baos)))

(defn create-test-jpg []
  (let [bi   (BufferedImage. 100 100 BufferedImage/TYPE_INT_RGB)
        baos (ByteArrayOutputStream.)]
    (ImageIO/write bi "jpg" baos)
    (.toByteArray baos)))

(defn write-test-file [path content]
  (with-open [fos (io/output-stream path)]
    (.write fos content)))

;;; url-string?

(deftest url-string-helper
  (testing "valid http/https URLs pass — have both protocol and host"
    (is (sut/url-string? "http://example.com/image.png"))
    (is (sut/url-string? "https://example.com/image.png")))
  (testing "invalid inputs fail"
    (is (not (sut/url-string? "not a url")))
    (is (not (sut/url-string? ""))))
  (testing "file:// URIs fail — no host component"
    (is (not (sut/url-string? "file:///path/to/file.png")))))

;;; filename

(deftest filename-protocol
  (testing "File"
    (let [f (io/file test-dir "test.png")]
      (spit f "")
      (is (= "test.png" (sut/filename f)))
      (io/delete-file f)))
  (testing "String path"
    (let [f (str test-dir "test_str.png")]
      (spit f "")
      (is (= "test_str.png" (sut/filename f)))
      (io/delete-file f)))
  (testing "String invalid throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/filename "not-a-file-or-url"))))
  (testing "URI"
    (let [u (URI. "file:/test_uri.png")]
      (is (string? (sut/filename u)))))
  (testing "URL"
    (let [u (URL. "file:/test_url.png")]
      (is (string? (sut/filename u))))))

;;; filesize

(deftest filesize-protocol
  (testing "File"
    (let [f       (io/file test-dir "test_size.png")
          content "hello world"]
      (spit f content)
      (is (= (count content) (sut/filesize f)))
      (io/delete-file f)))
  (testing "String path"
    (let [f       (str test-dir "test_size_str.png")
          content "hello world"]
      (spit f content)
      (is (= (count content) (sut/filesize f)))
      (io/delete-file f)))
  (testing "String invalid throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/filesize "not-a-file-or-url")))))

;;; ->bytes

(deftest ->bytes-protocol
  (testing "File"
    (let [f       (io/file test-dir "test_bytes.png")
          content (create-test-png)]
      (write-test-file f content)
      (is (bytes? (sut/->bytes f)))
      (io/delete-file f)))
  (testing "String path"
    (let [f       (str test-dir "test_bytes_str.png")
          content (create-test-png)]
      (write-test-file f content)
      (is (bytes? (sut/->bytes f)))
      (io/delete-file f)))
  (testing "String invalid throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/->bytes "not-a-file-or-url")))))

;;; ->base64

(deftest ->base64-protocol
  (testing "byte array"
    (is (string? (sut/->base64 (create-test-png))))))

;;; detect-image-format

(deftest detect-image-format-protocol
  (testing "byte array PNG"
    (is (= ["png"] (map str/lower-case (sut/detect-image-format (create-test-png))))))
  (testing "byte array JPG"
    (is (= ["jpeg"] (map str/lower-case (sut/detect-image-format (create-test-jpg))))))
  (testing "BufferedInputStream PNG"
    (let [bis (BufferedInputStream. (java.io.ByteArrayInputStream. (create-test-png)))]
      (is (= ["png"] (map str/lower-case (sut/detect-image-format bis))))))
  (testing "File PNG"
    (let [f (io/file test-dir "test_fmt.png")]
      (write-test-file f (create-test-png))
      (is (= ["png"] (map str/lower-case (sut/detect-image-format f))))
      (io/delete-file f))))

;;; mime-type

(deftest mime-type-protocol
  (testing "File"
    (let [f (io/file test-dir "test_mime.png")]
      (write-test-file f (create-test-png))
      (is (some? (sut/mime-type f)))
      (io/delete-file f)))
  (testing "String path"
    (let [f (str test-dir "test_mime_str.png")]
      (write-test-file f (create-test-png))
      (is (some? (sut/mime-type f)))
      (io/delete-file f))))

;;; write

(deftest write-protocol
  (testing "byte array - 1-arity throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/write (create-test-png)))))
  (testing "byte array - 2-arity to full dest path"
    (let [content (create-test-png)
          dest    (io/file test-dir "test_write.png")]
      (sut/write content (str dest))
      (is (.exists dest))
      (io/delete-file dest)))
  (testing "byte array - 3-arity with name and folder"
    (let [content (create-test-png)
          dest    (io/file test-dir "test_write_dir")]
      (.mkdirs dest)
      (sut/write content "write_test.png" (str dest))
      (is (.exists (io/file dest "write_test.png")))
      (io/delete-file dest :recursive true)))
  (testing "BufferedInputStream - 1-arity throws"
    (let [bis (BufferedInputStream. (java.io.ByteArrayInputStream. (create-test-png)))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (sut/write bis)))))
  (testing "BufferedInputStream - 2-arity to full dest path"
    (let [bis  (BufferedInputStream. (java.io.ByteArrayInputStream. (create-test-png)))
          dest (str test-dir "test_write_bis.png")]
      (sut/write bis dest)
      (is (.exists (io/file dest)))
      (io/delete-file dest)))
  (testing "BufferedInputStream - 3-arity with name and folder"
    (let [bis  (BufferedInputStream. (java.io.ByteArrayInputStream. (create-test-png)))
          dest (io/file test-dir "test_write_bis_dir")]
      (.mkdirs dest)
      (sut/write bis "bis_written.png" (str dest))
      (is (.exists (io/file dest "bis_written.png")))
      (io/delete-file dest :recursive true)))
  (testing "File - 3-arity with name and folder"
    (let [f    (io/file test-dir "test_write_file.png")
          dest (io/file test-dir "test_write_file_dest")]
      (write-test-file f (create-test-png))
      (.mkdirs dest)
      (sut/write f "written.png" (str dest))
      (is (.exists (io/file dest "written.png")))
      (io/delete-file f)
      (io/delete-file dest :recursive true)))
  (testing "String - 3-arity with name and folder"
    (let [f    (str test-dir "test_write_str.png")
          dest (io/file test-dir "test_write_str_dest")]
      (write-test-file f (create-test-png))
      (.mkdirs dest)
      (sut/write f "written.png" (str dest))
      (is (.exists (io/file dest "written.png")))
      (io/delete-file f)
      (io/delete-file dest :recursive true)))
  (testing "String invalid throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/write "not-a-file-or-url" "name.png" test-dir)))))

;;; scale

(deftest scale-protocol
  (testing "byte array - default 750x750"
    (is (bytes? (sut/scale (create-test-png)))))
  (testing "byte array - explicit dimensions"
    (is (bytes? (sut/scale (create-test-png) 50 50))))
  (testing "byte array - explicit dimensions and quality"
    (is (bytes? (sut/scale (create-test-png) 50 50 speed))))
  (testing "File"
    (let [f (io/file test-dir "test_scale_file.png")]
      (write-test-file f (create-test-png))
      (is (bytes? (sut/scale f 50 50)))
      (io/delete-file f)))
  (testing "String path"
    (let [f (str test-dir "test_scale_str.png")]
      (write-test-file f (create-test-png))
      (is (bytes? (sut/scale f 50 50)))
      (io/delete-file f)))
  (testing "String invalid throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/scale "not-a-file-or-url" 50 50))))
  (testing "BufferedInputStream"
    (let [bis (BufferedInputStream. (java.io.ByteArrayInputStream. (create-test-png)))]
      (is (bytes? (sut/scale bis 50 50))))))

;;; clone

(deftest clone-protocol
  (testing "BufferedInputStream produces n independent streams"
    (let [content (create-test-png)
          bis     (BufferedInputStream. (java.io.ByteArrayInputStream. content))
          cloned  (sut/clone bis 3)]
      (is (= 3 (count cloned)))
      (is (every? #(instance? java.io.InputStream %) cloned))
      (is (every? #(pos? (.available %)) cloned)))))
