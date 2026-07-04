;;;; String PHP builtin data.

(in-package :cl-cc/php)

(defparameter +php-trim-characters+
  (list #\Space #\Tab #\Newline #\Return #\Null #\Page)
  "Default characters trimmed by PHP trim/ltrim/rtrim.")

(defparameter +php-md5-shift-amounts+
  #(7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22
    5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20
    4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23
    6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21))

(defparameter +php-md5-round-constants+
  #(#xd76aa478 #xe8c7b756 #x242070db #xc1bdceee
    #xf57c0faf #x4787c62a #xa8304613 #xfd469501
    #x698098d8 #x8b44f7af #xffff5bb1 #x895cd7be
    #x6b901122 #xfd987193 #xa679438e #x49b40821
    #xf61e2562 #xc040b340 #x265e5a51 #xe9b6c7aa
    #xd62f105d #x02441453 #xd8a1e681 #xe7d3fbc8
    #x21e1cde6 #xc33707d6 #xf4d50d87 #x455a14ed
    #xa9e3e905 #xfcefa3f8 #x676f02d9 #x8d2a4c8a
    #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c
    #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70
    #x289b7ec6 #xeaa127fa #xd4ef3085 #x04881d05
    #xd9d4d039 #xe6db99e5 #x1fa27cf8 #xc4ac5665
    #xf4292244 #x432aff97 #xab9423a7 #xfc93a039
    #x655b59c3 #x8f0ccc92 #xffeff47d #x85845dd1
    #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1
    #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391))

(defconstant +php-str-pad-right+ 1)
(defconstant +php-str-pad-left+  0)
(defconstant +php-str-pad-both+  2)
