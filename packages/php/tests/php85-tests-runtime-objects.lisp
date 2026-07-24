(in-package :cl-cc/test)


(%php85-register-test 'php85-get-error-handler-reports-current-handler
  "get_error_handler() returns the active handler and null when none is installed."
  (lambda ()
(expect (%php-run-capture
                  "<?php function h($errno,$errstr){ return true; } echo get_error_handler() === null ? 'null' : 'bad'; set_error_handler('h'); echo ':' . get_error_handler() . ':'; restore_error_handler(); echo get_error_handler() === null ? 'null' : 'bad';") :to-equal "null:h:null")))

(%php85-register-test 'php85-get-exception-handler-reports-current-handler
  "get_exception_handler() returns the active handler and null when none is installed."
  (lambda ()
(expect (%php-run-capture
                  "<?php function eh($e){} echo get_exception_handler() === null ? 'null' : 'bad'; set_exception_handler('eh'); echo ':' . get_exception_handler() . ':'; restore_exception_handler(); echo get_exception_handler() === null ? 'null' : 'bad';") :to-equal "null:eh:null")))

(%php85-register-test 'php85-opcache-file-cache-helper-returns-false-without-file-cache
  "opcache_is_script_cached_in_file_cache() is available and false without an OPCache file cache."
  (lambda ()
(expect (cl-cc/php:%php-opcache-is-script-cached-in-file-cache "/tmp/example.php") :to-be-falsy)))

(%php85-register-test 'php85-gc-collect-cycles-returns-integer-zero
  "PHP 8.5 gc_collect_cycles() returns an integer 0 in cl-cc's no-GC PHP model."
  (lambda ()
(expect (= 0 (cl-cc/php::%php-gc-collect-cycles)) :to-be-truthy)))

(%php85-register-test 'php85-gc-collect-cycles-executes-as-builtin
  "The PHP 8.5 gc_collect_cycles() builtin executes from PHP source."
  (lambda ()
(expect (%php-run-capture
                  "<?php echo gettype(gc_collect_cycles()) . ':' . gc_collect_cycles();") :to-equal "integer:0")))

(%php85-register-test 'php85-opcache-file-cache-helper-executes-as-builtin
  "The PHP 8.5 opcache file-cache probe executes from PHP source."
  (lambda ()
(expect (%php-run-capture
                  "<?php echo opcache_is_script_cached_in_file_cache('/tmp/example.php') ? 'warm' : 'cold';") :to-equal "cold")))

(%php85-register-test 'php85-curl-share-init-persistent-reuses-handle
  "curl_share_init_persistent() returns a stable persistent handle for each ID."
  (lambda ()
(let ((a (cl-cc/php:%php-curl-share-init-persistent "cache"))
        (b (cl-cc/php:%php-curl-share-init-persistent "cache"))
        (c (cl-cc/php:%php-curl-share-init-persistent "other")))
    (expect b :to-be a)
    (expect (eq a c) :to-be-falsy)
    (expect (gethash "__class__" a) :to-equal "CurlSharePersistentHandle")
    (expect (gethash "id" a) :to-equal "cache")
    (expect (gethash "persistent" a) :to-be-truthy))))

(%php85-register-test 'php85-curl-share-init-persistent-executes-as-builtin
  "The PHP 8.5 persistent cURL share initializer executes from PHP source."
  (lambda ()
(expect (%php-run-capture
                  "<?php $a=curl_share_init_persistent('cache'); $b=curl_share_init_persistent('cache'); echo ($a === $b ? 'same' : 'diff') . ':' . gettype($a);") :to-equal "same:object")))

(%php85-register-test 'php85-curl-share-init-persistent-registers-its-class-tag
  "The persistent cURL share handle is visible to class_exists before it is created."
  (lambda ()
(expect (%php-run-capture
                  "<?php echo class_exists('CurlSharePersistentHandle') ? 'Y' : 'N';") :to-equal "Y")))

(%php85-register-test 'php85-object-bool-loose-comparison-follows-bool-cast
  "PHP 8.5 object/boolean loose comparison follows the object's bool cast."
  (lambda ()
    (expect (%php-run-capture
                     "<?php $o = curl_share_init_persistent('cmp'); $t = true; $f = false; echo ($o == true ? 'T' : 'F'); echo ($o == $t ? 'T' : 'F'); echo ($o == $f ? 'T' : 'F');") :to-equal "TTF")
    (let ((object (make-hash-table :test #'equal))
          (empty-array (cl-cc/php::%php-array)))
      (setf (gethash "__class__" object) "UncomparableInternal")
      (expect (cl-cc/php::%php-truthy object) :to-be-truthy)
      (expect (cl-cc/php::%php-eq-loose object t) :to-be-truthy)
      (expect (cl-cc/php::%php-eq-loose object nil) :to-be-falsy)
      (expect (cl-cc/php::%php-truthy empty-array) :to-be-falsy))))

(%php85-register-test 'php85-curl-multi-get-handles-returns-php-array
  "curl_multi_get_handles() exposes a multi-handle's child handles as a PHP array."
  (lambda ()
(let* ((multi (make-hash-table :test #'equal))
         (a (make-hash-table :test #'equal))
         (b (make-hash-table :test #'equal)))
    (setf (gethash "__class__" a) "CurlHandle"
          (gethash "__class__" b) "CurlHandle"
          (gethash "handles" multi) (list a b))
    (let ((result (cl-cc/php:%php-curl-multi-get-handles multi)))
      (expect (= 2 (cl-cc/php:%php-count result)) :to-be-truthy)
      (expect (cl-cc/php:%php-array-ref result 0) :to-be a)
      (expect (cl-cc/php:%php-array-ref result 1) :to-be b)))))

(%php85-register-test 'php85-curl-multi-get-handles-executes-as-builtin
  "curl_multi_get_handles() is registered as a PHP 8.5 builtin."
  (lambda ()
(expect (%php-run-capture
                  "<?php echo function_exists('curl_multi_get_handles') ? 'Y' : 'N';") :to-equal "Y")))

(%php85-register-test 'php85-locale-likely-subtags-static-methods-lower-to-helpers
  "The PHP 8.5 Locale likely-subtag static methods lower to runtime helpers."
  (lambda ()
    (expect (%php-run-capture
                     "<?php echo Locale::addLikelySubtags('en') . ':' . Locale::minimizeSubtags('en_Latn_US');") :to-equal "en-Latn-US:en")))

(%php85-register-test 'php85-new-runtime-symbols-are-pre-registered
  "PHP 8.5 runtime-visible classes, exceptions, and enums are visible to class_exists."
(lambda ()
(dolist (class-name '("NoDiscard"
                      "Closure"
                      "DelayedTargetValidation"
                      "CurlSharePersistentHandle"
                      "Dom\\Element"
                      "Dom\\HTMLCollection"
                      "Dom\\HTMLDocument"
                      "Filter\\FilterException"
                      "Filter\\FilterFailedException"
                      "IntlListFormatter"
                      "Locale"
                      "NumberFormatter"
                      "Pdo\\Sqlite"
                      "SoapClient"
                      "SoapFault"
                      "SoapServer"
                      "Uri\\UriError"
                      "Uri\\UriException"
                      "Uri\\InvalidUriException"
                      "Uri\\UriComparisonMode"
                      "Uri\\Rfc3986\\Uri"
                      "Uri\\WhatWg\\InvalidUrlException"
                      "Uri\\WhatWg\\UrlValidationErrorType"
                      "Uri\\WhatWg\\UrlValidationError"
                      "Uri\\WhatWg\\Url"
                      "XSLTProcessor"))
    (expect (cl-cc/php::%php-class-exists class-name) :to-be-truthy))))

(%php85-register-test 'php85-class-alias-registers-runtime-class-alias
  "PHP 8.5 class_alias registers aliases for runtime-visible classes."
  (lambda ()
    (let ((alias (format nil "Php85AliasRuntimeUri~A" (symbol-name (gensym)))))
      (expect (cl-cc/php::%php-class-alias "Uri\\Rfc3986\\Uri" alias) :to-be-truthy)
      (expect (cl-cc/php::%php-class-exists alias) :to-be-truthy)
      (expect (cl-cc/php::%php-method-exists alias "parse") :to-be-truthy)
      (let ((methods (cl-cc/php::%php-array-values-list
                      (cl-cc/php::%php-get-class-methods alias))))
        (expect (find "toRawString" methods :test #'string=) :to-be-truthy)))))

(%php85-register-test 'php85-class-alias-is-available-from-compiled-php
  "PHP 8.5 class_alias is registered for compiled PHP code."
  (lambda ()
    (let ((alias (format nil "Php85AliasSourceUri~A" (symbol-name (gensym)))))
      (expect (%php-run-capture
                       (format nil "<?php echo class_alias('Uri\\Rfc3986\\Uri', '~A') && class_exists('~A') && method_exists('~A', 'parse') ? 'Y' : 'N';"
                               alias alias alias)) :to-equal "Y"))))

(%php85-register-test 'php85-class-alias-rejects-array-and-callable-aliases
  "PHP 8.5 class_alias rejects array and callable aliases."
  (lambda ()
    (dolist (alias '("array" "callable" "ARRAY" "Callable"))
      (let ((condition (handler-case
                           (progn
                             (cl-cc/php::%php-class-alias "Uri\\Rfc3986\\Uri" alias)
                             nil)
                         (cl-cc/php:php-exception (e) e))))
           (expect condition :to-be-truthy)
           (expect (cl-cc/php:%php-exception-matches-p condition 'value-error) :to-be-truthy)))))

(%php85-register-test 'php85-intl-and-pdo-class-constants-lower-to-runtime-values
  "PHP 8.5 IntlListFormatter, NumberFormatter, and Pdo\\Sqlite class constants resolve."
  (lambda ()
    (expect (%php-run-capture
                     "<?php echo IntlListFormatter::TYPE_AND . ':' . IntlListFormatter::TYPE_OR . ':' . NumberFormatter::CURRENCY_ISO . ':' . NumberFormatter::CURRENCY_PLURAL . ':' . NumberFormatter::CURRENCY_ACCOUNTING . ':' . NumberFormatter::CASH_CURRENCY . ':' . NumberFormatter::DECIMAL_COMPACT_SHORT . ':' . NumberFormatter::DECIMAL_COMPACT_LONG . ':' . NumberFormatter::CURRENCY_STANDARD . ':' . Pdo\\Sqlite::ATTR_OPEN_FLAGS . ':' . Pdo\\Sqlite::OPEN_READONLY . ':' . Pdo\\Sqlite::OPEN_READWRITE . ':' . Pdo\\Sqlite::OPEN_CREATE . ':' . Pdo\\Sqlite::ATTR_READONLY_STATEMENT . ':' . Pdo\\Sqlite::ATTR_EXTENDED_RESULT_CODES . ':' . Pdo\\Sqlite::ATTR_BUSY_STATEMENT . ':' . Pdo\\Sqlite::ATTR_EXPLAIN_STATEMENT . ':' . Pdo\\Sqlite::EXPLAIN_MODE_PREPARED . ':' . Pdo\\Sqlite::EXPLAIN_MODE_EXPLAIN . ':' . Pdo\\Sqlite::EXPLAIN_MODE_EXPLAIN_QUERY_PLAN . ':' . Pdo\\Sqlite::ATTR_TRANSACTION_MODE . ':' . Pdo\\Sqlite::TRANSACTION_MODE_DEFERRED . ':' . Pdo\\Sqlite::TRANSACTION_MODE_IMMEDIATE . ':' . Pdo\\Sqlite::TRANSACTION_MODE_EXCLUSIVE . ':' . Pdo\\Sqlite::DETERMINISTIC;") :to-equal "0:1:10:11:12:13:14:15:16:1000:1:2:4:1001:1002:1003:1004:0:1:2:1005:0:1:2:2048")))

(%php85-register-test 'php85-new-method-surface-is-runtime-visible
  "PHP 8.5 built-in class method additions are visible to method_exists."
  (lambda ()
    (dolist (case '(("Dom\\Element" "getElementsByClassName")
                    ("Dom\\Element" "insertAdjacentHTML")
                    ("Dom\\HTMLDocument" "getElementsByName")
                    ("Closure" "getCurrent")
                    ("Locale" "addLikelySubtags")
                    ("Locale" "isRightToLeft")
                    ("Locale" "minimizeSubtags")
                    ("Pdo\\Sqlite" "setAuthorizer")
                    ("SoapClient" "__construct")
                    ("SoapClient" "__getTypes")
                    ("SoapFault" "__construct")
                    ("SoapServer" "__construct")
                    ("SoapServer" "fault")
                    ("XSLTProcessor" "__construct")
                    ("XSLTProcessor" "getParameter")
                    ("XSLTProcessor" "setParameter")
                    ("XSLTProcessor" "removeParameter")
                    ("SQLite3Stmt" "busy")
                    ("ReflectionConstant" "getFileName")
                    ("ReflectionConstant" "getExtension")
                    ("ReflectionConstant" "getExtensionName")
                    ("ReflectionConstant" "getAttributes")
                    ("ReflectionConstant" "isDeprecated")
                    ("ReflectionProperty" "getMangledName")
                    ("Uri\\Rfc3986\\Uri" "parse")
                    ("Uri\\Rfc3986\\Uri" "withUserInfo")
                    ("Uri\\WhatWg\\Url" "toAsciiString")
                    ("Uri\\WhatWg\\Url" "withUsername")
                    ("Uri\\WhatWg\\UrlValidationError" "__construct")))
      (destructuring-bind (class method) case
        (expect (cl-cc/php::%php-method-exists class method) :to-be-truthy)))))

(%php85-register-test 'php85-soap-and-xslt-runtime-methods-execute-from-php-source
  "PHP 8.5 SoapClient, SoapFault, SoapServer, and XSLTProcessor methods execute from PHP source."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
$client = new SoapClient(null, ['location' => 'http://example.test', 'uri' => 'urn:test']);
$fault = new SoapFault('Sender', 'Boom', 'actor', 'detail', 'name', 'header', 'ja');
$server = new SoapServer(null, ['uri' => 'urn:test']);
$proc = new XSLTProcessor();
$proc->setParameter('urn:a', 'foo', 'bar');
echo (is_array($client->__getTypes()) ? 'Y' : 'N') . ':' .
     ($fault->lang === 'ja' ? 'Y' : 'N') . ':' .
     $fault->lang . ':' .
     (is_null($server->fault('Client', 'Boom')) ? 'Y' : 'N') . ':' .
     ($proc->getParameter('urn:a', 'foo') === 'bar' ? 'Y' : 'N') . ':' .
     ($proc->getParameter('urn:b', 'foo') === false ? 'Y' : 'N') . ':' .
     ($proc->removeParameter('urn:a', 'foo') ? 'Y' : 'N');
") :to-equal "Y:Y:ja:Y:Y:Y:Y")))

(%php85-register-test 'php85-reflection-constant-runtime-methods-execute-from-php-source
  "PHP 8.5 ReflectionConstant method additions execute from PHP source."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
$r = new ReflectionConstant('PHP_VERSION');
echo ($r->isDeprecated() ? 'Y' : 'N') . ':' .
     (is_array($r->getAttributes()) ? 'Y' : 'N') . ':' .
     (is_null($r->getFileName()) ? 'Y' : 'N') . ':' .
     (is_null($r->getExtensionName()) ? 'Y' : 'N');
") :to-equal "N:Y:Y:Y")))

(%php85-register-test 'php85-reflection-property-get-mangled-name-executes-from-php-source
  "PHP 8.5 ReflectionProperty::getMangledName executes from PHP source."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
class RProp {
    public $name;
}
$r = new ReflectionProperty('RProp', 'name');
echo $r->getMangledName();
") :to-equal "name")))

(%php85-register-test 'php85-dom-element-runtime-methods-execute-from-php-source
  "PHP 8.5 Dom\\Element method additions execute from PHP source."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
$e = new Dom\\Element('div');
$e->insertAdjacentHTML('beforeend', '<span class=\"a\"></span>');
$items = $e->getElementsByClassName('a');
echo get_class($items) . ':' .
     (is_null($e->insertAdjacentHTML('afterbegin', '<b></b>')) ? 'Y' : 'N');
") :to-equal "Dom\\HTMLCollection:Y")))

(%php85-register-test 'php85-dom-element-runtime-properties-execute-from-php-source
  "PHP 8.5 Dom\\Element exposes outerHTML and children properties from PHP source."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
$e = new Dom\\Element('section');
echo $e->outerHTML . ':' . get_class($e->children) . ':' .
     (property_exists($e, 'outerHTML') ? 'Y' : 'N') . ':' .
     (property_exists($e, 'children') ? 'Y' : 'N') . ':' .
     (interface_exists('Dom\\\\ParentNode') ? 'Y' : 'N') . ':' .
     (class_exists('Dom\\\\HTMLCollection') ? 'Y' : 'N');
") :to-equal "<section></section>:Dom\\HTMLCollection:Y:Y:Y:Y")))

(%php85-register-test 'php85-dom-parent-node-children-records-owner
  "PHP 8.5 Dom\\ParentNode children property returns an owner-linked HTMLCollection shim."
  (lambda ()
    (let* ((element (cl-cc/php::%php-dom-element-new "article"))
           (children (gethash "children" element)))
      (expect (gethash "outerHTML" element) :to-equal "<article></article>")
      (expect (gethash "outerhtml" element) :to-equal "<article></article>")
      (expect (cl-cc/php::%php-get-class children) :to-equal "Dom\\HTMLCollection")
      (expect (gethash "__owner__" children) :to-be element)
      (expect (gethash "__property__" children) :to-equal "children"))))

(%php85-register-test 'php85-dom-html-document-get-elements-by-name-executes-from-php-source
  "PHP 8.5 Dom\\HTMLDocument::getElementsByName executes from PHP source."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
$doc = new Dom\\HTMLDocument('<form><input name=\"token\"></form>');
$items = $doc->getElementsByName('token');
echo get_class($doc) . ':' . get_class($items) . ':' .
     (method_exists($doc, 'getElementsByName') ? 'Y' : 'N');
") :to-equal "Dom\\HTMLDocument:Dom\\HTMLCollection:Y")))

(%php85-register-test 'php85-dom-html-document-get-elements-by-name-records-query
  "PHP 8.5 Dom\\HTMLDocument::getElementsByName records the query in the collection shim."
  (lambda ()
    (let* ((doc (cl-cc/php::%php-dom-html-document-new "<input name=\"q\">"))
           (items (cl-cc/php::%php-dom-html-document-get-elements-by-name doc "q")))
      (expect (cl-cc/php::%php-get-class items) :to-equal "Dom\\HTMLCollection")
      (expect (gethash "__name__" items) :to-equal "q")
      (expect (gethash "__owner__" items) :to-be doc))))

(%php85-register-test 'php85-dom-html-document-children-executes-from-php-source
  "PHP 8.5 Dom\\ParentNode children property is exposed on Dom\\HTMLDocument."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
$doc = new Dom\\HTMLDocument('<main></main>');
echo get_class($doc->children) . ':' .
     (property_exists($doc, 'children') ? 'Y' : 'N');
") :to-equal "Dom\\HTMLCollection:Y")))

(%php85-register-test 'php85-pdo-sqlite-set-authorizer-executes-from-php-source
  "PHP 8.5 Pdo\\Sqlite::setAuthorizer executes from PHP source."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
$pdo = new Pdo\\Sqlite('sqlite::memory:');
echo is_null($pdo->setAuthorizer(null)) ? 'Y' : 'N';
") :to-equal "Y")))

(%php85-register-test 'php85-sqlite3-stmt-busy-executes-from-php-source
  "PHP 8.5 SQLite3Stmt::busy executes from PHP source."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
$stmt = new SQLite3Stmt();
echo $stmt->busy() ? 'Y' : 'N';
") :to-equal "N")))

(%php85-register-test 'php85-uri-class-method-list-is-runtime-visible
  "PHP 8.5 URI class methods are listed for runtime class-name introspection."
  (lambda ()
    (let ((methods (cl-cc/php::%php-array-values-list
                    (cl-cc/php::%php-get-class-methods "Uri\\Rfc3986\\Uri"))))
      (expect (find "toRawString" methods :test #'string=) :to-be-truthy)
      (expect (find "withUserInfo" methods :test #'string=) :to-be-truthy))
    (let ((methods (cl-cc/php::%php-array-values-list
                    (cl-cc/php::%php-get-class-methods "Uri\\WhatWg\\Url"))))
      (expect (find "toAsciiString" methods :test #'string=) :to-be-truthy)
      (expect (find "withUsername" methods :test #'string=) :to-be-truthy))))

(%php85-register-test 'php85-uri-runtime-object-parses-and-clones-components
  "PHP 8.5 URI objects expose parsed components and with* cloning."
  (lambda ()
    (let* ((uri (cl-cc/php::%php-uri-rfc3986-new "https://user:pw@example.com/a?b=1#frag"))
           (copy (cl-cc/php::%php-uri-with-user-info uri "ada")))
      (expect (cl-cc/php::%php-uri-get-scheme uri) :to-equal "https")
      (expect (cl-cc/php::%php-uri-get-host uri) :to-equal "example.com")
      (expect (cl-cc/php::%php-uri-get-path uri) :to-equal "/a")
      (expect (cl-cc/php::%php-uri-get-query uri) :to-equal "b=1")
      (expect (cl-cc/php::%php-uri-get-fragment uri) :to-equal "frag")
      (expect (cl-cc/php::%php-uri-get-username uri) :to-equal "user")
      (expect (cl-cc/php::%php-uri-get-username copy) :to-equal "ada")
      (expect (cl-cc/php::%php-uri-get-username uri) :to-equal "user"))))

(%php85-register-test 'php85-uri-parse-invalid-inputs-return-null
  "PHP 8.5 URI parse helpers return null instead of throwing on invalid input."
  (lambda ()
    (expect (cl-cc/php::%php-null-p
      (cl-cc/php::%php-uri-rfc3986-parse "https://example.com/%zz")) :to-be-truthy)
    (expect (cl-cc/php::%php-null-p
      (cl-cc/php::%php-uri-rfc3986-parse "http://example.com:99999/")) :to-be-truthy)
    (expect (cl-cc/php::%php-null-p
      (cl-cc/php::%php-uri-whatwg-parse "/relative-only")) :to-be-truthy)
    (let ((base (cl-cc/php::%php-uri-whatwg-new "https://example.com/root")))
      (expect (cl-cc/php::%php-uri-get-path
                       (cl-cc/php::%php-uri-whatwg-parse "/child" base)) :to-equal "/child"))))

(%php85-register-test 'php85-uri-constructors-signal-invalid-uri-exceptions
  "PHP 8.5 URI constructors throw the documented URI exception classes on invalid input."
  (lambda ()
    (let ((condition (handler-case
                         (progn
                           (cl-cc/php::%php-uri-rfc3986-new "https://example.com/%zz")
                           nil)
                       (cl-cc/php:php-exception (e) e))))
      (expect condition :to-be-truthy)
      (expect (cl-cc/php:%php-exception-matches-p
        condition
        (intern "URI\\INVALIDURIEXCEPTION" :cl-cc/php)) :to-be-truthy))
    (let ((condition (handler-case
                         (progn
                           (cl-cc/php::%php-uri-whatwg-new "/relative-only")
                           nil)
                       (cl-cc/php:php-exception (e) e))))
      (expect condition :to-be-truthy)
      (expect (cl-cc/php:%php-exception-matches-p
        condition
        (intern "URI\\WHATWG\\INVALIDURLEXCEPTION" :cl-cc/php)) :to-be-truthy))))

(%php85-register-test 'php85-uri-equals-honors-fragment-comparison-mode
  "PHP 8.5 URI equality excludes fragments by default and includes them with UriComparisonMode::IncludeFragment."
  (lambda ()
    (let ((a (cl-cc/php::%php-uri-rfc3986-new "https://example.com/a#one"))
          (b (cl-cc/php::%php-uri-rfc3986-new "https://example.com/a#two")))
      (expect (cl-cc/php::%php-uri-equals a b) :to-be-truthy)
      (expect (cl-cc/php::%php-uri-equals
        a
        b
        (cl-cc/php:%php-predefined-class-constant
         "Uri\\UriComparisonMode"
         "IncludeFragment")) :to-be-falsy))
    (expect (%php-run-capture
                     "<?php
$a = new Uri\\Rfc3986\\Uri('https://example.com/a#one');
$b = new Uri\\Rfc3986\\Uri('https://example.com/a#two');
echo ($a->equals($b) ? 'Y' : 'N') . ':' . ($a->equals($b, Uri\\UriComparisonMode::IncludeFragment) ? 'Y' : 'N');
") :to-equal "Y:N")))

(%php85-register-test 'php85-get-class-methods-accepts-runtime-object
  "PHP 8.5 method introspection accepts runtime objects."
  (lambda ()
    (let* ((uri (cl-cc/php::%php-uri-rfc3986-new "https://example.com/"))
           (methods (cl-cc/php::%php-array-values-list
                     (cl-cc/php::%php-get-class-methods uri))))
      (expect (find "toRawString" methods :test #'string=) :to-be-truthy)
      (expect (find "withUserInfo" methods :test #'string=) :to-be-truthy))))

(%php85-register-test 'php85-uri-object-debug-output-uses-debug-info
  "PHP 8.5 object debug output uses __debugInfo for Uri objects."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
$u = new Uri\\Rfc3986\\Uri('https://user@example.com/a?b=1#frag');
var_dump($u);
") :to-equal "object(Uri\\Rfc3986\\Uri) (8) {
  [\"scheme\"]=>
  string(5) \"https\"
  [\"username\"]=>
  string(4) \"user\"
  [\"password\"]=>
  NULL
  [\"host\"]=>
  string(11) \"example.com\"
  [\"port\"]=>
  NULL
  [\"path\"]=>
  string(2) \"/a\"
  [\"query\"]=>
  string(3) \"b=1\"
  [\"fragment\"]=>
  string(4) \"frag\"
}")
    (expect (%php-run-capture
                     "<?php
$u = new Uri\\Rfc3986\\Uri('https://user@example.com/a?b=1#frag');
echo print_r($u, true);
") :to-equal "Uri\\Rfc3986\\Uri Object (
    [scheme] => https
    [username] => user
    [password] => NULL
    [host] => example.com
    [port] => NULL
    [path] => /a
    [query] => b=1
    [fragment] => frag
)")))

(%php85-register-test 'php85-uri-constructor-and-static-parse-execute-from-php-source
  "PHP 8.5 URI constructor and static parse helpers execute from PHP source."
  (lambda ()
    (expect (%php-run-capture
                     "<?php
$u = new Uri\\Rfc3986\\Uri('https://user@example.com/a?b=1#frag');
$v = $u->withUserInfo('ada');
$w = Uri\\WhatWg\\Url::parse('https://example.com/p');
echo $u->getScheme() . ':' . $u->getHost() . ':' . $u->getPath() . ':' . $u->getQuery() . ':' . $u->getFragment() . ':' . $u->getUsername() . ':' . $v->getUsername() . ':' . $w->toAsciiString();
") :to-equal "https:example.com:/a:b=1:frag:user:ada:https://example.com/p")))

(%php85-register-test 'php85-new-constants-are-predefined
  "PHP 8.5 predefined constants are available through constant lookup."
  (lambda ()
(dolist (constant '("PHP_BUILD_DATE"
                    "PHP_BUILD_PROVIDER"
                    "CURLINFO_USED_PROXY"
                    "CURLINFO_HTTPAUTH_USED"
                    "CURLINFO_PROXYAUTH_USED"
                    "CURLINFO_CONN_ID"
                    "CURLINFO_QUEUE_TIME_T"
                    "CURLOPT_INFILESIZE_LARGE"
                    "CURLOPT_SSL_SIGNATURE_ALGORITHMS"
                    "CURLFOLLOW_ALL"
                    "CURLFOLLOW_OBEYCODE"
                    "CURLFOLLOW_FIRSTONLY"
                    "FILTER_THROW_ON_FAILURE"
                    "DECIMAL_COMPACT_SHORT"
                    "DECIMAL_COMPACT_LONG"
                    "OPENSSL_PKCS1_PSS_PADDING"
                    "PKCS7_NOSMIMECAP"
                    "PKCS7_CRLFEOL"
                    "PKCS7_NOCRL"
                    "PKCS7_NO_DUAL_CONTENT"
                    "POSIX_SC_OPEN_MAX"
                    "IPPROTO_ICMP"
                    "IPPROTO_ICMPV6"
                    "TCP_FUNCTION_BLK"
                    "TCP_FUNCTION_ALIAS"
                    "TCP_REUSPORT_LB_NUMA"
                    "TCP_REUSPORT_LB_NUMA_NODOM"
                    "TCP_REUSPORT_LB_NUMA_CURDOM"
                    "TCP_BBR_ALGORITHM"
                    "AF_PACKET"
                    "IP_BINDANY"
                    "SO_BUSY_POLL"
                    "UDP_SEGMENT"
                    "SHUT_RD"
                    "SHUT_WR"
                    "SHUT_RDWR"
                    "TOKEN_PARSE"
                    "T_LNUMBER"
                    "T_DNUMBER"
                    "T_STRING"
                    "T_VARIABLE"
                    "T_CONSTANT_ENCAPSED_STRING"
                    "T_OBJECT_OPERATOR"
                    "T_DOUBLE_ARROW"
                    "T_COMMENT"
                    "T_DOC_COMMENT"
                    "T_OPEN_TAG"
                    "T_OPEN_TAG_WITH_ECHO"
                    "T_CLOSE_TAG"
                    "T_WHITESPACE"
                    "T_DOUBLE_COLON"
                    "T_VOID_CAST"
                    "T_PIPE"
                    "T_INLINE_HTML"
                    "T_ECHO"
                    "T_CLASS"
                    "T_CONST"
                    "T_PUBLIC"
                    "T_FUNCTION"
                    "T_ABSTRACT"
                    "T_ARRAY"
                    "T_AS"
                    "T_BREAK"
                    "T_CALLABLE"
                    "T_CASE"
                    "T_CATCH"
                    "T_CLONE"
                    "T_CONTINUE"
                    "T_DECLARE"
                    "T_DEFAULT"
                    "T_DO"
                    "T_ELSE"
                    "T_ELSEIF"
                    "T_EMPTY"
                    "T_ENDDECLARE"
                    "T_ENDFOR"
                    "T_ENDFOREACH"
                    "T_ENDIF"
                    "T_ENDSWITCH"
                    "T_ENDWHILE"
                    "T_ENUM"
                    "T_EVAL"
                    "T_EXIT"
                    "T_EXTENDS"
                    "T_FINAL"
                    "T_FINALLY"
                    "T_FN"
                    "T_FOR"
                    "T_FOREACH"
                    "T_GLOBAL"
                    "T_GOTO"
                    "T_IF"
                    "T_IMPLEMENTS"
                    "T_INCLUDE"
                    "T_INCLUDE_ONCE"
                    "T_INSTANCEOF"
                    "T_INSTEADOF"
                    "T_INTERFACE"
                    "T_ISSET"
                    "T_LIST"
                    "T_MATCH"
                    "T_NAMESPACE"
                    "T_NEW"
                    "T_PRINT"
                    "T_PRIVATE"
                    "T_PROTECTED"
                    "T_READONLY"
                    "T_REQUIRE"
                    "T_REQUIRE_ONCE"
                    "T_RETURN"
                    "T_STATIC"
                    "T_SWITCH"
                    "T_THROW"
                    "T_TRAIT"
                    "T_TRY"
                    "T_UNSET"
                    "T_USE"
                    "T_VAR"
                    "T_WHILE"
                    "T_YIELD"
                    "T_LOGICAL_AND"
                    "T_LOGICAL_OR"
                    "T_LOGICAL_XOR"
                    "T_YIELD_FROM"
                    "T_ATTRIBUTE"
                    "T_NS_SEPARATOR"
                    "T_NAME_FULLY_QUALIFIED"
                    "T_NAME_QUALIFIED"
                    "T_NAME_RELATIVE"
                    "T_BAD_CHARACTER"
                    "IMAGETYPE_UNKNOWN"
                    "IMAGETYPE_WEBP"
                    "IMAGETYPE_AVIF"
                    "IMAGETYPE_HEIF"
                    "IMAGETYPE_SVG"
                    "IMAGETYPE_COUNT"))
    (multiple-value-bind (value found)
        (cl-cc/php::%php-lookup-constant constant)
      (declare (ignore value))
      (expect found :to-be-truthy)))))


(eval-when (:load-toplevel :execute)
  (%php85-run-current-source-tests))
