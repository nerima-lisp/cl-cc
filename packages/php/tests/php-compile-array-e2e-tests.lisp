(in-package :cl-cc/test)

(in-suite cl-cc-php-e2e-suite)

(deftest php-e2e-array-map-closure-callback
  "array_map invokes a PHP closure callback (routed back into the VM via
%vm-call-closure-sync). Closure bound to a variable to avoid the inline-closure +
inline-array-literal register-clobber edge case."
  (assert-string= "10,20,30"
                  (%php-run-capture
                   "<?php $f=function($x){ return $x*10; }; $a=[1,2,3]; $r=array_map($f,$a); echo $r[0].','.$r[1].','.$r[2];")))


(deftest php-e2e-array-map-preserves-single-array-keys
  "array_map preserves keys when exactly one array is supplied."
  (assert-string= "7,8"
                  (%php-run-capture
                   "<?php $r=array_map(null,['x'=>7,'y'=>8]); echo $r['x'].','.$r['y'];")))


(deftest php-e2e-array-map-multiple-arrays
  "array_map zips multiple arrays by position and passes parallel values to the callback."
  (assert-string= "11,22,33"
                  (%php-run-capture
                   "<?php $f=function($a,$b){ return $a+$b; }; $r=array_map($f,[1,2,3],[10,20,30]); echo $r[0].','.$r[1].','.$r[2];")))


(deftest php-e2e-array-map-multiple-arrays-pads-missing-with-null
  "array_map uses the longest input array and passes null for missing parallel values."
  (assert-string= "1a,2null"
                  (%php-run-capture
                   "<?php $f=function($a,$b){ return $a.($b===null?'null':$b); }; $r=array_map($f,[1,2],['a']); echo $r[0].','.$r[1];")))


(deftest php-e2e-array-map-null-callback-zips
  "array_map(null, ...) returns positional row arrays when multiple arrays are supplied."
  (assert-string= "1a,2b"
                  (%php-run-capture
                   "<?php $r=array_map(null,[1,2],['a','b']); echo $r[0][0].$r[0][1].','.$r[1][0].$r[1][1];")))


(deftest php-e2e-array-map-null-callback-pads-missing-with-null
  "array_map(null, ...) also uses the longest input array and pads missing values with null."
  (assert-string= "[[1,\"a\"],[2,null]]"
                  (%php-run-capture
                   "<?php $r=array_map(null,[1,2],['a']); echo json_encode($r);")))


(deftest php-e2e-array-slice-preserves-string-keys
  "array_slice reindexes integer keys by default but keeps string keys."
  (assert-string= "1,2,3"
                  (%php-run-capture
                   "<?php $r=array_slice(['x'=>1,5=>2,'y'=>3],0); echo $r['x'].','.$r[0].','.$r['y'];")))


(deftest php-e2e-array-reverse-preserves-string-keys
  "array_reverse reindexes integer keys by default but keeps string keys."
  (assert-string= "3,2,1"
                  (%php-run-capture
                   "<?php $r=array_reverse(['a'=>1,4=>2,'b'=>3]); echo $r['b'].','.$r[0].','.$r['a'];")))


(deftest php-e2e-array-column-basic-and-index
  "array_column extracts present columns and uses index_key only when present on the row."
  (assert-string= "Ada,Linus"
                  (%php-run-capture
                   "<?php $rows=[['id'=>10,'name'=>'Ada'],['id'=>20,'name'=>'Linus']]; $r=array_column($rows,'name','id'); echo $r[10].','.$r[20];"))
  (assert-string= "Ada,Linus"
                  (%php-run-capture
                   "<?php $rows=[['name'=>'Ada'],['id'=>20,'name'=>'Linus']]; $r=array_column($rows,'name','id'); echo $r[0].','.$r[20];")))


(deftest php-e2e-array-column-skips-missing-column
  "array_column skips rows that do not contain the requested column_key."
  (assert-string= "2:Ada:Grace"
                  (%php-run-capture
                   "<?php $rows=[['name'=>'Ada'],['id'=>20],['name'=>'Grace']]; $r=array_column($rows,'name'); echo count($r).':'.$r[0].':'.$r[1];")))


(deftest php-e2e-array-column-null-column-returns-rows
  "array_column with null column_key returns whole rows and can index them."
  (assert-string= "Ada,Bob"
                  (%php-run-capture
                   "<?php $rows=[['id'=>'a','name'=>'Ada'],['id'=>'b','name'=>'Bob']]; $r=array_column($rows,null,'id'); echo $r['a']['name'].','.$r['b']['name'];")))


(deftest php-e2e-array-keys-filters-values
  "array_keys supports filter_value and strict comparison."
  (assert-string= "a,b"
                  (%php-run-capture
                   "<?php $r=array_keys(['a'=>1,'b'=>'1','c'=>2], 1); echo implode(',', $r);"))
  (assert-string= "a"
                  (%php-run-capture
                   "<?php $r=array_keys(['a'=>1,'b'=>'1','c'=>2], 1, true); echo implode(',', $r);"))
  (assert-string= "x"
                  (%php-run-capture
                   "<?php $r=array_keys(['x'=>null,'y'=>0], null, true); echo implode(',', $r);")))


(deftest php-e2e-array-search-strict-and-miss
  "array_search distinguishes loose and strict matches and returns PHP false when absent."
  (assert-string= "s:i:false"
                  (%php-run-capture
                   "<?php $a=['s'=>'1','i'=>1]; echo array_search(1,$a).':'.array_search(1,$a,true).':'.(array_search(2,$a)===false?'false':'bad');"))
  (assert-string= "zero"
                  (%php-run-capture
                   "<?php $a=[0=>'needle']; echo array_search('needle',$a)===0?'zero':'bad';")))


(deftest php-e2e-in-array-strict
  "in_array uses loose comparison by default and strict comparison when requested."
  (assert-string= "loose:strict"
                  (%php-run-capture
                   "<?php $a=['1']; echo (in_array(1,$a)?'loose':'x').':'.(in_array(1,$a,true)?'bad':'strict');")))


(deftest php-e2e-array-unique-compares-values-as-strings
  "array_unique uses PHP string representation comparison by default."
  (assert-string= "{\"a\":0,\"c\":false}"
                  (%php-run-capture
                   "<?php echo json_encode(array_unique(['a'=>0,'b'=>'0','c'=>false,'d'=>null,'e'=>'']));")))


(deftest php-e2e-array-count-values-counts-only-ints-and-strings
  "array_count_values only counts integer and string values."
  (assert-string= "{\"1\":2,\"x\":2}"
                  (%php-run-capture
                   "<?php echo json_encode(array_count_values([1,'1',true,null,1.5,'x','x']));")))


(deftest php-e2e-array-flip-skips-non-key-values
  "array_flip only flips integer and string values."
  (assert-string= "{\"x\":\"a\",\"1\":\"b\"}"
                  (%php-run-capture
                   "<?php echo json_encode(array_flip(['a'=>'x','b'=>1,'c'=>true,'d'=>null,'e'=>1.5]));")))


(deftest php-e2e-array-diff-compares-values-as-strings
  "array_diff compares values by PHP string representation."
  (assert-string= "[\"x\"]"
                  (%php-run-capture
                   "<?php echo json_encode(array_values(array_diff([0,'0','x'], ['0'])));")))


(deftest php-e2e-array-intersect-compares-values-as-strings
  "array_intersect compares values by PHP string representation."
  (assert-string= "[0,\"0\"]"
                  (%php-run-capture
                   "<?php echo json_encode(array_values(array_intersect([0,'0','x'], ['0'])));")))


(deftest php-e2e-array-diff-assoc-compares-values-as-strings
  "array_diff_assoc compares matching-key values by PHP string representation."
  (assert-string= "{\"b\":\"x\"}"
                  (%php-run-capture
                   "<?php echo json_encode(array_diff_assoc(['a'=>0,'b'=>'x'], ['a'=>'0']));")))


(deftest php-e2e-array-intersect-assoc-compares-values-as-strings
  "array_intersect_assoc compares matching-key values by PHP string representation."
  (assert-string= "{\"a\":0}"
                  (%php-run-capture
                   "<?php echo json_encode(array_intersect_assoc(['a'=>0,'b'=>'x'], ['a'=>'0','b'=>'y']));")))


(deftest php-e2e-array-diff-assoc-matches-explicit-null
  "array_diff_assoc treats an explicit null value as a present matching pair."
  (assert-string= "[]"
                  (%php-run-capture
                   "<?php echo json_encode(array_diff_assoc(['a'=>null], ['a'=>null]));")))


(deftest php-e2e-array-intersect-assoc-distinguishes-missing-key-from-null
  "array_intersect_assoc requires the key to exist when the first value is null."
  (assert-string= "[]"
                  (%php-run-capture
                   "<?php echo json_encode(array_intersect_assoc(['a'=>null], []));")))


(deftest php-e2e-array-filter-closure
  "array_filter invokes a PHP closure predicate per element (closure is the last
arg, so the inline-closure + array-literal register-clobber does not apply)."
  (assert-string= "3"
                  (%php-run-capture
                   "<?php $f=array_filter([0,1,0,2], function($x){ return $x; }); $s=0; foreach($f as $v){ $s=$s+$v; } echo $s;")))


(deftest php-e2e-array-filter-mode-flags
  "array_filter supports ARRAY_FILTER_USE_KEY and ARRAY_FILTER_USE_BOTH callback modes."
  (assert-string= "a,c"
                  (%php-run-capture
                   "<?php $f=array_filter(['a'=>1,'b'=>2,'c'=>3], fn($k)=>$k!='b', ARRAY_FILTER_USE_KEY); echo implode(',', array_keys($f));"))
  (assert-string= "b,c"
                  (%php-run-capture
                   "<?php $f=array_filter(['a'=>1,'b'=>2,'c'=>3], fn($v,$k)=>$v>1 && $k!='a', ARRAY_FILTER_USE_BOTH); echo implode(',', array_keys($f));"))
  (assert-string= "2"
                  (%php-run-capture
                   "<?php $f=array_filter(['x'=>0,'y'=>2], null, ARRAY_FILTER_USE_KEY); echo implode(',', $f);")))


(deftest php-e2e-array-pad-key-policy
  "array_pad preserves string keys and reindexes integer keys while padding."
  (assert-string= "x:1|0:2|1:0|2:0|"
                  (%php-run-capture
                   "<?php $r=array_pad(['x'=>1,4=>2],4,0); $o=''; foreach($r as $k=>$v){ $o=$o.$k.':'.$v.'|'; } echo $o;"))
  (assert-string= "0:0|1:0|x:1|2:2|"
                  (%php-run-capture
                   "<?php $r=array_pad(['x'=>1,4=>2],-4,0); $o=''; foreach($r as $k=>$v){ $o=$o.$k.':'.$v.'|'; } echo $o;"))
  (assert-string= "x:1|0:2|"
                  (%php-run-capture
                   "<?php $r=array_pad(['x'=>1,4=>2],2,0); $o=''; foreach($r as $k=>$v){ $o=$o.$k.':'.$v.'|'; } echo $o;")))


(deftest php-e2e-array-append
  "$a[] = v appends to an array (regression: $a[] hit a parse error on the empty
subscript). Pushing onto the referenced hash-table is the whole effect."
  (assert-string= "3"  (%php-run-capture "<?php $a=[1,2]; $a[]=3; echo count($a);"))
  (assert-string= "3"  (%php-run-capture "<?php $a=[1,2]; $a[]=3; echo $a[2];"))
  (assert-string= "xy" (%php-run-capture "<?php $a=[]; $a[]='x'; $a[]='y'; echo $a[0].$a[1];"))
  (assert-string= "01020"
                  (%php-run-capture "<?php $a=[]; for($i=0;$i<3;$i++){ $a[]=$i*10; } echo $a[0].$a[1].$a[2];"))
  (assert-string= "12" (%php-run-capture "<?php $a=['k'=>1]; $a[]=2; echo $a['k'].$a[0];")))


(deftest php-e2e-array-subscript-set-still-works
  "Keyed and indexed subscript assignment are unaffected by the [] append path."
  (assert-string= "9" (%php-run-capture "<?php $a=[1]; $a[0]=9; echo $a[0];"))
  (assert-string= "5" (%php-run-capture "<?php $a=[]; $a['x']=5; echo $a['x'];")))


(deftest php-runtime-array-unset-deletes-key
  "The PHP runtime helper for unset($array[$key]) removes the key and shrinks count."
  (let ((array (cl-cc/php:%php-array (list nil nil "a")
                                     (list nil nil "b"))))
    (assert-= 2 (cl-cc/php:%php-count array))
    (cl-cc/php:%php-array-unset array 0)
    (assert-= 1 (cl-cc/php:%php-count array))
    (assert-equal cl-cc/php:+php-null+ (cl-cc/php:%php-array-ref array 0))
    (assert-string= "b" (cl-cc/php:%php-array-ref array 1))))


(deftest php-e2e-array-spread
  "The spread operator splices an array into an array literal ([...$a, 3]),
re-indexing integer keys and preserving string keys (PHP 8.1).  Regression: the
spread element lowered to a (%php-spread ...) call with no backing function
('Undefined function: %PHP-SPREAD'); %php-array now splices spread markers."
  (assert-string= "6"       (%php-run-capture "<?php $a=[1,2]; $b=[...$a,3]; echo array_sum($b);"))
  (assert-string= "1,2,3,4" (%php-run-capture "<?php $a=[2,3]; $b=[1,...$a,4]; echo implode(',',$b);"))
  (assert-string= "2"       (%php-run-capture "<?php $a=[1]; $b=[2]; $c=[...$a,...$b]; echo count($c);"))
  (assert-string= "11"      (%php-run-capture "<?php $a=[5,6]; $b=[...$a]; echo array_sum($b);"))
  ;; string keys are preserved
  (assert-string= "12"      (%php-run-capture "<?php $a=['x'=>1]; $b=[...$a,'y'=>2]; echo $b['x'].$b['y'];"))
  ;; integer keys are re-indexed from 0
  (assert-string= "12"      (%php-run-capture "<?php $a=[5=>1, 9=>2]; $b=[...$a]; echo $b[0].$b[1];"))
  ;; regression guards: plain arrays and call spread still work
  (assert-string= "6"       (%php-run-capture "<?php echo array_sum([1,2,3]);"))
  (assert-string= "6"       (%php-run-capture "<?php function s(...$n){return array_sum($n);} $a=[1,2,3]; echo s(...$a);")))


(deftest php-e2e-array-union
  "The + operator on two arrays is array UNION — the result has all of the left
array's entries plus the right's entries whose keys are not already present
(left wins), with keys preserved (NOT reindexed like array_merge).  Regression:
%php-add coerced both arrays to 0, so [1,2]+[3,4,5] gave 0 and count() errored."
  (assert-string= "3"     (%php-run-capture "<?php $a=[1,2]+[3,4,5]; echo count($a);"))
  (assert-string= "1,2,5" (%php-run-capture "<?php $a=[1,2]+[3,4,5]; echo implode(',',$a);"))
  ;; left operand wins on key conflicts; string keys preserved
  (assert-string= "12"    (%php-run-capture "<?php $a=['x'=>1]+['x'=>9,'y'=>2]; echo $a['x'].$a['y'];"))
  ;; integer keys preserved (not reindexed)
  (assert-string= "ac"    (%php-run-capture "<?php $a=[1=>'a']+[1=>'b',2=>'c']; echo $a[1].$a[2];"))
  (assert-string= "2"     (%php-run-capture "<?php $a=[]+[1,2]; echo count($a);"))
  ;; regression guards: numeric +, coercion, and array_merge are unaffected
  (assert-string= "5"     (%php-run-capture "<?php echo 2+3;"))
  (assert-string= "8"     (%php-run-capture "<?php echo '5'+3;"))
  (assert-string= "4"     (%php-run-capture "<?php echo count(array_merge([1,2],[3,4]));")))


(deftest php-e2e-intdiv-fdiv-array-is-list
  "intdiv (7.0), fdiv (8.0) and array_is_list (8.1) builtins.  intdiv and fdiv
were unregistered; array_is_list was registered as a LAMBDA, which the builtin
dispatch (resolving a function SYMBOL) could not call ('Undefined function')."
  (assert-string= "3"  (%php-run-capture "<?php echo intdiv(7,2);"))
  (assert-string= "-3" (%php-run-capture "<?php echo intdiv(-7,2);"))
  (assert-string= "3"  (%php-run-capture "<?php echo intdiv('10','3');"))
  (assert-string= "2.5" (%php-run-capture "<?php echo fdiv(10,4);"))
  (assert-string= "3"  (%php-run-capture "<?php echo fdiv(6,2);"))
  ;; array_is_list: sequential 0..n-1 keys
  (assert-string= "y"  (%php-run-capture "<?php echo array_is_list([1,2,3])?'y':'n';"))
  (assert-string= "n"  (%php-run-capture "<?php echo array_is_list([1=>'a',0=>'b'])?'y':'n';"))
  (assert-string= "n"  (%php-run-capture "<?php echo array_is_list(['x'=>1])?'y':'n';"))
  (assert-string= "y"  (%php-run-capture "<?php echo array_is_list([])?'y':'n';")))


(deftest php-e2e-usort-in-place
  "usort/uasort/uksort take the array BY REFERENCE and sort IN PLACE.  The old
implementations built a fresh result array and returned it, so the caller's
variable was never updated and the sort silently did nothing.  Now they mutate
the passed hash-table (like sort/asort/ksort) and accept closure, arrow,
builtin-name and user-function-name comparators.  stable-sort matches PHP 8.0+."
  ;; usort with arrow comparators, ascending and descending
  (assert-string= "1,2,3" (%php-run-capture "<?php $a=[3,1,2]; usort($a, fn($x,$y)=>$x-$y); echo implode(',',$a);"))
  (assert-string= "3,2,1" (%php-run-capture "<?php $a=[3,1,2]; usort($a, fn($x,$y)=>$y-$x); echo implode(',',$a);"))
  ;; usort with a user-function-name string comparator
  (assert-string= "1,2,5,8" (%php-run-capture "<?php function cmp($a,$b){return $a-$b;} $c=[5,2,8,1]; usort($c,'cmp'); echo implode(',',$c);"))
  ;; usort with a builtin-name string comparator
  (assert-string= "apple,banana,cherry" (%php-run-capture "<?php $d=['banana','apple','cherry']; usort($d,'strcmp'); echo implode(',',$d);"))
  ;; uasort preserves keys, sorts by value
  (assert-string= "a,b,c" (%php-run-capture "<?php $e=['b'=>2,'a'=>1,'c'=>3]; uasort($e, fn($x,$y)=>$x-$y); echo implode(',',array_keys($e));"))
  ;; uksort sorts by key
  (assert-string= "apple,banana" (%php-run-capture "<?php $f=['banana'=>1,'apple'=>2]; uksort($f,'strcmp'); echo implode(',',array_keys($f));"))
  ;; stable: equal comparands keep insertion order
  (assert-string= "cab" (%php-run-capture "<?php $g=[['k'=>1,'v'=>'a'],['k'=>1,'v'=>'b'],['k'=>0,'v'=>'c']]; usort($g, fn($x,$y)=>$x['k']-$y['k']); echo $g[0]['v'].$g[1]['v'].$g[2]['v'];")))


(deftest php-e2e-array-multisort-syncs-arrays-and-flags
  "array_multisort lexicographically sorts multiple arrays with flags."
  (assert-string= "1,2,2|c,a,b"
                  (%php-run-capture
                   "<?php $a=[2,2,1]; $b=['b','a','c']; array_multisort($a, SORT_ASC, SORT_NUMERIC, $b, SORT_ASC, SORT_STRING); echo implode(',',$a).'|'.implode(',',$b);"))
  (assert-string= "10,2,1|b,c,a"
                  (%php-run-capture
                   "<?php $a=[1,10,2]; $b=['a','b','c']; array_multisort($a, SORT_DESC, SORT_NUMERIC, $b); echo implode(',',$a).'|'.implode(',',$b);"))
  (assert-string= "third,first|0,1"
                  (%php-run-capture
                   "<?php $a=['first'=>2,'third'=>1]; $b=[20,10]; array_multisort($a, $b); echo implode(',',array_keys($a)).'|'.implode(',',array_keys($b));")))


(deftest php-e2e-sort-builtins-honor-sort-flags
  "sort/rsort/asort/arsort/ksort/krsort accept SORT_* flags and mutate arrays in place."
  (assert-string= "1,2,10"
                  (%php-run-capture
                   "<?php $a=['10','2','1']; sort($a, SORT_NUMERIC); echo implode(',',$a);"))
  (assert-string= "1,10,2"
                  (%php-run-capture
                   "<?php $a=['10','2','1']; sort($a, SORT_STRING); echo implode(',',$a);"))
  (assert-string= "10,2,1|2,10,1"
                  (%php-run-capture
                   "<?php $a=['10','2','1']; rsort($a, SORT_NUMERIC); echo implode(',',$a); $b=['10','2','1']; rsort($b, SORT_STRING); echo '|'.implode(',',$b);"))
  (assert-string= "c,b,a|b,a,c"
                  (%php-run-capture
                   "<?php $a=['a'=>'10','b'=>'2','c'=>'1']; asort($a, SORT_NUMERIC); echo implode(',',array_keys($a)); $b=['a'=>'10','b'=>'2','c'=>'1']; arsort($b, SORT_STRING); echo '|'.implode(',',array_keys($b));"))
  (assert-string= "1,10,2|2,10,1"
                  (%php-run-capture
                   "<?php $a=[10=>'a',2=>'b',1=>'c']; ksort($a, SORT_STRING); echo implode(',',array_keys($a)); $b=[10=>'a',2=>'b',1=>'c']; krsort($b, SORT_STRING); echo '|'.implode(',',array_keys($b));")))


(deftest php-e2e-natsort
  "natsort/natcasesort sort in NATURAL order (digit runs compared numerically),
not lexically — img2 before img10.  Also adds strnatcmp/strnatcasecmp.  natsort
previously used plain string< so 'img10' sorted before 'img2'."
  (assert-string= "img1,img2,img10"
                  (%php-run-capture "<?php $a=['img10','img2','img1']; natsort($a); echo implode(',',$a);"))
  (assert-string= "file1.txt,file2.txt,file10.txt"
                  (%php-run-capture "<?php $a=['file10.txt','file1.txt','file2.txt']; natsort($a); echo implode(',',$a);"))
  ;; natsort preserves keys
  (assert-string= "2,1,3"
                  (%php-run-capture "<?php $a=[3=>'a10',1=>'a2',2=>'a1']; natsort($a); echo implode(',',array_keys($a));"))
  ;; case-insensitive natural sort
  (assert-string= "Img1,img2,IMG10"
                  (%php-run-capture "<?php $a=['IMG10','img2','Img1']; natcasesort($a); echo implode(',',$a);"))
  ;; strnatcmp / strnatcasecmp
  (assert-string= "-1" (%php-run-capture "<?php echo strnatcmp('img2','img10');"))
  (assert-string= "1"  (%php-run-capture "<?php echo strnatcmp('img10','img2');"))
  (assert-string= "0"  (%php-run-capture "<?php echo strnatcmp('abc','abc');"))
  (assert-string= "-1" (%php-run-capture "<?php echo strnatcasecmp('IMG2','img10');")))


(deftest php-e2e-array-merge-recursive
  "array_merge_recursive: integer keys are appended; colliding string keys are
combined into an array and merged recursively.  Merging two lists at a shared
string key previously over-nested (['a'=>[1]] + ['a'=>[2]] gave [[1,2]] instead
of [1,2]) because the recursive descent merged the lists' integer key 0 by key."
  (assert-string= "{\"a\":[1,2]}"
                  (%php-run-capture "<?php echo json_encode(array_merge_recursive(['a'=>[1]],['a'=>[2]]));"))
  (assert-string= "{\"a\":[1,2]}"
                  (%php-run-capture "<?php echo json_encode(array_merge_recursive(['a'=>1],['a'=>2]));"))
  (assert-string= "{\"a\":[1,2]}"
                  (%php-run-capture "<?php echo json_encode(array_merge_recursive(['a'=>[1]],['a'=>2]));"))
  (assert-string= "{\"a\":{\"x\":[1,2]}}"
                  (%php-run-capture "<?php echo json_encode(array_merge_recursive(['a'=>['x'=>1]],['a'=>['x'=>2]]));"))
  ;; integer keys append; distinct keys kept; 3-way merge
  (assert-string= "[1,2,3,4]"
                  (%php-run-capture "<?php echo json_encode(array_merge_recursive([1,2],[3,4]));"))
  (assert-string= "{\"a\":1,\"b\":2}"
                  (%php-run-capture "<?php echo json_encode(array_merge_recursive(['a'=>1],['b'=>2]));"))
  (assert-string= "{\"a\":[1,2,3]}"
                  (%php-run-capture "<?php echo json_encode(array_merge_recursive(['a'=>[1]],['a'=>[2]],['a'=>[3]]));")))
