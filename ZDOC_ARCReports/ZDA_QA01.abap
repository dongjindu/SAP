*****           Implementation of object type ZDA_QA01             *****
INCLUDE <OBJECT>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      POST_ID LIKE ZVDA_ARCH01-POST_ID,
  END OF KEY.
END_DATA OBJECT. " Do not change.. DATA is generated



BEGIN_METHOD ZDACREATE CHANGING CONTAINER.

DATA : transactioncode TYPE t020-tcode.

transactioncode = 'ZDA001'.
swc_get_element container 'TransactionCode' transactioncode.
CALL TRANSACTION transactioncode.

END_METHOD.
