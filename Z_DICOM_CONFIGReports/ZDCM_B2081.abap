*****           Implementation of object type ZDCM_B2081           *****
INCLUDE <OBJECT>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      INVOICEDOCNUMBER LIKE RBKP-BELNR,
      FISCALYEAR LIKE RBKP-GJAHR,
  END OF KEY.
END_DATA OBJECT. " Do not change.. DATA is generated

*** BEGIN Method
BEGIN_METHOD ZCREATEWD CHANGING CONTAINER.

*** Local Variables
DATA:
  TCODE           LIKE T020-TCODE.

*** Get transaction code from workflow.
SWC_GET_ELEMENT CONTAINER 'TransactionCode' TCODE.

*** Call wrapper function to display workitem details
CALL FUNCTION 'Z_DICOM_SHOW_WORKITEM_BASIC'
  EXPORTING
    TCODE               = TCODE
    BUS_OBJECT_NAME     = 'ZDCM_B2081'.

*** Check for errors
IF SY-SUBRC <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

END_METHOD.
