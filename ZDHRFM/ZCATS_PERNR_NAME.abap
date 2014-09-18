FUNCTION ZCATS_PERNR_NAME.
*"----------------------------------------------------------------------
*"*"Global interface:
*"       IMPORTING
*"             VALUE(BEG) LIKE  BAS_CHECK_PERNR-BEGDA
*"             VALUE(PNR) LIKE  BAS_CHECK_PERNR-PERNR
*"       EXPORTING
*"             REFERENCE(NAME) LIKE  BAS_CHECK_PERNR-NAME
*"             REFERENCE(SNAME) TYPE  SMNAM
*"----------------------------------------------------------------------
  data: i0001_wa like p0001,
        i0001 like p0001 occurs 0.

  CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.

  CALL FUNCTION 'HR_READ_INFOTYPE'
       EXPORTING
            PERNR           = PNR
            INFTY           = '0001'
            BEGDA           = BEG
            ENDDA           = BEG
       TABLES
            INFTY_TAB       = i0001
       EXCEPTIONS
            INFTY_NOT_FOUND = 1
            OTHERS          = 2            .
  IF SY-SUBRC = 0.
*   begin XNSP9CK276512
*    READ table i0001 into i0001_wa INDEX 1 transporting ename .
    READ table i0001 into i0001_wa INDEX 1 transporting ename sname.
*   end XNSP9CK276512
    name = i0001_wa-ename.
    sname = i0001_wa-sname. "..............................XNSP9CK276512
  ENDIF.
ENDFUNCTION.
