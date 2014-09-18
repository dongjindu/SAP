FUNCTION ZIM_EDI_SAMFILE_DELETE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"----------------------------------------------------------------------
DATA : UNIXFILE(300) TYPE C.


  SELECT SINGLE * FROM  ZTDHF1
                  WHERE ZFDHENO  EQ  ZFDHENO.

  IF SY-SUBRC EQ 0.
     DELETE DATASET  ZTDHF1-FILENAME.
     DELETE ZTDHF1.
  ENDIF.

ENDFUNCTION.
