FUNCTION Z_FFI_INBOUND_RECLAIM_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(ZRET) LIKE  BAPIRET2-TYPE
*"     VALUE(ZMSG) TYPE  BAPIRET2-MESSAGE_V1
*"  TABLES
*"      IT_DATA STRUCTURE  ZSFI_RECLAIM_DATA
*"----------------------------------------------------------------------
***********************************************************************
* Date       Developer      Request       Description
* 02/02/2007 Manju          UD1K930601    Import Reclaim data from GQRS
*                                         to SAP
* 08/02/2011 Valerian       UD1K952652    Fix dup. error message logic
***********************************************************************
  TABLES *ZTFI_RECLAIM_LOG.

  DATA : WA_DATA LIKE ZTFI_RECLAIM_DAT OCCURS 0 WITH HEADER LINE.

* UD1K940877 - by IG.MOON
  DATA : WA_ERR LIKE ZTFI_RECLAIM_LOG OCCURS 0 WITH HEADER LINE.
* end of UD1K940877

  IF NOT IT_DATA[] IS INITIAL.

    LOOP AT IT_DATA.
      MOVE-CORRESPONDING IT_DATA TO WA_DATA.
      WRITE IT_DATA-TIME TO WA_DATA-ERZET.

* UD1K940877 - by IG.MOON - to solve dup. problem
      CLEAR *ZTFI_RECLAIM_LOG.

      SELECT SINGLE RONO INTO *ZTFI_RECLAIM_LOG-RONO
                               FROM ZTFI_RECLAIM_DAT
                              WHERE CORP EQ WA_DATA-CORP
                                AND DOEX EQ WA_DATA-DOEX
                                AND VNDR EQ WA_DATA-VNDR
                                AND ISSU EQ WA_DATA-ISSU
                                AND ISSU EQ WA_DATA-ISSU
                                AND RONO EQ WA_DATA-RONO.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING WA_DATA TO WA_ERR.
        CLEAR WA_DATA.

        CONCATENATE *ZTFI_RECLAIM_LOG-RONO '/Dup. Record'
        INTO WA_ERR-MESSAGE.

        APPEND WA_ERR.CLEAR WA_ERR.

      ELSE.
        APPEND WA_DATA.
      ENDIF.
* end of UD1K940877

    ENDLOOP.

    INSERT ZTFI_RECLAIM_DAT FROM TABLE WA_DATA.

*    IF SY-SUBRC EQ 0.
    IF SY-SUBRC EQ 0 AND NOT WA_DATA[] IS INITIAL.          "UD1K952652
      COMMIT WORK.
      ZRET = 'S'.
    ELSE.
      ROLLBACK WORK.
      ZRET = 'E'.
      ZMSG = 'Duplicate Record'.
    ENDIF.
  ENDIF.

  IF NOT WA_ERR[] IS INITIAL.
    MODIFY ZTFI_RECLAIM_LOG FROM TABLE WA_ERR.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      ZRET = 'E'.
      ZMSG = 'Error was occured when creating log.'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
