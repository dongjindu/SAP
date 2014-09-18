FUNCTION Z_FCO_LTP_AT_QUANTITY_2.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_KSPP) LIKE  KSPP STRUCTURE  KSPP
*"     VALUE(I_MEMID) TYPE  CHAR30
*"  EXCEPTIONS
*"      PROCESS_ERROR
*"      UPD_TASK
*"      T442C
*"      READ_ERROR
*"----------------------------------------------------------------------

* Copy KSPP
  CLEAR KSPP.
  MOVE-CORRESPONDING I_KSPP TO KSPP.

* Check Version.
  PERFORM VERSN_CHECK
     USING
        KSPP-KOKRS
        KSPP-GJAHR
        KSPP-VERSN.
  PERFORM PERIODS_CHECK.

* Read t442c
  CHECK GD_DETAILDYNP IS INITIAL.

  SELECT SINGLE * FROM  T442C
         WHERE  KOKRS       = KSPP-KOKRS
         AND    VERSN       = KSPP-VERSN
         AND    GJAHR       = KSPP-GJAHR.

  IF SY-SUBRC <> 0.
    MESSAGE W601 WITH KSPP-VERSN KSPP-GJAHR KSPP-KOKRS.
* Raise
    RAISE PROCESS_ERROR.
  ENDIF.

* Process Control
  IF KSPP-DETAIL_KL = 'X'.
    GD_DETAILLEVEL = 0.
  ELSEIF KSPP-DETAIL_MW = 'X'.
    GD_DETAILLEVEL = 1.
  ELSEIF KSPP-DETAIL_PO = 'X'.
    GD_DETAILLEVEL = 2.
  ENDIF.

* GD_OK
  GD_OK = CON_AUSF .

* Main Process
*  PERFORM AUSFUEHREN
*     USING                                          "Note 178028
*        CON_SPACE                                   "Note 178028
*     CHANGING
*        KSPP
*        T442C
*        GD_SUBRC.
  PERFORM AUSFUEHREN_CUST
     USING
        CON_SPACE
     CHANGING
        KSPP
        T442C
        GD_SUBRC.

* Do not Check GD_SUBRC.
* It contains 12 because PCA is not activated IN HMMA.

  IF GT_DISPLAYLIST[] IS INITIAL .
    RAISE READ_ERROR.
  ENDIF.
* Exporting data using Memory ID
  FREE MEMORY ID I_MEMID.
  EXPORT GT_DISPLAYLIST TO MEMORY ID I_MEMID.
ENDFUNCTION.
