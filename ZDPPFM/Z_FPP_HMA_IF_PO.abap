FUNCTION z_fpp_hma_if_po.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(NEW) TYPE  CHAR1
*"     VALUE(IDOCNUM) LIKE  EDIDC-DOCNUM OPTIONAL
*"     VALUE(TRAN) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      RETURN STRUCTURE  BAPIRETURN
*"      HEADER STRUCTURE  ZPOSEG1
*"----------------------------------------------------------------------

  DATA : lt_new LIKE TABLE OF ztpp_ksbohmm WITH HEADER LINE,
         lt_old LIKE TABLE OF ztsd_sodata WITH HEADER LINE,
         lt_return LIKE TABLE OF bapireturn,
         lv_objek LIKE ausp-objek.

  DATA : w_header LIKE header.


  IF new EQ 'X'.
*   LT_NEW Create
    IF tran EQ 'X'.
      CALL FUNCTION 'Z_FPP_HMA_NEW_WO_IF'
           EXPORTING
                idocnum = idocnum
           TABLES
                return  = lt_return
                header  = header.
    ELSE.
      CALL FUNCTION 'Z_FPP_HMA_NEW_WO'  "This logic isNot used any more
           EXPORTING
                idocnum = idocnum
           TABLES
                return  = lt_return
                header  = header.
    ENDIF.

  ELSE .
    CALL FUNCTION 'Z_FPP_HMA_EXT_WO'
         EXPORTING
              idocnum = idocnum
         TABLES
              return  = lt_return
              header  = header.
  ENDIF.


  return[] = lt_return[].


ENDFUNCTION.
