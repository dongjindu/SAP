FUNCTION Z_FPP_GET_KSBOHMM.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(C_FLAG) LIKE  ZSPP_KSBOHMM-ZZRET
*"  TABLES
*"      T_ZSPP_KSBOHMM STRUCTURE  ZSPP_KSBOHMM
*"----------------------------------------------------------------------
*  Date        Developer    Request      Description
* 03/07/2007   Manju        UD1K930986   E-BOM Legacy work order
*                                        changes
*"----------------------------------------------------------------------
  DATA: L_DATE LIKE SY-DATUM.
  DATA : IT_ZSPP_KSBOHMM LIKE TABLE OF ZTPP_KSBOHMM WITH HEADER LINE,
       L_DEALER(2) TYPE C,
       L_DEAL(1) TYPE C.

  L_DATE = SY-DATUM.

** Added by Furong on 10/15/10
  DATA: LT_LOG LIKE TABLE OF ZTPP_KSBOHMM_LOG WITH HEADER LINE.
  LOOP AT T_ZSPP_KSBOHMM.
    MOVE-CORRESPONDING T_ZSPP_KSBOHMM TO LT_LOG.
    APPEND LT_LOG.
  ENDLOOP.
  INSERT ZTPP_KSBOHMM_LOG FROM TABLE LT_LOG
                      ACCEPTING DUPLICATE KEYS .

** end of addition

  DELETE FROM ZTPP_KSBOHMM CLIENT SPECIFIED WHERE ZSDAT < L_DATE
                                              AND MANDT = SY-MANDT.
*
*------> MOVE INBOUNDED TABLE TO ITAB

  LOOP AT T_ZSPP_KSBOHMM.
    MOVE-CORRESPONDING T_ZSPP_KSBOHMM TO IT_ZSPP_KSBOHMM.
* Begin of changes - UD1K930986
    IF  C_FLAG  EQ 'N'.
      L_DEAL =    IT_ZSPP_KSBOHMM-DEALER.
* Dealer Conversion
      CALL FUNCTION 'ZFEB_GET_OLD_DEALER_CODE'
           EXPORTING
                NEW_DEALER = L_DEAL
           IMPORTING
                OLD_DEALER = L_DEALER.

      IF NOT L_DEALER IS INITIAL.
        IT_ZSPP_KSBOHMM-DEALER =  L_DEALER.
      ELSE.
        MESSAGE  E001 WITH 'Dealer conversion error'.
      ENDIF.

* Version Conversion
      IF     IT_ZSPP_KSBOHMM-VERS IS INITIAL.
        IT_ZSPP_KSBOHMM-VERS = '000'.
      ELSE.
        UNPACK IT_ZSPP_KSBOHMM-VERS TO IT_ZSPP_KSBOHMM-VERS.
      ENDIF.
    ENDIF.
* End of changes - UD1K930986
    CONCATENATE IT_ZSPP_KSBOHMM-NATION IT_ZSPP_KSBOHMM-DEALER
                INTO IT_ZSPP_KSBOHMM-DEST.
    IT_ZSPP_KSBOHMM-ZSDAT = SY-DATUM.
    IT_ZSPP_KSBOHMM-ZSTIM = SY-UZEIT.
    APPEND IT_ZSPP_KSBOHMM.
    MODIFY ZTPP_KSBOHMM FROM IT_ZSPP_KSBOHMM.
    IF SY-SUBRC = 0.
      MOVE  'S' TO T_ZSPP_KSBOHMM-ZZRET.
    ELSE.
      MOVE  'E' TO T_ZSPP_KSBOHMM-ZZRET.
    ENDIF.

  ENDLOOP.

*  DELETE ADJACENT DUPLICATES FROM IT_ZSPP_KSBOHMM.

*------> TEMP TABLE INSERT


*  INSERT ZTPP_KSBOHMM FROM TABLE IT_ZSPP_KSBOHMM
*                      ACCEPTING DUPLICATE KEYS .
*
*------> RETURN CODE
*  IF SY-SUBRC EQ 0.
*    MOVE  'S'   TO   T_ZSPP_KSBOHMM-ZZRET.
*  ELSE.
*    MOVE  'E'   TO   T_ZSPP_KSBOHMM-ZZRET.
** changed by Furong on 11/01/05
*    DELETE FROM ZTPP_KSBOHMM CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
** end of change
*  ENDIF.

*  MODIFY T_ZSPP_KSBOHMM TRANSPORTING ZZRET
*                WHERE ZZRET EQ SPACE.
  COMMIT WORK.
ENDFUNCTION.
