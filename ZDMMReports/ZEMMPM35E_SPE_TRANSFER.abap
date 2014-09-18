************************************************************************
* Program Name : ZEMMPM35E_SPE_TRANSFER
* Created by   : Min-su Park
* Created on   : 2003.11.26.
* Pattern      :
* Description  : Manage Standard Price for NEW Purchase Material
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.26.     Min-su Park    UD1K901873     Initial Coding
************************************************************************

*&---------------------------------------------------------------------*
*& Report  ZEMMPM35E_SPE_TRANSFER                                      *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZEMMPM35E_SPE_TRANSFER .
*Internal Table.
DATA : BEGIN OF IT_ZTMM_SPE OCCURS 0.
        INCLUDE STRUCTURE ZTMM_SPE.
DATA : END OF IT_ZTMM_SPE.
DATA : BEGIN OF IT_ANALY OCCURS 0.
        INCLUDE STRUCTURE ZTMM_ANALY.
DATA : END OF IT_ANALY   .

START-OF-SELECTION.
  PERFORM GET_SPE_DATA.
  PERFORM UPDATE_MASTER.
*&---------------------------------------------------------------------*
*&      Form  GET_SPE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SPE_DATA.
  SELECT * FROM ZTMM_SPE
           INTO CORRESPONDING FIELDS OF TABLE IT_ZTMM_SPE.
ENDFORM.                    " GET_SPE_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_MASTER.
  DATA: WA_HEAD       LIKE BAPIMATHEAD, "Header with control information
        WA_PLANT      LIKE BAPI_MARC  , "plant-specific material DATA
        WA_PLANTX     LIKE BAPI_MARCX ,
        WA_MBEW       LIKE BAPI_MBEW  ,
        WA_MBEWX      LIKE BAPI_MBEWX .
  DATA: IT_BAPIRET2   LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.
  DATA: W_NETPR LIKE ZTMM_SPE-NETPR.

  LOOP AT IT_ZTMM_SPE.
*Set werks when plant is space.
    IF IT_ZTMM_SPE-WERKS = SPACE.
      SELECT SINGLE WERKS
               INTO IT_ZTMM_SPE-WERKS
               FROM MARC
              WHERE MATNR = IT_ZTMM_SPE-MATNR.
    ENDIF.

*when net price is exist in the info record, that price is w_netpr,
*when net price does'nt exist and user input tmp net price, w_netpr
* is tmp price
*when net price and tmp net price does'nt exist, w_netpr is 0.01.
    IF NOT IT_ZTMM_SPE-NETPR IS INITIAL.
      W_NETPR = IT_ZTMM_SPE-NETPR.
    ELSEIF IT_ZTMM_SPE-NETPR IS INITIAL
       AND NOT IT_ZTMM_SPE-TNETPR IS INITIAL.
      W_NETPR = IT_ZTMM_SPE-TNETPR.
    ELSEIF  IT_ZTMM_SPE-NETPR IS INITIAL
       AND  IT_ZTMM_SPE-TNETPR IS INITIAL.
      W_NETPR = '0.01'.
    ENDIF.

*Fill the parameters for bapi function.
    CASE IT_ZTMM_SPE-PTYPE.
      WHEN 'B'.
        WA_HEAD-MATERIAL     = IT_ZTMM_SPE-MATNR.
        WA_MBEW-VAL_AREA     = IT_ZTMM_SPE-WERKS.
        WA_MBEW-PLNDPRICE3   = W_NETPR.
        WA_MBEW-PLNDPRDATE3  = SY-DATUM.
        WA_MBEWX-VAL_AREA    = IT_ZTMM_SPE-WERKS.
        WA_MBEWX-PLNDPRICE3  = 'X'              .
        WA_MBEWX-PLNDPRDATE3 = 'X'              .
      WHEN 'P'.
        WA_HEAD-MATERIAL     = IT_ZTMM_SPE-MATNR.
        WA_MBEW-VAL_AREA     = IT_ZTMM_SPE-WERKS.
        WA_MBEW-PLNDPRICE1   = W_NETPR.
        WA_MBEW-PLNDPRDATE1  = SY-DATUM.
        WA_MBEWX-VAL_AREA    = IT_ZTMM_SPE-WERKS.
        WA_MBEWX-PLNDPRICE1  = 'X'              .
        WA_MBEWX-PLNDPRDATE1 = 'X'              .
    ENDCASE.

*Call bapi function .
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
         EXPORTING
              HEADDATA       = WA_HEAD
              VALUATIONDATA  = WA_MBEW
              VALUATIONDATAX = WA_MBEWX
         TABLES
              RETURNMESSAGES = IT_BAPIRET2.

    READ TABLE IT_BAPIRET2 WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.  "Error Occurred !
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*     EXPORTING
*       WAIT          =
*     IMPORTING
*       RETURN        =    .
      PERFORM GET_ZTMM_ANALY USING W_NETPR.
    ENDIF.
  ENDLOOP.

  CHECK NOT IT_ANALY IS INITIAL.
  INSERT ZTMM_ANALY FROM TABLE IT_ANALY.
ENDFORM.                    " UPDATE_MASTER
*&---------------------------------------------------------------------*
*&      Form  GET_ZTMM_ANALY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ZTMM_ANALY USING W_NETPR.
  IF IT_ZTMM_SPE-PTYPE = 'P'.
    IT_ANALY-BASE_D = IT_ZTMM_SPE-BASE_D. "Base Date
    IT_ANALY-WERKS  = IT_ZTMM_SPE-WERKS . "Plant
    IT_ANALY-MATNR  = IT_ZTMM_SPE-MATNR . "Material
    IT_ANALY-EKORG  = IT_ZTMM_SPE-EKORG . "
    IT_ANALY-KZUST  = IT_ZTMM_SPE-KZUST . "Reason Code
    CASE IT_ZTMM_SPE-ETYPE.
      WHEN 'T'. "temp net price.
        IF IT_ZTMM_SPE-NETPR IS INITIAL. "Not Change temp net price
          IT_ANALY-VALID_D = IT_ZTMM_SPE-VALID_D.
          IT_ANALY-SOURCE = 'I'.
        ELSE.                            "Change temp net price
          IT_ANALY-VALID_D = IT_ZTMM_SPE-BASE_D.
          IT_ANALY-SOURCE = 'M'.
        ENDIF.
      WHEN 'Z'. "zero price.
        IT_ANALY-VALID_D = IT_ZTMM_SPE-BASE_D.
        IT_ANALY-SOURCE = 'M'.
    ENDCASE.
    IT_ANALY-KBETR  = W_NETPR           . "Price
    IT_ANALY-KONWA  = IT_ZTMM_SPE-WAERS . "Currency
    IT_ANALY-ERDAT   = SY-DATUM     . "WORK DAY(Creation Day)

    IT_ANALY-ERZET   = SY-UZEIT     .
    IT_ANALY-ERNAM   = SY-UNAME     .
    APPEND IT_ANALY.
  ENDIF.
ENDFORM.                    " GET_ZTMM_ANALY
