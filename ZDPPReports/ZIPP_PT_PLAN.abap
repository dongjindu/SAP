************************************************************************
* Program Name      : ZIPP_PT_PLAN
* Author            : Furong Wang
* Creation Date     : 07/08/2010
* Specifications By : Daniel Kim
* Development Request No :
* Addl Documentation:
* Description       : PT: Send Production Stock
* Modification Logs
* Date       Developer    RequestNo    Description
* 08/17/10   Daniel       UD1K949660   Add used vehicle plant for eng
*********************************************************************

REPORT ZIPP_PT_PLAN NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TABLES: ZTPP_ENG_PIR.

DATA : W_MSGTXT(100),
       W_RESULT(1),
       W_YYMM(6).

CONSTANTS: C_DEST(10) VALUE 'WMHR01'.   "Interface Destination.

DATA: BEGIN OF IT_PT_PLAN OCCURS 0.
        INCLUDE STRUCTURE ZTPP_PT_PLAN.
DATA: END OF IT_PT_PLAN.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS: S_MATNR FOR ZTPP_ENG_PIR-MATNR.


PARAMETERS: P_DATE LIKE SY-DATUM.

SELECTION-SCREEN SKIP 1.
PARAMETERS: P_SEND AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM INIT_DATA.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM SEND_DATA.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA.

  FIELD-SYMBOLS: <FS>.

  DATA: BEGIN OF LT_MATNR OCCURS 0,
       MATNR LIKE ZTPP_ENG_PIR-MATNR,
       END OF LT_MATNR.

  DATA: LT_PIR LIKE TABLE OF ZTPP_ENG_PIR WITH HEADER LINE.

*  DATA: BEGIN OF LT_MSEG_GR OCCURS 0,
*     MATNR LIKE MSEG-MATNR,
*     MENGE LIKE MSEG-MENGE,
*     ZBUDAT LIKE MSEG-ZBUDAT,
*     END OF LT_MSEG_GR.
*
*  DATA: BEGIN OF LT_MSEG_GI OCCURS 0,
*      MATNR LIKE MSEG-MATNR,
*      MENGE LIKE MSEG-MENGE,
*      ZBUDAT LIKE MSEG-ZBUDAT,
*      BWART LIKE MSEG-BWART,
*      END OF LT_MSEG_GI.
*
*  DATA: WA_GR LIKE ZTPP_PT_PSTOCK,
*  WA_GI LIKE ZTPP_PT_PSTOCK,
*WA_STK LIKE ZTPP_PT_PSTOCK.
*

  DATA: L_DATE_C(8),
        L_TIME_C(6),
         L_DATE LIKE SY-DATUM,
         L_LAST_DAY LIKE SY-DATUM,
         L_CN(02) TYPE N,
         L_MATNR LIKE LT_PIR-MATNR,
*         L_QTY_GR LIKE MSEG-MENGE,
*         L_QTY_GI LIKE MSEG-MENGE,
*         L_QTY_STK LIKE MSEG-MENGE,
*         L_TOT_GR LIKE MSEG-MENGE,
*         L_TOT_GI LIKE MSEG-MENGE,
*         L_TOT_STK LIKE MSEG-MENGE,
**         L_OPEN_STK LIKE MSEG-MENGE,
         L_TEXT(40).
*         L_BWART LIKE MSEG-BWART,
*         L_YEAR LIKE MBEW-LFGJA,
*         L_PERIOD LIKE MBEW-LFMON.


  L_LAST_DAY = P_DATE + 31.

*CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
*  EXPORTING
*    DAY_IN                  = p_date
* IMPORTING
*   LAST_DAY_OF_MONTH       = l_last_day
* EXCEPTIONS
*   DAY_IN_NO_DATE          = 1
*   OTHERS                  = 2
  .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  SELECT *                   "MATNR PDATU WDATU PLNMG
     INTO TABLE LT_PIR
     FROM ZTPP_ENG_PIR
     WHERE MATNR IN S_MATNR
      AND PDATU BETWEEN P_DATE AND L_LAST_DAY
      AND ENTLU = '1'.


  SORT LT_PIR BY MATNR PDATU WDATU DESCENDING .
  DELETE ADJACENT DUPLICATES FROM LT_PIR COMPARING MATNR PDATU.

  LOOP AT LT_PIR.
    LT_MATNR-MATNR = LT_PIR-MATNR.
    COLLECT LT_MATNR.
  ENDLOOP.

  L_DATE_C = SY-DATUM.
  L_TIME_C = SY-UZEIT.

  LOOP AT LT_MATNR.

    IT_PT_PLAN-MIP_CD = LT_MATNR-MATNR.
    IT_PT_PLAN-MKR_CD = 'H201'.
    IT_PT_PLAN-SHOP_SCN_CD = 'E'.
    IT_PT_PLAN-PT_PRDN_PLNT_CD = 'HEA1'.

    PERFORM READ_NORMAL_CLASS USING LT_MATNR-MATNR 'EN_SPC14'
                          CHANGING  IT_PT_PLAN-MIP_LN_CD.

    PERFORM READ_NORMAL_CLASS USING LT_MATNR-MATNR 'EN_VEH_MODEL'
                         CHANGING  IT_PT_PLAN-PRDN_VEHL_CD.

* by Daniel on 08/17/10 {
    IF IT_PT_PLAN-PRDN_VEHL_CD = 'GD'.
        IT_PT_PLAN-PRDN_VEHL_CD = 'IN'.
    ENDIF.

    SELECT COUNT(*)
    FROM ZTPP_MODEL_CONV
    WHERE BMDL = IT_PT_PLAN-PRDN_VEHL_CD.

    IF SY-SUBRC <> 0.
        IT_PT_PLAN-PRDN_PLNT_CD = 'KVA1'.
    ELSE.
        IT_PT_PLAN-PRDN_PLNT_CD = 'HVA1'.
    ENDIF.
* }


    IT_PT_PLAN-CRTN_YYMMDD = P_DATE.
    CONCATENATE L_DATE_C L_TIME_C INTO IT_PT_PLAN-CREATEDATE.
    IT_PT_PLAN-CHANGEDATE = IT_PT_PLAN-CREATEDATE.

    L_DATE = P_DATE.
    L_CN = '00'.
    WHILE L_DATE <= L_LAST_DAY.
      READ TABLE LT_PIR WITH KEY MATNR = LT_MATNR-MATNR
                                PDATU = L_DATE.

      IF SY-SUBRC = 0.
        CONCATENATE 'IT_PT_PLAN-D' L_CN '_PLAN_PLN_QTY' INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS>.
        IF SY-SUBRC = 0.
          <FS> = LT_PIR-PLNMG.
*          WRITE LT_PIR-PLNMG TO <FS> NO-ZERO DECIMALS 0.
        ENDIF.
      ENDIF.
      L_DATE = L_DATE + 1.
      L_CN = L_CN + 1.
    ENDWHILE.
    APPEND IT_PT_PLAN.
    CLEAR: IT_PT_PLAN.

  ENDLOOP.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM SEND_DATA                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SEND_DATA.
  DATA: IT_SAVE LIKE TABLE OF ZTPP_PT_PLAN WITH HEADER LINE,
        L_FLAG(1).

  IF IT_PT_PLAN[] IS INITIAL.
    MESSAGE I000 WITH 'No data'.
    EXIT.
  ENDIF.
  IF P_SEND IS INITIAL.
    LOOP AT IT_PT_PLAN.
      MOVE-CORRESPONDING IT_PT_PLAN TO IT_SAVE.
      IT_SAVE-ZSDAT = SY-DATUM.
      IT_SAVE-ZSTIM = SY-UZEIT.
      APPEND IT_SAVE.
      CLEAR: IT_SAVE.
    ENDLOOP.

    DELETE FROM ZTPP_PT_PLAN WHERE CRTN_YYMMDD = P_DATE.
    INSERT ZTPP_PT_PLAN FROM TABLE IT_SAVE.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE I000 WITH 'Database table update error'.
    ENDIF.
  ELSE.
    CALL FUNCTION 'Z_FPP_PT_PLAN'
      DESTINATION C_DEST
      IMPORTING
        FLAG          = W_RESULT
      TABLES
        I_DATA        = IT_PT_PLAN
      EXCEPTIONS
             COMMUNICATION_FAILURE = 1 MESSAGE W_MSGTXT
             SYSTEM_FAILURE        = 2 MESSAGE W_MSGTXT.

    IF SY-SUBRC = 0.
      IF W_RESULT = 'S'.
        L_FLAG = 'S'.
        W_MSGTXT = 'Data successfully sent out'.
        MESSAGE I001 WITH W_MSGTXT.

      ELSE.
        L_FLAG = 'E'.
        W_MSGTXT =  'Data unsuccessfully sent out'.
        MESSAGE E001 WITH W_MSGTXT.
      ENDIF.
    ELSE.
      L_FLAG = 'E'.
      MESSAGE I001 WITH W_MSGTXT.
    ENDIF.

    LOOP AT IT_PT_PLAN.
      MOVE-CORRESPONDING IT_PT_PLAN TO IT_SAVE.
      IT_SAVE-ZRESULT = L_FLAG.
      IT_SAVE-ZMSG = W_MSGTXT.
      IT_SAVE-ZSDAT = SY-DATUM.
      IT_SAVE-ZSTIM = SY-UZEIT.
      IT_SAVE-ZEDAT = SY-DATUM.
      IT_SAVE-ZETIM = SY-UZEIT.
      APPEND IT_SAVE.
      CLEAR: IT_SAVE.
    ENDLOOP.

    DELETE FROM ZTPP_PT_PLAN WHERE CRTN_YYMMDD = P_DATE.
    INSERT ZTPP_PT_PLAN FROM TABLE IT_SAVE.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE I000 WITH 'Table saving error'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  read_normal_class
*&---------------------------------------------------------------------*
FORM READ_NORMAL_CLASS USING  P_VMNO  P_CHAR
                             CHANGING P_VALUE.
  SELECT SINGLE AU~ATWRT
    INTO P_VALUE
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO      AND
          KLART = '001'       AND   " material
          CA~ATNAM = P_CHAR  .
ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK_OBJEK  text
*      -->P_0461   text
*      <--P_L_ATFLV_TEMP  text
*----------------------------------------------------------------------*
FORM READ_NORMAL_CLASS_ATFLV USING  P_VMNO  P_CHAR
                             CHANGING P_VALUE.
  SELECT SINGLE AU~ATFLV
      INTO P_VALUE
      FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE OBJEK = P_VMNO      AND
            KLART = '002'       AND
            CA~ATNAM = P_CHAR  .
ENDFORM.                    " READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  P_DATE = SY-DATUM - 1.

*  S_MATNR-LOW = 'AU*'.
*  S_MATNR-SIGN = 'I'.
*  S_MATNR-OPTION = 'EQ'.
*  APPEND S_MATNR.
*
*  S_MATNR-LOW = 'AW*'.
*  S_MATNR-SIGN = 'I'.
*  S_MATNR-OPTION = 'EQ'.
*  APPEND S_MATNR.


ENDFORM.                    " init_data
