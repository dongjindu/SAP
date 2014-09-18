************************************************************************
* Program Name      : ZPPI_ALC_DEALER
* Creation Date     : 05/25/2011
* Development Request No :
* Addl Documentation:
* Description       : Send Dealer info to ALC
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZPPI_ALC_DEALER NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS, VRM.
TABLES: ZTSD_UM.
DATA: IT_DATA LIKE TABLE OF ZTPP_ALC_DEALER WITH HEADER LINE.

*
*CONSTANTS: C_DIR_NAME LIKE EPSF-EPSDIRNAM
*                      VALUE '/usr/sap/EDI_SAP/',
**           c_filename LIKE rlgrap-filename VALUE
**                      '/usr/sap/EDI_SAP/DTS_HMC_'.
*           C_FILENAME LIKE RLGRAP-FILENAME VALUE
*                      '/usr/sap/EDI_SAP/SUM_'.

CONSTANTS: C_DEST(10) VALUE 'WMPP01'.

**---// For FTP file creation
*DATA: W_FILENAME LIKE RLGRAP-FILENAME.
*
*DATA: BEGIN OF IT_OUT OCCURS 0,
*      DTS(83),
*      END OF IT_OUT.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_BATCH  AS CHECKBOX.
SELECT-OPTIONS: S_MODEL FOR ZTSD_UM-MODEL_CODE,
                S_BODY FOR ZTSD_UM-BODY_NO.
PARAMETERS: P_EAI AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  PERFORM GET_DATA.
  IF IT_DATA[] IS INITIAL.
    MESSAGE I001 WITH 'No data'.
  ELSE.
    PERFORM SAVE_SEND_DATA.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.

  DATA: BEGIN OF LT_TEMP OCCURS 0,
        MODL LIKE ZTPPVP-MODL,
        VHNO LIKE ZTPPVP-VHNO,
        ZEDAT LIKE ZTPPVP-ZEDAT,
        ZETIM  LIKE ZTPPVP-ZETIM,
        ZTIME(14),
        END OF LT_TEMP.

  DATA: BEGIN OF LT_UM OCCURS 0,
        MODEL_CODE LIKE ZTSD_UM-MODEL_CODE,
        BODY_NO LIKE ZTSD_UM-BODY_NO,
        WO_DEALER1 LIKE ZTSD_UM-WO_DEALER1,
        END OF LT_UM.

  DATA: L_FR_TIMESTAMP(14),
        L_TO_TIMESTAMP(14),
        L_TIMESTAMP(14),
        L_INDEX LIKE SY-TABIX.

  DATA L_UZEIT LIKE SY-UZEIT.
  DATA L_UZEIT_1 LIKE SY-UZEIT.
  DATA L_UZEIT_D LIKE SY-UZEIT.
  DATA L_FR_DATE LIKE SY-DATUM.
  DATA L_TO_DATE LIKE SY-DATUM.
  DATA LV_DIFFT LIKE SY-UZEIT.
  DATA LV_DIFFT2 LIKE SY-UZEIT.
*  DATA L_K01DAN LIKE IT_DATA-K01DAN.

  L_UZEIT = SY-UZEIT.
  L_TO_DATE = SY-DATUM.
  LV_DIFFT = '040000'.
  LV_DIFFT2 = '030000'.

  L_UZEIT_1 = L_UZEIT - LV_DIFFT.

  IF L_UZEIT_1 >  L_UZEIT.
    L_FR_DATE = L_TO_DATE - 1.
    CONCATENATE L_UZEIT_1+0(2) '00' '00' INTO L_UZEIT_1.
    CONCATENATE L_UZEIT+0(2) '59' '59' INTO L_UZEIT.
    L_UZEIT_D = L_UZEIT.
    L_UZEIT = L_UZEIT - LV_DIFFT + LV_DIFFT2.
    IF L_UZEIT > L_UZEIT_D.
      L_TO_DATE = L_FR_DATE.
    ENDIF.
    CONCATENATE L_FR_DATE L_UZEIT_1 INTO L_FR_TIMESTAMP.
    CONCATENATE L_TO_DATE L_UZEIT INTO L_TO_TIMESTAMP.
  ELSE.
    L_FR_DATE = L_TO_DATE.
    CONCATENATE L_UZEIT_1+0(2) '00' '00' INTO L_UZEIT_1.
    CONCATENATE L_UZEIT+0(2) '59' '59' INTO L_UZEIT.
    L_UZEIT = L_UZEIT - LV_DIFFT + LV_DIFFT2.
    CONCATENATE L_FR_DATE L_UZEIT_1 INTO L_FR_TIMESTAMP.
    CONCATENATE L_TO_DATE L_UZEIT INTO L_TO_TIMESTAMP.
  ENDIF.

  IF P_BATCH IS INITIAL.

** Furong on 10/18/11
*    SELECT MODEL_CODE AS K01CAR
*          BODY_NO AS K01BNO
*          WO_DEALER1 AS K01DAN
*      INTO CORRESPONDING FIELDS OF TABLE IT_DATA
*      FROM ZTSD_UM
*      WHERE MODEL_CODE IN S_MODEL
*        AND BODY_NO IN S_BODY
** Daniel on 06/08/11 {
*        AND ( STATUS = '' OR STATUS = 'F' ).
** }

*    SELECT MODEL_CODE AS K01CAR
*           BODY_NO AS K01BNO
*           SHIPMETHOD AS K01DAN
** WO_DEALER1 AS K01DAN
*       INTO CORRESPONDING FIELDS OF TABLE IT_DATA
*       FROM ZTSD_UM AS A
*       INNER JOIN ZTPP_WRAP_MASTER AS B
*       ON A~WO_DEALER1 = B~DEALER
*       WHERE MODEL_CODE IN S_MODEL
*         AND BODY_NO IN S_BODY
*         AND ( STATUS = '' OR STATUS = 'F' ).
*
*    IT_DATA-K01DAN = 'NA'.
*    MODIFY IT_DATA TRANSPORTING K01DAN
*     WHERE K01DAN IS INITIAL.

  SELECT MODEL_CODE BODY_NO WO_DEALER1
      INTO TABLE LT_UM
      FROM ZTSD_UM
      WHERE MODEL_CODE IN S_MODEL
        AND BODY_NO IN S_BODY
* Daniel on 06/08/11 {
        AND ( STATUS = '' OR STATUS = 'F' ).
* }


*    SELECT MODEL_CODE AS K01CAR
*          BODY_NO AS K01BNO
*          WO_DEALER1 AS K01DAN
*      INTO CORRESPONDING FIELDS OF TABLE IT_DATA
*      FROM ZTSD_UM
*      WHERE MODEL_CODE IN S_MODEL
*        AND BODY_NO IN S_BODY
** Daniel on 06/08/11 {
*        AND ( STATUS = '' OR STATUS = 'F' ).
** }
*
    LOOP AT LT_UM.
       IT_DATA-K01CAR = LT_UM-MODEL_CODE.
       IT_DATA-K01BNO = LT_UM-BODY_NO.

      SELECT SINGLE SHIPMETHOD INTO IT_DATA-K01DAN
        FROM ZTPP_WRAP_MASTER
        WHERE DEALER = LT_UM-WO_DEALER1.
      IF SY-SUBRC = 0.
      ELSE.
        IT_DATA-K01DAN = 'NA'.
      ENDIF.
      APPEND IT_DATA.
      CLEAR IT_DATA.
    ENDLOOP.
** End on 10/18/11

  ELSE.
    IF L_FR_DATE = L_TO_DATE.
      SELECT MODL AS MODL VHNO AS VHNO ZEDAT ZETIM
       INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
       FROM ZTPPVP
      WHERE ZEDAT = L_FR_DATE
      AND ZETIM BETWEEN L_UZEIT_1 AND L_UZEIT
         AND FLG = 'IR'.
    ELSE.
      SELECT MODL VHNO ZEDAT ZETIM
        INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
        FROM ZTPPVP
       WHERE ZEDAT BETWEEN L_FR_DATE AND L_TO_DATE
         AND FLG = 'IR' .
    ENDIF.
    IF SY-SUBRC = 0.
      LOOP AT LT_TEMP.
        CONCATENATE LT_TEMP-ZEDAT LT_TEMP-ZETIM INTO L_TIMESTAMP.
        IF L_TIMESTAMP BETWEEN L_FR_TIMESTAMP AND L_TO_TIMESTAMP.
          IT_DATA-K01CAR = LT_TEMP-MODL.
          IT_DATA-K01BNO = LT_TEMP-VHNO.

          APPEND IT_DATA.
        ENDIF.
      ENDLOOP.
      LOOP AT IT_DATA.
        L_INDEX = SY-TABIX.

** Furong on 10/18/11
*        SELECT SINGLE  WO_DEALER1 INTO IT_DATA-K01DAN
*          FROM ZTSD_UM
*         WHERE MODEL_CODE = IT_DATA-K01CAR
*           AND BODY_NO = IT_DATA-K01BNO
** Daniel on 06/08/11 {
*           AND ( STATUS = '' OR STATUS = 'F' ).

        SELECT SINGLE SHIPMETHOD INTO IT_DATA-K01DAN
          FROM ZTSD_UM AS A
          INNER JOIN ZTPP_WRAP_MASTER AS B
          ON A~WO_DEALER1 = B~DEALER
         WHERE MODEL_CODE = IT_DATA-K01CAR
           AND BODY_NO = IT_DATA-K01BNO
           AND ( STATUS = '' OR STATUS = 'F' ).
** End on 10/18/11

** Furong on 10/20/11
*        IF SY-SUBRC = 0 AND NOT IT_DATA-K01DAN IS INITIAL.
*          MODIFY IT_DATA INDEX L_INDEX.
*        ELSE.
*          DELETE IT_DATA INDEX L_INDEX.
*        ENDIF.
        IF SY-SUBRC = 0.
        ELSE.
          IT_DATA-K01DAN = 'NA'.
        ENDIF.
        MODIFY IT_DATA INDEX L_INDEX.
** End on 10/20/11
      ENDLOOP.

    ENDIF.
  ENDIF.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_SEND_DATA.
  DATA: L_RESULT(1),
        L_MSGTXT(100),
        L_INDEX LIKE SY-TABIX.

*  IF  NOT P_EAI IS INITIAL.
  CALL FUNCTION 'Z_FPP_SET_ALC_DEALER'
      DESTINATION C_DEST
      IMPORTING
        FLAG          = L_RESULT
      TABLES
        T_ALC         = IT_DATA
      EXCEPTIONS
             COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
             SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

  IF L_RESULT = 'S' OR L_RESULT = 's'.
    WRITE: / 'Successfully sent'.
    LOOP AT IT_DATA.
      L_INDEX = SY-TABIX.
      IT_DATA-ZEDAT = SY-DATUM.
      IT_DATA-ZETIM = SY-UZEIT.
      IT_DATA-ZRESULT  = 'S'.
      MODIFY IT_DATA INDEX L_INDEX.
    ENDLOOP.

  ELSE.
    WRITE: / 'EAI Failed'.
    LOOP AT IT_DATA.
      L_INDEX = SY-TABIX.
      IT_DATA-ZEDAT = SY-DATUM.
      IT_DATA-ZETIM = SY-UZEIT.
      IT_DATA-ZRESULT = 'E'.
      MODIFY IT_DATA INDEX L_INDEX.
    ENDLOOP.
*    it_error[] = it_data[].
  ENDIF.

  MODIFY ZTPP_ALC_DEALER FROM TABLE IT_DATA.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
*  ENDIF.
ENDFORM.                    " write_data
