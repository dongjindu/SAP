************************************************************************
* Program Name      : ZRPP_HMA_MEID
* Author            : Furong Wang
* Creation Date     : 04/28/2011
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Send production actual and plan data to HMC
* Modification Logs
* Date       Developer    RequestNo    Description
* 10/21/11   Daniel       UD1K953208   Smart Key, 219-131='T' for INF
*********************************************************************

REPORT ZRPP_HMA_MEID NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TABLES: AUSP, ZTPPVR.
DATA : L_MSGTXT(100),
       L_RESULT(1),
       W_ERROR(1).

CONSTANTS: C_DEST(10) VALUE 'WMPP01'.   "Interface Destination.

DATA: BEGIN OF IT_DATA OCCURS 0.
        INCLUDE STRUCTURE ZTPP_HMA_MEID.
DATA: END OF IT_DATA.
DATA  W_DEST(30).

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK IBOX1 WITH FRAME.
PARAMETERS: P_BATCH AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK IBOX1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK IBOX2 WITH FRAME.
SELECT-OPTIONS: S_OBJECT FOR AUSP-OBJEK.
SELECTION-SCREEN END OF BLOCK IBOX2.

SELECTION-SCREEN SKIP 1.

PARAMETERS: P_SEND AS CHECKBOX.

* by Daniel on 08/18/11 {
SELECTION-SCREEN SKIP 1.

PARAMETERS:  P_RVER	LIKE SOMLRECI1-RECEIVER OBLIGATORY
             DEFAULT 'PP_HMA_MEID'.
* }

SELECTION-SCREEN END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM GET_DATA.
  IF W_ERROR IS INITIAL.
    PERFORM SEND_DATA.
  ENDIF.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA.
  DATA: L_DATE LIKE SY-DATUM,
        L_UZEIT LIKE SY-UZEIT,
        L_UZEIT_1 LIKE SY-UZEIT,
        LV_DIFFT LIKE SY-UZEIT,
        L_MATNR LIKE MARA-MATNR.

  DATA: BEGIN OF LT_ZTPPVM OCCURS 0,
        P_MODEL LIKE ZTPPVR-P_MODEL,
        P_BODY_SERIAL LIKE ZTPPVR-P_BODY_SERIAL,
        P_AIRBAG_NO11 LIKE ZTPPVM-P_AIRBAG_NO11,
        END OF LT_ZTPPVM.

  DATA: BEGIN OF LT_AUSP OCCURS 0,
        LV_OBJECT LIKE AUSP-OBJEK,
        END OF LT_AUSP.

  DATA: BEGIN OF LT_ZTBM_ABXOPVDT OCCURS 0,
        CARX LIKE ZTBM_ABXOPVDT-CARX,
        CLNO LIKE ZTBM_ABXOPVDT-CLNO,
        VALU LIKE ZTBM_ABXOPVDT-VALU,
        CLNM LIKE ZTBM_ABXOPVDT-CLNM,
        END OF LT_ZTBM_ABXOPVDT.

* by Daniel on 05/23/11 {
  DATA: BEGIN OF LT_ZTPP_MODEL_CONV OCCURS 0,
        BMDL LIKE ZTPP_MODEL_CONV-BMDL,
        MODEL LIKE ZTPP_MODEL_CONV-MODEL,
        MOBIS LIKE ZTPP_MODEL_CONV-MOBIS,
        END OF LT_ZTPP_MODEL_CONV.
* }

  DATA: L_VALS LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  DATA: ZDATS LIKE SY-DATUM.

  CLEAR: W_ERROR.

  REFRESH LT_AUSP.

  IF P_BATCH EQ 'X'.

    L_UZEIT = SY-UZEIT.
    L_DATE = SY-DATUM.
    LV_DIFFT = '010000'.

    L_UZEIT_1 = L_UZEIT - LV_DIFFT.

    IF L_UZEIT+0(2) EQ '00'.
      L_DATE = L_DATE - 1.
    ENDIF.

    CONCATENATE L_UZEIT_1+0(2) '00' '00' INTO L_UZEIT_1.
    CONCATENATE L_UZEIT+0(2) '59' '59' INTO L_UZEIT.

    L_UZEIT = L_UZEIT - LV_DIFFT.

*  L_DATE = '20101201'.
*  L_UZEIT_1 = '000000'.
*  L_UZEIT = '240000'.

    SELECT P_MODEL P_BODY_SERIAL P_AIRBAG_NO11
      INTO TABLE LT_ZTPPVM
      FROM ZTPPVM
      WHERE K04PLN = 'M'
        AND P_AIRBAG_NO11 <> ''
        AND ZEDAT = L_DATE
        AND ZETIM BETWEEN L_UZEIT_1 AND L_UZEIT
        AND ZRESULT = 'S'.

*  L_ATFLV_TO = L_NUM = L_DATE.

    IF LT_ZTPPVM[] IS INITIAL.
      MESSAGE I000 WITH 'No Data'.
      W_ERROR = 'X'.
      EXIT.
    ENDIF.

    LOOP AT LT_ZTPPVM.
      CONCATENATE LT_ZTPPVM-P_MODEL LT_ZTPPVM-P_BODY_SERIAL
             INTO LT_AUSP-LV_OBJECT.
      COLLECT LT_AUSP.
    ENDLOOP.

  ELSE.

    LOOP AT S_OBJECT.
      APPEND S_OBJECT-LOW TO LT_AUSP.
    ENDLOOP.

    IF LT_AUSP[] IS INITIAL.
      MESSAGE I000 WITH 'No Data'.
      W_ERROR = 'X'.
      EXIT.
    ENDIF.

  ENDIF.

  SELECT CARX CLNO VALU CLNM INTO TABLE LT_ZTBM_ABXOPVDT
    FROM ZTBM_ABXOPVDT.

  SORT LT_ZTBM_ABXOPVDT BY CARX CLNO VALU CLNM.

* by Daniel on 05/23/11 {
  SELECT BMDL MODEL MOBIS INTO TABLE LT_ZTPP_MODEL_CONV
    FROM ZTPP_MODEL_CONV.

  SORT LT_ZTPP_MODEL_CONV BY BMDL MODEL MOBIS.
* }

  REFRESH IT_DATA.

  LOOP AT LT_AUSP.
    L_MATNR = LT_AUSP-LV_OBJECT.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        OBJECT       = L_MATNR
        CTYPE        = '002'
      TABLES
        VAL_TABLE    = L_VALS
      EXCEPTIONS
        NO_DATA      = 1
        ERROR_MODE   = 2
        ERROR_OBJECT = 3
        ERROR_VALUE  = 4
        OTHERS       = 5.

    LOOP AT L_VALS.
      CASE L_VALS-ATNAM.
        WHEN 'P_VIN'.
          IT_DATA-VIN = L_VALS-ATWRT.
        WHEN 'P_MODEL'.
          IT_DATA-MODEL = L_VALS-ATWRT.
          IT_DATA-HMA_MODEL = L_VALS-ATWRT.
        WHEN 'P_219_1'.
          IT_DATA-MODEL_YEAR = L_VALS-ATWRT.
        WHEN 'P_219_5'.
          IT_DATA-TRIM_LEVEL = L_VALS-ATWRT.
        WHEN 'P_EXT_COLOR'.
          IT_DATA-EXT_COL_D = L_VALS-ATWRT.
        WHEN 'P_INT_COLOR'.
          IT_DATA-INT_COL_D = L_VALS-ATWRT.
        WHEN 'P_219_30'.
          IF L_VALS-ATWRT = 'N'.
            IT_DATA-HEAD_UNIT_TYPE = 'AVN'.
          ELSE.
            IT_DATA-HEAD_UNIT_TYPE = 'Radio'.
          ENDIF.
        WHEN 'P_219_4'.
          IT_DATA-BODY_TYPE = L_VALS-ATWRT.
* by Daniel on 06/13/11 {
*        WHEN 'P_RP09_SHOP_DATE'.
        WHEN 'P_RP06_SHOP_DATE'.
* }
          IT_DATA-PRODUCTION_DATE = L_VALS-ATWRT.
        WHEN 'P_219_7'.
          IF L_VALS-ATWRT = 'F'.
            IT_DATA-TRANSMISSION_TYP = '1'.
          ELSE.
            IT_DATA-TRANSMISSION_TYP = '0'.
          ENDIF.
        WHEN 'P_219_131'.  "02.07.2014 Victor
          IF lt_ausp-lv_object+0(3) = 'TCF'.
            IF l_vals-atwrt = 'F'.
              it_data-smart_key_ind = '1'.
            ELSE.
              it_data-smart_key_ind = '0'.
            ENDIF.
          ELSEIF lt_ausp-lv_object+0(3) = 'INF'
              OR lt_ausp-lv_object+0(3) = 'C2F'.
            IF l_vals-atwrt = 'T'.
              it_data-smart_key_ind = '1'.
            ELSE.
              it_data-smart_key_ind = '0'.
            ENDIF.
          ELSE.  "Others
          ENDIF.
        WHEN 'P_219_13'.
          IF L_VALS-ATWRT = 'E'.
            IT_DATA-ABS_IND = '1'.
          ELSE.
            IT_DATA-ABS_IND = '0'.
          ENDIF.
        WHEN 'P_219_184'.
          IF L_VALS-ATWRT = 'T'.
            IT_DATA-TPMS_IND = '1'.
          ELSE.
            IT_DATA-TPMS_IND = '0'.
          ENDIF.
        WHEN 'P_AIRBAG_NO11'.
          IF L_VALS-ATWRT <> ''.
            IT_DATA-MEID = L_VALS-ATWRT.
          ELSE.
            IT_DATA-MEID = ''.
          ENDIF.
** Furong on 03/19/12
        WHEN 'P_DESTINATION_CODE'.
          IT_DATA-DEST_MKT_CODE = L_VALS-ATWRT.
** End on 03/19/12

** End on 08/08/11          ENDIF.
      ENDCASE.
    ENDLOOP.

* Furong on 08/08/11
    READ TABLE LT_ZTPPVM WITH KEY
       P_MODEL = LT_AUSP-LV_OBJECT+0(3)
    P_BODY_SERIAL = LT_AUSP-LV_OBJECT+3(6).
    IF SY-SUBRC = 0.
      IT_DATA-MEID  = LT_ZTPPVM-P_AIRBAG_NO11.
    ENDIF.

* by Daniel on 06/13/11 {
    IF IT_DATA-PRODUCTION_DATE IS INITIAL.
      CONCATENATE SY-DATUM+4(2) '/'
                  SY-DATUM+6(2) '/' SY-DATUM+0(4)
             INTO IT_DATA-PRODUCTION_DATE.
    ENDIF.
* }

    READ TABLE LT_ZTBM_ABXOPVDT WITH KEY CARX = IT_DATA-MODEL
                                         CLNO = '1'
                                         VALU = IT_DATA-MODEL_YEAR
                                         BINARY SEARCH.
    IT_DATA-MODEL_YEAR = LT_ZTBM_ABXOPVDT-CLNM.

    READ TABLE LT_ZTBM_ABXOPVDT WITH KEY CARX = IT_DATA-MODEL
                                         CLNO = '5'
                                         VALU = IT_DATA-TRIM_LEVEL
                                         BINARY SEARCH.
    IT_DATA-TRIM_LEVEL = LT_ZTBM_ABXOPVDT-CLNM.

    READ TABLE LT_ZTBM_ABXOPVDT WITH KEY CARX = IT_DATA-MODEL
                                         CLNO = '4'
                                         VALU = IT_DATA-BODY_TYPE
                                         BINARY SEARCH.
    IT_DATA-BODY_TYPE = LT_ZTBM_ABXOPVDT-CLNM.

* by Daniel on 05/23/11 {
    READ TABLE LT_ZTPP_MODEL_CONV WITH KEY BMDL = IT_DATA-HMA_MODEL.
    IT_DATA-HMA_MODEL = LT_ZTPP_MODEL_CONV-MOBIS.
* }


** Furong on 03/19/12 - get internal VIN
*   read table LT_ZVIN WITH KEY MODEL_CODE = p_model
*                               BODY_NO = p_body_serial.
    SELECT SINGLE ZVIN INTO IT_DATA-INT_VIN
      FROM ZTSD_UM
      WHERE MODEL_CODE = LT_AUSP-LV_OBJECT+0(3)
        AND BODY_NO = LT_AUSP-LV_OBJECT+3(6).

** End on 03/19/12

    IT_DATA-PRODUCTION_PLANT = 'HMMA'.
    IT_DATA-VEHICLE_TYPE_IND = '0'.
    IT_DATA-MDPS_IND = '1'.
    IT_DATA-EPB_IND = '0'.
    IT_DATA-SCC_IND = '0'.
    IT_DATA-ECS_IND = '1'.
    IT_DATA-AUTO_PARK_SUPP_S = '0'.
    IT_DATA-ACU_IND = '1'.
    IT_DATA-ENGINE_TYPE = ' '.
    IT_DATA-MINN = ' '.
    IT_DATA-TMU_MANUFACTURER = ' '.
    IT_DATA-TELEMATICS_USIM_ = ' '.
    CONCATENATE SY-DATUM SY-UZEIT INTO IT_DATA-DOWNLOADED_TIME.
    IT_DATA-ZUSER = SY-UNAME.
    IT_DATA-ZSDAT = SY-DATUM.
    IT_DATA-ZSTIM = SY-UZEIT.
*    IT_DATA-ZEDAT = SY-DATUM.
*    IT_DATA-ZETIM = SY-UZEIT.

    APPEND IT_DATA.
    CLEAR: IT_DATA.
    LOOP AT L_VALS.
      CLEAR: L_VALS-ATWRT.
      MODIFY L_VALS.
    ENDLOOP.
  ENDLOOP.

  CLEAR: LT_ZTBM_ABXOPVDT.

** Furong on 08/07/12
  IF IT_DATA[] IS INITIAL.
    W_ERROR = 'X'.
    EXIT.
  ENDIF.
** End on 08/07/12

** on 06/05/12
  IF P_BATCH EQ 'X'.
    PERFORM CHECK_DUPLICATE_DATA.
  ENDIF.
** End

** Furong on 08/07/12
*  IF IT_DATA[] IS INITIAL.
*    W_ERROR = 'X'.
*  ENDIF.
** End on 08/07/12

* by Daniel on 08/18/11 {
  PERFORM CHECK_EMPTY_DATA.
  IF IT_DATA[] IS INITIAL.
    W_ERROR = 'X'.
  ENDIF.
* }

ENDFORM.                    "get_data

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
          KLART = '002'       AND
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
*&      Form  SEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_DATA.

  DATA: L_RESULT(1),
         L_TOTREC TYPE I,
         L_SREC TYPE I,
         L_FREC TYPE I,
         L_MSGTXT(60).

  W_DEST = 'WMPP01'.

  DESCRIBE TABLE IT_DATA LINES L_TOTREC.

  IF P_SEND = 'X'.
    CALL FUNCTION 'Z_FPP_SET_HMA_MEID'
      DESTINATION W_DEST
      IMPORTING
        FLAG                  = L_RESULT
      TABLES
        I_CONTENTS            = IT_DATA
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
        SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

    IF L_RESULT = 'S' OR L_RESULT = 's'.
      LOOP AT IT_DATA.
        IT_DATA-ZRESULT = 'S'.
        IT_DATA-ZEDAT = SY-DATUM.
        IT_DATA-ZETIM = SY-UZEIT.
        MODIFY IT_DATA.
      ENDLOOP.
      WRITE: / 'EAI Success, Total records are: ', L_TOTREC.
    ELSE.
      LOOP AT IT_DATA.
        IT_DATA-ZRESULT = 'E'.
        IT_DATA-ZEDAT = SY-DATUM.
        IT_DATA-ZETIM = SY-UZEIT.
        IT_DATA-ZMSG = L_MSGTXT.
        MODIFY IT_DATA.
      ENDLOOP.
      WRITE: / 'EAI Fail, Total records are: ', L_TOTREC.
* by Daniel on 08/18/11 {
      PERFORM SEND_MAIL.
* }
    ENDIF.
  ELSE.
    LOOP AT IT_DATA.
      IT_DATA-ZMSG = 'Did not send the data to HMA...'.
      MODIFY IT_DATA.
    ENDLOOP.
    WRITE: / 'EAI is skipped, Total records saved are: ', L_TOTREC.
  ENDIF.

  MODIFY ZTPP_HMA_MEID FROM TABLE IT_DATA.

  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " SEND_DATA
*&---------------------------------------------------------------------*
*&      Form  check_duplicate_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DUPLICATE_DATA.
  DATA: LT_TEMP LIKE TABLE OF ZSPP_HMA_MEID WITH HEADER LINE,
        LT_EXIST LIKE TABLE OF ZSPP_HMA_MEID WITH HEADER LINE..
  DATA:    L_INDEX LIKE SY-TABIX.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_EXIST
    FROM ZTPP_HMA_MEID
    FOR ALL ENTRIES IN IT_DATA
    WHERE VIN = IT_DATA-VIN.
  SORT LT_EXIST BY VIN.
  LOOP AT IT_DATA.
    L_INDEX = SY-TABIX.
    MOVE-CORRESPONDING IT_DATA TO LT_TEMP.
    READ TABLE LT_EXIST WITH KEY VIN = LT_TEMP-VIN.
    IF SY-SUBRC = 0.
      IF LT_TEMP = LT_EXIST.
        DELETE IT_DATA INDEX L_INDEX.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_duplicate_data
*&---------------------------------------------------------------------*
*&      Form  check_empty_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_EMPTY_DATA.
  DATA: $SY_TABIX LIKE SY-TABIX.

  LOOP AT IT_DATA.
    $SY_TABIX = SY-TABIX.
    IF IT_DATA-VIN = SPACE OR IT_DATA-MEID = SPACE.
      DELETE IT_DATA INDEX $SY_TABIX.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_empty_data
*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_MAIL.
  DATA: $LT_BODY LIKE TABLE OF SOLISTI1 WITH HEADER LINE.

  MOVE 'MEID to HMA by EAI is failed. Please check.' TO $LT_BODY.
  APPEND $LT_BODY.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      P_SUBJECT  = 'MEID to HMA Interface Failure Alert'
      P_REC_TYPE = 'C'
      P_RECEIVER = P_RVER
    TABLES
      PT_BODY    = $LT_BODY.
ENDFORM.                    " send_mail
