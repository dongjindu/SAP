************************************************************************
* Program Name      : ZIPP_PRDTN_STATUS
* Author            : Furong Wang
* Creation Date     : 04/17/2009
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Send production actual and plan data to HMC
* Modification Logs
* Date       Developer    RequestNo    Description
*
*********************************************************************

REPORT  ZIPP_PRDTN_STATUS NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

DATA : L_MSGTXT(100),
       L_RESULT(1).

CONSTANTS: C_DEST(10) VALUE 'WMPP01'.   "Interface Destination.

DATA: BEGIN OF IT_DATA OCCURS 0.
        INCLUDE STRUCTURE ZSPP_PDTN_STATUS.
DATA: END OF IT_DATA.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS: P_DATUM LIKE SY-DATUM OBLIGATORY.
SELECTION-SCREEN SKIP 1.
PARAMETERS: P_OPT1 RADIOBUTTON GROUP GRP1,
            P_OPT2 RADIOBUTTON GROUP GRP1.
SELECTION-SCREEN SKIP 1.
PARAMETERS: P_SEND AS CHECKBOX.                             "(1).
*SELECT-OPTIONS: S_DATUM FOR SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.

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
  DATA: L_SUBRC    TYPE SY-SUBRC ,
        L_ATNAM    TYPE CABN-ATNAM,
        L_ATWRT    TYPE AUSP-ATWRT,
        L_NAME     TYPE CABN-ATNAM,  "Prod. Date
        L_NAME1    TYPE CABN-ATNAM,
        L_ATFLV    TYPE AUSP-ATFLV,  "Prod. Date
        L_ATFLV_TEMP TYPE AUSP-ATFLV,
        L_TEMP(06),
        L_DATUM    TYPE SY-DATUM,
        L_NUM(08)  TYPE N,
        L_EXRP(02) TYPE N,
        L_WONO LIKE AUSP-ATWRT,
        L_WONO_ZWOSUM LIKE ZTPP_WOSUM-WO_SER,
        L_NATION LIKE ZTPP_WOSUM-NATION,
        L_DEALER LIKE ZTPP_WOSUM-DEALER,
        L_DEALER1(1),
        L_EXCL LIKE ZTPP_WOSUM-EXTC,
        L_INCL LIKE ZTPP_WOSUM-INTC,
        L_SEQ_DATE LIKE SY-DATUM,
        L_DAYS TYPE I.

  DATA: BEGIN OF LT_OBJEK OCCURS 0,
          OBJEK    LIKE AUSP-OBJEK,
          ATWRT    LIKE AUSP-ATWRT,
        END OF LT_OBJEK.
  DATA: LT_OBJEK_TEMP LIKE TABLE OF LT_OBJEK WITH HEADER LINE.

  DATA: BEGIN OF LT_ZTPPVR OCCURS 0,
        P_MODEL LIKE ZTPPVR-P_MODEL,
        P_BODY_SERIAL LIKE ZTPPVR-P_BODY_SERIAL,
        END OF LT_ZTPPVR.

  IF P_OPT1 = 'X'.
    SELECT DISTINCT P_MODEL P_BODY_SERIAL
       INTO TABLE LT_ZTPPVR
       FROM ZTPPVR
       WHERE FLAG = 'LT'
         AND K04PDAT = P_DATUM.
    LOOP AT LT_ZTPPVR.
      CONCATENATE LT_ZTPPVR-P_MODEL LT_ZTPPVR-P_BODY_SERIAL
                INTO LT_OBJEK-OBJEK.
      APPEND LT_OBJEK.
      CLEAR: LT_OBJEK.
    ENDLOOP.
  ELSE.
    L_ATFLV = L_NUM = P_DATUM.
    REFRESH LT_OBJEK.
    L_NAME = 'P_STATUS'.

    SELECT DISTINCT OBJEK
       INTO TABLE LT_OBJEK
       FROM AUSP AS AU
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
       WHERE KLART = '002'
             AND ( AU~ATWRT <> 'V05' AND AU~ATWRT <> 'V07' )
             AND CA~ATNAM = L_NAME.

    L_NAME = 'P_RP25_SHOP_DATE'.
    L_NAME1 = 'P_RP27_SHOP_DATE'.

    SELECT DISTINCT OBJEK
      APPENDING TABLE LT_OBJEK
      FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE KLART = '002' AND
            AU~ATFLV = L_ATFLV AND
            ( CA~ATNAM = L_NAME OR CA~ATNAM = L_NAME ).
  ENDIF.
  SORT LT_OBJEK BY OBJEK.

  LOOP AT LT_OBJEK.
    CLEAR: L_ATWRT.
    PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_DIST_CODE'
                                CHANGING L_ATWRT.

    IF L_ATWRT = 'AA' OR L_ATWRT = 'AB'.
      CLEAR: L_ATWRT.
      PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_USAGE_CAR'
                                  CHANGING L_ATWRT.

      IF L_ATWRT = 'P'.
        CLEAR: L_ATWRT.
        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_VIN'
                                   CHANGING L_ATWRT.
        IT_DATA-VINN = L_ATWRT.
        CLEAR: L_ATWRT.

        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK
                                  'P_DESTINATION_CODE'
                                  CHANGING L_ATWRT.
        IT_DATA-DIST = L_ATWRT.
        CLEAR: L_ATWRT.

        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_WORK_ORDER'
                                    CHANGING L_ATWRT.
        L_WONO = L_ATWRT.
*        IT_DATA-UORD = L_WONO.
        CLEAR: IT_DATA-UORD.
        CLEAR: L_ATWRT.

        CONCATENATE '20' L_WONO+1(4) INTO IT_DATA-PACK.

        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_MI'
                                   CHANGING L_ATWRT.
        IT_DATA-MCCD = L_ATWRT.
        CLEAR: L_ATWRT.

        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_OCN'
                                    CHANGING L_ATWRT.
        IT_DATA-OCCN = L_ATWRT.
        CLEAR: L_ATWRT.

        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_EXT_COLOR'
                                    CHANGING L_ATWRT.
        IT_DATA-EXCL = L_ATWRT.
        CLEAR: L_ATWRT.

        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_INT_COLOR'
                                    CHANGING L_ATWRT.
        IT_DATA-INCL = L_ATWRT.
        CLEAR: L_ATWRT.

        IT_DATA-PORT = IT_DATA-UORD+5(1).
        IT_DATA-REFE = L_WONO.


        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_SALES_ORDER'
                                    CHANGING L_ATWRT.
        IT_DATA-IVNB = L_ATWRT.
        CLEAR: L_ATWRT.

        L_DEALER = L_WONO+12(2).
        CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
             EXPORTING
                  OLD_DEALER = L_DEALER
             IMPORTING
                  NEW_DEALER = L_DEALER1.

        IT_DATA-WKNO = L_WONO+0(12).
        CONCATENATE IT_DATA-WKNO L_DEALER1 INTO IT_DATA-WKNO.

*        IT_DATA-WKNO = L_WONO.

        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_FLEET'
                                     CHANGING L_ATWRT.
        IF L_ATWRT IS INITIAL.
          IT_DATA-FLET = 'N'.
        ELSE.
          IT_DATA-FLET = 'F'.
        ENDIF.
        CLEAR: L_ATWRT.

        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_ENGINE_NO'
                                      CHANGING L_ATWRT.
        IT_DATA-ENGN = L_ATWRT.
        CLEAR: L_ATWRT.

        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_KEY_NO'
                                      CHANGING L_ATWRT.
        IT_DATA-KEYN = L_ATWRT.
        CLEAR: L_ATWRT.

        L_WONO_ZWOSUM = L_WONO+0(9).
        L_NATION = L_WONO+9(3).
        L_DEALER = L_WONO+12(2).
        L_EXCL = IT_DATA-EXCL.
        L_INCL = IT_DATA-INCL.

        SELECT SINGLE WOCREDATE INTO IT_DATA-DT07
          FROM ZTPP_WOSUM
          WHERE WO_SER = L_WONO_ZWOSUM
           AND NATION = L_NATION
           AND DEALER = L_DEALER
           AND EXTC = L_EXCL
           AND INTC = L_INCL.

        CLEAR: L_ATFLV_TEMP.
        PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
                                         'P_RP01_SHOP_DATE'
                                      CHANGING L_ATFLV_TEMP.
        IF L_ATFLV_TEMP IS INITIAL.
          IT_DATA-ES08 = 'Y'.
        ELSE.
          IT_DATA-DT08 = L_NUM = L_ATFLV_TEMP.
          IT_DATA-ES08 = 'N'.
        ENDIF.
        CLEAR: L_ATFLV_TEMP.

        PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
    'P_RP02_SHOP_DATE'
                                      CHANGING L_ATFLV_TEMP.
        IF L_ATFLV_TEMP IS INITIAL.
          IT_DATA-ES09 = 'Y'.
        ELSE.
          IT_DATA-ES09 = 'N'.
          IT_DATA-DT09 = L_NUM = L_ATFLV_TEMP.
        ENDIF.
        CLEAR: L_ATFLV_TEMP.

        PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
    'P_RP07_SHOP_DATE'
                                      CHANGING L_ATFLV_TEMP.

        IF L_ATFLV_TEMP IS INITIAL.
          IT_DATA-ES10 = 'Y'.
        ELSE.
          IT_DATA-DT10 = L_NUM = L_ATFLV_TEMP.
          IT_DATA-ES10 = 'N'.
        ENDIF.
        CLEAR: L_ATFLV_TEMP.

        PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
    'P_RP18_SHOP_DATE'
                                      CHANGING L_ATFLV_TEMP.
        IF L_ATFLV_TEMP IS INITIAL.
          IT_DATA-ES11 = 'Y'.
        ELSE.
          IT_DATA-DT11 = L_NUM = L_ATFLV_TEMP.
          IT_DATA-ES11 = 'N'.
        ENDIF.
        CLEAR: L_ATFLV_TEMP.


        PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
    'P_RP20_SHOP_DATE'
                                      CHANGING L_ATFLV_TEMP.
        IF L_ATFLV_TEMP IS INITIAL.
          IT_DATA-ES12 = 'Y'.
        ELSE.
          IT_DATA-DT12 = L_NUM = L_ATFLV_TEMP.
          IT_DATA-ES12 = 'N'.
        ENDIF.
        CLEAR: L_ATFLV_TEMP.


        PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
    'P_RP22_SHOP_DATE'
                                      CHANGING L_ATFLV_TEMP.
        IF L_ATFLV_TEMP IS INITIAL.
          IT_DATA-ES13 = 'Y'.
        ELSE.
          IT_DATA-DT13 = L_NUM = L_ATFLV_TEMP.
          IT_DATA-ES13 = 'N'.
        ENDIF.
        CLEAR: L_ATFLV_TEMP.


        PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
     'P_RP24_SHOP_DATE'
                                        CHANGING L_ATFLV_TEMP.
        IF L_ATFLV_TEMP IS INITIAL.
          CLEAR: L_ATFLV_TEMP.
          PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
    'P_RP26_SHOP_DATE'
                                       CHANGING L_ATFLV_TEMP.

        ENDIF.
        IT_DATA-DT14 = L_NUM = L_ATFLV_TEMP.

        IF  IT_DATA-DT14 IS INITIAL.
          IT_DATA-ES14 = 'Y'.
        ELSE.
          IT_DATA-ES14 = 'N'.
        ENDIF.
        CLEAR: L_ATFLV_TEMP.


        PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
    'P_RP25_SHOP_DATE'
                                       CHANGING L_ATFLV_TEMP.
        IF L_ATFLV_TEMP IS INITIAL.
          CLEAR: L_ATFLV_TEMP.
          PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
    'P_RP27_SHOP_DATE'
                                       CHANGING L_ATFLV_TEMP.

        ENDIF.
        IT_DATA-DT15 = L_NUM = L_ATFLV_TEMP.

        IF  IT_DATA-DT15 IS INITIAL.
          IT_DATA-ES15 = 'Y'.
        ELSE.
          IT_DATA-ES15 = 'N'.
        ENDIF.
        CLEAR: L_ATFLV_TEMP.




        IT_DATA-CRDT = P_DATUM.

        CLEAR: L_ATWRT.
        PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_RP_STATUS'
                                CHANGING L_ATWRT.

        PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
       'P_SEQUENCE_DATE'
                                         CHANGING L_ATFLV_TEMP.
        IF L_ATFLV_TEMP IS INITIAL.

        ELSE.
          L_SEQ_DATE = L_NUM = L_ATFLV_TEMP.
        ENDIF.
        CLEAR: L_ATFLV_TEMP.


        IT_DATA-ST01 = '0'.
        IT_DATA-ST02 = '0'.
        IT_DATA-ST03 = '0'.
        IT_DATA-ST04 = '0'.
        IT_DATA-ST05 = '0'.
        IT_DATA-ST06 = '0'.
        IT_DATA-ST07 = '0'.
        IT_DATA-ST08 = '0'.
        IT_DATA-ST09 = '0'.
        IT_DATA-ST10 = '0'.
        IT_DATA-ST10 = '0'.
        IT_DATA-ST11 = '0'.
        IT_DATA-ST12 = '0'.
        IT_DATA-ST13 = '0'.
        IT_DATA-ST14 = '0'.
        IT_DATA-ST15 = '0'.

        CASE L_ATWRT.
          WHEN '00'.
            L_DAYS = SY-DATUM - L_SEQ_DATE.
            IF L_DAYS = 0.
              IT_DATA-ST01 = '1'.
            ELSEIF L_DAYS >= 29.
              IT_DATA-ST02 = '1'.
            ELSEIF L_DAYS >= 22 AND L_DAYS <= 28.
              IT_DATA-ST03 = '1'.
            ELSEIF L_DAYS >= 15 AND L_DAYS <= 21.
              IT_DATA-ST04 = '1'.
            ELSEIF L_DAYS >= 8 AND L_DAYS <= 14.
              IT_DATA-ST05 = '1'.
            ELSEIF L_DAYS >= 4 AND L_DAYS <= 7.
              IT_DATA-ST06 = '1'.
            ELSEIF L_DAYS >= 1 AND L_DAYS <= 3.
              IT_DATA-ST07 = '1'.
            ENDIF.
          WHEN '01'.
            IT_DATA-ST08 = '1'.
          WHEN '02' OR '03' OR '04' OR '06'.
            IT_DATA-ST09 = '1'.
          WHEN '07'.
            IT_DATA-ST10 = '1'.
          WHEN '18' OR '19' OR '20' OR '21'.
            IT_DATA-ST11 = '1'.
          WHEN '22' OR '23'.
            IT_DATA-ST13 = '1'.
          WHEN '24' OR '26'.
            IT_DATA-ST14 = '1'.
          WHEN '25' OR '27'.
            IT_DATA-ST15 = '1'.
          WHEN OTHERS.
            IF 08 >= L_ATWRT AND L_ATWRT <= 17.
              IT_DATA-ST10 = '1'.
            ENDIF.
        ENDCASE.

        IT_DATA-ST12 = '0'.
        IT_DATA-ST14 = '0'.

        APPEND IT_DATA.
        CLEAR: IT_DATA.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*call function ''.

FORM SEND_DATA.
  DATA: IT_SAVE LIKE TABLE OF ZTPP_PDTN_STATUS WITH HEADER LINE,
        L_FLAG(1).

  IF IT_DATA[] IS INITIAL.
    EXIT.
  ENDIF.
  IF P_SEND IS INITIAL.
    LOOP AT IT_DATA.
      MOVE-CORRESPONDING IT_DATA TO IT_SAVE.
      IT_SAVE-RUNDATE = SY-DATUM.
      IT_SAVE-RUNTIME = SY-UZEIT.
      APPEND IT_SAVE.
      CLEAR: IT_SAVE.
    ENDLOOP.

    DELETE FROM ZTPP_PDTN_STATUS  CLIENT SPECIFIED
              WHERE MANDT = SY-MANDT.
    INSERT ZTPP_PDTN_STATUS FROM TABLE IT_SAVE.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE I000 WITH 'Table saving error'.
    ENDIF.
  ELSE.
    CALL FUNCTION 'Z_FPP_PRDTN_STATUS'
      DESTINATION C_DEST
      IMPORTING
        FLAG          = L_RESULT
      TABLES
        I_PRDTN_STATUS    = IT_DATA
      EXCEPTIONS
             COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
             SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

    IF SY-SUBRC = 0.
      L_FLAG = 'S'.
      MESSAGE I001 WITH 'Production Status data successfully sent out'.
    ELSE.
      L_FLAG = 'E'.
      MESSAGE I001 WITH L_MSGTXT.
    ENDIF.

    LOOP AT IT_DATA.
      MOVE-CORRESPONDING IT_DATA TO IT_SAVE.
      IT_SAVE-FLAG = L_FLAG.
      IT_SAVE-MESS = L_MSGTXT.
      IT_SAVE-RUNDATE = SY-DATUM.
      IT_SAVE-RUNTIME = SY-UZEIT.
      APPEND IT_SAVE.
      CLEAR: IT_SAVE.
    ENDLOOP.

    DELETE FROM ZTPP_PDTN_STATUS  CLIENT SPECIFIED
             WHERE MANDT = SY-MANDT.
    INSERT ZTPP_PDTN_STATUS FROM TABLE IT_SAVE.
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
