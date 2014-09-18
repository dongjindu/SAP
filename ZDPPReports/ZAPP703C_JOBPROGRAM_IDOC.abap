************************************************************************
* Program Name      : ZAPP703C_JOBPROGRAM
* Author            : Bobby
* Creation Date     : 2003.09.16.
* Specifications By : Bobby
* Pattern           : 5.2.2
* Development Request No : UD1K902220
* Addl Documentation:
* Description       : Interface Work Order from Legacy System
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 1/16/2007  Furong Wang  UD1K930342   Get VIN SPEC from Variant
*                                      directly (no comparing 219)
************************************************************************
REPORT  ZAPP703C_JOBPROGRAM   LINE-SIZE  700 MESSAGE-ID ZMPP.

TABLES: ZTPP_PP_LOG_HEAD,       " Table of the Interface Log(Header)
        ZTPP_PP_LOG_DETA,       " Table of the Interface Log(Detail)
        ZTPP_COMMON_VALS,       " Table of the last Working Information
        ZTPP_WOSUM    ,         " Table of the WorkOrder Summary
        ZTPP_KSBOHMM  .

DATA: BEGIN OF IT_DATA       OCCURS 0.
        INCLUDE STRUCTURE    ZTPP_KSBOHMM .
DATA:   P_FLAG               TYPE C,
        WO                   TYPE C,
        H_C                  TYPE C,
        EXIST                TYPE C,
        MATERIAL             LIKE MARA-MATNR,
      END OF IT_DATA.

DATA: BEGIN OF IT_ALC         OCCURS 0.
        INCLUDE STRUCTURE     CUKB    .
DATA:   KNKTX                 LIKE CUKBT-KNKTX,
        CODE(3)               TYPE C  ,
        RP(2)                 TYPE N  ,
        TYPE_ALC              TYPE C  ,
        CHAR_ALC              LIKE CABN-ATNAM,
      END OF IT_ALC .

DATA: IT_MODEL LIKE TABLE OF ZTPP_MODEL_CONV WITH HEADER LINE.

DATA: IT_MSG                 LIKE TABLE OF BDCMSGCOLL  WITH HEADER LINE,
      IT_BDCDATA             LIKE TABLE OF BDCDATA     WITH HEADER LINE,
      IT_ERROR               LIKE TABLE OF IT_DATA     WITH HEADER LINE,
      WA_MODE                TYPE C VALUE 'A',
      WA_DATE                LIKE SY-DATUM   ,
      WA_CNT                 TYPE I       ,
      WA_FLG                 TYPE C       ,
      WA_ERROR               TYPE C       ,
      WA_NUMBER              LIKE ZTPP_PP_LOG_HEAD-LOGKEY,
      WA_DATA                LIKE IT_DATA ,
      WA_SEQ                 LIKE ZTPP_PP_LOG_DETA-SEQUENCE,
      WA_WKDATE              LIKE SY-DATUM.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS: P_WOSER          LIKE ZTPP_WOSUM-WO_SER,
            P_LOG            LIKE ZTPP_PP_LOG_HEAD-LOGKEY.
SELECTION-SCREEN END OF BLOCK B1.

*************** DO NOT USE!!!! *****************************************
DATA: IT_REC                  LIKE TABLE OF MARA ,
      P_TCODE                 LIKE  TSTC-TCODE                ,
      P_CMODE                 TYPE  C                         ,
      P_PMODE                 TYPE  C   VALUE   'N'           ,
      WA_FILENAME             LIKE  RLGRAP-FILENAME,
      WA_FILETYPE             LIKE  RLGRAP-FILETYPE VALUE 'DAT',
      WA_BDCGROUP             LIKE  SY-UNAME.          " APQI-GROUPID
************************************************************************

INITIALIZATION.

LOAD-OF-PROGRAM.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM BDC_PROCESSING .
  PERFORM CHECK_PO.
  INCLUDE ZCPP103_COMMON_ROUTINE .

*&---------------------------------------------------------------------*
*&      Form  SET_WORKDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_WORKDATE.
  DATA: L_DATE                TYPE D,
        L_GAPS                TYPE I.

  SELECT SINGLE DATES  INTO WA_WKDATE
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = SY-REPID   .

  IF SY-SUBRC <> 0.
    " Step 1: Table's Entry Error for the ZTPP_COMMON_VALS.......
    PERFORM CREATE_LOG USING 'E' 1 TEXT-001  SPACE .
    WA_ERROR = 'E' .     EXIT.
  ENDIF.

  L_DATE = WA_WKDATE.

  IF WA_DATE IS INITIAL.
    WA_DATE = WA_WKDATE.
  ENDIF.

  SELECT MAX( CHG_DATE ) INTO L_DATE
    FROM ZTPP_KSBOHMM_IF  .

  L_GAPS = L_DATE - WA_WKDATE.
  L_DATE = WA_WKDATE.

  IF L_GAPS > 0 .
    DO L_GAPS TIMES.
      L_DATE    = L_DATE    + 1 .
      SELECT SINGLE *
        FROM ZTPP_KSBOHMM
       WHERE CHG_DATE = L_DATE .

      IF SY-SUBRC = 0.
        IF WA_FLG IS INITIAL.
          WA_CNT = WA_CNT + 1 .
        ELSE.
          WA_WKDATE = L_DATE  .
          EXIT .
        ENDIF.
      ENDIF.
    ENDDO.
  ELSE.
    CHECK WA_FLG IS INITIAL .
    " Step 2: Not found entry for the processig.....
    PERFORM CREATE_LOG USING 'S' 2 TEXT-002  SPACE .
    WA_ERROR = 'E' .     EXIT.
  ENDIF.
ENDFORM.                    " SET_WORKDATE

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM ZTPP_KSBOHMM_IF
    WHERE WO_SER EQ P_WOSER.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_PROCESSING.
  DATA: L_MODSUM             LIKE ZTPP_KSBOHMM-MODQTY,
        L_INITSUM            LIKE ZTPP_KSBOHMM-INITQTY,
        L_MATERIAL           LIKE MARA-MATNR           ,
        L_COLOR              TYPE C       ,
        L_CM(15)             TYPE C       ,
        L_EXIST              TYPE C       ,
        L_BMDL(2).
  SELECT * INTO TABLE IT_MODEL
    FROM ZTPP_MODEL_CONV.


  SORT IT_DATA BY WO_SER NATION DEALER EXTC INTC .

  LOOP AT IT_DATA.
    CLEAR: L_EXIST, IT_BDCDATA, IT_BDCDATA[], WA_ERROR.

    PERFORM CONVERSION_SPACE_TO_HYPEN  USING IT_DATA-S219.
    PERFORM CHECK_COLOR      USING L_COLOR.

*## CHECK IT_DATA-EXTC and Choose Creating WOHD or WOCL.
    CASE   L_COLOR .
      WHEN 'Y'    .         " WORK ORDER HEADER.
*#1   Create Material Code ex) E0402X350B28AA except ColorNo.
        CONCATENATE IT_DATA-WO_SER
                    IT_DATA-NATION
                    IT_DATA-DEALER
               INTO L_MATERIAL .

        IT_DATA-WO = 'H' .

*#2    CHECK MATERIAL EXIST FROM MARA
        PERFORM CHECK_MATERIAL USING L_EXIST  L_MATERIAL .
        CASE L_EXIST.
          WHEN 'Y'    .         "  EXIST


          WHEN 'N'    .         "  NO DATA
            CLEAR: L_CM.
            L_BMDL = IT_DATA-BMDL(2).

            READ TABLE IT_MODEL WITH KEY BMDL = L_BMDL.
            IF SY-SUBRC = 0.
*#2-1       Create Model Number ex) INF_WOHD
              CONCATENATE IT_MODEL-MODEL '_' 'WOHD' INTO L_CM .
            ELSE.


              DATA  : LV_ERROR(100).

              CONCATENATE '[WOHD]' 'Error in Model Code :'
                           L_MATERIAL
                           IT_DATA-BMDL
                          INTO LV_ERROR.

              PERFORM UPDATE_STATUS_TABLE
                  USING 'E'
                        LV_ERROR
                        IT_DATA.

            ENDIF.

*#2-2 Run BDC Create Work Order Header.
            PERFORM BDC_MATERIAL_CREATE USING L_MATERIAL 'WOHD' L_CM .
            PERFORM CHECK_BDC_RESULT USING 'CREATE' 'WOHD' L_MATERIAL.
        ENDCASE.

      WHEN 'N'    .         " WORK ORDER COLOR .
*#1   Create Material Code ex) E07031255B28AAA1QZ include ColorNo
        CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
                    IT_DATA-EXTC   IT_DATA-INTC   INTO L_MATERIAL .
        IT_DATA-WO = 'C' .
        PERFORM CHECK_MATERIAL USING L_EXIST  L_MATERIAL.
        CASE L_EXIST.
          WHEN 'Y'    .         "  EXIST
** added by Furong
*            PERFORM bdc_material_update USING l_material 'WOCL' .
*            PERFORM update_wosum        USING l_material        .
** end of add
          WHEN 'N'    .         "  NO DATA
            CLEAR: L_CM.

            L_BMDL = IT_DATA-BMDL(2).
            READ TABLE IT_MODEL WITH KEY BMDL = L_BMDL.
            IF SY-SUBRC = 0.
              CONCATENATE IT_MODEL-MODEL '_' 'WOCL' INTO L_CM .
            ELSE.

              CONCATENATE '[WOCL]' 'Error in Model Code :'
                L_MATERIAL
                IT_DATA-BMDL
               INTO LV_ERROR.

              PERFORM UPDATE_STATUS_TABLE
                  USING 'E'
                        LV_ERROR
                        IT_DATA.

              CONTINUE.
            ENDIF.

*#2  Run BDC Create Work Order Header.
            PERFORM BDC_MATERIAL_CREATE USING L_MATERIAL 'WOCL' L_CM .
            PERFORM CHECK_BDC_RESULT USING 'CREATE' 'WOCL' L_MATERIAL.
*#3  Insert WOSUM .
            PERFORM INSERT_WOSUM        USING L_MATERIAL        .
        ENDCASE.
    ENDCASE.
    MODIFY IT_DATA.
  ENDLOOP.


  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            WAIT = 'X'.

  DATA: W_MODEL(3).

  LOOP AT IT_DATA  WHERE  P_FLAG = 'X'.
    CLEAR: L_MATERIAL, W_MODEL.
    CASE IT_DATA-WO.
      WHEN 'H'.
        CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
                                                  INTO L_MATERIAL .

        L_BMDL = IT_DATA-BMDL(2).
        READ TABLE IT_MODEL WITH KEY BMDL = L_BMDL.
        IF SY-SUBRC = 0.
          W_MODEL = IT_MODEL-MODEL.
        ELSE.
          CONCATENATE 'Error in Model Code :'
            L_MATERIAL
            IT_DATA-BMDL
           INTO LV_ERROR.

          PERFORM UPDATE_STATUS_TABLE
              USING 'E'
                    LV_ERROR
                    IT_DATA.
          CONTINUE.
        ENDIF.
        PERFORM BDC_WOHD USING L_MATERIAL W_MODEL.
      WHEN 'C'.
        CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
                    IT_DATA-EXTC   IT_DATA-INTC   INTO L_MATERIAL .
        PERFORM BDC_WOCL USING L_MATERIAL .
    ENDCASE.

    CLEAR: W_MODEL.
    L_BMDL = IT_DATA-BMDL(2).
    READ TABLE IT_MODEL WITH KEY BMDL = L_BMDL.
    IF SY-SUBRC = 0.
      W_MODEL = IT_MODEL-MODEL.
    ELSE.
      CONCATENATE 'Error in Model Code :'
        L_MATERIAL
        IT_DATA-BMDL
       INTO LV_ERROR.

      PERFORM UPDATE_STATUS_TABLE
          USING 'E'
                LV_ERROR
                IT_DATA.

      CONTINUE.
    ENDIF.

    PERFORM CREATE_CLASSIFICATION_MM   USING L_MATERIAL  IT_DATA-WO
                                             W_MODEL.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  check_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COLOR  text
*----------------------------------------------------------------------*
FORM CHECK_COLOR USING    PA_COLOR.
  PA_COLOR = 'N'  .
  IF IT_DATA-EXTC = '***' AND IT_DATA-INTC = '***'.
    PA_COLOR = 'Y' .
  ENDIF.
ENDFORM.                    " check_color

*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_EXIST  text
*----------------------------------------------------------------------*
FORM CHECK_MATERIAL USING    PA_EXIST  PA_MATERIAL .
  SELECT SINGLE MATNR INTO PA_MATERIAL
    FROM MARA
   WHERE MATNR = PA_MATERIAL .

  IF SY-SUBRC = 0.
    PA_EXIST = 'Y' .
  ELSE.
    PA_EXIST = 'N' .
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  bdc_material_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_MATERIAL_CREATE   USING  PA_MATERIAL  PA_TYPE  PA_CM .
  DATA: L_NAME0(20), L_NAME1(20), L_NAME2(20), L_NAME3(20),
        L_NAME4(20), L_NAME5(20), L_NAME6(20), L_NAME7(20),
        L_NAME8(20), L_NAME9(20), L_VALS0(01), L_VALS1(01),
        L_VALS2(20), L_VALS3(20), L_VALS4(20), L_VALS5(20),
        L_VALS6(20), L_VALS7(20), L_VALS8(20), L_VALS9(20),
        L_WO_CREATE_DATE(10) TYPE C          ,
        L_GEN_DATE(10)       TYPE C          ,
        L_NO(3)              TYPE N  VALUE 9 ,
        L_MATNR              LIKE MARA-MATNR ,
        L_MAKTX              LIKE MAKT-MAKTX ,
        L_CLASS(20)          TYPE C,
        L_MODEL(3),
        L_QTY                LIKE ZTPP_WOSUM-SEQQTY .

  DATA: L_LEN TYPE I,
        L_NEW_DEALER(1),
        L_BMDL(2).

  L_QTY = IT_DATA-MODQTY.

  SELECT SINGLE MATNR INTO L_MATNR
    FROM MARA
   WHERE MATNR = PA_CM .

  IF SY-SUBRC NE 0.
    " Step 5: Master Data does not exist..
    PERFORM CREATE_LOG USING 'E' 5 TEXT-004 PA_CM  .
    PERFORM CREATE_LOG USING 'R' 5 TEXT-004 IT_DATA  .
    PERFORM CREATE_LOG USING 'R' 5 TEXT-004 IT_DATA-S219  .
    WA_ERROR = 'X'.
    EXIT .
  ENDIF.

  CHECK WA_ERROR IS INITIAL .
  WRITE SY-DATUM TO L_GEN_DATE       .
** Changed by Furong on 10/09/07 for EBOM
*  CONCATENATE it_data-moye it_data-nation it_data-dealer it_data-bmdl
*         INTO l_maktx .
*  CONCATENATE l_maktx it_data-ocnn it_data-vers INTO l_maktx
*         SEPARATED BY space .
  L_LEN = STRLEN( IT_DATA-BMDL ).
  IF L_LEN = 7.
    CONCATENATE IT_DATA-MOYE IT_DATA-NATION IT_DATA-DEALER IT_DATA-BMDL
           INTO L_MAKTX .
    CONCATENATE L_MAKTX IT_DATA-OCNN IT_DATA-VERS INTO L_MAKTX
           SEPARATED BY SPACE .
  ELSE.
    CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
         EXPORTING
              OLD_DEALER = IT_DATA-DEALER
         IMPORTING
              NEW_DEALER = L_NEW_DEALER.
    CONCATENATE IT_DATA-MOYE IT_DATA-NATION L_NEW_DEALER IT_DATA-BMDL
          INTO L_MAKTX .
    CONCATENATE L_MAKTX IT_DATA-OCNN INTO L_MAKTX.
    CONCATENATE L_MAKTX IT_DATA-VERS INTO L_MAKTX SEPARATED BY SPACE.
  ENDIF.
** End of change

* Changed by Furong on 03/12/10
  L_BMDL = IT_DATA-BMDL(2).
  READ TABLE IT_MODEL WITH KEY BMDL = L_BMDL.
  IF SY-SUBRC = 0.
    L_MODEL = IT_MODEL-MODEL.
  ELSE.
    WRITE: 'Error in Model Code:' , L_MAKTX, IT_DATA-BMDL.
    EXIT.
  ENDIF.

*  IF IT_DATA-BMDL(2) = 'EM'.
*    L_MODEL = 'EMF'.
*  ELSEIF IT_DATA-BMDL(2) = 'CR'.
*    L_MODEL = 'CRA'.
*  ELSEIF IT_DATA-BMDL(2) = 'IN'.
*    L_MODEL = 'INF'.
*  ELSE.
*    WRITE: 'Error in Model Code:' , L_MAKTX, IT_DATA-BMDL.
*    EXIT.
*  ENDIF.
** end of chnage on 03/12/10

  PERFORM BDC_DYNPRO_PROCESSING USING :
                         'X'  'SAPLMGMM'             '0060',
                         ' '  'BDC_OKCODE'           '=AUSW' ,
                         ' '  'RMMG1-MATNR'           PA_MATERIAL,
                         ' '  'RMMG1-MBRSH'          'A'         ,
                         ' '  'RMMG1-MTART'           PA_TYPE    ,

                         'X'  'SAPLMGMM'             '0070',
                         ' '  'BDC_OKCODE'           '=RESA' ,

                         'X'  'SAPLMGMM'             '0070',
                         ' '  'BDC_OKCODE'           '=ENTR' ,
                         ' '  'MSICHTAUSW-KZSEL(01)' 'X'     ,
                         ' '  'MSICHTAUSW-KZSEL(02)' 'X'     ,
                         ' '  'MSICHTAUSW-KZSEL(03)' 'X'     ,


                         'X'  'SAPLMGMM'             '5004',
                         ' '  'BDC_OKCODE'           '=SP02' ,
                         ' '  'MAKT-MAKTX'            L_MAKTX    ,
                         ' '  'MARA-MEINS'           'EA'         ,

                         'X'  'SAPLMGMM'             '5004',
                         ' '  'BDC_OKCODE'           '=PB21' ,
                         ' '  'MARA-SATNR'            PA_CM  ,

                         'X'  'SAPLCEI0'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,
                         ' '  'RCTMS-MNAME(01)'      'P_MI'          ,
                         ' '  'RCTMS-MWERT(01)'       IT_DATA-BMDL   ,
                         ' '  'RCTMS-MNAME(02)'      'P_OCN'         ,
                         ' '  'RCTMS-MWERT(02)'       IT_DATA-OCNN   ,
                         ' '  'RCTMS-MNAME(03)'      'P_VERSION'     ,
                         ' '  'RCTMS-MWERT(03)'       IT_DATA-VERS   ,
                         ' '  'RCTMS-MNAME(04)'      'P_MODEL'       ,
                         ' '  'RCTMS-MWERT(04)'       L_MODEL        ,
*                         ' '  'RCTMS-MWERT(04)'       it_data-bmdl(3),
                         ' '  'RCTMS-MNAME(05)'      'P_WO_SER',
                         ' '  'RCTMS-MWERT(05)'       IT_DATA-WO_SER ,
                         ' '  'RCTMS-MNAME(06)'      'P_NATION',
                         ' '  'RCTMS-MWERT(06)'       IT_DATA-NATION ,
                         ' '  'RCTMS-MNAME(07)'      'P_DEALER',
                         ' '  'RCTMS-MWERT(07)'       IT_DATA-DEALER ,
                         ' '  'RCTMS-MNAME(08)'      'P_MODEL_YEAR'  ,
                         ' '  'RCTMS-MWERT(08)'       IT_DATA-MOYE   ,
                         ' '  'RCTMS-MNAME(09)'      'P_ORDER_PACK'  ,
                         ' '  'RCTMS-MWERT(09)'       PA_MATERIAL(5) ,

                         'X'  'SAPLMGMM'             '5004',
                         ' '  'BDC_OKCODE'           '=PB21' .

  IF PA_TYPE = 'WOCL' .
    PERFORM BDC_DYNPRO_PROCESSING USING :
                          'X'  'SAPLCEI0'             '0109',
                          ' '  'BDC_OKCODE'           '=BACK' ,
                          ' '  'RCTMS-MNAME(01)'      'COLOREXT',
                          ' '  'RCTMS-MWERT(01)'       IT_DATA-EXTC   ,
                          ' '  'RCTMS-MNAME(02)'      'COLORINT'    ,
                          ' '  'RCTMS-MWERT(02)'       IT_DATA-INTC ,

                          'X'  'SAPLMGMM'             '5004',
                          ' '  'BDC_OKCODE'           '=PB21' .
  ENDIF.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                        'X'  'SAPLCEI0'             '0109',
                        ' '  'BDC_OKCODE'           '=BACK' ,
                        ' '  'RCTMS-MNAME(01)'      'P_219_1',
                        ' '  'RCTMS-MWERT(01)'       IT_DATA-S219+0(1),
                        ' '  'RCTMS-MNAME(02)'      'P_219_2',
                        ' '  'RCTMS-MWERT(02)'       IT_DATA-S219+1(1),
                        ' '  'RCTMS-MNAME(03)'      'P_219_3',
                        ' '  'RCTMS-MWERT(03)'       IT_DATA-S219+2(1),
                        ' '  'RCTMS-MNAME(04)'      'P_219_4',
                        ' '  'RCTMS-MWERT(04)'       IT_DATA-S219+3(1),
                        ' '  'RCTMS-MNAME(05)'      'P_219_5',
                        ' '  'RCTMS-MWERT(05)'       IT_DATA-S219+4(1),
                        ' '  'RCTMS-MNAME(06)'      'P_219_6',
                        ' '  'RCTMS-MWERT(06)'       IT_DATA-S219+5(1),
                        ' '  'RCTMS-MNAME(07)'      'P_219_7',
                        ' '  'RCTMS-MWERT(07)'       IT_DATA-S219+6(1),
                        ' '  'RCTMS-MNAME(08)'      'P_219_8',
                        ' '  'RCTMS-MWERT(08)'       IT_DATA-S219+7(1),
                        ' '  'RCTMS-MNAME(09)'      'P_219_9',
                        ' '  'RCTMS-MWERT(09)'       IT_DATA-S219+8(1).

  DO  9 TIMES.
    L_VALS1 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO+1(2)  INTO  L_NAME1 .
    L_VALS2 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO+1(2)  INTO  L_NAME2 .
    L_VALS3 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO+1(2)  INTO  L_NAME3 .
    L_VALS4 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO+1(2)  INTO  L_NAME4 .
    L_VALS5 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO+1(2)  INTO  L_NAME5 .
    L_VALS6 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO+1(2)  INTO  L_NAME6 .
    L_VALS7 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO+1(2)  INTO  L_NAME7 .
    L_VALS8 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO+1(2)  INTO  L_NAME8 .
    L_VALS9 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO+1(2)  INTO  L_NAME9 .
    L_VALS0 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO+1(2)  INTO  L_NAME0 .

    PERFORM BDC_DYNPRO_PROCESSING USING :
                         'X'  'SAPLMGMM'             '5004',
                         ' '  'BDC_OKCODE'           '=PB21' ,

                         'X'  'SAPLCEI0'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,
                         ' '  'RCTMS-MNAME(01)'       L_NAME1,
                         ' '  'RCTMS-MWERT(01)'       L_VALS1,
                         ' '  'RCTMS-MNAME(02)'       L_NAME2,
                         ' '  'RCTMS-MWERT(02)'       L_VALS2,
                         ' '  'RCTMS-MNAME(03)'       L_NAME3,
                         ' '  'RCTMS-MWERT(03)'       L_VALS3,
                         ' '  'RCTMS-MNAME(04)'       L_NAME4,
                         ' '  'RCTMS-MWERT(04)'       L_VALS4,
                         ' '  'RCTMS-MNAME(05)'       L_NAME5,
                         ' '  'RCTMS-MWERT(05)'       L_VALS5,
                         ' '  'RCTMS-MNAME(06)'       L_NAME6,
                         ' '  'RCTMS-MWERT(06)'       L_VALS6,
                         ' '  'RCTMS-MNAME(07)'       L_NAME7,
                         ' '  'RCTMS-MWERT(07)'       L_VALS7,
                         ' '  'RCTMS-MNAME(08)'       L_NAME8,
                         ' '  'RCTMS-MWERT(08)'       L_VALS8,
                         ' '  'RCTMS-MNAME(09)'       L_NAME9,
                         ' '  'RCTMS-MWERT(09)'       L_VALS9,
                         ' '  'RCTMS-MNAME(10)'       L_NAME0,
                         ' '  'RCTMS-MWERT(10)'       L_VALS0.
  ENDDO.

  DO 12 TIMES.
    L_VALS1 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO       INTO  L_NAME1 .
    L_VALS2 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO       INTO  L_NAME2 .
    L_VALS3 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO       INTO  L_NAME3 .
    L_VALS4 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO       INTO  L_NAME4 .
    L_VALS5 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO       INTO  L_NAME5 .
    L_VALS6 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO       INTO  L_NAME6 .
    L_VALS7 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO       INTO  L_NAME7 .
    L_VALS8 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO       INTO  L_NAME8 .
    L_VALS9 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO       INTO  L_NAME9 .
    L_VALS0 = IT_DATA-S219+L_NO(1) .
    L_NO = L_NO + 1.   CONCATENATE 'P_219_' L_NO       INTO  L_NAME0 .

    PERFORM BDC_DYNPRO_PROCESSING USING :
                         'X'  'SAPLMGMM'             '5004',
                         ' '  'BDC_OKCODE'           '=PB21' ,

                         'X'  'SAPLCEI0'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,
                         ' '  'RCTMS-MNAME(01)'       L_NAME1,
                         ' '  'RCTMS-MWERT(01)'       L_VALS1,
                         ' '  'RCTMS-MNAME(02)'       L_NAME2,
                         ' '  'RCTMS-MWERT(02)'       L_VALS2,
                         ' '  'RCTMS-MNAME(03)'       L_NAME3,
                         ' '  'RCTMS-MWERT(03)'       L_VALS3,
                         ' '  'RCTMS-MNAME(04)'       L_NAME4,
                         ' '  'RCTMS-MWERT(04)'       L_VALS4,
                         ' '  'RCTMS-MNAME(05)'       L_NAME5,
                         ' '  'RCTMS-MWERT(05)'       L_VALS5,
                         ' '  'RCTMS-MNAME(06)'       L_NAME6,
                         ' '  'RCTMS-MWERT(06)'       L_VALS6,
                         ' '  'RCTMS-MNAME(07)'       L_NAME7,
                         ' '  'RCTMS-MWERT(07)'       L_VALS7,
                         ' '  'RCTMS-MNAME(08)'       L_NAME8,
                         ' '  'RCTMS-MWERT(08)'       L_VALS8,
                         ' '  'RCTMS-MNAME(09)'       L_NAME9,
                         ' '  'RCTMS-MWERT(09)'       L_VALS9,
                         ' '  'RCTMS-MNAME(10)'       L_NAME0,
                         ' '  'RCTMS-MWERT(10)'       L_VALS0.
  ENDDO.

  CONCATENATE  'P_'  PA_TYPE  '_001'                  INTO  L_CLASS.

  " Classificaition View : SP04
  PERFORM BDC_DYNPRO_PROCESSING USING :
                         'X'  'SAPLMGMM'             '5004',
                         ' '  'BDC_OKCODE'           '=SP03' ,

                         'X'  'SAPLCLCA'             '0602',
                         ' '  'BDC_OKCODE'           '=ENTE' ,
                         ' '  'RMCLF-KLART'          '001'   ,

                         'X'  'SAPLCLFM'             '0500',
                         ' '  'BDC_OKCODE'           '=AUSW' ,
                         ' '  'RMCLF-KREUZ(01)'      'X'     ,
                         ' '  'RMCLF-CLASS(01)'       L_CLASS,

                         'X'  'SAPLCTMS'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,

                         'X'  'SAPLCLFM'             '0500',
                         ' '  'BDC_OKCODE'           '=SAVE'.

  CALL TRANSACTION 'MM01'  USING IT_BDCDATA           MODE WA_MODE
                           MESSAGES INTO              IT_MSG .
ENDFORM.                    " bdc_material_create

*&---------------------------------------------------------------------*
*&      Form  bdc_material_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_MATERIAL_UPDATE  USING  PA_MATERIAL  PA_CM   .
  DATA: L_CONF               LIKE TABLE OF ZSPP_VIN_VALUE
                                                       WITH HEADER LINE,
        L_VARIABLE           LIKE TABLE OF L_CONF      WITH HEADER LINE,
        L_BASIC              LIKE TABLE OF L_CONF      WITH HEADER LINE,
        L_INSTANCE           LIKE MARA-CUOBF,
        L_QTY                TYPE I.

*  l_qty = it_data-modqty.
*
*  " Check the MODQTY Values..  if Old MODQTY is bigger.. Error..
*  L_CONF-ATNAM = 'P_SEQ_QTY' . APPEND L_CONF.
*  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*       EXPORTING
*            object    = pa_material
*            ctype     = '001'
*       TABLES
*            val_table = l_conf.
*
*  IF sy-subrc = 0.
**    READ TABLE l_conf  WITH KEY  atnam = 'P_SEQ_QTY' .
**    IF sy-subrc = 0.
*    READ TABLE L_CONF  INDEX 1.
*      IF l_conf-atwrt > l_qty .
*        " Step 3: Sequence Quantity is Over!!!!!
*        PERFORM create_log USING 'E' 3 text-003 it_data.
*        PERFORM create_log USING 'R' 3 text-003 it_data.
*        PERFORM create_log USING 'R' 3 text-003 it_data-S219.
*        wa_error = 'X' .
*        EXIT .
*      ENDIF.
**    ENDIF.
*  ENDIF.
*
*  CHECK wa_error IS INITIAL.

* l_variable-atnam = 'P_SEQ_QTY'        .
* l_variable-atwrt =  l_conf-atwrt      .   APPEND l_variable .
  L_VARIABLE-ATNAM = 'P_MOD_QTY'        .
  L_VARIABLE-ATWRT =  IT_DATA-MODQTY    .   APPEND L_VARIABLE .
*  l_variable-atnam = 'P_MOD_DATE'       .
*  l_variable-atwrt =  sy-datum          .   APPEND l_variable .
  L_VARIABLE-ATNAM = 'P_WO_MODI_DATE'   .
  L_VARIABLE-ATWRT =  IT_DATA-CHG_DATE  .   APPEND L_VARIABLE .
*  IF pa_cm = 'WOCL' .
*    l_variable-atnam = 'P_ORDER_ZONE'   .
*    l_variable-atwrt =  it_data-orzn    .     APPEND l_variable .
*  ELSE.
*    l_variable-atnam = 'P_LC_COUNT'     .
*    l_variable-atwrt =  it_data-lcnt    .     APPEND l_variable .
*    l_variable-atnam = 'P_REGION_PORT'  .
*    l_variable-atwrt =  it_data-regn    .     APPEND l_variable .
*  ENDIF.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = PA_MATERIAL
            MODE         = 'W'
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = L_VARIABLE
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC = 0.
    LOOP AT L_VARIABLE WHERE ZFLAG = 'E' .
      " Step 4: Material's Characteristic Value UPDATE Error...
      PERFORM CREATE_LOG USING 'E' 4 TEXT-010 IT_DATA.
      PERFORM CREATE_LOG USING 'E' 4 TEXT-010 L_VARIABLE.
      WA_ERROR = 'X' .
    ENDLOOP.
  ENDIF.
ENDFORM.                    " bdc_material_update

*&---------------------------------------------------------------------*
*&      Form  UPDATE_WOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_WOSUM  USING PA_MATERIAL .
  DATA: L_UDATE               LIKE SY-DATUM.

  CHECK WA_ERROR IS INITIAL  .
  L_UDATE = WA_DATE + 1      .
  " AEDAT Field is changed for the SD Module for the Testing...
  UPDATE ZTPP_WOSUM  SET: WOMODDATE = IT_DATA-CHG_DATE
                          MODQTY    = IT_DATA-MODQTY
                          AEDAT     = WA_DATE
                          AEZET     = SY-UZEIT
                          AENAM     = SY-UNAME
                    WHERE WO_SER = PA_MATERIAL(9)
                      AND NATION = PA_MATERIAL+9(3)
                      AND DEALER = PA_MATERIAL+12(2)
                      AND EXTC   = PA_MATERIAL+14(2)
                      AND INTC   = PA_MATERIAL+16(2) .

  IF SY-SUBRC NE 0.
    " Step 7: Summary Table Update Error..
    PERFORM CREATE_LOG USING 'E' 7 TEXT-006 IT_DATA.
  ENDIF.
ENDFORM.                    " UPDATE_WOSUM

*&---------------------------------------------------------------------*
*&      Form  INSERT_WOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_WOSUM  USING PA_MATERIAL .
  DATA: L_FSC                 LIKE ZTPP_WOSUM-FSC,
        L_IDATE               LIKE SY-DATUM.

  CHECK WA_ERROR IS INITIAL  .
  L_IDATE = WA_DATE + 1      .

  PERFORM CONCATE_FSC     USING L_FSC.
  CLEAR: ZTPP_WOSUM.
  ZTPP_WOSUM-WO_SER    = PA_MATERIAL(9) .
  ZTPP_WOSUM-NATION    = PA_MATERIAL+9(3).
  ZTPP_WOSUM-DEALER    = PA_MATERIAL+12(2).
  ZTPP_WOSUM-EXTC      = PA_MATERIAL+14(2).
  ZTPP_WOSUM-INTC      = PA_MATERIAL+16(2).
  ZTPP_WOSUM-INITQTY   = IT_DATA-INITQTY  .
  ZTPP_WOSUM-MODQTY    = IT_DATA-MODQTY   .
  ZTPP_WOSUM-WOCREDATE = IT_DATA-CRT_DATE .
  ZTPP_WOSUM-WOMODDATE = IT_DATA-CHG_DATE .
  ZTPP_WOSUM-VERSION   = IT_DATA-VERS     .
  ZTPP_WOSUM-FSC       = L_FSC            .
  ZTPP_WOSUM-ERDAT     = SY-DATUM         .
  ZTPP_WOSUM-ERZET     = SY-UZEIT         .
  ZTPP_WOSUM-ERNAM     = SY-UNAME         .
  ZTPP_WOSUM-AEDAT     = SY-DATUM         .
  ZTPP_WOSUM-AEZET     = SY-UZEIT         .
  ZTPP_WOSUM-AENAM     = SY-UNAME         .
  INSERT ZTPP_WOSUM .

  IF SY-SUBRC NE 0.
    " Step 8: Summary Table Insert Error..
    PERFORM CREATE_LOG USING 'E' 8 TEXT-005 ZTPP_WOSUM.
    PERFORM CREATE_LOG USING 'E' 8 TEXT-005 IT_DATA   .

    DATA : LV_ERROR(100) .
    CONCATENATE '[WOSUM]' TEXT-005 INTO LV_ERROR.
    PERFORM UPDATE_STATUS_TABLE
        USING 'E'
              LV_ERROR
              IT_DATA.
  ENDIF.
ENDFORM.                    " INSERT_WOSUM

*&---------------------------------------------------------------------*
*&      Form  BDC_WOHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM BDC_WOHD USING    PA_MATERIAL PA_MODEL.
  DATA: L_TABLES          LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
        L_VIN(17)         TYPE C ,
        L_RETURN(3)       TYPE C ,
        L_MATERIAL        LIKE MARA-MATNR,
        L_ATNAM           LIKE AUSP-ATWRT,
        L_FLAG            LIKE ZTPP_PMT07JB-ZRESULT.


  CALL FUNCTION 'Z_FPP_HANDLE_CLASSIFICATION'
       EXPORTING
            MATNR       = PA_MATERIAL
            MTYPE       = 'H'
       IMPORTING
            RETURN_FLAG = L_FLAG
       TABLES
            VAL_TABLE   = L_TABLES.

  IF L_FLAG   = 'S' .
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_123'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      l_vin = '---'.
*    ELSE.
*      l_vin = l_return.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_4'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_5'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_6'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_7'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_8'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '--'              INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return  '-'     INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_10'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_11'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.

    CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
           INTO L_MATERIAL .
*    PERFORM call_bdc_wohd_vin  USING l_material  l_vin.
  ELSE.
    CASE L_FLAG.
      WHEN 'L'.     " Instance not found...
        " Step 9: ALC Code --> Work Order Instance Not found!!
        PERFORM CREATE_LOG USING 'E' 9  TEXT-008 IT_DATA   .
      WHEN 'E'.     " Failure of the Classification Change...
        " Step 10: ALC Code --> Classification Table Update Error..
        PERFORM CREATE_LOG USING 'E' 10 TEXT-009 IT_DATA   .
      WHEN 'V'.     " Failure of the generation of vin spec
        PERFORM CREATE_LOG USING 'V' 10 TEXT-015 IT_DATA   .

    ENDCASE.
  ENDIF.
ENDFORM.                    " BDC_WOHD

*&---------------------------------------------------------------------*
*&      Form  BDC_WOCL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM BDC_WOCL USING    PA_MATERIAL.
  DATA: L_FLAG                LIKE ZTPP_PMT07JB-ZRESULT.

  CALL FUNCTION 'Z_FPP_HANDLE_CLASSIFICATION'
       EXPORTING
            MATNR       = PA_MATERIAL
            MTYPE       = 'C'
       IMPORTING
            RETURN_FLAG = L_FLAG.

  CASE L_FLAG.
    WHEN 'L'.     " Instance not found...
      " Step 9: ALC Code --> Work Order Instance Not found!!
      PERFORM CREATE_LOG USING 'E' 9  TEXT-008 IT_DATA   .
    WHEN 'E'.     " Failure of the Classification Change...
      " Step 10: ALC Code --> Classification Table Update Error..
      PERFORM CREATE_LOG USING 'E' 10 TEXT-009 IT_DATA   .
  ENDCASE.
ENDFORM.                    " BDC_WOCL

*&---------------------------------------------------------------------*
*&      Form  UPDATE_LASTDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_LASTDATE.
  SELECT SINGLE *
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = SY-REPID
     AND KEY2 = '******************'
     AND KEY3 = '******************' .

  ZTPP_COMMON_VALS-DATES = WA_WKDATE .
  ZTPP_COMMON_VALS-DESCRIPTION = 'VIN Last Serial-No.' .
  ZTPP_COMMON_VALS-UDATE = SY-DATUM  .
  ZTPP_COMMON_VALS-UTIME = SY-UZEIT  .
  MODIFY ZTPP_COMMON_VALS .
ENDFORM.                    " UPDATE_LASTDATE

*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_WOHD_VIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_L_VIN  text
*----------------------------------------------------------------------*
FORM CALL_BDC_WOHD_VIN USING    PA_MATERIAL   PA_VIN.
  DATA: L_VARIABLE       LIKE TABLE OF ZSPP_VIN_VALUE  WITH HEADER LINE.

  CLEAR: L_VARIABLE, L_VARIABLE[],
         IT_BDCDATA, IT_BDCDATA[], IT_MSG,  IT_MSG[].

  L_VARIABLE-ATNAM = 'P_VIN_SPEC'     .
  L_VARIABLE-ATWRT =  PA_VIN          .     APPEND L_VARIABLE .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = PA_MATERIAL
            MODE         = 'W'
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = L_VARIABLE
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC = 0.
    LOOP AT L_VARIABLE WHERE ZFLAG = 'E' .
      WA_ERROR = 'X' .
    ENDLOOP.
  ELSE.
    WA_ERROR = 'X' .
  ENDIF.
ENDFORM.                    " CALL_BDC_WOHD_VIN

*&---------------------------------------------------------------------*
*&      Form  concate_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_FSC  text
*----------------------------------------------------------------------*
FORM CONCATE_FSC USING    PA_FSC.
  DATA: L_LEN TYPE I,
        L_NEW_DEALER(1).

  CLEAR: PA_FSC .
  L_LEN = STRLEN( IT_DATA-BMDL ).
** Changed by Furong on 10/09/07 for EBOM
*   CONCATENATE IT_DATA-MOYE IT_DATA-NATION IT_DATA-DEALER IT_DATA-BMDL
*           INTO PA_FSC .
*    CONCATENATE PA_FSC IT_DATA-OCNN INTO PA_FSC SEPARATED BY SPACE .
  IF L_LEN = 7.
    CONCATENATE IT_DATA-MOYE IT_DATA-NATION IT_DATA-DEALER IT_DATA-BMDL
           INTO PA_FSC .
    CONCATENATE PA_FSC IT_DATA-OCNN INTO PA_FSC SEPARATED BY SPACE .
  ELSE.
    CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
         EXPORTING
              OLD_DEALER = IT_DATA-DEALER
         IMPORTING
              NEW_DEALER = L_NEW_DEALER.
    CONCATENATE IT_DATA-MOYE IT_DATA-NATION L_NEW_DEALER IT_DATA-BMDL
             INTO PA_FSC .
    CONCATENATE PA_FSC IT_DATA-OCNN INTO PA_FSC.
  ENDIF.
** end of change
ENDFORM.                    " concate_fsc

*&---------------------------------------------------------------------*
*&      Form  CHECK_BDC_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0420   text
*      -->P_0421   text
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM CHECK_BDC_RESULT USING    PA_MODE  PA_TYPE  PA_MATERIAL.
  DATA: L_MSG(100)           TYPE C.

  IF WA_ERROR NE ' '     .
    CLEAR: IT_MSG, IT_MSG[].
    EXIT .
  ENDIF.

  IT_DATA-P_FLAG = 'X' .

  READ TABLE IT_MSG WITH KEY MSGTYP = 'E' .

  CHECK SY-SUBRC = 0.
  WA_ERROR = 'E'    .
  CLEAR: IT_DATA-P_FLAG.

  LOOP AT IT_MSG WHERE MSGTYP = 'E' .
    PERFORM CREATE_MESSAGE USING L_MSG  IT_MSG-MSGID IT_MSG-MSGNR
                    IT_MSG-MSGV1 IT_MSG-MSGV2 IT_MSG-MSGV3 IT_MSG-MSGV4.
    " Step 6: Material Master Creation Error for the BDC
** Changed by Furong Wang on 02/01/08
*    PERFORM CREATE_LOG USING 'E' 6 L_MSG    IT_DATA.
    PERFORM CREATE_LOG USING 'E' 6 L_MSG    L_MSG.
** end of change
    PERFORM CREATE_LOG USING 'R' 6 L_MSG    IT_DATA.
    PERFORM CREATE_LOG USING 'R' 6 L_MSG    IT_DATA-S219.
  ENDLOOP.

  DATA  : LV_ERROR(100).

  CONCATENATE  '[BDC Error]' L_MSG
               PA_MATERIAL
               IT_DATA-BMDL
              INTO LV_ERROR.

  PERFORM UPDATE_STATUS_TABLE
      USING 'E'
            LV_ERROR
            IT_DATA.
  CLEAR: IT_MSG, IT_MSG[].
ENDFORM.                    " CHECK_BDC_RESULT

*&---------------------------------------------------------------------*
*&      Form  create_classification_mm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM CREATE_CLASSIFICATION_MM USING    PA_MATERIAL  PA_WO  PA_MODEL.
  DATA: L_VARIABLE           LIKE TABLE OF ZSPP_VIN_VALUE
                                                       WITH HEADER LINE,
        L_PERF               TYPE C,
        L_NO(3)              TYPE N,
        L_OF(3)              TYPE N,
        L_NAME(30)           TYPE C.

  PERFORM CHECK_PERF_ALC           USING PA_MATERIAL  L_PERF  PA_MODEL.

  L_VARIABLE-ATNAM = 'P_INIT_QTY'       .
  L_VARIABLE-ATWRT =  IT_DATA-INITQTY   .   APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_MOD_QTY'        .
  L_VARIABLE-ATWRT =  IT_DATA-MODQTY    .   APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_WO_SER'         .
  L_VARIABLE-ATWRT =  IT_DATA-WO_SER    .   APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_NATION'         .
  L_VARIABLE-ATWRT =  IT_DATA-NATION    .   APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_DEALER'         .
  L_VARIABLE-ATWRT =  IT_DATA-DEALER    .   APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_GEN_DATE'       .
  L_VARIABLE-ATWRT =  SY-DATUM          .   APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_MOD_DATE'       .
  L_VARIABLE-ATWRT =  SY-DATUM          .   APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_DESTINATION_CODE'.
  L_VARIABLE-ATWRT =  IT_DATA-DEST      .   APPEND L_VARIABLE .

  IF IT_DATA-WO = 'C' .
    L_VARIABLE-ATNAM = 'COLOREXT'       .
    L_VARIABLE-ATWRT =  IT_DATA-EXTC    .     APPEND L_VARIABLE .
    L_VARIABLE-ATNAM = 'COLORINT'       .
    L_VARIABLE-ATWRT =  IT_DATA-INTC    .     APPEND L_VARIABLE .
    L_VARIABLE-ATNAM = 'P_ORDER_ZONE'   .
    L_VARIABLE-ATWRT =  IT_DATA-ORZN    .     APPEND L_VARIABLE .
    L_VARIABLE-ATNAM = 'P_COLOR_SER'    .
    L_VARIABLE-ATWRT =  IT_DATA-CLSR    .     APPEND L_VARIABLE .
    L_VARIABLE-ATNAM = 'P_REGION_PORT'  .
    L_VARIABLE-ATWRT =  IT_DATA-REGN    .     APPEND L_VARIABLE .
    L_VARIABLE-ATNAM = 'P_FLEET'        .
    L_VARIABLE-ATWRT =  IT_DATA-FLET    .     APPEND L_VARIABLE .
    L_VARIABLE-ATNAM = 'P_MANUAL_ORDER' .
    L_VARIABLE-ATWRT =  IT_DATA-MAOR    .     APPEND L_VARIABLE .
  ELSE.
    L_VARIABLE-ATNAM = 'P_LC_NO'        .
    L_VARIABLE-ATWRT =  IT_DATA-LCNO    .     APPEND L_VARIABLE .
    L_VARIABLE-ATNAM = 'P_LC_COUNT'     .
    L_VARIABLE-ATWRT =  IT_DATA-LCNT    .     APPEND L_VARIABLE .
    L_VARIABLE-ATNAM = 'P_REGION_PORT'  .
    L_VARIABLE-ATWRT =  IT_DATA-REGN    .     APPEND L_VARIABLE .
  ENDIF.

  L_VARIABLE-ATNAM = 'P_MODEL_YEAR'   .
  L_VARIABLE-ATWRT =  IT_DATA-MOYE    .     APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_MI'           .
  L_VARIABLE-ATWRT =  IT_DATA-BMDL    .     APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_OCN'          .
  L_VARIABLE-ATWRT =  IT_DATA-OCNN    .     APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_VERSION'      .
  L_VARIABLE-ATWRT =  IT_DATA-VERS    .     APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_MODEL'        .
  L_VARIABLE-ATWRT =  PA_MODEL        .     APPEND L_VARIABLE .
*  l_variable-atwrt =  it_data-bmdl(3) .     APPEND l_variable .
*  l_variable-atnam = 'P_MI'           .
*  l_variable-atwrt =  it_data-bmdl    .     APPEND l_variable .
  L_VARIABLE-ATNAM = 'P_WO_MODI_DATE' .
  L_VARIABLE-ATWRT =  IT_DATA-CHG_DATE .     APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_WO_CREATE_DATE'.
  L_VARIABLE-ATWRT =  IT_DATA-CRT_DATE .     APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_PLAN_QTY'    .
  L_VARIABLE-ATWRT =  '0'            .       APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'P_FORECAST_QTY'.
  L_VARIABLE-ATWRT =  '0'            .       APPEND L_VARIABLE .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = PA_MATERIAL
            MODE         = 'W'
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = L_VARIABLE
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC = 0.
    LOOP AT L_VARIABLE WHERE ZFLAG = 'E' .
      " Step 4: Material's Characteristic Value UPDATE Error...
      PERFORM CREATE_LOG USING 'E' 4 TEXT-010 IT_DATA.
      PERFORM CREATE_LOG USING 'E' 4 TEXT-010 L_VARIABLE.

      DATA : LV_ERROR(200).
      CONCATENATE '[CLASS]' 'Create Classification MM : '
                  PA_MATERIAL INTO LV_ERROR.
      PERFORM UPDATE_STATUS_TABLE
                  USING 'E'
                        LV_ERROR
                        IT_DATA.

      WA_ERROR = 'X' .
    ENDLOOP.
  ENDIF.
ENDFORM.                    " create_classification_mm

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_SPACE_TO_HYPEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_S219  text
*----------------------------------------------------------------------*
FORM CONVERSION_SPACE_TO_HYPEN USING    PA_S219.
  DO 219 TIMES.
    REPLACE ' '  WITH '-' INTO PA_S219 .
    IF SY-SUBRC = 4 .
      EXIT .
    ENDIF.
  ENDDO.
ENDFORM.                    " CONVERSION_SPACE_TO_HYPEN

*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_TXT_001  text
*----------------------------------------------------------------------*
FORM CREATE_LOG USING    PA_TYPE  PA_STEP  PA_TEXT  PA_KEY .
  DATA: L_SEQ                LIKE ZTPP_PP_LOG_DETA-SEQUENCE.
  SELECT MAX( SEQUENCE ) INTO L_SEQ
    FROM ZTPP_PP_LOG_DETA
   WHERE LOGKEY = P_LOG   .

  L_SEQ = L_SEQ + 1.
  " Log Detail Creation
  ZTPP_PP_LOG_DETA-LOGKEY   = P_LOG        .
  ZTPP_PP_LOG_DETA-SEQUENCE = L_SEQ      .
  ZTPP_PP_LOG_DETA-LOGTYPE  = PA_TYPE     .
  ZTPP_PP_LOG_DETA-LOGSTEP  = PA_STEP     .
  ZTPP_PP_LOG_DETA-KEYDATA  = PA_KEY      .
  INSERT INTO ZTPP_PP_LOG_DETA VALUES ZTPP_PP_LOG_DETA .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  GET_LOGSERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LOGSERIAL.
  " Log Head Creation..
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'ZLOG'
       IMPORTING
            NUMBER                  = WA_NUMBER
       EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.
ENDFORM.                    " GET_LOGSERIAL

*&---------------------------------------------------------------------*
*&      Form  create_steplog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_STEPLOG.
  IF WA_ERROR = SPACE.
    " Step 11: Successfully creation...
    PERFORM CREATE_LOG USING 'S' 11 TEXT-011  IT_DATA.
  ELSE.
    " Step 11: Error creation...
    PERFORM CREATE_LOG USING 'E' 11 TEXT-012  IT_DATA.
  ENDIF.
ENDFORM.                    " create_steplog

*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DATA.
  DATA: L_MODSUM             LIKE ZTPP_KSBOHMM-MODQTY,
        L_INITSUM            LIKE ZTPP_KSBOHMM-INITQTY,
        L_HQTY               LIKE ZTPP_KSBOHMM-INITQTY,  " Head Qty
        L_CQTY               LIKE ZTPP_KSBOHMM-INITQTY,  " Color Qty
        L_RQTY               LIKE ZTPP_KSBOHMM-INITQTY,  " Remain Qty
        L_DATA               LIKE TABLE OF IT_DATA WITH HEADER LINE,
        L_MATERIAL           LIKE MARA-MATNR          ,
        L_ERROR              TYPE C       ,
        L_CM(15)             TYPE C       ,
        L_EXIST              LIKE SY-SUBRC.

  " Check the Sequence Quantity for the Work Order....
  SORT IT_DATA BY WO_SER NATION DEALER EXTC INTC .
  L_DATA[] = IT_DATA[].
*  DELETE l_data WHERE extc = '***' .

  LOOP AT IT_DATA.
    PERFORM CHECK_COLOR      USING IT_DATA-H_C.
    CASE  IT_DATA-H_C .
      WHEN 'Y'    .         " WORK ORDER HEADER.
        " Check the Result about the Previous Work order....
        IF L_HQTY = 0 .
          " Normal!!
        ELSE.
          " Error --> Unmatched the Sequence qty...
          PERFORM CREATE_LOG USING 'E' 13 TEXT-014  L_MATERIAL.
          LOOP AT L_DATA WHERE WO_SER = L_MATERIAL(9)    AND
                               NATION = L_MATERIAL+9(3)  AND
                               DEALER = L_MATERIAL+12(2) AND
                               EXTC   NE '***'           .
            PERFORM CREATE_LOG USING 'R' 13 TEXT-014  L_DATA.
            DELETE L_DATA.
          ENDLOOP.
          DELETE IT_DATA WHERE WO_SER = L_MATERIAL(9)   AND
                               NATION = L_MATERIAL+9(3) AND
                               DEALER = L_MATERIAL+12(2) .
        ENDIF.
        CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
               INTO IT_DATA-MATERIAL .
        IT_DATA-WO = 'H' .
        PERFORM CHECK_MATERIAL USING IT_DATA-EXIST  IT_DATA-MATERIAL .
        CASE IT_DATA-EXIST.
          WHEN 'Y'    .         "  EXIST
            " Check between the Old-Data(Header/Color) and the New-Data.
            CLEAR: L_ERROR.
            PERFORM CHECK_WORKORDER  USING L_ERROR.
            IF L_ERROR = 'X'.
              PERFORM CREATE_LOG USING 'E' 13 TEXT-014 IT_DATA-MATERIAL.
              LOOP AT L_DATA WHERE WO_SER = IT_DATA-MATERIAL(9)    AND
                                   NATION = IT_DATA-MATERIAL+9(3)  AND
                                   DEALER = IT_DATA-MATERIAL+12(2) .
                MOVE-CORRESPONDING L_DATA   TO  IT_ERROR           .
                APPEND IT_ERROR    .
              ENDLOOP.
              LOOP AT L_DATA WHERE WO_SER = IT_DATA-MATERIAL(9)    AND
                                   NATION = IT_DATA-MATERIAL+9(3)  AND
                                   DEALER = IT_DATA-MATERIAL+12(2) AND
                                   EXTC   NE '***'           .
                PERFORM CREATE_LOG USING 'R' 13 TEXT-014  L_DATA.
                DELETE L_DATA.
              ENDLOOP.
              DELETE IT_DATA WHERE WO_SER = IT_DATA-MATERIAL(9)   AND
                                   NATION = IT_DATA-MATERIAL+9(3) AND
                                   DEALER = IT_DATA-MATERIAL+12(2) .
            ENDIF.
*            l_hqty = it_data-modqty.
          WHEN 'N'    .         "  NO DATA
            L_HQTY = IT_DATA-MODQTY.
        ENDCASE.
      WHEN 'N'    .         " WORK ORDER COLOR .
        CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
                    IT_DATA-EXTC   IT_DATA-INTC   INTO IT_DATA-MATERIAL.
        IT_DATA-WO = 'C' .
        PERFORM CHECK_MATERIAL USING IT_DATA-EXIST  IT_DATA-MATERIAL .
        CASE IT_DATA-EXIST.
*          WHEN 'Y'    .         "  EXIST
          WHEN 'N'    .         "  NO DATA
            L_HQTY = L_HQTY - IT_DATA-MODQTY .
        ENDCASE.
    ENDCASE.
    CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
           INTO L_MATERIAL .
    CHECK L_ERROR = SPACE  .
    MODIFY IT_DATA.
  ENDLOOP.
ENDFORM.                    " check_data

*&---------------------------------------------------------------------*
*&      Form  check_workorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_WORKORDER   USING PA_FLAG.
  DATA: L_WORDER                 LIKE TABLE OF IT_DATA WITH HEADER LINE,
        L_CQTY                   LIKE ZTPP_KSBOHMM-INITQTY,  " Color Qty
        L_CONF                   LIKE TABLE OF ZSPP_VIN_VALUE
                                                 WITH HEADER LINE.

  L_WORDER[] = IT_DATA[].
  CLEAR: L_CONF, L_CONF[].
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_MOD_QTY'
       IMPORTING
            OUTPUT = L_CONF-ATINN.
  APPEND L_CONF.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_SEQ_QTY'
       IMPORTING
            OUTPUT = L_CONF-ATINN.
  APPEND L_CONF.

  LOOP AT L_WORDER WHERE WO_SER = IT_DATA-WO_SER  AND
                         NATION = IT_DATA-NATION  AND
                         DEALER = IT_DATA-DEALER  AND
                         EXTC   NE '***'          .
    CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
                IT_DATA-EXTC   IT_DATA-INTC   INTO L_WORDER-MATERIAL.


    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = L_WORDER-MATERIAL
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = L_CONF
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    " Compare the Qty....
    READ TABLE L_CONF WITH KEY ATNAM = 'P_SEQ_QTY' .
    L_CQTY = L_CONF-ATWRT.
    IF L_CQTY > L_WORDER-MODQTY.
      " Error.....
      PA_FLAG = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_workorder

*&---------------------------------------------------------------------*
*&      Form  check_perf_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_L_PERF  text
*----------------------------------------------------------------------*
FORM CHECK_PERF_ALC USING    PA_MATERIAL  PA_PERF  PA_MODEL .
  DATA: L_STR(10)            TYPE C         ,
        L_KNOBJ              LIKE CUCO-KNOBJ,
        L_KNNUM              LIKE CUOB-KNOBJ,
        L_KNNAM              LIKE CUKB-KNNAM.

  CLEAR: IT_ALC, IT_ALC[].

  CONCATENATE PA_MODEL '_WOHD'           INTO  L_KNNAM.
  PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
  PERFORM GET_KNNUM USING L_KNOBJ.
  CONCATENATE PA_MODEL '_WOCL'           INTO  L_KNNAM.
  PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
  PERFORM GET_KNNUM                      USING L_KNOBJ.

  LOOP AT IT_ALC.
    SELECT SINGLE B~KNNAM T~KNKTX
      INTO CORRESPONDING FIELDS OF IT_ALC
      FROM CUKB AS B INNER JOIN CUKBT AS T
        ON B~KNNUM = T~KNNUM
     WHERE B~KNNUM = IT_ALC-KNNUM
       AND T~SPRAS = SY-LANGU   .

    CONCATENATE 'D_' PA_MODEL '_ALC_'   INTO L_STR.
    IF IT_ALC-KNNAM(10) NE L_STR        .
      DELETE IT_ALC .
      CONTINUE .
    ENDIF.
    IT_ALC-CODE     = IT_ALC-KNNAM+12(3) .
    IT_ALC-TYPE_ALC = IT_ALC-KNNAM+10(1) .
    IT_ALC-RP       = IT_ALC-KNKTX(2)    .
    CONCATENATE 'P' IT_ALC-KNNAM+5(10)  INTO IT_ALC-CHAR_ALC .
    MODIFY IT_ALC .
  ENDLOOP.
  SORT IT_ALC BY KNNUM RP CODE .
ENDFORM.                    " check_perf_alc

*&---------------------------------------------------------------------*
*&      Form  get_knobj
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_KNOBJ USING    PA_KNNAM  PA_KNOBJ.
  SELECT SINGLE KNOBJ INTO PA_KNOBJ
    FROM CUCO
   WHERE OBTAB = 'MARA'
     AND OBJEK = PA_KNNAM .
ENDFORM.                    " GET_KNOBJ

*&---------------------------------------------------------------------*
*&      Form  get_KNNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_KNNUM USING    PA_KNOBJ.
  SELECT KNNUM APPENDING CORRESPONDING FIELDS OF TABLE IT_ALC
    FROM CUOB
   WHERE KNOBJ = PA_KNOBJ.
ENDFORM.                    " get_KNNUM
*&---------------------------------------------------------------------*
*&      Form  UPDATE_STATUS_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0482   text
*      -->P_0483   text
*      -->P_IT_DATA  text
*----------------------------------------------------------------------*
FORM UPDATE_STATUS_TABLE USING    P_STATUS
                                  P_MESSAGE
                                  P_DATA STRUCTURE IT_DATA.

  DATA :LV_DOCNUM LIKE EDIDC-DOCNUM .
  CLEAR : LV_DOCNUM.
  LV_DOCNUM = P_DATA-ZMSG.

  UPDATE ZTPP_PO_IDOC SET
    STATUS  = P_STATUS
    MESSAGE = P_MESSAGE
    ERDAT  = SY-DATUM
    ERTIM  = SY-UZEIT
    UNAME  = SY-UNAME
    WHERE DOCNUM = LV_DOCNUM
      AND WO_SER = P_DATA-WO_SER
      AND NATN  =  P_DATA-NATION
      AND DIST  =  P_DATA-DEALER
      AND WKEXC =  P_DATA-EXTC
      AND WKINC =  P_DATA-INTC
      AND ( STATUS = '' OR STATUS = 'E' )

.

ENDFORM.                    " UPDATE_STATUS_TABLE
*&---------------------------------------------------------------------*
*&      Form  CHECK_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_PO.
  DATA : LT_MARA LIKE TABLE OF MARA WITH HEADER LINE,
         LT_WOSUM LIKE TABLE OF ZTPP_WOSUM WITH HEADER LINE.
  DATA : LV_DOCNUM LIKE EDIDC-DOCNUM,
         LV_ERROR LIKE BAPIRETURN-MESSAGE.

  SORT IT_DATA BY WO_SER EXTC INTC MATERIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_WOSUM
   FROM ZTPP_WOSUM
   FOR aLL ENTRIES IN IT_DATA
   WHERE WO_SER = IT_DATA-WO_SER
           AND NATION = IT_DATA-NATION
           AND DEALER = IT_DATA-DEALER
           AND EXTC   = IT_DATA-EXTC
           AND INTC   = IT_DATA-INTC.

  SORT : LT_MARA  BY MATNR,
         LT_WOSUM BY WO_SER NATION DEALER EXTC INTC.

  LOOP AT IT_DATA WHERE EXTC NE '***'.

    CLEAR : LV_ERROR.
    READ TABLE LT_WOSUM WITH KEY WO_SER = IT_DATA-WO_SER
                                 NATION = IT_DATA-NATION
                                 DEALER = IT_DATA-DEALER
                                 EXTC   = IT_DATA-EXTC
                                 INTC   = IT_DATA-INTC .
    IF SY-SUBRC <> 0 .
      CONCATENATE '[WOSUM] '
                  IT_DATA-WO_SER IT_DATA-NATION
                  IT_DATA-DEALER IT_DATA-EXTC
                  SPACE ' is not exist' INTO LV_ERROR.
      CONTINUE.
    ELSE.
      IF LT_WOSUM-MODQTY NE IT_DATA-MODQTY .

        CONCATENATE '[WOSUM] '
                    IT_DATA-WO_SER IT_DATA-NATION
                    IT_DATA-DEALER IT_DATA-EXTC
                   SPACE  ' Quantity is not the same' INTO LV_ERROR.
        CONTINUE.
      ENDIF.
    ENDIF.
    IF NOT LV_ERROR IS INITIAL.
      PERFORM UPDATE_STATUS_TABLE
      USING  'E'
          LV_ERROR
          IT_DATA .
    ELSE.
      PERFORM UPDATE_STATUS_TABLE
      USING  'S'
             'ENDPO'
          IT_DATA .
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHECK_PO
