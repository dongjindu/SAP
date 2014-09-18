************************************************************************
* Program name : ZEMMPM23E_WELCOME
* Created by   : Min-su Park
* Created on   : 2003.11.02.
* Pattern      :
* Description  :
*   1. Modified Welcome screen-For Inbound delivery-Putaway process    *
*   2. Empty Container management                                      *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.02.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
* 03/17/2005      Shiva            UD1K915019     Don't select and
*                                                 insert bin status in
*                                            the table "ZTMM_CONTAINER".
* 11/30/2006     Haseeb Mohammad   UD1K922620
************************************************************************

REPORT ZEMMPM23E_WELCOME MESSAGE-ID ZMMM.

TABLES : LECI_TRA_DYN, ZTMM_CONTAINER, ZTMM_CT_ERRLOG.

TABLES: ZTBL, MAKT, A902, KONP, MARC, EKPO, T604T.

*Inbound Delivery
DATA : IT_LIKPS  LIKE LIKP OCCURS 0 WITH HEADER LINE .
DATA : BEGIN OF IT_LIPS OCCURS 0.
        INCLUDE STRUCTURE LIPS.
DATA : END OF IT_LIPS.

*Mode
DATA   : W_MODE   VALUE 'C'  ,
         W_STATUS VALUE 'N'  .

*Bdc_Data
DATA : BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF IT_BDC.
DATA : BEGIN OF IT_MESSAGE OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF IT_MESSAGE.

*General Variable
DATA : W_NLTYP     LIKE LTAP-NLTYP, "Storage type
       W_NLBER     LIKE LTAP-NLBER, "Storage section
       W_NLPLA     LIKE LTAP-NLPLA. "Storage bin
DATA : W_PASS_NUMB LIKE LECI_EVENT-PASS_NUMB. "Serial No

*BAPI variable
DATA: WA_GOODSMVT_HEADER   LIKE BAPI2017_GM_HEAD_01.
DATA: WA_GOODSMVT_CODE     LIKE BAPI2017_GM_CODE.
DATA: WA_GOODSMVT_HEADRET  LIKE BAPI2017_GM_HEAD_RET.
DATA: IT_GOODSMVT_ITEM     LIKE TABLE OF BAPI2017_GM_ITEM_CREATE.
DATA: WA_GOODSMVT_ITEM  LIKE LINE OF IT_GOODSMVT_ITEM.
DATA: IT_BAPIRET2       LIKE TABLE OF BAPIRET2.
DATA: WA_BAPIRET2       LIKE LINE OF IT_BAPIRET2.


**--- insert by stlim (2004/04/26)
DATA : W_BUTTON_CLICK_TIME TYPE T,
       W_BUTTON_CLICK_DATE TYPE D,
       EX_SUBRC LIKE SY-SUBRC,
       W_VBELN LIKE LIKP-VBELN.
**--- end of insert
DATA ZERR_FLAG TYPE C .
DATA: ZERRKEY LIKE INDX-SRTFD VALUE 'ZWME04KEY'.

*Parameters
PARAMETERS : P_NUMB1   LIKE LECI_TRA_DYN-CONT_REG_NUMB1    ,
             P_VHE     LIKE LECI_TRA_DYN-VEHICLE_REG_NUMB  ,
             P_PDATE   LIKE LECI_TRA_DYN-PASS_DATE         ,
             P_PTIME   LIKE LECI_TRA_DYN-PASS_TIME         ,
             P_NDRV    LIKE LECI_TRA_DYN-NAME_DRVR         ,
             P_WHGATE  LIKE LECI_TRA_DYN-WHS_GATE          ,
             P_PTXT    LIKE LECI_TRA_DYN-PARKING_TXT       ,
             P_NLTYP   LIKE LTAP-NLTYP ,
             P_NLBER   LIKE LTAP-NLBER ,
             P_NLPLA   LIKE LTAP-NLPLA .

**--- insert by stlim (2004/04/26)
PARAMETERS : P_DATE TYPE D,
             P_TIME TYPE T.
**--- end of insert

START-OF-SELECTION.

  PERFORM CREATE_INBOUND_TO_GR.
  PERFORM LECI_SAVE.
  PERFORM PRINT.





*&---------------------------------------------------------------------*
*&      Form  CREATE_INBOUND_TO_GR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_INBOUND_TO_GR.
  DATA : CONTAIN(20)                                   .
  CLEAR ZERR_FLAG.
*  free memory id  zerrkey.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = P_NUMB1
       IMPORTING
            OUTPUT = CONTAIN.

*Find MsntrnspId which is same with Container No.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_LIKPS
         FROM LIKP
        WHERE LFART = 'EL'
*          AND traty = '0005'
          AND TRAID = CONTAIN.

  CASE SY-SUBRC.
    WHEN 0.
      LOOP AT IT_LIKPS.
**A__ BY Paul
*        PERFORM create_to_from_inbound .
*2003.12.12
*I used bdc for gr because when use bapi, problem which document flow
*is not displayed in Inbound Delivy.
        PERFORM GR_BDC USING IT_LIKPS.
*       PERFORM GR USING IT_LIKPS.
*2003.12.12
      ENDLOOP.
    WHEN OTHERS.
      MESSAGE E005.
  ENDCASE.
ENDFORM.                    " CREATE_INBOUND_TO
*&---------------------------------------------------------------------*
*&      Form  LECI_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LECI_SAVE.
  DATA    : DATE(10).
  DATA    : VIECH(20), CONTAIN(20).
  CLEAR   : IT_BDC, IT_MESSAGE.
  REFRESH : IT_BDC, IT_MESSAGE.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = P_VHE
       IMPORTING
            OUTPUT = VIECH.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = P_NUMB1
       IMPORTING
            OUTPUT = CONTAIN.

  WRITE : P_PDATE TO DATE.
  PERFORM BDC_PASS USING:
    'X' 'SAPRLECHKIN' '1000'                            ,
    ' ' 'LECI_CHKPT_DYN-CHKIN_POINT' 'GATE 1'           ,
*Check in
    ' ' 'LECI_TRA_DYN-PASS_DATE' DATE                   ,
*Check in - Time
    ' ' 'LECI_TRA_DYN-PASS_TIME' P_PTIME+0(4),
*Truck license
    ' ' 'LECI_TRA_DYN-VEHICLE_REG_NUMB' VIECH           ,
*1st container number
    ' ' 'LECI_TRA_DYN-CONT_REG_NUMB1' CONTAIN           ,
*Deliver last name
    ' ' 'LECI_TRA_DYN-NAME_DRVR' P_NDRV ,
*Door for whse
    ' ' 'LECI_TRA_DYN-WHS_GATE'  P_WHGATE  ,
*PrkngSpce
    ' ' 'LECI_TRA_DYN-PARKING_TXT' P_PTXT,
    ' ' 'BDC_OKCODE' '=SAVE'.

  CALL TRANSACTION 'LECI'
           USING IT_BDC
           MODE W_STATUS
           UPDATE'S'
           MESSAGES INTO IT_MESSAGE.
  IF SY-SUBRC <> 0.
    READ TABLE IT_MESSAGE WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      PERFORM LECI_ERROR_LOG.
    ENDIF.
  ELSE.
*    if zerr_flag <> 'E'.
    PERFORM CREATE_ZTMM_CONTAINER_CT_YARD.
    W_MODE = 'E'.
*    endif.
  ENDIF.

ENDFORM.                    " LECI_SAVE
*&---------------------------------------------------------------------*
*&      Form  CREATE_TO_FROM_INBOUND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIKPS  text
*----------------------------------------------------------------------*
FORM CREATE_TO_FROM_INBOUND .

  DATA : NO TYPE I     .

*[ 0 ] . Get Inbound Items
  SELECT * FROM LIPS
           INTO CORRESPONDING FIELDS OF TABLE IT_LIPS
          WHERE VBELN = IT_LIKPS-VBELN.
  CHECK SY-SUBRC = 0.
  DESCRIBE TABLE IT_LIPS LINES NO.

*[ 1 ] . Create TO
  CLEAR   : IT_BDC, IT_MESSAGE.
  REFRESH : IT_BDC, IT_MESSAGE.

  PERFORM BDC_PASS USING:
     'X' 'SAPML03T'    '0151'      ,
     ' ' 'LTAK-LGNUM'  'P01'       ,
     ' ' 'VBLKK-VBELN' IT_LIKPS-VBELN,
     ' ' 'BDC_OKCODE'  '/00'       .

  CASE NO.
    WHEN 1.
*[ 1 ] - 1. TO Creation from one-Item
      LOOP AT IT_LIPS.
        IF SY-TABIX > 1.
          PERFORM BDC_PASS USING:
            'X' 'SAPML03T'    '0104'     ,
            ' ' 'BDC_OKCODE'  '=TATB'    .
        ENDIF.
        PERFORM BDC_PASS USING:
          'X' 'SAPML03T'    '0104'     ,
          ' ' 'RL03T-LETY2' 'BB'       ,
          ' ' 'BDC_OKCODE'  '/00'      .

        PERFORM BDC_PASS USING:
          'X' 'SAPML03T'    '0104'           ,
          ' ' 'LTAPE-NLENR(01)' IT_LIPS-KDMAT,
          ' ' 'BDC_OKCODE'  '/00'            .

        PERFORM BDC_PASS USING:
          'X' 'SAPML03T'    '0104'     ,
          ' ' 'BDC_OKCODE'  '=TAH1'    .
        PERFORM BDC_PASS USING:
          'X' 'SAPML03T'    '0102'     ,
          ' ' 'LTAP-NLTYP'  P_NLTYP    , "100
          ' ' 'LTAP-NLBER'  P_NLBER    , "001
          ' ' 'LTAP-NLPLA'  P_NLPLA    ,                    "AA - 02
**        ' ' 'LTAP-NLENR'  IT_LIPS-EAN11, "Storage Unit
*         ' ' 'LTAP-NLENR' IT_LIPS-KDMAT,
          ' ' 'BDC_OKCODE'  '/00'      .
      ENDLOOP.
      PERFORM BDC_PASS USING:
         'X' 'SAPML03T'    '0104'     ,
         ' ' 'BDC_OKCODE'  '=BU'      .

    WHEN OTHERS.
*[ 1 ] - 2. TO Creation from multi-Item
      PERFORM BDC_PASS USING:
         'X' 'SAPML03T'    '0154'      ,
         ' ' 'BDC_OKCODE'  '=MRKA'     . "Select All

      PERFORM BDC_PASS USING:
         'X' 'SAPML03T'    '0154'      ,
         ' ' 'BDC_OKCODE'  '=TPAL'     . "Palletization

      LOOP AT IT_LIPS.
        IF SY-TABIX > 1.
          PERFORM BDC_PASS USING:
            'X' 'SAPML03T'    '0104'     ,
            ' ' 'BDC_OKCODE'  '=TATB'    .
        ENDIF.
        PERFORM BDC_PASS USING:
          'X' 'SAPML03T'    '0104'     ,
          ' ' 'RL03T-LETY2' 'BB'       ,
          ' ' 'BDC_OKCODE'  '/00'      .

        PERFORM BDC_PASS USING:
          'X' 'SAPML03T'    '0104'           ,
          ' ' 'LTAPE-NLENR(01)' IT_LIPS-KDMAT,
          ' ' 'BDC_OKCODE'  '/00'            .

        PERFORM BDC_PASS USING:
          'X' 'SAPML03T'    '0104'     ,
          ' ' 'BDC_OKCODE'  '=TAH1'    .
        PERFORM BDC_PASS USING:
          'X' 'SAPML03T'    '0102'     ,
          ' ' 'LTAP-NLTYP'  P_NLTYP    , "100
          ' ' 'LTAP-NLBER'  P_NLBER    , "001
          ' ' 'LTAP-NLPLA'  P_NLPLA    ,                    "AA - 02
          ' ' 'BDC_OKCODE'  '/00'      .
      ENDLOOP.
      PERFORM BDC_PASS USING:
         'X' 'SAPML03T'    '0104'     ,
         ' ' 'BDC_OKCODE'  '=BU'      .
  ENDCASE.

  CALL TRANSACTION 'LT03'
              USING IT_BDC
              MODE W_STATUS
              UPDATE'S'
              MESSAGES INTO IT_MESSAGE.
  READ TABLE IT_MESSAGE WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC = 0.
    PERFORM TO_CREATION_ERROR.
  ELSE.
  ENDIF.
ENDFORM.                    " CREATE_TO_FROM_INBOUND
*&---------------------------------------------------------------------*
*&      Form  BDC_PASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0010   text
*      -->P_0011   text
*      -->P_0012   text
*----------------------------------------------------------------------*
FORM BDC_PASS USING PAR1 PAR2 PAR3.
  CLEAR IT_BDC.
  IF PAR1 = 'X'.
    IT_BDC-DYNBEGIN = 'X'.
    IT_BDC-PROGRAM  = PAR2.
    IT_BDC-DYNPRO   = PAR3.
    APPEND IT_BDC.
  ELSE.
    IT_BDC-FNAM = PAR2.
    IT_BDC-FVAL = PAR3.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.                    " BDC_PASS
*&---------------------------------------------------------------------*
*&      Form  GR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GR USING P_LIKP LIKE LIKP.
  DATA : TRAID LIKE LIKP-TRAID,
         LIKP  LIKE LIKP      ,
         CONTAIN(20)          .

  DATA : NO TYPE I,
         NLPLAT LIKE P_NLPLA.

* Header
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = P_NUMB1
       IMPORTING
            OUTPUT = CONTAIN.
* Item
  SELECT * FROM LIPS
           INTO CORRESPONDING FIELDS OF TABLE IT_LIPS
          WHERE VBELN = P_LIKP-VBELN.
  CHECK SY-SUBRC = 0.
  DESCRIBE TABLE IT_LIPS LINES NO.

* Mvt_item .
  LOOP AT IT_LIPS.
    CLEAR: IT_GOODSMVT_ITEM, WA_GOODSMVT_ITEM.
    WA_GOODSMVT_ITEM-MATERIAL  =
             IT_LIPS-MATNR.                    "'85850-3K100'.
    WA_GOODSMVT_ITEM-MOVE_TYPE =
             '101'.                                         "'101'.
    WA_GOODSMVT_ITEM-VENDOR    =
             LIKP-LIFNR.                       "'0000400016'.
    WA_GOODSMVT_ITEM-ENTRY_QNT =
             IT_LIPS-LGMNG.                                 "'1000'.
    WA_GOODSMVT_ITEM-ENTRY_UOM =
             IT_LIPS-MEINS.                    "'EA'.
    WA_GOODSMVT_ITEM-PO_NUMBER =
             IT_LIPS-VGBEL.                    "'4200000239'.
    WA_GOODSMVT_ITEM-PO_ITEM   =
             IT_LIPS-VGPOS.                                 "'00001'.
    WA_GOODSMVT_ITEM-MVT_IND   = 'B'.
    APPEND WA_GOODSMVT_ITEM TO IT_GOODSMVT_ITEM.
  ENDLOOP.

*Header
  WA_GOODSMVT_HEADER-PSTNG_DATE = SY-DATUM.
  WA_GOODSMVT_HEADER-DOC_DATE   = SY-DATUM.
  WA_GOODSMVT_HEADER-REF_DOC_NO =
              LIKP-VBELN.                      "'180000335'.
* Make goodsmvt_code
  WA_GOODSMVT_CODE-GM_CODE = '01'.

* Execute BAPI for Post
  PERFORM BAPI_GOODSMVT_CREATE TABLES   IT_GOODSMVT_ITEM
                                        IT_BAPIRET2
                               USING    WA_GOODSMVT_HEADER
                                        WA_GOODSMVT_CODE
                               CHANGING WA_GOODSMVT_HEADRET.


ENDFORM.                    " GR
*&---------------------------------------------------------------------*
*&      Form  bapi_goodsmvt_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_GOODSMVT_ITEM  text
*      -->P_IT_BAPIRET2  text
*      -->P_WA_GOODSMVT_HEADER  text
*      -->P_WA_GOODSMVT_CODE  text
*      <--P_WA_GOODSMVT_HEADRET  text
*----------------------------------------------------------------------*
FORM BAPI_GOODSMVT_CREATE
      TABLES   IMT_GOODSMVT_ITEM
                   STRUCTURE BAPI2017_GM_ITEM_CREATE
               EXT_RETURN
                   STRUCTURE BAPIRET2
      USING    VALUE(IM_GOODSMVT_HEADER)  LIKE BAPI2017_GM_HEAD_01
               VALUE(IM_GOODSMVT_CODE)    LIKE BAPI2017_GM_CODE
      CHANGING VALUE(EX_GOODSMVT_HEADRET) LIKE BAPI2017_GM_HEAD_RET.

  CLEAR: EXT_RETURN, EXT_RETURN[].
  CLEAR: EX_GOODSMVT_HEADRET.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      GOODSMVT_HEADER             = IM_GOODSMVT_HEADER
      GOODSMVT_CODE               = IM_GOODSMVT_CODE
*    TESTRUN                     = 'X'
    IMPORTING
      GOODSMVT_HEADRET            = EX_GOODSMVT_HEADRET
*   MATERIALDOCUMENT            =
*   MATDOCUMENTYEAR             =
    TABLES
      GOODSMVT_ITEM               = IMT_GOODSMVT_ITEM
*   GOODSMVT_SERIALNUMBER       =
      RETURN                      = EXT_RETURN.

  CLEAR: EXT_RETURN.
  READ TABLE EXT_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.  "Error Occurred !
    PERFORM GR_PROCESSING_ERROR USING EXT_RETURN.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*     EXPORTING
*       WAIT          =
*     IMPORTING
*       RETURN        =    .
  ENDIF.
ENDFORM.                    " bapi_goodsmvt_create
*&---------------------------------------------------------------------*
*&      Form  CREATE_ZTMM_CONTAINER_CT_YARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_ZTMM_CONTAINER_CT_YARD.
  CLEAR ZTMM_CONTAINER.
*[ 1 ] Get Storage Bin Data after TO Creation correctly
*Data emptied, Previous Location, Empty
  SELECT SINGLE BDATU "Date emptied
                LGPLA "Previous Location
*                kzler "Empty(STATUS)
           INTO (ZTMM_CONTAINER-BDATU,
                 ZTMM_CONTAINER-LGPLA)
*                 ztmm_container-kzler)
           FROM LAGP
          WHERE LGNUM = 'P01'
            AND LGTYP = P_NLTYP
            AND LGPLA = P_NLPLA.
*[ 2 ] Get Container Number and Check-in from Screen.
  ZTMM_CONTAINER-CONT_REG_NUMB1 = P_NUMB1.
  ZTMM_CONTAINER-PASS_DATE      = P_PDATE.
  ZTMM_CONTAINER-LGTYP          = P_NLTYP.
  ZTMM_CONTAINER-LGBER          = P_NLBER.
*[ 3 ] TIME STMP
  ZTMM_CONTAINER-ERDAT = SY-DATUM.
  ZTMM_CONTAINER-ERZET = SY-UZEIT.
  ZTMM_CONTAINER-ERNAM = SY-UNAME.
*haseeb
*  INSERT ztmm_container FROM ztmm_container.
  MODIFY ZTMM_CONTAINER FROM ZTMM_CONTAINER.
*haseeb
ENDFORM.                    " CREATE_ZTMM_CONTAINER_CT_YARD
*&---------------------------------------------------------------------*
*&      Form  TO_CREATION_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TO_CREATION_ERROR.
  DATA   : WA_LIPS LIKE LIPS.
  DATA : TXT LIKE T100-TEXT.
  DATA : MSGNR(3) TYPE N.
  CLEAR : ZTMM_CT_ERRLOG.
  READ TABLE IT_LIPS INTO WA_LIPS WITH KEY VBELN = IT_LIKPS-VBELN.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = P_NUMB1
       IMPORTING
            OUTPUT = ZTMM_CT_ERRLOG-ZCONTAINER.
  ZTMM_CT_ERRLOG-EPOSITION   = 'TO CREATION ERROR'.
  ZTMM_CT_ERRLOG-PARKING_TXT = P_PTXT.
  ZTMM_CT_ERRLOG-VBELN       = IT_LIKPS-VBELN     . "Inbound Delivery
  ZTMM_CT_ERRLOG-EBELN       = WA_LIPS-VGBEL      . "PO no.

  MSGNR = IT_MESSAGE-MSGNR.
  CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
       EXPORTING
            LANGU = SY-LANGU
            MSGID = IT_MESSAGE-MSGID
            MSGNO = MSGNR
            MSGV1 = IT_MESSAGE-MSGV1+0(50)
            MSGV2 = IT_MESSAGE-MSGV2+0(50)
            MSGV3 = IT_MESSAGE-MSGV3+0(50)
            MSGV4 = IT_MESSAGE-MSGV4+0(50)
       IMPORTING
            TEXT  = TXT.
  IF SY-SUBRC = 0.
    ZTMM_CT_ERRLOG-TEXT   =  TXT.        "Error Message
    ZTMM_CT_ERRLOG-ERDAT  =  SY-DATUM.   "Date on which the
    "record was created
    ZTMM_CT_ERRLOG-ERZET  =  SY-UZEIT.   "Entry time
    ZTMM_CT_ERRLOG-ERNAM  =  SY-UNAME.   "Name of Person who Created
    ZTMM_CT_ERRLOG-AEDAT  =  SY-DATUM.
    ZTMM_CT_ERRLOG-AEZET  =  SY-UZEIT.
    ZTMM_CT_ERRLOG-AENAM  =  SY-UNAME.
    INSERT ZTMM_CT_ERRLOG FROM ZTMM_CT_ERRLOG.
*    MESSAGE E009 WITH TXT.     " 2004/1/30 block by stlim
*    EXPORT ztmm_ct_errlog TO MEMORY ID 'TABL'.
  ENDIF.
ENDFORM.                    " TO_CREATION_ERROR
*&---------------------------------------------------------------------*
*&      Form  GR_PROCESSING_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GR_PROCESSING_ERROR USING ET_MESSAGE STRUCTURE BAPIRET2.
  DATA : WA_LIPS LIKE LIPS.
  DATA : TXT LIKE T100-TEXT.
  DATA : MSGNR(3) TYPE N.
  CLEAR: ZTMM_CT_ERRLOG.
  READ TABLE IT_LIPS INTO WA_LIPS WITH KEY VBELN = IT_LIKPS-VBELN.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = P_NUMB1
       IMPORTING
            OUTPUT = ZTMM_CT_ERRLOG-ZCONTAINER.
  ZTMM_CT_ERRLOG-EPOSITION = 'GR CREATION ERROR'.
  ZTMM_CT_ERRLOG-VBELN     = IT_LIKPS-VBELN     . "Inbound Delivery
  ZTMM_CT_ERRLOG-PARKING_TXT = P_PTXT           .
  ZTMM_CT_ERRLOG-EBELN     = WA_LIPS-VGBEL      . "PO no.

  MSGNR = IT_MESSAGE-MSGNR.
  CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
       EXPORTING
            LANGU = SY-LANGU
            MSGID = ET_MESSAGE-ID
            MSGNO = MSGNR
            MSGV1 = ET_MESSAGE-MESSAGE_V1+0(50)
            MSGV2 = ET_MESSAGE-MESSAGE_V2+0(50)
            MSGV3 = ET_MESSAGE-MESSAGE_V3+0(50)
            MSGV4 = ET_MESSAGE-MESSAGE_V4+0(50)
       IMPORTING
            TEXT  = TXT.
  IF SY-SUBRC = 0.
    ZTMM_CT_ERRLOG-TEXT   =  TXT.        "Error Message
    ZTMM_CT_ERRLOG-ERDAT  =  SY-DATUM.   "Date on which the
    "record was created
    ZTMM_CT_ERRLOG-ERZET  =  SY-UZEIT.   "Entry time
    ZTMM_CT_ERRLOG-ERNAM  =  SY-UNAME.   "Name of Person who Created
    ZTMM_CT_ERRLOG-AEDAT  =  SY-DATUM.
    ZTMM_CT_ERRLOG-AEZET  =  SY-UZEIT.
    ZTMM_CT_ERRLOG-AENAM  =  SY-UNAME.
    INSERT ZTMM_CT_ERRLOG FROM ZTMM_CT_ERRLOG.
*    MESSAGE e009 WITH txt.     " 2004/1/30 block by stlim
  ENDIF.
ENDFORM.                    " GR_PROCESSING_ERROR
*&---------------------------------------------------------------------*
*&      Form  LECI_ERROR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LECI_ERROR_LOG.
  DATA : WA_LIPS LIKE LIPS.
  DATA : TXT LIKE T100-TEXT.
  DATA : MSGNR(3) TYPE N.

  LOOP AT IT_LIKPS.
    CLEAR : ZTMM_CT_ERRLOG.
    READ TABLE IT_LIPS INTO WA_LIPS WITH KEY VBELN = IT_LIKPS-VBELN.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
         EXPORTING
              INPUT  = P_NUMB1
         IMPORTING
              OUTPUT = ZTMM_CT_ERRLOG-ZCONTAINER.
    ZTMM_CT_ERRLOG-EPOSITION = 'TO CREATION ERROR'.
    ZTMM_CT_ERRLOG-VBELN     = IT_LIKPS-VBELN     . "Inbound Delivery
    ZTMM_CT_ERRLOG-EBELN     = WA_LIPS-VGBEL      . "PO no.
    ZTMM_CT_ERRLOG-PARKING_TXT = P_PTXT           .
    MSGNR = IT_MESSAGE-MSGNR.
    CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
         EXPORTING
              LANGU = SY-LANGU
              MSGID = IT_MESSAGE-MSGID
              MSGNO = MSGNR
              MSGV1 = IT_MESSAGE-MSGV1+0(50)
              MSGV2 = IT_MESSAGE-MSGV2+0(50)
              MSGV3 = IT_MESSAGE-MSGV3+0(50)
              MSGV4 = IT_MESSAGE-MSGV4+0(50)
         IMPORTING
              TEXT  = TXT.
    IF SY-SUBRC = 0.
      ZTMM_CT_ERRLOG-TEXT   =  TXT.        "Error Message
      ZTMM_CT_ERRLOG-ERDAT  =  SY-DATUM.   "Date on which the
      "record was created
      ZTMM_CT_ERRLOG-ERZET  =  SY-UZEIT.   "Entry time
      ZTMM_CT_ERRLOG-ERNAM  =  SY-UNAME.   "Name of Person who Created
      ZTMM_CT_ERRLOG-AEDAT  =  SY-DATUM.
      ZTMM_CT_ERRLOG-AEZET  =  SY-UZEIT.
      ZTMM_CT_ERRLOG-AENAM  =  SY-UNAME.
      INSERT ZTMM_CT_ERRLOG FROM ZTMM_CT_ERRLOG.
*      MESSAGE e009 WITH txt.     " 2004/1/30 block by stlim
    ENDIF.
  ENDLOOP.
ENDFORM.                    " LECI_ERROR_LOG
*&---------------------------------------------------------------------*
*&      Form  PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT.
  TABLES : LECI_EVENT_DATA.
  SELECT SINGLE *
          FROM LECI_EVENT_DATA
         WHERE CONT_REG_NUMB1 = P_NUMB1.

  SELECT SINGLE  PASS_NUMB
             FROM LECI_EVENT
             INTO  W_PASS_NUMB
            WHERE EVENT = 'CI'
              AND GUID_EVENT_DATA = LECI_EVENT_DATA-GUID_EVENT_DATA.
  PERFORM PRINT_EXEC.
ENDFORM.                    " PRINT
*&---------------------------------------------------------------------*
*&      Form  PRINT_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_EXEC.
  DATA    : DATE(10).
  CLEAR   : IT_BDC, IT_MESSAGE.
  REFRESH : IT_BDC, IT_MESSAGE.

  WRITE : LECI_TRA_DYN-PASS_DATE TO DATE.
  PERFORM BDC_PASS USING:
    'X' 'SAPRLECHKIN' '1000'                 ,
    ' ' 'LECI_SELOPT_DYN-PASS_NUMB' W_PASS_NUMB,
    ' ' 'BDC_OKCODE'  '=SEARCH'              .
  PERFORM BDC_PASS USING:
   'X' 'SAPRLECHKIN' '1000'                 ,
   ' ' 'BDC_OKCODE'  '=PRINT'               .

  CALL TRANSACTION 'LECI'
           USING IT_BDC
           MODE W_STATUS
           UPDATE'S'
           MESSAGES INTO IT_MESSAGE.
ENDFORM.                    " PRINT_EXEC
*&---------------------------------------------------------------------*
*&      Form  GR_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIKPS  text
*----------------------------------------------------------------------*
FORM GR_BDC USING    P_LIKPS LIKE LIKP.
  CLEAR   : IT_BDC, IT_MESSAGE.
  REFRESH : IT_BDC, IT_MESSAGE.

**--- insert by stlim (2004/04/26)
  CLEAR : W_VBELN.

  MOVE : P_LIKPS-VBELN TO W_VBELN.

  DATA : L_BLDAT LIKE BDCDATA-FVAL,
         L_BUDAT LIKE BDCDATA-FVAL,
         L_VLIEF LIKE BDCDATA-FVAL,
         L_BUDAT_TEMP TYPE D.

  CONSTANTS : C_UZEIT_000000 TYPE T VALUE '000000',
              C_UZEIT_035959 TYPE T VALUE '035959'.

** For 3 shift
   CONSTANTS : C_UZEIT_062959 TYPE T VALUE '062959'.

   IF P_TIME GE C_UZEIT_000000 AND
     P_TIME LE C_UZEIT_062959.
*  IF P_TIME GE C_UZEIT_000000 AND
*     P_TIME LE C_UZEIT_035959.
    L_BUDAT_TEMP = P_DATE - 1.
  ELSE.
    L_BUDAT_TEMP = P_DATE.
  ENDIF.

  WRITE: P_DATE              TO L_BLDAT,
         L_BUDAT_TEMP        TO L_BUDAT.

  MOVE : W_VBELN TO L_VLIEF.

  CONDENSE : L_BLDAT, L_BUDAT, L_VLIEF.

** Changed by Furong on 07/25/11
  DO 3 TIMES.
**--- insert by stlim (2004/05/05)
    CLEAR: EX_SUBRC, IT_MESSAGE.
    REFRESH IT_MESSAGE.
** end on 07/25/11

    CALL FUNCTION 'Z_FMM_60XX_VL32N'
      EXPORTING
*   CTU                    = 'X'
*   MODE                   = 'N'
*   UPDATE                 = 'L'
*   GROUP                  =
*   USER                   =
*   KEEP                   =
*   HOLDDATE               =
*   NODATA                 = '/'
        VBELN_001              = L_VLIEF
        BLDAT_002              = L_BLDAT
        WADAT_IST_LA_003       = L_BUDAT
       IMPORTING
         SUBRC           = EX_SUBRC
       TABLES
         MESSTAB         = IT_MESSAGE.
**--- end of insert
** Changed by Furong on 07/25/11
    READ TABLE IT_MESSAGE WITH KEY MSGTYP = 'E'
                                   MSGID = 'VL'
                                   MSGNR = '247'.
    IF SY-SUBRC = 0.
      WAIT UP TO 5 SECONDS.
      CONTINUE.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
** end on 07/25/11

**--- blocked by stlim (2004/05/05)
*  CALL FUNCTION 'Z_FMM_60XX_MB01'
*   EXPORTING
**   CTU               = 'X'
*    mode              = w_status
**   UPDATE            = 'L'
**   GROUP             =
**   USER              =
**   KEEP              =
**   HOLDDATE          =
**   NODATA            = '/'
*     bldat_001         = l_bldat
*     budat_002         = l_budat
*     xfull_003         = 'X'
*     wvers1_004        = 'X'
*     bwartwe_005       = '101'
*     vlief_006         = l_vlief   "  '180000081'
*   IMPORTING
*     subrc             = ex_subrc
*   TABLES
*     messtab           = it_message.
**--- end of block
**--- end of insert

**--- blocked by stlim (2004/04/26)
*  PERFORM bdc_pass USING:
*        'X' 'SAPMV50A'   '4104'       ,
*        ' ' 'LIKP-VBELN' p_likps-vbeln,
*        ' ' 'BDC_OKCODE' '=WABU_T'    .
*  CALL TRANSACTION 'VL32N'
*              USING it_bdc
*              MODE w_status
*              UPDATE'S'
*              MESSAGES INTO it_message.
**--- end of block

  READ TABLE IT_MESSAGE WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC = 0.
    PERFORM ERROR_MESSAGE_GR.
  ELSE.
    PERFORM SUCCESS_MESSAGE_GR.
  ENDIF.
ENDFORM.                    " GR_BDC
*&---------------------------------------------------------------------*
*&      Form  ERROR_MESSAGE_GR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ERROR_MESSAGE_GR .
  DATA : WA_LIPS LIKE LIPS.
  DATA : TXT LIKE T100-TEXT.
  DATA : MSGNR(3) TYPE N,
         L_ERDAT LIKE ZTMM_CT_ERRLOG-ERDAT,
         L_ERZET  LIKE ZTMM_CT_ERRLOG-ERZET,
         L_AENAM LIKE ZTMM_CT_ERRLOG-AENAM.

  CLEAR: ZTMM_CT_ERRLOG.
  READ TABLE IT_LIPS INTO WA_LIPS WITH KEY VBELN = IT_LIKPS-VBELN.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = P_NUMB1
       IMPORTING
            OUTPUT = ZTMM_CT_ERRLOG-ZCONTAINER.
  ZTMM_CT_ERRLOG-EPOSITION = 'GR CREATION ERROR'.
  ZTMM_CT_ERRLOG-VBELN     = IT_LIKPS-VBELN     . "Inbound Delivery
  ZTMM_CT_ERRLOG-PARKING_TXT = P_PTXT           .
  ZTMM_CT_ERRLOG-EBELN     = WA_LIPS-VGBEL      . "PO no.

  MSGNR = IT_MESSAGE-MSGNR.
  CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
       EXPORTING
            LANGU = SY-LANGU
            MSGID = IT_MESSAGE-MSGID
            MSGNO = MSGNR
            MSGV1 = IT_MESSAGE-MSGV1+0(50)
            MSGV2 = IT_MESSAGE-MSGV2+0(50)
            MSGV3 = IT_MESSAGE-MSGV3+0(50)
            MSGV4 = IT_MESSAGE-MSGV4+0(50)
       IMPORTING
            TEXT  = TXT.
  IF SY-SUBRC = 0.
** Furong on 07/20/11
    SELECT SINGLE ERDAT ERZET AENAM
      INTO (L_ERDAT, L_ERZET, L_AENAM)
      FROM ZTMM_CT_ERRLOG
      WHERE ZCONTAINER = ZTMM_CT_ERRLOG-ZCONTAINER
        AND VBELN = ZTMM_CT_ERRLOG-VBELN.
    IF SY-SUBRC = 0.
      ZTMM_CT_ERRLOG-TEXT   =  TXT.        "Error Message
      ZTMM_CT_ERRLOG-ERDAT  =  L_ERDAT.   "creation Date on which the
      ZTMM_CT_ERRLOG-ERZET  =  L_ERZET.   "Entry time
      ZTMM_CT_ERRLOG-ERNAM  =  L_AENAM.   "Name of Person who Created
      ZTMM_CT_ERRLOG-AEDAT  =  SY-DATUM.   " change date
      ZTMM_CT_ERRLOG-AEZET  =  SY-UZEIT.
      ZTMM_CT_ERRLOG-AENAM  =  SY-UNAME.
      MODIFY ZTMM_CT_ERRLOG FROM ZTMM_CT_ERRLOG.
    ELSE.
      ZTMM_CT_ERRLOG-TEXT   =  TXT.        "Error Message
      ZTMM_CT_ERRLOG-ERDAT  =  SY-DATUM.   "Date on which the
      "record was created
      ZTMM_CT_ERRLOG-ERZET  =  SY-UZEIT.   "Entry time
      ZTMM_CT_ERRLOG-ERNAM  =  SY-UNAME.   "Name of Person who Created
      ZTMM_CT_ERRLOG-AEDAT  =  SY-DATUM.
      ZTMM_CT_ERRLOG-AEZET  =  SY-UZEIT.
      ZTMM_CT_ERRLOG-AENAM  =  SY-UNAME.
      INSERT ZTMM_CT_ERRLOG FROM ZTMM_CT_ERRLOG.
    ENDIF.
*    MESSAGE e009 WITH txt.     " 2004/1/30 block by stlim
*    EXPORT ztmm_ct_errlog TO MEMORY ID 'TABL'.

  ENDIF.

  COMMIT WORK.
ENDFORM.                    " ERROR_MESSAGE_GR
*&---------------------------------------------------------------------*
*&      Form  success_message_gr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUCCESS_MESSAGE_GR.
  DATA : WA_LIPS LIKE LIPS.
  DATA : TXT LIKE T100-TEXT.
  DATA : MSGNR(3) TYPE N,
         L_ERDAT LIKE ZTMM_CT_ERRLOG-ERDAT,
         L_ERZET  LIKE ZTMM_CT_ERRLOG-ERZET,
         L_AENAM LIKE ZTMM_CT_ERRLOG-AENAM.

  CLEAR: ZTMM_CT_ERRLOG.
  READ TABLE IT_LIPS INTO WA_LIPS WITH KEY VBELN = IT_LIKPS-VBELN.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = P_NUMB1
       IMPORTING
            OUTPUT = ZTMM_CT_ERRLOG-ZCONTAINER.
  ZTMM_CT_ERRLOG-EPOSITION = 'GR CREATION SUCCESS'.
  ZTMM_CT_ERRLOG-VBELN     = IT_LIKPS-VBELN     . "Inbound Delivery
  ZTMM_CT_ERRLOG-PARKING_TXT = P_PTXT           .
  ZTMM_CT_ERRLOG-EBELN     = WA_LIPS-VGBEL      . "PO no.

  MSGNR = IT_MESSAGE-MSGNR.
  CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
       EXPORTING
            LANGU = SY-LANGU
            MSGID = IT_MESSAGE-MSGID
            MSGNO = MSGNR
            MSGV1 = IT_MESSAGE-MSGV1+0(50)
            MSGV2 = IT_MESSAGE-MSGV2+0(50)
            MSGV3 = IT_MESSAGE-MSGV3+0(50)
            MSGV4 = IT_MESSAGE-MSGV4+0(50)
       IMPORTING
            TEXT  = TXT.
  IF SY-SUBRC = 0.
** Furong on 07/20/11
    SELECT SINGLE ERDAT ERZET AENAM
      INTO (L_ERDAT, L_ERZET, L_AENAM)
      FROM ZTMM_CT_ERRLOG
      WHERE ZCONTAINER = ZTMM_CT_ERRLOG-ZCONTAINER
        AND VBELN = ZTMM_CT_ERRLOG-VBELN.
    IF SY-SUBRC = 0.
      ZTMM_CT_ERRLOG-TEXT   =  TXT.        "Error Message
      ZTMM_CT_ERRLOG-ERDAT  =  L_ERDAT.   "creation Date on which the
      ZTMM_CT_ERRLOG-ERZET  =  L_ERZET.   "Entry time
      ZTMM_CT_ERRLOG-ERNAM  =  L_AENAM.   "Name of Person who Created
      ZTMM_CT_ERRLOG-AEDAT  =  SY-DATUM.   " change date
      ZTMM_CT_ERRLOG-AEZET  =  SY-UZEIT.
      ZTMM_CT_ERRLOG-AENAM  =  SY-UNAME.
      MODIFY ZTMM_CT_ERRLOG FROM ZTMM_CT_ERRLOG.
    ELSE.
      ZTMM_CT_ERRLOG-TEXT   =  TXT.        "Error Message
      ZTMM_CT_ERRLOG-ERDAT  =  SY-DATUM.   "creation Date on which the
      "record was created
      ZTMM_CT_ERRLOG-ERZET  =  SY-UZEIT.   "Entry time
      ZTMM_CT_ERRLOG-ERNAM  =  SY-UNAME.   "Name of Person who Created
      ZTMM_CT_ERRLOG-AEDAT  =  SY-DATUM.   " change date
      ZTMM_CT_ERRLOG-AEZET  =  SY-UZEIT.
      ZTMM_CT_ERRLOG-AENAM  =  SY-UNAME.
      INSERT ZTMM_CT_ERRLOG FROM ZTMM_CT_ERRLOG.
    ENDIF.
*    MESSAGE e009 WITH txt.     " 2004/1/30 block by stlim
*    EXPORT ztmm_ct_errlog TO MEMORY ID 'TABL'.

  ENDIF.

  COMMIT WORK.
ENDFORM.                    " success_message_gr
