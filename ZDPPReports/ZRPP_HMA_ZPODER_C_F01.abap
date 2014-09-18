*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_F01                                        *
*----------------------------------------------------------------------*

FORM P2000_GET_DATA.

  DATA : LT_EDIDC LIKE TABLE OF EDIDC WITH HEADER LINE,
         LT_EDIDD LIKE TABLE OF EDIDD WITH HEADER LINE.

  DATA : LT_ZSEG1 TYPE TABLE OF T_ZSEG1 WITH HEADER LINE,
         LT_ZSEG2 TYPE TABLE OF T_ZSEG2 WITH HEADER LINE.

*  DATA : LT_ZSEG1 LIKE TABLE OF ZPOSEG1 WITH HEADER LINE,
*         LT_ZSEG2 LIKE TABLE OF ZPOSEG2 WITH HEADER LINE.

  DATA : LT_DATA LIKE TABLE OF GT_DATA ,
         WA_DATA LIKE LINE OF GT_DATA.

  DATA : LT_IDOC LIKE TABLE OF ZTPP_PO_IDOC WITH HEADER LINE.

  RANGES : R_STATUS FOR EDIDC-STATUS,
           S_DOCNUM FOR EDIDC-DOCNUM.



  R_STATUS-SIGN = 'E'.
  R_STATUS-OPTION = 'EQ'.
  R_STATUS-LOW = '30'. APPEND R_STATUS.
  R_STATUS-LOW = '02'. APPEND R_STATUS.
  R_STATUS-LOW = '04'. APPEND R_STATUS.
  R_STATUS-LOW = '05'. APPEND R_STATUS.
  R_STATUS-LOW = '25'. APPEND R_STATUS.
  R_STATUS-LOW = '29'. APPEND R_STATUS.
  R_STATUS-LOW = '26'. APPEND R_STATUS.
  R_STATUS-LOW = '32'. APPEND R_STATUS.
*  R_STATUS-LOW = '51'. APPEND R_STATUS.
  R_STATUS-LOW = '56'. APPEND R_STATUS.
  R_STATUS-LOW = '61'. APPEND R_STATUS.
  R_STATUS-LOW = '63'. APPEND R_STATUS.
  R_STATUS-LOW = '65'. APPEND R_STATUS.
  R_STATUS-LOW = '60'. APPEND R_STATUS.
  R_STATUS-LOW = '64'. APPEND R_STATUS.
  R_STATUS-LOW = '66'. APPEND R_STATUS.
  R_STATUS-LOW = '69'. APPEND R_STATUS.

*  UPDATE ZTPP_PO_IDOC SET
*  STATUS = ''
*  WHERE STATUS = 'ENDPO'.

  DATA: PERCENT(3) TYPE N.

  DO 100 TIMES.
    MOVE: SY-INDEX TO PERCENT.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = PERCENT
              TEXT       = TEXT-001
         EXCEPTIONS
              OTHERS     = 1.

  ENDDO.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_IDOC
    FROM ZTPP_PO_IDOC
    WHERE CRDAT  IN S_CREDAT
    AND   STATUS EQ ''
    AND   ZVIN_STT EQ ''.

  SORT LT_IDOC BY DOCNUM CRDAT CRTIM DESCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_IDOC
    COMPARING WO_SER NATN DIST WKEXC WKINC.

  CHECK NOT LT_IDOC[] IS INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_EDIDC
  FROM EDIDC
  FOR ALL ENTRIES IN LT_IDOC
  WHERE CREDAT IN S_CREDAT
  AND MESTYP EQ P_MESTYP
  AND DOCNUM EQ LT_IDOC-DOCNUM
  AND DIRECT EQ '2'
  AND STATUS IN R_STATUS.

  CHECK  NOT LT_EDIDC[] IS INITIAL.

  LOOP AT  LT_EDIDC .

    CALL FUNCTION 'IDOC_READ_COMPLETELY'
         EXPORTING
              DOCUMENT_NUMBER         = LT_EDIDC-DOCNUM
         TABLES
              INT_EDIDD               = LT_EDIDD
         EXCEPTIONS
              DOCUMENT_NOT_EXIST      = 1
              DOCUMENT_NUMBER_INVALID = 2
              OTHERS                  = 3.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      LOOP AT LT_EDIDD WHERE SEGNAM = 'ZPOSEG1'.
        LT_ZSEG1 = LT_EDIDD-SDATA.
        LT_ZSEG1-UPDDAT = LT_EDIDC-UPDDAT.
        LT_ZSEG1-UPDTIM = LT_EDIDC-UPDTIM.
        LT_ZSEG1-DOCNUM = LT_EDIDC-DOCNUM.

        APPEND LT_ZSEG1.
      ENDLOOP.
      LOOP AT LT_EDIDD WHERE SEGNAM = 'ZPOSEG2'.
        LT_ZSEG2 = LT_EDIDD-SDATA.
        LT_ZSEG2-UPDDAT = LT_EDIDC-UPDDAT.
        LT_ZSEG2-UPDTIM = LT_EDIDC-UPDTIM.
        LT_ZSEG2-DOCNUM = LT_EDIDC-DOCNUM.
        APPEND LT_ZSEG2.
      ENDLOOP.
    ENDIF.
*    AT FIRST.
*      CLEAR GV_NEW.
*
*    ENDAT.

    CLEAR : LT_ZSEG1, LT_EDIDD.
  ENDLOOP.


  LOOP AT LT_ZSEG1 WHERE PRDOD NE ''.

    MOVE-CORRESPONDING LT_ZSEG1 TO GT_DATA.

    GT_DATA-UPDDAT   = LT_ZSEG1-UPDDAT.
    GT_DATA-UPDTIM   = LT_ZSEG1-UPDTIM.
    GT_DATA-DOCNUM   = LT_ZSEG1-DOCNUM.
    GT_DATA-WO_SER   = LT_ZSEG1-PRDOD .
    GT_DATA-NATION   = LT_ZSEG1-NATN  .
    GT_DATA-DEALER   = LT_ZSEG1-DIST  .
    GT_DATA-EXTC     = LT_ZSEG1-WKEXC .
    GT_DATA-INTC     = LT_ZSEG1-WKINC .

    GT_DATA-DEST     = LT_ZSEG1-DESTN .
    GT_DATA-MOYE     = LT_ZSEG1-MDYR  .
    GT_DATA-BMDL     = LT_ZSEG1-MDINX .
    GT_DATA-OCNN     = LT_ZSEG1-OCCN  .

    GT_DATA-VERS     = LT_ZSEG1-GRADE .
    GT_DATA-INITQTY  = LT_ZSEG1-IOQTY .
    GT_DATA-MODQTY   = LT_ZSEG1-MOQTY .
    GT_DATA-LCNO     = LT_ZSEG1-LCLDL .
    GT_DATA-LCNT     = LT_ZSEG1-LCCNT .
    GT_DATA-FLET     = LT_ZSEG1-FLTFG .
    GT_DATA-REQ_DATE = LT_ZSEG1-RDD   .
    GT_DATA-CRT_DATE = LT_ZSEG1-CRDAT .
    GT_DATA-CHG_DATE = LT_ZSEG1-AEDAT .

    GT_DATA-ORDQTY    =  LT_ZSEG1-IOQTY .
    GT_DATA-NEWQTY    =  LT_ZSEG1-MOQTY .
    GT_DATA-ZSDAT	  = LT_ZSEG1-CRDAT .
    GT_DATA-ZSTIM	  = LT_ZSEG1-AEDAT .
    GT_DATA-ZUSER	  = SY-UNAME.

    READ TABLE LT_IDOC WITH KEY DOCNUM = GT_DATA-DOCNUM.
    IF LT_IDOC-STATUS EQ 'S' .
      GT_DATA-STATUS = '@ME@'.
    ELSEIF LT_IDOC-STATUS EQ 'E'.
      GT_DATA-STATUS = '@02@'.
    ELSE.
      GT_DATA-STATUS = '@CG@'.
    ENDIF.

*    READ TABLE LT_IDOC WITH KEY WO_SER = GT_DATA-WO_SER.

*    IF LT_IDOC-STATUS NE '' AND
*       LT_IDOC-ZVIN_STT EQ ''.
*      GT_DATA-STATUS = '@5D@'.
*    ENDIF.


    IF NOT GT_DATA-WO_SER IS INITIAL.
      PERFORM CHECK_WOSUM USING LT_ZSEG1 GV_NEW .
      IF GV_NEW EQ 'X'.
        APPEND GT_DATA.
      ELSE .
        APPEND GT_DATA TO GT_OLD.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE LT_ZSEG1 WHERE PRDOD EQ ''.

  SORT : GT_OLD
  BY  DOCNUM UPDDAT UPDTIM WO_SER NATION DEALER EXTC INTC,
         GT_DATA
  BY  DOCNUM UPDDAT UPDTIM WO_SER NATION DEALER EXTC INTC.
  DELETE ADJACENT DUPLICATES FROM GT_OLD
    COMPARING DOCNUM UPDDAT UPDTIM WO_SER NATION DEALER EXTC INTC.
  DELETE ADJACENT DUPLICATES FROM GT_DATA
    COMPARING DOCNUM UPDDAT UPDTIM WO_SER NATION DEALER EXTC INTC.

*  GT_ZPOSEG1[] = LT_ZSEG1[] .
*  GT_ZPOSEG2[] = LT_ZSEG2[].

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CHECK_WOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ZPOSEG1  text
*      -->P_L_OLD  text
*----------------------------------------------------------------------*
FORM CHECK_WOSUM  USING    P_ZPOSEG1 TYPE T_ZSEG1
                           P_NEW.

  DATA: LT_WOSUM LIKE ZTPP_WOSUM.
  CLEAR  : LT_WOSUM.

  SELECT SINGLE * INTO LT_WOSUM
    FROM ZTPP_WOSUM
    WHERE WO_SER = P_ZPOSEG1-PRDOD
      AND NATION = P_ZPOSEG1-NATN
      AND DEALER = P_ZPOSEG1-DIST
      AND EXTC   = P_ZPOSEG1-WKEXC
      AND INTC   = P_ZPOSEG1-WKINC.

  IF SY-SUBRC <> 0 .
    P_NEW = 'X'.
  ELSE.
    CLEAR : P_NEW.
  ENDIF.

ENDFORM.                    " CHECK_WOSUM
*&---------------------------------------------------------------------*
*&      Form  P3000_PO_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_PO_PROC.
  DATA : RETURN LIKE TABLE OF BAPIRETURN.
  DATA : LT_EDIDD LIKE TABLE OF EDIDD WITH HEADER LINE,
         LT_EDIDC LIKE TABLE OF EDIDC WITH HEADER LINE.

  DATA : LV_CNT TYPE I.


**** Create PO or Modify PO

  CALL FUNCTION 'Z_FPP_HMA_IF_PO'
       EXPORTING
            NEW    = GV_NEW
       TABLES
            RETURN = RETURN
            HEADER = GT_ZPOSEG1.
  CLEAR GT_DATA .

****


  GT_DATA-STATUS = '@5D@'.
  GT_ZPOSEG1-STATUS = 'S'.

  MODIFY GT_DATA TRANSPORTING STATUS WHERE STATUS IS INITIAL.
  MODIFY GT_ZPOSEG1 TRANSPORTING STATUS WHERE STATUS IS INITIAL.

  LOOP AT GT_ZPOSEG1.
    LT_EDIDD-SEGNAM = C_ZPOSEG1.
    LT_EDIDD-SDATA  = GT_ZPOSEG1.

    APPEND LT_EDIDD.
  ENDLOOP.

  PERFORM P3300_SEND_IDOC TABLES LT_EDIDD LT_EDIDC .
  PERFORM REFRESH_DISPLAY .

ENDFORM.                    " P3000_PO_PROC
*&---------------------------------------------------------------------*
*&      Form  P3100_PO_REJC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3100_PO_REJC.

  DATA : LT_EDIDD LIKE TABLE OF EDIDD WITH HEADER LINE,
         LT_EDIDC LIKE TABLE OF EDIDC WITH HEADER LINE.

  CLEAR GT_DATA .
  GT_DATA-STATUS = '@5C@'.
  GT_ZPOSEG1-STATUS = 'E'.

  MODIFY GT_DATA TRANSPORTING STATUS WHERE STATUS IS INITIAL.
  MODIFY GT_ZPOSEG1 TRANSPORTING STATUS WHERE STATUS IS INITIAL.

  LOOP AT GT_ZPOSEG1.
    LT_EDIDD-SEGNAM = C_ZPOSEG1.
    LT_EDIDD-SDATA  = GT_ZPOSEG1.

    APPEND LT_EDIDD.

*    /for Test
*    UPDATE ZTPP_PO_IDOC SET STATUS = 'E'
*          WHERE WO_SER = GT_ZPOSEG1-PRDOD.

  ENDLOOP.

  PERFORM P3300_SEND_IDOC TABLES LT_EDIDD LT_EDIDC .
  PERFORM REFRESH_DISPLAY .
ENDFORM.                    " P3100_PO_REJC
*&---------------------------------------------------------------------*
*&      Form  P3200_ZVIN_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3200_ZVIN_SAVE.
  DATA : RETURN LIKE TABLE OF BAPIRETURN WITH HEADER LINE.
  CALL FUNCTION 'Z_FPP_HMA_UPDATE_UM'
       TABLES
            RETURN = RETURN
            ITEM   = GT_ZPOSEG2.

  READ TABLE RETURN INDEX 1.
  IF RETURN-TYPE NE 'E'.
    GT_DATA-STATUS = '@5B@'.

    MODIFY GT_DATA TRANSPORTING STATUS WHERE STATUS EQ '@5D@'.

    PERFORM REFRESH_DISPLAY .
  ENDIF.
ENDFORM.                    " P3200_ZVIN_SAVE
*---------------------------------------------------------------------*
*       FORM P3100_GENERATE_CONTROL_RECORD                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM P3200_GENERATE_CONTROL_RECORD USING P_EDIDC LIKE EDIDC .

  P_EDIDC-RCVPOR = C_RCVPOR. "Receiver Port
  P_EDIDC-MESTYP = C_MESTYP. "Message type
  P_EDIDC-IDOCTP = C_IDOCTP. "Basic IDOC type
  P_EDIDC-RCVPRT = C_RCVPRT. "Partner type of receiver
  P_EDIDC-RCVPRN = C_LOGSYS. "Partner number of receiver

  P_EDIDC-SNDPRT = C_SNDPRT. "Sender Partner type
  P_EDIDC-SNDPRN = C_SNDPRN. "Sender Partner Number
ENDFORM.                    " generate_control_record

*---------------------------------------------------------------------*
*       FORM P3000_SEND_IDOC                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_EDIDD                                                       *
*  -->  P_EDIDC                                                       *
*---------------------------------------------------------------------*
FORM P3300_SEND_IDOC TABLES P_EDIDD STRUCTURE EDIDD
                            P_EDIDC STRUCTURE EDIDC.

  DATA : L_IDOC_CTRL LIKE EDIDC.
  PERFORM  P3200_GENERATE_CONTROL_RECORD USING L_IDOC_CTRL.

  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
       EXPORTING
            MASTER_IDOC_CONTROL            = L_IDOC_CTRL
       TABLES
            COMMUNICATION_IDOC_CONTROL     = P_EDIDC
            MASTER_IDOC_DATA               = P_EDIDD
       EXCEPTIONS
            ERROR_IN_IDOC_CONTROL          = 1
            ERROR_WRITING_IDOC_STATUS      = 2
            ERROR_IN_IDOC_DATA             = 3
            SENDING_LOGICAL_SYSTEM_UNKNOWN = 4
            OTHERS                         = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    COMMIT WORK.
    LOOP AT P_EDIDC.
*      WRITE:/ 'IDoc Generated - ', P_EDIDC-DOCNUM.
    ENDLOOP.
  ENDIF.

ENDFORM.



*---------------------------------------------------------------------*
*       FORM P4000_CONVERSION_ATINN                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VALUE                                                       *
*  -->  P_ATINN                                                       *
*---------------------------------------------------------------------*
FORM P4000_CONVERSION_ATINN USING P_VALUE P_ATINN .

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = P_VALUE
       IMPORTING
            OUTPUT = P_ATINN.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM P5000_GET_ZVIN                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_DATA                                                        *
*---------------------------------------------------------------------*
FORM P5000_GET_ZVIN USING P_DATA STRUCTURE GT_DATA.
  CLEAR : GT_ZVIN[].

  LOOP AT GT_ZPOSEG2 WHERE PRDOD EQ GT_DATA-WO_SER
                      AND NATN  EQ GT_DATA-NATION
                      AND DIST  EQ GT_DATA-DEALER
                      AND WKEXC EQ GT_DATA-EXTC
                      AND WKINC EQ GT_DATA-INTC   .

    APPEND GT_ZPOSEG2 TO GT_ZVIN.

  ENDLOOP.

  CALL SCREEN 0200  STARTING AT 10 10
                   ENDING   AT 110 21.
ENDFORM.
*** INCLUDE ZRPP_HMA_ZPODER_C_F01
*** INCLUDE ZRPP_HMA_ZPODER_C_F01
