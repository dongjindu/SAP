************************************************************************
* Program Name      : Production and Sales Result by FSC to HMC
* Creation Date     : 09/15/2009
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZIPP_GLBP_PROD_SALES NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

DATA: IT_DATA LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE,
      IT_GLBP LIKE TABLE OF ZTPP_GLBP WITH HEADER LINE.

CONSTANTS:  C_PROD LIKE RLGRAP-FILENAME VALUE
                      '/usr/sap/EDI_SAP/GLBP_HMMA_PROD_',
           C_SALES LIKE RLGRAP-FILENAME VALUE
                      '/usr/sap/EDI_SAP/GLBP_HMMA_SALES_'.

CONSTANTS: C_DEST(10) VALUE 'WMPP01'.

DATA: W_FILENAME LIKE RLGRAP-FILENAME.

DATA: BEGIN OF IT_OUT OCCURS 0,
      GLBP(42),
      END OF IT_OUT.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_YYMM LIKE ZTPP_GLBP-YYMM.
SELECTION-SCREEN SKIP.
PARAMETERS: P_FILE RADIOBUTTON GROUP GRP1.
PARAMETERS: P_EAI DEFAULT 'X' RADIOBUTTON GROUP GRP1.   "NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM INIT_DATA.

START-OF-SELECTION.

  PERFORM GET_DATA.
  IF IT_GLBP[] IS INITIAL.
    MESSAGE I001 WITH 'No Data'.
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
  DATA : BEGIN OF LT_DATA OCCURS 0,
        NATN LIKE ZTPP_PROD_ACTUAL-NATN,
        MODEL LIKE ZTPP_PROD_ACTUAL-MODEL,
        BMDL LIKE ZTPP_PROD_ACTUAL-BMDL,
        QTY_SIGNOFF LIKE ZTPP_PROD_ACTUAL-QTY_SIGNOFF,
        QTY_CGATE LIKE ZTPP_PROD_ACTUAL-QTY_CGATE,
                QTY_SHIPOUT LIKE ZTPP_PROD_ACTUAL-QTY_SHIPOUT,
         END OF LT_DATA.

  DATA: L_DATE_C(8),
        L_FRDATE LIKE SY-DATUM,
         L_TODATE LIKE SY-DATUM,
        L_ZERO(8) VALUE '0000000',
        L_STOCK(8),
        L_CN TYPE I.
  RANGES: R_AAAB FOR ZTPP_GLBP-NATN.

  CONCATENATE P_YYMM '01' INTO L_DATE_C.
  L_FRDATE = L_DATE_C.
  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = L_FRDATE
       IMPORTING
            LAST_DAY_OF_MONTH = L_TODATE
       EXCEPTIONS
            DAY_IN_NO_DATE    = 1
            OTHERS            = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  R_AAAB-SIGN = 'I'.
  R_AAAB-OPTION = 'CP'.
  R_AAAB-LOW = '+++AA'.
  APPEND R_AAAB.
  R_AAAB-SIGN = 'I'.
  R_AAAB-OPTION = 'CP'.
  R_AAAB-LOW = '+++AB'.
  APPEND R_AAAB.

  SELECT NATN MODEL BMDL SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
       SUM( QTY_CGATE ) AS QTY_CGATE
       SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
       INTO CORRESPONDING FIELDS OF
       TABLE LT_DATA
       FROM ZTPP_PROD_ACTUAL
       WHERE PRDT_DATE BETWEEN L_FRDATE AND L_TODATE
         AND NATN IN R_AAAB
     GROUP BY NATN MODEL BMDL.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

*  DELETE LT_DATA WHERE QTY_SIGNOFF EQ '0'.

  LOOP AT LT_DATA.
    IT_GLBP-YYMM = P_YYMM.
    IT_GLBP-PLANT = 'HMMA'.
    IT_GLBP-PLANT_NO = '1'.
    IT_GLBP-NATN = LT_DATA-NATN.
    IT_GLBP-MODEL = LT_DATA-MODEL.
    IT_GLBP-BMDL = LT_DATA-BMDL.
    IT_GLBP-QTY_SIGNOFF = LT_DATA-QTY_SIGNOFF.
    IF IT_GLBP-NATN+0(3) = 'B28'.
      IT_GLBP-DOMEXP = 'D'.
      IT_GLBP-QTY_CGATE = LT_DATA-QTY_CGATE.
      IT_GLBP-QTY_SHIPOUT = 0.
    ELSE.
      IT_GLBP-DOMEXP = 'E'.
      IT_GLBP-QTY_CGATE = 0.
      IT_GLBP-QTY_SHIPOUT = LT_DATA-QTY_SHIPOUT.
    ENDIF.
    IT_GLBP-CRUSER = SY-UNAME.
    IT_GLBP-CRDATE = SY-DATUM.
    IT_GLBP-CRTIME = SY-UZEIT.
    APPEND IT_GLBP.
  ENDLOOP.

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
        L_MSGTXT(100).

  DATA: L_TOTREC TYPE I.

  DATA: LT_PROD LIKE TABLE OF ZSPP_GLBP WITH HEADER LINE,
       LT_SALES LIKE TABLE OF ZSPP_GLBP WITH HEADER LINE.

  DESCRIBE TABLE IT_GLBP LINES L_TOTREC.

  IF P_FILE IS INITIAL AND  P_EAI IS INITIAL.
    DELETE FROM ZTPP_GLBP WHERE YYMM = P_YYMM.
    INSERT ZTPP_GLBP FROM TABLE IT_GLBP.

    IF SY-SUBRC = 0.
      COMMIT WORK.
      WRITE: 'Total record number(s) are : ', L_TOTREC,
            'were saved successfully'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE I000 WITH 'Table Saving Error'.
    ENDIF.
    EXIT.
  ENDIF.

  LOOP AT IT_GLBP.
    MOVE-CORRESPONDING IT_GLBP TO LT_PROD.
    MOVE-CORRESPONDING IT_GLBP TO LT_SALES.
    LT_PROD-QTY = IT_GLBP-QTY_SIGNOFF.
    SHIFT LT_PROD-QTY LEFT DELETING LEADING SPACE.
    APPEND LT_PROD.
    IF IT_GLBP-NATN+0(3) = 'B28'.
      LT_SALES-QTY = IT_GLBP-QTY_CGATE.
    ELSE.
      LT_SALES-QTY = IT_GLBP-QTY_SHIPOUT.
    ENDIF.
    SHIFT LT_SALES-QTY LEFT DELETING LEADING SPACE.
    APPEND LT_SALES.
  ENDLOOP.

  IF  NOT P_FILE IS INITIAL.

** Send production data
    CONCATENATE C_PROD P_YYMM '_V01.txt'
          INTO W_FILENAME.

    OPEN DATASET W_FILENAME IN TEXT MODE FOR OUTPUT.
    IF SY-SUBRC <> 0.
      MESSAGE E000 WITH 'Error: Open data file'.
    ENDIF.

    LOOP AT LT_PROD.
      OPEN DATASET W_FILENAME IN TEXT MODE FOR APPENDING.
      TRANSFER LT_PROD TO W_FILENAME.
    ENDLOOP.

    CLOSE DATASET W_FILENAME.

    IF SY-SUBRC = 0.
      WRITE: / 'Total Production Records Are : ', L_TOTREC,
             'Were Downloaded successfully'.
      LOOP AT IT_GLBP.
        IT_GLBP-TRDATE = SY-DATUM.
        IT_GLBP-PROD_FLAG = 'S'.
        MODIFY IT_GLBP.
      ENDLOOP.

    ELSE.
   WRITE: / 'Download Failed, Total Production Records Are: ', L_TOTREC.
      LOOP AT IT_GLBP.
        IT_GLBP-TRDATE = SY-DATUM.
        IT_GLBP-PROD_FLAG = 'E'.
        MODIFY IT_GLBP.
      ENDLOOP.
    ENDIF.

** Send sales data
    CONCATENATE C_SALES P_YYMM '_V01.txt'
            INTO W_FILENAME.

    OPEN DATASET W_FILENAME IN TEXT MODE FOR OUTPUT.
    IF SY-SUBRC <> 0.
      MESSAGE E000 WITH 'Error: Open data file'.
    ENDIF.

    LOOP AT LT_SALES.
      OPEN DATASET W_FILENAME IN TEXT MODE FOR APPENDING.
      TRANSFER LT_SALES TO W_FILENAME.
    ENDLOOP.

    CLOSE DATASET W_FILENAME.

    IF SY-SUBRC = 0.
      WRITE: / 'Total Sales Record Are : ', L_TOTREC,
             'Were Downloaded successfully'.
      LOOP AT IT_GLBP.
        IT_GLBP-SALES_FLAG = 'S'.
        MODIFY IT_GLBP.
      ENDLOOP.

    ELSE.
      WRITE: / 'Download Failed, Total Sales Records Are: ', L_TOTREC.
      LOOP AT IT_GLBP.
        IT_GLBP-SALES_FLAG = 'E'.
        MODIFY IT_GLBP.
      ENDLOOP.
    ENDIF.

    DELETE FROM ZTPP_GLBP WHERE YYMM = P_YYMM.
    INSERT ZTPP_GLBP FROM TABLE IT_GLBP.
  ENDIF.

  IF  NOT P_EAI IS INITIAL.

** Send production data
    CALL FUNCTION 'Z_FPP_GLBP_PROD'
        DESTINATION C_DEST
        IMPORTING
          FLAG          = L_RESULT
        TABLES
          I_GLBP  = LT_PROD
        EXCEPTIONS
               COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
               SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.
    IF SY-SUBRC = 0.
      WRITE: / 'Total Production Record Number(s) Are : ', L_TOTREC,
             'Sent successfully'.
      LOOP AT IT_GLBP.
        IT_GLBP-TRDATE = SY-DATUM.
        IT_GLBP-PROD_FLAG = 'S'.
        MODIFY IT_GLBP.
      ENDLOOP.
    ELSE.
      WRITE: / 'EAI Failed, Total Production Data Are: ', L_TOTREC.
      LOOP AT IT_GLBP.
        IT_GLBP-TRDATE = SY-DATUM.
        IT_GLBP-PROD_FLAG = 'E'.
        MODIFY IT_GLBP.
      ENDLOOP.
    ENDIF.

** Send sales data

    CALL FUNCTION 'Z_FPP_GLBP_SALES'
          DESTINATION C_DEST
          IMPORTING
            FLAG          = L_RESULT
          TABLES
            I_GLBP  = LT_SALES
          EXCEPTIONS
                 COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
                 SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.
    IF SY-SUBRC = 0.
      WRITE: / 'Total Sales Record Number(s) Are : ', L_TOTREC,
             'Sent successfully'.
      LOOP AT IT_GLBP.
        IT_GLBP-SALES_FLAG = 'S'.
        MODIFY IT_GLBP.
      ENDLOOP.
    ELSE.
      WRITE: / 'EAI Failed, Total Sales Data Are: ', L_TOTREC.
      LOOP AT IT_GLBP.
        IT_GLBP-SALES_FLAG = 'E'.
        MODIFY IT_GLBP.
      ENDLOOP.
    ENDIF.

    DELETE FROM ZTPP_GLBP WHERE YYMM = P_YYMM.
    INSERT ZTPP_GLBP FROM TABLE IT_GLBP.

  ENDIF.
ENDFORM.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  DATA: L_MONTH(2) TYPE N,
        L_YEAR(4) TYPE N.

  L_MONTH = SY-DATUM+4(2).
  L_YEAR = SY-DATUM+0(4).
  IF L_MONTH = '01'.
    L_YEAR = L_YEAR - 1.
    CONCATENATE L_YEAR '01' INTO P_YYMM.
  ELSE.
    L_MONTH = L_MONTH - 1.
    CONCATENATE L_YEAR L_MONTH INTO P_YYMM.
  ENDIF.
ENDFORM.                    " INIT_DATA
