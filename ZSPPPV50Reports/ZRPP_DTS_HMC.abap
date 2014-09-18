************************************************************************
* Program Name      : ZRPP_DTS_HMC
* Creation Date     : 06/04/2009
* Development Request No :
* Addl Documentation:
* Description       : Send VPC Stock toHMC DTS
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZRPP_DTS_HMC NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS, VRM.
DATA: IT_DATA LIKE TABLE OF ZTPP_ACT_HMC WITH HEADER LINE,
      IT_DTS_HMC LIKE TABLE OF ZTPP_DTS_HMC WITH HEADER LINE.

CONSTANTS: C_DIR_NAME LIKE EPSF-EPSDIRNAM
                      VALUE '/usr/sap/EDI_SAP/',
*           c_filename LIKE rlgrap-filename VALUE
*                      '/usr/sap/EDI_SAP/DTS_HMC_'.
           C_FILENAME LIKE RLGRAP-FILENAME VALUE
                      '/usr/sap/EDI_SAP/SUM_'.

CONSTANTS: C_DEST(10) VALUE 'WMPP01'.

*---// For FTP file creation
DATA: W_FILENAME LIKE RLGRAP-FILENAME.

DATA: BEGIN OF IT_OUT OCCURS 0,
      DTS(83),
      END OF IT_OUT.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_DATUM LIKE SY-DATUM.
PARAMETERS: P_FILE(1)  NO-DISPLAY.
PARAMETERS: P_EAI AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  PERFORM GET_DATA.
  IF IT_DTS_HMC[] IS INITIAL.
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
  DATA : BEGIN OF LT_DATA OCCURS 0,
         DEST LIKE ZTPP_ACT_HMC-DEST,
         BMDL LIKE ZTPP_ACT_HMC-BMDL,
         STOCK TYPE ZSTOCK_QTY,
         END OF LT_DATA.
  DATA: L_DATE_C(8),
        L_DATE_C_1(8),
        L_ZERO(8) VALUE '0000000',
        L_STOCK(8),
        L_CN TYPE I.

* by ig.moon 6/11/2009 {
*  SELECT DEST BMDL
*  SUM( STOCK ) AS STOCK INTO CORRESPONDING FIELDS OF TABLE LT_DATA
*     FROM ZTPP_ACT_HMC
*     WHERE PRDT_DATE = P_DATUM
*     GROUP BY  DEST BMDL.

  SELECT DEST BMDL STOCK INTO CORRESPONDING FIELDS OF TABLE LT_DATA
     FROM ZTPP_ACT_HMC
     WHERE PRDT_DATE = P_DATUM.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  DATA $LT_DATA LIKE LT_DATA OCCURS 0 WITH HEADER LINE.

  LOOP AT LT_DATA.
    MOVE LT_DATA TO $LT_DATA.
    IF $LT_DATA-DEST+3(2) NE 'XA'.
      WRITE 'AM' TO $LT_DATA-DEST+3.
    ENDIF.
    COLLECT $LT_DATA.
  ENDLOOP.

  CLEAR : LT_DATA, LT_DATA[].
  LT_DATA[] = $LT_DATA[].

  DELETE LT_DATA WHERE STOCK EQ '0'.
* }


  L_DATE_C = P_DATUM.
  L_DATE_C_1 = SY-DATUM.

  LOOP AT LT_DATA.
    IF LT_DATA-STOCK < 0.
      MESSAGE I002 WITH 'Negative Stock :' LT_DATA-DEST LT_DATA-BMDL.
      LEAVE PROGRAM.
    ENDIF.
    L_STOCK = LT_DATA-STOCK.
    CONDENSE L_STOCK.
    L_CN = STRLEN( L_STOCK ).

    IF L_CN < 7.
      L_CN = 7 - STRLEN( L_STOCK ).
      DO L_CN TIMES.
        CONCATENATE '0' L_STOCK INTO L_STOCK.
      ENDDO.
    ENDIF.
    CONCATENATE LT_DATA-DEST+0(3) 'AM' L_DATE_C '5N'  LT_DATA-BMDL
        INTO IT_OUT.
    CONCATENATE IT_OUT ';' L_ZERO L_ZERO L_ZERO L_STOCK
        INTO IT_OUT.
    CONCATENATE IT_OUT ';' L_ZERO L_ZERO L_ZERO L_DATE_C_1
        INTO IT_OUT.
    APPEND IT_OUT.

    MOVE-CORRESPONDING LT_DATA TO IT_DTS_HMC.
    IT_DTS_HMC-SOURCE = '5N'.
    IT_DTS_HMC-PRDT_DATE = P_DATUM.
    IT_DTS_HMC-CRUSER = SY-UNAME.
    IT_DTS_HMC-CRDATE = SY-DATUM.
    IT_DTS_HMC-CRTIME = SY-UZEIT.
    APPEND IT_DTS_HMC.

    CLEAR:  IT_OUT, IT_DTS_HMC.
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

* by ig.moon 6/11/2009 {
  DATA : BEGIN OF IT_DEST OCCURS 0,
            DEST TYPE ZDEST,
         END OF IT_DEST.

  LOOP AT IT_DTS_HMC.
    IT_DEST-DEST = IT_DTS_HMC-DEST.
    COLLECT IT_DEST.
  ENDLOOP.

* }

  DATA: L_CHAR(8),
 L_TOTREC TYPE I.

  DESCRIBE TABLE IT_DTS_HMC LINES L_TOTREC.

** Chnaged by Furong on 06/17/09
  IF P_FILE IS INITIAL AND  P_EAI IS INITIAL.

    DELETE FROM ZTPP_DTS_HMC WHERE PRDT_DATE = P_DATUM.
    INSERT ZTPP_DTS_HMC FROM TABLE IT_DTS_HMC.

    IF SY-SUBRC = 0.
      COMMIT WORK.
      WRITE: 'Total record number(s) are : ', L_TOTREC,
            'were saved successfully'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE I000 WITH 'Table saving error'.
    ENDIF.
*  ELSE.
  ENDIF.

  IF  NOT P_FILE IS INITIAL.
    L_CHAR = P_DATUM.

* by ig.moon 6/11/2009 {

    LOOP AT IT_DEST.

*      CONCATENATE c_filename l_char '.txt'
*            INTO w_filename.

      CONCATENATE C_FILENAME IT_DEST-DEST '_' L_CHAR '.txt'
            INTO W_FILENAME.

      OPEN DATASET W_FILENAME IN TEXT MODE FOR OUTPUT.
      IF SY-SUBRC <> 0.
        MESSAGE E000 WITH 'Error: Open data file'.
      ENDIF.


* by ig.moon 6/11/2009 { -
*      LOOP AT it_out.
* }
      LOOP AT IT_OUT.
        CHECK IT_OUT-DTS(5) EQ IT_DEST-DEST.
        OPEN DATASET W_FILENAME IN TEXT MODE FOR APPENDING.
        TRANSFER IT_OUT TO W_FILENAME.
      ENDLOOP.

      CLOSE DATASET W_FILENAME.

    ENDLOOP.

    IF SY-SUBRC = 0.
      WRITE: / 'Total record number(s) are : ', L_TOTREC,
             'were downloaded successfully'.
      LOOP AT IT_DTS_HMC.
        IT_DTS_HMC-TRDATE = SY-DATUM.
        IT_DTS_HMC-FLAG = 'S'.
        MODIFY IT_DTS_HMC.
      ENDLOOP.

      DELETE FROM ZTPP_DTS_HMC WHERE PRDT_DATE = P_DATUM.
      INSERT ZTPP_DTS_HMC FROM TABLE IT_DTS_HMC.

    ELSE.
      WRITE: / 'Download fail, total records are: ', L_TOTREC.
      LOOP AT IT_DTS_HMC.
        IT_DTS_HMC-TRDATE = SY-DATUM.
        IT_DTS_HMC-FLAG = 'E'.
        MODIFY IT_DTS_HMC.
      ENDLOOP.

      DELETE FROM ZTPP_DTS_HMC WHERE PRDT_DATE = P_DATUM.
      INSERT ZTPP_DTS_HMC FROM TABLE IT_DTS_HMC.


    ENDIF.
  ENDIF.

  IF  NOT P_EAI IS INITIAL.
    CALL FUNCTION 'Z_FPP_DTS_HMC'
        DESTINATION C_DEST
        IMPORTING
          FLAG          = L_RESULT
        TABLES
          I_DTS_HMC  = IT_DTS_HMC
        EXCEPTIONS
               COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
               SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.
    IF SY-SUBRC = 0.
      WRITE: / 'Total record number(s) are : ', L_TOTREC,
             'were downloaded successfully'.
      LOOP AT IT_DTS_HMC.
        IT_DTS_HMC-TRDATE = SY-DATUM.
        IT_DTS_HMC-FLAG = 'S'.
        MODIFY IT_DTS_HMC.
      ENDLOOP.

      DELETE FROM ZTPP_DTS_HMC WHERE PRDT_DATE = P_DATUM.
      INSERT ZTPP_DTS_HMC FROM TABLE IT_DTS_HMC.

    ELSE.
      WRITE: / 'Download fail, total records are: ', L_TOTREC.
      LOOP AT IT_DTS_HMC.
        IT_DTS_HMC-TRDATE = SY-DATUM.
        IT_DTS_HMC-FLAG = 'E'.
        MODIFY IT_DTS_HMC.
      ENDLOOP.

      DELETE FROM ZTPP_DTS_HMC WHERE PRDT_DATE = P_DATUM.
      INSERT ZTPP_DTS_HMC FROM TABLE IT_DTS_HMC.
    ENDIF.
  ENDIF.
ENDFORM.                    " write_data
