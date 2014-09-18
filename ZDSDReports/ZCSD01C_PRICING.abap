************************************************************************
* Program Name      : ZCSD01C_PRICING
* Author            : jun ho choi
* Creation Date     : 2003.08.25.
* Specifications By : jun ho choi
* Pattern           : 2-3
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Uploading Price.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZCSD01C_PRICING NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMSD
                          LINE-SIZE 197.


*
TABLES : USR01.


*
DATA : BEGIN OF IT_UPFILE OCCURS 0,
       RECORD(67),
       END OF IT_UPFILE.

DATA : BEGIN OF BDC_TAB OCCURS 0.
       INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.

DATA : BEGIN OF MESS_TAB OCCURS 0.
       INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESS_TAB.

DATA : BEGIN OF BDC_LIST OCCURS 0,
       GUBUN_P(1),
       GUBUN_M(1),
       MESSAGE(75),

       DATAB(8),
       VKORG(4),
       VTWEG(2),
       SPART(2),
       KUNNR(10),
       MATNR(18),
       KBETR(10),
       MARGN(10),
       KONWA(3),
       END OF BDC_LIST.

DATA : W_CNT TYPE I,
       W_INDEX LIKE SY-TABIX,
       W_DATAB LIKE SY-DATUM,
       W_PACK_D2 TYPE P DECIMALS 2,
       W_PACK_D3 TYPE P DECIMALS 3,
       W_CHAR_10(10),
       WWW(1).


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_FILE LIKE RLGRAP-FILENAME OBLIGATORY
             DEFAULT 'c:\'.
SELECTION-SCREEN END OF BLOCK B1.


*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FETCH_FILENAME.


*
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.


*
START-OF-SELECTION.
  PERFORM UPLOAD_FILE.
  PERFORM BDC_PROCESS.
  PERFORM DISPLAY_RESULT.


*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  FETCH_FILENAME
*&---------------------------------------------------------------------*
FORM FETCH_FILENAME.
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
         DEF_FILENAME     = P_FILE
         DEF_PATH         = 'c:\'
         MASK             = ',*.txt,*.txt.'
         MODE             = 'O'
         TITLE            = TEXT-M01
       IMPORTING
         FILENAME         = P_FILE
       EXCEPTIONS
         INV_WINSYS       = 1
         NO_BATCH         = 2
         SELECTION_CANCEL = 3
         SELECTION_ERROR  = 4.
  IF SY-SUBRC NE 0.
  ENDIF.
ENDFORM.                    " FETCH_FILENAME
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_FILE.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
         CODEPAGE                = ' '
         FILENAME                = P_FILE
         FILETYPE                = 'ASC'
       TABLES
         DATA_TAB                = IT_UPFILE
       EXCEPTIONS
         CONVERSION_ERROR        = 1
         FILE_OPEN_ERROR         = 2
         FILE_READ_ERROR         = 3
         INVALID_TYPE            = 4
         NO_BATCH                = 5
         UNKNOWN_ERROR           = 6
         INVALID_TABLE_WIDTH     = 7
         GUI_REFUSE_FILETRANSFER = 8
         CUSTOMER_ERROR          = 9
         OTHERS                  = 10.
  IF SY-SUBRC NE 0.
    MESSAGE I000 WITH TEXT-M02.
    STOP.
  ENDIF.
ENDFORM.                    " UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DESCRIBE TABLE IT_UPFILE LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M02.
    STOP.
  ENDIF.

  REFRESH : BDC_LIST.
  CLEAR   : BDC_LIST.

  LOOP AT IT_UPFILE.
    REFRESH : BDC_TAB, MESS_TAB.
    CLEAR   : BDC_TAB, MESS_TAB.

    PERFORM MOVE_IT_UPFILE_2_BDC_LIST.

    PERFORM BDC_PRICE_ZP00.

    WWW = 'N'.
    CALL TRANSACTION 'VK11' USING BDC_TAB MODE WWW "'N'
                                  UPDATE 'S'
                                  MESSAGES INTO MESS_TAB.

    PERFORM MESSAGE_ADJUST USING BDC_LIST-GUBUN_P.

    APPEND BDC_LIST.
  ENDLOOP.

  LOOP AT BDC_LIST WHERE GUBUN_P = 'S'.
    W_INDEX = SY-TABIX.

    REFRESH : BDC_TAB, MESS_TAB.
    CLEAR   : BDC_TAB, MESS_TAB.

    PERFORM BDC_PRICE_ZP07.

    WWW = 'N'.
    CALL TRANSACTION 'VK11' USING BDC_TAB MODE WWW "'N'
                                  UPDATE 'S'
                                  MESSAGES INTO MESS_TAB.

    PERFORM MESSAGE_ADJUST USING BDC_LIST-GUBUN_M.

    MODIFY BDC_LIST INDEX W_INDEX.
  ENDLOOP.

ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MOVE_IT_UPFILE_2_BDC_LIST
*&---------------------------------------------------------------------*
FORM MOVE_IT_UPFILE_2_BDC_LIST.
  BDC_LIST-DATAB = IT_UPFILE-RECORD+0(8).
  BDC_LIST-VKORG = IT_UPFILE-RECORD+8(4).
  BDC_LIST-VTWEG = IT_UPFILE-RECORD+12(2).
  BDC_LIST-SPART = IT_UPFILE-RECORD+14(2).
  BDC_LIST-KUNNR = IT_UPFILE-RECORD+16(10).
  BDC_LIST-MATNR = IT_UPFILE-RECORD+26(18).
  BDC_LIST-KBETR = IT_UPFILE-RECORD+44(10).
  BDC_LIST-MARGN = IT_UPFILE-RECORD+54(10).
  BDC_LIST-KONWA = IT_UPFILE-RECORD+64(3).
ENDFORM.                    " MOVE_IT_UPFILE_2_BDC_LIST
*&---------------------------------------------------------------------*
*&      Form  BDC_PRICE_ZP00
*&---------------------------------------------------------------------*
FORM BDC_PRICE_ZP00.
  SELECT SINGLE *
         FROM USR01
        WHERE BNAME = SY-UNAME.
  CASE USR01-DATFM.
    WHEN '1'. "DD.MM.YYYY
      W_DATAB+4(4) = BDC_LIST-DATAB+0(4).
      W_DATAB+2(2) = BDC_LIST-DATAB+4(2).
      W_DATAB+0(2) = BDC_LIST-DATAB+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      W_DATAB+4(4) = BDC_LIST-DATAB+0(4).
      W_DATAB+0(2) = BDC_LIST-DATAB+4(2).
      W_DATAB+2(2) = BDC_LIST-DATAB+6(2).
  ENDCASE.

  W_PACK_D2 = BDC_LIST-KBETR / 100.
  WRITE W_PACK_D2 TO W_CHAR_10.

  PERFORM BDC_FILL USING :
          'X' 'SAPMV13A'             '0100',
          ' ' 'RV13A-KSCHL'          'ZP00',
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPLV14A'             '0100',
          ' ' 'RV130-SELKZ(02)'      'X',
          ' ' 'BDC_OKCODE'           '=WEIT',
          'X' 'SAPMV13A'             '1005',
          ' ' 'KOMG-VKORG'           BDC_LIST-VKORG,
          ' ' 'KOMG-VTWEG'           BDC_LIST-VTWEG,
          ' ' 'KOMG-KUNNR'           BDC_LIST-KUNNR,
          ' ' 'KOMG-MATNR(01)'       BDC_LIST-MATNR,
          ' ' 'KONP-KBETR(01)'       W_CHAR_10,
          ' ' 'KONP-KONWA(01)'       BDC_LIST-KONWA,
          ' ' 'RV13A-DATAB(01)'      W_DATAB,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV13A'             '1005',
          ' ' 'BDC_OKCODE'           '=SICH'.
ENDFORM.                    " BDC_PRICE_ZP00
*&---------------------------------------------------------------------*
*&      Form  BDC_PRICE_ZP07
*&---------------------------------------------------------------------*
FORM BDC_PRICE_ZP07.
  SELECT SINGLE *
         FROM USR01
        WHERE BNAME = SY-UNAME.
  CASE USR01-DATFM.
    WHEN '1'. "DD.MM.YYYY
      W_DATAB+4(4) = BDC_LIST-DATAB+0(4).
      W_DATAB+2(2) = BDC_LIST-DATAB+4(2).
      W_DATAB+0(2) = BDC_LIST-DATAB+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      W_DATAB+4(4) = BDC_LIST-DATAB+0(4).
      W_DATAB+0(2) = BDC_LIST-DATAB+4(2).
      W_DATAB+2(2) = BDC_LIST-DATAB+6(2).
  ENDCASE.

  W_PACK_D3 = BDC_LIST-MARGN / 1000.
  WRITE W_PACK_D2 TO W_CHAR_10.

  PERFORM BDC_FILL USING :
          'X' 'SAPMV13A'             '0100',
          ' ' 'RV13A-KSCHL'          'ZP07',
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV13A'             '1007',
          ' ' 'KOMG-VKORG'           BDC_LIST-VKORG,
          ' ' 'KOMG-VTWEG'           BDC_LIST-VTWEG,
          ' ' 'KOMG-SPART'           BDC_LIST-SPART,
          ' ' 'KOMG-KUNNR901)'       BDC_LIST-KUNNR,
          ' ' 'KONP-KBETR(01)'       W_CHAR_10,
          ' ' 'KONP-KONWA(01)'       BDC_LIST-KONWA,
          ' ' 'RV13A-DATAB(01)'      W_DATAB,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV13A'             '1007',
          ' ' 'BDC_OKCODE'           '=SICH'.
ENDFORM.                    " BDC_PRICE_ZP07
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
FORM BDC_FILL USING    P1 P2 P3.
  CLEAR BDC_TAB.
  IF P1 = 'X'.
     BDC_TAB-DYNBEGIN = P1.
     BDC_TAB-PROGRAM  = P2.
     BDC_TAB-DYNPRO   = P3.
  ELSE.
     BDC_TAB-DYNBEGIN = P1.
     BDC_TAB-FNAM     = P2.
     BDC_TAB-FVAL     = P3.
  ENDIF.
  APPEND BDC_TAB.
ENDFORM.                    " BDC_FILL
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ADJUST
*&---------------------------------------------------------------------*
FORM MESSAGE_ADJUST USING GUBUN.
  CLEAR : BDC_LIST-MESSAGE.

  READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
           MSGID               = MESS_TAB-MSGID
           MSGNR               = MESS_TAB-MSGNR
           MSGV1               = MESS_TAB-MSGV1
           MSGV2               = MESS_TAB-MSGV2
           MSGV3               = MESS_TAB-MSGV3
           MSGV4               = MESS_TAB-MSGV4
         IMPORTING
           MESSAGE_TEXT_OUTPUT = BDC_LIST-MESSAGE.
    GUBUN = 'E'.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'W'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
             MSGID               = MESS_TAB-MSGID
             MSGNR               = MESS_TAB-MSGNR
             MSGV1               = MESS_TAB-MSGV1
             MSGV2               = MESS_TAB-MSGV2
             MSGV3               = MESS_TAB-MSGV3
             MSGV4               = MESS_TAB-MSGV4
           IMPORTING
             MESSAGE_TEXT_OUTPUT = BDC_LIST-MESSAGE.
      GUBUN = 'W'.
    ELSE.
      GUBUN = 'S'.
    ENDIF.
  ENDIF.
ENDFORM.                    " MESSAGE_ADJUST
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM DISPLAY_RESULT.
  LOOP AT BDC_LIST.
    WRITE:/ SY-VLINE.
    CASE BDC_LIST-GUBUN_P.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) BDC_LIST-GUBUN_P.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'W'.
        FORMAT COLOR COL_GROUP.
        WRITE: (02) BDC_LIST-GUBUN_P.
        FORMAT COLOR COL_GROUP OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) BDC_LIST-GUBUN_P.
        FORMAT COLOR COL_NEGATIVE OFF.
    ENDCASE.

    WRITE:  SY-VLINE.
    CASE BDC_LIST-GUBUN_M.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) BDC_LIST-GUBUN_M.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'W'.
        FORMAT COLOR COL_GROUP.
        WRITE: (02) BDC_LIST-GUBUN_M.
        FORMAT COLOR COL_GROUP OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) BDC_LIST-GUBUN_M.
        FORMAT COLOR COL_NEGATIVE OFF.
    ENDCASE.

    W_DATAB = BDC_LIST-DATAB.
    W_PACK_D2 = BDC_LIST-KBETR / 100.
    W_PACK_D3 = BDC_LIST-MARGN / 1000.

    WRITE:  SY-VLINE, (10) W_DATAB,
            SY-VLINE, (04) BDC_LIST-VKORG,
            SY-VLINE, (04) BDC_LIST-VTWEG,
            SY-VLINE, (04) BDC_LIST-SPART,
            SY-VLINE, (10) BDC_LIST-KUNNR,
            SY-VLINE, (18) BDC_LIST-MATNR,
            SY-VLINE, (13) W_PACK_D2,
            SY-VLINE, (13) W_PACK_D3,
            SY-VLINE, (04) BDC_LIST-KONWA,
            SY-VLINE, (75) BDC_LIST-MESSAGE,
            SY-VLINE.
    WRITE:/(196) SY-ULINE.
  ENDLOOP.

ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  FORMAT COLOR COL_HEADING.
  WRITE:/(196) SY-ULINE.
  WRITE:/ SY-VLINE, (02) 'Pr',
          SY-VLINE, (02) 'Mg',
          SY-VLINE, (10) 'Start date',
          SY-VLINE, (04) 'SOrg',
          SY-VLINE, (04) 'DChn',
          SY-VLINE, (04) 'Div.',
          SY-VLINE, (10) 'Customer',
          SY-VLINE, (18) 'Material',
          SY-VLINE, (13) 'Price',
          SY-VLINE, (13) 'Margin',
          SY-VLINE, (04) 'CURR',
          SY-VLINE, (75) 'Message',
          SY-VLINE.
  WRITE:/(196) SY-ULINE.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
