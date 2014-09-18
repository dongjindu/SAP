************************************************************************
* Program Name      : ZPPR101_VIN_219
* Author            : Furong Wang
* Creation Date     : 12/21/2005
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Get 219 code from work order master
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZPPR101_VIN_219 NO STANDARD PAGE HEADING LINE-SIZE 255
                       MESSAGE-ID ZMPP.

*---// Constants
CONSTANTS: C_DIR_NAME LIKE EPSF-EPSDIRNAM
                      VALUE '/usr/sap/EDI_SAP/',
           C_FILENAME LIKE RLGRAP-FILENAME VALUE
                      '/usr/sap/EDI_SAP/vin_spec_'.

*---// For FTP file creation
DATA: W_FILENAME LIKE RLGRAP-FILENAME.

DATA: IT_AUSP LIKE TABLE OF AUSP WITH HEADER LINE.
DATA: BEGIN OF IT_OUT OCCURS 0,
      VIN(17),
      CODE219(219),
      END OF IT_OUT.

DATA  C_COUNT TYPE I.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
*PARAMETERS: p_datum LIKE sy-datum.
SELECT-OPTIONS: S_DATUM FOR SY-DATUM OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BL1.

START-OF-SELECTION.
  PERFORM READ_DATA.
  DESCRIBE TABLE IT_OUT LINES C_COUNT.
  IF C_COUNT > 0.
    PERFORM SEND_RTN.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  SEND_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_RTN.
  PERFORM SEND_BY_FTP.
*  PERFORM update_table.
ENDFORM.                    " SEND_RTN
*&---------------------------------------------------------------------*
*&      Form  send_by_FTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM SEND_BY_FTP.
  PERFORM WRITE_FILE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM write_file                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM WRITE_FILE.
  DATA: L_CHAR(8),
        L_TOTREC TYPE I.

  L_CHAR = S_DATUM-LOW.

  CONCATENATE C_FILENAME L_CHAR '.txt'
         INTO W_FILENAME.

  OPEN DATASET W_FILENAME IN TEXT MODE FOR OUTPUT.
  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH TEXT-M12.
  ENDIF.

  LOOP AT IT_OUT.
    OPEN DATASET W_FILENAME IN TEXT MODE FOR APPENDING.
    TRANSFER IT_OUT TO W_FILENAME.
  ENDLOOP.

  CLOSE DATASET W_FILENAME.

  DESCRIBE TABLE IT_OUT LINES L_TOTREC.

  IF SY-SUBRC = 0.
    WRITE: 'Total record number are : ', L_TOTREC,
           'were downloaded successfully'.
  ELSE.
    WRITE: 'Download fail, total records are: ', L_TOTREC.
  ENDIF.

  CLEAR: IT_OUT, IT_OUT[].

ENDFORM.                    " WRITE_FILE

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  DATA: L_ATFLV LIKE AUSP-ATFLV,
        L_NO(03) TYPE N,
        L_MATNR LIKE MARA-MATNR,
        L_ATINN LIKE AUSP-ATINN,
        L_NAME(30) TYPE C,
        L_219(219),
        L_NUM(08),
        L_BIT(1).

  DATA: L_RP_SHOP_DATE LIKE CABN-ATNAM.

  DATA: LT_AUSP_219 LIKE TABLE OF AUSP WITH HEADER LINE,
        LT_CABN LIKE TABLE OF CABN WITH HEADER LINE,
        L_CONF LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  DATA: BEGIN OF LT_VIN OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        ATWRT LIKE AUSP-ATWRT,
        END OF LT_VIN.

  DATA: BEGIN OF LT_WORDER OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        ATWRT LIKE AUSP-ATWRT,
        END OF LT_WORDER.

  DATA: BEGIN OF LT_AUSP_219VAL OCCURS 0,
         OBJEK LIKE AUSP-OBJEK,
         ATNAM LIKE CABN-ATNAM,
         ATINN LIKE AUSP-ATINN,
         ATWRT LIKE AUSP-ATWRT,
         END OF LT_AUSP_219VAL.

  DATA: BEGIN OF LT_AUSP OCCURS 0,
         ATNAM LIKE CABN-ATNAM,
         ATINN LIKE AUSP-ATINN,
         END OF LT_AUSP.

  RANGES: S_ATFLV FOR AUSP-ATFLV.

  L_RP_SHOP_DATE = 'P_RP18_SHOP_DATE'.

  L_NUM = S_DATUM-LOW.
  S_ATFLV-LOW = L_NUM.
  L_NUM = S_DATUM-HIGH.
  S_ATFLV-HIGH = L_NUM.
  S_ATFLV-SIGN = S_DATUM-SIGN.
  S_ATFLV-OPTION = S_DATUM-OPTION.
  APPEND S_ATFLV.

  SELECT * INTO TABLE LT_CABN
   FROM CABN
   CLIENT SPECIFIED.

  SORT LT_CABN BY ATNAM.

  READ TABLE LT_CABN WITH KEY ATNAM = L_RP_SHOP_DATE.

  SELECT DISTINCT * INTO TABLE IT_AUSP
    FROM AUSP
    WHERE ATINN = LT_CABN-ATINN
     AND ATFLV IN S_ATFLV
     AND KLART = '002'.
*    AND ATWRT = '18'.

  IF IT_AUSP[] IS INITIAL.
    MESSAGE I000 WITH TEXT-M01.
    EXIT.
  ENDIF.
  READ TABLE LT_CABN WITH KEY ATNAM = 'P_WORK_ORDER'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_WORDER
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.

  READ TABLE LT_CABN WITH KEY ATNAM = 'P_VIN'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_VIN
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.

  CLEAR L_NO.
  DO  9 TIMES.
    CLEAR: L_BIT.
    L_NO = L_NO + 1.
    CONCATENATE 'P_219_' L_NO+2(1)   INTO LT_AUSP-ATNAM.

*       READ TABLE lt_cabn WITH KEY atnam = lt_ausp-atnam..
*       lt_ausp-atinn = lt_cabn-atinn.
    APPEND LT_AUSP.
    CLEAR: LT_AUSP.
  ENDDO.

  DO 90 TIMES.
    CLEAR: L_BIT.
    L_NO = L_NO + 1.
    CONCATENATE 'P_219_' L_NO+1(2)   INTO LT_AUSP-ATNAM.

*      READ TABLE lt_cabn WITH KEY atnam = lt_ausp-atnam..
*      lt_ausp-atinn = lt_cabn-atinn.
    APPEND LT_AUSP.
    CLEAR: LT_AUSP.

  ENDDO.

  DO 120 TIMES.
    CLEAR: L_BIT.
    L_NO = L_NO + 1.
    CONCATENATE 'P_219_' L_NO   INTO LT_AUSP-ATNAM.
*      READ TABLE lt_cabn WITH KEY atnam = lt_ausp-atnam.
*
*      lt_ausp-atinn = lt_cabn-atinn.
    APPEND LT_AUSP.
    CLEAR: LT_AUSP.
  ENDDO.

  LOOP AT IT_AUSP.
    CLEAR: L_MATNR,L_NO,L_NAME.

    READ TABLE LT_WORDER WITH KEY OBJEK = IT_AUSP-OBJEK.
    L_MATNR = LT_WORDER-ATWRT.

*    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*         EXPORTING
*              object       = l_matnr
*              ctype        = '001'
*         TABLES
*              val_table    = l_conf
*         EXCEPTIONS
*              no_data      = 1
*              error_mode   = 2
*              error_object = 3
*              error_value  = 4
*              OTHERS       = 5.


    SELECT OBJEK ATNAM A~ATINN ATWRT INTO TABLE LT_AUSP_219VAL
    FROM AUSP AS A
     INNER JOIN CABN AS B
     ON A~ATINN = B~ATINN
     FOR ALL ENTRIES IN LT_AUSP
     WHERE OBJEK = L_MATNR
       AND KLART = '001'
       AND B~ATNAM = LT_AUSP-ATNAM.

    DO  9 TIMES.
      CLEAR: L_BIT.
      L_NO = L_NO + 1.
      CONCATENATE 'P_219_' L_NO+2(1)   INTO L_NAME.

*      READ TABLE lt_cabn WITH KEY atnam = l_name.

      READ TABLE LT_AUSP_219VAL WITH KEY ATNAM = L_NAME.
      IF SY-SUBRC  NE 0.
        L_BIT = '*'.
      ELSE.
        L_BIT = LT_AUSP_219VAL-ATWRT.
      ENDIF.

      CONCATENATE L_219 L_BIT INTO L_219.
    ENDDO.

    DO 90 TIMES.
      CLEAR: L_BIT.
      L_NO = L_NO + 1.
      CONCATENATE 'P_219_' L_NO+1(2)   INTO L_NAME.

*      READ TABLE lt_cabn WITH KEY atnam = l_name.

      READ TABLE LT_AUSP_219VAL WITH KEY ATNAM = L_NAME..
      IF SY-SUBRC  NE 0.
        L_BIT = '*'.
      ELSE.
        L_BIT = LT_AUSP_219VAL-ATWRT.
      ENDIF.

      CONCATENATE L_219 L_BIT INTO L_219.
    ENDDO.

    DO 120 TIMES.
      CLEAR: L_BIT.
      L_NO = L_NO + 1.
      CONCATENATE 'P_219_' L_NO   INTO L_NAME.
*      READ TABLE lt_cabn WITH KEY atnam = l_name.

      READ TABLE LT_AUSP_219VAL WITH KEY ATNAM = L_NAME.
      IF SY-SUBRC  NE 0.
        L_BIT = '*'.
      ELSE.
        L_BIT = LT_AUSP_219VAL-ATWRT.
      ENDIF.
      CONCATENATE L_219 L_BIT INTO L_219.
    ENDDO.

*   it_out-vin = it_ausp-objek.

    READ TABLE LT_VIN WITH KEY OBJEK = IT_AUSP-OBJEK.
    IT_OUT-VIN = LT_VIN-ATWRT.

    IT_OUT-CODE219 = L_219.
    APPEND IT_OUT.

    CLEAR: L_219, IT_OUT, IT_AUSP, LT_VIN, LT_WORDER, LT_AUSP_219VAL[].

  ENDLOOP.

ENDFORM.                    " READ_DATA
