************************************************************************
* Program Name      : ZISD07U_MOBIS_ORDER_CONF
* Author            : jun ho choi
* Creation Date     : 2003.08.14.
* Specifications By : jun ho choi
* Pattern           : 5-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Order confirmation is sent back to MOBIS.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*01/18/2005   100471      UD1K913873   MOBIS file name change.
*02/23/2005   100471      UD1K914601   Add extension '.txt'.
*
************************************************************************
REPORT ZISD07U_MOBIS_ORDER_CONF NO STANDARD PAGE HEADING
                                MESSAGE-ID ZMSD.


*
TABLES : VBAK,
         VBAP,
         VBKD,
         VBEP,
         MARA,
         VBPA.


*
DATA : BEGIN OF IT_HEADER OCCURS 0,
       RECORD(26),
       END OF IT_HEADER.

DATA : BEGIN OF IT_DOWNFILE OCCURS 0,
       RECORD(134),
       END OF IT_DOWNFILE.

DATA : BEGIN OF IT_VBAK OCCURS 0.
        INCLUDE STRUCTURE VBAK.
DATA : END OF IT_VBAK.

DATA : BEGIN OF IT_VBAP OCCURS 0.
        INCLUDE STRUCTURE VBAP.
DATA : END OF IT_VBAP.

DATA : BEGIN OF IT_VBKD OCCURS 0.
        INCLUDE STRUCTURE VBKD.
DATA : END OF IT_VBKD.

DATA : BEGIN OF IT_VBEP OCCURS 0.
        INCLUDE STRUCTURE VBEP.
DATA : END OF IT_VBEP.

DATA : BEGIN OF IT_VBPA OCCURS 0.
        INCLUDE STRUCTURE VBPA.
DATA : END OF IT_VBPA.

DATA : ITFTEXT TYPE TLINE OCCURS 0 WITH HEADER LINE.

DATA : W_CNT TYPE I,
       W_NUMC_7(7) TYPE N,
       W_NUMC_4(4) TYPE N,
       W_CHAR_11(11),
       W_NUMC_11(11) TYPE N,
       W_CHAR_9(9),
       W_NUMC_9(9) TYPE N,
       W_DSN(90), " VALUE '/usr/sap/EDI_SAP/',
       W_NAME LIKE THEAD-TDNAME,
       W_TOT_PIECES(9) TYPE N,
       W_TOT_ITEMS(7) TYPE N,
       W_TOT_VOLUM(7) TYPE N,
       W_TOT_NTGEW(7) TYPE N,
       W_ERR(1),
       W_ORG LIKE VBAP-MATNR.

DATA : SELECTFIELD   LIKE  HELP_INFO-FIELDNAME,
       X_FIELDS      LIKE  HELP_VALUE OCCURS 0 WITH HEADER LINE,
       SELECT_VALUE  LIKE  HELP_INFO-FLDVALUE,
       ID_TABIX      LIKE  SY-TABIX.
DATA : BEGIN OF X_BSTNK OCCURS 0,
       BSTNK LIKE VBAK-BSTNK,
       ERDAT LIKE VBAK-ERDAT,
       ERNAM LIKE VBAK-ERNAM,
       END OF X_BSTNK.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_BSTNK LIKE VBAK-BSTNK OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.


*
INITIALIZATION.


*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_BSTNK.
  PERFORM GET_BSTNK.


*
START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM MODIFY_DATA.
  PERFORM DOWNLOAD_DATA.


*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  GET_BSTNK
*&---------------------------------------------------------------------*
FORM GET_BSTNK.
  DATA W_SDATE LIKE SY-DATUM.

  CLEAR   : SELECTFIELD, X_FIELDS, SELECT_VALUE, ID_TABIX, X_BSTNK.
  REFRESH : X_FIELDS, X_BSTNK.

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
       EXPORTING
            MONTHS  = -2
            OLDDATE = SY-DATUM
       IMPORTING
            NEWDATE = W_SDATE.

  SELECT  BSTNK ERDAT ERNAM
  INTO CORRESPONDING FIELDS OF TABLE X_BSTNK
  FROM    VBAK
  WHERE   AUART EQ 'ZPSO'
  AND     ERDAT GE W_SDATE
  AND     ERDAT LE SY-DATUM.

  X_FIELDS-TABNAME = 'VBAK'.
  X_FIELDS-FIELDNAME = 'BSTNK'.
  X_FIELDS-SELECTFLAG = 'X'.
  APPEND X_FIELDS.

  X_FIELDS-TABNAME = 'VBAK'.
  X_FIELDS-FIELDNAME = 'ERDAT'.
  X_FIELDS-SELECTFLAG = ''.
  APPEND X_FIELDS.

  X_FIELDS-TABNAME = 'VBAK'.
  X_FIELDS-FIELDNAME = 'ERNAM'.
  X_FIELDS-SELECTFLAG = ''.
  APPEND X_FIELDS.

  SORT X_BSTNK DESCENDING BY ERDAT.
  DESCRIBE TABLE X_BSTNK LINES W_CNT.
  IF W_CNT = 1.
    READ TABLE X_BSTNK INDEX 1.
    P_BSTNK = X_BSTNK-BSTNK.
  ELSE.
    CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
         EXPORTING
              CUCOL                        = 20
              CUROW                        = 05
              SELECTFIELD                  = SELECTFIELD
              TITEL                        = 'Header P.O Number'
         IMPORTING
              IND                          = ID_TABIX
              SELECT_VALUE                 = SELECT_VALUE
         TABLES
              FIELDS                       = X_FIELDS
              FULL_TABLE                   = X_BSTNK
         EXCEPTIONS
              FULL_TABLE_EMPTY             = 1
              NO_TABLESTRUCTURE_GIVEN      = 2
              NO_TABLEFIELDS_IN_DICTIONARY = 3
              MORE_THEN_ONE_SELECTFIELD    = 4
              NO_SELECTFIELD               = 5
              OTHERS                       = 6.

    CHECK NOT ID_TABIX IS INITIAL.
    READ TABLE X_BSTNK INDEX ID_TABIX.
    P_BSTNK = X_BSTNK-BSTNK.
  ENDIF.
ENDFORM.                    " GET_BSTNK
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  SELECT *
         INTO TABLE IT_VBAK
         FROM VBAK
        WHERE BSTNK EQ P_BSTNK.

  SELECT *
         INTO TABLE IT_VBAP
         FROM VBAP
              FOR ALL ENTRIES IN IT_VBAK
        WHERE VBELN EQ IT_VBAK-VBELN.

  SELECT *
         INTO TABLE IT_VBKD
         FROM VBKD
              FOR ALL ENTRIES IN IT_VBAK
        WHERE VBELN EQ IT_VBAK-VBELN.

  SELECT *
         INTO TABLE IT_VBEP
         FROM VBEP
              FOR ALL ENTRIES IN IT_VBAK
        WHERE VBELN EQ IT_VBAK-VBELN.

  SELECT *
         INTO TABLE IT_VBPA
         FROM VBPA
              FOR ALL ENTRIES IN IT_VBAK
        WHERE VBELN EQ IT_VBAK-VBELN
        AND   PARVW EQ 'WE'. "SH
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_DATA.
  DESCRIBE TABLE IT_VBAP LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
    STOP.
  ENDIF.

  SORT IT_VBEP BY VBELN POSNR EDATU ETENR.
  REFRESH IT_DOWNFILE. CLEAR IT_DOWNFILE.
  LOOP AT IT_VBAK.
    CLEAR : W_TOT_PIECES, W_TOT_ITEMS.
    LOOP AT IT_VBAP WHERE VBELN EQ IT_VBAK-VBELN.
      W_NUMC_7 = IT_VBAP-KWMENG.
      W_TOT_PIECES = W_TOT_PIECES + W_NUMC_7.
      W_TOT_ITEMS = W_TOT_ITEMS + 1.
      W_TOT_VOLUM = IT_VBAP-VOLUM.
      W_TOT_NTGEW = IT_VBAP-NTGEW.
    ENDLOOP.

    LOOP AT IT_VBAP WHERE VBELN EQ IT_VBAK-VBELN.
      IT_DOWNFILE-RECORD+0(10)  = IT_VBAK-BSTNK.                "1
      IT_DOWNFILE-RECORD+10(10) = IT_VBAK-VBELN.                "2
      W_CHAR_11 = IT_VBAK-NETWR.
      W_NUMC_11 = W_CHAR_11.
      IT_DOWNFILE-RECORD+28(11) = W_NUMC_11.                    "4

      READ TABLE IT_VBPA WITH KEY VBELN = IT_VBAK-VBELN.
      IF SY-SUBRC = 0.
        IT_DOWNFILE-RECORD+130(04) = IT_VBPA-KUNNR.             "17
      ENDIF.

*      PERFORM READ_TEXT USING
*                        W_ERR IT_DOWNFILE-RECORD+128(02) W_ORG. "16

      IF W_ERR IS INITIAL.
        W_NUMC_7 = IT_VBAP-KWMENG.
        IT_DOWNFILE-RECORD+105(07) = W_NUMC_7.                  "13
        IT_DOWNFILE-RECORD+112(07) = '0000000'.                 "14
      ELSE.
        W_NUMC_7 = IT_VBAP-KWMENG.
        IT_DOWNFILE-RECORD+105(07) = '0000000'.                 "13
        IT_DOWNFILE-RECORD+112(07) = W_NUMC_7.                  "14
      ENDIF.

      IF IT_DOWNFILE-RECORD+128(02) = ''.
        IT_DOWNFILE-RECORD+74(18) = IT_VBAP-kdmat.              "11
      ELSE.
        w_org = it_vbap-kdmat.
        IT_DOWNFILE-RECORD+74(18) = W_ORG.                      "11
      ENDIF.


      IT_DOWNFILE-RECORD+92(13) = IT_VBAP-ARKTX.                "12

      IT_DOWNFILE-RECORD+39(09) = W_TOT_PIECES.                 "5
      IT_DOWNFILE-RECORD+48(07) = W_TOT_ITEMS.                  "6

      IT_DOWNFILE-RECORD+55(07) = W_TOT_VOLUM.                  "7
      IT_DOWNFILE-RECORD+62(07) = W_TOT_NTGEW.                  "8

      W_CHAR_9 = IT_VBAP-NETPR.
      W_NUMC_9 = W_CHAR_9.
      IT_DOWNFILE-RECORD+119(09) = W_NUMC_9.                    "15


      READ TABLE IT_VBKD WITH KEY VBELN = IT_VBAP-VBELN
                                  POSNR = IT_VBAP-POSNR.
      IF SY-SUBRC = 0.
        IT_DOWNFILE-RECORD+69(04) = IT_VBKD-BSTKD.              "9
      ENDIF.

      READ TABLE IT_VBEP WITH KEY VBELN = IT_VBAP-VBELN
                                  POSNR = IT_VBAP-POSNR
                                  ETENR = '1'.
      IF SY-SUBRC = 0.
        IT_DOWNFILE-RECORD+20(08) = IT_VBEP-EDATU.              "3
      ENDIF.

      W_NUMC_4 = 1.
      LOOP AT IT_VBEP WHERE VBELN EQ IT_VBAP-VBELN
                      AND   POSNR EQ IT_VBAP-POSNR.
*       IT_DOWNFILE-RECORD+73(01) = IT_VBEP-ETENR.              "10
        IT_DOWNFILE-RECORD+73(01) = W_NUMC_4+3(1).              "10

        APPEND IT_DOWNFILE.
        W_NUMC_4 = W_NUMC_4 + 1.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  DESCRIBE TABLE IT_DOWNFILE LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
    STOP.
  ENDIF.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT USING ERR ECODE ORG.
  CLEAR : ERR, ECODE, ORG.

  CONCATENATE IT_VBAP-VBELN IT_VBAP-POSNR
              INTO W_NAME.
  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            ID                      = '0002'
            LANGUAGE                = 'E'
            NAME                    = W_NAME
            OBJECT                  = 'VBBP'
       TABLES
            LINES                   = ITFTEXT
       EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.

  CHECK SY-SUBRC = 0.
  READ TABLE ITFTEXT WITH KEY TDLINE = 'Error List'.
  IF SY-SUBRC = 0.
    ERR = 'Y'.
    SY-TABIX = SY-TABIX + 1.
    READ TABLE ITFTEXT INDEX SY-TABIX.
    ECODE = ITFTEXT-TDLINE+0(2).
  ENDIF.

  READ TABLE ITFTEXT INDEX 2.
  IF SY-SUBRC = 0.
    ORG = ITFTEXT-TDLINE+0(18).
  ENDIF.
ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
FORM DOWNLOAD_DATA.
**  CONCATENATE  '/sapmnt/' SY-SYSID '/EDI/'
**               'm_off_'  IT_VBAK-BSTNK '_' IT_VBAK-VBELN
**               '.txt'
**               INTO W_DSN.
  w_dsn =  '/usr/sap/EDI_SAP/MOFF'.

  PERFORM MAKE_HEADER changing w_dsn.
  LOOP AT IT_DOWNFILE.
    OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
    TRANSFER IT_DOWNFILE TO W_DSN.
  ENDLOOP.

  CLOSE DATASET W_DSN.

  IF SY-SUBRC = 0.
    MESSAGE I000 WITH TEXT-M02.
  ELSE.
    MESSAGE I000 WITH TEXT-M03.
  ENDIF.
ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEADER
*&---------------------------------------------------------------------*
FORM MAKE_HEADER changing p_dsn.
  DATA : W_NUMC_8(8) TYPE N.
  data: w_month type c,
        w_date(2) type c,
        w_ser     type c.

  case sy-datum+4(2).
    when '10'.
      w_month = 'A'.
    when '11'.
      w_month = 'B'.
    when '12'.
      w_month = 'C'.
    when others.
      w_month = sy-datum+5(1).
  endcase.
  w_date = sy-datum+6(2).

  IT_HEADER-RECORD+0(5) = 'HMOFF'.
  IT_HEADER-RECORD+17(4) = SY-DATUM+4(4).
  IT_HEADER-RECORD+21(4) = SY-DATUM+0(4).

  IF IT_HEADER-RECORD+17(1) = '0'.
    IT_HEADER-RECORD+5(1) = IT_HEADER-RECORD+18(1).
  ELSEIF IT_HEADER-RECORD+17(2) = '10'.
    IT_HEADER-RECORD+5(1) = 'A'.
  ELSEIF IT_HEADER-RECORD+17(2) = '11'.
    IT_HEADER-RECORD+5(1) = 'B'.
  ELSEIF IT_HEADER-RECORD+17(2) = '12'.
    IT_HEADER-RECORD+5(1) = 'C'.
  ENDIF.
  IT_HEADER-RECORD+6(2) = IT_HEADER-RECORD+19(2).

  PERFORM MAKE_SEQ USING IT_HEADER-RECORD+8(1) changing w_ser.

  DESCRIBE TABLE IT_DOWNFILE LINES W_NUMC_8.

  IT_HEADER-RECORD+9(8) = W_NUMC_8.
  IT_HEADER-RECORD+25(1) = 'S'.

  APPEND IT_HEADER.
  concatenate p_dsn w_month w_date w_ser '.txt' into p_dsn.
  LOOP AT IT_HEADER.
    OPEN DATASET W_DSN IN TEXT MODE FOR OUTPUT. "CHECK DUPL.
    TRANSFER IT_HEADER TO W_DSN.
  ENDLOOP.
ENDFORM.                    " MAKE_HEADER
*&---------------------------------------------------------------------*
*&      Form  MAKE_SEQ
*&---------------------------------------------------------------------*
FORM MAKE_SEQ USING CNT changing p_ser.
  TABLES : ZTSD_SEQ.

  SELECT SINGLE *
         FROM ZTSD_SEQ
        WHERE ZKEY EQ IT_HEADER-RECORD+0(8).
  IF SY-SUBRC = 0.
    ZTSD_SEQ-ZSEQ = ZTSD_SEQ-ZSEQ + 1.
    MODIFY ZTSD_SEQ.
    CNT = ZTSD_SEQ-ZSEQ.
    p_ser = ZTSD_SEQ-ZSEQ.
  ELSE.
    ZTSD_SEQ-ZKEY = IT_HEADER-RECORD+0(8).
    ZTSD_SEQ-ZSEQ = '1'.
    INSERT ZTSD_SEQ.
    CNT = 1.
    p_ser = '1'.
  ENDIF.
ENDFORM.                    " MAKE_SEQ
