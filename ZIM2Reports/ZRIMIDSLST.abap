*&---------------------------------------------------------------------*
*& Report  ZRIMIDSLST                                                  *
*&---------------------------------------------------------------------*
*& Program Name : Entry Summary List                                   *
*& Created By   : Na Hyun Ju                                           *
*& Created On   : 2004.02.01                                           *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [Change Log]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMIDSLST   MESSAGE-ID ZIM
                     LINE-SIZE 180
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------
INCLUDE   ZRIMIDSLSTTOP.
INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS FOR ZTBL-BUKRS    NO-EXTENSION
                                             NO INTERVALS,
                   S_OPNNO FOR ZTBL-ZFOPNNO,
                   S_BELN  FOR ZTBL-ZFREBELN,
                   S_BLNO  FOR ZTBL-ZFBLNO,
                   S_HBLNO FOR ZTBL-ZFHBLNO,
                   S_IDRNO FOR ZTIDSUS-ZFENTNO,
                   S_IDWDT FOR ZTIDSUS-ZFEEDT,
                   S_IDSDT FOR ZTIDSUS-ZFEDT,
                   S_CUT   FOR ZTIDSUS-ZFCTW,
                   S_INRC  FOR ZTIDSUS-ZFINRC,
                   S_PRNAM FOR ZTBL-ZFPRNAM,
                   S_INCO1 FOR ZTIDSUS-INCO1,
                   S_SCON  FOR ZTIDSUS-ZFCAC,
                   S_AUTO  FOR ZTIDSUS-ZFAUTO.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
   PERFORM   P2000_SET_PARAMETER.
   PERFORM   P2000_SET_BUKRS.

* Title Text Write
TOP-OF-PAGE.
   IF SY-LANGU EQ '3'.
      PERFORM   P3000_TITLE_WRITE.
   ELSE.
      PERFORM   P3000_TITLE_WRITE_EN.
   ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
*  Data SELECT
   PERFORM   P1000_GET_ZTIDS      USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'. MESSAGE S738.   EXIT.    ENDIF.
* Report Write
   PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.  EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.
         W_FIELD_NM = 'ZFIDRNO'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'DISP'.
            PERFORM P2000_SHOW_IDS USING  IT_TAB-ZFIVNO.
      WHEN 'DSBL'.
            PERFORM P2000_SHOW_BL  USING  IT_TAB-ZFBLNO.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
           PERFORM   P1000_GET_ZTIDS       USING   W_ERR_CHK.
           IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
           PERFORM RESET_LIST.
      WHEN OTHERS.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR51'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50 '[ 수입신고필정보 조회 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /3 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,(15) '구매문서' NO-GAP,  SY-VLINE,
            (21) 'House B/L No'      NO-GAP,  SY-VLINE,
            (20) '결제금액'          NO-GAP,  SY-VLINE,
            (20) '보험료'            NO-GAP , SY-VLINE,
            (20) '세관'              NO-GAP,  SY-VLINE,
            (11) '통관일'            NO-GAP,  SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE :/ SY-VLINE,(15)  '면허번호' NO-GAP,  SY-VLINE,
            (21)  '대표 L/C 개설번호' NO-GAP, SY-VLINE,
            (20)  '대표품명'         NO-GAP,  SY-VLINE,
            (20)  '운임'             NO-GAP,  SY-VLINE,
            (20)  '관세사'           NO-GAP,  SY-VLINE,
            (11)  '신고희망일'       NO-GAP,  SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTIDS
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTIDS   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
  REFRESH IT_TAB.
  SELECT MAX( C~ZFBLNO )   AS ZFBLNO  MAX( C~ZFHBLNO ) AS ZFHBLNO
         MAX( C~ZFOPNNO )  AS ZFOPNNO MAX( C~ZFPRNAM ) AS ZFPRNAM
         MAX( C~ZFSHNO )   AS ZFSHNO  MAX( B~ZFENTNO ) AS ZFENTNO
         MAX( B~ZFEEDT )   AS ZFEEDT  MAX( B~ZFEDT )   AS ZFEDT
         MAX( B~ZFIVAMT )  AS ZFIVAMT MAX( B~ZFIVAMC ) AS ZFIVAMC
         MAX( B~ZFTOFEE )  AS ZFTOFEE MAX( B~ZFTOCUR ) AS ZFTOCUR
         MAX( B~INCO1 )    AS INCO1   MAX( B~ZFDUTY )  AS ZFDUTY
         MAX( B~ZFCAC )    AS ZFCAC   MAX( B~ZFINRC )  AS ZFINRC
         MAX( B~ZFCTW )    AS ZFCTW   MAX( B~ZFKRW )   AS ZFKRW
         MAX( B~ZFIVNO )   AS ZFIVNO  MAX( B~ZFTRMET ) AS ZFTRMET
         MAX( B~ZFCARNM )  AS ZFCARNM MAX( B~ZFEXPDT ) AS ZFEXPDT
         MAX( B~ZFENDT )   AS ZFENDT  MAX( B~ZFSUMDT ) AS ZFSUMDT
         MAX( C~ZFREBELN ) AS ZFREBELN
  INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM ( ZTIVIT AS A INNER JOIN  ZTIDSUS AS B
  ON     A~ZFIVNO    EQ  B~ZFIVNO    )
  INNER  JOIN ZTBL   AS  C
  ON     A~ZFBLNO    EQ  C~ZFBLNO
  WHERE  C~ZFREBELN  IN  S_BELN
  AND    C~ZFBLNO    IN  S_BLNO
  AND    C~ZFHBLNO   IN  S_HBLNO
  AND    C~ZFOPNNO   IN  S_OPNNO
  AND    C~ZFPRNAM   IN  S_PRNAM
  AND    B~ZFENTNO   IN  S_IDRNO
  AND    B~ZFEDT     IN  S_IDSDT
  AND    B~ZFEEDT    IN  S_IDWDT
  AND    B~ZFCTW     IN  S_CUT
  AND    B~ZFINRC    IN  S_INRC
  AND    B~INCO1     IN  S_INCO1
  AND    B~ZFCAC     IN  S_SCON
  AND    B~ZFAUTO    IN  S_AUTO
  GROUP BY
         A~ZFIVNO.

  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'. EXIT.
  ENDIF.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
     IF NOT IT_TAB-ZFSHNO IS INITIAL.
        CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO
                    INTO IT_TAB-W_EBELN.
     ELSE.
        MOVE  IT_TAB-ZFREBELN  TO  IT_TAB-W_EBELN.
     ENDIF.
     " Customs
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOTM'
                                                IT_TAB-ZFINRC
                                       CHANGING IT_TAB-DOMTEXT.
     " Custom Broker
     CLEAR : ZTIMIMG10, LFA1.
     SELECT SINGLE * FROM ZTIMIMG10 WHERE ZFCUT = IT_TAB-ZFCTW.

     SELECT SINGLE * FROM LFA1      WHERE LIFNR = ZTIMIMG10-ZFVEN.
     IF SY-SUBRC EQ 0.
        MOVE  LFA1-NAME1 TO IT_TAB-NAME1.
     ENDIF.

     "Invoice No
     SELECT C~ZFCIVNO
       INTO IT_TAB-ZFCIVNO
       FROM ( ( ZTBL AS A INNER JOIN ZTCIVIT AS B
                ON A~ZFBLNO EQ B~ZFBLNO )
      INNER JOIN ZTCIVHD AS C ON B~ZFCIVRN EQ C~ZFCIVRN )
      WHERE A~ZFHBLNO EQ IT_TAB-ZFHBLNO.
     ENDSELECT.

     MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_GET_ZTREQST
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIMR51'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMR51'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P3000_LINE_WRITE.

      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.
   ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

   MOVE 0 TO SY-LSIND.

   W_PAGE = 1.
   W_LINE = 1.
   W_COUNT = 0.
   IF SY-LANGU EQ '3'.
      PERFORM   P3000_TITLE_WRITE.
   ELSE.
      PERFORM   P3000_TITLE_WRITE_EN.
   ENDIF.

   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

   IF W_LINE >= 53.
      WRITE : / SY-ULINE.
      W_PAGE = W_PAGE + 1.    W_LINE = 0.
      NEW-PAGE.
   ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

   IF W_COUNT GT 0.
      WRITE : / 'Total', W_COUNT, 'Case'.
   ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.

   WRITE :/ SY-VLINE,
           (15) IT_TAB-W_EBELN                         NO-GAP, SY-VLINE,
           (21) IT_TAB-ZFHBLNO                         NO-GAP, SY-VLINE,
           (03) IT_TAB-ZFIVAMC,
           (16) IT_TAB-ZFIVAMT CURRENCY IT_TAB-ZFIVAMC NO-GAP, SY-VLINE,
           (03) IT_TAB-ZFTOCUR,
           (16) IT_TAB-ZFTOFEE CURRENCY IT_TAB-ZFTOCUR NO-GAP, SY-VLINE,
           (20) IT_TAB-DOMTEXT                         NO-GAP, SY-VLINE,
           (11) IT_TAB-ZFEDT                           NO-GAP, SY-VLINE,
           (15) IT_TAB-ZFCIVNO                         NO-GAP, SY-VLINE,
           (15) IT_TAB-ZFCARNM                         NO-GAP, SY-VLINE,
           (11) IT_TAB-ZFEXPDT                         NO-GAP, SY-VLINE,
           (11) IT_TAB-ZFSUMDT                         NO-GAP, SY-VLINE.

   HIDE: IT_TAB.

   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   WRITE :/ SY-VLINE,
           (15) IT_TAB-ZFENTNO                         NO-GAP, SY-VLINE,
           (21) IT_TAB-ZFOPNNO                         NO-GAP, SY-VLINE,
           (20) IT_TAB-ZFRGDSR                         NO-GAP, SY-VLINE,
           (03) IT_TAB-ZFTOCUR,
           (16) IT_TAB-ZFDUTY CURRENCY IT_TAB-ZFTOCUR  NO-GAP, SY-VLINE,
           (20) IT_TAB-NAME1                           NO-GAP, SY-VLINE,
           (11) IT_TAB-ZFEEDT                          NO-GAP, SY-VLINE,
           (15) IT_TAB-ZFTRMET                         NO-GAP, SY-VLINE,
           (15) IT_TAB-ZFWERKS                         NO-GAP, SY-VLINE,
           (11) IT_TAB-ZFENDT                          NO-GAP, SY-VLINE,
           (11) '           '                          NO-GAP, SY-VLINE.
   HIDE: IT_TAB.

   WRITE:/ SY-ULINE.
* hide
   W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IDS
*&---------------------------------------------------------------------*
FORM P2000_SHOW_IDS USING    P_ZFIVNO.

   SET PARAMETER ID 'ZPIVNO'    FIELD  P_ZFIVNO.
   SET PARAMETER ID 'ZPENTNO'   FIELD ''.

   CALL TRANSACTION 'ZIMCC3' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_IDS

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P2000_SET_BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> Company Code SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P2000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE_EN
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE_EN.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /80 '[ Entry Summary List ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /3 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,(15) 'Purchase Doc'      NO-GAP,  SY-VLINE,
            (21) 'House B/L No'      NO-GAP,  SY-VLINE,
            (20) 'Taxable Amount'    NO-GAP,  SY-VLINE,
            (20) 'Total Fee'         NO-GAP , SY-VLINE,
            (20) 'Customs'           NO-GAP,  SY-VLINE,
            (11) 'Entry Date'        NO-GAP,  SY-VLINE,
            (15) 'Invoice No'        NO-GAP,  SY-VLINE,
            (15) 'Vessel Name'       NO-GAP,  SY-VLINE,
            (11) 'Export Date'        NO-GAP,  SY-VLINE,
            (11) 'Ent.Summary'       NO-GAP,  SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE :/ SY-VLINE,(15) 'Entry No'  NO-GAP,  SY-VLINE,
            (21) 'Rep. Approve No'   NO-GAP,  SY-VLINE,
            (20) 'Rep. Goods Desc.'  NO-GAP,  SY-VLINE,
            (20) 'Duty'              NO-GAP,  SY-VLINE,
            (20) 'Broker'            NO-GAP,  SY-VLINE,
            (11) 'Est. Entry'        NO-GAP,  SY-VLINE,
            (15) 'Shipping Method'   NO-GAP,  SY-VLINE,
            (15) 'Plant'             NO-GAP,  SY-VLINE,
            (11) 'Import Date'       NO-GAP,  SY-VLINE,
            (11) '           '       NO-GAP,  SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE_EN
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFBLNO.

   SET PARAMETER ID 'ZPBLNO'    FIELD  P_ZFBLNO.
   SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
   SET PARAMETER ID 'BES'       FIELD ''.

   CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_BL
