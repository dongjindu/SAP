*&---------------------------------------------------------------------*
*& Report  ZRIMTRSLST                                                  *
*&---------------------------------------------------------------------*
*&  Program Name : The Object List to Delivery Order                   *
*&  Created By   : Na Hyun Joo                                         *
*&  Created on   : 12.05.2003                                          *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [Change Request
*&---------------------------------------------------------------------*
REPORT  ZRIMTRLST    MESSAGE-ID ZIM
                     LINE-SIZE 92
                     NO STANDARD PAGE HEADING.

TABLES : ZTBL, ZTBLIT, ZTIV, LIPS, LIKP, EKKO, EIKP, MAKT,
         ZTIDSUS, ZTTRITD, ZTIMIMG00, ZTCIVIT, ZTCIVHD.

*>> Declaration Type-Pool for ALV Display.
TYPE-POOLS : SLIS.

*-----------------------------------------------------------------------
* Internal Talbe for Write
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_CIV OCCURS 0,
       ZFCIVNO      LIKE  ZTCIVHD-ZFCIVNO.        " CIV NO
DATA : END OF IT_CIV.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK         TYPE  C,                      " Selection.
       TRAID        LIKE  LIKP-TRAID,             " Container Number.
       BORGR_GRP    like  LIKP-BORGR_GRP,         " Seal No.
       ZFHBLNO      LIKE  ZTBL-ZFHBLNO,           " House B/L No
       ZFBLNO       LIKE  ZTBL-ZFBLNO,            " B/L Document No
       ZFETD        LIKE  ZTBL-ZFETD,             " ETD
       ZFETA        LIKE  ZTBL-ZFETA,             " ETA
       ZFRETA       LIKE  ZTBL-ZFRETA,            " Arrival Date
*       ZFEDT        LIKE  ZTIDSUS-ZFEDT,          " Entry Date
       ZFREBELN     LIKE  ZTBL-ZFREBELN,          " Representive P/O
       CASE_QTY(3)  TYPE  N.                      " Case Qty.
DATA : END OF IT_TAB.

DATA : BEGIN OF IT_TAB_DETAIL OCCURS 0,
       ZFHBLNO      LIKE  ZTBL-ZFHBLNO,           " House B/L No
       ZFBLNO       LIKE  ZTBL-ZFBLNO,            " B/L Document No
       ZFETD        LIKE  ZTBL-ZFETD,             " ETD
       ZFETA        LIKE  ZTBL-ZFETA,             " ETA
       ZFRETA       LIKE  ZTBL-ZFRETA,            " Arrival Date
*       ZFEDT        LIKE  ZTIDSUS-ZFEDT,          " Entry Date
       ZFREBELN     LIKE  ZTBL-ZFREBELN,          " Representive P/O
       TRAID        LIKE  LIKP-TRAID,             " Container No.
       BORGR_GRP    like LIKP-BORGR_GRP,          " Seal No.
       WERKS        LIKE  LIPS-WERKS,             " Plant
       KDMAT        LIKE  LIPS-KDMAT,             " Case No.
       MATNR        LIKE  LIPS-MATNR,             " Material No.
       MAKTX        LIKE  MAKT-MAKTX,             " Material Description
       VGBEL        LIKE  LIPS-VGBEL,             " P/O
       VGPOS        LIKE  LIPS-VGPOS,             " P/O Item
       LFIMG        LIKE  LIPS-LFIMG,             " Material Total
       MEINS        LIKE  LIPS-MEINS,             " Unit
       VBELN        LIKE  LIKP-VBELN.             " Inbound Delivery
DATA : END   OF IT_TAB_DETAIL.

DATA : BEGIN OF IT_TAB_TEMP OCCURS 0,
       ZFHBLNO      LIKE  ZTBL-ZFHBLNO,           " House B/L No
       ZFBLNO       LIKE  ZTBL-ZFBLNO,            " B/L Document No
       ZFETD        LIKE  ZTBL-ZFETD,             " ETD
       ZFETA        LIKE  ZTBL-ZFETA,             " ETA
       ZFRETA       LIKE  ZTBL-ZFRETA,            " Arrival Date
*       ZFEDT        LIKE  ZTIDSUS-ZFEDT,          " Entry Date
       ZFREBELN     LIKE  ZTBL-ZFREBELN.          " Representive P/O
DATA : END   OF IT_TAB_TEMP.

DATA : BEGIN OF IT_TAB_TEMP1 OCCURS 0,
       TRAID        LIKE  LIKP-TRAID,             " Container No.
       BORGR_GRP    like  LIKP-BORGR_GRP,         " Seal No.
       WERKS        LIKE  LIPS-WERKS,             " Plant
       KDMAT        LIKE  LIPS-KDMAT,             " Case No.
       MATNR        LIKE  LIPS-MATNR,             " Material No.
       MAKTX        LIKE  MAKT-MAKTX,             " Material Description
       VGBEL        LIKE  LIPS-VGBEL,             " P/O
       VGPOS        LIKE  LIPS-VGPOS,             " P/O Item
       LFIMG        LIKE  LIPS-LFIMG,             " Material Total
       MEINS        LIKE  LIPS-MEINS,             " Unit
       VBELN        LIKE  LIKP-VBELN.             " Inbound Delivery
DATA : END   OF IT_TAB_TEMP1.

*-----------------------------------------------------------------------
* Selection Internal Table Define
*-----------------------------------------------------------------------
DATA : IT_DOHD     LIKE ZSDOHD OCCURS 50 WITH HEADER LINE.
DATA : IT_DOIT     LIKE ZSDOIT OCCURS 50 WITH HEADER LINE.

*-----------------------------------------------------------------------
* ALV Display Ineternal Table
*-----------------------------------------------------------------------
DATA: IS_LAYOUT     TYPE SLIS_LAYOUT_ALV,
      IT_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV,
      IT_EVENTS     TYPE SLIS_T_EVENT,
      IT_SORT       TYPE SLIS_T_SORTINFO_ALV,
      IT_SP_GROUP   TYPE SLIS_T_SP_GROUP_ALV,
      IT_TOP        TYPE SLIS_T_LISTHEADER,
      ALV_VARIANT   LIKE DISVARIANT,
      IT_SPECIAL_GROUPS TYPE  SLIS_T_SP_GROUP_ALV.

DATA: IT_FIELDCAT_0 TYPE  SLIS_FIELDCAT_ALV,
      IT_EVENTS_0   TYPE  SLIS_ALV_EVENT,
      IT_SORT_0     TYPE  SLIS_SORTINFO_ALV,
      IT_TOP_0      TYPE  SLIS_LISTHEADER.

DATA: IT_COLOR TYPE SLIS_SPECIALCOL_ALV OCCURS 0 WITH HEADER LINE.

*-----------------------------------------------------------------------
* Variable Define
*-----------------------------------------------------------------------
DATA: W_ERR_CHK     TYPE  C,
      W_TABIX       LIKE  SY-TABIX,
      W_REPID       LIKE  ALV_VARIANT-REPORT,
      W_VBELN       LIKE  LIKP-VBELN,
      W_ZFBLNO      LIKE  ZTBL-ZFBLNO,
      W_TRAID       LIKE  LIKP-TRAID,
      W_BORGR_GRP   LIKE  LIKP-BORGR_GRP,
      W_ZFHBLNO     LIKE  ZTBL-ZFHBLNO,
      W_EBELN       LIKE  ZTBL-ZFREBELN,
      W_ETD         LIKE  ZTBL-ZFETD,
      W_ETA         LIKE  ZTBL-ZFETA,
      P_BUKRS       LIKE  ZTBL-BUKRS,
      INCLUDE(8)    TYPE   C,
      W_EDT         LIKE  ZTIDSUS-ZFEDT,
      W_CNT         TYPE  I,
      POS           LIKE  SY-CUCOL,
      W_MOD         TYPE  I,
      W_LINE        TYPE  I,
      W_SELECTED_LINES TYPE  I.

*-----------------------------------------------------------------------
* Selection Screen.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR  ZTBL-BUKRS NO INTERVALS NO-EXTENSION,
                S_EBELN   FOR  ZTBLIT-EBELN,
                S_HBLNO   FOR  ZTBL-ZFHBLNO,
                S_BLNO    FOR  ZTBL-ZFBLNO,
                S_ETD     FOR  ZTBL-ZFETD,
                S_ETA     FOR  ZTBL-ZFETA,
                S_EKORG   FOR  ZTBL-EKORG,
                S_EKGRP   FOR  ZTBL-EKGRP,
                S_SHTY    FOR  ZTBL-ZFSHTY,
                S_FORD    FOR  ZTBL-ZFFORD,
                S_WERKS   FOR  ZTBL-ZFWERKS,
                S_MATNR   FOR  ZTBLIT-MATNR,
                S_VBELN   FOR  LIKP-VBELN,
                S_TRAID   FOR  LIKP-TRAID,
                S_VIA     FOR  ZTBL-ZFVIA.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_VARI LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN: END OF BLOCK B2.

*--------------------------------------------------------------------
*  AT SELECTION-SCREEN ON VALUE-REQUEST
*--------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_F4.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* START OF SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

  " Data Read
  PERFORM   P1000_GET_IT_TAB       USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

  " Container No. Grouping.
  PERFORM   P1000_CONTAINER_GROUP.

  " Data Write.
  PERFORM   P1000_APPEND_IT_FIELDCAT.
  PERFORM   P1000_APPEND_IT_EVENTS.
  PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
*------- Abbrechen (CNCL) ----------------------------------------------
    WHEN 'CNCL'.
      SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
    WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
    WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
    WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
    WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
    WHEN 'WESU'.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMT0'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'ZIMT0'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIMT0'.           " GUI TITLE SETTING..

  W_REPID = SY-REPID.
  ALV_VARIANT-REPORT = SY-REPID.
  ALV_VARIANT-VARIANT = P_VARI.
  IS_LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = W_REPID
            I_BYPASSING_BUFFER       = 'X'
            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
            I_CALLBACK_PF_STATUS_SET = 'SET_STATUS'
            IS_LAYOUT                = IS_LAYOUT
            IT_FIELDCAT              = IT_FIELDCAT[]
            IT_EVENTS                = IT_EVENTS[]
            I_SAVE                   = ' '
            IS_VARIANT               = ALV_VARIANT
       TABLES
            T_OUTTAB                 = IT_TAB
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_TAB USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting
  REFRESH : IT_TAB_TEMP.
  SELECT A~ZFHBLNO   A~ZFETD    A~ZFETA   A~ZFRETA   A~ZFETD
         A~ZFREBELN  A~ZFBLNO
  INTO   CORRESPONDING FIELDS OF TABLE IT_TAB_TEMP
  FROM   ZTBL     AS A
  WHERE  A~ZFHBLNO      IN     S_HBLNO
  AND    A~ZFBLNO       IN     S_BLNO
  AND    A~ZFETD        IN     S_ETD
  AND    A~ZFETA        IN     S_ETA
  AND    A~EKORG        IN     S_EKORG
  AND    A~EKGRP        IN     S_EKGRP
  AND    A~ZFSHTY       IN     S_SHTY
  AND    A~ZFFORD       IN     S_FORD
  AND    A~ZFVIA        IN     S_VIA.

  REFRESH : IT_TAB_DETAIL, IT_TAB.

  LOOP  AT  IT_TAB_TEMP.

    REFRESH : IT_CIV, IT_TAB_TEMP1.
    CLEAR   : IT_CIV, IT_TAB_TEMP1.
    SELECT A~ZFCIVNO  INTO CORRESPONDING FIELDS OF TABLE IT_CIV
    FROM   ZTCIVHD  AS  A  INNER  JOIN  ZTCIVIT  AS  B
    ON     A~ZFCIVRN       EQ     B~ZFCIVRN
    WHERE  B~ZFBLNO        EQ     IT_TAB_TEMP-ZFBLNO
    GROUP BY
           A~ZFCIVNO.

    IF SY-SUBRC EQ 0.
      SELECT  A~TRAID   B~KDMAT  B~MATNR  B~LFIMG  A~VBELN
              B~MEINS   B~WERKS  B~VGBEL  B~VGPOS  A~BORGR_GRP
      INTO    CORRESPONDING FIELDS OF TABLE IT_TAB_TEMP1
      FROM    LIKP   AS A    INNER  JOIN  LIPS  AS  B
      ON      A~VBELN        EQ     B~VBELN
      FOR     ALL  ENTRIES   IN     IT_CIV
      WHERE   A~BOLNR        EQ     IT_CIV-ZFCIVNO
      AND     B~VGBEL        IN     S_EBELN
      AND     B~WERKS        IN     S_WERKS
      AND     B~MATNR        IN     S_MATNR
      AND     B~VBELN        IN     S_VBELN
      AND     A~TRAID        IN     S_TRAID.
    ENDIF.

    LOOP  AT  IT_TAB_TEMP1.
      MOVE-CORRESPONDING IT_TAB_TEMP  TO  IT_TAB_DETAIL.
      MOVE : IT_TAB_TEMP1-TRAID       TO  IT_TAB_DETAIL-TRAID,
             IT_TAB_TEMP1-BORGR_GRP   TO  IT_TAB_DETAIL-BORGR_GRP,
             IT_TAB_TEMP1-KDMAT       TO  IT_TAB_DETAIL-KDMAT,
             IT_TAB_TEMP1-MATNR       TO  IT_TAB_DETAIL-MATNR,
             IT_TAB_TEMP1-LFIMG       TO  IT_TAB_DETAIL-LFIMG,
             IT_TAB_TEMP1-VBELN       TO  IT_TAB_DETAIL-VBELN,
             IT_TAB_TEMP1-MEINS       TO  IT_TAB_DETAIL-MEINS,
             IT_TAB_TEMP1-WERKS       TO  IT_TAB_DETAIL-WERKS,
             IT_TAB_TEMP1-VGBEL       TO  IT_TAB_DETAIL-VGBEL,
             IT_TAB_TEMP1-VGPOS       TO  IT_TAB_DETAIL-VGPOS.
      APPEND  IT_TAB_DETAIL.
    ENDLOOP.

  ENDLOOP.

  DESCRIBE  TABLE  IT_TAB_DETAIL LINES  W_LINE.
  IF W_LINE EQ 0.
    W_ERR_CHK = 'Y'.  MESSAGE S966.    EXIT.
  ENDIF.

  " Material Text Setting.
  LOOP AT IT_TAB_DETAIL.

    W_TABIX  =  SY-TABIX.

    " Delivry Order Table exist -> delete
    SELECT SINGLE * FROM ZTTRITD
    WHERE  VBELN    EQ   IT_TAB_DETAIL-VBELN.
    IF SY-SUBRC EQ 0.
      DELETE  IT_TAB_DETAIL  INDEX  W_TABIX.
      CONTINUE.
    ENDIF.

    " Material Text
    SELECT SINGLE * FROM  MAKT
    WHERE  MATNR    EQ    IT_TAB_DETAIL-MATNR
    AND    SPRAS    EQ    SY-LANGU.
    IF SY-SUBRC EQ 0.
      MOVE  MAKT-MAKTX   TO  IT_TAB_DETAIL-MAKTX.
    ENDIF.

    MODIFY IT_TAB_DETAIL INDEX W_TABIX.

  ENDLOOP.

ENDFORM.                    " P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

  " Company Code Set.
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P1000_CONTAINER_GROUP
*&---------------------------------------------------------------------*
FORM P1000_CONTAINER_GROUP.

  SORT  IT_TAB_DETAIL  BY  TRAID  VBELN  ZFETA  ZFHBLNO.

  LOOP AT IT_TAB_DETAIL.
    IF SY-TABIX EQ 1.
      MOVE : IT_TAB_DETAIL-VBELN    TO  W_VBELN,
             IT_TAB_DETAIL-TRAID    TO  W_TRAID,
             IT_TAB_DETAIL-BORGR_GRP TO W_BORGR_GRP,
             IT_TAB_DETAIL-ZFETD    TO  W_ETD,
             IT_TAB_DETAIL-ZFETA    TO  W_ETA,
             IT_TAB_DETAIL-ZFHBLNO  TO  W_ZFHBLNO,
             IT_TAB_DETAIL-ZFBLNO   TO  W_ZFBLNO,
             IT_TAB_DETAIL-ZFREBELN TO  W_EBELN,
             1                      TO  W_CNT.
    ENDIF.

    IF IT_TAB_DETAIL-TRAID  NE  W_TRAID.
      CLEAR : IT_TAB.
      MOVE : W_ZFBLNO               TO  IT_TAB-ZFBLNO,
             W_TRAID                TO  IT_TAB-TRAID,
             W_BORGR_GRP            TO  IT_TAB-BORGR_GRP,
             W_ETD                  TO  IT_TAB-ZFETD,
             W_ETA                  TO  IT_TAB-ZFETA,
             W_ZFHBLNO              TO  IT_TAB-ZFHBLNO,
             W_EBELN                TO  IT_TAB-ZFREBELN,
             W_CNT                  TO  IT_TAB-CASE_QTY.
      APPEND  IT_TAB.
      W_CNT  =  1.
      MOVE : IT_TAB_DETAIL-ZFBLNO   TO  W_ZFBLNO,
             IT_TAB_DETAIL-VBELN    TO  W_VBELN,
             IT_TAB_DETAIL-TRAID    TO  W_TRAID,
             IT_TAB_DETAIL-BORGR_GRP TO W_BORGR_GRP,
             IT_TAB_DETAIL-ZFETD    TO  W_ETD,
             IT_TAB_DETAIL-ZFETA    TO  W_ETA,
             IT_TAB_DETAIL-ZFHBLNO  TO  W_ZFHBLNO,
             IT_TAB_DETAIL-ZFREBELN TO  W_EBELN.
    ENDIF.
    IF IT_TAB_DETAIL-TRAID EQ W_TRAID AND
       IT_TAB_DETAIL-VBELN NE W_VBELN.
      W_CNT  =  W_CNT  +  1.
      MOVE  IT_TAB_DETAIL-VBELN  TO  W_VBELN.
    ENDIF.

    AT LAST.
      CLEAR : IT_TAB.
      MOVE : W_ZFBLNO               TO  IT_TAB-ZFBLNO,
             W_TRAID                TO  IT_TAB-TRAID,
             W_BORGR_GRP            TO  IT_TAB-BORGR_GRP,
             W_ETD                  TO  IT_TAB-ZFETD,
             W_ETA                  TO  IT_TAB-ZFETA,
             W_ZFHBLNO              TO  IT_TAB-ZFHBLNO,
             W_EBELN                TO  IT_TAB-ZFREBELN,
             W_CNT                  TO  IT_TAB-CASE_QTY.
      APPEND  IT_TAB.
      CLEAR : W_CNT.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " P1000_CONTAINER_GROUP
*&---------------------------------------------------------------------*
*&      Form  P1000_APPEND_IT_FIELDCAT
*&---------------------------------------------------------------------*
FORM P1000_APPEND_IT_FIELDCAT.

  REFRESH : IT_FIELDCAT.  CLEAR : IT_FIELDCAT_0.

  IT_FIELDCAT_0-CHECKBOX = 'X'.
  IT_FIELDCAT_0-INPUT    = 'X'.
  PERFORM FIELDCAT USING : 'MARK'        ' '              '1'.
  PERFORM FIELDCAT USING : 'TRAID'       'Container No'   '20'.
  PERFORM FIELDCAT USING : 'BORGR_GRP'   'Seal No'        '15'.
  PERFORM FIELDCAT USING : 'ZFHBLNO'     'B/L Number'     '25'.
  PERFORM FIELDCAT USING : 'ZFETD'       'E.T.D.'         '10'.
  PERFORM FIELDCAT USING : 'ZFETA'       'E.T.A.'         '10'.
  PERFORM FIELDCAT USING : 'ZFRETA'      'Arrival'        '10'.
  PERFORM FIELDCAT USING : 'ZFREBELN'    'P/O'            '10'.
  PERFORM FIELDCAT USING : 'CASE_QTY'    'Case'           '4'.

ENDFORM.                    " P1000_APPEND_IT_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM FIELDCAT USING   FIELDNAME
                      FIELDTEXT
                      OUTPUTLEN.

  IT_FIELDCAT_0-COL_POS        = POS.
  IT_FIELDCAT_0-FIELDNAME      = FIELDNAME.
  IT_FIELDCAT_0-REPTEXT_DDIC   = FIELDTEXT.
  IT_FIELDCAT_0-OUTPUTLEN      = OUTPUTLEN.
  APPEND IT_FIELDCAT_0 TO IT_FIELDCAT.
  CLEAR IT_FIELDCAT_0.

  POS = POS + 1.

ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  P1000_APPEND_IT_EVENTS
*&---------------------------------------------------------------------*
FORM P1000_APPEND_IT_EVENTS.

  REFRESH : IT_EVENTS.

  CLEAR IT_EVENTS_0.
  IT_EVENTS_0-NAME = 'USER_COMMAND'.
  IT_EVENTS_0-FORM = 'USER_COMMAND'.
  APPEND IT_EVENTS_0 TO IT_EVENTS.

  CLEAR IT_EVENTS_0.
  IT_EVENTS_0-NAME = 'TOP_OF_PAGE'.
  IT_EVENTS_0-FORM = 'TOP_OF_PAGE'.
  APPEND IT_EVENTS_0 TO IT_EVENTS.

  CLEAR IT_EVENTS_0.
  IT_EVENTS_0-NAME = 'PF_STATUS_SET'.
  IT_EVENTS_0-FORM = 'SET_STATUS'.
  APPEND IT_EVENTS_0 TO IT_EVENTS.

ENDFORM.                    " P1000_APPEND_IT_EVENTS

*&---------------------------------------------------------------------*
*&      Form  ALV_F4
*&---------------------------------------------------------------------*
FORM ALV_F4.

  ALV_VARIANT-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT = ALV_VARIANT
            I_SAVE     = 'A'
       IMPORTING
            ES_VARIANT = ALV_VARIANT
       EXCEPTIONS
            NOT_FOUND  = 2.
  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    P_VARI = ALV_VARIANT-VARIANT.
  ENDIF.

ENDFORM.                                                    " ALV_F4

*&--------------------------------------------------------------------
*&      Form  PF_STATUS
*&--------------------------------------------------------------------
FORM SET_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'ZIMT0'.

ENDFORM.

*&--------------------------------------------------------------------
*&      Form  USER_COMMAND
*&--------------------------------------------------------------------
FORM USER_COMMAND USING    L_UCOMM LIKE SY-UCOMM
                           L_SELFIELD TYPE SLIS_SELFIELD.

  CASE L_UCOMM.
      "--------------------------------------
      " Create Delivery Order
      "--------------------------------------
    WHEN 'CRDO'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES GE 1.
        PERFORM   P2000_PLANT_CHECK.
        PERFORM   P2000_DOHD_CHECK.
      ENDIF.

      "--------------------------------------
      " B/L Document Display
      "--------------------------------------
    WHEN 'DSBL'.
      READ TABLE IT_TAB INDEX L_SELFIELD-TABINDEX.
      CHECK SY-SUBRC EQ 0.
      PERFORM  P2000_DISPLAY_BL USING IT_TAB-ZFHBLNO.

      "--------------------------------------
      " All Record Selection
      "--------------------------------------
    WHEN 'MKAL'.
      LOOP AT IT_TAB.
        IT_TAB-MARK  =  'X'.
        MODIFY  IT_TAB  TRANSPORTING MARK.
      ENDLOOP.
      L_SELFIELD-REFRESH = 'X'.

      "--------------------------------------
      " All Record Deselection
      "--------------------------------------
    WHEN 'MKLO'.
      LOOP AT IT_TAB WHERE MARK = 'X'.
        IT_TAB-MARK = ' '.
        MODIFY IT_TAB TRANSPORTING MARK.
      ENDLOOP.
      L_SELFIELD-REFRESH = 'X'.

      "--------------------------------------
      " Container Detail Information Display
      "--------------------------------------
    WHEN '&IC1'.
      READ TABLE IT_TAB INDEX L_SELFIELD-TABINDEX.
      CHECK SY-SUBRC EQ 0.
      PERFORM  P2000_DETAIL_DISPLAY.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  P2000_DETAIL_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_DETAIL_DISPLAY.

  INCLUDE = 'POPU'.
  CALL SCREEN 0100 STARTING AT  5     3
                   ENDING   AT  105   20.
  CLEAR : INCLUDE.

ENDFORM.                    " P2000_DETAIL_DISPLAY
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Container Detail Information'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.

  LEAVE TO LIST-PROCESSING.
  CASE INCLUDE.
    WHEN 'POPU'.
      PERFORM  P1000_WRITE_CST_SCR0100.
  ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_CST_SCR0100
*&---------------------------------------------------------------------*
FORM P1000_WRITE_CST_SCR0100.

  PERFORM  P1000_WRITE_TITLE_SCR0100.

  SORT IT_TAB_DETAIL BY TRAID KDMAT.

  LOOP  AT  IT_TAB_DETAIL  WHERE  TRAID EQ IT_TAB-TRAID.

    W_LINE  =  W_LINE  +  1.
    W_MOD   =  W_LINE  MOD 2.

    " Line Write.
    PERFORM P2000_LINE_WRITE_DATA_SCR0100.

  ENDLOOP.

  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P1000_WRITE_CST_SCR0100

*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_TITLE_SCR0100
*&---------------------------------------------------------------------*
FORM P1000_WRITE_TITLE_SCR0100.

  WRITE: / 'Container No : '   NO-GAP,
           IT_TAB-TRAID        NO-GAP.

  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : /                                          SY-VLINE NO-GAP,
         (25) 'Case Number'                  NO-GAP, SY-VLINE NO-GAP,
         (40) 'Material'                     NO-GAP, SY-VLINE NO-GAP,
         (23) 'Total Qty'                    NO-GAP, SY-VLINE NO-GAP.

  WRITE : / SY-ULINE.

ENDFORM.                    " P1000_WRITE_TITLE_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_WRITE_DATA_SCR0100
*&---------------------------------------------------------------------*
FORM P2000_LINE_WRITE_DATA_SCR0100.

  IF W_MOD EQ 1.
    FORMAT RESET.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT RESET.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE :/                                           SY-VLINE NO-GAP,
         (25) IT_TAB_DETAIL-KDMAT            NO-GAP, SY-VLINE NO-GAP,
         (17) IT_TAB_DETAIL-MATNR            NO-GAP,
         (23) IT_TAB_DETAIL-MAKTX            NO-GAP, SY-VLINE NO-GAP,
         (18) IT_TAB_DETAIL-LFIMG UNIT IT_TAB_DETAIL-MEINS    NO-GAP,
         (05) IT_TAB_DETAIL-MEINS            NO-GAP, SY-VLINE NO-GAP.

ENDFORM.                    " P2000_LINE_WRITE_DATA_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLAY_BL
*&---------------------------------------------------------------------*
FORM P2000_DISPLAY_BL USING    P_ZFHBLNO.

  SET  PARAMETER ID 'ZPHBLNO' FIELD  P_ZFHBLNO.
  SET  PARAMETER ID 'ZPBLNO'  FIELD  ''.
  SET  PARAMETER ID 'BES'     FIELD  ''.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISPLAY_BL
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  CLEAR : IT_DOHD, IT_DOIT. REFRESH : IT_DOHD, IT_DOIT.
  CLEAR W_SELECTED_LINES.

  LOOP  AT  IT_TAB  WHERE  MARK  EQ  'X'.
    MOVE-CORRESPONDING IT_TAB  TO  IT_DOHD.
    APPEND  IT_DOHD.
    LOOP  AT  IT_TAB_DETAIL WHERE TRAID EQ IT_TAB-TRAID.
      MOVE-CORRESPONDING IT_TAB_DETAIL TO IT_DOIT.
      APPEND  IT_DOIT.
    ENDLOOP.
  ENDLOOP.

  W_ERR_CHK = 'N'.
  DESCRIBE  TABLE  IT_DOHD   LINES  W_SELECTED_LINES.
  IF W_SELECTED_LINES EQ 0.
    MESSAGE S951.
    W_ERR_CHK = 'Y'.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_PLANT_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PLANT_CHECK.

  DATA : WL_PLANT  LIKE LIPS-WERKS.

  READ TABLE IT_DOIT INDEX 1.
  MOVE  IT_DOIT-WERKS  TO  WL_PLANT.

  LOOP AT IT_DOIT.
    IF WL_PLANT NE IT_DOIT-WERKS.
      MESSAGE  I399(ZIM1).
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_PLANT_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_DOHD_CHECK
*&---------------------------------------------------------------------*
FORM P2000_DOHD_CHECK.

  DATA : WL_ANSWER,
         WL_PKCN(3),
         WL_PK_TEXT(70).

  CLEAR : WL_ANSWER, WL_PK_TEXT.

  WL_PK_TEXT = 'Total Container Quanity: '.
  WRITE W_SELECTED_LINES  NO-ZERO TO WL_PKCN.

  CONCATENATE WL_PK_TEXT WL_PKCN INTO WL_PK_TEXT.

  CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
       EXPORTING
            TEXTLINE1 = WL_PK_TEXT
            TEXTLINE2 = ' '
            TITEL     = 'Do you want to create delivery order'
       IMPORTING
            ANSWER    = WL_ANSWER.

  CASE WL_ANSWER.
    WHEN 'J'.
      PERFORM   P2000_EXECUTE_DO.

      " Refresh
      MOVE 0 TO SY-LSIND.

      " Data Read
      PERFORM   P1000_GET_IT_TAB       USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM   P1000_CONTAINER_GROUP.

      " Data Rewrite.
      PERFORM   P1000_APPEND_IT_FIELDCAT.
      PERFORM   P1000_APPEND_IT_EVENTS.
      PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

    WHEN 'N'.
      MESSAGE S148(ZIM1) WITH 'Creation Delivery Order'.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_DOHD_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_EXECUTE_DO
*&---------------------------------------------------------------------*
FORM P2000_EXECUTE_DO.

  EXPORT IT_DOHD            TO MEMORY ID 'DO_HD'.
  EXPORT IT_DOIT            TO MEMORY ID 'DO_IT'.

  CALL TRANSACTION 'ZIMT1'  AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_EXECUTE_DO
