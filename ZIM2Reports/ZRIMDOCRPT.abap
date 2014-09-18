*&---------------------------------------------------------------------*
*& Report  ZRIMDOCRPT                                                  *
*&---------------------------------------------------------------------*
*&  Program Name : Progress Status by P/O                              *
*&  Created by   : Na Shin Ho                                          *
*&  Created on   : 2000.06.16                                          *
*&---------------------------------------------------------------------*
*&  [Change Log] :
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMDOCRPT   MESSAGE-ID ZIM
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
*      Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMDOCRPTTOP.
INCLUDE   ZRIMUTIL01.

*-----------------------------------------------------------------------
*      Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.               " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS
                          NO INTERVALS NO-EXTENSION," Company Code
                S_EBELN   FOR ZTREQHD-EBELN         " P/O Number
                          MEMORY ID BES,
                S_REQDT   FOR ZTREQST-ZFREQDT,      " Requested Open dat
                S_OPNDT   FOR ZTREQST-ZFOPNDT,      " Open Date
                S_OPBN    FOR ZTREQHD-ZFOPBN,       " Open Bank
                S_MATGB   FOR ZTREQHD-ZFMATGB,      " Material Type
                S_REQTY   FOR ZTREQHD-ZFREQTY,      " Payment Type
                S_WERKS   FOR ZTREQHD-ZFWERKS,      " Representive plant
                S_EKORG   FOR ZTREQST-EKORG,        " Purch. Org.
                S_ERNAM   FOR ZTREQST-ERNAM         " Created by.
                          NO  INTERVALS.
PARAMETERS :    P_NAME    LIKE USR02-BNAME.         " in Charge
SELECT-OPTIONS: S_LIFNR   FOR ZTREQHD-LIFNR,        " vendor
                S_EKGRP   FOR ZTREQST-EKGRP,        " Purch. Grp.
                S_HBLNO   FOR ZTBL-ZFHBLNO,         " House B/L
                S_CONT    FOR LIKP-TRAID,           " Container No
                S_REQNO   FOR ZTREQHD-ZFREQNO       " Import Request No
                          MEMORY ID ZPREQNO,
                S_OPNNO   FOR ZTREQHD-ZFOPNNO
                          MEMORY ID ZPOPNNO ,       " Approve No
                S_DOCST   FOR ZTREQST-ZFDOCST.      " Document Stataus
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
*      AT SELECTION-SCREEN.
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.
  IF  S_REQDT IS INITIAL AND S_MATGB  IS INITIAL AND
      S_REQTY IS INITIAL AND S_WERKS  IS INITIAL AND
      S_EKORG IS INITIAL AND P_NAME   IS INITIAL AND
      S_EBELN IS INITIAL AND S_LIFNR  IS INITIAL AND
      S_EKGRP IS INITIAL AND S_REQNO  IS INITIAL AND
      S_DOCST IS INITIAL AND S_OPBN   IS INITIAL AND
      S_OPNDT IS INITIAL AND S_ERNAM  IS INITIAL AND
      S_OPNNO IS INITIAL .
    MESSAGE E193.
  ENDIF.

*-----------------------------------------------------------------------
* PARAMETER Initialization Setting
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* HEADER Write
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM P3000_WRITE_HEADER.

*-----------------------------------------------------------------------
*      START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

*>> Import Request Data Selection
  PERFORM   P1000_READ_REQ_DATA       USING   W_ERR_CHK.
  CHECK     W_ERR_CHK  NE  'Y'.

*>> P/O Data Selection
  PERFORM   P1000_READ_PO_DATA.

*>> MotherShip Data Selection
  PERFORM   P1000_READ_MS_DATA.

*>> Insuarance Data Selection
  PERFORM   P1000_READ_INS_DATA.

*>> AMEND DATA Selection
  PERFORM   P1000_READ_AMEND_DATA.

*>> COMMERCIAL INVOICE DATA Selection
  PERFORM   P1000_READ_CIV_DATA.

*>> B/L Data Selection
  PERFORM   P1000_READ_BL_DATA.

*>> LG DATA Selection.
  PERFORM   P1000_READ_LG_DATA.

*>> Bonded-in Data Selection
  PERFORM   P1000_READ_INR_DATA.

*>> Bonded-out Data Selection
  PERFORM   P1000_READ_OUR_DATA.

*>> Cargo DATA Selection
  PERFORM   P1000_READ_CG_DATA.

*>> BL Customs Clearance Data Selection
  PERFORM   P1000_READ_CUIV_DATA.

*>> Customs Clearance Request Data Selection
  PERFORM   P1000_READ_IV_DATA.

*>> Customs Declaration Data Selection
  PERFORM   P1000_READ_ZTIDR_DATA.

*>> G/R Data Selection
  PERFORM   P1000_READ_IN_DATA.

*-----------------------------------------------------------------------
*      END OF SELECTION
*-----------------------------------------------------------------------
END-OF-SELECTION.

* GUI Status
  SET  TITLEBAR 'ZIM91'.               " TITLE BAR
  SET  PF-STATUS 'ZIM91'.              " PF-STATUS

*>> INTERNAL TABLE SORT.
  PERFORM   P2000_SORT_DATA.
  PERFORM   P3000_WRITE_ALL_DATA.

*-----------------------------------------------------------------------
*      User Command
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

*-----------------------------------------------------------------------
* LINE Double Click -> Document DISPLAY.
*-----------------------------------------------------------------------
AT LINE-SELECTION.

  DATA : L_TEXT(30).

  GET CURSOR FIELD L_TEXT.

*>> PO & Vendor DISPLAY.
  IF L_TEXT(7)  EQ  'IT_EKKO' AND L_TEXT(13)  NE  'IT_EKKO-LIFNR'.
    SET  PARAMETER  ID  'BES'  FIELD  IT_EKKO-EBELN.
    CALL  TRANSACTION  'ME23N'  AND  SKIP FIRST SCREEN.
  ELSEIF  L_TEXT(13)  EQ  'IT_EKKO-LIFNR'.

    SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
    SET PARAMETER ID 'LIF' FIELD IT_EKKO-LIFNR.
    SET PARAMETER ID 'EKO' FIELD ''.
    EXPORT 'LIF'   TO MEMORY ID 'LIF'.
    EXPORT 'EKO'   TO MEMORY ID 'EKO'.

    CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.

  ENDIF.
*>> Import Request Document DISPLAY
  IF L_TEXT(10)  EQ  'IT_ZTREQHD'.
    SET PARAMETER ID 'ZPOPNNO'  FIELD  SPACE.
    SET PARAMETER ID 'BES'      FIELD  SPACE.
    SET PARAMETER ID 'ZPREQNO'  FIELD  IT_ZTREQHD-ZFREQNO.
    CALL TRANSACTION 'ZIM03'    AND SKIP FIRST SCREEN.
  ENDIF.
*>> Mother Ship DISPLAY
  IF L_TEXT(9)  EQ  'IT_ZTMSHD'.
    SET PARAMETER ID 'ZPMSNO' FIELD IT_ZTMSHD-ZFMSNO.
  ENDIF.
*>> Insurance DISPLAY.
  IF L_TEXT(8)  EQ  'IT_ZTINS'.
    IF  IT_ZTINS-ZFAMDNO  GT  '00000'.
      SET PARAMETER ID 'ZPOPNNO'  FIELD  SPACE.
      SET PARAMETER ID 'BES'      FIELD  SPACE.
      SET PARAMETER ID 'ZPREQNO'  FIELD  IT_ZTINS-ZFREQNO.
      SET PARAMETER ID 'ZPINSEQ'  FIELD  IT_ZTINS-ZFINSEQ.
      SET PARAMETER ID 'ZPAMDNO'  FIELD  IT_ZTINS-ZFAMDNO.
      CALL  TRANSACTION 'ZIM47'   AND SKIP FIRST SCREEN.
    ELSE.
      SET PARAMETER ID 'ZPOPNNO'  FIELD  SPACE.
      SET PARAMETER ID 'BES'      FIELD  SPACE.
      SET PARAMETER ID 'ZPREQNO'  FIELD  IT_ZTINS-ZFREQNO.
      SET PARAMETER ID 'ZPINSEQ'  FIELD  IT_ZTINS-ZFINSEQ.
      CALL  TRANSACTION 'ZIM43'   AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.
*>> AMEND DISPLAY.
  IF L_TEXT(10)  EQ  'IT_ZTREQST'.
    SET PARAMETER ID 'ZPOPNNO'  FIELD  SPACE.
    SET PARAMETER ID 'BES'      FIELD  SPACE.
    SET PARAMETER ID 'ZPREQNO'  FIELD  IT_ZTREQST-ZFREQNO.
    SET PARAMETER ID 'ZPAMDNO'  FIELD  IT_ZTREQST-ZFAMDNO.
    CALL TRANSACTION 'ZIM13'    AND SKIP FIRST SCREEN.
  ENDIF.
*>> Commercial Invoice DISPLAY.
  IF L_TEXT(6)  EQ  'IT_CIV'.
    SET PARAMETER ID 'ZPCIVNO'  FIELD  IT_CIV-ZFCIVNO.
    SET PARAMETER ID 'ZPCIVRN'  FIELD  IT_CIV-ZFCIVRN.
    CALL TRANSACTION 'ZIM37'    AND SKIP FIRST SCREEN.
  ENDIF.
*>> BL DISPLAY
  IF L_TEXT(5)  EQ  'IT_BL'.
    SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
    SET PARAMETER ID 'ZPBLNO'  FIELD  IT_BL-ZFBLNO.
    CALL TRANSACTION 'ZIM23'   AND SKIP FIRST SCREEN.
  ENDIF.
*>> LG DISPLAY
  IF L_TEXT(7)  EQ  'IT_ZTLG'.
    SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
    SET PARAMETER ID 'ZPBLNO'  FIELD  IT_ZTLG-ZFBLNO.
    SET PARAMETER ID 'ZPLGSEQ' FIELD  IT_ZTLG-ZFLGSEQ.
    CALL TRANSACTION 'ZIM28'   AND SKIP FIRST SCREEN.
  ENDIF.
*>> Cargo DISPLAY.
  IF L_TEXT(5)  EQ  'IT_CG'.
    SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
    SET PARAMETER ID 'ZPBLNO'  FIELD  IT_CG-ZFBLNO.
    SET PARAMETER ID 'ZPCGPT'  FIELD  IT_CG-ZFCGPT.
    SET PARAMETER ID 'ZPCGNO'  FIELD  IT_CG-ZFCGNO.
    CALL TRANSACTION 'ZIM83'   AND  SKIP FIRST SCREEN.
  ENDIF.
*>> Bonded-in transport DISPLAY
  IF L_TEXT(6)  EQ  'IT_INR'.
    SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
    SET PARAMETER ID 'ZPBLNO'  FIELD  IT_INR-ZFBLNO.
    SET PARAMETER ID 'ZPBTSEQ' FIELD  IT_INR-ZFBTSEQ.
    CALL TRANSACTION 'ZIMI8'   AND SKIP FIRST SCREEN.
  ENDIF.
*>> Bonded-out Document DISPLAY
  IF L_TEXT(6)  EQ  'IT_OUR'.
    SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
    SET PARAMETER ID 'ZPBLNO'  FIELD  IT_OUR-ZFBLNO.
    SET PARAMETER ID 'ZPBTSEQ' FIELD  IT_OUR-ZFBTSEQ.
    CALL TRANSACTION 'ZIMO3'   AND SKIP FIRST SCREEN.
  ENDIF.
*>> Customs Clearance/GR Request DISPLAY
  IF L_TEXT(5)  EQ  'IT_IV'.
    SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
    SET PARAMETER ID 'ZPBLNO'  FIELD  SPACE.
    SET PARAMETER ID 'ZPIVNO'  FIELD  IT_IV-ZFIVNO.
    CALL TRANSACTION 'ZIM33'   AND  SKIP FIRST SCREEN.
  ENDIF.
*>> Customs Clearance Request DISPLAY
  IF L_TEXT(9)  EQ  'IT_ZTCUIV'.
    SET PARAMETER ID 'ZPIVNO'  FIELD  IT_ZTCUIV-ZFIVNO.
    CALL TRANSACTION 'ZIM69'   AND  SKIP FIRST SCREEN.
  ENDIF.
*>> Customs Clearance DISPLAY
  IF L_TEXT(8)  EQ  'IT_ZTIDS'.
    SET PARAMETER ID 'ZPENTNO' FIELD  SPACE.
    SET PARAMETER ID 'ZPIVNO'  FIELD  IT_ZTIDS-ZFIVNO.
    SET PARAMETER ID 'ZPCLSEQ' FIELD  IT_ZTIDS-ZFCLSEQ.
    CALL TRANSACTION 'ZIMCC3'   AND  SKIP FIRST SCREEN.
  ENDIF.
*>> G/R Document DISPLAY.
  IF L_TEXT(5)  EQ  'IT_IN' AND L_TEXT(6) NE 'IT_INR'.
    SET PARAMETER ID 'BUK'    FIELD  IT_IN-BUKRS.
    SET PARAMETER ID 'MBN'    FIELD  IT_IN-MBLNR.
    SET PARAMETER ID 'MJA'    FIELD  IT_IN-MJAHR.
*>> G/R Material Document Display FUNCTION CALL.
    CALL FUNCTION 'MIGO_DIALOG'
       EXPORTING
          I_ACTION                  = 'A04'
          I_REFDOC                  = 'R02'
          I_NOTREE                  = 'X'
          I_SKIP_FIRST_SCREEN       = 'X'
          I_OKCODE                  = 'OK_GO'
          I_MBLNR                   = IT_IN-MBLNR
          I_MJAHR                   = IT_IN-MJAHR
       EXCEPTIONS
          ILLEGAL_COMBINATION       = 1
          OTHERS                    = 2.

  ENDIF.
*-----------------------------------------------------------------------
*      Form  P2000_SET_PARAMETER
*-----------------------------------------------------------------------
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM91'.               " TITLE BAR
ENDFORM.                               " P2000_SET_PARAMETER

*-----------------------------------------------------------------------
*      FORM P3000_WRITE_HEADER
*-----------------------------------------------------------------------
FORM P3000_WRITE_HEADER.
  SKIP 1.
  FORMAT COLOR COL_BACKGROUND  INTENSIFIED  ON.
  WRITE : 55  '  [ Document progress status ] '
           COLOR COL_HEADING INTENSIFIED.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_REQ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P1000_READ_REQ_DATA USING    W_ERR_CHK.

  CLEAR   : W_LINE.
  REFRESH : IT_ST, IT_ZTREQHD.

  MOVE  'N'      TO   W_ERR_CHK.
  CONCATENATE P_NAME '%' INTO P_NAME.

  SELECT  A~ZFREQNO         B~EBELN     MAX( A~ZFREQTY ) AS ZFREQTY
          MAX( A~ZFOPNNO )  AS ZFOPNNO  MAX( A~LIFNR )    AS LIFNR
          MAX( A~ZFMATGB )  AS ZFMATGB  MAX( A~INCO1 )    AS INCO1
          MAX( A~ZTERM )    AS ZTERM    MAX( A~ZFOPBN )   AS ZFOPBN
          MAX( A~ZFOPAMT )  AS ZFOPAMT  MAX( A~ZFLASTSD ) AS ZFLASTSD
          MAX( A~ZFLASTED ) AS ZFLASTED MAX( A~ZFLASTAM ) AS ZFLASTAM
          MAX( A~WAERS )    AS WAERS
  INTO    CORRESPONDING FIELDS OF TABLE  IT_ZTREQHD
  FROM    ZTREQHD  AS  A  INNER  JOIN  ZTREQIT  AS  B
  ON      A~ZFREQNO    EQ   B~ZFREQNO
  WHERE   A~ZFREQNO    IN   S_REQNO
  AND     A~BUKRS      IN   S_BUKRS
  AND     B~EBELN      IN   S_EBELN
  AND     A~LIFNR      IN   S_LIFNR
  AND     A~ZFMATGB    IN   S_MATGB
  AND     A~ZFWERKS    IN   S_WERKS
  AND     A~ZFOPBN     IN   S_OPBN
  AND     A~ZFOPNNO    IN S_OPNNO
  GROUP BY
          A~ZFREQNO  B~EBELN.

  LOOP  AT  IT_ZTREQHD.

    W_TABIX  =  SY-TABIX.

    SELECT  ZFREQNO  MAX( ZFOPNDT ) AS ZFOPNDT  MAX( EKGRP ) AS EKGRP
    INTO    CORRESPONDING FIELDS OF TABLE IT_ST
    FROM    ZTREQST
    WHERE   ZFREQTY    IN   S_REQTY
    AND     ZFREQNO    EQ   IT_ZTREQHD-ZFREQNO
    AND     ZFDOCST    IN   S_DOCST
    AND     CDAT       IN   S_REQDT
    AND     ZFOPNDT    IN   S_OPNDT
    AND     ERNAM      IN   S_ERNAM
    AND     ZFOPNNM    LIKE P_NAME
    AND     EKGRP      IN   S_EKGRP
    GROUP BY
            ZFREQNO.

    IF SY-SUBRC NE 0.
      DELETE  IT_ZTREQHD  INDEX  W_TABIX.
      CONTINUE.
    ENDIF.

    MOVE : IT_ST-ZFOPNDT  TO  IT_ZTREQHD-ZFOPNDT,
           IT_ST-EKGRP    TO  IT_ZTREQHD-EKGRP.

    MODIFY IT_ZTREQHD INDEX  W_TABIX.
  ENDLOOP.

  DESCRIBE  TABLE  IT_ZTREQHD  LINES  W_LINE.
  IF W_LINE EQ 0.
    MESSAGE   S738.
    MOVE   'Y'       TO      W_ERR_CHK.
  ENDIF.

ENDFORM.                    " P1000_READ_REQ_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

  REFRESH : IT_EKKO.
  MOVE      'N'        TO    W_ERR_CHK.

  SELECT   *   INTO  CORRESPONDING FIELDS OF TABLE IT_EKKO
  FROM    EKKO
  FOR     ALL   ENTRIES  IN  IT_ZTREQHD
  WHERE   EBELN          EQ  IT_ZTREQHD-EBELN
  AND     EKORG          IN  S_EKORG.

  LOOP AT IT_ZTREQHD.

     W_TABIX  =  SY-TABIX.

     IF NOT S_HBLNO[] IS INITIAL.

        CLEAR : W_BL_CNT.
        SELECT COUNT( * )   INTO  W_BL_CNT
        FROM   ZTBL  AS   A INNER JOIN ZTBLIT AS B
        ON     A~ZFBLNO     EQ    B~ZFBLNO
        WHERE  A~ZFHBLNO    IN    S_HBLNO
        AND    B~EBELN      EQ    IT_ZTREQHD-EBELN.

        IF W_BL_CNT LE 0.
           DELETE  IT_ZTREQHD  INDEX  W_TABIX.
           DELETE  IT_EKKO     WHERE  EBELN  =  IT_ZTREQHD-EBELN.
           CONTINUE.
        ENDIF.
     ENDIF.

     ">> Container No
     IF NOT S_CONT[] IS INITIAL.

        CLEAR : W_TR_CNT.
        SELECT COUNT( * )   INTO  W_TR_CNT
        FROM   LIKP  AS  A  INNER JOIN  LIPS AS B
        ON     A~VBELN      EQ    B~VBELN
        WHERE  A~TRAID      IN    S_CONT
        AND    B~VGBEL      EQ    IT_ZTREQHD-EBELN.

        IF W_TR_CNT LE 0.
           DELETE  IT_ZTREQHD  INDEX  W_TABIX.
           DELETE  IT_EKKO     WHERE  EBELN = IT_ZTREQHD-EBELN.
           CONTINUE.
        ENDIF.
     ENDIF.
  ENDLOOP.

ENDFORM.                    " P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_INS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_INS_DATA.

  REFRESH : IT_ZTINS.

  SELECT   *    INTO  CORRESPONDING FIELDS OF TABLE IT_ZTINS
  FROM    ZTINS
  FOR     ALL    ENTRIES   IN  IT_ZTREQHD
  WHERE   ZFREQNO          EQ  IT_ZTREQHD-ZFREQNO.

ENDFORM.                    " P1000_READ_INS_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_AMEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_AMEND_DATA.

*>> AMEND DATA SELECT.
  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTREQST
  FROM    ZTREQST
  FOR  ALL  ENTRIES  IN  IT_ZTREQHD
  WHERE   ZFREQNO    EQ  IT_ZTREQHD-ZFREQNO
  AND     ZFAMDNO    NE  '00000'.

ENDFORM.                    " P1000_READ_AMEND_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

  REFRESH :  IT_ZTCIV, IT_CIV.

  SELECT  ZFCIVRN  ZFREQNO
  INTO CORRESPONDING FIELDS OF TABLE IT_ZTCIV
  FROM    ZTCIVIT
  FOR     ALL   ENTRIES   IN     IT_ZTREQHD
  WHERE   ZFREQNO         EQ     IT_ZTREQHD-ZFREQNO
  GROUP BY
          ZFCIVRN  ZFREQNO.

  LOOP  AT  IT_ZTCIV.

    CLEAR   ZTCIVHD.
    SELECT  SINGLE *  FROM  ZTCIVHD
    WHERE   ZFCIVRN   EQ    IT_ZTCIV-ZFCIVRN.

    MOVE-CORRESPONDING ZTCIVHD TO  IT_CIV.
    MOVE  IT_ZTCIV-ZFREQNO     TO  IT_CIV-ZFREQNO.

    APPEND  IT_CIV.

  ENDLOOP.

ENDFORM.                    " P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

  REFRESH :  IT_ZTBL, IT_BL.

  SELECT  ZFBLNO  ZFREQNO
  INTO CORRESPONDING FIELDS OF TABLE IT_ZTBL
  FROM    ZTBLIT
  FOR     ALL   ENTRIES   IN     IT_ZTREQHD
  WHERE   ZFREQNO         EQ     IT_ZTREQHD-ZFREQNO
  GROUP BY
          ZFBLNO  ZFREQNO.

  LOOP  AT  IT_ZTBL.

    CLEAR   ZTBL.
    SELECT  SINGLE *  FROM  ZTBL
    WHERE   ZFBLNO    EQ    IT_ZTBL-ZFBLNO.

    MOVE-CORRESPONDING ZTBL    TO  IT_BL.
    MOVE  IT_ZTBL-ZFREQNO      TO  IT_BL-ZFREQNO.

    APPEND  IT_BL.

  ENDLOOP.

ENDFORM.                    " P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_IV_DATA.

  REFRESH : IT_ZTIV, IT_IV.

  SELECT  ZFIVNO    ZFREQNO   ZFBLNO
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZTIV
  FROM    ZTIVIT
  FOR     ALL  ENTRIES  IN  IT_ZTREQHD
  WHERE   ZFREQNO       EQ  IT_ZTREQHD-ZFREQNO
  GROUP BY
          ZFIVNO   ZFREQNO   ZFBLNO.

  LOOP  AT  IT_ZTIV.

    CLEAR  ZTIV.
    SELECT  SINGLE * FROM ZTIV
    WHERE   ZFIVNO   EQ   IT_ZTIV-ZFIVNO.

    MOVE-CORRESPONDING  ZTIV   TO  IT_IV.
    MOVE    IT_ZTIV-ZFREQNO    TO  IT_IV-ZFREQNO.
    MOVE    IT_ZTIV-ZFBLNO     TO  IT_IV-ZFBLNO.

    APPEND  IT_IV.

  ENDLOOP.

ENDFORM.                    " P1000_READ_IT_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIDR_DATA.

  REFRESH : IT_ZTIDS, IT_IDS.

  SELECT   *
  INTO     CORRESPONDING FIELDS OF TABLE IT_IDS
  FROM     ZTIDSUS
  FOR      ALL  ENTRIES IN IT_IV
  WHERE    ZFIVNO       EQ IT_IV-ZFIVNO .

ENDFORM.                    " P1000_READ_ZTIDR_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIDS_DATA.
*
*   SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTIDS
*   FROM    ZTIDS
*   FOR     ALL   ENTRIES  IN  IT_ZTIDR
*   WHERE   ZFBLNO         EQ  IT_ZTIDR-ZFBLNO
*   AND     ZFCLSEQ        EQ  IT_ZTIDR-ZFCLSEQ.
*
ENDFORM.                    " P1000_READ_ZTIDS_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SORT_DATA.

  SORT  IT_EKKO      BY  LIFNR    EBELN.
  SORT  IT_ZTREQHD   BY  LIFNR    EBELN    ZFREQNO.
  SORT  IT_ZTINS     BY  ZFREQNO  ZFAMDNO  ZFINSEQ.
  SORT  IT_ZTREQST   BY  ZFREQNO  ZFAMDNO.
  SORT  IT_CIV       BY  ZFREQNO  ZFCIVRN.
  SORT  IT_BL        BY  ZFREQNO  ZFBLNO.
  SORT  IT_ZTLG      BY  ZFBLNO   ZFLGSEQ.
  SORT  IT_INR       BY  ZFBLNO   ZFBTSEQ.
  SORT  IT_OUR       BY  ZFBLNO   ZFBTSEQ.
  SORT  IT_IV        BY  ZFREQNO  ZFIVNO.
  SORT  IT_ZTIDS     BY  ZFBLNO   ZFCLSEQ.
  SORT  IT_IN        BY  ZFIVNO   MBLNR.

ENDFORM.                    " P2000_SORT_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ALL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_ALL_DATA.

  CLEAR : W_LINE_CNT, W_MOD, W_IV_CNT.
  REFRESH : IT_ZTIVHST.

  LOOP  AT  IT_EKKO.

    IF  SY-TABIX  EQ  1.

      MOVE  IT_EKKO-LIFNR   TO   SV_LIFNR.
*>> Vendor WRITE.
      CLEAR  SV_NAME.
      SELECT SINGLE NAME1 INTO  SV_NAME
      FROM   LFA1         WHERE LIFNR   EQ  SV_LIFNR.
      PERFORM  P4000_WRITE_LIFNR.
    ENDIF.

    IF  IT_EKKO-LIFNR  NE  SV_LIFNR.

*>> Vendor Name SELECT
      CLEAR  SV_NAME.
      SELECT SINGLE NAME1 INTO SV_NAME
      FROM   LFA1   WHERE  LIFNR  EQ  IT_EKKO-LIFNR.
      PERFORM  P4000_WRITE_LIFNR.

      MOVE   IT_EKKO-LIFNR  TO  SV_LIFNR.

    ENDIF.

*>> PO DATA WRITE.
    CLEAR : T024E, T024.
    PERFORM   P4000_WRITE_PO_DATA.

  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ALL_DATA

*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_LIFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_LIFNR.

  SKIP  1.
  FORMAT COLOR COL_HEADING  INTENSIFIED  OFF.
  WRITE : /(14)   'Vendor      : '  ,
           (11)   IT_EKKO-LIFNR     ,
           (30)   SV_NAME      ,
         139(1)   ' '.
  HIDE : IT_EKKO.

ENDFORM.                    " P4000_WRITE_LIFNR
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_PO_DATA.

* Vendor Name SELECT
  CLEAR : LFA1, T001W, W_AMOUNT.
  SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ IT_EKKO-LIFNR.

* P/O Amount SUM.
  SELECT * FROM EKPO  WHERE EBELN  EQ  IT_EKKO-EBELN.
* Representive Goods Description, Plant Name Set
    IF EKPO-EBELP EQ '00010'.
      MOVE : EKPO-TXZ01   TO  W_TEXT,
             EKPO-WERKS   TO  W_WERKS.
    ENDIF.
    W_AMOUNT = W_AMOUNT +
         ( EKPO-MENGE * ( EKPO-BPUMZ / EKPO-BPUMN )
       * ( EKPO-NETPR / EKPO-PEINH ) ).
  ENDSELECT.

  SELECT SINGLE * FROM T001W  WHERE  WERKS EQ W_WERKS.

  FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
  WRITE : /1      'Order       : ',
           15(10) IT_EKKO-EBELN,
           34(10) IT_EKKO-AEDAT,
           45(5)  IT_EKKO-WAERS,
           50(19) W_AMOUNT CURRENCY IT_EKKO-WAERS,
           70(10) IT_EKKO-LIFNR  NO-GAP,
             (20) LFA1-NAME1,
          100(20) W_TEXT,
           120(4) W_WERKS        NO-GAP,
              (16) T001W-NAME1.

  HIDE : IT_EKKO.
  FORMAT COLOR COL_BACKGROUND.
*>> Import Request DATA WRITE.
  LOOP  AT  IT_ZTREQHD  WHERE  EBELN  =  IT_EKKO-EBELN.

    PERFORM   P4000_WRITE_REQHD_DATA.

  ENDLOOP.

ENDFORM.                    " P4000_WRITE_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_REQHD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_REQHD_DATA.

*>> Bank Name SELECT
  CLEAR  LFA1.
  SELECT   SINGLE  *  FROM  LFA1  WHERE  LIFNR  EQ  IT_ZTREQHD-ZFOPBN.
*>> Purchasing Group SELECT
  CLEAR  T024.
  SELECT   SINGLE  *  FROM  T024  WHERE  EKGRP  EQ  IT_ZTREQHD-EKGRP.

  WRITE : /1(14) 'Open        : ',
           15(10) IT_ZTREQHD-ZFREQNO  ,
           26(15) IT_ZTREQHD-ZFOPNNO  ,
           45(10) IT_ZTREQHD-ZFOPNDT  ,
           56(13) IT_ZTREQHD-ZFLASTAM CURRENCY IT_ZTREQHD-WAERS ,
           70(10) IT_ZTREQHD-ZFOPBN   ,
           80(20) LFA1-NAME1          ,
           100(4) IT_ZTREQHD-ZTERM    ,
           105(12) T024-EKNAM          .
  HIDE : IT_ZTREQHD.

*>> Mothership data WRITE.
  LOOP  AT  IT_ZTMSHD   WHERE  ZFREQNO  =  IT_ZTREQHD-ZFREQNO.

    PERFORM   P4000_WRITE_MS_DATA.

  ENDLOOP.

*>> Insurance WRITE.
  LOOP  AT  IT_ZTINS    WHERE  ZFREQNO  =  IT_ZTREQHD-ZFREQNO
                        AND    ZFAMDNO  =  '00000'.

    PERFORM   P4000_WRITE_INS_DATA.

  ENDLOOP.


*>> AMEND WRITE
  LOOP  AT  IT_ZTREQST  WHERE  ZFREQNO  =  IT_ZTREQHD-ZFREQNO.

    PERFORM  P4000_WRITE_AMEND_DATA.

  ENDLOOP.

*>> COMMERCIAL INVOICE WRITE
  LOOP  AT  IT_CIV     WHERE  ZFREQNO  =  IT_ZTREQHD-ZFREQNO.

    PERFORM   P4000_WRITE_CIV_DATA.

  ENDLOOP.

*>> Customs Clearance Request SELECT
  IF IT_ZTREQHD-ZFREQTY  EQ  'LO'  OR  IT_ZTREQHD-ZFREQTY  EQ  'PU'.
    LOOP  AT  IT_IV  WHERE   ZFREQNO  EQ   IT_ZTREQHD-ZFREQNO.

      PERFORM   P4000_WRITE_IV_DATA.

    ENDLOOP.
  ENDIF.

*>> BL DATA WRITE.
  LOOP  AT  IT_BL     WHERE   ZFREQNO  =  IT_ZTREQHD-ZFREQNO.

    PERFORM   P4000_WRITE_BL_DATA.

  ENDLOOP.

ENDFORM.                    " P4000_WRITE_REQHD_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_AMEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_AMEND_DATA.

  WRITE : /(14) 'Amend       : ',
           15(10) IT_ZTREQST-ZFAMDNO,
           45(10) IT_ZTREQST-ZFOPNDT,
           56(13) IT_ZTREQST-ZFOPAMT  CURRENCY  IT_ZTREQST-WAERS.
  HIDE : IT_ZTREQST.

  LOOP  AT  IT_ZTINS  WHERE  ZFREQNO    =   IT_ZTREQST-ZFREQNO
                      AND    ZFAMDNO    =   IT_ZTREQST-ZFAMDNO.

    PERFORM   P4000_WRITE_INS_DATA.

  ENDLOOP.

ENDFORM.                    " P4000_WRITE_AMEND_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_INS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_INS_DATA.

  W_LINE_CNT  =  W_LINE_CNT  +  1.
  W_MOD       =  W_LINE_CNT  MOD  2.
  IF IT_ZTINS-ZFINSFX = 'X'.
    W_TXT = 'Premium decided'.
  ELSE.
    W_TXT = 'Premium not decided'.
  ENDIF.
  WRITE : /1(14) 'Ins Cvering : ',
           15(10) IT_ZTINS-ZFREQNO,
           26(15) IT_ZTINS-ZFINNO,
           45(10) IT_ZTINS-ZFINSDT,
           56(13) IT_ZTINS-ZFINAMT  CURRENCY  IT_ZTINS-ZFINAMTC,
           80(15) W_TXT.
  HIDE : IT_ZTINS.

ENDFORM.                    " P4000_WRITE_INS_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_CIV_DATA.

  WRITE : /1(20) 'Invoice     : ',
           15(10) IT_CIV-ZFCIVRN,
           26(15) IT_CIV-ZFCIVNO,
           45(10) IT_CIV-ZFCIDT,
           56(13) IT_CIV-ZFIVAMP  CURRENCY  IT_CIV-ZFIVAMC.
  HIDE : IT_CIV.

ENDFORM.                    " P4000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_BL_DATA.
  WRITE : /1(14) 'B/L         : ',
           15(10) IT_BL-ZFBLNO,
           26(15) IT_BL-ZFHBLNO,
           45(10) IT_BL-ZFBLDT,
           56(13) IT_BL-ZFBLAMT  CURRENCY  IT_BL-ZFBLAMC,
           80(35) IT_BL-ZFCARNM.
  HIDE : IT_BL.

*>> LG 자료 SELECT.
  LOOP  AT  IT_ZTLG  WHERE  ZFBLNO  EQ  IT_BL-ZFBLNO.

    PERFORM   P4000_WRITE_LG_DATA.

  ENDLOOP.

*>> 하역 정보 SELECT
  LOOP  AT  IT_CG  WHERE   ZFBLNO  EQ  IT_BL-ZFBLNO.

    PERFORM   P4000_WRITE_CG_DATA.

  ENDLOOP.

*>> 반입, 반출 정보 SELECT
  LOOP  AT  IT_INR  WHERE  ZFBLNO  EQ  IT_BL-ZFBLNO.

    PERFORM   P4000_WRITE_INR_DATA.

  ENDLOOP.

*>> 통관요청 자료 SELECT
  LOOP  AT  IT_IV  WHERE   ZFBLNO  EQ   IT_BL-ZFBLNO.

    W_IV_CNT  =  W_IV_CNT + 1.
    PERFORM   P4000_WRITE_IV_DATA.

  ENDLOOP.

*>> 과세통관 자료 검색.
  IF W_IV_CNT LT 1.

    LOOP  AT  IT_ZTCUIV  WHERE   ZFBLNO  EQ   IT_BL-ZFBLNO.

      PERFORM   P4000_WRITE_CUIV_DATA.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " P4000_WRITE_BL_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_IV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_IV_DATA.

*>> Clearence indicator GET
  CLEAR : SV_TEXT, SV_TEXT1, SV_TEXT2.
  CASE  IT_IV-ZFCLCD.
    WHEN  'A'.
      MOVE  'Bonded transport'  TO   SV_TEXT.
    WHEN  'C'.
      MOVE  'FTZ Clearance'     TO   SV_TEXT.
    WHEN  'X'.
      MOVE  'Not object'        TO   SV_TEXT.
    WHEN  'B'.
      MOVE  'Taxation'          TO   SV_TEXT.
  ENDCASE.
*>> Clearance status GET
  CASE  IT_IV-ZFCUST.
    WHEN  '1'.
      MOVE  'created declartn req' TO  SV_TEXT1.
    WHEN  '2'.
      MOVE  'To request declaratn' TO  SV_TEXT1.
    WHEN  '3'.
      MOVE  'In request declaratn' TO  SV_TEXT1.
    WHEN  'Y'.
      MOVE  'Cleared'              TO  SV_TEXT1.
    WHEN  'N'.
      MOVE  'Not object'           TO  SV_TEXT1.
  ENDCASE.
*>> Good Receipt Status SET
  CASE   IT_IV-ZFGRST.
    WHEN  'Y'.
      MOVE  'G/R Completed'        TO  SV_TEXT2.
    WHEN  'N'.
      MOVE  'To G/R'               TO  SV_TEXT2.
    WHEN  'X'.
      MOVE  'Not object'           TO  SV_TEXT2.
  ENDCASE.

  IF IT_ZTREQHD-ZFREQTY  EQ  'LO'  OR  IT_ZTREQHD-ZFREQTY EQ 'PU'.
    WRITE : /1(14) 'G/R request : ',
             15(10) IT_IV-ZFIVNO,
             45(10) IT_IV-ZFCCDT,
             56(13) IT_IV-ZFIVAMT  CURRENCY  IT_IV-ZFIVAMC.
    HIDE : IT_IV.

*>> G/R TABLE WRITE
    LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IV-ZFIVNO.
      PERFORM   P4000_WRITE_GR_DATA.
    ENDLOOP.
  ELSE.
    SELECT SINGLE * FROM ZTIMIMG10
    WHERE  ZFCUT  = IT_IV-ZFCUT.

    SELECT SINGLE NAME1 INTO W_CUT_NAME FROM LFA1
    WHERE  LIFNR  = ZTIMIMG10-ZFVEN.

    WRITE : /1(14) 'Clearnce req: ',
             15(10) IT_IV-ZFIVNO,
             45(10) IT_IV-ZFCCDT,
             56(13) IT_IV-ZFIVAMT  CURRENCY  IT_IV-ZFIVAMC,
                    W_CUT_NAME.
    HIDE : IT_IV.

  ENDIF.

*>> Customs Declaration WRITE.
  LOOP  AT  IT_ZTIDS  WHERE  ZFIVNO   =   IT_IV-ZFIVNO.
    PERFORM   P4000_WRITE_IDR_DATA.
  ENDLOOP.

ENDFORM.                    " P4000_WRITE_IV_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_IDR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_IDR_DATA.
  SELECT SINGLE * FROM ZTIMIMG02
  WHERE  ZFCOTM = IT_ZTIDS-ZFINRC.

  SELECT SINGLE NAME1 INTO W_INRC_NAME FROM LFA1
  WHERE  LIFNR  = ZTIMIMG02-ZFVEN.

  WRITE : /1(14) 'Clearance   : ',
           15(10) IT_ZTIDS-ZFIVNO,
           26(15) IT_ZTIDS-ZFIDRNO,
           45(10) IT_ZTIDS-ZFIDSDT,
           56(13) IT_ZTIDS-ZFSTAMT  CURRENCY  IT_ZTIDS-ZFSTAMC,
                  W_INRC_NAME.
  HIDE : IT_ZTIDS.

*>> G/R Data SELECT & WRITE.
  LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IDS-ZFIVNO.
    PERFORM   P4000_WRITE_GR_DATA.
  ENDLOOP.

ENDFORM.                    " P4000_WRITE_IDR_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_LG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_LG_DATA.

  REFRESH : IT_ZTLG.

  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTLG
  FROM    ZTLG
  FOR     ALL  ENTRIES   IN  IT_BL
  WHERE   ZFBLNO         EQ  IT_BL-ZFBLNO.

ENDFORM.                    " P1000_READ_LG_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_INR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_INR_DATA.

  REFRESH : IT_INR.

  SELECT  *  INTO  CORRESPONDING  FIELDS  OF  TABLE  IT_INR
  FROM    ZTBLINR
  FOR     ALL  ENTRIES  IN  IT_BL
  WHERE   ZFBLNO        EQ  IT_BL-ZFBLNO.

ENDFORM.                    " P1000_READ_INR_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_OUR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_OUR_DATA.

  REFRESH : IT_OUR.

  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_OUR
  FROM    ZTBLOUR
  FOR     ALL   ENTRIES  IN  IT_BL
  WHERE   ZFBLNO         EQ  IT_BL-ZFBLNO.

ENDFORM.                    " P1000_READ_OUR_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_LG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_LG_DATA.

  CLEAR : LFA1.
  SELECT  SINGLE *  FROM  LFA1  WHERE  LIFNR  EQ  IT_ZTLG-ZFCARIR.

  WRITE : /1(14) 'LG          : ',
           15(10) IT_ZTLG-ZFLGSEQ,
           26(15) IT_ZTLG-ZFLGINO,
           45(10) IT_ZTLG-ZFLGIDT,
           56(13) IT_ZTLG-ZFCIAM  CURRENCY  IT_ZTLG-ZFCIAMC.
  HIDE : IT_ZTLG.

ENDFORM.                    " P4000_WRITE_LG_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_INR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_INR_DATA.

* WRITE : /1 '반입        : ',
  WRITE : /1 'Carry-in    : ',
           15(10) IT_INR-ZFBTSEQ,
           45(10) IT_INR-ZFINDT.
  HIDE : IT_INR.

**>> 반출자료 GET
*   LOOP  AT  IT_OUR  WHERE  ZFBLNO  EQ  IT_INR-ZFBLNO
*                     AND    ZFBTSEQ EQ  IT_INR-ZFBTSEQ.
*
*      PERFORM   P4000_WRITE_OUR_DATA.
*
*   ENDLOOP.
*
ENDFORM.                    " P4000_WRITE_INR_DATA

*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_OUR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_OUR_DATA.

* WRITE : /1(14) '반출        : ',
  WRITE : /1(14) 'Carry-out   : ',
           15(10) IT_OUR-ZFBTSEQ,
           26(15) IT_OUR-ZFOURNO,
           56(13) IT_OUR-ZFOUWT  UNIT  IT_OUR-ZFKG,
                  IT_OUR-ZFKG.
  HIDE : IT_OUR.

ENDFORM.                    " P4000_WRITE_OUR_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_IDS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_IDS_DATA.
*
*   W_LINE_CNT  =  W_LINE_CNT  +  1.
*   W_MOD       =  W_LINE_CNT  MOD  2.
*
*   IF  W_MOD  EQ  1.
*       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
*   ELSE.
*       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
*   ENDIF.
*
*   WRITE : /1(20) '********* 수입면허:',
*            22(10) IT_ZTIDS-ZFBLNO,
*            35(5)  IT_ZTIDS-ZFCLSEQ,
*            49(10) IT_ZTIDS-ZFIDWDT,
*            71(20) IT_ZTIDS-ZFIDRNO,
*            119    IT_ZTIDS-ZFSTAMT  CURRENCY  IT_ZTIDS-ZFSTAMC,
*                   IT_ZTIDS-ZFSTAMC.
*   HIDE : IT_ZTIDS.


ENDFORM.                    " P4000_WRITE_IDS_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_GR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_GR_DATA.

  IF SY-TABIX  EQ  1.
    PERFORM   P4000_WRITE_IN_DATA.
    MOVE  IT_IN-MBLNR  TO  SV_MBLNR.
  ENDIF.

  IF  SV_MBLNR  NE  IT_IN-MBLNR.
    PERFORM   P4000_WRITE_IN_DATA.
    MOVE  IT_IN-MBLNR  TO  SV_MBLNR.
  ENDIF.

ENDFORM.                    " P4000_WRITE_GR_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CG_DATA.

  REFRESH :  IT_ZTCG, IT_CG.

  SELECT  ZFCGNO   ZFBLNO
  INTO CORRESPONDING FIELDS OF TABLE IT_ZTCG
  FROM    ZTCGIT
  FOR     ALL   ENTRIES   IN     IT_BL
  WHERE   ZFBLNO          EQ     IT_BL-ZFBLNO
  GROUP BY
          ZFCGNO  ZFBLNO.

  LOOP  AT  IT_ZTCG.

    CLEAR   ZTCGHD.
    SELECT  SINGLE *  FROM  ZTCGHD
    WHERE   ZFCGNO    EQ    IT_ZTCG-ZFCGNO.

    MOVE-CORRESPONDING ZTCGHD  TO  IT_CG.
    MOVE  IT_ZTCG-ZFBLNO       TO  IT_CG-ZFBLNO.

    APPEND  IT_CG.

  ENDLOOP.

ENDFORM.                    " P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_CG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_CG_DATA.

* 하역금액 GET!
  CLEAR W_AMOUNT.
  SELECT * FROM ZTCGIT  WHERE ZFCGNO  EQ  IT_CG-ZFCGNO.
    W_AMOUNT = W_AMOUNT +
         ( ZTCGIT-CGMENGE * ( ZTCGIT-NETPR / ZTCGIT-PEINH ) ).
  ENDSELECT.
* WRITE : /1(14) '하역        : ',
  WRITE : /1(14) 'Unloading   : ',
           15(10) IT_CG-ZFCGNO,
           45(10) IT_CG-ZFARVLDT,
           56(13) W_AMOUNT   CURRENCY  IT_EKKO-WAERS.

  HIDE : IT_CG.

ENDFORM.                    " P4000_WRITE_CG_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CUIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CUIV_DATA.

  REFRESH : IT_ZTCUIV.

  SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTCUIV
  FROM   ZTCUCLIV
  FOR    ALL  ENTRIES  IN  IT_BL
  WHERE  ZFBLNO        EQ  IT_BL-ZFBLNO.

ENDFORM.                    " P1000_READ_CUIV_DATA

*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_CUIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_CUIV_DATA.

* WRITE : /1(20) '통관요청          : ',
  WRITE : /1(20) 'Clearance request : ',
           22(10) IT_ZTCUIV-ZFIVNO,
           34(10) IT_ZTCUIV-ZFCCDT,
           50     IT_ZTCUIV-ZFIVAMT  CURRENCY  IT_ZTCUIV-ZFIVAMC.
  HIDE : IT_ZTCUIV.

*>> 수입신고 자료 WRITE.
  LOOP  AT  IT_ZTIDS  WHERE  ZFIVNO   =   IT_IV-ZFIVNO.

    PERFORM   P4000_WRITE_IDR_DATA.

  ENDLOOP.

ENDFORM.                    " P4000_WRITE_CUIV_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_IN_DATA.

  REFRESH IT_IN.

  LOOP AT IT_IV WHERE ZFGRST EQ 'Y'.

    MOVE-CORRESPONDING  IT_IV  TO  IT_IN.

    SELECT  SINGLE * FROM ZTIVHST
    WHERE   ZFIVNO   EQ   IT_IV-ZFIVNO
    AND     ZFIVHST  EQ   ( SELECT MAX( ZFIVHST )
                            FROM   ZTIVHST
                            WHERE  ZFIVNO  EQ  IT_IV-ZFIVNO ).

    MOVE :  ZTIVHST-ZFIVHST    TO  IT_IN-ZFIVHST,
            ZTIVHST-BUDAT      TO  IT_IN-BUDAT,
            ZTIVHST-BLDAT      TO  IT_IN-BLDAT,
            ZTIVHST-BWART      TO  IT_IN-BWART,
            ZTIVHST-MBLNR      TO  IT_IN-MBLNR,
            ZTIVHST-MJAHR      TO  IT_IN-MJAHR.

    APPEND  IT_IN.

  ENDLOOP.

ENDFORM.                    " P1000_READ_IN_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_IN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_IN_DATA.

  WRITE : /1(14) 'Good receipt: ',
           15(10) IT_IN-MBLNR,
           45(10) IT_IN-BUDAT,
           56(13) IT_IN-ZFIVAMT  CURRENCY  IT_IN-ZFIVAMC,
                  IT_IN-ERNAM.

  HIDE : IT_IN.

ENDFORM.                    " P4000_WRITE_IN_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_MS_DATA.

  REFRESH : IT_ZTMSHD.

  SELECT   *    INTO  CORRESPONDING FIELDS OF TABLE IT_ZTMSHD
  FROM    ZTMSHD AS A INNER JOIN ZTMSIT AS B
    ON    A~ZFMSNO = B~ZFMSNO
  FOR     ALL    ENTRIES   IN  IT_ZTREQHD
  WHERE   B~ZFREQNO        EQ  IT_ZTREQHD-ZFREQNO.

ENDFORM.                    " P1000_READ_MS_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_MS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_MS_DATA.

  W_LINE_CNT  =  W_LINE_CNT  +  1.
  W_MOD       =  W_LINE_CNT  MOD  2.

  WRITE : /1(14) 'Mother ship : ',
           15(10) IT_ZTMSHD-ZFMSNO,
           26(15) IT_ZTMSHD-ZFMSNM,
           45(10) IT_ZTMSHD-ZFSHSDF.
  HIDE : IT_ZTMSHD.

ENDFORM.                    " P4000_WRITE_MS_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> Company Code SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
