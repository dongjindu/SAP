*&---------------------------------------------------------------------*
*& Report  ZRIMDOCTREE                                                 *
*&---------------------------------------------------------------------*
*&  Program Name : Hierarchy by P/O                                    *
*&  Created By   : Na Shin Ho INFOLINK Ltd.                            *
*&  Created On   : 2000.04.18                                          *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [Change Log]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMDOCTREE  MESSAGE-ID ZIM
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMDOCTREETOP.
INCLUDE   ZRIMTREECOM.
INCLUDE   ZRIMUTIL01.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_EBELN   FOR ZTREQHD-EBELN     " P/O Number
                          MEMORY ID BES.
SELECT-OPTIONS: S_REQNO   FOR ZTREQHD-ZFREQNO   " Import Request No
                          MEMORY ID ZPREQNO,
                S_OPNNO   FOR ZTREQHD-ZFOPNNO
                          MEMORY ID ZPOPNNO  ,  " L/C No
                S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                S_MATGB   FOR ZTREQHD-ZFMATGB,  " Material Type
                S_REQTY   FOR ZTREQHD-ZFREQTY,  " Import Request Type
                S_EKORG   FOR ZTREQST-EKORG,    " Purch. Org.
                S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
                S_WERKS   FOR ZTREQHD-ZFWERKS,  " Representive plant
                S_HBLNO   FOR ZTBL-ZFHBLNO,     " House B/L
                S_CONT    FOR LIKP-TRAID,       " Container No
                S_REQDT   FOR ZTREQST-ZFREQDT.  " Requested Date
PARAMETERS :    P_NAME    LIKE USR02-BNAME.     " Charge In
SELECT-OPTIONS: S_DOCST   FOR ZTREQST-ZFDOCST.  " Document Status
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
* AT SELECTION-SCREEN.
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.
  IF  S_REQDT IS INITIAL AND S_MATGB  IS INITIAL AND
      S_REQTY IS INITIAL AND S_WERKS  IS INITIAL AND
      S_EKORG IS INITIAL AND P_NAME   IS INITIAL AND
      S_EBELN IS INITIAL AND S_LIFNR  IS INITIAL AND
      S_EKGRP IS INITIAL AND S_REQNO  IS INITIAL AND
      S_DOCST IS INITIAL AND S_OPNNO  IS INITIAL.
    MESSAGE E193.
  ENDIF.

* PARAMETER Setting
INITIALIZATION.
  PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* START OF SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

  REFRESH : IT_TREELIST.    FREE :    IT_TREELIST.

*>> Import Request Data Selection.
  PERFORM   P1000_READ_REQ_DATA       USING   W_ERR_CHK.
  CHECK     W_ERR_CHK  NE  'Y'.

*>> P/O Data Selection
  PERFORM   P1000_READ_PO_DATA.

*>> Insurance Data Selection
  PERFORM   P1000_READ_INS_DATA.

*>> Amend Data Selection
  PERFORM   P1000_READ_AMEND_DATA.

*>> Commercial Invoice Data Selection
  PERFORM   P1000_READ_CIV_DATA.

*>> BL DATA Selection.
  PERFORM   P1000_READ_BL_DATA.

*>> LG DATA Selection
  PERFORM   P1000_READ_LG_DATA.

*>> Bonded-in Data Selection
  PERFORM   P1000_READ_INR_DATA.

*>> Bonded-out Data Selection
  PERFORM   P1000_READ_OUR_DATA.

*>> Cargo Data Selection
  PERFORM   P1000_READ_CG_DATA.

*>> Customs Clearance Request Data Selection
  PERFORM   P1000_READ_CUIV_DATA.

*>> Customs Clearance Request Data Selection.
  PERFORM   P1000_READ_IV_DATA.

*>> Customs Clearance Request Data Selection
  PERFORM   P1000_READ_ZTIDR_DATA.

*>> Customs Clearance Data Selection
  PERFORM   P1000_READ_ZTIDS_DATA.

*>> G/R Data Selection
  PERFORM   P1000_READ_IN_DATA.

*>> INTERNAL TABLE SORT.
  PERFORM   P2000_SORT_DATA.

*>> TREE Structure DATA WRITE.
  PERFORM   P3000_WRITE_ALL_DATA.

*-----------------------------------------------------------------------
* SELECTION END
*-----------------------------------------------------------------------
END-OF-SELECTION.

* Title Text Write.
  SET TITLEBAR  'ZIM93'.
  SET PF-STATUS 'ZIM93'.

* Hierarchy output
  PERFORM HIERARCHY.                   " construct & draw the tree

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'ERLE'.
      PERFORM    P2000_NODE_ACTION.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM93'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  ÇØ´ç È­¸é AUTHORITY CHECK
*-----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'ÀÇ·Ú Release Æ®·£Àè¼Ç'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_FILL_ITAB_HEAD
*&---------------------------------------------------------------------*
FORM P2000_FILL_ITAB_HEAD.

  CLEAR : IT_TREELIST.
  IT_TREELIST-NAME = 'ROOT'.
  IT_TREELIST-COLOR = 3.
  IT_TREELIST-INTENSIV = '0'.
  IT_TREELIST-TEXT = 'Progress hierarchy by import document'.
  IT_TREELIST-TLENGTH = STRLEN( IT_TREELIST-TEXT ).
  IT_TREELIST-TLEVEL = 1.
  IT_TREELIST-TCOLOR = 0.
  IT_TREELIST-TINTENSIV = '0'.
  IT_TREELIST-TEXT1 = '(By vendor, P/O, Import request)'.
  IT_TREELIST-TLENGTH1 = STRLEN( IT_TREELIST-TEXT1 ).
  IT_TREELIST-TCOLOR1 = 0.
  IT_TREELIST-TINTENSIV1 = '0'.
  APPEND      IT_TREELIST.

ENDFORM.                    " P2000_FILL_ITAB_HEAD
*&---------------------------------------------------------------------*
*&      Form  P2000_LIFNR_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P2000_LIFNR_NODE_WRITE.

  CLEAR : IT_TREELIST, SV_NAME.

* Vendor Name SELECT
  SELECT SINGLE  NAME1  INTO  SV_NAME
  FROM   LFA1           WHERE LIFNR  EQ  IT_EKKO-LIFNR.

* LEVEL Assign.
  IT_TREELIST-ID       = 2.                  " ID
  IT_TREELIST-TLEVEL   = 2.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL1'.           " LEVEL
  IT_TREELIST-COLOR    = 4.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
  IT_TREELIST-HIDE     = 'MK03'.             " T-CODE
* Vendor CODE
  IT_TREELIST-TEXT       = IT_EKKO-LIFNR.
  IT_TREELIST-TLENGTH    = 10.
  IT_TREELIST-TCOLOR     = 4.
  IT_TREELIST-TINTENSIV  = '0'.
* Vendor Name1
  IT_TREELIST-TEXT1      = SV_NAME.
  IT_TREELIST-TLENGTH1   = 35.
  IT_TREELIST-TCOLOR1    = 2.
  IT_TREELIST-TINTENSIV1 = '0'.

  APPEND     IT_TREELIST.

ENDFORM.                    " P2000_LIFNR_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_AMEND_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P2000_AMEND_NODE_WRITE.

  CLEAR : IT_TREELIST.

* LEVEL Assign.
  IT_TREELIST-ID       = 5.                  " ID
  IT_TREELIST-TLEVEL   = 5.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL4'.           " LEVEL
  IT_TREELIST-COLOR    = 3.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
  IT_TREELIST-HIDE     = 'ZIM13'.            " T-CODE
* Import Request No
  IT_TREELIST-TEXT       = IT_ZTREQST-ZFREQNO.
  IT_TREELIST-TLENGTH    = 10.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* AMEND NO
  IT_TREELIST-TEXT1      = IT_ZTREQST-ZFAMDNO.
  IT_TREELIST-TLENGTH1   = 12.
  IT_TREELIST-TCOLOR1    = 3.
  IT_TREELIST-TINTENSIV1 = '0'.
* SPACE
  IT_TREELIST-TEXT2      = 'Amend     '.
  IT_TREELIST-TLENGTH2   = 10.
  IT_TREELIST-TCOLOR2    = 'OFF'.
  IT_TREELIST-TINTENSIV2 = '0'.
* Charge in.
  IT_TREELIST-TEXT3      = IT_ZTREQST-ZFOPNNM.
  IT_TREELIST-TLENGTH3   = 20.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* LC No.
  IT_TREELIST-TEXT4      = IT_ZTREQST-ZFOPNNO.
  IT_TREELIST-TLENGTH4   = 35.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* Currency.
  IT_TREELIST-TEXT5      = IT_ZTREQST-WAERS.
  IT_TREELIST-TLENGTH5   = 5.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.
* Open Amount.
  WRITE : IT_ZTREQST-ZFOPAMT CURRENCY IT_ZTREQST-WAERS
                                    TO IT_TREELIST-TEXT6(19).
  IT_TREELIST-TLENGTH6    = 19.
  IT_TREELIST-TCOLOR6     = 2.
  IT_TREELIST-TINTENSIV6  = '0'.

  APPEND     IT_TREELIST.

*>> AMEND Insuarance
  LOOP  AT  IT_ZTINS  WHERE  ZFREQNO    =   IT_ZTREQST-ZFREQNO
                      AND    ZFAMDNO    =   IT_ZTREQST-ZFAMDNO.
    PERFORM   P2000_INS_NODE_WRITE.
  ENDLOOP.

ENDFORM.                    " P2000_AMEND_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P2000_BL_NODE_WRITE.

*>> BL  NO.  SET
  CLEAR : SV_BLNO, LFA1, IT_TREELIST.
  MOVE   IT_BL-ZFHBLNO   TO   SV_BLNO.
  IF  IT_BL-ZFHBLNO  IS  INITIAL.
    MOVE   IT_BL-ZFMBLNO   TO   SV_BLNO.
  ELSEIF  IT_BL-ZFMBLNO  IS  INITIAL.
    MOVE   IT_BL-ZFCGHNO   TO   SV_BLNO.
  ENDIF.

  SELECT SINGLE * FROM USR01
  WHERE  BNAME    EQ   SY-UNAME.

* LEVEL
  IT_TREELIST-ID       = 7.                  " ID
  IT_TREELIST-TLEVEL   = 7.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL6'.           " LEVEL
  IT_TREELIST-COLOR    = 3.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
  IT_TREELIST-HIDE     = 'ZIM23'.            " T-CODE
* BL NO
  IT_TREELIST-TEXT       = IT_BL-ZFBLNO.
  IT_TREELIST-TLENGTH    = 10.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* SPACE
  IT_TREELIST-TEXT1      = 'BL  '.
  IT_TREELIST-TLENGTH1   = 4 + W_SPACE.
  IT_TREELIST-TCOLOR1    = 'OFF'.
  IT_TREELIST-TINTENSIV1 = '0'.
* ETD.
  WRITE  IT_BL-ZFETD  TO   IT_TREELIST-TEXT2.
  IT_TREELIST-TLENGTH2   = 10.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* ETA.
  WRITE  IT_BL-ZFETA  TO   IT_TREELIST-TEXT3.
  IT_TREELIST-TLENGTH3   = 20.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* HOUSE BL NO OR MASTER BL NO OR Forwarder BL NO
  IT_TREELIST-TEXT4      = SV_TEXT.
  IT_TREELIST-TLENGTH4   = 35.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* Currency.
  IT_TREELIST-TEXT5      = IT_BL-ZFBLAMC.
  IT_TREELIST-TLENGTH5   = 5.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.
* BL Amount.
  WRITE : IT_BL-ZFBLAMT CURRENCY IT_BL-ZFBLAMC
                                 TO IT_TREELIST-TEXT6(19).
  IT_TREELIST-TLENGTH6    = 19.
  IT_TREELIST-TCOLOR6     = 2.
  IT_TREELIST-TINTENSIV6  = '0'.

  APPEND     IT_TREELIST.

*>> LG Data SELECT.
  LOOP  AT  IT_ZTLG  WHERE  ZFBLNO  EQ  IT_BL-ZFBLNO.
    PERFORM   P2000_LG_NODE_WRITE.
  ENDLOOP.

*>> Cargo Information SELECT
  CLEAR  W_NODE_CNT.
  LOOP  AT  IT_CG  WHERE   ZFBLNO  EQ  IT_BL-ZFBLNO.
    W_NODE_CNT = W_NODE_CNT + 1.
    PERFORM   P2000_CG_NODE_DATA.
  ENDLOOP.

*>> Bonded-in, Bonded-out Data SELECT
  LOOP  AT  IT_INR  WHERE  ZFBLNO  EQ  IT_BL-ZFBLNO.
    W_NODE_CNT = W_NODE_CNT + 1.
    PERFORM   P2000_INR_NODE_DATA.
  ENDLOOP.

  IF  W_NODE_CNT GT 0.
    W_SPACE1   =  W_SPACE1  -  4.
  ENDIF.

*>> Customs Clerance Request SELECT
  LOOP  AT  IT_IV  WHERE   ZFBLNO  EQ   IT_BL-ZFBLNO.
    W_IV_CNT  =  W_IV_CNT + 1.
    PERFORM   P2000_IV_NODE_WRITE.
  ENDLOOP.

*>> Taxable Clerance
  IF W_IV_CNT LT 1.

    LOOP  AT  IT_ZTCUIV  WHERE   ZFBLNO  EQ   IT_BL-ZFBLNO.
      PERFORM   P2000_CUIV_NODE_WRITE.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " P2000_BL_NODE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_NODE_SELECT
*&---------------------------------------------------------------------*
FORM P2000_NODE_SELECT TABLES   KNOTEN   STRUCTURE     SEUCOMM
                       USING    EXIT.
  CASE SY-TCODE.
    WHEN 'ZIM93'.     " Hierarchy
      IF KNOTEN-HIDE IS INITIAL.
        MESSAGE E962.
      ELSE.
        CASE KNOTEN-NAME.
          WHEN 'LEVEL1'.     " Vendor
            SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
            SET PARAMETER ID 'LIF' FIELD KNOTEN-TEXT.
            SET PARAMETER ID 'EKO' FIELD ''.
          WHEN 'LEVEL2'.     " P/O
            SET PARAMETER ID 'BES' FIELD KNOTEN-TEXT.
          WHEN 'LEVEL3'.     " L/C
            SET PARAMETER ID 'ZPREQNO' FIELD KNOTEN-TEXT.
            SET PARAMETER ID 'ZPOPNNO' FIELD ''.
            SET PARAMETER ID 'BES'     FIELD ''.
          WHEN 'LEVEL4'.     " Amend & Insurance.
            IF KNOTEN-HIDE EQ 'ZIM47'.
              SET PARAMETER ID 'ZPREQNO' FIELD KNOTEN-TEXT.
              SET PARAMETER ID 'ZPAMDNO' FIELD KNOTEN-TEXT1(5).
              SET PARAMETER ID 'ZPINSEQ' FIELD KNOTEN-TEXT1+6(5).
              SET PARAMETER ID 'BES'     FIELD SPACE.
              SET PARAMETER ID 'ZPOPNNO' FIELD SPACE.
            ELSEIF KNOTEN-HIDE EQ 'ZIM43'.
              SET PARAMETER ID 'ZPREQNO' FIELD KNOTEN-TEXT.
              SET PARAMETER ID 'ZPINSEQ' FIELD KNOTEN-TEXT1+6(5).
              SET PARAMETER ID 'BES'     FIELD SPACE.
              SET PARAMETER ID 'ZPOPNNO' FIELD SPACE.
            ELSE.
              SET PARAMETER ID 'ZPREQNO' FIELD KNOTEN-TEXT.
              SET PARAMETER ID 'ZPAMDNO' FIELD KNOTEN-TEXT1.
              SET PARAMETER ID 'BES'     FIELD SPACE.
              SET PARAMETER ID 'ZPOPNNO' FIELD SPACE.
            ENDIF.
          WHEN 'LEVEL5'.     " COMMERCIAL IV
            SET PARAMETER ID 'ZPCIVRN' FIELD KNOTEN-TEXT.
            SET PARAMETER ID 'ZPCIVNO' FIELD SPACE.
          WHEN 'LEVEL6'.     " BL & LG
            IF KNOTEN-HIDE(5)  EQ 'ZIM23'.
              SET PARAMETER ID 'ZPBLNO'  FIELD KNOTEN-TEXT.
              SET PARAMETER ID 'ZPHBLNO' FIELD SPACE.
            ELSE.
              SET PARAMETER ID 'ZPBLNO'  FIELD KNOTEN-HIDE+5(10).
              SET PARAMETER ID 'ZPLGSEQ' FIELD KNOTEN-TEXT.
              SET PARAMETER ID 'ZPHBLNO' FIELD SPACE.
            ENDIF.
          WHEN 'LEVEL7'.     " Cargo, Carry-in, Carry-out
            IF KNOTEN-HIDE(5)  EQ 'ZIM83'.
              SET PARAMETER ID 'ZPHBLNO' FIELD SPACE.
              SET PARAMETER ID 'ZPBLNO'  FIELD KNOTEN-HIDE+5(10).
              SET PARAMETER ID 'ZPCGPT'  FIELD KNOTEN-TEXT1.
            ELSEIF KNOTEN-HIDE(5) EQ 'ZIMI8'.
              SET PARAMETER ID 'ZPBLNO'  FIELD KNOTEN-TEXT(10).
              SET PARAMETER ID 'ZPBTSEQ' FIELD KNOTEN-TEXT+11(5).
              SET PARAMETER ID 'ZPHBLNO' FIELD SPACE.
            ELSE.
              SET PARAMETER ID 'ZPBLNO'  FIELD KNOTEN-TEXT(10).
              SET PARAMETER ID 'ZPBTSEQ' FIELD KNOTEN-TEXT+11(5).
              SET PARAMETER ID 'ZPHBLNO' FIELD SPACE.
            ENDIF.
          WHEN 'LEVEL8'.
            IF KNOTEN-HIDE(5) EQ 'ZIM33'.
              SET PARAMETER ID 'ZPBLNO'  FIELD SPACE.
              SET PARAMETER ID 'ZPHBLNO' FIELD SPACE.
              SET PARAMETER ID 'ZPIVNO'  FIELD KNOTEN-TEXT.
            ELSE.
              SET PARAMETER ID 'ZPIVNO'  FIELD KNOTEN-TEXT.
            ENDIF.
          WHEN 'LEVEL9'.
            SET PARAMETER ID 'ZPIVNO'     FIELD KNOTEN-TEXT(10).
            SET PARAMETER ID 'ZPCLSEQ'    FIELD KNOTEN-TEXT+11(5).
          WHEN 'LEVEL10'.
            SET PARAMETER ID 'ZPENTNO'    FIELD SPACE.
            SET PARAMETER ID 'ZPIVNO'     FIELD KNOTEN-TEXT(10).
            SET PARAMETER ID 'ZPCLSEQ'    FIELD KNOTEN-TEXT+11(5).
          WHEN 'LEVEL11'.
            SET PARAMETER ID 'BUK'        FIELD KNOTEN-HIDE+4(4).
            SET PARAMETER ID 'MBN'        FIELD KNOTEN-TEXT.
            SET PARAMETER ID 'MJA'        FIELD KNOTEN-HIDE+8(4).
            W_MBLNR  =  KNOTEN-TEXT.
            W_MJAHR  =  KNOTEN-HIDE+8(4).

*>> G/R Document FUNCTION CALL.
            CALL FUNCTION 'MIGO_DIALOG'
               EXPORTING
                  I_ACTION                  = 'A04'
                  I_REFDOC                  = 'R02'
                  I_NOTREE                  = 'X'
                  I_SKIP_FIRST_SCREEN       = 'X'
                  I_OKCODE                  = 'OK_GO'
                  I_MBLNR                   = W_MBLNR
                  I_MJAHR                   = W_MJAHR
               EXCEPTIONS
                  ILLEGAL_COMBINATION       = 1.
          WHEN OTHERS.
            MESSAGE E962.
        ENDCASE.
        IF KNOTEN-NAME NE 'LEVEL11' AND KNOTEN-NAME NE 'LEVEL10' AND
           KNOTEN-NAME NE 'LEVEL9'.
           CALL TRANSACTION KNOTEN-HIDE(5) AND SKIP  FIRST SCREEN.
        ELSEIF KNOTEN-NAME EQ 'LEVEL10' OR KNOTEN-NAME EQ 'LEVEL9'.
           CALL TRANSACTION KNOTEN-HIDE(6) AND SKIP  FIRST SCREEN.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
      CASE KNOTEN-NAME.
        WHEN 'EXEC' OR 'LEVEL11'.
          IF KNOTEN-HIDE IS INITIAL.
            MESSAGE E962.
          ELSEIF KNOTEN-NAME NE 'LEVEL11'.
            CALL TRANSACTION KNOTEN-HIDE.
          ENDIF.
        WHEN OTHERS.
          MESSAGE E962.
      ENDCASE.
  ENDCASE.
  EXIT = ' '.
ENDFORM.                    "
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

  SELECT  A~ZFREQNO         B~EBELN     MAX( A~ZFREQTY )  AS  ZFREQTY
          MAX( A~LIFNR )    AS LIFNR    MAX( A~ZFMATGB )  AS  ZFMATGB
          MAX( A~ZFOPBN )   AS  ZFOPBN
          MAX( A~INCO1 )    AS INCO1    MAX( A~ZFLASTSD ) AS ZFLASTSD
          MAX( A~ZFOPAMT )  AS ZFOPAMT  MAX( A~ZFLASTAM ) AS ZFLASTAM
          MAX( A~ZFLASTED ) AS ZFLASTED MAX( A~WAERS )    AS WAERS
  INTO    CORRESPONDING FIELDS OF TABLE  IT_ZTREQHD
  FROM    ZTREQHD  AS  A  INNER  JOIN  ZTREQIT  AS  B
  ON      A~ZFREQNO    EQ   B~ZFREQNO
  WHERE   A~ZFREQNO    IN   S_REQNO
  AND     B~EBELN      IN   S_EBELN
  AND     A~LIFNR      IN   S_LIFNR
  AND     A~ZFMATGB    IN   S_MATGB
  AND     A~ZFWERKS    IN   S_WERKS
  AND     A~ZFOPNNO    IN   S_OPNNO
  GROUP BY
          A~ZFREQNO  B~EBELN.

  LOOP  AT  IT_ZTREQHD.

    W_TABIX  =  SY-TABIX.

    SELECT  ZFREQNO  INTO CORRESPONDING FIELDS OF TABLE IT_ST
    FROM    ZTREQST
    WHERE   ZFREQNO    EQ   IT_ZTREQHD-ZFREQNO
    AND     ZFREQTY    IN   S_REQTY
    AND     ZFDOCST    IN   S_DOCST
    AND     CDAT       IN   S_REQDT
    AND     ZFREQNO    IN   S_REQNO
    AND     ZFOPNNM    LIKE P_NAME
    GROUP BY
            ZFREQNO.

    IF SY-SUBRC NE 0.
      DELETE  IT_ZTREQHD  INDEX  W_TABIX.
    ENDIF.
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
*  -->  p1        text
*  <--  p2        text
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
*  -->  p1        text
*  <--  p2        text
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
*&      Form  P1000_READ_IV_DATA
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

ENDFORM.                    " P1000_READ_IV_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIDR_DATA.

  REFRESH : IT_ZTIDR, IT_IDR.

  SELECT   *
  INTO     CORRESPONDING FIELDS OF TABLE IT_IDR
  FROM     ZTIDRUS
  FOR      ALL ENTRIES   IN  IT_IV
  WHERE    ZFIVNO        EQ  IT_IV-ZFIVNO.

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

  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTIDS
  FROM    ZTIDSUS
  FOR     ALL   ENTRIES  IN  IT_ZTIDR
  WHERE   ZFIVNO         EQ  IT_ZTIDR-ZFIVNO
  AND     ZFCLSEQ        EQ  IT_ZTIDR-ZFCLSEQ.

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
  SORT  IT_CG        BY  ZFBLNO   ZFCGNO.
  SORT  IT_ZTLG      BY  ZFBLNO   ZFLGSEQ.
  SORT  IT_INR       BY  ZFBLNO   ZFBTSEQ.
  SORT  IT_OUR       BY  ZFBLNO   ZFBTSEQ.
  SORT  IT_IV        BY  ZFREQNO  ZFIVNO.
  SORT  IT_ZTCUIV    BY  ZFBLNO   ZFIVNO.
  SORT  IT_ZTIDR     BY  ZFIVNO   ZFCLSEQ.
  SORT  IT_ZTIDS     BY  ZFIVNO   ZFCLSEQ.
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

*>> HEADER TREE SET
  PERFORM  P2000_FILL_ITAB_HEAD.

  CLEAR : W_LINE_CNT, W_MOD.
  REFRESH : IT_ZTIVHST.

  LOOP  AT  IT_EKKO.

    IF  SY-TABIX  EQ  1.
      MOVE  IT_EKKO-LIFNR   TO   SV_LIFNR.
      PERFORM  P2000_LIFNR_NODE_WRITE.
    ENDIF.

    IF  IT_EKKO-LIFNR  NE  SV_LIFNR.
      PERFORM  P2000_LIFNR_NODE_WRITE.
      MOVE   IT_EKKO-LIFNR  TO  SV_LIFNR.
    ENDIF.

*>> PO DATA WRITE.
    CLEAR : T024E, T024.
    SELECT SINGLE * FROM T024E WHERE EKORG  EQ  IT_EKKO-EKORG.
    SELECT SINGLE * FROM T024  WHERE EKGRP  EQ  IT_EKKO-EKGRP.

    PERFORM   P2000_PO_NODE_WRITE.

  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ALL_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_PO_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_PO_NODE_WRITE.

  CLEAR : IT_TREELIST.

* LEVEL Assign.
  IT_TREELIST-ID       = 3.                  " ID
  IT_TREELIST-TLEVEL   = 3.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL2'.           " LEVEL
  IT_TREELIST-COLOR    = 7.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
  IT_TREELIST-HIDE     = 'ME23N'.             " T-CODE
* PO NO.
  IT_TREELIST-TEXT       = IT_EKKO-EBELN.
  IT_TREELIST-TLENGTH    = 10.
  IT_TREELIST-TCOLOR     = 7.
  IT_TREELIST-TINTENSIV  = '0'.
* Purchase ORG.
  IT_TREELIST-TEXT1      = IT_EKKO-EKORG.
  IT_TREELIST-TLENGTH1   = 5.
  IT_TREELIST-TCOLOR1    = 2.
  IT_TREELIST-TINTENSIV1 = '0'.
* Purchase ORG Name.
  IT_TREELIST-TEXT2      = T024E-EKOTX.
  IT_TREELIST-TLENGTH2   = 20.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* Purchase GROUP.
  IT_TREELIST-TEXT3      = IT_EKKO-EKGRP.
  IT_TREELIST-TLENGTH3   = 4.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* Purchase GROUP Name
  IT_TREELIST-TEXT4      = T024-EKNAM.
  IT_TREELIST-TLENGTH4   = 20.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.

  APPEND     IT_TREELIST.

*>> Import Request DATA WRITE.
  LOOP  AT  IT_ZTREQHD  WHERE  EBELN  =  IT_EKKO-EBELN.
    CLEAR : W_SPACE, W_SPACE1.
    PERFORM   P2000_REQ_NODE_WRITE.
  ENDLOOP.

ENDFORM.                    " P2000_PO_NODE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_REQ_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_REQ_NODE_WRITE.

  CLEAR : IT_TREELIST, LFA1.
*>> Bank Name SELECT
  SELECT   SINGLE  *  FROM  LFA1  WHERE  LIFNR  EQ  IT_ZTREQHD-ZFOPBN.

* LEVEL Assign.
  IT_TREELIST-ID       = 4.                  " ID
  IT_TREELIST-TLEVEL   = 4.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL3'.           " LEVEL
  IT_TREELIST-COLOR    = 3.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
  IT_TREELIST-HIDE     = 'ZIM03'.             " T-CODE
* Import Request Document No
  IT_TREELIST-TEXT       = IT_ZTREQHD-ZFREQNO.
  IT_TREELIST-TLENGTH    = 10.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* Import Payment TYPE
  IT_TREELIST-TEXT1      = IT_ZTREQHD-ZFREQTY.
  IT_TREELIST-TLENGTH1   = 5.
  IT_TREELIST-TCOLOR1    = 2.
  IT_TREELIST-TINTENSIV1 = '0'.
* INCOTERMS.
  IT_TREELIST-TEXT2      = IT_ZTREQHD-INCO1.
  IT_TREELIST-TLENGTH2   = 10.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* OPEN BANK.
  IT_TREELIST-TEXT3      = IT_ZTREQHD-ZFOPBN.
  IT_TREELIST-TLENGTH3   = 10.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* Bank Name
  IT_TREELIST-TEXT4      = LFA1-NAME1.
  IT_TREELIST-TLENGTH4   = 20.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* LC No.
  IT_TREELIST-TEXT5      = IT_ZTREQHD-ZFOPNNO.
  IT_TREELIST-TLENGTH5   = 35.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.
* Currency.
  IT_TREELIST-TEXT6      = IT_ZTREQHD-WAERS.
  IT_TREELIST-TLENGTH6   = 5.
  IT_TREELIST-TCOLOR6    = 2.
  IT_TREELIST-TINTENSIV6 = '0'.
* Open Amount
  WRITE : IT_ZTREQHD-ZFLASTAM CURRENCY IT_ZTREQHD-WAERS
                                    TO IT_TREELIST-TEXT7(19).
  IT_TREELIST-TLENGTH7    = 19.
  IT_TREELIST-TCOLOR7     = 2.
  IT_TREELIST-TINTENSIV7  = '0'.

  APPEND     IT_TREELIST.

*>> Insurance WRITE.
  CLEAR  W_NODE_CNT.
  LOOP  AT  IT_ZTINS    WHERE  ZFREQNO  =  IT_ZTREQHD-ZFREQNO
                        AND    ZFAMDNO  =  '00000'.
    W_NODE_CNT = W_NODE_CNT + 1.
    PERFORM   P2000_INS_NODE_WRITE.
  ENDLOOP.

*>> AMEND WRITE
  LOOP  AT  IT_ZTREQST  WHERE  ZFREQNO  =  IT_ZTREQHD-ZFREQNO.
    W_NODE_CNT = W_NODE_CNT + 1.
    PERFORM  P2000_AMEND_NODE_WRITE.
  ENDLOOP.

  IF W_NODE_CNT EQ  0.
    W_SPACE  =  4.
  ENDIF.

*>> COMMERCIAL INVOICE WRITE
  CLEAR W_NODE_CNT.
  LOOP  AT  IT_CIV     WHERE  ZFREQNO  =  IT_ZTREQHD-ZFREQNO.
    W_NODE_CNT = W_NODE_CNT + 1.
    PERFORM   P2000_CIV_NODE_WRITE.
  ENDLOOP.

  IF W_NODE_CNT EQ 0.
    W_SPACE = W_SPACE + 4.
  ENDIF.
  IF IT_ZTREQHD-ZFREQTY  EQ  'LO'  OR  IT_ZTREQHD-ZFREQTY  EQ  'PU'.
    LOOP  AT  IT_IV  WHERE   ZFREQNO  EQ   IT_ZTREQHD-ZFREQNO.
      W_SPACE1  =  4.
      PERFORM   P2000_IV_NODE_WRITE.
    ENDLOOP.
  ENDIF.

*>> BL DATA WRITE.
  LOOP  AT  IT_BL     WHERE   ZFREQNO  =  IT_ZTREQHD-ZFREQNO.
    CLEAR  W_SPACE1.
    PERFORM   P2000_BL_NODE_WRITE.
  ENDLOOP.

ENDFORM.                    " P2000_REQ_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_INS_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INS_NODE_WRITE.

  CLEAR : IT_TREELIST, SV_TEXT.

  CASE  IT_ZTINS-ZFTRANS.
    WHEN  'A'.
      MOVE   'Air'          TO  SV_TEXT.
    WHEN  'O'.
      MOVE   'Ocean'        TO  SV_TEXT.
    WHEN  'B'.
      MOVE   'Air + Ocean'  TO  SV_TEXT.
    WHEN OTHERS.
      MOVE   'None(Other)'   TO  SV_TEXT.
  ENDCASE.

* LEVEL Assign.
  IT_TREELIST-ID       = 5.                  " ID
  IT_TREELIST-TLEVEL   = 5.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL4'.           " LEVEL
  IT_TREELIST-COLOR    = 3.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
* TRANSACTION CODE SET
  IF IT_ZTINS-ZFAMDNO  GT  '00000'.
    IT_TREELIST-HIDE  = 'ZIM47'.
  ELSE.
    IT_TREELIST-HIDE  = 'ZIM43'.
  ENDIF.
* Import Request No.
  IT_TREELIST-TEXT       = IT_ZTINS-ZFREQNO.
  IT_TREELIST-TLENGTH    = 10.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* AMEND NO + Insurance Sequence.
  CONCATENATE  IT_ZTINS-ZFAMDNO  '-'  IT_ZTINS-ZFINSEQ
                                 INTO IT_TREELIST-TEXT1.
  IT_TREELIST-TLENGTH1   = 12.
  IT_TREELIST-TCOLOR1    = 3.
  IT_TREELIST-TINTENSIV1 = '0'.
* SPACE.
  IT_TREELIST-TEXT2      = 'Insurance policy  '.
  IT_TREELIST-TLENGTH2   = 10.
  IT_TREELIST-TCOLOR2    = 'OFF'.
  IT_TREELIST-TINTENSIV2 = '0'.
* Transportation Method.
  IT_TREELIST-TEXT3      = SV_TEXT.
  IT_TREELIST-TLENGTH3   = 20.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* Insurance Policy No.
  IT_TREELIST-TEXT4      = IT_ZTINS-ZFINNO.
  IT_TREELIST-TLENGTH4   = 35.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* Currency.
  IT_TREELIST-TEXT5      = IT_ZTINS-ZFINAMTC.
  IT_TREELIST-TLENGTH5   = 5.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.
* Premium.
  WRITE : IT_ZTINS-ZFINAMT CURRENCY IT_ZTINS-ZFINAMTC
                                    TO IT_TREELIST-TEXT6(19).
  IT_TREELIST-TLENGTH6    = 19.
  IT_TREELIST-TCOLOR6     = 2.
  IT_TREELIST-TINTENSIV6  = '0'.

  APPEND     IT_TREELIST.

ENDFORM.                    " P2000_INS_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_CIV_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_CIV_NODE_WRITE.

*>> Vendor Name SELECT
  CLEAR : LFA1, IT_TREELIST.
  SELECT  SINGLE  *  FROM  LFA1  WHERE  LIFNR  EQ  IT_CIV-ZFMAVN.

* LEVEL Assign.
  IT_TREELIST-ID       = 6.                  " ID
  IT_TREELIST-TLEVEL   = 6.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL5'.           " LEVEL
  IT_TREELIST-COLOR    = 3.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
  IT_TREELIST-HIDE     = 'ZIM37'.            " T-CODE
* CIV NO.
  IT_TREELIST-TEXT      = IT_CIV-ZFCIVRN.
  IT_TREELIST-TLENGTH   = 10 + W_SPACE.
  IT_TREELIST-TCOLOR    = 3.
  IT_TREELIST-TINTENSIV = '0'.
* SPACE.
  IT_TREELIST-TEXT1      = 'CIV     '.
  IT_TREELIST-TLENGTH1   = 8.
  IT_TREELIST-TCOLOR1    = 'OFF'.
  IT_TREELIST-TINTENSIV1 = '0'.
* Vendor.
  IT_TREELIST-TEXT2      = IT_CIV-ZFMAVN.
  IT_TREELIST-TLENGTH2   = 10.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* Vendor Name.
  IT_TREELIST-TEXT3      = LFA1-NAME1.
  IT_TREELIST-TLENGTH3   = 20.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* SPACE
  IT_TREELIST-TEXT4      = IT_CIV-ZFCIVNO.
  IT_TREELIST-TLENGTH4   = 35.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* Currency
  IT_TREELIST-TEXT5      = IT_CIV-ZFIVAMC.
  IT_TREELIST-TLENGTH5   = 5.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.
* Invoice Amount
  WRITE : IT_CIV-ZFIVAMT CURRENCY IT_CIV-ZFIVAMC
                                  TO IT_TREELIST-TEXT6(19).
  IT_TREELIST-TLENGTH6    = 19.
  IT_TREELIST-TCOLOR6     = 2.
  IT_TREELIST-TINTENSIV6  = '0'.

  APPEND     IT_TREELIST.

ENDFORM.                    " P2000_CIV_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_LG_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_LG_NODE_WRITE.

  CLEAR : LFA1, IT_TREELIST.
  SELECT  SINGLE *  FROM  LFA1  WHERE  LIFNR  EQ  IT_ZTLG-ZFCARIR.

* LEVEL
  IT_TREELIST-ID       = 7.                  " ID
  IT_TREELIST-TLEVEL   = 7.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL6'.           " LEVEL
  IT_TREELIST-COLOR    = 3.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
  CONCATENATE  'ZIM28'  IT_ZTLG-ZFBLNO  INTO IT_TREELIST-HIDE.
* LG Sequence.
  IT_TREELIST-TEXT       = IT_ZTLG-ZFLGSEQ.
  IT_TREELIST-TLENGTH    = 10.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* SPACE
  IT_TREELIST-TEXT1      = 'LG  '.
  IT_TREELIST-TLENGTH1   = 4 + W_SPACE.
  IT_TREELIST-TCOLOR1    = 'OFF'.
  IT_TREELIST-TINTENSIV1 = '0'.
* Forwarder.
  IT_TREELIST-TEXT2      = IT_ZTLG-ZFCARIR.
  IT_TREELIST-TLENGTH2   = 10.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* Forwarder Name.
  IT_TREELIST-TEXT3      = LFA1-NAME1.
  IT_TREELIST-TLENGTH3   = 20.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* SPACE
  IT_TREELIST-TEXT4      = IT_ZTLG-ZFHBLNO.
  IT_TREELIST-TLENGTH4   = 35.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* Currency.
  IT_TREELIST-TEXT5      = IT_ZTLG-ZFCIAMC.
  IT_TREELIST-TLENGTH5   = 5.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.
* LG Amount
  WRITE : IT_ZTLG-ZFCIAM  CURRENCY IT_ZTLG-ZFCIAMC
                                   TO IT_TREELIST-TEXT6(19).
  IT_TREELIST-TLENGTH6    = 19.
  IT_TREELIST-TCOLOR6     = 2.
  IT_TREELIST-TINTENSIV6  = '0'.

  APPEND     IT_TREELIST.

ENDFORM.                    " P2000_LG_NODE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_CG_NODE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_CG_NODE_DATA.

*>> Arrival Port SELECT.
  CLEAR : SV_TEXT, IT_TREELIST.
  SELECT SINGLE ZFCDNM  INTO  SV_TEXT
  FROM   ZTIMIMG08
  WHERE  ZFCDTY  EQ  '002'
  AND    ZFCD    EQ  IT_CG-ZFCGPT.

* LEVEL
  IT_TREELIST-ID       = 8.                  " ID
  IT_TREELIST-TLEVEL   = 8.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL7'.           " LEVEL
  IT_TREELIST-COLOR    = 3.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
  CONCATENATE  'ZIM83'  IT_CG-ZFBLNO  INTO IT_TREELIST-HIDE.
* Cargo Document No
  IT_TREELIST-TEXT       = IT_CG-ZFCGNO.
  IT_TREELIST-TLENGTH    = 11.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* Arriving Port
  IT_TREELIST-TEXT1      = IT_CG-ZFCGPT.
  IT_TREELIST-TLENGTH1   = 10 + W_SPACE.
  IT_TREELIST-TCOLOR1    = 2.
  IT_TREELIST-TINTENSIV1 = '0'.
* Arriving Port Name.
  IT_TREELIST-TEXT2      = SV_TEXT.
  IT_TREELIST-TLENGTH2   = 20.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* SPACE
  IT_TREELIST-TEXT4      = '           '.
  IT_TREELIST-TLENGTH4   = 35.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* SPACE
  IT_TREELIST-TEXT5      = '     '.
  IT_TREELIST-TLENGTH5   = 5.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.
* SPACE
  IT_TREELIST-TEXT6       = '                   '.
  IT_TREELIST-TLENGTH6    = 19.
  IT_TREELIST-TCOLOR6     = 2.
  IT_TREELIST-TINTENSIV6  = '0'.

  APPEND     IT_TREELIST.

ENDFORM.                    " P2000_CG_NODE_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_CUIV_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_CUIV_NODE_WRITE.

*>> Clearance indicator GET
  CLEAR : SV_TEXT, SV_TEXT1, SV_TEXT2, IT_TREELIST.
  CASE  IT_ZTCUIV-ZFCLCD.
    WHEN  'A'.
      MOVE  'Bonded transport'  TO   SV_TEXT.
    WHEN  'C'.
      MOVE  'At port'           TO   SV_TEXT.
    WHEN  'X'.
      MOVE  'Not object'        TO   SV_TEXT.
    WHEN  'B'.
      MOVE  'Taxation'          TO   SV_TEXT.
  ENDCASE.
*>> Clearance status GET
  CASE  IT_ZTCUIV-ZFCUST.
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

* LEVEL
  IT_TREELIST-ID         = 9.                  " ID
  IT_TREELIST-TLEVEL     = 9.                  " LEVEL
  IT_TREELIST-NAME       = 'LEVEL8'.           " LEVEL
  IT_TREELIST-COLOR      = 3.                  " COLOR
  IT_TREELIST-INTENSIV   = '0'.                " INTENSIVE
  IT_TREELIST-HIDE       = 'ZIM23'.            " T-CODE
* INVOICE NO.
  IT_TREELIST-TEXT       = IT_ZTCUIV-ZFIVNO.
  IT_TREELIST-TLENGTH    = 10.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* SPACE
  IT_TREELIST-TEXT1      = 'Taxation'.
  IT_TREELIST-TLENGTH1   = 7.
  IT_TREELIST-TCOLOR1    = 'OFF'.
  IT_TREELIST-TINTENSIV1 = '0'.
* Clearance/Goods receipt request date.
  WRITE  IT_ZTCUIV-ZFCCDT  DD/MM/YYYY  TO  IT_TREELIST-TEXT2.
  IT_TREELIST-TLENGTH2   = 10.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* Clearance indicator.
  IT_TREELIST-TEXT3      = SV_TEXT.
  IT_TREELIST-TLENGTH3   = 12.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* Clearance status.
  IT_TREELIST-TEXT4      = SV_TEXT1.
  IT_TREELIST-TLENGTH4   = 20.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* Good Receipt Status.
  IT_TREELIST-TEXT5      = SV_TEXT2.
  IT_TREELIST-TLENGTH5   = 11.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.
* Currency.
  IT_TREELIST-TEXT6      = IT_ZTCUIV-ZFIVAMC.
  IT_TREELIST-TLENGTH6   = 5.
  IT_TREELIST-TCOLOR6    = 2.
  IT_TREELIST-TINTENSIV6 = '0'.
* Invoice Amount.
  WRITE   IT_ZTCUIV-ZFIVAMT  CURRENCY  IT_ZTCUIV-ZFIVAMC
                             TO        IT_TREELIST-TEXT7(19).
  IT_TREELIST-TLENGTH7   = 19.
  IT_TREELIST-TCOLOR7    = 2.
  IT_TREELIST-TINTENSIV7 = '0'.

  APPEND  IT_TREELIST.

*>> Customs Clerance WRITE.
  LOOP  AT  IT_ZTIDR  WHERE  ZFIVNO   =   IT_IV-ZFIVNO.
    PERFORM   P2000_IDR_NODE_WRITE.
  ENDLOOP.

ENDFORM.                    " P2000_CUIV_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_INR_NODE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INR_NODE_DATA.

*>> Arriving Bonded Area
  CLEAR : ZTIMIMG03, IT_TREELIST.
  SELECT  SINGLE * FROM ZTIMIMG03  WHERE  ZFBNAR  EQ  IT_INR-ZFABNAR.

* LEVEL
  IT_TREELIST-ID       = 8.                  " ID
  IT_TREELIST-TLEVEL   = 8.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL7'.           " LEVEL
  IT_TREELIST-COLOR    = 3.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
  IT_TREELIST-HIDE     = 'ZIMI8'.            " T-CODE
* BL NO.
  CONCATENATE  IT_INR-ZFBLNO  '-'  IT_INR-ZFBTSEQ
                              INTO IT_TREELIST-TEXT.
  IT_TREELIST-TLENGTH    = 16.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* SPACE
  IT_TREELIST-TEXT1      = 'Carry-in '.
  IT_TREELIST-TLENGTH1   = 5 + W_SPACE.
  IT_TREELIST-TCOLOR1    = 'OFF'.
  IT_TREELIST-TINTENSIV1 = '0'.
* Bonded Area Code.
  IT_TREELIST-TEXT2      = IT_INR-ZFABNAR.
  IT_TREELIST-TLENGTH2   = 20.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* Bonded Area Code Name
  IT_TREELIST-TEXT4      = ZTIMIMG03-ZFBNARM.
  IT_TREELIST-TLENGTH4   = 35.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* SPACE
  IT_TREELIST-TEXT5      = IT_INR-ZFINRNO.
  IT_TREELIST-TLENGTH5   = 25.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.

  APPEND     IT_TREELIST.

*>> Carry-Out GET
  LOOP  AT  IT_OUR  WHERE  ZFBLNO  EQ  IT_INR-ZFBLNO
                    AND    ZFBTSEQ EQ  IT_INR-ZFBTSEQ.
    PERFORM   P2000_OUR_NODE_WRITE.
  ENDLOOP.

ENDFORM.                    " P2000_INR_NODE_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_IV_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IV_NODE_WRITE.

*>> Clearence indicator GET
  CLEAR : SV_TEXT, SV_TEXT1, SV_TEXT2, IT_TREELIST.
  CASE  IT_IV-ZFCLCD.
    WHEN  'A'.
      MOVE  'Bonded transport'  TO   SV_TEXT.
    WHEN  'C'.
      MOVE  'FTZ Clearance'     TO   SV_TEXT.
    WHEN  'X'.
      MOVE  'Not object'        TO   SV_TEXT.
    WHEN  'B'.
      MOVE  'taxation'          TO   SV_TEXT.
  ENDCASE.
*>> Clearance Status GET
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

* LEVEL
  IT_TREELIST-ID         = 9.                  " ID
  IT_TREELIST-TLEVEL     = 9.                  " LEVEL
  IT_TREELIST-NAME       = 'LEVEL8'.           " LEVEL
  IT_TREELIST-COLOR      = 3.                  " COLOR
  IT_TREELIST-INTENSIV   = '0'.                " INTENSIVE
  IT_TREELIST-HIDE       = 'ZIM33'.            " T-CODE
* INVOICE NO.
  IT_TREELIST-TEXT       = IT_IV-ZFIVNO.
  IT_TREELIST-TLENGTH    = 10.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* SPACE
  IT_TREELIST-TEXT1      = ''.
  IT_TREELIST-TLENGTH1   = 11 + W_SPACE + W_SPACE1.
  IT_TREELIST-TCOLOR1    = 'OFF'.
  IT_TREELIST-TINTENSIV1 = '0'.
* Clearance Request Date.
  WRITE  IT_IV-ZFCCDT    TO   IT_TREELIST-TEXT2.
  IT_TREELIST-TLENGTH2   = 20.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* Clearance indicator
  IT_TREELIST-TEXT3      = SV_TEXT.
  IT_TREELIST-TLENGTH3   = 14.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* Clearance status
  IT_TREELIST-TEXT4      = SV_TEXT1.
  IT_TREELIST-TLENGTH4   = 20.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* Currency
  IT_TREELIST-TEXT5      = IT_IV-ZFIVAMC.
  IT_TREELIST-TLENGTH5   = 5.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.
* Invoice Amount
  WRITE   IT_IV-ZFIVAMT  CURRENCY  IT_IV-ZFIVAMC
                         TO        IT_TREELIST-TEXT6(19).
  IT_TREELIST-TLENGTH6   = 19.
  IT_TREELIST-TCOLOR6    = 2.
  IT_TREELIST-TINTENSIV6 = '0'.

  APPEND  IT_TREELIST.

  IF  IT_ZTREQHD-ZFREQTY  EQ  'LO'  OR  IT_ZTREQHD-ZFREQTY  EQ  'PU'.
    W_SPACE1 = W_SPACE1 + 8.
    LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IV-ZFIVNO.
      PERFORM   P2000_IN_NODE_WRITE.
    ENDLOOP.
  ENDIF.

*>> Customs Declare WRITE.
  LOOP  AT  IT_ZTIDR  WHERE  ZFIVNO   =   IT_IV-ZFIVNO.
    PERFORM   P2000_IDR_NODE_WRITE.
  ENDLOOP.

ENDFORM.                    " P2000_IV_NODE_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_OUR_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_OUR_NODE_WRITE.

*>> Bonded Area Code..
  CLEAR : ZTIMIMG03, IT_TREELIST.
  SELECT  SINGLE * FROM ZTIMIMG03  WHERE  ZFBNAR  EQ  IT_INR-ZFABNAR.

* LEVEL
  IT_TREELIST-ID       = 8.                  " ID
  IT_TREELIST-TLEVEL   = 8.                  " LEVEL
  IT_TREELIST-NAME     = 'LEVEL7'.           " LEVEL
  IT_TREELIST-COLOR    = 3.                  " COLOR
  IT_TREELIST-INTENSIV = '0'.                " INTENSIVE
  IT_TREELIST-HIDE     = 'ZIMO3'.            " T-CODE
* BL NO.
  CONCATENATE  IT_OUR-ZFBLNO  '-'   IT_OUR-ZFBTSEQ
                              INTO  IT_TREELIST-TEXT.
  IT_TREELIST-TLENGTH    = 16.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* SPACE.
  IT_TREELIST-TEXT1      = 'Carry-out '.
  IT_TREELIST-TLENGTH1   = 5 + W_SPACE.
  IT_TREELIST-TCOLOR1    = 'OFF'.
  IT_TREELIST-TINTENSIV1 = '0'.
* Bonded Area Code..
  IT_TREELIST-TEXT2      = IT_OUR-ZFABNAR.
  IT_TREELIST-TLENGTH2   = 20.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* Bonded Area Code Name.
  IT_TREELIST-TEXT4      = ZTIMIMG03-ZFBNARM.
  IT_TREELIST-TLENGTH4   = 35.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* SPACE
  IT_TREELIST-TEXT5      = IT_OUR-ZFOURNO.
  IT_TREELIST-TLENGTH5   = 25.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.

  APPEND     IT_TREELIST.

ENDFORM.                    " P2000_OUR_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_IDR_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IDR_NODE_WRITE.

  CLEAR : SV_NAME, IT_TREELIST.

*>> Customs Broker SELECT
  SELECT SINGLE A~NAME1  INTO  SV_NAME
  FROM   LFA1  AS  A  INNER  JOIN  ZTIMIMG10  AS  B
  ON     A~LIFNR      EQ     B~ZFVEN
  WHERE  B~ZFCUT      EQ     IT_ZTIDR-ZFCUT.

* LEVEL
  IT_TREELIST-ID         = 10.                  " ID
  IT_TREELIST-TLEVEL     = 10.                  " LEVEL
  IT_TREELIST-NAME       = 'LEVEL9'.           " LEVEL
  IT_TREELIST-COLOR      = 3.                  " COLOR
  IT_TREELIST-INTENSIV   = '0'.                " INTENSIVE
  IT_TREELIST-HIDE       = 'ZIMCD3'.           " T-CODE
* Invoice NO.
  CONCATENATE  IT_ZTIDR-ZFIVNO  '-'  IT_ZTIDR-ZFCLSEQ
                                INTO IT_TREELIST-TEXT.
  IT_TREELIST-TLENGTH    = 16.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* SPACE.
  IT_TREELIST-TEXT1      = ' '.
  IT_TREELIST-TLENGTH1   = 1.
  IT_TREELIST-TCOLOR1    = 'OFF'.
  IT_TREELIST-TINTENSIV1 = '0'.
* Broker
  IT_TREELIST-TEXT2      = SV_NAME.
  IT_TREELIST-TLENGTH2   = 20 + W_SPACE + W_SPACE1.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* Entry No
  IT_TREELIST-TEXT3      = IT_ZTIDR-ZFENTNO.
  IT_TREELIST-TLENGTH3   = 35.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* Currency
  IT_TREELIST-TEXT4      = IT_ZTIDR-ZFIVAMC.
  IT_TREELIST-TLENGTH4   = 5.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.
* Invoice Amount
  WRITE   IT_ZTIDR-ZFIVAMT  CURRENCY  IT_ZTIDR-ZFIVAMC
                            TO        IT_TREELIST-TEXT5(19).
  IT_TREELIST-TLENGTH5   = 19.
  IT_TREELIST-TCOLOR5    = 2.
  IT_TREELIST-TINTENSIV5 = '0'.

  APPEND  IT_TREELIST.

  LOOP  AT  IT_ZTIDS  WHERE  ZFIVNO  EQ  IT_ZTIDR-ZFIVNO
                      AND    ZFCLSEQ EQ  IT_ZTIDR-ZFCLSEQ.
    PERFORM  P2000_IDS_NODE_WRITE.
  ENDLOOP.


ENDFORM.                    " P2000_IDR_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_IDS_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IDS_NODE_WRITE.

*>> Customs Broker SELECT
  CLEAR : SV_NAME, IT_TREELIST.
  SELECT SINGLE A~NAME1  INTO  SV_NAME
  FROM   LFA1  AS  A  INNER  JOIN  ZTIMIMG10  AS  B
  ON     A~LIFNR      EQ     B~ZFVEN
  WHERE  B~ZFCUT      EQ     IT_ZTIDR-ZFCUT.
* LEVEL
  IT_TREELIST-ID         = 11.                  " ID
  IT_TREELIST-TLEVEL     = 11.                  " LEVEL
  IT_TREELIST-NAME       = 'LEVEL10'.           " LEVEL
  IT_TREELIST-COLOR      = 3.                  " COLOR
  IT_TREELIST-INTENSIV   = '0'.                " INTENSIVE
  IT_TREELIST-HIDE       = 'ZIMCC3'.           " T-CODE
* IV NO.
  CONCATENATE  IT_ZTIDS-ZFIVNO  '-'  IT_ZTIDS-ZFCLSEQ
                                INTO IT_TREELIST-TEXT.
  IT_TREELIST-TLENGTH    = 16.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
* Entry Date
  WRITE  IT_ZTIDS-ZFEDT  TO  IT_TREELIST-TEXT1.
  IT_TREELIST-TLENGTH1   = 18 + W_SPACE + W_SPACE1.
  IT_TREELIST-TCOLOR1    = 2.
  IT_TREELIST-TINTENSIV1 = '0'.
* Entry No
  IT_TREELIST-TEXT2      = IT_ZTIDS-ZFENTNO.
  IT_TREELIST-TLENGTH2   = 35.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* Currency
  IT_TREELIST-TEXT3      = IT_ZTIDS-ZFIVAMC.
  IT_TREELIST-TLENGTH3   = 5.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* Invoice Amount
  WRITE   IT_ZTIDS-ZFIVAMT  CURRENCY  IT_ZTIDS-ZFIVAMC
                            TO        IT_TREELIST-TEXT4(19).
  IT_TREELIST-TLENGTH4   = 19.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.

  APPEND  IT_TREELIST.

  LOOP  AT   IT_IN  WHERE  ZFIVNO  EQ  IT_IV-ZFIVNO.

    IF  SY-TABIX  EQ  1.
      PERFORM   P2000_IN_NODE_WRITE.
      MOVE  IT_IN-MBLNR  TO  SV_MBLNR.
    ENDIF.

    IF  SV_MBLNR  NE  IT_IN-MBLNR.
      PERFORM   P2000_IN_NODE_WRITE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " P2000_IDS_NODE_WRITE
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

  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_IN
  FROM    ZTIVHST
  FOR     ALL  ENTRIES    IN     IT_IV
  WHERE   ZFIVNO          EQ     IT_IV-ZFIVNO
  AND     ZFGRST          EQ     'Y'.

ENDFORM.                    " P1000_READ_IN_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_IN_NODE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IN_NODE_WRITE.

  CLEAR  IT_TREELIST.

* LEVEL
  IT_TREELIST-ID         = 12.                  " ID
  IT_TREELIST-TLEVEL     = 12.                  " LEVEL
  IT_TREELIST-NAME       = 'LEVEL11'.           " LEVEL
  IT_TREELIST-COLOR      = 3.                   " COLOR
  IT_TREELIST-INTENSIV   = '0'.                 " INTENSIVE
  CONCATENATE  'MBO3' IT_IN-MJAHR  IT_IN-BUKRS
                      INTO         IT_TREELIST-HIDE.
* Material Document
  IT_TREELIST-TEXT       = IT_IN-MBLNR.
  IT_TREELIST-TLENGTH    = 10.
  IT_TREELIST-TCOLOR     = 3.
  IT_TREELIST-TINTENSIV  = '0'.
*  Document Date
  WRITE  IT_IN-BLDAT  TO  IT_TREELIST-TEXT1.
  IT_TREELIST-TLENGTH1   = 20 + W_SPACE + W_SPACE1.
  IT_TREELIST-TCOLOR1    = 2.
  IT_TREELIST-TINTENSIV1 = '0'.
* Post Date.
  WRITE  IT_IN-BUDAT  TO  IT_TREELIST-TEXT2.
  IT_TREELIST-TLENGTH2   = 35.
  IT_TREELIST-TCOLOR2    = 2.
  IT_TREELIST-TINTENSIV2 = '0'.
* SPACE.
  IT_TREELIST-TEXT3      = '     '.
  IT_TREELIST-TLENGTH3   = 5.
  IT_TREELIST-TCOLOR3    = 2.
  IT_TREELIST-TINTENSIV3 = '0'.
* SPACE.
  IT_TREELIST-TEXT4      = '                   '.
  IT_TREELIST-TLENGTH4   = 19.
  IT_TREELIST-TCOLOR4    = 2.
  IT_TREELIST-TINTENSIV4 = '0'.

  APPEND  IT_TREELIST.

ENDFORM.                    " P2000_IN_NODE_WRITE
