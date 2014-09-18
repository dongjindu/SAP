*&---------------------------------------------------------------------*
*& INCLUDE ZRIM01E01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입B/L  POPUP용 Event Include                        *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.04.17                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&   Event AT USER-COMMAND
*&---------------------------------------------------------------------*
AT USER-COMMAND.
  CASE SY-UCOMM.
*------- Abbrechen (CNCL) ----------------------------------------------
    WHEN 'CANC' OR 'CNCL' OR 'CANCL'.
      ANTWORT = 'C'.
      SET SCREEN 0.    LEAVE SCREEN.
    WHEN 'UMAT'.
      ANTWORT = 'Y'.
      CLEAR: W_BLNO.
      SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
    WHEN 'SUCH'.
*      PERFORM SUCHEN.

*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
    WHEN 'SORB'.
*     SORT FELDTAB BY FTEXT.
*     INDEX = 1.
*     SET SCREEN 100.
*     LEAVE SCREEN.

*------- Sortieren nach Feldname (SORF) --------------------------------
    WHEN 'SORF'.
*     SORT FELDTAB BY FNAME.
*     INDEX = 1.
*     SET SCREEN 100.
*     LEAVE SCREEN.

*------- Techn. Name ein/aus (TECH) ------------------------------------
    WHEN 'TECH'.
*     TRANSLATE I_XTECH USING 'X  X'.
*     CLEAR: RFCU3-FNAME, RFCU1-FELDT.           " f? 'Weiter suchen'
*     INDEX = SY-STARO.
*     SET SCREEN 100.
*     LEAVE SCREEN.

*------- Weiter suchen (WESU) ------------------------------------------
    WHEN 'WESU'.
*     IF  RFCU3-FNAME IS INITIAL
*     AND RFCU1-FELDT IS INITIAL.
*       PERFORM SUCHEN.
*     ELSE.
*       OLD_INDEX = SY-STARO.
*       INDEX = SY-STARO.
*       PERFORM SUCHEN_IN_FELDTAB USING INDEX.
*       IF OLD_INDEX = INDEX.
*         MESSAGE I408.
*       ENDIF.
*       IF INDEX > 0.
*         SCROLL LIST TO PAGE 1 LINE INDEX.
*       ELSE.
*         SCROLL LIST TO PAGE 1 LINE OLD_INDEX.
*       ENDIF.
*       SET CURSOR 2 3.
*     ENDIF.
    WHEN 'DSID'.
      SET PARAMETER ID 'VLM' FIELD IT_CONTLST-VBELN.
      CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.

    WHEN 'PODSP'.
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P2000_SHOW_PO USING  IT_SELECTED-EBELN.
        ELSEIF W_SELECTED_LINES GT 1.
           MESSAGE E965.
           EXIT.
        ENDIF.
    WHEN 'BLDSP'.
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P2000_SHOW_BL USING  IT_SELECTED-ZFBLNO.
        ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE S965.
            EXIT.
        ENDIF.
*     WHEN 'PICK'.
*         PERFORM P2000_MULTI_SELECTION.
*         IF W_SELECTED_LINES EQ 1.
*             READ TABLE IT_SELECTED INDEX 1.
*             W_EBELN     = IT_SELECTED-EBELN.
*             W_BLNO      = IT_SELECTED-ZFBLNO.
*             W_HBLNO     = IT_SELECTED-ZFHBLNO.
*             ANTWORT = 'Y'.
*         ELSEIF W_SELECTED_LINES GT 1.
*             MESSAGE E965.
*         ENDIF.
  ENDCASE.

*&---------------------------------------------------------------------*
*&   Event AT LINE-SELECTION
*&---------------------------------------------------------------------*
AT LINE-SELECTION.
  CASE INCLUDE.
     WHEN 'POPU'.
         IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
*            MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
*            MESSAGE ID IT_ERR_LIST-MSGID TYPE 'I'
*                    NUMBER IT_ERR_LIST-MSGNR
*                    WITH   IT_ERR_LIST-MSGV1
*                           IT_ERR_LIST-MSGV2
*                           IT_ERR_LIST-MSGV3
*                           IT_ERR_LIST-MSGV4.
            call function 'MASS_MESSAGE_SHOW_LONGTEXT'
                 exporting
                    sprsl     = sy-langu
                    arbgb     = IT_ERR_LIST-MSGID
                    msgnr     = IT_ERR_LIST-MSGNR
                    msgv1     = IT_ERR_LIST-MSGV1
                    msgv2     = IT_ERR_LIST-MSGV2
                    msgv3     = IT_ERR_LIST-MSGV3
                    msgv4     = IT_ERR_LIST-MSGV4
*                    extravars = 'F'
                exceptions
                    not_found = 1
                    others    = 2.

         ENDIF.
         CLEAR : IT_ERR_LIST.
    WHEN 'BLCST'.
       REFRESH IT_COST_SELECTED.
       CLEAR W_SELECTED_LINES.

       DO.
          CLEAR MARKFIELD.
          READ LINE SY-INDEX FIELD VALUE MARKFIELD.
          IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
          IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
             ZSBLCST = IT_ZSBLCST_POST.
             ADD 1 TO W_SELECTED_LINES.
          ENDIF.
       ENDDO.
       CASE  W_SELECTED_LINES.
          WHEN 0.   MESSAGE S951.  EXIT.
          WHEN 1.
             REFRESH : IT_COST_SELECTED.
             LOOP AT IT_ZSBLCST_POST
                  WHERE ZFPAY     = ZSBLCST-ZFPAY
                  AND   COND_TYPE = ZSBLCST-COND_TYPE
                  AND   ZFWERKS   = ZSBLCST-ZFWERKS
                  AND   MWSKZ     = ZSBLCST-MWSKZ
                  AND   WAERS     = ZSBLCST-WAERS.
*                  AND   ZFCSCD    = ZSBLCST-ZFCSCD
                MOVE-CORRESPONDING IT_ZSBLCST_POST TO
                                   IT_COST_SELECTED.
                APPEND IT_COST_SELECTED.
             ENDLOOP.
*            IF ZSBLCST-ZFCSCD EQ 'CTT'.
*               DELETE IT_COST_SELECTED WHERE ZFCSCD NE 'CTT'.
*            ELSE.
*               DELETE IT_COST_SELECTED WHERE ZFCSCD EQ 'CTT'.
*            ENDIF.
             ANTWORT = 'Y'.
          WHEN OTHERS.  MESSAGE S965.   EXIT.
       ENDCASE.
    WHEN 'CIVPO' OR 'CIVRQ' OR 'CIVNO'.
        W_ZFCIVRN     = IT_ZSCIVHD-ZFCIVRN.
        ANTWORT = 'Y'.
    WHEN 'CCHBL' OR 'CCBL' OR 'LOGR' OR 'LOGR1'.
        W_ZFIVNO     = IT_ZSIV-ZFIVNO.
        ANTWORT = 'Y'.
    WHEN 'CGLIST' OR 'CGLIST1'.
        W_ZFCGNO     = IT_ZSCGHD-ZFCGNO.
        ANTWORT = 'Y'.
*------- 보험 CREATE ---------------------------------------------------
    WHEN 'INCREATE' OR 'INCREAT1' OR 'INDISPLY'.
        W_EBELN     = IT_ZSREQHD-EBELN.
        W_ZFREQNO   = IT_ZSREQHD-ZFREQNO.
        W_ZFOPNNO   = IT_ZSREQHD-ZFOPNNO.
        W_ZFAMDNO   = IT_ZSREQHD-ZFAMDNO.
        ANTWORT = 'Y'.
    WHEN 'COMMIV'.
        W_ZFCIVNO   = IT_ZSCIVHD-ZFCIVNO.
        W_ZFCIVRN   = IT_ZSCIVHD-ZFCIVRN.
        ANTWORT     = 'Y'.
*------- B/L  CREATE ---------------------------------------------------
    WHEN 'BLCREATE' OR 'BLCREAT1' OR 'ZTINS'.         "NCW MODIFY
        W_EBELN     = IT_ZSREQHD-EBELN.
        W_ZFREQNO   = IT_ZSREQHD-ZFREQNO.
        W_ZFOPNNO   = IT_ZSREQHD-ZFOPNNO.
        W_ZFAMDNO   = IT_ZSREQHD-ZFAMDNO.
        ANTWORT = 'Y'.
*------- L/G  CREATE ---------------------------------------------------
    WHEN 'LGCREATE'.
        W_BLNO      = IT_ZSREQHD-ZFBLNO.
        W_HBLNO     = IT_ZSREQHD-ZFHBLNO.
        ANTWORT = 'Y'.
    WHEN 'ZTLG'.
        W_BLNO      = IT_ZTLG-ZFBLNO.
        W_LGSEQ     = IT_ZTLG-ZFLGSEQ.
        ANTWORT = 'Y'.
*------ 반입예정/반입신고---------------------------------------------
    WHEN 'BLLST'.
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            W_EBELN     = IT_SELECTED-EBELN.
            W_HBLNO     = IT_SELECTED-ZFHBLNO.
            W_BLNO      = IT_SELECTED-ZFBLNO.
            W_TBLNO     = IT_SELECTED-ZFTBLNO.
            ANTWORT = 'Y'.
        ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
            EXIT.
        ENDIF.
    WHEN 'POLST'.
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            W_EBELN    = IT_SELECTED-EBELN.
            W_BLNO     = IT_SELECTED-ZFBLNO.
            W_TBLNO    = IT_SELECTED-ZFTBLNO.
            ANTWORT = 'Y'.
        ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
            EXIT.
        ENDIF.
    WHEN 'BLINOUCG'.
        ZSREQHD-ZFBTSEQ  = IT_ZSBLINOU-ZFBTSEQ.
        ANTWORT = 'Y'.
    WHEN 'ZTBLINRD'.
        ZSREQHD-ZFBTSEQ  = IT_ZTBLINR-ZFBTSEQ.
        ANTWORT = 'Y'.
    WHEN 'ZTBLOURD'.
        ZSREQHD-ZFBTSEQ  = IT_ZTBLOUR-ZFBTSEQ.
        ANTWORT = 'Y'.
    WHEN 'MSNMFIND'.
        W_NAME  = IT_MSNM-ZFMSNM.
        ANTWORT = 'Y'.
*    WHEN 'POPU'.
*       IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
*          MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
*                  NUMBER IT_ERR_LIST-MSGNR
*                  WITH   IT_ERR_LIST-MSGV1
*                         IT_ERR_LIST-MSGV2
*                         IT_ERR_LIST-MSGV3
*                         IT_ERR_LIST-MSGV4.
*       ENDIF.
*       CLEAR : IT_ERR_LIST.
  ENDCASE.

  SET SCREEN 0.    LEAVE SCREEN.

*&---------------------------------------------------------------------*
*&   Event TOP-OF-PAGE
*&---------------------------------------------------------------------*
TOP-OF-PAGE.
  CASE INCLUDE.
    WHEN 'CIVPO' OR 'CIVRQ' OR 'CIVNO'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      IF INCLUDE EQ 'CIVPO'.
         WRITE : / 'Purchase Order :', ZSCIVHD-EBELN.
      ELSEIF INCLUDE EQ 'CIVRQ'.
         WRITE : / 'Import Request :', ZSCIVHD-ZFREQNO.
      ELSEIF INCLUDE EQ 'CIVNO'.
         WRITE : / 'Import License No:', ZSCIVHD-ZFOPNNO.
      ENDIF.

      WRITE : / SY-ULINE(70).
      WRITE : / SY-VLINE,
                'Goods price No',   SY-VLINE,
                'COCD',         SY-VLINE NO-GAP,
                'Type' NO-GAP,  SY-VLINE,
                'Goods price (Foreign currency)',  SY-VLINE,
                'Goods price (KRW)',  SY-VLINE.

      WRITE : / SY-ULINE(70).

    WHEN 'LOGR' OR 'LOGR1'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      IF INCLUDE EQ 'LOGR'.
         WRITE : / 'Purchase Order :', ZSREQHD-EBELN.
      ELSEIF INCLUDE EQ 'LOGR1'.
         WRITE : / 'Import Request No :', ZSREQHD-ZFREQNO.
      ENDIF.

      WRITE : / SY-ULINE(73).
      WRITE : / SY-VLINE,
                ' G/R No ',   SY-VLINE,
                'G/R request Date',   SY-VLINE,
                'Vendor',   SY-VLINE NO-GAP,
                'Type' NO-GAP,  SY-VLINE,
                ' Creator ID  ',  SY-VLINE,
                ' G/R status ',   SY-VLINE.
      WRITE : / SY-ULINE(73).

    WHEN 'CCHBL' OR 'CCBL'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      IF INCLUDE EQ 'CCHBL'.
         WRITE : / 'B/L Number :', IT_ZSIV-ZFHBLNO.
      ELSE.
         WRITE : / 'B/L No :', IT_ZSIV-ZFBLNO.
      ENDIF.
      WRITE : / SY-ULINE(73).
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / SY-VLINE,  'Clearance ',             SY-VLINE,
                           'Req. Date ',             SY-VLINE,
                           '  Type  ',               SY-VLINE,
                           'Clearance Status ',      SY-VLINE,
                           'G/R Stat.',              SY-VLINE.
      FORMAT RESET.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE(73).

    WHEN 'CGLIST'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.

      SELECT SINGLE PORTT INTO ZSIMIMG08-ZFCDNM
                    FROM  ZTIEPORT
                    WHERE LAND1 EQ 'KR'
                    AND   PORT  EQ ZSCGHD-ZFCGPT.

      WRITE : / 'B/L Number :', IT_ZSCGHD-ZFHBLNO,
              / 'Unloading port :', ZSCGHD-ZFCGPT, ZSIMIMG08-ZFCDNM(50).
      WRITE : / SY-ULINE(73).
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / SY-VLINE,  'Unloading No',           SY-VLINE,
                '         Mother ship Name       ',  SY-VLINE,
                ' Arrival date ',                    SY-VLINE,
                ' Arrival port date ',               SY-VLINE.

      FORMAT RESET.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE(73).

    WHEN 'CGLIST1'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'B/L Number :', IT_ZSCGHD-ZFHBLNO,
              / SY-ULINE(79).
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / SY-VLINE,  'Unloading No',         SY-VLINE,
                '          Mother ship Name    ',  SY-VLINE,
                'Port' NO-GAP,                     SY-VLINE,
                ' Arrival date ',                  SY-VLINE,
                ' Arrival port date ',             SY-VLINE.

      FORMAT RESET.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE(79).

    WHEN 'ZTLG'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'B/L Number : ', ZTBL-ZFHBLNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'SeqNo'                 NO-GAP,  SY-VLINE NO-GAP,
                'Shipping Company '     NO-GAP,  SY-VLINE NO-GAP,
                '  Sender  '            NO-GAP,  SY-VLINE NO-GAP,
                'CIV Amount'            NO-GAP,  SY-VLINE NO-GAP,
                'Cur. '                 NO-GAP,  SY-VLINE NO-GAP,
                'Application date'      NO-GAP,  SY-VLINE NO-GAP,
                'Issuing Bank'          NO-GAP,  SY-VLINE NO-GAP,
                'Shp'                   NO-GAP,  SY-VLINE NO-GAP,
                'Arr'.

      FORMAT RESET.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE.

*------- COMMERCIAL INVOICE NO DUPLICATION )----------------------------
    WHEN 'COMMIV'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE.
      WRITE : / 'Commercial Invoice No :', ZSCIVHD-ZFCIVNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'Management No'         NO-GAP,  SY-VLINE NO-GAP,
                'Goods price Cusomer'   NO-GAP,  SY-VLINE NO-GAP,
                'Classfication'         NO-GAP,  SY-VLINE NO-GAP,
                'Rate'                  NO-GAP,  SY-VLINE NO-GAP,
                'CIV Amount'            NO-GAP,  SY-VLINE NO-GAP,
                'Cur. '                 NO-GAP,  SY-VLINE NO-GAP,
                'Creation date'         NO-GAP,  SY-VLINE NO-GAP,
                'V'                     NO-GAP,  SY-VLINE NO-GAP.

      FORMAT RESET.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE.

    WHEN 'LGCREATE'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'B/L Number : ', ZSREQHD-ZFHBLNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'B/L Doc.No'            NO-GAP,  SY-VLINE NO-GAP,
                'Cur. '                 NO-GAP,  SY-VLINE NO-GAP,
                '      Amount       '   NO-GAP,  SY-VLINE NO-GAP,
                'Shp'                   NO-GAP,  SY-VLINE NO-GAP,
                'Shipping por'          NO-GAP,  SY-VLINE NO-GAP,
                'Trp'                   NO-GAP,  SY-VLINE NO-GAP,
                'Arrival port'          NO-GAP,  SY-VLINE NO-GAP,
                'In charge'.
      FORMAT RESET.
      WRITE : / SY-ULINE.

*------- B/L  CREATE( P/O No. Duplicate )----------------------------
    WHEN 'BLCREATE'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Purch. Number : ', ZSREQHD-EBELN.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'Import Request No' NO-GAP, SY-VLINE NO-GAP,
                'Ty' NO-GAP,         SY-VLINE NO-GAP,
                '  Vendor  ' NO-GAP, SY-VLINE NO-GAP,
                '    Vendor desc.    ' NO-GAP, SY-VLINE NO-GAP,
                'Document(Approval) No'.
      FORMAT RESET.
      WRITE : / SY-ULINE.

    WHEN 'BLCREAT1'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Document(Approval) No : ', ZSREQHD-ZFOPNNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'Import Request No' NO-GAP, SY-VLINE NO-GAP,
                ' Purch.No ' NO-GAP, SY-VLINE NO-GAP,
                'Ty' NO-GAP,         SY-VLINE NO-GAP,
                '  Vendor  ' NO-GAP, SY-VLINE NO-GAP,
                '        Vendor  desc.         '.
      FORMAT RESET.
      WRITE : / SY-ULINE.

    WHEN 'ZTINS'.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'InsNo'          NO-GAP, SY-VLINE NO-GAP,
                'Ins Vendor'     NO-GAP, SY-VLINE NO-GAP,
           (19) 'Invoice Amount' NO-GAP, SY-VLINE NO-GAP,
                'Curre'          NO-GAP, SY-VLINE NO-GAP,
                'Effect.Dat'     NO-GAP, SY-VLINE NO-GAP,
           (28) '              ' NO-GAP, SY-VLINE.
      FORMAT RESET.
      WRITE : / SY-ULINE.

    WHEN 'BLINOUCG'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'B/L Doc. No. : ', ZTBL-ZFBLNO.
      WRITE : / 'B/L Number   : ', ZTBL-ZFHBLNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / ' Seq.' NO-GAP, SY-VLINE NO-GAP,
                'List on cargo management No' NO-GAP, SY-VLINE NO-GAP,
                'Forwarding' NO-GAP,         SY-VLINE NO-GAP,
                'Destination' NO-GAP, SY-VLINE NO-GAP,
                'Bonded trans Declaration No' NO-GAP, SY-VLINE NO-GAP,
                'Transportation Due date'.
      FORMAT RESET.
      WRITE : / SY-ULINE.

    WHEN 'ZTBLINRD'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'B/L Doc. No. : ', ZTBL-ZFBLNO.
      WRITE : / 'B/L Number   : ', ZTBL-ZFHBLNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / ' Seq.' NO-GAP, SY-VLINE NO-GAP,
                'Carry-in declaration No' NO-GAP, SY-VLINE NO-GAP,
                'Destination' NO-GAP, SY-VLINE NO-GAP,
                'Ty'         NO-GAP, SY-VLINE NO-GAP,
                ' Carry-in date' NO-GAP, SY-VLINE NO-GAP,
                ' Person in charge'.
      FORMAT RESET.
      WRITE : / SY-ULINE.

    WHEN 'ZTBLOURD'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'B/L Doc. No. : ', ZTBL-ZFBLNO.
      WRITE : / 'B/L Number   : ', ZTBL-ZFHBLNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / ' Seq.' NO-GAP, SY-VLINE NO-GAP,
                'Carry-out declaration No' NO-GAP, SY-VLINE NO-GAP,
                'Destination' NO-GAP, SY-VLINE NO-GAP,
                'Ty'         NO-GAP, SY-VLINE NO-GAP,
                'Carry-out date' NO-GAP, SY-VLINE NO-GAP,
                'Creator '.
      FORMAT RESET.
      WRITE : / SY-ULINE.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_DUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DUP_MESSAGE.

   PERFORM P2000_MESSAGE_BOX USING 'Confirmation'
                              'Already exist inputed data.'
                              'Do you want to continue?'
                              'Y'
                              '1'.

ENDFORM.                    " P2000_DUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBL_TITLELIST
*&---------------------------------------------------------------------*
FORM P3000_ZTBL_TITLELIST.

   FORMAT COLOR COL_HEADING INTENSIFIED ON.
   WRITE : / SY-ULINE(101).
   WRITE : / SY-VLINE, ' ',
             SY-VLINE, (10) 'P/O No',
             SY-VLINE, (04) 'Seq',
             SY-VLINE, (24) 'House B/L No',
             SY-VLINE, (15) 'VENDOR',
             SY-VLINE, (10) 'ETA',
             SY-VLINE, (15) 'B/L Status',
             SY-VLINE.
   WRITE : / SY-ULINE(101).

ENDFORM.                    " P3000_ZTBL_TITLELIST
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTBL_DUP_LIST_1
*&---------------------------------------------------------------------*
FORM P2000_ZTBL_DUP_LIST_1.

  DATA: W_DOM_TEX1  LIKE DD07T-DDTEXT.
  FORMAT RESET.
  IF W_MOD EQ 0.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  CLEAR LFA1.
  SELECT SINGLE *
         FROM LFA1
         WHERE LIFNR = IT_ZSREQHD-LLIEF.

 PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDBLST' IT_ZSREQHD-ZFBLST
                               CHANGING   W_DOM_TEX1.
  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
          SY-VLINE, (10) IT_ZSREQHD-EBELN,
          SY-VLINE, (04) IT_ZSREQHD-ZFSHNO,
          SY-VLINE, (24) IT_ZSREQHD-ZFHBLNO,
          SY-VLINE, (15) LFA1-NAME1,
          SY-VLINE, (10) IT_ZSREQHD-ZFETA,
          SY-VLINE, (15) W_DOM_TEX1,
          SY-VLINE.
  HIDE: IT_ZSREQHD.
ENDFORM.
