*&---------------------------------------------------------------------*
*& INCLUDE ZRIM00E01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 POPUP용 Event Include                        *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.04.11                                            *
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
    WHEN 'CNCL'.
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
  ENDCASE.
*&---------------------------------------------------------------------*
*&   Event AT LINE-SELECTION
*&---------------------------------------------------------------------*
AT LINE-SELECTION.
  CASE INCLUDE.
*------- 보험 CREATE ---------------------------------------------------
    WHEN 'INCREATE' OR 'INCREAT1' OR 'INDISPLY' OR 'INDISPL1' OR
         'INDISPL2' OR 'INDISPL3'.
        W_EBELN     = IT_ZSREQHD-EBELN.
        W_ZFREQNO   = IT_ZSREQHD-ZFREQNO.
        W_ZFOPNNO   = IT_ZSREQHD-ZFOPNNO.
        W_ZFAMDNO   = IT_ZSREQHD-ZFAMDNO.
        W_ZFINSEQ   = IT_ZSREQHD-ZFINSEQ.

        ANTWORT = 'Y'.
*------- L/C  CREATE ---------------------------------------------------
    WHEN 'LCCHANGE' OR 'LCDISPLY' OR 'LCDISPL1'.
        W_EBELN     = IT_ZSREQHD-EBELN.
        W_ZFREQNO   = IT_ZSREQHD-ZFREQNO.
        W_ZFOPNNO   = IT_ZSREQHD-ZFOPNNO.
        W_ZFAMDNO   = IT_ZSREQHD-ZFAMDNO.
        ANTWORT = 'Y'.
    WHEN 'POPU'.
       IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
          MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
                  NUMBER IT_ERR_LIST-MSGNR
                  WITH   IT_ERR_LIST-MSGV1
                         IT_ERR_LIST-MSGV2
                         IT_ERR_LIST-MSGV3
                         IT_ERR_LIST-MSGV4.
       ENDIF.
       CLEAR : IT_ERR_LIST.
  ENDCASE.

  SET SCREEN 0.    LEAVE SCREEN.

*&---------------------------------------------------------------------*
*&   Event TOP-OF-PAGE
*&---------------------------------------------------------------------*
TOP-OF-PAGE.

  CASE INCLUDE.
*------- L/C  CHANGE( P/O No. 중복시 )----------------------------------
    WHEN 'LCCHANGE'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Purch. Number : ', ZSREQHD-EBELN.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'Imp.req.No' NO-GAP,    SY-VLINE NO-GAP,
                'Amend' NO-GAP,         SY-VLINE NO-GAP,
                'Ty' NO-GAP,            SY-VLINE NO-GAP,
                'POrg' NO-GAP,          SY-VLINE NO-GAP,
                'PGp' NO-GAP,           SY-VLINE NO-GAP,
                'Cur. ' NO-GAP,         SY-VLINE NO-GAP,
                '      Amount      ' NO-GAP,  SY-VLINE NO-GAP,
                'ExpOpenDT ' NO-GAP,    SY-VLINE NO-GAP,
                'ReceiptDT ' NO-GAP,    SY-VLINE NO-GAP,
                'D' NO-GAP,             SY-VLINE NO-GAP,
                '1' NO-GAP,             SY-VLINE NO-GAP,
                '2' NO-GAP,             SY-VLINE NO-GAP,
                'C' NO-GAP.
      FORMAT RESET.
      WRITE : / SY-ULINE.
*------- L/C DISPLAY( L/C No. 중복시 )----------------------------------
    WHEN 'LCDISPLY'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Doc.Approval No : ', ZSREQHD-ZFOPNNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / ' Purch.No ' NO-GAP,    SY-VLINE NO-GAP,
                'Imp.req.No' NO-GAP,    SY-VLINE NO-GAP,
                'Amend' NO-GAP,         SY-VLINE NO-GAP,
                'Ty' NO-GAP,            SY-VLINE NO-GAP,
                'POrg' NO-GAP,          SY-VLINE NO-GAP,
                'PGp' NO-GAP,           SY-VLINE NO-GAP,
*               '  Vendor  ' NO-GAP, SY-VLINE NO-GAP,
*               '    Vendor desc.    ' NO-GAP, SY-VLINE NO-GAP,
                'Cur. ' NO-GAP,         SY-VLINE NO-GAP,
                '      Amount      ' NO-GAP,  SY-VLINE NO-GAP,
                'ExpOpenDT ' NO-GAP,    SY-VLINE NO-GAP,
                'ReceiptDT ' NO-GAP,    SY-VLINE NO-GAP,
                'D' NO-GAP,             SY-VLINE NO-GAP,
                '1' NO-GAP,             SY-VLINE NO-GAP,
                '2' NO-GAP,             SY-VLINE NO-GAP,
                'C' NO-GAP.
      FORMAT RESET.
      WRITE : / SY-ULINE.
*------- L/C DISPLAY( 관리 No. 중복시 )---------------------------------
    WHEN 'LCDISPL1'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Imp.req.docNo: ', ZSREQHD-ZFREQNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'Amend' NO-GAP,         SY-VLINE NO-GAP,
                'Ty' NO-GAP,            SY-VLINE NO-GAP,
                'POrg' NO-GAP,          SY-VLINE NO-GAP,
                'PGp' NO-GAP,           SY-VLINE NO-GAP,
                'Cur. ' NO-GAP,         SY-VLINE NO-GAP,
                '      Amount      ' NO-GAP,  SY-VLINE NO-GAP,
                'ExpOpenDT ' NO-GAP,    SY-VLINE NO-GAP,
                'ReceiptDT ' NO-GAP,    SY-VLINE NO-GAP,
                'D' NO-GAP,             SY-VLINE NO-GAP,
                '1' NO-GAP,             SY-VLINE NO-GAP,
                '2' NO-GAP,             SY-VLINE NO-GAP,
                'C' NO-GAP.
      FORMAT RESET.
      WRITE : / SY-ULINE.
*------- 보험 CREATE( P/O No. 중복시 )----------------------------------
    WHEN 'INCREATE'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Purch. Number : ', ZSREQHD-EBELN.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'Imp.req.No' NO-GAP, SY-VLINE NO-GAP,
                'Ty' NO-GAP,         SY-VLINE NO-GAP,
                '  Vendor  ' NO-GAP, SY-VLINE NO-GAP,
                '    Vendor desc.    ' NO-GAP, SY-VLINE NO-GAP,
                '  document  approval  No.          '.
      FORMAT RESET.
      WRITE : / SY-ULINE.
*------- 보험 CREATE( L/C No. 중복시 )----------------------------------
    WHEN 'INCREAT1'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Doc.Approval.No.: ', ZSREQHD-ZFOPNNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'Imp.req.No' NO-GAP, SY-VLINE NO-GAP,
                ' Purch.No ' NO-GAP, SY-VLINE NO-GAP,
                'Ty' NO-GAP,         SY-VLINE NO-GAP,
                '  Vendor  ' NO-GAP, SY-VLINE NO-GAP,
                '        Vendor  desc.         '.
      FORMAT RESET.
      WRITE : / SY-ULINE.
*------- 보험 display( P/O No. 중복시 )---------------------------------
    WHEN 'INDISPLY'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Doc.Approval.No.: ', ZSREQHD-ZFOPNNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'Imp.req.No' NO-GAP, SY-VLINE NO-GAP,
                ' Seq.'      NO-GAP, SY-VLINE NO-GAP,
                'Amend'      NO-GAP, SY-VLINE NO-GAP,
                'Ty'         NO-GAP, SY-VLINE NO-GAP,
                ' Purch.No ' NO-GAP, SY-VLINE NO-GAP,
                'Cur. '      NO-GAP, SY-VLINE NO-GAP,
                '       Amount      ' NO-GAP,  SY-VLINE NO-GAP,
                'D' NO-GAP.
      FORMAT RESET.
      WRITE : / SY-ULINE.
    WHEN 'INDISPL1'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Purch. Number : ', ZSREQHD-EBELN.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'Imp.req.No' NO-GAP, SY-VLINE NO-GAP,
                ' Seq.'      NO-GAP, SY-VLINE NO-GAP,
                'Amend'      NO-GAP, SY-VLINE NO-GAP,
                'Ty'         NO-GAP, SY-VLINE NO-GAP,
                'Cur. '      NO-GAP, SY-VLINE NO-GAP,
                '       Amount      ' NO-GAP,  SY-VLINE NO-GAP,
                'D' NO-GAP,                   SY-VLINE NO-GAP,
                '     Document Approval No.         '.
      FORMAT RESET.
      WRITE : / SY-ULINE.
    WHEN 'INDISPL2'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Import Document : ', ZSREQHD-ZFREQNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / ' Seq.'      NO-GAP, SY-VLINE NO-GAP,
                'Amend'      NO-GAP, SY-VLINE NO-GAP,
                'Ty'         NO-GAP, SY-VLINE NO-GAP,
                ' Purch.No ' NO-GAP, SY-VLINE NO-GAP,
                'Cur. '      NO-GAP, SY-VLINE NO-GAP,
                '       Amount      ' NO-GAP,  SY-VLINE NO-GAP,
                'D' NO-GAP,                   SY-VLINE NO-GAP,
                '      Document Approval No.        '.
      FORMAT RESET.
      WRITE : / SY-ULINE.
    WHEN 'INDISPL3'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Import Document : ', ZSREQHD-ZFREQNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / ' Seq.'      NO-GAP, SY-VLINE NO-GAP,
                'Amend'      NO-GAP, SY-VLINE NO-GAP,
                'Ty'         NO-GAP, SY-VLINE NO-GAP,
                ' Purch.No ' NO-GAP, SY-VLINE NO-GAP,
                'Cur. '      NO-GAP, SY-VLINE NO-GAP,
                '       Amount      ' NO-GAP,  SY-VLINE NO-GAP,
                'D' NO-GAP,                   SY-VLINE NO-GAP,
                '       Document Approval No.       '.
      FORMAT RESET.
      WRITE : / SY-ULINE.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSIMCOST_BUKRS  text
*      -->P_IT_ZSIMCOST_GJAHR  text
*      -->P_IT_ZSIMCOST_BELNR  text
*----------------------------------------------------------------------*
FORM P2000_ALV_PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'STANDA02' EXCLUDING EXTAB.

ENDFORM.                    " P2000_ALV_PF_STATUS
