*&---------------------------------------------------------------------*
*& INCLUDE ZRIM08E01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 기납증 POPUP용 Event Include                          *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.25                                            *
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
    WHEN 'TAXBKPO' OR 'TAXBKLC' OR 'TAXBKRQ'.
        W_EBELN     = IT_ZSTAXBKHD-EBELN.
        W_ZFREQNO   = IT_ZSTAXBKHD-ZFREQNO.
        W_ZFOPNNO   = IT_ZSTAXBKHD-BASISNO.
        W_ZFTBNO    = IT_ZSTAXBKHD-ZFTBNO.
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
    WHEN 'TAXBKPO' OR 'TAXBKLC' OR 'TAXBKRQ'.
*      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
*      WRITE : / 'Purch. Number : ', ZSREQHD-EBELN.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / ' 구매오더 ' NO-GAP,    SY-VLINE NO-GAP,
                '수입의뢰No' NO-GAP,    SY-VLINE NO-GAP,
           (22) '근거서류No' NO-GAP,    SY-VLINE NO-GAP,
                ' 양도일자 ' NO-GAP,    SY-VLINE NO-GAP,
                ' 기납증No ' NO-GAP,    SY-VLINE NO-GAP,
                '  접수번호  ' NO-GAP,    SY-VLINE.
      FORMAT RESET.
      WRITE : / SY-ULINE.

*------- L/C  CHANGE( P/O No. 중복시 )----------------------------------
    WHEN 'LCCHANGE'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'Purch. Number : ', ZSREQHD-EBELN.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / '수입의뢰No' NO-GAP,    SY-VLINE NO-GAP,
                'Amend' NO-GAP,         SY-VLINE NO-GAP,
                'Ty' NO-GAP,            SY-VLINE NO-GAP,
                'POrg' NO-GAP,          SY-VLINE NO-GAP,
                'PGp' NO-GAP,           SY-VLINE NO-GAP,
                'Cur. ' NO-GAP,         SY-VLINE NO-GAP,
                '      Amount      ' NO-GAP,  SY-VLINE NO-GAP,
                ' 요개설일 ' NO-GAP,    SY-VLINE NO-GAP,
                ' 접수일자 ' NO-GAP,    SY-VLINE NO-GAP,
                'D' NO-GAP,             SY-VLINE NO-GAP,
                '1' NO-GAP,             SY-VLINE NO-GAP,
                '2' NO-GAP,             SY-VLINE NO-GAP,
                'C' NO-GAP.
      FORMAT RESET.
      WRITE : / SY-ULINE.
*------- L/C DISPLAY( L/C No. 중복시 )----------------------------------
    WHEN 'LCDISPLY'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / '문서(승인) 번호 : ', ZSREQHD-ZFOPNNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / ' Purch.No ' NO-GAP,    SY-VLINE NO-GAP,
                '수입의뢰No' NO-GAP,    SY-VLINE NO-GAP,
                'Amend' NO-GAP,         SY-VLINE NO-GAP,
                'Ty' NO-GAP,            SY-VLINE NO-GAP,
                'POrg' NO-GAP,          SY-VLINE NO-GAP,
                'PGp' NO-GAP,           SY-VLINE NO-GAP,
*               '  Vendor  ' NO-GAP, SY-VLINE NO-GAP,
*               '    Vendor desc.    ' NO-GAP, SY-VLINE NO-GAP,
                'Cur. ' NO-GAP,         SY-VLINE NO-GAP,
                '      Amount      ' NO-GAP,  SY-VLINE NO-GAP,
                ' 요개설일 ' NO-GAP,    SY-VLINE NO-GAP,
                ' 접수일자 ' NO-GAP,    SY-VLINE NO-GAP,
                'D' NO-GAP,             SY-VLINE NO-GAP,
                '1' NO-GAP,             SY-VLINE NO-GAP,
                '2' NO-GAP,             SY-VLINE NO-GAP,
                'C' NO-GAP.
      FORMAT RESET.
      WRITE : / SY-ULINE.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
   CASE INCLUDE.
      WHEN 'POPU'.
         IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
*            MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
            MESSAGE ID IT_ERR_LIST-MSGID TYPE 'I'
                    NUMBER IT_ERR_LIST-MSGNR
                    WITH   IT_ERR_LIST-MSGV1
                           IT_ERR_LIST-MSGV2
                           IT_ERR_LIST-MSGV3
                           IT_ERR_LIST-MSGV4.
         ENDIF.
         CLEAR : IT_ERR_LIST.
     WHEN OTHERS.
  ENDCASE.
