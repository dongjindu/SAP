*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM09E01                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 보세창고 출고관련 SUB MODULE Include                  *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.17                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
AT USER-COMMAND.
  CASE SY-UCOMM.
*------- Abbrechen (CNCL) ----------------------------------------------
    WHEN 'CNCL'.
      ANTWORT = 'C'.
      SET SCREEN 0.    LEAVE SCREEN.
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
AT LINE-SELECTION.
    CASE INCLUDE.
       WHEN 'CCHBL' OR 'CCBL'.
          W_ZFIVNO     = IT_ZSIV-ZFIVNO.
          ANTWORT = 'Y'.
          CLEAR : IT_ZSIV.
       WHEN OTHERS.
    ENDCASE.
    SET SCREEN 0.    LEAVE SCREEN.
