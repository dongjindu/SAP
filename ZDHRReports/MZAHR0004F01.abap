*----------------------------------------------------------------------*
*   INCLUDE MZAHR0004F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  APPEND_INTAB
*&---------------------------------------------------------------------*
FORM APPEND_INTAB.
  CLEAR HRP1001.
  SELECT SINGLE RSIGN INTO HRP1001-RSIGN
    FROM HRP1001 WHERE OTYPE = 'O'
                   AND OBJID = W_ORGEH
                   AND PLVAR = W_PLVAR
                   AND ENDDA = '99991231'.
  IF SY-SUBRC <> 0.
    MESSAGE E007 WITH W_ORGEH.
  ENDIF.
*
  CLEAR IT_H1001.
  READ TABLE IT_H1001 WITH KEY OBJID = W_ORGEH
                               ZMARK = SPACE.
  IF SY-SUBRC = 0.
    MESSAGE W001 WITH 'Input Org. Unit already exists'.
  ELSE.
    IT_H1001-OBJID = W_ORGEH.
    IT_H1001-SHORT = W_SHORT.
    IT_H1001-STEXT = W_STEXT.
    IT_H1001-ZTYPE = W_ZTYPE.
    IT_H1001-ZMARK = SPACE.
    IT_H1001-ERDAT = SY-DATUM.
    IT_H1001-ERZET = SY-UZEIT.
    IT_H1001-ERNAM = SY-UNAME.
    APPEND IT_H1001. CLEAR IT_H1001.

*   REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
    DESCRIBE TABLE IT_H1001 LINES TC9000-LINES.
  ENDIF.
*
  SET SCREEN 0. LEAVE SCREEN.
ENDFORM.                    " APPEND_INTAB
*&---------------------------------------------------------------------*
*&      Form  SELECT_CBO_DATA
*&---------------------------------------------------------------------*
FORM SELECT_CBO_DATA.
  CLEAR ZTHR_1001.
  SELECT OBJID SHORT STEXT ZTYPE ZMARK
         ERDAT ERZET ERNAM AEDAT AEZET AENAM
    INTO (ZTHR_1001-OBJID, ZTHR_1001-SHORT, ZTHR_1001-STEXT,
          ZTHR_1001-ZTYPE, ZTHR_1001-ZMARK, ZTHR_1001-ERDAT,
          ZTHR_1001-ERZET, ZTHR_1001-ERNAM, ZTHR_1001-AEDAT,
          ZTHR_1001-AEZET, ZTHR_1001-AENAM)
    FROM ZTHR_1001 WHERE OBJID > 1.
    IT_H1001-OBJID = ZTHR_1001-OBJID.
    IT_H1001-SHORT = ZTHR_1001-SHORT.
    IT_H1001-STEXT = ZTHR_1001-STEXT.
    IT_H1001-ZTYPE = ZTHR_1001-ZTYPE.
    IT_H1001-ZMARK = ZTHR_1001-ZMARK.
    IT_H1001-ERDAT = ZTHR_1001-ERDAT.
    IT_H1001-ERZET = ZTHR_1001-ERZET.
    IT_H1001-ERNAM = ZTHR_1001-ERNAM.
    IT_H1001-AEDAT = ZTHR_1001-AEDAT.
    IT_H1001-AEZET = ZTHR_1001-AEZET.
    IT_H1001-AENAM = ZTHR_1001-AENAM.
    APPEND IT_H1001. CLEAR IT_H1001.
  ENDSELECT.
ENDFORM.                    " SELECT_CBO_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA.
  IF NOT IT_D1001[] IS INITIAL.
    DELETE ZTHR_1001 FROM TABLE IT_D1001.
  ENDIF.
*
  MODIFY ZTHR_1001 FROM TABLE IT_H1001.
  IF SY-SUBRC = 0.
    MESSAGE S001 WITH 'DATA SAVED'.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_LINE
*&---------------------------------------------------------------------*
FORM DELETE_LINE.
  LOOP AT IT_H1001 WHERE CHKBX = 'X'.
    MOVE-CORRESPONDING IT_H1001 TO IT_D1001.
    APPEND IT_D1001. CLEAR IT_D1001.
    DELETE IT_H1001. CLEAR IT_H1001.
  ENDLOOP.
*
  DESCRIBE TABLE IT_H1001 LINES TC9000-LINES.
ENDFORM.                    " DELETE_LINE
