*----------------------------------------------------------------------*
*   INCLUDE FIWT010                                                    *
*----------------------------------------------------------------------*

FORM CHECK_COMPANY_CODES.

DATA:  BEGIN OF BUK_TAB OCCURS 10,
        BUKRS LIKE BSIK-BUKRS,
       END OF BUK_TAB.

  CLEAR: OLD_WITH, NEW_WITH, BUK_TAB[].

  DESCRIBE TABLE KD_BUKRS LINES SY-TFILL.

  IF SY-TFILL NE 0.
    LOOP AT KD_BUKRS.
      IF NOT KD_BUKRS-LOW IS INITIAL.
        BUK_TAB-BUKRS = KD_BUKRS-LOW.
        COLLECT BUK_TAB.
      ENDIF.
      IF NOT KD_BUKRS-HIGH IS INITIAL.
        BUK_TAB-BUKRS = KD_BUKRS-HIGH.
        COLLECT BUK_TAB.
      ENDIF.
    ENDLOOP.
  ELSE.                                            "all company codes
    SELECT BUKRS FROM T001 INTO TABLE BUK_TAB.
  ENDIF.

  LOOP AT BUK_TAB.
    CALL FUNCTION 'FI_CHECK_EXTENDED_WT'
         EXPORTING
              I_BUKRS              = BUK_TAB-BUKRS
         EXCEPTIONS
              COMPONENT_NOT_ACTIVE = 1
              NOT_FOUND            = 2
              OTHERS               = 3.

    IF OLD_WITH = 'X' AND SY-SUBRC = 0.
      MESSAGE E454.
    ENDIF.
    IF NEW_WITH = 'X' AND SY-SUBRC NE 0.
      MESSAGE E454.
    ENDIF.
    IF SY-SUBRC = 0.
      NEW_WITH = 'X'.
    ELSE.
      OLD_WITH = 'X'.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_WT_TYPES
*&---------------------------------------------------------------------*
*       Checks WT tax types
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_WT_TYPES.

  IF WITHT NE SPACE AND NEW_WITH = 'X'.
    SELECT * FROM T059P WHERE WITHT IN WITHT.
    ENDSELECT.
    IF SY-SUBRC NE '0'.
      MESSAGE E456.
    ENDIF.
  ELSEIF WITHT NE SPACE.
    MESSAGE E455.
  ENDIF.

ENDFORM.                    " CHECK_WT_TYPES
*&---------------------------------------------------------------------*
*&      Form  CHECK_WT_CODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*Note 497879 begin
FORM CHECK_WT_CODES.
*  if cls_wtcd ne space.               " Old WT functionality
*    select * from t059q where qsskz in cls_wtcd.
*    endselect.
*    if sy-subrc ne '0'.
*      set cursor field 'classical_wtcode'.
*      message e151(fr).
*    endif.
*  endif.
*
*  if ext_wtcd ne space.                 " New WT functionality
*    if witht ne space.
*      select * from t059z  where witht     in witht and
*                                 wt_withcd in ext_wtcd.
*      endselect.
*    else.
*      select * from t059z  where wt_withcd in ext_wtcd.
*      endselect.
*    endif.
*  endif.
***
  IF WITHT NE SPACE.
    SELECT * FROM T059Z
      WHERE WITHT     IN WITHT
        AND WT_WITHCD IN QSTKZ.
    ENDSELECT.
  ELSE.
    SELECT * FROM T059Z
      WHERE WT_WITHCD IN QSTKZ.
    ENDSELECT.
  ENDIF.
  IF SY-SUBRC NE '0'.
    MESSAGE E151.
  ENDIF.
* begin of insertion: note 178383
  QSTKZ_EXT   = QSTKZ.
  QSTKZ_EXT[] = QSTKZ[].
* end of insertion: note 178383
ENDFORM.                    " CHECK_WT_CODES

*&---------------------------------------------------------------------*
*&      Form  GET_POSTING_TIME
*&---------------------------------------------------------------------*
*       Determines whether WT type is type with posting at invoice or
*       at payment time.
*----------------------------------------------------------------------*
FORM GET_POSTING_TIME
     USING VALUE(I_LAND1)
           VALUE(I_WITHT)
    CHANGING
           I_WT_POSTM.

  CLEAR I_WT_POSTM.
  SELECT SINGLE * FROM T059P
         WHERE LAND1 = I_LAND1
         AND   WITHT = I_WITHT.
  IF SY-SUBRC = 0.
    I_WT_POSTM = T059P-WT_POSTM.

  ENDIF.

ENDFORM.                   " GET_POSTING_TIME
