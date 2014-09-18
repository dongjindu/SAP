*----------------------------------------------------------------------*
*   INCLUDE MZAHR0001I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE BACK_EXIT INPUT.
  IF SY-DYNNR = '9000'.
    IF IT_WTMNG[] <> TMP_WTMNG[].
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
           EXPORTING
                DEFAULTOPTION = 'Y'
                TEXTLINE1     = 'Data was changed.'
                TEXTLINE2     = 'Save changes first?'
                TITEL         = 'Exit maintenance'
           IMPORTING
                ANSWER        = W_ANSWER
           EXCEPTIONS
                OTHERS        = 1.
      IF W_ANSWER = 'J'.
        PERFORM SAVE_DATA.
      ENDIF.
    ENDIF.
    SET SCREEN 0. LEAVE SCREEN.
  ELSE.
    SET SCREEN 0. LEAVE SCREEN.
  ENDIF.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LGTXT  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LGTXT INPUT.
  CLEAR FIELD_LISTS. REFRESH FIELD_LISTS.
  FIELD_LISTS-TABNAME    = 'T512T'.
  FIELD_LISTS-FIELDNAME  = 'LGART'.
  FIELD_LISTS-SELECTFLAG = 'X'.
  APPEND FIELD_LISTS. CLEAR FIELD_LISTS.
*
  FIELD_LISTS-TABNAME    = 'T512T'.
  FIELD_LISTS-FIELDNAME  = 'LGTXT'.
  FIELD_LISTS-SELECTFLAG = ' '.
  APPEND FIELD_LISTS. CLEAR FIELD_LISTS.
*
  CLEAR VALUETAB. REFRESH VALUETAB.
  SELECT DISTINCT A~LGART A~LGTXT INTO (T512T-LGART, T512T-LGTXT)
    FROM T512T AS A JOIN T512W AS B
      ON A~LGART = B~LGART
   WHERE A~SPRSL = SY-LANGU
     AND A~MOLGA = '10'
     AND B~MOLGA = '10'
     AND B~ENDDA = '99991231'.
    VALUETAB-VALUE = T512T-LGART. APPEND VALUETAB.
    VALUETAB-VALUE = T512T-LGTXT. APPEND VALUETAB.
  ENDSELECT.
*
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            CUCOL                         = 1
            CUROW                         = 10
            FIELDNAME                     = 'LGART'
            TABNAME                       = 'T512T'
            TITLE_IN_VALUES_LIST          = 'Wage Type info'
            SHOW_ALL_VALUES_AT_FIRST_TIME = 'X'
            TITEL                         = 'Wage Type'
            NO_SCROLL                     = ' '
       IMPORTING
            SELECT_VALUE                  = W_LGART
       TABLES
            FIELDS                        = FIELD_LISTS
            SELECT_VALUES                 = SELECT_VALUE
            VALUETAB                      = VALUETAB
       EXCEPTIONS
            FIELD_NOT_IN_DDIC             = 1
            MORE_THEN_ONE_SELECTFIELD     = 2
            NO_SELECTFIELD                = 3
            OTHERS                        = 4.
*
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
  READ TABLE SELECT_VALUE INDEX 2.
  W_LGTXT = SELECT_VALUE-VALUE.
  DYNPFIELDS-FIELDNAME = 'W_LGTXT'.
  DYNPFIELDS-FIELDVALUE = W_LGTXT.
  APPEND DYNPFIELDS. CLEAR DYNPFIELDS.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME               = SY-CPROG
            DYNUMB               = SY-DYNNR
       TABLES
            DYNPFIELDS           = DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA = 1
            INVALID_DYNPROFIELD  = 2
            INVALID_DYNPRONAME   = 3
            INVALID_DYNPRONUMMER = 4
            INVALID_REQUEST      = 5
            NO_FIELDDESCRIPTION  = 6
            UNDEFIND_ERROR       = 7
            OTHERS               = 8.
ENDMODULE.                 " GET_LGTXT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE SY-UCOMM.
    WHEN 'ZMOD'.
      CASE W_MODES.
        WHEN 'D'. W_MODES = 'M'.
        WHEN 'M'. W_MODES = 'D'.
      ENDCASE.
    WHEN 'DELE'.
      LOOP AT IT_WTMNG WHERE CHKBX = 'X'.
        MOVE-CORRESPONDING IT_WTMNG TO DEL_WTMNG.
        APPEND DEL_WTMNG. CLEAR DEL_WTMNG.
        DELETE IT_WTMNG. CLEAR IT_WTMNG.
      ENDLOOP.
*     REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
      DESCRIBE TABLE IT_WTMNG LINES TC9000-LINES.
    WHEN 'NEWE'.
      W_MODES = 'M'.
      CLEAR: W_ZFORM, W_LGART, W_LGTXT, W_ZCOLN, W_ZPRNT.
      CALL SCREEN 9100 STARTING AT 5 5
                       ENDING AT 48 11.
    WHEN 'SAVE'.
      PERFORM SAVE_DATA.
  ENDCASE.
*
  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_INTAB  INPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_INTAB INPUT.
  CLEAR IT_WTMNG.
  READ TABLE IT_WTMNG WITH KEY ZFORM = W_ZFORM
                               LGART = W_LGART
                               ZMARK = SPACE.
  IF SY-SUBRC = 0.
    MESSAGE E001 WITH 'Wage Type already exists with the same key'.
  ENDIF.
*
  IF IT_WTMNG-LGTXT = SPACE.
    CLEAR T512T.
    SELECT SINGLE LGTXT INTO T512T-LGTXT
      FROM T512T WHERE SPRSL = SY-LANGU
                   AND MOLGA = '10'
                   AND LGART = W_LGART.
    W_LGTXT = T512T-LGTXT.
  ENDIF.
ENDMODULE.                 " MODIFY_INTAB  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INTAB  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INTAB INPUT.
  IF IT_WTMNG-STATUS = 'M'.
    IT_WTMNG-AENAM = SY-UNAME.
    IT_WTMNG-AEDAT = SY-DATUM.
    IT_WTMNG-AEZET = SY-UZEIT.
  ENDIF.
*
  MODIFY IT_WTMNG INDEX TC9000-CURRENT_LINE.
ENDMODULE.                 " CHECK_INTAB  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.
  CASE SY-UCOMM.
    WHEN 'ENTS'.
      PERFORM APPEND_DATA.
      SET SCREEN 0. LEAVE SCREEN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECKC_COLNO  INPUT
*&---------------------------------------------------------------------*
MODULE CHECKC_COLNO INPUT.
* CLEAR IT_WTMNG.
* READ TABLE IT_WTMNG WITH KEY ZFORM = W_ZFORM
*                              ZCOLN = W_ZCOLN
*                              ZMARK = SPACE.
* IF SY-SUBRC = 0.
*   MESSAGE E001 WITH 'Column No. already exists with the same key'.
* ENDIF.
* IF W_ZFORM = 3 AND ( W_ZCOLN = 1 OR W_ZCOLN = 2 OR
*                      W_ZCOLN = 3 OR W_ZCOLN = 4 ).
*   MESSAGE E001 WITH 'Reserved for Health/LIfe Insurance'.
* ENDIF.
*
* IF W_ZFORM = 1 AND ( W_ZCOLN = 1 OR W_ZCOLN = 2 ).
*   MESSAGE E001 WITH 'Reserved for Salary'.
* ENDIF.
ENDMODULE.                 " CHECKC_COLNO  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_COLNO  INPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_COLNO INPUT.
  IF W_ZPRNT NE SPACE.
    CLEAR IT_WTMNG.
    READ TABLE IT_WTMNG WITH KEY ZFORM = W_ZFORM
                                 LGART = W_ZPRNT.
    T_ZCOLN = IT_WTMNG-ZCOLN.
  ENDIF.
ENDMODULE.                 " MODIFY_COLNO  INPUT
