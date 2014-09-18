*----------------------------------------------------------------------*
*   INCLUDE MZAHR0004I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE BACK_EXIT INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " BACK_EXIT  INPUT
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
    WHEN 'NEWE'.
      W_MODES = 'M'.
      CLEAR: W_ORGEH, W_SHORT, W_STEXT, W_ZTYPE.
      CALL SCREEN 9100 STARTING AT 5 5
                       ENDING AT 60 10.
    WHEN 'SAVE'.
      PERFORM SAVE_DATA.
    WHEN 'DELE'.
      PERFORM DELETE_LINE.
  ENDCASE.
  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_INTAB  INPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_INTAB INPUT.
  SELECT SINGLE ZMARK INTO ZTHR_1001-ZMARK
    FROM ZTHR_1001 WHERE OBJID = IT_H1001-OBJID.
  IF SY-SUBRC = 0.
    IT_H1001-AENAM = SY-UNAME.
    IT_H1001-AEDAT = SY-DATUM.
    IT_H1001-AEZET = SY-UZEIT.
  ENDIF.
*
  MODIFY IT_H1001 INDEX TC9000-CURRENT_LINE.
ENDMODULE.                 " MODIFY_INTAB  INPUT
*&---------------------------------------------------------------------*
*&      Module  POP_UP_ORG_UNIT  INPUT
*&---------------------------------------------------------------------*
MODULE POP_UP_ORG_UNIT INPUT.
  CALL FUNCTION 'RP_PNP_GET_VALUE_ORGEH'
       CHANGING ORGEH = W_ORGEH.
*
  CLEAR HRP1000.
  SELECT SINGLE SHORT STEXT INTO (HRP1000-SHORT, HRP1000-STEXT)
    FROM HRP1000 WHERE PLVAR = W_PLVAR
                   AND OTYPE = W_OTYPE
                   AND OBJID = W_ORGEH
                   AND ISTAT = '1'
                   AND ENDDA = '99991231'
                   AND LANGU = SY-LANGU.
*
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.

  W_SHORT = HRP1000-SHORT.
  DYNPFIELDS-FIELDNAME = 'W_SHORT'.
  DYNPFIELDS-FIELDVALUE = W_SHORT.
  APPEND DYNPFIELDS. CLEAR DYNPFIELDS.

  W_STEXT = HRP1000-STEXT.
  DYNPFIELDS-FIELDNAME = 'W_STEXT'.
  DYNPFIELDS-FIELDVALUE = W_STEXT.
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
ENDMODULE.                 " POP_UP_ORG_UNIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.
  CASE SY-UCOMM.
    WHEN 'ENTR'.
      PERFORM APPEND_INTAB.
  ENDCASE.
  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DESCRIPTION  INPUT
*&---------------------------------------------------------------------*
MODULE GET_DESCRIPTION INPUT.
  CLEAR: W_SHORT, W_STEXT.
*
  CLEAR HRP1000.
  SELECT SINGLE SHORT STEXT INTO (HRP1000-SHORT, HRP1000-STEXT)
    FROM HRP1000 WHERE PLVAR = W_PLVAR
                   AND OTYPE = W_OTYPE
                   AND OBJID = W_ORGEH
                   AND ISTAT = '1'
                   AND ENDDA = '99991231'
                   AND LANGU = SY-LANGU.
  IF SY-SUBRC = 0.
    W_SHORT = HRP1000-SHORT.
    W_STEXT = HRP1000-STEXT.
  ENDIF.
ENDMODULE.                 " GET_DESCRIPTION  INPUT
