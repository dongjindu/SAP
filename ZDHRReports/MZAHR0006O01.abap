*----------------------------------------------------------------------*
*   INCLUDE MZAHR0006O01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  IF IT_PCP03[] IS INITIAL.
    CLEAR IT_STATS. REFRESH IT_STATS.
    IT_STATS-FCODE = 'PSAV'. APPEND IT_STATS.
    IT_STATS-FCODE = 'FSAV'. APPEND IT_STATS.
    SET PF-STATUS 'PS9000' EXCLUDING IT_STATS.
  ELSE.
    SET PF-STATUS 'PS9000'.
  ENDIF.
*
  SET TITLEBAR '900'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_CHECK  OUTPUT
*&---------------------------------------------------------------------*
MODULE INIT_CHECK OUTPUT.
  IF W_FLAGS = SPACE.
    CALL FUNCTION 'HRCA_GETEMPLOYEEDATA_FROMUSER'
         EXPORTING
              USERNAME                   = SY-UNAME
              VALIDBEGIN                 = SY-DATUM
         IMPORTING
              EMPLOYEENUMBER             = W_PERNR
              PERSONNELAREA              = W_WERKS
              NAMEOFPERSAREA             = W_NAME1
              ORGUNIT                    = W_ORGEH
              NAMEOFORGUNIT              = W_ORGTX
         EXCEPTIONS
              USER_NOT_FOUND             = 1
              COUNTRYGROUPING_NOT_FOUND  = 2
              INFTY_NOT_FOUND            = 3
              CALL_OTHERS                = 4
              OTHERS                     = 5.
*.... test data (delete after go live)
    IF SY-SUBRC <> 0.
      W_PERNR = '00100104'.
      W_WERKS = '1010'.
      W_NAME1 = 'Montgomery'.
      W_ORGEH = '90001930'.
      W_ORGTX = 'Procurement Sub Division'.
      SY-SUBRC = 0.
    ENDIF.
*
    IF SY-SUBRC = 0.
      PERFORM CHECK_CODE_BOOK.
    ELSE.
      MESSAGE S008 WITH SY-TCODE.
    ENDIF.
    W_FLAGS = 'X'.
  ENDIF.
ENDMODULE.                 " INIT_CHECK  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
  CHECK W_MASTR = 'X'.
*
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'GR1'.
      SCREEN-INPUT = 1.
      SCREEN-INTENSIFIED = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
