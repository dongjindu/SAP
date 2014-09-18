*&---------------------------------------------------------------------*
*& Include ZRZIMGMO01                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입시스템 IMG 통합 관리 PBO Module Inculde           *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Module  STATUS_ROOT  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_ROOT OUTPUT.

  SET PF-STATUS 'GETROOT'.
  SET TITLEBAR  'GRT'.

ENDMODULE.                             " STATUS_root  OUTPUT

*---------------------------------------------------------------------*
*       MODULE PFSTATUS_0100 OUTPUT                                   *
*---------------------------------------------------------------------*
MODULE PFSTATUS_0100 OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'DEMOTREET-STEXT'.
      SCREEN-LENGTH = TLENGTH.
      SCREEN-INPUT  = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF VALUE_REQUEST = TRUE.
    VALUE_REQUEST = FALSE.
  ELSE.
    SET TITLEBAR  'RPU'.
    SET PF-STATUS 'REPOUPDA'.
  ENDIF.
  CLEAR: TSTC-TCODE, DEMOTREE-REPNAME.
  CLEAR: DEMO_RB.
  DEMOTREET-STEXT   = SEUCOMM-TEXT.
  IF SEUCOMM-TYPE = 'PROG'.
    DEMOTREE-REPNAME = SEUCOMM-TEXT1.
    DEMO_RB-REPO = 'X'.
  ELSEIF SEUCOMM-TYPE = 'TRAN'.
    TSTC-TCODE = SEUCOMM-TEXT1.
    DEMO_RB-TRAN = 'X'.
  ELSE.
    DEMO_RB-REPO = 'X'.
  ENDIF.
ENDMODULE.                             " PFSTATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.

  IF EDITOR IS INITIAL.
     SET PF-STATUS 'DMSHOWC'.
     SET TITLEBAR 'DMS'.
*> TEXT EDIT BOX.
      CREATE OBJECT EDITCONTAINER
             EXPORTING
                CONTAINER_NAME = 'SRC'.
*                REPID          = 'SAPMZIMGM'
*                DYNNR          = '0200'.

     CREATE OBJECT EDITOR
            EXPORTING
             PARENT = EDITCONTAINER
             WORDWRAP_MODE = CL_GUI_TEXTEDIT=>WORDWRAP_AT_WINDOWBORDER
*            WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE
             WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>FALSE
*            wordwrap_position = 72
*            wordwrap_position = 132
*            MAX_NUMBER_CHARS = 100000.
*            WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>FALSE
       EXCEPTIONS
            ERROR_CNTL_CREATE      = 1
            ERROR_CNTL_INIT        = 2
            ERROR_CNTL_LINK        = 3
            ERROR_DP_CREATE        = 4
            GUI_TYPE_NOT_SUPPORTED = 5.

     IF SY-SUBRC NE 0.
       CALL FUNCTION 'POPUP_TO_INFORM'
            EXPORTING
                 TITEL = REPID
                 TXT2  = SPACE
                 TXT1  = TEXT-101.
     ENDIF.

     REFRESH : SRC_TMP.
     CLEAR SRC_TMP. APPEND SRC_TMP.
*     DO 4 TIMES.
*        APPEND SRC.
*     ENDDO.

     TEXT_ID = '010'.
     DO.
        ADD 1 TO TEXT_ID.
        CONCATENATE 'TEXT-' TEXT_ID INTO FIELDNM.
        ASSIGN (FIELDNM) TO <F>.
        SRC_TMP-TDLINE = <F>.
        IF SRC_TMP-TDLINE IS INITIAL.
           CLEAR : SRC_TMP.
        ENDIF.
        APPEND SRC_TMP.

        IF TEXT_ID GE '020'.
           EXIT.
        ENDIF.
     ENDDO.

*    CALL METHOD editor->get_text_as_stream
*         IMPORTING text        = SRC.
*
*    CALL METHOD cl_gui_cfw=>flush.

     REFRESH : SRC.
     CLEAR : SRC, SRC_TMP.
     LOOP AT SRC_TMP.
        APPEND SRC_TMP-TDLINE TO SRC.
     ENDLOOP.

     CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
                 EXPORTING
                    TABLE = SRC[].

*    CALL METHOD editor->set_text_as_stream
*         EXPORTING  text = SRC
*         EXCEPTIONS error_dp        = 1
*                    error_dp_create = 2.

*     CALL METHOD cl_gui_cfw=>FLUSH.
*     CALL METHOD editor->set_dragdrop
*            EXPORTING dragdrop = behaviour_right.
*     set handler dragdrop->flavor_select FOR editor.
*     SET HANDLER dragdrop->right_drop FOR editor.
  ELSE.

*     REFRESH : SRC.
*     CLEAR : SRC, SRC_TMP.
*     LOOP AT SRC_TMP.
*        APPEND SRC_TMP-TDLINE TO SRC.
*     ENDLOOP.

*     CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
*                 EXPORTING TABLE = SRC.

*     CALL METHOD cl_gui_cfw=>FLUSH.
  ENDIF.

  IF W_STATUS EQ 'D'.
      CALL METHOD EDITOR->SET_READONLY_MODE
       EXPORTING
            READONLY_MODE = EDITOR->TRUE
       EXCEPTIONS
            ERROR_CNTL_CALL_METHOD = 1
            INVALID_PARAMETER      = 2
            OTHERS                 = 3.
  ELSE.
      CALL METHOD EDITOR->SET_READONLY_MODE
       EXPORTING
            READONLY_MODE = EDITOR->FALSE
       EXCEPTIONS
            ERROR_CNTL_CALL_METHOD = 1
            INVALID_PARAMETER      = 2
            OTHERS                 = 3.
  ENDIF.


ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCRCOM OUTPUT.

  LOOP AT SCREEN.
    CASE W_STATUS.
      WHEN 'C' OR 'U'.
        IF SCREEN-GROUP1 = 'I'.
           SCREEN-INPUT   = '1'.
           SCREEN-INVISIBLE = '0'.
        ELSE.
           SCREEN-INPUT   = '1'.
           SCREEN-INVISIBLE = '0'.
        ENDIF.
      WHEN 'D'.
        IF SCREEN-GROUP1 = 'I'.
           SCREEN-INPUT     = '0'.
           SCREEN-INVISIBLE = '1'.
        ELSE.
           SCREEN-INPUT     = '1'.
           SCREEN-INVISIBLE = '0'.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " OPERATION_MODE_SET_SCRCOM  OUTPUT
