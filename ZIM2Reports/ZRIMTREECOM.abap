*&---------------------------------------------------------------------*
*&  INCLUDE ZRIMTREECOM                                                *
*&---------------------------------------------------------------------*
*&  프로그램명 : 계층구조 구조용 공용 데이타 및 함수 INCLUDE           *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.31                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
INCLUDE <SYMBOL>.
INCLUDE <ICON>.

* TABLE 선?
TABLES : SNODETEXT,       " 계층구조노드에 대한 추가정?
         DOKHL.           " 문서: 헤?

* TREE VIEW용 INTERNAL TABLE
DATA: BEGIN OF IT_TREELIST OCCURS 0.        " Internal table hierarchy
      INCLUDE STRUCTURE SNODETEXT.
DATA: END OF IT_TREELIST.

DATA : TXT_REPORT LIKE DOKHL-OBJECT.    "Report name for documentation
DATA : F15 TYPE C.
DATA : W_BEWTP    LIKE EKBE-BEWTP.

*&---------------------------------------------------------------------*
*&      Form  HIERARCHY
*&---------------------------------------------------------------------*
FORM HIERARCHY.
  PERFORM BUILD_TREE.
  PERFORM DRAW_TREE.
ENDFORM.                    " HIERARCHY

*&---------------------------------------------------------------------*
*&      Form  BUILD_TREE
*&---------------------------------------------------------------------*
FORM BUILD_TREE.
  CALL FUNCTION 'RS_TREE_CONSTRUCT'
       TABLES
            NODETAB      = IT_TREELIST
       EXCEPTIONS
            TREE_FAILURE = 1.

  IF SY-SUBRC = 1.
  ELSE.
  ENDIF.
ENDFORM.                    " BUILD_TREE

*&---------------------------------------------------------------------*
*&      Form  DRAW_TREE
*&---------------------------------------------------------------------*
FORM DRAW_TREE.

  SY-LSIND = 0.
  CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
       EXPORTING
            CALLBACK_PROGRAM      = SY-CPROG
            CALLBACK_USER_COMMAND = 'NODE_SELECT'
       IMPORTING
            F15                   = F15.

ENDFORM.                    " DRAW_TREE

*&---------------------------------------------------------------------*
*&      Form  P2000_NODE_ACTION
*&---------------------------------------------------------------------*
FORM P2000_NODE_ACTION.
   TXT_REPORT = SY-CPROG.

   CALL FUNCTION 'ERGO_TEXT_SHOW'
        EXPORTING
             TEXTNAME       = TXT_REPORT
             ID             = 'RE'
             LANGU          = SY-LANGU
        EXCEPTIONS
             TEXT_NOT_FOUND = 01.

ENDFORM.                    " P2000_NODE_ACTION

*---------------------------------------------------------------------*
*       FORM NODE_SELECT                                              *
*---------------------------------------------------------------------*
*       Handles selection of nodes                                    *
*---------------------------------------------------------------------*
FORM NODE_SELECT TABLES   KNOTEN   STRUCTURE     SEUCOMM
                 USING    COMMAND
                 CHANGING EXIT     LIST_REFRESH.

  DATA: ANTWORT.

* Processing of commands for hierarchy list
  CASE COMMAND.
    WHEN 'TRSL'.                       "F2 = Select
      PERFORM   P2000_NODE_SELECT  TABLES  KNOTEN
                                   USING   EXIT.

    WHEN 'TRRT'.                       "F3 = Back
      PERFORM   P2000_NODE_BACK     USING  ANTWORT
                                           EXIT.
* F15 = Exit
    WHEN 'TREX'.                       "Beenden
      PERFORM   P2000_NODE_EXIT     USING  ANTWORT
                                           EXIT.
*  Cancel hierarchy
    WHEN 'TRCN'.
      PERFORM   P2000_NODE_CANCEL   USING  ANTWORT
                                           EXIT.
* SELECT
    WHEN 'ERLE'.
      PERFORM   P2000_NODE_ACTION.
    WHEN OTHERS.
  ENDCASE.

  LIST_REFRESH = 'X'.
* CLEAR : LIST_REFRESH.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  P2000_NODE_EXIT
*&---------------------------------------------------------------------*
FORM P2000_NODE_EXIT USING    ANTWORT
                              EXIT.

   CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
             DEFAULTOPTION = 'Y'
             TEXTLINE1     = 'Soll die Hierarchie vorher '
             TEXTLINE2     = 'gesichert werden?'
             TITEL         = 'Hierarchie beenden'
             START_COLUMN  = 25
             START_ROW     = 6
        IMPORTING
             ANSWER        = ANTWORT.
   CASE ANTWORT.
     WHEN 'J'.
                                       " Sichern
       EXIT = 'X'.
     WHEN  'N'.
       EXIT = 'X'.
     WHEN 'A'.
       EXIT = ' '.
   ENDCASE.

ENDFORM.                    " P2000_NODE_EXIT

*&---------------------------------------------------------------------*
*&      Form  P2000_NODE_BACK
*&---------------------------------------------------------------------*
FORM P2000_NODE_BACK USING    ANTWORT
                              EXIT.

   CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
             DEFAULTOPTION = 'Y'
             TEXTLINE1     = 'Soll die Hierarchie vorher '
             TEXTLINE2     = 'gesichert werden?'
             TITEL         = 'Hierarchie verlassen'
             START_COLUMN  = 25
             START_ROW     = 6
        IMPORTING
             ANSWER        = ANTWORT.
   CASE ANTWORT.
     WHEN 'J'.
                                       " Sichern
       EXIT = 'X'.
     WHEN  'N'.
       EXIT = 'X'.
     WHEN 'A'.
       EXIT = ' '.
   ENDCASE.

ENDFORM.                    " P2000_NODE_BACK

*&---------------------------------------------------------------------*
*&      Form  P2000_NODE_CANCEL
*&---------------------------------------------------------------------*
FORM P2000_NODE_CANCEL USING    ANTWORT
                                EXIT.

   CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
        EXPORTING
             TEXTLINE1    = 'M?hten Sie die Hierarchie'
             TEXTLINE2    = 'abbrechen?'
             TITEL        = 'Hierarchie abbrechen'
             START_COLUMN = 25
             START_ROW    = 6
        IMPORTING
             ANSWER       = ANTWORT.
   CASE ANTWORT.
     WHEN 'J'.
                                    " Sichern
       EXIT = 'X'.
     WHEN  'N'.
       EXIT = ' '.
   ENDCASE.

ENDFORM.                    " P2000_NODE_CANCEL

*&---------------------------------------------------------------------*
*&      Form  P2000_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P2000_NODE_WRITE    USING   LEVEL
                                 NAME
                                 COLOR
                                 INTENSIV
                                 TEXT
                                 HIDE.
  CLEAR : IT_TREELIST.

  IT_TREELIST-TLEVEL   = LEVEL.
  IT_TREELIST-NAME     = NAME.
  IT_TREELIST-COLOR    = 3.
  IT_TREELIST-INTENSIV = '0'.
  IT_TREELIST-HIDE     = HIDE.
  IF NAME EQ 'EXEC'.
     IT_TREELIST-TEXT     = ICON_EXECUTE_OBJECT.
  ELSE.
     CLEAR: IT_TREELIST-TEXT.
  ENDIF.
  IT_TREELIST-TLENGTH = STRLEN( IT_TREELIST-TEXT ).

  IT_TREELIST-TCOLOR    = COLOR.
  IT_TREELIST-TINTENSIV = INTENSIV.
  IT_TREELIST-TEXT1     = TEXT.
  IT_TREELIST-TLENGTH1 = STRLEN( IT_TREELIST-TEXT1 ).
  APPEND     IT_TREELIST.

ENDFORM.                    " P2000_NODE_WRITE
