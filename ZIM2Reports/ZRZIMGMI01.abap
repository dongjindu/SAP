*&---------------------------------------------------------------------*
*& Include ZRZIMGMI01                                                  *
*&---------------------------------------------------------------------*
*&  ÇÁ·Î±×·¥¸í : ¼öÀÔ½Ã½ºÅÛ IMG ÅëÇÕ °ü¸® PAI Module Inculde           *
*&      ÀÛ¼ºÀÚ : °­¼®ºÀ INFOLINK Ltd.                                  *
*&      ÀÛ¼ºÀÏ : 2000.01.25                                            *
*&  Àû¿ëÈ¸»çPJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  GET_ROOT  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ROOT INPUT.

  CASE SY-UCOMM.
    WHEN 'CONT'.
      PERFORM ENQUEUE_DEMO.
      CALL FUNCTION 'RS_TREE_CREATE'
           EXPORTING
                ROOT_NAME = RSEU0-NEWTEXT
           IMPORTING
                ROOT_ID   = TREE-ID.
      CALL FUNCTION 'RS_TREE_MODIFY_NODE'
           EXPORTING
                NODE_ID      = TREE-ID
                TEXT         = RSEU0-NEWTEXT
                TLENGTH      = TLENGTH
           EXCEPTIONS
                ID_NOT_FOUND = 01.

  ENDCASE.
  SET SCREEN 0. LEAVE SCREEN.

ENDMODULE.                             " GET_ROOT  INPUT

*&---------------------------------------------------------------------*
*&      Module  OKCODE_0100  INPUT
*&---------------------------------------------------------------------*
MODULE OKCODE_0100 INPUT.

  CASE OK_CODE.
    WHEN 'SAVD'.
      IF DEMO_RB-REPO = 'X'.
        SELECT SINGLE * FROM TRDIR WHERE NAME = DEMOTREE-REPNAME.
        IF SY-SUBRC NE 0.
          MESSAGE E037 WITH DEMOTREE-REPNAME.
*       Report $ existiert nicht
        ENDIF.
        SEUCOMM-TYPE = 'PROG'.
      ELSE.
        SELECT SINGLE * FROM TSTC WHERE TCODE = TSTC-TCODE.
        IF SY-SUBRC NE 0.
          MESSAGE E038 WITH TSTC-TCODE.
*       TA $ existiert nicht
        ENDIF.
        DEMOTREE-REPNAME = TSTC-TCODE.
        SEUCOMM-TYPE = 'TRAN'.
      ENDIF.
      UPDATE DEMOTREE SET REPNAME = DEMOTREE-REPNAME
                     WHERE ID     = SEUCOMM-ID.
      SEUCOMM-TEXT1 = DEMOTREE-REPNAME.
      CALL FUNCTION 'RS_TREE_MODIFY_NODE'
           EXPORTING
                NODE_ID = SEUCOMM-ID
                TYPE = SEUCOMM-TYPE
                TEXT1   = SEUCOMM-TEXT1.
    WHEN 'DELD'.
      UPDATE DEMOTREE SET REPNAME = SPACE
                    WHERE ID   = SEUCOMM-ID.
      CALL FUNCTION 'RS_TREE_MODIFY_NODE'
           EXPORTING
                NODE_ID = SEUCOMM-ID
                TEXT1   = SPACE.
  ENDCASE.
  CLEAR OK_CODE.
  SET SCREEN 0. LEAVE SCREEN.

ENDMODULE.                             " OKCODE_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  F4_DEVCLASS  INPUT
*&---------------------------------------------------------------------*
MODULE F4_PROGRAM INPUT.

  DATA: BEGIN OF FIELDTAB OCCURS 10.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF FIELDTAB.
  DATA: PROGNAME LIKE SY-REPID.

  REFRESH FIELDTAB.
  FIELDTAB-FIELDNAME = 'DEMOTREE-REPNAME'.
  APPEND FIELDTAB.

  PROGNAME = SY-REPID.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME     = PROGNAME
            DYNUMB     = SY-DYNNR
       TABLES
            DYNPFIELDS = FIELDTAB
       EXCEPTIONS
            OTHERS     = 01.
  CHECK SY-SUBRC = 0.

  READ TABLE FIELDTAB INDEX 1.
  TRDIR-NAME = FIELDTAB-FIELDVALUE.
  CALL FUNCTION 'F4_REPORT'
       EXPORTING
            OBJECT = TRDIR-NAME
       IMPORTING
            RESULT = DEMOTREE-REPNAME.
  CLEAR FIELDTAB-FIELDVALUE.
  FIELDTAB-FIELDVALUE = DEMOTREE-REPNAME.
  MODIFY FIELDTAB INDEX 1.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME     = PROGNAME
            DYNUMB     = SY-DYNNR
       TABLES
            DYNPFIELDS = FIELDTAB
       EXCEPTIONS
            OTHERS     = 01.

ENDMODULE.                             " F4_DEVCLASS  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TRANSACTION  INPUT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
MODULE F4_TRANSACTION INPUT.


  REFRESH FIELDTAB.
  FIELDTAB-FIELDNAME = 'TSTC-TCODE'.
  APPEND FIELDTAB.

  PROGNAME = SY-REPID.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME     = PROGNAME
            DYNUMB     = SY-DYNNR
       TABLES
            DYNPFIELDS = FIELDTAB
       EXCEPTIONS
            OTHERS     = 01.
  CHECK SY-SUBRC = 0.
  TSTCT-TCODE = FIELDTAB-FIELDVALUE.
  READ TABLE FIELDTAB INDEX 1.
  CALL FUNCTION 'F4_TRANSACTION'
       EXPORTING
            OBJECT = TSTCT-TCODE
       IMPORTING
            RESULT = TSTC-TCODE.
  CLEAR FIELDTAB-FIELDVALUE.
  FIELDTAB-FIELDVALUE = TSTC-TCODE.
  MODIFY FIELDTAB INDEX 1.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME     = PROGNAME
            DYNUMB     = SY-DYNNR
       TABLES
            DYNPFIELDS = FIELDTAB
       EXCEPTIONS
            OTHERS     = 01.

ENDMODULE.                             " F4_DEVCLASS  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CALL METHOD cl_gui_cfw=>dispatch
       importing
           return_code = return_code.

  IF RETURN_CODE <> cl_gui_cfw=>rc_noevent.
     IF G_EVENT = 'NODE_DOUBLE_CLICK' OR
        G_EVENT = 'ITEM_DOUBLE_CLICK'.
        IF G_NODE_KEY NE G_NODE_KEY_OLD.
           PERFORM  P1000_GET_DESCRIPTION.
           W_STATUS = 'D'.
        ENDIF.
     ENDIF.
     CLEAR : G_NODE_KEY.
     EXIT.
  ENDIF.

  CASE FCODE.
     WHEN 'BACK'.                 "Zurück
        LEAVE program.
     WHEN 'CANC'.                 "Abbrechen
        Leave program.
     WHEN 'EXIT'.                 "Beenden
        leave program.
     when 'LISTP'.
        leave to screen 0.
  WHEN 'HOME'.                 "Einstieg festlegen




*  WHEN 'INFO'.                 "Dokumentation
*      CALL METHOD treev->get_selected_node IMPORTING
*           node_key = nodekey.
*      CALL METHOD cl_gui_cfw=>flush.
*      if nodekey is initial.
*        call method treev->get_selected_item importing
*           node_key = nodekey
*           item_name = itemname.
*      endif.
*      CALL METHOD cl_gui_cfw=>flush.
*      READ TABLE nodetab WITH KEY id = nodekey INTO node.
*      IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*      IF node-type IS INITIAL.
*      ELSE.
*        CALL FUNCTION 'RS_TOOL_ACCESS'
*             EXPORTING
*                  operation           = 'DOCS'
*                 object_name         = node-text1
*                 object_type         = node-type
**         ENCLOSING_OBJECT    =
**         POSITION            = ' '
**         DEVCLASS            =
**         INCLUDE             =
**         MONITOR_ACTIVATION  = 'X'
**    IMPORTING
**         NEW_NAME            =
**    TABLES
**         OBJLIST             =
*    EXCEPTIONS
*         NOT_EXECUTED        = 1
*         INVALID_OBJECT_TYPE = 2
*         OTHERS              = 3
*                  .
*      ENDIF.
*
*  WHEN 'REME'.                 "Readme first
*        CALL FUNCTION 'RS_TOOL_ACCESS'
*             EXPORTING
*                  operation           = 'DOCS'
*                  object_name         = 'SAPMSDM1'
*                  object_type         = 'PROG'
*             EXCEPTIONS
*                  not_executed        = 1
*                  invalid_object_type = 2
*                  OTHERS              = 3.
*
*  WHEN 'REPO'.                 "Zuord. Report/Trans.
  WHEN 'TEXT'.
*     PERFORM GET_SELECTED_NODE.
      CALL METHOD treev->get_selected_node
           IMPORTING
              node_key = nodekey.

      CALL METHOD cl_gui_cfw=>flush.

      if nodekey is initial.
         call method treev->get_selected_item
              importing
                 node_key = nodekey
                 item_name = itemname.

         CALL METHOD cl_gui_cfw=>flush.
      endif.

      G_NODE_KEY = nodekey.
      W_STATUS   = 'C'.
      PERFORM  P1000_GET_DESCRIPTION.

      G_NODE_KEY_TXT = G_NODE_KEY.

**  WHEN 'CANC'.
**     IF node-type IS INITIAL or node-text1 is initial.
**        MESSAGE S901(ZIM1).
**     ELSE.
**
**        SELECT * INTO  TABLE SRC_TMP
**                 FROM  ZTIMGTXT
**                 WHERE TDLINE EQ NODE-ID+3(3).
**        LOOP AT SRC_TMP.
**           APPEND SRC_TMP-TDLINE TO SRC.
**        ENDLOOP.
***     SRC[] = ''.
**     ENDIF.

  WHEN 'SAVE'.                 "Sichern
     IF node-type IS INITIAL or node-text1 is initial.
        MESSAGE S901(ZIM1).
     ELSE.

        CALL METHOD editor->get_text_as_stream
             IMPORTING text        = SRC[].

        REFRESH : SRC_TMP.
        LOOP AT SRC  INTO SRC_TMP-TDLINE.
           SRC_TMP-ZFCD  = G_NODE_KEY_TXT+3(3).
           SRC_TMP-MANDT = SY-MANDT.
           SRC_TMP-ZFSEQ = SY-TABIX.
*           SRC_TMP-TDLINE = SRC-LINE.
           APPEND SRC_TMP.
        ENDLOOP.
        SET UPDATE TASK LOCAL.

        DELETE FROM ZTIMGTXT
               WHERE ZFCD EQ G_NODE_KEY_TXT+3(3).
        MODIFY ZTIMGTXT FROM TABLE SRC_TMP.
        IF SY-SUBRC EQ 0.
           COMMIT WORK.
           W_STATUS = 'D'.
        ELSE.
           ROLLBACK WORK.
        ENDIF.

     ENDIF.
  WHEN 'CAN'.
      W_STATUS = 'D'.
* WHEN 'SRC'.                  "Quelltext
  WHEN 'MHLP'.                 "±â´É¼³¸í.
      CALL METHOD treev->get_selected_node IMPORTING
           node_key = nodekey.

      CALL METHOD cl_gui_cfw=>flush.

      if nodekey is initial.
        call method treev->get_selected_item importing
           node_key = nodekey
           item_name = itemname.
      endif.

      CALL METHOD cl_gui_cfw=>flush.
      READ TABLE nodetab WITH KEY id = nodekey INTO node.

      IF sy-subrc <> 0.
         MESSAGE E902(ZIM1).
      ENDIF.
*      IF node-type IS INITIAL or node-text1 is initial.
*         MESSAGE S900(ZIM1).
*      ELSE.

      IF node-type IS INITIAL or node-text1 is initial.
         MESSAGE S901(ZIM1).
      ELSE.
         G_ZFCD = nodekey+3(3).
         REFRESH : SRC.
         SELECT * INTO  CORRESPONDING FIELDS OF TABLE SRC
                  FROM  ZTIMGTXT
                  WHERE ZFCD = G_ZFCD.

         ZTIMGTXT = NODE.
         APPEND ZTIMGTXT TO SRC.
      ENDIF.

*  WHEN 'SWIT'.                 "Anzeigen<->Ändern
*  IF sy-pfkey = 'DMSHOWC'.
**         Berechtigungsprüfung
*        PERFORM authority_check USING act_edit.
**         Sperren
*        PERFORM enqueue_demo.
**         Hierarchie neu lesen
*        PERFORM GET_TREE_DATA USING SPACE.
**       SET PF-STATUS 'DMEDIT'.
**       SET TITLEBAR 'DMU'.
*      ELSE.
*        IF save_necessary = true.
*          CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*               EXPORTING
*                    textline1 = text-005
*                    textline2 = text-010
*                    titel     = 'Demo Center'(020)
*               IMPORTING
*                    answer    = answer.
*          CASE answer.
*            WHEN 'J'.
*              PERFORM save_changes.
*            WHEN 'A'.
*
*            WHEN 'N'.
**               Hierarchie neu lesen
*              PERFORM get_tree_data USING space.
*          ENDCASE.
*        ENDIF.
*        PERFORM dequeue_demo.
*        SET PF-STATUS 'DMSHOWC'.
*        SET TITLEBAR 'DMS'.
*      ENDIF.

*  WHEN 'TRAD'.                 "Anlegen
*
*  WHEN 'TRCM'.                 "Teilbaum komprimier.
*      CALL METHOD treev->get_selected_node IMPORTING
*           node_key = nodekey.
*      CALL METHOD cl_gui_cfw=>flush.
*      if nodekey is initial.
*        call method treev->get_selected_item importing
*           node_key = nodekey
*           item_name = itemname.
*      endif.
*      call method cl_gui_cfw=>flush.
*      if not nodekey is initial.
*          refresh nodekeytab.
*          append nodekey to nodekeytab.
*          call method treev->collapse_nodes exporting
*                 NODE_KEY_TABLE = nodekeytab.
*      endif.
*
*  WHEN 'TRDL'.                 "Löschen
*
*  WHEN 'TREP'.                 "Teilbaum expandieren
*      CALL METHOD treev->get_selected_node IMPORTING
*           node_key = nodekey.
*      CALL METHOD cl_gui_cfw=>flush.
*      if nodekey is initial.
*        call method treev->get_selected_item importing
*           node_key = nodekey
*           item_name = itemname.
*      endif.
*      call method cl_gui_cfw=>flush.
*      if not nodekey is initial.
*          refresh nodekeytab.
*          append nodekey to nodekeytab.
*          call method treev->expand_nodes exporting
*                 NODE_KEY_TABLE = nodekeytab.
**          call method cl_gui_cfw=>flush.
*      endif.
*
*  WHEN 'TRMV'.                 "Umhängen
*
*  WHEN 'TRPI'.                 "Auswählen
*      CALL METHOD treev->get_selected_node IMPORTING
*           node_key = nodekey.
*      CALL METHOD cl_gui_cfw=>flush.
*      if nodekey is initial.
*        call method treev->get_selected_item importing
*           node_key = nodekey
*           item_name = itemname.
*      endif.
*      CALL METHOD cl_gui_cfw=>flush.
*      READ TABLE nodetab WITH KEY id = nodekey INTO node.
*      IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*      IF node-type IS INITIAL.
*      ELSE.
*        CALL FUNCTION 'RS_TOOL_ACCESS'
*             EXPORTING
*                  operation           = 'TEST'
*                 object_name         = node-text1
*                 object_type         = node-type
**         ENCLOSING_OBJECT    =
**         POSITION            = ' '
**         DEVCLASS            =
**         INCLUDE             =
**         MONITOR_ACTIVATION  = 'X'
**    IMPORTING
**         NEW_NAME            =
**    TABLES
**         OBJLIST             =
*    EXCEPTIONS
*         NOT_EXECUTED        = 1
*         INVALID_OBJECT_TYPE = 2
*         OTHERS              = 3
*                  .
*      ENDIF.
*
*  WHEN 'TRRN'.                 "Umbenennen
*
*  WHEN 'WTRP'.                 "Transportieren
*
*  WHEN OTHERS.

ENDCASE.
*data: rc type i.
*call method cl_gui_cfw=>dispatch
*     importing return_code = RC.
**CALL FUNCTION 'CONTROL_DISPATCH'
**     EXPORTING
**          FCODE        = FCODE
***         SYSTEM       = ' '
***    EXCEPTIONS
***         CB_NOT_FOUND = 1
***         OTHERS       = 2
**          .
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
CLEAR FCODE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_TEXT_DATA_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_TEXT_DATA_0200 INPUT.

  IF W_STATUS EQ 'C'.
*     REFRESH : SRC.
*     CALL METHOD EDITOR->GET_TEXT_AS_R3TABLE
*               IMPORTING
*                   TABLE = SRC
*               EXCEPTIONS
*                   OTHERS = 1.
*     IF SY-SUBRC NE 0.
*     ENDIF.

*     CALL METHOD editor->get_text_as_stream
*          IMPORTING
*              text        = SRC.
* Synchronize Automation Queue after Get Methods
*     CALL METHOD cl_gui_cfw=>flush.
  ENDIF.

ENDMODULE.                 " GET_TEXT_DATA_0200  INPUT
