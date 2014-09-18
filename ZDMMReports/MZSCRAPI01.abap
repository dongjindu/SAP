*----------------------------------------------------------------------*
***INCLUDE MZSCRAPI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000 INPUT.
*****  Screen Navigation part
  CLEAR: W_CALL, W_SUB_CALL.
  IF SY-UCOMM = 'RPSP'. "OR SY-UCOMM = 'VOQN'.
    CLEAR : MATNR,
        MAKT-MAKTG,
        MARD-LGPBE,
        MARA-MFRPN,
        MARC-VSPVB,
        LFA1-LIFNR,
        MSEG-CHARG,
        LFA1-NAME1.

    W_CALL = 'RPSP'.
    IF SY-UCOMM = 'VOQN'.
      W_OR_REPLACE = SY-UCOMM.
*      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
*        EXPORTING
**        TITEL              = ' '
*        TEXTLINE1     = '    You have entered the part reorder screen,'
*          TEXTLINE2     = '    first a scrap ticket must be generated.'
*         START_COLUMN       = 30
*         START_ROW          = 8
      .
    ELSE.
      CLEAR: W_OR_REPLACE.
    ENDIF.
    CALL SCREEN '1001'.
    LEAVE TO SCREEN '1000'.
  ELSEIF SY-UCOMM = 'OREP'.
    CLEAR : QMNUM,
            VN_MATNR,
            ERNAM,
            ERDAT,
            MZEIT,
            CUASETXT,
            TJ30T-TXT30.

    LEAVE TO SCREEN '1003'.
** Added by Furong on 11/10/08
  ELSEIF SY-UCOMM = 'PRSR'.
    CALL TRANSACTION 'ZRQM_SCRAP'.
  ELSEIF SY-UCOMM = 'PRST'.
    CALL SCREEN '1801'.
  ELSEIF SY-UCOMM = 'PQRE'.
    CALL TRANSACTION 'ZQM_PQRE'.
** End of addition on 11/10/08
** Added by Furong on 04/21/09
  ELSEIF SY-UCOMM = 'RCST'.
    CALL TRANSACTION 'ZQM_RCST'.
** End of addition
  ELSEIF SY-UCOMM = 'MGVR'.
    CLEAR : MATNR,
          MAKT-MAKTG,
          MARD-LGPBE,
          MARA-MFRPN,
          MARC-VSPVB,
          LFA1-LIFNR,
          LFA1-NAME1.

    W_CALL = 'MGVR'.
    CALL SCREEN '1001'.
** Changed on 09/15/09
*  ELSE.
  ELSEIF SY-UCOMM = 'VPC'.
    CLEAR : MATNR,
          MAKT-MAKTG,
          MARD-LGPBE,
          MARA-MFRPN,
          MARC-VSPVB,
          LFA1-LIFNR,
          LFA1-NAME1.

    W_CALL = 'VPC'.
    CALL SCREEN '1001'.
** End of change on 09/15/09
** Added by Furong on 05/17/10
  ELSEIF SY-UCOMM = 'SHD'.
    CLEAR : MATNR,
          MAKT-MAKTG,
          MARD-LGPBE,
          MARA-MFRPN,
          MARC-VSPVB,
          LFA1-LIFNR,
          LFA1-NAME1.

    W_CALL = 'SHD'.
    CALL SCREEN '1001'.
  ELSEIF SY-UCOMM = 'STPT'.
    CLEAR : MATNR,
          MAKT-MAKTG,
          MARD-LGPBE,
          MARA-MFRPN,
          MARC-VSPVB,
          LFA1-LIFNR,
          LFA1-NAME1.

    W_CALL = 'STPT'.
    CALL SCREEN '1001'.
** End of change on 05/17/10
** Added by Furong on 07/21/10
  ELSEIF SY-UCOMM = 'SSSC'.
** Added by Furong on 12/17/10
    CLEAR: W_ANS.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TEXT_QUESTION =
'DO you have a scrap ticket to create Supplier to Supplier Tag?'
       TEXT_BUTTON_1               = 'Yes'
       TEXT_BUTTON_2               = 'No'
      DISPLAY_CANCEL_BUTTON       = ' '
     IMPORTING
       ANSWER                      = W_ANS
     EXCEPTIONS
       TEXT_NOT_FOUND              = 1
       OTHERS                      = 2
              .
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
    IF W_ANS = '1'.
      CLEAR : QMNUM.
      W_CALL = 'SSSC'.
      CALL SCREEN '1201'.
    ELSE.
      CLEAR: W_ANS.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
         TEXT_QUESTION = 'Do you want to create a Rework Scrap Ticket?'
         TEXT_BUTTON_1               = 'Yes'
         TEXT_BUTTON_2               = 'No'
        DISPLAY_CANCEL_BUTTON       = ' '
       IMPORTING
         ANSWER                      = W_ANS
       EXCEPTIONS
         TEXT_NOT_FOUND              = 1
         OTHERS                      = 2
                .
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.
      IF W_ANS = '1'.
        CLEAR : MATNR,
               MAKT-MAKTG,
               MARD-LGPBE,
               MARA-MFRPN,
               MARC-VSPVB,
               LFA1-LIFNR,
               MSEG-CHARG,
               LFA1-NAME1.
        W_CALL = 'SSRW'.
        CALL SCREEN '1001'.
      ELSE.
** end of addition
        CLEAR : QMNUM.
        W_CALL = 'SSSC'.
        CALL SCREEN '1201'.
      ENDIF.
    ENDIF.
** End of change on 05/17/10

  ELSEIF SY-UCOMM ='EXIT' OR  SY-UCOMM ='BACK'.
    SY-UCOMM = 'EXIT'.
    LEAVE PROGRAM.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_1000  INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1001 INPUT.

  IF SY-UCOMM = 'EXIT'.
    LEAVE PROGRAM.
  ELSEIF SY-UCOMM = 'NEXT'.
    PERFORM CHECK_VALUES.
   MESSAGE I026 WITH 'Click "Display/Find" to fill required data first'.
  ELSEIF SY-UCOMM = 'BACK'.
    LEAVE TO SCREEN '1000'.
  ELSEIF SY-UCOMM = 'FIND'.
    PERFORM FIND_PART.
    PERFORM CHECK_VALUES.
    CASE W_CALL.
      WHEN 'MGVR' OR 'VPC'.
        LEAVE TO SCREEN '1004'.
      WHEN 'SHD'.
        LEAVE TO SCREEN '1005'.
      WHEN 'STPT'.
        LEAVE TO SCREEN '1006'.
      WHEN OTHERS.
        LEAVE TO SCREEN '1002'.
    ENDCASE.
*    IF W_CALL = 'MGVR' OR  W_CALL = 'VPC'.
*      LEAVE TO SCREEN '1004'.
*    ELSE.
*      LEAVE TO SCREEN '1002'.
*    ENDIF.
  ELSEIF SY-UCOMM = 'DISP'.
    PERFORM CHECK_VALUES.
    PERFORM UPDATE_VALUES.
    CASE W_CALL.
      WHEN 'MGVR' OR 'VPC'.
        LEAVE TO SCREEN '1004'.
      WHEN 'SHD'.
        LEAVE TO SCREEN '1005'.
      WHEN 'STPT'.
        LEAVE TO SCREEN '1006'.
      WHEN OTHERS.
        LEAVE TO SCREEN '1002'.
    ENDCASE.

*    IF W_CALL = 'MGVR'  OR  W_CALL = 'VPC'.
*      LEAVE TO SCREEN '1004'.
*    ELSE.
*      LEAVE TO SCREEN '1002'.
*    ENDIF.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1002 INPUT.

  IF SY-UCOMM = 'EXIT'.
    LEAVE PROGRAM .
  ELSEIF SY-UCOMM = 'PROCESS'.
    PERFORM PROCESS.
    PERFORM CLEAR_SCREEN_1001.
    IF W_OR_REPLACE = 'VOQN'.

      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
        EXPORTING
*        TITEL              = ' '
       TEXTLINE1 = 'PC has been notified to order your replacement part'
*       TEXTLINE2     = '   '
         START_COLUMN       = 30
         START_ROW          = 8.
    ENDIF.
    IF W_CALL = 'SSRW'.
      QMNUM = P_QMNUM.
      W_SUB_CALL = W_CALL.
      W_CALL = 'SSSC'.
      CALL SCREEN '1201'.
    ELSE.
      LEAVE TO SCREEN '1000'.
    ENDIF.
  ELSEIF SY-UCOMM = 'BACK'.
    LEAVE TO SCREEN '1001'.
  ELSEIF SY-UCOMM = 'CREAV'.
    LEAVE TO SCREEN '1100'.
  ELSEIF SY-UCOMM = 'DISPV'.
    PERFORM DISPLAY_VAR.
  ELSEIF SY-UCOMM = 'CHNGV'.
    PERFORM CHANGE_VAR.
  ELSEIF SY-UCOMM = 'DELEV'.
    PERFORM DELETE_VAR.
  ELSEIF SY-UCOMM = 'OK'.
    PERFORM GET_CURSOR.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_VALUES INPUT.

  DATA: DYNAME LIKE D020S-PROG , DYNUMB LIKE D020S-DNUM .
  DATA: BEGIN OF IT_DYNPFIELDS OCCURS 3.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF IT_DYNPFIELDS.

  DATA: BEGIN OF IT_MARA OCCURS 0,
        MATNR TYPE MARA-MATNR,
        END OF IT_MARA.

  DATA: RET_TAB TYPE DDSHRETVAL OCCURS 0 WITH HEADER LINE,
        FLD_TAB TYPE DFIES OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF VAL_TAB OCCURS 0,
        MATNR TYPE MARA-MATNR,
        END OF VAL_TAB.

  DATA: V_MATNR TYPE MARA-MATNR.

  DYNAME = 'SAPMZSCRAP'.
  DYNUMB = SY-DYNNR.
*  DYNUMB = '1001'.

  MOVE 'MATNR' TO IT_DYNPFIELDS-FIELDNAME.
  APPEND IT_DYNPFIELDS.


  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME                         = DYNAME
      DYNUMB                         = DYNUMB
    TRANSLATE_TO_UPPER               = 'X'
*   REQUEST                        = ' '
*   PERFORM_CONVERSION_EXITS       = ' '
*   PERFORM_INPUT_CONVERSION       = ' '
*   DETERMINE_LOOP_INDEX           = ' '
    TABLES
      DYNPFIELDS                     = IT_DYNPFIELDS
* EXCEPTIONS
*   INVALID_ABAPWORKAREA           = 1
*   INVALID_DYNPROFIELD            = 2
*   INVALID_DYNPRONAME             = 3
*   INVALID_DYNPRONUMMER           = 4
*   INVALID_REQUEST                = 5
*   NO_FIELDDESCRIPTION            = 6
*   INVALID_PARAMETER              = 7
*   UNDEFIND_ERROR                 = 8
*   DOUBLE_CONVERSION              = 9
*   STEPL_NOT_FOUND                = 10
*   OTHERS                         = 11
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  LOOP AT IT_DYNPFIELDS.

    IF IT_DYNPFIELDS-FIELDNAME = 'MATNR'.
      V_MATNR = IT_DYNPFIELDS-FIELDVALUE.
    ENDIF.

  ENDLOOP.


  IF  NOT V_MATNR IS INITIAL.

    DO.

      REPLACE '*' WITH '%' INTO V_MATNR.
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.

    ENDDO.

    SELECT MATNR INTO CORRESPONDING FIELDS OF TABLE VAL_TAB
                             FROM ZSCRAP
                             WHERE WERKS = MARC-WERKS
                             AND MATNR LIKE V_MATNR.
*                            and ( LGORT = 'P400' OR LGORT = 'P500' ).

    LOOP AT IT_MARA.
      MOVE-CORRESPONDING IT_MARA TO VAL_TAB.
      APPEND VAL_TAB.
      CLEAR VAL_TAB.
    ENDLOOP.

  ENDIF.

  SORT VAL_TAB.

  DELETE ADJACENT DUPLICATES FROM VAL_TAB.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*      DDIC_STRUCTURE         = ' '
      RETFIELD               = 'MATNR'
*      PVALKEY                = ' '
      DYNPPROG               = DYNAME
      DYNPNR                 = DYNUMB
*      DYNPROFIELD            = ' '
*      STEPL                  = 0
*      WINDOW_TITLE           =
*      VALUE                  = ' '
      VALUE_ORG              = 'S'
*      MULTIPLE_CHOICE        = ' '
*      DISPLAY                = ' '
*      CALLBACK_PROGRAM       = ' '
*      CALLBACK_FORM          = ' '
    TABLES
      VALUE_TAB              = VAL_TAB
*      FIELD_TAB              = fld_tab
      RETURN_TAB             = RET_TAB
*      DYNPFLD_MAPPING        =
*    EXCEPTIONS
*      PARAMETER_ERROR        = 1
*      NO_VALUES_FOUND        = 2
*      OTHERS                 = 3
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.

    READ TABLE RET_TAB INDEX 1.

    MATNR = RET_TAB-FIELDVAL.

  ENDIF.

  CLEAR: IT_DYNPFIELDS,IT_DYNPFIELDS[].


  IT_DYNPFIELDS-FIELDNAME = 'MATNR'.
  IT_DYNPFIELDS-FIELDVALUE =  MATNR.
  APPEND IT_DYNPFIELDS.
  CLEAR IT_DYNPFIELDS.


  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      DYNAME                     = DYNAME
      DYNUMB                     = DYNUMB
    TABLES
      DYNPFIELDS                 = IT_DYNPFIELDS
*   EXCEPTIONS
*     INVALID_ABAPWORKAREA       = 1
*     INVALID_DYNPROFIELD        = 2
*     INVALID_DYNPRONAME         = 3
*     INVALID_DYNPRONUMMER       = 4
*     INVALID_REQUEST            = 5
*     NO_FIELDDESCRIPTION        = 6
*     UNDEFIND_ERROR             = 7
*     OTHERS                     = 8
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.



ENDMODULE.                 " display_values  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1100 INPUT.

  IF SY-UCOMM = 'EXIT'.
    CALL SCREEN '1002'.
  ELSEIF SY-UCOMM = 'SAVE'.
    PERFORM UPDATE_VARIANT.
  ENDIF.


ENDMODULE.                 " USER_COMMAND_1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_Workcentre  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_VALUES_WORKCENTRE INPUT.

**** F4 help for authorization work cnetre and with thier dependencies

  DATA : P_KATALOGART TYPE QPGR-KATALOGART,
         P_FIELDNAME  TYPE DYNPREAD-FIELDNAME,
         P_FIELDNAME1 TYPE DYNPREAD-FIELDNAME.

  P_KATALOGART = 'X'.
  P_FIELDNAME  = 'QMGRP'.
  P_FIELDNAME1 = 'QMCOD'.
  PERFORM DISPLAY_SEARCH_HELP USING P_KATALOGART
                                    P_FIELDNAME
                                    P_FIELDNAME1.


ENDMODULE.                 " display_values_Workcentre  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_rootcause  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_VALUES_ROOTCAUSE INPUT.

**** F4 help for source and with thier dependencies

  CLEAR: P_KATALOGART,
         P_FIELDNAME,
         P_FIELDNAME1.


  P_KATALOGART = 'V'.
  P_FIELDNAME  = 'OTGRP'.
  P_FIELDNAME1 = 'OTEIL'.
  PERFORM DISPLAY_SEARCH_HELP USING P_KATALOGART
                                    P_FIELDNAME
                                    P_FIELDNAME1.


ENDMODULE.                 " display_values_rootcause  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_defect  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_VALUES_DEFECT INPUT.

**** F4 help for Responsbility and with thier dependencies

  CLEAR: P_KATALOGART,
         P_FIELDNAME,
         P_FIELDNAME1.


  P_KATALOGART = 'W'.
  P_FIELDNAME  = 'FEGRP'.
  P_FIELDNAME1 = 'FECOD'.
  PERFORM DISPLAY_SEARCH_HELP USING P_KATALOGART
                                    P_FIELDNAME
                                    P_FIELDNAME1.



ENDMODULE.                 " display_values_defect  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_causecode  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_VALUES_CAUSECODE INPUT.

**** F4 help for cause code and with thier dependencies

  CLEAR: P_KATALOGART,
         P_FIELDNAME,
         P_FIELDNAME1.


  P_KATALOGART = 'Y'.
  P_FIELDNAME  = 'URGRP'.
  P_FIELDNAME1 = 'URCOD'.
  PERFORM DISPLAY_SEARCH_HELP USING P_KATALOGART
                                    P_FIELDNAME
                                    P_FIELDNAME1.


ENDMODULE.                 " display_values_causecode  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1003 INPUT.

  IF SY-UCOMM = 'EXIT'.
    LEAVE PROGRAM.
  ELSEIF SY-UCOMM = 'BACK'.
    LEAVE TO SCREEN '1000'.
  ELSEIF SY-UCOMM = 'DISP'.
    PERFORM DISPLAY_NOTIF.
  ELSEIF SY-UCOMM = 'PROC'.
    IF CUASETXT IS INITIAL.
      MESSAGE I026 WITH 'Please enter Cause Text'.
      EXIT.
    ENDIF.
    PERFORM CREATE_VOIDNOTIF.
    PERFORM CLEAR_SCREEN_1003.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
   EXPORTING
*        TITEL              = ' '
   TEXTLINE1     = '  Please remember to destroy your scrap ticket'
*          TEXTLINE2     = '   '
    START_COLUMN       = 20
    START_ROW          = 8
           .
    LEAVE TO SCREEN '1000'.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_1003  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1801  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1801 INPUT.
  IF SY-UCOMM = 'EXIT'.
    LEAVE PROGRAM.
  ELSEIF SY-UCOMM = 'BACK'.
    PERFORM CLEAR_VALUE.
    LEAVE TO SCREEN '1000'.
  ELSEIF SY-UCOMM = 'FIND'.
    PERFORM FIND_QMNUM.
  ELSEIF SY-UCOMM = 'PRINT'.
    PERFORM REPRINT_LABEL.
  ELSEIF SY-UCOMM = 'DISP'.
    PERFORM DISPLAY_VALUES.
  ELSEIF SY-UCOMM = 'SHD' OR SY-UCOMM = 'STPT'.
    W_CALL = SY-UCOMM.
    LEAVE TO SCREEN '1802'.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_1801  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1004 INPUT.
  IF SY-UCOMM = 'EXIT'.
    LEAVE PROGRAM .
  ELSEIF SY-UCOMM = 'PROCESS'.
    PERFORM PROCESS_1004.
    PERFORM CLEAR_SCREEN_1001.
    LEAVE TO SCREEN '1000'.
  ELSEIF SY-UCOMM = 'BACK'.
    LEAVE TO SCREEN '1001'.
  ELSEIF SY-UCOMM = 'CREAV'.
    LEAVE TO SCREEN '1100'.
  ELSEIF SY-UCOMM = 'DISPV'.
    PERFORM DISPLAY_VAR.
  ELSEIF SY-UCOMM = 'CHNGV'.
    PERFORM CHANGE_VAR.
  ELSEIF SY-UCOMM = 'DELEV'.
    PERFORM DELETE_VAR.
  ELSEIF SY-UCOMM = 'OK'.
    PERFORM GET_CURSOR.
  ENDIF.

ENDMODULE.                 " user_command_1004  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_1005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1005 INPUT.
  IF SY-UCOMM = 'EXIT'.
    PERFORM CLEAR_VALUSE_1005_1006.
    LEAVE PROGRAM .
  ELSEIF SY-UCOMM = 'PROCESS'.
    PERFORM PROCESS_1005.
    PERFORM CLEAR_SCREEN_1001.
    PERFORM CLEAR_VALUSE_1005_1006.
    LEAVE TO SCREEN '1000'.
  ELSEIF SY-UCOMM = 'PRN_TAG'.
    PERFORM PRINT_LABEL_1.
    PERFORM CLEAR_SCREEN_1001.
    LEAVE TO SCREEN '1000'.
  ELSEIF SY-UCOMM = 'BACK'.
    PERFORM CLEAR_VALUSE_1005_1006.
    LEAVE TO SCREEN '1001'.
  ELSEIF SY-UCOMM = 'CREAV'.
    LEAVE TO SCREEN '1100'.
  ELSEIF SY-UCOMM = 'DISPV'.
    PERFORM DISPLAY_VAR.
  ELSEIF SY-UCOMM = 'CHNGV'.
    PERFORM CHANGE_VAR.
  ELSEIF SY-UCOMM = 'DELEV'.
    PERFORM DELETE_VAR.
  ELSEIF SY-UCOMM = 'OK'.
    PERFORM GET_CURSOR.
  ENDIF.

ENDMODULE.                 " user_command_1005  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_reasoncode  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_VALUES_REASONCODE INPUT.
*  DATA: XNAME    TYPE VRM_ID,
*      XLIST    TYPE VRM_VALUES,
*      XVALUE   LIKE LINE OF XLIST,
*      L_CN LIKE XVALUE-KEY.
*
*  DATA: LT_TEMP LIKE TABLE OF T157E WITH HEADER LINE.
*
*  CLEAR : XLIST[] , XVALUE.
*
*  SELECT GRUND INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
*    FROM T157E
*    WHERE SPRAS ='EN'
*      AND BWART ='201'
*      AND GRUND > '9000'.
*
*  L_CN = '1'.
*  LOOP AT LT_TEMP.
*    XVALUE-TEXT = LT_TEMP-GRUND.
*    XVALUE-KEY  = L_CN.
*    APPEND XVALUE TO XLIST.
*    L_CN = L_CN + 1.
*  ENDLOOP.
*
*  XVALUE-TEXT = 'ALL'.
*  XVALUE-KEY  = '4'.
*  APPEND XVALUE TO XLIST .
*
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*       EXPORTING
*            ID              = 'RM07M-GRUND'
*            VALUES          = XLIST
*       EXCEPTIONS
*            ID_ILLEGAL_NAME = 1
*            OTHERS          = 2.
*  IF RM07M-GRUND IS INITIAL.
*  READ TABLE XLIST INTO XVALUE  INDEX 1.
*  RM07M-GRUND = XVALUE-TEXT.
*  ENDIF.

  DATA : BEGIN OF VALUE_TAB OCCURS 0,
           RM07M-GRUND LIKE T157E-GRUND,
           GRTXT LIKE T157E-GRTXT,
           END OF VALUE_TAB.

  SELECT GRUND AS RM07M-GRUND GRTXT
    INTO CORRESPONDING FIELDS OF TABLE VALUE_TAB
   FROM T157E
   WHERE SPRAS ='EN'
     AND BWART ='201'
     AND GRUND > '9005'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'RM07M-GRUND'
            DYNPPROG        = W_REPID
            DYNPNR          = W_DYNNR
            DYNPROFIELD     = 'RM07M-GRUND'
            WINDOW_TITLE    = 'Reason for movement'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1.

ENDMODULE.                 " display_values_reasoncode  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1802  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1802 INPUT.
  IF SY-UCOMM = 'EXIT'.
    LEAVE PROGRAM.
  ELSEIF SY-UCOMM = 'BACK'.
    PERFORM CLEAR_1802.
    LEAVE TO SCREEN '1801'.
  ELSEIF SY-UCOMM = 'FIND'.
    PERFORM FIND_MDOC.
  ELSEIF SY-UCOMM = 'REPRINT'.
    PERFORM REPRINT_LABEL_OTHERS.
    PERFORM CLEAR_1802.
  ELSEIF SY-UCOMM = 'DISPLAY'.
    PERFORM DISPLAY_MDOC.
  ENDIF.


ENDMODULE.                 " USER_COMMAND_1802  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1201  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1201 INPUT.
  IF SY-UCOMM = 'EXIT'.
    LEAVE PROGRAM.
  ELSEIF SY-UCOMM = 'NEXT'.
    PERFORM CALL_1202.
    LEAVE TO SCREEN '1000'.
  ELSEIF SY-UCOMM = 'BACK'.
    LEAVE TO SCREEN '1000'.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_1201  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1202  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1202 INPUT.

  IF SY-UCOMM = 'EXIT'.
    LEAVE PROGRAM.
  ELSEIF SY-UCOMM = 'NEXT'.
    PERFORM CHECK_VALUES.
   MESSAGE I026 WITH 'Click "Display/Find" to fill required data first'.
  ELSEIF SY-UCOMM = 'BACK'.
    LEAVE TO SCREEN '1201'.
  ELSEIF SY-UCOMM = 'FIND'.
    PERFORM FIND_PART.
    PERFORM CHECK_VALUES.
*    PERFORM UPDATE_PRIMARY_VALUE.
    LEAVE TO SCREEN '1203'.
  ELSEIF SY-UCOMM = 'DISP'.
    PERFORM CHECK_VALUES.
    PERFORM UPDATE_VALUES.
*        PERFORM UPDATE_PRIMARY_VALUE.
    LEAVE TO SCREEN '1203'.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_1202  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_1203  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1203 INPUT.
  IF SY-UCOMM = 'EXIT'.
    LEAVE PROGRAM .
  ELSEIF SY-UCOMM = 'PROCESS'.
    PERFORM PROCESS_1203.
    PERFORM CLEAR_SCREEN_1001.
    LEAVE TO SCREEN '1000'.
  ELSEIF SY-UCOMM = 'BACK'.
    LEAVE TO SCREEN '1201'.
  ELSEIF SY-UCOMM = 'CREAV'.
    LEAVE TO SCREEN '1100'.
  ELSEIF SY-UCOMM = 'DISPV'.
    PERFORM DISPLAY_VAR.
  ELSEIF SY-UCOMM = 'CHNGV'.
    PERFORM CHANGE_VAR.
  ELSEIF SY-UCOMM = 'DELEV'.
    PERFORM DELETE_VAR.
  ELSEIF SY-UCOMM = 'OK'.
    PERFORM GET_CURSOR.
  ENDIF.

ENDMODULE.                 " user_command_1203  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_qmnum_values  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_QMNUM_VALUES INPUT.
  DATA: L_DYNAME LIKE D020S-PROG ,
        L_DYNUMB LIKE D020S-DNUM .
  DATA: BEGIN OF LT_DYNPFIELDS OCCURS 3.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF LT_DYNPFIELDS.

  DATA: BEGIN OF IT_QMEL OCCURS 0,
        QMNUM LIKE QMEL-QMNUM,
        END OF IT_QMEL.

  DATA: LT_RET_TAB TYPE DDSHRETVAL OCCURS 0 WITH HEADER LINE,
        LT_FLD_TAB TYPE DFIES OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF LT_VAL_TAB OCCURS 0,
        QMNUM TYPE QMEL-QMNUM,
        END OF LT_VAL_TAB.

  DATA: V_QMNUM LIKE QMEL-QMNUM.

  RANGES: R_QMNUM FOR QMEL-QMNUM.

  L_DYNAME = 'SAPMZSCRAP'.
  L_DYNUMB = '1201'.

  MOVE 'QMNUM' TO LT_DYNPFIELDS-FIELDNAME.
  APPEND LT_DYNPFIELDS.


  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME                         = L_DYNAME
      DYNUMB                         = L_DYNUMB
    TRANSLATE_TO_UPPER               = 'X'
*   REQUEST                        = ' '
*   PERFORM_CONVERSION_EXITS       = ' '
*   PERFORM_INPUT_CONVERSION       = ' '
*   DETERMINE_LOOP_INDEX           = ' '
    TABLES
      DYNPFIELDS                     = LT_DYNPFIELDS
* EXCEPTIONS
*   INVALID_ABAPWORKAREA           = 1
*   INVALID_DYNPROFIELD            = 2
*   INVALID_DYNPRONAME             = 3
*   INVALID_DYNPRONUMMER           = 4
*   INVALID_REQUEST                = 5
*   NO_FIELDDESCRIPTION            = 6
*   INVALID_PARAMETER              = 7
*   UNDEFIND_ERROR                 = 8
*   DOUBLE_CONVERSION              = 9
*   STEPL_NOT_FOUND                = 10
*   OTHERS                         = 11
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  LOOP AT LT_DYNPFIELDS.

    IF LT_DYNPFIELDS-FIELDNAME = 'QMNUM'.
      V_QMNUM = LT_DYNPFIELDS-FIELDVALUE.
    ENDIF.

  ENDLOOP.


  IF V_QMNUM IS INITIAL.

*    DO.
*
*      REPLACE '*' WITH '%' INTO V_QMNUM.
*      IF SY-SUBRC <> 0.
*        EXIT.
*      ENDIF.
*
*    ENDDO.
*

*    SELECT QMNUM INTO CORRESPONDING FIELDS OF TABLE IT_QMEL
*                             FROM QMEL
*                             WHERE QMNUM LIKE V_QMNUM.

*  R_QMNUM-SIGN = 'I'.
*  R_QMNUM-OPTION = 'CP'.
*   R_QMNUM-LOW =  V_QMNUM.
*  APPEND R_QMNUM.
*
    SELECT QMNUM INTO TABLE IT_QMEL
                               FROM QMEL
                               WHERE QMART = 'Q3'.

    LOOP AT IT_QMEL.
      MOVE-CORRESPONDING IT_QMEL TO LT_VAL_TAB.
      APPEND LT_VAL_TAB.
      CLEAR LT_VAL_TAB.
    ENDLOOP.



    SORT LT_VAL_TAB.

    DELETE ADJACENT DUPLICATES FROM LT_VAL_TAB.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
*      DDIC_STRUCTURE         = ' '
        RETFIELD               = 'QMNUM'
*      PVALKEY                = ' '
        DYNPPROG               = L_DYNAME
        DYNPNR                 = L_DYNUMB
*      DYNPROFIELD            = ' '
*      STEPL                  = 0
*      WINDOW_TITLE           =
*      VALUE                  = ' '
        VALUE_ORG              = 'S'
*      MULTIPLE_CHOICE        = ' '
*      DISPLAY                = ' '
*      CALLBACK_PROGRAM       = ' '
*      CALLBACK_FORM          = ' '
      TABLES
        VALUE_TAB              = LT_VAL_TAB
*      FIELD_TAB              = fld_tab
        RETURN_TAB             = LT_RET_TAB
*      DYNPFLD_MAPPING        =
*    EXCEPTIONS
*      PARAMETER_ERROR        = 1
*      NO_VALUES_FOUND        = 2
*      OTHERS                 = 3
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.

      READ TABLE LT_RET_TAB INDEX 1.

      QMNUM = LT_RET_TAB-FIELDVAL.

    ENDIF.

    CLEAR: LT_DYNPFIELDS,LT_DYNPFIELDS[].


    LT_DYNPFIELDS-FIELDNAME = 'QMNUM'.
    LT_DYNPFIELDS-FIELDVALUE =  QMNUM.
    APPEND LT_DYNPFIELDS.
    CLEAR LT_DYNPFIELDS.


    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME                     = L_DYNAME
        DYNUMB                     = L_DYNUMB
      TABLES
        DYNPFIELDS                 = LT_DYNPFIELDS
*   EXCEPTIONS
*     INVALID_ABAPWORKAREA       = 1
*     INVALID_DYNPROFIELD        = 2
*     INVALID_DYNPRONAME         = 3
*     INVALID_DYNPRONUMMER       = 4
*     INVALID_REQUEST            = 5
*     NO_FIELDDESCRIPTION        = 6
*     UNDEFIND_ERROR             = 7
*     OTHERS                     = 8
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDMODULE.                 " display_qmnum_values  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_workcentre_READ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_VALUES_WORKCENTRE_READ INPUT.

*  DATA : P_KATALOGART TYPE QPGR-KATALOGART,
*         P_FIELDNAME  TYPE DYNPREAD-FIELDNAME,
*         P_FIELDNAME1 TYPE DYNPREAD-FIELDNAME.

  P_KATALOGART = 'X'.
  P_FIELDNAME  = 'QMGRP'.
  P_FIELDNAME1 = 'QMCOD'.
  PERFORM DISPLAY_SEARCH_HELP_READ USING P_KATALOGART
                                    P_FIELDNAME
                                    P_FIELDNAME1.

ENDMODULE.                 " display_values_workcentre_READ  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_rootcause_READ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_VALUES_ROOTCAUSE_READ INPUT.
  CLEAR: P_KATALOGART,
         P_FIELDNAME,
         P_FIELDNAME1.


  P_KATALOGART = 'V'.
  P_FIELDNAME  = 'OTGRP'.
  P_FIELDNAME1 = 'OTEIL'.
  PERFORM DISPLAY_SEARCH_HELP_READ USING P_KATALOGART
                                    P_FIELDNAME
                                    P_FIELDNAME1.

ENDMODULE.                 " display_values_rootcause_READ  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_defect_READ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_VALUES_DEFECT_READ INPUT.

  CLEAR: P_KATALOGART,
         P_FIELDNAME,
         P_FIELDNAME1.


  P_KATALOGART = 'W'.
  P_FIELDNAME  = 'FEGRP'.
  P_FIELDNAME1 = 'FECOD'.
  PERFORM DISPLAY_SEARCH_HELP_READ USING P_KATALOGART
                                    P_FIELDNAME
                                    P_FIELDNAME1.

ENDMODULE.                 " display_values_defect_READ  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_causecode_READ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_VALUES_CAUSECODE_READ INPUT.
  CLEAR: P_KATALOGART,
         P_FIELDNAME,
         P_FIELDNAME1.


  P_KATALOGART = 'Y'.
  P_FIELDNAME  = 'URGRP'.
  P_FIELDNAME1 = 'URCOD'.
  PERFORM DISPLAY_SEARCH_HELP_READ USING P_KATALOGART
                                    P_FIELDNAME
                                    P_FIELDNAME1.

ENDMODULE.                 " display_values_causecode_READ  INPUT
