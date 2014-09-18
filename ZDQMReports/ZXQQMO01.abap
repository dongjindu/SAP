*----------------------------------------------------------------------*
***INCLUDE ZXQQMO01 .
*----------------------------------------------------------------------*
*<<<<<<<< Start of EQM01 - ZQMEX_02 QM-Notification Enhancement >>>>>>

TABLES: ZTQM_NOTI_PORTAL.
*CLEAR  W_FLAG.

*&------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD_0101  OUTPUT
*&------------------------------------------------------------------*
MODULE SET_CURSOR_FIELD_0101 OUTPUT.
  SET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDMODULE.                 " SET_CURSOR_FIELD_0101  OUTPUT
*&------------------------------------------------------------------*
*&      Module  SET_TEXT_EDITOR_0101  OUTPUT
*&------------------------------------------------------------------*
MODULE SET_TEXT_EDITOR_0101 OUTPUT.

*-- Create Text Editor for Description of Improvement
  PERFORM CREATE_OBJECT_FOR_TEXT_EDITOR
                        TABLES  IT_EDITOR_A
                                IT_TLINE_A
                        USING   TE_0101_A     "/Editor
                                TEC_0101_A    "/Editor Container
                                'CC_0101_A'. "// Custom Control Name


*-- Create Text Editor for Content of Confirmation
  PERFORM CREATE_OBJECT_FOR_TEXT_EDITOR
                        TABLES  IT_EDITOR_B
                                IT_TLINE_B
                        USING   TE_0101_B     "/Editor
                                TEC_0101_B    "/Editor Container
                                'CC_0101_B'. "// Custom Control Name

  PERFORM COPY_TEXT_TO_B.

ENDMODULE.                 " SET_TEXT_EDITOR_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_MODE_OF_TEXT_EDITOR_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_MODE_OF_TEXT_EDITOR_0101 OUTPUT.

  CASE SY-TCODE.
    WHEN 'QM01' OR 'QM02'.
      IF ZSQM_CI_QMEL-SUCCESS = ' '.
        PERFORM SET_EDITOR_MODE  USING : TE_0101_A  C_EDITABLE,
                                         TE_0101_B  C_EDITABLE.
      ELSE.
        PERFORM SET_EDITOR_MODE  USING : TE_0101_A  C_READ_ONLY.
      ENDIF.
    WHEN OTHERS.
      PERFORM SET_EDITOR_MODE  USING : TE_0101_A  C_READ_ONLY,
                                       TE_0101_B  C_READ_ONLY.

  ENDCASE.

ENDMODULE.                 " SET_MODE_OF_TEXT_EDITOR_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ATTRIBUTE_TEXT_EDITOR_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_ATTRIBUTE_TEXT_EDITOR_0101 OUTPUT.
  PERFORM SET_ATTRIBUTE_TEXTEDITOR  USING: TE_0101_A.
  PERFORM SET_ATTRIBUTE_TEXTEDITOR  USING: TE_0101_B.
ENDMODULE.                 " SET_ATTRIBUTE_TEXT_EDITOR_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_0101 OUTPUT.
  IF SY-TCODE = 'QM03'.  "/Display
    LOOP AT SCREEN.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDLOOP.
*  ELSEIF SY-TCODE = 'QM01'. "/create Mode - No input if Group4 = 'CHG'
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP4 = 'CHG'.
*        SCREEN-INPUT = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN_0101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  GET_TEXT_FOR_SCREEN_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_TEXT_FOR_SCREEN_0101 OUTPUT.
*Tables: ztqm_noti_portal..

*  CHECK WA_TEXT_READ_FLG IS INITIAL.
*  WA_TEXT_READ_FLG = 'X'.

*<<< Start of Delete : 10/17/2003 - sllee
**- Text of Vehicle.
*  PERFORM GET_MAKTX_OF_MATNR_FERT  USING ZSQM_CI_QMEL-VEHICLE
*                                         ZSQM_CI_QMEL-MAKTX .

**- Short Text for Code of Occurrence Location
*  PERFORM GET_TEXT_OF_CODE_QPCT      USING ZSQM_CI_QMEL-KATART_OC
*                                           ZSQM_CI_QMEL-CODEGRP_OC
*                                           ZSQM_CI_QMEL-CODE_OC
*                                  CHANGING ZSQM_CI_QMEL-KURZTEXT_OC.
*<<< End of Delete : 10/17/2003 - sllee


*<<< Start of  Added : 10/17/2003 - sllee
*- Short Text for Code of Vehicle/Engine type
  PERFORM GET_TEXT_OF_CODE_QPCT      USING ZSQM_CI_QMEL-KATART_VH
                                           ZSQM_CI_QMEL-CODEGRP_VH
                                           ZSQM_CI_QMEL-CODE_VH
                                  CHANGING ZSQM_CI_QMEL-KURZTEXT_VH.
*<<< End of Added : 10/17/2003 - sllee

*- Short Text for Code of Activity Type
  PERFORM GET_TEXT_OF_CODE_QPCT      USING ZSQM_CI_QMEL-KATART_AT
                                           ZSQM_CI_QMEL-CODEGRP_AT
                                           ZSQM_CI_QMEL-CODE_AT
                                  CHANGING ZSQM_CI_QMEL-KURZTEXT_AT.

*<<< Start of Delete : 10/17/2003 - sllee
**- Short Text for Code of Disposal Type
*  PERFORM GET_TEXT_OF_CODE_QPCT      USING ZSQM_CI_QMEL-KATART_DT
*                                           ZSQM_CI_QMEL-CODEGRP_DT
*                                           ZSQM_CI_QMEL-CODE_DT
*                                  CHANGING ZSQM_CI_QMEL-KURZTEXT_DT.
**<<< End of Delete : 10/17/2003 - sllee


*-- Get External Material Group Code.
  PERFORM GET_EXTERNAL_MAT_GRP_CODE   USING  VIQMEL-MATNR
                                   CHANGING  ZSQM_CI_QMEL-EXTWG.

*- Description for external material group
  PERFORM GET_TEXT_OF_TWEWT       USING ZSQM_CI_QMEL-EXTWG
                               CHANGING ZSQM_CI_QMEL-EWBEZ.
* code modified by 100565
  SELECT SINGLE * FROM ZTQM_NOTI_PORTAL WHERE
  QMART = VIQMEL-QMART AND QMNUM = VIQMEL-QMNUM.

*

ENDMODULE.                 " GET_TEXT_FOR_SCREEN_0101  OUTPUT
*<<<<<<<< End of EQM01 - ZQMEX_02 >>>>>>
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.
* SET PF-STATUS 'XXXXXXXXX'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LIST_BOX  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LIST_BOX OUTPUT.
  TYPE-POOLS:            VRM.

  DATA: XNAME    TYPE VRM_ID,
        XLIST    TYPE VRM_VALUES,
        XVALUE   LIKE LINE OF XLIST,
        NAME                    TYPE VRM_ID.
*  IF W_FLAG IS INITIAL.
*    W_FLAG = 'X'.
* LIST BOX SETTING

  NAME = 'ZSQM_CI_QMEL-RESPONSIVE'.

  CLEAR : XLIST[] , XVALUE.

  CASE ZSQM_CI_QMEL-RESPONSIVE.
    WHEN '100'.
      ZSQM_CI_QMEL-RESPONSIVE = '1'.
    WHEN '090'.
      ZSQM_CI_QMEL-RESPONSIVE = '2'.
    WHEN '080'.
      ZSQM_CI_QMEL-RESPONSIVE = '3'.
    WHEN '060'.
      ZSQM_CI_QMEL-RESPONSIVE = '4'.
    WHEN '  '.
      ZSQM_CI_QMEL-RESPONSIVE = '5'.
  ENDCASE.

  XVALUE-TEXT =
  '100 Timeliness, Impl of Containment,Quality of Resp (3YES)'.
  XVALUE-KEY  = '1'.
  APPEND XVALUE TO XLIST .

  XVALUE-TEXT =
  '090 Timeliness, Impl of Containment,Quality of Resp (2YES)'.
  XVALUE-KEY  = '2'.
  APPEND XVALUE TO XLIST .

  XVALUE-TEXT =
  '080 Timeliness, Impl of Containment,Quality of Resp (1YES)'.
  XVALUE-KEY  = '3'.
  APPEND XVALUE TO XLIST .

  XVALUE-TEXT =
  '060 Timeliness, Impl of Containment,Quality of Resp (0YES)'.
  XVALUE-KEY  = '4'.
  APPEND XVALUE TO XLIST .

  XVALUE-TEXT = ' '.
  XVALUE-KEY  = '5'.
  APPEND XVALUE TO XLIST .

  XVALUE-TEXT = '100 Meets Criteria 1 and 2'.
  XVALUE-KEY  = '6'.
  APPEND XVALUE TO XLIST .

  XVALUE-TEXT = '080 Fails to meet criteria 1'.
  XVALUE-KEY  = '7'.
  APPEND XVALUE TO XLIST .

  XVALUE-TEXT = '060 Fails to meet criteria 1 and 2, or 2 only'.
  XVALUE-KEY  = '8'.
  APPEND XVALUE TO XLIST .

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID              = NAME  " list box
            VALUES          = XLIST
       EXCEPTIONS
            ID_ILLEGAL_NAME = 1
            OTHERS          = 2.
*  IF ZSQM_CI_QMEL-RESPONSIVE IS INITIAL.
*    READ TABLE XLIST INTO XVALUE  INDEX 1.
*    ZSQM_CI_QMEL-RESPONSIVE = XVALUE-TEXT+0(3).
**    W_TEST = XVALUE-TEXT+0(3).
*  ELSE.
  CASE ZSQM_CI_QMEL-RESPONSIVE.
    WHEN '1' OR '6'.
      ZSQM_CI_QMEL-RESPONSIVE = '100'.
    WHEN '2'.
      ZSQM_CI_QMEL-RESPONSIVE = '090'.
    WHEN '3' OR '7'.
      ZSQM_CI_QMEL-RESPONSIVE = '080'.
    WHEN '4' OR '8'.
      ZSQM_CI_QMEL-RESPONSIVE = '060'.
    WHEN '5'.
      ZSQM_CI_QMEL-RESPONSIVE = ' '.
  ENDCASE.

*    CASE W_TEST.
*      WHEN '1' OR '6'.
*        W_TEST = '100'.
*      WHEN '2'.
*        W_TEST = '090'.
*      WHEN '3' OR '7'.
*        W_TEST = '080'.
*      WHEN '4' OR '8'.
*        W_TEST = '060'.
*    ENDCASE.
*
*  ENDIF.
  CLEAR : XLIST[] , XVALUE.
*  ENDIF.
*  PERFORM set_list_sreason
ENDMODULE.                 " LIST_BOX  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_6panel  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LIST_BOX_6PANEL OUTPUT.
  TYPE-POOLS:            VRM.

*  DATA: XLIST    TYPE VRM_VALUES,
*        XVALUE   LIKE LINE OF XLIST,
*        NAME                    TYPE VRM_ID.
**      XNAME    TYPE VRM_ID,
*  if ZSQM_CI_QMEL-PANEL6 is initial.
  NAME = 'ZSQM_CI_QMEL-PANEL6'.

*  IF NOT ZSQM_CI_QMEL-PANEL6 IS INITIAL.
*     clear: ZSQM_CI_QMEL-PANEL6.
**    IF ZSQM_CI_QMEL-PANEL6 = 'Yes'.
**      ZSQM_CI_QMEL-PANEL6 = '1'.
**    else.
**      ZSQM_CI_QMEL-PANEL6 = '2'.
**    Endif.
*  ENDIF.
CASE ZSQM_CI_QMEL-PANEL6.
  WHEN 'YES'.
    ZSQM_CI_QMEL-PANEL6 = 'Yes'.
  WHEN 'NO'.
    ZSQM_CI_QMEL-PANEL6 = 'No'.
  endcase.

  CLEAR : XLIST[] , XVALUE, XLIST.

  XVALUE-TEXT = 'Yes'.
  XVALUE-KEY  = 'Yes'.
  APPEND XVALUE TO XLIST .
  CLEAR: XVALUE.

  XVALUE-TEXT = 'No'.
  XVALUE-KEY  = 'No'.
  APPEND XVALUE TO XLIST .

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID              = NAME  " list box
            VALUES          = XLIST
       EXCEPTIONS
            ID_ILLEGAL_NAME = 1
            OTHERS          = 2.
*  if  ZSQM_CI_QMEL-PANEL6 is initial.
*  READ TABLE XLIST INTO XVALUE  INDEX 1.
*  ZSQM_CI_QMEL-PANEL6 = XVALUE-TEXT.
*  endif.
  CLEAR : XLIST[] , XVALUE.
*  endif.
ENDMODULE.                 " LIST_BOX_6panel  OUTPUT
