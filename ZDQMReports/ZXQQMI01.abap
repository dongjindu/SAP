*----------------------------------------------------------------------*
***INCLUDE ZXQQMI01 .
*----------------------------------------------------------------------*
*<<<<<<<< Start of EQM01 - ZQMEX_02 QM-Notification Enhancement >>>>>>

************************************************************************
* Program Name      : ZQMEX_02
* Author            : SeungLyong, Lee
* Creation Date     : 2003.08.08.
* Specifications By : SeungLyong, Lee
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Notification Enhancement
*
*
* Related Table List:
*   QMEL - Quality Notification
*
* Modification Logs
* Date        Developer    RequestNo     Description
* 10/21/2004  Shiva        UD1K912580    Retrive the OK-CODE value &
*                                        Suppress the error message
*                                 when the custom push buttons are used.
*
************************************************************************


*&------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD_0101  INPUT
*&------------------------------------------------------------------*
MODULE GET_CURSOR_FIELD_0101 INPUT.
  CLEAR: WA_FLDTXT, WA_CUR_LINE.
  GET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDMODULE.                 " GET_CURSOR_FIELD_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_TEXT_FROM_EDITOR_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_TEXT_FROM_EDITOR_0101 INPUT.

  PERFORM GET_TEXT_EDITOR   TABLES IT_EDITOR_A
                                   IT_TLINE_A
                            USING TE_0101_A.
  PERFORM GET_TEXT_EDITOR   TABLES IT_EDITOR_B
                                   IT_TLINE_B
                            USING TE_0101_B.


ENDMODULE.                 " GET_TEXT_FROM_EDITOR_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXPORT_EDITOR_DATA_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXPORT_EDITOR_DATA_0101 INPUT.

*  FREE MEMORY ID 'ZQMELDI'.
*  FREE MEMORY ID 'ZQMELCC'.

**-- Export Long Text in ABAP memory
*---  Description of improvement  data.
  EXPORT IT_TLINE_A TO MEMORY ID 'ZQMELDI'.
*---  Content of Confirmation data.
  EXPORT IT_TLINE_B TO MEMORY ID 'ZQMELCC'.

ENDMODULE.                 " EXPORT_EDITOR_DATA_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.
  DATA: W_OKCODE1 LIKE SY-UCOMM.
  DATA : ZDAYS TYPE I.

*-----Get OK-CODE.
*  PERFORM GET_OKCODDE IN PROGRAM SAPLIQS0 USING W_OKCODE1.
*{ 09/09/11 Paul : CHange Perform
  W_OKCODE1 = sy-UCOMM.
*}

  CASE W_OKCODE1.
    WHEN '+US1' OR 'REJECT'.  "/Reject improvement:Clear Planned date
      IF ZSQM_CI_QMEL-SUCCESS = ' '.
        ZSQM_CI_QMEL-PLANDAT_OLD = ZSQM_CI_QMEL-PLANDAT.
        CLEAR : ZSQM_CI_QMEL-PLANDAT,
                ZSQM_CI_QMEL-COMPLETED.
      ENDIF.
      PERFORM SUPRESS_F31 IN PROGRAM SAPLIQS0.
      CLEAR W_OKCODE1.
    WHEN 'SUCCESS'. "/Success improvement : Set success field (checkbox)
      IF  ZSQM_CI_QMEL-SUCCESS = 'X'.
*        ZSQM_CI_QMEL-SUCCESS = ' '. "/Clear
      ELSE.
        IF NOT ZSQM_CI_QMEL-PLANDAT IS INITIAL AND
           NOT IT_TLINE_A IS INITIAL           AND
           ZSQM_CI_QMEL-COMPLETED = 'X'.
          ZSQM_CI_QMEL-SUCCESS = 'X'.
        ENDIF.
      ENDIF.
      PERFORM SUPRESS_F31 IN PROGRAM SAPLIQS0.
      CLEAR W_OKCODE1.
*-  Complete Status for Vendor action : Completed field set Button
    WHEN 'COMPLT'.
*    IF  ZSQM_CI_QMEL-responsive is initial.
*     MESSAGE E003(ZMQM).
*     endif.
*            WITH 'Activity : Code.G is required.'(E11).
      IF NOT ZSQM_CI_QMEL-PLANDAT IS INITIAL AND            "UD1K940665
*         NOT IT_TLINE_A IS INITIAL     AND  "UD1K940665
*         ZSQM_CI_QMEL-COMPLETED IS INITIAL.
             ZSQM_CI_QMEL-COMPLETED NE 'X'.
        ZSQM_CI_QMEL-COMPLETED = 'X'.
      ENDIF.
      PERFORM SUPRESS_F31 IN PROGRAM SAPLIQS0.
      CLEAR W_OKCODE1.
* HASEEB MODIFIED.
    WHEN 'CANCEL'.
      CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
        EXPORTING
          BEG_DA              = VIQMEL-LTRMN
          END_DA              = SY-DATUM
        IMPORTING
          NO_DAY              = ZDAYS
*         NO_MONTH            =
*         NO_YEAR             =
*         NO_CAL_DAY          =
        EXCEPTIONS
          DATEINT_ERROR       = 1
          OTHERS              = 2.
      .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF ZDAYS > 30.
       MESSAGE E000(ZMQM) WITH 'Open New Notification for Re-occurance'.
      ELSE.
        IF  ZSQM_CI_QMEL-SUCCESS = 'X'.
*       IF  NOT ZSQM_CI_QMEL-SUCCESS IS INITIAL.
*          ZSQM_CI_QMEL-COMPLETED = 'X'.
          ZSQM_CI_QMEL-SUCCESS = ' '. "/Clear
        ENDIF.
      ENDIF.
      PERFORM SUPRESS_F31 IN PROGRAM SAPLIQS0.
      CLEAR W_OKCODE1.
    WHEN 'REPORT'.
      PERFORM CALL_ZQMP12.
      PERFORM SUPRESS_F31 IN PROGRAM SAPLIQS0.

    WHEN 'BUCH'.
      PERFORM  CHECK_NOTI_PORTAL.

*      clear w_okcode1.

* HASEEB MODIFIED.

    WHEN OTHERS.
      CLEAR W_OKCODE1.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0101  INPUT


*&---------------------------------------------------------------------*
*&      Module  CHECK_REQUIRED_FIELD_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_REQUIRED_FIELD_VALUE INPUT.

*  IF NOT ZSQM_CI_QMEL-CODE_VH    IS INITIAL AND
*         ZSQM_CI_QMEL-CODEGRP_VH IS INITIAL.
*    MESSAGE E000(ZMQM)
*            WITH 'Vehicle/Engine type: Code.G is required.'(E10).
*  ENDIF.
*
*  IF NOT ZSQM_CI_QMEL-CODE_AT    IS INITIAL AND
*         ZSQM_CI_QMEL-CODEGRP_AT IS INITIAL.
*    MESSAGE E000(ZMQM)
*            WITH 'Activity : Code.G is required.'(E11).
*  ENDIF.

ENDMODULE.                 " CHECK_REQUIRED_FIELD_VALUE  INPUT

*&---------------------------------------------------------------------*
*&      Module  FIELD_VALUE_CONTROL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FIELD_VALUE_CONTROL INPUT.
  IF ZSQM_CI_QMEL-CODE_VH    IS INITIAL.
    CLEAR ZSQM_CI_QMEL-CODEGRP_VH.
  ENDIF.

  IF ZSQM_CI_QMEL-CODE_AT    IS INITIAL.
    CLEAR ZSQM_CI_QMEL-CODEGRP_AT.
  ENDIF.

ENDMODULE.                 " FIELD_VALUE_CONTROL  INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102 INPUT.
  DATA: W_OKCODE2 LIKE SY-UCOMM.

*-----Get OK-CODE.
*  perform get_okcodde in program SAPLIQS0 using w_okcode2.
*
*  case w_okcode2.
*
*    when 'BUCH'.  "save
**      MESSAGE E000(ZMQM).
**      VIQMEL-QMNUM,VIQMEL-QMART
*      PERFORM CHECK_NOTI_PORTAL.
*      clear w_okcode2.
*    when others.
*      clear w_okcode2.
*  endcase.
ENDMODULE.                 " USER_COMMAND_0102  INPUT

*<<<<<<<< End of EQM01 - ZQMEX_02 >>>>>>

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0103 INPUT.

ENDMODULE.                 " USER_COMMAND_0103  INPUT
