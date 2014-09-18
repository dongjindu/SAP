************************************************************************
* Program Name      : ZIPP202I_MAIN
* Author            : DongYeop, Han
* Creation Date     : 2003.11.12.
* Specifications By : DongYeop, Han
* Pattern           : 1.1
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Transfer of Production Spec from PP to ALC
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  ZIPP202I_MAIN NO STANDARD PAGE HEADING
                      MESSAGE-ID ZMPP.
*----->SPEC PROCESS ERROR DATA
DATA : BEGIN OF IT_MESSAGE_S OCCURS 0,
       FUNCNAME(20),
       MSGTXT(100).
DATA : END OF IT_MESSAGE_S.

DATA : BEGIN OF IT_BDCE_S OCCURS 0,
       MSG(100).
DATA : END OF IT_BDCE_S.

*----->HCPP-P PROCESS ERROR DATA
DATA : BEGIN OF IT_MESSAGE_P OCCURS 0,
       FUNCNAME(20),
       MSGTXT(100).
DATA : END OF IT_MESSAGE_P.

*----->HCPP-Q PROCESS ERROR DATA
DATA : BEGIN OF IT_MESSAGE_Q OCCURS 0,
       FUNCNAME(20),
       MSGTXT(100).
DATA : END OF IT_MESSAGE_Q.

*----->HCPP-H PROCESS ERROR DATA
DATA : BEGIN OF IT_MESSAGE_H OCCURS 0,
       FUNCNAME(20),
       MSGTXT(100).
DATA : END OF IT_MESSAGE_H.
*----->HCPP-P PROCESS ERROR DATA
DATA : BEGIN OF IT_MESSAGE_B OCCURS 0,
       FUNCNAME(20),
       MSGTXT(100).
DATA : END OF IT_MESSAGE_B.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE T1.
SELECT-OPTIONS: S_DATE FOR SY-DATUM NO-EXTENSION.
SELECTION-SCREEN: BEGIN OF LINE,
                  POSITION 8.
PARAMETERS : C_IR RADIOBUTTON GROUP IR1 DEFAULT 'X' MODIF ID R1.
*                 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: COMMENT 12(10)  TEXT-001  FOR FIELD C_IR,
                  POSITION 28.
PARAMETERS : C_RP RADIOBUTTON GROUP IR1             MODIF ID R1.
*                  AS CHECKBOX.
SELECTION-SCREEN: COMMENT 34(10)  TEXT-002  FOR FIELD C_RP,
                  POSITION 50.
PARAMETERS : C_DL RADIOBUTTON GROUP IR1             MODIF ID R1.
*                  AS CHECKBOX.
SELECTION-SCREEN: COMMENT 56(10)  TEXT-003  FOR FIELD C_DL,
                  END OF LINE,
                  END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE T2.
POSITION 8.
PARAMETERS: C_TRANS AS CHECKBOX DEFAULT ' ' USER-COMMAND UCOMM.
SELECTION-SCREEN: COMMENT 40(20)  TEXT1.

SELECTION-SCREEN: BEGIN OF LINE,
                  POSITION 1.
PARAMETERS : C_SPEC AS CHECKBOX DEFAULT 'X' MODIF ID C1.
SELECTION-SCREEN: COMMENT 02(10)  TEXT-004  FOR FIELD C_SPEC,
                  POSITION 13.
PARAMETERS : C_HPCCQ AS CHECKBOX MODIF ID C1.
SELECTION-SCREEN: COMMENT 15(10)  TEXT-005  FOR FIELD C_HPCCQ,
                  POSITION 26.
PARAMETERS : C_HPCCP AS CHECKBOX MODIF ID C1.
SELECTION-SCREEN: COMMENT 28(10)  TEXT-006  FOR FIELD C_HPCCP,
                  POSITION 39.
PARAMETERS : C_HPCCH AS CHECKBOX MODIF ID C1.
SELECTION-SCREEN: COMMENT 41(10)  TEXT-007  FOR FIELD C_HPCCH,
                  POSITION 52.
PARAMETERS : C_HPCCB AS CHECKBOX MODIF ID C1.
SELECTION-SCREEN: COMMENT 54(10)  TEXT-008  FOR FIELD C_HPCCB,
                  POSITION 65.
PARAMETERS : C_TECH  AS CHECKBOX MODIF ID C1.
SELECTION-SCREEN: COMMENT 67(10)  TEXT-011  FOR FIELD C_TECH ,
                  END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE,
                  POSITION 1.
PARAMETERS : R_SPEC RADIOBUTTON GROUP R1  DEFAULT 'X' MODIF ID R1.
SELECTION-SCREEN: COMMENT 02(10)  TEXT-004 FOR FIELD R_SPEC ,
                  POSITION 13.
PARAMETERS : R_HPCCQ RADIOBUTTON GROUP R1 Modif ID R1.
SELECTION-SCREEN: COMMENT 15(10)  TEXT-005  FOR FIELD R_HPCCQ,
                  POSITION 26.
PARAMETERS : R_HPCCP RADIOBUTTON GROUP R1 Modif ID R1.
SELECTION-SCREEN: COMMENT 28(10)  TEXT-006  FOR FIELD R_HPCCP,
                  POSITION 39.
PARAMETERS : R_HPCCH RADIOBUTTON GROUP R1 Modif ID R1.
SELECTION-SCREEN: COMMENT 41(10)  TEXT-007  FOR FIELD R_HPCCH,
                  POSITION 52.
PARAMETERS : R_HPCCB RADIOBUTTON GROUP R1 Modif ID R1.
SELECTION-SCREEN: COMMENT 54(10)  TEXT-008  FOR FIELD R_HPCCB,
                 POSITION 65.
PARAMETERS : R_TECH  RADIOBUTTON GROUP R1 Modif ID R1.
SELECTION-SCREEN: COMMENT 67(10)  TEXT-011  FOR FIELD R_TECH ,
                  END OF LINE,
                  END OF BLOCK B2.

*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
  PERFORM SCREEN_OUTPUT.

*----------------------------------------------------------------------*
*              AT SELECTION SCREEN                                     *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_OUTPUT.

AT SELECTION-SCREEN.
  PERFORM TYPE_CHECK.

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************

START-OF-SELECTION.
  PERFORM SUBMIT_PROGRAM.
  PERFORM RETURN_MESSAGE.
  PERFORM WRITE_MESSAGE.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUTPUT
*&---------------------------------------------------------------------*
FORM SCREEN_OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'C_SPEC'.
      SCREEN-INPUT = 0.
    ENDIF.
    IF C_TRANS = 'X'.
      IF SCREEN-GROUP1 = 'R1'.
        SCREEN-ACTIVE = 0.
      ENDIF.
      TEXT1 = 'TRANSFER MODE'.
    ELSE.
      IF SCREEN-GROUP1 = 'C1'.
        SCREEN-ACTIVE = 0.
      ENDIF.
      TEXT1 = 'DISPLAY MODE'.
    ENDIF.
    IF SCREEN-NAME = 'TEXT1'.
      SCREEN-INTENSIFIED = '1'.
    ENDIF.
    IF SCREEN-NAME = 'C_HPCCH' OR SCREEN-NAME = 'R_HPCCH'  OR
       SCREEN-NAME = 'C_TECH'  OR SCREEN-NAME = 'R_TECH'   .
      SCREEN-INPUT       =  0 .
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  TYPE_CHECK
*&---------------------------------------------------------------------*
FORM TYPE_CHECK.
  IF C_IR = ' ' AND C_RP = ' ' AND C_DL = ' '.
    MESSAGE E001 WITH TEXT-009.
  ENDIF.
ENDFORM.                    " TYPE_CHECK
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_PROGRAM
*&---------------------------------------------------------------------*
FORM SUBMIT_PROGRAM.
  data: l_cnt               type i.

*--->TRANSER
  if c_rp    = 'X'.
     describe table s_date lines l_cnt.
     if l_cnt = 0.
        message i001 with text-001.
        exit.
     endif.
  endif.

  IF C_TRANS = 'X'.
    IF C_SPEC = 'X'.
      PERFORM SUBMIT_SPEC.
    ENDIF.
    IF C_HPCCQ = 'X'.
      PERFORM SUBMIT_HPCCQ.
    ENDIF.
    IF C_HPCCP = 'X'.
      PERFORM SUBMIT_HPCCP.
    ENDIF.
    IF C_HPCCH = 'X'.
      PERFORM SUBMIT_HPCCH.
    ENDIF.
    IF C_HPCCB = 'X'.
      PERFORM SUBMIT_HPCCB.
    ENDIF.
*    IF C_TECH  = 'X'.
*      PERFORM SUBMIT_TECH .
*    ENDIF.
    DELETE FROM ztpp_spec CLIENT SPECIFIED WHERE mandt = sy-mandt.
  ELSE.
*--->DISPLAY
    IF R_SPEC = 'X'.
      PERFORM SUBMIT_SPEC.
    ELSEIF R_HPCCP = 'X'.
      PERFORM SUBMIT_HPCCQ.
    ELSEIF R_HPCCQ = 'X'.
      PERFORM SUBMIT_HPCCP.
    ELSEIF R_HPCCH = 'X'.
      PERFORM SUBMIT_HPCCH.
    ELSEIF R_HPCCB = 'X'.
      PERFORM SUBMIT_HPCCB.
*   ELSEIF R_TECH  = 'X'.
*     PERFORM SUBMIT_TECH .
    ENDIF.
   ENDIF.
  ENDFORM.                    " SUBMIT_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_SPEC
*&---------------------------------------------------------------------*
FORM SUBMIT_SPEC.
  SUBMIT ZIPP202I_ZTPPVS AND RETURN
         with s_date  in s_date
         WITH C_TRANS = C_TRANS
         WITH C_IR    = C_IR
         WITH C_RP    = C_RP
         WITH C_DL    = C_DL.
ENDFORM.                    " SUBMIT_SPEC
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_HPCCQ
*&---------------------------------------------------------------------*
FORM SUBMIT_HPCCQ.
SUBMIT ZIPP204I_ZTPPVHQ AND RETURN
       with s_date  in s_date
       WITH C_TRANS = C_TRANS
       WITH C_IR    = C_IR
       WITH C_RP    = C_RP
       WITH C_DL    = C_DL.
ENDFORM.                    " SUBMIT_HPCCQ
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_HPCCP
*&---------------------------------------------------------------------*
FORM SUBMIT_HPCCP.
SUBMIT ZIPP203I_ZTPPVHP AND RETURN
       with s_date  in s_date
       WITH C_TRANS = C_TRANS
       WITH C_IR    = C_IR
       WITH C_RP    = C_RP
       WITH C_DL    = C_DL.
ENDFORM.                    " SUBMIT_HPCCP
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_HPCCH
*&---------------------------------------------------------------------*
FORM SUBMIT_HPCCH.
SUBMIT ZIPP205I_ZTPPVHH AND RETURN
       with s_date  in s_date
       WITH C_TRANS = C_TRANS
       WITH C_IR    = C_IR
       WITH C_RP    = C_RP
       WITH C_DL    = C_DL.
ENDFORM.                    " SUBMIT_HPCCH
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_HPCCB
*&---------------------------------------------------------------------*
FORM SUBMIT_HPCCB.
SUBMIT ZIPP210I_ZTPPVHB AND RETURN
       WITH C_TRANS = C_TRANS
       WITH C_IR    = C_IR
       WITH C_RP    = C_RP
       WITH C_DL    = C_DL.
ENDFORM.                    " SUBMIT_HPCCB

*&---------------------------------------------------------------------*
*&      Form  SUBMIT_TECH
*&---------------------------------------------------------------------*
FORM SUBMIT_TECH .
  MESSAGE W001 WITH TEXT-012 .
*SUBMIT ZIPP210I_ZTPPVHT AND RETURN
*       WITH C_TRANS = C_TRANS
*       WITH C_IR    = C_IR
*       WITH C_RP    = C_RP
*       WITH C_DL    = C_DL.
ENDFORM.                    " SUBMIT_TECH

*&---------------------------------------------------------------------*
*&      Form  RETURN_MESSAGE
*&---------------------------------------------------------------------*
FORM RETURN_MESSAGE.
  IMPORT  IT_MESSAGE_S
          IT_BDCE_S    FROM MEMORY ID 'MEMO'.
  IMPORT  IT_MESSAGE_P FROM MEMORY ID 'MEMO'.
  IMPORT  IT_MESSAGE_Q FROM MEMORY ID 'MEMO'.
  IMPORT  IT_MESSAGE_H FROM MEMORY ID 'MEMO'.
  IMPORT  IT_MESSAGE_B FROM MEMORY ID 'MEMO'.
ENDFORM.                    " RETURN_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE
*&---------------------------------------------------------------------*
FORM WRITE_MESSAGE.
  LOOP AT IT_MESSAGE_S.
    WRITE IT_MESSAGE_S.
  ENDLOOP.
  LOOP AT IT_BDCE_S.
    WRITE IT_BDCE_S.
  ENDLOOP.
  LOOP AT IT_MESSAGE_Q.
    WRITE IT_MESSAGE_Q.
  ENDLOOP.
  LOOP AT IT_MESSAGE_P.
    WRITE IT_MESSAGE_P.
  ENDLOOP.
  LOOP AT IT_MESSAGE_H.
    WRITE IT_MESSAGE_H.
  ENDLOOP.
  LOOP AT IT_MESSAGE_B.
    WRITE IT_MESSAGE_B.
  ENDLOOP.
  FREE MEMORY.
ENDFORM.                    " WRITE_MESSAGE
