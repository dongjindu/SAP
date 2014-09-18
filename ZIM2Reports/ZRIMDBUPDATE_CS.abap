************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZRIMDBUPDATE_CS                                            *
*& Type   : Report                                                     *
*& Author : Manjunath Venkatesh                                        *
*& Title  : Update Program                                             *
*&---------------------------------------------------------------------*
* Help Desk Request No  :                                              *
* System Id:                                                           *
*                                                                      *
*   Requested by:       Richard Davis                                  *
*   Assigned to:        Manju                                          *
*   Original Request #:                                                *
*   ABAP Analyst:       Manju                                          *
*                                                                      *
* Business Users:  Shantel Richards                                    *
*                                                                      *
* Business Requirement Description:                                    *
* This program will reset indicators involved with financial postings  *
* in the import system if for some reason a financial posting was      *
* reversed outside of the import system,indicators need to be reset    *
* to allow for the re-posting.                                         *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
*                                                                      *
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >                               *
*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:                                                     *
*       Online Report                                                  *
*                                                                      *
* Authorization Checks:                                                *
*     < Document if any authorization objects are checked >            *
*                                                                      *
* Direct Update Database Tables:                                       *
*   < No direct updates to SAP tables are allowed.List custom tables > *
*                                                                      *
*   ZTCIVHD,ZTBKPF                                                     *
* Outstanding Issues:                                                  *
*     < If the program is being delivered with any known open issues   *
*       document them here; they could be planned for next release >   *
*                                                                      *
* Instructions on how to test this program:                            *
*     < If this program needs any special inputs like an inbound file  *
*       from an EDI subsystem for testing, document it here >          *
*                                                                      *
* Instructions on how to re-start this program:                        *
*                                                                      *
* Volume Estimates:                                                    *
*                                                                      *
* Frequency of Execution:                                              *
*   o On demand                                                        *
*                                                                      *
* Execution Mode:                                                      *
*   o Online      - Transaction Code -  ZIMU01                         *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 02/15/06    Manju        UD1K919397   Initial Coding
************************************************************************
REPORT ZRIMDBUPDATE_CS  LINE-COUNT 65
                NO STANDARD PAGE HEADING message-id db .
*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES :  ZTCIVHD,
          ZTCIVHST,
          ZTBKPF.

*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*



*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK M1 WITH FRAME TITLE TEXT-001.
Parameters: P_CIV  RADIOBUTTON GROUP R1 default 'X'.
SELECT-OPTIONS: S_CIVRN  FOR ZTCIVHD-ZFCIVNO   NO-EXTENSION
                                              NO INTERVALS.
PARAMETERS : P_ACC  RADIOBUTTON group  R1.
select-options : S_BELNR for ZTBKPF-BELNR   NO-EXTENSION
                                            NO INTERVALS,
                 S_GJAHR  for ZTBKPF-GJAHR NO-EXTENSION
                                           NO INTERVALS.
SELECTION-SCREEN END OF BLOCK M1.

*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
at selection-screen.
  if p_CIV eq 'X'.
    if s_civrn[] is initial.
      message e000 with text-003.
      SET CURSOR FIELD s_civrn.
    endif.
  endif.

  if P_ACC  eq 'X'.
    if S_BELNR[] is initial.
      message e000 with text-004.
      SET CURSOR FIELD s_BELNR.
    endif.
    if S_GJAHR[] is initial.
      message e000 with text-005.
      SET CURSOR FIELD S_GJAHR.
    endif.
  endif.



  IF p_CIV eq 'X'.
    UPDATE ZTCIVHD
       SET ZFIVST  =  'N'
     WHERE ZFCIVNO     IN  S_CIVRN.
  ENDIF.

  if p_acc eq 'X'.

    Update ZTBKPF
           set ZFPOSYN = 'N'
           ZFFIYR = ''
           ZFACDO =  ''
    where  belnr in s_belnr
       and GjAHR in s_GJAHR.
  endif.

  IF SY-SUBRC EQ 0.
    MESSAGE S977 WITH 'Update Success!'.
    COMMIT WORK.
  ELSE.
    MESSAGE S977 WITH 'Update Fail!.'.
    ROLLBACK WORK.
  ENDIF.




*Perform OlD_REPORT.
*&---------------------------------------------------------------------*
*&      Form  OlD_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM OlD_REPORT.
*REPORT  ZRIMDBUPDATE   MESSAGE-ID ZIM          .
*
*TABLES : ZTBKPF,
*         ZTBDIV,
*         ZTBHIS.
*
*DATA : W_BUKRS   LIKE  ZTBDIV-BUKRS,
*       W_GJAHR   LIKE  ZTBDIV-GJAHR,
*       W_BELNR   LIKE  ZTBDIV-BELNR,
*       W_WRBTR   LIKE  ZTBDIV-WRBTR,
*       W_DMBTR   LIKE  ZTBDIV-DMBTR,
*       W_HMF     LIKE  ZTIDSUSD-ZFHMAMT,
*       W_MPF     LIKE  ZTIDSUSD-ZFMPAMT,
*       W_DUTY    LIKE  ZTIDSUSD-ZFDUTY.
*
*SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*   SELECT-OPTIONS: S_GJAHR  FOR ZTBKPF-GJAHR   NO-EXTENSION
*                                               NO INTERVALS,
*                   S_BELNR  FOR ZTBKPF-BELNR   NO-EXTENSION
*                                               NO INTERVALS.
*PARAMETERS : P_ITEM      AS CHECKBOX DEFAULT 'X'. ">Display Item
*SELECTION-SCREEN END OF BLOCK B1.
*
**----------------------------------------------------------------------
*-
** START OF SELECTION ?
**----------------------------------------------------------------------
*-
*START-OF-SELECTION.
*
*   IF S_GJAHR[] IS INITIAL.
*      MESSAGE E977 WITH 'Input Year'.
*      EXIT.
*   ENDIF.
*   IF S_BELNR[] IS INITIAL.
*      MESSAGE E977 WITH 'Input Import Document No!'.
*      EXIT.
*   ENDIF.
*
*   IF P_ITEM NE 'X'.
*      UPDATE ZTBKPF
*         SET ZFPOSYN  =  'N'
*             ZFFIYR   =  ' '
*             ZFACDO   =  ' '
*       WHERE GJAHR    IN  S_GJAHR
*         AND BELNR    IN  S_BELNR.
*   ENDIF.
*
*   DELETE FROM ZTBHIS
*    WHERE ZFGJAHR NE '2005'
*      AND GJAHR   IN S_GJAHR
*      AND BELNR   IN S_BELNR.
*
*   IF SY-SUBRC EQ 0.
*      MESSAGE S977 WITH 'Update Success!'.
*      COMMIT WORK.
*   ELSE.
*      MESSAGE S977 WITH 'Update Fail!.'.
*      ROLLBACK WORK.
*   ENDIF.
*ENDFORM.                    " OlD_REPORT
