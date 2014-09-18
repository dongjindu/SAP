************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZMMR_IF011
*& Type   : Report                                                     *
*& Author : Manju                                                      *
*& Title  : Update ZZTYPE/ZZTYPE1 field in EBAN
*&---------------------------------------------------------------------*
* Help Desk Request No  :                                              *
* System Id:                                                           *
*                                                                      *
*   Requested by:       Quentin Hendry                                 *
*   Assigned to:                                                       *
*   Original Request #:                                                *
*   ABAP Analyst:      Manjunath Venkatesh
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
*                                                                      *
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
* Even though Direct SAP Table Update should not be done.But due to the
* current requirement it has to be implemented.
*                                                                      *
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
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 11/15/06    Manju        UD1K923009   Initial Coding
************************************************************************
REPORT  ZMMR_IF011 LINE-SIZE 132 LINE-COUNT 65
                             NO STANDARD PAGE HEADING message-id db .


*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES : EBAN.

TYPE-POOLS VRM.
*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*



*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:   s_banfn FOR eban-banfn obligatory,
                  s_BNFPO for eban-BNFPO,
                  s_BSART for eban-bsart obligatory,
                  s_erdat for eban-ERDAT.
SELECTION-SCREEN END OF BLOCK b1.

*Division
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETERS: p_TYPE1(1) type c AS LISTBOX VISIBLE LENGTH 3,
            p_R1     RADIOBUTTON GROUP R1 DEFAULT 'X',
*           p_R2     RADIOBUTTON GROUP R1,
            p_R3     RADIOBUTTON GROUP R1.
SELECTION-SCREEN END OF BLOCK box2.


initialization.
  DATA: LT_LIST  TYPE VRM_VALUES ,
        LW_VALUE LIKE LINE OF LT_LIST,
        LW_NAME type VRM_ID.

  LW_NAME = 'P_TYPE1'.

  MOVE : 'Z'   TO LW_VALUE-KEY,
         'Z' TO LW_VALUE-TEXT.
  INSERT LW_VALUE INTO LT_LIST INDEX 1.

  MOVE : 'E'   TO LW_VALUE-KEY,
         'E' TO LW_VALUE-TEXT.
  INSERT LW_VALUE INTO LT_LIST INDEX 2.

  MOVE : 'W'   TO LW_VALUE-KEY,
         'W' TO LW_VALUE-TEXT.
  INSERT LW_VALUE INTO LT_LIST INDEX 3.

  MOVE : 'S'   TO LW_VALUE-KEY,
         'S' TO LW_VALUE-TEXT.
  INSERT LW_VALUE INTO LT_LIST INDEX 4.

  MOVE : 'Q'   TO LW_VALUE-KEY,
         'Q' TO LW_VALUE-TEXT.
  INSERT LW_VALUE INTO LT_LIST INDEX 5.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = LW_NAME
            VALUES = LT_LIST.
*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
start-of-selection.

  if P_R1 eq 'X'.
* If Option to Update ZZTYPE is choosen
    UPDATE eban SET zztype1 = p_TYPE1
               WHERE banfn IN s_banfn and
                     bnfpo in s_bnfpo and
                     bsart in s_bsart and
                     erdat in s_erdat.
    if sy-subrc eq 0.
      COMMIT WORK AND WAIT.
    else.
      rollback work.
    endif.

    write :/ 'No. of Records of Updated' ,sy-dbcnt.

*  elseif p_R2 eq 'X'.
** If Option to Update ZZTYPE1 is choosen
*
*    UPDATE eban SET zztype1 = ''
*             WHERE banfn IN s_banfn and
*                   bnfpo in s_bnfpo and
*                   bsart in s_bsart and
*                   erdat in s_erdat.
*    if sy-subrc eq 0.
*      COMMIT WORK AND WAIT.
*    else.
*      rollback work.
*    endif.
*
*    write :/ 'No. of Records of Updated' ,sy-dbcnt.

  elseif p_r3 eq  'X'.
* If Option to Update ZZTYPE1 with ZZTYPE is choosen

    UPDATE eban SET zztype1 = eban~zztype
           wHERE banfn IN s_banfn and
                 bnfpo in s_bnfpo and
                 bsart in s_bsart and
                 erdat in s_erdat.
    if sy-subrc eq 0.
      COMMIT WORK AND WAIT.
    else.
      rollback work.
    endif.

    write :/ 'No. of Records of Updated' ,sy-dbcnt.

  endif.


end-of-selection.
