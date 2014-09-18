************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZRPP_SET_FIRM_FLAG                                         *
*& Type   : Report/Interface                                           *
*& Author : Manjunath                                                  *
*& Title  : Program to set FIRM FLAG in Planned orders                 *
*&---------------------------------------------------------------------*
* Help Desk Request No  :- 638C453356                                  *
* System Id:                                                          *
*                                                                      *
*   Requested by:        Kevin Able
*   Assigned to:                                                       *
*   Original Request #:                                                *
*   ABAP Analyst:   Manjunath Venkatesh                                *
*                                                                      *
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
*             To make sure that Firm component flag is properly set and
*             improve the accuracy of MRP data and also improve the
*             runtime time of MRP.                                     *
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
* 04/03/06    Manju        UD1K919948   Initial Coding
************************************************************************
REPORT ZRPP_SET_FIRM_FLAG LINE-SIZE 132 LINE-COUNT 65
                             NO STANDARD PAGE HEADING message-id db .


*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES :plaf, ZTPP_BFST.


*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
data : begin of it_tab occurs 0,
        plnum like plaf-plnum,
        flag(1) type c,
       end of it_tab.


*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-BL1.
select-options : s_plnum for plaf-plnum.
parameters : p_date like plaf-pedtr default sy-datum obligatory.
SELECTION-SCREEN end OF BLOCK BL1.
*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
perform get_data.

*-------------------------------------------------------------*
* End-of-selection
*--------------------------------------------------------------*
Perform change_firm_ind.

Perform display_log.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  if s_plnum is initial.
    select plnum into table it_tab
           from plaf inner join ZTPP_BFST
           on plaf~plnum = ZTPP_BFST~PLAN_ORD
           where PEDTR <= p_date
               and auffx = 'X'
               and STLFX <> 'X'
               and ZTPP_BFST~BFP01_FLG <> '00'.
  else.
    select plnum into table it_tab
           from plaf inner join ZTPP_BFST
           on plaf~plnum = ZTPP_BFST~PLAN_ORD
           where plnum in s_plnum
               and PEDTR <= p_date
               and auffx = 'X'
               and STLFX <> 'X'
               and ZTPP_BFST~BFP01_FLG <> '00'.
  endif.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  change_firm_ind
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_firm_ind.
  DATA :ls_headerdata    LIKE  bapiplaf_i2,
         ls_headerdatax   LIKE  bapiplaf_i2x.
  write :/ 'Order Numbers'.
  skip 1.
  Loop at it_tab.

    CALL FUNCTION 'Z_FPP_BAPI_PLAN_ORDER_CHANGE'
         EXPORTING
              l_plnum = it_tab-Plnum.
    if sy-subrc eq 0.
      it_tab-flag = 'X'.
      modify it_tab transporting flag.
    endif.

  endloop.

ENDFORM.                    " change_firm_ind
*&---------------------------------------------------------------------*
*&      Form  display_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log.

  loop at it_tab where flag = 'X'.
    write :/ it_tab-plnum.
  endloop.

ENDFORM.                    " display_log
