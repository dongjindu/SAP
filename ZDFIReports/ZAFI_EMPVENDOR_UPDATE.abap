
************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZAFI_EMPVENDOR_UPDATE                                      *
*& Type   : Report/Interface                                           *
*& Author : Manju                                                      *
*& Title  : Update Employee Vendor data with HR master data            *
*&---------------------------------------------------------------------*
* Help Desk Request No  :                                              *
* System Id:                                                           *
*                                                                      *
*   Requested by:        Sudhakar Manohar                              *
*   Assigned to:         Manjunath Venkatesh                           *
*   Original Request #:                                                *
*   ABAP Analyst:       Manjunath Venkatesh                            *
*                                                                      *
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
* 05/06/05    Manju        UD1K920652  Initial Coding
************************************************************************
REPORT ZAFI_EMPVENDOR_UPDATE line-size 132 LINE-COUNT 65
                             NO STANDARD PAGE HEADING message-id db .


*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
Tables : Pa0009,
         LFa1.


*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
data : begin of it_HR occurs 0,
       Pernr like pa0009-pernr,
       banks like pa0009-banks,
       bankl like pa0009-bankl,
       bankn like pa0009-bankn,
       bkont like pa0009-bkont,
       end of it_hr.

data : begin of it_vend occurs 0,
        lifnr like lfa1-lifnr,
       end of it_vend.

data : begin of it_lifnr occurs 0,
        lifnr like lfa1-lifnr,
        NAME1 like lfa1-NAME1,
        flag type c,
        msg(80) type c,
       end of it_lifnr.

DATA:BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA:END OF BDC_TAB.

data  begin of messtab occurs 0.     " BDC MESSAGE TABLE.
        include structure bdcmsgcoll.
data  end of messtab.

data : l_mode type c value 'N'.

data : l_cnt type i,
       l_cnt1 type i.

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-BL1 .
select-options : s_pernr for pa0009-pernr,
                 s_date  for pa0009-aedtm obligatory default sy-datum.
SELECTION-SCREEN end OF BLOCK bl1 .

*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
perform select_hr_data.
perform select_vendor_data.
*-------------------------------------------------------------*
* End -of-selection
*--------------------------------------------------------------*

perform call_fk02.
perform write_log.
*&---------------------------------------------------------------------*
*&      Form  select_hr_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_hr_data.

  if not s_pernr is initial.
    select        Pernr
                  banks
                  bankl
                  bankn
                  bkont  into table it_hr
                  from pa0009
                  where pernr in s_pernr  and
                        SPRPS <> 'X'      and
                        begda <= sy-datum and
                        endda >= sy-datum and
                        aedtm in s_date.
  else.

    select        Pernr
                  banks
                  bankl
                  bankn
                  bkont  into table it_hr
                  from pa0009
                  where SPRPS <> 'X'      and
                        begda <= sy-datum and
                        endda >= sy-datum and
                        aedtm in s_date.


  endif.

ENDFORM.                    " select_hr_data
*&---------------------------------------------------------------------*
*&      Form  select_vendor_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_vendor_data.

*  it_vend[] = it_hr[].
  loop at it_hr.
    move  it_hr-pernr to it_vend-lifnr.
    unpack it_vend-lifnr to it_vend-lifnr.
    append it_vend.
  endloop.
  if not it_vend[] is initial.
    select lifnr name1 into table it_lifnr
           from lfa1 for all entries in it_vend
           where lifnr = it_vend-lifnr.

  endif.
ENDFORM.                    " select_vendor_data
*&---------------------------------------------------------------------*
*&      Form  call_fk02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_fk02.
clear l_cnt1.
  loop at it_lifnr.
    read table it_hr with key pernr = it_lifnr-lifnr.
    check sy-subrc eq 0.
    PERFORM DYNPRO USING:
  'X' 'SAPMF02K'          '0106',
  ' '  'RF02K-LIFNR'      it_lifnr-lifnr,
  ' '  'RF02K-BUKRS'      'H201',
  ' '  'RF02K-D0130'      'X',
  ' '  'BDC_OKCODE'       '/00'.

    if it_hr-banks eq '' and it_hr-bankl eq '' and
       it_hr-bankn eq '' .

      PERFORM DYNPRO USING:
'X' 'SAPMF02K'          '0130',
' '  'BDC_CURSOR'        'LFBK-BANKS(01)',
' '  'BDC_OKCODE'      '=BDEL'.

     it_lifnr-flag = 'X'.
      modify it_lifnr transporting flag.
      add 1 to l_cnt1.
    else.

      PERFORM DYNPRO USING:
  'X' 'SAPMF02K'          '0130',
  ' '  'BDC_CURSOR'        'LFBK-BKREF(01)',
  ' '  'LFBK-BANKS(01)'    it_hr-banks ,
  ' '  'LFBK-BANKL(01)'    it_hr-bankl,
  ' '  'LFBK-BANKN(01)'    it_hr-bankn ,
  ' '  'LFBK-KOINH(01)'     it_lifnr-name1,
  ' '  'LFBK-BKONT(01)'     it_hr-bkont.
    endif.

    PERFORM DYNPRO USING:
           'X'   'SAPMF02K'         '0130',
           ''    'BDC_OKCODE'      '/11'.

    CALL TRANSACTION 'FK02' USING BDC_TAB
                        MODE l_mode
                        messages into messtab.
    if sy-subrc eq 0.
     if  it_lifnr-flag = 'X'.
         modify it_lifnr transporting flag.
     else.
      it_lifnr-flag = 'X'.
      modify it_lifnr transporting flag.
      add 1 to l_cnt1.
    endif.
    else.
      it_lifnr-msg = sy-msgv1.
    endif.
    refresh bDC_TAB. clear bdc_tab.


  endloop.
ENDFORM.                                                    " call_fk02
*&---------------------------------------------------------------------*
*&      Form  write_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_log.
  describe table it_hr lines l_cnt.
  format color 1 on.
  write : / ' Summary Log '.
  write :/ .
  write :/  'Total Number of Records selected', l_cnt.
  write :/ .
  write:/ 'Total Number of records Updated', l_cnt1.
  write :/ .


  write :/ ' Employee Vendors that were updated with HR Master data'.
  write :/ .
  format color 1 off.

  loop at it_lifnr where flag = 'X'.
    write :/ it_lifnr-lifnr , it_lifnr-name1.
  endloop.

  write :/.

  write :/ 'Employee Vendors not Updated'.
  loop at it_lifnr where flag = ''.
    write :/ it_lifnr-lifnr,it_lifnr-name1, it_lifnr-msg.
  endloop.


ENDFORM.                    " write_log
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0212   text
*      -->P_0213   text
*      -->P_0214   text
*----------------------------------------------------------------------*
FORM DYNPRO USING    DYNBEGIN   P_NAME   VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR BDC_TAB.
    MOVE: P_NAME     TO BDC_TAB-PROGRAM,
          VALUE    TO BDC_TAB-DYNPRO,
          DYNBEGIN TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR BDC_TAB.
    MOVE: P_NAME    TO BDC_TAB-FNAM,
          VALUE   TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.

ENDFORM.                    " DYNPRO
