REPORT ZAFI_VEND_MAST_TO_GQRS LINE-SIZE 132 LINE-COUNT 65
                             NO STANDARD PAGE HEADING message-id db  .
*&---------------------------------------------------------------------*
*& Program: ZAFI_VEND_MAST_TO_GQRS                                     *
*& Type   : Interface                                                  *
*& Author : Manju                                                      *
*& Title  : Vendor Master information to GQRS System                   *
*&---------------------------------------------------------------------*
* Help Desk Request No  : 71UD66644                                    *
*   Requested by:       Andy Choi                                      *
*   Assigned to:                                                       *
*   Original Request #:                                                *
*   ABAP Analyst:       Manjunath Venkatesh                            *
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
* Need to send Vendor Master Information to GQRS Monthly               *
*                                                                      *
***********************************************************************
*  Date         Developer   Request      Description
*  02/01/2007   Manju       UD1K930601   Send Vendor master data to GQRS
*  02/12/2007   manju       UD1K930696   Program changes
***********************************************************************


*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES : KNB1.


*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
data : begin of it_knb1 occurs 0,
       burks like knb1-bukrs,
       kunnr like knb1-kunnr,
       busab like knb1-busab,
       erdat like knb1-erdat,
       ernam like knb1-ernam,
       updat like knb1-updat,
       uptim like knb1-uptim,
       end of it_knb1.

data:   it_data LIKE  TABLE OF ZSFI_VEND_MASTER
        WITH HEADER LINE.


CONSTANTS:  c_dest(10)  VALUE 'GPQS'."Outbound Destination

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-BL1 .

select-options : s_bukrs for KNB1-bukrs default 'H201'.
parameters     : p_busab like knb1-busab default 'R%',
                 P_DEST(10) type c default 'WMBOM01'.
SELECTION-SCREEN END OF BLOCK BL1.

*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
Start-of-selection.
* Select Data
  perform select_data.

* Populate final Data
  perform populate_date.

end-of-selection.

* Invoke RFC to GQRS  system to transfer Vendor Information
  Perform CALL_RFC.


*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  select bukrs kunnr busab  erdat ernam updat uptim into table it_knb1
         from KNB1
     where bukrs in s_bukrs and
           busab like p_busab.
ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  CALL_RFC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_RFC.
  DATA : lw_msgtxt(100),
         l_cnt type i.

  CHECK NOT it_DATA[] IS INITIAL.
  CALL FUNCTION 'Z_FFI_SEND_VEND_MAST_GQRS'
    DESTINATION  P_DEST
    TABLES
      IT_DATA       = IT_DATA
     EXCEPTIONS
        communication_failure  = 1  MESSAGE lw_msgtxt
        system_failure         = 2  MESSAGE lw_msgtxt.
  if sy-subrc ne 0.
    write :/ 'RFC CALL to GQRS System FAILED'.
    write :/ lw_msgtxt.
  else.
   describe table it_data lines l_cnt.   "UD1K930696
  write : / 'No of records transmitted to GQRS system', l_cnt.
"UD1K930696
  endif.

ENDFORM.                    " CALL_RFC
*&---------------------------------------------------------------------*
*&      Form  populate_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM populate_date.
  loop at it_KNB1.

    IT_DATA-COMP_CODE   = 'H'.
    IT_DATA-FACT_CODE   = 'A1'.
    IT_DATA-VEND_CODE   =  it_knb1-kunnr.

    case it_knb1-busab.
      when 'RN'.    "Reclaim; Bankrupt / FI (N)
        IT_DATA-VEND_BANK_IND =  'D'.
        IT_DATA-FI_TRAN = 'N'.
      when 'RY'.    "Reclaim; Bankrupt / FI (Y)
        IT_DATA-VEND_BANK_IND =  'D'.
        IT_DATA-FI_TRAN = 'Y'.
      when 'R1'.    "Reclaim; Normal Status
        IT_DATA-VEND_BANK_IND =  ''.
        IT_DATA-FI_TRAN = 'Y'.
      when others.
        IT_DATA-VEND_BANK_IND =  ''.
        IT_DATA-FI_TRAN = 'N'.
    endcase.

    IT_DATA-ENTER_DATE  = ''.
    IT_DATA-ENTER_TIME  = ''.
    IT_DATA-ENTERER_NAME = 'HMMAIT'.
    IT_DATA-REVISION_DATE = ''."it_knb1-updat.
    IT_DATA-REVISION_TIME = ''."it_knb1-uptim.
    IT_DATA-REVISER_NAME = 'HMMAIT'."it_knb1-ernam.
    Append it_DATA.
  endloop.
ENDFORM.                    " populate_date
