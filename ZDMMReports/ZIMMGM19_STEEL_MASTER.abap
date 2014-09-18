************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZIMMGM19_STEEL_MASTER                                     *
*& Type   : Interface                                                 *
*& Author : Manju                                                      *
*& Title  : Coil Master Interface program to MES System                *
*&---------------------------------------------------------------------*
* Help Desk Request No  : 67BE222414                                   *
*   Requested by:       Richard Davis                                  *
*   Assigned to:                                                       *
*   Original Request #:                                                *
*   ABAP Analyst:       Manjunath Venkatesh                            *
*                                                                      *
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
* To make Interface coil materials changes to MES System  event driven *

*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
*  This program is triggered by function module
*Z_MATERIAL_INTERFACE_00001250 which is called when material is changed
* or created through EVENT "ZMM_MAT_CHANGES".
* Program is executed as background job which is set-up to kick-off
*based on the event trigger in Function module.

* Function module.
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >
* In Config TCODE "FIBF" assign function Module
*"Z_MATERIAL_INTERFACE_00001250" to BTE Event 0001250 for Material
*master changes.
*
*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:                                                     *
*       Online Report                                                  *
* Invokes RFC to MES system and transfers material data.
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
* In MM02 makes changes to AM material group materials this program
* kicks up background job

* Instructions on how to re-start this program:                        *
*                                                                      *
* Volume Estimates:                                                    *
*                                                                      *
* Frequency of Execution:                                              *
*   o Whenever material is created or Changed in SAP
*
*                                                                      *
* Execution Mode:                                                      *
*   o Background      - Transaction Code -
*
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo      Description
* 08/10/06    Manju        UD1K921713     Initial Coding
* 08/16/06    Manju        UD1K921769     Changes to send value of    *
*                                         ZCOATING in 2 fields.     *
************************************************************************
REPORT ZIMMGM19_STEEL_MASTER  LINE-SIZE 132 LINE-COUNT 65
                             NO STANDARD PAGE HEADING message-id db .


*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*



*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*

TYPES: BEGIN OF ITAB_MAT,
       MATNR like mara-matnr,
       maktx like makt-maktx,
       profl like mara-profl,
       END OF ITAB_MAT.

data : ITAB TYPE STANDARD TABLE OF ITAB_MAT WITH NON-UNIQUE
       DEFAULT KEY INITIAL SIZE 2 with header line .

DATA: INDXKEY LIKE INDX-SRTFD  ,
      wa_zsmm_class  LIKE  zsmm_class_MES,
      it_list LIKE  TABLE OF ZSMM_SL_MASTER
      WITH HEADER LINE.

CONSTANTS:  c_dest(10)  VALUE 'WMRM01'."Outbound Destination

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*


*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*

*check sy-batch eq 'X'.
* Index Key is system date
write sy-datum to indxkey.

* Import material changes from  Data Cluster
* which was exported in FM Z_MATERIAL_INTERFACE_00001250
import  ITAB = ITAB  from DATABASE INDX(ZM) ID INDXKEY.

if sy-subrc eq 0.

* Get class & charactertics for a Material.
  loop at itab.
    perform get_CLASS_Number using itab.
  endloop.

* Invoke RFC to MES system to transfer Material data
  Perform CALL_RFC.

endif.
*&---------------------------------------------------------------------*
*&      Form  get_CLASS_Number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB_MATNR  text
*----------------------------------------------------------------------*
FORM get_CLASS_Number USING  pt_list    LIKE   itab.


  CLEAR wa_zsmm_class.
  CALL FUNCTION 'Z_FMM_GET_CHARACT_MES'
       EXPORTING
            i_matnr        = pt_list-matnr
       IMPORTING
            es_zsmm_class  = wa_zsmm_class
       EXCEPTIONS
            data_not_found = 1
            OTHERS         = 2.
  IF sy-subrc EQ 0.
    it_list-MATNR        =  pt_list-matnr.
    it_list-MAKTX        =  pt_list-MAKTX.
    it_list-PROFL        =  pt_list-PROFL.
    it_list-zprop        =  wa_zsmm_class-zprop.
*   it_list-zcoating     =  wa_zsmm_class-zcoating.
    if not wa_zsmm_class-ZFFINISH is initial.
      it_list-ZFFINISH     =  wa_zsmm_class-ZFFINISH.
    else.
      it_list-ZFFINISH     =  '000'.
    endif.
    if not wa_zsmm_class-ZBFINISH is initial.
      it_list-ZBFINISH     =  wa_zsmm_class-ZBFINISH.
    else.
      it_list-ZBFINISH    = '000'.
    endif.
    it_list-zthick       =  wa_zsmm_class-zthick.
    it_list-zwidth       =  wa_zsmm_class-zwidth.
    it_list-zlength      =  wa_zsmm_class-zlength.
    it_list-zedge        =  wa_zsmm_class-zedge.
    it_list-zmkg         =  wa_zsmm_class-zmkg.
    it_list-zstlgrade    =  wa_zsmm_class-zstlgrade.
    it_list-zdensity     =  wa_zsmm_class-zdensity.
    it_list-zinout       =  wa_zsmm_class-zinout.
    it_list-zproduct     =  wa_zsmm_class-zproduct.
    append it_list. clear it_list.
  else.
    write :/ 'No characteristics found'.
    write :/ pt_list-matnr.
  ENDIF.


ENDFORM.                    " get_CLASS_Number
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
         lw_tabix  LIKE sy-tabix.
  CLEAR: lw_msgtxt, lw_tabix.


  CHECK NOT it_list[] IS INITIAL.
  CALL FUNCTION 'Z_FMM_STEEL_MASTER'
    DESTINATION              c_dest
    TABLES
      it_ztmm_sl_master      = it_list
    EXCEPTIONS
      communication_failure  = 1  MESSAGE lw_msgtxt
      system_failure         = 2  MESSAGE lw_msgtxt.

  LOOP AT it_list.
    write :/ it_list-MATNR.
    lw_tabix = sy-tabix.
    IF it_list-zzret = 'S'.
      it_list-flag = it_list-zzret.
      MODIFY it_list INDEX lw_tabix.
    ELSE.
      it_list-zzret = 'E'.
      it_list-flag  = it_list-zzret.
      MODIFY it_list INDEX lw_tabix.
      write :/ 'Program log :-'.
      write :/ 'RFC CALL to MES System FAILED'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CALL_RFC
