************************************************************************
* Program Name      : SAPMZPP_APP202
* Author            : Bobby
* Creation Date     : 2003.08.26.
* Specifications By : Bobby
* Development Request No : UD1K902029
* Addl Documentation:
* Description       : [PP] Order Management - Order Color
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
*&---------------------------------------------------------------------*
*& Include MZPP_APP202TOP                                              *
*&                                                                     *
*&---------------------------------------------------------------------*


  CLASS LCL_APPLICATION DEFINITION DEFERRED.
  CLASS CL_GUI_CFW DEFINITION LOAD.

* CAUTION: MTREESNODE is the name of the node structure which must
* be defined by the programmer. DO NOT USE MTREESNODE!

  DATA: G_APPLICATION         TYPE REF TO LCL_APPLICATION,
        G_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
        G_TREE                TYPE REF TO CL_GUI_SIMPLE_TREE,
*        G_TREE                TYPE REF TO cl_column_tree_model,
        G_OK_CODE             TYPE SY-UCOMM.

* Fields on Dynpro 100
  DATA: hierarchy_header      LIKE treemhhdr ,
        G_NODE_KEY            TYPE TV_NODEKEY.

  DATA: SV_PROG               LIKE TRDIR-NAME,
        SV_SCNO               LIKE SY-DYNNR,
        SV_CODE               LIKE SY-UCOMM,
        OK_CODE               LIKE SY-UCOMM.
