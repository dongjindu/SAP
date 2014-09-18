*----------------------------------------------------------------------*
*   INCLUDE MZPP_APPLICATIONTOP_ALV                                    *
*----------------------------------------------------------------------*
************************************************************************
*  FOR ALV GRID
************************************************************************
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : wa_fname_tx(40),
       wa_saveline_ix     LIKE  sy-index.
*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : alv_grid               TYPE REF TO cl_gui_alv_grid,
       gs_custom_container    TYPE REF TO cl_gui_custom_container,
       wa_container           TYPE scrfname VALUE 'CONTAINER',
       WA_CC_2108             TYPE scrfname VALUE 'CC_2108'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : gs_variant        TYPE disvariant ,  "Display Variant
       gs_layout         TYPE lvc_s_layo ,  "Layout
       gs_print          TYPE lvc_s_prnt ,  "Print control
       gt_special_groups TYPE lvc_t_sgrp ,  "Field groups
       gt_toolbar_excluding TYPE ui_functions , "Exclu Toolbar Std FUNC
       gt_header         TYPE TABLE OF slis_listheader WITH HEADER LINE,
       gt_fieldcat       TYPE lvc_t_fcat ,  "Field Catalog
       gt_sort           TYPE lvc_t_sort ,  "Sort criteria
       gt_filter         TYPE lvc_t_filt .  "Filter criteria

DATA : gt_fieldcat_slis  TYPE slis_t_fieldcat_alv.
DATA : wa_fieldcat       TYPE lvc_s_fcat.
***************************************************
* Display Image
***************************************************
type-pools: cndp.

**********************************
*Define Macro
**********************************
DEFINE fieldcat_compose.
  it_fieldcat-fieldname = &1.
  it_fieldcat-reptext   = &2.
  it_fieldcat-outputlen = 20.
  append it_fieldcat to gt_fieldcat.

END-OF-DEFINITION.

DEFINE set_fieldcat.
  l_fieldcat-reptext   = &1.
  l_fieldcat-scrtext_l = &1.
  l_fieldcat-scrtext_m = &1.
  l_fieldcat-scrtext_s = &1.
  l_fieldcat-colddictxt = 'L'.
  l_fieldcat-outputlen = &2.

END-OF-DEFINITION.
