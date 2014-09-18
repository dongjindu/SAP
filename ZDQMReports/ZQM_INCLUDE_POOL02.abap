*----------------------------------------------------------------------*
*   INCLUDE ZQM_INCLUDE_POOL02
*
*----------------------------------------------------------------------*
*/// Notification - Constants and etc

*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'

*-- Vehicle/Engine Type Categoty
CONSTANTS : C_VH_ENG_CATEGORY  TYPE ZQKATART_VH  VALUE 'Q'.

*-- Coding Categoty
CONSTANTS : C_CODING_CATEGORY TYPE ZQKATART_VH  VALUE 'D'.

*-- Notification type
CONSTANTS :
  C_QMART_INT_DEF  TYPE QMART VALUE 'Q1', "/Internal defect
  C_QMART_LINE_EXT TYPE QMART VALUE 'Q2', "/Lineside ext. defect
  C_QMART_INSP_EXT TYPE QMART VALUE 'Q3'. "/Insp. ext. defect

*-- Partner Function
CONSTANTS :
  C_PARVW_Author    TYPE PARVW  VALUE 'Z1', "/Author
  C_PARVW_MANAGER   TYPE PARVW  VALUE 'Z2', "/Group Leader/Manager
  C_PARVW_RESP_DEP  TYPE PARVW  VALUE 'Z3', "/Resp. Department
  C_PARVW_CONT_PER  TYPE PARVW  VALUE 'Z4', "/Contact Person
  C_PARVW_VENDOR    TYPE PARVW  VALUE 'Z5'. "/Vendor

*-- Status of Object : JEST
CONSTANTS :
  C_STAT_NOTI_CREATION   TYPE J_STATUS  VALUE 'I0068', "/Noti. Creation
  C_STAT_NOTI_RELEASE    TYPE J_STATUS  VALUE 'I0070', "/Noti. Release
  C_STAT_NOTI_COMPLETION TYPE J_STATUS  VALUE 'I0072', "/Noti. Complete
  C_STAT_NOTI_DELETION   TYPE J_STATUS  VALUE 'I0076', "/Noti. Deletion

  C_STAT_TASK_CREATION   TYPE J_STATUS  VALUE 'I0154', "/Task Creation
  C_STAT_TASK_RELEASE    TYPE J_STATUS  VALUE 'I0155', "/Task Release
  C_STAT_TASK_COMPLETION TYPE J_STATUS  VALUE 'I0156', "/Task completion
  C_STAT_TASK_SUCCESSFUL TYPE J_STATUS  VALUE 'I0157'. "/Task successful

*-- Code group of Notification
Constants :
   C_CAUSE_URGRP  TYPE URGRP VALUE 'Q5%', "/Cause: code group
   C_DEFECT_FEGRP1 TYPE FEGRP VALUE 'Q9NOTI', "/Defect type  code group
   C_DEFECT_FEGRP2 TYPE FEGRP VALUE 'Q9MS', "/Defect type  code group
   C_Def_loc_OTGRP TYPE OTGRP VALUE 'QE'. "/Defect location code group
