************************************************************************
* Program Name      : ZACO08U_VRVA
* Author            : Hyung Jin Youn
* Creation Date     : 19/07/2003
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No: D1K901708
* Add documentation :
* Description       : Update Variable Amount and Fixed Amount In
*                     Cost and Activity Inputs
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
* Comments      : Should check data of t-code 'ZACO06_01'-Variable Ratio
*                 Master Maintenance and t-code 'ZACO07_01'-Variable
*                 Ratio Master Maintenance
REPORT ZACO08U_VRVA .

* For TOP include
INCLUDE ZACO08L_1TOP.
* For Sub-Rutine
INCLUDE ZACO08L_F001.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  GV_REPID = SY-REPID.
* Building Field Cat.
  PERFORM FIELDCAT_INIT .


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM CHK_INPUT_VALUE.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Calculating Period Count
  PERFORM CAL_PER_COUNT.
* Read Hierarchy From Object ID, Read CCtr from CCgrp 'HMMA1'
  PERFORM READ_HI_FR_SETID(SAPLZGCO_GLOBAL_FORM)
                            TABLES IT_NODES
                                   IT_VALUES
                            USING  P_KOKRS
                                   '0101'
                                   GV_CCGR_SETID.
* Read Variable Master.
  PERFORM READ_VARIABLE_MASTER.
* Retreival DATA from COSP
  PERFORM READ_DATA_FROM_COSP.
* Building POST data (also making report ITAB)
  PERFORM BUILD_POST_DATA.
* Preparing LIST
  PERFORM PRE_LIST_DATA.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.

*
