************************************************************************
* Program Name      : ZACO44M_PBOM
* Author            : Hyung Jin Youn
* Creation Date     : 2003.11.20.
* Specifications By : Hae Sung Cho
* Pattern           : Report 1-1
* Development Request No : UD1K904320
* Addl Documentation:
* Description       : Check Business Plan BOMs and the Routings.
*                     then Store the results in a CBO table
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZACO44M_PBOM  MESSAGE-ID ZMCO.

* For TOP include
INCLUDE ZACO44L_1TOP.
* For Sub-routine
INCLUDE ZACO44L_F001.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Check Input.
  PERFORM CHECK_INPUT.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  CASE 'X'.
** New Run
    WHEN P_NEW.
* Message.
      PERFORM SHOW_MESG_OF_REP.
* Read data from Pvt. table with the version '0'.
      PERFORM READ_DATA_FR_MKAL.
* Enqueue
      PERFORM ENQUEUE_ZTCO_EBUSPLANBOM.
* Delete data in table 'ZTCO_EBUSPLANBOM'
      PERFORM DELETE_DATA_FR_TABLE.
* Check The validation of Routing (Pvt. '0')
      PERFORM CHECK_VALID_ROUTING.
* Check The validation of BOM (Pvt. '0')
      PERFORM CHECK_VALID_BOM.

**// Mod. By Hyung Jin Youn 2004.02.02
* There is another material field for Routing errors
* -> IT_ZTCO_EBUSPLANBOM
      PERFORM COPY_MAT_AS_ERROR_TYPE.
**// End of Mod.

* Update/Insert
      PERFORM UPDATE_INSERT_TABLE.
* CALL Display/Modification Screen
      PERFORM CALL_MOD_DIS_SCR.

** Edit existing records
    WHEN P_UP.
* Enqueue
      PERFORM ENQUEUE_ZTCO_EBUSPLANBOM.
* Read DATA from ZTCO_EBUSPLANBOM
      PERFORM READ_DATA_FR_BOMTABLE.
* CALL Display/Modification Screen
      PERFORM CALL_MOD_DIS_SCR.
  ENDCASE.

* Let System Dequeue Locks.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

*
