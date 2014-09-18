*&---------------------------------------------------------------------*
*& Program ID     : ZFMR0030                                           *
*& Program Name   : [FM] Operating Budget vs. Actual Monthly Report    *
*& Created by     : YN.Kim                                             *
*& Created on     : 08/22/2011                                         *
*& Reference Pgm  :                                                    *
*&                                                                     *
*& Modification Log                                                    *
*----------------------------------------------------------------------*
* DATE      |  NAME          |Transport | Issue #  |      DESC         *
*----------------------------------------------------------------------*
*                                                                      *
*&=====================================================================*
REPORT  ZFMR0030 MESSAGE-ID ZMFI.

INCLUDE ZFMR0030TOP.
INCLUDE ZFMR0030CLS.
INCLUDE ZFMR0030F01.

INITIALIZATION.
  CONCATENATE 'HMMA_' SY-DATUM(4) INTO P_GRPID.
  GET PARAMETER ID 'FIK' FIELD P_FIKRS.

  IF P_FIKRS IS INITIAL.
    P_FIKRS = 'H201'.
  ENDIF.

TOP-OF-PAGE.

  PERFORM WRITE_TITLE.

START-OF-SELECTION.
  PERFORM USER_AUTH_CHECK_GEBER TABLES S_FICTR[]
                                 USING P_FIKRS.

  CHECK G_AUTH_CHECK IS INITIAL.

  PERFORM GET_COMMITMENT_ITEM_GROUP.
  PERFORM GET_BUDGET.
  PERFORM MAKE_ITAB.
  PERFORM MAKE_TREE_DATA.

  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND INPUT.

  CASE SY-UCOMM.
    WHEN 'EXPAND'.
      PERFORM EXPAND_NODE.

    WHEN 'COLLAPSE'.
      PERFORM COLLAPSE_NODE.

    WHEN 'DOWN'.
      PERFORM DATA_DOWN.

    WHEN 'PRINT'.
      PERFORM DATA_PRINT.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND  INPUT
