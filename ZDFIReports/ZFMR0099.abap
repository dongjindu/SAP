*&---------------------------------------------------------------------*
*& Program ID     : ZFMR0099                                           *
*& Program Name   : [FM] Status of Budget & Actuals                    *
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
REPORT  ZFMR0099    MESSAGE-ID zmfi.

  CALL FUNCTION 'RKD_REPORT_START'
    EXPORTING
      applclass = 'FM'
      function  = 'EXEC'
      repid     = 'ZFMR099'
*      smodus    = 'N'
      subclass  = '01'
      tabname   = 'IFMEISA'.
