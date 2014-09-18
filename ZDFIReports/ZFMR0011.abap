*&---------------------------------------------------------------------*
*& Program ID     : ZFMR0011                                           *
*& Program Name   : [FM] Status of Op.Budget Availability              *
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
REPORT  ZFMR0011    MESSAGE-ID zmfi.

DATA : l_fund TYPE bp_geber VALUE '',
       l_fictr TYPE fistl VALUE '',
       l_fipex TYPE fm_fipex VALUE ''.

SET PARAMETER ID 'FIC' FIELD l_fund.
SET PARAMETER ID 'FIS' FIELD l_fictr.
SET PARAMETER ID 'FPS' FIELD l_fipex.

CALL FUNCTION 'RKD_REPORT_START'
  EXPORTING
    applclass = 'FM'
    function  = 'EXEC'
    repid     = 'ZFMR001'
    smodus    = 'N'
    subclass  = '01'
    tabname   = 'IFMEISA'.
