*&------------------------------------------------------------------
*& Program ID     : ZMMR10000C
*& Program Name   : Interface Monitoring
*& Created by     : Paul
*& Created on     : 10.07.2011
*& Development ID :
*& Reference Pgm. :
*& Description    : Interface Monitoring
*& Modification Log
*&====================================================================
*& Date        Developer      Request ID      Description
*& 10.07.2011  Paul                           first dev.
*&--------------------------------------------------------------------

REPORT  ZMMR10000C MESSAGE-ID zmmm.

INCLUDE ZMMR10000C_TOP.
INCLUDE ZMMR10000C_C01.
INCLUDE ZMMR10000C_I01.
INCLUDE ZMMR10000C_O01.
INCLUDE ZMMR10000C_F01.

START-OF-SELECTION.

END-OF-SELECTION.
  CALL SCREEN 2000.
