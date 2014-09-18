*----------------------------------------------------------------------*
*   INCLUDE ZXCATU04                                                   *
*----------------------------------------------------------------------*

**VALUE(DYNNR) LIKE  SY-DYNNR
*"             VALUE(TCATS) LIKE  TCATS STRUCTURE  TCATS
*"             VALUE(MODE) LIKE  TC10-TRTYP
*"             VALUE(PERNO) LIKE  CATSDB-PERNR OPTIONAL
*"       TABLES
*"              T_CUAFC STRUCTURE  CUAFCODE

*----------------------------------------------------------------------*
* Created by Haseeb, Hassan with guidence of Manjunath Venkat.
* In this Include we are trying to suppress the buttons/menu items
* '+CU6' , '+CU7', '+CU3', '+CU4', '+CU5' in runtime, as these are
* included in CATS0011 menu exit by default. We are using CATS0011
* user exit to display "Quota Overview" report from CAT2 as per user
* request.
*----------------------------------------------------------------------*
* This code is used to suppress any SAP standard unwanted buttons
* or menu items in CAT2 transaction.
*----------------------------------------------------------------------*

CASE SY-DYNNR.
     WHEN  1000.
       T_CUAFC-FCODE = '+CU6'.
       APPEND T_CUAFC.
       T_CUAFC-FCODE = '+CU7'.
       APPEND T_CUAFC.

     WHEN  2002 OR 2003.
       T_CUAFC-FCODE = '+CU3'.
       APPEND T_CUAFC.
       T_CUAFC-FCODE = '+CU4'.
       APPEND T_CUAFC.
       T_CUAFC-FCODE = '+CU5'.
       APPEND T_CUAFC.

ENDCASE.
