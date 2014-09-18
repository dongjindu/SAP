*----------------------------------------------------------------------*
*   INCLUDE ZXCATU12                                                   *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Author            : Haseeb Mohammad & Hassan Siddiqui.
* Creation Date     : 01/20/2006
* Specifications By : Ahmar Khan
* Development Request No :UD1K918980
* Description       : This is a Menu Exit, User wants a Quota Overview
*                      To be displayed for each employee in CAT2.
*
*&   Date        User      Transport            Description
*  06/30/2006   Hassan    UD1K919414    Created new logic. When an
*                                       Employee gets selected on CAT2
*                                       screen, it also gets populated
*                                       on PT50 screen.
*& 07/01/2006   Hassan    UD1K921273    Created new logic. When multiple
*                                       employees on the CAT2 screen are
*                                       selected, most recent selected
*                                       EE# gets populated on PT50 scrn.
*
*&--------------------------------------------------------------------&*

DATA WA_SAP_PERNRLIST TYPE  PERNR_LIST_structure.
DATA WA_SAP_CATSD TYPE CATSD_EXT.
CASE SAP_FCODE.
  WHEN '+CU2'.
*    LOOP AT SAP_PERNRLIST into WA_SAP_PERNRLIST WHERE MARK EQ 'X'.
    LOOP AT SAP_CATSD INTO WA_SAP_CATSD where mark eq 'X'.
*      SET PARAMETER ID 'PER' FIELD WA_SAP_pernrlist-PERNR.
       SET PARAMETER ID 'PER' FIELD WA_SAP_catsd-PERNR.
      EXIT.
    ENDLOOP.
*     SET PARAMETER ID 'PER' FIELD SAP_CATSFIELDS-PERNR.
    CALL TRANSACTION 'PT50' AND SKIP FIRST SCREEN.
ENDCASE.
