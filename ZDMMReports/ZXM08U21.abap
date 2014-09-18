*&---------------------------------------------------------------------*
*&  Include           ZXM08U21
*&---------------------------------------------------------------------*
* Check  INVOICE document exists for the GR document/item**
*-----------------------------------------------------------------------

DATA: l_lfbnr LIKE rseg-lfbnr.

SELECT SINGLE lfbnr INTO l_lfbnr
   FROM rseg
   WHERE lfbnr = i_frseg-lfbnr
     AND lfgja = i_frseg-lfgja
     AND lfPos  = i_frseg-lfpos.
IF sy-subrc = 0.
****Check the GR document has been cancelled******
  SELECT SINGLE lfbnr INTO l_lfbnr
  FROM mseg
  WHERE smbln = i_frseg-lfbnr
    AND sjahr = i_frseg-lfgja
    AND smblp = i_frseg-lfpos
    AND bwart = '102'.

  IF sy-subrc <> 0.
     MESSAGE e004(zmm) WITH
     'There is the invoice exist for this GR/Return'.
  ENDIF.
ENDIF.
