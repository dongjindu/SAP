FUNCTION Z_FI_PAYMEDIUM_ACH_00.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_FPAYH) LIKE  FPAYH STRUCTURE  FPAYH
*"     VALUE(I_FPAYHX) LIKE  FPAYHX STRUCTURE  FPAYHX
*"  EXPORTING
*"     VALUE(E_SRTF1) LIKE  FPAYH-SRTF1
*"----------------------------------------------------------------------
* On change of the sort field, a new batch header has to be created
* (see event 30 in FI_PAYMEDIUM_ACH_30)
** BH6 - Standard Entry Class Code (PPD, CCD, CTX)
*  e_srtf1 = i_fpayhx-formz.
*
** BH7 - Company Entry Description
*  e_srtf1+6 = i_fpayh-paygr.
*

  DATA: BEGIN OF LT_KOSTL OCCURS 0,
          KOSTL  LIKE CSKS-KOSTL,
        END OF LT_KOSTL.
  DATA: L_LINES TYPE I.
  DATA: L_LIFNR LIKE LFA1-LIFNR.

  L_LIFNR = I_FPAYH-GPA1R.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*     EXPORTING
*        INPUT     =  L_LIFNR
*     IMPORTING
*        OUTPUT    =  L_LIFNR.

  SELECT PA0001~KOSTL INTO TABLE LT_KOSTL
   FROM PA0001 INNER JOIN CSKS ON
        PA0001~KOSTL = CSKS~KOSTL
   WHERE PA0001~PERNR = L_LIFNR
     AND PA0001~ENDDA GE I_FPAYH-LAUFD.


  DESCRIBE TABLE LT_KOSTL LINES L_LINES.
  READ TABLE LT_KOSTL INDEX L_LINES.

* Sort Key
  e_srtf1 = LT_KOSTL-KOSTL.
  e_srtf1+10 = I_FPAYH-GPA1R.

ENDFUNCTION.
