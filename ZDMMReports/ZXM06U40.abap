*----------------------------------------------------------------------*
*   INCLUDE ZXM06U40                                                   *
*----------------------------------------------------------------------*
*
* move stored global values back to the returning structure
*
e_ci_ekpo        = gl_ekpo_ci.

*
* in case the Button was clicked the value of ekpo_ci-ZZINFORECCRE
* will be taken from the copied PO
*

IF i_ucomm = 'ZZDETERMINE'.

  DATA: ls_ref_ekpo LIKE LINE OF gt_ref_ekpo_tab.

*
* gt_ref_ekpo_tab contains all positions of the copied PO
*

  READ TABLE gt_ref_ekpo_tab INTO ls_ref_ekpo
      WITH KEY ebelp = gl_ekpo-ebelp.

  IF sy-subrc IS INITIAL.

    ekpo_ci-zzinforeccre = ls_ref_ekpo-ekpo-zzinforeccre.

  ENDIF.

ELSE.

*
* return the function code of the Screen. This code will be handled
* in the SAP program. It can only be handled if you use SAP function
* codes
*

  e_ucomm = i_ucomm.
ENDIF.

*
* ekko_ci contains the actual values of the Dynpro fields
* e_ci_update is only set if you really want the fields
* on the Dynpro to be saved. You must set it then to 'X'
* the field ekko_ci-ZZINFORECCRE will here only be saved if there
* was a change and the transaction is not in display mode
*

IF gl_ekpo_ci-zzinforeccre NE ekpo_ci-zzinforeccre.
  e_ci_ekpo-zzinforeccre = ekpo_ci-zzinforeccre.
  IF gl_aktyp NE 'A'.
    e_ci_update = 'X'.
  ENDIF.
ENDIF.
