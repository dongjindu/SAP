*&---------------------------------------------------------------------*
*&  Include           ZXM08U27
*&---------------------------------------------------------------------*
IF sy-tcode = 'MRNB' OR sy-repid = 'RMMR1MRB'
    OR sy-repid = 'SAPLXM08'.
  es_rbkpv_change = is_rbkpv.
  es_rbkpv_change-sgtxt = 'Revaluation'.
  ef_change = 'X'.
ENDIF.
