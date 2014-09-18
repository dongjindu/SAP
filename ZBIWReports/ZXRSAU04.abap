*----------------------------------------------------------------------*
*   INCLUDE ZXRSAU04                                                   *
*----------------------------------------------------------------------*

  IF I_DATASOURCE = '4R_SAKNR_0000_HIER'
   AND I_S_HIEBAS-HCLASS = '0000'.       "Hierarchy class of FI-SL-Sets

* Add compound value to hierarchy leaves
  LOOP AT C_T_HIENODE WHERE IOBJNM <> '0HIER_NODE'.
*   Shift nodename right to get space for compound characteristic
    SHIFT C_T_HIENODE-NODENAME RIGHT BY 4 PLACES.
*   Put chart of accounts into first 4 places
    C_T_HIENODE-NODENAME(4) = 'HNA1'.
    MODIFY C_T_HIENODE.
  ENDLOOP.

* Add compound value to intervals in the same fashion
  LOOP AT C_T_HIEINTV.
    SHIFT C_T_HIEINTV-LEAFFROM RIGHT BY 4 PLACES.
    C_T_HIEINTV-LEAFFROM(4) = 'HNA1'.      "<-- Your compound value
    SHIFT C_T_HIEINTV-LEAFTO RIGHT BY 4 PLACES.
    C_T_HIEINTV-LEAFTO(4) = 'HNA1'.        "<-- Your compound value
    MODIFY C_T_HIEINTV.
  ENDLOOP.

ENDIF.
