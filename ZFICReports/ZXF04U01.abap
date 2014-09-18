*----------------------------------------------------------------------*
*   INCLUDE ZXF04U01                                                   *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(I_KNA1) LIKE  KNA1 STRUCTURE  KNA1
*"             VALUE(I_KNB1) LIKE  KNB1 STRUCTURE  KNB1 OPTIONAL
*"             VALUE(I_KNVV) LIKE  KNVV STRUCTURE  KNVV OPTIONAL
*"             VALUE(I_ADDRHANDLE) LIKE  ADDR1_SEL-ADDRHANDLE
*"                             OPTIONAL
*"       TABLES
*"              T_KNAS STRUCTURE  KNAS OPTIONAL
*"              T_KNAT STRUCTURE  KNAT OPTIONAL
*"              T_KNB5 STRUCTURE  KNB5 OPTIONAL
*"              T_KNBK STRUCTURE  KNBK OPTIONAL
*"              T_KNBW STRUCTURE  KNBW OPTIONAL
*"              T_KNEX STRUCTURE  KNEX OPTIONAL
*"              T_KNVA STRUCTURE  KNVA OPTIONAL
*"              T_KNVD STRUCTURE  KNVD OPTIONAL
*"              T_KNVI STRUCTURE  KNVI OPTIONAL
*"              T_KNVK STRUCTURE  KNVK OPTIONAL
*"              T_KNVL STRUCTURE  KNVL OPTIONAL
*"              T_KNVP STRUCTURE  KNVP OPTIONAL
*"              T_KNZA STRUCTURE  KNZA OPTIONAL
check i_knb1-bukrs = 'H201'.

if i_knb1-FDGRV(1) <> 'A'.
  message w001(zfi) with 'Warning from finance. Check Cash mgmnt group'.
endif.
