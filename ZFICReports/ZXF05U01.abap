*----------------------------------------------------------------------*
*   INCLUDE ZXF05U01                                                   *
*----------------------------------------------------------------------*

*"       IMPORTING
*"             VALUE(I_LFA1) LIKE  LFA1 STRUCTURE  LFA1
*"             VALUE(I_LFB1) LIKE  LFB1 STRUCTURE  LFB1
*"             VALUE(I_LFM1) LIKE  LFM1 STRUCTURE  LFM1
*"             VALUE(I_ADDRHANDLE) LIKE  ADDR1_SEL-ADDRHANDLE
*"                             OPTIONAL
*"       TABLES
*"              T_LFBK STRUCTURE  LFBK OPTIONAL
*"              T_LFB5 STRUCTURE  LFB5 OPTIONAL
*"              T_LFZA STRUCTURE  LFZA OPTIONAL
*"              T_LFBW STRUCTURE  LFBW OPTIONAL
*"              T_LFAS STRUCTURE  LFAS OPTIONAL
*"              T_LFAT STRUCTURE  LFAT OPTIONAL
*"              T_LFLR STRUCTURE  LFLR OPTIONAL
*"              T_LFM2 STRUCTURE  LFM2 OPTIONAL
*"              T_WYT1 STRUCTURE  WYT1 OPTIONAL
*"              T_WYT1T STRUCTURE  WYT1T OPTIONAL
*"              T_WYT3 STRUCTURE  WYT3 OPTIONAL

check i_lfb1-bukrs = 'H201'.

if i_lfb1-FDGRV(1) <> 'B'.
  message w001(zfi) with 'Please confirm Cash mgmnt group'.
endif.

*if i_lfb1-QSSKZ <> space and i_lfb1-ZUAWA <> 'Z12'.
*  message e001(zfi) with 'Please confirm sort key(Z12) for W/H tax'.
*endif.
