*&---------------------------------------------------------------------*
*& Include MZMMO_IF001TOP                                              *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  SAPMZMMO_IF001                .

data begin of it_sub occurs 0.
        include structure ztmm_if002.
data: l_mark type c.
data end of it_sub.

data: st_sub like ztmm_if002.

data: gv_first type c.

data: ok_code type sy-ucomm.

controls tc_list type tableview using screen 100.
