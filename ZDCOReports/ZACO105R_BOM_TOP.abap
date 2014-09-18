*----------------------------------------------------------------------*
*   INCLUDE ZACO105R_BOM_TOP                                           *
*----------------------------------------------------------------------*
TABLES :csks, stpo, STAS.



DATA : BEGIN OF it_stas OCCURS 0,
        stlty like stas-stlty,
        matnr LIKE mast-matnr,
        werks LIKE mast-werks,
        stlnr LIKE stas-stlnr,
        stlal LIKE stas-stlal,
        aennr LIKE stas-aennr,
        stvkn LIKE stas-stvkn,
        stlkn LIKE stas-stlkn,
        stasz LIKE stas-stasz,
        datuv LIKE stas-datuv,    "Valid from
        datub LIKE stas-datuv,    "Valid to
*        aennr LIKE stas-aennr,
        lkenz LIKE stas-lkenz,
        idnrk LIKE stpo-idnrk,
        text(3),
       END OF it_stas.

*DATA : it_stpo LIKE stpo OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_stpo OCCURS 0,
         stlnr      like stpo-stlnr,
         stlkn      like stpo-stlkn,
*         stlal      like stpo-stlal,
         aennr      like stpo-aennr,
         stvkn      like stpo-stvkn,
         stas_aennr like stpo-aennr,
         idnrk      like stpo-idnrk,
         datuv      like stpo-datuv,      "From date
         datub      like stpo-datuv,      "To date
       END OF it_stpo.


*DATA : it_display  LIKE ztcou105 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_display OCCURS 0,
         fmatnr  LIKE ztcou105-fmatnr,
         tmatnr  LIKE ztcou105-tmatnr,
         datuv   LIKE ztcou105-datuv,
         aennr   LIKE ztcou105-aennr ,
         matnr   LIKE ztcou105-matnr,
         stlnr   LIKE stpo-stlnr,
         werks   like mAsT-werks,
         status(1),
         type(1),
       END OF it_display.

RANGES : r_werks FOR mast-werks.
*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.

CONSTANTS : c_status_set   TYPE slis_formname
                                VALUE 'PF_STATUS_SET',
            c_user_command TYPE slis_formname
                               VALUE 'USER_COMMAND',
            c_top_of_page  TYPE slis_formname
                               VALUE 'TOP_OF_PAGE',
            c_top_of_list  TYPE slis_formname
                               VALUE 'TOP_OF_LIST',
            c_end_of_list  TYPE slis_formname
                               VALUE 'END_OF_LIST'.

*---- ALV
