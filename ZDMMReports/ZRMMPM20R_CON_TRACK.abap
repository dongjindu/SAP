************************************************************************
* Program Name      : ZRMMPM20R_CON_TRACK
* Author            : Min-Su Park
* Creation Date     : 2003. 11. 19.
* Specifications By : Min-Su Park
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : Container Tracking
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.19.     Min-su Park      UD1K901873     Initial Coding
* 08.13.2014      Victor     T-code has been deleted for APM
************************************************************************

REPORT zrmmpm20r_con_track NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

*---
INCLUDE : zrmmpmxxr_incl.

**--- Internal Tables
*[1]Tracking basic
DATA : BEGIN OF it_track OCCURS 0,
         vsart LIKE likp-vsart,      "Shipping Type
         bezei LIKE t173t-bezei,     "Shipping type description
         vgbel LIKE lips-vgbel,      "Order No.
         traid LIKE likp-traid,      "Container No.
         zfblno LIKE ztbl-zfblno,    "BL NO
         lfimg LIKE lips-lfimg,      "Case QTY.
         meins LIKE lips-meins,
         zfetd LIKE ztbl-zfetd,      "ETD Pusan
         zfeta LIKE ztbl-zfeta,      "ETA Mobile
         zfreta LIKE ztbl-zfreta,    "Arrival Date
         zfedt LIKE ztidsus-zfedt,   "C/CLEAR
         pass_date LIKE ztmm_container-pass_date, "Arrival
         lgpla LIKE ztmm_container-lgpla, "Location
         bdatu LIKE lagp-bdatu,      "Unpack Date
         leave_date LIKE ztmm_container-leave_date, "Retrun date
         hd_d(05),                   "Holding Days
         hd_i TYPE i,
         lgtyp LIKE ztmm_container-lgtyp,
         lgber LIKE ztmm_container-lgber,
        END OF it_track.

*[2]Tracking Containers by Case No.
DATA : BEGIN OF it_track02 OCCURS 0,
         kdmat LIKE lips-kdmat , "CASE NO
         matnr LIKE lips-matnr , "PART NO
         maktx LIKE makt-maktx , "PART NAME
*        LFIMG LIKE LIPS-LFIMG , "BOX QTY ?
         lfimg LIKE lips-lfimg ,  "TOTAL PCS
         meins LIKE lips-meins ,
         lgpla LIKE lein-lgpla , "Storage bin
         plpos LIKE lein-plpos , "Storage bin Position
         locat(13)             , "Bin + Position
         pass_date LIKE ztmm_container-pass_date , "Receiving date
       END OF it_track02.

*[3]Tracking Containers by Part No.
DATA : BEGIN OF it_track03 OCCURS 0,
         box   TYPE char1,
         vgbel LIKE lips-vgbel,      "Order No.
         traid LIKE likp-traid,      "Container No.
         kdmat LIKE lips-kdmat,      "CASE NO
         lfimg LIKE lips-lfimg,      "Case QTY.
         meins LIKE lips-meins,
         zfetd LIKE ztbl-zfetd,      "ETD Pusan
         zfeta LIKE ztbl-zfeta,      "ETA Mobile
         zfedt LIKE ztidsus-zfedt,   "C/CLEAR
         pass_date LIKE ztmm_container-pass_date, "Arrival
         lgpla LIKE ztmm_container-lgpla, "Location
       END OF it_track03.

**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
*  w_fieldcat-do_sum     = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  w_fieldcat-no_out     = &9.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_fieldcat02.
  &1 = &1 + 1.
  w_fieldcat02-col_pos    = &1.
  w_fieldcat02-fieldname  = &2.
  w_fieldcat02-outputlen  = &3.
  w_fieldcat02-seltext_l  = &4.
  w_fieldcat02-seltext_m  = &4.
  w_fieldcat02-seltext_s  = &4.
  w_fieldcat02-datatype   = &5.
  w_fieldcat02-key        = &6.
*  w_fieldcat02-do_sum     = &6.
  w_fieldcat02-qfieldname = &7.
  w_fieldcat02-cfieldname = &8.
  w_fieldcat02-no_out     = &9.
  append w_fieldcat02.
  clear : w_fieldcat02.
END-OF-DEFINITION.

DEFINE append_fieldcat03.
  &1 = &1 + 1.
  w_fieldcat03-col_pos    = &1.
  w_fieldcat03-fieldname  = &2.
  w_fieldcat03-outputlen  = &3.
  w_fieldcat03-seltext_l  = &4.
  w_fieldcat03-seltext_m  = &4.
  w_fieldcat03-seltext_s  = &4.
  w_fieldcat03-datatype   = &5.
  w_fieldcat03-key        = &6.
*  w_fieldcat03-do_sum     = &6.
  w_fieldcat03-qfieldname = &7.
  w_fieldcat03-cfieldname = &8.
  w_fieldcat03-no_out     = &9.
  append w_fieldcat03.
  clear : w_fieldcat03.
END-OF-DEFINITION.

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_vgbel FOR lips-vgbel,
                 s_matnr FOR lips-matnr,
                 s_traid FOR likp-traid,
                 s_kdmat FOR lips-kdmat.
PARAMETERS : p_date LIKE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK block1.

*---
AT SELECTION-SCREEN.


**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

**---
TOP-OF-PAGE.
  PERFORM top_of_page.
  PERFORM top_of_page02.
  PERFORM top_of_page03.

**---
START-OF-SELECTION.
  PERFORM get_data.

**---
END-OF-SELECTION.
  IF it_track[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
  ENDIF.


**---




*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : it_track, it_track[].

  SELECT lk~vsart        "Shipping Type
         lp~vgbel        "Order No.
         lk~traid        "Container No.
         zt~zfblno       "BL NO
         lp~lfimg        "CASE QTY
         lp~meins
         zt~zfetd        "ETD Pusan
         zt~zfeta        "ETA Mobile
         zt~zfreta       "Arrival Date
         zti~zfedt       "C/CLEAR
         cont~pass_date   "Arrival
         cont~lgpla      "Location
         cont~bdatu      "Unpack Date
         cont~leave_date   "Retrun date
         cont~lgpla
         cont~lgtyp
         cont~lgber
                   INTO CORRESPONDING FIELDS OF TABLE it_track
                   FROM lips AS lp INNER JOIN likp AS lk
                     ON lp~mandt EQ lk~mandt
                    AND lp~vbeln EQ lk~vbeln
                        INNER JOIN ztmm_container AS cont
                           ON lk~mandt EQ cont~mandt
                          AND lk~traid EQ cont~cont_reg_numb1
                              LEFT OUTER JOIN ztbl AS zt
                                ON lk~mandt EQ zt~mandt
                               AND lk~bolnr EQ zt~zfhblno
                                   LEFT OUTER JOIN ztidsus AS zti
                                     ON lk~mandt EQ zti~mandt
                                    AND lk~bolnr EQ zti~zfhblno
                  WHERE lp~vgbel IN s_vgbel
                    AND lp~matnr IN s_matnr
                    AND lk~traid IN s_traid
                    AND lp~kdmat IN s_kdmat
                    AND lk~traty = '0005'.

*---
  LOOP AT it_track.
    SELECT SINGLE bezei INTO it_track-bezei
                        FROM t173t
                       WHERE vsart EQ it_track-vsart
                         AND spras EQ sy-langu.

    CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
         EXPORTING
              begda = p_date
              endda = it_track-pass_date
         IMPORTING
              days  = it_track-hd_i.
*--- Get holding days
    it_track-hd_d = it_track-hd_i.
*--- Change Container No. from BL no. when shipping type is air.
    IF it_track-vsart = '05'.
      it_track-traid = it_track-zfblno.
    ENDIF.
*--- Get Unpack date
    SELECT SINGLE bdatu INTO it_track-bdatu
                        FROM lagp
                       WHERE lgnum = 'P01'
                         AND lgtyp = it_track-lgtyp
                         AND lgpla = it_track-lgpla.
    MODIFY it_track.
  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
**---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.

  append_top :
      'S' text-003 s_vgbel-low s_vgbel-high,
      'S' text-004 s_matnr-low s_matnr-high,
      'S' text-005 s_traid-low s_traid-high,
      'S' text-006 s_kdmat-low s_kdmat-high.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
**---
  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  MOVE : 'X' TO w_layout-colwidth_optimize.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = w_program
            is_layout               = w_layout
            it_fieldcat             = w_fieldcat[]
            it_events               = w_eventcat[]
            it_sort                 = w_sortcat[]
            i_callback_user_command = 'USER_COMMAND'
            i_save                  = 'A'
       TABLES
            t_outtab                = it_track
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : cur field        &9 : no out
  append_fieldcat :
   w_col_pos 'BEZEI' 20 'Shipping Type'  'CHAR' 'X'  ''      '' '',
   w_col_pos 'VGBEL' 10 'PO Number'      'CHAR' 'X'  ''      '' '',
   w_col_pos 'TRAID' 20 'Container No'   'CHAR' 'X'  ''      '' '',
   w_col_pos 'LFIMG' 12 'Case Qty'       'QUAN' ''   'MEINS' '' '',
   w_col_pos 'ZFETD' 10 'ETD Busan'      'DATS' ''   ''      '' '',
   w_col_pos 'ZFETA' 10 'ETD Mobile'     'DATS' ''   ''      '' '',
   w_col_pos 'ZFRETA' 10 'Arrival'        'DATS' ''   ''      '' '',
   w_col_pos 'ZFETD' 10 'C/Clear'        'DATS' ''   ''      '' '',
   w_col_pos 'PASS_DATE' 10 'HMMA CY Arriv'  'DATS' ''   ''      '' '',
   w_col_pos 'LGPLA' 10 'HMMA CY Loc'    'CHAR' ''   ''      '' '',
   w_col_pos 'BDATU' 10 'Unpack Date'    'DATS' ''   ''      '' '',
   w_col_pos 'LEAVE_DATE' 10 'Return Date'    'DATS' ''   ''      '' '',
   w_col_pos 'HD_D'  10 'Holding Days'   'INT1' ''   ''      '' ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.

ENDFORM.                    " build_sortcat

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm    LIKE sy-ucomm
                        selfield TYPE slis_selfield.
*---
  READ TABLE it_track INDEX selfield-tabindex.

  CHECK sy-subrc = 0.

  CHECK ucomm = '&IC1'.

  PERFORM get_track02.
  PERFORM alv_build02.
ENDFORM.     " user_command

*&---------------------------------------------------------------------*
*&      Form  get_track02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_track02.
*---
  CLEAR : it_track02, it_track02[].

  SELECT lp~kdmat        "CASE NO
         lp~matnr        "PART NO
         mt~maktx        "PART NAME
         lp~lfimg        "TOTAL PCS
         lp~meins
         cont~lgpla      "Storage bin
         pass_date       "Receiving date
*          LFIMG LIKE LIPS-LFIMG , "BOX QTY ?
*          PLPOS   "Storage bin Position
*          LOCAT(13)             , "Bin + Position
                  INTO CORRESPONDING FIELDS OF TABLE it_track02
                  FROM lips AS lp INNER JOIN likp AS lk
                    ON lp~mandt EQ lk~mandt
                   AND lp~vbeln = lk~vbeln
                       INNER JOIN ztmm_container AS cont
                          ON lk~mandt EQ cont~mandt
                         AND lk~traid = cont~cont_reg_numb1
                             INNER JOIN makt AS mt
                                ON lp~mandt EQ mt~mandt
                               AND lp~matnr = mt~matnr
                 WHERE lk~traid = it_track-traid
                   AND lk~traty = '0005'
                   AND mt~spras EQ sy-langu.

*---
  LOOP AT it_track02.
*-- Get bin position
    SELECT SINGLE plpos INTO it_track02-plpos
                        FROM lein
                       WHERE lenum = it_track02-kdmat.
*-- Get CONT. LOCATION
    CONCATENATE it_track02-lgpla '/' it_track02-plpos
                INTO it_track02-locat.
    MODIFY it_track02.
  ENDLOOP.
ENDFORM.                    " get_track02

*&---------------------------------------------------------------------*
*&      Form  alv_build02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_build02.
*---
  PERFORM event_build02 USING w_eventcat02[].
  PERFORM comment_build02.
  PERFORM build_fieldcat02.
  PERFORM make_alv_grid02.
ENDFORM.                    " alv_build02

*&---------------------------------------------------------------------*
*&      Form  comment_build02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build02.
**---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-007.
  APPEND w_line TO w_top_of_page02.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page02.

  CLEAR : w_line.
  w_line-typ  = 'S'.
  w_line-key  = text-008.
  w_line-info = it_track-traid.
  APPEND w_line TO w_top_of_page02.
ENDFORM.                    " comment_build02

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat02.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : cur field        &9 : no out
  append_fieldcat02 :
   w_col_pos 'KDMAT' 20 'Case Number'    'CHAR' 'X'  ''      '' '',
   w_col_pos 'MATNR' 18 'Part Number'    'CHAR' ''   ''      '' '',
   w_col_pos 'MAKTX' 30 'Part Name'      'CHAR' ''   ''      '' '',
   w_col_pos 'LFIMG' 12 'Total PCS'      'QUAN' ''   'MEINS' '' '',
   w_col_pos 'LOCAT' 13 'Cont Location'  'CHAR' ''   ''      '' '',
   w_col_pos 'PASS_DATE' 10 'Receiving Date' 'DATS' ''   ''      '' ''.
ENDFORM.                    " build_fieldcat02

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid02.
*---
  CLEAR : w_program02.

  MOVE : sy-repid TO w_program02.

  MOVE : 'X' TO w_layout02-colwidth_optimize.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = w_program02
            is_layout               = w_layout02
            it_fieldcat             = w_fieldcat02[]
            it_events               = w_eventcat02[]
            it_sort                 = w_sortcat02[]
            i_callback_user_command = 'USER_COMMAND01'
            i_save                  = 'A'
       TABLES
            t_outtab                = it_track02
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
ENDFORM.                    " make_alv_grid02

*---------------------------------------------------------------------*
*       FORM event_build02                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_W_EVENTCAT                                                  *
*---------------------------------------------------------------------*
FORM event_build02 USING    p_w_eventcat TYPE slis_t_event.
**---
  DATA : l_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = p_w_eventcat.

  READ TABLE p_w_eventcat WITH KEY name = slis_ev_top_of_page
                          INTO l_event.

  IF sy-subrc EQ 0.
    MOVE c_formname_top_of_page02 TO l_event-form.
    APPEND l_event TO p_w_eventcat.
  ENDIF.
ENDFORM.                    " event_build

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page02.
**---
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = w_top_of_page02.
ENDFORM.                    " top_of_page

*&---------------------------------------------------------------------*
*&      Form  top_of_page03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page03.
**---
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = w_top_of_page03.
ENDFORM.                    " top_of_page03

*---------------------------------------------------------------------*
*       FORM user_command01                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command01 USING ucomm    LIKE sy-ucomm
                          selfield TYPE slis_selfield.
*---
  READ TABLE it_track02 INDEX selfield-tabindex.

  CHECK sy-subrc = 0.

  CHECK ucomm = '&IC1'.

  PERFORM get_track03.
  PERFORM alv_build03.
ENDFORM. " USER_COMMAND01

*&---------------------------------------------------------------------*
*&      Form  get_track03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_track03.
*---
  CLEAR : it_track03, it_track03[].

  SELECT lp~vgbel   "Order No.
         lk~traid   "Container No.
         lp~kdmat   "CASE NO
         lp~lfimg   "CASE QTY
         lp~meins
         zt~zfetd   "ETD Pusan
         zt~zfeta   "ETA Mobile
         zti~zfedt   "C/CLEAR
         cont~pass_date   "Arrival
         cont~lgpla   "Location
                    INTO CORRESPONDING FIELDS OF TABLE it_track03
                    FROM lips AS lp INNER JOIN likp AS lk
                      ON lp~mandt EQ lk~mandt
                     AND lp~vbeln = lk~vbeln
                         INNER JOIN ztmm_container AS cont
                            ON lk~mandt EQ cont~mandt
                           AND lk~traid = cont~cont_reg_numb1
                               LEFT OUTER JOIN ztbl AS zt
                                 ON lk~mandt EQ zt~mandt
                                AND lk~bolnr = zt~zfhblno
                                    LEFT OUTER JOIN ztidsus AS zti
                                      ON lk~bolnr EQ zti~mandt
                                     AND lk~bolnr = zti~zfhblno
                   WHERE lp~matnr = it_track02-matnr
                     AND lk~traty = '0005'.
ENDFORM.                    " get_track03

*&---------------------------------------------------------------------*
*&      Form  alv_build03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_build03.
*---
  PERFORM event_build03 USING w_eventcat03[].
  PERFORM comment_build03.
  PERFORM build_fieldcat03.
  PERFORM make_alv_grid03.
ENDFORM.                    " alv_build03

*&---------------------------------------------------------------------*
*&      Form  event_build03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_EVENTCAT03[]  text
*----------------------------------------------------------------------*
FORM event_build03 USING    p_w_eventcat TYPE slis_t_event.
**---
  DATA : l_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = p_w_eventcat.

  READ TABLE p_w_eventcat WITH KEY name = slis_ev_top_of_page
                          INTO l_event.

  IF sy-subrc EQ 0.
    MOVE c_formname_top_of_page03 TO l_event-form.
    APPEND l_event TO p_w_eventcat.
  ENDIF.
ENDFORM.                    " event_build03

*&---------------------------------------------------------------------*
*&      Form  comment_build03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build03.
**---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-009.
  APPEND w_line TO w_top_of_page03.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page03.

  CLEAR : w_line.
  w_line-typ  = 'S'.
  w_line-key  = text-010.
  w_line-info = it_track02-matnr.
  APPEND w_line TO w_top_of_page03.
ENDFORM.                    " comment_build03

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat03.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : cur field        &9 : no out
  append_fieldcat03 :
   w_col_pos 'VGBEL' 10 'Order No'       'CHAR' 'X'  ''      '' '',
   w_col_pos 'TRAID' 20 'Container Number' 'CHAR' 'X'  ''      '' '',
   w_col_pos 'KDMAT' 20 'Case Number'      'CHAR' ''   ''      '' '',
   w_col_pos 'LFIMG' 12 'Case Quantity'    'QUAN' ''   'MEINS' '' '',
   w_col_pos 'ZFETD' 10 'ETD Busan'        'DATS' ''   ''      '' '',
   w_col_pos 'ZFETA' 10 'ETD Mobile'       'DATS' ''   ''      '' '',
   w_col_pos 'ZFEDT' 10 'C/Clear'          'DATS' ''   ''      '' '',
   w_col_pos 'PASS_DATE' 10 'HMMA CY Arrival' 'DATS' ''   ''      '' '',
   w_col_pos 'LGPLA' 10 'HMMA CY Location' 'CHAR' ''   ''      '' ''.
ENDFORM.                    " build_fieldcat03

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid03.
*---
  CLEAR : w_program03.

  MOVE : sy-repid TO w_program03.

  MOVE : 'X' TO w_layout03-colwidth_optimize.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = w_program03
            is_layout               = w_layout03
            it_fieldcat             = w_fieldcat03[]
            it_events               = w_eventcat03[]
            it_sort                 = w_sortcat03[]
*            i_callback_user_command = 'USER_COMMAND02'
            i_save                  = 'A'
       TABLES
            t_outtab                = it_track03
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
ENDFORM.                    " make_alv_grid03
