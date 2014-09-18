REPORT zcommchgh NO STANDARD PAGE HEADING
                LINE-SIZE 195 LINE-COUNT 60.

* Change doc listing
* Grouped into 3 chg types: 1. Part revision  2. Price change  3. Others

TABLES:
        cdhdr, cdpos, mara, makt, mard.
DATA: BEGIN OF chgdoc OCCURS 50.
        INCLUDE STRUCTURE cdred.
DATA: END OF chgdoc.

DATA:
      chgtype(1),
      plant(4),
      matnr1 LIKE chgdoc-objectid.

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
      gs_prnt     TYPE slis_print_alv.

*---- ALV

DATA: BEGIN OF itab OCCURS 0,
        matnr1    LIKE chgdoc-objectid,
        objectid  LIKE chgdoc-objectid,
        maktx     LIKE makt-maktx,
        chgtype(1),
        chgtypet(15),
        plant(4),
        changenr  LIKE  chgdoc-changenr,
        tcode     LIKE  chgdoc-tcode,
        username  LIKE  chgdoc-username,
        udate     LIKE  chgdoc-udate,
        ftext     LIKE  chgdoc-ftext,
        f_old     LIKE  chgdoc-f_old,
        f_new     LIKE  chgdoc-f_new,
      END OF itab.


FIELD-GROUPS: header.





SELECT-OPTIONS:
    xmatnr  FOR cdhdr-objectid,    "Material
    xudate  FOR cdhdr-udate,       "Change Date
    xuname  FOR cdhdr-username,    "User Name
    xtcode  FOR cdhdr-tcode,       "Transaction Code
    xwerks  FOR mard-werks.        "Plants

SELECTION-SCREEN SKIP.

*Filter change type
SELECTION-SCREEN BEGIN OF BLOCK chg0 WITH FRAME TITLE text-001.
PARAMETERS : xchg1 AS CHECKBOX DEFAULT 'X',
             xchg2 AS CHECKBOX DEFAULT 'X',
             xchg3 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK chg0.



START-OF-SELECTION.

  INSERT:
          chgdoc-objectid        "Material
          chgtype                "Change type
          plant
          chgdoc-changenr
          chgdoc-username
          chgdoc-udate
          chgdoc-tcode
          chgdoc-tabname
          chgdoc-tabkey
          chgdoc-chngind
          chgdoc-fname
          chgdoc-ftext
          chgdoc-textart
          chgdoc-outlen
          chgdoc-f_old
          chgdoc-f_new

  INTO header.

  SELECT * FROM mara WHERE matnr IN xmatnr.

    matnr1 = mara-matnr.
    CALL FUNCTION 'CHANGEDOCUMENT_READ'
  EXPORTING
*         ARCHIVE_HANDLE             = 0
*         CHANGENUMBER               = ' '
*         DATE_OF_CHANGE             = '00000000'
       objectclass                = 'MATERIAL'
       objectid                   = matnr1
*         TABLEKEY                   = ' '
*         TABLENAME                  = ' '
*         TIME_OF_CHANGE             = '000000'
*         USERNAME                   = ' '
*         LOCAL_TIME                 = ' '
  TABLES
       editpos                    = chgdoc
  EXCEPTIONS
       no_position_found          = 1
       wrong_access_to_archive    = 2
       time_zone_conversion_error = 3
       OTHERS                     = 4.

    LOOP AT chgdoc.

      CHECK:  chgdoc-udate    IN xudate,
              chgdoc-username IN xuname,
              chgdoc-tcode    IN xtcode.


*     Chg type: 1. Part revision, 2. Price change, 3. Others
      CASE chgdoc-tcode.
        WHEN 'MM01' OR 'MM02' OR 'MM03'.  chgtype = '1'.
        WHEN 'MR21'.  itab-chgtype = '2'.
        WHEN OTHERS.  itab-chgtype = '3'.
      ENDCASE.

*     Filter chg type
      IF ( chgtype = '1' AND xchg1 <> 'X' ) OR
         ( chgtype = '2' AND xchg2 <> 'X' ) OR
         ( chgtype = '3' AND xchg3 <> 'X' ).
        CONTINUE.
      ENDIF.

*     Plant is a substring of tabkey
      plant = chgdoc-tabkey+21(4).

      IF NOT ( xwerks IS INITIAL ) AND NOT ( plant IS INITIAL ).
        CHECK plant IN xwerks.
      ENDIF.

      EXTRACT header.

    ENDLOOP.

  ENDSELECT.


END-OF-SELECTION.

  REFRESH itab.

  SORT.
  LOOP.
    CLEAR itab.

    itab-objectid = chgdoc-objectid.

*  Material
    SELECT SINGLE maktx INTO itab-maktx
           FROM makt  WHERE matnr = chgdoc-objectid.

*  Change type
    CASE chgtype.
      WHEN '1'.   itab-chgtype = 1. itab-chgtypet = 'PARTS REVISION'.
      WHEN '2'.   itab-chgtype = 2. itab-chgtypet = 'PRICE CHANGE'.
      WHEN '3'.   itab-chgtype = 3. itab-chgtypet = 'OTHERS'.
    ENDCASE.

    SHIFT chgdoc-f_old LEFT DELETING LEADING space.
    SHIFT chgdoc-f_new LEFT DELETING LEADING space.

    itab-plant    = plant.
    itab-ftext    = chgdoc-ftext.
    itab-f_old    = chgdoc-f_old.
    itab-f_new    = chgdoc-f_new.
    itab-changenr = chgdoc-changenr.
    itab-tcode    = chgdoc-tcode.
    itab-username = chgdoc-username.
    itab-udate    = chgdoc-udate.

    itab-objectid = chgdoc-objectid.
    APPEND itab.
  ENDLOOP.

* ==> build field category
  PERFORM field_setting TABLES gt_fieldcat USING :
   'OBJECTID'  'Object'         '18' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'MAKTX'     'Material Desc'  '25' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'CHANGENR'  'ChgNo'          '09' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'CHGTYPE'   'C'              '1'  ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'CHGTYPET'  'Chg Type'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'PLANT'     'Plant'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'TCODE'     'Tcd'            '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'USERNAME'  'User'           '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'UDATE'     'Date'           '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'FTEXT'     'Fld'            '25' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'F_OLD'     'Old'            '20' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'F_NEW'     'New'            '20' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.



  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program = 'Z_TEST'
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       TABLES
            t_outtab           = itab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.


*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.


ENDFORM.                    " fill_field_category
