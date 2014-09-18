REPORT yca_update_sort_string_in_resb .
TABLES: PLAF, resb.

DATA: BEGIN OF it_resb OCCURS 0,
        plnum    LIKE plaf-plnum,
        pwwrk    LIKE plaf-pwwrk,
        matnr    LIKE plaf-matnr,
        auffx    LIKE plaf-auffx,
        rsnum    LIKE resb-rsnum,
        rspos    LIKE resb-rspos,
        rsart    LIKE resb-rsart,
        matnr_r  LIKE   resb-matnr,
        prvbe    LIKE resb-prvbe,
        lgort    LIKE resb-lgort,
        sortf    LIKE resb-sortf,
      END   OF it_resb.

  SELECT-OPTIONS: S_PLNUM FOR PLAF-PLNUM.

START-OF-SELECTION.

  SELECT a~plnum a~pwwrk a~matnr a~auffx b~rsnum
         b~rspos b~rsart b~matnr AS matnr_r b~prvbe
         b~lgort b~sortf
    INTO CORRESPONDING FIELDS OF TABLE it_resb
    FROM plaf AS a INNER JOIN resb AS b
      ON a~rsnum = b~rsnum
   WHERE A~PLNUM IN S_PLNUM
     AND A~AUFFX = 'X'
     AND ( b~sortf EQ ' ' OR
           B~PRVBE EQ ' ' OR
           B~LGORT EQ ' ' ).

  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH 'No data founded'.
  ENDIF.

  SET PF-STATUS 'BASE'.

  LOOP AT it_resb.
    WRITE:/ it_resb-plnum,
            it_resb-pwwrk,
            it_resb-matnr,
            it_resb-auffx,
            it_resb-rsnum,
            it_resb-rspos,
            it_resb-rsart,
            it_resb-matnr_r,
            it_resb-prvbe,
            it_resb-lgort,
            it_resb-sortf.
  ENDLOOP.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'DOWNLOAD'.
      CALL FUNCTION 'DOWNLOAD'
           EXPORTING
                filename                = 'C:\TEMP\RESB.TXT'
                filetype                = 'DAT'
           TABLES
                data_tab                = it_resb
           EXCEPTIONS
                invalid_filesize        = 1
                invalid_table_width     = 2
                invalid_type            = 3
                no_batch                = 4
                unknown_error           = 5
                gui_refuse_filetransfer = 6
                customer_error          = 7
                OTHERS                  = 8.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    WHEN 'UPLOAD'.
      CLEAR: it_resb, it_resb[].
      CALL FUNCTION 'UPLOAD'
           EXPORTING
                filename                = 'C:\TEMP\'
                filetype                = 'DAT'
           TABLES
                data_tab                = it_resb
           EXCEPTIONS
                conversion_error        = 1
                invalid_table_width     = 2
                invalid_type            = 3
                no_batch                = 4
                unknown_error           = 5
                gui_refuse_filetransfer = 6
                OTHERS                  = 7.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      LOOP AT it_resb.
        UPDATE resb
           SET sortf = it_resb-sortf
               prvbe = it_resb-prvbe
               lgort = it_resb-lgort
         WHERE rsnum = it_resb-rsnum
           AND rspos = it_resb-rspos
           AND rsart = it_resb-rsart.

        IF sy-subrc EQ 0.
          FORMAT COLOR COL_NORMAL.
          WRITE:/ it_resb-plnum,
                  it_resb-pwwrk,
                  it_resb-matnr,
                  it_resb-auffx,
                  it_resb-rsnum,
                  it_resb-rspos,
                  it_resb-rsart,
                  it_resb-matnr_r,
                  it_resb-prvbe,
                  it_resb-lgort,
                  it_resb-sortf.

        ELSE.
          FORMAT COLOR COL_NEGATIVE.
          WRITE:/ it_resb-plnum,
                  it_resb-pwwrk,
                  it_resb-matnr,
                  it_resb-auffx,
                  it_resb-rsnum,
                  it_resb-rspos,
                  it_resb-rsart,
                  it_resb-matnr_r,
                  it_resb-prvbe,
                  it_resb-lgort,
                  it_resb-sortf.
        ENDIF.
      ENDLOOP.


  ENDCASE.
