*----------------------------------------------------------------------*
*                                                                      *
*       Output-modules for infotype 9880                               *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       MODULE  P9880 OUTPUT                                           *
*----------------------------------------------------------------------*
*       Default values, Texts                                          *
*----------------------------------------------------------------------*
MODULE p9880 OUTPUT.

  DATA: lv_cnt       TYPE i.

  IF psyst-nselc EQ yes.
* read text fields etc.; do this whenever the screen is show for the
*  first time:
*   PERFORM RExxxx.
    SELECT * FROM zghrlt0001 INTO TABLE gt_zghrlt0001
      WHERE adatanr = p9880-adatanr.

    SORT gt_zghrlt0001 BY seqnr.

    IF psyst-iinit = yes AND psyst-ioper = insert.
* generate default values; do this the very first time on insert only:
*     PERFORM GET_DEFAULT.
    ENDIF.

    IF psyst-iinit = yes AND psyst-ioper = copy.
      IF NOT p9880-adatanr IS INITIAL.
        CALL FUNCTION 'GUID_CREATE'
             IMPORTING
                  ev_guid_32 = p9880-adatanr.
        gt_zghrlt0001-adatanr = p9880-adatanr.
        MODIFY gt_zghrlt0001 TRANSPORTING adatanr WHERE adatanr <> ''.
      ENDIF.
    ENDIF.

  ENDIF.

  IF NOT p9880-sprsl IS INITIAL.
    SELECT SINGLE sptxt FROM t002t INTO sptxt
      WHERE spras = sy-langu
      AND   sprsl = p9880-sprsl.
  ENDIF.


  LOOP AT SCREEN.
    IF screen-name = 'BTN_DISPLAY'.
      CLEAR lv_cnt.
      DESCRIBE TABLE gt_zghrlt0001 LINES lv_cnt.
*      IF lines( gt_zghrlt0001 ) = 0.
      IF lv_cnt = 0.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.
*----------------------------------------------------------------------*
*       MODULE  P9880L OUTPUT                                          *
*----------------------------------------------------------------------*
*       read texts for listscreen
*----------------------------------------------------------------------*
MODULE p9880l OUTPUT.
* PERFORM RExxxx.
ENDMODULE.
