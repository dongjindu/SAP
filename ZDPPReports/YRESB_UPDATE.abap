*&---------------------------------------------------------------------*
*& Report  YRESB_UPDATE                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  yresb_update MESSAGE-ID zmpp  .


TABLES : resb,plaf.

DATA : it_resb LIKE resb OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_plaf OCCURS 0,
        plnum LIKE plaf-plnum,
        rsnum LIKE plaf-rsnum,
       END OF it_plaf.
DATA : w_int TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_plnum FOR plaf-plnum obligatory,
                 s_sortf FOR resb-sortf.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  REFRESH: it_plaf,it_resb.

LOAD-OF-PROGRAM.

START-OF-SELECTION.
  SELECT rsnum INTO corresponding fields of TABLE it_plaf
     FROM plaf
      WHERE plnum IN s_plnum.
  DESCRIBE TABLE it_plaf LINES w_int.
  IF w_int <> 0.
    LOOP AT it_plaf.
      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_resb
       FROM resb
         WHERE rsnum EQ it_plaf-rsnum
           AND sortf IN s_sortf.
    ENDLOOP.
    CLEAR w_int.
    DESCRIBE TABLE it_resb LINES w_int.
    IF w_int <> 0.
      LOOP AT it_resb.
        IF it_resb-enmng EQ space.
          UPDATE resb SET enmng = it_resb-bdmng
           WHERE rsnum = it_resb-rsnum
              AND rspos = it_resb-rspos
              AND rsart = it_resb-rsart
              AND bdart = it_resb-bdart.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE i001 WITH 'no found data : resb'.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE i001 WITH 'Wrong Planned order'.
    EXIT.
  ENDIF.

END-OF-SELECTION.
  WRITE : / 'plaf'.
  LOOP AT it_plaf.
    WRITE : / it_plaf-plnum,it_plaf-rsnum.
  ENDLOOP.
  WRITE : / 'resb'.
  LOOP AT it_resb.
    WRITE : / it_resb-rsnum,it_resb-rspos,it_resb-bdmng,it_resb-enmng.
  ENDLOOP.
