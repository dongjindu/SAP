*&---------------------------------------------------------------------*
*&  Include           MP988300TOP
*&---------------------------------------------------------------------*


DATA: BEGIN OF gs_jikun,
    zzcgsjikun TYPE zcgsjikun,
    value       TYPE text,
  END OF gs_jikun.


DATA: BEGIN OF gs_jikub,
    zzcgsjikub TYPE zcgsjikub,
    value       TYPE text,
  END OF gs_jikub.

DATA:
              gt_zzcgsjikun   LIKE TABLE OF gs_jikun,
              gt_zzcgsjikub   LIKE TABLE OF gs_jikub,
              g_jikuntx           TYPE text,
              g_jikubtx           TYPE c LENGTH 100,
              gt_tab              TYPE TABLE OF ddshretval WITH HEADER LINE,
              gs_pa0001       TYPE pa0001.

TABLES:T501T,T503T,T528T,T513S,T527X.
