*----------------------------------------------------------------------*
*   INCLUDE ZXMBCU01                                                   *
*----------------------------------------------------------------------*


**--- add Posting Date(BUDAT) to MSEG Table
*
**  tables : *zmseg.
*
*
*  data : l_budat like mkpf-budat.
*
*  clear : l_budat.
*
*  read table xmkpf index 1.
*
*  move : xmkpf-budat to l_budat.
*
*  loop at xmseg.
**    move : l_budat to xmseg-zbudat.
*    move : '20031101' to xmseg-zbudat.
*    xmseg-sgtxt = '1234'.
*    modify xmseg.
*  endloop.
*
**loop at xmseg.
**    move-corresponding xmseg to *zmseg.
**    modify *zmseg.
**endloop.
*
*
*
**---
