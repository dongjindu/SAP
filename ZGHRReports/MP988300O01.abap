*&---------------------------------------------------------------------*
*&  Include           MP988300O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SET_TEXT_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TEXT_2000 OUTPUT.

  CLEAR: GS_JIKUN, GS_JIKUB.

  IF GT_ZZCGSJIKUN IS INITIAL.

    SELECT DOMVALUE_L AS ZZCGSJIKUN DDTEXT AS VALUE
      FROM DD07V
      INTO CORRESPONDING FIELDS OF TABLE GT_ZZCGSJIKUN
      WHERE DOMNAME = 'ZCGSJIKUN'
            AND DDLANGUAGE = SY-LANGU.

  ENDIF.

  IF GT_ZZCGSJIKUB IS INITIAL.

    SELECT DOMVALUE_L AS ZZCGSJIKUB DDTEXT AS VALUE
      FROM DD07V
      INTO CORRESPONDING FIELDS OF TABLE GT_ZZCGSJIKUB
      WHERE DOMNAME = 'ZCGSJIKUB'
            AND DDLANGUAGE = SY-LANGU.

  ENDIF.

* Set g_jikuntx text.
  READ TABLE GT_ZZCGSJIKUN INTO GS_JIKUN WITH KEY ZZCGSJIKUN = P9883-ZZCGSJIKUN.
  G_JIKUNTX = GS_JIKUN-VALUE.

* Set g_jikubtx text.
  READ TABLE GT_ZZCGSJIKUB INTO GS_JIKUB WITH KEY ZZCGSJIKUB = P9883-ZZCGSJIKUB.
  G_JIKUBTX = GS_JIKUB-VALUE.


  CASE GS_JIKUB-VALUE.
    WHEN 'MG5'.
      G_JIKUBTX = 'Executive'.
    WHEN 'MG4'.
      G_JIKUBTX = 'General Manager / Principal Engineer'.
    WHEN 'MG3'.
      G_JIKUBTX = 'Manager / Senior Engineer'.
    WHEN 'MG2'.
      G_JIKUBTX = 'Specialist / Engineer#'.
    WHEN 'MG1'.
      G_JIKUBTX = 'Associate / Engineer#'.
    WHEN 'TG3'.
      G_JIKUBTX = 'Supervisor'.
    WHEN 'TG2'.
      G_JIKUBTX = 'Senior Operator '.
    WHEN 'TG1'.
      G_JIKUBTX = 'Operator'.
  ENDCASE.




* read infotype 1

  DATA: LS_P9883  TYPE P9883.

  IF <PNNNN> IS  ASSIGNED.

    MOVE  <PNNNN> TO LS_P9883.

  ENDIF.

*    SORT wt BY endda DESCENDING.
*    READ TABLE wt INTO ls_wt INDEX 1.

  SELECT SINGLE * FROM PA0001
    INTO GS_PA0001
    WHERE PERNR   = LS_P9883-PERNR
          AND SUBTY   = LS_P9883-SUBTY
          AND OBJPS   = LS_P9883-OBJPS
          AND SPRPS   = LS_P9883-SPRPS
          AND ENDDA  >= LS_P9883-BEGDA
          AND BEGDA  <= LS_P9883-BEGDA
          AND SEQNR   = LS_P9883-SEQNR.


* Get EE group text

  SELECT SINGLE * FROM T501T
    WHERE SPRSL = SY-LANGU
      AND PERSG = GS_PA0001-PERSG.

* Get  EE subgroup text

  SELECT SINGLE * FROM T503T
    WHERE SPRSL = SY-LANGU
      AND PERSK = GS_PA0001-PERSK.


* Get Position text

  SELECT SINGLE * FROM T528T
    WHERE SPRSL = SY-LANGU
      AND OTYPE = 'S'
      AND PLANS = GS_PA0001-PLANS
      AND ENDDA >= GS_PA0001-ENDDA.

* Get Job key text

  SELECT SINGLE * FROM T513S
    WHERE SPRSL = SY-LANGU
      AND STELL = GS_PA0001-STELL
      AND ENDDA >= GS_PA0001-ENDDA.

* Get Org. Unit text

  SELECT SINGLE * FROM T527X
    WHERE SPRSL = SY-LANGU
      AND ORGEH = GS_PA0001-ORGEH
      AND ENDDA >= GS_PA0001-ENDDA.



ENDMODULE.                 " SET_TEXT_2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_DISPLAY OUTPUT.


  LOOP AT SCREEN.

    IF SCREEN-NAME EQ 'BTNHELP'.
      SCREEN-ACTIVE = 1.
      SCREEN-INPUT = 1.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDMODULE.                 " SET_DISPLAY  OUTPUT
