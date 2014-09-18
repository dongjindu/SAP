FUNCTION Z_FRF_GI_WITH_ORDER_ITEM .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_ORDER) TYPE  ZSRF_OD_DISP
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_ORDER STRUCTURE  ZSRF_GI_WITH_ORDER
*"----------------------------------------------------------------------
  DATA: L_TABIX LIKE SY-TABIX.
  DATA: BEGIN OF LT_ORDER OCCURS 0,
          AUFNR TYPE ZSRF_GI_WITH_ORDER-AUFNR,
          RSPOS TYPE ZSRF_GI_WITH_ORDER-RSPOS,
          BWART TYPE ZSRF_GI_WITH_ORDER-BWART,
          BUDAT TYPE ZSRF_GI_WITH_ORDER-BUDAT,
          MATNR TYPE ZSRF_GI_WITH_ORDER-MATNR,
          BDMNG TYPE BDMNG,
          ENMNG TYPE ENMNG,
          ERFME TYPE ZSRF_GI_WITH_ORDER-ERFME,
          LGORT TYPE ZSRF_GI_WITH_ORDER-LGORT,
          WERKS TYPE ZSRF_GI_WITH_ORDER-WERKS,
          WEMPF TYPE ZSRF_GI_WITH_ORDER-WEMPF,
          MAKTX TYPE ZSRF_GI_WITH_ORDER-MAKTX,
          ZCHAN TYPE ZSRF_GI_WITH_ORDER-ZCHAN,
        END OF LT_ORDER.

* CHECK
  CHECK NOT I_ORDER IS INITIAL.
  SELECT A~AUFNR
         B~RSPOS
         B~BWART
         A~GSTRP
         B~MATNR
         B~BDMNG
         B~ENMNG
         B~MEINS
         B~LGORT
         B~WERKS
*         WEMPF
       FROM AFKO AS A INNER JOIN RESB AS B
                        ON  A~RSNUM EQ B~RSNUM
                        AND B~XWAOK EQ 'X'
                        AND B~KZEAR EQ SPACE
                      INNER JOIN AUFK AS C
                        ON  A~AUFNR EQ C~AUFNR
                      INNER JOIN AFIH AS D
                        ON  A~AUFNR EQ D~AUFNR
                      INNER JOIN MAKT AS E
                        ON  B~MATNR EQ E~MATNR
                        AND E~SPRAS EQ SY-LANGU
       INTO TABLE LT_ORDER
       WHERE A~AUFNR EQ I_ORDER-AUFNR
       AND   C~AUART EQ I_ORDER-AUART
       AND   A~GSTRP EQ I_ORDER-DATUB
       AND   D~INGPR EQ I_ORDER-INGPR
       AND   B~XLOEK NE 'X'.
  IF SY-SUBRC EQ 0.
    LOOP AT LT_ORDER.
      MOVE-CORRESPONDING LT_ORDER TO T_ORDER.
      IF T_ORDER-BDMNG GT T_ORDER-ENMNG.
        LT_ORDER-BDMNG = LT_ORDER-BDMNG - LT_ORDER-ENMNG.
        WRITE: LT_ORDER-BDMNG TO T_ORDER-BDMNG
                               UNIT LT_ORDER-ERFME LEFT-JUSTIFIED,
               LT_ORDER-ENMNG TO T_ORDER-ENMNG
                               UNIT LT_ORDER-ERFME LEFT-JUSTIFIED.
        SELECT SINGLE MAKTX
                    FROM MAKT
                    INTO T_ORDER-MAKTX
                    WHERE MATNR EQ T_ORDER-MATNR
                    AND   SPRAS EQ SY-LANGU.
        APPEND T_ORDER. CLEAR T_ORDER.
      ENDIF.
      CLEAR T_ORDER.
    ENDLOOP.
    CLEAR: LT_ORDER, LT_ORDER[].
    ZRESULT = TEXT-M03.
    E_MESS	= TEXT-M01.
  ELSE.
    ZRESULT = TEXT-M04.
    E_MESS	= TEXT-M02.
  ENDIF.

  SORT T_ORDER BY RSPOS.

ENDFUNCTION.
