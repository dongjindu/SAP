*----------------------------------------------------------------------*
***INCLUDE ZMMR_PARTS_SHORTAGE_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
  DATA: L_CN(2) TYPE N,
        L_OHQTY LIKE ZMMS_SHORT-RP01,
        L_QTY LIKE ZMMS_SHORT-RP01,
        L_DAYS LIKE ZMMS_SHORT-RP01,
        L_FLAG(1),
        L_TEXT(40),
        L_TEXT_SHORT(40),
        L_INDEX LIKE SY-TABIX,
        L_LGPRO LIKE MARC-LGPRO.

  DATA: BEGIN OF LT_MARD OCCURS 0,
        MATNR LIKE MARD-MATNR,
        WERKS LIKE MARD-WERKS,
        LGORT LIKE MARD-LGORT,
        LABST LIKE MARD-LABST,
        END OF LT_MARD.
  FIELD-SYMBOLS: <FS_SHORT>,
                 <FS_DATA>.

  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO IT_SHORT.

    SELECT SINGLE MATKL DISPO VSPVB MAKTX INTO
        (IT_SHORT-MATKL, IT_SHORT-DISPO, IT_SHORT-PRVBE, IT_SHORT-MAKTX)
      FROM MARA AS A
      INNER JOIN MARC AS B
      ON A~MATNR = B~MATNR
      INNER JOIN MAKT AS C
      ON A~MATNR = C~MATNR
      WHERE A~MATNR = IT_DATA-MATNR
      AND SPRAS = 'EN'.

    SELECT SINGLE LIFNR INTO IT_SHORT-LIFNR
      FROM EORD
      WHERE MATNR = IT_DATA-MATNR
        AND FLIFN = 'X'
        AND VDATU <= SY-DATUM
        AND BDATU >= SY-DATUM.
*    ELSE.
*      READ TABLE IT_LIFNR WITH KEY MATNR =  IT_DATA-MATNR.
*      IT_SHORT-LIFNR  = IT_LIFNR-LIFNR.
*    ENDIF.

    IT_SHORT-RP = IT_DATA-SORTF.
** get stock
    REFRESH LT_MARD.

    SELECT MATNR WERKS LGORT LABST INTO TABLE LT_MARD
      FROM MARD
      WHERE MATNR =   IT_DATA-MATNR
        AND LGORT <> '9999'.

    SELECT SINGLE LGPRO INTO L_LGPRO
       FROM MARC
       WHERE MATNR =  IT_DATA-MATNR.

    LOOP AT LT_MARD.
      IF LT_MARD-LGORT = L_LGPRO.
        IT_SHORT-LN_OHQTY =  IT_SHORT-LN_OHQTY + LT_MARD-LABST.
      ELSE.
        IT_SHORT-WH_OHQTY =  IT_SHORT-WH_OHQTY + LT_MARD-LABST.
      ENDIF.
    ENDLOOP.

    L_OHQTY = IT_SHORT-LN_OHQTY + IT_SHORT-WH_OHQTY.
    L_CN = '00'.
    CLEAR: L_FLAG, L_DAYS.
    DO 40 TIMES.
      L_CN = L_CN + 1.
      CONCATENATE 'IT_SHORT-RP' L_CN INTO L_TEXT_SHORT.
      ASSIGN (L_TEXT_SHORT) TO <FS_SHORT>.
      CONCATENATE 'IT_DATA-RP' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS_DATA>.

      <FS_SHORT> = <FS_DATA>.
      MOVE <FS_SHORT> TO L_QTY.

      L_OHQTY = L_OHQTY - L_QTY.
      IF L_FLAG IS INITIAL.
        IF L_OHQTY >= 0.
          L_DAYS = L_DAYS + 1.
        ELSEIF  L_QTY <> 0.
          L_FLAG = 'X'.
          L_DAYS = L_DAYS + L_DAYS / L_QTY.
        ENDIF.
      ENDIF.
      IT_SHORT-HR_OH = L_DAYS.
    ENDDO.
    IF L_DAYS <= P_RED1.
      IT_SHORT-LIGHTS = '1'.  "red
    ENDIF.
    IF L_DAYS <=  P_YEL1 AND L_DAYS > P_RED1.
      IT_SHORT-LIGHTS = '2'.  "yellow
    ENDIF.
    IF L_DAYS >=  P_GRE1 .
      IT_SHORT-LIGHTS = '3'.  "green
    ENDIF.
*    IT_SHORT-SHORT_DAYS = IT_SHORT-HR_OH.
    APPEND IT_SHORT.
    CLEAR: IT_SHORT.
  ENDLOOP.
*  SORT IT_SHORT DESCENDING BY SHORT_DAYS as text MATNR.
    SORT IT_SHORT BY HR_OH MATNR.
  IF P_TOPB = 'X' AND P_TOPV > 0.
    L_INDEX = P_TOPV + 1.
    DELETE IT_SHORT FROM L_INDEX.
  ENDIF.

  IF P_LESB = 'X' AND P_GRTB = 'X'.
    DELETE IT_SHORT WHERE HR_OH >= P_LESV
                      OR HR_OH <= P_GRTV.
  ELSE.
    IF  P_LESB = 'X'.
      DELETE IT_SHORT WHERE HR_OH >= P_LESV.
    ENDIF.
    IF P_GRTB = 'X' .
      DELETE IT_SHORT WHERE HR_OH <= P_GRTV.
    ENDIF.
  ENDIF.

ENDFORM.                    " process_data
