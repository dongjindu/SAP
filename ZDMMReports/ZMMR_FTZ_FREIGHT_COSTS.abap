************************************************************************
* Program Name      : REPORT ZMMR_FTZ
* Creation Date     : 09/2011
* Development Request No :
* Addl Documentation:
* Description       : Dash Board
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 08.13.2014      Victor     T-code has been deleted for APM
************************************************************************
REPORT ZZMMR_FTZ NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.
TABLES: ZTBL, MKPF.
DATA: L_ALV      TYPE REF TO   CL_GUI_ALV_GRID,
      IT_DATA_AIR TYPE TABLE OF ZSMM_FTZ_AIR,
      WS_AIR TYPE ZSMM_FTZ_AIR,
      IT_DATA_SEA TYPE TABLE OF ZSMM_FTZ_SEA,
      WS_SEA TYPE ZSMM_FTZ_SEA.

* Define a 'empty' selection screen, for ALV to be based on
SELECTION-SCREEN BEGIN OF SCREEN 1001.

SELECTION-SCREEN END OF SCREEN 1001.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS:
  S_DATE  FOR SY-DATUM.
PARAMETERS: P_VIA LIKE ZTBL-ZFVIA OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

SELECT-OPTIONS: S_XBLNR FOR MKPF-xblnr.

INITIALIZATION.

START-OF-SELECTION.

  PERFORM GET_DATA.

  PERFORM DISPLAY_DATA.


*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.

*DATA: BEGIN OF LT_EKBE OCCURS 0,
*      EBELN LIKE EKBE-EBELN,
*      EBELP LIKE EKBE-EBELP,
*      CPUDT LIKE EKBE-CPUDT,
*      END OF LT_EKBE.
  DATA: BEGIN OF LT_EKBE OCCURS 0,
  VGBEL  LIKE LIPS-VGBEL ,
  VGPOS LIKE LIPS-VGPOS,
  CPUDT LIKE EKBE-CPUDT,
  EBELN LIKE EKBE-EBELN,
  EBELP LIKE EKBE-EBELP,
  XBLNR LIKE EKBE-XBLNR,
  END OF LT_EKBE.

  DATA: BEGIN OF LT_MKPF OCCURS 0,
   CPUDT LIKE MKPF-CPUdT,
   vbeln LIKE LIKP-vbeln,
   END OF LT_MKPF.

  DATA: BEGIN OF LT_LIPS OCCURS 0,
  VGBEL LIKE LIPS-VGBEL,
  VGPOS LIKE LIPS-VGPOS,
       VBELN LIKE LIPS-VBELN,
       MATNR LIKE LIPS-MATNR,
       ARKTX LIKE LIPS-ARKTX,
       BOLNR LIKE LIKP-BOLNR,
       LIFNR LIKE LIKP-LIFNR,
       LIFEX like likp-LIFEX,
       ZFHBLNO like ztbl-ZFHBLNO,
       ZFCIVNO like ZTMM_FTZ_INV_Hd-ZFCIVNO,
       traid like likp-traid,
        END OF LT_LIPS.

  DATA: BEGIN OF LT_LIKP OCCURS 0,
       VBELN LIKE LIPS-VBELN,
        END OF LT_LIKP.

  DATA: BEGIN OF LT_TEMP OCCURS 0.
          INCLUDE STRUCTURE ZSMM_FTZ_AIR.
  DATA: LIFEX LIKE LIKP-LIFEX,
        VBELN LIKE LIKP-VBELN,
        TRAID LIKE LIKP-TRAID,
        ZFCARC LIKE ZTBL-ZFCARC,
        ZFSPRTC LIKE ZTBL-ZFSPRTC,
        ZFAPPC LIKE ZTBL-ZFAPPC,
        ZFAPRTC LIKE ZTBL-ZFAPRT,
        ZF20FT LIKE ZTBL-ZF20FT,
        ZF40FT LIKE ZTBL-ZF40FT,
        RECNO(6) TYPE N.
  DATA: END OF LT_TEMP.

  DATA: WS_DATA LIKE LT_TEMP,
        LT_DATA LIKE TABLE OF LT_TEMP,
        LW_DATA LIKE LT_TEMP,
        IT_DATA LIKE TABLE OF LT_TEMP,
        L_RECNO TYPE I,
        L_NEW(1).

*  SELECT A~EBELN AS VGBEL EBELP AS VGPOS CPUDT
*  A~EBELN EBELP XBLNR
*  INTO TABLE LT_EKBE
*  FROM EKBE AS A
*  INNER JOIN EKKO AS B
*  ON A~EBELN = B~EBELN
*  WHERE CPUDT IN S_DATE
*    AND VGABE = '1'
*    AND BEWTP = 'E'
*    AND BSART = 'KD'
*    and xblnr = P_xblnr.

  select CPUDT XBLNR AS LIFEX into TABLE lt_mkpf
   from mkpf
   where budat IN S_DATE
     and BLART = 'WE'
     AND XBLNR IN S_XBLNR.

  IF SY-SUBRC = 0.

    SELECT VGBEL VGPOS A~VBELN MATNR ARKTX BOLNR LIFNR
        LIFEX LIFEX as ZFHBLNO bolnr as ZFCIVNO traid
        INTO TABLE LT_LIPS
        FROM LIKP AS A
        INNER JOIN LIPS AS B
        ON A~VBELN = B~VBELN
        FOR ALL ENTRIES IN LT_MKPF
        WHERE a~vbeln = LT_MKPF-vbeln
          AND A~LFART = 'EL'.

*    SELECT VGBEL VGPOS A~VBELN MATNR ARKTX
*    INTO TABLE LT_LIPS
*    FROM LIPS AS A
*    FOR ALL ENTRIES IN LT_EKBE
*    WHERE VGBEL = LT_EKBE-VGBEL
*     AND VGPOS = LT_EKBE-VGPOS.

    IF SY-SUBRC = 0.
*      SORT LT_LIPS BY VBELN.
*      LOOP AT LT_LIPS.
*        LT_LIKP-VBELN = LT_LIPS-VBELN.
*        COLLECT LT_LIKP.
*      ENDLOOP.

*      LOOP AT LT_LIKP.
*
*        SELECT B~ZFHBLNO B~ZFMBLNO BOLNR A~LIFNR  A~VBELN
*              TRAID ZFCARC ZFSPRTC ZFAPPC ZFAPRT
*              ZFETD ZFETA ZFNEWT ZFTOWT ZF20FT ZF40FT
*              ZFCAMT ZFIAMT LIFEX
*              APPENDING CORRESPONDING FIELDS OF TABLE LT_TEMP
*         FROM LIKP AS A
*           INNER JOIN ZTBL AS B
*                     ON A~LIFEX = B~ZFHBLNO
*           inner JOIN ZTMM_FTZ_INV_HD AS C
*           ON A~BOLNR = C~ZFCIVNO
*           AND A~LIFEX = C~ZFHBLNO
*          WHERE A~VBELN = LT_LIKP-VBELN
*          AND B~ZFVIA = P_VIA.
*      ENDLOOP.

*      SELECT B~ZFHBLNO B~ZFMBLNO BOLNR A~LIFNR  A~VBELN
*             TRAID ZFCARC ZFSPRTC ZFAPPC ZFAPRT
*             ZFETD ZFETA ZFNEWT ZFTOWT ZF20FT ZF40FT
*             ZFCAMT ZFIAMT
*        INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
*        FROM LIKP AS A
*          INNER JOIN ZTBL AS B
*                    ON A~LIFEX = B~ZFHBLNO
*          INNER JOIN ZTMM_FTZ_INV_HD AS C
*          ON A~BOLNR = C~ZFCIVNO
*          AND A~LIFEX = C~ZFHBLNO
*        FOR ALL ENTRIES IN LT_LIKP
*        WHERE A~VBELN = LT_LIKP-VBELN
*         AND B~ZFVIA = P_VIA.


      SELECT B~ZFHBLNO B~ZFMBLNO
*  BOLNR A~LIFNR  A~VBELN TRAID LIFEX
             ZFCARC ZFSPRTC ZFAPPC ZFAPRTC ZFAPRT
             ZFETD ZFETA ZFNEWT ZFTOWT ZF20FT ZF40FT
             ZFCAMT ZFIAMT
             APPENDING CORRESPONDING FIELDS OF TABLE LT_TEMP
        from ZTBL AS B
          inner JOIN ZTMM_FTZ_INV_HD AS C
          on b~ZFHBLNO = c~ZFHBLNO
         for all entries in LT_LIPS
         WHERE B~ZFHBLNO = LT_LIPS-ZFHBLNO
          and C~ZFCIVNO = LT_LIPS-ZFCIVNO
          AND C~ZFHBLNO = lT_LIPS-ZFHBLNO
          AND B~ZFVIA = P_VIA.

*      SORT LT_TEMP BY ZFHBLNO ZFMBLNO BOLNR VBELN.
*      DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING
*             ZFHBLNO ZFMBLNO BOLNR VBELN.
    ENDIF.

    if P_VIA = 'AIR'.
    sort LT_LIPS by LIFEX MATNR.
    DELETE ADJACENT DUPLICATES FROM LT_LIPS COMPARING LIFEX MATNR.
    ELSE.
     sort LT_LIPS by LIFEX TRAID.
    DELETE ADJACENT DUPLICATES FROM LT_LIPS COMPARING LIFEX TRAID.

    ENDIF.
    sort lt_mkpf by vbeln.

    LOOP AT LT_TEMP.
*      READ TABLE LT_EKBE WITH KEY
*          XBLNR = LT_TEMP-LIFEX BINARY SEARCH.
*      CHECK SY-SUBRC = 0.
      MOVE-CORRESPONDING LT_TEMP TO WS_DATA.
      CONCATENATE LT_TEMP-ZFCARC LT_TEMP-ZFSPRTC INTO WS_DATA-ZESPRT.
      CONCATENATE LT_TEMP-ZFAPPC LT_TEMP-ZFAPRTC INTO WS_DATA-ZFAPRT.

      LOOP AT LT_LIPS WHERE LIFEX = LT_TEMP-ZFHBLNO.
        WS_DATA-MATNR = LT_LIPS-MATNR.
        WS_DATA-ARKTX = LT_LIPS-ARKTX.
        WS_DATA-BOLNR = LT_LIPS-BOLNR.
        WS_DATA-LIFNR = LT_LIPS-LIFNR.
        WS_DATA-VBELN = LT_LIPS-VBELN.
        WS_DATA-TRAID = LT_LIPS-TRAID.
        WS_DATA-LIFEX = LT_LIPS-LIFEX.
  read table lt_mkpf with key vbeln = LT_LIPS-VBELN binary search.
      if sy-subrc = 0.
        WS_DATA-CPUDT = lt_mkpf-cpudt.
      endif.
        APPEND WS_DATA TO LT_DATA.
        CLEAR: WS_DATA-MATNR, WS_DATA-ARKTX, WS_DATA-BOLNR,
         ws_daTA-LIFNR, WS_DATA-VBELN, WS_DATA-TRAID,
         WS_DATA-LIFEX, WS_DATA-CPUDT.
      ENDLOOP.
      CLEAR: WS_DATA.
    ENDLOOP.

    SORT LT_DATA BY ZFHBLNO ZFMBLNO BOLNR.

    LOOP AT LT_DATA INTO WS_DATA.
      CLEAR: LW_DATA.
      LW_DATA-TRAID = WS_DATA-TRAID.
      LW_DATA-CPUDT = WS_DATA-CPUDT.
      LW_DATA-MATNR = WS_DATA-MATNR.
      LW_DATA-ARKTX = WS_DATA-ARKTX.

      AT NEW ZFHBLNO.
        L_NEW = 'X'.
      ENDAT.
      AT NEW ZFMBLNO.
        L_NEW = 'X'.
      ENDAT.
      AT NEW BOLNR.
        L_NEW = 'X'.
      ENDAT.

      IF L_NEW = 'X'.
        LW_DATA-RECNO = L_RECNO.
        MODIFY IT_DATA FROM LW_DATA TRANSPORTING RECNO
            WHERE RECNO = '999999'.
        LW_DATA = WS_DATA.
        LW_DATA-RECNO = '999999'.
        APPEND LW_DATA TO IT_DATA.
        CLEAR: L_NEW, L_RECNO.
      ELSE.
        APPEND LW_DATA TO IT_DATA.
      ENDIF.
      L_RECNO = L_RECNO + 1.

    ENDLOOP.
    LW_DATA-RECNO = L_RECNO.
    MODIFY IT_DATA FROM LW_DATA TRANSPORTING RECNO
        WHERE RECNO = '999999'.

    IF P_VIA = 'AIR'.
      LOOP AT IT_DATA INTO WS_DATA.
        MOVE-CORRESPONDING WS_DATA TO WS_AIR.
        APPEND WS_AIR TO IT_DATA_AIR.
      ENDLOOP.
    ELSE.
      LOOP AT IT_DATA INTO WS_DATA.
        MOVE-CORRESPONDING WS_DATA TO WS_SEA.
        APPEND WS_SEA TO IT_DATA_SEA.
      ENDLOOP.
    ENDIF.
  ELSE.
    MESSAGE E001 WITH 'No Data'.
    EXIT.
  ENDIF.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
* Creation of the ALV object, when we use cl_gui_container=>screen0 as
*parent, the ALVGrid control will
* automatically use the full screen to display the grid, NO CONTAINER
*DEFINITION IS REQUIRED !
  CREATE OBJECT L_ALV
    EXPORTING
      I_PARENT = CL_GUI_CONTAINER=>SCREEN0.

* calling the display of the grid, the system will automatically create
*the fieldcatalog based
* on the table name you pass in parameter
  IF P_VIA = 'AIR'.
    CALL METHOD L_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_STRUCTURE_NAME = 'ZSMM_FTZ_AIR'
      CHANGING
        IT_OUTTAB        = IT_DATA_AIR.
  ELSE.
    CALL METHOD L_ALV->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING
         I_STRUCTURE_NAME = 'ZSMM_FTZ_SEA'
       CHANGING
         IT_OUTTAB        = IT_DATA_SEA.
  ENDIF.

  CALL SELECTION-SCREEN 1001.

ENDFORM.                    " display_data
