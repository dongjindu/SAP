************************************************************************
* Program Name      : ZEMMPM52E_MATERIAL_0504_NEW
* Author            : Furong Wang
* Creation Date     : 11/26/08
* Specifications By :
* Development Request No :
* Addl Documentation: Originally copy from ZEMMPM52E_MATERIAL_0504
* Description       : Material Master Management
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
*
************************************************************************

REPORT ZEMMPM52E_MATERIAL_0504_NEW NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID ZMMM.

**---
INCLUDE : ZRMMPMXXR_INCL.

TABLES : ZTMM_MARA,
         ZTMM_AUTH,
         QMAT,
         MBEW,
         T024.

DATA : W_ERROR_CHECK(1),
       W_FIELD_NAME(20).


**--- Internal Tables
DATA : IT_ZTMM_AUTH LIKE ZTMM_AUTH OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_ITAB OCCURS 0.
        INCLUDE STRUCTURE ZTMM_MARA.
DATA :   W_SELECTED(1),
         W_CHANGE(1),
       END OF IT_ITAB.

CONSTANTS : C_FERT LIKE MARA-MTART VALUE 'FERT',
            C_HALB LIKE MARA-MTART VALUE 'HALB',
            C_ROH  LIKE MARA-MTART VALUE 'ROH'.


***excel

DATA: COL_TEXT LIKE GXXLT_V OCCURS 0 WITH HEADER LINE.
DATA: ONLINE_TEXT LIKE GXXLT_O OCCURS 0,
      PRINT_TEXT LIKE GXXLT_P OCCURS 0.


INCLUDE OLE2INCL.
DATA: H_EXCEL TYPE OLE2_OBJECT,        " Excel object
      H_MAPL TYPE OLE2_OBJECT,         " list of workbooks
      H_MAP TYPE OLE2_OBJECT,          " workbook
      H_ZL TYPE OLE2_OBJECT,           " cell
      H_F TYPE OLE2_OBJECT.            " font
DATA  H TYPE I.


DATA : IT_TEMP LIKE IT_ITAB OCCURS 0 WITH HEADER LINE,
       IT_COPY LIKE IT_ITAB OCCURS 0 WITH HEADER LINE.

DATA : IT_ZTMM_MARA LIKE ZTMM_MARA OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_FCODE OCCURS 0,
         FCODE LIKE SY-UCOMM,
       END OF IT_FCODE.

*
**--- BAPI
DATA : WA_HEADDATA             LIKE BAPIMATHEAD,
       WA_CLIENTDATA           LIKE BAPI_MARA,
       WA_CLIENTDATAX          LIKE BAPI_MARAX,
       WA_PLANTDATA            LIKE BAPI_MARC,
       WA_PLANTDATAX           LIKE BAPI_MARCX,
       WA_WAREHOUSENUMBERDATA  LIKE BAPI_MLGN,
       WA_WAREHOUSENUMBERDATAX LIKE BAPI_MLGNX,
       WA_STORAGETYPEDATA      LIKE BAPI_MLGT,
       WA_STORAGETYPEDATAX     LIKE BAPI_MLGTX,
       WA_VALUATIONDATA        LIKE BAPI_MBEW,
       WA_VALUATIONDATAX       LIKE BAPI_MBEWX,
       WA_STORAGELOCATIONDATA  LIKE BAPI_MARD,
       WA_STORAGELOCATIONDATAX LIKE BAPI_MARDX,
       WA_BAPIRET2             LIKE BAPIRET2.

DATA : IT_UNITSOFMEASURE  LIKE BAPI_MARM OCCURS 0 WITH HEADER LINE,
       IT_UNITSOFMEASUREX LIKE BAPI_MARMX OCCURS 0 WITH HEADER LINE.

DATA : IT_BAPIRET2 LIKE BAPI_MATRETURN2 OCCURS 0 WITH HEADER LINE.


**--- BDC
DATA : BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF IT_BDC.

DATA : BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF IT_MESS.

DATA : IT_MESSAGE LIKE IT_MESS OCCURS 0 WITH HEADER LINE.

DATA : W_MODE LIKE CTU_PARAMS-DISMODE VALUE 'N'.

**--- Variables
DATA : W_OKCODE LIKE SY-UCOMM,
       W_SAVE_OKCODE LIKE SY-UCOMM,
       W_LOOPC LIKE SY-LOOPC,
       W_TOT_LINES TYPE I,
       W_SELECTED(1),
       W_RADIO(1).

DATA : W_POSITION TYPE I,
       W_FOUND(1),
       W_FIND_POS TYPE I,
       W_LOOP_FIRST TYPE I.

DATA : WA_TC9000 TYPE CXTAB_COLUMN.

DATA : W_IND_BOM(1),
       W_IND_MM(1),
       W_IND_PUR(1),
       W_IND_DEV(1),
       W_IND_QM(1),
       W_IND_HALB(1).

DATA : W_SUBRC LIKE SY-SUBRC,
       W_MESSA(80).


**--- Table Control
CONTROLS : TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

DATA : WA_TAB TYPE CXTAB_COLUMN.


**--- Constants
CONSTANTS : C_MTART_FERT      LIKE MARA-MTART VALUE 'FERT',
            C_MTART_HALB      LIKE MARA-MTART VALUE 'HALB',
            C_MTART_ROH       LIKE MARA-MTART VALUE 'ROH'.

CONSTANTS : C_BOM             LIKE ZTMM_AUTH-FUNC1 VALUE 'BOM',
            C_MM              LIKE ZTMM_AUTH-FUNC1 VALUE 'MM',
            C_PUR             LIKE ZTMM_AUTH-FUNC1 VALUE 'PUR',
            C_SH              LIKE ZTMM_AUTH-FUNC1 VALUE 'SH',
            C_ST             LIKE ZTMM_AUTH-FUNC1 VALUE 'ST',
            C_WM              LIKE ZTMM_AUTH-FUNC1 VALUE 'WM'.

*             c_halb            LIKE ztmm_auth-func1 VALUE 'HALB'.

CONSTANTS : C_LGNUM_P01       LIKE ZTMM_MARA-LGNUM VALUE 'P01'.

CONSTANTS : C_WERKS_P001      LIKE T001W-WERKS VALUE 'P001',
            C_WERKS_E001      LIKE T001W-WERKS VALUE 'E001',
            C_WERKS_E002      LIKE T001W-WERKS VALUE 'E002',
            C_LGORT_P400      LIKE T001L-LGORT VALUE 'P400',
            C_LGORT_P500      LIKE T001L-LGORT VALUE 'P500',
            C_LGORT_E100      LIKE T001L-LGORT VALUE 'E100',
            C_LGORT_E110      LIKE T001L-LGORT VALUE 'E110',
            C_PROFL_K         LIKE MARA-PROFL VALUE 'K',
            C_PROFL_V         LIKE MARA-PROFL VALUE 'V',
            C_PROFL_M         LIKE MARA-PROFL VALUE 'M',
            C_LGTYP_422       LIKE ZTMM_MARA-LGTYP VALUE '422',
            C_LGTYP_431       LIKE ZTMM_MARA-LGTYP VALUE '431',
            C_LGTYP_432       LIKE ZTMM_MARA-LGTYP VALUE '432',
            C_LGTYP_433       LIKE ZTMM_MARA-LGTYP VALUE '433',
            C_LGTYP_434       LIKE ZTMM_MARA-LGTYP VALUE '434',
            C_LGTYP_435       LIKE ZTMM_MARA-LGTYP VALUE '435',
            C_LGTYP_436       LIKE ZTMM_MARA-LGTYP VALUE '436',
            C_MATKL_NF_KD     LIKE MARA-MATKL VALUE 'NF-KD',
            C_MATKL_NF_KD_EN  LIKE MARA-MATKL VALUE 'NF-KD-EN',
            C_MATKL_NF_KD_TM  LIKE MARA-MATKL VALUE 'NF-KD-TM',
            C_MATKL_CM_KD     LIKE MARA-MATKL VALUE 'CM-KD',
            C_MATKL_CM_KD_EN  LIKE MARA-MATKL VALUE 'CM-KD-EN',
            C_MATKL_CM_KD_TM  LIKE MARA-MATKL VALUE 'CM-KD-TM',
            C_MATKL_CO_KD     LIKE MARA-MATKL VALUE 'CO-KD',
            C_MATKL_CO_KD_EN  LIKE MARA-MATKL VALUE 'CO-KD-EN',
            C_MATKL_CO_KD_TM  LIKE MARA-MATKL VALUE 'CO-KD-TM',
            C_MATKL_NF_LP     LIKE MARA-MATKL VALUE 'NF-LP',
            C_MATKL_CM_LP     LIKE MARA-MATKL VALUE 'CM-LP',
            C_MATKL_CO_LP     LIKE MARA-MATKL VALUE 'CO-LP',
            C_MTPOS_MARA_ZNOR LIKE ZTMM_MARA-MTPOS_MARA VALUE 'ZNOR',
            C_DISLS_EX        LIKE ZTMM_MARA-DISLS VALUE 'EX',
            C_DISLS_PK        LIKE ZTMM_MARA-DISLS VALUE 'PK',
            C_DISLS_WB        LIKE ZTMM_MARA-DISLS VALUE 'WB',
            C_SOBSL_40        LIKE ZTMM_MARA-SOBSL VALUE '40',
            C_VSPVB_B1        LIKE ZTMM_MARA-VSPVB VALUE 'B1',
            C_VSPVB_P3        LIKE ZTMM_MARA-VSPVB VALUE 'P3',
            C_VSPVB_T1        LIKE ZTMM_MARA-VSPVB VALUE 'T1',
            C_VSPVB_T2        LIKE ZTMM_MARA-VSPVB VALUE 'T2',
            C_VSPVB_T3        LIKE ZTMM_MARA-VSPVB VALUE 'T3',
            C_VSPVB_C1        LIKE ZTMM_MARA-VSPVB VALUE 'C1',
            C_VSPVB_C2        LIKE ZTMM_MARA-VSPVB VALUE 'C2',
            C_VSPVB_F1        LIKE ZTMM_MARA-VSPVB VALUE 'F1',
            C_VSPVB_F2        LIKE ZTMM_MARA-VSPVB VALUE 'F2',
            C_VSPVB_F3        LIKE ZTMM_MARA-VSPVB VALUE 'F3',
            C_VSPVB_F4        LIKE ZTMM_MARA-VSPVB VALUE 'F4',
            C_VSPVB_OK        LIKE ZTMM_MARA-VSPVB VALUE 'OK',
            C_VSPVB_T1S       LIKE ZTMM_MARA-VSPVB VALUE 'T1S',
            C_VSPVB_T2S       LIKE ZTMM_MARA-VSPVB VALUE 'T2S',
            C_VSPVB_T3S       LIKE ZTMM_MARA-VSPVB VALUE 'T3S',
            C_VSPVB_C1S       LIKE ZTMM_MARA-VSPVB VALUE 'C1S',
            C_VSPVB_C2S       LIKE ZTMM_MARA-VSPVB VALUE 'C2S',
            C_VSPVB_F1S       LIKE ZTMM_MARA-VSPVB VALUE 'F1S',
            C_VSPVB_F2S       LIKE ZTMM_MARA-VSPVB VALUE 'F2S',
            C_VSPVB_F3S       LIKE ZTMM_MARA-VSPVB VALUE 'F3S',
            C_VSPVB_F4S       LIKE ZTMM_MARA-VSPVB VALUE 'F4S',
            C_VSPVB_ENG       LIKE ZTMM_MARA-VSPVB VALUE 'ENG',
            C_DISPO_M02       LIKE ZTMM_MARA-DISPO VALUE 'M02',
            C_TEMPB_11        LIKE ZTMM_MARA-TEMPB VALUE '11',
            C_TEMPB_12        LIKE ZTMM_MARA-TEMPB VALUE '12',
            C_RAUBE_11        LIKE ZTMM_MARA-RAUBE VALUE '11',
            C_RAUBE_12        LIKE ZTMM_MARA-RAUBE VALUE '12',
            C_RAUBE_13        LIKE ZTMM_MARA-RAUBE VALUE '13',
            C_RAUBE_14        LIKE ZTMM_MARA-RAUBE VALUE '14',
            C_ABCIN_A         LIKE ZTMM_MARA-ABCIN VALUE 'A',
            C_ABCIN_B         LIKE ZTMM_MARA-ABCIN VALUE 'B',
            C_ABCIN_C         LIKE ZTMM_MARA-ABCIN VALUE 'C',
            C_ABCIN_D         LIKE ZTMM_MARA-ABCIN VALUE 'D',
            C_BKLAS_3000      LIKE ZTMM_MARA-BKLAS VALUE '3000',
            C_BKLAS_3001      LIKE ZTMM_MARA-BKLAS VALUE '3001',
            C_BKLAS_3005      LIKE ZTMM_MARA-BKLAS VALUE '3005',
            C_LTKZA_003       LIKE ZTMM_MARA-LTKZA VALUE '003',
            C_LTKZA_004       LIKE ZTMM_MARA-LTKZA VALUE '004',
            C_GEWEI_KG        LIKE ZTMM_MARA-GEWEI VALUE 'KG',
            C_MMSTA_11        LIKE MARC-MMSTA VALUE '11',
            C_BESKZ_F         LIKE MARC-BESKZ VALUE 'F',
            C_SBDKZ_2         LIKE MARC-SBDKZ VALUE '2',
            C_DISPO_001       LIKE ZTMM_MARA-DISPO VALUE '001'.
*            C_DISPO_M02       LIKE ztmm_mara-dispo VALUE 'M02'.


**--- Macro
DEFINE APPEND_FIELDCAT.
  &1 = &1 + 1.
  W_FIELDCAT-COL_POS    = &1.
  W_FIELDCAT-FIELDNAME  = &2.
  W_FIELDCAT-OUTPUTLEN  = &3.
  W_FIELDCAT-SELTEXT_L  = &4.
  W_FIELDCAT-SELTEXT_M  = &4.
  W_FIELDCAT-SELTEXT_S  = &4.
  W_FIELDCAT-DATATYPE   = &5.
  W_FIELDCAT-KEY        = &6.
  W_FIELDCAT-QFIELDNAME = &7.
  W_FIELDCAT-CFIELDNAME = &8.
  APPEND W_FIELDCAT.
  CLEAR : W_FIELDCAT.
END-OF-DEFINITION.

DEFINE APPEND_TOP.
  CLEAR : W_LINE.
  IF NOT &3 IS INITIAL OR NOT &4 IS INITIAL.
    W_LINE-TYP   = &1.
    W_LINE-KEY   = &2.
    CONCATENATE &3 '~' &4 INTO W_LINE-INFO SEPARATED BY SPACE.
    APPEND W_LINE TO W_TOP_OF_PAGE.
  ENDIF.
END-OF-DEFINITION.

DEFINE APPEND_SORTCAT.
  W_SORTCAT-SPOS      = &1.
  W_SORTCAT-FIELDNAME = &2.
  W_SORTCAT-TABNAME   = &3.
  W_SORTCAT-UP        = &4.
  W_SORTCAT-SUBTOT    = &5.
  APPEND W_SORTCAT.
  CLEAR : W_SORTCAT.
END-OF-DEFINITION.


**---
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : S_MTART FOR MARA-MTART OBLIGATORY NO INTERVALS
                                        NO-EXTENSION,
                 S_MATNR FOR MARA-MATNR,
                 S_WERKS FOR MARC-WERKS NO INTERVALS NO-EXTENSION,
                 S_LGORT FOR MARD-LGORT NO INTERVALS NO-EXTENSION,
                 S_LGTYP FOR ZTMM_MARA-LGTYP,
                 S_PROFL FOR MARA-PROFL NO INTERVALS NO-EXTENSION,
*                 s_duedt FOR ztmm_mara-duedt,
                 S_MATKL FOR ZTMM_MARA-MATKL,
                 S_EKGRP FOR T024-EKGRP,
                 S_STAWN FOR MARC-STAWN,
                 S_DISPO FOR ZTMM_MARA-DISPO,
                 S_DISLS FOR MARC-DISLS NO INTERVALS NO-EXTENSION,
                 S_VSPVB FOR MARC-VSPVB,
                 S_ERDAT FOR ZTMM_MARA-ERDAT,
                 S_ERNAM FOR ZTMM_MARA-ERNAM,
                 S_MSTAE FOR MARA-MSTAE,
                 S_MSTDE FOR MARA-MSTDE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS : r_01 RADIOBUTTON GROUP gr1 DEFAULT 'X'.
*SELECTION-SCREEN COMMENT (25) text-002 FOR FIELD r_01.
*SELECTION-SCREEN COMMENT (01) TEXT-005.                     " (
PARAMETERS : R_011 RADIOBUTTON GROUP GR11 DEFAULT 'X' USER-COMMAND R01.
SELECTION-SCREEN COMMENT (06) TEXT-006 FOR FIELD R_011.     " master
PARAMETERS : R_012 RADIOBUTTON GROUP GR11.
SELECTION-SCREEN COMMENT (13) TEXT-007 FOR FIELD R_012.     "Storage /mm
PARAMETERS : R_013 RADIOBUTTON GROUP GR11.
SELECTION-SCREEN COMMENT (05) TEXT-008 FOR FIELD R_013.     "WM/ pur
*PARAMETERS : r_014 RADIOBUTTON GROUP gr11.
*SELECTION-SCREEN COMMENT (05) text-009 FOR FIELD r_014.     " dev
*PARAMETERS : r_015 RADIOBUTTON GROUP gr11.
*SELECTION-SCREEN COMMENT (05) text-010 FOR FIELD r_015.     " qm
*SELECTION-SCREEN COMMENT (01) TEXT-011.                     " )
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS : r_02 RADIOBUTTON GROUP gr1.
*SELECTION-SCREEN COMMENT (25) text-003 FOR FIELD r_02.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS : r_03 RADIOBUTTON GROUP gr1.
*SELECTION-SCREEN COMMENT (25) text-004 FOR FIELD r_03.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLOCK2.

SELECTION-SCREEN END OF BLOCK BLOCK1.


**---
*AT SELECTION-SCREEN ON RADIOBUTTON GROUP gr1.
AT SELECTION-SCREEN.
  CASE SY-UCOMM.
    WHEN 'R01'.
**      IF r_01 EQ space.
*        MOVE : 'X'   TO r_01,
*               space TO r_02,
*               space TO r_03.
*      ENDIF.
  ENDCASE.

**---
INITIALIZATION.

**---
TOP-OF-PAGE.

**---
START-OF-SELECTION.
  PERFORM CHECK_AUTHORITY.
  PERFORM GET_DATA.
*---
  IF IT_ITAB[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    IT_TEMP[] = IT_ITAB[].
    CALL SCREEN 9000.
  ENDIF.




**---

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA : BEGIN OF IT_QMAT OCCURS 0,
           ART LIKE QMAT-ART,
           AKTIV LIKE QMAT-AKTIV,
         END OF IT_QMAT.
  DATA:  L_FLAG(1).

  CLEAR : IT_ITAB, IT_ITAB[], IT_TEMP, IT_TEMP[].


  IF C_MTART_FERT = 'FERT' OR C_MTART_HALB = 'HALB'
                           OR C_MTART_ROH = 'ROH'.

*    IF S_LGTYP[] IS INITIAL.
*      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
*             FROM MARA AS A INNER JOIN MARC AS B
*               ON A~MANDT EQ B~MANDT
*              AND A~MATNR EQ B~MATNR
*              INNER JOIN MARD AS C
*              ON A~MANDT EQ C~MANDT
*              AND A~MATNR EQ C~MATNR
*              INNER JOIN mlgt AS d
*              ON A~MANDT EQ d~MANDT
*              AND A~MATNR EQ d~MATNR
*
*            WHERE MTART IN S_MTART
*              AND A~MATNR IN S_MATNR
*              AND B~WERKS IN S_WERKS
*              AND C~LGORT IN S_LGORT
*              AND PROFL IN S_PROFL
**            AND duedt IN s_duedt
*              AND MATKL IN S_MATKL
*              AND EKGRP IN S_EKGRP
*              AND STAWN IN S_STAWN
*              AND DISPO IN S_DISPO
*              AND DISLS IN S_DISLS
*              AND VSPVB IN S_VSPVB
*              AND MSTAE IN S_MSTAE
*              AND MSTDE IN S_MSTDE
*            AND A~ERSDA IN S_ERDAT
*            AND ERNAM IN S_ERNAM
**            AND loekz EQ space
*              AND A~LVORM EQ SPACE.
*    ELSE.

    IF R_011 = 'X'.
      W_RADIO = '1'.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
              FROM MARA AS A INNER JOIN MARC AS B
                ON A~MANDT EQ B~MANDT
               AND A~MATNR EQ B~MATNR
*               INNER JOIN MARD AS C
*               ON A~MANDT EQ C~MANDT
*               AND A~MATNR EQ C~MATNR
*                INNER JOIN MLGT AS D
*                ON A~MANDT EQ D~MANDT
*                AND A~MATNR EQ D~MATNR
             WHERE MTART IN S_MTART
               AND A~MATNR IN S_MATNR
               AND B~WERKS IN S_WERKS
               AND PROFL IN S_PROFL
*            AND duedt IN s_duedt
               AND MATKL IN S_MATKL
               AND EKGRP IN S_EKGRP
               AND STAWN IN S_STAWN
               AND DISPO IN S_DISPO
               AND DISLS IN S_DISLS
               AND VSPVB IN S_VSPVB
               AND MSTAE IN S_MSTAE
               AND MSTDE IN S_MSTDE
             AND A~ERSDA IN S_ERDAT
             AND ERNAM IN S_ERNAM
*            AND loekz EQ space
               AND A~LVORM EQ SPACE.

    ELSEIF R_012 = 'X'.
      W_RADIO = '2'.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
                  FROM MARA AS A INNER JOIN MARC AS B
                    ON A~MANDT EQ B~MANDT
                   AND A~MATNR EQ B~MATNR
                   INNER JOIN MARD AS C
                   ON A~MANDT EQ C~MANDT
                   AND A~MATNR EQ C~MATNR
                 WHERE MTART IN S_MTART
                   AND A~MATNR IN S_MATNR
                   AND B~WERKS IN S_WERKS
                   AND C~LGORT IN S_LGORT
*                AND LGTYP IN S_LGTYP
                   AND PROFL IN S_PROFL
*            AND duedt IN s_duedt
                   AND MATKL IN S_MATKL
                   AND EKGRP IN S_EKGRP
                   AND STAWN IN S_STAWN
                   AND DISPO IN S_DISPO
                   AND DISLS IN S_DISLS
                   AND VSPVB IN S_VSPVB
                   AND MSTAE IN S_MSTAE
                   AND MSTDE IN S_MSTDE
                 AND A~ERSDA IN S_ERDAT
                 AND ERNAM IN S_ERNAM
*            AND loekz EQ space
                   AND A~LVORM EQ SPACE.
    ELSE.
      W_RADIO = '3'.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
               FROM MARA AS A INNER JOIN MARC AS B
                 ON A~MANDT EQ B~MANDT
                AND A~MATNR EQ B~MATNR
                INNER JOIN MARD AS C
                ON A~MANDT EQ C~MANDT
                AND A~MATNR EQ C~MATNR
                INNER JOIN MLGT AS D
                ON A~MANDT EQ D~MANDT
                AND A~MATNR EQ D~MATNR
              WHERE MTART IN S_MTART
                AND A~MATNR IN S_MATNR
                AND B~WERKS IN S_WERKS
                AND C~LGORT IN S_LGORT
                AND LGTYP IN S_LGTYP
                AND PROFL IN S_PROFL
*            AND duedt IN s_duedt
                AND MATKL IN S_MATKL
                AND EKGRP IN S_EKGRP
                AND STAWN IN S_STAWN
                AND DISPO IN S_DISPO
                AND DISLS IN S_DISLS
                AND VSPVB IN S_VSPVB
                AND MSTAE IN S_MSTAE
                AND MSTDE IN S_MSTDE
              AND A~ERSDA IN S_ERDAT
              AND ERNAM IN S_ERNAM
*            AND loekz EQ space
                AND A~LVORM EQ SPACE.
    ENDIF.
*    ENDIF.
*    SORT IT_TEMP BY MATNR WERKS LGORT LGTYP.
*    DELETE ADJACENT DUPLICATES FROM IT_TEMP
*    COMPARING MATNR WERKS LGORT LGTYP.
*--- insert by stlim (2004/10/26)
    DELETE IT_TEMP WHERE
                   NOT ( ( MTART EQ C_FERT AND NTGEW EQ 0 ) OR
                         ( MTART EQ C_HALB AND
                         ( MATNR+0(1) EQ '6' OR MATNR+0(1) EQ '7' ) AND
                        ( MATNR+4(1) NE 'E' AND MATNR+4(1) NE 'M' ) AND
                           NTGEW EQ 0 ) OR
                         ( MTART EQ C_ROH  AND MATKL EQ 'INIT' ) ).
*--- end of insert

*---
*   if not IT_TEMP[] is initial and not s_lgtyp[] is initial.
*      select * into lt_mlgt
*        from mlgt
*        for all entries in it_temp.
*        where matnr = it_temp-matnr
*          and lgnum in s_lgnum
*          and lgtyp in s_lgtyp.
*     if sy-subrc = 0.
*        loop at it_temp.
*
*        endif.
*     endif.
*   endif.

*--- blocked by stlim (2004/10/26)
*  DELETE it_temp WHERE NOT ( ( mtart EQ c_fert AND ntgew EQ 0 ) OR
*                             ( mtart EQ c_halb AND matnr+4(1) NE 'E'
*                                               AND ntgew EQ 0 ) OR
*                             ( mtart EQ c_roh  AND matkl EQ 'INIT' ) ).
*--- end of block

*---

    LOOP AT IT_TEMP.
      MOVE-CORRESPONDING IT_TEMP TO IT_ITAB.
*---
      IF IT_TEMP-PROFL EQ 'K'.
        IT_ITAB-DUEDT = IT_ITAB-MSTDE - 63.
      ELSEIF IT_TEMP-PROFL EQ 'V'.
        IT_ITAB-DUEDT = IT_ITAB-MSTDE - 7.
      ENDIF.
*---
      CLEAR : MBEW.
      SELECT SINGLE BKLAS INTO IT_ITAB-BKLAS
                          FROM MBEW
                         WHERE MATNR EQ IT_ITAB-MATNR
                           AND BWKEY EQ IT_ITAB-WERKS.
*---
      CLEAR : IT_QMAT, IT_QMAT[].
      SELECT ART AKTIV INTO CORRESPONDING FIELDS OF TABLE IT_QMAT
                       FROM QMAT
                      WHERE MATNR EQ IT_ITAB-MATNR
                        AND WERKS EQ IT_ITAB-WERKS.
      READ TABLE IT_QMAT INDEX 1.
      MOVE : IT_QMAT-ART   TO IT_ITAB-ART01,
             IT_QMAT-AKTIV TO IT_ITAB-AKTI1.
      READ TABLE IT_QMAT INDEX 2.
      MOVE : IT_QMAT-ART   TO IT_ITAB-ART02,
             IT_QMAT-AKTIV TO IT_ITAB-AKTI2.
*---
      IT_ITAB-ERNAM = IT_ITAB-AENAM = SY-UNAME.
      IT_ITAB-ERDAT = IT_ITAB-AEDAT = SY-DATUM.
      IT_ITAB-ERZET = IT_ITAB-AEZET = SY-UZEIT.
*--- if source = 'V', then PUR confirmation indicator : 'X'
*      IF IT_ITAB-PROFL EQ 'V' OR IT_ITAB-DISPO EQ C_DISPO_M02.
*        MOVE : 'X' TO IT_ITAB-CONF2.
*      ENDIF.
*--- if 'HALB', move 'X' to QM indicator...
*---                        except the first digit has '6' or '7'...
*      IF IT_ITAB-MTART EQ 'HALB'.
*        IF NOT ( IT_ITAB-MATNR(1) EQ '6' OR IT_ITAB-MATNR(1) EQ '7' ).
*          MOVE : 'X' TO IT_ITAB-CONF4.
*        ENDIF.
*      ENDIF.
*--- if 'ROH' and source = 'K', then move 'X' to QM indicator...
*      IF IT_ITAB-MTART EQ 'ROH'.
*        IF IT_ITAB-PROFL EQ 'K'.
*          MOVE : 'X' TO IT_ITAB-CONF4.
*        ENDIF.
*      ENDIF.
*--- set Confirmed indicator
      IF NOT IT_ITAB-MSTDE IS INITIAL.
        MOVE : 'X' TO IT_ITAB-CONFT.
      ENDIF.
*---
      APPEND IT_ITAB.
      CLEAR : IT_ITAB, IT_TEMP.
    ENDLOOP.

  ELSE.
    MESSAGE S009 WITH 'Error Material Type'.
    EXIT.
  ENDIF.
  SORT IT_ITAB BY MATNR WERKS.
  LOOP AT IT_ITAB.
    AT NEW WERKS.
      L_FLAG = 'X'.
    ENDAT.
    IF L_FLAG = 'X'.
      L_FLAG = ' '.
      CONTINUE.
    ELSE.
      IT_ITAB-CONFD = 'X'.
      L_FLAG = ' '.
      MODIFY IT_ITAB TRANSPORTING CONFD.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  check_authority
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_AUTHORITY.
*---
  CLEAR : ZTMM_AUTH, IT_ZTMM_AUTH, IT_ZTMM_AUTH[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTMM_AUTH
           FROM ZTMM_AUTH
          WHERE UNAME EQ SY-UNAME
            AND TCODE EQ SY-TCODE
            AND LOEKZ EQ SPACE.

  IF IT_ZTMM_AUTH[] IS INITIAL.
    MESSAGE S077(S#) WITH SY-TCODE.
    STOP.
*    EXIT.
  ENDIF.

*---
  CLEAR : W_IND_MM, W_IND_PUR, W_IND_DEV, W_IND_QM.

  LOOP AT IT_ZTMM_AUTH.
    CASE IT_ZTMM_AUTH-FUNC1.
      WHEN C_BOM.
        MOVE : 'X' TO W_IND_BOM.
      WHEN C_MM.
        MOVE : 'X' TO W_IND_MM.
      WHEN C_PUR.
        MOVE : 'X' TO W_IND_PUR.
*      WHEN c_dev.
*        MOVE : 'X' TO w_ind_dev.
*      WHEN c_qm.
*        MOVE : 'X' TO w_ind_qm.
      WHEN C_HALB.
        MOVE : 'X' TO W_IND_HALB.
    ENDCASE.
  ENDLOOP.
  IF R_011 = 'X'.
    IF W_IND_BOM = 'X'.
      CLEAR: W_IND_MM,  W_IND_PUR.
    ELSE.
      MESSAGE S077(S#) WITH SY-TCODE.
      STOP.
    ENDIF.
  ENDIF.
  IF R_012 = 'X'.
    IF W_IND_MM = 'X'.
      CLEAR: W_IND_BOM,  W_IND_PUR.
    ELSE.
      MESSAGE S077(S#) WITH SY-TCODE.
      STOP.
    ENDIF.
  ENDIF.
  IF R_013 = 'X'.
    IF W_IND_PUR = 'X'.
      CLEAR: W_IND_BOM,  W_IND_MM.
    ELSE.
      MESSAGE S077(S#) WITH SY-TCODE.
      STOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_authority

*&---------------------------------------------------------------------*
*&      Module  status_scrcom  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_SCRCOM OUTPUT.
**---
  CASE SY-DYNNR.
    WHEN '9000'.
*      IF W_IND_BOM EQ SPACE.
*      CLEAR : IT_FCODE, IT_FCODE[].
*      MOVE : 'CREA' TO IT_FCODE-FCODE. APPEND IT_FCODE.
*      MOVE : 'DELE' TO IT_FCODE-FCODE. APPEND IT_FCODE.
*      SET PF-STATUS '9000' EXCLUDING IT_FCODE.
*      ELSEIF W_IND_MM EQ SPACE AND W_IND_PUR EQ SPACE.
**         AND w_ind_dev EQ space AND w_ind_qm EQ space.
      CLEAR : IT_FCODE, IT_FCODE[].
      MOVE : 'CONF' TO IT_FCODE-FCODE. APPEND IT_FCODE.
      MOVE : 'DELE' TO IT_FCODE-FCODE. APPEND IT_FCODE.
      SET PF-STATUS '9000' EXCLUDING IT_FCODE.
*      ELSE.
*        SET PF-STATUS '9000'.
*      ENDIF.
      SET TITLEBAR  '9000'.
  ENDCASE.
ENDMODULE.                 " status_scrcom  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
**---
  CASE SY-DYNNR.
    WHEN '9000'.
      CASE SY-UCOMM.
        WHEN 'BACK' OR 'EXIT' OR 'CANC'.
          LEAVE TO SCREEN 0.
*        WHEN 'DELE'.
*          CLEAR : w_save_okcode.
*          PERFORM delete_item.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_scrcom  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCRCOM INPUT.
**---
  MOVE : W_OKCODE TO W_SAVE_OKCODE.

  CLEAR : W_OKCODE.

  CASE SY-DYNNR.
    WHEN '9000'.
      CASE W_SAVE_OKCODE.
        WHEN 'CREA'.
          CLEAR : W_SAVE_OKCODE.
*          PERFORM CHECK_AUTHORITY.
          PERFORM CHANGE_MATERIAL_MASTER.
*---
          IF IT_ITAB[] IS INITIAL.
            MESSAGE S999 WITH TEXT-M01.
          ELSE.
            IT_TEMP[] = IT_ITAB[].
            LEAVE TO SCREEN 9000.
          ENDIF.
*        WHEN 'SAVE'.
*          CLEAR : W_SAVE_OKCODE.
*          PERFORM CHECK_INPUT_VALUE.
*          PERFORM SAVE_ROUTINE.
*          PERFORM CHECK_AUTHORITY.
**---
*          IF IT_ITAB[] IS INITIAL.
*            MESSAGE S999 WITH TEXT-M01.
*          ELSE.
*            IT_TEMP[] = IT_ITAB[].
*            LEAVE TO SCREEN 9000.
*          ENDIF.
        WHEN 'CONF'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM CHECK_INPUT_VALUE.
*          PERFORM SAVE_ROUTINE.
          PERFORM CHECK_AUTHORITY.
*---
          IF IT_ITAB[] IS INITIAL.
            MESSAGE S999 WITH TEXT-M01.
          ELSE.
            IT_TEMP[] = IT_ITAB[].
            LEAVE TO SCREEN 9000.
          ENDIF.
*        WHEN 'DELE'.
*          CLEAR : W_SAVE_OKCODE.
*          PERFORM DELETE_ITEM.
        WHEN 'SALL'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM SELECT_DESELECT_ALL_9000 USING 'X'.
        WHEN 'DALL'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM SELECT_DESELECT_ALL_9000 USING ' '.
        WHEN 'ASCE'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM ASCENDING_SORT.
        WHEN 'DESC'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM DESCENDING_SORT.
        WHEN 'FIND' OR 'FIND+'.
          PERFORM FIND_STRING.
          CLEAR : W_SAVE_OKCODE.
        WHEN 'P--' OR 'P-' OR 'P+' OR 'P++'.     " Page Scroll
          PERFORM TABLE_CONTROL_PAGE_SCROL USING W_SAVE_OKCODE
                                                 TC_9000-TOP_LINE
                                                 W_TOT_LINES
                                                 W_LOOPC.
          CLEAR : W_SAVE_OKCODE.
        WHEN 'GETM'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM GET_FROM_MATERIAL_MASTER.
        WHEN 'EXCE'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM SAVE_DATA_TO_EXCEL.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " user_command_scrcom  INPUT

*&---------------------------------------------------------------------*
*&      Form  select_deselect_all_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0421   text
*----------------------------------------------------------------------*
FORM SELECT_DESELECT_ALL_9000 USING    P_VALUE.
**---
  MOVE : P_VALUE TO IT_ITAB-W_SELECTED,
         P_VALUE TO IT_ITAB-CONFT.

  MODIFY IT_ITAB TRANSPORTING W_SELECTED CONFT WHERE MATNR NE SPACE.
*                                           AND CONF5 EQ SPACE.
ENDFORM.                    " select_deselect_all_9000

*&---------------------------------------------------------------------*
*&      Form  ascending_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASCENDING_SORT.
*---
  LOOP AT TC_9000-COLS INTO WA_TC9000.
    IF WA_TC9000-SELECTED = 'X'.
      SORT IT_ITAB BY (WA_TC9000-SCREEN-NAME+10) ASCENDING.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ascending_sort

*&---------------------------------------------------------------------*
*&      Form  descending_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DESCENDING_SORT.
*---
  LOOP AT TC_9000-COLS INTO WA_TC9000.
    IF WA_TC9000-SELECTED = 'X'.
      SORT IT_ITAB BY (WA_TC9000-SCREEN-NAME+10) DESCENDING.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " descending_sort

*&---------------------------------------------------------------------*
*&      Form  find_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_STRING.
**---
  IF W_SAVE_OKCODE EQ 'FIND'.
    MOVE : 1 TO W_POSITION.
  ELSEIF W_SAVE_OKCODE EQ 'FIND+'.
    W_POSITION = W_LOOP_FIRST + 1.
  ENDIF.

**---
  IF W_SAVE_OKCODE EQ 'FIND'.
    PERFORM POPUP_GET_VALUE(SAPFSFXX) USING    'FSTR' ' '
                                      CHANGING RSDXX-FINDSTR.
  ENDIF.

**---
  IF SY-UCOMM NE 'CANC'.
*---
    CLEAR : W_FOUND.
    IF SY-DYNNR EQ '9000'.
      LOOP AT IT_ITAB FROM W_POSITION.
        IF IT_ITAB CS RSDXX-FINDSTR OR IT_ITAB CP RSDXX-FINDSTR.
          MOVE : 'X' TO W_FOUND.
          MOVE : SY-TABIX TO W_FIND_POS.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF W_FOUND NE SPACE.
        MOVE : 1          TO TC_9000-CURRENT_LINE,
               W_FIND_POS TO TC_9000-TOP_LINE,
               W_FIND_POS TO W_LOOP_FIRST.
      ELSE.
        MESSAGE S042(E2) WITH RSDXX-FINDSTR.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " find_string

*&---------------------------------------------------------------------*
*&      Module  control_screen_scr9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CONTROL_SCREEN_SCR9000 OUTPUT.
*---
  LOOP AT SCREEN.
*---
    IF ZTMM_MARA-MTART EQ C_MTART_ROH.
      PERFORM CONTROL_BY_MTART_ROH.
    ELSEIF ZTMM_MARA-MTART EQ C_MTART_FERT OR
           ZTMM_MARA-MTART EQ C_MTART_HALB.
      PERFORM CONTROL_BY_MTART_FERTHALB.
    ENDIF.
*---
*    PERFORM control_required_field.
*---
    PERFORM CONTROL_BY_CONFIRM_STATUS.
*---
*    PERFORM COLUMN_INPUT TABLES TC_9000-COLS
*                             USING SCREEN-NAME W_RADIO.
*
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " control_screen_scr9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  display_data_scr9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_DATA_SCR9000 OUTPUT.
**---
  IF W_ERROR_CHECK NE SPACE.
    SET CURSOR FIELD W_FIELD_NAME LINE 1.
    CLEAR : W_ERROR_CHECK.
  ENDIF.

**---
  IF SY-STEPL EQ 1.
    CALL FUNCTION 'ME_GET_TC_LINES'
         EXPORTING
              IM_LINES_TOTAL    = W_TOT_LINES
              IM_LINES_PER_PAGE = SY-LOOPC
              IM_TOP_LINE       = TC_9000-TOP_LINE
         IMPORTING
              EX_TC_LINES       = TC_9000-LINES
         EXCEPTIONS
              OTHERS            = 1.
  ENDIF.

**---
  CLEAR : IT_ITAB.

  READ TABLE IT_ITAB INDEX TC_9000-CURRENT_LINE.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_ITAB TO ZTMM_MARA.
    MOVE : IT_ITAB-W_SELECTED  TO W_SELECTED,
           C_LGNUM_P01         TO ZTMM_MARA-LGNUM.
    PERFORM GET_MATERIAL_DESC USING ZTMM_MARA-MATNR.
  ELSE.
    EXIT FROM STEP-LOOP.
  ENDIF.

**---
  MOVE : SY-LOOPC TO W_LOOPC.
ENDMODULE.                 " display_data_scr9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  input_data_modify_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_DATA_MODIFY_SCR9000 INPUT.
**---

  CLEAR : IT_ITAB.

  READ TABLE IT_ITAB INDEX TC_9000-CURRENT_LINE.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING ZTMM_MARA TO IT_ITAB.
    MOVE : W_SELECTED            TO IT_ITAB-W_SELECTED.
*--- valuation class
    PERFORM VALUATION_CLASS.
*--- source change
    CLEAR : IT_TEMP.
    READ TABLE IT_TEMP WITH KEY MATNR = IT_ITAB-MATNR
                                WERKS = IT_ITAB-WERKS.
    IF IT_TEMP-PROFL NE ZTMM_MARA-PROFL AND
       IT_ITAB-MTART EQ C_MTART_ROH.
      CLEAR : IT_ITAB-CONF1, IT_ITAB-CONF2, IT_ITAB-CONF3,
              IT_ITAB-CONF4.
*      CLEAR: IT_ITAB-CONFT.
    ENDIF.
*---
    IF NOT IT_ITAB-NTGEW IS INITIAL.
      MOVE : C_GEWEI_KG TO IT_ITAB-GEWEI,
             IT_ITAB-NTGEW TO IT_ITAB-BRGEW.
    ENDIF.
*---
    MODIFY IT_ITAB INDEX TC_9000-CURRENT_LINE.
  ENDIF.
ENDMODULE.                 " input_data_modify_scr9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  control_by_mtart_roh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONTROL_BY_MTART_ROH.
*---
  IF SCREEN-NAME EQ 'ZTMM_MARA-CONFT' AND ZTMM_MARA-MSTDE IS INITIAL.
    SCREEN-INPUT = 1.
  ENDIF.

  IF W_IND_BOM NE SPACE.
    IF SCREEN-GROUP1 EQ C_BOM OR
        SCREEN-GROUP1 EQ C_SH.
      SCREEN-INPUT = 1.
    ELSE.
      SCREEN-INPUT = 0.
    ENDIF.
  ENDIF.
*---
*  IF W_IND_MM NE SPACE.
*    IF SCREEN-GROUP1 EQ C_MM.   "  OR SCREEN-NAME EQ 'ZTMM_MARA-CONF1'.
*      SCREEN-INPUT = 1.
*    ENDIF.
*  ENDIF.
**---
*  IF W_IND_PUR NE SPACE.
*    IF SCREEN-GROUP1 EQ C_PUR.  " OR SCREEN-NAME EQ 'ZTMM_MARA-CONF2'.
*      SCREEN-INPUT = 1.
*    ENDIF.
*  ENDIF.

**
  IF W_IND_MM NE SPACE.
    IF SCREEN-GROUP1 EQ C_ST
      OR SCREEN-GROUP1 EQ C_SH.   "OR SCREEN-NAME EQ 'ZTMM_MARA-CONF1'.
      SCREEN-INPUT = 1.
    ELSE.
      SCREEN-INPUT = 0.
    ENDIF.
  ENDIF.
*---
  IF W_IND_PUR NE SPACE.
    IF SCREEN-GROUP1 EQ C_WM
      OR SCREEN-GROUP1 EQ C_SH.  " OR SCREEN-NAME EQ 'ZTMM_MARA-CONF2'.
      SCREEN-INPUT = 1.
    ELSE.
      SCREEN-INPUT = 0.

    ENDIF.
  ENDIF.


*
*---
*  IF w_ind_dev NE space.
*    IF screen-group1 EQ c_dev OR screen-name EQ 'ZTMM_MARA-CONF3'.
*      screen-input = 1.
*    ENDIF.
*  ENDIF.
*---
*  IF w_ind_qm NE space.
*    IF screen-group1 EQ c_qm OR screen-name EQ 'ZTMM_MARA-CONF4'.
*      screen-input = 1.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " control_by_mtart_roh

*&---------------------------------------------------------------------*
*&      Form  control_by_mtart_ferthalb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONTROL_BY_MTART_FERTHALB.
*---
  IF W_IND_BOM NE SPACE.
    IF SCREEN-GROUP1 EQ C_BOM.
      SCREEN-INPUT = 1.
    ENDIF.
  ENDIF.
*---
  IF W_IND_HALB NE SPACE.
    IF SCREEN-NAME EQ 'ZTMM_MARA-NTGEW'.     " Net Weight
      SCREEN-INPUT = 1.
    ENDIF.
  ENDIF.

  IF SCREEN-NAME EQ 'ZTMM_MARA-CONFT' AND ZTMM_MARA-MSTDE IS INITIAL.
    SCREEN-INPUT = 1.
  ENDIF.

*---
*  IF w_ind_qm NE space.
*    IF screen-group1 EQ c_qm OR screen-name EQ 'ZTMM_MARA-CONF4'.
*      screen-input = 1.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " control_by_mtart_ferthalb

*&---------------------------------------------------------------------*
*&      Form  control_by_confirm_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONTROL_BY_CONFIRM_STATUS.
*---
*  IF ZTMM_MARA-MTART EQ C_MTART_ROH.
*    IF ZTMM_MARA-CONF1 NE SPACE.     " MM
*      IF SCREEN-GROUP1 EQ C_MM.
*        SCREEN-INPUT = 0.
*      ENDIF.
*    ENDIF.
*    IF ZTMM_MARA-CONF2 NE SPACE.     " PUR
*      IF SCREEN-GROUP1 EQ C_PUR.
*        SCREEN-INPUT = 0.
*      ENDIF.
*    ENDIF.
*
**    screen-active = 0.
**    IF ztmm_mara-conf3 NE space.     " DEV
**      IF screen-group1 EQ c_dev.
**        screen-input = 0.
**      ENDIF.
**    ENDIF.
**    IF ztmm_mara-conf4 NE space.     " QM
**      IF screen-group1 EQ c_qm.
**        screen-input = 0.
**      ENDIF.
**    ENDIF.
*  ELSEIF ZTMM_MARA-MTART EQ C_MTART_FERT OR
*         ZTMM_MARA-MTART EQ C_MTART_HALB.
**    IF ztmm_mara-conf4 NE space.     " QM
**      IF screen-group1 EQ c_qm.
**        screen-input = 0.
**      ENDIF.
**    ENDIF.
*  ENDIF.

*---
*  IF ZTMM_MARA-CONFD = 'X'.
*    SCREEN-INPUT = 0.
**    SCREEN-INTENSIFIED = 1.
*  ENDIF.

  IF ZTMM_MARA-CONFT NE SPACE AND ZTMM_MARA-MSTDE > 0.
    SCREEN-INPUT = 0.
  ELSE.
    IF SCREEN-NAME EQ 'ZTMM_MARA-CONFT'.
      SCREEN-INPUT = 1.
    ENDIF.
*    SCREEN-INTENSIFIED = 1.
  ENDIF.

*  IF ZTMM_MARA-CONF5 NE SPACE.
*    SCREEN-INPUT = 0.
**    SCREEN-INTENSIFIED = 1.
**    screen-active = 0.
*  ENDIF.
ENDFORM.                    " control_by_confirm_status

*&---------------------------------------------------------------------*
*&      Module  check_input_data_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_DATA_SCR9000 INPUT.
*---
*  CHECK W_SELECTED NE SPACE.
*  CHECK W_OKCODE NE 'DELE'.
*  CHECK W_OKCODE NE 'GETM'.

  CHECK ZTMM_MARA-CONFT NE SPACE.

  CHECK W_OKCODE NE 'EXCE'.
*--- BOM Check
** Changed by Furong on 03/05/09, requested by Mane
*  IF W_IND_BOM NE SPACE.
**--- Source
*    IF ZTMM_MARA-PROFL EQ SPACE AND ZTMM_MARA-MTART EQ C_MTART_ROH.
*      SET CURSOR FIELD 'ZTMM_MARA-PROFL' LINE SY-STEPL.
*      MESSAGE E999 WITH TEXT-M02.
*    ENDIF.
*  ENDIF.
*** End of change on 03/05/09
*--- MM Check
  IF W_IND_MM NE SPACE.
*--- Storage Location Check
    IF ZTMM_MARA-LGORT EQ SPACE AND ZTMM_MARA-MTART EQ C_MTART_ROH
       AND ZTMM_MARA-DISPO NE C_DISPO_M02.
      SET CURSOR FIELD 'ZTMM_MARA-LGORT' LINE SY-STEPL.
      MESSAGE E999 WITH TEXT-M03.
    ENDIF.
*--- Storage Type Check
*    IF ztmm_mara-lgtyp EQ space.
*      SET CURSOR FIELD 'ZTMM_MARA-LGTYP' LINE sy-stepl.
*      MESSAGE e999 WITH text-m04.
*    ENDIF.
*--- Material Group Check
    IF ZTMM_MARA-MATKL EQ SPACE AND ZTMM_MARA-MTART EQ C_MTART_ROH.
      SET CURSOR FIELD 'ZTMM_MARA-MATKL' LINE SY-STEPL.
      MESSAGE E999 WITH TEXT-M05.
    ENDIF.
*--- GenItem CatGroup Check
*    IF ztmm_mara-mtpos_mara EQ space.
*      SET CURSOR FIELD 'ZTMM_MARA-MTPOS_MARA' LINE sy-stepl.
*      MESSAGE e999 WITH text-m06.
*    ENDIF.
*--- MRP Controller Check
    IF ZTMM_MARA-DISPO EQ SPACE AND ZTMM_MARA-MTART EQ C_MTART_ROH.
      SET CURSOR FIELD 'ZTMM_MARA-DISPO' LINE SY-STEPL.
      MESSAGE E999 WITH TEXT-M07.
    ENDIF.
*--- Lot Size Check
    IF ZTMM_MARA-DISLS EQ SPACE AND ZTMM_MARA-MTART EQ C_MTART_ROH.
      SET CURSOR FIELD 'ZTMM_MARA-DISLS' LINE SY-STEPL.
      MESSAGE E999 WITH TEXT-M08.
    ENDIF.
*--- Default Supply Area Check
    IF ZTMM_MARA-LGORT EQ C_LGORT_P400 AND
       ZTMM_MARA-MTART EQ C_MTART_ROH.
      IF ZTMM_MARA-VSPVB EQ SPACE.
        SET CURSOR FIELD 'ZTMM_MARA-VSPVB' LINE SY-STEPL.
        MESSAGE E999 WITH TEXT-M22.
      ENDIF.
    ENDIF.
  ENDIF.

*--- Parts Development Check
  IF W_IND_DEV NE SPACE.
*--- Net Weight Check
    IF ZTMM_MARA-NTGEW EQ SPACE AND ZTMM_MARA-DISPO NE C_DISPO_M02.
      SET CURSOR FIELD 'ZTMM_MARA-NTGEW' LINE SY-STEPL.
      MESSAGE E999 WITH TEXT-M09.
    ENDIF.
*--- Purchasing Group Check
    IF ZTMM_MARA-EKGRP EQ SPACE AND ZTMM_MARA-MTART EQ C_MTART_ROH.
      SET CURSOR FIELD 'ZTMM_MARA-EKGRP' LINE SY-STEPL.
      MESSAGE E999 WITH TEXT-M10.
    ENDIF.
  ENDIF.

*--- PUR Check
  IF W_IND_PUR NE SPACE.
*--- HTS Number & Country Check
    IF ZTMM_MARA-PROFL EQ C_PROFL_K AND ZTMM_MARA-MTART EQ C_MTART_ROH
       AND ZTMM_MARA-DISPO NE C_DISPO_M02.
      IF ZTMM_MARA-STAWN EQ SPACE.
        SET CURSOR FIELD 'ZTMM_MARA-STAWN' LINE SY-STEPL.
        MESSAGE E999 WITH TEXT-M17.
      ENDIF.
      IF ZTMM_MARA-HERKL EQ SPACE.
        SET CURSOR FIELD 'ZTMM_MARA-HERKL' LINE SY-STEPL.
        MESSAGE E999 WITH TEXT-M18.
      ENDIF.
    ENDIF.
  ENDIF.

*--- HALB Check
*--- insert by stlim (2004/10/26)
  IF W_IND_HALB NE SPACE.
    IF ZTMM_MARA-MTART EQ C_MTART_HALB OR
       ZTMM_MARA-MTART EQ C_MTART_FERT.
      IF ZTMM_MARA-NTGEW IS INITIAL.
        SET CURSOR FIELD 'ZTMM_MARA-NTGEW' LINE SY-STEPL.
        MESSAGE E999 WITH TEXT-M09.
      ENDIF.
    ENDIF.
  ENDIF.
*--- end of insert

*--- blocked by stlim (2004/10/26)
*  IF w_ind_halb NE space.
*    IF ztmm_mara-ntgew IS INITIAL.
*      SET CURSOR FIELD 'ZTMM_MARA-NTGEW' LINE sy-stepl.
*      MESSAGE e999 WITH text-m09.
*    ENDIF.
*  ENDIF.
*--- end of block
ENDMODULE.                 " check_input_data_scr9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  get_control_lines_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CONTROL_LINES_9000 OUTPUT.
**---
  DESCRIBE TABLE IT_ITAB LINES W_TOT_LINES.
ENDMODULE.                 " get_control_lines_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  control_required_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONTROL_REQUIRED_FIELD.
**---
  IF W_IND_BOM NE SPACE.
    IF SCREEN-GROUP2 EQ C_BOM.
      SCREEN-REQUIRED = 1.
    ENDIF.
  ENDIF.
  IF W_IND_MM NE SPACE.
    IF SCREEN-GROUP2 EQ C_MM.
      SCREEN-REQUIRED = 1.
    ENDIF.
  ENDIF.
*  IF w_ind_dev NE space.
*    IF screen-group2 EQ c_dev.
*      screen-required = 1.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " control_required_field

*&---------------------------------------------------------------------*
*&      Module  control_column_hide_scr9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CONTROL_COLUMN_HIDE_SCR9000 OUTPUT.
*---
  LOOP AT SCREEN.
    PERFORM COLUMN_INVISIBLE TABLES TC_9000-COLS
                             USING SCREEN-NAME '1'.
  ENDLOOP.
ENDMODULE.                 " control_column_hide_scr9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  column_invisible
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_9000_COLS  text
*      -->P_SCREEN_NAME  text
*      -->P_1353   text
*----------------------------------------------------------------------*
FORM COLUMN_INVISIBLE TABLES   P_TC_9000_COLS STRUCTURE WA_TAB
                      USING    P_SCREEN_NAME
                               P_INVISIBLE.
*---
  CHECK P_SCREEN_NAME EQ 'ZTMM_MARA-LGNUM' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-MTART' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-MSTAE' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-MSTDE' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-BRGEW' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-UMREN' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-LGPRO' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-LGFSB' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-XMCNG' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-LTKZA' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-LTKZE' OR
*       P_SCREEN_NAME EQ 'ZTMM_MARA-CONF5' OR
        P_SCREEN_NAME EQ 'ZTMM_MARA-CONFD'.

  READ TABLE P_TC_9000_COLS WITH KEY SCREEN-NAME = P_SCREEN_NAME.

  IF SY-SUBRC = 0.
    MOVE : P_INVISIBLE TO P_TC_9000_COLS-INVISIBLE.
    MODIFY P_TC_9000_COLS INDEX SY-TABIX.
  ENDIF.
ENDFORM.                    " column_invisible

*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_VALUE.
*---
  READ TABLE IT_ITAB WITH KEY CONFT = 'X'.

  IF SY-SUBRC NE 0.
    MESSAGE E999 WITH TEXT-M30.
  ENDIF.

*---
  LOOP AT IT_ITAB WHERE CONFT NE SPACE.
*                    AND CONF5 EQ SPACE.
**--- BOM Check
    IF W_IND_BOM NE SPACE.
      PERFORM CHECK_INPUT_VALUE_BOM.
    ENDIF.
**--- MM Check
    IF W_IND_MM NE SPACE.
      PERFORM CHECK_INPUT_VALUE_MM.
    ENDIF.
**--- PUR Check
    IF W_IND_PUR NE SPACE.
      PERFORM CHECK_INPUT_VALUE_PUR.
    ENDIF.

*--- Valuation Class
    PERFORM VALUATION_CLASS.
*--- Warehouse Number
    IF IT_ITAB-LGTYP NE SPACE.
      MOVE : C_LGNUM_P01 TO IT_ITAB-LGNUM.
    ENDIF.
*--- Gross Weight
    MOVE : IT_ITAB-NTGEW TO IT_ITAB-BRGEW.
*--- Unit of Measure (additional)
    IF IT_ITAB-MEINH NE SPACE AND IT_ITAB-UMREZ NE SPACE.
      MOVE : '1' TO IT_ITAB-UMREN.
    ENDIF.
*--- Issue Storage Location & Storage Location for EP
    MOVE : IT_ITAB-LGORT TO IT_ITAB-LGPRO,
           IT_ITAB-LGORT TO IT_ITAB-LGFSB.
*--- Negative Stocks in Plant
*    IF IT_ITAB-TEMPB EQ C_TEMPB_11.
*s__by Paul
    IF IT_ITAB-TEMPB EQ '1' OR
       IT_ITAB-TEMPB EQ '2'.
      MOVE : 'X' TO IT_ITAB-XMCNG.
    ENDIF.
*--- Stock Removal & Stock Placement
    IF IT_ITAB-LGTYP EQ SPACE.
      MOVE : SPACE TO IT_ITAB-LTKZA,
             SPACE TO IT_ITAB-LTKZE.
    ELSEIF IT_ITAB-LGTYP EQ C_LGTYP_422.
      MOVE : C_LTKZA_003 TO IT_ITAB-LTKZA,
             C_LTKZA_003 TO IT_ITAB-LTKZE.
    ELSE.
      MOVE : C_LTKZA_004 TO IT_ITAB-LTKZA,
             C_LTKZA_004 TO IT_ITAB-LTKZE.
    ENDIF.
*--- if source = 'V', then HTS & country : initialize
*---                       PUR confirmation ind : 'X'
*    IF IT_ITAB-PROFL EQ C_PROFL_V OR IT_ITAB-DISPO = C_DISPO_M02.
*      CLEAR : IT_ITAB-STAWN, IT_ITAB-HERKL.
*      MOVE : 'X' TO IT_ITAB-CONF2.
*    ENDIF.
**--- if 'HALB' and source = 'K', then QM confirmation ind : 'X'
*    IF it_itab-mtart EQ c_mtart_halb.
*      IF it_itab-profl EQ c_profl_k.
*        MOVE : 'X' TO it_itab-conf4.
*      ENDIF.
*    ENDIF.
*--- if 'ROH' and source = 'K', then QM confirmation ind : 'X'
*    IF IT_ITAB-MTART EQ C_MTART_ROH.
*      IF IT_ITAB-PROFL EQ C_PROFL_K.
*        MOVE : 'X' TO IT_ITAB-CONF4.
*      ENDIF.
*    ENDIF.
*---
    MODIFY IT_ITAB.
    CLEAR : IT_ITAB.
  ENDLOOP.

*--- Special Procurement : '40' => copy Record
*  CLEAR : IT_COPY, IT_COPY[].
*
*  LOOP AT IT_ITAB WHERE W_SELECTED NE SPACE
*                    AND CONF5 EQ SPACE      " completed
*                    AND CONF1 NE SPACE      " MM
*                    AND SOBSL EQ C_SOBSL_40.
*    MOVE-CORRESPONDING IT_ITAB TO IT_COPY.
*    CASE IT_ITAB-WERKS.
*      WHEN C_WERKS_P001.
*        MOVE : SPACE        TO IT_COPY-W_SELECTED,
*               C_WERKS_E001 TO IT_COPY-WERKS,
*               C_LGORT_E110 TO IT_COPY-LGORT,
*               SPACE        TO IT_COPY-LGNUM,
*               SPACE        TO IT_COPY-LGTYP,
*               SPACE        TO IT_COPY-SOBSL,
*               C_LGORT_E110 TO IT_COPY-LGPRO,
*               C_LGORT_E110 TO IT_COPY-LGFSB,
*               SPACE        TO IT_COPY-VSPVB,
*               SPACE        TO IT_COPY-LTKZA,
*               SPACE        TO IT_COPY-LTKZE,
*               SPACE        TO IT_COPY-LGPLA,
*               SPACE        TO IT_COPY-RDMNG,
*               SPACE        TO IT_COPY-CONF1,
*               SPACE        TO IT_COPY-CONF2,
*               SPACE        TO IT_COPY-CONF3,
*               SPACE        TO IT_COPY-CONF4,
*               SPACE        TO IT_COPY-CONF5,
*               SPACE        TO IT_COPY-MESSA.
*      WHEN C_WERKS_E001.
*        MOVE : SPACE        TO IT_COPY-W_SELECTED,
*               C_WERKS_P001 TO IT_COPY-WERKS,
*               C_LGORT_P400 TO IT_COPY-LGORT,
*               C_LGNUM_P01  TO IT_COPY-LGNUM,
*               C_LGTYP_422  TO IT_COPY-LGTYP,
*               SPACE        TO IT_COPY-SOBSL,
*               C_LGORT_P400 TO IT_COPY-LGPRO,
*               C_LGORT_P400 TO IT_COPY-LGFSB,
*               SPACE        TO IT_COPY-VSPVB,
*               SPACE        TO IT_COPY-RGEKZ,
*               C_LTKZA_003  TO IT_COPY-LTKZA,
*               C_LTKZA_003  TO IT_COPY-LTKZE,
*               SPACE        TO IT_COPY-LGPLA,
*               SPACE        TO IT_COPY-RDMNG,
*               SPACE        TO IT_COPY-CONF1,
*               SPACE        TO IT_COPY-CONF2,
*               SPACE        TO IT_COPY-CONF3,
*               SPACE        TO IT_COPY-CONF4,
*               SPACE        TO IT_COPY-CONF5,
*               SPACE        TO IT_COPY-MESSA.
*    ENDCASE.
**--- existence check
*    CLEAR : ZTMM_MARA.
*    SELECT SINGLE MATNR INTO ZTMM_MARA-MATNR
*                        FROM ZTMM_MARA
*                       WHERE MATNR EQ IT_COPY-MATNR
*                         AND WERKS EQ IT_COPY-WERKS.
*    CHECK SY-SUBRC NE 0.
**---
*    APPEND IT_COPY.
*    CLEAR : IT_ITAB, IT_COPY.
*  ENDLOOP.
ENDFORM.                    " check_input_value

*&---------------------------------------------------------------------*
*&      Form  check_input_value_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_VALUE_BOM.
*---
  IF IT_ITAB-MTART EQ C_MTART_ROH.
*  CHECK it_itab-mtart EQ c_mtart_roh.

*--- Source Check
    IF NOT ( IT_ITAB-PROFL EQ C_PROFL_K OR IT_ITAB-PROFL EQ C_PROFL_V ).
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M14 'ZTMM_MARA-PROFL'.
    ELSEIF IT_ITAB-PROFL EQ SPACE.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M02 'ZTMM_MARA-PROFL'.
    ENDIF.

  ELSE.

    IF NOT ( IT_ITAB-PROFL EQ C_PROFL_M OR IT_ITAB-PROFL EQ SPACE ).
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M36 'ZTMM_MARA-PROFL'.
    ENDIF.

  ENDIF.
ENDFORM.                    " check_input_value_bom

*&---------------------------------------------------------------------*
*&      Form  check_input_value_mm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_VALUE_MM.
*---
  CHECK IT_ITAB-MTART EQ C_MTART_ROH.

*--- Storage Location Check
*  IF it_itab-werks EQ c_werks_p001.
*    IF NOT ( it_itab-lgort EQ c_lgort_p400 OR
*             it_itab-lgort EQ c_lgort_p500 ).
*      PERFORM show_message_top_line USING text-m11 'ZTMM_MARA-LGORT'.
*    ENDIF.
*  ELSEIF it_itab-werks EQ c_werks_e001.
*    IF NOT ( it_itab-lgort EQ c_lgort_e100 OR
*             it_itab-lgort EQ c_lgort_e110 ).
*      PERFORM show_message_top_line USING text-m12 'ZTMM_MARA-LGORT'.
*    ENDIF.
*  ELSE.
*    PERFORM show_message_top_line USING text-m03 'ZTMM_MARA-LGORT'.
*  ENDIF.
*--- Storage Type Check
  IF IT_ITAB-LGORT EQ C_LGORT_P400.
    IF NOT ( IT_ITAB-LGTYP EQ C_LGTYP_422 OR
             IT_ITAB-LGTYP EQ C_LGTYP_431 OR
             IT_ITAB-LGTYP EQ C_LGTYP_432 OR
             IT_ITAB-LGTYP EQ C_LGTYP_433 OR
             IT_ITAB-LGTYP EQ C_LGTYP_434 OR
             IT_ITAB-LGTYP EQ C_LGTYP_435 OR
             IT_ITAB-LGTYP EQ C_LGTYP_436 ).
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M13 'ZTMM_MARA-LGTYP'.
    ENDIF.
  ELSE.
    IF IT_ITAB-LGTYP NE SPACE.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M04 'ZTMM_MARA-LGTYP'.
    ENDIF.
  ENDIF.
*--- Material Group Check
  IF IT_ITAB-PROFL EQ C_PROFL_K.
    IF NOT ( IT_ITAB-MATKL EQ C_MATKL_NF_KD    OR
             IT_ITAB-MATKL EQ C_MATKL_NF_KD_EN OR
             IT_ITAB-MATKL EQ C_MATKL_NF_KD_TM OR
             IT_ITAB-MATKL EQ C_MATKL_CM_KD    OR
             IT_ITAB-MATKL EQ C_MATKL_CM_KD_EN OR
             IT_ITAB-MATKL EQ C_MATKL_CM_KD_TM OR
             IT_ITAB-MATKL EQ C_MATKL_CO_KD    OR
             IT_ITAB-MATKL EQ C_MATKL_CO_KD_EN OR
             IT_ITAB-MATKL EQ C_MATKL_CO_KD_TM ).
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M05 'ZTMM_MARA-MATKL'.
    ENDIF.
  ELSEIF IT_ITAB-PROFL EQ C_PROFL_V.
    IF NOT ( IT_ITAB-MATKL EQ C_MATKL_NF_LP OR
             IT_ITAB-MATKL EQ C_MATKL_CM_LP OR
             IT_ITAB-MATKL EQ C_MATKL_CO_LP ).
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M05 'ZTMM_MARA-MATKL'.
    ENDIF.
  ENDIF.
*--- General Item Category Group
  IF NOT ( IT_ITAB-MTPOS_MARA EQ C_MTPOS_MARA_ZNOR OR
           IT_ITAB-MTPOS_MARA EQ SPACE ).
    PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M06 'ZTMM_MARA-MTPOS_MARA'.
  ENDIF.
  IF IT_ITAB-LGORT EQ C_LGORT_P400.
    IF IT_ITAB-MTPOS_MARA EQ C_MTPOS_MARA_ZNOR.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M06
                                          'ZTMM_MARA-MTPOS_MARA'.
    ENDIF.
  ENDIF.
*--- Case & QpC Check
  IF NOT ( ( IT_ITAB-MEINH EQ SPACE AND IT_ITAB-UMREZ EQ SPACE ) OR
           ( IT_ITAB-MEINH NE SPACE AND IT_ITAB-UMREZ NE SPACE ) ).
    PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M15 'ZTMM_MARA-MEINH'.
  ENDIF.
*--- JIT Schedule Indicator Check
  IF NOT ( IT_ITAB-FABKZ EQ '1' OR IT_ITAB-FABKZ EQ SPACE ).
    PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M16 'ZTMM_MARA-FABKZ'.
  ELSE.
    IF IT_ITAB-FABKZ EQ '1'.
*    IF IT_ITAB-TEMPB EQ C_TEMPB_11.
*s__by Paul
      IF IT_ITAB-TEMPB EQ '1' OR
        IT_ITAB-TEMPB EQ '2'.
        PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M16 'ZTMM_MARA-FABKZ'.
      ENDIF.
      IF IT_ITAB-PROFL EQ C_PROFL_K.
        PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M16 'ZTMM_MARA-FABKZ'.
      ENDIF.
    ENDIF.
  ENDIF.
*--- MRP controller check
  IF IT_ITAB-DISPO EQ C_DISPO_001.
    PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M37 'ZTMM_MARA-DISPO'.
  ENDIF.
*--- Lot Size Check
  IF IT_ITAB-DISLS EQ C_DISLS_EX.
    IF IT_ITAB-PROFL EQ C_PROFL_K.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M20 'ZTMM_MARA-DISLS'.
    ENDIF.
  ENDIF.
  IF IT_ITAB-DISLS EQ C_DISLS_PK.
    IF IT_ITAB-PROFL EQ C_PROFL_K.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M20 'ZTMM_MARA-DISLS'.
    ENDIF.
  ENDIF.
  IF IT_ITAB-DISLS EQ C_DISLS_WB.
    IF IT_ITAB-PROFL EQ C_PROFL_V.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M20 'ZTMM_MARA-DISLS'.
    ENDIF.
  ENDIF.
*--- Special Procurement Check
  IF NOT ( IT_ITAB-SOBSL EQ C_SOBSL_40 OR IT_ITAB-SOBSL EQ SPACE ).
    PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M21 'ZTMM_MARA-SOBSL'.
  ENDIF.
*--- Default Supply Area Check
  IF IT_ITAB-WERKS EQ C_WERKS_P001.
    IF IT_ITAB-LGORT EQ C_LGORT_P400.
      IF NOT ( IT_ITAB-VSPVB EQ C_VSPVB_B1 OR
               IT_ITAB-VSPVB EQ C_VSPVB_P3 OR
               IT_ITAB-VSPVB EQ C_VSPVB_T1 OR
               IT_ITAB-VSPVB EQ C_VSPVB_T2 OR
               IT_ITAB-VSPVB EQ C_VSPVB_T3 OR
               IT_ITAB-VSPVB EQ C_VSPVB_C1 OR
               IT_ITAB-VSPVB EQ C_VSPVB_C2 OR
               IT_ITAB-VSPVB EQ C_VSPVB_F1 OR
               IT_ITAB-VSPVB EQ C_VSPVB_F2 OR
               IT_ITAB-VSPVB EQ C_VSPVB_F3 OR
               IT_ITAB-VSPVB EQ C_VSPVB_F4 OR
               IT_ITAB-VSPVB EQ C_VSPVB_OK ).
        PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M22 'ZTMM_MARA-VSPVB'.
      ENDIF.
    ELSEIF IT_ITAB-LGORT EQ C_LGORT_P500.
      IF NOT ( IT_ITAB-VSPVB EQ C_VSPVB_T1S OR
               IT_ITAB-VSPVB EQ C_VSPVB_T2S OR
               IT_ITAB-VSPVB EQ C_VSPVB_T3S OR
               IT_ITAB-VSPVB EQ C_VSPVB_C1S OR
               IT_ITAB-VSPVB EQ C_VSPVB_C2S OR
               IT_ITAB-VSPVB EQ C_VSPVB_F1S OR
               IT_ITAB-VSPVB EQ C_VSPVB_F2S OR
               IT_ITAB-VSPVB EQ C_VSPVB_F3S OR
               IT_ITAB-VSPVB EQ C_VSPVB_F4S OR
               IT_ITAB-VSPVB EQ C_VSPVB_ENG OR
               IT_ITAB-VSPVB EQ SPACE ).
        PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M22 'ZTMM_MARA-VSPVB'.
      ENDIF.
    ENDIF.
** for E002
*      ELSEIF IT_ITAB-WERKS EQ C_WERKS_E001
  ELSEIF IT_ITAB-WERKS EQ C_WERKS_E001 or
     IT_ITAB-WERKS EQ C_WERKS_E002.
** end e002
    IF IT_ITAB-VSPVB NE SPACE.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M22 'ZTMM_MARA-VSPVB'.
    ENDIF.
  ENDIF.
*--- Backflush Check
  IF NOT ( IT_ITAB-RGEKZ EQ '1' OR IT_ITAB-RGEKZ EQ SPACE ).
    PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M23 'ZTMM_MARA-RGEKZ'.
  ELSEIF IT_ITAB-RGEKZ EQ '1'.
    IF IT_ITAB-DISPO EQ C_DISPO_M02.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M33 'ZTMM_MARA-RGEKZ'.
    ENDIF.
    IF IT_ITAB-MATKL CP '++++++EN' OR IT_ITAB-MATKL CP '++++++TM'.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M34 'ZTMM_MARA-RGEKZ'.
    ENDIF.
*    IF it_itab-dispo EQ c_dispo_m02 OR
*     ( it_itab-matkl CP '++++++EN' OR it_itab-matkl CP '++++++TM' ).
*      PERFORM show_message_top_line USING text-m23 'ZTMM_MARA-RGEKZ'.
*    ENDIF.
  ENDIF.
*--- Planned Delivery Time Check
  IF IT_ITAB-PROFL EQ C_PROFL_K.
    IF IT_ITAB-PLIFZ EQ SPACE  AND IT_ITAB-DISPO <> C_DISPO_M02.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M24 'ZTMM_MARA-PLIFZ'.
    ENDIF.
  ENDIF.
*--- Planned Delivery Calendar
  IF IT_ITAB-DISLS EQ C_DISLS_PK.
    IF IT_ITAB-MRPPP EQ SPACE.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M25 'ZTMM_MARA-MRPPP'.
    ENDIF.
  ENDIF.
*--- Backflush Cycle Check
*S__By Paul
* IF NOT ( IT_ITAB-TEMPB EQ C_TEMPB_11 OR IT_ITAB-TEMPB EQ C_TEMPB_12 OR
*                                               IT_ITAB-TEMPB EQ SPACE )
*.
      IF  NOT ( IT_ITAB-TEMPB EQ '1' OR
                IT_ITAB-TEMPB EQ '2' OR
                IT_ITAB-TEMPB EQ '3' OR
                IT_ITAB-TEMPB EQ '4' OR
                IT_ITAB-TEMPB EQ '9' OR
                IT_ITAB-TEMPB EQ SPACE  ).

    PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M26 'ZTMM_MARA-TEMPB'.
*  ELSEIF IT_ITAB-TEMPB EQ C_TEMPB_11.
  ELSEIF IT_ITAB-TEMPB EQ '1' or
         IT_ITAB-TEMPB EQ '2'.
    IF IT_ITAB-LGORT NE C_LGORT_P500.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M26 'ZTMM_MARA-TEMPB'.
    ENDIF.
  ENDIF.
*--- Shop Check
  IF NOT ( IT_ITAB-RAUBE EQ C_RAUBE_11 OR
           IT_ITAB-RAUBE EQ C_RAUBE_12 OR
           IT_ITAB-RAUBE EQ C_RAUBE_13 OR
           IT_ITAB-RAUBE EQ C_RAUBE_14 OR
           IT_ITAB-RAUBE EQ SPACE ).
    PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M27 'ZTMM_MARA-RAUBE'.
  ENDIF.
*--- ABC Indicator Check
  IF NOT ( IT_ITAB-ABCIN EQ C_ABCIN_A OR
           IT_ITAB-ABCIN EQ C_ABCIN_B OR
           IT_ITAB-ABCIN EQ C_ABCIN_C OR
           IT_ITAB-ABCIN EQ C_ABCIN_D OR
           IT_ITAB-ABCIN EQ SPACE ).
    PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M28 'ZTMM_MARA-ABCIN'.
  ENDIF.
*--- Storage Bin Check
  IF IT_ITAB-LGTYP EQ SPACE.
    IF IT_ITAB-LGPLA NE SPACE.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M29 'ZTMM_MARA-LGPLA'.
    ENDIF.
  ELSEIF IT_ITAB-LGTYP NE SPACE.
    IF IT_ITAB-LGPLA EQ SPACE.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M29 'ZTMM_MARA-LGPLA'.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_input_value_mm

*&---------------------------------------------------------------------*
*&      Form  show_message_top_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_M29  text
*----------------------------------------------------------------------*
FORM SHOW_MESSAGE_TOP_LINE USING    P_TEXT P_FIELD_NAME.
*---
  MESSAGE S999 WITH P_TEXT.

  MOVE : SY-TABIX TO TC_9000-TOP_LINE.
  MOVE : 'X' TO W_ERROR_CHECK,
         P_FIELD_NAME TO W_FIELD_NAME.

  LEAVE TO SCREEN SY-DYNNR.
ENDFORM.                    " show_message_top_line

*&---------------------------------------------------------------------*
*&      Form  valuation_class
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VALUATION_CLASS.
*---
  IF IT_ITAB-PROFL EQ C_PROFL_K.
    MOVE : C_BKLAS_3000 TO IT_ITAB-BKLAS.
  ELSEIF IT_ITAB-PROFL EQ C_PROFL_V.
*    IF IT_ITAB-TEMPB EQ C_TEMPB_11.
*s__by Paul
    IF IT_ITAB-TEMPB EQ '1' or
       IT_ITAB-TEMPB EQ '2'.
      MOVE : C_BKLAS_3005 TO IT_ITAB-BKLAS.
    ELSE.
      MOVE : C_BKLAS_3001 TO IT_ITAB-BKLAS.
    ENDIF.
  ENDIF.
ENDFORM.                    " valuation_class

*&---------------------------------------------------------------------*
*&      Form  check_input_value_pur
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_VALUE_PUR.
*---
  CHECK IT_ITAB-MTART EQ C_MTART_ROH.

*--- HTS Number & Country Check
  IF IT_ITAB-PROFL EQ C_PROFL_V.
    IF IT_ITAB-STAWN NE SPACE OR IT_ITAB-HERKL NE SPACE.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M19 'ZTMM_MARA-STAWN'.
    ENDIF.
  ELSEIF IT_ITAB-PROFL EQ C_PROFL_K AND IT_ITAB-DISPO NE C_DISPO_M02.
    IF IT_ITAB-STAWN EQ SPACE OR IT_ITAB-HERKL EQ SPACE.
      PERFORM SHOW_MESSAGE_TOP_LINE USING TEXT-M19 'ZTMM_MARA-STAWN'.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_input_value_pur

*&---------------------------------------------------------------------*
*&      Form  save_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SAVE_ROUTINE.
**---
*  READ TABLE IT_ITAB WITH KEY W_SELECTED = 'X'.
*
*  IF SY-SUBRC NE 0.
*    MESSAGE E999 WITH TEXT-M30.
*  ENDIF.
*
**---
*  CLEAR : IT_ZTMM_MARA, IT_ZTMM_MARA[].
*
*  LOOP AT IT_ITAB WHERE W_SELECTED NE SPACE.
*    MOVE-CORRESPONDING IT_ITAB TO IT_ZTMM_MARA.
*    MOVE : SY-DATUM            TO IT_ZTMM_MARA-AEDAT,
*           SY-UZEIT            TO IT_ZTMM_MARA-AEZET,
*           SY-UNAME            TO IT_ZTMM_MARA-AENAM.
**---
*    IF IT_ITAB-MTART EQ C_MTART_HALB OR IT_ITAB-MTART EQ C_MTART_FERT.
*      MOVE : 'X'               TO IT_ZTMM_MARA-CONF1,
*             'X'               TO IT_ZTMM_MARA-CONF2,
*             'X'               TO IT_ZTMM_MARA-CONF3.
**             'X'               TO it_ztmm_mara-conf4.
**--- insert by stlim (2004/10/26)
*      MOVE : 'X'               TO IT_ZTMM_MARA-CONF4.
**--- end of insert
*    ENDIF.
**---
*    APPEND IT_ZTMM_MARA.
*    CLEAR : IT_ITAB, IT_ZTMM_MARA.
*  ENDLOOP.
*
*  LOOP AT IT_COPY.
*    MOVE-CORRESPONDING IT_COPY TO IT_ZTMM_MARA.
*    MOVE : SY-DATUM            TO IT_ZTMM_MARA-AEDAT,
*           SY-UZEIT            TO IT_ZTMM_MARA-AEZET,
*           SY-UNAME            TO IT_ZTMM_MARA-AENAM.
*    APPEND IT_ZTMM_MARA.
*    CLEAR : IT_ITAB, IT_ZTMM_MARA.
*  ENDLOOP.
*
*  MODIFY ZTMM_MARA FROM TABLE IT_ZTMM_MARA.
*
*  IF SY-SUBRC EQ 0.
*    COMMIT WORK.
*    MESSAGE S999 WITH TEXT-M31.
**    LEAVE TO SCREEN 0.
*  ELSE.
*    ROLLBACK WORK.
*    MESSAGE E999 WITH TEXT-M32.
*  ENDIF.
*ENDFORM.                    " save_routine

*&---------------------------------------------------------------------*
*&      Form  change_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_MATERIAL_MASTER.
*---
  READ TABLE IT_ITAB WITH KEY CONFT = 'X'.

  IF SY-SUBRC NE 0.
    MESSAGE E999 WITH TEXT-M30.
  ENDIF.

*---
  LOOP AT IT_ITAB WHERE CONFT  NE SPACE
                   AND MSTDE = '00000000'.

*  LOOP AT IT_ITAB WHERE CONF5 EQ SPACE     " completed
**                    AND conf1 NE space     " MM
**                    AND conf2 NE space     " PUR
**                    AND conf3 NE space     " DEV
**                    AND conf4 NE space     " QM
*                    AND W_SELECTED NE SPACE.
    CLEAR : W_SUBRC.

*---
*    IF IT_ITAB-CONF1 NE SPACE AND IT_ITAB-CONF2 NE SPACE.   " AND
*       IT_ITAB-CONF3 NE SPACE AND IT_ITAB-CONF4 NE SPACE.

*--- material master change by BAPI
    PERFORM CHANGE_BY_BAPI.
*---
    PERFORM CALL_BAPI_FUNCTION.
*--- material master change by BDC (MM02)
    PERFORM CHANGE_BY_BDC.
*--- update CBO table
    PERFORM UPDATE_TABLE.

*    ELSE.
*
*      MOVE : TEXT-M35 TO IT_ITAB-MESSA.
*      MOVE-CORRESPONDING IT_ITAB TO ZTMM_MARA.
*      MODIFY : ZTMM_MARA, IT_ITAB.
*      IF SY-SUBRC EQ 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*
*      MESSAGE S999 WITH IT_ITAB-MESSA.
*
*    ENDIF.
  ENDLOOP.
ENDFORM.                    " change_material_master

*&---------------------------------------------------------------------*
*&      Form  call_bapi_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_BAPI_FUNCTION.
*---

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
       EXPORTING
            HEADDATA             = WA_HEADDATA
            CLIENTDATA           = WA_CLIENTDATA
            CLIENTDATAX          = WA_CLIENTDATAX
            PLANTDATA            = WA_PLANTDATA
            PLANTDATAX           = WA_PLANTDATAX
            STORAGELOCATIONDATA  = WA_STORAGELOCATIONDATA
            STORAGELOCATIONDATAX = WA_STORAGELOCATIONDATAX
            VALUATIONDATA        = WA_VALUATIONDATA
            VALUATIONDATAX       = WA_VALUATIONDATAX
            WAREHOUSENUMBERDATA  = WA_WAREHOUSENUMBERDATA
            WAREHOUSENUMBERDATAX = WA_WAREHOUSENUMBERDATAX
            STORAGETYPEDATA      = WA_STORAGETYPEDATA
            STORAGETYPEDATAX     = WA_STORAGETYPEDATAX
       IMPORTING
            RETURN               = WA_BAPIRET2
       TABLES
            UNITSOFMEASURE       = IT_UNITSOFMEASURE
            UNITSOFMEASUREX      = IT_UNITSOFMEASUREX
            RETURNMESSAGES       = IT_BAPIRET2.

*---
  READ TABLE IT_BAPIRET2 WITH KEY TYPE = 'E'.

  IF SY-SUBRC EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MOVE : '4' TO W_SUBRC.
    MOVE : IT_BAPIRET2-MESSAGE TO IT_ITAB-MESSA.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              WAIT = 'X'.
    MOVE : '0' TO W_SUBRC.
    MOVE : IT_BAPIRET2-MESSAGE TO IT_ITAB-MESSA.
  ENDIF.
ENDFORM.                    " call_bapi_function

*&---------------------------------------------------------------------*
*&      Form  change_by_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_BY_BAPI.
*---
  CLEAR : WA_HEADDATA, WA_CLIENTDATA, WA_CLIENTDATAX, WA_PLANTDATA,
          WA_PLANTDATAX, WA_WAREHOUSENUMBERDATA,
          WA_WAREHOUSENUMBERDATA, WA_STORAGETYPEDATA,
          WA_STORAGETYPEDATAX, IT_BAPIRET2, IT_BAPIRET2[],
          WA_STORAGELOCATIONDATA, WA_STORAGELOCATIONDATAX,
          IT_UNITSOFMEASURE, IT_UNITSOFMEASURE[],
          IT_UNITSOFMEASUREX, IT_UNITSOFMEASUREX[].

  IF IT_ITAB-MTART EQ C_MTART_ROH.
*--- ROH
    PERFORM CHANGE_BY_BAPI_ROH.
  ELSEIF IT_ITAB-MTART EQ C_MTART_HALB OR IT_ITAB-MTART EQ C_MTART_FERT.
*--- HALB / FERT
    PERFORM CHANGE_BY_BAPI_HALB.
  ENDIF.
ENDFORM.                    " change_by_bapi

*&---------------------------------------------------------------------*
*&      Form  change_by_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_BY_BDC.
*---
  CHECK W_SUBRC EQ 0.

*---
  DATA : L_BRGEW(13).

  CLEAR : IT_BDC, IT_BDC[], IT_MESS, IT_MESS[],
          IT_MESSAGE, IT_MESSAGE[].

*---
  PERFORM DYNPRO USING : 'X'  'SAPLMGMM'        '0060',
                         ' '  'RMMG1-MATNR'     IT_ITAB-MATNR,
                         ' '  'BDC_OKCODE'      '/00'.

  PERFORM DYNPRO USING : 'X'  'SAPLMGMM'        '0070',
                         ' '  'BDC_OKCODE'      '=SELA'.

  PERFORM DYNPRO USING : 'X'  'SAPLMGMM'        '0070',
                         ' '  'BDC_OKCODE'      '=ENTR'.

  PERFORM DYNPRO USING : 'X'  'SAPLMGMM'        '0080',
                         ' '  'RMMG1-WERKS'     IT_ITAB-WERKS,
                         ' '  'BDC_OKCODE'      '=ENTR'.

  WRITE : IT_ITAB-BRGEW TO L_BRGEW UNIT IT_ITAB-GEWEI.

  PERFORM DYNPRO USING : 'X'  'SAPLMGMM'        '5004',
                         ' '  'MARA-EXTWG'      IT_ITAB-EXTWG,
                         ' '  'MARA-BRGEW'      L_BRGEW.

  IF IT_ITAB-MTART EQ C_MTART_ROH.

    PERFORM DYNPRO USING : ' '  'BDC_OKCODE'      '=SP13'.

    PERFORM DYNPRO USING : 'X'  'SAPLMGMM'        '5000',
                           ' '  'MARC-RGEKZ'      IT_ITAB-RGEKZ,
                           ' '  'BDC_OKCODE'      '=SP23'.

    PERFORM DYNPRO USING : 'X'  'SAPLMGMM'        '5000',
                           ' '  'BDC_OKCODE'      '=PB01'.

    PERFORM DYNPRO USING : 'X'  'SAPLQPLS'        '0100',
                           ' '  'BDC_OKCODE'      '=NEU'.

    PERFORM DYNPRO USING : 'X'  'SAPLQPLS'        '0100',
                           ' '  'RMQAM-ART(01)'   IT_ITAB-ART01,
                           ' '  'RMQAM-AKTIV(01)' IT_ITAB-AKTI1,
                           ' '  'RMQAM-ART(02)'   IT_ITAB-ART02,
                           ' '  'RMQAM-AKTIV(02)' IT_ITAB-AKTI2,
                           ' '  'BDC_OKCODE'      '=WEIT'.

    PERFORM DYNPRO USING : 'X'  'SAPLMGMM'        '5000',
                           ' '  'BDC_OKCODE'      '=BU'.

  ELSE.

    PERFORM DYNPRO USING : ' '  'BDC_OKCODE'      '=BU'.

  ENDIF.


*---
  CALL TRANSACTION 'MM02' USING IT_BDC
                          MODE W_MODE
                          UPDATE 'S'
                          MESSAGES INTO IT_MESS.

  APPEND LINES OF IT_MESS TO IT_MESSAGE.

*---
  CLEAR : IT_MESS.

  READ TABLE IT_MESS WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC EQ 0.
    MOVE : '4' TO W_SUBRC.
    PERFORM GET_MESSAGE CHANGING W_MESSA.
    MOVE : W_MESSA TO IT_ITAB-MESSA.
  ELSE.
    READ TABLE IT_MESS WITH KEY MSGTYP = 'S'.
    IF SY-SUBRC EQ 0.
      MOVE : '0' TO W_SUBRC.
      READ TABLE IT_MESS WITH KEY MSGTYP = 'S'
                                  MSGID  = 'M3'
                                  MSGNR  = '810'.
      IF SY-SUBRC NE 0.
        PERFORM GET_MESSAGE CHANGING W_MESSA.
        MOVE : W_MESSA TO IT_ITAB-MESSA.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " change_by_bdc

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3376   text
*      -->P_3377   text
*      -->P_3378   text
*----------------------------------------------------------------------*
FORM DYNPRO USING    DYNBEGIN
                     NAME
                     VALUE.
*---
  IF DYNBEGIN = 'X'.
    CLEAR : IT_BDC.
    MOVE : NAME  TO IT_BDC-PROGRAM,
           VALUE TO IT_BDC-DYNPRO,
           'X'   TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE .
    CLEAR : IT_BDC.
    MOVE : NAME  TO IT_BDC-FNAM,
           VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.                    " dynpro

*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE.
*---
  IF W_SUBRC EQ 0.
    MOVE : 'X' TO IT_ITAB-CONF5,
           SPACE TO IT_ITAB-W_SELECTED.
*    MESSAGE s999 WITH it_itab-messa.
  ELSE.
    MOVE : SPACE TO IT_ITAB-CONF5.
*    MESSAGE e999 WITH it_itab-messa.
  ENDIF.

*---
  CLEAR : ZTMM_MARA.

  MOVE-CORRESPONDING IT_ITAB TO ZTMM_MARA.

  MODIFY : ZTMM_MARA, IT_ITAB.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

*---
  IF W_SUBRC EQ 0.
    MESSAGE S999 WITH IT_ITAB-MESSA.
  ELSE.
    MESSAGE E999 WITH IT_ITAB-MESSA.
  ENDIF.
ENDFORM.                    " update_table

*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_W_MESSA  text
*----------------------------------------------------------------------*
FORM GET_MESSAGE CHANGING P_MESSA.
*---
  CLEAR : P_MESSA.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = IT_MESS-MSGID
            MSGNR               = IT_MESS-MSGNR
            MSGV1               = IT_MESS-MSGV1
            MSGV2               = IT_MESS-MSGV2
            MSGV3               = IT_MESS-MSGV3
            MSGV4               = IT_MESS-MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = P_MESSA.
ENDFORM.                    " get_message

*&---------------------------------------------------------------------*
*&      Form  change_by_bapi_roh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_BY_BAPI_ROH.
*--- header
  MOVE : IT_ITAB-MATNR TO WA_HEADDATA-MATERIAL,
         IT_ITAB-MTART TO WA_HEADDATA-MATL_TYPE,
         'X'           TO WA_HEADDATA-BASIC_VIEW,
         'X'           TO WA_HEADDATA-PURCHASE_VIEW,
         'X'           TO WA_HEADDATA-MRP_VIEW.
  IF IT_ITAB-MTART EQ C_MTART_ROH.
    IF NOT IT_ITAB-LGORT IS INITIAL.
      MOVE : 'X'           TO WA_HEADDATA-WAREHOUSE_VIEW,
             'X'           TO WA_HEADDATA-STORAGE_VIEW.
    ENDIF.
  ENDIF.
  MOVE : 'X'           TO WA_HEADDATA-QUALITY_VIEW,
         'X'           TO WA_HEADDATA-ACCOUNT_VIEW,
         'X'           TO WA_HEADDATA-COST_VIEW.
*--- client data
  MOVE : IT_ITAB-MATKL TO WA_CLIENTDATA-MATL_GROUP,
         IT_ITAB-BISMT TO WA_CLIENTDATA-OLD_MAT_NO,
         IT_ITAB-FERTH TO WA_CLIENTDATA-PROD_MEMO,
         IT_ITAB-MTPOS_MARA TO WA_CLIENTDATA-ITEM_CAT,
         IT_ITAB-NTGEW TO WA_CLIENTDATA-NET_WEIGHT,
         IT_ITAB-GEWEI TO WA_CLIENTDATA-UNIT_OF_WT,
         IT_ITAB-GEWEI TO WA_CLIENTDATA-UNIT_OF_WT_ISO,
         IT_ITAB-PROFL TO WA_CLIENTDATA-HAZMATPROF,
         IT_ITAB-TEMPB TO WA_CLIENTDATA-TEMP_CONDS,
         IT_ITAB-RAUBE TO WA_CLIENTDATA-STOR_CONDS.
  MOVE : 'X'           TO WA_CLIENTDATAX-MATL_GROUP,
         'X'           TO WA_CLIENTDATAX-OLD_MAT_NO,
         'X'           TO WA_CLIENTDATAX-PROD_MEMO,
         'X'           TO WA_CLIENTDATAX-ITEM_CAT,
         'X'           TO WA_CLIENTDATAX-NET_WEIGHT,
         'X'           TO WA_CLIENTDATAX-UNIT_OF_WT,
         'X'           TO WA_CLIENTDATAX-UNIT_OF_WT_ISO,
         'X'           TO WA_CLIENTDATAX-HAZMATPROF,
         'X'           TO WA_CLIENTDATAX-TEMP_CONDS,
         'X'           TO WA_CLIENTDATAX-STOR_CONDS.
  IF IT_ITAB-CONFT = 'X' AND IT_ITAB-MSTDE IS INITIAL.
    MOVE : SY-DATUM TO WA_CLIENTDATA-PVALIDFROM,
           'X' TO WA_CLIENTDATAX-PVALIDFROM.
  ENDIF.

*--- plant data
  MOVE : IT_ITAB-WERKS TO WA_PLANTDATA-PLANT,
         IT_ITAB-EKGRP TO WA_PLANTDATA-PUR_GROUP,
         IT_ITAB-USEQU TO WA_PLANTDATA-QUOTAUSAGE,
         IT_ITAB-FABKZ TO WA_PLANTDATA-JIT_RELVT,
         IT_ITAB-STAWN TO WA_PLANTDATA-COMM_CODE,
         IT_ITAB-HERKL TO WA_PLANTDATA-COUNTRYORI,
         IT_ITAB-DISPO TO WA_PLANTDATA-MRP_CTRLER,
         IT_ITAB-DISLS TO WA_PLANTDATA-LOTSIZEKEY,
         IT_ITAB-BSTMI TO WA_PLANTDATA-MINLOTSIZE,
         IT_ITAB-BSTMA TO WA_PLANTDATA-MAXLOTSIZE,
         IT_ITAB-BSTRF TO WA_PLANTDATA-ROUND_VAL,
         IT_ITAB-AUSDT TO WA_PLANTDATA-EFF_O_DAY,
         IT_ITAB-SOBSL TO WA_PLANTDATA-SPPROCTYPE,
         IT_ITAB-LGPRO TO WA_PLANTDATA-ISS_ST_LOC,
         IT_ITAB-LGFSB TO WA_PLANTDATA-SLOC_EXPRC,
         IT_ITAB-VSPVB TO WA_PLANTDATA-SUPPLY_AREA,
         IT_ITAB-PLIFZ TO WA_PLANTDATA-PLND_DELRY,
         IT_ITAB-MRPPP TO WA_PLANTDATA-PPC_PL_CAL,
         IT_ITAB-EISBE TO WA_PLANTDATA-SAFETY_STK,
         IT_ITAB-ABCIN TO WA_PLANTDATA-CC_PH_INV,
         IT_ITAB-XMCNG TO WA_PLANTDATA-NEG_STOCKS,
         IT_ITAB-SHFLG TO WA_PLANTDATA-SAFTY_T_ID,
         IT_ITAB-SHZET TO WA_PLANTDATA-SAFETYTIME.
  MOVE : IT_ITAB-WERKS TO WA_PLANTDATAX-PLANT,
         'X'           TO WA_PLANTDATAX-PUR_GROUP,
         'X'           TO WA_PLANTDATAX-QUOTAUSAGE,
         'X'           TO WA_PLANTDATAX-JIT_RELVT,
         'X'           TO WA_PLANTDATAX-COMM_CODE,
         'X'           TO WA_PLANTDATAX-COUNTRYORI,
         'X'           TO WA_PLANTDATAX-MRP_CTRLER,
         'X'           TO WA_PLANTDATAX-LOTSIZEKEY,
         'X'           TO WA_PLANTDATAX-MINLOTSIZE,
         'X'           TO WA_PLANTDATAX-MAXLOTSIZE,
         'X'           TO WA_PLANTDATAX-ROUND_VAL,
         'X'           TO WA_PLANTDATAX-EFF_O_DAY,
         'X'           TO WA_PLANTDATAX-SPPROCTYPE,
         'X'           TO WA_PLANTDATAX-ISS_ST_LOC,
         'X'           TO WA_PLANTDATAX-SLOC_EXPRC,
         'X'           TO WA_PLANTDATAX-SUPPLY_AREA,
         'X'           TO WA_PLANTDATAX-PLND_DELRY,
         'X'           TO WA_PLANTDATAX-PPC_PL_CAL,
         'X'           TO WA_PLANTDATAX-SAFETY_STK,
         'X'           TO WA_PLANTDATAX-CC_PH_INV,
         'X'           TO WA_PLANTDATAX-SAFTY_T_ID,
         'X'           TO WA_PLANTDATAX-SAFETYTIME,
         'X'           TO WA_PLANTDATAX-NEG_STOCKS.

*  IF IT_ITAB-CONFT = 'X' and it_itab-mstde is initial.
*    MOVE : sy-datum to WA_PLANTDATA-PVALIDFROM,
*           'X' to WA_PLANTDATAX-PVALIDFROM.
*  ENDIF.
*--- check existence => fill mandatory field
  CLEAR : MARC.
  SELECT SINGLE MATNR INTO MARC-MATNR
                      FROM MARC
                     WHERE MATNR EQ IT_ITAB-MATNR
                       AND WERKS EQ IT_ITAB-WERKS.
  IF SY-SUBRC NE 0.
    CLEAR : MARC.
    SELECT SINGLE MTVFP
                  DISMM
                  FHORI INTO (MARC-MTVFP, MARC-DISMM, MARC-FHORI)
                        FROM MARC
                       WHERE MATNR EQ IT_ITAB-MATNR.
    MOVE : MARC-MTVFP TO WA_PLANTDATA-AVAILCHECK,     " availability
           MARC-DISMM TO WA_PLANTDATA-MRP_TYPE,    " mrp type
           MARC-FHORI TO WA_PLANTDATA-SM_KEY,    " scheduling margin key
           'X'        TO WA_PLANTDATAX-AVAILCHECK,
           'X'        TO WA_PLANTDATAX-MRP_TYPE,
           'X'        TO WA_PLANTDATAX-SM_KEY.
    MOVE : C_MMSTA_11    TO WA_PLANTDATA-PUR_STATUS,
                                                   "plant-sp.matl status
*           it_itab-mstde TO wa_plantdata-pvalidfrom,
           'X'           TO WA_PLANTDATA-SOURCELIST,
           C_BESKZ_F     TO WA_PLANTDATA-PROC_TYPE,
           C_SBDKZ_2     TO WA_PLANTDATA-DEP_REQ_ID,
           'X'           TO WA_PLANTDATAX-PUR_STATUS,
*           'X'           TO wa_plantdatax-pvalidfrom,
           'X'           TO WA_PLANTDATAX-SOURCELIST,
           'X'           TO WA_PLANTDATAX-PROC_TYPE,
           'X'           TO WA_PLANTDATAX-DEP_REQ_ID.
  ENDIF.
*--- warehouse number
  IF IT_ITAB-WERKS EQ C_WERKS_P001 AND IT_ITAB-LGORT EQ C_LGORT_P400.
    MOVE : IT_ITAB-LGNUM TO WA_WAREHOUSENUMBERDATA-WHSE_NO,
           IT_ITAB-LTKZA TO WA_WAREHOUSENUMBERDATA-WITHDRAWAL,
           IT_ITAB-LTKZE TO WA_WAREHOUSENUMBERDATA-PLACEMENT.
    MOVE : IT_ITAB-LGNUM TO WA_WAREHOUSENUMBERDATAX-WHSE_NO,
           'X'           TO WA_WAREHOUSENUMBERDATAX-WITHDRAWAL,
           'X'           TO WA_WAREHOUSENUMBERDATAX-PLACEMENT.
*--- storage type
    MOVE : IT_ITAB-LGNUM TO WA_STORAGETYPEDATA-WHSE_NO,
           IT_ITAB-LGTYP TO WA_STORAGETYPEDATA-STGE_TYPE,
           IT_ITAB-LGPLA TO WA_STORAGETYPEDATA-STGE_BIN,
           IT_ITAB-RDMNG TO WA_STORAGETYPEDATA-ROUND_QTY.
    MOVE : IT_ITAB-LGNUM TO WA_STORAGETYPEDATAX-WHSE_NO,
           IT_ITAB-LGTYP TO WA_STORAGETYPEDATAX-STGE_TYPE,
           'X'           TO WA_STORAGETYPEDATAX-STGE_BIN,
           'X'           TO WA_STORAGETYPEDATAX-ROUND_QTY.
  ENDIF.
*--- valuation data
  MOVE : IT_ITAB-WERKS TO WA_VALUATIONDATA-VAL_AREA,
         IT_ITAB-BKLAS TO WA_VALUATIONDATA-VAL_CLASS.
  MOVE : IT_ITAB-WERKS TO WA_VALUATIONDATAX-VAL_AREA,
         'X'           TO WA_VALUATIONDATAX-VAL_CLASS.
*--- storage location
  IF IT_ITAB-MTART EQ C_MTART_ROH.
    IF NOT IT_ITAB-LGORT IS INITIAL.
      MOVE : IT_ITAB-WERKS TO WA_STORAGELOCATIONDATA-PLANT,
             IT_ITAB-LGORT TO WA_STORAGELOCATIONDATA-STGE_LOC,
             IT_ITAB-LGPBE TO WA_STORAGELOCATIONDATA-STGE_BIN.
      MOVE : IT_ITAB-WERKS TO WA_STORAGELOCATIONDATAX-PLANT,
             IT_ITAB-LGORT TO WA_STORAGELOCATIONDATAX-STGE_LOC,
             'X'           TO WA_STORAGELOCATIONDATAX-STGE_BIN.
    ENDIF.
  ENDIF.

*--- additional data
  IF NOT IT_ITAB-MEINH IS INITIAL AND NOT IT_ITAB-UMREZ IS INITIAL.
    MOVE : IT_ITAB-MEINH   TO IT_UNITSOFMEASURE-ALT_UNIT,
           IT_ITAB-MEINH   TO IT_UNITSOFMEASURE-ALT_UNIT_ISO,
           IT_ITAB-UMREZ   TO IT_UNITSOFMEASURE-NUMERATOR,
           IT_ITAB-UMREN   TO IT_UNITSOFMEASURE-DENOMINATR,
           IT_ITAB-LAENG   TO IT_UNITSOFMEASURE-LENGTH,
           IT_ITAB-BREIT   TO IT_UNITSOFMEASURE-WIDTH,
           IT_ITAB-HOEHE   TO IT_UNITSOFMEASURE-HEIGHT,
           IT_ITAB-MEABM   TO IT_UNITSOFMEASURE-UNIT_DIM,
           IT_ITAB-VOLEH   TO IT_UNITSOFMEASURE-VOLUMEUNIT.
    MOVE : IT_ITAB-MEINH   TO IT_UNITSOFMEASUREX-ALT_UNIT,
           IT_ITAB-MEINH   TO IT_UNITSOFMEASUREX-ALT_UNIT_ISO,
           'X'             TO IT_UNITSOFMEASUREX-NUMERATOR,
           'X'             TO IT_UNITSOFMEASUREX-DENOMINATR,
           'X'             TO IT_UNITSOFMEASUREX-LENGTH,
           'X'             TO IT_UNITSOFMEASUREX-WIDTH,
           'X'             TO IT_UNITSOFMEASUREX-HEIGHT,
           'X'             TO IT_UNITSOFMEASUREX-UNIT_DIM,
           'X'             TO IT_UNITSOFMEASUREX-VOLUMEUNIT.
    APPEND : IT_UNITSOFMEASURE, IT_UNITSOFMEASUREX.
  ENDIF.
ENDFORM.                    " change_by_bapi_roh

*&---------------------------------------------------------------------*
*&      Form  change_by_bapi_halb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_BY_BAPI_HALB.
*--- header
  MOVE : IT_ITAB-MATNR TO WA_HEADDATA-MATERIAL,
         IT_ITAB-MTART TO WA_HEADDATA-MATL_TYPE,
         'X'           TO WA_HEADDATA-BASIC_VIEW.
*--- client data
  MOVE : IT_ITAB-NTGEW TO WA_CLIENTDATA-NET_WEIGHT,
         IT_ITAB-GEWEI TO WA_CLIENTDATA-UNIT_OF_WT,
         IT_ITAB-GEWEI TO WA_CLIENTDATA-UNIT_OF_WT_ISO,
         IT_ITAB-PROFL TO WA_CLIENTDATA-HAZMATPROF.
  MOVE : 'X'           TO WA_CLIENTDATAX-NET_WEIGHT,
         'X'           TO WA_CLIENTDATAX-UNIT_OF_WT,
         'X'           TO WA_CLIENTDATAX-UNIT_OF_WT_ISO,
         'X'           TO WA_CLIENTDATAX-HAZMATPROF.
ENDFORM.                    " change_by_bapi_halb

*&---------------------------------------------------------------------*
*&      Form  delete_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM DELETE_ITEM.
**---
*  DATA : IT_DELE LIKE ZTMM_MARA OCCURS 0 WITH HEADER LINE.
*
*  DATA : L_ANSWER(1).
*
**---
*  READ TABLE IT_ITAB WITH KEY W_SELECTED = 'X'.
*
*  IF SY-SUBRC NE 0.
*    MESSAGE E999 WITH TEXT-M30.
*  ENDIF.
*
**---
*  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*       EXPORTING
*            DEFAULTOPTION  = 'Y'
*            TEXTLINE1      = TEXT-012
*            TITEL          = TEXT-013
*            START_COLUMN   = 25
*            START_ROW      = 6
*            CANCEL_DISPLAY = 'X'
*       IMPORTING
*            ANSWER         = L_ANSWER.
*
*  CHECK L_ANSWER EQ 'J'.
*
**---
*  CLEAR : IT_DELE, IT_DELE[].
*
*  LOOP AT IT_ITAB WHERE W_SELECTED NE SPACE
*                    AND CONF5 EQ SPACE.
*    MOVE-CORRESPONDING IT_ITAB TO IT_DELE.
*    MOVE : 'X'                 TO IT_DELE-LOEKZ,
*           SY-DATUM            TO IT_DELE-AEDAT,
*           SY-UZEIT            TO IT_DELE-AEZET,
*           SY-UNAME            TO IT_DELE-AENAM.
*    APPEND IT_DELE.
*    DELETE IT_ITAB.
*    CLEAR : IT_DELE.
*  ENDLOOP.
*
*  MODIFY ZTMM_MARA FROM TABLE IT_DELE.
*ENDFORM.                    " delete_item
*&---------------------------------------------------------------------*
*&      Form  get_from_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FROM_MATERIAL_MASTER.
  DATA: IT_MARA LIKE ZTMM_MARA OCCURS 0 WITH HEADER LINE.
  DATA: IT_QMAT LIKE QMAT OCCURS 0 WITH HEADER LINE.
  DATA: W_INDEX TYPE N.

  READ TABLE IT_ITAB WITH KEY W_SELECTED = 'X'.

  IF SY-SUBRC NE 0.
    MESSAGE E999 WITH TEXT-M30.
  ENDIF.

  SELECT A~MATNR MTART B~WERKS PROFL
         C~LGORT D~LGTYP A~MATKL A~BISMT A~EXTWG A~MTPOS_MARA
         A~NTGEW A~GEWEI EKGRP USEQU FABKZ STAWN SHFLG SHZET
*         umren
         B~HERKL DISPO DISLS BSTMI BSTMA BSTRF SOBSL VSPVB RGEKZ
         PLIFZ MRPPP EISBE TEMPB RAUBE ABCIN
         LGPLA D~RDMNG FERTH AUSDT LGPBE A~LAENG A~BREIT A~HOEHE A~MEABM
         INTO CORRESPONDING FIELDS OF TABLE IT_MARA
             FROM MARA AS A JOIN MARC AS B
             ON A~MATNR = B~MATNR
             LEFT OUTER JOIN MARD AS C
             ON A~MATNR  = C~MATNR
             LEFT OUTER JOIN MLGT AS D
             ON A~MATNR = D~MATNR
*             join marm as e
*             on a~matnr = e~matnr
*              and a~MEINS = e~meinh
             FOR ALL ENTRIES IN IT_ITAB
              WHERE MTART = IT_ITAB-MTART
              AND A~MATNR = IT_ITAB-MATNR
              AND B~WERKS = IT_ITAB-WERKS.
*              AND c~lgort = it_itab-lgort
*              AND d~lgtyp = it_itab-lgtyp
*              AND a~profl = it_itab-profl
*              AND a~matkl = it_itab-matkl
*              AND b~ekgrp = it_itab-ekgrp
*             AND b~stawn = it_itab-stawn
*             AND b~dispo = it_itab-dispo
*             AND b~disls = it_itab-disls
*             AND b~vspvb = it_itab-vspvb.

  SELECT * INTO TABLE IT_QMAT FROM QMAT
                FOR ALL ENTRIES IN IT_ITAB
                WHERE MATNR = IT_ITAB-MATNR
                  AND WERKS = IT_ITAB-WERKS.

  SORT IT_MARA BY MATNR WERKS LGORT.
  SORT IT_QMAT BY MATNR WERKS ART.

  LOOP AT IT_ITAB WHERE W_SELECTED NE SPACE
                    AND CONF5 EQ SPACE.
    READ TABLE IT_MARA WITH KEY MATNR = IT_ITAB-MATNR
                                WERKS = IT_ITAB-WERKS.
    IF IT_ITAB-LGORT IS INITIAL.
      READ TABLE IT_MARA WITH KEY MATNR = IT_ITAB-MATNR
                               WERKS = IT_ITAB-WERKS.
      IT_ITAB-LGORT = IT_MARA-LGORT.
    ELSE.
      READ TABLE IT_MARA WITH KEY MATNR = IT_ITAB-MATNR
                               WERKS = IT_ITAB-WERKS
                               LGORT = IT_ITAB-LGORT.
    ENDIF.
    IF ( IT_ITAB-MATKL IS INITIAL OR IT_ITAB-MATKL = 'INIT' )
       AND IT_MARA-MATKL NE ' '.
      IT_ITAB-MATKL = IT_MARA-MATKL.
    ENDIF.
    IF IT_ITAB-PROFL IS INITIAL.
      IT_ITAB-PROFL = IT_MARA-PROFL.
    ENDIF.

    IF IT_ITAB-LGTYP IS INITIAL.
      IT_ITAB-LGTYP = IT_MARA-LGTYP.
    ENDIF.
    IF IT_ITAB-BISMT IS INITIAL.
      IT_ITAB-BISMT = IT_MARA-BISMT.
    ENDIF.
    IF IT_ITAB-EXTWG IS INITIAL.
      IT_ITAB-EXTWG = IT_MARA-EXTWG.
    ENDIF.
    IF IT_ITAB-MTPOS_MARA IS INITIAL.
      IT_ITAB-MTPOS_MARA = IT_MARA-MTPOS_MARA.
    ENDIF.
    IF IT_ITAB-NTGEW IS INITIAL.
      IT_ITAB-NTGEW = IT_MARA-NTGEW.
    ENDIF.
    IF IT_ITAB-GEWEI IS INITIAL.
      IT_ITAB-GEWEI = IT_MARA-GEWEI.
    ENDIF.
*   if it_itab-umren is initial.
*      it_itab-umren = it_mara-umren.
*   endif.
    IF IT_ITAB-EKGRP IS INITIAL.
      IT_ITAB-EKGRP = IT_MARA-EKGRP.
    ENDIF.
    IF IT_ITAB-USEQU IS INITIAL.
      IT_ITAB-USEQU = IT_MARA-USEQU.
    ENDIF.
    IF IT_ITAB-FABKZ IS INITIAL.
      IT_ITAB-FABKZ = IT_MARA-FABKZ.
    ENDIF.
    IF IT_ITAB-STAWN IS INITIAL.
      IT_ITAB-STAWN = IT_MARA-STAWN.
    ENDIF.
    IF IT_ITAB-HERKL IS INITIAL.
      IT_ITAB-HERKL = IT_MARA-HERKL.
    ENDIF.
    IF IT_ITAB-DISPO IS INITIAL.
      IT_ITAB-DISPO = IT_MARA-DISPO.
    ENDIF.
    IF IT_ITAB-DISLS IS INITIAL.
      IT_ITAB-DISLS = IT_MARA-DISLS.
    ENDIF.
    IF IT_ITAB-BSTMI IS INITIAL.
      IT_ITAB-BSTMI = IT_MARA-BSTMI.
    ENDIF.
    IF IT_ITAB-BSTMA IS INITIAL.
      IT_ITAB-BSTMA = IT_MARA-BSTMA.
    ENDIF.
    IF IT_ITAB-BSTRF IS INITIAL.
      IT_ITAB-BSTRF = IT_MARA-BSTRF.
    ENDIF.
    IF IT_ITAB-SOBSL IS INITIAL.
      IT_ITAB-SOBSL = IT_MARA-SOBSL.
    ENDIF.
    IF IT_ITAB-VSPVB IS INITIAL.
      IT_ITAB-VSPVB = IT_MARA-VSPVB.
    ENDIF.
    IF IT_ITAB-RGEKZ IS INITIAL.
      IT_ITAB-RGEKZ = IT_MARA-RGEKZ.
    ENDIF.
    IF IT_ITAB-PLIFZ IS INITIAL.
      IT_ITAB-PLIFZ = IT_MARA-PLIFZ.
    ENDIF.
    IF IT_ITAB-MRPPP IS INITIAL.
      IT_ITAB-MRPPP = IT_MARA-MRPPP.
    ENDIF.
    IF IT_ITAB-EISBE IS INITIAL.
      IT_ITAB-EISBE = IT_MARA-EISBE.
    ENDIF.
    IF IT_ITAB-SHFLG IS INITIAL.
      IT_ITAB-SHFLG = IT_MARA-SHFLG.
    ENDIF.
    IF IT_ITAB-SHZET IS INITIAL.
      IT_ITAB-SHZET = IT_MARA-SHZET.
    ENDIF.
    IF IT_ITAB-TEMPB IS INITIAL.
      IT_ITAB-TEMPB = IT_MARA-TEMPB.
    ENDIF.
    IF IT_ITAB-RAUBE IS INITIAL.
      IT_ITAB-RAUBE = IT_MARA-RAUBE.
    ENDIF.
    IF IT_ITAB-ABCIN IS INITIAL.
      IT_ITAB-ABCIN = IT_MARA-ABCIN.
    ENDIF.
    IF IT_ITAB-LGPLA IS INITIAL.
      IT_ITAB-LGPLA = IT_MARA-LGPLA.
    ENDIF.
    IF IT_ITAB-RDMNG IS INITIAL.
      IT_ITAB-RDMNG = IT_MARA-RDMNG.
    ENDIF.
    IF IT_ITAB-FERTH IS INITIAL.
      IT_ITAB-FERTH = IT_MARA-FERTH.
    ENDIF.
    IF IT_ITAB-AUSDT IS INITIAL.
      IT_ITAB-AUSDT = IT_MARA-AUSDT.
    ENDIF.
    IF IT_ITAB-LGPBE IS INITIAL.
      IT_ITAB-LGPBE = IT_MARA-LGPBE.
    ENDIF.
    IF IT_ITAB-LAENG IS INITIAL.
      IT_ITAB-LAENG = IT_MARA-LAENG.
    ENDIF.
    IF IT_ITAB-BREIT IS INITIAL.
      IT_ITAB-BREIT = IT_MARA-BREIT.
    ENDIF.
    IF IT_ITAB-HOEHE IS INITIAL.
      IT_ITAB-HOEHE = IT_MARA-HOEHE.
    ENDIF.
    IF IT_ITAB-MEABM IS INITIAL.
      IT_ITAB-MEABM = IT_MARA-MEABM.
    ENDIF.
    IF IT_ITAB-ART01 IS INITIAL.
      READ TABLE IT_QMAT WITH KEY MATNR = IT_ITAB-MATNR
                                  WERKS = IT_ITAB-WERKS.
      IF SY-SUBRC EQ 0.
        IT_ITAB-ART01 = IT_QMAT-ART.
        IT_ITAB-AKTI1 = IT_QMAT-AKTIV.
        IF IT_ITAB-ART02 IS INITIAL.
          W_INDEX = SY-TABIX + 1.
*            read table it_qmat with key matnr = it_itab-matnr
*                                  werks = it_itab-werks
*                                  art <> it_itab-art01.
          READ TABLE IT_QMAT INDEX W_INDEX.
          IF SY-SUBRC EQ 0.
            IT_ITAB-ART02 = IT_QMAT-ART.
            IT_ITAB-AKTI2 = IT_QMAT-AKTIV.
          ENDIF.
          CLEAR W_INDEX.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY IT_ITAB.
    CLEAR IT_ITAB.
  ENDLOOP.
ENDFORM.                    " get_from_material_master
*&---------------------------------------------------------------------*
*&      Module  check_get_master_data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE check_get_master_data INPUT.
*  if sy-dynnr = '9000' and w_okcode = 'GETM'.
*     perform get_from_material_master.
*  endif.
*ENDMODULE.                 " check_get_master_data  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_to_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA_TO_EXCEL.
  DATA: W_COL TYPE I,
        W_COL_CHAR LIKE GXXLT_V-COL_NO.
  DATA: BEGIN OF IT_DATA OCCURS 0,
       MATNR LIKE IT_ITAB-MATNR,
       WERKS LIKE IT_ITAB-WERKS,
       MTART LIKE IT_ITAB-MTART,
       PROFL LIKE IT_ITAB-PROFL,
       LGORT LIKE IT_ITAB-LGORT,
       MAKTX LIKE MAKT-MAKTX,
*      LGNUM LIKE IT_ITAB-LGNUM,
       LGTYP LIKE IT_ITAB-LGTYP,
       BISMT LIKE IT_ITAB-BISMT,
       EXTWG LIKE IT_ITAB-EXTWG,
       MTPOS_MARA LIKE IT_ITAB-MTPOS_MARA,
       NTGEW LIKE IT_ITAB-NTGEW,
       GEWEI LIKE IT_ITAB-GEWEI,
       EKGRP LIKE IT_ITAB-EKGRP,
       USEQU LIKE IT_ITAB-USEQU,
       FABKZ LIKE IT_ITAB-FABKZ,
       STAWN LIKE IT_ITAB-STAWN,
       HERKL LIKE IT_ITAB-HERKL,
       DISPO LIKE IT_ITAB-DISPO,
       DISLS LIKE IT_ITAB-DISLS,
       BSTMI LIKE IT_ITAB-BSTMI,
       BSTMA LIKE IT_ITAB-BSTMA,
       BSTRF LIKE IT_ITAB-BSTRF,
       SOBSL LIKE IT_ITAB-SOBSL,
       VSPVB LIKE IT_ITAB-VSPVB,
       RGEKZ LIKE IT_ITAB-RGEKZ,
       PLIFZ LIKE IT_ITAB-PLIFZ,
       MRPPP LIKE IT_ITAB-MRPPP,
       EISBE LIKE IT_ITAB-EISBE,
       SHFLG LIKE IT_ITAB-SHFLG,
       SHZET LIKE IT_ITAB-SHZET,
       TEMPB LIKE IT_ITAB-TEMPB,
       RAUBE LIKE IT_ITAB-RAUBE,
       ABCIN LIKE IT_ITAB-ABCIN,
       LGPLA LIKE IT_ITAB-LGPLA,
       RDMNG LIKE IT_ITAB-RDMNG,
       FERTH LIKE IT_ITAB-FERTH,
       AUSDT LIKE IT_ITAB-AUSDT,
       LGPBE LIKE IT_ITAB-LGPBE,
       LAENG LIKE IT_ITAB-LAENG,
       BREIT LIKE IT_ITAB-BREIT,
       HOEHE LIKE IT_ITAB-HOEHE,
       MEABM LIKE IT_ITAB-MEABM,
       VOLEH LIKE IT_ITAB-VOLEH,
       ART01 LIKE IT_ITAB-ART01,
       AKTI1 LIKE IT_ITAB-AKTI1,
       ART02 LIKE IT_ITAB-ART02,
       AKTI2 LIKE IT_ITAB-AKTI2,
       END OF IT_DATA.

  REFRESH COL_TEXT.
  CLEAR COL_TEXT.
  READ TABLE IT_ITAB WITH KEY CONFT = 'X'.
  CHECK SY-SUBRC EQ 0.
  LOOP AT IT_ITAB WHERE CONFT = 'X'.
    MOVE-CORRESPONDING IT_ITAB TO IT_DATA.
*       select single maktx into makt-maktx
*                      from makt
*                     where matnr eq it_itab_matnr
*                       and spras eq sy-langu.
    PERFORM GET_MATERIAL_DESC USING IT_ITAB-MATNR.
    MOVE MAKT-MAKTX TO IT_DATA-MAKTX.
    APPEND IT_DATA.
  ENDLOOP.
  W_COL = 1.
  W_COL_CHAR = W_COL.
*  condense w_col_char.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Material'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Plant'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Type'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Source'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Loca'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Descrition'.
*  w_col = w_col + 1.
*  w_col_char = w_col.
*  PERFORM FILL_CELL USING: w_col_char 'WMN'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'S TY'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Old Material No.'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Ext Material grp'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Itm Cat'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Net Weight'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'W Uni'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Pur Grp'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Quota'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'JIT'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Commodity/Import'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Org.Cty'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'MRP Controller'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Lot Size'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Min Lot Size'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Max Lot Size'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Rounding Value'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Spec Proc Ty'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Supp Area'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'BF'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Plan Del'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Plan Calenlar'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Safety Stock'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Safe T Ind'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Safe Time'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'BF Cycle'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Shop'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Phy Inv'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Storage Bin'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Rounding Qty'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Prdt/Inspec Memo'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Eff-Out date'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Storage Bin'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Length'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Width'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Height'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Unit'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Vol Unit'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Insp Type'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Active'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Insp Type'.
  W_COL = W_COL + 1.
  W_COL_CHAR = W_COL.
  PERFORM FILL_CELL USING: W_COL_CHAR 'Active'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
*           PERCENTAGE = 0
           TEXT       = 'Downloading to Excel, Please Wait....'
       EXCEPTIONS
            OTHERS     = 1.

  CALL FUNCTION 'XXL_CHECK_API'
       EXPORTING
            QUESTION = 'STARTABLE'
*    IMPORTING
*         RETURN_CODE  =
     EXCEPTIONS
          INV_QUESTION = 1
          OTHERS       = 2.

  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH 'API Error'.
  ELSE.
    CALL FUNCTION 'XXL_SIMPLE_API'
      EXPORTING
*   FILENAME                = 'ULO     '
        HEADER                  = 'Downloding to Excel'
        N_KEY_COLS              = 1
      TABLES
        COL_TEXT                = COL_TEXT
        DATA                    = IT_DATA
        ONLINE_TEXT             = ONLINE_TEXT
        PRINT_TEXT              = PRINT_TEXT
      EXCEPTIONS
        DIM_MISMATCH_DATA       = 1
        FILE_OPEN_ERROR         = 2
        FILE_WRITE_ERROR        = 3
        INV_WINSYS              = 4
        INV_XXL                 = 5
        OTHERS                  = 6.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_cell                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_COL_NO                                                      *
*  -->  P_COL_NAME                                                    *
*---------------------------------------------------------------------*
FORM FILL_CELL USING P_COL_NO LIKE GXXLT_V-COL_NO
                    P_COL_NAME LIKE GXXLT_V-COL_NAME.
*  p_col_no = p_col_no + 1.
  COL_TEXT-COL_NO = P_COL_NO.
  COL_TEXT-COL_NAME = P_COL_NAME.
  APPEND COL_TEXT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  control_field_input_scr9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CONTROL_FIELD_INPUT_SCR9000 OUTPUT.
  LOOP AT SCREEN.
    PERFORM COLUMN_INPUT TABLES TC_9000-COLS
                             USING SCREEN-NAME W_RADIO.
  ENDLOOP.
ENDMODULE.                 " control_field_input_scr9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  COLUMN_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_9000_COLS  text
*      -->P_SCREEN_NAME  text
*      -->P_6697   text
*----------------------------------------------------------------------*
FORM COLUMN_INPUT     TABLES   P_TC_9000_COLS STRUCTURE WA_TAB
                      USING    P_SCREEN_NAME
                               P_RADIO.

  CASE P_RADIO.
    WHEN '1'.
      IF   P_SCREEN_NAME EQ 'ZTMM_MARA-MAKTX' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-MEINS' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-PROFL' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-MATKL' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-NTGEW' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-GEWEI' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-FABKZ' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-STAWN' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-HERKL' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-DISPO' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-DISLS' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-BSTMI' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-BSTMA' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-BSTRF' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-SOBSL' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-VSPVB' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-RGEKZ' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-PLIFZ' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-MRPPP' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-EISBE' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-TEMPB' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-RAUBE' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-ABCIN' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-BKLAS' OR
           P_SCREEN_NAME EQ 'ZTMM_MARA-EXTWG'.
        READ TABLE P_TC_9000_COLS WITH KEY SCREEN-NAME = P_SCREEN_NAME.
        IF SY-SUBRC = 0.
*          MOVE : '1' TO P_TC_9000_COLS-SCREEN-ACTIVE.
          MOVE : '1' TO P_TC_9000_COLS-SCREEN-INPUT.
          MODIFY P_TC_9000_COLS INDEX SY-TABIX.
        ENDIF.
      ELSE.
        READ TABLE P_TC_9000_COLS WITH KEY SCREEN-NAME = P_SCREEN_NAME.
        IF SY-SUBRC = 0.
*          MOVE : '0' TO P_TC_9000_COLS-SCREEN-ACTIVE.
          MOVE : '0' TO P_TC_9000_COLS-SCREEN-INPUT.
*          MOVE : '1' TO P_TC_9000_COLS-SCREEN-DISPLAY.
          MODIFY P_TC_9000_COLS INDEX SY-TABIX.
        ENDIF.
      ENDIF.

  ENDCASE.




ENDFORM.                    " COLUMN_input
