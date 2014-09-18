************************************************************************
* Program Name      : REPORT ZBOM_MATERIAL_DISMANTLE
* Author            : Bongsoo, Kim
* Creation Date     : 2003.11.26.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K901973
* Addl Documentation:
* Description       : Read available Material master in  *
*                     present visual point.
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZBOM_MATERIAL_DISMANTLE
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: MARA,
        MARC,
        MAKT,
        ZTBM_ABXMMODT,
        ZSBM_ABXMMODT_RFC.
*----------------------------------------------------------------------*
* INTERNAL TABLES  DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MARA OCCURS 0,
        MATNR      LIKE MARA-MATNR,
        WERKS      LIKE MARC-WERKS,
        MTART      LIKE MARA-MTART,
        MBRSH      LIKE MARA-MBRSH,
        MAKTX      LIKE MAKT-MAKTX,
        MEINS      LIKE MARA-MEINS,
        MATKL      LIKE MARA-MATKL,
        MTPOS_MARA LIKE MARA-MTPOS_MARA,
        PROFL      LIKE MARA-PROFL,
        KZKFG      LIKE MARA-KZKFG,
        DISMM      LIKE MARC-DISMM,
        DISPO      LIKE MARC-DISPO,
        DISLS      LIKE MARC-DISLS,
        BESKZ      LIKE MARC-BESKZ,
        FHORI      LIKE MARC-FHORI,
        MTVFP      LIKE MARC-MTVFP,
        STDPD      LIKE MARC-STDPD,
        ALTSL      LIKE MARC-ALTSL,
        SBDKZ      LIKE MARC-SBDKZ,
        VERKZ      LIKE MARC-VERKZ,
      END   OF IT_MARA.
DATA: IT_MMOT TYPE ZTBM_ABXMMODT OCCURS 0 WITH HEADER LINE.
DATA: IT_MRFC TYPE ZSBM_ABXMMODT_RFC OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
*  CONSTANS DECLARATION
*----------------------------------------------------------------------*
DATA : C_DEST(10) VALUE 'WMBOM01'.   "Outbound Interface Destination
DATA: WA_MRFC TYPE ZSBM_ABXMMODT_RFC.
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I,
      WA_LINES TYPE SY-TABIX,
      WA_DIV TYPE I VALUE '500'.
*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
RANGES: RT_MTART FOR MARA-MTART.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_MATNR FOR MARC-MATNR,
                S_WERKS FOR MARC-WERKS.
*PARAMETERS: P_DATUM LIKE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN END   OF BLOCK B2.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM RANGE_APPEND_MTART.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
*  PERFORM WRITE_PROCESS.
*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  RANGE_APPEND_MTART
*&---------------------------------------------------------------------*
FORM RANGE_APPEND_MTART.
  RT_MTART-LOW    = 'FERT'.
  RT_MTART-SIGN   = 'I'.
  RT_MTART-OPTION = 'EQ'.
  APPEND RT_MTART. CLEAR RT_MTART.
  RT_MTART-LOW    = 'HALB'.
  RT_MTART-SIGN   = 'I'.
  RT_MTART-OPTION = 'EQ'.
  APPEND RT_MTART. CLEAR RT_MTART.
  RT_MTART-LOW    = 'ROH'.
  RT_MTART-SIGN   = 'I'.
  RT_MTART-OPTION = 'EQ'.
  APPEND RT_MTART. CLEAR RT_MTART.
ENDFORM.                    " RANGE_APPEND_MTART
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  PERFORM READ_MARA_JOIN_MARC.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_MARA_JOIN_MARC
*&---------------------------------------------------------------------*
FORM READ_MARA_JOIN_MARC.
  SELECT A~MATNR
         B~WERKS
         A~MTART
         A~MBRSH
         C~MAKTX
         A~MEINS
         A~MATKL
         A~MTPOS_MARA
         A~PROFL
         A~KZKFG
         B~DISMM
         B~DISPO
         B~DISLS
         B~BESKZ
         B~FHORI
         B~MTVFP
         B~STDPD
         B~ALTSL
         B~SBDKZ
         B~VERKZ
       FROM MARA AS A INNER JOIN MARC AS B
                      ON A~MATNR EQ B~MATNR
                      INNER JOIN MAKT AS C
                      ON A~MATNR EQ C~MATNR
       INTO TABLE IT_MARA
       WHERE A~MATNR IN S_MATNR
       AND   B~WERKS IN S_WERKS
       AND   A~MTART IN RT_MTART
       AND   C~SPRAS EQ SY-LANGU.
  IF SY-SUBRC NE 0.
    WRITE: / 'NO DATA'.
  ENDIF.
ENDFORM.                    " READ_MARA_JOIN_MARC
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  PERFORM APPEND_IT_MARA_TO_IT_MMOT.
  PERFORM DELETE_ZTBM_ABXMMODT.
  PERFORM INSERT_ZTBM_ABXMMODT.
  PERFORM Z_FBM_ABXMMODT.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  APPEND_IT_MARA_TO_IT_MMOT
*&---------------------------------------------------------------------*
FORM APPEND_IT_MARA_TO_IT_MMOT.
  LOOP AT IT_MARA.
    IT_MMOT-MTNO = IT_MARA-MATNR.
    IT_MMOT-PLNT = IT_MARA-WERKS.
    IT_MMOT-MTYP = IT_MARA-MTART.
    IT_MMOT-INDU = IT_MARA-MBRSH.
    IT_MMOT-ZDESC = IT_MARA-MAKTX.
    IT_MMOT-UNIT = IT_MARA-MEINS.
    IT_MMOT-MGRP = IT_MARA-MATKL.
    IT_MMOT-GICA = IT_MARA-MTPOS_MARA.
    IT_MMOT-SOUR = IT_MARA-PROFL.
    IT_MMOT-MTCN = IT_MARA-KZKFG.
    IT_MMOT-MRPY = IT_MARA-DISMM.
    IT_MMOT-MRPC = IT_MARA-DISPO.
    IT_MMOT-LOTS = IT_MARA-DISLS.
    IT_MMOT-PRTY = IT_MARA-BESKZ.
    IT_MMOT-SMKY = IT_MARA-FHORI.
    IT_MMOT-AVCK = IT_MARA-MTVFP.
    IT_MMOT-CNMT = IT_MARA-STDPD.
    IT_MMOT-SLMD = IT_MARA-ALTSL.
    IT_MMOT-INCO = IT_MARA-SBDKZ.
    IT_MMOT-VESN = IT_MARA-VERKZ.
    IT_MMOT-ZUSER = SY-UNAME.

    IT_MMOT-ZSDAT = SY-DATUM.
    IT_MMOT-ZSTIM = SY-UZEIT.
    APPEND IT_MMOT.
    CLEAR: IT_MARA, IT_MMOT.
  ENDLOOP.
ENDFORM.                    " APPEND_IT_MARA_TO_IT_MMOT
*&---------------------------------------------------------------------*
*&      Form  DELETE_ZTBM_ABXMMODT
*&---------------------------------------------------------------------*
FORM DELETE_ZTBM_ABXMMODT.
  DATA: LT_MMOT TYPE ZTBM_ABXMMODT OCCURS 0 WITH HEADER LINE.
  SELECT *
       FROM ZTBM_ABXMMODT
       INTO TABLE LT_MMOT.
  IF SY-SUBRC EQ 0.
    DELETE ZTBM_ABXMMODT FROM TABLE LT_MMOT.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
    ELSE.
      WRITE: / 'TABLE ZTBM_ABXMMODT DELETE ERROR'.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " DELETE_ZTBM_ABXMMODT
*&---------------------------------------------------------------------*
*&      Form  INSERT_ZTBM_ABXMMODT
*&---------------------------------------------------------------------*
FORM INSERT_ZTBM_ABXMMODT.
  INSERT ZTBM_ABXMMODT FROM TABLE IT_MMOT.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    WRITE: / 'TABLE ZTBM_ABXMMODT INSERT ERROR'.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " INSERT_ZTBM_ABXMMODT
*&---------------------------------------------------------------------*
*&      Form  Z_FBM_ABXMMODT
*&---------------------------------------------------------------------*
FORM Z_FBM_ABXMMODT.
  DATA L_MSGTXT(200).
  DATA: LT_MRFC TYPE ZSBM_ABXMMODT_RFC OCCURS 0 WITH HEADER LINE.
  DATA: L_CURR   TYPE NUM9,
        L_TIMES TYPE I,
        L_MOD   TYPE I,
        L_INDEX TYPE SY-INDEX,
        L_FROM TYPE I VALUE '0',
        L_TO TYPE I VALUE '0',
        L_TOTAL TYPE CHAR1,
        L_TABIX TYPE SY-TABIX.
  PERFORM READ_ZTBM_ABXMMODT.
***************
  IF NOT IT_MRFC[] IS INITIAL.
    DESCRIBE TABLE IT_MRFC LINES WA_LINES.
*    LA_LINES = WA_LINES.
    L_TIMES = WA_LINES DIV WA_DIV.
    L_MOD   = WA_LINES MOD WA_DIV.

    IF NOT L_MOD IS INITIAL.
      L_TIMES = L_TIMES + 1.
    ENDIF.
    L_TOTAL = L_TIMES.
    DO L_TIMES TIMES.
      L_INDEX = SY-INDEX.
      L_CURR = L_INDEX.
      L_FROM = L_TO + 1.
      L_TO   = WA_DIV * L_INDEX.

      IF L_TIMES EQ L_INDEX.
        L_TO = WA_LINES.
      ENDIF.
      REFRESH LT_MRFC. CLEAR LT_MRFC.
      LOOP AT IT_MRFC FROM L_FROM TO L_TO.
        LT_MRFC = IT_MRFC.
        APPEND LT_MRFC.
        CLEAR: IT_MRFC, LT_MRFC.
      ENDLOOP.
      CALL FUNCTION 'Z_FBM_ABXMMODT'
         DESTINATION  C_DEST
        IMPORTING
          TOTAL_C  = L_TOTAL
          CURR_C   = L_CURR
         TABLES
           T_ABXMMODT       = LT_MRFC
        EXCEPTIONS
         COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
         SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.


      IF SY-SUBRC <> 0.
        MESSAGE S000 WITH L_MSGTXT.
        WRITE: / L_MSGTXT.
        EXIT.
      ELSE.
        LOOP AT LT_MRFC.
          L_TABIX = SY-TABIX.
          L_TABIX = L_FROM - 1 + L_TABIX.
          IT_MRFC = LT_MRFC.
          MODIFY IT_MRFC INDEX L_TABIX TRANSPORTING ZEDAT
                                                    ZETIM
                                                    ZMSG
                                                    ZZRET.
          CLEAR: IT_MRFC, LT_MRFC.
        ENDLOOP.
        READ TABLE LT_MRFC WITH KEY ZZRET = 'E'.
        IF SY-SUBRC EQ 0.
          MESSAGE S000 WITH LT_MRFC-ZMSG.
          WRITE: / L_MSGTXT.
          EXIT.
        ELSE.
          MESSAGE S000 WITH '  Successful in transmission to EAI'.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
  LOOP AT IT_MRFC.
    IF IT_MRFC-ZZRET EQ 'E'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
    ELSE.
      WA_LINE_IDX = WA_LINE_IDX + 1.
    ENDIF.
  ENDLOOP.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / 'Material OUTBOUND TOTAL LINES : ', WA_LINE_IDX.
    WRITE: / 'Material OUTBOUND ERROR TOTAL LINES : ', WA_ERRO_IDX.
    FORMAT COLOR OFF.
    SORT IT_MRFC BY ZZRET MTNO PLNT.
    WRITE: /(10) 'PLANT',
            (20) 'MATNRIAL TYPE',
            (20) 'MATNRIAL/NO',
            (40) 'MATNRIAL NAME',
            (10) 'INDUS/SECT',
            (03) 'U/M',
            (06) 'M/GROU',
            (12) 'GEN/I/CAT/GRO',
            (18) 'INDU/STAN/DESCR/',
            (10) 'CONIFGU/MAT',
            (07) 'MRPTYPE',
            (07) 'CONTROL',
            (07) 'LOTSIZE',
            (07) 'PRO/TYP',
            (10) 'SCH/MAR/K',
            (10) 'AVAIL/CHK',
            (18) 'CON/MATERI',
            (10) 'METHOD FOR',
            (10) 'DEPEN/REQU',
            (11) 'VERSION/IND',
            (10) 'ERROR TYPE',
           (220) 'MESSAGE'.
    LOOP AT IT_MRFC WHERE ZZRET EQ 'E'.
      WRITE: /(10) IT_MRFC-PLNT,
              (20) IT_MRFC-MTYP,
              (20) IT_MRFC-MTNO,
              (40) IT_MRFC-ZDESC,
              (10) IT_MRFC-INDU,
              (03) IT_MRFC-UNIT,
              (06) IT_MRFC-MGRP,
              (12) IT_MRFC-GICA,
              (18) IT_MRFC-SOUR,
              (10) IT_MRFC-MTCN,
              (07) IT_MRFC-MRPY,
              (07) IT_MRFC-MRPC,
              (07) IT_MRFC-LOTS,
              (07) IT_MRFC-PRTY,
              (10) IT_MRFC-SMKY,
              (10) IT_MRFC-AVCK,
              (18) IT_MRFC-CNMT,
              (10) IT_MRFC-SLMD,
              (10) IT_MRFC-INCO,
              (11) IT_MRFC-VESN,
              (10) IT_MRFC-ZRESULT,
                   IT_MRFC-ZMSG.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " Z_FBM_ABXMMODT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_ABXMMODT
*&---------------------------------------------------------------------*
FORM READ_ZTBM_ABXMMODT.
  REFRESH: IT_MRFC, IT_MMOT. CLEAR: IT_MRFC, IT_MMOT.
  SELECT *
       FROM ZTBM_ABXMMODT
       INTO TABLE IT_MMOT.
  IF SY-SUBRC EQ 0.
    LOOP AT IT_MMOT.
      MOVE-CORRESPONDING IT_MMOT TO IT_MRFC.
      APPEND IT_MRFC.
      CLEAR: IT_MRFC, IT_MMOT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " READ_ZTBM_ABXMMODT
