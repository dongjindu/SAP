***********************************************************************
* Data declaration and screen handling for report RFKQSU40
***********************************************************************

*Note 454478 BEGIN
TABLES: BSAK, *BSAK, LFA1, *LFA1,  LFB1, *LFB1, KNA1, *KNA1, BSIK,
        LFBW, *LFBW.
TABLES: T001, *T001, T001Z, SADR, *SADR, ADDR1_SEL,
        T005, *T005, T005T, *T005T, T005R, *T005R,
        T005U, T059Q, T059F, *T059F, t005a, *t005a,
        WITH_ITEM, T059P, T059Z, T059FB, T059FBH.
*Note 454478 END

DATA: BEGIN OF AGENTTAB OCCURS 20,
        BUKRS LIKE T001-BUKRS,
        WAERS LIKE T001-WAERS,
        LAND1 LIKE T001-LAND1,
        SPRAS LIKE T001-SPRAS,
        ADRNR LIKE T001-ADRNR,                      "SADR40A
        BUTXT LIKE T001-BUTXT,
        ADDRS LIKE T005-ADDRS,
        COUNT TYPE I,
END OF AGENTTAB,

AGENTTAB_IDX LIKE SY-TABIX.

DATA: BEGIN OF X059 OCCURS 1000,
        LAND1 LIKE T059Q-LAND1,
        QLAND LIKE T059F-QLAND,
        WAERS LIKE T059F-WAERS,
        QSCOD LIKE T059Q-QSCOD,
        QSSKZ LIKE T059Q-QSSKZ,
        QSATZ LIKE T059F-QSATZ,
        QMBAB LIKE T059F-QMBAB,
      END OF X059.

DATA: "LFB1 is read one time to check if the vendor is relevant for 1042
      BEGIN OF KTAB OCCURS 1000,
        BUKRS LIKE LFB1-BUKRS,
        LIFNR LIKE LFB1-LIFNR,
        QSZDT LIKE LFB1-QSZDT,
        QSREC LIKE LFB1-QSREC,         "line 4 on 1042s
*       QSBGR LIKE LFB1-QSBGR,         "column f on 1042s
        qsbgr like lfbw-wt_wtexrs,
        QLAND LIKE LFB1-QLAND,         "column h on 1042s
      END OF KTAB.

DATA: BEGIN OF RTAB OCCURS 10000,
        BUKRS LIKE BSAK-BUKRS,
        LIFNR LIKE BSAK-LIFNR,
        QSCOD LIKE T059Q-QSCOD,
        QSSKZ LIKE BSAK-QSSKZ,         "trigger for column a on 1042s
        QSSHH LIKE BSAK-QSSHB,         "column b on form 1042s
        QBSHH LIKE BSAK-QBSHB,
        ALLOW LIKE BSAK-QSSHB,         "Allowance for Income Type 15
      END OF RTAB.

DATA:
* Note 454478 begin
      BEGIN OF FORM1042 OCCURS 780,    "Data required for 1042
* Note 454478 end
        BUKRS    LIKE T001-BUKRS,      "1042 is summary of 1042s
        MONTH(2) TYPE N,
        DAY(2)   TYPE N,
        AMOUNT   LIKE BSAK-DMBTR,
      END OF FORM1042.

DATA:
* Note 454478 begin
      BEGIN OF 1042_REC OCCURS 780,    "Data required for 1042
        FIEL1042(780) TYPE C,
* Note 454478 end
      END OF 1042_REC.

DATA:
     CONTACT-PERSON(40)        TYPE C  VALUE ' ',   "For T record
     CONTACT-PERSON-TEL(20)    TYPE C  VALUE ' ',   "For T record
     FLAG                      TYPE I  VALUE 0,
     D_X                       TYPE I,

*Note 492783 begin
      w_bsak_qsfbt(10)        type c value ' ',
      *w_bsak_qsfbt(10)       type c value ' ',
      w_bsak_dmbtr(11)        type c value ' ',
      *w_bsak_dmbtr(11)       type c value ' ',
*Note 492783 end
*Note 497879 begin
      tr_zip1  like lfa1-pstlz,
      lfa1-pstlz1 like lfa1-pstlz,
      sadr-pstlz1 like lfa1-pstlz.
*Note 497879 end
* Note 454478 begin
DATA:
      T(780),
      W(780),
      Q(780),
      C(780),
      F(780).
*Note 454478 end.

DATA: X_WITH_ITEM   LIKE WITH_ITEM  OCCURS 1 WITH HEADER LINE,
      X_LFBW        LIKE LFBW OCCURS 1 WITH HEADER LINE,
      OLD_WITH      TYPE C,
      NEW_WITH      TYPE C,
      CONTINUE_FLAG TYPE C,
      COUNTRY LIKE T005-LAND1,
      TIN_TYPE,
      TIN_CODE(9),
      REC_COUNT TYPE I.

DATA: FILE_ERROR VALUE '0',
      TAPENAME    LIKE HEADA-DSN,
      PAGETYPE,

      SAPFORMS   LIKE RSSCF-TDFORM
                 VALUE 'ZF_RFKQSU40_10',
      BEGIN OF PRINT.
        INCLUDE STRUCTURE ITCPO.
DATA: END OF PRINT.
*Note 454478 BEGIN.
DATA: GD_ELEMENT(14) TYPE C,
      GD_WINDOW(14) TYPE C.
*Note 454478 END.

DATA: rounded_amount type p decimals 0.
*Note 803878
DATA: file_count type p decimals 0.
DATA: g_count(8) type c.
*Note 803878

*eject ------------------------
RANGES:
      LIFNR FOR BSAK-LIFNR,
      QSTKZ FOR BSIK-QSSKZ,
* begin of deletion: note 178383
*      KD_BUKRS FOR BSAK-BUKRS.
* end of deletion: note 178383
* begin of insertion: note 178383
      KD_BUKRS FOR BSAK-BUKRS,
      QSTKZ_EXT FOR T059Z-WT_WITHCD.
* end of insertion: note 178383

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-S08.
SELECT-OPTIONS:
      BUKRS FOR BSAK-BUKRS MEMORY ID BUK OBLIGATORY,
      WITHT FOR WITH_ITEM-WITHT NO-EXTENSION
                                  NO INTERVALS,

*Note 497879 begin
    ext_wtcd for with_item-wt_withcd,  " extended WT code
    cls_wtcd for bsak-qsskz.           " classical WT code
*Note 497879 end
PARAMETERS:
      FISCAL LIKE RFPDO1-KQSUFISC      "Calender Year, 1 year at a time
             OBLIGATORY.

SELECTION-SCREEN END OF BLOCK 1.

SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-S09.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-S06.
PARAMETERS:
      PRNTFORM LIKE RFPDO1-KQSUPRNX.
SELECTION-SCREEN COMMENT 35(25) TEXT-S07.
PARAMETERS:
      TESTPRNT LIKE RFPDO-FORDANZP
               DEFAULT 2.
SELECTION-SCREEN END OF LINE.
PARAMETERS:
      RECO1042 LIKE RFPDO1-KQSURECX,
      DETAILRP LIKE RFPDO1-KQSUDTLX,
      CREATFIL LIKE RFPDO1-KQSUFILX,
* Note 454478 begin
*      fpath   like rlgrap-filename  modif id mc6,  " file path
      TESTFIL LIKE RFPDO1-ALLGTEST.     "              for T-record
* Note 454478 end
SELECTION-SCREEN END OF BLOCK 2.

*----- Additional data for payer ---------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK 3 WITH FRAME TITLE TEXT-S03.
PARAMETER:
      PY_NAME LIKE RFPDO1-KQSURNAM, "payer's name if different from agnt
      PY_TIN LIKE RFPDO1-KQSUPTIN. "Payer's TIN if different from agent
SELECTION-SCREEN END OF BLOCK 3.

*----- Additional data for transmitter --------------------------------
SELECTION-SCREEN BEGIN OF BLOCK 4 WITH FRAME TITLE TEXT-S04.
PARAMETER:
      TR_NAME LIKE RFPDO1-KQSUNAME,    "                for T-record
      TR_ADDR LIKE RFPDO1-KQSUADDR.    "                for T-record
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-S02 FOR FIELD TR_CITY.
PARAMETERS:
      TR_CITY LIKE RFPDO1-KQSUCITY,    "for T-record
      TR_STAT LIKE RFPDO1-KQSUSTAT,    "for T-record
      TR_ZIP LIKE RFPDO1-KQSUTZIP.
SELECTION-SCREEN END OF LINE.
PARAMETERS:
      TR_TIN LIKE RFPDO1-KQSUTTIN,     "for T-record
      TR_TCC LIKE RFPDO1-KQSUTRCC,     "Transmitter Control Code TCC
* Note 454478 begin
      CONTACT LIKE CONTACT-PERSON,     "Contact person
      TEL LIKE CONTACT-PERSON-TEL.     "Telephone of contact person
* Note 454478 end
SELECTION-SCREEN END OF BLOCK 4.
*eject
AT SELECTION-SCREEN ON DETAILRP.
  IF NOT DETAILRP CO ' 12'.
    MESSAGE E600 WITH TEXT-I01.
  ENDIF.

AT SELECTION-SCREEN ON TR_NAME.
  IF NOT CREATFIL IS INITIAL AND TR_NAME IS INITIAL.
    MESSAGE E600 WITH TEXT-I09.
  ENDIF.

AT SELECTION-SCREEN ON TR_ADDR.
  IF NOT CREATFIL IS INITIAL AND TR_ADDR IS INITIAL.
    MESSAGE E600 WITH TEXT-I09.
  ENDIF.

AT SELECTION-SCREEN ON TR_CITY.
  IF NOT CREATFIL IS INITIAL AND TR_CITY IS INITIAL.
    MESSAGE E600 WITH TEXT-I09.
  ENDIF.

AT SELECTION-SCREEN ON TR_STAT.
  IF NOT CREATFIL IS INITIAL AND TR_STAT IS INITIAL.
    MESSAGE E600 WITH TEXT-I09.
  ENDIF.

AT SELECTION-SCREEN ON TR_ZIP.
  IF NOT CREATFIL IS INITIAL AND TR_ZIP IS INITIAL.
    MESSAGE E600 WITH TEXT-I09.
  ENDIF.

AT SELECTION-SCREEN ON TR_TIN.
  IF NOT CREATFIL IS INITIAL AND TR_TIN IS INITIAL.
    MESSAGE E600 WITH TEXT-I09.
  ENDIF.

AT SELECTION-SCREEN ON TR_TCC.
  IF NOT CREATFIL IS INITIAL AND TR_TCC IS INITIAL.
    MESSAGE E600 WITH TEXT-I09.
  ENDIF.
