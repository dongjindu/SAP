************************************************************************
* Program Name      : ZEMMPM36E_IDEL_CHGPO_ORIG_NEW
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.10.24.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K902658
* Addl Documentation:
* Description       : Inbound Delivery Create - KDWeb ASN
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.24.     Sung-Tae Lim     UD1K902658     Initial Coding
* 03/17/2005      Shiva            UD1K915000     Update error message
*                                                 if error occurs while
*                                                 creating delivery.
* 03/17/2005      Shiva            UD1K915032     Update error message
*                                                 if error occurs while
*                                               reading PO information.
* 03/21/2005      Shiva            UD1K915099   Type conflict for BAPI
*                                               reading error message.
* 04/25/2005      BSBAE            UD1K915715   Multiful Reprocessing
*                                               Probelm
* 01/28/2007      Manju            UD1K930445   Enhance KD ASN Process
*                                               to include BOL# in
*                                               delivery header.
* 10/06/2008      Vijay           UD1K944653    Dupilcate entries with
*                                               deletion indicator for
*                                               one item
* 02/26/2010      Furong          UD1K948453   copy from
*                                              ZEMMPM36E_IDEL_CHGPO_ORIG
*                                              for foreground process,
*                                              add BOL re-process
*                                              function
************************************************************************
REPORT ZEMMPM36E_IDEL_CHGPO_ORIG_NEW MESSAGE-ID ZMMM.
INCLUDE: <ICON>.

**---
INCLUDE : ZRMMPMXXR_INCL.

TABLES: ZTBLHD_INF.

DATA: ZSMM_ZEMMPM36E_IDEL_9000_NEW LIKE ZSMM_ZEMMPM36E_IDEL_9000_NEW.

**--- Tables, Views & Structures
DATA: IT_9000 TYPE STANDARD TABLE OF ZSMM_ZEMMPM36E_IDEL_9000_NEW
                                     WITH HEADER LINE.

DATA: IT_9000_TAR LIKE IT_9000 OCCURS 0 WITH HEADER LINE.

DATA: IT_9000_CHPO LIKE IT_9000 OCCURS 0 WITH HEADER LINE.

DATA: IT_9100 LIKE IT_9000 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_EKPO_SHORT OCCURS 0,
        EBELN LIKE EKPO-EBELN,
        EBELP LIKE EKPO-EBELP,
        WERKS LIKE EKPO-WERKS,
        LGORT LIKE EKPO-LGORT,
        MENGE LIKE EKPO-MENGE,
        MEINS LIKE EKPO-MEINS,
        LMEIN LIKE EKPO-LMEIN,
        UMREZ LIKE EKPO-UMREZ,
        UMREN LIKE EKPO-UMREN,
        MATNR LIKE EKPO-MATNR,
        EMATN LIKE EKPO-EMATN,                              "386409
        MFRNR LIKE EKPO-MFRNR,
        MFRPN LIKE EKPO-MFRPN,
        EMNFR LIKE EKPO-EMNFR,
        CUOBJ LIKE EKPO-CUOBJ,
        UEBTO LIKE EKPO-UEBTO,
        UNTTO LIKE EKPO-UNTTO,
        UEBTK LIKE EKPO-UEBTK,
        BWTAR LIKE EKPO-BWTAR,
        IDNLF LIKE EKPO-IDNLF,
        TXZ01 LIKE EKPO-TXZ01,
        MFRGR LIKE EKPO-MFRGR,
        GEWEI LIKE EKPO-GEWEI,
        VOLEH LIKE EKPO-VOLEH,
        NTGEW LIKE EKPO-NTGEW,
        BRGEW LIKE EKPO-BRGEW,
        VOLUM LIKE EKPO-VOLUM,
        EAN11 LIKE EKPO-EAN11,
        AKTNR LIKE EKPO-AKTNR,
        ABELN LIKE EKPO-ABELN,
        ABELP LIKE EKPO-ABELP,
        AUREL LIKE EKPO-AUREL,
        MATKL LIKE EKPO-MATKL,
        UPVOR LIKE EKPO-UPVOR,
        UPTYP LIKE EKPO-UPTYP,
        UEBPO LIKE EKPO-UEBPO,
        BSTAE LIKE EKPO-BSTAE,
        WEPOS LIKE EKPO-WEPOS,
        LOEKZ LIKE EKPO-LOEKZ,
        ELIKZ LIKE EKPO-ELIKZ,
        ANLMG LIKE EKPO-MENGE,
        INSMK LIKE EKPO-INSMK,
        PSTYP LIKE EKPO-PSTYP,
        SOBKZ LIKE EKPO-SOBKZ,
        KZVBR LIKE EKPO-KZVBR,            "note 384051
        KNTTP LIKE EKPO-KNTTP,
        KZFME LIKE EKPO-KZFME,
      END OF IT_EKPO_SHORT.

DATA: BEGIN OF IT_EKPO_SHORT1 OCCURS 0,
        EBELN LIKE EKPO-EBELN,
        EBELP LIKE EKPO-EBELP,
        WERKS LIKE EKPO-WERKS,
        LGORT LIKE EKPO-LGORT,
        MENGE LIKE EKPO-MENGE,
        MEINS LIKE EKPO-MEINS,
        LMEIN LIKE EKPO-LMEIN,
        UMREZ LIKE EKPO-UMREZ,
        UMREN LIKE EKPO-UMREN,
        MATNR LIKE EKPO-MATNR,
        EMATN LIKE EKPO-EMATN,                              "386409
        MFRNR LIKE EKPO-MFRNR,
        MFRPN LIKE EKPO-MFRPN,
        EMNFR LIKE EKPO-EMNFR,
        CUOBJ LIKE EKPO-CUOBJ,
        UEBTO LIKE EKPO-UEBTO,
        UNTTO LIKE EKPO-UNTTO,
        UEBTK LIKE EKPO-UEBTK,
        BWTAR LIKE EKPO-BWTAR,
        IDNLF LIKE EKPO-IDNLF,
        TXZ01 LIKE EKPO-TXZ01,
        MFRGR LIKE EKPO-MFRGR,
        GEWEI LIKE EKPO-GEWEI,
        VOLEH LIKE EKPO-VOLEH,
        NTGEW LIKE EKPO-NTGEW,
        BRGEW LIKE EKPO-BRGEW,
        VOLUM LIKE EKPO-VOLUM,
        EAN11 LIKE EKPO-EAN11,
        AKTNR LIKE EKPO-AKTNR,
        ABELN LIKE EKPO-ABELN,
        ABELP LIKE EKPO-ABELP,
        AUREL LIKE EKPO-AUREL,
        MATKL LIKE EKPO-MATKL,
        UPVOR LIKE EKPO-UPVOR,
        UPTYP LIKE EKPO-UPTYP,
        UEBPO LIKE EKPO-UEBPO,
        BSTAE LIKE EKPO-BSTAE,
        WEPOS LIKE EKPO-WEPOS,
        LOEKZ LIKE EKPO-LOEKZ,
        ELIKZ LIKE EKPO-ELIKZ,
        ANLMG LIKE EKPO-MENGE,
        INSMK LIKE EKPO-INSMK,
        PSTYP LIKE EKPO-PSTYP,
        SOBKZ LIKE EKPO-SOBKZ,
        KZVBR LIKE EKPO-KZVBR,            "note 384051
        KNTTP LIKE EKPO-KNTTP,
        KZFME LIKE EKPO-KZFME,
      END OF IT_EKPO_SHORT1.

DATA : ST_VBSK LIKE VBSK.

DATA : IT_KOMDLGN LIKE KOMDLGN OCCURS 0 WITH HEADER LINE,
       IT_VBFS    LIKE VBFS    OCCURS 0 WITH HEADER LINE,
       IT_VBLS    LIKE VBLS    OCCURS 0 WITH HEADER LINE,
       IT_LIPS    LIKE LIPS    OCCURS 0 WITH HEADER LINE.

DATA : IT_EKKN LIKE EKKN OCCURS 0 WITH HEADER LINE.

DATA : WA_ITAB LIKE IT_9000.

DATA : IT_XEKET LIKE BEKET OCCURS 0 WITH HEADER LINE.


*----- BDC
DATA : BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF IT_BDC.

DATA : BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF IT_MESS.

DATA : IT_MESSAGE LIKE IT_MESS OCCURS 0 WITH HEADER LINE.


**--- Variables
DATA : W_MODE LIKE CTU_PARAMS-DISMODE VALUE 'A'.

DATA:      GF_DLV_TYPE LIKE LIKP-LFART. "Delivery Type
DATA:      GF_EBTYP    LIKE T163D-EBTYP.                    "386409

DATA : W_QTY_SUM LIKE IT_KOMDLGN-LFIMG,
       W_SUCCESS(4)   TYPE   N,
       W_ERROR(4)     TYPE   N,
       W_READY(4)     TYPE   N,
       W_TOTAL(4)     TYPE   N.

* Begin of changes -  UD1K930445
DATA: BEGIN OF WA_ASN,
      TRAID(11) TYPE C,
      CINVO(11) TYPE C,
      END OF WA_ASN.
DATA: BEGIN OF WA_BOL_INF,
         CINVO(11) TYPE C,
         TRAID(11) TYPE C,
         ZFHBLNO(16) TYPE C,                                "UD1K919888
        END OF WA_BOL_INF.
DATA: IT_ASN LIKE TABLE OF WA_ASN,
      IT_BOL_INF LIKE TABLE OF WA_BOL_INF WITH HEADER LINE.

FIELD-SYMBOLS: <FS_KDASN> LIKE LINE OF IT_9000.

DATA: WA_9000 LIKE ZSMM_ZEMMPM36E_IDEL_9000_NEW,
          IT_ERR_9000 LIKE TABLE OF WA_9000,
          LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE.

* End of changes -  UD1K930445


CONSTANTS : C_IBTYP LIKE T163D-IBTYP VALUE '2',
            C_CHECK                  VALUE 'X'.

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

DEFINE APPEND_SORTCAT.
  W_SORTCAT-SPOS      = &1.
  W_SORTCAT-FIELDNAME = &2.
  W_SORTCAT-TABNAME   = &3.
  W_SORTCAT-UP        = &4.
  W_SORTCAT-SUBTOT    = &5.
  APPEND W_SORTCAT.
  CLEAR : W_SORTCAT.
END-OF-DEFINITION.


*-----/// ALV Control : START
* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: WC_CONTROL_9000   TYPE        SCRFNAME VALUE 'CC_9000_ALV',
      WC_ALV_9000       TYPE REF TO CL_GUI_ALV_GRID,
      WC_CONTAINER_9000 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: W_CONTAINER(50),
      W_CONTROL(50),
      W_ALV(50),
      W_ITAB(50),
      W_STRUCTURE LIKE DD02L-TABNAME.

FIELD-SYMBOLS: <CONTAINER> TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
               <CONTROL>   TYPE        SCRFNAME,
               <ALV>       TYPE REF TO CL_GUI_ALV_GRID,
               <ITAB>      TYPE STANDARD TABLE.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED. "/ALV Event Handling

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* Interal tables for ALV GRID
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

* Global variable for ALV GRID
DATA : W_IS_LAYOUT TYPE LVC_S_LAYO,
       W_VARIANT   TYPE DISVARIANT,          "for parameter IS_VARIANT
       W_FIELDNAME LIKE LINE OF IT_FIELDCAT,
       W_REPID     LIKE SY-REPID,
       W_CNT       TYPE I,                   "Field count
       W_SAVE      TYPE C   VALUE 'A'.   "for Parameter I_SAVE

CONSTANTS: C_STRUCTURE(100) VALUE 'ZSMM_ZEMMPM36E_IDEL_'.

*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

*-----/// ALV Control : END

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS LCL_EVENT_RECEIVER DEFINITION.
*  PUBLIC SECTION.
*    METHODS:
*
*    handle_double_click
*        FOR EVENT double_click OF cl_gui_alv_grid
*            IMPORTING e_row
*                      e_column
*                      es_row_no.
*
*    handle_user_command
*        FOR EVENT user_command OF cl_gui_alv_grid
*            IMPORTING e_ucomm,
*
*    handle_data_changed
*        FOR EVENT data_changed OF cl_gui_alv_grid
*            IMPORTING er_data_changed
*                      e_onf4
*                      e_onf4_before
*                      e_onf4_after.
ENDCLASS.

**---
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_ZEDAT FOR ZTMM_KD_ASN_MAIN-ZEDAT NO-EXTENSION
                             DEFAULT SY-DATUM.
SELECT-OPTIONS : S_ZINV  FOR ZTMM_KD_ASN_MAIN-ZINVOICE.
*SELECT-OPTIONS : s_zslno FOR ztmm_kd_asn_main-zslno.
SELECT-OPTIONS : S_ZFHBL FOR ZTBLHD_INF-ZFHBLNO.
SELECTION-SCREEN SKIP.
* Begin of changes - UD1K930445
PARAMETER:       P_UPDATE AS CHECKBOX.                      "UD1K930445
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 5.
*PARAMETERS       r1 RADIOBUTTON GROUP radi .
*SELECTION-SCREEN POSITION 7.
*SELECTION-SCREEN COMMENT 8(10) text-003 FOR FIELD r1.
SELECTION-SCREEN POSITION 25.
PARAMETERS       R2 RADIOBUTTON GROUP RADI DEFAULT 'X'.
SELECTION-SCREEN POSITION 26.
SELECTION-SCREEN COMMENT 27(25) TEXT-004 FOR FIELD R2.
SELECTION-SCREEN POSITION 55.
PARAMETERS       R3 RADIOBUTTON GROUP RADI.
SELECTION-SCREEN POSITION 56.
SELECTION-SCREEN COMMENT 57(10) TEXT-005 FOR FIELD R3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLOCK2.

PARAMETERS : P_MODE LIKE CTU_PARAMS-DISMODE DEFAULT 'N'.

START-OF-SELECTION.
  PERFORM GET_DATA.

**---
END-OF-SELECTION.
  IF IT_9000[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.

* Begin of changes - UD1K930445
    IF P_UPDATE EQ C_CHECK.
      PERFORM SET_TARGET_DATA TABLES LT_ROWS.
      PERFORM CREATE_DELIVERY TABLES IT_9000_TAR.
      PERFORM UPDATE_TABLE TABLES IT_9000_TAR.
      PERFORM CREATE_BOL.
    ENDIF.
* End of changes - UD1K919888

    PERFORM COUNT_RTN.
    PERFORM DISPLAY_DATA.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
*---
  CLEAR : IT_9000, IT_9000[].
  DATA: L_RESULT(1).

*--- Processing
*  IF r1 NE space.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_9000
*             FROM ztmm_kd_asn_main
*            WHERE zslno    IN s_zslno
*              AND zedat    IN s_zedat
*              AND zinvoice IN s_zinv
*              AND zresult  EQ space.
*  ENDIF.

*--- Re-Processing(Error)
  IF R2 NE SPACE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_9000
             FROM ZTMM_KD_ASN_MAIN
*            WHERE zslno    IN s_zslno
              WHERE ZEDAT    IN S_ZEDAT
              AND ZINVOICE IN S_ZINV
              AND  ZRESULT  EQ 'E'.
  ENDIF.

*--- Success
  IF R3 NE SPACE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_9000
             FROM ZTMM_KD_ASN_MAIN
*            WHERE zslno    IN s_zslno
             WHERE ZEDAT    IN S_ZEDAT
              AND ZINVOICE IN S_ZINV
              AND ZRESULT  EQ 'S'.
  ENDIF.

** Changed by Furong on 03/03/10
*  IF R2 = 'X'.
*    L_RESULT = 'E'.
*  ELSE.
*    L_RESULT = 'S'.
*  ENDIF.

 IF S_ZFHBL[] IS INITIAL.
    LOOP AT IT_9000.

      SELECT SINGLE B~ZFHBLNO B~ZMSG B~ZRESULT
        INTO (IT_9000-ZFHBLNO, IT_9000-ZMSG_BOL,
              IT_9000-ZRESULT_BOL)
        FROM ZTBLIT_INF AS A
          INNER JOIN ZTBLHD_INF AS B
          ON A~ZFHBLNO = B~ZFHBLNO
*         INNER JOIN ZTBL AS C
*          ON A~ZFHBLNO = C~ZFHBLNO
       WHERE A~ZFCIVNO = IT_9000-ZINVOICE
         AND B~ZRESULT <> 'I'.

      IF IT_9000-ZFHBLNO IS INITIAL.
      ELSE.
        SELECT SINGLE ZFBLNO INTO IT_9000-ZFBLNO
          FROM ZTBL
          WHERE ZFHBLNO = IT_9000-ZFHBLNO.
      ENDIF.
*      SELECT SINGLE B~ZFHBLNO B~ZMSG B~ZRESULT C~ZFBLNO
*       INTO (IT_9000-ZFHBLNO, IT_9000-ZMSG_BOL,
*             IT_9000-ZRESULT_BOL, IT_9000-ZFBLNO)
*       FROM ZTBLIT_INF AS A
*         INNER JOIN ZTBLHD_INF AS B
*         ON A~ZFHBLNO = B~ZFHBLNO
*         INNER JOIN ZTBL AS C
*          ON A~ZFHBLNO = C~ZFHBLNO
*      WHERE A~ZFCIVNO = IT_9000-ZINVOICE.
*        and b~ZRESULT = 'S'.
*      CASE IT_9000-ZRESULT_BOL.
*        WHEN 'S'.
*          MOVE: ICON_GREEN_LIGHT TO IT_9000-ICON_BOL.
*        WHEN 'E'.
*          MOVE: ICON_RED_LIGHT TO IT_9000-ICON_BOL.
*        WHEN SPACE.
*          MOVE: ICON_YELLOW_LIGHT TO IT_9000-ICON_BOL.
*      ENDCASE.
      IF IT_9000-ZFBLNO IS INITIAL.
        MOVE: ICON_RED_LIGHT TO IT_9000-ICON_BOL.
      ELSE.
        MOVE: ICON_GREEN_LIGHT TO IT_9000-ICON_BOL.
      ENDIF.

      MODIFY IT_9000.
    ENDLOOP.
  ELSE.

    LOOP AT IT_9000.
      SELECT SINGLE B~ZFHBLNO B~ZMSG B~ZRESULT
        INTO (IT_9000-ZFHBLNO, IT_9000-ZMSG_BOL,
              IT_9000-ZRESULT_BOL)
        FROM ZTBLIT_INF AS A
          INNER JOIN ZTBLHD_INF AS B
          ON A~ZFHBLNO = B~ZFHBLNO
       WHERE A~ZFCIVNO = IT_9000-ZINVOICE
         AND B~ZFHBLNO IN S_ZFHBL
         AND B~ZRESULT <> 'I'.

      IF IT_9000-ZFHBLNO IS INITIAL.
        DELETE IT_9000.
      ELSE.
        SELECT SINGLE ZFBLNO INTO IT_9000-ZFBLNO
          FROM ZTBL
          WHERE ZFHBLNO = IT_9000-ZFHBLNO.

*      SELECT SINGLE B~ZFHBLNO B~ZMSG B~ZRESULT C~ZFBLNO
*     INTO (IT_9000-ZFHBLNO, IT_9000-ZMSG_BOL,
*           IT_9000-ZRESULT_BOL, IT_9000-ZFBLNO)
*   FROM ZTBLIT_INF AS A
*     INNER JOIN ZTBLHD_INF AS B
*     ON A~ZFHBLNO = B~ZFHBLNO
*        INNER JOIN ZTBL AS C
*        ON A~ZFHBLNO = C~ZFHBLNO
*  WHERE A~ZFCIVNO = IT_9000-ZINVOICE
*   AND B~ZFHBLNO IN S_ZFHBL
*   AND B~ZRESULT = 'S'.
        IF SY-SUBRC = 0.
*        CASE IT_9000-ZRESULT_BOL.
*          WHEN 'S'.
*            MOVE: ICON_GREEN_LIGHT TO IT_9000-ICON_BOL.
*          WHEN 'E'.
*            MOVE: ICON_RED_LIGHT TO IT_9000-ICON_BOL.
*          WHEN SPACE.
*            MOVE: ICON_YELLOW_LIGHT TO IT_9000-ICON_BOL.
*        ENDCASE.
          IF IT_9000-ZFBLNO IS INITIAL.
            MOVE: ICON_RED_LIGHT TO IT_9000-ICON_BOL.
          ELSE.
            MOVE: ICON_GREEN_LIGHT TO IT_9000-ICON_BOL.
          ENDIF.
        endif.
        MODIFY IT_9000.
      endif.
    ENDLOOP.
  ENDIF.
** End of change on 03/03/10

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  create_delivery
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_DELIVERY TABLES PT_9000 STRUCTURE IT_9000.
*--- sort by Container No.(traid) & Case No.(ematn)
  SORT PT_9000 BY TRAID EMATN.

  LOOP AT PT_9000.
    CLEAR : WA_ITAB.
    MOVE : PT_9000 TO WA_ITAB.
    AT NEW EMATN.     " Case Number
      CLEAR : IT_KOMDLGN, IT_KOMDLGN[].
    ENDAT.
*    PERFORM vbsk_fill.
    PERFORM GET_INBOUND_DELIVERY_TYPE USING '2'.
    PERFORM IT_EKPO_SHORT_FILL.
    PERFORM KOMDLGN_FILL.
    AT END OF EMATN.     " Case Number
      PERFORM CALL_FUNCTION.
      PERFORM MODIFY_ITAB.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " create_delivery
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE TABLES PT_9000 STRUCTURE IT_9000.
*--- update table
  DATA : L_TOTAL TYPE I,
         L_SUCCS TYPE I,
         L_ERROR TYPE I.

  LOOP AT PT_9000.
    READ TABLE IT_9000 WITH KEY TRAID = PT_9000-TRAID
                                EMATN = PT_9000-EMATN
                                MATNR = PT_9000-MATNR
                                EBELN = PT_9000-EBELN.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M04.
    ENDIF.

    CLEAR: ZTMM_KD_ASN_MAIN.
    MOVE-CORRESPONDING IT_9000 TO ZTMM_KD_ASN_MAIN.
    MODIFY ZTMM_KD_ASN_MAIN.
    IF SY-SUBRC NE 0.
      MOVE: 'E'      TO IT_9000-ZRESULT,
            TEXT-009 TO IT_9000-ZMSG.
    ENDIF.

    CASE IT_9000-ZRESULT.
      WHEN 'S'.
        MOVE: ICON_GREEN_LIGHT TO IT_9000-ICON.
      WHEN 'E'.
        MOVE: ICON_RED_LIGHT TO IT_9000-ICON.
      WHEN SPACE.
        MOVE: ICON_YELLOW_LIGHT TO IT_9000-ICON.
    ENDCASE.

    MODIFY IT_9000 INDEX SY-TABIX.
  ENDLOOP.

* Also Update log table for the records which don't
* have  BOL data.
  LOOP AT IT_9000 WHERE ZRESULT = 'E'.
    MOVE-CORRESPONDING IT_9000 TO ZTMM_KD_ASN_MAIN.
    MODIFY ZTMM_KD_ASN_MAIN.
    IF SY-SUBRC NE 0.
      MOVE: 'E'      TO IT_9000-ZRESULT,
            TEXT-010 TO IT_9000-ZMSG.
    ENDIF.
  ENDLOOP.

*--- logging interface table
  DATA : ST_ZTCA_IF_LOG LIKE ZTCA_IF_LOG.

  CLEAR : ST_ZTCA_IF_LOG.

  MOVE : SY-TCODE TO ST_ZTCA_IF_LOG-TCODE,
         L_TOTAL  TO ST_ZTCA_IF_LOG-TOTAL,
         L_SUCCS  TO ST_ZTCA_IF_LOG-ZSUCC,
         L_ERROR  TO ST_ZTCA_IF_LOG-ERROR,
         SY-DATUM TO ST_ZTCA_IF_LOG-ERDAT,
         SY-UZEIT TO ST_ZTCA_IF_LOG-ERZET,
         SY-UNAME TO ST_ZTCA_IF_LOG-ERNAM,
         SY-DATUM TO ST_ZTCA_IF_LOG-AEDAT,
         SY-UZEIT TO ST_ZTCA_IF_LOG-AEZET,
         SY-UNAME TO ST_ZTCA_IF_LOG-AENAM.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      I_ZTCA_IF_LOG              = ST_ZTCA_IF_LOG
*   IMPORTING
*     E_ZTCA_IF_LOG              =
   EXCEPTIONS
     UPDATE_FAILED              = 1
     NUMBER_RANGE_ERROR         = 2
     TCODE_DOES_NOT_EXIST       = 3
     OTHERS                     = 4.
ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  SORT IT_9000 BY ICON TRAID EMATN MATNR.

  LOOP AT IT_9000.
    CASE IT_9000-ZRESULT.
      WHEN 'S'.
        MOVE: ICON_GREEN_LIGHT TO IT_9000-ICON.
      WHEN 'E'.
        MOVE: ICON_RED_LIGHT TO IT_9000-ICON.
      WHEN SPACE.
        MOVE: ICON_YELLOW_LIGHT TO IT_9000-ICON.
    ENDCASE.

    MODIFY IT_9000.
  ENDLOOP.

  CALL SCREEN 9000.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  get_inbound_delivery_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0263   text
*----------------------------------------------------------------------*
FORM GET_INBOUND_DELIVERY_TYPE USING    BESTTYP.
*--- get inbound delivery type from T163G
  DATA: H_IBTYP LIKE T163D-IBTYP.
* Lieferart für Grob-WE aus Bestätigungssteuerung ermitteln
  H_IBTYP = BESTTYP.
  CALL FUNCTION 'ME_CONFIRMATION_DELIVERY_TYPE'
       EXPORTING
            I_FUNC              = '1'
       CHANGING
            C_IBTYP             = H_IBTYP
            C_EBTYP             = GF_EBTYP                  "386409
            C_LFART             = GF_DLV_TYPE
       EXCEPTIONS
            FUNCTION_NOT_VALID  = 01
            PARAM_VALUE_MISSING = 02
            NO_ITEM_FOUND       = 03.
  IF SY-SUBRC = 0.
    IF GF_DLV_TYPE = SPACE.
      GF_DLV_TYPE = 'EL'.
    ENDIF.
  ELSE.
    GF_DLV_TYPE = 'EL'.
  ENDIF.
ENDFORM.                    " get_inbound_delivery_type
*&---------------------------------------------------------------------*
*&      Form  it_ekpo_short_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IT_EKPO_SHORT_FILL.

  CLEAR : IT_EKPO_SHORT, IT_EKPO_SHORT[].

** Changed by Furong on 03/07/08
  DATA: L_INDEX LIKE SY-TABIX.
*  SELECT
*         ebeln ebelp menge meins matnr werks lgort bstae loekz elikz
*         lmein umrez umren insmk pstyp sobkz knttp kzfme kzvbr"384051
*         ematn mfrnr mfrpn emnfr cuobj uebto untto uebtk bwtar idnlf
*         txz01 mfrgr gewei voleh ntgew brgew volum ean11 aktnr abeln
*         abelp aurel matkl upvor uptyp uebpo wepos          "386409
*          INTO CORRESPONDING FIELDS OF TABLE it_ekpo_short
*          FROM ekpo
*           WHERE ebeln EQ wa_itab-ebeln
**             AND ebelp EQ wa_itab-ebelp
*             AND matnr EQ wa_itab-matnr
**             AND werks IN s_werks
**             and matnr ne space
*"386409
**             AND lgort IN s_lgort
**             AND bstae IN r_bstae
*             AND loekz EQ space
*             AND elikz EQ space
*             AND retpo EQ space                             "327089
*           ORDER BY ebeln ebelp.
*  IF sy-subrc NE 0.
*    CONCATENATE text-008 wa_itab-matnr INTO it_9000-zmsg.
*    MOVE : 'E'           TO it_9000-zresult,
*           c_red         TO it_9000-linecolor.
**---
*    MOVE : sy-datum TO it_9000-zbdat,     " BDC Execute Date
*           sy-uzeit TO it_9000-zbtim,     " BDC Execute Time
*           sy-uname TO it_9000-zbnam,     " User ID
*           'C'      TO it_9000-zmode.     " Data Characteristic Flag
**---
*    MODIFY it_9000 TRANSPORTING zbdat zbtim zbnam zmode
*                                zresult zmsg linecolor
*                          WHERE traid EQ wa_itab-traid
*                            AND ematn EQ wa_itab-ematn
*                            AND matnr EQ wa_itab-matnr
*                            AND ebeln EQ wa_itab-ebeln.
*  ENDIF.

  SELECT
         EBELN EBELP MENGE MEINS MATNR WERKS LGORT BSTAE LOEKZ ELIKZ
         LMEIN UMREZ UMREN INSMK PSTYP SOBKZ KNTTP KZFME KZVBR"384051
         EMATN MFRNR MFRPN EMNFR CUOBJ UEBTO UNTTO UEBTK BWTAR IDNLF
         TXZ01 MFRGR GEWEI VOLEH NTGEW BRGEW VOLUM EAN11 AKTNR ABELN
         ABELP AUREL MATKL UPVOR UPTYP UEBPO WEPOS LOEKZ ELIKZ"386409
          INTO CORRESPONDING FIELDS OF TABLE IT_EKPO_SHORT
          FROM EKPO
           WHERE EBELN EQ WA_ITAB-EBELN
              AND MATNR EQ WA_ITAB-MATNR
*             AND loekz EQ space
*            AND ELIKZ EQ SPACE
             AND RETPO EQ SPACE                             "327089
           ORDER BY EBELN EBELP.
  IF SY-SUBRC NE 0.
    CONCATENATE TEXT-M13 WA_ITAB-MATNR INTO IT_9000-ZMSG.
    MOVE : 'E'           TO IT_9000-ZRESULT,
           C_RED         TO IT_9000-LINECOLOR.
*---
    MOVE : SY-DATUM TO IT_9000-ZBDAT,     " BDC Execute Date
           SY-UZEIT TO IT_9000-ZBTIM,     " BDC Execute Time
           SY-UNAME TO IT_9000-ZBNAM,     " User ID
           'C'      TO IT_9000-ZMODE.     " Data Characteristic Flag
*---
    MODIFY IT_9000 TRANSPORTING ZBDAT ZBTIM ZBNAM ZMODE
                                ZRESULT ZMSG LINECOLOR
                          WHERE TRAID EQ WA_ITAB-TRAID
                            AND EMATN EQ WA_ITAB-EMATN
                            AND MATNR EQ WA_ITAB-MATNR
                            AND EBELN EQ WA_ITAB-EBELN.
  ELSE.

******* added by Vijay UD1K94465 ***********************
    DATA: WA_EKPO LIKE IT_EKPO_SHORT.

    CLEAR: IT_EKPO_SHORT1, IT_EKPO_SHORT1[].

    IT_EKPO_SHORT1[] = IT_EKPO_SHORT[].

*    LOOP AT it_ekpo_short.
*      IF it_ekpo_short-ebeln = wa_ekpo-ebeln AND
*         it_ekpo_short-matnr = wa_ekpo-matnr.
*        IF it_ekpo_short-loekz = 'L'.
*          DELETE it_ekpo_short.
*        ELSE.
*          wa_ekpo-loekz = 'L'.
*          DELETE TABLE it_ekpo_short FROM wa_ekpo.
*        ENDIF.
*      ENDIF.
*      IF it_ekpo_short-loekz = 'L'.
*        wa_ekpo = it_ekpo_short.
*      ENDIF.
*    ENDLOOP.

    DELETE IT_EKPO_SHORT1 WHERE LOEKZ = 'L'.

    SORT IT_EKPO_SHORT1 BY EBELN MATNR.

    LOOP AT IT_EKPO_SHORT.
      IF IT_EKPO_SHORT-LOEKZ = 'L'.
        READ TABLE IT_EKPO_SHORT1 WITH KEY EBELN = IT_EKPO_SHORT-EBELN
                                           MATNR = IT_EKPO_SHORT-MATNR
                                           BINARY SEARCH.

        IF SY-SUBRC = 0.
          DELETE IT_EKPO_SHORT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR WA_EKPO.

    READ TABLE IT_EKPO_SHORT INDEX 1.


*****  changes end by Vijay   *******************

    IF IT_EKPO_SHORT-BSTAE IS INITIAL.     " CONF control
      CONCATENATE TEXT-M20 WA_ITAB-MATNR INTO IT_9000-ZMSG.
      MOVE : 'E'           TO IT_9000-ZRESULT,
             C_RED         TO IT_9000-LINECOLOR.
      MOVE : SY-DATUM TO IT_9000-ZBDAT,     " BDC Execute Date
             SY-UZEIT TO IT_9000-ZBTIM,     " BDC Execute Time
             SY-UNAME TO IT_9000-ZBNAM,     " User ID
             'C'      TO IT_9000-ZMODE.     " Data Characteristic Flag
      MODIFY IT_9000 TRANSPORTING ZBDAT ZBTIM ZBNAM ZMODE
                                ZRESULT ZMSG LINECOLOR
                          WHERE TRAID EQ WA_ITAB-TRAID
                            AND EMATN EQ WA_ITAB-EMATN
                            AND MATNR EQ WA_ITAB-MATNR
                            AND EBELN EQ WA_ITAB-EBELN.
      CLEAR: IT_EKPO_SHORT,IT_EKPO_SHORT[].
    ELSE.
      IF NOT IT_EKPO_SHORT-LOEKZ IS INITIAL.     " item deleted
        CONCATENATE TEXT-M19 WA_ITAB-MATNR INTO IT_9000-ZMSG.
        MOVE : 'E'           TO IT_9000-ZRESULT,
               C_RED         TO IT_9000-LINECOLOR.
        MOVE : SY-DATUM TO IT_9000-ZBDAT,    " BDC Execute Date
               SY-UZEIT TO IT_9000-ZBTIM,    " BDC Execute Time
               SY-UNAME TO IT_9000-ZBNAM,    " User ID
               'C'      TO IT_9000-ZMODE.    " Data Characteristic Flag
        MODIFY IT_9000 TRANSPORTING ZBDAT ZBTIM ZBNAM ZMODE
                                 ZRESULT ZMSG LINECOLOR
                           WHERE TRAID EQ WA_ITAB-TRAID
                             AND EMATN EQ WA_ITAB-EMATN
                             AND MATNR EQ WA_ITAB-MATNR
                             AND EBELN EQ WA_ITAB-EBELN.
        CLEAR: IT_EKPO_SHORT,IT_EKPO_SHORT[].
      ELSE.
        LOOP AT IT_EKPO_SHORT.
          L_INDEX = SY-TABIX.
          IF NOT IT_EKPO_SHORT-ELIKZ IS INITIAL.
            CONCATENATE TEXT-M21 WA_ITAB-MATNR INTO IT_9000-ZMSG.
            MOVE : 'E'           TO IT_9000-ZRESULT,
                  C_RED         TO IT_9000-LINECOLOR.
            MOVE : SY-DATUM TO IT_9000-ZBDAT,  " BDC Execute Date
                   SY-UZEIT TO IT_9000-ZBTIM,  " BDC Execute Time
                   SY-UNAME TO IT_9000-ZBNAM,  " User ID
                   'C'      TO IT_9000-ZMODE.  " Data Char. Flag
            MODIFY IT_9000 TRANSPORTING ZBDAT ZBTIM ZBNAM ZMODE
                                     ZRESULT ZMSG LINECOLOR
                               WHERE TRAID EQ WA_ITAB-TRAID
                                 AND EMATN EQ WA_ITAB-EMATN
                                 AND MATNR EQ WA_ITAB-MATNR
                                 AND EBELN EQ WA_ITAB-EBELN.
            DELETE IT_EKPO_SHORT INDEX L_INDEX.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
** End of change
ENDFORM.                    " it_ekpo_short_fill
*&---------------------------------------------------------------------*
*&      Form  komdlgn_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM KOMDLGN_FILL.

***  Changes added by Vijay

  DELETE IT_EKPO_SHORT WHERE LOEKZ = 'L'.
*---
  LOOP AT IT_EKPO_SHORT.
    STATICS : H_GRKOR LIKE LIPS-GRKOR,      "Liefergruppe
              H_BSMNG LIKE EKPO-MENGE.         "

    PERFORM READ_EKKO USING IT_EKPO_SHORT-EBELN.

*---
    IF NOT EKKO-LIFNR IS INITIAL.
      CALL FUNCTION 'VENDOR_MASTER_DATA_SELECT_12'
           EXPORTING
                PI_LIFNR       = EKKO-LIFNR
                PI_EKORG       = EKKO-EKORG
           IMPORTING
                PE_LFM1        = LFM1
           EXCEPTIONS
                NO_ENTRY_FOUND = 1
                OTHERS         = 2.

      IT_KOMDLGN-VSBED = LFM1-VSBED.     " Shipping conditions
    ELSE.
      CLEAR : IT_KOMDLGN-VSBED.
    ENDIF.

*---
    IT_KOMDLGN-LIFNR = EKKO-LIFNR.
    IT_KOMDLGN-INCO1 = EKKO-INCO1.                          "363954
    IT_KOMDLGN-INCO2 = EKKO-INCO2.                          "363954
    IT_KOMDLGN-EXNUM = EKKO-EXNUM.                          "363954
    IT_KOMDLGN-BUKRS_BEST = EKKO-BUKRS.                     "363954

*---
    IT_KOMDLGN-MATNR   = IT_EKPO_SHORT-MATNR.
    IT_KOMDLGN-WERKS   = IT_EKPO_SHORT-WERKS.
    IT_KOMDLGN-LGORT   = IT_EKPO_SHORT-LGORT.
*    xkomdlgn-charg     = ?
    IT_KOMDLGN-VRKME   = IT_EKPO_SHORT-MEINS.
    IT_KOMDLGN-MEINS   = IT_EKPO_SHORT-LMEIN.
    IT_KOMDLGN-UMVKZ   = IT_EKPO_SHORT-UMREZ.
    IT_KOMDLGN-UMVKN   = IT_EKPO_SHORT-UMREN.

*---
    IF IT_EKPO_SHORT-MATNR EQ SPACE.                        "386409
      IT_KOMDLGN-MEINS = IT_EKPO_SHORT-MEINS.
      IT_KOMDLGN-UMVKZ = 1.
      IT_KOMDLGN-UMVKN = 1.
    ENDIF.

*---
    IT_KOMDLGN-INSMK = IT_EKPO_SHORT-INSMK.
    IT_KOMDLGN-KZFME = IT_EKPO_SHORT-KZFME.
    IT_KOMDLGN-KZVBR = IT_EKPO_SHORT-KZVBR.           "note 384051

*--- get open qty
    CLEAR : IT_XEKET, IT_XEKET[], W_QTY_SUM.

    PERFORM GET_OPEN_QTY USING IT_EKPO_SHORT-EBELN
                               IT_EKPO_SHORT-EBELP
*--- 2004/02/17 block by stlim
*                               wa_itab-zcreate.
*--- 2004/02/17
*--- 2004/02/17 add by stlim
*                               wa_itab-lfdat_la.
                               WA_ITAB-ETA.
*--- 2004/02/17

*---
*    it_komdlgn-lfimg = wa_itab-lfimg.     "p_open_qty.

**--- block & insert by stlim (2004/05/13)
    IF WA_ITAB-LFIMG EQ W_QTY_SUM.     " I/D qty = open qty
      MOVE : W_QTY_SUM TO IT_KOMDLGN-LFIMG.
    ELSEIF WA_ITAB-LFIMG GT W_QTY_SUM.     " I/D qty > open qty
*      MOVE : w_qty_sum TO it_komdlgn-lfimg.
*
      MOVE : TEXT-M03      TO IT_9000-ZMSG,
             'E'           TO IT_9000-ZRESULT,
             C_RED         TO IT_9000-LINECOLOR.
*---
      MOVE : SY-DATUM TO IT_9000-ZBDAT,     " BDC Execute Date
             SY-UZEIT TO IT_9000-ZBTIM,     " BDC Execute Time
             SY-UNAME TO IT_9000-ZBNAM,     " User ID
             'C'      TO IT_9000-ZMODE.     " Data Characteristic Flag

      MODIFY IT_9000 TRANSPORTING ZBDAT ZBTIM ZBNAM ZMODE
                                  ZRESULT ZMSG LINECOLOR
                            WHERE TRAID EQ WA_ITAB-TRAID
                              AND EMATN EQ WA_ITAB-EMATN
                              AND MATNR EQ WA_ITAB-MATNR
                              AND EBELN EQ WA_ITAB-EBELN.
      EXIT.
    ELSEIF WA_ITAB-LFIMG LT W_QTY_SUM.     " I/D qty < open qty
      MOVE : WA_ITAB-LFIMG TO IT_KOMDLGN-LFIMG.
    ENDIF.
*    it_komdlgn-lfimg = w_qty_sum.     " open qty
**--- end of block & insert

*--- 2004/02/17 block by stlim
*    it_komdlgn-lfdat = wa_itab-zcreate.     "p_eindt.
*--- 2004/02/17
*--- 2004/02/17 add by stlim
*    it_komdlgn-lfdat = wa_itab-lfdat_la.

**--- blocked by stlim (2004/05/11)
    IT_KOMDLGN-LFDAT = WA_ITAB-ETA.
    IF IT_KOMDLGN-LFDAT IS INITIAL.
      CLEAR : EKET.
      SELECT SINGLE EINDT INTO IT_KOMDLGN-LFDAT
                          FROM EKET
                         WHERE EBELN EQ IT_EKPO_SHORT-EBELN
                           AND EBELP EQ IT_EKPO_SHORT-EBELP.
    ENDIF.
**--- end of block

**--- insert by stlim (2004/05/11)
*    CLEAR : eket.
*    SELECT SINGLE eindt INTO it_komdlgn-lfdat
*                        FROM eket
*                       WHERE ebeln EQ it_ekpo_short-ebeln
*                         AND ebelp EQ it_ekpo_short-ebelp.
**--- end of insert

*---
*    it_komdlgn-lfuhr = p_uzeit.
*    xkomdlgn-vstel = ?
*    xkomdlgn-vkorg = ?
*    xkomdlgn-vtweg = ?
*    xkomdlgn-spart = ?
    IT_KOMDLGN-VGBEL = IT_EKPO_SHORT-EBELN.
    IT_KOMDLGN-VGPOS = IT_EKPO_SHORT-EBELP.
    IT_KOMDLGN-LFART = GF_DLV_TYPE.
    IT_KOMDLGN-VGTYP = 'V'.
    IT_KOMDLGN-KZAZU = 'X'.                "??? what's that for ????
    IT_KOMDLGN-KNTTP = IT_EKPO_SHORT-KNTTP.
    IT_KOMDLGN-SOBKZ = IT_EKPO_SHORT-SOBKZ.

*--- note 386409:
    SELECT * FROM T163G WHERE BSTAE EQ IT_EKPO_SHORT-BSTAE
                          AND EBTYP EQ GF_EBTYP.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC = 0.
* Prüfen, ob Lieferavis WE-Zuordnung hat (Vorauss. für WE über VL32)
* und wepos prüfen
      IF T163G-WEZUO EQ SPACE OR IT_EKPO_SHORT-WEPOS EQ SPACE.
        IT_KOMDLGN-NOWAB = 'X'.
      ELSE.
        CLEAR IT_KOMDLGN-NOWAB.
      ENDIF.
    ENDIF.

*---
    IF IT_EKPO_SHORT-MATNR IS INITIAL OR IT_EKPO_SHORT-PSTYP = '6'.
      IT_KOMDLGN-POSAR = 'B'.
    ENDIF.

*---
    IT_KOMDLGN-EMATN = IT_EKPO_SHORT-EMATN.
    IT_KOMDLGN-MFRNR = IT_EKPO_SHORT-MFRNR.
    IT_KOMDLGN-MFRPN = IT_EKPO_SHORT-MFRPN.
    IT_KOMDLGN-EMNFR = IT_EKPO_SHORT-EMNFR.
    IT_KOMDLGN-CUOBJ = IT_EKPO_SHORT-CUOBJ.
    IT_KOMDLGN-UEBTO = IT_EKPO_SHORT-UEBTO.
    IT_KOMDLGN-UNTTO = IT_EKPO_SHORT-UNTTO.
    IT_KOMDLGN-UEBTK = IT_EKPO_SHORT-UEBTK.
*    it_komdlgn-lichn = p_licha.     " vendor batch number
*    it_komdlgn-charg = p_charg.
    IT_KOMDLGN-BWTAR = IT_EKPO_SHORT-BWTAR.

*---
*    it_komdlgn-kdmat = it_ekpo_short-idnlf.
*    MOVE : wa_itab-ematn TO it_komdlgn-kdmat.
*    CONCATENATE wa_itab-ematn wa_itab-zzkdwebpo INTO
*                              it_komdlgn-kdmat.
*    CONCATENATE wa_itab-ebeln wa_itab-ematn INTO
*                              it_komdlgn-kdmat.
    CONCATENATE WA_ITAB-ZZKDWEBPO WA_ITAB-EMATN INTO
                              IT_KOMDLGN-KDMAT.
*---

    IT_KOMDLGN-ARKTX = IT_EKPO_SHORT-TXZ01.
    IT_KOMDLGN-MFRGR = IT_EKPO_SHORT-MFRGR.
    IT_KOMDLGN-GEWEI = IT_EKPO_SHORT-GEWEI.
    IT_KOMDLGN-VOLEH = IT_EKPO_SHORT-VOLEH.
    IT_KOMDLGN-NTGEW = IT_EKPO_SHORT-NTGEW * IT_KOMDLGN-LFIMG.
    IT_KOMDLGN-BRGEW = IT_EKPO_SHORT-BRGEW * IT_KOMDLGN-LFIMG.
    IT_KOMDLGN-VOLUM = IT_EKPO_SHORT-VOLUM * IT_KOMDLGN-LFIMG.
    IT_KOMDLGN-EAN11 = IT_EKPO_SHORT-EAN11.
*    it_komdlgn-podrel = t163l-podrel.
    IT_KOMDLGN-AKTNR = IT_EKPO_SHORT-AKTNR.
    IT_KOMDLGN-ABELN = IT_EKPO_SHORT-ABELN.
    IT_KOMDLGN-ABELP = IT_EKPO_SHORT-ABELP.
* xkomdlgn-ltssf = only sort criteria in vl31n
    IT_KOMDLGN-AUREL = IT_EKPO_SHORT-AUREL.

*---
*    it_komdlgn-idnlf = it_ekpo_short-idnlf.
    MOVE : WA_ITAB-EMATN TO IT_KOMDLGN-IDNLF.
*---

    IT_KOMDLGN-MATKL = IT_EKPO_SHORT-MATKL.

*---
    CLEAR IT_KOMDLGN-GRKOR.
    CLEAR IT_KOMDLGN-KMPMG.
    CLEAR IT_KOMDLGN-UEPOS.
    CLEAR IT_KOMDLGN-UEPVW.                                 "549736

*---
    IF IT_EKPO_SHORT-UPVOR CA '3X'.
      H_GRKOR = H_GRKOR + 1.
      IT_KOMDLGN-GRKOR = H_GRKOR.
      H_BSMNG = IT_EKPO_SHORT-MENGE.
    ENDIF.

    IF NOT IT_EKPO_SHORT-UEBPO IS INITIAL AND
           IT_EKPO_SHORT-UPTYP CA '3X'.
      IT_KOMDLGN-UEPVW = 'G'.                               "549736
      IT_KOMDLGN-UEPOS = IT_EKPO_SHORT-UEBPO.
      IT_KOMDLGN-GRKOR = H_GRKOR.
      IF H_BSMNG NE 0.
        IT_KOMDLGN-KMPMG = IT_EKPO_SHORT-MENGE / H_BSMNG.
      ENDIF.
    ENDIF.

*---
    IF IT_EKPO_SHORT-PSTYP EQ '2'.
      IT_KOMDLGN-SOBKZ = 'K'.
    ENDIF.
* Kontierungsfelder
    IF IT_EKPO_SHORT-SOBKZ EQ 'E' OR IT_EKPO_SHORT-SOBKZ EQ 'Q'.
      CALL FUNCTION 'MMPUR_EKKN_READ_EBELN_EBELP'
           EXPORTING
                PI_EBELN             = IT_EKPO_SHORT-EBELN
                PI_EBELP             = IT_EKPO_SHORT-EBELP
           TABLES
                PTO_EKKN_PO          = IT_EKKN
           EXCEPTIONS
                NO_RECORDS_REQUESTED = 1
                OTHERS               = 2.

      IF SY-SUBRC EQ 0.
        READ TABLE IT_EKKN INDEX 1.
        IT_KOMDLGN-PS_PSP_PNR = IT_EKKN-PS_PSP_PNR.
        IT_KOMDLGN-VBELV      = IT_EKKN-VBELN.
        IT_KOMDLGN-POSNV      = IT_EKKN-VBELP.
      ENDIF.
    ENDIF.

*--- others
    MOVE : '0005'        TO IT_KOMDLGN-TRATY,
           WA_ITAB-TRAID TO IT_KOMDLGN-TRAID,
           WA_ITAB-LIFEXPOS TO IT_KOMDLGN-LIFEXPOS.

    MOVE : WA_ITAB-ZINVOICE TO IT_KOMDLGN-BOLNR.

*--- 2004/02/17 add by stlim
*--- add seal number
    MOVE : WA_ITAB-BORGR_GRP TO IT_KOMDLGN-BORGR_GRP.
*---

* Begin of changes -  UD1K930445
* Populate BOL# in Delivery Header
    READ TABLE IT_BOL_INF WITH KEY TRAID = IT_KOMDLGN-TRAID
                                   CINVO = IT_KOMDLGN-BOLNR
                                   BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_KOMDLGN-LIFEX = IT_BOL_INF-ZFHBLNO.
    ENDIF.

* End of changes -  UD1K930445

    APPEND IT_KOMDLGN.
    CLEAR : IT_KOMDLGN.
  ENDLOOP.
ENDFORM.                    " komdlgn_fill
*&---------------------------------------------------------------------*
*&      Form  read_ekko
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EKPO_SHORT_EBELN  text
*----------------------------------------------------------------------*
FORM READ_EKKO USING    P_IT_9000_EBELN.
*---
  CALL FUNCTION 'ME_EKKO_SINGLE_READ'
       EXPORTING
            PI_EBELN         = P_IT_9000_EBELN
       IMPORTING
            PO_EKKO          = EKKO
       EXCEPTIONS
            NO_RECORDS_FOUND = 1
            OTHERS           = 2.
ENDFORM.                    " read_ekko
*&---------------------------------------------------------------------*
*&      Form  get_open_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EKPO_SHORT_EBELN  text
*      -->P_IT_EKPO_SHORT_EBELP  text
*      -->P_WA_ITAB_ETA  text
*----------------------------------------------------------------------*
FORM GET_OPEN_QTY USING    P_IT_EKPO_SHORT_EBELN
                           P_IT_EKPO_SHORT_EBELP
                           P_WA_ITAB-ZREATE.
*---
  DATA : H-MENG1 LIKE EKES-DABMG,
         H-MENGE LIKE EKES-DABMG.

*---
  CLEAR : T163D.
  SELECT SINGLE * FROM T163D
                 WHERE IBTYP EQ C_IBTYP.

*---
  CLEAR : H-MENGE.
  SELECT * FROM EKES WHERE EBELN EQ P_IT_EKPO_SHORT_EBELN
                       AND EBELP EQ P_IT_EKPO_SHORT_EBELP
                       AND EBTYP EQ T163D-EBTYP.
    IF EKES-ESTKZ EQ '4'.
      H-MENGE = H-MENGE - EKES-MENGE.
    ELSE.
      H-MENGE = H-MENGE + EKES-MENGE.
    ENDIF.
  ENDSELECT.

*---
  CLEAR : H-MENG1.
  IF NOT P_WA_ITAB-ZREATE IS INITIAL.
    SELECT * FROM EKET WHERE EBELN EQ P_IT_EKPO_SHORT_EBELN
                         AND EBELP EQ P_IT_EKPO_SHORT_EBELP
                         AND EINDT LE P_WA_ITAB-ZREATE.
      H-MENG1 = H-MENG1 + EKET-MENGE.
    ENDSELECT.
*    IF sy-subrc NE 0.
*      CLEAR eket.
*      SELECT SINGLE * FROM eket WHERE ebeln EQ p_it_ekpo_short_ebeln
*                                AND   ebelp EQ p_it_ekpo_short_ebelp.
*    ENDIF.
  ELSE.
*    CLEAR eket.
*    SELECT SINGLE * FROM eket WHERE ebeln EQ ekpo-ebeln
*                                AND   ebelp EQ ekpo-ebelp.
  ENDIF.

*---
  IF H-MENG1 GT H-MENGE.
    W_QTY_SUM = H-MENG1 - H-MENGE.
  ELSE.
    W_QTY_SUM = IT_EKPO_SHORT-MENGE - H-MENGE.
  ENDIF.
  IF W_QTY_SUM LT 0.
    W_QTY_SUM = 0.
  ENDIF.

**---
*  CALL FUNCTION 'ME_CONFIRMATION_ANL_QTY'
*       EXPORTING
*            i_ebeln = p_it_ekpo_short_ebeln
*            i_ebelp = p_it_ekpo_short_ebelp
*            i_eindt = p_wa_itab-zreate
*       TABLES
*            xeket   = it_xeket.
*
**---
*  LOOP AT it_xeket.
*    w_qty_sum = w_qty_sum + it_xeket-menge.
*  ENDLOOP.
ENDFORM.                    " get_open_qty
*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_FUNCTION.
*---
  CLEAR : IT_VBFS, IT_VBFS[], IT_VBLS, IT_VBLS[], IT_LIPS, IT_LIPS[].

  CALL FUNCTION 'GN_DELIVERY_CREATE'
       EXPORTING
            VBSK_I   = ST_VBSK
       TABLES
            XKOMDLGN = IT_KOMDLGN
            XVBFS    = IT_VBFS
            XVBLS    = IT_VBLS
            XXLIPS   = IT_LIPS.
ENDFORM.                    " call_function
*&---------------------------------------------------------------------*
*&      Form  modify_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_ITAB.
  DATA: W_ERR_MSG LIKE BAPIRET2-MESSAGE,
        WA_RETURN LIKE BAPIRET2,
        W_MSGNO LIKE BAPIRET2-NUMBER.

  LOOP AT IT_KOMDLGN.
    READ TABLE IT_LIPS WITH KEY VGBEL = IT_KOMDLGN-VGBEL
                                VGPOS = IT_KOMDLGN-VGPOS
                                MATNR = IT_KOMDLGN-MATNR
                                LFIMG = IT_KOMDLGN-LFIMG.
    IF SY-SUBRC EQ 0.
      MOVE : IT_LIPS-VBELN TO IT_9000-ZMSG,
             'S'           TO IT_9000-ZRESULT,
             C_GREEN       TO IT_9000-LINECOLOR.

      MOVE : SY-DATUM TO IT_9000-ZBDAT,     " BDC Execute Date
             SY-UZEIT TO IT_9000-ZBTIM,     " BDC Execute Time
             SY-UNAME TO IT_9000-ZBNAM,     " User ID
             'C'      TO IT_9000-ZMODE.     " Data Characteristic Flag

*---
      MODIFY IT_9000 TRANSPORTING ZBDAT   ZBTIM ZBNAM ZMODE
                                  ZRESULT ZMSG  LINECOLOR
                            WHERE TRAID EQ WA_ITAB-TRAID
                              AND EMATN EQ WA_ITAB-EMATN
                              AND EBELN EQ IT_KOMDLGN-VGBEL
                              AND MATNR EQ IT_KOMDLGN-MATNR.

**--- insert by stlim (2004/05/11)
      UPDATE LIKP SET ZZDEPDT = WA_ITAB-LFDAT_LA
                      ZZARRDT = WA_ITAB-ETA
                WHERE VBELN EQ IT_LIPS-VBELN.
      COMMIT WORK.
    ELSE.
      READ TABLE IT_VBFS WITH KEY VBELN = IT_KOMDLGN-VGBEL
                                  POSNR = IT_KOMDLGN-VGPOS
                                  MSGV1 = IT_KOMDLGN-MATNR.
      IF SY-SUBRC EQ 0.
        W_MSGNO = IT_VBFS-MSGNO.
        CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
          EXPORTING
            ID                = IT_VBFS-MSGID
            NUMBER            = W_MSGNO
*       LANGUAGE          = SY-LANGU
            TEXTFORMAT        = 'NON'
*       LINKPATTERN       =
         MESSAGE_V1        = IT_VBFS-MSGV1
         MESSAGE_V2        = IT_VBFS-MSGV2
         MESSAGE_V3        = IT_VBFS-MSGV3
         MESSAGE_V4        = IT_VBFS-MSGV4
         IMPORTING
           MESSAGE           = W_ERR_MSG
           RETURN            = WA_RETURN .
*     TABLES
*       TEXT              =
        CLEAR: W_MSGNO.
        MOVE : W_ERR_MSG     TO IT_9000-ZMSG,
               'E'           TO IT_9000-ZRESULT,
               C_RED         TO IT_9000-LINECOLOR.
*---
        MOVE : SY-DATUM TO IT_9000-ZBDAT,     " BDC Execute Date
               SY-UZEIT TO IT_9000-ZBTIM,     " BDC Execute Time
               SY-UNAME TO IT_9000-ZBNAM,     " User ID
               'C'      TO IT_9000-ZMODE.     " Data Characteristic Flag
      ELSE.
        CLEAR: W_MSGNO.
        MOVE : TEXT-M02      TO IT_9000-ZMSG,
               'E'           TO IT_9000-ZRESULT,
               C_RED         TO IT_9000-LINECOLOR.
*---
        MOVE : SY-DATUM TO IT_9000-ZBDAT,     " BDC Execute Date
               SY-UZEIT TO IT_9000-ZBTIM,     " BDC Execute Time
               SY-UNAME TO IT_9000-ZBNAM,     " User ID
               'C'      TO IT_9000-ZMODE.     " Data Characteristic Flag
      ENDIF.

      MODIFY IT_9000 TRANSPORTING ZBDAT   ZBTIM ZBNAM ZMODE
                                  ZRESULT ZMSG  LINECOLOR
                            WHERE TRAID EQ WA_ITAB-TRAID
                              AND EMATN EQ WA_ITAB-EMATN
                              AND EBELN EQ IT_KOMDLGN-VGBEL
                              AND MATNR EQ IT_KOMDLGN-MATNR.
    ENDIF.
  ENDLOOP.



*
**---
*  READ TABLE it_lips INDEX 1.
*
**---
*  IF sy-subrc EQ 0.
*    MOVE : it_lips-vbeln TO IT_9000-zmsg,
*           'S'           TO IT_9000-zresult,
*           c_green       TO IT_9000-linecolor.
*
*    MOVE : sy-datum TO IT_9000-zbdat,     " BDC Execute Date
*           sy-uzeit TO IT_9000-zbtim,     " BDC Execute Time
*           sy-uname TO IT_9000-zbnam,     " User ID
*           'C'      TO IT_9000-zmode.     " Data Characteristic Flag
*
**---
*    MODIFY IT_9000 TRANSPORTING zbdat
*                                zbtim
*                                zbnam
*                                zmode
*                                zresult
*                                zmsg
*                                linecolor
*                                          WHERE traid EQ wa_itab-traid
*                                            AND ematn EQ wa_itab-ematn.
*
***--- insert by stlim (2004/05/11)
*    UPDATE likp SET zzdepdt = wa_itab-lfdat_la
*                    zzarrdt = wa_itab-eta
*              WHERE vbeln EQ it_lips-vbeln.
*    COMMIT WORK.
***--- end of insert
*  ELSE.
*    LOOP AT it_vbfs WHERE msgty = 'E'.
*      w_msgno = it_vbfs-msgno.
*      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
*        EXPORTING
*          id                = it_vbfs-msgid
*          number            = w_msgno
**       LANGUAGE          = SY-LANGU
*          textformat        = 'NON'
**       LINKPATTERN       =
*       message_v1        = it_vbfs-msgv1
*       message_v2        = it_vbfs-msgv2
*       message_v3        = it_vbfs-msgv3
*       message_v4        = it_vbfs-msgv4
*       IMPORTING
*         message           = w_err_msg
*         return            = wa_return .
**     TABLES
**       TEXT              =
*      CLEAR: w_msgno.
*      MOVE : w_err_msg     TO IT_9000-zmsg,
*             'E'           TO IT_9000-zresult,
*             c_red         TO IT_9000-linecolor.
**---
*      MOVE : sy-datum TO IT_9000-zbdat,     " BDC Execute Date
*             sy-uzeit TO IT_9000-zbtim,     " BDC Execute Time
*             sy-uname TO IT_9000-zbnam,     " User ID
*             'C'      TO IT_9000-zmode.     " Data Characteristic Flag
*
**---
*      MODIFY IT_9000 TRANSPORTING zbdat
*                                  zbtim
*                                  zbnam
*                                  zmode
*                                  zresult
*                                  zmsg
*                                  linecolor
*                                           WHERE traid EQ wa_itab-traid
*                                             AND ematn EQ wa_itab-ematn
  .
*    ENDLOOP.
*  ENDIF.
ENDFORM.                    " modify_itab
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  CASE SY-DYNNR.
    WHEN 9000.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
    WHEN 9100.
      SET PF-STATUS '9100'.
      SET TITLEBAR  '9100'.
  ENDCASE.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: SY-UCOMM.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR SY-UCOMM.
      LEAVE TO SCREEN 0.
    WHEN 'EXEC'.
      CLEAR: SY-UCOMM.
      PERFORM EXECUTE_RTN.
    WHEN 'CHGN'.
      CLEAR: SY-UCOMM.
      PERFORM CHANGE_PO.
    WHEN 'DISP'.
      CLEAR: SY-UCOMM.
      PERFORM DISPLAY_PO.

    WHEN 'REFR'.
      CLEAR: SY-UCOMM.
      PERFORM REFRESH_PO.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_ALV_OBJECT OUTPUT.
  PERFORM CREATE_ALV_OBJECT USING SY-DYNNR.
ENDMODULE.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_alv_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM CREATE_ALV_OBJECT USING P_DYNNR.
  CONCATENATE: 'WC_CONTAINER_' P_DYNNR INTO W_CONTAINER.
  ASSIGN:      (W_CONTAINER)           TO   <CONTAINER>.

  IF <CONTAINER> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT USING P_DYNNR.
    PERFORM SET_ATTRIBUTES_ALV_GRID USING P_DYNNR.
    PERFORM BUILD_FIELD_CATALOG USING P_DYNNR.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM ASSIGN_ITAB_TO_ALV USING P_DYNNR.
    PERFORM SSSIGN_EVENT.
  ELSE.
    PERFORM SET_ATTRIBUTES_ALV_GRID USING P_DYNNR.
    PERFORM BUILD_FIELD_CATALOG USING P_DYNNR.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM ASSIGN_ITAB_TO_ALV USING P_DYNNR.
  ENDIF.
ENDFORM.                    " create_alv_object
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT USING P_DYNNR.
*- Create Container('GRID_CONTAINER') with Custom Control on screen

  CONCATENATE: 'WC_CONTAINER_' P_DYNNR INTO W_CONTAINER,
               'WC_CONTROL_'   P_DYNNR INTO W_CONTROL,
               'WC_ALV_'       P_DYNNR INTO W_ALV.

  ASSIGN: (W_CONTAINER) TO <CONTAINER>,
          (W_CONTROL)   TO <CONTROL>,
          (W_ALV)       TO <ALV>.

  CREATE OBJECT <CONTAINER>
         EXPORTING CONTAINER_NAME = <CONTROL>
         EXCEPTIONS
          CNTL_ERROR = 1
          CNTL_SYSTEM_ERROR = 2
          CREATE_ERROR = 3
          LIFETIME_ERROR = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT <ALV>
         EXPORTING I_PARENT      = <CONTAINER>
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID USING P_DYNNR.
  CASE P_DYNNR.
    WHEN '9000'.
      PERFORM SET_ATTRIBUTES_ALV_9000.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_9000.
  CLEAR : W_IS_LAYOUT, W_VARIANT.

  W_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  W_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  W_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  W_IS_LAYOUT-CWIDTH_OPT = C_CHECK.  "/optimizes the column width
  W_IS_LAYOUT-NO_MERGING = C_CHECK.  "/Disable cell merging
  W_VARIANT-REPORT       = SY-REPID.
  W_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_9100
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_DYNNR.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM SET_FIELDNAME USING P_DYNNR.
  PERFORM SET_SCREEN_FIELDS USING P_DYNNR.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  set_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM SET_FIELDNAME USING P_DYNNR.
  DATA: LW_ITAB TYPE SLIS_TABNAME.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].

  MOVE: SY-REPID TO W_REPID.
  CONCATENATE C_STRUCTURE P_DYNNR '_NEW' INTO LW_ITAB.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.
ENDFORM.                    " set_fieldname
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS USING P_DYNNR.
  CASE P_DYNNR.
    WHEN '9000'.
      PERFORM SET_SCREEN_FIELDS_9000.
  ENDCASE.
ENDFORM.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS_9000.
  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                  'S' 'TRAID'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'EMATN'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ETA'         ' ',
                                  ' ' 'COLTEXT'     'ETA',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'EBELN'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'ZZKDWEBPO'   ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'LIFEXPOS'    ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'MEINS'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'BORGR_GRP'   ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'EXPVZ'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'ZRESULT'     ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'ZMSG'        ' ',
                                  'E' 'EMPHASIZE'   'C500'.
ENDFORM.                    " set_screen_fields_9000
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV USING P_DYNNR.
  DATA: LW_DYNNR   LIKE   SY-DYNNR.

  CONCATENATE: 'WC_ALV_'    P_DYNNR      INTO W_ALV,
               C_STRUCTURE  P_DYNNR  '_NEW' INTO W_STRUCTURE,
               'IT_'        P_DYNNR '[]' INTO W_ITAB.

  ASSIGN: (W_ALV)       TO <ALV>,
          (W_ITAB)      TO <ITAB>.

  CALL METHOD <ALV>->SET_TABLE_FOR_FIRST_DISPLAY
     EXPORTING I_STRUCTURE_NAME = W_STRUCTURE
               IS_LAYOUT        = W_IS_LAYOUT
               I_SAVE           = W_SAVE
               IS_VARIANT       = W_VARIANT
               I_DEFAULT        = SPACE
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = <ITAB>.
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SSSIGN_EVENT.

ENDFORM.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0836   text
*      -->P_0837   text
*      -->P_0838   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    IF P_FIELDCAT-COL_POS IS INITIAL.
      ADD 1 TO W_CNT.
      P_FIELDCAT-COL_POS = W_CNT.
    ENDIF.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  execute_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXECUTE_RTN.
  "/Indexes of Selected Rows
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows

  CALL METHOD WC_ALV_9000->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.

  PERFORM SET_TARGET_DATA TABLES LT_ROWS.

  PERFORM CREATE_DELIVERY TABLES IT_9000_TAR.
  PERFORM UPDATE_TABLE TABLES IT_9000_TAR.
  PERFORM COUNT_RTN.
  PERFORM CREATE_BOL_FOREGROUD  TABLES IT_9000_TAR.
ENDFORM.                    " execute_rtn
*&---------------------------------------------------------------------*
*&      Form  set_target_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*----------------------------------------------------------------------*
FORM SET_TARGET_DATA TABLES PT_ROWS STRUCTURE LVC_S_ROW.
  CLEAR: IT_9000_TAR, IT_9000_TAR[].

* Begin of changes  - UD1K930445
*&--------------------------------------------------------------------&*
*& Since we have different length defined in the tables:
*&                   1. ZTMM_KD_ASN_MAIN.
*&                   2. ZTBLIT_INF.
*&have to create an intermediate table for efficent "SELECT" from table.
*& BOL won't be success until ASN is created.
*&--------------------------------------------------------------------&*
  LOOP AT IT_9000 ASSIGNING <FS_KDASN>.
    WA_ASN-TRAID = <FS_KDASN>-TRAID.
    WA_ASN-CINVO = <FS_KDASN>-ZINVOICE.
    COLLECT WA_ASN INTO IT_ASN.
  ENDLOOP.

  SELECT ZFCIVNO ZFCONT
         ZFHBLNO                                            "UD1K919888
    FROM ZTBLIT_INF  INTO TABLE IT_BOL_INF
              FOR ALL ENTRIES IN IT_ASN
           WHERE ZFCIVNO = IT_ASN-CINVO
           AND   ZFCONT  = IT_ASN-TRAID.
*                        and   zresult = 'S'.
*  if sy-subrc ne '0'.
*    MESSAGE e000(zz) WITH text-011.
*  endif.
  SORT IT_BOL_INF BY TRAID CINVO.

  LOOP AT IT_9000 ASSIGNING <FS_KDASN>.
    READ TABLE IT_BOL_INF WITH KEY TRAID = <FS_KDASN>-TRAID
                                CINVO = <FS_KDASN>-ZINVOICE
                                              BINARY SEARCH
                                     TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      WA_9000 = <FS_KDASN>.
      WA_9000-ZRESULT = 'E'.
      WA_9000-ZMSG    = TEXT-011.                           "TEXT-010
      APPEND WA_9000 TO IT_ERR_9000.
      <FS_KDASN>-ZRESULT = 'E'.
      <FS_KDASN>-ZMSG = TEXT-011.
    ELSE.
      IF R2 EQ 'X' .  "Reprocess
        <FS_KDASN>-ZRESULT = ''.
        <FS_KDASN>-ZMSG = ''.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  delete it_9000 where zresult = 'D'.

* End of changes - UD1K930445
  IF  P_UPDATE EQ ''.
    LOOP AT PT_ROWS WHERE INDEX NE 0.
      READ TABLE IT_9000 INDEX PT_ROWS-INDEX.
      IF SY-SUBRC NE 0.
        MESSAGE E000(ZZ) WITH TEXT-M04.
      ENDIF.

      CHECK IT_9000-ICON EQ ICON_RED_LIGHT OR
            IT_9000-ICON EQ ICON_YELLOW_LIGHT.

* If No BOL DATA Found - Don't create Delivery
      CHECK IT_9000-ZRESULT NE 'E'.
      MOVE IT_9000 TO IT_9000_TAR.

      APPEND IT_9000_TAR.

    ENDLOOP.

  ELSE.

    LOOP AT  IT_9000 .

*      CHECK it_9000-icon EQ icon_red_light OR
*            it_9000-icon EQ icon_yellow_light.

* If No BOL DATA Found - Don't create Delivery
      CHECK IT_9000-ZRESULT NE 'E'.
      MOVE IT_9000 TO IT_9000_TAR.

      APPEND IT_9000_TAR.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " set_target_data
*&---------------------------------------------------------------------*
*&      Form  count_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COUNT_RTN.
  CLEAR: W_TOTAL, W_SUCCESS, W_ERROR, W_READY.
  LOOP AT IT_9000.
    CASE IT_9000-ZRESULT.
      WHEN 'S'.
        W_SUCCESS = W_SUCCESS + 1.
      WHEN 'E'.
        W_ERROR = W_ERROR + 1.
      WHEN SPACE.
        W_READY = W_READY + 1.
    ENDCASE.

    W_TOTAL = W_TOTAL + 1.
  ENDLOOP.
ENDFORM.                    " count_rtn
*&---------------------------------------------------------------------*
*&      Form  change_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_PO.
  DATA: IT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
          IT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows

  CALL METHOD WC_ALV_9000->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = IT_ROWS[]
                     ET_ROW_NO     = IT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

*  CLEAR: w_select, w_success, w_fail.

  READ TABLE IT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  CLEAR: IT_9000_CHPO, IT_9000_CHPO[].
  CLEAR: IT_9100, IT_9100[].

  LOOP AT IT_ROWS WHERE INDEX NE 0.
    READ TABLE IT_9000 INDEX IT_ROWS-INDEX.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M04.
    ENDIF.
    MOVE IT_9000 TO IT_9000_CHPO.
    APPEND IT_9000_CHPO.
  ENDLOOP.
  LOOP AT IT_9000_CHPO.
    MOVE-CORRESPONDING IT_9000_CHPO TO IT_9100.
    APPEND IT_9100.
  ENDLOOP.
  CALL SCREEN 9100.
ENDFORM.                    " change_po

*&spwizard: declaration of tablecontrol 'TC_9100' itself
CONTROLS: TC_9100 TYPE TABLEVIEW USING SCREEN 9100.

*&spwizard: output module for tc 'TC_9100'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE TC_9100_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_9100 LINES TC_9100-LINES.
ENDMODULE.

*&spwizard: input module for tc 'TC_9100'. do not change this line!
*&spwizard: modify table
MODULE TC_9100_MODIFY INPUT.
  MODIFY IT_9100
    INDEX TC_9100-CURRENT_LINE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.

      CLEAR SY-UCOMM.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      CLEAR SY-UCOMM.
      PERFORM SAVE_CHANGED_PO.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_CHANGED_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_CHANGED_PO.
  DELETE ZTMM_KD_ASN_MAIN FROM TABLE IT_9000_CHPO.
  MODIFY ZTMM_KD_ASN_MAIN FROM TABLE IT_9100.
  MESSAGE S999 WITH TEXT-S01.
ENDFORM.                    " SAVE_CHANGED_PO
*&---------------------------------------------------------------------*
*&      Form  refresh_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_PO.
  PERFORM GET_DATA.
  PERFORM DISPLAY_DATA.
ENDFORM.                    " refresh_po
*&---------------------------------------------------------------------*
*&      Module  TC_9100_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_9100_CHANGE_FIELD_ATTR OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'IT_9100-EBELN'.
      SCREEN-INPUT = 1.
    ELSE.
      SCREEN-INPUT = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " TC_9100_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_BOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_BOL.

  DATA: BEGIN OF WA_ZTBLHD_LOG.
          INCLUDE STRUCTURE ZTBLHD_INF.
  DATA:    END OF WA_ZTBLHD_LOG.

  DATA: IT_ZTBLHD_LOG LIKE TABLE OF WA_ZTBLHD_LOG WITH HEADER LINE.

  DATA: BEGIN OF IT_ZSBLHD OCCURS 1000.
          INCLUDE STRUCTURE ZSBLHD_INF.
  DATA  END OF IT_ZSBLHD.

  DATA: BEGIN OF IT_ZSBLIT OCCURS 1000.
          INCLUDE STRUCTURE ZSBLIT_INF.
  DATA  END OF IT_ZSBLIT.


  SELECT DISTINCT A~ZFBLNO B~ZFCIVNO A~ZFHBLNO
            A~ZFMBLNO A~ZFBLDT A~ZFSHTY
            A~ZFVIA A~ZFVSL A~ZF20FT A~ZF40FT A~ZF45FT A~ZF40HQ
            A~ZFNEWT A~ZFNEWTM A~ZFTOVL A~ZFTOVLM A~ZFETD A~ZFETA
            A~ZFSPRT A~ZFAPRT A~ZFAPRTC A~ZFSPRTC A~ZUSER A~ZSDAT
            A~ZSTIM A~ZEDAT A~ZETIM A~ZMODE A~ZRESULT A~ZMSG A~ZZRET
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLHD_LOG
        FROM   ZTBLHD_INF AS A INNER JOIN ZTBLIT_INF AS B
         ON A~ZFBLNO = B~ZFBLNO
        WHERE  A~ZEDAT        EQ   SY-DATUM AND
               A~ZRESULT      EQ   'E'  .

  LOOP AT IT_ZTBLHD_LOG.

* Create(B/L)
    CALL FUNCTION 'ZIM_WEB_TO_SAP_BL_TEST'
         EXPORTING
              ZFDOCNO   = IT_ZTBLHD_LOG-ZFBLNO
         TABLES
              IT_ZSBLHD = IT_ZSBLHD
              IT_ZSBLIT = IT_ZSBLIT.
* If error found - skip and process next record
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " create_BOL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_PO.
  DATA: IT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
          IT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows

  CALL METHOD WC_ALV_9000->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = IT_ROWS[]
                     ET_ROW_NO     = IT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

*  CLEAR: w_select, w_success, w_fail.

  READ TABLE IT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.

  READ TABLE IT_9000 INDEX IT_ROWS-INDEX.

  SET PARAMETER ID 'BES' FIELD IT_9000-EBELN.
  CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.


ENDFORM.                    " DISPLAY_PO
*&---------------------------------------------------------------------*
*&      Form  CREATE_BOL_FOREGROUD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_BOL_FOREGROUD TABLES PT_9000 STRUCTURE IT_9000.
  DATA: BEGIN OF WA_ZTBLHD_LOG.
          INCLUDE STRUCTURE ZTBLHD_INF.
  DATA:    END OF WA_ZTBLHD_LOG.

  DATA: IT_ZTBLHD_LOG LIKE TABLE OF WA_ZTBLHD_LOG WITH HEADER LINE.

  DATA: BEGIN OF IT_ZSBLHD OCCURS 1000.
          INCLUDE STRUCTURE ZSBLHD_INF.
  DATA  END OF IT_ZSBLHD.

  DATA: BEGIN OF IT_ZSBLIT OCCURS 1000.
          INCLUDE STRUCTURE ZSBLIT_INF.
  DATA  END OF IT_ZSBLIT.

  DATA: BEGIN OF  LT_ZFHBLNO OCCURS 1000,
        ZFHBLNO TYPE CHAR16,
        END OF LT_ZFHBLNO.

  LOOP AT PT_9000.
    LT_ZFHBLNO-ZFHBLNO = PT_9000-ZFHBLNO.
    APPEND LT_ZFHBLNO.
  ENDLOOP.

  SELECT DISTINCT A~ZFBLNO B~ZFCIVNO A~ZFHBLNO
            A~ZFMBLNO A~ZFBLDT A~ZFSHTY
            A~ZFVIA A~ZFVSL A~ZF20FT A~ZF40FT A~ZF45FT A~ZF40HQ
            A~ZFNEWT A~ZFNEWTM A~ZFTOVL A~ZFTOVLM A~ZFETD A~ZFETA
            A~ZFSPRT A~ZFAPRT A~ZFAPRTC A~ZFSPRTC A~ZUSER A~ZSDAT
            A~ZSTIM A~ZEDAT A~ZETIM A~ZMODE A~ZRESULT A~ZMSG A~ZZRET
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLHD_LOG
        FROM   ZTBLHD_INF AS A INNER JOIN ZTBLIT_INF AS B
         ON A~ZFBLNO = B~ZFBLNO
         FOR ALL ENTRIES IN LT_ZFHBLNO
        WHERE  A~ZFHBLNO        EQ LT_ZFHBLNO-ZFHBLNO AND
               A~ZRESULT      EQ   'E'  .

  LOOP AT IT_ZTBLHD_LOG.

* Create(B/L)
    CALL FUNCTION 'ZIM_WEB_TO_SAP_BL_TEST'
         EXPORTING
              ZFDOCNO   = IT_ZTBLHD_LOG-ZFBLNO
         TABLES
              IT_ZSBLHD = IT_ZSBLHD
              IT_ZSBLIT = IT_ZSBLIT.
* If error found - skip and process next record
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CREATE_BOL_FOREGROUD
