REPORT ZIMFAIA_AR_WITHOUT_VERSION1 .
TYPE-POOLS: imis, slis.
TABLES: imak, imakpa, imakps, imakpw.

DATA:
  HF_SELE    TYPE P,   " Z?ler ob Selektion nach Belegposition/-kopf
  HF_PFELD   TYPE P,                   " Z?ler Tabellenzeilen
  HF_COUNT   TYPE P,
  HF_REPID   LIKE  SYST-REPID,
  HF_STATUS TYPE SLIS_FORMNAME VALUE 'STANDARD_02',
  HF_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.

DATA: gt_selection_table TYPE imis_type_t_cedst WITH HEADER LINE,
      gt_imak            TYPE STANDARD TABLE OF imak WITH HEADER LINE,
      gt_imakpa          TYPE STANDARD TABLE OF imakpa WITH HEADER LINE,
      gt_imav            TYPE STANDARD TABLE OF imav WITH HEADER LINE,
      gt_imavz           TYPE STANDARD TABLE OF imavz WITH HEADER LINE,
      gt_imakt           TYPE STANDARD TABLE OF imakt WITH HEADER LINE,
      gd_kokrs           LIKE tka01-kokrs,
      gd_data_to_follow  TYPE c VALUE 'X',
      gd_repid           LIKE sy-repid,
      gd_layout          TYPE slis_layout_alv,
      gt_fieldcat        TYPE slis_t_fieldcat_alv,
      gd_variant         TYPE disvariant.

DATA: BEGIN OF gt_outtab OCCURS 0,
        posnr   LIKE imak-posnr,
        txt50   LIKE imakt-txt50,
        ivart   LIKE imak-ivart,
        ernam   LIKE imak-ernam,
        erdat   LIKE imak-erdat,
        aenam   LIKE imak-aenam,
        aedat   LIKE imak-aedat,
        vkokrs  LIKE imak-vkokrs,
        abukrs  LIKE imak-abukrs,
        vbukrs  LIKE imak-vbukrs,
        arcomp  LIKE imak-arcomp,
        vrcomp  LIKE imak-vrcomp,
        fkber   LIKE imak-fkber,
        tplnr   LIKE imak-tplnr,
        werks   LIKE imak-werks,
        sizecl  LIKE imak-sizecl,
        wdatu   LIKE imak-wdatu,
        priori  LIKE imak-priori,
        akostl  LIKE imakpa-akostl,
        agsber  LIKE imakpa-agsber,
        aprctr  LIKE imakpa-aprctr,
        vkostl  LIKE imak-vkostl,
        vprctr  LIKE imak-vprctr,
        vgsber  LIKE imak-vgsber,
        xextrn  LIKE imak-xextrn,
        gdatu   LIKE imak-gdatu,
        gjahr   LIKE imak-gjahr,
        stort   LIKE imak-stort,
        land1   LIKE imak-land1.
        INCLUDE STRUCTURE ima_usrfld.
DATA:   varnt   LIKE imav-varnt,
      END OF gt_outtab.

SELECT-OPTIONS: posid   FOR imak-posid,
                ivart   FOR imak-ivart.
SELECTION-SCREEN SKIP 1.

* search for requests without any version assignment
PARAMETERS: get_req RADIOBUTTON GROUP grp1 DEFAULT 'X'.
* search for single variants without version assignment
PARAMETERS: get_var RADIOBUTTON GROUP grp1.

SELECTION-SCREEN SKIP 1.

* status selection
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-003.
PARAMETERS: statsl LIKE rirasp-req_statsl.
SELECTION-SCREEN END   OF BLOCK block1.

* requesting orgUnits
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-004.
SELECT-OPTIONS: abukrs  FOR imak-abukrs,
                agsber  FOR imakpa-agsber,
                akostl  FOR imakpa-akostl,
                aprctr  FOR imakpa-aprctr,
                aspart  FOR imakps-aspart,
                amatkl  FOR imakpw-amatkl,
                arcomp  FOR imak-arcomp.
SELECTION-SCREEN END   OF BLOCK block2.

* display variant
SELECTION-SCREEN BEGIN OF BLOCK block4 WITH FRAME TITLE text-006.
PARAMETERS: pvariant         LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK block4.

* Set controlling area
CALL FUNCTION 'K_KOKRS_SET'
     IMPORTING
          e_kokrs = gd_kokrs.
gt_selection_table-fnam   = 'IMAK~VKOKRS'.
gt_selection_table-sign   = 'I'.
gt_selection_table-option = 'EQ'.
gt_selection_table-low    = gd_kokrs.
APPEND gt_selection_table.

* Move selections to selection table for AIAA_READ_REQUEST_SET
LOOP AT posid.
  MOVE-CORRESPONDING posid TO gt_selection_table.
  gt_selection_table-fnam = 'IMAK~POSID'.
  APPEND gt_selection_table.
ENDLOOP.

LOOP AT ivart.
  MOVE-CORRESPONDING ivart TO gt_selection_table.
  gt_selection_table-fnam = 'IMAK~IVART'.
  APPEND gt_selection_table.
ENDLOOP.

LOOP AT abukrs.
  MOVE-CORRESPONDING abukrs TO gt_selection_table.
  gt_selection_table-fnam = 'IMAK~ABUKRS'.
  APPEND gt_selection_table.
ENDLOOP.

LOOP AT arcomp.
  MOVE-CORRESPONDING arcomp TO gt_selection_table.
  gt_selection_table-fnam = 'IMAK~ARCOMP'.
  APPEND gt_selection_table.
ENDLOOP.

LOOP AT akostl.
  MOVE-CORRESPONDING akostl TO gt_selection_table.
  gt_selection_table-fnam = 'IMAKPA~AKOSTL'.
  APPEND gt_selection_table.
ENDLOOP.

LOOP AT agsber.
  MOVE-CORRESPONDING agsber TO gt_selection_table.
  gt_selection_table-fnam = 'IMAKPA~AGSBER'.
  APPEND gt_selection_table.
ENDLOOP.

LOOP AT aprctr.
  MOVE-CORRESPONDING aprctr TO gt_selection_table.
  gt_selection_table-fnam = 'IMAKPA~APRCTR'.
  APPEND gt_selection_table.
ENDLOOP.

LOOP AT aspart.
  MOVE-CORRESPONDING aspart TO gt_selection_table.
  gt_selection_table-fnam = 'IMAKPS~ASPART'.
  APPEND gt_selection_table.
ENDLOOP.

LOOP AT amatkl.
  MOVE-CORRESPONDING amatkl TO gt_selection_table.
  gt_selection_table-fnam = 'IMAKPW~AMATKL'.
  APPEND gt_selection_table.
ENDLOOP.

CALL FUNCTION 'MESSAGES_INITIALIZE'.

WHILE NOT gd_data_to_follow IS INITIAL.
* read appropriation requests from database
  CALL FUNCTION 'AIAA_READ_REQUEST_SET'
       EXPORTING
            it_selection_table  = gt_selection_table[]
            i_req_statsl        = statsl
            i_sel_req_tab_flg   = 'X'
            i_auth_check_flg    = 'X'
            i_auth_mes_stor_flg = 'X'
            i_auth_activity     = '03'
            package_size        = 500
       IMPORTING
            e_data_to_follow    = gd_data_to_follow
       TABLES
            et_imak             = gt_imak[]
            et_imakpa           = gt_imakpa[]
            et_imav             = gt_imav[]
            et_imavz            = gt_imavz[].


  SELECT * FROM imakt INTO TABLE gt_imakt
           FOR ALL ENTRIES IN gt_imak
           WHERE posnr = gt_imak-posnr AND
                 spras = sy-langu.

  SORT: gt_imak, gt_imakt, gt_imakpa BY posnr.
  SORT: gt_imav, gt_imavz BY posnr varnt.

  IF get_req = 'X'.
*   Search for appropriation requests without any version assignment
    LOOP AT gt_imak.
      READ TABLE gt_imavz WITH KEY posnr = gt_imak-posnr
                          BINARY SEARCH.
      IF sy-subrc <> 0.
*       store this request to the output list
        MOVE-CORRESPONDING gt_imak TO gt_outtab.
        READ TABLE gt_imakpa WITH KEY posnr = gt_imak-posnr
                             BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING gt_imakpa TO gt_outtab.
        ENDIF.
        READ TABLE gt_imakt WITH KEY posnr = gt_imak-posnr
                            BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING gt_imakt TO gt_outtab.
        ENDIF.
        APPEND gt_outtab.
      ENDIF.
    ENDLOOP.
  ELSE.
*   Search for variants without any version assignment
    LOOP AT gt_imav.
      READ TABLE gt_imavz WITH KEY posnr = gt_imav-posnr
                                   varnt = gt_imav-varnt
                          BINARY SEARCH.
      IF sy-subrc <> 0.
*       store this variant to the output list
        MOVE-CORRESPONDING gt_imav TO gt_outtab.
        READ TABLE gt_imak WITH KEY posnr = gt_imav-posnr BINARY SEARCH.
        MOVE-CORRESPONDING gt_imak TO gt_outtab.
        READ TABLE gt_imakpa WITH KEY posnr = gt_imak-posnr
                             BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING gt_imakpa TO gt_outtab.
        ENDIF.
        READ TABLE gt_imakt WITH KEY posnr = gt_imak-posnr
                            BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING gt_imakt TO gt_outtab.
        ENDIF.
        APPEND gt_outtab.

      ENDIF.
    ENDLOOP.
  ENDIF.

ENDWHILE.

gd_repid = sy-repid.
CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
     EXPORTING
          i_program_name     = gd_repid
          i_internal_tabname = 'GT_OUTTAB'
          i_inclname         = gd_repid
     CHANGING
          ct_fieldcat        = gt_fieldcat[].

* Remove field 'variant' from the field catalogue when searching for
* appropriation requests without any version assignment
IF get_req = 'X'.
  DELETE gt_fieldcat WHERE fieldname = 'VARNT'.
ENDIF.

gd_layout-max_linesize = 500.
gd_variant-report = sy-repid.
gd_variant-variant = pvariant.
HF_REPID = SY-REPID.
CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
     EXPORTING
          I_CALLBACK_PROGRAM       = HF_REPID
          is_layout                = gd_layout
          it_fieldcat              = gt_fieldcat[]
*         IT_EXCLUDING             =
*         IT_SPECIAL_GROUPS        =
          I_CALLBACK_USER_COMMAND  = HF_USER_COMMAND
          i_save                   = 'A'
          is_variant               = gd_variant
     TABLES
          t_outtab                 = gt_outtab.




FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                  RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN '&IC1'.                       "dobbleclick
         READ TABLE gt_outtab INDEX RS_SELFIELD-TABINDEX. "cursorposit.
         SET PARAMETER ID 'IAF' FIELD  GT_OUTTAB-POSNR.
         CALL TRANSACTION 'IMA2' AND SKIP FIRST SCREEN.
  ENDCASE.
ENDFORM.
