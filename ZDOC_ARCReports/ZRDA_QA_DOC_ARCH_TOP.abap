*&---------------------------------------------------------------------*
*&  Include           ZRDA_QA_DOC_ARCH_TOP
*&---------------------------------------------------------------------*
TABLES: ztda_posts, ztda_postmeta,   ztda_term_rel, ztda_meta_name,
        ztda_term_taxon, ztda_terms, ztsd_acm_h, ztpp_vm,
        zvda_arch01, lfa1.

* Screen buff
DATA  : g_aclnoonly        TYPE c,
        g_acl_no           TYPE zacln,
        g_vin_no           TYPE ze_vin,
        g_title(200)       TYPE c,
        g_lname            TYPE zedalname,
        g_fname            TYPE zedafname,
        g_month            TYPE spmon,
        g_cmonth           TYPE spmon,
        g_rmonth           TYPE spmon,
        g_link             TYPE c,
        g_lifnr            TYPE lifnr,
        g_name1            TYPE name1_gp.

DATA  : BEGIN OF is_g,
          aclno            TYPE zacln,
          vinno            TYPE ze_vin,
          cdate            TYPE dats,
          rdate            TYPE dats,
          err              TYPE c,
        END   OF is_g,
        BEGIN OF is_key,
          acl              TYPE c,
          vin              TYPE c,
          name             TYPE c,
          date             TYPE c,
        END   OF is_key.



DATA  : BEGIN OF is_buyback,
          aclonly          TYPE c,
          acl_no           TYPE zacln,
          vin_no           TYPE ze_vin,
          title(200)       TYPE c,
        END   OF is_buyback,
        is_buyback_s       LIKE is_buyback,

        BEGIN OF is_legal,
          aclonly          TYPE c,
          acl_no           TYPE zacln,
          vin_no           TYPE ze_vin,
          last_name        TYPE zemetaval,
          first_name       TYPE zemetaval,
          title(200)       TYPE c,
        END   OF is_legal,
        is_legal_s         LIKE is_legal,

        BEGIN OF is_claim,
          year_month       TYPE spmon,
          title(200)       TYPE c,
        END   OF is_claim.
DATA  : is_claim_s         LIKE is_claim.


DATA  : BEGIN OF is_ztda_posts.
        INCLUDE STRUCTURE ztda_posts.
DATA  : END   OF is_ztda_posts.


DATA  : BEGIN OF it_ztda_postmeta OCCURS 0.
        INCLUDE STRUCTURE ztda_postmeta.
DATA  : END   OF it_ztda_postmeta.


DATA  : BEGIN OF is_ztda_meta_name.
        INCLUDE STRUCTURE ztda_meta_name.
DATA  : END   OF is_ztda_meta_name.


DATA  : BEGIN OF is_ztda_term_rel.
        INCLUDE STRUCTURE ztda_term_rel.
DATA  : END   OF is_ztda_term_rel.


DATA  : BEGIN OF is_ztda_term_taxon.
        INCLUDE STRUCTURE ztda_term_taxon.
DATA  : END   OF is_ztda_term_taxon.
DATA  : cb1          TYPE c,
        cb2          TYPE c,
        cb3          TYPE c,
        cb4          TYPE c.


DATA  : g_post_id     TYPE zepostid,
        g_fldname(40) TYPE c,
        g_meta_id     TYPE zemetaid,
        g_aclno_o     TYPE zacln,
        g_postid_o    TYPE zepostid,
        g_title_o     TYPE zdl_post_title,
        g_meta_key    TYPE zemetakey,
        g_date        TYPE sydatum,
        g_date_tm     TYPE syuzeit,
        g_only,
        g_flg1,

        g_flg,
        g_proc        TYPE c,
        g_af          TYPE c,
        g_av          TYPE c,
        g_tid(5)      TYPE n.

DATA  : ok_code       LIKE sy-ucomm,
        save_ok       LIKE sy-ucomm,
        w_repid       LIKE sy-repid,
        w_cnt         TYPE   i.
*****************************************************************
DATA  : BEGIN OF last_document,
        archiv_id     LIKE toav0-archiv_id,
        arc_doc_id    LIKE toav0-arc_doc_id,
        doc_type      LIKE toadv-doc_type,
        END OF last_document.

DATA  : g_object_id   LIKE sapb-sapobjid
      , gv_sp         type c
      .
*****************************************************************


CONSTANTS: c_buyback  TYPE string VALUE `Buyback data input`,
           c_legal    TYPE string VALUE `Legal data input`,
           c_claim    TYPE string VALUE `Claim data input`,
           c_rclaim   TYPE string VALUE `Re-Claim data input`,
           gc_vinno   TYPE string VALUE 'VIN_NO',
           gc_name    TYPE string VALUE 'NAME',
           gc_date    TYPE string VALUE 'DATE',
           gc_only    TYPE string VALUE 'ACL_NO',
           gc_agree   TYPE string VALUE 'AGREEMENT',
           gc_meeting TYPE string VALUE 'MEETING',
           gc_report  TYPE string VALUE 'REPORT',
           gc_object  TYPE string VALUE 'OBJECTION'
           .

**************************************************************
** For ALV list
**************************************************************
TYPE-POOLS: slis.

DATA: gt_list_top_of_page TYPE slis_t_listheader. " value 'TOP_OF_PAGE'.

DATA: g_repid        TYPE sy-repid,
***      gt_fieldcat    TYPE LINE OF slis_t_fieldcat_alv OCCURS 0,
***      gt_fieldcat_ln LIKE LINE OF gt_fieldcat,
      gt_fieldcat    TYPE slis_t_fieldcat_alv,
      gs_fieldcat    LIKE LINE OF gt_fieldcat,
      gs_layout      TYPE slis_layout_alv,
      gt_sort        TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_variant     LIKE disvariant,            "VARIANT
      gt_event       TYPE slis_t_event,
      gs_print       TYPE slis_print_alv,
      gs_keyinfo     TYPE slis_keyinfo_alv,
      g_status       TYPE slis_formname VALUE 'STATUS_SET',
      g_top_of_page  TYPE slis_formname VALUE 'TOP_OF_THE_PAGE',
      gt_extab       TYPE slis_t_extab,
      g_user_command TYPE slis_formname VALUE 'USER_COMMAND',

      g_pos          TYPE i.
