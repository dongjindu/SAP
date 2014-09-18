*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_CPZP_CORRECTION_MASSTOP
*&---------------------------------------------------------------------*
REPORT  zsapbf_cpzp_correction_massive MESSAGE-ID zsapbf_qrprp NO STANDARD PAGE HEADING.

TABLES:ckmlmv013.

TYPE-POOLS: slis.
INCLUDE <icon>.

TYPES: BEGIN OF ts_aufnr,
       aufnr    TYPE aufnr,
       cc_guid  TYPE qrp_accassobj,
       END OF ts_aufnr.
TYPES: tt_aufnr TYPE STANDARD TABLE OF ts_aufnr.
*{ Add by SAPCDP08 2010-07-07 WIP correction report performance tunning
TYPES: tt_accassobj TYPE STANDARD TABLE OF qrp_accassobj.
*} end of Add

TYPES: tv_taskname(7) TYPE n.
TYPES: ts_aufnr_task TYPE zsapbf_aufnr_task.
TYPES: tt_aufnr_task TYPE STANDARD TABLE OF ts_aufnr_task.

DATA: gt_aufnr_task TYPE tt_aufnr_task.
*----------------------------------------------------------------------*
*   INCLUDE LPPC1PRDD1                                                 *
*----------------------------------------------------------------------*

* Abkürzungen
CONSTANTS:
*    char0 TYPE c VALUE '0',
*    char1 TYPE c VALUE '1',
*    char2 TYPE c VALUE '2',
*    char3 TYPE c VALUE '3',
*    char4 TYPE c VALUE '4',
*    char5 TYPE c VALUE '5',
*    char6 TYPE c VALUE '6',
*    char_ TYPE c VALUE ' ',
    chara TYPE c VALUE 'A',
    charc TYPE c VALUE 'C',
*    chard TYPE c VALUE 'D',
*    chare TYPE c VALUE 'E',
*    charf TYPE c VALUE 'F',
*    charh TYPE c VALUE 'H',
*    chari TYPE c VALUE 'I',
*    charl TYPE c VALUE 'L',
    charn TYPE c VALUE 'N',
*    charo TYPE c VALUE 'O',
*    charp TYPE c VALUE 'P',
*    charq TYPE c VALUE 'Q',
    charr TYPE c VALUE 'R',
    chars TYPE c VALUE 'S',
*    chart TYPE c VALUE 'T',
    charx TYPE c VALUE 'X'.
*    charw TYPE c VALUE 'W'.
*    charu TYPE c VALUE 'U'.
*    char_minus TYPE c VALUE '-'



* Konstanten für Application Log
CONSTANTS:
*    cf_al_object TYPE balobj_d VALUE 'PPCONF',
*    cf_al_subobj_pr TYPE balsubobj VALUE 'PROCESS_R3',
*    cf_al_extnumber_ppcgo TYPE balnrext
*                VALUE 'PPCGO',
*    cf_al_extnumber_step2 TYPE balnrext
*                VALUE 'PPCGO_STEP2',
*    cf_al_extnumber_recei TYPE balnrext
*                VALUE 'RECV_BACKFL',
*    cf_al_detlevel1 TYPE ballevel VALUE char1,
*    cf_al_detlevel2 TYPE ballevel VALUE char2,
*    cf_al_detlevel3 TYPE ballevel VALUE char3,
*    cf_al_detlevel4 TYPE ballevel VALUE char4,
*    cf_al_detlevel5 TYPE ballevel VALUE char5,
*    cf_al_detlevel6 TYPE ballevel VALUE char6,
*    cf_al_msgid LIKE sy-msgid VALUE 'PPC1PR',

    cf_al_appl_error TYPE c VALUE chara, "Applikationsfehler
    cf_al_syst_error TYPE c VALUE chars, "Systemfehler
    cf_al_comm_error TYPE c VALUE charc, "Kommunikationsfehler
    cf_al_nort_error TYPE c VALUE charn, "keine Prozeßrückkehr
    cf_al_ress_error TYPE c VALUE charr, "keine Ressource
*    cf_al_lock_error TYPE c VALUE charl, "keine Prozeßrückkehr
*    cf_al_updt_error TYPE c VALUE charu, "keine Ressource
    cf_al_ok         TYPE c VALUE space. "alles roger



* different status of posting processes
*CONSTANTS: cf_process_compl  TYPE c VALUE 'X',
*           cf_process_void   TYPE c VALUE 'V',
*           cf_process_todo   TYPE c VALUE ' '.
* processes that should be done
*CONSTANTS: cf_do     TYPE c VALUE 'X',
*           cf_dont   TYPE c VALUE ' '.
* sonstige Festwerte

* Zeitpunkt der Abarbeitung
*CONSTANTS:
*    cf_chron_ppcgo TYPE c VALUE chara,
*    cf_chron_recei TYPE c VALUE chars.

*CONSTANTS: con_error VALUE 'E',
*           con_chara VALUE 'A',
*           con_charc VALUE 'C',
*           con_charn VALUE 'N',
*           con_charx VALUE 'X'.

*CONSTANTS: con_worsnum(2) VALUE 'WO',         "WITHOUT RSNUM
*           con_wirsnum(2) VALUE 'WI'.         "WITH RSNUM

*--> Constants for PPC performance analysis
*CONSTANTS:
*  gc_astatopen   TYPE i VALUE 1,
*  gc_astatclose  TYPE i VALUE 2,
*  gc_ppcpa_key1  TYPE i VALUE 11,
*  gc_ppcpa_key2  TYPE i VALUE 12,
*  gc_ppcpa_key3  TYPE i VALUE 13,
*  gc_ppcpa_key4  TYPE i VALUE 14,
*  gc_ppcpa_key5  TYPE i VALUE 15,
*  gc_ppcpa_key6  TYPE i VALUE 16,
*  gc_ppcpa_key7  TYPE i VALUE 17,
*  gc_ppcpa_key8  TYPE i VALUE 18,
*  gc_ppcpa_key9  TYPE i VALUE 19,
*  gc_ppcpa_key10 TYPE i VALUE 20,
*  gc_ppcpa_key11 TYPE i VALUE 21.

*--> Variables for PPCGO in 2 steps
*DATA:
*  gf_splitcount  TYPE i VALUE 3,        "batch/stock split estimate
*  gf_mmdoclimit  TYPE i VALUE 9999.     "mm document positions limit

*CONSTANTS:
*  cf_statusone   TYPE c VALUE '1',
*  cf_statustwo   TYPE c VALUE '2',
*  cf_statusthree TYPE c VALUE '3',
*  cf_statusfour  TYPE c VALUE '4',
*  cf_statusfive  TYPE c VALUE '5',
*  cf_statussix   TYPE c VALUE '6'.

* ->not constant but it is never changed
*DATA: g_max_comp_number TYPE ppc_count VALUE '10000'.


**********************
*--> globale Varibalen
**********************
* Werte, die vom "Return_Info" gesetzt werden
DATA: gf_started_jobs     TYPE i VALUE 0,
      gf_received_jobs    TYPE i VALUE 0,
      gf_appl_errors      TYPE i VALUE 0.

* Tabelle der aufgerufenen Prozesse
*DATA: gt_tasklist_call TYPE HASHED TABLE OF tasklist_history
*                    WITH UNIQUE KEY taskname.

* Tabelle der beendeten Prozesse
*DATA: gt_tasklist_receive TYPE HASHED TABLE OF tasklist_history
*                   WITH UNIQUE KEY taskname.

* Struktur für Statistik
*DATA: gs_statistics TYPE ppcpr_type_statist.

CONSTANTS:
*  gc_application(8)  TYPE c  "LIKE tasklist_history-application
*    VALUE 'ZTWOSTP2',
*  gc_applic_tss1(8)  TYPE c VALUE 'ZTWOSTP1',
  gc_max_taskname    TYPE ps4s_mass_task_id VALUE 9999999,

*  gc_two_step  TYPE zsapbf_flg_ppc_type VALUE '2',
*  gc_processed TYPE char01 VALUE 'P',
*  gc_running   TYPE char01 VALUE 'R',
  gc_max_appl_errors TYPE ppc_mwerror VALUE 9999.

DATA:
  gv_gjper_curr TYPE co_gjper,
  gv_gjper_prev TYPE co_gjper.

DATA gt_cpzp    TYPE zsapbf_tt_cpzp.
DATA gt_cpzp_bk TYPE zsapbf_tt_cpzp.

* Add by SAPCD10 on 2010.07.09
DATA gv_ucomm   TYPE syucomm.

DATA: BEGIN OF gt_task OCCURS 0,
         name       TYPE char30,
         classname  TYPE rzlli_apcl,
         status     TYPE char1,
      END OF gt_task.

DATA : g_rcv       TYPE i,
       g_snd       TYPE i.

DATA : gt_aufnr TYPE TABLE OF aufnr_s,
       st_aufnr TYPE aufnr_s.
