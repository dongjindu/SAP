*---------- EEO-1 REPORT (USA COMPLIANCE REPORTS). -------------------*
*report rpseeou1 line-count 51 line-size 132 no standard page heading.
report rpseeou1 MESSAGE-ID PN line-size 200
                              line-count 65
                no standard page heading.
                                                            "XWIK029804
***********************************************************************
***********************************************************************
*   Corrections/Modifications:
* 4.6C WWQL9CK039367 01/26/2001 Note 211983
* 4.6C XIYL9CK020284 Employee exempted from report, Note 326744
* 4.6C XIYL9CK018050 note 188107
* 4.6B XIYPH9K009820 06/24/99 Update for requirement of 1999
* 4.6A WWQAHRK024351 Changed sorting of TAB02 and print of < 50 EES.  *
* 3.1H WJIP30K147213 Modification according to US GOVERNMENT STANDARD *
* 3.0C XWIP30K029804 271195 Line-Count entfernt
* 3.0B CSZK027910    091195 Felder im Selektionsbild vertikal         *
*                           angepasst.                                *
* 3.0B XWIP30K013937 021095 Rahmen im Selektionsbild eingefuegt
* 3.0                                                                 *
* VQSK098667  30.09.94  Change process of T5U13-EEOCT to be converted
*                       via Feature EEOCT rather than read directly.
* 2.2A                                                                *
* VQLK081488  12.05.94  New version (CSZ)                             *
*                                                                     *
***********************************************************************
* CHANGED BY   : HO JOONG. HWANG
* CHANGED ON   : 2003.10.15
* DESCRIPTION  : Layout Change
***********************************************************************
***********************************************************************
*                          DATA SOURCES                               *
***********************************************************************

tables: pernr,                         "Master data
        pme25,                         "Structure for feature EEOCL
        t001p,                         "WERKS, BTRTL
        t503,                          "Persongroup/-subgroup
        t505r,                         "Ethnic origin check table
        t5u13,                         "Job -> EEO classification
        t5u0e,                         "Historical EEO data
*       t536a,                         "Addresses WJIP30K147213
        t5uru,                         "US Reporting Units  WJIP30K14721
        t5uad,                         "US Reporting Unit Addresses
        t5u0p.                         "US specific plant/-section

infotypes: 0001,                       "Org. data (WERKS, BTRTL, STELL)
           0002,                       "Pers. data (GESCH)
           0077.                       "Addl. pers. data (RACKY)

***********************************************************************
*                          CONSTANT VALUES                            *
***********************************************************************

data: begin of const,
         prodt(11) value '06,07,08,09',
                                       "Categories for prod. trainees
         whita(2) value '12',          "Assignment for prof. trainees
         proda(2) value '13',          "Assignment for prod. trainees
         jobct(26) value '01,02,03,04,05,06,07,08,09',
                                       "Valid job categories
         train like t503-austa value '1',
                                       "Trainee ident. (T503)
         forms like t512p-forml value 'EEO1',
                                       "Formular id in T512P/Q
                                       "Single establishment
         formm like t512p-forml value 'EEO2',
                                       "Formular id in T512P/Q
                                       "Multi establishment
         molga like t001p-molga value '10',
                                       "Country modifier for US
*         anart like t536a-anart value '/U',              "WJIP30K147213
      end of const.                    "Address type for T536A

***********************************************************************
*                         DATA INCLUDES                               *
***********************************************************************

INCLUDE ZPSFRMUD.
*include rpsfrmud.                      "Include data for forms
INCLUDE ZPSERRUD.
*include rpserrud.                      "Include data for error routines

***********************************************************************
*                            VARIABLES                                *
***********************************************************************

data: begin of tab01 occurs 0,         "Extract of relevant master data
         werks like p0001-werks,
         btrtl like p0001-btrtl,
         persg like p0001-persg,
         persk like p0001-persk,
         racky like p0077-racky,
         gesch like p0002-gesch,
         eeoct like t5u13-eeoct,       " -> EEO job category
         count type i,
      end of tab01.

data: begin of tab02 occurs 50,        "Table with race/gender info
         multi(1),                     "Indicates 'Single or 'Multi
                                       "Establishment employer
         coind like t5u0p-coind,       "By company/unit id and job cat
         repun like t5u0p-repun,
         eeoct like t5u13-eeoct,       " -> EEO job category
         train(1),                     "Trainee indicator
         eeocl(1),                     " -> result from feature EEOCL
                                       "    race and gender -> emp.cat
         eeohq,                                             "
         count type i,
      end of tab02.

data: begin of coind occurs 5,         "Table to control single/multi
                                       "Establishment employers
         coind like t5u0p-coind,       "Company id
         count type i,
      end of coind.

data: begin of i5u0p occurs 5.         "Internal table I5U0P
        include structure t5u0p.       "WERKS/BTRTL -> US specifics
data: end of i5u0p.

data: begin of i505r occurs 5,         "Internal table T505R
        racky like t505r-racky,        "Check table P0077-RACKY
      end of i505r.

data: begin of rg2ee occurs 20,        "Will contain all occurring
         racky like p0077-racky,       "Race/gender combinations
         gesch like p0002-gesch,       "And corresponding EEO cat.
         eeocl(1),                     " -> result from feature EEOCL
                                       "    race and gender -> emp.cat
      end of rg2ee.

data: begin of train occurs 30,        "Contains persg/persk combin.
         persg like p0001-persg,       "And 'x' in TRAIN if apprentice
         persk like p0001-persk,
         train,
      end of train.

*------------------------------------------------------------ VQSK098667
data: begin of rconv occurs 100,       "Contains T5U13-EEOCT categories
         eeoct like t5u13-eeoct,       "and the correspondign row number
         rowno like t5u13-eeoct,       "01 to 09 in the EEO report
      end of rconv.
*------------------------------------------------------------ VQSK098667

data: begin of line1,
                                       "General info
         pagex(4),                     "Page number for mutli-est.
         coind like t5u0p-coind,       "Company id
         repun like t5u0p-repun,       "Reporting unit
         sicno like t5u0p-sicno,       "Sic number
         naics like t5u0p-naics,       "NAIC number       "RPGL9CK056416
         yearx(4),                     "Filing year
                                       "Section a
         typex,                        "Type of report
         title(40),                    "Type of report (text)
                                                            "Section b1
         col01(35),                    "Company name
         col02(35),                    "Company addr.
*        col03(35),                    "Company city/county PH9K009820
         col03(25),                    "Company city        PH9K009820
*        col05(10),                    "Company county      PH9K009820
         COL05(18),                    "Company county    VSDL9CK065654
         col04(35),                    "Company state/zip
         coein(10),                    "Company einum
                                                            "Section b2
         run01(35),                    "Repun name
         run02(35),                    "Repun addr.
*        run03(35),                    "Repun city/county  PH9K009820
         run03(25),                    "Repun city         PH9K009820
*        run05(10),                    "Repun county       PH9K009820
         RUN05(18),                    "Repun county       VSDL9CK065654
         run04(35),                    "Repun state/zip    PH9K009820
         einum(10),                    "Repun einum
                                       "Section c
         secc1 like t5u0e-secc1,       "100+ employees??
         secc2 like t5u0e-secc2,       "Affiliated with 100+??
         secc3 like t5u0e-secc3,       "Question C3??
         secc4 like t5u0e-secc4,       "SBA assistance??
*        dandb LIKE t5u0p-dandb,       "D&b number     PH9K009820
         dandb(11),                    "D&b number     PH9K009820
                                       "Section D
                                       "Row 01, officials/mgr
         xx01b    type i,              "Cat B
         xx01c    type i,              "Cat C
         xx01d    type i,              "Cat D
         xx01e    type i,              "Cat E
         xx01f    type i,              "Cat F
         xx01g    type i,              "Cat G
         xx01h    type i,              "Cat H
         xx01i    type i,              "Cat I
         xx01j    type i,              "Cat J
         xx01k    type i,              "Cat K
         row01    type i,              "Row total
                                       "Row 02, professionals
         xx02b    type i,              "Cat B
         xx02c    type i,              "Cat C
         xx02d    type i,              "Cat D
         xx02e    type i,              "Cat E
         xx02f    type i,              "Cat F
         xx02g    type i,              "Cat G
         xx02h    type i,              "Cat H
         xx02i    type i,              "Cat I
         xx02j    type i,              "Cat J
         xx02k    type i,              "Cat K
         row02    type i,              "Row total
                                       "Row 03, technicians
         xx03b    type i,              "Cat B
         xx03c    type i,              "Cat C
         xx03d    type i,              "Cat D
         xx03e    type i,              "Cat E
         xx03f    type i,              "Cat F
         xx03g    type i,              "Cat G
         xx03h    type i,              "Cat H
         xx03i    type i,              "Cat I
         xx03j    type i,              "Cat J
         xx03k    type i,              "Cat K
         row03    type i,              "Row total
                                       "Row 04, sales workers
         xx04b    type i,              "Cat B
         xx04c    type i,              "Cat C
         xx04d    type i,              "Cat D
         xx04e    type i,              "Cat E
         xx04f    type i,              "Cat F
         xx04g    type i,              "Cat G
         xx04h    type i,              "Cat H
         xx04i    type i,              "Cat I
         xx04j    type i,              "Cat J
         xx04k    type i,              "Cat K
         row04    type i,              "Row total
                                       "Row 05, office/clerical
         xx05b    type i,              "Cat B
         xx05c    type i,              "Cat C
         xx05d    type i,              "Cat D
         xx05e    type i,              "Cat E
         xx05f    type i,              "Cat F
         xx05g    type i,              "Cat G
         xx05h    type i,              "Cat H
         xx05i    type i,              "Cat I
         xx05j    type i,              "Cat J
         xx05k    type i,              "Cat K
         row05    type i,              "Row total
                                       "Row 06, craft workders
         xx06b    type i,              "Cat B
         xx06c    type i,              "Cat C
         xx06d    type i,              "Cat D
         xx06e    type i,              "Cat E
         xx06f    type i,              "Cat F
         xx06g    type i,              "Cat G
         xx06h    type i,              "Cat H
         xx06i    type i,              "Cat I
         xx06j    type i,              "Cat J
         xx06k    type i,              "Cat K
         row06    type i,              "Row total
                                       "Row 07, operatives
         xx07b    type i,              "Cat B
         xx07c    type i,              "Cat C
         xx07d    type i,              "Cat D
         xx07e    type i,              "Cat E
         xx07f    type i,              "Cat F
         xx07g    type i,              "Cat G
         xx07h    type i,              "Cat H
         xx07i    type i,              "Cat I
         xx07j    type i,              "Cat J
         xx07k    type i,              "Cat K
         row07    type i,              "Row total
                                       "Row 08, laborers
         xx08b    type i,              "Cat B
         xx08c    type i,              "Cat C
         xx08d    type i,              "Cat D
         xx08e    type i,              "Cat E
         xx08f    type i,              "Cat F
         xx08g    type i,              "Cat G
         xx08h    type i,              "Cat H
         xx08i    type i,              "Cat I
         xx08j    type i,              "Cat J
         xx08k    type i,              "Cat K
         row08    type i,              "Row total
                                       "Row 09, service workers
         xx09b    type i,              "Cat B
         xx09c    type i,              "Cat C
         xx09d    type i,              "Cat D
         xx09e    type i,              "Cat E
         xx09f    type i,              "Cat F
         xx09g    type i,              "Cat G
         xx09h    type i,              "Cat H
         xx09i    type i,              "Cat I
         xx09j    type i,              "Cat J
         xx09k    type i,              "Cat K
         row09    type i,              "Row total
                                       "Row 10, column totals
         colmb    type i,              "Cat B
         colmc    type i,              "Cat C
         colmd    type i,              "Cat D
         colme    type i,              "Cat E
         colmf    type i,              "Cat F
         colmg    type i,              "Cat G
         colmh    type i,              "Cat H
         colmi    type i,              "Cat I
         colmj    type i,              "Cat J
         colmk    type i,              "Cat K
         total    type i,              "Total
                                       "Row 11, previously rp.
         pcolb    type i,              "Cat B
         pcolc    type i,              "Cat C
         pcold    type i,              "Cat D
         pcole    type i,              "Cat E
         pcolf    type i,              "Cat F
         pcolg    type i,              "Cat G
         pcolh    type i,              "Cat H
         pcoli    type i,              "Cat I
         pcolj    type i,              "Cat J
         pcolk    type i,              "Cat K
         ptotl    type i,              "Total
                                       "Row 12, white collar
         xx12b    type i,              "Cat B   trainees
         xx12c    type i,              "Cat C
         xx12d    type i,              "Cat D
         xx12e    type i,              "Cat E
         xx12f    type i,              "Cat F
         xx12g    type i,              "Cat G
         xx12h    type i,              "Cat H
         xx12i    type i,              "Cat I
         xx12j    type i,              "Cat J
         xx12k    type i,              "Cat K
         row12    type i,              "Row total
                                       "Row 13, white collar
         xx13b    type i,              "Cat B   trainees
         xx13c    type i,              "Cat C
         xx13d    type i,              "Cat D
         xx13e    type i,              "Cat E
         xx13f    type i,              "Cat F
         xx13g    type i,              "Cat G
         xx13h    type i,              "Cat H
         xx13i    type i,              "Cat I
         xx13j    type i,              "Cat J
         xx13k    type i,              "Cat K
         row13    type i,              "Row total
                                       "Section e
         sece1 like t5u0e-sece1,       "Same est. as last yr
         sece2 like t5u0e-sece2,       "Same act. as last yr
         sece3 like t5u0e-sece3,       "Narrative 1
         sece4 like t5u0e-sece4,       "Major changes
         sece5 like t5u0e-sece5,       "Do.
         sece6 like t5u0e-sece6,       "Do.
                                       "Other questions
         begda(10),                    "From period
         endda(10),                    "To period
         apind like t5u0e-apind,       "Does... apprentices
         CNAME(35),         "Name of certifying official. VSDL9CK065654
         CTITL(35),         "Title of certifying official. VSDL9CK065654
      end of line1.

data: begin of linec.                  "Consolidated record for
        include structure line1.       "For consolidated report
data: end of linec.

data: begin of corpx,                  "Corporate info
                                       "General info
         coind like t5u0p-coind,       "Company id
         repun like t5u0p-repun,       "Reporting unit
         sicno like t5u0p-sicno,       "Sic number
         naics like t5u0p-naics,       "NAIC number       "RPGL9CK056416
                                                            "Section b1
         col01(35),                    "Company name
         col02(35),                    "Company addr.
*        col03(35),                    "Company city/county  PH9K009820
         col03(25),                    "Company city         PH9K009820
*        col05(10),                    "Company county       PH9K009820
         col05(18),                  "Company county       VSDL9CK065654
         col04(35),                    "Company state/zip
         coein(10),                    "Company einum
*        dandb LIKE t5u0p-dandb,       "D&b number           PH9K009820
         dandb(11),                    "D&b number           PH9K009820
         yearx(4),                     "Filing year
         begda(10),                    "From period
         endda(10),                    "To period
      end of corpx.

data: begin of temps,                  "Temporary fields
         subrc like sy-subrc,
         rcode like sy-subrc,
         stell like p0001-stell,
         werks like p0001-werks,
         btrtl like p0001-btrtl,
         repun like t5u0p-repun,
         coind like t5u0p-coind,
         fname like t512q-fname,
         eeoct like t5u13-eeoct,                            "VQSK098667
         rejec,
      end of temps.

data: begin of swtch,
         wrkna,                        "Indicates plant/-section has
                                       "The right MOLGA (end-of-sel)
         previ,                        "Indicates previous rec.found
       end of swtch.

data: begin of testx,                  "Fields to test parameters
         oldpe(6) type n,              "Previous period
         newpe(6) type n,              "Current period
      end of testx.

* List for establishment with <50 employees.        XIYPH9K009820
data: begin of tablist occurs 0,                            "PH9K009820
      run01(35),                       "Repun name          PH9K009820
      run02(35),                       "Repun address       PH9K009820
      run03(25),                       "Repun city          PH9K009820
      run05(10),                       "Repun county        PH9K009820
      run04(35),                       "Repun state/zip     PH9K009820
      total type i,                    "Total               PH9K009820
      sece3 like t5u0e-sece3,          "Majoy activity      PH9K009820
      sece4 like t5u0e-sece4,          "Majoy activity      PH9K009820
      sece5 like t5u0e-sece5,          "Majoy activity      PH9K009820
      sece6 like t5u0e-sece6,          "Majoy activity      PH9K009820
      end of tablist.                                       "PH9K009820
****Electronic File************
DATA: BEGIN OF FILE OCCURS 0,                    "VSDL9CK065654
          DAT(1074),                             "VSDL9CK065654
 END OF FILE.                                    "VSDL9CK065654
DATA : W_WIDTH TYPE I.

***********************************************************************
*                             PARAMETERS                              *
***********************************************************************

selection-screen begin of block frm1 with frame title frametxt.
"XWIP30K013937
*selection-screen uline.
selection-screen begin of line.
*election-screen comment 1(33) text-ss2. "which period for previous
selection-screen comment 1(31) text-ss2. "which period   "CSZK027910
parameters:                            "Reported total
   pa_rperi like t5u0e-rperi,          "Period to compare
   pa_ryear like t5u0e-ryear obligatory.
selection-screen end of line.
selection-screen uline.
selection-screen begin of line.
*election-screen comment 1(33) text-ss3. "which period to read/update
selection-screen comment 1(31) text-ss3. "which period to  "CSZK027910
parameters:
   pa_cperi like t5u0e-rperi,          "Period to read/update
   pa_cyear like t5u0e-ryear obligatory.
selection-screen end of line.
selection-screen uline.
selection-screen begin of line.
selection-screen comment 1(31) text-ss6.
parameters:
   pa_rtype as checkbox.               "Report type (s = special)
selection-screen end of line.
selection-screen begin of line.
*election-screen comment 1(33) text-ss5. "Write results in table T5U0E?
selection-screen comment 1(31) text-ss5. "Write results in  "CSZK027910
parameters:
   pa_updat as checkbox                "Update T5U0E?
            default text-pa4.          "_ _
selection-screen end of line.

selection-screen end of block frm1.    "XWIP30K013937
******                                                "VSDL9CK065654
SELECTION-SCREEN BEGIN OF BLOCK FRM2 WITH FRAME TITLE TEXT-CER.
PARAMETERS: TITLE(35) TYPE C OBLIGATORY,  "Title Of Certifying Official.
            NAME(35)  TYPE C OBLIGATORY,  "Name Of Certifying Official.
            TELNO(10) TYPE C OBLIGATORY,  "Tele. No. with area code
            FAXNO(10) TYPE C OBLIGATORY,  "FAX No. with area code
            EMAIL(40) TYPE C .  "E-mail of certifying official.
SELECTION-SCREEN END OF BLOCK FRM2.

SELECTION-SCREEN BEGIN OF BLOCK FRM3 WITH FRAME TITLE TEXT-OPT.
PARAMETERS: RAD1  RADIOBUTTON GROUP RAD,         "PAPER
            RAD2  RADIOBUTTON GROUP RAD,         "FILE
            FILENAME LIKE RLGRAP-FILENAME.       "FILENAME
SELECTION-SCREEN END OF BLOCK FRM3.
******                                             "VSDL9CK065654
***********************************************************************
*                           INITIALIZATION                            *
***********************************************************************

initialization.
  perform init_selscr.                 "Initialize statusflags and
  "Parameters in selection screen
  frametxt = pnp_rep_spec_standard_frametxt.             "XWIP30K013937
***********************************************************************
*                         AT-SELECTION-SCREEN                         *
***********************************************************************
AT SELECTION-SCREEN ON FILENAME.            "VSDL9CK065654
  IF RAD2 = 'X' AND FILENAME IS INITIAL.
    MESSAGE E150.
  ENDIF.                                    "VSDL9CK065654

***********************************************************************
*                         START-OF-SELECTION                          *
***********************************************************************

start-of-selection.
  perform start_checks using           "Check forms etc.
                       temps-rcode.
  if temps-rcode ne 0.                 "Exit on errors
    exit.
  endif.

  perform fill_i505r.                  "Fill I505R from T505R

  perform fill_rconv.                  "Fill table RCONV     "VQSK098667

***********************************************************************
*                              GET LOOP                               *
***********************************************************************

get pernr.
  clear: tab01,                        "Clear all temporary variables
         temps.

  rp_provide_from_last p0001 space pn-begda pn-endda.  "ORG.DATA
  perform err_chk1 using p0001-infty   "See whether valid
                         '0001'
                         pernr-pernr.
  move: p0001-werks to tab01-werks,    "Transfer to internal table
        p0001-btrtl to tab01-btrtl,
        p0001-persg to tab01-persg,
        p0001-persk to tab01-persk,
        p0001-stell to temps-stell.

  rp_provide_from_last p0002           "Pers. data
                       space
                       pn-begda
                       pn-endda.
  perform err_chk1 using p0002-infty   "See whether valid
                         '0002'
                         pernr-pernr.

  if p0002-gesch eq space.                                "RPGL9CK069143
    perform err_log using text-tbd
                          pernr-pernr
                          space
                          space
                          space.
    reject.
  endif.                                                  "RPGL9CK069143

  move: p0002-gesch to tab01-gesch.

  rp_provide_from_last p0077           "Addl. pers. data
                       space
                       pn-begda
                       pn-endda.       "ADDL.PERS.DATA
  perform err_chk1 using p0077-infty   "See whether valid
                         '0077'
                         pernr-pernr.
* employee exempted from eeo report                        "Note 326744
  if p0077-eeoex eq 'X'.                                   "Note 326744
    perform err_log using text-e21                         "Note 326744
                          pernr-pernr                      "Note 326744
                          space                            "Note 326744
                          space                            "Note 326744
                          space.                           "Note 326744
    reject.                                                "Note 326744
  endif.                                                   "Note 326744
  move: p0077-racky to tab01-racky.
  perform rei505r using tab01-racky    "Valid ethnic origin?
                        temps-subrc.

  if temps-subrc gt 0.
    perform err_log using text-e14     "Invalid RACKY
                          pernr-pernr
                          tab01-racky
                          space
                          space.
    move 'X' to temps-rejec.           "Mark for rejection
  endif.                               "temps-SUBRC gt 0

*------------------------------------------------------------ VQSK098667
* PERFORM RE5U13 USING TEMPS-STELL     "Get the job category for (DEL)
*                      PN/ENDDA        "Job from table T5U13     (DEL)
*                      TAB01-EEOCT.                              (DEL)

  perform re5u13 using temps-stell     "Get the job category for the
                       pn-endda        "Job from table T5U13
                       temps-eeoct.

  perform re_rconv using temps-eeoct   "Convert EEOCT to EEO row number
                         tab01-eeoct.
*------------------------------------------------------------ VQSK098667

  if const-jobct ns tab01-eeoct.       "Is it a valid job category?
    perform err_log using text-e01     "Invalid STELL cat.
                          pernr-pernr
                          temps-stell
                          '->'
                          tab01-eeoct.
    move 'X' to temps-rejec.           "Mark for rejection
  endif.
  if temps-rejec eq 'X'.               "If marked for rejection
    reject.                            "Do it.
  else.
    tab01-count = 1.                   "Count it
    collect: tab01.
  endif.                               "Temps-rejec

***********************************************************************
*                           END-OF-SELECTION                          *
***********************************************************************

end-of-selection.

  perform loop_tab01.                  "Fill the different auxiliary
  "Tables (RG2EE, TRAIN, etc.)
  perform fill_rg2ee.                  "Get the EEO category for all
  "Race/gender combination
  perform fill_train.                  "Get training status for all
  "Persg/persk combinations
  sort: rg2ee, train, i5u0p.           "For faster search

  perform tab01_to_tab02.              "Now put it in the right format

  perform single_or_mulit.             "Mark whether single or multi
  "Establishment employer in TAB02
  perform print_pages.                 "And print it

  perform print_tablist.               "XIYPH9K009820

* perform print_log.                   "Print log

***********************************************************************
*                           SUBROUTINES                               *
***********************************************************************
*---------------------------------------------------------------------*
*       FORM INIT_SELSCR                                              *
*---------------------------------------------------------------------*
*       INITIALIZES THE SELECTION SCREEN AT INITIALIZATION.           *
*       SETS THE STATUS PARAMETERSSCREEN AT INITIALIZATION.           *
*---------------------------------------------------------------------*

form init_selscr.
  move sy-datum to pnpendda.           "Set end date to system date
  move '01' to pnpendda+6(2).          "First day of current month
  pnpendda = pnpendda - 1.             "Last day of previous month
  pnpbegda = pnpendda.                 "Last day for begda/endda
  move '01' to pnpbegda+6(2).          "First day of previous month
  move: pnpbegda to pnpbegps,          "Move those dates to the date
        pnpendda to pnpendps.          "Selection parameters
  clear: pnpstatu.
  refresh: pnpstatu.
  pnpstatu-sign   = 'I'.               "Include
  pnpstatu-option = 'CP'.              "Pattern
  pnpstatu-low    = text-sta.          "+3+
  append pnpstatu.
endform.                               "INIT_SELSCR

*---------------------------------------------------------------------*
*       FORM START_CHECKS                                             *
*---------------------------------------------------------------------*
*       CHECKS WHETHER THE EEO FORMS ARE CONTAINED IN T512P/Q         *
*       AND WHETHER THE VARIABLES IN T512Q ARE VALID.                 *
*---------------------------------------------------------------------*
*  <--  RCODE = INDICATOR FOR SUCCESS OR FAILURE                      *
*---------------------------------------------------------------------*

form start_checks using rcode.
  perform form_check using             "Check the forms
                     const-molga
                     const-forms       "Single establishment emp.
                           rcode.
  if rcode gt 0.                       "Not ok? Stop it
    exit.
  endif.
  perform form_check using             "Check the forms
                     const-molga
                     const-formm       "Multi establishment emp.
                           rcode.
  if rcode gt 0.                       "Not ok? Stop it
    exit.
  endif.
  move pa_ryear to testx-oldpe+0(4).
  move pa_rperi to testx-oldpe+4(2).
  move pa_cyear to testx-newpe+0(4).
  move pa_cperi to testx-newpe+4(2).
  if testx-oldpe ge testx-newpe.       "If previous period >= current
    write: / text-e12.                 "Previous per.ge than current
    move 5 to rcode.
    exit.
  endif.
endform.                               "START-CHECKS

*---------------------------------------------------------------------*
*       FORM FORM_CHECK                                               *
*---------------------------------------------------------------------*
*       CALLS THE FORM CHECKING ROUTINE IN RPSFRM0S (INCLUDE) TO      *
*       VERIFY THE CORRECTNESS OF THE FORMS CONTAINED IN T512P/Q.     *
*---------------------------------------------------------------------*
*  -->  MOLGA = COUNTRY MODIFIER FOR FORM ROUTINES                   *
*  -->  FORML = FORM TO CHECK                                         *
*  <--  RCODE = INDICATOR FOR SUCCESS OR FAILURE                      *
*---------------------------------------------------------------------*

form form_check using molga forml rcode.
  perform test_form using sy-langu     "Check the form
                          molga
                          forml
                          temps-subrc
                          temps-fname.
  if temps-subrc gt 0.                 "If there's an error
    if temps-subrc eq 1.               "Check which one
      perform err_log using text-er1   "Form not in T512P.
                            temps-fname
                            'FORM:'
                            forml
                            space.
    endif.
    if temps-subrc eq 2.
      perform err_log using text-er2   "Form not in T512Q.
                            temps-fname
                            'FORM:'
                            forml
                            space.
    endif.
    if temps-subrc eq 3.
      perform err_log using text-er3   "Invalid fieldname in T512Q
                            temps-fname
                            'FORM:'
                            forml
                            space.
    endif.
    stop.
  endif.
endform.                               "FORM_CHECK

*---------------------------------------------------------------------*
*       FORM FILL_I505R                                               *
*---------------------------------------------------------------------*
*       FILLS INTERNAL TABLE I505R FROM T505R WITH VALID CODES FOR    *
*       P0077-RACKY.                                                  *
*---------------------------------------------------------------------*
form fill_i505r.
  clear: t505r,
         i505r.
  select * from t505r where
                molga eq const-molga.
    move-corresponding t505r to i505r.
    append i505r.
  endselect.
  if sy-subrc gt 0.
    perform err_log using text-e13     "No entries in T505R for MOLGA
                          const-molga
                          space
                          space
                          space.
  endif.
endform.                                                    "FILL_I505R

*----------------------------------------------------------- VQSK098667
*---------------------------------------------------------------------*
*       FORM FILL_RCONV                                               *
*---------------------------------------------------------------------*
*       FILLS TABLE RCONV WITH ALL CURRENT EEO CATEGORIES             *
*       (T5U13-EEOCT). LOOPING OVER TABLE RCONV, THE FEATURE EEOCT IS *
*       EVALUATED AND THE FIELD RCONV-ROWNO IS FILLED.                *
*---------------------------------------------------------------------*
form fill_rconv.
  clear: t5u13,
         rconv.
  select * from t5u13                  "All currently valid entries
                where endda ge pn-endda
                  and begda le pn-endda.
    move t5u13-eeoct to rconv-eeoct.
    collect rconv.
  endselect.

  clear: rconv.                        "Now we neet to assign the row
  loop at rconv.                       "numbers to the EEOCT values
    move-corresponding rconv to pme25. "PME25 is structure for
    "Feature EEOCT
    "The standard routine for
    "Reading decision trees
    "Contained in incl
                                                            "RPUMKC00
    perform re549d using 'EEOCT'       "Use the feature
                         ' '
                         rconv-rowno
                         temps-subrc.

    if rconv-rowno eq 'X' or           "If not found
       temps-subrc gt 0.
      perform err_log using
                      text-e15         "Feature EEOCT: No entry for
                      rconv-eeoct
                      space
                      space
                      space.
    endif.
    modify rconv.
  endloop.
endform.                               "FILL_RCONV
*------------------------------------------------------------- VQSK98667

*------------------------------------------------------------- VQSK98667
*---------------------------------------------------------------------*
*       FORM RE_RCONV                                                 *
*---------------------------------------------------------------------*
*       READS TABLE RCONV AND RETURNS THE EEO-1 REPORT ROWNUMBER      *
*---------------------------------------------------------------------*
*  -->  EEOCT = EEO CLASSIFICATION FOR A JOB (T5U13-EEOCT)
*  <--  ROWNO = ROW IN EEO-1 REPORT (01 TO 09)
*---------------------------------------------------------------------*
form re_rconv using eeoct rowno.
  if rconv-eeoct eq eeoct.             "Same as last call
    move rconv-rowno to rowno.
  else.
    loop at rconv where eeoct eq eeoct.
      move rconv-rowno to rowno.
    endloop.
    if sy-subrc gt 0.                  "No entry in RCONV
      clear: rconv.
      move 'X' to rowno.               "Mark it
    endif.                             "SY-SUBRC
  endif.                               "RCONV-EEOCT EQ EEOCT
endform.                               "RE_CONV
*------------------------------------------------------------ VQSK098667

*---------------------------------------------------------------------*
*       FORM LOOP_TAB01                                               *
*---------------------------------------------------------------------*
*       CHECKS - WHETHER PLANTS IN THE COLLECTED TABLE HAVE THE       *
*                CORRECT MOLGA                                        *
*              - WHETHER COIND AND REPUN ARE IN T5U0P                 *
*       FILLS  - INTERNAL TABLE I5U0P                                 *
*                INTERNAL TABLE TRAIN    PERSG/PERSK -> TRAINEE?      *
*                INTERNAL TABLE RG2EE    RACE/GENDER -> EEO CLASS     *
*                FOR LATER READING                                    *
*---------------------------------------------------------------------*

form loop_tab01.
  clear: tab01, temps.
  sort tab01 by werks
                btrtl
                persg
                persk
                racky
                gesch
                eeoct.
  loop at tab01.
    if temps-werks ne tab01-werks or   "On change of WERKS/BTRTL
       temps-btrtl ne tab01-btrtl.
      clear: swtch-wrkna.
      move: tab01-werks to temps-werks,
            tab01-btrtl to temps-btrtl.
      rp-read-t001p tab01-werks
                    tab01-btrtl
                    space.
      if sy-subrc gt 0.
        perform err_log using          "Append error log
                        text-e02       "No entry in T001P for:
                        tab01-werks
                        tab01-btrtl
                        space
                        space.
        move 'X' to swtch-wrkna.       "Plant no good
      else.
        if t001p-molga ne const-molga. "Desired country?
          move 'X' to swtch-wrkna.     "MOLGA no good
        endif.                         "T001P-MOLGA
        perform re5u0p using           "Get us info from table T5U0P
                       tab01-werks
                       tab01-btrtl
                       i5u0p.
        if i5u0p(1) eq '*'.            "Not a valid entry in T5U0P
          perform err_log using        "No entry in T5U0P for:
                          text-e03     "No entry in T5U0P for:
                          tab01-werks
                          tab01-btrtl
                          space
                          space.
          move 'X' to swtch-wrkna.     "Don't use this this pland
        else.
          if i5u0p-coind eq space and  "If coind and repun not filled
             i5u0p-repun eq space.
            perform err_log using      "No entry in T5U0P for:
                            text-e08
                            tab01-werks
                            tab01-btrtl
                            space
                            space.
          endif.
          append i5u0p.                "Put it into the internal table
        endif.                         "I5U0P eq '*'
      endif.                           "SY-SUBRC gt 0
    endif.                             "TAB01-WERKS eq temps-WERKS...

    if swtch-wrkna ne 'X'.             "If WERKS/BTRTL is in the
      "Tables T001P and T5U0P and
      "Has the right MOLGA

      if tab01-persg ne train-persg or
         tab01-persk ne train-persk.
        move-corresponding tab01       "Persg persk -> trainees?
                           to train.
        collect train.                 "Train will contain all
      endif.                           "Persg/persk combinations@

      if tab01-racky ne rg2ee-racky or
         tab01-gesch ne rg2ee-gesch.
        move-corresponding tab01       "Race/gender -> EEOCL
                           to rg2ee.
        collect rg2ee.                 "RG2EE will contain all
        "Race gender combinations
      endif.

    else.                              "If WERKS/BTRTL is not
      delete tab01.                    "Desired - delete it
    endif.                             "Swtch-wrkna ne 'x'
  endloop.                                                  "At TAB01
endform.                                                    "Loop_TAB01

*---------------------------------------------------------------------*
*       FORM TAB01_TO_TAB02                                           *
*---------------------------------------------------------------------*
*       FILLS RAW DATA FROM TAB01 INTO TAB02, THAT WILL CONTAIN THE   *
*       PURE DATA USED TO ESTABLISH THE MATRIX AND PRINT THE STATISTIC*
*---------------------------------------------------------------------*

form tab01_to_tab02.
  clear: rg2ee,
         train,
         temps,
         tab01.

  sort tab01 by werks
                btrtl
                persg
                persk
                racky
                gesch
                eeoct.
  loop at tab01.

    move-corresponding tab01 to tab02. "Job categories, count

    if tab01-werks ne temps-werks or   "Fill coind and repun into
       tab01-btrtl ne temps-btrtl.                          "TAB02
      move: tab01-werks to temps-werks,
            tab01-btrtl to temps-btrtl.
      perform rei5u0p using            "Fill in coind and repun
                      tab01-werks
                      tab01-btrtl
                      tab02-coind
                      tab02-repun
                      tab02-eeohq.     "WJIP30K147213
    endif.                             "TAB01-WERKS...

    if tab01-racky ne rg2ee-racky or   "Move the EEO class int TAB02
       tab01-gesch ne rg2ee-gesch.
      clear: rg2ee.
      loop at rg2ee where              "Get the EEOCL from RG2EE
              racky eq tab01-racky
              and gesch eq tab01-gesch.
        move rg2ee-eeocl               "We already checked
             to tab02-eeocl.           "That available
      endloop.                                              "At TAB01
    endif.

    collect tab02.

    "Check whether persg/persk combo
    if tab01-persg ne train-persg or   "Is trainee
       tab01-persk ne train-persk.
      clear: train.
      loop at train where
              persg eq tab01-persg and
              persk eq tab01-persk.
      endloop.
    endif.
    if train-train eq 'X'.             "Trainee
      move train-train to tab02-train.
      if const-prodt cs tab01-eeoct.
        move const-proda to            "Production worker trainee
             tab02-eeoct.
      else.
        move const-whita to            "Professional trainee
             tab02-eeoct.
      endif.                           "Const-prodt
      collect tab02.                   "Add the trainees
      clear: tab02-train.              "Clear the trainee indicator
    endif.                             "TRAIN-TRAIN eq 'X'
  endloop.                                                  "At TAB01
endform.                               "TAB01_TO_TAB02

*---------------------------------------------------------------------*
*       FORM SINGLE_OR_MULTI                                          *
*---------------------------------------------------------------------*
*       LOOP OVER TAB02 TO FIND OUT, WILL BE PUT INTO THE PRINT       *
*       RECORD LINE1. (VERY INTRICATE WORK WITH FIELD-SYMBOLS!)       *
*---------------------------------------------------------------------*
form single_or_mulit.
  data: name(35), addr(35), city(25),  "XIYPH9K009820
        state(35), county(10),         "XIYPH9K009820
        temp_coind like t5u0p-coind,   "XIYPH9K009820
        temp_repun like t5u0p-repun,   "XIYPH9K009820
        temp_eeohq.                    "XIYPH9K009820

  clear: tab02,
         coind.

*  SORT tab02 BY coind repun. "XIYPH9K009820
  sort tab02 by coind eeohq repun.     "XIYPH9K009820

  loop at tab02.
    temp_eeohq = tab02-eeohq.          "XIYPH9K009820
    at new coind.
      temp_coind = tab02-coind.        "XIYPH9K009820
      clear: coind.
    endat.

    at new repun.
* delete entries for reporting units located in PR, VI and GU
      temp_repun = tab02-repun.        "XIYPH9K009820
      perform re5uru using temp_repun  "XIYPH9K009820
                           name addr city state county."XIYPH9K009820
      if ( state(2) eq 'PR' ) or       "XIYPH9K009820
         ( state(2) eq 'VI' ) or       "XIYPH9K009820
         ( state(2) eq 'GU' ).         "XIYPH9K009820
        if temp_eeohq = 'X'.           "XIYPH9K009820
          delete tab02 where coind = temp_coind.         "XIYPH9K009820
        else.                          "XIYPH9K009820
          delete tab02 where coind = temp_coind          "XIYPH9K009820
                         and repun = temp_repun.         "XIYPH9K009820
        endif.                         "XIYPH9K009820
      else.                            "XIYPH9K009820
        clear: coind.
        move tab02-coind to coind-coind. "Coind will contain the no
        add 1 to coind-count.          "Of repuns per coind
        collect coind.
      endif.                           "XIYPH9K009820
    endat.
  endloop.                                                  "At TAB02

  clear: tab02,
         coind.

  sort coind by coind.
  loop at coind.
    loop at tab02 where
                   coind eq coind-coind.
      if coind-count gt 1.
        move 'M' to tab02-multi.
      else.
        move 'S' to tab02-multi.
      endif.
      modify tab02.
    endloop.                                                "At TAB02
  endloop.                             "At coind
endform.                               "SINGLE_OR_MULTI

*---------------------------------------------------------------------*
*       FORM PRINT_PAGES                                              *
*---------------------------------------------------------------------*
*       THIS IS THE LOOP WHERE TAB02 WILL BE PUT INTO THE PRINT       *
*       RECORD LINE1. (VERY INTRICATE WORK WITH FIELD-SYMBOLS!)       *
*---------------------------------------------------------------------*
form print_pages.
  data: cellx(11) value 'LINE1-XX'.    "Prepare for field symbol
  data: colxx(11) value 'LINE1-COLM'.  "Assignment
  data: rowxx(11) value 'LINE1-ROW'.
  data: cellc(11) value 'LINEC-XX'.
  data: colxc(11) value 'LINEC-COLM'.
  data: rowxc(11) value 'LINEC-ROW'.
  data: temp(20).                                           "PH9K009820
  field-symbols: <f>,                  "Field   for line1
                 <c>,                  "Column
                 <r>.                                       "row
  field-symbols: <fc>,                 "Field   for linec
                 <cc>,                 "Column
                 <rc>.                                      "row

  clear: tab02.
* SORT: tab02.
* SORT tab02 BY eeohq DESCENDING.      "WJIP30K1472
  sort tab02 by coind eeohq descending multi repun.      "WWQAHRK024351

  loop at tab02.                       "The loop
    at new multi.
      if tab02-multi eq 'S'.           "If its a single est. employer
        perform init_form using        "Create the form from T512P
                          sy-langu
                          const-molga
                          const-forms
                          sy-subrc.
      else.                            "Else multi est. employer
        perform init_form using        "Create the form from T512P
                          sy-langu
                          const-molga
                          const-formm
                          sy-subrc.
      endif.                           "TAB02-multi eq 's'
    endat.

    at new coind.                      "At the beginning of company
      perform fill_hq.                 "Get company info
      new-page.
    endat.

    at new repun.
      new-page.
      clear: line1.
      perform fill_line1.              "Get unit info
      perform fill_previous.           "Get the previous record from
                                                            "T5u0e
      perform fill_current.            "Get EEO parameters from T5u0e
* Company ID format XXXXXX-X                                "PH9K009820
      if not ( line1-coind is initial )                     "
         and ( line1-coind+6(1) ne '-' ).                   "
        temp = line1-coind.                                 "
        move: temp(6) to line1-coind(6),                    "
              '-' to line1-coind+6(1),                      "
              temp+6(1) to line1-coind+7(1).                "
      endif.                                                "
* Reporting unit format XXXXXX-X                            "
      if not ( line1-repun is initial )                     "
      and ( line1-repun+6(1) ne '-' ).                      "
        temp = line1-repun.                                 "
        move: temp(6) to line1-repun(6),                    "
              '-' to line1-repun+6(1),                      "
              temp+6(1) to line1-repun+7(1).                "
      endif.                                                "PH9K009820
    endat.
    "Build field names line1
    move: tab02-eeoct to cellx+8(2),   "Ex. line1-xx02
          tab02-eeoct to rowxx+9(2).
*     MOVE: tab02-eeocl TO cellx+10(1),"Ex. line1-xx02c   XIYPH9K009820
*           tab02-eeocl TO colxx+10(1).                   XIYPH9K009820

    move: tab02-eeoct to cellc+8(2),   "Same for linec
          tab02-eeoct to rowxc+9(2).
*     MOVE: tab02-eeocl TO cellc+10(1),                  XIYPH9K009820
*           tab02-eeocl TO colxc+10(1).                  XIYPH9K009820

    IF tab02-eeocl IS INITIAL or                          "Note 211983
      tab02-eeocl = 'X'.                                  "Note 211983
*      exit.                                               "Note 211983
      continue.                                          "APNL9CK111275
    ENDIF.                                                "Note 211983

* Establishment located in Hawaii only reports in B or G regardless of
* race/ethnic designation.                  XIYPH9K009820
    if line1-run04(2) ne 'HI'.         "Not Hawaii       XIYPH9K009820
      move: tab02-eeocl to cellx+10(1),"Ex. line1-xx02c
            tab02-eeocl to colxx+10(1),
            tab02-eeocl to cellc+10(1),
            tab02-eeocl to colxc+10(1).
    else.                              "Hawaii           XIYPH9K009820
      if tab02-eeocl co 'BCDEF'.       "Male             XIYPH9K009820
        move: 'B' to cellx+10(1),      "fill in column B XIYPH9K009820
              'B' to colxx+10(1),                           "
              'B' to cellc+10(1),                           "
              'B' to colxc+10(1).                           "
      endif.                                                "
      if tab02-eeocl co 'GHIJK'.       "Female           XIYPH9K009820
        move: 'G' to cellx+10(1),      "fill in column G XIYPH9K009820
              'G' to colxx+10(1),                           "
              'G' to cellc+10(1),                           "
              'G' to colxc+10(1).                           "
      endif.                                                "
    endif.                             "XIYPH9K009820

    assign (cellx) to <f>.             "<F> is now field 'line1-xx02c'
    assign (rowxx) to <r>.             "And we can add numbers to it
    assign (colxx) to <c>.

    assign (cellc) to <fc>.
    assign (rowxc) to <rc>.
    assign (colxc) to <cc>.

    add tab02-count to <f>.            "Add to line1
    add tab02-count to <r>.

    add tab02-count to <fc>.           "Add to linec
    add tab02-count to <rc>.

    if tab02-train eq space.           "If not trainee category
      add tab02-count to <c>.
      add tab02-count to line1-total.
      add tab02-count to <cc>.
      add tab02-count to linec-total.
    endif.                             "TAB02-train eq space

    at end of repun.
      new-page.                        "Build the page
      IF RAD2 EQ 'X'.                  " VSDL9CK065654
        PERFORM FILL_FILE.
      ENDIF.                           " VSDL9CK065654
      if line1-repun+0(3) eq 'Z$$'.
        clear line1-repun.
      endif.
      perform prepare_form using
                           temps-subrc
                           temps-fname.
      if temps-subrc gt 0.
        perform err_log using          "Error writing variable
                        text-e10
                        temps-fname
                        space
                        space
                        space.
      endif.                           "Temps-SUBRC gt 0
      if line1-typex eq 4.
        if line1-total gt 49.          "WJIP30K147213  XIYPH9K009820
*...      modify by hj.hwang (2003.10.15) - begin
*         perform print_form.          "Print the page
          perform print_form_change.   "layout change   UD1K902814
*...      modify end
          new-page.
        else.             "list for <50 reporting unit  XIYPH9K009820
          clear tablist.                                    "
          move-corresponding line1 to tablist.              "
          append tablist.                                   "
        endif.                         "WWQAHRK024351   XIYPH9K009820
      else.
*...    modify by hj.hwang (2003.10.15) - begin
*       perform print_form.            "Print the page
        perform print_form_change.     "layout change   UD1K902814
*...    modify end
        new-page.
      endif.
      if pa_updat eq 'X'.              "Default is not updating
        perform upd_current.           "Write results back into T5u0e
      endif.
      clear: line1.
    endat.

    at end of coind.
      if tab02-multi eq 'M'.           "Only print if multi establ.
        new-page.                      "Consolidated report
        move linec to line1.           "Cumulated values
        move-corresponding corpx       "Corp info
                           to line1.   "Corp info
        move corpx-coein to line1-einum.                    "PH9K009820
        unpack sy-pagno to line1-pagex.
* Company ID format XXXXXX-X                                "PH9K009820
        if not ( line1-coind is initial )                   "
           and ( line1-coind+6(1) ne '-' ).                 "
          temp = line1-coind.                               "
          move: temp(6) to line1-coind(6),                  "
                '-' to line1-coind+6(1),                    "
                temp+6(1) to line1-coind+7(1).              "
        endif.                                              "
        select single * from t5u0e where                    "
                             repun eq line1-repun           "
                             and ryear eq pa_cyear          "
                             and rperi eq pa_cperi.         "
        move: t5u0e-sece2 to line1-sece2,                   "
              t5u0e-secc1 to line1-secc1,                   "
              t5u0e-secc2 to line1-secc2,                   "
              t5u0e-secc3 to line1-secc3,                   "
              t5u0e-apind to line1-apind.                   "PH9K009820
        clear: line1-repun,            "Don't need these for
               line1-dandb,            "Consolidated report
               line1-sicno,
               line1-naics.                               "RPGL9CK056416
        move '2' to line1-typex.       "Consolidated report
        move text-rt2 to line1-title.  "Consolidated report
        perform prepare_form using     "Prepare the page
                        temps-subrc
                        temps-fname.
        if temps-subrc gt 0.
          perform err_log using        "Error writing variable
                          text-e10
                          temps-fname
                          space
                          space
                          space.
        endif.                         "Temps-SUBRC gt 0
        perform print_form.            "Print form
        PERFORM PRINT_SECG.  "Print Sec-G for consolidated report.
        clear: line1, linec.
        new-page.
      endif.                           "TAB02-multi ...
    endat.
  endloop.
endform.

*---------------------------------------------------------------------*
*       FORM FILL_HQ                                                  *
*---------------------------------------------------------------------*
*       AT NEW COIND, THE COMPANY INFORMATION WILL BE LOADED INTO     *
*       THE COLLECTION RECORD.                                        *
*---------------------------------------------------------------------*

form fill_hq.
  clear: corpx.
  loop at i5u0p where                  "Find the unit that
                coind eq tab02-coind   "Has the company id and
                and eeohq eq 'X'.      "Headquarter indicatord
    exit.
  endloop.                                                  "At I5U0P
  if sy-subrc <> 0.
    select single * from t5u0p into i5u0p where
                  coind eq tab02-coind   "Has the company id and
                  and eeohq eq 'X'.      "Headquarter indicatord
  endif.
  if sy-subrc gt 0                     "Not found but single est.emp
  and tab02-multi eq 'S'.
    loop at i5u0p where
                  coind eq tab02-coind.
      exit.
    endloop.                                                "At I5U0P
  endif.                               "SY-SUBRC gt 0 ...
* MOVE: i5u0p-einum TO corpx-coein,    "Save corp information PH9K009820
  move: i5u0p-einum to corpx-coein.    "Save corp information PH9K009820
*       T5U0P-SICNO TO CORPX-SICNO,                          "VQSK098667
*       T5U0P-DANDB TO CORPX-DANDB,                               "
*       T5U0P-COIND TO CORPX-COIND,                               "
*       T5U0P-REPUN TO CORPX-REPUN.                               "
  move-corresponding i5u0p to corpx.                        "PH9K009820
* DUNS number format XX-XXX-XXXX                            "
  if not ( i5u0p-dandb is initial ).                        "
    move: i5u0p-dandb(2) to corpx-dandb(2),                 "
          '-' to corpx-dandb+2(1),                          "
          i5u0p-dandb+2(3) to corpx-dandb+3(3),             "
          '-' to corpx-dandb+6(1),                          "
          i5u0p-dandb+5(4) to corpx-dandb+7(4).             "
  endif.                                                    "PH9K009820

*  perform re536a using const-anart     "Get the address WJIP30K147213
*                       i5u0p-werks
*                      i5u0p-btrtl
*                      corpx-col01
*                      corpx-col02
*                      corpx-col03
*                      corpx-col04.

  perform re5uru using   i5u0p-repun   "Get the address WJIP30K147213
                      corpx-col01
                      corpx-col02
                      corpx-col03
*                     corpx-col04.                       "PH9K009820
                      corpx-col04                           "PH9K009820
                      corpx-col05.                          "PH9K009820

  move pn-endda(4) to corpx-yearx.     "Fill in date/year fields
  write pn-begda to
*                corpx-begda MM/DD/YY.                      "PH9K009820
                 corpx-begda mm/dd/yyyy.                    "PH9K009820
  write pn-endda to
*                corpx-endda MM/DD/YY.                      "PH9K009820
                 corpx-endda mm/dd/yyyy.                    "PH9K009820
endform.                               "FILL_HQ

*---------------------------------------------------------------------*
*       FORM FILL_LINE1                                               *
*---------------------------------------------------------------------*
*       FILLS IN THE REPORTING UNIT INFORMATION AT NEW REPUN.         *
*---------------------------------------------------------------------*

form fill_line1.
  move-corresponding corpx to line1.   "Fill corp. infor
  if tab02-multi eq 'M'.               "When mulit est. employer
    unpack sy-pagno to line1-pagex.    "We need page numbers
    loop at i5u0p where                "Get unit info
            coind eq tab02-coind and
            repun eq tab02-repun.
      move-corresponding i5u0p         "Einum, sicno, naics, dandb etc.
                         to line1.
* EINMU SICNO DANDB overidden at the establishment level     PH9K009820
      if line1-einum is initial.                            "
        move corpx-coein to line1-einum.                    "
      endif.                                                "
      if line1-sicno is initial.                            "
        move corpx-sicno to line1-sicno.                    "
      endif.
      if line1-naics is initial.                          "RPGL9CK056416
        move corpx-naics to line1-naics.                  "RPGL9CK056416
      endif.                                              "RPGL9CK056416

      if line1-dandb is initial.                            "
        move corpx-dandb to line1-dandb.                    "
      else.                            "DANDB as format XX-XXX-XXXX
        move: i5u0p-dandb(2) to line1-dandb(2),             "
              '-' to line1-dandb+2(1),                      "
              i5u0p-dandb+2(3) to line1-dandb+3(3),         "
              '-' to line1-dandb+6(1),                      "
              i5u0p-dandb+5(4) to line1-dandb+7(4).         "
      endif.                           "XIYPH9K009820
      if i5u0p-eeohq eq space.         "Not headquarters
        move '4' to line1-typex.       "Individual establishment report
        move text-rt4 to line1-title.  "Individual establishment rep.

*  perform re536a using const-anart     "Get the address WJIP30K147213
*                       i5u0p-werks
*                       i5u0p-btrtl
*                       line1-run01
*                       line1-run02
*                       line1-run03
*                       line1-run04.
        perform re5uru using           "Get address WJIP30K147213
                        i5u0p-repun
                        line1-run01
                        line1-run02
                        line1-run03
*                       line1-run04.                "PH9K009820
                        line1-run04                         "PH9K009820
                        line1-run05.                        "PH9K009820
      else.
        move '3' to line1-typex.       "Headquarters
        move text-rt3 to line1-title.  "Headquarters rep.
        move: corpx-col01 to line1-run01,                   "WJI
        corpx-col02 to line1-run02,
        corpx-col03 to line1-run03,
        corpx-col04 to line1-run04,
        corpx-col05 to line1-run05,                         "PH9K009820
        corpx-col01 to linec-run01,                         "WJI
        corpx-col02 to linec-run02,
        corpx-col03 to linec-run03,
        corpx-col05 to linec-run05,                         "PH9K009820
        corpx-col04 to linec-run04.

*       CLEAR: LINE1-EINUM.            "XIYPH9K009820
      endif.                           "I5U0P-hqind
    endloop.
  else.
    move '1' to line1-typex.           "Single establishment emp.
    move text-rt1 to line1-title.      "Single establishm. emp. rep.
    perform re5uru using               "Get address
                    tab02-repun
                    line1-run01
                    line1-run02
                    line1-run03
                    line1-run04
                    line1-run05.
    move:  corpx-coein to line1-einum. " Note:632291

  endif.                               "TAB02-multi eq x
  if pa_rtype eq 'X'.
    move '5' to line1-typex.           "Special report
    move text-rt5 to line1-title.      "Special report
  endif.                               "Pa_rtype eq 's'

***move certifying official details.   "VSDL9CK065654
  move: name to line1-cname,
        title to line1-ctitl.         "VSDL9CK065654

endform.                               "FILL_LINE1.

*---------------------------------------------------------------------*
*       FORM FILL_PREVIOUS                                            *
*---------------------------------------------------------------------*
*       READS THE PREVIOUS RECORD FROM T5U0E AND MOVES IT TO THE      *
*       INDIVIDUAL AND CONSOLIDATED RECORDS.                          *
*---------------------------------------------------------------------*

form fill_previous.
  clear: swtch-previ.
  select single * from t5u0e where
                repun eq tab02-repun
                and ryear eq pa_ryear
                and rperi eq pa_rperi.
  if sy-subrc eq 0.
    move 'X' to swtch-previ.           "Previous record found
    move-corresponding t5u0e to line1. "Fill the switches in case we
    "don't find a current record
    add: t5u0e-secd1 to line1-ptotl,   "Total
         t5u0e-secd1 to linec-ptotl.

    add: t5u0e-secd2 to line1-pcolb,   "Cat b
         t5u0e-secd2 to linec-pcolb.

    add: t5u0e-secd3 to line1-pcolc,   "Cat c
         t5u0e-secd3 to linec-pcolc.

    add: t5u0e-secd4 to line1-pcold,   "Cat d
         t5u0e-secd4 to linec-pcold.

    add: t5u0e-secd5 to line1-pcole,   "Cat e
         t5u0e-secd5 to linec-pcole.

    add: t5u0e-secd6 to line1-pcolf,   "Cat f
         t5u0e-secd6 to linec-pcolf.

    add: t5u0e-secd7 to line1-pcolg,   "Cat g
         t5u0e-secd7 to linec-pcolg.

    add: t5u0e-secd8 to line1-pcolh,   "Cat h
         t5u0e-secd8 to linec-pcolh.

    add: t5u0e-secd9 to line1-pcoli,   "Cat i
         t5u0e-secd9 to linec-pcoli.

    add: t5u0e-secda to line1-pcolj,   "Cat j
         t5u0e-secda to linec-pcolj.

    add: t5u0e-secdb to line1-pcolk,   "Cat k
         t5u0e-secdb to linec-pcolk.
  else.
    perform err_log using text-e07     "Record not found
                          'T5U0E'
                          tab02-repun
                          pa_ryear
                          pa_rperi.
  endif.                               "SY-SUBRC eq 0
endform.                               "FILL_PREVIOUS.

*---------------------------------------------------------------------*
*       FORM FILL_CURRENT                                             *
*---------------------------------------------------------------------*
*       GETS THE RECORD FOR THE CURRENT PERIOD THAT THE USER HAS      *
*       PREPARED AND READS IN THE SWITCHES/INDICATORS THAT HAVE BEEN  *
*       SET.                                                          *
*---------------------------------------------------------------------*

form fill_current.
  select single * from t5u0e where     "Select current rec from T5u0e
                repun eq tab02-repun
                and ryear eq pa_cyear
                and rperi eq pa_cperi.
  if sy-subrc eq 0.                    "If its there
    move-corresponding t5u0e to line1. "Move it to line1
  else.
    if swtch-previ eq 'X'.             "If previous record found
      perform err_log using text-e11   "No current record used previous
                             'T5U0E'
                             tab02-repun
                             pa_cyear
                             pa_cperi.
      move-corresponding t5u0e to line1.
    else.
      perform err_log using text-e07   "Record not found
                            'T5U0E'
                            tab02-repun
                            pa_cyear
                            pa_cperi.
    endif.                             "Swtch-preiv eq 'x'
  endif.                               "SY-SUBRC eq 0
endform.                               "GET_CURRENT

*---------------------------------------------------------------------*
*       FORM UPD_CURRENT                                              *
*---------------------------------------------------------------------*
*       UPDATES THE RECORD THAT CONTAINS THE SWITCHES THAT THE USER   *
*       HAS SET WITH THE CURRENT TOTALS PER REPORTING UNIT.           *
*---------------------------------------------------------------------*

form upd_current.
  move-corresponding line1 to t5u0e.                        "PH9K009820
  move: tab02-repun to t5u0e-repun,
      pa_cyear    to t5u0e-ryear,
      pa_cperi    to t5u0e-rperi.
* MOVE-CORRESPONDING line1 TO t5u0e.                        "PH9K009820
  move: line1-total to t5u0e-secd1,    "TOTAL
        line1-colmb to t5u0e-secd2,    "Cat B
        line1-colmc to t5u0e-secd3,    "Cat C
        line1-colmd to t5u0e-secd4,    "Cat D
        line1-colme to t5u0e-secd5,    "Cat E
        line1-colmf to t5u0e-secd6,    "Cat F
        line1-colmg to t5u0e-secd7,    "Cat G
        line1-colmh to t5u0e-secd8,    "Cat H
        line1-colmi to t5u0e-secd9,    "Cat I
        line1-colmj to t5u0e-secda,    "Cat J
        line1-colmk to t5u0e-secdb.    "Cat K
  modify t5u0e.
  if sy-subrc gt 0.
    perform err_log using text-e09     "Error updating T5u0e
                          tab02-repun
                          pa_cyear
                          pa_cperi
                          space.
  endif.                               "SY-SUBRC gt 0
endform.                               "FILL_PREVIOUS.

*---------------------------------------------------------------------*
*       FORM REI505R                                                  *
*---------------------------------------------------------------------*
*       CHECKS VALIDITY OF P0077-RACKY                                *
*---------------------------------------------------------------------*
*  -->  RACKY  = ETHNIC ORIGIN CODE                                   *
*  <--  SUBRC  = RETURN CODE                                          *
*---------------------------------------------------------------------*

form rei505r using racky subrc.
  loop at i505r where racky eq racky.
    exit.
  endloop.                                                  "At I505R
  move sy-subrc to subrc.
endform.                                                    "REI505R

*---------------------------------------------------------------------*
*       FORM RE5U0P                                                   *
*---------------------------------------------------------------------*
*       READS TABLE T5U0P AND RETURNS A RECORD FOR INCLUDING IN I5UOP *
*---------------------------------------------------------------------*
*  -->  WERKS = WERKS                                                 *
*  -->  BTRTL = BTRTL                                                 *
*  <--  I5U0P = RECORD FOR TRANSFER TO INTERNAL I5U0P                 *
*---------------------------------------------------------------------*

form re5u0p using werks btrtl i5u0p.
  select single * from t5u0p where     "Read it
                  werks eq werks and
                  btrtl eq btrtl.
  if sy-subrc gt 0.                    "If not found, indicate it
    move '*' to i5u0p.
  else.
    move t5u0p to i5u0p.
  endif.
endform.                                                    "RE5U0P

*---------------------------------------------------------------------*
*       FORM RE5U13                                                   *
*---------------------------------------------------------------------*
*       READS TABLE T5U13 AND RETURNS THE EEO CATEGORY OF A JOB       *
*---------------------------------------------------------------------*
*  -->  STELL = JOB                                                   *
*  -->  ENDDA = DATE                                                  *
*  <--  EEOCT = EEO CATEGORY                                          *
*---------------------------------------------------------------------*

form re5u13 using stell endda eeoct.
  select * from t5u13 where
                stell eq stell
                and endda ge endda
                and begda le endda.
    move t5u13-eeoct to eeoct.         "Fill the return value
    exit.
  endselect.
  if sy-subrc gt 0.                    "Indicate the error
    move '*' to eeoct.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM FILL_TRAIN                                               *
*---------------------------------------------------------------------*
*       LOOPS AT INTERNAL TABLE TRAIN, CONTAINING PERSONGROUP/SUBGROUP*
*       AND READS TABLE T503 TO CHECK WHETHER THE PERSONGROUP/SUBGROUP*
*       INDICATES TRAINEES.                                           *
*---------------------------------------------------------------------*

form fill_train.
  clear: train.
  loop at train.                       "READ TABLE T503
    perform re503 using train-persg
                        train-persk
                        train-train.
    if train-train eq '*'.             "If not found
      perform err_log using text-e04   "Invalid persg/persk combo
                            train-persg
                            train-persk
                            space
                            space.

    endif.
    modify train.                      "Write it back
  endloop.                             "At TRAIN
endform.                               "FILL_TRAIN

*---------------------------------------------------------------------*
*       FORM RE503                                                    *
*---------------------------------------------------------------------*
*       READS TABLE T503 AND AND RETURNS 'X' IF AUSTA INDICATES       *
*       TRAINEE.                                                      *
*---------------------------------------------------------------------*
*  -->  PERSG   =  PERSONGROUP                                        *
*  -->  PERSK   =  PERSONSUBGROUP                                     *
*  <--  TRAIN   =  'X' IF TRAINEE, SPACE IF NOT                       *
*---------------------------------------------------------------------*

form re503 using persg persk train.
  select single * from t503 where
                  persg eq persg and
                  persk eq persk.
  if sy-subrc eq 0.
    if t503-austa eq const-train.      "When trainee
      move 'X' to train.
    endif.
  else.
    move '*' to train.
  endif.                               "SY-SUBRC eq 0
endform.                                                    "RE503

*---------------------------------------------------------------------*
*       FORM FILL_RG2EE                                               *
*---------------------------------------------------------------------*
*       LOOPS OVER TABLE RG2EE AND FINDS THE EEO EMPLOYEE CLASSES     *
*       BASED ON ETHNIC ORIGIN AND GENDER, USING THE FEATURE EEOCL    *
*---------------------------------------------------------------------*

form fill_rg2ee.
  clear: rg2ee.
  loop at rg2ee.
    clear: temps-subrc.
    move-corresponding rg2ee to pme25. "PME25 is structure for
    "Feature EEOCL
    "The standard routine for
    "Reading decision trees
    "Contained in incl
                                                            "RPUMKC00
    perform re549d using 'EEOCL'       "Use the feature
                         ' '
                         rg2ee-eeocl
                         temps-subrc.

    if rg2ee-eeocl eq 'X' or           "If not found
       temps-subrc gt 0.
      perform err_log using            "Feature EEOCL: no entry for:
                      text-e05
                      rg2ee-gesch
                      rg2ee-racky
                      space
                      space.
    else.
      modify rg2ee.
    endif.
  endloop.
endform.                                                    "FILL_RG2EE

*---------------------------------------------------------------------*
*       FORM REI5U0P                                                  *
*---------------------------------------------------------------------*
*       READS INTERNAL TABLE I5U0P AND RETURNS COIND REPUN FOR        *
*       WERKS AND BTRTL                                               *
*---------------------------------------------------------------------*
*  -->  WERKS   =  WERKS                                              *
*  -->  BTRTL   =  BTRTL                                              *
*  <--  COIND   =  COIND                                              *
*  <--  REPUN   =  REPUN                                              *
*---------------------------------------------------------------------*

form rei5u0p using werks btrtl coind repun eeohq.
  clear: i5u0p.
  loop at i5u0p where
                mandt eq sy-mandt and
                werks eq werks and
                btrtl eq btrtl.
  endloop.
  move i5u0p-coind to coind.           "Return it
  move i5u0p-repun to repun.
  move i5u0p-eeohq to eeohq.
endform.                                                    "REI5U0P

*---------------------------------------------------------------------*
*       FORM ERR_CHK1                                                 *
*---------------------------------------------------------------------*
*       SPECIAL ERROR ROUTINE FOR THE PROVIDES WITHIN THE GET (PERNR) *
*       LOOP.                                                         *
*---------------------------------------------------------------------*
*  -->  INFTY   = INFOTYPE (Pxxxx-INFTY)                              *
*  -->  ERRTX   = INFOTYPE TEXT ('XXXX')                              *
*  -->  PERNR   = PERSONNEL NUMBER                                    *
*---------------------------------------------------------------------*

form err_chk1 using infty errtx pernr.
  if infty eq space.
    perform err_log using text-e06     "Infotype not available
                          pernr
                          errtx
                          space
                          space.
    move 'X' to temps-rejec.           "Mark for rejection
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM RE536A                                                   *
*---------------------------------------------------------------------*
*       READ ADDRESS FROM TABLE T536A                                 *
*---------------------------------------------------------------------*
*  -->  ANART  = ADDRESS PARAMETER                                    *
*  -->  WERKS  = PLANT                                                *
*  -->  BTRTL  = PLANT SECTION                                        *
*  -->  TYPEA  = FIRST LINE                                           *
*  -->  TYPEB  = SECOND LINE                                          *
*  -->  TYPEC  = THIRD LINE                                           *
*  -->  TYPED  = FOURTH LINE                                          *
*---------------------------------------------------------------------*

*form re536a using anart werks btrtl typea typeb typec typed.
*  data: vargu like t536a-vargu.        "Build the argument
*  move: werks to vargu+0(4),
*        btrtl to vargu+4(4).
*  select * from t536a where
*                anart eq anart and
*                vargu eq vargu.
*    case t536a-seqnu.
*      when 'A'.
*        move t536a-antxt to typea.
*      when 'B'.
*        move t536a-antxt to typeb.
*      when 'C'.
*        move t536a-antxt to typec.
*      when 'D'.
*       move t536a-antxt to typed.
*     when 'M'.
*       move t536a-antxt to typem.
*    endcase.
*  endselect.
*endform.                               "RE536A

***********************************************************************
*                       SUBROUTINE INCLUDES                           *
***********************************************************************

INCLUDE ZPSFRMUS.
*include rpsfrmus.                      "Include routines for forms
INCLUDE ZPSERRUS.
*include rpserrus.                      "Include routines for errors
INCLUDE ZPUMKC00.
*include rpumkc00.                      "Include programs for reatures

************************* END OF REPORT *******************************
*&---------------------------------------------------------------------*
*&      Form  RE5URU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CORPX-COL02  text                                          *
*----------------------------------------------------------------------*
*FORM re5uru USING   repun type1 type2 type3 type4.     "PH9K009820
form re5uru using   repun type1 type2 type3 type4 type5.    "PH9K009820
* data:     type1(35),                  "Company name
*           type2(35),                  "Company addr.
*          type3(25),                  "Company city
*          type4(35).                  "Company state/zip
*          type5(10)                   "Company county

  clear: type1, type2, type3, type4.
  clear: type5.                        ""PH9K009820

  select single       * from  t5uru
         where  repin       = 'E'
         and    repun       = repun   .
  if sy-subrc <> 0.
    if repun is initial.
      perform err_log using text-er4   "Invalid repun in T5URU
                            'FOR PA:'
                            i5u0p-werks
                            'PSA:'
                            i5u0p-btrtl.
    else.
      perform err_log using text-er4   "Invalid repun in T5URU
                            'FOR'
                            'UNIT NUMBER'
                            repun
                            space.
    endif.
  else.

    select single       * from  t5uad
           where  addno       = t5uru-addno  .

    if sy-subrc <> 0.
      perform err_log using text-er5   "Invalid Address Number in T5UAD
                             'FOR'
                             'UNIT NUMBER'
                             repun
                             space.
    endif.
    move t5uad-reptx to type1.
    move t5uad-stras to type2.
*   MOVE t5uad-ort01 TO type3+0(25).           "PH9K009820
    move t5uad-ort01 to type3.                              "PH9K009820
*   MOVE ',' TO type3+24(1).                   "PH9K009820
*   MOVE t5uad-ort04 TO type3+25(10).          "PH9K009820
    move t5uad-ort04 to type5.                              "PH9K009820
    move t5uad-state to type4+0(3).
    move t5uad-zipcd to type4+5(10).
*   CONDENSE type3.                                         "PH9K009820
    clear: t5uru, t5uad.
  endif.

endform.                                                    " RE5URU
*&---------------------------------------------------------------------*
*&      Form  print_tablist                "XIYPH9K009820
*&---------------------------------------------------------------------*
* Print the list of establishment which has less than 50 employees.
*----------------------------------------------------------------------*
form print_tablist.                    "XIYPH9K009820
  data: line type i.
  describe table tablist lines line.
  if line gt 0.
    new-page.
    write: / text-e16.
    format color col_heading.
    write: /(130) sy-uline.
    write: / sy-vline no-gap, 'Name'(e17),
           36 sy-vline no-gap, 'Address'(e18),
           72 sy-vline no-gap, 'Total employees'(e19),
           88 sy-vline no-gap, 'Major activity'(e20),
           130 sy-vline no-gap.
    format color off.
    loop at tablist.
      write: /(130) sy-uline.
      write: / sy-vline no-gap, tablist-run01,
             36 sy-vline no-gap, tablist-run02,
             72 sy-vline no-gap, tablist-total,
             88 sy-vline no-gap, tablist-sece3,
             130 sy-vline no-gap,
             / sy-vline no-gap,
             36 sy-vline no-gap, tablist-run03,
             72 sy-vline no-gap,
             88 sy-vline no-gap, tablist-sece4,
             130 sy-vline no-gap,
             / sy-vline no-gap,
*            36 sy-vline NO-GAP, tablist-run05,             "note 188107
             36 sy-vline no-gap, tablist-run04,             "note 188107
             72 sy-vline no-gap,
             88 sy-vline no-gap, tablist-sece5,
             130 sy-vline no-gap,
             / sy-vline no-gap,
*            36 sy-vline NO-GAP, tablist-run04,             "note 188107
             36 sy-vline no-gap,                            "note 188107
             72 sy-vline no-gap,
             88 sy-vline no-gap, tablist-sece6,
             130 sy-vline no-gap.
    endloop.
    write: /(130) sy-uline.
  endif.
endform.                               " print_tablist

*&---------------------------------------------------------------------*
*&      Form  FILL_FILE (Electronic file download)
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_FILE.
  DATA : FIELD1(40) TYPE C,
       FIELD2(35) TYPE C,
       FIELD3     TYPE I,
       FLEN      TYPE I,    "field length
       TLEN      TYPE I VALUE 0.    "incremental length of the string
*      Company number
  MOVE LINE1-COIND TO FIELD1.            "VSDL9BK063900
  PERFORM CONVERT USING FIELD1 FIELD1.   "VSDL9BK063900
  PERFORM INSERT_FIELD USING FIELD1 7 CHANGING TLEN.   "VSDL9BK063900
  If tab02-multi eq 'S'.  "Single Establishment (EEO-1)"VSDL9BK063900
*      Status code
    PERFORM INSERT_FIELD USING LINE1-TYPEX 1 CHANGING TLEN.
*      Unit number
    FIELD1 = LINE1-REPUN.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 7 CHANGING TLEN.
*      Unit name
    MOVE LINE1-RUN01 TO FIELD1.
    TRANSLATE FIELD1 TO UPPER CASE.
    PERFORM CHECK_NAME USING FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING LINE1-RUN01   35 CHANGING TLEN.
*      Unit address
    MOVE LINE1-RUN02 TO FIELD1.
    PERFORM CHECK_NAME USING FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 59 CHANGING TLEN.
*      field length is sent as 59 includes(est.addr(34) + extended est.
**     addr(25)).
*      City name
    MOVE LINE1-RUN03 TO FIELD1.
    PERFORM CHECK_NAME USING FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 20 CHANGING TLEN.
*      state code.
    MOVE LINE1-RUN04(2) TO FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1   2 CHANGING TLEN.
*      zip code.
    MOVE LINE1-RUN04+5(5) TO FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1   5 CHANGING TLEN.
*      +4 zip code.
    MOVE LINE1-RUN04+11(4) TO FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1   4 CHANGING TLEN.
*      B 2.c
    PERFORM INSERT_FIELD USING LINE1-SECE2 1 CHANGING TLEN.
*      EI NUM
    PERFORM INSERT_FIELD USING LINE1-EINUM 9 CHANGING TLEN.
*      c.1
    PERFORM INSERT_FIELD USING LINE1-SECC1 1 CHANGING TLEN.
*      c.2
    PERFORM INSERT_FIELD USING LINE1-SECC2 1 CHANGING TLEN.
*      c.3
    PERFORM INSERT_FIELD USING LINE1-SECC3 1 CHANGING TLEN.
*      dun&bradstreet no.
    FIELD1 = LINE1-DANDB.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 9 CHANGING TLEN.
*      county name
    MOVE LINE1-RUN05(18) TO FIELD1.
    PERFORM CHECK_NAME USING FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 18 CHANGING TLEN.
*      D.1 ?
    MOVE LINE1-BEGDA TO FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    MOVE LINE1-ENDDA TO FIELD2.
    PERFORM CONVERT USING FIELD2 FIELD2.
    PERFORM FORMAT USING FIELD1 FIELD2.
    PERFORM INSERT_FIELD USING FIELD1 16 CHANGING TLEN.
*      D.2 ?
    PERFORM INSERT_FIELD USING LINE1-APIND 1 CHANGING TLEN.
*      Sic code.
    MOVE LINE1-SICNO TO FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 4 CHANGING TLEN.
*      NAICS code ???
    MOVE LINE1-NAICS TO FIELD1.
    PERFORM CONVERT_NAICS USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 6 CHANGING TLEN.
*      Title of certifying official ???
    MOVE TITLE TO FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 35 CHANGING TLEN.
*      Name of certifying official ????
    MOVE NAME TO FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 35 CHANGING TLEN.
*      Tel. No. ???
    MOVE TELNO TO FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 10 CHANGING TLEN.
*      Fax  No. ???
    MOVE FAXNO TO FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 10 CHANGING TLEN.
*      E-mail of cert. official ???
    MOVE EMAIL TO FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 40 CHANGING TLEN.
*      Matrix....
**        Line 1.
    MOVE LINE1-ROW01 TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-XX01B NEXT LINE1-XX01C.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
**        Line 2.
    MOVE LINE1-ROW02 TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-XX02B NEXT LINE1-XX02C.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
**        Line 3.
    MOVE LINE1-ROW03 TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-XX03B NEXT LINE1-XX03C.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
**        Line 4.
    MOVE LINE1-ROW04 TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-XX04B NEXT LINE1-XX04C.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
**        Line 5.
    MOVE LINE1-ROW05 TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-XX05B NEXT LINE1-XX05C.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
**        Line 6.
    MOVE LINE1-ROW06 TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-XX06B NEXT LINE1-XX06C.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
**        Line 7.
    MOVE LINE1-ROW07 TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-XX07B NEXT LINE1-XX07C.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
**        Line 8.
    MOVE LINE1-ROW08 TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-XX08B NEXT LINE1-XX08C.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
**        Line 9.
    MOVE LINE1-ROW09 TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-XX09B NEXT LINE1-XX09C.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
**        Line 10.
    MOVE LINE1-TOTAL TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-COLMB NEXT LINE1-COLMC.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
**        Line 11.
    MOVE LINE1-PTOTL TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 7 CHANGING TLEN.
    DO 10 TIMES VARYING FIELD3 FROM LINE1-PCOLB NEXT LINE1-PCOLC.
      PERFORM INSERT_FIELD USING FIELD3 6 CHANGING TLEN.
    ENDDO.
  else.   "Multi Establishment (EEO-2)   "VSDL9BK063900
*      Parent Company name
    MOVE LINE1-COL01 TO FIELD1.
    TRANSLATE FIELD1 TO UPPER CASE.
    PERFORM CHECK_NAME USING FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING LINE1-col01 35 CHANGING TLEN.

*      Establishment name
    MOVE LINE1-RUN01 TO FIELD1.
    TRANSLATE FIELD1 TO UPPER CASE.
    PERFORM CHECK_NAME USING FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING LINE1-RUN01 30 CHANGING TLEN.
*      Address
    MOVE LINE1-RUN02 TO FIELD1.
    PERFORM CHECK_NAME USING FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 34 CHANGING TLEN.
*      City name
    MOVE LINE1-RUN03 TO FIELD1.
    PERFORM CHECK_NAME USING FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1 20 CHANGING TLEN.
*      state code.
    MOVE LINE1-RUN04(2) TO FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1   2 CHANGING TLEN.
*      zip code.
    MOVE LINE1-RUN04+5(5) TO FIELD1.
    PERFORM CONVERT USING FIELD1 FIELD1.
    PERFORM INSERT_FIELD USING FIELD1   5 CHANGING TLEN.
**     Total Employment this location.
    MOVE LINE1-TOTAL TO FIELD3.
    PERFORM INSERT_FIELD USING FIELD3 2 CHANGING TLEN.
*      Description of major business activity at this location
    MOVE LINE1-SECE3 TO FIELD1.
    TRANSLATE FIELD1 TO UPPER CASE.
    PERFORM INSERT_FIELD USING FIELD1 40 CHANGING TLEN.

    MOVE LINE1-SECE4 TO FIELD1.
    TRANSLATE FIELD1 TO UPPER CASE.
    PERFORM INSERT_FIELD USING FIELD1 40 CHANGING TLEN.

    MOVE LINE1-SECE5 TO FIELD1.
    TRANSLATE FIELD1 TO UPPER CASE.
    PERFORM INSERT_FIELD USING FIELD1 20 CHANGING TLEN.
  ENDIF.  "TAB02-MULTI.  VSDL9BK063900



  IF NOT FILE IS INITIAL.
    APPEND FILE.
  ENDIF.

  CALL FUNCTION 'WS_DOWNLOAD'
      EXPORTING
*         BIN_FILESIZE            = ' '
*         CODEPAGE                = ' '
            FILENAME                = FILENAME
*         filetype                = 'ASC'
*         MODE                    = ' '
*         WK1_N_FORMAT            = ' '
*         WK1_N_SIZE              = ' '
*         WK1_T_FORMAT            = ' '
*         WK1_T_SIZE              = ' '
*         COL_SELECT              = ' '
*         COL_SELECTMASK          = ' '
*         NO_AUTH_CHECK           = ' '
*    IMPORTING
*         FILELENGTH              =
       TABLES
            DATA_TAB                = FILE
*         FIELDNAMES              =
       EXCEPTIONS
            FILE_OPEN_ERROR         = 1
            FILE_WRITE_ERROR        = 2
            INVALID_FILESIZE        = 3
            INVALID_TABLE_WIDTH     = 4
            INVALID_TYPE            = 5
            NO_BATCH                = 6
            UNKNOWN_ERROR           = 7
            GUI_REFUSE_FILETRANSFER = 8
            OTHERS                  = 9.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  clear file.               "VSDL9BK063900
ENDFORM.                    " FILL_FILE

*---------------------------------------------------------------------*
*       FORM CONVERT                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  C_PAR                                                         *
*  -->  N_PAR                                                         *
*---------------------------------------------------------------------*
FORM CONVERT USING C_PAR N_PAR.
  DATA: TEM_PAR(35) TYPE C,
        POS TYPE I VALUE 0,
        LEN TYPE I,
        ALSTR(37) TYPE C VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ '.
  DESCRIBE FIELD C_PAR LENGTH LEN.
  TEM_PAR = C_PAR.
  TRANSLATE TEM_PAR TO UPPER CASE.
  CLEAR N_PAR.
  DO LEN TIMES.
    IF NOT ( TEM_PAR IS INITIAL ).
      IF TEM_PAR(1) CA ALSTR.
        N_PAR+POS(1) = TEM_PAR(1).
        ADD 1 TO POS.
      ENDIF.
      SHIFT TEM_PAR LEFT.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INSERT_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_field  text
*      -->P_FINAL  text
*      <--P_LEN  text
*----------------------------------------------------------------------*
FORM INSERT_FIELD USING    VALUE(P_FIELD)
                           VALUE(F_LEN)
                  CHANGING P_LEN.
  DATA: S_FIELD(35) TYPE C.
  MOVE P_FIELD TO S_FIELD.
  CONDENSE S_FIELD.
  IF NOT ( S_FIELD IS INITIAL ).
    IF P_LEN EQ 0.
      FILE-DAT(F_LEN) =  S_FIELD.
    ELSE.
      FILE-DAT+P_LEN(F_LEN) = S_FIELD.
    ENDIF.
  ENDIF.
  P_LEN = P_LEN + F_LEN.
ENDFORM.                    " INSERT_FIELD

*&---------------------------------------------------------------------*
*&      Form  CHECK_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_field  text
*----------------------------------------------------------------------*
FORM CHECK_NAME USING    P_FIELD.
  DATA : NUMSTR(10) VALUE '0123456789',
         LEN TYPE I,
         S_FIELD(35) TYPE C.
  MOVE P_FIELD TO S_FIELD.
  TRANSLATE S_FIELD TO UPPER CASE.
  CONDENSE S_FIELD.
  IF S_FIELD(3) = 'THE'.
    SHIFT S_FIELD BY 4 PLACES.
    LEN = STRLEN( S_FIELD ).
    LEN = LEN + 2.
    S_FIELD+LEN(1) = ' THE'.
  ENDIF.
  IF S_FIELD(1) CA NUMSTR.
    S_FIELD(1) = 'X'.
  ENDIF.
  MOVE S_FIELD TO P_FIELD.
ENDFORM.                    " CHECK_NAME

*&---------------------------------------------------------------------*
*&      Form  FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD1  text
*      -->P_FIELD2  text
*----------------------------------------------------------------------*
FORM FORMAT USING    P_FIELD1
                     P_FIELD2.
  DATA :  DD(2) TYPE C,
          MM(2) TYPE C,
          YYYY(4) TYPE C,
          PFIELD(16) TYPE C.
  MOVE P_FIELD1(2) TO  DD.
  MOVE P_FIELD1+2(2) TO MM.
  MOVE P_FIELD1+4(4) TO YYYY.
  CONCATENATE YYYY MM DD INTO PFIELD.

  MOVE P_FIELD2(2) TO  DD.
  MOVE P_FIELD2+2(2) TO MM.
  MOVE P_FIELD2+4(4) TO YYYY.

  CONCATENATE PFIELD YYYY MM DD INTO PFIELD.

  MOVE PFIELD TO P_FIELD1.

ENDFORM.                    " FORMAT
*&---------------------------------------------------------------------*
*&      Form  CONVERT_NAICS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD1  text
*      -->P_FIELD1  text
*----------------------------------------------------------------------*
form convert_naics using    c_par
                            n_par.
  data: tem_par(35) type c,
        pos type i value 0,
        len type i,
        alstr(37) type c value '0123456789 '.
  describe field c_par length len.
  tem_par = c_par.
  translate tem_par to upper case.
  clear n_par.
  do len times.
    if not ( tem_par is initial ).
      if tem_par(1) ca alstr.
        n_par+pos(1) = tem_par(1).
        add 1 to pos.
      endif.
      shift tem_par left.
    else.
      exit.
    endif.
  enddo.
ENDFORM.                    " CONVERT_NAICS
******************************** VSDL4DK071983
*&---------------------------------------------------------------------*
*&      Form  PRINT_SECG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_SECG.
  WRITE AT 1(25) TEXT-001.
  WRITE:/.
  WRITE AT /1(132)  TEXT-002.
  WRITE AT /1(90) TEXT-003.
  WRITE:/.
  WRITE AT /1(21) TEXT-004.
  WRITE AT 22(27) NAME(27).
  WRITE AT 50(6) TEXT-005.
  WRITE AT 57(27) TITLE(27).
  WRITE AT 85(10) TEXT-006.
  WRITE AT 119(5) TEXT-007.
  WRITE:/.
  WRITE AT /1(40) TEXT-008.
  WRITE AT 78(15) TEXT-009.
  WRITE:/.
  WRITE AT /1(6) TEXT-005.
  WRITE AT 39(8) TEXT-010.
  WRITE AT 67(4) TEXT-011.
  WRITE AT 78(20) TEXT-012.
  WRITE AT 113(4) TEXT-013.
  WRITE:/.
ENDFORM.                    " PRINT_SECG
*&---------------------------------------------------------------------*
*&      Form  print_form_change
*&---------------------------------------------------------------------*
FORM print_form_change.
  data: l_text(38).
  concatenate pa_ryear text-z02 into l_text separated by space.
  w_width = 141.
*
  write: at /(w_width) text-z01 centered no-gap.
  write: at /(w_width) l_text centered no-gap.
  write: at /(w_width) text-rt1 centered no-gap.
  skip 1.
*
  write: /2(35) 'Section B - Company Identification',
          98 'Section C - Test for Filing Requirements'.
  write: /2(2) '1.', 98(2) '1.'.
  write: /2(2) ' ',  98(2) '2.'.
  write: /2(2) ' ',  98(2) '3.', 112(9) 'DUNS No.:'.
  write: /2(4) '2.a.'.
  write: /4(9) 'b.EIN :' no-gap.
  read table pagex index 10.
  write: 12 pagex-text1+67(9).
  write: /4(36) 'c.Was an EEO-1 Report Filed for this',
           (30) 'Establisment Last Year?'.
  skip 1.
*
  write: /2(35) 'Section D - Employment Data'.
  new-line.
  uline at (w_width).
  write: / sy-vline no-gap, (29) ' ' no-gap, sy-vline no-gap.
  write: (109) 'Number of Employees' centered no-gap, sy-vline no-gap.
* new-line.
  write: / sy-vline no-gap, (29) ' ' no-gap, sy-vline no-gap.
  uline at (109) NO-GAP.
  write: sy-vline no-gap.
* uline at (w_width).
  write: / sy-vline no-gap, (29) ' ' no-gap, sy-vline no-gap.
  write: (9) ' ' no-gap, sy-vline no-gap.
  write: (49) 'Male' centered no-gap, sy-vline no-gap,
         (49) 'Female' centered no-gap, sy-vline no-gap.
  write: / sy-vline no-gap, (29) ' ' no-gap, sy-vline no-gap.
  write: (9) 'Overrall' no-gap, sy-vline no-gap.
  uline at (99) NO-GAP.
  write: sy-vline no-gap.
*
  perform write_employment_data_heading.
  perform write_employment_data_body.
*
  write: /2(30) '* Other Questions *'.
  write: /2(2) '1.'.
  read table pagex index 36.
  write: pagex-text1+4(60).
  write: 75(5) '2.', pagex-text1+72(50).
  skip 1.
*
  write: /2(50) 'Section E - Establishment Information'.
  skip 1.
  write: /2(50) 'Section F - Remarks'.
  skip 1.
  write: /2(50) 'Section G - Certification'.
  skip 1.
*
  write: /2(13) 'Check One' no-gap.
  read table pagex index 41.
  write: pagex-text1+9(117) no-gap, ')'.

  write: /2(13) ' ' no-gap.
  read table pagex index 42.
  write: pagex-text1+9(100).
  skip 1.
*
  read table pagex index 44.
  write: /2 pagex-text1. skip 1.
  read table pagex index 46.
  write: /2 pagex-text1. skip 1.
  read table pagex index 48.
  write: /2 pagex-text1. skip 1.
ENDFORM.                    " print_form_change
*&---------------------------------------------------------------------*
*&      Form  write_employment_data_heading
*&---------------------------------------------------------------------*
FORM write_employment_data_heading.
  write: / sy-vline no-gap.
  write: (29) 'Job Categories' centered no-gap, sy-vline no-gap.
  write: (9) 'Totals'   left-justified no-gap, sy-vline no-gap.
  write: (9) 'White'    left-justified no-gap, sy-vline no-gap.
  write: (9) 'Black'    left-justified no-gap, sy-vline no-gap.
  write: (9) 'Hispanic' left-justified no-gap, sy-vline no-gap.
  write: (9) 'Asian or' left-justified no-gap, sy-vline no-gap.
  write: (9) 'American' left-justified no-gap, sy-vline no-gap.
  write: (9) 'White'    left-justified no-gap, sy-vline no-gap.
  write: (9) 'Black'    left-justified no-gap, sy-vline no-gap.
  write: (9) 'Hispanic' left-justified no-gap, sy-vline no-gap.
  write: (9) 'Asian or' left-justified no-gap, sy-vline no-gap.
  write: (9) 'American' left-justified no-gap, sy-vline no-gap.
*
  write: / sy-vline no-gap.
  write: (29) ' ' no-gap, sy-vline no-gap.
  write: (9) '(Sum of'  left-justified no-gap, sy-vline no-gap.
  write: (9) '(Not of'  left-justified no-gap, sy-vline no-gap.
  write: (9) '(Not of'  left-justified no-gap, sy-vline no-gap.
  write: (9) ' '        left-justified no-gap, sy-vline no-gap.
  write: (9) 'Pacific'  left-justified no-gap, sy-vline no-gap.
  write: (9) 'Indian or' left-justified no-gap, sy-vline no-gap.
  write: (9) '(Not of'  left-justified no-gap, sy-vline no-gap.
  write: (9) '(Not of'  left-justified no-gap, sy-vline no-gap.
  write: (9) ' '        left-justified no-gap, sy-vline no-gap.
  write: (9) 'Pacific'  left-justified no-gap, sy-vline no-gap.
  write: (9) 'Indian or' left-justified no-gap, sy-vline no-gap.
*
  write: / sy-vline no-gap.
  write: (29) ' ' no-gap, sy-vline no-gap.
  write: (9) 'B - K)'   left-justified no-gap, sy-vline no-gap.
  write: (9) 'Hispanic' left-justified no-gap, sy-vline no-gap.
  write: (9) 'Hispanic' left-justified no-gap, sy-vline no-gap.
  write: (9) ' '        left-justified no-gap, sy-vline no-gap.
  write: (9) 'Islander' left-justified no-gap, sy-vline no-gap.
  write: (9) 'Alaskan'  left-justified no-gap, sy-vline no-gap.
  write: (9) 'Hispanic' left-justified no-gap, sy-vline no-gap.
  write: (9) 'Hispanic' left-justified no-gap, sy-vline no-gap.
  write: (9) ' '        left-justified no-gap, sy-vline no-gap.
  write: (9) 'Islander' left-justified no-gap, sy-vline no-gap.
  write: (9) 'Alaskan'  left-justified no-gap, sy-vline no-gap.
*
  write: / sy-vline no-gap.
  write: (29) ' ' no-gap, sy-vline no-gap.
  write: (9) ' '        left-justified no-gap, sy-vline no-gap.
  write: (9) 'Origin)'  left-justified no-gap, sy-vline no-gap.
  write: (9) 'Origin)'  left-justified no-gap, sy-vline no-gap.
  write: (9) ' '        left-justified no-gap, sy-vline no-gap.
  write: (9) ' '        left-justified no-gap, sy-vline no-gap.
  write: (9) 'Native'   left-justified no-gap, sy-vline no-gap.
  write: (9) 'Origin)'  left-justified no-gap, sy-vline no-gap.
  write: (9) 'Origin)'  left-justified no-gap, sy-vline no-gap.
  write: (9) ' '        left-justified no-gap, sy-vline no-gap.
  write: (9) ' '        left-justified no-gap, sy-vline no-gap.
  write: (9) 'Native'   left-justified no-gap, sy-vline no-gap.
*
  write: / sy-vline no-gap, (29) ' ' no-gap, sy-vline no-gap.
  uline at (109) NO-GAP.
  write: sy-vline no-gap.
*
  write: / sy-vline no-gap.
  write: (29) ' ' no-gap, sy-vline no-gap.
  write: (9) 'A' centered no-gap, sy-vline no-gap.
  write: (9) 'B' centered no-gap, sy-vline no-gap.
  write: (9) 'C' centered no-gap, sy-vline no-gap.
  write: (9) 'D' centered no-gap, sy-vline no-gap.
  write: (9) 'E' centered no-gap, sy-vline no-gap.
  write: (9) 'F' centered no-gap, sy-vline no-gap.
  write: (9) 'G' centered no-gap, sy-vline no-gap.
  write: (9) 'H' centered no-gap, sy-vline no-gap.
  write: (9) 'I' centered no-gap, sy-vline no-gap.
  write: (9) 'J' centered no-gap, sy-vline no-gap.
  write: (9) 'K' centered no-gap, sy-vline no-gap.
  uline at (w_width).
ENDFORM.                    " write_employment_data_heading
*&---------------------------------------------------------------------*
*&      Form  write_employment_data_body
*&---------------------------------------------------------------------*
FORM write_employment_data_body.
  write: / sy-vline no-gap.
  write: (26) 'Officials and Managers' no-gap, sy-vline no-gap.
  write: (2) '1' right-justified no-gap, sy-vline no-gap.
  read table pagex index 19.
  perform write_value.

* write: / sy-vline no-gap.
* write: (16) 'Managers' no-gap, sy-vline no-gap.
* write: (3) ' ' centered no-gap, sy-vline no-gap.
* do 11 times.
*     write: (9) ' ' no-gap, sy-vline no-gap.
* enddo.
  uline at (w_width).
*
  write: / sy-vline no-gap.
  write: (26) 'Professionals' no-gap, sy-vline no-gap.
  write: (2) '2' right-justified no-gap, sy-vline no-gap.
  read table pagex index 20.
  perform write_value.
  uline at (w_width).
*
  write: / sy-vline no-gap.
  write: (26) 'Technicians' no-gap, sy-vline no-gap.
  write: (2) '3' right-justified no-gap, sy-vline no-gap.
  read table pagex index 21.
  perform write_value.
  uline at (w_width).
*
  write: / sy-vline no-gap.
  write: (26) 'Sales Workers' no-gap, sy-vline no-gap.
  write: (2) '4' right-justified no-gap, sy-vline no-gap.
  read table pagex index 22.
  perform write_value.
  uline at (w_width).
*
  write: / sy-vline no-gap.
  write: (26) 'Office and Clerical' no-gap, sy-vline no-gap.
  write: (2) '5' right-justified no-gap, sy-vline no-gap.
  read table pagex index 23.
  perform write_value.

* write: / sy-vline no-gap.
* write: (16) 'Clerical' no-gap, sy-vline no-gap.
* write: (3) ' ' centered no-gap, sy-vline no-gap.
* do 11 times.
*     write: (9) ' ' no-gap, sy-vline no-gap.
* enddo.
  uline at (w_width).
*
  write: / sy-vline no-gap.
  write: (26) 'Craft Workers (Skilled)' no-gap, sy-vline no-gap.
  write: (2) '6' right-justified no-gap, sy-vline no-gap.
  read table pagex index 24.
  perform write_value.

* write: / sy-vline no-gap.
* write: (16) '(Skilled)' no-gap, sy-vline no-gap.
* write: (3) ' ' centered no-gap, sy-vline no-gap.
* do 11 times.
*     write: (9) ' ' no-gap, sy-vline no-gap.
* enddo.
  uline at (w_width).
*
  write: / sy-vline no-gap.
  write: (26) 'Operatives (Semi Skilled)' no-gap, sy-vline no-gap.
  write: (2) '7' right-justified no-gap, sy-vline no-gap.
  read table pagex index 25.
  perform write_value.

* write: / sy-vline no-gap.
* write: (16) '(Semi Skilled)' no-gap, sy-vline no-gap.
* write: (3) ' ' centered no-gap, sy-vline no-gap.
* do 11 times.
*     write: (9) ' ' no-gap, sy-vline no-gap.
* enddo.
  uline at (w_width).
*
  write: / sy-vline no-gap.
  write: (26) 'Laberers (Unskilled)' no-gap, sy-vline no-gap.
  write: (2) '8' right-justified no-gap, sy-vline no-gap.
  read table pagex index 26.
  perform write_value.

* write: / sy-vline no-gap.
* write: (16) '(Unskilled)' no-gap, sy-vline no-gap.
* write: (3) ' ' centered no-gap, sy-vline no-gap.
* do 11 times.
*     write: (9) ' ' no-gap, sy-vline no-gap.
* enddo.
  uline at (w_width).
*
  write: / sy-vline no-gap.
  write: (26) 'Service Workers' no-gap, sy-vline no-gap.
  write: (2) '9' right-justified no-gap, sy-vline no-gap.
  read table pagex index 27.
  perform write_value.
  uline at (w_width).
*
  write: / sy-vline no-gap.
  write: (26) 'Total' right-justified no-gap, sy-vline no-gap.
  write: (2) '10' no-gap, sy-vline no-gap.
  read table pagex index 29.
  perform write_value.
  uline at (w_width).
*
  write: / sy-vline no-gap.
  write: (26) 'Previous Reported Total' right-justified no-gap,
               sy-vline no-gap.
  write: (2) '11' no-gap, sy-vline no-gap.
  read table pagex index 31.
  perform write_value.
  uline at (w_width).
ENDFORM.                    " write_employment_data_body
*&---------------------------------------------------------------------*
*&      Form  write_value
*&---------------------------------------------------------------------*
FORM write_value.
  write: (9) pagex-text1+54(5) centered no-gap, sy-vline no-gap.
  write: (9) pagex-text1+62(5) centered no-gap, sy-vline no-gap.
  write: (9) pagex-text1+69(5) centered no-gap, sy-vline no-gap.
  write: (9) pagex-text1+76(5) centered no-gap, sy-vline no-gap.
  write: (9) pagex-text1+83(5) centered no-gap, sy-vline no-gap.
  write: (9) pagex-text1+90(5) centered no-gap, sy-vline no-gap.
  write: (9) pagex-text1+98(5) centered no-gap, sy-vline no-gap.
  write: (9) pagex-text1+105(5) centered no-gap, sy-vline no-gap.
  write: (9) pagex-text1+112(5) centered no-gap, sy-vline no-gap.
  write: (9) pagex-text1+119(5) centered no-gap, sy-vline no-gap.
  write: (9) pagex-text1+126(5) centered no-gap, sy-vline no-gap.
ENDFORM.                    " write_value
