*&---------------------------------------------------------------------*
*&  Include           ZH_GQMS_IF_DAILY_T01
*&---------------------------------------------------------------------*

*                      --- TABLES ---
*----------------------------------------------------------------------
TABLES: p0001,             "Infotype 0001 - org. data
        zshrgqms2 ,        "Structure for GQMS Interface
        hrp1001,hrp1000,
       *hrp1001,*hrp1000,t001p,
        *rfcdes.

DATA : BEGIN OF itab OCCURS 0,
        pernr LIKE p0001-pernr,
        plans LIKE p0001-plans,
        sname LIKE p0001-sname,
        ename LIKE p0001-ename,
        btrtl LIKE p0001-btrtl,
        orgeh LIKE p0001-orgeh,
        werks LIKE p0001-werks,
        stell LIKE p0001-stell,
        telnr LIKE p0006-telnr,
        aedtm LIKE p0006-aedtm,
        stat2 LIKE p0000-stat2,
       END OF itab.

DATA : BEGIN OF $job_pos OCCURS 0,
        objid LIKE hrp1001-objid,
        sobid LIKE hrp1001-sobid,
       $sobid LIKE hrp1001-objid,
       END OF $job_pos.

DATA : BEGIN OF $stell_desc OCCURS 0,
        stell LIKE p0001-stell,
        stext LIKE hrp1000-stext,
       END OF $stell_desc.

DATA : BEGIN OF $dep_code OCCURS 0,
        objid LIKE hrp1001-objid,
        sobid LIKE hrp1001-sobid,
       $sobid LIKE hrp1001-objid,
       END OF $dep_code.

DATA : BEGIN OF $dep_code_new OCCURS 0,
        pernr LIKE pa0001-pernr,
        plans LIKE pa0001-plans,
       END OF $dep_code_new.


DATA : BEGIN OF $dep_name OCCURS 0,
        objid LIKE hrp1000-objid,
        stext LIKE hrp1000-stext,
       END OF $dep_name.
DATA : BEGIN OF $grp_name OCCURS 0,
        objid LIKE hrp1000-objid,
        stext LIKE hrp1000-stext,
       END OF $grp_name.

DATA  it_t001p LIKE t001p OCCURS 20 WITH HEADER LINE.

DATA : BEGIN OF i_dept OCCURS 0,
        objid LIKE hrp1001-objid,
        stext LIKE hrp1000-stext,
        sobid LIKE hrp1001-sobid,
        plans LIKE pa0001-plans,
       END OF i_dept.

DATA i_zshrgqms1 TYPE TABLE OF zshrgqms1 WITH HEADER LINE.
DATA : BEGIN OF i_mc_stext OCCURS 0,
        mc_stext LIKE hrp1000-mc_stext,
        objid LIKE hrp1000-objid,
       END OF i_mc_stext.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME TITLE text-100.
SELECT-OPTIONS s_pernr FOR p0001-pernr MATCHCODE OBJECT prem.
PARAMETERS : par_date LIKE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END   OF BLOCK 0.

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-101.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:
  par_r1 RADIOBUTTON GROUP 1.
SELECTION-SCREEN:
  COMMENT 03(29) text-003 FOR FIELD par_r1.
PARAMETERS:
  par_file(50).

SELECTION-SCREEN:
  END OF LINE,
  BEGIN OF LINE.
SELECTION-SCREEN:
  END OF LINE,
  BEGIN OF LINE.
PARAMETERS:
  par_r2 RADIOBUTTON GROUP 1.
SELECTION-SCREEN:
  COMMENT 03(50) text-005 FOR FIELD par_r2,
  END OF LINE.
PARAMETERS : par_dest LIKE rfcdes-rfcdest
                       DEFAULT 'WMHR01'.
SELECTION-SCREEN END   OF BLOCK 1.

PARAMETERS p_debug NO-DISPLAY.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
DEFINE u_break.
  if not p_debug is initial.
    break-point.
  endif.
END-OF-DEFINITION.
DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

****************************** Global Data *****************************

DATA  i_zshrgqms2 TYPE TABLE OF zshrgqms2 WITH HEADER LINE.
