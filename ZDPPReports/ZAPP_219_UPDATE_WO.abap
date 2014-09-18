************************************************************************
* Program Name      : ZAPP_219_UPDATE_WO
* Author            : Furong, Wang
* Creation Date     : 10/2005
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Module Cost Update
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT zapp_equ_char_update_1
                NO STANDARD PAGE HEADING
                LINE-SIZE  100
                LINE-COUNT 65
                MESSAGE-ID zmbm.

TABLES: ztpp_wosum.
DATA: it_wosum LIKE TABLE OF ztpp_wosum WITH HEADER LINE,
      it_wosum_hd LIKE TABLE OF ztpp_wosum WITH HEADER LINE,

      it_abxalcdt LIKE TABLE OF ztbm_abxalcdt WITH HEADER LINE .


DATA: wa_material             LIKE mara-matnr                 ,
      wa_material_cl          LIKE mara-matnr                 ,
      wa_error                TYPE c                          ,
      wa_date                 TYPE d                          .

DATA: wa_tline TYPE n.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-101.
SELECT-OPTIONS: s_wo FOR ztpp_wosum-wo_ser.
SELECT-OPTIONS: s_date FOR ztpp_wosum-wocredate.
SELECTION-SCREEN END   OF BLOCK b2.

START-OF-SELECTION.
  PERFORM read_data.
  PERFORM data_process.
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.

  DATA: l_vartable        LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_vartable_300    LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_name(30)        TYPE c           ,
        l_no(03)          TYPE n,
        l_recno(4)           TYPE n,
        l_fsc             LIKE ztbm_abxalcdt-mtno,
        l_alcd             LIKE ztbm_abxalcdt-alcd,
        l_material_300   LIKE mara-matnr,
        l_mess(40).

  DATA: l_name0(20), l_name1(20), l_name2(20), l_name3(20),
        l_name4(20), l_name5(20), l_name6(20), l_name7(20),
        l_name8(20), l_name9(20), l_vals0(01), l_vals1(01),
        l_vals2(20), l_vals3(20), l_vals4(20), l_vals5(20),
        l_vals6(20), l_vals7(20), l_vals8(20), l_vals9(20),
        l_vals(01).

  it_wosum_hd[] = it_wosum[].

  DELETE ADJACENT DUPLICATES FROM it_wosum_hd
       COMPARING wo_ser nation dealer.

** WO HEADER UPDATE
  LOOP AT it_wosum_hd.
    CLEAR: l_name0(20), l_name1(20), l_name2(20), l_name3(20),
        l_name4(20), l_name5(20), l_name6(20), l_name7(20),
        l_name8(20), l_name9(20), l_vals0(01), l_vals1(01),
        l_vals2(20), l_vals3(20), l_vals4(20), l_vals5(20),
        l_vals6(20), l_vals7(20), l_vals8(20), l_vals9(20).

    CLEAR: l_fsc, wa_material, wa_material_cl, l_material_300,
           l_vartable,l_vartable[],l_vartable_300,l_vartable_300[],l_no.

    CONCATENATE it_wosum_hd-wo_ser it_wosum_hd-nation
                it_wosum_hd-dealer INTO wa_material.

    CLEAR : l_name, l_vals.
    l_fsc = it_wosum_hd-fsc.
    SELECT SINGLE alcd INTO l_alcd FROM ztbm_abxalcdt
          WHERE mtno = l_fsc.

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name .
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name .
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name .
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name .
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    DO  9 TIMES.
      l_vals1 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name1 .
      l_vartable-atnam = l_name1.       l_vartable-atwrt = l_vals1.
      APPEND l_vartable        .

      l_vals2 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name2 .
      l_vartable-atnam = l_name2.       l_vartable-atwrt = l_vals2.
      APPEND l_vartable        .

      l_vals3 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name3 .
      l_vartable-atnam = l_name3.       l_vartable-atwrt = l_vals3.
      APPEND l_vartable        .

      l_vals4 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name4 .
      l_vartable-atnam = l_name4.       l_vartable-atwrt = l_vals4.
      APPEND l_vartable        .

      l_vals5 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name5 .
      l_vartable-atnam = l_name5.       l_vartable-atwrt = l_vals5.
      APPEND l_vartable        .

      l_vals6 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name6 .
      l_vartable-atnam = l_name6.       l_vartable-atwrt = l_vals6.
      APPEND l_vartable        .

      l_vals7 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name7 .
      l_vartable-atnam = l_name7.       l_vartable-atwrt = l_vals7.
      APPEND l_vartable        .

      l_vals8 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name8 .
      l_vartable-atnam = l_name8.       l_vartable-atwrt = l_vals8.
      APPEND l_vartable        .

      l_vals9 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name9 .
      l_vartable-atnam = l_name9.       l_vartable-atwrt = l_vals9.
      APPEND l_vartable.

      l_vals0 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name0 .
      l_vartable-atnam = l_name0.       l_vartable-atwrt = l_vals0.
      APPEND l_vartable.

    ENDDO.

    DO 12 TIMES.
      l_vals1 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name1 .
      l_vartable-atnam = l_name1.       l_vartable-atwrt = l_vals1.
      APPEND l_vartable        .

      l_vals2 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name2 .
      l_vartable-atnam = l_name2.       l_vartable-atwrt = l_vals2.
      APPEND l_vartable        .

      l_vals3 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name3 .
      l_vartable-atnam = l_name3.       l_vartable-atwrt = l_vals3.
      APPEND l_vartable        .

      l_vals4 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name4 .
      l_vartable-atnam = l_name4.       l_vartable-atwrt = l_vals4.
      APPEND l_vartable        .

      l_vals5 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name5 .
      l_vartable-atnam = l_name5.       l_vartable-atwrt = l_vals5.
      APPEND l_vartable        .

      l_vals6 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name6 .
      l_vartable-atnam = l_name6.       l_vartable-atwrt = l_vals6.
      APPEND l_vartable        .

      l_vals7 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name7 .
      l_vartable-atnam = l_name7.       l_vartable-atwrt = l_vals7.
      APPEND l_vartable        .

      l_vals8 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name8 .
      l_vartable-atnam = l_name8.       l_vartable-atwrt = l_vals8.
      APPEND l_vartable        .

      l_vals9 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name9 .
      l_vartable-atnam = l_name9.       l_vartable-atwrt = l_vals9.
      APPEND l_vartable        .

      l_vals0 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name0 .
      l_vartable-atnam = l_name0.       l_vartable-atwrt = l_vals0.
      APPEND l_vartable        .

    ENDDO.
*    l_vartable_300[] = l_vartable[].
*    l_material_300 = wa_material.
*
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              mode         = 'W'
              ctype        = '001'
         TABLES
              val_table    = l_vartable
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

*    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*         EXPORTING
*              object       = l_material_300
*              mode         = 'W'
*              ctype        = '300'
*         TABLES
*              val_table    = l_vartable_300
*         EXCEPTIONS
*              no_data      = 1
*              error_mode   = 2
*              error_object = 3
*              error_value  = 4
*              OTHERS       = 5.
*
    IF sy-subrc <> 0.
      WRITE:/ wa_material, 'Error'.      wa_error = 'X'      .
    ELSE.
      WRITE:/ wa_material, 'updated'.
*      COMMIT WORK.
    ENDIF.
    CLEAR it_wosum_hd.
*    l_recno = l_recno + 1.
**    if l_recno = 50 or l_recno = 100 or l_recno = 150.
*    CONCATENATE 'processed rec.' l_recno wa_material INTO l_mess.
*    PERFORM display_progress_bar USING l_mess.
**    endif.
  ENDLOOP.
  COMMIT WORK.
  CLEAR: l_recno.

** WOCL UPDATE

  LOOP AT it_wosum.
    CLEAR: l_name0(20), l_name1(20), l_name2(20), l_name3(20),
        l_name4(20), l_name5(20), l_name6(20), l_name7(20),
        l_name8(20), l_name9(20), l_vals0(01), l_vals1(01),
        l_vals2(20), l_vals3(20), l_vals4(20), l_vals5(20),
        l_vals6(20), l_vals7(20), l_vals8(20), l_vals9(20).
    CLEAR: l_fsc, wa_material, wa_material_cl, l_material_300,
           l_vartable,l_vartable[],l_vartable_300,l_vartable_300[],l_no.

    CONCATENATE it_wosum-wo_ser it_wosum-nation
                it_wosum-dealer it_wosum-extc it_wosum-intc
                INTO wa_material.

    CLEAR : l_name, l_vals.
    l_fsc = it_wosum-fsc.
    SELECT SINGLE alcd INTO l_alcd FROM ztbm_abxalcdt
          WHERE mtno = l_fsc.

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name .
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name .
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name .
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name .
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    l_vals = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
    APPEND l_vartable        .

    DO 9 TIMES.
      l_vals1 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name1 .
      l_vartable-atnam = l_name1.       l_vartable-atwrt = l_vals1.
      APPEND l_vartable        .

      l_vals2 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name2 .
      l_vartable-atnam = l_name2.       l_vartable-atwrt = l_vals2.
      APPEND l_vartable        .

      l_vals3 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name3 .
      l_vartable-atnam = l_name3.       l_vartable-atwrt = l_vals3.
      APPEND l_vartable        .

      l_vals4 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name4 .
      l_vartable-atnam = l_name4.       l_vartable-atwrt = l_vals4.
      APPEND l_vartable        .

      l_vals5 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name5 .
      l_vartable-atnam = l_name5.       l_vartable-atwrt = l_vals5.
      APPEND l_vartable        .

      l_vals6 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name6 .
      l_vartable-atnam = l_name6.       l_vartable-atwrt = l_vals6.
      APPEND l_vartable        .

      l_vals7 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name7 .
      l_vartable-atnam = l_name7.       l_vartable-atwrt = l_vals7.
      APPEND l_vartable        .

      l_vals8 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name8 .
      l_vartable-atnam = l_name8.       l_vartable-atwrt = l_vals8.
      APPEND l_vartable        .

      l_vals9 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name9 .
      l_vartable-atnam = l_name9.       l_vartable-atwrt = l_vals9.
      APPEND l_vartable.

      l_vals0 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name0 .
      l_vartable-atnam = l_name0.       l_vartable-atwrt = l_vals0.
      APPEND l_vartable.

    ENDDO.

    DO 12 TIMES.
      l_vals1 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name1 .
      l_vartable-atnam = l_name1.       l_vartable-atwrt = l_vals1.
      APPEND l_vartable        .

      l_vals2 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name2 .
      l_vartable-atnam = l_name2.       l_vartable-atwrt = l_vals2.
      APPEND l_vartable        .

      l_vals3 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name3 .
      l_vartable-atnam = l_name3.       l_vartable-atwrt = l_vals3.
      APPEND l_vartable        .

      l_vals4 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name4 .
      l_vartable-atnam = l_name4.       l_vartable-atwrt = l_vals4.
      APPEND l_vartable        .

      l_vals5 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name5 .
      l_vartable-atnam = l_name5.       l_vartable-atwrt = l_vals5.
      APPEND l_vartable        .

      l_vals6 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name6 .
      l_vartable-atnam = l_name6.       l_vartable-atwrt = l_vals6.
      APPEND l_vartable        .

      l_vals7 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name7 .
      l_vartable-atnam = l_name7.       l_vartable-atwrt = l_vals7.
      APPEND l_vartable        .

      l_vals8 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name8 .
      l_vartable-atnam = l_name8.       l_vartable-atwrt = l_vals8.
      APPEND l_vartable        .

      l_vals9 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name9 .
      l_vartable-atnam = l_name9.       l_vartable-atwrt = l_vals9.
      APPEND l_vartable        .

      l_vals0 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name0 .
      l_vartable-atnam = l_name0.       l_vartable-atwrt = l_vals0.
      APPEND l_vartable        .

    ENDDO.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              mode         = 'W'
              ctype        = '001'
         TABLES
              val_table    = l_vartable
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

*    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*         EXPORTING
*              object       = wa_material
*              mode         = 'W'
*              ctype        = '300'
*         TABLES
*              val_table    = l_vartable
*         EXCEPTIONS
*              no_data      = 1
*              error_mode   = 2
*              error_object = 3
*              error_value  = 4
*              OTHERS       = 5.

    IF sy-subrc <> 0.
      WRITE:/ wa_material, 'Error'.      wa_error = 'X'      .
    ELSE.
      WRITE:/ wa_material, 'updated'.
*      COMMIT WORK.
    ENDIF.
    CLEAR it_wosum.
*    l_recno = l_recno + 1.
**    if l_recno = 50 or l_recno = 100 or l_recno = 150.
*    CONCATENATE 'processed rec.' l_recno wa_material INTO l_mess.
*    PERFORM display_progress_bar USING l_mess.
**    endif.
  ENDLOOP.
  COMMIT WORK.

ENDFORM.                    " DATA_PROCESS

*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MESS  text
*----------------------------------------------------------------------*
FORM display_progress_bar USING    p_text.
  DATA: lw_text(50).

  MOVE: p_text TO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lw_text.

ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  SELECT * FROM ztpp_wosum INTO TABLE it_wosum
     WHERE wo_ser IN s_wo
      AND wocredate IN s_date.

  IF sy-subrc NE 0.
    WRITE: ' No data in WOSUM table'.
    EXIT.
  ENDIF.
ENDFORM.                    " read_data
