************************************************************************
*** Report  ID   : ZHBCR00101
*** Report  Name : DISTINCT VALUE ( TABLE FIELD )
*** Created By   : MIRALUZ Co.Ltd
*** Description.
************************************************************************
REPORT ZHBCR00101        NO STANDARD PAGE HEADING  LINE-COUNT 65
                                                   LINE-SIZE  80.

DATA: W_TABLE  LIKE  FIELDDIFTB-TABNAME_K,
      W_FIELD  LIKE  FIELDDIFTB-FIELDNAMEK.

DATA: LI_FIELD LIKE  FIELDDIFTB-TABNAME_K,
      lt_field       LIKE TABLE OF li_field.        " Dynamic field

*DATA: LI_GROUP(30),
DATA: LI_GROUP LIKE  FIELDDIFTB-TABNAME_K,
      lt_group       LIKE TABLE OF li_group.        " Dynamic field

DATA: cre_lvc_t_fcat TYPE lvc_t_fcat.
DATA: l_fcat TYPE lvc_s_fcat.
DATA: g_gen_table TYPE REF TO data.
DATA: t_gen_table TYPE REF TO data.

FIELD-SYMBOLS: <gt_table> TYPE table,
               <fs>.

SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-001.
PARAMETERS: p_table  LIKE DD02L-TABNAME OBLIGATORY MEMORY ID dtb ,
            p_field  LIKE  fielddiftb-fieldnamek OBLIGATORY .
SELECTION-SCREEN END OF BLOCK blk01.

INITIALIZATION.

START-OF-SELECTION.

  w_table = p_table.
  w_field = p_field.

  CLEAR l_fcat.
  l_fcat-ref_table = p_table.
  l_fcat-ref_field = p_field.
  l_fcat-col_pos   = 1.
  l_fcat-fieldname = p_field.
  APPEND l_fcat TO cre_lvc_t_fcat.

  CLEAR l_fcat.
  l_fcat-ref_table = 'SYST'.
  l_fcat-ref_field = 'TABIX'.
*  L_FCAT-INTTYPE = 'N'.
  l_fcat-col_pos   = 2.
  l_fcat-fieldname = 'CNT'.
  APPEND l_fcat TO cre_lvc_t_fcat.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
                       EXPORTING it_fieldcatalog = cre_lvc_t_fcat
                       IMPORTING ep_table = g_gen_table.
  ASSIGN g_gen_table->* TO <gt_table>.

*-Dynamic fields was selected in (w_table) table
  li_field  =  p_field.
  CONCATENATE p_field 'COUNT( * ) AS CNT' INTO li_field
              SEPARATED BY space.
  APPEND li_field TO lt_field.
  li_group = p_field.
  APPEND li_group TO lt_group.

  PERFORM aaa TABLES lt_field
*                     <gt_temp>
                     <gt_table>.

END-OF-SELECTION.

AT USER-COMMAND.

TOP-OF-PAGE.
  FORMAT COLOR COL_HEADING.
  WRITE:/2 'TABLE : ', w_table , 'FIELD : ',p_field.
  FORMAT COLOR OFF.
  ULINE.
  WRITE:/2 'Distinct Value', 25 'Count', 39 'Percentage'.
  ULINE.
*&---------------------------------------------------------------------*
*&      Form  aaa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM aaa TABLES   lt_field   TYPE table
*                  gt_temp    TYPE table
                  gt_table   TYPE table.

  FIELD-SYMBOLS: <ls>,
                 <ls1>.

  REFRESH: gt_table.

*-Read Table (w_table) with select field
  SELECT (lt_field) INTO TABLE gt_table FROM (w_table)
         GROUP BY (lt_group).


*  REFRESH gt_temp.
  DATA: tot_cnt TYPE i,
         t_per TYPE p DECIMALS 2,
         tcnt(07) TYPE n,
         dist_cnt(10).

  DESCRIBE TABLE gt_table LINES tcnt.
  CONCATENATE tcnt '#' INTO dist_cnt
              SEPARATED BY space.
  LOOP AT gt_table.
    ASSIGN COMPONENT 'CNT' OF STRUCTURE gt_table  TO <ls1>.
    tot_cnt = tot_cnt + <ls1>.
  ENDLOOP.
  SORT GT_TABLE.
  LOOP AT gt_table.
    ASSIGN COMPONENT p_field OF STRUCTURE gt_table  TO <ls>.
    ASSIGN COMPONENT 'CNT' OF STRUCTURE gt_table  TO <ls1>.
    t_per = ( <ls1> / tot_cnt ) * 100.
    WRITE:/2 <ls>,
           20 <ls1>,
           30 t_per, ' %'.
  ENDLOOP.
  ULINE.
  WRITE:/2 dist_cnt NO-ZERO, 20 tot_cnt.
ENDFORM.                    " aaa
