*----------------------------------------------------------------------*
*   INCLUDE YAPP223L_TOP                                               *
*----------------------------------------------------------------------*
REPORT yapp223m_alc_code MESSAGE-ID zmpp    .

TABLES: cabn,    "Characteristic
        cawn,    "Characteristic values
        cuvtln,  "Line object of variant table
        cuvtab,  "Variant table basic data
        CUVTAB_VALC,  "CHAR format values
        CUVTAB_VALN,  "NON-CHAR format values
        cuvtab_tx.  "Descriptions for Variant Table
*        klah,    "Class Header Data
*        ksml.    "Characteristics of a Class

CONTROLS: TC_APP223 TYPE TABLEVIEW USING SCREEN 1205.
controls: TC_APP223_NEW type tableview using screen 1206.

TYPE-POOLS: vrm.

*     Descriptions
DATA: BEGIN OF wa_descriptions OCCURS 1.
        INCLUDE STRUCTURE vtdescr.
DATA: END OF wa_descriptions.
*     Characteristics
DATA: BEGIN OF it_column OCCURS 0.
        INCLUDE STRUCTURE vtchars.
DATA: END OF it_column.

DATA: BEGIN OF it_vtentries OCCURS 0.
*.      Maintain Variant Table
        INCLUDE STRUCTURE vtentries.
DATA:   column TYPE vtentries-vtlinnoint ,
        col_name TYPE vtentries-vtcharact .
DATA: END OF it_vtentries.
*
*
DATA: BEGIN OF it_table_header occurs 0.
        INCLUDE STRUCTURE cuvtab.
DATA: END OF it_table_header.
DATA: BEGIN OF IT_CUVTLN OCCURS 0.
        INCLUDE STRUCTURE CUVTLN.
DATA: END OF IT_CUVTLN.
DATA: BEGIN OF it_lines_old OCCURS 0.
        INCLUDE STRUCTURE cuvtln.
DATA: END OF it_lines_old.

DATA: wa_lines_1205 LIKE CUVTLN.
DATA: BEGIN OF it_lines_new OCCURS 0.
        INCLUDE STRUCTURE cuvtln.
DATA: END OF it_lines_new.

DATA: BEGIN OF it_values_c_old OCCURS 0.
        INCLUDE STRUCTURE cuvtab_valc.
DATA: END OF it_values_c_old.

DATA: BEGIN OF it_values_c_new OCCURS 0.
        INCLUDE STRUCTURE cuvtab_valc.
DATA: END OF it_values_c_new.

DATA: BEGIN OF it_values_n_old OCCURS 0.
        INCLUDE STRUCTURE cuvtab_valn.
DATA: END OF it_values_n_old.

DATA: BEGIN OF it_values_n_new OCCURS 0.
        INCLUDE STRUCTURE cuvtab_valn.
DATA: END OF it_values_n_new.
*
*
TYPES: BEGIN OF ST_APP223,
*.      FIELDS TO BE DISPLAYED
         mark,
         line TYPE vtentries-vtlineno,
         code(04),            " Code
         date(10),            " Date
         con_col(25),         " Concatenated Keys
         col_01(04),          " 1st Column
         col_02(04),          " 2nd Column
         col_03(04),          " 3rd Column
         col_04(04),
         col_05(04),
         col_06(04),
         col_07(04),
         col_08(04),
         col_09(04),
         col_10(04),
         col_11(04),
         col_12(04),
         col_13(04),
         col_14(04),
         col_15(04),
         col_16(04),
         col_17(04),
         col_18(04),
         col_19(04),
         col_20(04),
         col_21(04),
         col_22(04),
         col_23(04),
         col_24(04),
         col_25(04),
         user(12),
      END OF ST_APP223,
      WA_APP223 TYPE ST_APP223.
DATA: IT_APP223 TYPE ST_APP223 OCCURS 0 WITH HEADER LINE.
data: IT_APP223_NEW type ST_APP223 occurs 0 with header line.

DATA: BEGIN OF IT_EXCEL_1205 OCCURS 0,
        code(10),      " Code
        con_col(20),   " Concatenated Key value.
        date(10),      " Date when data was changed
        col_01(15),    " 1st Column
        col_02(15),    " 2nd Column
        col_03(15),    " 3rd Column
        col_04(15),
        col_05(15),
        col_06(15),
        col_07(15),
        col_08(15),
        col_09(15),
        col_10(15),
        col_11(15),
        col_12(15),
        col_13(15),
        col_14(15),
        col_15(15),
        col_16(15),
        col_17(15),
        col_18(15),
        col_19(15),
        col_20(15),
      END OF IT_EXCEL_1205.

*. PARAMETERS
DATA: p_company(04),
      p_path(20),
      p_model(03),  "(MODEL (EM(SONATA), CM(SANTAFE)))
      p_part(01),  "UNIQUE OR COLOR PART OF THE ALC CODE
      p_key(03),                                            "(1 ~ 200)
      p_key_01(03),
      p_key_02(03),
      p_key_03(03),
      p_key_04(03),
      p_key_05(03),
      p_key_06(03),
      p_key_07(03),
      p_key_08(03),
      p_key_09(03),
      p_key_10(03),
      p_key_11(03),
      p_key_12(03),
      p_key_13(03),
      p_key_14(03),
      p_key_15(03),
      p_key_16(03),
      p_key_17(03),
      p_key_18(03),
      p_key_19(03),
      p_key_20(03),
      p_column(10),
      p_col_name(10),
      p_sort_seq,
      p_code(04),
      p_code_chg(04).

*Full Code(ALC) = p_model+'_ALC_'+p_part+'_'+p_key.
DATA: p_full_code LIKE tablstruct-var_tab.

* DROPDOWN LIST for Parameter
* P_MODEL(Model)
DATA: name        TYPE vrm_id,
      model_list  TYPE vrm_values,
      model_value LIKE LINE OF model_list.
RANGES: r_model FOR ztpp_veh_model-model.
* P_PART(U or C of ALC)
DATA: part_list TYPE vrm_values,
      part_value LIKE LINE OF part_list.
RANGES: r_part FOR p_part.
* P_KEY(1, ... , 200)
DATA: key_list TYPE vrm_values,
      key_value LIKE LINE OF key_list.
RANGES: r_key FOR p_key.

*For batch uploading
data: p_f_name  LIKE  rlgrap-filename ,
      p_f_type    LIKE  rlgrap-filetype .

DATA : ok_code TYPE sy-ucomm.
DATA  WA_INIT_1205.
DATA  wa_upd_1205.
DATA  WA_INIT_1206.
