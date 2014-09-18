*----------------------------------------------------------------------*
*   INCLUDE YAPP239L_TOP                                               *
*----------------------------------------------------------------------*

CONTROLS: tc_app239 TYPE TABLEVIEW USING SCREEN 110.

TABLES: ausp,  "Characteristic Values
        cabn,  "Characteristic
        ztpp_veh_model.  "Vehicle Model master

DATA: BEGIN OF IT_OBJEK OCCURS 0,
        objek TYPE ausp-objek,
      END OF IT_OBJEK.

RANGES: R_OBJEK FOR AUSP-OBJEK,
        R_ATINN FOR AUSP-ATINN.

DATA: BEGIN OF it_characteristic OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,  "Key of object to be classified
        atinn TYPE AUSP-atinn,  "Internal characteristic
        ATNAM TYPE CABN-ATNAM,  "Characteristic name
        atwrt TYPE AUSP-atwrt,  "Characteristic value
        atflv type ausp-atflv,
      END OF it_characteristic.

DATA: BEGIN OF it_app239 OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,      "Key of object to be classified
*        MITU,
        mitu_date(08),              "Date
        model(03),                  "Model
        body_serial(06),            "Body Number
        bodyno(09),                 "Body Number
        mi(10),                     "Spec
        ocn(10),                    "OCN
        WORKOrDER(14),               "Work Order
        ext_color(03),              "External Color
        int_color(03),              "Internal Color
        sequence_serial(04),        "Serial
        sequence_date(10),          "Sequence Date
      END OF it_app239.

DATA: BEGIN OF it_excel_239 OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,      "Key of object to be classified
*        MITU,
        mitu_date(10),              "Date
        model(10),                  "Model
        body_serial(12),            "Body Number
        mi(10),                     "Spec
        ocn(10),                    "OCN
        WORKOrDER(14),               "Work Order
        ext_color(15),              "External Color
        int_color(15),              "Internal Color
        sequence_serial(15),        "Serial
        sequence_date(15),          "Sequence Date
      END OF it_excel_239.

TYPE-POOLS: vrm.

DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.

* Parameters
DATA: p_company(10),             "Company
      p_model(03),               "Model
      p_bodyser(06) type n,      "Body Serial
      p_bodyno(09),              "Body NO = Model+BodySer
      p_cdate_st TYPE sy-datum,  "From_date
      p_cdate_en TYPE sy-datum,  "To_date
      p_orderno(14),             "Order Number
      p_ext_color(03),           "External Color
      p_int_color(03).           "Internal Color

* DROPDOWN LIST for Parameter
* P_MODEL(Model)
DATA: name        TYPE vrm_id,
      it_model_list  TYPE vrm_values,
      wa_model_value LIKE LINE OF it_model_list.
RANGES: r_model FOR p_model.

DATA  wa_init_flg.
