************************************************************************
* Program Name      : ZAPP220M_219_CONTENTS
* Author            : CHOI WOON-MOOK
* Creation Date     : 2003.08.27.
* Specifications By : CHOI WOON-MOOK
* Development Request No :
* Addl Documentation:
* Description       : 219 OPTION VALUE
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
*&---------------------------------------------------------------------*
*& Include ZAPP220MTOP                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*

program  zapp220m_219_contents   MESSAGE-ID  ZMPP.

tables: ztbm_219_desc,
        ztbm_219_value,
        ztpp_veh_model.

data: begin of it_219val  occurs  0.
        include structure  ztbm_219_value.
data: end of it_219val.

data: begin of is219,
        comp(5),    "default company 'HMMA'
        model     like  ztbm_219_desc-model,
        name219   like  ztbm_219_desc-name_219,
        desc219   like  ztbm_219_desc-desc_219.
data  end of is219.

controls: tc100 type tableview using screen 100.
