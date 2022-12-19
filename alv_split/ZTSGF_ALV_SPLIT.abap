*&---------------------------------------------------------------------*
*& Report  ZTSGF_ALV_SPLIT
*&
*&---------------------------------------------------------------------*
report  ztsgf_alv_split.

type-pools: abap.

selection-screen begin of block b1 with frame title text-t01.
parameters: p_werks type ztbca_ret_10-werks.
selection-screen end of block b1.

types:
begin of y_ret_10,
  werks       type ztbca_ret_10-werks,
  sernr       type ztbca_ret_10-sernr,
  connr       type ztbca_ret_10-connr,
  matnr       type ztbca_ret_10-matnr,
  qmnum       type ztbca_ret_10-qmnum,
  dissy_mblnr type ztbca_ret_10-dissy_mblnr,
  dissy_mjahr type ztbca_ret_10-dissy_mjahr,
  pymt_mblnr  type ztbca_ret_10-pymt_mblnr,
  pymt_mjahr  type ztbca_ret_10-pymt_mjahr,
  retn_mblnr  type ztbca_ret_10-retn_mblnr,
  retn_mjahr  type ztbca_ret_10-retn_mjahr,
end of y_ret_10,

y_t_ret_10 type standard table of y_ret_10,

begin of y_ret_11,
werks         type ztbca_ret_11-werks,
sernr         type ztbca_ret_11-sernr,
connr         type ztbca_ret_11-connr,
idnrk         type ztbca_ret_11-idnrk,
repair_type   type ztbca_ret_11-repair_type,
retnr         type ztbca_ret_11-retnr,
menge         type ztbca_ret_11-menge,
meins         type ztbca_ret_11-meins,
end of y_ret_11,

  y_t_ret_11 type standard table of y_ret_11,

begin of y_cons_99,
  cenario  type ztbpp_cons_99-cenario,
  seqnr    type ztbpp_cons_99-seqnr,
  connr    type ztbpp_cons_99-connr,
  operacao type ztbpp_cons_99-operacao,
end of y_cons_99,

y_t_cons_99 type standard table of y_cons_99.

data: v_row_id type lvc_s_roid,
      v_row_no type lvc_s_roid.

constants:
begin of c_structure_name,
  firstalv type dd02l-tabname value 'ZSTGF_ALV_SPLIT1',
  secondalv type dd02l-tabname value 'ZSTGF_ALV_SPLIT2',
  thirdalv type dd02l-tabname value 'ZSTGF_ALV_SPLIT3',
end of c_structure_name,

c_grid type c length 6 value 'V_GRID',

begin of c_fieldname,
  matnr type c length 5 value 'MATNR',
end of c_fieldname,

begin of c_ucomm,
  back type ui_func value 'BACK',
  canc type ui_func value 'CANC',
  exit type ui_func value 'EXIT',
  close type ui_func value 'CLOSE',
  show type ui_func value 'SHOW',
end of c_ucomm,

begin of c_description,
  close type iconquick value 'FECHAR',
  show type iconquick value 'MOSTRAR',
end of c_description.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
class lcl_event_receiver definition final.

  public section.
    class-methods: on_double_click
       for event double_click of cl_gui_alv_grid
       importing
         e_row e_column es_row_no,

      on_hotspot_click
        for event hotspot_click of cl_gui_alv_grid
        importing
          es_row_no,

*    Add a toolbar
      handle_toolbar_second_alv
        for event toolbar of cl_gui_alv_grid
          importing e_object,

      handle_toolbar_third_alv
        for event toolbar of cl_gui_alv_grid
          importing e_object,

*     Deal with user action
      handle_user_command_second_alv
        for event user_command of cl_gui_alv_grid
          importing e_ucomm,

      handle_user_command_third_alv
        for event user_command of cl_gui_alv_grid
          importing e_ucomm.

endclass.                    "lcl_event_receiver DEFINITION

data: v_first_grid type ref to cl_gui_custom_container,
      v_second_grid type ref to cl_gui_container,
      v_item_grid type ref to cl_gui_container,
      v_principal_grid type ref to cl_gui_container,
      v_detail_grid type ref to cl_gui_container,
      v_principal_alv type ref to cl_gui_alv_grid,
      v_itens_alv type ref to cl_gui_alv_grid,
      v_details_alv type ref to cl_gui_alv_grid,
      v_splitter1 type ref to cl_gui_splitter_container, "cl_gui_easy_splitter_container,
      v_splitter2 type ref to cl_gui_splitter_container, "cl_gui_easy_splitter_container,
      t_ret_10 type y_t_ret_10,
      t_toolbar type stb_button,
      t_ret_11 type y_t_ret_11,
      t_ret_11_aux type y_t_ret_11,
      t_cons_99 type y_t_cons_99.

start-of-selection.
  perform: zf_alv_split.

  include ztsgf_alv_split_pbo.
  include ztsgf_alv_split_pai.
  include ztsgf_alv_split_c01.
  include ztsgf_alv_split_f01.
