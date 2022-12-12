*&---------------------------------------------------------------------*
*&  Include           ZTSGF_ALV_SPLIT_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  zf_create_containers
*&---------------------------------------------------------------------*
form zf_create_containers.

  if v_grid is bound. return. endif.

  create object v_grid
    exporting
      container_name = 'V_GRID'.

  perform: zf_create_first_splitter,
           zf_create_second_splitter.

endform.                    "zf_create_containers
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_SPLIT
*&---------------------------------------------------------------------*
form zf_alv_split.

  perform: zf_read_ztbca_ret_10 changing t_ret_10.
  perform zf_read_ztbca_ret_11 using t_ret_10
                            changing t_ret_11.
  perform zf_read_ztbpp_cons_99 using t_ret_11
                             changing t_cons_99.

  call screen 9010.

endform.                    " ZF_ALV_SPLIT
*&---------------------------------------------------------------------*
*&      Form  ZF_READ_ZTBCA_RET_10
*&---------------------------------------------------------------------*
form zf_read_ztbca_ret_10 changing pt_ret_10 type y_t_ret_10.

  clear pt_ret_10.

  select werks sernr connr
         matnr qmnum dissy_mblnr
         dissy_mjahr pymt_mblnr pymt_mjahr
         retn_mblnr retn_mjahr
    from ztbca_ret_10
    into table pt_ret_10
    where werks eq p_werks.

endform.                    " ZF_READ_ZTBCA_RET_10
*&---------------------------------------------------------------------*
*&      Form  ZF_BUILD_ALV
*&---------------------------------------------------------------------*
form zf_build_alv.

  data: lt_fieldcat type lvc_t_fcat,
        lw_layout type lvc_s_layo,
        l_handler type ref to lcl_event_receiver.

  lw_layout-zebra = abap_true.
  lw_layout-cwidth_opt = abap_true.

  perform zf_create_field_cat changing lt_fieldcat.

  perform zf_create_hotspot changing lt_fieldcat.

  perform zf_create_alv.

  perform zf_create_handler changing l_handler.

  call method v_principal_alv->set_table_for_first_display
    exporting
      is_layout                     = lw_layout
    changing
      it_outtab                     = t_ret_10
      it_fieldcatalog               = lt_fieldcat
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.

endform.                    " ZF_BUILD_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_FIELD_CAT
*&---------------------------------------------------------------------*
form zf_create_field_cat changing pt_fieldcat type lvc_t_fcat.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = 'ZSTGF_ALV_SPLIT1'
    changing
      ct_fieldcat            = pt_fieldcat
    exceptions                                              "#EC *
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

endform.                    " ZF_CREATE_FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_ALV
*&---------------------------------------------------------------------*
form zf_create_alv.

  if v_principal_alv is bound. return. endif.

  create object v_principal_alv
    exporting
      i_parent = v_principal_grid.

endform.                    " ZF_CREATE_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_SECOND_ALV
*&---------------------------------------------------------------------*
form zf_second_alv.
*
  perform: "zf_create_splitter,
           zf_config_splitter.

  perform zf_get_right_row changing t_ret_11_aux.



  perform zf_prepare_second_alv changing t_ret_11_aux.

endform.                    " ZF_SECOND_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_HANDLER
*&---------------------------------------------------------------------*
form zf_create_handler changing p_handler type ref to lcl_event_receiver.

  if p_handler is bound. return. endif.

  create object p_handler.

  set handler: p_handler->on_double_click for v_principal_alv,
               p_handler->on_hotspot_click for v_principal_alv.

endform.                    " ZF_CREATE_HANDLER
*&---------------------------------------------------------------------*
*&      Form  ZF_SET_STATUS_9010
*&---------------------------------------------------------------------*
form zf_set_status_9010.
  set pf-status 'ZSTS_ALV'.
  set titlebar 'ZUTS_ALV'.
endform.                    " ZF_SET_STATUS_9010
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_SPLITTER
*&---------------------------------------------------------------------*
form zf_create_first_splitter.

  if v_splitter1 is bound. return. endif.

  create object v_splitter1
    exporting
      parent            = v_grid
      rows              = 2
      columns           = 1
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
      others            = 3.

  call method v_splitter1->set_row_height
    exporting
      id                = 1
      height            = 100
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
      others            = 3.

  call method v_splitter1->set_column_width
    exporting
      id                = 1
      width             = 100
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
      others            = 3.

  v_principal_grid = v_splitter1->get_container( row = 1 column = 1 ).
  v_grid2 = v_splitter1->get_container( row = 2 column = 1 ).

endform.                    " ZF_CREATE_SPLITTER
*&---------------------------------------------------------------------*
*&      Form  ZF_CONFIG_SPLITTER
*&---------------------------------------------------------------------*
form zf_config_splitter.

  call method v_splitter1->set_row_height
    exporting
      id                = 1
      height            = 60
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
      others            = 3.

endform.                    " ZF_CONFIG_SPLITTER
*&---------------------------------------------------------------------*
*&      Form  ZF_READ_ZTBCA_RET_11
*&---------------------------------------------------------------------*
form zf_read_ztbca_ret_11 using pt_ret_10 type y_t_ret_10
                       changing pt_ret_11 type y_t_ret_11.

  clear pt_ret_11.

  if pt_ret_10 is initial. return. endif.

  select werks sernr connr
         idnrk repair_type retnr
         menge meins
    from ztbca_ret_11
    into table pt_ret_11
    for all entries in pt_ret_10
    where werks eq pt_ret_10-werks
      and sernr eq pt_ret_10-sernr
      and connr eq pt_ret_10-connr.

endform.                    " ZF_READ_ZTBCA_RET_11
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARE_SECOND_ALV
*&---------------------------------------------------------------------*
form zf_prepare_second_alv changing pt_ret_11_aux type y_t_ret_11.

  data: lt_fieldcat type lvc_t_fcat,
        lw_layout type lvc_s_layo,
        l_handler type ref to lcl_event_receiver.

  lw_layout-zebra = abap_true.
  lw_layout-cwidth_opt = abap_true.
  lw_layout-sel_mode = 'A'.

  perform zf_create_second_field_cat changing lt_fieldcat.

  perform zf_create_second_alv.

  perform zf_create_second_handler changing l_handler.

  call method v_itens_alv->set_table_for_first_display
    exporting
      is_layout                     = lw_layout
    changing
      it_outtab                     = pt_ret_11_aux
      it_fieldcatalog               = lt_fieldcat
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.

endform.                    " ZF_PREPARE_SECOND_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_SECOND_FIELD_CAT
*&---------------------------------------------------------------------*
form zf_create_second_field_cat changing pt_fieldcat type lvc_t_fcat.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = 'ZSTGF_ALV_SPLIT2'
    changing
      ct_fieldcat            = pt_fieldcat
    exceptions                                              "#EC *
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

endform.                    " ZF_CREATE_SECOND_FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_SECOND_ALV
*&---------------------------------------------------------------------*
form zf_create_second_alv.

  if v_itens_alv is bound. return. endif.

  create object v_itens_alv
    exporting
      i_parent = v_item_grid.


endform.                    " ZF_CREATE_SECOND_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_GET_RIGHT_ROW
*&---------------------------------------------------------------------*
form zf_get_right_row changing pt_ret_11_aux type y_t_ret_11.

  clear pt_ret_11_aux.

  data: lw_ret_11 like line of t_ret_11.

  field-symbols: <fs_ret_10> like line of t_ret_10.

  read table t_ret_10 assigning <fs_ret_10> index v_row_id-row_id.

  if sy-subrc is initial.

    loop at t_ret_11 into lw_ret_11 where werks eq <fs_ret_10>-werks
                                      and sernr eq <fs_ret_10>-sernr
                                      and connr eq <fs_ret_10>-connr.

      append lw_ret_11 to pt_ret_11_aux.

    endloop.

  endif.

endform.                    " ZF_GET_RIGHT_ROW
*&---------------------------------------------------------------------*
*&      Form  ZF_RESET_SPLITTER_VIEW
*&---------------------------------------------------------------------*
form zf_reset_splitter_view.

  call method v_splitter1->set_row_height
    exporting
      id                = 1
      height            = 100
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
      others            = 3.

  call method v_splitter2->set_column_width
    exporting
      id                = 1
      width             = 100
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
      others            = 3.

endform.                    " ZF_RESET_SPLITTER_VIEW
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_SECOND_HANDLER
*&---------------------------------------------------------------------*
form zf_create_second_handler  changing p_handler type ref to lcl_event_receiver.

  if p_handler is bound. return. endif.

  create object p_handler.

  set handler: p_handler->handle_user_command_second_alv for v_itens_alv,
               p_handler->handle_toolbar_second_alv for v_itens_alv.

endform.                    " ZF_CREATE_SECOND_HANDLER
*&---------------------------------------------------------------------*
*&      Form  ZF_SHOW_ITENS_DETAILS
*&---------------------------------------------------------------------*
form zf_show_itens_details.

  data: lt_cons_99_aux type y_t_cons_99,
        lt_selected_rows type lvc_t_row.

  call method v_itens_alv->get_selected_rows
    importing
      et_index_rows = lt_selected_rows.

  if lt_selected_rows is not initial.

    perform zf_config_third_splitter.

    perform zf_get_selected_rows changing lt_selected_rows
                                          lt_cons_99_aux.

    perform zf_prepare_third_alv changing lt_cons_99_aux.

  else.

    message s000(oo) with text-t02 display like 'E'.

  endif.

endform.                    " ZF_SHOW_ITENS_DETAILS
*&---------------------------------------------------------------------*
*&      Form  ZF_READ_ZTBPP_CONS_99
*&---------------------------------------------------------------------*
form zf_read_ztbpp_cons_99 using pt_ret_11 type y_t_ret_11
                        changing pt_cons_99 type y_t_cons_99.

  clear pt_cons_99.

  if pt_ret_11 is initial. return. endif.

  select cenario seqnr
         connr operacao
    from ztbpp_cons_99
    into table pt_cons_99
    for all entries in pt_ret_11
    where connr eq pt_ret_11-connr.

endform.                    " ZF_READ_ZTBPP_CONS_99
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARE_THIRD_ALV
*&---------------------------------------------------------------------*
form zf_prepare_third_alv changing pt_cons_99_aux type y_t_cons_99.

  data: lt_fieldcat type lvc_t_fcat,
        lw_layout type lvc_s_layo,
        l_handler type ref to lcl_event_receiver.

  lw_layout-zebra = abap_true.
  lw_layout-cwidth_opt = abap_true.

  perform zf_create_third_field_cat changing lt_fieldcat.

  perform zf_create_third_alv.

  perform zf_create_third_handler changing l_handler.

  call method v_details_alv->set_table_for_first_display
    exporting
      is_layout                     = lw_layout
    changing
      it_outtab                     = pt_cons_99_aux
      it_fieldcatalog               = lt_fieldcat
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.

endform.                    " ZF_PREPARE_THIRD_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_THIRD_FIELD_CAT
*&---------------------------------------------------------------------*
form zf_create_third_field_cat  changing pt_fieldcat type lvc_t_fcat.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = 'ZSTGF_ALV_SPLIT3'
    changing
      ct_fieldcat            = pt_fieldcat
    exceptions                                              "#EC *
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

endform.                    " ZF_CREATE_THIRD_FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_THIRD_ALV
*&---------------------------------------------------------------------*
form zf_create_third_alv.

  if v_details_alv is bound. return. endif.

  create object v_details_alv
    exporting
      i_parent = v_detail_grid.

endform.                    " ZF_CREATE_THIRD_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_THIRD_HANDLER
*&---------------------------------------------------------------------*
form zf_create_third_handler changing p_handler type ref to lcl_event_receiver.

  if p_handler is bound. return. endif.

  create object p_handler.

  set handler: p_handler->handle_user_command_third_alv for v_details_alv,
               p_handler->handle_toolbar_third_alv for v_details_alv.

endform.                    " ZF_CREATE_THIRD_HANDLER
*&---------------------------------------------------------------------*
*&      Form  ZF_CLOSE_THIRD_ALV
*&---------------------------------------------------------------------*
form zf_close_third_alv.

  call method v_splitter2->set_column_width
    exporting
      id                = 1
      width             = 100
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
      others            = 3.

endform.                    " ZF_CLOSE_THIRD_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_CONFIG_THIRD_SPLITTER
*&---------------------------------------------------------------------*
form zf_config_third_splitter.

  call method v_splitter2->set_column_width
    exporting
      id                = 1
      width             = 50
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
      others            = 3.

endform.                    " ZF_CONFIG_THIRD_SPLITTER
*&---------------------------------------------------------------------*
*&      Form  ZF_GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
form zf_get_selected_rows changing pt_selected_rows type lvc_t_row
                                   pt_cons_99_aux type y_t_cons_99.

  clear pt_cons_99_aux.

  data: lw_cons_99 like line of t_cons_99.

  field-symbols: <fs_rows> like line of pt_selected_rows,
                 <fs_ret_11> like line of t_ret_11_aux.

  loop at pt_selected_rows assigning <fs_rows>.

    read table t_ret_11_aux assigning <fs_ret_11> index <fs_rows>-index.

    if sy-subrc is initial.

      loop at t_cons_99 into lw_cons_99 where connr eq <fs_ret_11>-connr.

        append lw_cons_99 to pt_cons_99_aux.

      endloop.

    endif.

  endloop.


endform.                    " ZF_GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_SECOND_SPLITTER
*&---------------------------------------------------------------------*
form zf_create_second_splitter.

  if v_splitter2 is bound. return. endif.

  create object v_splitter2
    exporting
      parent            = v_grid2
      rows              = 1
      columns           = 2
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
      others            = 3.

  call method v_splitter2->set_column_width
    exporting
      id                = 1
      width             = 100
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
      others            = 3.

  v_item_grid = v_splitter2->get_container( row = 1 column = 1 ).
  v_detail_grid = v_splitter2->get_container( row = 1 column = 2 ).

endform.                    " ZF_CREATE_SECOND_SPLITTER
*&---------------------------------------------------------------------*
*&      Form  ZF_CREATE_HOTSPOT
*&---------------------------------------------------------------------*
form zf_create_hotspot changing pt_fieldcat type lvc_t_fcat.

  field-symbols: <fs_fcat> like line of pt_fieldcat.

  loop at pt_fieldcat assigning <fs_fcat>.

    if <fs_fcat>-fieldname eq 'MATNR'.
      <fs_fcat>-hotspot = abap_true.
    endif.

  endloop.

endform.                    " ZF_CREATE_HOTSPOT
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_MM03
*&---------------------------------------------------------------------*
form zf_call_mm03.

  field-symbols: <fs_ret_10> like line of t_ret_10.

  read table t_ret_10 assigning <fs_ret_10> index v_row_no-row_id.

  if sy-subrc is initial.

    set parameter id 'MAT' field <fs_ret_10>-matnr.
*
    call transaction 'MM03' and skip first screen.

*    SUBMIT sapmmg01 USING SELECTION-SCREEN 1000 WITH p_matnr EQ <fs_ret_10>-matnr.

  endif.

endform.                    " ZF_CALL_MM03
