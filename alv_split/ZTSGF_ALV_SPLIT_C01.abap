*&---------------------------------------------------------------------*
*&  Include           ZTSGF_ALV_SPLIT_C01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

  method  on_double_click.
    v_row_id = es_row_no.
    perform zf_second_alv.
    v_principal_alv->set_scroll_info_via_id( exporting is_row_info = e_row
                                                       is_col_info = e_column
                                                       is_row_no   = es_row_no ).
  endmethod.                    "handle_double_clickENDCLASS.
  method  on_hotspot_click.
    v_row_no = es_row_no.
    perform zf_call_mm03.
  endmethod.                    "handle_double_clickENDCLASS.
*   Add toolbar
  method handle_toolbar_second_alv.
    clear t_toolbar.
    t_toolbar-function = 'CLOSE'.
    t_toolbar-icon = icon_incomplete.
    append t_toolbar to e_object->mt_toolbar.
    t_toolbar-function = 'SHOW'.
    t_toolbar-icon = icon_businav_value_chain.
    append t_toolbar to e_object->mt_toolbar.
  endmethod.
  method handle_toolbar_third_alv.
    clear t_toolbar.
    t_toolbar-function = 'CLOSE'.
    t_toolbar-icon = icon_incomplete.
    append t_toolbar to e_object->mt_toolbar.
  endmethod.                 "handle_toolbar
* When click menu, deal with this action
  method handle_user_command_second_alv.
    case e_ucomm.
      when 'CLOSE'.
        perform zf_reset_splitter_view.
      when 'SHOW'.
        perform zf_show_itens_details.
    endcase.
  endmethod.                    "handle_user_command
  method handle_user_command_third_alv.
    case e_ucomm.
      when 'CLOSE'.
        perform zf_close_third_alv.
    endcase.
  endmethod.                    "handle_user_command

endclass.                    "lcl_event_receiver IMPLEMENTATION
