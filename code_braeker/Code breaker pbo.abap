*&---------------------------------------------------------------------*
*&  Include           ZTSGF_CODE_BREAKER_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_status_9000 output.
  set pf-status 'ZSTS_SELECT_SCREEN'.
  set titlebar 'ZUTS_SELECT_SCREEN'.
endmodule.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_status_9010 output.
  set pf-status 'ZSTS_HELP_SCREEN'.
  set titlebar 'ZUTS_HELP_SCREEN'.
endmodule.                 " STATUS_9010  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9020  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_status_9020 output.
  set pf-status 'ZSTS_GAME_SCREEN'.
  set titlebar 'ZUTS_GAME_SCREEN'.
endmodule.                 " STATUS_9020  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9030  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_status_9030 output.
  set pf-status 'ZSTS_GAME_WIN'.
  set titlebar 'ZUTS_GAME_WIN'.

  perform zf_comput_score.

endmodule.                 " STATUS_9030  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9040  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_status_9040 output.
  set pf-status 'ZSTS_RANK_SCREEN'.
  set titlebar 'ZUTS_RANK_SCREEN'.

  case abap_true.
    when rb_easy.
      v_dif = c_easy_dificulty.
    when rb_medium.
      v_dif = c_medium_dificulty.
    when rb_hard.
      v_dif = c_hard_dificulty.
  endcase.

  perform zf_read_table_by_dif.
  sort t_code_break by tentativas ascending.

  if v_custom_container is initial.

    perform zf_build_custom_container.

  else.

    v_grid->refresh_table_display( ).

  endif.

endmodule.                 " STATUS_9040  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_status_9050 output.
  set pf-status 'ZSTS_GAME_SCREEN_MED'.
  set titlebar 'ZUTS_GAME_SCREEN_MED'.
endmodule.                 " STATUS_9050  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9060  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_status_9060 output.
  set pf-status 'ZSTS_GAME_SCREEN_H'.
  set titlebar 'ZUTS_GAME_SCREEN_H'.
endmodule.                 " STATUS_9060  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9070  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_status_9070 output.
  set pf-status 'ZSTS_NEW_REGISTER'.
  set titlebar 'ZUTS_NEW_REGISTER'.
endmodule.                 " STATUS_9070  OUTPUT
