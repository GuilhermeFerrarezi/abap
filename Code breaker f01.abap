*&---------------------------------------------------------------------*
*&  Include           ZTSGF_CODE_BREAKER_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHANGE_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_change_screen_easy.
  call screen 9020.
endform.                    " CHANGE_SCREEN

*&---------------------------------------------------------------------*
*&      Form  ZF_RANDOMIZE_GAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_randomize_game.

  v_seed = cl_abap_random=>seed( ).

  call method cl_abap_random_int=>create
    exporting
      seed = v_seed
      min  = 0
      max  = 9
    receiving
      prng = v_random.

  v_password1 = v_random->get_next( ).
  v_password2 = v_random->get_next( ).
  v_password3 = v_random->get_next( ).
  v_password4 = v_random->get_next( ).
  v_password5 = v_random->get_next( ).
  v_password6 = v_random->get_next( ).
  v_password7 = v_random->get_next( ).
  v_password8 = v_random->get_next( ).

endform.                    " ZF_RANDOMIZE_GAME

*&---------------------------------------------------------------------*
*&      Form  CHECK_PASSWORD_BY_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password_easy.

  data l_light type icon-name.

  perform zf_check_password1_easy changing l_light.
  perform zf_check_password2_easy changing l_light.
  perform zf_check_password3_easy changing l_light.
  perform zf_check_password4_easy changing l_light.

  if v_num1  eq v_password1
  and v_num2 eq v_password2
  and v_num3 eq v_password3
  and v_num4 eq v_password4.
    call screen 9030 starting at 5 5.
  endif.

endform.                    " CHECK_PASSWORD_BY_INPUT

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password1_easy changing p_light type icon-name.

  clear p_light.

  if v_num1 eq v_password1.
    p_light = c_green_icon.
    v_show_password1 = v_password1.
  elseif v_num1 eq v_password2
      or v_num1 eq v_password3
      or v_num1 eq v_password4.
    p_light = c_yellow_icon.
*    light = 'ICON_TOTAL_LEFT'.
  else.
    p_light = c_red_icon.
*    light = 'ICON_RED_LIGHT'.

  endif.

  perform zf_icon_create using p_light
                      changing icon1.

endform.                    " ZF_CHECK_PASSWORD1

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password2_easy changing p_light type icon-name.

  clear p_light.

  if v_num2 eq v_password2.
    p_light = c_green_icon.
    v_show_password2 = v_password2.
  elseif v_num2 eq v_password1
      or v_num2 eq v_password3
      or v_num2 eq v_password4.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon2.

endform.                    " ZF_CHECK_PASSWORD2

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password3_easy changing p_light type icon-name.

  clear p_light.

  if v_num3 eq v_password3.
    p_light = c_green_icon.
    v_show_password3 = v_password3.
  elseif v_num3 eq v_password2
      or v_num3 eq v_password1
      or v_num3 eq v_password4.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon3.

endform.                    " ZF_CHECK_PASSWORD3

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password4_easy changing p_light type icon-name.

  clear p_light.

  if v_num4 eq v_password4.
    p_light = c_green_icon.
    v_show_password4 = v_password4.
  elseif v_num4 eq v_password2
      or v_num4 eq v_password3
      or v_num4 eq v_password1.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon4.

endform.                    " ZF_CHECK_PASSWORD4

*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARE_GAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_prepare_game_easy.

  data l_light type icon-name.

  l_light = c_inactive_icon.

  perform zf_icon_create using l_light
                      changing icon1.

  perform zf_icon_create using l_light
                      changing icon2.

  perform zf_icon_create using l_light
                      changing icon3.

  perform zf_icon_create using l_light
                      changing icon4.

  clear: v_num1,v_num2,v_num3,v_num4,v_show_password1,v_show_password2,v_show_password3,v_show_password4.

endform.                    " ZF_PREPARE_GAME

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_IF_IS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_if_is_new.

  perform zf_read_code_break_table.

  perform zf_compare_if_is_new.

endform.                    " ZF_CHECK_IF_IS_NEW

*&---------------------------------------------------------------------*
*&      Form  ZF_READ_CODE_BREAK_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_CODE_BREAK  text
*----------------------------------------------------------------------*
form zf_read_code_break_table.

  clear t_code_break.

  select *
    from ztbgf_code_break
    into table t_code_break.

endform.                    " ZF_READ_CODE_BREAK_TABLE

*&---------------------------------------------------------------------*
*&      Form  ZF_COMPARE_IF_IS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CODE_BREAK  text
*----------------------------------------------------------------------*
form zf_compare_if_is_new.

  field-symbols <fs_code_break> like line of t_code_break.

  ztbgf_code_break-dificuldade = v_dif.

  read table t_code_break assigning <fs_code_break> with key cpf = ztbgf_code_break-cpf
                                                             dificuldade = v_dif.

  if sy-subrc is not initial.

    call screen 9070 starting at 5 2.

  endif.

endform.                    " ZF_COMPARE_IF_IS_NEW

*&---------------------------------------------------------------------*
*&      Form  ZF_COMPUT_SCORE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_comput_score.

  ztbgf_code_break-tentativas = v_tries.

  perform zf_read_code_break_table.

  perform zf_submit_score.

endform.                    " ZF_COMPUT_SCORE

*&---------------------------------------------------------------------*
*&      Form  ZF_SUBMIT_SCORE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_CODE_BREAK  text
*----------------------------------------------------------------------*
form zf_submit_score.

  field-symbols <fs_code_break> like line of t_code_break.

  read table t_code_break assigning <fs_code_break> with key cpf = ztbgf_code_break-cpf
                                                             dificuldade = v_dif.

  if sy-subrc is initial.

    if <fs_code_break>-tentativas is initial
    or <fs_code_break>-tentativas gt v_tries.

      update ztbgf_code_break set tentativas = v_tries
      where cpf = <fs_code_break>-cpf
      and dificuldade = v_dif.

    endif.

  endif.

endform.                    " ZF_SUBMIT_SCORE

*&---------------------------------------------------------------------*
*&      Form  ZF_BUILD_CUSTOM_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_build_custom_container.

  data: lw_variant type disvariant,
        l_repid type sy-repid.

  l_repid = sy-subrc.

  create object v_custom_container
    exporting
      container_name = 'ALVGRID'.

  create object v_grid
    exporting
      i_parent = v_custom_container.

  lw_variant-report    = l_repid.

  call method v_grid->set_table_for_first_display
    exporting
      is_variant       = lw_variant
      i_structure_name = 'ZSTGF_CODE_BREAK'
    changing
      it_outtab        = t_code_break.


endform.                    " ZF_BUILD_CUSTOM_CONTAINER

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD_MEDIUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password_medium.

  data l_light type icon-name.

  perform zf_check_password1_medium changing l_light.
  perform zf_check_password2_medium changing l_light.
  perform zf_check_password3_medium changing l_light.
  perform zf_check_password4_medium changing l_light.
  perform zf_check_password5_medium changing l_light.
  perform zf_check_password6_medium changing l_light.

  if v_num1 eq v_password1
  and v_num2 eq v_password2
  and v_num3 eq v_password3
  and v_num4 eq v_password4
  and v_num5 eq v_password5
  and v_num6 eq v_password6.
    call screen 9030 starting at 5 5.
  endif.

endform.                    " ZF_CHECK_PASSWORD_MEDIUM

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD1_MEDIUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password1_medium changing p_light type icon-name.

  clear p_light.

  if v_num1 eq v_password1.
    p_light = c_green_icon.
    v_show_password1 = v_password1.
  elseif v_num1 eq v_password2
      or v_num1 eq v_password3
      or v_num1 eq v_password4
      or v_num1 eq v_password5
      or v_num1 eq v_password6.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon1.

endform.                    " ZF_CHECK_PASSWORD1_MEDIUM

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD2_MEDIUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password2_medium changing p_light type icon-name.

  clear p_light.

  if v_num2 eq v_password2.
    p_light = c_green_icon.
    v_show_password2 = v_password2.
  elseif v_num2 eq v_password1
      or v_num2 eq v_password3
      or v_num2 eq v_password4
      or v_num2 eq v_password5
      or v_num2 eq v_password6.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon2.

endform.                    " ZF_CHECK_PASSWORD2_MEDIUM

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD3_MEDIUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password3_medium changing p_light type icon-name.

  clear p_light.

  if v_num3 eq v_password3.
    p_light = c_green_icon.
    v_show_password3 = v_password3.
  elseif v_num3 eq v_password2
      or v_num3 eq v_password1
      or v_num3 eq v_password4
      or v_num3 eq v_password5
      or v_num3 eq v_password6.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon3.

endform.                    " ZF_CHECK_PASSWORD3_MEDIUM

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD4_MEDIUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password4_medium changing p_light type icon-name.

  clear p_light.

  if v_num4 eq v_password4.
    p_light = c_green_icon.
    v_show_password4 = v_password4.
  elseif v_num4 eq v_password2
      or v_num4 eq v_password3
      or v_num4 eq v_password1
      or v_num4 eq v_password5
      or v_num4 eq v_password6.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon4.

endform.                    " ZF_CHECK_PASSWORD4_MEDIUM

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD5_MEDIUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password5_medium changing p_light type icon-name.

  clear p_light.

  if v_num5 eq v_password5.
    p_light = c_green_icon.
    v_show_password5 = v_password5.
  elseif v_num5 eq v_password2
      or v_num5 eq v_password3
      or v_num5 eq v_password4
      or v_num5 eq v_password1
      or v_num5 eq v_password6.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon5.
endform.                    " ZF_CHECK_PASSWORD5_MEDIUM

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD6_MEDIUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password6_medium changing p_light type icon-name.

  clear p_light.

  if v_num6 eq v_password6.
    p_light = c_green_icon.
    v_show_password6 = v_password6.
  elseif v_num6 eq v_password6
      or v_num6 eq v_password3
      or v_num6 eq v_password4
      or v_num6 eq v_password5
      or v_num6 eq v_password1.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon6.

endform.                    " ZF_CHECK_PASSWORD6_MEDIUM

*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARE_GAME_MEDIUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_prepare_game_medium.

  data l_light type icon-name.

  l_light = c_inactive_icon.

  perform zf_icon_create using l_light
                      changing icon1.

  perform zf_icon_create using l_light
                      changing icon2.

  perform zf_icon_create using l_light
                      changing icon3.

  perform zf_icon_create using l_light
                      changing icon4.

  perform zf_icon_create using l_light
                      changing icon5.

  perform zf_icon_create using l_light
                      changing icon6.

  clear: v_num1,v_num2,v_num3,v_num4,v_num6,
  v_show_password1,v_show_password2,v_show_password3,v_show_password4,v_show_password5,v_show_password6.

endform.                    " ZF_PREPARE_GAME_MEDIUM

*&---------------------------------------------------------------------*
*&      Form  ZF_CHANGE_SCREEN_MEDIUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_change_screen_medium.
  call screen 9050.
endform.                    " ZF_CHANGE_SCREEN_MEDIUM

*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARE_GAME_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_prepare_game_hard.

  data l_light type icon-name.

  l_light = c_inactive_icon.

  perform zf_icon_create using l_light
                      changing icon1.

  perform zf_icon_create using l_light
                      changing icon2.

  perform zf_icon_create using l_light
                      changing icon3.

  perform zf_icon_create using l_light
                      changing icon4.

  perform zf_icon_create using l_light
                      changing icon5.

  perform zf_icon_create using l_light
                      changing icon6.

  perform zf_icon_create using l_light
                      changing icon7.

  perform zf_icon_create using l_light
                      changing icon8.

  clear: v_num1,v_num2,v_num3,v_num4,v_num5,v_num6,v_num7,v_num8,
  v_show_password1,v_show_password2,v_show_password3,v_show_password4,v_show_password5,v_show_password6,v_show_password7,v_show_password8.

endform.                    " ZF_PREPARE_GAME_HARD

*&---------------------------------------------------------------------*
*&      Form  ZF_CHANGE_SCREEN_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_change_screen_hard.
  call screen 9060.
endform.                    " ZF_CHANGE_SCREEN_HARD

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password_hard.

  data l_light type icon-name.

  perform zf_check_password1_hard changing l_light.
  perform zf_check_password2_hard changing l_light.
  perform zf_check_password3_hard changing l_light.
  perform zf_check_password4_hard changing l_light.
  perform zf_check_password5_hard changing l_light.
  perform zf_check_password6_hard changing l_light.
  perform zf_check_password7_hard changing l_light.
  perform zf_check_password8_hard changing l_light.

  if v_num1 eq v_password1
  and v_num2 eq v_password2
  and v_num3 eq v_password3
  and v_num4 eq v_password4
  and v_num5 eq v_password5
  and v_num6 eq v_password6
  and v_num7 eq v_password7
  and v_num8 eq v_password8.
    call screen 9030 starting at 5 5.
  endif.

endform.                    " ZF_CHECK_PASSWORD_HARD

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD1_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password1_hard changing p_light type icon-name.

  clear p_light.

  if v_num1 eq v_password1.
    p_light = c_green_icon.
    v_show_password1 = v_password1.
  elseif v_num1 eq v_password2
      or v_num1 eq v_password3
      or v_num1 eq v_password4
      or v_num1 eq v_password5
      or v_num1 eq v_password6
      or v_num1 eq v_password7
      or v_num1 eq v_password8.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon1.

endform.                    " ZF_CHECK_PASSWORD1_HARD

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD2_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password2_hard changing p_light type icon-name.

  clear p_light.

  if v_num2 eq v_password2.
    p_light = c_green_icon.
    v_show_password2 = v_password2.
  elseif v_num2 eq v_password1
      or v_num2 eq v_password3
      or v_num2 eq v_password4
      or v_num2 eq v_password5
      or v_num2 eq v_password6
      or v_num2 eq v_password7
      or v_num2 eq v_password8.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon2.

endform.                    " ZF_CHECK_PASSWORD2_HARD

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD3_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password3_hard changing p_light type icon-name.

  clear p_light.

  if v_num3 eq v_password3.
    p_light = c_green_icon.
    v_show_password3 = v_password3.
  elseif v_num3 eq v_password2
      or v_num3 eq v_password1
      or v_num3 eq v_password4
      or v_num3 eq v_password5
      or v_num3 eq v_password6
      or v_num3 eq v_password7
      or v_num3 eq v_password8.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon3.

endform.                    " ZF_CHECK_PASSWORD3_HARD

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD4_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password4_hard changing p_light type icon-name.

  clear p_light.

  if v_num4 eq v_password4.
    p_light = c_green_icon.
    v_show_password4 = v_password4.
  elseif v_num4 eq v_password2
      or v_num4 eq v_password3
      or v_num4 eq v_password1
      or v_num4 eq v_password5
      or v_num4 eq v_password6
      or v_num4 eq v_password7
      or v_num4 eq v_password8.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon4.

endform.                    " ZF_CHECK_PASSWORD4_HARD

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD5_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password5_hard changing p_light type icon-name.

  clear p_light.

  if v_num5 eq v_password5.
    p_light = c_green_icon.
    v_show_password5 = v_password5.
  elseif v_num5 eq v_password2
      or v_num5 eq v_password3
      or v_num5 eq v_password4
      or v_num5 eq v_password1
      or v_num5 eq v_password6
      or v_num5 eq v_password7
      or v_num5 eq v_password8.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon5.

endform.                    " ZF_CHECK_PASSWORD5_HARD

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD6_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password6_hard changing p_light type icon-name.

  clear p_light.

  if v_num6 eq v_password6.
    p_light = c_green_icon.
    v_show_password6 = v_password6.
  elseif v_num6 eq v_password2
      or v_num6 eq v_password3
      or v_num6 eq v_password4
      or v_num6 eq v_password5
      or v_num6 eq v_password1
      or v_num6 eq v_password7
      or v_num6 eq v_password8.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon6.

endform.                    " ZF_CHECK_PASSWORD6_HARD

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD7_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password7_hard changing p_light type icon-name.

  clear p_light.

  if v_num7 eq v_password7.
    p_light = c_green_icon.
    v_show_password7 = v_password7.
  elseif v_num7 eq v_password2
      or v_num7 eq v_password3
      or v_num7 eq v_password4
      or v_num7 eq v_password5
      or v_num7 eq v_password6
      or v_num7 eq v_password1
      or v_num7 eq v_password8.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon7.

endform.                    " ZF_CHECK_PASSWORD7_HARD

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_PASSWORD8_HARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_check_password8_hard changing p_light type icon-name.

  clear p_light.

  if v_num8 eq v_password8.
    p_light = c_green_icon.
    v_show_password8 = v_password8.
  elseif v_num8 eq v_password2
      or v_num8 eq v_password3
      or v_num8 eq v_password4
      or v_num8 eq v_password5
      or v_num8 eq v_password6
      or v_num8 eq v_password7
      or v_num8 eq v_password1.
    p_light = c_yellow_icon.
  else.
    p_light = c_red_icon.
  endif.

  perform zf_icon_create using p_light
                      changing icon8.

endform.                    " ZF_CHECK_PASSWORD8_HARD
*&---------------------------------------------------------------------*
*&      Form  ZF_ICON_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_LIGHT  text
*      <--P_ICON1  text
*----------------------------------------------------------------------*
form zf_icon_create using p_light type icon-name
                 changing p_icon type icons-text.

  call function 'ICON_CREATE'
    exporting
      name   = p_light
    importing
      result = p_icon.

endform.                    " ZF_ICON_CREATE
*&---------------------------------------------------------------------*
*&      Form  ZF_READ_TABLE_BY_DIF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_read_table_by_dif.

  clear t_code_break.

  select *
    from ztbgf_code_break
    into table t_code_break
    where dificuldade eq v_dif.


endform.                    " ZF_READ_TABLE_BY_DIF
*&---------------------------------------------------------------------*
*&      Form  ZF_GARANTEE_CREATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_garantee_creation.

  perform zf_read_code_break_table.

  perform zf_compare_if_created.

endform.                    " ZF_GARANTEE_CREATION
*&---------------------------------------------------------------------*
*&      Form  ZF_COMPARE_IF_CREATED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_compare_if_created.

  field-symbols <fs_code_break> like line of t_code_break.

  read table t_code_break assigning <fs_code_break> with key cpf = ztbgf_code_break-cpf
                                                             dificuldade = v_dif.

  if sy-subrc is not initial.
    leave to screen 9000.
  endif.

endform.                    " ZF_COMPARE_IF_CREATED
