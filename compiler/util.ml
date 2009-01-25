let exit_with_code code =
  let code = if !Config.test_mode then 0 else code in
    exit code
