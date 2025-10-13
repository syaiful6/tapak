let () =
  Alcotest.run
    "Kernel Tests"
    (List.flatten [ Test_router.tests; Test_form.tests ])
