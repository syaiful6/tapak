let () =
  Alcotest.run
    "Kernel Tests"
    (List.flatten [ Test_form.tests; [ Test_router.tests ] ])
