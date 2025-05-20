(* open OUnit2
open P2b.Tree
open QCheck

let test_mirror_twice =
  Test.make
    ~name:"test_mirror_twice"
    ~count:25
    (int tree)
    (fun x -> mirror (mirror x) = x)

let suite =
  "secret" >::: [
    QCheck_runner.to_ounit2_test test_mirror_twice;
  ]

let _ = run_test_tt_main suite *)