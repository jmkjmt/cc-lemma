  (declare-datatypes () ((MyBool (MyTrue) (MyFalse))))
  (declare-datatypes () ((Unit (Null))))
  (declare-datatypes () ((List (Cons (proj_Cons_0 MyBool) (proj_Cons_1 List)) (Nil (proj_Nil_0 Unit)))))
  (declare-fun tf1 (List List) List)
  (declare-fun tf0 (List) List)
  (declare-datatypes () ((Nat (Zero) (Succ (proj_Succ_0 Nat)))))
  (declare-fun tf2 (List) Nat)
  (declare-fun singlepass (List) Nat)
  (declare-fun plus (Nat Nat) Nat)
  (declare-fun ite1 (MyBool Nat Nat) Nat)
  (declare-fun tf4 (Nat List) Nat)
  (declare-fun tf3 (Nat List) Nat)
  (declare-fun dropwhile (List) Nat)
  (declare-fun main (List) Nat)
  (declare-fun mynot (MyBool) MyBool)
  (declare-fun tf6 (List) Nat)
  (declare-fun tf5 (List) Nat)
  (declare-fun tf7 (List) Nat)
  (declare-fun singlepassNew (List) Nat)
  (declare-fun mainNew (List) Nat)
  (assert (forall ((tv5 Unit) (tv4 List)) (= (tf1 tv4 (Nil tv5)) tv4)))
  (assert (forall ((tv7 List) (tv6 MyBool) (tv4 List)) (= (tf1 tv4 (Cons tv6 tv7)) (Cons tv6 (tf0 tv7)))))
  (assert (forall ((tv2 List)) (= (tf0 tv2) (tf1 tv2 tv2))))
  (assert (forall ((tv9 List)) (= (tf2 tv9) (dropwhile (tf0 tv9)))))
  (assert (forall ((tv1 List)) (= (singlepass tv1) (tf2 tv1))))
  (assert (forall ((x Nat)) (= (plus Zero x) x)))
  (assert (forall ((y Nat) (x Nat)) (= (plus (Succ x) y) (Succ (plus x y)))))
  (assert (forall ((y Nat) (x Nat)) (= (ite1 MyTrue x y) x)))
  (assert (forall ((y Nat) (x Nat)) (= (ite1 MyFalse x y) y)))
  (assert (forall ((tv15 Unit) (tv14 Nat)) (= (tf4 tv14 (Nil tv15)) tv14)))
  (assert (forall ((tv17 List) (tv16 MyBool) (tv14 Nat)) (= (tf4 tv14 (Cons tv16 tv17)) (ite1 tv16 tv14 (tf3 (plus (Succ Zero) tv14) tv17)))))
  (assert (forall ((tv12 List) (tv11 Nat)) (= (tf3 tv11 tv12) (tf4 tv11 tv12))))
  (assert (forall ((tv10 List)) (= (dropwhile tv10) (tf3 Zero tv10))))
  (assert (forall ((tv18 List)) (= (main tv18) (singlepass tv18))))
  (assert (= (mynot MyTrue) MyFalse))
  (assert (= (mynot MyFalse) MyTrue))
  (assert (forall ((tv23 Unit)) (= (tf6 (Nil tv23)) Zero)))
  (assert (forall ((tv25 List) (tv24 MyBool)) (= (tf6 (Cons tv24 tv25)) (ite1 (mynot tv24) (plus (Succ Zero) (tf5 tv25)) Zero))))
  (assert (forall ((tv21 List)) (= (tf5 tv21) (tf6 tv21))))
  (assert (forall ((tv26 List)) (= (tf7 tv26) (tf5 tv26))))
  (assert (forall ((tv20 List)) (= (singlepassNew tv20) (tf7 tv20))))
  (assert (forall ((tv27 List)) (= (mainNew tv27) (singlepassNew tv27))))
  (assert (not (forall ((inp0 List)) (= (main inp0) (mainNew inp0)))))
  (check-sat)
