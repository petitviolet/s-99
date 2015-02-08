# s99 のtestを走らせる
scalac -cp scalatest_2.11-2.1.5.jar s99.scala S99Spec.scala;
scala -cp scalatest_2.11-2.1.5.jar org.scalatest.run S99Spec
