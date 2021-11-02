package me.shadaj.scalapy.py

import scala.collection.mutable

import org.scalatest.funsuite.AnyFunSuite

import me.shadaj.scalapy.interpreter.PyValue
import me.shadaj.scalapy.readwrite.{Reader, Writer}

@native class NdArray[T](val rawValue: PyValue)(implicit reader: Reader[T]) extends Object {
  def size: Int = native
}

object NdArray {
  implicit def reader[T](implicit reader: Reader[T]): Reader[NdArray[T]] = new Reader[NdArray[T]] {
    override def read(v: PyValue): NdArray[T] = new NdArray[T](v)(reader)
  }
}

class MyArray[T](rawValue: PyValue)(implicit reader: Reader[T]) extends NdArray[T](rawValue) {
  override def size: Int = 3
}

object MyArray {
  implicit def reader[T](implicit reader: Reader[T]): Reader[MyArray[T]] = new Reader[MyArray[T]] {
    override def read(v: PyValue): MyArray[T] = new MyArray[T](v)(reader)
  }
}

class ScalaPyDefinedTest extends AnyFunSuite {
  test("Gets exception when running Python fails") {
    local {
      val np = module("numpy")
      
      val a = np.ndarray(Seq(1, 2).toPythonProxy).as[NdArray[Int]]
      val b = np.ndarray(Seq(1, 2).toPythonProxy).as[MyArray[Int]]

      println(np.apply_along_axis(((x: NdArray[Int]) => x.size), 1, a)) // [2]
      println(np.apply_along_axis(((x: NdArray[Int]) => x.size), 1, b)) // expect: [3], actual: [2]

      val types = module("types")
      def clsexec = (ns: mutable.Map[Any, Any]) => {ns += (Any.from("size") -> Any.from(3)); Unit}
      val ndarrayClass = py"np.ndarray"
      val myarray = types.new_class("myarray", Seq(np.ndarray,), exec_body = clsexec)

    }
  }
}
