package me.shadaj.scalapy.readwrite

import me.shadaj.scalapy.interpreter.PyValue
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.FacadeCreator

import me.shadaj.scalapy.interpreter.Platform
import me.shadaj.scalapy.interpreter.CPythonInterpreter
import me.shadaj.scalapy.interpreter.CPythonAPI

trait FacadeReader {
    implicit def facadeReader[F <: py.Any](implicit creator: FacadeCreator[F]): Reader[F] = new Reader[F] {
    override def readNative(r: Platform.Pointer): F = creator.create(PyValue.fromBorrowed(r))
  }
}