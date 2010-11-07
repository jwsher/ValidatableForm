package com.scalabb {

  import org.specs._
  import org.specs.runner.JUnit4
  import org.specs.runner.ConsoleRunner
  import org.specs.matcher._
  import org.specs.specification._
  import java.util.Properties
  import _root_.junit.framework._
  import Assert._
  import java.io.File
  import com.scalabb.validation._
/*
 * Copyright 2010 Justin Sher
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
  object ValidationSpecs extends Specification {

    "Be Able to make a single element form with a required field and validate it" in {
      val f = new ValidatableForm((e) => {}, "name" ->
        new ValidString(List(new MinLength(3), new Required)))
      f.submit(Map("name" -> "f"))
      assert(!f("name").isValid)
      assert(!f.isValid)
      f.submit(Map("name" -> "foo"))
      assert(f("name").isValid)
      assert(f("name").toModel.equals("foo"))
      assert(f.isValid)
    }
    "Be Able to make a single element form with a non required field and validate it" in {
      val f = new ValidatableForm((e) => {}, "name" ->
        new ValidString(List(new MinLength(3))))
      f.submit(Map("name" -> "f"))
      assert(!f("name").isValid)
      assert(!f.isValid)
      f.submit(Map("name" -> "foo"))
      assert(f("name").isValid)
      assert(f("name").toModel.equals("foo"))
      assert(f.isValid)

      f.submit(Map("other" -> "f"))
      assert(f.isValid)
      assert(f("name").isValid)

      f.submit(Map("name" -> ""))
      assert(f.isValid)
      assert(f("name").isValid)
    }

    "Be Able to validate via regex" in {
      val f = new ValidatableForm((e) => {}, "zipcode" ->
        new ValidString(List(new ValidRegex("^([0-9]{5})$"), new Required)))
      f.submit(Map("zipcode" -> "a12345"))
      assert(!f("zipcode").isValid)
      f.submit(Map("zipcode" -> "01234"))
      assert(f("zipcode").isValid)
    }
    "Be Able to validate an integer field" in {
      val f = new ValidatableForm((e) => {}, "quantity" ->
        new ValidInteger(List(new Minimum(1), new Maximum(10), new Required)))
      val n = "quantity";
      f.submit(Map(n -> "f"))
      assert(!f(n).isValid)
      assert(!f.isValid)
      f.submit(Map(n -> "0"))
      assert(!f(n).isValid)
      assert(!f.isValid)
      f.submit(Map(n -> "11"))
      assert(!f(n).isValid)

      f.submit(Map(n -> "1"))
      assert(f(n).isValid)

      assert(f(n).toModel.equals(1))
      assert(f.isValid)
    }
    "Be able to perform form level validations" in {
      val f = new ValidatableForm((e) => {},
        "password" ->
        new ValidString(List(new MinLength(6),
          new MaxLength(8),
          new Required)),
        "password2" ->
        new ValidString(List(new MinLength(6),
          new MaxLength(8),
          new Required))).formValidations(
        new FieldsMatch(List("password", "password2")))

      f.submit(Map("password" -> "foobar",
        "password2" -> "foobarfoobar"))
      assert(!f.isValid)
      assert(f("password").isValid)
      assert(!f("password2").isValid)

      f.submit(Map("password" -> "foobar",
        "password2" -> "foobar1"))

      assert(!f.isValid)
      assert(f("password").isValid)
      assert(f("password2").isValid)

      f.submit(Map("password" -> "foobar",
        "password2" -> "foobar"))

      assert(f.isValid)
      assert(f("password").isValid)
      assert(f("password2").isValid)

      f.submit(Map("password" -> "foo",
        "password2" -> "foo"))

      assert(!f.isValid)
      assert(!f("password").isValid)
      assert(!f("password2").isValid)
    }
    "Be able to perform form host/port validations" in {
      val f = new ValidatableForm((e) => {},
        "host" ->
        new ValidHostname(List(new Required)),
        "port" ->
        new ValidPort(List(new Required)))
      f.submit(Map("host" -> "!!"))
      assert(!f.isValid)

      f.submit(Map("port" -> "9999"))
      f.submit(Map("host" -> "127.0.0.1"))
      assert(f.isValid)

      f.submit(Map("port" -> "99999"))
      assert(!f.isValid)

      f.submit(Map("port" -> "0"))
      assert(!f.isValid)

      f.submit(Map("port" -> "9999"))
      assert(f.isValid)

      f.submit(Map("host" -> "www.yahoo.com"))
      assert(f.isValid)
    }

  }

  class myValidationTest extends JUnit4(ValidationSpecs)
}