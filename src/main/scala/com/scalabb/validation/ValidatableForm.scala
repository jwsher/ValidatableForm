package com.scalabb.validation

import scala.collection.immutable.Map
import scala.util.matching.Regex
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

/**
 * @param v The field that contains the error.  Emtpy if it's an error for the whole form
 * @param defaultMsg The default error message if there is no locale specific message
 * @param i8nCode a standardized error code for use with locale specific messages
 * @param msgArgs for use with message format based i8n.
 * 
 * This class holds an error that occurs in a form element or an entire form if no specific element is specified
 */
case class FormError(v: Option[ValidField[_ <: Any]], defaultMsg: String, i18nCode: String, msgArgs: List[String])


/**
 * The abtract	 superclass for all validatable form fields
 * @author Justin Sher (justin@sher.net)
 */
abstract class ValidField[O](
  validators: List[Validator[O]]) {

  var form: ValidatableForm = _
  var parseExcetion: Exception = _
  def toModel: O
  var name: String = _
  
  /** Returns Some(string) or Empty if the value is emptye **/
  def stringOpt: Option[String] = { if (isEmpty) None else Some(toString()) }
  /**
   * @param v the default value 
   * Returns a string or the supplied default passed if the value is empty 
   * **/
  def stringOpt(v: String): String = { if (isEmpty) v else toString() }

  override def toString: String = { "" }

  /**
   * Turns the field's string value into an array or an empty list if empty
   */
  def toStringArr: Seq[String] = {
    toString match {
      case "" => List()
      case x => List(x)
    }
  }
  
  /**
   * Whether the value is empty
   */
  def isEmpty: Boolean

  /**
   * gets form specific errors related to this field.
   */
  def formErrors(): List[FormError] = {
    for (v <- validate) yield (v.formError(Some(this)))
  }
  
  /**
   * validates this field, setting the model value and producing errors 
   */
  def validate(): List[Validator[O]] = {
    val req = handleRequired
    System.out.println("Req " + req.mkString(",") + " Val " + validators.mkString(",") + " this " + this);
    if (req.isEmpty && isEmpty) { return List(); }
    if (!req.isEmpty && isEmpty) { return req; }

    for (
      a <- validators ::: req ::: classValidators if !a.validate(toModel)
    ) yield (a)
  }

  /**
   * Any class validators, for the form as a whole are provided here
   */
  def classValidators: List[Validator[O]] = {
    List()
  }
  
  /**
   * Does validation for required values
   */
  def handleRequired: List[Validator[O]] = {
    validators foreach (a => a match {
      case x: Required[O] => {
        x.setValid(!isEmpty)
        return List(x);
      }
      case _ => Nil
    })
    return List();
  }
  
  /**
   * returns true if valid, otherwise false
   */
  def isValid: Boolean = {
    val failedValidations = validate
    for (v <- failedValidations) {
      System.out.println("Failed : " + v);
    }
    failedValidations.length == 0
  }
  
  /**
   * List of errors after the field is validated
   */
  def errors: List[String] = {
    for (v <- validate) yield (v.error)
  }
  
  /**
   * Submit a value into the field
   */
  def submit(view: String): Unit
  
  /**
   * Clear the field's value
   */
  def clear: Unit
  
  /**
   * Submit the first value of a list
   **/
  def submit(view: List[String]): Unit = {
    if (view.size > 0) {
      submit(view(0))
    }
  }
}

/**
 * Class to validate fields that can only be valid if they are an Integer.
 * @author Justin Sher (justin@sher.net)
 */
class ValidInteger(validators: List[Validator[Integer]]) extends ValidField[Integer](validators) {

 /**
  * The model value, once successfully submitted
  */
  var value: Integer = null

  /**
   * clear the value
   */
  def clear: Unit = { value = null }

  /**
   * To string for integer, empty string if empty
   */
  override def toString: String = {
    if (isEmpty) { "" } else { value.toString() }
  }

  /**
   * returns model type
   */
  def toModel: Integer = {
    value;
  }

  /**
   * Checks if the value is empty
   */
  def isEmpty: Boolean = {
    value == null
  }

  /**
   * Submit an integer, ignore if it doesn't parse
   */
  def submit(view: String) = {
    try {
      if (view.length() > 0)
        value = Integer.parseInt(view);
    } catch {
      case nfe: NumberFormatException => {}
    }
  }
}

/**
 * @param key/value tuple sequence for multiple choice fields.  Valid model values are first tuple value 
 * Validator for multiple choice questions
 * @author Justin Sher (justin@sher.net)
 */
class ContainsOptionValidator(valid: Seq[(String, String)]) extends Validator[String] {

	/**
	 * returns true if it finds a valid value
	 */
	def validate(obj: String): Boolean = {
    valid.filter((k) => { k._1.equals(obj) }).size > 0;
  }
	/**
	 * default error message
	 */
  def error(): String = {
    "Invalid value"
  }
  /**
   * i18n error message code
   */
  def i18nError(): String = {
    "value.contains-option"
  }
  
  /**
   * No message args
   */
  def msgArgs(): List[String] = List();

}

/**
 * @param key/value tuple sequence for multiple choice fields.  Valid model values are first tuple value
 * @param validators, any custom validators for this field 
 * Validatable field for multiple choice questions
 * @author Justin Sher (justin@sher.net)
 */
class ValidOption(validOptions: Seq[(String, String)], validators: List[Validator[String]]) extends ValidField[String](validators) {

	/**
	 * Model value is a string
	 */
	var value: String = null;

	/**
	 * Clear value
	 */
  def clear: Unit = { value = null }

  /**
   * returns model after validation, a string
   */
  def toModel: String = {
    value;
  }
  
  /**
   * returns empty string if invalid, otherwise model value
   */
  override def toString: String = {
    if (isEmpty) { return ""; } else {
      return value
    }
  }

  /**
   * Class default validator checks if the option is contained in the set
   */
  override def classValidators: List[Validator[String]] = {
    List(new ContainsOptionValidator(validOptions))
  }
  
  /**
   * submits value, accepts any string, though it won't necessarily be valid
   */
  def submit(view: String) = {
    value = view;
  }
  
  /**
   * whether the field is empty or not
   */
  def isEmpty: Boolean = {
    value == null || value.length == 0
  }
}

/**
 * Class for whether something is a valid boolean.  If required must be checked
 * @param validators, any custom validators for this field 
 */
class ValidBoolean(validators: List[Validator[Boolean]])
  extends ValidField[Boolean](validators) {
  var value: java.lang.Boolean = null;

  def clear: Unit = { value = null }

  override def toString: String = {
    if (isEmpty) { "" } else { value.toString() }
  }

  def toModel: Boolean = {
    value.booleanValue;
  }

  def submit(view: String) = {
    value = if ("true".equals(view))
      true else if ("false".equals(view)) false
    else null
  }
  def isEmpty: Boolean = {
    value == null;
  }
}

/**
 * @param validators, any custom validators for this field 
 * Class for valid strings.  Anything passes, must add validators to get interesting behaviour
 * @author Justin Sher (justin@sher.net)
 */
class ValidString(validators: List[Validator[String]])
  extends ValidField[String](validators) {
  var value: String = null;
  def clear: Unit = { value = null }

  override def toString: String = {
    if (isEmpty) { "" } else { value.toString() }
  }

  def toModel: String = {
    value;
  }

  def submit(view: String) = {
    value = view;
  }
  def isEmpty: Boolean = {
    value == null || value.length == 0
  }
}

/**
 * @param validators, any custom validators for this field 
 * Class for valid strings.  Anything passes, must add validators to get interesting behaviour
 * Makes value lowercase when submitted
 * @author Justin Sher (justin@sher.net)
 */
class ValidLowercaseString(validators: List[Validator[String]])
  extends ValidString(validators) {

  override def submit(view: String) = {
    value = view.toLowerCase;
  }
}

/**
 * Abstract class for validators, which are used to validate a field or form of a specific model type
 */
abstract class Validator[S] {
  def validate(obj: S): Boolean;
  def error(): String
  def i18nError(): String
  def msgArgs(): List[String]
  def formError(v: Option[ValidField[_ <: Any]]): FormError = {
    return new FormError(v, error, i18nError, msgArgs);
  }
}

/**
 * will validate if any one condition in a list is true
 */
class OrValidator[T](subValidators: List[Validator[T]]) extends Validator[T] {

  def validate(obj: T): Boolean = {
    val valid = (for (
      v <- subValidators if v.validate(obj)
    ) yield (v));
    valid.size > 0

  }
  def error(): String = {
    "Value is invalid"
  }
  def i18nError(): String = {
    "value.invalid"
  }
  def msgArgs(): List[String] = List();

}

/**
 * A validator that requires that a field be not empty
 */
class Required[Any] extends Validator[Any] {
  var valid: Boolean = _
  def setValid(b: Boolean) {
    valid = b
  }
  def validate(obj: Any): Boolean = {
    return valid
  }
  def error(): String = {
    "is required"
  }
  def i18nError(): String = {
    "value.required"
  }
  def msgArgs(): List[String] = List();

}

/**
 * @param min the minimum value for an integer field
 * A validator that requires that an integer value be larger than or equal to a minimum 
 */
class Minimum(min: Integer) extends Validator[Integer] {
  def validate(obj: Integer): Boolean = {
    obj.intValue >= min.intValue
  }
  def error(): String = {
    "Value must be larger than " + (min.intValue - 1)
  }
  def i18nError(): String = {
    "value.too-small"
  }
  def msgArgs(): List[String] = List(min.toString());

}

/**
 * @param max the maximum value for an integer field
 * A validator that requires that an integer value be less than or equal to  maximum 
 */
class Maximum(max: Integer) extends Validator[Integer] {
  def validate(obj: Integer): Boolean = {
    obj.intValue <= max.intValue
  }
  def error(): String = {
    "Value must be smaller than " + (max.intValue + 1)
  }
  def i18nError(): String = {
    "value.too-big"
  }
  def msgArgs(): List[String] = List(max.toString());

}

/**
 * @param reg a regex that the validator will check
 * A validator that requires that a string value match a regex 
 */
class ValidRegex(reg: String) extends Validator[String] {
  def validate(obj: String): Boolean = {
    reg.r.findFirstIn(obj) match {
      case Some(_) => return true;
      case None => return false;
    }
  }

  def error(): String = {
    "Does not match required format"
  }
  def i18nError(): String = {
    "value.invalid-format"
  }
  def msgArgs(): List[String] = List();

}

/**
 * @param minLength the required minimum length of a string
 * A validator that requires that a string value be of a minimum length
 */
class MinLength(minLength: Int) extends Validator[String] {
  def validate(obj: String): Boolean = {
    obj.trim().length >= minLength
  }
  def error(): String = {
    "Value must be greater than or equal to " + minLength + " characters"
  }
  def i18nError(): String = {
    "value.too-short"
  }
  def msgArgs(): List[String] = List(minLength.toString());

}


/**
 * @param maxLength the required maximum length of a string
 * A validator that requires that a string value be of less than a maximum length
 */
class MaxLength(maxLength: Int) extends Validator[String] {
  def validate(obj: String): Boolean = {
    obj.trim().length <= maxLength
  }
  def error(): String = {
    "Value must be less than or equal to " + maxLength + " characters"
  }
  def i18nError(): String = {
    "value.too-long"
  }
  def msgArgs(): List[String] = List(maxLength.toString());
}

/**
 * @param handleErr Closure that takes a list of form errors, this is where custom error reporting happens
 * @param formSpec a tuple of form fields with the keys being the names of the fields and the values being valid fields
 * 
 * This is the class that holds form fields as a group.  You create one of these and then validate your form data against it.
 * It handles mutli-field validation, for instance verifying that a password and a confirm password match
 */
class ValidatableForm(handleErr: (List[FormError]) => Unit, formSpec: Tuple2[String, ValidField[_ <: Any]]*) {
  val formMap: Map[String, ValidField[_ <: Any]] = formSpec.toMap
  formSpec foreach ((k) => { k._2.form = this; k._2.name = k._1; })

  var formValid: List[Validator[ValidatableForm]] = List()
  var errorFields: Iterable[(String, ValidField[_ <: Any])] = List()
  var formErrorFields: Iterable[Validator[_ <: ValidatableForm]] = List()

  /**
   * Clear all form field values
   */
  def clear = {
    formMap.values.foreach(x => { x.clear })
  }

/**
 * Validations that apply to multiple form fields
 */
  def formValidations(valid: Validator[ValidatableForm]*): ValidatableForm = {
    formValid = valid.toList;
    return this
  }

  /**
   * Submit all fields in the map to the corresponding named valid fields, ignores extra fields 
   */
  def submit(parms: Map[String, String]): Boolean = {
    parms foreach ((k) =>
      try {
        formMap(k._1).submit(k._2)
      } catch {
        case x: NoSuchElementException => {} //Ignore Extra Keys
      })
    val valid = isValid
    valid
  }
  
  /**
   * Returns any form level errors
   */
  def formErrors(): List[FormError] = {
    for (v <- formValid) yield (v.formError(None))
  }


  /**
   * Get a particular form field out of the form by name
   */
  def apply(parm: String): ValidField[_ <: Any] = {
    return formMap(parm)
  }

  /**
   * Whether the form as a whole is valid or not
   */
  def isValid(): Boolean = {
    errorFields = for (
      k <- formSpec if (!k._2.isValid)
    ) yield (k)
    if (!errorFields.isEmpty) {
      handleErr(errorFields.flatMap((e) => e._2.formErrors).toList)
      return false;
    }
    formErrorFields = for (k <- formValid if (!k.validate(this))) yield (k)
    var valid = (formErrorFields.isEmpty)
    if (!valid) {
      handleErr(formErrors)
    }

    return valid
  }
}

/**
 * @param fields list of fields that should all match
 * Form level validation to check if two fields in the form match
 */
class FieldsMatch(fields: List[String]) extends Validator[ValidatableForm] {
  def validate(obj: ValidatableForm): Boolean = {
    var field: Option[ValidField[_ <: Any]] = None
    for (f <- fields) {
      field match {
        case None =>
          field = Some(obj(f))
        case Some(x) => {
          val field: Any = obj(f).toModel;
          val prev: Any = x.toModel;
          if (!prev.equals(field)) return false
        }
      }
    }
    return true
  }
  def error(): String = {
    "Values must match"
  }

  def i18nError(): String = {
    "value.must-match"
  }
  def msgArgs(): List[String] = List();

}

/**
 * @param validators, any custom validators for this field 
 * Checks that a string is a valid hostname
 */
class ValidHostname(validators: List[Validator[String]])
  extends ValidLowercaseString(validators) {
  override def classValidators: List[Validator[String]] = {
    return List(new OrValidator[String](List(
      new ValidRegex("^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"),
      new ValidRegex("^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)*([A-Za-z]|[A-Za-z][A-Za-z0-9\\-]*[A-Za-z0-9])$"))));

  }
}

/**
 * @param validators, any custom validators for this field 
 * Checks that a string is a valid email
 */
class ValidEmail(validators: List[Validator[String]])
  extends ValidLowercaseString(validators) {
  override def classValidators: List[Validator[String]] = {
    //http://tools.ietf.org/html/rfc2822#section-3.4.1
    return List(new OrValidator[String](List(
      new ValidRegex("^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])$"))));

  }
}

/**
 * @param validators, any custom validators for this field 
 * Checks that a string is a valid password (between 6 and 30 chars)
 */
class ValidPassword(validators: List[Validator[String]])
  extends ValidString(validators) {
  override def classValidators: List[Validator[String]] = {
    return List(new MinLength(6),
      new MaxLength(30));

  }
}

/**
 * @param validators, any custom validators for this field 
 * Checks that a string is a valid username (between 3 and 20 chars), composed of alpha numeric characters
 */
class ValidUsername(validators: List[Validator[String]])
  extends ValidString(validators) {
  override def classValidators: List[Validator[String]] = {
    return List(new MinLength(3),
      new MaxLength(20), new ValidRegex("^[a-z][0-9a-z_]+$"));

  }
}

/**
 * @param validators, any custom validators for this field 
 * Checks that an integer contains a valid port value (between 1 and 65535)
 */
class ValidPort(validators: List[Validator[Integer]])
  extends ValidInteger(validators) {
  override def classValidators: List[Validator[Integer]] = {
    return List(new Maximum(65535), new Minimum(1));

  }
}
