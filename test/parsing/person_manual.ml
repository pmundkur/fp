open Fp_lib

type phone_type =
  | Mobile
  | Home
  | Work
  | Unknown_phone_type of int

let to_phone_type = function
  | 0 -> Mobile
  | 1 -> Home
  | 2 -> Work
  | i -> Unknown_phone_type i

let of_phone_type = function
  | Mobile -> 0
  | Home -> 1
  | Work -> 2
  | Unknown_phone_type i -> i

module String = struct
  class o (content : FP_byte_vector.t) (len : FP_byte.t) = object
    method content = FP_byte_vector.read content
    method content_get indx = FP_byte_vector.read_elem indx content
    method content_set indx value = FP_byte_vector.write_elem indx value content
    method len = FP_byte.read len
    method len_set b = FP_byte.write b len
  end

  type v = <content: char array; content_get: int -> char; content_set: int -> char -> unit;
            len: char; len_set: char -> unit>

  let unmarshal env =
    let len, env = FP_byte.unmarshal env in
    let len_val = FP_byte.to_int (FP_byte.read len) in
    let content, env = FP_byte_vector.unmarshal len_val env in
      new o content len, env
end

module Phone_number = struct
  module No_type = struct
    class o = object end

    let unmarshal env =
      new o, env

    type v = < >
  end

  module Type = struct
    class o (typ : FP_byte.t) = object
      method typ = to_phone_type (FP_byte.to_int (FP_byte.read typ))
      method typ_set v = FP_byte.write (FP_byte.of_int (of_phone_type v)) typ
    end

    type v = <typ: phone_type; typ_set: phone_type -> unit>

    let unmarshal env =
      let typ, env = FP_byte.unmarshal env in
        new o typ, env
  end

  type typ =
    | No_type of No_type.v
    | Type of Type.v

  class o (number : String.v) (typ_option : FP_byte.t) (typ : typ) = object
    method number = number
    method typ_option = FP_byte.read typ_option
    method typ_option_set value = FP_byte.write value typ_option
    method typ = typ
  end

  type v = <number: String.v;
            typ_option: char; typ_option_set: char -> unit;
            typ: typ>

  let unmarshal env =
    let number, env = String.unmarshal env in
    let typ_option, env = FP_byte.unmarshal env in
    let typ , env = match FP_byte.to_int (FP_byte.read typ_option) with
      | 0 ->
          let typ, env = No_type.unmarshal env in
            No_type typ, env
      | 1 ->
          let typ, env = Type.unmarshal env  in
            Type typ, env
      | _ ->
          failwith "unknown type_option"
    in
      new o number typ_option typ, env
end

module Person = struct
  module No_email = struct
    class o = object end

    let unmarshal env =
      new o, env

    type v = < >
  end

  module Email = struct
    class o (email : String.v) = object
      method email = email
    end

    let unmarshal env =
      let email, env = String.unmarshal env in
        new o email, env

    type v = <email: String.v>
  end

  type email_address =
    | No_email of No_email.v
    | Email of Email.v

  module Phones_elem = struct
    class o (phone : Phone_number.v) = object
      method phone = phone
    end

    let unmarshal env =
      let phone, env = Phone_number.unmarshal env in
        new o phone, env

    type v = <phone: Phone_number.v>
  end

  module Phones = FP_array(Phones_elem)

  class o (name : String.v) (email_option : FP_byte.t) (email_address : email_address) (num_phones : FP_int16.t) (phones : Phones.v) = object
    method name = name
    method email_option = FP_byte.read email_option
    method email_option_set value = FP_byte.write value email_option
    method email_address = email_address
    method num_phones = FP_int16.read num_phones
    method num_phones_set value = FP_int16.write value num_phones
    method phones = phones
  end

  type v = <name: String.v;
            email_option: char; email_option_set: char -> unit;
            email_address: email_address;
            num_phones: int; num_phones_set: int -> unit;
            phones: Phones.v>

  let unmarshal env =
    let name, env = String.unmarshal env in
    let email_option, env = FP_byte.unmarshal env in
    let email_address, env =
      match FP_byte.to_int (FP_byte.read email_option) with
        | 0 ->
            let email_address, env = No_email.unmarshal env in
              No_email email_address, env
        | 1 | 2 ->
            let email_address, env = Email.unmarshal env in
              Email email_address, env
        | _ ->
            failwith "Unknown email_option" in
    let num_phones, env = FP_int16.unmarshal env in
    let phones, env =
      Phones.unmarshal (FP_int16.to_int (FP_int16.read num_phones)) env
    in
      new o name email_option email_address num_phones phones, env
end
