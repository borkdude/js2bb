import * as http from 'babashka.http-client';

function do_stuff() {
  let version = System.getProperty("java.version");
  // s`...` means symbol("...");
  println('The Java version is:', s`pr-str`(version));
  let response = http.get("https://clojure.org");
  // k`...` means keyword("...");
  println('Response from clojure.org:', k`status`(response));
}

do_stuff();
