jsish
-----

*jsish* is a simplistic language inspired by Javascript. 

Sample:
```javascript
function identity() {
   print "Hello World!\n";
}

function exponent(power) {

   function identity(number) {
      return number;
   }

   return function (base) {
      var count = identity(power);
      var result = 1;

      while (count > 0) {
         result = result * base;
         count = count - 1;
      }

      return result;
   };
}

var square = exponent(2);
var onehundred = square(10);

identity();
```
This code will print `Hello World!`.
