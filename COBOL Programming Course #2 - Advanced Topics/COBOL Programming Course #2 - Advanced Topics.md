---
geometry:
- margin=1in
header-includes:
- \lstset{basicstyle=\ttfamily,
    stepnumber=2,
    numbersep=5pt,
    backgroundcolor=\color{black!10},
    showspaces=false,
    showstringspaces=false,
    showtabs=false,
    tabsize=2,
    captionpos=b,
    breaklines=true,
    breakatwhitespace=true,
    breakautoindent=true,
    linewidth=\textwidth}
- \hypersetup{colorlinks=true,
              linkcolor=blue}
---
\newpage
## Additional information

This section provides useful resources in the form of manuals and videos to assist in learning more about the basics of COBOL.

### Professional manuals

As Enterprise COBOL experience advances, the need for the professional documentation is greater.  An internet search for Enterprise COBOL manuals includes: “Enterprise COBOL for z/OS documentation library - IBM”, link provided below.  The site content has tabs for each COBOL release level.  As of April 2020, the current release of Enterprise COBOL is V6.3.  Highlight V6.3 tab, then select product documentation.

[https://www.ibm.com/support/pages/enterprise-cobol-zos-documentation-library](https://www.ibm.com/support/pages/enterprise-cobol-zos-documentation-library)


Three ‘Enterprise COBOL for z/OS” manuals are referenced throughout the chapters as sources of additional information, for reference and to advance the level of knowledge. They are:

1. Language Reference - Describes the COBOL language such as program structure, reserved words, etc. 

   [http://publibfp.boulder.ibm.com/epubs/pdf/igy6lr30.pdf](http://publibfp.boulder.ibm.com/epubs/pdf/igy6lr30.pdf)

2. Programming Guide - Describes advanced topics such as COBOL compiler options, program performance optimization, handling errors, etc.

   [http://publibfp.boulder.ibm.com/epubs/pdf/igy6pg30.pdf](http://publibfp.boulder.ibm.com/epubs/pdf/igy6pg30.pdf)

3. Messages and Codes - To better understand certain COBOL compiler messages and return codes to diagnose problems.

   [http://publibfp.boulder.ibm.com/epubs/pdf/c2746481.pdf](http://publibfp.boulder.ibm.com/epubs/pdf/c2746481.pdf)

### Learn more about recent COBOL advancements

- What’s New in Enterprise COBOL for z/OS V6.1:

   [https://www.ibm.com/support/pages/cobol-v61-was-announced-whats-new](https://www.ibm.com/support/pages/cobol-v61-was-announced-whats-new)

- What’s New in Enterprise COBOL for z/OS V6.2:

   [https://www.ibm.com/support/pages/cobol-v62-was-announced-whats-new](https://www.ibm.com/support/pages/cobol-v62-was-announced-whats-new)

- What’s New in Enterprise COBOL for z/OS V6.3:

   [https://www.ibm.com/support/pages/cobol-v63-was-announced-whats-new](https://www.ibm.com/support/pages/cobol-v63-was-announced-whats-new)

\newpage
# Numerical Data Representation

In the first COBOL Programming Course, various types of data representation were discussed. This chapter seeks to expand upon the binary and hexadecimal numbering systems as well as the various numeric representations in COBOL. 

- **Numbering Systems**
     - **Binary System**
     - **Hexadecimal System**
     - **EBCDIC Encoding**
     - **COBOL Picture Clause**
- **Numeric Representations in COBOL**
     - **Zoned Decimal Format**
     - **Packed Decimal Format**
     - **Binary Format**
     - **Single Precision Floating Point**
     - **Double Precision Floating Point**


## Numbering systems

A numbering system provides a means to represent numbers. We are most familiar with using the base-10 number system known as decimal. Data such as numerical values and text are internally represented by zeros and ones in most computers, including mainframe computers used by enterprises. This base-2 number system known as binary. Although data is encoded in binary on computers, it is much easier to work with base-16 known as hexadecimal. Each sequence of four binary digits is represented by a hexadecimal value.

### Binary System

Just as in our decimal system, a binary integer is a sequence of binary digits 0 and 1 arranged in such an order that the position of each bit implies its value in the integer.  The binary representation of the number 21 is:

![](Images/binary-rep-of-21.png)

On the IBM Mainframe system, the two’s complement form is used for the representation of binary integers. In this form, the leftmost bit is used to represent the sign of the number: 0 for positive and 1 for negative. For a positive number, the two’s complement form is simply the binary form of the number with leading zero(s). For a negative number, the two’s complement is obtained by writing out the positive value of the number in binary, then complementing each bit and finally adding 1 to the result. Assuming one byte of storage and b<sub>0</sub>b<sub>1</sub>b<sub>2</sub>b<sub>3</sub>b<sub>4</sub>b<sub>5</sub>b<sub>6</sub>b<sub>7</sub> are the bits, let us look at some examples.

![](Images/twos-complement.png)

As we can see, the sign bit b<sub>0</sub> is 0 for a positive integer and 1 for a negative integer. This bit will participate in all arithmetic operations as though it represented the value (-b<sub>0</sub> * 2<sup>k-1</sup>) for a k bit number. In the above binary representation of -28:

![](Images/twenty8-binary-place-value.png)

The number of bits will clearly dictate the range of values that can be stored. With k bits, the maximum positive value that can be stored correctly is 2<sup>k-1</sup> - 1 and the minimum (negative) value will be -2<sup>k-1</sup>. The number Zero is always represented with sign bit zero. With k=4:

![](Images/all-values-in-4bits.png)

For K=32 bits, the range is -2<sup>31</sup> to +2<sup>31</sup> - 1

### Hexadecimal System

There are sixteen digits, represented by 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E and F. The first 10 symbols have their usual meaning; the remaining six, A through F, represent the values 10 through 15 when used as hexadecimal digits. 

![](Images/hex-1-16-values.png)

A423 is the hexadecimal equivalent of the decimal value 42019:

![](Images/hex-place-value-a423.png)

Although data is encoded in binary on computers, it is rather cumbersome to work with binary.  The hexadecimal numerals provide a human friendly representation of the binary coded values. An understanding of this system is invaluable to the COBOL programmer as he designs, develops and tests code.  Often, hex dumps of the data in memory are used to debug a program and understand what is going on. The conversion between binary and hexadecimal system is easy as 2<sup>4</sup> = 16. Each hexadecimal digit represents 4 binary digits, also known as a nibble, which is half a byte.

   - To convert from hexadecimal to binary, replace each hexadecimal digit with its equivalent 4-bit binary representation
   - To convert from binary to hexadecimal, replace every four consecutive binary digits by their equivalent hexadecimal digits, starting from the rightmost digit and adding zeros, on the left if necessary


![](Images/hex-binary-conversion.png)

The usual convention is to use X’  ‘  to denote a hexadecimal value, B’  ‘  to denote a binary value. 

### EBCDIC Encoding 
 
C' ' is used to represent a character value. It is helpful to familiarize yourself with the 8-bit EBCDIC encoding scheme that is used on the zOS and most IBM mainframes. 

![](Images/ebcdic-table.png)

8-bit EBCDIC Encoding

For numerical representations, the last column is of particular interest here; the character representations of numerical digits 0-9 in the EBCDIC encoding. C’5’ is encoded, for example, as X’F5’ and C’9’ as encoded as X’F9’. 

### COBOL Picture Clause

As a quick reminder, COBOL leverages numeric data with a PIC clause that can contain a 9, V and/or S. These symbols keep the number purely mathematical that can participate in arithmetic.                    
    
   - 9 is used to indicate numeric data consisting of the digits from 0 to 9
   - V indicates where the assumed decimal place is located
   - S will remember the sign which is necessary if the data is negative


![](Images/the-9-v-and-s.png)

Since the number of decimal places is determined and fixed in place by the V, this representation is called fixed point . Let us illustrate with an example to show how the V determines the value stored.

![](Images/how-v-plays-out.png)

## Numeric Representations in COBOL

In this section, we will investigate the numeric representations in COBOL:
   - Zoned Decimal (Fixed Point)
   - Packed Decimal (Fixed Point)
   - Binary (Fixed Point)
   - Single Precision Floating Point
   - Double Precision Floating Point

### Zoned Decimal Format

In this format, each byte of storage contains one digit. The high order 4-bits (or nibble) are called the Zone bits. The low order 4-bits are called the Decimal or Numeric bits and will contain the binary value for the digit. Considering a simple case, the number 25 is represented as X’F2F5’. 

![](Images/zone-numeric-bits.png)

The zone portion is the ‘upper half byte’ and numeric portion is the ‘lower half byte’. This format is the default numeric encoding in COBOL. The coding syntax of USAGE IS DISPLAY can also be used. Let us look at a few valid zoned decimal declarations.

![](Images/cobol-zd-declaration.png)

As discussed earlier, the first two declarations above are unsigned, indicated by the absence of a S. Such numbers are ‘implied positive’. The next two declarations are explicitly signed by the symbol S and are capable of representing positive and negative numbers. The sign is represented by the rightmost zone bits (in the above example the F above the 5) and is determined as follows:

  - F indicates the number is unsigned
  - C indicates the number is positive
  - D indicates the number is negative

![](Images/zoned-decimal-values-table.png)

It is clear some adjusting (or editing) needs to be done before printing a signed number. The V in the declarations above has no storage allocated for it and will also need to be edited for printing purposes. To illustrate the way it works, consider an input file with a number 12345 and the input PIC clause is 999V99. This means that there is a decimal point assumed between 3 and 4. When the number is later aligned with an edited field, say ‘999.99’, the result is printed as 123.45.

When we code arithmetic statements involving zoned decimal fields, under the covers, COBOL converts the data to packed decimal and/or binary representations in order to do the math and the result is converted back to zoned decimal, all seamlessly. This extra step and hence a loss in efficiency is the price to pay for the easy readability that this format provides.

![](Images/adv-disadv-zoned-decimal.png)

### Packed Decimal Format

In the zoned decimal format, the rightmost zone bits determine the sign; the other F’s are redundant. When a number is ‘packed’, those extra zone bits are removed, and only the rightmost zone bits are retained. Hence, the move from an unpacked field changes every byte in the field (except the last) from X’Fn’ to X’n’. The nibbles in the last byte get flipped (X’C2’ becomes X’2C’). 

![](Images/zd-to-pd.png)

As we can observe, when the number is packed into a field that is larger than necessary to hold that number, it is padded with zeroes on the left.

   - Number 1                        will be stored as X’1F’              in 1 byte
   - Number +12                      will be stored as X’012C’            in 2 bytes
   - Number -123                     will be stored as X’123D’            in 2 bytes
   - Number 1234 (unsigned)          will be stored as X’01234F’          in 3 bytes
   - Number +12345                   will be stored as X’12345C’          in 3 bytes

The COBOL syntax for this format is USAGE IS COMP-3 or just COMP-3.

![](Images/cobol-pd-declaration.png)

As the packed decimal representation stores two digits in one byte, it is a variable length format. Also, as we can see, the digits are stored in decimal notation, and each digit is binary coded. So, COMP-3 exactly represents values with decimal places.  A COMP-3 value can have up to 31 decimal digits. This format is somewhat unique and native to mainframe computers such as the IBM z architecture. The zOS has specialized hardware for packed decimal arithmetic and so the system can perform mathematical calculations without having to convert the format. This is, by far, the most utilized numerical value representation in COBOL programs. Storing information in this format may save a significant amount of storage space.

![](Images/pd-lengths.png)


It is usually the best choice for arithmetic involving decimal points/fractions.  After numerical processing, a packed decimal field is (moved) unpacked into a zoned decimal format which can then be edited for printing purposes.

![](Images/adv-disadv-packed-decimal.png)

### Binary Format

On the IBM Mainframe systems, the other main arithmetic type besides the packed decimal is the binary format which is built for efficiency in integer arithmetic operations. This encoding finds many uses in Legacy applications. Many datasets are created with binary fields. Variable length records and table processing in COBOL use this representation.  The binary format is largely implementation dependent and has many variations. On the zOS and IBM Mainframes, the twos complement encoding is used. 

The COBOL clauses for this format are COMP, COMP-4, COMPUTATIONAL or BINARY which can be used interchangeably. COMP-5 clause also falls in this category. Let us look at some valid declarations.

![](Images/cobol-binary-declaration.png)

The PIC Clause determines the storage space:

  - PIC 9(1)   through PIC 9(4)     will reserve 2 bytes (Binary halfword)
  - PIC 9(5)   through PIC 9(9)     will reserve 4 bytes (Binary fullword)
  - PIC 9(10)  through PIC 9(18)    will reserve 8 bytes (Binary doubleword)

  Next, let’s look at what numbers can be stored. For the COMP and COMP-4 fields, although the data is stored as binary numbers, the range is limited by the full value of the PIC Clause used in the field definition. The binary format, COMP-5 (also known as ‘Native Binary’) in which the PIC clause still defines the size of the field but the range of values that can be represented is much higher as every possible bit-value combination is valid. 

![](Images/binary-ranges-1.png)
![](Images/binary-ranges-2.png)
![](Images/binary-ranges-3.png)

Those numbers are more than sufficient for most business applications! To give a quick comparison, a two byte packed decimal field can range in value from -999 to +999 only. When faced with larger than capacity values, COMP truncates to the decimal value of the PIC clause and COMP-5 truncates to the size of the field.

Although very much suited for integer processing, the binary format is not a good choice for non-integer arithmetic. Many banking and insurance applications rely on accuracy for their business processing logic and packed decimal format is preferred in such cases. Let’s see why. 
In decimal systems, fractions are represented in terms of negative powers of 10:

![](Images/three-by-four-decimal.png) 

In binary system fractions are represented in terms of negative powers of 2:

![](Images/three-by-four-binary.png) 

There is a possible loss of accuracy when converting a decimal fraction to a binary fraction as there is not a one-to-one correspondence between the set of numbers expressible in a finite number of binary digits and the set of numbers expressible in a finite number of decimal digits. Let’s take the example of the fraction 1/10. In the decimal system:

![](Images/point-one-decimal.png) 

However, in the binary system, this is a never ending sequence of bits…!!   

![](Images/point-one-binary.png) 

So, we get different values when, for example, we multiply 1/10 by 10 in the decimal and in the binary systems. In the decimal system, 10 X 0.1 = 1.0.  In the binary system, we get

![](Images/multiply-point-1-binary.png) 

i.e. not quite 1.0 !! In scenarios involving a large number of calculations, this type of discrepancy may lead to cumulative rounding errors that may not be acceptable in many business applications. The use of packed decimal works very well in such cases. 

![](Images/adv-disadv-binary.png)


### COMP-1: Single Precision Floating Point

 Due to the floating-point nature, a COMP-1 value can be very small and close to zero, or it can be very large (about 10 to the power of 38).  However, a COMP-1 value has limited precision.  This means that even though a COMP-1 value can be up to 10 to the power of 38, it can only maintain about seven significant decimal digits.  Any value that has more than seven significant digits are rounded.  This means that a COMP-1 value cannot exactly represent a bank balance like $1,234,567.89 because this value has nine significant digits.  Instead, the amount is rounded.  The main application of COMP-1 is for scientific numerical value storage as well as computation.

### COMP-2: Double Precision Floating Point

COMP-2 extends the range of value that can be represented compared to COMP-1.  COMP-2 can represent values up to about 10 to the power of 307.  Like COMP-1, COMP-2 values also have a limited precision.  Due to the expanded format, COMP-2 has more significant digits, approximately 15 decimal digits.  This means that once a value reaches certain quadrillions (with no decimal places), it can no longer be exactly represented in COMP-2.

COMP-2 supersedes COMP-1 for more precise scientific data storage as well as computation.  Note that COMP-1 and COMP-2 have limited applications in financial data representation or computation.

**Note** : [This](https://www.ibm.com/support/pages/how-display-hexadecimal-using-cobol) COBOL program can display the hexadecimal contents (and hence the exact internal representation) of a field. You can declare binary, packed decimal or zoned variable (or anything else, for that matter), do arithmetic with them and use the program to see how they are internally stored. 

\newpage
# Dynamic-Length Item
So far during this course, we have only explored data items that have a fixed length. In other words, you need to define the length you need for each data type. But in this section, we will explore a fairly new concept in Enterprise COBOL - dynamic-length items.

Enterprise COBOL v6.3 supports dynamic-length items, which are items whose logical length might change at runtime.

## Dynamic-Length Elementary Items

Let us consider a dynamic-length elementary item. To recall, an elementary item is an item that cannot be further subdivided. These items have a PIC clause since storage is reserved for it. If we used dynamic-length elementary item to send data, it will be treated as a fixed-length item with a length equals to the current length of the dynamic-length item. On the other hand, if we used dynamic-length elementary item to receive data and it's not reference-modified, the content will simply be moved to the receiving's content buffer.

If the content received is longer than the current length, a new larger buffer will be allocated for it. Additionally, if the length of the sender is zero, the receiver's length will be set to zero as well.

Now, if the dynamic-length elementary item is used to receive data and we reference-modified it, the item will be treated as a fixed-length item with with a length equals to the current length of the dynamic-length item. In such cases, the compiler will not allocate or reallocated the buffer.

Note that not all statement supports dynamic-length elementary items. Common statement like REDEFINE or RENAME will not work. Additionally, we cannot take their address using the ADDRESS-OF special register. The full list of the statements supported is available on the [Language Reference](https://www.ibm.com/docs/en/cobol-zos/6.3?topic=relationships-dynamic-length-items).

When we compare a dynamic-length item with a fixed-length item, the comparison will follow the normal comparison rules (the shorter item will be extended to the right with enough spaces to make both items equal in length and then each character will be compared). Meanwhile, if you compare two dynamic-length elementary items, the lengths will be compared first and if they matched, the characters will then be examined.

We can also set the length of dynamic-length elementary item using the SET LENGTH OF syntax and pass dynamic-length elementary items as fixed-length items to a subroutine using the AS FIXED LENGTH phrase.

Note that doing the intrinsic function MIN and MAX are not supported for dynamic-length items.

## Dynamic-Length Group Items

A dynamic-length group item is a group item that contains at least one subordinate dynamic-length elementary item and whose logical length might change at runtime.

Any other group item is considered to be a fixed-length group item. These fixed-length group items can contain variable-length tables through the OCCURS DEPENDING ON clause.

Additionally, we cannot compare or move dynamic-length group items to any other group items. On the other hand, fixed-length group items are always compatible and comparable with other fixed-length group items.

## DYNAMIC LENGTH Clause

To define a dynamic length item, we can include the DYNAMIC LENGTH clause on the data description entry. Here are a couple of examples of how to indicate the clause:

```
01 MY-DYN PIC X DYNAMIC.
01 NAME PIC X DYNAMIC LENGTH.
01 DYN-PRICE PIC X DYNAMIC LIMIT 500.
```

Let us observe a few things from the examples above. Firstly, we note that the LENGTH keyword is optional. Next, we also have a LIMIT phrase that specifies the maximum length of the data item. If a sender's length is longer than the receiver's LIMIT value, the data will be truncated on the right. This LIMIT value defaults to 999999999 if not specified. Lastly, note that we use PIC X. To use dynamic-length clause, you can only use PIC X or PIC U (which is for UTF-8 data item).

\newpage
# UTF-8 Data Type

With Enterprise COBOL v6.3, we also have a new USAGE, which is UTF-8. This is indicated by the picture symbol 'U'. Unlike NATIONAL or DBCS characters, the byte length of each UTF-8 character varies between 1 and 4 bytes. Enterprise COBOL treats a single UTF-8 character as equivalent to a single Unicode code point.

## UTF-8 Data Items

There are three ways that Enterprise COBOL uses to define UTF-8 data items.

### Fixed Character-Length UTF-8 Data Items

This type of UTF-8 data item is defined when the PICTURE clause contains one or more 'U' characters, or a single 'U' followed by a repetition factor. Take for example the piece of code below:

```
01 NEW-UTF-CHAR PIC U(10).
```

In this case, we define a fixed character-length UTF-8 data item that holds 10 UTF-8 characters that occupy between 10 (n) and 40 (4 * n) bytes. Since UTF-8 character's byte length varies, 4 * n bytes are always reserved for UTF-8 item. If there are unused bytes, those will be padded with the UTF-8 blank statement (x'20'). When truncation is performed, it is done on a character boundary.

### Fixed Byte-Length UTF-8 Data Items

Like fixed character-length, we define this by the inclusion of the 'U' character in the PICTURE clause. But now, we will add a phrase called BYTE-LENGTH. Observe the code below:

```
01 NEW-UTF-BYTE PIC U BYTE-LENGTH 10.
```

In this case, we define a fixed byte-length UTF-8 data item that holds 10 bytes of UTF-8 data, this translates to up to 10 characters. When these are used to receive characters with byte length smaller than indicated, the unused bytes are padded by the UTF-8 blank statement (x'20').

### Dynamic-Length UTF-8 Data Items

Lastly, we have the dynamic-length UTF-8 data items. This is defined when we have a PICTURE clause with the 'U' character and the DYNAMIC LENGTH clause. Observe the code below:

```
01 NEW-UTF-DYN PIC U DYNAMIC LIMIT 10.
```

With dynamic-length UTF-8 data item, there is no restriction on the number of bytes besides the one indicated on the LIMIT phrase of the DYNAMIC LENGTH clause. Unlike the other two definitions, no padding is involved with the dynamic-length UTF-8 data item. Truncation will only occur on the character boundaries if it exceeds the specified limit.

Note that UTF-8 edited, numeric-edited, decimal and external float are not supported.

## UTF-8 Literals

There are two types of UTF-8 literals which are supported on Enterprise COBOL.

### Basic UTF-8 Literals

```
U'character-data'
```

When we define basic UTF-8 literals, the character-data is converted from EBCDIC to UTF-8. If we have double-byte EBCDIC characters, those must be delimited by shift-out and shift-in characters. The amount of Unicode code points which we can represent here varies depending on the size of the UTF-8 characters, but a maximum of 160 bytes (after conversion) is allowed before truncation.

### Hexadecimal UTF-8 Literals

```
UX'hexadecimal-digits'
```

In this case, the hexadecimal-digits are converted to bytes sequences which are used verbatim as the UTF-8 literal values. There is a minimum of 2 hexadecimal digits and a maximum of 320.

## UTF-8 Move Rules and Conversion

Generally speaking, a UTF-8 data item can be moved only to those of category National or UTF-8. While they can receive items from Alphabetic, Alphanumeric, National or UTF-8. If there are any padding or truncation, those are always done at the UTF-8 character.

Additionally, we can use the intrinsic function DISPLAY-OF to convert national to UTF-8 and UTF-8 to alphanumeric or the intrinsic function NATIONAL-OF to convert UTF-8 to national.

**Note** : For more information, please refer to the [Programming Guide](https://www.ibm.com/docs/en/cobol-zos/6.3?topic=cobol-converting-from-utf-8-unicode-representation).

\newpage
# COBOL Application Programming Interface (API)
API is the acronym for Application Programming Interface.  An API allows two applications to communicate. We use API's everyday from our phones, personal computers, using a credit card to make a payment at a point of sale, etc.  

Today's digital infrastructure is instrumented and interconnected.  It is the API's that enable the "instrumented" network to be "interconnected".  As a result, API has become a highly used acronym in the technology arena.  The phrase "API Economy" became strategic term since Forbes declared 2017 "The Year of the API Economy".

Business application solutions were architected decades ago using programming language API's.  Long before API became a strategic technology category, mainframe application developers understood the acronym as a way to enable a programming language to communicate with other software.  The value of being a programmer in any specific programming language increased by understanding and using API's.

- **Enterprise COBOL APIs**
     - **z/OS Middleware**
     - **COBOL API Communication with Middleware**
     - **COBOL EXEC SQL**
     - **COBOL Data Items**

- **SQL Capability within Enterprise COBOL**
     - **Enterprise COBOL Application Programming and SQL Guide**
     - **Db2 Data Base Administration (DBA) vs Application Programming**

- **Lab**
    - **Using VSCode and Zowe Explorer**

## Enterprise COBOL APIs
IBM mainframe flagship operating system, z/OS, includes software that has enabled large scale business applications for decades.  The software is frequently referred to as 'middleware'. Examples of z/OS 'middleware' is Db2, a relational database, CICS, transactional processor, IMS, both transactional and hierarchical database, and MQSeries, a mechanism to store and forward data between systems asynchonously.

### z/OS Middleware
A fundamental capability of z/OS middleware software is communication enablement of programming languages.  The z/OS middleware software includes documentation and examples of how any specific programming language can communicate with the specific z/OS middleware software.  A programming language, such as Enterprise COBOL, would use documented interfaces and techniques to initiate services and pass data between the COBOL application program and the middleware. 

### COBOL API Communication with Middleware
Each middleware has unique reserved words available to Enterprise COBOL.  

Enterprise COBOL unique API reserved words are in Example 1.

```
EXEC SQL
EXEC CICS
CALL 'MQ...'
CALL 'CBLTDLI'
```
*Example 1. COBOL API Reserved Words*

Each of the above COBOL API's enable the program to communcate with Db2, CICS, MQSeries, and IMS respectively.  When the COBOL source program is compiled, the API reserved words expand the number of lines in the COBOL source code.  The expanded lines of code does not need to be fully understood by the COBOL programmer.  The COBOL programmer uses an API to accomplish a task within the logic and the middleware expanded code follows through with accomplishing the task.

### COBOL EXEC SQL
SQL, Structured Query Language, is the documented standard for communicating will all relational databases.  Enterprise COBOL is capable of including Db2 for z/OS SQL.  A few simple COBOL EXEC SQL reserved words are shown in Example 2.

```
 WORKING-STORAGE SECTION. 
*****************************************************
* SQL INCLUDE FOR SQLCA                             *
*****************************************************
          EXEC SQL INCLUDE SQLCA  END-EXEC. 
*****************************************************
* SQL DECLARATION FOR VIEW ACCOUNTS                 *
*****************************************************
          EXEC SQL DECLARE my-acct-tbl TABLE 
                  (ACCTNO     CHAR(8)  NOT NULL, 
                   LIMIT      DECIMAL(9,2)     , 
                   BALANCE    DECIMAL(9,2)     , 
                   SURNAME    CHAR(20) NOT NULL, 
                   FIRSTN     CHAR(15) NOT NULL, 
                   ADDRESS1   CHAR(25) NOT NULL, 
                   ADDRESS2   CHAR(20) NOT NULL, 
                   ADDRESS3   CHAR(15) NOT NULL, 
                   RESERVED   CHAR(7)  NOT NULL, 
                   COMMENTS   CHAR(50) NOT NULL) 
                   END-EXEC. 
*****************************************************
* SQL CURSORS                                       *
*****************************************************
          EXEC SQL DECLARE CUR1  CURSOR FOR 
                   SELECT * FROM my-acct-tbl 
               END-EXEC. 

 PROCEDURE DIVISION.
*------------------ 
 LIST-ALL. 
          EXEC SQL OPEN CUR1 END-EXEC. 
          EXEC SQL FETCH CUR1 INTO :CUSTOMER-RECORD END-EXEC.
          PERFORM PRINT-AND-GET1 
               UNTIL SQLCODE IS NOT EQUAL TO ZERO. 
          EXEC SQL CLOSE CUR1   END-EXEC.
```
*Example 2. COBOL SQL Statements*

### COBOL Data Items
While the EXEC SQL is expanded into additional lines of code at compile time,
COBOL needs data items to manage the data passed between the COBOL program and Db2 table. 

The fields in the Db2 table record were defined using CREATE TABLE SQL.  The EXEC SQL DECLARE in Table xx describes the Db2 table format within the COBOL program.  The COBOL programmer with knowledge of the Db2 table format can code the table format or let Db2 for z/OS generate the code using a DCLGEN utility.  

Observe ":CUSTOMER-RECORD" in the EXEC SQL FETCH statement.  A colon (:) precedes COBOL program defined variables that are used in SQL statements so that Db2 can distinguish a variable name from a column name.  Example 3. shows the COBOL program data items describing the COBOL program variable names.

```
*****************************************************
* STRUCTURE FOR CUSTOMER RECORD                     *
*****************************************************
 01 CUSTOMER-RECORD. 
    02 ACCT-NO            PIC X(8). 
    02 ACCT-LIMIT         PIC S9(7)V99 COMP-3. 
    02 ACCT-BALANCE       PIC S9(7)V99 COMP-3. 
    02 ACCT-LASTN         PIC X(20). 
    02 ACCT-FIRSTN        PIC X(15). 
    02 ACCT-ADDR1         PIC X(25). 
    02 ACCT-ADDR2         PIC X(20). 
    02 ACCT-ADDR3         PIC X(15). 
    02 ACCT-RSRVD         PIC X(7). 
    02 ACCT-COMMENT       PIC X(50).
```
*Example 3. COBOL Data Item for storing variables where Db2 is the data source*

## SQL Capability within Enterprise COBOL
Learning SQL is a separate technical skill.  The objective of this brief chapter is familiarization with Enterprise COBOL use of SQL API.  A COBOL program is capable of any SQL communication with Db2 for z/OS assuming necessary authority is granted.  SQL has four catagories as outlined in Example 4.  Learning SQL is necessary for a COBOL programmer to become proficient with using the Db2 API for a variety of possible applications where COBOL provides the what, how, and when logic of executing specific SQL.  

```
DDL - Data Definition Language
CREATE
ALTER 
DROP

DML - Data Manipulation Language
SELECT
INSERT
UPDATE
DELETE 

DCL - Data Control Langauge
GRANT
REVOKE

TCL - Transaction Control Language
COMMIT
ROLLBACK
```
*Example 4. SQL Categories*

### Enterprise COBOL Application Programming and SQL Guide
Db2 for z/OS V12 is the most current release of Db2 at the moment.  The Db2 V12 for z/OS Application Programming and SQL Guide is available using internet search SC27-8845, the Db2 for z/OS professional manual number.  Db2 V12 for z/OS SQL Reference is also necessary to advance programming API capability (SC27-8859).

### Db2 Data Base Administration (DBA) vs Application Programming
In large enterprise, the roles and responsibilities are divided for a number of reasons.  The responsibility of the DBA would include the DDL and DCL outlined in Example 4.  The DBA is responsibile for managing the entire relational data base environment to insure availability, security, performance, etc.  The system programmers and DBAs frequently setup the application development procedures for COBOL programmer development, testing, and maintenance of the COBOL business applications.  A COBOL application programmer is typically provided documented procedures to follow to apply their COBOL programming and SQL API expertise.

Enterprise COBOL is a learning journey.  Each Enterprise COBOL API is a separate learning journey.  As is the case with most professional endeavors, learning, repetition, and applying what is learned is re-iterative process leading to advanced skill levels.

## Lab
The lab contains data used in previous labs from "COBOL Programming Course #1 - Getting Started" where the data source was sequential data set, then a VSAM data set.  The lab provides JCL to create a personal Db2 table in a DBA-created database name using a DBA-created storage group.  The DBA-created storage group directs the create tablespace and table to specific disk storage volumes.

The lab contains Enterprise COBOL source code with Db2 APIs along with the JCL to compile and execute the COBOL programs.

### Using VSCode and Zowe Explorer
Zowe Explorer is currently without the ability to execute Db2 SQL interactively. It is inevitable Zowe Explorer will eventually have the capability of connectiong to relational databases and executing SQL.

Therefore, JCL members were created to create and load user tables following examples provided.

1. Submit `zos.public.db2.jcl(db2setup)`
The result is new JCL and CBL members copied into personal JCL and CBL libraries

2. SUBMIT JCL(CRETBL)
The result is a personal Db2 tablespace, table, indexspace, and index
 
3. SUBMIT JCL(LOADTBL)
The result is data loaded into the personal Db2 tablespace, table, indexspace, and index
 
4. Edit each COBOL source code member in your CBL partition data set changing all occurrences of Z# to your personal ID. Example - If your ID was Z80001, then change all occurrences of Z#  to Z80001.
 
5. SUBMIT JCL(CBLDB21C)
The result is compile of CBL program CBLDB21 and a Db2 Plan needed for program execution
 
6. SUBMIT JCL(CBLDB21R)
The result is execution of COBOL program CBLDB21 to read the Db2 table and write each record from the Db2 table .

7. Two additional COBOL programs with Db2 API exist, CBLDB22 and CBLDB23 using the same Db2 table as the data source.


\newpage
# Multithreading and COBOL

We can run COBOL programs in multiple threads. To do so, we compile using the THREAD compiler option.

Note that COBOL does not directly support the management of the program threads. But we can run the programs that we compile in a multithreaded application server. So, other programs can call the COBOL program we wrote in a way that enables it to run in multiple threads.

**Choosing LOCAL-STORAGE or WORKING-STORAGE** 

- Data items in the LOCAL-STORAGE SECTION are allocated for each instance of a program invocation. So in this case, each copy of the program will have its copy of the LOCAL-STORAGE data.

- Data items in the WORKING-STORAGE SECTION are only allocated once for each program, so they will be available in their last-used state to all programs invocation.

So, if we want to isolate data to an individual invocation, we need to define the data in the LOCAL-STORAGE SECTION. If we decided to define them in the WORKING-STORAGE SECTION, we need to make sure that the data will not be accessed simultaneously from multiple threads, or if we do, write the appropriate serialization code for it.

## Multithreading

Let us first understand how multithreading works.

The operating system and multithreaded applications handle execution flow within a *process*, which is the course of events when the program runs. Programs within a process can share resources, and the processes themselves can be manipulated.

Within a process, an application can initiate one or more *threads*, basically a stream of computer instruction that controls it. A multithreaded process begins with one thread and can create more to perform tasks. These threads can run concurrently.

In a multithreaded environment, a COBOL *run unit* is the portion of the process that includes threads that have actively executing COBOL programs. The run unit will continue until no COBOL program is active in any of the threads. Within the run unit, COBOL programs can call non-COBOL programs and vice versa.

Within a thread, control is transferred between separate COBOL and non-COBOL programs. Each separately called program is a *program invocation instance*. Program invocation instances of a particular program can exist in multiple threads within a given process.

## THREAD to support multithreading

As mentioned previously, we will need to use the THREAD compiler option for multithreading support. Note that using THREAD might adversely affect performance due to the serialization logic that is generated.

To run multiple COBOL programs in more than one thread, all of them must be compiled using the THREAD and RENT compiler option, and link them with the RENT option of the binder.

We will also need to use the THREAD option to compile object-oriented clients and classes.

## Transferring control to multithreaded programs

When we write COBOL programs for a multithreaded environment, we will need to choose appropriate program linkage statements.

Just like single-threaded environments, a called program is in its initial state when it is first called within a run unit and when it is first called after a CANCEL to the called program. We need to ensure that the program we want to CANCEL is not active on any thread, or a Language Environment severe error will be produced.

## Ending multithreaded environment

We can end a multithread program by using GOBACK, EXIT PROGRAM or STOP RUN.

GO BACK will return control to the caller of the program. If the caller is the first program in a thread, the thread will be terminated. If the thread is the initial one in a process, the process will be terminated.

EXIT PROGRAM runs the same way as GO BACK, except from the main program where it has no effect.

STOP RUN will terminate the entire Language Environment process and return control to the caller of the main program (which might be the operating system). All threads in the process will also be terminated.

## Processing files with multithreading

In threaded applications, we can code COBOL statements for input and output in QSAM, VSAM, and line-sequential files.

Each file definition has an implicit serialization lock, which is used with automatic serialization logic during the I/O operations associated with the following statements: OPEN, CLOSE, READ, WRITE, REWRITE, START, DELETE.

However, automatic serialization is not applied to statements specified with the following conditional phrases: AT END, NOT AT END, INVALID KEY, NOT INVALID KEY, AT END-OF-PAGE, NOT AT END-OF-PAGE.

### File-definition storage

Upon program invocation, the storage associated with file definition (such as FD records) is allocated and available in its last-used state. Therefore, all threads of execution will share this storage. You can depend on automatic serialization for this storage during the execution of the statements mentioned previously, but not between uses of the statements.

### Serializing file access with multithreading

To take advantage of automatic serialization, we can use one of the recommended following file organization and usage patterns when we access files in threaded programs.

Recommended file organizations:
- Sequential organization
- Line-sequential organization
- Relative organization with sequential access
- Indexed organization with sequential access

The recommended pattern for input:
```
    OPEN INPUT fn
    ...
    READ fn INTO local-storage-item
    ...
  * Process the record from the local-storage item.
    ...
    CLOSE fn
```

The recommended pattern for output:
```
    OPEN OUTPUT fn
    ...
  * Construct output record in local-storage item.
    ...
    WRITE rec from local-storage-item
    ...
    CLOSE fn
```

With other usage patterns, you must ensure that two instances of the program are never simultaneously active on different threads or that serialization logic is coded explicitly by using calls to POSIX services.

To avoid serialization problems, we can define the data items that are associated with the file in the LOCAL-STORAGE SECTION.

## Limitation of COBOL with multithreading

In a multithreaded environment, there are some limitations on COBOL programs. In general, we must synchronize access to resources that are visible to the application within a run unit. 

- CICS: We cannot run a multithreaded application in CICS. However, programs compiled with the THREAD option can run in CICS as part of an application that does not have multiple threads.

- Recursive: Since we code the programs in a multithreaded application as recursive, we must adhere to all the restrictions and programming constraints that apply to recursive programs.

- Reentrancy: We must compile our multithreading programs with the RENT compiler option and link them with the RENT option of the binder.

- AMODE: We must run multithreaded applications with AMODE 31. However, programs compiled with the THREAD option can run with AMODE 24 as part of an application that does not have multiple threads.

- Older COBOL programs: To run your COBOL programs on multiple threads of a multithreaded application, we must compile them with Enterprise COBOL using the THREAD option.

To see more details on the limitation of COBOL with multithreading, check out the [Programming Guide](https://www.ibm.com/docs/en/cobol-zos/6.3?topic=multithreading-handling-cobol-limitations).

\newpage

# SORT and MERGE
Sometime when we want to process a file we need it to be sorted, COBOL provides the `SORT` verb for this.

If we have two or more file that are sorted using the same key or keys, we can combine them into one sorted file
using COBOL `MERGE` verb.

- **SORT**
    - **The formats of the SORT statement**
    - **INPUT PROCEDURE phrase**
    - **DUPLICATES phrase**
        - **Format 1**
        - **Format 2**
    - **ASCENDING KEY and DESCENDING KEY phrases (format 2)**
    - **SORT Example: Sorting an employee data file**
- **Merge**
    - **The Merge statement Format**
    - **Merge Example: merging employee, and interns data files together**
- **SORT & MERGE**
    - **USING phrase**
    - **GIVING phrase**
    - **OUTPUT PROCEDURE phrase**
    - **ASCENDING/DESCENDING KEY phrase**
        - **KEYs rules**
        - **KEYs types**
        - **COLLATING SEQUENCE phrase**
        - **Segmentation considerations**


## SORT
The `SORT` statement causes a set of records or table elements to be arranged in a user-specified sequence.

For sorting **files**, the `SORT` statement accepts records from one or more files, sorts them according to the specified
keys, and makes the sorted records available either through an output procedure or in an output file.

For sorting **tables**, the `SORT` statement sorts table elements according to specified table keys.

### The formats of the SORT statement
The `SORT` statement has two formats,
the first format is not supported for programs that are compiled with the `THREAD`
option, while the second can be used with programs that are compiled with the `THREAD` option.

both formats can be used anywhere in the `PROCEDURE DIVISION`.

#### **Format 1 for sorting files:**
![Sort statement format](Images/sort-and-merge/SORT-format-1.png)
_Format 1 of a sort statement_

`file-name-1`:
- the sort process uses this file as temporary file during the sort
- The name given in the SD entry that describes the records to be sorted.


No pair of **file-names** in a `SORT` statement can be specified in the same **SAME SORT AREA** clause or the SAME
**SORT-MERGE AREA** clause.

**File-names** associated with the **GIVING** clause (file-name-3) cannot be specified in the **SAME AREA** clause,
however, they can be associated with the **SAME RECORD AREA** clause.

#### **Format 2 for sorting Tables:**
![Sort statement format](Images/sort-and-merge/SORT-format2.png)
_Format 2 of a sort statement_

`data-name-2`
- Specifies a table data-name that is subject to the following rules:
    - must have an `OCCURS` clause in the data description entry.
    - can be qualified.
    - can be subscripted.
        - The rightmost or only subscript of the table must be omitted or replaced with the word `ALL`.


The number of occurrences of table elements that are referenced by `data-name-2` is determined by the rules in the
**OCCURS** clause.

The sorted table elements are placed in the same table that is referenced by **data-name-2**.

The `SORT` statement sorts the table that is referenced by `data-name-2` and presents the sorted table in` data-name-2`.

The sorting order is determined by either the `ASCENDING` and `DESCENDING` phrases (if specified), or by the KEY phrase
that is associated with `data-name-2`.

--------

### INPUT PROCEDURE phrase
This phrase specifies the name of a procedure that is to select or modify input records before the sorting operation
begins.

`procedure-name-1`
- Specifies the first (or only) section or paragraph in the input procedure.

`procedure-name-2`
- Identifies the last section or paragraph of the input procedure.

This procedure can consist of any procedure needed to select, modify, or copy the records that are made available one
at a time by the `RELEASE` statement to the file referenced by `file-name-1`.

If an input procedure is specified, control is passed to the input procedure before the file referenced by `file-name-1`
is sequenced by the `SORT` statement

The compiler inserts a return mechanism at the end of the last statement in the input procedure.

When control passes the last statement in the input procedure, the records that have been released to the file
referenced by `file-name-1` are sorted.

The range includes all statements that are executed as the result of a transfer of control by `CALL`, `EXIT`, `GO TO`,
`PERFORM`, and XML PARSE statements in the range of the input procedure, as well as all statements in declarative
procedures that are executed as a result of the execution of statements in the range of the input procedure.


The range of the input procedure must not cause the execution of any `MERGE`, `RETURN`, or format 1 `SORT` statement.

### DUPLICATES phrase
#### Format 1
If the `DUPLICATES` phrase is specified, and the contents of all the key elements associated with one record are equal
to the corresponding key elements in one or more other records, the order of return of these records is as follows:
- The order of the associated input files as specified in the SORT statement, and Within a given file the order is that
  in which the records are accessed from that file.
- or, the order in which these records are released by an input procedure, **when an input procedure is specified.**

If the `DUPLICATES` phrase is not specified, the order of these records is undefined.

#### Format 2
The contents of table elements are in the relative order that is the same as the order before sorting operation,
When both of the following conditions are met:
1. The `DUPLICATES` phrase is specified
2. The contents of all the key data items that are associated with one table element are equal to the contents of
   corresponding key data items that are associated with one or more other table elements.

If the `DUPLICATES` phrase is not specified and the second condition exists, the relative order of the contents of these
table elements is undefined.

### ASCENDING KEY and DESCENDING KEY phrases (format 2)
For format 1 these phrases are exactly the same as Merge KEY phrases, so it will be discussed in a joined subchapter at
the end of the chapter.

**For Format 2:**

This phrase specifies that **table** elements are to be processed in ascending or descending sequence,
based on the specified phrase and sort keys.

The KEY phrase can be omitted only if the description of the table that is referenced by `data-name-2` contains a KEY
phrase.

`data-name-1`
- Specifies a KEY data name that is subject to the following rules:
    - The data item that is identified by a key data-name must be the same as, or subordinate to, the data item that is
      referenced by `data-name-2`.
    - KEY data items can be qualified.

If the data item that is identified by a KEY data-name is subordinate to `data-name-2`, the following rules apply:
- The data item cannot be described with an `OCCURS` clause.
- The data item cannot be subordinate to an entry that is also subordinate to `data-name-2` and that contains an `OCCURS` clause.

The words `ASCENDING` and `DESCENDING` are transitive across all occurrences of `data-name-1` until another word
`ASCENDING` or `DESCENDING` is encountered.


The data items that are referenced by `data-name-1` are key data items, and these data items determine the order in which
the sorted table elements are stored.

The order of significance of the keys is the order in which data items are specified in the `SORT` statement, without
regard to the association with `ASCENDING` or `DESCENDING` phrases.

To determine the relative order in which table elements are stored, the contents of corresponding key data items are
compared according to the rules for comparison of operands in a relation condition.

The sorting starts with the most significant key data item with the following rules:
- If the contents of the corresponding key data items are not equal and the key is associated with the `ASCENDING`
  phrase, the table element that contains the key data item with the lower value has the lower occurrence number.

- If the contents of the corresponding key data items are not equal and the key is associated with the `DESCENDING` phrase,
  the table element that contains the key data item with the higher value has the lower occurrence number.
- If the contents of the corresponding key data items are equal, the determination is based on the contents of the next
  most significant key data item.

If the KEY phrase is not specified, the sequence is determined by the KEY phrase in the data description entry of the
table that is referenced by `data-name-2`.

If the KEY phrase is specified, it overrides any KEY phrase specified in the data description entry of the table that
is referenced by `data-name-2`.

If `data-name-1` is omitted, the data item that is referenced by `data-name-2` is the key data item.

Format 2 `KEY` data items cannot be:
- Class object or pointer
- USAGE OBJECT, USAGE POINTER, USAGE PROCEDURE-POINTER, or USAGE FUNCTION-POINTER
- Subscripted

### `SORT` Example: Sorting an employee data file
Let's say for example I have a file with the company employee data, and I want to sort it using the first name in
**ASCENDING** order.

1. in the `ENVIRONMENT DIVISION` we need to define
    - **the file to be sorted** (the input file),
    - the **sorted file**,
    - and the **work file**
    ```
      ENVIRONMENT DIVISION.
             FILE-CONTROL.
             SELECT EMPLOYEEFILE ASSIGN TO "employees.DAT"   
             ....
             SELECT SORTEDEMPLOYEES ASSIGN TO "sortedEmployees.DAT"
             ....
             SELECT WORKFILE ASSIGN TO "WORK.TMP".
             ....

    ```    
    - Note: *the data in the work file will be released when the program is done*


2. in the `DATA DIVISION` we put the file description for the input, output, and working files
    ```
    DATA DIVISION.
    FILE SECTION.
    FD EMPLOYEEFILE.
    ....
    FD SORTEDEMPLOYEES.
    ....
    SD WORKFILE.
    01 WORKREC
       ....
       02 WSEMPLOYEEFNAME PIC X(10).
       02 WSEMPLOYEELNAME PIC X(10).
       .....
    ```
- Note: *the work file is defined with `SD` and the definition of it contains
  the names of the filed we want to use for the sorting*


3. in the `PROCEDURE DIVISION` we sort the work file on a key using the input file and given the output file
    ```
     PROCEDURE DIVISION.
            BEGIN. 
                SORT WORKFILE ON ASCENDING KEY WSEMPLOYEEFNAME
                    USING EMPLOYEEFILE GIVEN SORTEDEMPLOYEES.
            .....
    ```

    - the sort is done using the first name, we can use more than one key by adding the keys one after the other separated by comma
        - ex. sorting on first name and last name
           ```
              SORT WORKFILE ON ASCENDING KEY WSEMPLOYEEFNAME, WSEMPLOYEELNAME 
                USING EMPLOYEEFILE GIVEN SORTEDEMPLOYEES.
           ```

## MERGE
The `MERGE` statement combines two or more identically sequenced files on one or more keys and makes records available
in merged order to an output procedure or output file.

The files must have already been sorted according to an identical set of ascending or descending keys before it is merged.

A `MERGE` statement can appear anywhere in the `PROCEDURE DIVISION` except in a declarative section, and
is not supported for programs compiled with the `THREAD` compiler option.

### The Merge statement Format
![MERGE statement format](Images/sort-and-merge/MERGE-format.png)
- _Merge statement format_


`file-name-1`
- The name given in the `SD` entry that describes the records to be merged.

No file-name can be repeated in the **MERGE** statement.

No pair of file-names in a **MERGE** statement can be specified in the same `SAME AREA`,
`SAME SORT AREA`, or SAME `SORT-MERGE AREA` clause.

However, any file-names in the `MERGE` statement can be specified in the
same `SAME RECORD AREA` clause.

When the `MERGE` statement is executed, all records contained in `file-name-2`, `file-name-3` are
accepted by the merge program and then merged according to the keys specified.

### Merge Example: merging employee, and interns data files together
Two files (employees, interns) are to be merged together by first and last name:
1. in the `ENVIRONMENT DIVISION` we define
    - **the files to be sorted** (the input file),
    - the **sorted file**,
    - the **work file**
    ```
    ENVIRONMENT DIVISION.
    FILE-CONTROL.
    SELECT EMPLOYEEFILE ASSIGN TO "employees.DAT"   
    ....
    SELECT INTERNFILE ASSIGN TO "interns.DAT"   
    ....
    SELECT SORTEDEMPLOYEES ASSIGN TO "sortedEmployees.DAT"
    ....
    SELECT WORKFILE ASSIGN TO "WORK.TMP".
    ....

    ```

2. in the `DATA DIVISION` we put the file description for the input, output, and working files
    ````
    DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEEFILE.
       ....
       FD INTERNFILE.
       ....
       FD SORTEDEMPLOYEES.
       ....
       SD WORKFILE.
       01 WORKREC
           ....
           02 WSEMPLOYEEFNAME PIC X(10).
           02 WSEMPLOYEELNAME PIC X(10).
           .....
    
    ````

3. in the `PROCEDURE DIVISION` we sort the work file on a key or keys
   using the input file and given the output file
    ```
    PROCEDURE DIVISION.
    .....
        MERGE WORKFILE ON ASCENDING KEY
            WSEMPLOYEEFNAME, WSEMPLOYEELNAME
            USING EMPLOYEEFILE INTERNFILE
            GIVEN SORTEDEMPLOYEES.
    .....
    
    ```

- Note: the output can be written to one or more file or process internally by the  program

## SORT & MERGE
The `SORT` and `MERGE` have some common Syntax and behaviour and follow some rules in the same way, so instead of discussing
them twice for both subchapter, they will be discussed here and can be applied to both.

### USING phrase
`file-name-2` in the sort statement, and `file-name-2` , `file-name-3` in the merge statement are the input files.

At the time the statement is executed, these files must not be open.

The input files are automatically opened, read, and closed.

All input files must be described in `FD` entries in the `DATA DIVISION`.

During the operation, all the records on the input files are transferred to `file-name-1`.

If `file-name-1` contains variable-length records, the size of the records contained in the input files must be neither
less than the smallest record nor greater than the largest record described for `file-name-1`.

If `file-name-1` contains fixed-length records, the size of the records contained in the input files must not be greater
than the largest record described for `file-name-1`.

### GIVING phrase
`file-name-3` in the sort statement, and `file-name-4` in the merge statement are the output files.

When the `GIVING` phrase is specified, all the merged or sorted records in `file-name-1` are automatically transferred to
the output files.

All output files must be described in `FD` entries in the `DATA DIVISION`.

If the output files contain variable-length records, the size of the records contained in `file-name-1` must be neither
less than the smallest record nor greater than the largest record described for the output files.

If the output files contain fixed-length records, the size of the records contained in `file-name-1` must not be
greater than the largest record described for the output files.

At the time the `SORT` or `MERGE` statement is executed, the output files must not be open, the output files are
automatically opened, written to, and closed.

### OUTPUT PROCEDURE phrase
This phrase specifies the name of a procedure that is to select or modify output records from the sort or merge operation.

If an output procedure is specified, control passes to it after the file referenced by `file-name-1` has been sequenced
by the `SORT` or `MERGE` statement.


`procedure-name-1`
- Specifies the first (or only) section or paragraph in the `OUTPUT PROCEDURE`.

`procedure-name-2`
- Identifies the last section or paragraph of the `OUTPUT PROCEDURE`.


The `OUTPUT PROCEDURE` can consist of any procedure needed to select, modify, or copy the records that are made
available one at time by the `RETURN` statement in order from the file referenced by `file-name-1`.

The range includes all statements that are executed as the result of a transfer of control by `CALL`, `EXIT`, `GO TO`,
`PERFORM`, and XML PARSE statements in the range of the output procedure.


The range also includes all statements in declarative procedures that are executed as a result of the execution of
statements in the range of the output procedure.


The range of the output procedure must not cause the execution of any `MERGE`, `RELEASE`, or format 1 SORT statement.


The compiler inserts a return mechanism at the end of the last statement in the output procedure and when control passes
the last statement in the output procedure, the return mechanism provides the termination of the sort or merge and then
passes control to the next executable statement after the statement.

The **RETURN** statements in the output procedure are the requests for the next record.

Before entering the output procedure, the procedure reaches a point at which it can select the next record in order when
requested.

The last statement in an `OUTPUT PROCEDURE` can be the `EXIT` statement.

The `OUTPUT PROCEDURE` phrase is similar to a basic `PERFORM` statement.

if you name a procedure in an `OUTPUT PROCEDURE`, that procedure is executed during the sorting or merging operation
just as if it were named in a `PERFORM` statement.

### ASCENDING/DESCENDING KEY phrase
This phrase specifies that records are to be processed in ascending or descending sequence
(depending on the phrase specified), based on the specified keys.

`data-name-1`
- Specifies a KEY data item on which the merge, or sort will be based.
- Each such data-name must identify a data item in a record associated with `file-name-1`.

The direction of the merge operation depends on the specification of the `ASCENDING` or `DESCENDING` keywords as follows:
- When `ASCENDING` is specified, the sequence is from the lowest key value to the highest key value.
- When `DESCENDING` is specified, the sequence is from the highest key value to the lowest key value.

The data-names following the word KEY are listed from left to right in the statement in order of decreasing
significance without regard to how they are divided into KEY phrases.

The leftmost **data-name** is the major key, the next **data-name** is the next most significant key, and so forth.

#### KEYs rules
- A specific KEY data item must be physically located in the same position and have the same data format in each input file.
  However, it need not have the same data-name.
- If `file-name-1` has more than one record description, the KEY data items need be described in only one of the record descriptions.
- If `file-name-1` contains variable-length records, all the KEY data-items must be contained within the first n character positions
  of the record, where n equals the minimum records size specified for `file-name-1`.
- KEY data items must not contain an `OCCURS` clause or be subordinate to an item that contains an `OCCURS` clause.
- KEY data items can be qualified.
- If the KEY data item is described with usage NATIONAL, the sequence of the KEY values is based on the binary values
  of the national characters.
- If the KEY data item is internal floating point, the sequence of key values will be in numeric order.

When the `COLLATING SEQUENCE` phrase is not specified, the key comparisons are performed according to the rules for
comparison of operands in a relation condition.

When the `COLLATING SEQUENCE` phrase is specified, the indicated collating sequence is used for key data items of
alphabetic, alphanumeric, alphanumeric-edited, external floating-point, and numeric-edited categories.

For all other key data items, the comparisons are performed according to the rules for comparison of operands in a
relation condition.

#### KEYs types
KEY data items can be any of the following data categories:
- Alphabetic, alphanumeric, alphanumeric-edited
- Numeric (except numeric with usage NATIONAL)
- Numeric-edited (with usage DISPLAY or NATIONAL)
- Internal floating-point or display floating-point
- National or national-edited

KEY data items cannot be:
- Variably located
- Group items that contain variable-occurrence data items
- Category numeric described with usage NATIONAL (national decimal type)
- Category external floating-point described with usage `NATIONAL` (national floating-point)
- Category `DBCS`
- Dynamic-length elementary items
- Dynamic-length group items

### COLLATING SEQUENCE phrase
This phrase specifies the collating sequence to be used in alphanumeric comparisons for the KEY data items in the operation.

The `COLLATING SEQUENCE` phrase has no effect for keys that are not alphabetic or alphanumeric.

When the `COLLATING SEQUENCE` phrase is omitted, the PROGRAM `COLLATING SEQUENCE` clause (if specified) in the
`OBJECT-COMPUTER` paragraph identifies the collating sequence to be used.

When both the `COLLATING SEQUENCE` phrase of the statement and the PROGRAM `COLLATING SEQUENCE` clause of the
`OBJECT-COMPUTER` paragraph are omitted, the **EBCDIC** collating sequence is used.

`alphabet-name-1`
- Must be specified in the `ALPHABET` clause of the `SPECIAL-NAMES` paragraph.
  Any one of the alphabet-name clause phrases can be specified, with the following results:
- `STANDARD-1`
    - The **ASCII** collating sequence is used for all alphanumeric comparisons.

- `STANDARD-2`
    - The 7-bit code defined in the International Reference Version of ISO/IEC 646, 7-bit coded character set for
      information interchange is used for all alphanumeric comparisons.
- `NATIVE`
    - The **EBCDIC** collating sequence is used for all alphanumeric comparisons.
- `EBCDIC`
    - The EBCDIC collating sequence is used for all alphanumeric comparisons.
- `literal`
    - The collating sequence established by the specification of literals in the `ALPHABET-NAME` clause is used for all
      alphanumeric comparisons.

### Segmentation considerations
If a `SORT` or `MERGE` statement is coded in a **fixed** segment, any input or output procedure referenced by that statement
must be:
- totally within a fixed segment
- or wholly contained in a single independent segment.


If the statement is coded in an **independent** segment, any input or output procedure referenced by it must be:
- totally within a fixed segment
- or wholly contained within the **same** independent segment as that statement.

\newpage


# COBOL Challenges
As you have now handled some basic exercises, we have prepared a new section containing more advanced exercises that test your ability to resolve bugs and other issues in COBOL programs. Each exercise will have a short description and a goal to be accomplished.

Happy Coding!

- **COBOL Challenge - Debugging**
- **COBOL Challenge - The COVID-19 Reports**
- **COBOL Challenge - The Unemployment Claims**
- **Hacker News Rankings for Mainframe/COBOL Posts**

\newpage
## COBOL Challenge - Debugging

It is 2020 in Washington, D.C. John Doe runs a program which provides financial reports on US Presidents and tallies the number of reports from the state of Virginia. Everything seems OK. (see below)

![](Images/cobolch1-img2.png)

John is satisfied, as he can see that everything is working as it should be. He calls it a day and goes home.

The next day, when he comes back to the office, his colleague Mari tells him “I’ve made some changes to one of your programs so that it also tallies the number of presidents who spent more than their allowed limit. Check it out.”

He runs his usual reports and sees the following:

![](Images/cobolch1-img1.png)

Clearly, Mari’s changes to the program that generates the reports have broken something.

Can you fix the code to get the correct result? The new source code is named **CBL0106** and the JCL is **CBL0106J**. In case you get stuck, the solution is in the file **CBL0106C**. 

You can find them in the github repository for the COBOL course, in the subfolder **/COBOL Programming Course #2 - Advanced Topics/Challenges/Debugging**.

\newpage
## COBOL Challenge - The COVID-19 Reports

Today, you are tasked to create a COVID-19 Summary Report of all the countries around the world, using information from the COVID19API website.

### Instructions

1. Extract the response from this API: https://api.covid19api.com/summary. You will receive a JSON file that is similar to the image below:

    ![](Images/cobolchCOV19-img1.png)

2. Convert that file to CSV format. It should look like this. In my example, I only chose the “Countries” part.

    ![](Images/cobolchCOV19-img2.png)

3. Using Zowe, upload the CSV file to the mainframe.

**Hint:** You can use the command `zowe files ul ftds "file location" "dataset name"` to upload the CSV file to the mainframe.

4. Create a new member in your *.CBL data set to write your COBOL program.

**Hint:** You can create a member using Zowe Explorer or Zowe CLI.

5. Write a COBOL program that reads the uploaded CSV file and reformats it to display the contents like this:

    ```
    ********************************************************************
    DATE: 2020-04-22
    TIME: T16:54:5
    COUNTRY: "Antigua and Barbuda"
    COUNTRY CODE: "AG"
    SLUG: "antigua-and-barbuda"
    NEW CONFIRMED CASES: 00000
    TOTAL CONFIRMED CASES: 00023
    NEW DEATHS: 00000
    TOTAL DEATHS: 00003
    NEW RECOVERIES: 00004
    TOTAL RECOVERIES: 00007
    ********************************************************************
    ```

6. Compile and test your work.

### Advanced Tasks

If you want a more challenging approach, try the optional tasks below:

- Reformat the data into a Report Form like this:

![](Images/cobolchCOV19-img3.png)

- Automate. Using NPM and Zowe CLI, run all these steps and create a “one click” COBOL build similar to this:

    ![](Images/cobolchCOV19-img4.gif)

### Solution

To check the solution, refer to the blog post [here](https://medium.com/@jessielaine.punongbayan/solution-covid-19-reports-cobol-challenge-6c509579e3fe?source=friends_link&sk=5a662034a03c91d639b77267ed6abfc9).

Happy Coding!

_Disclaimer: This challenge is also posted in [Medium.com](https://medium.com/@jessielaine.punongbayan/cobol-challenge-covid-19-reports-ee03a946bd23)._

\newpage
## COBOL Challenge - The Unemployment Claims

Now let's try a more advanced challenge! Your task is to create an end-to-end solution. Our end goal is to build an application that will fire Zowe APIs to the mainframe and display the result in the application. This is how the flow would look:

![](Images/cobolchClaims-img1.png)

_Of course, you do not have to complete the whole challenge if you do not want to. But it would be great if you do_

### Our Data

The data that we are going to use will come from https://www.data.gov/. According to their website, this is a repository of data that is available for public use. For more information, please visit their website.

To be more specific, we are going to get the monthly unemployment claims of the state of Missouri.
I chose this because it is separated according to different categories:

- **By Age:** https://catalog.data.gov/dataset/missouri-monthly-unemployment-claims-by-age-d20a7
- **By Ethnicity:** https://catalog.data.gov/dataset/missouri-monthly-unemployment-claims-by-ethnicity-2a03b
- **By Industry:** https://catalog.data.gov/dataset/missouri-monthly-unemployment-claims-by-industry-80e86
- **By Race:** https://catalog.data.gov/dataset/missouri-monthly-unemployment-claims-by-race-32ab3
- **By Gender:** https://catalog.data.gov/dataset/missouri-monthly-unemployment-claims-by-sex-f5cb6

You can consume the data in different formats such as CSV, RDF, JSON or XML. You can choose whatever format you like.

### Use Case

You are given a new set of data for The Unemployment Claims. Your tasks are as follows:

- To create a new database for the new set of data and combine the data based on the Record ID field.
- To provide a way for other COBOL programs and other applications to access this newly created database.
- To create a report specifying all the information available in the newly created database. The report will contain, but not be limited to, the following information: Record ID, Age, Ethnicity, Industry, Race and Gender.

### Instructions

1. Create a database. This can be done in various ways but the easiest one the I could think of is a VSAM file. First, create a COBOL program that will consume your data. Then, using the RECORD-ID as the key (which is more visible when reading the CSV file), create a KSDS VSAM file and store all the information there.

    _What this means is that one record will have the RECORD-ID as the key and all the information from The Monthly Unemployment Claims (Fields from Age, Ethnicity, Industry, Race and Gender) will be added or connected to the RECORD-ID._

    The flow would look like this:

    ![](Images/cobolchClaims-img2.png)

2. Create a COBOL sub-routine. This program will allow other programs to read the data from the VSAM file. This sub-routine should be able to perform the following tasks:  
    - accept requests to get information about a specific record ID.
    - _(Optional)_ accept requests to get information about all the records inside the database. What does this mean? It means that instead of providing a record ID, I could provide an indicator that I want to create a report of all the records inside the database.

    ![](Images/cobolchClaims-img3.png)

    _The purpose of the COBOL sub-routine is to allow other COBOL programs or other application to access the information inside the VSAM file._

3. Create a Main COBOL Program. This program will create a report based on the records inside the newly created database. The process is as follows:  
    - The program calls the COBOL sub-routine passing the Record ID or, optionally, an indicator that you want to print all records in the database.
    - It receives the response from the sub-routine.
    - It processes the response and generates a report. This report can be a formal report or just a display in the SYSOUT. It’s up to you.

    The flow should look like this:

    ![](Images/cobolchClaims-img4.png)

4. Create your JCLs.

    _By this point, if you choose to do the exercise using COBOL programs only, you should be able to read the data from your VSAM file, process it and generate a report. The generated report could be an information of a specific record or multiple records._

5. _(Optional)_ Create an application. It can be any type of application; a Mobile App, a Web App or an Electron App. It is up to you. In this application you should be able to view a record by providing a RECORD-ID. The flow would be similar to Step #3.

    This is an example of a possible application design:

    ![](Images/cobolchClaims-img5.png)


    **Hint:** How can I accomplish this? By using the Zowe CLI NPM package, you can fire Zowe APIs that submit your JCLs and get the results. From there, you can view the output and display it in your application. This article can provide a good example.

    **Hint:** What APIs am I going to use? You will use the Jobs Submit API and View Jobs Spool or View Dataset API. For more information, please visit [this site](https://docs.zowe.org/stable/web_help/index.html).

6. _(Optional)_ Create a CI/CD process that will create a nightly build of your application. [This article](https://medium.com/modern-mainframe/beginners-guide-cobol-made-easy-leveraging-open-source-tools-eb4f8dcd7a98?source=friends_link&sk=443517b1feaba8e392f5807246c25ca4) can help explain that process.

    Sample CI/CD Build using CircleCI:

    ![](Images/cobolchClaims-img6.gif)

### Craving more programming challenges?

Add more functionality to your COBOL Sub-routine like:

- Insert a new record
- Update an existing record
- Delete an existing record

I hope that by taking this challenge, you will be able to learn something new! 

Happy Coding!

_Disclaimer: This challenge is also posted in [Medium.com](https://medium.com/@jessielaine.punongbayan/zowe-cobol-challenge-the-unemployment-claims-2e35a42eabaa)._

\newpage
## Hacker News Rankings for Mainframe/COBOL Posts

![](Images/hacker-img1.png)

We will explore the popular Hacker News website for this challenge. Hacker News is an online community started by Paul Graham for sharing "Anything that good hackers would find interesting. That includes more than hacking and startups".


### A Little Background 
The site offers a dynamic list of posts/stories, submitted by users, each of which could be expanded into its own unique comment thread. Readers can upvote or downvote links and comments, and the top thirty links are featured on the front page. Today, more than five million people read Hacker News each month, and landing a blog post on the front page is a badge of honor for many technologists.

### Our Goal
We will be working on a Hacker News 2015-2016 dataset from Kaggle with a full year’s worth of stories:  Our goal is to extract only the Mainframe/COBOL related stories and assign ranking scores to them based on (a simplified version) the published Hacker News ranking algorithm. We will create a front page report that reflects this ranking order. The algorithm works in a way that nothing stays on the front page for too long, so a story’s score will eventually drop to zero over time (the gravity effect). Since our posts are spread out over a year and as older posts will always have a lower (or zero) ranking, we will distort the data so all our stories have the same date and and consider only the times in the ranking score calculation. This will give all our posts a fair chance of landing the front page.  Our front page report is published at 11:59pm. [Here's some additional information on the ranking.](https://www.righto.com/2013/11/how-hacker-news-ranking-really-works.html)

### The Plan
 - There are different creative ways of accomplishing this but here’s our plan: We will have a COBOL program that reads the input CSV file and retrieves only the ***Mainframe/COBOL*** stories. It then calculates the ranking score for the stories by factoring in the time they were posted and the number of votes they received. Each of the records is then written to an output dataset along with the ranking score. 

 - We will then use `DFSORT` to sort the output dataset on ranking score, highest to lowest and display the posts as a simple report mimicking the front page. 
 
 Let's get started!
1. Take a look and familiarize yourself with the dataset on z/OS: `ZOS.PUBLIC.HACKER.NEWS`. This is a CSV file that serves as input to your COBOL program. The file was created by downloading [this Kaggle dataset](https://www.kaggle.com/hacker-news/hacker-news-posts), removing the lengthy `URL` column that is of no relevance to us and uploading it to z/OS. You can directly reference this DS in your JCL. Please avoid making a copy as it is fairly large with around 300,000 records.

 ![](Images/hacker-img2.png)
  
2.	Create your COBOL program in `<userid>.CBL` using VS Code with the Code4z extension installed and enabled – This program will :
      1. Read in each record in the input CSV file
      2. Select only the records that have mention of the words ***Mainframe*** or ***COBOL*** (ignore case) in the `Title` field
      3. Calculate the ranking score for each record based on the number of votes it received and the time it was posted (Ignore date as we assume all posts were created on the same date)
          
          ![](Images/hacker-img3.png)
      4. Write the record to an output file along with the ranking score

      ![](Images/hacker-img4.png)
      
3. Copy/Modify/Create a JCL in `<userid>.JCL` for compiling/linking and running the program against input/output datasets.

4. Submit the job (via `Zowe Explorer` or `Zowe CLI`), debug and test to create the output dataset.

5. Next add a new step in the JCL member to run the `DFSORT` utility on the output dataset from the previous step. The sort should be done on the ranking score field, from highest to lowest. Use `DFSORT` to also print headers for our front page. As this is a new utility not covered in the course, please check out these links to explore this very powerful and versatile tool:
 
   [Getting started with DFSORT](https://www.ibm.com/docs/en/zos/2.4.0?topic=dfsort-zos-getting-started)

   [Example with DFSORT](https://www.ibm.com/docs/en/zos/2.4.0?topic=examples-example-10-sort-outfil)


6. Run and debug until the front page looks ready! Which posts ranked among the highest? Here's a look at the generated report:

![](Images/hacker-img5.png)


Hope you have fun working on this Challenge. Happy COBOL coding!
