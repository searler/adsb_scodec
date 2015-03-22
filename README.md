# adsb_scodec
Processing ADS-B messages using scodec

## References

[openskynetwork](https://github.com/openskynetwork/java-adsb)

[gr-air-modes](https://github.com/bistromath/gr-air-modes)

[Worked Example](http://www.lll.lu/~edward/edward/adsb/DecodingADSBposition.html)

## Caveats

The official specifications cost hundreds of dollars.

Google will find pieces of the specifications, generally in the form
of draft proposals. These often conflict with each other and the open source
code. For example, I found three different sets of values for the surface position containment
radius. 

The OSS implementations have minimal, if any, tests. The only searchable _test cases_ are  the
above worked example and some comments in the opensky documentation. 

The current implementation has a number of known limitations:

1. Ignores gilham encoding on altitude
2. No logic to perform CompactRepresentation processing on surface positions
3. Messsages are incomplete
4. Checksums are ignored
5. Limited testing


**This code is NOT production ready**

## Improvements over other OSS implementations

A Codec is implemented that provides both encoding and decoding.
This simplifies testing by allowing an instance to be round tripped.
The encoding functionality can be used to synthesis messages for testing other systems.

The data model is designed for clarity, type safety and immutability; with performance as a secondary concern.
Value classes are used provide type safe primitives with good performance.

[Scodec](http://scodec.org/) makes the required bit twiddling a trivial exercise. 

Tests are included.


## Implementation notes

### Scala

The code is cross coupled for 2.10 and 2.11.

Scala 2.11 has better Eclipse IDE and scodec support.

Apache Spark currently only supports Scala 2.10. 
This project originated as a basis for an Spark exploration.

### Scodec

The ADS-B messages do not have a neat structure, making it difficult to use the standard discriminated combinators.
The _choice_ combinator provides the required flexibility but with reduced efficiency since message is repeatedly processed
until a matching decoder is found. It also requires the ugly upcast. 

Custom _peek_ and _tc_ combinators are needed to extract the TypeCode value so the appropriate decoder can be ascertained
while retaining the value for computing details of the message (e.g. containment radius)

## Performance

A single core of an AMD Phenom II X6 1100T can parse ~20K messages per second, which handily exceeds the 1 Mbit/second Mode S
data rate. This appears to be sufficient to process all the aircraft airborne over the US or Europe.