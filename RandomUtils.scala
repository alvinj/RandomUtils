package com.alvinalexander.utils

import scala.util.Random

/**
 * TODO: ADD TESTS FOR ALL OF THESE.
 */
object RandomUtils:

    /**
     * Gets a random element from `seq`.
     */
    def getRandomElement[A](seq: Seq[A], random: Random): A = 
        seq(random.nextInt(seq.length))
    
    def randomizeSequence[A](seq: Seq[A]) =
        Random.shuffle(seq)

    def randomTrueOrFalse() =
        Random.shuffle(List(true, false))
              .head

    /**
     * Returns a randomly positive or negative version
     * of the Int value that’s passed in. So if you pass
     * in `1`, it will return `1` or `-1`.
     */
    def randomPositiveOrNegativeValue(i: Int): Int =
        if randomTrueOrFalse() then i else -i

    def randomIntBetween(min: Int, max: Int): Int =
        Random.nextInt((max - min) + 1) + min

    def createRandomAlphanumericString(strlen: Int): String =
        Random.alphanumeric.take(strlen).mkString

    def createRandomLengthRange(maxValue: Int): Range = 
        0 to randomIntBetween(1, maxValue)

    /**
     * Creates a Seq[Int] with length of at least `minSeqLength`
     * up to a max length of `maxSeqLength`, and the values can
     * go up to `maxSeqValue`.
     */
    def createRandomLengthSeqOfRandomValues(
        minSeqLength: Int =  1,
        maxSeqLength: Int = 20,
        maxSeqValue: Int = 1000
    ): Seq[Int] =
        val randomLength = randomIntBetween(minSeqLength, maxSeqLength)
        for i <- 1 to randomLength yield r.nextInt(maxSeqValue)

    /**
     * Produces random-length strings like these:
     *    genRandomVariableLengthStringWithBlankSpaces(r, 3, 15, "abcdef")
     *    "bff bcf"
     *    "f cbfe cff"
     */
    def genRandomVariableLengthStringWithBlankSpaces(
        r: Random,
        minLength: Int,
        maxLength: Int,
        charsToUse: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    ): String =
        val ab = scala.collection.mutable.ArrayBuffer[Char]()
        val strLength = randomIntBetween(minLength, maxLength)
        for i <- 1 to strLength do
            if i % 5 == 0 then
                ab.append(' ')
            else
                // ab.append(r.nextPrintableChar)
                val randomChar = charsToUse(Random.nextInt(charsToUse.length))
                ab.append(randomChar)
        val charSeq: scala.collection.mutable.Seq[Char] = Random.shuffle(ab)
        charSeq.mkString

    /**
     * Call like this:
     *     val uniqueRandomNumbers = generateUniqueRandomNumbers(20, 1, 25)
     * Something like `require` is needed to keep from creating
     * an infinite loop.
     */
    def generateUniqueRandomNumbers(
        count: Int, 
        minInclusive: Int, 
        maxExclusive: Int
    ): Vector[Int] =
        require(
            maxExclusive - minInclusive + 1 >= count, 
            "Range is too small to generate the desired number of unique random numbers"
        )
        val random = Random()
        var numbers = Set.empty[Int]
        while
            numbers.size < count
        do
            numbers += random.nextInt((maxExclusive - minInclusive) + 1) + minInclusive
        numbers.toVector


end RandomUtils




