package mining

import scorex.crypto.hash.CryptographicHash32

import scala.math.BigInt
import scala.util.Random

class PoWMiner[HF <: CryptographicHash32](hashFunction: HF) {

  private val MaxTarget: BigInt = BigInt(1, Array.fill(32)((-1).toByte))

  def doWork(data: Array[Byte], difficulty: BigInt): ProvedData = {
    BigInt(1, hashFunction.hash(data))
    // найти такой nonce, что h = hash(nonce . data) и начинается с нулей длинной difficulty

    /*
    // Single thread
    val nonce = Iterator.iterate(Int.MinValue)(_ + 1) .find(x => validateWork(ProvedData(data, x), difficulty))
    */

    val maxThreads = 32
    val nonce = Seq.fill(maxThreads){
      Iterator.iterate(Random.nextInt())(_ + 1) .find(x => validateWork(ProvedData(data, x), difficulty))
    }.par.find(_.isDefined).get

    ProvedData(data, nonce.get)
  }

  def validateWork(data: ProvedData, difficulty: BigInt): Boolean = realDifficulty(data) >= difficulty

  private def realDifficulty(noncedData: ProvedData): BigInt =
    MaxTarget / BigInt(1, hashFunction.hash(noncedData.bytes))

}
