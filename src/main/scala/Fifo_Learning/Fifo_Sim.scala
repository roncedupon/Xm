package Fifo_Learning
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import scala.collection.mutable.Queue
import spinal.lib.StreamFifo
import spinal.lib.master
import spinal.lib.slave
class Fifo_Test extends Component{
	val Fifo = new StreamFifo(//直接创建一个fifo
        dataType = UInt(16 bits),
        depth = 32
      )
  Fifo.io.pop.ready
	// val io=new Bundle{
	// 	val Data_Out=master Stream(UInt(16 bits))//输出数据
	// 	val Data_In=slave Stream(UInt(16 bits))//输入数据
	// }
	//Fifo.io.push<>io.Data_In
	//Fifo.io.pop <>io.Data_Out
}
object Sim_MyFifo {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Fifo_Test).doSim{ dut =>
       dut.clockDomain.forkStimulus(period = 10)
       SimTimeout(1000000*10)
       for(i <-0 to 100000){

        dut.clockDomain.waitSampling()//采样
       }

    }
  }
}
object SimStreamFifoExample {
  def main(args: Array[String]): Unit = {
    // Compile the Component for the simulator.
    val compiled = SimConfig.withWave.allOptimisation.compile(
      rtl = new StreamFifo(
        dataType = Bits(32 bits),
        depth = 32
      )
    )

    // Run the simulation.
    compiled.doSimUntilVoid{dut =>
      val queueModel = Queue[Long]()

      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000000*10)

      // Push data randomly, and fill the queueModel with pushed transactions.
      val pushThread = fork {
        dut.io.push.valid #= false
        while(true) {
          dut.io.push.valid.randomize()
          dut.io.push.payload.randomize()
          dut.clockDomain.waitSampling()
          if(dut.io.push.valid.toBoolean && dut.io.push.ready.toBoolean) {
            queueModel.enqueue(dut.io.push.payload.toLong)
          }
        }
      }

      // Pop data randomly, and check that it match with the queueModel.
      val popThread = fork {
        dut.io.pop.ready #= true
        for(i <- 0 until 100000) {
          dut.io.pop.ready.randomize()
          dut.clockDomain.waitSampling()
          if(dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean) {
            assert(dut.io.pop.payload.toLong == queueModel.dequeue())
          }
        }
        simSuccess()
      }
    }
  }
}