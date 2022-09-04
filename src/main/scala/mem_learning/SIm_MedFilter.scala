package mem_learning

//Your hardware toplevel
import spinal.core._
import spinal.sim._
import spinal.core.sim._

object Sim_MedFilter {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Mem_Medfilter).doSim{ dut =>
      // Simulation code here
      dut.clockDomain.forkStimulus(10)//设置
      // dut.clockDomain.assertReset()
      for(i <- 0 to 2048*20) {
        dut.clockDomain.waitSampling()
        if(i==5){
          dut.io.start#=true
        }else{
          dut.io.start#=false
        }
        if(i>20){
          dut.io.sData.valid#=true
          dut.io.sData.payload#=i-20
        }else{
          dut.io.sData.valid#=false
        }
        

      }


      // for(i <- 0 to 100) {
      //   dut.clockDomain.waitSampling()
      //   //println(dut.counter.toInt)
      // }
    }
  }
}