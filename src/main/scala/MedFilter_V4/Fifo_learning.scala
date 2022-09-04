import spinal.core._
import spinal.lib.StreamFifo
import spinal.lib.master
import spinal.lib.slave
class Fifo_Test extends Component{
	val Fifo = new StreamFifo(
        dataType = UInt(16 bits),
        depth = 32
      )
	val io=new Bundle{
		val Data_Out=master Stream(UInt(16 bits))
		val Data_In=slave Stream(UInt(16 bits))
	}
	Fifo.io.push<>io.Data_In
	Fifo.io.pop<>io.Data_Out
}