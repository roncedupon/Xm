package mem_learning
//简单的mem测试，如果没有给读使能那么读使能默认为1  22/8/02
import spinal.core._
class Mem_learning extends Component{
	val io=new Bundle{
		val Data_In=in Bits(64 bits)//
		val Data_Out=out UInt(16 bits)
		val Data_In_Addr=in Bits(5 bits)
		val Data_In_En=in Bool()
		val Data_Out_Addr=in Bits(7 bits)
	}
	noIoPrefix()
	val mem = Mem(UInt(64 bits), wordCount = 32)
    mem.write(io.Data_In_Addr.asUInt, io.Data_In.asUInt, io.Data_In_En)//将输入的数据写入ram中
    //io.Data_Out := mem.readSync(io.Data_Out_Addr.asUInt).asBits//同步读
	mem.readSyncMixedWidth(io.Data_Out_Addr.asUInt,io.Data_Out)
	//注意:没有给读使能那么读使能默认为1
}

