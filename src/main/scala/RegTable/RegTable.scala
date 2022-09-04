package RegTable

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import Config.TotalConfig
import spinal.lib.bus.regif.BusInterface
import spinal.lib.bus.misc.SizeMapping
class RegTable extends Component{
	val Config=TotalConfig()
	val io=new Bundle{
		val Inst_In=slave(AxiLite4(log2Up(Config.REG_TABLE_ADDR_DEPTH), Config.REG_TABLE_DATA_WIDTH))
	}
	val bus = BusInterface(io.Inst_In, sizeMap = SizeMapping(Config.REG_TABLE_BASE_ADDR, Config.REG_TABLE_ADDR_DEPTH))//创建一个总线接口,映射范围是0~1MiB
	
}