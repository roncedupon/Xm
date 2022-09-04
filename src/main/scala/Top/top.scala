package Top

import spinal.core._
import MedFilter_V5.MedFilter_V5
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import Config.TotalConfig
class top extends Component{
	val MedFilter=new MedFilter_V5
	val Config=TotalConfig()
	val io=new Bundle{
		val sData=slave Stream(UInt(Config.DATA_WIDTH bits))
        val mData=out Vec(UInt(Config.DATA_WIDTH bits), 2)
		val mValid=out Bool()//输出数据是否有效信号
        val start=in Bool()
        
	}
}