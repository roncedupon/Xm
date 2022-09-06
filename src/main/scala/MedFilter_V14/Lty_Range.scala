package MedFilter_V13
import spinal.core._
//连通域排序模块

class Lty_Sort extends Component{
    val io=new Bundle{
        val para1=in UInt(16 bits)
        val para2=in UInt(48 bits)
        val para3=in UInt(48 bits)
        val para4=in UInt(32 bits)
        val para5=in UInt(16 bits)
        val para6=in UInt(16 bits)//连通域的六个参数
        val Lty_Num=in UInt(13 bits)//连通域数量，根据情况修改
        val sReady=out Bool()
        val sValid=in Bool()

    }
}
