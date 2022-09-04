package MedFilter_V12


import spinal.core._
//提取三角形
class LTY_Pre_Process extends Component{//连通域预处理
    val Config=MemConfig()
    val io=new Bundle{
        val Lty_Nym=in UInt(Config.LTY_NUM_WIDTH bits)//上一层算完的连通域数量
    }

}
