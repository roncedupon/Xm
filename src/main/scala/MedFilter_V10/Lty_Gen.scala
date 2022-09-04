package MedFilter_V10
import spinal.core._
import spinal.lib.slave
import Archive.WaCounter
class Lty_Bram extends BlackBox{
    val Config=MemConfig()//浮点乘法器
    val io=new Bundle{//component要求out有驱动，但是black box不要求out的驱动
        val CLK=in Bool()
        // val addra=in UInt(Config.Mu)
        val A=in UInt(Config.MUL_A_IN bits)
        val B=in UInt(Config.MUL_B_In bits)
        val P=out UInt(Config.MUL_P_OUT bits)
    }
    noIoPrefix()
    mapClockDomain(clock=io.CLK)
}
object LTY_ENUM extends SpinalEnum(defaultEncoding = binaryOneHot) {
  val IDLE, INIT, LOAD_2_ROWS,EXTRACT_LTY,JUDGE_LAST_ROW,WAIT_NEXT_READY = newElement
  //WAIT_NEXT_READY：等待下一个模块ready（可以认为LTY参数累加模块ready了）
}
case class LTY_FSM(start: Bool) extends Area {
  

  val currentState = Reg(LTY_ENUM()) init LTY_ENUM.IDLE
  val nextState = LTY_ENUM()
  currentState := nextState

  val Init_End=Bool()
  val last_Row = Bool()  //是否为最后一行
  val Row_2_End=Bool()//前两行加载完
  val Col_End=Bool()//加载完一行
  val Load_2_Row_End=Bool()//加载完两行
  val Next_Ready=Bool()//累加模块准备好接受数据，ready了
  switch(currentState){
    is(LTY_ENUM.IDLE){
        when(start){
            nextState:=LTY_ENUM.INIT
        }otherwise{
            nextState:=LTY_ENUM.IDLE
        }
    }
    is(LTY_ENUM.INIT){
        when(Init_End){
            nextState:=LTY_ENUM.LOAD_2_ROWS//初始化完成就加载前两行
        }otherwise{
            nextState:=LTY_ENUM.INIT
        }
    }
    is(LTY_ENUM.LOAD_2_ROWS){
        when(Col_End){
            nextState:=LTY_ENUM.JUDGE_LAST_ROW
        }otherwise{
            nextState:=LTY_ENUM.LOAD_2_ROWS
        }
    }
    is(LTY_ENUM.JUDGE_LAST_ROW){
        when(last_Row){
            nextState:=LTY_ENUM.IDLE//最后一行就结束了
        }elsewhen(Load_2_Row_End){
            nextState:=LTY_ENUM.WAIT_NEXT_READY//等待累加模块准备好接受计算数据
        }otherwise{
            nextState:=LTY_ENUM.LOAD_2_ROWS//前两行没加载完继续加载前两行
        }
    }
    is(LTY_ENUM.WAIT_NEXT_READY){
        when(Next_Ready){
            nextState:=LTY_ENUM.EXTRACT_LTY
        }otherwise{
            nextState:=LTY_ENUM.WAIT_NEXT_READY//下一个模块没准备好，就一直等着
        }
    }
    is(LTY_ENUM.EXTRACT_LTY){
        when(!Next_Ready){
            nextState:=LTY_ENUM.WAIT_NEXT_READY
        }elsewhen(Col_End){
            nextState:=LTY_ENUM.JUDGE_LAST_ROW
        }otherwise{
            nextState:=LTY_ENUM.EXTRACT_LTY
        }
    }
    
  }
}
class Lty_Mark_Gen extends Component{//连通域标记
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(32 bits))//进来的数据
        val start=in Bool()//lty计算启动信号
    }
    val Fsm=LTY_FSM(io.start)
//状态机相关====================================================================
    val Col_Cnt=WaCounter(io.sData.valid&&io.sData.ready, log2Up(Config.LTY_COL_NUM), Config.COl_CNT_NUM-1)//创建列计数器
    val INIT_CNT=WaCounter(Fsm.currentState === LTY_ENUM.INIT, 3, 5)//初始化计数器,数五拍
    val Row_Cnt_All=WaCounter(Col_Cnt.valid,log2Up(Config.LTY_ROW_NUM),Config.LTY_ROW_NUM-1)//行计数器
    Fsm.Init_End:=INIT_CNT.valid
//创建标记矩阵---并行度为2，所以需要先加载两行=====================================
    val Up_Mark_Mem=Mem(UInt(Config.BRAM_IN_DATA_WIDTH bits),wordCount=Config.BRAM_DEPTH)//标记矩阵





}
class Lty_Mark_Sub_Module extends Component{//标记子模块
    val io=new Bundle{
        val Up_mark=in UInt(16 bits)//上标记

        val Left_Mark=in Vec(UInt(16 bits))
        val Lty_Total_NUm=in UInt(16 bits)//连通域总数量
        val Mark_Out=out UInt(16 bits)//输出标记点
        val Left_Mark_Valid=out Vec(Bool())
    }


    when(io.Up_mark === 0 && io.Left_Mark(0)===0) {//上面和左边都没被标记
        io.Mark_Out:=io.Lty_Total_NUm+1
    }.elsewhen(io.Up_mark === 0 && io.Left_Mark(0)=/=0) {//上面没被标记，左边被标记了,那么当前点的标记就应该和左边点标记一样
        io.Mark_Out:=io.Left_Mark(0)
    }.elsewhen(io.Up_mark =/= 0 && io.Left_Mark(0) === 0) {//上面点被标记，左边点没被标记，将当前点标记为上面的点
        io.Mark_Out:=io.Up_mark
        for(i<-0 to 3){
            io.Left_Mark_Valid(i):=(io.Left_Mark(i)===io.Up_mark)?True|False
        }
    }



}
class Lty_Para_Accu extends Component{
    //连通域参数累加
}