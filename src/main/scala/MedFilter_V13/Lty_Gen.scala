package MedFilter_V12
import spinal.core._
import spinal.lib.slave
import Archive.WaCounter
class Lty_Bram extends BlackBox{//黑盒，入32bit，出16 bit
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
  //Judge Last Row:每一行结束了都需要启动最后一行的判断
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
//大体思路：四块数据Bram，算时存-存时算
//首先加载完前两行，然后启动前两行的连通域提取，在提取前两行的连通域时也将后面两行的数据加载到另外两块Bram中
    //前两行连通域提取完后，切换到另外两个Bram，以此往复，从而实现2的并行度
class Lty_Mark_Gen extends Component{//连通域标记
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(32 bits))//进来的数据
        val start=in Bool()//lty计算启动信号
    }
    val Fsm=LTY_FSM(io.start)
//状态机相关====================================================================
    val Col_Cnt=WaCounter(io.sData.valid&&io.sData.ready, log2Up(Config.LTY_DATA_BRAM_DEPTH), Config.LTY_DATA_BRAM_DEPTH-1)//创建列计数器
    val INIT_CNT=WaCounter(Fsm.currentState === LTY_ENUM.INIT, 3, 5)//初始化计数器,数五拍
    val Row_Cnt_All=WaCounter(Col_Cnt.valid,log2Up(Config.LTY_ROW_NUM),Config.LTY_ROW_NUM-1)//行计数器
    val Bram_Write_Chose=WaCounter(Col_Cnt.valid,log2Up(4),3)//0，1，2，3循环写
    //一个Bram写选择器，一个Bram读选择器
    val Bram_Read_Chose=UInt(1 bits)//要准确控制才行
    Fsm.Init_End:=INIT_CNT.valid
//创建标记矩阵---并行度为2，所以需要先加载两行=====================================
    val Up_Mark_Mem=Mem(UInt(Config.LTY_MARK_BRAM_WIDTH bits),wordCount=Config.LTY_MARK_BRAM_DEPTH)//标记矩阵
//建立四个数据缓存Bram
    val Wr_En=Vec(Bool(),4)//循环写
    val mem=Array.tabulate(4)(i=>{
        def gen():Mem[UInt]={
            val mem=Mem(UInt(Config.BRAM_IN_DATA_WIDTH bits),wordCount=Config.BRAM_DEPTH)//
            mem.write(Col_Cnt.count,io.sData.payload,Bram_Write_Chose.count===i)//写地址,写数据,写使能都延迟了一拍
            
            mem
        }
        gen()
    })
    val RdData_Line1=Bram_Read_Chose.mux{//输出数据选择器
        0->mem(0).readSync(Col_Cnt.count)
        1->mem(3).readSync(Col_Cnt.count)
    }
    val RdData_Line2=Bram_Read_Chose.mux{//输出数据选择器
        0->mem(1).readSync(Col_Cnt.count)
        1->mem(2).readSync(Col_Cnt.count)
    }
//建立2个标记矩阵

}
class Lty_Mark_Sub_Module extends Component{//标记子模块
    val io=new Bundle{
        val Up_mark=in UInt(16 bits)//上标记

        val Left_Mark=in Vec(UInt(16 bits),4)//左边的四个标记点
        val Lty_Total_NUm=in UInt(16 bits)//连通域总数量
        val Mark_Out=out UInt(16 bits)//输出标记点
        val Left_Mark_Valid=out Vec(Bool())//
    }


    when(io.Up_mark === 0 && io.Left_Mark(0)===0) {//上面和左边都没被标记
        io.Mark_Out:=io.Lty_Total_NUm+1//那么这是一个新的连通域
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
object LtyGen extends App { 
    val verilog_path="./testcode_gen/MemGen" 
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Lty_Mark_Gen)
}