package test_package
import spinal.core._
import spinal.lib.slave
import Archive.WaCounter
import spinal.lib.master
class Lty_Bram extends BlackBox{//黑盒，入32bit，出16 bit
    val Config=MemConfig()//浮点乘法器
    
    val io=new Bundle{//component要求out有驱动，但是black box不要求out的驱动
        val clka=in Bool()
        val addra=in UInt(log2Up(Config.LTY_DATA_BRAM_A_DEPTH) bits)
        val dina=in UInt(Config.LTY_DATA_BRAM_A_WIDTH bits)
        val ena=in Bool()
        val wea=in Bool()

        //B口读使能一直有效----后来发现还是给个读使能比较方便
        val addrb=in UInt(log2Up(Config.LTY_DATA_BRAM_B_DEPTH) bits)
        val clkb=in Bool()
        val doutb=out UInt((Config.LTY_DATA_BRAM_B_WIDTH) bits)
        val enb=in Bool()
        
    }

    noIoPrefix()
    // Clock A is map on a specific clock Domain
    mapClockDomain(this.clockDomain, io.clka)
    // Clock B is map on the current clock domain
    mapCurrentClockDomain(io.clkb)
}
object LTY_ENUM extends SpinalEnum(defaultEncoding = binaryOneHot) {
  val IDLE, INIT, LOAD_2_ROWS,EXTRACT_LTY,JUDGE_LAST_ROW = newElement
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
  val Col_End=Bool()//输出完一行,这里要注意，这是输出完一行的意思
  val Load_2_Row_End=Bool()//加载完两行

  val Judge2Row_Col_End=Bool()//判断加载前两行的一行是否结束
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
    is(LTY_ENUM.LOAD_2_ROWS){//加载前两行
        when(Judge2Row_Col_End){//Col_End的标准是进一行
            nextState:=LTY_ENUM.JUDGE_LAST_ROW
        }otherwise{
            nextState:=LTY_ENUM.LOAD_2_ROWS
        }
    }
    is(LTY_ENUM.JUDGE_LAST_ROW){
        when(last_Row){
            nextState:=LTY_ENUM.IDLE//输出完最后一行
        }elsewhen(Load_2_Row_End){
            nextState:=LTY_ENUM.EXTRACT_LTY//等待下面的计算模块准备好接受计算数据
        }otherwise{
            nextState:=LTY_ENUM.LOAD_2_ROWS//前两行没加载完继续加载前两行
        }
    }
    // is(LTY_ENUM.WAIT_NEXT_READY){
    //     when(Next_Ready){
    //         nextState:=LTY_ENUM.EXTRACT_LTY
    //     }otherwise{
    //         nextState:=LTY_ENUM.WAIT_NEXT_READY//下一个模块没准备好，就一直等着
    //     }
    // }
    //由于进数据和出数据是独立的，无需考虑数据的反压，所以无需这个等下一层好的状态
    //只要在Extract Lty状态，那么数据就能valid，只要下一层ready，那么数据就能流下去
    is(LTY_ENUM.EXTRACT_LTY){
        when(Col_End){//只要进入这个状态，我们就可以保证数据要处理的那两行数据已经缓存完了，
            //接下来要做的就是正确地将数据输出去更新连通域标记矩阵
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

//Recoder：考虑将数据缓存和连通域提取分开
class Lty_Bram_Old extends Component{//连通域标记
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(Config.LTY_DATA_BRAM_A_WIDTH bits))//进来的数据
        val mData1=master Stream(UInt(Config.LTY_DATA_BRAM_B_WIDTH bits))//第一行出去的数据
        val mData2=master Stream(UInt(Config.LTY_DATA_BRAM_B_WIDTH bits))//第二行出去的数据
        val start=in Bool()//lty计算启动信号
    }
    noIoPrefix()
    val Fsm=LTY_FSM(io.start)
//状态机相关====================================================================
    //val Data_Out_Flag=Fsm.currentState===LTY_ENUM.EXTRACT_LTY||Fsm.currentState===LTY_ENUM.WAIT_NEXT_READY
    //不用这个↑作为mValid的判断是因为mValid标识出来的输出数据开头第一个会重复一下，结尾会少一个
    val Col_Cnt=WaCounter(io.sData.valid&&io.sData.ready, log2Up(Config.LTY_DATA_BRAM_A_DEPTH), Config.LTY_DATA_BRAM_A_DEPTH-1)//创建输入数据的列计数器
    val Bram_Out_Cnt=WaCounter(io.mData2.ready&&Fsm.currentState===LTY_ENUM.EXTRACT_LTY, log2Up(Config.LTY_DATA_BRAM_B_DEPTH), Config.LTY_DATA_BRAM_B_DEPTH-1)//创建输出数据的列计数器
    val Bram_Out_Row_Cnt=WaCounter(Bram_Out_Cnt.valid, log2Up(Config.LTY_ROW_NUM/2),Config.LTY_ROW_NUM/2-1)//创建输出行数计数器,记得除2，因为并行度是2
    val INIT_CNT=WaCounter(Fsm.currentState === LTY_ENUM.INIT, 3, 5)//初始化计数器,数五拍
    val Row_Cnt_All=WaCounter(Col_Cnt.valid,log2Up(Config.LTY_ROW_NUM),Config.LTY_ROW_NUM-1)//行计数器
    val Row_Cnt_2=WaCounter(Col_Cnt.valid,2,2)//缓存两行计数器，它的值一直是0，1，2，不可能是3，因为当Row_Cnt_2===2时，sReady拉低，不会再进数据
    //然后等输出完两行，进入最后一行判断，Row_Cnt_2被reset
    val Bram_Write_Choose=WaCounter(Col_Cnt.valid,log2Up(4),3)//0，1，2，3循环写
    Fsm.Init_End:=INIT_CNT.valid
    Fsm.last_Row:=Bram_Out_Row_Cnt.valid//缓存数完所有行，但是存在一种情况：
        //所有行都缓存完了，但是连通域还没被提取完，计算也没计算完，所以last_Row不能作为计算结束然后进入idle的标志
        //于是在状态机里又加了一些其他的判断
    Fsm.Judge2Row_Col_End:=Col_Cnt.valid//缓存完开头两行中的一行
    Fsm.Load_2_Row_End:=Row_Cnt_All.count>1//开头两行被缓存完了，行计数器为2的时候那么就加载完两行了,不能大于等于一，因为加载完第0行后，Row_Cnt就已经是1了
 
    Fsm.Col_End:=Bram_Out_Cnt.valid//发完了一行数据，也就是发送数据计数器数完了2040，表面上发送完一行，实际上处理完了两行
    //RegNext的原因：由于Bram输出慢一拍，所以需要RegNext一下，保证最后一个点也能正确输出
//一个Bram写选择器，一个Bram读选择器
    val Bram_Read_Chose=Reg(Bool())init(False)//要准确控制才行，是读0，1还是2，3
    //4个Bram的作用是在计算前两行的时候加载后两行数据
    when(RegNext(Bram_Out_Cnt.valid)){
        Bram_Read_Chose:=(!Bram_Read_Chose)
    }
   //违背逻辑的原因是避免在导入前两行的时候输出
    //也就是说，在加载第3，4行的时候，输出1，2行，加载1，2行时同理


//建立四个数据缓存Bram=======================================================================
    val Wr_En=Vec(Bool(),4)//循环写
    for(i<-0 to 3){
        Wr_En(i):=Bram_Write_Choose.count===i
    }
    val FeatureMem=Array.tabulate(4)(i=>{
        def gen():Lty_Bram={
            val mem=new Lty_Bram//
            mem
        }
        gen()
    })
    //写数据，需要处理写使能，写地址，写数据
    for(i<-0 to 3){
        FeatureMem(i).io.ena:=Wr_En(i)//4个Bram的写使能
        FeatureMem(i).io.addra:=Col_Cnt.count//4个Bram的写地址
        FeatureMem(i).io.dina:=io.sData.payload//4个Bram的写数据
        FeatureMem(i).io.wea:=True
    }//读数据=================================
    for(i<-0 to 3){
        FeatureMem(i).io.addrb:=Bram_Out_Cnt.count//读地址待处理
        FeatureMem(i).io.enb:=True//以后有需要在处理
    }
    io.mData1.payload:=Bram_Read_Chose?FeatureMem(2).io.doutb|FeatureMem(0).io.doutb//输出数据选择器
    io.mData2.payload:=Bram_Read_Chose?FeatureMem(3).io.doutb|FeatureMem(1).io.doutb//输出数据选择器
//mux必须是圆括号
//控制数据输入输出


    io.sData.ready:=(Fsm.currentState=/=LTY_ENUM.IDLE&&Fsm.currentState=/=LTY_ENUM.INIT)&&Row_Cnt_2.count<2//待处理
    //按道理说，只要另外两个Bram没满，任何时候都能接受数据
    when(Fsm.currentState===LTY_ENUM.JUDGE_LAST_ROW&&(!io.sData.ready)){//为毛线要取个反？我敲，我忘记为啥了
        Row_Cnt_2.clear//每缓存完一行数据都要进行是否是最后一行判断
    }
    // when(){//这里加RegNext的原因：对波形时开头多了一个点结尾少了一个点，待分析
    // }otherwise{
    //     io.mData1.valid:=False
    //     io.mData2.valid:=False
    // }
    io.mData1.valid:=RegNext(io.mData1.ready&&Fsm.currentState===LTY_ENUM.EXTRACT_LTY)//添加后面的Fsm.currentState===LTY_ENUM.WAIT_NEXT_READY条件是因为没必要让mValid在连通域提取状态拉高拉低
    io.mData2.valid:=RegNext(io.mData2.ready&&Fsm.currentState===LTY_ENUM.EXTRACT_LTY)//这里情况比较特殊，mValid应该由mReady驱动，也就是说，mReady不来的话，mValid不会拉高

    // io.mData1.valid:=RegNext(Data_Out_Flag)
    // io.mData2.valid:=RegNext(Data_Out_Flag)

}
object MARK_ENUM extends SpinalEnum(defaultEncoding = binaryOneHot) {
  val IDLE, INIT, GET_DATA,COND1,COND2,COND3 = newElement
    /*三种条件，
        COND1：  上，左都为0---新建一个连通域
        COND2:  上不为0---处理左边四个点
        COND3：上为0，左不为0---单独处理当前点  
    */    
}
class Lty_Mark_Sub_Module extends Component{//标记子模块
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(Config.LTY_DATA_BRAM_B_WIDTH bits))//进来的滤波后图片数据点
        val Up_mark=in UInt(16 bits)//上标记
        val Left_Mark=in Vec(UInt(16 bits),4)//左边的四个标记点
        val Lty_Total_NUm=in UInt(16 bits)//连通域总数量
        val Mark_Out=out UInt(16 bits)//输出标记点
        val Left_Mark_Valid=out Vec(Bool())//
    }
    val Pixel_Pos=WaCounter(io.sData.fire,log2Up(Config.LTY_DATA_BRAM_B_DEPTH),Config.LTY_DATA_BRAM_B_WIDTH-1)//当前处理的点的坐标计数器
    //创建标记矩阵---并行度为2，所以也得要两个Ram作为上标记矩阵=====================================
    val Up_Mark_Mem1=Mem(UInt(Config.LTY_MARK_BRAM_WIDTH bits),wordCount=Config.LTY_MARK_BRAM_DEPTH)//标记矩阵
    //希望单独处理一行的标记

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
    val verilog_path="./testcode_gen/Lty_Gen" 
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Lty_Bram_Old)
}