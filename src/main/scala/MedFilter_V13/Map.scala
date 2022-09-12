package MedFilter_V12
import spinal.core._
import spinal.lib.slave
import spinal.lib.master
import Archive.WaCounter
import spinal.lib.Delay
import spinal.core.internals.Resize
import spinal.lib.Min
//星图比对--版本一：Trilist取整转为整数输入
class Abs_Sub extends Component{
    //绝对值减法
    val io=new Bundle{
        val A=in UInt(32 bits)
        val B=in UInt(32 bits)
        val C=out UInt(32 bits)
    }
     noIoPrefix()
     io.C:=(io.A>io.B)?(io.A-io.B)|(io.B-io.A)
}

object MAP_ENUM extends SpinalEnum(defaultEncoding = binaryOneHot) {
  val IDLE, INIT,LOAD_20,COMPARE,SEND_20_DATA = newElement
}//LOAD_20：加载前20个trilist
case class Map_Fsm(start: Bool) extends Area {
    val currentState = Reg(MAP_ENUM()) init MAP_ENUM.IDLE
    val nextState = MAP_ENUM()
    currentState := nextState
    //===============================================
    val Init_End=Bool()
    val Load_20End=Bool()
    val Compare_End=Bool()
    val Send_20_End=Bool()
    val Wait3_End=Bool()
    switch(currentState){
        //注意：这个状态机以进来的数据为准，出去的数据会有延时，不能用出去数据来对齐这些状态，如果要对齐出去的数据，
            //分别看Compare_Valid和mValid
            //Compare_Valid指示的是3参数算完绝对值经过三个周期出去的结果，这个结果会再和minerror比较
            //mValid指示的是最终的输出结果
        is(MAP_ENUM.IDLE){
            when(start){
                nextState:=MAP_ENUM.INIT
            }otherwise{
                nextState:=MAP_ENUM.IDLE
            }
        }
        is(MAP_ENUM.INIT){
            when(Init_End){
                nextState:=MAP_ENUM.LOAD_20//初始化完成就加载前两行
            }otherwise{
                nextState:=MAP_ENUM.INIT
            }
        }
        is(MAP_ENUM.LOAD_20){
            when(Load_20End){
                nextState:=MAP_ENUM.COMPARE//
            }otherwise{
                nextState:=MAP_ENUM.LOAD_20
            }
        }
        // is(MAP_ENUM.WAIT_3){
        //     when(Wait3_End){
        //         nextState:=MAP_ENUM.COMPARE
        //     }otherwise{
        //         nextState:=MAP_ENUM.WAIT_3//最后才加的状态，等三个数进来后出来的数才是有效的
        //     }
        // }
        is(MAP_ENUM.COMPARE){
            when(Compare_End){
                nextState:=MAP_ENUM.SEND_20_DATA//
            }otherwise{
                nextState:=MAP_ENUM.COMPARE
            }
        }
        is(MAP_ENUM.SEND_20_DATA){
            when(Send_20_End){
                nextState:=MAP_ENUM.IDLE//初始化完成就加载前两行
            }otherwise{
                nextState:=MAP_ENUM.SEND_20_DATA
            }
        }
    }
}
class Map_One_Triangle extends Component{//匹配一个三角形
    val Config=MemConfig()
    val io=new Bundle{
        val Trilist1=in UInt(32 bits)//进来的数据是32bit比较数据（向上取整）
        val Trilist2=in UInt(32 bits)
        val Trilist3=in UInt(32 bits)//还是不能走flow流

        // val sReady_Trilist=out Bool()
        // val sValid_Trilist=in Bool()

        //16bit星图数据进来之前要补零
        val Trilib1=in UInt(32 bits)//星图匹配数据
        val Trilib2=in UInt(32 bits)
        val Trilib3=in UInt(32 bits)//全部是定点

        val sReady_Trilib=out Bool()
        val Compare_Valid=in Bool()


        // val Position=out UInt(32 bits)//位置
        val MinError_Out=out UInt(32 bits)//误差
        val MinError_Valid=out Bool()//指示要更新最误差以及position 
        val Reset_Minerror=in Bool()//第二次计算复位
        // val mReady=in Bool()
        // val mValid=out Bool()


        //20个三角形数据计算：（32，32，32）=12B*20=240B--浮点8B在Ps那边向上取整转为整形4B
    }
    noIoPrefix()

    val MiniError=Reg(UInt(32 bits))init(Config.IDENTTHRD)

    
    val Compare1_In=(io.Trilist1>io.Trilib1)?(io.Trilist1-io.Trilib1)|(io.Trilib1-io.Trilist1)//无延时
    val Compare2_In=(io.Trilist2>io.Trilib2)?(io.Trilist2-io.Trilib2)|(io.Trilib2-io.Trilist2)//无延时
    val Compare12=RegNext((Compare1_In>Compare2_In)?Compare1_In|Compare2_In)//选两个之间最大的----//一个延时
    val Compare3_In=RegNext((io.Trilist3>io.Trilib3)?(io.Trilist3-io.Trilib3)|(io.Trilib3-io.Trilist3))//和上面同步
    val Compare123=RegNext((Compare12>Compare3_In)?Compare12|Compare3_In)//一个延时
    when(io.Compare_Valid){//只有当星图数据进来才能开始比较
        MiniError:=((Compare123<MiniError)&&(Compare123<Config.IDENTTHRD))?Compare123|MiniError
    }elsewhen(io.Reset_Minerror){//一个延时
        MiniError:=Config.IDENTTHRD//第二次计算复位Minerror值
    }otherwise{
        MiniError:=MiniError
    }
    io.MinError_Out:=MiniError
    io.MinError_Valid:=(MiniError<RegNext(MiniError))//MiniError一定是单调递减的，用小于而不是不等于判断是为了排除第二次启动时Minivalid会在一开始拉高一下
}
class Map_Triangle_Mem extends Component{
    //三角形数据：12*20=240B，直接用一个Mem存起来就行
    //此模块用于整理进来的数据
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(96 bits))//ready可以认为要一直拉高
        val mData=out UInt(64 bits)//三角形数据32bit一个,星图数据16bit一个
        val mReady=in Bool()
        val mValid=out Bool()

        val start=in Bool()
        val mLast=out Bool()//给Dma的last信号
    }
    noIoPrefix()
    val counter=WaCounter(io.sData.valid&&io.sData.ready,log2Up(Config.TRIANGLE_DATA_IN_NUM),Config.TRIANGLE_DATA_IN_NUM-1)
    //Fsm========================================================
    val Start_Single_Time=(!RegNext(io.start))&&io.start//只启动一次
    val Fsm=Map_Fsm(Start_Single_Time)
    val INIT_CNT=WaCounter(Fsm.currentState === MAP_ENUM.INIT, 3, 5)//初始化计数器,数五拍
    Fsm.Init_End:=INIT_CNT.valid
    Fsm.Load_20End:=(counter.count===19&&io.sData.valid)//到底是20还是19？(19)---⭐⭐⭐2022/9/2:添加&&io.sData.valid不然sValid=~sValid过不去，bug修正
    Fsm.Compare_End:=Delay(counter.valid,3)//所有数发完了，就进入发送20个结果状态，Delay3的原因，counter valid了但是比较器还在比较
    val Send_20_Counter=WaCounter(io.mReady&&Fsm.currentState===MAP_ENUM.SEND_20_DATA,log2Up(20),20-1)//
    when(Fsm.currentState===MAP_ENUM.IDLE){
        Send_20_Counter.clear
    }
    Fsm.Send_20_End:=Send_20_Counter.count===19&&io.mReady//这个valid信号可能有问题
    //stream流握手控制============================================

    io.mValid:=RegNext(Fsm.currentState===MAP_ENUM.SEND_20_DATA)//RegNext的原因：下面用了一个mData_tmp做数据选择，不知道为什么不能用组合逻辑。。。
    val Reg20=Vec(Reg(UInt(96 bits))init(0),21)
    when(io.sData.valid){//数据有效才启动赋值操作
        when(counter.count<20){
            for(i<-0 to 19){//先将20个三角形数据存起来
                when(counter.count===i){
                    Reg20(i):=io.sData.payload
                }
            }
            Reg20(20):=0//设为0
        }otherwise{
            Reg20(20):=io.sData.payload
            for(i<-0 to 19)
            Reg20(i):=Reg20(i)
        }   
    }

//创建比较器
    val Compare_Valid=Delay(Fsm.currentState===MAP_ENUM.COMPARE&&io.sData.valid,3)//一开始手画波形图分析的是2，但是后面发现trilist这些也是由寄存器输入的，所以需要再加一个延时
    
    val Pos_Now=Vec(Reg(UInt(32 bits))init(0),20)
	val Map_Triangle_Modules=Array.tabulate(20)(i=>{
        def gen():Map_One_Triangle={
            val Map_Triangle_Module=new Map_One_Triangle
            //Delay_valid(0),ready--输入的ready信号,告诉fifo要pop数据了
            
            Map_Triangle_Module.io.Trilib1<>(B"16'b0"##Reg20(20)(15 downto 0)).asUInt
            Map_Triangle_Module.io.Trilib2<>(B"16'b0"##Reg20(20)(31 downto 16)).asUInt
            Map_Triangle_Module.io.Trilib3<>(B"16'b0"##Reg20(20)(47 downto 32)).asUInt
            Map_Triangle_Module.io.Trilist1<>Reg20(i)(31 downto 0)
            Map_Triangle_Module.io.Trilist2<>Reg20(i)(63 downto 32)
            Map_Triangle_Module.io.Trilist3<>Reg20(i)(95 downto 64)
            //首先状态得在比较状态才能开启比较
            //如果进来的trilib数据有效，延迟3拍后拿到有效比较数据，
            //如果进来的trilib数据无效，那么我们拉低Compare_valid信号，从而Minierror不保持无效数据，酱紫
            Map_Triangle_Module.io.Compare_Valid:=Compare_Valid//启动比较，与io.mvalid不同,io.mvalid与DMA进行握手
            Map_Triangle_Module.io.Reset_Minerror:=(Fsm.currentState===MAP_ENUM.IDLE)//Minerror复位信号
            when(Map_Triangle_Module.io.MinError_Valid){
                Pos_Now(i):=(Delay(counter.count,4)-20).resize(32)//
            }otherwise{
                Pos_Now(i):=Pos_Now(i)//对于每个mData来说，pos都是不一样的
            }


            Map_Triangle_Module
        }
        gen()
    }) 
    val mData_Tmp=Reg(UInt(64 bits))init(0)
    for(i<-0 to 19){
        when(Send_20_Counter.count===i){
            mData_Tmp:=Map_Triangle_Modules(i).io.MinError_Out@@(Pos_Now(i))//B"32'b0".asUInt@@
        }
    }
    io.mData:=mData_Tmp
    //握手信号
    when(Fsm.currentState===MAP_ENUM.COMPARE||Fsm.currentState===MAP_ENUM.LOAD_20){
        io.sData.ready:=True//只有在比较状态中才能接受数据
    }otherwise{
        io.sData.ready:=False
    }
    io.mLast:=RegNext(Send_20_Counter.valid)
}



class Map_Stream extends Component{
    val io=new Bundle{
        val m_axis_mm2s_tdata=out UInt(64 bits)
        val m_axis_mm2s_tkeep=out Bits(4 bits)
        val m_axis_mm2s_tlast=out Bool()
        val m_axis_mm2s_tready=in Bool()
        val m_axis_mm2s_tvalid=out Bool()

        val s_axis_s2mm_tdata=in UInt(96 bits)
        val s_axis_s2mm_tkeep=in UInt(4 bits)
        val s_axis_s2mm_tlast=in Bool()
        val s_axis_s2mm_tready=out Bool()
        val s_axis_s2mm_tvalid=in Bool()

        // val m_tlast=out Bool()
        val start=in Bool()
    }
    noIoPrefix()
    val Map_Triangle_Module=new Map_Triangle_Mem
    Map_Triangle_Module.io.mData<>io.m_axis_mm2s_tdata
    Map_Triangle_Module.io.mValid<>io.m_axis_mm2s_tvalid
    Map_Triangle_Module.io.mReady<>io.m_axis_mm2s_tready
    Map_Triangle_Module.io.mLast<>io.m_axis_mm2s_tlast
    io.m_axis_mm2s_tkeep:=B"4'b1111"

    Map_Triangle_Module.io.sData.payload<>io.s_axis_s2mm_tdata
    Map_Triangle_Module.io.sData.valid<>io.s_axis_s2mm_tvalid
    Map_Triangle_Module.io.sData.ready<>io.s_axis_s2mm_tready
    Map_Triangle_Module.io.start<>io.start
    

}
//V13版本：修改了V12版本的输出少一个点的Bug，实现全部20个点的输出
object MapGen extends App { 
    val verilog_path="./testcode_gen/MemGen" 
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Map_Stream)
}