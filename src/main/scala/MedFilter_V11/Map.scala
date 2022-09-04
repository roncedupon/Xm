package MedFilter_V11
import spinal.core._
import spinal.lib.slave
import spinal.lib.master
import Archive.WaCounter
import spinal.lib.Delay
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
                nextState:=MAP_ENUM.COMPARE//初始化完成就加载前两行
            }otherwise{
                nextState:=MAP_ENUM.LOAD_20
            }
        }
        is(MAP_ENUM.COMPARE){
            when(Compare_End){
                nextState:=MAP_ENUM.SEND_20_DATA//初始化完成就加载前两行
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
        val sValid_Trilist=in Bool()

        //16bit星图数据进来之前要补零
        val Trilib1=in UInt(32 bits)//星图匹配数据
        val Trilib2=in UInt(32 bits)
        val Trilib3=in UInt(32 bits)//全部是定点

        val sReady_Trilib=out Bool()
        val Compare_Valid=in Bool()


        // val Position=out UInt(32 bits)//位置
        val MinError_Out=out UInt(32 bits)//误差
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
    }elsewhen(io.Reset_Minerror){
        MiniError:=Config.IDENTTHRD//第二次计算复位Minerror值
    }otherwise{
        MiniError:=MiniError
    }
    io.MinError_Out:=MiniError
}
class Map_Triangle_Mem extends Component{
    //三角形数据：12*20=240B，直接用一个Mem存起来就行
    //此模块用于整理进来的数据
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(96 bits))//ready可以认为要一直拉高
        val mData=out Vec(UInt(96 bits),20)//三角形数据32bit一个,星图数据16bit一个
        val mReady=in Bool()
        val mValid=out Bool()

        val start=in Bool()
    }
    noIoPrefix()
    val counter=WaCounter(io.sData.valid&&io.sData.ready,log2Up(Config.TRIANGLE_DATA_IN_NUM),Config.TRIANGLE_DATA_IN_NUM-1)
    //Fsm========================================================
    val Start_Single_Time=(!RegNext(io.start))&&io.start//只启动一次
    val Fsm=Map_Fsm(Start_Single_Time)
    val INIT_CNT=WaCounter(Fsm.currentState === MAP_ENUM.INIT, 3, 5)//初始化计数器,数五拍
    Fsm.Init_End:=INIT_CNT.valid
    Fsm.Load_20End:=counter.count===19//
    Fsm.Compare_End:=counter.valid//所有数发完了，就进入发送20个结果状态
    val Send_Data_Valid=Delay(Fsm.currentState===MAP_ENUM.SEND_20_DATA,3)//发给Ps的计算结果数据
    val Send_20_Counter=WaCounter(io.mReady&&Send_Data_Valid&&Fsm.currentState===MAP_ENUM.SEND_20_DATA,log2Up(20),20-1)//不能加入状态判断，因为Send_20_End由这个计数器控制，再加入状态判断的话会重复耦合
    Fsm.Send_20_End:=Send_20_Counter.valid//这个valid信号可能有问题
    //stream流握手控制============================================

    io.mValid:=Send_Data_Valid&&Fsm.currentState===MAP_ENUM.SEND_20_DATA
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
    for(i<-0 to 19){
        io.mData(i):=0
    }
//创建比较器
    val Map_Triangle_Module=new Map_One_Triangle
    Map_Triangle_Module.io.Trilib1<>(B"16'b0"##Reg20(20)(15 downto 0)).asUInt
    Map_Triangle_Module.io.Trilib2<>(B"16'b0"##Reg20(20)(31 downto 16)).asUInt
    Map_Triangle_Module.io.Trilib3<>(B"16'b0"##Reg20(20)(47 downto 32)).asUInt
    Map_Triangle_Module.io.Trilist1<>Reg20(0)(31 downto 0)
    Map_Triangle_Module.io.Trilist2<>Reg20(0)(63 downto 32)
    Map_Triangle_Module.io.Trilist3<>Reg20(0)(95 downto 64)

    //握手信号
    when(Fsm.currentState===MAP_ENUM.COMPARE||Fsm.currentState===MAP_ENUM.LOAD_20){
        io.sData.ready:=True//只有在比较状态中才能接受数据
    }otherwise{
        io.sData.ready:=False
    }
    //首先状态得在比较状态才能开启比较
    //如果进来的trilib数据有效，延迟3拍后拿到有效比较数据，
    //如果进来的trilib数据无效，那么我们拉低Compare_valid信号，从而Minierror不保持无效数据，酱紫
    Map_Triangle_Module.io.Compare_Valid:=Delay(Fsm.currentState===MAP_ENUM.COMPARE&&io.sData.valid,3)//启动比较，与io.mvalid不同,io.mvalid与DMA进行握手
    
    //Minerror复位信号
    Map_Triangle_Module.io.Reset_Minerror:=(Fsm.currentState===MAP_ENUM.IDLE)
}

object MapGen extends App { 
    val verilog_path="./testcode_gen/MemGen" 
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Map_Triangle_Mem)
}