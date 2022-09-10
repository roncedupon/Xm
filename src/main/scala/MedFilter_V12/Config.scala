package MedFilter_V12

case class MemConfig() {
    val BRAM_IN_DATA_WIDTH=32
    val BRAM_OUT_DATA_WIDTH=32//还没学会怎么进128出16,但是想到了一种进64出64的办法
    val BRAM_DEPTH=(2048)/(BRAM_IN_DATA_WIDTH/16)//BRAM 数据深度
    //一次进64也就是4个点，所以需要进2048/4=512次，可以认为列计数为512
    val COl_CNT_NUM=(2048)/(BRAM_IN_DATA_WIDTH/16)//假如2048个点,一次进32bit,一次读32bit,那么需要1024个地址
    val ROW_CNT_NUM=2048//行计数
    val BRAM_NUM=8
    //乘法器相关--目前只针对滤波模块的A*B
    val MUL_LATENCY=4//平方和乘法器延迟
    val MUL_A_IN=16
    val MUL_B_In=16
    val MUL_P_OUT=32


    //Axis_Switch配置
    val AXIS_SWITCH_1S_WIDTH=64//1s的数据位宽是64
    val AXIS_SWITCH_1S_PORT=2//master口个数

    val AXIS_SWITCH_2S_WIDTH=64//2s的数据位宽是64
    val AXIS_SWITCH_2S_PORT=2//Slave口个数
    //连通域处理相关
    val LTY_NUM_WIDTH=11//连通域个数数据位宽
    val LTY_COL_NUM=1020//一次进32 bit，2048个点，减去前面4个和后面4个，2040，再除个2（要修改）
    val LTY_ROW_NUM=1020//一次进32 bit，2048个点，减去前面4个和后面4个，2040，再除个2（要修改）
    val LTY_MARK_MEM_DEPTH=2040//标记矩阵的深度
    val LTY_BRAM_DEPTH=1020//数据缓存深度
    val LTY_BRAM_IN_WIDTH=128//Bram进数据位宽64
    val LTY_BRAM_OUT_WIDTH=32//Bram出数据位宽32

    //星图匹配
    val IDENTTHRD=100000
    val TRIANGLE_DATA_IN_NUM=774320+20

}//中值滤波配置