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

    //连通域处理相关
    //一些要求：
        //⭐stream流数据位宽要和Bram数据位宽相同，这是为了以后实现更高的并行度准备的，目前的并行度仍然为2
    val STREAM_DATA_WIDTH=32//stream流数据位宽
    val FEATURE_DATA_WIDTH=16//滤波后图片像素位宽
    val LTY_NUM_WIDTH=11//连通域个数数据位宽
    val LTY_COL_NUM=2040//2048个点，减去前面4个和后面4个，2040，
    val LTY_ROW_NUM=2040//2048个点，减去前面4个和后面4个，2040，
        //标记矩阵相关
    val LTY_MARK_BRAM_DEPTH=LTY_COL_NUM//标记矩阵的深度，也不用加一
    val LTY_MARK_BRAM_WIDTH=16//标记矩阵的数据位宽
        //滤波后的图片缓存
    val LTY_DATA_BRAM_DATA_WIDTH=32//输入和输出位宽一样，懒得做位宽转换了，就这样吧
    val LTY_DATA_BRAM_DEPTH=LTY_COL_NUM/(LTY_DATA_BRAM_DATA_WIDTH/FEATURE_DATA_WIDTH)//滤波后图片数据的缓存深度


    //连通域后处理
    val STARPOINT_THRD=5//连通域所占的最小像素点
    val PARA6_THRD_WIDTH=46//SNR_thrd*temp_back_std*2^32---所以这里的值应该是SNR_thrd*temp_back_std放大2^32之后的值
    //这里需要注意的是：这里拿第六个参数进行比较，第六个参数是sum(ImgFilter(i,j)+0.4),我希望拿到的都是放大后的结果，要和上一层交接好



    //星图匹配
    val IDENTTHRD=100000
    val TRIANGLE_DATA_IN_NUM=774320+20

}//中值滤波配置