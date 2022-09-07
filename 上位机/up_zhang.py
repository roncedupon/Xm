# encoding: utf-8

import os
import mmap
import argparse
import numpy as np
import codecs
import time
import array


######  codecs 用做编码转换的  
######  codecs.getdecoder("hex_codec")  将 hex_codec (16进制编码格式) 解码成 unicode   （中间编码形式）
decode_hex = codecs.getdecoder("hex_codec")


def to_bytes(n, length, endianess='big'):
    h = '%x' % n              # h 将 n 转换为 无符号16进制格式
    
    # 将 h 转换成 unicode 形式，并且只取大小 (经过codecs.getdecoder 后得到的 unicode 编码形式是二维的tuple，[0] 为大小，[1] 为长度 )
    # zfill(width): 用0填充为总长度为 width 的数，填充的方式为高位添零，有效数字都在右侧
    s = decode_hex(('0'*(len(h) % 2) + h).zfill(length*2))[0]
 
    return s if endianess == 'big' else s[::-1]


class FPGA_MEM(object):
    def __init__(self,
        dma_lite_device = '/dev/xdma0_user',
        h2c_dma_device = '/dev/xdma0_h2c_0',
        c2h_dma_device = '/dev/xdma0_c2h_0',
        ):

        self.dma_lite_fd = os.open(dma_lite_device,os.O_RDWR)

         # mmap用来创建内存映射文件
         # m=mmap.mmap(fileno, length[, flags[, prot[, access[, offset]]]])
         # fileno   文件描述符，可以是file对象的fileno()方法，或者来自os.open()，在调用mmap()之前打开文件，不再需要文件时要关闭。
         # length   要映射文件部分的大小（以字节为单位），这个值为0，则映射整个文件，如果大小大于文件当前大小，则扩展这个文件。
         # flags         MAP_PRIVATE：这段内存映射只有本进程可用；mmap.MAP_SHARED：将内存映射和其他进程共享，所有映射了同一文件的进程，都能够看到其中一个所做的更改；
         # prot      mmap.PROT_READ, mmap.PROT_WRITE 和 mmap.PROT_WRITE ，mmap.PROT_READ。最后一者的含义是同时可读可写
         # access    在mmap中有可选参数access的值有：ACCESS_READ：读访问。ACCESS_WRITE：写访问，默认。ACCESS_COPY：拷贝访问，不会把更改写入到文件，使用flush把更改写到文件。
         # offset      开始的偏移量，也就是代表需要移动偏移的字节数
        
        
        self.dma_lite_mmap = mmap.mmap((self.dma_lite_fd),1024*1024,prot = mmap.PROT_READ|mmap.PROT_WRITE,offset = 0)

        os.close(self.dma_lite_fd)

        self.dma_h2c_fd = os.open(h2c_dma_device,os.O_RDWR)

        self.dma_c2h_fd = os.open(c2h_dma_device,os.O_RDWR)

    def close(self):
        self.dma_lite_mmap.close()
        os.close(self.dma_h2c_fd)
        os.close(self.dma_c2h_fd)

    def write(self,namespace,addr,data):
      #file的seek()函数     seek() 方法用于移动文件读取指针到指定位置。
        # fileObject.seek(offset[, whence])    
        # offset – 开始的偏移量，也就是代表需要移动偏移的字节数
        # whence：可选，默认值为 0。给offset参数一个定义，表示要从哪个位置开始偏移；0代表从文件开头开始算起，1代表从当前位置开始算起，2代表从文件末尾算起。
        # 如果操作成功，则返回新的文件位置，如果操作失败，则函数返回 -1。

            os.lseek(self.dma_h2c_fd,addr,0)
            os.write(self.dma_h2c_fd,data)
    def read(self,namespace, addr, size = None):
        assert namespace in ('lite_control','ddr')

        if namespace == 'lite_control':
            self.dma_lite_mmap.seek(addr)
            o = self.dma_lite_mmap.read(4)
            o = '0x'+''.join([hex(i)[2:].zfill(2) for i in reversed(bytearray(o))])
            return o
        
        else:

            os.lseek(self.dma_c2h_fd,addr,0)
            return os.read(self.dma_c2h_fd,int(size))



    def four2one (self,addr,reg_one,reg_two,reg_three,reg_four):
    
        self.dma_lite_mmap[addr] = reg_four
        self.dma_lite_mmap[addr+1] = reg_three
        self.dma_lite_mmap[addr+2] = reg_two
        self.dma_lite_mmap[addr+3] = reg_one

fpga_memspace = FPGA_MEM(dma_lite_device='/dev/xdma0_user', h2c_dma_device='/dev/xdma0_h2c_0',
                             c2h_dma_device='/dev/xdma0_c2h_0')

mmmm = time.time()
print("start send weight\n")
weight_conv1 = np.fromfile('./weight_all_714.bin',dtype = np.uint8)
fpga_memspace.write('ddr',0x6FFFFA60,weight_conv1)
print("send weight finish\n")
feature_one = np.fromfile("./feature_2.bin",dtype = np.uint8)
fpga_memspace.write("ddr",0x00000000,feature_one)
print("start send feature\n")

f = open('test.txt', encoding='gbk')
reg = ''
for line in f:
    reg = line.strip()
    if(int(reg[0:2], 16)!=17):
        fpga_memspace.four2one(int(reg[-10:-8], 16), int(reg[-8:-6], 16), int(reg[-6:-4], 16), int(reg[-4:-2], 16), int(reg[-2:], 16))
    else:
        while(1):
            if (fpga_memspace.read('lite_control', 0X04) == '0x0000000f' or fpga_memspace.read('lite_control', 0X14) == '0x0000000f' or fpga_memspace.read('lite_control', 0X14) == '0x000000f0' or fpga_memspace.read('lite_control', 0X14) == '0x00000f00'):
                fpga_memspace.four2one(int(reg[-10:-8], 16), int(reg[-8:-6], 16), int(reg[-6:-4], 16), int(reg[-4:-2], 16), int(reg[-2:], 16))
                break
       				 
nnnn = time.time()
print(nnnn-mmmm)


out_image1 = np.array(array.array('B',fpga_memspace.read("ddr",0X2B000000,0x00009600)),dtype = np.uint8)

i = 0
out1 = []

with open ('out_big.coe','a') as ff:
    for i in range(out_image1.shape[0]):
        out1.append(out_image1[i])
        if (i+1) % 8 == 0 and i != 0:
            out1.reverse()
            for m in range(len(out1)):
                ff.write('%02x'% out1[m] )
            ff.write(",\n")
            out1 = []
ff.close()


out_image2 = np.array(array.array('B',fpga_memspace.read("ddr",0x2E000000,0x00002580)),dtype = np.uint8)

i = 0
out2 = []

with open ('out_little.coe','a') as ff:
    for i in range(out_image2.shape[0]):
        out2.append(out_image2[i])
        if (i+1) % 8 == 0 and i != 0:
            out2.reverse()
            for m in range(len(out2)):
                ff.write('%02x'% out2[m] )
            ff.write(",\n")
            out2 = []
ff.close()

