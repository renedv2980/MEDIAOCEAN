*          DATA SET DMDATESTL  AT LEVEL 050 AS OF 10/30/00                      
*          DATA SET DMDATEST   AT LEVEL 061 AS OF 08/03/99                      
*PHASE DMDATSTL                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDADDS                                                                
*INCLUDE DMDYNDD                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'DMDATEST - TEST NEW BLKIND LINKS'                               
         GBLC  &OFFLINE                                                         
&OFFLINE SETC  'N'                                                              
*                                                                               
KEYLEN   EQU   17                                                               
*                                                                               
DMDATSTL CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DMDATSTL,=V(REGSAVE)                                           
         USING WORKD,RC                                                         
*                                                                               
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         LA    R9,DAFILE                                                        
         USING DTFPHD,R9                                                        
*                                                                               
         GOTO1 VDADDS,P1,14,,,DAFILE     OPEN THE FILE                          
         MVC   P(20),=CL20'OPEN'                                                
         GOTO1 VPRINTER                                                         
*                                                                               
TRK1     L     RF,=XL4'00010100'   START AT X'08001000'                         
         ST    RF,TRACK                                                         
         B     BLKNXT                                                           
*                                                                               
TRKNXT   LH    RF,TRACK            NEXT TRACK                                   
         LA    RF,1(RF)                                                         
         STH   RF,TRACK                                                         
         XC    BLOCK,BLOCK                                                      
*                                                                               
BLKNXT   LH    RF,BLOCK            NEXT BLOCK                                   
         AHI   RF,X'0100'                                                       
         STH   RF,BLOCK                                                         
*                                                                               
LOOP03   MVC   P6(4),TRACK         SET TTTT0000                                 
*                                                                               
*        GOTO1 VHEXOUT,DMCB,P6,P+1,4                                            
*        MVC   P+10(8),=CL8'READ'                                               
*        GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VDADDS,P1,01,AIOA,0,,P6                                          
         OC    P3(2),P3                                                         
         BZ    LOOP06                                                           
         TM    P3+1,X'08'          BLOCK NOT FOUND                              
         BO    TRKNXT                                                           
         TM    P3+1,X'04'          EOF                                          
         BO    CLOSE                                                            
         DCHO                                                                   
*                                                                               
*&&DO                                                                           
LOOP06   L     R3,AIOA                                                          
*        CLC   =XL4'100000F7',2(R3)                                             
*        BNE   BLKNXT                                                           
         BRAS  RE,PRTBUFF                                                       
         B     BLKNXT                                                           
*&&                                                                             
*&&DO                                                                           
LOOP06   L     R3,AIOA                                                          
         OC    2(4,R3),2(R3)       CHAIN HERE?                                  
         BZ    BLKNXT                                                           
*                                                                               
         MVC   P(30),=CL30'SPLIT BLOCK STARTS HERE'                             
         GOTO1 VPRINTER                                                         
         BRAS  RE,PRTBUFF                                                       
*                                                                               
LOOP08   MVC   P6,2(R3)            SAVE SEQUENTIAL READ CHAIN                   
         GOTO1 VDADDS,P1,01,AIOA,0,,P6                                          
         OC    P3(2),P3                                                         
         BZ    LOOP10                                                           
         DCHO                                                                   
*                                                                               
LOOP10   MVC   P(30),=CL30'FOLLOWING CHAIN'                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R3,AIOA                                                          
         BRAS  RE,PRTBUFF                                                       
         OC    2(4,R3),2(R3)                                                    
         BNZ   LOOP08                                                           
*                                                                               
         MVC   P(30),=CL30'END OF CHAIN'                                        
         GOTO1 VPRINTER                                                         
         B     CLOSE                                                            
*&&                                                                             
LOOP06   L     R3,AIOA                                                          
         OC    2(4,R3),2(R3)       CHAIN HERE?                                  
         BZ    BLKNXT                                                           
*                                                                               
         CLC   BLKIND,2(R3)        IS THIS AN INDEX BLOCK?                      
         BNE   BLKNXT              NO                                           
*                                                                               
         MVI   BADFLAG,C'N'                                                     
         MVC   P(30),=CL30'FOUND INDEX BLOCK'                                   
         GOTO1 VPRINTER                                                         
         BRAS  RE,PRTBUFF                                                       
*                                                                               
         AH    R3,0(R3)            POINT TO FIRST D/A IN LIST                   
         AHI   R3,KEYLEN-4                                                      
         MVC   CHAIN,0(R3)         SAVE IT FOR SEQUENTIAL READ CHAIN            
*                                                                               
LOOP08   MVC   P6,0(R3)                                                         
         GOTO1 VDADDS,P1,01,AIO2,0,,P6                                          
         OC    P3(2),P3                                                         
         BZ    LOOP10                                                           
         DCHO                                                                   
*                                                                               
LOOP10   LA    RE,KEYLEN(R3)                                                    
         L     RF,AIOA                                                          
         AHI   RF,1954                                                          
         CR    RE,RF                                                            
         BH    LOOP12                                                           
*                                                                               
         L     RE,AIO2                                                          
         CLC   2(4,RE),KEYLEN(R3)                                               
         BE    LOOP12                                                           
*                                                                               
         MVI   BADFLAG,C'Y'                                                     
         MVC   P(30),=CL30'BAD LINK IN CHAIN'                                   
         GOTO1 VPRINTER                                                         
*                                                                               
LOOP12   BRAS  RE,PRTBUFF2                                                      
         AHI   R3,KEYLEN                                                        
         L     RF,AIOA                                                          
         AHI   RF,1954                                                          
         CR    R3,RF                                                            
         BNH   LOOP08                                                           
*                                                                               
LOOP14   MVC   P(30),=CL30'FOLLOWING CHAIN'                                     
         GOTO1 VPRINTER                                                         
         MVC   P6,CHAIN                                                         
*                                                                               
LOOP16   GOTO1 VDADDS,P1,01,AIO2,0,,P6                                          
         OC    P3(2),P3                                                         
         BZ    LOOP18                                                           
         DCHO                                                                   
*                                                                               
LOOP18   BRAS  RE,PRTBUFF2                                                      
         L     RE,AIO2                                                          
         OC    2(4,RE),2(RE)                                                    
         BZ    BLKNXT                                                           
         MVC   P6,2(RE)                                                         
         B     LOOP16                                                           
*                                                                               
LOOP20   MVC   P(30),=CL30'END OF CHAIN'                                        
         GOTO1 VPRINTER                                                         
         B     CLOSE                                                            
         B     BLKNXT                                                           
*                                                                               
CLOSE    GOTO1 VDADDS,P1,15,,,DAFILE     CLOSE THE FILE                         
         MVC   P(20),=CL20'CLOSE'                                               
         GOTO1 VPRINTER                                                         
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
PRTBUFF  NTR1                                                                   
         LH    R0,P3+2                                                          
         GOTO1 VPRNTBL,DMCB,=C'D/A',P6,C'DUMP',4,=C'1D'                         
         GOTO1 VPRNTBL,DMCB,=C'IOA',AIOA,C'DUMP',(R0),=C'1D'                    
         XIT1  ,                                                                
PRTBUFF2 NTR1                                                                   
*        GOTO1 VPRNTBL,DMCB,=C'D/A',P6,C'DUMP',4,=C'1D'                         
*        LH    R0,P3+2                                                          
*        GOTO1 VPRNTBL,DMCB,=C'IOA',AIO2,C'DUMP',(R0),=C'1D'                    
*        XIT1                                                                   
*                                                                               
         MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
         MVC   P(4),=CL4'D/A='                                                  
         GOTO1 VHEXOUT,DMCB,P6,P+4,4                                            
         LH    R2,P3+2                                                          
         AHI   R2,-13                                                           
         A     R2,AIO2                                                          
         MVC   P+15(6),=CL8'HIKEY='                                             
         GOTO1 VHEXOUT,DMCB,(R2),P+21,13                                        
         GOTO1 VPRINTER                                                         
*        GOTO1 VPRNTBL,DMCB,=C'HIGH KEY',(R2),C'DUMP',13,=C'1D'                 
         XIT1  ,                                                                
         EJECT                                                                  
         ENTRY ADWAIT                                                           
         USING *,RF                                                             
ADWAIT   DS    0H                                                               
         LTR   RE,RE               RE IS NEG IF 31 BIT CALLER                   
         BP    ADWAITX                                                          
         AP    IOCOUNT,PCKD1                                                    
ADWAITX  BR    RE                                                               
         DROP  RF                                                               
IOCOUNT  DC    PL4'0'                                                           
PCKD1    DC    PL1'1'                                                           
         DS    0D                                                               
BLKIND   DC    XL4'FFFFFFFF'                                                    
VCPRINT  DC    V(CPRINT)                                                        
VDADDS   DC    V(DADDS)                                                         
VPRINTER DC    V(PRINTER)                                                       
VPRNTBL  DC    V(PRNTBL)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
         EJECT                                                                  
TRACK    DC    H'0'                                                             
BLOCK    DC    H'0'                                                             
         DS    0D                                                               
DUB      DC    D'0'                                                             
COUNT    DC    PL4'0'                                                           
MAXCOUNT DS    PL4                                                              
BADFLAG  DC    C'N'                                                             
CHAIN    DS    F                                                                
DMCB     DS    6F                                                               
P1       DC    F'0'                                                             
P2       DC    F'0'                                                             
P3       DC    F'0'                                                             
P4       DC    F'0'                                                             
P5       DC    F'0'                                                             
P6       DC    F'0'                                                             
DSKADR   DC    F'0'                                                             
AIOA     DC    A(IOA)                                                           
AIO2     DC    A(IO2)                                                           
*                                                                               
WORK     DC    20F'0'                                                           
C        DS    CL80                                                             
SVC      DS    CL80                                                             
MVSDUMP  DC    C'N'                                                             
KEYST    DC    X'00'                                                            
         DS    0D                                                               
         DC    CL8'**KEY**'                                                     
KEY      DC    XL32'00'                                                         
KEYSAVE  DC    XL32'00'                                                         
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
         PRINT GEN                                                              
DAFILE   DMDA  BLKSIZE=4628                                                     
D2FILE   DMDA  BLKSIZE=4628                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'**IOA**'                                                     
IOA      DS    16000C                                                           
         DS    0D                                                               
         DC    CL8'**IO2**'                                                     
IO2      DS    16000C                                                           
WORKD    DSECT                                                                  
         SPACE 2                                                                
SSB      CSECT                                                                  
         DC    X'0000',X'FF',X'88' WRITE TO FACWRK/COPIES REQUIRED              
         DC    X'00000000'                                                      
         DC    CL8' '                                                           
         DC    A(0),A(0)                                                        
         DC    XL32'00'                                                         
         SPACE 2                                                                
UTL      CSECT                                                                  
         DC    10F'0'                                                           
         SPACE 2                                                                
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050DMDATESTL 10/30/00'                                      
         END                                                                    
