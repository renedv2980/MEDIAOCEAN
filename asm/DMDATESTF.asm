*          DATA SET DMDATESTF  AT LEVEL 007 AS OF 04/11/02                      
*          DATA SET DMDATESTL  AT LEVEL 017 AS OF 08/24/99                      
*          DATA SET DMDATEST   AT LEVEL 061 AS OF 08/03/99                      
*PHASE DMDATSTF                                                                 
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
*        BRAS  RE,FIXIT            FIX BAD RECORD                               
*        MVC   P(20),=CL20'FIXED FUCKER'                                        
*        GOTO1 VPRINTER                                                         
*                                                                               
TRK1     ICM   RF,15,=XL4'00101000'   START AT X'20111000'                      
         SRL   RF,16                                                            
         STH   RF,TRACK                                                         
         B     BLK1                                                             
*                                                                               
TRKNXT   LH    RF,TRACK            NEXT TRACK                                   
         LA    RF,1(RF)                                                         
         STH   RF,TRACK                                                         
*                                                                               
BLK1     LHI   RF,1                FIRST BLOCK                                  
         STH   RF,BLOCK                                                         
         B     LOOP02                                                           
*                                                                               
BLKNXT   LH    RF,BLOCK            NEXT BLOCK                                   
         LA    RF,1(RF)                                                         
         STH   RF,BLOCK                                                         
*                                                                               
LOOP02   XR    RE,RE               MOD 15 IF REMAINDER IS 1 SKIP                
         LH    RF,TRACK                                                         
         D     RE,=A(15)                                                        
         CHI   RE,1                                                             
         BNE   LOOP03                                                           
*                                                                               
         XC    P6(4),P6                                                         
         MVC   P6(2),TRACK         SET TTTT0000                                 
         MVI   P6+2,1              SET TTTTBB00                                 
         GOTO1 VHEXOUT,DMCB,P6,P+1,4                                            
         MVC   P+10(8),=CL8'READ'                                               
         GOTO1 VPRINTER                                                         
         B     TRKNXT                                                           
*                                                                               
LOOP03   XC    P6(4),P6                                                         
         MVC   P6(2),TRACK         SET TTTT0000                                 
         MVC   P6+2(1),BLOCK+1     SET TTTTBB00                                 
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
*OOP06   GOTO1 VHEXOUT,DMCB,P6,P+1,4                                            
*        MVC   P+10(8),=CL8'READ'                                               
*        GOTO1 VPRINTER                                                         
*&&DO                                                                           
LOOP06   L     R3,AIOA                                                          
         CLC   =XL4'100000F7',2(R3)                                             
         BNE   BLKNXT                                                           
         BRAS  RE,PRTBUFF                                                       
         B     BLKNXT                                                           
*&&                                                                             
*OOP06   CLC   P6,=XL4'0C110000'   END AT X'3A110000'                           
*        BH    CLOSE                                                            
LOOP06   L     R3,AIOA                                                          
         CLC   BLKIND,2(R3)        IS THIS AN INDEX BLOCK?                      
         BNE   BLKNXT              NO                                           
         BRAS  RE,PRTBUFF                                                       
*                                                                               
         LA    R3,6(R3)            POINT TO FIRST D/A IN LIST                   
LOOP08   MVC   P6,0(R3)                                                         
         GOTO1 VDADDS,P1,01,AIO2,0,,P6                                          
         OC    P3(2),P3                                                         
         BZ    LOOP10                                                           
         DCHO                                                                   
*                                                                               
LOOP10   L     RE,AIO2                                                          
         CLC   2(4,RE),17(R3)                                                   
         BE    LOOP12                                                           
         MVC   P(10),=CL10'BAD LINK'                                            
         GOTO1 VPRINTER                                                         
*                                                                               
LOOP12   BRAS  RE,PRTBUFF2                                                      
         AHI   R3,KEYLEN                                                        
         OC    0(4,R3),0(R3)                                                    
         BNZ   LOOP08                                                           
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
         LH    R0,P3+2                                                          
         GOTO1 VPRNTBL,DMCB,=C'D/A',P6,C'DUMP',4,=C'1D'                         
         GOTO1 VPRNTBL,DMCB,=C'IOA',AIO2,C'DUMP',(R0),=C'1D'                    
         XIT1  ,                                                                
         EJECT                                                                  
FIXIT    NTR1  ,                                                                
         MVC   P6,=XL4'2AB31300'   SCREWED UP D/A                               
         GOTO1 VDADDS,P1,01,AIOA,0,,P6                                          
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOA             FIX POINTER                                  
         MVC   2(4,R3),=XL4'2AA51300'                                           
*                                                                               
         XC    P3,P3               SET WRITE LENGTH                             
         MVC   P3+2(2),=AL2(1954)                                               
         MVC   P6,=XL4'2AB31300'   SCREWED UP D/A                               
*                                                                               
         GOTO1 VDADDS,P1,04,AIOA,,,P6                                           
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1  ,                                                                
*                                                                               
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
**PAN#1  DC    CL21'007DMDATESTF 04/11/02'                                      
         END                                                                    
