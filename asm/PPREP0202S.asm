*          DATA SET PPREP0202S AT LEVEL 103 AS OF 05/01/02                      
*PHASE PP0202S,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202S - SPECIAL FOR BW - RESTORE UNCLOSED DATA'               
*                                                                               
*********  CHANGE LOG  ***********                                              
*                                                                               
*  SMYE  12/18/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   ESTCNT,=P'0'                                                     
         ZAP   BUCCNT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
*                                                                               
         CLI   QOPT1,C'N'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   DMINBTS,X'08'        SET TO PASS DELETES                         
         MVI   DMOUTBTS,X'FD'       SET TO PASS DELETES                         
*                                                                               
         OPEN  (IN,(INPUT))                                                     
GET      DS    0H                                                               
         BAS   RE,TAPEGET                                                       
         AP    INCNT,=P'1'                                                      
         CLC   REC(3),=X'FFFFFF'     END OF FILE                                
         BE    EOF                                                              
         CLC   REC(2),=C'BW'                                                    
         BE    GET3                                                             
         CLI   REC,0                IN CASE THERE IS NO TRAILER REC             
         BE    EOF                                                              
         B     GET                                                              
*                                  CHK MEDIA/CLIENT                             
GET3     MVC   WORK(1),REC+2                                                    
         MVC   WORK+1(3),REC+4                                                  
         LA    R2,MEDCLT                                                        
GET5     CLI   0(R2),X'FF'         END OF TABLE                                 
         BE    GET                                                              
         CLC   WORK(4),0(R2)                                                    
         BE    GET10                                                            
         LA    R2,4(R2)                                                         
         B     GET5                                                             
*                                                                               
GET10    B     CHKEST                                                           
*                                                                               
CHKEST   DS    0H                                                               
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
*                                                                               
         CLI   REC+3,X'07'                                                      
         BNE   CHKBUC                                                           
         MVC   WORK(3),REC     AGY/MED                                          
         MVC   WORK+3(3),REC+4   CLIENT                                         
         MVC   WORK+6(3),REC+7   PRODUCT                                        
         MVC   WORK+9(2),REC+10  EST                                            
         BAS   RE,SKIPCHK                                                       
         BNE   CHKEST5                                                          
         AP    SESTCNT,=P'1'                                                    
         B     GET                                                              
*                                                                               
CHKEST5  AP    ESTCNT,=P'1'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(20),REC                                                      
         BAS   RE,FMTEST                                                        
         GOTO1 ADDPRT                                                           
         B     GET                                                              
*                                                                               
CHKBUC   CLI   REC+3,X'09'                                                      
         BNE   CHKBILL                                                          
         MVC   WORK(3),REC     AGY/MED                                          
         MVC   WORK+3(3),REC+4   CLIENT                                         
         MVC   WORK+6(3),REC+7   PRODUCT                                        
         MVC   WORK+9(2),REC+10  EST                                            
         BAS   RE,SKIPCHK                                                       
         BNE   CHKBUC5                                                          
         AP    SBUCCNT,=P'1'                                                    
         B     GET                                                              
*                                                                               
CHKBUC5  AP    BUCCNT,=P'1'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(20),REC                                                      
         BAS   RE,FMTBKT                                                        
         GOTO1 ADDPRT                                                           
         B     GET                                                              
*                                                                               
CHKBILL  CLI   REC+3,X'08'                                                      
         BNE   CHKBUY                                                           
         MVC   WORK(3),REC     AGY/MED                                          
         MVC   WORK+3(3),REC+4   CLIENT                                         
         MVC   WORK+6(3),REC+7   PRODUCT                                        
         MVC   WORK+9(2),REC+10  EST                                            
         BAS   RE,SKIPCHK                                                       
         BNE   CHKBILL5                                                         
         AP    SBILLCNT,=P'1'                                                   
         B     GET                                                              
*                                                                               
CHKBILL5 AP    BILLCNT,=P'1'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         GOTO1 ADDPRT                                                           
         B     GET                                                              
*                                                                               
CHKBUY   CLI   REC+3,X'20'                                                      
         BNE   CHKUNK                                                           
         MVC   WORK(3),REC     AGY/MED                                          
         MVC   WORK+3(3),REC+4   CLIENT                                         
         MVC   WORK+6(3),REC+7   PRODUCT                                        
         MVC   WORK+9(2),REC+19  EST                                            
         BAS   RE,SKIPCHK                                                       
         BNE   CHKBUY5                                                          
         AP    SBUYCNT,=P'1'                                                    
         B     GET                                                              
*                                                                               
CHKBUY5  AP    BUYCNT,=P'1'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         MVC   KEY+25(2),REC+27          CONTROL BYTES                          
         GOTO1 ADDPRT                                                           
         CLC   REC(25),LASTREC                                                  
         BE    EOF                                                              
         B     GET                                                              
*                                                                               
CHKUNK   AP    UNKCNT,=P'1'                                                     
         B     GET                                                              
*                                                                               
LASTREC  DC    X'C2E6E220D7C9E9E2C6C40000540000005B0902007000000001'            
         EJECT                                                                  
*                                                                               
FMTEST   NTR1                                                                   
         LA    R7,P                                                             
         USING UBLIND,R7                                                        
         LA    R6,REC                                                           
         USING PESTREC,R6                                                       
*                                                                               
         MVC   UBREC,=C'EST'                                                    
*                                                                               
         MVC   UBMED,PESTKMED                                                   
         MVC   UBCLT,PESTKCLT                                                   
         MVC   UBPRD,PESTKPRD                                                   
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UBEST,DUB                                                        
         BAS   RE,RPRT                                                          
FMTESTX  XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         DROP  R7                                                               
*                                                                               
*                                                                               
FMTBKT   NTR1                                                                   
         LA    R7,P                                                             
         USING UBLIND,R7                                                        
         LA    R6,REC                                                           
         USING PBKREC,R6                                                        
*                                                                               
         MVC   UBREC,=C'BKT'                                                    
*                                                                               
         MVC   UBMED,PBKKMED                                                    
         MVC   UBCLT,PBKKCLT                                                    
         MVC   UBPRD,PBKKPRD                                                    
         MVC   HALF,PBKKEST                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UBEST,DUB                                                        
         BAS   RE,RPRT                                                          
FMTBKTX  XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         DROP  R7                                                               
*************************                                                       
         EJECT                                                                  
*                                                                               
TAPEGET  NTR1                                                                   
         GET   IN,REC-4                                                         
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0           END OF RECORD                                  
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
         DC    F'0'                                                             
SKIPCHK  ST    RE,SKIPCHK-4                                                     
         LA    R1,SKIPTAB                                                       
SKIP5    CLI   0(R1),X'FF'                                                      
         BE    NOSKIP                                                           
         CLC   WORK(11),0(R1)                                                   
         BE    YESSKIP                                                          
         LA    R1,11(R1)                                                        
         B     SKIP5                                                            
*                                                                               
NOSKIP   L     RE,SKIPCHK-4                                                     
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
YESSKIP  L     RE,SKIPCHK-4                                                     
         C     RE,SKIPCHK-4                                                     
         BR    RE                                                               
*                                                                               
*        TABLE OF ESTS TO SKIP                                                  
*                                                                               
SKIPTAB  DC    C'BWSPIZFRS',AL2(106)                                            
         DC    C'BWSPIZSAC',AL2(100)                                            
         DC    C'BWSPIZSAC',AL2(101)                                            
         DC    C'BWSPIZSFO',AL2(102)                                            
         DC    C'BWSPIZSFO',AL2(103)                                            
         DC    C'BWSPIZSFO',AL2(104)                                            
         DC    C'BWSPIZSFO',AL2(105)                                            
         DC    X'FFFF'                                                          
*                                                                               
FMTBUY   NTR1                                                                   
         GOTO1 GETINS,DMCB,REC,GROSS,REC+7,0                                    
         TM    PBUYCNTL,X'80'        SEE IF DELETED                             
         BZ    *+10                                                             
         XC    GROSS(12),GROSS                                                  
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BE    FMTBUYX                                                          
         LA    R7,P                                                             
         USING UBLIND,R7                                                        
*                                                                               
         MVC   UBMED,PBUYKMED                                                   
         MVC   UBCLT,PBUYKCLT                                                   
         MVC   UBPRD,PBUYKPRD                                                   
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UBEST,DUB                                                        
*        GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(4,UBYDAT)                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(7,UBYDAT)                              
         LA    R4,UBYDAT+5                                                      
         CLI   PBDFREQ,C'M'                                                     
         BNE   *+14                                                             
         MVC   UBYDAT+3(2),SPACES                                               
         LA    R4,UBYDAT+3                                                      
         CLI   PBUYKLIN,1                                                       
         BE    PRBY6                                                            
         SR    R5,R5                                                            
         IC    R5,PBUYKLIN                                                      
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R4),DUB                                                      
         MVI   0(R4),C'-'                                                       
*                                                                               
PRBY6    TM    PBUYCNTL,X'80'                                                   
         BZ    *+8                                                              
         MVI   UBYDAT-1,C'D'                                                    
*                                                                               
PRBY8    EQU   *                                                                
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,UBPUB                                      
*                                                                               
PRBY10   DS    0H                                                               
         MVC   DMCB(12),GROSS                                                   
         LA    R2,DMCB                                                          
         BAS   RE,EDIT3                                                         
FMTBUYX  XIT1                                                                   
*                                                                               
*                                                                               
EDIT3    EQU   *                                                                
         LA    R3,3                                                             
         LA    R4,UGRS                                                          
EDIT3A   EQU   *                                                                
         L     R0,0(R2)                                                         
         EDIT  (R0),(15,0(R4)),2,COMMAS=YES,CR=YES                              
         LA    R2,4(R2)                                                         
         LA    R4,16(R4)                                                        
         BCT   R3,EDIT3A                                                        
         BR    RE                                                               
*                                                                               
         DROP  R6                                                               
         DROP  R7                                                               
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DC    PL8'0'                                                           
         DC    CL20'TAPE RECS READ'                                             
ESTCNT   DC    PL8'0'                                                           
         DC    CL20'ESTS RESTORED'                                              
BUCCNT   DC    PL8'0'                                                           
         DC    CL20'BUCKETS RESTORED'                                           
BILLCNT  DC    PL8'0'                                                           
         DC    CL20'BILLS FOUND'                                                
BUYCNT   DC    PL8'0'                                                           
         DC    CL20'BUYS FOUND'                                                 
UNKCNT   DC    PL8'0'                                                           
         DC    CL20'UNKNOWN RECS'                                               
SESTCNT  DC    PL8'0'                                                           
         DC    CL20'SKIPPED ESTS'                                               
SBUCCNT  DC    PL8'0'                                                           
         DC    CL20'SKIPPED BUCS'                                               
SBILLCNT DC    PL8'0'                                                           
         DC    CL20'SKIPPED BILLS'                                              
SBUYCNT  DC    PL8'0'                                                           
         DC    CL20'SKIPPED BUYS'                                               
         DC    X'FF'                                                            
*                                                                               
MEDCLT   DC    C'NPIZ'                                                          
         DC    C'SPIZ'                                                          
         DC    X'FFFFFFFF'                                                      
*                                                                               
RUNL     DS    0H                                                               
         B     RUNL80                                                           
************************************ OLD CODE                                   
         LA    R5,MEDTAB                                                        
RUNL3    CLI   0(R5),X'FF'           END OF TABLE                               
         BE    RUNL80                                                           
         MVI   FORCEHED,C'Y'                                                    
         XC    BUFREC,BUFREC                                                    
         MVC   BUFREC(1),0(R5)                                                  
         MVC   MEDTYP,0(R5)                                                     
         MVC   RCSUBPRG(1),1(R5)                                                
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     RUNL10                                                           
*                                                                               
RUNL5    GOTO1 BUFFALO,DMCB,=C'SEQ',(MEDTYP,BUFFBUFF),BUFREC,0                  
RUNL10   CLI   DMCB+8,X'80'         END OF FILE                                 
         BE    RUNL25                                                           
         CLC   BUFMED,0(R5)                                                     
         BNE   RUNL25                                                           
*                                                                               
         MVC   P+1(20),BUFPNM                                                   
         CLC   BUFPNM(10),=10C'Z'                                               
         BNE   *+10                                                             
         MVC   P+1(20),=CL20'** PUB NOT ON FILE **'                             
         CLC   BUFPNM(10),=10X'FF'                                              
         BNE   RUNL13                                                           
         MVC   P+1(20),=CL20'** MEDIA   TOTALS **'                              
         MVC   P+10(1),BUFMED                                                   
         B     RUNL15                                                           
*                                                                               
RUNL13   MVC   P+22(20),BUFZNM                                                  
         ZIC   R3,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R3),BUFPUB),P+45                                  
*                                                                               
RUNL15   EDIT  (P8,BUFGRS),(14,P+63),2,COMMAS=YES,MINUS=YES                     
         ZAP   MYDUB,BUFGRS                                                     
         SP    MYDUB,BUFAC      NET                                             
         EDIT  (P8,MYDUB),(14,P+78),2,COMMAS=YES,MINUS=YES                      
*                                                                               
         EDIT  (P8,BUFCD),(14,P+93),2,COMMAS=YES,MINUS=YES                      
         MVC   RCSUBPRG(1),1(R5)                                                
         BAS   RE,RPRT                                                          
         B     RUNL5                                                            
*                                                                               
RUNL25   LA    R5,2(R5)                                                         
         B     RUNL3                                                            
*                                                                               
MEDTAB   DC    C'M',AL1(10)                                                     
         DC    C'N',AL1(20)                                                     
         DC    C'T',AL1(30)                                                     
         DC    X'FFFF'                                                          
*                                                                               
RUNL80   MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL85   CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(20),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,28(R4)                                                        
         B     RUNL85                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
EOF      CLOSE (IN,)                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)                                                      
         LH    R2,HALF              USE RECORD LENGHT                           
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         LTORG                                                                  
*                                                                               
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         SPACE 2                                                                
         BUFF  LINES=2000,ROWS=1,COLUMNS=3,FLAVOR=PACKED,KEYLIST=(47,A)X        
               ,COMMENT=10                                                      
         EJECT                                                                  
*                                                                               
PP02WRKD DSECT                                                                  
BUFREC   DS    0CL81                                                            
BUFMED   DS    CL1                                                              
BUFPNM   DS    CL20                                                             
BUFZNM   DS    CL20                                                             
BUFPUB   DS    CL6                                                              
BUFCOM   DS    CL10                                                             
BUFGRS   DS    PL8                                                              
BUFAC    DS    PL8                                                              
BUFCD    DS    PL8                                                              
*                                                                               
         DS    CL20                                                             
MEDTYP   DS    CL1                                                              
MYDUB    DS    PL8                                                              
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    CL50                                                             
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFIO   DS    A                                                                
BUFFBUFF DS    A                                                                
ITOT     DS    F                                                                
*                                                                               
         EJECT                                                                  
UBLIND   DSECT                                                                  
         DS    CL5                                                              
UBREC    DS    CL3                                                              
         DS    CL10                                                             
UBMED    DS    CL1                                                              
         DS    CL1                                                              
UBCLT    DS    CL3                                                              
         DS    CL1                                                              
UBPRD    DS    CL3                                                              
         DS    CL3                                                              
UBEST    DS    CL3                                                              
         DS    CL1                                                              
UBMOS    DS    CL6                                                              
         DS    CL1                                                              
UBBMN    DS    CL6                                                              
         DS    CL1                                                              
UBINV    DS    CL4                                                              
         DS    CL2                                                              
UBSEP    DS    CL3              ADJ/CD                                          
         DS    CL2                                                              
UBRET    DS    CL3              RET/SUM/COR/AOR                                 
         DS    CL3                                                              
UGRS     DS    CL15                                                             
         DS    CL1                                                              
UBIL     DS    CL15                                                             
         DS    CL1                                                              
UNET     DS    CL15                                                             
         ORG   UBMOS                                                            
UBYDAT   DS    CL8                                                              
         DS    CL1                                                              
UBPUB    DS    CL15                                                             
         DS    CL1                                                              
         ORG                                                                    
PBKRECD  DSECT                                                                  
       ++INCLUDE PBKREC                                                         
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'103PPREP0202S05/01/02'                                      
         END                                                                    
