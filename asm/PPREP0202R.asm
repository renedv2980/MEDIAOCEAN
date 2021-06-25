*          DATA SET PPREP0202R AT LEVEL 133 AS OF 05/01/02                      
*PHASE PP0202R,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202R - SPECIAL FOR BW - ACD AUDIT'                           
*                                                                               
*************  CHANGE LOG  ***********                                          
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
         ZAP   BUYCNT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
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
         BH    EOF                                                              
         BL    GET                                                              
*                                  CHK MEDIA/CLIENT                             
         MVC   WORK(1),REC+2                                                    
         MVC   WORK+1(3),REC+4                                                  
         LA    R2,MEDCLT                                                        
GET5     CLI   0(R2),X'FF'         END OF TABLE                                 
         BE    GET                                                              
         CLC   WORK(4),0(R2)                                                    
         BE    GET10                                                            
         LA    R2,4(R2)                                                         
         B     GET5                                                             
*                                                                               
GET10    B     CHKBUY                                                           
*                                                                               
CHKBUY   CLI   REC+3,X'20'                                                      
         BNE   GET                                                              
         CLC   REC+16(3),=X'560101'        SEE IF BEFORE JAN01/86               
         BL    GET                                                              
         CLC   REC+16(3),=X'560930'       OR AFTER SEP30/86                     
         BH    GET                                                              
*                                                                               
         AP    BUYCNT,=P'1'                                                     
*                                                                               
         LA    R6,REC                                                           
         USING PBUYREC,R6                                                       
         BAS   RE,FMTBUY                                                        
         CLI   QOPT1,C'Y'                                                       
         BE    CKBUY10                                                          
         BAS   RE,RPRT                                                          
*                                                                               
CKBUY10  BAS   RE,PUTBUFF                                                       
         B     GET                                                              
*                                                                               
         DC    F'0'                                                             
PUTBUFF  ST    RE,PUTBUFF-4                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(1),PBUYKMED                                                  
         MVC   KEY+1(6),PBUYKPUB                                                
         MVC   KEY+7(2),PBUYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         CLC   KEY(10),PUBREC        SEE IF I ALREADY HAVE                      
         BE    PUTB20                                                           
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BNE   PUTB10                                                           
         GOTO1 GETNAME                                                          
         B     PUTB20                                                           
*                                                                               
PUTB10   MVC   PUBNAME,SPACES          PUB NOT FOUND                            
         MVC   PUBNAME(10),=CL10'ZZZZZZZZZZ'                                    
         MVC   PUBZNAME,SPACES                                                  
         B     PUTB20                                                           
*                                                                               
PUTB20   DS    0H                                                               
         MVC   BUFMED,PBUYKMED                                                  
         MVC   BUFPNM,PUBNAME                                                   
         MVC   BUFZNM,PUBZNAME                                                  
*                                                                               
         MVC   BUFPUB,PBUYKPUB                                                  
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         ZAP   BUFGRS,DUB                                                       
         L     R0,AGYCOM                                                        
         CVD   R0,DUB                                                           
         ZAP   BUFAC,DUB                                                        
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         ZAP   BUFCD,DUB                                                        
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         MVC   BUFPNM,=20X'FF'             ADD TO MEDIA TOTALS                  
         MVC   BUFZNM,SPACES                                                    
         XC    BUFPUB,BUFPUB                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         L     RE,PUTBUFF-4                                                     
         BR    RE                                                               
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
INCNT    DS    PL8                                                              
         DC    CL20'TAPE RECS READ'                                             
BUYCNT   DS    PL8                                                              
         DC    CL20'BUYS FOUND'                                                 
         DC    X'FF'                                                            
*                                                                               
MEDCLT   DC    C'MACD'                                                          
         DC    C'NACD'                                                          
         DC    C'OACD'                                                          
         DC    C'SACD'                                                          
         DC    C'TACD'                                                          
         DC    X'FFFFFFFF'                                                      
*                                                                               
RUNL     DS    0H                                                               
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
******** MVC   HEAD5+62(8),=C'WRITE=NO'                                         
******** CLI   RCWRITE,C'Y'                                                     
******** BNE   *+10                                                             
******** MVC   HEAD5+68(3),=C'YES'                                              
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
         DS    CL18                                                             
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
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'133PPREP0202R05/01/02'                                      
         END                                                                    
