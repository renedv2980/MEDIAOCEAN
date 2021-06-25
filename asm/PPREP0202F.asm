*          DATA SET PPREP0202F AT LEVEL 135 AS OF 05/01/02                      
*PHASE PP0202F,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
*                                                                               
*                                                                               
*       THIS PROGRAM WILL FIND ANY BUYS WHOSE ESTIMATE IS NOT ON FILE           
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
         ZAP   MESTCNT,=P'0'                                                    
         ZAP   PRDCNT,=P'0'                                                     
         ZAP   TSTCNT,=P'0'                                                     
         ZAP   DELCNT,=P'0'                                                     
         ZAP   CLOCNT,=P'0'                                                     
         ZAP   PAIDCNT,=P'0'                                                    
         ZAP   BILLCNT,=P'0'                                                    
         ZAP   LIVCNT,=P'0'                                                     
         ZAP   LIVBLCNT,=P'0'                                                   
         ZAP   LIVPDCNT,=P'0'                                                   
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT1,C'N'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    LASTCP,LASTCP       CLEAR LAST CLT/PRD                           
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'20'          BUYS                                        
         MVC   KEY+4(3),QCLIENT     *START AT CLIENT                            
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
PROC3    GOTO1 SEQ                                                              
PROC5    CLC   KEY(4),KEYSAVE      JUST CHECK AGY/MEDIA RECORD CODE             
         BNE   PROC80              END OF MEDIA GO DO NEXT                      
         OC    KEY+21(3),KEY+21    SKIP PASSIVE POINTERS                        
         BNZ   PROC3                                                            
         OC    LASTCP,LASTCP                                                    
         BZ    PROC6                                                            
         CLC   KEY+4(6),LASTCP                                                  
         BE    PROC25                                                           
*                                                                               
PROC6    DS    0H                      NEW PRODUCT REBUILD ESTTAB               
         MVC   LASTCP,KEY+4                                                     
         LA    R1,ESTTAB                                                        
         LA    R2,4                                                             
PROC7    XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R2,PROC7                                                         
*                                                                               
         MVC   MYKEY,KEY        SAVE KEY                                        
         MVC   MYKEYS,KEYSAVE   AND KEYSAVE                                     
*                                                                               
         OI    DMINBTS,X'88'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(3),MYKEY                                                     
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(6),MYKEY+4                                                 
         GOTO1 HIGH                                                             
         B     PROC10                                                           
PROC8    GOTO1 SEQ                                                              
*                                                                               
PROC10   CLC   KEY(10),KEYSAVE        CHK AGY/MED/CLT/PRD                       
         BNE   PROC20                                                           
         MVC   HALF,KEY+10                                                      
         LH    R2,HALF                                                          
         SH    R2,=H'1'                                                         
         LA    R1,ESTTAB                                                        
         AR    R1,R2                                                            
         MVI   0(R1),C'Y'                                                       
         B     PROC8                                                            
*                                                                               
PROC20   MVC   KEY(32),MYKEY                                                    
         GOTO1 HIGH                                                             
         MVC   KEYSAVE,MYKEYS       RESTORE KEYSAVE                             
*                                                                               
PROC25   DS    0H                                                               
         MVC   HALF,KEY+19                                                      
         LH    R2,HALF                                                          
         SH    R2,=H'1'                                                         
         LA    R1,ESTTAB                                                        
         AR    R1,R2                                                            
         CLI   0(R1),C'Y'                                                       
         BE    PROC3                                                            
*                                                                               
         AP    MESTCNT,=P'1'                                                    
*                                                                               
         MVC   P+2(16),=C'MISSING ESTIMATE'                                     
         MVC   P+20(3),KEY                                                      
         MVC   P+25(3),KEY+4                                                    
         MVC   P+30(3),KEY+7                                                    
         CVD   R2,DUB                                                           
         AP    DUB,=P'1'          SINCE I DECREMENTED BY ONE                    
         OI    DUB+7,X'0F'                                                      
         UNPK  P+35(3),DUB+6(2)                                                 
*                                                                               
         CLI   KEY+7,C'*'                                                       
         BNE   PROC30                                                           
         AP    PRDCNT,=P'1'                                                     
         B     PROC70                                                           
*                                                                               
PROC30   GOTO1 GETPRT                                                           
         CLI   PBDBFD,C'T'            SEE IF TEST BUY                           
         BNE   PROC40                                                           
         AP    TSTCNT,=P'1'                                                     
         MVC   P+40(6),=C'*TEST*'                                               
         B     PROC70                                                           
*                                                                               
PROC40   DS    0H                                                               
         TM    PBUYCNTL,X'80'         SEE IF DELETED                            
         BZ    PROC60                                                           
         MVC   P+40(5),=C'*DEL*'                                                
         AP    DELCNT,=P'1'                                                     
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'                                                     
PROC42   BAS   RE,NEXTEL                                                        
         BNE   PROC45                                                           
         OC    2(3,R2),2(R2)                                                    
         BZ    PROC42                                                           
         AP    PAIDCNT,=P'1'                                                    
         MVC   P+50(4),=C'PAID'                                                 
*                                                                               
PROC45   LA   R2,PBUYREC+33                                                     
         MVI  ELCODE,X'26'                                                      
PROC47   BAS  RE,NEXTEL                                                         
         BNE  PROC70                                                            
         OC   5(3,R2),5(R2)                                                     
         BZ   PROC47                                                            
         AP   BILLCNT,=P'1'                                                     
         MVC  P+60(6),=C'BILLED'                                                
         B    PROC70                                                            
*                                                                               
PROC60   DS    0H                                                               
         AP    LIVCNT,=P'1'                                                     
         MVC   P+40(6),=C'*LIVE*'                                               
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'                                                     
PROC62   BAS   RE,NEXTEL                                                        
         BNE   PROC65                                                           
         OC    2(3,R2),2(R2)                                                    
         BZ    PROC62                                                           
         AP    LIVPDCNT,=P'1'                                                   
         MVC   P+50(4),=C'PAID'                                                 
*                                                                               
PROC65   LA   R2,PBUYREC+33                                                     
         MVI  ELCODE,X'26'                                                      
PROC67   BAS  RE,NEXTEL                                                         
         BNE  PROC70                                                            
         OC   5(3,R2),5(R2)                                                     
         BZ   PROC67                                                            
         AP   LIVBLCNT,=P'1'                                                    
         MVC  P+60(6),=C'BILLED'                                                
         B    PROC70                                                            
*                                                                               
*                                                                               
PROC70   BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         AP    CLOCNT,=P'1'                                                     
         OI    KEY+25,X'C0'                                                     
         GOTO1 WRT                                                              
         B     PROC3                                                            
*                                                                               
PROC80   DS    0H                                                               
         CLI   KEY,X'FF'          END OF FILE                                   
         BE    EXIT                                                             
         XC    LASTCP,LASTCP                                                    
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+3,X'FF'             SKIP TO NEXT AGY/MED                     
         XC    KEY+4(28),KEY+4                                                  
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'               END OF FILE                              
         BE    EXIT                                                             
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'20'                                                      
         B     PROC2                                                            
*                                                                               
************************************* END OF NEW CODE                           
*                                                                               
TAPEGET  NTR1                                                                   
         GET   IN,REC-4                                                         
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0           END OF RECORD                                  
         XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
MESTCNT  DS    PL8                                                              
         DC    CL15'MISSING EST BUY'                                            
PRDCNT   DS    PL8                                                              
         DC    CL15'* PRD BUYS     '                                            
TSTCNT   DS    PL8                                                              
         DC    CL15'TEST BUYS      '                                            
DELCNT   DS    PL8                                                              
         DC    CL15'DELETED BUYS   '                                            
PAIDCNT  DS    PL8                                                              
         DC    CL15'DELETED PD BUYS'                                            
BILLCNT  DS    PL8                                                              
         DC    CL15'DELETED BL BUYS'                                            
LIVCNT   DS    PL8                                                              
         DC    CL15'LIVE BUYS      '                                            
LIVPDCNT DS    PL8                                                              
         DC    CL15'LIVE PD BUYS   '                                            
LIVBLCNT DS    PL8                                                              
         DC    CL15'LIVE BL BUYS   '                                            
CLOCNT   DS    PL8                                                              
         DC    CL15'BUYS CLOSED-OUT'                                            
         DC    X'FF'                                                            
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
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
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1                                                                   
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         IC    R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(2),PAGYKAGY                                                
         GOTO1 HIGHPUB                                                          
         B     NP2B                                                             
*                                                                               
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
NP2B     DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NPX                                                              
         CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LASTPUB,KEY+1                                                    
         GOTO1 GETNAME                                                          
         L     RF,ALTLREC                                                       
         XC    0(50,RF),0(RF)                                                   
         GOTO1 SEQPUB                                                           
         CLC   KEY(9),PUBKEY                                                    
         BNE   NPX                                                              
         GOTO1 GETLTL                                                           
*                                                                               
NPX      DS    0H                                                               
         B     EXIT                                                             
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
         LA    R2,280                                                           
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
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
LASTCP   DS    CL6                                                              
ELCODE   DS    X                                                                
MYKEY    DS    CL32          SAVE KEY                                           
MYKEYS   DS    CL32          SAVE KEYSAVE                                       
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
*                                                                               
ESTTAB   DS    1000C                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135PPREP0202F05/01/02'                                      
         END                                                                    
