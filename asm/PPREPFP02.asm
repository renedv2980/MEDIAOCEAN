*          DATA SET PPREPFP02  AT LEVEL 013 AS OF 05/18/10                      
*PHASE PPFP02A,+0,NOAUTO                                                        
*INCLUDE PPGETSPC                                                               
*INCLUDE PPBVAL                                                                 
*INCLUDE MININAM                                                                
         TITLE 'PPFP02 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM WILL LIST SELECTED INFORMATION FROM BUYS                  
*        RELATING TO CANADIAN PROVINCIAL STATE TAX CODES                        
*        IF A BUY IS NEITHER BILLED NOR PAID, THE BUYREC PST ELEMENT            
*          (X'84' ELEMENT) WILL BE DELETED                                      
*        ALSO, THE PUB PST TAX CODE ELEMENT (X'90' ELEMENT)                     
*          WILL BE DELETED                                                      
*                                                                               
* ******************************************************************            
* *****  THIS PROCESS MUST BE LIMITED TO ONLY ONE PUBLICATION  *****            
* ******************************************************************            
*                                                                               
*    QOPT5   N = DON'T MARK FILE (EVEN IF WRITE=YES)                            
*    QOPT6   Y = DUMP FIRST 25 RECORDS                                          
*                                                                               
*                                                                               
         PRINT NOGEN                                                            
PPFP02   CSECT                                                                  
         NMOD1 0,PPFP02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'                                                    
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCBUY                                                     
         BE    PROC                                                             
         CLI   MODE,FBUYREQ                                                     
         BE    RUNF                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    RUNL                                                             
         CLI   MODE,FBUYCLI                                                     
         BE    PUBDEL                                                           
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   BUYCNT,=P'0'        BUY HEADER COUNT                             
         ZAP   ELECNT,=P'0'        ELEMENT DELETED COUNT                        
         MVI   PUBDELSW,0          OK TO DELETE PUB PST ELEMENT                 
         LA    R0,PBUYREC                                                       
         ST    R0,ADBUY                                                         
         OI    DMINBTS,X'08'      PASS DELETES                                  
         OI    DMOUTBTS,X'FD'     PASS DELETES                                  
         B     EXIT                                                             
*                                                                               
*                                                                               
PUBDEL   DS    0H                                                               
         CLI   PUBDELSW,1          HAS PUB PST ELEM BEEN CHECKED ?              
         BE    EXIT                YES                                          
*                                                                               
         LA    R4,KEY                                                           
         USING PUBREC,R4                                                        
         XC    KEY,KEY                                                          
         MVC   PUBKMED,PBUYKMED                                                 
         MVC   PUBKPUB(6),PBUYKPUB                                              
         MVC   PUBKAGY,PBUYKAGY                                                 
         MVI   PUBKCOD,X'81'                                                    
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         GOTO1 GETNAME                                                          
*                                                                               
         MVI   PUBDELSW,1          DO NOT TEST PUB AGAIN                        
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'90'        PST TAX CODE ELEM                            
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                ELEMENT NOT FOUND                            
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY+27,P1+57,4     OUTPUT DISK ADDRESS               
         MVC   P1+75(31),=C'*** PUB PST ELEMENT DELETED ***'                    
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
         AP    ELECNT,=P'1'                                                     
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PD10                NO                                           
         MVC   P1+10(14),=C'*** BEFORE ***'                                     
         BAS   RE,RPRT                                                          
         LA    R5,PUBREC           DMPREC USES R5 AS RECORD ADDRESS             
         BAS   RE,DMPREC           "BEFORE" DUMP                                
*                                                                               
PD10     GOTO1 RECUP,DMCB,(1,PUBREC),0(R2),0          DELETE ELEMENT            
*                                  CLEAR END OF RECORD                          
         MVC   HALF,PUBREC+25                                                   
         LH    R1,HALF                                                          
         LA    RE,PUBREC                                                        
         LR    RF,RE                                                            
         AR    RE,R1                                                            
         LA    RF,4000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         CLI   QOPT5,C'N'          WRITE BACK RECORD ?                          
         BE    PD40                NO                                           
         GOTO1 PUTPUB                                                           
*                                                                               
PD40     DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   EXIT                NO                                           
         MVC   P1+10(14),=C'*** AFTER  ***'                                     
         BAS   RE,RPRT                                                          
         LA    R5,PUBREC           DMPREC USES R5 AS RECORD ADDRESS             
         BAS   RE,DMPREC           "AFTER" DUMP                                 
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
PROC     DS    0H                                                               
         MVC   P,SPACES                                                         
         MVI   BUYDELSW,0          OK TO DELETE BUY PST ELEMENT                 
         AP    BUYCNT,=P'1'                                                     
*                                                                               
         MVC   P+1(3),PBUYKCLT                                                  
         MVC   P+6(3),PBUYKPRD                                                  
         EDIT  (2,PBUYKEST),(3,P+11),ALIGN=LEFT,FILL=0                          
         TM    PBUYCNTL,X'80'        SEE IF DELETED                             
         BZ    *+8                                                              
         MVI   P+15,C'D'                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(5,P+16)                                
         CLI   PBUYKLIN,1                                                       
         BNH   PROC5                                                            
         MVI   P+24,C'-'                                                        
         EDIT  (B1,PBUYKLIN),(2,P+25),ALIGN=LEFT                                
                                                                                
PROC5    DS    0H                                                               
         GOTO1 HEXOUT,DMCB,KEY+27,P+57,4     OUTPUT BUY DISK ADDRESS            
*                                                                               
         LA    R2,PBUYREC+33                                                    
         USING PBDELEM,R2                                                       
         MVC   P+41(1),PBDGST                                                   
         DROP  R2                                                               
*                                                                               
         MVI   ELCODE,X'26'                                                     
AGYC8    BAS   RE,NEXTEL                                                        
         BNE   AGYC10                                                           
         USING PBILELEM,R2                                                      
         OC    PBLDATE,PBLDATE     BILLED DATE PRESENT ?                        
         BZ    AGYC8               NO - SEE IF ANOTHER BILL ELEMENT             
         MVI   P+28,C'Y'           YES                                          
         MVI   BUYDELSW,1          DO NOT DELETE BUY PST ELEMENT                
         DROP  R2                                                               
*                                                                               
AGYC10   DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'                                                     
AGYC10A  BAS   RE,NEXTEL                                                        
         BNE   AGYC12                                                           
         USING PPAYELEM,R2                                                      
         OC    PPDDATE,PPDDATE     PAID DATE PRESENT ?                          
         BZ    AGYC10A             NO - SEE IF ANOTHER PAY ELEMENT              
         MVI   P+35,C'Y'           YES                                          
         MVI   BUYDELSW,1          DO NOT DELETE BUY PST ELEMENT                
         DROP  R2                                                               
*                                                                               
AGYC12   DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'84'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   AGYC20              NO PST CODES ELEM                            
         MVC   P+45(10),2(R2)      OUTPUT PST CODES                             
*                                                                               
         CLI   BUYDELSW,1          OK TO DELETE PST ELEMENT ?                   
         BE    AGYC20              NO                                           
*                                                                               
*****    GOTO1 HEXOUT,DMCB,KEY+27,P+57,4     OUTPUT DISK ADDRESS                
*                                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   AGYC14              NO                                           
         CP    ELECNT,=P'25'       25 RECORDS DUMPED ?                          
         BH    AGYC14              YES                                          
         MVC   P1+10(14),=C'*** BEFORE ***'                                     
         BAS   RE,RPRT                                                          
         LA    R5,PBUYREC          DMPREC USES R5 AS RECORD ADDRESS             
         BAS   RE,DMPREC           "BEFORE" DUMP                                
*                                                                               
AGYC14   GOTO1 RECUP,DMCB,(1,PBUYREC),0(R2),0          DELETE ELEMENT           
*                                  CLEAR END OF RECORD                          
         MVC   HALF,PBUYREC+25                                                  
         LH    R1,HALF                                                          
         LA    RE,PBUYREC                                                       
         LR    RF,RE                                                            
         AR    RE,R1                                                            
         LA    RF,3999(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         MVC   P+75(29),=C'** BUY PST ELEMENT DELETED **'                       
         AP    ELECNT,=P'1'                                                     
         CLI   QOPT5,C'N'          WRITE BACK RECORD ?                          
         BE    AGYC16              NO                                           
         GOTO1 PUTPRT                                                           
*                                                                               
AGYC16   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   AGYC20              NO                                           
         CP    ELECNT,=P'26'       25 RECORDS DUMPED ?                          
         BH    AGYC20              YES                                          
         MVC   P1+10(14),=C'*** AFTER  ***'                                     
         BAS   RE,RPRT                                                          
         LA    R5,PBUYREC          DMPREC USES R5 AS RECORD ADDRESS             
         BAS   RE,DMPREC           "AFTER" DUMP                                 
*                                                                               
AGYC20   DS    0H                                                               
         MVC   P1,P                PRINT INFO HELD IN P                         
         BAS   RE,RPRT                                                          
         B     EXIT                NEXT BUY                                     
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P1(15),8(R4)                                                     
         OI    7(R4),X'0F'                                                      
         UNPK  P1+20(10),0(8,R4)                                                
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
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
*                                                                               
NEXTAM   DS    0H                  GET NEXT AGY/MED                             
         MVC   WORK(3),KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(3),WORK                                                      
         MVI   KEY+3,X'FF'                                                      
*****    B     AGYC2                                                            
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
         GOTO1 HEXOUT,DMCB,(R5),P1+01,(R2),=C'N'                                
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P1+75(25),WORK                                                   
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         ZICM  R2,25(R5),2         R5 POINTS TO RECORD TO BE DUMPED             
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
         MVC   P1+01(8),WORK+00                                                 
         MVC   P1+10(8),WORK+08                                                 
         MVC   P1+19(8),WORK+16                                                 
         MVC   P1+28(8),WORK+24                                                 
         MVC   P1+37(8),WORK+32                                                 
         MVC   P1+46(8),WORK+40                                                 
         MVC   P1+55(8),WORK+48                                                 
         MVC   P1+64(8),WORK+56                                                 
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P1+75(0),WORK                                                    
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
COUNTS   DS    0C                                                               
BUYCNT   DS    PL8                                                              
         DC    CL15'BUYS READ'                                                  
ELECNT   DS    PL8                                                              
         DC    CL15'ELEMS DELETED'                                              
         DC    X'FF'                                                            
*                                                                               
SAVKEY   DS    CL32                                                             
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
PUBDELSW DS    CL1                 1 = ELEMENT HAS BEEN DELETED                 
BUYDELSW DS    CL1                 1 = DO NOT DELETE ELEMENT                    
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
BUYCNTD  DS    PL8                                                              
LASTPUB  DS    XL6                                                              
SVMCD    DS    XL3                                                              
VALUES   DS    CL8            SAVED PPGETSPC VALUES                             
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
PPBVWORK DS    0D                                                               
       ++INCLUDE PPBVALD                                                        
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREPFP02 05/18/10'                                      
         END                                                                    
