*          DATA SET PPREP03022 AT LEVEL 008 AS OF 05/01/02                      
*PHASE PP03022,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
PP0302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0302                                                         
*                                                                               
*                                                                               
*       THIS PROGRAM WILL DELETE (X'80') AGENCY "H0" RECORDS                    
*         ( X'07' ESTIMATE, X'09' EST BUCKET, X'20' BUY)                        
*              IF NO PRODUCT HEADERS EXIST FOR THEM                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP03WRKD,R8                                                      
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
         ZAP   TOTCNT,=P'0'                                                     
         ZAP   ESTCNT,=P'0'                                                     
         ZAP   ESTBKCNT,=P'0'                                                   
         ZAP   BUYCNT,=P'0'                                                     
         ZAP   BUYSIN,=P'0'                                                     
         ZAP   H0CNT,=P'0'                                                      
         ZAP   CLOSECNT,=P'0'                                                   
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         CLI   QOPT6,C'N'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,PBUYREC          USE AS I/O AREA                              
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
PROC3    GOTO1 SEQ                                                              
PROC5    DS    0H                                                               
         CLC   KEY(2),KEYSAVE      JUST CHECK AGY                               
         BNE   EXIT                END OF JOB                                   
         AP    TOTCNT,=P'1'                                                     
*                                                                               
         CLC   KEY(2),=C'H0'       TEST FOR AGENCY H0                           
         BNE   PROC3               GO GET ANOTHER RECORD                        
         AP    H0CNT,=P'1'                                                      
*                                                                               
         CLI   KEY+3,X'07'         ESTIMATE?                                    
         BE    PROC10              GO CHECK FOR PRODUCT HEADER                  
         CLI   KEY+3,X'09'         EST BUCKET?                                  
         BE    PROC10              GO CHECK FOR PRODUCT HEADER                  
         CLI   KEY+3,X'20'         BUY (INSERTION)?                             
         BNE   PROC3               NO - GO GET NEXT RECORD                      
*                                                                               
PROC10   DS    0H                  LOOK FOR PRODUCT HEADER                      
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY FOR TESTING AND RESETTING           
         MVI   KEY+3,X'06'         PRODUCT HEADER                               
         XC    KEY+10(15),KEY+10   CLEAR AFTER PRD CODE                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     PRODUCT HEADER ?                             
         BNE   DEL10               NO - PRODUCT NOT FOUND                       
*                                  PRODUCT HEADER FOUND                         
         CLI   MYKEY+3,X'20'       TESTING BUY ?                                
         BNE   PROC30              NO                                           
         AP    BUYSIN,=P'1'                                                     
         MVC   KEY+10(2),MYKEY+19  EST NUMBER FROM BUY                          
*NOP*    MVI   KEY+3,X'07'         EST HEADER                                   
         MVI   KEY+3,X'09'         EST BUCKET                                   
         XC    KEY+12(13),KEY+12   CLEAR AFTER EST NUMBER                       
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     ESTIMATE HEADER ?                            
         BNE   DEL10               NO - ESTIMATE NOT FOUND                      
*                                  ESTIMATE HEADER FOUND                        
PROC30   DS    0H                                                               
         MVC   KEY(32),MYKEY       RESTORE KEY                                  
         GOTO1 HIGH                AND POSITION                                 
*                                                                               
         CLC   KEY(25),KEYSAVE     SAME RECORD ?                                
         BE    PROC3               YES - GO GET NEXT RECORD                     
*                                                                               
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
DEL10    DS    0H                  DELETE RECORDS                               
*                                                                               
         MVC   KEY(32),MYKEY       RESTORE KEY                                  
         GOTO1 HIGH                AND POSITION                                 
*                                                                               
         CLC   KEY(25),KEYSAVE     SAME RECORD ?                                
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
         CLI   KEY+3,X'07'         ESTIMATE?                                    
         BNE   DEL14               NO                                           
         AP    ESTCNT,=P'1'                                                     
         B     DEL20               DELETE IT                                    
*                                                                               
DEL14    CLI   KEY+3,X'09'         EST BUCKET?                                  
         BNE   DEL16               NO                                           
         AP    ESTBKCNT,=P'1'                                                   
         B     DEL20               DELETE IT                                    
*                                                                               
DEL16    CLI   KEY+3,X'20'         BUY (INSERTION)?                             
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
         AP    BUYCNT,=P'1'                                                     
*                                                                               
DEL20    DS    0H                  DELETE RECORD                                
         OI    KEY+25,X'80'                                                     
*                                                                               
         CLI   QOPT6,C'N'          MEANS DON'T MARK FILE                        
         BE    DEL25                                                            
*                                                                               
         AP    CLOSECNT,=P'1'                                                   
         GOTO1 DATAMGR,DMCB,DMWRT,PRTDIR,KEY,KEY                                
*                                                                               
DEL25    DS    0H                                                               
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         B     PROC3               GO GET NEXT RECORD                           
*                                                                               
*                                                                               
************************************* END OF NEW CODE                           
*                                                                               
*                                                                               
PROC80   DS    0H                                                               
         CLI   KEY,X'FF'          END OF FILE                                   
         BE    EXIT                                                             
         XC    LASTPUB,LASTPUB                                                  
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+3,X'FF'             SKIP TO NEXT AGY/MED                     
         XC    KEY+4(28),KEY+4                                                  
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'               END OF FILE                              
         BE    EXIT                                                             
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'21'                                                      
         B     PROC2                                                            
*                                                                               
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
TOTCNT   DS    PL8                                                              
         DC    CL15'RECORDS READ   '                                            
H0CNT    DS    PL8                                                              
         DC    CL15'H0 RECORDS READ'                                            
ESTCNT   DS    PL8                                                              
         DC    CL15'ESTIMATES  CLSD'                                            
ESTBKCNT DS    PL8                                                              
         DC    CL15'EST BUCKTS CLSD'                                            
BUYCNT   DS    PL8                                                              
         DC    CL15'BUYS       CLSD'                                            
BUYSIN   DS    PL8                                                              
         DC    CL15'BUYS       READ'                                            
CLOSECNT DS    PL8                                                              
         DC    CL15'TOTAL RECS CLSD'                                            
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
         MVC   HEAD7+56(22),=C'DELETED AGENCY H0 RECS'                          
         MVC   HEAD8+56(22),=C'----------------------'                          
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
         LA    R2,26                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(26),0(R5)                                                   
         TR    WORK(26),TRTAB                                                   
         MVC   P+75(26),WORK                                                    
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
PP03WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
LASTCLT  DS    CL3                                                              
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
**PAN#1  DC    CL21'008PPREP0302205/01/02'                                      
         END                                                                    
