*          DATA SET PPREP0302W AT LEVEL 002 AS OF 05/01/02                      
*PHASE PP0302W,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE SORTER                                                                 
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM LISTS UNIQUE PARAGRAPH NAMES FROM SRDS DIGITAL RECORDS         
*                                                                               
*                                                                               
PP0302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0302                                                         
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
         ZAP   INDIGI,=P'0'        DIGITAL RECORDS READ                         
         ZAP   INSORT,=P'0'        PARAGRAPH NAMES PUT TO SORTER                
         ZAP   OUTSORT,=P'0'       PARAGRAPH NAMES READ FROM SORTER             
         ZAP   UNQCNT,=P'0'        UNIQUE PARAGRAPH NAMES                       
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                  FIRST FOR REQUEST                            
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                            INITIALIZE SORTER                                  
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
         OPEN  (XDIGI,INPUT)                                                    
*                                                                               
PROC10   DS    0H                                                               
         GET   XDIGI,DIGIREC                                                    
         AP    INDIGI,=P'1'                                                     
*                                                                               
*                  BUILD FILE OF PARAGRAPH-NAMES                                
*                                                                               
         OC    DIGIREC(130),SPACES                                              
         CLI   DIGIREC+9,C' '                                                   
         BE    PROC10                                                           
         MVC   THISREC(70),DIGIREC+9       TRUNCATED PARA-NAME                  
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',THISREC                                  
         AP    INSORT,=P'1'                                                     
*                                                                               
         B     PROC10              NEXT RECORD                                  
*                                                                               
PROC99X  MVI   MODE,RUNLAST                                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
RUNL     DS    0H                                                               
         CLOSE XDIGI                                                            
         XC    P,P                                                              
         ZAP   DUPCNT,=P'1'                                                     
         ZAP   OUTSORT,=P'1'                                                    
*                                                                               
*================================================================*              
* GET RECORDS FROM SORT AND LIST AND COUNT "UNIQUE" NAMES        *              
*================================================================*              
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'   GET FIRST RECORD                       
         ICM   R6,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   NEWREC,0(R6)                                                     
         MVC   THISREC,NEWREC                                                   
*                                                                               
RUNL10   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R6,15,4(R1)                                                      
         BNZ   RUNL20                                                           
         MVI   NEWREC,X'FF'                                                     
         B     RUNL30                                                           
*                                                                               
RUNL20   MVC   NEWREC,0(R6)        MOVE RECORD SO CAN SEE IT                    
         AP    OUTSORT,=P'1'                                                    
*                                                                               
         CLC   THISREC,NEWREC      TEST SAME                                    
         BNE   RUNL30              GO PRINT DESCRIPTION                         
         AP    DUPCNT,=P'1'                                                     
         B     RUNL10              NEXT SORTED DESCRIPTION                      
*                                                                               
RUNL30   DS    0H                                                               
         AP    UNQCNT,=P'1'                                                     
         MVC   P+01(70),THISREC                                                 
         EDIT  DUPCNT,(5,P+75)                                                  
         BAS   RE,RPRT                                                          
         MVC   THISREC,NEWREC                                                   
         CLI   THISREC,X'FF'       MORE RECORDS ?                               
         BE    RUNL40              NO                                           
         ZAP   DUPCNT,=P'1'                                                     
         B     RUNL10              NEXT SORTED DESCRIPTION                      
*                                                                               
RUNL40   DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
*                                                                               
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNL100                                                          
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL50                                                           
*                                                                               
RUNL100  DS    0H                                                               
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVI   RCSUBPRG,0                                                       
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
*****    L     RF,ALTLREC                                                       
*****    XC    0(50,RF),0(RF)                                                   
*****    GOTO1 SEQPUB                                                           
*****    CLC   KEY(9),PUBKEY                                                    
*****    BNE   NPX                                                              
*****    GOTO1 GETLTL                                                           
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
         EJECT                                                                  
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
*                                                                               
DMPRECA  NTR1                                                                   
*NOP*    LA    R5,REC-4                                                         
         MVC   HALF,29(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R2,4(R2)           REC-4   RECORD LENGTH                         
         LA    R3,0(R5,R2)                                                      
         B     DMPREC2                                                          
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
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
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
XDIGI    DCB   DDNAME=XDIGI,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00780,                                            X        
               BLKSIZE=07800,                                          X        
               EODAD=PROC99X,                                          X        
               MACRF=GM                                                         
*                                                                               
         SPACE 2                                                                
*                                                                               
COUNTS   DS    0C                                                               
INDIGI   DS    PL8                                                              
         DC    CL15'RECORDS READ'                                               
INSORT   DS    PL8                                                              
         DC    CL15'RECS TO SORTER'                                             
OUTSORT  DS    PL8                                                              
         DC    CL15'RECS FROM SORT'                                             
UNQCNT   DS    PL8                                                              
         DC    CL15'UNIQUE DSCRPTS'                                             
         DC    X'FF'                                                            
DUMPCNT  DS    PL8                                                              
DUPCNT   DS    PL8                                                              
*                                                                               
THISREC  DS    CL70                                                             
NEWREC   DS    CL70                                                             
*                                                                               
PRTFILES DS    0D                                                               
         DC    CL8' PRTDIR'                                                     
         DC    CL8' PRTFILE'                                                    
         DC    CL8' PUBDIR'                                                     
         DC    CL8' PUBFILE'                                                    
         DC    C'X'                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,70,A),FORMAT=BI,WORK=1'                      
RECCARD DC     CL80'RECORD TYPE=F,LENGTH=70'                                    
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
*                                                                               
SVAGYM   DS    CL3                 QAGENCY                                      
NEWAGYM  DS    CL3                 QPUB+1                                       
SVOPT1   DS    CL1                 QOPT1                                        
SVOPT2   DS    CL1                 QOPT2                                        
SVOPT6   DS    CL1                 QOPT6                                        
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
         DS    F                                                                
DIGIREC  DS    CL780                                                            
*                                                                               
*                                                                               
* PPNEWFILE                                                                     
* PPREPWORK                                                                     
* PPMODEQU                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREP0302W05/01/02'                                      
         END                                                                    
