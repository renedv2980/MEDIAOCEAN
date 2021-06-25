*          DATA SET PPREP0302L AT LEVEL 002 AS OF 05/01/02                      
*PHASE PP0302L,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM MARKS SELECTED X'25' CLEARANCE STATUS RECORDS                  
*   AND SELECTED X'08' BILLING RECORDS                                          
*   FOR DELETION                                                                
*                                                                               
*      QOPT5     Y= TEST RUN (DON'T MARK FILE)                                  
*      QOPT6     Y= DUMP FIRST 10 RECORDS (BEFORE AND AFTER)                    
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
         ZAP   INCLR,=P'0'         RECS READ                                    
         ZAP   OUTCLR,=P'0'        RECS DELETED                                 
         ZAP   INBIL,=P'0'         RECS READ                                    
         ZAP   OUTBIL,=P'0'        RECS DELETED                                 
         ZAP   DUMPCNT,=P'0'       RECS DUMPED                                  
*                                                                               
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                  FIRST BUY FOR REQUEST                        
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
*****    OI    DMINBTS,X'08'       PASS DELETES                                 
*                                                                               
         LA    R0,PBUYREC          READ INTO BUYREC                             
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),=C'DFS'       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'25'          CLEARANCE STATUS RECORD                     
*                                                                               
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
*                                                                               
PROC3    GOTO1 SEQ                                                              
*                                                                               
PROC5    CLC   KEY(4),KEYSAVE      SAME AGENCY/MEDIA/CODE ?                     
         BE    PROC10              YES - PROCESS                                
         CLI   KEYSAVE+3,X'25'     NO - WAS CLR STAT REC LAST ?                 
         BNE   PROC5X              NO - END                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),=C'DFS'      START AT AGENCY/MEDIA                        
         MVI   KEY+3,X'08'         BILLING RECORD                               
         MVI   FORCEHED,C'Y'                                                    
         B     PROC2               START OVER WITH NEW REC TYPE                 
*                                                                               
PROC5X   MVI   KEY,X'FF'           NO - END OF RUN                              
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
PROC10   DS    0H                                                               
         CLI   KEY+3,X'08'         BILLING RECORD ?                             
         BNE   PROC10D             NO                                           
         OC    KEY+10(2),KEY+10    ESTIMATE NULL ?                              
         BNZ   PROC3               NO - NEXT BILLING REC                        
         AP    INBIL,=P'1'         BILLING RECS READ                            
         B     PROC10G             YES                                          
PROC10D  AP    INCLR,=P'1'         CLEARANCE STATUS RECS READ                   
*                                                                               
PROC10G  GOTO1 GETPRT              GET THE RECORD                               
*                                                                               
         LA    R5,PBUYREC                                                       
         USING PBUYREC,R5                                                       
*                                                                               
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   PROC35D             NO                                           
         AP    DUMPCNT,=P'1'                                                    
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    PROC35D             YES                                          
*                                                                               
PROC35   DS    0H                                                               
         MVC   P+55(12),=C'** BEFORE **'                                        
*****    BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC35D  DS    0H                                                               
         OI    PBUYREC+27,X'FF'      MARK FOR DELETION                          
         OI    KEY+25,X'FF'          MARK FOR DELETION                          
         CLI   KEY+3,X'08'         BILLING RECORD ?                             
         BNE   PROC35F             NO                                           
         AP    OUTBIL,=P'1'        YES                                          
         B     PROC35J             NO                                           
PROC35F  AP    OUTCLR,=P'1'                                                     
*                                                                               
PROC35J  CLI   QOPT5,C'Y'          WRITE RECORD ?                               
         BE    PROC40              NO                                           
*                                                                               
         GOTO1 PUTPRT                                                           
         GOTO1 WRT                                                              
*                                                                               
PROC40   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC50              NO                                           
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    PROC50              YES                                          
*                                                                               
PROC42   MVC   P+55(12),=C'** AFTER ***'                                        
*****    BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC50   DS    0H                                                               
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*****    MVC   KEY(64),SKEY        RESTORE BUY KEYS                             
*****    GOTO1 HIGH                AND SEQUENCE                                 
*                                                                               
         B     PROC3               NEXT SEQ REC                                 
*                                                                               
         DROP  R5                                                               
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
*                                                                               
RUNL10   DS    0H                                                               
*                                                                               
RUNL40   GOTO1 REPORT                                                           
         LA    R4,COUNTS                                                        
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNL90                                                           
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL50                                                           
*                                                                               
RUNL90   DS    0H                                                               
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
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
         LA    R2,27                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(27),0(R5)                                                   
         TR    WORK(27),TRTAB                                                   
         MVC   P+75(27),WORK                                                    
         B     EXIT                                                             
*                                                                               
DMPRECA  NTR1                                                                   
         LA    R5,REC-4                                                         
         MVC   HALF,29(R5)        PCONREC RECORD LENGTH                         
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
*                                                                               
OUT      DCB   DDNAME=TAPEOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
*                                                                               
         SPACE 2                                                                
*                                                                               
COUNTS   DS    0C                                                               
INCLR    DS    PL8                                                              
         DC    CL15'CLR RECS READ'                                              
OUTCLR   DS    PL8                                                              
         DC    CL15'CLR RECS DELETD'                                            
INBIL    DS    PL8                                                              
         DC    CL15'BIL RECS READ'                                              
OUTBIL   DS    PL8                                                              
         DC    CL15'BIL RECS DELETD'                                            
         DC    X'FF'                                                            
DUMPCNT  DS    PL8                                                              
*                                                                               
PRTFILES DS    0D                                                               
         DC    CL8' PRTDIR'                                                     
         DC    CL8' PRTFILE'                                                    
         DC    CL8' PUBDIR'                                                     
         DC    CL8' PUBFILE'                                                    
         DC    C'X'                                                             
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
PUBSW    DS    CL1                                                              
COPYSW   DS    CL1                                                              
*                                                                               
PUBNET   DS    PL8                                                              
PUBCASH  DS    PL8                                                              
PUBMYTAX DS    PL8                                                              
PUBBASIS DS    PL8                                                              
PUBGSTT  DS    PL8                                                              
PUBGSTTP DS    PL8                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
*                                                                               
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
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
PPBYOWRK DS    600C                                                             
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREP0302L05/01/02'                                      
         END                                                                    
