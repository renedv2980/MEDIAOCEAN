*          DATA SET PPREP0302  AT LEVEL 003 AS OF 05/01/02                      
*PHASE PP0202P,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM WILL READ PUBS FOR A KILL DATE OF 12/31/96.               
*        IF FOUND THE PUBREC AND LTLREC (IF FOUND) WILL BE COPIED               
*        TO MEDIA CODE 'T' AND ADDED TO THE PUBFIL. IF ANY PUBREPELS            
*        (EL CODE X'14') ARE FOUND, THE THREE REP AREAS WILL BE                 
*        OPTIONALLY (QOPT1=Y) CLEARED TO HEX ZERO BEFORE ADDING.                
*                                                                               
*      QOPT1     Y= CLEAR (SET TO HEX 0'S) REPS IN PUBREPEL (IF FOUND)          
*      QOPT5     Y= TEST RUN (DON'T MARK FILE)                                  
*      QOPT6     Y= DUMP FIRST 25 RECORDS (BEFORE AND AFTER)                    
*                                                                               
PP0202   CSECT                                                                  
         PRINT NOGEN                                                            
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
         CLI   MODE,PROCPUB                                                     
         BE    PROC                                                             
         CLI   MODE,FPUBREQ                                                     
         BE    RUNF                                                             
         CLI   MODE,LPUBREQ                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   CPYCNT,=P'0'                                                     
         ZAP   ELEMCNT,=P'0'                                                    
         ZAP   LTLCNT,=P'0'                                                     
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         XC    LASTPUB,LASTPUB                                                  
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PROC5    DS    0H                                                               
         BAS   RE,NXTPUB           GET BOTH PUB RECS                            
         CLI   PUBKMED,X'FF'       EOF ?                                        
         BE    EXIT                YES                                          
         AP    INCNT,=P'1'                                                      
*                                                                               
         CLC   PUBKILL,=X'600C1F'    KILL DATE 12/31/96 ?                       
         BNE   PROC5               NO - GET NEXT PUB                            
****                               YES - PUB WILL BE COPIED                     
         MVC   KEY(25),PUBREC      REPLACE LITTLE REC KEY (FROM NXTPUB)         
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC7               NO                                           
         CP    CPYCNT,=P'24'       25 RECORDS DUMPED ?                          
         BH    PROC7               YES                                          
*                                                                               
         BAS   RE,RPRT                                                          
         MVC   P+55(12),=C'** BEFORE **'                                        
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC7    DS    0H                                                               
         MVC   P(5),=C'PUB# '                                                   
         GOTO1 PUBEDIT,DMCB,PUBREC+1,(0,P+5)                                    
         LA    R2,PUBREC+33       LOOK FOR PUB REP ELEMENTS                     
         MVI   ELCODE,X'14'                                                     
PROC10   BAS   RE,NEXTEL                                                        
         BNE   COPYPUB                                                          
*                                 REP ELEM FOUND                                
         USING PUBREPEL,R2                                                      
         MVC   P+30(8),=C'PAY REP='                                             
         MVC   P+38(4),PUBPAREP                                                 
         MVC   P+45(8),=C'TRF REP='                                             
         MVC   P+53(4),PUBTRREP                                                 
         MVC   P+60(8),=C'CON REP='                                             
         MVC   P+68(4),PUBCNREP                                                 
         BAS   RE,RPRT                                                          
         CLI   QOPT1,C'Y'          CLEAR REP CODES ?                            
         BNE   PROC10A             NO                                           
         XC    PUBPAREP,PUBPAREP                                                
         XC    PUBTRREP,PUBTRREP                                                
         XC    PUBCNREP,PUBCNREP                                                
PROC10A  AP    ELEMCNT,=P'1'                                                    
         B     PROC10              TEST FOR MORE REP ELEMS                      
         DROP  R2                                                               
*                                                                               
COPYPUB  DS    0H                                                               
         CLC   P(5),=C'PUB# '      PUB LINE TO BE PRINTED ?                     
         BNE   CLEARREC            NO                                           
         BAS   RE,RPRT                                                          
*                                                                               
CLEARREC SR    R5,R5             CLEAR END OF RECORD                            
         IC    R5,PUBREC+25                                                     
         SLL   R5,8                                                             
         IC    R5,PUBREC+26                                                     
         SR    RE,RE                                                            
         LA    RE,PUBREC                                                        
         AR    RE,R5                                                            
         SR    RF,RF                                                            
         LA    RF,PUBREC                                                        
         LA    RF,2000(RF)                                                      
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         MVI   PUBKMED,C'T'         CHANGE THE MEDIA CODE                       
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC12              NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PUBREC                                                   
         GOTO1 ADDPUB                                                           
*                                                                               
PROC12   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC20              NO                                           
         CP    CPYCNT,=P'24'       25 RECORDS DUMPED ?                          
         BH    PROC20              YES                                          
*                                                                               
         MVC   P+55(12),=C'** AFTER ***'                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC20   DS    0H                                                               
         AP    CPYCNT,=P'1'                                                     
*****                        NOW ADD LITTLE REC IF IT EXISTS                    
         L     RF,ALTLREC                                                       
         CLI   0(RF),0             LITTLE REC THERE ?                           
         BE    PROC5               NO - GO TEST NEXT RECORD                     
         AP    LTLCNT,=P'1'                                                     
*                                                                               
         ST    RF,AREC                                                          
*                                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC25              NO                                           
         CP    CPYCNT,=P'25'       25 RECORDS DUMPED ?                          
         BH    PROC25              YES                                          
*                                                                               
         MVC   P+5(19),=C'** LITTLE BEFORE **'                                  
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC25   DS    0H                                                               
         L     RF,ALTLREC                                                       
         MVI   0(RF),C'T'           CHANGE THE MEDIA CODE                       
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC30              NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),0(RF)                                                    
         GOTO1 ADDPUB                                                           
*                                                                               
PROC30   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC5               NO - GO TEST NEXT RECORD                     
         CP    CPYCNT,=P'25'       25 RECORDS DUMPED ?                          
         BH    PROC5               YES                                          
*                                                                               
         MVC   P+5(19),=C'** LITTLE AFTER  **'                                  
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
         B     PROC5               GO TEST NEXT RECORD                          
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    RUNL10                                                           
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
RUNL10   DS    0H                                                               
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
*****    MVI   RCSUBPRG,10                                                      
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
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'RECORDS READ'                                               
CPYCNT   DS    PL8                                                              
         DC    CL15'PUBS COPIED'                                                
ELEMCNT  DS    PL8                                                              
         DC    CL15'REPS FOUND'                                                 
LTLCNT   DS    PL8                                                              
         DC    CL15'LTLRECS COPIED'                                             
         DC    X'FF'                                                            
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
PUBSW    DS    CL1                                                              
CHKGST   DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
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
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREP0302 05/01/02'                                      
         END                                                                    
