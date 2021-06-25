*          DATA SET PPREP0302X AT LEVEL 002 AS OF 05/01/02                      
*PHASE PP0302X,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM WILL FLAG PUB ADDRESS RECORDS FOR DELETION                
*                                                                               
*      QOPT1     A= DELETE ALL ADDRESSES FOR PUB                                
*      QOPT1     C= DELETE CONTRACT ADDRESSES                                   
*      QOPT1     P= DELETE PAYING   ADDRESSES                                   
*      QOPT1     S= DELETE SHIPPING ADDRESSES                                   
*      QOPT1     T= DELETE TRAFFIC  ADDRESSES                                   
*                                                                               
*      QOPT6     Y= TEST RUN (DON'T MARK FILE)                                  
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
         ZAP   DELCNT,=P'0'                                                     
         ZAP   ADRCNT,=P'0'                                                     
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT6,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(25),PUBREC      SET KEY FROM PPG READ OF PUB                 
         GOTO1 HIGHPUB             REDO READ                                    
*                                                                               
PROC2    DS    0H                                                               
         GOTO1 SEQPUB              NEXT PUB RECORD                              
         AP    INCNT,=P'1'                                                      
PROC4    DS    0H                                                               
         CLC   KEY(09),KEYSAVE     SAME THRU AGENCY ?                           
         BE    PROC4D              YES                                          
*****    MVI   KEY,X'FF'           NO - FINISHED                                
*****    GOTO1 HIGHPUB                                                          
*****    MVI   MODE,RUNLAST                                                     
         B     EXIT                                                             
*                                                                               
PROC4D   CLI   KEY+9,X'82'         PUB ADDRESS RECORD ?                         
         BNE   PROC2               NO - READ NEXT                               
         AP    ADRCNT,=P'1'                                                     
         CLI   QOPT1,C'A'          DELETE ALL ADDRESSES ?                       
         BE    PROC6               YES                                          
         CLI   QOPT1,C'C'          DELETE CONTRACT ADDRESSES ?                  
         BNE   PROC4F              NO                                           
         CLI   KEY+10,X'0A'        CONTRACT ADDRESS RECORD ?                    
         BE    PROC6               YES - DELETE                                 
PROC4F   CLI   QOPT1,C'S'          DELETE SHIPPING ADDRESSES ?                  
         BNE   PROC4H              NO                                           
         CLI   KEY+10,X'0B'        SHIPPING ADDRESS RECORD ?                    
         BE    PROC6               YES - DELETE                                 
PROC4H   CLI   QOPT1,C'P'          DELETE PAYING   ADDRESSES ?                  
         BNE   PROC4K              NO                                           
         CLI   KEY+10,X'08'        PAYING   ADDRESS RECORD ?                    
         BE    PROC6               YES - DELETE                                 
PROC4K   CLI   QOPT1,C'T'          DELETE TRAFFIC  ADDRESSES ?                  
         BNE   PROC2               NO - READ NEXT                               
         CLI   KEY+10,X'09'        TRAFFIC  ADDRESS RECORD ?                    
         BNE   PROC2               NO - READ NEXT                               
*                                                                               
PROC6    DS    0H                  FLAG PUB ADDRESS KEY FOR DELETION            
*****    LA    R0,PUBREC                                                        
*****    ST    R0,AREC                                                          
*****    CLI   QOPT7,C'Y'          DUMP RECORDS ?                               
*****    BNE   PROC7               NO                                           
*****    CP    DELCNT,=P'10'       10 RECORDS DUMPED ?                          
*****    BH    PROC7               YES                                          
*                                                                               
         BAS   RE,RPRT                                                          
         MVC   P+65(12),=C'** BEFORE **'                                        
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*****    BAS   RE,DMPREC                                                        
*                                                                               
PROC7    DS    0H                                                               
*****    MVC   P(5),=C'PUB# '                                                   
*****    GOTO1 PUBEDIT,DMCB,KEY+1,(0,P+5)                                       
*****    BAS   RE,RPRT                                                          
*                                                                               
         OI    KEY+25,X'80'        FLAG FOR DELETION                            
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC12              NO                                           
*                                                                               
         GOTO1 WRTPUB                                                           
*                                                                               
PROC12   DS    0H                                                               
*****    CLI   QOPT7,C'Y'          DUMP RECORDS ?                               
*****    BNE   PROC20              NO                                           
*****    CP    DELCNT,=P'24'       25 RECORDS DUMPED ?                          
*****    BH    PROC20              YES                                          
*                                                                               
         MVC   P+65(12),=C'** AFTER ***'                                        
*****    BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
PROC20   DS    0H                                                               
         AP    DELCNT,=P'1'                                                     
*                                                                               
         B     PROC2               GO TEST NEXT RECORD                          
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
         LA    R2,31                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(31),0(R5)                                                   
         TR    WORK(31),TRTAB                                                   
         MVC   P+80(31),WORK                                                    
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
ADRCNT   DS    PL8                                                              
         DC    CL15'ADDRESSES READ'                                             
DELCNT   DS    PL8                                                              
         DC    CL15'RECORDS DELETED'                                            
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
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREP0302X05/01/02'                                      
         END                                                                    
