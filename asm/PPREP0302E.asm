*          DATA SET PPREP0302E AT LEVEL 003 AS OF 05/01/02                      
*PHASE PP0302E,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*      THIS PROGRAM CHECKS FOR PAY ADDRESS ELEMS (X'08') WITH                   
*      EMPTY NAME FIELDS                                                        
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
         CLI   MODE,PROCPUB                                                     
         BE    PROC                                                             
         CLI   MODE,FPUBREQ                                                     
         BE    RUNF                                                             
         CLI   MODE,LPUBREQ                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNF5    CLI   0(R4),X'FF'                                                      
         BE    RUNFX                                                            
         ZAP   0(8,R4),=P'0'                                                    
         LA    R4,23(R4)                                                        
         B     RUNF5                                                            
RUNFX    B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         GOTO1 HIGHPUB                                                          
         CLI   KEY+9,X'81'         PUB REC ?                                    
         BNE   PROC5               NO - BYPASS                                  
         TM    KEY+25,X'01'        PASSIVE ?                                    
         BO    PROC5               YES - BYPASS                                 
         GOTO1 GETNAME                                                          
         B     PROC5A                                                           
*                                                                               
PROC5    DS    0H                                                               
         CLI   KEY,X'FF'           EOF ?                                        
         BE    EXIT                YES                                          
         BAS   RE,NXTPUB           GET PUB REC                                  
*                                                                               
PROC5A   MVI   DUMPSW,0                                                         
         MVI   ADDSW,0                                                          
         MVI   DELSW,0                                                          
         CLI   PUBKMED,X'FF'       EOF ?                                        
         BNE   PROC5A2             NO                                           
         MVI   KEY,X'FF'                                                        
         MVI   PAGYKMED,X'FF'      TO FORCE PPG TO END                          
         B     EXIT                                                             
PROC5A2  AP    INCNT,=P'1'                                                      
*                             *******************************                   
*                             ******  LOOK FOR PAY ADDRESS                      
         LA    R2,PUBREC+33   *******************************                   
         MVI   ELCODE,X'08'                                                     
PROC5B   BAS   RE,NEXTEL           LOOK FOR PAY ADDRESS ELEM                    
         BNE   PROC5               NOT FOUND                                    
         AP    PAYCNT,=P'1'                                                     
         CLC   5(10,R2),=CL10' '                                                
         BH    PROC5B                                                           
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         BAS   RE,DMPREC                                                        
         AP    BPAYCNT,=P'1'                                                    
         B     PROC5                                                            
         EJECT                                                                  
*                                                                               
CLEARREC NTR1                      *****  CLEAR END OF RECORD                   
         SR    R5,R5                                                            
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
         B     EXIT                                                             
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
         SPACE 3                                                                
*                             **************************                        
***                           COUNT PUB RECS BY SIZE                            
*                                  R4 = COUNTERS ADDRESS                        
*                                  R5 = PUB REC LENGTH                          
*                             **************************                        
SIZEPUBS NTR1                                                                   
         LA    R6,501                                                           
SIZETST  CR    R5,R6               PUB LENGTH LESS ?                            
         BL    SIZEADD             YES                                          
         LA    R4,23(R4)           BUMP TO NEXT COUNTER                         
         LA    R6,500(R6)          NEXT TEST SIZE                               
         CH    R6,=H'4002'         MAXIMUM PUB LENGTH ?                         
         BL    SIZETST             NO                                           
SIZEADD  AP    0(8,R4),=P'1'                                                    
SIZEPUBX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                             READ FOR ALL PUB (X'81') RECORDS                  
NXTPUB   NTR1                                                                   
*                                                                               
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
         DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLI   KEY,C'T'            PAST HIGH MEDIA ?                            
         BNH   NP4                 NO - CONTINUE                                
         MVI   KEY,X'FF'           YES - EXIT WITH EOF SET                      
         MVI   PAGYKMED,X'FF'      TO FORCE PPG TO END                          
         B     EXIT                                                             
NP4      CLI   KEY+9,X'81'         PUB REC ?                                    
         BNE   NP2                 NO - BYPASS                                  
         CLI   KEY+25,X'01'        PASSIVE ?                                    
         BE    NP2                 YES - BYPASS                                 
         GOTO1 GETNAME                                                          
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
PAYCNT   DS    PL8                                                              
         DC    CL15'PAY ADDR      '                                             
BPAYCNT  DS    PL8                                                              
         DC    CL15'NO NAMES      '                                             
         DC    X'FF'                                                            
*                                                                               
SAVKEY   DS    CL32                                                             
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ADDSW    DS    CL1                                                              
DELSW    DS    CL1                                                              
DUMPADD  DS    PL5                                                              
DUMPDEL  DS    PL5                                                              
DUMPSW   DS    CL1                                                              
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
OUTSPC   DS    XL200                                                            
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
**PAN#1  DC    CL21'003PPREP0302E05/01/02'                                      
         END                                                                    
