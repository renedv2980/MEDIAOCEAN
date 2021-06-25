*          DATA SET PPREP0202D AT LEVEL 141 AS OF 05/01/02                      
*PHASE PP0202D,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM WILL SEARCH FOR ZZZ AND BRAND MISMATCHES                  
*        OF TEST STATUS                                                         
*                                                                               
*        QOPT1     Y= LIVE RUN (MARK FILE)                                      
*                                                                               
         PRINT NOGEN                                                            
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
*                                                                               
RUNF     DS    0H                                                               
         ZAP   ZZZTST,=P'0'  TEST ZZZ EST FOR LIVE BRAND                        
         ZAP   PRDTST,=P'0'  TEST PRODUCT FOR LIVE ZZZ                          
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLC   QCLIENT,=C'   '                                                  
         BNE   *+10                                                             
         MVC   QCLIENT,=C'ALL'                                                  
         CLC   QPRODUCT,=C'   '                                                 
         BNE   *+10                                                             
         MVC   QPRODUCT,=C'ALL'                                                 
*                                                                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),=C'AAM'       DEFAULT TO FIRST                            
         CLI   QAGENCY,C'P'         SEE IF DOING A PRINTFILE                    
         BE    *+10                                                             
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'07'         ESTIMATES                                    
         CLC   QPRODUCT(3),=C'ALL'                                              
         BE    *+10                                                             
         MVC   KEY+7(3),QPRODUCT   ONE PRODUCT                                  
         CLC   QCLIENT(3),=C'ALL'                                               
         BE    AGYC2                                                            
         MVC   KEY+4(3),QCLIENT    ONE CLIENT                                   
*                                                                               
AGYC2    GOTO1 HIGH                                                             
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
AGYC4    DS    0H                                                               
         CLI   KEY+3,X'07'                                                      
         BNE   AGYC35                                                           
         CLC   KEY(3),QAGENCY     ONLY MATCH AGENCY/MEDIA                       
         BNE   AGYC35                                                           
         CLC   QCLIENT(3),=C'ALL'                                               
         BE    AGYC6                                                            
         CLC   KEY+4(3),QCLIENT   MATCH AGENCY/MEDIA/CLIENT                     
         BNE   USERP              DONE                                          
         CLC   QPRODUCT(3),=C'ALL'                                              
         BE    AGYC6                                                            
         CLC   KEY+7(3),QPRODUCT  MATCH AGENCY/MEDIA/CLIENT/PRODUCT             
         BNE   USERP              DONE                                          
*                                                                               
AGYC6    DS    0H                                                               
         CLC   KEY+7(3),=C'ZZZ'  SKIP ZZZ ESTIMATES                             
         BE    AGYC3                                                            
         GOTO1 GETEST                                                           
         MVC   PRDSTAT,PESTTEST    SAVE STATUS OF PRD EST                       
*                                                                               
*        READ ZZZ EST AND GET STATUS                                            
*                                                                               
         MVC   SAVKEY,KEY                                                       
         MVC   KEY+7(3),=C'ZZZ'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   AGYC20             NO ZZZ ESTIMATE SKIP TO NEXT EST              
         GOTO1 GETEST                                                           
         TM    PESTTEST,X'80'     IS ZZZ TEST                                   
         BO    AGYC9              YES - SEE IF PRODUCT WAS                      
*                                                                               
         TM    PRDSTAT,X'80'      SEE IF PRODUCT IS TEST                        
         BNO   AGYC20             MATCH  - SKIP TO NEXT EST                     
         AP    PRDTST,=P'1'       TEST PRD FOR LIVE ZZZ                         
         MVC   P+4(35),=C'** TEST PRD EST FOR LIVE ZZZ EST **'                  
         MVC   P+50(4),=C'PRD='                                                 
         MVC   P+55(3),SAVKEY+7                                                 
         MVC   P+60(4),=C'EST='                                                 
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+66(3),DUB                                                      
         BAS   RE,RPRT                                                          
*                                                                               
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+4,25,=C'N'                                     
         GOTO1 HEXOUT,DMCB,KEY+27,P+100,4,=C'N'                                 
         MVC   PSECOND+6(25),KEY                                                
*                                                                               
         BAS   RE,RPRT                                                          
         B     AGYC20           GO TO NEXT EST                                  
*                                                                               
AGYC9    DS    0H                                                               
         TM    PRDSTAT,X'80'                                                    
         BO    AGYC20           BOTH TEST - SKIP TO NEXT                        
*                                                                               
         AP    ZZZTST,=P'1'       LIVE PRD FOR TEST ZZZ                         
         MVC   P+4(35),=C'** LIVE PRD EST FOR TEST ZZZ EST **'                  
*                                                                               
         MVC   P+50(4),=C'PRD='                                                 
         MVC   P+55(3),SAVKEY+7                                                 
         MVC   P+60(4),=C'EST='                                                 
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+66(3),DUB                                                      
         BAS   RE,RPRT                                                          
*                                                                               
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+4,25,=C'N'                                     
         GOTO1 HEXOUT,DMCB,KEY+27,P+100,4,=C'N'                                 
         MVC   PSECOND+6(25),KEY                                                
*                                                                               
         BAS   RE,RPRT                                                          
         B     AGYC20           GO TO NEXT EST                                  
*                                                                               
AGYC20   DS    0H                                                               
         MVC   KEY,SAVKEY                                                       
         GOTO1 HIGH                                                             
         B     AGYC3                                                            
*                                                                               
AGYC35   DS    0H                                                               
         CLI   KEY,X'FF'              END OF FILE                               
         BE    USERP                                                            
         CLI   QAGENCY,C'P'           SEE IF DOING ALL AGY/MEDS ON A            
         BNE   USERP                  PRINTFILE                                 
         XC    KEY+4(20),KEY+4                                                  
         MVI   KEY+3,X'FF'             SKIP TO NEXT AGY/MED                     
         GOTO1 HIGH                                                             
         XC    KEY+4(20),KEY+4                                                  
         MVI   KEY+3,X'07'             READ ESTS FOR NEXT AGY/MED               
         B     AGYC2                                                            
*********************************                                               
********** END OF MY CODE                                                       
*********************************                                               
*                                                                               
USERP    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
ZZZTST   DS    PL8                                                              
         DC    CL15'LIVE BRAND ESTS'                                            
PRDTST   DS    PL8                                                              
         DC    CL15'LIVE ZZZ ESTS'                                              
         DC    X'FF'                                                            
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
PRDSTAT  DS    CL1           PRODUCT ESTIMATE TEST STATUS                       
KEYPSW   DS    CL1                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
************************************* OLD CODE                                  
************************ END OF PROGRAM                                         
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
         LA    R2,220                                                           
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
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'141PPREP0202D05/01/02'                                      
         END                                                                    
