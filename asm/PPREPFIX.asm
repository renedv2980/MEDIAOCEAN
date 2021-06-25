*          DATA SET PPREPFIX   AT LEVEL 162 AS OF 05/01/02                      
*PHASE PP0202R                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM CHANGES LENGTHS OF PREPREC ELEM                           
*        QOPT5     Y= TEST RUN (DON'T MARK FILE)                                
*        QOPT1     Y= DUMP RECORDS BEFORE AND AFTER CONVERSION                  
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
RUNF     ZAP   REPRECS,=P'0'                                                    
         ZAP   REP126,=P'0'                                                     
         ZAP   REP152,=P'0'                                                     
         ZAP   REP164,=P'0'                                                     
         ZAP   REP166,=P'0'                                                     
         ZAP   CHARECS,=P'0'                                                    
         ZAP   PUBLS,=P'0'                                                      
         B     EXIT                                                             
         EJECT                                                                  
PROC     MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGY MEDIA                           
         MVI   KEY+3,X'11'                                                      
*                                                                               
AGYC2    GOTO1 HIGH                                                             
         CLI   8(R1),0                                                          
         BE    AGYC4                                                            
         DC    H'0'                                                             
*                                                                               
AGYC3    GOTO1 SEQ                                                              
         CLI   8(R1),0                                                          
         BE    AGYC4                                                            
         DC    H'0'                                                             
*                                                                               
AGYC4    DS    0H                                                               
         CLI   KEY,X'FF'            END OF FILE                                 
         BE    EXIT                                                             
*                                                                               
         CLI   KEY+3,X'11'          SEE IF REP REC                              
         BH    NEXTAM                GO DO NEXT AGY/MEDIA                       
         BE    AGYC4X                                                           
         MVI   KEY+3,X'11'            TRY FOR REPS                              
         XC    KEY+4(21),KEY+4                                                  
         B     AGYC2                                                            
*                                                                               
AGYC4X   DS    0H                                                               
         AP    REPRECS,=P'1'                                                    
         MVI   RECCHANG,C'N'                                                    
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         LA    R4,PREPREC                                                       
         XC    PREPREC(250),PREPREC    BE SURE IT CLEAR                         
         ST    R4,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT1,C'Y'     SEE IF DUMPING BEFORE AND AFTER                   
         BNE   AGYCX                                                            
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,PREPLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',AREC,C'DUMP',(R3),=C'2D',    +        
               (C'P',PRINT)                                                     
*                                                                               
AGYCX    DS    0H                                                               
         LA    R2,PREPELEM         A(FIRST ELEMENT)                             
*                                                                               
LOOP     CLI   0(R2),0             END OF RECORD?                               
         BE    WRITEIT             YES                                          
         CLC   0(2,R2),=X'117E'    MAIN ELEM  (126 BYTES)                       
         BNE   LOOP2                                                            
         AP    REP126,=P'1'                                                     
         B     FIX                                                              
*                                                                               
LOOP2    DS    0H                                                               
         CLC   0(2,R2),=X'1198'    MAIN ELEM  (152 BYTES)                       
         BNE   LOOP4                                                            
         AP    REP152,=P'1'                                                     
         B     FIX                                                              
*                                                                               
LOOP4    DS    0H                                                               
         CLC   0(2,R2),=X'11A4'    MAIN ELEM  (164 BYTES)                       
         BNE   LOOP5                                                            
         AP    REP164,=P'1'                                                     
         B     FIX                 BAD REP ELEM                                 
*                                                                               
LOOP5    DS    0H                                                               
         AP    REP166,=P'1'                                                     
         CLC   0(2,R2),=X'11A6'    MAIN ELEM  (166 BYTES)                       
         BE    FIX                                                              
         DC    H'0'                BAD REP ELEM                                 
*                                                                               
FIX      DS    0H                                                               
         MVI   RECCHANG,C'Y'       RECORD HAS CHANGED                           
         MVI   PREPELEM+1,X'A6'    SET NEW ELEM LENGTH                          
         MVC   PREPLEN,=H'199'     (166 + 33)                                   
         BAS   RE,PUBLCHK          GO SEE IF USED AS A PUBLISHER                
         OC    PREPSTAT,DUB                                                     
*                                                                               
NEXTELEM ZIC   R0,1(R2)                                                         
         AR    R2,R0               BUMP TO NEXT ELEMENT                         
         B     LOOP                                                             
*                                                                               
WRITEIT  CLI   RECCHANG,C'Y'       DID RECORD CHANGE?                           
         BNE   AGYC3               NO                                           
*                                                                               
*                                                                               
         TM    PREPSTAT,X'01'     SEE IF PUBLISHER                              
         BNO   WRITE5                                                           
         MVC   P+5(17),=C'USED AS PUBLISHER'                                    
         MVI   SPACING,2        SKIP A LINE                                     
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
WRITE5   CLI   QOPT1,C'Y'     SEE IF DUMPING BEFORE AND AFTER                   
         BNE   WRITEX                                                           
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,PREPLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',AREC,C'DUMP',(R3),=C'2D',     +        
               (C'P',PRINT)                                                     
*                                                                               
WRITEX   DS    0H                                                               
         AP    CHARECS,=P'1'                                                    
         GOTO1 PUTPRT                                                           
         CLI   8(R1),0                                                          
         BE    AGYC3               GET NEXT RECORD                              
         DC    H'0'                                                             
*                                                                               
NEXTAM   DS    0H                                                               
         MVI   KEY+3,X'FF'        HIGHEST POSSIBLE RECORD CODE                  
         XC    KEY+4(21),KEY+4    CLEAR EST OF KEY                              
         B     AGYC2                                                            
*                                                                               
*                                                                               
PUBLCHK  NTR1                                                                   
         MVI   DUB,0                                                            
         MVC   SVKEY,KEY          MUST SAVE KEY                                 
         XC    KEY,KEY                                                          
         MVI   KEY,X'F0'                                                        
         MVI   KEY+1,C'P'                                                       
         MVC   KEY+2(2),SVKEY       AGENCY                                      
         MVC   KEY+4(1),SVKEY+2      MEDIA                                      
         MVC   KEY+5(5),SVKEY+4      REP AND SUFFIX                             
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE       CHK THROUGH REP AND SUFFIX                 
         BNE   PUBLCX                                                           
         OI    DUB,X'01'            SET ON PUBLISHER BIT                        
         AP    PUBLS,=P'1'       PUBLISHER COUNT                                
*                                                                               
PUBLCX   MVC   KEY,SVKEY                                                        
         GOTO1 HIGH              MUST RESTORE SEQ READ                          
         XIT1                                                                   
         EJECT                                                                  
COUNTS   DS    0C                                                               
REPRECS  DS    PL8                                                              
         DC    CL15'   REPS READ'                                               
REP126   DS    PL8                                                              
         DC    CL15' OLD LEN=126'                                               
REP152   DS    PL8                                                              
         DC    CL15' OLD LEN=152'                                               
REP164   DS    PL8                                                              
         DC    CL15' OLD LEN=164'                                               
REP166   DS    PL8                                                              
         DC    CL15' OLD LEN=166'                                               
PUBLS    DS    PL8                                                              
         DC    CL15'  PUBLISHERS'                                               
CHARECS  DS    PL8                                                              
         DC    CL15'REPS CHANGED'                                               
         DC    X'FF'                                                            
*                                                                               
ELCODE   DS    C                                                                
ELEMENT  DS    XL256                                                            
SVKEY    DS    CL32                                                             
*                                                                               
RECCHANG DS    C                                                                
         EJECT                                                                  
RUNL     MVI   FORCEHED,C'Y'                                                    
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
NEXTEL   ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL                                                           
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,31                                                            
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
         EJECT                                                                  
         LTORG                                                                  
         SPACE 5                                                                
       ++INCLUDE PEUPLREC                                                       
         EJECT                                                                  
PP02WRKD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'162PPREPFIX  05/01/02'                                      
         END                                                                    
