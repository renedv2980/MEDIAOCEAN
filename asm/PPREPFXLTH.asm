*          DATA SET PPREPFXLTH AT LEVEL 017 AS OF 05/01/02                      
*PHASE PP0202R                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM CHANGES LENGTHS OF DIVISION, REGION, AND                  
*                     DISTRICT RECORDS                                          
*                                                                               
*        QOPT1     Y= DUMP RECORDS BEFORE AND AFTER CONVERSION                  
*                                                                               
*        QOPT2     Y= SHORTEN DIVISION RECORDS                                  
*        QOPT3     Y= SHORTEN REGION   RECORDS                                  
*        QOPT4     Y= SHORTEN DISTRICT RECORDS                                  
*                                                                               
*        QOPT5     Y= TEST RUN (DON'T MARK FILE)                                
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
RUNF     ZAP   DIVRECS,=P'0'                                                    
         ZAP   DIVCHAS,=P'0'                                                    
         ZAP   DIVODD,=P'0'                                                     
         ZAP   REGRECS,=P'0'                                                    
         ZAP   REGCHAS,=P'0'                                                    
         ZAP   REGODD,=P'0'                                                     
         ZAP   DSTRECS,=P'0'                                                    
         ZAP   DSTCHAS,=P'0'                                                    
         ZAP   DSTODD,=P'0'                                                     
         B     EXIT                                                             
         EJECT                                                                  
PROC     MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
OPTSEL   DS    0H                                                               
         CLI   QOPT2,C'Y'          CHANGE DIV RECS ?                            
         BE    DIVRTNS             YES                                          
OPTSEL3  CLI   QOPT3,C'Y'          CHANGE REG RECS ?                            
         BE    REGRTNS             YES                                          
OPTSEL4  CLI   QOPT4,C'Y'          CHANGE DST RECS ?                            
         BE    DSTRTNS             YES                                          
         B     EXIT                NO SELECTION ENTERED                         
*                                                                               
DIVRTNS  XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGY MEDIA                           
         MVI   KEY+3,X'03'                                                      
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
         BE    OPTSEL3             TEST FOR REG AND/OR DST SELECTION            
*                                                                               
         CLI   KEY+3,X'03'          SEE IF DIV REC                              
         BH    NEXTAM                GO DO NEXT AGY/MEDIA                       
         BE    AGYC4X                                                           
         MVI   KEY+3,X'03'            TRY FOR DIVS                              
         XC    KEY+4(21),KEY+4                                                  
         B     AGYC2                                                            
*                                                                               
AGYC4X   DS    0H                                                               
         AP    DIVRECS,=P'1'                                                    
         CP    DIVRECS,=P'50'                                                   
         BH    AGYC4X1             ONLY PRINT FIRST 50 RECS                     
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
AGYC4X1  LA    R4,PDIVREC                                                       
         XC    PDIVREC(175),PDIVREC    BE SURE IT CLEAR                         
         ST    R4,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT1,C'Y'     SEE IF DUMPING BEFORE AND AFTER                   
         BNE   AGYCX                                                            
         CP    DIVRECS,=P'50'                                                   
         BH    AGYCX               ONLY DUMP FIRST 50 RECS                      
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,PDIVLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',AREC,C'DUMP',(R3),=C'2D',    +        
               (C'P',PRINT)                                                     
*                                                                               
AGYCX    DS    0H                                                               
         LA    R2,PDIVELEM         A(FIRST ELEMENT)                             
*                                                                               
         CLC   0(2,R2),=X'0320'    ALREADY CORRECT LENGTH?                      
         BE    AGYC3               YES - NEXT RECORD                            
         CLC   0(2,R2),=X'038C'    MAIN ELEM  (140 BYTES)                       
         BE    FIX                                                              
*                                                                               
ODDREC   DS    0H                  DIVELEM NOT LENGTH 140 (X'8C')               
         AP    DIVODD,=P'1'                                                     
         MVC   P+5(17),=C'BAD RECORD LENGTH'                                    
         MVI   SPACING,2        SKIP A LINE                                     
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         B     AGYC3               NEXT RECORD                                  
*                                                                               
FIX      DS    0H                                                               
         MVI   PDIVELEM+1,X'20'    SET NEW ELEM LENGTH                          
         MVC   PDIVLEN,=H'65'      (32 + 33)                                    
*                                                                               
         CLI   QOPT1,C'Y'     SEE IF DUMPING BEFORE AND AFTER                   
         BNE   WRITEX                                                           
         CP    DIVRECS,=P'50'                                                   
         BH    WRITEX              ONLY DUMP FIRST 50 RECS                      
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,PDIVLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',AREC,C'DUMP',(R3),=C'2D',     +        
               (C'P',PRINT)                                                     
*                                                                               
WRITEX   DS    0H                                                               
         AP    DIVCHAS,=P'1'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BE    AGYC3               GET NEXT RECORD                              
         GOTO1 PUTPRT                                                           
         CLI   8(R1),0                                                          
         BE    AGYC3               GET NEXT RECORD                              
         DC    H'0'                                                             
*                                                                               
NEXTAM   DS    0H                                                               
         MVI   KEY+3,X'FF'        HIGHEST POSSIBLE RECORD CODE                  
         XC    KEY+4(21),KEY+4    CLEAR REST OF KEY                             
         B     AGYC2                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
REGRTNS  DS    0H              ALTER RECORD LENGTH OF REGION RECORDS            
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGY MEDIA                           
         MVI   KEY+3,X'04'         REGION RECORD CODE                           
*                                                                               
RAGYC2   GOTO1 HIGH                                                             
         CLI   8(R1),0                                                          
         BE    RAGYC4                                                           
         DC    H'0'                                                             
*                                                                               
RAGYC3   GOTO1 SEQ                                                              
         CLI   8(R1),0                                                          
         BE    RAGYC4                                                           
         DC    H'0'                                                             
*                                                                               
RAGYC4   DS    0H                                                               
         CLI   KEY,X'FF'            END OF FILE                                 
         BE    OPTSEL4             TEST FOR DST SELECTION                       
*                                                                               
         CLI   KEY+3,X'04'          SEE IF REG REC                              
         BH    RNEXTAM              GO DO NEXT AGY/MEDIA                        
         BE    RAGYC4X                                                          
         MVI   KEY+3,X'04'            TRY FOR REGS                              
         XC    KEY+4(21),KEY+4                                                  
         B     RAGYC2                                                           
*                                                                               
RAGYC4X  DS    0H                                                               
         AP    REGRECS,=P'1'                                                    
         CP    REGRECS,=P'50'                                                   
         BH    RAGYC4X1             ONLY PRINT FIRST 50 RECS                    
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
RAGYC4X1 LA    R4,PREGREC                                                       
         XC    PREGREC(175),PREGREC    BE SURE IT CLEAR                         
         ST    R4,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT1,C'Y'     SEE IF DUMPING BEFORE AND AFTER                   
         BNE   RAGYCX                                                           
         CP    REGRECS,=P'50'                                                   
         BH    RAGYCX               ONLY DUMP FIRST 50 RECS                     
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,PREGLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',AREC,C'DUMP',(R3),=C'2D',    +        
               (C'P',PRINT)                                                     
*                                                                               
RAGYCX   DS    0H                                                               
         LA    R2,PREGELEM         A(FIRST ELEMENT)                             
*                                                                               
         CLC   0(2,R2),=X'0420'    ALREADY CORRECT LENGTH?                      
         BE    RAGYC3               YES - NEXT RECORD                           
         CLC   0(2,R2),=X'048C'    MAIN ELEM  (140 BYTES)                       
         BE    RFIX                                                             
*                                                                               
RODDREC  DS    0H                  REG ELEM NOT LENGTH 140 (X'8C')              
         AP    REGODD,=P'1'                                                     
         MVC   P+5(17),=C'BAD RECORD LENGTH'                                    
         MVI   SPACING,2        SKIP A LINE                                     
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         B     RAGYC3               NEXT RECORD                                 
*                                                                               
RFIX     DS    0H                                                               
         MVI   PREGELEM+1,X'20'    SET NEW ELEM LENGTH                          
         MVC   PREGLEN,=H'65'      (32 + 33)                                    
*                                                                               
         CLI   QOPT1,C'Y'     SEE IF DUMPING BEFORE AND AFTER                   
         BNE   RWRITEX                                                          
         CP    REGRECS,=P'50'                                                   
         BH    RWRITEX              ONLY DUMP FIRST 50 RECS                     
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,PREGLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',AREC,C'DUMP',(R3),=C'2D',     +        
               (C'P',PRINT)                                                     
*                                                                               
RWRITEX  DS    0H                                                               
         AP    REGCHAS,=P'1'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BE    RAGYC3              GET NEXT RECORD                              
         GOTO1 PUTPRT                                                           
         CLI   8(R1),0                                                          
         BE    RAGYC3               GET NEXT RECORD                             
         DC    H'0'                                                             
*                                                                               
RNEXTAM  DS    0H                                                               
         MVI   KEY+3,X'FF'        HIGHEST POSSIBLE RECORD CODE                  
         XC    KEY+4(21),KEY+4    CLEAR REST OF KEY                             
         B     RAGYC2                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
DSTRTNS  DS    0H               ALTER LENGTH OF DISTRICT RECORDS                
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGY MEDIA                           
         MVI   KEY+3,X'05'         DISTRICT RECORD CODE                         
*                                                                               
DAGYC2   GOTO1 HIGH                                                             
         CLI   8(R1),0                                                          
         BE    DAGYC4                                                           
         DC    H'0'                                                             
*                                                                               
DAGYC3   GOTO1 SEQ                                                              
         CLI   8(R1),0                                                          
         BE    DAGYC4                                                           
         DC    H'0'                                                             
*                                                                               
DAGYC4   DS    0H                                                               
         CLI   KEY,X'FF'            END OF FILE                                 
         BE    EXIT                                                             
*                                                                               
         CLI   KEY+3,X'05'          SEE IF DST REC                              
         BH    DNEXTAM              GO DO NEXT AGY/MEDIA                        
         BE    DAGYC4X                                                          
         MVI   KEY+3,X'05'            TRY FOR DSTS                              
         XC    KEY+4(21),KEY+4                                                  
         B     DAGYC2                                                           
*                                                                               
DAGYC4X  DS    0H                                                               
         AP    DSTRECS,=P'1'                                                    
         CP    DSTRECS,=P'50'                                                   
         BH    DAGYC4X1             ONLY PRINT FIRST 50 RECS                    
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
DAGYC4X1 LA    R4,PDSTREC                                                       
         XC    PDSTREC(175),PDSTREC    BE SURE IT CLEAR                         
         ST    R4,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT1,C'Y'     SEE IF DUMPING BEFORE AND AFTER                   
         BNE   DAGYCX                                                           
         CP    DSTRECS,=P'50'                                                   
         BH    DAGYCX               ONLY DUMP FIRST 50 RECS                     
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,PDSTLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',AREC,C'DUMP',(R3),=C'2D',    +        
               (C'P',PRINT)                                                     
*                                                                               
DAGYCX   DS    0H                                                               
         LA    R2,PDSTELEM         A(FIRST ELEMENT)                             
*                                                                               
         CLC   0(2,R2),=X'0520'    ALREADY CORRECT LENGTH?                      
         BE    DAGYC3               YES - NEXT RECORD                           
         CLC   0(2,R2),=X'058C'    MAIN ELEM  (140 BYTES)                       
         BE    DFIX                                                             
*                                                                               
DODDREC  DS    0H                  DST ELEM NOT LENGTH 140 (X'8C')              
         AP    DSTODD,=P'1'                                                     
         MVC   P+5(17),=C'BAD RECORD LENGTH'                                    
         MVI   SPACING,2        SKIP A LINE                                     
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         B     DAGYC3               NEXT RECORD                                 
*                                                                               
DFIX     DS    0H                                                               
         MVI   PDSTELEM+1,X'20'    SET NEW ELEM LENGTH                          
         MVC   PDSTLEN,=H'65'      (32 + 33)                                    
*                                                                               
         CLI   QOPT1,C'Y'     SEE IF DUMPING BEFORE AND AFTER                   
         BNE   DWRITEX                                                          
         CP    DSTRECS,=P'50'                                                   
         BH    DWRITEX              ONLY DUMP FIRST 50 RECS                     
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,PDSTLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',AREC,C'DUMP',(R3),=C'2D',     +        
               (C'P',PRINT)                                                     
*                                                                               
DWRITEX  DS    0H                                                               
         AP    DSTCHAS,=P'1'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BE    DAGYC3              GET NEXT RECORD                              
         GOTO1 PUTPRT                                                           
         CLI   8(R1),0                                                          
         BE    DAGYC3               GET NEXT RECORD                             
         DC    H'0'                                                             
*                                                                               
DNEXTAM  DS    0H                                                               
         MVI   KEY+3,X'FF'        HIGHEST POSSIBLE RECORD CODE                  
         XC    KEY+4(21),KEY+4    CLEAR REST OF KEY                             
         B     DAGYC2                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
COUNTS   DS    0C                                                               
DIVRECS  DS    PL8                                                              
         DC    CL15'   DIVS READ'                                               
DIVCHAS  DS    PL8                                                              
         DC    CL15'DIVS CHANGED'                                               
DIVODD   DS    PL8                                                              
         DC    CL15'DIV LNTH BAD'                                               
REGRECS  DS    PL8                                                              
         DC    CL15'   REGS READ'                                               
REGCHAS  DS    PL8                                                              
         DC    CL15'REGS CHANGED'                                               
REGODD   DS    PL8                                                              
         DC    CL15'REG LNTH BAD'                                               
DSTRECS  DS    PL8                                                              
         DC    CL15'   DSTS READ'                                               
DSTCHAS  DS    PL8                                                              
         DC    CL15'DSTS CHANGED'                                               
DSTODD   DS    PL8                                                              
         DC    CL15'DST LNTH BAD'                                               
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
         LA    R6,31                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R6),=C'N'                                 
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
**PAN#1  DC    CL21'017PPREPFXLTH05/01/02'                                      
         END                                                                    
