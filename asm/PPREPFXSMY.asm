*          DATA SET PPREPFXSMY AT LEVEL 020 AS OF 05/01/02                      
*PHASE PP0202X                                                                  
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
         MVI   ELCODE,X'03'                                                     
         CLI   QOPT2,C'Y'          CHANGE DIV RECS ?                            
         BE    AGYRTNS             YES                                          
         MVI   ELCODE,X'04'                                                     
         CLI   QOPT3,C'Y'          CHANGE REG RECS ?                            
         BE    AGYRTNS             YES                                          
         MVI   ELCODE,X'05'                                                     
         CLI   QOPT4,C'Y'          CHANGE DST RECS ?                            
         BE    AGYRTNS             YES                                          
         B     EXIT                                                             
*                                                                               
AGYRTNS  XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGY MEDIA                           
         MVC   KEY+3(1),ELCODE     ELCODE USED HERE AS RECORD CODE              
*                RECORD CODE SAME AS ELCODE FOR DIV,REG, AND DST RECS           
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
         CLI   KEY,X'FF'           END OF FILE ?                                
         BNE   AGYC4D              NO                                           
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT2,C'Y'          YES                                          
         BNE   *+12                                                             
         MVI   QOPT2,C' '          DONE WITH DIV RECS                           
         B     OPTSEL              TEST FOR REG AND/OR DST SELECTION            
         CLI   QOPT3,C'Y'                                                       
         BNE   EXIT                FINISHED                                     
         MVI   QOPT3,C' '          DONE WITH REG RECS                           
         B     OPTSEL              TEST FOR DST SELECTION                       
*                                                                               
AGYC4D   CLC   KEY+3(1),ELCODE     SEE IF DIV,REG, OR DST REC                   
         BH    NEXTAM                GO DO NEXT AGY/MEDIA                       
         BE    AGYC4X                                                           
         MVC   KEY+3(1),ELCODE           TRY FOR REC WANTED                     
         XC    KEY+4(21),KEY+4                                                  
         B     AGYC2                                                            
*                                                                               
AGYC4X   DS    0H                                                               
         CLI   QOPT2,C'Y'          DOING DIV RECS ?                             
         BNE   AGYC4X1             NO                                           
         AP    DIVRECS,=P'1'                                                    
         CP    DIVRECS,=P'50'                                                   
         BH    AGYC4X9             ONLY PRINT FIRST 50 RECS                     
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         B     AGYC4X9                                                          
AGYC4X1  CLI   QOPT3,C'Y'          DOING REG RECS ?                             
         BNE   AGYC4X2             NO                                           
         AP    REGRECS,=P'1'                                                    
         CP    REGRECS,=P'50'                                                   
         BH    AGYC4X9             ONLY PRINT FIRST 50 RECS                     
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         B     AGYC4X9                                                          
AGYC4X2  AP    DSTRECS,=P'1'       MUST BE DST RECS                             
         CP    DSTRECS,=P'50'                                                   
         BH    AGYC4X9             ONLY PRINT FIRST 50 RECS                     
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
AGYC4X9  LA    R4,PDIVREC                                                       
         XC    PDIVREC(175),PDIVREC    BE SURE IT CLEAR                         
         ST    R4,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT1,C'Y'     SEE IF DUMPING BEFORE AND AFTER                   
         BNE   AGYCX                                                            
         CLI   QOPT2,C'Y'          DIV REC IN PROCESS ?                         
         BNE   AGYC5               NO                                           
         CP    DIVRECS,=P'50'      YES                                          
         BH    AGYCX               ONLY DUMP FIRST 50 RECS                      
         B     AGYC6                                                            
AGYC5    CLI   QOPT3,C'Y'          REG REC IN PROCESS ?                         
         BNE   AGYC5D              NO                                           
         CP    REGRECS,=P'50'      YES                                          
         BH    AGYCX               ONLY DUMP FIRST 50 RECS                      
         B     AGYC6                                                            
AGYC5D   CP    DSTRECS,=P'50'      MUST BE DOING DST RECS                       
         BH    AGYCX               ONLY DUMP FIRST 50 RECS                      
*                                                                               
AGYC6    SR    R3,R3                                                            
         ICM   R3,3,PDIVLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',AREC,C'DUMP',(R3),=C'2D',    +        
               (C'P',PRINT)                                                     
*                                                                               
AGYCX    DS    0H                                                               
         LA    R2,PDIVELEM         A(FIRST ELEMENT)                             
*                                                                               
         CLI   1(R2),X'20'         ALREADY CORRECT LENGTH?                      
         BE    AGYC3               YES - NEXT RECORD                            
         CLI   1(R2),X'8C'         MAIN ELEM  (140 BYTES)                       
         BE    FIX                                                              
*                                                                               
ODDREC   DS    0H                  ELEMENT NOT LENGTH 140 (X'8C')               
         CLI   QOPT2,C'Y'          DIV RECS IN PROCESS ?                        
         BNE   ODDRECD             NO                                           
         AP    DIVODD,=P'1'        YES                                          
         B     ODDRECX                                                          
ODDRECD  CLI   QOPT3,C'Y'          REG RECS IN PROCESS ?                        
         BNE   ODDRECF             NO                                           
         AP    REGODD,=P'1'        YES                                          
         B     ODDRECX                                                          
ODDRECF  AP    DSTODD,=P'1'        MUST BE DST REC                              
ODDRECX  MVC   P+5(17),=C'BAD RECORD LENGTH'                                    
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
         CLI   QOPT2,C'Y'          DOING DIV RECS ?                             
         BNE   FIXD                NO                                           
         CP    DIVRECS,=P'50'                                                   
         BH    WRITEX              ONLY DUMP FIRST 50 RECS                      
         B     FIXX                                                             
FIXD     CLI   QOPT3,C'Y'          DOING REG RECS ?                             
         BNE   FIXF                NO                                           
         CP    REGRECS,=P'50'                                                   
         BH    WRITEX              ONLY DUMP FIRST 50 RECS                      
         B     FIXX                                                             
FIXF     CP    DSTRECS,=P'50'      MUST BE DST RECS                             
         BH    WRITEX              ONLY DUMP FIRST 50 RECS                      
*                                                                               
FIXX     SR    R3,R3                                                            
         ICM   R3,3,PDIVLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',AREC,C'DUMP',(R3),=C'2D',     +        
               (C'P',PRINT)                                                     
*                                                                               
WRITEX   DS    0H                                                               
         CLI   QOPT2,C'Y'          DOING DIV RECS ?                             
         BNE   WRITEXD             NO                                           
         AP    DIVCHAS,=P'1'       YES                                          
         B     WRITEXX                                                          
WRITEXD  CLI   QOPT3,C'Y'          DOING REG RECS ?                             
         BNE   WRITEXF             NO                                           
         AP    REGCHAS,=P'1'       YES                                          
         B     WRITEXX                                                          
WRITEXF  AP    DSTCHAS,=P'1'       MUST BE DST RECS                             
*                                                                               
WRITEXX  CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
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
**PAN#1  DC    CL21'020PPREPFXSMY05/01/02'                                      
         END                                                                    
