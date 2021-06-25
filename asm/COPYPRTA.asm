*          DATA SET COPYPRTA   AT LEVEL 003 AS OF 08/18/00                      
*PHASE COPYPRTB                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE GETINS                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'COPYPRT - COPY PRINTPAK DATA'                                   
*                                                                               
*        THIS PROGRAM WILL COPY THE FOLLOWING RECORDS                           
*        FROM JT (PRINT 5) TO H0 (PRINT 4)                                      
*                                                                               
*        X'07'   ESTIMATES                                                      
*        X'09'   ESTIMATE BUCKETS                                               
*        X'15'   JOB RECORDS                                                    
*        X'20'   BUYS                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        FOR THIS RUN THE OUTPUT AGENCY IS HARDCODED TO H0                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
COPYPRTJ CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,COPYPRT,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,COPYPRTJ+4095                                                 
         LA    R6,1(R6)                                                         
         USING COPYPRTJ+4096,R6    R6 AS 2ND BASE REGISTER                      
*                                                                               
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*                                                                               
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*                                                                               
* SET ADCODE AND STDCOM AND ESTIMATE NUMBER TABLES BINSRCH PARS                 
*                                                                               
         XCEFL COMTAB,700                                                       
         XCEFL ESTTAB,3150                                                      
*                                                                               
         SR    R0,R0                                                            
         L     R1,=A(ADTAB)                                                     
         SR    R2,R2                                                            
         LA    R3,13                                                            
         LA    R4,13                                                            
         LA    R5,100                                                           
         STM   R0,R5,ADPARS                                                     
*                                                                               
         XCEFL (R1),1300                                                        
*                                                                               
         SR    R0,R0                                                            
         LA    R1,COMTAB                                                        
         SR    R2,R2                                                            
         LA    R3,7                                                             
         LA    R4,7                                                             
         LA    R5,100                                                           
         STM   R0,R5,COMPARS                                                    
*                                                                               
         SR    R0,R0                                                            
         LA    R1,ESTTAB                                                        
         SR    R2,R2                                                            
         LA    R3,9                                                             
         LA    R4,9                                                             
         LA    R5,350                                                           
         STM   R0,R5,ESTPARS                                                    
*                                                                               
START1   DS    0H                                                               
*                                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    START12                                                          
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
*                                                                               
START2   DS    0H                                                               
*                                                                               
         CLC   =C'PRINT',CARD                                                   
         BNE   START3B                                                          
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
*                                                                               
START3   DS    0H                                                               
*                                                                               
*                                                                               
START3B  DS    0H                                                               
*                                                                               
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
*                                                                               
         B     START1                                                           
*                                                                               
START10  DS    0H                                                               
*                                                                               
START12  DS    0H                                                               
*                                                                               
GET      DS    0H                                                               
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
GET10    DS    0H                                                               
*                                                                               
         CLI   REC,C'Z'            SPECIAL DDS RECORD                           
         BE    EOF                 PAST ALL REAL DATA                           
*                                                                               
* ADD CHECK FOR AGENCY BEING COPIED HERE                                        
*                                                                               
         CLC   REC(2),=C'JT'       JW THOMPSON                                  
         BH    EOF                                                              
         BNE   GET                                                              
*                                                                               
*                                                                               
* THESE RECORD(S) HAVE SPECIAL ROUTINES                                         
*                                                                               
         CLI   REC+3,X'07'         ESTIMATES                                    
         BE    EST                                                              
*                                                                               
         CLI   REC+3,X'09'         ESTIMATE BUCKETS                             
         BE    ESTB                                                             
*                                                                               
         CLI   REC+3,X'15'         JOB RECORDS                                  
         BE    JOB                                                              
*                                                                               
         CLI   REC+3,X'20'         BUYS                                         
         BE    BUY                                                              
*                                                                               
*NOP*    CLI   REC+3,X'40'         STANDARD COMMENTS                            
*NOP*    BE    COM                                                              
*                                                                               
         B     GET                SKIP OTHER RECORD TYPES                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EST      DS    0H                                                               
*                                                                               
         LA    R2,REC+33           POINT TO EST RECORD'S FIRST ELEM             
         CLI   0(R2),X'07'                                                      
         BE    *+6                                                              
         DC    H'0'                BAD RECORD OR PROCESSING WRONG REC           
*                                                                               
         USING PESTELEM,R2                                                      
         CLC   PESTST(4),=X'F9F9F0F8'     EST START AFTER  08/99 ?              
         BNH   GET                 NO - SKIP THIS RECORD                        
         CLC   PESTST(4),=X'FAF0F1F0'     EST START BEFORE 10/00 ?              
         BNL   GET                 NO - SKIP THIS RECORD                        
         CLC   PESTEND(4),=X'FAF0F0F8'     EST END  AFTER  08/00 ?              
         BNH   GET                 NO - SKIP THIS RECORD                        
*                                  ADD TO ESTIMATE TABLE                        
         DROP  R2                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),REC+2       MEDIA                                        
         MVC   WORK+1(8),REC+4     CLT/PRD/EST                                  
*                                                                               
         GOTO1 =V(BINSRCH),ESTPARS,(X'01',WORK)                                 
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         AP    ESTCNT,=P'1'                                                     
         B     AGSW                GO SWITCH AGENCYS                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ESTB     DS    0H                                                               
*                   SEE IF ESTIMATE BUCKET IS IN ESTIMATE TABLE                 
         XC    WORK,WORK                                                        
         MVC   WORK(1),REC+2       MEDIA                                        
         MVC   WORK+1(8),REC+4     CLT/PRD/EST                                  
*                                                                               
         GOTO1 =V(BINSRCH),ESTPARS,(X'00',WORK)                                 
         CLI   0(R1),1             RECORD FOUND ?                               
         BE    GET                 NO - SKIP                                    
*                                                                               
         AP    ESTBCNT,=P'1'                                                    
         B     AGSW                GO SWITCH AGENCYS                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BUY      DS    0H                                                               
*                                                                               
         LA    R2,REC                                                           
         USING PBUYREC,R2                                                       
         CLC   PBUYKDAT(2),=X'6408'       SEP/00 OR LATER ?                     
         BNH   GET                 NO - SKIP THIS RECORD                        
*                                                                               
         DROP  R2                                                               
*                                                                               
         LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
         CLI   0(R2),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                BAD RECORD OR PROCESSING WRONG REC           
*                                                                               
         USING PBDELEM,R2                                                       
         CLC   PBDJOB,=6C' '       ANY ADCODE PRESENT                           
         BNH   BUY20               NO                                           
*                                                                               
BUY10    DS    0H                  ADD TO ADCODE TABLE                          
         XC    WORK,WORK                                                        
         MVC   WORK(1),REC+2       MEDIA                                        
         MVC   WORK+1(6),REC+4     CLT/PRD                                      
         MVC   WORK+7(6),PBDJOB    ADCODE                                       
*                                                                               
         DROP  R2                                                               
*                                                                               
         GOTO1 =V(BINSRCH),ADPARS,(X'01',WORK)                                  
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         AP    ADCNT,=P'1'                                                      
*                                                                               
BUY20    DS    0H                                                               
         LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
         MVI   ELCODE,X'67'        COMMENT ELEM                                 
*                                                                               
BUY25    BAS   RE,NEXTEL           FOUND ?                                      
         BNE   BUY30               NO                                           
*                                                                               
         CLC   =C'COM=',2(R2)      STANDARD COMMENT ?                           
         BNE   BUY25               NO - TEST FOR MORE                           
*                                  ADD TO COMMENT TABLE                         
         XC    WORK,WORK                                                        
         MVC   WORK(1),REC+2       MEDIA                                        
         MVC   WORK+1(6),6(R2)     STD COMMENT CODE                             
*                                                                               
         GOTO1 =V(BINSRCH),COMPARS,(X'01',WORK)                                 
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         AP    COCNT,=P'1'                                                      
*                                                                               
         B     BUY25               TEST FOR MORE                                
*                                                                               
BUY30    DS    0H                                                               
         LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
         MVI   ELCODE,X'68'        COMMENT ELEM                                 
*                                                                               
BUY35    BAS   RE,NEXTEL           FOUND ?                                      
         BNE   BUY50               NO - DONE WITH COMMENTS                      
*                                                                               
         CLC   =C'COM=',2(R2)      STANDARD COMMENT ?                           
         BNE   BUY35               NO - TEST FOR MORE                           
*                                  ADD TO COMMENT TABLE                         
         XC    WORK,WORK                                                        
         MVC   WORK(1),REC+2       MEDIA                                        
         MVC   WORK+1(6),6(R2)     STD COMMENT CODE                             
*                                                                               
         GOTO1 =V(BINSRCH),COMPARS,(X'01',WORK)                                 
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         AP    COCNT,=P'1'                                                      
*                                                                               
         B     BUY35               TEST FOR MORE                                
*                                                                               
BUY50    DS    0H                                                               
         AP    BUYCNT,=P'1'                                                     
         B     AGSW                GO SWITCH AGENCYS                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
JOB      DS    0H                                                               
         LA    R5,ADRECTBL                                                      
JOBLUP   CLI   0(R5),X'FF'         END OF TABLE ?                               
         BE    GET                 YES - SKIP RECORD                            
         CLC   REC(16),0(R5)       AD RECORD IN TABLE ?                         
         BE    JOBCPY              YES - COPY                                   
         LA    R5,16(R5)           BUMP TO NEXT ENTRY                           
         B     JOBLUP              TEST NEXT                                    
*                                                                               
JOBCPY   DS    0H                                                               
         AP    JOBCNT,=P'1'                                                     
         B     AGSW                GO SWITCH AGENCYS                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
COM      DS    0H                                                               
         AP    COMCNT,=P'1'                                                     
*NOP*    B     AGSW                GO SWITCH AGENCYS                            
         B     GET                                                              
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ADD CODE HERE IF SWITCHING AGENCY/MEDIA                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
AGSW     MVC   REC(2),=C'H0'       MINDSHARE                                    
*                                                                               
OUT90    CLI   PRTSW,C'Y'                                                       
         BNE   PUT                                                              
         MVI   DMPSW,C'Y'                                                       
         B     PUT                                                              
*                                                                               
*                                                                               
PUT      DS    0H                                                               
*                                                                               
PUTXX    BAS   RE,PUTREC                                                        
         B     GET                                                              
*                                                                               
*                                                                               
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
*                                                                               
         BAS   RE,PRNT                                                          
         LA    R3,COUNTS                                                        
         LA    R4,25                                                            
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(20),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+22(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
*                                                                               
         BAS   RE,PRNT                                                          
         MVC   P+1(21),=C'ESTIMATES IN ESTTAB -'                                
         EDIT  (B4,ESTPARS+8),(4,P+23)                                          
         BAS   RE,PRNT                                                          
         MVC   P+1(21),=C' COMMENTS IN COMTAB -'                                
         EDIT  (B4,COMPARS+8),(4,P+23)                                          
         BAS   RE,PRNT                                                          
         MVC   P+1(21),=C' JOB CODES IN ADTAB -'                                
         EDIT  (B4,ADPARS+8),(4,P+23)                                           
         BAS   RE,PRNT                                                          
*                                                                               
*                                                                               
         B     EOJ                                                              
*                                                                               
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SKIP     MVC   PCOM,=C'BC01'                                                    
         ZAP   LNCNT,=P'0'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT3    MVC   PCOM,=C'BL03'                                                    
         AP    LNCNT,=P'3'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT2    MVC   PCOM,=C'BL02'                                                    
         AP    LNCNT,=P'2'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT     MVC   PCOM,=C'BL01'                                                    
         AP    LNCNT,=P'1'                                                      
*                                                                               
PRNTR    NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMPREC   NTR1                                                                   
*                                                                               
         LA    R5,REC                                                           
         MVC   HALF,REC+25                                                      
         SR    R2,R2                                                            
         LH    R2,HALF                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'REC',(R5),C'DUMP',(R2),=C'1D'                 
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMPKEY   NTR1                                                                   
*                                                                               
         LA    R5,REC                                                           
         LA    R2,KLEN                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'KEY',(R5),C'DUMP',(R2),=C'1D'                 
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
*                                                                               
         CLC   REC+25(2),=X'80FF'   SEE IF DIRECTORY ONLY (DELETED)             
         BE    GETRDO                                                           
         CLC   REC+25(2),=X'00FF'   SEE IF DIRECTORY ONLY                       
         BE    GETRDO                                                           
*                                                                               
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0             EOR                                          
         B     GETRX                                                            
*                                                                               
GETRDO   DS    0H               FOR DIRECTORY ONLY RECS                         
*                               NO NEED FOR END OF REC ZERO                     
GETRX    AP    INCNT,=P'1'                                                      
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUTREC   NTR1                                                                   
*                                                                               
         CLI   DMPSW,C'Y'                                                       
         BNE   PUTREC2                                                          
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         BNP   PUTREC2                                                          
         BAS   RE,DMPKEY                                                        
*NOP*    BAS   RE,DMPREC                                                        
*NOP*    BAS   RE,SKIP                                                          
PUTREC2  DS    0H                                                               
         MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
UPSI     DS    XL1                                                              
         DS    0F                                                               
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
LASTAGM  DS    CL3                                                              
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
LKPARS   DS    6F                                                               
ADPARS   DS    6F                                                               
COMPARS  DS    6F                                                               
ESTPARS  DS    6F                                                               
TODAY    DS    CL8                                                              
PRTSW    DS    CL1                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
*                                                                               
COUNTS   DS    0C                                                               
*                          1                 20                                 
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
ESTCNT   DC    PL5'0',CL20'ESTIMATES COPIED'                                    
ESTBCNT  DC    PL5'0',CL20'EST BUCKETS COPIED'                                  
JOBCNT   DC    PL5'0',CL20'AD RECORDS COPIED'                                   
BUYCNT   DC    PL5'0',CL20'BUYS COPIED'                                         
ADCNT    DC    PL5'0',CL20'BUYS WITH AD CODES'                                  
COCNT    DC    PL5'0',CL20'STD COM ELS IN BUYS'                                 
COMCNT   DC    PL5'0',CL20'STANDARD COMMENTS'                                   
*                                                                               
* REST OF COUNTERS SHOULD ALL BE ZERO IN THIS COPYPRT RUN                       
*                                                                               
* OTHER COUNTERS ADDED HERE WILL AUTOMATICALLY PRINT AT EOJ                     
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
*                                                                               
COMTAB   DS    100CL7                                                           
         DC    X'0000'                                                          
ESTTAB   DS    350CL9                                                           
         DC    X'0000'                                                          
*                                                                               
ADRECTBL DS    0H                                                               
         DC    C'JTI',X'15',C'AMSHRC119990'                                     
         DC    C'JTI',X'15',C'AMSHRC129914'                                     
         DC    C'JTI',X'15',C'AMSHRC129926'                                     
         DC    C'JTI',X'15',C'BDSHRC119991'                                     
         DC    C'JTM',X'15',C'AASPRA199963'                                     
         DC    C'JTM',X'15',C'AASPRA199969'                                     
         DC    C'JTM',X'15',C'BLMBABBABY00'                                     
         DC    C'JTM',X'15',C'KCMABSABSY2K'                                     
         DC    C'JTM',X'15',C'KCMALTALTY2K'                                     
         DC    C'JTM',X'15',C'KCMCRLCRLY2K'                                     
         DC    C'JTM',X'15',C'KCMCWPCWPDPS'                                     
         DC    C'JTM',X'15',C'KCMCWPCWP2YK'                                     
         DC    C'JTM',X'15',C'KCMSWUKRSWD1'                                     
         DC    C'JTM',X'15',C'KCMSWUKRSW4A'                                     
         DC    C'JTM',X'15',C'MCMCLURECI00'                                     
         DC    C'JTM',X'15',C'PLMBRIPLMF4C'                                     
         DC    C'JTM',X'15',C'PLMBRIWATBAN'                                     
         DC    C'JTN',X'15',C'AASHRA129901'                                     
         DC    C'JTN',X'15',C'AMSHRC119984'                                     
         DC    C'JTN',X'15',C'AMSHRC129906'                                     
         DC    C'JTN',X'15',C'AMSHRC129911'                                     
         DC    C'JTN',X'15',C'AMSHRC129924'                                     
         DC    C'JTN',X'15',C'BDSHCC100052'                                     
         DC    C'JTN',X'15',C'BDSHRC129915'                                     
         DC    C'JTN',X'15',C'BDSHRC129930'                                     
         DC    C'JTN',X'15',C'CHSHRD129920'                                     
         DC    C'JTN',X'15',C'KCMALTALTY2K'                                     
         DC    C'JTN',X'15',C'MLSHRO119987'                                     
         DC    C'JTN',X'15',C'MLSHRP129908'                                     
         DC    C'JTN',X'15',C'NCSHRW129918'                                     
         DC    C'JTN',X'15',C'OMSHRD129903'                                     
         DC    C'JTN',X'15',C'WRSHRC129902'                                     
         DC    C'JTN',X'15',C'WRSHRW129932'                                     
         DC    C'JTN',X'15',C'WRSHRW129933'                                     
         DC    C'JTO',X'15',C'KCMALTALTY2K'                                     
         DC    C'JTO',X'15',C'KCMTOBTOBY2K'                                     
         DC    X'FF'                                                            
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         DS    D                                                                
*                                                                               
         PRINT OFF                                                              
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PBILLREC                                                       
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PCONREC                                                        
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PESTREC                                                        
*                                                                               
         PRINT ON                                                               
*                                                                               
ADTAB    CSECT                                                                  
         DS    100CL13             ROOM FOR 100 JOB CODES                       
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003COPYPRTA  08/18/00'                                      
         END                                                                    
