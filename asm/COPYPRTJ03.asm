*          DATA SET COPYPRTJ03 AT LEVEL 041 AS OF 10/05/00                      
*PHASE COPYPRTF                                                                 
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
*        FROM JW (JWT) TO H7 (MINDSHARE)                                        
*                                                                               
*        X'03'   DIVISIONS                                                      
*        X'04'   REGIONS                                                        
*        X'05'   DISTRICTS                                                      
*        X'07'   ESTIMATES                                                      
*        X'09'   ESTIMATE BUCKETS                                               
*        X'10'   CONTRACTS                                                      
*        X'11'   REPS                                                           
*        X'15'   JOB RECORDS                                                    
*        X'17'   PUBLISTS                                                       
*        X'20'   BUYS                                                           
*        X'40'   STANDARD COMMENT                                               
*                                                                               
*                                                                               
*        SINCE THE JOB RECODE CODE IS X'15' AND THE BUY IS X'20'                
*        JOBS ARE ENCOUNTERED BEFORE THE BUYS ARE READ                          
*        SO A PRELIMINARY RUN MUST BE DONE WHICH WILL LIST                      
*        THE JOBS TO BE ADDED TO A HARDCODED TABLE (ADRECTBL)                   
*        FOR THE "LIVE" RUN.                                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        FOR THIS RUN THE OUTPUT AGENCY IS HARDCODED TO H7                      
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
         L     R4,=A(COMTAB)                                                    
         XCEFL 0(R4),7000                                                       
*                                                                               
*        800 X 7                                                                
*                                                                               
         L     R4,=A(ESTTAB)                                                    
         XCEFL 0(R4),9000                                                       
*                                                                               
*        4000 X 9                                                               
*                                                                               
         L     R4,=A(ADTAB)                                                     
         XCEFL 0(R4),2600                                                       
*                                                                               
*        4000 X 13                                                              
*                                                                               
         SR    R0,R0                                                            
         L     R1,=A(ADTAB)                                                     
         SR    R2,R2                                                            
         LA    R3,13                                                            
         LA    R4,13                                                            
         LA    R5,200                                                           
         STM   R0,R5,ADPARS                                                     
*                                                                               
         SR    R0,R0                                                            
         L     R1,=A(COMTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,7                                                             
         LA    R4,7                                                             
         LA    R5,1000                                                          
         STM   R0,R5,COMPARS                                                    
*                                                                               
         SR    R0,R0                                                            
         L     R1,=A(ESTTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,9                                                             
         LA    R4,9                                                             
         LA    R5,1000                                                          
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
         CLC   REC(2),=C'JW'                                                    
         BH    EOF                 CAN STOP READING                             
*                                                                               
         CLC   REC(3),=C'JWM'      JW THOMPSON - MAGAZINES                      
         BE    GET20                                                            
         CLC   REC(3),=C'JWN'      JW THOMPSON - NEWSPAPERS                     
         BE    GET20                                                            
         CLC   REC(3),=C'JWO'      JW THOMPSON - OUTDOOR                        
         BE    GET20                                                            
         CLC   REC(3),=C'JWT'      JW THOMPSON - TRADE                          
         BE    GET20                                                            
         B     GET                                                              
*                                                                               
*                                                                               
* THESE RECORD(S) HAVE SPECIAL ROUTINES                                         
*                                                                               
GET20    DS    0H                                                               
*                                                                               
*        DIV/REG/DST COPY NO-OPED                                               
*                                                                               
****     CLI   REC+3,X'03'         DIVISIONS                                    
****     BE    DIV                                                              
****     CLI   REC+3,X'04'         REGIONS                                      
****     BE    REG                                                              
****     CLI   REC+3,X'05'         DISTRICTS                                    
****     BE    DST                                                              
         CLI   REC+3,X'07'         ESTIMATES                                    
         BE    EST                                                              
*                                                                               
         CLI   REC+3,X'09'         ESTIMATE BUCKETS                             
         BE    ESTB                                                             
*                                                                               
         CLI   REC+3,X'10'         CONTRACTS                                    
         BE    CON                                                              
*                                                                               
         CLI   REC+3,X'11'         REPS                                         
         BE    REP                                                              
*                                                                               
         CLI   REC+3,X'15'         JOB RECORDS                                  
         BE    JOB                                                              
*                                                                               
         CLI   REC+3,X'17'         PUBLISTS                                     
         BE    PUBL                                                             
*                                                                               
         CLI   REC+3,X'20'         BUYS                                         
         BE    BUY                                                              
*                                                                               
         CLI   REC+3,X'40'         STANDARD COMMENTS                            
         BE    COM                                                              
*                                                                               
         B     GET                SKIP OTHER RECORD TYPES                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DIV      DS    0H                    DIVISIONS                                  
         AP    DIVCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
REG      DS    0H                    REGIONS                                    
         AP    REGCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
DST      DS    0H                    DISTRICTS                                  
         AP    DSTCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
PUBL     DS    0H                  PUBLISTS                                     
         AP    PUBLCNT,=P'1'                                                    
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
REP      DS    0H                    REPS                                       
         AP    REPCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
EST      DS    0H                                                               
*                                                                               
         LA    R2,REC                                                           
         USING PESTREC,R2                                                       
*                                                                               
         CLC   PESTST(6),=X'FAF0F1F2F3F1'   MUST START IN 2001                  
         BNH   GET                 (AFTER 12/31/00)                             
*                                  ADD TO ESTIMATE TABLE                        
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
*                                  ADD TO COMMENT TABLE                         
         CLC   PESTCOM,=C'      '                                               
         BNH   EST10                                                            
         XC    WORK,WORK                                                        
         MVC   WORK(1),REC+2       MEDIA                                        
         MVC   WORK+1(6),PESTCOM   STD COMMENT CODE                             
*                                                                               
         GOTO1 =V(BINSRCH),COMPARS,(X'01',WORK)                                 
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         AP    ECCNT,=P'1'                                                      
*                                                                               
EST10    DS    0H                                                               
         CLC   PESTCOM2,=C'      '                                              
         BNH   ESTX                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(1),REC+2       MEDIA                                        
         MVC   WORK+1(6),PESTCOM2  STD COMMENT CODE                             
*                                                                               
         GOTO1 =V(BINSRCH),COMPARS,(X'01',WORK)                                 
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         AP    ECCNT,=P'1'                                                      
*                                                                               
ESTX     B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
         DROP  R2                                                               
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
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
BUY      DS    0H                                                               
*                                                                               
         LA    R2,REC                                                           
         USING PBUYREC,R2                                                       
*                                                                               
         TM    PBUYCNTL,X'80'                                                   
         BNZ   GET              SKIP DELETED/CLOSED OUT INSERTIONS              
*                                                                               
         CLC   PBUYKDAT(3),=X'640C1F' MUST BE IN 2001 (AFTER DEC31/00)          
         BNH   GET                 NO - SKIP THIS RECORD                        
*                                                                               
*                         SEE IF ESTIMATE IS IN TABLE                           
         XC    WORK,WORK                                                        
         MVC   WORK(1),REC+2      MEDIA                                         
         MVC   WORK+1(6),REC+4     CLT AND PRODUCT                              
         MVC   WORK+7(2),PBUYKEST                                               
         GOTO1 =V(BINSRCH),ESTPARS,(X'00',WORK)                                 
         CLI   0(R1),1        SEE IF IN TABLE                                   
         BE    GET            NO - THEN SKIP                                    
*                                                                               
         DROP  R2                                                               
*                                                                               
         LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
         MVI   ELCODE,X'25'        PAY ELEMENT                                  
*                                                                               
BUY05    BAS   RE,NEXTEL           FOUND ?                                      
         BNE   BUY15               NO                                           
         OC    2(3,R2),2(R2)       CHECK FOR DATE                               
         BZ    BUY15                                                            
         MVC   P+1(22),=C'*** PAID INSERTION ***'                               
         BAS   RE,PRNT                                                          
         BAS   RE,DMPKEY                                                        
*                                                                               
BUY15    LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
         MVI   ELCODE,X'26'        BILL ELEMENT                                 
*                                                                               
         BAS   RE,NEXTEL           FOUND ?                                      
         BNE   BUY15X              NO                                           
         OC    5(3,R2),5(R2)       CHECK FOR DATE                               
         BZ    BUY15X                                                           
         MVC   P+1(24),=C'*** BILLED INSERTION ***'                             
         BAS   RE,PRNT                                                          
         BAS   RE,DMPKEY                                                        
*                                                                               
*                                                                               
BUY15X   LA    R2,REC            RESET R2 TO REC                                
         USING PBUYREC,R2                                                       
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
         B     AGSW                JUST GO SWITCH AGENCYS                       
****************************************************************                
*                                                                               
CON      DS    0H                                                               
*                                                                               
         LA    R2,REC                                                           
         USING PCONREC,R2                                                       
         CLC   PCONEDT(3),=X'640C1F' MUST BE IN 2001 (AFTER DEC31/00)           
         BNH   GET                 NO - SKIP THIS RECORD                        
*                                                                               
         DROP  R2                                                               
*                                                                               
CON20    DS    0H                                                               
         LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
         MVI   ELCODE,X'30'        STANDARD COMMENT ELEMENT                     
*                                                                               
CON25    BAS   RE,NEXTEL           FOUND ?                                      
         BNE   CON30               NO                                           
*                                  ADD TO COMMENT TABLE                         
         XC    WORK,WORK                                                        
         MVC   WORK(1),REC+2       MEDIA                                        
         MVC   WORK+1(6),2(R2)     STD COMMENT CODE                             
*                                                                               
         GOTO1 =V(BINSRCH),COMPARS,(X'01',WORK)                                 
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         AP    CCCNT,=P'1'                                                      
*                                                                               
         B     CON25               TEST FOR MORE                                
*                                                                               
CON30    DS    0H                                                               
         AP    CONCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
JOB      DS    0H                                                               
         LA    R5,ADRECTBL                                                      
JOBLUP   CLI   0(R5),X'FF'         END OF TABLE ?                               
         BE    GET                 YES - SKIP RECORD                            
         CLC   REC(16),0(R5)                                                    
         BE    JOBCPY                                                           
         LA    R5,16(R5)           BUMP TO NEXT ENTRY                           
         B     JOBLUP              TEST NEXT                                    
*                                                                               
JOBCPY   DS    0H                                                               
         AP    JOBCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
COM      DS    0H                                                               
*                         SEE IF COMMENT IS IN TABLE                            
         XC    WORK,WORK                                                        
         MVC   WORK(1),REC+2      MEDIA                                         
         MVC   WORK+1(6),REC+4     COMMENT CODE                                 
         GOTO1 =V(BINSRCH),COMPARS,(X'00',WORK)                                 
         CLI   0(R1),1                                                          
         BE    GET                                                              
         AP    COMCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ADD CODE HERE IF SWITCHING AGENCY/MEDIA                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
AGSW     MVC   REC(2),=C'H7'       MINDSHARE                                    
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
         MVC   P+1(21),=C'      JOBS IN ADTAB -'                                
         EDIT  (B4,ADPARS+8),(4,P+23)                                           
         BAS   RE,PRNT                                                          
*                                                                               
*                                                                               
         L     R3,=A(ADTAB)                                                     
         MVC   P+01(18),=C'JOB RECORDS FOLLOW'                                  
         BAS   RE,PRNT                                                          
         BAS   RE,PRNT                                                          
EOJADS   DS    0H                                                               
         MVC   P+05(13),0(R3)                                                   
         BAS   RE,PRNT                                                          
         LA    R3,13(R3)            NEXT TABLE ENTRY                            
         CLI   0(R3),0                                                          
         BNE   EOJADS                                                           
*                                                                               
EOJCOMS  DS    0H                                                               
         BAS   RE,PRNT       SKIP A LINE                                        
         L     R3,=A(COMTAB)                                                    
         MVC   P+01(18),=C'COMMENTS FOLLOW   '                                  
         BAS   RE,PRNT                                                          
         BAS   RE,PRNT                                                          
EOJCOM5  DS    0H                                                               
         MVC   P+05(7),0(R3)                                                    
         BAS   RE,PRNT                                                          
         LA    R3,7(R3)            NEXT TABLE ENTRY                             
         CLI   0(R3),0                                                          
         BNE   EOJCOM5                                                          
*                                                                               
EOJESTS  DS    0H                                                               
         BAS   RE,PRNT       SKIP A LINE                                        
         L     R3,=A(ESTTAB)                                                    
         MVC   P+01(18),=C'ESTIMATES FOLLOW '                                   
         BAS   RE,PRNT                                                          
         BAS   RE,PRNT                                                          
EOJEST5  DS    0H                                                               
         MVC   P+05(1),0(R3)     MEDIA                                          
         MVC   P+07(3),1(R3)     CLIENT                                         
         MVC   P+11(3),4(R3)     PRODUCT                                        
         MVC   HALF,7(R3)        ESTIMATE                                       
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+15(3),DUB+5(3)                                                 
         BAS   RE,PRNT                                                          
         LA    R3,9(R3)         NEXT TABLE ENTRY                                
         CLI   0(R3),0                                                          
         BNE   EOJEST5                                                          
         B     EOJ                                                              
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
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
DIVCNT   DC    PL5'0',CL20'DIVISIONS'                                           
REGCNT   DC    PL5'0',CL20'REGIONS'                                             
DSTCNT   DC    PL5'0',CL20'DISTRICTS'                                           
PUBLCNT  DC    PL5'0',CL20'PUB LISTS'                                           
REPCNT   DC    PL5'0',CL20'REPS'                                                
ESTCNT   DC    PL5'0',CL20'ESTIMATES'                                           
ESTBCNT  DC    PL5'0',CL20'ESTIMATE BUCKETS'                                    
JOBCNT   DC    PL5'0',CL20'AD RECORDS'                                          
ADCNT    DC    PL5'0',CL20'AD CODES IN BUYS'                                    
ECCNT    DC    PL5'0',CL20'COMMENTS IN ESTS'                                    
CCCNT    DC    PL5'0',CL20'COMMS IN CONTRACTS'                                  
COCNT    DC    PL5'0',CL20'COMMENTS IN BUYS'                                    
BUYCNT   DC    PL5'0',CL20'BUYS'                                                
CONCNT   DC    PL5'0',CL20'CONTRACTS'                                           
COMCNT   DC    PL5'0',CL20'STANDARD COMMENTS'                                   
*                                                                               
* REST OF COUNTERS SHOULD ALL BE ZERO IN THIS COPYPRT RUN                       
*                                                                               
* OTHER COUNTERS ADDED HERE WILL AUTOMATICALLY PRINT AT EOJ                     
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
ADRECTBL DS    0H                                                               
*****    DC    C'AGM',X'15',C'CLTPRDJOB456'                                     
         DC    C'JWM',X'15',C'TRCMRPM01054'     FROM 9/20/00 TEST               
         DC    C'JWN',X'15',C'FUBFUSN0000 '                                     
         DC    C'JWT',X'15',C'NSCGENIS    '                                     
*                                                                               
         DC    X'FF'                                                            
*********************         OLD TABLE                                         
******   DC    C'JWM',X'15',C'ASAMPMTBD   '                                     
**       DC    C'JWM',X'15',C'ASANUCTBD   '                                     
**       DC    C'JWM',X'15',C'BRTGENTBDIL '                                     
**       DC    C'JWM',X'15',C'BRTGENXXXXX '                                     
**       DC    C'JWM',X'15',C'BRTGEN02279A'                                     
**       DC    C'JWM',X'15',C'BRTGEN02290A'                                     
**       DC    C'JWM',X'15',C'BRTGEN02604 '                                     
**       DC    C'JWM',X'15',C'BRTGEN207028'                                     
**       DC    C'JWM',X'15',C'BRTGEN207029'                                     
**       DC    C'JWM',X'15',C'BRTGEN207030'                                     
**       DC    C'JWM',X'15',C'BRTGEN91976A'                                     
**       DC    C'JWM',X'15',C'YMDGENTBD   '                                     
**       DC    C'JWM',X'15',C'YMDGENTBD00 '                                     
**       DC    C'JWN',X'15',C'YR GENFEN   '                                     
**       DC    X'FF'                                                            
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
         DS    200CL13            ROOM FOR 200 JOB CODES                        
         DC    X'0000'                                                          
*                                                                               
COMTAB   CSECT                                                                  
         DS    1000CL7         ROOM FOR 1000 COMMENTS                           
         DC    X'0000'                                                          
*                                                                               
ESTTAB   CSECT                                                                  
         DS    1000CL9        ROOM FOR 1000 ESTIMATES                           
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041COPYPRTJ0310/05/00'                                      
         END                                                                    
