*          DATA SET COPYPRT    AT LEVEL 046 AS OF 09/19/03                      
*PHASE COPYPRTA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE GETINS                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'COPYPRT- COPY PRINTPAK DATA'                                    
*                                                                               
*        THIS PROGRAM CAN BE USED TO COPY DATA FROM ONE AGY TO ANOTHER          
*        IT ALWAYS PRODUCES AN OUTPUT TAPE THAT ONLY HAS THE RECORDS            
*        BEING COPIED                                                           
*                                                                               
*        ENTER A CONTROL CARD FOR EACH CLIENT THAT IS TO BE COPIED              
*        IF YOU WISH TO COPY DATA SET UP OR AN OFFICE - ENTER                   
*        A CARD FOR THE OFFICE AS WELL.                                         
*                                                                               
*        NOTE: IF YOU WANT TO COPY DATA FOR ALL CLIENTS IN AN OFFICE            
*              YOU MUST ENTER A CARD FOR EACH CLIENT AND ONE FOR THE            
*              OFFICE.                                                          
*                                                                               
*        THE PROGRAM WILL COPY ONLY CLIENTS (OR OFFICES) ENTERED                
*        IN THE CONTROL CARDS                                                   
*                                                                               
*        THE FOLLOWING RECORDS ARE COPIED TO THE NEW AGENCY                     
*                                                                               
*        THESE RECORDS ARE COPIED REGARDLESS OF THE CLIENT/OFFICE               
*                                                                               
*        X'11'   REPS                                                           
*        X'17'   PUBLIST RECORDS (FOR ALL CLIENTS)                              
*        X'27'   FSI RECORDS (FOR ALL CLIENTS)                                  
*        X'28'   CUREC RECORDS (FOR ALL CLIENTS)                                
*        X'29'   ISSUE DATE RECORDS (FOR ALL CLIENTS)                           
*        X'2B'   SPACE RECORDS (FOR ALL CLIENTS)                                
*        X'40'   STANDARD COMMENTS                                              
*        X'41'   NV TEXT RECORDS (FOR ALL CLIENTS)                              
*                                                                               
*        THE FOLLOWING RECORDS ARE ONLY COPIED TO THE NEW AGENCY                
*        IF THEY ARE FOR THE CLIENT OR OFFICE BEING COPIED                      
*                                                                               
*        X'02'   CLIENTS                                                        
*        X'03'   DIVISIONS                                                      
*        X'04'   REGIONS                                                        
*        X'05'   DISTRICTS                                                      
*        X'06'   PRODUCTS                                                       
*        X'07'   ESTIMATES                                                      
*        X'08'   BILLING HEADERS                                                
*        X'09'   ESTIMATE BUCKETS                                               
*        X'10'   CONTRACTS                                                      
*        X'14'   AOR RECORDS                                                    
*        X'15'   JOB RECORDS                                                    
*        X'17'   PUBLISTS                                                       
*        X'18'   BUDGETS                                                        
*        X'20'   BUYS                                                           
*        X'27'   FSI RECORDS                                                    
*        X'28'   CU RECORDS                                                     
*        X'29'   ISSUE DATE                                                     
*        X'2B'   SPACE                                                          
*        X'41'   NV TEXT RECORDS                                                
*                                                                               
*                                                                               
*************                                                                   
*        FOR THIS RUN THE OUTPUT AGENCY IS HARDCODED TO AF (ON FQA)             
*        AND ONLY DATA ESTIMATES THAT START AFTER DEC31/00 AND                  
*        END BEFORE JAN01/03 ARE COPIED                                         
*************                                                                   
*                                                                               
COPYPRT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,COPYPRT,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,COPYPRT+4095                                                  
         LA    R6,1(R6)                                                         
         USING COPYPRT+4096,R6                                                  
*                                  NOTE USE OF R6 AS BASE REGISTER              
         USING ETABD,R8                                                         
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*&&DO                                                                           
         OPEN  IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*&&                                                                             
*                                                                               
*                                  SET LOOKUP TABLE BINSRCH PARS                
         SR    R0,R0                                                            
         L     R1,=A(LKTAB)                                                     
         SR    R2,R2                                                            
         LA    R3,11                                                            
         LA    R4,11                                                            
         LA    R5,400                                                           
         STM   R0,R5,LKPARS                                                     
*                                  SET ESTTAB BINSRCH PARS                      
         SR    R0,R0                                                            
         L     R1,=A(ESTTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,ETABEL                                                        
         LA    R4,11                                                            
         L     R5,=F'15000'                                                     
         STM   R0,R5,ESTPARS                                                    
*                                                                               
START1   DS    0H                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    START10                                                          
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
START2   DS    0H                                                               
         CLC   =C'PRINT',CARD                                                   
         BNE   START3                                                           
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
START3   DS    0H                                                               
         XC    WORK(11),WORK                                                    
         MVC   WORK(6),CARD+2                                                   
         CLC   CARD+5(3),=C'ALL'                                                
         BNE   *+10                                                             
         MVC   WORK+3(3),=3X'FF'                                                
         MVC   WORK+6(3),CARD+11                                                
         CLC   CARD+11(3),=C'ALL'                                               
         BNE   *+10                                                             
         MVC   WORK+6(3),=3X'FF'                                                
         MVC   WORK+9(2),=3X'FF'                                                
         CLC   CARD+20(3),=C'ALL'                                               
         BE    START3B                                                          
         CLI   CARD+20,C' '                                                     
         BE    START3B                                                          
         PACK  DUB,CARD+20(3)                                                   
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   WORK+9(2),HALF                                                   
START3B  DS    0H                                                               
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
         GOTO1 =V(BINSRCH),LKPARS,(1,WORK)                                      
*                                                                               
         OC    LKPARS+1(3),LKPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     START1                                                           
START10  DS    0H                                                               
         XC    X,X                                                              
         OC    LKPARS+8(4),LKPARS+8                                             
         BNZ   START12                                                          
         MVC   P(18),=C'**NO INPUT CARDS**'                                     
         BAS   RE,PRNT                                                          
         B     EOJ                                                              
START12  DS    0H                                                               
GET      DS    0H                                                               
         BAS   RE,GETREC                                                        
*                                                                               
*                                                                               
         CLI   REC,C'Z'         SPECIAL DDS RECORD                              
         BE    EOF              PAST ALL REAL DATA                              
***                                                                             
***      ADD CHECK FOR AGENCY BEING COPIED HERE                                 
***                                                                             
         CLC   REC(2),=C'BD'       MUST BE BDDE - ALL MEDIA                     
         BH    EOF                                                              
         BNE   GET                                                              
*                                                                               
*        THESE RECORDS GO TO CLIENT ROUTINE                                     
*                                                                               
         CLI   REC+3,X'02'         CLIENT                                       
         BE    CLT                                                              
         CLI   REC+3,X'03'         DIVISIONS                                    
         BE    CLT                                                              
         CLI   REC+3,X'04'         REGIONSS                                     
         BE    CLT                                                              
         CLI   REC+3,X'05'         DISTRICTS                                    
         BE    CLT                                                              
         CLI   REC+3,X'06'         PRODUCT                                      
         BE    CLT                                                              
         CLI   REC+3,X'15'         JOB RECORDS                                  
         BE    CLT                                                              
*                                                                               
*        THESE RECORDS GO TO ESTIMATE ROUTINE                                   
*                                                                               
         CLI   REC+3,X'07'         ESTIMATES                                    
         BE    EST                 (HAS DATE CHECK)                             
         CLI   REC+3,X'09'         ESTIMATE - BUCKETS                           
         BE    EST                 SAME AS EST                                  
         CLI   REC+3,X'14'         AOR RECORDS                                  
         BE    EST                                                              
         CLI   REC+3,X'18'         BUDGET RECORDS                               
         BE    EST                 SAME AS EST                                  
         CLI   REC+3,X'4C'         BILLING FORMULA RECORDS                      
         BE    EST                 SAME AS EST                                  
*                                                                               
*        THESE RECORDS HAVE SPECIAL ROUTINES                                    
*                                                                               
         CLI   REC+3,X'08'         BILL HEADER                                  
         BE    BILL                                                             
         CLI   REC+3,X'10'         CONTRACTS                                    
         BE    CON                 (HAS DATE CHECK)                             
         CLI   REC+3,X'11'         REPS                                         
         BE    REP                                                              
         CLI   REC+3,X'20'         BUY                                          
         BE    BUY                                                              
         CLI   REC+3,X'40'        STANDARD COMMENTS                             
         BE    COMM                                                             
         CLI   REC+3,X'41'        NV TEXT RECS                                  
         BE    NVT                                                              
         CLI   REC+3,X'17'        PUB LIST                                      
         BE    PUBL                                                             
         CLI   REC+3,X'27'        FSI RECS                                      
         BE    FSI                                                              
         CLI   REC+3,X'28'        CU RECS                                       
         BE    CUR                                                              
         CLI   REC+3,X'29'        ISSUE DATE                                    
         BE    ISSUE                                                            
         CLI   REC+3,X'2B'        SPACE                                         
         BE    SPC                                                              
*                                                                               
         B     GET                SKIP OTHER RECORD TYPES                       
*                                                                               
         SPACE 3                                                                
CLT      DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+4                                                  
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
*                                                                               
         LA    RF,CLTCNT                                                        
         CLI   REC+3,X'02'        CLIENTS                                       
         BE    CLT10                                                            
         LA    RF,DIVCNT                                                        
         CLI   REC+3,X'03'        DIVISIONS                                     
         BE    CLT10                                                            
         LA    RF,REGCNT                                                        
         CLI   REC+3,X'04'        REGIONS                                       
         BE    CLT10                                                            
         LA    RF,DSTCNT                                                        
         CLI   REC+3,X'05'        DISTRICTS                                     
         BE    CLT10                                                            
         LA    RF,PRDCNT                                                        
         CLI   REC+3,X'06'        PRODUCTS                                      
         BE    CLT10                                                            
         LA    RF,JOBCNT                                                        
         CLI   REC+3,X'15'        JOBRECS                                       
         BE    CLT10                                                            
         DC    H'0'               UNKNOWN RECORD TYPE                           
CLT10    AP    0(5,RF),=P'1'                                                    
*                                                                               
***                               ADD CODE HERE IF SWITCHING                    
***                               AGENCY/MEDIA                                  
***                                                                             
AGSW     MVC   REC(2),=C'AF'      TO AGENCY AF (ON FQA)                         
*                                                                               
         CLI   PRTSW,C'Y'                                                       
         BNE   PUT                                                              
         MVI   DMPSW,C'Y'                                                       
         B     PUT                                                              
*                                                                               
         SPACE 3                                                                
CON      DS    0H             CONTRACTS                                         
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+4                                                  
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
******** CLI   PCONEDT,99                                                       
******** BL    GET          SKIP IF ENDS BEFORE 1999                            
         AP    CONCNT,=P'1'                                                     
         B     AGSW                                                             
*                                                                               
         SPACE 3                                                                
COMM     DS    0H                  OUTPUT COMMENTS                              
         AP    COMMCNT,=P'1'                                                    
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
         SPACE 3                                                                
REP      DS    0H                  OUTPUT REPS                                  
         AP    REPCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
ISSUE    DS    0H                  ISSUE DATE RECORDS                           
         AP    ISSCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
FSI      DS    0H                                                               
         LA    R5,FSICNT                                                        
         B     SFMRECS                                                          
CUR      DS    0H                                                               
         LA    R5,CURCNT                                                        
         B     SFMRECS                                                          
SPC      DS    0H                                                               
         LA    R5,SPCCNT                                                        
         B     SFMRECS                                                          
*                                                                               
SFMRECS  DS    0H                                                               
         CLC   REC+10(3),=X'FFFFFF'    ALL CLT REC?                             
         BE    SFMRX                   ALWAYS COPY                              
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+10        CLT                                      
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
*                                                                               
SFMRX    AP    0(5,R5),=P'1'                                                    
         B     AGSW                                                             
*                                                                               
NVT      DS    0H                                                               
         CLC   REC+4(3),=X'FFFFFF'      ALL CLIENT RECORD                       
         BE    NVTX                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+4                                                  
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
*                                                                               
NVTX     AP    NVTCNT,=P'1'                                                     
         B     AGSW                                                             
*                                                                               
PUBL     DS    0H                                                               
         CLC   REC+4(3),=C'ZZZ'         ALL CLIENT RECORD                       
         BE    PUBLX                                                            
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+4                                                  
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
*                                                                               
PUBLX    AP    PBLCNT,=P'1'                                                     
         B     AGSW                                                             
*                                                                               
         SPACE 3                                                                
EST      DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(8),REC+4     CLT/PRD/EST                                  
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
         CLI   REC+3,X'07'      SEE IF DOING EST REC                            
         BNE   EST10                                                            
*                                                                               
         CLC   PESTST(6),=X'FAF1F0F1F0F1'    JAN01/01                           
         BL    GET            SKIP IF BEFORE                                    
         CLC   PESTEND(6),=X'FAF2F1F2F3F1'   DEC31/02                           
         BH    GET                                                              
*                                                                               
         GOTO1 =V(BINSRCH),ESTPARS,(1,WORK)                                     
         L     R8,ESTPARS                                                       
         LTR   R8,R8                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AP    ESTCNT,=P'1'                                                     
         B     AGSW                                                             
*                                                                               
EST10    DS    0H             OTHER EST STYLE RECORDS                           
         GOTO1 =V(BINSRCH),ESTPARS,(2,WORK)                                     
         TM    ESTPARS,X'01'     SEE IF NOT FOUND                               
         BO    GET              NOT IN TABLE SO SKIP                            
         LA    R5,BUCCNT                                                        
         CLI   REC+3,X'09'       ESTIMATE BUCKET REC                            
         BE    ESTX                                                             
         LA    R5,BUDCNT                                                        
         CLI   REC+3,X'18'       BUDGET RECORD                                  
         BE    ESTX                                                             
         LA    R5,AORCNT                                                        
         CLI   REC+3,X'14'       AOR RECORD                                     
         BE    ESTX                                                             
         LA    R5,BLFCNT                                                        
         CLI   REC+3,X'4C'       BILLING FORMULA                                
         BE    ESTX                                                             
         DC    H'0'         UNKNOWN RECORD TYPE                                 
*                                                                               
ESTX     AP    0(5,R5),=P'1'                                                    
         B     AGSW                                                             
*                                                                               
         SPACE 3                                                                
BILL     DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(6),REC+4      CLT AND PRD                                 
         MVC   WORK+9(2),PBILKEST                                               
*                                                                               
*                                                                               
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
         GOTO1 =V(BINSRCH),ESTPARS,(2,WORK)    SEE IF EST FOUND                 
         TM    ESTPARS,X'01'      NO - THEN SKIP                                
         BO    GET                                                              
*                                                                               
         GOTO1 =V(BINSRCH),ESTPARS,(1,WORK)                                     
         L     R8,ESTPARS                                                       
         LTR   R8,R8                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,ETBILLS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,ETBILLS                                                       
*                                                                               
         AP    BILLCNT,=P'1'                                                    
         B     AGSW                                                             
*                                                                               
         SPACE 3                                                                
BUY      DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(6),REC+4      CLT AND PRD                                 
         MVC   WORK+9(2),PBUYKEST                                               
*                                                                               
*                                                                               
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
         GOTO1 =V(BINSRCH),ESTPARS,(2,WORK)    SEE IF EST FOUND                 
         TM    ESTPARS,X'01'      NO - THEN SKIP                                
         BO    GET                                                              
*                                                                               
         GOTO1 =V(BINSRCH),ESTPARS,(1,WORK)                                     
         L     R8,ESTPARS                                                       
         LTR   R8,R8                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(GETINS),DMCB,REC,GROSS,REC+7                                  
*                                                                               
         TM    REC+27,X'80'                                                     
         BZ    *+10                                                             
         XC    GROSS(20),GROSS                                                  
         L     RF,ETBUYS                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ETBUYS                                                        
*                                                                               
         LM    R0,R2,ETBUYGRS                                                   
         A     R0,GROSS                                                         
         A     R1,AGYCOM                                                        
         A     R2,CSHDSC                                                        
         STM   R0,R2,ETBUYGRS                                                   
*                                                                               
         LM    R0,R2,ETPAYGRS                                                   
         A     R0,PGROSS                                                        
         A     R1,PAGYCOM                                                       
         A     R2,PCSHDSC                                                       
         STM   R0,R2,ETPAYGRS                                                   
*                                                                               
         LM    R0,R2,ETBILGRS                                                   
         A     R0,BGROSS                                                        
         A     R1,BAGYCOM                                                       
         A     R2,BCSCHDSC                                                      
         STM   R0,R2,ETBILGRS                                                   
*                                                                               
         AP    BUYCNT,=P'1'                                                     
         B     AGSW           GO SWITCH AGENCY CODES                            
*                                                                               
         EJECT                                                                  
SRCH     DS    0H                                                               
         MVC   X(11),WORK                                                       
SRCH2    DS    0H                                                               
         GOTO1 =V(BINSRCH),LKPARS,X                                             
*                                                                               
         CLI   LKPARS,0                                                         
         BER   R9                                                               
         CLI   X+9,X'FF'                                                        
         BE    *+14                                                             
         MVC   X+9(2),=2X'FF'                                                   
         B     SRCH2                                                            
         CLI   X+6,X'FF'                                                        
         BE    *+14                                                             
         MVC   X+6(3),=3X'FF'                                                   
         B     SRCH2                                                            
         CLI   X+3,X'FF'                                                        
         BE    *+14                                                             
         MVC   X+3(3),=3X'FF'                                                   
         B     SRCH2                                                            
*                                                                               
SRCH6    DS    0H                                                               
         CLI   LKPARS,0                                                         
         BR    R9                                                               
*                                                                               
*                                                                               
PUT      DS    0H                                                               
*                           LEAVE STATUS BYTES ALONE                            
PUTXX    BAS   RE,PUTREC                                                        
         B     GET                                                              
         SPACE 2                                                                
*&&DO                                                                           
EOF      CLOSE IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
*&&                                                                             
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
*                                  PRINT ESTIMATE LIST                          
         BAS   RE,SKIP                                                          
         MVC   P+31(18),=C'PRINTPAK DATA COPY'                                  
         MVC   P+71(8),TODAY                                                    
         BAS   RE,PRNT3                                                         
******   MVC   P+01(78),=C'                                           G         
******         ROSS       GROSS-CD       GROSS-CD'                              
******   BAS   RE,PRNT                                                          
         MVC   P+01(81),=C'AG M CLT PRD EST     BUYS   BILLS         ORX        
               DERED            PAID          BILLED'                           
         BAS   RE,PRNT                                                          
         MVC   P+01(81),=C'-- - --- --- ---     ----   -----         --X        
               -----            ----          ------'                           
         BAS   RE,PRNT2                                                         
         L     R8,ESTPARS+4                                                     
         L     R2,ESTPARS+8                                                     
         LTR   R2,R2                                                            
         BZ    EOF10                                                            
         LA    R1,RTOTS            RUN TOTALS                                   
         LA    R0,11                                                            
EOF2C    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,EOF2C                                                         
         LA    R1,MTOTS            AGY/MEDIA TOTALS                             
         LA    R0,11                                                            
EOF2E    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,EOF2E                                                         
         XC    SAVAMED,SAVAMED                                                  
*                                                                               
EOF3     DS    0H                                                               
         CLI   SAVAMED,0           FIRST MEDIA                                  
         BNE   EOF3C                                                            
         MVC   SAVAMED,ETAGY                                                    
EOF3C    CLC   SAVAMED,ETAGY       CHK FOR CHANGE OF AGY MEDIA                  
         BE    EOF3G                                                            
         ST    R8,FULL                                                          
         LA    R8,MTOTS                                                         
         MVC   P+1(11),=C'*A/M TOTAL*'                                          
         BAS   R9,EDTPK            PACKED EDIT                                  
         BAS   RE,PRNT                                                          
         MVC   SAVAMED,ETAGY                                                    
         LA    R1,MTOTS                                                         
         LA    R0,11                                                            
EOF3E    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,EOF3E                                                         
         L     R8,FULL             RESTORE R8                                   
*                                                                               
EOF3G    DS    0H                                                               
         LA    R3,ETBUYS                                                        
         LA    R0,11                                                            
         LA    R7,MTOTS                                                         
EOF3K    DS    0H                                                               
         L     RF,0(R3)                                                         
         CVD   RF,DUB                                                           
         AP    0(8,R7),DUB                                                      
         LA    R3,4(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R0,EOF3K                                                         
*                                                                               
         MVC   P+1(2),ETAGY                                                     
         MVC   P+4(1),ETMED                                                     
         MVC   P+6(3),ETCLT                                                     
         MVC   P+10(3),ETPRD                                                    
         EDIT  (B2,ETEST),(3,P+14)                                              
         BAS   R9,EDT                                                           
         LA    R3,ETBUYS                                                        
         LA    R0,11                                                            
         LA    R7,RTOTS                                                         
EOF4     DS    0H                                                               
         L     RF,0(R3)                                                         
         CVD   RF,DUB                                                           
         AP    0(8,R7),DUB                                                      
         LA    R3,4(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R0,EOF4                                                          
*                                                                               
         BAS   RE,PRNT                                                          
         LA    R8,ETABEL(R8)                                                    
         BCT   R2,EOF3                                                          
*                                                                               
         LA    R8,MTOTS            DO LAST AGY/MEDIA                            
         MVC   P+1(11),=C'*A/M TOTAL*'                                          
         BAS   R9,EDTPK                                                         
         BAS   RE,PRNT                                                          
*                                                                               
         MVC   P+1(11),=C'*RUN TOTAL*'                                          
         LA    R8,RTOTS                                                         
         BAS   R9,EDTPK                                                         
         BAS   RE,PRNT                                                          
EOF10    DS    0H                                                               
         B     EOJ                                                              
*                                                                               
         EJECT                                                                  
EDT      DS    0H                                                               
         MVI   P+35,C'G'                                                        
         EDIT  (B4,ETBUYS),(7,P+19),COMMAS=YES                                  
         EDIT  (B4,ETBILLS),(7,P+27),COMMAS=YES                                 
         L     R0,ETBUYGRS                                                      
         EDIT  (R0),(15,P+36),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETPAYGRS                                                      
         EDIT  (R0),(15,P+52),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETBILGRS                                                      
         EDIT  (R0),(15,P+68),2,COMMAS=YES,MINUS=YES                            
         BAS   RE,PRNT                                                          
         MVI   P+35,C'A'                                                        
         L     R0,ETBUYAC                                                       
         EDIT  (R0),(15,P+36),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETPAYAC                                                       
         EDIT  (R0),(15,P+52),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETBILAC                                                       
         EDIT  (R0),(15,P+68),2,COMMAS=YES,MINUS=YES                            
         BAS   RE,PRNT                                                          
         MVI   P+35,C'C'                                                        
         L     R0,ETBUYCD                                                       
         EDIT  (R0),(15,P+36),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETPAYCD                                                       
         EDIT  (R0),(15,P+52),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETBILCD                                                       
         EDIT  (R0),(15,P+68),2,COMMAS=YES,MINUS=YES                            
         BAS   RE,PRNT                                                          
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
EDTPK    DS    0H                                                               
         EDIT  (P8,0(R8)),(7,P+19),COMMAS=YES                                   
         EDIT  (P8,8(R8)),(7,P+27),COMMAS=YES                                   
         MVI   P+35,C'G'                                                        
         ZAP   MYDUB,16(8,R8)      ORD GRS                                      
         EDIT  MYDUB,(15,P+36),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,40(8,R8)      PAY GROSS                                    
         EDIT  MYDUB,(15,P+52),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,64(8,R8)      BILL GROSS                                   
         EDIT  MYDUB,(15,P+68),2,COMMAS=YES,MINUS=YES                           
         BAS   RE,PRNT                                                          
         MVI   P+35,C'A'                                                        
         ZAP   MYDUB,24(8,R8)      ORD AC                                       
         EDIT  MYDUB,(15,P+36),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,48(8,R8)      PAY AC                                       
         EDIT  MYDUB,(15,P+52),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,72(8,R8)      BILL AC                                      
         EDIT  MYDUB,(15,P+68),2,COMMAS=YES,MINUS=YES                           
         BAS   RE,PRNT                                                          
         MVI   P+35,C'C'                                                        
         ZAP   MYDUB,32(8,R8)      ORD CD                                       
         EDIT  MYDUB,(15,P+36),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,56(8,R8)      PAY CD                                       
         EDIT  MYDUB,(15,P+52),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,80(8,R8)      BILL CD                                      
         EDIT  MYDUB,(15,P+68),2,COMMAS=YES,MINUS=YES                           
         BAS   RE,PRNT                                                          
         BR    R9                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         SPACE 2                                                                
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
         SPACE 3                                                                
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
DMPREC   NTR1                                                                   
*                                                                               
         LA    R5,REC-4                                                         
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R2,4(R2)                                                         
         LA    R3,0(R5,R2)         EOR                                          
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   XIT                                                              
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 =V(HEXOUT),DMCB,(R5),WORK,(R4),=C'N'                             
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,PRNT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         B     XIT                                                              
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
*                                                                               
         LA    R5,REC                                                           
         LA    R2,KLEN                                                          
         GOTO1 =V(HEXOUT),DMCB,(R5),P+01,(R2),=C'N'                             
*                                                                               
         MVC   WORK(KLEN),0(R5)                                                 
         TR    WORK(KLEN),TRTAB                                                 
         MVC   P+75(KLEN),WORK                                                  
         B     XIT                                                              
         SPACE 3                                                                
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
*                                                                               
         CLI   REC+3,X'5A'          DIRECTORY ONLY - STANDARD SPACE             
         BE    GETRNXTA                                                         
         CLI   REC+3,X'E0'          DIRECTORY ONLY?                             
         BE    GETRNXTA                                                         
         CLI   REC+3,X'E1'          DIRECTORY ONLY?                             
         BE    GETRNXTA                                                         
         CLI   REC+3,X'3A'          DIRECTORY ONLY?                             
         BL    GETRNXT                                                          
         CLI   REC+3,X'3F'          DIRECTORY ONLY?                             
         BNH   GETRNXTA                                                         
*                                                                               
GETRNXT  MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0             EOR                                          
         B     GETRX                                                            
*                                                                               
GETRNXTA DS    0H               FOR DIRECTORY ONLY RECS                         
*                               NO NEED FOR END OF REC ZERO                     
GETRX    AP    INCNT,=P'1'                                                      
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
PUTREC   NTR1                                                                   
*                                                                               
         CLI   DMPSW,C'Y'                                                       
         BNE   PUTREC2                                                          
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         BNP   PUTREC2                                                          
         BAS   RE,DMPREC                                                        
PUTREC2  DS    0H                                                               
         CLI   REC+3,X'5A'       DIRECTORY ONLY - STANDARD SPACE                
         BE    PUTREC3                                                          
         CLI   REC+3,X'E0'       DIRECTORY ONLY                                 
         BE    PUTREC3                                                          
         CLI   REC+3,X'E1'       DIRECTORY ONLY                                 
         BE    PUTREC3                                                          
         CLI   REC+3,X'3A'       DIRECTORY ONLY - GROUP                         
         BL    PUTREC2X                                                         
         CLI   REC+3,X'3F'       DIRECTORY ONLY - GROUP                         
         BNH   PUTREC3                                                          
*                                                                               
PUTREC2X MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
*                                                                               
PUTREC3  PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
         B     XIT                                                              
         SPACE 3                                                                
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
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*&&DO                                                                           
IN       DTFMT BLKSIZE=32760,RECFORM=VARBLK,TYPEFLE=INPUT,             X        
               IOAREA1=IN1,DEVADDR=SYS010,FILABL=NO,WORKA=YES,         X        
               EOFADDR=EOF                                                      
*&&                                                                             
*&&OS                                                                           
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*&&                                                                             
         EJECT                                                                  
*&&DO                                                                           
OUT      DTFMT BLKSIZE=32760,RECFORM=VARBLK,TYPEFLE=OUTPUT,            X        
               IOAREA1=OUT1,DEVADDR=SYS011,FILABL=NO,WORKA=YES                  
*&&                                                                             
*&&OS                                                                           
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
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
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
LKPARS   DS    6F                                                               
ESTPARS  DS    6F                                                               
TODAY    DS    CL8                                                              
PRTSW    DS    CL1                                                              
RTOTS    DS    11PL8               RUN TOTALS                                   
MTOTS    DS    11PL8               AGY MEDIA TOTALS                             
SAVAMED  DS    CL3                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
CLTCNT   DC    PL5'0',CL20'CLIENTS'                                             
PRDCNT   DC    PL5'0',CL20'PRODUCTS'                                            
ESTCNT   DC    PL5'0',CL20'ESTIMATES'                                           
BILLCNT  DC    PL5'0',CL20'BILLING HEADERS'                                     
BUYCNT   DC    PL5'0',CL20'INSERTION RECORDS'                                   
BUCCNT   DC    PL5'0',CL20'ESTIMATE BUCKETS'                                    
CONCNT   DC    PL5'0',CL20'CONTRACTS'                                           
BUDCNT   DC    PL5'0',CL20'BUDGETS'                                             
FSICNT   DC    PL5'0',CL20'FSI RECORDS'                                         
ISSCNT   DC    PL5'0',CL20'ISSUE DATE RECORDS'                                  
CURCNT   DC    PL5'0',CL20'CU RECORDS'                                          
SPCCNT   DC    PL5'0',CL20'SPACE RECORDS'                                       
AORCNT   DC    PL5'0',CL20'AOR RECORDS'                                         
BLFCNT   DC    PL5'0',CL20'BILLING FORMULA RECS'                                
COMMCNT  DC    PL5'0',CL20'COMMENTS'                                            
NVTCNT   DC    PL5'0',CL20'NV TEXT'                                             
JOBCNT   DC    PL5'0',CL20'JOB RECORDS'                                         
PBLCNT   DC    PL5'0',CL20'PUB LISTS'                                           
REPCNT   DC    PL5'0',CL20'REPS'                                                
DIVCNT   DC    PL5'0',CL20'DIVISIONS'                                           
REGCNT   DC    PL5'0',CL20'REGIONS'                                             
DSTCNT   DC    PL5'0',CL20'DISTRICTS'                                           
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    4000C                                                            
         DS    D                                                                
*&&DO                                                                           
IN1      DS    8500C                                                            
*&&                                                                             
*&&DO                                                                           
OUT1     DS    8500C                                                            
*&&                                                                             
         SPACE 3                                                                
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
ETABD    DSECT                                                                  
ETAGY    DS    CL2                                                              
ETMED    DS    CL1                                                              
ETCLT    DS    CL3                                                              
ETPRD    DS    CL3                                                              
ETEST    DS    XL2                                                              
ETESTSW  DS    CL1                                                              
ETBUYS   DS    F                                                                
ETBILLS  DS    F                                                                
ETBUYGRS DS    F                                                                
ETBUYAC  DS    F                                                                
ETBUYCD  DS    F                                                                
ETPAYGRS DS    F                                                                
ETPAYAC  DS    F                                                                
ETPAYCD  DS    F                                                                
ETBILGRS DS    F                                                                
ETBILAC  DS    F                                                                
ETBILCD  DS    F                                                                
ETABEL   EQU   *-ETABD                                                          
*                                                                               
ESTTAB   CSECT                                                                  
         DS    15000CL56            ROOM FOR 15000 ESTS                         
         DC    X'0000'                                                          
*                                                                               
LKTAB    CSECT                                                                  
         DS    400CL11                                                          
         DC    X'0000'                                                          
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046COPYPRT   09/19/03'                                      
         END                                                                    
