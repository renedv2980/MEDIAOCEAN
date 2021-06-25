*          DATA SET COPYOUG9   AT LEVEL 046 AS OF 08/12/02                      
*PHASE COPYPRTA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE GETINS                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'COPYPRT - COPY PRINTPAK DATA'                                   
*                                                                               
*     THIS PROGRAM WILL COPY CLIENT PRODUCT REP RECORDS FROM OU TO G9           
*                                                                               
*        X'02'   CLIENT                                                         
*        X'03'   DIVISIONS                                                      
*        X'04'   REGIONS                                                        
*        X'05'   DISTRICTS                                                      
*        X'06    PRODUCT                                                        
*        X'07'   ESTIMATES           (NO-OPPED FOR THIS RUN)                    
*        X'09'   ESTIMATE BUCKETS    (NO-OPPED FOR THIS RUN)                    
*        X'10'   CONTRACTS           (NO-OPPED FOR THIS RUN)                    
*        X'11'   REPS                                                           
*        X'15'   JOB RECORDS         (NO-OPPED FOR THIS RUN)                    
*        X'17'   PUBLISTS            (NO-OPPED FOR THIS RUN)                    
*        X'20'   BUYS                (NO-OPPED FOR THIS RUN)                    
*        X'40'   STANDARD COMMENT    (NO-OPPED FOR THIS RUN)                    
*                                                                               
*                                                                               
*        SINCE THE JOB RECORD CODE IS X'15' AND THE BUY IS X'20'                
*        JOBS ARE ENCOUNTERED BEFORE THE BUYS ARE READ                          
*        SO A PRELIMINARY RUN MUST BE DONE WHICH WILL LIST                      
*        THE JOBS TO BE ADDED TO A HARDCODED TABLE (ADRECTBL)                   
*        FOR THE "LIVE" RUN.                                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        FOR THIS RUN THE OUTPUT AGENCY IS CHANGED                              
*      CLIENT CODE OR CLIENT CODE/PRODUCT CODES ARE NOT CHANGED                 
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
*        1000 X 7                                                               
*                                                                               
         L     R4,=A(ESTTAB)                                                    
         XCEFL 0(R4),9000                                                       
*                                                                               
*        1000 X 9                                                               
*                                                                               
         L     R4,=A(ADTAB)                                                     
         XCEFL 0(R4),2600                                                       
*                                                                               
*        200 X 13                                                               
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
**********************************************************************          
**********************************************************************          
**********************************************************************          
**********************************************************************          
*                                                                               
* ADD CHECK FOR AGENCY BEING COPIED HERE                                        
*                                                                               
         CLC   REC(2),=C'OU'                                                    
         BH    EOF                 CAN STOP READING                             
*                                                                               
         CLC   REC(3),=C'OUI'      INTERACTIVE                                  
         BE    GET20                                                            
         CLC   REC(3),=C'OUM'      MAGAZINES                                    
         BE    GET20                                                            
         CLC   REC(3),=C'OUN'      NEWSPAPERS                                   
         BE    GET20                                                            
         CLC   REC(3),=C'OUO'      OUTDOOR                                      
         BE    GET20                                                            
         CLC   REC(3),=C'OUS'      SUPPLEMENT                                   
         BE    GET20                                                            
         CLC   REC(3),=C'OUT'      TRADE                                        
         BE    GET20                                                            
         B     GET                                                              
*                                                                               
GET20    DS    0H                                                               
*                                                                               
         CLC   REC+4(3),=C'CND'                                                 
         BE    GET30                                                            
         CLC   REC+4(3),=C'QDC'                                                 
         BE    GET30                                                            
*        *                                                                      
         B     GET                                                              
*                                                                               
GET30    DS    0H                                                               
*                                                                               
         CLI   REC+3,X'02'         CLIENT                                       
         BE    CLIENT                                                           
         CLI   REC+3,X'03'         DIVISION                                     
         BE    DIV                                                              
         CLI   REC+3,X'04'         REGION                                       
         BE    REG                                                              
         CLI   REC+3,X'05'         DISTRICT                                     
         BE    DST                                                              
         CLI   REC+3,X'06'         PRODUCT                                      
         BE    PRODUCT                                                          
         CLI   REC+3,X'11'         REPS                                         
         BE    REPS                                                             
         B     GET                                                              
*        *                                                                      
CLIENT   DS    0H                    CLIENT                                     
         GOTO1 =V(HELLO),DMCB,(C'D',=CL8'PRTFILE'),(X'10',REC),0,0              
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HELLO),DMCB,(C'D',=CL8'PRTFILE'),(X'11',REC),0,0              
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    CLICNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLSW                GO SWITCH CLIENTS ONLY                       
*                                                                               
*                                                                               
DIV      DS    0H                    DIVISIONS                                  
         AP    DIVCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLSW                GO SWITCH CLIENTS ONLY                       
*                                                                               
REG      DS    0H                    REGIONS                                    
         AP    REGCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLSW                GO SWITCH CLIENTS ONLY                       
*                                                                               
DST      DS    0H                    DISTRICTS                                  
         AP    DSTCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLSW                GO SWITCH CLIENTS ONLY                       
*                                                                               
PRODUCT  DS    0H                    PRODUCT                                    
         AP    PRDCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLSW                GO SWITCH CLIENTS ONLY                       
*                                                                               
*                                                                               
REPS     DS    0H                    REPS                                       
         AP    REPCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLSW                GO SWITCH CLIENTS ONLY                       
*        *                                                                      
*        *                                                                      
*NOP*    CLI   REC+3,X'03'         DIVISIONS                                    
*NOP*    BE    GET25                                                            
*NOP*    CLI   REC+3,X'04'         REGIONS                                      
*NOP*    BE    GET25                                                            
*NOP*    CLI   REC+3,X'05'         DISTRICTS                                    
*NOP*    BE    GET25                                                            
*NOP*    CLI   REC+3,X'17'         PUBLISTS                                     
*NOP*    BE    GET25                                                            
*NOP*                                                                           
*                                                                               
*      NOT A DIVISION, REGION, DISTRICT OR PUBLIST RECORD                       
*                                                                               
* CHECK FOR BOTH CLT/PRD HERE - BYPASS THOSE NOT ON TABLE (CLPRDTBL)            
*                                                                               
         LA    RE,CLPRDTBL                                                      
GET20F   CLI   0(RE),X'FF'         END OF TABLE ?                               
         BE    GET                                                              
         CLC   REC+4(6),0(RE)      CLT/PRD CODES MATCH ?                        
         BE    GET20X              YES - SAVE "REPLACEMENTS"                    
         LA    RE,12(RE)           BUMP TO NEXT ENTRY                           
         B     GET20F                                                           
GET20X   DS    0H                                                               
         MVC   NEWCODE,6(RE)       SAVE "REPLACEMENT" CLT/PRD CODES             
         B     GET30               GO PROCESS                                   
*                                                                               
GET25    DS    0H                                                               
*                                                                               
* CHECK FOR CLT CODE ONLY HERE - BYPASS THOSE NOT ON TABLE (CLPRDTBL)           
*                                                                               
         LA    RE,CLPRDTBL                                                      
GET25F   CLI   0(RE),X'FF'         END OF TABLE ?                               
         BE    GET                 YES - NOT FOUND                              
         CLC   REC+4(3),0(RE)      CLIENT CODE MATCH ?                          
         BE    GET25X              YES - SAVE "REPLACEMENTS"                    
         LA    RE,12(RE)           BUMP TO NEXT ENTRY                           
         B     GET25F                                                           
GET25X   DS    0H                                                               
         MVC   NEWCODE,6(RE)       SAVE "REPLACEMENT" CLIENT CODE               
*                                                                               
*ET30    DS    0H                                                               
*                                                                               
* THESE RECORD(S) HAVE SPECIAL ROUTINES                                         
*                                                                               
*                                                                               
*                                                                               
*NOP*    CLI   REC+3,X'03'         DIVISIONS                                    
*NOP*    BE    DIV                                                              
*NOP*    CLI   REC+3,X'04'         REGIONS                                      
*NOP*    BE    REG                                                              
*NOP*    CLI   REC+3,X'05'         DISTRICTS                                    
*NOP*    BE    DST                                                              
*                                                                               
*NOP*    CLI   REC+3,X'07'         ESTIMATES                                    
*NOP*    BE    EST                                                              
*                                                                               
*NOP*    CLI   REC+3,X'09'         ESTIMATE BUCKETS                             
*NOP*    BE    ESTB                                                             
*                                                                               
*NOP*    CLI   REC+3,X'10'         CONTRACTS                                    
*NOP*    BE    CON                                                              
*                                                                               
*NOP*    CLI   REC+3,X'11'         REPS                                         
*NOP*    BE    REP                                                              
*                                                                               
*NOP*    CLI   REC+3,X'15'         JOB RECORDS                                  
*NOP*    BE    JOB                                                              
*                                                                               
*NOP*    CLI   REC+3,X'17'         PUBLISTS                                     
*NOP*    BE    PUBL                                                             
*                                                                               
*NOP*    CLI   REC+3,X'20'         BUYS                                         
*NOP*    BE    BUY                                                              
*                                                                               
*NOP*    CLI   REC+3,X'40'         STANDARD COMMENTS                            
*NOP*    BE    COM                                                              
*                                                                               
         B     GET                SKIP OTHER RECORD TYPES                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*DIV      DS    0H                    DIVISIONS                                 
*        AP    DIVCNT,=P'1'                                                     
*NOP*    B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLSW                GO SWITCH CLIENTS ONLY                       
*                                                                               
*REG      DS    0H                    REGIONS                                   
*        AP    REGCNT,=P'1'                                                     
*NOP*    B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLSW                GO SWITCH CLIENTS ONLY                       
*                                                                               
*DST      DS    0H                    DISTRICTS                                 
*        AP    DSTCNT,=P'1'                                                     
*NOP*    B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLSW                GO SWITCH CLIENTS ONLY                       
*                                                                               
PUBL     DS    0H                  PUBLISTS                                     
*        AP    PUBLCNT,=P'1'                                                    
*NOP*    B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLSW                GO SWITCH CLIENTS ONLY                       
*                                                                               
*REP      DS    0H                    REPS                                      
*         AP    REPCNT,=P'1'                                                    
*         B     AGSW                JUST GO SWITCH AGENCYS                      
*                                                                               
EST      DS    0H                                                               
*                                                                               
*        LA    R2,REC                                                           
*        USING PESTREC,R2                                                       
*                                                                               
*        CLC   PESTST(6),=X'FAF0F1F2F3F1'   MUST START IN 2001                  
*        CLC   PESTEND(6),=X'FAF0F1F2F3F1'  MUST END   IN 2001                  
*        BNH   GET                 (AFTER 12/31/00)                             
*                                  ADD TO ESTIMATE TABLE                        
*                                                                               
*        XC    WORK,WORK                                                        
*        MVC   WORK(1),REC+2       MEDIA                                        
*        MVC   WORK+1(8),REC+4     CLT/PRD/EST                                  
*********                                                                       
*        LA    R1,COPYCTAB                                                      
COPYCH   CLI   0(R1),X'FF'                                                      
*        BE    GET                                                              
*        CLC   WORK(9),0(R1)                                                    
*        BE    CONTINUE                                                         
*        AHI   R1,COPYCEQU                                                      
*        B     COPYCH                                                           
*********                                                                       
*********                                                                       
*        LA    R1,DNTCPTAB         R1 AT THE START OF THE TABLE                 
*DNTCP   CLI   0(R1),X'FF'         CHECK IF R1 AT THE END                       
*        BE    CONTINUE            IF IT IS CONTINUE                            
*        CLC   WORK(9),0(R1)       CHECK IF IT MATCHED                          
*        BE    GET                 IF YES START ALL OVER                        
*        AHI   R1,DNTCPEQU         BUMP TO NEXT                                 
*        B     DNTCP               DNTCP MEANS DON'T COPY                       
*********                                                                       
*                                                                               
CONTINUE GOTO1 =V(BINSRCH),ESTPARS,(X'01',WORK)                                 
*        OC    0(4,R1),0(R1)                                                    
*        BNZ   *+6                                                              
*        DC    H'0'                TABLE IS FULL                                
*                                                                               
*        AP    ESTCNT,=P'1'                                                     
*                                  ADD TO COMMENT TABLE                         
*NOP*    CLC   PESTCOM,=C'      '                                               
*NOP*    BNH   EST10                                                            
*NOP*    XC    WORK,WORK                                                        
*NOP*    MVC   WORK(1),REC+2       MEDIA                                        
*NOP*    MVC   WORK+1(6),PESTCOM   STD COMMENT CODE                             
*                                                                               
*NOP*    GOTO1 =V(BINSRCH),COMPARS,(X'01',WORK)                                 
*NOP*    OC    0(4,R1),0(R1)                                                    
*NOP*    BNZ   *+6                                                              
*NOP*    DC    H'0'                TABLE IS FULL                                
*                                                                               
*NOP*    AP    ECCNT,=P'1'                                                      
*                                                                               
EST10    DS    0H                                                               
*NOP*    CLC   PESTCOM2,=C'      '                                              
*NOP*    BNH   ESTX                                                             
*NOP*    XC    WORK,WORK                                                        
*NOP*    MVC   WORK(1),REC+2       MEDIA                                        
*NOP*    MVC   WORK+1(6),PESTCOM2  STD COMMENT CODE                             
*                                                                               
*NOP*    GOTO1 =V(BINSRCH),COMPARS,(X'01',WORK)                                 
*NOP*    OC    0(4,R1),0(R1)                                                    
*NOP*    BNZ   *+6                                                              
*NOP*    DC    H'0'                TABLE IS FULL                                
*                                                                               
*NOP*    AP    ECCNT,=P'1'                                                      
*                                                                               
ESTX     DS    0H                                                               
*NOP*    B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLPRSW              GO SWITCH CLIENTS/PRODUCTS                   
*                                                                               
*        DROP  R2                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ESTB     DS    0H                                                               
*                   SEE IF ESTIMATE BUCKET IS IN ESTIMATE TABLE                 
*        XC    WORK,WORK                                                        
*        MVC   WORK(1),REC+2       MEDIA                                        
*        MVC   WORK+1(8),REC+4     CLT/PRD/EST                                  
*                                                                               
*        GOTO1 =V(BINSRCH),ESTPARS,(X'00',WORK)                                 
*        CLI   0(R1),1             RECORD FOUND ?                               
*        BE    GET                 NO - SKIP                                    
*                                                                               
*        AP    ESTBCNT,=P'1'                                                    
*NOP*    B     AGSW                JUST GO SWITCH AGENCYS                       
*        B     CLPRSW              GO SWITCH CLIENTS/PRODUCTS                   
*                                                                               
BUY      DS    0H                                                               
*                                                                               
*        LA    R2,REC                                                           
*        USING PBUYREC,R2                                                       
*                                                                               
*        TM    PBUYCNTL,X'80'                                                   
*        BNZ   GET              SKIP DELETED/CLOSED OUT INSERTIONS              
*                                                                               
*        CLC   PBUYKDAT(3),=X'640C1F' MUST BE IN 2001 (AFTER DEC31/00)          
*        BNH   GET                 NO - SKIP THIS RECORD                        
*                                                                               
*                         SEE IF ESTIMATE IS IN TABLE                           
*        XC    WORK,WORK                                                        
*        MVC   WORK(1),REC+2      MEDIA                                         
*        MVC   WORK+1(6),REC+4     CLT AND PRODUCT                              
*        MVC   WORK+7(2),PBUYKEST                                               
*        GOTO1 =V(BINSRCH),ESTPARS,(X'00',WORK)                                 
*        CLI   0(R1),1        SEE IF IN TABLE                                   
*        BE    GET            NO - THEN SKIP                                    
*                                                                               
*        DROP  R2                                                               
*                                                                               
*        LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
*        MVI   ELCODE,X'25'        PAY ELEMENT                                  
*                                                                               
BUY05    BAS   RE,NEXTEL           FOUND ?                                      
*        BNE   BUY15               NO                                           
*        OC    2(3,R2),2(R2)       CHECK FOR DATE                               
*        BZ    BUY15                                                            
*        MVC   P+1(22),=C'*** PAID INSERTION ***'                               
*        BAS   RE,PRNT                                                          
*        BAS   RE,DMPKEY                                                        
*                                                                               
BUY15    LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
*        MVI   ELCODE,X'26'        BILL ELEMENT                                 
*                                                                               
NEXTELEM BAS   RE,NEXTEL           FOUND ?                                      
*        BNE   BUY15XX             NO                                           
*        OC    5(3,R2),5(R2)       CHECK FOR DATE                               
*        BZ    BUY15X                                                           
*        BAS   RE,PRNT                                                          
*        BAS   RE,DMPKEY           DUMP KEY                                     
*        BAS   RE,DMPELEM          DUMP ELEM IF DATE NON EMPTY                  
*        XC    5(3,R2),5(R2)                                                    
*                                                                               
BUY15X   MVC   2(3,R2),=C'VRS'                                                  
*        XC    8(15,R2),8(R2)                                                   
*        BAS   RE,DMPELEM                                                       
*        B     NEXTELEM                                                         
*                                                                               
BUY15XX  LA    R2,REC            RESET R2 TO REC                                
*        USING PBUYREC,R2                                                       
*        CLC   PBDJOB,=6C' '       ANY ADCODE PRESENT                           
*        BNH   BUY20               NO                                           
*                                                                               
BUY10    DS    0H                  ADD TO ADCODE TABLE                          
*        XC    WORK,WORK                                                        
*        MVC   WORK(1),REC+2       MEDIA                                        
*        MVC   WORK+1(6),REC+4     CLT/PRD                                      
*        MVC   WORK+7(6),PBDJOB    ADCODE                                       
*                                                                               
*        DROP  R2                                                               
*                                                                               
*        GOTO1 =V(BINSRCH),ADPARS,(X'01',WORK)                                  
*        OC    0(4,R1),0(R1)                                                    
*        BNZ   *+6                                                              
*        DC    H'0'                TABLE IS FULL                                
*                                                                               
*        AP    ADCNT,=P'1'                                                      
*                                                                               
BUY20    DS    0H                                                               
*NOP*    LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
*NOP*    MVI   ELCODE,X'67'        COMMENT ELEM                                 
*                                                                               
*NOP*BUY25    BAS   RE,NEXTEL           FOUND ?                                 
*NOP*    BNE   BUY30               NO                                           
*                                                                               
*NOP*    CLC   =C'COM=',2(R2)      STANDARD COMMENT ?                           
*NOP*    BNE   BUY25               NO - TEST FOR MORE                           
*                                  ADD TO COMMENT TABLE                         
*NOP*    XC    WORK,WORK                                                        
*NOP*    MVC   WORK(1),REC+2       MEDIA                                        
*NOP*    MVC   WORK+1(6),6(R2)     STD COMMENT CODE                             
*                                                                               
*NOP*    GOTO1 =V(BINSRCH),COMPARS,(X'01',WORK)                                 
*NOP*    OC    0(4,R1),0(R1)                                                    
*NOP*    BNZ   *+6                                                              
*NOP*    DC    H'0'                TABLE IS FULL                                
*                                                                               
*NOP*    AP    COCNT,=P'1'                                                      
*                                                                               
*NOP*    B     BUY25               TEST FOR MORE                                
*                                                                               
BUY30    DS    0H                                                               
*NOP*    LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
*NOP*    MVI   ELCODE,X'68'        COMMENT ELEM                                 
*                                                                               
*NOP*BUY35    BAS   RE,NEXTEL           FOUND ?                                 
*NOP*    BNE   BUY50               NO - DONE WITH COMMENTS                      
*                                                                               
*NOP*    CLC   =C'COM=',2(R2)      STANDARD COMMENT ?                           
*NOP*    BNE   BUY35               NO - TEST FOR MORE                           
*                                  ADD TO COMMENT TABLE                         
*NOP*    XC    WORK,WORK                                                        
*NOP*    MVC   WORK(1),REC+2       MEDIA                                        
*NOP*    MVC   WORK+1(6),6(R2)     STD COMMENT CODE                             
*                                                                               
*NOP*    GOTO1 =V(BINSRCH),COMPARS,(X'01',WORK)                                 
*NOP*    OC    0(4,R1),0(R1)                                                    
*NOP*    BNZ   *+6                                                              
*NOP*    DC    H'0'                TABLE IS FULL                                
*                                                                               
*NOP*    AP    COCNT,=P'1'                                                      
*                                                                               
*NOP*    B     BUY35               TEST FOR MORE                                
*                                                                               
BUY50    DS    0H                                                               
***  ACCUMULATE GROSS, PAID AND BILLED DOLLARS FOR COPIED BUYS  ***             
*        GOTO1 =V(GETINS),DMCB,REC,PVALUES,REC+7                                
*        L     RE,GROSS                                                         
*        CVD   RE,DUB                                                           
*        AP    BUYGRS,DUB                                                       
*        L     RE,AGYCOM                                                        
*        CVD   RE,DUB                                                           
*        AP    BUYAGY,DUB                                                       
*        L     RE,CSHDSC                                                        
*        CVD   RE,DUB                                                           
*        AP    BUYDSC,DUB                                                       
*        L     RE,PGROSS                                                        
*        CVD   RE,DUB                                                           
*        AP    BUYPGRS,DUB                                                      
*        L     RE,PAGYCOM                                                       
*        CVD   RE,DUB                                                           
*        AP    BUYPAGY,DUB                                                      
*        L     RE,PCSHDSC                                                       
*        CVD   RE,DUB                                                           
*        AP    BUYPDSC,DUB                                                      
*        L     RE,BGROSS                                                        
*        CVD   RE,DUB                                                           
*        AP    BUYBGRS,DUB                                                      
*        L     RE,BAGYCOM                                                       
*        CVD   RE,DUB                                                           
*        AP    BUYBAGY,DUB                                                      
*        L     RE,BCSCHDSC                                                      
*        CVD   RE,DUB                                                           
*        AP    BUYBDSC,DUB                                                      
*                                                                               
*        AP    BUYCNT,=P'1'                                                     
*NOP*    B     AGSW                JUST GO SWITCH AGENCYS                       
*        B     CLPRSW              GO SWITCH CLIENTS/PRODUCTS                   
****************************************************************                
*                                                                               
CON      DS    0H                                                               
*                                                                               
*        LA    R2,REC                                                           
*        USING PCONREC,R2                                                       
*        CLC   PCONEDT(3),=X'640C1F' MUST BE IN 2001 (AFTER DEC31/00)           
*        BNH   GET                 NO - SKIP THIS RECORD                        
*                                                                               
*        DROP  R2                                                               
*                                                                               
CON20    DS    0H                                                               
*NOP*    LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
*NOP*    MVI   ELCODE,X'30'        STANDARD COMMENT ELEMENT                     
*                                                                               
*NOP*CON25    BAS   RE,NEXTEL           FOUND ?                                 
*NOP*    BNE   CON30               NO                                           
*                                  ADD TO COMMENT TABLE                         
*NOP*    XC    WORK,WORK                                                        
*NOP*    MVC   WORK(1),REC+2       MEDIA                                        
*NOP*    MVC   WORK+1(6),2(R2)     STD COMMENT CODE                             
*                                                                               
*NOP*    GOTO1 =V(BINSRCH),COMPARS,(X'01',WORK)                                 
*NOP*    OC    0(4,R1),0(R1)                                                    
*NOP*    BNZ   *+6                                                              
*NOP*    DC    H'0'                TABLE IS FULL                                
*                                                                               
*NOP*    AP    CCCNT,=P'1'                                                      
*                                                                               
*NOP*    B     CON25               TEST FOR MORE                                
*                                                                               
CON30    DS    0H                                                               
*        AP    CONCNT,=P'1'                                                     
*NOP*    B     AGSW                JUST GO SWITCH AGENCYS                       
*        B     CLPRSW              GO SWITCH CLIENTS/PRODUCTS                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
JOB      DS    0H                                                               
*        LA    R5,ADRECTBL                                                      
JOBLUP   CLI   0(R5),X'FF'         END OF TABLE ?                               
*        BE    GET                 YES - SKIP RECORD                            
*        CLC   REC(16),0(R5)                                                    
*        BE    JOBCPY                                                           
*        LA    R5,16(R5)           BUMP TO NEXT ENTRY                           
*        B     JOBLUP              TEST NEXT                                    
*                                                                               
JOBCPY   DS    0H                                                               
*        AP    JOBCNT,=P'1'                                                     
*        B     AGSW                JUST GO SWITCH AGENCYS                       
*NOP*    B     CLPRSW              GO SWITCH CLIENTS/PRODUCTS                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
COM      DS    0H                                                               
*                         SEE IF COMMENT IS IN TABLE                            
*        XC    WORK,WORK                                                        
*        MVC   WORK(1),REC+2      MEDIA                                         
*        MVC   WORK+1(6),REC+4     COMMENT CODE                                 
*        GOTO1 =V(BINSRCH),COMPARS,(X'00',WORK)                                 
*        CLI   0(R1),1                                                          
*        BE    GET                                                              
*        AP    COMCNT,=P'1'                                                     
*        B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*    ADD CODE HERE IF SWITCHING OTHER THAN AGENCY AS IN AGSW BELOW              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
AGSW     MVC   REC(2),=C'G9'       PENTAMARK INTERONE                           
         BAS   RE,DMPREC                                                        
         B     OUT90                                                            
*                                                                               
CLSW     MVC   REC+4(3),NEWCODE    CLIENT ONLY                                  
         B     OUT90                                                            
*                                                                               
CLPRSW   MVC   REC+4(6),NEWCODE    CLIENT AND PRODUCT                           
         B     OUT90                                                            
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
*        BAS   RE,PRNT                                                          
*        LA    R3,BUYDOLS                                                       
*        LA    R4,34                                                            
*        LA    R5,BUYDOLSX                                                      
*                                                                               
*EOF4     MVC   P+1(25),9(R3)                                                   
*        EDIT  (P9,0(R3)),(15,P+27),2,COMMAS=YES,CR=YES                         
*        BAS   RE,PRNT                                                          
*        BXLE  R3,R4,EOF4                                                       
*                                                                               
*        BAS   RE,PRNT                                                          
*        MVC   P+1(21),=C'ESTIMATES IN ESTTAB -'                                
*        EDIT  (B4,ESTPARS+8),(4,P+23)                                          
*        BAS   RE,PRNT                                                          
*        MVC   P+1(21),=C' COMMENTS IN COMTAB -'                                
*        EDIT  (B4,COMPARS+8),(4,P+23)                                          
*        BAS   RE,PRNT                                                          
*        MVC   P+1(21),=C'      JOBS IN ADTAB -'                                
*        EDIT  (B4,ADPARS+8),(4,P+23)                                           
*        BAS   RE,PRNT                                                          
*                                                                               
*                                                                               
*        L     R3,=A(ADTAB)                                                     
*        MVC   P+01(18),=C'JOB RECORDS FOLLOW'                                  
*        BAS   RE,PRNT                                                          
*        BAS   RE,PRNT                                                          
EOJADS   DS    0H                                                               
*        MVC   P+05(13),0(R3)                                                   
*        BAS   RE,PRNT                                                          
*        LA    R3,13(R3)            NEXT TABLE ENTRY                            
*        CLI   0(R3),0                                                          
*        BNE   EOJADS                                                           
*                                                                               
EOJCOMS  DS    0H                                                               
*        BAS   RE,PRNT       SKIP A LINE                                        
*        L     R3,=A(COMTAB)                                                    
*        MVC   P+01(18),=C'COMMENTS FOLLOW   '                                  
*        BAS   RE,PRNT                                                          
*        BAS   RE,PRNT                                                          
EOJCOM5  DS    0H                                                               
*        MVC   P+05(7),0(R3)                                                    
*        BAS   RE,PRNT                                                          
*        LA    R3,7(R3)            NEXT TABLE ENTRY                             
*        CLI   0(R3),0                                                          
*        BNE   EOJCOM5                                                          
*                                                                               
EOJESTS  DS    0H                                                               
*        BAS   RE,PRNT       SKIP A LINE                                        
*        L     R3,=A(ESTTAB)                                                    
*        MVC   P+01(18),=C'ESTIMATES FOLLOW '                                   
*        BAS   RE,PRNT                                                          
*        BAS   RE,PRNT                                                          
EOJEST5  DS    0H                                                               
*        MVC   P+05(1),0(R3)     MEDIA                                          
*        MVC   P+07(3),1(R3)     CLIENT                                         
*        MVC   P+11(3),4(R3)     PRODUCT                                        
*        MVC   HALF,7(R3)        ESTIMATE                                       
*        LH    R0,HALF                                                          
*        CVD   R0,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  P+15(3),DUB+5(3)                                                 
*        BAS   RE,PRNT                                                          
*        LA    R3,9(R3)         NEXT TABLE ENTRY                                
*        CLI   0(R3),0                                                          
*        BNE   EOJEST5                                                          
*        B     EOJ                                                              
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
DMPKEY   NTR1                     DUMP KEY                                      
*                                                                               
         LA    R5,REC                                                           
         LA    R3,KLEN                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'KEY',(R5),C'DUMP',(R3),=C'1D'                 
*                                                                               
*                                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMPELEM  NTR1                          DUMP ELEM                                
*                                                                               
         LA    R4,ELEN                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'ELEM',(R2),C'DUMP',(R4),=C'1H'                
*                                                                               
         B     XIT                                                              
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
*NOP*    BAS   RE,DMPKEY                                                        
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
DMDM     DS    CL6                                                              
         DS    0F                                                               
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
ELEN     EQU   23                                                               
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
NEWCODE  DS    CL6    "REPLACEMENT" CLT/PRD CODE FOR NEW RECORD                 
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
CLICNT   DC    PL5'0',CL20'CLIENT COUNT'                                        
DIVCNT   DC    PL5'0',CL20'DIVISIONS'                                           
REGCNT   DC    PL5'0',CL20'REGIONS'                                             
DSTCNT   DC    PL5'0',CL20'DISTRICTS'                                           
PRDCNT   DC    PL5'0',CL20'PRODUCT COUNT'                                       
REPCNT   DC    PL5'0',CL20'REPS COUNT'                                          
*NOOP*    PUBLCNT  DC    PL5'0',CL20'PUB LISTS'                                 
*NOOP*    REPCNT   DC    PL5'0',CL20'REPS'                                      
*NOOP*    ESTCNT   DC    PL5'0',CL20'ESTIMATES'                                 
*NOOP*    ESTBCNT  DC    PL5'0',CL20'ESTIMATE BUCKETS'                          
*NOOP*    JOBCNT   DC    PL5'0',CL20'AD RECORDS'                                
*NOOP*    ADCNT    DC    PL5'0',CL20'AD CODES IN BUYS'                          
*NOOP*    ECCNT    DC    PL5'0',CL20'COMMENTS IN ESTS'                          
*NOOP*    CCCNT    DC    PL5'0',CL20'COMMS IN CONTRACTS'                        
*NOOP*    COCNT    DC    PL5'0',CL20'COMMENTS IN BUYS'                          
*NOOP*    BUYCNT   DC    PL5'0',CL20'BUYS'                                      
*NOOP*    CONCNT   DC    PL5'0',CL20'CONTRACTS'                                 
*NOOP*    COMCNT   DC    PL5'0',CL20'STANDARD COMMENTS'                         
*                                                                               
* REST OF COUNTERS SHOULD ALL BE ZERO IN THIS COPYPRT RUN                       
*                                                                               
* OTHER COUNTERS ADDED HERE WILL AUTOMATICALLY PRINT AT EOJ                     
*                                                                               
COUNTSX  EQU   *-1                                                              
*                                                                               
BUYDOLS  DS    0C                                                               
*                                                                               
BUYGRS   DC    PL9'0',CL25'GROSS ORDERED'                                       
BUYAGY   DC    PL9'0',CL25'AGENCY COMMISSION'                                   
BUYDSC   DC    PL9'0',CL25'CASH DISCOUNT'                                       
BUYPGRS  DC    PL9'0',CL25'GROSS PAID'                                          
BUYPAGY  DC    PL9'0',CL25'AGENCY COMMISSION PAID'                              
BUYPDSC  DC    PL9'0',CL25'CASH DISCOUNT PAID'                                  
BUYBGRS  DC    PL9'0',CL25'GROSS BILLED'                                        
BUYBAGY  DC    PL9'0',CL25'AGENCY COMMISSION BILLED'                            
BUYBDSC  DC    PL9'0',CL25'CASH DISCOUNT BILLED'                                
***************************************************************                 
****************************************************************                
* OTHER ACCUMULATORS ADDED HERE WILL AUTOMATICALLY PRINT AT EOJ                 
*                                                                               
BUYDOLSX EQU   *-1                                                              
*                                                                               
P        DC    CL133' '                                                         
*                                                                               
***************************************************************                 
*   PAID/BILLED ESTIMATES DON'T COPY TABLE                                      
*                                                                               
*                                                                               
DNTCPTAB DS    0H                                                               
**FORMAT DC    CL7'MCLTPND',X'EST'                                              
         DC    CL7'MCKNDA1',X'00AC'                                             
DNTCPSEC DC    CL7'OCKNDA1',X'00B3'                                             
         DC    CL7'OLPHVRS',X'00A9'                                             
         DC    CL7'TLPHCAR',X'00A5'                                             
         DC    CL7'OFPOVRS',X'00AA'                                             
         DC    CL7'OFPOVRS',X'00AF'                                             
         DC    CL7'TFPOVRS',X'00AC'                                             
*                                                                               
DNTCPEQU EQU   DNTCPSEC-DNTCPTAB                                                
*                                                                               
         DC    X'FF'                                                            
***************************************************************                 
***************************************************************                 
***************************************************************                 
*   CHANGE FROM JW TO H7                                                        
*                                                                               
*                                                                               
COPYCTAB DS    0H                                                               
**FORMAT DC    CL7'MCLTPND',X'EST'                                              
         DC    CL7'NFSPQUI',X'00CD'                                             
COPYCSEC DC    CL7'MFSPQUI',X'00CD'                                             
         DC    CL7'NFSPQUI',X'00CE'                                             
         DC    CL7'NFSPQUI',X'00CF'                                             
         DC    CL7'NFSPQUI',X'00D0'                                             
         DC    CL7'NFOXDWM',X'00B6'                                             
         DC    CL7'NFOXDWM',X'00B7'                                             
         DC    CL7'NFOXDWM',X'00B8'                                             
         DC    CL7'NFOXCAS',X'00DD'                                             
         DC    CL7'NFOXCAS',X'00DE'                                             
         DC    CL7'MFOXCAS',X'00DE'                                             
         DC    CL7'NFOXCAS',X'00DF'                                             
         DC    CL7'NFOXCAS',X'00E0'                                             
*                                                                               
COPYCEQU EQU   COPYCSEC-COPYCTAB                                                
*                                                                               
         DC    X'FF'                                                            
***************************************************************                 
***************************************************************                 
CLPRDTBL DS    0H                                                               
*******  DC    C'CLTPRDCLTPRD'  CURRENT CLT/PRD (6) & COPY CLT/PRD (6)          
*SAMPLE  DC    C'GP GPABP BPA'                                                  
         DC    C'CCIDA1DCNVRS'                                                  
         DC    C'CKNDA1DKCVRS'                                                  
         DC    C'CSODA1DSTVRS'                                                  
         DC    C'CSMDA1DSLVRS'                                                  
         DC    C'SAADA1DATVRS'                                                  
         DC    C'SCLDA1DCHVRS'                                                  
         DC    C'SDMDA1DDLVRS'                                                  
         DC    C'SHMDA1DHUVRS'                                                  
         DC    C'SMMDA1DMEVRS'                                                  
         DC    C'SNODA1DNWVRS'                                                  
         DC    C'SORDA1DORVRS'                                                  
         DC    C'SFLDA1DFLVRS'                                                  
         DC    C'CCMDA1DCTVRS'                                                  
         DC    C'CCODA1DCOVRS'                                                  
         DC    C'LDMDA1DDMVRS'                                                  
         DC    C'LDODA1DDOVRS'                                                  
         DC    C'LLODA1DLLVRS'                                                  
         DC    C'LPHDA1DPXVRS'                                                  
         DC    C'FPODA1DPOVRS'                                                  
         DC    C'LSDDA1DSDVRS'                                                  
         DC    C'FFMDA1DBAVRS'                                                  
         DC    C'FFODA1DNCVRS'                                                  
         DC    C'FAKDA1DAKVRS'                                                  
         DC    C'LHIDA1DHAVRS'                                                  
*                                                                               
         DC    C'CCIVRSDCNVRS'                                                  
         DC    C'CKNVRSDKCVRS'                                                  
         DC    C'CSOVRSDSTVRS'                                                  
         DC    C'CSMVRSDSLVRS'                                                  
         DC    C'SAAVRSDATVRS'                                                  
         DC    C'SCLVRSDCHVRS'                                                  
         DC    C'SDMVRSDDLVRS'                                                  
         DC    C'SHMVRSDHUVRS'                                                  
         DC    C'SMMVRSDMEVRS'                                                  
         DC    C'SNOVRSDNWVRS'                                                  
         DC    C'SORVRSDORVRS'                                                  
         DC    C'SFLVRSDFLVRS'                                                  
         DC    C'CCMVRSDCTVRS'                                                  
         DC    C'CCOVRSDCOVRS'                                                  
         DC    C'LDMVRSDDMVRS'                                                  
         DC    C'LDOVRSDDOVRS'                                                  
         DC    C'LLOVRSDLLVRS'                                                  
         DC    C'LPHVRSDPXVRS'                                                  
         DC    C'FPOVRSDPOVRS'                                                  
         DC    C'LSDVRSDSDVRS'                                                  
         DC    C'FFMVRSDBAVRS'                                                  
         DC    C'FFOVRSDNCVRS'                                                  
         DC    C'FAKVRSDAKVRS'                                                  
         DC    C'LHIVRSDHAVRS'                                                  
*                                                                               
         DC    C'LDMCARDDMVRS'                                                  
         DC    C'LDOCARDDOVRS'                                                  
         DC    C'LLOCARDLLVRS'                                                  
         DC    C'LPHCARDPXVRS'                                                  
         DC    C'FPOCARDPOVRS'                                                  
         DC    C'LSDCARDSDVRS'                                                  
         DC    C'FFMCARDBAVRS'                                                  
         DC    C'FFOCARDNCVRS'                                                  
         DC    C'FAKCARDAKVRS'                                                  
         DC    C'LSDCARDSDVRS'                                                  
         DC    C'LHICARDHAVRS'                                                  
         DC    C'SMMCARDMEVRS'                                                  
         DC    C'SNOCARDNWVRS'                                                  
*                                                                               
         DC    X'FF'                                                            
ADRECTBL DS    0H                                                               
*****    DC    C'AGM',X'15',C'CLTPRDJOB456'                                     
         DC    C'BDM',X'15',C'CCIDA1CCI   '                                     
         DC    C'BDM',X'15',C'CCMDA1CCM   '                                     
         DC    C'BDM',X'15',C'CCODA1CCO   '                                     
         DC    C'BDM',X'15',C'CKNDA1CKN   '                                     
         DC    C'BDM',X'15',C'CSMDA1CSM   '                                     
         DC    C'BDM',X'15',C'SDMDA1TBD   '                                     
         DC    C'BDM',X'15',C'SHMDA1TBD   '                                     
         DC    C'BDN',X'15',C'FAKVRSNL1111'                                     
         DC    C'BDN',X'15',C'SORDA1TBD   '                                     
         DC    C'BDO',X'15',C'CCIDA1CCI   '                                     
         DC    C'BDO',X'15',C'CCMDA1CCM   '                                     
         DC    C'BDO',X'15',C'CKNDA1CKN   '                                     
         DC    C'BDO',X'15',C'CSODA1CSO   '                                     
         DC    C'BDT',X'15',C'SORDA1000   '                                     
*                                                                               
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*          DATA SET PVALUES    AT LEVEL 005 AS OF 06/30/86                      
*                        *** OUTPUT PARAMETER BLOCK FOR GETINS ****             
PVALUES  DS    0F                                                               
*                                                                               
* ORDERED DATA                                                                  
*                                                                               
GROSS    DS    F                   GROSS ORDERED                                
AGYCOM   DS    F                   AGENCY COMMISSION                            
CSHDSC   DS    F                   CASH DISCOUNT                                
PYABLE   DS    F                   GROSS-AGYCOMM-CASHDSC                        
BLABLE   DS    F                   GROSS-CASH DSC                               
PREMIUM  DS    F                   (INCLUDED IN ABOVE FIELDS)                   
UNITS    DS    F                   NUMBER OF LINES BOUGHT                       
*                                                                               
***** NOTE ORDERED TAX UNDER PAID DATA                                          
* PAID DATA                                                                     
*                                                                               
PGROSS   DS    F                   GROSS PAID                                   
PAGYCOM  DS    F                   AGY COMM PAID                                
PCSHDSC  DS    F                   CASH DISCOUNT PAID                           
PAID     DS    F                   ACTUAL PAID AMOUNT                           
*                                                                               
TAX      DS    F                   ORDERED TAX - WAS PAYABLE DATE               
*                          (INCLUDED IN ORDERED GROSS,PYABLE,BLABLE)            
*                                  NET X PBDTAX (4 DECIMALS)                    
*                                                                               
* BILLED DATA                                                                   
*                                                                               
BGROSS   DS    F                   GROSS BILLED                                 
BAGYCOM  DS    F                   AGY COMM BILLED                              
BCSCHDSC DS    F                   CASH DISCOUNT BILLED                         
BILLED   DS    F                   ACTUAL BILLED AMOUNT                         
BLBLDT   DS    CL3                 BILLABLE DATE -YMD                           
*                                                                               
PVALUESX DS    0C                                                               
*                                                                               
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         DS    D                                                                
*                                                                               
**********RINT OFF                                                              
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
**PAN#1  DC    CL21'046COPYOUG9  08/12/02'                                      
         END                                                                    
