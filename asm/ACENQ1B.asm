*          DATA SET ACENQ1B    AT LEVEL 016 AS OF 03/16/20                      
*PHASE T6201BA                                                                  
                                                                                
T6201B   TITLE 'MCS ESTIMATES - DETAILS ENQUIRY'                                
                                                                                
***********************************************************************         
* LEVEL CHANGE DOCUMENTATION                                          *         
* --------------------------                                          *         
* TKLU 001 19MAY06 <DU01-5319> NEW ESTIMATE DETAILS ENQUIRY           *         
* TKLU 002 09JAN08 <DU01-7171> BUG FIX TO STATUS LINE                 *         
* TKLU 003 07JAN09 <LO01-8495> - ESTIMATE NAME TO BE OPTIONAL FIELD   *         
* TKLU 004 04FEB09 AGYCURR -> COMPCURR (AFFECTS GERMANY)              *         
* MPEN 005 28JAN09 <LO01-8281> RELINK DUE TO WORKING STORAGE CHANGE   *         
* SMAN 006 09OCT09 <DU01-8790> - M/F SUP FOR MERGED & INTERNAL APP    *         
* RGUP 016 16MAR20 DSRD-25860  - EMDELD EXTENDED LENGTH               *         
***********************************************************************         
* THIS HAS THE LATEST CHANGES FROM THE UK BUT IT IS NOT THE UK VERSION          
*                                                                               
                                                                                
T6201B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ENQ1B*,R8,CLEAR=YES,RR=RE                                    
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LHI   RC,OVERWORK-WORKD                                                
         LA    RC,WORKD(RC)        RC=A(LOCAL WORKIN STORAGE)                   
         USING OVERWRKD,RC                                                      
         ST    RE,ORELO                                                         
                                                                                
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
                                                                                
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN20              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERRXIT                                                           
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN100                                                          
                                                                                
MAIN20   MVC   IOKEY,CURRKEY       REREAD RECORD                                
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
                                                                                
         TM    DISPFLAG,NORECQ     NO ELEMENTS?                                 
         BO    MAINX                                                            
         TM    DISPFLAG,ALLREADQ   ALL ELEMENTS READ?                           
         BO    MAIN60              YES                                          
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    MAIN40              YES                                          
                                                                                
MAIN40   CLC   TSCURRNO,TSLSTREC   HAVE WE ALREADY GOT RECORD IN TSAR?          
         BH    MAIN80              NO                                           
                                                                                
MAIN60   GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     MAIN150                                                          
                                                                                
MAIN80   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BZ    *+6                                                              
         DC    H'0'                THIS CANNOT HAPPEN                           
         TM    DISPFLAG,ALLREADQ   HAVE ALL ELEMENTS BEEN READ?                 
         BZ    MAIN100             YES                                          
         B     MAINX                                                            
                                                                                
         USING ERDELD,R3                                                        
MAIN100  L     R3,CURRELEM         RESTORE CURRENT ELEMENT                      
         XR    R0,R0                                                            
                                                                                
MAIN110  CLI   ERDEL,ERDELQ                                                     
         BE    MAIN130                                                          
         CLI   ERDEL,0             EOR?                                         
         BE    MAIN120                                                          
         IC    R0,ERDLN                                                         
         AR    R3,R0                                                            
         B     MAIN110             NEXT ELEMENT                                 
                                                                                
MAIN120  BAS   RE,GETNXT           LOOK FOR NEXT RECORD                         
         BE    MAIN100                                                          
         OI    DISPFLAG,ALLREADQ                                                
         B     MAIN160                                                          
                                                                                
MAIN130  LR    RE,R3               SET NEXT ELEMENT'S ADDRESS                   
         IC    R0,ERDLN                                                         
         AR    RE,R0                                                            
         ST    RE,CURRELEM                                                      
                                                                                
         BAS   RE,FILTERD          ELEMENT WANTED?                              
         BNE   MAIN100             DO WE WANT TO KEEP THIS ELEMENT?             
                                                                                
         BAS   RE,BLDTSAR          BUILD TSAR RECORD FOR DATA LINE              
         BNE   ERRXIT                                                           
                                                                                
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   MAIN150                                                          
         ICM   RF,3,TSCURRNO       UPDATE TSAR RECORD COUNTER                   
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN80                                                           
                                                                                
MAIN150  MVC   KEYSAVE,CURRKEY                                                  
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN40                                                           
                                                                                
MAIN160  OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   *+12                                                             
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     MAINX                                                            
         TM    DISPFLAG,ALLREADQ                                                
         BO    MAINX                                                            
         OI    DISPFLAG,ALLREADQ   ALL RECORDS READ                             
                                                                                
MAINX    GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
         B     OKXIT                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
                                                                                
         USING FLDHDRD,R2                                                       
FSTDIS   NTR1                                                                   
                                                                                
         MVC   LSTCAT,SPACES                                                    
         MVC   LSTWCOD,SPACES                                                   
         MVI   TXTFLAG,0                                                        
*                                                                               
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         BAS   RE,GETSJL                                                        
         BNE   FSTDERR                                                          
*                                                                               
         CLI   FLDDATA,C'='        SEARCH?                                      
         JNE   FSTD00                                                           
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,EST,ACOMFACS,0                    
*                                                                               
FSTD00   CLI   FLDILEN,0           ERROR IF NO DATA IN KEY                      
         BH    FSTD02                                                           
         MVC   FVMSGNO,=AL2(AE$ENONS)                                           
         B     FSTDERR                                                          
*                                                                               
FSTD02   MVC   FVMSGNO,=AL2(AE$FLDTS)                                           
         CLC   FLDILEN,PCLILEN                                                  
         BL    FSTDERR             LENGTH < L'CLIENT = TOO SHORT                
         BH    FSTD04              LENGTH > L'CLIENT = CHECK PRODUCT            
         MVC   FVMSGNO,=AL2(AE$INCLI)                                           
         BAS   RE,VALCPJ           VALIDATE CLIENT                              
         BE    FSTD08              OK = CHECK FOR CONTRA                        
         B     FSTDERR             NG = ERROR                                   
*                                                                               
FSTD04   MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         CLI   FLDILEN,12                                                       
         BH    FSTDERR             LENGTH > L'ACCOUNT = TOO LONG                
         CLC   FLDILEN,PPROLEN                                                  
         BH    FSTD06              LENGTH > L'PRODUCT = CHECK JOB               
         MVC   FVMSGNO,=AL2(AE$INPRO)                                           
         BAS   RE,VALCPJ           VALIDATE PRODUCT                             
         BE    FSTD08              OK = CHECK FOR CONTRA                        
         CLC   FLDILEN,PPROLEN                                                  
         BNE   FSTDERR             LENGTH NOT = L'PRODUCT = ERROR               
*                                                                               
         CLI   FLDILEN,L'EGNPNUM                                                
         BNE   FSTD06              LENGTH NOT = L'ESTIMATE = JOB                
         MVC   FVMSGNO,=AL2(AE$ESTNE)                                           
         BAS   RE,VALNUM           VALIDATE FOR NUMBER                          
         BNE   FSTDERR             NG = ERROR                                   
         LA    R2,BASCACH          OK = CHECK FOR NO CONTRA                     
         ST    R2,FVADDR                                                        
         CLI   FLDILEN,0                                                        
         BE    FSTD16                                                           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     FSTDERR                                                          
*                                                                               
FSTD06   MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         CLC   FLDILEN,PJOBLEN                                                  
         BH    FSTDERR                                                          
         MVC   FVMSGNO,=AL2(AE$INJOB)                                           
         BAS   RE,VALCPJ                                                        
         BNE   FSTDERR                                                          
*                                                                               
FSTD08   LA    R2,BASCACH                                                       
         ST    R2,FVADDR                                                        
         CLI   FLDILEN,0                                                        
         BH    FSTD10                                                           
         MVC   FVMSGNO,=AL2(AE$MISEQ)                                           
         B     FSTDERR                                                          
*                                                                               
FSTD10   TM    FLDIIND,FINPNUM                                                  
         BNZ   FSTD12                                                           
         MVC   FVMSGNO,=AL2(AE$NONIF)                                           
         B     FSTDERR                                                          
*                                                                               
FSTD12   CLI   FLDILEN,3                                                        
         BNH   FSTD14                                                           
         MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         B     FSTDERR                                                          
*                                                                               
FSTD14   BAS   RE,VALLOC                                                        
         BE    FSTD16                                                           
         MVC   FVMSGNO,=AL2(AE$ESTNE)                                           
         B     FSTDERR                                                          
*                                                                               
         USING OFFALD,R1                                                        
         USING ESTRECD,R3                                                       
FSTD16   LA    R3,IOKEY            TEST LIMIT ACCESS                            
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,ESTKSOFF                                                
         OC    OFFALIMA,OFFALIMA                                                
         BZ    FSTD18                                                           
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         BE    FSTD18                                                           
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         B     FSTDERR                                                          
         DROP  R1                                                               
                                                                                
FSTD18   DS    0H                  GET MAIN ESTIMATE RECORD                     
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,AIO1                                                          
         LA    R4,ESTRFST                                                       
         USING EMDELD,R4                                                        
         CLI   EMDEL,EMDELQ        MUST BE FIRST ELEMENT ON MAIN RECORD         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZAP   ESTTOT,EMDAMT       ESTIMATE TOTAL AMOUNT                        
         ZAP   ESTFCT,EMDFCA       (FC TOTAL AMOUNT)                            
                                                                                
         MVI   TEMP,C' '           ESTIMATE DETAILS IN TEMP                     
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
                                                                                
         LA    R2,TEMP             - GLOBAL NUMBER                              
         MVC   0(L'MX@ESTNO,R2),MX@ESTNO                                        
         AHI   R2,L'MX@ESTNO                                                    
         BAS   RE,BUMPR2                                                        
         MVI   0(R2),C'='                                                       
         AHI   R2,1                                                             
         MVC   0(L'EMDGNO,R2),EMDGNO                                            
         AHI   R2,L'EMDGNO                                                      
         MVI   0(R2),C','                                                       
         AHI   R2,2                                                             
                                                                                
         DS    0H                  - CLIENT/PRODUCT/JOB AND LOCAL #             
         MVC   0(L'MX@CLPJO,R2),MX@CLPJO                                        
         AHI   R2,L'MX@CLPJO                                                    
         BAS   RE,BUMPR2                                                        
         MVI   0(R2),C'='                                                       
         AHI   R2,1                                                             
         MVC   0(L'ESTKCLI,R2),ESTKCLI                                          
         AHI   R2,L'ESTKCLI                                                     
         BAS   RE,BUMPR2                                                        
         MVI   0(R2),C'/'                                                       
         AHI   R2,1                                                             
         MVC   0(L'ESTKPRO,R2),ESTKPRO                                          
         AHI   R2,L'ESTKPRO                                                     
         BAS   RE,BUMPR2                                                        
         MVI   0(R2),C'/'                                                       
         AHI   R2,1                                                             
         MVC   0(L'ESTKJOB,R2),ESTKJOB                                          
         AHI   R2,L'ESTKJOB                                                     
         BAS   RE,BUMPR2                                                        
         AHI   R2,1                                                             
         MVI   0(R2),C'('                                                       
         AHI   R2,1                                                             
         EDIT  (B1,ESTKLNO),(3,0(R2)),ALIGN=LEFT                                
         AR    R2,R0                                                            
         MVI   0(R2),C')'                                                       
         AHI   R2,1                                                             
                                                                                
         CP    EMDTVR,PZERO        TVR TOTAL?                                   
         BE    FSTD20                                                           
         SHI   R2,1                                                             
         MVI   0(R2),C','                                                       
         AHI   R2,2                                                             
         MVC   0(4,R2),=C'TVR='                                                 
         AHI   R2,4                                                             
         CURED (P6,EMDTVR),(12,0(R2)),0,ALIGN=LEFT                              
         AR    R2,R0                                                            
         MVI   0(R2),C')'                                                       
         AHI   R2,1                                                             
                                                                                
FSTD20   LR    RF,R2               GET LENGTH OF STRING                         
         LA    R2,TEMP                                                          
         SR    RF,R2                                                            
         CHI   RF,L'ENQDAT1                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         SHI   RF,1                                                             
         LA    R2,ENQDAT1H         AND SET FOR DISPLAY                          
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),TEMP                                                  
         OI    FLDOIND,FOUTTRN                                                  
         AHI   R2,ENQDAT2H-ENQDAT1H                                             
         ST    R2,SAVER2                                                        
                                                                                
         MVI   TEMP,C' '           NEXT LINE                                    
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
                                                                                
         LA    R2,TEMP             - STATUS                                     
         MVC   0(L'MX@STT,R2),MX@STT                                            
         AHI   R2,L'MX@STT                                                      
         BAS   RE,BUMPR2                                                        
         MVI   0(R2),C'='                                                       
         AHI   R2,1                                                             
         TM    ESTRSTA2,ESTKMERG+ESTKSINA                                       
         BZ    FSTD21                                                           
         MVI   BYTE1,ESTKMERG                                                   
         TM    ESTRSTA2,ESTKMERG                                                
         BO    *+8                                                              
         MVI   BYTE1,ESTKSINA                                                   
         BAS   RE,SET2STA                                                       
         B     *+14                                                             
*                                                                               
FSTD21   MVC   BYTE1,ESTRSTA1                                                   
         BAS   RE,SETSTA                                                        
         BAS   RE,BUMPR2                                                        
         MVI   0(R2),C','                                                       
         AHI   R2,1                                                             
                                                                                
         DS    0H                  - TOTAL AMOUNT                               
         MVC   0(L'MX@ESTTO,R2),MX@ESTTO                                        
         AHI   R2,L'MX@ESTTO                                                    
         BAS   RE,BUMPR2                                                        
         MVI   0(R2),C'='                                                       
         AHI   R2,1                                                             
         CURED (P6,EMDAMT),(12,0(R2)),2,MINUS=YES,ZERO=YES,ALIGN=LEFT           
         AR    R2,R0                                                            
                                                                                
         CLC   EMDCUR,COMPCURR     CURRENCY?                                    
         BE    FSTD22                                                           
         MVI   0(R2),C','                                                       
         AHI   R2,2                                                             
                                                                                
         DS    0H                  - CURRENCY                                   
         MVC   0(L'MX@CURRY,R2),MX@CURRY                                        
         AHI   R2,L'MX@CURRY                                                    
         BAS   RE,BUMPR2                                                        
         MVI   0(R2),C'='                                                       
         AHI   R2,1                                                             
         MVC   0(3,R2),EMDCUR                                                   
         AHI   R2,3                                                             
                                                                                
FSTD22   LR    RF,R2               GET LENGTH OF STRING                         
         LA    R2,TEMP                                                          
         SR    RF,R2                                                            
         CHI   RF,L'ENQDAT1                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         SHI   RF,1                                                             
         L     R2,SAVER2           AND SET FOR DISPLAY                          
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),TEMP                                                  
         OI    FLDOIND,FOUTTRN                                                  
         AHI   R2,ENQDAT2H-ENQDAT1H                                             
         ST    R2,SAVER2                                                        
                                                                                
         MVI   TEMP,C' '           NEXT LINE                                    
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
                                                                                
         LA    R2,TEMP             - DESCRIPTION                                
         MVC   0(L'MX@ESTDS,R2),MX@ESTDS                                        
         AHI   R2,L'MX@ESTDS                                                    
         BAS   RE,BUMPR2                                                        
         MVI   0(R2),C'='                                                       
         AHI   R2,1                                                             
         LR    RF,R4                                                            
*        AHI   RF,EMDLNQ                                                        
         LLC   R0,EMDLN                                                         
         AR    RF,R0                                                            
         USING ENMELD,RF                                                        
         CLI   ENMEL,ENMELQ        NAME ELEMENT FOLLOWS MAIN ELEMENT            
         BE    FSTD22A                                                          
         LA    R1,1                                                             
         MVI   0(R2),C' '                                                       
         B     FSTD23                                                           
*                                                                               
FSTD22A  XR    R1,R1                                                            
         IC    R1,ENMLN                                                         
         SHI   R1,ENMLNQ+1                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),ENMNAME                                                  
         AHI   R1,1                                                             
         AR    R2,R1                                                            
                                                                                
FSTD23   LR    RF,R2               GET LENGTH OF STRING                         
         LA    R2,TEMP                                                          
         SR    RF,R2                                                            
         CHI   RF,L'ENQDAT1                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         SHI   RF,1                                                             
         L     R2,SAVER2           AND SET FOR DISPLAY                          
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),TEMP                                                  
         OI    FLDOIND,FOUTTRN                                                  
         AHI   R2,ENQDAT2H-ENQDAT1H                                             
         ST    R2,SAVER2                                                        
                                                                                
         L     R2,SAVER2           DISPLAY COLUMN HEADINGS                      
         MVC   FLDDATA(L'MX@ENH44),MX@ENH44                                     
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
                                                                                
         AHI   R2,ENQDAT2H-ENQDAT1H   GET SCREEN DIMENSIONS                     
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
                                                                                
         L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
                                                                                
         XC    CURTENT,CURTENT                                                  
         CLC   EMDCUR,COMPCURR     FC INVOLVED?                                 
         BE    FSTD24                                                           
                                                                                
         USING COMFACSD,R3                                                      
         L     R3,ACOMFACS                                                      
         GOTO1 CBLDCUR,DMCB,EMDCUR,(X'80',CURTENT),ACOMFACS                     
         CLI   0(R1),0                                                          
         BE    FSTD24                                                           
         DC    H'0'                                                             
                                                                                
         USING ESTRECD,R3                                                       
FSTD24   LA    R3,IOKEY            NOW READ FIRST DATA RECORD                   
                                                                                
DARK     USING ESTRECD,R1                                                       
         L     R1,AIO1                                                          
         MVC   ESTKEY,DARK.ESTKEY                                               
         MVI   ESTKSEQ,1                                                        
         DROP  DARK                                                             
                                                                                
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                MUST EXIST                                   
                                                                                
         L     R3,AIO1                                                          
         MVC   CURRKEY,IOKEY                                                    
         LA    R1,ESTRFST                                                       
         ST    R1,CURRELEM                                                      
                                                                                
FSTDX    CR    RB,RB                                                            
         B     XIT                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE GLOBAL NUMBER INPUT AND READ RECORD                 *         
***********************************************************************         
                                                                                
         USING EGNPASD,R3                                                       
VALNUM   NTR1                                                                   
                                                                                
         LA    R3,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,MYCO                                                     
         MVC   EGNPNUM,FLDDATA                                                  
         MVC   CURRKEY,IOKEY                                                    
                                                                                
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         BNE   VALNUMN                                                          
         CLC   CURRKEY(EGNPCLI-EGNPASD),IOKEY                                   
         BNE   VALNUMN                                                          
                                                                                
VALNUMY  CR    RB,RB                                                            
         B     *+6                                                              
VALNUMN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CLIENT/PRODUCT/JOB INPUT                            *         
***********************************************************************         
                                                                                
         USING ACTRECD,R3                                                       
VALCPJ   NTR1                                                                   
                                                                                
         LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,MYCO                                                     
         MVC   ACTKULA(2),=C'SJ'                                                
         XR    R1,R1                                                            
         IC    R1,FLDILEN                                                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FLDDATA                                               
                                                                                
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BNE   VALCPJN                                                          
                                                                                
         MVC   MYCPJACT,ACTKACT                                                 
                                                                                
VALCPJY  CR    RB,RB                                                            
         B     *+6                                                              
VALCPJN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LOCAL NUMBER AND READ RECORD                        *         
***********************************************************************         
                                                                                
         USING ESTRECD,R3                                                       
VALLOC   NTR1                                                                   
                                                                                
         XR    R1,R1                                                            
         IC    R1,FLDILEN                                                       
         LR    RF,R1                                                            
         MVI   FULL,C'0'                                                        
         MVC   FULL+1(L'FULL-1),FULL                                            
         LA    RE,FULL+L'FULL                                                   
         SR    RE,RF                                                            
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FLDDATA                                                  
         PACK  DUB,FULL                                                         
         ZAP   DUB,DUB                                                          
         CVB   R1,DUB                                                           
                                                                                
         LA    R3,IOKEY                                                         
         XC    ESTKEY,ESTKEY                                                    
         MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         MVC   ESTKCPY,MYCO                                                     
         MVC   ESTKCLI,SPACES                                                   
         MVC   ESTKPRO,SPACES                                                   
         MVC   ESTKJOB,SPACES                                                   
         STC   R1,ESTKLNO                                                       
                                                                                
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ESTKCLI(0),MYCPJACT                                              
         LA    RE,MYCPJACT+1(RE)                                                
         XR    R1,R1                                                            
         XR    RF,RF                                                            
         IC    R1,PCLILEN                                                       
         IC    RF,PPROLEN                                                       
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ESTKPRO(0),0(RE)                                                 
         AHI   RF,1                                                             
         AR    RE,RF                                                            
         LA    R1,L'ACTKACT                                                     
         XR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    R1,RF                                                            
         CHI   R1,L'ESTKJOB                                                     
         BNH   *+8                                                              
         LA    R1,L'ESTKJOB                                                     
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ESTKJOB(0),0(RE)                                                 
                                                                                
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BNE   VALLOCN                                                          
                                                                                
VALLOCY  CR    RB,RB                                                            
         B     *+6                                                              
VALLOCN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        BUMP R2 BACK TO LAST NON SPACE CHARACTER PLUS 1              *         
***********************************************************************         
                                                                                
BUMPR2   DS    0H                                                               
                                                                                
BUMPR2A  CLI   0(R2),C' '                                                       
         BH    BUMPR2B                                                          
         SHI   R2,1                                                             
         B     BUMPR2A                                                          
                                                                                
BUMPR2B  AHI   R2,1                                                             
                                                                                
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        SET STATUS                                                   *         
***********************************************************************         
                                                                                
SETSTA   DS    0H                                                               
         USING STATABD,R1                                                       
         LA    R1,STATAB                                                        
*                                                                               
SETSTA2  CLI   STATSTC,X'FF'                                                    
         BE    SETSTA4                                                          
         CLC   BYTE1,STATSTB                                                    
         BE    SETSTA4                                                          
         AHI   R1,STATLNQ                                                       
         B     SETSTA2                                                          
*                                                                               
*ETSTA4  XR    RF,RF                                                            
*        ICM   RF,3,1(R1)                                                       
*        LA    RF,DSMIX(RF)                                                     
SETSTA4  MVC   0(L'STATSTC,R2),STATSTC                                          
         AHI   R2,L'STATSTC                                                     
         BR    RE                                                               
*                                                                               
STATAB   DC    C'DE',AL1(ESTKLOGD),AL2(MX@DELD-DSMIX)                           
         DC    C'RE',AL1(ESTKREJE),AL2(MX@ESREJ-DSMIX)                          
         DC    C'CA',AL1(ESTKCAPP),AL2(MX@CAPPD-DSMIX)                          
         DC    C'SC',AL1(ESTKSUBM),AL2(MX@ESSUB-DSMIX)                          
         DC    C'IP',AL1(ESTKCREA),AL2(MX@INPRO-DSMIX)                          
         DC    C'IA',AL1(ESTKINTA),AL2(MX@INAPP-DSMIX)                          
         DC    XL1'FF',AL2(MX@UNKWN-DSMIX)                                      
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
*        SET STATUS FROM ESTKSTA2                                               
**********************************************************************          
                                                                                
SET2STA  DS    0H                                                               
         USING STATABD,R1                                                       
         LA    R1,STATAB2                                                       
*                                                                               
SET2STA2 CLI   STATSTC,X'FF'                                                    
         BE    SET2STA4                                                         
         CLC   BYTE1,STATSTB                                                    
         BE    SET2STA4                                                         
         AHI   R1,STATLNQ                                                       
         B     SET2STA2                                                         
*                                                                               
*ET2STA4 XR    RF,RF                                                            
*        ICM   RF,3,1(R1)                                                       
*        LA    RF,DSMIX(RF)                                                     
SET2STA4 MVC   0(L'STATSTC,R2),STATSTC                                          
         AHI   R2,L'STATSTC                                                     
         BR    RE                                                               
*                                                                               
STATAB2  DC    C'ME',AL1(ESTKMERG),AL2(MX@MRGED-DSMIX)                          
         DC    C'SI',AL1(ESTKSINA),AL2(MX@SIAPP-DSMIX)                          
         DC    XL1'FF',AL2(MX@UNKWN-DSMIX)                                      
                                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FILTER ERD ELEMENT                                           *         
***********************************************************************         
                                                                                
         USING OPTVALSD,R2                                                      
         USING ERDELD,R3                                                        
FILTERD  NTR1                                                                   
                                                                                
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
                                                                                
         CLI   ERDTYP,ERDTWDQ      W/C DATA                                     
         BNE   FILT05                                                           
         NI    TXTFLAG,X'FF'-X'11' RESET W/C AND ITEM TEXT FLAG                 
         MVC   LSTWCOD,ERDWCOD                                                  
         B     FILT50                                                           
                                                                                
FILT05   CLI   OEXP,C'Y'           IF EXPAND OPTION                             
         BNE   FILTREJX                                                         
         CLI   ERDTYP,ERDTIDQ      THEN ITEM DATA                               
         BNE   FILT10                                                           
         NI    TXTFLAG,X'FF'-X'10' RESET ITEM TEXT FLAG                         
         B     FILT50                                                           
                                                                                
FILT10   CLI   ERDTYP,ERDTHTQ      HEADER TEXT?                                 
         BNE   FILT15                                                           
         OC    OWCODE,OWCODE S                                                  
         BNZ   FILTREJX                                                         
         TM    TXTFLAG,X'08'       ALREADY PROCESSED?                           
         BNZ   FILTREJX                                                         
         OI    TXTFLAG,X'08'                                                    
         B     FILT50                                                           
                                                                                
FILT15   CLI   ERDTYP,ERDTFTQ      FOOTER TEXT?                                 
         BNE   FILT20                                                           
         OC    OWCODE,OWCODE                                                    
         BNZ   FILTREJX                                                         
         TM    TXTFLAG,X'80'       ALREADY PROCESSED?                           
         BNZ   FILTREJX                                                         
         OI    TXTFLAG,X'80'                                                    
         B     FILT50                                                           
                                                                                
FILT20   CLI   ERDTYP,ERDTWTQ      W/C TEXT?                                    
         BNE   FILT25                                                           
         TM    TXTFLAG,X'01'       ALREADY PROCESSED?                           
         BNZ   FILTREJX                                                         
         OI    TXTFLAG,X'01'                                                    
         B     FILT50                                                           
                                                                                
FILT25   CLI   ERDTYP,ERDTITQ      ITEM TEXT?                                   
         BNE   FILT30                                                           
         TM    TXTFLAG,X'10'       ALREADY PROCESSED?                           
         BNZ   FILTREJX                                                         
         OI    TXTFLAG,X'10'                                                    
         B     FILT50                                                           
                                                                                
FILT30   B     FILTREJX                                                         
                                                                                
FILT50   OC    OWCODE,OWCODE       W/C FILTER?                                  
         BZ    FILT60                                                           
         CLI   OWCODEFI,NEGFILTR                                                
         BE    FILT55                                                           
         CLC   LSTWCOD,OWCODEVL                                                 
         BNE   FILTREJX                                                         
         B     FILT60                                                           
                                                                                
FILT55   CLC   LSTWCOD,OWCODEVL                                                 
         BE    FILTREJX                                                         
         B     FILT60                                                           
                                                                                
FILT60   DS    0H                                                               
                                                                                
FILTX    CR    RB,RB                                                            
         B     XIT                                                              
FILTREJX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        GET NEXT ESTIMATE RECORD AND SET VALUES                      *         
***********************************************************************         
                                                                                
         USING ESTRECD,R3                                                       
GETNXT   NTR1                                                                   
                                                                                
         LA    R3,IOKEY                                                         
         MVC   ESTKEY,CURRKEY                                                   
         XR    R1,R1                                                            
         IC    R1,ESTKSEQ                                                       
         AHI   R1,1                                                             
         STC   R1,ESTKSEQ                                                       
         CLI   ESTKSEQ,0                                                        
         BE    GETNXTN                                                          
                                                                                
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BNE   GETNXTN                                                          
                                                                                
         L     R3,AIO1                                                          
         MVC   CURRKEY,IOKEY                                                    
         LA    R1,ESTRFST                                                       
         ST    R1,CURRELEM                                                      
                                                                                
GETNXTY  CR    RB,RB                                                            
         B     XIT                                                              
GETNXTN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        GET SJ LEDGER                                                *         
***********************************************************************         
                                                                                
         USING LDGRECD,R3                                                       
GETSJL   NTR1                                                                   
                                                                                
         USING LDGRECD,R3                                                       
         LA    R3,IOKEY           READ SJ LEDGER RECORD                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,MYCO                                                     
         MVC   LDGKUNT(2),=C'SJ'                                                
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    GETSJL1                                                          
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     GETSJLN                                                          
                                                                                
GETSJL1  GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    GETSJL2                                                          
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     ERRXIT                                                           
                                                                                
GETSJL2  L     R3,AIO1                                                          
         LA    R3,LDGRFST                                                       
         XR    R0,R0                                                            
                                                                                
         USING ACLELD,R3                                                        
GETSJL3  CLI   ACLEL,0                                                          
         BNE   GETSJL4                                                          
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     GETSJLN                                                          
                                                                                
GETSJL4  CLI   ACLEL,ACLELQ                                                     
         BE    GETSJL5                                                          
         IC    R0,ACLLN                                                         
         AR    R3,R0                                                            
         B     GETSJL3                                                          
                                                                                
GETSJL5  MVC   PCLILEN,ACLELLVA                                                 
         MVC   PPROLEN,ACLELLVB                                                 
         MVC   PJOBLEN,ACLELLVC                                                 
                                                                                
GETSJLY  CR    RB,RB                                                            
         B     XIT                                                              
GETSJLN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM                       *         
***********************************************************************         
                                                                                
         USING ERDELD,R3                                                        
BLDTSAR  NTR1                                                                   
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING TSARRECD,R2                                                      
         L     R2,ATSARREC                                                      
         LHI   RF,TSDLENQ                                                       
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R2,TSARDATA                                                      
                                                                                
         USING TSARDATD,R2                                                      
         CLI   ERDTYP,ERDTWDQ      W/C DATA?                                    
         BNE   BLDT10                                                           
                                                                                
         MVI   TSDFMT,TSDFWCQ                                                   
         MVC   TSDWCAT,ERDWCAT                                                  
         MVC   TSDWCOD,ERDWCOD                                                  
         MVC   TSDWNAM,ERDWNAM                                                  
*        MVC   TSDWVCD,ERDWVCO                                                  
         ZAP   TSDWAMT,ERDWAMT                                                  
         ZAP   TSDWCAM,ERDWCAM                                                  
         ZAP   TSDWCAF,ERDWCFC                                                  
         ZAP   TSDWVAM,ERDWVAM                                                  
         ZAP   TSDWVAF,ERDWVFC                                                  
         ZAP   TSDWNIC,ERDWNIC                                                  
         ZAP   TSDWNFC,ERDWNFC                                                  
         ZAP   TSDWFCA,ERDWFCA                                                  
         MVC   TSDWIND,ERDWIND                                                  
         B     BLDT90                                                           
                                                                                
BLDT10   CLI   ERDTYP,ERDTIDQ      ITEM DATA                                    
         BNE   BLDT20                                                           
                                                                                
         MVI   TSDFMT,TSDFIAQ                                                   
         MVC   TSDINUM,ERDICOD                                                  
         MVC   TSDIDES,ERDIDES                                                  
         ZAP   TSDIPRI,ERDIAPR                                                  
         ZAP   TSDIFCP,ERDIFPR                                                  
         ZAP   TSDINIC,ERDINIC                                                  
         ZAP   TSDINFC,ERDINFC                                                  
         ZAP   TSDIMUL,ERDIMUL                                                  
         MVC   TSDIIND,ERDIIND                                                  
         B     BLDT90                                                           
                                                                                
BLDT20   CLI   ERDTYP,ERDTHTQ      HEADER TEXT                                  
         BNE   BLDT30                                                           
                                                                                
         MVI   TSDFMT,TSDFHTQ                                                   
         LA    RF,TSDHTXT                                                       
         B     BLDT80                                                           
                                                                                
BLDT30   CLI   ERDTYP,ERDTFTQ      FOOTER TEXT                                  
         BNE   BLDT40                                                           
                                                                                
         MVI   TSDFMT,TSDFFTQ                                                   
         LA    RF,TSDHTXT                                                       
         B     BLDT80                                                           
                                                                                
BLDT40   CLI   ERDTYP,ERDTWTQ      W/C TEXT                                     
         BNE   BLDT50                                                           
                                                                                
         MVI   TSDFMT,TSDFWTQ                                                   
         LA    RF,TSDHTXT                                                       
         B     BLDT80                                                           
                                                                                
BLDT50   CLI   ERDTYP,ERDTITQ      ITEM TEXT                                    
         BNE   BLDT60                                                           
                                                                                
         MVI   TSDFMT,TSDFITQ                                                   
         LA    RF,TSDHTXT                                                       
         B     BLDT80                                                           
                                                                                
BLDT60   DC    H'0'                FILTERD ROUTINE BUG                          
                                                                                
BLDT80   XR    R1,R1               GENERAL TEXT HANDLING                        
         MVC   0(L'TSDHTXT,RF),SPACES                                           
         IC    R1,ERDLN                                                         
         SHI   R1,ERDTLNQ                                                       
         BM    BLDT90                                                           
         CHI   R1,L'TSDHTXT                                                     
         BNH   *+8                                                              
         LA    R1,L'TSDHTXT                                                     
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),ERDTEXT                                                  
                                                                                
BLDT90   BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
                                                                                
         MVC   TSDLINES,LINSUSED                                                
                                                                                
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    BLDT95                                                           
         LTR   RB,RB               TSAR BLOCK IS FULL                           
         B     BLDTX                                                            
                                                                                
BLDT95   MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         CR    RB,RB                                                            
                                                                                
BLDTX    B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
                                                                                
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR TSAR RECORD                            
         LHI   R1,DUMLINLN                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         USING TSARRECD,R3                                                      
         L     R3,ATSARREC         R4=A(TSAR RECORD )                           
                                                                                
         USING SCRLIN1D,R2         DSECT FOR NORMAL DATA LINE                   
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARDATD,R4                                                      
                                                                                
         CLI   TSDFMT,TSDFWCQ      W/C?                                         
         BNE   FORM40                                                           
                                                                                
         CLC   LSTCAT,SPACES       1ST TIME?                                    
         BE    FORM10                                                           
         CLC   LSTCAT,TSDWCAT      SAME AS PREVIOUS?                            
         BE    FORM15                                                           
                                                                                
FORM10   MVC   SCR1CAT,TSDWCAT                                                  
                                                                                
FORM15   MVC   LSTCAT,TSDWCAT                                                   
         MVC   SCR1WCD,TSDWCOD                                                  
         MVC   SCR1NAM,TSDWNAM                                                  
*&&UK*&& MVC   SCR1VCD,TSDWVCD                                                  
*&&UK*&& OC    CURTENT,CURTENT                                                  
*&&UK*&& BNZ   FORM20                                                           
         CURED (P6,TSDWAMT),(L'SCR1AMT,SCR1AMT),2,MINUS=YES,ZERO=YES            
*&&UK*&& CURED (P6,TSDWVAM),(L'SCR1VAT,SCR1VAT),2,MINUS=YES,ZERO=YES            
         B     FORM25                                                           
                                                                                
FORM20   CURED (P6,TSDWFCA),(L'SCR1AMT,SCR1AMT),CURTENT,CURSYMB=NO,    *        
               MINUS=YES,ZERO=YES                                               
*&&UK                                                                           
         CURED (P6,TSDWVAF),(L'SCR1VAT,SCR1VAT),CURTENT,CURSYMB=NO,    *        
               MINUS=YES,ZERO=YES                                               
*&&                                                                             
FORM25   TM    TSDWIND,ERDWIIQ                                                  
         BZ    *+8                                                              
         MVI   SCR1IND+0,C'I'                                                   
         TM    TSDWIND,ERDWICQ                                                  
         BZ    *+8                                                              
         MVI   SCR1IND+1,C'C'                                                   
*                                                                               
         L     RF,AOPTVALS                                                      
         CLI   OXDETAIL-OPTVALSD(RF),C'Y'     EXTRA DETAILS (FOR W/C)?          
         BNE   FORM35                                                           
         XR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         AHI   RF,1                                                             
         STC   RF,LINSUSED                                                      
         AHI   R2,L'DUMLIN1                                                     
         OC    CURTENT,CURTENT                                                  
         BNZ   FORM30                                                           
         ZAP   DUB,TSDWAMT                                                      
         AP    DUB,TSDWCAM                                                      
         CURED (P6,TSDWCAM),(L'SCR2COM,SCR2COM),2,MINUS=YES,ZERO=YES            
         CURED (P8,DUB),(L'SCR2GRS,SCR2GRS),2,MINUS=YES,ZERO=YES                
*&&UK*&& CURED (P6,TSDWNIC),(L'SCR2NIC,SCR2NIC),2,MINUS=YES,ZERO=YES            
         B     FORM35                                                           
                                                                                
FORM30   ZAP   DUB,TSDWFCA                                                      
         AP    DUB,TSDWCAF                                                      
         CURED (P6,TSDWCAF),(L'SCR2COM,SCR2COM),CURTENT,CURSYMB=NO,    *        
               MINUS=YES,ZERO=YES                                               
         CURED (P8,DUB),(L'SCR2GRS,SCR2GRS),CURTENT,CURSYMB=NO,        *        
               MINUS=YES,ZERO=YES                                               
         CURED (P6,TSDWNFC),(L'SCR2NIC,SCR2NIC),CURTENT,CURSYMB=NO,    *        
               MINUS=YES,ZERO=YES                                               
*                                                                               
FORM35   L     RF,AOPTVALS                                                      
         CLI   OEXP-OPTVALSD(RF),C'Y'    IF EXPAND OPTION (FOR ALL)?            
         BNE   FORM90                                                           
         B     FORM90                    (NO DATA YET)                          
                                                                                
FORM40   CLI   TSDFMT,TSDFIAQ      ITEM?                                        
         BNE   FORM60                                                           
                                                                                
         MVC   SCR3ITM,TSDINUM                                                  
         MVC   SCR3DES,TSDIDES                                                  
         OC    CURTENT,CURTENT                                                  
         BNZ   FORM50                                                           
         CURED (P6,TSDIPRI),(L'SCR3PRI,SCR3PRI),2,MINUS=YES,ZERO=YES            
         CURED (P6,TSDINIC),(L'SCR3NIC,SCR3NIC),2,MINUS=YES,ZERO=YES            
         B     FORM55                                                           
                                                                                
FORM50   CURED (P6,TSDIFCP),(L'SCR3PRI,SCR3PRI),CURTENT,CURSYMB=NO,    *        
               MINUS=YES,ZERO=YES                                               
         CURED (P6,TSDINFC),(L'SCR3NIC,SCR3NIC),CURTENT,CURSYMB=NO,    *        
               MINUS=YES,ZERO=YES                                               
                                                                                
FORM55   CURED (P6,TSDIMUL),(L'SCR3MUL,SCR3MUL),2,MINUS=YES,ZERO=YES            
                                                                                
         TM    TSDIIND,ERDIIAQ                                                  
         BZ    *+8                                                              
         MVI   SCR3IND+0,C'A'                                                   
         TM    TSDIIND,ERDIINQ                                                  
         BZ    *+8                                                              
         MVI   SCR3IND+1,C'N'                                                   
         B     FORM90                                                           
                                                                                
FORM60   CLI   TSDFMT,TSDFHTQ      HEADER TEXT?                                 
         BNE   FORM65                                                           
         MVC   SCRTEXT,TSDHTXT                                                  
         MVI   SCRTIND,C'H'                                                     
         B     FORM90                                                           
                                                                                
FORM65   CLI   TSDFMT,TSDFFTQ      FOOTER TEXT?                                 
         BNE   FORM70                                                           
         MVC   SCRTEXT,TSDFTXT                                                  
         MVI   SCRTIND,C'F'                                                     
         B     FORM90                                                           
                                                                                
FORM70   CLI   TSDFMT,TSDFWTQ      W/C TEXT?                                    
         BNE   FORM75                                                           
         MVC   SCRTEXT,TSDWTXT                                                  
         MVI   SCRTIND,C'W'                                                     
         B     FORM90                                                           
                                                                                
FORM75   CLI   TSDFMT,TSDFITQ      ITEM TEXT?                                   
         BNE   FORM80                                                           
         MVC   SCRTEXT,TSDITXT                                                  
         MVI   SCRTIND,C'I'                                                     
         B     FORM90                                                           
                                                                                
FORM80   DC    H'0'                UNDEFINED TYPE                               
                                                                                
FORM90   DS    0H                                                               
                                                                                
         XR    RF,RF                                                            
         IC    RF,LINSUSED         BUMP UP THE NUMBER OF LINES USED             
         AHI   RF,1                                                             
         STC   RF,LINSUSED         SAVE                                         
                                                                                
FORMX    B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
                                                                                
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
                                                                                
ERRXIT   LTR   RB,RB                                                            
         GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
                                                                                
XIT      XIT1  ,                                                                
         EJECT                                                                  
EST      DC    CL3'EST'                                                         
                                                                                
DCMIX    DS    0X                                                               
         DCDDL AC#ENH44,78                                                      
         DCDDL AC#ESTTO,L'MX@ESTTO                                              
         DCDDL AC#CURRY,L'MX@CURRY                                              
         DCDDL AC#STT,L'MX@STT                                                  
         DCDDL AC#ESTNO,L'MX@ESTNO                                              
         DCDDL AC#CLPJO,L'MX@CLPJO                                              
         DCDDL AC#ESTDS,L'MX@ESTDS                                              
         DCDDL AC#DELD,L'MX@DELD                                                
         DCDDL AC#ESREJ,L'MX@ESREJ                                              
         DCDDL AC#APRVD,L'MX@APRVD                                              
         DCDDL AC#ESSUB,L'MX@ESSUB                                              
         DCDDL AC#INPRO,L'MX@INPRO                                              
         DCDDL AC#UNKWN,L'MX@UNKWN                                              
         DCDDL AC#INAPP,L'MX@INAPP                                              
         DCDDL AC#MRGED,L'MX@MRGED                                              
         DCDDL AC#SIAPP,L'MX@SIAPP                                              
         DCDDL AC#CLIAP,L'MX@CAPPD                                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
                                                                                
OVERWRKD DSECT                                                                  
ORELO    DS    A                                                                
                                                                                
DSMIX    DS    0C                                                               
MX@ENH44 DS    CL78                                                             
MX@ESTTO DS    CL15                                                             
MX@CURRY DS    CL15                                                             
MX@STT   DS    CL8                                                              
MX@ESTNO DS    CL8                                                              
MX@CLPJO DS    CL20                                                             
MX@ESTDS DS    CL20                                                             
MX@DELD  DS    CL10                                                             
MX@ESREJ DS    CL10                                                             
MX@APRVD DS    CL10                                                             
MX@ESSUB DS    CL10                                                             
MX@INPRO DS    CL10                                                             
MX@UNKWN DS    CL10                                                             
MX@INAPP DS    CL20                                                             
MX@MRGED DS    CL20                                                             
MX@SIAPP DS    CL20                                                             
MX@CAPPD DS    CL20                                                             
         EJECT                                                                  
***********************************************************************         
*        DSECT                                                        *         
***********************************************************************         
                                                                                
SCRLIN1D DSECT                     COVER SCREEN W/C AND ITEM LINE               
SCRLINE  DS    0X                  * LINE 1                                     
SCR1CAT  DS    CL2                 CATEGORY                                     
         DS    XL1                                                              
SCR1WCD  DS    CL2                 WORK CODE                                    
         DS    XL1                                                              
SCR1NAM  DS    CL36                W/C NAME                                     
         DS    XL1                                                              
SCR1IND  DS    CL5                 W/C INDICATORS                               
         DS    XL1                                                              
SCR1VCD  DS    CL1                 VAT CODE                                     
         DS    XL1                                                              
SCR1VAT  DS    CL10                VAT AMOUNT                                   
         DS    XL1                                                              
SCR1AMT  DS    CL10                W/C AMOUNT                                   
         DS    XL1                                                              
         ORG   SCRLINE             * LINE 2                                     
         DS    XL6                                                              
SCR2COM  DS    CL10                COMMISSION                                   
         DS    XL1                                                              
SCR2GRS  DS    CL10                GROSS                                        
         DS    XL1                                                              
SCR2NIC  DS    CL10                NIC                                          
         DS    XL1                                                              
         ORG   SCRLINE             * LINE 3                                     
         DS    XL1                                                              
SCR3ITM  DS    CL4                 ITEM CODE                                    
         DS    XL1                                                              
SCR3DES  DS    CL36                ITEM DESCRIPTION                             
         DS    XL1                                                              
SCR3IND  DS    CL2                 ITEM INDICATORS                              
         DS    XL1                                                              
SCR3NIC  DS    CL10                ITEM NIC                                     
         DS    XL1                                                              
SCR3PRI  DS    CL10                ITEM PRICE                                   
         DS    XL1                                                              
SCR3MUL  DS    CL10                ITEM MULTIPLIER                              
         DS    XL1                                                              
         ORG   SCRLINE             * LINE 4                                     
         DS    XL2                                                              
SCRTIND  DS    CL2                 TEXT TYPE                                    
         DS    XL1                                                              
SCRTEXT  DS    CL70                TEXT FIELD                                   
         DS    XL1                                                              
         EJECT                                                                  
                                                                                
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 FORMAT TYPE                                  
TSDDATA  DS    XL91                                                             
TSDLENQ  EQU   *-TSARDATD                                                       
                                                                                
         ORG   TSDDATA                                                          
TSDFHTQ  EQU   1                   - HEADER TEXT                                
TSDHTXT  DS    CL(L'SCRTEXT)         FIRST 70 BYTES ONLY                        
                                                                                
         ORG   TSDDATA                                                          
TSDFWCQ  EQU   2                   - WORK CODE DATA                             
TSDWCAT  DS    CL2                   CATEGORY                                   
TSDWCOD  DS    CL2                   WORK CODE                                  
TSDWNAM  DS    CL36                  NAME                                       
*SDWVCD  DS    CL1                   VAT CODE                                   
TSDWAMT  DS    PL6                   AMOUNT                                     
TSDWFCA  DS    PL6                   AMOUNT FC                                  
TSDWCAM  DS    PL6                   COMMISSION                                 
TSDWCAF  DS    PL6                   COMMISSION FC                              
TSDWVAM  DS    PL6                   VAT AMOUNT                                 
TSDWVAF  DS    PL6                   VAT AMOUNT FC                              
TSDWNIC  DS    PL6                   NIC AMOUNT                                 
TSDWNFC  DS    PL6                   NIC AMOUNT FC                              
TSDWIND  DS    XL1                   INDICATORS                                 
                                                                                
         ORG   TSDDATA                                                          
TSDFWTQ  EQU   3                   - WORK CODE TEXT                             
TSDWTXT  DS    CL(L'SCRTEXT)         FIRST 70 BYTES ONLY                        
                                                                                
         ORG   TSDDATA                                                          
TSDFIAQ  EQU   4                   - ITEM DATA                                  
TSDINUM  DS    CL4                   ITEM CODE                                  
TSDIDES  DS    CL36                  DESCRIPTION                                
TSDIPRI  DS    PL6                   PRICE                                      
TSDIFCP  DS    PL6                   FC PRICE                                   
TSDINIC  DS    PL6                   NIC AMOUNT                                 
TSDINFC  DS    PL6                   NIC MAOUNT FC                              
TSDIMUL  DS    PL6                   MULTIPLIER                                 
TSDIIND  DS    XL1                   INDICATORS                                 
                                                                                
         ORG   TSDDATA                                                          
TSDFITQ  EQU   5                   - ITEM TEXT                                  
TSDITXT  DS    CL(L'SCRTEXT)         FIRST 70 BYTES ONLY                        
                                                                                
         ORG   TSDDATA                                                          
TSDFFTQ  EQU   6                   - FOOTER TEXT                                
TSDFTXT  DS    CL(L'SCRTEXT)         FIRST 70 BYTES ONLY                        
         EJECT                                                                  
STATABD  DSECT                                                                  
STATSTC  DS    CL2                                                              
STATSTB  DS    XL1                                                              
STATSDD  DS    AL2                                                              
STATLNQ  EQU   *-STATABD                                                        
         EJECT                                                                  
* ACENQWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACENQWORK                                                      
         PRINT ON                                                               
         EJECT                                                                  
                                                                                
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
                                                                                
CURRELEM DS    A                   CURRENT ELEMENT'S ADDRESS                    
CURRKEY  DS    XL42                CURRENT RECORD KEY                           
SAVER2   DS    A                                                                
MYCPJACT DS    CL12                                                             
PCLILEN  DS    XL1                                                              
PPROLEN  DS    XL1                                                              
PJOBLEN  DS    XL1                                                              
CURTENT  DS    CL(CURTABL+L'CURTSHRT+L'CURTLONG)                                
ESTTOT   DS    PL6                 ESTIMATE TOTAL AMOUNT                        
ESTFCT   DS    PL6                 (IN FC)                                      
LSTCAT   DS    CL2                                                              
TXTFLAG  DS    XL1                                                              
LSTWCOD  DS    CL2                                                              
                                                                                
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE))        SPARE OVERLAY SAVE AREA           
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACENQ1B   03/16/20'                                      
         END                                                                    
