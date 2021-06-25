*          DATA SET ACENQ0B    AT LEVEL 013 AS OF 05/05/14                      
*PHASE T6200BC                                                                  
T6200B   TITLE 'ACCOUNT ENQUIRY - JOB ESTIMATES'                                
T6200B   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQB**,R7,CLEAR=YES,RR=RE                                    
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKIN STORAGE)                   
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
         L     RF,=A(GCTBL)                                                     
         AR    RF,RE                                                            
         ST    RF,AGCTBL                                                        
         L     RF,=A(GRDSP)                                                     
         AR    RF,RE                                                            
         ST    RF,AGRDSP                                                        
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERRXIT                                                           
         TM    DISPFLAG,DISIOMAX   HAVE  MAX IO'S BEEN REACHED?                 
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN50                                                           
*                                                                               
MAIN10   TM    OVRSTAT,OVRGDONE    HAVE WE ALREADY FINISHED GRIDS?              
         BO    MAINXGX             NO                                           
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+12                                                             
         TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINXGX             YES                                          
         CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN30              NO                                           
*                                                                               
         GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     MAIN80                                                           
*                                                                               
MAIN30   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BZ    MAIN40              YES                                          
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN40   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BZ    MAIN49              YES                                          
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    MAINX                                                            
         GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),SPROUNIT                          
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES                   
         B     MAINX                                                            
*                                                                               
MAIN49   MVC   IOKEY,KEYSAVE       RESTABLISH SEQUENCE                          
MAIN50   GOTO1 AGETJOB             GET JOB RECORD                               
         BNE   MAIN90                                                           
         USING ACTRECD,R3                                                       
         L     R3,AIO1             R3=A(IOAREA1 CONTAINING JOB REC)             
         BAS   RE,FLTJOB                                                        
         BNE   MAIN60                                                           
         L     R3,AIO1             R3=A(IOAREA1 CONTAINING JOB REC)             
         GOTO1 AOFFACC                                                          
         BNE   MAIN60                                                           
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
*                                                                               
         USING ACTRECD,R3                                                       
         BAS   RE,GETUMTCH         READ UNMATCHED ORDER TRANSACTIONS            
         BNE   MAIN60                                                           
         TM    DISPFLAG,DISIOMAX   MAX IO REACHED?                              
         BO    MAINX                                                            
*                                                                               
         BAS   RE,RDOPT                                                         
         TM    DISPFLAG,DISIOMAX   MAX IO REACHED                               
         BO    MAINX                                                            
         BAS   RE,LOOKUP           GET ESTIMATES                                
         BAS   RE,FILTER           APPLY FILTERING                              
         BE    MAIN70                                                           
*                                                                               
MAIN60   MVC   KEYSAVE,ACTKEY      PREPARE TO GET NEXT JOB RECORD               
         MVI   KEYSAVE+L'ACTKACT+ACTKACT-ACTKEY,X'FF'                           
         B     MAIN30              JOB RECORD REJECTED DUE TO FILTER            
*                                                                               
MAIN70   BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE              
         MVC   KEYSAVE,ACTKEY      SAVE THE KEY                                 
         BNE   ERRXIT                                                           
         MVI   KEYSAVE+L'ACTKACT+ACTKACT-ACTKEY,X'FF'                           
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   MAIN80              YES                                          
         SR    RF,RF               UPDATE TSAR RECORD COUNTER                   
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN30                                                           
*                                                                               
MAIN80   GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN10                                                           
*                                                                               
MAIN90   TM    DISPFLAG,DISIOMAX   MAX IO REACHED?                              
         BO    MAINX                                                            
         OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   *+12                                                             
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     MAINX                                                            
         OI    DISPFLAG,ALLREADQ   ALL RECORDS READ                             
*                                                                               
MAINX    GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
         B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),SPROUNIT                          
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
*                                                                               
         XC    DETFLAG,DETFLAG                                                  
*                                                                               
         LA    R2,BASCACH          R2=A(CONTRA ACCOUNT FIELD)                   
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFNOTV) INPUT IN THIS FIELD NOT VALID             
         CLI   FLDILEN,0           INPUT NOT ALLOWED IN THIS FIELD              
         BNE   ERRXIT                                                           
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         GOTO1 AUNITLDG,SPROUNIT   GET UNIT AND LEDGER DETAILS                  
         BNE   FSTDERR                                                          
         TM    DISPFLAG,DISIOMAX                                                
         BO    FSTDX                                                            
*                                                                               
FSTD05   GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,SPROUL,ACOMFACS,(0,0)             
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
*                                                                               
         LA    R3,IOKEY            R3=A(KEY FOR CLIENT RECORD READ)             
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,MYCO                                                     
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),SPROUNIT                            
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        RF=L'(KEY FIELD INPUT)                       
         BNZ   FSTD10                                                           
         MVI   ACTKACT+L'ACTKACT,X'FF'                                          
         B     FSTD20                                                           
FSTD10   CLI   FLDILEN,L'ACTKACT   ENSURE LENGTH NOT TOO LONG                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     FSTDERR                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FLDDATA                                               
         SR    RE,RE                                                            
         IC    RE,LEDGTLVB         JOB LEVEL PROVIDED?                          
         CR    RF,RE                                                            
         BH    *+8                                                              
         MVI   ACTKACT+L'ACTKACT,X'FF'                                          
*                                                                               
FSTD20   OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    FSTD25                                                           
         MVC   GRDHEMCO,SPACES          UNCOMMITTED EST-CHGS & ORDERS           
         LA    RF,GRDHEMCO                                                      
         MVC   0(L'MX@UCMD,RF),MX@UCMD                                          
         LA    RF,L'MX@UCMD+1(RF)                                               
         MVC   0(L'MX@EMCH,RF),MX@EMCH                                          
         LA    RF,L'MX@EMCH+1(RF)                                               
         MVC   0(L'MX@ORDS,RF),MX@ORDS                                          
*                                                                               
         MVC   GRDHUBES,SPACES          UNBILLED ESTIMATE                       
         LA    RF,GRDHUBES                                                      
         MVC   0(L'MX@UBLD,RF),MX@UBLD                                          
         LA    RF,L'MX@UBLD+1(RF)                                               
         MVC   0(L'MX@EST,RF),MX@EST                                            
*                                                                               
         MVC   GRDHCHBI,SPACES          CHARGES-BILLING                         
         LA    RF,GRDHCHBI                                                      
         MVC   0(L'MX@CHGS,RF),MX@CHGS                                          
         LA    RF,L'MX@CHGS-1(RF)                                               
         MVI   0(RF),C'-'                                                       
         MVC   1(L'MX@BLG,RF),MX@BLG                                            
         B     FSTD70                                                           
*                                                                               
FSTD25   LA    R2,ENQDAT1H                                                      
         GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
*                                                                               
         LA    R2,ENQDAT1H+ENQDAT2H-ENQDAT1H                                    
         MVC   FLDDATA(L'MX@ENH22),MX@ENH22  DEFAULT HEADING LINE 1             
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OSHOW,0             SHOW OPTION?                                 
         BE    FSTD40                                                           
*                                                                               
         LA    R3,HEADTAB          R3=A(HEADING TABLE)                          
         USING HEADTABD,R3                                                      
FSTD30   CLI   HEADTYPE,EOT        END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OSHOW,HEADTYPE      MATCH ON SHOW OPTION?                        
         BE    *+12                                                             
         LA    R3,HEADLNQ(R3)      BUMP TO NEXT ENTRY                           
         B     FSTD30                                                           
         DROP  RF                                                               
*                                                                               
         LA    R4,FLDDATA          R4=A(CURRENT SCREEN LINE)                    
         USING SCRHEAD,R4                                                       
         MVC   SCRHCOL,SPACES      CLEAR OUT COLUMN TO BE REPLACED              
         SR    R1,R1                                                            
         ICM   R1,3,HEADLIN1       R1=(DISPLACEMENT TO DICT ENTRY)              
         AR    R1,R8               R1=A(DICT ENTRY FOR HEADING LINE 1)          
         LA    RF,L'SCRHCOL-1(R1)  FIND LENGTH OF DICT ENTRY                    
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCT   RF,*-8                                                           
         DC    H'0'                                                             
         SR    RF,R1               RF=(EXECUTABLE LENGTH OF DICT ENTRY)         
         LA    RE,L'SCRHCOL-1                                                   
         SR    RE,RF                                                            
         LA    RE,SCRHCOL(RE)      RE=(DISPLACEMENT FOR RIGHT JUSTIFY)          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       DISPLAY HEADING LINE 1                       
FSTD40   LA    R2,ENQDAT2H-ENQDAT1H(R2)  BUMP TO NEXT SCREEN LINE               
         MVC   FLDDATA(L'MX@ENH23),MX@ENH23                                     
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OSHOW-OPTVALSD(RF),0 SHOW DIFFO COLUMN OPTION?                   
         BE    FSTD50                                                           
         LA    R4,ENQDAT2H-ENQDAT1H(R4) BUMP R4 TO SECOND HEADING LINE          
         MVC   WORK,SPACES         CLEAR WORK                                   
         MVC   SCRHCOL,SPACES      CLEAR HEADING TO BE REPLACED                 
         LA    RF,WORK                                                          
         CLI   HEADSEP,0           SEPERATOR NEEDED ON SECOND LINE?             
         BE    *+14                                                             
         MVC   WORK(L'HEADSEP),HEADSEP                                          
         LA    RF,1(RF)                                                         
         SR    R1,R1                                                            
         ICM   R1,3,HEADLIN2       R1=(DISPLACEMENT TO DICT ENTRY)              
         AR    R1,R8               R1=A(DICT ENTRY FOR HEADING LINE 1)          
         MVC   0(L'SCRHCOL,RF),0(R1)                                            
         LA    R1,WORK                                                          
         LA    RF,L'SCRHCOL-1(R1)  GET LENGTH OF SECOND HEADING (MAX8)          
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCT   RF,*-8                                                           
         DC    H'0'                                                             
         SR    RF,R1               RF=(EXECUTABLE LENGTH OF DICT ENTRY)         
         LA    RE,L'SCRHCOL-1                                                   
         SR    RE,RF                                                            
         LA    RE,SCRHCOL(RE)      RE=(DISPLACEMENT FOR RIGHT JUSTIFY)          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       DISPLAY HEADING ON LINE 2                    
FSTD50   LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
*                                                                               
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
         B     FSTD80                                                           
*                                                                               
FSTD70   LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
*                                                                               
FSTD80   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        READ UNMATCHED ORDERS, APPLY FILTER AND SAVE ORDER AMOUNTS   *         
*        THIS ROUTINE USES AIOAREA2                                   *         
* ON ENTRY R3=A(AIO AREA CONTAINING JOB RECORD)                       *         
* ON EXIT  CC EQUAL-     ACTION COMPLETED OK                          *         
*                        'ORDAMT' CONTAINS UNMATCHED ORDER AMT FOR JOB*         
*          CC NOT EQUAL- MAX IOS DONE OR JOB REJECTED BY FILTER       *         
***********************************************************************         
GETUMTCH NTR1                                                                   
*                                                                               
         USING ACTRECD,R3                                                       
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OSHOW-OPTVALSD(RF),0 ORDERS ONLY NEEDED FOR DEFAULT DISP         
         BNE   GETUX                                                            
         ZAP   ORDAMT,=P'0'        CLEAR ORDER AMOUNT                           
         MVI   ORDFLAG,0           ORDER FLAG                                   
         MVC   IOKEY,ACTKEY        GET KEY FORM CURRENT JOB IN IOAREA1          
         LA    R2,IOKEY            R2=A(KEY)                                    
         USING TRNRECD,R2                                                       
         MVC   TRNKWORK,ORDER      WE WANT AN UNMATCHED ORDER RECORD            
         GOTO1 AIO,IOHIGH+IOACCDIR+IO2                                          
         BE    GETU10                                                           
         TM    IOERR,IOMAX         HAVE WE EXCEEDED MAX IO'S?                   
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   YES                                          
         B     GETUX                                                            
*                                                                               
GETU10   LA    R2,IOKEY            R2=A(RECORD IN IOEAREA2)                     
         CLC   TRNKWORK,ORDER      IS IT AN UNMATCHED ORDER RECORD?             
         BNE   GETU80              NO                                           
*                                                                               
GETU30   CLC   TRNKDATE,SPACES     HAVE WE GOT TRANSACTION RECORD?              
         BNH   GETU60                                                           
         OI    ORDFLAG,FOUND       MATCHED ORDER FOUND                          
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OORDERS-OPTVALSD(RF),C'N'                                        
         BE    GETUERRX                                                         
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    GETU35                                                           
         TM    IOERR,IOMAX         HAVE WE EXCEEDED MAX IO'S?                   
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   YES                                          
         B     GETUX                                                            
                                                                                
GETU35   L     R2,AIO2             R2=A(RECORD IN IOEAREA2)                     
         LA    R4,TRNRFST          R4=A(FIRST ELEMENT)                          
                                                                                
GETU40   CLI   0(R4),EOR           END OF RECORD?                               
         BE    GETUERRX                                                         
*                                                                               
         CLI   0(R4),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    GETU50                                                           
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETU40                                                           
*                                                                               
         USING OAMELD,R4                                                        
GETU50   AP    ORDAMT,OAMAMNT      ORDER AMOUNT                                 
         SP    ORDAMT,OAMIVAL      AMOUNT INVOICED TODATE                       
*                                                                               
GETU55   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             LOOK FOR MORE ELEMENTS                       
         BE    GETU60              END OF RECORD, READ AGAIN                    
         CLI   0(R4),OAMELQ        CHECK FOR ORDER                              
         BE    GETU50              YES, SAVE AMOUNTS                            
         B     GETU55              NO, NEXT ELEMENT                             
*                                                                               
GETU60   GOTO1 AIO,IOSEQ+IOACCDIR+IO2                                           
         BE    GETU10                                                           
         TM    IOERR,IOMAX         HAVE WE EXCEEDED MAX IO'S?                   
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   YES                                          
         B     GETUX                                                            
*                                                                               
GETU80   TM    ORDFLAG,FOUND       ANY UNMATCHED ORDERS FOUND                   
         BO    GETUX                                                            
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OORDERS-OPTVALSD(RF),C'Y'                                        
         BE    GETUERRX                                                         
         CLI   OORDERS-OPTVALSD(RF),C'O'                                        
         BE    GETUERRX                                                         
*                                                                               
GETUX    CR    RB,RB                                                            
         BE    *+6                                                              
GETUERRX LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FILTER JOBS                                                  *         
* ON ENTRY R3=A(JOB RECORD)                                           *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
FILTER   NTR1                                                                   
*                                                                               
         USING ACTRECD,R3                                                       
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         CP    CESTAMT,=P'0'       IS THERE A CURRENT ESTIMATE?                 
         BE    FILT10                                                           
         CLI   OEST,C'N'           DO WE WANT JOBS WITHOUT ESTIMATES?           
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT10   CLI   OEST,C'O'           OR WITH?                                     
         BE    FILTREJX                                                         
*                                                                               
         LA    R4,ACTRFST          R4=A(FIRST ELEMENT ON JOB RECORD)            
FILT20   CLI   0(R4),EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),ABLELQ        ACCOUNT BALANCE ELEMENT?                     
         BE    FILT30                                                           
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     FILT20                                                           
*                                                                               
         USING ABLELD,R4           ACCOUNT BALANCE ELEMENT                      
FILT30   CLI   OCHARGE,0           CHARGES  FILTER?                             
         BE    FILT60                                                           
         CP    ABLDR,CESTAMT                                                    
         BH    FILT40                                                           
         BL    FILT50                                                           
         CLI   OCHARGE,EQUESTQ     CHARGES=ESTIMATE FILTER?                     
         BNE   FILTREJX                                                         
         B     FILT60                                                           
FILT40   CLI   OCHARGE,OVESTQ      CHARGES=OVER ESTIMATE FILTER?                
         BNE   FILTREJX                                                         
         B     FILT60                                                           
FILT50   CLI   OCHARGE,UNESTQ      CHARGES=UNDER ESTIMATE FILTER?               
         BNE   FILTREJX                                                         
*                                                                               
FILT60   CLI   OBILLED,0           BILLED FILTER?                               
         BE    FILT90                                                           
         CP    ABLCR,CESTAMT                                                    
         BH    FILT70                                                           
         BL    FILT80                                                           
         CLI   OBILLED,EQUESTQ     BILLED=ESTIMATE FILTER?                      
         BNE   FILTREJX                                                         
         B     FILT90                                                           
FILT70   CLI   OBILLED,OVESTQ      BILLED=OVER ESTIMATE FILTER?                 
         BNE   FILTREJX                                                         
         B     FILT90                                                           
FILT80   CLI   OBILLED,UNESTQ      BILLED=UNDER ESTIMATE FILTER?                
         BNE   FILTREJX                                                         
*                                                                               
*        ZAP   UNBILAMT,CESTAMT    CURRENT ESTIMATE                             
*        SP    UNBILAMT,ABLCR      CURRENT ESTIMATE-BILLED AMOUNT               
FILT90   CP    ABLCR,=P'0'         ANYTHING BILLED?                             
         BE    FILT100                                                          
         CLI   OUNBILL,C'O'        WANT ONLY UNBILLED?                          
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT100  CLI   OUNBILL,C'N'        WANT ONLY TOTALLY BILLED?                    
         BE    FILTREJX                                                         
         DROP  R4                                                               
*                                                                               
FILTX    CR    RB,RB               JOB WANTED                                   
         B     XIT                                                              
FILTREJX LTR   RB,RB               REJECT THE JOB                               
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON ENTRY R3=A(AIO AREA CONTAINING ORDER RECORD)                     *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         SPACE 1                                                                
BLDTSDAT NTR1                                                                   
         USING ACTRECD,R3                                                       
         LA    R4,ACTRFST          R4=A(FIRST ELEMENT)                          
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC                                                      
         USING TSARRECD,R2                                                      
         LH    RF,=Y(TSDLENQ)                                                   
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         MVC   TSDJOB,ACTKACT      JOB NUMBER                                   
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
*                                                                               
         ZAP   TSDORG,OESTAMT      ORIGINAL ESTIMATE AMOUNT                     
         ZAP   TSDCUR,CESTAMT      CURRENT ESTIMATE AMOUNT                      
*                                                                               
BLDT10   CLI   0(R4),EOR           END OF RECORD?                               
         BE    BLDT50                                                           
         CLI   0(R4),NAMELQ        NAME ELEMENT?                                
         BE    BLDT30                                                           
         CLI   0(R4),JOBELQ        JOB ELEMENT?                                 
         BE    BLDT35                                                           
         CLI   0(R4),ABLELQ        ACCOUNT BALANCE ELEMENT?                     
         BE    BLDT40                                                           
*                                                                               
BLDT20   SR    R0,R0               BUMP TO NEXT RECORD                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     BLDT10                                                           
*                                                                               
         USING NAMELD,R4           NAME ELEMENT                                 
BLDT30   SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSDNAME(0),NAMEREC  JOB NAME                                     
         B     BLDT20                                                           
         DROP  R4                                                               
*                                                                               
         USING JOBELD,R4           JOB ELEMENT                                  
BLDT35   MVC   TSDJOBST,JOBSTA1    GET JOB STATUS BYTE                          
         B     BLDT20                                                           
         DROP  R4                                                               
*                                                                               
         USING ABLELD,R4           BALANCE ELEMENT                              
BLDT40   ZAP   TSDACT,ABLDR        ACTUAL CHARGES                               
         ZAP   TSDBIL,ABLCR        BILLED AMOUNT                                
         B     BLDT20                                                           
         DROP  R4                                                               
*                                                                               
BLDT50   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OSHOW,0             DEFAULT DISPLAY?                             
         BNE   BLDT60                                                           
         ZAP   TSDGEN,CESTAMT                                                   
         SP    TSDGEN,ORDAMT                                                    
         SP    TSDGEN,TSDACT       CURREST - (CHARGES + UNMATCHED ORD)          
         B     BLDT90                                                           
BLDT60   CLI   OSHOW,BALQ          SHOW BALANCE?                                
         BNE   BLDT70                                                           
         ZAP   TSDGEN,TSDACT                                                    
         SP    TSDGEN,TSDBIL       CHARGES-BILLED                               
         B     BLDT90                                                           
BLDT70   CLI   OSHOW,UNBQ          SHOW UNBILLED?                               
         BNE   BLDT80                                                           
         ZAP   TSDGEN,TSDCUR                                                    
         SP    TSDGEN,TSDBIL       CURREST-BILLED                               
         B     BLDT90                                                           
BLDT80   CLI   OSHOW,ESTCHGQ       ESTIMAT-CHARGES?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   TSDGEN,TSDCUR                                                    
         SP    TSDGEN,TSDACT                                                    
         DROP  RF                                                               
*                                                                               
BLDT90   TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         MVC   TSDLINES,LINSUSED                                                
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    *+10                                                             
         LTR   RB,RB               TSAR BLOCK IS FULL                           
         B     BLDTX                                                            
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
                                                                                
BLDTX    CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
FORMTSAR NTR1                                                                   
*                                                                               
         MVI   LINSUSED,0          NUMBER OF DISPLAY LINES                      
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY LINES                            
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         USING SCRLIN1D,R2         DSECT FOR NORMAL DATA LINE                   
         L     R3,ATSARREC         R4=A(TSAR RECORD)                            
         USING TSARRECD,R3                                                      
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARDATD,R4                                                      
*                                                                               
         MVC   SCR1JOB,TSDJOB      JOB CODE                                     
         MVC   TMPNBLK,SPACES                                                   
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'TSDNAME),TSDNAME                                          
         LA    RF,L'TSDNAME-1                                                   
         TM    TSDJOBST,JOBSXJOB   EXPENSE JOB?                                 
         BNO   FORMT10                                                          
         LA    RE,TEMP(RF)                                                      
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-12                                                          
         MVI   2(RE),C'/'                                                       
         MVC   4(L'MX@EXP,RE),MX@EXP                                            
         LA    RF,L'MX@EXP+5(RF)                                                
*                                                                               
FORMT10  GOTO1 VCHOPPER,DMCB,((RF),TEMP),(L'TMPNB1,TMPNBLK),3                   
         MVC   SCR1NAME,TMPNB1     LINE 1 OF JOB NAME                           
         CURED (P6,TSDORG),(L'SCR1ORG,SCR1ORG),2,MINUS=YES,DECS=ROUND           
         CURED (P6,TSDCUR),(L'SCR1CUR,SCR1CUR),2,MINUS=YES,DECS=ROUND           
         CURED (P6,TSDACT),(L'SCR1ACT,SCR1ACT),2,MINUS=YES,DECS=ROUND           
         CURED (P6,TSDBIL),(L'SCR1BIL,SCR1BIL),2,MINUS=YES,DECS=ROUND           
         CURED (P6,TSDGEN),(L'SCR1GEN,SCR1GEN),2,MINUS=YES,DECS=ROUND           
         MVI   LINSUSED,1          NUMBER OF DUMMY SCREEN LINES USED            
         CLC   TMPNB2,SPACES       TWO SCREEN LINES REQUIRED FOR NAME?          
         BE    FORMTX                                                           
         DROP  R2                                                               
         LA    R2,L'DUMLIN1(R2)                                                 
         USING SCRLIN2D,R2                                                      
         MVC   SCR2NAME,TMPNB2     LINE 2 OF JOB NAME                           
         MVI   LINSUSED,2          NUMBER OF DUMMY SCREEN LINES USED            
         CLC   TMPNB2,SPACES       3 SCREEN LINES REQUIRED FOR NAME?            
         BE    FORMTX                                                           
         LA    R2,L'DUMLIN1(R2)                                                 
         MVC   SCR2NAME,TMPNB3     LINE 3 OF JOB NAME                           
         MVI   LINSUSED,3          NUMBER OF DUMMY SCREEN LINES USED            
*                                                                               
FORMTX   B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN  LINES                    
         LH    R1,=Y(L'DUMLIN1*NDUMLINE)                                        
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R3,ATSARREC         R4=A(TSAR RECORD)                            
         USING TSARRECD,R3                                                      
*                                                                               
         TM    DETFLAG,DETINIT                                                  
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL),SPROUNIT                         
         GOTO1 ADISPLAY,DISATRIB        DISPLAY DUMMY SCREEN LINES              
         OI    DETFLAG,DETINIT                                                  
         B     FGRM10                                                           
*                                                                               
FGRM20   GOTO1 ADISGRD,DMCB,(0,AGCTBL),SPROUNIT                                 
         B     FGRMX                                                            
*                                                                               
FGRMX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CALL GETOPT FOR JOBBER CALL                                  *         
***********************************************************************         
RDOPT    NTR1                                                                   
*                                                                               
         LA    R3,IOKEY                                                         
         USING CPYRECD,R3                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,MYCO                                                     
         GOTO1 AIO,IOREAD+IOACCMST+IO3                                          
         BE    RDOP10                                                           
         TM    IOERR,IOMAX         HAVE WE EXCEEDED MAX IO'S?                   
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   YES                                          
         B     RDOPX                                                            
*                                                                               
RDOP10   GOTO1 VACCEMU,DMCB,=C'NEWO',,,AIO3                                     
         ORG   *-2                                                              
         LR    R2,R1               EMU REQUIRES R2=A(DMCB)                      
         BASR  RE,RF                                                            
*                                                                               
         L     R5,AGOBLOCK                                                      
         USING GOBLOCKD,R5                                                      
         MVC   GOADM,VDATAMGR                                                   
         L     RE,AOPTBUFF                                                      
         ST    RE,GOABUFF                                                       
         LA    RE,L'OPTBUFF                                                     
         ST    RE,GOLBUFF                                                       
         L     RF,AIO3             RF=A(COMPANY RECORD)                         
         ST    RF,GOACOMP                                                       
         XC    GOAKEY,GOAKEY       CLEAR SEQUENCE RESET                         
         L     R4,AIO1                                                          
         ST    R4,GOAKEY                                                        
*        ST    R4,GOAJOB                                                        
         MVC   GOSELCUL,0(R4)                                                   
         LA    R4,3(R4)                                                         
         MVC   GOSELCLI,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,LEDGTLVA                                                      
         LR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELCLI(0),0(R4)                                                
         LA    R4,0(RF,R4)                                                      
         MVC   GOSELPRO,SPACES                                                  
         IC    R1,LEDGTLVB                                                      
         SR    R1,RF                                                            
         LR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELPRO(0),0(R4)                                                
         LA    R4,0(RF,R4)                                                      
         MVC   GOSELJOB,SPACES                                                  
         IC    R1,LEDGTLVC                                                      
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELJOB(0),0(R4)                                                
         MVI   GOWHICH,0                                                        
         MVI   GOANYWC,C'N'                                                     
         GOTO1 VGETOPT,DMCB,AGOBLOCK                                            
*                                                                               
RDOPX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        FILTER JOB BY TYPE=Y,N,O OPTION FOR MCS                      *         
***********************************************************************         
         SPACE 1                                                                
         USING JOBELD,R3                                                        
         USING OPTVALSD,RF                                                      
FLTJOB   DS    0H                                                               
         L     RF,AOPTVALS                                                      
*                                                                               
         CLI   OTYPE1,C' '                                                      
         BNH   FLTJOBY                                                          
         CLI   OTYPE1,C'Y'                                                      
         BE    FLTJOBY                                                          
*                                                                               
         MVI   MCSFLAG,C'N'        DETERMINE JOB ESTIMATE TYPE                  
         AHI   R3,ACTRFST-ACTRECD                                               
         XR    R0,R0                                                            
*                                                                               
FLTJOB2  CLI   JOBEL,JOBELQ                                                     
         BE    FLTJOB4                                                          
         CLI   JOBEL,0                                                          
         BE    FLTJOB6                                                          
         IC    R0,JOBLN                                                         
         AR    R3,R0                                                            
         B     FLTJOB2                                                          
*                                                                               
FLTJOB4  CLI   JOBLN,JOBLN3Q                                                    
         BL    FLTJOB6                                                          
         TM    JOBSTA1,JOBSMCSE                                                 
         BZ    FLTJOB6                                                          
         MVI   MCSFLAG,C'Y'                                                     
*                                                                               
FLTJOB6  CLI   OTYPE1,C'N'                                                      
         BNE   FLTJOB8                                                          
         CLI   MCSFLAG,C'N'                                                     
         BE    FLTJOBY                                                          
         B     FLTJOBN                                                          
*                                                                               
FLTJOB8  CLI   MCSFLAG,C'Y'                                                     
         BE    FLTJOBY                                                          
         B     FLTJOBN                                                          
*                                                                               
*                                                                               
FLTJOBY  CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
FLTJOBN  L     R3,AIO1                                                          
         LTR   RE,RE                                                            
         BR    RE                                                               
         DROP  R3,RF                                                            
         EJECT                                                                  
***********************************************************************         
*        CALL JOBBER FOR CURRENT ESTIMATE                             *         
***********************************************************************         
LOOKUP   NTR1                                                                   
*                                                                               
         L     RF,ACOLIST                                                       
         OC    0(L'COLIST,RF),0(RF)                                             
         BNZ   LOOK10                                                           
         GOTO1 VJOBCOL,DMCB,LOOKFLDH,ACOLIST,ACOMFACS                           
*                                                                               
LOOK10   L     R5,AJOBLOCK                                                      
         USING JBLOCKD,R5                                                       
         LR    RE,R5                                                            
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 VACCEMU,DMCB,=C'NEWO',,,AIO1                                     
         ORG   *-2                                                              
         LR    R2,R1               EMU REQUIRES R2=A(DMCB)                      
         BASR  RE,RF                                                            
*                                                                               
         MVC   JBAJOB,AIO1                                                      
         XC    JBAKEY,JBAKEY       CLEAR SEQUENCE RESET                         
         L     R1,AIO1                                                          
         ST    R1,JBAKEY                                                        
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         L     R1,AGOBLOCK                                                      
         ST    R1,JBAGOBLK                                                      
         MVC   JBGETOPT,VGETOPT                                                 
         L     RE,AIO2                                                          
         ST    RE,JBAIO                                                         
         LR    RF,RE                                                            
         L     RF,ACOLTAB                                                       
         ST    RF,JBACOLTB                                                      
         LA    RF,L'COLTAB                                                      
         ST    RF,JBLCOLTB                                                      
         L     RF,AOPTVTAB                                                      
         ST    RF,JBAOPVTB                                                      
         LA    RF,L'OPTVTAB                                                     
         ST    RF,JBLOPVTB                                                      
*                                                                               
         LA    RE,LOOKFLDH                                                      
         ST    RE,JBORICLI                                                      
*                                                                               
*        MVI   JBRETCOL+0,JBRETOEN OE                                           
*        MVI   JBRETCOL+1,JBRETCEN CE                                           
*        MVI   JBRETCOL+2,JBRETEND END                                          
*                                                                               
         GOTO1 VJOBBER,DMCB,AJOBLOCK                                            
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,JBACOLTB                                                      
         CLI   JBNEWEST,JBMCSQ     FOR MCS ESTIMATES                            
         BE    LOOK20                                                           
*                                                                               
         ZAP   OESTAMT,JBCOLVAL-JBCOLD(L'JBCOLVAL,RF)                           
         ZAP   CESTAMT,JBCOLVAL+L'JBCOLVAL-JBCOLD(L'JBCOLVAL,RF)                
         B     LOOK30                                                           
*                                                                               
LOOK20   ZAP   OESTAMT,MJETVAL-MJETABD(L'MJETVAL,RF)                            
         ZAP   CESTAMT,MJETVAL-MJETABD+L'MJETVAL(L'MJETVAL,RF)                  
*                                                                               
LOOK30   GOTO1 VACCEMU,DMCB,=C'OLDN',,,AIO1                                     
         ORG   *-2                                                              
         LR    R2,R1               EMU REQUIRES R2=A(DMCB)                      
         BASR  RE,RF                                                            
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
         GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
XIT      XIT1                                                                   
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
***********************************************************************         
         LTORG                                                                  
*                                                                               
ORDER    DC    C'**'               ORDER                                        
*                                                                               
LOOKFLDH DC    AL1(8+L'LOOKFLD)                                                 
         DC    XL4'00'                                                          
         DC    AL1(L'LOOKFLD)                                                   
         DC    XL2'00'                                                          
LOOKFLD  DC    C'OE,CE'                                                         
*                                                                               
HEADTAB  DS    0H                                                               
         DC    AL1(BALQ,MINUSQ)                       ACTUAL-BILLING            
         DC    AL2(MX@ACL-OVERWRKD,MX@BLG-OVERWRKD)                             
         DC    AL1(UNBQ,0)                            UNBILLED ESTIMATE         
         DC    AL2(MX@UBLD-OVERWRKD,MX@EST-OVERWRKD)                            
         DC    AL1(ESTCHGQ,MINUSQ)                    ESTIMATE-CHARGES          
         DC    AL2(MX@EST-OVERWRKD,MX@CHGS-OVERWRKD)                            
         DC    AL1(EOT)                                                         
*                                                                               
EQUESTQ  EQU   C'E'                EQUAL ESTIMATE                               
OVESTQ   EQU   C'O'                OVER ESTIMATE                                
UNESTQ   EQU   C'U'                UNDER ESTIMATE                               
         EJECT                                                                  
*                                                                               
*-----------------*                                                             
* DATA DICTIONARY *                                                             
*-----------------*                                                             
DCMIX    DS    0X                                                               
         DCDDL AC#ENH22,78                                                      
         DCDDL AC#ENH23,78                                                      
         DCDDL AC#EST,8                                                         
         DCDDL AC#CHGS,8                                                        
         DCDDL AC#UBLD,8                                                        
         DCDDL AC#ACL,8                                                         
         DCDDL AC#BLG,8                                                         
         DCDDL AC#EXP,3                                                         
         DCDDL AC#RSR1,L'MX@RSR1        ORIGINAL ESTIMATE                       
         DCDDL AC#CEST,L'MX@CEST        CURRENT ESTIMATE                        
         DCDDL AC#UCMD,L'MX@UCMD        UNCOMMITTED                             
         DCDDL AC#ACTG,L'MX@ACTG        ACTUAL CHARGESGROSS                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*            SF GRID COLUMN TABLE - COVERED BY GCTBLD                 *         
***********************************************************************         
GCTBL    DS    0F                                                               
*                                                                               
GCTCOL1  DC    AL1(GCT1LQ,99,L'MX@JOBC,L'TSDJOB)                                
         DC    AL2(MX@JOBC-OVERWRKD,TSDJOB-TSARDATD)   JOB NUMBER               
         DC    AL1(GCTINOTO+GCTIOVER,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GCT1LQ   EQU   *-GCTCOL1                                                        
*                                                                               
GCTCOL2  DC    AL1(GCT2LQ,01,L'MX@JOBN,L'TSDNAME)      JOB NAME                 
         DC    AL2(MX@JOBN-OVERWRKD,TSDNAME-TSARDATD)                           
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GCT2LQ   EQU   *-GCTCOL2                                                        
*                                                                               
GCTCOL3  DC    AL1(GCT3LQ,03,L'MX@RSR1,L'TSDORG)    ORIGINAL ESTIMATE           
         DC    AL2(MX@RSR1-OVERWRKD,TSDORG-TSARDATD)                            
         DC    AL1(GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT3LQ   EQU   *-GCTCOL3                                                        
*                                                                               
GCTCOL5  DC    AL1(GCT5LQ,05,L'MX@CEST,L'TSDCUR)    CURRENT ESTIMATE            
         DC    AL2(MX@CEST-OVERWRKD,TSDCUR-TSARDATD)                            
         DC    AL1(GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT5LQ   EQU   *-GCTCOL5                                                        
*                                                                               
GCTCOL6  DC    AL1(GCT6LQ,06,L'MX@ACTCH,L'TSDACT)   ACTUAL CHARGES              
         DC    AL2(MX@ACTCH-OVERWRKD,TSDACT-TSARDATD)                           
         DC    AL1(GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT6LQ   EQU   *-GCTCOL6                                                        
*                                                                               
GCTCOL7  DC    AL1(GCT7LQ,07,L'MX@BLG,L'TSDBIL)     BILLING                     
         DC    AL2(MX@BLG-OVERWRKD,TSDBIL-TSARDATD)                             
         DC    AL1(GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT7LQ   EQU   *-GCTCOL7                                                        
*                                                                               
GCTCOL10 DC    AL1(GCT10LQ,10,L'GRDHCHBI,0)         CHARGES-BILLING             
         DC    AL2(GRDHCHBI-OVERWRKD,AGRDSP-OVERWRKD)                           
         DC    AL1(GCTIOVER+GCTIROUT,0,GCTFNUM+GCTFRGHT,0)                      
         DC    AL1(0,0),AL2(0)                                                  
GCT10LQ  EQU   *-GCTCOL10                                                       
GCTCHBIQ EQU   10                                                               
*                                                                               
GCTCOL8  DC    AL1(GCT8LQ,08,L'GRDHEMCO,L'TSDGEN)   ESTIMATE-CHARG&ORD          
         DC    AL2(GRDHEMCO-OVERWRKD,TSDGEN-TSARDATD)                           
         DC    AL1(GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT8LQ   EQU   *-GCTCOL8                                                        
*                                                                               
GCTCOL9  DC    AL1(GCT9LQ,09,L'GRDHUBES,0)          UNBILLED ESTIMATE           
         DC    AL2(GRDHUBES-OVERWRKD,AGRDSP-OVERWRKD)                           
         DC    AL1(GCTIOVER+GCTIROUT,0,GCTFNUM+GCTFRGHT,0)                      
         DC    AL1(0,0),AL2(0)                                                  
GCT9LQ   EQU   *-GCTCOL9                                                        
GCTUBESQ EQU   9                                                                
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DISPLAY DIFFERENCE COLUMN FOR GRIDS                          *         
*         R3 = ADDRESS OF GRID TAB ENTRY                              *         
***********************************************************************         
         USING GCTBLD,R3                                                        
         USING TSARRECD,R4                                                      
GRDSP    NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=RE-ESTABLISH LOCAL WORKING STOR           
         XC    TEMP,TEMP                                                        
*                                                                               
         L     R4,ATSARREC             R2=A(TSAR RECORD)                        
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*                                                                               
         CLI   GCTCOID,GCTCHBIQ     CHARGES - BILLED                            
         BNE   GRDSP20                                                          
         ZAP   DUB,TSDACT           CHARGES -                                   
         SP    DUB,TSDBIL           BILLED                                      
         B     GRDSP50                                                          
*                                                                               
GRDSP20  CLI   GCTCOID,GCTUBESQ     UNBILLED                                    
         BNE   GRDSPERX                                                         
         ZAP   DUB,TSDCUR           CURRENT ESTIMATE -                          
         SP    DUB,TSDBIL           BILLED                                      
         B     GRDSP50                                                          
*                                                                               
GRDSP50  CURED DUB,(20,TEMP),2,FLOAT=-,COMMAS=YES,ALIGN=LEFT                    
         LR    R1,R0                                                            
*                                                                               
GRDSPX   J     XITR1                                                            
GRDSPERX MVI   TEMP,C' '                                                        
         LHI   R1,1                                                             
         J     XITR1                                                            
         LTORG                                                                  
         DROP  R3,R4,RB                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
OVERWRKD DSECT                                                                  
TMPNBLK  DS    0CL57               TEMPORARY JOB NAME BLOCK                     
TMPNB1   DS    CL19                TEMP JOB NAME LINE 1                         
TMPNB2   DS    CL19                TEMP JOB NAME LINE 2                         
TMPNB3   DS    CL19                TEMP JOB NAME LINE 3                         
ORDAMT   DS    PL6                 UNMATCHED ORDER AMOUNT FOR JOB               
OESTAMT  DS    PL6                 ORIGINAL ESTIMATE AMOUNT                     
CESTAMT  DS    PL6                 CURRENT ESTIMATE AMOUNT                      
UNBILAMT DS    PL6                 UNBILLED AMOUNT                              
ORDFLAG  DS    X                   ORDER FLAG                                   
FOUND    EQU   1                   AT LEAST 1 UNMATCHED ORDER ON JOB            
MCSFLAG  DS    C                                                                
AGCTBL   DS    A                   GRID COLUMN TABLE                            
AGRDSP   DS    A                   SPECIAL COLUMN ROUTINE                       
*                                                                               
GRDHEMCO DS    CL29                ESTIMATE-CHARGES AND ORDERS                  
GRDHUBES DS    CL17                UNBILLED ESTIMATE                            
GRDHCHBI DS    CL17                CHARGES-BILLING                              
*                                                                               
DSMIX    DS    0C                                                               
*                                                                               
MX@ENH22 DS    CL78                                                             
         ORG   MX@ENH22                                                         
MX@JOBC  DS    CL8                                                              
         DS    CL5                                                              
MX@JOBN  DS    CL8                                                              
         DS    CL12                                                             
         DS    CL8                                                              
         DS    CL2                                                              
         DS    CL7                                                              
         DS    CL3                                                              
         DS    CL6                                                              
         DS    CL3                                                              
         DS    CL6                                                              
         DS    CL1                                                              
MX@EMCH  DS    CL8                                                              
         ORG                                                                    
MX@ENH23 DS    CL78                                                             
         ORG   MX@ENH23                                                         
         DS    CL8                                                              
         DS    CL5                                                              
         DS    CL8                                                              
         DS    CL12                                                             
         DS    CL8                                                              
         DS    CL2                                                              
         DS    CL7                                                              
         DS    CL2                                                              
         DS    CL7                                                              
         DS    CL2                                                              
         DS    CL7                                                              
         DS    CL1                                                              
MX@ORDS  DS    CL8                                                              
         ORG                                                                    
MX@EST   DS    CL8                                                              
MX@CHGS  DS    CL8                                                              
MX@UBLD  DS    CL8                                                              
MX@ACL   DS    CL8                                                              
MX@BLG   DS    CL8                                                              
MX@EXP   DS    CL3                                                              
MX@RSR1  DS    CL17                   ORIGINAL ESTIMATE                         
MX@CEST  DS    CL16                   CURRENT ESTIMATE                          
MX@UCMD  DS    CL11                   UNCOMMITTED                               
MX@ACTG  DS    CL19                   ACTUAL CHARGESGROSS                       
         ORG   MX@ACTG                                                          
MX@ACTCH DS    CL14                   ACTUAL CHARGES                            
         ORG                                                                    
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SCREEN LINES                                                                  
***********************************************************************         
SCRHEAD  DSECT                     COVER HEADING LINES FOR CHANGING COL         
         DS    CL69                                                             
SCRHCOL  DS    CL8                                                              
*                                                                               
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1JOB  DS    CL(L'ACTKACT)       JOB CODE                                     
         DS    CL1                                                              
SCR1NAME DS    CL19                JOB NAME (PART 1)                            
         DS    CL1                                                              
SCR1ORG  DS    CL8                 ORIGINAL ESTIMATE AMOUNT                     
         DS    CL1                                                              
SCR1CUR  DS    CL8                 CURRENT ESTIMATE AMOUNT                      
         DS    CL1                                                              
SCR1ACT  DS    CL8                 ACTUAL CHARGES AMOUNT                        
         DS    CL1                                                              
SCR1BIL  DS    CL8                 BILLING AMOUNT                               
         DS    CL1                                                              
SCR1GEN  DS    CL8                 GENERAL COLUMN (SEE OSHOW)                   
*                                                                               
SCRLIN2D DSECT                     COVER SCREEN ITEM LINE2                      
         DS    CL(L'ACTKACT+1)                                                  
SCR2NAME DS    CL19                JOB NAME (PART 2)                            
*                                                                               
***********************************************************************         
* SCREEN LINES                                                                  
***********************************************************************         
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDJOB   DS    CL(L'ACTKACT)       JOB NUMBER                                   
TSDNAME  DS    CL36                JOB NAME                                     
TSDJOBST DS    XL(L'JOBSTA1)       JOB STATUS FROM JOBELD                       
TSDORG   DS    PL6                 ORIGINAL ESTIMATE AMOUNT                     
TSDCUR   DS    PL6                 CURRENT ESTIMATE AMOUNT                      
TSDACT   DS    PL6                 ACTUAL CHARGES AMOUNT                        
TSDBIL   DS    PL6                 BILLED AMOUNT                                
TSDGEN   DS    PL6                 GENERAL AMOUNT                               
TSDLENQ  EQU   *-TSARDATD                                                       
*                                                                               
***********************************************************************         
* SCREEN LINES                                                                  
***********************************************************************         
HEADTABD DSECT COVER HEADING TABLE                                              
HEADTYPE DS    CL1                                                              
BALQ     EQU   C'B'                ACTUAL-BILLING                               
UNBQ     EQU   C'U'                UNBILLED ESTIMATE                            
ESTCHGQ  EQU   C'E'                ESTIMATE-CHARGES                             
HEADSEP  DS    CL1                 HEADING SEPERATOR                            
MINUSQ   EQU   C'-'                SEPERATE WITH MINUS SIGN                     
HEADLIN1 DS    AL2                 DISPLACEMENT TO HEADING LINE 1               
HEADLIN2 DS    AL2                 DISPLACMENT TO HEADING LINE 2                
HEADLNQ  EQU   *-HEADTABD                                                       
         EJECT                                                                  
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
DETFLAG  DS    X                                                                
DETINIT  EQU   X'80'                                                            
*                                                                               
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACENQ0B   05/05/14'                                      
         END                                                                    
