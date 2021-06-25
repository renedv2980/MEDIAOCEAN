*          DATA SET ACENQ0E    AT LEVEL 013 AS OF 07/23/13                      
*PHASE T6200EC                                                                  
T6200E   TITLE 'ACCOUNT ENQUIRY - ORDER SUMMARY'                                
T6200E   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQE**,R7,CLEAR=YES,RR=RE                                    
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKIN STORAGE)                   
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*                                                                               
         A     RE,=A(GCTBL)        SET UP GRID COLUMN TABLE                     
         ST    RE,AGCTBL                                                        
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERRXIT                                                           
         TM    DISPFLAG,DISIOMAX   HAVE THE MAX IO'S BEEN DONE?                 
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN60                                                           
*                                                                               
MAIN10   TM    OVRSTAT,OVRGDONE                                                 
         BO    MAINXGX                                                          
*                                                                               
         TM    DISPFLAG,DISIOMAX   RE-ENTER AFTER MAX IO?                       
         BO    *+14                                                             
         MVC   DISLINE,DISSTART    SET LAST SCREEN LINE USED                    
         B     *+8                                                              
         NI    DISPFLAG,X'FF'-DISIOMAX                                          
         MVC   IOKEY,KEYSAVE       RESTABLISH SEQUENCE                          
*                                                                               
MAIN20   CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN40              NO                                           
*                                                                               
         GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         B     MAIN110                                                          
*                                                                               
MAIN40   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAIN50              YES                                          
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN50   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAIN70              YES                                          
         MVC   IOKEY,KEYSAVE       RESTABLISH SEQUENCE                          
MAIN60   GOTO1 AGETJOB             GET JOB RECORD                               
         BE    MAIN80                                                           
         TM    DISPFLAG,DISIOMAX   MAX IO REACHED?                              
         BO    MAINX                                                            
         OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   *+12                                                             
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     MAINX                                                            
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    *+12                                                             
         OI    DISPFLAG,ALLREADQ   ALL RECORDS READ                             
         BAS   RE,TOTAL            DEAL WITH TOTAL LINE                         
MAIN70   B     MAINX                                                            
*                                                                               
MAIN80   L     R3,AIO1             R3=A(IOAREA1 CONTAINING JOB REC)             
         USING ACTRECD,R3                                                       
*                                                                               
         GOTO1 AOFFACC                                                          
         BNE   MAIN90                                                           
         TM    DISPFLAG,DISIOMAX   MAX IO REACHED?                              
         BO    MAINX                                                            
         L     RF,AOPTVALS                                                      
         USING OPTVALSD,RF                                                      
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         SR    R0,R0                                                            
         ICM   R0,3,OFFAWORK                                                    
         BZ    MAIN87                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -             
         CLI   TERMACCS,C'*'       LIMIT ACCESS?                                
         BE    *+12                                                             
         CLI   TERMACCS,C'$'                                                    
         BNE   MAIN87                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -             
         LA    RE,OFFAWORK+2       TEST OFFICE IS IN USER ACCESS LIST           
MAIN81   CLC   RECOFFC,0(RE)       MATCH OFFICE TO LIMIT ACCESS LIST            
         BE    MAIN82                                                           
         LA    RE,L'TRNKOFF(RE)    BUMP TO NEXT OFFICE                          
         BCT   R0,MAIN81           DO FOR NUMBER OF LIST ENTRIES                
*                                                                               
         CLI   OOFFICFI,NEGFILTR                                                
         BNE   MAIN90                                                           
         B     MAIN87                                                           
MAIN82   CLI   OOFFICFI,NEGFILTR                                                
         BE    MAIN90                                                           
         DROP  RF,R1                                                            
*                                                                               
MAIN87   BAS   RE,GETUMTCH         READ UNMATCHED ORDER RECORDS                 
         BNE   MAIN90                                                           
         TM    DISPFLAG,DISIOMAX   MAX IO REACHED?                              
         BO    MAINX                                                            
         B     MAIN100                                                          
MAIN90   MVC   KEYSAVE,ACTKEY                                                   
         MVI   KEYSAVE+L'ACTKACT+ACTKACT-ACTKEY,X'FF'                           
         B     MAIN40              JOB RECORD REJECTED DUE TO FILTER            
MAIN100  DS    0H                                                               
         BAS   RE,RDOPT                                                         
         TM    DISPFLAG,DISIOMAX   MAX IO REACHED                               
         BO    MAINX                                                            
         BAS   RE,LOOKUP                                                        
         BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE              
         BNE   ERRXIT                                                           
         MVC   KEYSAVE,ACTKEY      SAVE THE KEY                                 
         MVI   KEYSAVE+L'ACTKACT+ACTKACT-ACTKEY,X'FF'                           
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   MAIN110             YES                                          
         SR    RF,RF               UPDATE TSAR RECORD COUNTER                   
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN40                                                           
*                                                                               
MAIN110  GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN20                                                           
*                                                                               
MAINX    GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
         B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),0                                 
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
         SPACE 1                                                                
FSTDIS   NTR1                                                                   
         MVI   DETFLAG,0           INIT DETAIL FLAG                             
         ZAP   ORDTOT,=P'0'        ORDER AMIOUNT TOTAL                          
         ZAP   ESTTOT,=P'0'        PRESENT ESTIMATE TOTAL                       
         ZAP   ACTTOT,=P'0'        ACTUAL CHARGES TOTAL                         
         ZAP   BILTOT,=P'0'        BILLING TOTAL                                
         MVC   UNILDG,SPACES                                                    
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
         BE    *+6                 MAX IOS                                      
         DC    H'0'                                                             
         MVC   UNILDG,SPROUNIT     SAVE UNIT/LEDGER                             
         TM    DISPFLAG,DISIOMAX                                                
         BO    FSTDX                                                            
         LA    R2,GRDDAT1H                                                      
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    FSTDIS05                                                         
         LA    R2,ENQDAT1H                                                      
         GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
*                                                                               
FSTDIS05 GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,SPROUL,ACOMFACS,(0,0)             
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
         IC    RE,LEDGTLVB                                                      
         CR    RF,RE                                                            
         BH    *+8                                                              
         MVI   ACTKACT+L'ACTKACT,X'FF'                                          
*                                                                               
FSTD20   OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    FSTD30                                                           
         LA    R2,ENQDAT1H+ENQDAT2H-ENQDAT1H                                    
         MVC   FLDDATA(L'MX@ENH5),MX@ENH5                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH6),MX@ENH6                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         B     *+8                                                              
*                                                                               
FSTD30   LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+10                                                             
         L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        READ UNMATCHED ORDERS, APPLY FILTER AND SAVE ORDER AMOUNTS   *         
*        THIS ROUTINE USES AIOAREA2                                   *         
* ON ENTRY R3=A(AIO AREA CONTAINING JOB RECORD)                       *         
* ON EXIT  CC EQUAL-     ACTION COMPLETED OK                          *         
*          CC NOT EQUAL- JOB REJECTED BY FILTER                       *         
*                        'ORDAMT' CONTAINS UNMATCHED ORDER AMT FOR JOB*         
***********************************************************************         
         SPACE 1                                                                
GETUMTCH NTR1                                                                   
         USING ACTRECD,R3                                                       
         ZAP   ORDAMT,=P'0'                                                     
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
         BNE   GETU80              YES                                          
*                                                                               
GETU30   GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    GETU35                                                           
         TM    IOERR,IOMAX         HAVE WE EXCEEDED MAX IO'S?                   
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   YES                                          
         B     GETUX                                                            
GETU35   L     R2,AIO2             R2=A(RECORD IN IOEAREA2)                     
         LA    R4,TRNRFST          R4=A(FIRST ELEMENT)                          
         CLI   0(R4),TRNELQ        HAVE WE GOT TRANSACTION ELEMENT              
         BNE   GETU60                                                           
         OI    ORDFLAG,FOUND       NMATCHED ORDER FOUND                         
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OORDERS-OPTVALSD(RF),C'N'                                        
         BE    GETUERRX                                                         
GETU40   CLI   0(R4),EOR           END OF RECORD?                               
         BE    GETU60                                                           
         CLI   0(R4),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    GETU50                                                           
*                                                                               
GETU45   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETU40                                                           
*                                                                               
         USING OAMELD,R4                                                        
GETU50   AP    ORDAMT,OAMAMNT      ORDER AMOUNT                                 
         SP    ORDAMT,OAMIVAL      AMOUNT INVOICED TODATE                       
         B     GETU45                                                           
*                                                                               
GETU60   GOTO1 AIO,IOSEQ+IOACCDIR+IO2                                           
         BE    GETU70                                                           
         TM    IOERR,IOMAX         HAVE WE EXCEEDED MAX IO'S?                   
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   YES                                          
         B     GETUX                                                            
*                                                                               
GETU70   L     R2,AIO2             R2=A(RECORD IN IOEAREA2)                     
         CLC   TRNKWORK,ORDER      IS IT AN UNMATCHED ORDER RECORD?             
         BE    GETU30              YES                                          
         TM    ORDFLAG,FOUND       ANY UNMATCHED ORDERS FOUND                   
         BO    GETUX                                                            
GETU80   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OORDERS-OPTVALSD(RF),C'O'                                        
         BE    GETUERRX                                                         
*                                                                               
GETUX    AP    ORDTOT,ORDAMT       UNMATCHED ORDER TOTAL                        
         CR    RB,RB                                                            
         BE    *+6                                                              
GETUERRX LTR   RB,RB                                                            
         B     XIT                                                              
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
         ZAP   TSDOAMT,ORDAMT                                                   
         ZAP   TSDEAMT,ESTAMT                                                   
*                                                                               
BLDT40   CLI   0(R4),EOR           END OF RECORD?                               
         BE    BLDT120                                                          
         CLI   0(R4),NAMELQ        NAME ELEMENT?                                
         BE    BLDT60                                                           
         CLI   0(R4),JOBELQ        JOB ELEMENT?                                 
         BE    BLDT65                                                           
         CLI   0(R4),ABLELQ        ACCOUNT BALANCE ELEMENT?                     
         BE    BLDT70                                                           
*                                                                               
BLDT50   SR    R0,R0               BUMP TO NEXT RECORD                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     BLDT40                                                           
*                                                                               
         USING NAMELD,R4           NAME ELEMENT                                 
BLDT60   SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSDNAME(0),NAMEREC                                               
         B     BLDT50                                                           
         DROP  R4                                                               
*                                                                               
         USING JOBELD,R4           JOB ELEMENT                                  
BLDT65   MVC   TSDJOBST,JOBSTA1    GET JOB STATUS BYTE                          
         B     BLDT50                                                           
         DROP  R4                                                               
*                                                                               
         USING ABLELD,R4           ORDER AMOUNT ELEMENT                         
BLDT70   ZAP   TSDAAMT,ABLDR                                                    
         ZAP   TSDBAMT,ABLCR                                                    
         AP    ACTTOT,ABLDR        ACTUAL CHARGES TOTAL                         
         AP    BILTOT,ABLCR        BILLING TOTAL                                
         B     BLDT50                                                           
         DROP  R4                                                               
                                                                                
BLDT120  TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         MVC   TSDLINES,LINSUSED                                                
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         TM    DISPFLAG,DISIOMAX                                                
         BO    BLDTERRX                                                         
         TM    DISPFLAG,TSARFULQ                                                
         BO    BLDTERRX                                                         
*                                                                               
BLDTX    CR    RB,RB                                                            
         B     XIT                                                              
BLDTERRX LTR   RB,RB               TSAR BLOCK IS FULL                           
         B     BLDTX                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
         SPACE 1                                                                
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0          NUMBER OF DISPLAY LINES                      
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY LINES                            
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R3,ATSARREC         R4=A(TSAR RECORD)                            
         USING TSARRECD,R3                                                      
         CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
         BE    FORM150                                                          
*                                                                               
         USING SCRLIN1D,R2         DSECT FOR NORMAL DATA LINE                   
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARDATD,R4                                                      
         MVC   SCR1JOB,TSDJOB      JOB NUMBER                                   
         MVC   TMPNBLK,SPACES                                                   
         LA    RF,L'TSDNAME        RF=L'(MAX JOB NAME)                          
         MVC   TEMP,TSDNAME        SET JOB NAME                                 
         TM    TSDJOBST,JOBSXJOB   EXPENSE JOB?                                 
         BNO   FORM005                                                          
         LA    RE,TEMP-1(RF)                                                    
         CLI   0(RE),C' '          FIND LAST NON SPACE CHAR                     
         BH    *+8                                                              
         BCT   RF,*-12                                                          
         MVI   1(RE),C'/'                                                       
         MVC   2(L'MX@EXP,RE),MX@EXP INDICATE EXPENSE JOB                       
         LA    RF,L'MX@EXP+1(RF)   INCREASE LENGTH OF NAME                      
*                                                                               
FORM005  GOTO1 VCHOPPER,DMCB,((RF),TEMP),(L'TMPNB1,TMPNBLK),2                   
         MVC   SCR1NAME,TMPNB1     LINE 1 OF JOB NAME                           
*                                                                               
         L     RF,AOPTVALS                                                      
         CLI   OORDERS-OPTVALSD(RF),C'O'                                        
         BNE   FORM015                                                          
*                                                                               
         PACK  TEMP(L'TSDOAMT),NINES(L'SCOAMT-2) GET STRING OF NINES            
         CP    TEMP(L'TSDOAMT),TSDOAMT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM010                                                          
         MP    TEMP(L'TSDOAMT),=P'-1'                                           
         CP    TEMP(L'TSDOAMT),TSDOAMT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM010                                                          
         CURED (P6,TSDOAMT),(L'SCOAMT,SCOAMT),2,MINUS=YES                       
         B     FORM025                                                          
FORM010  CURED (P6,TSDOAMT),(L'SCOAMT,SCOAMT),2,MINUS=YES,DECS=ROUND            
         B     FORM025                                                          
*                                                                               
FORM015  PACK  TEMP(L'TSDOAMT),NINES(L'SCOAMT-2) GET STRING OF NINES            
         CP    TEMP(L'TSDOAMT),TSDOAMT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM020                                                          
         MP    TEMP(L'TSDOAMT),=P'-1'                                           
         CP    TEMP(L'TSDOAMT),TSDOAMT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM020                                                          
         CURED (P6,TSDOAMT),(L'SCOAMT,SCOAMT),2,ZERO=BLANK,MINUS=YES            
         B     FORM025                                                          
FORM020  CURED (P6,TSDOAMT),(L'SCOAMT,SCOAMT),2,MINUS=YES,ZERO=BLANK,DE+        
               CS=ROUND                                                         
*                                                                               
FORM025  PACK  TEMP(L'TSDEAMT),NINES(L'SCEAMT-2) GET STRING OF NINES            
         CP    TEMP(L'TSDEAMT),TSDEAMT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM030                                                          
         MP    TEMP(L'TSDEAMT),=P'-1'                                           
         CP    TEMP(L'TSDEAMT),TSDEAMT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM030                                                          
         CURED (P6,TSDEAMT),(L'SCEAMT,SCEAMT),2,ZERO=BLANK,MINUS=YES            
         B     FORM040                                                          
FORM030  CURED (P6,TSDEAMT),(L'SCEAMT,SCEAMT),2,MINUS=YES,ZERO=BLANK,DE+        
               CS=ROUND                                                         
*                                                                               
FORM040  PACK  TEMP(L'TSDAAMT),NINES(L'SCAAMT-2) GET STRING OF NINES            
         CP    TEMP(L'TSDAAMT),TSDAAMT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM045                                                          
         MP    TEMP(L'TSDAAMT),=P'-1'                                           
         CP    TEMP(L'TSDAAMT),TSDAAMT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM045                                                          
         CURED (P8,TSDAAMT),(L'SCAAMT,SCAAMT),2,ZERO=BLANK,MINUS=YES            
         B     FORM050                                                          
FORM045  CURED (P8,TSDAAMT),(L'SCAAMT,SCAAMT),2,MINUS=YES,ZERO=BLANK,DE+        
               CS=ROUND                                                         
*                                                                               
FORM050  PACK  TEMP(L'TSDBAMT),NINES(L'SCBAMT-2) GET STRING OF NINES            
         CP    TEMP(L'TSDBAMT),TSDBAMT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM055                                                          
         MP    TEMP(L'TSDBAMT),=P'-1'                                           
         CP    TEMP(L'TSDBAMT),TSDBAMT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM055                                                          
         CURED (P8,TSDBAMT),(L'SCBAMT,SCBAMT),2,ZERO=BLANK,MINUS=YES            
         B     FORM100                                                          
FORM055  CURED (P8,TSDBAMT),(L'SCBAMT,SCBAMT),2,MINUS=YES,ZERO=BLANK,DE+        
               CS=ROUND                                                         
*                                                                               
FORM100  MVI   LINSUSED,1          NUMBER OF DUMMY SCREEN LINES USED            
         CLC   TMPNB2,SPACES                                                    
         BE    FORMTX                                                           
         DROP  R2                                                               
         LA    R2,L'DUMLIN1(R2)                                                 
         USING SCRLIN2D,R2                                                      
         MVC   SCR2NAME,TMPNB2     LINE 2 OF JOB NAME                           
         MVI   LINSUSED,2          NUMBER OF DUMMY SCREEN LINES USED            
         B     FORMTX                                                           
         DROP  R2,R4                                                            
*                                                                               
         USING SCRTOT1D,R2         DSECT FOR TOTAL LINE                         
FORM150  LA    R2,L'DUMLIN1(R2)    R2=A(DUMMY SCREEN LINE)                      
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R4                                                      
         MVC   SCRTOTAL,TSTOTAL    TOTAL                                        
         MVC   SCRTOTAL,MX@TOTAL                                                
*                                                                               
         PACK  TEMP(L'TSTOTOT),NINES(L'SCROTOT-2) GET STRING OF NINES           
         CP    TEMP(L'TSTOTOT),TSTOTOT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM165                                                          
         MP    TEMP(L'TSTOTOT),=P'-1'                                           
         CP    TEMP(L'TSTOTOT),TSTOTOT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM165                                                          
         CURED (P6,TSTOTOT),(L'SCROTOT,SCROTOT),2,ZERO=BLANK,MINUS=YES          
         B     FORM170                                                          
FORM165  CURED (P6,TSTOTOT),(L'SCROTOT,SCROTOT),2,MINUS=YES,ZERO=BLANK,*        
               DECS=ROUND                                                       
*                                                                               
FORM170  PACK  TEMP(L'TSTETOT),NINES(L'SCRETOT-2) GET STRING OF NINES           
         CP    TEMP(L'TSTETOT),TSTETOT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM175                                                          
         MP    TEMP(L'TSTETOT),=P'-1'                                           
         CP    TEMP(L'TSTETOT),TSTETOT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM175                                                          
         CURED (P6,TSTETOT),(L'SCRETOT,SCRETOT),2,ZERO=BLANK,MINUS=YES          
         B     FORM180                                                          
FORM175  CURED (P6,TSTETOT),(L'SCRETOT,SCRETOT),2,MINUS=YES,ZERO=BLANK,*        
               DECS=ROUND                                                       
*                                                                               
FORM180  PACK  TEMP(L'TSTATOT),NINES(L'SCRATOT-2) GET STRING OF NINES           
         CP    TEMP(L'TSTATOT),TSTATOT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM185                                                          
         MP    TEMP(L'TSTATOT),=P'-1'                                           
         CP    TEMP(L'TSTATOT),TSTATOT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM185                                                          
         CURED (P8,TSTATOT),(L'SCRATOT,SCRATOT),2,ZERO=BLANK,MINUS=YES          
         B     FORM190                                                          
FORM185  CURED (P8,TSTATOT),(L'SCRATOT,SCRATOT),2,MINUS=YES,ZERO=BLANK,*        
               DECS=ROUND                                                       
*                                                                               
FORM190  PACK  TEMP(L'TSTBTOT),NINES(L'SCRBTOT-2) GET STRING OF NINES           
         CP    TEMP(L'TSTBTOT),TSTBTOT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM195                                                          
         MP    TEMP(L'TSTBTOT),=P'-1'                                           
         CP    TEMP(L'TSTBTOT),TSTBTOT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM195                                                          
         CURED (P8,TSTBTOT),(L'SCRBTOT,SCRBTOT),2,ZERO=BLANK,MINUS=YES          
         B     FORM200                                                          
FORM195  CURED (P8,TSTBTOT),(L'SCRBTOT,SCRBTOT),2,MINUS=YES,ZERO=BLANK,*        
               DECS=ROUND                                                       
*                                                                               
FORM200  MVI   LINSUSED,2          NUMBER OF LINES USED                         
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
         B     FORMTX                                                           
         DROP  R2,R3,R4                                                         
*                                                                               
FORMTX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
         SPACE 1                                                                
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0          NUMBER OF DISPLAY LINES                      
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY LINES                            
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         USING TSARRECD,R3                                                      
         L     R3,ATSARREC         R3=A(TSAR RECORD)                            
*                                                                               
         TM    DETFLAG,DETGRINQ                                                 
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL),UNILDG                           
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES                   
         OI    DETFLAG,DETGRINQ                                                 
         B     FGRM10                                                           
*                                                                               
FGRM20   CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
         BE    FGRM30                                                           
*                                                                               
         CLI   TSARFMT,TSITEM1     DETAIL LINE?                                 
         BNE   FGRMX                                                            
*                                                                               
         GOTO1 ADISGRD,DMCB,(0,AGCTBL),UNILDG                                   
         B     FGRMX                                                            
*                                                                               
FGRM30   GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
*                                                                               
FGRMX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
         SPACE 1                                                                
TOTAL    NTR1                                                                   
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSTLNQ)                                                    
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSTOTITM     TOTAL ITEM                                   
         ZAP   TSTOTOT,ORDTOT      ORDER TOTAL                                  
         ZAP   TSTETOT,ESTTOT      PRESENT ESTIMATE TOTAL                       
         ZAP   TSTATOT,ACTTOT      ACTUAL CHARGES TOTAL                         
         ZAP   TSTBTOT,BILTOT      BILLING TOTAL                                
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FGRMTSAR                                                      
         MVC   TSTLINES,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        CALL GETOPT FOR JOBBER CALL                                  *         
***********************************************************************         
         SPACE 1                                                                
RDOPT    NTR1                                                                   
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
*                                  CALL JOBBER FOR CURRENT ESTIMATE             
***********************************************************************         
*        CALL JOBBER FOR CURRENT ESTIMATE                             *         
***********************************************************************         
         SPACE 1                                                                
LOOKUP   NTR1                                                                   
         L     RF,ACOLIST                                                       
         OC    0(L'COLIST,RF),0(RF)                                             
         BNZ   LOOK10                                                           
*&&UK*&& GOTO1 VJOBCOL,DMCB,(X'FF',LOOKFLDH),ACOLIST,ACOMFACS                   
*&&US*&& GOTO1 VJOBCOL,DMCB,LOOKFLDH,ACOLIST,ACOMFACS                           
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
*                                                                               
         MVI   JBRETCOL+0,JBRETCEN CE                                           
         MVI   JBRETCOL+1,JBRETEND END                                          
*                                                                               
         GOTO1 VJOBBER,DMCB,AJOBLOCK                                            
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,JBACOLTB                                                      
         CLI   JBNEWEST,JBMCSQ     FOR MCS ESTIMATES                            
         BE    LOOK20                                                           
         ZAP   ESTAMT,JBCOLVAL-JBCOLD(L'JBCOLVAL,RF)                            
         B     LOOK30                                                           
*                                                                               
LOOK20   ZAP   ESTAMT,MJETVAL-MJETABD(L'MJETVAL,RF)                             
*                                                                               
LOOK30   AP    ESTTOT,ESTAMT                                                    
*                                                                               
         GOTO1 VACCEMU,DMCB,=C'OLDN',,,AIO1                                     
         ORG   *-2                                                              
         LR    R2,R1               EMU REQUIRES R2=A(DMCB)                      
         BASR  RE,RF                                                            
*                                                                               
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
         GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
XIT      XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
ORDER    DC    C'**'               ORDER                                        
NINES    DC    C'99999999999999999999'                                          
*                                                                               
LOOKFLDH DC    AL1(8+L'LOOKFLD)                                                 
         DC    XL4'00'                                                          
         DC    AL1(L'LOOKFLD)                                                   
         DC    XL2'00'                                                          
LOOKFLD  DC    C'CE'                                                            
*                                                                               
         EJECT                                                                  
DCMIX    DS    0X                                                               
         DCDDL AC#ENH5,78                                                       
         DCDDL AC#ENH6,78                                                       
         DCDDL AC#TOTAL,9                                                       
         DCDDL AC#EXP,3                                                         
         DCDDL AC#JOBN,L'MX@JOBN                                                
         DCDDL AC#JOBC,L'MX@JOBC                                                
         DCDDL AC#UMTCD,L'MX@UMTCD                                              
         DCDDL AC#CEST,L'MX@CEST                                                
         DCDDL AC#ACL,L'MX@ACL                                                  
         DCDDL AC#CHGS,L'MX@CHGS                                                
         DCDDL AC#BLG,L'MX@BLG                                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*     ORDER SUMMARY GRID COLUMN TABLE - COVERED BY GCTBLD             *         
***********************************************************************         
GCTBL    DS    0F                                                               
*                                                                               
GCTCOL1  DC    AL1(GCT1LQ,91,L'MX@JOBC,L'TSDJOB)    JOB CODE                    
         DC    AL2(MX@JOBC-OVERWRKD,TSDJOB-TSARDATD)                            
         DC    AL1(GCTITOT+GCTIOVER,0,0,0)                                      
         DC    AL1(0,L'MX@TOTAL),AL2(MX@TOTAL-OVERWRKD)                         
GCT1LQ   EQU   *-GCTCOL1                                                        
*                                                                               
GCTCOL2  DC    AL1(GCT2LQ,02,L'MX@JOBN,L'TSDNAME)   JOB NAME                    
         DC    AL2(MX@JOBN-OVERWRKD,TSDNAME-TSARDATD)                           
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GCT2LQ   EQU   *-GCTCOL2                                                        
*                                                                               
GCTCOL3  DC    AL1(GCT3LQ,03,L'MX@UMTCD,L'TSDOAMT)    UNMATCHED ORDER           
         DC    AL2(MX@UMTCD-OVERWRKD,TSDOAMT-TSARDATD)                          
         DC    AL1(GCTIOVER+GCTITOT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTOTOT),AL2(TSTOTOT-TSARTOTD)                           
GCT3LQ   EQU   *-GCTCOL3                                                        
*                                                                               
GCTCOL4  DC    AL1(GCT4LQ,04,L'MX@CEST,L'TSDEAMT)   CURRENT ESTIMATE            
         DC    AL2(MX@CEST-OVERWRKD,TSDEAMT-TSARDATD)                           
         DC    AL1(GCTIOVER+GCTITOT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTETOT),AL2(TSTETOT-TSARTOTD)                           
GCT4LQ   EQU   *-GCTCOL4                                                        
*                                                                               
GCTCOL5  DC    AL1(GCT5LQ,05,L'MX@ACL+L'MX@CHGS,L'TSDAAMT)                      
         DC    AL2(MX@ACL-OVERWRKD,TSDAAMT-TSARDATD)                            
         DC    AL1(GCTIOVER+GCTITOT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTATOT),AL2(TSTATOT-TSARTOTD)                           
GCT5LQ   EQU   *-GCTCOL5                                                        
*                                                                               
GCTCOL6  DC    AL1(GCT6LQ,06,L'MX@BLG,L'TSDBAMT)    BILLING AMOUNT              
         DC    AL2(MX@BLG-OVERWRKD,TSDBAMT-TSARDATD)                            
         DC    AL1(GCTIOVER+GCTITOT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTBTOT),AL2(TSTBTOT-TSARTOTD)                           
GCT6LQ   EQU   *-GCTCOL6                                                        
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
OVERWRKD DSECT                                                                  
TMPNBLK  DS    0CL40               TEMPORARY JOB NAME BLOCK                     
TMPNB1   DS    CL20                TEMP JOB NAME LINE 1                         
TMPNB2   DS    CL20                TEMP JOB NAME LINE 2                         
ORDAMT   DS    PL8                 UNMATCHED ORDER AMOUNT FOR JOB               
ESTAMT   DS    PL8                 ESTIMATE AMOUNT                              
ORDFLAG  DS    X                   ORDER FLAG                                   
FOUND    EQU   1                   AT LEAST 1 UNMATCHED ORDER ON JOB            
*                                                                               
***********************************************************************         
DSMIX    DS    0C                                                               
MX@ENH5  DS    CL78                                                             
MX@ENH6  DS    CL78                                                             
MX@TOTAL DS    CL9                                                              
MX@EXP   DS    CL3                                                              
MX@JOBN  DS    CL8                  JOB NAME                                    
MX@JOBC  DS    CL8                  JOB CODE                                    
MX@UMTCD DS    CL9                  UNMATCHED                                   
MX@CEST  DS    CL16                 CURRENT ESTIMATE                            
MX@ACL   DS    CL7                  ACTUAL                                      
MX@CHGS  DS    CL7                  CHARGES                                     
MX@BLG   DS    CL7                  BILLING                                     
         EJECT                                                                  
*                                                                               
***********************************************************************         
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1JOB  DS    CL(L'ACTKACT)       JOB CODE                                     
         DS    CL1                                                              
SCR1NAME DS    CL20                JOB NAME (PART 1)                            
         DS    CL2                                                              
SCOAMT   DS    CL10                UNMATCHED ORDER AMOUNT                       
         DS    CL1                                                              
SCEAMT   DS    CL10                PRESENT ESTIMATE AMOUNT                      
         DS    CL1                                                              
SCAAMT   DS    CL10                ACTUAL CHARGES AMOUNT                        
         DS    CL1                                                              
SCBAMT   DS    CL10                BILLING AMOUNT                               
*                                                                               
*                                                                               
SCRLIN2D DSECT                     COVER SCREEN ITEM LINE2                      
         DS    CL(L'ACTKACT+1)                                                  
SCR2NAME DS    CL20                JOB NAME (PART 2)                            
*                                                                               
*                                                                               
SCRTOT1D DSECT                     COVER SCREEN TOTAL LINE 1                    
SCRTOTAL DS    CL(L'MX@TOTAL)      TOTAL                                        
         DS    CL26                                                             
SCROTOT  DS    CL10                UNMATCHED ORDER TOTAL                        
         DS    CL1                                                              
SCRETOT  DS    CL10                PRESENT ESTIMATE TOTAL                       
         DS    CL1                                                              
SCRATOT  DS    CL10                ACTUAL CHARGES TOTAL                         
         DS    CL1                                                              
SCRBTOT  DS    CL10                BILLING TOTAL                                
*                                                                               
**********************************************************************          
* TSAR DATA RECORDS                                                             
**********************************************************************          
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDJOB   DS    CL(L'ACTKACT)       JOB NUMBER                                   
TSDNAME  DS    CL36                JOB NAME                                     
TSDOAMT  DS    PL(L'ORDTOT)        UNMATCHED ORDER AMOUNT                       
TSDEAMT  DS    PL(L'ESTTOT)        PRESENT ESTIMATE AMOUNT                      
TSDAAMT  DS    PL(L'ACTTOT)        ACTUAL CHARGES AMOUNT                        
TSDBAMT  DS    PL(L'BILTOT)        BILLING AMOUNT                               
TSDJOBST DS    XL1                 JOB STATUS BYTE                              
TSDLENQ  EQU   *-TSARDATD                                                       
*                                                                               
TSARTOTD DSECT                     COVER SCREEN LINE 1                          
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITME FOMAT TYPE                              
TSTOTITM EQU   2                   TOTAL ITEM TYPE                              
TSTOTAL  DS    CL(L'MX@TOTAL)      TOTAL                                        
TSTOTOT  DS    PL(L'ORDTOT)        UNMATCHED ORDER AMOUNT                       
TSTETOT  DS    PL(L'ESTTOT)        PRESENT ESTIMATE AMOUNT                      
TSTATOT  DS    PL(L'ACTTOT)        ACTUAL CHARGES AMOUNT                        
TSTBTOT  DS    PL(L'BILTOT)        BILLING AMOUNT                               
TSTLNQ   EQU   *-TSARTOTD                                                       
         EJECT                                                                  
**********************************************************************          
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
**********************************************************************          
* SAVED STORAGE                                                                 
**********************************************************************          
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
UNILDG   DS    CL2                 SAVED UNIT AND LEDGER                        
DETFLAG  DS    X                                                                
DETGRINQ EQU   X'80'               SCREEN INITIALIZED FOR GRIDS                 
OSVALS   DS    0PL6                CREDITOR VALUES                              
ORDTOT   DS    PL6                 ORDER TOTAL                                  
ESTTOT   DS    PL6                 ESTIMATE TOTAL                               
OSVALLNQ EQU   *-OSVALS                                                         
ACTTOT   DS    PL8                 ACTUAL CHARGES TOTAL                         
BILTOT   DS    PL8                 BILLING TOTAL                                
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACENQ0E   07/23/13'                                      
         END                                                                    
