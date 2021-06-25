*          DATA SET ACENQ1A    AT LEVEL 025 AS OF 03/16/20                      
*PHASE T6201AA                                                                  
T6201A   TITLE 'MCS ESTIMATES LIST'                                             
                                                                                
* USER LVL DATE    LEVEL CHANGE COMMENTS                                        
* ---- --- ------- ---------------------------------------------------          
* TKLU 001 15MAY06 <DU01-5319> - NEW APPLICATION                                
* TKLU 002 11JAN07 <BR10684L> - VALKEY BUG FIX                                  
* TKLU 003 09JAN08 <LO01-7131> - STATUS IN GRID AS 'FULL WORD'                  
* TKLU 004 07JAN09 <LO01-8495> - ESTIMATE NAME TO BE OPTIONAL FIELD             
* MPEN 005 28JAN09 <LO01-8281> - RELINK DUE TO WORKING STORAGE CHANGE           
* SMAN 006 09OCT09 <DU01-8790> - M/F SUP FOR MERGED & INTERNAL APP              
* RGUP 025 16MAR20 DSRD-25860  - EMDELD EXTENDED LENGTH                         
*                                                                               
* THIS HAS THE LATEST CHANGES FROM THE UK BUT IT IS NOT THE UK VERSION          
*                                                                               
                                                                                
T6201A   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQ1A**,R7,R8,CLEAR=YES,RR=RE                                
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LHI   RC,OVERWORK-WORKD                                                
         LA    RC,WORKD(RC)        RC=A(LOCAL WORKING STORAGE)                  
         USING OVERWRKD,RC                                                      
         ST    RE,ORELO                                                         
         L     RE,=A(FGRTSAR)                                                   
         A     RE,ORELO                                                         
         ST    RE,AFGRTSAR                                                      
         L     RE,=A(COLTBL)                                                    
         A     RE,ORELO                                                         
         ST    RE,ACOLTBL                                                       
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN20                                                           
         BAS   RE,FSTDIS           PERFORM FIRST DISPLAY FUNCTIONS              
         BNE   ERRXIT                                                           
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1   GET ACC DIRECTORY RECORD               
         BE    MAIN10                                                           
         TM    IOERR,IOMAX               MAX IO?                                
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN10   TM    DISPFLAG,DISIOMAX   MAX IO'S BEEN REACHED?                       
         BO    MAINX                                                            
         TM    DISPFLAG,NORECQ     NO RECORDS FOUND?                            
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN60                                                           
*                                                                               
MAIN20   TM    STATFLAG,STPGRTOT   PRINT GRID TOTAL?                            
         BNZ   MAIN170                                                          
         CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN30              NO                                           
*                                                                               
         GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    MAIN22                                                           
         GOTO1 AFGRTSAR,(RC)       FORMAT TSAR ONTO GRID SCREEN LINES           
         B     MAIN24                                                           
*                                                                               
MAIN22   BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
MAIN24   GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               IS SCREEN FULL?                              
         MVC   TSLSTLIN,TSCURRNO   KEEP TRACK OF LAST TSAR REC USED             
         XR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO       BUMP UP THE CURRENT TSAR NUMBER              
         B     MAIN20                                                           
*                                                                               
MAIN30   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAIN40                                                           
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$TIRES)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN40   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINX                                                            
         MVC   IOKEY,KEYSAVE                                                    
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1   GET ACC DIRECTORY RECORD               
         BE    MAIN50                                                           
         TM    IOERR,IOMAX               MAX IO?                                
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN50   GOTO1 AIO,IOSEQ+IOACCDIR+IO1    READ SEQ FOR NEXT RECORD               
         BE    MAIN60                                                           
         TM    IOERR,IOMAX               MAX IOS REACHED?                       
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN60   BAS   RE,VALKEY           VALIDATE KEY                                 
         BNE   MAIN150                                                          
         MVC   KEYSAVE,IOKEY       SAVE KEY FOR SEQUENCE RESTORE                
*                                                                               
         BAS   RE,FILTKEY          FILTER ON DIRECTORY RECORD                   
         BNE   MAIN130                                                          
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN70                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN70   L     R3,AIO1                                                          
         BAS   RE,FILTREC                                                       
         BNE   MAIN130                                                          
         BAS   RE,BLDDIS           BUILD/DISPLAY ACCOUNT TSAR REC(S)            
         BNE   MAINX                                                            
*                                                                               
MAIN100  TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
*                                                                               
MAIN130  B     MAIN50                                                           
*                                                                               
MAIN140  MVC   KEYSAVE,IOKEY                                                    
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN150  OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED?                       
         BNZ   MAIN160                                                          
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     MAINX                                                            
*                                                                               
MAIN160  OI    DISPFLAG,ALLREADQ                                                
         BAS   RE,TOTAL                                                         
         B     MAINX                                                            
*                                                                               
MAIN170  TM    PCDRIVEN,PCGRIDQ          TEST RUNNING UNDER GRID?               
         BZ    MAIN180                                                          
         OI    PCDRIVEN,PCGMSG1    SHOW GRID MESSAGE 1?                         
         GOTO1 AFGRTSAR,(RC)                                                    
         GOTO1 ADISPLAY,DISATRIB         DISPLAY DUMMY SCREEN LINES             
         MVC   TSLSTLIN,TSCURRNO                                                
         MVI   DISPFLAG,0                                                       
*                                                                               
MAIN180  NI    STATFLAG,X'FF'-STPGRTOT   ENSURE NEW PAGE                        
*                                                                               
MAINX    GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
         B     OKXIT                                                            
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
         SPACE 1                                                                
TOTAL    NTR1                                                                   
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    XIT                                                              
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LHI   RF,TSDLENQ                                                       
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARDATD,R3                                                      
         MVI   TSDLINES,1                                                       
         MVI   TSDFMT,TSDFTOTQ     TOTAL LINE TYPE                              
         MVC   TSDLINES,LINSUSED                                                
         GOTO1 AFGRTSAR,(RC)       FORMAT TSAR ONTO GRID SCREEN LINES           
         GOTO1 ATSARADD                                                         
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY TOTAL LINE                     
         BNE   TOTAL02                                                          
         MVC   TSLSTLIN,TSCURRNO                                                
         MVI   DISPFLAG,0                                                       
         B     TOTALX                                                           
*                                                                               
TOTAL02  OI    STATFLAG,STPGRTOT   PRINT GRID TOTAL NEXT TIME IN                
*        OI    INTFLAG,SCRFULLQ    SCREEN IS FULL                               
*                                                                               
TOTALX   B     XIT                                                              
         DROP  R3                                                               
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
         SPACE 1                                                                
FSTDIS   NTR1                                                                   
*                                                                               
         USING FLDHDRD,R2                                                       
         LA    R2,BASCACH          C/A FIELD - INPUT NOT ALLOWED                
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         CLI   FLDILEN,0                                                        
         BNE   FSTDERR                                                          
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         USING LDGRECD,R3                                                       
         LA    R3,IOKEY           READ SJ LEDGER RECORD                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,MYCO                                                     
         MVC   LDGKUNT(2),=C'SJ'                                                
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    FSTD05                                                           
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     ERRXIT                                                           
*                                                                               
FSTD05   GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    FSTD10                                                           
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     ERRXIT                                                           
*                                                                               
FSTD10   L     R3,AIO1                                                          
         LA    R3,LDGRFST                                                       
         XR    R0,R0                                                            
*                                                                               
         USING ACLELD,R3                                                        
FSTD15   CLI   ACLEL,0                                                          
         BNE   FSTD20                                                           
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     ERRXIT                                                           
*                                                                               
FSTD20   CLI   ACLEL,ACLELQ                                                     
         BE    FSTD25                                                           
         IC    R0,ACLLN                                                         
         AR    R3,R0                                                            
         B     FSTD15                                                           
*                                                                               
FSTD25   MVC   PCLILEN,ACLELLVA                                                 
         MVC   PPROLEN,ACLELLVB                                                 
         MVC   PJOBLEN,ACLELLVC                                                 
         DROP  R3                                                               
*                                                                               
* KEY INPUT: EITHER 6 CHARACTER GLOBAL NUMBER (START) OR CLI OR                 
*            CLI/PRO OR CLI/PRO/JOB OR =* SEARCH STRING                         
*                                                                               
         MVI   KEYINPUT,C' '                                                    
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         CLI   FLDDATA,C'='        SEARCH?                                      
         JNE   FSTD27                                                           
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,EST,ACOMFACS,0                    
*                                                                               
FSTD27   MVC   FVMSGNO,=AL2(AE$FLDTS)                                           
         CLC   FLDILEN,PCLILEN                                                  
         BL    FSTDERR                                                          
         BH    FSTD30                                                           
         MVI   KEYINPUT,C'C'       CLIENT INPUT                                 
         MVC   FVMSGNO,=AL2(AE$INCLI)                                           
         BAS   RE,VALSJA                                                        
         BE    FSTD50                                                           
         B     FSTDERR                                                          
*                                                                               
FSTD30   MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         CLI   FLDILEN,12                                                       
         BH    FSTDERR                                                          
         CLC   FLDILEN,PPROLEN                                                  
*&&US*&& BH    FSTD35                                                           
*&&UK*&& BNE   FSTD35                                                           
         MVI   KEYINPUT,C'P'       PRODUCT INPUT                                
         MVC   FVMSGNO,=AL2(AE$INPRO)                                           
         BAS   RE,VALSJA                                                        
         BE    FSTD50                                                           
         CLC   FLDILEN,PPROLEN                                                  
         BNE   FSTDERR                                                          
*                                                                               
FSTD35   CLI   FLDILEN,L'EGNPNUM                                                
         BNE   FSTD40                                                           
         MVI   KEYINPUT,C'N'       NUMBER INPUT                                 
         BAS   RE,VALNUM                                                        
         BE    FSTD50                                                           
         B     FSTDERR                                                          
*                                                                               
FSTD40   MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         CLC   FLDILEN,PJOBLEN                                                  
         BH    FSTDERR                                                          
         MVI   KEYINPUT,C'J'       JOB INPUT                                    
         MVC   FVMSGNO,=AL2(AE$INJOB)                                           
         BAS   RE,VALSJA                                                        
         BE    FSTD50                                                           
         B     FSTDERR                                                          
*                                                                               
FSTD50   MVC   MYKEYSV,IOKEY       VALIDATE OPTIONS                             
*                                                                               
         LA    R2,BASOPTH                                                       
         ST    R2,FVADDR                                                        
         USING OPTVALSD,RF                                                      
         L     RF,AOPTVALS                                                      
         MVC   XDOPTN,OXDETAIL                                                  
         CLI   OXDETAIL,C' '       EXTRA DETAILS                                
         BNH   FSTD55                                                           
         TM    PCDRIVEN,PCGRIDQ    NOT ALLOWED UNDER GRID                       
         BZ    FSTD55                                                           
         MVC   FVMSGNO,=AL2(AE$OPTNG)                                           
         B     FSTDERR                                                          
*                                                                               
FSTD55   MVI   LSTATUS1,X'FF'       STATUS                                      
         MVI   LSTATUS2,X'FF'                                                   
         LA    RE,STATAB1                                                       
         ST    RE,ASTATAB1                                                      
         LA    RE,STATAB2                                                       
         ST    RE,ASTATAB2                                                      
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         CLI   OSTAT,0             ANY OPTION SET?                              
         BE    FSTD70                                                           
*                                                                               
         L     RE,ASTATAB1                                                      
*                                                                               
FSTD60   CLI   0(RE),0                                                          
         BE    FSTD66                                                           
         CLC   OSTAT,0(RE)                                                      
         BE    FSTD61                                                           
         AHI   RE,2                                                             
         B     FSTD60                                                           
*                                                                               
FSTD61   MVC   LSTATUS1,1(RE)                                                   
         B     FSTD70                                                           
*                                                                               
FSTD66   L     RE,ASTATAB2                                                      
                                                                                
FSTD67   CLI   0(RE),0                                                          
         BE    FSTDERR                                                          
         CLC   OSTAT,0(RE)                                                      
         BE    FSTD69                                                           
         AHI   RE,2                                                             
         B     FSTD67                                                           
                                                                                
FSTD69   MVC   LSTATUS2,1(RE)                                                   
*                                                                               
FSTD70   CLI   ODATEFI,NEGFILTR                                                 
         BNE   FSTD75                                                           
         MVC   FVMSGNO,=AL2(AE$INOPT)                                           
         B     FSTDERR                                                          
         DROP  RF                                                               
*                                                                               
FSTD75   LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
*                                                                               
         MVI   STATFLAG,0                                                       
*                                                                               
         LA    R2,GRDDAT1H         GRID DATA                                    
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BNZ   FSTD90                                                           
         LA    R2,ENQDAT1H         DISPLAY SCREEN HEADINGS                      
         OI    FLDOIND,FOUTPRT                                                  
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*&&US*&& MVC   FLDDATA(L'MX@ENH43),MX@ENH43                                     
*&&UK*&& MVC   FLDDATA(L'MX@ENH40),MX@ENH40                                     
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
*&&US*&& MVC   FLDDATA(L'MX_ENH43),MX_ENH43                                     
*&&UK*&& MVC   FLDDATA(L'MX_ENH40),MX_ENH40                                     
         CLI   XDOPTN,C'Y'         EXTRA DETAILS?                               
         BNE   *+10                                                             
*&&US*&& MVC   FLDDATA(L'MX@ENH45),MX@ENH45                                     
*&&UK*&& MVC   FLDDATA(L'MX@ENH41),MX@ENH41                                     
         OI    FLDOIND,FOUTPRT                                                  
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
*                                                                               
FSTD90   GOTO1 ASCRNDIM,DMCB,(0,(R2)) GET SCREEN DIMENSIONS                     
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
*&&UK*&& BNZ   FSTDX                                                            
*&&US                                                                           
         BZ    FSTD100                                                          
         LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
         LA    R2,GRDPFAH                                                       
         MVC   GRDPFA(2),LC@PFK    DISPLAY PF ON LAST LINE OF SCREEN            
*                                  LAST RECD WAS MISSING AND NEED PF            
*                                  FOR EMULATOR FOR GRIDS                       
         OI    FLDOIND,FOUTTRN                                                  
         B     FSTDX                                                            
*                                                                               
FSTD100  L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*&&                                                                             
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
         DS    0H                                                               
STATAB1  DC    C'DE',AL1(ESTKLOGD),AL2(MX@DELD-DSMIX)                           
         DC    C'RE',AL1(ESTKREJE),AL2(MX@ESREJ-DSMIX)                          
         DC    C'CA',AL1(ESTKCAPP),AL2(MX@CAPPD-DSMIX)                          
         DC    C'IA',AL1(ESTKINTA),AL2(MX@INAPP-DSMIX)                          
         DC    C'SC',AL1(ESTKSUBM),AL2(MX@ESSUB-DSMIX)                          
         DC    C'IP',AL1(ESTKCREA),AL2(MX@INPRO-DSMIX)                          
         DC    X'00'                                                            
*                                                                               
         DS    0H                                                               
STATAB2  DC    C'ME',AL1(ESTKMERG),AL2(MX@MRGED-DSMIX)                          
         DC    C'SI',AL1(ESTKSINA),AL2(MX@SIAPP-DSMIX)                          
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE SJ ACCOUNT                                    *         
***********************************************************************         
         SPACE 1                                                                
VALSJA   NTR1                                                                   
*                                                                               
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,MYCO                                                     
         MVC   ACTKUNT(2),=C'SJ'                                                
         XR    R1,R1                                                            
         IC    R1,FLDILEN          (POINTS TO BASKEYH)                          
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FLDDATA                                               
*                                                                               
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BNE   VALSJAN                                                          
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BNE   VALSJAN                                                          
                                                                                
         USING RSTELD,RF                                                        
         L     R3,AIO1                                                          
         LA    RF,ACTRFST                                                       
         XR    R0,R0                                                            
         MVC   FVMSGNO,=AL2(AE$NODTA)                                           
*                                                                               
VALSJA2  CLI   RSTEL,0                                                          
         BE    VALSJAN                                                          
         CLI   RSTEL,RSTELQ                                                     
         BE    VALSJA4                                                          
         IC    R0,RSTLN                                                         
         AR    RF,R0                                                            
         B     VALSJA2                                                          
*                                                                               
VALSJA4  CLI   RSTLN,RSTLN3Q                                                    
         BL    VALSJAN                                                          
         TM    RSTSTAT6,RSTSMCSE   ANY ESTIMATES EXISTING?                      
         BZ    VALSJAN                                                          
         DROP  RF                                                               
*                                                                               
         USING ESTRECD,R4                                                       
         LA    R4,IOKEY                                                         
         XC    ESTKEY,ESTKEY                                                    
         MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         MVC   ESTKCPY,MYCO                                                     
         MVC   ESTKCLI,SPACES                                                   
         MVC   ESTKPRO,SPACES                                                   
         MVC   ESTKJOB,SPACES                                                   
                                                                                
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ESTKCLI(0),ACTKACT                                               
         LA    RE,ACTKACT+1(RE)                                                 
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
*                                                                               
VALSJAY  CR    RB,RB                                                            
         B     *+6                                                              
VALSJAN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE NUMBER                                        *         
***********************************************************************         
         SPACE 1                                                                
VALNUM   NTR1                                                                   
*                                                                               
         MVC   FVMSGNO,=AL2(AE$NONIF)                                           
         LA    R1,FLDDATA+5                                                     
         LA    RF,5                                                             
         TM    COMPSTA4,CPYSOFF2                                                
         BZ    *+8                                                              
         LA    RF,4                                                             
*                                                                               
VALNUM2  CLI   0(R1),C'0'                                                       
         BL    VALNUMN                                                          
         CLI   0(R1),C'9'                                                       
         BH    VALNUMN                                                          
         SHI   R1,1                                                             
         BCT   RF,VALNUM2                                                       
*                                                                               
         USING EGNPASD,R1                                                       
         LA    R1,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,MYCO                                                     
         MVC   EGNPNUM,FLDDATA                                                  
*                                                                               
VALNUMY  CR    RB,RB                                                            
         B     *+6                                                              
VALNUMN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
*        FILTER ACCOUNT DIRECTORY RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
FILTKEY  NTR1                                                                   
         LA    R3,IOKEY                                                         
*                                                                               
         MVI   BYTE2,0                                                          
         CLI   KEYINPUT,C'N'       NUMBER INPUT?                                
         BE    FILTK50                                                          
*                                                                               
         USING ESTRECD,R3                                                       
         CLI   ESTKSEQ,ESTKSMQ     INTERESTED IN MAIN ONLY                      
         BNE   FILTKN                                                           
         TM    ESTKSTA2,ESTKMERG+ESTKSINA                                       
         BZ    FILTK40                                                          
         MVI   BYTE2,ESTKMERG                                                   
         TM    ESTKSTA2,ESTKMERG                                                
         BO    FILTK40                                                          
         MVI   BYTE2,ESTKSINA                                                   
FILTK40  MVC   BYTE1,ESTKSTA1                                                   
         B     FILTK80                                                          
*                                                                               
         USING EGNPASD,R3                                                       
         USING OFFALD,R1                                                        
FILTK50  L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,EGNPSOFF                                                
         OC    OFFALIMA,OFFALIMA   TEST LIMIT ACCESS                            
         BZ    FILTK55                                                          
         MVI   OFFAACT,OFFAVAL     TEST OFFICE LIMIT ACCESS                     
         GOTO1 VOFFAL                                                           
         BNE   FILTKN                                                           
         DROP  R1                                                               
*                                                                               
FILTK55  MVC   BYTE1,EGNPSTA1                                                   
         TM    EGNPSTA2,ESTKMERG+ESTKSINA                                       
         BZ    FILTK80                                                          
         MVI   BYTE2,ESTKMERG                                                   
         TM    EGNPSTA2,ESTKMERG                                                
         BO    FILTK80                                                          
         MVI   BYTE2,ESTKSINA                                                   
*                                                                               
FILTK80  CLI   LSTATUS1,X'FF'       STATUS CHECK                                
         BE    FILTK82                                                          
         NC    BYTE1,LSTATUS1                                                   
         BZ    FILTKN                                                           
         USING OPTVALSD,RF                                                      
         L     RF,AOPTVALS                                                      
         CLI   OSTAT,C'S'                                                       
         BNE   FILTKY                                                           
         CLI   BYTE2,ESTKSINA                                                   
         BE    FILTKN                                                           
         B     FILTKY                                                           
*                                                                               
FILTK82  CLI   LSTATUS2,X'FF'      STATUS CHECK                                 
         BE    FILTKY                                                           
         NC    BYTE2,LSTATUS2                                                   
         BZ    FILTKN                                                           
*                                                                               
FILTKY   CR    RB,RB                                                            
         B     XIT                                                              
FILTKN   LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3,RF                                                            
***********************************************************************         
*        VALIDATE ESTIMATE KEY                                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   NTR1                                                                   
*                                                                               
         LA    R3,IOKEY                                                         
         MVC   DADDRESS,ESTKDA-ESTRECD(R3)    SAVE D/A                          
*                                                                               
         CLI   KEYINPUT,C'N'                  NUMBER INPUT?                     
         BE    VALKEY50                                                         
*                                                                               
         USING ESTRECD,R3                                                       
         CLI   ESTKSEQ,ESTKSMQ     SKIP SEQUENTIALS                             
         BNE   VALKEYY                                                          
         CLC   ESTKEY(ESTKLNO-ESTRECD),MYKEYSV                                  
         BNE   VALKEYN                                                          
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,ESTKSOFF                                                
         OC    OFFALIMA,OFFALIMA   TEST LIMIT ACCESS                            
         BZ    VALKEYY                                                          
         MVI   OFFAACT,OFFAVAL     TEST OFFICE LIMIT ACCESS                     
         GOTO1 VOFFAL                                                           
         BNE   VALKEYN                                                          
         B     VALKEYY                                                          
*                                                                               
         USING EGNPASD,R3                                                       
VALKEY50 CLC   EGNPAS(EGNPNUM+1-EGNPASD),MYKEYSV                                
         BNE   VALKEYN                                                          
*                                                                               
VALKEYY  CR    RB,RB                                                            
         B     XIT                                                              
VALKEYN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3                                                               
***********************************************************************         
*        FILTER ACCOUNT DATA RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING EMDELD,R3                                                        
         USING OPTVALSD,R2                                                      
FILTREC  NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS                                                      
*                                                                               
         AHI   R3,ESTRFST-ESTRECD                                               
         CLI   EMDEL,EMDELQ                                                     
         BNE   FILTRN                                                           
*                                                                               
         OC    ODATE,ODATE                                                      
         BZ    FILTR10                                                          
         CLC   EMDDAT,ODATEST                                                   
         BL    FILTRN                                                           
         OC    ODATEEN,ODATEEN                                                  
         BNZ   *+10                                                             
         MVC   ODATEEN,=X'FFFFFF'                                               
         CLC   EMDDAT,ODATEEN                                                   
         BH    FILTRN                                                           
*                                                                               
FILTR10  DS    0H                                                               
*                                                                               
FILTRY   CR    RB,RB                                                            
         B     XIT                                                              
FILTRN   LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORDS AND DISPLAY IF REQUIRED                   *         
***********************************************************************         
         SPACE 1                                                                
         USING ESTRECD,R3                                                       
         USING EMDELD,R5                                                        
BLDDIS   NTR1                                                                   
         LA    R5,ESTRFST                                                       
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
         LA    R4,TSARDATA         R4=A(TSAR DATA)                              
         USING TSARDATD,R4                                                      
         LHI   RF,TSDLENQ          LENGTH OF REC FOR HIGHER LEVS                
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   RECORD NUMBER                                
         MVI   TSDFMT,TSDFRECQ     SCREEN DATA ITEM 1                           
*                                                                               
         CLI   EMDEL,EMDELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TSDGLO#,EMDGNO                                                   
         MVC   TSDCPJA,SPACES                                                   
         XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         LR    RF,R1                                                            
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSDCPJA(0),ESTKCLI                                               
         LA    RF,TSDCPJA(RF)                                                   
*&&UK*&& MVC   0(2,RF),ESTKPRO                                                  
*&&US*&& MVC   0(3,RF),ESTKPRO                                                  
         XR    R1,R1                                                            
         IC    R1,PJOBLEN                                                       
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         SR    R1,RE                                                            
         CHI   R1,6                                                             
         BNH   *+8                                                              
         LA    R1,6                                                             
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
*&&UK*&& MVC   2(0,RF),ESTKJOB                                                  
*&&US*&& MVC   3(0,RF),ESTKJOB                                                  
         MVC   TSDLOC#,ESTKLNO                                                  
         MVC   TSDSTAT1,ESTRSTA1                                                
         MVI   TSDSTAT2,0                                                       
         TM    ESTRSTA2,ESTKMERG+ESTKSINA                                       
         BZ    BLDD00                                                           
         MVI   TSDSTAT2,ESTKMERG                                                
         TM    ESTRSTA2,ESTKMERG                                                
         BO    *+8                                                              
         MVI   TSDSTAT2,ESTKSINA                                                
*                                                                               
BLDD00   MVC   TSDDATE,EMDDAT                                                   
         MVC   TSDSCHC,EMDSCH                                                   
         MVC   TSDBPMC,EMDBMC                                                   
         CLI   TSDBPMC,C' '                                                     
         BH    *+10                                                             
         MVC   TSDBPMC,ESTKJOB                                                  
         MVC   TSDCURC,EMDCUR                                                   
         MVC   TSDNODP,EMDNDP                                                   
         ZAP   TSDTAMT,EMDAMT                                                   
         ZAP   TSDTFCA,EMDFCA                                                   
         ZAP   TSDTVAM,EMDTVA                                                   
         ZAP   TSDTVFC,EMDTVF                                                   
         ZAP   TSDTCAM,EMDTCA                                                   
         ZAP   TSDTCFC,EMDTCF                                                   
         MVC   TSDTRDA,DADDRESS                                                 
*                                                                               
*        AHI   R5,EMDLNQ                                                        
         LLC   R0,EMDLN                                                         
         AR    R5,R0                                                            
         USING ENMELD,R5                                                        
*&&US*&& MVC   TSDDESC,SPACES                                                   
         CLI   ENMEL,ENMELQ                                                     
         BNE   BLDD01              NO LONGER REQUIRED                           
*&&UK*&& MVC   TSDDESC,SPACES                                                   
         XR    R1,R1                                                            
         IC    R1,ENMLN                                                         
         SHI   R1,ENMLNQ+1                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSDDESC(0),ENMNAME                                               
*                                                                               
BLDD01   TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    BLDD02                                                           
         GOTO1 AFGRTSAR,(RC)       FORMAT TSAR ONTO GRID SCREEN LINES           
         B     BLDD04                                                           
*                                                                               
BLDD02   BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
BLDD04   MVC   TSDLINES,LINSUSED   NUMBER OF SCREEN LINES USED                  
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    *+12                                                             
         TM    DISPFLAG,DISIOMAX                                                
         BNO   BLDTERRX                                                         
*                                                                               
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         TM    DISPFLAG,DISIOMAX                                                
         BO    BLDTERRX                                                         
         B     BLDOK                                                            
*                                                                               
BLDTERRX TM    DISPFLAG,DISIOMAX                                                
         BNO   BLDDERRX                                                         
BLDOK    TM    LSTINDS,LSTIRTN     TEST RETURN BACK TO LI                       
         BZ    BLDD14                                                           
         L     R1,ASVSES                   A(SAVED SESSION)                     
         CLC   TSCURRNO,SESCURNO-SESD(R1)  DO WE WANT IT?                       
         BNL   BLDD14                                                           
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUMBER            
         B     *+14                NO - GET NEXT                                
BLDD14   CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   BLDD20              YES                                          
         XR    RF,RF               INCREMENT CURRENT TSAR REC NUMBER            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     BLDD50                                                           
*                                                                               
BLDD20   TM    INTFLAG,SCRFULLQ    MAY BE FULL FROM HIGHER LEVELS               
         BO    BLDD50                                                           
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    *+12                SCREEN FULL?                                 
         OI    INTFLAG,SCRFULLQ                                                 
         B     BLDD50                                                           
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUMBER            
         XR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
BLDD50   TM    DISPFLAG,DISIOMAX   MAX IO?                                      
         BO    BLDDERRX                                                         
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    BLDDERRX                                                         
*                                                                               
BLDDX    CR    RB,RB                                                            
         B     XIT                                                              
BLDDERRX LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
         SPACE 1                                                                
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY LINES                            
         LHI   R1,DUMLINLN                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R4,ATSARREC         R4=A(TSAR RECORD AREA)                       
         USING SCRLIN1D,R2                                                      
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         MVC   SCR1GLO,TSDGLO#                                                  
         MVC   SCR1SJA,TSDCPJA                                                  
         GOTO1 VDATCON,DMCB,(1,TSDDATE),(17,SCR1DAT)                            
         EDIT  (B1,TSDLOC#),(L'SCR1LOC,SCR1LOC),0                               
         CURED (P6,TSDTAMT),(L'SCR1AMT,SCR1AMT),2,MINUS=YES,ZERO=YES            
         MVC   SCR1DES,TSDDESC                                                  
*                                                                               
         MVI   SCR1STA,C'?'                                                     
         USING STATABD,RF                                                       
         LA    RF,STATAB2                                                       
FORM05   CLI   STATSTC,0                                                        
         BE    FORM08                                                           
         CLC   STATSTB,TSDSTAT2                                                 
         BE    FORM10                                                           
         AHI   RF,STATLNQ                                                       
         B     FORM05                                                           
*                                                                               
FORM08   LA    RF,STATAB1                                                       
FORM09   CLI   STATSTC,0                                                        
         BE    FORM15                                                           
         CLC   STATSTB,TSDSTAT1                                                 
         BE    FORM10                                                           
         AHI   RF,STATLNQ                                                       
         B     FORM09                                                           
*                                                                               
FORM10   MVC   SCR1STA,STATSTC                                                  
         DROP  RF                                                               
*                                                                               
FORM15   MVI   LINSUSED,1                                                       
         CLI   XDOPTN,C'Y'                                                      
         BNE   FORMX                                                            
         MVI   LINSUSED,3                                                       
         AHI   R2,L'DUMLIN1                                                     
         CLI   TWAOFFC,C'*'        DDS?                                         
         BNE   FORM20                                                           
         XOUT  TSDTRDA,SCR#MDA,4                                                
*                                                                               
FORM20   MVC   SCR#SCH,TSDSCHC                                                  
         MVC   SCR#BMC,TSDBPMC                                                  
         CLC   TSDCURC,COMPCURR                                                 
         BE    FORM30                                                           
         MVC   SCR#CUR,TSDCURC                                                  
         CLI   TSDNODP,2                                                        
         BNE   FORM25                                                           
         CURED (P6,TSDTFCA),(L'SCR1AMT,SCR1AMT),2,MINUS=YES,ZERO=YES            
         B     FORM30                                                           
*                                  ASSUME NO DEC PLACES IF NOT 2                
FORM25   CURED (P6,TSDTFCA),(L'SCR1AMT,SCR1AMT),0,MINUS=YES,ZERO=YES            
*                                                                               
FORM30   MVC   SCR#DES,TSDDESC+L'SCR1DES                                        
*                                                                               
         AHI   R2,L'DUMLIN1        LINE 3: SHOW VAT AND COMMISSION              
*                                                                               
         CURED (P6,TSDTCAM),(L'SCR_COM,SCR_COM),2,MINUS=YES,ZERO=YES            
*&&UK*&& CURED (P6,TSDTVAM),(L'SCR_VAT,SCR_VAT),2,MINUS=YES,ZERO=YES            
*                                                                               
         DS    0H                  AND SHOW GROSS TOTAL                         
         ZAP   DUB,TSDTAMT                                                      
         AP    DUB,TSDTCAM                                                      
         CURED (P8,DUB),(L'SCR_TOT,SCR_TOT),2,MINUS=YES,ZERO=YES                
*                                                                               
FORMX    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
AMTLNQ   EQU   17                                                               
EST      DC    CL3'EST'                                                         
         SPACE 1                                                                
DCMIX    DS    0X                                                               
         DCDDL AC#ENH43,L'MX@ENH43,L                                            
         DCDDL AC#ENH43,L'MX_ENH43,LU                                           
         DCDDL AC#ENH45,L'MX@ENH45,L                                            
         DCDDL AC#ESTNO,L'MX@ESTNO,L                                            
         DCDDL AC#CLIC,L'MX@CLIC,L                                              
         DCDDL AC#PROC,L'MX@PROC,L                                              
         DCDDL AC#JOBC,L'MX@JOBC,L                                              
         DCDDL AC#DATE,L'MX@DATE,L                                              
         DCDDL AC#NUM,L'MX@NUM,L                                                
         DCDDL AC#CURRC,L'MX@CURRC,L                                            
         DCDDL AC#AMT,L'MX@AMT,L                                                
         DCDDL AC#FCCOM,L'MX@FCCOM,L                                            
         DCDDL AC#FCVAT,L'MX@FCVAT,L                                            
         DCDDL AC#FCAMT,L'MX@FCAMT,L                                            
         DCDDL AC#CMN,L'MX@CMN,L                                                
         DCDDL AC#VATAM,L'MX@VATAM,L                                            
         DCDDL AC#STT,L'MX@STT,L                                                
         DCDDL AC#SCM,L'MX@SCM,L                                                
         DCDDL AC#DESC,L'MX@DESC,L                                              
         DCDDL AC#ENQEL,L'MX@ENQEL                                              
         DCDDL AC#MEDC,L'MX@MEDC                                                
         DCDDL AC#DELD,L'MX@DELD                                                
         DCDDL AC#ESREJ,L'MX@ESREJ                                              
         DCDDL AC#CLIAP,L'MX@CAPPD                                              
         DCDDL AC#SUBC,L'MX@ESSUB                                               
         DCDDL AC#INPRO,L'MX@INPRO                                              
         DCDDL AC#INAPP,L'MX@INAPP                                              
         DCDDL AC#MRGED,L'MX@MRGED                                              
         DCDDL AC#SIAPP,L'MX@SIAPP                                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*        GRID COLUMN TABLE                                            *         
***********************************************************************         
COLTBL   DS    0F                                                               
*                                                                               
COL1     DC    AL1(COLGLO)               COLUMN 1                               
COLGLO   EQU   X'01'                                                            
         DC    AL2(MX@ESTNO-OVERWRKD)    GLOBAL NUMBER                          
         DC    AL1(L'MX@ESTNO-1,0,0,0,0,0)                                      
*                                                                               
COL2     DC    AL1(COLCLI)               COLUMN 2                               
COLCLI   EQU   X'02'                                                            
         DC    AL2(MX@CLIC-OVERWRKD)     CLIENT CODE                            
         DC    AL1(L'MX@CLIC-1,0,0,0,0,0)                                       
*                                                                               
COL3     DC    AL1(COLPRO)               COLUMN 3                               
COLPRO   EQU   X'03'                                                            
         DC    AL2(MX@PROC-OVERWRKD)     PRODUCT CODE                           
         DC    AL1(L'MX@PROC-1,0,0,0,0,0)                                       
*                                                                               
COL4     DC    AL1(COLJOB)               COLUMN 4                               
COLJOB   EQU   X'04'                                                            
         DC    AL2(MX@JOBC-OVERWRKD)     JOB CODE                               
         DC    AL1(L'MX@JOBC-1,0,0,0,0,0)                                       
*                                                                               
COL5     DC    AL1(COLLOC)               COLUMN 5                               
COLLOC   EQU   X'05'                                                            
         DC    AL2(MX@NUM-OVERWRKD)      LOCAL NUMBER                           
         DC    AL1(L'MX@NUM-1,0,0,0,0,0)                                        
*                                                                               
COL6     DC    AL1(COLDAT)               COLUMN 6                               
COLDAT   EQU   X'06'                                                            
         DC    AL2(MX@DATE-OVERWRKD)     DATE                                   
         DC    AL1(L'MX@DATE-1,0,COLFDAT,COLARGHT,0,0)                          
*                                                                               
COL7     DC    AL1(COLAMT)               COLUMN 7                               
COLAMT   EQU   X'07'                                                            
         DC    AL2(MX@AMT-OVERWRKD)      AMOUNT                                 
         DC    AL1(L'MX@AMT-1,0,COLFNUM,COLARGHT,0,0)                           
*&&UK                                                                           
COL8     DC    AL1(COLCUR)               COLUMN 8                               
COLCUR   EQU   X'08'                                                            
         DC    AL2(MX@CURRC-OVERWRKD)    CURRENCY                               
         DC    AL1(L'MX@CURRC-1,0,0,0,0,0)                                      
*                                                                               
COL9     DC    AL1(COLFCA)               COLUMN 9                               
COLFCA   EQU   X'09'                                                            
         DC    AL2(MX@FCAMT-OVERWRKD)    FC AMOUNT                              
         DC    AL1(L'MX@FCAMT-1,0,COLFNUM,COLARGHT,0,0)                         
*&&                                                                             
COL10    DC    AL1(COLSTA)               COLUMN 10                              
COLSTA   EQU   X'0A'                                                            
         DC    AL2(MX@STT-OVERWRKD)      STATUS                                 
         DC    AL1(L'MX@STT-1,0,0,0,0,0)                                        
*                                                                               
COL11    DC    AL1(COLSCH)               COLUMN 11                              
COLSCH   EQU   X'0B'                                                            
         DC    AL2(MX@SCM-OVERWRKD)      SCHEME CODE                            
         DC    AL1(L'MX@SCM-1,0,0,0,0,0)                                        
*                                                                               
COL12    DC    AL1(COLCAM)               COLUMN 12                              
COLCAM   EQU   X'0C'                                                            
         DC    AL2(MX@CMN-OVERWRKD)      COMMISSION                             
         DC    AL1(L'MX@CMN-1,0,COLFNUM,COLARGHT,0,0)                           
*&&UK                                                                           
COL13    DC    AL1(COLVAM)               COLUMN 13                              
COLVAM   EQU   X'0D'                                                            
         DC    AL2(MX@VATAM-OVERWRKD)    VAT                                    
         DC    AL1(L'MX@VATAM-1,0,COLFNUM,COLARGHT,0,0)                         
*&&                                                                             
COL14    DC    AL1(COLDES)               COLUMN 14                              
COLDES   EQU   X'0E'                                                            
         DC    AL2(MX@DESC-OVERWRKD)     DESCRIPTION                            
         DC    AL1(L'MX@DESC-1,0,0,0,0,0)                                       
*&&UK                                                                           
COL15    DC    AL1(COLCFC)               COLUMN 15                              
COLCFC   EQU   X'0F'                                                            
         DC    AL2(MX@FCCOM-OVERWRKD)    COMMISSION FC                          
         DC    AL1(L'MX@FCCOM-1,0,COLFNUM,COLARGHT,0,0)                         
*                                                                               
COL16    DC    AL1(COLVFC)               COLUMN 16                              
COLVFC   EQU   X'10'                                                            
         DC    AL2(MX@FCVAT-OVERWRKD)    VAT FC                                 
         DC    AL1(L'MX@FCVAT-1,0,COLFNUM,COLARGHT,0,0)                         
*                                                                               
COL17    DC    AL1(COLBMC)               COLUMN 17                              
COLBMC   EQU   X'11'                                                            
         DC    AL2(MX@MEDC-OVERWRKD)     BALLPARK MEDIA CODE                    
         DC    AL1(L'MX@MEDC-1,0,0,0,0,0)                                       
*&&                                                                             
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO GRID SCREEN LINES                  *         
*                                                                     *         
* REGS USED: R3=A(TSAR DATA)                                          *         
*            R4=A(DOWNLOAD BLOCK)                                     *         
***********************************************************************         
         SPACE 1                                                                
FGRTSAR  CSECT                                                                  
         NMOD1 0,**FGRT**                                                       
         LR    RC,R1                                                            
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         TM    STATFLAG,STPGRTOT   PRINT GRID TOTAL NEXT TIME IN                
         BNZ   FGRT010             NO - SHOW HEADINGS                           
         CLC   TSCURRNO,=H'1'      FIRST TSAR RECORD?                           
         BNE   FGRT010             NO - DON'T INITIALIZE GRID                   
         LR    R1,RC                                                            
         L     RF,=A(GRDINIT)                                                   
         A     RF,ORELO                                                         
         BASR  RE,RF               INITIALIZE GRID PRINT LINES                  
*                                                                               
FGRT010  MVI   LINSUSED,0          RESET NUMBER OF LINES USED                   
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LHI   R1,DUMLINLN                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,ADLCB            BUILD DOWNLOAD CONTROL BLOCK                 
         USING DLCBD,R4                                                         
         XC    DLCBD(DLCBL),DLCBD                                               
         LA    RF,FGRT300          DUMMY PRINT RTN                              
         ST    RF,DLCBAPR                                                       
         L     R2,ADUMLINE         RE=A(FIRST SCREEN LINE)                      
         ST    R2,DLCBAPL                                                       
         MVI   DLCBACT,DLCBSOR     INITIALIZE CALL                              
         GOTO1 VDLFLD,DLCBD        FIRST FOR SCREEN LINE                        
*                                                                               
         TM    STATFLAG,STPGRTOT                                                
         BNZ   FGRT050                                                          
*                                                                               
         L     R3,ATSARREC         R3=A(TSAR RECORD )                           
         LA    R3,TSARDATA-TSARRECD(R3)                                         
         USING TSARDATD,R3                                                      
         CLI   TSDFMT,TSDFTOTQ     TOTAL LINE TYPE                              
         BE    FGRT050                                                          
*                                  COLUMN 1                                     
         MVC   DLCBFLD+1(L'TSDGLO#),TSDGLO#                                     
         BAS   RE,FGDLTXT                                                       
*                                  COLUMN 2                                     
         XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),TSDCPJA                                               
         BAS   RE,FGDLTXT                                                       
*                                  COLUMN 3                                     
         XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         LA    R1,TSDCPJA(R1)                                                   
*&&US*&& MVC   DLCBFLD(3),0(R1)                                                 
*&&UK*&& MVC   DLCBFLD(2),0(R1)                                                 
         BAS   RE,FGDLTXT                                                       
*                                  COLUMN 4                                     
         XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
*&&US*&& LA    R1,TSDCPJA+3(R1)                                                 
*&&UK*&& LA    R1,TSDCPJA+2(R1)                                                 
         MVC   DLCBFLD(6),0(R1)                                                 
         BAS   RE,FGDLTXT                                                       
*                                  COLUMN 5                                     
         EDIT  (B1,TSDLOC#),(3,DLCBFLD),0,ZERO=YES                              
         BAS   RE,FGDLTXT                                                       
*                                  COLUMN 6                                     
         GOTO1 VDATCON,DMCB,(1,TSDDATE),(10,DLCBFLD)                            
         BAS   RE,FGDLTXT                                                       
*                                  COLUMN 7                                     
         CURED (P6,TSDTAMT),(AMTLNQ,DLCBFLD),2,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
         BAS   RE,FGDLNUM                                                       
*&&UK                                                                           
*                                  COLUMN 8                                     
         MVC   DLCBFLD(3),TSDCURC                                               
         BAS   RE,FGDLTXT                                                       
*                                  COLUMN 9                                     
         CLC   TSDCURC,COMPCURR                                                 
         BE    FGRT015                                                          
         CURED (P6,TSDTFCA),(AMTLNQ,DLCBFLD),2,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
         CLI   TSDNODP,2                                                        
         BE    FGRT015             (SEE FORMTSAR FC TOTAL EDIT)                 
         CURED (P6,TSDTFCA),(AMTLNQ,DLCBFLD),0,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
FGRT015  BAS   RE,FGDLNUM                                                       
*&&                                                                             
*                                  COLUMN 10                                    
         MVI   DLCBFLD,C'?'                                                     
         USING STATABD,R1                                                       
         L     R1,ASTATAB2                                                      
FGRT020  CLI   STATSTC,0                                                        
         BE    FGRT021                                                          
         CLC   STATSTB,TSDSTAT2                                                 
         BE    FGRT025                                                          
         AHI   R1,STATLNQ                                                       
         B     FGRT020                                                          
*                                                                               
FGRT021  L     R1,ASTATAB1                                                      
FGRT022  CLI   STATSTC,0                                                        
         BE    FGRT030                                                          
         CLC   STATSTB,TSDSTAT1                                                 
         BE    FGRT025                                                          
         AHI   R1,STATLNQ                                                       
         B     FGRT022                                                          
*                                                                               
FGRT025  LA    RE,DSMIX                                                         
         XR    RF,RF                                                            
         ICM   RF,3,STATSDD                                                     
         AR    RE,RF                                                            
         MVC   DLCBFLD(20),0(RE)                                                
         DROP  R1                                                               
*                                                                               
FGRT030  BAS   RE,FGDLTXT                                                       
*                                  COLUMN 11                                    
         MVC   DLCBFLD(L'TSDSCHC),TSDSCHC                                       
         BAS   RE,FGDLTXT                                                       
*                                  COLUMN 12                                    
         CURED (P6,TSDTCAM),(AMTLNQ,DLCBFLD),2,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
         BAS   RE,FGDLNUM                                                       
*&&UK                                                                           
*                                  COLUMN 13                                    
         CURED (P6,TSDTVAM),(AMTLNQ,DLCBFLD),2,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
         BAS   RE,FGDLNUM                                                       
*&&                                                                             
*                                  COLUMN 14                                    
         MVC   DLCBFLD(40),TSDDESC                                              
         BAS   RE,FGDLTXT                                                       
*&&UK                                                                           
*                                  COLUMN 15                                    
         CLC   TSDCURC,COMPCURR                                                 
         BE    FGRT035                                                          
         CURED (P6,TSDTCFC),(AMTLNQ,DLCBFLD),2,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
         CLI   TSDNODP,2                                                        
         BE    FGRT035             (SEE FORMTSAR FC TOTAL EDIT)                 
         CURED (P6,TSDTCFC),(AMTLNQ,DLCBFLD),0,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
*                                                                               
FGRT035  BAS   RE,FGDLNUM                                                       
*                                  COLUMN 16                                    
         CLC   TSDCURC,COMPCURR                                                 
         BE    FGRT040                                                          
         CURED (P6,TSDTVFC),(AMTLNQ,DLCBFLD),2,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
         CLI   TSDNODP,2                                                        
         BE    FGRT040             (SEE FORMTSAR FC TOTAL EDIT)                 
         CURED (P6,TSDTVFC),(AMTLNQ,DLCBFLD),0,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
*                                                                               
FGRT040  BAS   RE,FGDLNUM                                                       
*                                  COLUMN 17                                    
         MVC   DLCBFLD(1),TSDBPMC                                               
         BAS   RE,FGDLTXT                                                       
*&&                                                                             
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 VDLFLD,DLCBD                                                     
         XR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         A     RF,DLCBTOTL         NUM OF LINES PUT TO DUMMY SCREEN             
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
         B     FGRTXIT                                                          
*                                                                               
FGRT050  L     RF,ATSARREC         RF=A(TSAR RECORD )                           
         MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 VDLFLD,DLCBD        END OF RECORD                                
         XR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         AHI   RF,1                                                             
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
*                                                                               
FGRTXIT  XIT1                                                                   
*                                                                               
FGRT300  L     RF,DLCBAPL          BUMP TO NEXT DOWNLOAD PRINT LINE             
         LA    RF,L'DUMLIN1(,RF)                                                
         ST    RF,DLCBAPL                                                       
         BR    RE                                                               
         DROP  R3,R5                                                            
         SPACE 2                                                                
***********************************************************************         
*        BUILD DOWNLOAD DATA                                          *         
* ON ENTRY     R2=A(DUMMY SCREEN LINES)                               *         
*              R4=A(DOWNLOAD BLOCK)                                   *         
*              FOR FGDLTXT/FGDLNUM                                    *         
*                  - DLCBFLD = TEXT/NUMERIC DATA                      *         
*              FOR FGDLLTXT (PUT LONG TEXT DOWNLOAD FIELD)            *         
*                  - TEMP    = TEXT DATA UPTO 200 CHARACTERS          *         
*                  - MYBYTE1 = LENGTH OF TEXT DATA                    *         
***********************************************************************         
         SPACE 1                                                                
FGDLTXT  LR    R0,RE               SAVE RETURN REGITER                          
         MVI   DLCBTYP,DLCBTXT     TEXT DATA                                    
         B     FGDLPUT                                                          
*                                                                               
FGDLNUM  LR    R0,RE                                                            
         MVI   DLCBTYP,DLCBNUM     NUMERIC DATA                                 
         CLC   DLCBFLD,SPACES      ANY DATA                                     
         BH    FGDLPUT                                                          
         MVI   DLCBTYP,DLCBTXT     MOVE IN "" IF NOT APPLICABLE                 
         B     FGDLPUT                                                          
*                                                                               
FGDLLTXT LR    R0,RE               BUILD LONG TEXT FIELD                        
         MVI   DLCBTYP,DLCBTXT     TEXT DATA                                    
         XR    RF,RF                                                            
         ICM   RF,1,MYBYTE1                                                     
         BZ    FGDLPUT                                                          
         CHI   RF,L'DLCBFLD                                                     
         BH    FGLTXT02                                                         
         MVC   DLCBFLD,TEMP        MOVE DATA TO DOWNLOAD FIELD                  
         B     FGLTXT04                                                         
*                                                                               
FGLTXT02 CHI   RF,L'DUMLIN1-4      MAKE SURE IT'S NOT TOO LONG                  
         BH    FGLTXT06            YES - DON'T CALL DLFLD                       
         OI    DLCBFLG1,DLCBFXFL   USE EXTENDED FIELD                           
         MVC   DLCBFLX,TEMP        MOVE DATA TO EXTENDED FIELD                  
                                                                                
FGLTXT04 STC   RF,DLCBLEN          SET LENGTH                                   
         B     FGDLPUT                                                          
*                                                                               
FGLTXT06 LA    RE,TEMP             HANDLE LONG FIELD                            
FGLTXT08 CLI   0(RE),C'"'          TEST IF (EOTCHR) DEFINED                     
         BNE   *+8                                                              
         MVI   0(RE),C''''         YES REPLACE BY ALTERNATE CHAR                
         LA    RE,1(,RE)                                                        
         BCT   RF,FGLTXT08                                                      
*                                                                               
         L     RE,DLCBAPL          CURRENT DOWNLOAD PRINT LINES                 
         AH    RE,DLCBNUMC         RE=A(NEXT AVAIL CHR IN PRINT LINE)           
         MVI   0(RE),C'"'                                                       
         IC    RF,MYBYTE1                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),TEMP        MOVE IN FIELD                                
         LA    RE,1+1(RF,RE)       END OF FIELD                                 
         MVI   0(RE),C'"'                                                       
         L     RF,DLCBAPL          CURRENT DOWNLOAD PRINT LINES                 
         SR    RE,RF                                                            
         LA    RE,2(,RE)           TOTAL LENGTH FROM START OF LINE              
         L     RF,DLCBTOTC         BUMP TOTAL NUMBER OF CHARS                   
         AR    RF,RE                                                            
         ST    RF,DLCBTOTC                                                      
         SRDL  RE,32                                                            
         D     RE,=A(L'DUMLIN1)                                                 
         STH   RE,DLCBNUMC         BUMP NUM OF CHARS THIS LINE                  
         MVC   DLCBNUMF,=H'1'      BUMP NUM OF FIELDS THIS LINE                 
         L     RE,DLCBTOTL         BUMP TOTAL NUMBER OF LINES                   
         AR    RE,RF                                                            
         ST    RE,DLCBTOTL                                                      
         MHI   RF,L'DUMLIN1                                                     
         L     RE,DLCBAPL          BUMP NUM OF DOWNLOAD PRINT LINES             
         AR    RE,RF                                                            
         ST    RE,DLCBAPL                                                       
         LR    RE,R0               RESTORE RETURN ADDR.                         
         BR    RE                                                               
*                                                                               
FGDLPUT  MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLCBD                                                     
         NI    DLCBFLG1,X'FF'-DLCBFXFL                                          
         LR    RE,R0               RESTORE RETURN ADDR.                         
         BR    RE                                                               
         DROP  R4                                                               
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* INITIALIZE SCREEN LINES - BUILD GRID DETAIL LINES AND HEADERS       *         
* ON ENTRY     R2=A(NEXT AVAILABLE SCREEN LINE)                       *         
* ON EXIT      R2=A(NEXT AVAILABLE SCREEN LINE)                       *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
* REGS USED: R2=A(DUMMY SCREEN LINES)                                 *         
*            R3=A(COLUMN TABLE)                                       *         
*            R4=A(DOWNLOAD BLOCK)                                     *         
***********************************************************************         
         SPACE 1                                                                
GRDINIT  CSECT                                                                  
         NMOD1 0,**GINI**                                                       
         LR    RC,R1                                                            
         MVI   LINSUSED,0          RESET NUMBER OF LINES USED                   
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LHI   R1,DUMLINLN                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ADUMLINE         RE=A(FIRST SCREEN LINE)                      
         GOTO1 ABLDDOR,DMCB,(R2),(L'MX@ENQEL,MX@ENQEL),0                        
*                                                                               
         L     R4,ADLCB            BUILD DOWNLOAD CONTROL BLOCK                 
         USING DLCBD,R4                                                         
         XC    DLCBD(DLCBL),DLCBD                                               
         LA    RF,GRDI40           DUMMY PRINT RTN FOR INIT                     
         ST    RF,DLCBAPR                                                       
         ST    R2,DLCBAPL                                                       
         MVI   DLCBACT,DLCBSOR     INITIALIZE CALL                              
         GOTO1 VDLFLD,DLCBD        FIRST FOR REPORT                             
*                                                                               
         L     R3,ACOLTBL          R3=A(COLUMN TABLE)                           
         USING COLTBLD,R3                                                       
GRDI10   CLI   COLETRY,EOT         END OF TABLE                                 
         BE    GRDIX                                                            
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         XR    RE,RE                                                            
         ICM   RE,3,COLNAME        RE=COLUMN NAME IN STORAGE                    
         LA    RE,OVERWRKD(RE)                                                  
         XR    RF,RF                                                            
         IC    RF,COLNMLN          LENGTH OF COLUMN - 1                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(RE)    MOVE COLUMN HEADING                          
         LA    R5,DLCBFLD(RF)      END OF COLUMN HEADING                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C'*'                                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,COLETRY,2(R5),L'COLETRY,0                           
*                                                                               
         OC    COLINF(COLINFLQ),COLINF TEST ANY COLUMN INFO.                    
         BZ    GRDI28                  NO - CALL DLFLD                          
         MVI   4(R5),C'*'                                                       
         LA    R5,5(,R5)                                                        
         LA    RF,COLINF           COLUMN INFO.                                 
         LA    RE,COLINFLQ         LENGTH OF COLUMN INFO.                       
GRDI20   CLI   0(RF),0                                                          
         BE    *+14                                                             
         MVC   0(L'COLINF,R5),0(RF)                                             
         LA    R5,L'COLINF(,R5)                                                 
         LA    RF,L'COLINF(,RF)                                                 
         BCT   RE,GRDI20                                                        
GRDI28   GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
GRDI30   LA    R3,COLLN1Q(,R3)     NEXT COLUMN                                  
         B     GRDI10                                                           
*                                                                               
GRDI40   L     RF,DLCBAPL          BUMP TO NEXT DOWNLOAD PRINT LINE             
         LA    RF,L'DUMLIN1(,RF)                                                
         ST    RF,DLCBAPL                                                       
         BR    RE                                                               
*                                                                               
GRDIX    MVI   DLCBACT,DLCBEOL     END OF HEADING TEXT LINE                     
         GOTO1 VDLFLD,DLCBD                                                     
         L     RF,DLCBTOTL             NUM OF HEADING LINES                     
         MHI   RF,L'DUMLIN1            RF=LENGTH OF HEADINGS                    
         L     R2,ADUMLINE             R2=A(FIRST SCREEN LINE)                  
         XR    R1,R1                                                            
         IC    R1,GRDNDOR                                                       
         MHI   R1,L'DUMLIN1                                                     
         LA    R2,0(R1,R2)             R2=A(FIRST HEADING LINE)                 
         GOTO1 VSQASHER,DMCB,(R2),(RF) SQUASH THE HEADINGS                      
         ICM   RF,15,DMCB+4                                                     
         LA    RF,2(,RF)           RF=NEW LENGTH OF HEADINGS                    
         XR    RE,RE                                                            
         D     RE,=A(L'DUMLIN1)                                                 
         XR    R1,R1                                                            
         IC    R1,GRDNDOR          NO. OF DETAIL OF REQ. LINES                  
         LA    RF,0(R1,RF)                                                      
         LTR   RE,RE               ANY REMAINDER?                               
         BZ    *+8                                                              
         LA    RF,1(,RF)           RF=NEW TOTAL NUMBER OF LINES                 
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
ORELO    DS    A                   OVERLAY RELOCATION                           
AFGRTSAR DS    A                   FORMAT TSAR RECORD FOR GRID                  
ACOLTBL  DS    A                   A(GRID COLUMN TABLE)                         
*                                                                               
DADDRESS DS    XL4                                                              
MYBYTE1  DS    XL1                                                              
MYBYTE2  DS    XL1                                                              
*                                                                               
INTFLAG  DS    X     INTERNAL FLAG                                              
SCRFULLQ EQU   1     SCREEN IS FULL                                             
         SPACE 1                                                                
DSMIX    DS    0C                                                               
MX@ENH43 DS    CL(L'LSTDAT2)                                                    
MX_ENH43 DS    CL(L'LSTDAT2)                                                    
MX@ENH45 DS    CL(L'LSTDAT2)                                                    
MX@ESTNO DS    CL6                                                              
MX@CLIC  DS    CL7                                                              
MX@PROC  DS    CL7                                                              
MX@JOBC  DS    CL7                                                              
MX@DATE  DS    CL6                                                              
MX@NUM   DS    CL6                                                              
MX@CURRC DS    CL8                                                              
MX@AMT   DS    CL7                                                              
MX@FCCOM DS    CL15                                                             
MX@FCVAT DS    CL15                                                             
MX@FCAMT DS    CL12                                                             
MX@CMN   DS    CL12                                                             
MX@VATAM DS    CL12                                                             
MX@STT   DS    CL6                                                              
MX@SCM   DS    CL6                                                              
MX@DESC  DS    CL15                                                             
MX@ENQEL DS    CL15                                                             
MX@MEDC  DS    CL10                                                             
MX@DELD  DS    CL20                                                             
MX@ESREJ DS    CL20                                                             
MX@CAPPD DS    CL20                                                             
MX@ESSUB DS    CL20                                                             
MX@INPRO DS    CL20                                                             
MX@INAPP DS    CL20                                                             
MX@MRGED DS    CL20                                                             
MX@SIAPP DS    CL20                                                             
         EJECT                                                                  
SCRLIN1D DSECT                     COVER SCREEN RECORD LINE 1                   
SCR1GLO  DS    CL6                 GLOBAL NUMBER                                
         DS    CL1                                                              
SCR#SCH  DS    0CL8                (XD=Y: SCHEME CODE)                          
SCR_COM  DS    0CL12               (XD=Y: COMMISSION AMOUNT)                    
SCR1SJA  DS    CL12                SJ ACCOUNT                                   
         DS    CL1                                                              
SCR#MDA  DS    0CL8                (XD=Y: D/A MAIN RECORD)                      
SCR_VAT  DS    0CL12               (XD=Y: VAT AMOUNT)                           
SCR1DAT  DS    CL8                 DATE                                         
         DS    CL2                                                              
SCR#BMC  DS    0CL1                (XD=Y: BALLPARK MEDIA CODE)                  
SCR1STA  DS    CL2                 STATUS                                       
         DS    CL1                                                              
SCR#CUR  DS    0CL3                (XD=Y: CURRENCY)                             
SCR1LOC  DS    CL3                 LOCAL NUMBER                                 
         DS    CL1                                                              
SCR#FCA  DS    0CL14               (XD=Y: FC TOTAL)                             
SCR_TOT  DS    0CL14               (XD=Y: GROSS TOTAL)                          
SCR1AMT  DS    CL14                TOTAL AMOUNT                                 
         DS    CL1                                                              
SCR#DES  DS    0CL25               (XD=Y: DESCRIPTION PART 2)                   
SCR1DES  DS    CL25                DESCRIPTION                                  
SCR1LNQ  EQU   *-SCRLIN1D                                                       
         SPACE 1                                                                
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 FORMAT TYPE                                  
TSDFRECQ EQU   1                   RECORD FORMAT                                
TSDFTOTQ EQU   2                   TOTAL FORMAT                                 
TSDGLO#  DS    CL6                 ESTIMATE GLOBAL NUMBER                       
TSDCPJA  DS    CL12                SJ ACCOUNT CODE                              
TSDLOC#  DS    XL1                 ESTIMATE LOCAL NUMBER                        
TSDSTAT1 DS    XL1                 STATUS BYTE 1                                
TSDSTAT2 DS    XL1                 STATUS BYTE 2                                
TSDBPMC  DS    CL1                 BALLPARK MEDIA CODE                          
TSDDATE  DS    XL3                 DATE                                         
TSDSCHC  DS    CL8                 SCHEME CODE                                  
TSDDESC  DS    CL50                DESCRIPTION                                  
TSDCURC  DS    CL3                 CURRENCY                                     
TSDNODP  DS    XL1                 NUMBER OF DECIMAL PLACES                     
TSDTAMT  DS    PL6                 TOTAL AMOUNT                                 
TSDTFCA  DS    PL6                 TOTAL FC AMOUNT                              
TSDTVAM  DS    PL6                 TOTAL VAT AMOUNT                             
TSDTVFC  DS    PL6                 TOTAL VAT FC AMOUNT                          
TSDTCAM  DS    PL6                 TOTAL COMMISSION AMOUNT                      
TSDTCFC  DS    PL6                 TOTAL COMMISSION FC AMOUNT                   
TSDTRDA  DS    XL4                 D/A                                          
TSDLENQ  EQU   *-TSARDATD                                                       
         SPACE 1                                                                
COLTBLD  DSECT                                                                  
COLETRY  DS    XL1                 COLUMN NUMBER (FROM 1 TO 255, 0=EOT)         
COLNAME  DS    AL2                 A(COLUMN NAME)                               
COLNMLN  DS    AL1                 LENGTH OF COLUMN - 1                         
COLINDS  DS    AL1                 COLUMN INDICATOR                             
COLINF   DS    0CL1                COLUMN INFORMATION                           
COLIFRM  DS    CL1                 COLUMN FORMAT - TEXT DEFAULT                 
COLFDAT  EQU   C'D'                                DATE                         
COLFNUM  EQU   C'N'                                NUMERIC                      
COLIALGN DS    CL1                 ALIGN - LEFT DEFAULT                         
COLARGHT EQU   C'R'                        RIGHT                                
COLINAM  DS    CL1                 COLUMN NAME -                                
COLNRENM EQU   C'F'                              NO COLUMN RENAME               
COLITOT  DS    CL1                 COLUMN TOTAL -                               
COLNOTOT EQU   C'G'                               NO TOTAL                      
COLINFLQ EQU   *-COLINF                                                         
COLLN1Q  EQU   *-COLTBLD           LENGTH OF ENTRY                              
*                                                                               
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
         SPACE 2                                                                
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
STATFLAG DS    X                   STATEMENT FLAG                               
STPGRTOT EQU   X'08'               PRINT GRID TOTAL                             
ASTATAB1 DS    A                                                                
ASTATAB2 DS    A                                                                
PCLILEN  DS    XL1                                                              
PPROLEN  DS    XL1                                                              
PJOBLEN  DS    XL1                                                              
KEYINPUT DS    CL1                                                              
MYKEYSV  DS    XL42                                                             
XDOPTN   DS    CL1                                                              
LSTATUS1 DS    XL1                                                              
LSTATUS2 DS    XL1                                                              
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025ACENQ1A   03/16/20'                                      
         END                                                                    
