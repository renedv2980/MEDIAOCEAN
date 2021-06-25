*          DATA SET ACENQ18    AT LEVEL 016 AS OF 03/08/10                      
*PHASE T62018A                                                                  
T62018   TITLE 'MCS EXPENSE CLAIM LIST'                                         
         SPACE 2                                                                
* USER LVL DATE    LEVEL CHANGE COMMENTS                                        
* ---- --- ------- ---------------------------------------------------          
* TKLU 001 21FEB06 <DU01-4972> NEW VERSION - DDS ONLY                           
* TKLU 002 25APR06 SOME ADDITIONS (DDS ONLY)                                    
* TKLU 003 30JUN06 DATE BUG FIX (FOUND WHEN TESTING MYSELF)                     
* NSHE 004 20MAY09 CHANGE TO EXPENSE RECORD STRUCTURE                           
* MPEN 005 12AUG09 <LO01-9248> RELINK FOR THE NEW EXPENSE RECORD                
         SPACE 2                                                                
T62020   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQ18**,R7,R8,CLEAR=YES,RR=RE                                
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
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1 GET AN ACC DIRECTORY RECORD              
         BE    MAIN10                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
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
         AHI   RF,1                                                             
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
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1 GET AN ACC DIRECTORY RECORD              
         BE    MAIN50                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN50   GOTO1 AIO,IOSEQ+IOACCDIR+IO1 READ SEQ FOR NEXT RECORD                  
         BE    MAIN60                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
         USING EXCRECD,R3                                                       
MAIN60   LA    R3,IOKEY            R3=A(ACCOUNT KEY)                            
         CLI   EXCKTYP,EXCKTYPQ                                                 
         BNE   MAIN150             MATCH ON RELEVANT PART OF KEY?               
         CLI   EXCKSUB,EXCKSUBQ                                                 
         BNE   MAIN150                                                          
         CLC   EXCKCPY,MYCO                                                     
         BNE   MAIN150                                                          
         OC    PERXID,PERXID                                                    
         BZ    MAIN62                                                           
         CLC   PERXID,EXCKPIDB                                                  
         BNE   MAIN150                                                          
*                                                                               
MAIN62   MVC   KEYSAVE,IOKEY       SAVE KEY FOR SEQUENCE RESTORE                
*                                                                               
         BAS   RE,FILTKEY          FILTER ON DIRECTORY RECORD                   
         BNE   MAIN130                                                          
         MVC   DADDRESS,EXCKDA     DISK ADDRESS                                 
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN70                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN70   MVC   IOKEY,KEYSAVE       REREAD LAST RECORD                           
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    MAIN75                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
MAIN75   GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN76                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
MAIN76   CLI   OFFLFLAG,0                                                       
         BNE   MAIN130                                                          
         DROP  R3                                                               
*                                                                               
MAIN80   L     R3,AIO1                                                          
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
         USING EXCRECD,R3                                                       
MAIN140  MVC   KEYSAVE,EXCKEY                                                   
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
MAINX    B     OKXIT                                                            
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
         MVI   TSDFMT,TSITEMT      TOTAL LINE TYPE                              
         MVC   TSDLINES,LINSUSED                                                
         GOTO1 AFGRTSAR,(RC)       FORMAT TSAR ONTO GRID SCREEN LINES           
***      MVC   TSARLINE,LINSUSED                                                
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
                                                                                
         MVI   UNDERS,C'-'                                                      
         MVC   UNDERS+1(L'UNDERS-1),UNDERS                                      
                                                                                
         GOTOR VGETFACT,DMCB,0                                                  
         L     R2,0(R1)                                                         
         USING FACTSD,R2           R1=A(SYSTEM DEFINITION BLOCK)                
         MVC   SECALPHA,FATAGYSC   SET SECURITY AGENCY ALPHA                    
         DROP  R2                                                               
         GOTO1 AUNITLDG,=C'1R'     READ UNIT/LEDGER RECORDS                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING FLDHDRD,R2                                                       
         LA    R2,BASCACH          R2=A(CONTRA ACCOUNT FIELD)                   
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         CLI   FLDILEN,0           INPUT NOT ALLOWED IN THIS FIELD              
         BNE   ERRXIT                                                           
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         OC    SECALPHA,SECALPHA   USE SECURITY AGENCY IF PRESENT               
         BNZ   *+10                                                             
         MVC   SECALPHA,TWAAGY     ELSE NATIVE AGENCY                           
*                                                                               
         XC    PERXID,PERXID                                                    
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         ST    R2,FVADDR                                                        
         CLI   FLDILEN,1                                                        
         BL    FSTD20                                                           
         MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         CLI   FLDILEN,8                                                        
         BH    ERRXIT                                                           
         MVC   PERCID,SPACES                                                    
         XR    R1,R1                                                            
         IC    R1,FLDILEN                                                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERCID(0),FLDDATA                                                
*                                                                               
         USING SAPEREC,R3                                                       
         LA    R3,IOKEY                                                         
         XC    SAPEKEY,SAPEKEY     BUILD KEY TO READ                            
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         OC    SAPEAGY,SECALPHA                                                 
         MVC   SAPEPID,PERCID                                                   
         MVC   SAVEKEY,IOKEY                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',IOKEY,AIO2                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO2             MATCH KEY ON ALL BUT EFFECTIVE DATE          
         CLC   SAPEKEY(L'SAPEKEY-L'SAPEDEF),SAVEKEY                             
         BE    FSTD04                                                           
FSTD02   MVC   FVMSGNO,=AL2(AE$INPID)                                           
         B     ERRXIT                                                           
*                                                                               
FSTD04   LA    R3,SAPEDATA                                                      
         USING SAPWDD,R3                                                        
         XR    R0,R0                                                            
FSTD06   CLI   SAPWDEL,SAPWDELQ                                                 
         BE    FSTD08                                                           
         CLI   SAPWDEL,0                                                        
         BE    FSTD02                                                           
         IC    R0,SAPWDLN                                                       
         AR    R3,R0                                                            
         B     FSTD06                                                           
*                                                                               
FSTD08   MVC   PERXID,SAPWDNUM     PASS 2 CHAR HEX PID                          
         DROP  R3                                                               
*                                                                               
FSTD20   LA    R2,BASOPTH          R2=A(KEY FIELD)                              
         ST    R2,FVADDR                                                        
         USING OPTVALSD,RF                                                      
         L     RF,AOPTVALS         CHECK FOR VALID OPTIONS                      
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
*                                                                               
         MVI   LSTATUS,X'FF'       ALL STATUS'                                  
         CLI   OSTAT,0             ANY OPTION SET?                              
         BE    FSTD30                                                           
         LA    RE,STATAB                                                        
FSTD22   CLI   0(RE),0                                                          
         BE    ERRXIT                                                           
         CLC   OSTAT,0(RE)                                                      
         BE    FSTD24                                                           
         AHI   RE,2                                                             
         B     FSTD22                                                           
*                                                                               
FSTD24   MVC   LSTATUS,1(RE)                                                    
*                                                                               
FSTD30   XC    LENDDAT,LENDDAT                                                  
         MVC   LSTADAT,=X'FFFF'                                                 
         CLI   ODATEFI,NEGFILTR                                                 
         BE    ERRXIT                                                           
         OC    ODATE,ODATE                                                      
         BZ    FSTD32                                                           
         GOTO1 VDATCON,DMCB,(1,ODATEST),(2,DUB)                                 
         L     RF,AOPTVALS                                                      
         XR    R1,R1                                                            
         ICM   R1,3,DUB                                                         
         LNR   R1,R1                                                            
         STCM  R1,3,LSTADAT                                                     
         GOTO1 VDATCON,DMCB,(1,ODATEEN),(2,DUB)                                 
         L     RF,AOPTVALS                                                      
         XR    R1,R1                                                            
         ICM   R1,3,DUB                                                         
         LNR   R1,R1                                                            
         STCM  R1,3,LENDDAT                                                     
*                                                                               
FSTD32   CLI   OREFFI,NEGFILTR                                                  
         BE    ERRXIT                                                           
*                                                                               
FSTD50   DS    0H                  NO MORE YET                                  
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         ST    R2,FVADDR                                                        
         DROP  RF                                                               
*                                                                               
         USING EXCRECD,R3                                                       
         USING OPTVALSD,RF                                                      
FSTD80   LA    R3,IOKEY            BUILD START KEY                              
         L     RF,AOPTVALS                                                      
         XC    EXCKEY,EXCKEY                                                    
         MVI   EXCKTYP,EXCKTYPQ                                                 
         MVI   EXCKSUB,EXCKSUBQ                                                 
         MVC   EXCKCPY,MYCO                                                     
         MVC   EXCKPIDB,PERXID                                                  
         MVC   EXCKDATE,LENDDAT                                                 
         CLI   ODRAFT,C' '                                                      
         BH    *+8                                                              
         MVI   ODRAFT,C'N'                                                      
         MVI   EXCKTYPE,EXNPTLQ                                                 
         CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BNE   *+8                                                              
         MVI   EXCKTYPE,EXNPTDQ                                                 
         CLI   ODRAFT,C'Y'         DRAFTS?                                      
         BNE   *+8                                                              
         MVI   EXCKTYPE,EXNPTDQ                                                 
         MVC   EXCKREF,OREFVL1                                                  
         MVC   EXCK1RAC,SPACES                                                  
         DROP  RF                                                               
*                                                                               
         MVI   STATFLAG,0                                                       
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BNZ   FSTD90                                                           
         LA    R2,ENQDAT1H         DISPLAY SCREEN HEADINGS                      
         MVC   FLDDATA(L'MX@ENH46),MX@ENH46                                     
         OI    FLDOIND,FOUTPRT                                                  
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         LA    RE,FLDDATA                                                       
         USING SCRLIN1D,RE                                                      
         MVC   SCR1PER,UNDERS                                                   
         MVC   SCR1DAT,UNDERS                                                   
         MVC   SCR1TYP,UNDERS                                                   
         MVC   SCR1SLA,UNDERS                                                   
         MVC   SCR1NUM,UNDERS                                                   
         MVC   SCR1DSC,UNDERS                                                   
         MVC   SCR1OFF,UNDERS                                                   
         MVC   SCR1STA,UNDERS                                                   
         MVC   SCR1APP,UNDERS                                                   
         MVC   SCR1ITM,UNDERS                                                   
         MVC   SCR1AMT,UNDERS                                                   
         MVC   SCR1DAD,UNDERS                                                   
         DROP  RE                                                               
         OI    FLDOIND,FOUTPRT                                                  
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         B     FSTD92                                                           
*                                                                               
FSTD90   LA    R2,GRDDAT1H                                                      
*                                                                               
FSTD92   GOTO1 ASCRNDIM,DMCB,(0,(R2)) GET SCREEN DIMENSIONS                     
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BNZ   FSTDX                                                            
***      L     RF,ADISPFK          DON'T DO AS NO PFKEYS THERE                  
***      BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
STATAB   DC    C'S',AL1(EXCSSUBM)                                               
         DC    C'P',AL1(EXCSPAPP)                                               
         DC    C'C',AL1(EXCSCOMP)                                               
         DC    C'F',AL1(EXCSFNTA)                                               
         DC    C'R',AL1(EXCSREJE)                                               
         DC    C'D',AL1(EXCSLOGD)                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        FILTER ACCOUNT DIRECTORY RECORD                              *         
* ON ENTRY R3=A(ACCOUNT KEY)                                          *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         SPACE 1                                                                
FILTKEY  NTR1                                                                   
         USING EXCRECD,R3                                                       
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
*                                                                               
         CLI   EXCKSEQ,0                                                        
         BNE   FILTKRJX                                                         
*                                                                               
         CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BNE   FILTK00                                                          
         CLI   EXCKTYPE,EXNPTDQ                                                 
         BE    FILTK02                                                          
         MVI   EXCKTYPE,X'FF'      SKIP 'LIVE'                                  
         B     FILTKRJX                                                         
*                                                                               
FILTK00  CLI   ODRAFT,C'N'         NO DRAFTS?                                   
         BNE   FILTK02                                                          
         CLI   EXCKTYPE,EXNPTDQ                                                 
         BNE   FILTK02                                                          
         MVI   EXCKTYPE,EXNPTLQ    SKIP 'DRAFT'                                 
         B     FILTKRJX                                                         
*                                                                               
FILTK02  CLC   EXCKDATE,LSTADAT    DATE RANGE                                   
         BNH   FILTK04                                                          
         MVC   EXCKDATE,LSTADAT                                                 
         B     FILTKRJX                                                         
*                                                                               
FILTK04  CLC   EXCKDATE,LENDDAT                                                 
         BNL   FILTK06                                                          
         MVC   EXCKDATE,LENDDAT                                                 
         B     FILTKRJX                                                         
*                                                                               
FILTK06  OC    OOFFICVL,OOFFICVL   OFFICE CODE FILTER?                          
         BZ    FILTK10                                                          
         LA    RE,OOFFICVL+2                                                    
         TM    COMPSTA4,CPYSOFF2                                                
         BZ    *+8                                                              
         LA    RE,OOFFICVL                                                      
         XR    RF,RF                                                            
         TM    COMPSTA4,CPYSOFF2                                                
         BZ    *+8                                                              
         LA    RF,1                                                             
         CLI   OOFFICFI,NEGFILTR                                                
         BE    FILTK08                                                          
         EX    RF,*+8                                                           
         BNE   FILTKRJX                                                         
         CLC   EXCK1RAC(0),0(RE)                                                
         B     FILTK10                                                          
*                                                                               
FILTK08  EX    RF,*+8                                                           
         BE    FILTKRJX                                                         
         CLC   EXCK1RAC(0),0(RE)                                                
*                                                                               
FILTK10  CLI   LSTATUS,X'FF'       ALL STATUS?                                  
         BE    FILTK12                                                          
         DS    0H                  O/S                                          
*                                                                               
FILTK12  MVI   OFFLFLAG,0                                                       
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1           R1=A(OFFAL CONTROL BLOCK)                    
         XR    RF,RF                                                            
         TM    COMPSTA4,CPYSOFF2                                                
         BZ    *+8                                                              
         LA    RF,1                                                             
         MVC   OFFAOFFC(0),EXCK1RAC   SET OFFICE IN OFFBLK                      
         EX    RF,*-6                                                           
         OC    OFFALIMA,OFFALIMA   TEST LIMIT ACCESS                            
         BZ    FILTK16                                                          
         MVI   OFFAACT,OFFAVAL     TEST OFFICE LIMIT ACCESS                     
         GOTO1 VOFFAL                                                           
         BNE   FILTKRJX            OFFICE LOCKOUT                               
         DROP  R1                                                               
*                                                                               
FILTK16  OC    OREF,OREF           REFERENCE RANGE?                             
         BZ    FILTK20                                                          
         CLI   OREFLN1,0                                                        
         BE    FILTK18                                                          
         XR    R1,R1                                                            
         IC    R1,OREFLN1                                                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         BL    FILTKRJX                                                         
         CLC   OREFVL1,EXCKREF                                                  
*                                                                               
FILTK18  CLI   OREFLN2,0                                                        
         BE    FILTK20                                                          
         XR    R1,R1                                                            
         IC    R1,OREFLN2                                                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         BH    FILTKRJX                                                         
         CLC   OREFVL2,EXCKREF                                                  
*                                                                               
FILTK20  DS    0H                                                               
*                                                                               
FILTKX   CR    RB,RB                                                            
         B     XIT                                                              
FILTKRJX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
***********************************************************************         
*        FILTER ACCOUNT DATA RECORD                                   *         
* ON ENTRY R3=A(ACCOUNT RECORD)                                       *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         SPACE 1                                                                
FILTREC  NTR1                                                                   
         USING EXCRECD,R3                                                       
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
*                                                                               
FILTR02  DS    0H                                                               
*                                                                               
FILTRX   CR    RB,RB                                                            
         B     XIT                                                              
FILTRRJX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORDS AND DISPLAY IF REQUIRED                   *         
***********************************************************************         
         SPACE 1                                                                
         USING EXCRECD,R3                                                       
         USING CLDELD,R5                                                        
BLDDIS   NTR1                                                                   
         LA    R5,EXCRFST                                                       
         XR    R0,R0                                                            
BLDD02   CLI   CLDEL,CLDELQ                                                     
         BE    BLDD04                                                           
         CLI   CLDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,CLDLN                                                         
         AR    R5,R0                                                            
         B     BLDD02                                                           
*                                                                               
BLDD04   L     R0,ATSARREC         CLEAR TSAR RECORD                            
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
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
*                                                                               
         MVC   TSDTDAD,DADDRESS                                                 
         XR    R1,R1                                                            
         ICM   R1,3,EXCKDATE                                                    
         LNR   R1,R1                                                            
         STCM  R1,3,MYDUB                                                       
         GOTO1 VDATCON,DMCB,(2,MYDUB),(1,TSDTDAT)                               
         MVC   TSDTTYP,EXCKTYPE                                                 
         MVC   TSDTCLN,EXCKREF                                                  
         MVC   TSDTCLD,SPACES                                                   
         XR    R1,R1                                                            
         IC    R1,CLDLN                                                         
         SHI   R1,CLDLNQ+1                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSDTCLD(0),CLDDESC                                               
         LLC   RF,LEDGTLVA                                                      
         SHI   RF,1                                                             
         MVC   TSDTOFF(0),EXCK1RAC                                              
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         LA    R1,EXCK1RAC(RF)                                                  
         LLC   RE,LEDGTLVB                                                      
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         MVC   TSDTDEP(0),0(R1)                                                 
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         LA    R1,0(RE,R1)                                                      
         LLC   RF,LEDGTLVC                                                      
         LLC   RE,LEDGTLVB                                                      
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         MVC   TSDTSUB(0),0(R1)                                                 
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         LA    R1,0(RF,R1)                                                      
         LLC   RF,LEDGTLVD                                                      
         LLC   RE,LEDGTLVC                                                      
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         MVC   TSDTPER(0),0(R1)                                                 
         EX    RF,*-6                                                           
         MVC   TSDTSTA,EXCRSTAT                                                 
         MVC   TSDTST2,EXCRSTA2                                                 
         MVC   TSDTIDN,CLDIDN                                                   
         MVC   TSDTUID,CLDUID                                                   
         ZAP   TSDTTOT,CLDTAMT                                                  
         ZAP   TSDTNOA,EXCRNAPP                                                 
         ZAP   TSDTNOI,EXCRNITM                                                 
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    BLDD06                                                           
         GOTO1 AFGRTSAR,(RC)       FORMAT TSAR ONTO GRID SCREEN LINES           
         B     BLDD08                                                           
*                                                                               
BLDD06   BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
BLDD08   MVC   TSDLINES,LINSUSED   NUMBER OF SCREEN LINES USED                  
         MVC   TSARLINE,LINSUSED                                                
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
         USING SCRLIN1D,R2         ACCOUNT DATA LINE                            
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         MVC   SCR1PER,TSDTPER                                                  
         MVC   SCR1TYP,TSDTTYP                                                  
         MVI   SCR1SLA,C'/'                                                     
         MVC   SCR1NUM,TSDTCLN                                                  
         MVC   SCR1DSC,TSDTCLD                                                  
         CLC   TSDTCLD+L'SCR1DSC(L'TSDTCLD-L'SCR1DSC),SPACES                    
         BE    FORM02                                                           
         L     RF,AOPTVALS                                                      
         CLI   OXDETAIL-OPTVALSD(RF),C'Y'                                       
         BE    FORM02                                                           
         MVC   SCR1DSC+L'SCR1DSC-3(3),=C'...'                                   
FORM02   MVC   SCR1OFF,TSDTOFF                                                  
         GOTO1 VDATCON,DMCB,(1,TSDTDAT),(17,SCR1DAT)                            
         CLI   TSDTSTA,0                                                        
         BNE   FORM04                                                           
         MVI   SCR1STA,C'A'                                                     
         B     FORM10                                                           
FORM04   LA    RF,STATAB                                                        
FORM06   CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   1(1,RF),TSDTSTA                                                  
         BE    FORM08                                                           
         AHI   RF,2                                                             
         B     FORM06                                                           
FORM08   MVC   SCR1STA(1),0(RF)                                                 
FORM10   CURED (P6,TSDTTOT),(L'SCR1AMT,SCR1AMT),2,MINUS=YES,ZERO=YES            
         CURED (P2,TSDTNOA),(L'SCR1APP,SCR1APP),0,MINUS=YES,ZERO=YES            
         CURED (P2,TSDTNOI),(L'SCR1ITM,SCR1ITM),0,MINUS=YES,ZERO=YES            
         CLI   TWAOFFC,C'*'        DDS?                                         
         BNE   FORM20                                                           
         XOUT  TSDTDAD,SCR1DAD,4                                                
         SPACE 1                                                                
FORM20   MVI   LINSUSED,1                                                       
         L     RF,AOPTVALS                                                      
         CLI   OXDETAIL-OPTVALSD(RF),C'Y' XTRA DETAIL REQUIRED?                 
         BNE   FORMX                                                            
         MVI   LINSUSED,2                                                       
         AHI   R2,L'DUMLIN1                                                     
         MVC   SCR1LI2,=CL3'>>>'                                                
         MVC   SCR1DEP,TSDTDEP                                                  
         MVC   SCR1SUB,TSDTSUB                                                  
         XOUT  TSDTIDN,SCR1IDN,2                                                
         TM    TSDTST2,EXCKSADQ                                                 
         BZ    *+8                                                              
         MVI   SCR1ST2,C'A'                                                     
         TM    TSDTST2,EXCKSFCQ                                                 
         BZ    *+8                                                              
         MVI   SCR1ST2+1,C'F'                                                   
         TM    TSDTST2,EXCKSLMQ                                                 
         BZ    *+8                                                              
         MVI   SCR1ST2+2,C'L'                                                   
         TM    TSDTST2,EXCKSCLQ                                                 
         BZ    *+8                                                              
         MVI   SCR1ST2+3,C'C'                                                   
         TM    TSDTST2,EXCKSFRQ                                                 
         BZ    *+8                                                              
         MVI   SCR1ST2+4,C'F'                                                   
         MVC   SCR1DSC(L'TSDTCLD-L'SCR1DSC),TSDTCLD+L'SCR1DSC                   
         MVC   KEYSAVE2,IOKEY                                                   
         USING CTIREC,R3                                                        
         LA    R3,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TSDTUID                                                  
         MVI   SCR1UID,C'?'                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',IOKEY,AIO2                   
         BNE   FORM25              CAN'T FIND USERID RECORD                     
         L     R3,AIO2                                                          
         LA    R3,CTIDATA                                                       
         DROP  R3                                                               
         XR    RE,RE                                                            
         IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'02'         ID DESCRIPTION                               
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    RE,1(R3)                                                         
         SHI   RE,3                                                             
         CHI   RE,L'SCR1UID-1                                                   
         BL    *+8                                                              
         LA    RE,L'SCR1UID-1                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SCR1UID(0),2(R3)                                                 
FORM25   MVC   IOKEY,KEYSAVE2      REESTABLISH SEQ                              
         CLC   IOKEY,SPACES                                                     
         BE    FORM27                                                           
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    FORM27                                                           
         DC    H'0'                                                             
FORM27   DS    0H                                                               
         SPACE 1                                                                
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
         SPACE 1                                                                
DCMIX    DS    0X                                                               
         DCDDL AC#OFF,L'MX@OFF                                                  
         DCDDL AC#CPID,L'MX@CPID                                                
         DCDDL AC#DATE,L'MX@DATE                                                
         DCDDL AC#TYPE,L'MX@TYPE                                                
         DCDDL AC#AMT,L'MX@AMT                                                  
         DCDDL AC#DESC,L'MX@DESC                                                
         DCDDL AC#NUM,L'MX@NUM                                                  
         DCDDL AC#APRVR,L'MX@APRVR                                              
         DCDDL AC#ITEMS,L'MX@ITEMS                                              
         DCDDL AC#ENH46,L'MX@ENH46,L                                            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*        GRID COLUMN TABLE                                            *         
***********************************************************************         
COLTBL   DS    0F                                                               
*                                                                               
COL1     DC    AL1(COLPID)                                                      
COLPID   EQU   X'01'                                                            
         DC    AL2(MX@CPID-OVERWRKD)                                            
         DC    AL1(L'MX@CPID-1,0,0,0,0,0)                                       
*                                                                               
COL2     DC    AL1(COLDAT)                                                      
COLDAT   EQU   X'02'                                                            
         DC    AL2(MX@DATE-OVERWRKD)                                            
         DC    AL1(L'MX@DATE-1,0,COLFDAT,COLARGHT,0,0)                          
*                                                                               
COL3     DC    AL1(COLTYP)                                                      
COLTYP   EQU   X'03'                                                            
         DC    AL2(MX@TYPE-OVERWRKD)                                            
         DC    AL1(L'MX@TYPE-1,0,0,0,0,0)                                       
*                                                                               
COL4     DC    AL1(COLNUM)                                                      
COLNUM   EQU   X'04'                                                            
         DC    AL2(MX@NUM-OVERWRKD)                                             
         DC    AL1(L'MX@NUM-1,0,0,0,0,0)                                        
*                                                                               
COL5     DC    AL1(COLDSC)                                                      
COLDSC   EQU   X'05'                                                            
         DC    AL2(MX@DESC-OVERWRKD)                                            
         DC    AL1(L'MX@DESC-1,0,0,0,0,0)                                       
*                                                                               
COL6     DC    AL1(COLOFF)                                                      
COLOFF   EQU   X'06'                                                            
         DC    AL2(MX@OFF-OVERWRKD)                                             
         DC    AL1(L'MX@OFF-1,0,0,0,0,0)                                        
*                                                                               
COL7     DC    AL1(COLAPP)                                                      
COLAPP   EQU   X'07'                                                            
         DC    AL2(MX@APRVR-OVERWRKD)                                           
         DC    AL1(L'MX@APRVR-1,0,COLFNUM,COLARGHT,0,0)                         
*                                                                               
COL8     DC    AL1(COLITM)                                                      
COLITM   EQU   X'08'                                                            
         DC    AL2(MX@ITEMS-OVERWRKD)                                           
         DC    AL1(L'MX@ITEMS-1,0,COLFNUM,COLARGHT,0,0)                         
*                                                                               
COL9     DC    AL1(COLAMT)                                                      
COLAMT   EQU   X'09'                                                            
         DC    AL2(MX@AMT-OVERWRKD)                                             
         DC    AL1(L'MX@AMT-1,0,COLFNUM,COLARGHT,0,0)                           
*                                                                               
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
         BNZ   FGRT030                                                          
*                                                                               
         L     R3,ATSARREC         R3=A(TSAR RECORD )                           
         LA    R3,TSARDATA-TSARRECD(R3)                                         
         USING TSARDATD,R3                                                      
         CLI   TSDFMT,TSITEMT      TOTAL LINE TYPE                              
         BE    FGRT030                                                          
*                                                                               
         MVC   DLCBFLD(L'SCR1PER),TSDTPER                                       
         BAS   RE,FGDLTXT          COLUMN 1: PERSON CODE                        
         GOTO1 VDATCON,DMCB,(1,TSDTDAT),(10,DLCBFLD)                            
         BAS   RE,FGDLTXT          COLUMN 2: DATE                               
         MVC   DLCBFLD(L'SCR1TYP),TSDTTYP                                       
         BAS   RE,FGDLTXT          COLUMN 3: TYPE                               
         MVC   DLCBFLD(L'TSDTCLN),TSDTCLN                                       
         BAS   RE,FGDLTXT          COLUMN 4: NUMBER                             
         MVC   DLCBFLD(L'TSDTCLD),TSDTCLD                                       
         BAS   RE,FGDLTXT          COLUMN 5: DESCRIPTION                        
         MVC   DLCBFLD(L'TSDTOFF),TSDTOFF                                       
         CLI   TWAOFFC,C'*'        DDS?                                         
         BNE   FGRT020                                                          
         XOUT  TSDTDAD,DLCBFLD+37,4                                             
FGRT020  BAS   RE,FGDLTXT          COLUMN 6: OFFICE                             
         CURED (P2,TSDTNOA),(AMTLNQ,DLCBFLD),2,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
         BAS   RE,FGDLNUM          COLUMN 7: # OF APPROVERS                     
         CURED (P2,TSDTNOI),(AMTLNQ,DLCBFLD),2,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
         BAS   RE,FGDLNUM          COLUMN 8: # OF ITEMS                         
         CURED (P6,TSDTTOT),(AMTLNQ,DLCBFLD),2,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
         BAS   RE,FGDLNUM          COLUMN 9: AMOUNT                             
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 VDLFLD,DLCBD                                                     
         XR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         A     RF,DLCBTOTL         NUM OF LINES PUT TO DUMMY SCREEN             
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
         B     FGRTXIT                                                          
*                                                                               
FGRT030  L     RF,ATSARREC         RF=A(TSAR RECORD )                           
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
IKIPIND  DS    XL1                                                              
CURROFF  DS    CL2                                                              
*                                                                               
MYDUB    DS    D                                                                
MYBYTE1  DS    XL1                                                              
MYBYTE2  DS    XL1                                                              
*                                                                               
INTFLAG  DS    X     INTERNAL FLAG                                              
SCRFULLQ EQU   1     SCREEN IS FULL                                             
         SPACE 1                                                                
DSMIX    DS    0C                                                               
MX@OFF   DS    CL8                                                              
MX@CPID  DS    CL12                                                             
MX@DATE  DS    CL6                                                              
MX@TYPE  DS    CL6                                                              
MX@AMT   DS    CL7                                                              
MX@DESC  DS    CL20                                                             
MX@NUM   DS    CL10                                                             
MX@APRVR DS    CL15                                                             
MX@ITEMS DS    CL12                                                             
MX@ENH46 DS    CL(L'LSTDAT2)                                                    
         EJECT                                                                  
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1PER  DS    CL8                 PERSON CODE                                  
         DS    CL1                                                              
SCR1DAT  DS    CL8                 DATE                                         
         DS    CL1                                                              
SCR1TYP  DS    CL1                 DRAFT/LIVE                                   
SCR1SLA  DS    CL1                                                              
SCR1NUM  DS    CL6                 CLAIM #                                      
         DS    CL1                                                              
SCR1DSC  DS    CL17                DESCRIPTION                                  
         DS    CL1                                                              
SCR1OFF  DS    CL2                 OFFICE                                       
         DS    CL1                                                              
SCR1STA  DS    CL2                 STATUS                                       
         DS    CL1                                                              
SCR1APP  DS    CL3                 NUMBER OF APPROVERS                          
         DS    CL1                                                              
SCR1ITM  DS    CL3                 NUMBER OF ITEMS                              
         DS    CL1                                                              
SCR1AMT  DS    CL10                AMOUNT                                       
         DS    CL1                                                              
SCR1DAD  DS    CL8                 D/A                                          
SCR1LNQ  EQU   *-SCRLIN1D                                                       
         ORG   SCR1PER                                                          
         DS    CL1                                                              
SCR1LI2  DS    CL3                 LINE 2                                       
         ORG   SCR1DAT                                                          
SCR1DEP  DS    CL2                 DEPARTMENT                                   
         DS    CL1                                                              
SCR1SUB  DS    CL2                 SUB DEPARTMENT                               
         ORG   SCR1TYP                                                          
SCR1ST2  DS    CL5                 STATUS 2                                     
         ORG   SCR1AMT                                                          
         DS    CL5                                                              
SCR1IDN  DS    CL4                 ID NUMBER                                    
         ORG   SCR1DAD                                                          
SCR1UID  DS    CL8                 USER ID                                      
         SPACE 1                                                                
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSITEMT  EQU   2                   TOTAL FORMAT                                 
TSDTPER  DS    CL8                 CLAIMER CODE                                 
TSDTDAT  DS    XL3                 DATE                                         
TSDTTYP  DS    CL1                 DRAFT/LIVE                                   
TSDTCLN  DS    CL6                 CLAIM #                                      
TSDTCLD  DS    CL36                DESCRIPTION                                  
TSDTOFF  DS    CL2                 OFFICE                                       
TSDTSTA  DS    XL1                 RECORD STATUS                                
TSDTST2  DS    XL1                 RECORD STATUS 2                              
TSDTDEP  DS    CL2                 DEPARTMENT                                   
TSDTSUB  DS    CL2                 SUB DEPARTMENT                               
TSDTIDN  DS    XL2                 ID NUMBER                                    
TSDTUID  DS    XL2                 USER ID                                      
TSDTTOT  DS    PL6                 TOTAL AMOUNT                                 
TSDTNOA  DS    PL2                 NUMBER OF APPROVERS                          
TSDTNOI  DS    PL2                 NUMBER OF ITEMS ON CLAIM                     
TSDTDAD  DS    XL4                 D/A                                          
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
         EJECT                                                                  
* ACENQWORK AND SEACSFILE                                                       
         PRINT OFF                                                              
       ++INCLUDE ACENQWORK                                                      
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
STATFLAG DS    X                   STATEMENT FLAG                               
STPGRTOT EQU   X'08'               PRINT GRID TOTAL                             
SAVEKEY  DS    XL64                                                             
PERXID   DS    XL2                                                              
LSTATUS  DS    XL1                                                              
LSTADAT  DS    XL2                                                              
LENDDAT  DS    XL2                                                              
SECALPHA DS    CL2                                                              
PERCID   DS    CL8                                                              
UNDERS   DS    CL25                                                             
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACENQ18   03/08/10'                                      
         END                                                                    
