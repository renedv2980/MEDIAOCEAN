*          DATA SET ACCLB06B   AT LEVEL 251 AS OF 12/22/99                      
*PHASE T62106B                                                                  
CLB06    TITLE '- BILL PROGRAM - UPDATE/DRAFT SCREEN'                           
CLB06    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB6**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         L     RC,AOVERWRK                                                      
         USING UWORKD,RC                                                        
         USING PBPARMS,UWPBPARM                                                 
         L     R5,ALSVALS                                                       
         ST    RE,RELO                                                          
*                                                                               
         GOTO1 SETPBLK                                                          
*                                                                               
         CLI   TWASCRN,S#BILUPD    TEST 1ST TIME (SCREEN LOADED?)               
         BE    UPDPRC                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME                                                          *         
***********************************************************************         
         SPACE 1                                                                
UPDFST   DS    0H                                                               
         L     RE,AGOPBLK          CLEAR SAVED GOBLOCK VALUES                   
         XC    8(GOADM+8-GOBLOCK,RE),8(RE)                                      
         GOTO1 LOADSCR                                                          
         TM    CSINDSG1,CSINDSET   TEST SETUP VALUES SAVED                      
         BNZ   UFST02                                                           
         LA    RF,USCCLIH                                                       
         ST    RF,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
         B     EXIT                                                             
UFST02   MVC   PBDRAFT#,CSBILNUM                                                
         GOTO1 DISJOB                                                           
         GOTO1 VALJOB                                                           
         BNE   EXITN                                                            
         MVI   PBMODE,PBDISQ                                                    
*                                                                               
         CLC   PBDRAFT#,BCSPACES   TEST BILL NUMBER PASSED                      
         BNH   UFST06                                                           
         LA    R2,IOKEY            TEST VALID DRAFT BILL FOR THIS JOB           
         USING PBRPAS,R2                                                        
         XC    PBRPAS,PBRPAS                                                    
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,PBDRAFT#                                                
         MVI   PBRPIND,PBRPIDFT                                                 
         GOTO1 AIO,IOHIGH+IOACCDIR                                              
         BNE   UFST06                                                           
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BNE   UFST06                                                           
         CLC   PBRPJOB,BCJOBCOD                                                 
         BE    UFST10                                                           
         MVC   PBDRAFT#,BCSPACES                                                
         DROP  R2                                                               
*                                                                               
UFST06   OI    PBINDS1,PBILDRA     GET LAST DRAFT BILL# IF EXISTS               
*                                                                               
UFST10   GOTO1 APOSTIT,BOPARM,PBLKD                                             
         B     UPDCUR                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS SCREEN                                                      *         
***********************************************************************         
         SPACE 1                                                                
UPDPRC   DS    0H                                                               
         CLI   CSACT,ACTDRA        TEST ACTION DRAFT                            
         BNE   UPRC02                                                           
         CLI   BCPFKEY,PFK06       TEST PFK HIT (UPDATE)                        
         BNE   UPRC02                                                           
         MVI   CSACT,ACTUPD        SET ACTION UPDATE                            
         GOTO1 ARECACT,CSREC                                                    
*                                                                               
UPRC02   GOTO1 VALJOB                                                           
         BNE   EXITN                                                            
         TM    UWFLAG,UWFJOBC      TEST JOB HAS CHANGED                         
         BZ    UPRC04                                                           
         GOTO1 CLRSCR              CLEAR SCREEN                                 
         MVI   PBMODE,PBDISQ                                                    
         GOTO1 APOSTIT,BOPARM,PBLKD                                             
         LA    RF,USCBLDFH                                                      
         ST    RF,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
         B     EXIT                                                             
*                                                                               
UPRC04   MVI   PBMODE,PBDRAFTQ                                                  
         CLI   CSACT,ACTUPD                                                     
         BNE   *+8                                                              
         MVI   PBMODE,PBLIVEQ                                                   
         GOTO1 APOSTIT,BOPARM,PBLKD                                             
         BNE   EXIT                                                             
         CLI   PBTYPES,0           TEST HAVEN'T ENTERED ANYTHING                
         BNE   EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET DEFAULT CURSOR POSITION                                         *         
***********************************************************************         
         SPACE 1                                                                
UPDCUR   DS    0H                                                               
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
*                                                                               
         MVC   IODAOVER,BCJOBDA                                                 
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING JCBELD,R3                                                        
         XR    RF,RF                                                            
UCUR02   CLI   JCBEL,0                                                          
         BE    UCUR08                                                           
         CLI   JCBEL,JCBELQ                                                     
         BE    *+12                                                             
         IC    RF,JCBLN                                                         
         BXH   R3,RF,UCUR02                                                     
*                                                                               
         LA    RF,USCBLDFH         TEST DRAFT BILL                              
         CLC   USCBLDF,BCSPACES                                                 
         BH    UCUR10                                                           
         LA    RF,USCWOBRH         TEST WRITE-OFFS PENDING                      
         OC    JCBWOF,JCBWOF                                                    
         BNZ   UCUR10                                                           
         LA    RF,USCRVBRH         TEST RECOVERIES PENDING                      
         OC    JCBRCV,JCBRCV                                                    
         BNZ   UCUR10                                                           
         LA    RF,USCXFBRH         TEST TRANSFERS PENDING                       
         OC    JCBXFR,JCBXFR                                                    
         BNZ   UCUR10                                                           
         LA    RF,USCINBRH         TEST INTERNAL-INVOICES PENDING               
         OC    JCBFEE,JCBFEE                                                    
         BNZ   UCUR10                                                           
UCUR08   LA    RF,USCBLDFH         NOTHING TO DO                                
         MVI   FVOMTYP,GTMERR      SO SAY SO                                    
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
*                                                                               
UCUR10   ST    RF,FVADDR                                                        
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC=HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC=LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC=EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LOAD THE SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
LOADSCR  NTR1  ,                                                                
         GOTO1 AOVRSCR,BOPARM,('S#BILUPD',BASOLAYH)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CUCTRY,CTRYGER      CONCEAL GERMAN LANGUAGE FIELDS               
         BE    LOADSCRX                                                         
         XC    USCLANP,USCLANP                                                  
         OI    USCLANGH+FHATD,FHATPR                                            
*                                                                               
LOADSCRX B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* CLEAR SCREEN                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLRSCR   NTR1  ,                                                                
         LA    R3,USCBLDDH         CLEAR FORM BILL DUE DATE ONWARDS             
         USING FHD,R3                                                           
         XR    RF,RF                                                            
CSCR02   ICM   RF,1,FHLN                                                        
         BZ    CSCR10                                                           
         TM    FHAT,FHATPR                                                      
         BO    CSCR08                                                           
         LR    RE,RF                                                            
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH                                                      
         BO    *+8                                                              
         LA    RE,FHDAD(RE)                                                     
         EX    RE,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
         OI    FHOI,FHOITR                                                      
         NI    FHII,FF-FHIIVA                                                   
         MVI   FHIL,0                                                           
CSCR08   BXH   R3,RF,CSCR02                                                     
         DROP  R3                                                               
*                                                                               
CSCR10   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET PBLKD VALUES                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETPBLK  NTR1  ,                                                                
         MVI   PBTYPES,PBBILQ+PBWOFQ+PBRECQ+PBXFRQ+PBFEEQ                       
*                                                                               
         LA    R1,FLDLST           SET FIELD ADDRESSES                          
         LA    R0,FLDLSTN                                                       
SPBLK02  LH    RE,0(R1)                                                         
         LH    RF,2(R1)                                                         
         LA    RE,PBPARMS(RE)                                                   
         LA    RF,TWAD(RF)                                                      
         ST    RF,0(RE)                                                         
         LA    R1,L'FLDLST(R1)                                                  
         BCT   R0,SPBLK02                                                       
*                                                                               
         CLI   CUCTRY,CTRYGER      CONCEAL GERMAN LANGUAGE FIELDS               
         BE    *+10                                                             
         XC    PBALANH,PBALANH                                                  
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY CLIENT/PRODUCT/JOB FIELDS                                   *         
***********************************************************************         
         SPACE 1                                                                
DISJOB   NTR1  ,                                                                
*                                  DISPLAY CLIENT                               
         MVC   USCCLI(L'BCCLICOD),BCCLICOD                                      
         OI    USCCLIH+FHIID,FHIIVA                                             
*                                                                               
         SR    RE,RE               DISPLAY PRODUCT                              
         IC    RE,BCCLILEN                                                      
         LA    R1,BCPROCOD(RE)                                                  
         SR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   USCPRO(0),0(R1)                                                  
         OI    USCPROH+FHIID,FHIIVA                                             
*                                                                               
         IC    RF,BCPROLEN         DISPLAY JOB                                  
         LA    R1,BCJOBCOD(RF)                                                  
         IC    RE,BCJOBLEN                                                      
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   USCJOB(0),0(R1)                                                  
         OI    USCJOBH+FHIID,FHIIVA                                             
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT/PRODUCT/JOB FIELDS                                  *         
***********************************************************************         
         SPACE 1                                                                
VALJOB   NTR1  ,                                                                
         NI    CSINDSG1,FF-CSINDSET                                             
         LA    R2,UWKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
*                                  VALIDATE CLIENT CODE                         
         GOTO1 VACSRCHC,BOPARM,(4,USCCLIH),ATWA,BCCPYPRD,ACOM,         *        
               (X'11',0)                                                        
         MVI   FVMINL,1                                                         
         MVC   FVMAXL,BCCLILEN                                                  
         GOTO1 AFVAL,USCCLIH                                                    
         BE    *+14                                                             
         XC    BCCLI(BCCLIL),BCCLI                                              
         B     EXITN                                                            
         MVC   ACTKACT,FVIFLD                                                   
         TM    USCCLIH+FHIID,FHIIVA                                             
         BNZ   *+12                                                             
         NI    USCPROH+FHIID,FF-FHIIVA                                          
         OI    UWFLAG,UWFJOBC                                                   
         GOTO1 ASETUP,BOPARM,(X'80',ACTKACT),0,0                                
         BNE   EXITN                                                            
*&&US                                                                           
         MVC   ACCODE+(ACTKACT-ACTRECD)(L'BCCLICOD),BCCLICOD                    
         GOTO1 VACSRCHC,BOPARM,USCCLIH,ATWA,(BCCLILEN,0),              *        
               (X'C0',19),ACCODE,(L'BCCLINAM,BCCLINAM)                          
         OI    USCCLIH+FHIID,FHIIVA                                             
*&&                                                                             
*                                  VALIDATE PRODUCT                             
         GOTO1 VACSRCHC,BOPARM,(4,USCPROH),ATWA,BCCPYPRD,              *        
               (BCCLILEN,ACOM),(X'22',BCCLICOD)                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,BCCLILEN                                                      
         IC    RF,BCPROLEN                                                      
         SR    RF,RE                                                            
         STC   RF,FVMAXL                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,USCPROH                                                    
         BE    *+14                                                             
         XC    BCPRO(BCPROL),BCPRO                                              
         B     EXITN                                                            
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    RE,ACTKACT(RE)                                                   
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   0(0,RE),FVIFLD                                                   
         TM    USCPROH+FHIID,FHIIVA                                             
         BNZ   *+12                                                             
         NI    USCJOBH+FHIID,FF-FHIIVA                                          
         OI    UWFLAG,UWFJOBC                                                   
         GOTO1 ASETUP,BOPARM,(X'40',ACTKACT),0,0                                
         BNE   EXITN                                                            
*&&US                                                                           
         MVC   ACCODE+(ACTKACT-ACTRECD)(L'BCPROCOD),BCPROCOD                    
         GOTO1 VACSRCHC,BOPARM,USCPROH,ATWA,(BCPROLEN,BCCLILEN),       *        
               (X'C0',19),ACCODE,(L'BCPRONAM,BCPRONAM)                          
         OI    USCPROH+FHIID,FHIIVA                                             
*&&                                                                             
*                                  VALIDATE JOB CODE                            
         GOTO1 VACSRCHC,BOPARM,(4,USCJOBH),ATWA,BCCPYPRD,              *        
               (BCPROLEN,ACOM),(X'33',BCPROCOD)                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,BCPROLEN                                                      
         IC    RF,BCJOBLEN                                                      
         SR    RF,RE                                                            
         STC   RF,FVMAXL                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,USCJOBH                                                    
         BE    *+14                                                             
         XC    BCJOB(BCJOBL),BCJOB                                              
         B     EXITN                                                            
         SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,ACTKACT(RE)                                                   
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   0(0,RE),FVIFLD                                                   
         TM    USCJOBH+FHIID,FHIIVA                                             
         BNZ   *+8                                                              
         OI    UWFLAG,UWFJOBC                                                   
         GOTO1 ASETUP,BOPARM,(X'20',ACTKACT),0,0                                
         BNE   EXITN                                                            
         DROP  R2                                                               
*&&US                                                                           
         MVC   ACCODE+(ACTKACT-ACTRECD)(L'BCJOBCOD),BCJOBCOD                    
         GOTO1 VACSRCHC,BOPARM,USCJOBH,ATWA,(BCJOBLEN,BCPROLEN),       *        
               (X'C0',19),ACCODE,(L'BCJOBNAM,BCJOBNAM)                          
         OI    USCJOBH+FHIID,FHIIVA                                             
*&&                                                                             
*&&UK                                                                           
         GOTO1 VACSRCHC,BOPARM,USCCLIH,ATWA,(BCCLILEN,0),              *        
               (X'C0',19),ACCODE,(L'BCCLINAM,BCCLINAM)                          
         OI    USCCLIH+FHIID,FHIIVA                                             
         GOTO1 VACSRCHC,BOPARM,USCPROH,ATWA,(BCPROLEN,BCCLILEN),       *        
               (X'C0',19),ACCODE,(L'BCPRONAM,BCPRONAM)                          
         OI    USCPROH+FHIID,FHIIVA                                             
         GOTO1 VACSRCHC,BOPARM,USCJOBH,ATWA,(BCJOBLEN,BCPROLEN),       *        
               (X'C0',19),ACCODE,(L'BCJOBNAM,BCJOBNAM)                          
         OI    USCJOBH+FHIID,FHIIVA                                             
*&&                                                                             
*                                                                               
VALJOBY  B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              LITERALS AND CONSTANTS FOR VALIDATION CSECT           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* FIELD LIST                                                          *         
***********************************************************************         
         SPACE 1                                                                
FLDLST   DS    0XL4                                                             
*                                                                               
         DC    AL2(PBADATH-PBPARMS,USCBLDTH-TWAD)                               
         DC    AL2(PBADRAH-PBPARMS,USCBLDFH-TWAD)                               
         DC    AL2(PBALIVH-PBPARMS,USCBLLVH-TWAD)                               
         DC    AL2(PBADUEH-PBPARMS,USCBLDDH-TWAD)                               
         DC    AL2(PBALANH-PBPARMS,USCLANGH-TWAD)                               
*                                                                               
         DC    AL2(PBBILBAT+(PBAREFH-PBBATCHD)-PBPARMS,USCBLBRH-TWAD)           
         DC    AL2(PBBILBAT+(PBAMOAH-PBBATCHD)-PBPARMS,USCBLMOH-TWAD)           
         DC    AL2(PBWOFBAT+(PBAREFH-PBBATCHD)-PBPARMS,USCWOBRH-TWAD)           
         DC    AL2(PBWOFBAT+(PBAMOAH-PBBATCHD)-PBPARMS,USCWOMOH-TWAD)           
         DC    AL2(PBRECBAT+(PBAREFH-PBBATCHD)-PBPARMS,USCRVBRH-TWAD)           
         DC    AL2(PBRECBAT+(PBAMOAH-PBBATCHD)-PBPARMS,USCRVMOH-TWAD)           
         DC    AL2(PBXFRBAT+(PBAREFH-PBBATCHD)-PBPARMS,USCXFBRH-TWAD)           
         DC    AL2(PBXFRBAT+(PBAMOAH-PBBATCHD)-PBPARMS,USCXFMOH-TWAD)           
         DC    AL2(PBFEEBAT+(PBAREFH-PBBATCHD)-PBPARMS,USCINBRH-TWAD)           
         DC    AL2(PBFEEBAT+(PBAMOAH-PBBATCHD)-PBPARMS,USCINMOH-TWAD)           
*                                                                               
         DC    AL2(PBADEBH-PBPARMS,USCDEBH-TWAD)                                
         DC    AL2(PBADEBNH-PBPARMS,USCDEBNH-TWAD)                              
         DC    AL2(PBASRCPH-PBPARMS,USCSRCAH-TWAD)                              
         DC    AL2(PBASRCH-PBPARMS,USCSRCH-TWAD)                                
         DC    AL2(PBASRCNH-PBPARMS,USCSRCNH-TWAD)                              
         DC    AL2(PBADSCPH-PBPARMS,USCDSCAH-TWAD)                              
         DC    AL2(PBADSCH-PBPARMS,USCDSCH-TWAD)                                
         DC    AL2(PBADSCNH-PBPARMS,USCDSCNH-TWAD)                              
         DC    AL2(PBAACCH-PBPARMS,USCACCH-TWAD)                                
         DC    AL2(PBAACCNH-PBPARMS,USCACCNH-TWAD)                              
         DC    AL2(PBAMNTHH-PBPARMS,USCACC1H-TWAD)                              
*                                                                               
FLDLSTN  EQU   (*-FLDLST)/L'FLDLST                                              
FLDLSTX  DC    AL2(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OVERLAY LOCAL W/S                                                   *         
***********************************************************************         
         SPACE 1                                                                
UWORKD   DSECT                                                                  
RELO     DS    A                                                                
*                                                                               
UWFLAG   DS    XL1                 FLAG                                         
UWFJOBC  EQU   X'80'               JOB HAS CHANGED                              
*                                                                               
UWKEY    DS    CL42                JOB KEY                                      
*                                                                               
UWPBPARM DS    XL(PBPARMSL)                                                     
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBFDBD                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'251ACCLB06B  12/22/99'                                      
         END                                                                    
