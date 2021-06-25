*          DATA SET SPNWS04    AT LEVEL 151 AS OF 01/22/08                      
*PHASE T20704A,*                                                                
*INCLUDE SPAUTH                                                                 
         TITLE 'NWS04 - BUYERS WORK SHEET - DETAIL FILE MAINT OVERLAY'          
T20704   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 EXTRAWKL,T20704**,RA,RR=RE                                       
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         LR    R1,RC                                                            
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    R1,AEXTRAWK         SAVE A(EXTRA WORKING STORAGE)                
         USING EXTRAWKD,R1                                                      
         XC    LSTDARKY,LSTDARKY                                                
         DROP  R1                                                               
*                                                                               
         ST    R6,LASAVE                                                        
         ST    RE,APRELO                                                        
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         LA    R2,IOKEY            R2=A(CAMPAIGN MARKET HEADER KEY)             
         USING BWHRECD,R2                                                       
         LA    R3,APRECKEY         R3=A(CAMPAIGN MARKET DETAIL KEY)             
         USING BWDRECD,R3                                                       
*                                                                               
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(EXTRA)                                                     
         A     R1,APRELO                                                        
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELREC                                                           
         B     EXIT    RESREC                                                   
         B     VALPAR                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     VALSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     FSTSCR                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   XC    BWHKEY,BWHKEY       SET UP HEADER KEY                            
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
         L     RE,AEXTRAWK                                                      
         LA    RF,EXTRAWKL                                                      
         XCEFL ,                                                                
*                                                                               
         GOTO1 AVALMED,DETMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         GOTO1 AVALBYR,DETBYRH     VALIDATE BUYER                               
         BNE   VALKX                                                            
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW         BUYER PASSWORD                               
         OC    BWHKAGMD,BBYRMASK                                                
         BZ    VALK2                                                            
         GOTO1 AVALPWD                                                          
         BNE   VALKX                                                            
*                                                                               
VALK2    GOTO1 AVALCAM,DETNUMH     VALIDATE CAMPAIGN NUMBER                     
         BNE   VALKX                                                            
         MVC   BWHKCAM,BCAM                                                     
         GOTO1 AGETCLT,CMPCLTC     (GET CLIENT)                                 
         BNE   VALKX                                                            
         BRAS  RE,COS2CHK          CHECK FOR COS2!!                             
         BNE   VALKX               ERROR OUT                                    
*                                                                               
VALK2E   GOTO1 AGETPRD,CMPPRDN     (GET PRODUCT)                                
         BNE   VALKX                                                            
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BNE   VALKX                                                            
         LA    R8,ESTDEMS          FORMAT DEMO NAMES TO SCREEN                  
         BAS   RE,FMTDEMS                                                       
*                                                                               
         GOTO1 AVALSTA,DETSTAH     VALIDATE STATION                             
         BNE   VALKX                                                            
         MVC   BWHKMKT,BMKT        STATION'S MARKET                             
         XC    LESTREP,LESTREP                                                  
****  NEED TO CHECK THE ESTREP FIELD TO SEE IF WE HAVE DEFAULT                  
         OC    ESTREP,ESTREP       ANYTHING?                                    
         BZ    VALK2G               - NOPE                                      
                                                                                
*     WE NEED LESTREP FILLED EVEN IF DETREPN HAS SOMETHING!!                    
         GOTO1 VCOLY,APPARM,0,X'D9000ABC'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),APPARM,(C'U',ESTREP),LESTREP                                
****                                MHC  06/10/04                               
         OC    DETREPN,DETREPN     SOMETHING IN DETAIL SCREEN FOR REP?          
         BNZ   VALK2G               - YUP!                                      
*                                                                               
****  CODE FOR THE NEW REP FIELD ON THE SCREEN                                  
         MVC   DETREPN,LESTREP                                                  
VALK2F   MVI   DETREPNH+5,3                                                     
         OI    DETREPNH+6,X'80'                                                 
****                                 MHC  06/10/04                              
*                                                                               
VALK2G   CLI   APRECNUM,RECPKG     SKIP DAYS/TIMES FOR PACKAGE/ORBIT            
         BE    VALK4                                   RECORDS                  
         CLI   APRECNUM,RECORB                                                  
         BE    VALK4                                                            
*                                                                               
*** WE NEED TO SEE IF THERE ARE SPACES IN THE DAYS FIELD, SPACE NO GOOD         
         CLI   DETDAYH+5,0         ANY INPUT?                                   
         BE    VALK3C               - NAH, USE THE SUBROUTINE                   
*                                                                               
         ZIC   RE,DETDAYH+5        OUR COUNTER FOR CHECKING SPACES              
         LA    RF,DETDAYH+8        LOAD THE FIELD INTO RF                       
*                                                                               
VALK3A   CLI   0(RF),C' '          DO WE HAVE A SPACE?                          
         BNE   VALK3B               - NOPE, CONTINUE                            
         MVC   FVMSGNO,=AL2(FVIDAYS)                                            
         LA    R1,DETDAYH          PLACE CURSOR IN DAYS FIELD                   
         ST    R1,FVADDR                                                        
         B     VALKX                - YUP, GET OUTTA HERE                       
*                                                                               
VALK3B   LA    RF,1(RF)            BUMP TO NEXT CHAR IN FIELD                   
         BCT   RE,VALK3A                                                        
***                                    MHC  04/30/03                            
VALK3C   GOTO1 AVALDAY,DETDAYH     VALIDATE DAYS                                
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALTIM,DETTIMH     VALIDATE TIMES                               
         BNE   VALKX                                                            
*                                                                               
VALK4    GOTO1 AVALDPL,DETDPLH     VALIDATE DAYPART/LENGTH                      
         BNE   VALKX                                                            
         CLI   BDPT,0                                                           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVIDPT)                                             
         B     VALKX                                                            
         CLI   BSLN,0                                                           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVISLN)                                             
         B     VALKX                                                            
*                                                                               
         CLI   CMPDPOPT,C'M'       TEST SUBDPTS SCHEDULED UNDER MASTER          
         BNE   *+12                                                             
         CLI   DPTTYPE,C'S'        YES-TEST THIS IS A SUBDAYPART                
         BE    ESUB                    YES-ERROR                                
*                                                                               
         GOTO1 AIO,DIRHID+IO1      READ CAMPAIGN MARKET HEADER POINTER          
         BNE   VALKX                                                            
         CLC   BWHKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                  
         BE    *+12                                                             
         MVI   APINDS,APIOKADD                                                  
         B     *+12                                                             
         BRAS  RE,IOCHECK                                                       
         BNE   VALKX                                                            
         MVC   LHDRKEY,IOKEY       SAVE THE HEADER KEY                          
         MVC   FVMSGNO,=AL2(FVFOK) SET OK RETURN CODE                           
***      MVI   LINDS,0                                                          
         NI    LINDS,LCOS2         TAKE OFF ALL FLAGS BESIDES THIS ONE          
         CLI   APACTN,ACTADD       FOR ACTION NOT ADD -                         
         BE    VALK8                                                            
         TM    APINDS,APIOKADD     TEST FOR HEADER NOT FOUND                    
         BO    VALK30                                                           
         TM    APINDS,APIOKRES     TEST FOR HEADER DELETED                      
         BZ    VALK11                                                           
         NI    APINDS,FF-APIOKDIS-APIOKCHA-APIOKDEL YES                         
         B     VALKX                                                            
*                                  ACTION ADD -                                 
VALK8    TM    APINDS,APIOKADD     TEST FOR HEADER NOT FOUND                    
         BZ    VALK10                                                           
         OI    LINDS,LNOHDR        INDICATE NO HEADER                           
         MVC   LHDRKEY,IOKEYSAV    SAVE ORIGINAL KEY                            
         XC    IOKEY,IOKEY         READ PASSIVE POINTERS TO GET NEXT            
         MVI   BWHPTYP,BWHPTYPQ    CAMPAIGN/MARKET SEQ NO                       
         MVI   BWHPSUB,BWHPSUBQ                                                 
         MVC   BWHPAGMD,BAGYMD                                                  
         OC    BWHPAGMD,BBYRMASK                                                
         MVC   BWHPBYR,BBYR                                                     
****     GOTO1 AIO,DIRHID          NEED READ FOR UPDATES!!    MHC               
****  11/04/03         SO THAT 2 PEOPLE CAN'T HAVE SAME SEQUENCE NUM            
**       GOTO1 AIO,DIRHIU+IORDEL   READ FOR UPDATES AND DELETES                 
**   05/17/05  THAT'S NOT RIGHT YOU IDIOT                                       
         GOTO1 AIO,DIRHIU          READ FOR UPDATES AND DELETES                 
***      BNE   VALKX                                                            
         BE    VALK8G                                                           
         TM    IOERR,IOEDEL        RECORD IS DELETED?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OI    APINDS,APIOKRES     IT'S OK TO RESTORE IT                        
         MVC   FVMSGNO,=AL2(FVFOK) SET OK RETURN CODE                           
*                                                                               
VALK8G   MVC   APHALF,=X'FFFF'                                                  
         CLC   IOKEY(BWHPSEQ-BWHPKEY),IOKEYSAV  TEST FIRST CAMPAIGN             
         BNE   VALK9                            FOR BUYER                       
         SR    RE,RE               NO-NEXT SEQ NO                               
         ICM   RE,3,BWHPSEQ                                                     
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    ECMSEQ                                                           
         STH   RE,APHALF                                                        
*                                                                               
VALK9    MVC   IOKEY,LHDRKEY       SET NEW SEQ NO IN HEADER KEY                 
         MVC   BWHKSEQ,APHALF                                                   
         MVC   LHDRKEY,IOKEY                                                    
         B     VALK15                                                           
*                                                                               
VALK10   TM    APINDS,APIOKCHA     HDR EXISTS - MUST BE ABLE TO CHANGE          
         BZ    VALKX                                                            
         TM    APINDS,APIOKRES                  MUST NOT BE DELETED             
         BO    VALKX                                                            
*                                                                               
VALK11   MVC   LHDRDA,IODA         SAVE THE HEADER DISK ADDRESS                 
*                                                                               
         LA    R1,FILGETU1         GET THE HEADER RECORD                        
******** LA    R1,FILGET1          (ALWAYS GET HEADER RECORD FOR UPDATE         
******** CLI   APACTN,ACTADD       TO AVOID DEADLY EMBRACES IN LIST             
******** BNE   *+8                 SCREENS)                                     
******** LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   APRECNUM,RECPKG     TEST PACKAGE/ORBIT RECORD MAINT              
         BE    *+12                                                             
         CLI   APRECNUM,RECORB                                                  
         BNE   VALK12                                                           
         MVC   IOKEY,APRECKEY      YES - GET THE PACKAGE/ORBIT RECORD           
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALK30                                                           
*                                                                               
VALK12   L     R2,AIOAREA1         DOES THIS STATION ALREADY EXIST?             
         LA    R4,BWHFSTEL                                                      
         SR    R0,R0                                                            
         SR    R8,R8                                                            
         SR    RF,RF                                                            
         NI    LFLAG1,X'FF'-LF1BUYRV                                            
*                                                                               
VALK13   CLI   0(R4),0                                                          
         BE    VALK13M                                                          
         CLI   0(R4),INFELCDQ                                                   
         BNE   VALK13B                                                          
         USING INFELCD,R4                                                       
         TM    INFFLAG1,IFF1BYRV                                                
         BZ    VALK13M                                                          
         OI    LFLAG1,LF1BUYRV                                                  
         CLI   APACTN,ACTADD       ARE WE TRYING TO ADD WORK RECORDS?           
         BNE   VALK13A                                                          
         MVC   FVMSGNO,=AL2(FVIBUYRV)                                           
         LA    R1,BWSRECH          POSITION CSR SOMEWHERE SENSIBLE              
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
*                                                                               
VALK13A  BRAS  RE,PROTIFLD         PROTECT UNPROTECT INPUT FIELDS               
         B     VALK13M                                                          
*                                                                               
VALK13B  IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALK13                                                           
*                                                                               
VALK13M  LA    R4,BWHFSTEL                                                      
VALK13O  CLI   0(R4),0                                                          
         BE    VALK14                                                           
         CLI   0(R4),BWHELCDQ                                                   
         BNE   VALK13R                                                          
         USING BWHEL,R4                                                         
         IC    R8,BWHSEQ                                                        
         CR    R8,RF                                                            
         BNH   *+6                                                              
         LR    RF,R8                                                            
         CLC   BWHSTA,QSTA                                                      
         BE    VALK15                                                           
*                                                                               
VALK13R  IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALK13O                                                          
*                                                                               
VALK14   OI    LINDS,LNEWSTA       NEW STATION                                  
         XC    APELEM,APELEM                                                    
         LA    R4,APELEM           ADD NEW STATION ELEMENT TO HEADER            
         MVI   BWHELCD,BWHELCDQ                                                 
         MVI   BWHELLN,BWHELLNQ                                                 
         LA    R8,1(RF)            R8=STATION CODE                              
         CHI   R8,256                                                           
         BL    VALK14E                                                          
****     DC    H'0'                DEEP TROUBLE                                 
***  ERROR OUT HERE                                                             
         MVC   FVMSGNO,=AL2(1321)   MORE THAN 255 UNIQUE STATIONS               
         LA    R1,DETNUM                                                        
         ST    R1,FVADDR                                                        
         CR    RE,RA               SET != CONDITION                             
         B     VALKX               GET OUT WITH ERROR                           
*                                                                               
VALK14E  STC   R8,BWHSEQ                                                        
         MVC   BWHSTA,QSTA                                                      
         GOTO1 AADDELS,BWHRECD                                                  
*                                                                               
VALK15   TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    VALK16                                                           
         TM    LINDS,LNOHDR+LNEWSTA  YES-CHECK HDR AND STATION EXIST            
         BNZ   VALK99                                                           
*                                                                               
         CLC   APRECDA,BWHSTA      STATION LETTERS THE SAME (4 BYTES)?          
         BE    VALK15A                                                          
* FIX 16APR02 (PWES) - IF SEL FROM 'DUP', APRECDA WILL NOT BE SET &             
* BWHSTA IS INFACT THE STATION WE JUST PUT IN THE FIELD BY DISPKEY!             
* SO NOT AN ERROR                                                               
         CLI   TWALACT,ACTDUP                                                   
         BNE   VALK98              NOT FROM 'DUP' - ERROR                       
         OC    APRECDA,APRECDA                                                  
         BNZ   VALK98                                                           
         MVC   APRECDA,BWHSTA      SET IT NOW                                   
*                                                                               
VALK15A  L     RF,ALSM             SET THE KEY FROM SAVED STAORAGE              
         LH    R1,LSMRDSP-LSMD(RF)                                              
         AR    R1,RF                                                            
         MVC   APRECKEY,LSMRKEY-LSMRTAB(R1)                                     
         CLM   R8,1,BWDKELST       TEST STATION CODE HAS CHANGED                
         BE    VALK17                                                           
         B     VALK98              YES-ERROR                                    
*                                                                               
VALK16   XC    APRECKEY,APRECKEY                                                
         MVI   BWDKTYP,BWDKTYPQ    SET UP FIRST PART OF DETAIL KEY              
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ     CAMPAIGN/MARKET SEQ NO                       
         MVI   BWDKELCD,1                                                       
         MVI   BWDKELST,1          STATION CODE                                 
         TM    LINDS,LNOHDR                                                     
         BO    *+8                                                              
         STC   R8,BWDKELST                                                      
         MVI   BWDKELPO,0                                                       
         MVC   BWDKELDY,BDAYS                                                   
         MVC   BWDKELTM,PTIMES                                                  
*                                                                               
         TM    LINDS,LNOHDR        TEST HEADER EXISTS                           
         BZ    VALK17                                                           
         MVI   LSEQNUM,1           NO-SET DETAIL SEQ NUM TO 1                   
         MVI   BWDKELSQ,1                                                       
         B     VALK30              AND EXIT NOW                                 
*                                                                               
VALK17   MVC   IOKEY,APRECKEY                                                   
         MVI   LSEQNUM,0                                                        
         LA    R1,MINHI2                                                        
         B     VALK18+4                                                         
*                                                                               
VALK18   LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    VALK19                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    TWAMODE,TWAMLSM     EOF-TEST LIST/SELECT MODE                    
         BO    VALK99              YES-SET RECORD NOT FOUND                     
         B     VALK22                                                           
*                                                                               
VALK19   L     R3,AIOAREA2                                                      
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    VALK20                                                           
         CLC   IOKEY(13),IOKEYSAV  YES-KEY MUST MATCH                           
         BNE   VALK99                                                           
         MVC   LSEQNUM,BWDKELSQ                                                 
         B     VALK30                                                           
*                                                                               
VALK20   CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV  NO-MATCH UP TO TIMES            
         BNE   VALK22                                                           
         CLI   APACTN,ACTADD       TEST ACTION=ADD                              
         BE    VALK21              YES-ONLY NEED TO GET HIGHEST SEQ NO          
         CLC   BWDTIMES,BTIMES     NO-MATCH ACTUAL TIMES                        
         BNE   VALK18                                                           
         CLC   BWDDPT,BDPT         MATCH DAYPART                                
         BNE   VALK18                                                           
         CLC   BWDSLN,BSLN         MATCH SPOT LENGTH                            
         BNE   VALK18                                                           
         CLI   LSEQNUM,0           TEST ALREADY HAVE A MATCH                    
         BNE   EDUPREC             YES-USER NEEDS THE 'DUP' SCREEN              
*                                                                               
VALK21   MVC   LSEQNUM,BWDKELSQ    SAVE THE SEQUENCE NUMBER                     
         B     VALK18                                                           
*                                                                               
VALK22   CLI   APACTN,ACTADD       TEST ACTION=ADD                              
         BNE   VALK24                                                           
         ZIC   RE,LSEQNUM                                                       
         LA    RE,1(RE)                                                         
         CHI   RE,255                                                           
         BH    ETMD                                                             
         STC   RE,LSEQNUM                                                       
         LA    R3,APRECKEY                                                      
         MVC   BWDKELSQ,LSEQNUM                                                 
         B     VALK26                                                           
*                                                                               
VALK24   CLI   LSEQNUM,0           TEST RECORD FOUND                            
         BE    VALK26              NO                                           
         TM    LINDS,LNEWSTA       YES-MAKE SURE STATION IS IN HDR REC          
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R3,APRECKEY                                                      
         MVC   BWDKELSQ,LSEQNUM    MOVE SEQ NUM TO KEY                          
         MVC   IOKEY,APRECKEY      AND READ THE RECORD                          
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALK30                                                           
*                                                                               
*ALK26   OI    APINDS,APIOKADD     OK TO ADD                                    
VALK26   NI    APINDS,FF-APIOKDIS-APIOKCHA-APIOKDEL-APIOKRES                    
*                                                                               
VALK30   CLI   APACTN,ACTADD       IS IT ADD?                                   
         BNE   *+8                                                              
         OI    APINDS,APIOKADD     OK TO ADD                                    
*                                                                               
         TM    APINDS,APIOKADD     TEST RECORD NOT FOUND                        
         BZ    VALKX                                                            
         TM    TWAMODE,TWAMLSM     AND IN LIST/SELECT MODE                      
         BNO   VALKX                - NOPE, IT'S DONE WITH                      
*   SETTING THIS TO ALSO STOP THE DUMPS IN GEGEN00:LFM4                         
         OI    APINDS,APIOKDIS                                                  
*   SETTING THIS TO ALSO STOP THE DUMPS IN GEGEN00:LFM4                         
         BRAS  RE,DISPRECD         WE NEED TO DISPLAY IT FIRST                  
         BRAS  RE,SETLNTH          SET LENGTHS FOR FIELDS ON SCREEN             
         XC    LDEMVALS(LDEMOLD-LDEMVALS),LDEMVALS                              
****     NI    BWSACTH+4,X'FF'-X'20'                                            
         B     VALKX                                                            
*                                                                               
VALK98   MVC   FVMSGNO,=AL2(FVFSKHC)    SELECTED RECORD KEY HAS CHANGED         
         LA    R1,DETSTAH          IN PARTICULAR, STATION                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
*                                                                               
VALK99   MVC   FVMSGNO,=AL2(FVFERNF)    RECORD NOT FOUND                        
*                                                                               
VALKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALREC   OC    BYRPW,BYRPW         TEST BUYER PASSWORD REQUIRED                 
         BZ    VALR1                                                            
         GOTO1 AVALPWD             YES-VALIDATE IT                              
         BNE   VALRX                                                            
*                                                                               
VALR1    XC    LSPWEL,LSPWEL       BUILD DETAIL RECORD                          
         XCEFL LBTREL,300                                                       
         XCEFL LBTREL2,300                                                      
         XCEFL LBTREL3,300                                                      
         XC    LSHPEL,LSHPEL                                                    
         XC    LDTREL,LDTREL                                                    
         XC    LDMOEL,LDMOEL                                                    
         XC    LBWDELSV,LBWDELSV                                                
         L     R3,AIOAREA2                                                      
         CLI   APACTN,ACTADD       FOR ACTION NOT ADD,                          
         BE    VALR8                                                            
*                                                                               
         CLI   BWDELLN,BWDELLNQ                                                 
         BH    VALR1A                                                           
         MVC   LBWDELSV(BWDELLNQ),BWDEL                                         
         MVI   LBWDELSV+1,BWDELL2Q                                              
* SO WE DON'T TRASH THE ELEMENTS THAT FOLLOW BWDEL                              
         XC    APELEM,APELEM                                                    
         MVI   APELEM,BWDELCDQ     X'01' - DESCRIPTION ELEM                     
         GOTO1 ADELELS,BWDRECD                                                  
*                                                                               
         MVC   APELEM(BWDELL2Q),LBWDELSV                                        
         GOTO1 AADDELS,BWDRECD                                                  
         B     VALR1B                                                           
*                                                                               
VALR1A   MVC   LBWDELSV,BWDEL      SAVE OLD DESCRIPTION ELEMENT                 
VALR1B   LA    R2,LBTREL                                                        
         LA    R8,LBTREL2                                                       
         LA    R9,LBTREL3                                                       
         LA    R5,LDTREL                                                        
         LA    R1,BWDEL            LOOK FOR SPOTS PER WEEK ELEMENT,             
         SR    RE,RE                    BUY TRANSFER ELEMENT, AND               
*                                       DEMO ELEMENT                            
VALR2    CLI   0(R1),0                                                          
         BE    VALR8                                                            
         LA    R4,LSHPEL           ADDED 01/09/03                               
         CLI   0(R1),SHPELCDQ         FOR PUT OVERRIDE ELEMENT                  
         BE    VALR4                                                            
         LA    R4,LSPWEL                                                        
         CLI   0(R1),SPWELCDQ                                                   
         BE    VALR4                                                            
         LA    R4,LDMOEL                                                        
         CLI   0(R1),DMOELCDQ                                                   
         BE    VALR4                                                            
         LR    R4,R5                                                            
         LA    RF,LDTREL+L'LDTREL                                               
         CLI   0(R1),DTRELCDQ                                                   
         BE    VALR4                                                            
         LR    R4,R2                                                            
         LA    RF,LBTREL+L'LBTREL                                               
         CLI   0(R1),BTRELCDQ                                                   
         BNE   VALR6                                                            
         TM    BTRIND-BTREL(R1),BTRIEC2+BTRIEC3                                 
         BZ    VALR4                                                            
         LR    R4,R8                                                            
         LA    RF,LBTREL2+L'LBTREL2                                             
         TM    BTRIND-BTREL(R1),BTRIEC2                                         
         BO    VALR4                                                            
         LR    R4,R9                                                            
         LA    RF,LBTREL3+L'LBTREL3                                             
*                                                                               
VALR4    IC    RE,1(R1)            FOUND - SAVE THE ELEMENT                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R1)       LSPWEL CONTAINS SAVED SPOTS/WEEK ELE         
*                                  LBTREL CONTAINS SAVED BUY XFR ELEM           
*                                  LDTREL CONTAINS SAVED DAILY XFR ELEM         
*                                  LDMOEL CONTAINS SAVED DEMO ELEMENT           
*             MHC     01/09/03     LSHPEL CONTAINS SAVED PUT OVERRIDE           
         CLI   0(R1),BTRELCDQ                                                   
         BE    *+12                                                             
         CLI   0(R1),DTRELCDQ                                                   
         BNE   VALR6                                                            
         LA    R4,1(RE,R4)                                                      
         CR    R4,RF                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R4                                                            
         CLI   0(R1),DTRELCDQ                                                   
         BE    VALR6                                                            
         TM    BTRIND-BTREL(R1),BTRIEC2+BTRIEC3                                 
         BNZ   *+10                                                             
         LR    R2,R4                                                            
         B     VALR6                                                            
         TM    BTRIND-BTREL(R1),BTRIEC2                                         
         BZ    *+10                                                             
         LR    R8,R4                                                            
         B     VALR6                                                            
         LR    R9,R4                                                            
*                                                                               
VALR6    IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VALR2                                                            
*                                                                               
VALR8    L     R5,ATWA                                                          
         XC    BWDRECD(256),BWDRECD                                             
         MVC   BWDKEY,APRECKEY                                                  
         MVI   BWDKELSQ,0                                                       
         LA    R0,BWDEL+BWDELL2Q+1-BWDRECD                                      
         STCM  R0,3,BWDLEN                                                      
         MVC   BWDEL(BWDELL2Q),LBWDELSV   SAVED DESCRIPTION ELEM IF ANY         
         MVI   BWDELCD,BWDELCDQ                                                 
         MVI   BWDELLN,BWDELL2Q                                                 
         MVC   BWDSTACD,BWDKELST                                                
         MVC   BWDPKOR,BWDKELPO                                                 
         MVC   BWDDAYS,BWDKELDY                                                 
         MVC   BWDTIMCD,BWDKELTM                                                
         MVC   BWDSTA,QSTA                                                      
         MVC   BWDTIMES,BTIMES                                                  
         MVC   BWDTIMCD,PTIMES                                                  
         MVC   BWDDPT,BDPT                                                      
         MVC   BWDSLN,BSLN                                                      
*                                                                               
         MVC   BWDKELTM,PTIMES                                                  
         MVC   IOKEY(13),BWDKEY                                                 
         LA    R8,1                                                             
         LA    R1,MINHI3           READ RECORDS FOR NEW DAYS/TIMES              
         B     VALR9B                                                           
                                                                                
VALR9A   LA    R1,MINSEQ3                                                       
VALR9B   GOTO1 AMIN                                                             
         BNE   VALR9C                                                           
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   VALR9C                                                           
         IC    R8,IOKEY+BWDKELSQ-BWDKEY     SAVE LATEST SEQ NUM                 
         LA    R8,1(R8)            NEXT SEQUENCE NUMBER                         
         B     VALR9A                                                           
**********  THESE LINES DELETED  AS IT WOULD DIE ON DUPLICATE ON ADD            
*&&DO                                                                           
VALR9C   CLI   APACTN,ACTADD                                                    
         BNE   VALR9K                                                           
*&&                                                                             
**********  THESE LINES DELETED  AS IT WOULD DIE ON DUPLICATE ON ADD            
*                                                                               
VALR9C   STC   R8,BWDSEQ           ONLY FOR ADD SHOULD THIS CHANGE              
         STC   R8,BWDKELSQ                                                      
         MVC   IOKEY(13),APRECKEY  READ RECORD WITH OLD KEY                     
         CLI   APACTN,ACTADD                                                    
         BE    VALR9M                                                           
         GOTO1 AMIN,MINRD3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALR9M   MVI   BWDSUBDP,0          SUB-DAYPART                                  
         MVI   FVMAXL,1                                                         
         GOTO1 AFVAL,DETSDPH                                                    
         BH    VALRX                                                            
         BL    VALR12                                                           
         CLI   CMPDPOPT,C'S'       TEST SCHEDULE SUBDPTS SEPERATELY             
         BE    EIIF                YES-INVALID INPUT FIELD                      
         LA    RE,L'DPTSUBS                                                     
         LA    RF,DPTSUBS                                                       
*                                                                               
VALR10   CLC   FVIFLD(1),0(RF)                                                  
         BNE   *+14                                                             
         MVC   BWDSUBDP,0(RF)                                                   
         B     VALR12                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,VALR10                                                        
         MVC   FVMSGNO,=AL2(FVIDPT)                                             
         B     VALRX                                                            
*                                                                               
VALR12   MVI   APFLAG,0                                                         
         XC    APDUB,APDUB                                                      
         LA    R1,DETCS1H          COST                                         
*                                                                               
         TM    LFLAG1,LF1BUYRV     UNDER BUY REVISIONS?                         
         BNZ   VALRX                                                            
*                                                                               
         CLI   FVILEN-FVIHDR(R1),0                                              
         BE    ENOC                                                             
         LA    R8,BWDCOST1                                                      
         BAS   RE,VALRCST                                                       
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBALIGN,C'L'                                                     
         BAS   R9,DISRCST                                                       
         LA    R1,DETEF2H          EFFECTIVE DATE 2                             
         LA    R8,BWDEFDT2                                                      
         BAS   RE,VALRDAT                                                       
         OC    BWDEFDT2,BWDEFDT2                                                
         BZ    VALR14                                                           
         CLC   BWDEFDT2,CMPST                                                   
         BNH   EDLO                                                             
         CLC   BWDEFDT2,CMPND                                                   
         BH    EDHI                                                             
         GOTO1 AVALRDAY                                                         
         BNE   EDIV                                                             
         CLC   CMPSTMON,APWORK     TEST EFFECTIVE DATE IN SAME WEEK             
         BE    EDIV                AS START DATE                                
         MVC   APWORK+12(6),APWORK                                              
*                                                                               
VALR14   LA    R1,DETCS2H          COST 2                                       
         LA    R8,BWDCOST2                                                      
         BAS   RE,VALRCST                                                       
         OC    BWDCOST2,BWDCOST2                                                
         BZ    *+8                                                              
         BAS   R9,DISRCST                                                       
         LA    R1,DETEF3H          EFFECTIVE DATE 3                             
         LA    R8,BWDEFDT3                                                      
         BAS   RE,VALRDAT                                                       
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    VALR15                                                           
         CLC   BWDEFDT3,BWDEFDT2                                                
         BNH   EDLL                                                             
         CLC   BWDEFDT3,CMPND                                                   
         BH    EDHI                                                             
         GOTO1 AVALRDAY                                                         
         BNE   EDIV                                                             
         CLC   APWORK+12(6),APWORK TEST EFFECTIVE DATE 3 IN SAME WEEK           
         BE    EDIV                AS EFFECTIVE DATE 2                          
*                                                                               
VALR15   LA    R1,DETCS3H          COST 3                                       
         LA    R8,BWDCOST3                                                      
         BAS   RE,VALRCST                                                       
         OC    BWDCOST3,BWDCOST3                                                
         BZ    *+8                                                              
         BAS   R9,DISRCST                                                       
****  CODE FOR THE NEW REP FIELD ON THE SCREEN                                  
         OC    DETREPN,DETREPN     ANYTHING HERE? (DETREPNH+5 NO GOOD)          
***      BZ    VALR16G              - NOPE, NOTHING                             
         BNZ   VALR16E             NON-ZERO                                     
         XC    BWDREP,BWDREP       TAKE OUT THE REP                             
         XC    LESTREP,LESTREP     DELIBERATELY NO REP                          
         OI    LINDS,LCHG          CHANGED FIELD                                
         B     VALR16S                                                          
*                                                                               
VALR16E  BRAS  RE,REPCHK                                                        
         BNE   EBADREP                                                          
*                                                                               
***      MVC   BWDREP,DETREPN                                                   
         MVC   BWDREP,LREP         NOW A BINARY FIELD!!                         
         OI    LINDS,LCHG          CHANGED FIELD                                
         B     VALR16S                                                          
*                                                                               
VALR16G  OC    ESTREP,ESTREP       IS THERE AN ESTIMATE REP?                    
         BZ    VALR16S              - NOPE                                      
         MVC   BWDREP,ESTREP       BINARY                                       
         MVC   DETREPN,LESTREP     EBCDIC                                       
         MVI   DETREPNH+5,3                                                     
         OI    DETREPNH+6,X'80'                                                 
         OI    LINDS,LCHG          CHANGED FIELD                                
****                                 MHC  06/10/04                              
VALR16S  GOTO1 ACHKBPPR,DETPRGH                                                 
         BNE   VALRX                                                            
*                                                                               
VALR16X  GOTO1 AFVAL,DETPRGH       PROGRAMMING                                  
         BH    VALRX                                                            
         BL    VALR18                                                           
         CLI   FVIFLD,C'$'         TEST PROGRAM ADJACENCY CODE                  
         BNE   VALR17                                                           
         GOTO1 AVALADJ,FVIFLD+1    YES - VALIDATE IT                            
         BNE   VALRX                                                            
         MVC   BWDADJ,QADJCD       VALID - SET THE ADJACENCY CODE               
         B     VALR23                                                           
*                                                                               
VALR17   CLI   APACTN,ACTADD       PROGRAM NOT BLANK-                           
         BE    VALR20              IF ADD, THEN IT'S AN OVERRIDE                
         B     VALR19              ELSE, TEST FOR PROGRAM CHANGE                
*                                                                               
VALR18   CLI   APACTN,ACTADD       PROGRAM BLANK-                               
         BE    VALR22              IF ADD, LEAVE PROGRAM BLANK FOR NOW          
*                                                                               
VALR19   OC    BWDPROG,SPACES      PAD IT FIRST (MIGHT NOT BE PADDED)           
         CLC   BWDPROG,FVIFLD      TEST PROGRAM CHANGE                          
         BE    VALR22                                                           
VALR20   OI    BWDINDS,BWDIPRG     PROGRAMMING OVERRIDE                         
VALR22   MVC   BWDPROG,FVIFLD                                                   
*                                                                               
VALR23   TM    LINDS,LCOS2         WE NEED COS2?                                
         BZ    VALR24               - NOPE                                      
         LA    R1,DETC2NH                                                       
         LA    R8,APWORK           TEMP USAGE                                   
         BAS   RE,VALRCST                                                       
         OC    APWORK(4),APWORK    DID WE HAVE ANYTHING?                        
         BZ    VALR24                                                           
         BAS   R9,DISRCST                                                       
         XC    APELEM,APELEM       ADD COS2 ELEMENT                             
         LA    R4,APELEM                                                        
         USING CS2EL,R4                                                         
         MVI   CS2ELCD,CS2ELCDQ    X'13' ELEMENT                                
         MVI   CS2ELLN,CS2ELLNQ                                                 
         MVC   CS2COST2,APWORK     SAVE IT!                                     
         GOTO1 AADDELS,BWDRECD                                                  
         DROP  R4                                                               
*                                                                               
VALR24   GOTO1 AFVAL,DETIDH        VALIDATE ID                                  
         BNE   VALR25                                                           
         XC    APELEM,APELEM       ADD ID ELEMENT                               
         LA    R4,APELEM                                                        
         USING BWIDEL,R4                                                        
         MVI   BWIDELCD,BWIDECDQ                                                
         MVI   BWIDELLN,BWIDELNQ                                                
         MVC   BWID,FVIFLD                                                      
         GOTO1 AADDELS,BWDRECD                                                  
*                                                                               
VALR25   GOTO1 AFVAL,DETDATH       VALIDATE DATES                               
         BE    *+14                                                             
         XC    BWDDATES,BWDDATES                                                
         B     VALR25C                                                          
*                                                                               
         GOTO1 AVALDATE,DETDATH    VALIDATE DATES                               
         BNE   VALRX                                                            
         MVC   BWDDATES,APFULL     SET DATES                                    
         MVC   BWDWKS,APHALF       SET INACTIVE WEEKS                           
         MVC   BWDWKS2,APDUB       GET OTHER INACTIVE WEEKS                     
         CLI   APACTN,ACTADD       IS IT AN ADD?                                
         BE    VALR25C              - YUP, SKIP FOLLOWING SUBROUTINE            
         BRAS  RE,INAWKCHK         CHK EXISTING SPOTS FOR INACTIVE WKS          
         BNE   EINAWK               - INACTIVE WEEKS HAVE SPOTS, ERROR          
*******   ADDED INACTIVE WEEK MAKES FROM TAM                                    
VALR25C  TM    TWATAMFL,TWAFRTAM   CAME FROM TAM?                               
         BZ    VALR25E              - NOPE                                      
         TM    TWATAMFL,TWAWKMSK   ANY MASK?                                    
         BZ    VALR25E              - NOPE                                      
         GOTO1 VHEXIN,APPARM,DETMSK,APWORK,14                                   
         MVC   BWDWKS,APWORK                                                    
         MVC   BWDWKS2,APWORK+2                                                 
*******   ADDED INACTIVE WEEK MAKES FROM TAM                                    
*                                                                               
VALR25E  CLI   LSPWEL,0            TEST FOR SPOTS PER WEEK ELEMENT              
         BE    VALR25G                                                          
         MVC   APELEM(L'LSPWEL),LSPWEL                                          
         GOTO1 AADDELS,BWDRECD     YES - ADD IT                                 
*                                                                               
VALR25G  CLI   LSHPEL,0            TEST FOR PUT OVERRIDE ELEMENT                
         BE    VALR26                                                           
         MVC   APELEM(L'LSHPEL),LSHPEL                                          
         GOTO1 AADDELS,BWDRECD     YES - ADD IT                                 
*                                                                               
VALR26   LA    R4,LBTREL           ADD BACK IN THE BUY TRANFER ELES             
         BAS   RE,VALR28                                                        
         LA    R4,LBTREL2                                                       
         BAS   RE,VALR28                                                        
         LA    R4,LBTREL3                                                       
         BAS   RE,VALR28                                                        
         LA    R4,LDTREL                                                        
         BAS   RE,VALR28                                                        
         B     VALR29                                                           
*                                                                               
VALR28   LR    R0,RE                                                            
VALR28A  CLI   0(R4),0                                                          
         BE    VALR28X                                                          
         XC    APELEM,APELEM                                                    
         ZIC   R8,1(R4)                                                         
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R4)                                                  
         GOTO1 AADDELS,BWDRECD                                                  
         LA    R4,1(R8,R4)                                                      
         B     VALR28A                                                          
VALR28X  LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
VALR29   LA    R4,APELEM                                                        
         XC    LUPFIL,LUPFIL       CLEAR UPGRADE VALUES                         
         XC    LUPGRD,LUPGRD                                                    
         XC    LUPFRBK,LUPFRBK                                                  
         MVI   LUPFRBKT,0          LUPFRBK BOOKTYPE                             
         XC    LUPFRBKL,LUPFRBKL                                                
         XC    LUPPUT,LUPPUT                                                    
         XC    LUPSHR,LUPSHR                                                    
         XC    LOVDAY,LOVDAY                                                    
         XC    LOVTIME,LOVTIME                                                  
*                                                                               
         CLI   DETUPGH+FVILEN-FVIHDR,0   IF NO UPGRADE INPUT                    
         BNE   VALR30                    AND THERE IS A DEFAULT                 
         MVI   QBOOKTYP,0                                                       
         OC    CMPUP,CMPUP               CAMPAIGN UPGRADE, THEN SAVE            
         BZ    VALR31                    THE DEFAULT VALUES                     
*                                                                               
         MVC   LUPFIL,CMPUF                                                     
         MVC   LUPGRD,CMPUP                                                     
         MVC   LUPFRBK,CMPFB                                                    
         TM    CMPFB+1,BTY2CHAR                                                 
         BNO   VALR29R                                                          
         CLI   CMPFBTP,0           ANYTHING HERE?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LUPFRBKT,CMPFBTP                                                 
VALR29R  MVC   LUPFRBKL,CMPFBLST                                                
         MVC   LUPPUT,CMPUPUT                                                   
         MVC   LUPSHR,CMPUSHR                                                   
         MVC   QBOOKTYP,CMPBKTYP                                                
         TM    LUPFRBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    VALR31                                                           
         TM    LUPFRBK+1,BTY2CHAR  2 CHARACTER BOOKTYPES?                       
         BNO   VALR29T              - NOPE                                      
         CLI   LUPFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   QBOOKTYP,LUPFRBKT    - YUP                                       
         B     VALR31                                                           
*                                                                               
VALR29T  GOTO1 AGETBKTY,APPARM,(C'B',LUPFRBK+1),QBOOKTYP                        
         B     VALR31                                                           
*                                                                               
VALR30   MVI   APFLAG,X'F8'        UPGRADE EXPRESSION                           
         GOTO1 AVALUPG,DETUPGH                                                  
         BNE   VALRX                                                            
         MVC   BWDUPUT,APWORK+16   PUT/SHR AVERAGING                            
         MVC   BWDUSHR,APWORK+17                                                
         XC    APELEM,APELEM       BUILD UPGRADE ELEMENT                        
         USING UPGEL,R4                                                         
         MVI   UPGELCD,UPGELCDQ                                                 
         MVI   UPGELLN,UPGELLNQ                                                 
         MVC   UPGFILE,APWORK                                                   
         MVC   UPGRADE,APWORK+1                                                 
         MVC   UPGFRBK,APWORK+9                                                 
         TM    APWORK+9+1,BTY2CHAR   2 CHARACTER BOKOTYPE?                      
         BNO   VALR30E                                                          
         CLI   QBOOKTYP,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   UPGFRBKT,QBOOKTYP                                                
         MVI   UPGELLN,UPGELLQ2    EXTENDED LENGTH                              
VALR30E  MVC   UPGFRBKL,APWORK+18                                               
         MVC   UPGINPUT,SPACES                                                  
         ZIC   RE,DETUPGH+FVILEN-FVIHDR                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   UPGINPUT(0),DETUPG                                               
         GOTO1 AADDELS,BWDRECD                                                  
*                                                                               
         MVC   LUPFIL,UPGFILE      SAVE UPGRADE VALUES                          
         MVC   LUPGRD,UPGRADE                                                   
         MVC   LUPFRBK,UPGFRBK                                                  
         TM    UPGFRBK+1,BTY2CHAR   2 CHARACTER BOOKTYPE?                       
         BNO   VALR30G                                                          
         CLI   UPGELLN,UPGELLNQ    EXTENDED LENGTH?                             
         BH    *+6                  - YUP                                       
         DC    H'0'                                                             
         MVC   LUPFRBKT,UPGFRBKT                                                
VALR30G  MVC   LUPFRBKL,UPGFRBKL                                                
         MVC   LUPPUT,BWDUPUT                                                   
         MVC   LUPSHR,BWDUSHR                                                   
*                                                                               
         OC    APWORK+11(5),APWORK+11                                           
         BZ    VALR31                                                           
         XC    APELEM,APELEM       BUILD OVERRIDE DAY/TIME/STATION ELE          
         USING ODTEL,R4                                                         
         MVI   ODTELCD,ODTELCDQ                                                 
         MVI   ODTELLN,ODTELLNQ                                                 
         MVC   ODTDAY,APWORK+11                                                 
         MVC   ODTTIME,APWORK+12                                                
         GOTO1 AADDELS,BWDRECD                                                  
*                                                                               
         MVC   LOVDAY,ODTDAY       SAVE OVERRIDE DAY/TIME                       
         MVC   LOVTIME,ODTTIME                                                  
*                                                                               
VALR31   MVC   BWDUCODE,SPACES     USER CODE                                    
         GOTO1 AFVAL,DETCODH                                                    
         BH    VALRX                                                            
         BL    *+16                                                             
         MVC   BWDUCODE,DETCOD                                                  
         OC    BWDUCODE,SPACES                                                  
*                                                                               
         CLI   APACTN,ACTADD       ADDING WITH                                  
         BNE   VALR31Y                                                          
         CLC   =C'SKED=',DETCM1       SCHEDULE OPTION                           
         BNE   VALR31Y                                                          
         GOTO1 AFVAL,DETCM1H                                                    
*                                                                               
         L     RF,AEXTRAWK                                                      
         AHI   RF,LIUN-EXTRAWKD                                                 
         ST    RF,APPARM+4                                                      
         MVI   APPARM+4,15                                                      
*                                                                               
         GOTO1 VSCANNER,APPARM,(X'80',DETCM1H),,0                               
         CLI   APPARM+4,0                                                       
         BE    EIIF                                                             
*                                                                               
         XC    APELEM,APELEM                                                    
         USING SPWEL,R4                                                         
         MVI   SPWELCD,SPWELCDQ                                                 
*                                                                               
         L     R1,AEXTRAWK         R1 = 1ST SCAN ENTRY                          
         AHI   R1,LIUN-EXTRAWKD                                                 
         ZIC   RF,APPARM+4         RF = # OF SCAN RECORDS                       
         LA    RE,SPWPERWK         RE = 1ST SPOT WEEK                           
*                                                                               
         CLI   1(R1),0             NOTHING FOR THE SKED=                        
         BE    EIIF                                                             
         TM    3(R1),X'80'         VALID NUMERIC?                               
         BZ    EIIF                                                             
         OC    9(2,R1),9(R1)       MAKE SURE NOT MORE THAN 255                  
         BNZ   EIIF                                                             
         MVC   0(1,RE),11(R1)      COPY THE NUMBER OF SPOTS                     
         B     VALR31B                                                          
*                                                                               
VALR31A  CLI   0(R1),0             THIS WEEK HAS NO SPOTS?                      
         BE    VALR31B                                                          
         TM    2(R1),X'80'         VALID NUMERIC?                               
         BZ    EIIF                                                             
         OC    5(2,R1),5(R1)       MAKE SURE NOT MORE THAN 255                  
         BNZ   EIIF                                                             
         MVC   0(1,RE),7(R1)       COPY THE NUMBER OF SPOTS                     
*                                                                               
VALR31B  LA    R1,32(R1)                                                        
         LA    RE,1(RE)                                                         
         BCT   RF,VALR31A                                                       
*                                                                               
         CLI   0(RE),0             FIND LAST WEEK WITH SPOTS                    
         BNE   *+8                 SO WE CAN CALCULATE L(ELEMENT)               
         BCT   RE,*-8                                                           
         LR    R0,RE                                                            
         SR    R0,R4                                                            
         CH    R0,=Y(SPWPERWK-SPWEL)                                            
         BNH   VALR31X                                                          
         AHI   R0,1                                                             
         STC   R0,SPWELLN                                                       
*                                                                               
         GOTO1 AADDELS,BWDRECD     ADD THE ELEMENT TO RECORD                    
*                                                                               
VALR31X  LA    R0,4                                                             
         LA    R9,DETCM2H                                                       
         B     VALR31Z                                                          
*                                                                               
VALR31Y  LA    R0,5                MAX 5                                        
         LA    R9,DETCM1H          COMMENT ELEMENTS                             
VALR31Z  SR    R8,R8                                                            
*                                                                               
VALR32   LR    R1,R9                                                            
         GOTO1 AFVAL                                                            
         BH    VALRX                                                            
         BL    VALR33                                                           
         XC    APELEM,APELEM                                                    
         USING COMEL,R4                                                         
         MVI   COMELCD,COMELCDQ                                                 
         LNR   RF,R0                                                            
         LA    RF,6(RF)                                                         
         STC   RF,COMNUM           COMMENT NUMBER                               
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   COMCOM(0),FVIFLD                                                 
         LA    RE,COMCOM-COMEL+1(RE)                                            
         STC   RE,COMELLN                                                       
         GOTO1 AADDELS,BWDRECD                                                  
*                                                                               
VALR33   LA    R9,DETCM2-DETCM1(R9)    NEXT COMMENT                             
         BCT   R0,VALR32                                                        
*                                                                               
VALR34   XC    APELEM,APELEM       DEMOS                                        
         USING DMOEL,R4                                                         
         MVI   DMOELCD,DMOELCDQ                                                 
         LA    R0,LNDEMOS                                                       
         LA    R2,ESTDEMS                                                       
         LA    R6,LDEMOVR                                                       
         LA    R8,DETDM1H                                                       
         LA    R9,DMODEMO                                                       
         MVI   0(R6),0                                                          
*                                                                               
VALR36   MVC   0(3,R9),0(R2)       DEMO CODE                                    
         LR    R1,R8                                                            
         GOTO1 AFVAL               VALIDATE DEMO VALUE FIELD                    
         BL    VALR42              MISSING - NOT OVERRIDE                       
         BH    VALRX                                                            
         XC    LTRT,LTRT           LOOK FOR * IN FIELD                          
         LA    R1,C'*'                                                          
         LA    R1,LTRT(R1)                                                      
         MVI   0(R1),C' '                                                       
         ST    R2,APFULL                                                        
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         TRT   FVIFLD(0),LTRT                                                   
         L     R2,APFULL                                                        
         BZ    VALR38               NO *                                        
         MVI   0(R1),C' '           YES - REPLACE WITH SPACE                    
         B     VALR40                                                           
*                                                                               
VALR38   TM    FVIIND,FVIVAL       IF NOT PREV VALIDATED, MUST BE               
         BZ    VALR40              AN OVERRIDE                                  
         CLI   APACTN,ACTADD       TEST ACTION IS ADD                           
         BNE   VALR42              NO - NOT OVERRIDE                            
*                                                                               
VALR40   ZIC   RF,FVILEN           DEMO IS AN OVERRIDE                          
         LA    RE,FVIFLD                                                        
         ST    RE,APPARM           A(INPUT FIELD)                               
         MVI   APPARM,1            DECIMAL POINTS                               
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   VALR41C                                                          
         CLI   1(R9),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R9),C'E'                                                       
         BNE   VALR41C             IT IS IMPRESSION                             
         MVI   APPARM,2            SHOULD BE 2 DECIMALS                         
***  2 DECIMAL  ***                                                             
VALR41C  ST    RF,APPARM+4         LENGTH                                       
         GOTO1 VCASHVAL,APPARM     VALIDATE DEMO VALUE                          
         CLI   APPARM,X'FF'                                                     
         BE    EIIF                                                             
         MVC   4(4,R9),APPARM+4    DEMO VALUE                                   
         OI    4(R9),DMODEMOV      INDICATE OVERRIDE                            
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   IS IT 2 DECIMALS?                            
         BNO   VALR41E              - NOPE, NO NEED FOR FLAG                    
         CLI   1(R9),C'R'          IS IT RATING?                                
         BE    *+12                 - YUP                                       
         CLI   1(R9),C'E'          IS IT E-RATING?                              
         BNE   VALR41E              - NOPE                                      
         OI    4(R9),DMODEM2D      INDICATE 2 DECIMAL PRECISION                 
***  2 DECIMAL  ***                                                             
VALR41E  MVI   0(R6),OVERELEM     BUILD SPDEMUP DEMO OVERRIDE LIST ELEM         
         MVI   1(R6),6                                                          
         MVC   2(2,R6),1(R2)                                                    
         MVC   4(2,R6),APPARM+6                                                 
         LA    R6,6(R6)                                                         
         MVI   0(R6),0                                                          
*                                                                               
VALR42   LA    R9,L'DMODEMO(R9)    NEXT DEMO                                    
         LA    R2,3(R2)                                                         
         OC    0(3,R2),0(R2)       TEST FOR ANY MORE DEMOS                      
         BZ    *+8                                                              
         BCT   R0,*+12                                                          
         MVI   0(R9),X'FF'         NO - INDICATE END IN THE DEMO ELEM           
         B     VALR44                                                           
         SR    R1,R1               FIND NEXT DEMO VALUE FIELD                   
         IC    R1,0(R8)                                                         
         AR    R8,R1                                                            
         TM    FVATRB-FVIHDR(R8),FVAPROT                                        
         BO    *-10                                                             
         B     VALR36                                                           
*                                                                               
VALR44   SR    R9,R4              DETERMINE DEMO ELEMENT LENGTH                 
         CHI   R9,2                                                             
         BNH   VALR80                                                           
         STC   R9,DMOELLN                                                       
         DROP  R4                                                               
*                                                                               
         OC    LUPGRD,LUPGRD       TEST UPGRADE EXPRESSION EXISTS               
         BNZ   VALR44A                                                          
         CLI   QMED,C'R'                                                        
         BNE   ENUE                                                             
         B     VALR64G                                                          
*                                                                               
VALR44A  LA    R6,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R6                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         L     RE,AEXTRAWK                                                      
         AHI   RE,LIUN-EXTRAWKD                                                 
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   QSTA,C'0'           IS IT A NUMBER?                              
         BL    VALR44E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         XC    SPUPSYSC,SPUPSYSC   CLEAR THIS SO 2ND+ INTERATION WORKS!         
         MVC   SPUPSTA(3),QSTA+5     MOVE THE NETWORK IN                        
         MVC   SPUPSYSE,QSTA                                                    
         B     VALR44G                                                          
*****  CABLE/FUSION DATE LOOKUP                                                 
VALR44E  MVC   SPUPSTA,QSTA                                                     
VALR44G  MVC   SPUPDAY,BDAYS                                                    
         MVC   SPUPTIM,BTIMES                                                   
         MVC   SPUPFIL,LUPFIL                                                   
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,LUPFRBK                                                  
         MVC   SPUPFBKL,LUPFRBKL                                                
         OC    SPUPFBK,SPUPFBK                                                  
         BNZ   *+16                                                             
         MVC   SPUPFBK,ESTBOOK                                                  
         XC    SPUPFBKL,SPUPFBKL                                                
         MVC   SPUPUDAY,LOVDAY                                                  
         MVC   SPUPUTIM,LOVTIME                                                 
         MVC   SPUPTYPE(L'LUPGRD),LUPGRD                                        
         MVC   SPUPBTYP,STABKTYP                                                
*                                                                               
         CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
         TM    SPUPFBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    VALR44T                                                          
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   VALR44Q              - NOPE                                      
         CLI   LUPFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,LUPFRBKT                                                
         B     VALR44R                                                          
*                                                                               
VALR44Q  GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
VALR44R  NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
VALR44T  CLI   CUDMED,C'C'         TEST CANADA                                  
         BNE   *+16                                                             
         TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
         BZ    *+8                                                              
         OI    SPUPOPTS,SPOANGFR   YES                                          
*                                                                               
         CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         CLI   LUPPUT,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   LUPPUT,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   LUPSHR,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   LUPSHR,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
*                                                                               
         NI    LINDS,FF-LAUTADJ-LALLADJ                                         
         TM    INFIND,INFINOAD     TEST OPTION TO NOT AUTO ADJUST DEMOS         
         BO    VALR60                                                           
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT  NO-TEST FOR AUTOADJ          
         BZ    VALR60                                                           
         OI    LINDS,LAUTADJ       YES                                          
         XC    LADJDEM,LADJDEM                                                  
         TM    CMPOPTS,CAMOAALL+CAMOATGT   TEST ALL OR TGT                      
         BZ    VALR45                                                           
         MVC   LADJDEM,=X'D901'    YES-SET ADJUSTMENT DEMO                      
         TM    CMPOPTS,CAMOAALL                                                 
         BO    VALR45                                                           
         MVC   LADJDEM,ESTDEMS+1                                                
*                                                                               
VALR45   LA    R9,LIMPVALS                                                      
         MVI   0(R9),FF            INIT IMPRESSION ADJUSTS                      
         MVI   LALLVALS,FF         INIT ALL/TGT ADJUSTS                         
         MVI   LDEMS,0                                                          
         MVI   LDEMS+3,FF                                                       
         LA    R2,LDEMOVR                                                       
*                                                                               
VALR46   CLI   0(R2),0             SCAN OVERRIDE ELEM FOR OVERRIDES             
         BE    VALR60                                                           
         OC    LADJDEM,LADJDEM     TEST FOR ADJUSTMENT DEMO                     
         BZ    VALR52                                                           
         CLC   LADJDEM,2(R2)       YES-IS THIS IT?                              
****     BNE   VALR52                                                           
         BNE   VALR58               - NOPE IT'S NOT                             
         OI    LINDS,LALLADJ       YES-INDICATE TO ADJUST ALL                   
         MVC   LDEMS+1(2),2(R2)    GET RATING                                   
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   VALR47                                                           
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
VALR47   GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS                          
         XC    LDEMNEW,LDEMNEW                                                  
         MVC   LDEMNEW+2(2),4(R2)                                               
         MVC   LDEMOLD,LDEMVALS                                                 
         GOTO1 AADJCALC            CALCULATE ADJUSTMENT FOR NEW                 
         BNE   VALRX                                                            
         MVC   LADJUST,LDEMADJ     DEMOS                                        
         BAS   RE,GETOLD           FIND RTG IN OLD DEMO ELEM                    
         BNE   VALR58                                                           
         XC    LDEMOLD,LDEMOLD     FOUND - CALCULATE ADJUSTMENT                 
***      MVC   LDEMOLD+1(3),5(R1)                                               
         MVC   LDEMOLD,4(R1)                                                    
         GOTO1 AADJCALC            (LDEMADJ = ADJUSTMENT PERCENTAGE)            
         BNE   VALRX                                                            
         OC    LDMOEL,LDMOEL                                                    
         BZ    VALR58                                                           
         LA    RF,LDMOEL           SCAN OLD DEMO ELEMNT TO BUILD LIST           
         USING DMOEL,RF            OF AUTO OVERRIDES                            
         LA    R0,L'DMODEMO                                                     
         ZIC   R1,DMOELLN                                                       
         AR    R1,RF                                                            
         BCTR  R1,0                                                             
         LA    RF,DMODEMO                                                       
         XC    LDEMOLD,LDEMOLD                                                  
         LA    R8,LALLVALS                                                      
*                                                                               
VALR48   CLC   1(2,RF),LADJDEM     TEST ADJUSTMENT DEMO                         
         BE    VALR50              YES - SKIP                                   
         TM    4(RF),DMODEMOV      TEST ALREADY MANUALLY OVERRIDDEN             
         BO    VALR50              YES - SKIP                                   
***      MVC   LDEMOLD+1(3),5(RF)                                               
         MVC   LDEMOLD,4(RF)                                                    
         BRAS  RE,ADJUST           ADJUST                                       
         MVC   0(2,R8),1(RF)                                                    
         MVC   2(4,R8),LDEMNEW     SAVE NEW AUTO-ADJUSTED DEMO VALUE            
         LA    R8,6(R8)                                                         
         MVI   0(R8),FF                                                         
VALR50   BXLE  RF,R0,VALR48                                                     
         B     VALR58                                                           
         DROP  RF                                                               
*                                                                               
VALR52   CLI   2(R2),C'R'          NOT AN 'ALL' ADJUSTMENT - TEST RTG           
         BNE   VALR58                                                           
         MVI   0(R9),C'I'          YES - ENTER IN IMP LIST                      
         MVC   1(1,R9),3(R2)                                                    
         MVC   LDEMS+1(2),0(R9)                                                 
         BAS   RE,GETOLD           FIND IMPRESSION IN OLD DEMO ELEM             
         BNE   VALR54                                                           
         TM    4(R1),DMODEMOV      FOUND - TEST MANUALLY OVERRIDDEN             
         BO    VALR54                                                           
         ST    R1,APDUB            NO - APDUB = A(IMP ENTRY)                    
         MVI   LDEMS+1,C'R'             FIND RATING IN OLD DEMO ELEM            
         BAS   RE,GETOLD                                                        
         BNE   VALR54                                                           
         XC    LDEMOLD,LDEMOLD     FOUND - CALCULATE PCT ADJUST                 
***      MVC   LDEMOLD+1(3),5(R1)                                               
         MVC   LDEMOLD,4(R1)                                                    
         XC    LDEMNEW,LDEMNEW                                                  
         MVC   LDEMNEW+2(2),4(R2)                                               
         GOTO1 AADJCALC                                                         
         BNE   VALRX                                                            
         L     R1,APDUB            AUTO ADJUST THE IMPRESSION                   
***      MVC   LDEMOLD+1(3),5(R1)                                               
         MVC   LDEMOLD,4(R1)                                                    
         BRAS  RE,ADJUST                                                        
         MVC   2(4,R9),LDEMNEW                                                  
         B     VALR56                                                           
*                                                                               
VALR54   MVI   LDEMS+1,C'R'                                                     
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   VALR54E                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
VALR54E  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS   GET ACTUAL RTG         
         XC    LDEMNEW,LDEMNEW                                                  
         MVC   LDEMNEW+2(2),4(R2)                                               
         MVC   LDEMOLD,LDEMVALS                                                 
         GOTO1 AADJCALC            CALCULATE ADJUSTMENT FOR NEW DEMO            
         BNE   VALRX                                                            
         MVC   2(4,R9),LDEMADJ                                                  
         OI    2(R9),X'80'                                                      
*                                                                               
VALR56   LA    R9,6(R9)                                                         
         MVI   0(R9),FF                                                         
*                                                                               
VALR58   LA    R2,6(R2)            NEXT DEMO OVERRIDE                           
         B     VALR46                                                           
*                                                                               
VALR60   LA    R0,LNDEMOS          PREPARE DEMO LIST FOR SPDEMUP                
         LA    R1,LDEMS                                                         
         LA    RE,ESTDEMS                                                       
*                                                                               
VALR62   OC    0(3,RE),0(RE)                                                    
         BZ    VALR64                                                           
         MVC   0(3,R1),0(RE)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,VALR62                                                        
*                                                                               
VALR64   MVI   0(R1),X'FF'                                                      
         LA    RE,LDEMOVR          OVERRIDE ELEMENT LIST                        
         ST    RE,SPUPAOVR                                                      
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   VALR64E                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
VALR64E  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS    CALL SPDEMUP          
*                                                                               
VALR64G  TM    BWDINDS,BWDIPRG                                                  
         BO    VALR65                                                           
         MVC   BWDPROG,SPACES                                                   
         MVC   BWDPROG(L'SPUPPRG),SPUPPRG                                       
         LA    R0,L'BWDPROG-2                                                   
         LA    R1,BWDPROG+L'BWDPROG-2                                           
         CLC   0(2,R1),=C'-S'                                                   
         BE    *+14                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-12                                                          
         B     VALR65                                                           
         MVC   0(2,R1),SPACES                                                   
*                                                                               
VALR65   MVC   DETPRG,BWDPROG      RE-DISPLAY THE PROGRAMMING                   
         OI    DETPRGH+6,FVOXMT                                                 
         MVC   BWDBOOK,SPUPFBK     ACTUAL BOOK                                  
*                                                                               
         CLI   APACTN,ACTADD       IF WE ADDING THEN WE NEED TO GET THE         
         BNE   VALR65Z                  NWS COMP RECORD                         
         GOTO1 AGETCMPR,APPARM,BWDDAYS,BWDTIMES,BWDSTA,LDEMVALS                 
         BNE   VALR65Z             NONE, USE DEMOS WE HAVE ALREADY              
         LA    R4,APELEM                                                        
         USING DMOEL,R4                                                         
         LA    R2,DMODEMO                                                       
         LA    R6,LDEMVALS                                                      
VALR65A  CLI   0(R2),X'FF'                                                      
         BE    VALR65Z                                                          
         TM    4(R2),X'80'         USER INPUT?                                  
         BNZ   *+14                                                             
         MVC   4(4,R2),0(R6)       NO, USE DEMO VALUE WE GOT                    
         B     *+10                                                             
         MVC   0(4,R6),4(R2)       YES, USER INPUT TAKES PRECEDENCE             
*                                                                               
         LA    R2,L'DMODEMO(R2)    NEXT DEMO                                    
         LA    R6,4(R6)                                                         
         B     VALR65A                                                          
*                                                                               
VALR65Z  LA    R4,APELEM                                                        
         USING DMOEL,R4                                                         
         LA    R2,DMODEMO          MOVE DEMO VALUES TO DEMO ELEMENT             
         LA    R6,LDEMVALS                                                      
*                                                                               
VALR66   CLI   0(R2),X'FF'         TEST FOR END                                 
         BE    VALR76                                                           
         OC    5(3,R2),5(R2)       IS IT EMPTY?                                 
         BNZ   VALR66E              - NOPE                                      
         TM    4(R2),X'80'         OVERRIDDEN?                                  
         BO    VALR66E              - YUP                                       
         OC    4(4,R2),0(R6)       DEMO VALUE (PRESERVING OVERRIDE BIT)         
*  ONLY THE FIRST BYTE FOR THE BITS!!!                                          
VALR66E  TM    LINDS,LAUTADJ       TEST FOR AUTO ADJUSTMENTS                    
         BZ    VALR75                                                           
         TM    4(R2),DMODEMOV      YES - TEST THIS IS A MANUAL OVERRIDE         
         BO    VALR75                                                           
         LA    R8,LIMPVALS         NO - SEE IF THERE'S AN IMP ADJUST            
*                                                                               
VALR68   CLI   0(R8),FF                                                         
         BE    VALR72              NO                                           
         CLC   0(2,R8),1(R2)                                                    
         BNE   VALR70                                                           
         TM    2(R8),X'80'         YES - TEST DO THE ADJUSTMENT NOW             
         BO    *+14                                                             
         MVC   4(4,R2),2(R8)       NO - MOVE IN THE ADJUSTED DEMO VALUE         
         B     VALR75                                                           
         MVC   LDEMOLD,0(R6)       YES - DO THE ADJUSTMENT                      
         XC    LDEMADJ,LDEMADJ                                                  
         MVC   LDEMADJ+1(3),3(R8)                                               
         BRAS  RE,ADJUST                                                        
         MVC   4(4,R2),LDEMNEW     ADJUSTED DEMO VALUE                          
         B     VALR75                                                           
*                                                                               
VALR70   LA    R8,6(R8)                                                         
         B     VALR68                                                           
*                                                                               
VALR72   TM    LINDS,LALLADJ       TEST FOR 'ALL' ADJUSTMENTS                   
         BZ    VALR75                                                           
         LA    R8,LALLVALS                                                      
*                                                                               
VALR73   CLI   0(R8),FF            SEE IF ADJUSTED DEM VALUE IS IN LIST         
         BE    VALR74                                                           
         CLC   0(2,R8),1(R2)                                                    
         BNE   *+14                                                             
         MVC   4(4,R2),2(R8)       YES - MOVE IN AUTO-ADJUSTED VALUE            
         B     VALR75                                                           
         LA    R8,6(R8)                                                         
         B     VALR73                                                           
*                                                                               
VALR74   MVC   LDEMADJ,LADJUST     NO - DO THE ADJUSTMENT WITH RAW RTG          
         MVC   LDEMOLD,0(R6)            ADJUST PCT                              
         BRAS  RE,ADJUST                                                        
         MVC   4(4,R2),LDEMNEW                                                  
*                                                                               
VALR75   LA    R2,L'DMODEMO(R2)    NEXT DEMO                                    
         LA    R6,4(R6)                                                         
         B     VALR66                                                           
*                                                                               
VALR76   MVI   0(R2),0                                                          
         XC    LRTG,LRTG           SAVE PRIMARY DEMO VALUE                      
         MVC   LRTG+1(3),DMODEMO+5                                              
*                                                                               
         GOTO1 AADDELS,BWDRECD     ADD THE DEMO ELEMENT                         
*                                                                               
         GOTO1 APUTCMPR,APPARM,(X'C0',BWDDAYS),BWDTIMES,BWDSTA,        X        
               APELEM,BWDPROG                                                   
*                                                                               
         XC    EBLOCK,EBLOCK       NOW REDISPLAY THE DEMO VALUES                
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         LA    R1,DMODEMO                                                       
         LA    R8,L'DMODEMO                                                     
         ZIC   R9,DMOELLN                                                       
         LA    R9,DMOEL(R9)                                                     
         BCTR  R9,0                                                             
         LA    R6,DETDM1H                                                       
*                                                                               
***  2 DECIMAL  ***                                                             
*ALR78   ST    R1,APPARM                                                        
*        LR    R0,R1               SAVE OFF R1                                  
*        GOTO1 AADJPREC,APPARM,,   SEE IF WE NEED TO CONVERT                    
*        LR    R1,R0               PUT IT BACK IN R1                            
***  2 DECIMAL  ***                                                             
VALR78   MVC   APFULL,4(R1)                                                     
         GOTO1 ADISDEM                                                          
         BXLE  R1,R8,*+8                                                        
         B     VALR80                                                           
         ZIC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         TM    FVATRB-FVIHDR(R6),FVAPROT                                        
         BO    *-10                                                             
         B     VALR78                                                           
*                                                                               
VALR80   L     R2,AIOAREA1         BUILD NEW HEADER RECORD                      
         TM    LINDS,LNOHDR        IF A HEADER EXISTS,                          
         BO    VALR82                                                           
         TM    LINDS,LNEWSTA       TEST FOR NEW STATION                         
         BZ    VALR86                                                           
         B     VALR84                                                           
*                                                                               
VALR82   XC    BWHKEY(256),BWHKEY   NO HEADER - BUILD FROM SCRATCH              
         MVC   BWHKEY,LHDRKEY                                                   
         MVC   APHALF,BWHKSEQ       SAVE SEQ NO IN APHALF                       
         MVI   BWHELCD,BWHELCDQ                                                 
         MVI   BWHELLN,BWHELLNQ                                                 
         MVI   BWHSEQ,1                                                         
         MVC   BWHSTA,QSTA                                                      
         LA    R0,BWHEL-BWHRECD+1+BWHELLNQ                                      
         STCM  R0,3,BWHLEN                                                      
*                                                                               
VALR84   DS    0H                   CK FOR '06' ELEM, ADD/UPDATE IT             
         BRAS  RE,UPD06EL                                                       
         LA    R1,FILADD1           PUT NEW HEADER RECORD                       
         TM    LINDS,LNOHDR                                                     
         BO    *+8                                                              
         LA    R1,FILPUT1                                                       
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    LINDS,LNOHDR        TEST NEW HEADER RECORD                       
         BZ    VALR86                                                           
         XC    IOKEY,IOKEY         YES-ADD PASSIVE POINTER                      
         LA    R2,IOKEY                                                         
         MVI   BWHPTYP,BWHPTYPQ                                                 
         MVI   BWHPSUB,BWHPSUBQ                                                 
         MVC   BWHPAGMD,BAGYMD                                                  
         OC    BWHPAGMD,BBYRMASK                                                
         MVC   BWHPBYR,BBYR                                                     
         MVC   BWHPSEQ,APHALF                                                   
         MVC   BWHKCNTL+1(4),IODA                                               
         GOTO1 AIO,DIRADD                                                       
         BNL   VALR86                                                           
         DC    H'0'                                                             
*                                                                               
VALR86   CLI   APACTN,ACTADD       TEST ACTION=ADD                              
         BNE   VALR88                                                           
         L     R2,AIOAREA1                                                      
         LA    R1,BWHFSTEL         YES-DOUBLE CHECK THAT THE                    
         SR    R0,R0                   STATION CODE IS CORRECT                  
*                                                                               
VALR87   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),BWHELCDQ                                                   
         BNE   *+14                                                             
         USING BWHEL,R1                                                         
         CLC   BWHSTA,BWDSTA                                                    
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VALR87                                                           
         CLC   BWHSEQ,BWDKELST                                                  
         BE    VALR88                                                           
         DC    H'0'                                                             
*                                                                               
VALR88   LA    R1,MINADD2          PUT DETAIL RECORD                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,MINWRT2                                                       
         GOTO1 AMIN                                                             
         MVC   APRECKEY(13),BWDKEY  SET POSSIBLE KEY CHANGE FOR GENERAL         
**                                                                              
         TM    APROFBTS,ASDNOWRK   DO WE NEED CHKAUTH?                          
         BO    *+8                                                              
         BRAS  RE,CHKAUTH                                                       
**                                                                              
VALRX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND DEMO ENTRY IN OLD DEMO ELEMENT                      *         
* INPUT  : LDEMS(3) = DEMO                                            *         
* OUTPUT : CC EQ 0 AND R1 = A(DEMO ENTRY)                             *         
*          CC NE 0 - DEMO NOT FOUND                                   *         
***********************************************************************         
         SPACE 1                                                                
GETOLD   LR    R0,RE                                                            
         OC    LDMOEL,LDMOEL                                                    
         BZ    GE10                                                             
         LA    R1,LDMOEL                                                        
         USING DMOEL,R1                                                         
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,DMOELLN                                                       
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,DMODEMO                                                       
         CLC   1(2,R1),LDEMS+1                                                  
         BE    *+14                                                             
         BXLE  R1,RE,*-10                                                       
GE10     LTR   RE,R0                                                            
         B     *+8                                                              
         LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* MISC VALIDATAION ROUTINES                                           *         
***********************************************************************         
         SPACE 1                                                                
VALRCST  ST    RE,APFULL        ** VALIDATE COST **                             
         GOTO1 AFVAL                                                            
         BH    VALRX                                                            
         BE    *+14                                                             
         XC    0(L'BWDCOST1,R8),0(R8)                                           
         B     VALRCSTX                                                         
         CLC   DETC2NH,0(R1)       IS IT COST2 FIELD?                           
         BE    VALRCST5             - YUP, THERE IS NO EFFECTIVE DATE           
         CLI   APFLAG,1            COST FOUND -                                 
         BNE   *+14                 IF NO EFFECTIVE DATE, ERROR                 
         MVC   FVADDR,APDUB                                                     
         B     EMED                                                             
VALRCST5 ZIC   RF,FVILEN                                                        
         ST    RF,APPARM+4                                                      
         ST    R1,LSVREG1                                                       
         GOTO1 VCASHVAL,APPARM,FVIFLD                                           
         CLI   APPARM,X'FF'                                                     
         BE    EICO                                                             
*****  THE COST CANNOT BE MORE THAN $4294967 OR IT DIES IN BW5C LATER           
         L     RE,APPARM+4                                                      
         C     RE,=F'429496700'                                                 
         BH    EICO                HIGHER, COST IS TOO BIG                      
*****                                          MHC  07/21/03                    
         GOTO1 ANETCOST,APPARM+4   NET DOWN IF NECESSARY                        
         L     R1,LSVREG1                                                       
         CLC   DETC2NH,0(R1)       WE ON THE COS 2 FIELD HEADER?                
         BE    VALRCST9             - YUP, ALWAYS WANT THE MOVE                 
         CLI   APACTN,ACTADD       IF NOT ACTION=ADD                            
         BE    *+12                                                             
         TM    FVIIND-FVIHDR(R1),FVIVAL   AND PREVIOUSLY VALIDATED,             
         BO    VALRCSTX                   THEN AVOID DOUBLE NET DOWN            
VALRCST9 MVC   0(L'BWDCOST1,R8),APPARM+4                                        
*                                                                               
VALRCSTX L     RE,APFULL                                                        
         BR    RE                                                               
         SPACE 2                                                                
VALRDAT  ST    RE,APFULL        ** VALIDATE DATE **                             
         ST    R1,APDUB+4                                                       
         XC    0(L'BWDEFDT2,R8),0(R8)                                           
         CLI   5(R1),0                                                          
         BNE   VALRDAT2                                                         
         CLI   APFLAG,1            DATE NOT FOUND -                             
         BE    VALRDATX             IF PREVIOUS EMPTY DATE, OK                  
         MVI   APFLAG,1             ELSE                                        
         MVC   APDUB(4),APDUB+4        SAVE A(FIRST EMPTY DATE HDR)             
         B     VALRDATX                                                         
*                                                                               
VALRDAT2 GOTO1 AVALDAT                                                          
         BNE   VALRX                                                            
         CLI   APFLAG,1            DATE FOUND -                                 
         BNE   *+14                                                             
         MVC   FVADDR,APDUB         IF PREVIOUS EMPTY DATE, ERROR               
         B     EMED                                                             
         OC    APWORK+6(6),APWORK+6                                             
         BNZ   EIDT                ONLY ONE DATE PLEASE                         
         GOTO1 VDATCON,APPARM,APWORK,(3,0(R8))                                  
*                                                                               
VALRDATX L     RE,APFULL                                                        
         BR    RE                                                               
         SPACE 2                                                                
VALRCPP  SR    R0,R0            ** CALCULATE CPP **                             
         M     R0,=F'10'           IN: R1=COST                                  
         DR    R0,R8                   R8=RATING                                
         BR    RE                  OUT:R1=CPP                                   
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
DISKEY   GOTO1 ADISPKEY                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISREC   GOTO1 =A(DISPRECD),RR=APRELO                                           
         B     EXIT                                                             
***********************************************************************         
* DELETE RECORD                                                       *         
***********************************************************************         
         SPACE 1                                                                
DELREC   MVC   IOKEY,APRECKEY                                                   
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIOAREA2         CHECK RECORD FOR BUY TRANSFER                
         LA    R1,BWDEL            ELEMENT                                      
         SR    R0,R0                                                            
*                                                                               
DELR1    CLI   0(R1),0                                                          
         BE    DELR2                                                            
         CLI   0(R1),BTRELCDQ      YES-CANNOT DELETE                            
         BE    DELR9                                                            
         CLI   0(R1),DTRELCDQ                                                   
         BE    DELR9                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     DELR1                                                            
*                                                                               
DELR2    TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    DELR3                                                            
         GOTO1 ACHPTDOL            YES-CHANGE ACTUAL PTS/DOL                    
*                                                                               
DELR3    GOTO1 AMIN,MINDEL                                                      
*                                                                               
         CLI   BWDKELPO,0          TEST PACKAGE/ORBIT RECORD                    
         BE    DELR8                                                            
         CLI   BWDKELSQ,0          YES-TEST PACKAGE/ORBIT SLAVE                 
         BNE   DELRX               YES-EXIT                                     
*                                  NO-THEN IT'S A PACKAGE/ORBIT MASTER          
         MVC   IOKEY,APRECKEY      DELETE ASSOCIATED SLAVES                     
         LA    R1,MINHI2                                                        
         B     DELR4+4                                                          
*                                                                               
DELR4    LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DELR8                                                            
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   DELR8                                                            
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    DELR6                                                            
         GOTO1 ACHPTDOL            YES-CHANGE ACTUAL PTS/DOL                    
*                                                                               
DELR6    GOTO1 AMIN,MINDEL                                                      
         B     DELR4               READ ALL PACKAGE RECORDS                     
*                                                                               
DELR8    B     DELRX                                                            
*                                                                               
DELR9    MVC   FVMSGNO,=AL2(FVNODEL)    DELETE PROHIBITED                       
*                                                                               
DELRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* OUTPUT : FVMSGNO NE FVFOK IF KEY IS INVALID                         *         
*          APRECKEY                                                   *         
*          APPARM FOR ROOT                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   DS    0H                                                               
         BRAS  RE,VALSLPAR                                                      
         B     EXIT                                                             
***********************************************************************         
* FIRST LIST SCREEN HOOK                                              *         
***********************************************************************         
         SPACE 1                                                                
FSTSCR   SR    R1,R1                                                            
         OC    INORTG,INORTG       TEST FOR RATING OPTION                       
         BZ    FSCR4                                                            
         LA    RE,ESTDEMS                                                       
*                                                                               
FSCR2    OC    0(3,RE),0(RE)       YES - VALIDATE IT                            
         BZ    ERTGOP                                                           
         CLC   1(2,RE),INORTG+1                                                 
         BE    FSCR4                                                            
         LA    RE,3(RE)                                                         
         BCT   R1,FSCR2                                                         
*                                                                               
FSCR4    LPR   R1,R1                                                            
         LA    R8,ESTDEMS                                                       
         GOTO1 AGETDEMS                                                         
         LA    RE,L'LDNAMES                                                     
         MR    R0,RE                                                            
         LA    R1,LDNAMES(R1)      POINT TO TARGET DEMO NAME                    
         MVC   DUPHED(L'LDNAMES),0(R1)   FORMAT DEMO NAME TO HEADLINE           
         OI    DUPHEDH+6,FVOXMT                                                 
*                                                                               
FSCRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   MVC   IOKEY(13),APRECKEY                                               
         LA    R1,MINSEQ2                                                       
         CLI   APINDS,APILNSEQ                                                  
         BE    GETS2                                                            
         LA    R1,MINHI2                                                        
*                                                                               
GETS2    GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETS99                                                           
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   GETS99                                                           
         L     R3,AIOAREA2                                                      
         CLC   BWDDPT,BDPT         MATCH DAYPART                                
         BNE   GETS3                                                            
         CLC   BWDSLN,BSLN         MATCH SPOT LENGTH                            
         BNE   GETS3                                                            
         CLI   LSUBDPT,0           TEST SUBDAYPART FILTER                       
         BE    GETS4                                                            
         CLC   BWDSUBDP,LSUBDPT    YES-MATCH SUBDAYPART                         
         BE    GETS4                                                            
*                                                                               
GETS3    LA    R1,MINSEQ2                                                       
         B     GETS2                                                            
*                                                                               
GETS4    XC    APRECKEY,APRECKEY                                                
         MVC   APRECKEY(13),IOKEY                                               
         B     GETSX                                                            
*                                                                               
GETS99   MVI   APMODE,APMEOFS                                                   
*                                                                               
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD                                          *         
***********************************************************************         
         SPACE  1                                                               
DISSEL   L     R5,APPARM                                                        
         USING DUPL1H,R5                                                        
         L     R3,AIOAREA2                                                      
         LA    R1,DUPCSTH          COST                                         
         LA    R8,BWDCOST1                                                      
         XC    EBLOCK,EBLOCK       SET UP EBLOCK FOR EDITING                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBALIGN,C'L'                                                     
         MVI   EBFLOAT,C'$'                                                     
         BAS   R9,DISRCST                                                       
*                                                                               
         LA    R4,BWDEL            RATING                                       
         SR    R0,R0                                                            
*                                                                               
DISS2    CLI   0(R4),0                                                          
         BE    DISS6                                                            
         CLI   0(R4),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DISS2                                                            
         USING DMOEL,R4                                                         
         LA    R1,DMODEMO          FIND TARGET RATING                           
         OC    INORTG,INORTG                                                    
         BZ    DISS4                                                            
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         CLC   1(2,R1),INORTG+1                                                 
         BE    DISS4                                                            
         BXLE  R1,RE,*-10                                                       
         B     DISS6                                                            
*                                                                               
***  2 DECIMAL  ***                                                             
*ISS4    ST    R1,APPARM                                                        
*        LR    R0,R1               SAVE OFF R1                                  
*        GOTO1 AADJPREC,APPARM,,   SEE IF WE NEED TO CONVERT                    
*        LR    R1,R0               PUT IT BACK IN R1                            
***  2 DECIMAL  ***                                                             
DISS4    MVC   APFULL,4(R1)                                                     
         LA    R6,DUPRTGH                                                       
         GOTO1 ADISDEM                                                          
         OI    DUPRTGH+6,FVOXMT                                                 
*                                                                               
DISS6    MVC   DUPPRG,BWDPROG      PROGRAMMING                                  
         OI    DUPPRGH+6,FVOXMT                                                 
*                                                                               
         MVC   DUPCOD,BWDUCODE     USER CODE                                    
         OI    DUPCODH+6,FVOXMT                                                 
*                                                                               
         XC    DUPDAT,DUPDAT       DATES                                        
         OI    DUPDATH+6,FVOXMT                                                 
         OC    BWDDATES,BWDDATES                                                
         BZ    DISS8                                                            
         GOTO1 VDATCON,APPARM,(2,BWDDATES),(8,DUPDAT)                           
         MVI   DUPDAT+8,C'-'                                                    
         GOTO1 (RF),(R1),(2,BWDDATES+2),(8,DUPDAT+9)                            
*                                                                               
         DROP  R5                                                               
DISS8    L     R5,ATWA             RE-ESTABLISH TWA ADDRESSABILITY              
         USING TWAD,R5                                                          
*                                                                               
         B      EXIT                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST/SELECT DATA                                           *         
* INPUT  : APPARM(4)=A(TWA DISPLAY LINE)                              *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   DS    0H                                                               
         GOTO1 =A(VALISEL),RR=APRELO                                            
         B     EXIT                                                             
***********************************************************************         
* DISPLAY COST ROUTINE                                                *         
* R1 = A(FIELD HEADER)                                                *         
* R8 = A(4-BYTE COST)                                                 *         
* R9 = RETURN ADDRESS                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISRCST  ST    R4,LAR4SAVE         SAVE OFF R4 BEFORE IT GETS DESTROYED         
         MVI   EBDECS,2                                                         
         ST    R8,EBAIN                                                         
         LA    RE,L'FVIHDR(R1)                                                  
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'DETCS1                                                  
         ICM   R4,15,0(R8)                                                      
         BNZ   *+14                                                             
         MVC   0(2,RE),=C'$0'                                                   
         B     DISRCSTX                                                         
         SR    RE,RE                                                            
         LR    RF,R4                                                            
         D     RE,=F'100'                                                       
         LTR   RE,RE                                                            
         BZ    *+12                                                             
         C     RF,=F'100000'                                                    
         BL    DISRCST2                                                         
         MVI   EBDECS,0                                                         
         ST    RF,APDUB                                                         
         LA    RE,APDUB                                                         
         ST    RE,EBAIN                                                         
*                                                                               
DISRCST2 ST    R1,APDUB+4                                                       
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         L     R1,APDUB+4                                                       
*                                                                               
DISRCSTX OI    6(R1),FVOXMT                                                     
         OI    FVIIND-FVIHDR(R1),FVIVAL  SET PREVIOUSLY VALIDATED BIT           
         L     R4,LAR4SAVE         LET'S PUT R4 BACK                            
         BR    R9                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATE ROUTINE                                                *         
* R4 = A(FIELD HEADER)                                                *         
* R8 = A(3-BYTE DATE)                                                 *         
* R9 = RETURN ADDRESS                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISRDAT  GOTO1 VDATCON,APPARM,(3,0(R8)),(8,L'FVIHDR(R4))                        
         OI    6(R4),FVOXMT                                                     
         BR    R9                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET DEMO NAMES AND FORMAT TO THE SCREEN                  *         
* INPUT : R8 = A(LIST OF 3-BYTE DEMO CODES)                           *         
***********************************************************************         
         SPACE 1                                                                
FMTDEMS  ST    RE,APFULL                                                        
         GOTO1 AGETDEMS            GET THE NAMES                                
         LA    R0,LNDEMOS          FORMAT DEMO NAMES TO SCREEN                  
         SR    R1,R1                                                            
         LA    RE,DETDN1H                                                       
         LA    RF,LDNAMES                                                       
*                                                                               
FMTD2    OC    0(7,RF),0(RF)                                                    
         BZ    FMTDX                                                            
         MVC   L'FVIHDR(L'DETDN1,RE),0(RF)                                      
         OI    6(RE),FVOXMT                                                     
         LA    RF,7(RF)                                                         
         BCT   R0,*+8                                                           
         B     FMTDX                                                            
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         TM    FVATRB-FVIHDR(RE),FVAPROT                                        
         BZ    *-10                                                             
         B     FMTD2                                                            
*                                                                               
FMTDX    L     RE,APFULL                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ENOC     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
EIDT     MVC   FVMSGNO,=AL2(FVIDAT)                                             
         B     EXIT                                                             
EICO     MVC   FVMSGNO,=AL2(FVICST)                                             
         B     EXIT                                                             
EMED     MVC   FVMSGNO,=AL2(FVNOEFDT)                                           
         B     EXIT                                                             
EDLO     MVC   FVMSGNO,=AL2(FVDTLO)                                             
         B     EXIT                                                             
EDHI     MVC   FVMSGNO,=AL2(FVDTHI)                                             
         B     EXIT                                                             
EDLL     MVC   FVMSGNO,=AL2(FVDLL)                                              
         B     EXIT                                                             
EDIV     MVC   FVMSGNO,=AL2(FVIEFDT)                                            
         B     EXIT                                                             
EESD     MVC   FVMSGNO,=AL2(FVDEMES)                                            
         B     EXIT                                                             
ESUB     MVC   FVMSGNO,=AL2(FVISDPT)                                            
         B     EXIT                                                             
ENOSLN   MVC   FVMSGNO,=AL2(FVNOSLN)                                            
         B     EXIT                                                             
EINAWK   MVC   FVMSGNO,=AL2(FVINAWK)                                            
         B     EXIT                                                             
*                                                                               
EBADREP  MVC   FVMSGNO,=AL2(FVIREP)                                             
         LA    R1,DETREPNH                                                      
         B     EEXITR1                                                          
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         LA    R1,DUPMEDH                                                       
         B     EEXITR1                                                          
EMAXSTA  MVC   FVMSGNO,=AL2(FVSTAOV)                                            
         LA    R1,DETSTAH                                                       
         B     EEXITR1                                                          
ECMSEQ   MVC   FVMSGNO,=AL2(FVCMSEQ)                                            
         LA    R1,DETNUMH                                                       
         B     EEXITR1                                                          
ETMD     MVC   FVMSGNO,=AL2(FVTMD)                                              
         LA    R1,DETMEDH                                                       
         B     EEXITR1                                                          
ENUE     MVC   FVMSGNO,=AL2(FVNOUPG)                                            
         LA    R1,DETUPGH                                                       
         B     EEXITR1                                                          
ERTGOP   MVC   FVMSGNO,=AL2(FVIRTGOP)                                           
         LA    R1,BWSOPTH                                                       
         B     EEXITR1                                                          
EDUPREC  MVC   FVMSGNO,=AL2(FVDUPREC)                                           
         XC    BWSACT,BWSACT                                                    
         MVC   BWSACT(3),=C'DUP'                                                
         MVI   BWSACTH+FVILEN-FVIHDR,3                                          
         OI    BWSACTH+6,FVOXMT                                                 
         XC    BWSKEY,BWSKEY                                                    
         MVI   BWSKEY,C','                                                      
         MVI   BWSKEYH+FVILEN-FVIHDR,1                                          
         OI    BWSKEYH+6,FVOXMT                                                 
         LA    R1,BWSRECH                                                       
         B     EEXITR1                                                          
*                                                                               
EEXITR1  ST    R1,FVADDR                                                        
         B     EXIT                                                             
         EJECT                                                                  
FF       EQU   X'FF'                                                            
OVERELEM EQU   X'DE'                                                            
*                                                                               
***EXIT     XIT1  ,                                                             
EXIT     TM    TWATAMFL,TWAFRTAM   CAME FROM TAM?                               
         BZ    EXITX                                                            
         CLC   FVMSGNO,=AL2(FVFOK) EVERYTHING OKAY?                             
         BE    EXITX                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FVMSGNO                                                     
         GOTO1 ASPCLERR,(R1)       NO, RETURN SPECIAL ERROR                     
EXITX    XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
SPACES   DC    32C' '                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST/SELECT DATA                                           *         
* INPUT  : APPARM(4)=A(TWA DISPLAY LINE)                              *         
***********************************************************************         
VALISEL  NTR1  BASE=*,LABEL=*                                                   
         L     R4,APPARM                                                        
         USING DUPL1H,R4                                                        
         MVC   IOKEY(13),APRECKEY  READ THE RECORD                              
         GOTO1 AMIN,MINRD2                                                      
         BE    *+14                                                             
         MVC   FVADDR,APPARM                                                    
         B     VALSX                                                            
         L     R3,AIOAREA2                                                      
         NI    LINDS,255-LCHG                                                   
         TM    DUPCODH+FVIIND-FVIHDR,FVIVAL  TEST USER CODE CHANGED             
         BO    VALS2                                                            
         MVC   APFULL,DUPCOD                                                    
         OC    APFULL,SPACES                                                    
         CLC   BWDUCODE,APFULL     TEST CODE CHANGED                            
         BE    VALS2                                                            
         MVC   BWDUCODE,APFULL     YES-REPLACE                                  
         OI    LINDS,LCHG                                                       
*                                                                               
VALS2    TM    DUPDATH+FVIIND-FVIHDR,FVIVAL  TEST DATES CHANGED                 
         BO    VALS4                                                            
         GOTO1 AVALDATE,DUPDATH    YES-VALIDATE DATES                           
         BNE   VALSX                                                            
         CLC   APFULL,BWDDATES     TEST ANY CHANGE                              
         BNE   VALS3                                                            
         CLC   APHALF,BWDWKS                                                    
         BNE   VALS3                                                            
         CLI   BWDELLN,BWDELLNQ           DESC ELEM IN THE OLD FORMAT?          
         BNH   VALS4                                                            
         CLC   APDUB(L'BWDWKS2),BWDWKS2   NO, CHECK 2ND MASK BITS SET           
         BE    VALS4                                                            
VALS3    OI    LINDS,LCHG          YES                                          
         MVC   BWDDATES,APFULL     SET DATES                                    
         MVC   BWDWKS,APHALF       SET INACTIVE WEEKS                           
         MVC   BWDWKS2,APDUB       GET OTHER INACTIVE WEEKS                     
*                                                                               
VALS4    TM    LINDS,LCHG          TEST RECORD CHANGED                          
         BZ    VALSX                                                            
         GOTO1 AMIN,MINWRT2        YES-WRITE BACK RECORD                        
         BE    VALSX                                                            
         DC    H'0'                                                             
*                                                                               
VALSX    J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK IO ERROR BITS                                      *         
* IN : IOERR  - IO ERROR RETURN BYTE                                  *         
* OUT: APINDS - APPLICATION INDICATORS BYTE                           *         
*      CC     - EQ  OK                                                *         
*             - NE  NON RECOVERABLE ERROR                             *         
***********************************************************************         
         SPACE 1                                                                
IOCHECK  NTR1  BASE=*,LABEL=*                                                   
         TM    IOERR,IOEDSK        NON-RECOVERABLE DISK ERROR                   
         BO    IOCH99                                                           
*                                                                               
         MVI   APINDS,0                                                         
         OI    APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
*                                                                               
         TM    IOERR,IOEEOF        END-OF-FILE                                  
         BO    *+12                                                             
         TM    IOERR,IOERNF        RECORD NOT FOUND                             
         BZ    *+12                                                             
         NI    APINDS,FF-APIOKDIS-APIOKCHA-APIOKDEL                             
         OI    APINDS,APIOKADD                                                  
*                                                                               
         TM    IOERR,IOEDEL         RECORD IS DELETED                           
         BZ    *+12                                                             
         NI    APINDS,FF-APIOKADD-APIOKCHA-APIOKDEL                             
         OI    APINDS,APIOKRES                                                  
*                                                                               
         CR    RE,RE                                                            
         B     IOCHX                                                            
*                                                                               
IOCH99   MVC   FVMSGNO,=AL2(FVFIOER)    IO ERROR                                
         LA    R1,DETMEDH                                                       
         ST    R1,FVADDR                                                        
         LTR   RE,RE                                                            
*                                                                               
IOCHX    J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROTECTS INPUT FIELDS BECAUSE THE CAMP/MKT IS UNDER BUY REVISIONS             
***********************************************************************         
PROTIFLD NTR1  BASE=*,LABEL=*                                                   
         LA    RE,FLDTABLE                                                      
PIFLD10  OC    0(L'FLDTABLE,RE),0(RE)   ANY MORE INPUT FLDS?                    
         BZ    PIFLD50                                                          
*                                                                               
         XR    R1,R1               R1 = A(FIELD)                                
         ICM   R1,3,0(RE)                                                       
         AR    R1,R5                                                            
         OI    6(R1),X'20'+X'80'   PROTECT AND TRANSMIT THE FIELD               
         LA    RE,L'FLDTABLE(RE)   NEXT FIELD                                   
         B     PIFLD10                                                          
*                                                                               
PIFLD50  LA    R1,DETDM1H                                                       
PIFLD60  OI    6(R1),X'20'+X'80'   PROTECT AND TRANSMIT THE FIELD               
*                                                                               
         LA    R1,DETDM1X-DETDN1H+L'DETDM1X(R1)                                 
         LA    R0,DETCM1H                                                       
         CR    R1,R0                                                            
         BL    PIFLD60                                                          
PIFLDX   B     EXIT                                                             
*                                                                               
FLDTABLE DS    0XL2                DISP INTO TWA FOR SPECIFIC INPUT             
         DC    AL2(DETDAYH-TWAD)                                                
         DC    AL2(DETTIMH-TWAD)                                                
         DC    AL2(DETDPLH-TWAD)                                                
         DC    AL2(DETSDPH-TWAD)                                                
         DC    AL2(DETCS1H-TWAD)                                                
         DC    AL2(DETEF2H-TWAD)                                                
         DC    AL2(DETCS2H-TWAD)                                                
         DC    AL2(DETEF3H-TWAD)                                                
         DC    AL2(DETCS3H-TWAD)                                                
         DC    AL2(DETPRGH-TWAD)                                                
         DC    AL2(DETIDH-TWAD)                                                 
         DC    AL2(DETDATH-TWAD)                                                
         DC    AL2(DETUPGH-TWAD)                                                
         DC    AL2(DETCODH-TWAD)                                                
         DC    AL2(DETCM1H-TWAD)                                                
         DC    AL2(DETCM2H-TWAD)                                                
         DC    AL2(DETCM3H-TWAD)                                                
         DC    AL2(DETCM4H-TWAD)                                                
         DC    AL2(DETCM5H-TWAD)                                                
         DC    X'0000'                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO AUTO ADJUST A DEMO VALUE                                 *         
* INPUT  : LDEMOLD = DEMO VALUE                                       *         
*          LDEMADJ = PERCENT ADJUSTMENT                               *         
* OUTPUT : LDEMNEW = ADJUSTED DEMO VALUE                              *         
***********************************************************************         
         SPACE 1                                                                
ADJUST   NTR1  BASE=*,LABEL=*                                                   
         L     RE,LDEMOLD                                                       
***  2 DECIMAL                                                                  
         STCM  RE,8,APWORK+64                                                   
         NI    APWORK+64,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         ICM   RE,8,APWORK+64                                                   
***  2 DECIMAL                                                                  
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         M     RE,LDEMADJ                                                       
         D     RE,=F'1000'                                                      
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,LDEMNEW                                                       
***  2 DECIMAL                                                                  
         TM    LDEMOLD,X'40'       IS IT 2 DECIMAL?                             
         JZ    EXIT                 - NOPE                                      
         OI    LDEMNEW,X'40'                                                    
***  2 DECIMAL                                                                  
         J     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*  REP CHECK                          MHC   06/16/04                  *         
***********************************************************************         
REPCHK   NTR1  BASE=*,LABEL=*                                                   
         MVC   LTMPKEY,IOKEY                                                    
*                                                                               
         GOTO1 VCOLY,APPARM,0,X'D9000ABC'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),APPARM,(C'P',DETREPN),LREP                                  
         BNZ   REPNO                                                            
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVI   IOKEY,C'R'                                                       
         MVC   IOKEY+1(1),QMED                                                  
         MVC   IOKEY+2(3),DETREPN                                               
         MVC   IOKEY+5(2),QAGY                                                  
         MVC   IOKEY+7(10),=10C'0'                                              
         L     R4,AIOAREA4                                                      
         MVC   LIOADDR,IOADDR                                                   
         MVC   IOADDR,AIOAREA4     LET'S USE AIO4!!!                            
****                                                                            
         GOTO1 AIO,IOSTAFIL+IOHI                                                
****                                                                            
         MVC   IOADDR,LIOADDR      MOVE ORIGINAL IOADDR BACK!!                  
         CLC   IOKEY(7),0(R4)      DID WE FIND A MATCHING RECORD?               
         BNE   REPNO                - NOPE WE DIDN'T                            
*                                                                               
REPYES   CR    RE,RE               SET THE CONDITION                            
         B     REPCHKX                                                          
REPNO    CR    RE,RA                                                            
*                                                                               
REPCHKX  MVC   IOKEY,LTMPKEY                                                    
         J     EXITX                                                            
***********************************************************************         
* COS2 CHECK                          MHC   06/08/04                  *         
***********************************************************************         
COS2CHK  NTR1  BASE=*,LABEL=*                                                   
         MVC   LTMPKEY,IOKEY       SAVE OFF THE IOKEY                           
***                                                                             
         LA    R4,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R4                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
*                                                                               
         MVC   APFULL,IOADDR       SAVE OFF THE IOADDR                          
         MVC   IOADDR,AIOAREA4     WE'LL USE AIOAREA4                           
*                                                                               
         GOTO1 AIO,FILRD           READ CLIENT RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   IOADDR,APFULL       WE'RE MOVING IT BACK                         
*                                                                               
         L     R4,AIOAREA4                                                      
         TM    COPT4,COP4TRD       TRADE?                                       
         BO    COS2CHK8             - YUP!  EQUAL ON EXIT                       
         TM    COPT3,COP3COSQ  OPTIONAL?                                        
         BNO   COS2NO               - NOPE, NOT EQUAL ON EXIT                   
         DROP  R4                                                               
*                                                                               
COS2CHK8 OI    LINDS,LCOS2                                                      
         NI    DETC2KH+1,X'FF'-X'0C'   NORMAL INTENSITY                         
         NI    DETC2NH+1,X'FF'-X'0C'   NORMAL INTENSITY                         
         NI    DETC2NH+1,X'FF'-X'20'   TAKE OFF PROTECTION                      
         OI    DETC2KH+6,X'80'                                                  
         OI    DETC2NH+6,X'80'                                                  
*                                                                               
COS2YES  B     COS2CHKX                                                         
                                                                                
COS2NO   DS    0H                                                               
         NI    LINDS,X'FF'-LCOS2   TAKE OFF THIS FLAG                           
         TM    TWATAMFL,TWAFRTAM   CAME FROM TAM?                               
         BNZ   COS2CHKX             - YUP WE DID                                
         XC    DETC2N,DETC2N       WIPE OUT THIS, WE DON'T NEED COST 2          
         B     COS2CHKX             - NOPE, CONTINUE NORMALLY                   
COS2NO10 OC    DETC2N,DETC2N       ANYTHING IN THE COST2 FIELD?                 
         BZ    COS2CHKX             - NOPE, EXIT                                
         MVC   FVMSGNO,=AL2(FVICST)                                             
         LA    R1,DETC2N                                                        
         ST    R1,FVADDR           IF WE'RE HERE, IT CAME FROM TAM              
         CR    RE,RA               SET != CONDITION                             
         J     EXIT                SPECIAL TAM ERROR EXIT!                      
*                                                                               
COS2CHKX LA    R2,IOKEY            BWHREC DSECT ALREADY ON R2                   
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,BCAM                                                     
         MVC   BWHKMKT,BMKT                                                     
*                                                                               
         GOTO1 AIO,DIRHID+IO1      REREAD CAMPAIGN MKT HEADER POINTER           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BWHKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                  
         BNE   COS2XX                                                           
         GOTO1 AIO,FILGETU1                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COS2XX   MVC   IOKEY,LTMPKEY       RESTORE THE IOKEY                            
         CR    RE,RE               SET THE CONDITION                            
         J     EXITX                                                            
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                                    
***********************************************************************         
VALSLPAR NTR1  BASE=*,LABEL=*                                                   
         XC    BWHKEY,BWHKEY       SET UP HEADER KEY                            
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
         GOTO1 AVALMED,DUPMEDH     VALIDATE MEDIA                               
         BNE   VALPX                                                            
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         GOTO1 AVALBYR,DUPBYRH     VALIDATE BUYER                               
         BNE   VALPX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW                                                      
         BZ    VALP0                                                            
         GOTO1 AVALPWD                                                          
         BNE   VALPX                                                            
*                                                                               
VALP0    GOTO1 AVALCAM,DUPNUMH     VALIDATE CAMPAIGN NUMBER                     
         BNE   VALPX                                                            
         MVC   BWHKCAM,BCAM                                                     
*                                                                               
         GOTO1 AGETCLT,CMPCLTC     GET CLIENT                                   
         BE    VALP00                                                           
         LA    R1,DUPNUMH                                                       
         ST    R1,FVADDR                                                        
         B     VALPX                                                            
******                                                                          
VALP00   OI    DETC2KH+1,X'0C'     ZERO INTENSITY                               
         OI    DETC2NH+1,X'0C'       "      "                                   
         OI    DETC2NH+1,X'20'     PROTECT THE FIELD                            
         XC    DETC2N,DETC2N       JUST IN CASE IF ANYTHING IS HERE             
*                                                                               
         OI    DETC2KH+6,FVOXMT    TRANSMIT THESE 2 FIELDS                      
         OI    DETC2NH+6,FVOXMT                                                 
*                                                                               
         BRAS  RE,COS2CHK          CHECK IF WE NEED COS2 FIELD                  
         BNE   VALPX                                                            
******                                                                          
*                                                                               
         GOTO1 AGETPRD,CMPPRDN     GET PRODUCT                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AVALSTA,DUPSTAH     VALIDATE STATION                             
         BNE   VALPX                                                            
         MVC   BWHKMKT,BMKT        STATION'S MARKET                             
*                                                                               
         GOTO1 AVALDAY,DUPDAYH     VALIDATE DAYS                                
         BNE   VALPX                                                            
*                                                                               
         GOTO1 AVALTIM,DUPTIMH     VALIDATE TIMES                               
         BNE   VALPX                                                            
*                                                                               
         GOTO1 AVALDPL,DUPDPLH     VALIDATE DAYPART/LENGTH                      
         BNE   VALPX                                                            
         CLI   CMPDPOPT,C'M'       TEST SUBDPTS SCHEDULED UNDER MASTER          
         BNE   *+12                                                             
         CLI   DPTTYPE,C'S'        YES-TEST THIS IS A SUBDAYPART                
         BE    ESUB                    YES-ERROR                                
         CLI   BSLN,0              TEST FOR SPOT LENGTH                         
         BE    ENOSLN                                                           
*                                                                               
         MVI   LSUBDPT,0           SUB-DAYPART                                  
         MVI   FVMAXL,1                                                         
         GOTO1 AFVAL,DUPSDPH                                                    
         BH    VALPX                                                            
         BL    VALP1                                                            
         CLI   CMPDPOPT,C'S'       TEST SCHEDULE SUBDPTS SEPERATELY             
         BE    EIIF                YES-INVALID INPUT FIELD                      
         MVC   LSUBDPT,FVIFLD                                                   
         LA    RE,L'DPTSUBS                                                     
         LA    RF,DPTSUBS                                                       
         CLC   FVIFLD(1),0(RF)                                                  
         BE    VALP1                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,*-14                                                          
         MVC   FVMSGNO,=AL2(FVIDPT)                                             
         B     VALPX                                                            
*                                                                               
VALP1    GOTO1 AIO,DIRHID+IO1      READ CAMPAIGN MARKET HEADER POINTER          
         BNE   VALPX                                                            
         CLC   BWHKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                  
         BNE   ERNF                                                             
         MVC   LHDRKEY,IOKEY       SAVE THE HEADER KEY                          
         MVC   LHDRDA,IODA         SAVE THE HEADER DISK ADDRESS                 
*                                                                               
         GOTO1 AIO,FILGET1         GET THE HEADER RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1         DOES THIS STATION ALREADY EXIST?             
         LA    R4,BWHFSTEL                                                      
         SR    R0,R0                                                            
         SR    RE,RE                                                            
*                                                                               
VALP2    CLI   0(R4),0                                                          
         BE    ERNF                                                             
         CLI   0(R4),BWHELCDQ                                                   
         BNE   *+18                                                             
         USING BWHEL,R4                                                         
         IC    RE,BWHSEQ                                                        
         CLC   BWHSTA,QSTA                                                      
         BE    VALP4                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALP2                                                            
*                                                                               
VALP4    XC    APRECKEY,APRECKEY                                                
         MVI   BWDKTYP,BWDKTYPQ    SET UP FIRST PART OF DETAIL KEY              
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ     CAMPAIGN/MARKET SEQ NO                       
         MVI   BWDKELCD,1                                                       
         STC   RE,BWDKELST         STATION CODE                                 
         MVI   BWDKELPO,0                                                       
         MVC   BWDKELDY,BDAYS                                                   
         MVC   BWDKELTM,PTIMES                                                  
*                                                                               
         MVC   APRECKEY+20(8),QSTA SET STATION/DPT/LEN IN KEY SO THAT           
         MVC   APRECKEY+28(1),BDPT ROOT CAN DETECT KEY CHANGE                   
         MVC   APRECKEY+29(1),BSLN                                              
*                                                                               
         LA    R1,DUPL1H           SET APPARM FOR ROOT                          
         ST    R1,APPARM                                                        
         MVI   APPARM+4,0                                                       
         LA    RF,DUPL2H                                                        
         SR    RF,R1                                                            
         STH   RF,APPARM+6                                                      
*                                                                               
VALPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
DISPRECD NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**DREC**'                                                    
*                                                                               
         OC    BYRPW,BYRPW         TEST BUYER PASSWORD REQUIRED                 
         BZ    DISR0                                                            
         GOTO1 AVALPWD             YES-VALIDATE IT                              
         BNE   DISRX                                                            
*                                                                               
DISR0    OI    DETC2KH+1,X'0C'     ZERO INTENSITY                               
         OI    DETC2NH+1,X'0C'       "      "                                   
         OI    DETC2NH+1,X'20'     PROTECT THE FIELD                            
         XC    DETC2N,DETC2N       JUST IN CASE IF ANYTHING IS HERE             
*                                                                               
         OI    DETC2KH+6,FVOXMT    TRANSMIT THESE 2 FIELDS                      
         OI    DETC2NH+6,FVOXMT                                                 
*                                                                               
         BRAS  RE,COS2CHK          CHECK IF WE NEED COS2 FIELD                  
         BNE   DISRX                                                            
*                                                                               
DISR1    L     R3,AIOAREA2                                                      
         TWAXC DETCS1H                                                          
         MVC   DETSDP,BWDSUBDP     SUBDAYPART                                   
         OI    DETSDPH+6,FVOXMT                                                 
         XC    EBLOCK,EBLOCK       SET UP EBLOCK FOR EDITING                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBALIGN,C'L'                                                     
         LA    R1,DETCS1H          COST                                         
         LA    R8,BWDCOST1                                                      
         BAS   R9,DISRCST                                                       
         OC    BWDEFDT2,BWDEFDT2   EFFECTIVE DATE 2                             
         BZ    DISR1C                                                           
         LA    R4,DETEF2H                                                       
         LA    R8,BWDEFDT2                                                      
         BAS   R9,DISRDAT                                                       
         LA    R1,DETCS2H          COST 2                                       
         LA    R8,BWDCOST2                                                      
         BAS   R9,DISRCST                                                       
         OC    BWDEFDT3,BWDEFDT3   EFFECTIVE DATE 3                             
         BZ    DISR1C                                                           
         LA    R4,DETEF3H                                                       
         LA    R8,BWDEFDT3                                                      
         BAS   R9,DISRDAT                                                       
         LA    R1,DETCS3H          COST 3                                       
         LA    R8,BWDCOST3                                                      
         BAS   R9,DISRCST                                                       
*                                                                               
DISR1C   OC    BWDREP,BWDREP       ANYTHING HERE?                               
         BZ    DISR1E               - NOPE, SKIP                                
         GOTO1 VCOLY,APPARM,0,X'D9000ABC'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),APPARM,(C'U',BWDREP),DETREPN                                
         B     DISR1X                                                           
*                                                                               
DISR1E   OC    LESTREP,LESTREP                                                  
         BZ    DISR2                                                            
         MVC   DETREPN,LESTREP     LESTREP COULD BE EMPTY                       
DISR1X   OI    DETREPNH+6,FVOXMT                                                
*                                                                               
DISR2    MVC   DETPRG,BWDPROG      PROGRAMMING                                  
         OI    DETPRGH+6,FVOXMT                                                 
*                                                                               
         XC    DETDAT,DETDAT       DATES                                        
         OI    DETDATH+6,FVOXMT                                                 
         OC    BWDDATES,BWDDATES                                                
         BZ    DISR3                                                            
         GOTO1 VDATCON,APPARM,(2,BWDDATES),(8,DETDAT)                           
         MVI   DETDAT+8,C'-'                                                    
         GOTO1 (RF),(R1),(2,BWDDATES+2),(8,DETDAT+9)                            
*                                                                               
DISR3    MVC   DETCOD,BWDUCODE     USER CODE                                    
         OI    DETCODH+6,FVOXMT                                                 
*                                                                               
         LA    R4,BWDEL                                                         
*                                                                               
DISR4    CLI   0(R4),0                                                          
         BE    DISR40                                                           
         CLI   0(R4),BWIDECDQ      ID                                           
         BNE   DISR5                                                            
         USING BWIDEL,R4                                                        
         MVC   DETID,BWID                                                       
         OI    DETIDH+6,FVOXMT                                                  
         B     DISR30                                                           
*                                                                               
DISR5    CLI   0(R4),UPGELCDQ      UPGRADE                                      
         BNE   DISR6                                                            
         USING UPGEL,R4                                                         
         MVC   DETUPG,UPGINPUT                                                  
         OI    DETUPGH+6,FVOXMT                                                 
         B     DISR30                                                           
*                                                                               
DISR6    CLI   0(R4),COMELCDQ      COMMENT                                      
         BNE   DISR8                                                            
         USING COMEL,R4                                                         
         LA    RF,COMCOM-1                                                      
         ZIC   RE,COMELLN                                                       
         SH    RE,=Y(COMCOM-COMEL)                                              
         SR    R1,R1                                                            
         CLI   COMNUM,1            TEST OLD COMMENT ELEMENT                     
         BL    DISR7               (WITHOUT NUMBER)                             
         CLI   COMNUM,5                                                         
         BH    DISR7                                                            
         BCTR  RE,0                                                             
         LA    RF,1(RF)                                                         
         IC    R1,COMNUM           COMMENT NUMBER                               
         BCTR  R1,0                                                             
         MH    R1,=Y(DETCM2-DETCM1)                                             
*                                                                               
DISR7    LA    R1,DETCM1H(R1)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),0(RF)                                                    
         OI    6(R1),FVOXMT                                                     
         B     DISR30                                                           
*                                                                               
DISR8    CLI   0(R4),DMOELCDQ      DEMOS                                        
         BNE   DISR18                                                           
         USING DMOEL,R4                                                         
         ZIC   R9,DMOELLN                                                       
         LA    R9,DMOEL(R9)                                                     
         BCTR  R9,0                                                             
         LA    R8,L'DMODEMO                                                     
         LA    R1,DMODEMO                                                       
*                                                                               
DISR10   LA    R0,LNDEMOS                                                       
         LA    R2,ESTDEMS                                                       
         LA    R6,DETDM1H                                                       
*                                                                               
DISR12   CLC   1(2,R1),1(R2)                                                    
         BNE   DISR14                                                           
         CLI   0(R1),OVERELEM                                                   
         BE    DISR16                                                           
         CLC   0(1,R1),0(R2)                                                    
         BE    DISR16                                                           
DISR14   LA    R2,3(R2)                                                         
         BCT   R0,*+8                                                           
         B     EESD                ERROR - EST HDR DEMO CHANGED                 
         ZIC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         TM    FVATRB-FVIHDR(R6),FVAPROT                                        
         BO    *-10                                                             
         B     DISR12                                                           
*                                                                               
DISR16   ST    R1,APPARM                                                        
***  2 DECIMAL  ***                                                             
*        LR    R0,R1               SAVE OFF R1                                  
*        GOTO1 AADJPREC,APPARM,,   SEE IF WE NEED TO CONVERT                    
*        LR    R1,R0               PUT IT BACK IN R1                            
***  2 DECIMAL  ***                                                             
         MVC   APFULL,4(R1)                                                     
         GOTO1 ADISDEM                                                          
         BXLE  R1,R8,DISR10                                                     
         B     DISR30                                                           
*                                                                               
DISR18   CLI   0(R4),BTRELCDQ      BUY TRANSFER ELEMENT                         
         BNE   DISR22                                                           
         USING BTREL,R4                                                         
         OI    DETXFRH+6,FVOXMT                                                 
         LA    R1,DETXFR                                                        
         LA    RE,L'DETXFR-4(R1)                                                
*                                                                               
DISR19   DS    0H                  FORMAT THE BUY TRANSFER LINES                
         EDIT  (B1,BTRLINE),(3,(R1)),ALIGN=LEFT,WRK=APWORK,DUB=APDUB            
         AR    R1,R0                                                            
*                                                                               
DISR20   ZIC   R0,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         CLI   0(R4),BTRELCDQ                                                   
         BNE   DISR4                                                            
*                                                                               
         CR    R1,RE               TEST PAST END OF LINE                        
         BNH   DISR21                                                           
         LA    RF,DETXF2           YES                                          
         CR    RE,RF                                                            
         BL    *+14                                                             
         MVC   0(3,R1),=C',..'                                                  
         B     DISR20                                                           
*                                                                               
         OI    DETXF2H+6,FVOXMT                                                 
         LA    R1,DETXF2                                                        
         LA    RE,L'DETXF2-4(R1)                                                
         B     DISR19                                                           
*                                                                               
DISR21   MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         B     DISR19                                                           
*                                                                               
DISR22   CLI   0(R4),DTRELCDQ      DAILY BUY TRANSFER ELEMENT                   
         BNE   DISR28                                                           
         USING DTREL,R4                                                         
         OI    DETXFRH+6,FVOXMT                                                 
         LA    R1,DETXFR                                                        
         LA    RE,L'DETXFR-4(R1)                                                
*                                                                               
DISR24   DS    0H                  FORMAT THE BUY TRANSFER LINES                
         EDIT  (B1,DTRLINE),(3,(R1)),ALIGN=LEFT,WRK=APWORK,DUB=APDUB            
         AR    R1,R0                                                            
*                                                                               
DISR25   ZIC   R0,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         CLI   0(R4),DTRELCDQ                                                   
         BNE   DISR4                                                            
*                                                                               
         CR    R1,RE               TEST GO TO 2ND LINE                          
         BNH   DISR26                                                           
         LA    RF,DETXF2           YES                                          
         CR    RE,RF                                                            
         BL    *+14                                                             
         MVC   0(3,R1),=C',..'                                                  
         B     DISR25                                                           
*                                                                               
         OI    DETXF2H+6,FVOXMT                                                 
         LA    R1,DETXF2                                                        
         LA    RE,L'DETXF2-4(R1)                                                
         B     DISR24                                                           
*                                                                               
DISR26   MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         B     DISR24                                                           
*                                                                               
DISR28   CLI   0(R4),CS2ELCDQ      COST 2 OVERRIDE ELEMENT                      
         BNE   DISR30                                                           
         USING CS2EL,R4                                                         
         OI    DETC2NH+6,FVOXMT                                                 
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBALIGN,C'L'                                                     
         LA    R1,DETC2NH                                                       
         LA    R8,CS2COST2                                                      
         BAS   R9,DISRCST                                                       
*                                                                               
DISR30   ZIC   R0,1(R4)            NEXT ELEMENT                                 
         AR    R4,R0                                                            
         B     DISR4                                                            
*                                                                               
DISR40   B     DISRX                                                            
*                                                                               
DISRX    B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,**BW4X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALDATES                                                         
         B     VALRDAY                                                          
         B     CHPTSDOL                                                         
         B     GETDEMS                                                          
         B     DISDEM                                                           
         B     ADJCALC                                                          
         B     DISPKEY                                                          
*                                                                               
XIT      CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATES                                                                
* INPUT  : R1=A(DATES FIELD HEADER)                                             
* OUTPUT : APFULL=PACKED DATES START/END                                        
*          APHALF=INACTIVE WEEKS MASK                                           
*          APDUB =INACTIVE WEEKS MASK 2                                         
*                                                                               
* NOTE: APDUB WAS ALREADY CLOBBERED IN THIS ROUTINE SO NOW I'M USING TO         
*        STORE THE ADDITIONAL BITS FOR MASK2                                    
***********************************************************************         
         SPACE 1                                                                
VALDATES XC    APFULL,APFULL                                                    
         XC    APHALF,APHALF                                                    
         CLI   FVILEN-FVIHDR(R1),0 TEST DATES ABSENT                            
         BE    VALDX                                                            
         GOTO1 AVALDAT             VALIDATE DATES                               
         BNE   VALDX                                                            
         MVI   FVINDX,0                                                         
         OC    APWORK+6(6),APWORK+6  CHECK FOR SECOND DATE                      
         BNZ   *+10                                                             
         MVC   APWORK+6(6),APWORK  IF NONE, COPY FIRST DATE                     
*                                                                               
         CLC   APWORK(6),APWORK+6  CHECK START LE END                           
         BH    VALD8                                                            
         GOTO1 VDATCON,APPARM,(3,CMPST),APDUB  CHECK DATES WITHIN               
         CLC   APWORK(6),APDUB                 CAMPAIGN PERIOD                  
         BL    VALD7                                                            
         GOTO1 (RF),(R1),(3,CMPND),APDUB                                        
         CLC   APWORK+6(6),APDUB                                                
         BH    VALD7                                                            
         GOTO1 VDATCON,APPARM,APWORK,(2,APFULL)                                 
         GOTO1 (RF),(R1),APWORK+6,(2,APFULL+2)                                  
*                                                                               
         XC    APDUB,APDUB         CAN USE APDUB(7) NOW FOR ADDITIONAL          
         L     R4,ATWA                                                          
         AHI   R4,CMPDATSP-TWAD                                                 
*                                                                               
         LA    R0,53               53 DAYS/WEEKS IS ABSOLUTE MAX                
         SR    RE,RE                                                            
         LA    R1,1                                                             
         SLL   R1,31                                                            
         MVI   APBYTE,0                                                         
*                                                                               
VALD2    CLI   APBYTE,2            TEST BEYOND DATES                            
         BE    VALD4                                                            
         CLI   0(R4),X'FF'         TEST END OF FLIGHT WEEKS                     
         BNE   *+12                                                             
         MVI   APBYTE,2            YES-FORCE INACTIVE                           
         B     VALD4                                                            
         CLI   APBYTE,0            TEST BEFORE DATES                            
         BNE   VALD3                                                            
         CLC   APFULL(2),2(R4)     YES-TEST START DATE AFTER THIS WEEK          
         BH    VALD4                   YES-STILL BEFORE DATES                   
         MVI   APBYTE,1                NO-DURING DATES                          
*                                                                               
VALD3    CLC   APFULL+2(2),0(R4)    DURING DATES-TEST END DATE BEFORE           
         BNL   VALD5                             THIS WEEK                      
         MVI   APBYTE,2             YES-BEYOND DATES NOW                        
*                                                                               
VALD4    MVI   APDUB+7,X'80'       INACTIVE WEEK                                
         B     *+8                                                              
VALD5    MVI   APDUB+7,X'00'       ACTIVE WEEK                                  
*                                                                               
         LM    RE,RF,APDUB                                                      
         SLDL  RE,1                                                             
         STM   RE,RF,APDUB                                                      
         LA    R4,4(R4)                                                         
         BCT   R0,VALD2                                                         
*                                                                               
         LM    RE,RF,APDUB                                                      
         SLDL  RE,3                   MAXWKS=53, SO SHIFT FOR 56 BITS           
         STCM  RE,12,APHALF                                                     
         XC    APDUB,APDUB                                                      
         STCM  RE,3,APDUB                                                       
         STCM  RF,14,APDUB+2                                                    
         B     VALDX                                                            
*                                                                               
VALD7    MVC   FVMSGNO,=AL2(FVIDTCAM) DATES NOT WITHIN CAMPAIGN PERIOD          
         B     VALDX                                                            
*                                                                               
VALD8    MVC   FVMSGNO,=AL2(FVIDAT)   INVALID DATE FORMAT                       
         B     VALDX                                                            
*                                                                               
VALD9    MVC   FVMSGNO,=AL2(FVNOEDT)  END DATE MISSING                          
*                                                                               
VALDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE THE DAY OF AN EFFECTIVE DATE                    *         
* INPUT  : R8 = A(PACKED DATE)                                        *         
* OUTPUT : CC EQ OK                                                   *         
*             NE ERROR                                                *         
*          APPARM(1) = DAY OF INPUT DATE                              *         
*          APWORK(6) = MONDAY OF WEEK OF INPUT DATE                   *         
***********************************************************************         
         SPACE 1                                                                
VALRDAY  DS    0H                                                               
         GOTO1 VDATCON,APPARM,(3,(R8)),(0,APWORK)                               
         GOTO1 VGETDAY,APPARM,APWORK,APWORK+6                                   
         CLC   APWORK+6(3),BLANKS                                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APBYTE,APPARM                                                    
         ZIC   RE,APBYTE                                                        
         CLI   ESTOWSDY,1          TEST OUT-OF-WEEK RORATORS                    
         BNH   VALRD2                                                           
         ZIC   RF,ESTOWSDY                                                      
         SR    RE,RF                                                            
         BZ    VALRDEQ                                                          
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         LNR   RE,RE                                                            
         B     VALRD4                                                           
*                                                                               
VALRD2   BCTR  RE,0                                                             
         LNR   RE,RE                                                            
         BZ    VALRDEQ             MONDAY IS OK                                 
*                                                                               
VALRD4   ST    RE,APPARM+8                                                      
         GOTO1 VADDAY,APPARM,APWORK,APWORK+6                                    
         MVC   APWORK(6),APWORK+6                                               
         ZIC   RE,APBYTE                                                        
         LA    RF,8                                                             
         SR    RF,RE                                                            
         LA    RE,1                                                             
         B     *+8                                                              
         SLL   RE,1                                                             
         BCT   RF,*-4                                                           
         IC    RF,BDAYS                                                         
         NR    RE,RF                                                            
         BZ    VALRDNE                                                          
         SLL   RE,1                                                             
         CLI   ESTOWSDY,1          TEST OUT-OF-WEEK-ROTATOR                     
         BNH   VALRD6                                                           
         ZIC   R0,ESTOWSDY         YES-ELIMINATE DAYS PREVIOUS                  
         LR    R1,R0                   TO WEEK START DAY                        
         SLL   RF,24                                                            
         SLL   RF,1                                                             
         BCT   R1,*-4                                                           
         SRL   RF,1                                                             
         BCT   R0,*-4                                                           
         SRL   RF,24                                                            
*                                                                               
VALRD6   CR    RE,RF                                                            
         BNH   VALRDNE                                                          
*                                                                               
VALRDEQ  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALRDX                                                           
VALRDNE  MVC   FVMSGNO,=AL2(FVIEFDT)                                            
VALRDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE THE TOTAL ACTUAL POINTS AND DOLLARS,              *         
* FOR ACTION=DELETE IN LIST/SELECT MODE                               *         
***********************************************************************         
         SPACE 1                                                                
CHPTSDOL L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         USING SAVAREA,R6                                                       
         L     R6,LASAVE                                                        
         NI    LINDS,255-LPKGSLV                                                
         CLI   BWDKELPO,0          TEST PACKAGE SLAVE                           
         BE    *+16                                                             
         CLI   BWDKELSQ,0                                                       
         BE    *+8                                                              
         OI    LINDS,LPKGSLV       YES                                          
         XC    APDUB,APDUB                                                      
         SR    R0,R0                                                            
         LA    R4,BWDEL            SCAN ELEMENTS                                
*                                                                               
CHPD1    CLI   0(R4),0                                                          
         BE    CHPD2                                                            
         CLI   0(R4),DMOELCDQ                                                   
         BNE   *+8                                                              
         ST    R4,APDUB            APDUB(4)=A(DEMO ELEMENT)                     
         CLI   0(R4),SPWELCDQ                                                   
         BNE   *+8                                                              
         ST    R4,APDUB+4          APDUB+4(4)=A(SPTS/WEEK ELEMENT)              
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CHPD1                                                            
*                                                                               
CHPD2    OC    APDUB+4(4),APDUB+4  TEST ANY SCHEDULING                          
         BZ    CHPDX               NO-EXIT                                      
         ICM   R4,15,APDUB         TEST DEMO ELEMENT                            
         BZ    CHPD10              NO                                           
         USING DMOEL,R4                                                         
         LA    R1,DMODEMO                                                       
         OC    INORTG,INORTG       TEST FOR OVERRIDE TARGET DEMO                
         BZ    CHPD4               NO - THEN TARGET = PRIMARY                   
         LA    RE,L'DMODEMO        YES - FIND TARGET                            
         ZIC   RF,DMOELLN                                                       
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         CLC   1(2,R1),INORTG+1                                                 
         BE    CHPD4                                                            
         BXLE  R1,RE,*-10                                                       
         B     CHPD10                                                           
         DROP  R4                                                               
*                                                                               
CHPD4    MVC   LDEMOLD,4(R1)       TARGET RATING                                
         NI    LDEMOLD,FF-DMODEMOV                                              
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BZ    CHPD4E                                                           
         MVC   APWORK+64(4),LDEMOLD                                             
         NI    APWORK+64,FF-DMODEM2D                                            
         SR    R0,R0                                                            
         L     R1,APWORK+64                                                     
         D     R0,=F'10'           MAKE IT ONE DECIMAL VALUE                    
         ST    R1,APWORK+64                                                     
***  2 DECIMAL                                                                  
CHPD4E   L     R4,APDUB+4          R4=A(SPOTS/WEEK ELEMENT)                     
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         LA    R4,SPWPERWK-SPWEL(R4)                                            
         LA    R8,SVACPTS          RE-CALCULATE WEEKLY ACTUAL POINTS            
         L     R5,ATWA                                                          
         AHI   R5,SVPKAPTS-TWAD                                                 
         ZIC   R9,CMPNWKS                                                       
*                                                                               
CHPD6    ZIC   R1,0(R4)                                                         
         SR    R0,R0                                                            
***      M     R0,LDEMOLD          SPOTS X RATING                               
         M     R0,APWORK+64        SPOTS X RATING                               
         L     R0,0(R8)                                                         
         SR    R0,R1                                                            
         ST    R0,0(R8)                                                         
         TM    LINDS,LPKGSLV                                                    
         BZ    *+14                                                             
         L     R0,0(R5)                                                         
         SR    R0,R1                                                            
         ST    R0,0(R5)                                                         
         LA    R4,1(R4)                                                         
         CR    R4,RF                                                            
         BNL   CHPD10                                                           
         LA    R8,4(R8)                                                         
         LA    R5,4(R5)                                                         
         BCT   R9,CHPD6                                                         
*                                                                               
*                                  RE-CALCULATE ACTUAL DOLLARS                  
CHPD10   XC    APFULL,APFULL                                                    
         OC    BWDEFDT2,BWDEFDT2                                                
         BZ    CHPD11                                                           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(2,APFULL)                           
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    CHPD11                                                           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT3),(2,APFULL+2)                         
*                                                                               
CHPD11   L     R4,APDUB+4          R4=A(SPOTS/WEEK ELEMENT)                     
         ZIC   R8,1(R4)                                                         
         AR    R8,R4                                                            
         L     R2,ATWA                                                          
         AHI   R2,CMPDATSP-TWAD                                                 
         LA    R4,SPWPERWK-SPWEL(R4)                                            
         ZIC   R9,CMPNWKS                                                       
         ICM   RE,15,BWDCOST1      RE=COST                                      
         L     RF,SVACDOL                                                       
*                                                                               
         L     R5,ATWA                                                          
         AHI   R5,SVPKADOL-TWAD                                                 
         L     R5,0(R5)                                                         
*                                                                               
CHPD12   OC    APFULL+2(2),APFULL+2                                             
         BZ    CHPD14                                                           
         CLC   APFULL+2(2),2(R2)                                                
         BH    CHPD14                                                           
         ICM   RE,15,BWDCOST3                                                   
         B     CHPD16                                                           
*                                                                               
CHPD14   OC    APFULL(2),APFULL                                                 
         BZ    CHPD16                                                           
         CLC   APFULL(2),2(R2)                                                  
         BH    CHPD16                                                           
         ICM   RE,15,BWDCOST2                                                   
*                                                                               
CHPD16   SR    R0,R0                                                            
         ZIC   R1,0(R4)                                                         
         MR    R0,RE                                                            
         SR    RF,R1                                                            
         TM    LINDS,LPKGSLV                                                    
         BZ    *+6                                                              
         SR    R5,R1                                                            
         LA    R2,4(R2)            NEXT WEEK                                    
         LA    R4,1(R4)                                                         
         CR    R4,R8                                                            
         BNL   *+8                                                              
         BCT   R9,CHPD12                                                        
         ST    RF,SVACDOL                                                       
         TM    LINDS,LPKGSLV                                                    
         BZ    CHPD18                                                           
         L     RF,ATWA                                                          
         AHI   RF,SVPKADOL-TWAD                                                 
         ST    R5,0(RF)                                                         
*                                                                               
CHPD18   L     R5,ATWA                                                          
         OI    TWAFLAG,TWAFMTPD    INDICATE FORMAT ACTUAL PTS/DOL               
*                                                                               
CHPDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET DEMO NAMES VIA CALL TO DEMOCON                       *         
* INPUT : R8 = A(LIST OF 3-BYTE DEMO CODES)                           *         
***********************************************************************         
         SPACE 1                                                                
GETDEMS  DS    0H                                                               
         XC    DBLOCK,DBLOCK       GET DEMO NAMES                               
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         MVI   APBYTE,LNDEMOS                                                   
         ICM   R8,8,APBYTE                                                      
         ST    R8,APPARM                                                        
         LA    R0,ESTUSRNM                                                      
         XC    LDNAMES(7*LNDEMOS),LDNAMES                                       
         GOTO1 VDEMOCON,APPARM,,(2,LDNAMES),(C'S',DBLOCK),(R0)                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY DEMO VALUE ROUTINE                                          *         
* R6 = A(FIELD HEADER)                                                *         
* APFULL = 4-BYTE DEMO VALUE                                          *         
***********************************************************************         
         SPACE 1                                                                
DISDEM   DS    0H                                                               
         MVI   EBDECS,1                                                         
***  2 DECIMAL  ***                                                             
         TM    APFULL,DMODEM2D     WE NEED 2 DECIMAL?                           
         BNO   *+8                                                              
         MVI   EBDECS,2                                                         
***  2 DECIMAL  ***                                                             
         MVI   EBSCIN,0                                                         
         MVI   EBFLOAT,0                                                        
         MVI   EBALIGN,C'L'                                                     
         MVI   EBOPT,0                                                          
         LA    R1,L'FVIHDR(R6)                                                  
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'DETDM1                                                  
         LA    RF,APFULL                                                        
***  2 DECIMAL  ***                                                             
         NI    APFULL,FF-DMODEM2D   TAKE OFF THE BIT FOR CORRECT AMOUNT         
***  2 DECIMAL  ***                                                             
         TM    APFULL,DMODEMOV                                                  
         BZ    DISDEM2                                                          
         MVI   EBFLOAT,C'*'        DEMO OVERRIDE GETS A *                       
         NI    APFULL,FF-DMODEMOV                                               
         OC    APFULL,APFULL       TEST DEMO = 0                                
         BNZ   DISDEM2                                                          
         MVC   0(6,R1),=C'  *0.0'  YES-EDIT MYSELF                              
***  2 DECIMAL  ***                                                             
         CLI   EBDECS,2            ARE WE DOING 2 DECIMALS?                     
         BNE   *+10                 - NOPE                                      
         MVC   0(6,R1),=C' *0.00'  YES-EDIT MYSELF                              
***  2 DECIMAL  ***                                                             
         B     DISDEM6                                                          
*                                                                               
DISDEM2  ST    RF,EBAIN                                                         
***  2 DECIMAL  ***                                                             
         CLI   EBDECS,2            ARE WE DOING 2 DECIMALS?                     
         BE    DISDEM2E             - YUP WE ARE                                
***  2 DECIMAL  ***                                                             
         CLC   APFULL,=F'10000'                                                 
         BL    DISDEM4                                                          
         CLI   EBFLOAT,C'*'                                                     
         BE    *+14                                                             
         CLC   APFULL,=F'100000'                                                
         BL    DISDEM4                                                          
         B     DISDEM2G                                                         
*                                                                               
***  2 DECIMAL  ***                                                             
DISDEM2E CLC   APFULL,=F'100000'   10X BIGGER THAN NORMAL                       
         BL    DISDEM4             SINCE IT'S 2 DECIMALS                        
         CLI   EBFLOAT,C'*'                                                     
         BE    *+14                                                             
         CLC   APFULL,=F'1000000'                                               
         BL    DISDEM4                                                          
         B     DISDEM2G                                                         
***  2 DECIMAL  ***                                                             
*                                                                               
***  2 DECIMAL  ***                                                             
DISDEM2G XR    RE,RE               USED TO BE JUST NO DECIMALS                  
         IC    RE,EBDECS                                                        
         BCTR  RE,0                DECREMENT WHETHER 1 OR 2 DECIMALS            
         STC   RE,EBDECS                                                        
***  2 DECIMAL  ***                                                             
***      MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
*                                                                               
DISDEM4  GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISDEM6  OI    6(R6),FVOXMT                                                     
         OI    FVIIND-FVIHDR(R6),FVIVAL   TURN ON PREV VALIDATED BIT            
*                                                                               
DISDEMX  B     XIT                                                              
ADJCALC  DS    0H                                                               
***  2 DECIMAL                                                                  
         XC    APWORK+64(16),APWORK+64                                          
         MVC   APWORK+64(4),LDEMOLD   OLD TARGET RATING                         
         MVC   APWORK+68(4),LDEMNEW   NEW TARGET RATING                         
*                                                                               
         NI    APWORK+64,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         NI    APWORK+68,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BZ    AD00K                - NOPE WE'RE NOT                            
***                                                                             
         TM    LDEMOLD,X'40'       2 DECIMAL?                                   
         BNZ   AD00E                - YUP, NOTHING TO DO                        
         L     R1,APWORK+64                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+64                                                     
AD00E    TM    LDEMNEW,X'40'       2 DECIMAL?                                   
         BNZ   AD00K                                                            
***  IT SEEMS LIKE LDEMNEW IS IN 2 DECIMAL WITHOUT X'40' BIT ON                 
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNZ   AD00K                - YUP                                       
***  IT SEEMS LIKE LDEMNEW IS IN 2 DECIMAL WITHOUT X'40' BIT ON                 
         L     R1,APWORK+68                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+68                                                     
*****  BOTH APFULL AND LNEWRTG SHOULD BE IN 2 DECIMAL MODE BY NOW               
AD00K    DS    0H                                                               
***  2 DECIMAL                                                                  
         L     RF,=F'1000'                                                      
         OC    APWORK+64(4),APWORK+64   TEST OLD DEMO VALUE = 0                 
         BNZ   AD1                                                              
         OC    APWORK+68(4),APWORK+68    YES-TEST NEW VALUE = 0                 
         BZ    AD2                 YES-ADJUST 100%                              
         B     AD99                NO-ERROR                                     
AD1      SR    RE,RE                                                            
         L     RF,APWORK+68                                                     
         M     RE,=F'2000'                                                      
         D     RE,APWORK+64                                                     
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
*                                                                               
AD2      ST    RF,LDEMADJ                                                       
         CR    RB,RB               SET EQUAL CONDITION                          
         B     ADX                                                              
*                                                                               
AD99     MVC   FVMSGNO,=AL2(FVIDADJ)                                            
         XR    R1,R1                                                            
         CR    RB,R1               SET != CONDITION                             
*                                                                               
ADX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
         SPACE 1                                                                
DISPKEY  MVC   IOKEY,APRECKEY      GET THE RECORD                               
         GOTO1 AMIN,MINRD2                                                      
         BNE   DISKX                                                            
         L     R3,AIOAREA2                                                      
         GOTO1 AGETMED,BWDKAGMD                                                 
         BNE   *+10                                                             
         MVC   DETMED,QMED         MEDIA                                        
         LA    R1,BWDKBYR                                                       
         ICM   R1,8,=X'01'                                                      
         GOTO1 AGETBYR                                                          
         MVC   DETBYR,QBYR         BUYER                                        
         GOTO1 AGETCM,BWDKSEQ      CAMPAIGN/MARKET                              
         MVC   DETNUM,QCAM                                                      
         MVC   DETSTA(5),BWDSTA    STATION                                      
         CLI   DETSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   DETSTA+4,C' '                                                    
         CLI   DETSTA,C'0'                                                      
         BL    *+14                                                             
         MVI   DETSTA+4,C'/'                                                    
         MVC   DETSTA+5(3),BWDSTA+5                                             
*                                                                               
         XC    DETDAY,DETDAY       DAYS                                         
         CLI   BWDDAYS,0                                                        
         BE    DISK2                                                            
         GOTO1 AGETDAY,BWDDAYS                                                  
         MVC   DETDAY,QDAYS                                                     
*                                                                               
DISK2    XC    DETTIM,DETTIM       TIMES                                        
         OC    BWDTIMES,BWDTIMES                                                
         BZ    DISK4                                                            
         GOTO1 AGETTIM,BWDTIMES                                                 
         MVC   DETTIM,QTIMES                                                    
*                                                                               
DISK4    GOTO1 AGETDPT,BWDDPT      DAYPART/LENGTH                               
         MVC   DETDPL(1),BDPT                                                   
         LA    RF,DETDPL+1                                                      
         CLI   BDPT,C'0'           TEST NUMERIC DAYPART                         
         BL    *+12                                                             
         MVI   0(RF),C'/'          YES-SEPARATE DPT AND SLN WITH /              
         LA    RF,1(RF)                                                         
         ZIC   RE,BWDSLN                                                        
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APFULL(3),APDUB                                                  
         LA    RE,APFULL                                                        
         CLI   0(RE),C'0'                                                       
         BNE   *+12                                                             
         LA    RE,APFULL+1                                                      
         MVI   2(RE),C' '                                                       
         MVC   0(3,RF),0(RE)                                                    
*                                                                               
DISK6    MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
DISKX    XIT1                                                                   
         EJECT                                                                  
BLANKS   DC    32C' '                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE SETS THE LENGTH FOR FILLED FIELDS AFTER GOING THROUGH       *         
* DISPRECD.  SPECIFICLY DESIGNED FOR C'=' FROM LIST (DUP WORK RECORD) *         
*                                   MHC   06/12/03                    *         
***********************************************************************         
SETLNTH  NTR1  BASE=*                                                           
         LA    R2,DETCS1H          START OFF WITH COST1 FIELD                   
*                                                                               
SETLN00  CLI   0(R2),0             IS THE LENGTH TOTAL 0?                       
         BE    SETLNX               - YA, GET OUTTA HERE                        
         TM    1(R2),X'20'         IS IT A PROTECTED FIELD?                     
         BO    SETLNNXT             - YES, GO TO THE NEXT FIELD                 
         XR    R0,R0                                                            
         IC    R0,0(R2)            R0 NOW HAS THE TOTAL LENGTH                  
         TM    1(R2),X'02'         WE HAVE EXTENDED FIELD HEADER?               
         BNO   *+8                                                              
         SHI   R0,8                TAKE OUT LENGTH OF EXTENDED FIELD            
         SHI   R0,8                TAKE OUT HEADER LENGTH                       
         LR    R3,R0               R3 HOLDS THE REAL LENGTH                     
         LA    R6,8(R2)            START OF FIELD                               
         SHI   R0,1                TO POINT RE TO THE LAST BYTE                 
         AR    R6,R0                                                            
*                                                                               
SETLN20  CLI   0(R6),C' '          IS IT A SPACE OR LOWER?                      
         BH    SETLN50              - NOPE, GET OUTTA HERE                      
         BCTR  R6,0                MOVE TO CHECK FOR NON-SPACE/NULLS            
         BCT   R3,SETLN20                                                       
*                                                                               
SETLN50  STC   R3,5(R2)                                                         
*                                                                               
SETLNNXT XR    R0,R0               BUMP R2 TO POINT TO NEXT FIELD               
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     SETLN00                                                          
*                                                                               
SETLNX   J     EXITX                                                            
***********************************************************************         
* ROUTINE CHECKS IF THERE ARE ANY SPOTS WITHIN INACTIVE WEEKS         *         
* WHEN THE DATE FIELD IS FILLED IN, GIVES ERROR IF A SPOT IS FOUND    *         
*                                   MHC   04/28/03                    *         
***********************************************************************         
         SPACE 1                                                                
INAWKCHK NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOAREA2         NEW DETAIL                                   
         USING BWDRECD,R3                                                       
*                                                                               
         XC    APDUB,APDUB         GET THE INACTIVE WEEKS MASK                  
         MVC   APDUB(2),BWDWKS                                                  
         MVC   APDUB+2(5),BWDWKS2                                               
         OC    APDUB,APDUB         ANY BITS ON?                                 
         BZ    INAWKX              NONE, NO PROBLEM (WE ALREADY SET CC)         
         LM    R8,R9,APDUB                                                      
*                                                                               
         L     R3,AIOAREA3         OLD DETAIL                                   
         LA    R3,BWDFSTEL                                                      
*                                                                               
INAWK10  CLI   0(R3),0             CAN'T FIND X'03'???                          
         BE    INAWKNS              - NO SPOTS FOUND AT ALL, NO PROB            
         CLI   0(R3),SPWELCDQ      X'03' SPOTS PER WEEK ELEMENT                 
         BE    INAWK20              - FOUND IT, WE'RE GOOD                      
         XR    R0,R0                - NOPE, BUMP                                
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     INAWK10                                                          
*                                                                               
         USING SPWEL,R3                                                         
INAWK20  XR    RE,RE                                                            
         ICM   RE,1,SPWELLN        COUNTER, GET DYNAMIC LENGTH                  
         SHI   RE,4                (NOT ALWAYS 14 WEEKS)                        
         LA    R3,SPWPERWK         POINT R3 AT SPOTS PER FIRST WEEK             
*                                                                               
INAWK30  STCM  R8,8,APBYTE                                                      
         TM    APBYTE,X'80'        THIS WEEK IS MASKED?                         
         BZ    INAWK40                                                          
         CLI   0(R3),0             YES, ARE THERE ANY SPOTS?                    
         BNE   INAWKX                   YES, CONDITION SET TO !=                
INAWK40  LA    R3,1(R3)                                                         
         SLDL  R8,1                                                             
         BCT   RE,INAWK30                                                       
*                                                                               
INAWKNS  CR    RE,RE               CONDITION SET TO =                           
*                                                                               
INAWKX   XIT1                                                                   
         DROP  R3                                                               
***********************************************************************         
* ROUTINE CHECKS 06 ELEM IF THERE IS ANY DATE                         *         
* IF IT FINDS NO DATE OR NO 06 ELEM IT PUTS/ADDS 06 ELEM W/ CURR DATE *         
***********************************************************************         
         SPACE 1                                                                
UPD06EL  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOAREA1                                                      
         LA    R6,BWHFSTEL         POINT TO 1ST ELEM                            
*                                  LOOK FOR X'06' ELEM                          
UPD06A   CLI   0(R6),0                                                          
         BE    NEW06               DIDN'T FIND, ADD W/ TODAY'S DATE             
         CLI   0(R6),INFELCDQ                                                   
         BE    OLD06               FOUND OLD, CK DATE, ADD TODAYS' IF 0         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               CK NXT ELEM                                  
         B     UPD06A                                                           
*                                                                               
NEW06    DS    0H                                                               
         XC    APELEM,APELEM       ADD ELEM W/ TODAYS DATE                      
         LA    R4,APELEM                                                        
         USING INFELD,R4                                                        
         MVI   INFELCD,INFELCDQ                                                 
         MVI   INFELLN,INFELLNQ                                                 
         GOTO1 VDATCON,APPARM,(5,0),(3,INFADDED)                                
         MVC   SVDAT,INFADDED      SAVE FOR CHKAUTH                             
         GOTO1 AADDELS,BWHRECD     ADD ELEM                                     
         B     UPDX                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
OLD06    DS    0H                                                               
         USING INFELD,R6                                                        
         MVC   SVDAT,INFADDED      SAVE FOR CHKAUTH                             
         OC    INFADDED,INFADDED   ANYTHING IN IT?                              
         BNZ   UPDX                YES, LEAVE ALONE                             
         GOTO1 VDATCON,APPARM,(5,0),(3,INFADDED)   NO, PUT TODAY'S DATE         
         MVC   SVDAT,INFADDED      SAVE FOR CHKAUTH                             
*                                                                               
UPDX     XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL SPAUTH TO UPDATE SUPERDESK AUTHORIZATION RECORDS               *         
***********************************************************************         
         SPACE 1                                                                
CHKAUTH  NTR1  BASE=*,LABEL=*                                                   
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         TM    XTRAFLG1,XAUTH      ALREADY WENT THROUGH CODE?                   
         BNZ   CHKAUTHZ            YES, GET OUT                                 
         TM    XTRAFLG1,XSDE       SDESK AUTH OPEN FOR PRD OPTION?              
         BO    CKAUTH10            YES                                          
         DROP  RF                                                               
*                                                                               
         L     RF,ATWA                                                          
         AHI   RF,SVINDS-TWAD                                                   
         TM    0(RF),SVIEAUTH      SDESK AUTH OPEN FOR CAMPAIGN PRD?            
         BNO   CHKAUTHZ            NO, GET OUT                                  
*                                                                               
CKAUTH10 MVC   APHALF+0(1),BPRD                                                 
         MVC   APHALF+1(1),CMPPRD2                                              
         BRAS  RE,SETPRD                                                        
         CLI   APHALF+1,0          IS THERE A PIGGYBACK PRODUCT?                
         BE    CKAUTH50            NO                                           
         TM    CLTIND2,CLTPONLY    POL ONLY?                                    
         BZ    CKAUTH20                                                         
         CLI   CLTPROF+0,C'0'      TRUE POL?                                    
         BE    CKAUTH50            IF TRUE POL DON'T READ EST RECORD            
*                                                                               
CKAUTH20 MVC   APBYTE,APHALF+1                                                  
         BRAS  RE,GETPIG           GET 3 BYTE PRODUCT MNEMONIC                  
*                                                                               
         USING ESTHDRD,R2                                                       
         LA    R2,IOKEY            READ ESTIMATE RECORD FOR PIGGYBACK           
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,APFULL      PIGGYBACK PRODUCT                            
         MVC   EKEYEST,BEST                                                     
*                                                                               
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   *+14                                                             
         CLC   EKEY,IOKEYSAV                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 AIO,FILGET1         GET ESTIMATE HDR                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         TM    EFLAG1,EF1SDE       PIGGYBACK AUTHORIZATION OPEN?                
         BZ    CHKAUTHZ            NO, GET OUT                                  
         DROP  R2                                                               
*                                                                               
* NEED TO ALPHABETIZE PRD1 AND PRD2                                             
         CLI   INOPRD,0            PRD CODE HERE?                               
         BE    CKAUTH30            NO, CHECK CAMPAIGN                           
*                                                                               
*                                                                               
         CLC   POLPRD1,POLPRD2     ALREADY ALPHABETIZED?                        
         BH    CKAUTH40            NO                                           
         B     CKAUTH50            YES                                          
*                                                                               
CKAUTH30 CLC   CMPPRDC,APFULL      ALREADY ALPHABETIZED?                        
         BNH   CKAUTH50            YES                                          
*                                                                               
CKAUTH40 MVC   APBYTE,APHALF       SWAP THEM                                    
         MVC   APHALF(1),APHALF+1                                               
         MVC   APHALF+1(1),APBYTE                                               
*                                                                               
         PUSH  USING                                                            
CKAUTH50 XC    APWORK,APWORK       CALL SPAUTH                                  
*                                                                               
         USING SPAUTHD,APWORK                                                   
         MVC   SPACOM,ACOM                                                      
         L     RF,AIOAREA1                                                      
         ST    RF,SPAIO                                                         
         MVC   SPAKAM,BAGYMD                                                    
         MVC   SPAKCLT,BCLT                                                     
         MVC   SPAKPRD(2),APHALF                                                
         MVC   SPAKEST,BEST                                                     
         MVC   SPAKMKT,BMKT                                                     
         MVI   SPAUPDT,SPAUPDWK    UPDATE NWS WORK REC ADDED DATE               
         GOTO1 VDATCON,APPARM,(3,SVDAT),(2,SPAWRKDT)                            
         GOTO1 VDATCON,APPARM,(3,CMPST),(2,SPASDTE)                             
         GOTO1 (RF),(R1),(3,CMPND),(2,SPAEDTE)                                  
         GOTO1 VSPAUTH,APWORK                                                   
         CLI   SPAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
CHKAUTHX L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         OI    XTRAFLG1,XAUTH      TURN FLAG ON                                 
         DROP  RF                                                               
CHKAUTHZ XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*******************************************************************             
*                                                                               
SETPRD   DS    0H                                                               
         TM    LIND,LPOL           TEST POL BUY                                 
         JZ    SETPRDX             NO-THERE'S NO BDMASPRD                       
         XC    APHALF,APHALF                                                    
         MVC   APHALF(1),BPRD      CAMPAIGN PRODUCT                             
         CLI   CMPPRD1,0           TEST FOR CAMPAIGN PIGGYBACKS                 
         JE    SETPRD2                                                          
         MVC   APHALF(1),CMPPRD1   YES                                          
         MVC   APHALF+1(1),CMPPRD2                                              
         J     SETPRD4                                                          
*                                                                               
SETPRD2  CLI   BPRD,FF             TEST CAMPAIGN PRODUCT = POL                  
         JNE   SETPRD4                                                          
         CLI   INOPRD,0            YES-SET MASTER PRODUCT FROM THE              
         JE    SETPRDX                 PRD OPTION IF IT'S GIVEN                 
         MVC   APHALF,INOPRD                                                    
*                                                                               
SETPRD4  MVC   LMASPRD,APHALF                                                   
*                                                                               
SETPRDX  BR    RE                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
GETPIG   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R4                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         GOTO1 AIO,FILRD1          READ CLIENT RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIOAREA1                                                      
         LA    R1,CLIST            LOOK FOR PASSIVE PRODUCT CODE                
*                                                                               
GETPIG10 CLI   0(R1),C'A'                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   APBYTE,3(R1)                                                     
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     GETPIG10                                                         
         MVC   APFULL,0(R1)         FOUND - PRODUCT MNEMONIC                    
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
EXTRAWKD DSECT                                                                  
XTRAFLG1 DS    XL1                 EXTRA FLAGS                                  
XF1NXESL EQU   X'80'                - SOME LINES NOT XFR CAUSE OF ESLN          
XAUTH    EQU   X'40'               IF ON, ALREADY WENT THROUGH CODE             
XSDE     EQU   X'20'               IF ON, SDESK AUTH OPEN FOR PRD OPTN          
*                                                                               
LSTDARKY DS    XL(L'DOKEY)         LAST DARE KEY (BY CLIENT)                    
LIUN     DS    2000X               TEMP BLOCK AS USED AS SPUPAREC               
EXTRAWKL EQU   *-EXTRAWKD                                                       
         EJECT                                                                  
STABYD   DSECT                     DSECT FOR STATION/BUYLINE TABLE              
SBSTA    DS    CL8                 STATION                                      
SBLNLO   DS    PL2                 LOW BUY LINE                                 
SBLNHI   DS    PL2                 HIGH BUY LINE                                
STABYL   EQU   *-STABYD                                                         
*                                                                               
LOCALD   DSECT                                                                  
AXTRA    DS    0F               ** EXTENTION ROUTINE ADDRESSES **               
AVALDATE DS    A                                                                
AVALRDAY DS    A                                                                
ACHPTDOL DS    A                                                                
AGETDEMS DS    A                                                                
ADISDEM  DS    A                                                                
AADJCALC DS    A                                                                
ADISPKEY DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
LASAVE   DS    A                                                                
LAR4SAVE DS    A                   SAVE REGISTER 4                              
LHDRDA   DS    F                                                                
LDEMVALS DS    (LNDEMOS)XL4                                                     
LDEMOLD  DS    F                                                                
LDEMNEW  DS    F                                                                
LDEMADJ  DS    F                                                                
LADJUST  DS    F                                                                
LRTG     DS    F                                                                
LSVREG1  DS    F                                                                
LIOADDR  DS    F                   SAVE OFF THE IOADDR                          
*                                                                               
LFLAG1   DS    XL1                                                              
LF1BUYRV EQU   X'80'                                                            
*                                                                               
LINDS    DS    XL1                                                              
LNOHDR   EQU   X'80'                                                            
LAUTADJ  EQU   X'40'                                                            
LALLADJ  EQU   X'20'                                                            
LNEWSTA  EQU   X'10'                                                            
LPKGSLV  EQU   X'08'                                                            
LCHG     EQU   X'04'                                                            
LCOS2    EQU   X'02'                - WE HAVE A COS2 FACTOR                     
*                                                                               
LHDRKEY  DS    XL32                                                             
LTMPKEY  DS    XL32                                                             
LSEQNUM  DS    XL1                                                              
LSUBDPT  DS    CL1                                                              
LNDEMOS  EQU   14                                                               
LDNAMES  DS    (LNDEMOS)CL7                                                     
LDEMS    DS    (LNDEMOS)XL3                                                     
         DS    X                                                                
LUPFIL   DS    C                                                                
LUPGRD   DS    XL8                                                              
LUPFRBK  DS    XL2                                                              
LUPFRBKT DS    XL1                 LUPFRBK BOOK TYPE                            
LUPFRBKL DS    XL6                                                              
LUPPUT   DS    CL1                                                              
LUPSHR   DS    CL1                                                              
LOVDAY   DS    X                                                                
LOVTIME  DS    XL4                                                              
LDEMOVR  DS    (LNDEMOS)XL6                                                     
         DS    X                                                                
LDMUPBLK DS    (SPDEMUP2)X                                                      
LSPWEL   DS    XL64                                                             
LBTREL   DS    XL300               <===  EXPANDED FROM 256 TO 300               
LBTREL2  DS    XL300               <===  EXPANDED FROM 256 TO 300               
LBTREL3  DS    XL300               <===  EXPANDED FROM 256 TO 300               
LSHPEL   DS    XL256               <===  NEW FOR PUT OVERRIDE ELEMENT           
*                            MHC    01/09/03                                    
LDTREL   DS    XL256                                                            
LDMOEL   DS    XL128                                                            
LBWDELSV DS    XL(BWDELL2Q)                                                     
LSEQNOS  DS    XL256                                                            
LIMPVALS DS    (LNDEMOS)XL6                                                     
LALLVALS DS    (LNDEMOS)XL6                                                     
LADJDEM  DS    XL2                                                              
LMASPRD  DS    XL2                                                              
LESTREP  DS    CL3                 SAVE THE ESTIMATE DEFAULT REP CODE           
LREP     DS    XL2                 SAVE THE REP CODE                            
LID      DS    CL12                                                             
LTRT     DS    XL256                                                            
         EJECT                                                                  
* SPDEMLK                                                                       
         DS    0H                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDEMLK                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DEDBLOCK                                                                      
         DS    0H                                                               
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDEBLOCK                                                                      
         DS    0H                                                               
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
         SPACE 3                                                                
AEXTRAWK DS    A                                                                
SVDAT    DS    XL3                 SAVE INFADDED FOR AUTH                       
*                                                                               
LIND     DS    X                                                                
LPOL     EQU   X'80'                                                            
LREXFR   EQU   X'40'                                                            
LFSTREC  EQU   X'20'                                                            
LSEPLINE EQU   X'10'                                                            
LPUTREC  EQU   X'08'                                                            
LSEPDLY  EQU   X'04'                                                            
LSPILLEL EQU   X'02'                                                            
LXFR1STP EQU   X'01'                                                            
         SPACE 1                                                                
LOCALX   EQU   *                                                                
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSFCD                                                       
         EJECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSECD                                                       
         EJECT                                                                  
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
* SPNWSCOMP                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPNWSCOMP                                                      
         PRINT ON                                                               
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         PRINT ON                                                               
* SPGENEST                                                                      
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENAUTH                                                      
         EJECT                                                                  
       ++INCLUDE SPAUTHD                                                        
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
       ++INCLUDE SPDEMLKXTD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'151SPNWS04   01/22/08'                                      
         END                                                                    
