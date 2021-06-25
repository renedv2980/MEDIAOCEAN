*          DATA SET SPNWS15A   AT LEVEL 044 AS OF 07/17/02                      
*PHASE T20715A,*                                                                
         TITLE 'BWS15 - BUYERS WORK SHEET - SPILL ESTIMATING SCREEN'            
T20715   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20715**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
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
         ST    R6,LASAVE                                                        
         AH    R6,=Y(SAVAREAX-SAVAREA)                                          
         AH    R6,=H'1024'                                                      
         USING SAVED,R6                                                         
*                                                                               
         L     R1,ACOM             SET V(DEMAND)                                
         L     R1,CDEMAND-COMFACSD(R1)                                          
         ST    R1,VDEMAND                                                       
*                                                                               
         LA    R2,IOKEY            R2=A(HEADER KEY)                             
         USING BWHRECD,R2                                                       
         LA    R3,APRECKEY         R3=A(DETAIL KEY)                             
         USING BWDRECD,R3                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         DC    AL4(0)                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     FSTRPT                                                           
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   MVI   LCHG,0                                                           
         MVI   LFLAG,0                                                          
         MVI   APINDS,0                                                         
         XC    BWHKEY,BWHKEY       BUILD BWS HEADER RECORD KEY                  
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
         GOTO1 AVALMED,SPLMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         GOTO1 AVALBYR,SPLBYRH     VALIDATE BUYER                               
         BNE   VALKX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW         CHECK BUYER'S PASSWORD                       
         BZ    VALK2                                                            
         GOTO1 AVALPWD                                                          
         BNE   VALKX                                                            
*                                                                               
VALK2    GOTO1 AVALCAM,SPLCAMH     VALIDATE CAMPAIGN NUMBER                     
         BNE   VALKX                                                            
         MVC   BWHKCAM,BCAM                                                     
         GOTO1 AGETCLT,CMPCLTC     (GET CLIENT)                                 
         BE    *+16                                                             
         LA    R1,SPLCAMH                                                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
         GOTO1 AGETPRD,CMPPRDN     (GET PRODUCT)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DBLOCK,DBLOCK       GET PRIMARY DEMO NAME                        
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         LA    R0,ESTUSRNM                                                      
         LA    R4,ESTDEMS                                                       
         XC    LDNAME,LDNAME                                                    
         GOTO1 VDEMOCON,APPARM,(1,(R4)),(2,LDNAME),(C'S',DBLOCK),(R0)           
         MVC   LDEM(3),ESTDEMS                                                  
         MVI   LDEM+3,FF                                                        
*                                                                               
         GOTO1 AVALSTA,SPLSTNH     VALIDATE STATION                             
         BNE   VALKX                                                            
         MVC   BWHKMKT,BMKT        STATION'S MARKET                             
*                                                                               
         GOTO1 AVALDAY,SPLDAYH     VALIDATE DAYS                                
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALTIM,SPLTIMH     VALIDATE TIMES                               
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALDPL,SPLDLNH     VALIDATE DAYPART/LENGTH                      
         BNE   VALKX                                                            
         CLI   CMPDPOPT,C'M'       TEST SUBDPT SCHEDULED UNDER MASTER           
         BNE   *+12                                                             
         CLI   DPTTYPE,C'S'        YES-TEST THIS IS A SUBDAYPART                
         BE    ESUB                YES-ERROR                                    
         CLI   BSLN,0              SPOT LENGTH MUST BE SPECIFIED                
         BE    ENOSLN                                                           
*                                                                               
         GOTO1 AIO,DIRHI+IO1          READ THE HEADER RECORD                    
         BNE   VALKX                                                            
         CLC   BWHKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                  
         BE    VALK4                                                            
         CLI   APRECNUM,RECSID        NO HEADER-TEST FOR NSID                   
         BNE   VALK99                 NO-ERROR                                  
         OI    TWAFLAG,TWANOHDR                                                 
         XC    BCMSEQ,BCMSEQ                                                    
         B     VALK10                                                           
*                                                                               
VALK4    GOTO1 AIO,FILGETU1        GET THE HEADER RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         MVC   BCMSEQ,BWHKSEQ      SAVE CAMPAIGN/MARKET SEQ CODE                
         MVI   BSTACD,0                                                         
         LA    R4,BWHFSTEL         LOOK FOR STATION IN HEADER                   
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
VALK6    CLI   0(R4),0                                                          
         BE    VALK8                                                            
         CLI   0(R4),BWHELCDQ                                                   
         BNE   VALK7                                                            
         USING BWHEL,R4                                                         
         IC    RE,BWHSEQ                                                        
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         CLC   QSTA,BWHSTA                                                      
         BNE   VALK7                                                            
         STC   RE,BSTACD           SET STATION SEQ NO                           
         B     VALK10                                                           
*                                                                               
VALK7    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALK6                                                            
*                                                                               
VALK8    CLI   APRECNUM,RECSID     STATION NOT FOUND-TEST FOR NSID              
         BNE   VALK99              NO-ERROR                                     
         OI    TWAFLAG,TWANOSTA    SET NO STATION                               
         TM    TWAFLAG,TWAFRST     TEST FIRST TIME                              
         BO    VALK10                                                           
         XC    APELEM,APELEM       NO-ADD STATION ELEMENT TO HEADER             
         LA    R4,APELEM                                                        
         MVI   BWHELCD,BWHELCDQ                                                 
         MVI   BWHELLN,BWHELLNQ                                                 
         LA    RF,1(RF)                                                         
         STC   RF,BWHSEQ                                                        
         MVC   BWHSTA,QSTA                                                      
         GOTO1 AADDELS,BWHRECD                                                  
*                                                                               
VALK10   CLI   APRECNUM,RECSID     TEST NSID RECORD                             
         BNE   VALK14              NO                                           
         TM    TWAFLAG,TWAFRST     YES-TEST FIRST TIME                          
         BO    VALK12              YES-BUILD BWS RECORD                         
         L     RE,LASAVE           NO-RESTORE BWS RECORD                        
         AH    RE,=Y(SVREC-SAVAREA)                                             
         L     R0,AIOAREA2                                                      
         LA    R1,2000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R3,AIOAREA2                                                      
         MVC   BCMSEQ,BWDKSEQ      RESET CAMPAIGN/MARKET CODE                   
         MVC   BSTACD,BWDKELST     AND STATION CODE                             
         B     VALK26                                                           
*                                                                               
VALK12   NI    TWAFLAG,FF-TWAFRST                                               
         GOTO1 ADTLBLD                                                          
         BNE   VALKX                                                            
         B     VALK26                                                           
*                                                                               
VALK14   TM    TWAMODE,TWAMLSM     BWS RECORD-TEST LIST/SELECT MODE             
         BZ    VALK16                                                           
         L     RE,ALSM             YES-SET THE KEY FROM SAVED STORAGE           
         LH    R1,LSMRDSP-LSMD(RE)                                              
         AR    R1,RE                                                            
         MVC   APRECKEY,LSMRKEY-LSMRTAB(R1)                                     
         B     VALK18                                                           
*                                                                               
VALK16   XC    APRECKEY,APRECKEY                                                
         MVI   BWDKTYP,BWDKTYPQ    SET UP FIRST PART OF DETAIL KEY              
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ                                                  
         MVI   BWDKELCD,BWDELCDQ                                                
         MVC   BWDKELST,BSTACD                                                  
         MVI   BWDKELPO,0                                                       
         MVC   BWDKELDY,BDAYS                                                   
         MVC   BWDKELTM,PTIMES                                                  
*                                                                               
VALK18   MVC   IOKEY,APRECKEY      READ DETAIL RECORD                           
         MVI   APBYTE,0                                                         
         LA    R1,MINHI2                                                        
         B     VALK20+4                                                         
*                                                                               
VALK20   LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    VALK21                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    TWAMODE,TWAMLSM                                                  
         BO    VALK99                                                           
         B     VALK24                                                           
*                                                                               
VALK21   L     R3,AIOAREA2                                                      
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    VALK22                                                           
         CLC   IOKEY(13),IOKEYSAV  YES-KEY MUST MATCH                           
         BNE   VALK99                                                           
         B     VALK26                                                           
*                                                                               
VALK22   CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV   MATCH THE KEY                  
         BNE   VALK24                                                           
         CLC   BWDTIMES,BTIMES     MATCH TIMES                                  
         BNE   VALK20                                                           
         CLC   BWDDPT,BDPT         MATCH DAYPART                                
         BNE   VALK20                                                           
         CLC   BWDSLN,BSLN         MATCH SPOT LENGTH                            
         BNE   VALK20                                                           
         CLI   APBYTE,0            TEST SEQ NO ALREADY FOUND                    
         BNE   EDUPREC             YES-DUPLICATE RECORDS                        
         MVC   APBYTE,BWDKELSQ     NO-SAVE THE SEQ NO                           
         B     VALK20                                                           
*                                                                               
VALK24   CLI   APBYTE,0            TEST RECORD FOUND                            
         BE    VALK99                                                           
         LA    R3,APRECKEY         YES-REREAD IT                                
         MVC   BWDKELSQ,APBYTE                                                  
         MVC   IOKEY,APRECKEY                                                   
         GOTO1 AMIN,MINRD2                                                      
         BE    VALK26                                                           
         DC    H'0'                                                             
*                                                                               
VALK26   L     R3,AIOAREA2                                                      
         LA    RE,BWDCOST1                                                      
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MONITOR ACTIVE              
         BZ    VALK28                                                           
         CLI   APRECNUM,RECSID     AND NOT SID RECORD                           
         BE    VALK28                                                           
         TM    APRECID,RIEFFDT2    YES-APRECID(1)=RECORD INDICATOR              
         BZ    *+8                                                              
         LA    RE,BWDCOST2                                                      
         TM    APRECID,RIEFFDT3                                                 
         BZ    VALK28                                                           
         LA    RE,BWDCOST3                                                      
*                                                                               
VALK28   ST    RE,LACOST           SAVE A(COST FIELD)                           
*                                                                               
         OI    APINDS,APIOKDIS     INDICATE RECORD FOUND                        
         XC    LAUPGEL,LAUPGEL                                                  
         XC    LAODTEL,LAODTEL                                                  
         XC    LASPIEL,LASPIEL                                                  
         L     R3,AIOAREA2                                                      
         SR    R0,R0               LOCATE ELEMENTS IN DETAIL RECORD             
         LA    R8,BWDEL                                                         
*                                                                               
VALK30   CLI   0(R8),0                                                          
         BE    VALK32                                                           
         CLI   0(R8),UPGELCDQ                                                   
         BNE   *+8                                                              
         ST    R8,LAUPGEL          A(UPGRADE ELEMENT)                           
         CLI   0(R8),ODTELCDQ                                                   
         BNE   *+8                                                              
         ST    R8,LAODTEL          A(OVERRIDE ELEMENT)                          
         CLI   0(R8),SPIELCDQ                                                   
         BNE   VALK31                                                           
         OC    LASPIEL,LASPIEL                                                  
         BNZ   VALK31                                                           
         ST    R8,LASPIEL          A(FIRST SPILL DEMO ELEMENT)                  
*                                                                               
VALK31   IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     VALK30                                                           
*                                                                               
VALK32   B     VALKX                                                            
*                                                                               
VALK99   MVC   FVMSGNO,=AL2(FVFERNF)  RECORD NOT FOUND                          
         LA    R1,SPLMEDH                                                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
*                                                                               
VALKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR REPORT                                               *         
***********************************************************************         
         SPACE 1                                                                
FSTRPT   L     R3,AIOAREA2                                                      
         MVI   SVFIRST,C'Y'                                                     
         SR    R0,R0                                                            
         LA    R4,SPLDEMH          TURN PREV VALIDATED BITS ON                  
*                                                                               
FSTR2    TM    FVATRB-FVIHDR(R4),FVAPROT                                        
         BO    *+8                                                              
         OI    FVIIND-FVIHDR(R4),FVIVAL                                         
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   FSTR2                                                            
*                                                                               
         XC    SVUPFILE,SVUPFILE   CLEAR SAVED VALUES                           
         XC    SVUPGRD,SVUPGRD                                                  
         XC    SVUPFRBK,SVUPFRBK                                                
         XC    SVUPFBL,SVUPFBL                                                  
         XC    SVUPINP,SVUPINP                                                  
         XC    SVUPDAY,SVUPDAY                                                  
         XC    SVUPTIM,SVUPTIM                                                  
         XC    SVUPPUT,SVUPPUT                                                  
         XC    SVUPSHR,SVUPSHR                                                  
         XC    SVBKS,SVBKS                                                      
         XC    SVDEM,SVDEM                                                      
*                                                                               
         OC    CMPUP,CMPUP         TEST FOR CAMPAIGN UPGRADE                    
         BZ    FSTR4                                                            
         MVC   SVUPFILE,CMPUF                                                   
         MVC   SVUPGRD,CMPUP                                                    
         MVC   SVUPFRBK,CMPFB                                                   
         MVC   SVUPFBL,CMPFBLST                                                 
         MVC   SVUPINP,CMPUPIN                                                  
         MVC   SVUPPUT,CMPUPUT                                                  
         MVC   SVUPSHR,CMPUSHR                                                  
*                                                                               
FSTR4    ICM   R4,15,LAUPGEL       UPGRADE ELEMENT                              
         BZ    FSTR6                                                            
         USING UPGEL,R4                                                         
         MVC   SVUPFILE,UPGFILE    DETAIL UPGRADE VALUES                        
         MVC   SVUPGRD,UPGRADE                                                  
         MVC   SVUPFRBK,UPGFRBK                                                 
         XC    SVUPFBL,SVUPFBL                                                  
         CLI   UPGELLN,51                                                       
         BL    *+10                                                             
         MVC   SVUPFBL,UPGFRBKL                                                 
         MVC   SVUPINP,UPGINPUT                                                 
         MVC   SVUPPUT,BWDUPUT                                                  
         MVC   SVUPSHR,BWDUSHR                                                  
*                                                                               
FSTR6    ICM   R4,15,LAODTEL                                                    
         BZ    FSTR8                                                            
         USING ODTEL,R4                                                         
         MVC   SVUPDAY,ODTDAY      OVERRIDE DAY/TIME                            
         MVC   SVUPTIM,ODTTIME                                                  
*                                                                               
FSTR8    BAS   RE,DEMO             INSPECT DEMO FIELD                           
         BNE   FSTRX                                                            
         BAS   RE,PROG             INSPECT PROGRAM                              
         BNE   FSTRX                                                            
         BAS   RE,COST             INSPECT COST                                 
         BL    FSTRX                                                            
         BE    FSTR10                                                           
         L     R3,AIOAREA2         EDIT THE COST                                
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBFLOAT,C'$'                                                     
         MVC   EBAIN,LACOST                                                     
         LA    R1,SPLCST                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'SPLCST                                                  
         MVI   EBDECS,2                                                         
         MVI   EBALIGN,C'L'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    SPLCSTH+6,FVOXMT                                                 
*                                                                               
FSTR10   BAS   RE,WEEKLY           INSPECT WEEKLY FIELD                         
         BNE   FSTRX                                                            
         BAS   RE,VALUPGRD         INSPECT UPGRADE FIELD                        
         BL    FSTRX                                                            
         BE    *+14                                                             
         MVC   SPLUPG,SVUPINP                                                   
         OI    SPLUPGH+6,FVOXMT                                                 
*                                                                               
         BAS   RE,BOOKS            INSPECT BOOK FIELDS                          
         BNE   FSTRX                                                            
         OC    SVBKS,SVBKS                                                      
         BNZ   *+8                 BOOKS INPUT                                  
         BAS   RE,GETBKS           NONE-GET THE BOOKS                           
         BAS   RE,DISBKS           DISPLAY THE BOOKS                            
*                                                                               
         BAS   RE,GETMKTS          FIND THE SPILL MARKETS                       
         BNE   FSTRX                                                            
*                                                                               
         GOTO1 ADEMUP              DO THE UPGRADES                              
         GOTO1 AGETDEMS            GET DEMOS                                    
*                                                                               
         OI    SPLFUTH+6,FVOXMT    FORMAT THE FOOTLINE                          
         XC    SPLFUT,SPLFUT                                                    
         TM    TWAMODE,TWAMLSM                                                  
         BZ    FSTRX                                                            
         MVC   SPLFUT(L'FUT1),FUT1                                              
         LA    R4,SPLFUT+L'FUT1+1                                               
         CLI   APRECNUM,RECSID                                                  
         BNE   *+14                                                             
         MVC   0(L'FUT2,R4),FUT2                                                
         LA    R4,L'FUT2+1(R4)                                                  
         MVC   0(L'FUT3,R4),FUT3                                                
         B     FSTRX                                                            
*                                                                               
FSTR99   MVC   FVMSGNO,=AL2(FVNODEMO)   NO DEMO INFO AVAILABLE                  
*                                                                               
FSTRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   MVC   LSVDPTLN,BDPT       SAVE DAYPART/LENGTH                          
         CLI   APRECNUM,RECSID     TEST NSID                                    
         BE    DISK2                                                            
         MVC   IOKEY(13),APRECKEY  NO-GET THE RECORD                            
         GOTO1 AMIN,MINRD2                                                      
         BNE   DISKX                                                            
         L     R3,AIOAREA2                                                      
         B     DISK14                                                           
*                                                                               
DISK2    OI    TWAFLAG,TWAFRST     NSID                                         
         LA    R8,APRECKEY                                                      
         USING NSIDKEYD,R8                                                      
         GOTO1 AGETMED,NSAGYMD                                                  
         BNE   *+10                                                             
         MVC   SPLMED,QMED         MEDIA                                        
         MVI   LDPT,0                                                           
         SR    R0,R0                                                            
         L     R6,LASAVE                                                        
         USING SAVAREA,R6                                                       
         LA    R4,SAVKEYS          EXTRACT BUYER CAMPAIGN AND DAYPART           
*                                  FROM SAVED KEY                               
DISK4    CLI   0(R4),0                                                          
         BE    DISK10                                                           
         LA    RF,SPLBYR                                                        
         CLI   0(R4),KEYBYR        BUYER                                        
         BE    DISK6                                                            
         LA    RF,SPLCAM                                                        
         CLI   0(R4),KEYCAM        CAMPAIGN                                     
         BE    DISK6                                                            
         CLI   0(R4),KEYDPL        DAYPART                                      
         BNE   DISK8                                                            
         MVC   LDPT,2(R4)                                                       
         B     DISK8                                                            
*                                                                               
DISK6    ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     DISK8                                                            
         MVC   0(0,RF),2(R4)                                                    
*                                                                               
DISK8    ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     DISK4                                                            
*                                                                               
DISK10   XC    APDUB,APDUB                                                      
         MVC   APDUB+2(3),NSSTA                                                 
         GOTO1 VMSUNPK,APPARM,(X'80',APDUB),APWORK,APWORK+4                     
         MVC   SPLSTN(8),APWORK+4  STATION                                      
         CLI   SPLSTN+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   SPLSTN+4,C' '                                                    
         CLI   SPLSTN,C'0'         TEST CABLE                                   
         BL    *+8                                                              
         MVI   SPLSTN+4,C'/'                                                    
         GOTO1 AGETDAY,NSDAY       DAYS                                         
         MVC   SPLDAY,QDAYS                                                     
         GOTO1 AGETTIM,NSTIME      TIMES                                        
         MVC   SPLTIM,QTIMES                                                    
         XC    SPLSUB,SPLSUB                                                    
         MVI   SPLSDP,0                                                         
         CLI   LDPT,0              TEST DAYPART IN THE KEY                      
         BNE   DISK12                                                           
         MVC   LDPT,NSDPT          NO-                                          
         CLI   CMPDPOPT,C'M'       TEST SUB-DAYPARTS UNDER MASTER               
         BNE   DISK12                                                           
         GOTO1 AGETDPT,LDPT        YES-                                         
         CLI   DPTTYPE,C'S'        TEST DAYPART IS A SUB-DAYPART                
         BNE   DISK12                                                           
         MVC   LDPT,DPTMAS                                                      
*                                                                               
DISK12   CLC   LDPT,NSDPT          TEST SUBDAYPART                              
         BE    *+16                                                             
         MVC   SPLSDP,NSDPT                                                     
         MVC   SPLSUB(10),=C'Subdaypart'                                        
         MVC   SPLDLN(1),LDPT                                                   
         ZIC   RF,NSSLN            SPOT LENGTH                                  
         BAS   RE,DISLEN                                                        
         B     DISKX                                                            
*                                                                               
         USING BWDRECD,R3                                                       
DISK14   GOTO1 AGETMED,BWDKAGMD    BWS RECORD                                   
         BNE   *+10                                                             
         MVC   SPLMED,QMED         MEDIA                                        
         LA    R1,BWDKBYR          BUYER                                        
         ICM   R1,8,=X'B'                                                       
         GOTO1 AGETBYR                                                          
         MVC   SPLBYR,QBYR                                                      
         GOTO1 AGETCM,BWDKSEQ      CAMPAIGN                                     
         MVC   SPLCAM,QCAM                                                      
         MVC   SPLSTN(L'BWDSTA),BWDSTA   STATION                                
         CLI   SPLSTN+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   SPLSTN+4,C' '                                                    
         CLI   SPLSTN,C'0'         TEST CABLE                                   
         BL    *+8                                                              
         MVI   SPLSTN+4,C'/'                                                    
         MVC   SPLDLN(1),BWDDPT    DAYPART                                      
         GOTO1 AGETDAY,BWDDAYS     DAYS                                         
         MVC   SPLDAY,QDAYS                                                     
         GOTO1 AGETTIM,BWDTIMES    TIMES                                        
         MVC   SPLTIM,QTIMES                                                    
         XC    SPLSUB,SPLSUB                                                    
         MVI   SPLSDP,0                                                         
         CLI   BWDSUBDP,0          TEST SUBDAYPART                              
         BE    *+16                                                             
         MVC   SPLSDP,BWDSUBDP                                                  
         MVC   SPLSUB(10),=C'Subdaypart'                                        
         ZIC   RF,BWDSLN                                                        
         BAS   RE,DISLEN                                                        
*                                                                               
DISKX    MVC   BDPT(2),LSVDPTLN    RESTORE DAYPART/LENGTH                       
         B     EXIT                                                             
         SPACE  2                                                               
DISLEN   CVD   RF,APDUB            DISPLAY SPOT LENGTH                          
         OI    APDUB+7,X'0F'                                                    
         LA    R1,SPLDLN+1                                                      
         CLI   SPLDLN,C'1'                                                      
         BL    *+12                                                             
         MVI   0(R1),C'/'                                                       
         LA    R1,1(R1)                                                         
         UNPK  0(3,R1),APDUB                                                    
         CLI   0(R1),C'0'                                                       
         BNE   *+14                                                             
         MVC   0(2,R1),1(R1)                                                    
         MVI   2(R1),C' '                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORDS                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SAVED,R6                                                         
DISREC   TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    DISR2                                                            
         CLI   APPFKEY,PFK04       AND PF4, PF5, PF6 OR PF12                    
         BL    DISR2                                                            
         CLI   APPFKEY,PFK06                                                    
         BNH   *+12                                                             
         CLI   APPFKEY,PFK12                                                    
         BNE   DISR2                                                            
         MVI   APMODE,APMLRP       YES-TELL ROOT ITS THE LAST SCREEN            
         MVC   SCPFKEY,APPFKEY         AND PASS ON PF KEY VALUE                 
         MVI   TWAFLAG,0                                                        
         B     DISRX                                                            
*                                                                               
DISR2    CLI   APPFKEY,PFK02       TEST PF2                                     
         BNE   DISR4                                                            
         CLI   APRECNUM,RECSID     AND NOT NSID RECORD                          
         BE    DISR4                                                            
         MVI   APPFKEY,0           YES-IGNORE                                   
*                                                                               
DISR4    L     R3,AIOAREA2         INSPECT DAYS FIELD                           
         USING BWDRECD,R3                                                       
         MVC   APHALF,BDPT                                                      
         MVC   APFULL,BTIMES                                                    
         MVC   APBYTE,BDAYS                                                     
         GOTO1 AVALDAY,SPLDAYH                                                  
         BNE   DISRX                                                            
         MVC   BDPT(2),APHALF                                                   
         MVC   BTIMES,APFULL                                                    
         CLC   BDAYS,BWDDAYS       TEST DAYS = THE RECORD'S DAYS                
         BE    *+8                                                              
         OI    LFLAG,LDAYOVR       NO-INDICATE DAYS ARE OVERRIDDEN              
         TM    SPLDAYH+FVIIND-FVIHDR,FVIVAL    TEST DAYS FIELD CHANGED          
         BO    *+12                                                             
         OI    SPLDAYH+FVIIND-FVIHDR,FVIVAL    YES -                            
         OI    LFLAG,LNEWDAYS      INDICATE DAYS CHANGED THIS TIME              
         MVC   LDAYS,BDAYS         SAVE THE DAYS                                
         MVC   BDAYS,APBYTE                                                     
*                                                                               
         MVC   APHALF,BDPT         INSPECT TIMES FIELD                          
         MVC   APFULL,BTIMES                                                    
         GOTO1 AVALTIM,SPLTIMH                                                  
         BNE   DISRX                                                            
         MVC   BDPT(2),APHALF                                                   
         CLC   BTIMES,BWDTIMES     TEST TIMES = THE RECORD'S TIMES              
         BE    *+8                                                              
         OI    LFLAG,LTIMOVR       NO-INDICATE TIMES ARE OVERRIDDEN             
         TM    SPLTIMH+FVIIND-FVIHDR,FVIVAL    TEST TIMES FIELD CHANGED         
         BO    *+12                                                             
         OI    SPLTIMH+FVIIND-FVIHDR,FVIVAL    YES -                            
         OI    LFLAG,LNEWTIME      INDICATE TIMES CHANGED THIS TIME             
         MVC   LTIMES,BTIMES       SAVE THE TIMES                               
         MVC   BTIMES,APFULL                                                    
*                                                                               
         TM    SPLDEMH+FVIIND-FVIHDR,FVIVAL    TEST DEMO FIELD CHANGED          
         BO    DISR6                                                            
         OI    SPLDEMH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BAS   RE,DEMO                         INSPECT DEMO FIELD               
         BNE   DISRX                                                            
*                                                                               
DISR6    TM    SPLPRGH+FVIIND-FVIHDR,FVIVAL    TEST PROGRAMMING CHANGED         
         BO    DISR8                                                            
         OI    SPLPRGH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BAS   RE,PROG             INSPECT PROGRAM                              
         BNE   FSTRX                                                            
*                                                                               
DISR8    TM    SPLCSTH+FVIIND-FVIHDR,FVIVAL    TEST COST FIELD CHANGED          
         BO    DISR10                                                           
         OI    SPLCSTH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BAS   RE,COST             INSPECT COST                                 
         BL    DISRX                                                            
         BE    DISR10                                                           
         L     RE,LACOST                                                        
         OC    0(L'BWDCOST1,RE),0(RE)          COST FIELD BLANK                 
         BZ    DISR10                                                           
         XC    0(L'BWDCOST1,RE),0(RE)                                           
         OI    LCHG,LCOST                                                       
*                                                                               
DISR10   TM    SPLWKYH+FVIIND-FVIHDR,FVIVAL    TEST WEEKLY FIELD CHANGE         
         BO    DISR12                                                           
         OI    SPLWKYH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BAS   RE,WEEKLY                       INSPECT WEEKLY FIELD             
         BNE   DISRX                                                            
*                                                                               
DISR12   TM    SPLUPGH+FVIIND-FVIHDR,FVIVAL    TEST UPGRADE CHANGED             
         BO    DISR16                                                           
         OI    SPLUPGH+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,VALUPGRD                                                      
         BL    DISRX                                                            
         BE    DISR16                                                           
         OC    LAUPGEL,LAUPGEL     UPGRADE REMOVED - TEST FOR UPGRD ELE         
         BZ    DISR14                                                           
         OI    LCHG,LUPG           YES - INDICATE UPGRADE CHANGE                
         MVC   SVUPFILE,CMPUF            USE CAMPAIGN UPGRADE VALUES            
         MVC   SVUPGRD,CMPUP                                                    
         MVC   SVUPFRBK,CMPFB                                                   
         MVC   SVUPFBL,CMPFBLST                                                 
         MVC   SVUPINP,CMPUPIN                                                  
         MVI   APELEM,UPGELCDQ           DELETE UPGRADE ELEMENT                 
         GOTO1 ADELELS,BWDRECD                                                  
         XC    LAUPGEL,LAUPGEL                                                  
         MVI   APELEM,ODTELCDQ           DELETE OVERRIDE ELEMENT                
         GOTO1 ADELELS,BWDRECD                                                  
         XC    LAODTEL,LAODTEL                                                  
*                                                                               
DISR14   MVC   SPLUPG,CMPUPIN            DISPLAY CAMPAIGN UPGRADE               
         OI    SPLUPGH+6,FVOXMT                                                 
*                                                                               
DISR16   TM    SPLBK1H+FVIIND-FVIHDR,FVIVAL    TEST ANY BOOK CHANGES            
         BZ    DISR18                                                           
         TM    SPLBK2H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR18                                                           
         TM    SPLBK3H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR18                                                           
         TM    SPLBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR18                                                           
         TM    LFLAG,LGETBKS       NO-TEST NEED TO GET BOOKS AGAIN              
         BZ    DISR20                                                           
         BAS   RE,GETBKS           YES-GET THE BOOKS                            
         BAS   RE,DISBKS               AND DISPLAY THEM                         
         B     DISR20                                                           
*                                                                               
DISR18   OI    SPLBK1H+FVIIND-FVIHDR,FVIVAL                                     
         OI    SPLBK2H+FVIIND-FVIHDR,FVIVAL                                     
         OI    SPLBK3H+FVIIND-FVIHDR,FVIVAL                                     
         OI    SPLBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,BOOKS            INSPECT THE BOOK FIELDS                      
         BNE   DISRX                                                            
         TM    SVWKYIND,SVONEBK    TEST SINGLE BOOK, 4 WEEKS                    
         BZ    DISR20                                                           
         BAS   RE,DISBKS           YES-MAKE SURE BOOKS ARE DISPLAYED            
*                                                                               
DISR20   CLI   SVFIRST,C'Y'        SKIP TO DISPLAY FOR VERY FIRST SCRN          
         BE    DISR24                                                           
         TM    LCHG,LUPG+LDEMO     TEST FOR UPGRADE CHANGE                      
         BNZ   *+12                                                             
         TM    LFLAG,LNEWTIME+LNEWDAYS  OR DAYS/TIMES CHANGED THIS TIME         
         BZ    DISR22                                                           
         GOTO1 ADEMUP              YES - DO THE UPGRADES                        
*                                                                               
DISR22   TM    LCHG,LDEMO+LBK      TEST DEMO OR BOOK CHANGES                    
         BNZ   *+12                                                             
         TM    LFLAG,LNEWTIME+LNEWDAYS OR DAYS/TIMES CHANGED THIS TIME          
         BZ    DISR24                                                           
         GOTO1 AGETDEMS            YES-GET DEMOS                                
         OC    SVWKY,SVWKY         TEST WEEKLY DEMO LOOKUPS                     
         BZ    DISR24                                                           
         BAS   RE,DISBKS           YES-DISPLAY THE BOOKS WITH WEEK NOS          
*                                                                               
DISR24   BAS   RE,DISSRC           DISPLAY RATING SERVICE                       
         CLI   SVFIRST,C'Y'        TEST FOR VERY FIRST SCREEN                   
         BE    DISR26                                                           
         GOTO1 AVALRTG             NO-VALIDATE RATING FIELDS                    
         BNE   DISRX                                                            
         CLI   APPFKEY,PFK02       TEST SID TRANFER                             
         BE    DISR50              YES - DO NOT RE-DISPLAY SCREEN               
         TM    LCHG,LDEMO+LBK+LUPG+LRTG  TEST SCREEN CHANGES                    
         BNZ   DISR26                    YES- RE-DISPLAY CURRENT SCREEN         
         TM    LFLAG,LNEWTIME+LNEWDAYS SAME FOR DAYS/TIMES CHANGED              
         BNZ   DISR26                                                           
         MVI   APMODE,APMLRP       ELSE TELL CONTROLLER WE'RE DONE              
         B     DISR50                                                           
*                                                                               
DISR26   MVI   SVFIRST,C'N'                                                     
         SR    RE,RE               CLEAR THE SCREEN                             
         LA    R4,SPLMK1H                                                       
         LA    R8,SPLFUTH                                                       
DISR28   IC    RE,0(R4)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,DISRCLC                                                       
         BE    DISR30                                                           
         EX    RE,DISROC                                                        
         BZ    DISR30                                                           
         EX    RE,DISRXC                                                        
         OI    6(R4),FVOXMT                                                     
*                                                                               
DISR30   LA    R4,9(RE,R4)                                                      
         CR    R4,R8                                                            
         BL    DISR28                                                           
*                                                                               
         ZIC   R0,SVNMKTS          FORMAT MARKETS                               
         LA    RE,SVMKTS                                                        
         LA    RF,SVMKTNMS                                                      
         LA    R4,SPLMK1H                                                       
*                                                                               
DISR32   SR    R1,R1                                                            
         ICM   R1,3,0(RE)                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  8(4,R4),APDUB                                                    
         MVI   12(R4),C' '                                                      
         MVC   13(24,R4),0(RF)                                                  
         OI    6(R4),FVOXMT                                                     
         LA    R4,SPLMK2H-SPLMK1H(R4)                                           
         LA    RE,L'SVMKTS(RE)                                                  
         LA    RF,L'SVMKTNMS(RF)                                                
         BCT   R0,DISR32                                                        
*                                                                               
         XC    EBLOCK,EBLOCK       FORMAT RATINGS                               
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         LA    R3,SPLRT1H                                                       
         LA    R4,SVDEMVAL                                                      
         LA    R8,SPLPRJH                                                       
         LA    R9,SVPROJ                                                        
         MVI   LMKTNUM,1                                                        
*                                                                               
DISR36   LR    R2,R3                                                            
         LA    R0,4                                                             
*                                                                               
DISR38   XC    8(L'SPLRT1,R2),8(R2)                                             
         OI    6(R2),FVOXMT                                                     
         LA    R1,8(R2)                                                         
         ST    R1,EBAOUT                                                        
         LA    RE,L'SPLRT1                                                      
         STC   RE,EBLOUT                                                        
         OC    0(4,R4),0(R4)                                                    
         BNZ   DISR40                                                           
         SH    RE,=H'3'                                                         
         AR    RE,R1                                                            
         MVC   0(3,RE),=C'0.0'                                                  
         B     DISR42                                                           
*                                                                               
DISR40   MVI   EBFLOAT,0                                                        
         ST    R4,EBAIN                                                         
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR42   LA    R2,SPLRT2H-SPLRT1H(R2)                                           
         LA    R4,4(R4)                                                         
         BCT   R0,DISR38           DO FOR ALL BOOKS                             
*                                                                               
         XC    8(L'SPLPRJ,R8),8(R8)   FORMAT PROJECTED VALUE                    
         OI    6(R8),FVOXMT                                                     
         LA    R1,8(R8)                                                         
         ST    R1,EBAOUT                                                        
         LA    RE,L'SPLPRJ                                                      
         STC   RE,EBLOUT                                                        
         MVI   EBFLOAT,0                                                        
         TM    0(R9),X'80'                                                      
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         OC    1(3,R9),1(R9)                                                    
         BNZ   DISR44                                                           
         SH    RE,=H'4'                                                         
         AR    RE,R1                                                            
         MVC   0(1,RE),EBFLOAT                                                  
         MVC   1(3,RE),=C'0.0'                                                  
         B     DISR46                                                           
*                                                                               
DISR44   MVC   APFULL,0(R9)                                                     
         NI    APFULL,X'7F'                                                     
         LA    R1,APFULL                                                        
         ST    R1,EBAIN                                                         
         MVI   EBDECS,1                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR46   LA    R3,SPLMK2H-SPLMK1H(R3)   NEXT MARKET                             
         LA    R8,SPLMK2H-SPLMK1H(R8)                                           
         LA    R9,4(R9)                                                         
         ZIC   R1,LMKTNUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LMKTNUM                                                       
         CLC   LMKTNUM,SVNMKTS                                                  
         BNH   DISR36                                                           
*                                                                               
DISR50   CLI   APRECNUM,RECSID     TEST SID RECORD                              
         BNE   DISR52                                                           
         L     R0,LASAVE           YES - SAVE BWS RECORD THAT'S BEEN            
         AH    R0,=Y(SVREC-SAVAREA)      BUILT FROM IT                          
         LA    R1,2000                                                          
         L     RE,AIOAREA2                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         CLI   APPFKEY,PFK02       TEST SID TRANSFER                            
         BNE   DISRX                                                            
         MVI   APPFKEY,0                                                        
         GOTO1 AXFRADD             YES-TRANSFER NSID TO BWS                     
         BNE   DISRX                                                            
         MVI   SCPFKEY,PFK05       AND RETURN TO LIST SCREEN                    
         MVI   APMODE,APMLRP                                                    
         B     DISRX                                                            
*                                                                               
DISR52   TM    LCHG,LRTG+LPRG+LADJ+LCOST+LUPG TEST RECORD CHANGE                
         BZ    DISR54                                                           
         MVC   IOKEY(13),APRECKEY  YES-WRITE RECORD BACK                        
         GOTO1 AMIN,MINWRT2                                                     
         BE    DISR54                                                           
         DC    H'0'                                                             
*                                                                               
DISR54   B     DISRX                                                            
*                                                                               
DISRX    B     EXIT                                                             
         SPACE 2                                                                
DISRCLC  CLC   8(0,R4),SPACES      EXECUTED INSTRUCTIONS                        
DISROC   OC    8(0,R4),8(R4)                                                    
DISRXC   XC    8(0,R4),8(R4)                                                    
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RATING SERVICE                                          *         
***********************************************************************         
         SPACE 1                                                                
DISSRC   LR    R0,RE                                                            
         MVC   APBYTE,CLTSRC                                                    
         OI    SPLSRCH+6,FVOXMT                                                 
         MVC   SPLSRC,=C'NSI'                                                   
         CLI   APBYTE,C'N'                                                      
         BE    *+10                                                             
         MVC   SPLSRC,=C'ARB'                                                   
         CLI   APROF7,C'C'                                                      
         BNE   DISSRCX                                                          
         MVC   SPLSRC,=C'CSI'                                                   
         CLI   APBYTE,C'N'                                                      
         BE    DISSRCX                                                          
         MVC   SPLSRC,=C'BBM'                                                   
*                                                                               
DISSRCX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE BOOKS                                            *         
***********************************************************************         
         SPACE 1                                                                
GETBKS   NTR1  ,                                                                
         OC    CMPBOOKS,CMPBOOKS   NONE-TEST FOR CAMPAIGN DEFINED BKS           
         BZ    *+14                                                             
         MVC   SVBKS,CMPBOOKS             YES - USE THOSE                       
         B     GETBX                                                            
         XC    DBLOCK,DBLOCK       NO CAMPAIGN DEFINED BOOKS -                  
         MVC   DBCOMFCS,ACOM       GET LATEST BOOK FROM DEMAND                  
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBAREC,AIOAREA3                                                  
         MVC   DBSELMED,CUDMED                                                  
         MVC   DBSELSRC,CLTSRC                                                  
         MVC   DBSELUMK,BMKT                                                    
         MVC   DBSELSTA,QSTA                                                    
         MVC   DBSELAGY,CUAALF                                                  
         MVC   DBSELDAT,=X'630C'                                                
         MVI   DBFUNCT,DBGETTLB                                                 
         GOTO1 VDEMAND,APPARM,DBLOCK,0                                          
         XC    SVBKS,SVBKS         CLEAR SAVED BOOKS FIELD                      
         LA    R4,SVBKS+6                                                       
         LA    R1,4                4 BOOKS                                      
         MVC   0(2,R4),DBACTBK                                                  
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    GETB12              NO-THEN INCLUDE LATEST BOOK                  
*                                                                               
GETB2    MVC   0(2,R4),DBACTBK     DETERMINE ALL FOUR BOOKS                     
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    *+14                                                             
         CLC   0(2,R4),SVUPFRBK    YES - COMPARE TO SHARE BOOK MONTH            
         BE    GETB12                    EQUAL - THIS BOOK IS OK                
         CLC   0(2,R4),SVUPGRD+2   COMPARE TO PUT MONTH                         
         BE    GETB12              EQUAL - THIS BOOK IS OK                      
         LA    R0,L'UMAJBKS                                                     
         LA    R8,UMAJBKS                                                       
         CLI   CUDMED,C'C'         TEST CANADIAN                                
         BNE   GETB3                                                            
         LA    R0,L'CMAJBKSA                                                    
         LA    R8,CMAJBKSA         BBM BOOKS                                    
         CLI   CLTSRC,C'N'                                                      
         BNE   GETB3                                                            
         LA    R0,L'CMAJBKSN                                                    
         LA    R8,CMAJBKSN         CSI BOOKS                                    
*                                                                               
GETB3    LR    RE,R8                                                            
         LR    R9,R0                                                            
*                                                                               
GETB4    CLC   1(1,R4),0(RE)       IS THIS MONTH A MAJOR SWEEP MONTH            
         BE    GETB12              YES - THIS BOOK IS OK                        
         BL    GETB6                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,GETB4                                                         
*                                                                               
GETB6    CR    R0,R9               NO - IS THIS MONTH BEFORE FRST MAJOR         
         BNE   GETB8                                                            
         LA    RF,0(R9,R8)              YES - BACK UP ONE YEAR                  
         BCTR  RF,0                                                             
         MVC   1(1,R4),0(RF)                                                    
         ZIC   RF,DBACTBK                                                       
         BCTR  RF,0                                                             
         STC   RF,0(R4)                                                         
         B     GETB12                                                           
*                                                                               
GETB8    BCTR  RE,0                MONTH IS BETWEEN MAJORS                      
         SR    RF,RF                                                            
*                                                                               
GETB10   IC    RF,1(R4)                                                         
         BCTR  RF,0                                                             
         STC   RF,1(R4)                                                         
         CLC   1(1,R4),0(RE)                                                    
         BNH   GETB12                                                           
         CLC   0(2,R4),SVUPFRBK                                                 
         BE    GETB12                                                           
         CLC   0(2,R4),SVUPGRD+2                                                
         BNE   GETB10                                                           
*                                                                               
GETB12   MVC   DBACTBK,0(R4)       BACK UP ONE MONTH FOR NEXT                   
         ZIC   RF,1(R4)            PREVIOUS BOOK                                
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         STC   RF,DBACTBK+1                                                     
         B     GETB14                                                           
         MVI   DBACTBK+1,12                                                     
         IC    RF,0(R4)                                                         
         BCTR  RF,0                                                             
         STC   RF,DBACTBK                                                       
*                                                                               
GETB14   BCTR  R4,0                NEXT BOOK                                    
         BCTR  R4,0                                                             
         BCT   R1,GETB2            DO FOR ALL BOOKS                             
*                                                                               
GETBX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY THE BOOKS                                        *         
***********************************************************************         
         SPACE 1                                                                
DISBKS   NTR1                                                                   
         LA    R0,4                DISPLAY THE BOOKS                            
         LA    R4,SVBKS                                                         
         LA    R8,SPLBK1H                                                       
         LA    R9,C'1'                                                          
         MVI   APWORK+2,1                                                       
*                                                                               
DISB2    XC    8(L'SPLBK1,R8),8(R8)                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    DISB4                                                            
         MVC   APWORK(2),0(R4)                                                  
         MVI   8(R8),C' '                                                       
         GOTO1 VDATCON,APPARM,(3,APWORK),(6,9(R8))                              
         OC    SVWKY,SVWKY         TEST WEEKLY OPTION                           
         BZ    DISB4                                                            
         MVC   8(3,R8),9(R8)                                                    
         MVC   11(2,R8),13(R8)     YES-                                         
         MVI   13(R8),C'-'         1 BOOK, 4 WEEKS                              
         STC   R9,14(R8)                                                        
         CLI   SVWKY,C'W'                                                       
         BNE   DISB4                                                            
         MVC   14(1,R8),SVWKY+1    4 BOOKS, 1 WEEK                              
*                                                                               
DISB4    OI    6(R8),FVOXMT                                                     
         LA    R4,2(R4)                                                         
         LA    R8,SPLBK2H-SPLBK1H(R8)                                           
         LA    R9,1(R9)                                                         
         BCT   R0,DISB2                                                         
*                                                                               
DISBX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET THE SPILL MARKETS                                               *         
***********************************************************************         
         SPACE 1                                                                
GETMKTS  NTR1  ,                                                                
         LA    R9,SVMKTS                                                        
         LA    R0,MAXMKTS+1                                                     
         SR    R8,R8                                                            
         LA    R4,IOKEY            BUILD SPILL RECORD KEY                       
         USING SDEFRECD,R4                                                      
         XC    SDEFKEY,SDEFKEY                                                  
         MVC   SDEFKTYP,=X'0D13'                                                
         MVC   SDEFKAGY,CUAALF                                                  
         MVI   SDEFKRSV,C'0'                                                    
         CLI   CLTSRC,C'A'         0=NSI,1=ARB                                  
         BNE   *+8                                                              
         MVI   SDEFKRSV,C'1'                                                    
         MVC   SDEFKSTA(4),QSTA                                                 
         MVC   SDEFKCLT,BCLT       TRY CLIENT SPECIFIC FIRST                    
*                                                                               
GETM2    GOTO1 AIO,DIRHI+IO3       READ SPILL RECORD POINTER                    
         BNE   *+14                                                             
         CLC   SDEFKEY(13),IOKEYSAV                                             
         BE    GETM4                                                            
         MVC   IOKEY,IOKEYSAV                                                   
         OC    SDEFKCLT,SDEFKCLT                                                
         BZ    GETM99                                                           
         XC    SDEFKCLT,SDEFKCLT   TRY NOT CLIENT SPECIFIC                      
         B     GETM2                                                            
*                                                                               
GETM4    GOTO1 AIO,FILGET3         GET SPILL RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIOAREA3                                                      
         CLC   SDEFLEN,=H'256'     RECORD LENGTH MUST BE LE 256                 
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    R1,SDEFEL                                                        
         USING SDEFEL05,R1                                                      
         SR    RF,RF               LOOK FOR SPILL MARKET ELEMENTS               
*                                                                               
GETM6    CLI   0(R1),0             TEST END OF SPILL RECORD                     
         BE    GETM10                                                           
         CLI   0(R1),5             TEST SPILL MARKET ELEMENT                    
         BNE   GETM8                                                            
         CLC   SDEFAMKT,BMKT       TEST SPILL MKT EQUAL ACTUAL MKT              
         BE    GETM8               YES - SKIP                                   
         TM    SDEFCEX,X'80'       TEST '*' FEATURE                             
         BO    GETM8               YES - SKIP                                   
         MVC   0(2,R9),SDEFAMKT    SAVE AGENCY MARKET                           
         MVC   2(2,R9),SDEFRMKT    SAVE RATING SVC MARKET                       
         MVC   4(1,R9),SDEFBKTY    SAVE SPECIAL BOOK TYPE                       
         LA    R9,L'SVMKTS(R9)                                                  
         LA    R8,1(R8)                                                         
         BCT   R0,GETM8                                                         
         DC    H'0'                EXCEEDED MAX N'MARKETS                       
*                                                                               
GETM8    IC    RF,1(R1)            NEXT ELEMENT                                 
         AR    R1,RF                                                            
         B     GETM6                                                            
*                                                                               
GETM10   STC   R8,SVNMKTS          SAVE ACTUAL N'SPILL MARKETS                  
         LTR   R8,R8                                                            
         BZ    GETM99                                                           
         LA    R9,SVMKTS           GET THE MARKET NAMES                         
         LA    R4,SVMKTNMS                                                      
*                                                                               
GETM12   MVC   0(24,R4),SPACES                                                  
         MVC   0(9,R4),=C'*UNKNOWN*'                                            
         LA    R2,IOKEY                                                         
         USING MKTRECD,R2                                                       
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(MKTKEYLN-1),MKTKEY                                      
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         SR    RE,RE                                                            
         ICM   RE,3,0(R9)                                                       
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  MKTKMKT,APDUB                                                    
         MVC   MKTKAGY,CUAALF                                                   
         L     R2,AIOAREA3                                                      
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSTAFIL+IORD                                                
         BNE   *+10                                                             
         MVC   0(24,R4),MKTNAME                                                 
         LA    R4,L'SVMKTNMS(R4)                                                
         LA    R9,L'SVMKTS(R9)                                                  
         BCT   R8,GETM12                                                        
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETMX                                                            
*                                                                               
GETM99   MVC   FVMSGNO,=AL2(FVNOSPL)                                            
*                                                                               
GETMX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT THE PROGRAM FIELD                                           *         
* OUTPUT : LCHG = LPRG IF THE PROGRAMMING CHANGES                     *         
*          LCHG = LADJ IF ADJACENCY CODE CHANGES                      *         
*          CC EQ OK                                                   *         
*             NE ERROR                                                *         
***********************************************************************         
         SPACE 1                                                                
PROG     NTR1  ,                                                                
         GOTO1 AFVAL,SPLPRGH       VALIDATE PROGRAM NAME                        
         BH    PROGX                                                            
         BE    PROG1                                                            
         MVC   SPLPRG,BWDPROG      MISSING - DISPLAY PROGRAMMING                
         OI    SPLPRGH+6,FVOXMT                                                 
         B     PROGX                                                            
*                                                                               
PROG1    CLI   FVIFLD,C'='         TEST PROGRAM ADJACENCY CODE                  
         BNE   PROG2                                                            
         GOTO1 AVALADJ,FVIFLD+1    YES-VALIDATE IT                              
         BNE   PROGX                                                            
         CLC   ADJCODE,BWDADJ      VALID-TEST CHANGE                            
         BE    PROGX                                                            
         MVC   BWDADJ,ADJCODE      YES-SET ADJACENCY CODE                       
         OI    LCHG,LADJ               INDICATE CHANGE                          
         B     PROGX                                                            
*                                                                               
PROG2    CLC   BWDPROG,FVIFLD      TEST PROGRAM CHANGE                          
         BE    PROGX                                                            
         MVC   BWDPROG,FVIFLD      YES -                                        
         OI    BWDINDS,BWDIPRG                                                  
         OI    LCHG,LPRG                                                        
*                                                                               
PROGX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT THE COST FIELD                                              *         
* OUTPUT : LCHG = LCOST IF COST FIELD IN RECORD CHANGED               *         
*          CC EQ  - COST FOUND AND RECORD CHANGED IF NECESSARY        *         
*             LO  - BAD ERROR                                         *         
*             HI  - COST NOT FOUND                                    *         
***********************************************************************         
         SPACE 1                                                                
COST     NTR1  ,                                                                
         GOTO1 AFVAL,SPLCSTH                                                    
         BH    COSTX2                                                           
         BL    COSTX1                                                           
         ZIC   RF,FVILEN                                                        
         ST    RF,APPARM+4                                                      
         GOTO1 VCASHVAL,APPARM,FVIFLD                                           
         CLI   APPARM,X'FF'                                                     
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVICST)                                             
         B     COSTX2                                                           
         GOTO1 ANETCOST,APPARM+4   NET DOWN THE COST IF NECESSARY               
         L     RE,LACOST                                                        
         CLC   APPARM+4(4),0(RE)                                                
         BE    COSTX2                                                           
         OI    LCHG,LCOST                                                       
         MVC   0(4,RE),APPARM+4                                                 
         B     COSTX2                                                           
*                                                                               
COSTX1   CR    RA,RB                                                            
         B     EXIT                                                             
*                                                                               
COSTX2   CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT THE PROGRAM FIELD                                           *         
* OUTPUT : LCHG = LDEMO IF THE DEMO CHANGES                           *         
*          CC EQ OK                                                   *         
*             NE ERROR                                                *         
***********************************************************************         
         SPACE 1                                                                
DEMO     NTR1                                                                   
         GOTO1 AFVAL,SPLDEMH       VALIDATE THE DEMO FIELD                      
         BH    DEMOX                                                            
         BE    DEMO6                                                            
         OC    INORTG,INORTG       MISSING-TEST OVERRIDE TARGET DEMO            
         BZ    DEMO4               NO                                           
         CLC   SVDEM(3),INORTG     YES-USE IT                                   
         BE    DEMO2                                                            
         OI    LCHG,LDEMO                                                       
         MVC   SVDEM(3),INORTG                                                  
         MVI   SVDEM+3,FF                                                       
*                                                                               
DEMO2    LA    R0,ESTUSRNM         FORMAT DEMO NAME                             
         LA    R4,SVDEM                                                         
         GOTO1 VDEMOCON,APPARM,(1,(R4)),(2,SPLDEM),(C'S',DBLOCK),(R0)           
         OI    SPLDEMH+6,FVOXMT                                                 
         B     DEMOX                                                            
*                                                                               
DEMO4    MVC   SPLDEM,LDNAME       USE PRIMARY DEMO                             
         OI    SPLDEMH+6,FVOXMT                                                 
         CLC   SVDEM,LDEM          TEST ALREADY PRIMARY DEMO                    
         BE    DEMOX                                                            
         MVC   SVDEM,LDEM          NO                                           
         OI    LCHG,LDEMO                                                       
         B     DEMOX                                                            
*                                                                               
DEMO6    CLI   SPLDEM,C'R'         RATING DEMO?                                 
         BE    DEMO7                                                            
         CLI   SPLDEM,C'E'                                                      
         BNE   DEMOERR             MUST BE AN IMPRESSION DEMO, ERROR            
*                                                                               
DEMO7    LA    R0,ESTUSRNM                                                      
         GOTO1 VDEMOVAL,APPARM,(1,SPLDEMH),(1,APDUB),(C'S',DBLOCK),(R0)         
         CLI   0(R1),0                                                          
         BE    *+14                                                             
DEMOERR  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     DEMOX                                                            
         CLC   SVDEM,APDUB         TEST DEMO HAS CHANGED                        
         BE    DEMOX                                                            
         MVC   SVDEM,APDUB         YES                                          
         OI    LCHG,LDEMO                                                       
*                                                                               
DEMOX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT THE WEEKLY FIELD                                            *         
* OUTPUT : LCHG = LBK IF WEEKLY OPTION HAS CHANGED                    *         
*          CC EQ  - OK                                                *         
*             NE  - ERROR                                             *         
***********************************************************************         
         SPACE 1                                                                
WEEKLY   NTR1  ,                                                                
         XC    APHALF,APHALF                                                    
         NI    LFLAG,255-LGETBKS                                                
         NI    SVWKYIND,255-SVONEBK                                             
         GOTO1 AFVAL,SPLWKYH       VALIDATE WEEKLY FIELD                        
         BH    WEEKX                                                            
         BL    WEEK6               MISSING                                      
         CLC   FVIFLD(2),=C'W='    TEST SINGLE WEEK NUMBER                      
         BNE   WEEK4                                                            
         CLI   FVILEN,3            YES-VALIDATE THE WEEK NUMBER                 
         BNE   WEEK9                                                            
         CLI   FVIFLD+2,C'1'                                                    
         BL    WEEK9                                                            
         CLI   FVIFLD+2,C'4'                                                    
         BH    WEEK9                                                            
         MVI   APHALF,C'W'                                                      
         MVC   APHALF+1(1),FVIFLD+2                                             
         OC    SVWKY,SVWKY         TEST WEEKLY ALREADY SET                      
         BZ    WEEK6                                                            
         CLI   SVWKY,C'W'          YES-TEST ONE BOOK, 4 WEEKS                   
         BE    WEEK6                                                            
         OI    LFLAG,LGETBKS       YES-NEED TO GET BOOKS AGAIN                  
         B     WEEK6                                                            
*                                                                               
WEEK4    GOTO1 VSCANNER,APPARM,SPLWKYH,(1,APELEM),C',=,-'                       
         CLI   4(R1),1                                                          
         BNE   WEEK9                                                            
         LA    R4,APELEM           VALIDATE MMM/YY-W FORM                       
         CLI   1(R4),1                                                          
         BNE   WEEK9                                                            
         CLI   22(R4),C'W'                                                      
         BNE   WEEK9                                                            
         GOTO1 VDATVAL,(R1),(2,12(R4)),APWORK                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    WEEK9                                                            
         OI    SVWKYIND,SVONEBK    INDICATE SINGLE BOOK, 4 WEEKS                
         GOTO1 VDATCON,(R1),(0,APWORK),(3,APWORK+6)                             
         MVC   APHALF,APWORK+6                                                  
         LA    R0,4                ALL 4 BOOKS THE SAME                         
         LA    R1,SVBKS                                                         
         MVC   0(2,R1),APHALF                                                   
         LA    R1,2(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
WEEK6    CLC   APHALF,SVWKY        TEST WEEKLY OPTION CHANGED                   
         BE    WEEKX                                                            
         MVC   SVWKY,APHALF        YES-SAVE NEW WEEKLY OPTION                   
         OI    LCHG,LBK                AND INDICATE CHANGE                      
         OC    SVWKY,SVWKY         TEST WEEKLY OPTION SET                       
         BNZ   WEEKX                                                            
         OI    LFLAG,LGETBKS       NO-GET BOOKS AGAIN                           
         B     WEEKX                                                            
*                                                                               
WEEK9    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
WEEKX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT UPGRADE FIELD                                               *         
* OUTPUT : LCHG = LUPG IF UPGRADE IN RECORD CHANGED                   *         
*          CC EQ  - UPGRADE FOUND AND RECORD CHANGED IF NECESSARY     *         
*             LO  - BAD ERROR                                         *         
*             HI  - UPGRADE NOT FOUND                                 *         
***********************************************************************         
         SPACE 1                                                                
VALUPGRD NTR1  ,                                                                
         CLI   SPLUPGH+5,0         TEST UPGRADE ENTERED                         
         BE    UPGRX1                                                           
         MVI   APFLAG,X'F8'                                                     
         GOTO1 AVALUPG,SPLUPGH     VALIDATE UPGRADE FIELD                       
         BNE   UPGRX2              ERROR                                        
         ICM   R4,15,LAUPGEL       TEST FOR UPGRADE ELEM                        
         BZ    UPGR2                                                            
         USING UPGEL,R4                                                         
         CLC   UPGFILE,APWORK      YES - TEST FOR CHANGES                       
         BNE   UPGR4                                                            
         CLC   UPGRADE,APWORK+1                                                 
         BNE   UPGR4                                                            
         CLC   UPGFRBK,APWORK+9                                                 
         BNE   UPGR4                                                            
         CLC   BWDUPUT,APWORK+16                                                
         BNE   UPGR4                                                            
         CLC   BWDUSHR,APWORK+17                                                
         BNE   UPGR4                                                            
         B     UPGR6                                                            
*                                                                               
UPGR2    XC    APELEM,APELEM       NO - BUILD UPGRADE ELEM                      
         LA    R4,APELEM                                                        
         MVI   UPGELCD,UPGELCDQ                                                 
         MVI   UPGELLN,UPGELLNQ                                                 
*                                                                               
UPGR4    MVC   UPGFILE,APWORK                                                   
         MVC   UPGRADE,APWORK+1                                                 
         MVC   UPGFRBK,APWORK+9                                                 
         MVC   UPGINPUT(L'SPLUPG),SPLUPG                                        
         MVC   BWDUPUT,APWORK+16                                                
         MVC   BWDUSHR,APWORK+17                                                
         MVC   SVUPFILE,UPGFILE    SAVE UPGRADE VALUES                          
         MVC   SVUPGRD,UPGRADE                                                  
         MVC   SVUPFRBK,UPGFRBK                                                 
         XC    SVUPFBL,SVUPFBL                                                  
         CLI   UPGELLN,51                                                       
         BL    *+10                                                             
         MVC   SVUPFBL,UPGFRBKL                                                 
         MVC   SVUPINP,UPGINPUT                                                 
         MVC   SVUPPUT,BWDUPUT                                                  
         MVC   SVUPSHR,BWDUSHR                                                  
         OI    LCHG,LUPG                                                        
         OC    LAUPGEL,LAUPGEL                                                  
         BNZ   UPGR6                                                            
         GOTO1 AADDELS,BWDRECD     ADD NEW UPGRADE ELEM                         
         MVC   LAUPGEL,16(R1)                                                   
*                                                                               
UPGR6    OC    APWORK+11(5),APWORK+11   TEST FOR OVERRIDE DAY/TIME              
         BNZ   UPGR8                                                            
         OC    LAODTEL,LAODTEL          NO - TEST FOR OVERRIDE ELEM             
         BZ    UPGR14                                                           
         MVI   APELEM,ODTELCDQ               YES - DELETE IT                    
         GOTO1 ADELELS                                                          
         XC    LAODTEL,LAODTEL                                                  
         OI    LCHG,LUPG                                                        
         B     UPGR14                                                           
*                                                                               
UPGR8    ICM   R4,15,LAODTEL            YES - TEST FOR OVERRIDE ELEM            
         BZ    UPGR10                                                           
         USING ODTEL,R4                                                         
         CLC   ODTDAY,APWORK+11                YES - TEST FOR CHANGES           
         BNE   UPGR12                                                           
         CLC   ODTTIME,APWORK+12                                                
         BNE   UPGR12                                                           
         B     UPGR14                                                           
*                                                                               
UPGR10   XC    APELEM,APELEM       BUILD OVERRIDE ELEM                          
         LA    R4,APELEM                                                        
         MVI   ODTELCD,ODTELCDQ                                                 
         MVI   ODTELLN,ODTELLNQ                                                 
*                                                                               
UPGR12   MVC   ODTDAY,APWORK+11                                                 
         MVC   ODTTIME,APWORK+12                                                
         OI    LCHG,LUPG                                                        
         OC    LAODTEL,LAODTEL                                                  
         BNZ   UPGR14                                                           
         GOTO1 AADDELS,BWDRECD     ADD NEW OVERRIDE ELEM                        
         MVC   LAODTEL,16(R1)                                                   
*                                                                               
UPGR14   B     UPGRX2                                                           
*                                                                               
UPGRX1   MVC   FVMSGNO,=AL2(FVFOK)                                              
         CR    RA,RB                                                            
         B     EXIT                                                             
*                                                                               
UPGRX2   CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT BOOK FIELDS                                                 *         
* OUTPUT : LCHG = LBK IF BOOKS WERE CHANGED                           *         
*          CC EQ  - OK                                                *         
*             NE  - ERROR                                             *         
***********************************************************************         
         SPACE 1                                                                
BOOKS    NTR1                                                                   
         TM    SVWKYIND,SVONEBK    TEST SINGLE BOOK, 4 WEEKS                    
         BO    BOOKX               YES-BOOKS ALREADY DETERMINED                 
         LA    R4,SPLBK1H                                                       
         LA    R8,SVBKS                                                         
         LA    R0,4                                                             
*                                                                               
BOOK2    LR    R1,R4               VALIDATE BOOK FIELD                          
         GOTO1 AFVAL                                                            
         BH    BOOKX                                                            
         BE    BOOK4                                                            
         OC    0(2,R8),0(R8)       MISSING                                      
         BZ    BOOK6                                                            
         XC    0(2,R8),0(R8)                                                    
         OI    LCHG,LBK                                                         
         B     BOOK6                                                            
*                                                                               
BOOK4    LA    R1,FVIFLD+5         TEST FOR '-WEEK'                             
         CLI   0(R1),C'-'                                                       
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'-'                                                       
         BNE   BOOK5               NO                                           
         OC    SVWKY,SVWKY         YES-TEST WEEKLY OPTION SET                   
         BZ    BOOK9               NO-ERROR                                     
         CLC   1(1,R1),SVWKY+1     YES-TEST CORRECT WEEK                        
         BNE   BOOK9                                                            
         MVC   0(2,R1),SPACES                                                   
*                                                                               
BOOK5    GOTO1 VDATVAL,APPARM,(2,FVIFLD),APWORK                                 
         OC    0(4,R1),0(R1)                                                    
         BZ    BOOK9               INVALID DATE                                 
         GOTO1 VDATCON,APPARM,(0,APWORK),(3,APWORK+6)                           
         CLC   0(2,R8),APWORK+6    COMPARE TO OLD VALUE                         
         BE    BOOK6                                                            
         MVC   0(2,R8),APWORK+6    SAVE BOOK YR/MN                              
         OI    LCHG,LBK                                                         
*                                                                               
BOOK6    LA    R4,SPLBK2H-SPLBK1H(R4)   NEXT BOOK                               
         LA    R8,2(R8)                                                         
         BCT   R0,BOOK2            DO FOR ALL BOOKS                             
         B     BOOKX                                                            
*                                                                               
BOOK9    MVC   FVMSGNO,=AL2(FVIBOOK)   INVALID BOOK                             
*                                                                               
BOOKX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS AND MESSAGES                                            *         
***********************************************************************         
         SPACE 1                                                                
ESUB     MVC   FVMSGNO,=AL2(FVISDPT)                                            
         B     EXIT                                                             
*                                                                               
ENOSLN   MVC   FVMSGNO,=AL2(FVNOSLN)                                            
         B     EXIT                                                             
*                                                                               
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
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
AXTRA    DS    0F                  EXTENSION ROUTINE ADDRESSES                  
AGETDEMS DS    A                                                                
ADEMUP   DS    A                                                                
ASDEMUP  DS    A                                                                
AVALRTG  DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
UMAJBKS  DC    XL4'0205070B'       USA MAJOR BOOKS                              
CMAJBKSA DC    XL3'03070B'         CANADIAN BBM BOOKS                           
CMAJBKSN DC    XL4'0103080B'       CANADIAN CSI BOOKS                           
SPACES   DC    CL80' '                                                          
FUT1     DC    CL10'ENTER=NEXT'                                                 
FUT2     DC    CL8'PF2=TRAN'                                                    
FUT3     DC    CL9'PF12=QUIT'                                                   
*                                                                               
NMAXSTA  EQU   21                  MAX STATIONS                                 
NSTASCR  EQU   7                   STATIONS/SCREEN                              
FF       EQU   X'FF'                                                            
OVERELEM EQU   X'DE'                                                            
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,**B15X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     GETDEMS                                                          
         B     DEMUP                                                            
         B     SDEMUP                                                           
         B     VALRTG                                                           
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE DEMOS FOR THE 4 BOOKS                            *         
***********************************************************************         
         SPACE 1                                                                
GETDEMS  XC    SPDEMLK,SPDEMLK                                                  
         LA    RE,LIUN                                                          
         ST    RE,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOM                                                    
         MVC   SPLKAGY,CUAALF                                                   
         MVC   SPLKMED,CUDMED                                                   
         MVC   SPLKCLI,QCLT                                                     
         MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKSRC,CLTSRC                                                   
         MVC   SPLKUMK,MKTLKUP                                                  
         MVC   SPLKSTA,QSTA                                                     
         MVC   SPLKDAY,BDAYS                                                    
         TM    LFLAG,LDAYOVR       TEST DAYS OVERRIDDEN                         
         BZ    *+10                                                             
         MVC   SPLKDAY,LDAYS                                                    
         MVC   SPLKTIM,BTIMES                                                   
         TM    LFLAG,LTIMOVR       TEST TIMES OVERRIDDEN                        
         BZ    *+10                                                             
         MVC   SPLKTIM,LTIMES                                                   
         MVI   SPLKSVI,X'FF'                                                    
         CLI   SVWKY,C'W'          TEST 4 BOOKS, 1 WEEK                         
         BNE   *+14                                                             
         MVC   SPLKWKN,SVWKY+1     YES-SET WEEK NUMBER                          
         NI    SPLKWKN,X'0F'                                                    
*                                                                               
         LA    R1,G1WPROF                                                       
         ST    R1,SPLKA1W                                                       
*        CLI   CUDMED,C'C'         TEST CANADA                                  
*        BNE   GETD1                                                            
*        TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
*        BZ    GETD1                                                            
*        XC    L1WPROF,L1WPROF     YES-PASS A(1W PROFILE)                       
*        LA    R1,L1WPROF                                                       
*        MVI   3(R1),C'Y'                                                       
*        ST    R1,SPLKA1W                                                       
*                                                                               
GETD1    LA    R1,SVDEM                                                         
         ST    R1,SPLKALST                                                      
         LA    R1,APWORK                                                        
         ST    R1,SPLKAVAL                                                      
         ZIC   R2,SVNMKTS          R2 = NUMBER OF MARKETS                       
         LA    R4,SVMKTS           R4 = A(MARKET LIST)                          
         LA    R9,SVDEMVAL         R9 = A(DEMO VALUE AREA)                      
         LR    RE,R9               CLEAR DEMO VALUE AREA                        
         LA    RF,SVDEMVLL                                                      
         XCEF  ,                                                                
*                                                                               
GETD2    MVC   SPLKSPL,2(R4)       SPILL MARKET                                 
         MVC   SPLKBTYP,4(R4)      BOOK TYPE (IF ANY)                           
         LA    R0,4                                                             
         LA    R8,SVBKS            R8 = A(BOOK LIST)                            
         LA    R3,1                                                             
*                                                                               
GETD4    OC    0(2,R8),0(R8)                                                    
         BZ    GETD6                                                            
         MVC   SPLKDBK,0(R8)                                                    
         TM    SVWKYIND,SVONEBK    TEST 1 BOOK, 4 WEEKS                         
         BZ    *+8                                                              
         STC   R3,SPLKWKN          YES-SET THE WEEK NUMBER                      
         GOTO1 VSPDEMLK,APPARM,(X'FF',SPDEMLK)                                  
         MVC   0(4,R9),APWORK      RATING                                       
*                                                                               
GETD6    LA    R8,2(R8)            NEXT BOOK                                    
         LA    R9,4(R9)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,GETD4                                                         
*                                                                               
         LA    R4,L'SVMKTS(R4)     NEXT MARKET                                  
         BCT   R2,GETD2                                                         
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DEMO UPGRADE ROUTINE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING BWDRECD,R3                                                       
DEMUP    LA    R9,SVPROJ           R9 = A(DEMO VALUE AREA)                      
         LR    RE,R9               CLEAR IT                                     
         LA    RF,SVPROJL                                                       
         XCEF  ,                                                                
         OC    SVUPGRD,SVUPGRD     TEST UPGRADE EXISTS                          
         BZ    DEMUX                                                            
         LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         LA    RE,LIUN                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
         MVC   SPUPSTA,QSTA                                                     
         MVC   SPUPDAY,BWDDAYS                                                  
         TM    LFLAG,LDAYOVR       TEST DAYS OVERRIDDEN                         
         BZ    *+10                                                             
         MVC   SPUPDAY,LDAYS                                                    
         MVC   SPUPTIM,BWDTIMES                                                 
         TM    LFLAG,LTIMOVR       TEST TIMES OVERRIDDEN                        
         BZ    *+10                                                             
         MVC   SPUPTIM,LTIMES                                                   
         MVC   SPUPFIL,SVUPFILE                                                 
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,SVUPFRBK                                                 
         MVC   SPUPFBKL,SVUPFBL                                                 
         MVC   SPUPUDAY,SVUPDAY                                                 
         MVC   SPUPUTIM,SVUPTIM                                                 
         CLI   CUDMED,C'C'         TEST CANADA                                  
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
         MVC   SPUPTYPE(L'SVUPGRD),SVUPGRD                                      
         CLI   SVUPPUT,C'1'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   SVUPPUT,C'2'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   SVUPSHR,C'1'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   SVUPSHR,C'2'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
         ZIC   R2,SVNMKTS          R2 = NUMBER OF MARKETS                       
         LA    R8,SVMKTS           R8 = A(MARKET LIST)                          
*                                                                               
DEMU2    MVC   SPUPSPL,2(R8)       RATING SERVICE SPILL MARKET                  
         MVC   SPUPBTYP,4(R8)      SPECIAL BOOK TYPE (IF ANY)                   
*                                                                               
         CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
         TM    SPUPFBK+1,X'80'     OLYMPIC BOOK?                                
         BZ    *+12                                                             
         MVI   SPUPBTYP,C'O'       YES                                          
         NI    SPUPFBK+1,X'FF'-X'80'                                            
*                                                                               
         ICM   RE,15,LASPIEL       TEST SPILL DEMO ELEMENT                      
         BZ    DEMU14                                                           
         USING SPIEL,RE                                                         
         SR    R0,R0                                                            
*                                                                               
DEMU4    CLC   SPIRMKT,2(R8)       YES-FIND ELEMENT FOR THIS MKT                
         BE    DEMU8                                                            
*                                                                               
DEMU6    IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    DEMU14                                                           
         CLI   0(RE),SPIELCDQ                                                   
         BNE   DEMU6                                                            
         B     DEMU4                                                            
*                                                                               
DEMU8    ZIC   RF,1(RE)            FOUND-LOOK FOR DEMO OVERRIDE                 
         AR    RF,RE                                                            
         BCTR  RF,0                                                             
         LA    R1,SPIDEMO                                                       
         USING SPIDEMO,R1                                                       
         DROP  RE                                                               
         LA    RE,L'SPIDEMO                                                     
*                                                                               
DEMU10   CLC   SPIDEMO+1(2),SVDEM+1                                             
         BNE   DEMU12                                                           
         TM    SPIDEMO+4,SPIDEMOV     TEST FOR OVERRIDE                         
         BZ    DEMU14                                                           
         MVC   0(4,R9),SPIDEMO+4      YES-GRAB VALUE FROM ELEMENT               
         B     DEMU16                                                           
*                                                                               
DEMU12   BXLE  R1,RE,DEMU10                                                     
         DROP  R1                                                               
*                                                                               
DEMU14   DS    0H                                                               
         GOTO1 ASPDEMUP,APPARM,LDMUPBLK,SVDEM,(R9)                              
*                                                                               
DEMU16   LA    R8,L'SVMKTS(R8)      NEXT MARKET                                 
         LA    R9,4(R9)                                                         
         BCT   R2,DEMU2                                                         
*                                                                               
DEMUX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SINGLE SPILL MARKET DEMO UPGRADE ROUTINE                            *         
* INPUT  : LMKT  = SPILL MARKET                                       *         
*        : LBKTYP= BOOK TYPE (IF ANY)                                 *         
* OUTPUT : LDEMVAL= DEMO VALUE                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING BWDRECD,R3                                                       
LSDEMUP  NTR1  ,                   LOCAL ENTRY                                  
*                                                                               
SDEMUP   XC    LDEMVAL,LDEMVAL                                                  
         OC    SVUPGRD,SVUPGRD                                                  
         BZ    SDEMX                                                            
         LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         LA    RE,LIUN                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
         MVC   SPUPSTA,QSTA                                                     
         MVC   SPUPDAY,BWDDAYS                                                  
         MVC   SPUPSPL,LMKT        RATING SERVICE SPILL MARKET                  
         MVC   SPUPBTYP,LBKTYP     SPECIAL BOOK TYPE (IF ANY)                   
*                                                                               
         TM    LFLAG,LDAYOVR       TEST DAYS OVERRIDDEN                         
         BZ    *+10                                                             
         MVC   SPUPDAY,LDAYS                                                    
         MVC   SPUPTIM,BWDTIMES                                                 
         TM    LFLAG,LTIMOVR       TEST TIMES OVERRIDDEN                        
         BZ    *+10                                                             
         MVC   SPUPTIM,LTIMES                                                   
         MVC   SPUPFIL,SVUPFILE                                                 
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,SVUPFRBK                                                 
         MVC   SPUPFBKL,SVUPFBL                                                 
         MVC   SPUPUDAY,SVUPDAY                                                 
         MVC   SPUPUTIM,SVUPTIM                                                 
         CLI   CUDMED,C'C'         TEST CANADA                                  
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
         MVC   SPUPTYPE(L'SVUPGRD),SVUPGRD                                      
         MVC   SPUPBTYP,STABKTYP                                                
*                                                                               
         CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
         TM    SPUPFBK+1,X'80'     OLYMPIC BOOK?                                
         BZ    *+12                                                             
         MVI   SPUPBTYP,C'O'       YES                                          
         NI    SPUPFBK+1,X'FF'-X'80'                                            
*                                                                               
         CLI   SVUPPUT,C'1'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   SVUPPUT,C'2'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   SVUPSHR,C'1'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   SVUPSHR,C'2'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
         GOTO1 ASPDEMUP,APPARM,LDMUPBLK,SVDEM,LDEMVAL                           
*                                                                               
SDEMX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE RATING FIELDS                                              *         
***********************************************************************         
         SPACE 1                                                                
VALRTG   LA    R8,SPLPRJH          SCAN THE RATING FIELDS                       
         LA    R9,SVMKTS                                                        
         LA    R4,SVPROJ                                                        
         MVI   LMKTNUM,1                                                        
*                                                                               
VRTG1    TM    FVIIND-FVIHDR(R8),FVIVAL   TEST RATING FIELD CHANGED             
         BO    VRTG16                                                           
         OI    FVIIND-FVIHDR(R8),FVIVAL   YES-SET PREVIOUSLY VALIDATED          
         XC    LADEMEL,LADEMEL                                                  
         XC    LADEMO,LADEMO                                                    
         ICM   R1,15,LASPIEL       LOOK FOR SPILL ELEMENT FOR THIS MKT          
         BZ    VRTG6                                                            
         SR    R0,R0                                                            
*                                                                               
         USING SPIEL,R1                                                         
VRTG2    CLC   SPIAMKT,0(R9)       MATCH AGENCY MARKET                          
         BE    VRTG4                                                            
*                                                                               
VRTG3    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    VRTG6                                                            
         CLI   0(R1),SPIELCDQ                                                   
         BNE   VRTG3                                                            
         B     VRTG2                                                            
*                                                                               
VRTG4    ST    R1,LADEMEL          SAVE A(ELEMENT)                              
         ZIC   RF,SPIELLN          NOW LOOK FOR DEMO CATEGORY                   
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    RE,L'SPIDEMO                                                     
         LA    R2,SPIDEMO                                                       
         CLC   1(2,R2),SVDEM+1                                                  
         BE    *+12                                                             
         BXLE  R2,RE,*-10                                                       
         B     VRTG6                                                            
         ST    R2,LADEMO           SAVE A(POSITION IN ELEMENT)                  
*                                                                               
VRTG6    LR    R1,R8                                                            
         GOTO1 AFVAL               VALIDATE RATING FIELD                        
         BNE   VRTGX                                                            
         ZIC   RF,FVILEN                                                        
         ST    RF,APPARM+4                                                      
         LA    R1,FVIFLD           REMOVE THE * IF ANY                          
*                                                                               
VRTG8    CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    *+16                YES-REMOVE THE OVERRIDE                      
         LA    R1,1(R1)                                                         
         BCT   RF,VRTG8                                                         
         B     VRTG10                                                           
         MVC   LMKT,2(R9)          RATING SERVICE MARKET                        
         MVC   LBKTYP,4(R9)        BOOK TYPE (IF ANY)                           
         BAS   RE,LSDEMUP          UPGRADE JUST FOR THIS MARKET                 
         B     VRTG12                                                           
*                                                                               
VRTG10   GOTO1 VCASHVAL,APPARM,(1,FVIFLD)  VALIDATE RATING                      
         CLI   APPARM,X'FF'                                                     
         BE    VRTG99                                                           
         MVC   LDEMVAL,APPARM+4    SAVE DEMO VALUE                              
         OI    LDEMVAL,SPIDEMOV    OVERRIDE INDICATOR                           
*                                                                               
VRTG12   OI    LCHG,LRTG           INDICATE CHANGE TO THE RATINGS               
         MVC   0(4,R4),LDEMVAL     MOVE IN NEW RATING                           
         ICM   R1,15,LADEMEL       TEST SPILL ELEMENT EXISTS                    
         BNZ   VRTG14                                                           
         LA    R1,APELEM           NO-CREATE IT                                 
         XC    APELEM,APELEM                                                    
         USING SPIEL,R1                                                         
         MVI   SPIELCD,SPIELCDQ                                                 
         LH    RE,=Y(SPIDEMO-SPIEL)                                             
         LA    RE,L'SPIDEMO(RE)                                                 
         STC   RE,SPIELLN                                                       
         MVC   SPIAMKT,0(R9)       AGENCY MARKET                                
         MVC   SPIRMKT,2(R9)       RATING SERVICE MARKET                        
         MVC   SPIDEMO(3),SVDEM                                                 
         MVC   SPIDEMO+4(4),LDEMVAL                                             
         GOTO1 AADDELS,BWDRECD     ADD                                          
         B     VRTG16                                                           
*                                                                               
VRTG14   ICM   RE,15,LADEMO        TEST DEMO ALEADY IN ELEMENT                  
         BZ    *+14                                                             
         MVC   4(4,RE),LDEMVAL     YES-REPLACE THE VALUE                        
         B     VRTG16                                                           
         XC    APELEM,APELEM       NOT FOUND - ADD DEMO TO ELEMENT              
         ZIC   RF,SPIELLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R1)     SAVE THE ELEMENT                             
         MVI   APELEM,X'FF'                                                     
         MVI   0(R1),X'FF'                                                      
         GOTO1 ADELELS,BWDRECD     DELETE IT FROM RECORD                        
         LA    R1,APELEM                                                        
         MVI   0(R1),SPIELCDQ                                                   
         ZIC   RF,SPIELLN                                                       
         LA    RE,0(R1,RF)                                                      
         MVC   0(3,RE),SVDEM                                                    
         MVC   4(4,RE),LDEMVAL                                                  
         LA    RF,L'SPIDEMO(RF)    LENGTHEN ELEMENT BY ONE DEMO                 
         STC   RF,SPIELLN                                                       
         GOTO1 AADDELS,BWDRECD     ADD BACK THE ELEMENT                         
         DROP  R1                                                               
*                                                                               
VRTG16   LA    R4,4(R4)            NEXT MARKET                                  
         LA    R8,SPLMK2H-SPLMK1H(R8)                                           
         LA    R9,L'SVMKTS(R9)                                                  
         ZIC   R1,LMKTNUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LMKTNUM                                                       
         CLC   LMKTNUM,SVNMKTS                                                  
         BNH   VRTG1                                                            
         B     VRTGX                                                            
*                                                                               
VRTG99   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VRTGX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LOCALD   DSECT                                                                  
VDEMAND  DS    V                                                                
LAUPGEL  DS    A                                                                
LAODTEL  DS    A                                                                
LASPIEL  DS    A                                                                
LADEMEL  DS    A                                                                
LADEMO   DS    A                                                                
LASAVE   DS    A                                                                
LACOST   DS    A                                                                
LDNAME   DS    CL7                                                              
LMKTNUM  DS    X                                                                
LMKT     DS    XL2                                                              
LBKTYP   DS    X                                                                
LDEM     DS    XL4                                                              
LDPT     DS    CL1                                                              
LSVDPTLN DS    CL2                                                              
LDEMVAL  DS    XL4                                                              
LDAYS    DS    XL1                                                              
LTIMES   DS    XL4                                                              
L1WPROF  DS    XL16                                                             
LDMUPBLK DS    (SPDEMUPL)X                                                      
*                                                                               
LCHG     DS    X                                                                
LBK      EQU   X'80'                                                            
LDEMO    EQU   X'40'                                                            
LPRG     EQU   X'20'                                                            
LRTG     EQU   X'10'                                                            
LUPG     EQU   X'08'                                                            
LADJ     EQU   X'04'                                                            
LCOST    EQU   X'02'                                                            
*                                                                               
LFLAG    DS    X                                                                
LGETBKS  EQU   X'80'                                                            
LNEWTIME EQU   X'40'                                                            
LTIMOVR  EQU   X'20'                                                            
LNEWDAYS EQU   X'10'                                                            
LDAYOVR  EQU   X'08'                                                            
         EJECT                                                                  
       ++INCLUDE SPDEMLK                                                        
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
LIUN     DS    2000X                                                            
*                                                                               
LOCALX   EQU   *                                                                
         EJECT                                                                  
SAVED    DSECT                     OVERLAY SAVE AREA DSECT                      
*                                                                               
         DS    0F                                                               
*                                                                               
SVDEMVAL DS    (MAXMKTS*4)XL4                                                   
SVDEMVLL EQU   *-SVDEMVAL                                                       
*                                                                               
SVPROJ   DS    (MAXMKTS)XL4                                                     
SVPROJL  EQU   *-SVPROJ                                                         
*                                                                               
SVNMKTS  DS    X                                                                
SVMKTS   DS    (MAXMKTS)CL5                                                     
SVMKTNMS DS    (MAXMKTS)CL24                                                    
MAXMKTS  EQU   7                                                                
*                                                                               
SVBKS    DS    XL8                                                              
SVWKY    DS    XL2                                                              
SVWKYIND DS    XL1                                                              
SVONEBK  EQU   X'80'                                                            
SVFIRST  DS    C                                                                
SVDEM    DS    XL4                                                              
SVUPFILE DS    C                                                                
SVUPGRD  DS    XL8                                                              
SVUPFRBK DS    XL2                                                              
SVUPFBL  DS    XL6                                                              
SVUPPUT  DS    CL1                                                              
SVUPSHR  DS    CL1                                                              
SVUPINP  DS    CL32                                                             
SVUPDAY  DS    X                                                                
SVUPTIM  DS    XL4                                                              
*                                                                               
         ORG   SAVED+6144                                                       
SAVEX    EQU   *                                                                
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSE5D                                                       
         EJECT                                                                  
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENMKT                                                                      
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENSDEF                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENSDEF                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044SPNWS15A  07/17/02'                                      
         END                                                                    
