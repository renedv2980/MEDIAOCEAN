*          DATA SET SPNWS13    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T20713B,*                                                                
         TITLE 'BWS13 - BUYERS WORK SHEET - SPILL MARKETS SCREEN'               
T20713   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20713**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    R5,LATWA                                                         
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
         L     R6,ATIA             USE TIA FOR SAVED VALUES                     
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
         MVI   APINDS,0                                                         
         XC    BWHKEY,BWHKEY       BUILD BWS HEADER RECORD KEY                  
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
         GOTO1 AVALMED,COMMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         GOTO1 AVALBYR,COMBYRH     VALIDATE BUYER                               
         BNE   VALKX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW         CHECK BUYER'S PASSWORD                       
         BZ    VALK2                                                            
         GOTO1 AVALPWD                                                          
         BNE   VALKX                                                            
*                                                                               
VALK2    GOTO1 AVALCAM,COMCAMH     VALIDATE CAMPAIGN NUMBER                     
         BNE   VALKX                                                            
         MVC   BWHKCAM,BCAM                                                     
         GOTO1 AGETCLT,CMPCLTC     (GET CLIENT)                                 
         BE    *+6                                                              
         DC    H'0'                                                             
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
         GOTO1 AVALSTA,COMSTAH     VALIDATE STATION                             
         BNE   VALKX                                                            
         MVC   BWHKMKT,BMKT        STATION'S MARKET                             
*                                                                               
         GOTO1 AVALDAY,COMDAYH     VALIDATE DAYS                                
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALTIM,COMTIMH     VALIDATE TIMES                               
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALDPL,COMDLNH     VALIDATE DAYPART/LENGTH                      
         BNE   VALKX                                                            
         CLI   CMPDPOPT,C'M'       TEST SUBDPT SCHEDULED UNDER MASTER           
         BNE   *+12                                                             
         CLI   DPTTYPE,C'S'        YES-TEST THIS IS A SUBDAYPART                
         BE    ESUB                YES-ERROR                                    
         CLI   BSLN,0              SPOT LENGTH MUST BE SPECIFIED                
         BE    ENOSLN                                                           
         MVI   LFLAG,0                                                          
         MVI   LFLAG2,0                                                         
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
         LA    R4,BWHSTA           LOOK FOR STATION IN HEADER                   
         LA    R0,BWHMAXST                                                      
         LA    RE,1                RE=STATION CODE                              
*                                                                               
VALK6    OC    0(L'BWHSTA,R4),0(R4)                                             
         BZ    VALK8                                                            
         CLC   QSTA,0(R4)                                                       
         BNE   *+12                                                             
         STC   RE,BSTACD           SET STATION SEQ NO                           
         B     VALK10                                                           
         LA    R4,L'BWHSTA(R4)                                                  
         LA    RE,1(RE)                                                         
         BCT   R0,VALK6                                                         
*                                                                               
VALK8    CLI   APRECNUM,RECSID     STATION NOT FOUND-TEST FOR NSID              
         BNE   VALK99              NO-ERROR                                     
         OI    TWAFLAG,TWANOSTA    SET NO STATION                               
         TM    TWAFLAG,TWAFRST     TEST FIRST TIME                              
         BO    VALK10                                                           
         MVC   0(L'BWHSTA,R4),QSTA   NO-MOVE STATION TO HEADER                  
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
VALK26   OI    APINDS,APIOKDIS     INDICATE RECORD FOUND                        
         XC    LADEMEL,LADEMEL                                                  
         XC    LAUPGEL,LAUPGEL                                                  
         XC    LAODTEL,LAODTEL                                                  
         XC    LASHPEL,LASHPEL                                                  
         L     R3,AIOAREA2                                                      
         SR    R0,R0               LOCATE ELEMENTS IN DETAIL RECORD             
         LA    R8,BWDEL                                                         
*                                                                               
VALK30   CLI   0(R8),0                                                          
         BE    VALK32                                                           
         CLI   0(R8),DMOELCDQ                                                   
         BNE   *+8                                                              
         ST    R8,LADEMEL          A(DEMO ELEMENT)                              
         CLI   0(R8),UPGELCDQ                                                   
         BNE   *+8                                                              
         ST    R8,LAUPGEL          A(UPGRADE ELEMENT)                           
         CLI   0(R8),ODTELCDQ                                                   
         BNE   *+8                                                              
         ST    R8,LAODTEL          A(OVERRIDE ELEMENT)                          
         CLI   0(R8),SHPELCDQ                                                   
         BNE   *+8                                                              
         ST    R8,LASHPEL          A(SHARE/PUT OVERRIDE ELEMENT)                
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     VALK30                                                           
*                                                                               
VALK32   TM    TWAMODE,TWAMDFN     TEST FIRST SCREEN                            
         BZ    VALKX               YES                                          
         BAS   RE,GETSPOVR         NO-GET SHARE/PUT OVERRIDES                   
         B     VALKX                                                            
*                                                                               
VALK99   MVC   FVMSGNO,=AL2(FVFERNF)  RECORD NOT FOUND                          
         LA    R1,COMMEDH                                                       
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
         MVI   TWASTA,FF           INITIALIZE STATION POINTER                   
         SR    R0,R0                                                            
         LA    R4,COMDEMH          TURN PREV VALIDATED BITS ON                  
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
         XC    SVSTA,SVSTA                                                      
         XC    SVDEM,SVDEM                                                      
         MVI   SVRTGSVC,0                                                       
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
FSTR8    MVC   COMUPG,SVUPINP      EDIT UPGRADE EXPRESSION                      
         OI    COMUPGH+6,FVOXMT                                                 
*                                                                               
         BAS   RE,DEMO             INSPECT DEMO FIELD                           
         BNE   FSTRX                                                            
*                                                                               
         OC    MKTLKUP,MKTLKUP     TEST POSSIBILITY OF MARKET OVERRIDE          
         BZ    FSTR10                                                           
         MVC   LDEMLST,SVDEM       YES-CALL SPDEMUP TO GET RTG SERVICE          
         GOTO1 ASDEMUP                                                          
*                                                                               
FSTR10   BAS   RE,WEEKLY           INSPECT WEEEKLY FIELD                        
         BNE   FSTRX                                                            
*                                                                               
         BAS   RE,BOOKS            INSPECT BOOK FIELDS                          
         BNE   FSTRX                                                            
         OC    SVBKS,SVBKS                                                      
         BNZ   *+8                 BOOKS INPUT                                  
         BAS   RE,GETBKS           NONE-GET THE BOOKS                           
         BAS   RE,DISBKS           DISPLAY THE BOOKS                            
*                                                                               
         BAS   RE,PROJPUT          INSPECT PROJECTED PUT VALUE                  
         BNE   FSTRX                                                            
*                                                                               
         MVC   SVSTA(5),QSTA       FIND STATIONS IN THE MARKET                  
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY            READ STATION LIST RECORD                     
         USING CLSRECD,R4                                                       
         MVI   CLSKTYP,CLSKTYPQ                                                 
         MVI   CLSKSUB,CLSKSUBQ                                                 
         MVC   CLSKAGMD,BAGYMD                                                  
         MVC   CLSKMKT,BMKT                                                     
         MVC   CLSKSCHM,CMPPSCHM                                                
         GOTO1 AIO,FILRD3                                                       
         BE    FSTR32                                                           
         MVC   IOKEY,IOKEYSAV      TRY WITHOUT NSID SCHEME                      
         XC    CLSKSCHM,CLSKSCHM                                                
         BASR  RE,RF                                                            
         BNE   FSTR39              NOT FOUND-GET STATIONS FROM DEMAND           
*                                                                               
FSTR32   L     R4,AIOAREA3                                                      
         LA    RE,L'CLSSTA         FOUND - EXTRACT STATIONS FROM                
         ZIC   RF,CLSELLN                  STATION LIST RECORD                  
         LA    RF,CLSEL(RF)                                                     
         BCTR  RF,0                                                             
         LA    R4,CLSSTA                                                        
         LA    R8,SVSTA+7                                                       
         LA    R9,1                R9=STATION COUNT                             
*                                                                               
FSTR34   CLC   QSTA,0(R4)                                                       
         BE    FSTR35                                                           
         MVC   0(5,R8),0(R4)                                                    
         LA    R8,7(R8)                                                         
         LA    R9,1(R9)                                                         
*                                                                               
FSTR35   BXLE  R4,RE,FSTR34                                                     
*                                                                               
         L     R4,AIOAREA3         LOOK FOR SPILL STATIONS                      
         LA    R2,CLSEL                                                         
         SR    R0,R0                                                            
*                                                                               
FSTR36   CLI   0(R2),0                                                          
         BE    FSTR38                                                           
         CLI   0(R2),CSPELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     FSTR36                                                           
         LA    R4,L'CSPSTA         FOUND-EXTRACT SPILL STATIONS                 
         ZIC   R5,1(R2)                                                         
         AR    R5,R2                                                            
         BCTR  R5,0                                                             
         LA    R2,CSPSTA-CSPEL(R2)                                              
*                                                                               
FSTR37   MVC   0(5,R8),0(R2)       SAVE SPILL STATION                           
         MVC   5(2,R8),MKTRS       SAVE RATING SERVICE MARKET NUMBER            
         LA    R8,7(R8)                                                         
         LA    R9,1(R9)                                                         
         BXLE  R2,R4,FSTR37                                                     
         L     R5,ATWA             RESTORE A(TWA)                               
*                                                                               
FSTR38   STC   R9,SVSTANO                                                       
         CLI   SVSTANO,NMAXSTA                                                  
         BNH   FSTR44                                                           
         DC    H'0'                BLOW IF TOO MANY STATIONS                    
*                                                                               
FSTR39   XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIOAREA3                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBSELMED,CUDMED                                                  
         MVC   DBSELSRC,CLTSRC                                                  
         CLI   SVRTGSVC,0                                                       
         BE    *+10                                                             
         MVC   DBSELSRC,SVRTGSVC                                                
         MVC   DBSELSTA,QSTA                                                    
         LA    R0,4                LATEST BOOK                                  
         LA    R1,SVBKS+6                                                       
*                                                                               
FSTR40   OC    0(2,R1),0(R1)                                                    
         BNZ   FSTR42                                                           
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         BCT   R0,FSTR40                                                        
         LA    R1,=X'570B'                                                      
*                                                                               
FSTR42   MVC   DBSELBK,0(R1)                                                    
         MVI   DBFUNCT,DBGETMK     GET THE RATING SERVICE MARKET                
         GOTO1 VDEMAND,APPARM,DBLOCK,0,0                                        
         OC    DBACTRMK,DBACTRMK                                                
         BZ    FSTR99                                                           
         MVI   DBFUNCT,DBGETMS     GET THE STATIONS                             
         MVC   DBSELRMK,DBACTRMK                                                
         MVI   SVSTANO,1                                                        
         GOTO1 VDEMAND,APPARM,DBLOCK,STAHOOK,0                                  
*                                                                               
FSTR44   DS    0H                                                               
         BAS   RE,GETPJPUT         GET PROJECTED PUT VALUE                      
         GOTO1 ADEMUP              DO THE UPGRADES                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    LFLAG,LPUTADJ       TEST PUT WAS ADJUSTED                        
         BZ    *+8                                                              
         BAS   RE,GETPJPUT         YES-FORMAT PUT VALUE AGAIN                   
         GOTO1 AGETPUTS            GET THE PUTS AND FORMAT THEM                 
         GOTO1 AGETDEMS            GET THE RATING/SHARE VALUES                  
*                                                                               
         OI    COMFUTH+6,FVOXMT    FORMAT THE FOOTLINE                          
         XC    COMFUT,COMFUT                                                    
         MVC   COMFUT(L'FUT1),FUT1                                              
         LA    R4,COMFUT+L'FUT1+1                                               
         CLI   APRECNUM,RECSID                                                  
         BNE   *+14                                                             
         MVC   0(L'FUT2,R4),FUT2                                                
         LA    R4,L'FUT2+1(R4)                                                  
         TM    TWAMODE,TWAMLSM                                                  
         BZ    FSTRX                                                            
         MVC   0(L'FUT3,R4),FUT3                                                
         B     FSTRX                                                            
*                                                                               
FSTR99   MVC   FVMSGNO,=AL2(FVNODEMO)   NO DEMO INFO AVAILABLE                  
*                                                                               
FSTRX    GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),SAVED                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING MARKET STATIONS                          *         
***********************************************************************         
         SPACE 1                                                                
STAHOOK  L     R4,DBAREC                                                        
         USING MLKEY,R4                                                         
         OC    MLKMKT,MLKMKT       TEST SPILL MARKET                            
         BNZ   STAHX               YES - IGNORE                                 
         TM    MLSTAT,X'F0'        TEST STATION NUMERIC                         
         BO    STAHX               YES - IGNORE                                 
         CLC   QSTA,MLSTAT         TEST STATION = REQUEST STATION               
         BE    STAHX               YES - IGNORE (ALREADY FIRST IN LIST)         
         CLI   SVSTANO,NMAXSTA                                                  
         BL    *+6                                                              
         DC    H'0'                BLOW IF TOO MANY STATIONS                    
         ZIC   R1,SVSTANO                                                       
         LR    RF,R1                                                            
         MH    RF,=H'7'                                                         
         LA    RF,SVSTA(RF)                                                     
         MVC   0(5,RF),MLSTAT                                                   
         LA    R1,1(R1)                                                         
         STC   R1,SVSTANO                                                       
STAHX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   MVC   LSVDPTLN,BDPT       SAVE DAYPART/LENGTH                          
         CLI   APRECNUM,RECSID     TEST NSID RECORD                             
         BNE   *+16                                                             
         TM    TWAFLAG,TWAFEST     YES-TEST SWAPPED FROM EST SCREEN             
         BO    *+8                                                              
         OI    TWAFLAG,TWAFRST     NO-FLAG FIRST TIME                           
*                                                                               
         CLI   APRECNUM,RECSID     TEST NSID                                    
         BE    DISK2                                                            
         MVC   IOKEY(13),APRECKEY  NO-GET THE RECORD                            
         GOTO1 AMIN,MINRD2                                                      
         BNE   DISKX                                                            
         L     R3,AIOAREA2                                                      
         B     DISK14                                                           
*                                                                               
DISK2    LA    R8,APRECKEY         NSID                                         
         USING NSIDKEYD,R8                                                      
         GOTO1 AGETMED,NSAGYMD                                                  
         BNE   *+10                                                             
         MVC   COMMED,QMED         MEDIA                                        
         MVI   LDPT,0                                                           
         SR    R0,R0                                                            
         L     R6,LASAVE                                                        
         USING SAVAREA,R6                                                       
         LA    R4,SAVKEYS          EXTRACT BUYER CAMPAIGN AND DAYPART           
*                                  FROM SAVED KEY                               
DISK4    CLI   0(R4),0                                                          
         BE    DISK10                                                           
         LA    RF,COMBYR                                                        
         CLI   0(R4),KEYBYR        BUYER                                        
         BE    DISK6                                                            
         LA    RF,COMCAM                                                        
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
         GOTO1 VMSUNPK,APPARM,APDUB,APWORK,APWORK+4                             
         MVC   COMSTA(5),APWORK+4  STATION                                      
         CLI   COMSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   COMSTA+4,C' '                                                    
         GOTO1 AGETDAY,NSDAY       DAYS                                         
         MVC   COMDAY,QDAYS                                                     
         GOTO1 AGETTIM,NSTIME      TIMES                                        
         MVC   COMTIM,QTIMES                                                    
         XC    COMSUB,COMSUB                                                    
         MVI   COMSDP,0                                                         
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
         MVC   COMSDP,NSDPT                                                     
         MVC   COMSUB(10),=C'Subdaypart'                                        
         MVC   COMDLN(1),LDPT                                                   
         ZIC   RF,NSSLN            SPOT LENGTH                                  
         BAS   RE,DISLEN                                                        
         B     DISKX                                                            
*                                                                               
         USING BWDRECD,R3                                                       
DISK14   GOTO1 AGETMED,BWDKAGMD    BWS RECORD                                   
         BNE   *+10                                                             
         MVC   COMMED,QMED         MEDIA                                        
         LA    R1,BWDKBYR          BUYER                                        
         ICM   R1,8,=X'B'                                                       
         GOTO1 AGETBYR                                                          
         MVC   COMBYR,QBYR                                                      
         GOTO1 AGETCM,BWDKSEQ      CAMPAIGN                                     
         MVC   COMCAM,QCAM                                                      
         MVC   COMSTA(L'BWDSTA),BWDSTA   STATION                                
         CLI   COMSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   COMSTA+4,C' '                                                    
         MVC   COMDLN(1),BWDDPT    DAYPART                                      
         GOTO1 AGETDAY,BWDDAYS     DAYS                                         
         MVC   COMDAY,QDAYS                                                     
         GOTO1 AGETTIM,BWDTIMES    TIMES                                        
         MVC   COMTIM,QTIMES                                                    
         XC    COMSUB,COMSUB                                                    
         MVI   COMSDP,0                                                         
         CLI   BWDSUBDP,0          TEST SUBDAYPART                              
         BE    *+16                                                             
         MVC   COMSDP,BWDSUBDP                                                  
         MVC   COMSUB(10),=C'Subdaypart'                                        
         ZIC   RF,BWDSLN                                                        
         BAS   RE,DISLEN                                                        
*                                                                               
DISKX    MVC   BDPT(2),LSVDPTLN    RESTORE DAYPART/LENGTH                       
         B     EXIT                                                             
         SPACE  2                                                               
DISLEN   CVD   RF,APDUB            DISPLAY SPOT LENGTH                          
         OI    APDUB+7,X'0F'                                                    
         LA    R1,COMDLN+1                                                      
         CLI   COMDLN,C'1'                                                      
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
DISREC   GOTO1 VDMGR,APPARM,DMREAD,TEMPSTR,(4,0),SAVED                          
*                                                                               
         TM    TWAFLAG,TWAFRET     TEST RETURN FROM ESTIMATE SCREEN             
         BZ    *+12                                                             
         NI    TWAFLAG,FF-TWAFRET  YES-JUST EXIT                                
         B     DISRX                                                            
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
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
DISR2    CLI   TWASTA,FF           TEST NOT VERY FIRST TIME                     
         BE    DISR8                                                            
         CLI   APPFKEY,PFK01       TEST PF1                                     
         BNE   DISR4                                                            
         MVI   APPFKEY,0                                                        
         TM    TWAFLAG,TWAFEST     YES - TEST COME FROM ESTIMATE SCRN           
         BZ    DISR6                                                            
         BAS   RE,RESEST                 YES - RESTORE ESTIMATE SCREEN          
         B     DISRX2                                                           
*                                                                               
DISR4    CLI   APPFKEY,PFK02       TEST PF2                                     
         BNE   DISR8                                                            
         CLI   APRECNUM,RECSID     YES-TEST SID RECORD                          
         BE    DISR8                                                            
         MVI   APPFKEY,0           NO-IGNORE                                    
         B     DISR8                                                            
*                                                                               
         SPACE 1                                                                
* SWAP TO ESTIMATE SCREEN                                                       
*                                                                               
DISR6    GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(3,0),TWAD                          
         MVI   APMODE,APMSWP                                                    
         OI    TWAFLAG,TWAFCOM                                                  
         B     DISRX                                                            
         SPACE 1                                                                
*                                                                               
DISR8    L     R3,AIOAREA2         INSPECT DAYS FIELD                           
         USING BWDRECD,R3                                                       
         MVI   LFLAG2,0                                                         
         MVC   APHALF,BDPT                                                      
         MVC   APFULL,BTIMES                                                    
         MVC   APBYTE,BDAYS                                                     
         GOTO1 AVALDAY,COMDAYH                                                  
         BNE   DISRX                                                            
         MVC   BDPT(2),APHALF                                                   
         MVC   BTIMES,APFULL                                                    
         CLC   BDAYS,BWDDAYS       TEST DAYS = THE RECORD'S DAYS                
         BE    *+8                                                              
         OI    LFLAG2,LDAYOVR      NO-INDICATE DAYS ARE OVERRIDDEN              
         TM    COMDAYH+FVIIND-FVIHDR,FVIVAL    TEST DAYS FIELD CHANGED          
         BO    *+12                                                             
         OI    COMDAYH+FVIIND-FVIHDR,FVIVAL    YES -                            
         OI    LFLAG2,LNEWDAYS     INDICATE DAYS CHANGED THIS TIME              
         MVC   LDAYS,BDAYS         SAVE THE DAYS                                
         MVC   BDAYS,APBYTE                                                     
*                                                                               
         MVC   APHALF,BDPT         INSPECT TIMES FIELD                          
         MVC   APFULL,BTIMES                                                    
         GOTO1 AVALTIM,COMTIMH                                                  
         BNE   DISRX                                                            
         MVC   BDPT(2),APHALF                                                   
         CLC   BTIMES,BWDTIMES     TEST TIMES = THE RECORD'S TIMES              
         BE    *+8                                                              
         OI    LFLAG2,LTIMOVR      NO-INDICATE TIMES ARE OVERRIDDEN             
         TM    COMTIMH+FVIIND-FVIHDR,FVIVAL    TEST TIMES FIELD CHANGED         
         BO    *+12                                                             
         OI    COMTIMH+FVIIND-FVIHDR,FVIVAL    YES -                            
         OI    LFLAG2,LNEWTIME     INDICATE TIMES CHANGED THIS TIME             
         MVC   LTIMES,BTIMES       SAVE THE TIMES                               
         MVC   BTIMES,APFULL                                                    
*                                                                               
DISR9    TM    COMDEMH+FVIIND-FVIHDR,FVIVAL    TEST DEMO FIELD CHANGED          
         BO    DISR10                                                           
         OI    COMDEMH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BAS   RE,DEMO                         INSPECT DEMO FIELD               
         BNE   DISRX                                                            
*                                                                               
DISR10   TM    COMWKYH+FVIIND-FVIHDR,FVIVAL    TEST WEEKLY FIELD CHANGE         
         BO    DISR11                                                           
         OI    COMWKYH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BAS   RE,WEEKLY                       INSPECT WEEKLY FIELD             
         BNE   DISRX                                                            
*                                                                               
DISR11   TM    COMBK1H+FVIIND-FVIHDR,FVIVAL    TEST ANY BOOK CHANGES            
         BZ    DISR12                                                           
         TM    COMBK2H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR12                                                           
         TM    COMBK3H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR12                                                           
         TM    COMBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR12                                                           
         TM    LFLAG,LGETBKS       NO-TEST NEED TO GET BOOKS AGAIN              
         BZ    DISR14                                                           
         BAS   RE,GETBKS           YES-GET THE BOOKS                            
         BAS   RE,DISBKS               AND DISPLAY THEM                         
         B     DISR14                                                           
*                                                                               
DISR12   OI    COMBK1H+FVIIND-FVIHDR,FVIVAL                                     
         OI    COMBK2H+FVIIND-FVIHDR,FVIVAL                                     
         OI    COMBK3H+FVIIND-FVIHDR,FVIVAL                                     
         OI    COMBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,BOOKS            INSPECT THE BOOK FIELDS                      
         BNE   DISRX                                                            
         TM    SVWKYIND,SVONEBK    TEST SINGLE BOOK, 4 WEEKS                    
         BZ    DISR14                                                           
         BAS   RE,DISBKS           YES-MAKE SURE BOOKS ARE DISPLAYED            
*                                                                               
DISR14   TM    COMPPTH+FVIIND-FVIHDR,FVIVAL   TEST PROJECTED PUT CHANGE         
         BO    DISR15                                                           
         OI    COMPPTH+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,PROJPUT          YES-INSPECT PROJECTED PUT VALUE              
         BNE   DISRX                                                            
*                                                                               
DISR15   TM    COMPRGH+FVIIND-FVIHDR,FVIVAL   INSPECT PROGRAM NAME FLD          
         BO    DISR18                                                           
         OI    COMPRGH+FVIIND-FVIHDR,FVIVAL                                     
         CLI   TWASTA,0                       ONLY ACCEPT PROG CHANGE           
         BNE   DISR18                         ON FIRST DISPLAYED SCREEN         
         GOTO1 AFVAL,COMPRGH                  VALIDATE PROGRAM NAME             
         BH    DISRX                                                            
         CLI   FVIFLD,C'='         TEST PROGRAM ADJACENCY CODE                  
         BNE   DISR16                                                           
         GOTO1 AVALADJ,FVIFLD+1    YES-VALIDATE IT                              
         BNE   DISRX                                                            
         CLC   ADJCODE,BWDADJ      VALID-TEST CHANGE                            
         BE    DISR18                                                           
         MVC   BWDADJ,ADJCODE      YES-SET ADJACENCY CODE                       
         OI    LCHG,LADJ               INDICATE CHANGE                          
         B     DISR18                                                           
*                                                                               
DISR16   CLC   BWDPROG,FVIFLD      TEST PROGRAM CHANGE                          
         BE    DISR18                                                           
         MVC   BWDPROG,FVIFLD      YES -                                        
         OI    BWDINDS,BWDIPRG                                                  
         MVC   SVPJPRGS(L'L1PJPROG),FVIFLD                                      
         OI    LCHG,LPRG                                                        
*                                                                               
DISR18   TM    COMRSHH+FVIIND-FVIHDR,FVIVAL   INSPECT RTG/SHR FIELD             
         BO    DISR20                                                           
         OI    COMRSHH+FVIIND-FVIHDR,FVIVAL                                     
         CLI   TWASTA,FF           FOR VERY FIRST SCREEN, SKIP TO               
         BE    DISR20              DISPLAY LOGIC                                
         CLI   TWASTA,0            ONLY ACCEPT RTG/SHR CHANGE ON                
         BNE   DISR20              FIRST DISPLAYED SCREEN                       
         TM    LCHG,LDEMO          TEST DEMO CHANGE                             
         BO    DISR20              YES-IGNORE RTG/SHR CHANGE                    
*                                                                               
         GOTO1 AVALRSH             VALIDATE RATING/SHARE FIELD                  
         BNE   DISRX                                                            
*                                                                               
DISR20   TM    LCHG,LSHR           TEST SHARE CHANGE                            
         BZ    DISR21                                                           
         SR    RE,RE                                                            
         ICM   RE,7,SVPROJ+5       YES - CALCULATE NEW PROJ CUM SHARE           
         SR    RF,RF                                                            
         ICM   RF,7,LOLDSHR+1                                                   
         SR    RE,RF                                                            
         A     RE,SVPJCUM                                                       
         ST    RE,SVPJCUM                                                       
         GOTO1 AFMTPJCM                  FORMAT PROJ CUM SHARE                  
*                                                                               
DISR21   CLI   TWASTA,FF           SKIP TO DISPLAY FOR VERY FIRST SCRN          
         BE    DISR24                                                           
         TM    LCHG,LDEMO+LBK      TEST DEMO OR BOOK CHANGES                    
         BNZ   *+12                                                             
         TM    LFLAG2,LNEWTIME+LNEWDAYS OR DAYS/TIMES CHANGED THIS TIME         
         BZ    DISR23                                                           
         GOTO1 AGETPUTS            YES - GET PUTS                               
         GOTO1 AGETDEMS                  GET RATINGS AND SHARES                 
         OC    SVWKY,SVWKY         TEST WEEKLY DEMO LOOKUPS                     
         BZ    DISR23                                                           
         BAS   RE,DISBKS           YES-DISPLAY THE BOOKS WITH WEEK NOS          
*                                                                               
DISR23   TM    LCHG,LDEMO+LPUT     TEST DEMO OR PROJECTED PUT CHANGE            
         BNZ   *+12                                                             
         TM    LFLAG2,LNEWTIME+LNEWDAYS OR DAYS/TIMES CHANGED THIS TIME         
         BZ    DISR24                                                           
         GOTO1 ADEMUP              YES - DO THE UPGRADES                        
         BAS   RE,GETPJPUT         GET AND FORMAT PROJECTED PUT VALUE           
*                                                                               
DISR24   BAS   RE,DISSRC           DISPLAY RATING SERVICE                       
         CLI   TWASTA,FF           TEST FOR VERY FIRST SCREEN                   
         BNE   *+12                                                             
         MVI   TWASTA,0            YES                                          
         B     DISR26                                                           
         CLI   APPFKEY,PFK02       TEST SID TRANFER                             
         BE    DISR52              YES - DO NOT RE-DISPLAY SCREEN               
         TM    LCHG,LDEMO+LBK+LRTG+LSHR+LPUT   TEST SCREEN CHANGES              
         BNZ   DISR26                   YES- RE-DISPLAY CURRENT SCREEN          
         TM    LFLAG2,LNEWTIME+LNEWDAYS SAME FOR DAYS/TIMES CHANGED             
         BNZ   DISR26                                                           
         ZIC   RE,TWASTA                NO - DISPLAY NEXT SCREEN                
         LA    RF,NSTASCR                                                       
         AR    RE,RF                                                            
         STC   RE,TWASTA                                                        
         CLC   TWASTA,SVSTANO      TEST ALL SCREENS DISPLAYED                   
         BL    DISR26                                                           
         MVI   TWASTA,0            YES - DISPLAY FROM FIRST SCREEN              
         MVI   APMODE,APMLRP             TELL CONTROLLER                        
         TM    TWAFLAG,TWAFEST           TEST COME FROM ESTIMATE SCREEN         
         BZ    DISR26                                                           
         BAS   RE,RESEST                 YES - RESTORE ESTIMATE SCREEN          
         B     DISRX2                                                           
*                                                                               
DISR26   SR    RE,RE               CLEAR THE SCREEN                             
         LA    R4,COMLN1H                                                       
         LA    R8,COMCUSH                                                       
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
         OI    COMPRGH+FVATRB-FVIHDR,FVAHIGH    SHOW HIGH INTENSITY             
         OI    COMRSHH+FVATRB-FVIHDR,FVAHIGH    FOR FIRST SCREEN                
         CLI   TWASTA,0                                                         
         BE    *+12                                                             
         NI    COMPRGH+FVATRB-FVIHDR,FF-FVAHIGH                                 
         NI    COMRSHH+FVATRB-FVIHDR,FF-FVAHIGH                                 
*                                                                               
         LA    R0,NSTASCR          FORMAT STATIONS                              
         ZIC   RE,TWASTA                                                        
         ZIC   RF,SVSTANO                                                       
         SR    RF,RE                                                            
         CR    R0,RF               TEST FOR A FULL SCREEN                       
         BNH   *+6                                                              
         LR    R0,RF               NO                                           
         STC   R0,LNSTASCR         NUMBER OF STATIONS ON THIS SCREEN            
         MH    RE,=H'7'                                                         
         LA    RE,SVSTA(RE)                                                     
         LA    R4,COMLN1H                                                       
         LA    R8,COMLN1                                                        
         USING LINE1D,R8                                                        
         LA    R9,COMLN2H                                                       
*                                                                               
DISR32   OC    0(5,RE),0(RE)                                                    
         BZ    DISR38                                                           
         MVC   L1STA(4),0(RE)                                                   
         MVC   L1STA+4(3),=C'-TV'                                               
         CLI   4(RE),C'T'                                                       
         BE    *+14                                                             
         MVC   L1STA+5(1),4(RE)                                                 
         MVI   L1STA+6,C'M'                                                     
         CLI   3(RE),C' '                                                       
         BNE   *+14                                                             
         MVC   L1STA+3(3),L1STA+4                                               
         MVI   L1STA+6,C' '                                                     
         OI    6(R4),FVOXMT        TRANSMIT BOTH STATION LINES                  
         OI    6(R9),FVOXMT                                                     
         CLM   R0,1,LNSTASCR                                                    
         BNE   DISR34                                                           
         LA    R4,COMLN3H                                                       
         LA    R8,COMLN3                                                        
         LA    R9,COMLN4H                                                       
         B     DISR36                                                           
*                                                                               
DISR34   LA    R4,COMLN5H-COMLN3H(R4)                                           
         LA    R8,COMLN5H-COMLN3H(R8)                                           
         LA    R9,COMLN5H-COMLN3H(R9)                                           
*                                                                               
DISR36   LA    RE,7(RE)                                                         
         BCT   R0,DISR32                                                        
         DROP  R8                                                               
*                                                                               
DISR38   ZIC   R4,TWASTA           FORMAT PROGRAM NAMES                         
         MH    R4,=Y(4*L'L1PROG1)                                               
         LA    R4,SVPROGS(R4)                                                   
         ZIC   R8,TWASTA                                                        
         MH    R8,=Y(L'L1PJPROG)                                                
         LA    R8,SVPJPRGS(R8)                                                  
         LA    R9,COMLN1                                                        
         USING LINE1D,R9                                                        
         ZIC   RF,LNSTASCR                                                      
*                                                                               
DISR40   LA    RE,L1PROG1                                                       
         LA    R0,4                                                             
*                                                                               
DISR42   MVC   0(L'L1PROG1,RE),0(R4)                                            
         LA    R4,L'L1PROG1(R4)                                                 
         LA    RE,L1PROG2-L1PROG1(RE)                                           
         BCT   R0,DISR42           DO FOR ALL BOOKS                             
*                                                                               
         CLM   RF,1,LNSTASCR                                                    
         BE    *+18                                                             
         MVC   L1PJPROG,0(R8)      PROJ PROGRAM NAME                            
         LA    R9,COMLN5-COMLN3(R9)                                             
         B     DISR44                                                           
         MVC   COMPRG(L'L1PJPROG),0(R8)                                         
         OI    COMPRGH+6,FVOXMT                                                 
         LA    R9,COMLN3                                                        
*                                                                               
DISR44   LA    R8,L'L1PJPROG(R8)                                                
         BCT   RF,DISR40           DO FOR ALL STATIONS                          
*                                                                               
         XC    EBLOCK,EBLOCK       FORMAT RATINGS AND SHARES                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         ZIC   R4,TWASTA                                                        
         SLL   R4,5                                                             
         LA    R4,SVDEMVAL(R4)     R4 = A(RTG/SHR)                              
         ZIC   R9,TWASTA                                                        
         SLL   R9,3                                                             
         LA    R9,SVPROJ(R9)       R9 = A(PROJ RTG/SHR)                         
         ZIC   RF,LNSTASCR                                                      
         LA    R2,COMLN2                                                        
         USING LINE2D,R2                                                        
*                                                                               
DISR46   LA    R8,L2RS1                                                         
         LA    R0,4                                                             
*                                                                               
DISR48   XC    0(L'L2RS1,R8),0(R8)                                              
         BAS   RE,FMTRS            FORMAT RATING/SHARE ROUTINE                  
         LA    R8,L2RS2-L2RS1(R8)                                               
         LA    R4,8(R4)                                                         
         BCT   R0,DISR48           DO FOR ALL BOOKS                             
*                                                                               
         CLM   RF,1,LNSTASCR                                                    
         BE    *+18                                                             
         XC    L2PJRS,L2PJRS                                                    
         LA    R8,L2PJRS                                                        
         B     DISR50                                                           
         XC    COMRSH,COMRSH                                                    
         OI    COMRSHH+6,FVOXMT                                                 
         LA    R8,COMRSH                                                        
*                                                                               
DISR50   ST    R4,APFULL                                                        
         LR    R4,R9                                                            
         BAS   RE,FMTRS            FORMAT PROJ RTG/SHR                          
         L     R4,APFULL                                                        
         LA    R9,8(R9)                                                         
         CLM   RF,1,LNSTASCR                                                    
         BNE   *+12                                                             
         LA    R2,COMLN4                                                        
         B     *+8                                                              
         LA    R2,COMLN5-COMLN3(R2)                                             
         BCT   RF,DISR46           DO FOR ALL STATIONS                          
*                                                                               
DISR52   CLI   APRECNUM,RECSID     TEST SID RECORD                              
         BNE   DISR54                                                           
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
         MVI   TWAFLAG,0                                                        
         MVI   APMODE,APMLRP                                                    
         B     DISRX                                                            
*                                                                               
DISR54   TM    LCHG,LRTG+LSHR+LPUT+LPRG+LADJ  TEST RECORD CHANGE                
         BZ    DISR56                                                           
         MVC   IOKEY(13),APRECKEY                                               
         GOTO1 AMIN,MINWRT2                                                     
         BE    DISR56                                                           
         DC    H'0'                                                             
*                                                                               
DISR56   B     DISRX                                                            
*                                                                               
DISRX    GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),SAVED                         
*                                                                               
DISRX2   TM    TWAMODE,TWAMLSM     TEST LIST/SELECT ACTIVE                      
         BZ    *+8                                                              
         MVI   TWALSACT,ACTCOM     YES-LAST SELECT ACTION = COMP                
         CLI   APMODE,APMSWP                                                    
         BE    *+12                                                             
         CLI   APMODE,APMRET                                                    
         BNE   EXIT                                                             
         MVI   APPARM,RECWRK       FOR SWAP, RECORD = WORK                      
         MVI   APPARM+1,ACTEST               ACTION = ESTIMATE                  
         B     EXIT                                                             
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
         MVC   APBYTE,SVRTGSVC                                                  
         CLI   APBYTE,0                                                         
         BNE   *+10                                                             
         MVC   APBYTE,CLTSRC                                                    
         OI    COMSRCH+6,FVOXMT                                                 
         MVC   COMSRC,=C'NSI'                                                   
         CLI   APBYTE,C'N'                                                      
         BE    *+10                                                             
         MVC   COMSRC,=C'ARB'                                                   
         CLI   APROF7,C'C'                                                      
         BNE   DISSRCX                                                          
         MVC   COMSRC,=C'CSI'                                                   
         CLI   APBYTE,C'N'                                                      
         BE    DISSRCX                                                          
         MVC   COMSRC,=C'BBM'                                                   
*                                                                               
DISSRCX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE BOOKS                                            *         
***********************************************************************         
         SPACE 1                                                                
GETBKS   NTR1                                                                   
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
         CLI   SVRTGSVC,0                                                       
         BE    *+10                                                             
         MVC   DBSELSRC,SVRTGSVC                                                
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
         LA    R8,COMBK1H                                                       
         LA    R9,C'1'                                                          
         MVI   APWORK+2,1                                                       
*                                                                               
DISB2    XC    8(L'COMBK1,R8),8(R8)                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    DISB4                                                            
         MVC   APWORK(2),0(R4)                                                  
         GOTO1 VDATCON,APPARM,(3,APWORK),(6,8(R8))                              
         OC    SVWKY,SVWKY         TEST WEEKLY OPTION                           
         BZ    DISB4                                                            
         MVC   11(2,R8),12(R8)     YES-                                         
         MVI   13(R8),C'-'         1 BOOK, 4 WEEKS                              
         STC   R9,14(R8)                                                        
         CLI   SVWKY,C'W'                                                       
         BNE   DISB4                                                            
         MVC   14(1,R8),SVWKY+1    4 BOOKS, 1 WEEK                              
*                                                                               
DISB4    OI    6(R8),FVOXMT                                                     
         LA    R4,2(R4)                                                         
         LA    R8,COMBK2H-COMBK1H(R8)                                           
         LA    R9,1(R9)                                                         
         BCT   R0,DISB2                                                         
*                                                                               
DISBX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT RATING/SHARE                                      *         
* INPUT  : R4 = A(RTG/SHR)                                            *         
*          R8 = A(FORMAT AREA)                                        *         
***********************************************************************         
         SPACE 1                                                                
FMTRS    NTR1  ,                                                                
         MVC   APDUB,0(R4)         APDUB=RATING/SHARE                           
         LA    R9,APDUB                                                         
         NI    0(R9),255-X'80'                                                  
         NI    4(R9),255-X'80'                                                  
         LA    RE,L'COMRSH                                                      
         CLI   CLTBWPRO+15,C'Y'    TEST SUPPRESS SHARE                          
         BE    FMTR5               YES-RATING TAKES UP WHOLE FIELD              
         LA    RE,4                RE=L'SHARE FIELD                             
         TM    4(R4),X'80'                                                      
         BO    FMTR2                                                            
         CLC   4(4,R9),=F'100'                                                  
         BNL   FMTR4                                                            
         BCTR  RE,0                                                             
         B     FMTR4                                                            
*                                                                               
FMTR2    CLC   4(4,R9),=F'100'                                                  
         BL    FMTR4                                                            
         LA    RE,1(RE)                                                         
*                                                                               
FMTR4    STC   RE,APBYTE           APBYTE=L'SHARE FIELD                         
         LNR   RE,RE                                                            
         LA    RF,L'COMRSH-1                                                    
         AR    RE,RF                                                            
*                                                                               
FMTR5    STC   RE,EBLOUT           FORMAT THE RATING                            
         OC    0(4,R9),0(R9)                                                    
         BNZ   FMTR6                                                            
         SH    RE,=H'3'                                                         
         AR    RE,R8                                                            
         MVC   0(3,RE),=C'0.0'                                                  
         B     FMTR8                                                            
*                                                                               
FMTR6    MVI   EBFLOAT,0                                                        
         TM    0(R4),X'80'         TEST OVERRIDE                                
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         ST    R8,EBAOUT                                                        
         LA    R1,APDUB                                                         
         ST    R1,EBAIN                                                         
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
FMTR8    CLI   CLTBWPRO+15,C'Y'    TEST SUPPRESS SHARES                         
         BE    FMTRX                                                            
         ZIC   RE,EBLOUT           NO-FORMAT THE SHARE                          
         AR    R8,RE                                                            
         MVI   0(R8),C'/'                                                       
         LA    R8,1(R8)                                                         
         ST    R8,EBAOUT                                                        
         LA    R1,4(R9)                                                         
         ST    R1,EBAIN                                                         
         OC    4(4,R9),4(R9)                                                    
         BNZ   *+14                                                             
         MVC   0(3,R8),=C'0.0'                                                  
         B     FMTRX                                                            
         MVC   EBLOUT,APBYTE                                                    
         MVI   EBFLOAT,0                                                        
         TM    4(R4),X'80'                                                      
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
FMTRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RESTORE THE ESTIMATE SCREEN                                         *         
***********************************************************************         
         SPACE 1                                                                
RESEST   NTR1                                                                   
         GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),SAVED  SAVE TIA               
         NI    TWAFLAG,FF-TWAFEST                                               
         L     R4,ATIA                                                          
         GOTO1 VDMGR,APPARM,DMREAD,TEMPSTR,(3,0),(R4)                           
         LA    R0,BWSLAST                                                       
         LA    RE,BWSLAST-TWAD(R4)                                              
         LH    R1,=Y(TWASVLEN)                                                  
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R1,BWSMSGH          TRANSMIT THE SCREEN                          
         LH    RF,=Y(TWASVLEN)                                                  
         LA    RF,BWSLAST(RF)                                                   
         SR    RE,RE                                                            
         OI    6(R1),FVOXMT                                                     
         ICM   RE,1,0(R1)                                                       
         BZ    *+10                                                             
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                                                             
         MVI   1(R1),1                                                          
         MVI   2(R1),1                                                          
         MVI   APMODE,APMRET       PASS RETURN MODE                             
         OI    TWAFLAG,TWAFRET     RETURN BIT                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT THE DEMO FIELD                                              *         
* OUTPUT : LCHG = LDEMO IF THE DEMO CHANGES                           *         
*          CC EQ OK                                                   *         
*             NE ERROR                                                *         
***********************************************************************         
         SPACE 1                                                                
DEMO     NTR1                                                                   
         CLI   TWASTA,FF                                                        
         BNE   DEMO2                                                            
         TM    TWAFLAG,TWAFEST     TEST COME FROM ESTIMATE SCREEN               
         BZ    DEMO2                                                            
         MVC   SVDEM,TWADEMO       YES-SET DEMO FROM SAVED DEMO CODE            
         MVI   SVDEM+3,FF                                                       
         OI    LCHG,LDEMO                                                       
         B     DEMO8                                                            
*                                                                               
DEMO2    GOTO1 AFVAL,COMDEMH       VALIDATE THE DEMO FIELD                      
         BH    DEMOX                                                            
         BE    DEMO6                                                            
         OC    INORTG,INORTG       MISSING-TEST OVERRIDE TARGET DEMO            
         BZ    DEMO4               NO                                           
         CLC   SVDEM(3),INORTG     YES-USE IT                                   
         BE    DEMO8                                                            
         OI    LCHG,LDEMO                                                       
         MVC   SVDEM(3),INORTG                                                  
         MVI   SVDEM+3,FF                                                       
         B     DEMO8                                                            
*                                                                               
DEMO4    MVC   COMDEM,LDNAME       USE PRIMARY DEMO                             
         OI    COMDEMH+6,FVOXMT                                                 
         CLC   SVDEM,LDEM          TEST ALREADY PRIMARY DEMO                    
         BE    DEMO10                                                           
         MVC   SVDEM,LDEM          NO                                           
         OI    LCHG,LDEMO                                                       
         B     DEMO10                                                           
*                                                                               
DEMO6    LA    R0,ESTUSRNM                                                      
         GOTO1 VDEMOVAL,APPARM,(1,COMDEMH),(1,APDUB),(C'S',DBLOCK),(R0)         
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     DEMOX                                                            
         CLC   SVDEM,APDUB         TEST DEMO HAS CHANGED                        
         BE    DEMOX                                                            
         MVC   SVDEM,APDUB         YES                                          
         OI    LCHG,LDEMO                                                       
         B     DEMO10                                                           
*                                                                               
DEMO8    LA    R0,ESTUSRNM         FORMAT DEMO NAME                             
         LA    R4,SVDEM                                                         
         GOTO1 VDEMOCON,APPARM,(1,(R4)),(2,COMDEM),(C'S',DBLOCK),(R0)           
         OI    COMDEMH+6,FVOXMT                                                 
         TM    LCHG,LDEMO                                                       
         BZ    DEMOX                                                            
*                                                                               
DEMO10   MVC   SVPUT,SVDEM         DETERMINE PUT AND SHARE DEMO CODES           
         MVC   SVRTGSHR(3),SVDEM                                                
         MVC   SVRTGSHR+3(4),SVDEM                                              
         MVI   SVPUT+1,C'P'                                                     
         MVI   SVRTGSHR+4,C'S'                                                  
         CLI   SVDEM+1,C'I'                                                     
         BNE   *+12                                                             
         MVI   SVPUT+1,C'Q'                                                     
         MVI   SVRTGSHR+4,C'X'                                                  
         BAS   RE,GETSPOVR         GET SHARE/PUT OVERRIDES                      
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
WEEKLY   NTR1                                                                   
         XC    APHALF,APHALF                                                    
         NI    LFLAG,255-LGETBKS                                                
         NI    SVWKYIND,255-SVONEBK                                             
         GOTO1 AFVAL,COMWKYH       VALIDATE WEEKLY FIELD                        
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
WEEK4    GOTO1 VSCANNER,APPARM,COMWKYH,(1,APELEM),C',=,-'                       
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
* INSPECT BOOK FIELDS                                                 *         
* OUTPUT : LCHG = LBK IF BOOKS WERE CHANGED                           *         
*          CC EQ  - OK                                                *         
*             NE  - ERROR                                             *         
***********************************************************************         
         SPACE 1                                                                
BOOKS    NTR1                                                                   
         TM    SVWKYIND,SVONEBK    TEST SINGLE BOOK, 4 WEEKS                    
         BO    BOOKX               YES-BOOKS ALREADY DETERMINED                 
         LA    R4,COMBK1H                                                       
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
BOOK6    LA    R4,COMBK2H-COMBK1H(R4)   NEXT BOOK                               
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
* ROUTINE TO INSPECT THE PROJECTED PUT VALUE FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
PROJPUT  NTR1                                                                   
         GOTO1 AFVAL,COMPPTH                                                    
         BE    PROP1                                                            
         CLI   CLTBWPRO+15,C'Y'    MISSING-TEST SUPPRESS PUTS                   
         BE    PROPX               YES-OK                                       
         CLI   APMODE,APMFRP       NO-TEST INITIAL MODE                         
         BE    PROPX               YES-OK                                       
         B     PROP8               NO-USER MUST ENTER SOMETHING                 
*                                                                               
PROP1    CLI   CLTBWPRO+15,C'Y'    PRESENT-TEST SUPPRESS PUTS                   
         BE    PROP9               YES-INVALID                                  
         ZIC   RF,FVILEN                                                        
         ST    RF,APPARM+4                                                      
         LA    R1,FVIFLD           REMOVE THE * IF ANY                          
*                                                                               
PROP2    CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,PROP2                                                         
         B     PROP4                                                            
         MVC   LDEMLST,SVPUT       YES-REMOVE THE OVERRIDE                      
         GOTO1 ASDEMUP                                                          
         MVC   SVPJPUT,LDEMVAL                                                  
         ICM   R1,15,LAPUTOVR      TEST ENTRY IN SHARE/PUT OVERRIDE EL          
         BZ    *+14                                                             
         NI    4(R1),255-SHPDEMOV  YES-REMOVE                                   
         XC    LAPUTOVR,LAPUTOVR                                                
         OI    LCHG,LPUT           INDICATE PUT CHANGE                          
         XC    EBLOCK,EBLOCK       EDIT THE RAW PUT VALUE NOW                   
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         LA    R1,SVPJPUT                                                       
         ST    R1,EBAIN                                                         
         MVI   EBLOUT,L'COMPPT                                                  
         LA    R1,COMPPT                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBFLOAT,C'*'                                                     
         MVI   EBDECS,1                                                         
         CLC   SVPJPUT,=F'1000'                                                 
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    COMPPTH+6,FVOXMT                                                 
         B     PROPX                                                            
*                                                                               
PROP4    GOTO1 VCASHVAL,APPARM,(1,FVIFLD)   VALIDATE PUT VALUE                  
         CLI   APPARM,X'FF'                                                     
         BE    PROP9                                                            
         CLC   SVPJPUT+1(3),APPARM+5   TEST CHANGE                              
         BE    PROPX                                                            
         MVC   SVPJPUT,APPARM+4    YES                                          
         OI    SVPJPUT,X'80'       OVERRIDE BIT                                 
         OI    LCHG,LPUT           INDICATE PUT CHANGE                          
         OI    LCHG,LPUTOVR        INDICATE PUT OVERIDDEN THIS TIME             
         ICM   R1,15,LAPUTOVR      TEST ENTRY IN SHR/PUT OVERRIDE ELEM          
         BNZ   PROP6               YES                                          
         GOTO1 AADDSHP             NO-ADD PUT OVERRIDE ENTRY                    
         BNE   PROPX                                                            
         L     R1,LASHPENT                                                      
         MVC   0(3,R1),SVPUT                                                    
         ST    R1,LAPUTOVR                                                      
*                                                                               
PROP6    MVC   5(3,R1),SVPJPUT+1                                                
         OI    4(R1),SHPDEMOV                                                   
         B     PROPX                                                            
*                                                                               
PROP8    MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     PROPX                                                            
*                                                                               
PROP9    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
PROPX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET SHARE/PUT OVERRIDES                                             *         
***********************************************************************         
         SPACE 1                                                                
GETSPOVR NTR1  ,                                                                
         XC    LASHROVR,LASHROVR                                                
         XC    LAPUTOVR,LAPUTOVR                                                
         ICM   R4,15,LASHPEL       TEST SHARE/PUT OVERRIDE ELEMENT              
         BZ    GETSX                                                            
         ZIC   RF,1(R4)            YES-FIND OVERRIDES FOR SHARE AND PUT         
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         LA    RE,L'SHPDEMO                                                     
         LA    R4,SHPDEMO-SHPEL(R4)                                             
*                                                                               
GETS2    TM    4(R4),SHPDEMOV                                                   
         BZ    GETS4                                                            
         CLC   1(2,R4),SVRTGSHR+4                                               
         BNE   *+8                                                              
         ST    R4,LASHROVR                                                      
         CLC   1(2,R4),SVPUT+1                                                  
         BNE   *+8                                                              
         ST    R4,LAPUTOVR                                                      
GETS4    BXLE  R4,RE,GETS2                                                      
*                                                                               
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET AND FORMAT THE PROJECTED PUT VALUE                              *         
***********************************************************************         
         SPACE 1                                                                
GETPJPUT NTR1  ,                                                                
         ICM   R1,15,LAPUTOVR      TEST PROJECTED PUT OVERRIDE                  
         BNZ   GETJ2               YES                                          
         TM    LFLAG,LPUTADJ       NO-TEST PUT HAS BEEN ADJUSTED                
         BZ    GETJ4               NO-CALL SPDEMUP                              
         NI    LFLAG,255-LPUTADJ   YES-USE THAT VALUE                           
         B     GETJ6                                                            
*                                                                               
GETJ2    XC    SVPJPUT,SVPJPUT     USE PUT OVERRIDE                             
         MVC   SVPJPUT+1(3),5(R1)                                               
         MVC   APFULL,SVPJPUT                                                   
         OI    SVPJPUT,X'80'                                                    
         LA    R1,APFULL                                                        
         MVI   EBFLOAT,C'*'                                                     
         LA    RF,1000                                                          
         B     GETJ8                                                            
*                                                                               
GETJ4    MVC   LDEMLST,SVPUT       CALL SPDEMUP FOR PROJECTED PUT               
         GOTO1 ASDEMUP                                                          
         MVC   SVPJPUT,LDEMVAL                                                  
*                                                                               
GETJ6    LA    R1,SVPJPUT                                                       
         LH    RF,=H'10000'                                                     
*                                                                               
GETJ8    ST    R1,EBAIN            FORMAT PROJECTED PUT                         
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         LA    RE,COMPPT                                                        
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'COMPPT                                                  
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         CLM   RF,7,1(R1)                                                       
         BH    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    COMPPTH+6,FVOXMT                                                 
*                                                                               
GETJX    B     EXIT                                                             
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
EDEM     MVC   FVMSGNO,=AL2(FVIDEMOP)                                           
         LA    R1,BWSOPTH                                                       
         ST    R1,FVADDR                                                        
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
AVALRSH  DS    A                                                                
AGETPUTS DS    A                                                                
AGETDEMS DS    A                                                                
ADEMUP   DS    A                                                                
ASDEMUP  DS    A                                                                
AADDSHP  DS    A                                                                
AFMTPJCM DS    A                                                                
ARDMKT   DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
TEMPSTR  DC    CL7'TEMPSTR'                                                     
DMREAD   DC    CL7'DMREAD '                                                     
DMWRITE  DC    CL7'DMWRT  '                                                     
UMAJBKS  DC    XL4'0205070B'       USA MAJOR BOOKS                              
CMAJBKSA DC    XL3'03070B'         CANADIAN BBM BOOKS                           
CMAJBKSN DC    XL4'0103080B'       CANADIAN CSI BOOKS                           
SPACES   DC    CL80' '                                                          
EFFS     DC    XL3'FFFFFF'                                                      
FUT1     DC    CL18'ENTER=NEXT PF1=EST'                                         
FUT2     DC    CL8'PF2=TRAN'                                                    
FUT3     DC    CL36'PF4=FRST PF5=PREV PF6=NEXT PF12=QUIT'                       
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
EXTRA    NMOD1 0,**B14X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALRSH                                                           
         B     GETPUTS                                                          
         B     GETDEMS                                                          
         B     DEMUP                                                            
         B     SDEMUP                                                           
         B     ADDSHP                                                           
         B     FMTPJCUM                                                         
         B     RDMKT                                                            
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE RATING/SHARE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALRSH   GOTO1 AFVAL,COMRSHH       VALIDATE RTG/SHR FIELD                       
         BNE   VALRX                                                            
         CLI   CLTBWPRO+15,C'Y'    TEST SHARES ARE SUPPRESSED                   
         BNE   VALR1                                                            
         ZIC   RE,FVILEN           YES-SCAN FIELD FOR X                         
         LR    R0,RE                                                            
         SR    R1,R1                                                            
         LA    R9,FVIFLD                                                        
         LR    RF,R9                                                            
         CLI   0(RF),C'X'                                                       
         BE    VALR8               YES-REMOVE RATING OVERRIDE                   
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         ST    RE,APPARM+4         NO-VALIDATE RATING ALONE                     
         B     VALR7                                                            
*                                                                               
VALR1    MVC   LOLDSHR,SVPROJ+4    SAVE OLD SHARE                               
         XC    APELEM,APELEM                                                    
         LA    RF,C'/'                                                          
         STC   RF,APELEM(RF)                                                    
         TRT   FVIFLD(L'COMRSH),APELEM    SEARCH FOR / CHARACTER                
         BZ    VALR99                                                           
         LR    R9,R1               R1 = A(C'/')                                 
         SR    RE,RE               LOCATE RATING FIELD                          
         BCTR  RE,0                                                             
         LA    RF,FVIFLD                                                        
         BCTR  RF,0                                                             
         XC    APFULL,APFULL                                                    
*                                                                               
VALR2    BXH   R9,RE,*+18                                                       
         OC    APFULL,APFULL                                                    
         BZ    VALR99                                                           
         B     VALR6                                                            
         CLI   0(R9),C' '                                                       
         BNH   VALR4                                                            
         CLI   0(R9),C'*'          REMOVE * IF ANY                              
         BNE   *+8                                                              
         MVI   0(R9),C' '                                                       
         CLI   0(R9),C'X'          TEST FOR X                                   
         BE    VALR8               YES-REMOVE THE OVERRIDE                      
         OC    APFULL,APFULL                                                    
         BNZ   VALR2                                                            
         ST    R9,APFULL                                                        
         B     VALR2                                                            
*                                                                               
VALR4    OC    APFULL,APFULL                                                    
         BZ    VALR2                                                            
*                                                                               
VALR6    L     RF,APFULL                                                        
         SR    RF,R9                                                            
         ST    RF,APPARM+4                                                      
         LA    R9,1(R9)                                                         
         ST    R1,APFULL           APFULL = A(C'/')                             
*                                                                               
VALR7    GOTO1 VCASHVAL,APPARM,(1,(R9))     VALIDATE RATING FIELD               
         CLI   APPARM,FF                                                        
         BE    VALR99                                                           
         OC    APPARM+4(4),APPARM+4                                             
         BZ    VALR99                                                           
         CLC   SVPROJ+1(3),APPARM+5  TEST RATING CHANGE                         
         BNE   *+12                                                             
         TM    LFLAG2,LTIMOVR+LDAYOVR  OR OVERRIDDEN DAYS/TIMES                 
         BZ    VALR10                                                           
         OI    LCHG,LRTG             YES -                                      
         MVC   SVPROJ(4),APPARM+4    CHANGE SAVED RATING                        
         OI    SVPROJ,X'80'          INDICATE OVERRIDE                          
         B     VALR10                                                           
*                                  REMOVE RATING OVERRIDE                       
VALR8    ST    R1,APFULL           APFULL=A(C'/')                               
         MVC   LDEMLST,SVDEM                                                    
         BAS   RE,LSDEMUP          SINGLE UPGRADE FOR RATING                    
         MVC   SVPROJ(4),LDEMVAL                                                
         OI    LCHG,LRTG           INDICATE RATING CHANGE                       
*                                                                               
VALR10   CLI   CLTBWPRO+15,C'Y'    TEST SUPPRESS SHARES                         
         BE    VALR26                                                           
         L     R9,APFULL           NO-VALIDATE SHARE FIELD                      
         LA    R9,1(R9)                                                         
         ZIC   RE,FVILEN                                                        
         LA    RE,FVIFLD(RE)                                                    
         SR    RE,R9                                                            
         ST    RE,APPARM+4                                                      
         LR    R1,R9                                                            
*                                                                               
VALR12   CLI   0(R1),C'*'          REMOVE *, IF ANY                             
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    VALR16              YES-REMOVE OVERRIDE                          
         LA    R1,1(R1)                                                         
         BCT   RE,VALR12                                                        
         GOTO1 VCASHVAL,APPARM,(1,(R9))                                         
         CLI   APPARM,FF                                                        
         BE    VALR99                                                           
         OC    APPARM+4(4),APPARM+4                                             
         BZ    VALR99                                                           
         CLC   SVPROJ+5(3),APPARM+5   TEST SHARE CHANGE                         
         BE    VALR18                                                           
         OI    LCHG,LSHR              YES -                                     
         MVC   SVPROJ+4(4),APPARM+4   CHANGE SAVED SHARE                        
         OI    SVPROJ+4,X'80'         OVERRIDE BIT                              
         ICM   R1,15,LASHROVR         TEST ENTRY IN SHARE/PUT OVR ELEM          
         BNZ   VALR14                                                           
         BAS   RE,LADDSHP             NO-ADD SHARE OVERRIDE ENTRY               
         BNE   VALRX                                                            
         L     R1,LASHPENT                                                      
         MVC   0(3,R1),SVRTGSHR+3                                               
         ST    R1,LASHROVR                                                      
*                                                                               
VALR14   MVC   4(4,R1),APPARM+4                                                 
         OI    4(R1),SHPDEMOV                                                   
         B     VALR18                                                           
*                                                                               
VALR16   MVC   LDEMLST,SVRTGSHR+3  REMOVE SHARE OVERRIDE                        
         BAS   RE,LSDEMUP                                                       
         MVC   SVPROJ+4(4),LDEMVAL                                              
         OI    LCHG,LSHR                                                        
         ICM   R1,15,LASHROVR      TEST ENTRY IN SHARE/PUT OVERRIDE ELE         
         BZ    VALR18                                                           
         NI    4(R1),255-SHPDEMOV  YES-REMOVE OVERRIDE BIT                      
         XC    LASHROVR,LASHROVR       OBLITERATE TRACE OF OVERRIDE             
*                                                                               
VALR18   TM    LCHG,LRTG+LSHR+LPUT TEST FOR ANY DEMO VALUE CHANGES              
         BZ    VALRX               NO                                           
         TM    SVPROJ,X'80'        TEST RATING OVERRIDE                         
         BO    VALR19              YES                                          
         TM    SVPROJ+4,X'80'      NO-TEST ANY SHARE/PUT OVERRIDES              
         BO    *+12                                                             
         TM    SVPJPUT,X'80'                                                    
         BZ    VALR22              NO-THEN ADJUST THE SHARE                     
         SR    R1,R1               YES-CALCULATE NEW RATING                     
         L     R0,SVPROJ+4                                                      
         SLL   R0,1                                                             
         SRDL  R0,32                                                            
         L     RE,SVPJPUT                                                       
         SLL   RE,1                                                             
         SRL   RE,1                                                             
         MR    R0,RE                                                            
         D     R0,=F'1000'                                                      
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,SVPROJ                                                        
         OI    SVPROJ,X'80'        RATING OVERRIDE BIT                          
         OI    LCHG,LRTG           INDICATE RATING CHANGE                       
         B     VALR26                                                           
*                                  THERE IS A RATING OVERRIDE --                
VALR19   TM    LCHG,LSHR+LPUT      TEST FOR SHARE/PUT CHANGE                    
         BZ    VALR20              NO                                           
         TM    LCHG,LPUTOVR        YES-TEST PUT OVERRIDDEN THIS TIME            
         BO    VALR22              YES-ADJUST THE SHARE                         
         TM    SVPROJ+4,X'80'      NO-TEST FOR SHARE OVERRIDE                   
         BO    VALR24              YES-ADJUST THE PUT                           
         B     VALR22              NO-ADJUST THE SHARE                          
*                                                                               
VALR20   TM    SVPJPUT,X'80'       TEST PUT OVERRIDE                            
         BO    VALR22              YES-ADJUST THE SHARE                         
         TM    SVPROJ+4,X'80'      NO-TEST SHARE OVERRIDE                       
         BO    VALR24              YES-ADJUST THE PUT                           
*                                                                               
VALR22   OC    SVPJPUT+1(3),SVPJPUT+1    ** ADJUST THE SHARE **                 
         BZ    VALR26                                                           
         SR    R1,R1                                                            
         L     R0,SVPROJ                                                        
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
         SRDL  R0,32                                                            
         M     R0,=F'1000'                                                      
         L     RE,SVPJPUT                                                       
         SLL   RE,1                                                             
         SRL   RE,1                                                             
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         STCM  R1,7,SVPROJ+5                                                    
         OI    LCHG,LSHR           INDICATE SHARE CHANGE                        
         ICM   RE,15,LASHROVR      TEST ENTRY IN SHARE/PUT OVERIDE ELEM         
         BZ    VALR26                                                           
         TM    4(RE),SHPDEMOV      YES-TEST IT'S A REAL OVERRIDE                
         BZ    VALR26                                                           
         STCM  R1,7,5(RE)          YES-UPDATE THE OVERRIDE                      
         B     VALR26                                                           
*                                                                               
VALR24   OC    SVPROJ+5(3),SVPROJ+5    ** ADJUST THE PUT **                     
         BZ    VALR26                                                           
         SR    R1,R1                                                            
         L     R0,SVPROJ                                                        
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
         SRDL  R0,32                                                            
         M     R0,=F'1000'                                                      
         L     RE,SVPROJ+4                                                      
         SLL   RE,1                                                             
         SRL   RE,1                                                             
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         STCM  R1,7,SVPJPUT+1                                                   
         OI    LCHG,LPUT           INDICATE PUT CHANGE                          
         ICM   RE,15,LAPUTOVR      TEST ENTRY IN SHARE/PUT OVERIDE ELEM         
         BZ    VALR26                                                           
         TM    4(RE),SHPDEMOV      YES-TEST IT'S A REAL OVERRIDE                
         BZ    VALR26                                                           
         STCM  R1,7,5(RE)          YES-UPDATE THE OVERRIDE                      
*                                                                               
VALR26   TM    LCHG,LRTG           TEST RATING CHANGE                           
         BZ    VALR30                                                           
         ICM   R1,15,LADEMEL       YES-CHANGE RATING IN BWS RECORD              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DMOEL,R1                                                         
         ZIC   RF,DMOELLN                                                       
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    RE,L'DMODEMO                                                     
         LA    R4,DMODEMO                                                       
         CLC   1(2,R4),SVDEM+1                                                  
         BE    VALR28              R4 = A(DEMO ENTRY IN DEMO ELEMENT)           
         BXLE  R4,RE,*-10                                                       
         XC    APELEM,APELEM       NOT FOUND - ADD DEMO TO ELEMENT              
         ZIC   RF,DMOELLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R1)     SAVE THE ELEMENT                             
         GOTO1 ADELELS,BWDRECD     DELETE IT FROM RECORD                        
         ZIC   RF,APELEM+1                                                      
         LA    RF,L'DMODEMO(RF)    LENGTHEN ELEMENT BY ONE DEMO                 
         STC   RF,APELEM+1                                                      
         GOTO1 AADDELS,BWDRECD     ADD BACK THE ELEMENT                         
         MVC   0(3,R4),SVDEM       MOVE IN THE DEMO CODE                        
*                                                                               
VALR28   XC    LDEMOLD,LDEMOLD     SAVE OLD DEMO VALUE                          
         MVC   LDEMOLD+1(3),5(R4)                                               
         MVC   4(4,R4),SVPROJ      MOVE RATING TO DEMO ELEMENT                  
         TM    SVPROJ,X'80'        CARRY OVER THE OVERRIDE BIT                  
         BZ    *+8                                                              
         OI    4(R4),DMODEMOV                                                   
*                                                                               
         TM    INFIND,INFINOAD     TEST OPTION TO NOT AUTO ADJUST DEMOS         
         BO    VALR30                                                           
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT   NO-TEST AUTO DEMO           
         BZ    VALR30                                  ADJUST                   
         OC    LDEMOLD,LDEMOLD     YES - TEST OLD DEMO VALUE = 0                
         BZ    VALR98                    YES - ERROR                            
         BAS   RE,DEMADJ                 NO - DO THE ADJUSTMETS                 
*                                                                               
VALR30   B     VALRX                                                            
*                                                                               
VALR98   MVC   FVMSGNO,=AL2(FVIDADJ)                                            
         B     VALRX                                                            
*                                                                               
VALR99   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALRX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AUTO DEMO ADJUSTMENT ROUTINE                                        *         
* INPUT  : R4 = A(NEW DEMO ENTRY IN DEMO ELEMENT)                     *         
*          LADEMEL = A(DEMO ELEMENT)                                  *         
*          LDEMOLD = OLD RATING                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMADJ   NTR1                                                                   
         NI    LFLAG,FF-LADJALL                                                 
         OI    LFLAG,LADJIMP       ADJUST IMPRESSION                            
         TM    CMPOPTS,CAMOAALL+CAMOATGT  TEST AUTOADJ=ALL/TGT                  
         BZ    DEMA2                                                            
         MVC   APHALF,=X'D901'                                                  
         TM    CMPOPTS,CAMOAALL                                                 
         BO    *+10                                                             
         MVC   APHALF,ESTDEMS+1                                                 
         CLC   APHALF,1(R4)        YES-TEST TARGET IS ADJUSTMENT DEMO           
         BNE   DEMA2                                                            
         OI    LFLAG,LADJALL       YES                                          
         B     DEMA4                                                            
*                                                                               
DEMA2    CLI   1(R4),C'R'          TEST TARGET IS A RATING                      
         BNE   DAX                 NO - NO ADJUSTMENTS                          
*                                                                               
DEMA4    L     R1,LDEMOLD          CALCULATE PCT ADJUSTMENT                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,5(R4)                                                       
         M     RE,=F'2000'                                                      
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,LDEMADJ          LDEMADJ = PCT ADJUSTMENT                     
*                                                                               
         L     R1,LADEMEL          SCAN ALL DEMOS IN DEMO ALEMENT               
         USING DMOEL,R1                                                         
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,DMODEMO                                                       
         LA    RE,L'DMODEMO                                                     
         DROP  R1                                                               
*                                                                               
DEMA6    CLC   1(2,R1),1(R4)       TEST OUR DEMO                                
         BE    DEMA12              YES - NO ADJ                                 
         TM    4(R1),DMODEMOV      TEST ALREADY MANUALLY OVERRIDDEN             
         BO    DEMA12              YES - NO ADJ                                 
         TM    LFLAG,LADJIMP       TEST TARGET IMPRESSION ADJUST                
         BZ    DEMA8                                                            
         CLI   1(R1),C'I'          YES - TEST ITS THE IMP WE WANT               
         BNE   DEMA8                                                            
         CLC   2(1,R1),2(R4)                                                    
         BE    DEMA10                    YES - ADJUST                           
*                                                                               
DEMA8    TM    LFLAG,LADJALL       TEST ADJUST ALL                              
         BZ    DEMA12              NO                                           
         CLI   1(R1),C'I'          YES - TEST ITS AN IMPRESSION                 
         BNE   DEMA10                    NO - GO AND ADJUST                     
         L     R8,LADEMEL                YES - CHECK THAT ITS RATING            
         LA    R8,DMODEMO-DMOEL(R8)            HAS NOT BEEN MANUALLY            
         CLI   1(R8),C'R'                      OVERRIDDEN                       
         BNE   *+14                                                             
         CLC   2(1,R8),2(R1)                                                    
         BE    *+12                                                             
         BXLE  R8,RE,*-18                                                       
         B     DEMA10                                                           
         TM    4(R8),DMODEMOV                                                   
         BO    DEMA12                                                           
*                                                                               
DEMA10   SR    R8,R8               DO THE ADJUSTMENT                            
         ICM   R8,7,5(R1)                                                       
         SR    R9,R9                                                            
         SRDA  R8,31                                                            
         M     R8,LDEMADJ                                                       
         D     R8,=F'1000'                                                      
         AH    R9,=H'1'                                                         
         SRA   R9,1                                                             
         ST    R9,4(R1)            STORE ADJUSTED DEMO IN ELEMENT               
*                                                                               
DEMA12   BXLE  R1,RE,DEMA6         NEXT DEMO                                    
*                                                                               
DAX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET THE PUT VALUES AND FORMAT TO HEADLINE                           *         
***********************************************************************         
         SPACE 1                                                                
GETPUTS  XC    EBLOCK,EBLOCK       PREPARE EDITOR BLOCK                         
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,L'COMPT1                                                  
         XC    SPDEMLK,SPDEMLK     LOOK UP PUT VALUES FOR EACH BOOK             
         LA    RE,LIUN                                                          
         ST    RE,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOM                                                    
         MVC   SPLKAGY,CUAALF                                                   
         MVC   SPLKMED,CUDMED                                                   
         MVC   SPLKCLI,QCLT                                                     
         MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKSTA,QSTA                                                     
         MVC   SPLKSRC,CLTSRC                                                   
         CLI   SVRTGSVC,0          TEST SOURCE RETURNED FROM SPDEMUP            
         BE    *+14                                                             
         MVC   SPLKSRC,SVRTGSVC    YES-USE IT AND                               
         B     *+10                    SUPPRESS MARKET OVERRIDE                 
         MVC   SPLKUMK,MKTLKUP                                                  
         MVC   SPLKDAY,BDAYS                                                    
         TM    LFLAG2,LDAYOVR      TEST DAYS OVERRIDDEN                         
         BZ    *+10                                                             
         MVC   SPLKDAY,LDAYS                                                    
         MVC   SPLKTIM,BTIMES                                                   
         TM    LFLAG2,LTIMOVR      TEST TIMES OVERRIDDEN                        
         BZ    *+10                                                             
         MVC   SPLKTIM,LTIMES                                                   
         MVI   SPLKSVI,FF                                                       
         CLI   SVWKY,C'W'          TEST 4 BOOKS, 1 WEEK                         
         BNE   *+14                                                             
         MVC   SPLKWKN,SVWKY+1     YES-SET WEEK NUMBER                          
         NI    SPLKWKN,X'0F'                                                    
*                                                                               
         LA    R1,G1WPROF                                                       
         ST    R1,SPLKA1W                                                       
*        CLI   CUDMED,C'C'         TEST CANADA                                  
*        BNE   GETP1                                                            
*        TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
*        BZ    GETP1                                                            
*        XC    L1WPROF,L1WPROF     YES-PASS A(1W PROFILE)                       
*        LA    R1,L1WPROF                                                       
*        MVI   3(R1),C'Y'                                                       
*        ST    R1,SPLKA1W                                                       
*                                                                               
GETP1    LA    R1,SVPUT                                                         
         ST    R1,SPLKALST                                                      
         LA    R1,APWORK                                                        
         ST    R1,SPLKAVAL                                                      
         LA    R0,4                                                             
         LA    R2,COMPT1H                                                       
         LA    R4,SVPUTVAL                                                      
         LA    R8,SVBKS                                                         
         LA    R9,1                                                             
         XC    SVPUTVAL,SVPUTVAL                                                
*                                                                               
GETP2    XC    8(L'COMPT1,R2),8(R2)                                             
         OI    6(R2),FVOXMT                                                     
         OC    0(2,R8),0(R8)                                                    
         BZ    GETP4                                                            
         MVC   SPLKDBK,0(R8)                                                    
         TM    SVWKYIND,SVONEBK    TEST 1 BOOK, 4 WEEKS                         
         BZ    *+8                                                              
         STC   R9,SPLKWKN          YES-SET THE WEEK NUMBER                      
         BAS   RE,DEMLOOK          SPDEMLK                                      
*                                                                               
         MVC   0(4,R4),APWORK      PUT VALUE RETURNED                           
         ST    R4,EBAIN            FORMAT IT                                    
         LA    R1,8(R2)                                                         
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         CLC   0(4,R4),=F'1000'                                                 
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
GETP4    LA    R2,COMPT2H-COMPT1H(R2)   NEXT BOOK                               
         LA    R4,4(R4)                                                         
         LA    R8,2(R8)                                                         
         LA    R9,1(R9)                                                         
         BCT   R0,GETP2                                                         
*                                                                               
GETPX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE RATING AND SHARE VALUES                          *         
*            AND FORMAT THE CUM SHARES                                *         
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
         CLI   SVRTGSVC,0          TEST SOURCE RETURNED FROM SPDEMUP            
         BE    *+14                                                             
         MVC   SPLKSRC,SVRTGSVC    YES-USE IT AND                               
         B     *+10                    SUPPRESS MARKET OVERRIDE                 
         MVC   SPLKUMK,MKTLKUP                                                  
         MVC   SPLKDAY,BDAYS                                                    
         TM    LFLAG2,LDAYOVR      TEST DAYS OVERRIDDEN                         
         BZ    *+10                                                             
         MVC   SPLKDAY,LDAYS                                                    
         MVC   SPLKTIM,BTIMES                                                   
         TM    LFLAG2,LTIMOVR      TEST TIMES OVERRIDDEN                        
         BZ    *+10                                                             
         MVC   SPLKTIM,LTIMES                                                   
         MVI   SPLKSVI,FF                                                       
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
GETD1    LA    R1,SVRTGSHR                                                      
         ST    R1,SPLKALST                                                      
         LA    R1,APWORK                                                        
         ST    R1,SPLKAVAL                                                      
         ZIC   R2,SVSTANO          R2 = NUMBER OF STATIONS                      
         LA    R4,SVSTA            R4 = A(STATION LIST)                         
         LA    R5,SVPROGS          R5 = A(PROGRAM NAME AREA)                    
         LA    R9,SVDEMVAL         R9 = A(DEMO VALUE AREA)                      
         LR    RE,R5               CLEAR PROGRAM NAME AREA                      
         LA    RF,SVPROGSL                                                      
         XCEF                                                                   
         LR    RE,R9               CLEAR DEMO VALUE AREA                        
         LA    RF,SVDEMVLL                                                      
         XCEF                                                                   
         XC    SVCUMS,SVCUMS       CLEAR CUM SHARE VALUE AREA                   
*                                                                               
GETD2    MVC   SPLKSTA,0(R4)       STATION                                      
         MVC   SPLKSPL,5(R4)       SPILL MARKET (IF ANY)                        
         LA    R0,4                                                             
         LA    R3,SVCUMS           R3 = A(CUM SHARE VALUE AREA)                 
         LA    R8,SVBKS            R8 = A(BOOK LIST)                            
         LA    RF,1                                                             
*                                                                               
GETD4    OC    0(2,R8),0(R8)                                                    
         BZ    GETD6                                                            
         MVC   SPLKDBK,0(R8)                                                    
         TM    SVWKYIND,SVONEBK    TEST 1 BOOK, 4 WEEKS                         
         BZ    *+8                                                              
         STC   RF,SPLKWKN          YES-SET THE WEEK NUMBER                      
         BAS   RE,DEMLOOK          SPDEMLK                                      
*                                                                               
         MVC   0(L'L1PROG1,R5),SPLKPRG    PROGRAM NAME                          
         MVC   0(4,R9),APWORK      RATING                                       
         MVC   4(4,R9),APWORK+8    SHARE                                        
         L     R1,0(R3)            ACCUMULATE TOTAL BOOK SHARE                  
         A     R1,APWORK+8                                                      
         ST    R1,0(R3)                                                         
*                                                                               
GETD6    LA    R3,4(R3)            NEXT BOOK                                    
         LA    R5,L'L1PROG1(R5)                                                 
         LA    R8,2(R8)                                                         
         LA    R9,8(R9)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,GETD4                                                         
*                                                                               
         LA    R4,7(R4)            NEXT STATION                                 
         BCT   R2,GETD2                                                         
*                                                                               
         XC    EBLOCK,EBLOCK       FORMAT CUM SHARES                            
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,L'CUM1                                                    
         MVI   EBDECS,1                                                         
         LA    R0,4                                                             
         LA    R3,SVCUMS                                                        
         L     R5,LATWA                                                         
         LA    R4,COMCUM                                                        
         USING CUMLINED,R4                                                      
         LA    R8,CUM1                                                          
*                                                                               
GETD8    ST    R3,EBAIN                                                         
         ST    R8,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R3,4(R3)                                                         
         LA    R8,CUM2-CUM1(R8)                                                 
         BCT   R0,GETD8                                                         
         OI    COMCUMH+6,FVOXMT                                                 
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DEMO LOOKUP ROUTINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
DEMLOOK  NTR1                                                                   
         GOTO1 VSPDEMLK,APPARM,(X'FF',SPDEMLK)   CALL SPGETDEM                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DEMO UPGRADE ROUTINE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING BWDRECD,R3                                                       
DEMUP    OC    SVUPGRD,SVUPGRD     TEST UPGRADE EXISTS                          
         BZ    DEMUX                                                            
         XC    LARTGENT,LARTGENT   BUILD DEMO OVERRIDE ELEMENT                  
         XC    LDEMOVR,LDEMOVR                                                  
         NI    LFLAG,FF-LDEMFRZ                                                 
         ICM   R8,15,LADEMEL                                                    
         BZ    DEMU12                                                           
         USING DMOEL,R8                                                         
         ZIC   RF,1(R8)                                                         
         AR    RF,R8                                                            
         BCTR  RF,0                                                             
         LA    RE,L'DMODEMO                                                     
         LA    R1,DMODEMO                                                       
         USING DMODEMO,R1                                                       
*                                                                               
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT   TEST AUTO DEMO              
         BZ    DEMU10                               ADJUST                      
         TM    CMPOPTS,CAMOAALL+CAMOATGT  TEST AUTOADJ=ALL/TGT                  
         BNZ   DEMU2                                                            
         CLI   SVDEM+1,C'I'        NO-TEST DEMO IS IMPRESSION                   
         BNE   DEMU10                 NO-DEMO IS NOT FROZEN                     
*                                                                               
DEMU2    TM    DMODEMO+4,DMODEMOV  TEST MANUAL OVERRIDE                         
         BZ    DEMU6                                                            
         TM    CMPOPTS,CAMOAALL+CAMOATGT  TEST AUTOADJ=ALL/TGT                  
         BZ    DEMU4                                                            
         MVC   APHALF,=X'D901'                                                  
         TM    CMPOPTS,CAMOAALL                                                 
         BO    *+10                                                             
         MVC   APHALF,ESTDEMS+1                                                 
         CLC   APHALF,DMODEMO+1    YES-TEST ADJUSTMENT DEMO                     
         BNE   DEMU4                                                            
         OI    LFLAG,LDEMFRZ       YES-FREEZE THE DEMO                          
         B     DEMU8                                                            
*                                                                               
DEMU4    CLI   SVDEM+1,C'I'        TEST DEMO IS IMPRESSION                      
         BNE   DEMU6                                                            
         CLI   DMODEMO+1,C'R'      YES - TEST THIS IS ITS RATING                
         BNE   DEMU6                                                            
         CLC   DMODEMO+2(1),SVDEM+2                                             
         BNE   DEMU6                                                            
         OI    LFLAG,LDEMFRZ       YES - FREEZE THE DEMO                        
         B     DEMU8                                                            
*                                                                               
DEMU6    BXLE  R1,RE,DEMU2                                                      
*                                                                               
DEMU8    L     R1,LADEMEL                                                       
         LA    R1,DMODEMO-DMOEL(R1)                                             
*                                                                               
DEMU10   CLC   DMODEMO+1(2),SVDEM+1   FIND DEMO IN DEMO ELEMENT                 
         BNE   DEMU11                                                           
         ST    R1,LARTGENT            SAVE A(SLOT IN DEMO ELEMENT)              
         TM    LFLAG,LDEMFRZ          TEST FREEZE THE DEMO VALUE                
         BO    *+12                                                             
         TM    DMODEMO+4,DMODEMOV     TEST FOR OVERRIDE                         
         BZ    DEMU12                                                           
         MVI   LDEMOVR,OVERELEM       YES - BUILD OVERRIDE ELEM                 
         MVI   LDEMOVR+1,6                                                      
         MVC   LDEMOVR+2(2),DMODEMO+1                                           
         MVC   LDEMOVR+4(2),DMODEMO+6                                           
         TM    DMODEMO+4,DMODEMOV     TEST THIS IS MANUAL OVERRIDE              
         BZ    DEMU12                                                           
         NI    LFLAG,FF-LDEMFRZ       YES-NOT RESULT OF FREEZE                  
         OI    LFLAG,LRTGOVR          INDICATE MANUAL RATING OVERRIDE           
         B     DEMU12                                                           
*                                                                               
DEMU11   BXLE  R1,RE,DEMU10                                                     
         DROP  R1                                                               
*                                                                               
DEMU12   LA    RE,LDEMOVR                                                       
         CLI   0(RE),0                                                          
         BE    *+8                                                              
         LA    RE,6(RE)                                                         
         LA    R0,2                                                             
         ICM   R1,15,LASHROVR      TEST SHARE OVERRIDE                          
         BZ    DEMU14              NO                                           
*                                                                               
DEMU13   MVI   0(RE),OVERELEM      YES-BUILD OVERRIDE ELEMENT                   
         MVI   1(RE),6                                                          
         MVC   2(2,RE),1(R1)                                                    
         MVC   4(2,RE),6(R1)                                                    
         LA    RE,6(RE)                                                         
*                                                                               
DEMU14   ICM   R1,15,LAPUTOVR      TEST PUT OVERRIDE                            
         BZ    DEMU15                                                           
         BCT   R0,DEMU13           YES-BUILD PUT OVERRIDE ELEMENT               
*                                                                               
DEMU15   MVI   SVRTGSHR,0          REMOVE OVERRIDE INDICATORS                   
         MVI   SVRTGSHR+3,0                                                     
*                                                                               
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
         MVC   SPUPDAY,BWDDAYS                                                  
         TM    LFLAG2,LDAYOVR      TEST DAYS OVERRIDDEN                         
         BZ    *+10                                                             
         MVC   SPUPDAY,LDAYS                                                    
         MVC   SPUPTIM,BWDTIMES                                                 
         TM    LFLAG2,LTIMOVR      TEST TIMES OVERRIDDEN                        
         BZ    *+10                                                             
         MVC   SPUPTIM,LTIMES                                                   
         MVC   SPUPFIL,SVUPFILE                                                 
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,SVUPFRBK                                                 
         MVC   SPUPFBKL,SVUPFBL                                                 
         LA    R1,LDEMOVR                                                       
         ST    R1,SPUPAOVR                                                      
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
         ZIC   R2,SVSTANO          R2 = NUMBER OF STATIONS                      
         LA    R5,SVPJPRGS         R5 = A(PROGRAM NAMES)                        
         LA    R8,SVSTA            R8 = A(STATION LIST)                         
         LA    R9,SVPROJ           R9 = A(DEMO VALUE AREA)                      
         LR    RE,R9               CLEAR DEMO VALUE AREA                        
         LA    RF,SVPROJL                                                       
         XCEF                                                                   
         LR    RE,R5               CLEAR PROGRAM NAME AREA                      
         LA    RF,SVPJPRGL                                                      
         XCEF                                                                   
         XC    SVPJCUM,SVPJCUM     CLEAR CUM SHARES                             
*                                                                               
DEMU16   MVC   SPUPSTA,0(R8)       STATION                                      
         MVC   SPUPSPL,5(R8)       SPILL MARKET (IF ANY)                        
*                                                                               
         GOTO1 ASPDEMUP,APPARM,LDMUPBLK,SVRTGSHR,(R9)                           
*                                                                               
         MVC   SVRTGSVC,SPUPACTS   SAVE ACTUAL RATING SERVICE                   
         MVC   0(L'L1PJPROG,R5),SPUPPRG    PROGRAM NAME                         
         CLM   R2,1,SVSTANO        TEST ON FIRST STATION                        
         BNE   DEMU18              NO                                           
         XC    SPUPAOVR,SPUPAOVR   YES -                                        
         MVC   0(L'L1PJPROG,R5),BWDPROG  PROGRAM NAME FROM BWS RECORD           
*                                                                               
         CLI   LDEMOVR,0           TEST ANY OVERRIDES                           
         BE    DEMU28              NO                                           
         TM    LFLAG,LRTGOVR       YES-TEST MANUAL RATING OVERRIDE              
         BO    DEMU20              YES                                          
         CLI   SVRTGSHR+3,OVERELEM NO-TEST SHARE OR PUT OVERRIDE                
         BE    DEMU27              YES-ADJUST THE RATING                        
*                                                                               
DEMU18   TM    SVPJPUT,X'80'       TEST PUT OVERRIDE                            
         BO    DEMU27              YES-ADJUST THE RATING                        
         B     DEMU28              NO ADJUSTMENTS                               
*                                                                               
DEMU20   NI    LFLAG,255-LRTGOVR   THERE'S A RATING OVERRIDE --                 
         TM    SVPJPUT,X'80'       TEST PUT OVERRIDE                            
         BO    DEMU22              YES-ADJUST THE SHARE                         
         CLI   SVRTGSHR+3,OVERELEM TEST SHARE OVERRIDE                          
         BE    DEMU24              YES-ADJUST THE PUT                           
         CLI   SVRTGSHR,OVERELEM   TEST RATING OVERRIDE                         
         BNE   DEMU28              NO-NO ADJUSTMENT NECESSARY                   
*                                                                               
DEMU22   SR    RE,RE               ** ADJUST THE SHARE **                       
         ICM   RE,7,SVPJPUT+1                                                   
         BZ    DEMU26                                                           
         SR    R1,R1                                                            
         L     R0,0(R9)                                                         
         SRDA  R0,31                                                            
         M     R0,=F'1000'                                                      
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         C     R1,4(R9)            TEST SHARE CHANGE                            
         BE    DEMU26                                                           
         ST    R1,4(R9)                                                         
         CLI   SVRTGSHR+3,OVERELEM TEST SHARE HAS BEEN OVERRIDDEN               
         BNE   DEMU26                                                           
         ICM   R1,15,LASHROVR      YES-CHANGE THE OVERRIDE                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   4(4,R1),4(R9)                                                    
         OI    4(R1),SHPDEMOV                                                   
         OI    LCHG,LPRG           INDICATE RECORD CHANGE                       
         B     DEMU26                                                           
*                                                                               
DEMU24   ICM   RE,15,4(R9)         ** ADJUST THE PUT **                         
         BZ    DEMU26                                                           
         SR    R1,R1                                                            
         L     R0,0(R9)                                                         
         SRDA  R0,31                                                            
         M     R0,=F'1000'                                                      
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         C     R1,SVPJPUT          TEST PUT CHANGE                              
         BE    DEMU26                                                           
         ST    R1,SVPJPUT          YES-SAVE ADJUSTED PUT                        
         OI    LFLAG,LPUTADJ       INDICATE PUT HAS BEEN ADJUSTED               
*                                                                               
DEMU26   CLI   SVRTGSHR,OVERELEM   TEST RATING OVERRIDE                         
         BNE   *+16                                                             
         TM    LFLAG,LDEMFRZ       YES-                                         
         BO    *+8                                                              
         OI    0(R9),X'80'         RATING OVERRIDE BIT                          
         CLI   SVRTGSHR+3,OVERELEM TEST SHARE OVERRIDE                          
         BNE   *+8                                                              
         OI    4(R9),X'80'         YES                                          
         B     DEMU28                                                           
*                                                                               
DEMU27   SR    R1,R1               ** ADJUST THE RATING **                      
         L     R0,SVPJPUT                                                       
         SLL   R0,1                                                             
         SRDL  R0,32                                                            
         L     RE,4(R9)                                                         
         MR    R0,RE                                                            
         D     R0,=F'1000'                                                      
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,0(R9)                                                         
         CLM   R2,1,SVSTANO        TEST FIRST STATION                           
         BNE   DEMU28                                                           
         CLI   SVRTGSHR+3,OVERELEM YES-TEST SHARE OVERRIDE                      
         BNE   *+8                                                              
         OI    4(R9),X'80'         YES                                          
         ICM   R1,15,LARTGENT                                                   
         BZ    DEMU28                                                           
         CLC   5(3,R1),1(R9)       TEST CHANGE IN RATING                        
         BE    DEMU28                                                           
         MVC   4(4,R1),0(R9)       YES-MOVE IN NEW RATING OVERRIDE              
         OI    4(R1),DMODEMOV                                                   
         OI    0(R9),X'80'                                                      
         OI    LCHG,LRTG           INDICATE RATING CHANGE                       
*                                                                               
DEMU28   MVI   SVRTGSHR,0          REMOVE OVERRIDE INDICATORS                   
         MVI   SVRTGSHR+3,0                                                     
         L     R1,SVPJCUM          ACCUMULATE PROJ CUM SHARE                    
         SR    RE,RE                                                            
         ICM   RE,7,5(R9)                                                       
         AR    R1,RE                                                            
         ST    R1,SVPJCUM                                                       
         LA    R5,L'L1PJPROG(R5)   NEXT STATION                                 
         LA    R8,7(R8)                                                         
         LA    R9,8(R9)                                                         
         BCT   R2,DEMU16                                                        
*                                                                               
         BAS   RE,LFMTPJCM         FORMAT PROJ CUM SHARE                        
*                                                                               
DEMUX    B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* SINGLE DEMO UPGRADE ROUTINE                                         *         
* INPUT  : LDEMLST=SINGLE DEMO                                        *         
* OUTPUT : LDEMVAL=DEMO VALUE                                         *         
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
         TM    LFLAG2,LDAYOVR      TEST DAYS OVERRIDDEN                         
         BZ    *+10                                                             
         MVC   SPUPDAY,LDAYS                                                    
         MVC   SPUPTIM,BWDTIMES                                                 
         TM    LFLAG2,LTIMOVR      TEST TIMES OVERRIDDEN                        
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
         GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMLST,LDEMVAL                         
         MVC   SVRTGSVC,SPUPACTS   SAVE ACTUAL RATING SERVICE                   
*                                                                               
SDEMX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A SHARE/PUT OVERRIDE                                 *         
* OUTPUT : LASHPENT=A(SHARE/PUT ENTRY IN SHARE/PUT ELEMENT)           *         
*          LASHPEL=A(SHARE/PUT ELEMENT) IF NEEDED TO ADD              *         
***********************************************************************         
         SPACE 1                                                                
LADDSHP  NTR1  ,                   LOCAL ENTRY POINT                            
*                                                                               
ADDSHP   XC    APELEM,APELEM                                                    
         ICM   RE,15,LASHPEL       TEST SHARE/PUT ELEMENT EXISTS                
         BZ    ASHP2               NO                                           
         ZIC   RF,1(RE)            YES-ADD SHARE/PUT TO THE ELEMENT             
         LR    R0,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(RE)                                                  
         GOTO1 ADELELS,BWDRECD                                                  
         LR    RF,R0                                                            
         LA    RF,L'SHPDEMO(RF)                                                 
         STC   RF,APELEM+1                                                      
         GOTO1 AADDELS,BWDRECD                                                  
         BNE   ASHPX                                                            
         L     R1,LASHPEL                                                       
         AR    R1,R0                                                            
         ST    R1,LASHPENT                                                      
         B     ASHPX                                                            
*                                                                               
ASHP2    LA    RE,APELEM                                                        
         USING SHPEL,RE                                                         
         MVI   SHPELCD,SHPELCDQ                                                 
         LA    RF,SHPDEMO-SHPEL+L'SHPDEMO                                       
         STC   RF,SHPELLN                                                       
         GOTO1 AADDELS,BWDRECD                                                  
         BNE   ASHPX                                                            
         L     R1,ACPARM+16                                                     
         ST    R1,LASHPEL                                                       
         LA    R1,SHPDEMO-SHPEL(R1)                                             
         ST    R1,LASHPENT                                                      
*                                                                               
ASHPX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* FORMAT PROJECTED CUM SHARE                                          *         
***********************************************************************         
         SPACE 1                                                                
LFMTPJCM NTR1  ,                   LOCAL ENTRY POINT                            
*                                                                               
FMTPJCUM XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,L'CUMPROJ                                                 
         MVI   EBDECS,1                                                         
         LA    R1,SVPJCUM                                                       
         ST    R1,EBAIN                                                         
         L     R5,LATWA                                                         
         LA    R4,COMCUM                                                        
         USING CUMLINED,R4                                                      
         LA    R1,CUMPROJ                                                       
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    COMCUMH+6,FVOXMT                                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* READ MARKET RECORD                                                  *         
* INPUT  : R1=A(BINARY AGENCY MARKET NUMBER)                          *         
* OUTPUT : LSPLMKT = RATING SERVICE MARKET NUMBER                     *         
*          CC EQ - RATING SERVICE MARKET NUMBER NOT FOUND             *         
***********************************************************************         
         SPACE 1                                                                
RDMKT    XC    LSPLMKT,LSPLMKT                                                  
         LA    R2,IOKEY                                                         
         USING MKTRECD,R2                                                       
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  MKTKMKT,APDUB                                                    
         MVC   MKTKAGY,CUAALF                                                   
         L     R2,AIOAREA4                                                      
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSTAFIL+IORD                                                
         BNE   RDMKTX                                                           
         MVC   APBYTE,CLTSRC       EXTRACT RATING SERVICE MARKET NUMBER         
         CLI   SVRTGSVC,0                                                       
         BE    *+10                                                             
         MVC   APBYTE,SVRTGSVC                                                  
         MVI   APHALF,C'0'                                                      
         CLI   APBYTE,C'N'                                                      
         BE    *+8                                                              
         MVI   APHALF,C'1'                                                      
         CLC   MKTRS1,APHALF                                                    
         BNE   *+10                                                             
         MVC   LSPLMKT,MKTRSM1                                                  
         CLC   MKTRS2,APHALF                                                    
         BNE   RDMKTX                                                           
         MVC   LSPLMKT,MKTRSM2                                                  
*                                                                               
RDMKTX   OC    LSPLMKT,LSPLMKT                                                  
         B     XIT                                                              
         EJECT                                                                  
LOCALD   DSECT                                                                  
VDEMAND  DS    V                                                                
LAUPGEL  DS    A                                                                
LAODTEL  DS    A                                                                
LADEMEL  DS    A                                                                
LASHPEL  DS    A                                                                
LATWA    DS    A                                                                
LASAVE   DS    A                                                                
LARTGENT DS    A                                                                
LASHROVR DS    A                                                                
LAPUTOVR DS    A                                                                
LASHPENT DS    A                                                                
LOLDSHR  DS    F                                                                
LDEMOLD  DS    F                                                                
LDEMADJ  DS    F                                                                
LDNAME   DS    CL7                                                              
LDEM     DS    XL4                                                              
LDEMOVR  DS    XL19                                                             
LNSTASCR DS    XL1                                                              
LDPT     DS    CL1                                                              
LSVDPTLN DS    CL2                                                              
LDEMLST  DS    XL4                                                              
LDEMVAL  DS    XL4                                                              
LDAYS    DS    XL1                                                              
LTIMES   DS    XL4                                                              
LSPLMKT  DS    XL2                                                              
L1WPROF  DS    XL16                                                             
LDMUPBLK DS    (SPDEMUPL)X                                                      
*                                                                               
LCHG     DS    X                                                                
LBK      EQU   X'80'                                                            
LDEMO    EQU   X'40'                                                            
LPRG     EQU   X'20'                                                            
LRTG     EQU   X'10'                                                            
LSHR     EQU   X'08'                                                            
LPUT     EQU   X'04'                                                            
LPUTOVR  EQU   X'02'                                                            
LADJ     EQU   X'01'                                                            
*                                                                               
LFLAG    DS    X                                                                
LADJIMP  EQU   X'80'                                                            
LADJALL  EQU   X'40'                                                            
LDEMFRZ  EQU   X'20'                                                            
LGETBKS  EQU   X'08'                                                            
LPUTADJ  EQU   X'04'                                                            
LRTGOVR  EQU   X'02'                                                            
*                                                                               
LFLAG2   DS    X                                                                
LNEWTIME EQU   X'80'                                                            
LTIMOVR  EQU   X'40'                                                            
LNEWDAYS EQU   X'20'                                                            
LDAYOVR  EQU   X'10'                                                            
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
LINE1D   DSECT                                                                  
L1STA    DS    CL7                                                              
         DS    XL3                                                              
L1PROG1  DS    CL12                                                             
         DS    XL2                                                              
L1PROG2  DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
         DS    XL2                                                              
L1PJPROG DS    CL12                                                             
         SPACE 2                                                                
LINE2D   DSECT                                                                  
         DS    CL10                                                             
L2RS1    DS    CL12                                                             
         DS    XL2                                                              
L2RS2    DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
         DS    XL2                                                              
L2PJRS   DS    CL12                                                             
         SPACE 2                                                                
CUMLINED DSECT                                                                  
         DS    XL6                                                              
CUM1     DS    CL5                                                              
         DS    XL9                                                              
CUM2     DS    CL5                                                              
         DS    XL9                                                              
         DS    CL5                                                              
         DS    XL9                                                              
         DS    CL5                                                              
         DS    XL9                                                              
CUMPROJ  DS    CL5                                                              
         EJECT                                                                  
SAVED    DSECT                     OVERLAY SAVE AREA DSECT                      
*                                                                               
         DS    0H              *** RESERVED FOR BWS12 ***                       
*                                                                               
         ORG   SAVED+1280      *** RESERVED FOR BWS14 ***                       
SVCUMS   DS    XL(4*4)                                                          
SVPJCUM  DS    XL4                                                              
SVPUTVAL DS    XL(4*4)                                                          
SVPJPUT  DS    XL4                                                              
*                                                                               
SVDEMVAL DS    (NMAXSTA*2*4)XL4                                                 
SVDEMVLL EQU   *-SVDEMVAL                                                       
*                                                                               
SVPROJ   DS    (NMAXSTA*2)XL4                                                   
SVPROJL  EQU   *-SVPROJ                                                         
*                                                                               
SVBKS    DS    XL8                                                              
SVWKY    DS    XL2                                                              
SVWKYIND DS    XL1                                                              
SVONEBK  EQU   X'80'                                                            
SVSTA    DS    XL(NMAXSTA*7)                                                    
SVDEM    DS    XL4                                                              
SVPUT    DS    XL4                                                              
SVRTGSHR DS    XL7                                                              
SVSTANO  DS    X                                                                
SVUPFILE DS    C                                                                
SVUPGRD  DS    XL8                                                              
SVUPFRBK DS    XL2                                                              
SVUPFBL  DS    XL6                                                              
SVUPPUT  DS    CL1                                                              
SVUPSHR  DS    CL1                                                              
SVUPINP  DS    CL32                                                             
SVUPDAY  DS    X                                                                
SVUPTIM  DS    XL4                                                              
SVRTGSVC DS    C                                                                
*                                                                               
SVPROGS  DS    (NMAXSTA*4)XL(L'L1PROG1)                                         
SVPROGSL EQU   *-SVPROGS                                                        
*                                                                               
SVPJPRGS DS    (NMAXSTA)XL(L'L1PJPROG)                                          
SVPJPRGL EQU   *-SVPJPRGS                                                       
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
       ++INCLUDE SPNWSF5D                                                       
         EJECT                                                                  
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENCLST                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENCLST                                                      
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPNWS13   05/01/02'                                      
         END                                                                    
