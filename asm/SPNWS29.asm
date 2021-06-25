*          DATA SET SPNWS29    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T20729B,*                                                                
         TITLE 'BWS29 - BUYERS WORK SHEET - ROTATIONAL ANALYSIS'                
T20729   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20729**,RA,RR=RE                                              
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
         XC    BWHKEY,BWHKEY       BUILD HEADER KEY                             
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
         GOTO1 AVALMED,ROTMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         GOTO1 AVALBYR,ROTBYRH     VALIDATE BUYER                               
         BNE   VALKX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW         TEST BUYER PASSWORD                          
         BZ    VALK10                                                           
         GOTO1 AVALPWD             YES-VALIDATE IT                              
         BNE   VALKX                                                            
*                                                                               
VALK10   GOTO1 AVALCAM,ROTNUMH     VALIDATE CAMPAIGN NUMBER                     
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
         MVC   LTGTDEM,ESTDEMS     SET TARGET FROM ESTIMATE                     
         OC    INORTG,INORTG       EXCEPT IF OVERRIDDEN                         
         BZ    *+10                                                             
         MVC   LTGTDEM,INORTG                                                   
         MVI   LTGTDEM+3,X'FF'                                                  
         XC    DBLOCK,DBLOCK       GET DEMO NAME                                
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         LA    R0,ESTUSRNM                                                      
         XC    LDNAME,LDNAME                                                    
         GOTO1 VDEMOCON,APPARM,(1,LTGTDEM),(2,LDNAME),(C'S',DBLOCK),   C        
               (R0)                                                             
*                                                                               
         GOTO1 AVALSTA,ROTSTAH     VALIDATE STATION                             
         BNE   VALKX                                                            
         MVC   BWHKMKT,BMKT        STATION'S MARKET                             
*                                                                               
         GOTO1 AVALDAY,ROTDAYH     VALIDATE DAYS                                
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALTIM,ROTTIMH     VALIDATE TIMES                               
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALDPL,ROTDLNH     VALIDATE DAYPART/LENGTH                      
         BNE   VALKX                                                            
         CLI   CMPDPOPT,C'M'       TEST SUBDPT SCHEDULED UNDER MASTER           
         BNE   *+12                                                             
         CLI   DPTTYPE,C'S'        YES-TEST THIS IS A SUBDAYPART                
         BE    ESUB                    YES-ERROR                                
         CLI   BSLN,0              MAKE SURE SPOT LENGTH IS SPECIFIED           
         BE    ENOSLN                                                           
*                                                                               
         MVI   LFLAG,0                                                          
         MVI   LFLAG2,0                                                         
         GOTO1 AIO,DIRHI+IO1       READ THE HEADER RECORD                       
         BNE   VALKX                                                            
         CLC   BWHKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                  
         BE    VALK24                                                           
         CLI   APRECNUM,RECSID     NO HEADER-TEST FOR NSID                      
         BNE   VALK99                        NO-ERROR                           
         OI    TWAFLAG,TWANOHDR                                                 
         XC    BCMSEQ,BCMSEQ                                                    
         B     VALK30                                                           
*                                                                               
VALK24   GOTO1 AIO,FILGETU1        GET HEADER RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         MVC   BCMSEQ,BWHKSEQ      SAVE CAMPAIGN/MARKET SEQ NO                  
         MVI   BSTACD,0                                                         
         LA    R4,BWHFSTEL         LOOK FOR STATION IN HEADER                   
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
VALK26   CLI   0(R4),0                                                          
         BE    VALK28                                                           
         CLI   0(R4),BWHELCDQ                                                   
         BNE   VALK27                                                           
         USING BWHEL,R4                                                         
         IC    RE,BWHSEQ                                                        
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         CLC   QSTA,BWHSTA                                                      
         BNE   VALK27                                                           
         STC   RE,BSTACD           SET STATION SEQ NO                           
         B     VALK30                                                           
*                                                                               
VALK27   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALK26                                                           
*                                                                               
VALK28   CLI   APRECNUM,RECSID     STATION NOT FOUND-TEST FOR NSID              
         BNE   VALK99              NO-ERROR                                     
         OI    TWAFLAG,TWANOSTA    SET NO STATION                               
         TM    TWAFLAG,TWAFRST     TEST FIRST TIME                              
         BO    VALK30                                                           
         XC    APELEM,APELEM       NO-ADD STATION ELEMENT TO HEADER             
         LA    R4,APELEM                                                        
         MVI   BWHELCD,BWHELCDQ                                                 
         MVI   BWHELLN,BWHELLNQ                                                 
         LA    RF,1(RF)                                                         
         STC   RF,BWHSEQ                                                        
         CH    RF,=H'255'                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   BWHSTA,QSTA                                                      
         GOTO1 AADDELS,BWHRECD                                                  
*                                                                               
VALK30   CLI   APRECNUM,RECSID     TEST SID RECORD                              
         BNE   VALK36                                                           
         TM    TWAFLAG,TWAFRST     YES - TEST FIRST TIME                        
         BO    VALK32                    YES - BUILD BWS RECORD                 
         L     RE,LASAVE                 NO - RESTORE BWS RECORD                
         AH    RE,=Y(SVREC-SAVAREA)                                             
         L     R0,AIOAREA2                                                      
         LA    R1,2000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R3,AIOAREA2                                                      
         MVC   BCMSEQ,BWDKSEQ      SET CAMPAIGN/MARKET SEQ CODE                 
         MVC   BSTACD,BWDKELST     SET STATION CODE                             
         B     VALK48                                                           
*                                                                               
VALK32   NI    TWAFLAG,255-TWAFRST                                              
         GOTO1 ADTLBLD             BUILD BWS RECORD                             
         BNE   VALKX                                                            
         B     VALK48                                                           
*                                                                               
VALK36   TM    TWAMODE,TWAMLSM     BWS RECORD-TEST LIST/SELECT MODE             
         BZ    VALK38                                                           
         L     RE,ALSM             YES-SET THE KEY FROM SAVED STORAGE           
         LH    R1,LSMRDSP-LSMD(RE)                                              
         AR    R1,RE                                                            
         MVC   APRECKEY,LSMRKEY-LSMRTAB(R1)                                     
         B     VALK40                                                           
*                                                                               
VALK38   XC    APRECKEY,APRECKEY                                                
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
VALK40   MVC   IOKEY,APRECKEY      READ DETAIL RECORD                           
         MVI   APBYTE,0                                                         
         LA    R1,MINHI2                                                        
         B     VALK42+4                                                         
*                                                                               
VALK42   LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    VALK43                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    TWAMODE,TWAMLSM                                                  
         BO    VALK99                                                           
         B     VALK46                                                           
*                                                                               
VALK43   L     R3,AIOAREA2                                                      
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    VALK44                                                           
         CLC   IOKEY(13),IOKEYSAV  YES-KEY MUST MATCH                           
         BNE   VALK99                                                           
         B     VALK48                                                           
*                                                                               
VALK44   CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV   MATCH THE KEY                  
         BNE   VALK46                                                           
         CLC   BWDTIMES,BTIMES     MATCH TIMES                                  
         BNE   VALK42                                                           
         CLC   BWDDPT,BDPT         MATCH DAYPART                                
         BNE   VALK42                                                           
         CLC   BWDSLN,BSLN         MATCH SPOT LENGTH                            
         BNE   VALK42                                                           
         CLI   APBYTE,0            TEST SEQ NO ALREADY FOUND                    
         BNE   EDUPREC             YES-DUPLICATE RECORDS                        
         MVC   APBYTE,BWDKELSQ     NO-SAVE THE SEQ NO                           
         B     VALK42                                                           
*                                                                               
VALK46   CLI   APBYTE,0            TEST RECORD FOUND                            
         BE    VALK99                                                           
         LA    R3,APRECKEY         YES-REREAD IT                                
         MVC   BWDKELSQ,APBYTE                                                  
         MVC   IOKEY,APRECKEY                                                   
         GOTO1 AMIN,MINRD2                                                      
         BE    VALK48                                                           
         DC    H'0'                                                             
*                                                                               
VALK48   OI    APINDS,APIOKDIS     SET INDICATOR TO RECORD FOUND                
         L     R3,AIOAREA2                                                      
         LA    RE,BWDCOST1                                                      
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MONITOR ACTIVE              
         BZ    VALK50                                                           
         CLI   APRECNUM,RECSID     AND NOT SID RECORD                           
         BE    VALK50                                                           
         TM    APRECID,RIEFFDT2    YES-APRECID(1)=RECORD INDICATOR              
         BZ    *+8                                                              
         LA    RE,BWDCOST2                                                      
         TM    APRECID,RIEFFDT3                                                 
         BZ    VALK50                                                           
         LA    RE,BWDCOST3                                                      
*                                                                               
VALK50   ST    RE,LACOST           SAVE A(COST FIELD)                           
*                                                                               
         XC    LADEMEL,LADEMEL                                                  
         XC    LAUPGEL,LAUPGEL                                                  
         XC    LAODTEL,LAODTEL                                                  
         SR    R0,R0               LOCATE ELEMENTS IN DETAIL RECORD             
         LA    R8,BWDEL                                                         
*                                                                               
VALK52   CLI   0(R8),0                                                          
         BE    VALK54                                                           
         CLI   0(R8),DMOELCDQ                                                   
         BNE   *+8                                                              
         ST    R8,LADEMEL          A(DEMO ELEMENT)                              
         CLI   0(R8),UPGELCDQ                                                   
         BNE   *+8                                                              
         ST    R8,LAUPGEL          A(UPGRADE ELEMENT)                           
         CLI   0(R8),ODTELCDQ                                                   
         BNE   *+8                                                              
         ST    R8,LAODTEL          A(OVERRIDE ELEMENT)                          
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     VALK52                                                           
*                                                                               
VALK54   B     VALKX                                                            
*                                                                               
VALK99   MVC   FVMSGNO,=AL2(FVFERNF)    RECORD NOT FOUND                        
         LA    R1,ROTMEDH                                                       
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
         SR    R0,R0                                                            
         LA    R4,ROTCSTH          TURN PREV VALIDATED BITS ON                  
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
         MVI   SVRTGSVC,0                                                       
         XC    SVDEM,SVDEM                                                      
         MVI   SVDEM+3,X'FF'                                                    
         MVI   SVFLAG,SVFIRST      FIRST TIME FLAG                              
         MVC   SVDAYS,BDAYS        SET DAYS/TIMES                               
         MVC   SVTIMES,BTIMES                                                   
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
FSTR8    BAS   RE,COST             INSPECT THE COST FIELD                       
         BL    FSTRX                                                            
         L     R3,AIOAREA2         EDIT THE COST                                
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBFLOAT,C'$'                                                     
         MVC   EBAIN,LACOST                                                     
         LA    R1,ROTCST                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'ROTCST                                                  
         MVI   EBDECS,2                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    ROTCSTH+6,FVOXMT                                                 
*                                                                               
         BAS   RE,VALUPGRD         INSPECT UPGRADE FIELD                        
         BL    FSTRX                                                            
         BE    FSTR9                                                            
         MVC   ROTUPG,SVUPINP      NONE - EDIT UPGRADE EXPRESSION               
         OI    ROTUPGH+6,FVOXMT                                                 
*                                                                               
FSTR9    BAS   RE,DEMO             INSPECT DEMO FIELD                           
         BNE   FSTRX                                                            
         OC    MKTLKUP,MKTLKUP     TEST POSSIBILITY OF MARKET OVERRIDE          
         BZ    FSTR10                                                           
         MVC   LDEMLST,SVDEM       YES-CALL SPDEMUP TO GET RTG SERVICE          
         GOTO1 ASDEMUP                                                          
*                                                                               
FSTR10   MVC   APBYTE,BDAYS                                                     
         MVC   APFULL,BTIMES                                                    
         MVC   APHALF(1),BDPT                                                   
         MVC   APHALF+1(1),BSLN                                                 
         MVC   SVLIDAYS,APBYTE     DISPLAY LEAD-IN DAYS                         
         MVC   ROTLID,ROTDAY                                                    
         OI    ROTLIDH+6,FVOXMT                                                 
*                                                                               
         BAS   RE,GETLTIM          GET DEFAULT LEAD-IN/LEAD-OUT TIMES           
         SR    R9,R9                                                            
         LA    R1,ROTLITH                                                       
         BAS   RE,LEADTM           INSPECT LEAD-IN TIMES FIELD                  
         BL    FSTRX                                                            
         BE    FSTR11                                                           
         MVC   SVLITIME,SVDFLITM   MISSING-USE THE DEFAULT                      
         GOTO1 VUNTIME,APPARM,SVLITIME,ROTLIT  DISPLAY TIMES                    
         OI    ROTLITH+6,FVOXMT                                                 
         LA    R9,1                NOTE LEAD-IN TIMES WERE MISSING              
*                                                                               
FSTR11   MVC   SVLODAYS,APBYTE     DISPLAY LEAD-OUT DAYS                        
         MVC   ROTLOD,ROTDAY                                                    
         OI    ROTLODH+6,FVOXMT                                                 
*                                                                               
         LA    R1,ROTLOTH                                                       
         BAS   RE,LEADTM           INSPECT LEAD-OUT TIMES FIELD                 
         BL    FSTRX                                                            
         BH    *+14                                                             
         MVC   SVLOTIME,BTIMES                                                  
         B     FSTR13                                                           
         MVC   SVLOTIME,SVDFLOTM   MISSING-USE THE DEFAULT                      
         GOTO1 VUNTIME,APPARM,SVLOTIME,ROTLOT  DISPLAY LEAD-OUT TIMES           
         OI    ROTLOTH+6,FVOXMT                                                 
*                                                                               
FSTR13   MVC   BDAYS,APBYTE                                                     
         MVC   BTIMES,APFULL                                                    
         MVC   BDPT,APHALF                                                      
         MVC   BSLN,APHALF+1                                                    
         BAS   RE,GETDTLST         GET DAYS/TIMES LIST FOR LOOKUPS              
*                                                                               
         BAS   RE,BOOKS            INSPECT BOOK FIELDS                          
         BNE   FSTRX                                                            
         OC    SVBKS,SVBKS                                                      
         BNZ   FSTR14              BOOKS INPUT                                  
*                                                                               
         BAS   RE,GETBKS           GET THE BOOKS                                
*                                                                               
FSTR14   BAS   RE,DISBKS           DISPLAY THE BOOKS                            
*                                                                               
         MVC   ROTPRG,BWDPROG      DISPLAY PROGRAMMING                          
         OI    ROTPRGH+6,FVOXMT                                                 
*                                                                               
         GOTO1 ADEMUP              DO THE UPGRADES                              
*                                                                               
         GOTO1 AGETDEMS            GET THE DEMO VALUES                          
*                                                                               
         OI    ROTCMTH+6,FVOXMT    FORMAT BOTTOM LINE                           
         XC    ROTCMT,ROTCMT                                                    
         LA    R4,ROTCMT                                                        
         CLI   APRECNUM,RECSID                                                  
         BNE   *+14                                                             
         MVC   0(L'CMT1,R4),CMT1                                                
         LA    R4,L'CMT1+1(R4)                                                  
         TM    TWAMODE,TWAMLSM                                                  
         BZ    FSTRX                                                            
         MVC   0(L'CMT2,R4),CMT2                                                
*                                                                               
FSTRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   MVC   LSVDPTLN,BDPT       SAVE DAYPART/LENGTH                          
         CLI   APRECNUM,RECSID     TEST NSID RECORD                             
         BNE   DISK10                                                           
         OI    TWAFLAG,TWAFRST                                                  
         LA    R8,APRECKEY                                                      
         USING NSIDKEYD,R8                                                      
         GOTO1 AGETMED,NSAGYMD                                                  
         BNE   *+10                                                             
         MVC   ROTMED,QMED         MEDIA                                        
         MVI   LDPT,0                                                           
         SR    R0,R0                                                            
         L     R6,LASAVE                                                        
         USING SAVAREA,R6                                                       
         LA    R4,SAVKEYS          EXTRACT BUYER CAMPAIGN AND DAYPART           
*                                  FROM SAVED KEY                               
DISK2    CLI   0(R4),0                                                          
         BE    DISK8                                                            
         LA    RF,ROTBYR                                                        
         CLI   0(R4),KEYBYR        BUYER                                        
         BE    DISK4                                                            
         LA    RF,ROTNUM                                                        
         CLI   0(R4),KEYCAM        CAMPAIGN                                     
         BE    DISK4                                                            
         CLI   0(R4),KEYDPL        DAYPART                                      
         BNE   DISK6                                                            
         MVC   LDPT,2(R4)                                                       
         B     DISK6                                                            
*                                                                               
DISK4    ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     DISK6                                                            
         MVC   0(0,RF),2(R4)                                                    
*                                                                               
DISK6    ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     DISK2                                                            
*                                                                               
DISK8    XC    APDUB,APDUB                                                      
         MVC   APDUB+2(3),NSSTA                                                 
         GOTO1 VMSUNPK,APPARM,(X'80',APDUB),APWORK,APWORK+4                     
         MVC   ROTSTA(8),APWORK+4  STATION                                      
         CLI   ROTSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   ROTSTA+4,C' '                                                    
         CLI   APWORK+4,C'0'       TEST CABLE                                   
         BL    *+8                                                              
         MVI   ROTSTA+4,C'/'                                                    
         GOTO1 AGETDAY,NSDAY       DAYS                                         
         MVC   ROTDAY,QDAYS                                                     
         GOTO1 AGETTIM,NSTIME      TIMES                                        
         MVC   ROTTIM,QTIMES                                                    
         XC    ROTSUB,ROTSUB                                                    
         MVI   ROTSDP,0                                                         
         CLI   LDPT,0              TEST DAYPART IN THE KEY                      
         BNE   DISK9                                                            
         MVC   LDPT,NSDPT          NO-                                          
         CLI   CMPDPOPT,C'M'       TEST SUB-DAYPARTS UNDER MASTER               
         BNE   DISK9                                                            
         GOTO1 AGETDPT,LDPT        YES-                                         
         CLI   DPTTYPE,C'S'        TEST DAYPART IS A SUB-DAYPART                
         BNE   DISK9                                                            
         MVC   LDPT,DPTMAS         YES-THEN USE MASTER DAYPART                  
*                                                                               
DISK9    CLC   LDPT,NSDPT          TEST SUBDAYPART                              
         BE    *+16                                                             
         MVC   ROTSDP,NSDPT                                                     
         MVC   ROTSUB(10),=C'Subdaypart'                                        
         MVC   ROTDLN(1),LDPT                                                   
         ZIC   RF,NSSLN            SPOT LENGTH                                  
         BAS   RE,DISLEN                                                        
         B     DISKX                                                            
*                                                                               
DISK10   MVC   IOKEY(13),APRECKEY  GET BWS RECORD                               
         GOTO1 AMIN,MINRD2                                                      
         BNE   DISKX                                                            
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         GOTO1 AGETMED,BWDKAGMD                                                 
         BNE   *+10                                                             
         MVC   ROTMED,QMED         MEDIA                                        
         LA    R1,BWDKBYR          BUYER                                        
         ICM   R1,8,=X'B'                                                       
         GOTO1 AGETBYR                                                          
         MVC   ROTBYR,QBYR                                                      
         GOTO1 AGETCM,BWDKSEQ      CAMPAIGN                                     
         MVC   ROTNUM,QCAM                                                      
         MVC   ROTSTA(L'BWDSTA),BWDSTA   STATION                                
         CLI   ROTSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   ROTSTA+4,C' '                                                    
         CLI   BWDSTA,C'0'         TEST CABLE                                   
         BL    *+8                                                              
         MVI   ROTSTA+4,C'/'                                                    
         GOTO1 AGETDAY,BWDDAYS     DAYS                                         
         MVC   ROTDAY,QDAYS                                                     
         GOTO1 AGETTIM,BWDTIMES    TIMES                                        
         MVC   ROTTIM,QTIMES                                                    
         MVC   ROTDLN(1),BWDDPT    DAYPART                                      
         XC    ROTSUB,ROTSUB                                                    
         MVI   ROTSDP,0                                                         
         CLI   BWDSUBDP,0          TEST SUBDAYPART                              
         BE    *+16                                                             
         MVC   ROTSDP,BWDSUBDP                                                  
         MVC   ROTSUB(10),=C'Subdaypart'                                        
         ZIC   RF,BWDSLN                                                        
         BAS   RE,DISLEN                                                        
*                                                                               
DISKX    MVC   BDPT(2),LSVDPTLN    RESTORE DAYPART/LENGTH                       
         B     EXIT                                                             
         SPACE  2                                                               
DISLEN   CVD   RF,APDUB            DISPLAY SPOT LENGTH                          
         OI    APDUB+7,X'0F'                                                    
         LA    R1,ROTDLN+1                                                      
         CLI   ROTDLN,C'1'                                                      
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
DISREC   L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
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
         B     DISRX                                                            
*                                                                               
DISR2    MVI   LFLAG2,0                                                         
         TM    SVFLAG,SVFIRST      TEST FIRST TIME                              
         BO    DISR44              YES-DISPLAY                                  
         CLI   APPFKEY,PFK02       NO-TEST PF2                                  
         BNE   DISR3                                                            
         CLI   APRECNUM,RECSID     YES-TEST SID RECORD                          
         BE    DISR3                                                            
         MVI   APPFKEY,0           NO-IGNORE                                    
*                                                                               
DISR3    MVC   APHALF,BDPT         INSPECT DAYS AND TIMES FIELDS                
         MVC   APBYTE,BDAYS                                                     
         GOTO1 AVALDAY,ROTDAYH                                                  
         BNE   DISRX                                                            
         CLC   BDAYS,BWDDAYS       TEST DAYS = THE RECORD'S DAYS                
         BE    *+8                                                              
         OI    LFLAG2,LDAYOVR      NO-DAYS OVERRIDE                             
         CLC   BDAYS,SVDAYS        TEST DAYS ALTERED THIS TIME                  
         BE    *+14                                                             
         OI    LFLAG2,LNEWDAYS     YES-                                         
         MVC   SVDAYS,BDAYS                                                     
         MVC   BDAYS,APBYTE        RESTORE RECORD'S DAYS                        
*                                                                               
         MVC   APFULL,BTIMES                                                    
         GOTO1 AVALTIM,ROTTIMH                                                  
         BNE   DISRX                                                            
         MVC   BDPT(2),APHALF                                                   
         CLC   BTIMES,BWDTIMES     TEST TIMES = THE RECORD'S TIMES              
         BE    *+8                                                              
         OI    LFLAG2,LTIMOVR      NO-TIMES OVERRIDE                            
         CLC   BTIMES,SVTIMES      TEST TIMES ALTERED THIS TIME                 
         BE    *+18                                                             
         OI    LFLAG2,LNEWTIME     YES-                                         
         MVC   SVTIMES,BTIMES                                                   
         BAS   RE,GETLTIM          GET DEFAULT LEAD-IN/LEAD-OUT TIMES           
         MVC   BTIMES,APFULL       RESTORE RECORD'S TIMES                       
*                                                                               
         TM    ROTCSTH+FVIIND-FVIHDR,FVIVAL    TEST COST FIELD CHANGED          
         BO    DISR4                                                            
         OI    ROTCSTH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BAS   RE,COST                         INSPECT COST FIELD               
         BL    DISRX                                                            
         BE    DISR4                                                            
         L     RE,LACOST                                                        
         OC    0(L'BWDCOST1,RE),0(RE)          COST FIELD BLANK                 
         BZ    DISR4                                                            
         XC    0(L'BWDCOST1,RE),0(RE)                                           
         OI    LCHG,LCOST                                                       
*                                                                               
DISR4    TM    ROTUPGH+FVIIND-FVIHDR,FVIVAL    TEST UPGRADE CHANGED             
         BO    DISR8                                                            
         OI    ROTUPGH+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,VALUPGRD                                                      
         BL    DISRX                                                            
         BE    DISR8                                                            
         OC    LAUPGEL,LAUPGEL     UPGRADE REMOVED - TEST FOR UPGRD ELE         
         BZ    DISR6                                                            
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
DISR6    MVC   ROTUPG,CMPUPIN            DISPLAY CAMPAIGN UPGRADE               
         OI    ROTUPGH+6,FVOXMT                                                 
*                                                                               
DISR8    TM    ROTDEMH+FVIIND-FVIHDR,FVIVAL    TEST DEMO FIELD CHANGED          
         BO    DISR10                                                           
         OI    ROTDEMH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BAS   RE,DEMO                         INSPECT DEMO FIELD               
         BL    DISRX                                                            
*                                                                               
DISR10   TM    ROTPRGH+FVIIND-FVIHDR,FVIVAL   TEST FOR PROGRAMMING              
         BO    DISR14                         CHANGE                            
         OI    ROTPRGH+FVIIND-FVIHDR,FVIVAL                                     
         GOTO1 AFVAL,ROTPRGH                                                    
         BH    DISRX                                                            
         CLI   FVIFLD,C'='         TEST PROGRAM ADJACENCY CODE                  
         BNE   DISR12                                                           
         GOTO1 AVALADJ,FVIFLD+1    YES - VALIDATE                               
         BNE   DISRX                                                            
         CLC   BWDADJ,ADJCODE      VALID - TEST CHANGE                          
         BE    DISR14                                                           
         MVC   BWDADJ,ADJCODE      YES - SET ADJACENCY CODE                     
         OI    LCHG,LPRG                 INDICATE RECORD CHANGE                 
         B     DISR14                                                           
*                                                                               
DISR12   CLC   BWDPROG,FVIFLD      TEST PROGRAM CHANGE                          
         BE    DISR14                                                           
         MVC   BWDPROG,FVIFLD                                                   
         OI    BWDINDS,BWDIPRG                                                  
         OI    LCHG,LPRG                                                        
*                                                                               
DISR14   MVC   APHALF,BDPT                                                      
         MVC   APFULL,BTIMES                                                    
         TM    LFLAG2,LNEWDAYS     TEST DAYS OVERRIDDEN THIS TIME               
         BZ    DISR20                                                           
         CLC   SVLIDAYS,SVDAYS     YES-COMPARE PREV VALUE TO DAYS               
         BE    *+14                                                             
         OI    LCHG,LLEAD          CHANGE                                       
         MVC   SVLIDAYS,SVDAYS                                                  
         MVC   ROTLID,ROTDAY                                                    
         OI    ROTLIDH+6,FVOXMT                                                 
*                                                                               
DISR20   LA    R1,ROTLITH                                                       
         TM    FVIIND-FVIHDR(R1),FVIVAL   TEST LEAD-IN TIMES CHANGED            
         BZ    DISR22              YES                                          
         TM    LFLAG2,LNEWTIME     NO-TEST TIMES OVERRIDDEN THIS TIME           
         BO    DISR23              YES-USE DEFAULT LEAD-IN TIMES                
         B     DISR24                                                           
*                                                                               
DISR22   OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         BAS   RE,LEADTM           INSPECT LEAD-IN TIMES FIELD                  
         BL    DISRX                                                            
         BH    DISR23                                                           
         CLC   SVLITIME,BTIMES     TEST FOR CHANGE                              
         BE    DISR24                                                           
         OI    LCHG,LLEAD          YES                                          
         MVC   SVLITIME,BTIMES                                                  
         B     DISR24                                                           
*                                                                               
DISR23   MVC   SVLITIME,SVDFLITM   MISSING-USE THE DEFAULT                      
         XC    ROTLIT,ROTLIT                                                    
         GOTO1 VUNTIME,APPARM,SVLITIME,ROTLIT  DISPLAY TIMES                    
         OI    ROTLITH+6,FVOXMT                                                 
*                                                                               
DISR24   TM    LFLAG2,LNEWDAYS     TEST DAYS OVERRIDDEN THIS TIME               
         BZ    DISR28                                                           
         CLC   SVLODAYS,SVDAYS     YES-COMPARE PREV VALUE TO DAYS               
         BE    *+14                                                             
         OI    LCHG,LLEAD          CHANGE                                       
         MVC   SVLODAYS,SVDAYS                                                  
         MVC   ROTLOD,ROTDAY                                                    
         OI    ROTLODH+6,FVOXMT                                                 
*                                                                               
DISR28   LA    R1,ROTLOTH                                                       
         TM    FVIIND-FVIHDR(R1),FVIVAL   TEST LEAD-OUT TIMES CHANGED           
         BZ    DISR29              YES                                          
         TM    LFLAG2,LNEWTIME     NO-TEST TIMES OVERRIDDEN THIS TIME           
         BO    DISR30              YES-SET DEFAULT LEAD-OUT TIMES               
         B     DISR32                                                           
*                                                                               
DISR29   OI    FVIIND-FVIHDR(R1),FVIVAL   YES-                                  
         BAS   RE,LEADTM                                                        
         BL    FSTRX                                                            
         BH    DISR30                                                           
         CLC   SVLOTIME,BTIMES     TEST FOR CHANGE                              
         BE    DISR32                                                           
         OI    LCHG,LLEAD          YES                                          
         MVC   SVLOTIME,BTIMES                                                  
         B     DISR32                                                           
*                                                                               
DISR30   MVC   SVLOTIME,SVDFLOTM   MISSING-USE THE DEFAULT                      
         XC    ROTLOT,ROTLOT                                                    
         GOTO1 VUNTIME,APPARM,SVLOTIME,ROTLOT  DISPLAY LEAD-OUT TIMES           
         OI    ROTLOTH+6,FVOXMT                                                 
*                                                                               
DISR32   MVC   BDAYS,APBYTE                                                     
         MVC   BTIMES,APFULL                                                    
         MVC   BDPT(2),APHALF                                                   
         BAS   RE,GETDTLST         GET DAYS/TIMES LIST FOR LOOKUPS              
*                                                                               
         TM    LCHG,LUPG+LLEAD+LDEMO TEST UPGRADE, LI/LO OR DEMO CHANGE         
         BNZ   *+12                                                             
         TM    LFLAG2,LNEWTIME+LNEWDAYS OR DAYS/TIMES CHANGED THIS TIME         
         BZ    DISR34                                                           
         GOTO1 ADEMUP              YES - DO THE UPGRADES                        
*                                                                               
DISR34   TM    ROTBK1H+FVIIND-FVIHDR,FVIVAL    TEST ANY BOOK CHANGES            
         BZ    DISR36                                                           
         TM    ROTBK2H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR36                                                           
         TM    ROTBK3H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR36                                                           
         TM    ROTBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BO    DISR38                                                           
*                                                                               
DISR36   OI    ROTBK1H+FVIIND-FVIHDR,FVIVAL                                     
         OI    ROTBK2H+FVIIND-FVIHDR,FVIVAL                                     
         OI    ROTBK3H+FVIIND-FVIHDR,FVIVAL                                     
         OI    ROTBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,BOOKS            INSPECT THE BOOK FIELDS                      
         BNE   DISRX                                                            
         BAS   RE,DISBKS           MAKE SURE BOOKS ARE DISPLAYED                
*                                                                               
DISR38   TM    LCHG,LBK+LLEAD+LDEMO  TEST BOOK, LI/LO OR DEMO CHANGE            
         BNZ   *+12                                                             
         TM    LFLAG2,LNEWTIME+LNEWDAYS OR DAYS/TIMES CHANGED THIS TIME         
         BZ    DISR40                                                           
         GOTO1 AGETDEMS            YES-GET THE DEMO VALUES                      
*                                                                               
DISR40   DS    0H                                                               
         GOTO1 AVALRTG             VALIDATE RATING FIELD                        
         BNE   DISRX                                                            
*                                                                               
DISR42   CLI   APPFKEY,PFK02       TEST SID TRANSFER                            
         BE    DISR65              YES-TRANSFER                                 
*                                                                               
*                                  NO - TEST FOR CHANGES THAT WILL              
         TM    LCHG,LCOST+LUPG+LBK+LLEAD+LRTG+LDEMO AFFECT THE SCREEN           
         BNZ   DISR44                   YES- RE-DISPLAY CURRENT SCREEN          
         TM    LFLAG2,LNEWTIME+LNEWDAYS SAME FOR NEW DAYS/TIMES                 
         BNZ   DISR44                                                           
         MVI   APMODE,APMLRP       NO-TELL CONTROLLER THIS IS LAST              
         B     DISR66                 SCREEN                                    
*                                                                               
DISR44   BAS   RE,DISSRC           DISPLAY RATING SERVICE                       
         MVC   ROTLIP,SVLIPROG     DISPLAY LI/LO PROGRAMS                       
         OI    ROTLIPH+6,FVOXMT                                                 
         MVC   ROTLOP,SVLOPROG                                                  
         OI    ROTLOPH+6,FVOXMT                                                 
         SR    RE,RE               CLEAR THE SCREEN                             
         LA    R4,ROTROTH                                                       
         LA    R8,ROTCMTH                                                       
*                                                                               
DISR46   IC    RE,0(R4)                                                         
         SH    RE,=H'9'                                                         
         BNM   *+6                                                              
         DC    H'0'                TWA MUST BE FU--ED                           
         TM    1(R4),X'20'                                                      
         BO    DISR48                                                           
         EX    RE,DRCLC                                                         
         BE    DISR48                                                           
         EX    RE,DROC                                                          
         BZ    DISR48                                                           
         EX    RE,DRXC                                                          
         OI    6(R4),FVOXMT                                                     
*                                                                               
DISR48   LA    R4,9(RE,R4)                                                      
         CR    R4,R8                                                            
         BL    DISR46                                                           
*                                                                               
         XC    EBLOCK,EBLOCK       FORMAT THE BOOK VALUES                       
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         LA    R0,6                6 DAYS/TIMES                                 
         LA    R4,SVDEMVAL                                                      
         LA    R2,ROTROTH                                                       
*                                                                               
DISR50   LA    R9,4                4 BOOKS                                      
         OI    6(R2),FVOXMT                                                     
         LA    R1,8(R2)                                                         
         USING LINED,R1                                                         
         LA    R8,LRTG1                                                         
*                                                                               
DISR52   ST    R8,EBAOUT                                                        
         MVI   EBLOUT,L'LRTG1                                                   
         ST    R4,EBAIN                                                         
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         ZIC   RE,EBLOUT                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R8),0(R8)                                                    
         OC    0(4,R4),0(R4)                                                    
         BZ    DISR54                                                           
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR54   LA    R4,4(R4)                                                         
         LA    R8,LRTG2-LRTG1(R8)                                               
         BCT   R9,DISR52           DO FOR ALL BOOKS                             
*                                                                               
         CH    R0,=H'6'                                                         
         BNE   *+12                                                             
         LA    R2,ROTLIQH                                                       
         B     DISR55                                                           
         LA    R1,2                                                             
         SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,*-6                                                           
*                                                                               
DISR55   BCT   R0,DISR50           DO FOR ALL DAYS/TIMES                        
*                                                                               
         OI    ROTRTGH+6,FVOXMT    FORMAT PROJECTED VALUES                      
         OI    ROTCPPH+6,FVOXMT                                                 
         LA    R2,ROTRTG                                                        
         LA    R4,SVPROJ                                                        
         LA    R9,6                                                             
*                                                                               
DISR56   MVI   EBLOUT,L'ROTRTG     FORMAT PROJECTED RATING                      
         ST    R2,EBAOUT                                                        
         LA    RE,APFULL                                                        
         ST    RE,EBAIN                                                         
         XC    0(L'ROTRTG,R2),0(R2)                                             
         MVC   APFULL,0(R4)                                                     
         NI    APFULL,255-X'80'                                                 
         OC    APFULL,APFULL                                                    
         BZ    DISR58                                                           
         MVI   EBFLOAT,0                                                        
         TM    0(R4),X'80'                                                      
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR58   MVI   EBLOUT,L'ROTCPP     FORMAT THE CPP/CPM                           
         LA    RE,ROTCPP                                                        
         CH    R9,=H'6'                                                         
         BE    *+8                                                              
         LA    RE,LCPP-LPROJECT(R2)                                             
         ST    RE,EBAOUT                                                        
         XC    0(L'ROTCPP,RE),0(RE)                                             
         L     R1,LACOST                                                        
         ICM   RE,15,0(R1)                                                      
         BZ    DISR60                                                           
         OC    APFULL,APFULL                                                    
         BZ    DISR60                                                           
         SRDA  RE,32                                                            
         M     RE,=F'20'                                                        
         D     RE,APFULL                                                        
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         ST    RF,APFULL                                                        
         MVI   EBDECS,2                                                         
         MVI   EBFLOAT,0                                                        
         CLC   APFULL,=F'100000'                                                
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         MVI   EBFLOAT,C'$'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR60   BCT   R9,*+8              DO FOR ALL DAYS/TIMES                        
         B     DISR64                                                           
         CH    R9,=H'5'                                                         
         BNE   *+12                                                             
         LA    R8,ROTLIQH                                                       
         B     DISR62                                                           
         ZIC   RE,0(R8)                                                         
         AR    R8,RE                                                            
         IC    RE,0(R8)                                                         
         AR    R8,RE                                                            
*                                                                               
DISR62   OI    6(R8),FVOXMT                                                     
         LA    R2,LPROJECT-LINED+8(R8)                                          
         LA    R4,4(R4)                                                         
         B     DISR56                                                           
*                                                                               
DISR64   OI    ROTIPRH+6,FVOXMT    DISPLAY LEAD-IN AND LEAD-OUT                 
         LA    R2,ROTIPR           PROGRAMS FOR ALL 4 BOOKS                     
         USING LINE2D,R2                                                        
         LA    R8,LPRGBK1                                                       
         LA    R4,SVLIPRGS                                                      
         LA    R0,4                                                             
         MVC   0(L'LPRGBK1,R8),0(R4)                                            
         LA    R4,16(R4)                                                        
         LA    R8,LPRGBK2-LPRGBK1(R8)                                           
         BCT   R0,*-14                                                          
         MVC   LPRGPROJ,SVLIPROG   PROJECTED LEAD-IN PROGRAM                    
*                                                                               
         OI    ROTOPRH+6,FVOXMT    LEAD-OUT -                                   
         LA    R2,ROTOPR                                                        
         USING LINE2D,R2                                                        
         LA    R8,LPRGBK1                                                       
         LA    R4,SVLOPRGS                                                      
         LA    R0,4                                                             
         MVC   0(L'LPRGBK1,R8),0(R4)                                            
         LA    R4,16(R4)                                                        
         LA    R8,LPRGBK2-LPRGBK1(R8)                                           
         BCT   R0,*-14                                                          
         MVC   LPRGPROJ,SVLOPROG   PROJECTED LEAD-OUT PROGRAM                   
*                                                                               
DISR65   CLI   APRECNUM,RECSID     TEST SID RECORD                              
         BNE   DISR66                                                           
         L     R0,LASAVE           YES - SAVE BWS RECORD THAT'S BEEN            
         AH    R0,=Y(SVREC-SAVAREA)      BUILT FROM IT                          
         LA    R1,2000                                                          
         L     RE,AIOAREA2                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         CLI   APPFKEY,PFK02       TEST SID TRANSFER                            
         BNE   DISRX                                                            
         MVI   APPFKEY,0                                                        
         GOTO1 AXFRADD             YES - TRANSFER SID TO BWS                    
         BNE   DISRX                                                            
         MVI   SCPFKEY,PFK05             AND RETURN TO LIST SCREEN              
         MVI   APMODE,APMLRP                                                    
         B     DISRX                                                            
*                                                                               
DISR66   TM    SVFLAG,SVFIRST      TEST NOT FIRST TIME                          
         BO    DISRX                                                            
         TM    LCHG,LCOST+LUPG+LPRG+LRTG    AND ANY DETAIL RECORD               
         BZ    DISRX                        CHANGES                             
         MVC   IOKEY(13),APRECKEY                                               
         GOTO1 AMIN,MINWRT2                 YES - PUT THE RECORD                
         BE    DISRX                                                            
         DC    H'0'                                                             
*                                                                               
DISRX    NI    SVFLAG,255-SVFIRST                                               
         B     EXIT                                                             
         SPACE 2                                                                
DRCLC    CLC   8(0,R4),SPACES      EXECUTED INSTRUCTIONS                        
DROC     OC    8(0,R4),8(R4)                                                    
DRXC     XC    8(0,R4),8(R4)                                                    
         EJECT                                                                  
***********************************************************************         
* GET THE BOOKS                                                       *         
***********************************************************************         
         SPACE 1                                                                
GETBKS   NTR1                                                                   
         OC    CMPBOOKS,CMPBOOKS   NONE - TEST FOR CAMPAIGN DEFINED BKS         
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
         MVC   DBSELDAT,=X'630C'   LATEST BOOK LIMIT = DEC99                    
         MVI   DBFUNCT,DBGETTLB                                                 
         L     RF,ACOM                                                          
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),APPARM,DBLOCK,0                                             
         XC    SVBKS,SVBKS         CLEAR SAVED BOOKS FIELD                      
         LA    R4,SVBKS+6                                                       
         LA    R1,4                                                             
         MVC   0(2,R4),DBACTBK                                                  
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    GETB12              NO - THEN INCLUDE LATEST BOOK                
*                                                                               
GETB2    MVC   0(2,R4),DBACTBK     DETERMINE ALL FOUR BOOKS                     
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    *+14                                                             
         CLC   0(2,R4),SVUPFRBK    YES - COMPARE TO SHARE BOOK MONTH            
         BE    GETB12                                                           
         CLC   0(2,R4),SVUPGRD+2   COMPARE TO PUT MONTH                         
         BE    GETB12                                                           
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
GETB4    CLC   1(1,R4),0(RE)                                                    
         BE    GETB12                                                           
         BL    GETB6                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,GETB4                                                         
*                                                                               
GETB6    CR    R0,R9                                                            
         BNE   GETB8                                                            
         LA    RF,0(R9,R8)                                                      
         BCTR  RF,0                                                             
         MVC   1(1,R4),0(RF)                                                    
         ZIC   RF,DBACTBK                                                       
         BCTR  RF,0                                                             
         STC   RF,0(R4)                                                         
         B     GETB12                                                           
*                                                                               
GETB8    BCTR  RE,0                                                             
         SR    RF,RF                                                            
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
GETB12   MVC   DBACTBK,0(R4)                                                    
         ZIC   RF,1(R4)                                                         
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
GETB14   BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         BCT   R1,GETB2                                                         
*                                                                               
GETBX    B     EXIT                                                             
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
         OI    ROTSRCH+6,FVOXMT                                                 
         MVC   ROTSRC,=C'NSI'                                                   
         CLI   APBYTE,C'N'                                                      
         BE    *+10                                                             
         MVC   ROTSRC,=C'ARB'                                                   
         CLI   CUDMED,C'C'                                                      
         BNE   DISSRCX                                                          
         MVC   ROTSRC,=C'CSI'                                                   
         CLI   APBYTE,C'N'                                                      
         BE    DISSRCX                                                          
         MVC   ROTSRC,=C'BBM'                                                   
*                                                                               
DISSRCX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE BOOKS                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISBKS   NTR1                                                                   
         LA    R0,4                DISPLAY THE BOOKS                            
         LA    R4,SVBKS                                                         
         LA    R8,ROTBK1H                                                       
         LA    R9,C'1'                                                          
         MVI   APWORK+2,1                                                       
*                                                                               
DISB2    XC    8(L'ROTBK1,R8),8(R8)                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    DISB4                                                            
         MVC   APWORK(2),0(R4)                                                  
         GOTO1 VDATCON,APPARM,(3,APWORK),(6,10(R8))                             
*                                                                               
DISB4    OI    6(R8),FVOXMT                                                     
         LA    R4,2(R4)                                                         
         ZIC   RF,0(R8)                                                         
         AR    R8,RF                                                            
         LA    R9,1(R9)                                                         
         BCT   R0,DISB2                                                         
*                                                                               
DISBX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT THE DEMO FIELD                                              *         
* OUTPUT : LCHG = LDEMO IF THE DEMO CHANGES                           *         
*          CC EQ OK                                                   *         
*             NE ERROR                                                *         
***********************************************************************         
         SPACE 1                                                                
DEMO     NTR1  ,                                                                
         GOTO1 AFVAL,ROTDEMH       VALIDATE THE DEMO FIELD                      
         BH    DEMOX                                                            
         BE    DEMO2                                                            
         MVC   ROTDEM,LDNAME       MISSING-USE PRIMARY DEMO                     
         OI    ROTDEMH+6,FVOXMT                                                 
         CLC   SVDEM,LTGTDEM       TEST ALREADY PRIMARY DEMO                    
         BE    DEMOX                                                            
         MVC   SVDEM,LTGTDEM       NO                                           
         OI    LCHG,LDEMO                                                       
         B     DEMOX                                                            
*                                                                               
DEMO2    LA    R0,ESTUSRNM                                                      
         GOTO1 VDEMOVAL,APPARM,(1,ROTDEMH),(1,APDUB),(C'S',DBLOCK),(R0)         
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     DEMOX                                                            
         CLC   SVDEM(3),APDUB      TEST DEMO HAS CHANGED                        
         BE    DEMOX                                                            
         MVC   SVDEM(3),APDUB      YES                                          
         MVI   SVDEM+3,X'FF'                                                    
         OI    LCHG,LDEMO                                                       
         B     DEMOX                                                            
*                                                                               
DEMOX    CLC   FVMSGNO,=AL2(FVFOK)                                              
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
COST     NTR1                                                                   
         GOTO1 AFVAL,ROTCSTH                                                    
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
* INSPECT UPGRADE FIELD                                               *         
* OUTPUT : LCHG = LUPG IF UPGRADE IN RECORD CHANGED                   *         
*          CC EQ  - UPGRADE FOUND AND RECORD CHANGED IF NECESSARY     *         
*             LO  - BAD ERROR                                         *         
*             HI  - UPGRADE NOT FOUND                                 *         
***********************************************************************         
         SPACE 1                                                                
VALUPGRD NTR1                                                                   
         CLI   ROTUPGH+5,0         TEST UPGRADE ENTERED                         
         BE    UPGRX1                                                           
         MVI   APFLAG,X'F8'                                                     
         GOTO1 AVALUPG,ROTUPGH     VALIDATE UPGRADE FIELD                       
         BNE   UPGRX2              ERROR                                        
         MVI   FVINDX,0                                                         
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
         MVC   UPGINPUT(L'ROTUPG),ROTUPG                                        
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
* INSPECT LEAD-IN/LEAD-OUT TIMES FIELDS                               *         
* INPUT  : R1=A(TIMES FIELD HEADER)                                   *         
* OUTPUT : BTIMES SET TO TIMES                                        *         
*          CC EQ  - OK                                                *         
*             LO  - BAD ERROR                                         *         
*             HI  - DAYS NOT FOUND                                    *         
***********************************************************************         
         SPACE 1                                                                
LEADTM   NTR1                                                                   
         GOTO1 AFVAL               TEST ANYTHING ENTERED                        
         BH    LEADTMX2                                                         
         BL    LEADTMX1                                                         
         GOTO1 AVALTIM             YES-VALIDATE                                 
         B     LEADTMX2                                                         
*                                                                               
LEADTMX1 CR    RA,RB                                                            
         B     EXIT                                                             
*                                                                               
LEADTMX2 CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET DEFAULT LEAD-IN AND LEAD-OUT TIMES                              *         
* INPUT  : SVTIMES=TIMES                                              *         
* OUTPUT : SVDFLITM=LEAD-IN TIMES                                     *         
*          SVDFLOTM=LEAD-OUT TIMES                                    *         
***********************************************************************         
         SPACE 1                                                                
GETLTIM  LR    R0,RE                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,SVTIMES        START TIME                                   
         D     RE,=F'100'                                                       
         LR    R1,RF               R1=RF=HOURS                                  
         LR    R8,RE               R8=RE=MINUTES                                
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'           ROUND MINUTES DOWN TO QTR HOUR               
         SR    R8,RE                                                            
         LR    RF,R1                                                            
         MH    RF,=H'100'          HOURS X 100                                  
         AR    RF,R8               + MINUTES                                    
         STCM  RF,3,SVDFLITM+2     SAVE LEAD-IN END TIME                        
         S     R8,=F'15'           SUBTRACT 15 MIN                              
         BNM   GETLTIM2                                                         
         LA    R8,60(R8)           BACK TO HOUR BEFORE                          
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BNM   GETLTIM2                                                         
         LA    R1,24(R1)                                                        
*                                                                               
GETLTIM2 MH    R1,=H'100'          HOURS X 100                                  
         AR    R1,R8               + MINUTES                                    
         STCM  R1,3,SVDFLITM       SAVE LEAD-IN START TIME                      
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,SVTIMES+2      END TIME                                     
         BNZ   *+8                                                              
         ICM   RF,3,SVTIMES        ZERO-FORCE END TO START                      
         D     RE,=F'100'                                                       
         LR    R1,RF               R1=RF=HOURS                                  
         LR    R8,RE               R8=RE=MINUTES                                
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'           ROUND MINUTES UP TO QTR HOUR                 
         LNR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RE,15(RE)                                                        
         AR    R8,RE                                                            
         CH    R8,=H'60'                                                        
         BL    *+10                                                             
         SR    R8,R8                                                            
         LA    R1,1(R1)                                                         
         LR    RF,R1                                                            
         MH    RF,=H'100'          HOURS X 100                                  
         AR    RF,R8               + MINUTES                                    
         STCM  RF,3,SVDFLOTM       SAVE LEAD-OUT START TIME                     
         AH    R8,=H'15'           ADD 15 MIN                                   
         CH    R8,=H'60'                                                        
         BL    GETLTIM4                                                         
         SR    R8,R8               NEXT HOUR                                    
         LA    R1,1(R1)                                                         
         CH    R1,=H'24'                                                        
         BNH   GETLTIM4                                                         
         SH    R1,=H'24'                                                        
*                                                                               
GETLTIM4 MH    R1,=H'100'          HOURS X 100                                  
         AR    R1,R8               + MINUTES                                    
         STCM  R1,3,SVDFLOTM+2     SAVE LEAD-OUT END TIME                       
*                                                                               
GETLTIMX LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET DAYS/TIMES LIST FOR DEMO LOOKUPS                                *         
***********************************************************************         
         SPACE 1                                                                
GETDTLST LR    R0,RE                                                            
         XC    SVDTLST,SVDTLST                                                  
         LA    R8,SVDTLST                                                       
         MVC   0(1,R8),SVDAYS      1ST IS BUY DAYS/TIMES                        
         MVC   1(4,R8),SVTIMES                                                  
*                                                                               
         MVC   5(1,R8),SVLIDAYS    2ND IS LEAD-IN TIMES                         
         MVC   6(2,R8),SVLITIME                                                 
         MVC   8(2,R8),SVLITIME+2                                               
*                                                                               
         MVC   10(1,R8),SVLODAYS   3RD IS LEAD-OUT TIMES                        
         MVC   11(2,R8),SVLOTIME                                                
         MVC   13(2,R8),SVLOTIME+2                                              
*                                                                               
         MVC   15(1,R8),SVDAYS     4TH IS ROTATION + LEAD-IN/LEAD-OUT           
         MVC   16(2,R8),SVLITIME                                                
         CLC   SVLITIME(2),SVTIMES                                              
         BNH   *+10                                                             
         MVC   16(2,R8),SVTIMES                                                 
         MVC   18(2,R8),SVLOTIME+2                                              
         CLC   SVLOTIME+2(2),SVTIMES+2                                          
         BNL   *+10                                                             
         MVC   18(2,R8),SVTIMES+2                                               
*                                                                               
         MVC   20(1,R8),SVDAYS     5TH IS IN-BREAK AVERAGE                      
         SR    RF,RF                                                            
         ICM   RF,3,SVDFLITM+2     I MIN BEFORE END OF DEFAULT LEAD-IN          
         BAS   R9,MINBEFOR                                                      
         STCM  RF,3,21(R8)                                                      
         XC    23(2,R8),23(R8)                                                  
*                                                                               
         MVC   25(1,R8),SVLODAYS   6TH IS OUT-BREAK AVERAGE                     
         SR    RF,RF                                                            
         ICM   RF,3,SVDFLOTM       1 MIN AFTER START OF DEFAULT                 
         LA    RF,1(RF)            LEAD-OUT                                     
         STCM  RF,3,26(R8)                                                      
         XC    28(2,R8),28(R8)                                                  
*                                                                               
DTLSTX   LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
MINBEFOR SR    RE,RE               GET MINUTE BEFORE QTR HOUR END               
         D     RE,=F'100'          TIME IS IN RF                                
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BNM   MINBEFO2                                                         
         LA    RE,60(RE)                                                        
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNM   MINBEFO2                                                         
         LA    RF,24(RF)                                                        
MINBEFO2 MH    RF,=H'100'                                                       
         AR    RF,RE               TIME RETURNED IN RF                          
         BR    R9                                                               
         EJECT                                                                  
***********************************************************************         
* INSPECT BOOK FIELDS                                                 *         
* OUTPUT : LCHG = LBK IF BOOKS WERE CHANGED                           *         
*          CC EQ  - OK                                                *         
*             NE  - ERROR                                             *         
***********************************************************************         
         SPACE 1                                                                
BOOKS    NTR1                                                                   
         LA    R4,ROTBK1H                                                       
         LA    R8,SVBKS                                                         
         LA    R0,4                                                             
*                                                                               
BOOK2    LR    R1,R4               VALIDATE BOOK FIELD                          
         GOTO1 AFVAL                                                            
         BH    BOOKX                                                            
         BE    BOOK4                                                            
         OC    0(2,R8),0(R8)                                                    
         BZ    BOOK6                                                            
         XC    0(2,R8),0(R8)                                                    
         OI    LCHG,LBK                                                         
         B     BOOK6                                                            
*                                                                               
BOOK4    LA    R2,FVIFLD           VALIDATE MONTH/YEAR                          
         ZIC   RF,FVILEN                                                        
         CLI   0(R2),C' '                                                       
         BH    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   RF,*-12                                                          
         B     BOOK9                                                            
         GOTO1 VDATVAL,APPARM,(2,(R2)),APWORK                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    BOOK9               INVALID DATE                                 
         GOTO1 VDATCON,APPARM,(0,APWORK),(3,APWORK+6)                           
         CLC   0(2,R8),APWORK+6    COMPARE TO OLD VALUE                         
         BE    BOOK6                                                            
         MVC   0(2,R8),APWORK+6    SAVE BOOK YR/MN                              
         OI    LCHG,LBK                                                         
*                                                                               
BOOK6    SR    RF,RF               NEXT BOOK                                    
         ICM   RF,1,0(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,RF                                                            
         LA    R8,2(R8)                                                         
         BCT   R0,BOOK2            DO FOR ALL BOOKS                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     BOOKX                                                            
*                                                                               
BOOK9    MVC   FVMSGNO,=AL2(FVIBOOK)                                            
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
* CONSTANTS                                                                     
*                                                                               
AXTRA    DS    0F                  EXTENSION ROUTINE ADDRESSES                  
AVALRTG  DS    A                                                                
ADEMUP   DS    A                                                                
ASDEMUP  DS    A                                                                
AGETDEMS DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
UMAJBKS  DC    XL4'0205070B'       USA MAJOR BOOKS                              
CMAJBKSA DC    XL3'03070B'         CANADIAN BBM BOOKS                           
CMAJBKSN DC    XL4'0103080B'       CANADIAN CSI BOOKS                           
CMT1     DC    CL8'PF2=Tran'                                                    
CMT2     DC    CL36'PF4=Frst PF5=Curr PF6=Next PF12=Quit'                       
SPACES   DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,**B28X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALRTG                                                           
         B     DEMUP                                                            
         B     SDEMUP2                                                          
         B     GETDEMS                                                          
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE RATING FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALRTG   LA    R1,ROTRTGH                                                       
         TM    FVIIND-FVIHDR(R1),FVIVAL   TEST RATING FIELD CHANGED             
         BO    VALRTGX                                                          
         OI    FVIIND-FVIHDR(R1),FVIVAL   YES-SET PREVIOUSLY VALIDATED          
         GOTO1 AFVAL               VALIDATE RATING FIELD                        
         BNE   VALRTGX                                                          
*                                                                               
         ICM   R1,15,LADEMEL       LOCATE DEMO IN BWS DETAIL RECORD             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DMOEL,R1                                                         
         ZIC   RF,DMOELLN                                                       
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    RE,L'DMODEMO                                                     
         LA    R4,DMODEMO                                                       
         CLC   1(2,R4),SVDEM+1                                                  
         BE    VALRTG2             R4 = A(DEMO ENTRY IN DEMO ELEMENT)           
         BXLE  R4,RE,*-10                                                       
*                                                                               
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
         DROP  R1                                                               
*                                                                               
VALRTG2  XC    APDUB,APDUB                                                      
         XC    LDEMOLD,LDEMOLD     SAVE CURRENT DEMO VALUE                      
         MVC   LDEMOLD+1(3),5(R4)                                               
         ZIC   R8,FVILEN                                                        
         LR    RF,R8                                                            
         LA    R1,FVIFLD           REMOVE THE * IF ANY                          
*                                                                               
VALRTG4  CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    *+16                YES-REMOVE THE OVERRIDE                      
         LA    R1,1(R1)                                                         
         BCT   RF,VALRTG4                                                       
         B     VALRTG6                                                          
         MVC   LDEMLST(3),0(R4)    SINGLE UPGRADE FOR RATING                    
         MVI   LDEMLST+3,X'FF'                                                  
         BAS   RE,SDEMUP                                                        
         MVC   4(4,R4),LDEMVAL                                                  
         MVC   SVPROJ(4),LDEMVAL                                                
         OI    LCHG,LRTG                                                        
         B     VALRTG8                                                          
*                                                                               
VALRTG6  GOTO1 VCASHVAL,APPARM,(1,FVIFLD),(R8)                                  
         CLI   APPARM,X'FF'                                                     
         BE    VALRTG99                                                         
         CLC   5(3,R4),APPARM+5    TEST FOR RATING CHANGE                       
         BNE   *+12                                                             
         TM    LFLAG2,LTIMOVR+LDAYOVR  OR OVERRIDDEN DAYS/TIMES                 
         BZ    VALRTGX                                                          
         MVC   4(4,R4),APPARM+4    YES - MOVE IN NEW RATING                     
         OI    4(R4),DMODEMOV            OVERRIDE INDICATOR                     
         OI    LCHG,LRTG                                                        
         MVC   SVPROJ(4),APPARM+4                                               
         OI    SVPROJ,X'80'              INDICATE OVERRIDE                      
*                                                                               
VALRTG8  TM    INFIND,INFINOAD    TEST OPTION TO NOT AUTO ADJUST DEMOS          
         BO    VALRTGX                                                          
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT   NO-TEST AUTO DEMO           
         BZ    VALRTGX                                 ADJUSTMENTS              
         OC    LDEMOLD,LDEMOLD     YES-TEST OLD DEMO VALUE = 0                  
         BZ    VALRTG98            YES-ERROR                                    
         BAS   RE,DEMADJ           NO-DO THE ADJUSTMENTS                        
         B     VALRTGX                                                          
*                                                                               
VALRTG98 MVC   FVMSGNO,=AL2(FVIDADJ)                                            
         B     VALRTGX                                                          
*                                                                               
VALRTG99 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALRTGX  CLC   FVMSGNO,=AL2(FVFOK)                                              
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
         NI    LFLAG,255-LADJALL                                                
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
         BNE   DEMAX               NO - NO ADJUSTMENTS                          
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
         L     R5,LADEMEL          SCAN ALL DEMOS IN DEMO ELEMENT               
         ZIC   R3,1(R5)                                                         
         AR    R3,R5                                                            
         BCTR  R3,0                                                             
         LA    R5,DMODEMO-DMOEL(R5)                                             
         LA    R2,L'DMODEMO                                                     
*                                                                               
DEMA6    CLC   1(2,R5),1(R4)       TEST OUR DEMO                                
         BE    DEMA12              YES - NO ADJ                                 
         TM    4(R5),DMODEMOV      TEST ALREADY MANUALLY OVERRIDDEN             
         BO    DEMA12              YES - NO ADJ                                 
         TM    LFLAG,LADJIMP       TEST TARGET IMPRESSION ADJUST                
         BZ    DEMA8                                                            
         CLI   1(R5),C'I'          YES - TEST ITS THE IMP WE WANT               
         BNE   DEMA8                                                            
         CLC   2(1,R5),2(R4)                                                    
         BE    DEMA10                    YES - ADJUST                           
*                                                                               
DEMA8    TM    LFLAG,LADJALL       TEST ADJUST ALL                              
         BZ    DEMA12              NO                                           
         CLI   1(R5),C'I'          YES - TEST ITS AN IMPRESSION                 
         BNE   DEMA10                    NO - GO AND ADJUST                     
         L     R8,LADEMEL                YES - CHECK THAT ITS RATING            
         LA    R8,DMODEMO-DMOEL(R8)            HAS NOT BEEN MANUALLY            
         CLI   1(R8),C'R'                      OVERRIDDEN                       
         BNE   *+14                                                             
         CLC   2(1,R8),2(R5)                                                    
         BE    *+12                                                             
         BXLE  R8,R2,*-18                                                       
         B     DEMA10                                                           
         TM    4(R8),DMODEMOV                                                   
         BO    DEMA12                                                           
*                                                                               
DEMA10   SR    R8,R8               DO THE ADJUSTMENT                            
         ICM   R8,7,5(R5)                                                       
         SR    R9,R9                                                            
         SRDA  R8,31                                                            
         M     R8,LDEMADJ                                                       
         D     R8,=F'1000'                                                      
         AH    R9,=H'1'                                                         
         SRA   R9,1                                                             
         ST    R9,4(R5)            STORE ADJUSTED DEMO IN ELEMENT               
*                                                                               
DEMA12   BXLE  R5,R2,DEMA6         NEXT DEMO                                    
*                                                                               
DEMAX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SINGLE DEMO UPGRADE ROUTINE                                         *         
* INPUT  : LDEMLST=SINGLE DEMO                                        *         
* OUTPUT : LDEMVAL=DEMO VALUE                                         *         
*          SVRTGSVC=ACTUAL RATING SERVICE                             *         
***********************************************************************         
         SPACE 1                                                                
         USING BWDRECD,R3                                                       
SDEMUP   NTR1                                                                   
*                                                                               
SDEMUP2  XC    LDEMVAL,LDEMVAL                                                  
         OC    SVUPGRD,SVUPGRD                                                  
         BZ    SDEMUPX                                                          
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
         MVC   SPUPDAY,SVDAYS                                                   
         MVC   SPUPTIM,SVTIMES                                                  
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
         GOTO1 VSPDEMUP,APPARM,LDMUPBLK,LDEMLST,LDEMVAL                         
         MVC   SVRTGSVC,SPUPACTS   SAVE ACTUAL RATING SERVICE                   
*                                                                               
SDEMUPX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DEMO UPGRADE ROUTINE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING BWDRECD,R3                                                       
DEMUP    XC    LDEMOVR,LDEMOVR                                                  
         OC    SVUPGRD,SVUPGRD     TEST UPGRADE EXISTS                          
         BZ    DEMUX                                                            
         NI    LFLAG,255-LDEMFRZ   BUILD DEMO OVERRIDE ELEMENT                  
         ICM   R8,15,LADEMEL                                                    
         BZ    DEMU14                                                           
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
         BNE   DEMU12                                                           
         TM    LFLAG,LDEMFRZ          TEST FREEZE THE DEMO VALUE                
         BO    *+12                                                             
         TM    DMODEMO+4,DMODEMOV     OR MANUAL OVERRIDE                        
         BZ    DEMU14                                                           
         MVI   LDEMOVR,OVERELEM       YES - BUILD OVERRIDE ELEM                 
         MVI   LDEMOVR+1,6                                                      
         MVC   LDEMOVR+2(2),DMODEMO+1                                           
         MVC   LDEMOVR+4(2),DMODEMO+6                                           
         TM    DMODEMO+4,DMODEMOV     TEST THIS IS MANUAL OVERRIDE              
         BZ    DEMU14                                                           
         NI    LFLAG,255-LDEMFRZ      YES-NOT RESULT OF FREEZE                  
         OI    LFLAG,LRTGOVR          INDICATE MANUAL RATING OVERRIDE           
         B     DEMU14                                                           
*                                                                               
DEMU12   BXLE  R1,RE,DEMU10                                                     
         DROP  R1                                                               
*                                                                               
DEMU14   LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         LA    RE,LIUN                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
         MVC   SPUPSTA,BWDSTA                                                   
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
         MVC   SPUPBTYP,STABKTYP                                                
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
         LA    R2,6                R2 = NUMBER OF LOOKUPS                       
         LA    R8,SVDTLST          R8 = A(DAYS/TIMES LIST)                      
         LA    R9,SVPROJ           R9 = A(DEMO VALUE AREA)                      
         XC    SVPROJ,SVPROJ       CLEAR DEMO VALUE AREA                        
         XC    SVLIPROG,SVLIPROG   CLEAR PROGRAM NAMES                          
         XC    SVLOPROG,SVLOPROG                                                
*                                                                               
DEMU16   MVC   SPUPDAY,0(R8)       SET DAYS/TIMES                               
         MVC   SPUPTIM,1(R8)                                                    
*                                                                               
         GOTO1 VSPDEMUP,APPARM,LDMUPBLK,SVDEM,(R9)                              
*                                                                               
         MVC   SVRTGSVC,SPUPACTS   SAVE ACTUAL RATING SERVICE                   
         XC    SPUPAOVR,SPUPAOVR   CLEAR DEMO OVERRIDE                          
         MVI   SVDEM,0             REMOVE OVERRIDE INDICATOR                    
         TM    LFLAG,LRTGOVR       TEST MANUAL OVERRIDE                         
         BZ    *+12                                                             
         OI    0(R9),X'80'         YES                                          
         NI    LFLAG,255-LRTGOVR                                                
         CH    R2,=H'5'            SAVE LEAD-IN AND LEAD-OUT                    
         BNE   *+10                PROGRAMMING                                  
         MVC   SVLIPROG,SPUPPRG                                                 
         CH    R2,=H'4'                                                         
         BNE   *+10                                                             
         MVC   SVLOPROG,SPUPPRG                                                 
         LA    R8,5(R8)            NEXT DAYS/TIMES                              
         LA    R9,4(R9)                                                         
         BCT   R2,DEMU16                                                        
*                                                                               
DEMUX    B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOOKUP THE DEMO VALUES FOR 4 BOOKS                       *         
***********************************************************************         
         SPACE 1                                                                
GETDEMS  XC    SPDEMLK,SPDEMLK                                                  
         LA    RE,LIUN                                                          
         ST    RE,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOM                                                    
         MVC   SPLKAGY,CUAALF                                                   
         MVC   SPLKMED,CUDMED                                                   
         MVC   SPLKCLI,QCLT                                                     
         MVC   SPLKSTA,BWDSTA                                                   
         MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKSRC,CLTSRC                                                   
         MVC   SPLKBTYP,STABKTYP                                                
         CLI   SVRTGSVC,0          TEST SOURCE RETURNED FROM SPDEMUP            
         BE    *+14                                                             
         MVC   SPLKSRC,SVRTGSVC    YES-USE IT AND                               
         B     *+10                    SUPPRESS MARKET OVERRIDE                 
         MVC   SPLKUMK,MKTLKUP                                                  
         MVI   SPLKSVI,X'FF'                                                    
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
         LA    R1,APFULL                                                        
         ST    R1,SPLKAVAL                                                      
         LA    R2,6                R2 = NUMBER DAYS/TIMES                       
         LA    R4,SVDTLST          R4 = A(DAYS/TIMES LIST)                      
         LA    R9,SVDEMVAL         R9 = A(DEMO VALUE AREA)                      
         XC    SVDEMVAL,SVDEMVAL   CLEAR DEMO VALUE AREA                        
         SR    R3,R3                                                            
*                                                                               
GETD2    MVC   SPLKDAY,0(R4)       SET DAYS/TIMES                               
         MVC   SPLKTIM,1(R4)                                                    
         LA    R0,4                                                             
         LA    R8,SVBKS            R8 = A(BOOK LIST)                            
*                                                                               
GETD4    OC    0(2,R8),0(R8)                                                    
         BZ    GETD6                                                            
         MVC   SPLKDBK,0(R8)                                                    
         XC    APFULL,APFULL                                                    
         GOTO1 VSPDEMLK,APPARM,(X'FF',SPDEMLK)                                  
         MVC   0(4,R9),APFULL      DEMO VALUE                                   
         LTR   R3,R3               TEST NEED TO SAVE PROGRAM                    
         BZ    GETD6                                                            
         MVC   0(16,R3),SPLKPRG    YES                                          
         LA    R3,16(R3)                                                        
*                                                                               
GETD6    LA    R8,2(R8)            NEXT BOOK                                    
         LA    R9,4(R9)                                                         
         BCT   R0,GETD4                                                         
*                                                                               
         LA    R4,5(R4)            NEXT DAYS/TIMES                              
         LA    R3,SVLIPRGS         SAVE LEAD-IN AND LEAD-OUT PROGRAMS           
         CH    R2,=H'6'                                                         
         BE    GETD8                                                            
         LA    R3,SVLOPRGS                                                      
         CH    R2,=H'5'                                                         
         BE    GETD8                                                            
         SR    R3,R3                                                            
*                                                                               
GETD8    BCT   R2,GETD2                                                         
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
OVERELEM EQU   X'DE'                                                            
         EJECT                                                                  
LOCALD   DSECT                                                                  
LASAVE   DS    A                                                                
LACOST   DS    A                                                                
LAUPGEL  DS    A                                                                
LAODTEL  DS    A                                                                
LADEMEL  DS    A                                                                
LDEMADJ  DS    F                                                                
LDEMOLD  DS    F                                                                
LDNAME   DS    CL7                                                              
LDMUPBLK DS    (SPDEMUPL)X                                                      
LTGTDEM  DS    XL4                                                              
LDEMOVR  DS    XL7                                                              
LSVDPTLN DS    XL2                                                              
LDEMLST  DS    XL4                                                              
LDEMVAL  DS    XL4                                                              
LDPT     DS    CL1                                                              
L1WPROF  DS    XL16                                                             
*                                                                               
LFLAG    DS    X                                                                
LADJIMP  EQU   X'80'                                                            
LADJALL  EQU   X'40'                                                            
LDEMFRZ  EQU   X'20'                                                            
LRTGOVR  EQU   X'10'                                                            
*                                                                               
LFLAG2   DS    X                                                                
LDAYOVR  EQU   X'80'                                                            
LNEWDAYS EQU   X'40'                                                            
LTIMOVR  EQU   X'20'                                                            
LNEWTIME EQU   X'10'                                                            
*                                                                               
LCHG     DS    X                                                                
LCOST    EQU   X'80'                                                            
LUPG     EQU   X'40'                                                            
LBK      EQU   X'20'                                                            
LPRG     EQU   X'10'                                                            
LRTG     EQU   X'08'                                                            
LLEAD    EQU   X'04'                                                            
LDEMO    EQU   X'02'                                                            
         EJECT                                                                  
       ++INCLUDE SPDEMLK                                                        
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
LIUN     DS    2000X                                                            
*                                                                               
         ORG   LOCALD+4096                                                      
*                                                                               
LOCALX   EQU   *                                                                
         EJECT                                                                  
LINED    DSECT                                                                  
         DS    XL2                                                              
LRTG1    DS    CL6                                                              
         DS    XL7                                                              
LRTG2    DS    CL6                                                              
         DS    XL7                                                              
LRTG3    DS    CL6                                                              
         DS    XL7                                                              
LRTG4    DS    CL6                                                              
         DS    XL2                                                              
LPROJECT DS    CL10                                                             
         DS    XL1                                                              
LCPP     DS    CL7                                                              
         SPACE 2                                                                
LINE2D   DSECT                                                                  
LPRGBK1  DS    CL10                                                             
         DS    CL3                                                              
LPRGBK2  DS    CL10                                                             
         DS    CL3                                                              
LPRGBK3  DS    CL10                                                             
         DS    CL3                                                              
LPRGBK4  DS    CL10                                                             
         DS    CL2                                                              
LPRGPROJ DS    CL10                                                             
         EJECT                                                                  
SAVED    DSECT                     OVERLAY SAVE AREA DSECT                      
SVBKS    DS    XL8                                                              
SVFLAG   DS    XL1                                                              
SVFIRST  EQU   X'80'                                                            
SVDEM    DS    XL4                                                              
SVDAYS   DS    XL1                                                              
SVTIMES  DS    XL4                                                              
SVLIDAYS DS    XL1                                                              
SVLITIME DS    XL4                                                              
SVLIPROG DS    CL16                                                             
SVLODAYS DS    XL1                                                              
SVLOTIME DS    XL4                                                              
SVLOPROG DS    CL16                                                             
SVDFLITM DS    XL4                                                              
SVDFLOTM DS    XL4                                                              
SVDTLST  DS    XL(6*5)                                                          
SVUPFILE DS    C                                                                
SVUPGRD  DS    XL8                                                              
SVUPFRBK DS    XL2                                                              
SVUPFBL  DS    XL6                                                              
SVUPINP  DS    CL32                                                             
SVUPDAY  DS    X                                                                
SVUPTIM  DS    XL4                                                              
SVUPPUT  DS    CL1                                                              
SVUPSHR  DS    CL1                                                              
SVRTGSVC DS    CL1                                                              
SVDEMVAL DS    XL(4*4*6)                                                        
SVPROJ   DS    6XL4                                                             
SVLIPRGS DS    CL(4*16)                                                         
SVLOPRGS DS    CL(4*16)                                                         
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
       ++INCLUDE SPNWSE8D                                                       
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
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPNWS29   05/01/02'                                      
         END                                                                    
