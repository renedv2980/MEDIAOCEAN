*          DATA SET SPNWS22    AT LEVEL 013 AS OF 03/20/07                      
*PHASE T20722A,*                                                                
         TITLE 'BWS22 - BWS - AUDIENCE COMPOSITION SCREEN'                      
T20722   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20722**,RA,RR=RE                                              
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
         AHI   R6,SAVAREAX-SAVAREA                                              
         AHI   R6,2048                                                          
******** L     R6,ATIA             USE TIA FOR SAVED VALUES                     
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
         GOTO1 AVALMED,AUDMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         GOTO1 AVALBYR,AUDBYRH     VALIDATE BUYER                               
         BNE   VALKX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW         TEST BUYER PASSWORD                          
         BZ    VALK1                                                            
         GOTO1 AVALPWD             YES-VALIDATE IT                              
         BNE   VALKX                                                            
*                                                                               
VALK1    GOTO1 AVALCAM,AUDNUMH     VALIDATE CAMPAIGN NUMBER                     
         BNE   VALKX                                                            
         MVC   BWHKCAM,BCAM                                                     
         GOTO1 AGETCLT,CMPCLTC     (GET CLIENT)                                 
         BE    *+16                                                             
         LA    R1,AUDNUMH                                                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
         GOTO1 AGETPRD,CMPPRDN     (GET PRODUCT)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,ESTDEMS          SAVE ONLY RATINGS AND IMPRESSIONS            
         LA    RE,LESTDEMS                                                      
         XC    LESTDEMS,LESTDEMS                                                
*                                                                               
VALK2    OC    0(3,R1),0(R1)                                                    
         BZ    VALK10                                                           
         CLI   1(R1),C'R'                                                       
         BE    VALK4                                                            
         CLI   1(R1),C'E'                                                       
         BE    VALK4                                                            
         CLI   1(R1),C'I'                                                       
         BNE   *+14                                                             
VALK4    MVC   0(3,RE),0(R1)                                                    
         LA    RE,3(RE)                                                         
         LA    R1,3(R1)                                                         
         B     VALK2                                                            
*                                                                               
******                                                                          
VALK10   GOTO1 AVALSTA,AUDSTAH     VALIDATE STATION                             
         BNE   VALKX                                                            
         MVC   BWHKMKT,BMKT        STATION'S MARKET                             
*                                                                               
         GOTO1 AVALDAY,AUDDAYH     VALIDATE DAYS                                
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALTIM,AUDTIMH     VALIDATE TIMES                               
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALDPL,AUDDLNH     VALIDATE DAYPART/LENGTH                      
         BNE   VALKX                                                            
         CLI   CMPDPOPT,C'M'       TEST SUBDPT SCHEDULED UNDER MASTER           
         BNE   *+12                                                             
         CLI   DPTTYPE,C'S'        YES-TEST THIS IS A SUBDAYPART                
         BE    ESUB                    YES-ERROR                                
         CLI   BSLN,0              MAKE SURE SPOT LENGTH IS SPECIFIED           
         BE    ENOSLN                                                           
*                                                                               
         MVI   LFLAG,0                                                          
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
         NI    LFLAG1,X'FF'-LF1BUYRV                                            
*                                                                               
VALK26   LA    R4,BWHFSTEL                                                      
VALK26O  CLI   0(R4),0                                                          
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
         B     VALK26O                                                          
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
         CHI   RF,255                                                           
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
VALK32   NI    TWAFLAG,FF-TWAFRST                                               
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
         XC    LASHPEL,LASHPEL                                                  
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
         CLI   0(R8),SHPELCDQ                                                   
         BNE   *+8                                                              
         ST    R8,LASHPEL          A(SHARE/PUT OVERRIDE ELEMENT)                
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     VALK52                                                           
*                                                                               
VALK54   DS    0H                                                               
*                                                                               
         B     VALKX                                                            
*                                                                               
VALK99   MVC   FVMSGNO,=AL2(FVFERNF)    RECORD NOT FOUND                        
         LA    R1,AUDMEDH                                                       
         ST    R1,FVADDR                                                        
*                                                                               
VALKX    DS    0H                                                               
         XC    IOKEY,IOKEY         READ BWA PROFILE                             
         MVC   IOKEY(4),=C'sBWA'  <== Need the lowercase s                      
         MVC   IOKEY+4(2),CUAALF                                                
         MVC   IOKEY+6(1),QMED                                                  
         MVC   IOKEY+7(3),QCLT                                                  
         GOTO1 VGETPROF,APPARM,IOKEY,APWORK,VDMGR                               
         CLI   APWORK+7,C'Y'       DO WE NEED TO LOOK UP BDEMOS?                
         BNE   *+8                  - NOPE                                      
         OI    LFLAG1,LF1USEI       - YUP                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR REPORT                                               *         
***********************************************************************         
         SPACE 1                                                                
FSTRPT   L     R3,AIOAREA2                                                      
         OI    SVFLAG,SVF1TIME     FIRST TIME THROUGH                           
         SR    R0,R0                                                            
         LA    R4,AUDUPGH          TURN PREV VALIDATED BITS ON                  
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
         MVI   SVUPFRBT,0                                                       
         XC    SVUPFBL,SVUPFBL                                                  
         XC    SVUPINP,SVUPINP                                                  
         XC    SVUPDAY,SVUPDAY                                                  
         XC    SVUPTIM,SVUPTIM                                                  
         XC    SVUPPUT,SVUPPUT                                                  
         XC    SVUPSHR,SVUPSHR                                                  
         MVI   SVRTGSVC,0                                                       
*                                                                               
         OC    CMPUP,CMPUP         TEST FOR CAMPAIGN UPGRADE                    
         BZ    FSTR4                                                            
         MVC   SVUPFILE,CMPUF                                                   
         MVC   SVUPGRD,CMPUP                                                    
         MVC   SVUPFRBK,CMPFB                                                   
         TM    CMPFB+1,BTY2CHAR                                                 
         BNO   FSTR3G                                                           
         CLI   CMPFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SVUPFRBT,CMPFBTP    SVUPFRBK BOOKTYPE                            
FSTR3G   MVC   SVUPFBL,CMPFBLST                                                 
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
         TM    UPGFRBK+1,BTY2CHAR                                               
         BNO   FSTR4G                                                           
         CLI   UPGELLN,UPGELLNQ                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   UPGFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SVUPFRBT,UPGFRBKT                                                
FSTR4G   XC    SVUPFBL,SVUPFBL                                                  
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
FSTR8    BAS   RE,VALUPGRD         INSPECT UPGRADE FIELD                        
         BL    FSTRX                                                            
         BE    FSTR10                                                           
         MVC   AUDUPG,SVUPINP      NONE - EDIT UPGRADE EXPRESSION               
         OI    AUDUPGH+6,FVOXMT                                                 
*                                                                               
FSTR10   OC    MKTLKUP,MKTLKUP     TEST POSSIBILITY OF MARKET OVERRIDE          
         BZ    FSTRX                                                            
         MVC   LDEMLST,=X'00D901FF' YES-CALL SPDEMUP TO GET RTG SERVICE         
*                                                                               
         MVI   QBOOKTYP,0                                                       
         TM    SVUPFRBK+1,BTYBITSQ SPECIAL BOOK?                                
         BZ    FSTR11                                                           
         TM    SVUPFRBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                       
         BNO   FSTR10T              - NOPE                                      
         CLI   SVUPFRBT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   QBOOKTYP,SVUPFRBT    - YUP                                       
         B     FSTR11                                                           
*                                                                               
FSTR10T  GOTO1 AGETBKTY,APPARM,(C'B',SVUPFRBK+1),QBOOKTYP                       
*                                                                               
FSTR11   GOTO1 ASDEMUP                                                          
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
         TM    TWAFLAG,TWAFCOM     YES-TEST SWAPPED FROM COMP SCREEN            
         BO    *+8                                                              
         OI    TWAFLAG,TWAFRST         NO-FLAG FIRST TIME                       
         LA    R8,APRECKEY                                                      
         USING NSIDKEYD,R8                                                      
         GOTO1 AGETMED,NSAGYMD                                                  
         BNE   *+10                                                             
         MVC   AUDMED,QMED         MEDIA                                        
         MVI   LDPT,0                                                           
         SR    R0,R0                                                            
         L     R6,LASAVE                                                        
         USING SAVAREA,R6                                                       
         LA    R4,SAVKEYS          EXTRACT BUYER CAMPAIGN AND DAYPART           
*                                  FROM SAVED KEY                               
DISK2    CLI   0(R4),0                                                          
         BE    DISK8                                                            
         LA    RF,AUDBYR                                                        
         CLI   0(R4),KEYBYR        BUYER                                        
         BE    DISK4                                                            
         LA    RF,AUDNUM                                                        
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
         MVC   AUDSTA(8),APWORK+4  STATION                                      
         CLI   AUDSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   AUDSTA+4,C' '                                                    
         CLI   APWORK+4,C'0'       TEST CABLE                                   
         BL    *+8                                                              
         MVI   AUDSTA+4,C'/'                                                    
         GOTO1 AGETDAY,NSDAY       DAYS                                         
         MVC   AUDDAY,QDAYS                                                     
         GOTO1 AGETTIM,NSTIME      TIMES                                        
         MVC   AUDTIM,QTIMES                                                    
         XC    AUDSUB,AUDSUB                                                    
         MVI   AUDSDP,0                                                         
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
         MVC   AUDSDP,NSDPT                                                     
         MVC   AUDSUB(10),=C'Subdaypart'                                        
         MVC   AUDDLN(1),LDPT                                                   
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
         MVC   AUDMED,QMED         MEDIA                                        
         LA    R1,BWDKBYR          BUYER                                        
         ICM   R1,8,=X'B'                                                       
         GOTO1 AGETBYR                                                          
         MVC   AUDBYR,QBYR                                                      
         GOTO1 AGETCM,BWDKSEQ      CAMPAIGN                                     
         MVC   AUDNUM,QCAM                                                      
         MVC   AUDSTA(L'BWDSTA),BWDSTA   STATION                                
         CLI   AUDSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   AUDSTA+4,C' '                                                    
         CLI   BWDSTA,C'0'         TEST CABLE                                   
         BL    *+8                                                              
         MVI   AUDSTA+4,C'/'                                                    
         GOTO1 AGETDAY,BWDDAYS     DAYS                                         
         MVC   AUDDAY,QDAYS                                                     
         GOTO1 AGETTIM,BWDTIMES    TIMES                                        
         MVC   AUDTIM,QTIMES                                                    
         MVC   AUDDLN(1),BWDDPT    DAYPART                                      
         XC    AUDSUB,AUDSUB                                                    
         MVI   AUDSDP,0                                                         
         CLI   BWDSUBDP,0          TEST SUBDAYPART                              
         BE    *+16                                                             
         MVC   AUDSDP,BWDSUBDP                                                  
         MVC   AUDSUB(10),=C'Subdaypart'                                        
         ZIC   RF,BWDSLN                                                        
         BAS   RE,DISLEN                                                        
         B     DISKX                                                            
*                                                                               
DISKX    MVC   BDPT(2),LSVDPTLN    RESTORE DAYPART/LENGTH                       
         B     EXIT                                                             
         SPACE  2                                                               
DISLEN   CVD   RF,APDUB            DISPLAY SPOT LENGTH                          
         OI    APDUB+7,X'0F'                                                    
         LA    R1,AUDDLN+1                                                      
         CLI   AUDDLN,C'1'                                                      
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
*                                                                               
*****C   CLI   APROF7,C'C'         TEST CANADIAN AGENCY                         
*****    BE    DISNOCAN             - YUP, GET OUTTA HERE                       
DISREC   TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BO    DISNOCAN             - YUP, GET OUTTA HERE                       
*                                                                               
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    DISR10                                                           
         CLI   APPFKEY,PFK04       AND PF4, PF5, PF6 OR PF12                    
         BL    DISR10                                                           
         CLI   APPFKEY,PFK06                                                    
         BNH   *+12                                                             
         CLI   APPFKEY,PFK12                                                    
         BNE   DISR10                                                           
         MVI   APMODE,APMLRP       YES-TELL ROOT ITS THE LAST SCREEN            
         MVC   SCPFKEY,APPFKEY         AND PASS ON PF KEY VALUE                 
         MVI   TWAFLAG,0                                                        
         B     DISRX                                                            
*                                                                               
***  LET'S CLEAR SOME STUFF EH?                                                 
DISR10   XC    SVDMA(NDEMOS*4),SVDMA                                            
         XC    LAUDDEMS(LAUDDNFO-LAUDDEMS),LAUDDEMS                             
*                                                                               
         TM    AUDDAYH+4,X'80'     FIELD INPUT THIS TIME?                       
         BZ    *+8                                                              
         OI    LCHG,LDAYS           - DAYS WERE CHANGED                         
*                                                                               
         TM    AUDTIMH+4,X'80'     FIELD INPUT THIS TIME?                       
         BZ    *+8                                                              
         OI    LCHG,LTIME           - TIMES WERE CHANGED                        
*                                                                               
         TM    AUDMDMH+FVIIND-FVIHDR,FVIVAL   TEST ANY DEMO CHANGES             
         BZ    DISR20                                                           
         TM    AUDDM1H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR20                                                           
         TM    AUDDM2H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR20                                                           
         TM    AUDDM3H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR20                                                           
         TM    AUDDM4H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR20                                                           
         B     *+8                                                              
*                                                                               
DISR20   OI    LCHG,LDEMS          DEMO(S) WERE CHANGED                         
*                                                                               
         XC    DBLOCK,DBLOCK       GET PRIMARY DEMO NAME                        
         MVC   DBBTYPE,QBOOKTYP    BOOKTYPE FROM THE MASTER RECORD              
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         MVC   DBSELSRC,CLTSRC                                                  
         CLI   SVRTGSVC,0                                                       
         BE    *+10                                                             
         MVC   DBSELSRC,SVRTGSVC                                                
         MVC   DBSELUMK,BMKT                                                    
         MVC   DBSELSTA,QSTA                                                    
         MVC   DBSELAGY,CUAALF                                                  
*                                                                               
         LA    R0,ESTUSRNM                                                      
*                                                                               
         LA    R2,AUDMDMH                                                       
         GOTO1 VDEMOVAL,APPARM,(1,AUDMDMH),(1,LAUDDEMS),(C'S',DBLOCK), +        
               (R0)                                                             
         CLI   4(R1),0             WAS THERE AN ERROR?                          
         BE    DISR99               - YUP, GET OUTTA HERE                       
*                                                                               
         OC    AUDMDM,SPACES                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    FVIIND-FVIHDR(R2),FVIVAL   VALIDATED                             
         LA    R1,LAUDDEMS         LAUDDEMS ONLY HAVE THE IMPRESSION            
         USING DEMTAB,R1           ...WE NEED RTG, SHR, AND DMA!!               
         MVC   SHRVAL,RTGVAL                                                    
         MVC   DMAVAL,SHRVAL                                                    
         MVI   RTGVAL+1,C'R'                                                    
         MVI   SHRVAL+1,C'S'                                                    
         MVI   DMAVAL+1,C'D'                                                    
         TM    LFLAG1,LF1USEI      DO WE NEED BDEMOS?                           
         BZ    *+8                  - NOPE                                      
         MVI   DMAVAL+1,C'I'        - YUP                                       
         DROP  R1                                                               
***                                                                             
         GOTO1 VDEMOVAL,APPARM,(1,AUDMDMH),(1,LAUDDNFO),(C'A',DBLOCK), +        
               (R0)                                                             
***  THAT SAVES OFF THE SEX, START AGE, AND END AGE OF DENOMINATOR              
*********************                                                           
         LA    R2,AUDDM1H                                                       
         OI    FVIIND-FVIHDR(R2),FVIVAL   VALIDATED                             
         OC    AUDDM1,AUDDM1       ANY DEMO?                                    
         BZ    DISR22               - NOPE                                      
*                                                                               
         GOTO1 VDEMOVAL,APPARM,(1,AUDDM1H),(1,LAUDDEMS+L'LAUDDEMS*3),  +        
               (C'S',DBLOCK),(R0)                                               
         CLI   4(R1),0             WAS THERE AN ERROR?                          
         BE    DISR99               - YUP, GET OUTTA HERE                       
*                                                                               
         OC    AUDDM1,SPACES                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         MVI   LAUDDEMS+L'LAUDDEMS*3+1,C'D'   ONLY NEED DMA FOR THESE           
         TM    LFLAG1,LF1USEI      DO WE NEED BDEMOS?                           
         BZ    *+8                  - NOPE                                      
         MVI   LAUDDEMS+L'LAUDDEMS*3+1,C'I'   WE NEED BDEMOS                    
***                                                                             
         GOTO1 VDEMOVAL,APPARM,(1,AUDDM1H),(1,LAUDNNFO),(C'A',DBLOCK), +        
               (R0)                                                             
         BAS   RE,INFOCHK                                                       
         BNE   ENOTSUB                                                          
***  NUMERATOR INFO WILL BE OVERWRITTEN WITH THE NEXT DEMO                      
*****                                                                           
DISR22   LA    R2,AUDDM2H                                                       
         OI    FVIIND-FVIHDR(R2),FVIVAL   VALIDATED                             
         OC    AUDDM2,AUDDM2       ANY DEMO?                                    
         BZ    DISR25               - NOPE                                      
*                                                                               
         GOTO1 VDEMOVAL,APPARM,(1,AUDDM2H),(1,LAUDDEMS+L'LAUDDEMS*4),  +        
               (C'S',DBLOCK),(R0)                                               
         CLI   4(R1),0             WAS THERE AN ERROR?                          
         BE    DISR99               - YUP, GET OUTTA HERE                       
*                                                                               
         OC    AUDDM2,SPACES                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         MVI   LAUDDEMS+L'LAUDDEMS*4+1,C'D'                                     
         TM    LFLAG1,LF1USEI      DO WE NEED BDEMOS?                           
         BZ    *+8                  - NOPE                                      
         MVI   LAUDDEMS+L'LAUDDEMS*4+1,C'I'   - YES WE DO                       
***                                                                             
         GOTO1 VDEMOVAL,APPARM,(1,AUDDM2H),(1,LAUDNNFO),(C'A',DBLOCK), +        
               (R0)                                                             
         BAS   RE,INFOCHK                                                       
         BNE   ENOTSUB                                                          
***  NUMERATOR INFO WILL BE OVERWRITTEN WITH THE NEXT DEMO                      
*****                                                                           
DISR25   LA    R2,AUDDM3H                                                       
         OI    FVIIND-FVIHDR(R2),FVIVAL   VALIDATED                             
         OC    AUDDM3,AUDDM3       ANY DEMO?                                    
         BZ    DISR27               - NOPE                                      
*                                                                               
         GOTO1 VDEMOVAL,APPARM,(1,AUDDM3H),(1,LAUDDEMS+L'LAUDDEMS*5),  +        
               (C'S',DBLOCK),(R0)                                               
         CLI   4(R1),0             WAS THERE AN ERROR?                          
         BE    DISR99               - YUP, GET OUTTA HERE                       
*                                                                               
         OC    AUDDM3,SPACES                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         MVI   LAUDDEMS+L'LAUDDEMS*5+1,C'D'                                     
         TM    LFLAG1,LF1USEI      DO WE NEED BDEMOS?                           
         BZ    *+8                  - NOPE                                      
         MVI   LAUDDEMS+L'LAUDDEMS*5+1,C'I'   - WE NEED BDEMOS                  
***                                                                             
         GOTO1 VDEMOVAL,APPARM,(1,AUDDM3H),(1,LAUDNNFO),(C'A',DBLOCK), +        
               (R0)                                                             
         BAS   RE,INFOCHK                                                       
         BNE   ENOTSUB                                                          
***  NUMERATOR INFO WILL BE OVERWRITTEN WITH THE NEXT DEMO                      
*****                                                                           
DISR27   LA    R2,AUDDM4H                                                       
         OI    FVIIND-FVIHDR(R2),FVIVAL   VALIDATED                             
         OC    AUDDM4,AUDDM4       ANY DEMO?                                    
         BZ    DISR29               - NOPE                                      
*                                                                               
         GOTO1 VDEMOVAL,APPARM,(1,AUDDM4H),(1,LAUDDEMS+L'LAUDDEMS*6),  +        
               (C'S',DBLOCK),(R0)                                               
         CLI   4(R1),0             WAS THERE AN ERROR?                          
         BE    DISR99               - YUP, GET OUTTA HERE                       
*                                                                               
         OC    AUDDM4,SPACES                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         MVI   LAUDDEMS+L'LAUDDEMS*6+1,C'D'                                     
         TM    LFLAG1,LF1USEI      DO WE NEED BDEMOS?                           
         BZ    *+8                  - NOPE                                      
         MVI   LAUDDEMS+L'LAUDDEMS*6+1,C'I'   - USE THE BDEMOS                  
***                                                                             
         GOTO1 VDEMOVAL,APPARM,(1,AUDDM4H),(1,LAUDNNFO),(C'A',DBLOCK), +        
               (R0)                                                             
         BAS   RE,INFOCHK                                                       
         BNE   ENOTSUB                                                          
*****                                                                           
DISR29   TM    AUDUPGH+FVIIND-FVIHDR,FVIVAL   VALIDATED?                        
         BNZ   DISR30               - YUP, NO NEED TO DO IT AGAIN               
         LA    R2,AUDUPGH                                                       
         BAS   RE,VALUPGRD         INSPECT UPGRADE FLD (TURNS ON LDEMS)         
         BNE   DISR98               - ERROR, INVALID UPGRADE EXPRESSION         
         OI    AUDUPGH+FVIIND-FVIHDR,FVIVAL   VALIDATED                         
*****                                                                           
*                                                                               
*ISR30   BAS   RE,DISSRC           DISPLAY RATING SERVICE                       
DISR30   TM    SVFLAG,SVF1TIME     TEST FOR VERY FIRST TIME THROUGH             
         BO    DISR31               - YUP, IT IS                                
*                                  NO - TEST FOR CHANGES THAT WILL              
         TM    LCHG,LUPG+LDEMS+LDAYS+LTIME   AFFECT NUMBERS ON SCREEN           
         BNZ   DISR31                    YES- RE-DISPLAY CURRENT SCREEN         
         CLC   SVTIME1,SVTIMEND    DID WE REACH THE END YET                     
         BNE   DISR32                                                           
DISR30A  MVI   APMODE,APMLRP             TELL CONTROLLER                        
         TM    TWAFLAG,TWAFCOM           TEST COME FROM COMP SCREEN             
         BZ    DISR32                                                           
         BAS   RE,RESCOM                 YES - RESTORE COMP SCREEN              
         B     DISRX                                                            
*                                                                               
DISR31   OI    SVFLAG,SVF1TIME     WE'RE AT PAGE 1 AGAIN!                       
         XC    SVTIME,SVTIME       ONLY TIME WE NEED TO CLEAR THESE             
         XC    SVTIMNOW,SVTIMNOW                                                
******  CLEAR THE SCREEN                                                        
DISR32   SR    RE,RE               CLEAR THE SCREEN                             
         LA    R4,AUDL12H                                                       
         LA    R8,AUDL24H                                                       
*                                                                               
DISR34   IC    RE,0(R4)                                                         
         SHI   RE,9                                                             
         BNM   *+6                                                              
         DC    H'0'                TWA MUST BE FUCKED                           
         EX    RE,DRCLC                                                         
         BE    DISR35                                                           
         EX    RE,DROC                                                          
         BZ    DISR35                                                           
         EX    RE,DRXC                                                          
         OI    6(R4),FVOXMT                                                     
*                                                                               
DISR35   LA    R4,9(RE,R4)                                                      
         CR    R4,R8                                                            
         BL    DISR34                                                           
******  CLEAR THE SCREEN                                                        
DISR36   NI    LCHG,X'FF'-LUPG-LDEMS-LDAYS-LTIME   RESET LCHG FLAGSTUFF         
*                                                                               
         TM    SVFLAG,SVF1TIME     IS IT THE FIRST TIME?                        
         BNO   DISR38               - NOPE                                      
*                                                                               
         NI    SVFLAG,X'FF'-SVF1TIME   RESET FIRST TIME FLAG                    
         LA    R2,AUDTIMH                                                       
         GOTO1 AVALTIM,AUDTIMH                                                  
         BNE   DISRX                                                            
         MVC   SVTIME,BTIMES                                                    
*                                                                               
         MVC   SVTIME1,SVTIME      BEGINNING                                    
*        MVC   SVTIMEND,SVTIME+2   END ALREADY MOVED FROM BTIMES                
*                                                                               
DISR38   LA    R4,LDEMVAL                                                       
         LA    R2,AUDL12H                                                       
         USING CURLINE,R2                                                       
*                                                                               
*ISR40   XC    8(L'AUDL12,R2),8(R2)   CLEAR THE DISPLAY LINE                    
*                                                                               
DISR40   CLC   SVTIME1,SVTIMEND    WE DONE?                                     
         BE    DISRX                - YUP                                       
         CLC   AUDL24H,0(R2)       END OF SCREEN?                               
         BE    DISRX                - YUP, WE'RE DONE                           
*                                                                               
         OC    SVTIMEND,SVTIMEND   DO WE HAVE A TIME RANGE?                     
         BNZ   DISR42               - YUP, USUAL CASE                           
         OC    SVTIMNOW,SVTIMNOW    - NOPE, WE HAVE 9P, OR 515A, ETC            
         BZ    DISR50                - NOT FIRST TIME GOING THROUGH             
         B     DISR43                                                           
*                                                                               
DISR42   CLC   SVTIME2,SVTIMEND    DID WE REACH THE END?                        
         BE    DISR50               - YUP, GET OUT                              
*                                                                               
DISR43   BAS   RE,TIMES            BREAK UP TIME INTO INTERVALS                 
         GOTO1 AGETTIM,SVTIMNOW                                                 
         MVC   CURTIME,QTIMES                                                   
*                                                                               
         LA    R3,LAUDDEMS                                                      
         USING DEMTAB,R3                                                        
*                                                                               
         GOTO1 DEMVALS,APPARM,RTGVAL,CURRTG                                     
         GOTO1 DEMVALS,APPARM,SHRVAL,CURSHR                                     
         GOTO1 DEMVALS,APPARM,DMAVAL,SVDMA                                      
         GOTO1 DEMVALS,APPARM,DMAVAL+4,SVDMA+4                                  
         GOTO1 DEMVALS,APPARM,DMAVAL+8,SVDMA+8                                  
         GOTO1 DEMVALS,APPARM,DMAVAL+12,SVDMA+12                                
         GOTO1 DEMVALS,APPARM,DMAVAL+16,SVDMA+16                                
*                                                                               
         GOTO1 DMAVALS,APPARM,SVDMA+4,AUDDM1,CURDM1                             
         GOTO1 DMAVALS,APPARM,SVDMA+8,AUDDM2,CURDM2                             
         GOTO1 DMAVALS,APPARM,SVDMA+12,AUDDM3,CURDM3                            
         GOTO1 DMAVALS,APPARM,SVDMA+16,AUDDM4,CURDM4                            
*                                                                               
         DROP  R3                                                               
         USING BWDRECD,R3                                                       
*                                                                               
DISR45   MVC   CURPROG,SVPROGNM                                                 
         OC    SVPROGNM,SVPROGNM   DID IT PULL OUT ANY PROGRAM NAME?            
         BNZ   DISR50               - YUP, NO PROBLEMS                          
         MVC   CURPROG(17),=C'NO PROGRAMS AVAIL'                                
*                                                                               
DISR50   OI    CURLINEH+6,X'80'    TRANSMIT PLZ                                 
         LA    R2,AUDL13H-AUDL12H(R2)   MOVE R2 TO NEXT LINE                    
         MVC   SVTIME1,SVTIME2                                                  
         B     DISR40                                                           
*******                                                                         
DISR98   MVC   FVMSGNO,=AL2(FVIUPG)   INVALID UPGRADE EXPRESSION                
         ST    R2,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
DISR99   MVC   FVMSGNO,=AL2(388)   INVALID DEMO                                 
         ST    R2,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
DISNOCAN TM    SVFLAG,SVF1TIME     IS IT FIRST TIME?                            
         BO    DISNO10              - YUP IT IS                                 
         OI    SVFLAG,SVF1TIME     TURN ON FIRST TIME FLAG AGAIN                
         MVI   APMODE,APMLRP       TELL CONTROLLER IT'S LAST PAGE               
         TM    TWAMODE,TWAMLSM     BWS RECORD-TEST LIST/SELECT                  
         BO    *+10                 - DON'T ERASE APRECKEY IF LIST              
         XC    APRECKEY,APRECKEY                                                
         B     DISRX                                                            
*                                                                               
DISNO10  MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   SAVRECK,APRECKEY    FAKE OUT GENERAL (KEY NOT CHANGED)           
         LA    R2,BWSACTH                                                       
         ST    R2,FVADDR                                                        
         XC    BWSMSG,BWSMSG                                                    
         MVC   BWSMSG(L'NOCANADA),NOCANADA                                      
         OI    BWSMSGH+4,X'20'     VALIDATED                                    
         OI    BWSMSGH+6,X'80'                                                  
         NI    SVFLAG,X'FF'-SVF1TIME                                            
         B     DISRX                                                            
*                                                                               
NOCANADA DC    C'Audience Composition is NOT available for Canada'              
*                                                                               
DISRX    TM    TWAMODE,TWAMLSM     TEST LIST/SELECT ACTIVE                      
         BZ    *+8                                                              
         MVI   TWALSACT,ACTAUD     YES-LAST SELECT ACTION = AUDCOMP             
         CLI   APMODE,APMSWP                                                    
         BE    *+12                                                             
         CLI   APMODE,APMRET                                                    
         BNE   EXIT                                                             
         MVI   APPARM,RECWRK       FOR SWAP, RECORD = WORK                      
         MVI   APPARM+1,ACTCOM               ACTION = COMPETITION               
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
DRCLC    CLC   8(0,R4),SPACES      EXECUTED INSTRUCTIONS                        
DROC     OC    8(0,R4),8(R4)                                                    
DRXC     XC    8(0,R4),8(R4)                                                    
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RATING SERVICE                                          *         
***********************************************************************         
*&&DO                                                                           
         SPACE 1                                                                
DISSRC   LR    R0,RE                                                            
         MVC   APBYTE,SVRTGSVC                                                  
         CLI   APBYTE,0                                                         
         BNE   *+10                                                             
         MVC   APBYTE,CLTSRC                                                    
         OI    AUDSRCH+6,FVOXMT                                                 
         MVC   AUDSRC,=C'NSI'                                                   
         CLI   APBYTE,C'N'                                                      
         BE    *+10                                                             
         MVC   AUDSRC,=C'ARB'                                                   
         CLI   CUDMED,C'C'                                                      
         BNE   DISSRCX                                                          
         MVC   AUDSRC,=C'CSI'                                                   
         CLI   APBYTE,C'N'                                                      
         BE    DISSRCX                                                          
         MVC   AUDSRC,=C'BBM'                                                   
*                                                                               
DISSRCX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* ROUTINE TO SEE IF DEMOS ARE A SUBSET OF THE MAIN DEMO               *         
* INPUT  : COMPARE LAUDDNFO AND LAUDNNFO                              *         
* OUTPUT : SEE IF DEMOS ARE A SUBSET (CONDITION = EQ)                 *         
***********************************************************************         
INFOCHK  NTR1                                                                   
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         IC    R0,LAUDDNFO         MAKING SURE LAUDNNFO IS A SUBSET...          
         IC    R1,LAUDNNFO         ...OF LAUDDNFO                               
         SR    R0,R1                                                            
         BM    INFONO              NO GOOD!                                     
         CLC   LAUDNNFO+1(1),LAUDDNFO+1                                         
         BL    INFONO                                                           
         CLC   LAUDDNFO+2(1),LAUDNNFO+2                                         
         BL    INFONO                                                           
*                                                                               
INFOYES  CR    RE,RE                                                            
         B     *+6                                                              
INFONO   CR    RB,RE                                                            
         B     EXIT                                                             
***********************************************************************         
* ROUTINE TO GET RTG, SHR, DMA VALUES WITH SDEMUP                     *         
* INPUT  : R4 = A(LDEMVAL)                                            *         
* OUTPUT : AVERAGE AUDIENCE % BASE FOR RESPECTIVE DEMO CATEGORY       *         
***********************************************************************         
DMAVALS  NTR1                                                                   
         L     R2,0(R1)            SVDMA* ADDRESS                               
         L     RE,4(R1)            SAVE OFF THE FIELD                           
         L     R3,8(R1)            ADDRESS OF FIELD TO DISPLAY                  
*                                                                               
         OC    0(L'AUDDM1,RE),0(RE)   DO WE HAVE ANYTHING IN THE FIELD?         
         BNZ   DMAV10               - YUP WE DO                                 
         XC    0(L'CURDM1,R3),0(R3)   CLEAR IT OUT                              
         B     DMAVALSX            AND WE'RE DONE!                              
*                                                                               
DMAV10   XR    R0,R0                                                            
         ICM   R1,15,0(R2)         WE'RE LOADING VALUE OF SVDMA*                
***  2 DECIMAL  ***                                                             
         TM    0(R2),DMODEM2D      IS IT 2 DECIMAL?                             
         BNO   DMAV13                                                           
         STCM  R1,8,APBYTE                                                      
         NI    APBYTE,FF-DMODEM2D   TURN OFF THE 2 DECIMAL X'40' BIT            
         ICM   R1,8,APBYTE                                                      
***  2 DECIMAL  ***                                                             
DMAV13   MHI   R1,2000             IT'S GOING TO BE A PERCENTAGE W/DEC          
*                                                                               
         ICM   R2,15,SVDMA         CAN REUSE R2 NOW                             
***  2 DECIMAL  ***                                                             
         TM    SVDMA,DMODEM2D      IS IT 2 DECIMAL?                             
         BNO   DMAV17                                                           
         STCM  R2,8,APBYTE                                                      
         NI    APBYTE,FF-DMODEM2D   TURN OFF THE 2 DECIMAL X'40' BIT            
         ICM   R2,8,APBYTE                                                      
***  2 DECIMAL  ***                                                             
DMAV17   LTR   R2,R2               IS THE DENOMINATOR 0?                        
         BNZ   DMAV20               - NOPE, NORMAL                              
         LTR   R1,R1               ANYTHING IN R1?                              
         BNZ   DMAV50               - DIVISION BY ZERO                          
         LA    R2,1(R2)            JUST PUT ANYTHING IN DENOMINATOR             
*                                                                               
DMAV20   DS    0H                  R1 ALREADY IS MULTIPLED BY 2 (2000)          
         DR    R0,R2               WE ALWAYS NEED SVDMA (DENOMINATOR)           
         AHI   R1,1                FOR ROUNDING                                 
         SRA   R1,1                 "        "                                  
         STCM  R1,15,LDEMVAL       BORROWING LDEMVAL SPACE TO PUT %             
*                                                                               
         OC    0(4,R4),0(R4)       ANYTHING IN R4 (LDEMVAL)?                    
         BZ    DMAV30                                                           
*                                                                               
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLOUT,5                                                         
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         ST    R3,EBAOUT                                                        
         ST    R4,EBAIN                                                         
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         B     DMAVALSX                                                         
*                                                                               
***  WEIRD SITUATIONS  -  DIVISION BY ZERO                                      
DMAV30   MVC   0(5,R3),=C'  0.0'   NUMERATOR IS 0, DISPLAY 0                    
         B     DMAVALSX                                                         
*                                                                               
DMAV50   MVC   0(7,R3),=C'ERR#DIV'   NUMERATOR HAS SOMETHING                    
*                                                                               
DMAVALSX B     XIT                                                              
***********************************************************************         
* ROUTINE TO GET RTG, SHR, DMA VALUES WITH SDEMUP                     *         
* INPUT  : R4 = A(LDEMVAL)                                            *         
* OUTPUT : LDEMVAL (R4) CONTAIN EITHER RTG, SHR, OR DMA               *         
***********************************************************************         
DEMVALS  NTR1                                                                   
         L     R2,0(R1)            LAUDDEMS ADDRESS                             
         L     R3,4(R1)            ADDRESS OF FIELD TO DISPLAY                  
*                                                                               
         MVC   LDEMLST(3),0(R2)                                                 
         MVI   LDEMLST+3,FF                                                     
*                                                                               
         GOTO1 ASDEMUP                                                          
*                                                                               
         CLI   1(R2),C'D'          IS IT DMA?                                   
         BE    DEMV50               - YUP, NO NEED FOR VEDITOR                  
         CLI   1(R2),C'I'          IS IT BDEMOS?                                
         BE    DEMV50               - YUP, NO NEED FOR VEDITOR                  
*                                                                               
*        OC    0(4,R4),0(R4)       ANYTHING IN R4 (LDEMVAL)?                    
*        BZ    DEMV30                                                           
*                                                                               
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLOUT,5                                                         
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         ST    R3,EBAOUT                                                        
         ST    R4,EBAIN                                                         
         MVI   EBDECS,1                                                         
***  2 DECIMAL  ***                                                             
         TM    0(R4),DMODEM2D      WE NEED 2 DECIMAL?                           
         BNO   *+16                                                             
         MVI   EBDECS,2                                                         
         MVI   EBLOUT,6            NEED ONE MORE SPACE FOR THE DECIMAL          
         NI    0(R4),FF-DMODEM2D   TAKE OFF THE BIT FOR CORRECT VALUE           
***  2 DECIMAL  ***                                                             
         MVI   EBSCIN,0                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
***  2 DECIMAL  ***                                                             
         CLI   EBDECS,2                                                         
         BNE   *+8                                                              
         OI    0(R4),DMODEM2D      TURN THE 2 DECIMAL X'40' BIT BACK ON         
***  2 DECIMAL  ***                                                             
         B     DEMVALSX                                                         
*                                                                               
*EMV30   MVC   0(5,R3),=C'  0.0'                                                
*        B     DEMVALSX                                                         
*                                                                               
DEMV50   MVC   0(4,R3),0(R4)       PUTS VALUE IN SVDMA*                         
*                                                                               
DEMVALSX B     XIT                                                              
***********************************************************************         
* RESTORE COMPETITION SCREEN                                          *         
***********************************************************************         
         SPACE 1                                                                
RESCOM   NTR1                                                                   
         NI    TWAFLAG,FF-TWAFCOM                                               
******** GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),SAVED  SAVE TIA               
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
         ICM   R1,15,AINP                                                       
         BZ    EXIT                                                             
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSCRN                                                
         MVI   TIOBCNT,X'F5'                                                    
         MVI   TIOBCNT+1,0                                                      
         DROP  R1                                                               
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
         CLI   AUDUPGH+5,0         TEST UPGRADE ENTERED                         
         BE    UPGRX1                                                           
         MVI   APFLAG,X'F8'                                                     
         GOTO1 AVALUPG,AUDUPGH     VALIDATE UPGRADE FIELD                       
         BNE   UPGRX2              ERROR                                        
*                                                                               
         USING UPGEL,R4                                                         
UPGR2    XC    APELEM,APELEM       NO - BUILD UPGRADE ELEM                      
         LA    R4,APELEM                                                        
         MVI   UPGELCD,UPGELCDQ                                                 
         MVI   UPGELLN,UPGELLNQ                                                 
*                                                                               
UPGR4    MVC   UPGFILE,APWORK                                                   
         MVC   UPGRADE,APWORK+1                                                 
         MVC   UPGFRBK,APWORK+9                                                 
         TM    APWORK+9+1,BTY2CHAR                                              
         BNO   UPGR4G                                                           
         CLI   QBOOKTYP,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   UPGFRBKT,QBOOKTYP                                                
         MVI   UPGELLN,UPGELLQ2    EXTENDED LENGTH                              
UPGR4G   MVC   UPGINPUT(L'AUDUPG),AUDUPG                                        
***      MVC   BWDUPUT,APWORK+16                                                
***      MVC   BWDUSHR,APWORK+17                                                
         MVC   SVUPFILE,UPGFILE    SAVE UPGRADE VALUES                          
         MVC   SVUPGRD,UPGRADE                                                  
         MVC   SVUPFRBK,UPGFRBK                                                 
         TM    UPGFRBK+1,BTY2CHAR                                               
         BNO   UPGR5G                                                           
         CLI   UPGELLN,UPGELLNQ                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   UPGFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SVUPFRBT,UPGFRBKT                                                
UPGR5G   XC    SVUPFBL,SVUPFBL                                                  
*        CLI   UPGELLN,51                                                       
*        BL    *+10                                                             
*        MVC   SVUPFBL,UPGFRBKL                                                 
         MVC   SVUPINP,AUDUPG                                                   
         MVC   SVUPPUT,APWORK+16                                                
         MVC   SVUPSHR,APWORK+17                                                
         OI    LCHG,LUPG                                                        
         OC    AUDUPG,SPACES                                                    
         OI    AUDUPGH+6,X'80'                                                  
*        OC    LAUPGEL,LAUPGEL                                                  
*        BNZ   UPGR6                                                            
*        GOTO1 AADDELS,BWDRECD     ADD NEW UPGRADE ELEM                         
*        MVC   LAUPGEL,16(R1)                                                   
*                                                                               
UPGR6    DS    0H                                                               
*PGR6    OC    APWORK+11(5),APWORK+11   TEST FOR OVERRIDE DAY/TIME              
*        BNZ   UPGR8                                                            
*        OC    LAODTEL,LAODTEL          NO - TEST FOR OVERRIDE ELEM             
*        BZ    UPGR14                                                           
*        MVI   APELEM,ODTELCDQ               YES - DELETE IT                    
*        GOTO1 ADELELS                                                          
*        XC    LAODTEL,LAODTEL                                                  
*        B     UPGR14                                                           
*                                                                               
*PGR8    ICM   R4,15,LAODTEL            YES - TEST FOR OVERRIDE ELEM            
*        BZ    UPGR10                                                           
*        USING ODTEL,R4                                                         
*        CLC   ODTDAY,APWORK+11                YES - TEST FOR CHANGES           
*        BNE   UPGR12                                                           
*        CLC   ODTTIME,APWORK+12                                                
*        BNE   UPGR12                                                           
*        B     UPGR14                                                           
*                                                                               
*PGR10   XC    APELEM,APELEM       BUILD OVERRIDE ELEM                          
*        LA    R4,APELEM                                                        
*        MVI   ODTELCD,ODTELCDQ                                                 
*        MVI   ODTELLN,ODTELLNQ                                                 
*                                                                               
*PGR12   MVC   ODTDAY,APWORK+11                                                 
*        MVC   ODTTIME,APWORK+12                                                
*        OI    LCHG,LUPG                                                        
*        OC    LAODTEL,LAODTEL                                                  
*        BNZ   UPGR14                                                           
*        GOTO1 AADDELS,BWDRECD     ADD NEW OVERRIDE ELEM                        
*        MVC   LAODTEL,16(R1)                                                   
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
* SET UP THE TIME PERIODS FOR EACH LINE                               *         
***********************************************************************         
TIMES    NTR1                                                                   
         OC    SVTIMEND,SVTIMEND   DO WE HAVE A TIME RANGE?                     
         BZ    TIME60               - NOPE, WE DON'T, RARE OCCASION             
         MVC   SVTIME2,SVTIME1                                                  
*                                                                               
         XR    R0,R0               GETTING READY TO DIVIDE                      
         XR    R1,R1                                                            
         ICM   R1,3,SVTIME2                                                     
         CR    R3,R1               SAVE OFF NUMBER FOR 2ND DIVISION             
         D     R0,=F'100'          CHECKING TO SEE IF TOP OF THE HOUR           
*                                                                               
         MHI   R1,100              NEED THE FULL HOUR AGAIN                     
         LTR   R0,R0               WAS IT TOP OF THE HOUR?                      
         BZ    TIME40               - YUP,ADD ONLY 30 (MINUTES)                 
         CHI   R0,30               MORE THAN 30 MINUTES REMAINING?              
         BL    TIME40               - NOPE                                      
         AHI   R1,100               - YEAH, ADD AN HOUR TO THE TIME             
         B     TIME50                                                           
*                                                                               
TIME40   AHI   R1,30               HALF AN HOUR                                 
*                                                                               
TIME50   STCM  R1,3,SVTIME2                                                     
         CLC   SVTIME2,SVTIMEND    IS SVTIME2 PAST SVTIMEND?                    
         BNH   *+10                 - NOPE, DON'T WORRY ABOUT IT                
         MVC   SVTIME2,SVTIMEND     - YUP, JUST USE END TIME                    
*                                                                               
TIME60   MVC   LTIMES,SVTIMNOW     FOR SDEMUP A BIT LATER                       
*                                                                               
TIMEX    B     EXIT                                                             
***********************************************************************         
* ERROR EXITS AND MESSAGES                                            *         
***********************************************************************         
         SPACE 1                                                                
ENOTSUB  MVC   FVMSGNO,=AL2(FVFSET)                                             
         ST    R2,FVADDR                                                        
         XC    BWSMSG,BWSMSG                                                    
         MVC   BWSMSG(L'NOTSUB),NOTSUB                                          
         OI    BWSMSGH+4,X'20'     VALIDATED                                    
         OI    BWSMSGH+6,X'80'                                                  
         MVC   BWSMSG(7),8(R2)     DENOMINATOR DEMO NAME                        
         MVC   BWSMSG+27(7),AUDMDM   NUMERATOR DEMO NAME                        
         B     EXIT                                                             
NOTSUB   DC    C'        IS NOT A SUBSET OF        '                            
*                                                                               
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
* CONSTANTS                                                                     
*                                                                               
AXTRA    DS    0F                  EXTENSION ROUTINE ADDRESSES                  
AVALRSP  DS    A                                                                
ADEMUP   DS    A                                                                
ASDEMUP  DS    A                                                                
AGETDEMS DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
TEMPSTR  DC    CL7'TEMPSTR'                                                     
DMREAD   DC    CL7'DMREAD '                                                     
DMWRITE  DC    CL7'DMWRT  '                                                     
*                                                                               
QUESTION DC    CL8'????????'                                                    
SPACES   DC    CL80' '                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,**B22X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALRSP                                                           
         B     DEMUP                                                            
         B     SDEMUP2                                                          
         B     GETDEMS                                                          
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE RATING AND SHARE/PUT FIELDS                                *         
***********************************************************************         
         SPACE 1                                                                
VALRSP   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SINGLE DEMO UPGRADE ROUTINE                                         *         
* INPUT  : LDEMLST=SINGLE DEMO                                        *         
* OUTPUT : LDEMVAL=DEMO VALUE                                         *         
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
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         LA    RE,LIUN                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   QSTA,C'0'           IS IT A NUMBER?                              
         BL    SDEMUP2E             - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),QSTA+5   MOVE THE NETWORK IN                          
         MVC   SPUPSYSE,QSTA                                                    
         B     SDEMUP2G                                                         
*****  CABLE/FUSION DATE LOOKUP                                                 
SDEMUP2E MVC   SPUPSTA,QSTA                                                     
SDEMUP2G MVC   SPUPDAY,BDAYS                                                    
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
         CLI   SPUPBTYP,0                                                       
         BNE   SDEMUP4                                                          
         CLI   CMPBKTYP,0                                                       
         BE    SDEMUP4                                                          
         MVC   SPUPBTYP,CMPBKTYP                                                
*                                                                               
SDEMUP4  DS    0H                                                               
         CLI   QBOOKTYP,0                                                       
         BNE   SDEMUP4E                                                         
         CLI   STABKTYP,0          ANYTHING HERE?                               
         BNE   SDEMUP4G             - NO, OVERRIDE CMPBKTYP REGARDLESS          
SDEMUP4E MVC   SPUPBTYP,QBOOKTYP                                                
SDEMUP4G TM    SPUPFBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    SDEMUP7                                                          
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   SDEMUP5              - NOPE                                      
         CLI   SVUPFRBT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,SVUPFRBT    - YUP                                       
         B     SDEMUP6                                                          
SDEMUP5  GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
SDEMUP6  NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
SDEMUP7  CLI   SVUPPUT,C'1'                                                     
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
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   SDEMUP8                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
SDEMUP8  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMLST,LDEMVAL                         
****  CANADA DOESN'T NEED TO OVERRIDE THE RATING SERVICE                        
         CLI   CUDMED,C'C'         TEST CANADA                                  
         BE    *+10                 - YUP, SKIP THE OVERRIDE                    
****  CANADA DOESN'T NEED TO OVERRIDE THE RATING SERVICE                        
         MVC   SVRTGSVC,SPUPACTS   SAVE ACTUAL RATING SERVICE                   
         MVC   SVPROGNM,SPUPPRG    SAVE PROGRAM NAME                            
*                                                                               
SDEMUPX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DEMO UPGRADE ROUTINE                                                *         
* OUTPUT : RATING SERVICE IS RETURNED IN LRTGSVC                      *         
***********************************************************************         
         SPACE 1                                                                
DEMUP    DS    0H                                                               
*                                                                               
DEMUX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET DEMO VALUES ROUTINE                                             *         
* ROUTINE ALSO EDITS THE PROGRAMS NAMES TO THE HEADLINES              *         
***********************************************************************         
         SPACE 1                                                                
GETDEMS  DS    0H                                                               
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
BLANKS   DC    CL20' '                                                          
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
NDEMOS   EQU   5                                                                
NDEMSCR  EQU   6                                                                
FF       EQU   X'FF'                                                            
OVERELEM EQU   X'DE'                                                            
         EJECT                                                                  
LOCALD   DSECT                                                                  
LASAVE   DS    A                                                                
LAPROJ   DS    A                                                                
LANDX    DS    A                                                                
LACOST   DS    A                                                                
LAUPGEL  DS    A                                                                
LAODTEL  DS    A                                                                
LADEMEL  DS    A                                                                
LASHPEL  DS    A                                                                
LASHPENT DS    A                                                                
LDEMADJ  DS    F                                                                
LDEMOLD  DS    F                                                                
LDNAMES  DS    (NDEMOS)CL7                                                      
LDMUPBLK DS    (SPDEMUP2)X                                                      
LESTDEMS DS    XL((NDEMOS+1)*3)                                                 
LAUDDEMS DS    (NDEMOS+2)XL4       IMPRESSIONS, RATING, AND SHARE               
LAUDDNFO DS    XL3                 DENOMINATOR SEX, START AGE, END AGE          
LAUDNNFO DS    XL3                 NUMERATOR SEX, START AGE, END AGE            
LDEMNO   DS    X                                                                
LNDEMSCR DS    X                                                                
LSVDPTLN DS    XL2                                                              
LDEMLST  DS    XL4                                                              
LDEMVAL  DS    XL4                                                              
LDPT     DS    CL1                                                              
LTIMES   DS    XL4                                                              
*                                                                               
LFLAG    DS    X                                                                
LADJIMP  EQU   X'80'                                                            
LADJALL  EQU   X'40'                                                            
LOVRALL  EQU   X'20'                                                            
LGETBKS  EQU   X'10'                                                            
LNEWDAYS EQU   X'08'                                                            
LDAYOVR  EQU   X'04'                                                            
LNEWTIME EQU   X'02'                                                            
LTIMOVR  EQU   X'01'                                                            
*                                                                               
LFLAG1   DS    X                                                                
LF1BUYRV EQU   X'80'                                                            
LF1NOLBK EQU   X'40'                                                            
LF1USEI  EQU   X'20'                                                            
*                                                                               
LCHG     DS    X                                                                
LDEMS    EQU   X'80'                                                            
LUPG     EQU   X'40'                                                            
LDAYS    EQU   X'20'                                                            
LTIME    EQU   X'10'                                                            
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
HEDLINED DSECT                                                                  
HEDPRG1  DS    CL12                PROGRAM NAME 1                               
         DS    CL1                                                              
HEDPRG2  DS    CL12                PROGRAM NAME 2                               
         DS    CL1                                                              
HEDPRG3  DS    CL12                PROGRAM NAME 3                               
         DS    CL1                                                              
HEDPRG4  DS    CL12                PROGRAM NAME 4                               
         SPACE 2                                                                
*                                                                               
CURLINE  DSECT                                                                  
CURLINEH DS    CL8                 THE HEADER                                   
CURTIME  DS    CL8                                                              
         DS    CL2                                                              
CURPROG  DS    CL16                                                             
         DS    CL1                                                              
CURRTG   DS    CL6                                                              
         DS    CL1                                                              
CURSHR   DS    CL5                                                              
         DS    CL5                                                              
CURDM1   DS    CL5                                                              
         DS    CL4                                                              
CURDM2   DS    CL5                                                              
         DS    CL4                                                              
CURDM3   DS    CL5                                                              
         DS    CL4                                                              
CURDM4   DS    CL5                                                              
*                                                                               
DEMTAB   DSECT                     SOLELY USED FOR LAUDDEMS                     
RTGVAL   DS    CL4                                                              
SHRVAL   DS    CL4                                                              
DMAVAL   DS    CL4                                                              
         EJECT                                                                  
SAVED    DSECT                     OVERLAY SAVE AREA DSECT                      
SVDEMNO  DS    X                                                                
SVUPFILE DS    C                                                                
SVUPGRD  DS    XL8                                                              
SVUPFRBK DS    XL2                                                              
SVUPFRBT DS    XL1                 SVUPFRBK BOOKTYPE                            
SVUPFBL  DS    XL6                                                              
SVUPINP  DS    CL32                                                             
SVUPDAY  DS    X                                                                
SVUPTIM  DS    XL4                                                              
SVUPPUT  DS    CL1                                                              
SVUPSHR  DS    CL1                                                              
SVRTGSVC DS    CL1                                                              
SVPROGNM DS    CL16                                                             
SVTIMNOW DS    0XL4                THE TIME NOW ON THIS LINE                    
SVTIME1  DS    XL2                 THE FIRST TIME - MILITARY                    
SVTIME2  DS    XL2                 THE SECOND TIME - MILITARY                   
SVTIME   DS    0XL4                SAVE TIME FOR MANIPULATION - BINARY          
SVTIMSTR DS    XL2                 THE STARTING TIME                            
SVTIMEND DS    XL2                 THE ENDING TIME                              
SVFLAG   DS    XL1                 FLAG FOR PAGING                              
SVF1TIME EQU   X'80'                - FIRST TIME THROUGH                        
SVDMA    DS    (NDEMOS)XL4                                                      
*                                                                               
         ORG   SAVED+1280      *** RESERVED FOR BWS14 ***                       
         DS    0H                                                               
         ORG   SAVED+6144                                                       
SAVEX    EQU   *                                                                
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
       ++INCLUDE DDSCANBLKD                                                     
         EJECT                                                                  
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSD3D                                                       
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
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPNWS22   03/20/07'                                      
         END                                                                    
