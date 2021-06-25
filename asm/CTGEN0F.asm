*          DATA SET CTGEN0F    AT LEVEL 072 AS OF 10/25/07                      
*&&      SET   NOP=N                                                            
*PHASE TA0B0FA                                                                  
         TITLE 'CTGEN0F - FILE MAINTENANCE - EXCHANGE RECORDS'                  
*                                                                               
* RMOR 11 06APR93 CORRECTIONS TO XCEF ROUTINE ETC.                              
* CMOL 10 05APR93 ALLOW EXPRESSIONS OF 1000 TO N, 100 TO N, 10 TO N             
* RMOR 41 29JUN98 RESTRICT ES TO XCEF ROUTINE ETC.                              
* RMOR 60 22MAY02 BAL/BALR -> BAS/BASR                                          
* TKLU 61 23JAN03 <DU01-0583> GERMAN FT RATES, UPDATE EUROTAB                   
* TKLU 62 18FEB03 BUG FIX FOR NAME DISPLAY AND DEFAULT VALUES                   
* TKLU 63 28MAR03 USE UPDATE OPTION (CTGEN01A) FOR DE RATES                     
* TKLU 64 03APR03 BUG FIXES LVL *63                                             
* TKLU 67 21SEP06 <LO01-5184> SIT (SLOVENIA) JOINS EURO                         
* TKLU 68 11JUL07 <DU01-6615> - MTL/MALTA AND CYP/CYPRUS JOIN EURO              
*                                                                               
GEN0F    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GENF**,RA,R8,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         CLI   ASONOFF,ASOFF                                                    
         BNE   *+8                                                              
         L     R6,ATIA             USE TIA IF OFFLINE                           
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GEXCD,R2            R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         L     RF,=A(VALKEY)                                                    
         A     RF,APRELO                                                        
         ST    RF,AVALKEY                                                       
         L     RF,=A(TYPTAB)                                                    
         A     RF,APRELO                                                        
         ST    RF,ATYPTAB                                                       
         L     RF,=A(ACCCLI)                                                    
         A     RF,APRELO                                                        
         ST    RF,AACCCLI                                                       
         L     RF,=A(NUMCHEK)                                                   
         A     RF,APRELO                                                        
         ST    RF,ANUMCHEK                                                      
*                                                                               
         L     R1,=A(SAVERECS-SAVAREA)                                          
         AR    R1,R6                                                            
         ST    R1,ASAVRECS         GET A(SAVERECS)                              
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BNE   *+10                                                             
         MVC   CUAUTH,=X'FFFF'                                                  
*                                                                               
INIT010  CLI   INITFLG,C'Y'        TEST DICT RESOLVED                           
         BE    INITX                                                            
         L     RF,ACOM                                                          
         L     RF,CDICTATE-COMFACSD(RF)                                         
         L     R3,=A(DDDCLST)                                                   
         A     R3,APRELO                                                        
         GOTO1 (RF),APPARM,C'LU  ',(R3),DDDSLST                                 
*                                                                               
         LA    RF,2                BUILD FORWARD RATE CONSTANTS                 
         LA    R1,DSFRWD                                                        
INIT011  MVC   0(2,R1),=C'1 '                                                   
         MVC   10(2,R1),=C'3 '                                                  
         MVC   20(2,R1),=C'6 '                                                  
         LA    R1,30(R1)                                                        
         BCT   RF,INIT011                                                       
         LA    R1,DSFRWD                                                        
         LA    RF,3                                                             
         MVC   2(6,R1),CT@MTH                                                   
         LA    R1,10(R1)                                                        
         BCT   RF,*-10                                                          
         LA    RF,3                                                             
         MVC   1(6,R1),CT@MTH                                                   
         LA    R1,10(R1)                                                        
         BCT   RF,*-10                                                          
         MVI   INITFLG,C'Y'                                                     
         SPACE 1                                                                
         BAS   RE,GETSYS           GET SYSTEM INFO                              
         L     RF,SCAUTL           GET DEFAULT CURRENCY                         
         MVC   AGYCUR,TAGCURR-UTLD(RF)                                          
         OC    AGYCUR,AGYCUR                                                    
         BNZ   INIT012                                                          
         MVC   AGYCUR,=C'GBP'      SET TO GBP                                   
         CLI   CULANG,3                                                         
         BNE   *+10                                                             
         MVC   AGYCUR,=C'EUR'      SET TO DEM, NOW EUR                          
*                                                                               
INIT012  CLC   AGYCUR,=C'DEM'      ENSURE ALL POINTS TO EURO                    
         BNE   INIT014                                                          
         MVC   AGYCUR,=C'EUR'                                                   
*                                                                               
INIT014  MVC   ONEVAL,=C'1.0'      SET DEFAULTS                                 
         MVC   TENVAL,=C'10.0'                                                  
         MVC   HUNVAL,=C'100.0'                                                 
         MVC   THOUVAL,=C'1000.0'                                               
         CLI   CULANG,2            CHANGE DECIMAL POINTS IF NOT UK              
         BNH   INITX                                                            
         MVI   ONEVAL+1,C','                                                    
         MVI   TENVAL+2,C','                                                    
         MVI   HUNVAL+3,C','                                                    
         MVI   THOUVAL+4,C','                                                   
INITX    EQU   *                                                                
         MVC   TODEF,AGYCUR        SET DEFAULT DEFAULTS                         
         MVC   FROMDEF,CT@ALL                                                   
*                                                                               
         EJECT                                                                  
         CLI   APMODE,1            DISPLAY CALL                                 
         BE    MAIN                                                             
         CLI   APMODE,15           VALIDATE REQ CALL                            
         BE    VALREQ                                                           
         CLI   APMODE,16           PRINT REPORT CALL                            
         BE    PRTREP                                                           
*                                                                               
XEXIT    L     RD,APWORKA          RESTORE BASE RD                              
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
*        MAIN ONLINE CONTROL                                          *         
***********************************************************************         
         SPACE 1                                                                
MAIN     MVI   APFLAG,0                                                         
         CLC   SAVEACT,APACTN                                                   
         BE    MAIN005                                                          
         OI    APFLAG,X'20'        FLAG ACTION CHANGED                          
         MVC   SAVEACT,APACTN                                                   
*                                                                               
MAIN005  GOTO1 AVALKEY,DMCB,(0,(RC))                                            
         BNE   XEXIT                                                            
         CLI   SELSYS,GEKSFTQ      ARE WE USING FT RATES                        
         BE    MAIN05A                                                          
         CLI   SELSYS,GEKSDEQ      ARE WE USING GERMAN FT RATES                 
         BNE   MAIN05B                                                          
         CLI   OPTTES,C'Y'         UP}DATE OPTION?                              
         BE    MAIN05B                                                          
*                                                                               
MAIN05A  CLI   APACTN,ACTDIS       DISPLAY IS THE ONLY VALID ACTION             
         BE    MAIN05B                                                          
         CLI   FTUPFLAG,C'Y'                                                    
         BE    MAIN05B                                                          
         L     RF,SCAUTL                                                        
         CLC   TSYM-UTLD(5,RF),=C'DUMMY'                                        
         BE    MAIN05B                                                          
         CLC   TUSER-UTLD(2,RF),=X'071B' ALLOW DDSZ                             
         BE    MAIN05B                                                          
         BNE   ECMSG164                                                         
*                                                                               
MAIN05B  OC    SAVEKEY,SAVEKEY     ANY KEY SAVED?                               
         BZ    MAIN010                                                          
         TM    APFLAG,X'20'        ACTION CHANGED                               
         BNO   MAIN006                                                          
         CLI   APACTN,ACTCHA                                                    
         BE    MAIN021             IF CHANGE KEEP SCREEN THE SAME               
         B     MAIN010             ELSE START AGAIN                             
*                                                                               
MAIN006  CLC   SAVESEL,SELKEY      SAME KEY AS PREVIOUS?                        
         BE    MAIN020                                                          
         MVI   MODE,0                                                           
*                                                                               
MAIN010  EQU   *                                                                
         MVC   SAVESEL,SELKEY      SAVE THIS KEY                                
         MVC   SAVEKEY,SELKEY                                                   
*                                                                               
         MVC   GEKAGY,SELAGY       BUILD KEY                                    
         MVC   GEKSYS,SELSYS                                                    
         MVC   GEKCTYP,SELTYPE                                                  
         MVC   GEKKEY,SELKKEY                                                   
         MVC   GEKCURF,SELCURF                                                  
         MVC   GEKCURT,SELCURT                                                  
         MVC   GEKPEND,SELPSTA                                                  
         B     MAIN022                                                          
*                                                                               
MAIN020  MVC   GEKEY,SAVEKEY       CONTINUE WITH LIST                           
         B     MAIN022                                                          
*                                                                               
MAIN021  L     R1,ASAVRECS                                                      
         MVC   GEKEY,0(R1)                                                      
*                                                                               
MAIN022  XC    SAVEKEY,SAVEKEY                                                  
         CLI   APACTN,ACTADD       TEST FOR ADD                                 
         BNE   MAIN030                                                          
         BAS   RE,VALADD           GOTO VALADD                                  
         B     EXIT                                                             
*                                                                               
MAIN030  CLI   APACTN,ACTCHA       TEST FOR CHANGE                              
         BNE   MAIN040                                                          
         CLC   IOKEY,CHNGKEY                                                    
         MVC   CHNGKEY,IOKEY                                                    
         BNE   MAIN090                                                          
         CLI   MODE,0              HAVE WE DONE A CHANGE PASS                   
         BE    MAIN090                                                          
         BAS   RE,VALCHA           VALIDATE CHANGES                             
         CLI   MODE,1                                                           
         BNE   MAIN090                                                          
         L     R1,ASAVRECS                                                      
         MVC   GEKEY,0(R1)                                                      
         B     MAIN090                                                          
*                                                                               
MAIN040  CLI   APACTN,ACTDEL       TEST FOR DELETE OR RESTORE                   
         BE    *+12                                                             
         CLI   APACTN,ACTRES                                                    
         BNE   MAIN090                                                          
         BAS   RE,VALDEL           TEST FOR SELECT FIELDS                       
         CLI   MODE,1                                                           
         BNE   MAIN090                                                          
         L     R1,ASAVRECS                                                      
         MVC   GEKEY,0(R1)                                                      
*                                                                               
MAIN090  BAS   RE,DISREC                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD EXCHANGE RECORDS                              *         
***********************************************************************         
         SPACE 1                                                                
VALADD   NTR1                                                                   
         LA    R4,LSTACT1H         R4=SCREEN START                              
         USING EXLINED,R4                                                       
         BAS   RE,TWAPROC          PROTECT SELECTED FIELDS                      
         LA    R4,LSTACT1H         R4=SCREEN START                              
         CLI   MODE,0                                                           
         BNE   VALA010                                                          
         TM    APFLAG,X'20'        DON'T CLEAR SCREEN FIRST TIME                
         BO    VALA005                                                          
         BAS   RE,TWAXC            CLEAR OLD DATA                               
         MVI   MODE,1                                                           
         LA    R4,LSTACT1H         R4=SCREEN START                              
         LA    R1,EXLCURH-EXLINED(R4)                                           
         ST    R1,APCURSOR                                                      
         B     ICMSG26             ENTER DATA                                   
*                                                                               
VALA005  MVI   MODE,1              1ST TIME LEAVE SCREEN ALONE                  
         CLI   EXLCUR,C' '                                                      
         BNH   VALA006                                                          
         LA    R4,LSTACT2-LSTACT1(R4)                                           
         LA    R1,LSTLAST                                                       
         CR    R4,R1                                                            
         BL    VALA005             TEST FOR END OF SCREEN                       
         LA    R4,LSTACT1H         R4=SCREEN START                              
         BAS   RE,TWAXC                                                         
VALA006  LA    R1,EXLCURH-EXLINED(R4)                                           
         ST    R1,APCURSOR                                                      
         B     ICMSG26             ENTER DATA                                   
*                                                                               
VALA010  MVI   MODE,2              SET MODE2                                    
VALA011  BAS   RE,VALLINE          VALIDATE EXCHANGE RECORD                     
         BNE   VALA090                                                          
*                                                                               
VALA012  MVC   IOKEY,IOAREA1       MAKE SURE IT'S NOT FOUND                     
         GOTO1 AIO,IORDD+IOGENDIR+IO2                                           
         TM    IOERR,IOERNF                                                     
         BO    VALA020                                                          
         TM    IOERR,IOEDEL                                                     
         BO    EGMSG58                                                          
         B     EGMSG49                                                          
*                                                                               
VALA020  MVC   IOKEY,IOAREA1       ADD THIS RECORD                              
         GOTO1 AIO,IOADD+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MODE,1              SET TO INPUT MODE                            
*                                                                               
VALA090  LA    R4,LSTACT2-LSTACT1(R4)                                           
         LA    R1,LSTLAST                                                       
         CR    R4,R1                                                            
         BL    VALA011             TEST FOR END OF SCREEN                       
         CLI   MODE,1                                                           
         BNE   VALA099                                                          
         LA    R4,LSTACT1H                                                      
VALA091  CLI   EXLCURH+5,0                                                      
         BE    VALA092                                                          
         LA    R4,LSTACT2-LSTACT1(R4)                                           
         LA    R1,LSTLAST                                                       
         CR    R4,R1                                                            
         BL    VALA091             TEST FOR END OF SCREEN                       
         B     VALA099                                                          
VALA092  LA    R1,EXLCURH                                                       
         ST    R1,APCURSOR                                                      
         B     ICMSG23             RECORDS ADDED                                
*                                                                               
VALA099  LA    R4,LSTACT1H         R4=SCREEN START                              
         BAS   RE,TWAXC            CLEAR OLD DATA                               
         MVI   MODE,1                                                           
         LA    R4,LSTACT1H         R4=SCREEN START                              
         LA    R1,EXLCURH-EXLINED(R4)                                           
         ST    R1,APCURSOR                                                      
         B     ICMSG26             ENTER DATA                                   
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE CHANGES                                  *         
***********************************************************************         
         SPACE 1                                                                
VALCHA   NTR1                                                                   
         LA    R4,LSTACT1H         R4=SCREEN START                              
         BAS   RE,TWAPROC          PROTECT SELECTED FIELDS                      
         LA    R4,LSTACT1H         R4=SCREEN START                              
         MVI   MODE,0                                                           
VALC010  BAS   RE,VALLINE          VALIDATE EXCHANGE RECORD                     
         BNE   VALC090                                                          
         BAS   RE,GETSAVE          FIND SAVED RECORD                            
         CLC   0(L'SAVERECS,R1),IOAREA1                                         
         BE    VALC090                                                          
         OC    0(L'SAVERECS,R1),0(R1)                                           
         BZ    VALC090                                                          
*                                                                               
         L     R2,AIOAREA2         CHANGE RECORD                                
         MVC   0(L'SAVERECS,R2),0(R1)                                           
         MVC   IOKEY,0(R2)                                                      
         GOTO1 AIO,IORD+IOGENDIR+IOLOCK+IO2                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY+GEDSTAT-GEXCD(1),IOAREA1+GESTAT-GEXCD                      
         GOTO1 AIO,IOWRITE+IOGENDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOGENFIL+IOLOCK+IO2                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         MVC   GEKEY(L'SAVERECS),IOAREA1                                        
         GOTO1 AIO,IOPUT+IOGENFIL+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MODE,1              CHANGES MADE SO MODE 1 STILL                 
*                                                                               
VALC090  LA    R4,LSTACT2-LSTACT1(R4)                                           
         LA    R1,LSTLAST                                                       
         CR    R4,R1                                                            
         BL    VALC010             TEST FOR END OF SCREEN                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK FOR DELETE OR RESTORE                       *         
***********************************************************************         
         SPACE 1                                                                
VALDEL   NTR1                                                                   
         LA    R4,LSTACT1H         R4=SCREEN START                              
         ST    R4,APCURSOR                                                      
         USING EXLINED,R4                                                       
         MVC   APFULL,CT@DEL       SET WHAT TO LOOK FOR IN SEL                  
         CLI   APACTN,ACTDEL                                                    
         BE    *+10                                                             
         MVC   APFULL,CT@RSR                                                    
*                                                                               
VALD010  TM    EXLSELH+FVIIND-FVIHDR,FVITHIS                                    
         BNO   VALD090                                                          
         CLI   EXLSEL,C' '         DID HE ENTER A BLANK                         
         BL    VALD090             IGNORE IT                                    
         ST    R4,APCURSOR                                                      
         SR    R1,R1                                                            
         IC    R1,EXLSELH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   APFULL(0),EXLSEL                                                 
         BNE   ECMSG2                                                           
         BAS   RE,GETSAVE                                                       
*                                                                               
         CLI   2(R1),GEKRECQ       DO WE HAVE A RECORD HERE                     
         BNE   VALD090                                                          
*                                                                               
VALD020  LA    R2,IOKEY            DELETE OR RESTORE RECORD                     
         MVC   IOKEY,0(R1)                                                      
         GOTO1 AIO,IORDD+IOGENDIR+IOLOCK+IO1                                    
         TM    IOERR,255-X'02'                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    GEDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         CLI   APACTN,ACTDEL                                                    
         BE    VALD021                                                          
         GOTO1 CHECKUP             TEST IF OK TO ADD                            
         BNE   ERRDATE                                                          
         GOTO1 CHECKEUR            TEST IF OK FOR EURO                          
         BH    ERREURO                                                          
         BL    ERREDATE                                                         
         NI    GEDSTAT,255-X'80'   CLEAR DELETE FLAG IN DIRECTORY               
VALD021  GOTO1 AIO,IOWRITE+IOGENDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALD030  LA    R2,IOAREA1                                                       
         GOTO1 ASETACT,GEXCD                                                    
         GOTO1 AIO,IOGET+IOGENFIL+IOLOCK+IO1                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    GESTAT,X'80'        SET DELETE                                   
         CLI   APACTN,ACTDEL                                                    
         BE    VALD031                                                          
         NI    GESTAT,X'FF'-X'80'  UNSET DELETE                                 
VALD031  GOTO1 ASETACT,GEXCD                                                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MODE,1                                                           
*                                                                               
VALD090  LA    R4,LSTACT2-LSTACT1(R4)                                           
         LA    R1,LSTLAST                                                       
         CR    R4,R1                                                            
         BL    VALD010             TEST FOR END OF SCREEN                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY EXCHANGE RECORDS                          *         
***********************************************************************         
         SPACE 1                                                                
DISREC   MVI   MODE,0              RESET MODE                                   
         L     R2,AIOAREA1         R2=AIOAREA                                   
         LA    R4,LSTACT1H         R4=SCREEN START                              
         BAS   RE,TWAPROC          PROTECT SELECTED FIELDS                      
         LA    R4,LSTACT1H         R4=SCREEN START                              
         BAS   RE,TWAXC            CLEAR PREVIOUS SCREEN                        
*                                                                               
         L     R0,ASAVRECS         CLEAR SAVERECS                               
         LHI   R1,14*L'SAVERECS                                                 
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    APINDS,APILFLST     SET FIRST TIME FLAG                          
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL+APIOKADD                       
         BAS   RE,GETSEL                                                        
         XC    SAVEKEY,SAVEKEY                                                  
         CLI   APMODE,APMEOFS      NO RECORDS TO COME                           
         BE    IGMSG26             NOTHING TO DISPLAY                           
*                                                                               
DISR050  BAS   RE,DISLINE          DISPLAY AN EXCHANGE RECORD                   
*                                                                               
DISR051  BAS   RE,GETSAVE          SAVE RECORD                                  
         MVC   0(L'SAVERECS,R1),IOAREA1                                         
*                                                                               
         BAS   RE,GETSEL           GET NEXT RECORD                              
         CLI   APMODE,APMEOFS      NO MORE RECORDS TO COME                      
         BE    DISR090                                                          
         LA    R4,LSTACT2-LSTACT1(R4)                                           
         LA    R1,LSTLAST                                                       
         CR    R4,R1                                                            
         BL    DISR050             TEST FOR END OF SCREEN                       
*                                                                               
         MVC   SAVEKEY,IOKEY       SAVE KEY VALUE                               
         CLI   APACTN,ACTDEL                                                    
         BE    ICMSG24             DELETE MSG                                   
         CLI   APACTN,ACTRES                                                    
         BE    ICMSG25             RESTORE MSG                                  
*                                                                               
         CLI   APACTN,ACTCHA                                                    
         BNE   IGMSG15             LIST DISPLAYED HIT ENTER                     
         LA    R1,LSTACT1H                                                      
         LA    R1,EXLSELH-EXLINED(R1)                                           
         ST    R1,APCURSOR                                                      
         MVI   MODE,1                                                           
         B     IGMSG32                                                          
*                                                                               
DISR090  EQU   *                                                                
DISR091  XC    SAVEKEY,SAVEKEY                                                  
         CLI   APACTN,ACTDIS                                                    
         BE    IGMSG16             END OF LIST                                  
         LA    R1,LSTACT1H                                                      
         LA    R1,EXLSELH-EXLINED(R1)                                           
         ST    R1,APCURSOR                                                      
         CLI   APACTN,ACTDEL                                                    
         BE    ICMSG24             DELETE MSG                                   
         CLI   APACTN,ACTRES                                                    
         BE    ICMSG25             RESTORE MSG                                  
         MVI   MODE,1                                                           
         B     IGMSG33             CHANGE MSG                                   
         EJECT                                                                  
*************************************************************                   
*        GET NEXT FOR LIST                                  *                   
*************************************************************                   
         SPACE 1                                                                
GETSEL   NTR1                                                                   
         LA    R2,IOKEY                                                         
         TM    APINDS,APILFLST     TEST FIRST TIME FLAG                         
         BNO   GETSEL2                                                          
         NI    APINDS,255-APILFLST                                              
         B     GETSEL6             READ HIGH                                    
GETSEL2  EQU   *                                                                
         TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GETSEL4                                                          
         NI    APINDS,255-APILRERD                                              
         LA    R1,IOGENDIR+IORDD+IO1                                            
         GOTO1 AIO                                                              
         TM    IOERR,255-X'02'                                                  
         BNZ   GETSELN                                                          
         B     GETSEL8                                                          
GETSEL4  TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GETSEL8                                                          
GETSEL6  LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOGENDIR+IOSQ+IO1                                             
         CLI   APACTN,ACTRES       TEST FOR RESTORE                             
         BNE   GETSEL9                                                          
         LA    R1,IORDEL(R1)       READ DELETES                                 
GETSEL9  GOTO1 AIO                                                              
         CLI   IOERR,IOEEOF                                                     
         BE    GETSELN                                                          
*                                                                               
         OI    APINDS,APILNSEQ     SET FOR SEQ READS                            
         LA    R2,IOKEY                                                         
         CLI   GEKREC,GEKRECQ      CHECK STILL EXCHANGE RECORD                  
         BNE   GETSELN                                                          
*                                                                               
         CLC   SELAGY,GEKAGY       TEST STILL SAME AGY                          
         BNE   GETSELN                                                          
*                                                                               
         CLC   SELSYS,GEKSYS       TEST STILL SAME SYSTEM                       
         BNE   GETSELN                                                          
*                                                                               
         CLC   SELCURF,GEKCURF     TEST FROM CURRENCY                           
         BE    GETSEL10                                                         
         OC    SELCURF,SELCURF     TEST FROM ALL CURRENCYS                      
         BNZ   GETSELN                                                          
*                                                                               
GETSEL10 OC    SELCURT,SELCURT     TEST TO ALL                                  
         BZ    GETSEL11                                                         
         CLC   SELCURT,GEKCURT     TEST TO CURRENCY                             
         BE    GETSEL11                                                         
         OC    SELCURF,SELCURF     TEST FROM ALL CURRENCYS                      
         BNZ   GETSELN                                                          
         SR    R1,R1                                                            
         ICM   R1,7,GEKCURT                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,7,GEKCURT                                                     
         XC    GEKCTYP(GEDSTAT-GEKCTYP),GEKCTYP                                 
         B     GETSEL6                                                          
*                                                                               
GETSEL11 CLC   SELTYPE,GEKCTYP     TEST EXCHANGE TYPE                           
         BE    GETSEL12                                                         
*NOP     OC    SELCURF,SELCURF                                                  
*NOP     BNZ   GETSELN                                                          
GSEL11A  SR    R1,R1                                                            
         IC    R1,GEKCTYP                                                       
         LA    R1,1(R1)                                                         
         STC   R1,GEKCTYP                                                       
         XC    GEKKEY(GEDSTAT-GEKKEY),GEKKEY                                    
         B     GETSEL6                                                          
         EJECT                                                                  
GETSEL12 CLI   SELSYS,MEDQ         MEDIA                                        
         BE    GTSMED                                                           
         CLI   SELSYS,ACCQ         ACCOUNTING                                   
         BE    GTSACC                                                           
         CLI   SELSYS,GEKSFTQ      NO FILTER FOR FINSTAT                        
         BE    GETSEL13                                                         
         CLI   SELSYS,GEKSDEQ      NO FILTER FOR GFTSTAT                        
         BE    GETSEL13                                                         
         SPACE 1                                                                
************************************************                                
*      MEDIA SYSTEM KEY FILTERS                *                                
************************************************                                
         SPACE 1                                                                
GTSMED   CLC   GEKCLI,SELCLI                                                    
         BE    GETSEL13                                                         
         OC    SELCLI,SELCLI                                                    
         BZ    GETSEL13                                                         
         B     GETSEL8                                                          
         SPACE 1                                                                
************************************************                                
*      ACCOUNTING SYSTEM KEY FILTERS           *                                
************************************************                                
         SPACE 1                                                                
GTSACC   CLC   GEKACT,SELACT                                                    
         BE    GETSEL13                                                         
         OC    SELACT,SELACT                                                    
         BZ    GETSEL13                                                         
         B     GETSEL8                                                          
         EJECT                                                                  
GETSEL13 CLC   GEKPEND,SELPSTA     IS END BEFORE MY START                       
         BNL   GSEL13A                                                          
         MVC   GEKPEND,SELPSTA     USE MY START AND READ HI                     
         XC    GEKPSTA,GEKPSTA                                                  
         B     GETSEL6                                                          
GSEL13A  CLC   GEKPSTA,SELPEND     IS START AFTER MY END                        
         BNH   GETSELY                                                          
         B     GETSEL8                                                          
*                                                                               
GETSELY  CLI   APACTN,ACTRES       IF RESTORE READ DELETES ONLY                 
         BNE   GETSELY1                                                         
         CLI   IOERR,IOEDEL                                                     
         BE    GETSELY1                                                         
         OI    APINDS,APILRERD     FLAG RDSEQ BROKEN                            
         B     GETSEL2                                                          
*                                                                               
GETSELY1 GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
*                                                                               
         MVC   APRECKEY(L'GEKEY),GEKEY                                          
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CALCULATE WHICH SAVE ENTRY TO USE R1=A(ENTRY)     *         
***********************************************************************         
         SPACE 1                                                                
GETSAVE  ST    RE,SAVERE                                                        
         LR    R1,R4                                                            
         LA    R0,LSTACT1H                                                      
         SR    R1,R0                                                            
         SR    R0,R0                                                            
         LHI   RE,LSTACT2-LSTACT1                                               
         DR    R0,RE                                                            
         MHI   R1,L'SAVERECS                                                    
         A     R1,ASAVRECS                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        TWAXC R4 TO END OF SCREEN                                    *         
***********************************************************************         
         SPACE 1                                                                
TWAXC    ST    RE,SAVERE                                                        
         TWAXC (R4),PROT=Y                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
TWAPROC  ST    RE,SAVERE                                                        
         USING EXLINED,R4                                                       
         MVI   APBYTE,0                                                         
         CLI   APACTN,ACTDIS                                                    
         BE    TPROC001                                                         
         CLI   APACTN,ACTDEL                                                    
         BE    TPROC001                                                         
         CLI   APACTN,ACTRES                                                    
         BNE   *+8                                                              
TPROC001 OI    APBYTE,CURPROTQ+CLIPROTQ+RATPROTQ+PERPROTQ                       
         CLI   APACTN,ACTCHA                                                    
         BNE   *+8                                                              
         OI    APBYTE,CURPROTQ+CLIPROTQ+PERPROTQ                                
*                                                                               
TPROC010 NI    EXLCURH+FVATRB-FVIHDR,255-FVAPROT                                
         NI    EXLCURTH+FVATRB-FVIHDR,255-FVAPROT                               
         NI    EXLCLIH+FVATRB-FVIHDR,255-FVAPROT                                
         NI    EXLRTFH+FVATRB-FVIHDR,255-FVAPROT                                
         NI    EXLRTTH+FVATRB-FVIHDR,255-FVAPROT                                
         NI    EXLPERH+FVATRB-FVIHDR,255-FVAPROT                                
*                                                                               
         OI    EXLCURH+FVOIND-FVIHDR,FVOXMT                                     
         OI    EXLCURTH+FVOIND-FVIHDR,FVOXMT                                    
         OI    EXLCLIH+FVOIND-FVIHDR,FVOXMT                                     
         OI    EXLNAMH+FVOIND-FVIHDR,FVOXMT                                     
         OI    EXLRTFH+FVOIND-FVIHDR,FVOXMT                                     
         OI    EXLRTTH+FVOIND-FVIHDR,FVOXMT                                     
         OI    EXLPERH+FVOIND-FVIHDR,FVOXMT                                     
*                                                                               
         TM    APBYTE,CURPROTQ                                                  
         BNO   *+20                                                             
         OI    EXLCURH+FVATRB-FVIHDR,FVAPROT                                    
         OI    EXLCURTH+FVATRB-FVIHDR,FVAPROT                                   
         NI    EXLCURH+FVIIND-FVIHDR,255-FVITHIS                                
         NI    EXLCURTH+FVIIND-FVIHDR,255-FVITHIS                               
*                                                                               
         TM    APBYTE,CLIPROTQ                                                  
         BNO   *+12                                                             
         OI    EXLCLIH+FVATRB-FVIHDR,FVAPROT                                    
         NI    EXLCLIH+FVIIND-FVIHDR,255-FVITHIS                                
*                                                                               
         TM    APBYTE,RATPROTQ                                                  
         BNO   *+20                                                             
         OI    EXLRTFH+FVATRB-FVIHDR,FVAPROT                                    
         OI    EXLRTTH+FVATRB-FVIHDR,FVAPROT                                    
         NI    EXLRTFH+FVIIND-FVIHDR,255-FVITHIS                                
         NI    EXLRTTH+FVIIND-FVIHDR,255-FVITHIS                                
*                                                                               
         TM    APBYTE,PERPROTQ                                                  
         BNO   *+12                                                             
         OI    EXLPERH+FVATRB-FVIHDR,FVAPROT                                    
         NI    EXLPERH+FVIIND-FVIHDR,255-FVITHIS                                
*                                                                               
         TM    EXLCURH+FVIIND-FVIHDR,FVITHIS                                    
         BO    TPROC020                                                         
         TM    EXLCURTH+FVIIND-FVIHDR,FVITHIS                                   
         BO    TPROC020                                                         
         TM    EXLCLIH+FVIIND-FVIHDR,FVITHIS                                    
         BO    TPROC020                                                         
         TM    EXLRTFH+FVIIND-FVIHDR,FVITHIS                                    
         BO    TPROC020                                                         
         TM    EXLRTTH+FVIIND-FVIHDR,FVITHIS                                    
         BO    TPROC020                                                         
         TM    EXLPERH+FVIIND-FVIHDR,FVITHIS                                    
         BNO   TPROC021                                                         
*                                                                               
TPROC020 OI    EXLSELH+FVIIND-FVIHDR,FVITHIS                                    
         OI    EXLSELH+6,X'81'                                                  
TPROC021 LA    R4,LSTACT2-LSTACT1(R4)                                           
         LA    R1,LSTLAST                                                       
         CR    R4,R1                                                            
         BL    TPROC010            TEST FOR END OF SCREEN                       
*                                                                               
TPEXIT   L     RE,SAVERE                                                        
         BR    RE                                                               
CURPROTQ EQU   X'80'                                                            
CLIPROTQ EQU   X'40'                                                            
RATPROTQ EQU   X'20'                                                            
PERPROTQ EQU   X'10'                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         XC    SELKEY,SELKEY       SELECTION CRITERION                          
         XC    APRECKEY,APRECKEY                                                
         MVC   SELAGY,TWAAGY       SET AGENCY                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPSYSH       VALIDATE EXCHANGE SYSTEM                     
         BNE   VALREQX                                                          
         CLI   FVILEN,2                                                         
         BNE   VRQ005                                                           
         CLC   FVIFLD(2),FINSTAT   TEST FOR FT                                  
         BE    VRQ010                                                           
         CLC   FVIFLD(2),GFTSTAT   TEST FOR GERMAN FT                           
         BE    VRQ015                                                           
*                                                                               
VRQ005   CLI   FVILEN,3                                                         
         BNE   *+14                                                             
         CLC   FVIFLD(3),CT@ALL                                                 
         BE    VRQ018                                                           
         GOTO1 AVALSYS,REPSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALREQX                                                          
         MVC   FVMSGNO,=AL2(FVFESYS)                                            
         CLI   APWORK,MEDQ         MEDIA                                        
         BE    *+12                                                             
         CLI   APWORK,ACCQ         ACCOUNTING                                   
         BNE   VALREQX                                                          
         MVC   SELSYS,APWORK                                                    
         B     VRQ018                                                           
*                                                                               
VRQ010   MVI   SELSYS,GEKSFTQ      SYSTEM FT                                    
         XC    SELAGY,SELAGY                                                    
         B     VRQ018                                                           
*                                                                               
VRQ015   MVI   SELSYS,GEKSDEQ      SYSTEM DE (GERMAN FT)                        
         XC    SELAGY,SELAGY                                                    
*                                                                               
VRQ018   MVC   FVMSGNO,=AL2(FVFESYS)                                            
         CLI   SELSYS,MEDQ         CHECK AUTHORIZED                             
         BNE   *+12                                                             
         TM    CUAUTH+1,AUT2EXM                                                 
         BNO   VALREQX                                                          
         CLI   SELSYS,ACCQ                                                      
         BNE   *+12                                                             
         TM    CUAUTH+1,AUT2EXA                                                 
         BNO   VALREQX                                                          
         CLI   SELSYS,GEKSFTQ                                                   
         BE    VRQ019                                                           
         CLI   SELSYS,GEKSDEQ                                                   
         BNE   VRQ020                                                           
VRQ019   TM    CUAUTH+1,AUT2EXF                                                 
         BNO   VALREQX                                                          
*                                                                               
VRQ020   CLI   SELSYS,0                                                         
         BNE   VRQ021                                                           
         TM    CUAUTH+1,AUT2EXF+AUT2EXM+AUT2EXA                                 
         BNO   VALREQX                                                          
*                                                                               
VRQ021   MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPCURFH      VALIDATE FROM CURRENCY                       
         BNE   VALREQX                                                          
         CLC   FVIFLD(3),CT@ALL                                                 
         BE    VRQ030                                                           
         BAS   RE,VALCUR                                                        
         BNE   VALREQX                                                          
         MVC   SELCURF,FVIFLD                                                   
*                                                                               
VRQ030   MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPCURTH      VALIDATE TO CURRENCY                         
         BNE   VRQ035                                                           
         CLC   FVIFLD(3),CT@ALL                                                 
         BE    VRQ040                                                           
         BAS   RE,VALCUR                                                        
         BNE   VALREQX                                                          
         MVC   SELCURT,FVIFLD                                                   
         B     VRQ040                                                           
VRQ035   MVC   SELCURT,TODEF       DEFAULT AGYCUR                               
         MVC   APWORK(3),TODEF                                                  
         GOTO1 DISPFLD,REPCURTH                                                 
*                                                                               
VRQ040   MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPTYPH       EXCHANGE TYPE                                
         BNE   VRQ050                                                           
         CLI   FVILEN,3                                                         
         BNE   *+14                                                             
         CLC   FVIFLD(3),CT@ALL    TEST FOR ALL TYPES                           
         BE    VRQ050                                                           
         L     R1,ATYPTAB                                                       
         MVC   MYSYS,SELSYS                                                     
         CLI   MYSYS,GEKSDEQ       FOR HERE SET TO FT                           
         BNE   *+8                                                              
         MVI   MYSYS,GEKSFTQ                                                    
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
VRQ041   EX    0,0(R1)             RF=A(KEYWORK)                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RF)     TEST TYPE                                    
         BNE   VRQ042                                                           
         CLC   MYSYS,5(R1)         TEST CORRECT TYPE FOR SYSTEM                 
         BE    VRQ045                                                           
VRQ042   LA    R1,6(R1)                                                         
         CLI   0(R1),0             ALL TYPES TESTED ?                           
         BNE   VRQ041                                                           
         B     ERRTYPE             SET FOR INVALID RATE                         
*                                                                               
VRQ045   MVC   SELTYPE,4(R1)       MOVE IN TYPE                                 
*                                                                               
VRQ050   EQU   *                                                                
         MVC   SELKKEY(6),FFS      SET DEFAULT FOR START                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPCLIH                                                    
         BNE   VRQ060              NO INPUT = DEFAULT                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   SELSYS,MEDQ                                                      
         BNE   VALREQX                                                          
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),CT@DFALT                                               
         BE    VRQ060                                                           
*                                                                               
VRQ052   CLC   FVIFLD(3),CT@ALL                                                 
         BNE   *+14                                                             
         XC    SELCLI,SELCLI       SET ALL CLIENTS                              
         B     VRQ060                                                           
         CLI   FVILEN,3            TEST FOR A9999                               
         BH    *+14                                                             
         MVC   SELCLI,FVIFLD       OR SPECIFIC                                  
         B     VRQ060                                                           
         CLI   FVILEN,5                                                         
         BNE   ERRCLIC             INVALID CLIENT                               
         MVC   APFULL,FVIFLD+1                                                  
         NC    APFULL,NUMERIC                                                   
         CLC   APFULL,NUMERIC                                                   
         BNE   VALREQX                                                          
         MVC   SELCLI(1),FVIFLD                                                 
         PACK  APDUB,FVIFLD+1(4)                                                
         CVB   R1,APDUB                                                         
         STCM  R1,3,SELCLI+1                                                    
*                                                                               
VRQ060   MVC   SELPEND,FFS                                                      
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPPERH                                                    
         BNE   VRQ069                                                           
         XC    SELPEND,SELPEND                                                  
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         SHI   R1,3                                                             
         BM    VRQ065                                                           
         LA    R1,FVIFLD(R1)                                                    
         CLC   0(3,R1),CT@UFN      TEST FOR UFN                                 
         BNE   VRQ065                                                           
         MVC   SELPEND,FFS                                                      
         MVC   0(3,R1),SPACES                                                   
VRQ065   MVC   APBYTE,CULANG                                                    
         OI    APBYTE,X'20'                                                     
         GOTO1 VPERVAL,APPARM,(FVILEN,FVIFLD),(APBYTE,SCWORK)                   
         CLI   4(R1),X'04'                                                      
         BNE   *+10                                                             
         MVC   SCWORK+36(2),SCWORK+34                                           
         NI    4(R1),(X'FF'-X'04') ONE DATE IS OK                               
         CLI   4(R1),X'00'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFINVDT) INVALID DATE                              
         B     VALREQX                                                          
*                                                                               
         MVC   SELPSTA,SCWORK+34                                                
         CLC   SELPEND,FFS                                                      
         BE    *+10                                                             
         MVC   SELPEND,SCWORK+36                                                
*                                                                               
VRQ069   MVC   APWORK+0(2),SELPSTA                                              
         OC    SELPSTA,SELPSTA      IF START DATE IS ZERO                       
         BNZ   VRQ069A                                                          
         MVC   APWORK+0(2),ASCDAT   SET START TO 1JAN THIS YEAR                 
         NC    APWORK+0(2),=X'FE00'                                             
         OC    APWORK+0(2),=X'0021'                                             
VRQ069A  MVC   APWORK+2(2),ASCDAT                                               
         CLC   SELPEND,FFS         IF UFN USE TODAYS DATE                       
         BE    *+10                                                             
         MVC   APWORK+2(2),SELPEND                                              
         MVC   SELPSTA,APWORK                                                   
         MVC   SELPEND,APWORK+2                                                 
         GOTO1 VDATCON,DMCB,(2,APWORK+0),(0,APWORK+4)                           
         GOTO1 VDATCON,DMCB,(2,APWORK+2),(0,APWORK+10)                          
         GOTO1 VPERVERT,DMCB,APWORK+4,APWORK+10                                 
         LH    R0,8(R1)            R1 = NUMBER OF DAYS INCLUSIVE                
*                                                                               
         CLI   SELSYS,0                                                         
         BE    VRQ069B                                                          
         CLI   SELSYS,GEKSFTQ                                                   
         BE    VRQ069B                                                          
         CLI   SELSYS,GEKSDEQ                                                   
         BNE   VRQ070                                                           
*                                                                               
VRQ069B  CHI   R0,8                                                             
         BL    VRQ070                                                           
         OC    SELCURF,SELCURF                                                  
         BZ    ERRMX1W                                                          
         OC    SELCURT,SELCURT                                                  
         BZ    ERRMX1W                                                          
         CHI   R0,366                                                           
         BH    ERRMX1Y                                                          
*                                                                               
VRQ070   CLI   CULANG,2                                                         
         BNH   VRQ070A                                                          
         GOTO1 VDATCON,APPARM,(2,SELPSTA),(8,REPPER)                            
         OI    REPPER,X'F0'                                                     
         MVI   REPPER+8,C'-'                                                    
         MVC   REPPER+9(8),CT@UFN                                               
         CLC   SELPEND,=X'FFFF'                                                 
         MVI   REPPERH+5,12                                                     
         BE    VRQ080                                                           
         GOTO1 VDATCON,APPARM,(2,SELPEND),(8,REPPER+9)                          
         OI    REPPER+9,X'F0'                                                   
         OI    REPPERH+6,X'80'                                                  
         MVI   REPPERH+5,17                                                     
         B     VRQ080                                                           
*                                                                               
VRQ070A  GOTO1 VDATCON,APPARM,(2,SELPSTA),(8,REPPER)                            
         OI    REPPER,X'F0'                                                     
         MVI   REPPER+7,C'-'                                                    
         MVC   REPPER+8(8),CT@UFN                                               
         CLC   SELPEND,=X'FFFF'                                                 
         MVI   REPPERH+5,11                                                     
         BE    VRQ080                                                           
         GOTO1 VDATCON,APPARM,(2,SELPEND),(8,REPPER+8)                          
         OI    REPPER+8,X'F0'                                                   
         OI    REPPERH+6,X'80'                                                  
         MVI   REPPERH+5,15                                                     
*                                                                               
VRQ080   LA    R2,APRECKEY                                                      
         MVI   GEKREC,GEKRECQ                                                   
         MVC   GEKAGY,SELAGY                                                    
         MVC   GEKSYS,SELSYS                                                    
         MVC   GEKCTYP,SELTYPE                                                  
         MVC   GEKKEY,SELKKEY                                                   
         MVC   GEKCURF,SELCURF                                                  
         MVC   GEKCURT,SELCURT                                                  
         MVC   GEKPEND,SELPSTA                                                  
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE EXCHANGE REPORT                                 *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
         L     R2,AIOAREA1                                                      
         LA    R4,RECBUFF                                                       
         MVC   IOKEY,APRECKEY      SET INITIAL KEY VALUE                        
*                                                                               
         BAS   RE,BOXHEAD          START BOX                                    
         GOTO1 VREPORT,REPD                                                     
         MVI   APFLAG,0                                                         
         B     PRTRDHI             THEN READ HI                                 
*                                                                               
PRTRNXT  TM    APFLAG,X'80'                                                     
         BNO   PRTRSEQ                                                          
         OC    GEKEY,GEKEY                                                      
         BZ    PRTREPXX                                                         
         XC    0(L'GEKEY,R2),0(R2)                                              
         B     PRBUFF                                                           
*                                                                               
PRTRDHI  LA    R1,IOHI+IOGENDIR+IO1                                             
         B     *+8                                                              
PRTRSEQ  LA    R1,IOSQ+IOGENDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         CLI   GEKREC,GEKRECQ      TEST STILL A EXCHANGE RECORD                 
         BNE   PRTREPX                                                          
*                                                                               
         CLI   GEKSYS,GEKSFTQ      ARE WE ON FT RECORDS                         
         BE    PR001                                                            
         CLI   GEKSYS,GEKSDEQ      ARE WE ON GERMAN FT RECORDS                  
         BNE   PR005                                                            
PR001    OI    APFLAG,X'01'        SET FLAG FOR FT RECS                         
         B     PR020                                                            
*                                                                               
PR005    TM    APFLAG,X'01'        HAVE WE DONE FT RECS                         
         BO    PRTREPX             YES SO THATS IT                              
         OC    SELAGY,SELAGY       DO WE HAVE AGENCY FILTER                     
         BZ    PR010                                                            
         CLC   GEKAGY,SELAGY       TEST SAME AGENCY                             
         BE    PR020                                                            
*                                                                               
         CLI   SELSYS,0            DO WE WANT ALL SYSTEMS                       
         BNE   PRTREPX                                                          
*                                                                               
         XC    GEKAGY(GEKPSTA-GEKAGY),GEKAGY  CLEAR AGENCY                      
*        MVI   GEKSYS,GEKSFTQ                                                   
         MVI   GEKSYS,GEKSDEQ      FE LOWER THAN FF                             
         MVC   GEKCURF,SELCURF                                                  
         MVC   GEKCURT,SELCURT                                                  
         MVC   GEKPEND,SELPSTA                                                  
         B     PRTRDHI             READ HI FOR FT RECORDS                       
*                                                                               
PR010    CLI   SELSYS,0            DO WE WANT ALL SYSTEMS                       
         BNE   PRTREPX                                                          
         MVC   SELAGY,TWAAGY       OK SET AGENCY NOW                            
         CLC   GEKAGY,SELAGY                                                    
         BE    PR020               IS IT OUR AGENCY (PROBABLY NOT)              
         MVC   GEKAGY,SELAGY                                                    
         XC    GEKSYS(GEKPSTA-GEKSYS),GEKSYS                                    
         B     PRTRDHI             READ HI FOR OUR AGENCY RECORDS               
*                                                                               
PR020    CLI   SELSYS,0            ALL SYSTEMS                                  
         BE    PR030                                                            
         CLC   GEKSYS,SELSYS       MATCH ON SYSTEM                              
         BNE   PRTREPX                                                          
*                                                                               
PR030    OC    SELCURF,SELCURF     FROM ALL CURRENCIES                          
         BZ    PR040                                                            
         CLC   GEKCURF,SELCURF     IS THIS OUR CURRENCY                         
         BE    PR040                                                            
         CLI   GEKSYS,GEKSFTQ                                                   
         BE    PRTREPX                                                          
         CLI   GEKSYS,GEKSDEQ                                                   
         BE    PRTREPX                                                          
         CLI   SELSYS,0                                                         
         BNE   PRTREPX             END OF REPORT IF SYSTEM IS SPECIFIC          
         SR    R1,R1                                                            
         ICM   R1,7,GEKCURF                                                     
         LA    R1,1(R1)            ELSE BUMP FROM CURRENCY                      
         STCM  R1,7,GEKCURF                                                     
         XC    GEKCURT(GEDSTAT-GEKCURT),GEKCURT                                 
         B     PRTRDHI                                                          
*                                                                               
PR040    OC    SELCURT,SELCURT     TO ALL CURRENCIES                            
         BZ    PR050                                                            
         CLC   GEKCURT,SELCURT     IS THIS OUR CURRENCY                         
         BE    PR050                                                            
         OC    SELCURF,SELCURF     ALL CURRENCIES FROM                          
         BZ    PR045                                                            
         CLI   GEKSYS,GEKSFTQ                                                   
         BE    PRTREPX                                                          
         CLI   GEKSYS,GEKSDEQ                                                   
         BE    PRTREPX                                                          
         CLI   SELSYS,0            TEST ALL SYSTEMS                             
         BNE   PRTREPX                                                          
PR045    SR    R1,R1                                                            
         ICM   R1,7,GEKCURT                                                     
         LA    R1,1(R1)            BUMP TO CURRENCY                             
         STCM  R1,7,GEKCURT                                                     
         XC    GEKCTYP(GEDSTAT-GEKCTYP),GEKCTYP                                 
         B     PRTRDHI                                                          
*                                                                               
PR050    OC    SELTYPE,SELTYPE     ALL TYPES ?                                  
         BZ    PR060                                                            
         CLC   GEKCTYP,SELTYPE     IS THIS OUR TYPE                             
         BE    PR060                                                            
         SR    R1,R1                                                            
         IC    R1,GEKCTYP                                                       
         LA    R1,1(R1)            BUMP TYPE                                    
         STC   R1,GEKCTYP                                                       
         XC    GEKKEY(GEDSTAT-GEKKEY),GEKKEY                                    
         B     PRTRDHI                                                          
         EJECT                                                                  
PR060    CLI   GEKSYS,MEDQ         MEDIA                                        
         BE    PRTMED                                                           
         CLI   GEKSYS,ACCQ         ACCOUNTING                                   
         BE    PRTACC                                                           
         CLI   GEKSYS,GEKSFTQ      NO FILTER FOR FINSTAT                        
         BE    PR070                                                            
         CLI   GEKSYS,GEKSDEQ      NO FILTER FOR GFTSTAT                        
         BE    PR070                                                            
************************************************                                
*      MEDIA SYSTEM KEY FILTERS                *                                
************************************************                                
         SPACE 1                                                                
PRTMED   CLC   GEKCLI,SELCLI                                                    
         BE    PR070                                                            
         OC    SELCLI,SELCLI                                                    
         BZ    PR070                                                            
         B     PRTRSEQ                                                          
************************************************                                
*      ACCOUNTING SYSTEM KEY FILTERS           *                                
************************************************                                
         SPACE 1                                                                
PRTACC   CLC   GEKACT,SELACT                                                    
         BE    PR070                                                            
         OC    SELACT,SELACT                                                    
         BZ    PR070                                                            
         B     PRTRSEQ                                                          
         EJECT                                                                  
PR070    CLC   GEKPEND,SELPSTA     IS END BEFORE MY START                       
         BNL   PR071                                                            
         MVC   GEKPEND,SELPSTA     USE MY START AND READ HI                     
         XC    GEKPSTA,GEKPSTA                                                  
         B     PRTRDHI                                                          
PR071    CLC   GEKPSTA,SELPEND     IS START AFTER MY END                        
         BNH   PR072                                                            
         B     PRTRSEQ                                                          
*                                                                               
PR072    GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
*                                                                               
PRBUFF   L     R2,AIOAREA1                                                      
         MVC   0(GEFIRST+GEXELLQ+GIXELLQ,R4),0(R2)                              
         LA    R4,GEFIRST+GEXELLQ+GIXELLQ(R4)                                   
         LA    R1,RECBUFFX                                                      
         CR    R1,R4                                                            
         BNE   *+8                                                              
         LA    R4,RECBUFF                                                       
         MVC   0(GEFIRST+GEXELLQ+GIXELLQ,R2),0(R4)                              
         OC    GEKEY,GEKEY                                                      
         BZ    PRTRNXT                                                          
*                                                                               
         OI    APFLAG,X'40'                                                     
         CLC   REPKEYS,GEKEY       SAME TYPE OF RATE AS BEFORE ?                
         BE    PR075                                                            
         OC    REPKEYS,REPKEYS     OR FIRST RATE                                
         BZ    PR075                                                            
         GOTO1 BOXMID,PRTLIN       SPLIT BOX                                    
         LR    R1,R4                                                            
         SR    RF,RF               COUNT NUM OF LINES TO NEXT SPLIT             
PRTCNT   CHI   RF,10               10 MAX                                       
         BE    PRTCNT1                                                          
         CLC   GEKEY(L'REPKEYS),0(R1)                                           
         BNE   PRTCNT1                                                          
         LA    R1,GEFIRST+GEXELLQ+GIXELLQ(R1)                                   
         LA    R0,RECBUFFX                                                      
         CR    R0,R1                                                            
         BNE   *+8                                                              
         LA    R1,RECBUFF                                                       
         LA    RF,1(RF)                                                         
         B     PRTCNT                                                           
*                                                                               
PRTCNT1  ZIC   R1,REPLINE                                                       
         LA    R1,2(R1,RF)         WILL IT FIT                                  
         STC   R1,APBYTE                                                        
         CLC   APBYTE,REPMAXL                                                   
         BL    PR074                                                            
         GOTO1 BOXBTM,PRTLIN       CLOSE IF BOTTOM OF PAGE                      
         GOTO1 VREPORT,REPD                                                     
         OI    REPHEADI,REPHFRCE                                                
         BAS   RE,BOXHEAD                                                       
         GOTO1 VREPORT,REPD                                                     
         B     PR075                                                            
PR074    GOTO1 VREPORT,REPD                                                     
*                                                                               
PR075    MVC   REPKEYS,GEKEY       SAVE KEY VALUES UP TO PERIOD                 
         GOTO1 BOXLIN,PRTLIN                                                    
*                                                                               
         CLI   GEKSYS,GEKSFTQ      DISPLAY FT                                   
         BNE   *+14                                                             
         MVC   PRTSYS(7),FINSTAT                                                
         B     PR080                                                            
         CLI   GEKSYS,GEKSDEQ      DISPLAY GERMAN FT                            
         BNE   *+14                                                             
         MVC   PRTSYS(7),GFTSTAT                                                
         B     PR080                                                            
         GOTO1 ADISSYS,GEKSYS      GET SYSTEM NAME                              
         MVC   PRTSYS,APWORK                                                    
*                                                                               
PR080    CLC   GEKCURF,FFS         FROM ALL                                     
         BNE   *+14                                                             
         MVC   PRTCURF,CT@ALL                                                   
         B     *+10                                                             
         MVC   PRTCURF,GEKCURF     CURRENCY FROM                                
         MVC   PRTCURT,GEKCURT     CURRENCY TO                                  
*                                                                               
         L     R1,ATYPTAB          FIND TYPE IN TYPTAB                          
PR090    CLC   4(1,R1),GEKCTYP                                                  
         BE    PR095                                                            
         LA    R1,6(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   PR090                                                            
         DC    H'0'                TABLE ENTRY MUST EXIST                       
PR095    XC    APWORK(10),APWORK                                                
         EX    0,0(R1)             LOCATE DICTIONARY ENTRY                      
         CLI   0(RF),C'/'          DON'T BOTHER WITH THIS ONE                   
         BE    *+10                                                             
         MVC   PRTTYPE(10),0(RF)                                                
*                                                                               
         CLI   GEKSYS,MEDQ                                                      
         BNE   PR095A                                                           
         MVC   PRTCLIN(10),CT@DFALT                                             
         CLC   GEKCLI,FFS                                                       
         BE    PR096                                                            
         MVC   PRTCLIN,SPACES                                                   
         BAS   RE,DISPCLI          PRINT CLIENT CODE                            
         MVC   PRTCLI,APWORK                                                    
         MVC   GCLICODE(L'GEKCLI),GEKCLI                                        
         BAS   RE,MEDCLI           PRINT CLIENT SHORT NAME                      
         MVC   PRTCLIN,GCLINAME                                                 
         B     PR096                                                            
*                                                                               
PR095A   CLI   GEKSYS,ACCQ                                                      
         BNE   PR096                                                            
         MVC   PRTCLIN(10),CT@DFALT                                             
         CLC   GEKACT,FFS                                                       
         BE    PR096                                                            
         MVC   PRTCLIN,SPACES                                                   
         MVC   PRTCLI,GEKACT                                                    
         MVC   GCLICODE(L'GEKACT),GEKACT                                        
         MVI   GCLILEN,0                                                        
         BAS   RE,ACCCLI                                                        
         MVC   PRTCLIN(L'GCLINAME),GCLINAME                                     
*                                                                               
PR096    CLI   CUCTRY,CTRYGER                                                   
         BE    PR096A                                                           
         CLI   CUCTRY,CTRYHOL                                                   
         BNE   PR096B                                                           
*                                                                               
PR096A   MVI   APELEM,GIXELQ                                                    
         L     R1,AIOAREA1                                                      
         GOTO1 AGETELS             LOOK FOR A RATE INPUT ELEMENT                
         OC    APPARM(4),APPARM                                                 
         BZ    PR096B              NOT FOUND - DO EDITING AS NORMAL             
         L     R3,APPARM                                                        
         USING GIXEL,R3                                                         
         LA    R1,GIXFROM          EDIT FROM CURRENCY AS IT WAS INPUT           
         BAS   RE,EDITNUM                                                       
         MVC   PRTRATF,FVOMSG                                                   
         LA    R1,GIXTO            EDIT 'TO' CURRENCY AS IT WAS INPUT           
         BAS   RE,EDITNUM                                                       
         MVC   PRTRATT,FVOMSG                                                   
         B     PR097                                                            
         DROP  R3                                                               
*                                                                               
PR096B   LA    R1,GEXRATE                                                       
         BAS   RE,EDITNUM                                                       
         TM    GEXFLAG,GEXFTPL+GEXFTMI                                          
         BNZ   PR096C                                                           
         TM    GESTAT,GEINVRT                                                   
         BO    PR096B1                                                          
         MVC   PRTRATT,FVOMSG                                                   
*                                                                               
         CLI   GEKSYS,MEDQ                                                      
         BNE   PR096B0                                                          
*                                                                               
         CLI   CUCTRY,CTRYGER                                                   
         BNE   PR096B0                                                          
*                                                                               
         TM    GESTAT,GETENQ                                                    
         BNO   *+14                                                             
         MVC   PRTRATF(4),TENVAL                                                
         B     PR097                                                            
*                                                                               
         TM    GESTAT,GEHUNQ                                                    
         BNO   *+14                                                             
         MVC   PRTRATF(5),HUNVAL                                                
         B     PR097                                                            
*                                                                               
         TM    GESTAT,GETHOUQ                                                   
         BNO   *+14                                                             
         MVC   PRTRATF(6),THOUVAL                                               
         B     PR097                                                            
*                                                                               
PR096B0  MVC   PRTRATF(3),ONEVAL                                                
         B     PR097                                                            
*                                                                               
PR096B1  MVC   PRTRATF,FVOMSG                                                   
         CLI   GEKSYS,MEDQ                                                      
         BNE   PR096B2                                                          
*                                                                               
         CLI   CUCTRY,CTRYGER                                                   
         BNE   PR096B2                                                          
*                                                                               
         TM    GESTAT,GETENQ                                                    
         BNO   *+14                                                             
         MVC   PRTRATT(4),TENVAL                                                
         B     PR097                                                            
*                                                                               
         TM    GESTAT,GEHUNQ                                                    
         BNO   *+14                                                             
         MVC   PRTRATT(5),HUNVAL                                                
         B     PR097                                                            
*                                                                               
         TM    GESTAT,GETHOUQ                                                   
         BNO   *+14                                                             
         MVC   PRTRATT(6),THOUVAL                                               
         B     PR097                                                            
*                                                                               
PR096B2  MVC   PRTRATT(3),ONEVAL                                                
         B     PR097                                                            
*                                                                               
PR096C   MVC   PRTRATF(3),=C'FT+'                                               
         TM    GEXFLAG,GEXFTMI                                                  
         BNO   *+8                                                              
         MVI   PRTRATF+2,C'-'                                                   
*                                                                               
         TM    GEXFLAG,GEXFTPC                                                  
         BNO   *+8                                                              
         MVI   1(R1),C'%'                                                       
         MVC   PRTRATF+3(7),FVOMSG                                              
*                                                                               
PR097    CLI   CULANG,2                                                         
         BNH   PR079A                                                           
         GOTO1 VDATCON,APPARM,(2,GEKPSTA),(8,PRTPER)                            
         OI    PRTPER,X'F0'                                                     
         MVI   PRTPER+8,C'-'                                                    
         MVC   PRTPER+9(8),CT@UFN                                               
         CLC   GEKPEND,=X'FFFF'                                                 
         BE    PR100                                                            
         GOTO1 VDATCON,APPARM,(2,GEKPEND),(8,PRTPER+9)                          
         OI    PRTPER+9,X'F0'                                                   
*                                                                               
PR079A   GOTO1 VDATCON,APPARM,(2,GEKPSTA),(8,PRTPER)                            
         OI    PRTPER,X'F0'                                                     
         MVI   PRTPER+7,C'-'                                                    
         MVC   PRTPER+8(8),CT@UFN                                               
         CLC   GEKPEND,=X'FFFF'                                                 
         BE    PR100                                                            
         GOTO1 VDATCON,APPARM,(2,GEKPEND),(8,PRTPER+8)                          
         OI    PRTPER+8,X'F0'                                                   
*                                                                               
PR100    GOTO1 VREPORT,REPD                                                     
         B     PRTRNXT                                                          
*                                                                               
PRTREPX  OI    APFLAG,X'80'                                                     
         TM    APFLAG,X'40'        IF BUFFER NOT EVEN FULL                      
         BO    PRTRNXT                                                          
         LA    R4,RECBUFL          POINT TO LAST BUFFER ENTRY                   
         B     PRTRNXT                                                          
*                                                                               
PRTREPXX GOTO1 BOXBTM,PRTLIN       CLOSE FINAL BOX                              
         GOTO1 VREPORT,REPD                                                     
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*   ROUTINE TO CHECK FOR OVERLAPING DATE PERIODS BEFORE     *                   
*   UPDATIVE ACTIONS AND TO TRUNCATE ANY UFN RECORDS THAT   *                   
*   OVERLAP.                                                *                   
*   ON ENTRY IOAREA1 IS ASSUMED TO HOLD THE RECORD          *                   
*   TO BE ADDED OR RESTORED AND IOKEY THE KEY.              *                   
*                                                           *                   
*   EXIT CC EQU  MEANS OK TO ADD OR RESTORE                 *                   
*   EXIT CC NEQ  MEANS DATE PERIODS OVERLAP                 *                   
*************************************************************                   
         SPACE 1                                                                
CHECKUP  NTR1                                                                   
         LA    R2,IOKEY                                                         
         MVC   KEYSAVE,IOKEY                                                    
         GOTO1 AIO,IORD+IOGENDIR+IO2                                            
         BE    ECMSG14                                                          
         MVC   IOKEY,KEYSAVE                                                    
         MVC   GEKPEND,GEKPSTA     MOVE START DATE TO END                       
         XC    GEKPSTA,GEKPSTA                                                  
         LA    R1,IOGENDIR+IOHI+IO2     READ HI                                 
         GOTO1 AIO                                                              
         BNE   CHKUPX                                                           
*                                                                               
         CLC   KEYSAVE(GEKPEND-GEKEY),GEKEY                                     
         BNE   CHKUPY              NOTHING FOUND SO OK TO ADD                   
*                                                                               
         CLC   GEKPEND,KEYSAVE+GEKPSTA-GEKEY                                    
         BL    CHKUPY                                                           
         CLC   GEKPSTA,KEYSAVE+GEKPEND-GEKEY                                    
         BH    CHKUPY                                                           
*                                                                               
         CLC   GEKPEND,FFS                                                      
         BNE   CHKUPX              END DATE MUST BE UFN                         
         CLC   GEKPSTA,KEYSAVE+GEKPSTA-GEKEY                                    
         BNL   CHKUPX              START MUST BE LESS THAN OURS                 
*                                                                               
         LA    R1,IORD+IOGENDIR+IO2+IOLOCK                                      
         GOTO1 AIO                                                              
         BZ    *+6                                                              
         DC    H'0'                I/O ERROR                                    
*                                                                               
         LA    R1,IOGET+IOGENFIL+IO2+IOLOCK                                     
         GOTO1 AIO                 GET RECORD                                   
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
*                                                                               
         OI    GEDSTAT,X'80'       DELETE UFN REC IN DIRECTORY                  
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         GOTO1 ASETACT,GEXCD                                                    
         OI    GESTAT,X'80'        DELETE UFN RECORD                            
         GOTO1 AIO,IOPUT+IOGENFIL+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    GESTAT,255-X'80'    CLEAR DELETE FLAG                            
         GOTO1 VDATCON,DMCB,(2,KEYSAVE+GEKPSTA-GEKEY),(0,APDUB)                 
         GOTO1 VADDAY,DMCB,APDUB,DUB,X'FFFFFFFF'                                
         GOTO1 VDATCON,DMCB,(0,DUB),(2,GEKPEND)                                 
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO2                                            
         GOTO1 AIO                 REPLACE UFN WITH NEW REC                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKUPY   CR    RB,RB               OK TO ADD / RESTORE                          
         MVC   IOKEY,KEYSAVE                                                    
         MVC   APRECKEY,KEYSAVE                                                 
         B     EXIT                                                             
CHKUPX   LTR   RB,RB               ERROR DATES OVERLAP                          
         MVC   IOKEY,KEYSAVE                                                    
         MVC   APRECKEY,KEYSAVE                                                 
         B     EXIT                                                             
         SPACE 1                                                                
*************************************************************                   
*   ROUTINE TO CHECK FOR EURO CURRENCY                      *                   
*   EXIT CC EQU  MEANS OK                                   *                   
*   EXIT CC NEQ  MEANS NOT ALLOWED                          *                   
*************************************************************                   
         SPACE 1                                                                
CHECKEUR NTR1                                                                   
         LA    R2,IOKEY            SCAN EUROTAB FOR CURRENCY                    
         CLI   GEKSYS,GEKSFTQ                                                   
         BE    CHKEUY              FT RATES OK                                  
         CLI   GEKSYS,GEKSDEQ                                                   
         BE    CHKEUY              GERMAN FT RATES OK                           
*                                                                               
         CLC   GEKCURF,=C'EUR'     TEST FOR EURO                                
         BE    *+14                                                             
         CLC   GEKCURT,=C'EUR'                                                  
         BNE   CHKEU05                                                          
*                                                                               
         CLC   GEKPSTA,=X'C621'    START DATE MUST BE > CUT OFF                 
         BL    CHKEUD                                                           
         CLC   GEKPEND,=X'C621'    END DATE MUST BE > CUT OFF                   
         BL    CHKEUD                                                           
         B     CHKEUY              FT RATES OK                                  
*                                                                               
CHKEU05  L     R1,=A(EUROTAB)                                                   
         A     R1,APRELO                                                        
CHKEU10  CLC   GEKCURF,0(R1)                                                    
         BE    CHKEU20                                                          
         CLC   GEKCURT,0(R1)                                                    
         BE    CHKEU20                                                          
         LA    R1,5(R1)                                                         
         CLI   0(R1),X'FF'         CHECK FOR EOT                                
         BNE   CHKEU10                                                          
         B     CHKEUY              EOT OK                                       
*                                                                               
CHKEU20  MVC   HALF1,3(R1)         SET DATE                                     
         CLC   HALF1,=X'C621'      TEST FOR 01 JAN 99                           
         BNE   CHKEU30                                                          
**       L     R1,ASYSFACS         NOT REQUIRED ANYMORE                         
**       L     R1,VSSB-SYSFACD(R1)                                              
**       CLC   SSBSYSNA-SSBD(3,R1),=C'Y2K'                                      
**       BNE   *+10                                                             
**       MVC   HALF1,=X'C821'      01 JAN 00 FOR Y2K SYSTEM                     
*                                                                               
CHKEU30  CLC   GEKPSTA,HALF1       START DATE MUST NOT BE > CUT OFF             
         BNL   CHKEUX                                                           
         CLC   GEKPEND,FFS         END DATE OF UFN IS OK                        
         BE    CHKEUY                                                           
         CLC   GEKPEND,HALF1       END DATE MUST NOT BE > CUT OFF               
         BNL   CHKEUX                                                           
*                                                                               
CHKEUY   CR    RB,RB               OK                                           
         B     EXIT                                                             
CHKEUX   SR    R1,R1                                                            
         CR    RB,R1               ERROR EURO CURRENCY NOT ALLOWED              
         B     EXIT                                                             
CHKEUD   SR    R1,R1                                                            
         CR    R1,RB               EURO DATE NOT ALLOWED                        
         B     EXIT                                                             
         SPACE 1                                                                
**********************************************************************          
* FIND WHICH MEDIA AND ACC SYSTEM THIS USER BELONGS TO               *          
* SAVE USEFULL INFO                                                  *          
**********************************************************************          
         SPACE 1                                                                
GETSYS   NTR1                                                                   
         MVC   APWORK(L'IOKEY),IOKEY                                            
         LA    R2,IOKEY                                                         
         USING CT5REC,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE SYSTEM ACCESS                    
         MVC   CT5KALPH,TWAAGY     AGENCY ID                                    
         LA    R1,IORD+IOCONFIL+IO1                                             
         GOTO1 AIO                 READ ACCESS RECORD                           
         BNE   GETSYSX                                                          
         L     R2,AIOAREA1                                                      
         LA    R3,CT5DATA                                                       
GETSYS1  CLI   0(R3),X'21'                                                      
         BNE   GETSYSN                                                          
         CLI   CTSYSNUM-CTSYSD(R3),MEDQ                                         
         BE    GETSYS2             MEDIA ELEMENT FOUND                          
         CLI   CTSYSNUM-CTSYSD(R3),ACCQ                                         
         BE    GETSYS3             ACC ELEMENT FOUND                            
*                                                                               
GETSYSN  SR    R0,R0                                                            
         ICM   R0,1,1(R3)                                                       
         BZ    GETSYSX                                                          
         AR    R3,R0                                                            
         B     GETSYS1                                                          
*                                                                               
GETSYS2  MVC   MEDSE,CTSYSSE-CTSYSD(R3)                                         
         MVC   MEDAGY,CTSYSAGB-CTSYSD(R3)                                       
         CLI   ASONOFF,ASON                                                     
         BE    GETSYSN                                                          
*                                                                               
         L     RF,SCAUTL           SWITCH TO MEDIA                              
         MVC   4(1,RF),MEDSE                                                    
         GOTO1 VDMGR,DMCB,=C'DMOPEN',=C'MEDIA',MFLIST,(R2)                      
         L     RF,SCAUTL                                                        
         MVI   4(RF),X'0A'         SWITCH BACK TO CONTROL                       
         B     GETSYSN                                                          
*                                                                               
GETSYS3  MVC   ACCSE,CTSYSSE-CTSYSD(R3)                                         
         MVC   ACCAGY,CTSYSAGB-CTSYSD(R3)                                       
         CLI   ASONOFF,ASON                                                     
         BE    GETSYSN                                                          
*                                                                               
         L     RF,SCAUTL           SWITCH TO ACC                                
         MVC   4(1,RF),ACCSE                                                    
         GOTO1 VDMGR,DMCB,=C'DMOPEN',=C'ACCOUNT',AFLIST,(R2)                    
         L     RF,SCAUTL                                                        
         MVI   4(RF),X'0A'         SWITCH BACK TO CONTROL                       
         B     GETSYSN                                                          
*                                                                               
GETSYSX  MVC   IOKEY,APWORK        RESTORE OLD KEY VALUE                        
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*        GET CLIENT CODE FROM GEKCLI AND MOVE TO 7 CHRS AT APWORK    *          
**********************************************************************          
         SPACE 1                                                                
         USING GEXCD,R2                                                         
DISPCLI  MVC   APWORK(7),SPACES                                                 
         MVC   APWORK(3),GEKCLI                                                 
         CLI   GEKCLI+1,X'40'      TEST FOR ANNNN CLIENT CODE                   
         BNL   DISPCLIX                                                         
         SR    R1,R1                                                            
         ICM   R1,3,GEKCLI+1                                                    
         CVD   R1,APDUB                                                         
         UNPK  APWORK+1(4),APDUB                                                
         OI    APWORK+4,X'F0'                                                   
DISPCLIX BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*   ROUTINE TO VALIDATE NUMERIC                             *                   
*   INPUT IN THE FORM 99999.99999                           *                   
*   ENTRY R1=MAX NUMBER OF SIGNIFICANT DIGITS               *                   
*   EXIT CC=EQU OK / CC NEQ=ERROR -(MESSAGE SET FOR EXIT)   *                   
*   R1=NUMBER OF DECIMAL PLACES                             *                   
*   APFULL=ABSOLUTE BINARY VALUE DECIMAL POINT IGNORED      *                   
*   APDUB=PACKED 99999999999.99999 VALUE                    *                   
*************************************************************                   
         SPACE 1                                                                
VALNUM   NTR1                                                                   
         ST    R1,APFULL                                                        
         SR    RE,RE                                                            
         SR    R1,R1                                                            
         IC    R1,FVXLEN           GET INPUT LEN - 1                            
         LA    RF,FVIFLD                                                        
VALN01   CLC   0(1,RF),CURSEP      SCAN FOR '.' OR ','                          
         BE    VALN02                                                           
         CLI   0(RF),C' '          OR FIRST SPACE                               
         BE    VALN01A                                                          
         TM    0(RF),X'F0'         ALL ELSE MUST BE NUMERIC                     
         BNO   VALNOTN                                                          
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)            COUNT SIGNIFICANT DIGITS                     
         B     VALN01                                                           
*                                                                               
VALN01A  LA    R1,1(R1)            IF NO '.' USE REAL LENGTH                    
*                                                                               
VALN02   STC   R1,APBYTE           SAVE LEN IN APBYTE                           
         C     RE,APFULL           CHECK MAX SIGNIFICANT DIGITS                 
         BH    VALNTOG                                                          
         SR    R1,RE               R1=NUM OF DEC PLACES                         
         STH   R1,APHALF                                                        
         BZ    VALN02A                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(RF)       SQUASH OUT DEC POINT                         
         LA    R1,1(R1)                                                         
VALN02A  LA    RE,5                                                             
         SR    RE,R1               CALCULATE                                    
         BM    VALNDCP                                                          
         SLL   RE,2                SHIFT VALUE                                  
*                                                                               
         LA    RF,FVIFLD                                                        
         IC    R1,APBYTE                                                        
         BCTR  R1,0                                                             
         EX    R1,*+16                                                          
         EX    R1,*+18                                                          
         EX    R1,*+20                                                          
         B     *+22                                                             
         MVC   APWORK(0),0(RF)                                                  
         NC    APWORK(0),NUMERIC                                                
         CLC   APWORK(0),NUMERIC   CHECK NUMERIC                                
         BNE   VALNOTN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB(8),0(0,RF)    PACK NUMBER                                  
         CP    APDUB(8),MAXNUM                                                  
         BH    VALNDCP             ABSOLUTE VALUE TOO LARGE TO HANDLE           
         CVB   R1,APDUB                                                         
         ST    R1,APFULL                                                        
         LM    R0,R1,APDUB         LOAD INTO R0,R1                              
         SRDL  R0,4                LOSE SIGN BITS                               
         SLDL  R0,0(RE)            SHIFT CORRECT AMOUNT                         
         STM   R0,R1,APDUB         AND STORE BACK                               
         LH    R1,APHALF                                                        
         CR    RB,RB                                                            
         B     VALNX                                                            
*                                                                               
VALNOTN  MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALNERR                                                          
VALNTOG  B     ERRFMAX                                                          
VALNDCP  B     ERRDECP                                                          
VALNERR  LTR   RB,RB                                                            
*                                                                               
VALNX    XIT1  REGS=(R1)                                                        
         SPACE 1                                                                
*************************************************************                   
*  ROUTINE TO OUTPUT NUMBERS IN  99999.99999 FORMAT         *                   
*************************************************************                   
         SPACE 1                                                                
EDITNUM  ST    RE,APFULL                                                        
         XC    APDUB,APDUB                                                      
         MVC   APDUB+2(6),0(R1)                                                 
         MVC   FVOMSG(12),=X'4020202021204B2020202020'                          
         MVC   FVOMSG+6(1),CURSEP                                               
         ED    FVOMSG(12),APDUB+2                                               
         LA    R1,FVOMSG+11                                                     
EDITN1   LA    RF,FVOMSG                                                        
         CR    R1,RF                                                            
         BNH   EDITN2                                                           
         CLC   0(1,R1),CURSEP      SEPARATOR                                    
         BE    EDITN2                                                           
         CLI   0(R1),C'0'                                                       
         BNE   EDITN3                                                           
         MVI   0(R1),C' '                                                       
         BCT   R1,EDITN1                                                        
EDITN2   MVI   1(R1),C'0'                                                       
         LA    R1,1(R1)                                                         
EDITN3   LA    R0,12                                                            
         CLI   FVOMSG,C' '                                                      
         BNE   *+20                                                             
         MVC   FVOMSG(11),FVOMSG+1                                              
         MVI   FVOMSG+11,C' '                                                   
         BCTR  R1,0                                                             
         BCT   R0,*-20                                                          
EDNMX    L     RE,APFULL                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* TEST CODE IN FVIFLD IS A VALID 3 CHR CURRENCY CODE                 *          
* EXIT CC=EQU OK  CC=NEQ INVALID CURRENCY                            *          
**********************************************************************          
         SPACE 1                                                                
VALCUR   NTR1                                                                   
         MVC   KEYSAVE,IOKEY                                                    
         LA    R2,IOKEY                                                         
         USING GCURD,R2                                                         
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ      RECORD TYPE CURRENCY                         
         MVC   GCKCURR,FVIFLD                                                   
         CLC   GCKCURR(3),FFS                                                   
         BE    VALCURD                                                          
         LA    R1,IORDD+IOGENDIR+IO2                                            
         GOTO1 AIO                 READ CURRENCY RECORD                         
         BNE   VALCURN                                                          
         GOTO1 AIO,IOGENFIL+IOGET+IO2                                           
         BE    VALCURX                                                          
         DC    H'0'                RECORD MUST EXIST                            
VALCURN  B     ERRCURR             CURRENCY NOT FOUND                           
*                                                                               
VALCURD  L     R1,AIOAREA2                                                      
         MVC   0(L'ALLCURN,R1),ALLCURN                                          
*                                                                               
VALCURX  CLC   FVIFLD(3),=C'BEL'   DISABLE BELGIAN FIN FRANK                    
         BNE   VALCURXX                                                         
         CLI   APACTN,ACTDIS       FOR ALL BUT DISPLAY                          
         BNE   VALCURN                                                          
*                                                                               
VALCURXX CR    RB,RB                                                            
         MVC   IOKEY,KEYSAVE       RESTORE OLD KEY VALUE                        
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* CALCULATE SHIFT VALUE FROM DEC PLACES IN CURRENCY RECS             *          
**********************************************************************          
         SPACE 1                                                                
GETSHFT  NTR1                                                                   
         MVC   KEYSAVE,IOKEY                                                    
         LA    R2,IOKEY                                                         
         USING GCURD,R2                                                         
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ      RECORD TYPE CURRENCY                         
         MVC   GCKCURR,SPACES                                                   
         MVC   GCKCURU,CURF                                                     
         LA    R1,IORDD+IOGENDIR+IO2                                            
         GOTO1 AIO                 READ FROM CURRENCY RECORD                    
         BE    *+6                                                              
         DC    H'0'                CURRENCY MUST EXIST                          
         LA    R1,IOGET+IOGENFIL+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA2                                                      
         MVC   APHALF(1),GCRDECP                                                
         XC    TEMP(12),TEMP                                                    
         MVC   TEMP+3(5),GCRMNEXC  EXTRACT MIN MAX RATES                        
         MVI   TEMP+11,X'0C'                                                    
         XC    TEMP+18(12),TEMP+18                                              
         MVC   TEMP+21(5),GCRMXEXC                                              
         MVI   TEMP+29,X'0C'                                                    
         LA    R2,IOKEY                                                         
         MVC   GCKCURU,CURT                                                     
         LA    R1,IORDD+IOGENDIR+IO2                                            
         GOTO1 AIO                 READ TO CURRENCY RECORD                      
         BE    *+6                                                              
         DC    H'0'                CURRENCY MUST EXIST                          
         LA    R1,IOGET+IOGENFIL+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA2                                                      
         MVC   APHALF+1(1),GCRDECP                                              
         MVC   APWORK(5),GCRMNEXC  EXTRACT MIN MAX RATES                        
         MVI   APWORK+5,X'0C'                                                   
         ZAP   TEMP+30(6),APWORK(6)                                             
         MVC   APWORK(5),GCRMXEXC                                               
         MVI   APWORK+5,X'0C'                                                   
         ZAP   TEMP+12(6),APWORK(6)                                             
         SR    RE,RE               CALCULATE SHIFT VALUE                        
         SR    RF,RF                                                            
         IC    RE,APHALF                                                        
         IC    RF,APHALF+1                                                      
         SR    RF,RE                                                            
         STC   RF,XSHFT            PUT VALUE IN XSHFT                           
         DP    TEMP+0(12),TEMP+12(6)                                            
         DP    TEMP+18(12),TEMP+30(6)                                           
         MVC   APWORK(5),XRATE                                                  
         MVI   APWORK+5,X'0C'                                                   
         TM    XSTAT,GEINVRT                                                    
         BO    GETSHFT1                                                         
         ZAP   APWORK+10(13),INVERT INVERT RATE VALUE                           
         DP    APWORK+10(13),APWORK(6)                                          
         MVC   APWORK(6),APWORK+11                                              
GETSHFT1 CP    APWORK(6),TEMP(6)   CHECK MIN RATE                               
         BL    GETSHFTX                                                         
         CP    APWORK(6),TEMP+18(6) CHECK MAX RATE                              
         BH    GETSHFTX                                                         
         CR    RB,RB                                                            
*                                                                               
GETSHFTX MVC   IOKEY,KEYSAVE                                                    
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        VALIDATE MEDIA CLIENT CODE IN GCLICODE             *                   
*        RETURN CLIENT NAME IN GCLINAME                     *                   
*************************************************************                   
         SPACE 1                                                                
MEDCLI   NTR1                                                                   
*&&UK                                                                           
         CLC   GCLIOLD,GCLICODE    SAME CALL AS BEFORE?                         
         BE    EXIT                DON'T WASTE MY TIME                          
         MVC   GCLIOLD,GCLICODE                                                 
         MVC   GCLINAME,SPACES                                                  
*                                                                               
         MVC   KEYSAVE,IOKEY       SAVE CURRENT KEY                             
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         L     R9,AIOAREA2                                                      
         USING DCLI,R2                                                          
         MVC   CLIKAM,MEDAGY       SET AGY AND FIRST MEDIA                      
         MVI   CLIKTYP,CLIKTYPQ    SET CLIENT RECORD TYPE                       
         MVC   CLIKCLI,GCLICODE                                                 
*                                                                               
MEDCLIRD OI    IOINDS1,IOISWAUT    SET AUTO SWITCHING                           
         OI    APINDS,APILRERD     FLAG RDSEQ BROKEN                            
         GOTO1 AIO,IOCONMDR+IOHI+IO2                                            
         TM    IOERR,255-IOEEOF                                                 
         BNZ   MEDCLIEX                                                         
         TM    IOERR,IOEEOF                                                     
         BO    MEDCLINO                                                         
         CLC   CLIKEY(5),IOKEYSAV  TEST AM/C/CLI                                
         BE    MEDCLIFO            GOT IT                                       
         MVC   APBYTE,IOKEY                                                     
         NI    APBYTE,X'F0'        ISOLATE AGY OF REC READ                      
         CLC   APBYTE,MEDAGY       IS IT OUR AGENCY                             
         BNE   MEDCLINO            NO - FORGET IT                               
*                                                                               
         CLC   CLIKTYP(4),IOKEYSAV+1 TEST C/CLI                                 
         BE    MEDCLIFO            EQU - WE GOT IT                              
         BL    MEDCLINX            LOW - SET C/CLI & READ HI                    
*                                                                               
         TM    IOKEY,X'0F'         HIGH - BUMP MEDIA                            
         BO    MEDCLINO            UNLESS WE ARE ALREADY ON MEDIA F             
         SR    R1,R1                                                            
         IC    R1,CLIKAM           BUMP MEDIA                                   
         LA    R1,1(R1)                                                         
         STC   R1,CLIKAM                                                        
*                                                                               
MEDCLINX MVC   IOKEY+1(L'IOKEY-1),IOKEYSAV+1                                    
         B     MEDCLIRD                                                         
*                                                                               
MEDCLIFO OI    IOINDS1,IOISWAUT    GET CLIENT RECORD                            
         GOTO1 AIO,IOGET+IOCONMFL+IO2                                           
         BNE   MEDCLIEX                                                         
         LR    R2,R9                                                            
         MVC   GCLINAME,SPACES                                                  
         MVC   GCLINAME(L'CLISH),CLISH  SAVE SHORT NAME                         
         B     MEDCLIOK                                                         
*                                                                               
MEDCLIEX MVC   GCLINAME+0(2),=CL2'**'   ERROR ON CLIENT READ                    
         MVCDD GCLINAME+2(7),CT#ERROR                                           
         MVC   GCLINAME+9(2),=CL2'**'                                           
         B     MEDCLINZ                                                         
*                                                                               
MEDCLINO MVC   GCLINAME+0(2),=CL2'**' CLIENT NOT ON FILE                        
         MVCDD GCLINAME+2(7),CT#UNKNW                                           
         MVC   GCLINAME+9(2),=CL2'**' CLIENT NOT ON FILE                        
MEDCLINZ MVC   IOKEY,KEYSAVE                                                    
         LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
MEDCLIOK MVC   IOKEY,KEYSAVE                                                    
*&&                                                                             
         CR    RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        ACCOUNT CLIENT VALIDATION                          *                   
*************************************************************                   
         SPACE 1                                                                
ACCCLI   NTR1                                                                   
         CLC   GCLIOLD,GCLICODE    SAME CALL AS BEFORE?                         
         BE    ACCCLIX             DON'T WASTE MY TIME                          
         MVC   GCLIOLD,GCLICODE                                                 
         MVC   GCLINAME,SPACES                                                  
*                                                                               
         MVC   KEYSAVE,IOKEY       SAVE CURRENT KEY                             
         MVC   IOKEY,SPACES                                                     
         LA    R2,IOKEY                                                         
         L     R9,AIOAREA2                                                      
         OI    IOINDS1,IOISWAUT    SET AUTO SWITCHING                           
         OI    APINDS,APILRERD     FLAG RDSEQ BROKEN                            
         CLI   GCLILEN,0           IGNORE LEN CHECK IF ZERO                     
         BE    ACCCLI1                                                          
         USING LDGRECD,R2                                                       
         MVC   LDGKCPY,ACCAGY      SET COMPANY CODE                             
         MVI   LDGKUNT,C'S'                                                     
         MVI   LDGKLDG,C'J'                                                     
*                                                                               
         GOTO1 AIO,IOCONACD+IORD+IO2                                            
         BNZ   ACCCLIEX                                                         
         GOTO1 AIO,IOCONACM+IOGET+IO2                                           
         BNZ   ACCCLIEX                                                         
         MVI   APELEM,ACLELQ       FIND ACCOUNT LENGTHS ELEMENT                 
         L     R0,AIOAREA2                                                      
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),(APELEM,(R0)),0                        
         CLI   12(R1),0                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
         USING ACLELD,R3                                                        
         CLC   GCLILEN,ACLVLEN     CHECK INPUT LENGTH                           
         BH    ACCCLINO                                                         
*                                                                               
         USING ACTRECD,R2          READ ACCOUNT RECORD                          
ACCCLI1  MVC   IOKEY,SPACES                                                     
         MVC   ACTKCPY,ACCAGY      SET COMPANY CODE                             
         MVI   ACTKUNT,C'S'                                                     
         MVI   ACTKLDG,C'J'                                                     
         MVC   ACTKACT(5),GCLICODE CLIENT CODE                                  
*                                                                               
         GOTO1 AIO,IOCONACD+IORD+IO2                                            
         TM    IOERR,255-IOERNF                                                 
         BNZ   ACCCLIEX            HIDEOUS ERROR                                
         TM    IOERR,IOERNF                                                     
         BNZ   ACCCLINO            NOT FOUND                                    
         GOTO1 AIO,IOCONACM+IOGET+IO2                                           
         BNZ   ACCCLIEX            HIDEOUS ERROR                                
*                                                                               
         MVI   APELEM,SNMELQ       FIND SHRT NAME ELEMENT                       
         L     R0,AIOAREA2                                                      
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),(APELEM,(R0)),0                        
         CLI   12(R1),0                                                         
         BNZ   ACCCLI2             NOT FOUND                                    
         L     R3,12(R1)                                                        
         USING SNMELD,R3                                                        
         MVC   GCLINAME,SPACES                                                  
         MVC   GCLINAME(L'SNMNAME),SNMNAME                                      
         B     ACCCLIOK                                                         
*                                                                               
ACCCLI2  MVI   APELEM,NAMELQ       FIND NAME ELEMENT                            
         L     R0,AIOAREA2                                                      
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),(APELEM,(R0)),0                        
         CLI   12(R1),0                                                         
         BNZ   ACCCLINO            NOT FOUND                                    
         L     R3,12(R1)                                                        
         USING NAMELD,R3                                                        
         MVC   GCLINAME,SPACES     EX-MOVE NAME IN TO APWORK                    
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         CLM   R1,1,=AL1(16)       17 CHRS MAX                                  
         BL    *+8                                                              
         LA    R1,16                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GCLINAME(0),NAMEREC                                              
         B     ACCCLIOK                                                         
*                                                                               
ACCCLIEX MVC   GCLINAME+0(2),=CL2' *'   ERROR ON CLIENT READ                    
         MVCDD GCLINAME+2(7),CT#ERROR                                           
         MVC   GCLINAME+9(2),=CL2' *' CLIENT NOT ON FILE                        
         B     ACCCLINX                                                         
*                                                                               
ACCCLINO MVC   GCLINAME+0(2),=CL2'**'   CLIENT NOT ON FILE                      
         MVCDD GCLINAME+2(7),CT#UNKNW                                           
         MVC   GCLINAME+9(2),=CL2'**' CLIENT NOT ON FILE                        
ACCCLINX MVC   IOKEY,KEYSAVE                                                    
         LTR   RB,RB                                                            
         B     ACCCLIX                                                          
*                                                                               
ACCCLIOK MVC   IOKEY,KEYSAVE                                                    
         CR    RB,RB                                                            
*                                                                               
ACCCLIX  XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        VALIDATE LINE SUBROUTINE R4=A(LINE) IOAREA1=RECORD *                   
*************************************************************                   
         SPACE 1                                                                
VALLINE  NTR1                                                                   
         USING EXLINED,R4                                                       
         USING GEXCD,R2                                                         
         LA    R2,IOAREA1                                                       
         XC    0(255,R2),0(R2)                                                  
         XC    APCURSOR,APCURSOR                                                
*                                                                               
         TM    EXLSELH+FVIIND-FVIHDR,FVITHIS                                    
         BO    VALL010                                                          
         BNO   VALL090                                                          
*                                                                               
VALL010  MVI   GEKREC,GEKRECQ      SET FIXED VALUES FROM SELKEY                 
         MVC   GEKAGY,SELAGY       AGY                                          
         MVC   GEKSYS,SELSYS       SYSTEM                                       
         MVC   GEKCTYP,SELTYPE     TYPE                                         
*                                                                               
         MVC   GERLEN,=Y(GEFIRST+1)  SET INITIAL REC LEN                        
*                                                                               
VALL011  MVI   FVMINL,3            VALIDATE FROM CURRENCY                       
         GOTO1 AFVAL,EXLCURH                                                    
         BNE   XEXIT                                                            
         CLC   FVIFLD(3),CT@ALL    TEST FOR ALL                                 
         BNE   *+14                                                             
         MVC   GEKCURF,FFS         SET TO FFS                                   
         B     VALL012                                                          
         BAS   RE,VALCUR                                                        
         BNE   XEXIT               IF NEQ RETURN WITH ERROR                     
         MVC   GEKCURF,FVIFLD                                                   
VALL012  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         MVI   FVMINL,3            VALIDATE FROM CURRENCY                       
         GOTO1 AFVAL,EXLCURTH                                                   
         BNE   XEXIT                                                            
         BAS   RE,VALCUR                                                        
         BNE   XEXIT               IF NEQ RETURN WITH ERROR                     
         MVC   GEKCURT,FVIFLD                                                   
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLC   GEKCURF,GEKCURT     FROM & TO CANNOT BE SAME                     
         BE    VALL090             IGNORE IF THEY ARE                           
*                                                                               
VALL020  MVC   GCLINAME,SPACES                                                  
         CLI   GEKSYS,MEDQ         MEDIA DEFAULTS                               
         BNE   *+10                                                             
         MVC   GEKKEY(6),FFS                                                    
         CLI   GEKSYS,ACCQ         ACCOUNT DEFAULTS                             
         BNE   *+10                                                             
         MVC   GEKKEY(5),FFS                                                    
*                                                                               
         MVC   GCLINAME(8),CT@DFALT                                             
         XC    GCLIOLD,GCLIOLD                                                  
         MVI   FVMINL,3                                                         
         GOTO1 AFVAL,EXLCLIH                                                    
         BNE   VALL031             NO INP LEAVE DEFAULT                         
*                                                                               
         SR    R1,R1               CLIENT DATA INPUT                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),CT@DFALT  CHECK FOR DEFAULT                            
         BE    VALL031                                                          
*                                                                               
VALL022  CLI   SELSYS,ACCQ         TEST ACCOUNTING CLIENT                       
         BE    VALL025                                                          
         CLI   FVILEN,3            TEST FOR 3 CHR CLI CODE                      
         BNH   VALL024                                                          
         CLI   FVILEN,5                                                         
         BNE   ECMSG161            INVALID CLI CODE                             
         MVC   APFULL,FVIFLD+1                                                  
         NC    APFULL,NUMERIC                                                   
         CLC   APFULL,NUMERIC                                                   
         BNE   ECMSG161            MUST BE ANNNN                                
         MVC   GEKCLI(1),FVIFLD                                                 
         PACK  APDUB,FVIFLD+1(4)                                                
         CVB   R1,APDUB                                                         
         STCM  R1,3,GEKCLI+1       FORMAT TO 1CHR+PL4                           
         B     VALL024A                                                         
*                                                                               
VALL024  MVC   GEKCLI,FVIFLD       SET MEDIA CLIENT FIELD                       
VALL024A MVC   GCLICODE,GEKCLI                                                  
         BAS   RE,MEDCLI                                                        
         B     VALL030                                                          
*                                                                               
VALL025  MVC   GEKACT,FVIFLD       SET ACC CLIENT FIELD                         
         MVC   GCLILEN,FVILEN                                                   
         MVC   GCLICODE(5),GEKACT                                               
         BAS   RE,ACCCLI                                                        
*                                                                               
VALL030  BNE   ECMSG161            INVALID CLIENT                               
VALL031  MVC   EXLNAM,SPACES                                                    
         MVC   EXLNAM,GCLINAME                                                  
*                                                                               
VALL040  BAS   RE,FTVALS           SPECIAL FT MODS                              
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         USING GEXEL,R3                                                         
         MVI   GEXEL,GEXELQ                                                     
         MVI   GEXELL,GEXELLQ                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,EXLRTFH       GET FROM RATE                                
         BNE   XEXIT                                                            
         CLC   FVIFLD(2),=C'FT'                                                 
         BE    *+14                                                             
         CLC   FVIFLD(2),=C'DE'                                                 
         BNE   VALL040A                                                         
         TM    CUAUTH+1,AUT2EXF    TEST AUTHORIZED                              
         BNO   ECMSG170                                                         
         B     VALL050                                                          
*                                                                               
VALL040A CLC   GEKCURF,FFS         RATE MUST BE FT+                             
         BE    ECMSG169                                                         
         GOTO1 VALNUM,5            CONVERT TO 99999.99999 FORMAT                
         BNE   XEXIT                                                            
         OC    APFULL,APFULL       RATE CANT BE ZERO                            
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     XEXIT                                                            
         MVC   RATEFRM(5),APDUB+3                                               
         MVI   RATEFRM+5,X'0C'     CONVERT TO 99999.999999C                     
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,EXLRTTH       GET TO RATE                                  
         BE    *+8                                                              
         MVI   FVIFLD,C'1'                                                      
         GOTO1 VALNUM,5            CONVERT TO 99999.99999 FORMAT                
         BNE   XEXIT                                                            
         OC    APFULL,APFULL       RATE CANT BE ZERO                            
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     XEXIT                                                            
         MVC   RATETO(5),APDUB+3                                                
         MVI   RATETO+5,X'0C'      CONVERT TO 99999.999999C                     
*                                                                               
         MVC   XRATE,RATEFRM       TEST XXXX TO 1.0                             
         OI    GESTAT,GEINVRT                                                   
         CP    RATETO,=P'1000000'                                               
         BE    VALL045                                                          
*                                                                               
         MVC   XRATE,RATETO        TSET 1.0 TO XXXX                             
         NI    GESTAT,255-GEINVRT                                               
         CP    RATEFRM,=P'1000000'                                              
         BE    VALL045                                                          
*                                                                               
         XC    XRATE,XRATE         CALCULATE RATE VALUE                         
         XC    TEMP,TEMP                                                        
         CLC   RATEFRM,RATETO      FIND WHICH IS GREATER                        
         BL    VALL041                                                          
         OI    GESTAT,GEINVRT                                                   
         MVC   TEMP+7(5),RATEFRM   SET UP TEMP FOR DP                           
         MVI   TEMP+15,X'0C'                                                    
         ZAP   TEMP+16(6),RATETO                                                
         B     VALL042                                                          
*                                                                               
VALL041  NI    GESTAT,255-GEINVRT                                               
         MVC   TEMP+7(5),RATETO    SET UP TEMP FOR DP                           
         MVI   TEMP+15,X'0C'                                                    
         ZAP   TEMP+16(6),RATEFRM                                               
*                                                                               
VALL042  LA    R1,EXLRTFH          POSITION CURSOR TO RATE FROM                 
         ST    R1,APCURSOR                                                      
         DP    TEMP(16),TEMP+16(6) DIVIDE TO GET SINGLE RATE                    
         OC    TEMP(4),TEMP                                                     
         BNZ   ECMSG162            RATE TOO LARGE EVEN TO CALCULATE             
         MVC   XRATE,TEMP+4                                                     
*                                                                               
VALL045  MVC   XSTAT,GESTAT                                                     
         MVC   CURF,GEKCURF                                                     
         MVC   CURT,GEKCURT                                                     
         GOTO1 GETSHFT             GET SHFT VALUE AND TEST RANGE                
         BNE   VALL045F                                                         
*                                                                               
         CLI   GEKSYS,MEDQ                                                      
         BNE   VALL046                                                          
*                                                                               
         CLI   CUCTRY,CTRYGER                                                   
         BNE   VALL046                                                          
*                                                                               
         CP    RATEFRM,=P'1000000000'      RATE IS 1000 TO N?                   
         BNE   VALL045A                                                         
         MVI   HALF1+1,5                                                        
         LA    RF,EXLRTT                                                        
         STCM  RF,15,NUMBER        STORE ADDRESS OF RATE                        
         GOTO1 ANUMCHEK,DMCB,(RC)                                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     XEXIT                                                            
*                                                                               
         OI    GESTAT,GETHOUQ                                                   
         NI    GESTAT,255-GEINVRT                                               
         MVC   XRATE,RATETO                                                     
         B     VALL046                                                          
*                                                                               
VALL045A CP    RATEFRM,=P'100000000'       RATE IS 100 TO N?                    
         BNE   VALL045B                                                         
         MVI   HALF1+1,6                                                        
         LA    RF,EXLRTT                                                        
         STCM  RF,15,NUMBER                                                     
         GOTO1 ANUMCHEK,DMCB,(RC)                                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     XEXIT                                                            
         OI    GESTAT,GEHUNQ                                                    
         NI    GESTAT,255-GEINVRT                                               
         MVC   XRATE,RATETO                                                     
         B     VALL046                                                          
*                                                                               
VALL045B CP    RATEFRM,=P'10000000'        RATE IS 10 TO N?                     
         BNE   VALL045C                                                         
         MVI   HALF1+1,7                                                        
         LA    RF,EXLRTT                                                        
         STCM  RF,15,NUMBER                                                     
         GOTO1 ANUMCHEK,DMCB,(RC)                                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     XEXIT                                                            
         OI    GESTAT,GETENQ                                                    
         NI    GESTAT,255-GEINVRT                                               
         MVC   XRATE,RATETO                                                     
         B     VALL046                                                          
*                                                                               
VALL045C CP    RATETO,=P'1000000000'       RATE IS N TO 1000?                   
         BNE   VALL045D                                                         
         MVI   HALF1+1,5                                                        
         LA    RF,EXLRTF                                                        
         STCM  RF,15,NUMBER                                                     
         GOTO1 ANUMCHEK,DMCB,(RC)                                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     XEXIT                                                            
         OI    GESTAT,GETHOUQ                                                   
         OI    GESTAT,GEINVRT                                                   
         MVC   XRATE,RATEFRM                                                    
         B     VALL046                                                          
*                                                                               
VALL045D CP    RATETO,=P'100000000'        RATE IS N TO 100?                    
         BNE   VALL045E                                                         
         MVI   HALF1+1,6                                                        
         LA    RF,EXLRTF                                                        
         STCM  RF,15,NUMBER                                                     
         GOTO1 ANUMCHEK,DMCB,(RC)                                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     XEXIT                                                            
         OI    GESTAT,GEHUNQ                                                    
         OI    GESTAT,GEINVRT                                                   
         MVC   XRATE,RATEFRM                                                    
         B     VALL046                                                          
*                                                                               
VALL045E CP    RATETO,=P'10000000'         RATE IS N TO 10?                     
         BNE   VALL046                                                          
         MVI   HALF1+1,7                                                        
         LA    RF,EXLRTF                                                        
         STCM  RF,15,NUMBER                                                     
         GOTO1 ANUMCHEK,DMCB,(RC),(RF)                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     XEXIT                                                            
         OI    GESTAT,GETENQ                                                    
         OI    GESTAT,GEINVRT                                                   
         MVC   XRATE,RATEFRM                                                    
         B     VALL046                                                          
*                                                                               
VALL045F LA    R1,EXLRTFH          POSITION CURSOR TO RATE FROM                 
         ST    R1,APCURSOR                                                      
         BNZ   ECMSG162            RANGE ERROR                                  
*                                                                               
VALL046  MVC   GEXRATE,XRATE                                                    
         MVC   GEXSHFT,XSHFT                                                    
         GOTO1 AADDELS,GEXCD       ADD EXCHANGE ELEMENT                         
*                                                                               
         GOTO1 ASETACT,GEXCD       DEFINE ACTIVITY ELEMENT                      
*                                                                               
         LA    R3,APELEM           CLEAR SPACE FOR ELEMENT                      
         XC    APELEM,APELEM                                                    
         USING GIXEL,R3                                                         
         MVI   GIXEL,GIXELQ        SET UP A RATE INPUT ELEMENT                  
         MVI   GIXELL,GIXELLQ                                                   
         MVC   GIXFROM,RATEFRM                                                  
         MVC   GIXTO,RATETO                                                     
         GOTO1 AADDELS,GEXCD       THEN ADD IT                                  
         B     VALL070                                                          
*                                                                               
         USING GEXEL,R3                                                         
VALL050  EQU   *                                                                
         CLI   FVIFLD+2,C'+'                                                    
         BNE   *+12                                                             
         OI    GEXFLAG,GEXFTPL                                                  
         B     VALL051                                                          
         CLI   FVIFLD+2,C'-'                                                    
         BNE   ECMSG169                                                         
         OI    GEXFLAG,GEXFTMI                                                  
*                                                                               
VALL051  SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         LR    RE,R1                                                            
         LA    R1,FVIFLD(R1)                                                    
         CLI   0(R1),C'%'                                                       
         BNE   ECMSG169                                                         
         OI    GEXFLAG,GEXFTPC                                                  
         BCTR  RE,0                                                             
*                                                                               
VALL052  MVC   APWORK(10),FVIFLD+3 MODIFY FIELD                                 
         MVC   FVIFLD,SPACES                                                    
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),APWORK                                                 
         STC   RE,FVXLEN                                                        
         GOTO1 VALNUM,5            CONVERT TO 99999.99999 FORMAT                
         BNE   XEXIT                                                            
         MVC   GEXRATE,APDUB+3                                                  
         TM    GEXFLAG,GEXFTPC     30.00000% IS MAX PERCENTAGE                  
         BNO   *+14                                                             
         CLC   GEXRATE,=X'0003000000'                                           
         BH    ERRFMAX                                                          
*                                                                               
         GOTO1 AADDELS,GEXCD       ADD EXCHANGE ELEMENT                         
*                                                                               
         GOTO1 ASETACT,GEXCD       DEFINE ACTIVITY ELEMENT                      
*                                                                               
VALL070  LA    R1,EXLPERH                                                       
         ST    R1,APCURSOR                                                      
         MVC   GEKPEND,FFS                                                      
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,EXLPERH                                                    
         BNE   XEXIT                                                            
         XC    GEKPEND,GEKPEND                                                  
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         SHI   R1,3                                                             
         BM    VALL075                                                          
         LA    R1,FVIFLD(R1)                                                    
         CLC   0(3,R1),CT@UFN      TEST FOR UFN                                 
         BNE   VALL075                                                          
         MVC   GEKPEND,FFS                                                      
         MVC   0(3,R1),SPACES                                                   
VALL075  MVC   APBYTE,CULANG                                                    
         OI    APBYTE,X'20'                                                     
         GOTO1 VPERVAL,APPARM,(FVILEN,FVIFLD),(APBYTE,SCWORK)                   
         CLI   4(R1),X'04'                                                      
         BNE   *+10                                                             
         MVC   SCWORK+36(2),SCWORK+34                                           
         NI    4(R1),(X'FF'-X'04') ONE DATE IS OK                               
         CLI   4(R1),X'00'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFINVDT) INVALID DATE                              
         B     XEXIT                                                            
*                                                                               
         MVC   GEKPSTA,SCWORK+34                                                
         CLC   GEKPEND,FFS                                                      
         BE    *+10                                                             
         MVC   GEKPEND,SCWORK+36                                                
*                                                                               
         MVC   APWORK,SPACES                                                    
         MVC   APWORK(17),SCWORK+11                                             
         CLC   GEKPEND,FFS                                                      
         BNE   *+14                                                             
         MVI   APWORK+7,C'-'                                                    
         MVC   APWORK+8(8),CT@UFN                                               
         GOTO1 DISPFLD,EXLPERH                                                  
*                                                                               
         CLI   APACTN,ACTCHA       DON'T CHECK DATES ON CHANGE                  
         BE    VALL080                                                          
*                                                                               
         MVC   IOKEY,0(R2)                                                      
         GOTO1 CHECKUP             TEST IF OK TO ADD                            
         BNE   ERRDATE             OVERLAPING DATES                             
         GOTO1 CHECKEUR            TEST IF OK FOR EURO                          
         BH    ERREURO                                                          
         BL    ERREDATE                                                         
*                                                                               
VALL080  NI    EXLSELH+6,255-X'01' RESET SEL IP NEXT TIME                       
         BAS   RE,DISLINE                                                       
         CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
VALL090  LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        MODIFY FT INPUT FOR UAK                            *                   
*************************************************************                   
         SPACE 1                                                                
FTVALS   NTR1                                                                   
         CLI   SELSYS,GEKSFTQ                                                   
         BE    FTVAL001                                                         
         CLI   SELSYS,GEKSDEQ                                                   
         BNE   FTVALX                                                           
*                                                                               
FTVAL001 CLC   SELCURF,=C'UAK'                                                  
         BNE   FTVALX                                                           
*                                                                               
         XC    APWORK,APWORK                                                    
         LA    RF,10                                                            
         LA    R1,EXLRTF+10        RATE= 123456.1234                            
         LA    RE,APWORK+10                                                     
FTVAL010 MVC   0(1,RE),0(R1)                                                    
FTVAL011 BCTR  RE,0                                                             
         BCTR  R1,0                                                             
         CLI   0(R1),C'.'                                                       
         BE    FTVAL020                                                         
         BCT   RF,FTVAL010                                                      
         B     FTVAL030                                                         
*                                                                               
FTVAL020 SHI   R1,3                                                             
         SHI   RE,2                                                             
         MVC   0(3,RE),0(R1)                                                    
         BCTR  RE,0                                                             
         MVI   0(RE),C'.'                                                       
         SHI   RF,3                                                             
         BP    FTVAL011                                                         
         DC    H'0'                DUMP IF IT GOES NEG                          
*                                                                               
FTVAL030 XC    EXLRTF,EXLRTF                                                    
         MVC   EXLRTF(8),APWORK                                                 
         MVI   EXLRTFH+5,9                                                      
*                                                                               
FTVALX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY LINE SUBROUTINE R4=A(LINE) R2=A(RECORD)    *                   
*************************************************************                   
         SPACE 1                                                                
DISLINE  NTR1                                                                   
         USING GEXCD,R2            R2=A(RECORD KEY)                             
         USING EXLINED,R4                                                       
         CLC   GEKCURF,FFS         TEST FOR ALL CURRENCYS                       
         BNE   *+14                                                             
         MVC   EXLCUR,CT@ALL                                                    
         B     *+10                                                             
         MVC   EXLCUR(3),GEKCURF                                                
         MVC   EXLCURT(3),GEKCURT                                               
*                                                                               
         CLI   GEKSYS,GEKSFTQ      TEST FINSTAT                                 
         BE    DLI005                                                           
         CLI   GEKSYS,GEKSDEQ      TEST GFTSTAT                                 
         BE    DLI005                                                           
         OC    SELCURF,SELCURF     TEST CURRENCY SPECIFIC                       
         BNZ   DLI010                                                           
         CLC   SELCLI,FFS          TEST DEFAULT CLIENT                          
         BNE   DLI010                                                           
*                                                                               
DLI005   CLC   GEKCURF,OLDINFO     ARE WE ON SAME CURRENCY                      
         BE    DLI020                                                           
         MVC   FVIFLD(10),SPACES   YES SO READ CURRENCY REC                     
         MVC   FVIFLD(3),GEKCURF                                                
         BAS   RE,VALCUR                                                        
         L     R1,AIOAREA2                                                      
         SR    RF,RF               CALCULATE LENGTH OF NAME                     
         IC    RF,GCRLEN-GCKEY(R1)                                              
         LA    RE,GCRLENQ+1                                                     
         SR    RF,RE                                                            
         CHI   RF,16               MUST BE < 17                                 
         BL    *+8                                                              
         LA    RF,16                                                            
         MVC   EXLNAM,SPACES                                                    
         EX    RF,*+8              MOVE TO PRINT LINE                           
         B     *+10                                                             
         MVC   EXLNAM(0),GCRNAME-GCKEY(R1)                                      
         MVC   OLDINFO(3),GEKCURF                                               
         MVI   APINDS,APILRERD     SET RSEQ BROKEN                              
         B     DLI020                                                           
*                                                                               
DLI010   CLI   GEKSYS,MEDQ         TEST MEDIA                                   
         BNE   DLI015                                                           
*                                                                               
         CLC   GEKCLI,FFS          DEFAULT CLIENT                               
         BNE   *+14                                                             
         MVC   EXLNAM(10),CT@DFALT                                              
         B     DLI020                                                           
         BAS   RE,DISPCLI                                                       
         MVC   EXLCLI,APWORK                                                    
         MVC   GCLICODE(L'GEKCLI),GEKCLI                                        
         BAS   RE,MEDCLI                                                        
         B     DLI019                                                           
*                                                                               
DLI015   CLC   GEKACT,FFS          DEFAULT CLIENT                               
         BNE   *+14                                                             
         MVC   EXLNAM(10),CT@DFALT                                              
         B     DLI020                                                           
         MVC   GCLICODE(L'GEKACT),GEKACT                                        
         MVC   EXLCLI(L'GEKACT),GEKACT                                          
         MVI   GCLILEN,0                                                        
         BAS   RE,ACCCLI                                                        
*                                                                               
DLI019   MVC   EXLNAM,GCLINAME                                                  
*                                                                               
DLI020   XC    APELEM,APELEM                                                    
         MVI   APELEM,GIXELQ                                                    
         LR    R1,R2                                                            
         GOTO1 AGETELS             LOOK FOR A RATE INPUT ELEMENT                
         OC    APPARM(4),APPARM                                                 
         BZ    DLI030              NOT FOUND - DO EDITING AS NORMAL             
*                                                                               
         L     R3,APPARM                                                        
         USING GIXEL,R3                                                         
         LA    R1,GIXFROM          EDIT FROM CURRENCY AS IT WAS INPUT           
         BAS   RE,EDITNUM                                                       
         MVC   EXLRTF,FVOMSG                                                    
         LA    R1,GIXTO            EDIT 'TO' CURRENCY AS IT WAS INPUT           
         BAS   RE,EDITNUM                                                       
         MVC   EXLRTT,FVOMSG                                                    
         B     DLI040                                                           
         DROP  R3                                                               
*                                                                               
DLI030   LA    R1,GEXRATE                                                       
         BAS   RE,EDITNUM                                                       
         MVC   EXLRTF,SPACES                                                    
         MVC   EXLRTT,SPACES                                                    
         TM    GEXFLAG,GEXFTPL+GEXFTMI                                          
         BNZ   DLI035                                                           
         TM    GESTAT,GEINVRT                                                   
         BO    DLI032                                                           
         MVC   EXLRTT,FVOMSG                                                    
         CLI   GEKSYS,MEDQ                                                      
         BNE   DLI031                                                           
         CLI   CUCTRY,CTRYGER                                                   
         BNE   DLI031                                                           
         TM    GESTAT,GETENQ                                                    
         BNO   *+14                                                             
         MVC   EXLRTF(4),TENVAL                                                 
         B     DLI040                                                           
*                                                                               
         TM    GESTAT,GEHUNQ                                                    
         BNO   *+14                                                             
         MVC   EXLRTF(5),HUNVAL                                                 
         B     DLI040                                                           
*                                                                               
         TM    GESTAT,GETHOUQ                                                   
         BNO   *+14                                                             
         MVC   EXLRTF(6),THOUVAL                                                
         B     DLI040                                                           
*                                                                               
DLI031   MVC   EXLRTF(3),ONEVAL                                                 
         B     DLI040                                                           
*                                                                               
DLI032   MVC   EXLRTF,FVOMSG                                                    
         CLI   GEKSYS,MEDQ                                                      
         BNE   DLI033                                                           
         CLI   CUCTRY,CTRYGER                                                   
         BNE   DLI033                                                           
         TM    GESTAT,GETENQ                                                    
         BNO   *+14                                                             
         MVC   EXLRTT(4),TENVAL                                                 
         B     DLI040                                                           
*                                                                               
         TM    GESTAT,GEHUNQ                                                    
         BNO   *+14                                                             
         MVC   EXLRTT(5),HUNVAL                                                 
         B     DLI040                                                           
*                                                                               
         TM    GESTAT,GETHOUQ                                                   
         BNO   *+14                                                             
         MVC   EXLRTT(6),THOUVAL                                                
         B     DLI040                                                           
*                                                                               
DLI033   MVC   EXLRTT(3),ONEVAL                                                 
         B     DLI040                                                           
*                                                                               
DLI035   MVC   EXLRTF(3),=C'FT+'                                                
         TM    GEXFLAG,GEXFTMI                                                  
         BNO   *+8                                                              
         MVI   EXLRTF+2,C'-'                                                    
*                                                                               
         TM    GEXFLAG,GEXFTPC                                                  
         BNO   *+8                                                              
         MVI   1(R1),C'%'                                                       
         MVC   EXLRTF+3(7),FVOMSG                                               
*                                                                               
DLI040   CLI   CULANG,2                                                         
         BNH   DLI040A                                                          
         GOTO1 VDATCON,APPARM,(2,GEKPSTA),(8,EXLPER)                            
         OI    EXLPER,X'F0'                                                     
         MVI   EXLPER+8,C'-'                                                    
         MVC   EXLPER+9(8),CT@UFN                                               
         CLC   GEKPEND,=X'FFFF'                                                 
         BE    DLI050                                                           
         GOTO1 VDATCON,APPARM,(2,GEKPEND),(8,EXLPER+9)                          
         OI    EXLPER+9,X'F0'                                                   
         B     DLI050                                                           
*                                                                               
DLI040A  GOTO1 VDATCON,APPARM,(2,GEKPSTA),(8,EXLPER)                            
         OI    EXLPER,X'F0'                                                     
         MVI   EXLPER+7,C'-'                                                    
         MVC   EXLPER+8(8),CT@UFN                                               
         CLC   GEKPEND,=X'FFFF'                                                 
         BE    DLI050                                                           
         GOTO1 VDATCON,APPARM,(2,GEKPEND),(8,EXLPER+8)                          
         OI    EXLPER+8,X'F0'                                                   
*                                                                               
DLI050   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GENERAL FIELD XMT IF CHANGED                                       *          
* R1=A(TWAHDR)                                                       *          
* APWORK MUST CONTAIN THE NEW TEXT                                   *          
**********************************************************************          
         SPACE 1                                                                
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SHI   RF,L'FVIHDR                                                      
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SHI   RF,L'FVIHDR         KNOCK OFF HEADER EXTENSION                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         COMPARE FIELD CONTENTS                       
         BER   RE                                                               
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW FIELD                            
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
         SPACE 1                                                                
***********************************************************************         
* FIND NEXT ELEMENT OF SAME CODE WITHIN RECORD                        *         
* NTRY R3=A(CURRENT ELEMENT)                                          *         
*      APELEM = ELCODE TO FIND                                        *         
* EXIT CC EQ - FOUND - R3=A(NEW ELEMENT)                              *         
*      CC NE - NOT FOUND                                              *         
***********************************************************************         
         SPACE 1                                                                
NEXTEL   ZIC   RF,1(R3)            L'ELEMENT                                    
         AR    R3,RF               A(NEXT ELEMNT)                               
         ICM   RF,1,1(R3)          L'ELEMENT                                    
         BNZ   *+8                                                              
         LTR   RB,RB               FORCE CC NON ZERO                            
         BR    RE                                                               
         CLC   0(1,R3),APELEM                                                   
         BR    RE                                                               
         SPACE 1                                                                
*************************************************************                   
*        GENERAL INFO EXITS                                 *                   
*************************************************************                   
         SPACE 1                                                                
IGMSG15  LA    R1,15               HIT ENTER FOR NEXT                           
         B     SETGINF                                                          
IGMSG16  LA    R1,16               END OF LIST                                  
         B     SETGINF                                                          
IGMSG26  LA    R1,26               NOTHING TO DISPLAY                           
         B     SETGINF                                                          
IGMSG28  LA    R4,LSTACT1H         R4=SCREEN START                              
         LA    R1,EXLCURH-EXLINED(R4)                                           
         ST    R1,APCURSOR                                                      
         LA    R1,28               ENTER DATA                                   
         B     SETGINF                                                          
IGMSG32  LA    R1,32               LIST DISPLAYED ENTER CHANGES OR NEXT         
         B     SETGINF                                                          
IGMSG33  LA    R1,33               END OF LIST ENTER CHANGES OR FIRST           
         B     SETGINF                                                          
*                                                                               
SETGINF  XC    APPARM(32),APPARM   SET UP GETTXT BLOCK                          
         LA    RF,APPARM                                                        
         MVI   8(RF),C'I'          MESSAGE TYPE                                 
         MVI   21(RF),X'FF'        GENERAL MSG                                  
         STH   R1,2(RF)            MESSAGE NUMBER                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     XEXIT               BACK TO GENERAL                              
         EJECT                                                                  
*************************************************************                   
*        GENERAL ERROR EXITS                                *                   
*************************************************************                   
         SPACE 1                                                                
EGMSG49  LA    R1,49               RECORD EXISTS                                
         B     SETGERR                                                          
EGMSG58  LA    R1,58               RECORD EXISTS AND IS DELETED                 
         B     SETGERR                                                          
*                                                                               
SETGERR  XC    APPARM(32),APPARM   SET UP GETTXT BLOCK                          
         LA    RF,APPARM                                                        
         MVI   8(RF),C'E'          MESSAGE TYPE                                 
         MVI   21(RF),X'FF'        GENERAL MSG                                  
         STH   R1,2(RF)            MESSAGE NUMBER                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     XEXIT               BACK TO GENERAL                              
         EJECT                                                                  
*************************************************************                   
*        CONTROL ERROR EXITS                                *                   
*************************************************************                   
         SPACE 1                                                                
ECMSG2   LA    R1,2                INVALID INPUT FIELD                          
         B     SETCERR                                                          
ECMSG14  LA    R1,14               RECORD ALREADY EXISTS                        
         B     SETCERR                                                          
ECMSG159 LA    R1,159              CURRENCY NOT FOUND                           
         B     SETCERR                                                          
ECMSG161 LA    R1,161              INVALID CLIENT CODE                          
         B     SETCERR                                                          
ECMSG162 LA    R1,162              VALUE OUTSIDE RANGE FOR CURRENCY             
         B     SETCERR                                                          
ECMSG163 LA    R1,163              PERIOD OVERLAPS ANOTHER                      
         B     SETCERR                                                          
ECMSG164 LA    R1,164              FT RATES CANNOT BE CHANGED                   
         B     SETCERR                                                          
ECMSG169 LA    R1,169              RATE MUST BE FT+/-                           
         B     SETCERR                                                          
ECMSG170 LA    R1,170              NOT AUTHORIZED FOR FT RATES                  
         B     SETCERR                                                          
ERRCURR  LA    R1,159              CURRENCY NOT FOUND                           
         B     SETCERR                                                          
ERRTYPE  LA    R1,160              INVALID RATE TYPE                            
         B     SETCERR                                                          
ERRCLIC  LA    R1,161              INVALID CLIENT CODE                          
         B     SETCERR                                                          
ERRRNGE  LA    R1,162              VALUE OUTSIDE RANGE FOR CURRS                
         B     SETCERR                                                          
ERRDATE  LA    R1,163              DATE PERIOD OVERLAPS ANOTHER                 
         B     SETCERR                                                          
ERRFTUP  LA    R1,164              CAN'T UPDATE FT RATES                        
         B     SETCERR                                                          
ERRMX1W  LA    R1,165              MAX 1 WEEK FOR THIS REPORT                   
         B     SETCERR                                                          
ERRMX1Y  LA    R1,166              MAX 1 YEAR FOR THIS REPORT                   
         B     SETCERR                                                          
ERRFTDSP LA    R1,167              ACTION DISPLAY INVALID FOR FT                
         B     SETCERR                                                          
ERRFMAX  LA    R1,9                FIELD VALUE GREATER THAN MAX                 
         B     SETCERR                                                          
ERRDECP  LA    R1,153              TOO MANY DECIMAL PLACES                      
         B     SETCERR                                                          
ERREURO  LA    R1,171              EURO MEMBER INVALID FOR EXCHANGE             
         B     SETCERR                                                          
ERREDATE LA    R1,260              INVALID EFFECTIVE DATE                       
         B     SETCERR                                                          
*                                                                               
SETCERR  MVI   MODE,1                                                           
         L     RF,SCAUTL                                                        
         CLC   TSYM-UTLD(5,RF),=C'DUMMY'                                        
         BNE   *+8                                                              
         MVI   MODE,0                                                           
         XC    APPARM(32),APPARM   SET UP GETTXT BLOCK                          
         LA    RF,APPARM                                                        
         MVI   8(RF),C'E'          MESSAGE TYPE                                 
         MVI   21(RF),X'0A'        CONTROL MSG                                  
         STH   R1,2(RF)            MESSAGE NUMBER                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     XEXIT               BACK TO GENERAL                              
         EJECT                                                                  
*************************************************************                   
*        CONTROL INFO EXITS                                 *                   
*************************************************************                   
         SPACE 1                                                                
ICMSG23  LA    R1,23               RECORDS ADDED - HIT ENTER                    
         B     SETCINF                                                          
ICMSG24  LA    R1,24               RECORDS DISPLAYED D TO DELETE                
         B     SETCINF                                                          
ICMSG25  LA    R1,25               RECORDS DISPLAYED R TO RESTORE               
         B     SETCINF                                                          
ICMSG26  LA    R1,26               ENTER CURR/CLI RATE                          
         B     SETCINF                                                          
*                                                                               
SETCINF  XC    APPARM(32),APPARM   SET UP GETTXT BLOCK                          
         LA    RF,APPARM                                                        
         MVI   8(RF),C'I'          MESSAGE TYPE                                 
         MVI   21(RF),X'0A'        CONTROL MSG                                  
         STH   R1,2(RF)            MESSAGE NUMBER                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     XEXIT               BACK TO GENERAL                              
         EJECT                                                                  
ALLCURN  DS    0CL80                                                            
         DC    XL42'00',X'3A26',XL22'00',C'ALL CURRENCIES'                      
FFS      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
NUMERIC  DC    XL16'F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0'                           
INVERT   DC    PL7'1000000000000'                                               
         DC    CL10'FT UPDATE='                                                 
FTUPFLAG DC    C'N'                CHANGE TO Y TO ENABLE FT UPDATES             
MAXNUM   DC    PL8'214783647'                                                   
MEDIR    DC    CL8'MEDIR '                                                      
MEDFILE  DC    CL8'MEDFILE'                                                     
ACCMST   DC    CL8'ACCMST '                                                     
DMRDHI   DC    CL8'DMRDHI'                                                      
GETREC   DC    CL8'GETREC'                                                      
MFLIST   DC    C'NMEDDIR '                                                      
         DC    C'NMEDFIL '                                                      
         DC    C'X'                                                             
AFLIST   DC    C'NACCDIR '                                                      
         DC    C'NACCMST '                                                      
         DC    C'X'                                                             
*                                                                               
FINSTAT  DC    C'FT     '                                                       
GFTSTAT  DC    C'DE     '                                                       
DC@SLASH DC    CL10'/'                                                          
         SPACE 1                                                                
REPDESCL DC    C'EXCHANGE LIST'                                                 
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#EXREP,20,L                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,57,CT#EXREP,20,LU                                             
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
BOXLEN   EQU   100                                                              
BOXTOP   MVI   0(R1),X'BF'                                                      
         MVC   1(107,R1),0(R1)                                                  
         MVI   0(R1),X'AC'                                                      
         MVI   8(R1),X'CC'                                                      
         MVI   22(R1),X'CC'                                                     
         MVI   33(R1),X'CC'                                                     
         MVI   63(R1),X'CC'                                                     
         MVI   89(R1),X'CC'                                                     
         MVI   107(R1),X'BC'                                                    
         BR    RE                                                               
BOXMID   MVI   0(R1),X'BF'                                                      
         MVC   1(107,R1),0(R1)                                                  
         MVI   0(R1),X'EB'                                                      
         MVI   8(R1),X'8F'                                                      
         MVI   22(R1),X'8F'                                                     
         MVI   33(R1),X'8F'                                                     
         MVI   63(R1),X'8F'                                                     
         MVI   89(R1),X'8F'                                                     
         MVI   107(R1),X'EC'                                                    
         BR    RE                                                               
BOXBTM   MVI   0(R1),X'BF'                                                      
         MVC   1(107,R1),0(R1)                                                  
         MVI   0(R1),X'AB'                                                      
         MVI   8(R1),X'CB'                                                      
         MVI   22(R1),X'CB'                                                     
         MVI   33(R1),X'CB'                                                     
         MVI   63(R1),X'CB'                                                     
         MVI   89(R1),X'CB'                                                     
         MVI   107(R1),X'BB'                                                    
         BR    RE                                                               
BOXLIN   MVI   0(R1),X'40'                                                      
         MVC   1(107,R1),0(R1)                                                  
         MVI   0(R1),X'FA'                                                      
         MVI   8(R1),X'FA'                                                      
         MVI   22(R1),X'FA'                                                     
         MVI   33(R1),X'FA'                                                     
         MVI   63(R1),X'FA'                                                     
         MVI   89(R1),X'FA'                                                     
         MVI   107(R1),X'FA'                                                    
         BR    RE                                                               
BOXHEAD  ST    RE,SAVERE                                                        
         LA    R1,REPP1                                                         
         BAS   RE,BOXTOP                                                        
         LA    R1,REPP2                                                         
         BAS   RE,BOXLIN                                                        
         MVCDD 9(13,R1),CT#CUR,C                                                
         MVCDD 64(25,R1),CT#XRATE,C                                             
         LA    R1,REPP3                                                         
         BAS   RE,BOXLIN                                                        
         MVCDD 1(7,R1),CT#SYS,L                                                 
         MVCDD 11(5,R1),CT#FROM,C                                               
         MVCDD 17(5,R1),CT#TO,C                                                 
         MVCDD 23(10,R1),CT#TYPE,C                                              
         MVCDD 36(7,R1),CT#CLI,C                                                
         MVCDD 66(11,R1),CT#FROM,C                                              
         MVCDD 78(11,R1),CT#TO,C                                                
         MVCDD 90(17,R1),CT#PERD,C                                              
         LA    R1,REPP4                                                         
         BAS   RE,BOXMID                                                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        EQUATES & LITERALS & DD & TABLES                   *                   
*************************************************************                   
         SPACE 1                                                                
MEDQ     EQU   4                                                                
ACCQ     EQU   6                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
DDDCLST  DS    0C                                                               
         DCDDL CT#ACCNG,10,L                                                    
         DCDDL CT#BKNG,10,L                                                     
         DCDDL CT#CLOSE,10,L                                                    
         DCDDL CT#DFALT,10,L                                                    
         DCDDL CT#MTH,6,L                                                       
         DCDDL CT#ALL,10,L                                                      
         DCDDL CT#UFN,8,L                                                       
         DCDDL CT#DEL,3,L                                                       
         DCDDL CT#RSR,8,L                                                       
         DC    X'00'                                                            
         SPACE 1                                                                
TYPTAB   DS    0H                                                               
         DC    X'41F0',S(CT@ALL),X'0',AL1(ACCQ)                                 
         DC    X'41F0',S(CT@BKNG),C'B',AL1(MEDQ)                                
         DC    X'41F0',S(CT@ACCNG),C'A',AL1(MEDQ)                               
         DC    X'41F0',S(CT@CLOSE),C'C',X'FF'                                   
         DC    X'41F0',S(DS@1MNTH),C'1',X'FF'                                   
         DC    X'41F0',S(DS@3MNTH),C'3',X'FF'                                   
         DC    X'41F0',S(DS@6MNTH),C'6',X'FF'                                   
         DC    X'41F0',S(DS@1MT),C'1',X'FF'                                     
         DC    X'41F0',S(DS@3MT),C'3',X'FF'                                     
         DC    X'41F0',S(DS@6MT),C'6',X'FF'                                     
         DC    H'0'                                                             
         EJECT                                                                  
*******************************************************************             
*        EUROTAB - EURO MEMBER CURRENCIES AND EFFECTIVE DATE      *             
*******************************************************************             
         SPACE 1                                                                
EUROTAB  DC    C'ATS',X'C621'                                                   
         DC    C'BEF',X'C621'                                                   
         DC    C'BEL',X'C621'                                                   
         DC    C'DEM',X'C621'                                                   
         DC    C'ESP',X'C621'                                                   
         DC    C'FIM',X'C621'                                                   
         DC    C'FRF',X'C621'                                                   
         DC    C'IEP',X'C621'                                                   
         DC    C'ITL',X'C621'                                                   
         DC    C'LUF',X'C621'                                                   
         DC    C'NLG',X'C621'                                                   
         DC    C'PTE',X'C621'                                                   
         DC    C'GRD',X'CA21'                                                   
         DC    C'SIT',X'D621'                                                   
         DC    C'MTL',X'D821'                                                   
         DC    C'CYP',X'D821'                                                   
         DC    X'FF'                                                            
         SPACE 3                                                                
*        DROP  R8,RA          SHOULD DROP BASE REGISTERS HERE                   
*                             BUT THAT MEANS A LOT OF WORK                      
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE SELECT PARAMETERS                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   NMOD1 0,**VALK**                                                       
         L     RC,0(R1)            RESTORE W/S                                  
         USING LOCALD,RC                                                        
         LA    R2,IOKEY                                                         
         XC    GEKEY,GEKEY                                                      
         XC    SELKEY,SELKEY                                                    
         OI    APINDS,APILFLST     SET FIRST TIME FLAG                          
         MVI   GEKREC,GEKRECQ      RECORD TYPE                                  
*                                                                               
         MVC   SELAGY,TWAAGY                                                    
         OI    LSTSYSH+6,X'81'     SET SYS TO XMT+MODIFY                        
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTSYSH       VALIDATE EXCHANGE SYSTEM                     
         BE    VS005                                                            
         CLI   SAVESYS,0           TEST PREVIOUS SYSTEM                         
         BE    VALSELX                                                          
         CLI   SAVESYS,GEKSFTQ     WAS IT FT                                    
         BE    VS010                                                            
         CLI   SAVESYS,GEKSDEQ     WAS IT GERMAN FT                             
         BE    VS010                                                            
         MVC   SELSYS,SAVESYS                                                   
         GOTO1 ADISSYS,SELSYS      GET SYSTEM NAME                              
         B     VS020                                                            
*                                                                               
VS005    SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+8              TEST FINSTAT INPUT                           
         B     *+10                                                             
         CLC   FVIFLD(0),FINSTAT                                                
         BE    VS010                                                            
         EX    R1,*+8              TEST GFTSTAT INPUT                           
         B     *+10                                                             
         CLC   FVIFLD(0),GFTSTAT                                                
         BE    VS012                                                            
         GOTO1 AVALSYS,LSTSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALSELX                                                          
         MVC   FVMSGNO,=AL2(FVFESYS)                                            
*                                                                               
         CLI   APWORK,MEDQ         MEDIA                                        
         BE    VS006                                                            
         CLI   APWORK,ACCQ         ACCOUNTING                                   
         BNE   VALSELX                                                          
         MVC   FROMDEF,AGYCUR      SET ACCOUNTING DEFAULTS                      
         MVC   TODEF,CT@ALL                                                     
VS006    MVC   SELSYS,APWORK                                                    
         GOTO1 ADISSYS,SELSYS      DISPLAY SYSTEM NAME                          
         B     VS020                                                            
*                                                                               
VS010    MVI   SELSYS,GEKSFTQ      SYSTEM 'FF' = FINSTAT                        
         XC    APWORK,APWORK                                                    
         MVC   APWORK(L'FINSTAT),FINSTAT                                        
         B     VS014                                                            
*                                                                               
VS012    MVI   SELSYS,GEKSDEQ      SYSTEM 'FE' = GFTSTAT                        
         XC    APWORK,APWORK                                                    
         MVC   APWORK(L'FINSTAT),GFTSTAT                                        
*                                                                               
VS014    CLI   LSTPERH+5,0         TEST NO PERIOD ENTERED                       
         BNE   VS020                                                            
         MVC   LSTPER(L'ASEDAT),ASEDAT                                          
         MVI   LSTPERH+5,L'ASEDAT  DEFAULT TO TODAY                             
*                                                                               
VS020    MVC   FVMSGNO,=AL2(FVFESYS)                                            
         CLI   SELSYS,MEDQ         CHECK AUTHORIZED                             
         BNE   *+12                                                             
         TM    CUAUTH+1,AUT2EXM                                                 
         BNO   VALSELX                                                          
         CLI   SELSYS,ACCQ                                                      
         BNE   *+12                                                             
         TM    CUAUTH+1,AUT2EXA                                                 
         BNO   VALSELX                                                          
         CLI   SELSYS,GEKSFTQ                                                   
         BE    VS028                                                            
         CLI   SELSYS,GEKSDEQ                                                   
         BNE   VS029                                                            
*                                                                               
VS028    TM    CUAUTH+1,AUT2EXF                                                 
         BNO   VALSELX                                                          
*                                                                               
VS029    MVC   SAVESYS,SELSYS                                                   
         LA    R1,LSTSYSH                                                       
         BAS   RE,DISPFL           REDISPLAY                                    
*                                                                               
VS030    MVC   SAVESYS,SELSYS                                                   
         XC    APWORK,APWORK                                                    
         L     R3,ATYPTAB                                                       
         MVC   MYSYS,SELSYS                                                     
         CLI   MYSYS,GEKSDEQ       FOR HERE SET TO FT                           
         BNE   *+8                                                              
         MVI   MYSYS,GEKSFTQ                                                    
*                                                                               
VS031    CLC   MYSYS,5(R3)         TEST SYSTEM                                  
         BNE   VS032                                                            
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTTYPH       EXCHANGE TYPE                                
*                                                                               
         C     R3,ATYPTAB                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    0,0(R3)             RF=A(KEYWORK)                                
         CLI   FVILEN,0                                                         
         BE    VS033                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RF)     TEST TYPE                                    
         BE    VS033                                                            
VS032    LA    R3,6(R3)                                                         
         CLI   0(R3),0             ALL TYPES TESTED ?                           
         BNE   VS031                                                            
         B     ERRTYPE2            SET FOR INVALID RATE                         
*                                                                               
VS033    MVC   SELTYPE,4(R3)       SET EXCHANGE TYPE                            
         MVC   APWORK(10),0(RF)                                                 
         LA    R1,LSTTYPH                                                       
         BAS   RE,DISPFL           DISPLAY TYPE                                 
*                                                                               
VS040    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTCFRMH      VALIDATE FROM CURRENCY                       
         BE    VS041                                                            
         XC    SELCURF,SELCURF                                                  
         MVC   LSTCFRM(3),FROMDEF                                               
         CLC   FROMDEF,CT@ALL                                                   
         BE    VS050                                                            
         MVC   SELCURF,FROMDEF                                                  
         B     VS050                                                            
VS041    CLC   FVIFLD(3),CT@ALL                                                 
         BE    VS050                                                            
         BAS   RE,VALCURR                                                       
         BNE   VALSELX                                                          
         MVC   SELCURF,FVIFLD                                                   
*                                                                               
VS050    OI    LSTCFRMH+FVOIND-FVIHDR,FVOXMT                                    
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTCRTOH      VALIDATE TO CURRENCY                         
         BE    VS051                                                            
         XC    SELCURT,SELCURT                                                  
         MVC   LSTCRTO(3),TODEF                                                 
         CLC   TODEF,CT@ALL                                                     
         BE    VS060                                                            
         MVC   SELCURT,TODEF                                                    
         B     VS060                                                            
VS051    CLC   FVIFLD(3),CT@ALL                                                 
         BE    VS060                                                            
         BAS   RE,VALCURR                                                       
         BNE   VALSELX                                                          
         MVC   SELCURT,FVIFLD                                                   
*                                                                               
VS060    OI    LSTCRTOH+FVOIND-FVIHDR,FVOXMT                                    
*        OI    LSTHDRH+FVOIND-FVIHDR,FVOXMT                                     
         CLI   SELSYS,GEKSFTQ                                                   
         BE    VS067              SKIP FOR FINSTAT                              
         CLI   SELSYS,GEKSDEQ                                                   
         BE    VS067              SKIP FOR GFTSTAT                              
*                                                                               
         MVI   FVMINL,1                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         GOTO1 AFVAL,LSTCLIH                                                    
         BNE   VS061               NO INP SET DEFAULT                           
*                                                                               
         SR    R1,R1               CLIENT DATA INPUT                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),CT@DFALT  CHECK FOR DEFAULT                            
         BNE   VS062                                                            
*                                                                               
VS061    MVC   LSTCLI,CT@DFALT     MED OR ACC - DEFAULT CLIENT                  
         OI    LSTCLIH+6,X'80'                                                  
         MVC   LSTCLIN,SPACES                                                   
         OI    LSTCLINH+FVOIND-FVIHDR,FVOXMT                                    
         CLI   SELSYS,MEDQ         MEDIA DEFAULTS                               
         BNE   *+10                                                             
         MVC   SELKKEY(6),FFS                                                   
         CLI   SELSYS,ACCQ         ACCOUNT DEFAULTS                             
         BNE   *+10                                                             
         MVC   SELKKEY(5),FFS                                                   
         B     VS070                                                            
*                                                                               
VS062    CLC   FVIFLD(3),CT@ALL    CHECK FOR ALL                                
         BE    VS067                                                            
         CLI   SELSYS,ACCQ         TEST ACCOUNTING CLIENT                       
         BE    VS065                                                            
         CLI   FVILEN,3            TEST FOR 3 CHR CLI CODE                      
         BNH   VS064                                                            
         CLI   FVILEN,5                                                         
         BNE   ERRCLI              MUST BE 5 CHR                                
         MVC   APFULL,FVIFLD+1                                                  
         NC    APFULL,NUMERIC                                                   
         CLC   APFULL,NUMERIC                                                   
         BNE   VALSELX             MUST BE ANNNN                                
         MVC   SELCLI(1),FVIFLD                                                 
         PACK  APDUB,FVIFLD+1(4)                                                
         CVB   R1,APDUB                                                         
         STCM  R1,3,SELCLI+1       FORMAT TO 1CHR+PL4                           
         B     VS064A                                                           
*                                                                               
VS064    MVC   SELCLI,FVIFLD       SET MEDIA CLIENT FIELD                       
VS064A   MVC   GCLICODE,SELCLI                                                  
         BAS   RE,MEDCLIC                                                       
         B     VS069                                                            
VS065    MVC   SELACT,FVIFLD       SET ACC CLIENT FIELD                         
         MVC   GCLILEN,FVILEN                                                   
         MVC   GCLICODE(5),SELACT                                               
         GOTO1 AACCCLI                                                          
         B     VS069                                                            
*                                                                               
VS067    MVC   LSTCLIN,SPACES                                                   
         OI    LSTCLIH+FVOIND-FVIHDR,FVOXMT                                     
         OI    LSTCLINH+FVOIND-FVIHDR,FVOXMT                                    
         CLI   SELSYS,MEDQ                                                      
         BNE   *+14                                                             
         XC    SELCLI,SELCLI                                                    
         B     VS070                                                            
         CLI   SELSYS,ACCQ                                                      
         BNE   VS067A                                                           
         XC    SELACT,SELACT                                                    
         B     VS070                                                            
*                                                                               
VS067A   CLI   SELSYS,GEKSFTQ                                                   
         BNE   VS068                                                            
         MVC   LSTCLI,SPACES                                                    
         B     VS070                                                            
*                                                                               
VS068    CLI   SELSYS,GEKSDEQ                                                   
         BNE   VS069                                                            
         MVC   LSTCLI,SPACES                                                    
         B     VS070                                                            
*                                                                               
VS069    MVC   LSTCLIN,GCLINAME                                                 
         OI    LSTCLIH+FVOIND-FVIHDR,FVOXMT                                     
         OI    LSTCLINH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
VS070    MVC   SELPEND,FFS                                                      
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTPERH                                                    
         BNE   VS080                                                            
         XC    SELPEND,SELPEND                                                  
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         SHI   R1,3                                                             
         BM    VS075                                                            
         LA    R1,FVIFLD(R1)                                                    
         CLC   0(3,R1),CT@UFN      TEST FOR UFN                                 
         BNE   VS075                                                            
         MVC   SELPEND,FFS                                                      
         MVC   0(3,R1),SPACES                                                   
VS075    MVC   APBYTE,CULANG                                                    
         OI    APBYTE,X'20'                                                     
         GOTO1 VPERVAL,APPARM,(FVILEN,FVIFLD),(APBYTE,SCWORK)                   
         CLI   4(R1),X'04'                                                      
         BNE   *+10                                                             
         MVC   SCWORK+36(2),SCWORK+34                                           
         NI    4(R1),(X'FF'-X'04') ONE DATE IS OK                               
         CLI   4(R1),X'00'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFINVDT) INVALID DATE                              
         B     VALSELX                                                          
*                                                                               
         MVC   SELPSTA,SCWORK+34                                                
         CLC   SELPEND,FFS                                                      
         BE    *+10                                                             
         MVC   SELPEND,SCWORK+36                                                
*                                                                               
         MVC   APWORK,SPACES                                                    
         MVC   APWORK(17),SCWORK+11                                             
         CLC   SELPEND,FFS                                                      
         BNE   *+14                                                             
         MVI   APWORK+7,C'-'                                                    
         MVC   APWORK+8(8),CT@UFN                                               
         LA    R1,LSTPERH                                                       
         BAS   RE,DISPFL                                                        
*                                                                               
VS080    CLI   SELSYS,GEKSFTQ      TEST FINSTAT                                 
         BE    VS082                                                            
         CLI   SELSYS,GEKSDEQ      TEST GFTSTAT                                 
         BNE   *+10                                                             
*                                                                               
VS082    XC    SELAGY,SELAGY       FINSTAT/GFTSTAT = AGY ZERO                   
         CR    RB,RB               SET CC EQU FOR GOOD EXIT                     
         B     VALKEYX                                                          
*                                                                               
ERRTYPE2 LA    R1,160              SET ERROR MESS FOR INVALID TYPE              
         XC    APPARM(32),APPARM                                                
         LA    RF,APPARM                                                        
         MVI   8(RF),C'E'                                                       
         MVI   21(RF),X'0A'                                                     
         STH   R1,2(RF)                                                         
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
*                                                                               
VALSELX  LTR   RB,RB               SET CC=NEQ                                   
*                                                                               
VALKEYX  XMOD1                                                                  
*                                                                               
VALCURR  NTR1                                                                   
         MVC   KEYSAVE,IOKEY                                                    
         LA    R2,IOKEY                                                         
         USING GCURD,R2                                                         
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ      RECORD TYPE CURRENCY                         
         MVC   GCKCURR,FVIFLD                                                   
         CLC   GCKCURR(3),FFS                                                   
         BE    VALCURRD                                                         
         LA    R1,IORDD+IOGENDIR+IO2                                            
         GOTO1 AIO                 READ CURRENCY RECORD                         
         BNE   VALCURRN                                                         
         GOTO1 AIO,IOGENFIL+IOGET+IO2                                           
         BE    VALCURRX                                                         
         DC    H'0'                RECORD MUST EXIST                            
VALCURRN B     ERCURR1             CURRENCY NOT FOUND                           
*                                                                               
VALCURRD L     R1,AIOAREA2                                                      
         MVC   0(L'ALLCURN,R1),ALLCURN                                          
*                                                                               
VALCURRX MVC   IOKEY,KEYSAVE       RESTORE OLD KEY VALUE                        
         CR    RB,RB                                                            
         B     VALKEYX                                                          
         DROP  R2                                                               
         LTORG                                                                  
**********************************************************************          
*   REDISPLAY FIELDS IF REQUIRED                                     *          
**********************************************************************          
DISPFL   ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SHI   RF,L'FVIHDR                                                      
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SHI   RF,L'FVIHDR        KNOCK OFF HEADER EXTENSION                    
         BCTR  RF,0                                                             
         EX    RF,DISPFLDA         COMPARE FIELD CONTENTS                       
         BER   RE                                                               
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDB         MOVE IN NEW FIELD                            
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDA CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDB MVC   L'FVIHDR(0,R1),APWORK                                            
*                                                                               
MEDCLIC  NTR1                                                                   
*&&UK                                                                           
         CLC   GCLIOLD,GCLICODE    SAME CALL AS BEFORE?                         
         BE    VALKEYX             DON'T WASTE MY TIME                          
         MVC   GCLIOLD,GCLICODE                                                 
         MVC   GCLINAME,SPACES                                                  
*                                                                               
         MVC   KEYSAVE,IOKEY       SAVE CURRENT KEY                             
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         L     R9,AIOAREA2                                                      
         USING DCLI,R2                                                          
         MVC   CLIKAM,MEDAGY       SET AGY AND FIRST MEDIA                      
         MVI   CLIKTYP,CLIKTYPQ    SET CLIENT RECORD TYPE                       
         MVC   CLIKCLI,GCLICODE                                                 
*                                                                               
MEDCLCRD OI    IOINDS1,IOISWAUT    SET AUTO SWITCHING                           
         OI    APINDS,APILRERD     FLAG RDSEQ BROKEN                            
         GOTO1 AIO,IOCONMDR+IOHI+IO2                                            
         TM    IOERR,255-IOEEOF                                                 
         BNZ   MEDCLCEX                                                         
         TM    IOERR,IOEEOF                                                     
         BO    MEDCLCNO                                                         
         CLC   CLIKEY(5),IOKEYSAV  TEST AM/C/CLI                                
         BE    MEDCLCFO            GOT IT                                       
         MVC   APBYTE,IOKEY                                                     
         NI    APBYTE,X'F0'        ISOLATE AGY OF REC READ                      
         CLC   APBYTE,MEDAGY       IS IT OUR AGENCY                             
         BNE   MEDCLCNO            NO - FORGET IT                               
*                                                                               
         CLC   CLIKTYP(4),IOKEYSAV+1 TEST C/CLI                                 
         BE    MEDCLCFO            EQU - WE GOT IT                              
         BL    MEDCLCNX            LOW - SET C/CLI & READ HI                    
*                                                                               
         TM    IOKEY,X'0F'         HIGH - BUMP MEDIA                            
         BO    MEDCLCNO            UNLESS WE ARE ALREADY ON MEDIA F             
         SR    R1,R1                                                            
         IC    R1,CLIKAM           BUMP MEDIA                                   
         LA    R1,1(R1)                                                         
         STC   R1,CLIKAM                                                        
*                                                                               
MEDCLCNX MVC   IOKEY+1(L'IOKEY-1),IOKEYSAV+1                                    
         B     MEDCLCRD                                                         
*                                                                               
MEDCLCFO OI    IOINDS1,IOISWAUT    GET CLIENT RECORD                            
         GOTO1 AIO,IOGET+IOCONMFL+IO2                                           
         BNE   MEDCLCEX                                                         
         LR    R2,R9                                                            
         MVC   GCLINAME,SPACES                                                  
         MVC   GCLINAME(L'CLISH),CLISH  SAVE SHORT NAME                         
         B     MEDCLCOK                                                         
*                                                                               
MEDCLCEX MVC   GCLINAME+0(2),=CL2'**'   ERROR ON CLIENT READ                    
         MVCDD GCLINAME+2(7),CT#ERROR                                           
         MVC   GCLINAME+9(2),=CL2'**'                                           
         B     MEDCLCNZ                                                         
*                                                                               
MEDCLCNO MVC   GCLINAME+0(2),=CL2'**' CLIENT NOT ON FILE                        
         MVCDD GCLINAME+2(7),CT#UNKNW                                           
         MVC   GCLINAME+9(2),=CL2'**' CLIENT NOT ON FILE                        
*                                                                               
MEDCLCNZ MVC   IOKEY,KEYSAVE                                                    
         LTR   RB,RB                                                            
         B     VALSELX                                                          
*                                                                               
MEDCLCOK MVC   IOKEY,KEYSAVE                                                    
         CR    RB,RB               SET CC OK                                    
*&&                                                                             
         B     VALKEYX                                                          
*                                                                               
ACCCLIC  NTR1                                                                   
         CLC   GCLIOLD,GCLICODE    SAME CALL AS BEFORE?                         
         BE    VALKEYX             DON'T WASTE MY TIME                          
         MVC   GCLIOLD,GCLICODE                                                 
         MVC   GCLINAME,SPACES                                                  
*                                                                               
         MVC   KEYSAVE,IOKEY       SAVE CURRENT KEY                             
         MVC   IOKEY,SPACES                                                     
         LA    R2,IOKEY                                                         
         L     R9,AIOAREA2                                                      
         OI    IOINDS1,IOISWAUT    SET AUTO SWITCHING                           
         OI    APINDS,APILRERD     FLAG RDSEQ BROKEN                            
         CLI   GCLILEN,0           IGNORE LEN CHECK IF ZERO                     
         BE    ACCLI1                                                           
         USING LDGRECD,R2                                                       
         MVC   LDGKCPY,ACCAGY      SET COMPANY CODE                             
         MVI   LDGKUNT,C'S'                                                     
         MVI   LDGKLDG,C'J'                                                     
*                                                                               
         GOTO1 AIO,IOCONACD+IORD+IO2                                            
         BNZ   ACCLIEX                                                          
         GOTO1 AIO,IOCONACM+IOGET+IO2                                           
         BNZ   ACCLIEX                                                          
         MVI   APELEM,ACLELQ       FIND ACCOUNT LENGTHS ELEMENT                 
         L     R0,AIOAREA2                                                      
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),(APELEM,(R0)),0                        
         CLI   12(R1),0                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
         USING ACLELD,R3                                                        
         CLC   GCLILEN,ACLVLEN     CHECK INPUT LENGTH                           
         BH    ACCLINO                                                          
*                                                                               
         USING ACTRECD,R2          READ ACCOUNT RECORD                          
ACCLI1   MVC   IOKEY,SPACES                                                     
         MVC   ACTKCPY,ACCAGY      SET COMPANY CODE                             
         MVI   ACTKUNT,C'S'                                                     
         MVI   ACTKLDG,C'J'                                                     
         MVC   ACTKACT(5),GCLICODE CLIENT CODE                                  
*                                                                               
         GOTO1 AIO,IOCONACD+IORD+IO2                                            
         TM    IOERR,255-IOERNF                                                 
         BNZ   ACCLIEX            HIDEOUS ERROR                                 
         TM    IOERR,IOERNF                                                     
         BNZ   ACCLINO            NOT FOUND                                     
         GOTO1 AIO,IOCONACM+IOGET+IO2                                           
         BNZ   ACCLIEX            HIDEOUS ERROR                                 
*                                                                               
         MVI   APELEM,SNMELQ       FIND SHRT NAME ELEMENT                       
         L     R0,AIOAREA2                                                      
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),(APELEM,(R0)),0                        
         CLI   12(R1),0                                                         
         BNZ   ACCLI2             NOT FOUND                                     
         L     R3,12(R1)                                                        
         USING SNMELD,R3                                                        
         MVC   GCLINAME,SPACES                                                  
         MVC   GCLINAME(L'SNMNAME),SNMNAME                                      
         B     ACCLIOK                                                          
*                                                                               
ACCLI2   MVI   APELEM,NAMELQ       FIND NAME ELEMENT                            
         L     R0,AIOAREA2                                                      
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),(APELEM,(R0)),0                        
         CLI   12(R1),0                                                         
         BNZ   ACCLINO            NOT FOUND                                     
         L     R3,12(R1)                                                        
         USING NAMELD,R3                                                        
         MVC   GCLINAME,SPACES     EX-MOVE NAME IN TO APWORK                    
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         CLM   R1,1,=AL1(16)       17 CHRS MAX                                  
         BL    *+8                                                              
         LA    R1,16                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GCLINAME(0),NAMEREC                                              
         B     ACCLIOK                                                          
*                                                                               
ACCLIEX  MVC   GCLINAME+0(2),=CL2' *'   ERROR ON CLIENT READ                    
         MVCDD GCLINAME+2(7),CT#ERROR                                           
         MVC   GCLINAME+9(2),=CL2' *' CLIENT NOT ON FILE                        
         B     ACCLINX                                                          
*                                                                               
ACCLINO  MVC   GCLINAME+0(2),=CL2'**'   CLIENT NOT ON FILE                      
         MVCDD GCLINAME+2(7),CT#UNKNW                                           
         MVC   GCLINAME+9(2),=CL2'**' CLIENT NOT ON FILE                        
ACCLINX  MVC   IOKEY,KEYSAVE                                                    
         LTR   RB,RB                                                            
         B     VALKEYX                                                          
*                                                                               
ACCLIOK  MVC   IOKEY,KEYSAVE                                                    
         CR    RB,RB                                                            
         B     VALKEYX                                                          
*                                                                               
ERRCLI   LA    R1,161              INVALID CLIENT CODE                          
         B     SETCER                                                           
*                                                                               
ERCURR1  LA    R1,159                                                           
         B     SETCER                                                           
*                                                                               
SETCER   XC    APPARM(32),APPARM   SET UP GETTXT BLOCK                          
         LA    RF,APPARM                                                        
         MVI   8(RF),C'E'          MESSAGE TYPE                                 
         MVI   21(RF),X'0A'        CONTROL MSG                                  
         STH   R1,2(RF)            MESSAGE NUMBER                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         LTR   RB,RB                                                            
         B     VALSELX                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
*********************************************************************           
* NUMCHEK - CHECK THAT EXPRESSIONS WILL FIT INTO 10 CHARACTERS      *           
*         - ON ENTRY, R1 POINTS TO RATE TO/FROM INPUT FIELD         *           
*           AND HALF1+1 CONTAINS THE MAX. NO OF CHARS ALLOWED       *           
*           FOR THE EXPRESSION (AFTER NON-SIGNIFICANT FIGURES       *           
*           HAVE BEEN REMOVED)                                      *           
*********************************************************************           
NUMCHEK  NMOD1 0,**NUMC**                                                       
         ICM   RC,15,0(R1)         RESTORE W/S                                  
         USING LOCALD,RC                                                        
         ICM   R1,15,NUMBER        GET A(RATE EXPRESSION)                       
         LA    RE,8                8 CHARACTERS TO CHECK                        
         XR    RF,RF               RF = CHARACTER COUNT                         
*                                                                               
NUMP00   EQU   *                   POSIT INTEGER                                
         CLI   0(R1),C'.'                                                       
         BE    NUMA00              QUIT IF DECIMAL POINT FOUND                  
         CLI   0(R1),C','                                                       
         BE    NUMA00                                                           
         CLI   0(R1),C' '          MIGHT BE LEADING SPACES                      
         BH    NUMP10                                                           
         LA    R1,1(R1)                                                         
         BCT   RE,NUMP00                                                        
         B     NUMP00X                                                          
*                                                                               
NUMP10   EQU   *                                                                
         CLI   0(R1),C'0'                                                       
         BH    NUMP20                                                           
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    NUMOK                                                            
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'.'                                                       
         BE    NUMA00              QUIT IF DECIMAL POINT FOUND                  
         CLI   0(R1),C','                                                       
         BE    NUMA00                                                           
         CLI   0(R1),C' '          MIGHT BE LEADING SPACES                      
         BNH   NUMP00X             END OF STRING                                
         LA    R1,1(R1)                                                         
         BCT   RE,NUMP10                                                        
         B     NUMP00X                                                          
*                                                                               
NUMP20   BCTR  RE,0                                                             
*                                                                               
NUMP25   LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'.'                                                       
         BE    NUMA00              QUIT IF DECIMAL POINT FOUND                  
         CLI   0(R1),C','                                                       
         BE    NUMA00                                                           
         CLI   0(R1),C' '          MIGHT BE SPACES                              
         BNH   NUMP00X             END OF STRING                                
         BCT   RE,NUMP25                                                        
         B     NUMP00X                                                          
*                                                                               
NUMP00X  STC   RF,HALF1                                                         
         CLC   HALF1(1),HALF1+1                                                 
         BNH   NUMOK                                                            
         B     NUMBAD              NUMBER EXCEEDS MAXIMUM ALLOWED               
*                                                                               
NUMA00   EQU   *                   ADMIT DECIMAL                                
         LA    RF,1(RF)            ADD ONE FOR DECIMAL POINT                    
         LA    R1,1(R1)            NEXT CHARACTER                               
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    NUMA00X                                                          
         CLI   0(R1),C' '          END OF STRING?                               
         BNH   NUMA00X                                                          
         XR    R8,R8               R8 COUNTS PLACES AFTER DECIMAL POINT         
         ICM   R1,15,NUMBER        POINT BACK TO START OF RATE                  
         LA    R1,6(R1)            POINT R1 TO END OF STRING                    
         LA    RE,6                                                             
*                                                                               
NUMA10   CLI   0(R1),C'.'                                                       
         BE    NUMA00X                                                          
         CLI   0(R1),C','                                                       
         BE    NUMA00X                                                          
         CLI   0(R1),C'0'                                                       
         BH    NUMA20                                                           
         BCTR  R1,0                                                             
         BCT   RE,NUMA10                                                        
         B     NUMA00X                                                          
*                                                                               
NUMA20   EQU   *                                                                
         LA    R8,1(R8)            ADD ONE TO DECIMAL PLACE COUNT               
         BCTR  R1,0                BACK A CHARACTER                             
         CLI   0(R1),C','          GERMAN DECIMAL POINT?                        
         BE    NUMA20X                                                          
         CLI   0(R1),C'.'          UK DECIMAL POINT?                            
         BE    NUMA20X                                                          
         BCT   RE,NUMA20                                                        
*                                                                               
NUMA20X  EQU   *                                                                
         AR    RF,R8               ADD PRE+POST DECIMAL COUNTS                  
*                                                                               
NUMA00X  EQU   *                   END OF DECIMAL VALIDATION                    
         STC   RF,HALF1                                                         
         CLC   HALF1(1),HALF1+1                                                 
         BH    NUMBAD                                                           
*                                                                               
NUMOK    CR    RB,RB               SET CC OK FOR GOOD EXIT                      
         B     NUMCHEKX                                                         
*                                                                               
NUMBAD   LTR   RB,RB               SET CC NEQ FOR BAD NUMBER                    
*                                                                               
NUMCHEKX XMOD1                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* GEGENEXC                                                                      
       ++INCLUDE GEGENEXC                                                       
* GEGENCUR                                                                      
       ++INCLUDE GEGENCUR                                                       
* DDCTRYEQUS                                                                    
       ++INCLUDE DDCTRYEQUS                                                     
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENF0D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENB0D                                                       
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        LINE DSECT                                         *                   
*************************************************************                   
         SPACE 1                                                                
EXLINED  DSECT                                                                  
EXLSELH  DS    CL8                                                              
EXLSEL   DS    CL3                 SEL                                          
EXLCURH  DS    CL8                                                              
EXLCUR   DS    CL3                 CURRENCY                                     
EXLCURTH DS    CL8                                                              
EXLCURT  DS    CL3                 CURRENCY                                     
EXLCLIH  DS    CL8                                                              
EXLCLI   DS    CL5                 CLIENT                                       
EXLNAMH  DS    CL8                                                              
EXLNAM   DS    CL17                NAME                                         
EXLRTFH  DS    CL8                                                              
EXLRTF   DS    CL10                RATE1                                        
EXLRTTH  DS    CL8                                                              
EXLRTT   DS    CL8                 RATE2                                        
EXLPERH  DS    CL8                                                              
EXLPER   DS    CL8                 PERIOD                                       
EXLLEN   EQU   *-EXLINED                                                        
         EJECT                                                                  
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
PRTLIN   DS    0CL(L'REPP1)                                                     
         DS    CL1                                                              
PRTSYS   DS    CL7                 SYSTEM NAME                                  
         DS    CL3                                                              
PRTCURF  DS    CL3                 CURRENCY FROM                                
         DS    CL3                                                              
PRTCURT  DS    CL3                 CURRENCY TO                                  
         DS    CL3                                                              
PRTTYPE  DS    CL10                RATE TYPE                                    
         DS    CL3                                                              
PRTCLI   DS    CL5                 CLIENT CODE                                  
         DS    CL2                                                              
PRTCLIN  DS    CL20                CLIENT SHORT NAME                            
         DS    CL2                                                              
PRTRATF  DS    CL10                RATE FROM                                    
PRTTO    DS    CL4                                                              
PRTRATT  DS    CL10                RATE TO                                      
         DS    CL1                                                              
PRTPER   DS    CL17                PERIOD                                       
         DS    CL1                                                              
         ORG   PRTLIN+L'PRTLIN                                                  
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
SELKEY   DS    0XL44                                                            
         DS    CL3                                                              
SELAGY   DS    CL2                                                              
SELSYS   DS    CL1                 EXCHANGE SYSTEM                              
SELCURF  DS    CL3                 CURRENCY FROM                                
SELCURT  DS    CL3                 CURRENCY TO                                  
SELTYPE  DS    CL1                 EXCHANGE TYPE                                
*                                                                               
SELKKEY  DS    0CL14               MEDIA VALUES                                 
SELMED   DS    CL1                                                              
SELCLI   DS    CL3                                                              
SELPRO   DS    CL1                                                              
SELCAM   DS    CL1                                                              
         DS    CL8                                                              
         ORG   SELKKEY             ACCOUNTING VALUES                            
SELACT   DS    CL5                                                              
         DS    CL9                                                              
*                                                                               
SELPSTA  DS    CL2                 START DATE                                   
SELPEND  DS    CL2                 END DATE                                     
         ORG   SELKEY+L'SELKEY                                                  
*                                                                               
KEYSAVE  DS    CL44                TEMP STORAGE FOR IOKEY                       
REPKEYS  DS    CL27                SAVED KEY FOR BOX CALCULATION                
DMCB     DS    6F                                                               
DUB      DS    D                                                                
ADDR     DS    F                                                                
SAVERE   DS    F                                                                
ASAVRECS DS    A                                                                
NUMBER   DS    CL4                 SAVED ADDRESS OF RATE EXPRESSION             
HALF1    DS    CL2                 WORK AREA FOR NUMCHEK                        
OLDINFO  DS    CL5                                                              
RATEFRM  DS    PL6                                                              
RATETO   DS    PL6                                                              
XRATE    DS    CL5                                                              
XSHFT    DS    CL5                                                              
XSTAT    DS    CL1                                                              
CURF     DS    CL3                                                              
CURT     DS    CL3                                                              
TEMP     DS    CL64                                                             
*                                                                               
MYSYS    DS    XL1                                                              
*                                                                               
GCLILEN  DS    CL1                 GETCLI WORK AREAS                            
GCLICODE DS    CL5                                                              
GCLIOLD  DS    CL5                                                              
GCLINAME DS    CL20                                                             
*                                                                               
RECBUFF  DS    19CL(GEFIRST+GEXELLQ+GIXELLQ)                                    
RECBUFL  DS    1CL(GEFIRST+GEXELLQ+GIXELLQ)                                     
RECBUFFX DS    0CL1                                                             
RECCOUNT DS    H                                                                
*                                                                               
SPACES   DS    CL80                                                             
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SAVOVER                                                          
MEDSE    DS    X                   SAVE SYSTEM VALUES                           
MEDAGY   DS    X                                                                
ACCSE    DS    X                                                                
ACCAGY   DS    X                                                                
AGYCUR   DS    CL3                                                              
ONEVAL   DS    0CL3                1.0 OR 1,0                                   
         DS    CL1                 1                                            
CURSEP   DS    CL1                 ./,                                          
         DS    CL1                 0                                            
TODEF    DS    CL3                 DEFAULT TO CUR                               
FROMDEF  DS    CL3                 DEFAULT FROM CUR                             
*                                                                               
TENVAL   DS    CL4                 10.0 OR 10,0                                 
HUNVAL   DS    CL5                 100.0 OR 100,0                               
THOUVAL  DS    CL6                 1000.0 OR 1000,0                             
*                                                                               
AVALKEY  DS    A                   RELOC ADDRESS FOR VALKEY ROUTINE             
ANUMCHEK DS    A                   RELOC ADDRESS FOR NUMCHEK ROUTINE            
ATYPTAB  DS    A                   RELOC ADDRESS FOR TYPTAB TABLE               
AACCCLI  DS    A                   RELOC ADDRESS FOR ACCCLI ROUTINE             
*                                                                               
SAVESEL  DS    CL44                SAVED SELECT KEY                             
SAVEKEY  DS    CL44                SAVED LAST KEY VALUE                         
CHNGKEY  DS    CL44                SAVED KEY FOR CHANGE                         
MODE     DS    X                                                                
SAVEACT  DS    X                   PREVIOUS ACTION                              
*                                                                               
SAVESYS  DS    C                   LAST VALUE FOR SYSTEM                        
DSFRWD   DS    0CL10                                                            
DS@1MNTH DS    CL10                1 MONTH                                      
DS@3MNTH DS    CL10                3 MONTH                                      
DS@6MNTH DS    CL10                6 MONTH                                      
DS@1MT   DS    CL10                1MONTH                                       
DS@3MT   DS    CL10                3MONTH                                       
DS@6MT   DS    CL10                6MONTH                                       
INITFLG  DS    C                                                                
DDDSLST  DS    0C                                                               
         DSDDL PRINT=YES                                                        
         ORG   SAVAREAX                                                         
SAVERECS DS    14XL100             SAVE AREA FOR ON-SCREEN RECORDS              
*                                                                               
         EJECT                                                                  
*MEFILCLID                                                                      
*ACGENFILE                                                                      
*FAUTL                                                                          
         PRINT OFF                                                              
*&&UK                                                                           
       ++INCLUDE MEFILCLID                                                      
*&&                                                                             
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072CTGEN0F   10/25/07'                                      
         END                                                                    
