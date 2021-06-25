*          DATA SET CTGEN1B    AT LEVEL 014 AS OF 05/01/02                      
*PHASE TA0B1BA                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE NUMVAL                                                                 
*INCLUDE DLFLD                                                                  
         TITLE 'CTGEN1B - SYSLIST RECORD MAINTENANCE'                           
GEN1B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GE1B**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTWREC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         L     R1,ACOM                                                          
         L     R1,CXSORT-COMFACSD(R1)                                           
         ST    R1,VXSORT                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
*        SAVE ADDRESSES OF COMMON ROUTINES                                      
*                                                                               
         L     R1,=A(VCOMMON)      R1 = A(VCOMMON)                              
         A     R1,APRELO                                                        
         SR    RF,RF               RF = 0                                       
         LA    RE,COMMADRS         RE = A(FIRST ROUTINE ADDRESS)                
         LA    R0,VCOUNT           R0 = NUMBER OF CONTROLLER ROUTINES           
*                                                                               
         LTR   R0,R0               IF NO CONTROLLER ROUTINES THEN DONE          
         BZ    GEN1B020                                                         
*                                                                               
GEN1B010 ST    R1,0(RE)            SAVE A(VCOMMON) IN LAST 3 BYTES              
         STC   RF,0(RE)            SAVE ROUTINE NUMBER IN FIRST BYTE            
*                                                                               
         LA    RF,1(RF)            BUMP ROUTINE NUMBER                          
         LA    RE,4(RE)            BUMP TO NEXT ROUTINE ADDRESS                 
         BCT   R0,GEN1B010         LOOP BACK                                    
*                                                                               
GEN1B020 EQU   *                                                                
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              APMVALK                                      
         B     VALREC              APMVALR                                      
         B     DISKEY              APMDISK                                      
         B     DISREC              APMDISR                                      
         B     DELREC              APMDELR                                      
         B     RESREC              APMRESR                                      
         B     VALSEL              APMVALP                                      
         B     GETSEL              APMGETS                                      
         B     DISSEL              APMDISS                                      
         B     EXIT                APMVALS                                      
         B     EXIT                APMFLST                                      
         B     EXIT                APMPROC                                      
         B     EXIT                APMFSCR                                      
         B     LSTSCR              APMLSCR                                      
         B     VALREQ              APMVALQ                                      
         B     PRTREP              APMREPP                                      
         B     EXIT                APMSETT                                      
         B     EXIT                APMPUTK                                      
         B     EXIT                APMNEWK                                      
         B     EXIT                APMFRP                                       
         B     EXIT                APMDISS2                                     
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF SYSLIST RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
*                                  VALIDATE LIST TYPE                           
         MVI   FVMINL,1                                                         
         GOTO1 VALLTYP,APPARM,SYLLTYPH,CTWKREC                                  
         BNE   VALKEYX                                                          
         L     R8,ATYPNTRY                                                      
         USING TYPTABD,R8                                                       
         CLI   TYPTSYSN,0          TEST IF FACPAK SYSTEM # ENTERED              
         BE    *+10                                                             
         MVC   CTWKSYSN,TYPTSYSN   MOVE SYSTEM # TO KEY FIELD                   
         TM    CUSTAT,CUSDDS       IF NON-DDS THEN IDGROUP ONLY                 
         BNZ   VK010                                                            
         TM    TYPTINDS,TYPTIAGA                                                
         BZ    EIIF                                                             
*                                                                               
VK010    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,SYLIDH        VALIDATE LIST-ID                             
         BNE   VK020                                                            
         CLC   FVILEN,TYPTKMIN                                                  
         BL    EFTS                                                             
         CLC   FVILEN,TYPTKMAX                                                  
         BH    EFTL                                                             
         MVC   CTWKID,FVIFLD                                                    
         TM    TYPTINDS,TYPTIUKY                                                
         BNO   VK020                                                            
         XC    WORK,WORK                                                        
         MVC   WORK,FVIFLD                                                      
         GOTO1 VALUSID                                                          
         BNE   VALKEYX                                                          
         MVC   LISTIDAG,AGAID      SAVE LIST ID AGENCY ALPHA ID                 
*                                                                               
VK020    TM    TYPTINDS,TYPTIAGA   TEST AGENCY ALPHA IN KEY                     
         BZ    VK030                                                            
         MVC   CTWKAGY,AGAID                                                    
         TM    CUSTAT,CUSDDS       IF NON-DDS THEN CONNECT AGENCY MUST          
         BNZ   VK030                 BE SAME AS IDGROUP AGENCY                  
         CLC   CUAALF,AGAID                                                     
         BNE   EIIF                                                             
*                                                                               
VK030    MVC   APRECKEY(L'CTWKEY),CTWKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY                                                          
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
         MVI   APINDS,APIOKADD                                                  
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE PROFILE RECORDS                            *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1         INITIALISE RECORD                            
         L     R8,ATYPNTRY                                                      
         USING TYPTABD,R8                                                       
         XC    CTWREC(256),CTWREC                                               
         LA    R0,CTWDATA+1-CTWREC                                              
         MVC   CTWKEY,APRECKEY                                                  
         STCM  R0,2,CTWLEN                                                      
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,SYLDESCH      BUILD & ADD DESCRIPTION ELEMENT              
         BNE   EMIF                                                             
         USING CTDSCD,R3                                                        
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         MVI   CTDSCEL,CTDSCELQ                                                 
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         EX    R1,*+4                                                           
         MVC   CTDSC(0),FVIFLD                                                  
         LA    R1,CTDSC-CTDSCD(R1)                                              
         STC   R1,CTDSCLEN                                                      
         DROP  R3                                                               
         GOTO1 AADDELN,CTWREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         ZAP   ELS,=P'0'                                                        
         XC    NUMNTRY,NUMNTRY                                                  
         LA    R9,SYLL1H                                                        
DATAV2   CLI   0(R9),0             END OF TWA                                   
         BE    DATAV20                                                          
         GOTO1 AFVAL,(R9)                                                       
         BNE   DATAV18             NOTHING ON THIS LINE                         
         GOTO1 VSCANNER,APPARM,FVIHDR,BLOCK1,C',=,/'                            
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   FLDCNT,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FVINDX,1                                                         
         LA    R4,BLOCK1           R4=A(SCAN BLOCK ENTRY)                       
*                                                                               
DATAV4   CLC   FVINDX,FLDCNT       DONE WITH THIS LINE                          
         BH    DATAV18                                                          
         CLI   0(R4),0             L'FIRST HALF                                 
         BE    EIIF                                                             
         CLI   1(R4),0             L'SECOND HALF                                
         BNE   DATAV6                                                           
         CLC   12(2,R4),=C'L='     CHECK FOR INCLUDE PREFIX                     
         BNE   DATAV6                                                           
*                                                                               
         TM    TYPTINDS,TYPTINON   OPTION TO DISALLOW NESTING                   
         BO    EIRT                                                             
         SR    RE,RE                                                            
         IC    RE,0(R4)                                                         
         SH    RE,=H'2'                                                         
         STC   RE,DUB                                                           
         CLC   DUB(1),TYPTKMIN     CHECK L'INCLUDE KEY                          
         BL    EFTS                                                             
         CLC   DUB(1),TYPTKMAX                                                  
         BH    EFTL                                                             
         BAS   RE,VALLIST          READ INCLUDE RECORD                          
         BNE   EXIT                                                             
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTLIND,R3           BUILD INCLUDE ELEMENT                        
         XC    CTLINEL(20),CTLINEL                                              
         MVI   CTLINEL,CTLINELQ                                                 
         MVI   CTLINLEN,CTLINLNQ                                                
         MVC   CTLINC,14(R4)                                                    
         B     DATAV16                                                          
*                                  BUILD LIST ELEMENT                           
DATAV6   XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTLSTD,R3                                                        
         XC    CTLSTEL(20),CTLSTEL                                              
         MVI   CTLSTEL,CTLSTELQ                                                 
         SR    RE,RE                                                            
         IC    RE,TYPTIMAX         CALCULATE L'ELEMENT                          
         SR    RF,RF                                                            
         IC    RF,TYPTIHEX                                                      
         SR    R1,R1                                                            
         IC    R1,TYPTICHR                                                      
         LA    RE,CTLSTDTA-CTLSTD(RE,RF)                                        
         AR    RE,R1                                                            
         STC   RE,CTLSTLEN                                                      
*                                                                               
         CLI   0(R4),2             L'FIRST HALF                                 
         BL    EIIF                                                             
         IC    RE,0(R4)                                                         
         LA    RF,12(R4)                                                        
         STC   RE,DUB              SET L'INPUT                                  
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   WORK(0),0(RF)       SET V'INPUT                                  
         CLC   DUB(1),TYPTIMIN     CHECK L'DATA                                 
         BL    EFTS                                                             
         CLC   DUB(1),TYPTIMAX                                                  
         BH    EFTL                                                             
         GOTO1 AVALROUT            VALIDATE DATA                                
         BNE   EXIT                                                             
         TM    TYPTINDS,TYPTIUKY                                                
         BNO   *+14                                                             
         CLC   LISTIDAG,AGAID      CHECK USER ID SAME AGECNY AS LIST ID         
         BNE   EIDA                                                             
         CLI   TYPTIHEX,0          HEX REQD                                     
         BE    DATAV8                                                           
         CLI   1(R4),0             YES - ANY INPUT                              
         BE    EMIF                                                             
         TM    1(R4),1                                                          
         BNZ   EFNH                                                             
         TM    3(R4),X'20'                                                      
         BZ    EFNH                                                             
         SR    RE,RE                                                            
         IC    RE,1(R4)            YES - CHECK VALID HEX                        
         SRL   RE,1                                                             
         STC   RE,DUB                                                           
         CLC   DUB(1),TYPTIHEX     CHECK L'HEX INPUT                            
         BL    EFTS                                                             
         BH    EFTL                                                             
         B     DATAV10                                                          
*                                                                               
DATAV8   CLI   1(R4),0                                                          
         BNE   EIIF                                                             
*                                                                               
DATAV10  SR    RE,RE               MOVE DATA TO ELEMENT                         
         SR    RE,RE                                                            
         IC    RE,TYPTIMAX                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   CTLSTDTA(0),WORK                                                 
         LA    R3,CTLSTDTA+1(RE)                                                
         CLI   TYPTIHEX,0                                                       
         BE    DATAV12                                                          
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         GOTO1 VHEXIN,APPARM,22(R4),(R3),(R0)                                   
         SR    RE,RE                                                            
         IC    RE,TYPTIHEX                                                      
         LA    R3,0(RE,R3)                                                      
*                                                                               
DATAV12  SR    RE,RE               MOVE OTHER DATA TO ELEMENT                   
         ICM   RE,1,TYPTICHR                                                    
         BZ    DATAV14                                                          
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R3),OTHRDATA                                                 
*                                                                               
DATAV14  LA    R3,APELEM                                                        
         BAS   RE,DUPCHK           CHECK FOR DUPLICATE LIST ENTRY               
         BNE   EXIT                                                             
*                                                                               
DATAV16  GOTO1 AADDELN,CTWREC      ADD ELEMENT TO RECORD                        
         BNE   VALRECER            RECORD TOO BIG                               
         AP    ELS,=P'1'                                                        
         LA    R4,L'BLOCK1(R4)     BUMP SCAN BLOCK POINTER                      
         IC    RE,FVINDX                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FVINDX                                                        
         B     DATAV4                                                           
*                                                                               
DATAV18  SR    R0,R0               BUMP TO NEXT TWA INPUT LINE                  
         IC    R0,0(R9)                                                         
         AR    R9,R0                                                            
         B     DATAV2                                                           
*                                                                               
DATAV20  LA    R1,SYLL1H                                                        
         ST    R1,FVADDR                                                        
         MVI   FVINDX,0                                                         
         CP    ELS,=P'0'           CHECK FOR ANY INPUT                          
         BE    EMIF                                                             
         LA    R1,SYLIDH                                                        
         ST    R1,FVADDR                                                        
*                                                                               
*                                  UPDATE RECORD                                
         GOTO1 ASETACN,CTWREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         CLI   APACTN,ACTADD                                                    
         BNE   *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  EXIT RECORD VALIDATION AND UPDATE OK         
VALRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC              REDISPLAY RECORD                             
*                                                                               
VALRECER B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST CODE FOR NESTED LIST CALL                             *         
***********************************************************************         
         SPACE 1                                                                
VALLIST  NTR1  ,                                                                
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA2                                                      
         MVC   CTWKEY,KEYSAVE                                                   
         MVC   CTWKID,14(R4)                                                    
         MVC   IOKEY(L'CTWKEY),CTWKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                CHECK RECORD FOUND OK                        
         BH    ERNF                                                             
         LA    R3,CTWDATA                                                       
         SR    R0,R0                                                            
         USING CTLSTD,R3                                                        
VALLIST2 CLI   CTLSTEL,0           TEST E-O-R                                   
         BE    VALLIST6                                                         
         CLI   CTLSTEL,CTLINELQ    NOT MORE THAN 1 NEST LEVEL ALLOWED           
         BE    EIRT                                                             
         CLI   CTLSTEL,CTLSTELQ                                                 
         BNE   VALLIST4                                                         
         BAS   RE,DUPCHK           CHECK FOR DUPLICATE ENTRY                    
         BNE   EXIT                                                             
VALLIST4 IC    R0,CTLSTLEN                                                      
         AR    R3,R0                                                            
         B     VALLIST2                                                         
VALLIST6 EQU   *                                                                
         MVC   IOKEY,KEYSAVE                                                    
         SR    RC,RC                                                            
         LTR   RC,RC               EXIT OK                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK FOR DUPLICATE LIST ENTRY                                      *         
***********************************************************************         
         SPACE 1                                                                
DUPCHK   NTR1  ,                                                                
         USING CTLSTD,R3                                                        
         L     RE,ATIA             CHECK NEW ELEMENT AGAINST PREVIOUS           
         ICM   RF,15,NUMNTRY       RF=NUMBER OF ELEMENTS SO FAR                 
         BZ    DUPCHK4                                                          
*                                                                               
DUPCHK2  SR    R1,R1                                                            
         IC    R1,TYPTIMAX                                                      
         SR    R0,R0                                                            
         TM    TYPTINDS,TYPTIHIN   TEST INCLUDE HEX LENGTH                      
         BZ    *+8                                                              
         IC    R0,TYPTIHEX                                                      
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BE    EDIF                                                             
         CLC   0(0,RE),CTLSTDTA    COMPARE FIRST OR BOTH PARTS OF DATA          
         LA    RE,1(R1,RE)                                                      
*                                                                               
         TM    TYPTINDS,TYPTICHS   TEST FOR SEPARATE HEX CHECK                  
         BZ    DUPCHK3                                                          
         ST    RF,DUB                                                           
         SR    RF,RF                                                            
         IC    RF,TYPTIMAX                                                      
         LA    RF,CTLSTDTA(RF)                                                  
         SR    R1,R1                                                            
         IC    R1,TYPTIHEX                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BE    EDIF                                                             
         CLC   0(0,RE),0(RF)       COMPARE SECOND PART OF DATA(HEX)             
         LA    RE,1(R1,RE)                                                      
         L     RF,DUB                                                           
*                                                                               
DUPCHK3  BCT   RF,DUPCHK2                                                       
*                                                                               
DUPCHK4  SR    R1,R1               ADD NEW ELEMENT                              
         IC    R1,TYPTIMAX                                                      
         SR    R0,R0                                                            
         TM    TYPTINDS,TYPTIHIN                                                
         BNZ   *+12                                                             
         TM    TYPTINDS,TYPTICHS                                                
         BZ    *+8                                                              
         IC    R0,TYPTIHEX         INCLUDE HEX PART                             
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),CTLSTDTA                                                 
         L     RF,NUMNTRY          INCREMENT ELEMENT COUNTER                    
         LA    RF,1(RF)                                                         
         ST    RF,NUMNTRY                                                       
DUPCHKX  SR    RC,RC                                                            
         LTR   RC,RC               EXIT OK                                      
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A SYSLIST RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R2                                                        
DELREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACN,CTWREC                                                   
         BNE   DISRECX             RECORD TOO BIG                               
         OI    CTWSTAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED SYSLIST RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R2                                                        
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACN,CTWREC                                                   
         BNE   RESRECX             RECORD TOO BIG                               
         NI    CTWSTAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RESRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF SYSLIST RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R2                                                        
DISKEY   LA    R2,APRECKEY                                                      
*                                                                               
         GOTO1 DISLTYP,APPARM,SYLLTYP,CTWKREC,CTWKSYSN                          
         OI    SYLLTYPH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
         MVC   SYLID(L'CTWKID),CTWKID                                           
         OI    SYLIDH+FVOIND-FVIHDR,FVOXMT                                      
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SYSLIST RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R2                                                        
DISREC   EQU   *                                                                
         L     R2,AIOAREA1                                                      
         L     R8,ATYPNTRY                                                      
         USING TYPTABD,R8                                                       
         TWAXC SYLDESCH                                                         
         LA    R3,CTWDATA          R3=A(ELEMENT)                                
         USING CTLSTD,R3                                                        
*                                                                               
         TM    TYPTINDS,TYPTISRT   IF PRINTER LIST RECORD                       
         BZ    DISP8               SORT ENTRIES BY LOGICAL PRINTER #            
         SR    R0,R0                                                            
DISP4    CLI   CTLSTEL,CTLSTELQ    FIND FIRST LIST ELEMENT                      
         BE    *+14                                                             
         IC    R0,CTLSTLEN                                                      
         AR    R3,R0                                                            
         B     DISP4                                                            
         ST    R3,APPARM           SAVE POINTER TO FIRST LIST ELEMENT           
         LA    RF,1                                                             
DISP6    IC    R0,CTLSTLEN                                                      
         AR    R3,R0                                                            
         CLI   CTLSTEL,CTLSTELQ                                                 
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         B     DISP6                                                            
*                                                                               
         ST    RF,APPARM+4                                                      
         L     R3,APPARM                                                        
         SR    RF,RF                                                            
         IC    RF,CTLSTLEN                                                      
         LR    R0,RF                                                            
         BCTR  R0,0                                                             
         GOTO1 VXSORT,APPARM,,,(RF),1,(R0)                                      
         LA    R3,CTWDATA                                                       
*                                                                               
DISP8    L     RE,ATIA                                                          
         ST    RE,ANXTNTRY                                                      
         XC    NUMNTRY,NUMNTRY                                                  
         SR    RE,RE                                                            
         IC    RE,TYPTIMAX         CALCULATE LENGTH OF EACH DISPLAY             
         SR    RF,RF               ENTRY                                        
         IC    RF,TYPTIHEX                                                      
         SLL   RF,1                                                             
         LA    RE,1(RE,RF)                                                      
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         LA    RE,1(RE)                                                         
         CH    RE,=H'8'            MINIMUM LENGTH OF 8                          
         BH    *+8                                                              
         LH    RE,=H'8'                                                         
         STC   RE,LENTRY                                                        
*                                                                               
DISP10   CLI   0(R3),0             END OF RECORD                                
         BE    DISP14                                                           
         CLI   0(R3),CTDSCELQ      DESCRIPTION ELEMENT                          
         BE    DISP16                                                           
         CLI   0(R3),CTLSTELQ      LIST ELEMENT                                 
         BE    DISP18                                                           
         CLI   0(R3),CTLINELQ      LIST INCLUDE ELEMENT                         
         BE    DISP20                                                           
*                                                                               
DISP12   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DISP10                                                           
*                                                                               
DISP14   OC    NUMNTRY,NUMNTRY     TEST FOR NO ENTRIES                          
         BZ    DISRECX                                                          
         GOTO1 =V(SCINKEY),APPARM,(18,SYLL1H),(LENTRY,ATIA),NUMNTRY,   *        
               RR=APRELO                                                        
         B     DISRECX                                                          
         EJECT                                                                  
         USING CTDSCD,R3                                                        
DISP16   SR    R1,R1               OUTPUT LIST DESCRIPTION                      
         IC    R1,CTDSCLEN                                                      
         SH    R1,=Y(CTDSC+1-CTDSCD)                                            
         EX    R1,*+4                                                           
         MVC   SYLDESC(0),CTDSC                                                 
         B     DISP12                                                           
*                                                                               
         USING CTLSTD,R3                                                        
DISP18   BAS   RE,GETNTRY          HANDLE LIST ENTRY                            
         SR    RE,RE                                                            
         IC    RE,TYPTIMAX                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R4),CTLSTDTA    MOVE ALPHA DATA                              
         SR    R0,R0                                                            
         ICM   R0,1,TYPTIHEX                                                    
         BZ    DISP12                                                           
         LA    RF,CTLSTDTA+1(RE)   AND HEX IF ANY                               
         LA    R4,0(R4,RE)                                                      
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'/'                                                       
         GOTO1 VHEXOUT,APPARM,(RF),2(R4),(R0),=C'TOG'                           
         B     DISP12                                                           
*                                                                               
         USING CTLIND,R3                                                        
DISP20   BAS   RE,GETNTRY          HANDLE INCLUDE ELEMENT                       
         MVC   0(2,R4),=C'L='                                                   
         MVC   2(L'CTLINC,R4),CTLINC                                            
         B     DISP12                                                           
*                                                                               
DISRECX  GOTO1 ADISACT,CTWREC                                                   
         B     EXIT                                                             
*                                                                               
GETNTRY  L     R4,ANXTNTRY         GET NEXT TIA OUTPUT ENTRY                    
         SR    RF,RF                                                            
         IC    RF,LENTRY                                                        
         LA    RF,0(R4,RF)                                                      
         ST    RF,ANXTNTRY                                                      
         L     RF,NUMNTRY                                                       
         LA    RF,1(RF)                                                         
         ST    RF,NUMNTRY                                                       
         SR    RF,RF                                                            
         IC    RF,LENTRY                                                        
         SH    RF,=H'2'                                                         
         MVI   0(R4),C' '          CLEAR THE ENTRY                              
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   1(0,R4),0(R4)                                                    
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R2                                                        
VALSEL   LA    R2,APRECKEY                                                      
         XC    SELDATA,SELDATA                                                  
*                                                                               
         LA    R4,LSTLTYPH                                                      
         GOTO1 VALPARS             GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
         XC    CTWKEY,CTWKEY       BUILD AN INITIAL KEY                         
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVC   CTWKREC,SELLTYP                                                  
         MVC   CTWKID,SELID                                                     
*                                                                               
         MVI   GETSEQF,0           INTERNAL READ SEQUENCE FLAG                  
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R2                                                        
GETSEL   LA    R2,IOKEY            READ NEXT LIST RECORD                        
         MVC   CTWKEY,APRECKEY       FROM LAST SAVED KEY                        
*                                                                               
         TM    GETSEQF,APILRERD    TEST GETSEL READ SEQUENCE BROKEN             
         BZ    GSEL02                                                           
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     GSEL04                                                           
GSEL02   TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    GSEL10                                                           
         NI    APINDS,X'FF'-APILRERD                                            
GSEL04   GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GSEL20                                                           
         B     GETSELN                                                          
GSEL10   TM    APINDS,APILNSEQ     TEST FIRST LINE IN LIST SEQUENCE             
         BNZ   GSEL20                                                           
*                                                                               
         LA    R1,IOCONFIL+IOHI+IO1                                             
         GOTO1 GETREC                                                           
         BNE   GETSELN                                                          
         B     GETSELY                                                          
GSEL20   LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 GETREC                                                           
         BNE   GETSELN             (EOF)                                        
*                                                                               
GETSELY  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTWKEY),CTWKEY                                        
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R2                                                        
DISSEL   EQU   *                                                                
*                                                                               
         L     R4,APPARM                                                        
         GOTO1 LINE                                                             
*                                                                               
DISSELX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R2                                                        
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
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
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
*                                                                               
         LA    R2,APRECKEY                                                      
         XC    SELDATA,SELDATA                                                  
         LA    R4,REPLTYPH                                                      
         GOTO1 VALPARS             GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
*                                                                               
         XC    CTWKEY,CTWKEY       BUILD AN INITIAL KEY                         
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVC   CTWKREC,SELLTYP                                                  
*                                                                               
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         CLI   OPTDET,C'Y'                                                      
         BE    VREQ100                                                          
         MVI   REPHEADI,REPHSPAC                                                
         MVI   REPMIDSI,REPMSPAC                                                
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         B     VREQ200                                                          
*                                                                               
VREQ100  EQU   *                                                                
         XC    SELUSR,SELUSR                                                    
         OC    OPTUSR,OPTUSR                                                    
         BZ    VREQ120                                                          
         MVC   SELUSR,OPTUSR                                                    
         LA    RE,L'SELUSR                                                      
         LA    RF,SELUSR+L'SELUSR-1                                             
VREQ110  CLI   0(RF),0                                                          
         BNE   VREQ200                                                          
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   RE,VREQ110                                                       
*                                                                               
VREQ120  EQU   *                                                                
         XC    SELAGY,SELAGY                                                    
         OC    OPTAGY,OPTAGY                                                    
         BZ    VREQ200                                                          
         MVC   SELAGY,OPTAGY                                                    
*                                                                               
VREQ200  EQU   *                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT PROFILE LIST                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R2                                                        
PRTREP   EQU   *                                                                
         L     R9,AREP                                                          
         LA    R2,IOKEY                                                         
         CLI   OPTDET,C'Y'                                                      
         BNE   PREC008                                                          
         L     RF,=A(BLDIDT)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF               BUILD USER ID RECORD TABLE                   
*                                                                               
PREC008  MVC   CTWKEY,APRECKEY                                                  
*                                                                               
         LA    R1,IOHI+IOCONFIL+IO1                                             
         GOTO1 GETREC                                                           
         BNE   PRTREPX                                                          
         B     PREC100                                                          
*                                                                               
PREC010  EQU   *                                                                
         TM    GETSEQF,APILRERD    TEST GETSEL READ SEQUENCE BROKEN             
         BZ    PREC020                                                          
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     PREC030                                                          
PREC020  TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    PREC040                                                          
         NI    APINDS,X'FF'-APILRERD                                            
PREC030  LA    R2,IOKEY                                                         
         MVC   CTWKEY,APRECKEY                                                  
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    PREC040                                                          
         B     PRTREPX                                                          
*                                                                               
PREC040  LA    R1,IOSQ+IOCONFIL+IO1                                             
         GOTO1 GETREC              GO GET NEXT REC                              
         BNE   PRTREPX                                                          
*                                                                               
PREC100  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTWKEY),CTWKEY                                        
         CLI   OPTDET,C'Y'                                                      
         BE    PREC200                                                          
         LA    R4,REPP1-14                                                      
         GOTO1 LINE                                                             
*                                                                               
         GOTO1 VREPORT,REPD                                                     
         B     PREC010                                                          
*                                                                               
PREC200  EQU   *                                                                
         L     RF,=A(DETREP)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF               GO PROCESS DETAIL FORMAT REPORT              
         B     PREC010                                                          
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
*                                  GETTXT MESSAGE # ERROR EXITS                 
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EFTL     MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
EFNN     MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
EFTS     MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
EFNH     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EMIF     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
EIIO     MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
EDIF     MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
ERAE     MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
EFNV     MVC   FVMSGNO,=AL2(CE#NOFUN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FUNCTION NOT AVAILABLE                       
ERTB     MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  RECORD TO BIG                                
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE > MAX                            
EIRT     MVC   FVMSGNO,=AL2(CE#INVRE)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  INVALID RECORD TYPE                          
EIDA     MVC   FVMSGNO,=AL2(CE#LIDAG)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  INVALID RECORD TYPE                          
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
REPDESCL DC    C'SYSLIST RECORD LIST'                                           
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'SYSLIST RECORD LIST'                                     
         SPEC  H2,57,C'-------------------'                                     
         SPEC  M1,1,C'     AG  LIST TYPE LIST ID   DESCRIPTION'                 
         SPEC  M2,1,C'     ID  --------- -------   -----------'                 
         SPEC  M1,41,C'                                    '                    
         SPEC  M2,41,C'                                    '                    
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
*                                                                               
TABCLA   DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ012345',X'FF'                        
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    80C' '                                                           
FFILL    DC    80X'FF'                                                          
CAPFILL  DC    (L'APWORK)X'40'                                                  
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
         SPACE 1                                                                
         DROP  RB,RA                                                            
*                                                                               
* ENTRY POINT FOR THE CONMMON ROUTINES                                          
* UPON ENTRY, RF HOLDS A(VCOMMON) IN ITS LOW ORDER THREE                        
* BYTES AND THE ROUTINE NUMBER IN ITS HIGH ORDER BYTE.  VCOMMON WILL            
* USE THE ROUTINE NUMBER TO BRANCH TO THE DESIRED ROUTINE.                      
*                                                                               
VCOMMON  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VCOMMON,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+VCO'    INSERT NAME                                  
*                                                                               
         SRL   RF,24               BRANCH TO DESIRED ROUTINE                    
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
*                                                                               
* TABLE OF BRANCH ADDRESSES TO CONTROLLER ROUTINES                              
*                                                                               
VBRANCH  B     VVALPARS                                                         
         B     VGETREC                                                          
         B     VLINE                                                            
         B     VVALLTYP                                                         
         B     VDISLTYP                                                         
         B     VVALUSID                                                         
         B     VVALSYSN                                                         
         B     VVALNOOP                                                         
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
         SPACE 1                                                                
**********************************************************************          
* VALIDATE I/P PARAMETERS FOR LIST/REPORT                            *          
* R4 = A(FIRST FIELD HEADER IN STANDARD DISPLAY)                     *          
*   APPLICABLE TO BOTH LIST AND REPORT SCREEN FIELD OFFSETS          *          
**********************************************************************          
         SPACE 1                                                                
         USING CTWREC,R2                                                        
         USING LSTLTYPH,R4                                                      
VVALPARS EQU   *                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VPLTYP   EQU   *                   LIST TYPE FILTER                             
         MVI   FVMINL,0                                                         
         GOTO1 VALLTYP,APPARM,LSTLTYPH,SELLTYP                                  
         BNE   VALPARSX                                                         
VPLTYPX  EQU   *                                                                
*                                                                               
VPID     EQU   *                   VALIDATE LIST ID                             
         GOTO1 AFVAL,LSTIDH                                                     
         BNE   VPIDX                                                            
         MVC   SELID,FVIFLD                                                     
VPIDX    EQU   *                                                                
*                                                                               
VPINID   EQU   *                   VALIDATE INCLUDE ID                          
         GOTO1 AFVAL,LSTINIDH                                                   
         BNE   VPINIDX                                                          
         USING CTIREC,R1                                                        
         LA    R1,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,FVIFLD                                                    
         DROP  R1                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    VPINID2                                                          
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALPARSX            ID RECORD NOT FOUND                          
VPINID2  MVC   SELINID,FVIFLD                                                   
VPINIDX  EQU   *                                                                
*                                                                               
         B     VALPARSX                                                         
*                                                                               
VPARNO   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALPARSX XIT1  ,                                                                
         EJECT                                                                  
**********************************************************************          
* GET NEXT RECORD FOR LIST/REPORT, FILTERING ON I/P PARAMETERS       *          
**********************************************************************          
         SPACE 1                                                                
         USING CTWREC,R2                                                        
VGETREC  EQU   *                                                                
*                                                                               
         B     GETRECIO                                                         
*                                                                               
GETRECRD EQU   *                                                                
         TM    GETSEQF,APILRERD    READ NEXT RECORD                             
         BZ    GETRECSQ            CHECK SEQUENCE BROKEN                        
         NI    GETSEQF,X'FF'-APILRERD                                           
         MVC   IOKEY(L'CTPKEY),CTWKEY                                           
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETRECN                                                          
GETRECSQ LA    R1,IOCONFIL+IOSQ+IO1                                             
GETRECIO GOTO1 AIO                                                              
         BNE   GETRECN                                                          
         L     R2,AIOAREA1                                                      
*                                  CHECK STILL CORRECT RECORD TYPE              
         CLC   IOKEYSAV(CTWKAGY-CTWKEY),CTWKEY                                  
         BNE   GETRECN                                                          
         TM    CUSTAT,CUSDDS       NON DDS LIST CONNECT AGENCY                  
         BNZ   GRLTYP                IDGROUP RECORDS ONLY                       
         CLC   CTWKAGY,CUAALF                                                   
         BNE   GETRECRD                                                         
*                                                                               
GRLTYP   EQU   *                   FILTER ON LIST TYPE                          
         OC    SELLTYP,SELLTYP                                                  
         BZ    GRLTYPX                                                          
         CLC   CTWKREC,SELLTYP                                                  
         BNE   GETRECRD                                                         
GRLTYPX  EQU   *                                                                
*                                                                               
GRID     EQU   *                   FILTER ON LIST ID                            
         OC    SELID,SELID                                                      
         BZ    GRIDX                                                            
         CLC   CTWKID,SELID                                                     
         BNE   GETRECRD                                                         
GRIDX    EQU   *                                                                
*                                                                               
GRINID   EQU   *                   FILTER ON INCLUDE ID                         
         OC    SELINID,SELINID                                                  
         BZ    GRINIDX                                                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTLSTELQ     GET LIST ELEMENT                             
         MVI   APELEM+1,1+L'SELINID                                             
         MVI   APELEM+2,0                                                       
         MVC   APELEM+3(L'SELINID),SELINID                                      
         GOTO1 AGETELS,CTWREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
*                                                                               
GRINIDX  EQU   *                                                                
*                                                                               
GETRECY  SR    RC,RC               RETURN CC EQUAL RECORD OK                    
*                                                                               
GETRECN  LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD A LINE OF SYSLIST RECORD DATA                                 *         
***********************************************************************         
         SPACE                                                                  
         USING CTWREC,R2                                                        
VLINE    EQU   *                                                                
         L     R2,AIOAREA1                                                      
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
LNLTYP   EQU   *                                                                
         GOTO1 DISLTYP,APPARM,LISTLTYP,CTWKREC,CTWKSYSN                         
LNLTYPX  EQU   *                                                                
*                                                                               
LNAGY    EQU   *                                                                
         OC    CTWKAGY,CTWKAGY                                                  
         BZ    LNAGYX                                                           
         MVC   LISTAGY,CTWKAGY                                                  
LNAGYX   EQU   *                                                                
*                                                                               
LNID     EQU   *                                                                
         MVC   LISTID,CTWKID                                                    
LNIDX    EQU   *                                                                
*                                                                               
         USING CTDSCD,R3                                                        
LNDSC    EQU   *                                                                
         MVI   APELEM+1,0                                                       
         MVI   APELEM,CTDSCELQ                                                  
         GOTO1 AGETELS,CTWREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNDSCX                                                           
         SR    R1,R1               OUTPUT LIST DESCRIPTION                      
         IC    R1,CTDSCLEN                                                      
         SH    R1,=Y(CTDSC+1-CTDSCD)                                            
         EX    R1,*+4                                                           
         MVC   LISTDSC(0),CTDSC                                                 
LNDSCX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
LINEX    XIT1  ,                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SYSTEM LIST TYPE IN FIELD AND RETURN INTERNAL CODE         *         
***********************************************************************         
         SPACE                                                                  
VVALLTYP EQU   *                                                                
         LM    R3,R4,0(R1)                                                      
         GOTO1 AFVAL,(R3)                                                       
         BL    VCOK                                                             
         BH    VCNO                                                             
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         BCTR  R1,0                                                             
         LA    R8,TYPTAB                                                        
         USING TYPTABD,R8                                                       
*                                                                               
VLTY010  CLI   TYPTABD,TYPTEOTQ    TEST END OF TABLE                            
         BE    VCEIIF                                                           
         EX    R1,*+8                                                           
         BE    VLTY020                                                          
         CLC   TYPTNAME(0),FVIFLD                                               
         LA    R8,TYPTABL(R8)                                                   
         B     VLTY010                                                          
*                                                                               
VLTY020  CLC   TYPTNAME,FVIFLD                                                  
         BE    *+14                                                             
         MVC   L'FVIHDR(L'TYPTNAME,R3),TYPTNAME                                 
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
         SR    RE,RE                                                            
         ICM   RE,3,TYPTROUT       SET UP ROUTINE POINTER                       
         L     RF,COMMADRS(RE)                                                  
         ST    RF,AVALROUT                                                      
         MVC   0(1,R4),TYPTRTYP    SET RECORD TYPE                              
         ST    R8,ATYPNTRY         SAVE A(LIST TYPE TABLE ENTRY)                
         B     VCOK                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY SYSTEM LIST TYPE FIELD                                      *         
***********************************************************************         
         SPACE                                                                  
VDISLTYP EQU   *                                                                
         LM    R2,R4,0(R1)                                                      
         XC    ATYPTAB,ATYPTAB                                                  
         LA    R8,TYPTAB                                                        
         USING TYPTABD,R8                                                       
DLTY010  CLI   TYPTABD,TYPTEOTQ    TEST END OF TABLE                            
         BE    DLTYX                                                            
         CLC   TYPTRTYP,0(R3)                                                   
         BE    DLTY020                                                          
DLTY012  LA    R8,TYPTABL(R8)                                                   
         B     DLTY010                                                          
*                                                                               
DLTY020  CLI   0(R4),0             TEST SPECIFIC SYSTEM NUMBER                  
         BE    DLTY030                                                          
         CLC   TYPTSYSN,0(R4)      IF SO MATCH ON SYSTEM NUMBER                 
         BNE   DLTY012                                                          
*                                                                               
DLTY030  MVC   0(L'TYPTNAME,R2),TYPTNAME                                        
         ST    R8,ATYPTAB                                                       
*                                                                               
DLTYX    B     VCOK                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE USER-ID IN WORK                                            *         
* RETURN AGENCY ALPHA ID AND USER ID NUMBER                           *         
***********************************************************************         
         SPACE 1                                                                
VVALUSID EQU   *                                                                
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA2                                                      
         USING CTIKEY,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,WORK                                                      
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    VCEIIO              CHECK RECORD FOUND OK                        
         BH    VCEUID                                                           
         LA    R3,CTIDATA                                                       
         SR    R0,R0                                                            
         MVI   BYTE,0                                                           
VUID010  CLI   0(R3),0                                                          
         BE    VUID100                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    VUID030                                                          
         CLI   0(R3),X'06'                                                      
         BE    VUID040                                                          
VUID020  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VUID010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
VUID030  MVC   OTHRDATA(2),CTDSC                                                
         MVI   BYTE,1                                                           
         B     VUID020                                                          
*                                                                               
VUID040  MVC   AGAID,2(R3)                                                      
         B     VUID020                                                          
*                                                                               
VUID100  CLI   BYTE,0                                                           
         BE    VCEIRT                                                           
*                                                                               
VALUSIDX EQU   *                                                                
         MVC   IOKEY,KEYSAVE                                                    
         B     VCOK                                                             
         DROP  R2,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SYSTEM NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
VVALSYSN EQU   *                                                                
         L     R1,ASYSFACS                                                      
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SENAME,WORK                                                      
         BE    *+12                                                             
VSYS010  BXLE  R1,RE,*-10                                                       
         B     VCEIIF                                                           
         CLI   TYPTSYSN,0          TEST IF ALL SYSTEMS                          
         BE    VSYS020                                                          
         CLC   TYPTSYSN,SEOVSYS    ELSE FILTER ON OV SYSTEM NUMBER              
         BNE   VSYS010                                                          
*                                                                               
VSYS020  MVC   OTHRDATA+0(1),SEOVSYS                                            
         MVC   OTHRDATA+1(1),SESYS                                              
         MVC   OTHRDATA+2(1),SEFILSET                                           
*                                                                               
VALSYSNX EQU   *                                                                
         B     VCOK                                                             
         DROP  R1                                                               
         SPACE 2                                                                
VVALNOOP B     VCOK                NO-OP VALIDATION                             
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
VCEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VCNO                                                             
VCEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     VCNO                                                             
VCERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VCNO                                                             
VCEIRT   MVC   FVMSGNO,=AL2(CE#INVRE)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     VCNO                                                             
VCEUID   MVC   FVMSGNO,=AL2(CE#IDRNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     VCNO                                                             
*                                                                               
VCOK     SR    RC,RC               RETURN CC EQUAL RECORD OK                    
VCNO     LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         XIT1  ,                                                                
*                                                                               
VCSPACES DC    80C' '                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
TYPTAB   DS    0X                  ** TYPE TABLE **                             
         DC    C'IDS     ',AL1(0),AL1(0)                                        
         DC    AL2(VALUSID-COMMADRS),AL1(CTWKRUSR)                              
         DC    AL1(1,L'CTWKID,3,10,0,2)                                         
         DC    C'IDGROUP ',AL1(TYPTIAGA+TYPTIUKY),AL1(0)                        
         DC    AL2(VALUSID-COMMADRS),AL1(CTWKRIDG)                              
         DC    AL1(1,L'CTWKID,3,10,0,2)                                         
         DC    C'SYSTEMS ',AL1(0),AL1(0)                                        
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSMPL  ',AL1(0),AL1(TYPTSMPL)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSACC  ',AL1(0),AL1(TYPTSACC)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSMBASE',AL1(0),AL1(TYPTSMBA)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSCPP  ',AL1(0),AL1(TYPTSCPP)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSPER  ',AL1(0),AL1(TYPTSPER)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSCON  ',AL1(0),AL1(TYPTSCON)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSSEC  ',AL1(0),AL1(TYPTSSEC)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSGAMES',AL1(0),AL1(TYPTSGAM)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
*&&US                                                                           
         DC    C'SYSSPOT ',AL1(0),AL1(TYPTSSPT)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSNET  ',AL1(0),AL1(TYPTSNET)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSPRINT',AL1(0),AL1(TYPTSPRT)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSTAL  ',AL1(0),AL1(TYPTSTAL)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSREP  ',AL1(0),AL1(TYPTSREP)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSSTR  ',AL1(0),AL1(TYPTSSTR)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
*&&                                                                             
*&&UK                                                                           
         DC    C'SYSMEDIA',AL1(0),AL1(TYPTSMED)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSFEE  ',AL1(0),AL1(TYPTSFEE)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'SYSABS  ',AL1(0),AL1(TYPTSABS)                                 
         DC    AL2(VALSYSN-COMMADRS),AL1(CTWKRSYS)                              
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
*&&                                                                             
         DC    C'PRINTERS',AL1(TYPTINON+TYPTICHS+TYPTISRT),AL1(0)               
         DC    AL2(VALNOOP-COMMADRS),AL1(CTWKRPRT)                              
         DC    AL1(1,L'CTWKID,8,8,1,0)                                          
         DC    C'ACCESS  ',AL1(TYPTINON),AL1(0)                                 
         DC    AL2(VALNOOP-COMMADRS),AL1(CTWKRAGY)                              
         DC    AL1(1,L'CTWKID,2,2,0,0)                                          
TYPTABX  DC    AL1(TYPTEOTQ)                                                    
         SPACE 2                                                                
TYPTABD  DSECT                     ** LIST TYPE TABLE **                        
TYPTEOTQ EQU   0                   END OF TABLE INDICATOR                       
TYPTNAME DS    CL8                 LIST TYPE NAME                               
TYPTINDS DS    XL1                 INDICATORS                                   
TYPTIHIN EQU   X'80'               INCLUDE HEX DATA FOR DUPLICATES              
TYPTINON EQU   X'40'               NESTING NOT ALLOWED                          
TYPTICHS EQU   X'20'               CHECK HEX SEPARATELY FOR DUPLICATES          
TYPTIAGA EQU   X'10'               SET CTWKAGY FROM TWAAGY                      
TYPTISRT EQU   X'08'               SORT DATA ON LOGICAL PRINTER NUMBER          
TYPTIUKY EQU   X'04'               USER ID VALIDATION OF KEY LIST ID            
TYPTSYSN DS    XL1                 FACPAK SYSTEM NUMBER                         
TYPTSSPT EQU   X'02'               SPOT SYSTEM NUMBER                           
TYPTSNET EQU   X'03'               NETWORK SYSTEM NUMBER                        
TYPTSPRT EQU   X'04'               PRINT SYSTEM NUMBER                          
TYPTSMPL EQU   X'05'               MEDIA PLANNING SYSTEM NUMBER                 
TYPTSACC EQU   X'06'               ACCOUNT SYSTEM NUMBER                        
TYPTSTAL EQU   X'07'               TALENT SYSTEM NUMBER                         
TYPTSREP EQU   X'08'               REPORT SYSTEM NUMBER                         
TYPTSMBA EQU   X'09'               MEDIA BASE SYSTEM NUMBER                     
TYPTSMED EQU   X'04'               MEDIA SYSTEM NUMBER                          
TYPTSFEE EQU   X'07'               FEE SYSTEM NUMBER                            
TYPTSCPP EQU   X'0C'               CPP SYSTEM NUMBER                            
TYPTSSTR EQU   X'0D'               STRAFFIC SYSTEM NUMBER                       
TYPTSPER EQU   X'0E'               PERSON SYSTEM NUMBER                         
TYPTSCON EQU   X'0A'               CONTROL SYSTEM NUMBER                        
TYPTSSEC EQU   X'0A'               SECURITY SYSTEM NUMBER                       
TYPTSGAM EQU   X'0B'               GAMES SYSTEM NUMBER                          
TYPTSABS EQU   X'05'               ABS SYSTEM NUMBER                            
TYPTROUT DS    AL2                 VALIDATION ROUTINE NUMBER                    
TYPTRTYP DS    CL1                 RECORD TYPE                                  
TYPTKMIN DS    AL1                 MINIMUM INPUT KEY LENGTH                     
TYPTKMAX DS    AL1                 MAXIMUM INPUT KEY LENGTH                     
TYPTIMIN DS    AL1                 MINIMUM INPUT DATA LENGTH                    
TYPTIMAX DS    AL1                 MAXIMUM INPUT DATA LENGTH                    
TYPTIHEX DS    AL1                 LENGTH OF HEXADECIMAL INPUT OR ZERO          
TYPTICHR DS    AL1                 LENGTH OF CHARACTER INPUT OR ZERO            
TYPTABL  EQU   *-TYPTABD                                                        
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD FOR REPORT IN DETAILED FORMAT MODE                   *         
***********************************************************************         
         SPACE                                                                  
         DS    0D                                                               
DETREP   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DETREP,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+DET'    INSERT NAME                                  
         USING CTWREC,R2                                                        
*                                                                               
DETFLT   EQU   *                                                                
         OC    SELUSR,SELUSR                                                    
         BNZ   *+14                                                             
         OC    SELAGY,SELAGY                                                    
         BZ    DFLTX                                                            
         BAS   RE,CHKIDT                                                        
         BNE   DREPX                                                            
DFLTX    EQU   *                                                                
*                                                                               
DETTIT   EQU   *                                                                
         MVC   REPP1(20),=CL20'SYSLIST RECORD ENTRY'                            
         MVC   REPP2(20),=40C'-'                                                
         GOTO1 VREPORT,REPD                                                     
DTITX    EQU   *                                                                
*                                                                               
DETLHDR  EQU   *                                                                
         MVC   REPP1(20),=CL20'LIST TYPE: '                                     
         GOTO1 DISLTYP,APPARM,REPP1+13,CTWKREC,CTWKSYSN                         
*                                                                               
         ICM   R8,15,ATYPTAB                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING TYPTABD,R8                                                       
*                                                                               
         MVC   REPP1+24(20),=CL20'LIST ID: '                                    
         MVC   REPP1+33(L'CTWKID),CTWKID                                        
*                                                                               
         USING CTDSCD,R3                                                        
         MVC   REPP1+44(20),=CL20'LIST DESCRIPTION: '                           
         MVI   APELEM+1,0                                                       
         MVI   APELEM,CTDSCELQ                                                  
         GOTO1 AGETELS,CTWREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DHDR010                                                          
         SR    R1,R1               OUTPUT LIST DESCRIPTION                      
         IC    R1,CTDSCLEN                                                      
         SH    R1,=Y(CTDSC+1-CTDSCD)                                            
         EX    R1,*+4                                                           
         MVC   REPP1+62(0),CTDSC                                                
DHDR010  EQU   *                                                                
         GOTO1 VREPORT,REPD                                                     
         OC    CTWKAGY,CTWKAGY                                                  
         BZ    *+16                                                             
         MVC   REPP1(20),=CL20'LIST AGENCY: '                                   
         MVC   REPP1+13(2),CTWKAGY                                              
         GOTO1 VREPORT,REPD                                                     
DHDRX    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
DETIDL   EQU   *                                                                
         LA    RE,AREA                                                          
         LA    RF,2000                                                          
         LA    R0,*                                                             
         SR    R1,R1                                                            
         ICM   R1,8,DTSPACES                                                    
         MVCL  RE,R0                                                            
         LA    R3,CTWDATA          R3=A(ELEMENT)                                
         USING CTLSTD,R3                                                        
*                                                                               
         TM    TYPTINDS,TYPTISRT   IF PRINTER LIST RECORD                       
         BZ    DIDL030             SORT ENTRIES BY LOGICAL PRINTER #            
         SR    R0,R0                                                            
DIDL010  CLI   CTLSTEL,CTLSTELQ    FIND FIRST LIST ELEMENT                      
         BE    *+14                                                             
         IC    R0,CTLSTLEN                                                      
         AR    R3,R0                                                            
         B     DIDL010                                                          
         ST    R3,APPARM           SAVE POINTER TO FIRST LIST ELEMENT           
         LA    RF,1                                                             
DIDL020  IC    R0,CTLSTLEN                                                      
         AR    R3,R0                                                            
         CLI   CTLSTEL,CTLSTELQ                                                 
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         B     DIDL020                                                          
*                                                                               
         ST    RF,APPARM+4                                                      
         L     R3,APPARM                                                        
         SR    RF,RF                                                            
         IC    RF,CTLSTLEN                                                      
         LR    R0,RF                                                            
         BCTR  R0,0                                                             
         GOTO1 VXSORT,APPARM,,,(RF),1,(R0)                                      
         LA    R3,CTWDATA                                                       
*                                                                               
DIDL030  LA    RE,AREA                                                          
         ST    RE,ANXTNTRY                                                      
         XC    NUMNTRY,NUMNTRY                                                  
         SR    RE,RE                                                            
         IC    RE,TYPTIMAX         CALCULATE LENGTH OF EACH DISPLAY             
         SR    RF,RF               ENTRY                                        
         IC    RF,TYPTIHEX                                                      
         SLL   RF,1                                                             
         LA    RE,1(RE,RF)                                                      
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         LA    RE,1(RE)                                                         
         CH    RE,=H'8'            MINIMUM LENGTH OF 8                          
         BH    *+8                                                              
         LH    RE,=H'8'                                                         
         STC   RE,LENTRY                                                        
*                                                                               
DIDL100  CLI   0(R3),0             END OF RECORD                                
         BE    DIDL200                                                          
         CLI   0(R3),CTLSTELQ      LIST ELEMENT                                 
         BE    DIDL120                                                          
         CLI   0(R3),CTLINELQ      LIST INCLUDE ELEMENT                         
         BE    DIDL130                                                          
*                                                                               
DIDL110  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DIDL100                                                          
*                                                                               
         USING CTLSTD,R3                                                        
DIDL120  BAS   RE,IDLIST           HANDLE LIST ENTRY                            
         SR    RE,RE                                                            
         IC    RE,TYPTIMAX                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R4),CTLSTDTA    MOVE ALPHA DATA                              
         SR    R0,R0                                                            
         ICM   R0,1,TYPTIHEX                                                    
         BZ    DIDL110                                                          
         LA    RF,CTLSTDTA+1(RE)   AND HEX IF ANY                               
         LA    R4,0(R4,RE)                                                      
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'/'                                                       
         GOTO1 VHEXOUT,APPARM,(RF),2(R4),(R0),=C'TOG'                           
         B     DIDL110                                                          
*                                                                               
         USING CTLIND,R3                                                        
DIDL130  BAS   RE,IDLIST           HANDLE INCLUDE ELEMENT                       
         MVC   0(2,R4),=C'L='                                                   
         MVC   2(L'CTLINC,R4),CTLINC                                            
         B     DIDL110                                                          
*                                                                               
DIDL200  OC    NUMNTRY,NUMNTRY     TEST FOR NO ENTRIES                          
         BZ    DIDLX                                                            
         MVC   REPP1(7),=C'ID LIST'                                             
         MVC   REPP2(7),=40C'-'                                                 
         GOTO1 =V(SQUASHER),APPARM,AREA,2000,RR=APRELO                          
         GOTO1 =V(CHOPPER),APPARM,(250,AREA),(110,REPP1+13),(C'P',10), +        
               RR=APRELO                                                        
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
DIDLX    EQU   *                                                                
*                                                                               
DETAID   EQU   *                                                                
         LA    RE,AREA                                                          
         LA    RF,2000                                                          
         LA    R0,*                                                             
         SR    R1,R1                                                            
         ICM   R1,8,DTSPACES                                                    
         MVCL  RE,R0                                                            
         L     R3,AIDLTAB                                                       
         USING IDLTABD,R3                                                       
DAID010  EQU   *                                                                
         CLI   0(R3),0                                                          
         BE    DAID040                                                          
         CLC   IDLTSLC,CTWKID                                                   
         BE    DAID030                                                          
DAID020  EQU   *                                                                
         LA    R3,IDLTABL(R3)                                                   
         B     DAID010                                                          
         DROP  R3                                                               
DAID030  EQU   *                                                                
         BAS   RE,AIDLIST                                                       
         B     DAID020                                                          
DAID040  EQU   *                                                                
         CLI   AREA,C' '                                                        
         BE    DAIDX                                                            
         MVC   REPP1(11),=CL11'X-REFERENCE '                                    
         MVC   REPP2(11),=11C'-'                                                
         GOTO1 =V(SQUASHER),APPARM,AREA,2000,RR=APRELO                          
         GOTO1 =V(CHOPPER),APPARM,(250,AREA),(110,REPP1+13),(C'P',10), +        
               RR=APRELO                                                        
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
DAIDX    EQU   *                                                                
*                                                                               
         NI    REPHEADI,X'FF'-(REPHSPAC)                                        
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
*                                                                               
DREPX    EQU   *                                                                
         XIT1  ,                                                                
         EJECT                                                                  
         SPACE 2                                                                
IDLIST   EQU   *                                                                
         L     R4,ANXTNTRY         GET NEXT TIA OUTPUT ENTRY                    
         SR    RF,RF                                                            
         IC    RF,LENTRY                                                        
         LA    RF,0(R4,RF)                                                      
         ST    RF,ANXTNTRY                                                      
         L     RF,NUMNTRY                                                       
         LA    RF,1(RF)                                                         
         ST    RF,NUMNTRY                                                       
         SR    RF,RF                                                            
         IC    RF,LENTRY                                                        
         SH    RF,=H'2'                                                         
         MVI   0(R4),C' '          CLEAR THE ENTRY                              
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   1(0,R4),0(R4)                                                    
         SPACE 2                                                                
AIDLIST  NTR1                                                                   
         LA    R4,AREA                                                          
*                                                                               
AID2     CLC   0(10,R4),DTSPACES                                                
         BE    AID4                                                             
         LA    R4,11(R4)                                                        
         B     AID2                                                             
*                                                                               
AID4     EQU   *                                                                
         USING IDLTABD,R3                                                       
         MVC   0(10,R4),IDLTID                                                  
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
CHKIDT   NTR1                                                                   
         L     R4,AIDLTAB                                                       
         USING IDLTABD,R4                                                       
*                                                                               
CIDT010  OC    0(10,R4),0(R4)                                                   
         BZ    CIDTNO                                                           
         OC    SELAGY,SELAGY                                                    
         BZ    *+14                                                             
         CLC   IDLTAGY,SELAGY                                                   
         BNE   CIDT020                                                          
         OC    SELUSR,SELUSR                                                    
         BZ    *+14                                                             
         CLC   IDLTID,SELUSR                                                    
         BNE   CIDT020                                                          
         CLC   IDLTSLT,CTWKREC                                                  
         BNE   CIDT020                                                          
         CLC   IDLTSLC,CTWKID                                                   
         BE    CIDTOK                                                           
CIDT020  LA    R4,IDLTABL(R4)                                                   
         B     CIDT010                                                          
*                                                                               
CIDTOK   SR    RC,RC                                                            
CIDTNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
DTSPACES DC    80C' '                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD USER ID TABLE FOR COMPATIBLE SYSLIST XREF                     *         
***********************************************************************         
         SPACE                                                                  
         DS    0D                                                               
BLDIDT   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING BLDIDT,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+BID'    INSERT NAME                                  
*                                  CLEAR ID RECORD DATA TABLE                   
         ICM   R4,15,=AL4((IDLTMAX+1)*(IDLTABL))                                
         GETMAIN RC,LV=(R4),BNDRY=PAGE                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,AIDLTAB          SAVE A(GETMAIN REGION)                       
         ST    R0,LIDLTAB          SAVE LENGTH(GETMAIN REGION)                  
         AR    R1,R0                                                            
         ST    R1,AIDLTABX                                                      
         SPACE 1                                                                
*                                  CLEAR ACCESS RECORD DATA TABLE               
         L     RE,AIDLTAB                                                       
         L     RF,AIDLTABX                                                      
         SR    RF,RE                                                            
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,IOKEY            READ USER ID RECORDS                         
         USING CTIKEY,R2           INTO AIOAREA2                                
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         GOTO1 AIO,IOHIGH+IOCONFIL+IO2                                          
         B     BIDT020                                                          
BIDT010  EQU   *                                                                
         GOTO1 AIO,IOSQ+IOCONFIL+IO2                                            
BIDT020  BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         CLI   CTIKTYP,CTIKTYPQ                                                 
         BNE   BIDT400                                                          
         OC    CTIKID(8),CTIKID                                                 
         BZ    BIDT010                                                          
         MVC   AGYID,=CL2'  '                                                   
*                                                                               
         LA    R3,CTIDATA                                                       
         SR    R0,R0                                                            
BIDT030  CLI   0(R3),0                                                          
         BE    BIDT090                                                          
         CLI   0(R3),X'06'                                                      
         BE    BIDT040                                                          
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BIDT030                                                          
*                                                                               
BIDT040  EQU   *                                                                
         MVC   AGYID,2(R3)                                                      
*                                                                               
BIDT090  EQU   *                                                                
         LA    R3,CTIDATA                                                       
         SR    R0,R0                                                            
BIDT100  CLI   0(R3),0                                                          
         BE    BIDT010                                                          
         CLI   0(R3),CTIDELQ                                                    
         BE    BIDT200                                                          
         CLI   0(R3),CTPRNELQ                                                   
         BE    BIDT300                                                          
BIDT110  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BIDT100                                                          
*                                                                               
BIDT200  EQU   *                                                                
         USING CTIDD,R3                                                         
         OC    CTID(2),CTID                                                     
         BNZ   BIDT110                                                          
         L     R4,AIDLTAB                                                       
         USING IDLTABD,R4                                                       
BIDT220  OC    IDLTID,IDLTID                                                    
         BZ    BIDT230                                                          
         B     BIDT240                                                          
BIDT230  MVC   IDLTID,CTIKID                                                    
         MVI   IDLTSLT,CTWKRUSR                                                 
         MVC   IDLTSLC,CTID+2                                                   
         MVC   IDLTAGY,AGYID                                                    
         B     BIDT110                                                          
BIDT240  LA    R4,IDLTABL(R4)                                                   
         CLM   R4,15,AIDLTABX                                                   
         BL    BIDT220                                                          
         DC    H'0'                                                             
*                                                                               
BIDT300  EQU   *                                                                
         USING CTPRND,R3                                                        
         OC    CTPRNFLG,CTPRNFLG                                                
         BNZ   BIDT110                                                          
         L     R4,AIDLTAB                                                       
         USING IDLTABD,R4                                                       
BIDT320  OC    IDLTID,IDLTID                                                    
         BZ    BIDT330                                                          
         B     BIDT340                                                          
BIDT330  MVC   IDLTID,CTIKID                                                    
         MVI   IDLTSLT,CTWKRPRT                                                 
         MVC   IDLTSLC,CTPRNLST                                                 
         MVC   IDLTAGY,AGYID                                                    
         B     BIDT110                                                          
BIDT340  LA    R4,IDLTABL(R4)                                                   
         CLM   R4,15,AIDLTABX                                                   
         BL    BIDT320                                                          
         DC    H'0'                                                             
*                                                                               
BIDT400  EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENE4D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENC4D                                                       
         EJECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENA4D                                                       
         ORG                                                                    
         EJECT                                                                  
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
LASTPRO  DS    CL4                                                              
LIDNUM   DS    CL2                                                              
SAVKEY   DS    XL(L'CTIKEY)        SAVE LAST RECORD KEY READ FOR COPY           
*                                                                               
PRISCSAV DS    CL1                                                              
PRIPTSAV DS    CL1                                                              
PRIPOSAV DS    CL1                                                              
*                                                                               
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTAGY  DS    CL2                                                              
         DS    CL2                                                              
LISTLTYP DS    CL8                                                              
         DS    CL2                                                              
LISTID   DS    CL6                                                              
         DS    CL4                                                              
LISTDSC  DS    CL24                                                             
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE  1                                                               
IDLTABD  DSECT                     USER ID SYSLIST TABLE                        
IDLTID   DS    CL10                USER ID                                      
IDLTAGY  DS    CL2                 AGENCY ALPHA ID                              
IDLTSLT  DS    CL1                 SYSLIST TYPE                                 
IDLTSLC  DS    CL6                 SYSLIST CODE                                 
IDLTABL  EQU   *-IDLTABD                                                        
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
DUB1     DS    D                                                                
RETURN   DS    F                                                                
ASYSEL   DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
WORK     DS    XL(L'APWORK)                                                     
BYTE     DS    CL1                                                              
*                                                                               
SELDATA  DS    0XL(SELDATAL)                                                    
SELLTYP  DS    CL1                 LIST TYPE                                    
SELID    DS    CL6                 LIST ID                                      
SELINID  DS    CL10                INCLUDE ID                                   
SELUSR   DS    CL10                ASSOCIATED USER ID                           
SELAGY   DS    CL10                ASSOCIATED AGENCY ALPHA ID                   
SELDATAL EQU   *-SELLTYP                                                        
*                                                                               
FLDCNT   DS    XL1                                                              
COUNTER  DS    F                                                                
*                                                                               
AGYID    DS    CL2                                                              
SYSTEM   DS    CL1                                                              
SFLAGS   DS    CL1                                                              
PROGRAM  DS    CL1                                                              
PGNAME   DS    CL8                                                              
*                                                                               
IDNUM    DS    XL(L'CTIKNUM)                                                    
AGNUM    DS    XL(L'CTSYSAGB)                                                   
AGAID    DS    XL(L'CT5KALPH)                                                   
LISTIDAG DS    XL(L'CT5KALPH)                                                   
*                                                                               
GETSEQF  DS    XL1                                                              
IOCOUNT  DS    H                                                                
*                                                                               
LENTRY   DS    XL1                                                              
OTHRDATA DS    CL10                                                             
ELS      DS    PL3                                                              
ANXTNTRY DS    A                                                                
NUMNTRY  DS    A                                                                
ATYPNTRY DS    A                                                                
ATYPTAB  DS    A                                                                
AVALROUT DS    A                                                                
VXSORT   DS    V                                                                
*                                                                               
COMMADRS DS    0A                                                               
VALPARS  DS    A                                                                
GETREC   DS    A                                                                
LINE     DS    A                                                                
VALLTYP  DS    A                                                                
DISLTYP  DS    A                                                                
VALUSID  DS    A                                                                
VALSYSN  DS    A                                                                
VALNOOP  DS    A                                                                
         DS    22A                 SPARE                                        
*                                                                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
BLOCK3   DS    20CL32                                                           
BLOCK4   DS    20CL32                                                           
*                                                                               
DELKEY   DS    CL(L'IOKEY)                                                      
KEYSAVE  DS    CL(L'IOKEY)                                                      
*                                                                               
LIDLTAB  DS    F                                                                
AIDLTAB  DS    A                                                                
AIDLTABX DS    A                                                                
IDLTMAX  EQU   100000                                                           
*                                                                               
AREA     DS    3000XL1                                                          
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014CTGEN1B   05/01/02'                                      
         END                                                                    
