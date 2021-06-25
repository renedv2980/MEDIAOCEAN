*          DATA SET CTGEN1C    AT LEVEL 013 AS OF 11/08/13                      
*PHASE TA0B1CA                                                                  
         TITLE 'CTGEN1C - SECURITY PROGRAM ACCESS MAINTENANCE'                  
GEN1C    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN1C*,RA,R9,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(RECORD KEY)                             
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
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     EXIT                05 - APMDELR                                 
         B     EXIT                06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     EXIT                11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     EXIT                15 - APMVALQ                                 
         B     EXIT                16 - APMREPP                                 
         B     EXIT                17 - APMSETT                                 
         B     EXIT                18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF ACCESS RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   EQU   *                                                                
         LA    R2,IOKEY            R2=A(RECORD KEY)                             
         XC    CT5KEY,CT5KEY       INITIALISE RECORD KEY                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,SECALPHH      VALIDATE AGENCY ALPHA ID                     
         BNE   VALKEYX                                                          
         MVC   CT5KALPH,FVIFLD                                                  
         MVC   APRECKEY(L'CT5KEY),CT5KEY                                        
*                                                                               
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX                                                          
         BH    VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
*                                                                               
VK010    MVI   FVMINL,1            READ SYSTEM FIELD                            
         GOTO1 AFVAL,SECSYSH                                                    
         BNE   VK020                                                            
         USING SYSLSTD,RE                                                       
         L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         LA    RE,SYSLLEN(RE)      GO PAST SERVICE SYSTEM ENTRY                 
         ZIC   RF,FVXLEN                                                        
VKSYS10  CLI   SYSLNUM,0                                                        
         BE    ESYS                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VKSYS20                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VKSYS10                                                          
*                                                                               
VKSYS20  MVC   SYSTEM,SYSLNUM      SET SYSTEM NUMBER FROM LIST                  
         MVC   SECSYS(7),SYSLNAME  DISPLAY FULL SYSTEM NAME                     
         OI    SECSYSH+6,X'80'                                                  
* ??     B     VALKEYY                                                          
         DROP  RE                                                               
*                                  DEFAULT TO FIRST IN RECORD                   
VK020    XC    APELEM,APELEM                                                    
         MVI   APELEM,CTSYSELQ                                                  
         CLI   SYSTEM,0                                                         
         BE    VK030                                                            
         MVI   APELEM+1,1                                                       
         MVC   APELEM+2(1),SYSTEM                                               
*                                                                               
VK030    L     R2,AIOAREA1                                                      
         GOTO1 AGETELS,(R2)                                                     
         ICM   R1,15,APPARM                                                     
         BZ    ESYS                                                             
         MVC   SYSTEM,CTSYSNUM-CTSYSD(R1)                                       
         XC    SYSEL,SYSEL                                                      
         SR    RF,RF                                                            
         IC    RF,CTSYSLEN-CTSYSD(R1)                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SYSEL(0),0(R1)                                                   
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE ACCESS RECORD SECURITY PROGRAM ACCESS DATA        *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   CT5KEY,APRECKEY                                                  
         CLI   SYSTEM,0                                                         
         BE    DISREC                                                           
         CLC   SYSTEM,SAVSYS                                                    
         BNE   DISREC                                                           
*                                  PROCESS SYSTEM ELEMENT                       
         USING CTSYSD,R3                                                        
         LA    R3,SYSEL                                                         
         MVI   ALLPGM,0                                                         
         GOTO1 AFVAL,SECALLH       READ DEFAULT ACCESS CODE                     
         BNE   VR010                                                            
*                                    FOR ALL PROGRAMS                           
         CLI   FVILEN,0                                                         
         BE    VR010                                                            
         CLI   FVIFLD,C'N'                                                      
         BE    *+12                                                             
         CLI   FVIFLD,C'Y'                                                      
         BNE   EIIF                                                             
         MVC   ALLPGM,FVIFLD                                                    
*                                  VALIDATE ACCESS CODES                        
VR010    BAS   RE,VALPGMS            FOR EACH PROGRAM IN TABLE                  
         BNE   EXIT                                                             
*                                                                               
VR020    XC    APELEM,APELEM                                                    
         MVI   APELEM,X'21'                                                     
         MVI   APELEM+1,1                                                       
         MVC   APELEM+2,CTSYSNUM                                                
         GOTO1 ADELELS,CT5REC                                                   
         MVC   APELEM,SYSEL                                                     
         GOTO1 AADDELS,CT5REC      REPLACE UPDATED SYSTEM EL                    
         B     VR200                                                            
         DROP  R3                                                               
         EJECT                                                                  
VR200    GOTO1 ASETACT,CT5REC      UPDATE ACTIVITY ELEMENT                      
*                                  UPDATE RECORD ON FILE                        
         MVC   IOKEY(L'CT5KEY),APRECKEY  RESTORE ACCESS RECORD KEY              
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                 PERFORM THE I/O                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF AN ACCESS RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         TWAXC SECALPHH,SECALPHH                                                
         OI    SECALPHH+FHOID,FHOITR                                            
         MVC   SECALPH,CT5KALPH                                                 
         CLI   SYSTEM,0                                                         
         BE    DISKEYX                                                          
         GOTO1 ADISSYS,SYSTEM                                                   
         MVC   SECSYS,APWORK                                                    
         SPACE 1                                                                
DISKEYX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY ACCESS RECORD SECUIRTY PROGRAM ACCESS DATA       *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         OI    SECALPHH+FHOID,FHOITR                                            
         MVC   SECALPH,CT5KALPH                                                 
         TWAXC SECSYSH                                                          
         TWAXC SECPN01H,PROT=Y                                                  
         BAS   RE,DISSHD           DISPLAY SYSTEM SUMMARY HEADER                
         GOTO1 ADISSYS,SYSTEM      REDISPLAY SYSTEM NAME                        
         MVC   SECSYS,APWORK                                                    
         BAS   RE,GETACTY          GET AGENCY COUNTRY CODE                      
         BAS   RE,INITPGML         INITIALISE PROGRAM TABLE DISPLAY             
*                                                                               
         LA    R3,SYSEL                                                         
         BAS   RE,DISPGMS                                                       
*                                                                               
DISRECX  MVC   SAVSYS,SYSTEM       SAVE LAST DISPLAYED SYSTEM                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS FOR ACCESS RECORDS            *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY         SET UP FIRST LIST RECORD KEY                 
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         GOTO1 AFVAL,LSTALPHH                                                   
         BNE   *+10                                                             
         MVC   CT5KALPH,FVIFLD                                                  
         XC    SELKEY,SELKEY       CLEAR SELECT DATA FIELDS                     
*                                                                               
         XC    SELALPH,SELALPH     CLEAR ALL FILTER VALUES                      
         XC    SELSYS,SELSYS                                                    
         XC    SELPGM,SELPGM                                                    
         XC    SELAAWA,SELAAWA                                                  
         XC    SELAGYO,SELAGYO                                                  
*                                                                               
         GOTO1 AFVAL,LSTAAWAH      ACCESS AWARE?                                
         BNE   VS25                                                             
         CLI   FVIFLD,C'Y'                                                      
         BE    VS20                                                             
         CLI   FVIFLD,C'N'                                                      
         BE    VS20                                                             
         B     EIIF                                                             
VS20     MVC   SELAAWA,FVIFLD                                                   
*                                                                               
VS25     GOTO1 AFVAL,LSTSYSH       SYSTEM                                       
         BE    VS30                                                             
         CLI   LSTPGMH+5,0         REQUIRED WHEN PGM IS GIVEN                   
         BNE   EMIF                                                             
         OC    SELAAWA,SELAAWA     REQUIRED WHEN ACCESS AWARE IS GIVEN          
         BZ    VS60                                                             
         B     EMIF                                                             
*                                                                               
         USING SYSLSTD,RE                                                       
VS30     L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         LA    RE,SYSLLEN(RE)      GO PAST SERVICE SYSTEM ENTRY                 
         ZIC   RF,FVXLEN                                                        
VS40     CLI   SYSLNUM,0                                                        
         BE    ESYS                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VS50                                                             
         LA    RE,SYSLLEN(RE)                                                   
         B     VS40                                                             
*                                                                               
VS50     MVC   SELSYS,SYSLNUM      SET SYSTEM NUMBER FROM LIST                  
         OI    LSTSYSH+6,X'80'                                                  
         MVC   SYSTEM,SELSYS                                                    
         GOTO1 ADISSYS,SELSYS      REDISPLAY SYSTEM NAME                        
         MVC   LSTSYS,APWORK                                                    
         DROP  RE                                                               
*                                                                               
VS60     GOTO1 AFVAL,LSTPGMH       PROGRAM                                      
         BE    VS70                                                             
         OC    SELAAWA,SELAAWA     REQUIRED WHEN ACCESS AWARE IS GIVEN          
         BZ    VS110                                                            
         B     EMIF                                                             
*                                                                               
VS70     L     R3,ASYS                                                          
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3                                                       
*                                                                               
         CLC   SELSYS,SEOVSYS                                                   
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
*                                                                               
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
         DROP  R3                                                               
*                                                                               
         L     R1,APGM                                                          
         LH    RE,0(R1)            LEN OF PGM ENTRY                             
         L     RF,2(R1)            A(LAST ENTRY)                                
         LA    R1,6(R1)            A(1ST ENTRY)                                 
         ST    R1,APFULL                                                        
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
*                                                                               
         ZIC   R3,FVXLEN                                                        
VS80     EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),PGMNAME                                                
         BE    VS100                                                            
VS90     BXLE  R1,RE,VS80                                                       
         B     EIIF                                                             
*                                                                               
VS100    MVC   SELPGM,PGMNUM                                                    
         MVC   LSTPGM,PGMNAME      DISPLAY FULL PROGRAM NAME                    
         OI    LSTPGMH+6,X'80'                                                  
*                                                                               
         MVC   PROGIND2,PGMIND2    SAVE PGM INDICATOR 2                         
         MVC   PROGRAM,PGMNUM      SAVE PGM NUMBER                              
         DROP  R1                                                               
*                                                                               
VS110    GOTO1 AFVAL,LSTAGYOH      SEC AGY ONLY?                                
         BNE   VSY                                                              
         CLI   FVIFLD,C'Y'                                                      
         BE    VS120                                                            
         CLI   FVIFLD,C'N'                                                      
         BE    VS120                                                            
         B     EIIF                                                             
VS120    MVC   SELAGYO,FVIFLD                                                   
*                                                                               
VSY      LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VSX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST-SELECT RECORD FOR ACCESS RECORDS                      *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY                                                         
         MVC   CT5KEY,APRECKEY                                                  
         TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GS20                                                             
*                                                                               
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GS40                                                             
         B     GSN                                                              
*                                                                               
GS20     TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GS40                                                             
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GS40     LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GSN                                                              
         L     R2,AIOAREA1                                                      
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   GSN                                                              
*                                                                               
         MVI   APFLAG,0            SET SYSTEM # SELECT SEARCH FLAG              
         LA    R3,CT5DATA          GET ELEMENTS FOR SELECT FILTER               
GS60     CLI   0(R3),0             E-O-R                                        
         BE    GS100                                                            
         CLI   0(R3),CTSEAELQ      SECURITY AGENCY                              
         BNE   *+12                                                             
         OI    APFLAG,X'40'        SEC AGY ELEMENT FOUND                        
         BE    GS90                                                             
         CLI   0(R3),CTSYSELQ      SYSTEM                                       
         BNE   GS90                                                             
*                                                                               
         USING CTSYSD,R3                                                        
         OC    SELSYS,SELSYS       SYSTEM # SELECT                              
         BZ    GS70                                                             
         CLC   SELSYS,CTSYSNUM                                                  
         BNE   GS90                                                             
         OI    APFLAG,X'80'        FLAG MATCHING SYSTEM # FOUND                 
*                                                                               
GS70     OC    SELPGM,SELPGM       PROGRAM #                                    
         BZ    GS90                                                             
*                                                                               
         MVI   PACCVAL,C'N'                                                     
         CLI   CTSYSLEN,X'18'      CHECK EXISTING FLAGS PRESENT                 
         BE    GS75                                                             
         TM    PROGIND2,PGMISECA   ELSE USE PROGRAM INDICATORS DEFAULT          
         BZ    GS85                                                             
         TM    PROGIND2,PGMISECB                                                
         BNZ   GS85                                                             
         MVI   PACCVAL,C'Y'                                                     
         B     GS85                                                             
*                                                                               
GS75     TM    PROGIND2,PGMISECB   TEST NEW/OLD SEC SWITCHABLE                  
         BNZ   GS80                                                             
         TM    PROGIND2,PGMISECA   TEST IF NEW SECURITY ONLY                    
         BZ    GS85                                                             
         MVI   PACCVAL,C'Y'                                                     
         B     GS85                                                             
*                                                                               
GS80     BAS   RE,GETPAVAL         GET ACCESS FLAG FROM SYSTEM ELEMENT          
GS85     OC    SELAAWA,SELAAWA     ACCESS AWARE IS GIVEN                        
         BZ    GS90                                                             
         CLC   PACCVAL,SELAAWA                                                  
         BNE   GS40                                                             
         DROP  R3                                                               
*                                                                               
GS90     SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GS60                                                             
*                                                                               
GS100    OC    SELSYS,SELSYS       SYSTEM # SELECT                              
         BZ    GS110                                                            
         TM    APFLAG,X'80'        TEST MATCHING SYSTEM # FOUND                 
         BZ    GS40                                                             
*                                                                               
GS110    OC    SELAGYO,SELAGYO     SEC AGY ONLY                                 
         BZ    GSY                                                              
         TM    APFLAG,X'40'        TEST SEC AGY ELEMENT FOUND                   
         BZ    GSY                                                              
         CLI   SELAGYO,C'Y'        ONLY LIST REC W/O SEC AGY ELEMENT            
         BE    GS40                                                             
*                                                                               
GSY      MVC   APRECKEY(L'CT5KEY),CT5KEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GSX                                                              
*                                                                               
GSN      MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GSX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE FOR ACCESS RECORDS                         *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         MVC   LISTALPH,CT5KALPH   DISPLAY AGENCY ALPHA ID                      
*                                                                               
         OC    SELPGM,SELPGM                                                    
         BZ    DS10                                                             
         MVC   LISTPGMN,LSTPGM     PROGRAM NAME                                 
         LA    R6,LISTPGMN+L'LISTPGMN-1                                         
         CLI   0(R6),C' '          BACK UP TO THE LAST NON-BLANK CHAR           
         BNE   *+10                                                             
         BCTR  R6,0                                                             
         B     *-10                                                             
         MVI   1(R6),C'='                                                       
         MVC   2(1,R6),PACCVAL     VALUE SET FROM GETSEL MODE                   
*                                                                               
DS10     XC    LISTBUFF,LISTBUFF                                                
         LA    R6,LISTBUFF         SET POINTER TO SYS# FIELD BUFFER             
         LA    R3,CT5DATA          READ EACH ELEMENT OF RECORD                  
*                                                                               
DS20     CLI   0(R3),0             E-O-R                                        
         BE    DS100                                                            
         CLI   0(R3),CTSYSELQ      SYSTEM                                       
         BE    DS30                                                             
         CLI   0(R3),CTAADELQ      AGENCY ACCESS DETAILS (X'B9')                
         BE    DS40                                                             
         CLI   0(R3),CTSEAELQ      SECURITY AGENCY ID                           
         BNE   DS90                                                             
*                                                                               
         MVC   LISTSECA,CTSEAAID-CTSEAD(R3)     SEC AGY ID                      
         B     DS90                                                             
*                                                                               
DS30     MVC   0(1,R6),CTSYSNUM-CTSYSD(R3)      SAVE SYS#                       
         AHI   R6,1                                                             
         B     DS90                                                             
*                                                                               
DS40     MVC   SVTEAM,CTAACSTN-CTAADD(R3)      SAVE SERVICE TEAM                
*                                                                               
DS90     SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DS20                                                             
*                                                                               
DS100    LA    R8,LISTBUFF         SET POINTER TO SYS# FIELD BUFFER             
         SR    R6,R8               # OF SYS ELEMENT FOUND                       
         STC   R6,DUB                                                           
         CHI   R6,10               CAN'T HAVE MORE THAN 10 SYS ELEMENTS         
         BNH   *+8                                                              
         LA    R6,10                                                            
         SR    R1,R1                                                            
*                                                                               
DS110    L     R3,ASYS             SEARCH SE LIST FOR SE NUM SESYSNUM           
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)            R3=A(SELIST ENTRY)                           
         USING SELISTD,R3                                                       
         CLC   SEOVSYS,0(R8)                                                    
         BE    DS115                                                            
         BXLE  R3,RE,*-10                                                       
         LA    R3,=CL7'XXX    '    SET UNKNOWN SYSTEM NAME                      
         B     *+8                                                              
DS115    LA    R3,SENAME                                                        
         DROP  R3                                                               
*                                                                               
DS120    LR    RE,R1                                                            
         MHI   RE,4                                                             
         LA    RE,LISTACCS(RE)                                                  
         MVC   0(3,RE),0(R3)       EXTRACT SE NAME                              
         NC    1(2,RE),=8X'BF'     SET TO LOWER CASE                            
         MVI   3(RE),C','                                                       
*                                                                               
         AHI   R8,1                                                             
         AHI   R1,1                                                             
         CR    R1,R6                                                            
         BL    DS110                                                            
         MVI   3(RE),C' '                                                       
         CLI   DUB,10              PUT '>' AT THE END WHEN > 10 SYS             
         BNH   *+8                                                              
         MVI   3(RE),C'>'                                                       
*                                                                               
         CLI   SVTEAM,0                                                         
         BE    EXIT                                                             
*                                                                               
         LA    R1,TEAMLST                                                       
         USING TEAMLSTD,R1                                                      
DS140    CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         CLC   SVTEAM,TEAMNUM                                                   
         BE    DS145                                                            
         LA    R1,TEAMLLEN(,R1)                                                 
         B     DS140                                                            
DS145    MVC   LISTTEAM,TEAMNAME                                                
         DROP  R1                                                               
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SYSTEMS SUMMARY HEADER                           *         
***********************************************************************         
         SPACE 1                                                                
DISSHD   NTR1                                                                   
         MVI   SYSNAMS,C' '                                                     
         MVC   SYSNAMS+1(L'SYSNAMS-1),SYSNAMS                                   
         MVC   SYSNAMS(7),=C'SYSTEMS'                                           
         NC    SYSNAMS+1(6),=8X'BF'                                             
         MVC   SYSNAMS+8(8),=C'ASSIGNED'                                        
         NC    SYSNAMS+9(7),=8X'BF'                                             
         LA    RE,SYSNAMS+18       SET A(NEXT ENTRY)                            
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
         LA    R3,CT5DATA                                                       
*                                                                               
DSHD10   CLI   0(R3),0             TEST END OF RECORD                           
         BE    DSHD100                                                          
         CLI   0(R3),CTSYSELQ      TEST SYSTEM ELEMENT                          
         BE    DSHD30                                                           
DSHD20   ZIC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         B     DSHD10                                                           
*                                                                               
         USING CTSYSD,R3                                                        
DSHD30   L     R4,ASYS             SEARCH SE LIST FOR SE NUM SESYSNUM           
         L     R4,VSELIST-SYSFACD(R4)                                           
         LH    RE,0(R4)                                                         
         L     RF,2(R4)                                                         
         LA    R4,6(R4)            R3=A(SELIST ENTRY)                           
         USING SELISTD,R4                                                       
         CLC   CTSYSNUM,SEOVSYS                                                 
         BE    DSHD40                                                           
         BXLE  R4,RE,*-10                                                       
         LA    R4,=CL7'XXX    '    SET UNKNOWN SYSTEM NAME                      
*                                                                               
DSHD40   L     RE,ASYSNAMS         MOVE NAME TO LIST                            
         SR    R1,R1                                                            
         ICM   R1,1,SYCNT          TEST COUNT OF ITEMS IN LIST                  
         BZ    *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SYCNT            BUMP ITEM COUNT                              
         LA    RF,SYSNAMS+L'SYSNAMS-3                                           
         CR    RE,RF                                                            
         BH    DSHD20                                                           
         MVC   0(3,RE),SENAME      EXTRACT SE NAME                              
         NC    1(2,RE),=8X'BF'     SET TO LOWER CASE                            
         LA    RE,3(RE)                                                         
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
         B     DSHD20                                                           
         DROP  R4                                                               
*                                                                               
DSHD100  MVI   SECSHD,C'-'         DISPLAY LIST OF SYSTEMS                      
         MVC   SECSHD+1(L'SECSHD-1),SECSHD                                      
         SR    RF,RF                                                            
         ICM   RF,1,SYSNAMSL       RF=L'SYSTEM NAMES LIST                       
         BZ    DSHD110                                                          
         LA    R1,L'SECSHD                                                      
         SR    R1,RF                                                            
         BNP   DSHD110                                                          
         SRL   R1,1                                                             
         LA    RE,SECSHD(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAMS                                                  
DSHD110  OI    SECSHDH+6,X'80'                                                  
*                                                                               
DSHDX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET AGENCY COUNTRY CODE FROM ACCESS RECORD (ADDRESS AT R2)          *         
***********************************************************************         
         SPACE 1                                                                
GETACTY  NTR1                                                                   
         MVC   ACCTRY,CUCTRY                                                    
         LA    R3,CT5DATA                                                       
*                                                                               
CCTY010  CLI   0(R3),0             TEST END OF RECORD                           
         BE    CCTYX                                                            
         CLI   0(R3),CTAGDELQ      TEST AGENCY DETAILS ELEMENT                  
         BE    CCTY030                                                          
CCTY020  ZIC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         B     CCTY010                                                          
*                                                                               
         USING CTAGDD,R3                                                        
CCTY030  MVC   ACCTRY,CTAGDCTY     SAVE AGENCY ACCESS COUNTRY CODE              
         B     CCTYX                                                            
         DROP  R3                                                               
*                                                                               
CCTYX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PROGRAM NAME LIST AND CODE SAVE TABLE              *         
***********************************************************************         
         SPACE 1                                                                
INITPGML NTR1                                                                   
         BAS   RE,GETSE            GET SELIST ENTRY FOR SYSTEM                  
         L     R1,ASE                ADDRESS IN ASE                             
         L     R1,SEPGMS-SELISTD(R1)                                            
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         ST    R1,APFULL                                                        
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         LA    R4,XSORTBLK         SORT PROGRAM NAMES ALPHA                     
         SR    R8,R8               COUNT NUMBER OF PROGRAMS                     
         XC    PROGRAM,PROGRAM                                                  
*                                  SORT PROGRAM NAMES ALPHABETICALLY            
IPL010   EQU   *                                                                
*&&UK                                                                           
         CLI   PGMCTRY,0           TEST COUNTRY CODE (NOT IN US)                
         BE    *+14                  KEEP ENGLISH DEFAULT                       
         CLC   PGMCTRY,ACCTRY        AND CONNECT COUNTRY                        
         BNE   IPL011                                                           
*&&                                                                             
         MVC   L'PGMNAME(1,R4),PGMNUM                                           
         MVC   L'PGMNAME+1(1,R4),PGMIND2                                        
         MVC   0(L'PGMNAME,R4),PGMNAME                                          
         LA    R4,L'PGMNAME+2(R4)                                               
         LA    R8,1(R8)                                                         
IPL011   BXLE  R1,RE,IPL012                                                     
         B     IPL016                                                           
IPL012   MVC   PROGRAM,PGMNUM                                                   
         MVC   PROGCTRY,PGMCTRY                                                 
         LR    R0,RF                                                            
         LR    RF,R1                                                            
         SR    RF,RE                                                            
         L     R3,APFULL                                                        
IPL013   EQU   *                                                                
*&&UK                                                                           
         CLC   ACCTRY,PGMCTRY-PGMLSTD(R3)                                       
         BE    *+14                NO SYNONYM CHECK ACROSS COUNTRIES            
         CLC   PROGCTRY,PGMCTRY-PGMLSTD(R3)                                     
         BNE   IPL014                                                           
*&&                                                                             
         CLC   PROGRAM,PGMNUM-PGMLSTD(R3)                                       
         BNE   IPL014              AVOID SYNONOMOUS PROGRAMS                    
         LR    RF,R0                                                            
         B     IPL011                                                           
IPL014   BXLE  R3,RE,IPL013                                                     
         LR    RF,R0                                                            
         B     IPL010                                                           
*                                                                               
IPL016   LA    R4,XSORTBLK                                                      
         LA    R0,L'PGMNAME                                                     
         LA    R3,L'PGMNAME+L'PGMNUM+L'PGMIND2                                  
         GOTO1 VXSORT,APPARM,(X'00',(R4)),(R8),(R3),(R0),0                      
*                                                                               
         TWAXC SECPN01H,PROT=Y                                                  
         LA    R3,PSAVTAB          POINT TO SAVE TABLE                          
         LTR   R8,R8                                                            
         BZ    IPLX                EXIT IF NO PROGRAMS IN LIST                  
         LA    R1,SECPN01H         BUILD PROGRAM CODE SAVE TABLE                
         LR    RF,R1                 AND DISPLAY PROGRAM NAMES                  
         LA    RE,SECTENDH         SAVE A(DISPLAY TABLE END)                    
         LHI   R0,6                                                             
         USING PGMLD,R1                                                         
IPL020   CR    R1,RE               DIE IF OVERFLOW                              
         BL    *+6                                                              
         DC    H'00'                                                            
         MVC   0(1,R3),L'PGMNAME(R4)  SAVE PROGRAM CODE                         
         MVC   1(1,R3),L'PGMNAME+1(R4)  SAVE PROGRAM IND2                       
         S     R1,APRELO           SAVE DISPLAY ADDRESS                         
         ST    R1,2(R3)              IN RELOCATABLE FORM                        
         A     R1,APRELO                                                        
         LA    R3,L'PSAVTAB(R3)    BUMP SAVE TABLE POINTER                      
*                                  DISPLAY PROGRAM NAME                         
         MVC   PGMLNAM(L'PGMLNAM),0(R4)                                         
         OI    PGMLNAMH+(FVOIND-FVIHDR),FVOXMT                                  
         NI    PGMLNAMH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         XC    PGMLVAL,PGMLVAL                                                  
         OI    PGMLVALH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PGMLVALH+(FVATRB-FVIHDR),FVAHIGH                                 
         BCT   R0,IPL022                                                        
         LA    R1,PGMLINE(RF)                                                   
         LA    R0,6                                                             
         LR    RF,R1                                                            
         B     IPL024                                                           
IPL022   LA    R1,PGMLLEN(R1)                                                   
IPL024   LA    R4,L'PGMNAME+2(R4)                                               
         BCT   R8,IPL020                                                        
         DROP  R1                                                               
*                                                                               
IPLX     XC    0(L'PSAVTAB,R3),0(R3)  MARK END OF SAVE TABLE                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOCATE SELIST ENTRY FOR SYSTEM AND SAVE AT ASE                      *         
***********************************************************************         
GETSE    NTR1                                                                   
         L     R3,ASYS                                                          
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3                                                       
*                                                                               
         CLC   SYSTEM,SEOVSYS                                                   
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
*                                                                               
         ST    R3,ASE              SAVE A(SELIST ENTRY)                         
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
********************************************************************            
*   DISPLAY PROGRAM ACCESS CODES                                   *            
*   R3 = SYS ELEMENT                                               *            
********************************************************************            
         USING CTSYSD,R3                                                        
DISPGMS  NTR1                                                                   
         LA    R4,PSAVTAB          POINT TO PROGRAM CODE SAVE TABLE             
*                                                                               
         USING PGMLD,R8                                                         
DPGS010  OC    0(L'PSAVTAB,R4),0(R4)                                            
         BZ    DPGSX               EXIT AT END OF TABLE                         
         L     R8,2(R4)                                                         
         A     R8,APRELO                                                        
         MVC   PROGRAM,0(R4)       SAVE PROGRAM CODE                            
         MVC   PROGIND2,1(R4)      SAVE PROGRAM IND2                            
         MVI   PACCVAL,C'N'                                                     
         CLI   CTSYSLEN,X'18'      CHECK EXISTING FLAGS PRESENT                 
         BE    DPGS012                                                          
         TM    PROGIND2,PGMISECA   ELSE USE PROGRAM INDICATORS DEFAULT          
         BZ    DPGS014                                                          
         TM    PROGIND2,PGMISECB                                                
         BNZ   DPGS014                                                          
         MVI   PACCVAL,C'Y'                                                     
         B     DPGS020                                                          
*                                                                               
DPGS012  TM    PROGIND2,PGMISECB   TEST NEW/OLD SEC SWITCHABLE                  
         BNZ   DPGS013                                                          
         TM    PROGIND2,PGMISECA   TEST IF NEW SECURITY ONLY                    
         BZ    DPGS014                                                          
         MVI   PACCVAL,C'Y'                                                     
         B     DPGS020                                                          
*                                                                               
DPGS013  BAS   RE,GETPAVAL         GET ACCESS FLAG FROM SYSTEM ELEMENT          
         CLI   PACCVAL,C'Y'                                                     
         BE    DPGS020                                                          
*                                                                               
DPGS014  OI    PGMLNAMH+(FVATRB-FVIHDR),FVAHIGH                                 
         NI    PGMLVALH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         B     DPGS030                                                          
*                                                                               
DPGS020  NI    PGMLNAMH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         OI    PGMLVALH+(FVATRB-FVIHDR),FVAHIGH                                 
*                                                                               
*                                  DISPLAY ACCESS CODE VALUE IN TABLE           
DPGS030  MVC   PGMLVAL,PACCVAL                                                  
         LA    R4,L'PSAVTAB(R4)                                                 
         B     DPGS010             DO NEXT TABLE ENTRY                          
*                                                                               
DPGSX    B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
********************************************************************            
*   GET PROGRAM ACCESS FLAG VALUE FROM SYSTEM ELEMENT              *            
*   ON INPUT PROGRAM 1 BYTE PROGRAM CODE, R3 POINTS TO SYSTEM ELEM.*            
*   ON OUTPUT PACCVAL LIMIT ACCESS FLAG IF FOUND ELSE CC .NE.      *            
********************************************************************            
         SPACE 1                                                                
         USING CTSYSD,R3                                                        
GETPAVAL NTR1                                                                   
         LA    RE,1                                                             
         SLL   RE,31                                                            
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         IC    R1,PROGRAM                                                       
         BCTR  R1,0                                                             
         SRDL  RE,0(R1)                                                         
         LTR   RE,RE                                                            
         BZ    GPAV010                                                          
         ICM   RF,15,CTSYSPGM                                                   
         NR    RE,RF                                                            
         BZ    GPAVNO                                                           
         B     GPAVYES                                                          
*                                                                               
GPAV010  ICM   RE,15,CTSYSPGM+4                                                 
         NR    RF,RE                                                            
         BZ    GPAVNO                                                           
         B     GPAVYES                                                          
*                                                                               
GPAVNO   B     NO                  PROGRAM NOT FOUND                            
*                                                                               
GPAVYES  MVI   PACCVAL,C'Y'        SAVE ACCESS FLAG VALUE                       
         B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
********************************************************************            
*   VALIDATE PROGRAM ACCESS CODES                                  *            
*   R3 = SYS ELEMENT                                               *            
********************************************************************            
         USING CTSYSD,R3                                                        
VALPGMS  NTR1                                                                   
         LA    R2,PSAVTAB          POINT TO PROGRAM CODE SAVE TABLE             
*                                                                               
VPGS010  OC    0(L'PSAVTAB,R2),0(R2)                                            
         BZ    VPGS200             END OF TABLE                                 
         L     R1,2(R2)            GET DISPLAY ADDRESS                          
         A     R1,APRELO                                                        
         LA    R1,PGMLVALH-PGMLD(R1)                                            
         XC    PACCVAL,PACCVAL                                                  
         GOTO1 AFVAL               VALIDATE ACCESS CODE FIELD                   
         BNE   VPGS100                                                          
         CLI   FVILEN,0                                                         
         BE    VPGS100                                                          
*                                                                               
VPGS020  LA    RE,1                                                             
         SLL   RE,31                                                            
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         BCTR  R1,0                                                             
         SRDL  RE,0(R1)                                                         
*                                  VALIDATE CODE RETURN IN APWORK               
         TM    1(R2),PGMISECA                                                   
         BNO   VPGS030                                                          
         CLI   ALLPGM,0                                                         
         BE    *+12                                                             
         CLI   ALLPGM,C'Y'                                                      
         BE    VPGS050                                                          
         CLI   FVIFLD,C'Y'                                                      
         BE    VPGS050                                                          
         TM    1(R2),PGMISECB                                                   
         BNO   EIIF                                                             
*                                                                               
VPGS030  CLI   ALLPGM,0                                                         
         BE    VPGS032                                                          
         CLI   ALLPGM,C'Y'                                                      
         BE    VPGS050                                                          
         B     VPGS040                                                          
VPGS032  CLI   FVIFLD,C'Y'                                                      
         BE    VPGS050                                                          
         CLI   FVIFLD,C'N'                                                      
         BNE   EIIF                                                             
*                                                                               
VPGS040  LTR   RE,RE                                                            
         BZ    VPGS042                                                          
         X     RE,FFILL                                                         
         N     RE,CTSYSPGM                                                      
         ST    RE,CTSYSPGM                                                      
         B     VPGS100                                                          
*                                                                               
VPGS042  X     RF,FFILL                                                         
         N     RF,CTSYSPGM+4                                                    
         ST    RF,CTSYSPGM+4                                                    
         B     VPGS100                                                          
*                                                                               
VPGS050  LTR   RE,RE                                                            
         BZ    VPGS052                                                          
         O     RE,CTSYSPGM                                                      
         ST    RE,CTSYSPGM                                                      
         B     VPGS100                                                          
*                                                                               
VPGS052  O     RF,CTSYSPGM+4                                                    
         ST    RF,CTSYSPGM+4                                                    
         B     VPGS100                                                          
*                                                                               
VPGS100  LA    R2,L'PSAVTAB(R2)                                                 
         B     VPGS010             GET NEXT PROGRAM                             
*                                                                               
VPGS200  MVI   CTSYSLEN,X'18'      SAVE NEW LENGTH OF SYSTEM ELEMENT            
         B     VPGSYES                                                          
*                                                                               
VPGSNO   B     NO                                                               
*                                                                               
VPGSYES  B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                  ERROR EXITS                                  
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
ESYS     MVC   FVMSGNO,=AL2(FVFESYS)                                            
         B     NO                  SYSTEM NAME ERROR                            
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE EXCEEDS MAXIMUM                  
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
FFILL    DC    32X'FF'                                                          
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    132C' '                                                          
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
*                                                                               
       ++INCLUDE DDTEAMLST                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDTEAMLSTD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
*                                                                               
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENE3D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENE9D                                                       
         SPACE 1                                                                
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 SAVE CURRENT SYSTEM                          
*                                                                               
* TABLE OF SYSTEM PROGRAM CODES AND CORRESPONDING DISPLAY ADDRESSES             
* EACH ENTRY IS 1 BYTE PROGRAM CODE, 1 BYTE PROGRAM INDICATOR 2                 
* AND 4 BYTES DISPLAY ADDRESS (-RELO)                                           
PSAVTAB  DS    84XL6               TABLE ENTRIES                                
*                                                                               
PGMLD    DSECT                     ** PROGRAM LIST DISPLAY DSECT **             
PGMLNAMH DS    CL8                                                              
PGMLNAM  DS    CL(L'SECPN01)                                                    
PGMLVALH DS    CL8                                                              
PGMLVAL  DS    CL(L'SECPV01)                                                    
PGMLLEN  EQU   *-PGMLD                                                          
PGMLINE  EQU   SECPN07H-SECPN01H                                                
         EJECT                                                                  
LOCALD   DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    XL64                                                             
ASE      DS    A                                                                
APGM     DS    A                                                                
VXSORT   DS    A                                                                
PROGRAM  DS    CL1                 PROGRAM CODE                                 
PROGCTRY DS    CL1                 PROGRAM COUNTRY CODE                         
ACCTRY   DS    CL1                 AGENCY ACCESS RECORD COUNTRY CODE            
PROGIND2 DS    CL1                 PROGRAM INDICATOR 2                          
PACCVAL  DS    XL1                 PROGRAM ACCESS FLAG                          
MGNAME   DS    CL8                 PROGRAM NAME SAVE                            
SYSTEM   DS    CL1                 SYSTEM SE NUMBER SAVE                        
SYCNT    DS    CL1                                                              
ALLPGM   DS    CL1                 ALL PROGRAM FLAG OVERIDE                     
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
SVTEAM   DS    X                   SERVICE TEAM NUMBER                          
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
*                                                                               
SYSEL    DS    XL(L'APELEM)                                                     
*                                                                               
SELKEY   DS    0XL9                SELECT KEY PARAMETERS                        
SELALPH  DS    CL2                 ALPHA ID                                     
SELSYS   DS    CL1                 SYSTEM #                                     
SELPGM   DS    CL1                 PROGRAM #                                    
SELAAWA  DS    CL1                 ACCESS AWARE (Y/N)                           
SELAGYO  DS    CL1                 SECURITY AGENCY ONLY (Y/N)                   
*                                                                               
LISTBUFF DS    CL256               SYSTEM INFO DISPLAY BUFFER                   
*                                                                               
XSORTBLK DS    84XL(10)                                                         
*                                                                               
LOCALX   EQU   *                                                                
*                                                                               
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL(L'LSTACT1)       ACTION FIELD                                 
LISTLINH DS    XL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTALPH DS    CL2                 AGENCY ACCESS ALPHA CODE                     
         DS    CL3                                                              
LISTSECA DS    CL2                 SECURITY AGENCY                              
         DS    CL3                                                              
LISTACCS DS    CL44                SYSTEM LIST VALUES                           
         DS    CL2                                                              
LISTPGMN DS    CL7                 PROGRAM NAME                                 
         DS    CL3                                                              
LISTTEAM DS    CL8                 SERVICE TEAM                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013CTGEN1C   11/08/13'                                      
         END                                                                    
