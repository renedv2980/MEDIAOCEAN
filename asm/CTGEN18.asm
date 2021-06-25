*          DATA SET CTGEN18    AT LEVEL 013 AS OF 11/06/17                      
*PHASE TA0B18A                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE NUMVAL                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE PWDVAL                                                                 
***********************************************************************         
* AHYD NOV06/14 - IN THE "DDS AGY TYPE" FIELD YOU CAN ADD SAP BUT               
*                 TO REMOVE IT YOU WILL NEED TO PFM IT OFF. THIS IS             
*                 SO NO ONE REMOVES IT ACCIDENTLY                               
***********************************************************************         
         TITLE 'CTGEN18 - FILE MAINTENANCE - ACCESS TYPE (5) RECORDS'           
GEN18    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GE18**,RA,R8,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         L     R0,=V(SCINKEY)                                                   
         AR    R0,RE                                                            
         ST    R0,VSCINKEY                                                      
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
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
         B     RESREC                                                           
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
MODEXIT  CLI   APMODE,APMVALK          VALKEY?                                  
         JE    EXIT                    YES: EXIT                                
         CLI   APPFKEY,2               PFKEY PRESSED?                           
         JNE   EXIT                    NO: EXIT                                 
         CLI   APACTN,ACTDIS           ACTION=DISPLAY                           
         JE    MODEXIT1                YES: SWAP                                
         CLI   APACTN,ACTCHA           ACTION=CHANGE                            
         JNE   EXIT                    NO: EXIT                                 
MODEXIT1 XC    APCURSOR,APCURSOR       DON'T SET CURSOR                         
         MVI   APMODE,APMSWP           SWAP                                     
         MVI   APPARM,RECADA           SWAP RECORD                              
         MVI   APPARM+1,ACTDIS         SWAP ACTION                              
         CLI   APPFKEY,0                                                        
EXIT     XIT1  ,                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF ACCESS TYPE RECORD                       *         
***********************************************************************         
VALKEY   LA    R2,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,SYSALPHH                                                   
         BNE   VALKEYX                                                          
*&&DO                                                                           
         CLI   APACTN,ACTADD       NO NEW ALPHA IDS W/ C'+'                     
         BNE   VK012                                                            
         CLI   FVIFLD,C'+'         PLUS SIGN CAUSES PROBLEM FOR =PC             
         BE    VK010                                                            
         CLI   FVIFLD+1,C'+'                                                    
         BNE   VK012                                                            
VK010    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYX                                                          
*&&                                                                             
VK012    MVC   CT5KALPH,FVIFLD                                                  
         MVC   APRECKEY(L'CT5KEY),CT5KEY                                        
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
VALKEYX  B     MODEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN ACCESS TYPE RECORD                      *         
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
         MVI   MEDSE,0                                                          
         MVI   MEDAGB,0                                                         
         XC    PRELAB,PRELAB                                                    
*&&US*&& XC    OLDCAGEL,OLDCAGEL   CLEAR ORIGINAL CFM AGY LIST ELEM             
         CLI   APACTN,ACTADD                                                    
         BE    VRADD                                                            
*                                  CHANGE FUNCTION - SAVE SYSTEM ELMS.          
VRCHA    LA    R3,CT5DATA          AND STRIP DOWN RECORD                        
VRCHA10  CLI   0(R3),0                                                          
         BE    VRCHAX                                                           
         CLI   0(R3),X'01'                                                      
         BE    VRCHA30                                                          
         CLI   0(R3),X'02'                                                      
         BE    VRCHA30                                                          
         CLI   0(R3),CTAGDELQ     X'B4'                                         
         BE    VRCHA21                                                          
         CLI   0(R3),CTAGCELQ     X'1B'                                         
         BE    VRCHA22                                                          
         CLI   0(R3),CTAGLELQ     X'B6'                                         
         BE    VRCHA30                                                          
         CLI   0(R3),WEBELQ                                                     
         BE    VRCHA30                                                          
*&&US                                                                           
         CLI   0(R3),CTSPUELQ                                                   
         BE    VRCHA30                                                          
         CLI   0(R3),CTCAGELQ                                                   
         BE    VRCHA24                                                          
*&&                                                                             
         CLI   0(R3),CTSEAELQ                                                   
         BE    VRCHA30                                                          
         CLI   0(R3),CTAADELQ     X'B9'                                         
         BE    VRCHA30                                                          
         CLI   0(R3),CTTOUELQ                                                   
         BE    VRCHA30                                                          
         CLI   0(R3),CTSYSELQ                                                   
         BNE   VRCHA20                                                          
         MVI   0(R3),X'FF'         MARK SYSTEM ELEMENT                          
*                                  GET NEXT ELEMENT                             
VRCHA20  ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRCHA10                                                          
*                                                                               
         USING CTAGDD,R3                                                        
VRCHA21  MVC   PREAGOPT,CTAGOPTS   SAVE OFF OPTIONS                             
         B     VRCHA30                                                          
         DROP  R3                                                               
*                                                                               
         USING CTAGCD,R3                                                        
VRCHA22  MVC   PRELAB,CTAGCCOD                                                  
         B     VRCHA30                                                          
         DROP  R3                                                               
*&&US                                                                           
         USING CTCAGD,R3                                                        
VRCHA24  MVC   OLDCAGEL,CTCAGEL    ORIGINAL CFM AGENCY LIST ELEMENT             
         B     VRCHA30                                                          
         DROP  R3                                                               
*&&                                                                             
VRCHA30  SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),CT5REC),0,0                    
         CLI   APPARM+12,0                                                      
         JNE   *+2                                                              
         B     VRCHA10                                                          
*                                                                               
VRCHAX   B     VRDATA                                                           
*                                                                               
VRADD    XC    CT5REC(256),CT5REC  ADD FUNCTION                                 
         MVC   CT5KEY,APRECKEY                                                  
         LA    R0,CT5DATA+1-CT5REC                                              
         STCM  R0,3,CT5LEN                                                      
*                                                                               
VRDATA   LA    R3,APELEM                                                        
         MVI   CTRY,0                                                           
         XC    CURR,CURR                                                        
         MVI   FVMINL,1            SET FIELD REQUIRED FLAG                      
         GOTO1 AFVAL,SYSCTRYH      VALIDATE COUNTRY                             
         BNE   VALRECX                                                          
         GOTO1 GETCT                                                            
         BNE   VALRECX                                                          
         MVC   CTRY(L'CTRYCODE),APWORK                                          
         CLC   APWORK+L'CTRYCODE(L'CTRYNAM),SYSCTRY                             
         BE    *+14                                                             
         MVC   SYSCTRY(L'CTRYNAM),APWORK+L'CTRYCODE                             
         OI    SYSCTRYH+6,X'80'    DISPLAY FULL NAME                            
         EJECT                                                                  
*                                                                               
DATAV6   XC    SYSUSNM,SYSUSNM                                                  
         OI    SYSUSNMH+6,X'80'                                                 
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,SYSUSERH      VALIDATE PRINCIPAL ID                        
         BNE   VALRECX                                                          
         GOTO1 VALID                                                            
         BNE   VALRECX                                                          
         MVC   SYSUSNM,USERNM      DISPLAY ID NAME                              
         USING CTDSCD,R3                                                        
         MVI   CTDSCEL,CTDSCELQ    ADD ID NUMBER ELEMENT                        
         MVI   CTDSCLEN,4                                                       
         MVC   CTDSC(2),USERNO                                                  
         GOTO1 AADDELS,CT5REC                                                   
*                                                                               
         XC    SYSGRNM,SYSGRNM                                                  
         OI    SYSGRNMH+6,X'80'                                                 
         XC    GRUPID,GRUPID                                                    
*&&UK*&& MVI   FVMINL,1                                                         
*&&US*&& MVI   FVMINL,0                                                         
         GOTO1 AFVAL,SYSGRUPH      VALIDATE AGENCY GROUP                        
*&&UK*&& BNE   VALRECX                                                          
*&&US*&& BNE   DATAV7                                                           
         GOTO1 VALID                                                            
         BNE   VALRECX                                                          
         MVC   SYSGRNM,USERNM      DISPLAY GROUP NAME                           
         MVC   GRUPID,USERNO       SAVE GROUP NUMBER                            
*                                                                               
DATAV7   XC    SYSHONM,SYSHONM                                                  
         OI    SYSHONMH+6,X'80'                                                 
         XC    HOLDID,HOLDID                                                    
*&&UK*&& MVI   FVMINL,1                                                         
*&&US*&& MVI   FVMINL,0                                                         
         GOTO1 AFVAL,SYSHOLDH      VALIDATE HOLDING COMPANY                     
*&&UK*&& BNE   VALRECX                                                          
*&&US*&& BNE   DATAV9                                                           
         GOTO1 VALID                                                            
         BNE   VALRECX                                                          
         MVC   SYSHONM,USERNM      DISPLAY COMPANY NAME                         
         MVC   HOLDID,USERNO       SAVE COMPANY NUMBER                          
*                                                                               
DATAV9   MVI   FVMINL,1                                                         
         GOTO1 AFVAL,SYSTYPEH      VALIDATE TYPE FIELD                          
         BNE   VALRECX                                                          
         MVC   CLTYPE,FVIFLD       SAVE TYPE VALUE                              
         CLI   CTRY,CTRYGER        CHECK VALUE FOR GERMANY                      
         BNE   DATAV91                                                          
         CLI   CLTYPE,CTAGYTTQ                                                  
         BE    DATAV91                                                          
         CLI   CLTYPE,CTAGYTDQ                                                  
         BE    DATAV91                                                          
         CLI   CLTYPE,CTAGYTCQ                                                  
         BE    DATAV91                                                          
         CLI   CLTYPE,CTAGYTRQ                                                  
         BE    DATAV91                                                          
         CLI   CLTYPE,CTAGYTOQ                                                  
         BE    DATAV91                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
DATAV91  XC    DDSACC,DDSACC                                                    
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,SYSDDSAH      VALIDATE DDS ACCESS LEVEL FIELD              
         BNE   DATAV9A                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   *+12                                                             
         OI    DDSACC,CTAGDDAY     BYPASS PASSWORD SECURITY ACCESS              
         B     DATAV9A                                                          
         CLI   FVIFLD,C'N'                                                      
         BNE   *+12                                                             
         OI    DDSACC,CTAGDDAN     NO DDS TERMINAL ACCESS                       
         B     DATAV9A                                                          
         CLI   FVIFLD,C'P'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         OI    DDSACC,CTAGDDAP     NO DDS AGENCY PASSWORD ALLOWED               
*                                                                               
         USING CTAGDD,R3                                                        
DATAV9A  XC    CTAGDEL(CTAGDL2Q),CTAGDEL                                        
         MVI   CTAGDEL,CTAGDELQ    ADD AGENCY GROUP ELEMENT                     
         MVI   CTAGDLEN,CTAGDL2Q                                                
         MVC   CTAGDHG,HOLDID                                                   
         MVC   CTAGDAG,GRUPID                                                   
         MVC   CTAGDTY,CLTYPE                                                   
         MVC   CTAGDCTY,CTRY                                                    
         MVC   CTAGDCUR,CURR                                                    
         MVC   CTAGDDA,DDSACC                                                   
         MVC   CTAGOPTS,PREAGOPT   RESTORE                                      
         NI    CTAGOPTS,255-(CTAGTST+CTAGTNG+CTAGUAT)                           
*                                                                               
DVALAGY  GOTO1 AFVAL,SYSDTAH       VALIDATE DDS TEST AGENCY                     
         BNE   DVALAG90                                                         
         OI    SYSDTAH+6,X'80'     RETRANSMIT FIELD                             
         GOTO1 VSCANNER,APPARM,SYSDTAH,(2,BLOCKI),0                             
         CLI   4(R1),0             ANY PARAMETERS                               
         BE    DVALAG90                                                         
*                                                                               
         USING SCANBLKD,R4                                                      
         LA    R4,BLOCKI           R4=A(SCAN BLOCK)                             
         LLC   R0,4(R1)            LOOP FOR # OF PARAMS                         
         MVI   BYTE,0                                                           
*                                                                               
         USING AGYTYPD,RE                                                       
DVALAG10 LARL  RE,AGYTYP           DDS TYPE AGENCY TABLE                        
DVALAG12 CLC   SC1STLEN,AGYMINL    INPUT LENGTH TO SHORT?                       
         BL    DVALAG80            TRY NEXT ENTRY                               
         CLC   SC1STLEN,AGYMAXL    INPUT LENGTH TO LONG?                        
         BH    DVALAG80            TRY NEXT ENTRY                               
         LLC   RF,SC1STLEN         GET LENGTH OF INPUT                          
         BCTR  RF,0                                                             
         EX    RF,AGYTYCLC                                                      
         BNE   DVALAG80            TRY NEXT ENTRY                               
         LLC   RF,AGYCOMBO                                                      
         EX    RF,AGYCOTM                                                       
         BO    DVALAG82            NOT VALID, COMBO BIT ALREADY ON              
         OC    BYTE,AGYCOMBO       SAVE OFF BITS THAT ARE ONE                   
         OC    CTAGOPTS,AGYOPT     OR ON FLAGS                                  
         AHI   R4,SCBLKLQ          NEXT                                         
         BCT   R0,DVALAG10         DO IT AGAIN                                  
         B     DVALAG90                                                         
*                                                                               
DVALAG80 CLI   0(RE),X'FF'         EOF TABLE                                    
         BNE   DVALAG88            KEEP GOING                                   
DVALAG82 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
DVALAG88 LA    RE,AGYTYLNQ(,RE)    NEXT ENTRY                                   
         B     DVALAG12                                                         
* ------------------------                                                      
* EXCUTED INSTRUCTION                                                           
* ------------------------                                                      
AGYCOTM  TM    BYTE,0                                                           
AGYTYCLC CLC   SC1STFLD(0),AGYTEXT                                              
         DROP  R4,RE                                                            
*                                                                               
DVALAG90 TM    CTAGOPTS,CTAGUAT    UAT AGENCY INDICATOR ON?                     
         BZ    *+12                NO                                           
         BRAS  RE,CHKUAT           YES: CHECK USER IDS                          
         BNE   VALRECX                                                          
         GOTO1 AADDELS,CT5REC                                                   
         BRAS  RE,DISPAGYT                                                      
         DROP  R3                                                               
*&&US                                                                           
DVCAG    MVI   FVMINL,0            VALIDATE CFM AGENCY LIST ELEMENT             
         GOTO1 AFVAL,SYSCAGH                                                    
         BNE   DVCAGX                                                           
         BRAS  RE,CHKCAG           CHECK CFM AGENCIES AND BUILD ELEM            
         BL    VALRECX             ERROR WITH FIELD                             
         BH    DVCAGX              ELEMENT NOT NEEDED                           
         GOTO1 AADDELS,CT5REC                                                   
DVCAGX   EQU   *                                                                
*                                                                               
DVSPU    EQU   *                   VALIDATE SECURITY PRINCIPLE USER ID          
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         XC    SYSSPUN,SYSSPUN                                                  
         OI    SYSSPUNH+6,X'80'                                                 
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,SYSSPUH                                                    
         BNE   DVSPUX                                                           
         GOTO1 VALID                                                            
         BNE   VALRECX                                                          
         MVC   SYSSPUN,USERNM      DISPLAY ID NAME                              
         USING CTSPUD,R3                                                        
         MVI   CTSPUEL,CTSPUELQ    ADD ELEMENT                                  
         MVI   CTSPULEN,CTSPULNQ                                                
         MVC   CTSPUNUM(2),USERNO                                               
         GOTO1 AADDELS,CT5REC                                                   
DVSPUX   EQU   *                                                                
*&&                                                                             
         MVI   FVMINL,0            SET OPTIONAL FIELD FLAG                      
         LA    R9,SYSACCSH         VALIDATE SYSTEM ACCESS LIST                  
         LA    R0,2                                                             
         XC    OVSYS,OVSYS                                                      
         B     DATAV10                                                          
*                                                                               
DATAV8   SR    RF,RF               BUMP TO NEXT TWA FIELD                       
         IC    RF,0(R9)                                                         
         AR    R9,RF                                                            
         TM    1(R9),X'20'         TEST PROTECTED                               
         BNZ   DATAV8                                                           
*                                                                               
DATAV10  LR    R1,R9               VALIDATE NEXT SCAN FIELD                     
         GOTO1 AFVAL                                                            
         BNE   DATAV14                                                          
         MVI   FVINDX,0                                                         
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCKI)                               
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         LA    R4,BLOCKI           POINT TO START OF FIELD SCAN LIST            
         MVC   FLDCNT,4(R1)        # SCAN BLOCK ENTRIES                         
         MVI   FVINDX,1            SCAN BLOCK ENTRY INDEX                       
*                                                                               
DATAV12  CLC   FVINDX,FLDCNT       VALIDATE EACH FIELD IN SCAN                  
         BH    DATAV14                                                          
         CLI   0(R4),L'SENAME      VALIDATE SYSTEM NAME                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         MVC   APWORK(L'SENAME),12(R4)                                          
*                                  GET SYSTEM LIST INFO                         
         GOTO1 GETSE                                                            
         BNE   VALRECX                                                          
         ZIC   RE,APWORK+1         VALIDATE SYSTEM CODE                         
         LA    RE,OVSYS(RE)                                                     
         CLI   0(RE),0             ENSURE SYSTEM NOT DUPLICATE                  
         MVC   0(1,RE),APWORK+1                                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     VALRECX                                                          
*                                  PREPARE SYSTEM ELEMENT                       
         XC    APELEM,APELEM                                                    
         USING CTSYSD,R3                                                        
         MVI   CTSYSEL,CTSYSELQ                                                 
         MVI   CTSYSLEN,CTSYSPGM-CTSYSD                                         
         MVC   CTSYSNUM,APWORK+1                                                
         CLI   APACTN,ACTADD                                                    
         BE    DATAV12E                                                         
         GOTO1 VHELLO,APPARM,(C'G',CTFILE),(X'FF',CT5REC),(1,APWORK+1)          
         CLI   12(R1),X'06'                                                     
         BE    DATAV12E                                                         
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R1,12(R1)                                                        
         LA    R1,0(R1)                                                         
         ZIC   RF,1(R1)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)                                                    
         MVI   CTSYSEL,CTSYSELQ                                                 
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),(X'FF',CT5REC),(1,APWORK+1)          
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
DATAV12E MVC   CTSYSSE,APWORK                                                   
         MVI   CTSYSIND,CTSYSINF                                                
         XC    CTSYSAGB,CTSYSAGB                                                
*&&US                                                                           
         CLI   CTSYSNUM,2          SPOT SYSTEM                                  
         BNE   DATAV12K                                                         
         CLI   SYSCABL,C'Y'        USES AMS CABLE DATA?                         
         BNE   *+8                                                              
         OI    CTSYSIND,CTSYSNCA   YES, FLAG IT IN RECORD                       
         CLI   SYSMST,C'S'         M STREET SUBSCRIBER?                         
         BNE   *+8                 NO                                           
         OI    CTSYSIND,CTSYSRAD   YES, FLAG IT IN RECORD                       
         CLI   SYSMST,C'F'         MEDIA FRAMEWORKS SUBSCRIBER?                 
         BNE   *+8                 NO                                           
         OI    CTSYSIND,CTSYSMF    YES, FLAG IT IN RECORD                       
         CLI   SYSMST,C'R'         SRDS OWNERSHIP SUBSCRIBER?                   
         BNE   *+8                 NO                                           
         OI    CTSYSIND,CTSYSOWN   YES, FLAG IT IN RECORD                       
*&&                                                                             
DATAV12K DS    0H                                                               
*&&UK                                                                           
         CLI   CTSYSNUM,MEDSEQ     MEDIA SYSTEM                                 
         BNE   *+14                                                             
         MVC   MEDSE,CTSYSSE       SAVE SYSTEM SE NUMBER                        
         B     DATAV12D                                                         
         CLI   CTSYSNUM,ACCSEQ     ACCOUNTING SYSTEM                            
         BNE   DATAV12D                                                         
         MVC   ACCSE,CTSYSSE       SAVE SYSTEM SE NUMBER                        
*&&                                                                             
DATAV12D EQU   *                                                                
         GOTO1 ADISSYS,CTSYSNUM                                                 
         ICM   RF,15,APPARM        CHECK IF BINARY ID REQUIRED                  
         BNZ   *+6                 FROM SYSTEM LIST INDICATOR                   
         DC    H'00'                                                            
         USING SYSLSTD,RF                                                       
         CLI   SYSLIND1,X'40'                                                   
         BNE   DATAV12B                                                         
         DROP  RF                                                               
*                                                                               
         CLI   1(R4),2                                                          
         BE    DATAV12A                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAV12B CLI   1(R4),0                                                          
         BE    DATAV12C                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
DATAV12A GOTO1 VHEXIN,APPARM,22(R4),CTSYSAGB,2                                  
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*&&UK                                                                           
         CLI   CTSYSNUM,MEDSEQ     MEDIA SYSTEM                                 
         BNE   *+10                                                             
         MVC   MEDAGB,CTSYSAGB     SAVE AGENCY BINARY                           
*&&                                                                             
*&&US                                                                           
         CLI   CTSYSNUM,X'07'      IGNORE US TALENT SYSTEM                      
         BE    DATAV12C                                                         
         MVC   APWORK(1),CTSYSSE                                                
         MVC   APWORK+1(1),CTSYSAGB                                             
         GOTO1 CHKAGY                                                           
         CLI   APWORK,0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*&&                                                                             
DATAV12C GOTO1 AADDELS,CT5REC      ADD SYSTEM ELEMENT                           
*                                                                               
         ZIC   R1,FVINDX           BUMP TO NEXT FIELD IN BLOCK                  
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         LA    R4,L'BLOCKI(R4)                                                  
         B     DATAV12                                                          
*                                                                               
DATAV14  BCT   R0,DATAV8           DO FOR NUMBER OF SCAN FIELDS                 
         LA    R1,SYSACCSH                                                      
         OC    OVSYS,OVSYS         ENSURE A SYSTEM WAS SPECIFIED                
         BNZ   DATAVA                                                           
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         LA    R0,SYSACCSH                                                      
         ST    R0,FVADDR                                                        
         B     VALRECX                                                          
         EJECT                                                                  
DATAVA   EQU   *                                                                
*                                  DELETE REMAINING SYSTEM ELEMENTS             
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),(X'FF',CT5REC),0,0                   
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
*&&UK                                                                           
         GOTO1 AFVAL,SYSASAGH      READ ASSOCIATED AGENCIES LIST                
         BNE   DATAVB                                                           
         MVI   FVINDX,0            SCAN INPUT FIELD LIST                        
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCKI)                               
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     VALRECX                                                          
         LA    R9,BLOCKI                                                        
         CLI   4(R1),CTAGLMAX                                                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     VALRECX                                                          
         MVC   FLDCNT,4(R1)        SAVE # FIELDS IN SCAN                        
         USING CTAGLD,R3                                                        
         MVI   CTAGLEL,CTAGLELQ    SET UP ASSOC AGCY ELEMENT HEADER             
         LA    R3,CTAGLAID                                                      
         MVI   FVINDX,1            INITIALISE SCAN FIELD INDEX                  
*                                                                               
DATAVA1  CLC   FVINDX,FLDCNT       PROCESS EACH FIELD IN SCAN LIST              
         BH    DATAVA7                                                          
         CLI   1(R9),0             VALID SINGLE FIELD                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         CLI   0(R9),L'CTAGLAID    VALID FIELD LENGTH                           
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALRECX                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALRECX                                                          
*                                                                               
         LR    R0,R3               TEST ID NOT ALREADY INPUT                    
         LA    R3,APELEM                                                        
         LA    R3,2(R3)                                                         
DATAVA8  CR    R3,R0                                                            
         BNL   DATAVA9                                                          
         CLC   0(L'CTAGLAID,R3),12(R9)                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     VALRECX                                                          
         LA    R3,L'CTAGLAID(R3)                                                
         B     DATAVA8                                                          
DATAVA9  CLC   12(L'CTAGLAID,R9),CT5KALPH                                       
         BE    DATAVAB             ASSOCIATED WITH ITSELF                       
         L     R2,AIOAREA2         GET SYSTEM ACCESS RECORD FOR ID              
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,12(R9)                                                  
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         BE    DATAVAA                                                          
         TM    IOERR,IOEDEL                                                     
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALRECX                                                          
         MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     VALRECX                                                          
DATAVAA  MVI   ACCFLAG,X'00'       SET SYSTEM TYPE FLAGS                        
         MVI   MEDFLAG,X'00'                                                    
         LA    R4,CT5DATA                                                       
         EJECT                                                                  
*                                  VALIDATE ID                                  
DATAVA3  CLI   0(R4),0             E-O-R                                        
         BE    DATAVA6                                                          
         CLI   0(R4),CTAGLELQ      INVALID IF ASSOC LIST PRESENT                
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFISYS)                                            
         B     VALRECX                                                          
         CLI   0(R4),CTSYSELQ      SYSTEM AUTHORIZ ELEMENT                      
         BNE   DATAVA5                                                          
         USING CTSYSD,R4                                                        
         CLI   CTSYSNUM,ACCSEQ     VALID IF SAME ACCOUNT SYSTEM                 
         BNE   DATAVA4                                                          
*        CLC   CTSYSSE,ACCSE       THIS TEST IGNORED NOW                        
*        BE    *+14                                                             
*        MVC   FVMSGNO,=AL2(FVFISYS)                                            
*        B     VALRECX                                                          
         MVC   ACCFLAG,ACCSE       FLAG FOUND OK                                
         B     DATAVA5                                                          
*                                                                               
DATAVA4  CLI   CTSYSNUM,MEDSEQ     VALID IF SAME MEDIA SYSTEM                   
         BNE   DATAVA5                                                          
*        CLC   CTSYSSE,MEDSE       THIS TEST IGNORED ALSO                       
*        BE    *+14                                                             
*        MVC   FVMSGNO,=AL2(FVFISYS)                                            
*        B     VALRECX                                                          
         MVC   MEDFLAG,MEDSE       FLAG FOUND OK                                
*                                                                               
DATAVA5  ZIC   RF,1(R4)            GO TO NEXT ELEMENT IN ACCESS RECORD          
         AR    R4,RF                                                            
         B     DATAVA3                                                          
         DROP  R4                                                               
*                                                                               
DATAVA6  CLC   ACCFLAG,ACCSE       CHECK IF VALID ACCOUNT SYSTEM                
*        BE    *+14                THIS TEST IGNORED NOW                        
*        MVC   FVMSGNO,=AL2(FVFISYS)                                            
*        B     VALRECX                                                          
         CLC   MEDFLAG,MEDSE       CHECK IF VALID MEDIA SYSTEM                  
*        BE    *+14                THIS TEST IGNORED ALSO                       
*        MVC   FVMSGNO,=AL2(FVFISYS)                                            
*        B     VALRECX                                                          
DATAVAB  L     R2,AIOAREA1                                                      
         MVC   0(L'CTAGLAID,R3),12(R9)                                          
         LA    R3,L'CTAGLAID(R3)                                                
         LA    R9,L'BLOCKI(R9)                                                  
         ZIC   RF,FVINDX           DO NEXT FIELD IN SCAN LIST                   
         LA    RF,1(RF)                                                         
         STC   RF,FVINDX                                                        
         B     DATAVA1                                                          
*                                                                               
DATAVA7  ZIC   RF,FLDCNT           SET ELEMENT LENGTH                           
         SLL   RF,1                AND SAVE ASSOC AGENCY ELEMENT                
         LA    RF,2(RF)                                                         
         LA    R3,APELEM                                                        
         STC   RF,CTAGLLEN                                                      
         MVI   FVINDX,0                                                         
         GOTO1 AADDELS,CT5REC                                                   
*&&                                                                             
DATAVB   GOTO1 AFVAL,SYSSECAH      VALIDATE SECURITY AGENCY ALPHA ID            
         BNE   DATAVBX                                                          
         CLI   FVILEN,2                                                         
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         CLC   CT5KALPH,FVIFLD                                                  
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         L     R2,AIOAREA2         GET SYSTEM ACCESS RECORD FOR ID              
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         BE    DATAVB1                                                          
         TM    IOERR,IOEDEL                                                     
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALRECX                                                          
         MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     VALRECX                                                          
*                                                                               
DATAVB1  LA    R4,CT5DATA                                                       
*                                  VALIDATE ID                                  
DATAVB2  CLI   0(R4),0             E-O-R                                        
         BE    DATAVB3                                                          
         CLI   0(R4),CTSEAELQ      INVALID IF SECURITY AGENCY PRESENT           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         ZIC   RF,1(R4)            GO TO NEXT ELEMENT IN ACCESS RECORD          
         AR    R4,RF                                                            
         B     DATAVB2                                                          
*                                                                               
DATAVB3  L     R2,AIOAREA1                                                      
*                                  ADD ELEMENT                                  
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTSEAD,R3                                                        
         MVI   CTSEAEL,CTSEAELQ                                                 
         MVI   CTSEALEN,CTSEALNQ                                                
         MVC   CTSEAAID,FVIFLD                                                  
         GOTO1 AADDELS,CT5REC                                                   
*                                                                               
DATAVBX  B     DATAVC                                                           
*                                                                               
DATAVC   EQU   *                   VALIDATE AGENCY ACCESS DETAILS               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTAADD,R3                                                        
         MVI   CTAADEL,CTAADELQ    INITIALISE ELEMENT                           
         MVI   CTAADLEN,CTAADLNQ                                                
*                                                                               
DATAVC0  OI    SYSPIDRH+6,X'80'    VALIDATE PERSONAL-ID REQUIRED FLAG           
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,SYSPIDRH                                                   
         BE    DATAVC0A                                                         
         CLI   APACTN,ACTADD       IF ADDING THEN FLAG VALUE REQUIRED           
         BNE   DATAVC1                                                          
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VALRECX                                                          
DATAVC0A CLI   FVIFLD,C'N'                                                      
         BE    DATAVC1                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   DATAVC0E                                                         
         OI    CTAADFLG,CTAADPRQ                                                
         B     DATAVC1                                                          
DATAVC0E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
DATAVC1  GOTO1 AFVAL,SYSPTOUH      VALIDATE PASSWORD TIMEOUT                    
         BE    DATAVC1A                                                         
         CLI   SYSPIDR,C'Y'                                                     
         BNE   DATAVC2                                                          
         B     DATAVC1E                                                         
DATAVC1A GOTO1 =V(NUMVAL),APPARM,SYSPTOU,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    DATAVC1E                                                         
         L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    DATAVC1E                                                         
         C     R1,=F'255'                                                       
         BH    DATAVC1E                                                         
         STCM  R1,1,CTAADPTO                                                    
         B     DATAVC2                                                          
DATAVC1E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
DATAVC2  GOTO1 AFVAL,SYSPTOWH      VALIDATE PASSWORD TIMEOUT WARNING            
         BE    DATAVC2A                                                         
         CLI   SYSPIDR,C'Y'                                                     
         BNE   DATAVC3                                                          
         B     DATAVC2E                                                         
DATAVC2A GOTO1 =V(NUMVAL),APPARM,SYSPTOW,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    DATAVC2E                                                         
         L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    DATAVC2E                                                         
         C     R1,=F'255'                                                       
         BH    DATAVC2E                                                         
         CLM   R1,1,CTAADPTO       CANT BE GREATER THAN TIMEOUT VALUE           
         BH    DATAVC2E                                                         
         STCM  R1,1,CTAADPTW                                                    
         B     DATAVC3                                                          
DATAVC2E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
DATAVC3  GOTO1 AFVAL,SYSPMINH      VALIDATE PASSWORD MINIMUM LENGTH             
         BE    DATAVC3A                                                         
         CLI   SYSPIDR,C'Y'                                                     
         BNE   DATAVC4                                                          
         B     DATAVC3E                                                         
DATAVC3A GOTO1 =V(NUMVAL),APPARM,SYSPMIN,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    DATAVC3E                                                         
         L     R1,4(R1)                                                         
         C     R1,=F'3'                                                         
         BL    DATAVC3E                                                         
         C     R1,=F'10'                                                        
         BH    DATAVC3E                                                         
         STCM  R1,1,CTAADPML                                                    
         B     DATAVC4                                                          
DATAVC3E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
DATAVC4  GOTO1 AFVAL,SYSPRUSH      VALIDATE PASSWORD REUSE NUMBER               
         BE    DATAVC4A                                                         
         CLI   SYSPIDR,C'Y'                                                     
         BNE   DATAVC4X                                                         
         B     DATAVC4E                                                         
DATAVC4A GOTO1 =V(NUMVAL),APPARM,SYSPRUS,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    DATAVC4E                                                         
         L     R1,4(R1)                                                         
         CHI   R1,1                MUST BE AT LEAST 1 FOR PPS                   
         BL    DATAVC4E                                                         
         CHI   R1,30               MAXIMUM REUSE SET TO 30                      
         BH    DATAVC4E                                                         
         STCM  R1,1,CTAADPRU                                                    
         B     DATAVC4X                                                         
DATAVC4E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVC4X EQU   *                                                                
*                                                                               
DATAUC4  GOTO1 AFVAL,SYSPMXEH      VALIDATE PASSWORD MAX ERRORS NUMBER          
         BE    DATAUC4A                                                         
         CLI   SYSPIDR,C'Y'                                                     
         BNE   DATAUC4X                                                         
         MVI   CTAADPME,6          DEFAULT IS 6 IF NOT INPUT                    
         B     DATAUC4X                                                         
DATAUC4A GOTO1 =V(NUMVAL),APPARM,SYSPMXE,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    DATAUC4E                                                         
         L     R1,4(R1)                                                         
         CHI   R1,1                MUST BE AT LEAST 1 FOR PPS                   
         BL    DATAUC4E                                                         
         CHI   R1,15               MAXIMUM ERRORS SET TO 15                     
         BH    DATAUC4E                                                         
         STCM  R1,1,CTAADPME                                                    
         B     DATAUC4X                                                         
DATAUC4E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAUC4X EQU   *                                                                
*                                                                               
DATAVC5  XC    SYSPRNA,SYSPRNA     VALIDATE PASSWORD RULE NUMBER                
         OI    SYSPRNAH+6,X'80'                                                 
         GOTO1 AFVAL,SYSPRNUH                                                   
         BNE   DATAVC5X            IF NOT INPUT DEFAULTS TO ZERO                
         CLI   SYSPIDR,C'Y'        RULE ONLY VALID FOR PPS AGENCIES             
         BNE   DATAVC5E                                                         
DATAVC5A GOTO1 =V(NUMVAL),APPARM,SYSPRNU,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    DATAVC5E                                                         
         L     R0,4(R1)                                                         
         CHI   R0,255              MAX RULE NUMBER                              
         BH    DATAVC5E                                                         
         GOTO1 =V(PWDVAL),APPARM,0,((R0),APWORK),(1,0),RR=APRELO                
         CLI   8(R1),0                                                          
         BNE   DATAVC5E            INVALID RULE NUMBER                          
         STCM  R0,1,CTAADPVR                                                    
         MVC   SYSPRNA,APWORK                                                   
         B     DATAVC5X                                                         
DATAVC5E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVC5X EQU   *                                                                
*                                                                               
DATAWC1  GOTO1 AFVAL,SYSPEXPH      VALIDATE PERSON EXPIRY DAYS                  
         BNE   DATAWC1X                                                         
DATAWC1A GOTO1 =V(NUMVAL),APPARM,SYSPEXP,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    DATAWC1E                                                         
         L     R1,4(R1)                                                         
         CHI   R1,1                                                             
         BL    DATAWC1E                                                         
         CHI   R1,255                                                           
         BH    DATAWC1E                                                         
         STCM  R1,1,CTAAEXPD                                                    
         B     DATAWC1X                                                         
DATAWC1E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAWC1X EQU   *                                                                
*                                                                               
DATAWC2  GOTO1 AFVAL,SYSPEXGH      VALIDATE PERSON EXPIRY GROUP                 
         BNE   DATAWC2X                                                         
         OI    SYSPEXGH+6,X'80'                                                 
         MVC   APHALF,CT5KALPH                                                  
         L     R4,AIOAREA2         READ ACCESS GROUP RECORD                     
         USING SAAGREC,R4                                                       
         XC    SAAGKEY,SAAGKEY                                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,APHALF                                                   
         MVC   SAAGAGR,FVIFLD                                                   
         MVC   IOKEY(L'SAAGKEY),SAAGKEY                                         
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNE   DATAWC2E                                                         
         MVC   CTAAEXPG,FVIFLD                                                  
         B     DATAWC2X                                                         
DATAWC2E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAWC2X EQU   *                                                                
*                                                                               
DATAVC6  GOTO1 AFVAL,SYSCONH       VALIDATE CONNECT ONCE FLAG                   
         BNE   DATAVC7                                                          
         OI    SYSCONH+6,X'80'                                                  
         CLI   FVIFLD,C'N'                                                      
         BE    DATAVC7                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   DATAVC6E                                                         
         OI    CTAADFLG,CTAADCON                                                
         B     DATAVC7                                                          
DATAVC6E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
DATAVC7  DS    0H                                                               
*&&US                                                                           
         GOTO1 AFVAL,SYSOFFH       VALIDATE 2 CHAR MEDIA OFFICE FLAG            
         BNE   DATAVC8                                                          
         OI    SYSOFFH+6,X'80'                                                  
         CLI   FVIFLD,C'N'                                                      
         BE    DATAVC8                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   DATAVC7E                                                         
         OI    CTAADFLG,CTAAD2OF                                                
         B     DATAVC8                                                          
DATAVC7E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
DATAVC8  GOTO1 AFVAL,SYSDABH       VALIDATE DATA BUILD                          
         BNE   DATAVC9                                                          
         OI    SYSDABH+6,X'80'                                                  
         CLI   FVIFLD,C'N'                                                      
         BE    DATAVC9                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   DATAVC8E                                                         
         OI    CTAADFLG,CTAADDAB                                                
         B     DATAVC9                                                          
*                                                                               
DATAVC8E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*&&                                                                             
DATAVC9  DS    0H                                                               
*&&DO                                                                           
DATAVC9  GOTO1 AFVAL,SYSDTAH       VALIDATE DDS TEST AGENCY                     
         BNE   DATAVC10                                                         
         OI    SYSDTAH+6,X'80'                                                  
         CLI   FVIFLD,C'N'                                                      
         BE    DATAVC10                                                         
         CLI   FVIFLD,C'Y'                                                      
         BNE   DATAVC9A                                                         
         OI    CTAADFLG,CTAADDTA   AGENCY IS A DDS TEST AGENCY                  
         B     DATAVC10                                                         
*                                                                               
DATAVC9A CLI   FVIFLD,C'T'                                                      
         BNE   DATAVC9E                                                         
         OI    CTAADFLG,CTAADTRA   AGENCY IS A TRAINING AGENCY                  
         B     DATAVC10                                                         
*                                                                               
DATAVC9E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*&&                                                                             
DATAVC10 GOTO1 AFVAL,SYSTEAMH      VALIDATE SERVICE TEAM                        
         BE    DATAVC12                                                         
         MVC   FVMSGNO,=AL2(CE#MISIF)                                           
         B     VALRECX                                                          
*                                                                               
DATAVC12 SR    R1,R1               GET INPUT LENGTH                             
         IC    R1,FVXLEN                                                        
         LA    RF,TEAMLST                                                       
         USING TEAMLSTD,RF                                                      
DATAVC14 CLI   TEAMNUM,X'FF'       END OF TEAM LIST                             
         BE    DATAVC20                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSTEAM(0),TEAMNAME MATCH ON TEAM NAME                           
         BE    DATAVC16                                                         
         LA    RF,L'TEAMLST(RF)    NEXT NAME                                    
         B     DATAVC14                                                         
*                                                                               
DATAVC16 MVC   CTAACSTN,TEAMNUM    SAVE TEAM NUMBER                             
         MVC   SYSTEAM,TEAMNAME                                                 
         OI    SYSTEAMH+6,X'80'                                                 
         B     DATAVC50                                                         
DATAVC20 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         DROP  RF                                                               
                                                                                
DATAVC50 GOTO1 AADDELS,CT5REC                                                   
*                                                                               
         XC    APELEM,APELEM       ADD ELEMENT                                  
         LA    R3,APELEM                                                        
         USING CTAGCD,R3                                                        
         MVI   CTAGCEL,CTAGCELQ    X'1B' AGENCY LABEL ELEMENT                   
         MVI   CTAGCLEN,CTAGCLNQ                                                
*                                                                               
         GOTO1 AFVAL,SYSALABH      VALIDATE AGENCY LABEL                        
         BNE   DATAVD                                                           
         LHI   R0,L'SYSALAB                                                     
         LA    R1,SYSALAB                                                       
DATAVC54 CLI   0(R1),C'A'                                                       
         BL    DATAVC20                                                         
         CLI   0(R1),C'Z'                                                       
         BH    DATAVC20                                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,DATAVC54                                                      
*                                                                               
         CLC   PRELAB,FVIFLD       IF NEW LABEL THEN                            
         BE    DATAVC60                                                         
         BRAS  RE,CHKLABEL         MUST CHECK IF AVAILABLE                      
         BE    DATAVC60                                                         
         MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     VALRECX                                                          
*                                                                               
DATAVC60 MVC   CTAGCCOD,FVIFLD                                                  
         GOTO1 AADDELS,CT5REC                                                   
*                                                                               
DATAVD   EQU   *                   VALIDATE TIME OUT ELEMENT                    
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTTOUD,R3                                                        
         MVI   CTTOUEL,CTTOUELQ    INITIALISE ELEMENT                           
         MVI   CTTOULEN,CTTOULNQ                                                
         GOTO1 AFVAL,SYSATOUH      VALIDATE ADV TIMEOUT                         
         BNE   DATAVDX                                                          
         GOTO1 =V(NUMVAL),APPARM,SYSATOU,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    DATAVDE1                                                         
         L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    DATAVDE1                                                         
         C     R1,=F'1275'                                                      
         BH    DATAVDE1                                                         
         STCM  R1,3,CTTOUADV                                                    
         GOTO1 AADDELS,CT5REC                                                   
         B     DATAVDX                                                          
DATAVDE1 MVC   FVMSGNO,=AL2(CE#ADTOU)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     VALRECX                                                          
DATAVDX  EQU   *                                                                
*&&UK                                                                           
DATAVE   EQU   *                   UK MEDIA AGENCY ALPHA - NOT USED             
*&&                                                                             
DATAVF   EQU   *                   VALIDATE WEB ACCESS ELEMENT                  
*&&UK                                                                           
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING WEBELD,R3                                                        
         MVI   WEBEL,WEBELQ        INITIALISE ELEMENT                           
         MVI   WEBLN,WEBLNQ                                                     
         GOTO1 AFVAL,SYSWIPCH                                                   
         BNE   DATAVF2                                                          
         CLI   FVIFLD,C'Y'                                                      
         BE    DATAVF2                                                          
         CLI   FVIFLD,C'N'                                                      
         BE    DATAVF1                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVF1  EQU   *                                                                
         OI    WEBFLAG,WEBFIP                                                   
DATAVF2  EQU   *                                                                
         GOTO1 AFVAL,SYSWSSCH                                                   
         BNE   DATAVF4                                                          
         CLI   FVIFLD,C'Y'                                                      
         BE    DATAVF4                                                          
         CLI   FVIFLD,C'N'                                                      
         BE    DATAVF3                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVF3  EQU   *                                                                
         OI    WEBFLAG,WEBFSS                                                   
DATAVF4  EQU   *                                                                
         GOTO1 AFVAL,SYSWSATH                                                   
         BNE   DATAVF6                                                          
         CLI   FVIFLD,WEBARASQ                                                  
         BE    DATAVF5                                                          
         CLI   FVIFLD,C'N'                                                      
         BE    DATAVF6                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVF5  EQU   *                                                                
         OI    WEBFLAG,WEBFAC                                                   
         MVI   WEBACT,WEBARASQ                                                  
DATAVF6  EQU   *                                                                
         GOTO1 AFVAL,SYSWNIDH                                                   
         BNE   DATAVF7                                                          
         MVC   WEBNID,FVIFLD                                                    
         B     DATAVF7                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVF7  EQU   *                                                                
         GOTO1 AFVAL,SYSWACDH                                                   
         BNE   DATAVF8                                                          
         MVC   WEBACOD,FVIFLD                                                   
         B     DATAVF8                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVF8  EQU   *                                                                
*                                                                               
         GOTO1 AADDELS,CT5REC                                                   
*&&                                                                             
DATAVFX  EQU   *                                                                
*                                                                               
         B     DATAVUPD                                                         
         EJECT                                                                  
*                                  SAVE UPDATED RECORD                          
DATAVUPD GOTO1 ASETACT,CT5REC                                                   
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         CLI   APACTN,ACTADD                                                    
         BNE   *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
*&&US*&& BRAS  RE,SETCAG           CHANGE OTHER ACCESS RECS FOR CFM             
*                                                                               
VALRECX  B     MODEXIT                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF ACCESS TYPE RECORD                        *         
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         MVC   SYSALPH,CT5KALPH                                                 
DISKEYX  B     MODEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ACCESS TYPE RECORD                               *         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
         TWAXC SYSCTRYH                                                         
         XC    SYSUSNM,SYSUSNM     SET NAME FIELD TRANSMIT FLAGS                
         OI    SYSUSNMH+6,X'80'                                                 
         XC    SYSGRNM,SYSGRNM                                                  
         OI    SYSGRNMH+6,X'80'                                                 
         XC    SYSHONM,SYSHONM                                                  
         OI    SYSHONMH+6,X'80'                                                 
         XC    SYSALAB,SYSALAB                                                  
         OI    SYSALABH+6,X'80'                                                 
         XC    SYSPRNA,SYSPRNA                                                  
         OI    SYSPRNAH+6,X'80'                                                 
         XC    SYSACS,SYSACS                                                    
         OI    SYSACSH+6,X'80'                                                  
         XC    SYSACSD,SYSACSD                                                  
         OI    SYSACSDH+6,X'80'                                                 
*&&US                                                                           
         XC    SYSSPUN,SYSSPUN                                                  
         OI    SYSSPUNH+6,X'80'                                                 
*&&                                                                             
*&&UK                                                                           
         XC    SYSASAG,SYSASAG                                                  
         OI    SYSASAGH+6,X'80'                                                 
*&&                                                                             
         LA    R3,CT5DATA          DISPLAY EACH ELEMENT OF RECORD               
DISP2    CLI   0(R3),0             E-O-R                                        
         BE    DISP6                                                            
         CLI   0(R3),CTDSCELQ      USER-ID                                      
         BE    DISPID                                                           
         CLI   0(R3),CTSYSELQ      SYSTEM                                       
         BE    DISPSS                                                           
         CLI   0(R3),CTAGDELQ      AGENCY GROUP DETAILS                         
         BE    DISPAD                                                           
         CLI   0(R3),CTAGCELQ      AGENCY LABEL X'1B'                           
         BE    DISAGC                                                           
*&&US                                                                           
         CLI   0(R3),CTCAGELQ      CFM AGENCY LIST ELEMENT                      
         BE    DISCAG                                                           
         CLI   0(R3),CTSPUELQ      SECURITY PRINCIPLE USER ID ELEMENT           
         BE    DISPSP                                                           
*&&                                                                             
         CLI   0(R3),CTSEAELQ      SECURITY AGENCY ID                           
         BE    DISPSA                                                           
         CLI   0(R3),CTAADELQ      AGENCY ACCESS DETAILS                        
         BE    DISPDE                                                           
         CLI   0(R3),CTTOUELQ      TIME OUT ELEMENT                             
         BE    DISPTO                                                           
         CLI   0(R3),WEBELQ        WEB ELEMENT                                  
         BE    DISPWB                                                           
         CLI   0(R3),CTACCELQ      ACCESS DATE ELEMENT                          
         BE    DISPAC                                                           
*&&UK                                                                           
         CLI   0(R3),CTAGLELQ      ASSOC AGENCY LIST                            
         BE    DISPAA                                                           
*&&                                                                             
DISP4    SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DISP2                                                            
*                                                                               
DISP6    CLI   SYSCNT,0            DISPLAY SYSTEM ELEMENTS LIST                 
         BE    DISPX                                                            
         ZIC   R0,SYSCNT                                                        
         GOTO1 VSCINKEY,APPARM,(2,SYSACCSH),(20,BLOCKI),(R0)                    
*                                                                               
DISPX    GOTO1 ADISACT,CT5REC      DISPLAY ACTIVITY DATE                        
         B     MODEXIT                                                          
*                                                                               
         USING CTDSCD,R3                                                        
DISPID   GOTO1 GETID,CTDSC         DISPLAY PRINCIPAL ID                         
         MVC   SYSUSER,USERID                                                   
         MVC   SYSUSNM,USERNM                                                   
         B     DISP4                                                            
                                                                                
         USING CTAGDD,R3                                                        
DISPAD   OC    CTAGDAG,CTAGDAG     DISPLAY AGENCY GROUP                         
         BZ    DISPAD1             IF PRESENT                                   
         GOTO1 GETID,CTAGDAG                                                    
         MVC   SYSGRUP,USERID                                                   
         MVC   SYSGRNM,USERNM                                                   
DISPAD1  OC    CTAGDHG,CTAGDHG     DISPLAY HOLDING GROUP                        
         BZ    DISPAD2             IF PRESENT                                   
         GOTO1 GETID,CTAGDHG                                                    
         MVC   SYSHOLD,USERID                                                   
         MVC   SYSHONM,USERNM                                                   
DISPAD2  MVI   SYSDDSA,C' '        DISPLAY DDS ACCESS LEVEL                     
         CLI   CTAGDDA,0                                                        
         BE    DISPAD3                                                          
         TM    CTAGDDA,CTAGDDAY    BYPASS PASSWORD SECURITY ACCESS              
         BNO   DISPAD2A                                                         
         MVI   SYSDDSA,C'Y'                                                     
         B     DISPAD3                                                          
*                                                                               
DISPAD2A TM    CTAGDDA,CTAGDDAN    NO DDS TERMINAL ACCESS                       
         BNO   DISPAD2B                                                         
         MVI   SYSDDSA,C'N'                                                     
         B     DISPAD3                                                          
*                                                                               
DISPAD2B TM    CTAGDDA,CTAGDDAP    NO DDS AGENCY PASSWORD ACCESS                
         BNO   DISPAD3                                                          
         MVI   SYSDDSA,C'P'                                                     
*                                                                               
DISPAD3  MVC   PREAGOPT,CTAGOPTS   SAVE OFF VALUES                              
         BRAS  RE,DISPAGYT         R3 IS SET TO CTAGDD                          
*                                                                               
DISPAD4  MVC   SYSTYPE,CTAGDTY     DISPLAY CLIENT TYPE                          
         CLI   CTAGDLEN,CTAGDL2Q                                                
         BL    DISP4                                                            
         L     R1,ACTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         CLC   CTRYCODE,CTAGDCTY                                                
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   SYSCTRY,CTRYNAM     DISPLAY COUNTRY NAME                         
         B     DISP4                                                            
         DROP  R1                                                               
*&&UK                                                                           
DISPRP   B     DISP4               RFP PRINCIPLE USER ID - NOT USED             
*&&                                                                             
*&&US                                                                           
         USING CTSPUD,R3                                                        
DISPSP   GOTO1 GETID,CTSPUNUM      DISPLAY SECURITY PRINCIPLE USER ID           
         MVC   SYSSPU,USERID                                                    
         MVC   SYSSPUN,USERNM                                                   
         B     DISP4                                                            
*&&                                                                             
         USING CTSYSD,R3                                                        
DISPSS   GOTO1 ADISSE,CTSYSSE      FORMAT SYSTEM BLOCK ENTRY                    
*NOP*    OC    APPARM(4),APPARM    RETURN STRING IN APWORK                      
*NOP*    BNZ   *+6                 ELSE APPARM=0 NOT FOUND                      
*NOP*    DC    H'0'                PLOUGH ON                                    
         ZIC   RE,SYSCNT                                                        
         LA    R0,1(RE)                                                         
         STC   R0,SYSCNT                                                        
         LA    RF,20                                                            
         MR    RE,RE                                                            
         LA    R6,BLOCKI(RF)                                                    
         MVI   0(R6),C' '                                                       
         MVC   1(19,R6),0(R6)                                                   
         MVC   0(L'SENAME,R6),APWORK                                            
         LA    R6,L'SENAME-1(R6)                                                
         CLI   0(R6),C' '                                                       
         BNE   *+8                                                              
         BCT   R6,*-8                                                           
         GOTO1 ADISSYS,CTSYSNUM                                                 
         ICM   R1,15,APPARM        SAVE A(SELIST ENTRY)                         
         BNZ   *+6                                                              
         DC    H'00'                                                            
         USING SYSLSTD,R1                                                       
         CLI   SYSLIND1,X'40'                                                   
         BNE   DISPSS1             TEST IF BINARY AGENCY KEY                    
         MVI   1(R6),C'='          IF SO DISPLAY                                
         GOTO1 VHEXOUT,APPARM,CTSYSAGB,2(R6),1,=C'TOG'                          
DISPSS1  EQU   *                                                                
*&&US                                                                           
         CLI   CTSYSNUM,X'02'      SPOT SYSTEM                                  
         BNE   DISP4                                                            
         OI    SYSCABLH+6,X'80'    SEE IF AGENCY USES NCA                       
         MVI   SYSCABL,C'N'                                                     
         TM    CTSYSIND,CTSYSNCA                                                
         BZ    *+8                                                              
         MVI   SYSCABL,C'Y'                                                     
         OI    SYSMSTH+6,X'80'     MSTREET/MEDIA FRAMEWORKS                     
         MVI   SYSMST,C'N'                                                      
         TM    CTSYSIND,CTSYSRAD   MSTREET?                                     
         BZ    *+8                 NO                                           
         MVI   SYSMST,C'S'         YES - DISPLAY "S"                            
         TM    CTSYSIND,CTSYSMF    MEDIA FRAMEWORKS?                            
         BZ    *+8                 NO                                           
         MVI   SYSMST,C'F'         YES - DISPLAY "F"                            
         TM    CTSYSIND,CTSYSOWN   SRDS OWNERSHIP?                              
         BZ    *+8                 NO                                           
         MVI   SYSMST,C'R'         YES - DISPLAY "R"                            
*&&                                                                             
         B     DISP4                                                            
         DROP  R1                                                               
                                                                                
         USING CTSEAD,R3                                                        
DISPSA   MVC   SYSSECA,CTSEAAID    SECURITY AGENCY ID                           
         B     DISP4                                                            
         DROP  R3                                                               
                                                                                
         USING CTAADD,R3                                                        
DISPDE   TM    CTAADFLG,CTAADPRQ   AGENCY ACCESS DETAILS ELEMENT                
         BZ    DIDE010                                                          
         MVI   SYSPIDR,C'Y'        PPS AGENCY                                   
         B     *+8                                                              
DIDE010  MVI   SYSPIDR,C'N'                                                     
         EDIT  CTAADPTO,(3,SYSPTOU),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         EDIT  CTAADPTW,(3,SYSPTOW),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         EDIT  CTAADPML,(2,SYSPMIN),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         EDIT  CTAADPRU,(3,SYSPRUS),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         CLI   CTAADPME,0                                                       
         BNE   DIDE012                                                          
         CLI   SYSPIDR,C'Y'        DEFAULT IS 6 IF NEVER SET                    
         BNE   DIDE012                                                          
         MVI   CTAADPME,6                                                       
DIDE012  EDIT  CTAADPME,(3,SYSPMXE),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         EDIT  CTAADPVR,(2,SYSPRNU),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
*                                                                               
         CLI   SYSPIDR,C'Y'        PASSWORD RULE IF PPS AGENCY                  
         BNE   DIDE015                                                          
         SR    R0,R0                                                            
         ICM   R0,1,CTAADPVR                                                    
         GOTO1 =V(PWDVAL),APPARM,0,((R0),APWORK),(1,0),RR=APRELO                
         CLI   8(R1),0                                                          
         BNE   *+10                                                             
         MVC   SYSPRNA,APWORK                                                   
*                                                                               
DIDE015  TM    CTAADFLG,CTAADCON                                                
         BZ    *+12                                                             
         MVI   SYSCON,C'Y'                                                      
         B     *+8                                                              
         MVI   SYSCON,C'N'                                                      
*                                                                               
DIDE020  CLI   CTAADLEN,CTAADLNQ   TEST IF NEW FULL SIZED ELEMENT               
         BNE   DIDE025                                                          
         EDIT  CTAAEXPD,(3,SYSPEXP),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         MVC   SYSPEXG,CTAAEXPG                                                 
DIDE025  EQU   *                                                                
*&&US                                                                           
         TM    CTAADFLG,CTAAD2OF                                                
         BZ    DIDE030                                                          
         MVI   SYSOFF,C'Y'                                                      
         B     *+8                                                              
DIDE030  MVI   SYSOFF,C'N'                                                      
*                                                                               
         TM    CTAADFLG,CTAADDAB                                                
         BZ    DIDE040                                                          
         MVI   SYSDAB,C'Y'                                                      
         B     *+8                                                              
DIDE040  MVI   SYSDAB,C'N'                                                      
*&&                                                                             
* CAN REMOVE ONCE CONVERTED ALL ELEMENT TO USE X'B4'                            
*&&DO                                                                           
         NI    SYSDTAH+1,X'FF'-X'08'                                            
         LA    RE,WORK                                                          
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         TM    CTAADFLG,X'01'       DDS TEST AGENCY (CTAADDTA)                  
         BZ    DIDE041                                                          
         MVC   0(3,RE),=C'TST'                                                  
         AHI   RE,3                                                             
*                                                                               
DIDE041  TM    CTAADFLG,X'02'       TRAINING AGENCY (CTAADTRA)                  
         BZ    DIDE042                                                          
         MVC   0(3,RE),=C'TRN'                                                  
         AHI   RE,3                                                             
*                                                                               
DIDE042  TM    PREAGOPT,CTAGSAP    WAS SAP ON?                                  
         BZ    DIDE045             NO                                           
         LA    RF,WORK                                                          
         CR    RE,RF                                                            
         BE    DIDE043                                                          
         MVC   0(1,RE),SCCOMMA                                                  
         AHI   RE,1                                                             
*                                                                               
DIDE043  MVC   0(3,RE),=C'SAP'                                                  
*                                                                               
DIDE045  CLI   WORK,C' '           ANY DATA FILLED IN?                          
         BE    DIDE050             NO                                           
         MVC   SYSDTA,WORK                                                      
         OI    SYSDTAH+1,X'08'      HIGH INTENSITY                              
*&&                                                                             
DIDE050  CLI   CTAACSTN,0           CLIENT SERVICE TEAM NAME                    
         BE    DIDEX                                                            
         LA    RF,TEAMLST                                                       
         USING TEAMLSTD,RF                                                      
DIDE054  CLI   TEAMNUM,X'FF'        END OF TEAM LIST                            
         BE    DIDE055                                                          
         CLC   TEAMNUM,CTAACSTN                                                 
         BE    *+12                                                             
         LA    RF,L'TEAMLST(RF)                                                 
         B     DIDE054                                                          
         MVC   SYSTEAM,TEAMNAME                                                 
         B     DIDEX                                                            
DIDE055  MVC   SYSTEAM(2),=C'??'                                                
         B     DIDEX                                                            
         DROP  RF                                                               
*                                                                               
DIDEX    B     DISP4                                                            
         DROP  R3                                                               
         USING CTTOUD,R3                                                        
*                                  TIME OUT ELEMENT                             
DISPTO   EQU   *                                                                
         EDIT  CTTOUADV,(4,SYSATOU),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
DITOX    B     DISP4                                                            
         DROP  R3                                                               
*&&UK                                                                           
         USING CTAGLD,R3                                                        
DISPAA   ZIC   R0,CTAGLLEN         DISPLAY ASSOCIATED AGENCY LIST               
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         SRL   R0,1                                                             
         LA    RF,CTAGLAID         LOOP FOR EACH 2 CHARACTER ID                 
         LA    R1,SYSASAG                                                       
DISPAA1  MVC   0(L'CTAGLAID,R1),0(RF)                                           
         LA    RF,L'CTAGLAID(RF)                                                
         LA    R1,L'CTAGLAID(R1)                                                
         BCT   R0,*+8                                                           
         B     DISP4                                                            
         MVI   0(R1),C','          SEPARATED BY COMMAS                          
         LA    R1,1(R1)                                                         
         B     DISPAA1                                                          
         DROP  R3                                                               
*&&                                                                             
*&&UK                                                                           
DISPMA   B     DISP4               UK MEDIA AGENCY - NOT USED                   
*&&                                                                             
         USING WEBELD,R3                                                        
DISPWB   EQU   *                   WEB ACCESS ELEMENT                           
         TM    WEBFLAG,WEBFIP                                                   
         BZ    *+12                                                             
         MVI   SYSWIPC,C'N'                                                     
         B     *+8                                                              
         MVI   SYSWIPC,C'Y'                                                     
         TM    WEBFLAG,WEBFSS                                                   
         BZ    *+12                                                             
         MVI   SYSWSSC,C'N'                                                     
         B     *+8                                                              
         MVI   SYSWSSC,C'Y'                                                     
         TM    WEBFLAG,WEBFAC                                                   
         BZ    *+14                                                             
         MVC   SYSWSAT,WEBACT                                                   
         B     *+8                                                              
         MVI   SYSWSAT,C'N'                                                     
         MVC   SYSWNID(L'WEBNID),WEBNID                                         
         MVC   SYSWACD(L'WEBACOD),WEBACOD                                       
         B     DISP4                                                            
*                                                                               
         USING CTAGCD,R3                                                        
DISAGC   MVC   SYSALAB,CTAGCCOD                                                 
         B     DISP4                                                            
*                                                                               
         USING CTACCD,R3                                                        
DISPAC   MVC   SYSACS,=CL10'RESTRICTED'                                         
         MVC   SYSACSD,=CL10'ACCESS'                                            
         GOTO1 VDATCON,APPARM,(5,0),(3,APWORK) TA..TA.. TODAY JUNIOR            
         CLI   CTACCROD,0          ANY READ-ONLY DATE                           
         BE    DISPAC2             NO                                           
         MVC   SYSACS,=CL10'READ-ONLY '                                         
         GOTO1 VDATCON,APPARM,(3,CTACCROD),(21,SYSACSD)                         
DISPAC2  CLI   CTACCNAD,0          ANY NO ACCESS DATE                           
         BE    DISP4                                                            
         MVC   SYSACS,=CL10'NO ACCESS '                                         
         GOTO1 VDATCON,APPARM,(3,CTACCNAD),(21,SYSACSD)                         
         B     DISP4                                                            
*&&US                                                                           
DISCAG   BRAS  RE,DCAG             DISPLAY CFM AGENCY LIST ELEMENT              
         B     DISP4                                                            
*&&                                                                             
         DROP  R3                                                               
                                                                                
*********************************************************************           
* DISPLAY "DDS AGY TYPE"                                                        
*********************************************************************           
         USING AGYTYPD,RE                                                       
         USING CTAGDD,R3                                                        
DISPAGYT DS    0H                  WHAT TYPE OF COMPANY FILE                    
         STM   RE,RF,SVRERF        SAVE RF SO WE CAN SEE WHENCE WE CAME         
         NI    SYSDTAH+1,X'FF'-X'08'                                            
         LARL  RE,AGYTYP                                                        
         LA    R1,WORK                                                          
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK   MOVE SPACES IN                           
DISAGY10 CLI   0(RE),X'FF'         END OF TABLE, THEN DONE                      
         BE    DISAGY90            YES                                          
         LLC   RF,AGYOPT           EXTACT OPTION BIT TO TEST                    
         EX    RF,AGYTYTM          TEST BIT                                     
         BZ    DISAGY30            NOT A HIT SO NEXT ENTRY                      
         LA    RF,WORK             BIT MATCHED                                  
         CR    R1,RF               SEE IF FIRST MATCH OF THE MOMENT             
         BE    DISAGY18            IF THE SAME, THEN 1ST ENTRY                  
         MVC   0(1,R1),SCCOMMA     MUST BE 2ND ENTRY SO PUT A COMMA             
         AHI   R1,1                BUMP PAST COMMA                              
*                                                                               
DISAGY18 LLC   RF,AGYMAXL          GET FULL LENGTH                              
         BCTR  RF,0                LESS ONE FOR EXCUTE                          
         EX    RF,AGYTYMVC         MVC INTO WORK                                
         LA    R1,1(R1,RF)         BUMP UP TO END OF DATA MOVED                 
*                                                                               
DISAGY30 LA    RE,AGYTYLNQ(,RE)    KEEP GOING TILL END OF TABLE                 
         B     DISAGY10                                                         
* ----------------------------------------                                      
* EXCUTE STATEMENTS                                                             
* ----------------------------------------                                      
AGYTYTM  TM    CTAGOPTS,0                                                       
AGYTYMVC MVC   0(0,R1),AGYTEXT                                                  
*                                                                               
DISAGY90 LA    RF,WORK             DID WE BUILD ANYTHING?                       
         CR    R1,RF               IF THE SAME THEN NO                          
         BNE   DISAGY92            YES WE DID                                   
         LLC   RF,AGYMAXL          MAX LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,AGYTYMVC                                                      
         DROP  RE                                                               
*                                                                               
DISAGY92 MVC   SYSDTA,WORK         MOVE IN WHAT WE BUILT                        
         OI    SYSDTAH+1,X'08'     DISPLAY FIELD (HIGH INTENSITY)               
         L     RE,SVRE             JUST RESTORE RF                              
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ROUTINE TO DELETE AN ACCESS TYPE RECORD                             *         
***********************************************************************         
DELREC   L     R2,AIOAREA1                                                      
*                                                                               
*&&US*&& GOTO1 AFVAL,SYSCAGH                                                    
*&&US*&& BE    DRERX                                                            
*                                                                               
         GOTO1 ASETACT,CT5REC                                                   
         OI    CT5STAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     MODEXIT                                                          
*                                                                               
DRERX    MVC   FVMSGNO,=AL2(FVFXDEL)     CAN'T DELETE IF CFM AGY LIST           
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED ACCESS TYPE RECORD                     *         
***********************************************************************         
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CT5REC                                                   
         NI    CT5STAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     MODEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS FOR ACCESS RECORDS            *         
***********************************************************************         
VALSEL   LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
         LA    R2,APRECKEY         SET UP FIRST LIST RECORD KEY                 
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         GOTO1 AFVAL,LSTALPHH                                                   
         BNE   *+10                                                             
         MVC   CT5KALPH,FVIFLD                                                  
         XC    SELKEY,SELKEY       CLEAR SELECT DATA FIELDS                     
*                                                                               
         GOTO1 AFVAL,LSTCTRYH      COUNTRY FILTER                               
         BNE   VALSEL2                                                          
         GOTO1 GETCT                                                            
         BNE   VALSELX                                                          
         MVC   SELCTY(L'CTRYCODE),APWORK                                        
         CLC   APWORK+L'CTRYCODE(L'CTRYNAM),LSTCTRY                             
         BE    *+14                                                             
         MVC   LSTCTRY(L'CTRYNAM),APWORK+L'CTRYCODE                             
         OI    LSTCTRYH+6,X'80'                                                 
*                                                                               
VALSEL2  GOTO1 AFVAL,LSTGRUPH      GROUP ID                                     
         BNE   VALSEL3                                                          
         GOTO1 VALID                                                            
         BNE   VALSELX                                                          
         MVC   SELGID,USERNO                                                    
*                                                                               
VALSEL3  GOTO1 AFVAL,LSTHOLDH      HOLDING COMPANY                              
         BNE   VALSEL4                                                          
         GOTO1 VALID                                                            
         BNE   VALSELX                                                          
         MVC   SELHID,USERNO                                                    
*                                                                               
VALSEL4  GOTO1 AFVAL,LSTTYPEH      CLIENT TYPE                                  
         BNE   VALSEL6                                                          
         MVC   SELTYP,FVIFLD                                                    
*                                                                               
VALSEL6  GOTO1 AFVAL,LSTACCSH      SYSTEM NUMBER E.G. MED1 CODE                 
         BNE   VALSEL8                                                          
         CLI   FVILEN,L'SENAME                                                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
         MVC   APWORK(L'SENAME),FVIFLD                                          
         GOTO1 GETSE                                                            
         BNE   VALSELX                                                          
         MVC   SELSYS,APWORK                                                    
*                                                                               
VALSEL8  GOTO1 AFVAL,LSTSECAH      CLIENT TYPE                                  
         BNE   VALSEL9                                                          
         MVC   SELSECA,FVIFLD                                                   
         CLC   FVIFLD(4),=C'ONLY'                                               
         BNE   *+8                                                              
         MVI   SELSECA,SELSAOQ     SECURITY AGENCIES ONLY                       
*                                                                               
VALSEL9  GOTO1 AFVAL,LSTTEAMH      VALIDATE SERVICE TEAM                        
         BNE   VALSEL9X                                                         
         SR    R1,R1               GET INPUT LENGTH-1                           
         IC    R1,FVXLEN                                                        
         LA    RF,TEAMLST                                                       
         USING TEAMLSTD,RF                                                      
VALSEL9A CLI   TEAMNUM,X'FF'       END OF TEAM LIST                             
         BE    VALSEL9B                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LSTTEAM(0),TEAMNAME MATCH ON TEAM NAME                           
         BE    *+12                                                             
         LA    RF,L'TEAMLST(RF)    NEXT NAME                                    
         B     VALSEL9A                                                         
         MVC   SELCSTN,TEAMNUM     SAVE CLIENT SERVICE TEAM NUMBER              
         B     VALSEL9X                                                         
VALSEL9B MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
VALSEL9X EQU   *                                                                
         DROP  RF                                                               
*                                                                               
VS010    GOTO1 AFVAL,LSTLABH       AGENCY LABEL                                 
         BNE   VALSELY                                                          
         LHI   R0,L'CTAGCCOD                                                    
         LA    R1,LSTLAB                                                        
VS012    CLI   0(R1),C' '                                                       
         BNH   VS013                                                            
         CLI   0(R1),C'A'                                                       
         BL    VS016                                                            
         CLI   0(R1),C'Z'                                                       
         BH    VS016                                                            
VS013    LA    R1,1(,R1)                                                        
         BCT   R0,VS012                                                         
         B     VALSELY                                                          
VS016    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  B     MODEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST-SELECT RECORD FOR ACCESS RECORDS                      *         
***********************************************************************         
GETSEL   LA    R2,IOKEY                                                         
         MVC   CT5KEY,APRECKEY                                                  
         TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GETSEL2                                                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GETSEL4                                                          
         B     GETSELN                                                          
GETSEL2  TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GETSEL4                                                          
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GETSEL4  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         L     R2,AIOAREA1                                                      
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   GETSELN                                                          
*                                                                               
         MVI   APFLAG,0            SET SYSTEM # SELECT SEARCH FLAG              
         NI    ACDFLAG,X'FF'-ACDPPSQ-ACDLABQ-ACDAADQ                            
         MVI   SECAFLAG,C'N'       SET SECURITY AGENCY ELEMENT FLAG             
         LA    R3,CT5DATA          GET ELEMENTS FOR SELECT FILTER               
GETSELA  CLI   0(R3),0             E-O-R                                        
         BE    GETSELF                                                          
         CLI   0(R3),CTAGDELQ      AGENCY GROUP DETAILS                         
         BE    GETSELB                                                          
         CLI   0(R3),CTSYSELQ      SYSTEM                                       
         BE    GETSELD                                                          
         CLI   0(R3),CTSEAELQ      SECURITY AGENCY                              
         BE    GSELSA                                                           
         CLI   0(R3),CTAADELQ      AGENCY ACCESS DETAILS                        
         BE    GSELAA                                                           
         CLI   0(R3),CTAGCELQ      AGENCY LABEL                                 
         BE    GSELAL                                                           
*                                                                               
GETSELE  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETSELA                                                          
*                                                                               
GETSELF  OC    SELSECA,SELSECA     TEST SECURITY AGENCY SELECT                  
         BZ    GETSELF1                                                         
         CLI   SELSECA,SELSAOQ     SECURITY AGENCIES ONLY                       
         BE    GETSELF1                                                         
         CLI   SECAFLAG,C'N'                                                    
         BE    GETSEL4                                                          
*                                                                               
GETSELF1 OC    SELSYS,SELSYS       TEST SYSTEM # SELECT                         
         BZ    GETSELK                                                          
         CLI   APFLAG,0            TEST MATCHING SYSTEM # FOUND                 
         BE    GETSEL4                                                          
         B     GETSELK                                                          
*                                                                               
         USING CTAGDD,R3                                                        
GETSELB  OC    SELGID,SELGID       FILTER ON AGENCY GROUP ID                    
         BZ    *+14                UNLESS NO SELECTS                            
         CLC   SELGID,CTAGDAG                                                   
         BNE   GETSEL4                                                          
         OC    SELHID,SELHID       HOLDING COMPANY ID                           
         BZ    *+14                                                             
         CLC   SELHID,CTAGDHG                                                   
         BNE   GETSEL4                                                          
         OC    SELTYP,SELTYP       CLIENT TYPE                                  
         BZ    *+14                DO NEXT ELEMENT                              
         CLC   SELTYP,CTAGDTY                                                   
         BNE   GETSEL4                                                          
         OC    SELCTY,SELCTY       COUNTRY CODE                                 
         BZ    GETSELE                                                          
         CLI   CTAGDLEN,CTAGDL2Q   SHORT ELEMENT                                
         BL    GETSEL4                                                          
         CLC   SELCTY,CTAGDCTY                                                  
         BNE   GETSEL4                                                          
         B     GETSELE                                                          
*                                                                               
         USING CTSEAD,R3                                                        
GSELSA   MVI   SECAFLAG,C'Y'                                                    
         OC    SELSECA,SELSECA     FILTER ON SECURITY AGENCY                    
         BZ    GETSELE                                                          
         CLI   SELSECA,SELSAOQ     SECURITY AGENCIES ONLY                       
         BNE   *+16                                                             
         CLI   CTSEAAID,C' '       SECURITY AGENCY ON RECORD?                   
         BH    GETSEL4             YES,THEN THIS AGY NOT A SEC AGY              
         B     GETSELE                                                          
         CLC   SELSECA,CTSEAAID                                                 
         BNE   GETSEL4                                                          
         B     GETSELE                                                          
*                                                                               
         USING CTSYSD,R3                                                        
GETSELD  OC    SELSYS,SELSYS       SYSTEM # SELECT                              
         BZ    GETSELE                                                          
         CLC   SELSYS,CTSYSSE                                                   
         BNE   GETSELE                                                          
         MVI   APFLAG,1            FLAG MATCHING SYSTEM # FOUND                 
         B     GETSELE                                                          
         DROP  R3                                                               
*                                                                               
         USING CTAGCD,R3                                                        
GSELAL   OI    ACDFLAG,ACDLABQ     AGENCY LABEL ELEMENT FOUND                   
         CLI   LSTLAB,C' '         ANY AGENCY LABEL FILTER                      
         BNH   GETSELE                                                          
         SR    R1,R1                                                            
         IC    R1,LSTLABH+FHILD                                                 
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LSTLAB(0),CTAGCCOD                                               
         BNE   GETSEL4                                                          
         B     GETSELE                                                          
*                                                                               
         USING CTAADD,R3                                                        
GSELAA   OI    ACDFLAG,ACDAADQ     AGENCY ACCESS DETAIL ELEMENT FOUND           
         TM    CTAADFLG,CTAADPRQ                                                
         BZ    *+8                                                              
         OI    ACDFLAG,ACDPPSQ     SET PPS AGENCY FLAG                          
         CLI   SELCSTN,0                                                        
         BE    GETSELE                                                          
         CLC   SELCSTN,CTAACSTN    FILTER ON SERVICE TEAM                       
         BNE   GETSEL4                                                          
         B     GETSELE                                                          
*                                                                               
* FINISHED CHECKING ELEMENTS                                                    
*                                                                               
GETSELK  CLI   OPTPPS,C'Y'         WANT PPS AGENCIES ONLY?                      
         BNE   *+12                                                             
         TM    ACDFLAG,ACDPPSQ     AGENCY ON PPS                                
         BNO   GETSEL4                                                          
         CLI   OPTPPS,C'N'         WANT NON-PPS AGENCIES ONLY?                  
         BNE   *+12                                                             
         TM    ACDFLAG,ACDPPSQ     AGENCY ON PPS                                
         BO    GETSEL4                                                          
*                                                                               
         CLI   LSTLAB,C' '         FILTERING ON AGENCY LABEL                    
         BNH   *+12                                                             
         TM    ACDFLAG,ACDLABQ                                                  
         BZ    GETSEL4             NO AGENCY LABEL ELEMENT FOUND                
*                                                                               
         CLI   LSTTEAM,0           FILTERING ON CLIENT SERVICE TEAM             
         BE    *+12                                                             
         TM    ACDFLAG,ACDAADQ                                                  
         BZ    GETSEL4             NO AGENCY ACCESS DETAIL ELMENT FOUND         
*                                                                               
GETSELY  MVC   APRECKEY(L'CT5KEY),CT5KEY RECORD SELECTED                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     MODEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE FOR ACCESS RECORDS                         *         
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         MVC   LISTALPH,CT5KALPH   DISPLAY AGENCY ALPHA ID                      
         MVI   LISTBUFF,C' '                                                    
         MVC   LISTBUFF+1(L'LISTBUFF-1),LISTBUFF                                
         LA    R6,LISTBUFF         SET POINTER TO SYS# FIELD BUFFER             
         LA    R3,CT5DATA          READ EACH ELEMENT OF RECORD                  
DSEL2    CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),CTDSCELQ      USER-ID                                      
         BE    DSELID                                                           
         CLI   0(R3),CTSYSELQ      SYSTEM                                       
         BE    DSELSS                                                           
         CLI   0(R3),CTAGDELQ      AGENCY GROUP DETAILS                         
         BE    DSELAD                                                           
         CLI   0(R3),CTSEAELQ      SECURITY AGENCY ID                           
         BE    DSELSA                                                           
         CLI   0(R3),CTAADELQ      AGENCY ACCESS DETAILS                        
         BE    DSELAA                                                           
         CLI   0(R3),CTAGCELQ      AGENCY LABEL                                 
         BE    DSELLAB                                                          
*                                                                               
DSEL4    SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSEL2                                                            
*                                                                               
DISSELX  BCTR  R6,0                                                             
         MVI   0(R6),C' '          REMOVE COMMA FROM DISPLAY BUFFER             
         CLI   LISTBUFF+L'LISTACCS,C' '                                         
         BE    DISSELX2                                                         
         LA    R6,LISTBUFF+(L'LISTACCS-1)                                       
DISSELX1 CLI   0(R6),C','          INDICATE BUFFER OVERFLOW                     
         BE    *+12                                                             
         MVI   0(R6),C' '                                                       
         BCT   R6,DISSELX1                                                      
         MVI   0(R6),C'>'                                                       
DISSELX2 MVC   LISTACCS,LISTBUFF   TRANSFER BUFFER TO SYSTEM FIELD              
         OI    APINDS,APILRERD     FLAG READ IO SEQUENCE BROKEN                 
         B     MODEXIT                                                          
*                                                                               
         USING CTDSCD,R3                                                        
DSELID   GOTO1 GETID,CTDSC         DISPLAY PRINCIPAL ID                         
         MVC   LISTUSER,USERID                                                  
         B     DSEL4                                                            
*                                                                               
         USING CTAGCD,R3                                                        
DSELLAB  MVC   LISTLAB,CTAGCCOD    AGENCY LABEL                                 
         B     DSEL4                                                            
*                                                                               
         USING CTAGDD,R3                                                        
DSELAD   OC    CTAGDAG,CTAGDAG     DISPLAY AGENCY GROUP                         
         BZ    DSELAD1             IF PRESENT                                   
         GOTO1 GETID,CTAGDAG                                                    
         MVC   LISTGRUP,USERID                                                  
DSELAD1  OC    CTAGDHG,CTAGDHG     DISPLAY HOLDING GROUP                        
         BZ    DSELAD3             IF PRESENT                                   
         GOTO1 GETID,CTAGDHG                                                    
         MVC   LISTHOLD,USERID                                                  
*                                                                               
DSELAD3  MVC   LISTTYPE,CTAGDTY    DISPLAY CLIENT TYPE                          
         CLI   CTAGDLEN,CTAGDL2Q                                                
         BL    DSEL4                                                            
         L     R1,ACTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         CLC   CTRYCODE,CTAGDCTY                                                
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   LISTCTRY,CTRYSHR    DISPLAY COUNTRY SHORT NAME                   
         B     DSEL4                                                            
         DROP  R1                                                               
                                                                                
         USING CTSEAD,R3                                                        
DSELSA   MVC   LISTSECA,CTSEAAID   SECURITY AGENCY ID                           
         B     DSEL4                                                            
                                                                                
         USING CTAADD,R3                                                        
DSELAA   CLI   CTAACSTN,0          CLIENT SERVICE TEAM NAME                     
         BE    DSEL4                                                            
         LA    RF,TEAMLST                                                       
         USING TEAMLSTD,RF                                                      
DSELAA1  CLI   TEAMNUM,X'FF'       END OF TEAM LIST                             
         BE    DSELAA2                                                          
         CLC   TEAMNUM,CTAACSTN                                                 
         BE    *+12                                                             
         LA    RF,L'TEAMLST(RF)                                                 
         B     DSELAA1                                                          
         MVC   LISTTEAM,TEAMNAME                                                
         B     DSEL4                                                            
DSELAA2  MVC   LISTTEAM(2),=C'??'                                               
         B     DSEL4                                                            
         DROP  RF                                                               
                                                                                
         USING CTSYSD,R3                                                        
DSELSS   GOTO1 ADISSYS,CTSYSNUM                                                 
         ICM   R9,15,APPARM        SAVE A(SELIST ENTRY)                         
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                  MAKE SYSTEM SHORT EVEN SHORTER               
         USING SYSLSTD,R9                                                       
         MVC   0(L'SYSLSHRT-2,R6),SYSLSHRT                                      
         CLC   =C'STR',SYSLSHRT                                                 
         BNE   *+8                                                              
         MVI   0(R6),C'F'          TRAFFIC                                      
         LA    R6,L'SYSLSHRT-2(R6)                                              
         GOTO1 CNTSE,CTSYSNUM                                                   
         CLI   APWORK,0                                                         
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   APWORK,1                                                         
         BE    DSELSS1                                                          
         GOTO1 ADISSE,CTSYSSE                                                   
         LA    RF,APWORK+4                                                      
         CLC   =C'CONTROL',APWORK                                               
         BE    DSELSS1                                                          
         CLC   =C'PRNT',APWORK                                                  
         BE    DSELSS0                                                          
         CLC   =C'SPOT',APWORK                                                  
         BE    DSELSS0                                                          
         LA    RF,APWORK+3                                                      
*                                                                               
DSELSS0  MVC   0(2,R6),0(RF)                                                    
         AHI   R6,1                ONE CHARACTER SYSTEM ID                      
         CLI   1(RF),C' '                                                       
         BE    DSELSS1                                                          
         AHI   R6,1                TWO CHARACTR SYSTEM ID                       
*                                                                               
DSELSS1  CLI   SYSLIND1,X'40'                                                   
         BNE   DSELSS2                                                          
         MVI   0(R6),C'='                                                       
         GOTO1 VHEXOUT,APPARM,CTSYSAGB,1(R6),1,=C'TOG'                          
         LA    R6,3(R6)                                                         
DSELSS2  MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
         B     DSEL4                                                            
         DROP  R9                                                               
         DROP  R3                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     MODEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN FOR ACCESS RECORDS        *         
***********************************************************************         
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
*                                  VALIDATE COUNTRY FILTER                      
         GOTO1 AFVAL,REPCTYH                                                    
         BNE   VALREQ2                                                          
         GOTO1 GETCT                                                            
         BNE   VALREQX                                                          
         MVC   SELCTY(L'CTRYCODE),APWORK                                        
         CLC   APWORK+L'CTRYCODE(L'CTRYNAM),REPCTY                              
         BE    *+14                                                             
         MVC   REPCTY(L'CTRYNAM),APWORK+L'CTRYCODE                              
         OI    REPCTYH+6,X'80'                                                  
*                                  VALIDATE SYSTEM FILTER                       
VALREQ2  GOTO1 AFVAL,REPACCSH                                                   
         BNE   VALREQ3                                                          
         CLI   FVILEN,L'SENAME                                                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALREQX                                                          
         MVC   APWORK(L'SENAME),FVIFLD                                          
*                                  GET SYSTEM LIST INFO                         
         GOTO1 GETSE                                                            
         BNE   VALREQX                                                          
         MVC   SELSYS,APWORK                                                    
*                                  VALIDATE SECURITY AGENCY FILTER              
VALREQ3  GOTO1 AFVAL,REPSECAH                                                   
         BNE   VALREQ4                                                          
         MVC   SELSECA,FVIFLD                                                   
         CLC   FVIFLD(4),=C'ONLY'                                               
         BNE   *+8                                                              
         MVI   SELSECA,SELSAOQ     SECURITY AGENCIES ONLY                       
*                                                                               
VALREQ4  MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
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
* ROUTINE TO PRINT ACCESS TYPE REPORT                                 *         
***********************************************************************         
PRTREP   L     R9,AREP                                                          
         LA    R2,IOKEY                                                         
         XC    APRECKEY,APRECKEY                                                
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         LA    R1,IOHI+IOCONFIL+IO1                                             
         B     PRTREP2                                                          
*                                  GET RECORD (SEQUENCE IS BROKEN)              
PRTREP1  LA    R2,IOKEY                                                         
         MVC   CT5KEY,APRECKEY                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   PRTREPX                                                          
         LA    R1,IOSQ+IOCONFIL+IO1                                             
PRTREP2  GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
         L     R2,AIOAREA1                                                      
         CLI   CT5KTYP,CT5KTYPQ    TEST IF AN ACCESS TYPE RECORD                
         BNE   PRTREPX                                                          
         MVC   APRECKEY(L'CT5KEY),CT5KEY                                        
         XC    REPP1,REPP1         CLEAR THE PRINT LINE                         
         MVC   LINEALPH,CT5KALPH                                                
         MVC   LINESECA,CT5KALPH   SET SECURITY AGENCY DEFAULT                  
         LA    R6,LINEACCS         SET POINTER TO SYSTEMS FIELD                 
         MVI   APFLAG,0            SET SYSTEM FILTER FLAG                       
         XC    REPP2,REPP2         CLEAR EXTRA PRINT LINE                       
         MVI   SECAFLAG,C'N'       SECURITY AGENCY FILTER FLAG                  
*                                                                               
         LA    R3,CT5DATA          PRINT EACH ELEMENT OF RECORD                 
PRTREP3  CLI   0(R3),0             E-O-R                                        
         BE    PRTREP6                                                          
         CLI   0(R3),CTDSCELQ      USER-ID                                      
         BE    PREPID                                                           
         CLI   0(R3),CTSYSELQ      SYSTEM                                       
         BE    PREPSS                                                           
         CLI   0(R3),CTAGDELQ      AGENCY GROUP DETAILS                         
         BE    PREPAD                                                           
         CLI   0(R3),CTSEAELQ      SECURITY AGENCY                              
         BE    PREPSA                                                           
*&&UK                                                                           
         CLI   0(R3),CTAGLELQ      ASSOC AGENCY LIST                            
         BE    PREPAA                                                           
*&&                                                                             
PRTREP4  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PRTREP3                                                          
*                                                                               
PRTREP6  BCTR  R6,0                REMOVE COMMA FROM SYSTEM FIELD               
         MVI   0(R6),C' '                                                       
         TM    APFLAG,X'02'        SELIST OVERFLOW?                             
         BNO   *+8                 NO                                           
         MVI   0(R6),C'>'          YES                                          
                                                                                
         OC    SELSECA,SELSECA     TEST SECURITY AGENCY SELECT                  
         BZ    PR050                                                            
         CLI   SELSECA,SELSAOQ     SECURITY AGENCIES ONLY                       
         BE    PR050                                                            
         CLI   SECAFLAG,C'N'                                                    
         BE    PRTREP1                                                          
*                                                                               
PR050    OC    SELSYS,SELSYS       SYSTEM FILTER TEST                           
         BZ    *+12                                                             
         TM    APFLAG,X'01'                                                     
         BNO   PRTREP1                                                          
*                                  PRINT REPORT LINE                            
         GOTO1 VREPORT,REPD                                                     
         B     PRTREP1                                                          
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
         USING CTDSCD,R3                                                        
PREPID   GOTO1 GETID,CTDSC         PRINT PRINCIPAL ID DATA                      
         MVC   LINEUSER,USERID                                                  
         MVC   LINEUSRN,USERNM                                                  
         B     PRTREP4                                                          
                                                                                
         USING CTAGDD,R3                                                        
PREPAD   OC    CTAGDAG,CTAGDAG     PRINT AGENCY GROUP DATA                      
         BZ    PREPAD1             IF PRESENT                                   
         GOTO1 GETID,CTAGDAG                                                    
         MVC   LINEGRUP,USERID                                                  
PREPAD1  OC    CTAGDHG,CTAGDHG     PRINT HOLDING GROUP DATA                     
         BZ    PREPAD2             IF PRESENT                                   
         GOTO1 GETID,CTAGDHG                                                    
         MVC   LINEHOLD,USERID                                                  
PREPAD2  MVC   LINETYPE,CTAGDTY    PRINT CLIENT TYPE                            
         OC    SELCTY,SELCTY       COUNTRY FILTER                               
         BZ    PREPAD3                                                          
         CLI   CTAGDLEN,CTAGDL2Q                                                
         BL    PRTREP1                                                          
         CLC   SELCTY,CTAGDCTY                                                  
         BNE   PRTREP1                                                          
PREPAD3  CLI   CTAGDLEN,CTAGDL2Q                                                
         BL    PRTREP4                                                          
         L     R1,ACTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         CLC   CTRYCODE,CTAGDCTY                                                
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   LINECTRY,CTRYSHR    PRINT COUNTRY SHORT NAME                     
         B     PRTREP4                                                          
*                                                                               
         USING CTSEAD,R3                                                        
PREPSA   MVI   SECAFLAG,C'Y'       PRINT SECURITY AGENCY                        
         MVC   LINESECA,CTSEAAID                                                
         OC    SELSECA,SELSECA                                                  
         BZ    PRTREP4                                                          
         CLI   SELSECA,SELSAOQ     SECURITY AGENCIES ONLY                       
         BNE   *+16                                                             
         CLI   CTSEAAID,C' '       SECURITY AGENCY ON RECORD?                   
         BH    PRTREP1             YES,THEN THIS AGY NOT A SEC AGY              
         B     PRTREP4                                                          
         CLC   SELSECA,CTSEAAID                                                 
         BNE   PRTREP1                                                          
         B     PRTREP4                                                          
         DROP  R1                                                               
*                                                                               
         USING CTSYSD,R3                                                        
PREPSS   OC    SELSYS,SELSYS       SYSTEM FILTER                                
         BZ    PREPSS1                                                          
         CLC   SELSYS,CTSYSSE                                                   
         BNE   PREPSS1                                                          
         OI    APFLAG,X'01'        FLAG MATCHING SYSTEM FOUND                   
PREPSS1  GOTO1 ADISSE,CTSYSSE      FORMAT SYSTEM BLOCK ENTRY                    
*NOP*    OC    APPARM(4),APPARM    RETURN STRING IN APWORK                      
*NOP*    BNZ   *+6                 ELSE APPARM=0 NOT FOUND                      
*NOP*    DC    H'00'               DON'T PANIC                                  
         LA    RF,L'SENAME+4(R6)                                                
         LA    R1,L'REPPS+REPP1                                                 
         CLR   RF,R1                                                            
         BNH   *+12                                                             
         OI    APFLAG,X'02'        SELIST OVERFLOW                              
         B     PRTREP4                                                          
         MVC   0(L'SENAME,R6),APWORK                                            
         LA    R6,L'SENAME-1(R6)                                                
         CLI   0(R6),C' '                                                       
         BNE   *+8                                                              
         BCT   R6,*-8                                                           
         GOTO1 ADISSYS,CTSYSNUM                                                 
         ICM   R1,15,APPARM        SAVE A(SELIST ENTRY)                         
         BNZ   *+6                                                              
         DC    H'00'                                                            
         USING SYSLSTD,R1                                                       
         CLI   SYSLIND1,X'40'                                                   
         BNE   PREPSS2             TEST IF BINARY AGENCY KEY                    
         MVI   1(R6),C'='                                                       
         GOTO1 VHEXOUT,APPARM,CTSYSAGB,2(R6),1,=C'TOG'                          
         LA    R6,3(R6)                                                         
PREPSS2  MVI   1(R6),C','                                                       
         LA    R6,2(R6)                                                         
         B     PRTREP4                                                          
         DROP  R1                                                               
*&&UK                                                                           
         USING CTAGLD,R3                                                        
PREPAA   ZIC   R0,CTAGLLEN         DISPLAY ASSOCIATED AGENCY LIST               
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         SRL   R0,1                                                             
         LA    RF,CTAGLAID         LOOP FOR EACH 2 CHARACTER ID                 
         XC    APWORK,APWORK                                                    
         LA    R1,APWORK                                                        
PREPAA1  MVC   0(L'CTAGLAID,R1),0(RF)                                           
         LA    RF,L'CTAGLAID(RF)                                                
         LA    R1,L'CTAGLAID(R1)                                                
         BCT   R0,PREPAA2                                                       
         MVC   LINEASAG(8),=C'ASCAGY: '                                         
         MVC   LINEASAG+8,APWORK                                                
         B     PRTREP4                                                          
PREPAA2  MVI   0(R1),C','          SEPARATED BY COMMAS                          
         LA    R1,1(R1)                                                         
         B     PREPAA1                                                          
*&&                                                                             
         DROP  R3                                                               
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* GET ID INFO FROM ID RECORDS                                         *         
* R1 POINTS TO ID NUMBER                                              *         
***********************************************************************         
GETID    NTR1  ,                                                                
         XC    USERVAL(USERVALL),USERVAL                                        
         MVC   USERNO,0(R1)        EXTRACT ID NUMBER                            
         MVC   USERID(3),=C'ID='                                                
         SR    R0,R0                                                            
         ICM   R0,3,USERNO                                                      
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  USERID+3(5),APDUB                                                
         MVI   USERNM,C'?'                                                      
         MVC   USERNM+1(L'USERNM-1),USERNM                                      
         L     R4,AIOAREA2         READ ID RECORD FROM CONTROL FILE             
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USERNO                                               
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GETIDX                                                           
*                                                                               
GETID1   LA    R1,CTIDATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
GETID2   CLI   0(R1),0                                                          
         BE    GETIDX                                                           
         CLI   0(R1),CTDSCELQ                                                   
         BNE   *+14                                                             
         MVC   USERID,CTDSC-CTDSCD(R1)                                          
         B     GETID4                                                           
         CLI   0(R1),CTAGYELQ                                                   
         BNE   *+14                                                             
         MVC   USERAL,CTAGYID-CTAGYD(R1)                                        
         B     GETID4                                                           
         CLI   0(R1),CTDSTELQ                                                   
         BNE   *+14                                                             
         MVC   USERNM,CTDSTNAM-CTDSTD(R1)                                       
         B     GETID4                                                           
*                                                                               
GETID4   IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     GETID2                                                           
*                                                                               
GETIDX   MVC   FVMSGNO,=AL2(FVFOK) IGNORE IO ERROR MESSAGE                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ID STRING FROM ID RECORDS                                  *         
* ID IN CURRENT SCEEN FIELD                                           *         
***********************************************************************         
VALID    NTR1  ,                                                                
         XC    USERVAL(USERVALL),USERVAL                                        
         MVC   USERID,FVIFLD                                                    
         CLI   FVILEN,3                                                         
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALIDER                                                          
         L     R4,AIOAREA2                                                      
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,USERID                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALIDER                                                          
         BE    VALID1                                                           
         TM    IOERR,IOEDEL                                                     
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALIDER                                                          
         MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     VALIDER                                                          
*                                                                               
VALID1   LA    R1,CTIDATA                                                       
         SR    R0,R0                                                            
VALID2   CLI   0(R1),0                                                          
         BE    VALIDOK                                                          
         CLI   0(R1),CTAGYELQ      TEST AGENCY ALPHA ID ELEMENT                 
         BNE   *+14                                                             
         MVC   USERAL,CTAGYID-CTAGYD(R1)                                        
         B     VALID4                                                           
         CLI   0(R1),CTDSCELQ      TEST AGENCY ID NUMBER                        
         BNE   *+14                                                             
         MVC   USERNO,CTDSC-CTDSCD(R1)                                          
         B     VALID4                                                           
         CLI   0(R1),CTDSTELQ                                                   
         BNE   *+14                                                             
         MVC   USERNM,CTDSTNAM-CTDSTD(R1)                                       
         B     VALID4                                                           
*                                                                               
VALID4   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VALID2                                                           
*                                                                               
VALIDER  LA    R0,1                                                             
         B     VALIDX                                                           
*                                                                               
VALIDOK  SR    R0,R0                                                            
*                                                                               
VALIDX   LTR   R0,R0                                                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK AGENCY BINARY NUMBER IS UNIQUE FOR THE SPECIFIED SYSTEM       *         
* CALLED ON UPDATE OF RECORD SYSTEM ELEMENTS                          *         
* APWORK+0(1)=CTSYSSE SYSTEM NUMBER                                   *         
* APWORK+1(1)=CTSYSAGB AGENCY BINARY CODE                             *         
***********************************************************************         
         DROP  R2                                                               
CHKAGY   NTR1  ,                                                                
         CLI   APWORK+1,0                                                       
         BE    CHAGOKX             IGNORE NULL AGENCY CODE                      
         L     R4,AIOAREA2                                                      
         USING CT5REC,R4                                                        
         XC    CT5KEY,CT5KEY       BUILD KEY OF ACCESS RECORD                   
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         GOTO1 AIO,IOHIGH+IOCONFIL+IO2   READ FIRST RECORD                      
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    CHAGOKX             CHECK EOF                                    
         B     CHAGL2                                                           
*                                                                               
CHAGL1   MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         CLI   IOKEY,CT5KTYPQ                                                   
         BNE   CHAGOKX             CHECK EOF                                    
         GOTO1 AIO,IOSQ+IOCONFIL+IO2  READ NEXT RECORD                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    CHAGOKX             CHECK EOF                                    
         B     CHAGL2                                                           
*                                                                               
CHAGL2   CLC   CT5KEY,APRECKEY                                                  
         BE    CHAGL1              AVOID CURRENT ACCESS RECORD                  
         LA    R1,CT5DATA          COMPARE DATA IN SYSTEM ELEMENTS              
         SR    R0,R0                                                            
CHAGL2A  CLI   0(R1),0             END OF RECORD                                
         BE    CHAGL2X                                                          
         CLI   0(R1),CTSYSELQ      FIND SYSTEM ELEMENTS                         
         BNE   CHAGL2B                                                          
         USING CTSYSD,R1                                                        
         CLC   APWORK(1),CTSYSSE     CHECK SAME SYSTEM #                        
         BNE   CHAGL2B                                                          
         CLC   APWORK+1(1),CTSYSAGB  COMPARE BINARY CODE                        
         BE    CHAGERRX            EXIT TO FLAG CODE EXISTS                     
         DROP  R1                                                               
         B     CHAGL2B                                                          
*                                                                               
CHAGL2B  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CHAGL2A                                                          
*                                                                               
CHAGL2X  B     CHAGL1                                                           
*                                                                               
CHAGERRX MVI   APWORK,1            FLAG CODE FOUND                              
         B     CHAGX                                                            
*                                                                               
CHAGOKX  MVI   APWORK,0            SET CC TO FLAG 'NZ' CODE NOT FOUND           
         B     CHAGX                                                            
*                                                                               
CHAGX    MVC   IOKEY(L'CT5KEY),APRECKEY                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM EXECUTIVE LIST VALUES                                    *         
***********************************************************************         
GETSE    NTR1  ,                                                                
         L     R1,ASYSFACS         GET SYSTEM LIST INFO                         
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SENAME,APWORK                                                    
         BE    GETSEOK                                                          
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)                                            
         LA    R0,1                                                             
         B     GETSEX                                                           
*                                                                               
GETSEOK  MVC   APWORK(2),SESYS                                                  
         SR    R0,R0                                                            
*                                                                               
GETSEX   LTR   R0,R0                                                            
         B     EXIT                                                             
         DROP  R1                                                               
***********************************************************************         
* COUNT SYSTEM ENTRIES IN PHASE LIST                                  *         
* SYSTEM # AT 0(R1) COUNT IN APWORK(1)                                *         
***********************************************************************         
CNTSE    NTR1  ,                                                                
         MVI   APWORK,0                                                         
         MVC   APWORK+1(1),0(R1)                                                
         L     R1,ASYSFACS         GET SYSTEM LIST INFO                         
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
CNTSE1   CLC   SEOVSYS(1),APWORK+1                                              
         BNE   CNTSE2                                                           
         ZIC   R4,APWORK                                                        
         LA    R4,1(R4)                                                         
         STC   R4,APWORK                                                        
CNTSE2   BXLE  R1,RE,CNTSE1                                                     
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* GET COUNTRY TABLE VALUES                                            *         
***********************************************************************         
GETCT    NTR1  ,                                                                
         L     R1,ACTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         ZIC   R4,FVILEN                                                        
         BCTR  R4,0                R4=L'INPUT-1                                 
GETCT1   CLI   FVILEN,L'CTRYSHR                                                 
         BH    GETCT2                                                           
         EX    R4,*+8              MATCH ON SHORT NAME                          
         BE    GETCTOK                                                          
         CLC   CTRYSHR(0),FVIFLD                                                
GETCT2   EX    R4,*+8                                                           
         BE    GETCTOK                                                          
         CLC   CTRYNAM(0),FVIFLD   MATCH ON LONG NAME                           
         BXLE  R1,RE,GETCT1                                                     
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         LA    R0,1                                                             
         B     GETCTX                                                           
*                                                                               
GETCTOK  MVC   CURR,CTRYCURR                                                    
         MVC   APWORK(L'CTRYCODE),CTRYCODE                                      
         MVC   APWORK+L'CTRYCODE(L'CTRYNAM),CTRYNAM                             
         SR    R0,R0                                                            
*                                                                               
GETCTX   LTR   R0,R0                                                            
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A CHARACTER AGAINST A TABLE                     *         
***********************************************************************         
VALCHAR  CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         LTR   RE,RE                                                            
         BR    RE                                                               
         CLC   0(1,R1),FVIFLD                                                   
         BER   RE                                                               
         LA    R1,1(R1)                                                         
         B     VALCHAR                                                          
         LTORG                                                                  
         EJECT                                                                  
MEDSEQ   EQU   X'04'               MEDIA SYSTEM CODE NUMBER                     
ACCSEQ   EQU   X'06'               ACCOUNT SYSTEM CODE NUMBER                   
*                                                                               
CTFILE   DC    CL8'CTFILE  '                                                    
*                                                                               
REPDESCL DC    C'ACCESS LIST'                                                   
                                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'ACCESS TYPE LIST'                                        
         SPEC  H2,57,C'----------------'                                        
         SPEC  M1,1,C'ID CTY PRINCIPAL PRINCIPAL       GROUP'                   
         SPEC  M2,1,C'-- --- ID        ID NAME         ID   '                   
         SPEC  M1,44,C'HOLDING   CLT SE SYSTEM SPECIFICATIONS'                  
         SPEC  M2,44,C'ID        TYP AG ---------------------'                  
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
* DDTEAMLST                                                                     
       ++INCLUDE DDTEAMLST                                                      
*&&US                                                                           
***********************************************************************         
* DISPLAY CFM AGENCY LIST ELEMENT                                     *         
***********************************************************************         
         USING CTCAGD,R3                                                        
DCAG     NTR1  BASE=*,LABEL=*                                                   
         OI    SYSCAGH+6,X'80'                                                  
         XC    SYSCAG,SYSCAG                                                    
*                                                                               
         LA    RE,SYSCAG           RE=A(SCREEN FIELD)                           
         LA    RF,CTCAGAGY         RF=A(ELEMENT AGENCY LIST)                    
         LLC   R1,CTCAGLEN                                                      
         AHI   R1,-CTCAGLNQ        DISP TO DATA                                 
*                                                                               
DC010    LTR   R1,R1               ANY MORE AGENCIES IN LIST                    
         BNP   DC030               NO                                           
         CLC   0(L'CTCAGAGY,RF),=C'  '                                          
         BNH   DCX                                                              
         CLC   SYSALPH,0(RF)                                                    
         BE    DC020                                                            
*                                                                               
         MVC   0(L'CTCAGAGY,RE),0(RF)                                           
         MVI   L'CTCAGAGY(RE),C','                                              
         LA    RE,(L'CTCAGAGY+1)(RE)                                            
DC020    LA    RF,L'CTCAGAGY(RF)                                                
         AHI   R1,-L'CTCAGAGY                                                   
         B     DC010                                                            
*                                                                               
DC030    AHI   RE,-1               REMOVE COMMA                                 
         MVI   0(RE),C' '                                                       
DCX      J     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
                                                                                
*&&                                                                             
***********************************************************************         
* CHECK THAT USER IDS ARE COMPATIBLE WITH UAT ACCESS SETTING                    
***********************************************************************         
         USING CT9BREC,R2          R2=A(RECORD KEY)                             
CHKUAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,IOKEY                                                         
         XC    CT9BKEY,CT9BKEY                                                  
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01                                                
         MVC   CT9BKAGY,SYSALPH                                                 
*                                                                               
         L     R2,AIOAREA2                                                      
         GOTO1 AIO,IOCONFIL+IOHI+IO2                                            
         BNE   CHKUOKX                                                          
         B     CHKU020                                                          
CHKU010  GOTO1 AIO,IOCONFIL+IOSEQ+IO2                                           
         BNE   CHKUOKX             ERROR EXIT                                   
CHKU020  CLI   CT9BKTYP,CT9BKTYQ   MAKE SURE CORRECT TYPE OF RECORD             
         BNE   CHKUOKX                                                          
         CLI   CT9BKSUB,CT9BKS01   MAKE SURE CORRECT SUB TYPE                   
         BNE   CHKUOKX                                                          
         CLC   CT9BKAGY,SYSALPH    MAKE SURE CORRECT AGENCY ALPHA               
         BNE   CHKUOKX                                                          
*                                                                               
         LA    R3,CT9BDATA                                                      
CHKU030  CLI   0(R3),0                                                          
         BE    CHKU010                                                          
         CLI   0(R3),CTDSCELQ      X'02' DESCRIPTION ELEMENT                    
         BE    CHKU050                                                          
CHKU040  LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CHKU030                                                          
*                                                                               
         USING CTDSCD,R3                                                        
CHKU050  LLC   R1,CTDSCLEN         ELEMENT LENGTH                               
         AHI   R1,-(CTDSC-CTDSCEL)-1                                            
         BM    CHKU010             NO ID FOUND, GET NEXT RECORD                 
         LA    R4,CTDSC                                                         
         AR    R4,R1               LAST CHARACTER OF USER ID                    
CHKU055  CLI   0(R4),C' '                                                       
         BH    CHKU060                                                          
         BCTR  R4,0                                                             
         BCT   R1,CHKU055                                                       
         B     CHKU010             NO ID FOUND, GET NEXT RECORD                 
CHKU060  AHI   R4,-2                                                            
         CLC   0(3,R4),=C'UAT'     "UAT"?                                       
         BNE   CHKUERX             NO: ERROR, MUST END IN "UAT"                 
         B     CHKU010             GOOD: GET NEXT RECORD                        
*                                                                               
CHKUERX  MVC   FVMSGNO,=AL2(111)                                                
         LTR   RB,RB                                                            
         J     EXIT                                                             
CHKUOKX  CR    RB,RB                                                            
         J     EXIT                                                             
         DROP  R2,R3                                                            
         LTORG                                                                  
                                                                                
***********************************************************************         
* CHECK FOR NEW AGENCY LABEL ALREADY IN USE                           *         
***********************************************************************         
         USING CT5REC,R2           R2=A(RECORD KEY)                             
CHKLABEL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         L     R2,AIOAREA2                                                      
*                                                                               
         GOTO1 AIO,IOCONFIL+IOHI+IO2                                            
         BNE   CHKLERX                                                          
         B     CHKL020                                                          
*                                                                               
CHKL010  GOTO1 AIO,IOCONFIL+IOSEQ+IO2                                           
         BNE   CHKLOKX             ERROR EXIT                                   
*                                                                               
CHKL020  CLI   0(R2),CT5KTYPQ      MAKE SURE CORRECT TYPE OF RECORD             
         BNE   CHKLOKX                                                          
*                                                                               
         LA    R3,CT5DATA                                                       
CHKL030  CLI   0(R3),0                                                          
         BE    CHKL010                                                          
                                                                                
         CLI   0(R3),CTAGCELQ                                                   
         BE    CHKL050                                                          
*                                                                               
CHKL040  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CHKL030                                                          
*                                                                               
         USING CTAGCD,R3                                                        
CHKL050  CLC   CTAGCCOD,FVIFLD     CHECK FOR AGENCY LABEL                       
         BNE   CHKL010                                                          
*                                                                               
CHKLERX  LTR   RB,RB                                                            
         J     EXIT                                                             
CHKLOKX  CR    RB,RB                                                            
CHKLABX  J     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* CHECK CFM AGENCY LIST FOR VALID AGENCIES AND BUILD ELEMENT                    
***********************************************************************         
         USING CT5REC,R2                                                        
CHKCAG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    APELEM,APELEM                                                    
         OI    SYSCAGH+6,X'80'                                                  
         NI    ACDFLAG,X'FF'-ACDAICQ                                            
*                                                                               
         LA    R3,APELEM           R9=A(NEW CFM AGENCY LIST ELEMENT)            
         USING CTCAGD,R3                                                        
         MVI   CTCAGEL,CTCAGELQ    X'C5' CFM AGENCY LIST                        
         MVI   CTCAGLEN,CTCAGLNQ                                                
*                                                                               
         MVI   FVINDX,0            SCAN INPUT FIELD LIST                        
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCKI)                               
         CLI   4(R1),0                                                          
         BE    CCNVX                                                            
*                                                                               
         LA    R4,BLOCKI           R4=A(SCAN BLOCK)                             
         USING SCANBLKD,R4                                                      
         MVC   FLDCNT,4(R1)        SAVE # FIELDS IN SCAN                        
         MVI   FVINDX,1            INITIALISE SCAN FIELD INDEX                  
*                                                                               
CC010    CLC   FVINDX,FLDCNT       PROCESS EACH FIELD IN SCAN LIST              
         BH    CC060                                                            
         CLI   SC2NDLEN,0          VALID SINGLE FIELD                           
         BNE   CCNVX                                                            
         CLI   SC1STLEN,L'CTCAGAGY VALID FIELD LENGTH                           
         BNE   CCNVX                                                            
*                                                                               
         LLC   RF,FLDCNT           CHECK FOR DUPLICATE IN LIST                  
         LA    R1,BLOCKI                                                        
L        USING SCANBLKD,R1                                                      
CC015    CR    R1,R4               NO NEED TO COMPARE TO SELF                   
         BE    CC016                                                            
         CLC   SCONEFLD,L.SCONEFLD ERROR IF DUPE                                
         BE    CCDPX                                                            
CC016    LA    R1,SCBLKLQ(R1)                                                   
         BCT   RF,CC015                                                         
         DROP  L                                                                
*                                                                               
         CLC   SYSALPH,SCONEFLD    NO NEED TO VALIDATE THIS AGENCY              
         BNE   CC018                                                            
         CLI   FLDCNT,1            IS IT THE ONLY ONE?                          
         BE    CCHIX               YES,EXIT AND TREAT AS EMPTY                  
         OI    ACDFLAG,ACDAICQ     DONT ADD IT LATER                            
         B     CC040                                                            
*                                                                               
CC018    XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            GET ACCESS RECORD                            
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,SCONEFLD                                                
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNE   CCNVX               NOT A VALID AGENCY                           
*                                                                               
         L     R2,AIOAREA2         GET CFM AGENCY LIST ELEMENT                  
         GOTO1 VHELLO,APPARM,(C'G',CTFILE),('CTCAGELQ',CT5KEY),0,0              
         CLI   12(R1),X'06'        NOT FOUND                                    
         BE    CC040                                                            
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,12(R1)           IF ELEMENT FOUND, MAKE SURE IT'S             
A        USING CTCAGD,R1           THE SAME AS THE CFM AGENCY LIST              
         LLC   RF,A.CTCAGLEN       LIST ELEMENT AS ORIGINALLY ON THE            
         AHI   RF,-1               ACCESS RECORD WE ARE CURRENTLY               
         EX    RF,*+8              MAINTAINING                                  
         B     *+10                                                             
         CLC   A.CTCAGEL(0),OLDCAGEL                                            
         BNE   CCRAX                                                            
         DROP  A                                                                
*                                                                               
CC040    LA    R1,APELEM           PUT AGENCY INTO THE ELEMENT                  
         AHI   R1,CTCAGLNQ                                                      
         LLC   R0,FVINDX                                                        
         AHI   R0,-1                                                            
         MHI   R0,L'CTCAGAGY                                                    
         AR    R1,R0                                                            
         MVC   0(L'CTCAGAGY,R1),SCONEFLD                                        
*                                                                               
         LLC   R0,CTCAGLEN         INCREASE ELEMENT LENGTH                      
         AHI   R0,L'CTCAGAGY                                                    
         STC   R0,CTCAGLEN                                                      
*                                                                               
         LLC   R0,FVINDX           BUMP INDEX                                   
         AHI   R0,1                                                             
         STC   R0,FVINDX                                                        
*                                                                               
CC050    LA    R4,SCBLKLQ(R4)                                                   
         L     R2,AIOAREA1         RESET R2=A(CURRENT ACCESS RECORD)            
         B     CC010                                                            
         DROP  R4                                                               
*                                                                               
CC060    TM    ACDFLAG,ACDAICQ     THIS AGENCY ALREADY IN LIST                  
         BZ    CC070               YES,DONT NEED TO ADD IT                      
         LLC   R0,FVINDX                                                        
         AHI   R0,-1                                                            
         STC   R0,FVINDX                                                        
         B     CC080                                                            
*                                                                               
CC070    LA    R1,APELEM                                                        
         AHI   R1,CTCAGLNQ                                                      
         LLC   R0,FVINDX                                                        
         AHI   R0,-1                                                            
         MHI   R0,L'CTCAGAGY                                                    
         AR    R1,R0                                                            
         MVC   0(L'CTCAGAGY,R1),SYSALPH PUT THIS AGENCY IN ALWAYS               
*                                                                               
         LLC   R0,CTCAGLEN         INCREASE ELEMENT LENGTH                      
         AHI   R0,L'CTCAGAGY                                                    
         STC   R0,CTCAGLEN                                                      
*                                                                               
CC080    LLC   R4,FVINDX                                                        
         GOTO1 =V(XSORT),APPARM,(0,CTCAGAGY),(R4),                     *        
               L'CTCAGAGY,L'CTCAGAGY,0,RR=APRELO                                
         B     CCOKX                                                            
*                                                                               
CCRAX    MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     CCLOX                                                            
CCDPX    MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     CCLOX                                                            
CCNVX    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     CCLOX                                                            
*                                                                               
CCHIX    BRAS  RE,DCAG             DISPLAY NEW CFM AGENCY LIST ELEMENT          
         MVI   BYTE,2              HIGH MEANS NO ELEMENT NEEDED                 
         B     CCX                                                              
CCLOX    MVI   BYTE,0              LOW MEANS ERROR IN FIELD                     
         B     CCX                                                              
CCOKX    BRAS  RE,DCAG             DISPLAY NEW CFM AGENCY LIST ELEMENT          
         MVI   BYTE,1              EQUAL MEANS ELEMENT CREATED                  
*                                                                               
CCX      CLI   BYTE,1                                                           
         J     EXIT                                                             
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK CFM AGENCY LIST FOR VALID AGENCIES AND BUILD ELEMENT          *         
***********************************************************************         
         USING CT5REC,R2                                                        
SETCAG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    NEWCAGEL,NEWCAGEL                                                
*                                                                               
         L     R2,AIOAREA1         GET CFM AGENCY LIST ELEMENT                  
         GOTO1 VHELLO,APPARM,(C'G',CTFILE),('CTCAGELQ',CT5KEY),0,0              
         CLI   12(R1),X'06'        NOT FOUND                                    
         BE    SC010                                                            
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,12(R1)                                                        
         USING CTCAGD,R1                                                        
         LLC   RF,CTCAGLEN         GRAB CURRENT CFM AGY LIST ELEM               
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NEWCAGEL(0),CTCAGEL                                              
         DROP  R1                                                               
*                                                                               
SC010    CLC   NEWCAGEL,OLDCAGEL   IF OLD AND NEW SAME, NO CHANGE               
         BE    SCX                                                              
*                                                                               
         OC    OLDCAGEL,OLDCAGEL                                                
         BZ    SC100                                                            
*                                                                               
         LA    R4,OLDCAGEL         A(OLD CTCAGEL)                               
         LLC   R3,1(R4)            LENGTH OF ELEMENT                            
         AHI   R3,-CTCAGLNQ        LENGTH OF OVERHEAD                           
         SRL   R3,1                /2 (OK, IT'S A CHEAT FOR L'CTCAGAGY)         
         AHI   R4,CTCAGLNQ         R4=A(AGENCY IN OLD LIST)                     
*                                                                               
         OC    NEWCAGEL,NEWCAGEL                                                
         BZ    SC060                                                            
*                                                                               
SC020    CLC   0(L'CTCAGAGY,R4),SYSALPH                                         
         BE    SC050               SKIP THIS AGENCY                             
*                                                                               
         LA    RF,NEWCAGEL         A(NEW CTCAGEL)                               
         LLC   RE,1(RF)            LENGTH OF ELEMENT                            
         AHI   RE,-CTCAGLNQ        R1=NUMBER OF AGENCIES                        
         SRL   RE,1                /2 (ANOTHER CHEAT FOR L'CTCAGAGY)            
         AHI   RF,CTCAGLNQ         RF=A(AGENCY IN NEW LIST)                     
*                                                                               
SC030    CLC   0(L'CTCAGAGY,R4),0(RF) AGENCY IN THE NEW LIST                    
         BE    SC050               YES THEN NOTHING NEEDED                      
         LA    RF,L'CTCAGAGY(RF)                                                
         BCT   RE,SC030                                                         
*                                                                               
SC040    XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            GET ACCESS RECORD                            
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R4)      PULL AGENCY FROM OLD LIST                    
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2     THIS NEEDS TO BE UPDATE             
         BE    *+6                 NOT A VALID AGENCY                           
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
*                                                                               
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),('CTCAGELQ',CT5KEY),0,0              
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    SC050                                                            
         DC    H'0'                                                             
*                                                                               
SC050    LA    R4,L'CTCAGAGY(R4)                                                
         BCT   R3,SC020                                                         
         B     SC100                                                            
*                                                                               
SC060    LA    RF,OLDCAGEL         REMOVE THIS AGENCY FROM THE OLD ELEM         
         LLC   RE,1(RF)            THEN REPLACE OTHER AGENCIES WITH THE         
         AHI   RE,-CTCAGLNQ        THIS ELEMENT                                 
         SRL   RE,1                                                             
         AHI   RF,CTCAGLNQ                                                      
         MVC   NEWCAGEL(CTCAGLNQ),OLDCAGEL                                      
         LLC   R1,NEWCAGEL+1                                                    
         AHI   R1,-L'CTCAGAGY                                                   
         STC   R1,NEWCAGEL+1                                                    
         LA    R1,NEWCAGEL                                                      
         AHI   R1,CTCAGLNQ                                                      
*                                                                               
SC070    CLC   0(L'CTCAGAGY,RF),SYSALPH  COPY OLD TO NEW WITHOUT THIS           
         BE    *+14                      AGENCY                                 
         MVC   0(L'CTCAGAGY,R1),0(RF)                                           
         LA    R1,L'CTCAGAGY(R1)                                                
         LA    RF,L'CTCAGAGY(RF)                                                
         BCT   RE,SC070                                                         
*                                                                               
         CLI   NEWCAGEL+1,CTCAGLNQ+L'CTCAGAGY   ONLY ONE AGENCY LEFT?           
         BNE   SC100                            NO CONTINUE                     
         MVC   OLDCAGEL,NEWCAGEL                                                
*                                                                               
         LHI   R3,1                                                             
         LA    R4,OLDCAGEL+(CTCAGAGY-CTCAGEL)                                   
         XC    NEWCAGEL,NEWCAGEL                                                
         B     SC040               DELETE ELEM FROM REC                         
*                                                                               
SC100    OC    NEWCAGEL,NEWCAGEL                                                
         BZ    SCX                                                              
*                                                                               
         LA    R4,NEWCAGEL         A(NEW CTCAGEL)                               
         LLC   R3,1(R4)            LENGTH OF ELEMENT                            
         AHI   R3,-CTCAGLNQ        LENGTH OF OVERHEAD                           
         SRL   R3,1                /2 (OK, IT'S A CHEAT FOR L'CTCAGAGY)         
         AHI   R4,CTCAGLNQ         R4=A(AGENCY IN OLD LIST)                     
*                                                                               
SC110    CLC   0(L'CTCAGAGY,R4),SYSALPH                                         
         BE    SC130               SKIP THIS AGENCY                             
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            GET ACCESS RECORD                            
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R4)      PULL AGENCY FROM OLD LIST                    
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2     THIS NEEDS TO BE UPDATE             
         BE    *+6                 NOT A VALID AGENCY                           
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA2                                                      
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),('CTCAGELQ',CT5KEY),0,0              
         CLI   12(R1),X'06'        ELEMENT NOT FOUND                            
         BE    SC120                                                            
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
SC120    GOTO1 VHELLO,APPARM,(C'P',CTFILE),CT5KEY,NEWCAGEL,0                    
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    SC130                                                            
         DC    H'0'                                                             
*                                                                               
SC130    LA    R4,L'CTCAGAGY(R4)                                                
         BCT   R3,SC110                                                         
*                                                                               
SCX      J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
AGYTYP   DC    X'01',AL1(2,3,CTAGTST),CL4'TST'   TEST     AGENCY                
         DC    X'01',AL1(2,3,CTAGTNG),CL4'TRN'   TRAINING AGENCY                
         DC    X'01',AL1(1,3,CTAGUAT),CL4'UAT'   UAT      AGENCY                
         DC    X'02',AL1(1,3,CTAGSAP),CL4'SAP'   SAP      AGENCY                
         DC    X'FF',AL1(1,4,CTAGNUL),CL4'NONE'  NONE                           
CTAGNUL  EQU   0                                                                
         EJECT                                                                  
**********************************************************************          
* SHARED DSECTS                                                                 
**********************************************************************          
*&&                                                                             
* DDTEAMLSTD                                                                    
       ++INCLUDE DDTEAMLSTD                                                     
* DDSCANBLKD                                                                    
       ++INCLUDE DDSCANBLKD                                                     
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
* CTGENWEB                                                                      
       ++INCLUDE CTGENWEB                                                       
* DDCTRYEQUS                                                                    
       ++INCLUDE DDCTRYEQUS                                                     
* SEACSFILE                                                                     
       ++INCLUDE SEACSFILE                                                      
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENE8D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENC8D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENA8D                                                       
         ORG                                                                    
         EJECT                                                                  
**********************************************************************          
* INTERNAL DSECTS                                                               
**********************************************************************          
AGYTYPD  DSECT                                                                  
AGYCOMBO DS    X                   COMBO TESTING BIT (ALLOWABLE COMBOS)         
AGYMINL  DS    AL1                 MIN LENGTH                                   
AGYMAXL  DS    AL1                 MAX LENGTH                                   
AGYOPT   DS    X                   AGENCY TYPE FLAG OPTION                      
AGYTEXT  DS    CL4                 TEXT TO VALIDATE / DISPLAY                   
AGYTYLNQ EQU   *-AGYTYPD                                                        
*                                                                               
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL(L'LSTACT1)       ACTION FIELD                                 
LISTLINH DS    XL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTALPH DS    CL2                 AGENCY ACCESS ALPHA CODE                     
         DS    CL1                                                              
LISTCTRY DS    CL3                 COUNTRY (ABREVIATED)                         
         DS    CL1                                                              
LISTUSER DS    CL8                 PRINCIPAL ID                                 
         DS    CL1                                                              
LISTGRUP DS    CL8                 AGENCY GROUP ID                              
         DS    CL1                                                              
LISTHOLD DS    CL8                 HOLDING COMPANY ID                           
         DS    CL1                                                              
LISTTYPE DS    CL1                 CLIENT TYPE                                  
         DS    CL1                                                              
LISTSECA DS    CL2                 SECURITY AGENCY                              
         DS    CL1                                                              
LISTTEAM DS    CL4                 SERVICE TEAM                                 
         DS    CL1                                                              
LISTACCS DS    CL24                SYSTEM LIST VALUES                           
         DS    CL1                                                              
LISTLAB  DS    CL4                 AGENCY LABEL                                 
         ORG   LISTLIN+L'LISTLIN                                                
                                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
LINEALPH DS    CL2                 AGENCY ALPHA CODE                            
         DS    CL1                                                              
LINECTRY DS    CL3                 COUNTRY (ABREVIATED)                         
         DS    CL1                                                              
LINEUSER DS    CL8                 PRINCIPAL ID                                 
         DS    CL2                                                              
LINEUSRN DS    CL15                PRINCIPAL ID NAME                            
         DS    CL1                                                              
LINEGRUP DS    CL8                 AGENCY GROUP ID                              
         DS    CL2                                                              
LINEHOLD DS    CL8                 HOLDING COMPANY ID                           
         DS    CL2                                                              
         DS    CL1                                                              
LINETYPE DS    CL1                 CLIENT TYPE                                  
         DS    CL1                                                              
         DS    CL1                                                              
LINESECA DS    CL2                 SECURITY AGENCY                              
         DS    CL1                                                              
LINEACCS DS    CL72                SYSTEM INFO                                  
         ORG   REPP2                                                            
         DS    CL45                                                             
LINEASAG DS    CL87                ASSOCIATED AGENCY LIST                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER LOCAL W/S                                            *         
***********************************************************************         
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
SELKEY   DS    0XL10               SELECT KEY PARAMETERS                        
SELCTY   DS    XL1                 COUNTRY                                      
SELGID   DS    CL2                 GROUP ID                                     
SELHID   DS    CL2                 HOLDING ID                                   
SELTYP   DS    CL1                 CLIENT TYPE                                  
SELSYS   DS    CL1                 SYSTEM #                                     
SELSECA  DS    CL2                 SECURITY AGENCY                              
SELSAOQ  EQU   1                   SELECT SECURITY AGENCIES ONLY                
SELCSTN  DS    XL1                 CLIENT SERVICE TEAM NUMBER                   
         ORG   SELKEY+L'SELKEY                                                  
VSCINKEY DS    V                   UNSCAN ROUTINE ADDRESS                       
DMCB     DS    8F                                                               
SVRERF   DS    0A                                                               
SVRE     DS    2A                                                               
BYTE     DS    X                                                                
WORK     DS    XL256                                                            
CTRY     DS    XL1                 COUNTRY CODE                                 
CURR     DS    CL3                 CURRENCY                                     
HOLDID   DS    XL2                 HOLDING COMPANY ID                           
GRUPID   DS    XL2                 AGENCY GROUP ID                              
CLTYPE   DS    CL1                 CLIENT TYPE                                  
DDSACC   DS    XL1                 DDS ACCESS LEVEL FLAG                        
SYSCNT   DS    X                   SYSTEM # FIELD COUNT                         
FLDCNT   DS    X                   SUB-FIELD COUNT                              
SECAFLAG DS    CL1                 SECURITY AGENCY FILTER FLAG                  
ACDFLAG  DS    XL1                 ACCESS DETAIL FLAG                           
ACDPPSQ  EQU   X'80'               PPS=Y FOUND ON ACCESS DETAIL ELEM            
ACDLABQ  EQU   X'40'               AGENCY LABEL FOUND                           
ACDAICQ  EQU   X'20'               AGENCY IN CAG FIELD                          
ACDAADQ  EQU   X'10'               AGENCY ACCESS DETAILS FOUND                  
PRELAB   DS    CL4                 PREVIOUS AGENCY LABEL                        
PREAGOPT DS    X                   PREVIOUS CTAGOPTS                            
USERVAL  DS    0C                  USER ID VALUES                               
USERID   DS    CL10                USER-ID CODE                                 
USERNM   DS    CL33                USER-ID NAME                                 
USERNO   DS    CL2                 USER-ID NUMBER                               
USERAL   DS    CL2                 USER-ID ALPHA-ID                             
USERVALL EQU   *-USERVAL                                                        
ACCSE    DS    CL1                 ACCOUNT SE # SAVE                            
MEDSE    DS    CL1                 MEDIA SE # SAVE                              
MEDAGB   DS    CL1                 MEDIA AGENCY BINARY SAVE                     
ACCFLAG  DS    CL1                 ACCOUNT SE MATCH FLAG                        
MEDFLAG  DS    CL1                 MEDIA SE MATCH FLAG                          
*&&US                                                                           
OLDCAGEL DS    CL256               OLD CFM AGENCY LIST ELEMENT                  
NEWCAGEL DS    CL256               NEW CFM AGENCY LIST ELEMENT                  
*&&                                                                             
LISTBUFF DS    CL256               SYSTEM INFO DISPLAY BUFFER                   
*                                                                               
OVSYS    DS    XL256                                                            
BLOCKI   DS    20CL32              SCAN BLOCK                                   
         ORG   BLOCKI                                                           
BLOCKO   DS    20CL20              UNSCAN BLOCK                                 
         ORG                                                                    
*                                                                               
LOCALX   EQU   *                   END OF LOCAL WORKING STORAGE                 
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013CTGEN18   11/06/17'                                      
         END                                                                    
