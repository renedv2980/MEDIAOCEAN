*          DATA SET CTGEN18S   AT LEVEL 076 AS OF 08/22/00                      
*PHASE TA0B18A                                                                  
*                                                                               
*INCLUDE SCINKEY                                                                
*INCLUDE NUMVAL                                                                 
*                                                                               
         TITLE 'CTGEN18 - FILE MAINTENANCE - ACCESS TYPE (5) RECORDS'           
GEN18    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GE18**,RA,RR=RE                                              
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
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF ACCESS TYPE RECORD                       *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,SYSALPHH                                                   
         BNE   VALKEYX                                                          
         MVC   CT5KALPH,FVIFLD                                                  
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
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN ACCESS TYPE RECORD                      *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVI   MEDSE,0                                                          
         MVI   MEDAGB,0                                                         
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
         CLI   0(R3),CTAGDELQ                                                   
         BE    VRCHA30                                                          
         CLI   0(R3),CTAGLELQ                                                   
         BE    VRCHA30                                                          
         CLI   0(R3),CTRPIELQ                                                   
         BE    VRCHA30                                                          
*&&US                                                                           
         CLI   0(R3),CTSPUELQ                                                   
         BE    VRCHA30                                                          
*&&                                                                             
*&&UK                                                                           
         CLI   0(R3),CTMAGELQ                                                   
         BE    VRCHA30                                                          
*&&                                                                             
         CLI   0(R3),CTSEAELQ                                                   
         BE    VRCHA30                                                          
         CLI   0(R3),CTAADELQ                                                   
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
VRCHA30  SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),CT5REC),0,0                    
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
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
*                                                                               
         XC    DDSACC,DDSACC                                                    
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
         B     DATAV9A                                                          
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
         GOTO1 AADDELS,CT5REC                                                   
         EJECT                                                                  
*                                                                               
DVRPI    EQU   *                   VALIDATE RFP PRINCIPLE USER ID               
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         XC    SYSRPIN,SYSRPIN                                                  
         OI    SYSRPINH+6,X'80'                                                 
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,SYSRPIH                                                    
         BNE   DVRPIX                                                           
         GOTO1 VALID                                                            
         BNE   VALRECX                                                          
         MVC   SYSRPIN,USERNM      DISPLAY ID NAME                              
         USING CTRPID,R3                                                        
         MVI   CTRPIEL,CTRPIELQ    ADD ELEMENT                                  
         MVI   CTRPILEN,CTRPILNQ                                                
         MVC   CTRPINUM(2),USERNO                                               
         GOTO1 AADDELS,CT5REC                                                   
DVRPIX   EQU   *                                                                
*                                                                               
*&&US                                                                           
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
*                                                                               
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
         BNE   *+16                                                             
         CLI   SYSCABL,C'Y'        USES AMS CABLE DATA?                         
         BNE   *+8                                                              
         OI    CTSYSIND,CTSYSNCA   YES, FLAG IT IN RECORD                       
*&&                                                                             
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
         ICM   R8,15,APPARM        CHECK IF BINARY ID REQUIRED                  
         BNZ   *+6                 FROM SYSTEM LIST INDICATOR                   
         DC    H'00'                                                            
         USING SYSLSTD,R8                                                       
         CLI   SYSLIND1,X'40'                                                   
         BNE   DATAV12B                                                         
         DROP  R8                                                               
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
*                                  ADD SYSTEM ELEMENT                           
DATAV12C GOTO1 AADDELS,CT5REC                                                   
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
*                                  READ ASSOCIATED AGENCIES LIST                
         GOTO1 AFVAL,SYSASAGH                                                   
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
DATAVB   EQU   *                   VALIDATE SECURITY AGENCY ALPHA ID            
         GOTO1 AFVAL,SYSSECAH                                                   
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
DATAVB1  EQU   *                                                                
         LA    R4,CT5DATA                                                       
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
DATAVC1  EQU   *                                                                
DATAVC2  GOTO1 AFVAL,SYSPTOUH      VALIDATE PASSWORD TIMEOUT                    
         BNE   DATAVC3                                                          
         GOTO1 =V(NUMVAL),APPARM,SYSPTOU,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    DATAVC2E                                                         
         L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    DATAVC2E                                                         
         C     R1,=F'255'                                                       
         BH    DATAVC2E                                                         
         STCM  R1,1,CTAADPTO                                                    
         B     DATAVC3                                                          
DATAVC2E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVC3  GOTO1 AFVAL,SYSPMINH      VALIDATE PASSWORD MINIMUM LENGTH             
         BNE   DATAVC4                                                          
         GOTO1 =V(NUMVAL),APPARM,SYSPMIN,(X'01',0),RR=APRELO                    
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
DATAVC4  GOTO1 AFVAL,SYSPRUSH      VALIDATE PASSWORD REUSE NUMBER               
         BNE   DATAVC5                                                          
         GOTO1 =V(NUMVAL),APPARM,SYSPRUS,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    DATAVC4E                                                         
         L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    DATAVC4E                                                         
         C     R1,=F'255'                                                       
         BH    DATAVC4E                                                         
         STCM  R1,1,CTAADPRU                                                    
         B     DATAVC5                                                          
DATAVC4E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVC5  GOTO1 AFVAL,SYSPIDRH       VALIDATE PERSONAL-ID REQUIRED FLAG          
         BNE   DATAVC6                                                          
         OI    SYSPIDRH+6,X'80'                                                 
         CLI   FVIFLD,C'N'                                                      
         BE    DATAVC6                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   DATAVC5E                                                         
         OI    CTAADFLG,CTAADPRQ                                                
         B     DATAVC6                                                          
DATAVC5E MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVC6  GOTO1 AADDELS,CT5REC                                                   
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
*                                                                               
*&&UK                                                                           
DATAVE   EQU   *                   VALIDATE MEDIA AGENCY ALPHA ID               
         GOTO1 AFVAL,SYSMAGH                                                    
         BNE   DATAVEX                                                          
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
         BE    DATAVE1                                                          
         TM    IOERR,IOEDEL                                                     
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALRECX                                                          
         MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     VALRECX                                                          
DATAVE1  EQU   *                                                                
         LA    R4,CT5DATA                                                       
*                                  VALIDATE ID                                  
DATAVE2  CLI   0(R4),0             E-O-R                                        
         BE    DATAVE3                                                          
         CLI   0(R4),CTMAGELQ      INVALID IF MEDIA AGENCY PRESENT              
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         ZIC   RF,1(R4)            GO TO NEXT ELEMENT IN ACCESS RECORD          
         AR    R4,RF                                                            
         B     DATAVE2                                                          
*                                  VALIDATE ID                                  
DATAVE3  EQU   *                                                                
         LA    R4,CT5DATA                                                       
*                                  VALIDATE ID                                  
         USING CTSYSD,R4                                                        
DATAVE4  CLI   0(R4),0             E-O-R                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         CLI   CTSYSEL,CTSYSELQ    FIND MEDIA SYSTEM ELEMENT                    
         BNE   DATAVE5                                                          
         CLI   CTSYSNUM,MEDSEQ                                                  
         BNE   DATAVE5                                                          
         CLC   CTSYSSE,MEDSE                                                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         CLC   CTSYSAGB,MEDAGB                                                  
         BE    DATAVE6                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
DATAVE5  ZIC   RF,1(R4)            GO TO NEXT ELEMENT IN ACCESS RECORD          
         AR    R4,RF                                                            
         B     DATAVE4                                                          
         DROP  R4                                                               
*                                                                               
DATAVE6  L     R2,AIOAREA1                                                      
*                                  ADD ELEMENT                                  
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTMAGD,R3                                                        
         MVI   CTMAGEL,CTMAGELQ                                                 
         MVI   CTMAGLEN,CTMAGLNQ                                                
         MVC   CTMAGAID,FVIFLD                                                  
         GOTO1 AADDELS,CT5REC                                                   
*                                                                               
DATAVEX  EQU   *                                                                
*&&                                                                             
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
VALRECX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF ACCESS TYPE RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         MVC   SYSALPH,CT5KALPH                                                 
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ACCESS TYPE RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC SYSCTRYH                                                         
         XC    SYSUSNM,SYSUSNM     SET NAME FIELD TRANSMIT FLAGS                
         OI    SYSUSNMH+6,X'80'                                                 
         XC    SYSGRNM,SYSGRNM                                                  
         OI    SYSGRNMH+6,X'80'                                                 
         XC    SYSHONM,SYSHONM                                                  
         OI    SYSHONMH+6,X'80'                                                 
         XC    SYSRPIN,SYSRPIN                                                  
         OI    SYSRPINH+6,X'80'                                                 
*&&US                                                                           
         XC    SYSSPUN,SYSSPUN                                                  
         OI    SYSSPUNH+6,X'80'                                                 
*&&                                                                             
*&&UK                                                                           
         XC    SYSASAG,SYSASAG                                                  
         OI    SYSASAGH+6,X'80'                                                 
*&&                                                                             
*                                                                               
         LA    R3,CT5DATA          DISPLAY EACH ELEMENT OF RECORD               
DISP2    CLI   0(R3),0             E-O-R                                        
         BE    DISP6                                                            
         CLI   0(R3),CTDSCELQ      USER-ID                                      
         BE    DISPID                                                           
         CLI   0(R3),CTSYSELQ      SYSTEM                                       
         BE    DISPSS                                                           
         CLI   0(R3),CTAGDELQ      AGENCY GROUP DETAILS                         
         BE    DISPAD                                                           
         CLI   0(R3),CTRPIELQ      RFP PRINCIPLE USER ID ELEMENT                
         BE    DISPRP                                                           
*&&US                                                                           
         CLI   0(R3),CTSPUELQ      SECURITY PRINCIPLE USER ID ELEMENT           
         BE    DISPSP                                                           
*&&                                                                             
         CLI   0(R3),CTSEAELQ      SECURITY AGENCY ID                           
         BE    DISPSA                                                           
         CLI   0(R3),CTAADELQ      AGENCY ACCESS DETAILS                        
         BE    DISPDE                                                           
         CLI   0(R3),CTTOUELQ      TIME OUT ELEMENT                             
         BE    DISPTO                                                           
*&&UK                                                                           
         CLI   0(R3),CTAGLELQ      ASSOC AGENCY LIST                            
         BE    DISPAA                                                           
         CLI   0(R3),CTMAGELQ      UK MEDIA AGENCY                              
         BE    DISPMA                                                           
*&&                                                                             
*                                                                               
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
         B     EXIT                                                             
         EJECT                                                                  
         USING CTDSCD,R3                                                        
DISPID   GOTO1 GETID,CTDSC         DISPLAY PRINCIPAL ID                         
         MVC   SYSUSER,USERID                                                   
         MVC   SYSUSNM,USERNM                                                   
         B     DISP4                                                            
         SPACE 1                                                                
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
         BNO   *+12                                                             
         MVI   SYSDDSA,C'Y'                                                     
         B     DISPAD3                                                          
         TM    CTAGDDA,CTAGDDAN    NO DDS TERMINAL ACCESS                       
         BNO   *+12                                                             
         MVI   SYSDDSA,C'N'                                                     
         B     DISPAD3                                                          
         TM    CTAGDDA,CTAGDDAP    NO DDS AGENCY PASSWORD ACCESS                
         BNO   DISPAD3                                                          
         MVI   SYSDDSA,C'P'                                                     
DISPAD3  MVC   SYSTYPE,CTAGDTY     DISPLAY CLIENT TYPE                          
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
*                                  DISPLAY RFP PRINCIPLE USER ID                
         USING CTRPID,R3                                                        
DISPRP   GOTO1 GETID,CTRPINUM                                                   
         MVC   SYSRPI,USERID                                                    
         MVC   SYSRPIN,USERNM                                                   
         B     DISP4                                                            
*&&US                                                                           
*                                  DISPLAY SECURITY PRINCIPLE USER ID           
         USING CTSPUD,R3                                                        
DISPSP   GOTO1 GETID,CTSPUNUM                                                   
         MVC   SYSSPU,USERID                                                    
         MVC   SYSSPUN,USERNM                                                   
         B     DISP4                                                            
*&&                                                                             
         SPACE 1                                                                
         USING CTSYSD,R3                                                        
DISPSS   GOTO1 ADISSE,CTSYSSE  FORMAT SYSTEM BLOCK ENTRY                        
         OC    APPARM(4),APPARM  RETURN STRING IN APWORK                        
         BNZ   *+6             ELSE APPARM=0 NOT FOUND                          
         DC    H'00'                                                            
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
*&&                                                                             
         B     DISP4                                                            
         DROP  R1                                                               
         SPACE 1                                                                
         USING CTSEAD,R3                                                        
DISPSA   MVC   SYSSECA,CTSEAAID    SECURITY AGENCY ID                           
         B     DISP4                                                            
         DROP  R3                                                               
*                                                                               
         USING CTAADD,R3                                                        
*                                  AGENCY ACCESS DETAILS ELEMENT                
DISPDE   EQU   *                                                                
         EDIT  CTAADPTO,(3,SYSPTOU),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         EDIT  CTAADPRU,(3,SYSPRUS),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         EDIT  CTAADPML,(2,SYSPMIN),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         TM    CTAADFLG,CTAADPRQ                                                
         BZ    DIDE010                                                          
         MVI   SYSPIDR,C'Y'                                                     
         B     DIDEX                                                            
DIDE010  MVI   SYSPIDR,C'N'                                                     
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
         USING CTMAGD,R3                                                        
DISPMA   EQU   *                   UK MEDIA AGENCY                              
         MVC   SYSMAG,CTMAGAID                                                  
         B     DISP4                                                            
         DROP  R3                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN ACCESS TYPE RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CT5REC                                                   
         OI    CT5STAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED ACCESS TYPE RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CT5REC                                                   
         NI    CT5STAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS FOR ACCESS RECORDS            *         
***********************************************************************         
         SPACE 1                                                                
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
*                                  GET SYSTEM LIST INFO                         
         GOTO1 GETSE                                                            
         BNE   VALSELX                                                          
         MVC   SELSYS,APWORK                                                    
*                                                                               
VALSEL8  GOTO1 AFVAL,LSTSECAH      CLIENT TYPE                                  
         BNE   VALSELY                                                          
         MVC   SELSECA,FVIFLD                                                   
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST-SELECT RECORD FOR ACCESS RECORDS                      *         
***********************************************************************         
         SPACE 1                                                                
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
*                                                                               
GETSELE  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETSELA                                                          
*                                                                               
GETSELF  EQU   *                                                                
         OC    SELSECA,SELSECA     TEST SECURITY AGENCY SELECT                  
         BZ    *+12                                                             
         CLI   SECAFLAG,C'N'                                                    
         BE    GETSEL4                                                          
         OC    SELSYS,SELSYS       TEST SYSTEM # SELECT                         
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
GETSELK  DS    0H                  RECORD SELECTED                              
*                                                                               
GETSELY  MVC   APRECKEY(L'CT5KEY),CT5KEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE FOR ACCESS RECORDS                         *         
***********************************************************************         
         SPACE 1                                                                
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
         B     EXIT                                                             
         SPACE 2                                                                
         USING CTDSCD,R3                                                        
DSELID   GOTO1 GETID,CTDSC         DISPLAY PRINCIPAL ID                         
         MVC   LISTUSER,USERID                                                  
         B     DSEL4                                                            
         SPACE 1                                                                
         USING CTAGDD,R3                                                        
DSELAD   OC    CTAGDAG,CTAGDAG     DISPLAY AGENCY GROUP                         
         BZ    DSELAD1             IF PRESENT                                   
         GOTO1 GETID,CTAGDAG                                                    
         MVC   LISTGRUP,USERID                                                  
DSELAD1  OC    CTAGDHG,CTAGDHG     DISPLAY HOLDING GROUP                        
         BZ    DSELAD2             IF PRESENT                                   
         GOTO1 GETID,CTAGDHG                                                    
         MVC   LISTHOLD,USERID                                                  
DSELAD2  MVC   LISTTYPE,CTAGDTY    DISPLAY CLIENT TYPE                          
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
         SPACE 1                                                                
         USING CTSEAD,R3                                                        
DSELSA   MVC   LISTSECA,CTSEAAID   SECURITY AGENCY ID                           
         B     DSEL4                                                            
         SPACE 1                                                                
         USING CTSYSD,R3                                                        
DSELSS   GOTO1 ADISSYS,CTSYSNUM                                                 
         ICM   R8,15,APPARM        SAVE A(SELIST ENTRY)                         
         BNZ   *+6                                                              
         DC    H'00'                                                            
         USING SYSLSTD,R8                                                       
         MVC   0(L'SYSLSHRT,R6),SYSLSHRT                                        
         LA    R6,L'SYSLSHRT(R6)                                                
         GOTO1 CNTSE,CTSYSNUM                                                   
         CLI   APWORK,0                                                         
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   APWORK,1                                                         
         BE    DSELSS1                                                          
         GOTO1 ADISSE,CTSYSSE                                                   
         OC    APPARM(4),APPARM                                                 
         BNZ   *+6                                                              
         DC    H'00'                                                            
         LA    RF,APWORK+(L'SENAME-1)                                           
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVC   0(1,R6),0(RF)                                                    
         LA    R6,1(R6)                                                         
DSELSS1  CLI   SYSLIND1,X'40'                                                   
         BNE   DSELSS2                                                          
         MVI   0(R6),C'='                                                       
         GOTO1 VHEXOUT,APPARM,CTSYSAGB,1(R6),1,=C'TOG'                          
         LA    R6,3(R6)                                                         
DSELSS2  MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
         B     DSEL4                                                            
         DROP  R8                                                               
         DROP  R3                                                               
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN FOR ACCESS RECORDS        *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         MVC   LINEALPH,CT5KALPH                                                
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
*                                                                               
PRTREP4  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PRTREP3                                                          
*                                                                               
PRTREP6  BCTR  R6,0                REMOVE COMMA FROM SYSTEM FIELD               
         MVI   0(R6),C' '                                                       
         OC    SELSECA,SELSECA     SECURITY AGENCY FILTER TEST                  
         BZ    *+12                                                             
         CLI   SECAFLAG,C'N'                                                    
         BE    PRTREP1                                                          
         OC    SELSYS,SELSYS       SYSTEM FILTER TEST                           
         BZ    *+12                                                             
         CLI   APFLAG,0                                                         
         BE    PRTREP1                                                          
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
         B     PRTREP4                                                          
         SPACE 1                                                                
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
         USING CTSEAD,R3                                                        
PREPSA   MVI   SECAFLAG,C'Y'       PRINT SECURITY AGENCY                        
         OC    SELSECA,SELSECA                                                  
         BZ    PRTREP4                                                          
         CLC   SELSECA,CTSEAAID                                                 
         BNE   PRTREP1                                                          
         MVC   LINESECA,CTSEAAID                                                
         B     PRTREP4                                                          
         DROP  R1                                                               
         SPACE 1                                                                
         USING CTSYSD,R3                                                        
PREPSS   OC    SELSYS,SELSYS       SYSTEM FILTER                                
         BZ    PREPSS1                                                          
         CLC   SELSYS,CTSYSSE                                                   
         BNE   PREPSS1                                                          
         MVI   APFLAG,1            FLAG MATCHING SYSTEM FOUND                   
PREPSS1  GOTO1 ADISSE,CTSYSSE      FORMAT SYSTEM BLOCK ENTRY                    
*NOP*    OC    APPARM(4),APPARM    RETURN STRING IN APWORK                      
*NOP*    BNZ   *+6                 ELSE APPARM=0 NOT FOUND                      
*NOP*    DC    H'00'               DON'T PANIC                                  
         LA    RF,L'SENAME+4(R6)                                                
         LA    R1,L'REPPS+REPP1                                                 
         CLR   RF,R1                                                            
         BH    PRTREP4                                                          
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'ACCESS TYPE LIST'                                        
         SPEC  H2,57,C'----------------'                                        
         SPEC  M1,1,C'ID  CTY  PRINCIPAL   GROUP'                               
         SPEC  M2,1,C'--  ---  ID          ID   '                               
         SPEC  M1,34,C'HOLDING     CLIENT  SE SYSTEM SPECIFICATIONS'            
         SPEC  M2,34,C'ID          TYPE    AG ---------------------'            
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
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
LISTACCS DS    CL34                SYSTEM LIST VALUES                           
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
LINEALPH DS    CL2                 AGENCY ALPHA CODE                            
         DS    CL2                                                              
LINECTRY DS    CL3                 COUNTRY (ABREVIATED)                         
         DS    CL2                                                              
LINEUSER DS    CL10                PRINCIPAL ID                                 
         DS    CL2                                                              
LINEGRUP DS    CL10                AGENCY GROUP ID                              
         DS    CL2                                                              
LINEHOLD DS    CL10                HOLDING COMPANY ID                           
         DS    CL2                                                              
LINETYPE DS    CL1                 CLIENT TYPE                                  
         DS    CL7                                                              
LINESECA DS    CL2                 SECURITY AGENCY                              
         DS    CL1                                                              
LINEACCS DS    CL74                SYSTEM INFO                                  
         ORG   REPP2                                                            
         DS    CL45                                                             
LINEASAG DS    CL87                ASSOCIATED AGENCY LIST                       
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
SELKEY   DS    0XL9                SELECT KEY PARAMETERS                        
SELCTY   DS    XL1                 COUNTRY                                      
SELGID   DS    CL2                 GROUP ID                                     
SELHID   DS    CL2                 HOLDING ID                                   
SELTYP   DS    CL1                 CLIENT TYPE                                  
SELSYS   DS    CL1                 SYSTEM #                                     
SELSECA  DS    CL2                 SECURITY AGENCY                              
         ORG   SELKEY+L'SELKEY                                                  
VSCINKEY DS    V                   UNSCAN ROUTINE ADDRESS                       
CTRY     DS    XL1                 COUNTRY CODE                                 
CURR     DS    CL3                 CURRENCY                                     
HOLDID   DS    XL2                 HOLDING COMPANY ID                           
GRUPID   DS    XL2                 AGENCY GROUP ID                              
CLTYPE   DS    CL1                 CLIENT TYPE                                  
DDSACC   DS    XL1                 DDS ACCESS LEVEL FLAG                        
SYSCNT   DS    X                   SYSTEM # FIELD COUNT                         
FLDCNT   DS    X                   SUB-FIELD COUNT                              
SECAFLAG DS    CL1                 SECURITY AGENCY FILTER FLAG                  
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
LISTBUFF DS    CL256               SYSTEM INFO DISPLAY BUFFER                   
*                                                                               
OVSYS    DS    XL256                                                            
BLOCKI   DS    20CL32              SCAN BLOCK                                   
         ORG   BLOCKI                                                           
BLOCKO   DS    20CL20              UNSCAN BLOCK                                 
         ORG                                                                    
*                                                                               
LOCALX   EQU   *                   END OF LOCAL WORKING STORAGE                 
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076CTGEN18S  08/22/00'                                      
         END                                                                    
