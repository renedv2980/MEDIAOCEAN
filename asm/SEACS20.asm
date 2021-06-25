*          DATA SET SEACS20    AT LEVEL 003 AS OF 06/23/16                      
*PHASE TA0D20A                                                                  
*INCLUDE DATTIM                                                                 
         TITLE 'SEACS20 - PASSWORD RESET'                                       
ACS20    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AS20**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
*                                                                               
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   AGENCYID,OPTAGY                                                  
         B     *+10                                                             
         MVC   AGENCYID,CUAALF                                                  
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
         B     VALKEY              01 - APMVALK                                 
         B     EXIT                02 - APMVALR                                 
         B     EXIT                03 - APMDISK                                 
         B     EXIT                04 - APMDISR                                 
         B     EXIT                05 - APMDELR                                 
         B     EXIT                06 - APMRESR                                 
         B     EXIT                07 - APMVALP                                 
         B     EXIT                08 - APMGETS                                 
         B     EXIT                09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     EXIT                11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     EXIT                14 - APMLSCR                                 
         B     EXIT                15 - APMVALQ                                 
         B     EXIT                16 - APMREPP                                 
         B     SETTWA              17 - APMSETT                                 
         B     EXIT                18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE INITIALISATION OF TWA                             *         
***********************************************************************         
SETTWA   EQU   *                                                                
*                                  CHECK PASSWORD FIELD SECURITY                
         OC    ACASEC,ACASEC                                                    
         BZ    SETTWAX                                                          
*                                                                               
         MVI   APBYTE,FLDPWD                                                    
         LA    RF,APBYTE                                                        
         GOTO1 VSECRET,SCPARM,('SECPFLDP',ACASEC),(RF)                          
         BNE   STWA110                                                          
         B     SETTWAX             FULL READ/WRITE ACCESS                       
*                                  NO ACCESS                                    
STWA110  OI    PWDPWD1H+(FVATRB-FVIHDR),X'FF'                                   
         OI    PWDPWHH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PWDPWD2H+(FVATRB-FVIHDR),X'FF'                                   
         OI    PWDPNHH+(FVATRB-FVIHDR),X'FF'                                    
         B     SETTWAX                                                          
*                                                                               
SETTWAX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF PERSON AND ASSOCIATED PASSWORD RECORD    *         
***********************************************************************         
VALKEY   EQU   *                                                                
*                                  GET AGENCY ACCESS DETAILS                    
         MVI   PIDREQD,C'N'                                                     
         MVI   PWDMINLN,3                                                       
         GOTO1 AGETAAD,AGENCYID                                                 
         TM    APWORK,CTAADPRQ     PERSONAL ID REQUIRED WITH PASSWORD?          
         BZ    *+8                                                              
         MVI   PIDREQD,C'Y'                                                     
         MVC   PWDTOUT,APWORK+1    PASSWORD TIMEOUT DAYS                        
         MVC   PWDMINLN,APWORK+2   PASSWORD MINIMUM LENGTH                      
         MVC   PWDREUSE,APWORK+3   PASSWORD REUSE MAXIMUM NUMBER                
*                                                                               
         MVI   FVMINL,3                                                         
         GOTO1 AFVAL,PWDPIDH       READ PERSONAL-ID                             
         BNE   EXIT                                                             
         OC    PWDPID,SPACES                                                    
*                                                                               
*                                  GET CURRENT DATE/TIME INTEGER                
         GOTO1 =V(DATTIM),APPARM,(X'01',DATETIME),0,RR=APRELO                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATETIMC,DATETIME                                                
         XC    DATETIMC,FFILL                                                   
*                                  GET TODAYS DATE                              
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   TODAYC,FFILL          BUILD CURRENT EFFECTIVE                    
         XC    TODAYC,TODAY          DATE IN PERSON ID RECORD KEY               
*                                                                               
*                                  READ PERSON RECORD                           
         LA    R2,IOKEY            BUILD PERSON KEY                             
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,PWDPID                                                   
         MVC   SAPEDEF,TODAYC                                                   
         MVC   APRECKEY(L'SAPEKEY),SAPEKEY                                      
*                                                                               
         GOTO1 AIO,IOHI+IOLOCK+IOCONFIL+IO1                                     
         BL    SAEIIO                                                           
*                                                                               
         L     R2,AIOAREA1                                                      
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),APRECKEY                                
         BNE   SAERNF              RECORD FOUND                                 
*                                                                               
         TM    PWDPIDH+(FVIIND-FVIHDR),FVITHIS                                  
         BNO   VK030                                                            
         BRAS  RE,DISREC                                                        
*                                                                               
*CLEAR AND UNPROTECT PASSWORD FIELDS                                            
         OC    ACASEC,ACASEC                                                    
         BZ    VK025                                                            
         MVI   APBYTE,FLDPWD                                                    
         LA    RF,APBYTE                                                        
         GOTO1 VSECRET,SCPARM,('SECPFLDP',ACASEC),(RF)                          
         BNE   VK030                                                            
*                                                                               
VK025    MVC   PWDPWD1,SPACES                                                   
         MVC   PWDPWD2,SPACES                                                   
         NI    PWDPWD1H+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    PWDPWD2H+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         OI    PWDPWD1H+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PWDPWD2H+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,PWDPWD1H                                                      
         ST    R1,APCURSOR         SET CURSOR FOR DATA INPUT                    
*                                                                               
         XC    APPARM(8*L'APPARM),APPARM   SET UP GETTXT BLOCK                  
         LA    R1,APPARM                                                        
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO,=AL2(CI#RDNEC)      REC DISPLAY, ENTER CHG               
         MVI   GTMTYP,GTMINF                                                    
         MVI   GTMAXL,60                                                        
         DROP  R1                                                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     VKX                                                              
*                                                                               
VK030    EQU   *                                                                
         TM    PWDPWD1H+(FVATRB-FVIHDR),FVAPROT  PWD FIELD PROTED?              
         BNO   VK050                             NO                             
*                                                                               
         XC    APPARM(8*L'APPARM),APPARM   SET UP GETTXT BLOCK                  
         LA    R1,APPARM                                                        
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO,=AL2(CI#RECSD)           RECORD DISPLAYED                
         MVI   GTMTYP,GTMINF                                                    
         MVI   GTMAXL,60                                                        
         DROP  R1                                                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     VKX                              EXIT                            
*                                                                               
VK050    EQU   *                                                                
         BRAS  RE,GETPWD                                                        
         BRAS  RE,VALPWD                                                        
         BNE   VKX                                                              
*                                                                               
*CLEAR AND PROTECT PASSWORD FIELDS AFTER RESET                                  
         MVC   PWDPWD1,SPACES                                                   
         MVC   PWDPWD2,SPACES                                                   
         OI    PWDPWD1H+(FVATRB-FVIHDR),FVAPROT                                 
         OI    PWDPWD2H+(FVATRB-FVIHDR),FVAPROT                                 
         OI    PWDPWD1H+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PWDPWD2H+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,PWDPIDH                                                       
         ST    R1,APCURSOR         SET CURSOR FOR DATA INPUT                    
*                                                                               
         XC    APPARM(8*L'APPARM),APPARM   SET UP GETTXT BLOCK                  
         LA    R1,APPARM                                                        
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO,=AL2(CI#PWDRT)                                           
         MVI   GTMTYP,GTMINF                                                    
         MVI   GTMAXL,60                                                        
         DROP  R1                                                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
*                                                                               
VKX      J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PERSON INFO                                      *         
***********************************************************************         
GETPWD   NTR1  ,                                                                
         L     R2,AIOAREA1         DISPLAY PERSON RECORD DATA                   
         USING SAPEREC,R2                                                       
         LA    R3,SAPEDATA                                                      
         SR    R0,R0                                                            
*                                                                               
GPWD010  CLI   0(R3),0             TEST END-OF-RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),SAPWDELQ      TEST PASSWORD ELEMENT                        
         BE    GPWD180                                                          
         ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     GPWD010                                                          
*                                                                               
         USING SAPWDD,R3                                                        
GPWD180  MVC   PWDNUM,SAPWDNUM     SAVE PWD#                                    
         MVC   PWDCODE,SAPWDCOD    SAVE PWD CODE                                
*                                                                               
GPWDX    J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PERSON INFO                                      *         
***********************************************************************         
DISREC   NTR1  ,                                                                
         TWAXC PWDFNAMH                                                         
*                                                                               
         MVC   PWDFNAM,SPACES                                                   
         MVC   PWDMNAM,SPACES                                                   
         MVC   PWDLNAM,SPACES                                                   
         MVC   PWDOFF,SPACES                                                    
         MVC   PWDDID,SPACES                                                    
*                                                                               
         L     R2,AIOAREA1         DISPLAY PERSON RECORD DATA                   
         USING SAPEREC,R2                                                       
         LA    R3,SAPEDATA                                                      
         SR    R0,R0                                                            
*                                                                               
*                                  GET DATA FROM ELEMENTS                       
DREC010  CLI   0(R3),0             TEST END-OF-RECORD                           
         BE    DREC200                                                          
         CLI   0(R3),SANAMELQ      TEST NAME ELEMENT                            
         BE    DREC040                                                          
         CLI   0(R3),SAPERELQ      TEST PERSONNEL ELEMENT                       
         BE    DREC070                                                          
*                                                                               
DREC020  ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     DREC010                                                          
*                                                                               
         USING SANAMD,R3                                                        
DREC040  LA    RE,SANAMELN         PROCESS NAME ELEMENT                         
         SR    R1,R1                                                            
         TM    SANAMIND,SANAMIFN                                                
         BZ    DREC050                                                          
         IC    R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PWDFNAM(0),1(RE)                                                 
         LA    RE,2(R1,RE)                                                      
DREC050  TM    SANAMIND,SANAMIMN                                                
         BZ    DREC060                                                          
         IC    R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PWDMNAM(0),1(RE)                                                 
         LA    RE,2(R1,RE)                                                      
DREC060  TM    SANAMIND,SANAMILN                                                
         BZ    DREC020                                                          
         IC    R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PWDLNAM(0),1(RE)                                                 
         LA    RE,2(R1,RE)                                                      
         B     DREC020                                                          
*                                  PROCESS PERSONNEL DETAILS ELEMENT            
         USING SAPERD,R3                                                        
DREC070  OC    SAPEROFF,SAPEROFF   OFFICE ID                                    
         BZ    DREC080                                                          
         MVC   PWDOFF,SAPEROFF                                                  
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY                                                  
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,PWDOFF                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   DREC020                                                          
         L     R1,AIOAREA2                                                      
         GOTO1 AGETONAM                                                         
         MVC   PWDOFFN,APWORK                                                   
*                                                                               
DREC080  L     R2,AIOAREA1                                                      
         OC    SAPERDID,SAPERDID   DEPARTMENT ID                                
         BZ    DREC020                                                          
         MVC   PWDDID,SAPERDID                                                  
         LA    R2,IOKEY                                                         
         USING SADPREC,R2                                                       
         XC    SADPKEY,SADPKEY                                                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,PWDOFF                                                   
         MVC   SADPDID,PWDDID                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   DREC020                                                          
         L     R1,AIOAREA2                                                      
         GOTO1 AGETDNAM                                                         
         MVC   PWDDIDN,APWORK                                                   
         B     DREC020                                                          
*                                                                               
DREC200  EQU   *                                                                
*                                                                               
         OI    PWDPIDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         OI    PWDFNAMH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PWDMNAMH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PWDLNAMH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         OI    PWDOFFH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    PWDDIDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         OI    PWDOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PWDDIDNH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
DRX      J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE PASSWORD                                        *         
***********************************************************************         
VALPWD   NTR1  ,                                                                
*                                                                               
         MVC   FVMINL,PWDMINLN                                                  
         GOTO1 AFVAL,PWDPWD1H      VALIDATE NEW PASSWORD                        
         BNE   SAEFTS              TOO SHORT                                    
         MVC   PWDPWD1L,FVILEN                                                  
*                                                                               
         OC    PWDPWD1,SPACES                                                   
         CLC   PWDPWD1,=CL10'DDS'  CANNOT SET PWD TO 'DDS'                      
         BNE   VP010                                                            
         MVC   FVMSGNO,=AL2(CE#PWDRS)                                           
         B     VRXNO                                                            
*                                                                               
VP010    CLC   PWDPWD1,=CL10'***'  CANNOT SET PWD TO '***'                      
         BNE   VP020                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VRXNO                                                            
*                                                                               
VP020    EQU   *                                                                
         CLC   PWDPWD2,SPACES                                                   
         BH    VP030                                                            
         LA    R1,PWDPWD2H                                                      
         ST    R1,APCURSOR         SET CURSOR FOR DATA INPUT                    
         B     SAEMIF                                                           
VP030    OC    PWDPWD2,SPACES                                                   
*                                                                               
         CLC   PWDPWD1,PWDPWD2     PASSWORDS THE SAME?                          
         BE    VP040                                                            
         LA    R1,PWDPWD1H                                                      
         ST    R1,APCURSOR         SET CURSOR FOR DATA INPUT                    
         MVC   FVMSGNO,=AL2(CE#PWDNS)                                           
         B     VRXNO                                                            
*                                                                               
VP040    EQU   *                                                                
         L     RF,ASYS                                                          
         L     RF,VSSB-SYSFACD(RF) GET SSB                                      
         ICM   RF,15,SSBXLAT-SSBD(RF)                                           
         AHI   RF,-8               A(MIXED CASES/SYMBOLS) TRT TABLE             
         ICM   RE,15,0(RF)                                                      
         BNZ   *+12                AVALIABLE - USE IT, OTHERWISE..              
         AHI   RF,-4               A(VALUAZ09) UPPER A-Z,0-9 TRT TABLE          
         L     RE,0(RF)                                                         
*                                                                               
         LLC   R1,PWDPWD1L                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         TRT   PWDPWD1(0),0(RE)    WARNING: TRT DESTROYS R1 & R2                
         BNZ   SAEIIF              NO SPECIAL CHAR ALLOWED FOR NEW PWD          
*                                                                               
         CLI   PIDREQD,C'Y'        TEST PPS?                                    
         BE    VP100               YES                                          
*                                                                               
VP080    EQU   *                                                                
         BRAS  RE,CHGPWD1          CHANGE PASSWORD FOR NON-PPS                  
         BNE   NO                                                               
         B     VP200                                                            
*                                                                               
VP100    EQU   *                                                                
         BRAS  RE,CHGPWD2          CHANGE PASSWORD FOR PPS                      
         BNE   NO                                                               
         B     VP200                                                            
*                                                                               
VP200    EQU   *                                                                
         BRAS  RE,CHGPID           CHANGE PERSON RECORDS                        
*                                                                               
VRX      B     YES                                                              
*                                                                               
VRXNO    MVC   PWDPWD1,SPACES      CLEAR PASSWORD FIELDS                        
         MVC   PWDPWD2,SPACES                                                   
         OI    PWDPWD1H+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PWDPWD2H+(FVOIND-FVIHDR),FVOXMT                                  
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* CHANGE PASSWORD FOR NON-PPS PERSON                                  *         
***********************************************************************         
CHGPWD1  NTR1  ,                                                                
*                                                                               
         LA    R2,IOKEY            ENSURE PASSWORD CODE NOT USE YET             
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KCODE,PWDPWD1                                                 
         GOTO1 AIO,IORDD+IOCONFIL+IO2                                           
         BL    SAEIIO                                                           
         BE    SAERAE                                                           
         TM    IOERR,IOEEOF                                                     
         BNZ   CP1_010             EOF                                          
         TM    IOERR,IOERNF                                                     
         BNZ   CP1_010             CONTINUE WITH EXISTING NUMBER                
         TM    IOERR,IOEDEL                                                     
         BZ    SAEIIO                                                           
*                                                                               
CP1_010  EQU   *                   CHANGE PWD# REC                              
         LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KNUM,PWDNUM                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA2                                                      
*                                                                               
         LA    R3,APELEM           DEL AND THEN READD PASS PT ELEM              
         USING SAPASD,R3                                                        
         MVI   SAPASEL,SAPASELQ                                                 
         MVI   SAPASLEN,0                                                       
         GOTO1 ADELELS,SA0REC                                                   
         MVI   SAPASLEN,X'0C'                                                   
         MVC   SAPASDTA(L'SA0KCODE),PWDPWD1                                     
         GOTO1 AADDELS,SA0REC      READD THE ELEMENT                            
*                                                                               
         GOTO1 ASETACT,SA0REC                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
*                                                                               
*                                  PWD CODE REC UPDATE                          
         L     R2,AIOAREA2                                                      
         LA    R3,APELEM           DEL AND THEN READD PASS PT ELEM              
         USING SAPASD,R3                                                        
         MVI   SAPASEL,SAPASELQ                                                 
         MVI   SAPASLEN,0                                                       
         GOTO1 ADELELS,SA0REC                                                   
         MVI   SAPASLEN,X'04'                                                   
         MVC   SAPASDTA(L'SA0KNUM),PWDNUM                                       
         GOTO1 AADDELS,SA0REC      READD THE ELEMENT                            
*                                                                               
         LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KCODE,PWDPWD1                                                 
         GOTO1 AIO,IORDUP+IOCONFIL+IO3                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         TM    IOERR,IOEDEL                                                     
         BNZ   CP1_040                                                          
         TM    IOERR,IOERNF                                                     
         BNZ   CP1_050                                                          
CP1_040  LA    R1,IOWRITE+IOCONFIL+IO2                                          
         B     *+8                   ELSE OVERWRITE WITH CHANGE                 
CP1_050  LA    R1,IOADD+IOCONFIL+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CP1_X    J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHANGE PASSWORD RECORD FOR PPS PERSON                               *         
***********************************************************************         
CHGPWD2  NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KNUM,PWDNUM                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA2                                                      
*                                                                               
         LA    R3,APELEM           DEL AND THEN READD PASS PT ELEM              
         USING SAPASD,R3                                                        
         MVI   SAPASEL,SAPASELQ                                                 
         MVI   SAPASLEN,0                                                       
         GOTO1 ADELELS,SA0REC                                                   
         MVI   SAPASLEN,X'0C'                                                   
         MVC   SAPASDTA(L'SA0KCODE),PWDPWD1                                     
         GOTO1 AADDELS,SA0REC      READD THE ELEMENT                            
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SAPWHD,R3                                                        
         MVI   SAPWHEL,SAPWHELQ    BUILD PASSWORD HISTORY ELEMENT               
         MVC   SAPWHDTE,FFILL                                                   
         TIME  BIN                                                              
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  R0,7,SAPWHTME       TIME                                         
         OI    SAPWHFLG,SAPWHEXP   FORCE IMMEDIATE EXPIRE                       
*                                                                               
         LLC   R1,PWDPWD1L                                                      
         LR    RE,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAPWHPWD(0),PWDPWD1 PASSWORD CODE                                
*                                                                               
         LHI   R1,SAPWHLNQ                                                      
         AR    R1,RE                                                            
         STC   R1,SAPWHLN          ELEMENT LENGTH                               
*                                                                               
         GOTO1 VHELLO,APPARM,(C'P',CTFILE),('SAPWHELQ',SA0REC),APELEM,0         
         CLI   APPARM+12,0                                                      
         JNE   *+2                                                              
         L     R3,APPARM+16                                                     
         MVC   SAPWHDTE,TODAY      CORRECT TO TODAY'S DATE                      
         DROP  R3                                                               
*                                                                               
*                                                                               
         GOTO1 ASETACT,SA0REC                                                   
*                                                                               
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CP2_X    J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CHANGE CURRENT AND FUTURE PERSON RECORD'S PASSWORD                            
***********************************************************************         
CHGPID   NTR1                                                                   
         CLI   PIDREQD,C'Y'        PPS?                                         
         BNE   CPIDX               NO - EXIT                                    
*                                  READ PERSON RECORD                           
CPID010  LA    R2,IOKEY            BUILD PERSON KEY                             
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,PWDPID                                                   
         MVC   SAPEDEF,TODAYC                                                   
         MVC   APRECKEY(L'SAPEKEY),SAPEKEY                                      
*                                                                               
         GOTO1 AIO,IOHIUPD+IOCONFIL+IO2                                         
         BNL   *+6                                                              
         DC    H'00'                                                            
         TM    IOERR,IOEDEL                                                     
         BNZ   *+12                                                             
         TM    IOERR,IOEEOF                                                     
         BNZ   CPIDX                                                            
*                                                                               
         L     R2,AIOAREA2                                                      
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),APRECKEY                                
         BNE   CPIDX               NOT THE SAME PERSON - EXIT                   
*                                                                               
         LA    R3,SAPEDATA                                                      
CPID020  CLI   0(R3),0                                                          
         BE    CPID040                                                          
         CLI   0(R3),SALLOELQ      LAST LOGON/SECURITY VIOLATION ELEM           
         BE    CPID030                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CPID020                                                          
*                                                                               
         USING SALLOD,R3                                                        
CPID030  CLI   SALLOLEN,SALLOLNQ   IS THIS THE NEW ELEMENT LENGTH?              
         BL    CPID040             NO: NO VIOLATION FIELDS ON ELEM              
         MVI   SALLOFLG,0          RESET SECURITY VIOLATION FLAG                
         XC    SALLOTME,SALLOTME   RESET SECURITY VIOLATION TIME                
         MVI   SALLOCNT,0          RESET SECURITY VIOLATION COUNT               
         DROP  R3                                                               
*                                                                               
CPID040  GOTO1 ASETACT,SAPEREC     MARK ACTIVITY                                
         LA    R3,APELEM                                                        
         USING SAPWDD,R3                                                        
         MVI   SAPWDEL,SAPWDELQ    BUILD PASSWORD ELEMENT                       
         MVI   SAPWDLN,0                                                        
         GOTO1 ADELELS,SAPEREC                                                  
         MVI   SAPWDLN,SAPWDLNQ                                                 
         MVC   SAPWDNUM,PWDNUM     OLD PID#                                     
         MVC   SAPWDCOD,PWDPWD1    NEW PASSWORD                                 
         GOTO1 AADDELS,SAPEREC                                                  
         DROP  R3                                                               
*                                                                               
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLC   SAPEDEF,TODAYC      FUTURE EFF DATE?                             
         BNL   CPIDX               NO - EXIT                                    
         SR    RE,RE                                                            
         ICM   RE,3,SAPEDEF                                                     
         AHI   RE,1                                                             
         STCM  RE,3,SAPEDEF                                                     
         B     CPID010                                                          
*                                                                               
CPIDX    J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
*                                  ERROR EXITS                                  
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
SAEFTL   MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
SAEFNN   MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
SAEFTS   MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
SAEFNH   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    132C' '                                                          
FFILL    DC    32X'FF'                                                          
*                                                                               
*                                                                               
* SEACSDICT                                                                     
       ++INCLUDE SEACSDICT                                                      
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSE3D                                                       
         ORG                                                                    
         EJECT                                                                  
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
PIDREQD  DS    XL1                 PERSONID REQUIRED WITH PASSWD=Y              
PWDTOUT  DS    XL1                 PASSWORD TIMEOUT IN DAYS                     
PWDMINLN DS    XL1                 PASSWORD MINIMUM LENGTH                      
PWDREUSE DS    XL1                 PASSWORD REUSE MAXIMUM NUMBER                
*                                                                               
TODAY    DS    XL2                 TODAYS DATE COMPRESSED BINARY                
TODAYC   DS    XL2                 TODAYS DATE COMPLEMENT COMPRESSED            
DATETIME DS    XL4                 DATE/TIME VALUE FROM DATTIM                  
DATETIMC DS    XL4                 DATE/TIME VALUE 1'S COMPLEMENTED             
*                                                                               
PWDNUM   DS    XL2                 PASSWORD NUMBER SAVE                         
PWDCODE  DS    CL10                PASSWORD CODE SAVE                           
PWDPWD1L DS    X                   PASSWORD CODE INPUT LENGTH                   
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
SAVPARM  DS    8F                                                               
VSCINKEY DS    A                                                                
FIELD    DS    F                                                                
*                                                                               
*                                                                               
ASYSEL   DS    A                                                                
ASEPGMS  DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
RETURN   DS    A                                                                
WORK     DS    CL80                                                             
*                                                                               
*                                                                               
GETSEQF  DS    XL1                 APINDS SAVE FLAG FOR GETSEL                  
*                                                                               
SAPEKSAV DS    XL(L'SAPEKEY)       PERSONAL-ID RECORD KEY SAVE                  
*                                                                               
FLDCNT   DS    XL1                 FIELD COUNT                                  
COUNT    DS    XL1                 COUNTER                                      
PROGRAM  DS    CL1                 PROGRAM CODE                                 
PGNAME   DS    CL8                 PROGRAM NAME SAVE                            
SYSTEM   DS    CL1                 SYSTEM SE NUMBER                             
SYSALL   DS    CL(L'SASYSALL)      DEFAULT ALL PROGRAM ACCESS CODE              
*                                                                               
LOCALX   EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SEACS20   06/23/16'                                      
         END                                                                    
