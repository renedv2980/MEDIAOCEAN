*          DATA SET CTESS06    AT LEVEL 052 AS OF 05/16/16                      
*PHASE TA0E06A                                                                  
*INCLUDE TIMBER                                                                 
*INCLUDE NUMVAL                                                                 
*                                                                               
         TITLE 'CTESS06 - FILE MAINTENANCE - SERVER TRANSFER RECORDS'           
ESS06    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ES06**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GXTRD,R2            R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
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
* ROUTINE TO VALIDATE KEY OF EXTRACT SERVER TRANSFER CONTROL RECORD   *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         USING GXTRD,R2            R2=A(XSERVER RECORD KEY)                     
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSKRECQ     RECORD TYPE                                  
         MVI   FVMINL,1                                                         
         GOTO1 VALEID,XTREIDH      VALIDATE ESS SERVER ID                       
         BNE   VALKEYX                                                          
         MVC   GXSKEID,FVIFLD                                                   
*                                                                               
VKAGY    MVI   FVMINL,1            VALIDATE AGENCY FIELD                        
         GOTO1 AFVAL,XTRAGYH                                                    
         BNE   VALKEYX                                                          
         MVC   GXSKAGY,FVIFLD                                                   
*                                                                               
VKSYS    MVI   FVMINL,1            VALIDATE SYSTEM FIELD                        
         GOTO1 AFVAL,XTRSYSH                                                    
         BNE   VALKEYX                                                          
         USING SYSLSTD,RE                                                       
         L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VKSY010  CLI   SYSLNUM,0                                                        
         BE    VKSY012                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VKSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VKSY010                                                          
*                                                                               
VKSY012  LA    RE,SYSLEX           CHECK IN EXTENDED SYSTEM TABLE               
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VKSY014  CLI   SYSLNUM,0                                                        
         BE    ESYS                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VKSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VKSY014                                                          
*                                                                               
VKSY020  MVC   GXSKSYS,SYSLNUM     SET SYSTEM NUMBER FROM LIST                  
         MVC   XTRSYS(7),SYSLNAME  DISPLAY FULL SYSTEM NAME                     
         OI    XTRSYSH+6,X'80'                                                  
         DROP  RE                                                               
*                                                                               
VKSUB    EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 VALSUB,XTRSUBH                                                   
         BNE   EIIF                                                             
         MVC   GXSKSUB,APHALF                                                   
         MVC   XTRSUB,APWORK                                                    
*                                                                               
VK100    EQU   *                                                                
         MVC   AGYALPH,GXSKAGY                                                  
         MVC   SYSTEM,GXSKSYS                                                   
         MVC   SUBSYS,GXSKSUB                                                   
         BAS   RE,VALXAGY          VALIDATE XAGENCY RECORD                      
         BNE   VALKEYX                                                          
*                                                                               
         MVC   APRECKEY(GXKEYL),GXKEY                                           
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK010                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     NOT DEL THEREFORE NO REC                     
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VK010    LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET RECORD                                   
         TM    IOERR,IOERRS-IOEDEL DELETED IS ONLY SAFE ERR                     
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
*                                                                               
VALKEYY  EQU   *                                                                
         OI    XTREIDH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XTRAGYH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XTRSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XTRSUBH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A XTRAN RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   GXKEY,APRECKEY                                                   
         LA    R3,APELEM                                                        
         CLI   APACTN,ACTADD                                                    
         BNE   VRCHG                                                            
*                                                                               
VRADD    MVC   GXKEY,APRECKEY                                                   
         MVC   GXFLEN,=AL2(GXFIRST)                                             
         XC    GXFSTAT(GXFIRST-GXKEYL),GXFSTAT                                  
VRADDX   B     VRDATA                                                           
*                                                                               
VRCHG    EQU   *                                                                
         XC    GXFCTL,GXFCTL                                                    
         USING GXSTEL,R3                                                        
         LA    R3,APELEM           SAVE ANY EXISTING SYSTEM ELEMENT             
         MVI   GXSTEL,GXSTELQ                                                   
         MVI   GXSTELL,4                                                        
         MVC   GXSTAGY,GXSKAGY                                                  
         MVC   GXSTSYS,GXSKSYS                                                  
         MVC   GXSTSUB,GXSKSUB                                                  
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    VRCHGX                                                           
         MVC   LASTCRFN,GXSTCRFN   SAVE LAST CREATE FILE NUMBER                 
         MVC   LASTNOFN,GXSTNOFN   SAVE LAST NOTIFIED FILE NUMBER               
         MVC   LASTREFN,GXSTREFN   SAVE LAST RECEIVED FILE NUMBER               
         MVC   LASTCOFN,GXSTCOFN   SAVE LAST COMMIT FILE NUMBER                 
         GOTO1 ADELELS,GXTRD       DELETE EXISTING TRANSFER ELEMENT             
VRCHGX   B     VRDATA                                                           
         DROP  R3                                                               
         EJECT                                                                  
VRDATA   EQU   *                   PROCESS ELEMENT DATA                         
*                                                                               
         USING GXSTEL,R3                                                        
VRDAPP   LA    R3,APELEM           PROCESS SERVER TRANSFER ELEMENT              
         XC    APELEM,APELEM                                                    
         MVI   GXSTEL,GXSTELQ                                                   
         MVI   GXSTELL,GXSTELLQ                                                 
         MVC   GXSTAGY,GXSKAGY                                                  
         MVC   GXSTSYS,GXSKSYS                                                  
         MVC   GXSTSUB,GXSKSUB                                                  
         MVC   GXSTCRFN,LASTCRFN                                                
         MVC   GXSTNOFN,LASTNOFN                                                
         MVC   GXSTREFN,LASTREFN                                                
         MVC   GXSTCOFN,LASTCOFN                                                
         MVI   GXSTFCOD,C'E'                                                    
         MVI   GXSTMODE,C'N'                                                    
         MVI   GXSTLOAD,C'1'                                                    
*                                                                               
VRDA010  GOTO1 AFVAL,XTRFCODH                                                   
         BNE   VRDA030                                                          
         CLI   FVIFLD,C'N'                                                      
         BE    VRDA022                                                          
         CLI   FVIFLD,C'M'                                                      
         BE    VRDA022                                                          
         CLI   FVIFLD,C'W'                                                      
         BE    VRDA022                                                          
         CLI   FVIFLD,C'D'                                                      
         BE    VRDA022                                                          
         CLI   FVIFLD,C'E'                                                      
         BE    VRDA022                                                          
         B     EIIF                                                             
VRDA022  MVC   GXSTFCOD,FVIFLD                                                  
*                                                                               
VRDA030  GOTO1 AFVAL,XTRFTIMH                                                   
         BNE   VRDA040                                                          
         CLI   GXSTFCOD,C'E'                                                    
         BNE   EIIF                                                             
         LA    RF,L'XTRFTIM                                                     
         GOTO1 =V(TIMBER),APPARM,(X'C0',(RF)),(X'02',APHALF),XTRFTIM,  +        
               RR=APRELO                                                        
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         MVC   GXSTFTIM,APHALF                                                  
*                                                                               
VRDA040  GOTO1 AFVAL,XTRMODEH                                                   
         BNE   VRDA042                                                          
         MVC   GXSTMODE,FVIFLD                                                  
         CLI   GXSTMODE,GXSTMINQ                                                
         BNE   VRDA042                                                          
         OI    GXFCTL+1,GXCTINQ    SET INHIBIT MODE FLAG IN DIRECTORY           
*                                                                               
VRDA042  GOTO1 AFVAL,XTRLOADH                                                   
         BNE   VRDA050                                                          
         MVC   GXSTLOAD,FVIFLD                                                  
*                                                                               
VRDA050  CLI   APACTN,ACTADD                                                    
         BE    VRDA100                                                          
         CLI   OPTRST,C'Y'         RESET=Y OPTION                               
         BNE   VRDA060                                                          
         GOTO1 AFVAL,XTRCRFH                                                    
         BNE   VRDA060                                                          
         GOTO1 =V(NUMVAL),APPARM,FVIFLD,(X'02',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         STCM  R1,3,GXSTCRFN                                                    
*                                                                               
VRDA060  EQU   *                                                                
         CLI   OPTRST,C'Y'         RESET=Y OPTION                               
         BNE   VRDA070                                                          
         GOTO1 AFVAL,XTRNOFH                                                    
         BNE   VRDA070                                                          
         GOTO1 =V(NUMVAL),APPARM,FVIFLD,(X'02',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         STCM  R1,3,GXSTNOFN                                                    
*                                                                               
VRDA070  EQU   *                                                                
         CLI   OPTRST,C'Y'         RESET=Y OPTION                               
         BNE   VRDA080                                                          
         GOTO1 AFVAL,XTRREFH                                                    
         BNE   VRDA080                                                          
         GOTO1 =V(NUMVAL),APPARM,FVIFLD,(X'02',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         STCM  R1,3,GXSTREFN                                                    
*                                                                               
VRDA080  EQU   *                                                                
         CLI   OPTRST,C'Y'         RESET=Y OPTION                               
         BNE   VRDA100                                                          
         GOTO1 AFVAL,XTRCOFH                                                    
         BNE   VRDA100                                                          
         GOTO1 =V(NUMVAL),APPARM,FVIFLD,(X'02',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         STCM  R1,3,GXSTCOFN                                                    
*                                                                               
         B     VRDA100                                                          
*                                                                               
VRDA100  EQU   *                                                                
         GOTO1 AADDELS,GXTRD       REPLACE UPDATED TRANSFER ELEMENT             
         B     VRD200                                                           
         DROP  R3                                                               
*                                                                               
VRD200   EQU   *                                                                
         EJECT                                                                  
*                                  UPDATE RECORD                                
VRUPD    GOTO1 ASETACT,GXTRD       DEFINE ACTIVITY ELEMENT                      
*                                                                               
         MVC   CTRLSAV,GXFCTL                                                   
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   APWORK(L'GXSKEID),GXSKEID                                        
         CLI   APACTN,ACTADD       UPDATE GENDIR ON WRITE ONLY                  
         BE    VRUPDADD                                                         
         LA    R2,IOKEY                                                         
         MVC   GXDCTL,CTRLSAV                                                   
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         USING GXTRD,R2                                                         
         XC    GXKEY,GXKEY         CHANGE AGENCY PASSIVE POINTER                
         MVI   GXKREC,GXSARECQ                                                  
         MVC   GXSAAGY,AGYALPH                                                  
         MVC   GXSASYS,SYSTEM                                                   
         MVC   GXSASUB,SUBSYS                                                   
         MVC   GXSAEID,APWORK                                                   
         LA    R1,IOREAD+IOGENDIR                                               
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    GXDSTAT(GXDDA-GXDSTAT),GXDSTAT                                   
         MVC   GXDCTL,CTRLSAV                                                   
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VALRECOK                                                         
*                                                                               
VRUPDADD EQU   *                   ADD AGENCY PASSIVE POINTER                   
         LA    R2,IOKEY                                                         
         USING GXTRD,R2                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSARECQ                                                  
         MVC   GXSAAGY,AGYALPH                                                  
         MVC   GXSASYS,SYSTEM                                                   
         MVC   GXSASUB,SUBSYS                                                   
         MVC   GXSAEID,APWORK                                                   
*                                                                               
         MVC   GXDDA,IODA                                                       
         XC    GXDSTAT(GXDDA-GXDSTAT),GXDSTAT                                   
         MVC   GXDCTL,CTRLSAV                                                   
*                                  ADD PASSIVE POINTER RECORD                   
         GOTO1 AIO,IOADD+IOGENDIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALRECOK MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF XSERVER RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         MVC   XTREID,GXSKEID                                                   
         MVC   XTRAGY,GXSKAGY                                                   
         GOTO1 ADISSYS,GXSKSYS                                                  
         MVC   XTRSYS,APWORK                                                    
         GOTO1 DISSUB,GXSKSUB                                                   
         MVC   XTRSUB,APWORK                                                    
         OI    XTREIDH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XTRAGYH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XTRSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XTRSUBH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
DISKEYX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY XSERVER RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC XTRFCODH                                                         
*                                                                               
         USING GXSTEL,R3                                                        
         LA    R3,APELEM                                                        
         MVI   GXSTEL,GXSTELQ                                                   
         MVI   GXSTELL,4                                                        
         MVC   GXSTAGY,GXSKAGY                                                  
         MVC   GXSTSYS,GXSKSYS                                                  
         MVC   GXSTSUB,GXSKSUB                                                  
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DREC100                                                          
         MVC   XTRFCOD,GXSTFCOD                                                 
         CLI   GXSTFCOD,C'E'                                                    
         BNE   DREC010                                                          
         B     DREC010                                                          
         LA    RF,L'XTRFTIM                                                     
         GOTO1 =V(TIMBER),APPARM,(X'40',(RF)),(X'02',GXSTFTIM),        +        
               (4,XTRFTIM),RR=APRELO                                            
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
DREC010  MVC   XTRMODE,GXSTMODE                                                 
         MVC   XTRLOAD,GXSTLOAD                                                 
*                                                                               
         XC    XTRCRF,XTRCRF                                                    
         SR    RF,RF                                                            
         ICM   RF,3,GXSTCRFN                                                    
         EDIT  (RF),(8,XTRCRF),ZERO=NOBLANK,ALIGN=LEFT                          
         OI    XTRCRFH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XTRCRFH+(FVATRB-FVIHDR),FVAPROT                                  
         CLI   OPTRST,C'Y'         RESET=Y OPTION - UNPROTECT THIS              
         BNE   *+8                                                              
         NI    XTRCRFH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
*                                                                               
         XC    XTRREF,XTRREF                                                    
         SR    RF,RF                                                            
         ICM   RF,3,GXSTREFN                                                    
         EDIT  (RF),(8,XTRREF),ZERO=NOBLANK,ALIGN=LEFT                          
         OI    XTRREFH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XTRREFH+(FVATRB-FVIHDR),FVAPROT                                  
         CLI   OPTRST,C'Y'         RESET=Y OPTION - UNPROTECT THIS              
         BNE   *+8                                                              
         NI    XTRREFH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
*                                                                               
         XC    XTRNOF,XTRNOF                                                    
         SR    RF,RF                                                            
         ICM   RF,3,GXSTNOFN                                                    
         EDIT  (RF),(8,XTRNOF),ZERO=NOBLANK,ALIGN=LEFT                          
         OI    XTRNOFH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XTRNOFH+(FVATRB-FVIHDR),FVAPROT                                  
         CLI   OPTRST,C'Y'         RESET=Y OPTION - UNPROTECT THIS              
         BNE   *+8                                                              
         NI    XTRNOFH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
*                                                                               
         XC    XTRCOF,XTRCOF                                                    
         SR    RF,RF                                                            
         ICM   RF,3,GXSTCOFN                                                    
         EDIT  (RF),(8,XTRCOF),ZERO=NOBLANK,ALIGN=LEFT                          
         OI    XTRCOFH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XTRCOFH+(FVATRB-FVIHDR),FVAPROT                                  
         CLI   OPTRST,C'Y'         RESET=Y OPTION - UNPROTECT THIS              
         BNE   *+8                                                              
         NI    XTRCOFH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         DROP  R3                                                               
*                                                                               
DREC100  EQU   *                                                                
*                                                                               
         GOTO1 ADISACT,GXTRD       DISPLAY ACTIVITY DATE                        
*                                                                               
DISRECX  EQU   *                   SAVE LAST DISPLAYED SYSTEM                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN XSERVER RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
DELREC   EQU   *                                                                
         MVC   ESSID,GXSKEID                                                    
         MVC   AGYALPH,GXSKAGY                                                  
         MVC   SYSTEM,GXSKSYS                                                   
         MVC   SUBSYS,GXSKSUB                                                   
         MVC   IOKEYSV,IOKEY                                                    
         LA    R2,IOKEY            CHECK NO XAPPLIC RECORDS SET UP              
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSARECQ                                                  
         MVC   GXSAAGY,AGYALPH                                                  
         MVC   GXSASYS,SYSTEM                                                   
         MVC   GXSASUB,SUBSYS                                                   
         MVC   GXSAEID,ESSID                                                    
         MVI   GXSAAPID+L'GXSAAPID-1,X'01'                                      
         GOTO1 AIO,IOGENDIR+IOHIGH+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GXSAAGY,AGYALPH                                                  
         BNE   DELR010                                                          
         CLC   GXSASYS,SYSTEM                                                   
         BNE   DELR010                                                          
         CLC   GXSASUB,SUBSYS                                                   
         BNE   DELR010                                                          
         CLC   GXSAEID,ESSID                                                    
         BNE   DELR010                                                          
         B     ECDX                                                             
*                                                                               
DELR010  MVC   IOKEY,IOKEYSV                                                    
         LA    R2,IOKEY                                                         
         OI    GXDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GXTRD                                                    
         OI    GXFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  DEL AGENCY PASSIVE POINTER                   
         MVC   APWORK(L'GXSKEID),GXSKEID                                        
         LA    R2,IOKEY                                                         
         USING GXTRD,R2                                                         
         XC    GXKEY,GXKEY         BUILD FILE RECORD KEY                        
         MVI   GXKREC,GXSARECQ                                                  
         MVC   GXSAAGY,AGYALPH                                                  
         MVC   GXSASYS,SYSTEM                                                   
         MVC   GXSASUB,SUBSYS                                                   
         MVC   GXSAEID,APWORK                                                   
         GOTO1 AIO,IOREAD+IOGENDIR                                              
         BNE   DELR020                                                          
         OI    GXDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DELRECX                                                          
*                                                                               
DELR020  MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    IOERR,IOERR                                                      
*                                                                               
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED XSERVER RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
         NI    GXDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GXTRD                                                    
         NI    GXFSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENFIL                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  RES AGENCY PASSIVE POINTER ON ADD            
         MVC   APWORK(L'GXSKEID),GXSKEID                                        
         LA    R2,IOKEY                                                         
         USING GXTRD,R2                                                         
         XC    GXKEY,GXKEY         BUILD FILE RECORD KEY                        
         MVI   GXKREC,GXSARECQ                                                  
         MVC   GXSAAGY,AGYALPH                                                  
         MVC   GXSASYS,SYSTEM                                                   
         MVC   GXSASUB,SUBSYS                                                   
         MVC   GXSAEID,APWORK                                                   
         GOTO1 AIO,IORDD+IOGENDIR                                               
         BNL   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL                                                     
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    GXDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         XC    GXKEY,GXKEY                                                      
         MVI   GXKMAJ,X'FF'        FLAG FOR FIRST PASS                          
         MVI   GXKREC,GXSKRECQ                                                  
         XC    SELKEY,SELKEY                                                    
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VSELEID  EQU   *                                                                
         GOTO1 AFVAL,LSTEIDH                                                    
         BNE   VSEIDX                                                           
         MVC   SELEID,FVIFLD                                                    
         MVC   GXSKEID,SELEID                                                   
VSEIDX   EQU   *                                                                
*                                                                               
VSELFRQ  EQU   *                                                                
         GOTO1 AFVAL,LSTFREQH                                                   
         BNE   VSFRQX                                                           
         MVC   SELFREQ,FVIFLD                                                   
VSFRQX   EQU   *                                                                
*                                                                               
VSELMOD  EQU   *                                                                
         GOTO1 AFVAL,LSTMODEH                                                   
         BNE   VSMODX                                                           
         MVC   SELMODE,FVIFLD                                                   
VSMODX   EQU   *                                                                
*                                                                               
VSELAGY  EQU   *                                                                
         GOTO1 AFVAL,LSTAGYH                                                    
         BNE   VSAGYX                                                           
         GOTO1 VALAGY,LSTAGYH                                                   
         BNE   EIIF                                                             
         MVC   SELAGY,FVIFLD                                                    
         MVC   GXSKAGY,SELAGY                                                   
VSAGYX   EQU   *                                                                
*                                                                               
VSELSYS  EQU   *                                                                
         GOTO1 AFVAL,LSTSYSH                                                    
         BNE   VSSYSX                                                           
         USING SYSLSTD,RE                                                       
         L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VSSY010  CLI   SYSLNUM,0                                                        
         BE    VSSY012                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VSSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VSSY010                                                          
*                                                                               
VSSY012  LA    RE,SYSLEX           CHECK IN TABLE EXTENDED SYSTEMS              
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VSSY014  CLI   SYSLNUM,0                                                        
         BE    ESYS                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VSSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VSSY014                                                          
*                                                                               
VSSY020  MVC   SELSYS,SYSLNUM      SET SYSTEM NUMBER FROM LIST                  
         MVC   GXSKSYS,SELSYS                                                   
         MVC   LSTSYS(7),SYSLNAME  DISPLAY FULL SYSTEM NAME                     
         OI    LSTSYSH+6,X'80'                                                  
VSSYSX   EQU   *                                                                
*                                                                               
VSELSUB  EQU   *                                                                
         GOTO1 AFVAL,LSTSUBH                                                    
         BNE   VSSUBX                                                           
         GOTO1 VALSUB,LSTSUBH                                                   
         BNE   EIIF                                                             
         MVC   SELSUB,APHALF                                                    
VSSUBX   EQU   *                                                                
*                                                                               
VSELDIF  EQU   *                                                                
         GOTO1 AFVAL,LSTDIFH                                                    
         BNE   VSDIFX                                                           
         CLI   LSTDIF,C'-'                                                      
         BNE   *+14                SPECIAL "-" OPTION                           
         MVC   SELDIFF(1),LSTDIF                                                
         B     VSDIFX                                                           
         TM    FVIIND,FVINUM                                                    
         BZ    EIIF                MUST BE A POSITIVE NUMBER                    
         MVC   SELDIFF,SCFULL                                                   
VSDIFX   EQU   *                                                                
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
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
GETSEL   LA    R2,IOKEY                                                         
         MVC   GXKEY,APRECKEY                                                   
         CLI   GXKMAJ,X'FF'        TEST FIRST TIME FLAG                         
         BNE   GETSEL2                                                          
         MVI   GXKMAJ,0                                                         
         B     GETSEL6             READ HIGH                                    
GETSEL2  TM    APINDS,2            TEST SEQUENCE BROKEN                         
         BZ    GETSEL4                                                          
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    GETSEL8                                                          
         B     GETSELN                                                          
GETSEL4  TM    APINDS,1            TEST READ OR READ HIGH                       
         BNZ   GETSEL8                                                          
GETSEL6  LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOGENDIR+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         LA    R2,IOKEY                                                         
         CLI   GXKREC,GXSKRECQ     CHECK STILL XTRANS RECORD                    
         BNE   GETSELN                                                          
         CLI   GXSKAGY,0           CHECK STILL XTRANS RECORD                    
         BE    GETSEL8                                                          
         OC    GXSKAPID,GXSKAPID                                                
         BNZ   GETSEL8                                                          
         SPACE 1                                                                
*                                                                               
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
*                                                                               
GSEID    EQU   *                                                                
         OC    SELEID,SELEID                                                    
         BZ    GSEIDX                                                           
         CLC   SELEID,GXSKEID                                                   
         BNE   GETSEL8                                                          
GSEIDX   EQU   *                                                                
*                                                                               
GSAGY    EQU   *                                                                
         OC    SELAGY,SELAGY                                                    
         BZ    GSAGYX                                                           
         CLC   SELAGY,GXSKAGY                                                   
         BNE   GETSEL8                                                          
GSAGYX   EQU   *                                                                
*                                                                               
GSSYS    EQU   *                                                                
         OC    SELSYS,SELSYS                                                    
         BZ    GSSYSX                                                           
         CLC   SELSYS,GXSKSYS                                                   
         BNE   GETSEL8                                                          
GSSYSX   EQU   *                                                                
*                                                                               
GSSUB    EQU   *                                                                
         OC    SELSUB,SELSUB                                                    
         BZ    GSSUBX                                                           
         CLC   SELSUB,GXSKSUB                                                   
         BNE   GETSEL8                                                          
GSSUBX   EQU   *                                                                
*                                                                               
GSELEM   EQU   *                   GET ELEMENT DATA                             
         LA    R3,GXFIRST(R2)                                                   
         SR    RF,RF                                                            
         USING GXSTEL,R3                                                        
GSEM010  CLI   GXSTEL,0                                                         
         BE    GSEMEX                                                           
         CLI   GXSTEL,GXSTELQ                                                   
         BE    GSMODE                                                           
GSEM020  IC    RF,GXSTELL                                                       
         AR    R3,RF                                                            
         B     GSEM010                                                          
*                                                                               
GSMODE   EQU   *                                                                
         OC    SELMODE,SELMODE                                                  
         BZ    GSMODX                                                           
         CLC   SELMODE,GXSTMODE                                                 
         BNE   GETSEL8                                                          
GSMODX   EQU   *                                                                
*                                                                               
GSFREQ   EQU   *                                                                
         OC    SELFREQ,SELFREQ                                                  
         BZ    GSFRQX                                                           
         CLC   SELFREQ,GXSTFCOD                                                 
         BNE   GETSEL8                                                          
GSFRQX   EQU   *                                                                
*                                                                               
GSDIF    EQU   *                                                                
         OC    SELDIFF,SELDIFF                                                  
         BZ    GSDIFX                                                           
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,3,GXSTCRFN                                                    
         ICM   RF,3,GXSTCOFN                                                    
*                                                                               
         CLI   SELDIFF,C'-'                                                     
         BNE   GSDIF10                                                          
         SR    RE,RF               COMMIT# - CREATE#                            
         BNM   GETSEL8             NOT NEGATIVE, EXIT                           
         B     GSDIFX                                                           
*                                                                               
GSDIF10  SR    RE,RF               COMMIT# - CREATE#                            
         ICM   R1,15,SELDIFF                                                    
         CR    RE,R1                                                            
         BL    GETSEL8             < DIFF#, EXIT                                
GSDIFX   EQU   *                                                                
*                                                                               
GSEMEX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
GETSELY  EQU   *                                                                
         LA    R2,IOKEY                                                         
         MVC   APRECKEY(L'GXKEY),GXKEY                                          
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
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                  GET DATA FROM RECORD KEY                     
         MVC   LISTEID,GXSKEID                                                  
         MVC   LISTAGY,GXSKAGY                                                  
         GOTO1 ADISSYS,GXSKSYS                                                  
         MVC   LISTSYS,APWORK                                                   
         GOTO1 DISSUB,GXSKSUB                                                   
         MVC   LISTSUB,APWORK                                                   
*                                  GET DATA FROM ELEMENTS                       
         LA    R3,GXFIRST(R2)                                                   
DSLP1    CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),GXSTELQ                                                    
         BE    DSLAPP                                                           
*                                                                               
DSLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSLP1                                                            
*                                                                               
         USING GXSTEL,R3                                                        
DSLAPP   EQU   *                                                                
         MVC   LISTFCOD,GXSTFCOD                                                
         MVC   LISTMODE,GXSTMODE                                                
         SR    RF,RF                                                            
         ICM   RF,3,GXSTCRFN                                                    
         EDIT  (RF),(8,LISTCRFN),ZERO=NOBLANK,ALIGN=LEFT                        
         SR    RF,RF                                                            
         ICM   RF,3,GXSTNOFN                                                    
         EDIT  (RF),(8,LISTNOFN),ZERO=NOBLANK,ALIGN=LEFT                        
         SR    RF,RF                                                            
         ICM   RF,3,GXSTREFN                                                    
         EDIT  (RF),(8,LISTREFN),ZERO=NOBLANK,ALIGN=LEFT                        
         SR    RF,RF                                                            
         ICM   RF,3,GXSTCOFN                                                    
         EDIT  (RF),(8,LISTCOFN),ZERO=NOBLANK,ALIGN=LEFT                        
         B     DSLP1A                                                           
         DROP  R3                                                               
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
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         XC    SELKEY,SELKEY       SELECTION CRITERION                          
         XC    APRECKEY,APRECKEY                                                
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
*                                                                               
VRQ50    LA    R2,APRECKEY         SET UP INITIAL KEY                           
         MVI   GXKREC,GXAKRECQ                                                  
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
* ROUTINE TO GENERATE MESSAGE REPORT                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
         L     R2,AIOAREA1                                                      
         MVC   IOKEY,APRECKEY      SET INITIAL KEY VALUE                        
*                                                                               
PR010    LA    R1,IOHI+IOGENDIR+IO1                                             
         B     *+8                                                              
PR020    LA    R1,IOSQ+IOGENDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         CLI   GXKREC,GXAKRECQ     TEST STILL A MESSAGE RECORD                  
         BNE   PRTREPX                                                          
*                                                                               
PR040    GOTO1 VREPORT,REPD                                                     
         B     PR020                                                            
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GENERAL FIELD XMT IF CHANGED                                       *          
* R1=A(TWAHDR)                                                       *          
* APWORK MUST CONTAIN THE NEW TEXT                                   *          
**********************************************************************          
         SPACE 1                                                                
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)     KNOCK OFF HEADER EXTENSION                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         COMPARE FIELD CONTENTS                       
         BER   RE                                                               
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW FIELD                            
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
         EJECT                                                                  
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
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ESS ID                                          *         
* NTRY: R1=A(FIELD HEADER)                                            *         
* EXIT: CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
VALEID   NTR1                                                                   
         MVC   IOKEYSV(L'IOKEY),IOKEY                                           
         LR    R3,R1                                                            
         MVI   FVMAXL,L'GXSKEID                                                 
         GOTO1 AFVAL                                                            
         BNE   VEIDNO                                                           
         CLC   FVIFLD(3),=CL3'ESS'                                              
         BE    VEID010                                                          
         GOTO1 =V(NUMVAL),APPARM,0(R3),(X'01',0),RR=APRELO                      
         CLI   0(R1),0                                                          
         BNE   VEIDNO                                                           
         L     R1,4(R1)                                                         
         B     VEID020                                                          
*                                                                               
VEID010  GOTO1 =V(NUMVAL),APPARM,FVIFLD+3,(X'00',5),RR=APRELO                   
         CLI   0(R1),0                                                          
         BNE   VEIDNO                                                           
         L     R1,4(R1)                                                         
*                                                                               
VEID020  LA    R2,IOKEY                                                         
         USING GESSD,R2            R2=A(ESSID RECORD KEY)                       
         XC    GSKEY,GSKEY                                                      
         MVI   GSKREC,GSKRECQ      RECORD TYPE                                  
         STCM  R1,3,GEKNUM                                                      
         GOTO1 AIO,IORD+IOGENDIR+IO2                                            
         BNL   *+14                TEST AIO RETURN CONDITION                    
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     VEIDNO                                                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VEIDNO                                                           
         B     VEIDOK                                                           
*                                                                               
VEIDNO   B     NO                                                               
VEIDOK   EQU   *                                                                
         MVC   IOKEY(L'IOKEY),IOKEYSV                                           
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AGENCY ALPHA ID                                 *         
* NTRY: R1=A(FIELD HEADER)                                            *         
* EXIT: CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
VALAGY   NTR1                                                                   
         MVI   FVMAXL,L'CT5KALPH                                                
         GOTO1 AFVAL                                                            
         BNE   VAGYNO                                                           
         MVC   IOKEYSV(L'IOKEY),IOKEY                                           
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    CT5KEY,CT5KEY       BUILD KEY                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNL   *+14                TEST AIO RETURN CONDITION                    
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     VAGYNO                                                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VAGYNO                                                           
*                                                                               
         B     VAGYOK                                                           
*                                                                               
VAGYNO   B     NO                                                               
VAGYOK   EQU   *                                                                
         MVC   IOKEY(L'IOKEY),IOKEYSV                                           
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE EXTRACT SUB SYSTEM                              *         
* NTRY: R1=A(FIELD HEADER)                                            *         
* EXIT: APHALF=SUB SYSTEM NUMBER, APWORK=SUB SYSTEM NAME              *         
*       CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
VALSUB   NTR1                                                                   
         MVI   FVMAXL,L'SUBLNAME                                                
         GOTO1 AFVAL                                                            
         BNE   VSUBNO                                                           
         USING SUBLSTD,RE                                                       
         LA    RE,SUBLST           CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VSUB010  CLI   SUBLNUM,0                                                        
         BE    VSUBNO                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SUBLNAME                                               
         BE    VSUB020                                                          
         LA    RE,SUBLLEN(RE)                                                   
         B     VSUB010                                                          
*                                                                               
VSUB020  MVC   APHALF(L'SUBLNUM),SUBLNUM   SET SYSTEM NUMBER FROM LIST          
         MVC   APWORK(L'SUBLNAME),SUBLNAME  GET FULL SYSTEM NAME                
         B     VSUBOK                                                           
*                                                                               
VSUBNO   B     NO                                                               
VSUBOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY EXTRACT SUB SYSTEM                               *         
* NTRY: R1=A(SUB SYSTEM NUMBER)                                       *         
* EXIT: APWORK=SUB SYSTEM NAME                                        *         
*       CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
DISSUB   NTR1                                                                   
         USING SUBLSTD,RE                                                       
         MVC   APWORK(7),=CL7'UNKNOWN'                                          
         LA    RE,SUBLST           CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
*                                                                               
DSUB010  CLI   SUBLNUM,0                                                        
         BE    DSUBNO                                                           
         CLC   0(1,R1),SUBLNUM                                                  
         BE    DSUB020                                                          
         LA    RE,SUBLLEN(RE)                                                   
         B     DSUB010                                                          
*                                                                               
DSUB020  EQU   *                           SET SYSTEM NUMBER FROM LIST          
         MVC   APWORK(L'SUBLNAME),SUBLNAME  GET FULL SYSTEM NAME                
         B     DSUBOK                                                           
*                                                                               
DSUBNO   B     NO                                                               
DSUBOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE XAGENCY RECORD DATA                             *         
* EXIT: CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
VALXAGY  NTR1                                                                   
         MVC   IOKEYSV(L'IOKEY),IOKEY                                           
         LA    R2,IOKEY                                                         
         USING GXTRD,R2            R2=A(ESS SERVER RECORD)                      
         XC    GXKEY,GXKEY         BUILD KEY                                    
         MVI   GXKREC,GXAKRECQ                                                  
         MVC   GXAKAGY,AGYALPH                                                  
         MVC   GXAKSYS,SYSTEM                                                   
         MVC   GXAKSUB,SUBSYS                                                   
         GOTO1 AIO,IORD+IOGENDIR+IO2                                            
         BNL   *+14                TEST AIO RETURN CONDITION                    
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     VXAGNO                                                           
         BE    VXAGOK                                                           
         MVC   FVMSGNO,=AL2(CE#MSXAG)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,XTRAGYH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         B     VXAGNO                                                           
*                                                                               
VXAGNO   B     NO                                                               
VXAGOK   EQU   *                                                                
         MVC   IOKEY(L'IOKEY),IOKEYSV                                           
         B     YES                                                              
         DROP  R2                                                               
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
ECDX     MVC   FVMSGNO,=AL2(CE#CDXAP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,ESSACTH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         B     NO                  CANNOT DELETE - EXISTING XAPPL RECS          
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
SPACES   DC    CL80' '                                                          
         SPACE 1                                                                
         EJECT                                                                  
REPDESCL DC    C'MESSAGE LIST'                                                  
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'MESSAGE RECORD LIST'                                     
         SPEC  H2,57,C'-------------------'                                     
         SPEC  M1,24,C'MESSAGE'                                                 
         SPEC  M2,1,C'SYSTEM  LANGUAGE      REFERENCE MESSAGE TEXT'             
         SPEC  M3,1,C'------  --------      --------- ------------'             
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
* DXSYSLEX                                                                      
       ++INCLUDE DXSYSLEX                                                       
         EJECT                                                                  
* DXSUBLST                                                                      
       ++INCLUDE DXSUBLST                                                       
         EJECT                                                                  
* CTESSWRK                                                                      
       ++INCLUDE CTESSWRK                                                       
         SPACE 1                                                                
* DXSUBLSTD                                                                     
       ++INCLUDE DXSUBLSTD                                                      
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSF9D                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSD9D                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSB9D                                                       
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 SAVE CURRENT DISPLAYED SYSTEM                
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTEID  DS    CL8                                                              
         DS    CL1                                                              
LISTAGY  DS    CL2                                                              
         DS    CL1                                                              
LISTSYS  DS    CL7                                                              
         DS    CL1                                                              
LISTSUB  DS    CL7                                                              
         DS    CL1                                                              
LISTFCOD DS    CL1                                                              
         DS    CL2                                                              
LISTMODE DS    CL1                                                              
         DS    CL2                                                              
LISTCRFN DS    CL8                                                              
         DS    CL1                                                              
LISTNOFN DS    CL8                                                              
         DS    CL1                                                              
LISTREFN DS    CL8                                                              
         DS    CL1                                                              
LISTCOFN DS    CL8                                                              
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
PRTLIN   DS    0CL(L'REPP1)                                                     
PRTSYS   DS    CL7                                                              
         DS    CL1                                                              
PRTLANG  DS    CL13                                                             
         DS    CL1                                                              
PRTREF   DS    CL8                                                              
         DS    CL2                                                              
PRTMSG   DS    CL(L'REPP1-(PRTMSG-REPP1))                                       
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
IOKEYSV  DS    XL(L'IOKEY)                                                      
ASE      DS    A                                                                
*                                                                               
LASTCRFN DS    XL2                 LAST CREATE FILE NUMBER                      
LASTNOFN DS    XL2                 LAST NOTIFIED FILE NUMBER                    
LASTREFN DS    XL2                 LAST RECEIVED FILE NUMBER                    
LASTCOFN DS    XL2                 LAST COMMIT FILE NUMBER                      
AGYALPH  DS    CL2                 AGENCY ALPHA ID                              
SUBSYS   DS    CL1                 SUB SYSTEM CODE                              
SYSTEM   DS    CL1                 SYSTEM NUMBER                                
ESSID    DS    CL8                                                              
SYCNT    DS    CL1                                                              
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
CTRLSAV  DS    XL(L'GXFCTL)                                                     
*                                                                               
SELKEY   DS    0XL32                                                            
SELEID   DS    CL8                                                              
SELAGY   DS    CL2                                                              
SELSYS   DS    CL1                                                              
SELSUB   DS    CL1                                                              
SELFREQ  DS    CL1                                                              
SELMODE  DS    CL1                                                              
SELDIFF  DS    XL4                                                              
         ORG   SELKEY+L'SELKEY                                                  
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052CTESS06   05/16/16'                                      
         END                                                                    
