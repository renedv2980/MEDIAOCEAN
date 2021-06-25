*          DATA SET SEACS03    AT LEVEL 012 AS OF 01/25/18                      
*PHASE TA0D03A                                                                  
ACS03    TITLE '- SECURITY ACCESS - ACTION RECORDS'                             
ACS03    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS3**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING SAACREC,R2          R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     DELREC              05 - APMDELR                                 
         B     RESREC              06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     XIT                 10 - APMVALS                                 
         B     FSTLST              11 - APMFLST                                 
         B     XIT                 12 - APMPROC                                 
         B     XIT                 13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     XIT                 15 - APMVALQ                                 
         B     XIT                 16 - APMREPP                                 
         B     SETTWA              17 - APMSETT                                 
         B     XIT                 18 - APMPUTK                                 
         B     XIT                 19 - APMNEWK                                 
         B     XIT                 20 - APMFRP                                  
         B     XIT                 21 - APMDISS2                                
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALIZE TWA                                           *         
***********************************************************************         
         SPACE 1                                                                
SETTWA   XC    SAVOVER(SAVCLRL),SAVOVER                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF ACTION RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   GOTO1 AVALOVPG,PARM,DSPSYSH,(1,DSPPGMH),DSPPGMAH                       
         BNE   VALKEYX                                                          
         MVC   SAVOVPG,APHALF                                                   
         MVC   SAVFMT,APELEM+(SAPGMAIF-SAPGMD)                                  
         MVC   DSPACTW,ACTCODE                                                  
         TM    DICINDS,DICIACTO    TEST ACTION WORD OVERRIDEN                   
         BZ    *+10                                                             
         MVC   DSPACTW,AC@ACT                                                   
         OI    DSPACTWH+FHOID,FHOITR                                            
*                                                                               
         MVCDD DSPFMTD,CT#HEX      DISPLAY FORMAT OF ACTION CODE                
         CLI   SAVFMT,SAPGMIFA                                                  
         BNE   *+10                                                             
         MVCDD DSPFMTD,CT#ALPH                                                  
         CLI   SAVFMT,SAPGMIFN                                                  
         BNE   *+10                                                             
         MVCDD DSPFMTD,CT#NMRCL                                                 
         OI    DSPFMTDH+FHOID,FHOITR                                            
*                                                                               
         CLI   DSPACT,FF           TEST LAST ACTION WAS DISKEY                  
         BNE   VKEY2                                                            
         MVC   SAVACT,DSPACT+1                                                  
         GOTO1 ADISCODE,PARM,(SAVFMT,SAVACT)                                    
         MVC   DSPACT,APWORK                                                    
         B     VKEY4                                                            
*                                                                               
VKEY2    GOTO1 AVALCODE,PARM,(SAVFMT,DSPACTH)                                   
         BNE   VALKEYX             VALIDATE ACTION CODE                         
         MVC   SAVACT,APBYTE                                                    
VKEY4    OI    DSPACTH+FHOID,FHOITR                                             
*                                                                               
         LA    R2,IOKEY                                                         
         USING SAACREC,R2          INITIALIZE ACTION RECORD KEY                 
         XC    SAACKEY,SAACKEY                                                  
         MVI   SAACTYP,SAACTYPQ                                                 
         MVI   SAACSUB,SAACSUBQ                                                 
         MVC   SAACOVPG,SAVOVPG                                                 
         MVC   SAACACT,SAVACT                                                   
         MVC   APRECKEY(L'SAACKEY),SAACKEY                                      
*                                                                               
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)       IF NOT DISPLAY READ FOR UPDATE               
         GOTO1 AIO                                                              
         BL    VALKEYX             IF I/O ERROR EXIT                            
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         BE    VALKEYY                                                          
*                                                                               
         MVI   APINDS,APIOKADD                                                  
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN ACTION RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1         SET UP ELEMENTLESS RECORD                    
         MVC   SAACKEY,APRECKEY                                                 
         LA    R0,SAACDATA-SAACREC+1                                            
         STCM  R0,3,SAACLEN                                                     
         MVI   SAACSTAT,0                                                       
         MVI   SAACDATA,0                                                       
*                                                                               
         LA    R3,APELEM                                                        
         USING SAACTD,R3           R3=A(ACTION ELEMENT)                         
         XC    SAACTD(SAACTLNQ),SAACTD                                          
         MVI   SAACTEL,SAACTELQ                                                 
         MVI   SAACTLN,SAACTLNQ                                                 
*                                                                               
         MVI   FVMINL,1            VALIDATE WORD DICTIONARY NUMBER              
         GOTO1 AVALDIC,PARM,(SAACOVS,L'DSPWRDA),(C'U',DSPWRDNH)                 
         BNE   VALRECX                                                          
         MVC   SAACTWRD,APHALF                                                  
         MVC   DSPWRDA,APWORK                                                   
         OI    DSPWRDAH+FHOID,FHOITR                                            
*                                                                               
         IC    RF,SAACOVS          VALIDATE TEXT DESCRIPTION NUMBER             
         LA    RF,X'80'(RF)                                                     
         GOTO1 AVALTXT,PARM,((RF),DSPDSCNH),SAACTWRD                            
         BNE   VALRECX                                                          
         MVC   SAACTDSC,APHALF                                                  
         MVC   DSPDSCA,APWORK                                                   
         OI    DSPDSCAH+FHOID,FHOITR                                            
*                                                                               
         GOTO1 AADDELS,SAACREC     ADD ACTION ELEMENT                           
         DROP  R3                                                               
*                                                                               
         GOTO1 ASETACT,SAACREC     DEFINE ACTIVITY ELEMENT                      
*                                                                               
         CLI   APACTN,ACTADD       TEST ADD RECORD                              
         BNE   VREC10                                                           
         BAS   RE,UPDTPGM          UPDATE PROGRAM RECORD                        
         BNE   VALRECX                                                          
         GOTO1 AIO,IOADD+IOCONFIL+IO1                                           
         BE    VALRECY                                                          
         DC    H'0'                                                             
*                                                                               
VREC10   GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALRECY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF ACTION RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         GOTO1 ADISSYS,SAACOVS                                                  
         MVC   DSPSYS,APWORK       SYSTEM NAME                                  
*                                                                               
         GOTO1 ADISPGM,PARM,(SAACOVS,SAACPGM)                                   
         MVC   DSPPGM,APWORK       PROGRAM NAME                                 
*                                                                               
         MVI   DSPACT,FF           SAVE ACTION CODE                             
         MVC   DSPACT+1(1),SAACACT                                              
*                                                                               
DISKEYX  B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY ACTION RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         GOTO1 FINDEL,PARM,('SAACTELQ',SAACREC)                                 
         USING SAACTD,R3           R3=A(ACTION ELEMENT)                         
*                                                                               
         MVC   APBYTE,SAACOVS                                                   
         OI    APBYTE,X'80'                                                     
         GOTO1 ADISDIC,PARM,(APBYTE,L'DSPWRDA),(C'U',SAACTWRD)                  
         MVC   DSPWRDN,APWORK+64                                                
         OI    DSPWRDNH+FHOID,FHOITR                                            
         MVC   DSPWRDA,APWORK                                                   
         OI    DSPWRDAH+FHOID,FHOITR                                            
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),SAACTDSC                                               
         EDIT  (B4,FULL),(5,DSPDSCN),ALIGN=LEFT                                 
         OI    DSPDSCNH+FHOID,FHOITR                                            
         IC    RF,SAACOVS                                                       
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),SAACTDSC),SAACTWRD                            
         MVC   DSPDSCA,APWORK                                                   
         OI    DSPDSCAH+FHOID,FHOITR                                            
*                                                                               
         GOTO1 ADISACT,SAACREC     DISPLAY ACTIVITY DATE                        
*                                                                               
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO   SET SCREEN TO MODIFIED             
*                                                                               
DISRECX  B     XIT                                                              
         DROP R3                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN ACTION RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
DELREC   BAS   RE,UPDTPGM          UPDATE PROGRAM RECORD                        
         BNE   DELRECX                                                          
*                                                                               
         L     R2,AIOAREA1         R2=A(ACTION RECORD)                          
*                                                                               
         GOTO1 ASETACT,SAACREC                                                  
         OI    SAACSTAT,X'80'      SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     XIT                                                              
         SPACE 5                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED ACTION RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
RESREC   BAS   RE,UPDTPGM                                                       
         BNE   RESRECX                                                          
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SAACREC                                                  
         NI    SAACSTAT,FF-X'80'   UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE PROGRAM RECORD & VERIFY ACTION CAN BE ADDED/DEL'D *         
*                                                                     *         
* EXIT: CC=EQUAL IF SUCCESSFULLY UPDATED                              *         
***********************************************************************         
         SPACE 1                                                                
UPDTPGM  NTR1  ,                                                                
         LA    R4,IOKEY            READ PROGRAM RECORD                          
         USING SAPGREC,R4                                                       
         XC    SAPGKEY,SAPGKEY                                                  
         MVI   SAPGTYP,SAPGTYPQ                                                 
         MVI   SAPGSUB,SAPGSUBQ                                                 
         MVC   SAPGOVPG,SAVOVPG                                                 
         L     R4,AIOAREA2                                                      
         GOTO1 AIO,IOCONFIL+IORDUP+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 FINDEL,PARM,('SAPGMELQ',SAPGREC)                                 
         USING SAPGMD,R3           R3=A(PROGRAM ELEMENT)                        
         CLI   SAPGMLN,SAPGMLNQ    TEST HAVE LONGER ELEMENT                     
         BE    UPGM01                                                           
         XC    APELEM,APELEM       NO THEN ADD LONGER ELEMENT                   
         IC    RF,SAPGMLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APELEM(0),SAPGMD                                                 
         MVI   APELEM+(SAPGMLN-SAPGMD),SAPGMLNQ                                 
         GOTO1 VHELLO,PARM,(C'D',CTFILE),('SAPGMELQ',SAPGREC),0                 
         GOTO1 (RF),(R1),(C'P',CTFILE),SAPGREC,APELEM                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,16(R1)                                                        
*                                                                               
UPGM01   CLI   APACTN,ACTDEL       TEST ACTION BEING DELETED                    
         BNE   UPGM10                                                           
         LA    R0,L'SAPGMACT                                                    
         LA    RF,SAPGMACT                                                      
         LR    R1,RF                                                            
UPGM02   CLC   SAVACT,0(RF)        FIND CURRENT ACTION CODE IN LIST             
         BE    UPGM04                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,UPGM02                                                        
         DC    H'0'                CODE MUST BE IN LIST                         
*                                                                               
UPGM04   MVI   0(RF),0             ZERO OUT CODE                                
         SR    RF,R1                                                            
         STC   RF,ASEQNO           SAVE ACTION SEQUENCE NUMBER                  
*                                                                               
         LA    R8,IOKEY            READ ALL RECORD RECORDS FOR PROGRAM          
         USING SARCREC,R8                                                       
         XC    SARCKEY,SARCKEY                                                  
         MVI   SARCTYP,SARCTYPQ                                                 
         MVI   SARCSUB,SARCSUBQ                                                 
         MVC   SARCOVPG,SAVOVPG                                                 
         L     R8,AIOAREA3                                                      
*                                                                               
         LA    R1,IOCONFIL+IOHI+IO3                                             
         B     *+8                                                              
UPGM06   LA    R1,IOCONFIL+IOSQ+IO3                                             
         GOTO1 AIO                                                              
         BNE   UPGM08                                                           
         CLC   SARCKEY(SARCRCD-SARCKEY),IOKEYSAV                                
         BNE   UPGM08              TEST RECORD RECORD FOR PROGRAM               
*                                                                               
         GOTO1 FINDEL,PARM,('SAMIXELQ',SARCREC)  TEST ACTION VALID FOR          
         GOTO1 ABLDBITT,PARM,(R3),BITTABLE         ACTION                       
         GOTO1 TESTBIT,ASEQNO                                                   
         BZ     UPGM06                                                          
         MVC    FVMSGNO,=AL2(FVFXDEL)  IF SO CAN'T DELETE                       
         B      UPDTPGMN                                                        
         DROP   R8                                                              
*                                                                               
UPGM08   B     UPGM20                                                           
*                                                                               
UPGM10   LA    RF,SAPGMACT         FIND FIRST ZERO IN LIST                      
         LA    R0,L'SAPGMACT                                                    
UPGM12   CLI   0(RF),0             FIND FIRST ZERO IN LIST                      
         BE    UPGM14                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,UPGM12                                                        
         MVC   FVMSGNO,=AL2(CE#MADFP)  ACTION LIST IS FULL                      
         B     UPDTPGMN                                                         
*                                                                               
UPGM14   MVC   0(1,RF),SAVACT      PUT ACTION CODE IN LIST                      
*                                                                               
UPGM20   GOTO1 AIO,IOCONFIL+IOPUT+IO2  WRITE PROGRAM RECORD                     
UPDTPGMY BE    XIT                 XIT WITH CC EQUAL                            
         DC    H'0'                                                             
*                                                                               
UPDTPGMN LTR   RB,RB               SET CC NOT EQUAL                             
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY         INITIALIZE RECORD KEY                        
         XC    SAACKEY,SAACKEY                                                  
         MVI   SAACTYP,SAACTYPQ                                                 
         MVI   SAACSUB,SAACSUBQ                                                 
*                                                                               
         CLI   LSTSYSH+FHILD,0     TEST SYSTEM NAME ENTERED                     
         BNE   VSEL2                                                            
         CLI   LSTPGMH+FHILD,0     PROGRAM NAME SHOULD BE EMPTY TOO             
         BE    VSEL4                                                            
         MVC   FVMSGNO,=AL2(CE#PFNVS)                                           
         B     VALSELX                                                          
*                                                                               
VSEL2    GOTO1 AVALOVPG,PARM,LSTSYSH,(0,LSTPGMH),0                              
         BNE   VALSELX                                                          
         MVC   SAACOVPG,APHALF                                                  
*                                                                               
VSEL4    MVC   SELSYS,SAACOVS                                                   
         MVC   SELPGM,SAACPGM                                                   
*                                                                               
         LA    R0,LSTACTH          SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM                                                        
         MVI   APPARM+4,LSTLINEN   SET NO. OF LIST LINES                        
         LA    R0,LSTLINEL                                                      
         STCM  R0,3,APPARM+6       SET LIST LINE LENGTH                         
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
*                                                                               
VALSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   L     R2,AIOAREA1                                                      
         MVC   IOKEY,APRECKEY                                                   
*                                                                               
GSEL2    TM    APINDS,APILRERD     TEST SEQUENTIAL READ BROKEN                  
         BZ    GSEL4                                                            
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETSELN                                                          
         B     GSEL6                                                            
*                                                                               
GSEL4    TM    APINDS,APILNSEQ     TEST START OF NEW SCREEN                     
         BO    GSEL6                                                            
         GOTO1 AIO,IOCONFIL+IOHI+IO1                                            
         B     GSEL8                                                            
*                                                                               
GSEL6    GOTO1 AIO,IOCONFIL+IOSQ+IO1                                            
         BNE   GETSELN                                                          
*                                                                               
GSEL8    CLI   SAACTYP,SAACTYPQ    TEST RECORD TYPE                             
         BNE   GETSELN                                                          
         CLI   SAACSUB,SAACSUBQ                                                 
         BNE   GETSELN                                                          
         CLI   SELSYS,0            TEST SYSTEM FILTER                           
         BE    GSEL10                                                           
         CLC   SAACOVS,SELSYS                                                   
         BNE   GETSELN                                                          
         CLI   SELPGM,0            TEST PROGRAM FILTER                          
         BE    GSEL10                                                           
         CLC   SAACPGM,SELPGM                                                   
         BNE   GETSELN                                                          
*                                                                               
GSEL10   MVC   APRECKEY(L'SAACKEY),SAACKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         GOTO1 FINDEL,PARM,('SAACTELQ',SAACREC)                                 
         USING SAACTD,R3           R3=A(ACTION ELEMENT)                         
*                                                                               
         L     R4,APPARM                                                        
         USING LSTACTH,R4          R4=A(LIST/SELECT LINE)                       
*                                                                               
         GOTO1 ADISSYS,SAACOVS                                                  
         MVC   LSTLSYS,APWORK      SYSTEM NAME                                  
*                                                                               
         GOTO1 ADISPGM,PARM,(SAACOVS,SAACPGM)                                   
         MVC   LSTLPGM,APWORK      PROGRAM NAME                                 
*                                                                               
         GOTO1 ADISDIC,PARM,(SAACOVS,L'LSTLWRD),(C'U',SAACTWRD)                 
         MVC   LSTLWRD,APWORK      ACTION WORD                                  
*                                                                               
         IC    RF,SAACOVS                                                       
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),SAACTDSC),SAACTWRD                            
         MVC   LSTLDSC,APWORK      ACTION DESCRIPTION                           
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+3(1),SAACACT                                                
         EDIT  (B4,FULL),(3,LSTLNUM),ALIGN=LEFT                                 
*                                                                               
DISSELX  B     XIT                                                              
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR LIST (SET SCREEN TO MODIFIED)           *         
***********************************************************************         
         SPACE 1                                                                
FSTLST   OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND AN ELEMENT                                          *         
*                                                                     *         
* NTRY: P1=(ELEMENT CODE, A(IO AREA))                                 *         
* EXIT: R3=A(ELEMENT), RF=L(ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
FINDEL   XR    R3,R3                                                            
         ICM   R3,7,1(R1)                                                       
         LA    R3,SAACDATA-SAACREC(R3)                                          
         XR    RF,RF                                                            
FEL2     IC    RF,1(R3)                                                         
         CLI   0(R3),0                                                          
         BE    FELOVER                                                          
         CLC   0(1,R3),0(R1)                                                    
         BER   RE                                                               
         BXH   R3,RF,FEL2                                                       
FELOVER  SR    RE,RB                                                            
         DC    H'0'                                                             
         SPACE 3                                                                
***********************************************************************         
* ROUTINE TO TEST A BIT OF THE BIT TABLE                              *         
*                                                                     *         
* NTRY: R1=A(BIT CODE)                                                *         
***********************************************************************         
         SPACE 1                                                                
TESTBIT  XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         LA    R1,X'07'                                                         
         NR    R1,RF                                                            
         IC    R1,MASKS(R1)        R1=MASK                                      
         SRL   RF,3                                                             
         LA    RF,BITTABLE(RF)     RF=A(BYTE OF BIT)                            
         EX    R1,TESTBITM         EX A TM                                      
         BR    RE                                                               
*                                                                               
TESTBITM TM    0(RF),0                                                          
         SPACE 1                                                                
MASKS    DC    X'8040201008040201'                                              
         EJECT                                                                  
FF       EQU   X'FF'                                                            
CTFILE   DC    C'CTFILE '                                                       
ACTCODE  DC    CL(L'DSPACTW)'Action code'                                       
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* SEACSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSFCD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSDCD                                                       
         ORG   LSTLINE             * LIST LINE LAYOUT *                         
LSTLSYS  DS    CL7                 SYSTEM                                       
         DS    CL1                                                              
LSTLPGM  DS    CL7                 PROGRAM                                      
         DS    CL1                                                              
LSTLWRD  DS    CL8                 ACTION WORD                                  
         DS    CL1                                                              
LSTLNUM  DS    CL3                 ACTION NUMBER                                
         DS    CL1                                                              
LSTLDSC  DS    CL45                ACTION DESCRIPTION                           
*                                                                               
LSTLINEL EQU   LSTACT2H-LSTACTH    LIST LINE LENGTH                             
LSTLINES EQU   LSTFOOTH-LSTACTH    LIST LINES LENGTH                            
LSTLINEN EQU   LSTLINES/LSTLINEL   NO. OF LIST LINES                            
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE SAVED IN TWA                                        *         
***********************************************************************         
         SPACE 1                                                                
         ORG   SAVOVER                                                          
*                                                                               
SAVOVPG  DS    0XL2                CURRENT OVERLAY SYSTEM/PROGRAM               
SAVOVS   DS    XL1                 CURRENT OVERLAY SYSTEM                       
SAVPGM   DS    XL1                 CURRENT PROGRAM                              
SAVACT   DS    XL1                 CURRENT ACTION CODE                          
SAVFMT   DS    XL1                 CURRENT FORMAT OF ACTION CODE                
*                                                                               
SAVCLRL  EQU   *-SAVOVER                                                        
         SPACE 1                                                                
         ORG                                                                    
         SPACE 3                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
DUB      DS    D                                                                
FULL     DS    F                                                                
PARM     DS    6A                                                               
WORK     DS    XL64                                                             
BITTABLE DS    XL32                                                             
*                                                                               
ASEQNO   DS    XL1                 ACTION SEQUENCE NUMBER                       
*                                                                               
SELKEY   DS    0XL32               * SELECTS *                                  
SELSYS   DS    XL1                 SELECT SYSTEM                                
SELPGM   DS    XL1                 SELECT PROGRAM                               
         ORG   SELKEY+L'SELKEY                                                  
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SEACS03   01/25/18'                                      
         END                                                                    
