*          DATA SET SEACS05    AT LEVEL 006 AS OF 05/01/02                      
*PHASE TA0D05A                                                                  
         TITLE '- SECURITY ACCESS - FIELD RECORDS'                              
ACS05    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS5**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING SAFDREC,R2          R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         SPACE 1                                                                
         XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
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
         B     XIT VALREQ              15 - APMVALQ                             
         B     XIT PRTREP              16 - APMREPP                             
         B     SETTWA              17 - APMSETT                                 
         B     XIT                                                              
         B     XIT                 19 - APMNEWK                                 
         B     XIT                 20 - APMFRP                                  
         B     XIT                 21 - APMDISS2                                
         SPACE 1                                                                
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALIZE TWA                                           *         
***********************************************************************         
         SPACE 1                                                                
SETTWA   XC    SAVOVPG,SAVOVPG                                                  
         XC    SAVFMT,SAVFMT                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF FIELD RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   GOTO1 AVALOVPG,PARM,DSPSYSH,(0,DSPPGMH),DSPPGMAH                       
         BNE   VALKEYX                                                          
         MVC   SAVOVPG,APHALF                                                   
         MVC   SAVFMT,APELEM+(SAPGMAIF-SAPGMD)                                  
         MVI   SAVFMT,C'N'         !!                                           
         SPACE 1                                                                
         LA    R2,IOKEY                                                         
         USING SAFDREC,R2          INITIALIZE FIELD RECORD KEY                  
         XC    SAFDKEY,SAFDKEY                                                  
         MVI   SAFDTYP,SAFDTYPQ                                                 
         MVI   SAFDSUB,SAFDSUBQ                                                 
         MVC   SAFDOVPG,SAVOVPG                                                 
         SPACE 1                                                                
         CLI   DSPFLDN,FF          TEST LAST ACTION WAS DISKEY                  
         BNE   VKEY2                                                            
         MVC   SAFDFCD,DSPFLDN+1   !!                                           
         GOTO1 ADISCODE,PARM,(SAVFMT,SAFDFCD)                                   
         MVC   DSPFLDN,APWORK                                                   
         B     VKEY4                                                            
         SPACE 1                                                                
VKEY2    GOTO1 AVALCODE,PARM,(SAVFMT,DSPFLDNH)                                  
         BNE   VALKEYX             VALIDATE FIELD CODE !!                       
         MVC   SAFDFCD,APBYTE                                                   
VKEY4    OI    DSPFLDNH+FHOID,FHOITR                                            
         SPACE 1                                                                
         MVC   APRECKEY,SAFDKEY                                                 
         SPACE 1                                                                
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)       IF NOT DISPLAY READ FOR UPDATE               
         GOTO1 AIO                                                              
         BL    VALKEYX             IF I/O ERROR EXIT                            
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         BE    VALKEYY                                                          
         SPACE 1                                                                
         MVI   APINDS,APIOKADD                                                  
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         SPACE 1                                                                
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         SPACE 1                                                                
VALKEYX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A FIELD RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1         SET UP ELEMENTLESS RECORD                    
         MVC   SAFDKEY,APRECKEY                                                 
         LA    R0,SAFDDATA-SAFDREC+1                                            
         STCM  R0,3,SAFDLEN                                                     
         MVI   SAFDSTAT,0                                                       
         MVI   SAFDDATA,0                                                       
         SPACE 1                                                                
         LA    R3,APELEM                                                        
         USING SAFLDD,R3           R3=A(FIELD ELEMENT)                          
         XC    SAFLDD(SAFLDLNQ),SAFLDD                                          
         MVI   SAFLDEL,SAFLDELQ                                                 
         MVI   SAFLDLN,SAFLDLNQ                                                 
         SPACE 1                                                                
         MVI   FVMINL,1            VALIDATE WORD DICTIONARY NUMBER              
         GOTO1 AVALDIC,PARM,(SAFDOVS,L'DSPWRDA),(C'U',DSPWRDNH)                 
         BNE   VALRECX                                                          
         MVC   SAFLDWRD,APHALF                                                  
         MVC   DSPWRDA,APWORK                                                   
         OI    DSPWRDAH+FHOID,FHOITR                                            
         SPACE 1                                                                
         IC    RF,SAFDOVS          VALIDATE TEXT DESCRIPTION NUMBER             
         LA    RF,X'80'(RF)                                                     
         GOTO1 AVALTXT,PARM,((RF),DSPDSCNH),SAFLDWRD                            
         BNE   VALRECX                                                          
         MVC   SAFLDDSC,APHALF                                                  
         MVC   DSPDSCA,APWORK                                                   
         OI    DSPDSCAH+FHOID,FHOITR                                            
         SPACE 1                                                                
         GOTO1 AADDELS,SAFDREC     ADD FIELD ELEMENT                            
         DROP  R3                                                               
         SPACE 1                                                                
         GOTO1 ASETACT,SAFDREC     DEFINE ACTIVITY ELEMENT                      
         SPACE 1                                                                
         LA    R1,IOADD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOCONFIL+IO1                                            
         GOTO1 AIO                 ADD/WRITE RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SPACE 1                                                                
VALRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF FIELD RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         GOTO1 ADISSYS,SAFDOVS                                                  
         MVC   DSPSYS,APWORK       SYSTEM NAME                                  
         SPACE 1                                                                
         CLI   SAFDPGM,0                                                        
         BE    DKEY010                                                          
         GOTO1 ADISPGM,PARM,(SAFDOVS,SAFDPGM)                                   
         MVC   DSPPGM,APWORK       PROGRAM NAME                                 
         MVI   DSPPGMH+(FVILEN-FVIHDR),L'DSPPGM                                 
         SPACE 1                                                                
DKEY010  MVI   DSPFLDN,FF           SAVE FIELD CODE                             
         MVC   DSPFLDN+1(1),SAFDFCD                                             
         SPACE 1                                                                
DISKEYX  B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY FIELD RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         BAS   RE,REENTER          HANDLE DELETE/RESTORE PROMPT                 
         GOTO1 FINDEL,SAFLDELQ                                                  
         USING SAFLDD,R3           R3=A(FIELD ELEMENT)                          
         SPACE 1                                                                
         MVC   APBYTE,SAFDOVS                                                   
         OI    APBYTE,X'80'                                                     
         GOTO1 ADISDIC,PARM,(APBYTE,L'DSPWRDA),(C'U',SAFLDWRD)                  
         MVC   DSPWRDN,APWORK+64                                                
         OI    DSPWRDNH+FHOID,FHOITR                                            
         MVC   DSPWRDA,APWORK                                                   
         OI    DSPWRDAH+FHOID,FHOITR                                            
         SPACE 1                                                                
         EDIT  (B2,SAFLDDSC),(5,DSPDSCN),ALIGN=LEFT                             
         OI    DSPDSCNH+FHOID,FHOITR                                            
         IC    RF,SAFDOVS                                                       
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),SAFLDDSC),SAFLDWRD                            
         MVC   DSPDSCA,APWORK                                                   
         OI    DSPDSCAH+FHOID,FHOITR                                            
         SPACE 1                                                                
         GOTO1 ADISACT,SAFDREC     DISPLAY ACTIVITY DATE                        
         SPACE 1                                                                
DISRECX  B     XIT                                                              
         DROP R3                                                                
         SPACE 1                                                                
REENTER  CLI   APACTN,ACTDEL       HANDLE DELETE REENTER PROMPT                 
         BE    *+12                                                             
         CLI   APACTN,ACTRES       HANDLE RESTORE REENTER PROMPT                
         BNE   *+8                                                              
*                                  AVOID NO DATA ENTERED SYSTEM MESSAGE         
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A FIELD RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SAFDREC                                                  
         OI    SAFDSTAT,X'80'      SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED FIELD RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SAFDREC                                                  
         NI    SAFDSTAT,FF-X'80'   UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)    *         
***********************************************************************         
         SPACE 1                                                                
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY         INITIALIZE RECORD KEY                        
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    SAFDKEY,SAFDKEY                                                  
         MVI   SAFDTYP,SAFDTYPQ                                                 
         MVI   SAFDSUB,SAFDSUBQ                                                 
         SPACE 1                                                                
         CLI   LSTSYSH+FHILD,0     TEST SYSTEM NAME ENTERED                     
         BE    VSEL2                                                            
         GOTO1 AVALSYS,LSTSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALSELX                                                          
         MVC   SAFDOVS,APWORK                                                   
         SPACE 1                                                                
VSEL2    CLI   LSTPGMH+FHILD,0     TEST PROGRAM NAME ENTERED                    
         BE    VSEL4                                                            
         CLI   SAFDOVS,0           IF SO NEED SYSTEM                            
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(CE#PFNVS)                                           
         B     VALSELX                                                          
         GOTO1 AVALPGM,PARM,(SAFDOVS,LSTPGMH)                                   
         BNE   VALSELX                                                          
         MVC   SAFDPGM,APWORK                                                   
         SPACE 1                                                                
VSEL4    MVC   SELSYS,SAFDOVS                                                   
         MVC   SELPGM,SAFDPGM                                                   
         SPACE 1                                                                
         LA    R0,LSTACTH          SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM                                                        
         LA    R0,LSTACT2H-LSTACTH                                              
         STCM  R0,3,APPARM+6       SET LIST LINE LENGTH                         
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SPACE 1                                                                
VALSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   L     R2,AIOAREA1                                                      
         MVC   IOKEY,APRECKEY                                                   
         SPACE 1                                                                
GSEL2    TM    APINDS,APILRERD     TEST SEQUENTIAL READ BROKEN                  
         BZ    GSEL4                                                            
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETSELN                                                          
         B     GSEL6                                                            
         SPACE 1                                                                
GSEL4    TM    APINDS,APILNSEQ     TEST START OF NEW SCREEN                     
         BO    GSEL6                                                            
         GOTO1 AIO,IOCONFIL+IOHI+IO1                                            
         B     GSEL8                                                            
         SPACE 1                                                                
GSEL6    GOTO1 AIO,IOCONFIL+IOSQ+IO1                                            
         BNE   GETSELN                                                          
         SPACE 1                                                                
GSEL8    CLI   SAFDTYP,SAFDTYPQ    TEST RECORD TYPE                             
         BNE   GETSELN                                                          
         CLI   SAFDSUB,SAFDSUBQ                                                 
         BNE   GETSELN                                                          
         CLI   SELSYS,0            TEST SYSTEM FILTER                           
         BE    GSEL10                                                           
         CLC   SAFDOVS,SELSYS                                                   
         BNE   GETSELN                                                          
         CLI   SELPGM,0            TEST PROGRAM FILTER                          
         BE    GSEL10                                                           
         CLC   SAFDPGM,SELPGM                                                   
         BNE   GETSELN                                                          
         SPACE 1                                                                
GSEL10   MVC   APRECKEY(L'SAFDKEY),SAFDKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
         SPACE 1                                                                
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
         SPACE 1                                                                
GETSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         GOTO1 FINDEL,SAFLDELQ                                                  
         USING SAFLDD,R3           R3=A(FIELD ELEMENT)                          
         SPACE 1                                                                
         L     R4,APPARM                                                        
         USING LSTACTH,R4          R4=A(LIST/SELECT LINE)                       
         SPACE 1                                                                
         GOTO1 ADISSYS,SAFDOVS                                                  
         MVC   LSTLSYS,APWORK      SYSTEM NAME                                  
         SPACE 1                                                                
         CLI   SAFDPGM,0                                                        
         BE    DISSEL2                                                          
         GOTO1 ADISPGM,PARM,(SAFDOVS,SAFDPGM)                                   
         MVC   LSTLPGM,APWORK      PROGRAM NAME                                 
         SPACE 1                                                                
DISSEL2  GOTO1 ADISDIC,PARM,(SAFDOVS,L'LSTLWRD),(C'U',SAFLDWRD)                 
         MVC   LSTLWRD,APWORK      FIELD WORD                                   
         SPACE 1                                                                
         IC    RF,SAFDOVS                                                       
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),SAFLDDSC),SAFLDWRD                            
         MVC   LSTLDSC,APWORK      FIELD DESCRIPTION                            
         SPACE 1                                                                
DISSELX  B     XIT                                                              
         DROP  R3,R4                                                            
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
* NTRY: RECORD IN IOAREA1, R1=ELEMENT CODE                            *         
* EXIT: R3=A(ELEMENT), RF=L(ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
FINDEL   L     R3,AIOAREA1                                                      
         LA    R3,SAFDDATA-SAFDREC(R3)                                          
         XR    RF,RF                                                            
FEL2     IC    RF,1(R3)                                                         
         CLI   0(R3),0                                                          
         BE    FELOVER                                                          
         CLM   R1,1,0(R3)                                                       
         BER   RE                                                               
         BXH   R3,RF,*-10                                                       
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
         SPACE 1                                                                
TESTBITM TM    0(RF),0                                                          
         SPACE 1                                                                
MASKS    DC    X'8040201008040201'                                              
         EJECT                                                                  
FF       EQU   X'FF'                                                            
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
       ++INCLUDE SEACSFAD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSDAD                                                       
         ORG   LSTLINE             * LIST LINE LAYOUT *                         
LSTLSYS  DS    CL7                 SYSTEM                                       
         DS    CL2                                                              
LSTLPGM  DS    CL7                 PROGRAM                                      
         DS    CL2                                                              
LSTLWRD  DS    CL8                 FIELD WORD                                   
         DS    CL2                                                              
LSTLDSC  DS    CL46                FIELD DESCRIPTION                            
         SPACE 2                                                                
         ORG                                                                    
         SPACE 2                                                                
***********************************************************************         
* WORKING STORAGE SAVED IN TWA                                        *         
***********************************************************************         
         SPACE 1                                                                
         ORG   SAVOVER                                                          
         SPACE 1                                                                
SAVOVPG  DS    0XL2                                                             
SAVOVS   DS    XL1                                                              
SAVPGM   DS    XL1                                                              
SAVFMT   DS    XL1                                                              
         SPACE 1                                                                
         ORG                                                                    
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
         EJECT                                                                  
WORKD    DSECT                     ** DSECT TO COVER LOCAL W/S **               
         ORG   APLOCAL                                                          
DUB      DS    D                                                                
PARM     DS    6A                                                               
WORK     DS    XL64                                                             
BITTABLE DS    XL32                                                             
SAVEKEY  DS    XL(L'SAFDKEY)                                                    
SAVEFCD  DS    XL(L'SAFDFCD)                                                    
SELKEY   DS    0XL32                                                            
SELSYS   DS    XL1                 MESSAGE SYSTEM                               
SELPGM   DS    XL1                 MESSAGE PROGRAM                              
         ORG   SELKEY+L'SELKEY                                                  
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SEACS05   05/01/02'                                      
         END                                                                    
