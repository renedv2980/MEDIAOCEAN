*          DATA SET SEACS13    AT LEVEL 001 AS OF 05/21/96                      
*          DATA SET SEACS13    AT LEVEL 022 AS OF 24/08/95                      
*PHASE TA0D13,*                                                                 
         TITLE 'SEACS13 - SECURITY ACCESS - LIMIT ACCESS LIST RECORDS'          
ACS13    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS13*,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6                                                       
         LA    R6,SAVOVER                                                       
         DROP  R6                                                               
         AH    R6,SAFETY                                                        
         USING MASKSD,R6                                                        
         LA    R2,IOKEY                                                         
         USING SALMREC,R2          R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         SPACE  1                                                               
         LA    RF,ACSACTH          THIS SHOULD CUT OUT ERRORS FOR               
         USING FLDHDRD,RF          PFKEY PRESSES                                
         OI    FLDIIND,FINPTHIS                                                 
         MVC   FLDILEN,FLDOLEN                                                  
         DROP  RF                                                               
         XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE  1                                                               
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
         B     VALREP              15 - APMVALQ                                 
         B     PRTREP              16 - APMREPP                                 
         B     XIT                 17 - APMSETT                                 
         B     XIT                 18 - APMPUTK                                 
         B     XIT                 19 - APMNEWK                                 
         B     XIT                 20 - APMFRP                                  
         B     XIT                 21 - APMDISS2                                
         SPACE  1                                                               
XIT      XIT1  ,                                                                
         SPACE  1                                                               
FELLOVER SR    RE,RB SAVE THIS HERE FOR NOW                                     
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF LIMIT ACCESS LIST RECORD               @@*         
***********************************************************************         
         SPACE 1                                                                
VALKEY   EQU   *                                                                
         XC    IOKEY,IOKEY                                                      
         MVI   SALMTYP,SALMTYPQ       RECORD TYPE IS C'F'X'19'                  
         MVI   SALMSUB,SALMSUBQ                                                 
         MVC   SALMAGY,TWAAGY         AGENCY ALPHA                              
         SPACE  1                                                               
         LA    R1,SCRSYSH             WHICH SYSTEM?                             
         ST    R1,APCURSOR                                                      
         GOTO1 AVALSYS                                                          
         BNE   VALKEYX                                                          
         L     R3,APPARM+4                                                      
         USING SYSLSTD,R3                                                       
*                                                                               
VK020    MVC   SALMSYS,APWORK                                                   
         MVC   LIMSYS,APWORK                                                    
         DROP  R3                                                               
         LA    R3,SCRLISTH                                                      
         ST    R3,APCURSOR                                                      
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R3)                                                       
         BNE   VALKEYX                                                          
         MVC   SALMLID,FVIFLD                                                   
*                                                                               
         XC    APRECKEY,APRECKEY                                                
         MVC   APRECKEY(L'SALMKEY),SALMKEY                                      
         MVC   SAVKEY(L'SALMKEY),SALMKEY                                        
*                                                                               
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)       IF NOT DISPLAY READ FOR UPDATE               
         GOTO1 AIO                                                              
         BL    VALKEYX             IF I/O ERROR XIT                             
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
VALREC   CLI   APACTN,ACTADD       ADDING A RECORD - TEMPORARY                  
         BNE   VR005                                                            
         CLI   FINBIT,C'*'                                                      
         BE    VR005                                                            
         MVC   LIMBITS(4),=X'08080808' ***FOR NOW***                            
         XC    LOCAL1,LOCAL1                                                    
         XC    LOCAL2,LOCAL2                                                    
         XC    LOCAL3,LOCAL3                                                    
         XC    LOCAL4,LOCAL4                                                    
VR005    CLI   APPFKEY,0           FUNCTION KEY PRESS?                          
         BE    EDITOR              NO                                           
         BAS   RE,WHICHPF                                                       
         CLI   UPDATE,C'Y'         UPDATE RECORD?                               
         BNE   EDITOR              NO                                           
         SPACE 1                                                                
VR010    L     R2,AIOAREA1                                                      
         MVC   SALMKEY,APRECKEY    SET UP AN ELEMENTLESS RECORD                 
         LA    R1,SALMDATA-SALMKEY+1                                            
         STCM  R1,3,SALMLEN                                                     
         MVI   SALMDATA,0                                                       
         SPACE 1                                                                
         BAS   RE,VALINP           GO VALIDATE THE LAST SCREEN OF DATA          
         XR    RF,RF                                                            
         MVC   LIMBITS(4),=X'08080808'                                          
         LA    R0,4                THERE ARE 4 OF THESE ELEMENTS                
         LA    R1,1                R1 CONTAINS SUB NUMBER                       
         LA    R3,SALMDATA         THIS IS A BIT UNORTHODOX                     
         SPACE 1                                                                
         USING SALIMD,R3                                                        
VR020    MVI   SALIMEL,SALIMELQ    X'05'ELEMENT                                 
         LR    RE,R1                                                            
         BCTR  RE,0                MAKE IT ZERO BASED                           
         STC   R1,SALIMSUB                                                      
         XC    SALIMCHK,SALIMCHK   CHECKSUM NOT USED AT PRESENT                 
         IC    RF,LIMBITS(RE)      NUMBER OF LIMIT BITS IN THIS ONE             
         STC   RF,SALIMBIT                                                      
         SLL   RE,7                                                             
         LA    RE,LOCAL(RE)        GIVES A(BITMASK)                             
         LA    R4,1                                                             
         SLL   R4,0(RF)                                                         
         SRL   R4,3                NUMBER OF BYTES FOR THIS BITMASK             
         BCTR  R4,0                -1 FOR EX.                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SALIMDTA(0),0(RE)                                                
         LA    R9,SALIMDTA(R4)     R9 POINTS TO LAST BYTE                       
         SPACE 1                                                                
VR030    CLI   0(R9),0                                                          
         BNE   VR040               COMPRESSES DATA                              
         BCTR  R9,0                BY REMOVING NULL BYTES                       
         BCTR  R4,0                FROM END OF ELEMENT                          
         B     VR030                                                            
         SPACE 1                                                                
VR040    LA    R4,SALIMDTA-SALIMD+1(R4) LENGTH OF ELEMENT                       
         STC   R4,SALIMLEN         SAVE ELEMENT LENGTH                          
         LA    R3,0(R4,R3)         NEXT AVAILABLE SPACE                         
         MVI   0(R3),0             PUT A 0 AT THE END                           
         XR    RF,RF                                                            
         ICM   RF,3,SALMLEN        RECORD LENGTH                                
         LA    RF,0(RE,RF)         BUMP RECORD LENGTH                           
         STCM  RF,3,SALMLEN        AND SAVE IT                                  
         LA    R1,1(R1)            BUMP TO NEXT SUB NUMBER                      
         BCT   R0,VR020                                                         
         DROP  R3                                                               
         SPACE 1                                                                
         GOTO1 ASETACT,SALMREC     DEFINE ACTIVITY ELEMENT                      
         SPACE 1                                                                
         LA    R1,SCRNAMEH                                                      
         GOTO1 AFVAL                                                            
         BNE   VR050                                                            
         LA    R3,APELEM                                                        
         USING SADSCD,R3                                                        
         XC    APELEM,APELEM                                                    
         MVI   SADSCEL,SADSCELQ                                                 
         ZIC   RF,FVILEN                                                        
         LA    RF,2(RF)                                                         
         STC   RF,SADSCLEN                                                      
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SADSC(0),FVIFLD                                                  
         GOTO1 AADDELS,SALMREC                                                  
         DROP  R3                                                               
         SPACE 1                                                                
VR050    MVC   IOKEY,APRECKEY                                                   
         CLI   APACTN,ACTADD       TEST ADD RECORD                              
         BNE   VR060                                                            
         GOTO1 AIO,IOADD+IOCONFIL+IO1                                           
         BE    VALRECY                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
VR060    GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
VALRECY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         SPACE 1                                                                
VALRECX  MVI   APMODE,APMPFKS                                                   
         OI    APINDS2,APIOVROK                                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF A LIMIT ACCESS LIST RECORD              @@*         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         L     R1,ASYSLST                                                       
         ICM   RF,15,2(R1)                                                      
         USING SYSLSTD,R1                                                       
         XR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         LA    R1,6(R1)                                                         
DK010    CLC   SALMSYS,SYSLNUM                                                  
         BE    DK020                                                            
         BXLE  R1,RE,DK010                                                      
         DC    H'0'                                                             
         SPACE 1                                                                
DK020    XC    APWORK,APWORK                                                    
         MVC   APWORK(L'SYSLNAME),SYSLNAME                                      
         DROP  R1                                                               
         LA    R1,SCRSYSH                                                       
         GOTO1 DISPFLD               SYSTEM NAME                                
         SPACE 1                                                                
         LA    R1,SCRLISTH                                                      
         XC    APWORK,APWORK                                                    
         MVC   APWORK(L'SALMLID),SALMLID                                        
         GOTO1 DISPFLD                                                          
         SPACE 1                                                                
DISKEYX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY EDIT RECORD                                      *         
***********************************************************************         
         SPACE 1                                                                
EDITOR   BAS   RE,VALINP                                                        
         CLI   WHICHLIM,0                                                       
         BNE   *+8                                                              
         MVI   WHICHLIM,1                                                       
         CLI   WHICHLIM,4                                                       
         BNH   *+8                                                              
         MVI   WHICHLIM,1                                                       
         TWAXC SCRINP1H,SCRLSTH,PROT=Y                                          
         LA    R0,NSCRLIN                                                       
         LA    RF,SCRTXT1H                                                      
         USING FLDHDRD,RF                                                       
ER020    NI    FLDATB,255-FATBLOW                                               
         LA    RF,SCRLINL(RF)                                                   
         BCT   R0,ER020                                                         
         MVI   FINBIT,C'*'         FLAG BEEN IN                                 
         DROP  RF                                                               
         SPACE 1                                                                
         BAS   RE,PFSETUP                                                       
         CLI   LIMSYS,X'04'                ?? MULTIPLE SYSTEMS                  
         BAS   RE,FILREAD                                                       
*                                                                               
         CLI   FLDCHNG,X'FF'       ANY CHANGES TO SCREEN                        
         BNE   ER030                                                            
         MVC   ERRFLAG,=H'38'                                                   
         B     MESSAGE                                                          
*                                                                               
ER030    MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI#EDATA)                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIMIT ACCESS LIST RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         L     R3,APELEM                                                        
         CLI   APACTN,ACTADD       ADDING A RECORD - TEMPORARY                  
         BNE   DR005                                                            
         MVC   LIMBITS(4),=X'08080808' ***FOR NOW***                            
         XC    LOCAL1(256),LOCAL1                                               
         XC    LOCAL3(256),LOCAL3                                               
         XC    APWORK,APWORK                                                    
         LA    R1,SCRNAMEH                                                      
         GOTO1 DISPFLD                                                          
         USING SADSCD,R3                                                        
DR005    MVI   APELEM,SADSCELQ                                                  
         GOTO1 AGETELS,SALMREC                                                  
         L     R3,APPARM                                                        
         LTR   R3,R3                                                            
         BZ    DR010                                                            
         ZIC   RF,SADSCLEN                                                      
         SH    RF,=H'3'                                                         
         BNP   DR010                                                            
         XC    APWORKX,APWORKX                                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SADSC                                                  
         LA    R1,SCRNAMEH                                                      
         GOTO1 DISPFLD                                                          
         DROP  R3                                                               
                                                                                
DR010    CLI   WHICHLIM,0                                                       
         BNE   *+8                                                              
         MVI   WHICHLIM,1                                                       
         CLI   WHICHLIM,4                                                       
         BNH   *+8                                                              
         MVI   WHICHLIM,1                                                       
         BAS   RE,GETLSTS          PULL OUT THE CURRENT LIST ELEMENTS           
                                                                                
         CLI   APPFKEY,0           WAS A PFKEY ENTERED?                         
         BE    *+8                 NO                                           
         BAS   RE,WHICHPF          GO VALIDATE IT THEN                          
         BAS   RE,PFSETUP                                                       
                                                                                
         TWAXC SCRINP1H,SCRLSTH,PROT=Y                                          
         LA    R0,NSCRLIN                                                       
         LA    RF,SCRTXT1H                                                      
         USING FLDHDRD,RF                                                       
DR020    NI    FLDATB,255-FATBLOW                                               
         LA    RF,SCRLINL(RF)                                                   
         BCT   R0,DR020                                                         
         DROP  RF                                                               
         CLI   LIMSYS,X'04'                                                     
         BAS   RE,FILREAD                                                       
                                                                                
DISRECY  CLI   APACTN,ACTCHA                                                    
         BNE   DISRECY1                                                         
         MVI   APMODE,APMFMOK                                                   
         OI    APINDS2,APIOVROK                                                 
         MVC   ERRFLAG,=H'38'                                                   
         B     MESSAGE                                                          
         SPACE 1                                                                
DISRECY1 MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    TWAMODE,TWAMLSM                                                  
         BZ    DISRECX                                                          
         OI    TWALSCTL,TWALSHLD+TWALSRTN+TWALSHSL                              
DISRECX  MVI   APMODE,APMPFKS                                                   
         GOTO1 ADISACT,SALMREC                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A LIMIT ACCESS LIST RECORD                      @@*         
***********************************************************************         
         SPACE 1                                                                
DELREC   CLI   APPFKEY,0           ONLY DELETE ON ENTER KEY PRESS               
         BE    DEL02                                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI#PETDE)                                           
         B     XIT                                                              
*                                                                               
DEL02    L     R2,AIOAREA1         R2=A(ACTION RECORD)                          
         GOTO1 ASETACT,SALMREC                                                  
*                                                                               
         OI    SALMSTAT,X'80'      SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED LIMIT ACCESS LIST RECORD             @@*         
***********************************************************************         
         SPACE 1                                                                
RESREC   CLI   APPFKEY,0           ONLY RESTORE ON ENTER KEY PRESS              
         BE    RES02                                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI#PETRE)                                           
         B     XIT                                                              
*                                                                               
RES02    L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SALMREC                                                  
         NI    SALMSTAT,X'FF'-X'80'   UNSET DELETE                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY         INITIALIZE RECORD KEY                        
         XC    APRECKEY,APRECKEY                                                
         MVI   SALMTYP,SALMTYPQ    RECORD TYPE IS C'F'X'19'                     
         MVI   SALMSUB,SALMSUBQ                                                 
         MVC   SALMAGY,TWAAGY      AGENCY ALPHA                                 
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         LA    R1,LISSYSH                                                       
         ST    R1,APCURSOR                                                      
         MVI   FVMINL,1            FIELD MUST HAVE I/P                          
         GOTO1 AFVAL                                                            
         BNE   VALSELX                                                          
         GOTO1 AVALSYS                                                          
         BNE   VALSELX                                                          
         SPACE 1                                                                
         L     R3,APPARM+4                                                      
         USING SYSLSTD,R3                                                       
         SPACE 1                                                                
         MVC   SALMSYS,APWORK                                                   
         MVC   LIMSYS,APWORK                                                    
         DROP  R3                                                               
         LA    R3,LISLISTH                                                      
         ST    R3,APCURSOR                                                      
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R3)                                                       
         BNE   VALSELY                                                          
         MVC   SALMLID,FVIFLD                                                   
         SPACE 1                                                                
VALSELY  XC    IOKEY,IOKEY         SAVE THE KEY                                 
         MVC   IOKEY(L'SALMKEY),SALMKEY                                         
         MVC   SAVKEY(L'SALMKEY),SALMKEY                                        
         SPACE 1                                                                
         LA    R0,LISACT1H         START AT APPARM+1(4)                         
         ST    R0,APPARM                                                        
         MVI   APPARM+4,NLISLIN    # OF LINES AT APPARM+4(1)                    
         LA    RF,LISLINL          LINE LENGTH AT APPARM+6(2)                   
         STCM  RF,3,APPARM+6                                                    
         XC    APCURSOR,APCURSOR                                                
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
         TM    APINDS,APILFLST                                                  
         BO    GS030                                                            
         SPACE 1                                                                
GS010    TM    APINDS,APILRERD     TEST SEQUENTIAL READ BROKEN                  
         BZ    GS020                                                            
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETSELN                                                          
         B     GS040                                                            
         SPACE 1                                                                
GS020    TM    APINDS,APILNSEQ     TEST START OF NEW SCREEN                     
         BO    GS040                                                            
         SPACE 1                                                                
GS030    GOTO1 AIO,IOCONFIL+IOHI+IO1                                            
         BNE   GETSELN                                                          
         B     GS050                                                            
         SPACE 1                                                                
GS040    GOTO1 AIO,IOCONFIL+IOSQ+IO1                                            
         BNE   GETSELN                                                          
         SPACE 1                                                                
GS050    CLC   APRECKEY(SALMLID-SALMKEY),SALMKEY                                
         BNE   GETSELN                                                          
         CLI   SALMSTAT,X'80'      NO DELETED ON LIST                           
         BE    GS040                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   APRECKEY(L'SALMKEY),SALMKEY                                      
         MVC   SAVKEY(L'SALMKEY),SALMKEY                                        
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
         ICM   R4,15,APPARM                                                     
         USING LISTLIND,R4                                                      
         MVC   LSTNAME,SALMLID                                                  
         SPACE 1                                                                
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SADSCD,R3                                                        
         MVI   0(R3),SADSCELQ                                                   
         GOTO1 AGETELS,SALMREC                                                  
         ICM   R3,15,APPARM                                                     
         BZ    DISS020                                                          
         ZIC   RF,SADSCLEN                                                      
         SH    RF,=H'3'                                                         
         BNP   DISS020                                                          
         EX    RF,DISMOVE                                                       
         B     DISS020                                                          
         SPACE 1                                                                
DISMOVE  MVC   LSTDSC(0),SADSC                                                  
         DROP  R3                                                               
         SPACE 1                                                                
DISS020  GOTO1 AGETACT,SALMREC                                                  
         BNE   FELLOVER                                                         
         LA    R3,ACTEL                                                         
         USING SAACVD,R3                                                        
         LA    R4,LSTACTV                                                       
         GOTO1 VDATCON,APPARM,(3,SAACVDT),(8,(R4))                              
*&&UK*&& OI    0(R4),X'F0'                                                      
         DROP  R3                                                               
         SPACE 1                                                                
DISSELX  B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR LIST (SET SCREEN TO MODIFIED)           *         
***********************************************************************         
         SPACE 1                                                                
FSTLST   OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREP   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         XC    APRECKEY,APRECKEY                                                
         XC    VALLID,VALLID                                                    
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
VRQ05    GOTO1 AFVAL,REPSYSH       VALIDATE SYSTEM (IF INPUT)                   
         BE    VRQ06                                                            
         SPACE 1                                                                
*@@ CODE IN HERE FOR SYSTEM SPECIFIC STUFF WHEN MULTIPLE SYSTEMS?               
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFREQD) FIELD IS REQUIRED                          
         B     VALREQX                                                          
*                                                                               
VRQ06    TM    FVIIND,FVINUM                                                    
         BO    VRQ10                                                            
         GOTO1 AVALSYS,REPSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALREQX                                                          
         MVC   VALSYS,APWORK                                                    
         B     VRQ20                                                            
*                                                                               
VRQ10    GOTO1 ADISSYS,VALSYS      GET SYSTEM NAME                              
         GOTO1 DISPFLD,REPSYSH     REDISPLAY                                    
         B     VRQ05                                                            
*                                                                               
VRQ20    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPLIDH       VALIDATE MESSAGE TYPE (IF INPUT)             
         BNE   VRQ40                                                            
*                                                                               
VRQ30    MVC   VALLID,FVIFLD                                                    
         LA    R2,APRECKEY         SET UP INITIAL KEY                           
         MVI   SALMTYP,SALMTYPQ    TYPE                                         
         MVI   SALMSUB,SALMSUBQ    SUB-TYPE                                     
         MVC   SALMSYS,VALSYS      SYSTEM                                       
         MVC   SALMAGY,TWAAGY      AGENCY ALPHA                                 
         MVC   SALMLID,VALLID      LIST ID.                                     
         LA    R1,IORDD+IOCONFIL+IO1                                            
         GOTO1 AIO                 TRY AND FIND IT                              
         BE    VRQ40               OK.                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALREQX                                                          
*                                                                               
VRQ40    LA    R2,APRECKEY         SET UP INITIAL KEY                           
         MVI   SALMTYP,SALMTYPQ    TYPE                                         
         MVI   SALMSUB,SALMSUBQ    SUB-TYPE                                     
         MVC   SALMSYS,VALSYS      SYSTEM                                       
         MVC   LIMSYS,VALSYS       SYSTEM                                       
         MVC   VALAGY,TWAAGY       AGENCY ALPHA FOR COMPARE                     
         MVC   SALMAGY,VALAGY      AGENCY                                       
         MVC   SALMLID,VALLID      LIST ID.                                     
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC                                                
         MVI   REPMIDSI,REPMCLRA                                                
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE MESSAGE REPORT                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
         TM    REPIND1,REPIOFF     OFF-LINE - OPEN MEDIA FILES                  
         BZ    PR005                                                            
         SPACE 1                                                                
         LA    R1,ASSWTAB          SYSTEM SWITCH TABLE                          
         USING SYSSWTAB,R1                                                      
         LA    R0,SYSSWMAX         MAX # OF ENTRIES                             
         SPACE 1                                                                
PR001    CLC   SYSSWSOV,LIMSYS     REQUESTED SYSTEM?                            
         BE    PR002                                                            
         LA    R1,SYSSWLEN(R1)     BUMP TO NEXT                                 
         BCT   R0,PR001            TRY AGAIN                                    
         DC    H'0'                REQUESTED SYSTEM NOT FOUND                   
         SPACE 1                                                                
PR002    MVC   LIMSE,SYSSWSYS      GET SE SYSTEM                                
         L     R3,SCAUTL                                                        
         MVC   SAVEUTL,4(R3)       SAVE CURRENT UTL                             
         MVC   4(1,R3),LIMSE       BUNG IN SE VALUE                             
         XC    ACPARM(24),ACPARM                                                
         GOTO1 VDMGR,ACPARM,DMOPEN,=C'MED',OFFMED,AIOAREA3                      
         MVC   4(1,R3),SAVEUTL     GET BACK OUR UTL                             
*                                                                               
PR005    LA    R8,REPPS                                                         
         USING RMEFILD,R8                                                       
         MVC   IOKEY,APRECKEY      SET INITIAL KEY VALUE                        
         MVI   LASTSYS,0           CLEAR LAST SYSTEM VALUE                      
*                                                                               
PR010    LA    R1,IOHI+IOCONFIL+IO1 READ NEXT ONE DOWN                          
         GOTO1 AIO                                                              
         BNE   PRTREPX             START CHECKS HERE                            
*                                                                               
         L     R2,AIOAREA1                                                      
         CLI   SALMTYP,SALMTYPQ    TEST STILL A LIMIT RECORD                    
         BNE   PRTREPX                                                          
         CLI   SALMSUB,SALMSUBQ    TEST STILL A LIMIT RECORD                    
         BNE   PRTREPX                                                          
         SPACE 1                                                                
         CLC   SALMAGY,VALAGY      AGENCY ALPHA SAME?                           
         BNE   PRTREPX                                                          
         SPACE 1                                                                
         CLI   VALSYS,0            TEST IF FILTER ON SYSTEM                     
         BE    *+14                                                             
         CLC   SALMSYS,VALSYS                                                   
         BNE   PRTREPX                                                          
         SPACE 1                                                                
         OC    VALLID,VALLID       TEST IF FILTER ON LIST ID                    
         BZ    *+14                                                             
         CLC   SALMLID,VALLID                                                   
         BNE   PRTREPX                                                          
         SPACE 1                                                                
         TM    REPIND1,REPIOFF     OFF-LINE PUT BOX ROUND IT.                   
         BZ    PR020                                                            
         OI    REPHEADI,REPHOPEN   OPEN BOX AFTER HEADLINES                     
         OI    REPMIDSI,REPMMLIN   PUT BOX MID-LINE AFTER MIDLINES              
         DROP  R8                                                               
         L     RF,REPABOX          BOX LAYOUT HERE??                            
         USING BOXD,RF                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,C'N'                                                      
         LA    RE,BOXROWS+L'BOXROWS-1                                           
         MVI   0(RE),C'B'                                                       
         LA    RE,BOXCOLS                                                       
         USING RMEFILD,RE                                                       
         MVI   RMCOL1,C'L'         DO SOME COLUMNS                              
         MVI   RMCOL2,C'C'                                                      
         MVI   RMCOL3,C'C'                                                      
         MVI   RMCOL4,C'C'                                                      
         MVI   RMCOL5,C'C'                                                      
         MVI   RMCOL6,C'C'                                                      
         MVI   RMCOL7,C'R'         LOTS OF COLUMNS...                           
         B     *+8                 NO NEED FOR SPACE AFTER MIDS.                
         DROP  RE,RF                                                            
         USING RMEFILD,R8                                                       
         SPACE 1                                                                
PR020    OI    REPMIDSI,REPMSPAC                                                
         MVC   REPH4+12(2),SALMLID PUSH OUT NAME OF THIS LIST ID                
         MVC   REPH4+16(30),SPACE  CLEAR DESCRIPTION FIELD                      
         SPACE 1                                                                
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SADSCD,R3                                                        
         MVI   0(R3),SADSCELQ                                                   
         GOTO1 AGETELS,SALMREC                                                  
         ICM   R3,15,APPARM                                                     
         BZ    PR025                                                            
         ZIC   RF,SADSCLEN                                                      
         SH    RF,=H'3'                                                         
         BNP   PR025                                                            
         EX    RF,PRMOVE                                                        
         B     PR025                                                            
         SPACE 1                                                                
PRMOVE   MVC   REPH4+16(0),SADSC   AND DESCRIPTION IF THERE                     
*                                                                               
PR025    MVC   LASTSYS,SALMSYS     SAVE SYSTEM (OBSOLETE NOW)                   
         MVI   WHICHLIM,1          LIMIT ACCESS 1-4 TO REPORT                   
         MVC   APRECKEY,SALMKEY    SAVE THIS KEY                                
         GOTO1 VREPORT,REPD        PRINT THE DETAILS                            
         SPACE 1                                                                
PR030    BAS   RE,GETLSTS          PULL OUT LIST ELEMENTS                       
         SPACE 1                                                                
PR1270   BAS   RE,KEYMAKE          BUILD INITIAL KEY                            
         SPACE 1                                                                
         XC    SUBKEY,SUBKEY                                                    
         MVC   SUBKEY(L'AGYKEY),IOKEY SAVE THIS KEY                             
         SPACE 1                                                                
         LA    R0,L'LOCAL1         ..                                           
         ZIC   R3,WHICHLIM                                                      
         BCTR  R3,0                                                             
         SLL   R3,7                                                             
         LA    R3,LOCAL(R3)                                                     
         ST    R3,DISSTART         SAVE IT                                      
         OC    0(L'LOCAL1,R3),0(R3)                                             
         BZ    PR1405              NOTHING SELECTED - ALL ALLOWED???            
         SPACE 1                                                                
PR1350   OC    0(1,R3),0(R3)       ARE THERE ANY BITS IN NEXT BYTE              
         BNZ   PR1370              YES                                          
         SPACE 1                                                                
PR1360   LA    R3,1(R3)                                                         
         BCT   R0,PR1350                                                        
         B     PR1410              ALL DONE FOR THIS LIMIT                      
         SPACE 1                                                                
PR1370   L     RF,DISSTART                                                      
         LR    R4,R3                                                            
         SR    R4,RF               R4 NOW CONTAINS DISP THIS BYTE               
         SLL   R4,3                R4 NOW CONTAINS # FOR THIS BYTE              
         XR    RE,RE               RE USED AS POINTER TO BIT                    
         SPACE 1                                                                
PR1380   LA    RF,BITMASK(RE)      GET THIS BIT                                 
         MVC   APBYTE,0(R3)        THIS IS COPY OF DISP BYTE                    
         OC    APBYTE,0(RF)        TEST THIS BIT ON                             
         CLC   APBYTE,0(R3)        WAS IT ON?                                   
         BE    PR1400              YES - BIT TO DISPLAY AT LAST                 
         SPACE 1                                                                
PR1390   LA    RE,1(RE)            BUMP TO NEXT                                 
         CH    RE,=H'8'            IN RANGE?                                    
         BL    PR1380              YES                                          
         B     PR1360              NO - CHECK NEXT BYTE FOR BITS                
         SPACE 1                                                                
PR1400   STC   RE,BITCNT                                                        
         BAS   RE,RECPRINT         PRINT THIS RECORD                            
         ZIC   RE,BITCNT           CURRENT BIT COUNT FOR THIS BYTE              
         B     PR1390              UNTIL FINISHED FOR THIS RECORD               
*                                                                               
PR1405   ZIC   R1,WHICHLIM                                                      
         BCTR  R1,0                                                             
         MH    R1,=H'20'                                                        
         LA    R1,MRTITS(R1)                                                    
         MVC   RMECBL,0(R1)                                                     
         MVC   RMESUBNO,NOTHING                                                 
         GOTO1 VREPORT,REPD        PRINT THE DETAILS                            
*                                                                               
PR1410   XC    FTLTYPE,FTLTYPE     CLEAR FLTYPE SO TITLE SHOWN                  
         ZIC   RF,WHICHLIM         NEXT LIMIT ACCESS LIST TO SHOW               
         LA    RF,1(RF)                                                         
         CH    RF,=H'5'            ONLY EVER 4 LIMIT ACCESSES                   
         BNL   PR1420                                                           
         STC   RF,WHICHLIM                                                      
         TM    REPIND1,REPIOFF     OFF-LINE PUT BOX ROUND IT.                   
         BZ    PR1415                                                           
         L     RF,REPABOX          BOX LAYOUT HERE??                            
         USING BOXD,RF                                                          
         ZIC   R3,REPLINE                                                       
         LA    R3,BOXROWS(R3)                                                   
         BCTR  R3,0                                                             
         MVI   0(R3),C'M'                                                       
         DROP  RF                                                               
         GOTO1 VREPORT,REPD                                                     
         MVI   0(R3),0                                                          
PR1415   B     PR1270              GO DO NEXT ONE                               
         SPACE 1                                                                
PR1420   LA    R2,IOKEY            NEXT RECORD - GET BY BUMPING LIST #          
         MVC   IOKEY,APRECKEY      BY 1 AND READING HIGH                        
         ICM   RF,3,SALMLID                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,SALMLID                                                     
         OI    REPHEADI,REPHFRCE   FORCE NEW PAGE                               
         B     PR010               ROUND AND ROUND WE GO....                    
         SPACE 1                                                                
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY RECORD ON A REPORT (MEDIA RECORDS)               *         
***********************************************************************         
         SPACE 1                                                                
RECPRINT NTR1                                                                   
         LA    R8,REPPS                                                         
         USING RMEFILD,R8                                                       
         ZIC   RE,BITCNT           CURRENT BIT COUNT FOR THIS BYTE              
         LA    RF,IOKEY                                                         
         MVC   IOKEY(L'SUBKEY),SUBKEY                                           
         USING DAGY,RF                                                          
         XC    AGYKBAG,AGYKBAG                                                  
         LA    R1,0(RE,R4)                                                      
         STC   R1,AGYKBAG                                                       
         LA    R1,IORD+IOCONMDR+IO3                                             
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGET+IOCONMFL+IO3                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         DROP  RF                                                               
         SPACE 1                                                                
RECP050  L     R2,AIOAREA3         RECORD IS NOW IN IO3                         
         XC    APWORK,APWORK                                                    
         CLI   WHICHLIM,1          CREATIVE AGENCY                              
         BE    RECP450                                                          
         CLI   WHICHLIM,2          BUYING AGENCY                                
         BE    RECP450                                                          
         CLI   WHICHLIM,3          LIMIT 1                                      
         BE    RECP500                                                          
         CLI   WHICHLIM,4          LIMIT 2                                      
         BE    RECP500                                                          
         DC    H'0'                NO BUSINESS HERE                             
         SPACE 1                                                                
RECP450  EQU   *                   EXTRACT AND DISPLAY                          
         USING DAGY,R2                                                          
         LA    RF,APWORK                                                        
         USING DMEFILD,RF                                                       
         ST    R0,BASER0                                                        
         EDIT  AGYKBAG,(4,RMESUBNO),WRK=APWORK,DUB=APDUB,ZERO=NOBLANK           
         L     R0,BASER0                                                        
         MVC   RMEAGYLO,AGYNAME                                                 
         MVC   RMEAGYSH,AGYSH                                                   
         TM    AGYKSTYP,AGYKSBAG   SPECIAL STUFF FOR BUYING AGENCY              
         BZ    RECP550                                                          
         MVC   RMEOFC(L'AGYOFFCE),AGYOFFCE                                      
         OC    AGYIDNO,AGYIDNO     DO WE HAVE AN AGENCY ID NO.?                 
         BZ    RECP550             NO                                           
         SPACE 1                                                                
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,AGYIDNO                                                  
         LA    R1,IORD+IOCONFIL+IO2                                             
         GOTO1 AIO                                                              
         BNE   RECP550                                                          
         L     R3,AIOAREA2                                                      
         LA    RF,CTIDATA                                                       
         SPACE 1                                                                
RECP460  CLI   0(RF),0             THIS IS A PAIN IN THE BACKSIDE               
         BE    RECP550                                                          
         CLI   0(RF),CTDSCELQ                                                   
         BE    RECP470                                                          
         ZIC   R1,1(RF)                                                         
         LA    RF,0(R1,RF)                                                      
         B     RECP460                                                          
         SPACE 1                                                                
RECP470  ZIC   R1,1(RF)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RMEUSRID(0),2(RF)   PULL IN USER ID                              
         B     RECP550                                                          
         DROP  R2                                                               
         SPACE 1                                                                
RECP500  EQU   *                                                                
         USING DLAC,R2                                                          
         LA    RF,APWORK                                                        
         USING DMEFILD,RF                                                       
         ST    R0,BASER0                                                        
         EDIT  LACKLAC,(4,RMESUBNO),WRK=APWORK,DUB=APDUB,ZERO=NOBLANK           
         L     R0,BASER0                                                        
         MVC   RMELACNM,LACNAME                                                 
         B     RECP550                                                          
         DROP  R2                                                               
         SPACE 1                                                                
         USING DLAC,R2                                                          
RECP550  OC    FTLTYPE,FTLTYPE     FIRST FOR THIS TYPE                          
         BNZ   RECP560                                                          
         ZIC   R1,WHICHLIM                                                      
         BCTR  R1,0                                                             
         MH    R1,=H'20'                                                        
         LA    R1,MRTITS(R1)                                                    
         MVC   RMECBL,0(R1)                                                     
         MVI   FTLTYPE,C'N'                                                     
RECP560  GOTO1 VREPORT,REPD        PRINT THE DETAILS                            
         B     XIT                                                              
         DROP  R2                                                               
         DROP  R8                                                               
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT IN ACTION FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
VALINP   NTR1                                                                   
         LA    R0,NSCRLIN                                                       
         LA    R4,SCRINP1H                                                      
VALI010  GOTO1 AFVAL,(R4)                                                       
         CLI   FVILEN,1                                                         
         BNE   VALI040                                                          
         SPACE 1                                                                
         LA    RE,ACTTAB                                                        
VALI020  CLI   0(RE),0                                                          
         BE    VALI040                                                          
         CLC   0(1,RE),FVIFLD         MATCH?                                    
         BE    VALI030                                                          
         LA    RE,ACTTBLQ(RE)                                                   
         B     VALI020                                                          
         SPACE 1                                                                
VALI030  MVI   FLDCHNG,X'FF'          FLAG INPUT TO A FIELD                     
         ICM   RF,15,1(RE)                                                      
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         SPACE 1                                                                
VALI040  LA    R4,SCRLINL(R4)                                                   
         BCT   R0,VALI010                                                       
         B     XIT                                                              
         SPACE 1                                                                
VALI050  NTR1                                                                   
         ZIC   RF,0(R4)            BUMP TO NEXT FIELD                           
         LA    RF,8(RF,R4)         DATA DISPLAYED HERE                          
         USING DMEFILD,RF                                                       
         OC    DMESUBNO,DMESUBNO   ANYTHING ON THIS LINE?                       
         BZ    XIT                 NO                                           
         PACK  APDUB,DMESUBNO      GET NUMBER OF RECORD                         
         DROP  RF                                                               
         CVB   RE,APDUB            PUT IT INTO RE                               
         LTR   RE,RE                                                            
         BZ    XIT                 MAKE SURE SOMETHING IS IN RE                 
         ZIC   R1,WHICHLIM         FIND OUT WHICH IS BEING CHANGED              
         BCTR  R1,0                ZERO BASED IT                                
         SLL   R1,7                *128 FOR L' BITMASK                          
         LA    R1,LOCAL(R1)        INDEX INTO LOCAL BIT MASK                    
         SRDL  RE,3 RE             HOLDS DISPLACEMENT INTO LOCAL                
         SRL   RF,32-3             RF INDICATES WHICH BIT TO ALTER              
         LA    RF,BITMASK(RF)                                                   
         LA    R1,0(RE,R1)         A(BYTE CONTAINING BIT)                       
         OC    0(1,R1),0(RF)       ALTER IT                                     
         B     XIT                                                              
         SPACE 1                                                                
VALI060  NTR1                                                                   
         ZIC   RF,0(R4)            EXACTLY SAME PROCEDURE AS ABOVE              
         LA    RF,8(RF,R4)                                                      
         USING DMEFILD,RF                                                       
         OC    DMESUBNO,DMESUBNO   ANYTHING ON THIS LINE?                       
         BZ    XIT                 NO                                           
         PACK  APDUB,DMESUBNO                                                   
         DROP  RF                                                               
         CVB   RE,APDUB                                                         
         LTR   RE,RE                                                            
         BZ    XIT                                                              
         ZIC   R1,WHICHLIM                                                      
         BCTR  R1,0                                                             
         SLL   R1,7                                                             
         LA    R1,LOCAL(R1)                                                     
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RF,BITMASK(RF)      THIS IS THE BIT TO TURN OFF                  
         MVC   APBYTE,0(RF)                                                     
         XC    APBYTE,=X'FF'       INVERT THE BITS                              
         LA    R1,0(RE,R1)         R1=A(THE BYTE REQUIRED)                      
         NC    0(1,R1),APBYTE      TURN IT OFF                                  
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE PFKEY PRESSES                                   *         
***********************************************************************         
         SPACE 1                                                                
WHICHPF  NTR1                                                                   
         XR    RF,RF                                                            
         IC    RF,APPFKEY          GET THE PFKEY PRESSED                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
         B    XIT                  PF01                                         
         B    WPF0200              PF02                                         
         B    WPF0300              PF03                                         
         B    WPF0400              PF04                                         
         B    WPF0500              PF05                                         
         B    WPF0600              PF06                                         
         B    WPF0700              PF07                                         
         B    WPF0800              PF08                                         
         B    WPF0900              PF09                                         
         B    WPF1000              PF10                                         
         B    WPF1100              PF11                                         
         B    WPF1200              PF12                                         
         SPACE 1                                                                
WPF0200  MVI   UPDATE,C'Y'                                                      
         B     XIT                                                              
         SPACE 1                                                                
WPF0300  MVI   WHICHLIM,1          LIMIT ACCESS 1                               
         B     XITC                                                             
         SPACE 1                                                                
WPF0400  MVI   WHICHLIM,2          LIMIT ACCESS 2                               
         B     XITC                                                             
         SPACE 1                                                                
WPF0500  MVI   WHICHLIM,3          LIMIT ACCESS 3                               
         B     XITC                                                             
         SPACE 1                                                                
WPF0600  MVI   WHICHLIM,4          LIMIT ACCESS 4                               
         B     XITC                                                             
         SPACE 1                                                                
XITC     XC    CURRENT,CURRENT                                                  
         B     XIT                                                              
         SPACE 1                                                                
WPF0700  CLI   APACTN,ACTCHA       SELECT ALL - ONLY FOR CHANGE/ADD             
         BE    *+12                                                             
         CLI   APACTN,ACTADD                                                    
         BNE   XIT                                                              
         ZIC   RF,WHICHLIM                                                      
         BCTR  RF,0                                                             
         SLL   RF,7                                                             
         LA    RE,LOCAL(RF)                                                     
         LA    RF,MASTER(RF)                                                    
         MVC   0(L'MASTER1,RE),0(RF)                                            
         B     XIT                                                              
         SPACE 1                                                                
WPF0800  CLI   APACTN,ACTCHA       DESELECT ALL - ONLY FOR CHANGE/ADD           
         BE    *+12                                                             
         CLI   APACTN,ACTADD                                                    
         BNE   XIT                                                              
         ZIC   RF,WHICHLIM                                                      
         BCTR  RF,0                                                             
         SLL   RF,7                                                             
         LA    RF,LOCAL(RF)                                                     
         XC    0(L'LOCAL1,RF),0(RF)                                             
         B     XIT                                                              
         SPACE 1                                                                
WPF0900  BAS   RE,SHIFTCNT         SCROLL UP                                    
         LH    RF,CURRENT                                                       
         LA    RE,NSCRLIN-1                                                     
         SR    RF,RE                                                            
         BP    *+8                                                              
         LA    RF,1                                                             
         STH   RF,CURRENT                                                       
         B     XIT                                                              
         SPACE 1                                                                
WPF1000  BAS   RE,SHIFTCNT         SCROLL DOWN                                  
         LH    RF,CURRENT                                                       
         LA    RF,NSCRLIN-1(RF)                                                 
         CH    RF,RECTOTL                                                       
         BL    *+8                                                              
         LH    RF,RECTOTL                                                       
         STH   RF,CURRENT                                                       
         B     XIT                                                              
         SPACE 1                                                                
WPF1100  EQU   *                                                                
WPF1200  EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PULL OUT LIST ELEMENTS                                   *         
***********************************************************************         
         SPACE 1                                                                
GETLSTS  NTR1                                                                   
         L     R2,AIOAREA1                                                      
         USING SALMREC,R2                                                       
         LA    R3,SALMDATA                                                      
         LA    R0,4               MUST BE 4 LIMIT ACCESS ELEMENTS               
         XR    RF,RF                                                            
         XR    RE,RE                                                            
GLST010  CLI   0(R3),0            E.O.R.                                        
         BNE   *+6                                                              
         DC    H'0'               YES DIE AT ONCE                               
         CLI   0(R3),SALIMELQ                                                   
         BE    GLST020                                                          
         IC    RF,1(R3)                                                         
         LA    R3,0(RF,R3)                                                      
         B     GLST010                                                          
         SPACE 1                                                                
         USING SALIMD,R3                                                        
GLST020  IC    RE,SALIMSUB        SUB NUMBER                                    
         BCTR  RE,0               MAKE IT ZERO-BASED                            
         IC    RF,SALIMBIT        NUMBER OF BITS USED FOR THIS LIMIT            
         LA    R1,LIMBITS(RE)     WHERE TO SAVE IT                              
         STC   RF,0(R1)           SAVE IT                                       
         SLL   RE,7               THIS IS QUITE NEAT                            
         LA    R1,LOCAL(RE)       R1 NOW HOLDS A(THIS BITMASK)                  
         ZIC   RE,SALIMLEN                                                      
         LA    R4,SALIMDTA-SALIMD NOTE THE +1 FOR THE E                         
         SR    RE,R4                                                            
         BZ    GLST030                                                          
         BCTR  RE,0                                                             
         EX    RE,GLSMOVE                                                       
GLST030  IC    RF,SALIMLEN                                                      
         LA    R3,0(RF,R3)        ON TO THE NEXT ONE                            
         BCT   R0,GLST010                                                       
         B     XIT                                                              
         SPACE 1                                                                
GLSMOVE  MVC   0(0,R1),SALIMDTA                                                 
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ FILES AND EXTRACT DISPLAY INFORMATION               *         
***********************************************************************         
         SPACE 1                                                                
FILREAD  NTR1                                                                   
         BAS   RE,KEYMAKE          BUILD INITIAL RECORD KEY                     
         SPACE 1                                                                
         XC    SUBKEY,SUBKEY                                                    
         MVC   SUBKEY(L'AGYKEY),IOKEY SAVE THIS KEY                             
         ZIC   R1,WHICHLIM                                                      
         BCTR  R1,0                                                             
         SLL   R1,7                                                             
         LA    R1,MASTER(R1)                                                    
         OC    0(L'MASTER1,R1),0(R1) MASK BEEN BUILT ALREADY?                   
         BNZ   MEDR270             YES - GOOD, NO NEED TO DO AGAIN.             
         BAS   RE,MSTKEYS          GO BUILD THE SUCKER                          
         SPACE 1                                                                
MEDR270  LH    R0,CURRENT          GET DISPLACEMENT TO START ON SCREEN          
         LTR   R0,R0               IF ZERO SET TO 1                             
         BNZ   *+8                 ..                                           
         LA    R0,1                ..                                           
         ZIC   R3,WHICHLIM                                                      
         BCTR  R3,0                                                             
         SLL   R3,7                                                             
         LA    R3,MASTER(R3)       R3 HOLDS START OF MASTER MASK                
         ST    R3,DISSTART         SAVE IT                                      
         ST    R3,DISNOW           THIS IS WHERE WE HAVE GOT UP TO              
         XR    R3,R3               R3 HOLDS COUNT OF TOTAL RECORDS              
         SPACE 1                                                                
MEDR280  L     R3,DISNOW           GET CURRENT DISPLACEMENT                     
         OC    0(1,R3),0(R3)       ARE THERE ANY BITS IN NEXT BYTE              
         BNZ   MEDR300             YES, PROCESS THEM FOR DISPLAY                
         SPACE 1                                                                
MEDR290  L     R3,DISNOW           BUMP TO NEXT MASTER BYTE                     
         LA    R3,1(R3)            ..                                           
         ST    R3,DISNOW           AND SAVE THE DISPLACEMENT                    
         LA    RF,L'MASTER1(RF)                                                 
         CR    RF,R3                                                            
         BH    MEDR280             TRY NEXT ONE                                 
         MVC   IOKEY,APRECKEY                                                   
         B     XIT                 NOTHING MORE TO DISPLAY                      
         SPACE 1                                                                
MEDR300  L     RF,DISSTART         WHERE WE STARTED FROM                        
         LR    R4,R3               WHERE WE ARE NOW                             
         SR    R4,RF               R4 NOW CONTAINS DISP THIS BYTE               
         SLL   R4,3                R4 NOW CONTAINS # FOR THIS BYTE              
         XR    RE,RE               RE USED AS POINTER TO BITS                   
         SPACE 1                                                                
MEDR310  LA    RF,BITMASK(RE)      GET THIS BIT                                 
         MVC   APBYTE,0(R3)        THIS IS COPY OF DISP BYTE                    
         OC    APBYTE,0(RF)        TEST THIS BIT ON                             
         CLC   APBYTE,0(R3)        WAS IT ON?                                   
         BE    MEDR330             YES                                          
         SPACE 1                                                                
MEDR320  LA    RE,1(RE)            BUMP TO NEXT                                 
         CH    RE,=H'8'            IN RANGE?                                    
         BL    MEDR310             YES                                          
         B     MEDR290             NO - CHECK NEXT BYTE FOR BITS                
         SPACE 1                                                                
MEDR330  BCT   R0,MEDR320                                                       
         SPACE 1                                                                
MEDR340  LA    R0,NSCRLIN          DISPLAY THE DATA ON THE SCREEN               
         LA    R8,SCRTXT1H         FIRST DISPLAY LINE                           
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'AGYKEY),SUBKEY                                           
         B     MEDR400                                                          
         SPACE 1                                                                
MEDR350  L     R3,DISNOW           GET CURRENT DISPLACEMENT                     
         OC    0(1,R3),0(R3)       ARE THERE ANY BITS IN NEXT BYTE              
         BNZ   MEDR370                                                          
         SPACE 1                                                                
MEDR360  L     R3,DISNOW           BUMP TO NEXT MASTER BYTE                     
         LA    R3,1(R3)                                                         
         ST    R3,DISNOW                                                        
         L     RF,DISSTART                                                      
         LA    RF,L'MASTER1(RF)                                                 
         CR    RF,R3                                                            
         BH    MEDR350                                                          
         MVC   IOKEY,APRECKEY                                                   
         B     XIT                 NOTHING MORE TO DISPLAY                      
         SPACE 1                                                                
MEDR370  L     RF,DISSTART                                                      
         LR    R4,R3                                                            
         SR    R4,RF               R4 NOW CONTAINS DISP THIS BYTE               
         SLL   R4,3                R4 NOW CONTAINS # FOR THIS BYTE              
         XR    RE,RE               RE USED AS POINTER TO BIT                    
         SPACE 1                                                                
MEDR380  LA    RF,BITMASK(RE)      GET THIS BIT                                 
         MVC   APBYTE,0(R3)        THIS IS COPY OF DISP BYTE                    
         OC    APBYTE,0(RF)        TEST THIS BIT ON                             
         CLC   APBYTE,0(R3)        WAS IT ON?                                   
         BE    MEDR400             YES - BIT TO DISPLAY AT LAST                 
         SPACE 1                                                                
MEDR390  LA    RE,1(RE)            BUMP TO NEXT                                 
         CH    RE,=H'8'            IN RANGE?                                    
         BL    MEDR380             YES                                          
         B     MEDR360             NO - CHECK NEXT BYTE FOR BITS                
         SPACE 1                                                                
MEDR400  STC   RE,BITCNT                                                        
         BAS   RE,RECSHOW                                                       
         LA    R8,SCRLINL(R8)      NEXT SCREEN LINE                             
         ZIC   RE,BITCNT           CURRENT BIT COUNT FOR THIS BYTE              
         BCT   R0,MEDR390          UNTIL THE SCREEN IS FULL                     
         MVC   IOKEY,APRECKEY                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY RECORD DETAILS                                   *         
***********************************************************************         
         SPACE 1                                                                
RECSHOW  NTR1                                                                   
         ZIC   RE,BITCNT           CURRENT BIT COUNT FOR THIS BYTE              
         LA    RF,IOKEY                                                         
         USING DAGY,RF                                                          
         XC    AGYKBAG,AGYKBAG                                                  
         LA    R9,0(RE,R4)                                                      
         STC   R9,AGYKBAG                                                       
         LA    R1,IORD+IOCONMDR+IO3                                             
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGET+IOCONMFL+IO3                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         DROP  RF                                                               
         SPACE 1                                                                
MEDR410  L     R2,AIOAREA3         RECORD IS NOW IN IO3                         
         XC    APWORK,APWORK                                                    
         CLI   WHICHLIM,1          CREATIVE AGENCY                              
         BE    MEDR450                                                          
         CLI   WHICHLIM,2          BUYING AGENCY                                
         BE    MEDR450                                                          
         CLI   WHICHLIM,3          LIMIT 1                                      
         BE    MEDR500                                                          
         CLI   WHICHLIM,4          LIMIT 2                                      
         BE    MEDR500                                                          
         DC    H'0'                NO BUSINESS HERE                             
         SPACE 1                                                                
MEDR450  EQU   *                   EXTRACT AND DISPLAY                          
         USING DAGY,R2                                                          
         LA    RF,APWORK                                                        
         USING DMEFILD,RF                                                       
         ST    R0,BASER0                                                        
         EDIT  AGYKBAG,(4,DMESUBNO),WRK=APWORK,DUB=APDUB,ZERO=NOBLANK           
         L     R0,BASER0                                                        
         MVC   DMEAGYLO,AGYNAME                                                 
         MVC   DMEAGYSH,AGYSH                                                   
         GOTO1 DISPFLD,(R8)                                                     
         B     MEDR550                                                          
         DROP  R2                                                               
         SPACE 1                                                                
MEDR500  EQU   *                                                                
         USING DLAC,R2                                                          
         LA    RF,APWORK                                                        
         USING DMEFILD,RF                                                       
         ST    R0,BASER0                                                        
         EDIT  LACKLAC,(4,DMESUBNO),WRK=APWORK,DUB=APDUB,ZERO=NOBLANK           
         L     R0,BASER0                                                        
         MVC   DMELACNM,LACNAME                                                 
         GOTO1 DISPFLD,(R8)                                                     
         B     MEDR550                                                          
         DROP  R2                                                               
         SPACE 1                                                                
         USING DLAC,R2                                                          
MEDR550  ZIC   R1,WHICHLIM                                                      
         BCTR  R1,0                                                             
         SLL   R1,7                                                             
         LA    R1,LOCAL(R1)                                                     
         ZIC   RE,LACKLAC                                                       
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RF,BITMASK(RF)                                                   
         LA    R1,0(RE,R1)                                                      
         MVC   APBYTE,0(R1)                                                     
         OC    APBYTE,0(RF)                                                     
         CLC   APBYTE,0(R1)                                                     
         BNE   MEDR560                                                          
         USING FLDHDRD,R8                                                       
         NI    FLDATB,255-FATBLOW                                               
         OI    FLDATB,FATBHIGH                                                  
MEDR560  B     XIT                                                              
         DROP  R2                                                               
         DROP  R8                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO BUILD INITIAL RECORD KEY                                 *         
***********************************************************************         
         SPACE 1                                                                
KEYMAKE  NTR1                                                                   
         USING DAGY,RF                                                          
         LA    RF,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         LA    R1,ASSWTAB          SYSTEM SWITCH TABLE                          
         USING SYSSWTAB,R1                                                      
         LA    R0,SYSSWMAX         MAX # OF ENTRIES                             
         SPACE 1                                                                
*EYM010  CLC   SYSSWSYS,LIMSYS     REQUESTED SYSTEM?                            
KEYM010  CLC   SYSSWSOV,LIMSYS     REQUESTED SYSTEM?                            
         BE    KEYM020                                                          
         LA    R1,SYSSWLEN(R1)     BUMP TO NEXT                                 
         BCT   R0,KEYM010          TRY AGAIN                                    
         DC    H'0'                REQUESTED SYSTEM NOT FOUND                   
         SPACE 1                                                                
KEYM020  MVC   AGYKAGY,SYSSWAGB    GET AGENCY BINARY                            
         DROP  R1                                                               
         DROP  RF                                                               
         SPACE 1                                                                
         USING DAGY,RF                                                          
         CLI   WHICHLIM,1          CREATIVE AGENCY                              
         BNE   KEYM050                                                          
         MVI   AGYKTYP,AGYKTYPQ                                                 
         MVI   AGYKSTYP,AGYKSCAG                                                
         B     KEYM250                                                          
         DROP  RF                                                               
         SPACE 1                                                                
         USING DAGY,RF                                                          
KEYM050  CLI   WHICHLIM,2          BUYING AGENCY                                
         BNE   KEYM100                                                          
         MVI   AGYKTYP,AGYKTYPQ                                                 
         MVI   AGYKSTYP,AGYKSBAG                                                
         B     KEYM250                                                          
         DROP  RF                                                               
         SPACE 1                                                                
         USING DLAC,RF                                                          
KEYM100  CLI   WHICHLIM,3          LIMIT 1                                      
         BNE   KEYM150                                                          
         MVI   LACKTYP,LACKTYPQ                                                 
         MVI   LACKSTY,X'01'                                                    
         B     KEYM250                                                          
         DROP  RF                                                               
         SPACE 1                                                                
         USING DLAC,RF                                                          
KEYM150  CLI   WHICHLIM,4          LIMIT 2                                      
         BE    *+6                                                              
         DC    H'0'                NO BUSINESS HERE                             
         MVI   LACKTYP,LACKTYPQ                                                 
         MVI   LACKSTY,X'02'                                                    
         B     KEYM250                                                          
KEYM250  B     XIT                                                              
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO BUILD MASTER KEYS                                        *         
***********************************************************************         
         SPACE 1                                                                
MSTKEYS  LR    R0,RE                                                            
         LA    R1,IOHIGH+IOCONMDR+IO3                                           
         B     *+8                                                              
MSTK02   LA    R1,IOSEQ+IOCONMDR+IO3                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,IOKEY            CURRENT RECORD IN IOKEY                      
         USING DAGY,RF                                                          
         CLC   AGYKEY(AGYKBAG-AGYKEY),SUBKEY IS IT ONE WE WANT?                 
         BNE   MSTKX                                                            
         ZIC   RE,AGYKBAG                                                       
         SRDL  RE,3                DISP. TO THIS BYTE IN RE                     
         SRL   RF,32-3             DISP. INTO BITMASK IN RF                     
         ZIC   R1,WHICHLIM                                                      
         BCTR  R1,0                MAKE IT ZERO BASED                           
         SLL   R1,7                                                             
         LA    R1,MASTER(R1)       THIS MASTER MASK                             
         LA    R1,0(RE,R1)         THIS BYTE                                    
         LA    RF,BITMASK(RF)      THIS BIT IN BYTE                             
         OC    0(1,R1),0(RF)       TURN ON BIT CORRESPONDING TO #               
         B     MSTK02              YES, GO BACK AND PROCESS IT                  
*                                                                               
MSTKX    LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* GENERAL SCROLL ROUTINE - GETS MAX RECORDS FOR THIS LIST             *         
***********************************************************************         
         SPACE 1                                                                
SHIFTCNT NTR1                                                                   
         ZIC   R1,WHICHLIM                                                      
         BCTR  R1,0                                                             
         ZIC   RF,LIMBITS(R1)      RF HOLDS NUMBER OF LIMIT BITS                
         SLL   R1,7                                                             
         LA    R1,MASTER(R1)       R1 HOLDS THIS RECORD`S DETAILS               
         LA    R0,1                                                             
         SLL   R0,0(RF)                                                         
         SRL   R0,3                NUMBER OF BYTES FOR THIS BITMASK             
         ST    R1,DISSTART         SAVE IT                                      
         ST    R1,DISNOW           SAVE IT                                      
         XR    R3,R3               R3 HOLDS COUNT OF TOTAL RECORDS              
         SPACE 1                                                                
SHIF010  L     R1,DISNOW           GET CURRENT DISPLACEMENT                     
         OC    0(1,R1),0(R1)       ARE THERE ANY BITS IN NEXT BYTE              
         BNZ   SHIF030                                                          
         SPACE 1                                                                
SHIF020  L     R1,DISNOW           BUMP TO NEXT MASTER BYTE                     
         LA    R1,1(R1)            ..                                           
         ST    R1,DISNOW           AND SAVE THE DISPLACEMENT                    
         BCT   R0,SHIF010                                                       
         B     SHIF050             ALL RECORDS COUNTED                          
         SPACE 1                                                                
SHIF030  L     RF,DISSTART                                                      
         LR    R4,R1                                                            
         SR    R4,RF               R4 NOW CONTAINS DISP THIS BYTE               
         SLL   R4,3                R4 NOW CONTAINS # FOR THIS BYTE              
         XR    RE,RE               RE USED AS POINTER TO BITS                   
         SPACE 1                                                                
SHIF040  LA    RF,BITMASK(RE)      GET THIS BIT                                 
         MVC   APBYTE,0(R1)        THIS IS COPY OF DISP BYTE                    
         OC    APBYTE,0(RF)        TEST THIS BIT ON                             
         CLC   APBYTE,0(R1)        WAS IT ON?                                   
         BNE   *+8                 NO                                           
         LA    R3,1(R3)            YES - ADD THIS BIT TO TOTAL                  
         SPACE 1                                                                
         LA    RE,1(RE)            BUMP TO NEXT                                 
         CH    RE,=H'8'            IN RANGE?                                    
         BL    SHIF040             YES                                          
         B     SHIF020             NO - CHECK NEXT BYTE FOR BITS                
         SPACE 1                                                                
SHIF050  STH   R3,RECTOTL                                                       
         B     XIT                                                              
***********************************************************************         
* ROUTINE TO SETUP PFKEY LINES AT BOTTOM OF SCREEN, AND TITLE AT TOP  *         
***********************************************************************         
         SPACE 1                                                                
PFSETUP  NTR1                                                                   
         MVC   SCRTXTT(72),SPACE                                                
*                                                                               
*@@            CAN TEST FOR SYSTEM SPECIFIC TITLES HERE AND USE TO              
*@@            INDEX INTO TITLE/PFKEY SETUPS AT BOTTOM OF PROGRAM               
*                                                                               
         ZIC   RF,WHICHLIM        WHICH LIMIT ACCESS TITLE DO WE WANT           
         BCTR  RF,0               MAKE IT ZERO-BASED                            
         MH    RF,=H'72'          LENGTH OF A TITLE TO INDEX IN                 
         LA    RF,MEDTITS(RF)     WE WANT THIS TITLE                            
         MVC   SCRTXTT(72),0(RF)  GET THE BUGGER                                
         SPACE 1                                                                
         LA    R1,SCRTXTTH        HEADER FOR TITLE                              
         LA    RF,L'SCRTXTT       LENGTH OF THE FIELD                           
         BAS   RE,TURNON          DISPLAY IT                                    
         SPACE 1                                                                
MPF010   XC    SCRINS1,SCRINS1    CLEAR FIRST LINE OF HELP                      
         LA    R1,SCRINS1         FIRST LINE OF IDIOT-HELP AT BOTTOM            
         LA    R4,MEDPFKS                                                       
         USING LINEINF,R1         DSECT TO COVER DISPLACEMENTS ON LINE          
         MVC   PF2K,=C'PF03='     SELF - EXPLANATORY                            
         MVC   PF3K,=C'PF05='     ..                                            
         MVC   PF5K,=C'PF09='     ..                                            
         SPACE  1                                                               
         CLI   APACTN,ACTDIS      CHANGE/ADD NEED EXTRA SHOWING...              
         BE    MPF020                                                           
         MVC   PF4K,=C'PF07='     PF07=SELECT ALL                               
         LA    RF,6                                                             
         SLL   RF,4                                                             
         LA    RF,0(R4,RF)                                                      
         MVC   PF4KV,0(RF)                                                      
         SPACE 1                                                                
MPF020   LA    RF,2               INFO FOR PF03=                                
         SLL   RF,4                                                             
         LA    RF,0(R4,RF)                                                      
         MVC   PF2KV,0(RF)                                                      
         LA    RF,4               INFO FOR PF05=                                
         SLL   RF,4                                                             
         LA    RF,0(R4,RF)                                                      
         MVC   PF3KV,0(RF)                                                      
         LA    RF,8               INFO FOR PF09=                                
         SLL   RF,4                                                             
         LA    RF,0(R4,RF)                                                      
         MVC   PF5KV,0(RF)                                                      
         LA    R1,SCRINS1H                                                      
         LA    RF,L'SCRINS1                                                     
         BAS   RE,TURNON          DISPLAY THIS LINE                             
         SPACE 1                                                                
         LA    R1,SCRINS2                                                       
         XC    SCRINS2,SCRINS2                                                  
         USING LINEINF,R1                                                       
         MVC   PF2K,=C'PF04='                                                   
         MVC   PF3K,=C'PF06='                                                   
         MVC   PF5K,=C'PF10='                                                   
         CLI   APACTN,ACTDIS                                                    
         BE    MPF030                                                           
         MVC   PF1K,=C'PF02='     PF02=UPDATE                                   
         MVC   PF4K,=C'PF08='     PF08=DESELECT ALL                             
         LA    RF,1                                                             
         SLL   RF,4                                                             
         LA    RF,0(RF,R4)                                                      
         MVC   PF1KV,0(RF)                                                      
         LA    RF,7                                                             
         SLL   RF,4                                                             
         LA    RF,0(RF,R4)                                                      
         MVC   PF4KV,0(RF)                                                      
         SPACE 1                                                                
MPF030   LA    RF,3               INFO FOR PF04=                                
         SLL   RF,4                                                             
         LA    RF,0(R4,RF)                                                      
         MVC   PF2KV,0(RF)                                                      
         LA    RF,5               INFO FOR PF06=                                
         SLL   RF,4                                                             
         LA    RF,0(R4,RF)                                                      
         MVC   PF3KV,0(RF)                                                      
         LA    RF,9               INFO FOR PF10=                                
         SLL   RF,4                                                             
         LA    RF,0(R4,RF)                                                      
         MVC   PF5KV,0(RF)                                                      
         LA    R1,SCRINS2H                                                      
         LA    RF,L'SCRINS2                                                     
         BAS   RE,TURNON                                                        
         B     XIT                                                              
         SPACE 1                                                                
         USING FLDHDRD,R1                                                       
TURNON   OI    FLDIIND,FINPTHIS                                                 
         OI    FLDOIND,FOUTTRN                                                  
         STC   RF,FLDILEN                                                       
         STC   RF,FLDOLEN                                                       
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL FIELD TRANSMIT - R1=A(HEADER), TEXT IN APWORK               *         
***********************************************************************         
         SPACE 1                                                                
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)     KNOCK OFF HEADER EXTENSION                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         COMPARE FIELD CONTENTS                       
         BER   RE                  EQUAL DON`T BOTHER TO MOVE IN DATA           
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW DATA                             
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
         SPACE 1                                                                
**********************************************************************          
* OUTPUT NON   STANDARD MESSAGES                                      *         
**********************************************************************          
         SPACE 1                                                                
MESSAGE  XC    APPARM(32),APPARM                                                
         LA    RF,APPARM                                                        
         MVI   8(RF),C'I'                                                       
         MVI   21(RF),X'FF'                                                     
         LH    R1,ERRFLAG                                                       
         STH   R1,2(RF)                                                         
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         MVI   APMODE,APMPFKS                                                   
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* LITERALS AND CONSTANTS                                             *          
**********************************************************************          
         SPACE 2                                                                
DCLIST   DS    0D                  DATA DICTIONARY REFERENCES                   
         DCDDL CT#UPDT,6,L                                                      
         DCDDL CT#CAG,20                                                        
         DCDDL CT#BAG,20                                                        
         DCDDL CT#LIM,20                                                        
         DCDDL CT#SELA,20                                                       
         DCDDL CT#DSEL,20                                                       
         DCDDL CT#FWD,8                                                         
         DCDDL CT#BACK,8                                                        
         DCDDL CT#TITL,3                                                        
         DCDDL CT#CATL,30                                                       
         DCDDL CT#BATL,30                                                       
         DCDDL CT#SNAME,11                                                      
DCLISTX  DC    X'00'                                                            
         SPACE 3                                                                
GI#PETDE EQU   X'FF00'+24          PRESS ENTER TO DELETE                        
GI#PETRE EQU   X'FF00'+25          PRESS ENTER TO RESTORE                       
GI#EDATA EQU   X'FF00'+28          ENTER DATA                                   
CTFILE   DC    C'CTFILE '                                                       
SPACE    DC    80CL1' '                                                         
BITMASK  DC    X'8040201008040201' FOR BITWISE COMPARES                         
SAFETY   DC    Y(SAVAREAX-SAVAREA) SAVED STORAGE AREA                           
         SPACE 1                                                                
                                                                                
MEDPFKS  DS    0F                 DATA DICTIONARY EQUS FOR MEDIA PFKEYS         
MPF01    DC    16CL1' '                                                         
MPF02    DCDD  CT#UPDT,6           UPDATE                                       
         DC    10CL1' '                                                         
MPF03    DCDD  CT#CAG,14           CREATIVE AGENCY                              
         DC    2CL1' '                                                          
MPF04    DCDD  CT#BAG,14           BUYING AGENCY                                
         DC    2CL1' '                                                          
MPF05    DCDD  CT#LIM,5            LIMIT-1                                      
         DC    CL2' 1'                                                          
         DC    9CL1' '                                                          
MPF06    DCDD  CT#LIM,5            LIMIT-2                                      
         DC    CL2' 2'                                                          
         DC    9CL1' '                                                          
MPF07    DCDD  CT#SELA,12          SELECT ALL                                   
         DC    4CL1' '                                                          
MPF08    DCDD  CT#DSEL,12          DESELECT ALL                                 
         DC    4CL1' '                                                          
MPF09    DCDD  CT#BACK,5           BACK                                         
         DC    11CL1' '                                                         
MPF10    DCDD  CT#FWD,5            FORWARD                                      
         DC    11CL1' '                                                         
MPF11    DC    16C' '                                                           
MPF12    DC    16C' '                                                           
                                                                                
MEDTITS  EQU   *                  TITLES FOR MEDIA LISTS                        
MTIT1    DC    CL1' '              (0)                                          
         DCDD  CT#TITL,3           (1) NO.                                      
         DC    3CL1' '             (4)                                          
         DCDD  CT#CATL,30          (7) CREATIVE AGENCY                          
         DC    3CL1' '            (37)                                          
         DCDD  CT#SNAME,11        (40) SHORT NAME                               
         DC    21CL1' '           (51)                                          
*                                                                               
MTIT2    DC    CL1' '              (0)                                          
         DCDD  CT#TITL,3           (1) NO.                                      
         DC    3CL1' '             (4)                                          
         DCDD  CT#BATL,30          (7) BUYING AGENCY                            
         DC    3CL1' '            (37)                                          
         DCDD  CT#SNAME,11        (40) SHORT NAME                               
         DC    21CL1' '           (51)                                          
*                                                                               
MTIT3    DC    CL1' '              (0)                                          
         DCDD  CT#TITL,3           (1) NO.                                      
         DC    3CL1' '             (4)                                          
         DCDD  CT#LIMD1,30         (7) LIMIT 1 DESCRIPTION                      
         DC    35CL1' '           (14)                                          
*                                                                               
MTIT4    DC    CL1' '              (0)                                          
         DCDD  CT#TITL,3           (1) NO.                                      
         DC    3CL1' '             (4)                                          
         DCDD  CT#LIMD2,30         (7) LIMIT 2 DESCRIPTION                      
         DC    35CL1' '           (14)                                          
         EJECT                                                                  
MRTITS   EQU   *                                                                
         DCDD  CT#CAG,20                                                        
         DCDD  CT#BAG,20                                                        
         DCDD  CT#LIM,5                                                         
         DC    CL2' 1'                                                          
         DC    13CL1' '                                                         
         DCDD  CT#LIM,5                                                         
         DC    CL2' 2'                                                          
         DC    13CL1' '                                                         
         SPACE 1                                                                
SCRLINL  EQU   SCRTXT2H-SCRTXT1H   L'DISPLAY LINE                               
NSCRLIN  EQU   ((SCRLST-SCRTXT1H)/SCRLINL)+1   # DISPLAY LINES                  
LISLINL  EQU   LISACT2H-LISACT1H   L'LIST LINE                                  
NLISLIN  EQU   ((LISLAST-LISACT1H)/LISLINL)+1  # LIST LINES                     
         SPACE 1                                                                
ACTTAB   EQU   *                                                                
         DC    C'S',AL4(VALI050)                                                
ACTTBLQ  EQU   *-ACTTAB                                                         
         DC    C'D',AL4(VALI060)                                                
         DC    X'00'                                                            
         SPACE 1                                                                
REPDESCL DCDD  CT#LAGL,30                                                       
NOTHING  DCDD  CT#NONE,5                                                        
         SPACE 1                                                                
DMOPEN   DC    C'DMOPEN'                                                        
         SPACE 1                                                                
OFFMED   DC    CL8'NMEDDIR'                                                     
         DC    CL8'NMEDFIL'                                                     
         DC    CL1'X'                                                           
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,30,CT#LAGL,24                                                 
         SPEC  H1,55,CT#RPT,6                                                   
         SPEC  H2,30,C'------------------------------'                          
         SPEC  H4,1,CT#LSTD,9                                                   
         SPEC  M1,3,CT#TYPE,5                                                   
         SPEC  M1,26,CT#CODE,5                                                  
         SPEC  M1,33,CT#FULN,15                                                 
         SPEC  M1,66,CT#SHNAM,12                                                
         SPEC  M1,80,CT#USRID,7                                                 
         SPEC  M1,94,CT#OFFC,12                                                 
         SPEC  H1,82,AGYNAME                                                    
         SPEC  H2,82,AGYADD                                                     
         SPEC  H3,82,REQUESTOR                                                  
         SPEC  H3,102,PAGE                                                      
         SPEC  H4,82,REPORT                                                     
         SPEC  END                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* SEACSWRK                                                                      
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSWRK                                                       
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* MEFILLAC                                                                      
* MEFILAGY                                                                      
         PRINT OFF                                                              
       ++INCLUDE MEFILLACD                                                      
       ++INCLUDE MEFILAGYD                                                      
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSEDD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSCDD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSADD                                                       
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE SAVED IN TWA                                        *         
***********************************************************************         
         SPACE 1                                                                
MASKSD   DSECT                                                                  
SAVKEY   DS    XL25                COPY OF LIMIT ACCESS KEY                     
WHICHLIM DS    X                   WHICH LIMIT ACCESS TO DISPLAY                
LIMSYS   DS    X                   LIMIT ACCESS CONNECTED SYSTEM                
LIMSE    DS    X                   SE LIST SYSTEM FOR OFFLINE STUFF             
SAVEUTL  DS    X                   CURRENT CONNECTED SYSTEM                     
FINBIT   DS    X                   BEEN TO EDIT FOR ADD                         
LIMBITS  DS    0XL4                LIMIT BIT NUMBERS                            
LIM1BIT  DS    X                   # OF BITS USED FOR LIMIT 1                   
LIM2BIT  DS    X                   # OF BITS USED FOR LIMIT 2                   
LIM3BIT  DS    X                   # OF BITS USED FOR LIMIT 3                   
LIM4BIT  DS    X                   # OF BITS USED FOR LIMIT 4                   
*                                                                               
CURRENT  DS    H                   CURRENT DISPLACEMENT INTO RECORD             
RECTOTL  DS    H                   TOTAL FOR THIS LSIT                          
*                                                                               
MASTER   DS    0X                  BITMASKS REFLECTING CURRENT FILE             
MASTER1  DS    XL128               READ STATUS                                  
MASTER2  DS    XL128                                                            
MASTER3  DS    XL128                                                            
MASTER4  DS    XL128                                                            
LOCAL    DS    0X                  BITMASKS FOR LOCAL SELECTIONS                
LOCAL1   DS    XL128                                                            
LOCAL2   DS    XL128                                                            
LOCAL3   DS    XL128                                                            
LOCAL4   DS    XL128                                                            
MASKSX   DS    0C                                                               
*                                                                               
DSLIST   DS    0D                  DATA DICTIONARY REFERENCES                   
*T@UPDT  DS    CL6                                                              
*T@CAG   DS    CL20                                                             
*T@BAG   DS    CL20                                                             
*T@LIM   DS    CL6                                                              
*T@SELA  DS    CL12                                                             
*T@DSEL  DS    CL12                                                             
*T@FWD   DS    CL8                                                              
*T@BACK  DS    CL8                                                              
*T@TITL  DS    CL3                                                              
*T@CATL  DS    CL30                                                             
*T@BATL  DS    CL30                                                             
*T@SNAME DS    CL11                                                             
*                                                                               
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
DISSTART DS    A                                                                
DISNOW   DS    A                                                                
BASER0   DS    F                                                                
ERRFLAG  DS    H                   MESSAGE FLAG FOR GETTEXT                     
FLDCHNG  DS    XL1                 FIELD HAS CHANGED                            
*                                                                               
SUBKEY   DS    XL20                SAVED MED KEY                                
BITCNT   DS    XL1                                                              
UPDATE   DS    XL1                                                              
VALSYS   DS    XL1                 REPORT SYSTEM                                
VALAGY   DS    XL2                 ''     AGENCY                                
VALLID   DS    XL2                 ''     LIST ID                               
LASTSYS  DS    XL1                                                              
FTLTYPE  DS    XL1                                                              
LOCALX   EQU   *                                                                
         SPACE 1                                                                
LISTLIND DSECT                                                                  
         DS    XL8                                                              
LSTACT   DS    XL3                                                              
         DS    XL8                                                              
         DS    XL8                                                              
LSTNAME  DS    XL2                                                              
         DS    XL3                                                              
LSTDSC   DS    XL30                                                             
         DS    XL5                                                              
LSTACTV  DS    XL7                                                              
         SPACE 1                                                                
DMEFILD  DSECT                     DSECT FOR MEFILAGY DISPLAY                   
DMESUBNO DS    XL4                                                              
         DS    XL3                                                              
DMEAGYLO DS    XL30                                                             
         ORG   DMEAGYLO                                                         
DMELACNM DS    XL30                                                             
         DS    XL3                                                              
DMEAGYSH DS    XL7                                                              
         SPACE 1                                                                
RMEFILD  DSECT                     DSECT FOR MEFILAGY REPORT                    
RMCOL1   DS    XL1                                                              
         DS    XL1                                                              
RMECBL   DS    XL20                                                             
         DS    XL1                                                              
RMCOL2   DS    XL1                                                              
         DS    XL1                                                              
RMESUBNO DS    XL4                                                              
         DS    XL1                                                              
RMCOL3   DS    XL1                                                              
         DS    XL1                                                              
RMEAGYLO DS    XL30                                                             
         ORG   RMEAGYLO                                                         
RMELACNM DS    XL30                                                             
         DS    XL1                                                              
RMCOL4   DS    XL1                                                              
         DS    XL1                                                              
RMEAGYSH DS    XL7                                                              
         DS    XL5                                                              
         DS    XL1                                                              
RMCOL5   DS    XL1                                                              
         DS    XL1                                                              
RMEUSRID DS    XL10                                                             
         DS    XL1                                                              
RMCOL6   DS    XL1                                                              
         DS    XL1                                                              
RMEOFC   DS    XL1                                                              
         DS    XL1                                                              
         DS    XL10                                                             
         DS    XL1                                                              
RMCOL7   DS    XL1                                                              
         SPACE 1                                                                
LINEINF  DSECT                                                                  
PF1K     DS    XL5                                                              
PF1KV    DS    XL7                                                              
PF2K     DS    XL5                                                              
PF2KV    DS    XL14                                                             
PF3K     DS    XL5                                                              
PF3KV    DS    XL14                                                             
PF4K     DS    XL5                                                              
PF4KV    DS    XL13                                                             
PF5K     DS    XL5                                                              
PF5KV    DS    XL5                 UP/DOWN                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SEACS13   05/21/96'                                      
         END                                                                    
PROGTAB  EQU   *                   EXTRA VALID PROGRAM ENTRIES                  
         DC    CL8'OFFLINE'                                                     
         DC    CL8'ALL'                                                         
         DC    H'0'                                                             
DMPRINT  DC    CL8'DMPRINT'                                                     
         EJECT                                                                  
*        XC    APPARM(8*L'APPARM),APPARM   SET UP GETTXT BLOCK                  
*        LA    R1,APPARM                                                        
*        USING GETTXTD,R1                                                       
*        MVC   GTMSGNO,=AL2(CI#USEDD)                                           
*        MVI   GTMTYP,GTMINF                                                    
*        MVI   GTMAXL,60                                                        
*        DROP  R1                                                               
*        MVC   FVMSGNO,=AL2(FVFGTSET)                                           
*        B     VALREQX                                                          
