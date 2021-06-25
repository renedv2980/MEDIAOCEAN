*          DATA SET SEACS17    AT LEVEL 002 AS OF 12/26/02                      
*PHASE TA0D17A                                                                  
         TITLE 'SEACS17-SECURITY ACCESS-TEXT LIMIT ACCESS LIST RECORDS'         
ACS17    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS17*,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6                                                       
         LA    R6,SAVOVER                                                       
         DROP  R6                                                               
         AH    R6,SAFETY                                                        
         USING MASKSD,R6                                                        
         LA    R2,IOKEY                                                         
         USING SATLREC,R2          R2=A(RECORD KEY)                             
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
         B     VALREC              19 - APMNEWK                                 
         B     XIT                 20 - APMFRP                                  
         B     XIT                 21 - APMDISS2                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF TEXT LIMIT ACCESS LIST RECORD            *         
***********************************************************************         
VALKEY   XC    IOKEY,IOKEY                                                      
         MVI   SATLTYP,SATLTYPQ       RECORD TYPE IS C'F'X'21'                  
         MVI   SATLSUB,SATLSUBQ                                                 
         MVC   SATLAGY,TWAAGY         AGENCY ALPHA                              
*                                                                               
         MVC   ANYINP,SCRSYSH+FHIID                                             
         OC    ANYINP,SCRLISTH+FHIID                                            
         OC    ANYINP,SCRDEFH+FHIID                                             
         TM    ANYINP,FHIITH          ANY KEY INPUT THIS TIME?                  
         BZ    *+8                    NO                                        
         MVI   PAGENUM,0              YES - RESET PAGE NUM                      
*                                                                               
         LA    R1,SCRSYSH             WHICH SYSTEM?                             
         GOTO1 AVALSYS                                                          
         BNE   VALKEYX                                                          
         L     R3,APPARM+4                                                      
         USING SYSLSTD,R3                                                       
         MVC   SATLSYS,APWORK                                                   
         DROP  R3                                                               
*                                     LIST NAME                                 
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,SCRLISTH                                                   
         BNE   VALKEYX                                                          
         MVC   SATLLID,FVIFLD                                                   
         MVC   SCRLIST,FVIFLD         REDISPLAY LIST NAME                       
         OI    FVOIND-FVIHDR+SCRLISTH,FVOXMT                                    
*                                                                               
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)  GET TODAY'S DATE         
         MVC   XFTODAY,TODAY                                                    
         XC    XFTODAY,FFILL                                                    
*                                                                               
         MVI   COPYFLAG,0                                                       
         CLI   APACTN,ACTCPY       SET FLAG IF SECOND COPY KEY                  
         BNE   VK010                                                            
         TM    ACLFMIND,ACLFMIFK   TEST FIRST TIME COPY FLAG                    
         BNZ   *+8                                                              
         MVI   COPYFLAG,1          SECOND COPY KEY                              
*                                                                               
VK010    CLI   APACTN,ACTADD                                                    
         BNE   VK040                                                            
*                                                                               
         L     R2,AIOAREA1                                                      
         LA    R1,IOHID+IOLOCK+IOCONFIL+IO1                                     
         GOTO1 AIO                                                              
         BL    VALKEYX             IF I/O ERROR XIT                             
         BE    VK020                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    VALKEYX             NO OTHER ERRORS ALLOWED                      
*                                                                               
VK020    CLC   IOKEY(SATLLID-SATLREC+L'SATLLID),SATLKEY                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERAE)  RECORD ALREADY EXIST                      
         B     VALKEYX                                                          
         MVI   APINDS,APIOKADD                                                  
         LA    R2,IOKEY                                                         
         MVC   DISTAB(2),FFILL                                                  
*                                                                               
VK040    MVI   FVMINL,1            ANY EFFECTIVE DATE?                          
         GOTO1 AFVAL,SCRDEFH                                                    
         BE    VK060                                                            
*                                                                               
         MVC   SATLDEF,XFTODAY     1'S COMPLEMENT OF THE EFF DATE               
         B     VK080                                                            
*                                                                               
VK060    ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   4(R1),PVRCONE                                                    
         BNE   VALKEYX                                                          
*                                                                               
         MVC   SATLDEF,APWORK+PVALCSTA-PERVALD                                  
         XC    SATLDEF,FFILL       1'S COMPLEMENT OF THE EFF DATE               
         MVC   OLDDEF,SATLDEF                                                   
*                                                                               
         CLI   APACTN,ACTCHA                                                    
         BNE   VK080                                                            
*                                                                               
         CLC   SATLDEF,XFTODAY                                                  
         BNH   *+10                TODAY OR LATER DATE                          
         MVC   SATLDEF,XFTODAY                                                  
*                                                                               
VK080    CLI   APACTN,ACTADD                                                    
         BE    VK190                                                            
*                                                                               
         L     R2,AIOAREA1                                                      
         LA    R1,IOHID+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)       IF NOT DISPLAY READ FOR UPDATE               
         GOTO1 AIO                                                              
         BL    VALKEYX             IF I/O ERROR XIT                             
         BE    VK100                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    VALKEYX             NO OTHER ERRORS ALLOWED                      
*                                                                               
VK100    CLC   IOKEY(SATLLID-SATLREC+L'SATLLID),SATLKEY                         
         BE    VK150                                                            
*                                                                               
         CLI   APACTN,ACTCHA                                                    
         BNE   VK130                                                            
*                       CHECK IF ANY REC FOR THIS LIST NAME AT ALL              
         L     R2,AIOAREA2                                                      
         MVC   SAVKEY,IOKEY                                                     
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(SATLLID-SATLREC+L'SATLLID),SAVKEY                          
         LA    R1,IOHID+IOCONFIL+IO2                                            
         GOTO1 AIO                                                              
         BL    VALKEYX             IF I/O ERROR XIT                             
         MVC   IOKEY,SAVKEY        RESTORE IOKEY                                
         CLC   IOKEY(SATLLID-SATLREC+L'SATLLID),SATLKEY                         
         BNE   VK130               NO, SO SURELY REC NOT FOUND                  
*                                                                               
*                                  YES, BUILD A EMPTY REC FOR DIS               
         L     R2,AIOAREA1                                                      
         MVC   SATLKEY,IOKEY       SET UP AN ELEMENTLESS RECORD                 
         LA    R1,SATLDATA-SATLKEY+1                                            
         STCM  R1,3,SATLLEN                                                     
         MVI   SATLSTAT,0                                                       
         MVI   SATLDATA,0                                                       
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK190                                                            
*                                                                               
VK130    CLI   COPYFLAG,1          SECOND COPY KEY?                             
         BNE   VK140                                                            
         MVI   APINDS,APIOKADD                                                  
         LA    R2,IOKEY                                                         
         B     VK190                                                            
*                                                                               
VK140    LA    R1,SCRLISTH            SET CURSOR TO LIST NAME                   
         ST    R1,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVFERNF)  RECORD NOT FOUND                          
         B     VALKEYX                                                          
*                                                                               
VK150    MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
         CLI   APACTN,ACTCHA                                                    
         BNE   VK190                                                            
         CLC   SATLDEF,SATLDEF-SATLREC+IOKEY                                    
         BE    *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         MVC   SATLDEF,SATLDEF-SATLREC+IOKEY                                    
*                                                                               
VK190    XC    APRECKEY,APRECKEY                                                
         MVC   APRECKEY(SATLDEF-SATLREC+L'SATLDEF),SATLKEY                      
*                                                                               
         XC    APWORK,APWORK       DISPLAY EFFECTIVE DATE                       
         MVC   APHALF,SATLDEF                                                   
         XC    APHALF,FFILL                                                     
         GOTO1 VDATCON,APPARM,(2,APHALF),(8,APWORK)                             
         LA    R1,SCRDEFH                                                       
         GOTO1 DISPFLD                                                          
*                                                                               
         MVC   SAVKEY,APRECKEY                                                  
         CLC   SEL,ACSACT                                                       
         BNE   VALKEYY                                                          
         CLI   APACTN,ACTCHA                                                    
         BNE   VALKEYY                                                          
         XC    APRECKEY,APRECKEY                                                
         MVC   APRECKEY(SATLDEF-SATLREC),SATLKEY                                
         MVC   APRECDA(2),OLDDEF                                                
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     XIT                                                              
FFILL    DC    4X'FF'                                                           
SEL      DC    CL3'SEL'                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN ACTION RECORD                           *         
***********************************************************************         
VALREC   DS    0H                                                               
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         OI    TWALSCTL,TWALSHLD+TWALSRTN  HOLD ON TO CURRENT SCREEN            
*                                                                               
         L     R2,AIOAREA1                                                      
         MVC   SATLKEY,SAVKEY      SET UP AN ELEMENTLESS RECORD                 
         LA    R1,SATLDATA-SATLKEY+1                                            
         STCM  R1,3,SATLLEN                                                     
         MVI   SATLSTAT,0                                                       
         MVI   SATLDATA,0                                                       
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,SADSCELQ                                                  
         GOTO1 ADELELS,SATLREC                                                  
*                                                                               
         GOTO1 AFVAL,SCRNAMEH                                                   
         BNE   VR060                                                            
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SADSCD,R3                                                        
         MVI   SADSCEL,SADSCELQ                                                 
         IC    R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SADSC(0),FVIFLD                                                  
         AHI   R1,SADSC-SADSCD+1                                                
         STC   R1,SADSCLEN                                                      
         GOTO1 AADDELS,SATLREC                                                  
         DROP  R3                                                               
*                                                                               
*        GOTO1 ASETACT,SATLREC     DEFINE ACTIVITY ELEMENT                      
*                                                                               
VR060    LA    R4,SCRTYP1H                                                      
         LA    RF,NFILTER                                                       
*                                                                               
VR080    BAS   RE,VALTYPE                                                       
         BNE   VALRECX                                                          
VR085    AHI   R4,FILLINE                                                       
         BCT   RF,VR080                                                         
*                                                                               
         BAS   RE,CHKPFK                                                        
*                                                                               
         CLI   APPFKEY,5           NEW PAGE?                                    
         BNE   VR110                                                            
         BAS   RE,NEWPAGE                                                       
         BNE   VALRECX                                                          
*                                                                               
VR110    BAS   RE,DISFIL                                                        
         CLI   APACTN,ACTADD                                                    
         BE    VR130                                                            
         CLI   COPYFLAG,1          SECOND COPY KEY?                             
         BE    VR130                                                            
*                                                                               
VR120    CLI   APPFKEY,6           UPDATE RECORD?                               
         BNE   VALRECY                                                          
*                                                                               
VR130    BAS   RE,UPDREC                                                        
         BE    DISREC                                                           
         CLI   ERRFLAG,C'Y'                                                     
         BE    DISREC                                                           
         B     VALRECX                                                          
*                                                                               
VALRECY  MVI   FVOMTYP,GTMWRN                                                   
         MVC   FVMSGNO,=AL2(CW#RNUPD)        REC NOT UPDATE MESSAGE             
         LA    RE,SCRTYP1H         SET THE CURSOR TO 1ST TYPE                   
         ST    RE,APCURSOR                                                      
*                                                                               
VALRECX  B     XIT                                                              
FILLINE  EQU   (SCRTYP2H-SCRTYP1H)              ONE LINE LEN                    
NFILTER  EQU   ((SCRTYPXH-SCRTYP1H)/FILLINE)+1    # LINES                       
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE THE TYPE OF FILTER                              *         
* NTRY: R4 = SCREEN HEADER OF THE INPUT LINE                          *         
***********************************************************************         
VALTYPE  NTR1                                                                   
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SATLMD,R3                                                        
         MVI   SATLMEL,SATLMELQ                                                 
*                                                                               
         MVI   FVMINL,1            READ THE TYPE                                
         LA    R1,0(R4)                                                         
         GOTO1 AFVAL                                                            
         BNE   VT060               NO TYPE                                      
*                                                                               
*                                  VALIDATE TYPE AGAINST SYS                    
         LA    RE,TYPETAB                                                       
VT020    OC    TYTS1LN-TYPETAB(,RE),TYTS1LN-TYPETAB(RE)                         
         BNZ   *+6                                                              
         DC    H'0'                NO TYPE DEFINED FOR THIS SYS                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,TYTSYSL                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCRSYS(0),TYTSYS1-TYPETAB(RE)                                    
         BE    VT025                                                            
*                                  NEXT SYS                                     
         SR    RF,RF                                                            
         ICM   RF,3,TYTS1LN-TYPETAB(RE)                                         
         AR    RE,RF                                                            
         B     VT020                                                            
*                                                                               
VT025    LA    RF,TYTS1VL-TYPETAB(RE)                                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VT027    CLI   0(RF),X'FF'                                                      
         BE    VTXN                INVALID TYPE                                 
         CLC   FVIFLD(1),0(RF)                                                  
         BE    VT040               VALID TYPE                                   
         AHI   RF,1                                                             
         B     VT027                                                            
*                                                                               
VT040    MVC   FULL,1(RF)          HOLD ON ADDR OF VALIDATION RTN               
         MVC   SATLMTYP,FVIFLD     SAVE THE TYPE                                
*                                                                               
         MVI   FVMINL,1            READ THE VALUE                               
         LA    R1,SCRVALH-SCRTYP1H(R4)                                          
         GOTO1 AFVAL                                                            
         BNE   VTXN                EXIT W/ CC=NE                                
*                                                                               
         LA    RE,FVIFLD                                                        
         CLI   FVIFLD,C'-'                                                      
         BNE   *+12                                                             
         OI    SATLMCTL,SATLMNFQ   SET BIT FOR - FILTER                         
         AHI   RE,1                                                             
*                                                                               
         ZIC   R1,FVXLEN                                                        
         TM    SATLMCTL,SATLMNFQ                                                
         BNO   *+6                                                              
         BCTR  R1,0                SUBTRACT ONE FOR - FILTER                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SATLMDTA(0),0(RE)   SAVE THE VALUE AND LENGTH                    
         LA    R0,1(R1)            SAVE THE LENGHT FOR LATER VALIDATE           
         AHI   R1,SATLMDTA-SATLMD+1                                             
         STC   R1,SATLMLEN                                                      
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         LA    R1,SATLMDTA         R0=LEN, R1=A(DATA)                           
         L     RF,FULL                                                          
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   VTXN                EXIT W/ CC=NE                                
*                                                                               
         MVI   FVMINL,1            READ THE MODE                                
         LA    R1,SCRMODEH-SCRTYP1H(R4)                                         
         GOTO1 AFVAL                                                            
         BNE   VT080               NO MODE, ASSUME WRITE ACCESS                 
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   FVIFLD,C'W'                                                      
         BE    VT080               DEFAULT TO WRITE ACCESS                      
         CLI   FVIFLD,C'R'                                                      
         BNE   VTXN                INVALD ACCESS MODE, EXIT WITH CC=NE          
         OI    SATLMCTL,SATLMRDQ                                                
         B     VT080               SET READ ONLY ACCESS                         
*                                                                               
VT060    MVI   FVMINL,1            READ THE VALUE                               
         LA    R1,SCRVALH-SCRTYP1H(R4)                                          
         GOTO1 AFVAL                                                            
         BNE   VT070                                                            
*                                                                               
         MVI   FVMINL,1                                                         
         LA    R1,0(R4)                                                         
         GOTO1 AFVAL                                                            
         BNE   VTXN                VALUE WITHOUT TYPE, EXIT WITH CC=NE          
*                                                                               
*                                  NO TYPE AND NO VALUE, PUT EMPTY ELEM         
VT070    MVI   SATLMDTA,0                                                       
         LA    R1,SATLMDTA-SATLMD+1                                             
         STC   R1,SATLMLEN                                                      
         DROP  R3                                                               
*                                                                               
VT080    BAS   RE,CHADT                                                         
*                                                                               
VTXY     CR    RB,RB                                                            
         B     XIT                                                              
VTXN     BAS   RE,CHADT                                                         
         LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VTYM     NTR1                     VALIDATE TYPE M FOR MEDIA                     
*NTRY:  R0 - LENGTH OF DATA                                                     
*       R1 - A(DATA)                                                            
*                                                                               
         CHI   R0,1                                                             
         BNE   VTYMN               LENGHT MUST BE 1                             
*                                                                               
         MVC   BYTE,0(R1)                                                       
         BAS   RE,VAN00                                                         
         BNE   VTYMN                                                            
*                                                                               
VTYMY    CR    RB,RB                                                            
         B     VTYMX                                                            
VTYMN    LTR   RB,RB                                                            
VTYMX    B     XIT                                                              
***********************************************************************         
VTYO     NTR1                     VALIDATE TYPE O FOR OFFICE                    
*NTRY:  R0 - LENGTH OF DATA                                                     
*       R1 - A(DATA)                                                            
*                                                                               
         CHI   R0,1                                                             
         BNE   VTYO05                                                           
         CLI   0(R1),C'*'                                                       
         BE    VTYOY                                                            
         B     VTYON                                                            
*                                                                               
VTYO05   CHI   R0,2                OFFICE IS 1 OR 2 CHAR                        
         BNE   VTYON                                                            
*                                                                               
VTYO10   MVC   BYTE,0(R1)                                                       
         BAS   RE,VAN00                                                         
         BNE   VTYON                                                            
         AHI   R1,1                                                             
         BCT   R0,VTYO10                                                        
*                                                                               
VTYOY    CR    RB,RB                                                            
         B     VTYOX                                                            
VTYON    LTR   RB,RB                                                            
VTYOX    B     XIT                                                              
***********************************************************************         
VTYC     NTR1                     VALIDATE TYPE C FOR CLIENT                    
*NTRY:  R0 - LENGTH OF DATA                                                     
*       R1 - A(DATA)                                                            
*                                                                               
         MVC   BYTE,0(R1)          MEDIA                                        
         BAS   RE,VAN00                                                         
         BNE   VTYCN                                                            
         AHI   R1,1                                                             
         BCTR  R0,0                                                             
*                                                                               
         CLI   0(R1),C','                                                       
         BNE   VTYCN               MEDIA IS ONLY ONE CHAR                       
         AHI   R1,1                                                             
         BCTR  R0,0                                                             
*                                                                               
         CLI   0(R1),C'*'                                                       
         BNE   VTYC05                                                           
         CHI   R0,1                                                             
         BE    VTYCY                                                            
         B     VTYCN                                                            
*                                                                               
VTYC05   CHI   R0,3                CLIENT IS 2 OR 3 CHAR                        
         BH    VTYCN                                                            
         CHI   R0,2                                                             
         BL    VTYCN                                                            
*                                                                               
VTYC10   MVC   BYTE,0(R1)                                                       
         BAS   RE,VAN00                                                         
         BNE   VTYCN                                                            
         AHI   R1,1                                                             
         BCT   R0,VTYC10                                                        
*                                                                               
VTYCY    CR    RB,RB                                                            
         B     VTYCX                                                            
VTYCN    LTR   RB,RB                                                            
VTYCX    B     XIT                                                              
***********************************************************************         
VTYP     NTR1                     VALIDATE TYPE P FOR PRODUCT                   
*NTRY:  R0 - LENGTH OF DATA                                                     
*       R1 - A(DATA)                                                            
*                                                                               
         MVC   BYTE,0(R1)          MEDIA                                        
         BAS   RE,VAN00                                                         
         BNE   VTYPN                                                            
         AHI   R1,1                                                             
         BCTR  R0,0                                                             
*                                                                               
         CLI   0(R1),C','                                                       
         BNE   VTYPN               MEDIA IS ONLY ONE CHAR                       
         AHI   R1,1                                                             
         BCTR  R0,0                                                             
*                                                                               
         CLI   0(R1),C'*'                                                       
         BNE   VTYP05                                                           
         CLI   1(R1),C','                                                       
         BNE   VTYPN                                                            
         AHI   R1,2                                                             
         SHI   R0,2                                                             
         B     VTYP20                                                           
*                                                                               
VTYP05   CLI   2(R1),C','                                                       
         BNE   *+16                                                             
         LA    R2,2                2 CHAR CLIENT CODE                           
         SHI   R0,3                LEN + 1 FOR ,                                
         B     VTYP10                                                           
*                                                                               
         CLI   3(R1),C','                                                       
         BNE   *+12                                                             
         LA    R2,3                3 CHAR CLIENT CODE                           
         SHI   R0,4                LEN + 1 FOR ,                                
*                                                                               
VTYP10   MVC   BYTE,0(R1)                                                       
         BAS   RE,VAN00                                                         
         BNE   VTYPN                                                            
         AHI   R1,1                                                             
         BCT   R2,VTYP10                                                        
*                                                                               
         AHI   R1,1                SKIP OVER THE C','                           
*                                                                               
VTYP20   CLI   0(R1),C'*'                                                       
         BNE   VTYP25                                                           
         CHI   R0,1                                                             
         BE    VTYPY                                                            
         B     VTYPN                                                            
*                                                                               
VTYP25   CHI   R0,3                PRODUCT IS 2 OR 3 CHAR                       
         BH    VTYPN                                                            
         CHI   R0,2                                                             
         BL    VTYPN                                                            
*                                                                               
VTYP30   MVC   BYTE,0(R1)                                                       
         BAS   RE,VAN00                                                         
         BNE   VTYPN                                                            
         AHI   R1,1                                                             
         BCT   R0,VTYP30                                                        
*                                                                               
VTYPY    CR    RB,RB                                                            
         B     VTYPX                                                            
VTYPN    LTR   RB,RB                                                            
VTYPX    B     XIT                                                              
***********************************************************************         
VTYE     NTR1                     VALIDATE TYPE E FOR ESTIMATE                  
*NTRY:  R0 - LENGTH OF DATA                                                     
*       R1 - A(DATA)                                                            
*                                                                               
         MVC   BYTE,0(R1)          MEDIA                                        
         BAS   RE,VAN00                                                         
         BNE   VTYEN                                                            
         AHI   R1,1                                                             
         BCTR  R0,0                                                             
*                                                                               
         CLI   0(R1),C','                                                       
         BNE   VTYEN               MEDIA IS ONLY ONE CHAR                       
         AHI   R1,1                                                             
         BCTR  R0,0                                                             
*                                                                               
         CLI   0(R1),C'*'                                                       
         BNE   VTYE05                                                           
         CLI   1(R1),C','                                                       
         BNE   VTYEN                                                            
         AHI   R1,2                                                             
         SHI   R0,2                                                             
         B     VTYE20                                                           
*                                                                               
VTYE05   CLI   2(R1),C','                                                       
         BNE   *+16                                                             
         LA    R2,2                2 CHAR CLIENT CODE                           
         SHI   R0,3                LEN + 1 FOR ,                                
         B     VTYE10                                                           
*                                                                               
         CLI   3(R1),C','                                                       
         BNE   *+12                                                             
         LA    R2,3                3 CHAR CLIENT CODE                           
         SHI   R0,4                LEN + 1 FOR ,                                
*                                                                               
VTYE10   MVC   BYTE,0(R1)                                                       
         BAS   RE,VAN00                                                         
         BNE   VTYEN                                                            
         AHI   R1,1                                                             
         BCT   R2,VTYE10                                                        
*                                                                               
         AHI   R1,1                SKIP OVER THE C','                           
*                                                                               
VTYE20   CLI   0(R1),C'*'                                                       
         BNE   VTYE25                                                           
         CLI   1(R1),C','                                                       
         BNE   VTYEN                                                            
         AHI   R1,2                                                             
         SHI   R0,2                                                             
         B     VTYE40                                                           
*                                                                               
VTYE25   CLI   2(R1),C','                                                       
         BNE   *+16                                                             
         LA    R2,2                2 CHAR PRODUCT                               
         SHI   R0,3                LEN + 1 FOR ,                                
         B     VTYE10                                                           
*                                                                               
         CLI   3(R1),C','                                                       
         BNE   *+12                                                             
         LA    R2,3                3 CHAR PRODUCT                               
         SHI   R0,4                LEN + 1 FOR ,                                
*                                                                               
VTYE30   MVC   BYTE,0(R1)                                                       
         BAS   RE,VAN00                                                         
         BNE   VTYEN                                                            
         AHI   R1,1                                                             
         BCT   R2,VTYE30                                                        
*                                                                               
         AHI   R1,1                SKIP OVER THE C','                           
*                                                                               
VTYE40   CLI   0(R1),C'*'                                                       
         BNE   VTYE45                                                           
         CHI   R0,1                                                             
         BE    VTYEY                                                            
         B     VTYEN                                                            
*                                                                               
VTYE45   CHI   R0,2                ESTIMATE IS 1 OR 2 CHAR                      
         BH    VTYEN                                                            
         CHI   R0,1                                                             
         BL    VTYEN                                                            
*                                                                               
VTYE50   MVC   BYTE,0(R1)                                                       
         BAS   RE,VAN00                                                         
         BNE   VTYEN                                                            
         AHI   R1,1                                                             
         BCT   R0,VTYE50                                                        
*                                                                               
VTYEY    CR    RB,RB                                                            
         B     VTYEX                                                            
VTYEN    LTR   RB,RB                                                            
VTYEX    B     XIT                                                              
***********************************************************************         
VAN00    LA    RF,ALPHANUM                                                      
VAN10    CLI   0(RF),X'FF'                                                      
         BE    VANBAD              NOT A MATCH                                  
*                                                                               
         CLC   BYTE,0(RF)                                                       
         BE    VANOKAY             FIND A MATCH                                 
         AHI   RF,1                                                             
         B     VAN10                                                            
*                                                                               
VANOKAY  CR    RB,RB                                                            
         BR    RE                                                               
VANBAD   LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789*',X'FF'                   
***********************************************************************         
* ROUTINE TO CHANGE/ADD AN ENTRY TO THE "DISTAB"                      *         
* NTRY: R4 = A(HEADER OF THE INPUT LINE)                              *         
*       APELEM = ELEMENT TO BE INSERTED                               *         
***********************************************************************         
CHADT    NTR1                                                                   
*                                  COPY DISTAB TO IO3                           
         LA    RE,DISTAB                                                        
         LH    RF,DISTABL                                                       
         L     R0,AIOAREA3                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RE,SCRTYP1H                                                      
         SR    R0,R0                                                            
         LR    R1,R4                                                            
         SR    R1,RE                                                            
         LA    RE,FILLINE                                                       
         DR    R0,RE               R1 = LINE #                                  
*                                                                               
         ZIC   RE,PAGENUM                                                       
         MHI   RE,NFILTER                                                       
         AR    RE,R1               RE = ENTRY # IN THE "DISTAB"                 
*                                                                               
*       R3,R4 MAY NEED TO PT THE CORRECT POSITION IF DIFF TYPE                  
*                                                                               
         LA    R4,DISTAB           FIND THE CORRECT POSITION                    
         L     R3,AIOAREA3                                                      
         LTR   RE,RE                                                            
         BZ    CHADT040                                                         
*                                                                               
CHADT020 CLC   0(2,R3),FFILL                                                    
         BE    CHADT040                                                         
*                                                                               
         ZIC   RF,1(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MVC THIS ENTRY FROM AIO3 TO DISTAB           
*                                                                               
         ZIC   R0,1(R3)            GOTO NEXT ENTRY                              
         AR    R3,R0                                                            
         AR    R4,R0                                                            
         BCT   RE,CHADT020                                                      
*                                                                               
*                                  R4 = A(TARGET ENTRY POSITION)                
CHADT040 ZIC   RF,APELEM+1         ELEMENT LENGTH TO BE INSERTED                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),APELEM                                                   
         LA    R4,1(RF,R4)                                                      
*                                                                               
         CLC   0(2,R3),FFILL       END OF TABLE                                 
         BE    CHADT060            NO                                           
*                                                                               
         ZIC   R0,1(R3)            SKIP THIS ENTRY IN AIO3                      
         AR    R3,R0                                                            
*                                  THEN, MOVE THE REST ENTRIES                  
CHADT060 LR    RE,R4                                                            
         L     RF,AIOAREA3                                                      
         AH    RF,DISTABL                                                       
         SR    RF,R3                                                            
         AR    R4,RF                                                            
         LR    R0,R3                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
CHADT100 LA    RE,DISTAB           UPDATE TABLE LENGTH                          
         SR    R4,RE                                                            
         STH   R4,DISTABL                                                       
*                                                                               
CHADTX   B     XIT                                                              
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF A TEXT LIMIT ACCESS LIST RECORD           *         
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         L     R1,ASYSLST                                                       
         ICM   RF,15,2(R1)                                                      
         USING SYSLSTD,R1                                                       
         XR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         LA    R1,6(R1)                                                         
DK010    CLC   SATLSYS,SYSLNUM                                                  
         BE    DK020                                                            
         BXLE  R1,RE,DK010                                                      
         DC    H'0'                                                             
*                                                                               
DK020    XC    APWORK,APWORK                                                    
         MVC   APWORK(L'SYSLNAME),SYSLNAME                                      
         DROP  R1                                                               
         LA    R1,SCRSYSH                                                       
         GOTO1 DISPFLD               SYSTEM NAME                                
*                                                                               
         LA    R1,SCRLISTH                                                      
         XC    APWORK,APWORK                                                    
         MVC   APWORK(L'SATLLID),SATLLID                                        
         GOTO1 DISPFLD                                                          
*                                                                               
         MVC   APHALF,SATLDEF                                                   
         OC    SATLDEF,SATLDEF                                                  
         BNE   *+10                                                             
         MVC   APHALF,APRECDA                                                   
         XC    APHALF,FFILL                                                     
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,APHALF),(8,APWORK)                             
         LA    R1,SCRDEFH                                                       
         GOTO1 DISPFLD                                                          
*                                                                               
DISKEYX  MVI   PAGENUM,0                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIMIT ACCESS LIST RECORD                         *         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
*                                                                               
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         OI    TWALSCTL,TWALSHLD+TWALSRTN  HOLD ON TO CURRENT SCREEN            
*                                                                               
         BAS   RE,CHKPFK                                                        
*                                                                               
         XC    SCRNAME,SCRNAME                                                  
         OI    FVOIND-FVIHDR+SCRNAMEH,FVOXMT                                    
*                                                                               
         SR    R8,R8               # OF ENTRIES IN DISTAB                       
         LA    R4,DISTAB                                                        
*                                                                               
         LA    R3,SATLDATA         GET ELEMENT DATA                             
DR020    CLI   0(R3),0                                                          
         BE    DR080               END OF RECORD                                
         CLI   0(R3),SADSCELQ                                                   
         BE    DR040                                                            
         CLI   0(R3),SATLMELQ                                                   
         BE    DR060                                                            
*                                                                               
DR030    SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR020                                                            
*                                                                               
         USING SADSCD,R3                                                        
DR040    ZIC   RF,SADSCLEN                                                      
         SHI   RF,SADSC-SADSCD-1                                                
         BNP   DR020                                                            
         XC    APWORK,APWORK                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SADSC                                                  
         LA    R1,SCRNAMEH                                                      
         GOTO1 DISPFLD                                                          
         B     DR030                                                            
         DROP  R3                                                               
*                                                                               
         USING SATLMD,R3                                                        
DR060    ZIC   RF,SATLMLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SATLMD                                                   
*                                                                               
         LA    R4,1(RF,R4)                                                      
         AHI   R8,1                                                             
         B     DR030                                                            
         DROP  R3                                                               
*                                                                               
DR080    EQU   *                                                                
         SR    R0,R0               PREPARE FOR DIVIDE                           
         LR    R1,R8                                                            
         LA    RE,NFILTER                                                       
         DR    R0,RE               R1=#PAGES, R0=#LINES IN LAST PAGE            
*                                                                               
DR090    LTR   R0,R0                                                            
         BNZ   DR095                                                            
*                                                                               
         LTR   R1,R1                                                            
         BZ    DR100               0 PAGE & LINE, FILL THIS EMPTY PAGE          
         B     DR120                                                            
*                                                                               
DR095    SR    RE,R0               RE=#EMPTY LINES                              
*                                  FILL IN EMPTRY ENTRIES FOR LAST PAGE         
DR100    MVC   0(L'EMTNTRY,R4),EMTNTRY                                          
         AHI   R4,L'EMTNTRY                                                     
         AHI   R8,1                                                             
         BCT   RE,DR100                                                         
*                                                                               
DR120    MVC   0(2,R4),FFILL       MARK THE END OF DISTAB                       
         AHI   R4,2                                                             
         LA    RE,DISTAB                                                        
         SR    R4,RE                                                            
         STH   R4,DISTABL          SAVE CURRENT LENGTH OF THIS TABLE            
*                                                                               
         GOTO1 ADISACT,SATLREC     DISPLAY ACTIVITY DATE                        
*                                                                               
         BAS   RE,DISFIL                                                        
         CLI   ERRFLAG,C'Y'                                                     
         BNE   XIT                                                              
         MVC   FVMSGNO,=AL2(CE#MFTR)  MIX +/- FILTERS FOR SAME TYPE             
         MVI   ERRFLAG,C'N'                                                     
         B     XIT                                                              
EMTNTRY  DC    XL6'070600000000'   EMPTY ENTRY                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE FILTER FROM THE INTERNAL "DISTAB"                       *         
***********************************************************************         
DISFIL   NTR1                                                                   
*                                  CLEAR THE SCREEN                             
         LA    RE,SCRTYP1H                                                      
         LA    RF,NFILTER                                                       
DF010    XC    8(L'SCRTYP1,RE),8(RE)                                            
         OI    FVOIND-FVIHDR(RE),FVOXMT   FHOITR+FHOIMO                         
         LA    R1,SCRVALH-SCRTYP1H(RE)                                          
         XC    8(L'SCRVAL,R1),8(R1)                                             
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         LA    R1,SCRMODEH-SCRTYP1H(RE)                                         
         XC    8(L'SCRMODE,R1),8(R1)                                            
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         AHI   RE,FILLINE                                                       
         BCT   RF,DF010                                                         
*                                                                               
         ZIC   RE,PAGENUM                                                       
         MHI   RE,NFILTER                                                       
         LA    R3,DISTAB           MAY NEED TO PT DIFF TYPE                     
         LTR   RE,RE                                                            
         BZ    DF040                                                            
*                                                                               
DF020    ZIC   R0,1(R3)            GET NEXT ELEMENT                             
         AR    R3,R0                                                            
         BCT   RE,DF020                                                         
*                                                                               
DF040    LA    R4,SCRTYP1H                                                      
         LA    R8,NFILTER                                                       
         MVI   LASTPAGE,C'N'                                                    
*                                                                               
         USING SATLMD,R3                                                        
DF060    CLC   0(2,R3),FFILL                                                    
         BNE   *+12                                                             
         MVI   LASTPAGE,C'Y'                                                    
         B     DF190                                                            
*                                                                               
         CLI   SATLMDTA,C' '                                                    
         BNH   DF080                                                            
         ZIC   RF,SATLMLEN                                                      
         SHI   RF,SATLMDTA-SATLMD-1                                             
         XC    APWORK,APWORK                                                    
         LA    RE,APWORK                                                        
*                                                                               
         TM    SATLMCTL,SATLMNFQ                                                
         BZ    *+12                                                             
         MVI   0(RE),C'-'          NEGATIVE FILTER                              
         AHI   RE,1                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SATLMDTA                                                 
         LA    R1,SCRVALH-SCRTYP1H(R4)                                          
         GOTO1 DISPFLD                                                          
*                                                                               
         XC    APWORK,APWORK                                                    
         MVC   APWORK(1),SATLMTYP                                               
         LA    R1,0(R4)                                                         
         GOTO1 DISPFLD                                                          
*                                                                               
         MVI   APWORK,C'W'                                                      
         TM    SATLMCTL,SATLMRDQ                                                
         BZ    *+8                                                              
         MVI   APWORK,C'R'                                                      
         LA    R1,SCRMODEH-SCRTYP1H(R4)                                         
         GOTO1 DISPFLD                                                          
*                                                                               
DF080    ZIC   RF,SATLMLEN                                                      
         AR    R3,RF                                                            
         AHI   R4,FILLINE                                                       
         BCT   R8,DF060                                                         
         DROP  R3                                                               
*                                                                               
         CLC   0(2,R3),FFILL                                                    
         BNE   *+8                                                              
         MVI   LASTPAGE,C'Y'                                                    
*                                                                               
DF190    DS    0H                                                               
DFX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK WHICH PFKEYS IS PRESSED                                       *         
***********************************************************************         
CHKPFK   NTR1                                                                   
         CLI   APPFKEY,0           WAS A PFKEY ENTERED?                         
         BE    CPFKX                                                            
*                                  BACK UP A PAGE                               
         CLI   APPFKEY,9                                                        
         BNE   CPFK020                                                          
         ZIC   RE,PAGENUM                                                       
         SHI   RE,1                                                             
         BM    CPFK040                                                          
         STC   RE,PAGENUM                                                       
         B     CPFK040                                                          
*                                  FORWARD A PAGE                               
CPFK020  CLI   APPFKEY,10                                                       
         BNE   CPFKX                                                            
         CLI   LASTPAGE,C'Y'                                                    
         BE    CPFK040                                                          
         ZIC   RE,PAGENUM                                                       
         AHI   RE,1                                                             
         STC   RE,PAGENUM                                                       
*                                                                               
CPFK040  LA    RE,SCRTYP1H         SET THE CURSOR TO 1ST TYPE                   
         ST    RE,APCURSOR                                                      
*                                                                               
CPFKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE THE RECORD                                                  
***********************************************************************         
UPDREC   NTR1                                                                   
*                                                                               
         L     R2,AIOAREA1                                                      
*                                                                               
         CLC   SATLDEF,XFTODAY                                                  
         BNH   *+10                TODAY OR LATER DATE                          
         MVC   SATLDEF,XFTODAY                                                  
*                                                                               
         XC    APWORK,APWORK       DISPLAY EFFECTIVE DATE                       
         MVC   APHALF,SATLDEF                                                   
         XC    APHALF,FFILL                                                     
         GOTO1 VDATCON,APPARM,(2,APHALF),(8,APWORK)                             
         LA    R1,SCRDEFH                                                       
         GOTO1 DISPFLD                                                          
*                                                                               
         LA    R4,DISTAB                                                        
         USING SATLMD,R4                                                        
*                                                                               
UR020    CLC   0(2,R4),FFILL                                                    
         BE    UR080               NO MORE ELEMENT                              
*                                                                               
         CLI   SATLMDTA,0                                                       
         BE    UR040               EMPTY ELEMENT, SKIP THIS                     
         DROP  R4                                                               
*                                                                               
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R4)                                                  
*                                                                               
         GOTO1 AADDELS,SATLREC                                                  
*                                                                               
UR040    ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     UR020                                                            
*                                                                               
UR080    SR    R1,R1                                                            
         ICM   R1,3,SATLLEN                                                     
         CHI   R1,L'IOAREA1                                                     
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     URX                                                              
*                                                                               
         BAS   RE,NPFILTR          CHECK CONSISTENCE OF +/- FILTER              
         BNE   URX                                                              
*                                                                               
         GOTO1 ASETACT,SATLREC     DEFINE ACTIVITY ELEMENT                      
*                                                                               
         MVC   IOKEY(SATLDEF-SATLREC+L'SATLDEF),SATLKEY                         
         LA    R1,IORD+IOLOCK+IOCONFIL+IO2                                      
         GOTO1 AIO                                                              
         BNE   UR100                                                            
         LA    R1,IOWRITE+IOCONFIL+IO1        WRITE REC                         
         GOTO1 AIO                                                              
         B     UR120                                                            
*                                                                               
UR100    BAS   RE,DELOLD                      DELETE OLD REC                    
         BE    UR110                          OKAY TO ADD NEW REC               
*                                                                               
         MVC   FVMSGNO,=AL2(CE#NANR)       CANNOT ADD NEW REC ERROR             
         B     URX                                                              
*                                                                               
UR110    LA    R1,IOADD+IOCONFIL+IO1          ADD A NEW REC                     
         GOTO1 AIO                                                              
*                                                                               
UR120    MVI   PAGENUM,0           RESET PAGE NUM                               
*                                                                               
URXY     CR    R1,R1                                                            
URX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK THE CONSISTENCE OF +/- FILTERS                               
***********************************************************************         
NPFILTR  NTR1                                                                   
*                                                                               
         MVI   ERRFLAG,C'N'                                                     
         XC    BYTE,BYTE                                                        
         SR    R4,R4               # OF FILTER ELEMENT                          
         L     R2,AIOAREA1                                                      
         LA    R3,SATLDATA         GET ELEMENT DATA                             
*                                                                               
NF020    CLI   0(R3),0                                                          
         BE    NFXY                END OF RECORD                                
         CLI   0(R3),SATLMELQ                                                   
         BE    NF060                                                            
*                                                                               
NF040    SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     NF020                                                            
*                                                                               
         USING SATLMD,R3                                                        
NF060    AHI   R4,1                                                             
         CLC   BYTE,SATLMTYP       SAME TYPE?                                   
         BE    NF080               YES                                          
         MVC   BYTE,SATLMTYP       SAVE THIS TYPE NOW                           
         MVC   BYTE2,SATLMCTL                                                   
         NI    BYTE2,SATLMNFQ      X'80'=-, X'00'=+                             
         B     NF040               NEXT ELEMENT                                 
*                                                                               
NF080    MVC   HALF(1),SATLMCTL                                                 
         NI    HALF,SATLMNFQ                                                    
         CLC   BYTE2,HALF          SAME +/- FILTER                              
         BE    NF040                                                            
         DROP  R3                                                               
*                                                                               
         BCTR  R4,0                                                             
         SR    R0,R0               PREPARE FOR DIVIDE                           
         LR    R1,R4                                                            
         LA    RE,NFILTER                                                       
         DR    R0,RE               R1=#PAGES, R0=#LINES IN LAST PAGE            
*                                                                               
         STC   R1,PAGENUM                                                       
         LR    RE,R0                                                            
         MHI   RE,FILLINE                                                       
         LA    RE,SCRTYP1H(RE)                                                  
         ST    RE,APCURSOR                                                      
         MVI   ERRFLAG,C'Y'                                                     
*                                                                               
NFXN     LTR   RB,RB               CC=NE, +/- FILTER CONFLICT                   
         B     XIT                                                              
NFXY     CR    R1,R1               CC=EQ, NOT +/- FILTER CONFLICT               
NFX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN OLD RECORD (15 MAX RECORDS PER LIST NAME)                
***********************************************************************         
DELOLD   NTR1                                                                   
*                                                                               
         MVC   OLDKEY,SATLKEY                                                   
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(SATLLID-SATLREC+L'SATLLID),OLDKEY                          
         GOTO1 AIO,IOHI+IOCONFIL+IO2                                            
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA2                                                      
         CLC   SATLREC(SATLLID-SATLREC+L'SATLLID),OLDKEY                        
         BNE   DOXY                                                             
*                                                                               
         LA    R4,14                                                            
DO020    GOTO1 AIO,IOSQ+IOCONFIL+IO2                                            
         CLC   SATLREC(SATLLID-SATLREC+L'SATLLID),OLDKEY                        
         BNE   DOXY                                                             
         BCT   R4,DO020                                                         
*                                                                               
         CLC   SATLDEF,XFTODAY                                                  
         BNH   DOXN      TODAY/FUTURE DATE, CAN'T DEL, SO CAN'T ADD NEW         
*                                                                               
*                        DELETE THIS ONE, SO OK TO ADD NEW                      
         GOTO1 ASETACT,SATLREC                                                  
         OI    SATLSTAT,X'80'      SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    DOXY                                                             
         DC    H'0'                                                             
*                                                                               
DOXN     LTR   RB,RB               CC=NE, CAN'T ADD NEW REC                     
         B     XIT                                                              
DOXY     CR    R1,R1               CC=EQ, OKAY TO ADD NEW REC                   
DOX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CREATE A NEW PAGE FOR INPUT                                        
***********************************************************************         
NEWPAGE  NTR1                                                                   
* FIND THE LAST PAGE, R4=A(LAST PAGE), R8=LAST PAGE #                           
         LA    R3,DISTAB                                                        
         LR    R4,R3               R4=A(LAST PAGE)                              
         LA    R8,0                R8=PAGE #                                    
*                                                                               
         USING SATLMD,R3                                                        
NP020    LA    R1,NFILTER                                                       
NP025    ZIC   RE,SATLMLEN                                                      
         AR    R3,RE                                                            
         BCT   R1,NP025                                                         
*                                                                               
         CLC   0(2,R3),FFILL                                                    
         BE    NP040                                                            
         LR    R4,R3                                                            
         AHI   R8,1                                                             
         B     NP020                                                            
         DROP  R3                                                               
*                                                                               
* CHECK IF LAST PAGE IS EMPTY                                                   
         USING SATLMD,R4                                                        
NP040    LA    R1,NFILTER                                                       
NP045    CLI   SATLMDTA,0          EMPTY ELEMENT?                               
         BNE   NP060               NO                                           
         ZIC   RE,SATLMLEN                                                      
         AR    R3,RE                                                            
         BCT   R1,NP045                                                         
         DROP  R4                                                               
*                                  EMPTY PAGE, SO USE THIS ONE                  
         STC   R8,PAGENUM                                                       
         B     NP100                                                            
*                                                                               
* CHECK IF WE HAVE ROOM FOR A NEW PAGE                                          
NP060    LA    RE,NFILTER                                                       
         MHI   RE,L'EMTNTRY                                                     
         AR    RE,R3                                                            
         LA    RF,DISTAB                                                        
         SR    RE,RF                                                            
         CHI   RE,L'DISTAB                                                      
         BNH   NP080                                                            
*                                  CAN'T CREATE ANY NEW PAGE                    
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NPX                                                              
*                                  CREATE AN EMPTY PAGE                         
NP080    LA    R1,NFILTER                                                       
NP085    MVC   0(L'EMTNTRY,R3),EMTNTRY                                          
         AHI   R3,L'EMTNTRY                                                     
         BCT   R1,NP085                                                         
*                                                                               
         MVC   0(2,R3),FFILL       MARK THE END OF DISTAB                       
         AHI   R3,2                                                             
         LA    RE,DISTAB                                                        
         SR    R3,RE                                                            
         STH   R3,DISTABL                                                       
*                                                                               
         AHI   R8,1                                                             
         STC   R8,PAGENUM                                                       
         MVI   LASTPAGE,C'Y'                                                    
*                                                                               
NP100    LA    RE,SCRTYP1H         SET THE CURSOR TO 1ST TYPE                   
         ST    RE,APCURSOR                                                      
*                                                                               
NPXY     CR    R1,R1                                                            
NPX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A LIMIT ACCESS LIST RECORD                      @@*         
***********************************************************************         
DELREC   CLI   APPFKEY,0           ONLY DELETE ON ENTER KEY PRESS               
         BE    DEL020                                                           
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI#PETDE)                                           
         B     DELRECX                                                          
*                                                                               
DEL020   L     R2,AIOAREA1         R2=A(ACTION RECORD)                          
*                                                                               
         CLC   SATLDEF,XFTODAY                                                  
         BL    DEL040              FUTURE DATE - OK TO DELETE                   
*                                  CAN'T DELETE HISTORY RECORD                  
         MVC   FVMSGNO,=AL2(CE#NDREC)                                           
         B     DELRECX                                                          
*                                                                               
DEL040   GOTO1 ASETACT,SATLREC                                                  
*                                                                               
         OI    SATLSTAT,X'80'      SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RESTORE A DELETED LIMIT ACCESS LIST RECORD             @@*         
***********************************************************************         
         SPACE 1                                                                
RESREC   CLI   APPFKEY,0           ONLY RESTORE ON ENTER KEY PRESS              
         BE    RES020                                                           
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI#PETRE)                                           
         B     XIT                                                              
*                                                                               
RES020   L     R2,AIOAREA1                                                      
*                                                                               
         CLC   SATLDEF,XFTODAY                                                  
         BL    RES040              FUTURE DATE - OK TO RESTORE                  
*                                  CAN'T RESTORE HISTORY RECORD                 
         MVC   FVMSGNO,=AL2(CE#NRREC)                                           
         B     DELRECX                                                          
*                                                                               
RES040   GOTO1 ASETACT,SATLREC                                                  
         NI    SATLSTAT,X'FF'-X'80'   UNSET DELETE                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RESRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
VALSEL   LA    R2,APRECKEY         INITIALIZE RECORD KEY                        
         XC    APRECKEY,APRECKEY                                                
         MVI   SATLTYP,SATLTYPQ    RECORD TYPE IS C'F'X'21'                     
         MVI   SATLSUB,SATLSUBQ                                                 
         MVC   SATLAGY,TWAAGY      AGENCY ALPHA                                 
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         MVI   FVMINL,1            FIELD MUST HAVE I/P                          
         GOTO1 AFVAL,LISSYSH                                                    
         BNE   VALSELX                                                          
         GOTO1 AVALSYS                                                          
         BNE   VALSELX                                                          
         MVC   SATLSYS,APWORK                                                   
*                                                                               
         CLC   =C'ALL',LISDEF                                                   
         BNE   VS020                                                            
         XC    SELDEF,SELDEF                                                    
         B     VS060                                                            
*                                                                               
VS020    MVI   FVMINL,1            ANY EFFECTIVE DATE?                          
         GOTO1 AFVAL,LISDEFH                                                    
         BE    VS030                                                            
*                                  NOT GIVEN, USE TODAY'S DATE                  
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   XFTODAY,TODAY                                                    
         XC    XFTODAY,FFILL       1ST COMPLEMENT OF TODAY'S DATE               
         MVC   SELDEF,XFTODAY                                                   
         B     VS040                                                            
*                                                                               
VS030    ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   4(R1),PVRCONE                                                    
         BNE   VALSELX                                                          
         MVC   SELDEF,APWORK+PVALCSTA-PERVALD                                   
         XC    SELDEF,FFILL        1'S COMPLEMENT OF THE EFF DATE               
*                                                                               
VS040    XC    APWORK,APWORK       REDISPLAY EFF DATE                           
         MVC   APHALF,SELDEF                                                    
         XC    APHALF,FFILL                                                     
         GOTO1 VDATCON,APPARM,(2,APHALF),(8,APWORK)                             
         LA    R1,LISDEFH                                                       
         GOTO1 DISPFLD                                                          
*                                                                               
VS060    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LISLISTH                                                   
         BNE   VALSELY                                                          
         MVC   SATLLID,FVIFLD                                                   
*                                                                               
VALSELY  XC    IOKEY,IOKEY         SAVE THE KEY                                 
         MVC   SATLDEF,SELDEF                                                   
         MVC   IOKEY(L'SATLKEY),SATLKEY                                         
*                                                                               
         LA    R0,LISACT1H         START AT APPARM+1(4)                         
         ST    R0,APPARM                                                        
         MVI   APPARM+4,NLISLIN    # OF LINES AT APPARM+4(1)                    
         LA    RF,LISLINL          LINE LENGTH AT APPARM+6(2)                   
         STCM  RF,3,APPARM+6                                                    
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  B     XIT                                                              
*                                                                               
LISLINL  EQU   LISACT2H-LISACT1H                  L'LIST LINE                   
NLISLIN  EQU   ((LISACTXH-LISACT1H)/LISLINL)+1    # LIST LINES                  
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
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
GS050    CLC   APRECKEY(SATLLID-SATLKEY),SATLKEY                                
         BNE   GETSELN                                                          
         CLI   SATLSTAT,X'80'      NO DELETED ON LIST                           
         BE    GS040                                                            
*                                                                               
         OC    SELDEF,SELDEF                                                    
         BZ    GS070               DISPLAY ALL EFF DATES                        
         CLC   SATLDEF,SELDEF                                                   
         BL    GS040               DON'T DISPLAY FUTURE DATE                    
         TM    APINDS,APILNSEQ     TEST START OF NEW SCREEN                     
         BZ    GS070               NO                                           
         CLC   APRECKEY(SATLLID-SATLKEY+L'SATLLID),SATLKEY                      
         BE    GS040           DIS ONLY LATEST EFF DATE & SKIP OTHERS           
*                                                                               
GS070    MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   APRECKEY(L'SATLKEY),SATLKEY                                      
         B     GETSELX                                                          
         SPACE 1                                                                
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
         SPACE 1                                                                
GETSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
*                                                                               
         ICM   R4,15,APPARM                                                     
         USING LISTLIND,R4                                                      
         MVC   LSTNAME,SATLLID                                                  
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SADSCD,R3                                                        
         MVI   0(R3),SADSCELQ                                                   
         GOTO1 AGETELS,SATLREC                                                  
         ICM   R3,15,APPARM                                                     
         BZ    DISS020                                                          
*                                                                               
         ZIC   RF,SADSCLEN                                                      
         SH    RF,=H'3'                                                         
         BNP   DISS020                                                          
         EX    RF,*+8                                                           
         B     DISS020                                                          
         MVC   LSTDSC(0),SADSC                                                  
         DROP  R3                                                               
*                                                                               
DISS020  MVC   APHALF,SATLDEF                                                   
         XC    APHALF,FFILL                                                     
         GOTO1 VDATCON,APPARM,(2,APHALF),(8,LSTDEF)                             
*                                                                               
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
VALREP   DS    0H                                                               
VALREQX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE MESSAGE REPORT                                  *         
***********************************************************************         
PRTREP   DS    0H                                                               
         B     XIT                                                              
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
* LITERALS AND CONSTANTS                                             *          
**********************************************************************          
         SPACE 2                                                                
*&&US                                                                           
TYPETAB  EQU   *                                                                
TYTS1LN  DC    AL2(TYTS1X-*)                                                    
TYTSYSL  DC    X'4'                                                             
TYTSYS1  DC    CL8'SPOT'                                                        
TYTS1VL  DC    C'M',AL4(VTYM)                                                   
         DC    C'C',AL4(VTYC)                                                   
         DC    C'P',AL4(VTYP)                                                   
         DC    C'E',AL4(VTYE)                                                   
         DC    X'FF'                                                            
TYTS1X   EQU   *                                                                
*                                                                               
         DC    AL2(TYTS2X-*)                                                    
         DC    X'7'                                                             
TYTSYS2  DC    CL8'ACCOUNT'                                                     
         DC    C'M',AL4(VTYM)                                                   
         DC    C'C',AL4(VTYC)                                                   
         DC    C'P',AL4(VTYP)                                                   
         DC    C'O',AL4(VTYO)                                                   
         DC    X'FF'                                                            
TYTS2X   EQU   *                                                                
*                                                                               
         DC    AL2(0)              END OF THE TABLE                             
*&&                                                                             
*&&UK                                                                           
TYPETAB  EQU   *                   THIS PART TO BE CHANGED BY UK PRGR           
TYTS1LN  DC    AL2(TYTS1X-*)                                                    
TYTSYSL  DC    X'4'                                                             
TYTSYS1  DC    CL8'SPOT'                                                        
TYTS1VL  DC    C'M',AL4(VTYM)                                                   
         DC    C'C',AL4(VTYC)                                                   
         DC    C'P',AL4(VTYP)                                                   
         DC    C'E',AL4(VTYE)                                                   
         DC    X'FF'                                                            
TYTS1X   EQU   *                                                                
*                                                                               
         DC    AL2(0)              END OF THE TABLE                             
*&&                                                                             
         SPACE 2                                                                
GI#PETDE EQU   X'FF00'+24          PRESS ENTER TO DELETE                        
GI#PETRE EQU   X'FF00'+25          PRESS ENTER TO RESTORE                       
GI#EDATA EQU   X'FF00'+28          ENTER DATA                                   
CTFILE   DC    C'CTFILE '                                                       
SPACE    DC    80CL1' '                                                         
SAFETY   DC    Y(SAVAREAX-SAVAREA) SAVED STORAGE AREA                           
         SPACE 1                                                                
REPDESCL DCDD  CT#LAGL,30                                                       
NOTHING  DCDD  CT#NONE,5                                                        
         SPACE 1                                                                
DMOPEN   DC    C'DMOPEN'                                                        
         SPACE 1                                                                
* SEACSWRK                                                                      
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSWRK                                                       
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSECD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSBCD                                                       
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE SAVED IN TWA                                        *         
***********************************************************************         
         SPACE 1                                                                
MASKSD   DSECT                                                                  
SAVKEY   DS    XL25                COPY OF LIMIT ACCESS KEY                     
OLDKEY   DS    XL25                COPY OF LIMIT ACCESS KEY                     
*                                                                               
ERRFLAG  DS    X                                                                
ANYINP   DS    X                                                                
OLDDEF   DS    XL2                                                              
SELDEF   DS    XL2                                                              
PAGENUM  DS    X                                                                
COPYFLAG DS    X                                                                
LASTPAGE DS    C                                                                
*                                                                               
DISTABL  DS    H                   CURRENT LENGTH OF DISTAB                     
DISTAB   DS    XL2500                                                           
         DS    XL300               OVERFLOW AREA                                
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
HALF     DS    H                                                                
TODAY    DS    XL2                 TODAY'S DATE (COMPRESSED)                    
XFTODAY  DS    XL2                 TODAY'S DATE (COMPRESSED, 1ST COMP)          
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
LISTLIND DSECT                                                                  
         DS    XL8                 ACT HEADER                                   
LSTACT   DS    XL(L'LISACT1)                                                    
         DS    XL8                 ACT EXT                                      
         DS    XL8                 LINE HEADER                                  
LSTLINE  DS    0CL(L'LISLIN1)                                                   
LSTNAME  DS    XL3                 LIMIT ACCESS LIST NAME                       
         DS    XL2                                                              
LSTDSC   DS    XL30                DESCRIPTION                                  
         DS    XL8                                                              
LSTDEF   DS    XL8                 EFFECTIVE DATE                               
         ORG   LSTLINE+L'LSTLINE                                                
         DS    XL8                 LINE EXT                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SEACS17   12/26/02'                                      
         END                                                                    
