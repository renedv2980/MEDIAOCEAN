*          DATA SET DDZIP      AT LEVEL 051 AS OF 08/04/14                      
*PHASE ZIPA                                                                     
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DDWTO                                                                  
*INCLUDE SCANNER                                                                
*INCLUDR DCL                                                                    
*INCLUDE FAWLMJN                                                                
*INCLUDE FAWLMLV                                                                
         TITLE 'USE PKZIP TO COMPRESS/DECOMPRESS INFORMATION'                   
         PRINT NOGEN                                                            
         ENTRY WORKAREA                                                         
         ENTRY POSTERR                                                          
MAIN     START                                                                  
         NBASE 0,**DDZIP*,RA,WORK=VWRKAREA                                      
         L     R9,ACOMMON                                                       
         USING COMMON,R9           R9 = A(COMMONLY ADDRESSIBLE STORAGE)         
*                                                                               
         LR    RC,RD               GET SOME W/S FOR THIS MODULE                 
         USING WORKD,RC                                                         
         A     RD,=A((WORKL+7)/8*8)                                             
         XC    0(12,RD),0(RD)                                                   
         MVC   0(4,RD),=CL4'*ZIP'                                               
         MVC   4(4,RD),4(RC)                                                    
*                                                                               
         LR    R0,RC                                                            
         LR    R1,RD                                                            
         SR    R1,RC                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR W/S                                    
*                                                                               
         ST    R9,SAVER9           SAVE THESE FOR LATER                         
         ST    RD,SAVERD                                                        
*                                                                               
         BRAS  RE,INIT             INITIALISE PKZIP JOB                         
         BL    MAINX                                                            
*                                                                               
MAIN02   BRAS  RE,MOREWRK          SEE IF ANY MORE WORK HAS ARRIVED             
         BE    MAIN04              YES                                          
         BRAS  RE,WAIT             WAIT UNTIL POSTED TO ZIP                     
         BL    MAINX                                 OR TERMINATE               
*                                                                               
MAIN04   BRAS  RE,CYCLE            MAIN PROGRAM CYCLE                           
         B     MAIN02                                                           
*                                                                               
MAINX    BRAS  RE,UNTIEME          CLEAR ENTRY FROM FACPAK SSB                  
         CLOSE SYSPRINT            CLOSE PRINT OUTPUT                           
         B     XBASE                                                            
*                                                                               
ACOMMON  DC    A(COMMON)                                                        
VWRKAREA DC    V(WORKAREA)                                                      
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  ,                                                                
         OPEN  (SYSPRINT,OUTPUT)   INITIALISE PRINT OUTPUT                      
*                                                                               
         BRAS  RE,CARDIN           READ AND VALIDATE INPUT CARDS                
         BL    EXITL               ERROR                                        
*                                                                               
         BRAS  RE,GETSPC           BIND TO DATAMGR DATASPACE                    
         BRAS  RE,TIEMEUP                                                       
         BRAS  RE,SETOPS           SET UP OPERATOR COMMS                        
         BRAS  RE,SETENQ                                                        
*                                                                               
         LA    R0,4                MAKE NON-SWAPPABLE                           
         LNR   R0,R0                                                            
         SVC   247                                                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET WAIT UNTIL POSTED EITHER BY OPERATOR OR FACPAK                  *         
***********************************************************************         
WAIT     NTR1  ,                                                                
*                                                                               
WAIT02   BRAS  RE,GETSSB2          GET SSB INTO R2 AND SET ALET ETC             
         USING SSBD,R2                                                          
         MVI   SSBZSTAT,SSBZSSLP   SET SLEEPING                                 
         BRAS  RE,ARSOFF                                                        
         DROP  R2                                                               
*                                                                               
         LA    R1,ECBLST           BUILD ECBLIST                                
         MVC   0(4,R1),AOPERECB    OPERATOR ECB COMES FIRST                     
         LA    R1,4(R1)                                                         
         LA    RF,POSTECB                                                       
         ST    RF,0(R1)            NEXT COMES POST ECB                          
         OI    0(R1),X'80'         FLAG EOL                                     
         LA    R1,ECBLST                                                        
         WAIT  ECBLIST=(R1)                                                     
*                                                                               
         L     RF,AOPERECB         OPERATOR ECB POST MEANS TERMINATE            
         TM    0(RF),X'40'                                                      
         BZ    WAIT04                                                           
*                                                                               
         L     RF,ACOMM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BE    EXITL                                                            
*                                                                               
         CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         XR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         BCTR  R1,0                                                             
         MVC   CARD,SPACES         MOVE DATA TO WORK AND SCAN IT                
         MVC   CARD(0),CIBDATA                                                  
         EX    R1,*-6                                                           
*WLM     EX    R1,*+4                                                           
*WLM     MVC   CARD(0),CIBDATA                                                  
*                                                                               
         CLC   =C'EOJ',CARD        EOJ WILL END JOB                             
         BE    EXITL                                                            
*                                                                               
         MVC   PLINE(32),=C'INVALID KEYWORD               //'                   
         MVC   PLINE+16(13),CARD                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
*                                                                               
         L     RF,ACOMM            RESET OPER COMMS                             
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         BZ    EXITOK                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     WAIT02                                                           
         DROP  RF                                                               
*                                                                               
WAIT04   BRAS  RE,GETSSB2          GET SSB INTO R2 AND SET ALET ETC             
         USING SSBD,R2                                                          
         MVI   SSBZSTAT,SSBZSBSY   SET BUSY                                     
         BRAS  RE,ARSOFF                                                        
         DROP  R2                                                               
*                                                                               
         NI    POSTECB,255-X'40'   TURN OFF POST FLAG                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM CYCLE CALLED WHEN WAKE-UP POST SENT BY FACPAK          *         
***********************************************************************         
CYCLE    NTR1  ,                                                                
         BRAS  RE,GETTCB2          GET TCB IN FACPAK INTO R2                    
         SAM31 ,                                                                
         XR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         ICM   RF,15,2(R2)                                                      
         AHI   R2,6                                                             
         USING TCBD,R2                                                          
*                                                                               
         CPYA  AR3,AR2             SCAN TCB LOOKING FOR NEW WORK                
CYCLE04  ICM   R3,15,TCBAZIP                                                    
         USING ZIPHDRD,R3                                                       
         CLI   ZIPSTAT,ZIPSNEW     NEW WORK?                                    
         BE    CYCLE06                                                          
         BXLE  R2,RE,CYCLE04                                                    
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
*                                                                               
CYCLE06  ST    R2,THISTCB          SAVE A(TCB ENTRY)                            
         ST    R3,THISHDR          SAVE A(ENTRY)                                
         MVC   ETOKEN,ZIPETOKN     SAVE ENCLAVE TOKEN             WLM           
         MVC   HOLDTCB,00(R2)      SAVE TCB STRUCTURE -DEBUGGING  WLM           
         MVC   HOLDTCB$,=C'HOLDTCB$'    TCB STRUCTURE -DEBUGGING  WLM           
         MVI   ZIPSTAT,ZIPSPRC     SET PROCESSING                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,BEGJOIN          Join Enclave                   WLM           
         SAM24 ,                                                                
         DROP  R2,R3                                                            
*                                                                               
         CLI   WTLOG,C'Y'          WANT HARDCOPY OF WHAT ZIP DID?               
         BNE   CYCLE08             NO                                           
         TIME  DEC,TIMEIN,LINKAGE=SYSTEM                                        
*                                                                               
CYCLE08  BRAS  RE,ZIP                                                           
*                                                                               
         CLI   WTLOG,C'Y'          WANT HARDCOPY OF WHAT ZIP DID?               
         BNE   CYCLE10             NO                                           
         TIME  DEC,TIMEOUT,LINKAGE=SYSTEM                                       
*                                                                               
CYCLE10  TIME  TU                  SET TIME COMPLETED                           
         ST    R0,FULL                                                          
*                                                                               
         BRAS  RE,GETTCB2          CHECK TCB STILL VALID                        
         SAM31 ,                                                                
         ICM   R2,15,THISHDR                                                    
         USING ZIPHDRD,R2                                                       
         CLI   ZIPSTAT,ZIPSERR                                                  
         BE    *+8                                                              
         MVI   ZIPSTAT,ZIPSDON     SET COMPLETE                                 
         MVC   ZIPTIME,FULL        SET TIME COMPLETED                           
*                                                                               
         MVC   LCLNTRY,0(R2)       SAVE A COPY OF IT                            
         ICM   R3,15,ZIPECB        SET A(WAKEUP ECB) IN R3                      
         BRAS  RE,ARSOFF                                                        
         SAM24 ,                                                                
*                                                                               
         CLI   WTLOG,C'Y'          WANT HARDCOPY OF WHAT ZIP DID?               
         BNE   *+8                 NO                                           
         BRAS  RE,LOGGER           OUTPUT DEBUG MESSAGE                         
*                                                                               
         SAM31 ,                   Switch To 31 Bit Mode          WLM           
         BRAS  RE,ENDJOIN          Leave Enclave                  WLM           
         SAM24 ,                   Switch 31 Bit Off              WLM           
*                                                                               
         XR    R4,R4               R4 = A(ASCB OR ZERO)                         
CYCLE12  LH    R5,FASID            GET ASCB FOR FACPAK                          
         LOCASCB ASID=(R5)                                                      
         LTR   RF,RF               ASCB RETURNED IN R1 OK?                      
         BNZ   NOASCB                                                           
*                                                                               
         LR    R4,R1               SET ASCB IN R4                               
         USING ASCB,R4                                                          
         CLC   ASCBASCB,=C'ASCB'   IS IT STILL AN ASCB?                         
         BNE   NOASCB                                                           
*                                                                               
         L     R6,=V(POSTERR)                                                   
         POST  (R3),99,ASCB=(R4),LINKAGE=SYSTEM,ECBKEY=8,ERRET=(R6)             
*                                                                               
         CHI   RF,0                IF NO ERRORS                                 
         BE    EXITOK              THEN EXIT                                    
*                                                                               
         STC   RF,BYTE                                                          
         GOTO1 VHEXOUT,DMCB,BYTE,NZRCCC,1,0                                     
         WTO   TEXT=NZRCH,MCSFLAG=HRDCPY                                        
         B     EXITOK                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ASCB ERROR HANDLING ROUTINE - ASSUME FACPAK HAS ABENDED             *         
***********************************************************************         
NOASCB   BRAS  RE,ARSOFF                                                        
         SAM24 ,                                                                
*                                                                               
         MVC   NOAERR,NQASID                                                    
         CVD   RF,DUB             OUTPUT RC IN RF                               
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  NOARC,DUB                                                        
*                                                                               
X        USING ZIPHDRD,LCLNTRY                                                  
         GOTO1 VHEXOUT,DMCB,X.ZIPECB,NOAECB,4,0                                 
         MVC   NOABLK(ZIPLENQ),LCLNTRY                                          
         DROP  X                                                                
*                                                                               
         XR    R0,R0                                                            
         WTO   TEXT=((NOASCB1H,D),(NOASCB2H,D),(0,E))                           
*                                                                               
         BRAS  RE,UNTIEME                                                       
         ABEND 921,DUMP                                                         
         EJECT                                                                  
***********************************************************************         
* CHECK TO SEE IF ANY MORE NEW WORK IS WAITING                        *         
***********************************************************************         
MOREWRK  NTR1  ,                                                                
         BRAS  RE,GETTCB2                                                       
         SAM31 ,                                                                
         XR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         ICM   RF,15,2(R2)                                                      
         AHI   R2,6                                                             
         USING TCBD,R2                                                          
*                                                                               
         CPYA  AR3,AR2                                                          
MWRK02   ICM   R3,15,TCBAZIP                                                    
         USING ZIPHDRD,R3                                                       
         CLI   ZIPSTAT,ZIPSNEW     NEW WORK?                                    
         BE    MWRK04              YES                                          
         BXLE  R2,RE,MWRK02                                                     
*                                                                               
         BRAS  RE,GETSSB2          GET SSB                                      
         USING SSBD,R2                                                          
         MVI   SSBZSTAT,SSBZSSLP   SET SLEEPING                                 
         BRAS  RE,ARSOFF                                                        
         B     EXITL               CC LOW = NO NEW WORK                         
*                                                                               
MWRK04   BRAS  RE,ARSOFF                                                        
         B     EXITOK              CC EQUAL = NEW WORK                          
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
SETOPS   NTR1  ,                                                                
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP ENQUEUE SO ADV/REP can not come down until released.         *         
***********************************************************************         
SETENQ   NTR1  ,                                                                
         MVC   SYSDSPCE,DSPACE+3                                                
         MVC   SYSFAC,NFAC                                                      
         LA    R1,L'SYSMIN-1                                                    
         LA    RE,SYSMIN+L'SYSMIN-1                                             
SETENQ2  CLI   0(RE),C' '                                                       
         BH    SETENQ4                                                          
         SHI   RE,1                                                             
         BCT   R1,SETENQ2                                                       
         DC    H'00'                                                            
*                                                                               
SETENQ4  AHI   RE,1                Bump up to blank                             
         AHI   R1,1                                                             
         OC    AOR,AOR             Is this set?                                 
         BZ    SETENQ6                                                          
         AHI   R1,1                add one back                                 
         MVC   0(1,RE),AOR+3                                                    
         OI    0(RE),X'C0'         This is for AORA to AORI (1 to 9)            
*                                                                               
SETENQ6  STC   R1,SYSMINL          Reset length                                 
         ISGENQ REQUEST=OBTAIN,QNAME=SYSMAJ,RNAME=SYSMIN,              +        
               RNAMELEN=SYSMINL,SCOPE=SYSTEM,CONTROL=EXCLUSIVE,        +        
               ENQTOKEN=TOKNTOKN,RESLIST=NO,RNL=NO,COND=YES,           +        
               CONTENTIONACT=WAIT,WAITTYPE=SUSPEND,RETCODE=15,RSNCODE=0         
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         CIJNE RF,4,*+2            ISGENQRC_WARN                                
         NILH  GR0,X'0000'                                                      
         CHI   R0,ISGENQRSN_UNPROTECTEDQNAME                                    
         BE    EXITOK              WE GOT IT                                    
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
PRINTL   NTR1  ,                                                                
         PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ AND VALIDATE INPUT CARDS                                       *         
***********************************************************************         
CARDIN   NTR1  ,                                                                
CRD02    GOTO1 VCARDS,DMCB,PLINE+1,=C'RE00'                                     
         MVC   CARD,PLINE+1                                                     
         CLC   =C'/*',CARD         END OF CARDS?                                
         BE    EXITOK              YES - RETURN NORMALLY                        
*                                                                               
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
         BRAS  RE,VALCARD          VALIDATE PARAMETER CARD                      
         B     CRD02                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT CARD                                                 *         
***********************************************************************         
VALCARD  NTR1  ,                                                                
         CLI   CARD,C'*'           COMMENT?                                     
         BE    EXITOK              YES                                          
*                                                                               
         GOTO1 VSCAN31,DMCB,CARD,SCNBLK,0,(1,SCICARD),20                        
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         L     R3,ACARDTAB                                                      
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
VCRD02   CLI   CNAME,CARDEOT       EOT?                                         
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,CARDCMP                                                       
         BE    VCRD04                                                           
         AHI   R3,CARDTABL                                                      
         B     VCRD02                                                           
*                                                                               
CARDCMP  CLC   SC1STFLD(0),CNAME                                                
*                                                                               
VCRD04   CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   VCRD06              NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
VCRD06   CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   VCRD08              NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         MVC   0(0,RE),SC2NDFLD                                                 
         EX    RF,*-6                                                           
*WLM     EX    RF,*+4                                                           
*WLM     MVC   0(0,RE),SC2NDFLD                                                 
         B     EXITOK                                                           
*                                                                               
VCRD08   DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,CINVLIN          INVALID LINE                                 
         B     CERR                                                             
CEINVKEY LA    R1,CINVKEY          INVALID KEYWORD                              
         B     CERR                                                             
CENOTNUM LA    R1,CNOTNUM          NOT A NUMBER                                 
         B     CERR                                                             
CENOTCHR LA    R1,CNOTCHR          NOT CHARACTER                                
         B     CERR                                                             
CETOOSHT LA    R1,CTOOSHT          TOO SHORT                                    
         B     CERR                                                             
CETOOLNG LA    R1,CTOOLNG          TOO LONG                                     
         B     CERR                                                             
CETOOLOW LA    R1,CTOOLOW          TOO SMALL                                    
         B     CERR                                                             
CETOOBIG LA    R1,CTOOBIG          TOO BIG                                      
         B     CERR                                                             
CENOINP  LA    R1,CNOINP           NO INPUT                                     
         B     CERR                                                             
*                                                                               
CERR     MVC   PLINE,SPACES                                                     
         MVC   PLINE(CERRHDRL),CERRHDR                                          
         MVC   PLINE+CERRHDRL(CERRMSGL),0(R1)                                   
         BRAS  RE,PRINTL                                                        
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* GET REQUESTED DATASPACE                                             *         
***********************************************************************         
GETSPC   NTR1  ,                                                                
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   OFFS,WORK+20        EXTRACT VALUES                               
         MVC   ALET,WORK+24                                                     
         MVC   STOKN,WORK+28                                                    
*                                                                               
         OC    ALET,ALET           CHECK WE GOT AN ALET OK                      
         BNZ   EXITOK                                                           
         LA    R1,ERR2                                                          
         B     DIE                                                              
         EJECT                                                                  
***********************************************************************         
* BIND TO CORRECT FACPAK                                              *         
***********************************************************************         
TIEMEUP  NTR1  ,                                                                
         LA    R2,FULL                                                          
         EXTRACT RESULTS,'S',FIELDS=(TIOT,ASID)                                 
*                                                                               
         L     RF,RXTIOT                                                        
         MVC   JOBNAME,0(RF)       GET JOBNAME FOR THIS IMAGE                   
         L     RF,RXASID                                                        
         STH   RF,ASIDFLD          GET ASID FOR THIS IMAGE                      
*                                                                               
         USING FACIDD,RF                                                        
         LA    RF,FACIDTAB                                                      
TIE01    CLI   FACIDSPC,X'FF'      End of table                                 
         JE    *+2                 Death                                        
         CLC   FACIDSPC,DSPACE+3                                                
         JE    TIE01B              Found them                                   
         AHI   RF,FACIDLNQ                                                      
         J     TIE01                                                            
*                                                                               
TIE01B   L     RF,FACAID           FACPAK=                                      
         USING FACITABD,RF                                                      
         XR    R0,R0                                                            
TIE02    CLI   FACISN4,X'FF'                                                    
         BE    TIE04                                                            
         CLC   FACISN4,NFAC        MATCH FACPAK NAME                            
         BE    TIE06                                                            
         AHI   RF,L'FACITAB                                                     
         B     TIE02                                                            
         DROP  RF                                                               
*                                                                               
TIE04    LA    R1,ERR1             INVALID FACPAK MESSAGE                       
         B     DIE                                                              
***********************************************************************         
* SBFACMAX - Max number of AOR (includes TOR)                         *         
* SBEXCHLQ - Table entry length (row) for each                        *         
* TORFACLQ - TOR information area length                              *         
* FACIDMAX - Max number of TOR regions per dataspace                  *         
***********************************************************************         
TIE06    LA    R0,FACIDMAX         Max number of TORs                           
         L     R1,=A((SBFACMAX*SBEXCHLQ)+TORFACLQ+6)                            
*                                                                               
         LAM   AR2,AR2,ALET        GET A(TOR)                                   
         L     R2,OFFS                                                          
         SAC   512                                                              
         ICM   R2,15,DHATOR-DMDSHDR(R2)                                         
*                                                                               
         USING TORFACD,R2                                                       
TIE10    OC    TOREYE(4),TOREYE                                                 
         JZ    TIE18                                                            
         CLC   TOREYE(4),=C'TOR-'                                               
         JNE   TIE19                                                            
*                                                                               
TIE12    CLC   NFAC,TOREYE+4       Is this the right one?                       
         JE    TIE20                                                            
*                                                                               
TIE18    AR    R2,R1               Next TOR please                              
         JCT   R0,TIE10                                                         
         LA    R1,ERR6             FACPAK not found                             
         J     DIE                                                              
*                                                                               
TIE19    LA    R1,ERR5             Eye catcher missing                          
         J     DIE                                                              
*                                                                               
TIE20    ST    R2,ADSTOR           SET A(TOR) FOR THIS FACPAK NAME              
         AHI   R2,TORFACLQ                                                      
         L     R0,AOR                                                           
         MH    R0,0(,R2)           INDEX INTO BLOCK                             
         AR    R2,R0                                                            
         AHI   R2,6                                                             
         USING SBEXCHD,R2                                                       
         ST    R2,ADSFAC                                                        
         OC    SBSTOKEN,SBSTOKEN                                                
         BNZ   *+12                                                             
         LA    R1,ERR3                                                          
         B     DIE                                                              
*                                                                               
         MVC   FSTOKN,SBSTOKEN     GET FACPAK STOKEN LOCALLY                    
         MVC   FASID,SBASID                                                     
         MVC   FSSB,SBSSB                                                       
         MVC   FTCB,SBTCB                                                       
         BRAS  RE,ARSOFF                                                        
*                                                                               
         XC    WORK,WORK           SVC 247 NEEDS CHANGING                       
         MVC   WORK+00(04),=C'PALE'                                             
         MVC   WORK+04(12),=C'TESTDATAMGRT'                                     
         MVC   WORK+28(08),FSTOKN                                               
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         LA    R1,ERR4                                                          
         B     DIE                                                              
*                                                                               
         MVC   FOFFS,WORK+20        EXTRACT VALUES                              
         MVC   FALET,WORK+24                                                    
*                                                                               
         BRAS  RE,GETSSB2           SET VALUES IN SSB FOR OUR FACPAK            
         USING SSBD,R2                                                          
         LA    R0,POSTECB                                                       
         STCM  R0,15,SSBZECB                                                    
         MVC   SSBZASID,ASIDFLD                                                 
         MVI   SSBZSTAT,SSBZSSLP                                                
         MVC   FACNAME,SSBSYSN4                                                 
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CLEAR SELF FROM FACPAK                                              *         
***********************************************************************         
UNTIEME  NTR1  ,                                                                
         BRAS  RE,ARSOFF                                                        
         SAM24 ,                                                                
*                                                                               
         LH    R5,FASID                                                         
         LOCASCB ASID=(R5)         USE ASID TO GET ASCB                         
         LTR   RF,RF                                                            
         BNZ   EXITOK                                                           
*                                                                               
         LR    R4,R1                                                            
         USING ASCB,R4             MAKE SURE ASCB IS OURS                       
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   EXITOK                                                           
         DROP  R4                                                               
*                                                                               
         BRAS  RE,GETSSB2          GET SSB                                      
         USING SSBD,R2                                                          
         XC    SSBZIP,SSBZIP       CLEAR ZIP INFO                               
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOG PKZIP WORK TO MSGLOG                                            *         
***********************************************************************         
LOGGER   NTR1  ,                                                                
         UNPK  WORK(15),TIMEIN     TIME IN                                      
         MVC   MSGTIN+00(2),WORK+00                                             
         MVI   MSGTIN+02,C':'                                                   
         MVC   MSGTIN+03(2),WORK+02                                             
         MVI   MSGTIN+05,C':'                                                   
         MVC   MSGTIN+06(2),WORK+04                                             
         MVI   MSGTIN+08,C'.'                                                   
         MVC   MSGTIN+09(6),WORK+06                                             
*                                                                               
         UNPK  WORK(15),TIMEOUT    TIME OUT                                     
         MVC   MSGTOUT+00(2),WORK+00                                            
         MVI   MSGTOUT+02,C':'                                                  
         MVC   MSGTOUT+03(2),WORK+02                                            
         MVI   MSGTOUT+05,C':'                                                  
         MVC   MSGTOUT+06(2),WORK+04                                            
         MVI   MSGTOUT+08,C'.'                                                  
         MVC   MSGTOUT+09(6),WORK+06                                            
*                                                                               
         MVI   TIMEIN+L'TIMEIN-1,X'0F'                                          
         MVI   TIMEOUT+L'TIMEOUT-1,X'0F'                                        
         SP    TIMEOUT,TIMEIN                                                   
*                                                                               
         UNPK  WORK(15),TIMEOUT    PROCESSING TIME                              
         MVC   MSGPTIM+00(2),WORK+00                                            
         MVI   MSGPTIM+02,C':'                                                  
         MVC   MSGPTIM+03(2),WORK+02                                            
         MVI   MSGPTIM+05,C':'                                                  
         MVC   MSGPTIM+06(2),WORK+04                                            
         MVI   MSGPTIM+08,C'.'                                                  
         MVC   MSGPTIM+09(6),WORK+06                                            
*                                                                               
         MVC   MSGFAC,FACNAME      SYSTEM NAME FOR HEADER                       
*                                                                               
         LA    R2,LCLNTRY                                                       
         USING ZIPHDRD,R2                                                       
*                                                                               
         MVC   MSGACT,UNKNOWN      ACTION FOR HEADER                            
         CLI   ZIPACTN,ZIPACMP                                                  
         BNE   *+10                                                             
         MVC   MSGACT,CMPRS                                                     
         CLI   ZIPACTN,ZIPAUNC                                                  
         BNE   *+10                                                             
         MVC   MSGACT,UNCMPRS                                                   
*                                                                               
         EDIT  (B4,ZIPULEN),MSGIN,0,ALIGN=LEFT,ZERO=NOBLANK                     
         EDIT  (B4,ZIPCLEN),MSGOUT,0,ALIGN=LEFT,ZERO=NOBLANK                    
*                                                                               
         MVC   MSGCMP,SPACES       COMPRESSION RATIO                            
         L     RF,ZIPULEN          UNCOMPRESSED LENGTH                          
         CVD   RF,DUB                                                           
         ZAP   UNCPCK,DUB                                                       
         L     RF,ZIPCLEN          COMPRESSED LENGTH                            
         CVD   RF,DUB                                                           
         ZAP   CMPPCK,DUB                                                       
*                                                                               
         ZAP   DIVPCK,CMPPCK       COMPRESS LENGTH * 100.00                     
         MP    DIVPCK,=P'10000'                                                 
         DP    DIVPCK,UNCPCK                                                    
         ZAP   PCTPCK,=P'10000'                                                 
         SP    PCTPCK,DIVPCK(8)   100.00-(% OF ORIGINAL SIZE)                   
         EDIT  PCTPCK,MSGCMP,2,ALIGN=LEFT,ZERO=NOBLANK                          
         LA    R1,MSGCMP                                                        
         AR    R1,R0                                                            
         MVI   0(R1),C'%'                                                       
*                                                                               
         XR    R0,R0                HAVE TO CLEAR FOR MULTILINE WTO             
         WTO   TEXT=((MSGHL,C),(M1L,D),(M2L,D),(M3L,D),(0,E)),         +        
               MCSFLAG=HRDCPY                                                   
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO PKZIP FOR STREAM DATA                                  *         
***********************************************************************         
ZIP      NTR1  ,                                                                
         BRAS  RE,GETTCB2                                                       
         SAM31 ,                                                                
         L     R2,THISHDR                                                       
         USING ZIPHDRD,R2                                                       
         CLI   ZIPACTN,ZIPACMP     REQUEST COMPRESSION?                         
         BE    ZIPUP               YES                                          
         CLI   ZIPACTN,ZIPAUNC     REQUEST DECOMPRESSION?                       
         BE    ZIPDOWN             YES                                          
*                                                                               
         MVC   BADHDR,ZIPACTN      SET BAD CALL ACTION                          
         MVI   ZIPSTAT,ZIPSERR     SET ERROR                                    
         BRAS  RE,ARSOFF                                                        
         WTO   TEXT=BADHDRH                                                     
         B     ZIPX                                                             
*                                                                               
ZIPUP    BRAS  RE,IMPINIT          INITIALISE PARAMETERS TO IMPLODE             
         BRAS  RE,ARSOFF           CLEAR ALL FOR CALL TO IMPLODE                
         SAM24 ,                                                                
*                                                                               
         ST    RD,ZIPSRD           SAVE RD CHAIN                                
         LA    RD,PZREGS           GIVE ZIP A 72 BYTE SAVEAREA                  
         GOTO1 VIMPLODE,PLIST                                                   
         L     RD,ZIPSRD           RESTORE RD CHAIN                             
         LTR   RF,RF                                                            
         BZ    ZIPX                ZERO RETURN IS OK                            
*                                                                               
         BRAS  RE,ARSOFF           RESET ALL THESE                              
         SAM24 ,                                                                
*                                                                               
         STC   RF,BYTE                                                          
         GOTO1 VHEXOUT,DMCB,BYTE,BADCMP,1,0                                     
         WTO   TEXT=BADCMPH        SET BAD ZIP ERROR MESSAGE                    
*                                                                               
         SAM31 ,                                                                
         BRAS  RE,GETTCB2                                                       
         L     R2,THISHDR                                                       
         USING ZIPHDRD,R2                                                       
         MVI   ZIPSTAT,ZIPSERR     SET ERROR FOR ZIP JOB                        
         B     ZIPX                                                             
*                                                                               
ZIPDOWN  BRAS  RE,EXPINIT          INITIALISE PARAMETERS TO EXPLODE             
         BRAS  RE,ARSOFF                                                        
         SAM24 ,                                                                
*                                                                               
         ST    RD,ZIPSRD           SAVE RD CHAIN                                
         LA    RD,PZREGS           GIVE ZIP A 72 BYTE SAVEAREA                  
         GOTO1 VEXPLODE,PLIST                                                   
         L     RD,ZIPSRD           RESTORE RD CHAIN                             
         LTR   RF,RF                                                            
         BZ    ZIPX                ZERO RETURN IS OK                            
         DC    H'0'                                                             
*                                                                               
ZIPX     BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE PARAMETERS TO IMPLODE FOR COMPRESSION OF STREAM DATA     *         
***********************************************************************         
         USING ZIPHDRD,R2                                                       
IMPINIT  NTR1  ,                                                                
         XC    PKREAD,PKREAD       PARAMETERS FOR READ                          
         OI    PKREAD,X'80'        DIRECT ADDRESS MODE                          
         MVC   PKREAD+4(4),AIMPREAD                                             
*                                                                               
         XC    PKWRITE,PKWRITE     PARAMETERS FOR WRITE                         
         OI    PKWRITE,X'80'       DIRECT ADDRESS MODE                          
         MVC   PKWRITE+4(4),AIMPWRTE                                            
*                                                                               
         LHI   RF,2048             DICTIONARY SIZE (1024,2048 OR 4096)          
         TM    ZIPFDETL,ZIPDSML    OVERRIDE DICTIONARY SIZE?                    
         BZ    *+8                                                              
         LHI   RF,1024             DICTIONARY SIZE 1024                         
         TM    ZIPFDETL,ZIPDBIG    OVERRIDE DICTIONARY SIZE?                    
         BZ    *+8                                                              
         LHI   RF,4096             DICTIONARY SIZE 4096                         
         ST    RF,DICTPARM                                                      
*                                                                               
         LA    RF,IMPLBNRY         BINARY COMPRESSION DEFAULT                   
         TM    ZIPFDETL,ZIPDASC    ASCII COMPRESSION REQUEST?                   
         BZ    *+8                                                              
         LA    RF,IMPLASCI         ASCII  COMPRESSION                           
         ST    RF,COMPPARM                                                      
*                                                                               
         LA    R3,PLIST                                                         
         USING IMPLODDS,R3                                                      
         LA    RF,PKREAD                                                        
         ST    RF,IMPLRDFH       P1  SET A(PARAMETERS FOR READ)                 
         ST    RC,IMPLRDDT       P2  SET A(WORK AREA FOR READ)                  
         LA    RF,PKWRITE                                                       
         ST    RF,IMPLWRFH       P3  SET A(PARAMETERS FOR WRITE)                
         ST    RC,IMPLWRDT       P4  SET A(WORK AREA FOR WRITE)                 
*                                                                               
         LA    RF,PZBUFF                                                        
         ST    RF,IMPLWKBF       P5  SET A(64K BUFFER FOR ZIP W/S)              
         LA    RF,DICTPARM                                                      
         ST    RF,IMPLDCSZ       P6  SET A(DICTIONARY SIZE)                     
         LA    RF,COMPPARM                                                      
         ST    RF,IMPLMODE       P7  SET A(COMPRESSION MODE)                    
         XC    IMPLFHPL,IMPLFHPL P8                                             
         XC    IMPLCRC,IMPLCRC   P9                                             
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE PARAMETERS TO EXPLODE FOR DECOMPRESSION OF STREAM DATA   *         
***********************************************************************         
EXPINIT  NTR1  ,                                                                
         USING ZIPHDRD,R2                                                       
         XC    PKREAD,PKREAD       PARAMETERS FOR READ                          
         OI    PKREAD,X'80'        DIRECT ADDRESS MODE                          
         L     RF,AEXPREAD                                                      
         ST    RF,PKREAD+4                                                      
*                                                                               
         XC    PKWRITE,PKWRITE     PARAMETERS FOR WRITE                         
         OI    PKWRITE,X'80'       DIRECT ADDRESS MODE                          
         L     RF,AEXPWRTE                                                      
         ST    RF,PKWRITE+4                                                     
*                                                                               
         LHI   RF,2048             DICTIONARY SIZE (1024,2048 OR 4096)          
         TM    ZIPFDETL,ZIPDSML    OVERRIDE DICTIONARY SIZE?                    
         BZ    *+8                                                              
         LHI   RF,1024             DICTIONARY SIZE 1024                         
         TM    ZIPFDETL,ZIPDBIG    OVERRIDE DICTIONARY SIZE?                    
         BZ    *+8                                                              
         LHI   RF,4096             DICTIONARY SIZE 4096                         
         ST    RF,DICTPARM                                                      
*                                                                               
         LA    RF,IMPLBNRY         BINARY COMPRESSION DEFAULT                   
         TM    ZIPFDETL,ZIPDASC    ASCII COMPRESSION REQUEST?                   
         BZ    *+8                                                              
         LA    RF,IMPLASCI         ASCII  COMPRESSION                           
         ST    RF,COMPPARM                                                      
*                                                                               
         LA    R3,PLIST                                                         
         USING EXPLODDS,R3                                                      
         LA    RF,PKREAD                                                        
         ST    RF,EXPLRDFH       P1  SET A(PARAMETERS FOR READ)                 
         ST    RC,EXPLRDDT       P2  SET A(WORK AREA FOR READ)                  
         LA    RF,PKWRITE                                                       
         ST    RF,EXPLWRFH       P3  SET A(PARAMETERS FOR WRITE)                
         ST    RC,EXPLWRDT       P4  SET A(WORK AREA FOR WRITE)                 
                                                                                
         LA    RF,PZBUFF                                                        
         ST    RF,EXPLWKBF       P5  SET A(64K BUFFER FOR ZIP W/S)              
         XC    EXPLFHPL,EXPLFHPL P6                                             
         XC    EXPLCRC,EXPLCRC   P7                                             
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         LTORG                      Generte Local Litteral Pool    WLM          
         EJECT                                                                  
***********************************************************************         
* JOIN WLM ENCLAVE ROUTINE                                            *         
***********************************************************************         
         USING TCBD,R2                                             WLM          
         USING WLMED,R1                                            WLM          
BEGJOIN  NTR1  ,                                                   WLM          
         LA    R1,ELCVPARM          Enclave Work Area              WLM          
         XC    ELCVPARM(ECLVWNQ),ELCVPARM                          WLM          
         MVC   ECLVTOKEN,ETOKEN     Pass Enclave Token             WLM          
         XC    FWORD,FWORD                                                      
         MVC   FWORD+2(1),TCBPRG                                                
         ICM   R0,15,FWORD                                                      
         CVD   R0,BYTES8                                                        
         UNPK  BYTES8(5),BYTES8+5(3)                                            
         MVC   ECLPRG,BYTES8+1      PROGRAM IDENTIFIER             WLM          
         GOTO1 VWLMJN,ELCVPARM,C'WLZJ'                             WLM          
         B     EXITOK               Return to caller               WLM          
         DROP  R1,R2                WLMED, TCBD                    WLM          
         EJECT                                                                  
***********************************************************************         
* LEAVE WLM ENCLAVE ROUTINE                                           *         
***********************************************************************         
         USING WLMED,R1                                            WLM          
ENDJOIN  NTR1  ,                    Preserve Callers Registers     WLM          
         LA    R1,ELCVPARM          Enclave Work Area              WLM          
         GOTO1 VWLMLV,ELCVPARM,C'WLML'                             WLM          
         B     EXITOK               Return to caller               WLM          
         DROP  R1                   WLMED                          WLM          
         EJECT                                                                  
***********************************************************************         
* COMMON ROUTINES AND LITERALS                                        *         
***********************************************************************         
         DROP  RA,RB                                                            
         ORG   MAIN+X'2000'        TEMP TO GET RA OUT OF THE PICTURE            
COMMON   DS    0D                                                               
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
GETSSB2  LAM   AR0,ARF,ARZERO      GET A(SSB)                                   
         LAM   AR2,AR2,FALET                                                    
         L     R2,FSSB                                                          
         SAC   512                                                              
         BR    RE                                                               
*                                                                               
GETTCB2  LAM   AR0,ARF,ARZERO      GET A(TCB)                                   
         LAM   AR2,AR2,FALET                                                    
         L     R2,FTCB                                                          
         SAC   512                                                              
         BR    RE                                                               
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD                                                        
         XBASE ,                                                                
*                                                                               
DIE      SAC   0                   OUTPUT ERROR MESSAGE                         
         MVC   ERRMSG,0(R1)        AND THEN ABEND                               
         WTO   TEXT=ERRMSGH                                                     
         ABEND 911,DUMP                                                         
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
ARZERO   DC    16F'0'                                                           
*                                                                               
         DC    CL8'CMPBUFF '                                                    
VCMPBUFF DC    V(CMPBUFF)                                                       
         DC    F'0'                                                             
         DC    CL8'UNCBUFF '                                                    
VUNCBUFF DC    V(UNCBUFF)                                                       
         DC    F'0'                                                             
*                                                                               
         DC    CL8'EXTRACTR'                                                    
RESULTS  DS    0F                  RESULTS FROM EXTRACT MACRO                   
RXTIOT   DC    F'0'                                                             
RXASID   DC    F'0'                                                             
*                                                                               
         DC    CL8'JOBNAME '                                                    
JOBNAME  DC    CL8' '              MVS JOBNAME FOR THIS IMAGE                   
*                                                                               
         DC    CL8'ASIDFLD '                                                    
ASIDFLD  DC    H'0'                ASID FOR THIS IMAGE                          
         DC    6X'00'                                                           
POSTECB  DC    A(0)                WAKEUP ECB FOR THIS IMAGE                    
         DC    F'0'                                                             
         DC    CL8'AOPERECB'                                                    
AOPERECB DC    A(0)                OPERATOR ECB FOR THIS IMAGE                  
         DC    F'0'                                                             
         DC    CL8'ACOMM   '                                                    
ACOMM    DC    A(0)                COMMS BLOCK FOR THIS IMAGE                   
         DC    F'0'                                                             
         DC    CL8'ECBLST  '       ECBLIST                                      
ECBLST   DC    4F'0'                                                            
*                                                                               
SPACES   DC    CL166' '                                                         
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
MAXLINE  DC    P'60'                                                            
*                                                                               
ERRMSGH  DC    AL2(40)                                                          
ERRMSG   DC    CL40' '                                                          
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
ACARDTAB DC    A(CARDTAB)                                                       
VCARDS   DC    V(CARDS)                                                         
VSCAN31  DC    V(SCAN31)              updated SCANNER                           
VHEXOUT  DC    V(HEXOUT)                                                        
*                                                                               
VIMPLODE DC    V(IMPLODE)                                                       
VEXPLODE DC    V(EXPLODE)                                                       
AIMPREAD DC    A(IMPREAD)                                                       
AEXPREAD DC    A(EXPREAD)                                                       
AIMPWRTE DC    A(IMPWRTE)                                                       
AEXPWRTE DC    A(EXPWRTE)                                                       
VWLMJN   DC    V(FAWLMJN)                                         WLM           
VWLMLV   DC    V(FAWLMLV)                                         WLM           
         EJECT                                                                  
***********************************************************************         
* ERROR LOGGING OUTPUT BLOCK                                          *         
***********************************************************************         
NOASCB1H DC    AL2(70)                                                          
NOASCB1  DC    CL70' '                                                          
         ORG   NOASCB1                                                          
NOAERR   DC    CL14'              '                                             
NOARC    DC    CL02'  '                                                         
         DC    CL06',ASID='                                                     
NOAASID  DC    CL04'    '                                                       
         DC    CL5',ECB='                                                       
NOAECB   DC    CL8'        '                                                    
         ORG                                                                    
NOASCB2H DC    AL2(70)                                                          
NOASCB2  DC    CL70' '                                                          
         ORG   NOASCB2                                                          
         DC    CL06'Block='                                                     
NOABLK   DC    CL32' '                                                          
         ORG                                                                    
*                                                                               
NQASID   DC    CL14'ASCB error RC='                                             
NQPOST   DC    CL14'Post error RC='                                             
*                                                                               
BADCMPH  DC    AL2(45)                                                          
         DC    CL45' '                                                          
         ORG   BADCMPH                                                          
         DC    C'Bad completion from Compress - RF='                            
BADCMP   DC    CL2' '                                                           
*                                                                               
BADHDRH  DC    AL2(30)                                                          
         DC    CL30' '                                                          
         ORG   BADHDRH                                                          
         DC    C'Bad Zip command - '                                            
BADHDR   DC    CL1' '                                                           
         ORG                                                                    
*                                                                               
NZRCH    DC    AL2(35)                                                          
NZRC     DC    CL35' '                                                          
         ORG   NZRC                                                             
         DC    C'Non-zero POST return - RC='                                    
NZRCCC   DS    CL2'??'                                                          
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED BOOKS                                                *         
***********************************************************************         
*FACIDTAB                                                                       
         PRINT OFF                                                              
       ++INCLUDE FACIDTABL                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
CARDTAB  DS    0D                                                               
         DC    CL8'DSPACE  ',F'001',F'012'                                      
         DC    X'05',AL1(CTCHR),AL1(L'DSPACE),AL1(0),AL4(DSPACE)                
         DC    CL8'ZIPLOG  ',F'001',F'003'                                      
         DC    X'05',AL1(CTCHR),AL1(L'WTLOG),AL1(0),AL4(WTLOG)                  
         DC    CL8'FACPAK  ',F'002',F'004'                                      
         DC    X'05',AL1(CTCHR),AL1(L'NFAC),AL1(0),AL4(NFAC)                    
         DC    CL8'AORNUM  ',F'000',F'032'                                      
         DC    X'05',AL1(CTNUM),AL1(L'AOR),AL1(0),AL4(AOR)                      
         DC    X'FFFFFFFF'                                                      
         EJECT                                                                  
***********************************************************************         
* ENQ for job running                                                 *         
***********************************************************************         
SYSMAJ   DC    CL8'DDSENQ'                                                      
SYSMINL  DC    AL1(L'SYSMIN)                                                    
SYSMIN   DC    C'?.ZIPADV  '                                                    
         ORG   SYSMIN                                                           
SYSDSPCE DS    C                   DSPACE                                       
         DS    CL4                 '.ZIP'                                       
SYSFAC   DS    CL5                 ADVx or REPx + AOR value A-H                 
         ORG                                                                    
                                                                                
TOKNTOKN DC    CL32' '                                                          
***********************************************************************         
* INPUT CARD STORAGE AREAS                                            *         
***********************************************************************         
DSPACE   DC    CL12'            '  NAME OF DATASPACE                            
WTLOG    DC    CL03'NO '           LOGGING                                      
NFAC     DC    CL04'    '          FAPAK NAME                                   
AOR      DC    F'0'                                                             
***********************************************************************         
* LOGGER RESULTS MESSAGE                                              *         
***********************************************************************         
MSGHL    DC    AL2(30)                                                          
MSGH     DC    CL30' '                                                          
         ORG   MSGH                                                             
         DC    CL04'+ZIP'                                                       
MSGFAC   DC    CL04'    '                                                       
         DC    CL07' Log - '                                                    
MSGACT   DC    CL11' '                                                          
         ORG   MSGH+L'MSGH                                                      
*                                                                               
M1L      DC    AL2(70)                                                          
M1       DC    CL70' '                                                          
         ORG   M1                                                               
         DC    CL13'Start time   '                                              
MSGTIN   DC    CL15' '                                                          
         DC    C'  '                                                            
         DC    CL15'Input length  '                                             
MSGIN    DC    CL06'       '                                                    
         ORG                                                                    
*                                                                               
M2L      DC    AL2(70)                                                          
M2       DC    CL70' '                                                          
         ORG   M2                                                               
         DC    CL13'End time     '                                              
MSGTOUT  DC    CL15' '    '                                                     
         DC    C'  '                                                            
         DC    CL15'Output Length '                                             
MSGOUT   DC    CL06'       '                                                    
         ORG                                                                    
*                                                                               
M3L      DC    AL2(70)                                                          
M3       DC    CL70' '                                                          
         ORG   M3                                                               
         DC    CL13'Process time '                                              
MSGPTIM  DC    CL15'     '                                                      
         DC    C'  '                                                            
         DC    CL15'Compression   '                                             
MSGCMP   DC    CL08'         '                                                  
         ORG   M3+L'M3                                                          
*                                                                               
CMPRS    DC    CL11'Compress+'                                                  
UNCMPRS  DC    CL11'Uncompress+'                                                
UNKNOWN  DC    CL11'??????????+'                                                
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
CERRMSGL EQU   40                                                               
CERRHDRL EQU   20                                                               
*                                                                               
CERRHDR  DC    CL20' *** CARD ERROR *** '                                       
CINVLIN  DC    CL40'Invalid Line Format'                                        
CINVKEY  DC    CL40'Invalid Keyword'                                            
CNOTNUM  DC    CL40'Value not a valid number'                                   
CNOTCHR  DC    CL40'Value not a valid character string'                         
CTOOSHT  DC    CL40'Length of input string too short'                           
CTOOLNG  DC    CL40'Length of input string too long'                            
CTOOLOW  DC    CL40'Numeric value too small'                                    
CTOOBIG  DC    CL40'Numeric value too large'                                    
CNOINP   DC    CL40'Invalid/missing value'                                      
*                                                                               
ERR1     DC    CL40'INVALID FACPAK NAME - CHECK INPUT CARDS'                    
ERR2     DC    CL40'UNABLE TO CONNECT TO DATASPACE'                             
ERR3     DC    CL40'NO STOKEN - CHECK FACPAK AVAILABLE'                         
ERR4     DC    CL40'INVALID STOKEN - CHECK FACPAK'                              
ERR5     DC    CL40'TOR- eyecatcher not found'                                  
ERR6     DC    CL40'FACPAK not found in DMGR Dataspace'                         
         EJECT                                                                  
***********************************************************************         
* READ DATA ROUTINE CALLED BY IMPLODE AS REQUIRED                     *         
***********************************************************************         
IMPREAD  STM   RE,RC,12(RD)                                                     
         BASR  RB,R0                                                            
         LA    RB,0(RB)                                                         
         SH    RB,12(RB)                                                        
         B     32(RB)                                                           
         DC    AL2(6)                                                           
         DC    AL2(0)                                                           
         DC    CL8'*IMPREAD'                                                    
         DC    AL2(4096)                                                        
         USING *-32,RB                                                          
         LR    R3,R1               R3 = INCOMING PARAMS                         
         USING FILEHDS,R3                                                       
         L     RC,FHCLDATA         RESTORE W/S                                  
         USING WORKD,RC                                                         
         ST    RD,ZIPCRD           SAVE CALLER'S RD                             
         L     RD,ZIPSRD           RESTORE OUR RD CHAIN                         
         L     R9,SAVER9           RESTORE COMMON STORAGE                       
         USING COMMON,R9                                                        
         SAM31 ,                                                                
*                                                                               
         L     RF,FHOPTYPE         A(OPERATION TYPE)                            
         CLI   3(RF),FHOPREAD      READ NEXT BUFFER OF DATA                     
         BE    IRBUFF                                                           
         CLI   3(RF),FHOPOPRD      OPEN FILE FOR READ                           
         BE    IROPEN                                                           
         CLI   3(RF),FHOPCLRD      CLOSE FILE FOR READ                          
         BE    IRCLSE                                                           
         DC    H'0'                                                             
*                                                                               
IROPEN   LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,FALET       GET BACK A(HEADER)                           
         L     R2,THISHDR                                                       
         SAC   512                                                              
         USING ZIPHDRD,R2                                                       
*                                                                               
         LA    RE,ZIPLENQ(R2)      A(INPUT BUFFER)                              
         LAM   ARE,ARE,FALET       PLUS ALET                                    
         L     RF,ZIPULEN                                                       
         ST    RF,PZINLEN          SET INPUT LENGTH                             
         L     R0,VUNCBUFF                                                      
         ST    R0,PZINADR          SET A(INPUT BUFFER)                          
         L     R1,=A(UNCBUFFL)                                                  
*                                                                               
         MVCL  R0,RE               COPY INPUT BUFFER LOCALLY                    
*                                                                               
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         XC    PZINDSP,PZINDSP                                                  
         B     IMPREADX                                                         
*                                                                               
IRCLSE   B     IMPREADX                                                         
*                                                                               
IRBUFF   ICM   RF,15,PZINLEN       LENGTH LEFT TO COPY                          
         BNZ   IRBUFF02                                                         
         L     RE,FHLENGTH         SET NO MORE DATA                             
         ST    RF,0(RE)                                                         
         B     IMPREADX                                                         
*                                                                               
IRBUFF02 L     R4,FHBUFFER         R4=A(BUFFER TO FILL)                         
         L     R5,FHBFFRSZ         R5=A(BUFFER LENGTH)                          
         ICM   R5,15,0(R5)         R5=BUFFER LENGTH                             
*                                                                               
         L     RE,PZINADR          RE=COPY FROM POINT                           
         A     RE,PZINDSP                                                       
         CR    RF,R5               CHECK LENGTH REMAINING                       
         BH    *+6                                                              
         LR    R5,RF               SET LENGTH TO BE AMOUNT REMAINING            
*                                                                               
         SR    RF,R5                                                            
         ST    RF,PZINLEN          REDUCE AMOUNT REMAINING                      
         LR    RF,R5                                                            
         A     RF,PZINDSP          BUMP DISPLACEMENT POINTER                    
         ST    RF,PZINDSP          AND SAVE IT                                  
         LR    RF,R5                                                            
*                                                                               
         L     R6,FHLENGTH         SET LENGTH MOVED INTO BUFFER                 
         ST    R5,0(R6)                                                         
         MVCL  R4,RE                                                            
         B     IMPREADX                                                         
*                                                                               
IMPREADX SAM24 ,                                                                
*                                                                               
         XR    RF,RF                                                            
         L     RE,FHRESULT         SET RESULT OK IN ALL CASES                   
         ST    RF,0(RE)                                                         
*                                                                               
         L     RD,ZIPCRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R3,RB,RC                                                      
         EJECT                                                                  
***********************************************************************         
* WRITE DATA ROUTINE CALLED BY IMPLODE AS REQUIRED                    *         
***********************************************************************         
         DS    0D                                                               
IMPWRTE  STM   RE,RC,12(RD)                                                     
         BASR  RB,R0                                                            
         LA    RB,0(RB)                                                         
         SH    RB,12(RB)                                                        
         B     32(RB)                                                           
         DC    AL2(6)                                                           
         DC    AL2(0)                                                           
         DC    CL8'*IMPWRTE'                                                    
         DC    AL2(4096)                                                        
         USING *-32,RB                                                          
         LR    R3,R1               R3 = INCOMING PARAMS                         
         USING FILEHDS,R3                                                       
         L     RC,FHCLDATA         RESTORE W/S                                  
         USING WORKD,RC                                                         
         ST    RD,ZIPCRD           SAVE CALLER'S RD                             
         L     RD,ZIPSRD           RESTORE OUR RD CHAIN                         
         L     R9,SAVER9           RESTORE COMMON STORAGE                       
         USING COMMON,R9                                                        
*                                                                               
         SAM31 ,                                                                
         L     RF,FHOPTYPE                                                      
         CLI   3(RF),FHOPWRIT      PUT NEXT BUFFER OF DATA                      
         BE    IWBUFF                                                           
         CLI   3(RF),FHOPOPWR      OPEN FILE FOR WRITE                          
         BE    IWOPEN                                                           
         CLI   3(RF),FHOPCLWR      CLOSE FILE FOR WRITE                         
         BE    IWCLSE                                                           
         DC    H'0'                                                             
*                                                                               
IWOPEN   LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,FALET       OPEN OUTPUT BUFFER CALL                      
         L     R2,THISHDR                                                       
         SAC   512                                                              
         USING ZIPHDRD,R2                                                       
         MVC   PZOUTMAX,ZIPBLEN    SET MAX LENGTH OF OUTPUT BUFFER              
*                                                                               
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         XC    PZOUTLEN,PZOUTLEN   CLEAR CURRENT DISPLACEMENT                   
*                                                                               
         L     R0,VCMPBUFF                                                      
         ST    R0,PZOUTADR         SET A(OUTPUT BUFFER)                         
         L     R1,=A(CMPBUFFL)                                                  
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR OUTPUT BUFFER                          
         B     IMPWRTEX                                                         
*                                                                               
IWCLSE   LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,FALET                                                    
         L     R2,THISHDR                                                       
         SAC   512                                                              
         USING ZIPHDRD,R2                                                       
*                                                                               
         L     R0,VCMPBUFF                                                      
         L     R1,PZOUTLEN                                                      
         STCM  R1,15,ZIPCLEN       SET L'OUTPUT BUFFER                          
         CLM   R1,15,ZIPBLEN                                                    
         BNH   *+12                                                             
         MVI   ZIPSTAT,ZIPSERR                                                  
         B     IMPWRTEX                                                         
*                                                                               
         LR    RF,R1                                                            
         LA    RE,ZIPLENQ(R2)      A(OUTPUT BUFFER IN FACPAK)                   
         LAM   ARE,ARE,FALET                                                    
         MVCL  RE,R0                                                            
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         B     IMPWRTEX                                                         
*                                                                               
IWBUFF   L     R4,FHBUFFER         R4=A(BUFFER TO WRITE)                        
         L     R5,FHBFFRSZ         R5=A(BUFFER LENGTH)                          
         ICM   R5,15,0(R5)         R5=BUFFER LENGTH                             
         L     RF,FHLENGTH         SET LENGTH MOVED FROM BUFFER                 
         ST    R5,0(RF)                                                         
*                                                                               
         L     RE,PZOUTADR         SET A(OUTPUT BUFFER BLOCK)                   
         A     RE,PZOUTLEN         INDEX TO CURRENT MOVE POINT                  
*                                                                               
         LR    RF,R5                                                            
         A     RF,PZOUTLEN                                                      
         ST    RF,PZOUTLEN                                                      
         CLC   PZOUTLEN,PZOUTMAX                                                
         BNH   *+12                                                             
         MVI   ZIPSTAT,ZIPSERR                                                  
         B     IMPWRTEX                                                         
*                                                                               
         LR    RF,R5                                                            
         MVCL  RE,R4               MOVE OUT WRITE BUFFER                        
         B     IMPWRTEX                                                         
*                                                                               
IMPWRTEX SAM24 ,                                                                
*                                                                               
         XR    RF,RF                                                            
         L     RE,FHRESULT         SET RESULT OK IN ALL CASES                   
         ST    RF,0(RE)                                                         
         L     RD,ZIPCRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R3,RB,RC                                                      
         EJECT                                                                  
***********************************************************************         
* READ DATA ROUTINE CALLED BY EXPLODE AS REQUIRED                     *         
***********************************************************************         
EXPREAD  STM   RE,RC,12(RD)                                                     
         BASR  RB,R0                                                            
         LA    RB,0(RB)                                                         
         SH    RB,12(RB)                                                        
         B     32(RB)                                                           
         DC    AL2(6)                                                           
         DC    AL2(0)                                                           
         DC    CL8'*IMPREAD'                                                    
         DC    AL2(4096)                                                        
         USING *-32,RB                                                          
         LR    R3,R1               R3 = INCOMING PARAMS                         
         USING FILEHDS,R3                                                       
         L     RC,FHCLDATA         RESTORE W/S                                  
         USING WORKD,RC                                                         
         ST    RD,ZIPCRD           SAVE CALLER'S RD                             
         L     RD,ZIPSRD           RESTORE OUR RD CHAIN                         
         L     R9,SAVER9           RESTORE COMMON STORAGE                       
         USING COMMON,R9                                                        
*                                                                               
         SAM31 ,                                                                
         LAM   AR2,AR2,FALET                                                    
         L     R2,THISHDR                                                       
         SAC   512                                                              
         USING ZIPHDRD,R2                                                       
*                                                                               
         L     RF,FHOPTYPE         A(OPERATION TYPE)                            
         CLI   3(RF),FHOPREAD      READ NEXT BUFFER OF DATA                     
         BE    ERBUFF                                                           
         CLI   3(RF),FHOPOPRD      OPEN FILE FOR READ                           
         BE    EROPEN                                                           
         CLI   3(RF),FHOPCLRD      CLOSE FILE FOR READ                          
         BE    ERCLSE                                                           
         DC    H'0'                                                             
*                                                                               
EROPEN   L     RF,ZIPLENQ(R2)      A(INPUT BUFFER)                              
         ST    RF,PZINADR                                                       
         L     RF,ZIPCLEN          L'INPUT BUFFER                               
         ST    RF,PZINLEN                                                       
         B     EXPREADX                                                         
*                                                                               
ERCLSE   B     EXPREADX                                                         
*                                                                               
ERBUFF   L     R4,FHBUFFER         R4=A(BUFFER TO FILL)                         
         L     R5,FHBFFRSZ         R5=A(BUFFER LENGTH)                          
         ICM   R5,15,0(R5)         R5=BUFFER LENGTH                             
*                                                                               
         L     RE,PZINADR          RE=COPY FROM POINT                           
         ICM   RF,15,PZINLEN       LENGTH LEFT TO COPY                          
         BZ    ERBUFF02                                                         
*                                                                               
         CR    RF,R5               CHECK LENGTH REMAINING                       
         BH    *+6                                                              
         LR    R5,RF               SET LENGTH TO BE AMOUNT REMAINING            
*                                                                               
         SR    RF,R5                                                            
         ST    RF,PZINLEN          REDUCE AMOUNT REMAINING                      
         LR    RF,R5                                                            
*                                                                               
         L     R6,FHLENGTH         SET LENGTH MOVED INTO BUFFER                 
         ST    R5,0(R6)                                                         
         LAM   ARE,ARE,FALET                                                    
         MVCL  R4,RE                                                            
         B     EXPREADX                                                         
*                                                                               
ERBUFF02 LAM   ARE,ARE,ARZERO                                                   
         L     RE,FHLENGTH         SET NO MORE DATA                             
         ST    RF,0(RE)                                                         
         B     EXPREADX                                                         
*                                                                               
EXPREADX SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         SAM24 ,                                                                
*                                                                               
         XR    RF,RF                                                            
         L     RE,FHRESULT         SET RESULT OK IN ALL CASES                   
         ST    RF,0(RE)                                                         
*                                                                               
         L     RD,ZIPCRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R3,RB,RC                                                      
         EJECT                                                                  
***********************************************************************         
* WRITE DATA ROUTINE CALLED BY EXPLODE AS REQUIRED                    *         
***********************************************************************         
         DS    0D                                                               
EXPWRTE  STM   RE,RC,12(RD)                                                     
         BASR  RB,R0                                                            
         LA    RB,0(RB)                                                         
         SH    RB,12(RB)                                                        
         B     32(RB)                                                           
         DC    AL2(6)                                                           
         DC    AL2(0)                                                           
         DC    CL8'*IMPWRTE'                                                    
         DC    AL2(4096)                                                        
         USING *-32,RB                                                          
         LR    R3,R1               R3 = INCOMING PARAMS                         
         USING FILEHDS,R3                                                       
         L     RC,FHCLDATA         RESTORE W/S                                  
         USING WORKD,RC                                                         
         ST    RD,ZIPCRD           SAVE CALLER'S RD                             
         L     RD,ZIPSRD           RESTORE OUR RD CHAIN                         
         L     R9,SAVER9           RESTORE COMMON STORAGE                       
         USING COMMON,R9                                                        
*                                                                               
         SAM31 ,                                                                
         LAM   AR2,AR2,FALET                                                    
         L     R2,THISHDR                                                       
         SAC   512                                                              
         USING ZIPHDRD,R2                                                       
*                                                                               
         L     RF,FHOPTYPE                                                      
         CLI   3(RF),FHOPWRIT      PUT NEXT BUFFER OF DATA                      
         BE    EWBUFF                                                           
         CLI   3(RF),FHOPOPWR      OPEN FILE FOR WRITE                          
         BE    EWOPEN                                                           
         CLI   3(RF),FHOPCLWR      CLOSE FILE FOR WRITE                         
         BE    EWCLSE                                                           
         DC    H'0'                                                             
*                                                                               
EWOPEN   L     RF,ZIPLENQ(R2)      A(OUTPUT BUFFER)                             
         ST    RF,PZOUTADR                                                      
         L     RF,ZIPBLEN          L'OUTPUT BUFFER                              
         ST    RF,PZOUTMAX                                                      
         XC    PZOUTLEN,PZOUTLEN                                                
         B     EXPWRTEX                                                         
*                                                                               
EWCLSE   L     RF,PZOUTLEN                                                      
         ST    RF,ZIPULEN          SET L'OUTPUT BUFFER                          
         B     EXPWRTEX                                                         
*                                                                               
EWBUFF   L     R4,FHBUFFER         R4=A(BUFFER TO WRITE)                        
         L     R5,FHBFFRSZ         R5=A(BUFFER LENGTH)                          
         ICM   R5,15,0(R5)         R5=BUFFER LENGTH                             
         L     RF,FHLENGTH         SET LENGTH MOVED FROM BUFFER                 
         ST    R5,0(RF)                                                         
*                                                                               
         L     RE,PZOUTADR         SET A(OUTPUT BUFFER BLOCK)                   
         A     RE,PZOUTLEN         INDEX TO CURRENT MOVE POINT                  
         LAM   ARE,ARE,FALET                                                    
*                                                                               
         LR    RF,R5                                                            
         A     RF,PZOUTLEN                                                      
         ST    RF,PZOUTLEN                                                      
         CLC   PZOUTLEN,PZOUTMAX                                                
         BNH   *+6                                                              
         DC    H'0'                OVERFLOW CONDITION                           
*                                                                               
         LR    RF,R5                                                            
         MVCL  RE,R4               MOVE OUT WRITE BUFFER                        
         B     EXPWRTEX                                                         
*                                                                               
EXPWRTEX SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         SAM24 ,                                                                
*                                                                               
         XR    RF,RF                                                            
         L     RE,FHRESULT         SET RESULT OK IN ALL CASES                   
         ST    RF,0(RE)                                                         
         L     RD,ZIPCRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R3,RB,RC                                                      
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
WORKD    DSECT                                                                  
SAVERE   DS    A                                                                
SAVERD   DS    A                                                                
SAVER9   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
*                                                                               
DMCB     DS    6F                                                               
DIVPCK   DS    PL16                                                             
UNCPCK   DS    PL8                                                              
CMPPCK   DS    PL8                                                              
PCTPCK   DS    PL8                                                              
*                                                                               
TIMEIN   DS    PL8                                                              
TIMEOUT  DS    PL8                                                              
TIMEFLD  DS    CL20                                                             
*                                                                               
OFFS     DS    A                   DATASPACE OFFSET                             
ALET     DS    A                   ALET                                         
STOKN    DS    CL8                 STOKEN                                       
*                                                                               
ADSTOR   DS    A                   A(TOR IN DATASPACE)                          
ADSFAC   DS    A                   A(SBEXCHD FOR FACPAK IN DATASPACE)           
FSTOKN   DS    CL8                 FACPAK STOKEN                                
FASID    DS    H                                                                
FALET    DS    A                                                                
FOFFS    DS    A                                                                
FSSB     DS    A                                                                
FTCB     DS    A                                                                
ATOR     DS    A                   A(TOR IN DATASPACE)                          
*                                                                               
BUFFHDR  DS    A                   A(BUFFER HEADER)                             
THISTCB  DS    A                   A(TCB ENTRY)                                 
THISHDR  DS    A                   A(BUFFER ENTRY)                              
FACNAME  DS    CL4                                                              
*                                                                               
WORK     DS    CL64                                                             
CARD     DS    CL80                                                             
PLINE    DS    CL166                                                            
*                                                                               
LCLNTRY  DS    XL(ZIPLENQ)                                                      
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
*                                                                               
         DS 0A                      Ensure Alignment              WLM           
ELCVPARM DS CL(ECLVWNQ)             Join/Leave Enclave Storage    WLM           
ETOKEN   DS XL18                    Enclave Token                 WLM           
BYTES8   DS PL8                                                   WLM           
FWORD    DS XL4                                                   WLM           
         DS 0D                                        -DEBUGGING  WLM           
HOLDTCB$ DC C'HOLDTCB$'            SAVE TCB STRUCTURE -DEBUGGING  WLM           
HOLDTCB  DS CL256                  SAVE TCB STRUCTURE -DEBUGGING  WLM           
*                                                                               
PLIST    DS    9F                  PARAMETER LIST FOR PKZIP                     
PZWRKA   DS    A                   A(64K W/S FOR PKZIP)                         
ZIPCRD   DS    A                   PKZIP CALL RD                                
ZIPSRD   DS    A                   PKZIP SAVE RD                                
PZREGS   DS    XL72                72 BYTE REGISTER AREA FOR PKZIP              
*                                                                               
PZINLEN  DS    F                   INPUT BUFFER LENGTH                          
PZINADR  DS    F                   INPUT BUFFER ADDRESS                         
PZINDSP  DS    F                   INPUT BUFFER DISPLACEMENT                    
PZOUTADR DS    F                   OUTPUT BUFFER ADDRESS                        
PZOUTLEN DS    F                   OUTPUT BUFFER LENGTH                         
PZOUTMAX DS    F                   OUTPUT BUFFER MAX LENGTH                     
*                                                                               
PKREAD   DS    2F                  PARAMETERS FOR READ FILE HANDLER             
PKWRITE  DS    2F                  PARAMETERS FOR WRITE FILE HANDLER            
COMPPARM DS    F                   COMPRESSION PARAMETER                        
DICTPARM DS    F                   DICTIONARY SIZE PARAMETER                    
*                                                                               
PZBUFF   DS    64XL1024            PKZIP W/S BUFFER                             
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    XL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    X                   OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    XL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* POST ERROR HANDLING ROUTINE                                         *         
***********************************************************************         
POSTERR  CSECT                                                                  
         LR    RB,RF               GET ADDRESSIBILITY                           
         USING POSTERR,RB                                                       
         L     RD,EMSAVE           GET SOME W/S FOR HEXOUT                      
         ST    RE,ESAVERE          SAVE RETURN ADDRESS                          
         STM   R0,R3,EREGSIN       SAVE INCOMING INFORMATION                    
*                                                                               
         XR    R0,R0                                                            
         WTO   TEXT=((ERRG1H,D),(ERRG2H,D),(ERRG3H,D),(0,E))                    
*                                                                               
         GOTO1 EHEXOUT,EDMCB,EREGSIN,EREGSOUT,16,0                              
         MVC   EMECB,EREGSOUT+00                                                
         MVC   EMASCB,EREGSOUT+08                                               
         MVC   EMCC,EREGSOUT+16                                                 
         MVC   EMFAIL,EREGSOUT+24                                               
*                                                                               
         XR    R0,R0                                                            
         WTO   TEXT=((EMSGHH,C),(EMSG1H,D),(EMSG2H,D),(0,E))                    
*                                                                               
         L     RE,ESAVERE                                                       
         BR    RE                                                               
*                                                                               
ESAVERE  DC    A(0)                RETURN ADDRESS                               
EHEXOUT  DC    V(HEXOUT)                                                        
EDMCB    DS    6F                                                               
EREGSIN  DS    4F                                                               
EREGSOUT DS    XL32                                                             
*                                                                               
ERRG1H   DC    AL2(45)                                                          
ERRG1    DC    CL45'***************************************'                    
ERRG2H   DC    AL2(45)                                                          
ERRG2    DC    CL45'* **WARNING** ERRET Routine entered   *'                    
ERRG3H   DC    AL2(45)                                                          
ERRG3    DC    CL45'***************************************'                    
*                                                                               
EMSGHH   DC    AL2(30)                                                          
EMSGH    DC    CL30'Entered ZIP ERRET routine'                                  
*                                                                               
EMSG1H   DC    AL2(50)                                                          
EMSG1    DC    CL50' '                                                          
         ORG   EMSG1                                                            
         DC    CL10'ECB=      '                                                 
EMECB    DC    CL08'XXXXXXXX'                                                   
         DC    CL2' '                                                           
         DC    CL10'ASCB=     '                                                 
EMASCB   DC    CL08'XXXXXXXX'                                                   
         ORG                                                                    
*                                                                               
EMSG2H   DC    AL2(50)                                                          
EMSG2    DC    CL50' '                                                          
         ORG   EMSG2                                                            
         DC    CL10'POST CC=  '                                                 
EMCC     DC    CL08'XXXXXXXX'                                                   
         DC    CL2' '                                                           
         DC    CL10'SYSTEM CC='                                                 
EMFAIL   DC    CL08'XXXXXXXX'                                                   
         ORG                                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
EMSAVE   DS    64L                                                              
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DCS                                                 *         
***********************************************************************         
WORKAREA CSECT                                                                  
         DC    200000X'00'                                                      
*                                                                               
CMPBUFF  CSECT                                                                  
         DC    128000X'00'                                                      
CMPBUFFL EQU   *-CMPBUFF                                                        
*                                                                               
UNCBUFF  CSECT                                                                  
         DC    128000X'00'                                                      
UNCBUFFL EQU   *-UNCBUFF                                                        
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* IEZCIB                                                                        
         PRINT OFF                                                              
         DSECT                                                                  
         IEZCIB                                                                 
         PRINT ON                                                               
* IEZCOM                                                                        
         PRINT OFF                                                              
         IEZCOM                                                                 
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FAPIGFACD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAPIGFACD                                                      
         PRINT ON                                                               
* DDZIPBLK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDZIPBLK                                                       
         PRINT ON                                                               
* DDZIPWRKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDZIPWRKD                                                      
         PRINT ON                                                               
* FAWLMED                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAWLMED                                                        
         PRINT ON                                                               
* IMPLODDS                                                                      
         PRINT OFF                                                              
         IMPLODDS                                                               
         PRINT ON                                                               
* EXPLODDS                                                                      
         PRINT OFF                                                              
         EXPLODDS                                                               
         PRINT ON                                                               
* FILEHDS                                                                       
         PRINT OFF                                                              
         FILEHDS                                                                
         PRINT ON                                                               
* IHAASCB                                                                       
         PRINT OFF                                                              
         IHAASCB                                                                
         PRINT ON                                                               
* ISGYENQ                                                                       
         PRINT OFF                                                              
         ISGYENQ                                                                
         PRINT ON                                                               
* ISGYCON                                                                       
         PRINT OFF                                                              
         ISGYCON                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051DDZIP     08/04/14'                                      
         END                                                                    
