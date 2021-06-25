*          DATA SET CTPRO00    AT LEVEL 027 AS OF 02/17/17                      
*PHASE TA0300A                                                                  
*INCLUDE EXPRESS                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE SIXPACK                                                                
         TITLE 'CTPRO00 - CONTROL FILE USER PROFILE RECORDS '                   
PROFILES CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PWRKX-PWRKD,**PROF**,R9,RR=RE,CLEAR=YES                          
         USING PWRKD,RC            RC=A(TEMP W/S)                               
         ST    RE,RELO                                                          
         MVC   PPARMS,0(R1)        SAVE S/R PARMLIST                            
         L     RA,PPTWA                                                         
         USING CTPROFFD,RA         RA=A(TWA)                                    
         LA    R8,PREC                                                          
         ST    R8,APREC            R8=A(I/O)                                    
*                                                                               
         LR    R1,RA                                                            
         A     R1,=F'2350'         SAVE PROFILES HERE                           
         ST    R1,APROFTAB         A(TABLE OF SAVED OFF PROFILES)               
*                                                                               
         LR    R1,RC               ADDRESS OF SECURITY BLOCK                    
         AHI   R1,SECBLK-PWRKD                                                  
         ST    R1,ASECBLK                                                       
*                                  GET A(FACILITIES)                            
         L     RE,=V(EXPRESS)                                                   
         A     RE,RELO                                                          
         ST    RE,PEXPRESS                                                      
*                                                                               
         L     RE,=V(SIXPACK)                                                   
         A     RE,RELO                                                          
         ST    RE,PSIXPACK                                                      
*                                                                               
         L     RE,PPCFACS                                                       
         USING COMFACSD,RE                                                      
         MVC   PDATAMGR,CDATAMGR                                                
         MVC   PGETMSG,CGETMSG                                                  
         MVC   PHELLO,CHELLO                                                    
         MVC   PHEXIN,CHEXIN                                                    
         MVC   PHEXOUT,CHEXOUT                                                  
         MVC   PSCANNER,CSCANNER                                                
         MVC   PCCALLOV,CCALLOV                                                 
         MVC   PDATVAL,CDATVAL                                                  
         MVC   PDATCON,CDATCON                                                  
         MVC   PSWITCH,CSWITCH                                                  
         MVC   PGETTXT,CGETTXT                                                  
         MVC   PDICTATE,CDICTATE                                                
         MVC   PSECRET,CSECRET                                                  
*                                  GET A(SYSTEMS LIST)                          
         L     RF,CCALLOV                                                       
         DROP  RE                                                               
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QGETIDS                                                   
         GOTO1 (RF),DMCB,0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PGETIDS,0(R1)                                                    
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QOFFICER     GET OFFICER ADDRESS                          
         GOTO1 PCCALLOV,DMCB                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   POFFICER,0(R1)                                                   
*                                                                               
         L     RF,PPCFACS          GET A(SYSTEMS LIST)                          
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTOR (RF),DMCB,0                                                      
*                                                                               
         L     R1,0(R1)                                                         
         L     R1,FASYSLST-FACTSD(R1)                                           
         LA    R1,6(R1)                                                         
         LA    R1,SYSLLEN(R1)      BUMP PAST SERVICE TABLE ENTRY                
         ST    R1,ASYSTAB                                                       
*                                                                               
         L     RF,PPTIOB           A(TIOB)                                      
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID                                                       
         CHI   R0,12                                                            
         BNH   *+8                                                              
         SHI   R0,12                                                            
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY                          
         MVC   CURDISP,TIOBCURD    SAVE CURSOR DISPLACEMENT                     
         DROP  RF                                                               
*                                                                               
         L     RE,PPUTL                                                         
         USING UTLD,RE                                                          
         MVC   LANG,TLANG          SAVE CONNECTED LANGUAGE CODE                 
         XC    PPASSWD,PPASSWD                                                  
         TM    TFLAG,TFLAGSEC      TEST CONNECT ID PASSWORD PROTECTED           
         BZ    *+10                                                             
         MVC   PPASSWD,TPASSWD     SAVE PASSWORD NUMBER                         
         MVC   PUSER,TUSER         SAVE USER-ID NUMBER                          
         MVC   PAGYC,TAGY          SAVE CONNECTED AGENCY                        
         MVI   PDDS,0              SET DDS INDIC                                
         CLI   TOFFICE+3,C'*'                                                   
         BNE   *+8                                                              
         MVI   PDDS,1                                                           
         XC    PLIST,PLIST         CLEAR PARAMETER LIST                         
         DROP  RE                                                               
*                                                                               
         XC    DMCB(24),DMCB       INIT SECRET                                  
         GOTO1 PSECRET,DMCB,('SECPINIT',ASECBLK)                                
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         L     R1,ASECBLK                                                       
         USING SECD,R1                                                          
         MVC   PSECINDS,SECINDS    SAVE SECURITY INDICATORS                     
         DROP  R1                                                               
*                                                                               
         GOTOR PDICTATE,DMCB,C'LU  ',DDDCLST,DDDSLST                            
*                                                                               
         CLI   INITFLAG,0          CHECK FIRST TIME INITIALIZATION FLAG         
         BNE   PROF030               NOT FIRST TIME                             
         MVI   INITFLAG,1            ELSE SET UP USERID INPUT FIELD             
*                                                                               
         L     R3,APROFTAB                                                      
         XC    0(APTABLN,R3),0(R3) CLEAR TABLE IN TWA SPACE                     
         XC    CURSCRN,CURSCRN     CLEAR CURRENT SCREEN                         
         XC    LASTSCRN,LASTSCRN   CLEAR LAST SCREEN                            
*                                                                               
         TM    PSECINDS,SECIOLD    OLD SECURITY?                                
         BO    PROF008                                                          
         MVI   WORK,OUIDQ                                                       
         LA    RF,WORK                                                          
         GOTO1 PSECRET,DMCB,('SECPOPTP',ASECBLK),(RF)                           
         BNE   PROF020             NOT USED                                     
         B     PROF010                                                          
*                                                                               
PROF008  CLI   PDDS,1                IF AUTHORISED                              
         BE    PROF010                                                          
         TM    TWAAUTH-TWAD(RA),X'40'  ALLOW IF AUTH=4000                       
         BNO   PROF020                                                          
*                                                                               
PROF010  MVCDD PROUHD,CT#USRID     SET UP USERID INPUT FIELD                    
         GOTOR PDICTATE,DMCB,C'SL  ',PROUHD                                     
         OI    PROUHDH+6,X'80'                                                  
         NI    PROUIDH+1,X'FF'-X'20'                                            
         OI    PROUIDH+6,X'80'                                                  
         MVI   INITFLAG,2          FLAG USERID INPUT FIELD SET UP               
*                                                                               
PROF020  B     EXIT                EXIT FIRST TIME PASS                         
*                                                                               
PROF030  MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         EJECT                                                                  
***********************************************************************         
*        LOAD PROFILE SCREEN (IF NECESSARY)                           *         
***********************************************************************         
*                                                                               
         CLI   CURSCRN,X'FD'       PROFILE SCREEN ALREADY LOADED?               
         BE    ACTVAL              YES - VALIDATE KEY FIELDS                    
         CLI   PFKEY,1             NO - CURRENTLY ON HELP SCREEN                
         BE    ACTVAL                                                           
*                                                                               
         GOTOR PCCALLOV,DMCB,(X'FD',PROHEREH),0     GET PROFILE SCRN            
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         BAS   RE,XMTALL                                                        
*                                                                               
         MVC   LASTSCRN,CURSCRN                                                 
         MVI   CURSCRN,X'FD'       PROFILE SCREEN                               
                                                                                
***********************************************************************         
*        VALIDATE ACTION                                              *         
***********************************************************************         
ACTVAL   MVI   PACTN,DISPLAY       DEFAULT IS DISPLAY IF N/I                    
         LA    R3,ACTNTAB                                                       
         LA    RF,CT@DSP                                                        
         LA    R2,PROACTNH                                                      
         CLI   5(R2),0                                                          
         BE    ACTV4                                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         CLI   5(R2),3             ONLY FIRST 3 BYTES OF INPUT                  
         BNH   *+8                                                              
         LA    R1,3                                                             
         BCTR  R1,0                R1=L'COMPARE                                 
*                                                                               
ACTV2    CLI   0(R3),X'FF'         END OF LIST                                  
         BE    EIIF                                                             
         EX    0,0(R3)             FIND KEYWORD                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(RF)       MATCH TABLE WITH INPUT                       
         BE    ACTV4                                                            
         LA    R3,L'ACTNTAB(R3)    BUMP TO NEXT ENTRY                           
         B     ACTV2                                                            
*                                                                               
ACTV4    MVC   8(8,R2),0(RF)       RE-DISPLAY ACTION                            
         OI    6(R2),X'80'                                                      
*                                                                               
         TM    PSECINDS,SECIOLD    OLD SECURITY?                                
         BNO   ACTV6                                                            
         TM    5(R3),X'80'         X'80' MEANS DDS ONLY                         
         BZ    ACTV6                                                            
         TM    TWAAUTH-TWAD(RA),X'80'  ALLOW IF AUTH=8000                       
         BO    ACTV6                                                            
         CLI   PDDS,0                                                           
         BE    EIIF                                                             
*                                                                               
ACTV6    MVC   PACTN,4(R3)         SET ACTION FROM TABLE                        
         MVC   PACTN2,PACTN                                                     
*                                                                               
         TM    PSECINDS,SECIOLD    OLD SECURITY?                                
         BO    SYSVAL                                                           
         LA    R2,PROACTNH         R3=(RECORD, A(ACTION))                       
         GOTO1 PSECRET,DMCB,('SECPRACT',ASECBLK),('PPROFQ',PACTN)               
         BNE   EISL                DON'T HAVE THE SECURITY                      
                                                                                
***********************************************************************         
*        VALIDATE SYSTEM                                              *         
***********************************************************************         
SYSVAL   LA    R2,PROSYSH                                                       
         CLI   5(R2),0             MUST BE PRESENT                              
         BE    EMIF                                                             
*                                                                               
         CLC   PROSYS,=C'SPOT/NET' IF SPOT/NET DROP /NET                        
         BNE   *+8                                                              
         MVI   5(R2),4                                                          
*                                                                               
         LA    R3,DQUPROF          SEE IF IT'S A SPECIAL FOR $DQU               
SYSV2    CLI   0(R3),X'FF'                                                      
         BE    SYSV6               NO MATCH -- CHECK SYSTAB                     
         CLC   0(2,R3),8(R2)                                                    
         BE    *+12                                                             
         LA    R3,3(R3)                                                         
         B     SYSV2                                                            
*                                                                               
         MVC   PDQUSYS,2(R3)       SAVE SPECIAL $DQU PROFILE SYSTEM             
         NI    PDQUSYS,X'0F'       TURN OFF HIGH NIBBLE FOR NOW                 
         L     R3,ASYSTAB                                                       
         USING SYSLSTD,R3                                                       
SYSV4    CLI   SYSLNUM,0           END OF LIST                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SYSLNUM,PDQUSYS     MATCH TABLE WITH INPUT                       
         BE    *+12                                                             
         LA    R3,SYSLLEN(R3)      BUMP TO NEXT ENTRY                           
         B     SYSV4                                                            
*                                                                               
         OI    PDQUSYS,X'F0'       TURN HIGH NIBBLE BACK ON                     
         MVC   PSYSA,SYSLUSLT      SYSTEM LETTER                                
         MVC   PSYSN,SYSLNUM       SYSTEM OVNUM                                 
         MVC   PSYSI,SYSLIND1      SYSTEM INDICATORS                            
         B     SYSV20                                                           
         DROP  R3                                                               
*                                                                               
SYSV6    ZIC   R1,5(R2)                                                         
         BCTR  R1,0                R1=L'COMPARE                                 
         L     R3,ASYSTAB                                                       
         USING SYSLSTD,R3          R3=A(SYSTEMS TABLE)                          
         MVC   DUB,8(R2)           MOVE OUT SYSTEM NAME                         
*                                                                               
SYSV8    CLI   SYSLNUM,0           END OF LIST                                  
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),DUB     MATCH TABLE WITH INPUT                       
         BE    *+12                                                             
         LA    R3,SYSLLEN(R3)      BUMP TO NEXT ENTRY                           
         B     SYSV8                                                            
*                                                                               
         LA    R1,SYSLNAME                                                      
*                                                                               
         XC    8(8,R2),8(R2)                                                    
         MVC   8(L'SYSLNAME,R2),0(R1)   MOVE SYSTEM NAME                        
         OI    6(R2),X'80'                                                      
         MVC   PSYSA,SYSLUSLT      SYSTEM LETTER                                
         MVC   PSYSN,SYSLNUM       SYSTEM OVNUM                                 
         MVC   PSYSI,SYSLIND1      SYSTEM INDICATORS                            
         MVC   PDQUSYS,PSYSA                                                    
         B     SYSV20                                                           
         DROP  R3                                                               
*                                                                               
SYSV20   TM    PSECINDS,SECIOLD    OLD SECURITY?                                
         BO    PRGVAL                                                           
         LA    R2,PROSYSH          R3=(RECORD, A(ACTION))                       
         MVC   BYTE,PSYSN                                                       
*                                                                               
SYSV24   OI    BYTE,X'20'          ADJUST FOR SYSTEM/ACTION NUMBER              
         GOTO1 PSECRET,DMCB,('SECPRACT',ASECBLK),('PPSYSQ',BYTE)                
         BNE   EISL                DON'T HAVE THE SECURITY                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PROGRAM                                             *         
***********************************************************************         
                                                                                
PRGVAL   LA    R2,PROPRGH                                                       
         CLI   5(R2),0             MUST BE PRESENT                              
         BE    EMIF                                                             
         CLI   5(R2),2             2 CHARS MEANS OFFLINE PROG NUM               
         BL    EFTS                                                             
         MVI   PPRG,0                                                           
         MVC   PPRG+1(2),8(R2)     OFFLINE=X'00PPPP'                            
         BE    PRGVAL1                                                          
         MVC   PPRG,8(R2)                                                       
*&&US                                                                           
PRGVAL1  CLC   PAGYC,=C'MC'        MCCANNS ONLY                                 
         BNE   AGYVAL                                                           
         CLI   PSYSA,C'S'          FOR SPOT ONLY                                
         BNE   AGYVAL                                                           
         CLI   PACTN,DISPLAY       DISPLAY ONLY                                 
         BNE   AGYVAL                                                           
         L     R1,=A(SPOTNET)      CHECK SPOTNET TABLE                          
         A     R1,RELO                                                          
PRGVAL2  CLC   0(3,R1),=X'FFFFFF'  EOT                                          
         BE    AGYVAL                                                           
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R1)                                                    
         BE    PRGVAL3                                                          
         LA    R1,3(R1)                                                         
         B     PRGVAL2                                                          
*                                                                               
PRGVAL3  MVC   PROSYS,=C'SPOT/NET' SPECIAL SPOT/NET                             
         B     AGYVAL                                                           
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*        EXTRACT DATA FROM CONNECTED USER ID RECORD                   *         
*        INCLUDING AGENCY CODES, USER ID ALPHA AND COMPATIBLE IDS     *         
***********************************************************************         
                                                                                
AGYVAL   DS    0H                                                               
         MVI   PLEVEL,1            SET KEY LEVEL                                
         USING CTIREC,R8                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID+8(2),PUSER                                                
*                                                                               
AGYV4    MVC   PKEY,CTIKEY                                                      
         GOTOR READ                READ ID RECORD                               
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         LA    R3,CTIDATA                                                       
         SR    R4,R4                                                            
         XC    SYSFLAG,SYSFLAG                                                  
         XC    PAGYA,PAGYA                                                      
         MVC   PUSERA,SPACES                                                    
*                                                                               
AGYV010  CLI   0(R3),0             EXTRACT ELEMENT DATA                         
         BE    AGYV100                                                          
         CLI   0(R3),X'02'         USER-ID ALPHA POINTER ELEMENT                
         BE    AGYVUSA                                                          
         CLI   0(R3),X'06'         AGENCY ID ELEMENT                            
         BE    AGYVAID                                                          
         CLI   0(R3),X'21'         SYSTEM ELEMENT                               
         BE    AGYVSYS                                                          
*                                                                               
AGYV020  IC    R4,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R4                                                            
         B     AGYV010                                                          
*                                  ID ELEMENT-DISPLAY ID IF N/I                 
*                                  SYSTEM ELEMENT                               
         USING CTSYSD,R3                                                        
AGYVSYS  CLC   CTSYSNUM,PSYSN      SAME SYSTEM                                  
         BNE   AGYV020                                                          
         MVI   SYSFLAG,1           FLAG ELEMENT FOUND                           
         MVC   PAGY,CTSYSAGB       YES - SAVE AGENCY BINARY                     
         MVC   PSYSSEN,CTSYSSE     AND SYSTEM SE NUMBER                         
         B     AGYV020                                                          
         DROP  R3                                                               
*                                                                               
AGYVUSA  MVC   PUSERA,2(R3)                                                     
         B     AGYV020                                                          
*                                                                               
AGYVAID  MVC   PAGYA,2(R3)                                                      
         B     AGYV020                                                          
*                                                                               
AGYV100  CLI   SYSFLAG,1           CHECK SYSTEM ELEMENT FOUND                   
         BE    AGYVX                                                            
         LA    R2,PROSYSH            ERROR IF NOT FOUND                         
         B     EIIF                                                             
*                                                                               
AGYVX    B     CHKOFFL                                                          
         DROP  R8                                                               
                                                                                
***********************************************************************         
* CHECK FOR OFFICE LIST PROFILE                                                 
***********************************************************************         
CHKOFFL  DS    0H                                                               
*&&US                                                                           
         CLI   PSYSA,C'S'          SPOT?                                        
         BE    *+12                                                             
         CLI   PSYSA,C'P'          OR PRINT?                                    
         BNE   GETPID              NO: MOVE ON                                  
         CLI   PPRG,C'$'           CHECK FOR OFFICE LISTS                       
         BE    *+12                                                             
         CLI   PPRG+1,C'$'         OFFICE LIST?                                 
         BNE   GETPID              NO: MOVE ON                                  
         BRAS  RE,CHKOFL           OFFICE LISTS STILL MAINTAINED HERE?          
         BH    ENOL                NO: MESSAGE SAYS SO                          
         BL    EIIF                INVALID OFFICE LIST                          
*&&                                                                             
***********************************************************************         
*        GET PRINCIPLE ID FROM AGENCY ACCESS RECORD                   *         
***********************************************************************         
GETPID   DS    0H                                                               
*                                                                               
         USING CT5REC,R8                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KEY,C'5'                                                      
         MVC   CT5KALPH,PAGYA                                                   
         MVC   PKEY,CT5KEY                                                      
         GOTOR READ                READ ACCESS RECORD                           
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,CT5DATA                                                       
         SR    R1,R1                                                            
         XC    PPIDN,PPIDN                                                      
*                                                                               
GPID010  CLI   0(R3),0             EXTRACT ELEMENT DATA                         
         BE    UIDVAL                                                           
         CLI   0(R3),X'02'         PID NUMBER ELEMENT                           
         BE    GPIDPID                                                          
*                                                                               
GPID020  IC    R1,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R1                                                            
         B     GPID010                                                          
*                                                                               
GPIDPID  MVC   PPIDN,2(R3)                                                      
         B     UIDVAL                                                           
         DROP  R8                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        VALIDATE USER-ID                                             *         
***********************************************************************         
UIDVAL   DS    0H                                                               
         XC    PUIDN,PUIDN                                                      
         MVC   PUIDA,SPACES                                                     
         LA    RF,PHEADS           INITIALISE PROF. VALUE HEADING               
         ST    RF,APHEADS            ADDRESS TO AGENCY TYPE                     
         CLI   INITFLAG,2          CHECK USERID INPUT FIELD SET UP              
         BNE   MEDVAL                CONTINUE WITHOUT UID CHECK                 
         LA    R2,PROUIDH          SET POINTER TO USER-ID FIELD                 
         CLI   5(R2),0             IF NO INPUT                                  
*&&UK*&& BE    EUID                ERROR IN UK ONLY                             
         BNE   UIDV004                                                          
         MVC   8(4,R2),CT@ALL        DISPLAY ALL DEFAULT                        
         OI    6(R2),X'80'                                                      
         B     MEDVAL                                                           
*                                                                               
UIDV004  SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         CLI   5(R2),4                                                          
         BH    UIDV008                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),CT@ALL      REQUEST FOR AGENCY LEVEL                     
         BNE   UIDV008                                                          
         MVC   8(4,R2),CT@ALL                                                   
         OI    6(R2),X'80'         REDISPLAY USER-ID FIELD                      
         B     MEDVAL                                                           
*                                                                               
UIDV008  EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PUIDA(0),8(R2)                                                   
*                                                                               
         USING CTIREC,R8                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,PUIDA                                                     
*                                                                               
         MVC   PKEY,CTIKEY                                                      
         GOTOR READ                READ ID RECORD                               
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         XC    PROGAUTH,PROGAUTH                                                
         LA    R3,CTIDATA                                                       
*                                  EXTRACT ELEMENT DATA                         
UIDV010  CLI   0(R3),0                                                          
         BE    UIDV100                                                          
         CLI   0(R3),X'02'         USERID# POINTER                              
         BE    UIDV030                                                          
         CLI   0(R3),X'06'         AGENCY ID                                    
         BE    UIDV040                                                          
         CLI   0(R3),X'21'         SYSTEM                                       
         BE    UIDV050                                                          
UIDV020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     UIDV010                                                          
*                                                                               
UIDV030  MVC   PUIDN,2(R3)                                                      
         B     UIDV020                                                          
*                                                                               
UIDV040  MVC   PUIDAGY,2(R3)                                                    
         B     UIDV020                                                          
*                                                                               
         USING CTSYSD,R3                                                        
UIDV050  CLI   CTSYSNUM,X'0A'      SAVE CONTROL/PRO AUTH CODE                   
         BNE   UIDV020                                                          
         MVC   PROGAUTH,CTSYSALL                                                
         LA    R1,CTSYSPGM                                                      
         ZIC   RE,CTSYSLEN                                                      
*                                                                               
UIDV052  CHI   RE,16                                                            
         BNH   UIDV020                                                          
         L     RF,PPUTL            FIND =PROF PROGRAM ENTRY                     
         CLC   0(1,R1),TPRG-UTLD(RF)                                            
         BE    UIDV054                                                          
         LA    R1,3(R1)            GET NEXT PROGAM CODE                         
         SHI   RE,3                                                             
         B     UIDV052                                                          
*                                                                               
UIDV054  MVC   PROGAUTH,1(R1)      SAVE ACCESS CODE                             
         B     UIDV020                                                          
         DROP  R3                                                               
*                                  CHECK VALID USERID ACCESS                    
UIDV100  CLC   PUIDAGY,PAGYA                                                    
         BNE   EISL                NOT OK IF NOT SAME AGENCY                    
         CLI   PDDS,1              OK IF DDS                                    
         BE    UIDV200                                                          
         CLI   PACTN,DISPLAY       DISPLAY                                      
         BE    UIDV200                                                          
         TM    PSECINDS,SECIOLD    OLD SECURITY?                                
         BZ    *+12                                                             
         TM    PROGAUTH,X'40'      OK IF =PROF AUTH=4000                        
         BZ    EUAU                                                             
         CLC   PPIDN,PUSER         OK IF PRINCIPLE ID                           
         BNE   EUCP                                                             
*                                                                               
UIDV200  LA    RF,PHEADSU          CHANGE PROF. VALUE HEADING ADDRESS           
         ST    RF,APHEADS            TO USERID TYPE                             
         MVC   8(8,R2),PUIDA                                                    
         OI    6(R2),X'80'         REDISPLAY USER-ID                            
         B     MEDVAL                                                           
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
*                                **NO LONGER ALLOW COMPATIBLE IDS               
*                                  BUILD COMPATIBLE ID LIST IN TIA              
*                                    FROM PASSWORD AUTH RECORD                  
*        USING CT0REC,R8                                                        
*IDV110  OC    PPASSWD,PPASSWD     UNLESS NOT CONNECTED TO A PASSWORD           
*        BZ    EISL                                                             
*        XC    CT0KEY,CT0KEY                                                    
*        MVI   CT0KTYP,CT0KTEQU                                                 
*        MVC   CT0KAGY,PAGYA                                                    
*        MVC   CT0KNUM,PPASSWD                                                  
*        MVC   PKEY,CT0KEY                                                      
*        GOTOR READ                READ AUTH RECORD                             
*        BZ    EIIO                                                             
*        CLI   DMCB+8,0                                                         
*        BE    *+6                                                              
*        DC    H'00'                                                            
*        GOTOR PGETIDS,DMCB,(C'C',CT0REC),PPTIA,PDATAMGR                        
*        CLI   0(R1),X'FF'                                                      
*        BE    EIIO                                                             
*                                  IN CONNECT ID COMPATIBLE LIST?               
*        CLI   0(R1),0             CHECK NULL LIST                              
*        BE    EISL                  EXIT SECURITY INVALID                      
*        L     RF,PPTIA            SEARCH DOWN LIST                             
*IDV120  CLC   0(10,RF),PUIDA                                                   
*        BE    UIDV200             MATCH FOUND                                  
*        LA    RF,12(RF)           GET NEXT LIST ENTRY                          
*        CLI   0(RF),X'FF'           UPTO END                                   
*        BNE   UIDV120                                                          
*        B     EISL                  EXIT SECURITY INVALID                      
*                                  USERID IS VALID                              
***********************************************************************         
*        VALIDATE MEDIA                                               *         
***********************************************************************         
                                                                                
MEDVAL   XC    PMED,PMED           CLEAR MEDIA/CLIENT                           
         XC    PCLI,PCLI                                                        
         XC    PDQUNAM,PDQUNAM                                                  
         LA    R2,PROMEDH                                                       
*                                                                               
MV010    TM    PDQUSYS,X'F0'       IS THIS A SPECIAL $DQU PROFILE?              
         BNO   MEDV1                NO                                          
*                                                                               
         LA    RF,WRITBLE                                                       
         CLC   0(1,RF),PSYSA       THIS SYSTEM?                                 
         BNE   *+14                 NO                                          
         CLC   1(2,RF),PPRG+1      IS IT A VALID WRITER TYPE REPORT?            
         BE    MEDVDQU              YES - ALLOW CODE IN MEDIA FLD               
         LA    RF,21(RF)           NEXT ENTRY                                   
         CLI   0(RF),X'FF'         EOT?                                         
         BNE   *-28                 NO                                          
*                                                                               
MEDV1    CLI   PSYSA,C'A'          ACCOUNT HAS SPECIAL MEDIA CODE               
         BNE   MEDV1A                                                           
         CLI   5(R2),0                                                          
         BE    CLIVAL                                                           
         CLI   5(R2),2                                                          
         BL    EFTS                                                             
         MVC   PMED,8(R2)                                                       
         BH    MEDV1A1                                                          
         B     MEDV2                                                            
MEDV1A   CLI   5(R2),1                                                          
         BL    CLIVAL              NOT REQUIRED                                 
         MVC   PMED,8(R2)                                                       
         BE    MEDV2                                                            
MEDV1A1  CLI   5(R2),3                                                          
         BNE   EIIF                                                             
         CLI   PSYSA,C'A'          ACCOUNT HAS SPECIAL MEDIA CODE               
         BNE   MEDV1C                                                           
         CLI   PROSYS,C'Q'         IGNORE SPECIAL CODE FOR DQU PROFILES         
         BE    MEDV1C                                                           
*&&US                                                                           
         CLI   8(R2),C'T'          SPECIAL FOR UNIT T                           
         BNE   MEDV1B                                                           
         GOTOR PHEXIN,DMCB,9(R2),PMED+1,2                                       
         OC    DMCB+12(4),DMCB+12                                               
         BZ    EIIF                                                             
         B     MEDV2                                                            
*&&                                                                             
MEDV1B   CLI   PACTN,DISPLAY       ALLOW ONLY DISPLAY                           
         BE    MEDV1C                                                           
         CLI   PACTN,DELETE        AND DELETE OF 'ALL' MEDIA                    
         BNE   EIIF                                                             
MEDV1C   CLC   8(3,R2),CT@ALL      CHECK FOR 'ALL'                              
         BNE   EIIF                                                             
         XC    PMED,PMED                                                        
         LA    R2,PROCLIH                                                       
         CLI   5(R2),0                                                          
         BE    EMIF                                                             
MEDV2    MVI   PLEVEL,2                                                         
         B     CLIVAL                                                           
*                                                                               
* SPECIAL VALIDATION - MEDIA IS REALLY A WRITER NAME                            
MEDVDQU  DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    CLIVAL                                                           
         ZIC   R0,PROMEDH+5        GET INP LENGTH                               
         LA    R1,PROMED                                                        
MEDVD10  LA    RE,L'VALCHARS                                                    
         LA    RF,VALCHARS                                                      
*                                                                               
         CLC   0(1,RF),0(R1)       IS THIS A VALID CHAR?                        
         BE    *+16                 YES                                         
         LA    RF,1(RF)            NEXT VALID CHAR                              
         BCT   RE,*-14                                                          
         B     EIIF                INVALID CHAR                                 
         LA    R1,1(R1)            NEXT CHAR IN FIELD                           
         BCT   R0,MEDVD10                                                       
*                                                                               
         GOTOR PSIXPACK,DMCB,PROMED,PDQUNAM,8                                   
         MVI   PLEVEL,2                                                         
                                                                                
***********************************************************************         
*        VALIDATE CLIENT                                              *         
***********************************************************************         
CLIVAL   LA    R2,PROCLIH                                                       
*                                                                               
         TM    PSECINDS,SECIOLD    OLD SECURITY?                                
         BO    CV010                                                            
         CLI   5(R2),0             ANY CLIENT ENTERED?                          
         BE    VALEND                                                           
         B     CV020                                                            
*                                                                               
CV010    CLI   5(R2),0             NOT REQUIRED                                 
         BNE   CV020                                                            
         CLI   PDDS,1              OVERRIDE CHECK IF DDS                        
         BE    VALEND                                                           
         TM    TWAAUTH-TWAD(RA),X'10'  MUST ENTER IF AUTH=1000                  
         BNZ   EMIF                                                             
         B     VALEND                                                           
*                                                                               
CV020    OC    PDQUNAM,PDQUNAM     IS THIS A DQU WRITER PROFILE?                
         BNZ   EIIF                 YES - CLT N/A                               
         CLI   PLEVEL,2            IF INPUT THEN MEDIA MUST BE INPUT            
         BE    *+12                                                             
         LA    R2,PROMEDH                                                       
         B     EMIF                                                             
         CLI   PSYSA,C'A'          ACCOUNTING CAN BE 1 THRU 3                   
         BNE   CV030                                                            
         CLI   PROSYS,C'Q'         IGNORE SPECIAL CODE FOR DQU PROFILES         
         BE    CV030                                                            
*&&US*&& CLI   5(R2),3                                                          
*&&UK*&& CLI   5(R2),5                                                          
         BH    EFTL                                                             
*&&US*&& B     CV050                                                            
*&&UK*&& B     CV040                                                            
CV030    CLI   5(R2),2             CAN BE 2 OR 3 CHRS                           
         BL    EFTS                                                             
*&&US                                                                           
         CLI   5(R2),3                                                          
         BH    EFTL                                                             
*&&                                                                             
*&&UK                                                                           
         CLI   PSYSA,C'M'          MEDIA SYSTEM CAN HAVE 5 CHR CLIENTS          
         BE    CV040                                                            
         CLI   5(R2),3                                                          
         BH    EFTL                                                             
         B     CV050                                                            
CV040    DS    0H                                                               
         CLI   5(R2),4                                                          
         BE    EIIF                                                             
         CLI   5(R2),5                                                          
         BH    EFTL                                                             
         BNE   CV050                                                            
         MVC   PCLI(1),8(R2)       DEAL WITH 5 CHR CLIENTS                      
         MVC   DUB(4),=4C'0'                                                    
         MVZ   DUB(4),9(R2)                                                     
         CLC   DUB(4),=4C'0'                                                    
         BNE   EIIF                                                             
         PACK  DUB,9(4,R2)                                                      
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   PCLI+1(2),DUB                                                    
         B     CV060                                                            
*&&                                                                             
CV050    MVC   PCLI,SPACES                                                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCLI(0),8(R2)       SAVE CLIENT                                  
*&&US                                                                           
         GOTOR CONOFF,DMCB,(2,PCLI),(C'C',0) CONVERT/VALIDATE OFFICE            
         BL    EORN                                                             
*&&                                                                             
CV060    MVI   PLEVEL,3            SET KEY LEVEL                                
         EJECT                                                                  
***********************************************************************         
*        READ DEFINITION & PROFILE RECORDS                            *         
***********************************************************************         
                                                                                
VALEND   LA    R2,PROPRGH                                                       
         XC    PRODESC,PRODESC                                                  
         OI    PRODESCH+6,X'80'                                                 
         USING CTUREC,R8           BUILD DEFINITION RECORD KEY                  
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKEY,C'U'                                                      
         MVC   CTUKSYS,PDQUSYS                                                  
         MVC   CTUKPROG,PPRG                                                    
         MVC   CTUKLANG,LANG       USE CONNECTED LANGUAGE                       
         MVC   PKEY,CTUKEY                                                      
         GOTOR READUP                                                           
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BE    VALEND1             DEFINITION REC FOUND                         
*                                    ELSE USE DEFAULT LANGUAGE NULL             
         MVI   PKEY+CTUKLANG-CTUKEY,0                                           
         GOTOR READUP                                                           
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                DEFINITION REC N/F                           
*                                                                               
VALEND1  LA    R3,CTUDATA                                                       
         SR    R4,R4                                                            
         XC    PPROFS,PPROFS                                                    
         XC    PROFOTHR,PROFOTHR                                                
VALEND2  CLI   0(R3),0                                                          
         BE    VALEND4                                                          
         CLI   0(R3),X'02'                                                      
         BE    VALEND2B                                                         
         CLI   0(R3),X'70'                                                      
         BE    VALEND2C                                                         
VALEND2A IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     VALEND2                                                          
VALEND2B IC    R4,1(R3)            DISPLAY PROGRAM DESCRIPTION                  
         SHI   R4,3                                                             
         EX    R4,*+8                                                           
         B     VALEND2A                                                         
         MVC   PRODESC(0),2(R3)                                                 
         USING CTFDD,R3                                                         
VALEND2C ZIC   R1,CTFDNUM                                                       
         LR    RE,R1                                                            
         LA    R1,PROFOTHR-1(R1)                                                
         LA    RE,PPROFD-1(RE)                                                  
         MVC   0(1,RE),CTFDDEF     MOVE DEFAULT VALUE TO LIST                   
         OI    0(R1),X'80'         FIELD EXISTS                                 
         TM    CTFDOTHR,X'80'                                                   
         BZ    *+8                                                              
         OI    0(R1),X'40'         DDS ONLY FLAG                                
         TM    CTFDOTHR,X'40'                                                   
         BO    *+8                                                              
         OI    0(R1),X'08'         DEFAULT EXISTS                               
         B     VALEND2A                                                         
         DROP  R3                                                               
*                                                                               
VALEND4  DS    0H                                                               
         LA    R8,PREC2            SWITCH I/O AREAS                             
         ST    R8,APREC                                                         
         MVC   CTUKEY,PKEY                                                      
         MVI   CTUKLANG,0          CLEAR ANY LANGUAGE CODE FROM DEFN.           
         LA    R2,PROMEDH                                                       
         OC    PUIDN,PUIDN         CHECK IF USERID INPUT                        
         BZ    VALEND4A                                                         
         MVC   CTUKUID,PUIDN       MOVE USERID TO KEY                           
         B     VALEND6                                                          
VALEND4A MVC   CTUKAGY,PAGYA         ELSE MOVE AGENCY TO KEY                    
         CLI   PSYSA,C'A'                                                       
         BNE   VALEND6                                                          
         CLI   PROSYS,C'Q'         IGNORE SPECIAL CODE FOR DQU PROFILES         
         BE    VALEND6                                                          
         TM    PSYSI,X'80'                                                      
         BNO   VALEND6             ACCOUNT HAS BEEN CONVERTED                   
         MVC   CTUKAGY(1),PAGY     OLD WAY IS BINARY NUMBER                     
         MVI   CTUKAGY+1,0                                                      
*                                                                               
VALEND6  MVC   PKEY,CTUKEY                                                      
         LA    R2,PROPRGH                                                       
         CLI   INITFLAG,2                                                       
         BNE   *+8                                                              
         LA    R2,PROUIDH                                                       
         GOTOR READUP                                                           
         BZ    EIIO                                                             
         MVC   PPROFA,PPROFD       IF AGENCY LEVEL USE FIELD DEFAULTS           
         TM    DMCB+8,X'10'        CHECK RECORD NOT FOUND                       
         BNO   VALEND8                                                          
         CLI   PLEVEL,1                                                         
         BNE   ERNF                                                             
         OC    PUIDN,PUIDN         CHECK IF USERID INPUT                        
         BZ    VALENDB                                                          
         MVC   CTUKEY,PKEY                                                      
         MVC   CTUKAGY,PAGYA         IF SO GET PROFILE FROM AGENCY              
         CLI   PSYSA,C'A'                                                       
         BNE   VALEND6A                                                         
         CLI   PROSYS,C'Q'         IGNORE SPECIAL CODE FOR DQU PROFILES         
         BE    VALEND6A                                                         
         TM    PSYSI,X'80'                                                      
         BNO   VALEND6A            ACCOUNT HAS BEEN CONVERTED                   
         MVC   CTUKAGY(1),PAGY     OLD WAY IS BINARY NUMBER                     
         MVI   CTUKAGY+1,0                                                      
VALEND6A MVC   PKEY,CTUKEY                                                      
         GOTOR READUP                                                           
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'                                                     
         BO    EAGP                                                             
         LA    R1,PPROFA           EXTRACT AGENCY/USERID PROFILE                
         GOTOR GETPROF                                                          
         MVC   CTUKEY,PKEY                                                      
         MVC   CTUKUID,PUIDN       MOVE USERID TO KEY                           
         MVC   PKEY,CTUKEY                                                      
         GOTOR READUP                                                           
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        CHECK RECORD NOT FOUND                       
         BO    VALENDB                                                          
         B     EIIO                                                             
*                                                                               
VALEND8  LA    R1,PPROFA           EXTRACT AGENCY/USERID PROFILE                
         GOTOR GETPROF                                                          
         LA    R2,PROMEDH                                                       
         CLI   PSYSA,C'A'          ACCOUNT HAS SPECIAL KEY VALUES               
         BNE   VALEND8E                                                         
         CLI   PROSYS,C'Q'         IGNORE SPECIAL CODE FOR DQU PROFILES         
         BE    VALEND8E                                                         
         TM    PSYSI,X'80'                                                      
         BNO   VALEND8A            FILE STILL HAS ONE BYE COMMPANY              
         OC    PUIDN,PUIDN           AND USERID NOT INPUT                       
         BZ    VALEND8B                                                         
VALEND8A MVC   CTUKUNT(2),PMED     FILE HAS BEEN CONVERTED                      
         B     VALEND8F              OR USERID INPUT                            
VALEND8B MVC   CTUKAGY+1(2),PMED                                                
         B     VALEND8F                                                         
VALEND8E DS    0H                                                               
         OC    PDQUNAM,PDQUNAM     DQU WRITER PROFILE?                          
         BZ    *+14                 NO                                          
         MVC   CTUKNAM,PDQUNAM                                                  
         B     *+10                                                             
         MVC   CTUKMED,PMED        MOVE MEDIA TO KEY                            
VALEND8F MVC   PKEY,CTUKEY                                                      
         GOTOR READUP                                                           
         BZ    EIIO                                                             
         MVC   PPROFM,PPROFA       USE AGENCY/USERID DEFAULTS                   
         TM    DMCB+8,X'10'                                                     
         BO    *+16                                                             
         LA    R1,PPROFM           EXTRACT MEDIA PROFILE                        
         GOTOR GETPROF                                                          
         B     VALENDA                                                          
         CLI   PLEVEL,2                                                         
         BE    VALENDB                                                          
         B     ERNF                                                             
*                                                                               
VALENDA  LA    R2,PROCLIH                                                       
         CLI   PSYSA,C'A'          ACCOUNT HAS SPECIAL KEY VALUES               
         BNE   VALENDAE                                                         
         CLI   PROSYS,C'Q'         IGNORE SPECIAL CODE FOR DQU PROFILES         
         BE    VALENDAE                                                         
         TM    PSYSI,X'80'                                                      
         BNO   VALENDAA            FILE STILL HAS ONE BYTE COMMPANY             
         OC    PUIDN,PUIDN           AND USERID NOT INPUT                       
         BZ    VALENDAE                                                         
VALENDAA MVC   CTUKACT,PCLI        FILE HAS BEEN CONVERTED                      
         B     VALENDAF              OR USERID INPUT                            
VALENDAE DS    0H                                                               
         OC    PDQUNAM,PDQUNAM                                                  
         BNZ   VALENDB                                                          
         MVC   CTUKCLT,PCLI        MOVE CLIENT TO KEY                           
VALENDAF MVC   PKEY,CTUKEY                                                      
         GOTOR READUP                                                           
         BZ    EIIO                                                             
         MVC   PPROFC,PPROFM       USE MEDIA DEFAULTS                           
         TM    DMCB+8,X'10'                                                     
         BO    VALENDB                                                          
         LA    R1,PPROFC           AND EXTRACT CLIENT PROFILE                   
         GOTOR GETPROF                                                          
*                                                                               
VALENDB  CLI   PACTN,DELETE                                                     
         BE    *+12                                                             
         CLI   PACTN,RESTORE                                                    
         BNE   VALENDC                                                          
*                                                                               
         TM    PSECINDS,SECIOLD    OLD SECURITY?                                
         BNO   VALENDC                                                          
         CLI   PDDS,1              DDS CAN DELETE ANY                           
         BE    VALENDC                                                          
         TM    TWAAUTH-TWAD(RA),X'80'  OR IF AUTH=8000                          
         BO    VALENDC                                                          
         OC    PCLI,PCLI           TEST CLIENT LEVEL                            
         BZ    EIIF                                                             
*                                                                               
VALENDC  TM    DMCB+8,X'10'        VALIDATE ACTION & RECORD STATUS              
         BZ    *+16                                                             
         CLI   PACTN,NEW                                                        
         BNE   ERNF                N/F ONLY VALID FOR ADD                       
         B     VALENDE                                                          
         CLI   PACTN,NEW                                                        
         BE    ERAE                F NOT VALID FOR ADD                          
         LA    R2,PROACTNH         POSN CURSOR TO ACTION                        
         TM    DMCB+8,X'02'                                                     
         BO    *+16                                                             
         CLI   PACTN,RESTORE                                                    
         BE    ERND                CAN'T RESTORE IF N/D                         
         B     VALENDE                                                          
         CLI   PACTN,DELETE                                                     
         BE    ERAD                CAN'T DELETE IF ALREADY DELETED              
         CLI   PACTN,CHANGE                                                     
         BE    ERID                CAN'T CHANGE IF DELETED                      
*                                                                               
VALENDE  CLI   PACTN,NEW           IF ACTION=CHANGE OR NEW AND A FIELD          
         BE    *+12                DEFN REC FOR THIS SYS/PRG NOT DISPYD         
         CLI   PACTN,CHANGE        SET MAJOR ACTION TO DISPLAY                  
         BNE   VALENDG                                                          
         CLC   PKEY,LKEY                                                        
         BE    *+12                                                             
         MVI   PACTN,DISPLAY                                                    
         B     VALENDH                                                          
*                                                                               
VALENDG  CLC   PKEY,LKEY                                                        
         BNE   VALENDH                                                          
         CLI   PFKEY,1             HELP REQUESTED?                              
         BNE   VALENDH                                                          
         MVI   PACTN,GOHELP                                                     
         B     VALENDI                                                          
*                                                                               
VALENDH  DS    0H                                                               
         CLI   PACTN,DISPLAY                                                    
         BE    VALENDI                                                          
         CLI   LASTSCRN,X'FE'      CAME FROM HELP                               
         BNE   VALENDI                                                          
         MVI   PACTN,DISPLAY                                                    
         XC    LASTSCRN,LASTSCRN                                                
*                                                                               
VALENDI  SR    R1,R1               NOW GO TO ROUTINES                           
         IC    R1,PACTN                                                         
         SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     ADDREC              ADD                                          
         B     DISREC              DISPLAY                                      
         B     CHAREC              CHANGE                                       
         B     DELREC              DELETE                                       
         B     RESREC              RESTORE                                      
         B     PFHELP              GO CHECK IF PFKEY WAS HIT                    
         EJECT                                                                  
***********************************************************************         
*        GET HELP RECORD                                              *         
***********************************************************************         
PFHELP   DS    0H                                                               
         CLI   CURSCRN,X'FE'       ALREADY HELP SCREEN                          
         BE    PFHELPX             YES - SKIP                                   
*                                                                               
         L     RF,=A(HELPTAB)                                                   
         A     RF,RELO                                                          
         CLC   CURDISP,0(RF)       IS IT BEFORE FIRST FIELD DESC?               
         BL    PFHELPX                                                          
*                                                                               
         LA    R7,HELPKEY          BUILD HELP KEY AND GET HELP RECORD           
         USING HV1KEYD,R7                                                       
         MVC   0(2,R7),=X'0002'    HELP RECORD ID                               
         MVC   HV1SYS,PSYSN        SYSTEM NUMBER                                
*                                                                               
PFHELP5  MVC   HV1PROF,PPRG        PROGRAM NAME                                 
         CLI   HV1PROF,0           FIRST BYTE NULLS?                            
         BNE   PFHELP7             NO - 3 CH PROGRAM NAME                       
         MVC   HV1PROF(2),PPRG+1   YES - SHIFT PROGRAM BY ONE BYTE              
         MVI   HV1PROF+2,0         CLEAR OUT LAST BYTE - 2 CH PGM NAME          
*                                                                               
PFHELP7  MVC   HV1LANG,LANG        LANGUAGE CODE                                
         XI    HV1LANG,X'FF'       FLIP BITS OF LANG CODE                       
         MVC   HV1PRFN,2(RF)       FIELD NUMBER                                 
*                                                                               
PFHELP10 DS    0H                                                               
         CLC   =X'0000',0(RF)      END OF TABLE                                 
         BE    PFHGO               GO GET HELP RECORD                           
         CLC   CURDISP,0(RF)       IS IT BEFORE FIRST FIELD DESC?               
         BL    PFHGO               GO GET HELP RECORD                           
         MVC   HV1PRFN,2(RF)       FIELD NUMBER                                 
         LA    RF,3(RF)                                                         
         B     PFHELP10                                                         
*                                                                               
PFHGO    DS    0H                  GO GET HELP RECORD                           
         MVC   HKEY,HELPKEY                                                     
         MVC   SVHKEY,HKEY         SAVE BUILT HELP KEY                          
*                                                                               
         GOTOR PDATAMGR,DMCB,=C'DMRDHI ',=C'GENDIR ',HKEY,HKEY,0                
         CLC   SVHKEY,HKEY         FOUND KEY                                    
         BE    PFH5                                                             
         LA    RF,CI#HLPNF         HELP RECORD NOT FOUND                        
         GOTOR INFOMSG                                                          
         B     PFHELPX                                                          
*                                                                               
PFH5     L     R8,APREC                                                         
         GOTOR PDATAMGR,DMCB,=C'GETREC ',=C'GENFIL ',HKEY+36,(R8),WORK          
*                                                                               
         BAS   RE,SAVEPROF         SAVE OFF PROFILES                            
*                                                                               
         GOTOR PCCALLOV,DMCB,(X'FE',PROHEREH),0     GET HELP SCREEN             
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         BAS   RE,XMTALL                                                        
*                                                                               
         MVC   LASTSCRN,CURSCRN                                                 
         MVI   CURSCRN,X'FE'       HELP SCREEN                                  
         DROP  R7                                                               
*                                                                               
PFH20    DS    0H                  PRINT OUT HELP RECORD                        
         L     R8,APREC                                                         
         LA    R5,42                                                            
         STH   R5,DATADISP                                                      
*                                                                               
         MVI   ELCODE,X'10'        GET HEADING ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               <<<<<<<                                      
*                                                                               
         USING HV1HEDD,R8                                                       
*                                                                               
         MVI   HLPHEAD,C'*'                                                     
         MVC   HLPHEAD+27(L'HV1HEDTX),HV1HEDTX                                  
         MVI   HLPHEAD+78,C'*'                                                  
         OI    HLPHEADH+6,X'80'    PRINT OUT HELP HEADER                        
         DROP  R8                                                               
*                                                                               
PFH30    DS    0H                  GET HELP TEXT                                
         LA    R2,HLPLIN1H         A(FIRST HELP LINE)                           
         LA    R4,16               # OF HELP LINES ALLOWED                      
*                                                                               
PFH32    MVI   8(R2),C'*'          PUT OUTLINE OF *'S                           
         MVI   8+78(R2),C'*'                                                    
         GOTOR BMPFLD                                                           
         BCT   R4,PFH32                                                         
*                                                                               
         LA    R2,HLPLIN1H         A(FIRST HELP LINE)                           
         LA    R4,16               # OF HELP LINES ALLOWED                      
         L     R8,APREC                                                         
         MVI   ELCODE,X'20'        TEXT ELEMENT                                 
         BAS   RE,GETEL                                                         
         BE    PFH35                                                            
         MVC   8(21,R2),=C'UH OH - NO HELP FOUND'                               
         OI    6(R2),X'80'                                                      
         B     EXIT                                                             
*                                                                               
PFH35    DS    0H                                                               
         USING HV1TXTD,R8                                                       
         ZIC   R3,1(R8)            ELEMENT LENGTH                               
         SHI   R3,7                SUBTRACT OF USELESS BYTES                    
         BCTR  R3,0                                                             
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R2),HV1TXTTX   LINE OF HELP TEXT                            
         OI    6(R2),X'80'                                                      
*                                                                               
         BRAS  RE,BMPFLD           BUMP TO NEXT LINE                            
         BAS   RE,NEXTEL           GET NEXT X'20' ELEMENT                       
         BNE   *+8                 THERE'S NO MORE                              
         BCT   R4,PFH35            PRINT OUT THE TEXT LINE                      
         LA    RF,CI#HLPDS         HELP DISPLAYED                               
         GOTOR INFOMSG                                                          
*                                                                               
PFHELPX  DS    0H                                                               
         DROP  R8                                                               
         USING CTUREC,R8           RESTORE SETTINGS BEFORE HELP                 
*                                                                               
         B     EXIT                                                             
*                                                                               
         GETEL R8,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        SAVE OFF PROFILES                                            *         
***********************************************************************         
SAVEPROF NTR1  ,                                                                
         LA    R2,PR2PRO2H                                                      
         L     R3,APROFTAB                                                      
         XC    0(APTABLN,R3),0(R3) CLEAR TWA AREA                               
         LA    R4,16                                                            
*                                                                               
SP10     DS    0H                                                               
         MVC   0(L'PR2PRO2,R3),8(R2) SAVE PROFILE                               
         BRAS  RE,BMPFLD                                                        
         BRAS  RE,BMPFLD                                                        
         LA    R3,L'PR2PRO2(R3)    NEXT TABLE ENTRY                             
         BCT   R4,SP10                                                          
*                                                                               
SPX      DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
*        DISPLAY PROFILES                                             *         
***********************************************************************         
                                                                                
DISREC   TWAXC PR2PRO1H,PROT=Y                                                  
         SR    RF,RF               MOVE PROFILE HEADING TO TWA FIELD            
         IC    RF,PLEVEL                                                        
         BCTR  RF,0                                                             
         SLL   RF,1                                                             
         L     R1,APHEADS                                                       
         AR    R1,RF                                                            
         LH    RF,0(R1)                                                         
         XC    DMCB(24),DMCB                                                    
         GOTOR PGETTXT,DMCB,(RF),PR2HEADH,(C'S',0),0,(X'04',0)                  
         OI    PR2HEADH+6,X'80'                                                 
*                                                                               
         LA    R8,PREC             R8=A(FIELD DEFINITION RECORD)                
         LA    R3,CTUDATA          R3=A(ELEMENT)                                
         LA    R7,SAVAREA          R7=A(VALIDATION PARAMETER LIST)              
         USING POVLAY,R7                                                        
         MVC   PRGNME,PPRG         SET PROGRAM NAME                             
         MVC   PACTION(1),PACTN2   SET ACTION                                   
         MVC   PSTAGE(1),PLEVEL    SET ID LEVEL                                 
*                                                                               
DISPREC6 CLI   0(R3),0                                                          
         BE    DISPEND                                                          
         CLI   0(R3),X'70'         DEFINITION ELEMENT                           
         BE    DISPRECA                                                         
*                                                                               
DISPREC8 SR    R4,R4               BUMP TO NEXT ELEMENT                         
         IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     DISPREC6                                                         
*                                  DISPLAY FIELD DEFINITION ELEMENT             
DISPRECA DS    0H                                                               
         USING CTFDD,R3                                                         
         SR    R4,R4                                                            
         IC    R4,CTFDNUM                                                       
         STC   R4,PFLDNUM                                                       
         BCTR  R4,0                                                             
         MHI   R4,92               L'SCREEN LINE                                
         LA    R4,PR2PRO1H(R4)                                                  
         USING PLINED,R4           R4=A(SCREEN LINE)                            
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         SHI   R1,27                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLDESC(0),CTFDDESC  MOVE DESCRIPTION                             
         MVC   PLLIST,CTFDLIST     MOVE VALUES                                  
         ZIC   R1,CTFDNUM                                                       
         LR    R5,R1                                                            
         LR    RE,R1                                                            
         LA    R1,PPROFA-1(R1)                                                  
         LA    R5,PROFOTHR-1(R5)                                                
         LA    RE,PPROFD-1(RE)     SHOW DEFAULT VALUE                           
         MVC   PDEFAULT,0(RE)                                                   
         MVC   PVALUE(1),PDEFAULT                                               
         GOTOR GETVALUE                                                         
         MVC   PLDEFT,WORK                                                      
         LA    R2,3                                                             
         LA    R6,PVALS                                                         
* !!!!!                                                                         
         BAS   RE,HASHELP          DOES THIS FIELD HAVE A HELP RECORD?          
*                                                                               
*                                                                               
DISPRECC MVC   PVALUE,0(R1)        GET ALL LEVEL VALUES                         
*&&US                                                                           
         ST    R1,SAVER1                                                        
         GOTOR CONOFF,DMCB,(1,PVALUE),(C'P',CTFDOTHR) CONVERT OFFICE            
         L     R1,SAVER1                                                        
         BNE   DR050                                                            
         MVC   WORK(L'PVALUE),SPACES                                            
         MVC   WORK(L'OFCOFC2),PVALUE                                           
         B     DISPRECD                                                         
*&&                                                                             
DR050    GOTOR GETVALUE                                                         
         ST    R1,SAVER1                                                        
         GOTOR PEXPRESS,DMCB,CTFDTYPE,CTFDLIST,WORK,PSCANNER                    
         L     R1,SAVER1                                                        
         CLI   DMCB,0              ERROR ?                                      
         BE    DISPRECD                                                         
         MVC   PVALUE,PDEFAULT     YES - SHOW DEFAULT VALUE INSTEAD             
         GOTOR GETVALUE                                                         
*                                                                               
DISPRECD MVC   0(4,R6),WORK                                                     
         LA    R1,16(R1)                                                        
         LA    R6,4(R6)                                                         
         BCT   R2,DISPRECC                                                      
         DROP  R3                                                               
*                                  DISPLAY PROFILE VALUES                       
         CLI   PLEVEL,1            AGENCY                                       
         BNE   DISPRECE                                                         
         MVC   PLVAL3,PVALA                                                     
         MVC   PPROFIL(4),PVALA                                                 
         LA    R7,OVLLN(R7)                                                     
         B     DISPRECH                                                         
*                                                                               
DISPRECE CLI   PLEVEL,2            AGENCY/MEDIA                                 
         BNE   DISPRECG                                                         
         MVC   PLVAL2,PVALA                                                     
         MVC   PLVAL3,PVALM                                                     
         MVC   0(4,R7),PVALM                                                    
         LA    R7,OVLLN(R7)                                                     
         B     DISPRECH                                                         
*                                                                               
DISPRECG MVC   PLVAL1,PVALA        AGENCY/MEDIA/CLIENT                          
         MVC   PLVAL2,PVALM                                                     
         MVC   PLVAL3,PVALC                                                     
         MVC   0(4,R7),PVALC                                                    
         LA    R7,OVLLN(R7)                                                     
*                                                                               
DISPRECH TM    0(R5),X'40'         CHECK FOR DDS ONLY FIELD                     
         BZ    DISPREC8                                                         
         GOTOR DDSAUTH                                                          
         BE    DISPREC8                  UNLESS AUTHORISED                      
         CLI   PDDS,0                                                           
         BNE   DISPREC8                                                         
         MVI   PLVAL3,C'*'         NO - INDICATE DDS ONLY FIELD                 
         MVC   PLVAL3+1(3),PVALC                                                
         MVC   0(4,R7),PVALC                                                    
         LA    R7,OVLLN(R7)                                                     
         B     DISPREC8                                                         
*                                  DECIDE WHY I CAME HERE AND OUTPUT            
DISPEND  XC    PROHDR,PROHDR       APPROPRIATE MESSAGE                          
         MVI   0(R7),X'FF'         END OF PROFILE                               
         LA    R2,PROACTNH                                                      
         CLI   PACTN2,DELETE                                                    
         BE    DISPEND2                                                         
         CLI   PACTN2,RESTORE                                                   
         BE    DISPEND4                                                         
         CLI   PACTN2,NEW                                                       
         BE    DISPEND6                                                         
         LA    R8,PREC2                                                         
         TM    CTUSTAT,X'80'                                                    
         BZ    DISPENDA                                                         
         LA    RF,CI#RDDEL                                                      
         B     DISPX                                                            
DISPENDA CLI   PACTN2,CHANGE                                                    
         BE    DISPENDB                                                         
*                                                                               
         LA    R3,CTUDATA          ADDRESS OF FIRST ELEMENT                     
         CLI   0(R3),CTACTELQ      ACTIVITY ELEMENT                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CTACTD,R3                                                        
         LA    RE,CTACTDT          ACTIVITY DATE                                
         ST    RE,DMCB                                                          
         MVI   DMCB,3              INPUT TYPE                                   
         OI    DMCB,X'80'          RETURN LENGTH OF OUTPUT                      
         GOTOR PDATCON,DMCB,,(5,ACTDATE)                                        
         DROP  R3                                                               
         LA    RE,ACTDATE                                                       
         ST    RE,GTLTXT                                                        
         MVC   GTLTXT(1),4(R1)     LENGTH OF OUTPUT                             
*                                                                               
         LA    RF,CI#LSTCH                                                      
         B     DISPX                                                            
DISPENDB LA    R2,PR2PRO2H                                                      
         LA    RF,CI#RDNEC                                                      
         B     DISPX                                                            
*                                                                               
DISPEND2 LA    RF,CI#RECDL                                                      
         B     DISPX                                                            
*                                                                               
DISPEND4 LA    RF,CI#RRCHG                                                      
         B     DISPX                                                            
*                                                                               
DISPEND6 LA    RF,CI#DEFED                                                      
         LA    R2,PR2PRO2H                                                      
*                                                                               
DISPX    GOTOR INFOMSG                                                          
         MVC   LKEY,PKEY                                                        
*                                                                               
         L     R3,APROFTAB                                                      
         OC    0(APTABLN,R3),0(R3) ANYTHING IN PROFILE TABLE?                   
         BZ    *+8                 NO - DIDN'T COME FROM HELP                   
         BRAS  RE,DISPPROF         DISPLAY PROFILES                             
*                                                                               
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        DOES FIELD HAVE HELP RECORD                                  *         
***********************************************************************         
HASHELP  NTR1  ,                                                                
         MVI   PLHELP,C' '                                                      
*                                                                               
         LR    RF,R4                                                            
         LA    R1,PROHDRH                                                       
         SR    RF,R1                                                            
         LA    RF,64(RF)                                                        
         STH   RF,CURDISP                                                       
*                                                                               
         L     RF,=A(HELPTAB)                                                   
         A     RF,RELO                                                          
         CLC   CURDISP,0(RF)       IS IT BEFORE FIRST FIELD DESC?               
         BL    HASHELPX                                                         
*                                                                               
         LA    R7,HELPKEY          BUILD HELP KEY AND GET HELP RECORD           
         USING HV1KEYD,R7                                                       
         MVC   0(2,R7),=X'0002'    HELP RECORD ID                               
         MVC   HV1SYS,PSYSN        SYSTEM NUMBER                                
*                                                                               
         MVC   HV1PROF,PPRG        PROGRAM NAME                                 
         CLI   HV1PROF,0           FIRST BYTE NULLS?                            
         BNE   HH7                 NO - 3 CH PROGRAM NAME                       
         MVC   HV1PROF(2),PPRG+1   YES - SHIFT PROGRAM BY ONE BYTE              
         MVI   HV1PROF+2,0         CLEAR OUT LAST BYTE - 2 CH PGM NAME          
*                                                                               
HH7      MVC   HV1LANG,LANG        LANGUAGE CODE                                
         XI    HV1LANG,X'FF'       FLIP BITS OF LANG CODE                       
         MVC   HV1PRFN,2(RF)       FIELD NUMBER                                 
*                                                                               
HH10     DS    0H                                                               
         CLC   =X'0000',0(RF)      END OF TABLE                                 
         BE    HHGO                GO GET HELP RECORD                           
         CLC   CURDISP,0(RF)       IS IT BEFORE FIRST FIELD DESC?               
         BL    HHGO                GO GET HELP RECORD                           
         MVC   HV1PRFN,2(RF)       FIELD NUMBER                                 
         LA    RF,3(RF)                                                         
         B     HH10                                                             
*                                                                               
HHGO     DS    0H                  GO GET HELP RECORD                           
         MVC   HKEY,HELPKEY                                                     
         MVC   SVHKEY,HKEY         SAVE BUILT HELP KEY                          
*                                                                               
         GOTOR PDATAMGR,DMCB,=C'DMRDHI ',=C'GENDIR ',HKEY,HKEY,0                
         CLC   SVHKEY,HKEY         FOUND KEY                                    
         BNE   HASHELPX                                                         
         MVI   PLHELP,C'='                                                      
*                                                                               
HASHELPX DS    0H                                                               
         NI    1(R4),X'FF'-X'08'                                                
         CLI   PLHELP,C'='         HELP RECORD HERE                             
         BNE   *+8                                                              
         OI    1(R4),X'08'         HIGH INTENSITY                               
         B     EXIT                                                             
         DROP  R7                                                               
***********************************************************************         
*        ADD/CHANGE RECORD                                            *         
***********************************************************************         
                                                                                
ADDREC   XC    CTUKEY(40),CTUKEY   BUILD KEY/LENGTH/STATUS                      
         MVC   CTUKEY,PKEY                                                      
         LA    R3,CTUDATA+1                                                     
         SR    R3,R8                                                            
         STH   R3,DUB                                                           
         MVC   CTULEN,DUB                                                       
         B     CHAREC2                                                          
*                                                                               
CHAREC   MVI   WORK,X'01'          STRIP OFF ELEMENTS                           
         GOTOR DELEL                                                            
         MVI   WORK,X'72'                                                       
         GOTOR DELEL                                                            
*                                  BUILD AND ADD ACTIVITY ELEMENT               
CHAREC2  MVC   WORK(2),=X'0105'                                                 
         GOTOR PDATCON,DMCB,(5,0),(3,WORK+2)                                    
*                                                                               
         LA    R7,PLIST            SET R7 TO PARAMETER LIST                     
*                                                                               
         GOTOR PUTEL               ADD ACTIVITY ELEMENT                         
*                                  GET A(PROFILE FOR THIS LEVEL)                
         SR    R1,R1                                                            
         IC    R1,PLEVEL                                                        
         BCTR  R1,0                                                             
         SLL   R1,4                                                             
         LA    R1,PPROFA(R1)                                                    
         ST    R1,APROF                                                         
         LA    R8,PREC                                                          
         LA    R3,CTUDATA                                                       
*                                  PROCESS DEFINITION RECORD                    
CHAREC6  CLI   0(R3),0                                                          
         BE    CHAEND                                                           
         CLI   0(R3),X'70'                                                      
         BE    CHARECA                                                          
*                                                                               
CHAREC8  SR    R4,R4               BUMP TO NEXT ELEMENT                         
         IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     CHAREC6                                                          
*                                                                               
CHARECA  DS    0H                  GET A(SCREEN LINE)                           
         USING CTFDD,R3                                                         
         ZIC   R4,CTFDNUM                                                       
         STC   R4,PFLDNUM                                                       
         LR    R5,R4                                                            
         LA    R5,PROFOTHR-1(R5)                                                
         BCTR  R4,0                                                             
         MHI   R4,92                                                            
         LA    R4,PR2PRO1H(R4)                                                  
         USING PLINED,R4                                                        
         MVC   PVALUE,SPACES                                                    
         CLI   PLDATAH+5,0         ANYTHING INPUT                               
         BE    CHARECC                                                          
         LA    R2,PLDATAH                                                       
         TM    0(R5),X'40'         DDS ONLY FIELD                               
         BZ    CHARECB                                                          
         GOTOR DDSAUTH                                                          
         BE    CHARECB                   UNLESS AUTHORISED                      
         CLI   PDDS,0                                                           
         BNE   CHARECB                                                          
         CLI   PLDATA,C'*'                                                      
         BNE   EISL                                                             
         B     CHAREC8                                                          
CHARECB  DS    0H                                                               
         SR    R1,R1               YES - EXTRACT IT                             
         IC    R1,PLDATAH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PVALUE(0),PLDATA                                                 
*&&US                                                                           
         GOTOR CONOFF,DMCB,(2,PVALUE),(C'P',CTFDOTHR) CONVERT OFFICE            
         BH    CHARECG             NO OFFICE CONVERSION NEEDED                  
         BL    EORN                INVALID OFFICE                               
         MVC   DMCB+4(L'PVALUE),PVALUE                                          
         B     CHARECI             GET NEXT ELEMENT                             
*&&                                                                             
         B     CHARECG                                                          
*                                                                               
CHARECC  CLI   PDDS,0              NO INPUT MAY BE BECAUSE USER                 
         BNE   CHARECCA            TERMS NOT ALLOWED TO CHANGE DDS              
         GOTOR DDSAUTH                                                          
         BE    CHARECCA                                                         
         TM    0(R5),X'40'                                                      
         BO    CHAREC8                                                          
CHARECCA TM    0(R5),X'08'         OR THEY PRESSED THE ENTER KEY TOO            
         BO    *+12                SOON SO IF THERE IS A DEFAULT VALUE          
         LA    R2,PLDATAH          USE IT                                       
         B     EMIF                                                             
         ZIC   R1,CTFDNUM                                                       
         BCTR  R1,0                                                             
         A     R1,APROF                                                         
         MVC   PVALUE(1),0(R1)     OUTPUT THE VALUE WE ASSUMED                  
         GOTOR GETVALUE                                                         
         MVC   PLDATA,WORK                                                      
         MVC   0(1,R7),PLDATA                                                   
         LA    R7,4(R7)                                                         
         OI    PLDATAH+6,X'80'                                                  
*&&US*&& GOTOR CONOFF,DMCB,(1,PLDATA),(C'P',CTFDOTHR) CONVERT OFFICE            
         B     CHAREC8                                                          
*                                  PASS INPUT TO EXPRESS FOR VALIDTN            
CHARECG  GOTOR PEXPRESS,DMCB,CTFDTYPE,CTFDLIST,PVALUE,PSCANNER                  
         CLI   0(R1),0             ERROR ?                                      
         BE    CHARECI                                                          
         CLI   0(R1),3             YES - MUST BE ON 3RD PARAM                   
         BE    *+6                                                              
         DC    H'0'                DIE IF IT ISN'T                              
         L     R1,16(R1)                                                        
         MVC   PROHDR,0(R1)        SET RETURN ERROR MESSAGE                     
         OI    PROHDRH+6,X'80'                                                  
         OI    PLDATAH+6,X'40'     POSISTION CURSOR & EXIT                      
         B     EXIT                                                             
*                                                                               
CHARECI  SR    R1,R1               MOVE VALUE TO PROFILE                        
         IC    R1,CTFDNUM                                                       
         BCTR  R1,0                                                             
         A     R1,APROF                                                         
         MVC   0(1,R1),DMCB+4                                                   
         MVC   0(1,R7),DMCB+4                                                   
         LA    R7,4(R7)                                                         
         B     CHAREC8             GET NEXT ELEMENT                             
*                                                                               
CHAEND   XC    WORK,WORK           BUILD A PROFILE ELEMENT                      
         SR    R5,R5                                                            
         LA    R5,SAVAREA                                                       
         USING POVLAY,R5                                                        
         LA    R7,PLIST                                                         
         LA    R7,64(R7)                                                        
         MVC   0(3,R7),PRGNME      MOVE PROGRAM NAME TO PARAMETER LIST          
         LA    R7,3(R7)                                                         
         MVC   0(1,R7),PACTION     MOVE ACTION TO PARAMETER LIST                
         LA    R7,1(R7)                                                         
         MVC   0(1,R7),PSTAGE      MOVE ID LEVEL TO PARAMETER LIST              
         LA    R7,1(R7)                                                         
CHAENDA  CLI   0(R5),X'FF'         MOVE OLD PROFILE TO PARAMETER LIST           
         BE    CHAEND1                                                          
         MVC   0(4,R7),PPROFIL                                                  
         LA    R5,OVLLN(R5)                                                     
         LA    R7,4(R7)                                                         
         B     CHAENDA                                                          
*                                                                               
         DROP  R3                                                               
         DROP  R5                                                               
*                                                                               
CHAEND1  XC    WORK,WORK                                                        
         CLI   PACTN,DELETE                                                     
         BE    CHAEND2                                                          
         BAS   RE,VALPROF                                                       
         BZ    CHAEND2            INCORRECT PROFILE VALUES ?                    
         LA    R2,PR2PRO2H        YES                                           
         C     R1,=F'1'                                                         
         BE    ERPV                                                             
         LA    R2,8+L'PR2PRO2(R2)  POSITION CURSOR ON WRONG PROFILE             
         LA    R2,8+L'PR2PRO1(R2)                                               
         BCT   R1,*-8                                                           
         B     ERPV                                                             
*                                                                               
CHAEND2  MVC   WORK(2),=X'7215'                                                 
         LA    R3,WORK                                                          
         USING CTPVD,R3                                                         
         L     R1,APROF                                                         
         MVC   CTPVALUE,0(R1)                                                   
         LA    R8,PREC2            SET A(I/O)                                   
         ST    R8,APREC                                                         
         GOTOR PUTEL               ADD PROFILE ELEMENT                          
         XC    PROHDR,PROHDR                                                    
*                                  ADD/WRITE RECORD & RETURN                    
         CLI   PACTN,NEW                                                        
         BNE   CHAEND3                                                          
         GOTOR ADD                                                              
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    RF,CI#RECAD                                                      
         B     CHAX                                                             
*                                                                               
CHAEND3  GOTOR WRITE                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    RF,CI#RECCG                                                      
         B     CHAX                                                             
*                                                                               
CHAX     LA    R2,PROACTNH                                                      
         GOTOR INFOMSG                                                          
         EJECT                                                                  
***********************************************************************         
* SWITCH TO SYSTEM TO WHICH PROFILE BELONGS                           *         
* AND GENERATE TURNAROUND REQUEST                                     *         
***********************************************************************         
*&&US                                                                           
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),PSYSSEN                                                  
         GOTOR PSWITCH,DMCB                                                     
         CLI   4(R1),0                                                          
         BNE   SWERR                                                            
*&&                                                                             
         LA    R5,PREC                                                          
         USING REQD,R5                                                          
         XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
         MVI   REQNUMB,74                                                       
         MVC   REQPROG,=C'74'                                                   
         MVI   REQPROG+2,C'*'      SET T/A IND                                  
         MVC   REQAGYA,PAGYA                                                    
*&&US                                                                           
         L     R1,PPFACS                                                        
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   PSYSSEN,SESYS-SELISTD(R1)                                        
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   REQUEST+59(1),PAGY                                               
         TM    PSYSI,X'80'                                                      
         BO    *+10                                                             
         MVC   REQUEST+59(2),PAGYA                                              
         MVC   REQUEST+61(1),0(R1)                                              
*                                                                               
         CLI   SEOVSYS-SELISTD(R1),X'0D'   TEST SPOT TRAFFIC SYSTEM             
         BNE   *+8                                                              
         MVI   REQUEST+61,C'F'                                                  
*                                                                               
         LA    R1,L'SENAME-1(R1)                                                
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   REQUEST+62(1),0(R1)                                              
         MVC   REQUEST+63(3),PPRG                                               
         CLI   PPRG,0                                                           
         BNE   *+8                                                              
         MVI   REQUEST+63,C' '                                                  
         MVC   REQSTOR,=C'FILE CONTROL'                                         
         MVC   REQUEST+62(1),PSYSSEN                                            
         MVI   REQSTOR+4,C'#'      SET PASSING SE NUMBER                        
*                                                                               
* SKIP T/A FOR SPOT WL PROFILE                                                  
         CLI   REQUEST+61,C'S'                                                  
         BNE   *+14                                                             
         CLC   =C' WL',REQUEST+63                                               
         BE    TA10                                                             
*                                                                               
* SKIP T/A FOR CONTROL PROFILE                                                  
         CLI   REQUEST+61,C'C'                                                  
         BE    TA10                                                             
*                                                                               
         GOTOR PDATAMGR,DMCB,=C'DMADD',=C'REQUEST',(R5),(R5)                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T ADD REQUEST                     
* SWITCH BACK TO CONTROL                                                        
TA10     GOTOR PSWITCH,DMCB,=C'CON',0                                           
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         DROP  R5                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PROFILE                                             *         
***********************************************************************         
                                                                                
VALPROF  ST    RE,SAVEREG                                                       
         LA    R7,PLIST                                                         
         L     R5,ASYSTAB                                                       
         USING SYSLSTD,R5                                                       
VALPROA  CLI   SYSLNUM,0           END OF LIST                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SYSLNUM,PSYSN       MATCH TABLE WITH INPUT                       
         BE    *+12                                                             
         LA    R5,SYSLLEN(R5)      BUMP TO NEXT ENTRY                           
         B     VALPROA                                                          
*                                                                               
VALPROB  MVC   PHASNO(1),PSYSN                                                  
         DS    0H                                                               
         GOTOR PCCALLOV,DMCB,(PHASNO,0),0                                       
         CLI   4(R1),X'FF'                                                      
         BE    LOADERR             DISPLAY REC IF NOT ALREADY DISPLYD           
         L     R6,0(R1)                                                         
         GOTOR (R6),DMCB,PHASLV,R7,PDATVAL                                      
         BAS   RE,XMTALL                                                        
         L     RE,SAVEREG                                                       
         BR    RE                                                               
                                                                                
LOADERR  SR    R5,R5               NO VALIDATION MODULE EXISTS                  
         LTR   R5,R5               SET CC TO ZERO                               
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SET ON ALL XMT BITS                                                           
*                                                                               
XMTALL   NTR1  ,                                                                
         LA    RE,64(RA)                                                        
XMTALL2  OI    6(RE),X'80'                                                      
         SR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   XMTALL2                                                          
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        DELETE A PROFILE RECORD                                      *         
***********************************************************************         
                                                                                
DELREC   LA    R2,PROACTNH                                                      
         L     R3,APREC                                                         
         USING CTUREC,R8                                                        
         OI    CTUSTAT,X'80'                                                    
         GOTOR WRITE                                                            
         BZ    EIIO                                                             
         CLC   LKEY,CTUKEY                                                      
         BE    *+12                                                             
         MVI   PACTN,DISPLAY                                                    
         B     DISREC              DISPLAY REC IF NOT ALREADY DISPLYD           
         XC    PROHDR,PROHDR                                                    
         LA    RF,CI#RECDL                                                      
         GOTOR INFOMSG                                                          
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        RESTORE A PROFILE RECORD                                     *         
***********************************************************************         
                                                                                
RESREC   LA    R2,PROACTNH                                                      
         L     R8,APREC                                                         
         NI    CTUSTAT,X'7F'                                                    
         GOTOR WRITE                                                            
         BZ    EIIO                                                             
         CLC   LKEY,CTUKEY                                                      
         BE    *+12                                                             
         MVI   PACTN,DISPLAY                                                    
         B     DISREC              DISPLAY REC IF NOT ALREADY DISPLYD           
         XC    PROHDR,PROHDR                                                    
         LA    RF,CI#RECRS                                                      
         GOTOR INFOMSG                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        EXTRACT A USER PROFILE FROM RECORD                           *         
***********************************************************************         
                                                                                
GETPROF  NTR1  ,                                                                
         L     R8,APREC            R8=A(RECORD)                                 
         USING CTUREC,R8                                                        
         LA    R3,CTUDATA          R3=A(FIRST ELEMENT)                          
         SR    R4,R4                                                            
*                                                                               
GETPROF2 CLI   0(R3),0             DIE IF N/F                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'72'                                                      
         BE    GETPROF6                                                         
*                                                                               
GETPROF4 IC    R4,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R4                                                            
         B     GETPROF2                                                         
*                                                                               
GETPROF6 DS    0H                                                               
         USING CTPVD,R3                                                         
         MVC   0(16,R1),CTPVALUE   EXTRACT PROFILE                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        DECODE A 1 BYTE VALUE INTO WORK                              *         
***********************************************************************         
                                                                                
GETVALUE NTR1  ,                                                                
         USING CTFDD,R3                                                         
         MVC   WORK(4),SPACES                                                   
         TM    0(R5),X'08'                                                      
         BO    *+12                                                             
         CLI   PVALUE,0                                                         
         BE    EXIT                                                             
         MVC   WORK(1),PVALUE                                                   
         CLI   CTFDTYPE,C'C'                                                    
         BE    EXIT                                                             
         CLI   CTFDTYPE,C'X'                                                    
         BE    GETVAL2                                                          
         EDIT  (B1,PVALUE),(3,WORK),ALIGN=LEFT,WRK=WORK+4,ZERO=NOBLANK          
         OI    WORK,X'F0'                                                       
         B     EXIT                                                             
*                                                                               
GETVAL2  GOTOR PHEXOUT,DMCB,PVALUE,WORK,1,=C'TOG'                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        ADD/DELETE AN ELEMENT FROM RECORD                            *         
***********************************************************************         
                                                                                
DELEL    NTR1  ,                                                                
         L     R8,APREC                                                         
         GOTOR PHELLO,DMCB,(C'D',=C'CTFILE '),(WORK,(R8)),0                     
         B     EXIT                                                             
*                                                                               
PUTEL    NTR1  ,                                                                
         L     R8,APREC                                                         
         GOTOR PHELLO,DMCB,(C'P',=C'CTFILE '),(R8),WORK                         
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        DATAMGR INTERFACES                                           *         
***********************************************************************         
                                                                                
READHI   NTR1  ,                                                                
         LA    R0,=C'DMRDHI  '                                                  
         MVI   RFLAG,X'08'                                                      
         B     DMIO                                                             
READ     NTR1  ,                                                                
         LA    R0,=C'DMREAD  '                                                  
         MVI   RFLAG,X'08'                                                      
         B     DMIO                                                             
READUP   NTR1  ,                                                                
         LA    R0,=C'DMREAD  '                                                  
         MVI   RFLAG,X'88'                                                      
         CLI   PACTN,DISPLAY       ALLOW ONLY DISPLAY                           
         BNE   *+8                                                              
         MVI   RFLAG,X'08'         THEN NO RFFU                                 
         B     DMIO                                                             
*                                                                               
WRITE    NTR1  ,                                                                
         LA    R0,=C'DMWRT   '                                                  
         MVI   RFLAG,X'08'                                                      
         B     DMIO                                                             
ADD      NTR1  ,                                                                
         LA    R0,=C'DMADD   '                                                  
         MVI   RFLAG,X'08'                                                      
         B     DMIO                                                             
*                                                                               
DMIO     L     R8,APREC                                                         
         MVI   ERROR,X'FF'                                                      
         GOTOR PDATAMGR,DMCB,(RFLAG,(R0)),=C'CTFILE ',PKEY,(R8),0               
         TM    8(R1),X'ED'                                                      
         BZ    *+8                                                              
         MVI   ERROR,0             SET CC=EQ ON FUNNY                           
         CLI   ERROR,0                                                          
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*                  INFO MESSAGES                            *                   
*************************************************************                   
                                                                                
INFOMSG  NTR1  ,                                                                
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+12(4),GTLTXT     OPTNL TEXT STRING (ACTIVITY DATE)          
         GOTOR PGETTXT,DMCB,(RF),0,(C'I',0)                                     
         OI    6(R2),X'40'                                                      
         OI    PROHDRH+6,X'80'                                                  
         XC    GTLTXT,GTLTXT                                                    
         B     EXIT                                                             
                                                                                
*************************************************************                   
*                       ERROR MESSAGES                      *                   
*************************************************************                   
                                                                                
EIIO     LA    RF,0                                                             
         B     ERRX                                                             
EMIF     LA    RF,CE#MISIF                                                      
         B     ERRX                                                             
EIIF     LA    RF,CE#INVIF                                                      
         B     ERRX                                                             
EFNH     LA    RF,CE#FNOTX                                                      
         B     ERRX                                                             
EFTS     LA    RF,CE#FLMIN                                                      
         B     ERRX                                                             
EFTL     LA    RF,CE#FLMAX                                                      
         B     ERRX                                                             
ERNF     LA    RF,CE#RECNF                                                      
         B     ERRX                                                             
ERAE     LA    RF,CE#RECAE                                                      
         B     ERRX                                                             
ERAD     LA    RF,CE#NDREC                                                      
         B     ERRX                                                             
ERND     LA    RF,CE#CRRND                                                      
         B     ERRX                                                             
ERID     LA    RF,CE#RECDE                                                      
         B     ERRX                                                             
ERPV     LA    RF,CE#INCPV                                                      
         B     ERRX                                                             
EISL     LA    RF,CE#SECLO                                                      
         B     ERRX                                                             
EUAU     LA    RF,CE#UIDAU                                                      
         B     ERRX                                                             
EUID     LA    RF,CE#INUID                                                      
         B     ERRX                                                             
EAGP     LA    RF,CE#AGYPR                                                      
         B     ERRX                                                             
EORN     LA    RF,CE#OFRNF         OFFICE RECORD NOT FOUND                      
         B     ERRX                                                             
EUCP     LA    RF,CE#MUSP          USER ID LEVEL CHNG FROM PRINCIPAL ID         
         B     ERRX                                                             
ENOL     LA    RF,113              NO OFFICE LIST SUPPORT HERE                  
         B     ERRX                                                             
SWERR    DS    0H                                                               
         GOTOR PSWITCH,DMCB,=C'CON',0  SWITCH BACK TO CONTROL                   
         LA    RF,56                                                            
         B     ERRX                                                             
*                                  READ MSG FROM CTFILE                         
ERRX     MVC   DMCB2(24),DMCB      SAVE DMCB FOR IO ERROR ANALYSIS              
         LA    R0,DMCB2                                                         
         XC    DMCB(24),DMCB                                                    
         GOTOR PGETTXT,DMCB,(RF),0,(C'E',(R0))                                  
         OI    PROHDRH+6,X'80'                                                  
         OI    6(R2),X'40'                                                      
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALID CHARS FOR WRITER NAME                                                   
VALCHARS DC    CL36'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                       
*                                                                               
* TABLE OF VALID ACTIONS                                                        
*        LA    RF,S(TEXT),ACTNUM,FLAGS                                          
*                                                                               
* ACTION EQUATES                                                                
*                                                                               
NEW      EQU   1                   ADD                                          
DISPLAY  EQU   2                   DISPLAY                                      
CHANGE   EQU   3                   CHANGE                                       
DELETE   EQU   4                   DELETE                                       
RESTORE  EQU   5                   RESTORE                                      
GOHELP   EQU   6                                                                
                                                                                
         DS    0F                                                               
ACTNTAB  DS    0CL6                                                             
         DC    X'41F0',SL2(CT@ADD),AL1(1),X'00'                                 
         DC    X'41F0',SL2(CT@NEW),AL1(1),X'00'                                 
         DC    X'41F0',SL2(CT@DSP),AL1(2),X'00'                                 
         DC    X'41F0',SL2(CT@ENQ),AL1(2),X'00'                                 
         DC    X'41F0',SL2(CT@INQ),AL1(2),X'00'                                 
         DC    X'41F0',SL2(CT@CHG),AL1(3),X'00'                                 
         DC    X'41F0',SL2(CT@AMEND),AL1(3),X'00'                               
         DC    X'41F0',SL2(CT@ALTER),AL1(3),X'00'                               
         DC    X'41F0',SL2(CT@DEL),AL1(4),X'80'                                 
         DC    X'41F0',SL2(CT@RSR),AL1(5),X'80'                                 
         DC    X'FFFF'                                                          
                                                                                
DDDCLST  DS    0C                                                               
*                                                                               
         DCDDL CT#DSP,8,L                                                       
         DCDDL CT#ENQ,8,L                                                       
         DCDDL CT#INQ,8,L                                                       
         DCDDL CT#CHG,8,L                                                       
         DCDDL CT#ALTER,8,L                                                     
         DCDDL CT#AMEND,8,L                                                     
         DCDDL CT#ADD,8,L                                                       
         DCDDL CT#NEW,8,L                                                       
         DCDDL CT#DEL,8,L                                                       
         DCDDL CT#RSR,8,L                                                       
*                                                                               
         DCDDL CT#YES,3,L                                                       
         DCDDL CT#ALL,4,L                                                       
*                                                                               
* HEADLINES                                                                     
*                                                                               
PHEADS   DS    0H                                                               
         DC    AL2(CS#PROH1)                                                    
         DC    AL2(CS#PROH2)                                                    
         DC    AL2(CS#PROH3)                                                    
*                                                                               
PHEADSU  DS    0H                                                               
         DC    AL2(CS#PROH4)                                                    
         DC    AL2(CS#PROH5)                                                    
         DC    AL2(CS#PROH6)                                                    
                                                                                
       ++INCLUDE SRDQUPROF                                                      
*                                  TABLE FOR CALCULATING HELP NUMBER            
HELPTAB  DS    0CL3                                                             
         DC    AL2(PR2PRO1H-PROHDRH+64),X'01'                                   
         DC    AL2(PR2PRO3H-PROHDRH+64),X'02'                                   
         DC    AL2(PR2PRO4H-PROHDRH+64),X'03'                                   
         DC    AL2(PR2PRO5H-PROHDRH+64),X'04'                                   
         DC    AL2(PR2PRO6H-PROHDRH+64),X'05'                                   
         DC    AL2(PR2PRO7H-PROHDRH+64),X'06'                                   
         DC    AL2(PR2PRO8H-PROHDRH+64),X'07'                                   
         DC    AL2(PR2PRO9H-PROHDRH+64),X'08'                                   
         DC    AL2(PR2PR10H-PROHDRH+64),X'09'                                   
         DC    AL2(PR2PR11H-PROHDRH+64),X'0A'                                   
         DC    AL2(PR2PR12H-PROHDRH+64),X'0B'                                   
         DC    AL2(PR2PR13H-PROHDRH+64),X'0C'                                   
         DC    AL2(PR2PR14H-PROHDRH+64),X'0D'                                   
         DC    AL2(PR2PR15H-PROHDRH+64),X'0E'                                   
         DC    AL2(PR2PR16H-PROHDRH+64),X'0F'                                   
         DC    AL2(PR2PR17H-PROHDRH+64),X'10'                                   
         DC    X'0000'                                                          
*                                                                               
* PROFILES THAT ARE SPOT AND NET                                                
*                                                                               
SPOTNET  DC    C'A8 '                                                           
         DC    C'B1 '                                                           
         DC    C'B1X'                                                           
         DC    C'B2 '                                                           
         DC    C'B2A'                                                           
         DC    C'B2B'                                                           
         DC    C'B3 '                                                           
         DC    C'B4 '                                                           
         DC    C'B5 '                                                           
         DC    C'B6 '                                                           
         DC    C'B7 '                                                           
         DC    C'B4A'                                                           
         DC    C'B5A'                                                           
         DC    C'B6A'                                                           
         DC    C'B7A'                                                           
         DC    C'BT '                                                           
         DC    C'EZ '                                                           
         DC    C'I2 '                                                           
         DC    C'U2 '                                                           
         DC    C'I2S'                                                           
         DC    C'I2X'                                                           
         DC    C'I2Y'                                                           
         DC    C'I2Z'                                                           
         DC    C'LK '                                                           
         DC    C'TI '                                                           
         DC    C'Z5 '                                                           
         DC    X'FFFFFF'                                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        BUMP FIELD ON SCREEN                                         *         
***********************************************************************         
BMPFLD   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
*        CHECK DDS ONLY AUTHORISATION                                 *         
***********************************************************************         
DDSAUTH  NTR1  BASE=*                                                           
*                                                                               
         TM    PSECINDS,SECIOLD    OLD SECURITY?                                
         BO    DAUT020                                                          
         B     DAUTNO              NO ACCESS                                    
*                                                                               
DAUT020  TM    TWAAUTH-TWAD(RA),X'20'                                           
         BNO   DAUTNO                                                           
DAUT021  EQU   *                                                                
*&&US*&& B     DAUTOK                                                           
*                                  NOT WITH M00 AND M00A PROFILES IN UK         
*&&UK                              NOR WITH =BUY PROFILES IN UK                 
         CLI   PSYSA,C'M'                                                       
         BNE   DAUTOK                                                           
         CLC   PPRG(3),=CL3'BUY'                                                
         BE    DAUTNO                                                           
         CLC   PPRG(3),=CL3'00A'                                                
         BE    DAUTNO                                                           
         CLI   PPRG,X'00'                                                       
         BNE   DAUTOK                                                           
         CLC   PPRG+1(2),=CL2'00'                                               
         BNE   DAUTOK                                                           
         CLI   PFLDNUM,4                                                        
         BE    DAUTOK                                                           
         CLI   PFLDNUM,6                                                        
         BE    DAUTOK                                                           
         CLI   PFLDNUM,7                                                        
         BE    DAUTOK                                                           
         CLI   PFLDNUM,8                                                        
         BE    DAUTOK                                                           
         CLI   PFLDNUM,9                                                        
         BE    DAUTOK                                                           
         CLI   PFLDNUM,10                                                       
         BE    DAUTOK                                                           
         CLI   PFLDNUM,11                                                       
         BE    DAUTOK                                                           
         CLI   PFLDNUM,12                                                       
         BE    DAUTOK                                                           
         CLI   PFLDNUM,13                                                       
         BE    DAUTOK                                                           
         CLI   PFLDNUM,14                                                       
         BE    DAUTOK                                                           
         CLI   PFLDNUM,16                                                       
         BE    DAUTOK                                                           
         B     DAUTNO                                                           
*&&                                                                             
DAUTNO   SR    RF,RF                                                            
         B     DAUTX                                                            
DAUTOK   LR    RF,RE                                                            
DAUTX    CR    RF,RE                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        DISPLAY PROFILES                                             *         
***********************************************************************         
DISPPROF NTR1  BASE=*,LABEL=*                                                   
         LA    R2,PR2PRO2H                                                      
         L     R3,APROFTAB                                                      
         LA    R4,16                                                            
*                                                                               
DP10     DS    0H                                                               
         MVC   8(L'PR2PRO2,R2),0(R3)                                            
         OI    6(R2),X'80'                                                      
         BRAS  RE,BMPFLD                                                        
         BRAS  RE,BMPFLD                                                        
         LA    R3,L'PR2PRO2(R3)                                                 
         BCT   R4,DP10                                                          
*                                                                               
DPX      DS    0H                                                               
         L     R3,APROFTAB                                                      
         XC    0(APTABLN,R3),0(R3)                                              
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
***********************************************************************         
* CHECK OFFICE LISTS                                                            
***********************************************************************         
*&&US                                                                           
CHKOFL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING OFFICED,R5                                                       
         MVC   OFCMOL,PPRG+1                                                    
         CLI   PPRG,C'$'                                                        
         BE    *+10                                                             
         MVC   OFCMOL,PPRG+2       OFFICE LIST VALUE                            
         MVC   OFCSYS,PSYSA        SYSTEM ID                                    
         MVC   OFCAGY,PAGYA        ALPHA AGENCY                                 
         OI    OFCINDS,OFCIMOLC    CONVERT TWO CHAR OFFICE LIST                 
*                                                                               
         GOTO1 POFFICER,DMCB,(C'2',WORK),PPCFACS                                
         JNE   EXITLOX             INVALID OFFICE LIST                          
*                                                                               
         USING CTUREC,R8           BUILD MAIN OFFICE LIST RECORD                
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKEY,CTUKTYPQ                                                  
         MVC   CTUKSYS,PSYSA                                                    
         MVI   CTUKPROG+1,C'$'                                                  
         MVC   CTUKPROG+2(1),OFCMOL                                             
         MVC   CTUKAGY,PAGYA                                                    
         MVC   PKEY,CTUKEY                                                      
*                                                                               
         LA    R0,=C'DMREAD  '                                                  
         GOTOR PDATAMGR,DMCB,(X'08',(R0)),=C'CTFILE ',PKEY,APREC,0              
         CLI   8(R1),0                                                          
         BE    CKMO10                                                           
         TM    8(R1),X'02'                                                      
         BO    CKMO10                                                           
         B     EXITEQX                                                          
*                                                                               
CKMO10   LHI   RE,CTUDATA-CTUREC                                                
         STH   RE,DATADISP                                                      
         MVI   ELCODE,CTOFELQ      GET NEW OFFICE LIST ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    EXITHIX             NEW LIST PROGRAM NEEDED                      
         J     EXITEQX                                                          
         DROP  R5                                                               
         LTORG                                                                  
*&&                                                                             
***********************************************************************         
* CONVERT MEDIA OFFICE                                                          
*         ON ENTRY P1 BYTE   0 IS LENGTH OF OFFICE PASSED (1 OR 2)              
*                  P1 BYTE 1-3 IS A(OFFICE)                                     
*                  P2 BYTE   0 IS C=CLIENT FIELD                                
*                                 P=PROFILE                                     
*                  P2 BYTE 1-3 IS A(PROFILE INDICATOR - CTFDOTHR)               
***********************************************************************         
CONOFF   NTR1  BASE=*,LABEL=*                                                   
         MVC   DUB+1(1),0(R1)            OFFICE LENGTH                          
         L     R2,0(R1)                  A(OFFICE FIELD)                        
         L     R3,4(R1)                  A(PROFILE TYPE)                        
*                                                                               
         CLI   4(R1),C'C'                CLIENT FIELD?                          
         BNE   COFF010                                                          
         CLI   PCLI,C'*'                 OFFICE?                                
         BNE   EXITHIX                                                          
         LA    R2,1(R2)                  BUMP PAST STAR                         
         B     COFF020                                                          
*                                                                               
COFF010  TM    0(R3),X'20'               VALUE IS A MEDIA OFFICE?               
         BO    COFF015                                                          
         CLI   PPRG,C'$'                 OFFICE LIST?                           
         BE    COFF015                                                          
         CLI   PPRG,0                    OR COULD BE X'00'C'$N'                 
         BNE   EXITHIX                                                          
         CLI   PPRG+1,C'$'               OFFICE LIST?                           
         BNE   EXITHIX                                                          
*                                                                               
COFF015  CLI   0(R2),C'*'                OFF PROF OF * MEANS NO OFFICE          
         BE    EXITHIX                                                          
         CLI   0(R2),C'0'                OFF PROF OF 0 MEANS NO OFFICE          
         BNE   COFF020                                                          
         CLI   1(R2),C' '                MAKE SURE IT'S ZERO-BLANK              
         BNH   EXITHIX                                                          
*                                                                               
COFF020  MVI   DUB,C'S'                                                         
         CLI   PSYSN,2                   SPOT                                   
         BE    COFF030                                                          
         CLI   PSYSN,13                  SPOT TRAFFIC                           
         BE    COFF030                                                          
         MVI   DUB,C'N'                                                         
         CLI   PSYSN,3                   NETWORK                                
         BE    COFF030                                                          
         MVI   DUB,C'P'                                                         
         CLI   PSYSN,4                   PRINT                                  
         BNE   EXITHIX                                                          
*                                                                               
COFF030  LA    R5,WORK                                                          
         USING OFFICED,R5                                                       
         XC    WORK,WORK                                                        
         MVC   OFCSYS,DUB                SYSTEM ID                              
         MVC   OFCAGY,PAGYA              ALPHA AGENCY                           
*                                                                               
         CLI   DUB+1,1                   ONE BYTE OFFICE?                       
         BNE   COFF040                   . NO                                   
         MVC   OFCOFC,0(R2)              ONE BYTE OFFICE                        
         B     COFF050                                                          
COFF040  MVC   OFCOFC2,0(R2)             TWO BYTE OFFICE                        
         DROP  R5                                                               
*                                                                               
COFF050  GOTO1 POFFICER,DMCB,(C'2',WORK),PPCFACS                                
*                                                                               
         LA    R5,WORK                                                          
         USING OFFICED,R5                                                       
         TM    OFCINDS,OFCINOLA          NOT USING TWO CHAR OFFICES             
         BO    EXITHIX                                                          
         TM    OFCINDS,OFCIOINV          INVALID                                
         BO    EXITLOX                                                          
         CLI   DUB+1,1                   ONE BYTE OFFICE?                       
         BNE   COFF060                   . NO                                   
         MVC   0(L'OFCOFC2,R2),OFCOFC2   2 BYTE OFFICE CODE                     
         B     EXITEQX                                                          
COFF060  MVC   0(L'OFCOFC,R2),OFCOFC     1 BYTE OFFICE CODE                     
         MVI   L'OFCOFC(R2),C' '         CLEAR OUT SECOND BYTE                  
         B     EXITEQX                                                          
         DROP  R5                                                               
*                                                                               
EXITHIX  LHI   R1,2                      HIGH                                   
         J     EXITOKX                                                          
EXITLOX  LHI   R1,0                      LOW                                    
         J     EXITOKX                                                          
EXITEQX  LHI   R1,1                      EQUAL                                  
EXITOKX  CHI   R1,1                                                             
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* DESECT TO COVER WORKING STORAGE                                               
***********************************************************************         
PWRKD    DSECT                                                                  
DUB      DS    D                   GENRAL W/S                                   
DUB1     DS    D                   *                                            
DMCB     DS    6F                  *                                            
DMCB2    DS    6F                  *                                            
WORK     DS    CL80                *                                            
SPACES   DS    CL80                *                                            
ERROR    DS    CL1                 ERROR NUM (FOR GETMSG)                       
RFLAG    DS    XL1                                                              
BYTE     DS    X                                                                
*                                  V-TYPES                                      
RELO     DS    A                   *                                            
PEXPRESS DS    A                   *                                            
PSIXPACK DS    A                   *                                            
PDATAMGR DS    A                   *                                            
PGETMSG  DS    A                   *                                            
PHELLO   DS    A                   *                                            
PHEXIN   DS    A                   *                                            
PHEXOUT  DS    A                   *                                            
PDATVAL  DS    A                   *                                            
PCCALLOV DS    A                   *                                            
PSCANNER DS    A                   *                                            
PSWITCH  DS    A                   *                                            
PGETIDS  DS    A                   *                                            
POFFICER DS    A                   *                                            
PGETTXT  DS    A                   *                                            
PDICTATE DS    A                   *                                            
ASYSTAB  DS    A                   *                                            
PDATCON  DS    A                   *                                            
PGETHELP DS    A                   *                                            
PSECRET  DS    A                   *                                            
*                                  A-TYPES                                      
APREC    DS    A                   A(RECORD)                                    
APROF    DS    A                   A(THIS PROFILE)                              
*                                  PARAMETER LIST                               
PPARMS   DS    0CL32               *                                            
PPFACS   DS    A                   *                                            
PPTIA    DS    A                   *                                            
PPUTL    DS    A                   *                                            
PPCFACS  DS    A                   *                                            
PPSE     DS    A                   *                                            
PPTWA    DS    A                   *                                            
         DS    A                   *                                            
PPTIOB   DS    A                   *                                            
*                                  GENERAL W/S                                  
SAVER1   DS    A                   SAVED R1 VALUE                               
APHEADS  DS    A                   A(PROFILE VALUE HEADLINE)                    
ASECBLK  DS    A                   A(SECURITY BLOCK)                            
GTLTXT   DS    F                   OPTIONAL TEXT STRING FOR GETTXT              
ACTDATE  DS    CL9                 ACTIVITY DATE                                
LANG     DS    XL1                 CONNECTED LANGUAGE CODE                      
SYSFLAG  DS    XL1                 AGENCY SYSTEM ELEM FOUND FLAG                
NUMCID   DS    XL1                 NUMBER OF COMPATIBLE GETIDS                  
PHASNO   DS    XL1                 PROGRAM PHASE NUMBER FOR OVERLAY             
PHASLV   DS    F                   PROGRAM PHASE LEVEL                          
SAVEREG  DS    F                  *                                             
PUSER    DS    H                   LOG-ON USER-ID# (FROM TUSER)                 
PUSERA   DS    CL10                LOG-ON USER-ID ALPHA (FROM TUSER)            
CHAVER   DS    CL2                 CHARECTER VERSION OF ROUTINE NO              
PSECINDS DS    XL1                 SAVED FASECRET SECURITY INDICATOR            
PDDS     DS    XL1                 0=USER TERM,1=DDS TERM                       
PPASSWD  DS    H                   CONNECTED PASSWORD NUMBER                    
PACTN    DS    XL1                 MAJOR ACTION                                 
PACTN2   DS    XL1                 MINOR ACTION                                 
PSYSA    DS    CL1                 SYSTEM ALPHA                                 
PSYSN    DS    XL1                 SYSTEM NUMERIC                               
PSYSI    DS    XL1                 SYSTEM INDICS                                
PDQUSYS  DS    XL1                 SPECIAL SYSTEM FOR $DQU PROFILES             
PSYSSEN  DS    XL1                 SYSTEM SE NUMBER                             
PDQUNAM  DS    CL6                 SIXPACK'D WRITER REPORT NAME                 
PPRG     DS    CL3                 PROGRAM                                      
PAGY     DS    XL1                 AGENCY BINARY (FROM CTSYSEL)                 
PAGYA    DS    CL2                 AGENCY ALPHA (FROM CTSYSEL)                  
PAGYC    DS    CL2                 AGENCY ALPHA (CONNECTED)                     
PPIDN    DS    XL2                 PRINCIPLE USER-ID # FOR AGENCY               
PUIDA    DS    CL10                USER-ID ALPHA AS INPUT                       
PUIDN    DS    XL2                 USER-ID NUMBER AS INPUT                      
PUIDAGY  DS    CL2                 AGENCY ID OF USER-ID AS INPUT                
PMED     DS    CL2                 MEDIA OR UNIT/LEDGER                         
PCLI     DS    CL3                 CLIENT                                       
PLEVEL   DS    XL1                 KEY LEVEL (1=A/2=AM/3=AMC)                   
PDEFAULT DS    XL1                 DEFAULT FIELD VALUE                          
PVALUE   DS    CL4                 PROFILE VALUE                                
PROGAUTH DS    H                   USER ID CON/PRO AUTH CODE                    
PFLDNUM  DS    XL1                 PROFILE FIELD NUMBER                         
PROFOTHR DS    CL16                EXTRACTED PROFILES                           
*                                                                               
PPROFS   DS    0CL64                                                            
PPROFD   DS    CL16                                                             
PPROFA   DS    CL16                AGENCY/USERID                                
PPROFM   DS    CL16                MEDIA                                        
PPROFC   DS    CL16                CLIENT                                       
*                                                                               
PVALS    DS    0CL12               *                                            
PVALA    DS    CL4                 AGENCY/USERID                                
PVALM    DS    CL4                 MEDIA                                        
PVALC    DS    CL4                 CLIENT                                       
*                                  PFKEY + CURSOR DISPLACEMENT                  
PFKEY    DS    XL1                                                              
CURDISP  DS    XL2                                                              
*                                  KEYS                                         
PKEY     DS    CL25                *                                            
PSKEY    DS    CL25                *                                            
*                                                                               
HKEY     DS    CL40                HELP KEY FOR DATAMANAGER                     
SVHKEY   DS    CL32                SAVED BUILD HELP KEY                         
HELPKEY  DS    CL32                HELP KEY                                     
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    XL1                                                              
*                                                                               
PLIST    DS    CL133               PARAMETER LIST                               
PREC     DS    1000C               *                                            
PREC2    DS    2000C               *                                            
*                                                                               
APROFTAB DS    F                   A(SAVED PROFILES IN TWA)                     
APTABLN  EQU   16*(L'PR2PRO2)      4 BYTES FOR EACH PROFILE                     
*                                                                               
DDDSLST  DS    0C                                                               
         DSDDL                                                                  
*                                                                               
SECBLK   DS    CL1024              SECRET BLOCK                                 
*                                                                               
PWRKX    EQU   *                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* PROGRAM ACCESS SECURITY EQUATES                                               
***********************************************************************         
PPROFQ   EQU   1     ACCESS        PROFILE RECORD                               
PPSYSQ   EQU   2     SYSTEM        PROFILE RECORD SYSTEM                        
*                                                                               
OUIDQ    EQU   1     OCONTROL      USER-ID                                      
*ODDSQ    EQU   2     OCONTROL      AUTHORIZED FOR DDS ONLY PROFILES            
*                                                                               
FMEDQ    EQU   1     FCONTROL      MEDIA FIELD RESTRICTION                      
FCLIQ    EQU   2     FCONTROL      CLIENT FIELD RESTRICTION                     
                                                                                
***********************************************************************         
* DSECT TO COVER SCREEN LINE                                                    
***********************************************************************         
PLINED   DSECT                                                                  
         DS    CL8                                                              
PLDESC   DS    CL30                                                             
PLHELP   DS    CL1                                                              
PLLIST   DS    CL20                                                             
         DS    CL2                                                              
PLDEFT   DS    CL4                                                              
         DS    CL3                                                              
PLVAL1   DS    CL4                                                              
         DS    CL3                                                              
PLVAL2   DS    CL4                                                              
         DS    CL1                                                              
PLDATAH  DS    CL8                                                              
PLDATA   DS    CL4                                                              
         ORG   PLDATA                                                           
PLVAL3   DS    CL4                                                              
                                                                                
*                                                                               
* DSECT TO COVER SAVAREA                                                        
*                                                                               
POVLAY   DSECT                                                                  
PRGNME   DS    CL3                                                              
PACTION  DS    XL1                                                              
PSTAGE   DS    XL1                                                              
PPROFIL  DS    CL4                                                              
OVLLN    EQU   *-PPROFIL                                                        
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
                                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
                                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
* CTLFMREQ                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTLFMREQ                                                       
         PRINT ON                                                               
                                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
                                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
                                                                                
* DDOFFICED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
                                                                                
* CTDDEQUSS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTDDEQUSS                                                      
         PRINT ON                                                               
                                                                                
* CTMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
         EJECT                                                                  
CTPROFFD DSECT                                                                  
         DS    CL16                                                             
INITFLAG DS    X                   FIRST TIME INITIALISATION FLAG               
*                                  1=NO USERID FIELD, 2=USERID MIELD            
CURSCRN  DS    XL1                 CURRENT SCREEN PHASE BEING USED              
LASTSCRN DS    XL1                 LASTA SCREEN USED                            
LKEY     DS    CL25                                                             
         DS    CL20                                                             
* CTPROFFDA                                                                     
       ++INCLUDE CTPROFFD         <<<<<<<<<<<<<<<<                              
* CTPROFED                                                                      
         ORG   PROHEREH                                                         
       ++INCLUDE CTPROFED                                                       
         EJECT                                                                  
* CTPROFDD                                                                      
         ORG   PROHEREH                                                         
       ++INCLUDE CTPROFDD                                                       
         EJECT                                                                  
         ORG   CTPROFFD+3000       SPACE FOR LARGEST SCREEN                     
SAVAREA  DS    XL69                                                             
* CTGENHV1                                                                      
       ++INCLUDE CTGENHV1                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027CTPRO00   02/17/17'                                      
         END                                                                    
