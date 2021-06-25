*          DATA SET ACSCR14    AT LEVEL 072 AS OF 10/10/19                      
*PHASE T60C14A                                                                  
*&&ONLIN SET   Y                                                                
*INCLUDE FAPQSEC                                                                
*INCLUDE LOADER                                                                 
*                                                                               
* YNGX 065 22FEB15 <PCA02294> download option to remove Request pages           
* YNGX 066 17MAR16<PCA2358> Merge US and UK versions                            
* YNGX 067 19APR16 <PC0A2378> download reports to EDIHUB                        
* YNGX 068 08AUG17 PCA02728 RELINK TO EXTEND MAX NO OF COLUMNS                  
* YNGX 069 21AUG18 SPEC-26412 US support for EDIHUB                             
* JSAY 072 09APR19 SPEC-33734 Relink to new options added in alternate          
*                             date table for timesheet approval date.           
* JSAY 072 09APR19 SPEC-33735 Relink to new options added in alternate          
*                             date table for timesheet submitted date.          
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE US VERSION LEVEL 62 AS OF 22/01/15         *         
*                                                                     *         
***********************************************************************         
         TITLE 'REQUEST REPORT'                                                 
*                                                                               
                                                                                
T60C14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C14,RA,R9,RR=RE                                              
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    R9,APBASE3                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         L     RF,=V(LOADER)                                                    
         AR    RF,RE                                                            
         ST    RF,ALOADER                                                       
         L     RF,=V(FAPQSEC)                                                   
         AR    RF,RE                                                            
         ST    RF,AFAPQSEC                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         EJECT ,                                                                
         USING RESRECD,R2                                                       
         CLI   APMODE,APMVALR                                                   
         BNE   SCR50                                                            
         CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APPFKEY,PFKEXIT                                                  
         BNE   SCR50                                                            
         TM    TWASWPST,TWASWAP                                                 
         BZ    EXIT                                                             
         MVI   APPFKEY,0                                                        
         MVI   APMODE,APMSWP                                                    
         MVC   APPARM(1),TWASWPRE                                               
         MVC   APPARM+1(1),TWASWPAC                                             
         B     EXIT                                                             
*                                                                               
SCR50    CLI   APMODE,APMVALK                                                   
         BNE   SCR52                                                            
         MVC   SAVPFKEY,APPFKEY                                                 
*                                                                               
SCR52    LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY              VALKEY                                       
         B     VALREC              VALREC                                       
         B     DISKEY                                                           
         B     DISREQ                                                           
         B     EXIT                DELETE COLUMN ELEMENT'S ONLY                 
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                COPY COLUMN ELEMENT'S                        
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
         CLC   FVMSGNO,=AL2(FVFOK)   SET CONDITION CODE                         
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  VALKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   DS    0H                                                               
         OI    APINDS2,APINOSOX                                                 
*&&UK                                                                           
         TM    GENIND,GENEURO      2ND CURRENCY CODE IN USE ?                   
         BO    VK020                                                            
         MVC   REQRCU,SPACES                                                    
         OI    REQRCUH+6,FVOXMT+X'20'  PROTECT FIELD                            
         MVC   REQRCF,SPACES           WIPE OUT DISCRIPTION                     
         OI    REQRCFH+6,FVOXMT                                                 
*&&                                                                             
VK020    MVI   NEWKEY,NO                                                        
         MVI   CHGKEY,NO                                                        
         MVI   APINDS,APIOKDIS                                                  
         MVI   PRFFLAG,0                                                        
         CLI   SAVPFKEY,PFKEXIT    GOING SOME PLACE ELSE?                       
         BNE   VK060                                                            
         CLI   SVRECORD,0          CAN WE GO TO SOMEPLACE?                      
         BE    *+8                 NO                                           
         CLI   SVRECORD,RECRPT     IS IT STILL REPORT/REQUEST?                  
         BNE   VK060               NO                                           
         MVC   FVMSGNO,=AL2(ACEPFK)                                             
         B     VK999                                                            
*                                                                               
VK060    TM    TWAMODE,TWAMLSM                                                  
         BNZ   VK066                                                            
         TM    REQFMTH+4,FVITHIS                                                
         BO    *+10                                                             
         MVC   REQFMT,SAVFORM      REPORT FORMAT                                
*                                                                               
VK066    OI    REQFMTH+6,FVOXMT                                                 
         GOTO1 AFVAL,REQFMTH                                                    
         CLI   APGFLAG,YES                                                      
         BE    VK080                                                            
*                                                                               
VK070    MVI   APGFLAG,NO                                                       
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         MVC   RESKFORM,FVIFLD     FORMAT                                       
         CLI   FVILEN,0                                                         
         BNE   *+10                                                             
         MVC   REQFMT,SAVFORM                                                   
         GOTO1 AFVAL,REQFMTH                                                    
         BNE   IVALFMT                                                          
         MVC   RESKFORM,FVIFLD                                                  
         B     VK120                                                            
*                                                                               
         USING APGRECD,R2                                                       
VK080    GOTO1 VCOLY,APPARM,('OVLYBBLK',0),0,0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   VK082                                                            
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     VK999                                                            
*                                                                               
VK082    MVC   AAPGIO,0(R1)                                                     
         MVC   APGKEY,SPACES                                                    
         MVI   APGKTYP,RESKTYPQ    X'2D'                                        
         MVI   APGKSUB,APGKSUBQ    X'07'                                        
         MVC   APGKCPY,CUABIN      COMPANY CODE                                 
         MVC   APGKRTY,=C'FI'                                                   
         CLI   APREPJCL,REPJCLFI   APG FI FORMAT?                               
         BE    VK085                                                            
         MVC   APGKRTY,=C'M2'                                                   
         CLI   APREPJCL,REPJCLM2   APG M2 FORMAT?                               
         BE    VK085                                                            
         DC    H'00'                                                            
*                                                                               
VK085    MVC   APGKFMT,FVIFLD      FORMAT                                       
         CLI   FVILEN,1            BLANK IS VALID HERE                          
         BH    VK070               TRY SCRIBE FORMAT TYPE THEN                  
         OI    REQFMTH+6,FVOXMT                                                 
*                                                                               
VK120    TM    TWAMODE,TWAMLSM     LIST/SELECT MODE?                            
         BO    VK150               YES, SO NO NEED                              
         CLC   SAVFORM,FVIFLD                                                   
         BE    VK150                                                            
         XC    SAVRECK,SAVRECK                                                  
*                                                                               
VK150    CLI   APGFLAG,YES                                                      
         BNE   VK160                                                            
         MVC   SAVFORM,SPACES                                                   
         MVC   SAVFORM(1),APGKFMT                                               
         MVC   APRECKEY(L'APGKEY),APGKEY                                        
         B     VK165                                                            
*                                                                               
         USING RESRECD,R2                                                       
VK160    CLC   SAVFORM,FVIFLD                                                   
         BE    *+8                                                              
         MVI   CHGKEY,YES                                                       
         MVC   SAVFORM,RESKFORM                                                 
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
*                                                                               
VK165    GOTO1 AIO,IORD+IOACCFIL+IO1                                            
         BE    VK200                                                            
         TM    IOERR,IOEDEL        IS IT DELETED?                               
         BZ    IVALFMT             INVALID FORMAT                               
         MVI   NEWKEY,YES                                                       
         MVI   APINDS,APIOKDIS                                                  
         B     VK999                                                            
*                                                                               
VK200    L     R1,AIOAREA1                                                      
         GOTO1 GETTYPE                                                          
         BNE   VK999                                                            
*                                                                               
VK220    MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         OI    SCRTYPH+6,FVOXMT                                                 
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
*                                                                               
         USING APGRECD,R2                                                       
         CLI   APGFLAG,YES                                                      
         BNE   VK300                                                            
         L     RF,=A(GETAPG)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     VK320                                                            
*                                                                               
VK300    L     RF,=A(GETRCAP)      CHECK FOR RCAP                               
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
VK320    L     RF,=A(SCRTABLE)                                                  
         A     RF,APRELO                                                        
*                                                                               
VK460    CLI   0(RF),X'FF'         EOT?                                         
         BNE   VK480                                                            
         MVC   FVMSGNO,=AL2(2090)                                               
         B     VK999                                                            
*                                                                               
VK480    CLC   APREPJCL,0(RF)                                                   
         BNE   VK490                                                            
         MVI   APBYTE,NO                                                        
         TM    1(RF),REPIAPG                                                    
         BZ    *+8                                                              
         MVI   APBYTE,YES                                                       
         CLC   APGFLAG,APBYTE                                                   
         BE    VK495                                                            
*                                                                               
VK490    LA    RF,4(,RF)                                                        
         B     VK460                                                            
*                                                                               
VK495    SR    R2,R2                                                            
         ICM   R2,3,2(RF)                                                       
         A     R2,=A(REPFLDS)                                                   
         A     R2,APRELO                                                        
         ST    R2,ASCRTAB                                                       
*                                                                               
         CLC   APRECKEY(L'RESKEY),SAVRECK    SAME FORMAT ?                      
         BNE   VK800                         NO,  REBUILD   SCREEN              
         TM    INFLAG1,INFDISR               FORCE     DISPLAY   RCD ?          
         BZ    VK900                         NO,  DO   NOT  REBUILD             
*                                                                               
VK800    L     RF,=A(SETSCRN)                REBUILD   SCREEN                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
VK900    MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VK999    B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  VALREC                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R1                                                        
         USING RESRECD,R2                                                       
         USING ACQD,R3                                                          
VALREC   L     R2,AIOAREA1                                                      
         BRAS  RE,INITREQ                                                       
         GOTOR VALFRMT,APPARM,('IO1',RESKFORM)                                  
         BNE   IVALEXIT            Yes, format has errors                       
*                                                                               
         MVI   APELCODE,RPFELQ     X'C4' PROFILE                                
         GOTO1 GETEL,(R2)                                                       
         BNE   VR005                                                            
*                                                                               
         CLI   APREPJCL,REPJCLB    BANK (CASH) ?                                
         BNE   VR001A                                                           
         MVC   SVRECON,APYES       INCLUDE RECONCILED ITEMS                     
         TM    RPFOPT5,RPFXBRI     EXCLUDE RECONCILED ITEMS ?                   
         BZ    *+10                                                             
         MVC   SVRECON,APNO        EXCLUE  RECONCILED ITEMS                     
         TM    RPFOPT5,RPFOBRI     ONLY    RECONCILED ITEMS ?                   
         BZ    *+10                                                             
         MVC   SVRECON,APONLY      ONLY    RECONCILED ITEMS                     
*                                                                               
VR001A   TM    RPFOPT1,RPFIDRFT    INCLUDE DRAFT ITEMS?                         
         BZ    *+8                                                              
         MVI   SVDFTOPT,YES                                                     
         TM    RPFOPT1,RPFODRFT    ONLY THEM                                    
         BZ    *+8                                                              
         MVI   SVDFTOPT,ONLY                                                    
*                                                                               
         CLI   RPFLN,RPFLNQ        OLD LENGTH ?                                 
         BNH   VR001C              SKIP OLDER ELEMENTS                          
*&&UK                                                                           
         TM    RPFXMIT,RPFXBDE                                                  
         BZ    *+12                                                             
         MVI   ACQOPT1,C'B'                                                     
         B     VR001D                                                           
*                                                                               
         TM    RPFXMIT,RPFXUSS                                                  
         BZ    *+12                                                             
         MVI   ACQOPT1,C'U'                                                     
         B     VR001D                                                           
*&&                                                                             
         TM    RPFXMIT2,RPFXEDI                                                 
         BZ    *+12                                                             
         MVI   ACQOPT1,C'E'                                                     
         B     VR001D                                                           
*                                                                               
         TM    RPFXMIT,RPFXFTP+RPFXFIL                                          
         BZ    VR001B                                                           
         MVI   ACQOPT1,C'F'                                                     
         B     VR001D                                                           
*                                                                               
VR001B   TM    RPFXMIT,RPFXDSN                                                  
         BZ    VR001C                                                           
         MVI   ACQOPT1,C'N'                                                     
         B     VR001D                                                           
*                                                                               
VR001C   TM    RPFDNOPT,RPFDDOWN   DOWNLOAD REPORT ONLY                         
         BZ    VR001F                                                           
                                                                                
VR001D   OI    PRFFLAG,PRFDOWN     FORCE DOWNLOAD OPTION                        
         B     VR002                                                            
                                                                                
VR001F   CLC   REQOTYP,SPACES                                                   
         BNH   VR002               NO INPUT SO DON'T CARE                       
         CLI   CHGKEY,YES                                                       
         BNE   VR002               LEAVE AS IS                                  
         MVC   REQOTYP,SPACES                                                   
         OI    REQOTYPH+6,FVOXMT   TRANSMIT                                     
         B     VR002                                                            
*                                                                               
VR002    TM    RPFROPT,RPFXRVRS    EXCLUDE REVERSALS?                           
         BZ    *+8                                                              
         MVI   SVREVOPT,NO                                                      
         TM    RPFROPT,RPFORVRS    REVERSALS ONLY?                              
         BZ    *+8                                                              
         MVI   SVREVOPT,ONLY                                                    
*&&US                                                                           
         CLI   APREPJCL,REPJCLV    PRODUCTION?                                  
         BNE   VR004                                                            
         TM    RPFOPT4,RPFIEXPS                                                 
         BZ    *+8                                                              
         MVI   ACQXJOB,YES         INCLUDE EXPENSE JOBS                         
         TM    RPFOPT4,RPFOEXPS                                                 
         BZ    *+8                                                              
         MVI   ACQXJOB,ONLY        ONLY EXPENSE JOBS                            
*&&                                                                             
         TM    RPFOPT7,RPFIDJBS                                                 
         BZ    *+8                                                              
         MVI   ACQDJOB,YES         INCLUDE DRAFT JOBS                           
         TM    RPFOPT7,RPFODJBS                                                 
         BZ    *+8                                                              
         MVI   ACQDJOB,ONLY        ONLY DRAFT JOBS                              
*                                                                               
*&&UK                                                                           
         MVI   ACQALOCK,C'N'       INCLUDE UNLOCKED ONLY                        
         TM    RPFROPT,RPFXLOCK                                                 
         BO    *+20                                                             
         MVI   ACQALOCK,C'O'       INCLUDE LOCKED ONLY                          
         TM    RPFROPT,RPFOLOCK                                                 
         BO    *+8                                                              
         MVI   ACQALOCK,C'Y'       INCLUDE ALL ACCOUNTS                         
*&&                                                                             
VR004    CLI   RPFBLTRN,YES                                                     
         BE    VR005                                                            
         CLI   RPFBLTRN,C' '                                                    
         BNH   VR005                                                            
         MVC   ACQOPT4,RPFBLTRN    MOVE IN UTILIZE OPTION                       
*                                                                               
VR005    CLI   REQFMT,C'@'                                                      
         BNE   *+8                                                              
         OI    PRFFLAG,PRFSQL                                                   
                                                                                
         L     R2,AIOAREA1                                                      
         XC    ALTSDTE(L'ALTSDTE*2),ALTSDTE                                     
         MVC   ACQREVOP,SVREVOPT   REVERSAL OPTION                              
         MVC   ACQDRFOP,SVDFTOPT   DRAFT OPTION                                 
*                                                                               
         GOTO1 AVALWHEN,REQRUNH    PRINT                                        
         BNE   VR999                                                            
         CLI   SOONVLUE,0          SOONABLE REPORT?                             
         BNH   IVALREQ             INVALID REQUEST                              
         BRAS  RE,GETUSER                                                       
         BNE   IVALEXIT            ERROR                                        
                                                                                
         GOTO1 AVALDEST,REQDESH    DESTINATION                                  
         BE    VR010                                                            
         CLC   AC@FILE,FVIFLD      CHECK RFP STATUS                             
         BNE   VR999                                                            
         GOTO1 RFPINT              INITIALIZE RFP                               
         OI    RLPFLAG,RLPFON      SET FLAG ON                                  
         L     R8,APID             Re-load pid area                             
         XC    1(L'TWAPERSN,R8),1(R8)                                           
         MVC   1(5,R8),=C'RLP#='                                                
         GOTOR VHEXOUT,APPARM,CUPASS,6(R8),2                                    
*        MVC   6(2,R8),CUPASS      Store password                               
                                                                                
         GOTO1 RFPGRP,REQOTYPH                                                  
         BNE   VR999               GROUP NO GOOD                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VR020                                                            
*                                                                               
VR010    DS    0H                                                               
         CLC   REQOTYP,SPACES                                                   
         BH    VR010C                                                           
         TM    PRFFLAG,PRFDOWN     Force download ?                             
         BZ    VR010C              No                                           
         MVC   REQOTYP,SPACES                                                   
         MVC   REQOTYP(6),AC@DOWN                                               
         OI    REQOTYPH+6,FVOXMT   TRANSMIT                                     
*                                                                               
VR010C   GOTO1 AFVAL,REQOTYPH      ANY  OUTPUT TYPE ?                           
         BNE   VR014               NO,  SKIP                                    
         TM    PRFFLAG,PRFSQL      SQL  ?                                       
         BO    VR014               YES, SKIP                                    
*                                                                               
         USING OUTTYPD,RE                                                       
         LA    R0,OUTTYPQ          NUMBER TO CHECK FOR                          
         L     RE,=A(OUTTYPES)     OUTPUT TYPES CONVERSION TABLE                
         A     RE,APRELO                                                        
*                                                                               
VR011    CLC   OUTTYOLD,FVIFLD     OLD  TYPE ?                                  
         BE    VR012               YES, CONVERT                                 
         CLC   OUTTYNEW,FVIFLD     NEW  TYPE                                    
         BE    VR014               YES, USE  TYPE                               
         LA    RE,OUTTYLNQ(,RE)    GET  NEXT TABLE ENTRY                        
         BCT   R0,VR011                                                         
         MVC   FVMSGNO,=AL2(FVFEOUT) SET INVALID OUTPUT TYPE                    
         B     VR999                                                            
*                                                                               
VR012    MVC   REQOTYP,SPACES                 CLEAR TYPE                        
         MVC   REQOTYP(L'OUTTYNEW),OUTTYNEW   USE   NEW TYPE                    
         OI    REQOTYPH+6,FVOXMT              TRANSMIT                          
         DROP  RE                                                               
*                                                                               
VR014    GOTO1 AVALOTYP,REQOTYPH               VALIDATE TYPE                    
         BNE   VR999                                                            
*                                                                               
VR020    MVC   ACQCPY,CUABIN       REQUEST CARD COMPANY                         
*&&UK                                                                           
         TM    GENIND,GENEURO      2ND CURRENCY PRESENT                         
         BZ    VR025               NO                                           
         CLI   REQRCU,C' '                                                      
         BH    *+8                                                              
         MVI   REQRCU,ACQC1ST      DEFAULT TO C'1'                              
         CLI   REQRCU,ACQC1ST                                                   
         BE    *+8                                                              
         CLI   REQRCU,ACQC2ND                                                   
         BNE   IVALINPT                                                         
         MVC   ACQCURR,REQRCU                                                   
*&&                                                                             
VR025    L     R2,AIOAREA1                                                      
         CLI   APGFLAG,YES                                                      
         BE    VR030                                                            
         MVC   ACQAPPL(L'RESKFORM),RESKFORM                                     
         B     VR050                                                            
*                                                                               
         USING APGRECD,R2                                                       
VR030    MVC   ACQSRTAR(2),=C'P='                                               
         MVC   ACQSRTAR+2(2),APREPCDE                                           
         MVC   ACQSRTAR+4(1),APGKFMT                                            
         MVC   ACQOPT2,APGKFMT                                                  
         XC    ACQAPPL,ACQAPPL     CLEAR BUDGET LOCATION FOR APG                
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DYNAMIC VALIDATION                                                 *         
***********************************************************************         
         SPACE 1                                                                
VR050    SR    R6,R6                                                            
         ICM   R6,1,RECAP#                                                      
         BZ    VR058                                                            
*                                                                               
         USING RECAPD,R2           Recap table                                  
         LA    R2,RCAPFMTS         A(Saved recap codes)                         
VR051    GOTOR VALFRMT,APPARM,('IO2',RECAPFMT)                                  
         BE    VR052                                                            
         MVC   RECAPERR,FVMSGNO                                                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VR052    AHI   R2,RECAPLNQ         Bump to next Recap                           
         BCT   R6,VR051                                                         
*                                                                               
VR058    LA    R6,REQFMTNH         LAST FIELD ON PANGEN SCREEN                  
         BRAS  RE,NEXTUNPT                                                      
         L     R2,ASCRTAB                                                       
*                                                                               
         USING REPFD,R2            REPORT FIELD DSECT                           
         USING RTND,R4             ROUTINES TABLE DSECT                         
VR060    CLI   0(R2),X'FF'         EOT?                                         
         BE    VR800               NO MORE SCREEN FIELDS, BUILD REQUEST         
         TM    REPFIND1,REPFSTO    STEREO FIELD ONLY                            
         BZ    *+12                NO, SO DON'T CARE                            
         TM    GENIND,GENSTRO      YES, SO STEREO MODE                          
         BZ    VR085               NO                                           
         L     R4,=A(ROUTINES)                                                  
         A     R4,APRELO                                                        
*                                                                               
VR062    CLI   0(R4),X'FF'         EOT?                                         
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   REPFFLDN,RTNFLDN    MATCH ON FIELD NUMBER?                       
         BE    VR065                                                            
         LA    R4,RTNLNQ(,R4)      BUMP TO CHECK NEXT ONE                       
         B     VR062                                                            
*                                                                               
VR065    DS    0H                                                               
         ST    R4,ARTND            SAVE ADDRESS OF ROUTINE TABLE ENTRY          
         L     RF,=A(FLDCNTL)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   VR085                                                            
         LA    RF,VALROUT          GET VALIDATE ROUTINE #                       
         ICM   RF,8,RTNVDISP                                                    
         BASR  RE,RF                                                            
         BNE   EXIT                                                             
         BRAS  RE,NEXTUNPT                                                      
*                                                                               
VR085    LA    R2,REPFLNQ(,R2)     BUMP TO NEXT FIELD ON SCREEN                 
         B     VR060                                                            
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  BUILD REQUEST                                                      *         
***********************************************************************         
         SPACE 1                                                                
VR800    DS    0H                  BUILD REQUEST                                
         OC    OCURDTE,OCURDTE     ANY OVER-RIDE DATE ?                         
         BZ    VR801                                                            
         GOTO1 GETFFLD,APPARM,REQCARDS                                          
         L     RF,APPARM                                                        
         MVI   0(RF),ACQDATE                                                    
         MVI   1(RF),ACQDTREL                                                   
         LA    RF,ACQDTEND-ACQTYP1(,RF)    POINT TO DATES AREA                  
         MVC   0(6,RF),OCURDTE                                                  
*                                                                               
VR801    DS    0H                                                               
*&&US                                                                           
         OC    MMOSRNG,MMOSRNG     Any Media MOS range?                         
         BZ    VR802                                                            
         GOTO1 GETFFLD,APPARM,REQCARDS                                          
         L     RF,APPARM                                                        
         MVI   0(RF),ACQDATE                                                    
         MVI   1(RF),ACQDTMOS                                                   
         LA    RF,ACQDTSTR-ACQTYP1(,RF)    POINT TO DATES AREA                  
         MVC   0(L'ACQDTSTR+L'ACQDTEND,RF),MMOSRNG                              
*                                                                               
VR802    DS    0H                                                               
*        CLI   APREPJCL,REPJCL1    PERSON WRITER?                               
*        BNE   VR804               NO SO DON'T CARE                             
         TM    INWHEN,MIXIOKS      ARE WE SOONING?                              
         BZ    VR804               NO                                           
         CLI   SOONVLUE,1          OK TO SOON WITHOUT ACCOUNT DETIAL            
         BL    VR803                  IF < 1 THEN NO SOONING                    
         BH    VR804                  IF > 1 NO RESTRICTION ON SOONING          
         TM    PRFFLAG,PRFACC         IF = 1 THE OK TO SOON, NEED ACCT          
         BO    VR804               PERSON OR ACCOUNT FIELD FOUND                
         CLI   ACQOPT8,ONLY        FORMAT ONLY REPORT ?                         
         BE    VR804               YES, OKAY TO SOON                            
*                                                                               
VR803    L     R1,AACTHDRH         ACCOUNT FIELD HEADER                         
         GOTO1 AFVAL                                                            
         B     IVALSOON            INVALID SOONING REQUEST                      
*                                                                               
VR804    GOTO1 AFVAL,REQOPT2H                                                   
         MVC   REQNUMB,APREPLD#    $REQ ID # FOR LANDSCAPE                      
         CLI   FVIFLD,C'L'                                                      
         BE    VR805                                                            
         MVC   REQNUMB,APREPPT#    $REQ ID # FOR PORTRAIT                       
         CLI   FVIFLD,C'P'                                                      
         BNE   IVALINPT                                                         
*                                                                               
VR805    MVC   INREPORT+5(1),FVIFLD                                             
         MVC   INREPORT+3(1),FVIFLD                                             
*&&                                                                             
*&&UK                                                                           
         MVC   REQNUMB,APREPLD#    $REQ ID FOR LANDSCAPE                        
         MVI   INREPORT+5,C'L'                                                  
         MVI   INREPORT+3,C'L'                                                  
*&&                                                                             
         MVC   INREPORT+2(1),APREPJCL                                           
         MVC   INREPORT+4(1),APREPJCL                                           
         MVC   ACQLANG,CULANG                                                   
         LA    RE,MAXWDLS          Landscape                                    
         CLI   INREPORT+4,C'L'                                                  
         BE    *+8                                                              
         LA    RE,MAXWDPT          Portrait                                     
         CLC   RPTWIDTH,0(RE)      Will it fit ?                                
         BH    IVALPTCH                                                         
*                                                                               
VR805A   CLC   ACQSTART,SPACES     ANY   START DATE ?                           
         BE    VR806               NO,   SKIP                                   
         CLC   ACQEND,SPACES       ANY   END   DATE ?                           
         BE    VR806               NO,   SKIP                                   
         NC    SVSTARTH,SVSTARTH   BOTH  DATES ARE  CONSTANTS  ?                
         BZ    VR807               NO,   SKIP                                   
         CLC   ACQSTART,ACQEND     START DATE  >    END   DATE ?                
         BNH   VR807               NO,   SKIP                                   
         MVC   APCURSOR,SVSTARTH   RESET CURSOR                                 
         B     IVALEDBS            END   DATE  BEFORE     START DATE            
*                                                                               
VR806    CLC   ACQMOSST,SPACES     MOA?                                         
         BNH   *+14                                                             
         CLC   ACQMOSND,SPACES                                                  
         BH    VR807                                                            
         CLI   RUNJOB,RUNSHORT                                                  
         BE    VR807                                                            
         MVI   RUNJOB,RUNLONG                                                   
VR807    CLC   ACQACTST,SPACES     Any activity start date ?                    
         BE    VR808               No, skip                                     
         CLC   ACQACTND,SPACES     Any activity end   date ?                    
         BE    VR808               No, skip                                     
         NC    SVACTSTH,SVACTSTH   Both date are constants ?                    
         BZ    VR808               No, skip                                     
         CLC   ACQACTST,ACQACTND   Start date > End date ?                      
         BNH   VR808               No, skip                                     
         MVC   APCURSOR,SVACTSTH   Reset cursor                                 
         B     IVALEDBS            End date before start date                   
*                                                                               
VR808    CLI   RECAP#,0            Any recap formats ?                          
         BE    VR809               No, skip                                     
         GOTO1 GETFFLD,APPARM,(R3) Yes, get 1st un-used in req. card            
*                                                                               
         USING ACQTYP1,R8                                                       
         L     R8,APPARM             Available location in REQ card             
         MVI   ACQTYP1,ACQRCAP                 Mark as recap                    
         MVC   ACQFLT1+1(L'RQRCAPSW),RQRCAPSW  Set flags for recaps             
         DROP  R8                                                               
*                                                                               
VR809    LA    RF,4                COUNT CARD                                   
         LA    RE,ACQCARD4                                                      
VR810    CLC   0(L'ACQCARD1,RE),SPACES                                          
         BNE   VR820                                                            
         SHI   RE,L'ACQCARD1                                                    
         BCT   RF,VR810                                                         
         B     VR875               EXIT NO REQUEST DATA                         
*                                                                               
VR820    MVI   REQFLAG,X'01'       SET AS LINKED REQUEST                        
         SR    R1,R1                                                            
         TM    INWHEN,MIXIOKS      Are we sooning?                              
         BZ    VR822                                                            
         CLI   TESTCHR,C' '        Special TEST=02 card ?                       
         BNH   VR822               No                                           
         LHI   R1,1                Add one to count                             
                                                                                
VR822    AR    RF,R1               Adjust for TEST=02 card                      
         SHI   RF,1                                                             
         BZ    VR830                                                            
         SLL   RF,4                # OF REQUEST CARDS-1 IN HO NIBBLE            
         LA    RF,3(,RF)           TURN ON X'01' AND X'02'                      
         STC   RF,REQFLAG                                                       
         SRL   RF,4                SHIFT IT BACK                                
         MVI   ACQCONT1,C'C'       CONTINUATION MARKER                          
         SR    RF,R1               Adjust for TEST=02 card                      
*                                                                               
VR825    SHI   RF,1                MARK CARDS 2-7 ONLY                          
         BZ    VR830               IF ZERO NO MORE                              
         SHI   RE,L'ACQCARD1                                                    
         MVI   ACQCONT2-ACQCARD2(RE),C'C'                                       
         B     VR825                                                            
*                                                                               
VR830    MVC   REQDEST,INDEST                                                   
         MVC   REQRUN,SPACES                                                    
         MVC   REQORIG,CUUSER                                                   
         OI    REQRUNH+6,FVOXMT    TRANSMIT                                     
         MVC   REQOUT,INOTYP       BUILD REQUEST HEADER                         
         GOTO1 AFVAL,REQOTYPH                                                   
         TM    PRFFLAG,PRFSQL      RDR (SQL)                                    
         BZ    VR832               No                                           
         MVC   INOTYP,REQFMT                                                    
         B     VR835                                                            
*                                                                               
VR832    TM    PRFFLAG,PRFDOWN     FORCE DOWN-LOAD ?                            
         BO    VR833               YES,  SKIP                                   
         CLC   AC@DOWN,FVIFLD      USER  INPUTTED  DOWN ?                       
         BNE   VR835               NO,   NOT  DOWN-LOAD                         
         OI    PRFFLAG,PRFDOWN     FORCE DOWNLOAD OPTION                        
*                                                                               
VR833    CLI   APGFLAG,YES         APG ?                                        
         BNE   VR834               NO,   SKIP                                   
         MVI   ACQOPT7,YES                                                      
         MVC   REQOUT,AC@DOWN      DOWN-LOAD  THE  OUTPUT                       
         B     VR840               CONTINUE                                     
*                                                                               
*        TM    RLPFLAG,RLPFON      IS    IT   RFP ?                             
VR834    MVC   REQOUT,AC@DOWN      DOWN-LOAD  THE  OUTPUT                       
         B     VR838               CONTINUE                                     
*                                                                               
VR835    CLI   INOTYP,C'@'         RDR, SQL DOWNLOAD                            
         BNE   VR840                                                            
*                                                                               
VR838    CLI   ACQOPT1,C'N'        DSN type                                     
         BE    VR840               Yes, so leave as is                          
*&&UK                                                                           
         CLI   ACQOPT1,C'B'        BDE type                                     
         BE    VR840               ditto                                        
*                                                                               
         CLI   ACQOPT1,C'U'        USS type                                     
         BE    VR840               ditto                                        
*&&                                                                             
         CLI   ACQOPT1,C'E'        EDIHUB type                                  
         BE    VR840               ditto                                        
*                                                                               
         CLI   ACQOPT1,C'F'        FTP type                                     
         BNE   VR839               No                                           
         TM    INOPT1,INOSEAR+INOYMD+INOAIU                                     
         BZ    VR840                                                            
         B     IVALOPTN                                                         
*                                                                               
VR839    MVI   ACQOPT1,C'Y'                                                     
*&&UK                                                                           
         TM    INOPT1,INOSEAR+INOYMD+INOAIU                                     
         BNZ   VR839A                                                           
         L     R2,AIOAREA1                                                      
         MVI   APELCODE,RPFELQ     X'C4' PROFILE                                
         GOTO1 GETEL,(R2)                                                       
         BNE   VR839A                                                           
         CLI   RPFLN,RPFLN2Q       TEST OLD ELEMENT                             
         BL    VR839A              YES                                          
         TM    RPFPOPT3,RPFNORQP   SUPPRESS REQUEST PAGE                        
         BZ    *+8                                                              
         MVI   ACQOPT1,C'S'        SPECIAL DL TO SUPPRESS REQ PAGE              
*&&                                                                             
VR839A   TM    INOPT1,INOSEAR      SPECIAL DOWNLOAD?                            
         BZ    *+8                                                              
         MVI   ACQOPT1,C'T'                                                     
         TM    INOPT1,INOYMD       SPECIAL DOWNLOAD?                            
         BZ    *+8                                                              
         MVI   ACQOPT1,C'D'                                                     
         TM    INOPT1,INOAIU       SPECIAL AIU DOWNLOAD?                        
         BZ    *+8                                                              
         MVI   ACQOPT1,C'A'                                                     
*                                                                               
VR840    MVC   ACQPROG,INJCLID                                                  
         CLI   APGFLAG,YES                                                      
         BNE   VR841                                                            
         MVC   ACQPROG,APREPCDE                                                 
         MVC   INJCLID,APREPCDE                                                 
*                                                                               
         USING RFLELD,R2                                                        
VR841    L     R2,AIOAREA1                                                      
         AH    R2,DATADISP         GET    TO   1ST  ELEMENT                     
         SR    RF,RF               CLEAR  REGISTER                              
*                                                                               
VR842    CLI   0(R2),0             END    OF   RECORD ?                         
         BE    VR845               YES,   NO   MORE ELEMENTS TO DELETE          
         CLI   0(R2),RFLELQ        X'C5'  ELEMENT ?                             
         BNE   *+8                 NO,    LOOP TO   NEXT ELEMENT                
         CLI   RFLTYPE,RFLLDG      X'01'  LEDGER ?                              
         BE    VR843               NO,    LOOP TO   NEXT ELEMENT                
         IC    RF,1(,R2)           GET    LENGTH                                
         AR    R2,RF               BUMP   TO   NEXT ELEMENT                     
         B     VR842               LOOP                                         
*                                  *** CHECK LEDGER SECURITY ***                
VR843    CLI   MULTIUL,YES         MULTI-LEDGER REQUEST MADE?                   
         BNE   VR844B              NO, SO ALREADY CHECKED                       
         SR    R0,R0                                                            
         IC    R0,1(,R2)                                                        
         AR    R0,R2               POINT TO END      OF DATA                    
         LA    R1,RFLDATA          POINT TO BEGINING OF DATA                    
         L     RE,AFLDUNLG                                                      
         A     RE,ATWA                                                          
         ST    RE,FVADDR                                                        
*                                                                               
VR844A   MVC   FVXTRA(2),0(R1)                                                  
         GOTO1 VALLEDG                                                          
         BNE   VR999               ERROR SET ALREADY                            
         CLC   CUAUTH+1(1),ACLEDSEC+1                                           
         BL    IVALSEC             CHECK LEDGER SECURITY                        
         LA    R1,3(,R1)           BUMP TO NEXT LEDGER                          
         CR    R1,R0               AT END OF STRING OF LEDGERS?                 
         BL    VR844A              NO, NOT YET                                  
*                                                                               
         MVC   FVXTRA(2),SPACES                                                 
         XC    FVADDR,FVADDR                                                    
*                                                                               
VR844B   CLI   APGFLAG,YES                                                      
         BNE   VR845                                                            
         MVI   0(R2),X'FF'         MARK   FOR  DELETION                         
         MVI   APELCODE,X'FF'      DELETE X'FF'     ELEMENTS                    
         L     R1,AIOAREA1                                                      
         GOTO1 DELEL                                                            
         DROP  R2                                                               
*                                                                               
VR845    TM    RLPFLAG,RLPFON      RFP    IS   ON ?                             
         BO    VR865               YES,   ADD  REQUEST   TO   RFP               
         TM    INWHEN,MIXIOKO      OVER   NIGHT ?                               
         BZ    VR850                                                            
         GOTO1 VDMGR,APPARM,(X'20',DMADD),REQUEST,ADDR,REQREC,,L'REQREC         
         CLI   APPARM+8,0                                                       
         BNE   IVALREQ                                                          
         ICM   RF,15,ADDR                                                       
         GOTO1 DTESTAMP,APPARM,AIOAREA1,(RF)                                    
*        TM    GENIND,GENREADO                                                  
*        BO    VR846                                                            
         GOTO1 AIO,IOSOXWRT+IOACCFIL+IO1                                        
*                                                                               
VR846    MVC   FVADDR,AACTHDR      SET CURSOR TO ACTION FIELD                   
         MVC   FVMSGNO,=AL2(INFREP1)                                            
         MVI   FVOMTYP,GTMINF      REPORT WILL BE PROCESSED OVER NIGHT          
         B     VR870                                                            
*                                                                               
         USING SPOOK,R2                                                         
VR850    LA    R2,APELEM           BUILD SPOOK                                  
         XC    SPOOK(SPOOKXL),SPOOK                                             
*        TM    CUSTAT,CUSDDS       Is it a DDS terminal ?                       
*        BZ    VR852                                                            
         CLI   TESTCHR,C' '              Test new version                       
         BE    VR852                                                            
         MVC   REQCARD5,REQCARD4                                                
         MVC   REQCARD4,REQCARD3                                                
         MVC   REQCARD3,REQCARD2                                                
         MVC   REQCARD2,REQCARD1                                                
         MVC   REQCARD1,SPACES                                                  
         MVC   REQCARD1(8),=C'=TEST=02'                                         
         MVC   REQCARD1+8(1),TESTCHR                                            
                                                                                
VR852    MVC   SPOOKUID,CUUSER                                                  
         MVC   SPOOKDES,INDEST                                                  
         MVC   SPOOKTID,CUTRMN                                                  
         CLI   RUNJOB,RUNLONG                                                   
         BNE   VR852C              NO                                           
*&&US                                                                           
         L     RE,=A(AGYTAB)       YES BUT IF AGENCY IS LISTED IN THIS          
         A     RE,APRELO           TABLE THEN DON'T PUT AS A LONG               
VR852A   CLI   0(RE),X'FF'         RUNNING SOON                                 
         BE    VR852B                                                           
         CLC   0(2,RE),CUAALF                                                   
         BE    VR852C                                                           
         AHI   RE,2                                                             
         B     VR852A                                                           
*&&                                                                             
VR852B   MVC   SPOOKSML,RUNJOB                                                  
VR852C   MVC   SPOOKAGY,CUAALF                                                  
         MVC   SPOOKAGX,CUABIN                                                  
         MVC   SPOOKDID,INUSER                                                  
         MVC   SPOOKSYS,=C'AC'                                                  
         MVC   SPOOKEOD,INPRGID                                                 
         MVC   SPOOKJCL,INJCLID                                                 
         MVC   SPOOKPR1,INPRTY1                                                 
         MVC   SPOOKPR2,INPRTY2                                                 
         MVC   SPOOKWEN,INWHEN                                                  
         TM    PRFFLAG,PRFSQL      SQL  ?                                       
         BO    VR854                                                            
         CLI   INOTYP,C'@'                                                      
         BNE   VR855                                                            
                                                                                
VR854    MVC   SPOOKSQL,INOTYP+1                                                
         OI    SPOOKTY,X'08'                                                    
*                                                                               
VR855    TM    PRFFLAG,RPFDOWN                                                  
         BO    *+12                                                             
         CLI   ACQOPT1,C' '                                                     
         BNH   VR860                                                            
         TM    SPOOKTY,X'08'                                                    
         BO    *+8                                                              
         OI    SPOOKTY,X'10'                                                    
*                                                                               
VR860    MVC   SPOOKXT(3),=C'XT='                                               
         GOTO1 VREQTWA,APPARM,(5,(R5)),REQHEAD,VDMGR,ACOM,(R2)                  
         MVC   FVMSGNO,=AL2(FVFTFUL)                                            
         CLI   8(R1),X'FE'         TEST TERMINAL QUEUE FULL?                    
         BE    VR870                                                            
         MVC   FVMSGNO,=AL2(FVFQFUL)                                            
         BH    VR870                                                            
         MVC   FVMSGNO,=AL2(FVFEJCL)                                            
         L     R4,8(R1)                                                         
         OC    0(7,R4),0(R4)                                                    
         BZ    VR870                                                            
         SR    R0,R0                                                            
         ICM   R0,3,6(R4)          GET  SOON NUMBER                             
         CVD   R0,WORK             SAVE SOON NUMBER                             
         GOTO1 DTESTAMP,APPARM,AIOAREA1,0                                       
*&&UK*&& TM    GENIND,GENREADO     SYSTEM IN READ ONLY MODE                     
*&&UK*&& BO    VR864                                                            
         GOTO1 AIO,IOSOXWRT+IOACCFIL+IO1                                        
*                                                                               
VR864    MVC   FVADDR,AACTHDR      SET CURSOR TO ACTION FIELD                   
         MVC   FVMSGNO,=AL2(INFREP2)                                            
         MVI   FVOMTYP,GTMINF      REPORT WILL BE PROCESSED SOON                
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'INUSER),INUSER                                          
         MVC   FVXTRA+3(1),SCCOMMA                                              
         MVC   FVXTRA+4(6),=XL6'402020202020'                                   
         LA    R1,FVXTRA+4                                                      
         EDMK  FVXTRA+4(6),WORK+5  EDIT SOON NUMBER                             
         MVC   FVXTRA+4(6),0(R1)   SQUISH                                       
         B     VR870                                                            
*                                                                               
VR865    XC    REQDEST,REQDEST     No destination since = "FILE"                
         GOTO1 RFPADD,APPARM,REQREC,REQHEAD                                     
         BNE   VR875                                                            
*                                                                               
VR870    MVI   APMODE,APMFMOK                                                   
*                                                                               
VR875    B     EXIT                                                             
*                                                                               
VR900    MVC   REQRUN,SPACES                                                    
         OI    REQRUNH+6,FVOXMT    TRANSMIT                                     
         SR    RE,RE                                                            
*                                                                               
VR999    B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  SET UP FIELD FOR VALIDATION ROUTINE AND GO                         *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
VALROUT  NTR1  ,                                                                
         ST    RF,SVRF             SAVE RF                                      
         GOTO1 AFVAL,(R6)                                                       
         SR    RF,RF                                                            
         ICM   RF,3,REPFDDSP                                                    
         AR    RF,R3                                                            
         ST    RF,AREQLOC          Save request card location                   
         L     RF,SVRF             RESTORE RF                                   
         TM    REPFIND1,REPFREQ    REQUIRED INPUT FIELD                         
         BZ    VALRO10                                                          
         CLI   FVILEN,0                                                         
         BE    ERRMISS                                                          
         B     VALRO20                                                          
*                                                                               
VALRO10  DS    0H                                                               
*&&UK                                                                           
         CLI   REPFFLDN,FLDNPRDR   PERIOD FIELD?                                
         BNE   *+8                                                              
         ST    R6,SVPERDAH         SAVE FIELD HEADER                            
*&&                                                                             
         CLI   REPFFLDN,FLDNACCT   ACCOUNT NEEDED IF SOONING                    
         BE    VALRO20                                                          
         CLI   REPFFLDN,FLDNTYPE   SPECIAL CASE (ALTERNATE TYPE)                
         BE    VALRO20                                                          
         TM    REPFIND2,REPFPRF    DO WE NEED TO REFILL PROFILE?                
         BO    VALRO20                                                          
         CLI   FVILEN,0            ANY DATA AND DO WE CARE                      
         BH    VALRO20             YES - VALIDATE FIELD                         
*&&UK                                                                           
         TM    FFOUND,FPERIOD      PERIOD TYPE KEYOWRDS?                        
         BZ    VALROUTX            NO - OK                                      
         CLI   REPFFLDN,FLDNCNDR   CALENDER FIELD                               
         BNE   VALROUTX            NO - OK                                      
         L     RE,AREQLOC                                                       
         CLC   0(L'ACQSTART+L'ACQEND,RE),SPACES                                 
         BNE   VALROUTX            NO,  SKIP                                    
         MVC   FVMSGNO,=AL2(574)                                                
         MVC   APCURSOR,SVPERDAH   SET CURSOR TO PERIOD FIELD                   
*&&                                                                             
         B     VALROUTX            DATE RANGE REQUIRED                          
*                                                                               
VALRO20  DS    0H                                                               
         OI    6(R6),FVOXMT        RETRANSMIT AFTER VALIDATION                  
         SRL   RF,24               MOVE ROUTINE # TO LOB                        
         SLL   RF,2                TIME 4                                       
         B     *(RF)                                                            
*                                                                               
         B     VALUNLG              1) UNIT/LEDGER                              
         B     VALACNT              2) ACCOUNT                                  
         B     VALCNTR              3) CONTRA ACCOUNT                           
         B     VALOFGP              4) OFFICE GROUP                             
         B     VALOTHR              5) GENERAL/OTHER                            
         B     VALWKCD              6) WORKCODE                                 
         B     VALESTS              7) ESTIMATE STATUS                          
         B     VALDTES              8) DATES                                    
         B     VALMOAR              9) MOA RANGE                                
         B     VALALDT             10) ALTERNATE DATE TYPE                      
         B     VALTRNT             11) TRANSACTION TYPE                         
         B     VALCLIA             12) CLIENT/COSTING ACCT.                     
         B     VALBUDG             13) BUDGET         (APG)                     
         B     VALOPTS             14) OPTIONS        (APG)                     
         B     VAL2DEC             15) DECIMAL $      (APG)                     
         B     VALNARR             16) NARRATIVE      (APG)                     
         B     VALMTHD             17) METHOD                                   
         B     VALRANGE            18) DATE RANGE                               
         B     VALLOCS             19) LOCATION STATUS                          
         B     VALPRSC             20) PERSON CODE                              
         B     VALBILT             21) BILLING TYPE                             
         B     VALRECAP            22) RECAP                                    
         B     VALYNO              23) YES, NO OR ONLY                          
*&&US*&& B     VALAUTH             24) VALIDATE AUTHORIZATION (US ONLY)         
*&&UK*&& B     VALROUTX            24) N/D FOR UK                               
         B     VALOFFC             25) OFFICE                                   
*                                                                               
VALROUTX CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE UNIT/LEDGER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
VALUNLG  CLI   APGFLAG,YES                                                      
         BNE   VALUNL10                                                         
         BRAS  RE,ADDAPGUL         ADD APG U/L ELEMENT TO RECORD                
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS ?                                
         BNE   IVALEXIT            YES, EXIT                                    
*                                                                               
VALUNL10 MVI   MULTIUL,NO          NOT  MULTIPLE UNIT LEDGERS                   
         SR    R5,R5                                                            
         TM    REPFIND1,REPFNME    IS THERE A NAME (DETAIL) FIELD ?             
         BZ    VALUNL12            NO,  SKIP                                    
         IC    R5,0(,R6)                                                        
         AR    R5,R6               POINT TO NAME FIELD HEADER                   
*                                                                               
VALUNL12 LR    RF,R6                                                            
         S     RF,ATWA                                                          
         ST    RF,AFLDUNLG         SAVE OFF DISPLACEMENT                        
         BRAS  RE,CHKSPC                                                        
         CLI   APPARM,1            USER DATA SUPPLIED ?                         
         BE    VALUNL30            YES, PROCESS IT                              
         BNL   VALUNL20            NO,  U/L  FLD  NOT EMPTY, CONTINUE           
         BRAS  RE,CHKRFILL         REFILL    LEGITIMATE ?                       
         BE    ERRMISS             YES, ERROR                                   
         B     VALUNL90            EXIT                                         
*                                                                               
VALUNL20 CLI   APPARM,3            MULTIPLE ITEMS OR LISTS ?                    
         BNE   ERRIVPUT            NO,  ERROR                                   
         MVI   MULTIUL,YES         HAVE MULTIPLE UNIT LEDGERS                   
         CLI   APMLDGR,YES         MAY  WE HAVE MULTIPLE LEDGERS ?              
         BE    VALUNL90            YES, OKAY                                    
         CLI   APGFLAG,YES         APG  UNIT/LEDGERS ?                          
         BE    VALUNL90            YES, OKAY                                    
         B     ERRIVPUT            NO,  ERROR                                   
*                                                                               
VALUNL30 LA    R1,8(,R6)           POINT TO DATA                                
         MVC   FVXTRA(2),0(R1)                                                  
         GOTO1 VALLEDG                                                          
         BNE   VALROUTX            ERROR CODE ALREADY SET                       
         CLC   CUAUTH+1(1),ACLEDSEC+1                                           
         BL    ERRULSEC            CHECK LEDGER SECURITY                        
         MVC   FVXTRA(2),SPACES                                                 
         SR    RF,RF                                                            
         IC    RF,0(,R5)           LENGTH OF NAME FIELD                         
         SHI   RF,8                MINUS HEADER LENGTH                          
         TM    1(R5),FVAXTND                                                    
         BZ    *+8                                                              
         SHI   RF,8                                                             
         LA    R1,L'ACULNAME                                                    
         CR    RF,R1                                                            
         BL    *+6                                                              
         LR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   8(0,R5),ACULNAME    REFRESH UNIT/LEDGER NAME                     
         OI    6(R5),FVOXMT        TRANSMIT                                     
         MVC   ACQUNT(2),8(R6)                                                  
         MVC   APREPUL(2),8(R6)                                                 
*                                                                               
         ICM   RF,15,ACLE#TRX      TEST TRX COUNT SET ON LEDGER                 
         BZ    VALUNL90                                                         
         MVI   RUNJOB,RUNSHORT                                                  
         C     RF,=A(MAXSHORT)     TEST MAX COUNT FOR SHORT SOON                
         BNH   *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
*                                                                               
VALUNL90 B     VALROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE ACCOUNT DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
VALACNT  SR    R5,R5                                                            
         TM    REPFIND1,REPFNME    IS THERE A NAME (DETAIL) FIELD ?             
         BZ    *+10                NO,  SKIP                                    
         IC    R5,0(,R6)                                                        
         AR    R5,R6               ADDR OF NEXT (DETAIL) FIELD NAME             
         MVI   APBYTE,NO           SET  TO ACCOUNT                              
         ST    R6,AACTHDRH         SAVE FIELD HEADER                            
         BRAS  RE,CHKSPC           IF   BLANK, INSERT FIELD FROM RCD            
*                                                                               
         CLI   APPARM,1            ANY  ACCOUNT DATA ?                          
         BNL   VALACN04            YES                                          
         CLI   RUNJOB,RUNSHORT     TEST SET AT LEDGER LVL                       
         BE    *+8                                                              
         MVI   RUNJOB,RUNLONG      NO, MAKE LONG RUNNING JOB                    
         B     VALROUTX                                                         
                                                                                
VALACN04 CLI   MULTIUL,YES         MULTIPLE UNIT/LEDGERS ?                      
         BNE   VALACN10                                                         
         MVI   RUNJOB,RUNLONG                                                   
         CLI   APPARM,1                                                         
         BE    ERRIVPUT            NO,  INVALID INPUT                           
*                                                                               
VALACN10 DS    0H                                                               
         CLI   APPARM,1            USER DATA SUPPLIED ?                         
         BE    VALACN20            YES, VALIDATE SINGLE ACCOUNT                 
*                                  LIST(S) OR MULTIPLE ITEMS SUPPLIED           
         CLI   APPARM,2            IS   IT +/- LIST?                            
         BNE   VALACN20            NO,  ACCOUNT SET                             
         CLI   APGFLAG,YES         APG  FORMAT ?                                
         BE    ERRIVPUT            YES, INVALID INPUT                           
         GOTOR VALLST,APPARM,('FLDNACCT',(R6)),(R5),RR=APRELO                   
*                                  VALID SINGLE LIST ?                          
         CLI   APPARM,0            SHOULD NOT COME BACK AS A 1 OR 3             
         BNE   ERRLIST             THUS, BAD LIST IF >= 1                       
         MVI   APPARM,2            RESET TO SAY +/- LIST                        
*                                                                               
VALACN20 DS    0H                                                               
         CLI   FVIFLD,C'*'         EXCLUDE REQUESTED ?                          
         BE    VALACN25            YES, SKIP                                    
         CLI   FVILEN,LACCOUNT     IS   THE LENGTH > 12 ?                       
         BH    ERRIVPUT            YES, INVALID INPUT                           
         MVC   ACQACT,FVIFLD       MOVE FIELD TO CARD                           
         B     VALACN30            CONTINUE                                     
*                                                                               
VALACN25 DS    0H                  EXCLUDE REQUESTED                            
         TM    REPFIND1,REPFXLD    EXCLUDE SUPPORTED ?                          
         BZ    ERRXCLUD            NO,  NOT VALID                               
         CLI   FVILEN,LACCOUNT+1   IS   THE LENGTH > 13 ?                       
         BH    ERRIVPUT            YES, INVALID INPUT                           
         MVC   ACQACT,FVIFLD+1     MOVE ACTUAL DATA TO CARD                     
         CLI   FVXLEN,0            ONLY '*' INPUTED ?                           
         BE    ERRIVPUT            YES, INVALID INPUT                           
*                                                                               
VALACN30 DS    0H                  INCLUDE/EXCLUDE REQUESTED                    
         CLI   APPARM,2            MULTIPLE ITEMS OR LISTS ?                    
         BNH   VALACN40                                                         
         LR    R1,R5               R1 POINTS TO NEXT FIELD HDR                  
         CLI   8(R1),C'?'          STARTS WITH A WILDCARD?                      
         BE    VALACN38                                                         
*                                                                               
VALACN35 CLI   8(R1),X'40'         END OF LIST?                                 
         BE    VALACN90                                                         
         CLC   8(1,R1),SCCOMMA     LOOKING FOR LIST                             
         BE    *+12                                                             
         AHI   R1,1                                                             
         B     VALACN35                                                         
         CLI   9(R1),C'?'          STARTS WITH A WILDCARD?                      
         BE    VALACN38                                                         
         AHI   R1,1                                                             
         B     VALACN35                                                         
*                                                                               
VALACN38 CLI   RUNJOB,RUNSHORT     TEST SET AT LEDGER LVL                       
         BE    *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
         B     VALACN90            MUST BE "(ACCT PROF)", EXIT                  
*                                                                               
VALACN40 CLI   APMLDGR,YES                                                      
         BNE   VALACN50                                                         
         L     RF,AFLDUNLG         POINT TO U/L HEADER FIELD                    
         A     RF,ATWA                                                          
         CLI   8(RF),C'('          IS MULTIPLE LEDGERS?                         
         BE    VALACN50            YES, SKIP                                    
         MVC   ACQUNT(2),8(RF)     NO,  INSERT UNIT AND LEDGER                  
*                                                                               
VALACN50 DS    0H                                                               
         CLI   APPARM,1            SINGLE ACCOUNT ?                             
         BNE   VALACN62            NO                                           
         CLI   APGFLAG,YES         APG  FORMAT ?                                
         BNE   VALACN52            NO,  CONTINUE                                
         CLI   MULTIUL,YES         MULTIPLE U/LS ?                              
         BE    ERRIVPUT            YES, INVALID INPUT                           
*                                                                               
VALACN52 DS    0H                                                               
*                                                                               
         TM    REPFIND2,REPFWLD    SUPPORT FOR WILD CARD?                       
         BZ    VALACN55            NO                                           
         GOTOR WILDCARD,APPARM,ACQUNT,(1,L'ACQACT+2),RR=APRELO                  
*                                  ANY WILDCARD CHARACTERS ?                    
         BM    ERRIVPUT            BAD, INVALID INPUT                           
         BZ    VALACN55            NO,VALIDATE                                  
         CLI   ACQACT,C'?'         COULD IT BE A LONG-RUNNING JOB?              
         BNE   VALACN70            NO, SKIP VALIDATION                          
         CLI   RUNJOB,RUNSHORT     TEST SET AT LEDGER LVL                       
         BE    *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
         B     VALACN70            SKIP VALIDATION                              
*                                                                               
         USING ACTRECD,R1                                                       
VALACN55 DS    0H                                                               
         LA    R1,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN           COMPANY CODE                            
         MVC   ACTKUNT(LUNLG),APREPUL   UNIT LEDGER                             
         MVC   ACTKACT,ACQACT           ACCOUNT                                 
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   ERRIVPUT                 INVALID INPUT                           
         DROP  R1                                                               
*                                                                               
         GOTO1 GETNAME,APPARM,AIOAREA2,(R5)                                     
         OI    6(R5),FVOXMT        RETRANSMIT                                   
         USING ACTRECD,RF                                                       
         SR    R0,R0                                                            
         L     RF,AIOAREA2                                                      
         AH    RF,DATADISP                                                      
         USING ABLELD,RF                                                        
VALACN56 CLI   ABLEL,0                                                          
         BE    VALACN70                                                         
         CLI   ABLEL,NUMELQ                                                     
         BE    VALACN58                                                         
         CLI   ABLEL,ABLELQ                                                     
         BE    *+14                                                             
VALACN57 IC    R0,ABLLN                                                         
         AR    RF,R0                                                            
         B     VALACN56                                                         
                                                                                
         CLI   ABLLN,ABLLN3Q                                                    
         BL    VALACN70            NO TRX COUNT - USE OLD-STYLE TESTS           
         ICM   RE,15,ABLTXS                                                     
         B     VALACN60                                                         
*                                                                               
         USING NUMELD,RF                                                        
VALACN58 CLI   NUMLN,NUMLN2Q                                                    
         BL    VALACN57                                                         
         CLI   NUMTYPE,NUMTYHIQ    TEST TRX COUNT EL, HIGH LVL A/C              
         BNE   VALACN57                                                         
         ICM   RE,15,NUM#TRX                                                    
         DROP  RF                                                               
VALACN60 STCM  RE,15,ACAC#TRX      SAVE COUNT                                   
                                                                                
VALACN62 ICM   RE,15,ACAC#TRX                                                   
         CLI   FVIFLD,C'*'         TEST -VE SINGLE A/C                          
         BE    *+8                                                              
         CLI   FVIFLD,C'-'         OR -VE LIST                                  
         BE    *+10                                                             
         LR    RF,RE               NO, SIMPLE TOTAL                             
         B     VALACN64                                                         
         ICM   RF,15,ACLE#TRX      ELSE TAKE LEDGER TOTAL...                    
         BZ    VALACN80                                                         
         SR    RF,RE               ...MINUS A/C OR A/C LIST TOTAL               
VALACN64 MVI   RUNJOB,RUNSHORT                                                  
         C     RF,=A(MAXSHORT)     TEST MAX COUNT FOR SHORT SOON                
         BNH   *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
         B     VALACN80                                                         
*                                                                               
VALACN70 CLI   APREPJCL,REPJCLV    PRODUCTION?                                  
         BNE   *+14                                                             
         CLC   APREPUL,=C'SJ'                                                   
         BE    VALACN72                                                         
         GOTO1 GETLEDG,APREPUL     GET LEVEL STRUCTURE                          
         CLI   ACLEV1,12           ONLY ONE LEVEL?                              
         BE    VALACN72                                                         
         LA    R1,ACQACT                                                        
         ZIC   R0,ACLEV2                                                        
         AR    R1,R0               POINT TO THIRD LEVEL                         
         CLI   0(R1),X'40'                                                      
         BH    VALACN72                                                         
         CLI   ACLEV3,0            ONLY 2 LEVELS?                               
         BNE   VALACN80                                                         
         SR    R1,R0                                                            
         IC    R0,ACLEV1                                                        
         AR    R1,R0               POINT TO SECOND LEVEL                        
         CLI   0(R1),X'40'                                                      
         BNH   VALACN80                                                         
*                                                                               
VALACN72 MVI   RUNJOB,RUNSHORT                                                  
*                                                                               
VALACN80 CLI   FVIFLD,C'*'         EXCLUDE REQUESTED ?                          
         BNE   *+8                 NO,  EXIT                                    
         NI    ACQACT,TURNOFF-X'40'     MAKE LOWER CASE LETTER                  
*                                                                               
VALACN90 DS    0H                                                               
         OI    PRFFLAG,PRFACC      PROFILE HAS ACCOUNTS                         
         B     VALROUTX            EXIT                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE CONTRA ACCOUNT DATA AND                                   *         
*  VALIDATE BILLING SOURCE DATA                                       *         
*                                                                     *         
*  NOTE: NO CHECK FOR WILDCARD CHARACTERS FOR CONTRA ACCOUNTS         *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
         USING ACQD,R3                                                          
         USING RTND,R4             RTND ADDRESSABILITY                          
VALCNTR  DS    0H                                                               
         L     R4,ARTND            ->   RTND                                    
*                                  GET FIRST AVAILABLE FLD IN REQ CARD          
         GOTO1 GETFFLD,APPARM,(R3)                                              
         L     R8,APPARM           AVAILABLE LOCATION IN REQ CARD               
         SR    R5,R5                                                            
         TM    REPFIND1,REPFNME    IS THERE A NAME (DETAIL) FIELD ?             
         BZ    *+10                NO,  SKIP                                    
         IC    R5,0(,R6)                                                        
         AR    R5,R6               ADDR OF NEXT (DETAIL) FIELD NAME             
*                                                                               
         BRAS  RE,CHKSPC           IF   BLANK,INSERT FIELD FROM RCD             
         CLI   APPARM,1                                                         
         BL    VALROUTX            NO INPUT, EXIT                               
         BE    VALCNT20            NOT LIST DATA, VALIDATE ACCOUNT              
*                                  LIST(S) OR MULTIPLE ITEMS SUPPLIED           
         CLI   APPARM,2            LIST(S) SUPPLIED ?                           
         BNE   VALCNT20            NO, CHECK FOR EXCLUDE                        
*                                                                               
         GOTOR VALLST,APPARM,(RTNFLDN,(R6)),(R5),RR=APRELO                      
*                                                                               
*                                  VALID SINGLE LIST ?                          
         CLI   APPARM,0            SHOULD NOT COME BACK AS 1 OR 3               
         BNE   ERRLIST             THUS BAD LIST IF >= 1                        
         MVI   APPARM,2            RESET TO SAY +/- LIST                        
*                                                                               
VALCNT20 DS    0H                                                               
         MVI   0(R8),ACQCNTR       CONTRA ACOUNT/LIST                           
         CLI   FVIFLD,C'*'         EXCLUDE REQUESTED ?                          
         BE    VALCNT25            YES, SKIP                                    
         CLI   RTNFLDN,FLDNBSRC    BILLING SOURCE ?                             
         BNE   VALCNT22            NO,  SKIP                                    
         CLI   APPARM,1            SINGLE ACCOUNT ?                             
         BE    VALCNT23            YES, USE ALTERNATE MOVE                      
*                                                                               
VALCNT22 DS    0H                  MOVE FIELD TO CARD                           
         CLI   FVILEN,LULACNT      IS   THE LENGTH > 14 ?                       
         BH    ERRIVPUT            YES, INVALID INPUT                           
         MVC   1(L'ACQFLT1,R8),FVIFLD                                           
         B     VALCNT30            CONTINUE                                     
*                                                                               
VALCNT23 DS    0H                  MOVE FIELD TO CARD                           
         CLI   FVILEN,LACCOUNT     IS   THE LENGTH > 12 ?                       
         BH    ERRIVPUT            YES, INVALID INPUT                           
         MVC   3(L'ACQFLT1-2,R8),FVIFLD                                         
         B     VALCNT30            CONTINUE                                     
*                                                                               
VALCNT25 DS    0H                  EXCLUDE REQUESTED                            
         TM    REPFIND1,REPFXLD    EXCLUDE SUPPORTED ?                          
         BZ    ERRXCLUD            NO,  NOT VALID                               
         CLI   FVXLEN,0            ONLY '*' INPUTED ?                           
         BE    ERRIVPUT            YES, INVALID INPUT                           
         CLI   RTNFLDN,FLDNBSRC    BILLING SOURCE ?                             
         BNE   VALCNT27            NO,  SKIP                                    
         CLI   APPARM,1            SINGLE ACCOUNT ?                             
         BE    VALCNT28            YES, USE ALTERNATE MOVE                      
*                                                                               
VALCNT27 DS    0H                  MOVE FIELD TO CARD                           
         CLI   FVILEN,LULACNT+1    IS   THE LENGTH > 15 ?                       
         BH    ERRIVPUT            YES, INVALID INPUT                           
*                                  MOVE ACTUAL DATA TO CARD                     
         MVC   1(L'ACQFLT1,R8),FVIFLD+1                                         
         B     VALCNT30            CONTINUE                                     
*                                                                               
VALCNT28 DS    0H                  MOVE FIELD TO CARD                           
         CLI   FVILEN,LACCOUNT+1   IS   THE LENGTH > 13 ?                       
         BH    ERRIVPUT            YES, INVALID INPUT                           
         MVC   1(2,R8),SPACES                                                   
         MVC   3(L'ACQFLT1-2,R8),FVIFLD+1                                       
*        B     VALCNT30            CONTINUE                                     
*                                                                               
VALCNT30 DS    0H                  INCLUDE/EXCLUDE REQUESTED                    
         CLI   APPARM,1            SINGLE ACCOUNT ?                             
         BNE   VALROUTX            NO,  EXIT                                    
         CLI   RTNFLDN,FLDNBSRC    BILLING SOURCE ?                             
         BE    VALCNT60            NO,  DO NOT VALIDATE                         
*                                                                               
         USING CNTRTBLD,RF         CONTRA ACCOUNT TABLE                         
         L     RF,ACCNTTAB         ADDR OF CONTRA   TABLE                       
         CLI   APREPJCL,REPJCLV    IS   IT PRODUCT. TYPE ?                      
         BE    VALCNT50            YES, SO DO NOT   VALIDATE                    
         CLI   APREPJCL,REPJCLX    IS   IT EXPENSE  TYPE ?                      
         BE    VALCNT50            YES, SO DO NOT   VALIDATE                    
         CLI   APREPJCL,REPJCLP    IS   IT PAYABLES TYPE ?                      
         BE    VALCNT50            YES, SO DO NOT   VALIDATE                    
         CLI   APREPJCL,REPJCLB    IS   IT CASH     TYPE ?                      
         BE    VALCNT50            YES, SO DO NOT   VALIDATE                    
         CLI   APREPJCL,REPJCLG    IS   IT G/L      TYPE ?                      
         BE    VALCNT50            YES, SO DO NOT   VALIDATE                    
*                                                                               
VALCNT40 DS    0H                                                               
         CLC   APREPNUM,CNTRRTYP   MATCHING REPORT TYPE ?                       
         BNE   VALCNT45            NO,  TRY NEXT ENTRY                          
         CLC   CNTRUNLG,1(R8)      MATCHING UNIT/LEDGER ?                       
         BE    VALCNT50            YES, GOOD CONTRA ACCOUNT                     
*                                                                               
VALCNT45 DS    0H                                                               
         LA    RF,CNTRLNQ(,RF)     NEXT TABLE ENTRY                             
         CLI   0(RF),EOT           END  OF  CONTRA TABLE ?                      
         BNE   VALCNT40            NO,  TRY AGAIN                               
         B     ERRCNTRA            YES, INVALID CONTRA ACCOUNT                  
         DROP  RF                                                               
*                                                                               
         USING ACTRECD,R1                                                       
VALCNT50 DS    0H                                                               
         LA    R1,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN           COMPANY CODE                            
         MVC   ACTKUNT(LULACNT),1(R8)   UNIT LEDGER AND ACCOUNT                 
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   ERRCNTR                  INVALID CONTRA ACCOUNT                  
         DROP  R1                                                               
*                                                                               
         GOTO1 GETNAME,APPARM,AIOAREA2,(R5)                                     
         OI    6(R5),FVOXMT        RETRANSMIT                                   
*                                                                               
VALCNT60 DS    0H                                                               
         CLI   FVIFLD,C'*'              EXCLUDE REQUESTED ?                     
         BNE   VALROUTX                 NO,  EXIT                               
         NI    1(R8),TURNOFF-X'40'      MAKE LOWER CASE LETTER                  
         B     VALROUTX                 EXIT                                    
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
***********************************************************************         
*  OFFICE GROUP - WORK CODE GROUP                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RTND,R4             RTND ADDRESSABILITY                          
VALOFGP  DS    0H                                                               
         L     R4,ARTND            ->   RTND                                    
         BRAS  RE,CHKSPC                                                        
         BE    VGENERAL            ASSUME SPACES OR PROFILE IS CORRECT          
         GOTOR CHKLIST,APPARM,(RTNFLDN,(R6)),RR=APRELO                          
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VGENERAL                                                         
         B     VALROUTX                                                         
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  BILLING TYPE                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALBILT  DS    0H                                                               
         BRAS  RE,CHKSPC                                                        
         BE    VGENERAL            ASSUME SPACES OR PROFILE IS CORRECT          
         CLI   FVIFLD,C'('         STRING      LIST ?                           
         BE    VGENERAL            YES,   ASSUME    OKAY                        
         LA    RE,FVIFLD           ->     INPUT     DATA                        
         ZIC   RF,FVXLEN           GET    LENGTH    OF   INPUT DATA - 1         
         CLI   FVIFLD,C'*'         EXCLUDE     REQUESTED ?                      
         BNE   VALBIL10            NO,    SKIP                                  
         LA    RE,1(,RE)           ->     REAL DATA                             
         BCTR  RF,0                SUBTRACT    ONE  FROM DATA LENGTH            
*                                                                               
VALBIL10 DS    0H                                                               
         LTR   RF,RF               IS     INPUT     DATA LENGTH   1 ?           
         BNZ   ERRIBTYP            NO,    INVALID   BILLING TYPE                
         L     RF,=A(BILLTY)       ->     BILLING   TYPE TABLE                  
         A     RF,APRELO           RELOCATE    THE  TABLE                       
*                                                                               
VALBIL20 DS    0H                                                               
         CLI   0(RF),EOT           END    OF   TABLE     ?                      
         BE    ERRIBTYP            YES,   INVALID   BILLING TYPE                
         CLC   0(1,RF),0(RE)       MATCH  BILLING   TYPE ?                      
         BE    VALBIL50            YES,   GOOD DATA                             
         LA    RF,1(,RF)           NO,    TRY  NEXT BILLING TYPE                
         B     VALBIL20            TEST   NEXT BILLING  TYPE                    
*                                                                               
VALBIL50 DS    0H                                                               
         B     VGENERAL            INPUT  IS   OKAY                             
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE YES/NO/ONLY                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT REQUEST SCREEN                        
VALYNO   MVI   APBYTE,YES                                                       
         CLC   APYES,FVIFLD        YES ?                                        
         BE    VALYNO90                                                         
         MVI   APBYTE,NO                                                        
         CLC   APNO,FVIFLD         NO                                           
         BE    VALYNO90                                                         
         MVI   APBYTE,ONLY                                                      
         CLC   APONLY,FVIFLD       ONLY                                         
         BE    VALYNO90                                                         
         CLI   REPFFLDN,FLDNFRPT   Format report ?                              
         BE    VALYNO30            Yes, try blank                               
         CLI   REPFFLDN,FLDNRCON   Reconcile ?                                  
         BNE   VALYNO40            No, blank is not valid                       
         CLI   RECAP#,0            Recapping ?                                  
         BE    VALYNO40            No                                           
*                                                                               
VALYNO30 CLI   FVIFLD,C' '         Blank is valid                               
         BE    VALROUTX            Exit if it is blank                          
*                                                                               
VALYNO40 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALROUTX            NOT         OKAY                             
*                                                                               
VALYNO90 L     RE,AREQLOC                                                       
         MVC   0(1,RE),APBYTE                                                   
         B     VALROUTX            INPUT  IS   OKAY                             
         EJECT ,                                                                
*&&US                                                                           
***********************************************************************         
*  VALIDATE AUTHORIZATION                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING AUTRECD,R1                                                       
         USING REPFD,R2            REPORT REQUEST SCREEN                        
         USING ACQD,R3                                                          
VALAUTH  LA    R1,IOKEY                                                         
         XC    AUTKEY,AUTKEY                                                    
         MVI   AUTKTYP,AUTKTYPQ    X'3F' Authroization record                   
         MVI   AUTKSUB,AUTKSUBQ    X'04' Sub-type                               
         MVC   AUTKCPY,CUABIN      COMPANY CODE                                 
         MVI   AUTKSEQ,X'40'       Just is                                      
         MVC   AUTKUNT(2),=C'SJ'                                                
         CLI   ACQOFGRP,C' '       Office Group ?                               
         BNH   *+10                No                                           
         MVC   AUTKOGR,ACQOFGRP                                                 
                                                                                
         CLC   ACQOFFFL,SPACES     Office ?                                     
         BNH   *+10                No                                           
         MVC   AUTKOFC,ACQOFFFL                                                 
                                                                                
         CLC   ACQACT(3),SPACES    Check for client                             
         BNH   VALAUT20            No, so product either                        
         MVC   AUTKCLI,SPACES                                                   
         MVC   AUTKCLI(3),ACQACT   US, length is hard                           
                                                                                
         CLC   ACQACT+3(3),SPACES  Check for product                            
         BNH   VALAUT20            No                                           
         MVC   AUTKPRO,SPACES                                                   
         MVC   AUTKPRO(3),ACQACT+3                                              
                                                                                
VALAUT20 CLI   ACQMEDGP,C' '       Any media group requested ?                  
         BNH   *+10                No                                           
         MVC   AUTKMGR,ACQMEDGP                                                 
                                                                                
         CLI   ACQMEDFL,C' '       Any media requested ?                        
         BNH   *+10                No                                           
         MVC   AUTKMED,ACQMEDFL                                                 
                                                                                
         MVC   AUTKNUM,FVIFLD                                                   
         GOTO1 AIO,IOACCDIR+IORD+IO2                                            
         BE    VALAUT90                                                         
         MVC   FVMSGNO,=AL2(1834)                                               
         B     VALROUTX            Not okay                                     
         DROP  R1,R3                                                            
                                                                                
VALAUT90 L     RE,AREQLOC                                                       
         MVI   0(RE),ACQAUTH                                                    
         MVC   1(L'ACQAUTH#,RE),FVIFLD                                          
         B     VALROUTX            Input is okay                                
         EJECT ,                                                                
*&&                                                                             
***********************************************************************         
*  VALIDATE RECAP FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            Report request screen                        
VALRECAP LH    R4,REPFDDSP         Get report number                            
         LA    R4,RQRCAPSW(R4)     ->  Recap ACQTYP1 field                      
         BCTR  R4,0                                                             
         MVI   0(R4),C'Y'          Assume recap                                 
         CLC   APYES,8(R6)         Recap requested ?                            
         BE    VALRCP10            Yes,  Okay                                   
         MVI   0(R4),C'N'          Assume not recaps                            
         CLC   APNO,8(R6)          No, recap requested ?                        
         BE    VALROUTX            Yes, Okay                                    
         B     ERRIVPUT            No, invalid input                            
*                                                                               
*ALRCP10 OI    FFOUND,FRECAP       Recap is set to yes                          
VALRCP10 LR    R1,R6               Point to Y/N field for recap                 
         SHI   R1,16               Bounce back to recap code field              
         GOTO1 AFVAL                                                            
*                                                                               
         USING RECAPD,RE           Recap table                                  
         SR    RF,RF                                                            
         ICM   RF,1,RECAP#         Number of recaps                             
         BNZ   *+6                                                              
         DC    H'00'               This shouldn't happen (no Recaps)            
*                                                                               
         LA    RE,RCAPFMTS         A(Saved recap codes)                         
VALRCP15 CLC   RECAPFMT,FVIFLD                                                  
         BE    VALRCP18                                                         
         AHI   RE,RECAPLNQ         Bump to next Recap                           
         BCT   RF,VALRCP15                                                      
         DC    H'00'               No Match, Somethings wrong                   
*                                                                               
VALRCP18 CLC   RECAPERR,=AL2(FVFOK)                                             
         BE    VALROUTX                                                         
         MVC   FVMSGNO,RECAPERR    Set error that was saved                     
         B     ERRXIT                                                           
         DROP  RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  OTHER GENERAL VALIDATION                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING ACQD,R3                                                          
         USING RTND,R4             Routine table                                
VALOTHR  L     R4,ARTND            Current entry in table                       
         BRAS  RE,CHKSPC           Check for input                              
         B     VGENERAL            No, try gernal validation                    
         SPACE 2                                                                
***********************************************************************         
*  OFFICE VALIDATION                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING ACQD,R3                                                          
         USING RTND,R4             Routine table                                
VALOFFC  DS    0H                                                               
         L     R4,ARTND            Current entry in table                       
         BRAS  RE,CHKSPC           Check for input                              
         L     RF,AREQLOC          Request card location                        
                                                                                
         GOTOR VALOFF,APPARM,(R6),(RF),(R4),RR=APRELO                           
         B     VALROUTX                                                         
         DROP  R3,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  WORKCODE                                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
VALWKCD  BRAS  RE,CHKSPC                                                        
         BNE   VALWKCD1            BRANCH IF PERSON ENTERED DATA                
         CLI   APPARM,1                                                         
         BL    VALROUTX            NO DATA ON PROFILE                           
         BE    VALWKCD1                                                         
         B     VGENERAL            MUST BE A +/- LIST OR A STRING LIST          
*                                                                               
         USING WCORECD,R1                                                       
VALWKCD1 CLI   APPARM,1                                                         
         BH    VALWKCD8                                                         
         LA    R1,IOKEY                                                         
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ    X'0A'                                        
         MVC   WCOKCPY,CUABIN      COMPANY                                      
         MVC   WCOKUNT(2),=C'SJ'                                                
         LA    RE,FVIFLD                                                        
         CLI   FVILEN,2            COMPARE LENGTH TO 2                          
         BL    ERRWRKC             LOW,    INVALID WORK CODE                    
         BE    VALWKCD5            EQUAL,  CONTINUE                             
         CLI   FVILEN,3            COMPARE LENGTH TO 3                          
         BH    ERRWRKC             HIGH,   INVALID WORK CODE                    
*                                  MUST    BE EQUAL                             
         CLI   FVIFLD,C'*'         EXCLUDE WORKCODE                             
         BNE   ERRWRKC             NO,     INVALID WORK CODE                    
         LA    RE,FVIFLD+1                                                      
*                                                                               
VALWKCD5 MVC   WCOKWRK(2),0(RE)                                                 
         CLC   WCOKWRK(2),=C'99'   SPECIAL CASE ?                               
         BE    VALWKCD7            YES,    SKIP VALIDATION                      
         CLC   WCOKWRK(2),=C'**'   SPECIAL CASE ?                               
         BE    VALWKCD7            YES,    SKIP VALIDATION                      
*                                                                               
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   ERRWRKC                                                          
         TM    REPFIND1,REPFNME                                                 
         BZ    VGENERAL                                                         
         SR    R5,R5                                                            
         TM    REPFIND1,REPFNME    IS   THERE A NAME (DETAIL) FIELD ?           
         BZ    *+10                NO,  SKIP                                    
         IC    R5,0(,R6)                                                        
         AR    R5,R6                                                            
         GOTO1 GETNAME,APPARM,AIOAREA2,(R5)                                     
         OI    6(R5),FVOXMT        RETRANSMIT                                   
*                                                                               
VALWKCD7 DS    0H                                                               
         B     VGENERAL                                                         
         DROP  R1                                                               
*                                                                               
VALWKCD8 DS    0H                                                               
         SR    R5,R5               CLEAR REGISTER                               
         TM    REPFIND1,REPFNME    IS   THERE A NAME (DETAIL) FIELD ?           
         BZ    *+10                NO,  SKIP                                    
         IC    R5,0(,R6)           ADDR OF NAME (DETAIL) FIELD                  
         AR    R5,R6                                                            
*                                                                               
         GOTOR VALLST,APPARM,('FLDNWKCD',(R6)),(R5),RR=APRELO                   
         CLI   APPARM,0            VALID SINGLE LIST ?                          
*                                  SHOULD NOT COME BACK WITH A 1 OR 3           
         BNE   ERRLIST             NO,  NOT A VALID LIST                        
         B     VGENERAL                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  ESTIMATE STATUS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R1                                                        
         USING REPFD,R2            REPORT REQUEST SCREEN                        
         USING ACQD,R3                                                          
VALESTS  CLI   FVILEN,0                                                         
         BNE   VALESTS2                                                         
         BRAS  RE,CHKRFILL         SHOULD WE REFILL ?                           
         BNE   VALROUTX            NO,    SKIP                                  
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RPFELQ                                                  
         GOTO1 GETEL                                                            
         BNE   VALROUTX                                                         
         MVI   5(R6),1                                                          
         MVC   8(1,R6),RPFESTST    ESTIMATE STATUS                              
         GOTO1 AFVAL,(R6)                                                       
*                                                                               
VALESTS2 CLI   FVILEN,0                                                         
         BE    VALROUTX            LEAVE                                        
         CLI   FVIFLD,C'A'         APPROVED ESTIMATES ONLY                      
         BE    *+8                                                              
         CLI   FVIFLD,C'U'         UNAPPROVED ESTIMATES                         
         BE    *+8                                                              
         CLI   FVIFLD,C'R'         REVISED ESTIMATES AWAITING APPRVL            
         BE    *+8                                                              
         CLI   FVIFLD,C'N'         REQUIRING JOBS REQUIRING ESTIMATES           
         BNE   ERRIVPUT            BUT DO NOT HAVE ESTIMATES                    
         MVC   ACQOPT7,FVIFLD                                                   
         B     VALROUTX                                                         
         DROP  R1,R2,R3                                                         
         EJECT ,                                                                
***********************************************************************         
*  DATES                                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
         USING RTND,R4             RTND ADDRESSABILITY                          
VALDTES  L     R4,ARTND            -> RTND                                      
         L     R5,AREQLOC          -> OUTPUT DATES                              
         MVI   DTESWS,0            Clear date switches                          
*                                                                               
         CLI   RTNFLDN,FLDNSTDT    START DATE?                                  
         BNE   VALDTE02            No, skip                                     
         XC    SVSTARTH,SVSTARTH   Clear start date header address              
         OI    DTESWS,DTESWSTA     Start Date                                   
*                                                                               
VALDTE02 CLI   RTNFLDN,FLDNACTS    ACTIVITY START DATE?                         
         BNE   VALDTE04            No, skip                                     
         XC    SVACTSTH,SVACTSTH   Clear activity start header addr             
         OI    DTESWS,DTESWSTA     Start Date                                   
*                                                                               
VALDTE04 CLI   RTNFLDN,FLDNALTS    ALTERNATE START DATE?                        
         BNE   *+12                No, skip                                     
         OI    DTESWS,DTESWSTA     START DATE                                   
         LA    R5,ALTSDTE                                                       
*                                                                               
         CLI   RTNFLDN,FLDNALTE    ALTERNATE END DATE                           
         BNE   *+8                                                              
         LA    R5,ALTEDTE                                                       
*                                                                               
         CLI   RTNFLDN,FLDNOCUR    OVERRIDE CURRENT DATE                        
         BNE   *+8                                                              
         LA    R5,OCURDTE                                                       
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VALROUTX            NOTHING HERE, NEXT                           
*                                                                               
         TM    RLPFLAG,RLPFON      CHECK RFP                                    
         BZ    VALDTE06            BAD DATE                                     
         CLI   RTNFLDN,FLDNALTS    ALTERNATE START DATE                         
         BE    *+8                                                              
         CLI   RTNFLDN,FLDNALTE    ALTERNATE END DATE                           
         BNE   *+8                                                              
         OI    RLPFLAG,RLPFALT     SET TO SAY ALTERNATE SYMBOL USED             
         GOTO1 RFPSYM,APPARM,(RTNFLDN,(R6)),(L'ALTSDTE,(R5))                    
         BE    VALDTEEX                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         USING SOFDATD,R1                                                       
VALDTE06 LA    R1,SOFBLOCK                                                      
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R6,SOFAINP                                                       
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVC   SOFLANG,CULANG                                                   
         MVC   SOFACFST,FISCALMO                                                
         MVI   SOFITYPE,SOFITYMD                                                
         MVI   SOFIINDS,SOFIIANY+SOFIIONE                                       
*                                                                               
         CLI   RTNFLDN,FLDNENDT              END DATE ?                         
         BE    VALDTE07                                                         
         CLI   RTNFLDN,FLDNALTE    ALTERNATE END DATE                           
         BE    VALDTE07                                                         
         CLI   RTNFLDN,FLDNACTE    ACTIVITY  END DATE                           
         BE    VALDTE07                                                         
         CLI   RTNFLDN,FLDNOCUR    OVERRIDE CURRENT DATE                        
         BNE   *+8                                                              
VALDTE07 OI    SOFITYPE,SOFITEND                                                
*                                                                               
         TM    RLPFLAG,RLPFON                                                   
         BO    *+8                                                              
         OI    SOFIINDS,SOFIIRES+SOFIIOUT  NOT RFP - RESOLVE DATES              
         MVI   SOFOTYPE,SOFOTSD2                                                
         ST    R5,SOFAOUT                                                       
         GOTO1 ASOFDAT                                                          
         BZ    *+14                                                             
         MVC   FVMSGNO,SOFERROR    OR ERROR NUMBER                              
         B     VALDTEEX                                                         
*                                                                               
         CLI   RTNFLDN,FLDNENDT    END   DATE  ?                                
         BNE   *+10                NO,   SKIP                                   
         XC    SVSTARTH,SVSTARTH   CLEAR START DATE HEADER ADDRESS              
*                                                                               
         CLI   RTNFLDN,FLDNACTE    ACTIVITY    END  DATE   ?                    
         BNE   *+10                NO,   SKIP                                   
         XC    SVACTSTH,SVACTSTH   CLEAR ACTIVITY   START  DATE ADDR            
*                                                                               
         CLI   RTNFLDN,FLDNSTDT    START DATE  ?                                
         BNE   *+8                 NO,   SKIP                                   
         ST    R6,SVSTARTH         SAVE  START DATE  HEADER ADDRESS             
*                                                                               
         CLI   RTNFLDN,FLDNACTS    ACTIVITY    START DATE   ?                   
         BNE   *+8                 NO,   SKIP                                   
         ST    R6,SVACTSTH         SAVE  ACTIVITY    START  HEADER ADDR         
*                                                                               
VALDTEEX B     VALROUTX            EXIT                                         
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  MOA RANGE                                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING ACQD,R3                                                          
         USING RTND,R4             RTND ADDRESSABILITY                          
VALMOAR  L     R4,ARTND            ->   RTND                                    
         TM    RLPFLAG,RLPFON      CHECK RFP                                    
         BZ    VALMOAR2                                                         
         GOTO1 RFPSYM,APPARM,(RTNFLDN,(R6)),(L'ACQMOSST*2,ACQMOSST)             
         BE    VALROUTX                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         USING SOFDATD,R1                                                       
VALMOAR2 LA    R1,SOFBLOCK                                                      
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R6,SOFAINP                                                       
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVC   SOFLANG,CULANG                                                   
         MVC   SOFACFST,FISCALMO                                                
         MVI   SOFITYPE,SOFITYM                                                 
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT+SOFIIF1O                              
         TM    RLPFLAG,RLPFON                                                   
         BO    *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         MVI   SOFOTYPE,SOFOTSD1                                                
         LA    R0,ACQMOSST                                                      
         ST    R0,SOFAOUT                                                       
         GOTO1 ASOFDAT                                                          
         MVC   FVMSGNO,=AL2(FVFOK) SET OK                                       
         BZ    *+10                                                             
         MVC   FVMSGNO,SOFERROR    OR ERROR NUMBER                              
         B     VALROUTX                                                         
         DROP  R1,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  ALTERNATE DATE TYPE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING ACQD,R3                                                          
VALALDT  OC    ALTSDTE(L'ALTSDTE*2),ALTSDTE    DATES ENTERED?                   
         BZ    VALALDTX            NO, FORGET ABOUT THIS THEN                   
         CLI   FVIFLD,0                                                         
         BE    ERRMISS                                                          
         GOTOR VALOPT,APPARM,('ACQOPT3-ACQD',(R6)),0,RR=APRELO                  
         BNE   VALALDTX                                                         
*&&UK                                                                           
         CLI   OPTEQU,X'01'        SPECIAL EARLY PAYMENT TYPE                   
         BNE   VALALD10                                                         
         LA    RF,ACQEPDST         A(EALRY PAYMENT DATES IN ACQD)               
         B     VALALD12                                                         
*&&                                                                             
VALALD10 GOTO1 GETFFLD,APPARM,REQCARDS                                          
         L     RF,APPARM                                                        
         MVI   0(RF),ACQDATE                                                    
         MVC   1(1,RF),OPTEQU                                                   
         LA    RF,ACQDTSTR-ACQTYP1(,RF)    POINT TO DATES AREA                  
*                                                                               
VALALD12 MVC   0(12,RF),ALTSDTE                                                 
         OC    ALTSDTE,ALTSDTE     IF NO INPUT THEN PAD WITH SPACES             
         BNZ   VALALD20                                                         
         MVC   0(6,RF),SPACES      DATE ALWAYS C'YYMMDD'                        
         B     VALALDTX            DON'T CHECK END BEFORE START                 
*                                                                               
VALALD20 OC    ALTEDTE,ALTEDTE     IF NO INPUT THEN PAD WITH SPACES             
         BNZ   VALALD25                                                         
         MVC   6(6,RF),SPACES      DATE ALWAYS C'YYMMDD'                        
         B     VALALDTX            DON'T CHECK END BEFORE START                 
*                                                                               
VALALD25 TM    RLPFLAG,RLPFALT                                                  
         BO    VALALDTX            DON'T CHECK END BEFORE START                 
         CLC   0(6,RF),6(RF)       START DATE > END DATE ?                      
         BH    ERREDBSD            YES, END DATE BEFORE START DATE              
*                                                                               
VALALDTX B     VALROUTX                                                         
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE DATE RANGE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RTND,R4             RTND ADDRESSABILITY                          
VALRANGE L     R4,ARTND            ->   RTND                                    
         L     R5,AREQLOC                                                       
*                                                                               
         TM    RLPFLAG,RLPFON      IS    IT   RFP ?                             
         BZ    VALRNG04            NO,   SO   JUMP TO SINGLE INPUT              
         GOTO1 RFPSYM,APPARM,(RTNFLDN,(R6)),(RTNFLDLN,(R5))                     
         BE    VALRNGX             NOT   AN   RFP  SYMBOL,   TRY SINGLE         
         MVC   FVMSGNO,=AL2(FVFOK) RESET                                        
*                                                                               
         USING SOFDATD,R1                                                       
VALRNG04 LA    R1,SOFBLOCK                                                      
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R6,SOFAINP                                                       
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,6           Set ACCPAK System                            
         MVC   SOFLANG,CULANG                                                   
         MVC   SOFACFST,FISCALMO                                                
         MVI   SOFITYPE,SOFITYMD   Dates are YYMMDD                             
         MVI   SOFOTYPE,SOFOTSD2   Output 6 byte EBCDIC pairs                   
         CLI   RTNFLDN,FLDNCNDR    Calendar date range is in use ?              
*     - - - - - - - - - - - - - - - - -                                         
         BE    VALRNG05                                                         
         CLI   RTNFLDN,FLDNMMOS    Media Month of Service in use ?              
         BNE   VALRNG06                                                         
         LA    R5,MMOSRNG                                                       
*     - - - - - - - - - - - - - - - - -                                         
VALRNG05 MVI   SOFITYPE,SOFITYM            Dates are YYMM                       
         MVI   SOFOTYPE,SOFOTSD2+SOFOTSPC  Output 6 byte EBCDIC pairs           
*                                             w/day as spaces                   
VALRNG06 MVI   SOFIINDS,SOFIIANY+SOFIIF1O+SOFIIF2O                              
         TM    RLPFLAG,RLPFON                                                   
         BO    *+8                                                              
         OI    SOFIINDS,SOFIIRES+SOFIIOUT  NOT RFP - RESOLVE DATES              
*                                                                               
         ST    R5,SOFAOUT                                                       
         MVC   FVMSGNO,=AL2(FVFOK) SET OK                                       
         GOTO1 ASOFDAT                                                          
         BZ    VALRNGX                                                          
         MVC   FVMSGNO,SOFERROR    OR ERROR NUMBER                              
*                                                                               
VALRNGX  B     VALROUTX            RETURN                                       
         DROP  R1,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  LOCATION STATUS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT   FIELD DSECT                         
VALLOCS  BRAS  RE,CHKSPC                                                        
         CLI   APPARM,0                                                         
         BNE   VALLOC05                                                         
         BRAS  RE,CHKRFILL         SHOULD   WE   REFILL ?                       
         BNE   VALLOCX             NO,      EXIT                                
         GOTOR DISLOCS,(R6),RR=APRELO                                           
         GOTO1 AFVAL,(R6)                                                       
         BNE   VALLOCX                                                          
*                                                                               
VALLOC05 DS    0H                                                               
         L     R5,ACLOSTAB         LOCATION STATUS TABLE                        
         ZIC   RE,FVILEN           FIELD    LENGTH                              
         ZIC   R8,FVXLEN           EX       INSTRUCTION LENGTH                  
         SR    R1,R1                                                            
*                                  SAVE     LOCATION STATUS INPUT               
         MVC   APWORK(LENFLOCS-1),FVIFLD                                        
         CLI   FVIFLD,C'*'                                                      
         BNE   VALLOC08                                                         
         TM    REPFIND1,REPFXLD    EXCLUDE  SUPPORTED ?                         
         BZ    VALLOCEX            NO,      INVALID EXCLUDE                     
         CLI   FVXLEN,0            ONLY     '*'  INPUTED ?                      
         BE    VALLOCER            YES,     INVALID LOCATION STATUS             
*                                  SAVE     LOCATION STATUS INPUT               
         MVC   APWORK(LENFLOCS-1),FVIFLD+1                                      
         BCTR  RE,0                SUBTRACT ONE                                 
         BCTR  R8,0                SUBTRACT ONE                                 
*                                                                               
VALLOC08 DS    0H                                                               
         CHI   RE,LENFLOCS-1       LENGTH   > MAX LENGTH W/O EXCLUDE ?          
         BH    VALLOCER            YES,     INVALID LOCATION STATUS             
*                                                                               
VALLOC10 DS    0H                                                               
         CLI   0(R5),EOT           END      OF   TABLE ?                        
         BE    VALLOCER            YES,     INVALID LOCATION STATUS             
*                                                                               
         MVC   LOSTATUS,1(R5)      LOCATION STATUS TO WORK AREA                 
         CLI   LOSTATUS,ESCHIGHQ   LOCATION STATUS A CONSTANT ?                 
         BNL   VALLOC15            YES,     SKIP                                
*                                  GET      LOCATION STATUS VALUE               
         GOTO1 VDICTAT,APPARM,C'SU  ',('LLOSTATU',LOSTATUS),0                   
*                                                                               
VALLOC15 DS    0H                                                               
         EX    R8,VALLOCCL         COMPARE  INPUT TO LOC STATUS VALUE           
         BE    VALLOC20            MATCH,   PROCESS FOUND MATCH                 
*                                                                               
         LA    R5,LLOSTATU+1(,R5)  NEXT     TABLE ENTRY                         
         B     VALLOC10            TRY      AGAIN                               
*                                                                               
VALLOCCL CLC   LOSTATUS(0),APWORK  COMPARE  TABLE ENTRY TO INPUT DATA           
*                                                                               
VALLOC20 DS    0H                                                               
         MVC   SAVELOCS,0(R5)      SAVE     LOCATION STATUS                     
*                                                                               
*                                  RERUN    THE  LOOP SINCE THERE MAY           
*                                           BE   AN   EARLIER ENTRY             
*                                           WITH THE  SAME CODE, E.G.,          
*                                           SEE  AC#LOA AND AC#LEAVE            
*                                                                               
         L     R5,ACLOSTAB         LOCATION STATUS TABLE                        
*                                                                               
VALLOC30 DS    0H                                                               
         CLI   0(R5),EOT           END      OF   TABLE ?                        
         BNE   *+6                 NO,      CONTINUE                            
         DC    H'0'                YES,     ABEND                               
         CLC   SAVELOCS(1),0(R5)   COMPARE  SAVED LOC STATUS TO TABLE           
         BE    VALLOC40            MATCH,   PROCESS FOUND MATCH                 
*                                                                               
         LA    R5,LLOSTATU+1(,R5)  NEXT     TABLE ENTRY                         
         B     VALLOC30            TRY      AGAIN                               
*                                                                               
*                                                                               
VALLOC40 DS    0H                                                               
         MVC   LOSTATUS,1(R5)      LOCATION STATUS TO WORK AREA                 
         CLI   LOSTATUS,ESCHIGHQ   LOCATION STATUS A CONSTANT ?                 
         BNL   VALLOC45            YES,     SKIP                                
*                                  GET      LOCATION STATUS VALUE               
         GOTO1 VDICTAT,APPARM,C'SU  ',('LLOSTATU',LOSTATUS),0                   
*                                                                               
VALLOC45 DS    0H                                                               
*                                  CLEAR    INPUT FIELD                         
         MVC   8(LENFLOCS,R6),SPACES                                            
         LA    RE,FVIFLD           ->       FVIFLD                              
         CLI   FVIFLD,C'*'                                                      
         BNE   *+8                                                              
         LA    RE,FVIFLD+1                                                      
*                                  INSERT   EXPANDED INPUT                      
         MVC   0(LLOSTATU,RE),LOSTATUS                                          
*                                  COPY     TO   SCREEN FIELD                   
         MVC   8(LENFLOCS,R6),FVIFLD                                            
         L     RF,AREQLOC          ->       REQUEST CARD                        
         MVC   0(1,RF),0(R5)       INSERT   LOCATION STATUS CODE                
         CLI   FVIFLD,C'*'         EXCLUDE  REQUESTED ?                         
         BNE   VALLOCX             NO,      EXIT                                
         NI    0(RF),TURNOFF-X'40' YES,     USE  LOWER CASE                     
         B     VALLOCX             EXIT                                         
*                                                                               
VALLOCER DS    0H                  INVALID  LOCATION STATUS                     
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALLOCX                                                          
*                                                                               
VALLOCEX DS    0H                  EXCLUDE  NOT  ALLOWED                        
         MVC   FVMSGNO,=AL2(2091)                                               
         B     VALLOCX                                                          
*                                                                               
VALLOCX  DS    0H                                                               
         B     VALROUTX            EXIT                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  PERSON CODE                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
         USING PERRECD,R8                                                       
VALPRSC  SR    R5,R5                                                            
         TM    REPFIND1,REPFNME                                                 
         BZ    VALPRSC3                                                         
         IC    R5,0(,R6)                                                        
         AR    R5,R6               POINT TO NAME FIELD                          
         SR    R1,R1                                                            
         IC    R1,0(,R5)           LENGTH OF FIELD + HEADERS                    
         AHI   R1,-9                                                            
         TM    1(R5),FVAXTND                                                    
         BZ    *+8                                                              
         AHI   R1,-8                                                            
         STC   R1,BYTE                                                          
         EX    R1,*+4                                                           
         MVC   8(0,R5),SPACES      CLEAR                                        
         OI    6(R5),FVOXMT        TRANSMIT                                     
*                                                                               
VALPRSC3 LA    R8,IOKEY                                                         
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,CUABIN      COMPANY CODE                                 
         MVC   PERKCODE,FVIFLD                                                  
         GOTO1 AIO,IO3+IOACCFIL+IORD                                            
         BNE   ERRUSER             INVALID USER CODE                            
*                                                                               
         GOTO1 GETFFLD,APPARM,REQCARDS                                          
         L     RF,APPARM           SET UP JCL                                   
         MVI   0(RF),ACQPRSN                                                    
         MVC   1(8,RF),FVIFLD                                                   
         LTR   R5,R5               NO PLACE TO BUILD NAME                       
         BZ    VALPRSC9                                                         
         L     R8,AIOAREA3                                                      
         AH    R8,DATADISP                                                      
         MVC   APWORK,SPACES                                                    
         LA    RE,APWORK                                                        
*                                                                               
         USING GPNELD,R8                                                        
VALPRSC5 CLI   0(R8),0                                                          
         BE    VALPRSC8                                                         
         CLI   0(R8),GPNELQ        X'5A'                                        
         BNE   VALPRSC6                                                         
         SR    RF,RF                                                            
         IC    RF,1(,R8)                                                        
         AHI   RF,-(GPNLNQ+1)      GET LENGTH OF NAME                           
         BM    VALPRSC6                                                         
         EX    RF,*+4                                                           
         MVC   0(0,RE),GPNNME                                                   
         LA    RE,1(RE,RF)                                                      
         MVC   0(1,RE),SCCOMMA     SEPORATE LAST, FIRST NAME                    
         LA    RE,1(,RE)                                                        
VALPRSC6 SR    RF,RF                                                            
         IC    RF,1(,R8)                                                        
         AR    R8,RF                                                            
         B     VALPRSC5                                                         
*                                                                               
VALPRSC8 BCTR  RE,0                SUBTRACT ONE                                 
         MVI   0(RE),C' '          BLANK OUT LAST COMMA                         
         LA    RF,APWORK                                                        
         SR    RE,RF                                                            
         BM    VALPRSC8                                                         
         ST    RE,APPARM+4         LENGTH                                       
         GOTO1 VSQUASH,APPARM,APWORK                                            
         L     RF,APPARM+4         LENGTH OF TEXT                               
         AHI   RF,-1                                                            
         BM    VALPRSC9            NO DATA                                      
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         CR    R1,RF               AVAILABLE VS ACTUAL SIZE                     
         BNL   *+6                                                              
         LR    RF,R1                                                            
         EX    RF,*+4                                                           
         MVC   8(0,R5),APWORK      MOVE IN NAME                                 
         OI    PRFFLAG,PRFACC                                                   
                                                                                
VALPRSC9 B     VALROUTX                                                         
         DROP  R2,R8                                                            
         EJECT ,                                                                
***********************************************************************         
*  TRANSACTION TYPE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING ACQD,R3                                                          
VALTRNT  BRAS  RE,CHKSPC                                                        
         BNE   VALTRT20            USER INPUT  SUPPLIED ?                       
         CLI   FVILEN,0            ANY  DATA ?                                  
         BE    VALROUTX            NO,  ASSUME CORRECT                          
         CLI   RECAP#,0            RECAPPING ?                                  
         BE    VALROUTX            NO,  ASSUME CORRECT                          
*                                                                               
VALTRT20 DS    0H                                                               
         L     R4,AREQLOC                                                       
         GOTOR VALTTYP,APPARM,(R6),(R4),RR=APRELO                               
         B     VALROUTX                                                         
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  CLIENT / COSTING ACCOUNT                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            Report field  DSECT                          
         USING RTND,R4             Routine table DSECT                          
VALCLIA  L     R4,ARTND            A(Routine field entry)                       
         MVI   EXCLUDE,NO          Set exclude to no                            
*                                                                               
VALCLI05 SR    R5,R5                                                            
         TM    REPFIND1,REPFNME    IS THERE A NAME (DETAIL) FIELD ?             
         BZ    *+10                                                             
         IC    R5,0(,R6)                                                        
         AR    R5,R6                                                            
         BRAS  RE,CHKSPC                                                        
         CLI   APPARM,1                                                         
         BNL   VALCLI04            DATA                                         
*        MVI   RUNJOB,RUNLONG                                                   
         B     VALROUTX            NO   DATA                                    
                                                                                
VALCLI04 BE    VALCLI10            NOT  A LIST                                  
         CLI   RTNFLDN,FLDNCSTA    COSTING ACCOUNT?                             
         BNE   *+8                                                              
         CLI   APREPNUM,REP#ICST   MATCH ON INCOME COST                         
         BE    ERRIVPUT                                                         
         CLI   APPARM,2            MULTIPLE ITEMS OR LISTS ?                    
         BH    VALCLI40            YES, ASSUME OK FROM PROFILE                  
*                                                                               
         GOTOR VALLST,APPARM,(RTNFLDN,(R6)),(R5),RR=APRELO                      
         CLI   APPARM,0            SINGLE VALID LIST ?                          
         BNE   ERRLIST             NO,  ERROR                                   
         CLI   RUNJOB,RUNSHORT     TEST SET AT LEDGER LVL                       
         BE    *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
         B     VALCLI40            YES, CONTINUE                                
*                                                                               
VALCLI10 DS    0H                  NOT  A LIST                                  
         CLI   RTNFLDN,FLDNCSTA    COSTING ACCOUNT?                             
         BNE   *+8                                                              
         CLI   APREPNUM,REP#ICST   MATCH ON INCOME COST                         
         BE    ERRIVPUT                                                         
         L     RF,=A(LEDGLIST)     FIND LEDGER FOR FIELD                        
         A     RF,APRELO           ADD  RELOCATION FACTOR                       
*                                                                               
VALCLI12 CLI   0(RF),X'FF'         EOT  ?                                       
         BNE   *+6                 NO,  CONTINUE                                
         DC    H'00'               TABLE ENTRY ERROR?                           
         CLC   RTNFLDN,0(RF)                                                    
         BE    *+12                                                             
         LA    RF,3(,RF)                                                        
         B     VALCLI12                                                         
*                                                                               
         SR    R8,R8                                                            
         CLI   RTNFLDN,FLDNVNDR    VENDOR?                                      
         BNE   VALCLI16                                                         
*                                                                               
         LA    RE,FVIFLD                                                        
         CLI   FVIFLD,C'*'                                                      
         BNE   *+8                                                              
         LA    RE,FVIFLD+1         BUMP PAST EXCLUDE CHARACTER                  
*                                                                               
VALCLI14 CLI   0(RF),FLDNVNDR      STILL VENDOR MATCHING?                       
         BNE   ERRLDGR                                                          
         CLC   0(LUNLG,RE),1(RF)   MATCH LEDGER                                 
         BE    VALCLI18                                                         
         LA    RF,3(,RF)                                                        
         B     VALCLI14                                                         
*                                                                               
VALCLI16 DS    0H                                                               
         CLI   RTNFLDN,FLDNPART    PARTICIPANT?                                 
         BE    *+8                                                              
         LA    R8,BLOCK                                                         
*                                                                               
VALCLI18 MVC   BLOCK(LUNLG),1(RF)  UNIT/LEDGER                                  
         LA    R3,FVIFLD           ->   TO THE FIRST BYTE OF THE DATA           
         ZIC   RF,FVILEN           GET  DATA LENGTH                             
         CLI   0(R3),C'*'          EXCLUDE REQUESTED ?                          
         BNE   VALCLI20            NO,  SKIP                                    
         TM    REPFIND1,REPFXLD    EXCLUDE SUPPORTED ?                          
         BZ    ERRXCLUD            NO,  NOT VALID                               
         CLI   FVXLEN,0            ONLY "*" INPUTED ?                           
         BE    ERRIVPUT            YES, INVALID INPUT                           
         LA    R3,1(,R3)           YES, VALIDATE WITHOUT THE "*"                
         BCTR  RF,0                     LENGTH   WITHOUT THE "*"                
         MVI   EXCLUDE,YES         SAVE EXCLUDE INDICATOR                       
*                                                                               
VALCLI20 DS    0H                  NOT  EXCLUDE                                 
         CLM   RF,1,RTNFLDLN       COMPARE TO MAX DATA LENGTH W/O "*"           
         BH    ERRIVPUT            TOO  BIG, INVALID INPUT                      
*                                                                               
*        CLI   RTNFLDN,FLDNCLNT    CLIENT ?                                     
*        BNE   VALCLI25            NO,   SKIP                                   
         CLC   APREPUL,=C'1C'      INCOME/COSTING  OR   P&L                     
         BNE   VALCLI25            NO,   SKIP                                   
         ST    RF,SVRF             SAVE  REGISTER                               
         MVC   SAVEUNL,ACLEDGER    SAVE  U/L  INFORMATION                       
         LA    R1,=C'SJ'                                                        
         GOTO1 GETLEDG,(R1)        FIND  SIZE OF   LVL  1                       
         CLC   SVRF+3(1),ACLEV1    COMPARE    SIZE TO   MAX  DATA LEN           
         BH    ERRIVPUT            TOO   BIG, INVALID   INPUT                   
         GOTO1 GETLEDG,SAVEUNL     RESTORE    UNIT/LEDGER                       
         L     RF,SVRF             RESTORE    REGISTER                          
*                                                                               
*                                                                               
VALCLI25 DS    0H                                                               
*                                                                               
         TM    REPFIND2,REPFWLD    SUPPORT FOR WILD CARD?                       
         BZ    VALCLI30            NO                                           
*                                                                               
         GOTOR WILDCARD,APPARM,(R3),(RF),RR=APRELO                              
*                                  ANY  WILDCARD CHARACTERS ?                   
         BM    ERRIVPUT            BAD, INVALID INPUT                           
         BZ    VALCLI30            NO,VALIDATE                                  
         CLI   0(R3),C'?'          COULD IT BE A LONG-RUNNING JOB?              
         BNE   VALCLI40            NO, SKIP VALIDATION                          
         CLI   RUNJOB,RUNSHORT     TEST SET AT LEDGER LVL                       
         BE    *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
         B     VALCLI40            SKIP VALIDATION                              
*                                                                               
*                                                                               
         USING ACTRECD,R2                                                       
VALCLI30 DS    0H                                                               
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         LTR   R8,R8               IS   THE UNIT/LEDGER IN THE FIELD            
         BZ    VALCLI33            YES, JUST MOVE THE WHOLE DATA                
         MVC   ACTKUNT(LUNLG),BLOCK     UNIT LEDGER                             
         MVC   ACTKACT,0(R3)            ACCOUNT                                 
         B     VALCLI35            GO   AND GET THE DESIRED RECORD              
*                                                                               
VALCLI33 DS    0H                  UNIT/LEDGER IN THE FIELD                     
         CLC   0(1,R3),BLOCK       VALIDATE UNIT ONLY                           
         BNE   ERRIVPUT            BAD,  INVALID INPUT                          
         MVC   ACTKULA,0(R3) MOVE UNIT/LEDGER + ACCOUNT                         
*                                                                               
VALCLI35 DS    0H                  GET  THE DESIRED RECORD                      
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   ERRIVPUT            INVALID INPUT                                
         DROP  R2                                                               
*                                                                               
         GOTO1 GETNAME,APPARM,AIOAREA2,(R5)                                     
         OI    6(R5),FVOXMT        RETRANSMIT                                   
*                                                                               
VALCLI40 LA    RF,REQCARDS                                                      
*                                                                               
         GOTO1 GETFFLD,APPARM,(RF)                                              
         L     RF,APPARM                                                        
         MVI   0(RF),ACQANAL       ANALYSIS                                     
         CLI   8(R6),C'('                                                       
         BNE   VALCLI50                                                         
         MVC   1(LULACNT,RF),FVIFLD                                             
         CLI   RECAP#,0            ANY  RECAP FORMATS ?                         
         BE    VALROUTX            NO,  EXIT                                    
         MVC   14(1,RF),RTNFLDN    PASS THE   FIELD NUMBER                      
         B     VALROUTX            AND  EXIT                                    
*                                                                               
VALCLI50 CLI   8(R6),C'+'          WAS IT A +LIST                               
         BE    *+8                                                              
         CLI   8(R6),C'-'          WAS IT A -LIST                               
         BNE   VALCLI60                                                         
*                                  INSERT LIST NAME WITH +/-                    
         MVC   1(LLIST+1,RF),FVIFLD                                             
         B     VALROUTX                                                         
*                                                                               
VALCLI60 DS    0H                                                               
         LA    R8,FVIFLD           DATA AREA                                    
         CLI   EXCLUDE,YES         EXCLUDE REQUESTED ?                          
         BNE   *+8                 NO,  SKIP                                    
         LA    R8,1(,R8)           YES, DO NOT OUTPUT THE "*"                   
         MVC   1(LUNLG,RF),BLOCK   UNIT/LEDGER                                  
*                                  INSERT THE FIELD W/O UL                      
         MVC   1+LUNLG(L'ACQFLT1-2,RF),0(R8)                                    
         CLI   RTNFLDN,FLDNVNDR                                                 
         BE    *+8                                                              
         CLI   RTNFLDN,FLDNPART    PARTICIPANT?                                 
         BNE   *+10                                                             
*                                  INSERT THE FIELD W/  UL                      
         MVC   1(L'ACQFLT1,RF),0(R8)                                            
         OC    1(L'ACQFLT1,RF),SPACES                                           
         CLI   EXCLUDE,YES         EXCLUDE REQUESTED ?                          
         BNE   VALROUTX            NO,  EXIT                                    
         NI    1(RF),TURNOFF-X'40' YES, MAKE LOWER CASE LETTER                  
         B     VALROUTX                                                         
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE BUDGET NAME/CODE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
         USING BUDRECD,R8                                                       
VALBUDG  CLI   FVILEN,0                                                         
         BE    VALROUTX                                                         
         LA    R8,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN      COMPANY CODE                                 
*                                                                               
         TM    FVIIND,FVINUM       IS IT NUMERIC                                
         BZ    VALBUD20                                                         
         CLI   FVILEN,LBUDG#       IS   LENGTH > 5                              
         BH    ERRBUDG             YES, INVALID BUDGET                          
         ZIC   RF,FVXLEN           GET  LENGTH - 1                              
         EX    RF,VALBUDPK         PACK INTO APDUB                              
         CVB   RF,APDUB            CONVERT TO BINARY                            
         C     RF,=A(32767)        >    MAX HALFWORD NUMBER ?                   
         BH    ERRBUDG             YES, INVALID BUDGET                          
         STH   RF,APHALF           SAVE BUDGET NUMBER                           
         MVC   BUDKNO1,APHALF      INSERT INTO RECORD                           
         GOTO1 AIO,IO3+IOACCDIR+IOHI                                            
         CLC   BUDKNO1,APHALF      BUDKNO CHANGED ?                             
         BNE   ERRBUDG             YES, INVALID BUDGET                          
         SR    RF,RF                                                            
         IC    RF,0(,R6)                                                        
         AHI   RF,-9                                                            
         TM    1(R6),FVAXTND                                                    
         BZ    *+8                                                              
         AHI   RF,-8                                                            
         EX    RF,*+4                                                           
         MVC   8(0,R6),BUDKCOD   MOVE IN NAME                                   
         B     VALBUD80                                                         
*                                                                               
VALBUD20 MVC   BUDKCOD,FVIFLD                                                   
         GOTO1 AIO,IO3+IOACCDIR+IOHI                                            
         CLC   BUDKCOD,FVIFLD                                                   
         BNE   ERRBUDG                                                          
         MVC   APHALF,BUDKNO2                                                   
*                                                                               
VALBUD80 SR    RF,RF                                                            
         ICM   RF,3,REPFDDSP                                                    
         LR    R1,RF                                                            
         AHI   R1,-(ACQAPPL-ACQD)                                               
         LA    RE,BUDLIST(R1)                                                   
         CLC   APHALF+1(1),0(RE)                                                
         BE    VALBUDX                                                          
         LH    R1,APHALF                                                        
         CHI   R1,255                                                           
         BH    ERRBUDG                                                          
         AHI   R1,ESCHIGHQ         FIX FOR RFP (MUST BE > X'30')                
         AR    RF,R3                                                            
         STC   R1,0(,RF)                                                        
VALBUDX  B     VALROUTX                                                         
*                                                                               
VALBUDPK PACK  APDUB,FVIFLD(0)     PACK FIELD INTO APDUB                        
         DROP  R2,R8                                                            
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE OPTIONS                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
VALOPTS  CLI   FVILEN,0                                                         
         BE    VALOPSX                                                          
         LA    RF,OPT1#            OPTION 1                                     
         LA    RE,OPT1LST                                                       
         CLI   REPFFLDN,FLDNOPT1                                                
         BE    VALOPS10                                                         
         LA    RF,OPT3#            OPTION 3                                     
         LA    RE,OPT3LST                                                       
         CLI   REPFFLDN,FLDNOPT3                                                
         BE    VALOPS10                                                         
         LA    RF,OPT4#            OPTION 4                                     
         LA    RE,OPT4LST                                                       
         CLI   REPFFLDN,FLDNOPT6   OPTION 6                                     
         BNE   VALOPS10                                                         
         CLI   APGFLAG,YES                                                      
         BNE   VALOPSX                                                          
         CLI   FVIFLD,C'N'                                                      
         BE    VALOPS20                                                         
         CLI   FVIFLD,C'S'                                                      
         BE    VALOPS20                                                         
         B     ERRIVPUT            MUST BE A 'S' OR 'N'                         
*                                                                               
VALOPS10 SR    R1,R1                                                            
         ICM   R1,1,0(RF)                                                       
         BZ    ERRIVPUT            NO INPUT REQUIRED                            
VALOPS12 CLC   0(1,RE),FVIFLD                                                   
         BE    VALOPS20                                                         
         LA    RE,1(RE)            TRY NEXT OPTION                              
         BCT   R1,VALOPS12                                                      
         B     ERRIVPUT            NO INPUT REQUIRED                            
*                                                                               
VALOPS20 L     RE,AREQLOC                                                       
         MVC   0(1,RE),FVIFLD      MOVE IN OPTION                               
VALOPSX  B     VALROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE CURRENCY AND OVERHEAD AMOUNTS                             *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
VAL2DEC  SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    VALDECX                                                          
         GOTO1 VCASHVAL,APPARM,(2,FVIFLD),(RF)                                  
         CLI   APPARM,0                                                         
         BNE   ERRIVPUT                                                         
         L     RE,AREQLOC                                                       
         CLI   REPFFLDN,FLDNCURC   CURRENCY NUMBER                              
         BNE   *+8                                                              
         LA    R5,2                                                             
         CLI   REPFFLDN,FLDNOVHD   OVERHEAD                                     
         BNE   VALDEC05                                                         
         GOTO1 GETFFLD,APPARM,(R3) FIND 1ST  AVAILABLE FIELD                    
         L     RE,APPARM           ->   FIELD                                   
         MVI   0(RE),ACQOVHD       SAY  OVERHEAD                                
*                                  ->   DATA AREA FOR  FIELD                    
         LA    RE,ACQFLT1-ACQTYP1(,RE)                                          
         LA    R5,4                                                             
*                                                                               
VALDEC05 EX    R5,*+4                                                           
         MVC   0(0,RE),=C'00000'   PAD WITH ZEROS                               
         AR    RE,R5               POINT TO END                                 
         SR    RF,RF                                                            
         XC    WORK,WORK           CLEAR WORK AREA                              
         IC    RF,FVXLEN           MAKE  SURE WE   DO   NOT  PICK UP            
         EX    RF,*+4                    A    RANDOM    C'0'                    
         MVC   WORK+2(0),FVIFLD                                                 
         IC    RF,FVILEN                                                        
         LA    R0,2                                                             
         LA    R1,WORK+2-3(RF)     ->    POSSIBLE  DECIMAL   POINT              
*                                                                               
VALDEC10 CLI   0(R1),C'.'          ARE   THERE 2,1 OR NO DECIMAL PLACES         
         BE    VALDEC20                                                         
         LA    R1,1(,R1)                                                        
         BCTR  RE,0                                                             
         BCT   R0,VALDEC10                                                      
         B     *+8                 SKIP NEXT INSTRUCTION, NO DECIMAL            
*                                                                               
VALDEC20 AHI   R0,1                ADD ONE FOR DECIMAL                          
         LA    R1,FVIFLD-1(RF)     POINT TO END                                 
         BCTR  R5,0                NUMBER OF NONE DEC ALLOWED                   
         SR    RF,R0               NUMBER OF NONE DEC #'S                       
         CR    RF,R5               TOO BIG A NUMBER?                            
         BH    ERRIVPUT            YES                                          
*                                                                               
         IC    R0,FVILEN           MOVE IN DATA                                 
         MVI   BYTE,NO             NO   DECIMAL   POINT                         
*                                                                               
VALDEC25 CLI   0(R1),C'.'          DECIMAL   POINT     ?                        
         BE    VALDEC30            YES, TEST FOR  MORE THAN ONE                 
*                                  NO,  TEST FOR  NUMERIC                       
         CLI   0(R1),C'0'          >=   ZERO ?                                  
         BL    ERRIVPUT            NO,  INVALID   INPUT                         
         CLI   0(R1),C'9'          <=   NINE ?                                  
         BH    ERRIVPUT            NO,  INVALID   INPUT                         
         MVC   0(1,RE),0(R1)       MOVE NUMBER    INTO FIELD                    
         BCTR  RE,0                PREVIOUS  CHARACTER FOR  INSERTION           
         B     VALDEC40            PROCESS   IT                                 
*                                                                               
VALDEC30 DS    0H                  FOUND     C'.'                               
         CLI   BYTE,NO             PREVIOUS  C'.' ?                             
         BNE   ERRIVPUT            YES, INVALID   INPUT                         
         MVI   BYTE,YES            SAY  FOUND     C'.'                          
*                                                                               
VALDEC40 DS    0H                                                               
         BCTR  R1,0                                                             
         BCT   R0,VALDEC25                                                      
*                                                                               
VALDECX  B     VALROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE NARRATIVE (COMMENT) RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         USING SCMRECD,R1                                                       
VALNARR  LA    R1,IOKEY                                                         
         MVC   APWORK,SPACES                                                    
         SR    R0,R0                                                            
         ICM   R0,1,FVILEN         LENGTH                                       
         BZ    VALNARX                                                          
         LA    RF,6                                                             
         SR    RF,R0               CALCULATE NUMBER OF CHAR TO INDENT           
         LA    RE,APWORK(RF)       POINT IN TO RIGHT JUSTIFY                    
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   0(0,RE),FVIFLD                                                   
         XC    SCMKEY,SCMKEY                                                    
         MVI   SCMKTYP,SCMKTYPQ    X'0C' STANDARD COMMENT                       
         MVC   SCMKCPY,CUABIN      COMPANY CODE                                 
         MVC   SCMKCODE,APWORK                                                  
         GOTO1 AIO,IO3+IOACCDIR+IORD                                            
         BNE   ERRIVPUT                                                         
         L     RE,AREQLOC                                                       
         MVC   0(6,RE),APWORK                                                   
VALNARX  B     VALROUTX                                                         
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE METHOD RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT REQUEST SCREEN                        
VALMTHD  CLI   APGFLAG,YES                                                      
         BE    VALMHD10                                                         
         CLI   FVILEN,0                                                         
         BNE   VALMHD10                                                         
         BRAS  RE,CHKRFILL         SHOULD WE REFILL ?                           
         BNE   VALMHDX             NO,    SKIP                                  
         L     RF,=A(GETMTHD)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   VALMHDX             NO METHOD SUPPLIED                           
         MVI   5(R6),1             INPUT LENGTH                                 
         MVC   8(1,R6),SAVEMTHD                                                 
         GOTO1 AFVAL,(R6)                                                       
*                                                                               
         USING CAHRECD,R1                                                       
VALMHD10 LA    R1,IOKEY                                                         
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    X'3E'                                        
         MVC   CAHKCPY,CUABIN      COMPANY                                      
         CLI   FVILEN,1                                                         
         BL    VALMHDX             NO INPUT                                     
         BH    VALMHD20            CAN'T BE A NUMBER THEN                       
         TM    FVIIND,FVINUM       IS IT A NUMBER?                              
         BZ    VALMHD20            NO                                           
         CLI   FVIFLD,C'1'                                                      
         BL    ERRIVPUT                                                         
         MVI   CAHKSUB,CAHKSUBQ    X'01'                                        
         MVC   CAHKMTHD,FVIFLD                                                  
         XC    CAHKOFC,CAHKOFC                                                  
         B     VALMHD30                                                         
*                                                                               
         USING CMTRECD,R1                                                       
VALMHD20 DS    0H                                                               
         CLI   FVILEN,L'CMTKMTHD   MORE THAN 3 CHARACTERS ?                     
         BH    ERRIVPUT            YES, INVALID INPUT                           
         MVI   CMTKSUB,CMTKSUBQ    X'02'                                        
         MVC   CMTKMTHD,FVIFLD                                                  
*                                                                               
VALMHD30 GOTO1 AIO,IO3+IOACCFIL+IORD                                            
         BNE   ERRIVPUT                                                         
*                                                                               
         USING METELD,R1                                                        
         L     R1,AIOAREA3                                                      
         MVI   APELCODE,METELQ     X'82' METHOD ELEMENT                         
         GOTO1 GETEL                                                            
         BNE   ERRIVPUT                                                         
         L     RE,AREQLOC          REQUEST CARD LOCATION                        
         MVC   0(1,RE),METNUM                                                   
         MVC   8(3,R6),METCODE                                                  
*                                                                               
VALMHDX  B     VALROUTX                                                         
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  GENERAL VALIDATION OF FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            Report  field DSECT                          
         USING RTND,R4             Routine field DSECT                          
VGENERAL DS    0H                                                               
         L     R4,ARTND            Table entry                                  
         GOTO1 AFVAL,(R6)          Check for input. Set FVIFLD info             
         BNE   VALGENX             None                                         
         LA    RF,FVIFLD           RF = data                                    
         SR    R1,R1                                                            
         IC    R1,FVILEN           R1 = input data length                       
         CHI   R1,1                                                             
         BE    VALGEN30            Can't be options below                       
         CLI   FVIFLD,C'('         Profile indicated ?                          
         BE    VALGEN50            Yes                                          
         CLI   FVIFLD,C'+'         +LIST   indicated ?                          
         BE    VALGEN30            Yes                                          
         CLI   FVIFLD,C'-'         -LIST   indicated ?                          
         BE    VALGEN30            Yes                                          
         CLI   FVIFLD,C'*'         Exclude data option ?                        
         BNE   VALGEN30            No, skip                                     
         CLI   FVILEN,2            Length < 2 ?                                 
         BL    VALGENN             Yes, invalid                                 
******************************************************                          
*  Check workcode since it can start with an '*'     *                          
******************************************************                          
         CLI   REPFFLDN,FLDNWKCD   Is this workcode field ?                     
         BNE   VALGEN20            No, process exclude data option              
         CLI   FVILEN,2            Yes, check length                            
         BE    VALGEN30            Okay if length is 2                          
         CLI   FVIFLD,C'*'         High, means it must have exclude             
         BNE   VALGENN             Not exclude so invalid                       
*                                                                               
VALGEN20 TM    REPFIND1,REPFXLD    Does field support exclude ?                 
         BZ    VALGENER                 No, error                               
         NI    FVIFLD+1,TURNOFF-X'40'   Yes, set indication exclude             
         LA    RF,FVIFLD+1         RF = Data beyound exclude character          
         BCTR  R1,0                R1 = Len. of data, adjusted for "*"          
*                                                                               
VALGEN30 CLM   R1,1,RTNFLDLN       Check max length w/o exclude                 
         BH    VALGENN             High, error                                  
         SHI   R1,1                Adjust for EX                                
         BM    VALGENN             Invalid value for EX                         
         L     RE,AREQLOC          Move data into request card location         
         EX    R1,*+4                                                           
         MVC   0(0,RE),0(RF)                                                    
         B     VALGENX                                                          
*                                                                               
* If recaping and a recap uses it own profile then ???                          
*                                                                               
VALGEN50 CLI   RECAP#,0            Recapping ?                                  
         BE    VALGENX             No, leave request card blank                 
         TM    RCAPSW,RCAPPROF     Found use repcap's profile ?                 
         BZ    VALGENX             No, leave request card blank                 
         L     RF,AREQLOC          Request card location for data               
                                                                                
         GOTOR GDEFTEXT,APPARM,(RF),RR=APRELO                                   
         B     VALGENX                                                          
*                                                                               
VALGENER MVC   FVMSGNO,=AL2(2091)     Exclude not allowed                       
         B     VALGENX                                                          
*                                                                               
VALGENN  MVC   FVMSGNO,=AL2(FVFNOTV)  Not valid input                           
*                                                                               
VALGENX  B     VALROUTX                                                         
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* DISKEY                                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
         USING TWAD,R5                                                          
DISKEY   MVC   IOKEY,APRECKEY                                                   
         MVC   SVSELACT,SCACTN     SAVE SELECT ACTION                           
         LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
*MN      ICM   RF,1,RESKFORM+L'RESKFORM         REQUEST NUMBER - 1              
         ICM   RF,1,RESKFORM+L'RESKFORM+1       REQUEST NUMBER - 1              
         BCTR  RF,0                                                             
         STC   RF,BYTE                                                          
         MVC   REQFMT,SPACES                                                    
*                                                                               
*MN      MVI   RESKFORM+L'RESKFORM,C' '         NEED FORMAT'S KEY               
         MVC   RESKFORM+L'RESKFORM(2),SPACES    NEED FORMAT'S KEY               
         MVC   REQFMT,RESKFORM                                                  
         CLI   RESKSUB,RESKSUBQ    X'02' OR X'07' RECORD?                       
         BE    *+10                                                             
*                                                                               
         USING APGRECD,R2                                                       
         MVC   REQFMT,APGKFMT                                                   
         TM    TWASWPST,TWASWAP        DID I COME FROM SWAPPING                 
         BNZ   *+8                                                              
         MVI   SVRECORD,0          NO SO CLEAR SAVE RECORD                      
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
* DISREQ                                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
         USING TWAD,R5                                                          
DISREQ   MVI   RPTIND,0                                                         
         MVI   SVFRPT,C' '         CLEAR FORMAT DESIGN REPORT                   
         TM    TWAMODE,TWAMLSM                                                  
         BZ    DR010                                                            
         CLI   TWALREC,RECRPT      LAST RECORD = "REPORT" ?                     
         BNE   DR010                                                            
         MVC   SVFRPT,RQFRPT       SAVE FORMAT DESIGN REPORT                    
         CLC   RQBLOCK(RQSTR-RQBLOCK),SPACES                                    
         BH    DR005               NO DATA TO OVER-RIDE                         
         OC    RQSTR(RQCURCY-RQSTR),RQSTR                                       
         BNZ   DR005                                                            
*&&UK*&& CLI   RQCURCY,ACQC1ST     DID THEY OVER CURRENCY ?                     
*&&UK*&& BH    DR005               YES                                          
         CLC   RQNAME,SPACES       WAS USER NAME INPUT FROM ACSCR07 ?           
         BNH   DR010               NO                                           
*                                                                               
DR005    OI    RPTIND,RPTFILL      SET TO INDATCATE FILL FROM ACSCR07           
         CLC   RQOUTP,SPACES                                                    
         BNH   *+10                                                             
         MVC   REQOTYP(6),RQOUTP                                                
         CLC   RQDEST,SPACES                                                    
         BNH   *+10                                                             
         MVC   REQDES(6),RQDEST                                                 
         CLC   RQNAME,SPACES       WAS USER NAME INPUT FROM ACSCR07 ?           
         BNH   DR010               NO                                           
         MVC   REQPER,RQNAME                                                    
*&&UK                                                                           
         TM    GENIND,GENEURO                                                   
         BZ    *+10                                                             
         MVC   REQRCU,RQCURCY      CURRENCY #                                   
*&&                                                                             
*                                                                               
DR010    CLC   REQPER,SPACES                                                    
         BH    *+10                                                             
         MVC   REQPER(L'TWAPERSN),TWAPERSN                                      
         OI    REQPERH+6,FVOXMT                                                 
*&&UK                                                                           
         TM    GENIND,GENEURO                                                   
         BZ    DR020                                                            
         CLI   REQRCU,C' '                                                      
         BH    *+8                                                              
         MVI   REQRCU,ACQC1ST      DEFAULT TO C'1'                              
         OI    REQRCUH+6,FVOXMT                                                 
*&&                                                                             
DR020    LA    R2,REQRUNH          FORCE CURSOR TO PRINT LINE                   
         ST    R2,APCURSOR                                                      
*&&US*&& MVI   REQOPT2,C'L'                                                     
*&&US*&& OI    REQOPT2H+6,FVOXMT                                                
         L     R3,AIOAREA3         CLEAR REQUEST AREA                           
         MVI   0(R3),C' '                                                       
         MVC   1(254,R3),0(R3)     FILL W/ SPACES                               
*                                                                               
         USING RECAPD,R3           Recap table                                  
         SR    R6,R6                                                            
         ICM   R6,1,RECAP#         Number of recaps                             
         BZ    DR024                                                            
         LA    R3,RCAPFMTS         A(Saved recap codes)                         
DR021    GOTOR VALFRMT,APPARM,('IO2',RECAPFMT)                                  
         LA    R3,RECAPLNQ(,R3)                                                 
         BCT   R6,DR021                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         DROP  R3                                                               
*                                                                               
DR024    CLI   APREPJCL,REPJCL1    MANPOWER (PERSON) ?                          
         BNE   DRQ21                                                            
         SR    R4,R4                                                            
         ICM   R4,3,TWAOFFLD       Displacement to office field                 
         AR    R4,R5               Add base of screen                           
         SR    RF,RF                                                            
         IC    RF,0(,R4)                                                        
         AHI   RF,-9                                                            
         EX    RF,*+4                                                           
         MVC   8(0,R4),SPACES                                                   
         EX    RF,*+4                                                           
         MVC   8(0,R4),=CL17'Person office'                                     
         STC   RF,BYTE                                                          
*                                                                               
         USING RPFELD,R1                                                        
DRQ21    L     R2,AIOAREA1                                                      
         MVI   APELCODE,RPFELQ     PROFILE ELEMENT X'C4'                        
         GOTO1 GETEL,(R2)                                                       
         BNE   DRQ40                                                            
*&&US*&& TM    RPFPOPT,RPFPORT     USE PORTRAIT?                                
*&&US*&& BZ    *+8                                                              
*&&US*&& MVI   REQOPT2,C'P'                                                     
*                                                                               
DRQ21C   CLI   APREPJCL,REPJCL1    MANPOWER (PERSON) ?                          
         BNE   DRQ21E                                                           
         SR    RF,RF                                                            
         IC    RF,BYTE                                                          
         CLI   RPFCLIOF,C'C'              (CONTRA SECURITY)                     
         BNE   DRQ21D                                                           
         EX    RF,*+4                                                           
         MVC   8(0,R4),=CL17'Contra office'                                     
*                                                                               
DRQ21D   CLI   RPFCLIOF,C'B'              (BOTH PERSON OR CONTRA)               
         BNE   DRQ21E                                                           
         EX    RF,*+4                                                           
         MVC   8(0,R4),=CL17'1R or 1C office'                                   
*                                                                               
DRQ21E   CLI   RECAP#,0            Recapping ?                                  
         BNE   *+10                                                             
         MVC   SVRECON,APYES       INCLUDE RECONCILED ITEMS                     
         CLI   APREPJCL,REPJCLB    BANK (CASH) ?                                
         BNE   DRQ22                                                            
         TM    RPFOPT5,RPFXBRI                                                  
         BZ    *+10                                                             
         MVC   SVRECON,APNO        EXCLUDE RECONCILED                           
         TM    RPFOPT5,RPFOBRI                                                  
         BZ    *+10                                                             
         MVC   SVRECON,APONLY      ONLY    RECONCILED                           
*                                                                               
DRQ22    DS    0H                                                               
*&&US*&& TM    RPFXMIT,RPFXFTP+RPFXDSN+RPFXFIL                                  
*&&UK*&& TM    RPFXMIT,RPFXFTP+RPFXDSN+RPFXFIL+RPFXBDE+RPFXUSS                  
         BNZ   DRQ39                                                            
         TM    RPFXMIT2,RPFXEDI                                                 
         BNZ   DRQ39                                                            
         TM    RPFDNOPT,RPFDDOWN   DOWNLOAD REPORT ONLY                         
         BZ    DRQ40                                                            
*                                                                               
DRQ39    OI    PRFFLAG,PRFDOWN     FORCE DOWNLOAD OPTION                        
*                                                                               
DRQ40    CLC   REQOTYP,SPACES                                                   
         BH    DRQ42               INPUT SO CHECK IT                            
         TM    PRFFLAG,PRFDOWN     NO INPUT, SHOULD WE FORCE DOWN ?             
         BZ    DRQ44               DO NOTHING                                   
         MVC   REQOTYP(6),AC@DOWN                                               
         OI    REQOTYPH+6,FVOXMT   TRANSMIT                                     
         B     DRQ44                                                            
*                                                                               
DRQ42    CLI   CHGKEY,YES                                                       
         BNE   DRQ44               LEAVE AS IS                                  
         MVC   REQOTYP,SPACES                                                   
         OI    REQOTYPH+6,FVOXMT   TRANSMIT                                     
         DROP  R1                                                               
*                                                                               
DRQ44    CLI   REQFMT,C'@'                                                      
         BNE   DRQ45                                                            
         MVC   REQOTYP(6),REQFMT                                                
         OI    REQOTYPH+6,FVOXMT                                                
*                                                                               
DRQ45    GOTO1 GETNAME,APPARM,AIOAREA1,REQFMTNH                                 
*                                                                               
DRQ55    L     R2,ASCRTAB                                                       
         LA    R6,REQFMTNH         LAST FIELD ON PANGEN SCREEN                  
*                                                                               
         USING REPFD,R2            REPORT FIELD DSECT                           
         BRAS  RE,NEXTUNPT         R6=NEXT UNPROTECTED FIELD                    
DRQ80    CLI   0(R2),X'FF'         EOT?                                         
         BE    DRQ98               NO MORE SCREEN FIELDS, LEAVE                 
         TM    REPFIND1,REPFSTO    STEREO FIELD ONLY                            
         BZ    *+12                NO, SO DON'T CARE                            
         TM    GENIND,GENSTRO      YES, SO STEREO MODE                          
         BZ    DRQ90               NO                                           
*                                                                               
         USING RTND,R4             ROUTINES TABLE DSECT                         
         L     R4,=A(ROUTINES)                                                  
         A     R4,APRELO                                                        
*                                                                               
DRQ85    CLI   0(R4),X'FF'         EOT?                                         
         BE    DRQ90                                                            
         CLC   REPFFLDN,RTNFLDN    MATCH ON FIELD NUMBER?                       
         BE    DRQ88                                                            
         LA    R4,RTNLNQ(R4)       BUMP TO CHECK NEXT ONE                       
         B     DRQ85                                                            
*                                                                               
DRQ88    DS    0H                                                               
         ST    R4,ARTND            SAVE ROUTINE TABLE ENTRY ADDRESS             
         L     RF,=A(FLDCNTL)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   DRQ90                                                            
         LA    RF,DISROUT                                                       
         ICM   RF,8,RTNDDISP       GET ROUTINE DISPLAY #                        
         BASR  RE,RF               BRANCH TO DISPLAY ROUTINE                    
*                                                                               
         BRAS  RE,NEXTUNPT         R6=NEXT UNPROTECTED FIELD                    
*                                                                               
DRQ90    LA    R2,REPFLNQ(R2)      BUMP TO NEXT FIELD ON SCREEN                 
         B     DRQ80                                                            
*                                                                               
DRQ98    SR    RE,RE               SET CONCODE TO YES                           
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
         USING RTND,R4                                                          
DISROUT  NTR1  ,                                                                
         MVI   FVILEN,0                                                         
         MVC   FVIFLD,SPACES                                                    
         ST    RF,SVRF                                                          
         OI    6(R6),FVOXMT        RETRANSMIT                                   
         ST    R4,ARTND            SAVE ROUTINE TABLE ENTRY ADDRESS             
         CLI   RTNFLDN,FLDNUNLG    Unit/ledger field ?                          
         BNE   DISROUT2                                                         
         CLI   APREPJCL,REPJCL1    PERSON     (1R) ?                            
         BE    DISROUT4                                                         
         CLI   APREPJCL,REPJCLV    PRODUCTION (SJ) ?                            
         BE    DISROUT4                                                         
*                                                                               
DISROUT2 BRAS  RE,CHKRFILL         SHOULD WE REFILL ?                           
         BNE   DISROUTX            NO,    SKIP                                  
*                                                                               
DISROUT4 L     RF,SVRF                                                          
         SRL   RF,24               MOVE   ROUTINE # TO LOB                      
         SLL   RF,2                TIMES  4                                     
         LTR   RF,RF                                                            
         BZ    DISROUTX            NO     ROUTINE                               
         B     *(RF)                                                            
*                                                                               
         B     DISULAC              1) UNIT/LEDGER/ACCOUNT                      
         B     DISGENC              2) GENERAL/GENERIC                          
         B     DISBUDG              3) BUDGETS     (APG)                        
         B     DISOPTS              4) OPTIONS     (APG)                        
         B     DISSELT              5) SELECT INFO (APG)                        
         B     DISESTS              6) ESTIMATE STATUS                          
         B     DISACCT              7) ACCOUNTS/CONTRA                          
         B     DISFLTR              8) FILTERS 1-5                              
         B     DISRECAP             9) DISPLAY RECAP FIELD                      
         B     DISMTHD             10) METHOD                                   
         B     DISDATE             11) DATES FROM REPORT LIST FIELDS            
         B     DISLSTA             12) LOCATION STATUS                          
         B     DISRCON             13) RECONCILED                               
         B     DISFRPT             14) FORMAT REPORT                            
*                                                                               
DISROUTX MVC   FVMSGNO,=AL2(FVFOK) IGNORE ERRORS                                
         B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY UNIT/LEDGER/ACCOUNT DATA                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
         USING RTND,R4                                                          
DISULAC  DS    0H                                                               
         L     R4,ARTND            Routnine table entry                         
         LR    R8,R6                                                            
         CLI   RTNFLDN,FLDNUNLG    Unit/ledger field ?                          
         BNE   DISULA20            No                                           
         S     R8,ATWA             Displacment to field in TWA                  
         ST    R8,AFLDUNLG         Save off so we can use later                 
         TM    RPTIND,RPTFILL      Fill in from report screen                   
         BZ    DISULA08                                                         
         MVC   8(2,R6),RQUL        Fill in from Report List Screen              
         MVC   RQUL,SPACES                                                      
*                                                                               
DISULA08 CLI   APREPJCL,REPJCL1    PERSON     (1R) ?                            
         BNE   *+10                                                             
         MVC   8(2,R6),=C'1R'      Always the case                              
         CLI   APREPJCL,REPJCLV    PRODUCTION (SJ) ?                            
         BNE   *+10                                                             
         MVC   8(2,R6),=C'SJ'      Always the case                              
         GOTO1 AFVAL,(R6)          RESET                                        
*                                                                               
         MVC   SAVEUNL,SPACES                                                   
         CLI   APGFLAG,YES                                                      
         BNE   DISULA10                                                         
         BRAS  RE,ADDAPGUL         ADD APG U/L ELEMENT TO RECORD                
*                                                                               
DISULA10 MVI   MULTIUL,NO                                                       
         BRAS  RE,CHKSPC                                                        
         CLI   APPARM,3            MULI-LEDGERS USED?                           
         BNE   *+8                                                              
         MVI   MULTIUL,YES         YES MULTIPLE UNIT LEDGERS USED               
*                                                                               
DISULA20 L     R8,AFLDUNLG         DISPLACEMENT TO U/L FIELD                    
         A     R8,ATWA             ADD BASE OF SCREEN                           
*                                                                               
         CLI   RTNFLDN,FLDNACCT    ACCOUNT ?                                    
         BNE   DISULA30                                                         
         TM    RPTIND,RPTFILL                                                   
         BZ    DISULA30                                                         
         LA    RE,8(,R6)           POINT TO INPUT FIELD                         
         TM    RQACC,X'40'         WAS IT EXCLUDE ?                             
         BO    DISULA24            NO                                           
         MVI   0(RE),C'*'                                                       
         LA    RE,1(,RE)                                                        
         OI    RQACC,X'40'                                                      
*                                                                               
DISULA24 MVC   0(L'RQACC,RE),RQACC     FILL IN ACCOUNT                          
         MVC   RQACC,SPACES                                                     
         GOTO1 AFVAL,(R6)          RESET                                        
*                                                                               
DISULA30 BRAS  RE,CHKSPC                                                        
         CLI   APPARM,1            SINGLE ACCOUNT?                              
         BNE   DISROUTX            NO ,SO EXIT                                  
*                                                                               
DISULA80 MVC   IOKEY,SPACES                                                     
         MVC   IOKEY(1),CUABIN     COMPANY CODE                                 
         CLI   RTNFLDN,FLDNUNLG    UNIT/LEDGER?                                 
         BNE   DISULA86                                                         
         MVC   SAVEUNL,8(R6)                                                    
         MVC   IOKEY+1(2),8(R6)                                                 
         B     DISULA90                                                         
*                                                                               
DISULA86 MVC   IOKEY+1(2),SAVEUNL  MOVE IN U/L                                  
         CLC   SAVEUNL,8(R8)       DID IT CHANGE?                               
         BE    DISULA87                                                         
         MVC   IOKEY+1(2),8(R8)             GET NEW U/L                         
         GOTO1 AIO,IORD+IOACCFIL+IO2        REFRESH U/L NAME                    
         BNE   DISULA87                                                         
         SR    RF,RF                                                            
         IC    RF,0(,R8)                                                        
         AR    RF,R8                                                            
         OI    6(RF),FVOXMT                 TRANSMIT                            
         GOTO1 GETNAME,APPARM,AIOAREA2,(RF)                                     
*                                                                               
DISULA87 DS    0H                                                               
         LA    R8,FVIFLD                    GET  ADDRESS OF ACCOUNT             
         CLI   FVIFLD,C'*'                  IS   EXCLUDE INDICATED              
         BNE   *+8                          NO,  SKIP                           
         LA    R8,1(,R8)                    YES, BUMP BY ONE                    
         MVC   IOKEY+3(12),0(R8)                                                
*                                                                               
DISULA90 GOTO1 AIO,IORD+IOACCFIL+IO2        REFRESH U/L NAME                    
         BNE   DISROUTX                                                         
         OI    6(R6),FVOXMT        TRANSMIT                                     
         TM    REPFIND1,REPFNME                                                 
         BZ    DISROUTX                                                         
         SR    R8,R8                                                            
         IC    R8,0(,R6)                                                        
         AR    R8,R6                                                            
         GOTO1 GETNAME,APPARM,AIOAREA2,(R8)                                     
         OI    6(R8),FVOXMT        TRANSMIT                                     
         B     DISROUTX                                                         
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  GENERIC ROUTINE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISGENC  BRAS  RE,CHKSPC                                                        
         B     DISROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY LOCATION                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
DISLSTA  GOTOR DISLOCS,(R6),RR=APRELO                                           
         B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RECONCILED                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
DISRCON  MVI   8(R6),C' '                                                       
         CLI   RECAP#,0            Recapping ?                                  
         BNE   DISROUTX            No                                           
         MVC   8(1,R6),SVRECON                                                  
         B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY FORMAT REPORT                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
DISFRPT  CLI   SVFRPT,X'40'        Any format report passed ?                   
         BNH   DISROUTX            No, exit                                     
         MVC   8(1,R6),SVFRPT      Use the passed data                          
         B     DISROUTX            Exit                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY BUDGETS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING BUDRECD,R8                                                       
         USING REPFD,R2            REPORT FIELD DSECT                           
DISBUDG  OC    BUDLIST,BUDLIST                                                  
         BZ    DISROUTX                                                         
         SR    R1,R1                                                            
         ICM   R1,3,REPFDDSP                                                    
         AHI   R1,-(ACQAPPL-ACQD)                                               
         CLM   R1,1,BUD#                                                        
         BNL   DISROUTX                                                         
         LA    RE,BUDLIST(R1)                                                   
         CLI   0(RE),0             ANY BUDGET?                                  
         BE    DISROUTX                                                         
         XC    APHALF,APHALF                                                    
         MVI   BUDOK,YES           LOCAL BUDGET SWITCH                          
         MVC   APHALF+1(1),0(RE)                                                
         LA    R8,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN      COMPANY CODE                                 
         MVC   BUDKNO1,APHALF                                                   
         GOTO1 AIO,IO1+IOACCDIR+IOHI                                            
         BL    DISROUTX            EXIT IF HARDWARE ERROR                       
         CLC   BUDKNO1,APHALF                                                   
         BE    *+8                 SKIP IF MATCH                                
         MVI   BUDOK,NO            SET  SW BUDGET NOT OKAY                      
         SR    RF,RF                                                            
         IC    RF,0(,R6)           LENGTH OF FIELD                              
         AHI   RF,-9                                                            
         TM    1(R6),FVAXTND       EXTENDED HEADER?                             
         BZ    *+8                                                              
         AHI   RF,-8                                                            
         CLI   BUDOK,YES           GOOD BUDGET ?                                
         BNE   DISBUD10            NO,  DISPLAY BAD BUDGET                      
         EX    RF,*+4                                                           
         MVC   8(0,R6),BUDKCOD                                                  
         B     DISROUTX                                                         
*                                                                               
*                                  THIS    BUDGET    NUMBER   MIGHT             
*                                          NOT  EXIST    BECAUSE IT             
*                                          WAS  OBSOLETED     -                 
*                                  DISPLAY THE  BUDGET   AS   A  NUMBER         
*                                          INSTEAD   OF  AS   A                 
*                                          BUDGET    ID  NAME                   
DISBUD10 DS    0H                                                               
         SR    R0,R0               CLEAR   REGISTER                             
         ICM   R0,3,APHALF         GET     THE  BUDGET   NUMBER                 
         CVD   R0,APDUB            CONVERT IT   TO       PACKED DECIMAL         
         OI    APDUB+7,X'0F'       SETUP   FOR  DISPLAY                         
         UNPK  8(5,R6),APDUB       CONVERT TO   DISPLAY                         
         MVC   8+5(2,R6),=C' ?'    SAY     SOMETHING IS  WRONG                  
         B     DISROUTX            DISPLAY IT                                   
         DROP  R2,R8                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY SELECT DESCRIPTION                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
DISSELT  TM    REPFIND1,REPFNME                                                 
         BZ    DISSELTX                                                         
         SR    RF,RF                                                            
         IC    RF,0(,R6)                                                        
         AR    RF,R6               POINT TO NEXT FIELD                          
         GOTOR DISPFLD,APPARM,(10,(RF)),RR=APRELO                               
*                                                                               
DISSELTX B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY OPTIONS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
         USING TWAD,R5                                                          
DISOPTS  DS    0H                                                               
         LA    RF,OPT1#            OPTION 1                                     
         LA    RE,OPT1LST                                                       
         MVI   APBYTE,1                                                         
         CLI   REPFFLDN,FLDNOPT1                                                
         BE    DISOPT10                                                         
         LA    RF,OPT3#            OPTION 3                                     
         LA    RE,OPT3LST                                                       
         MVI   APBYTE,3                                                         
         CLI   REPFFLDN,FLDNOPT3                                                
         BE    DISOPT10                                                         
         LA    RF,OPT4#            OPTION 4                                     
         LA    RE,OPT4LST                                                       
         MVI   APBYTE,4                                                         
         CLI   REPFFLDN,FLDNOPT6   OPTION 6                                     
         BNE   DISOPT10                                                         
*&&US                                                                           
         TM    SVCMPST5,CPYAPGS                                                 
         BZ    DISOPTX                                                          
         TM    TWAAUTH,X'20'                                                    
         BZ    DISOPTX                                                          
         MVI   8(R6),C'N'          SET DEFAULT VALUE                            
         B     DISOPTX                                                          
*&&                                                                             
DISOPT10 MVC   NOPTS,0(RF)         SAVE NUMBER OF OPTIONS                       
         ST    RE,APFULL           SAVE ADDRESS OF OPTION LIST                  
         L     RE,ATEXTFLD         DESCRIPTION FIELD FOR OPTION                 
         A     RE,ATWA                                                          
         GOTOR DISPFLD,APPARM,(APBYTE,(RE)),RR=APRELO                           
         SR    R1,R1                                                            
         ICM   R1,1,NOPTS                                                       
         BZ    DISOPTX             NO OPTIONS IN REPORT                         
         TM    REPFIND1,REPFNME    IS THERE ROOM ON SCREEN TO DISPLAY?          
         BZ    DISOPTX             NO                                           
         L     RE,APFULL           RESTORE ADDRESS OF OPTION LIST               
         LA    RF,APWORK                                                        
*                                                                               
DISOPT12 CLI   0(RE),C' '                                                       
         BNE   DISOPT15                                                         
         MVC   0(6,RF),=C'SPACE,'                                               
         LA    RF,6(RF)                                                         
         B     DISOPT18                                                         
*                                                                               
DISOPT15 MVC   0(1,RF),0(RE)                                                    
         MVC   1(1,RF),SCCOMMA                                                  
         LA    RF,2(,RF)                                                        
*                                                                               
DISOPT18 LA    RE,1(,RE)                                                        
         BCT   R1,DISOPT12                                                      
*                                                                               
         BCTR  RF,0                PUT VALID OPTIONS ON SCREEN                  
         LA    R1,APWORK                                                        
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         SR    R5,R5                                                            
         IC    R5,0(,R6)                                                        
         AR    R5,R6               POINT TO NAME FIELD                          
         SR    R1,R1                                                            
         IC    R1,0(,R5)                                                        
         AHI   R1,-9                                                            
         TM    1(R5),FVAXTND                                                    
         BZ    *+8                                                              
         AHI   R1,-8                                                            
         CR    R1,RF               TOO BIG FOR FIELD?                           
         BNL   *+6                 NO                                           
         LR    RF,R1               YES SO USE FIELD LENGTH                      
         EX    RF,*+4                                                           
         MVC   8(0,R5),APWORK                                                   
*                                                                               
DISOPTX  B     DISROUTX                                                         
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  ESTIMATE STATUS DATA                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R1                                                        
DISESTS  DS    0H                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RPFELQ                                                  
         GOTO1 GETEL                                                            
         BNE   DISROUTX                                                         
         MVC   8(L'RPFESTST,R6),RPFESTST                                        
         B     DISROUTX                                                         
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* DISPLAY ACCOUNT OR CONTRA DATA                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
         USING RTND,R4             RTND ADDRESSABILITY                          
DISACCT  DS    0H                                                               
         L     R4,ARTND            ->   RTND                                    
         BRAS  RE,CHKSPC                                                        
         CLI   APPARM,0                                                         
         BE    DISROUTX            NOTHING FOUND                                
         CLI   APPARM,2                                                         
         BNL   DISROUTX            IS WAS A LIST OF ACCOUNTS                    
         TM    REPFIND1,REPFNME    DISPLAY NAME                                 
         BZ    DISROUTX                                                         
         MVC   IOKEY,SPACES                                                     
         MVC   IOKEY(1),CUABIN     COMPANY CODE                                 
*                                                                               
         CLI   RTNFLDN,FLDNPART                                                 
         BE    DISACT30            WHOLE ACCT MUST BE IN SCREEN FIELD           
         CLI   RTNFLDN,FLDNVNDR                                                 
         BE    DISACT30            WHOLE ACCT MUST BE IN SCREEN FIELD           
         L     RF,=A(LEDGLIST)     FIND LEDGER FOR FIELD                        
         A     RF,APRELO                                                        
*                                                                               
DISACT25 CLI   0(RF),X'FF'         EOT?                                         
         BE    DISACT30            ASSUME U/L & ACCT                            
         CLC   RTNFLDN,0(RF)                                                    
         BE    DISACT28                                                         
         LA    RF,3(,RF)                                                        
         B     DISACT25                                                         
         DROP  R2,R4                                                            
*                                                                               
DISACT28 DS    0H                                                               
*                                  UNIT/LEDGER                                  
         MVC   IOKEY+1(LUNLG),1(RF)                                             
         LA    RF,FVIFLD           POINT TO ACCOUNT DATA                        
         CLI   FVIFLD,C'*'         IS   EXCLUDE REQUESTED ?                     
         BNE   *+8                 NO,  SKIP                                    
         LA    RF,1(,RF)           YES, POINT TO ACCOUNT DATA                   
*                                  INSERT THE ACOUNT NAME                       
         MVC   IOKEY+1+LUNLG(LACCOUNT),0(RF)                                    
         B     DISACT40                                                         
*                                                                               
DISACT30 DS    0H                                                               
         LA    RF,FVIFLD           POINT TO U/L AND ACCOUNT DATA                
         CLI   FVIFLD,C'*'         IS   EXCLUDE REQUESTED ?                     
         BNE   *+8                 NO,  SKIP                                    
         LA    RF,1(,RF)           YES, POINT TO U/L AND ACCOUNT DATA           
*                                  INSERT THE U/L AND ACCOUNT NAME              
         MVC   IOKEY+1(LULACNT),0(RF)                                           
*                                                                               
DISACT40 DS    0H                                                               
*                                  MAKE SURE NAME IS IN UPPER CASE              
         OC    IOKEY+1(LULACNT),SPACES                                          
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   DISROUTX                                                         
         ZIC   RF,0(,R6)                                                        
         AR    RF,R6                                                            
         GOTO1 GETNAME,APPARM,AIOAREA2,(RF)                                     
         B     DISROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY FILTERS 1-5                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R1                                                        
         USING REPFD,R2            REPORT FIELD DSECT                           
DISFLTR  LA    R5,8(,R6)                                                        
         L     R1,AIOAREA1                                                      
         MVI   APELEM,RPFELQ       X'C4' PROFILE ELEMENT                        
         GOTO1 GETEL                                                            
         BNE   DISROUTX                                                         
*                                                                               
         USING RTND,R4             RTND ADDRESSABILITY                          
         L     R4,ARTND            ->   RTND                                    
         LA    RE,RPFFLT5          POINT TO VALUE F5                            
         CLI   RTNFLDN,FLDNFLT5                                                 
         BE    DISFLT30                                                         
         LA    RE,RPFFLT1          POINT TO VALUE F1 - F4                       
         SR    RF,RF                                                            
         IC    RF,RTNFLDN                                                       
         SHI   RF,FLDNFLT1                                                      
         AR    RE,RF                                                            
*                                                                               
DISFLT30 MVC   APFLAG,0(RE)                                                     
         TM    REPFIND1,REPFXLD                                                 
         BZ    DISFLT35                                                         
         CLI   APFLAG,C' '                                                      
         BNH   DISROUTX            NO VALUE AT ALL                              
         TM    APFLAG,X'40'        IS IT LOWER CASE LETTER?                     
         BNZ   DISFLT35                                                         
         MVI   0(R5),C'*'                                                       
         LA    R5,1(,R5)                                                        
         OI    APFLAG,X'40'        MAKE UPPER CASE                              
*                                                                               
DISFLT35 MVC   0(1,R5),APFLAG                                                   
         B     DISROUTX                                                         
         DROP  R1,R2,R4                                                         
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RECAP FIELD                                                *         
***********************************************************************         
*                                                                               
DISRECAP MVC   8(1,R6),APYES       ASSUME RECAP = YES                           
         OI    6(R6),FVOXMT        TRANSMIT                                     
         B     DISROUTX            RETURN                                       
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY DATES                                                      *         
***********************************************************************         
*                                                                               
DISDATE  BRAS  RE,DISDTE                                                        
         B     DISROUTX            RETURN                                       
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY COST METHOD                                                *         
***********************************************************************         
         SPACE 1                                                                
DISMTHD  CLI   APGFLAG,YES                                                      
         BE    DISROUTX                                                         
         L     RF,=A(GETMTHD)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   DISROUTX                                                         
*                                                                               
         USING CAHRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    X'3E'                                        
         MVI   CAHKSUB,CAHKSUBQ    X'01'                                        
         MVC   CAHKCPY,CUABIN      COMPANY                                      
         MVC   CAHKMTHD,SAVEMTHD                                                
         XC    CAHKOFC,CAHKOFC                                                  
         GOTO1 AIO,IO3+IOACCFIL+IORD                                            
         BNE   DISROUTX                                                         
*                                                                               
         USING METELD,R1                                                        
         L     R1,AIOAREA3                                                      
         MVI   APELCODE,METELQ     X'82'                                        
         GOTO1 GETEL                                                            
         BNE   DISROUTX                                                         
         MVC   8(3,R6),METCODE                                                  
         B     DISROUTX                                                         
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  GET NEXT AVAILABLE FIELD IN REQUEST CARD 3 OR 4                    *         
***********************************************************************         
         SPACE 1                                                                
         USING ACQD,R3                                                          
GETFFLD  NTR1  ,                                                                
         LA    R0,8                                                             
         L     R3,0(,R1)                                                        
         LA    R4,ACQTYP1          POINT TO 1ST FIELD IN 3RD CARD               
*                                                                               
GETFFLD4 DS    0H                                                               
         CHI   R0,4                GO TO 2ND CARD ?                             
         BNE   *+8                                                              
         LA    R4,ACQTYP5          POINT TO 1ST FIELD 4TH CARD                  
         CLI   0(R4),C' '          IS   THIS FIELD AVAILABLE ?                  
         BNH   GETFFLD9            YES, USE IT                                  
         LA    R4,ACQTYP2-ACQTYP1(,R4)  BUMP TO NEXT FIELD                      
         BCT   R0,GETFFLD4                                                      
         SR    R4,R4               SET TO ZERO TO SAY NO                        
*                                                                               
GETFFLD9 ST    R4,0(,R1)           SAVE ADDRESS                                 
         B     XIT                                                              
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* ERRORS TO SET IN A ROUTINE (NTR1)                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TWAD,R5                                                          
ERRRNOF  MVC   FVMSGNO,=AL2(2105)          Recap record not on file             
         B     ERRXIT                                                           
*                                                                               
ERRRDEL  MVC   FVMSGNO,=AL2(0077)          Record marked deleted                
         B     ERRXIT                                                           
*                                                                               
ERRXCLUD MVC   FVMSGNO,=AL2(2091)          EXCLUDE NOT ALLOWED                  
         B     ERRXIT                                                           
*                                                                               
ERRIBTYP MVC   FVMSGNO,=AL2(ACEBTYPE)      INVALID BILLING TYPE                 
         B     ERRXIT                                                           
*                                                                               
ERRULSEC MVC   FVMSGNO,=AL2(ACEIVSEC)      LEDGER SECURITY UNAUTH               
         B     ERRXIT                                                           
*                                                                               
ERRIVPUT MVC   FVMSGNO,=AL2(FVFNOTV)       INVALID INPUT                        
         B     ERRXIT                                                           
*                                                                               
ERRMISS  MVC   FVMSGNO,=AL2(FVFMISS)       INPUT MISSING                        
         B     ERRXIT                                                           
*                                                                               
ERRLIST  MVC   FVMSGNO,=AL2(ACEIVLT)       INVALID LIST                         
         B     ERRXIT                                                           
*                                                                               
ERRWRKC  MVC   FVMSGNO,=AL2(19)            INVALID WORKCODE                     
         B     ERRXIT                                                           
*                                                                               
ERRDATE  MVC   FVMSGNO,=AL2(ACEIVDT)       INVALID DATE                         
         B     ERRXIT                                                           
*                                                                               
ERREDBSD MVC   FVMSGNO,=AL2(ACEEDBSD)      END  DATE BEFORE START DATE          
         B     ERRXIT                                                           
*                                                                               
ERRBUDG  MVC   FVMSGNO,=AL2(100)           INVALID BUDGET                       
         B     ERRXIT                                                           
*                                                                               
ERRLDGR  MVC   FVMSGNO,=AL2(ACELEDG)       INVALID LEDGER                       
         B     ERRXIT                                                           
*                                                                               
ERRCNTR  MVC   FVMSGNO,=AL2(ACEICNTR)      INVALID CONTRA ACCOUNT               
         B     ERRCNTR1                                                         
*                                                                               
ERRCNTRA MVC   FVMSGNO,=AL2(74)            BAD CONTRA                           
*                                                                               
ERRCNTR1 DS    0H                                                               
         MVC   FVXTRA(LULACNT),FVIFLD                                           
         CLI   FVXTRA,C'*'                                                      
         BNE   *+10                                                             
         MVC   FVXTRA(LULACNT),FVIFLD+1                                         
         B     ERRXIT                                                           
*                                                                               
ERRUSER  MVC   FVMSGNO,=AL2(ACEPRSN)       INVALID PERSON CODE                  
         B     ERRXIT                                                           
*                                                                               
ERRXIT   CLC   FVMSGNO,=AL2(FVFOK)         SET  CONDITION CODE                  
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* ERROR TO DISPLAY ON TOP OF SCREEN                                   *         
***********************************************************************         
         SPACE 1                                                                
IVALFMT  MVC   FVMSGNO,=AL2(ACEIVFT)                                            
         MVC   SAVFORM,SPACES                                                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALPTCH MVC   FVMSGNO,=AL2(1456)          Invalid pitch                        
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(2),PITCH                                                  
         B     IVALEXIT                                                         
                                                                                
IVALSEC  MVC   FVMSGNO,=AL2(ACEIVSEC)      INVALID SECURITY                     
         B     IVALEXIT                                                         
                                                                                
IVALREQ  MVC   FVMSGNO,=AL2(ACEIVRQ)       INVALID REQUEST                      
         B     IVALEXIT                                                         
                                                                                
IVALUSER MVC   FVMSGNO,=AL2(ACEPRSN)       INVALID PERSON CODE                  
         B     IVALEXIT                                                         
                                                                                
IVALINPT MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     IVALEXIT                                                         
                                                                                
IVALSOON MVC   FVMSGNO,=AL2(ACESOON)       INVALID SOONING REQUEST              
         B     IVALEXIT                                                         
                                                                                
IVALEDBS MVC   FVMSGNO,=AL2(ACEEDBSD)      END  DATE BEFORE START DATE          
         B     IVALEXIT                                                         
                                                                                
IVALEMSG MVC   FVMSGNO,=AL2(2096)          FORMAT CONTAINS ERROR MSGS           
         B     IVALEXIT                                                         
                                                                                
IVALOPTN MVC   FVMSGNO,=AL2(0082)          INCOMPATIBLE OPTIONS                 
         MVC   FVADDR,AOPTHDR                                                   
         B     IVALEXIT                                                         
                                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
ADDR     DC    XL4'AAAAAAAA'                                                    
DMRDIR   DC    CL8'DMRDIR'                                                      
DMADD    DC    CL8'DMADD'                                                       
REQUEST  DC    CL8'ACCREQ'                                                      
         LTORG                                                                  
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
*  NEXTUNPT - FIND NEXT UNPROTECTED FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
NEXTUNPT STM   RF,R0,12(RD)                                                     
         SR    R0,R0                                                            
NEXTUNP2 IC    R0,FVTLEN-FVIHDR(R6)                                             
         LR    RF,R6                                                            
         SR    RF,R5                                                            
         ST    RF,ATEXTFLD         SAVE DESCRIPTION FIELD ADDRESS               
         AR    R6,R0               BUMP TO NEXT ONE                             
         TM    FVATRB-FVIHDR(R6),FVAPROT                                        
         JNZ   NEXTUNP2                                                         
         LM    RF,R0,12(RD)                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* Initialize cards with baics                                         *         
***********************************************************************         
         USING ACQD,R3                                                          
         USING TWAD,R5                                                          
INITREQ  ST    RE,SVRE                                                          
         LA    R3,REQCARDS            CLEAR THE REQUEST CARDS                   
         MVI   RUNJOB,C' '                                                      
         XC    REQHDR,REQHDR                   (1st 80 bytes)                   
         LA    R0,ACQCARD1                                                      
         LA    R1,L'ACQCARD1*8                                                  
         LA    RE,SPACES                                                        
         LA    RF,1                                                             
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE                                                            
         XC    OCURDTE,OCURDTE                                                  
         XC    MMOSRNG,MMOSRNG                                                  
         MVI   FFOUND,0                                                         
*                                                                               
         GOTOR GETFFLD,APPARM,(R3)                                              
         L     R8,APPARM           AVAILABLE LOCATION IN REQ CARD               
         MVI   0(R8),ACQPID        SET TO PERSONAL ID                           
         MVC   1(L'TWAPERSN,R8),TWAPERSN                                        
         ST    R8,APID                                                          
*                                                                               
         MVI   SVREVOPT,YES        DEFAULT TO INCLUDE REVERSALS                 
         MVI   SVDFTOPT,NO         DEFAULT TO EXCLUDE DRAFT ITEMS               
         CLI   APREPJCL,REPJCLV    PRODUCTION?                                  
         JNE   *+12                                                             
         MVI   ACQXJOB,NO          EXCLUDE EXPENSE JOBS                         
         MVI   ACQDJOB,NO          EXCLUDE DRAFT JOBS                           
         L     RE,SVRE                                                          
         BR    RE                                                               
         DROP  R3,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY DATES                                                      *         
***********************************************************************         
*                                                                               
         USING RTND,R4             RTND ADDRESSABILITY                          
DISDTE   NTR1  BASE=*,LABEL=*                                                   
         TM    RPTIND,RPTFILL                                                   
         JZ    DISDTEX                                                          
         L     R4,ARTND            ->   RTND                                    
         LA    R5,RQSTR            START DATE FROM REPORT LIST SCREEN           
         CLI   RTNFLDN,FLDNSTDT    START DATE ?                                 
         JE    DISDTE04                                                         
         LA    R5,RQEND            End Date from Report List Screen             
         CLI   RTNFLDN,FLDNENDT    End Date?                                    
         JNE   DISDTE10                                                         
*                                                                               
DISDTE04 OC    0(L'RQSTR,R5),0(R5)                                              
         JZ    DISDTE10                                                         
         GOTO1 VDATCON,APPARM,(1,(R5)),(8,8(R6))                                
*                                                                               
DISDTE10 CLI   RTNFLDN,FLDNMOAR    MOA range ?                                  
         JNE   DISDTE20                                                         
         OC    RQMOAE,RQMOAE                                                    
         JZ    DISDTE20            No dates                                     
         LA    R5,8(,R6)           Point to output location                     
         OC    RQMOAS,RQMOAS       Any start MOA ?                              
         JZ    DISDTE14                                                         
         MVC   APWORK(2),RQMOAS                                                 
         MVI   APWORK+2,01                                                      
         GOTO1 VDATCON,APPARM,(1,APWORK),(9,8(R6))                              
*&&US*&& LA    R5,14(,R6)                                                       
*&&UK*&& LA    R5,13(,R6)                                                       
         CLC   RQMOAS,RQMOAE                                                    
         JE    DISDTE20            FINISHED, JUST THIS MONTH                    
*                                                                               
DISDTE14 MVI   0(R5),C'-'                                                       
         LA    R5,1(,R5)                                                        
         MVC   APWORK(2),RQMOAE                                                 
         MVI   APWORK+2,01                                                      
         GOTO1 VDATCON,APPARM,(1,APWORK),(9,(R5))                               
*                                                                               
DISDTE20 LA    RF,8                DATCON MMMDD/YY                              
         CLI   RTNFLDN,FLDNPRDR    Period range?                                
         JE    DISDTE21                                                         
         LA    RF,9                DATCON MMM/YY                                
         CLI   RTNFLDN,FLDNCNDR    Calendar range?                              
         JNE   DISDTE30                                                         
*                                                                               
DISDTE21 LA    R5,8(,R6)                                                        
         OC    RQSTR,RQSTR         Start date ?                                 
         JZ    DISDTE25                                                         
         GOTO1 VDATCON,APPARM,(1,RQSTR),((RF),8(R6))                            
*                                                                               
         LA    RF,9                                                             
DISDTE22 CLI   0(R5),C' '          Find end of data in date field               
         JNH   DISDTE25                                                         
         LA    R5,1(,R5)           Next character                               
         JCT   RF,DISDTE22                                                      
*                                                                               
DISDTE25 OC    RQEND,RQEND                                                      
         JZ    DISDTE30                                                         
         LA    RF,8                DATCON MMMDD/YY                              
         CLI   RTNFLDN,FLDNPRDR    Period range ?                               
         JE    *+8                                                              
         LA    RF,9                DATCON MMM/YY                                
         MVI   0(R5),C'-'                                                       
         LA    R5,1(,R5)                                                        
         GOTO1 VDATCON,APPARM,(1,RQEND),((RF),(R5))                             
*                                                                               
DISDTE30 DS    0H                                                               
         OI    6(R6),FVOXMT        TRANSMIT                                     
*                                                                               
DISDTEX  J     XIT                                                              
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* Get user job code. Optional Name. Optional T=A                      *         
*     Ex.      Bobby         --> Name = Bobby, code = Bob             *         
*     Ex.      Bobby,BBY     --> Name = Bobby, code = BBY             *         
*     Ex.      Bobby,T=A     --> Name = Bobby, Code = Bob, TEST=02A   *         
*     Ex.      Bobby,BTT,T=B --> Name = Bobby, Code = BBT, TEST=02B   *         
* as above, with pin=1234 or n=1234, or pid=* or i=*                  *         
***********************************************************************         
         USING ACQD,R3                                                          
         USING TWAD,R5                                                          
         USING SCANBLKD,R8                                                      
GETUSER  NTR1  BASE=*,LABEL=*                                                   
         MVC   FVXTRA,SPACES                                                    
         GOTOR AFVAL,REQPERH       REQUESTOR NAME IS REQUIRED                   
         JE    GETUSR10            NONE, ERROR                                  
         MVC   FVXTRA(L'REQPER),REQPER                                          
         J     GETUSERN                                                         
*                                                                               
GETUSR10 DS    0H                                                               
*&&UK                                                                           
         CLI   CUCTRY,X'03'        Prevent input of \ or , as delimiter         
         BNE   GETUSR15            for Germany                                  
         LA    R8,REQPERH          (Only # is valid)                            
         LA    R6,L'REQPER                                                      
GETUSR12 CLI   0(R8),C','                                                       
         BE    GETUSR13                                                         
         CLI   0(R8),C'\'                                                       
         BE    GETUSR13                                                         
         LA    R8,1(R8)                                                         
         BCT   R6,GETUSR12                                                      
         B     GETUSR15                                                         
                                                                                
GETUSR13 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     GETUSERN                                                         
                                                                                
                                                                                
GETUSR15 GOTOR VSCANNER,APPARM,REQPERH,(4,BLOCK),SCNP3NEQ                       
                                                                                
         LA    R8,BLOCK                                                         
         CLC   SCONEFLD(3),=C'PIN'                                              
         BE    GETUSR95                                                         
         CLC   SCONEFLD(3),=C'PID'                                              
         BE    GETUSR95                                                         
         CLC   SCONEFLD(2),=C'I='                                               
         BE    GETUSR95                                                         
         CLC   SCONEFLD(2),=C'N='                                               
         BE    GETUSR95                                                         
*&&                                                                             
*&&US                                                                           
         GOTOR VSCANNER,APPARM,REQPERH,(3,BLOCK),SCNP3NEQ                       
         LA    R8,BLOCK                                                         
*&&                                                                             
         MVC   ACQESTOR,SCONEFLD                                                
         MVI   TESTCHR,C' '        For T=A or B or C or S                       
         MVC   INUSER,SCONEFLD     Three character job code                     
         CLI   APPARM+4,2          Two parameters ?                             
         JL    GETUSR90            No, just one so done                         
         AHI   R8,SCBLKLQ          Next block entry                             
*&&UK*&& TM    CUSTAT,CUSDDS       IS IT A DDS TERMINAL?                        
*&&UK*&& BZ    *+8                                                              
         BRAS  RE,TESTCARD                                                      
         CLI   TESTCHR,C' '        Was this set ?                               
*&&US*&& JNE   GETUSR90            Yes                                          
*&&UK                                                                           
         JNE   GETUSR20            Yes                                          
         BRAS  RE,VALPIN           NO - CHECK IF WE HAVE A PIN/PID              
         BNE   GETUSERN                                                         
         CLI   RQHFLG1,0                                                        
         BNE   GETUSR90                                                         
*&&                                                                             
         MVC   INUSER,SCONEFLD     No, so set three character job code          
         CLI   APPARM+4,3          Three parameters                             
         JL    GETUSR90                                                         
         AHI   R8,SCBLKLQ          Next block entry                             
*&&UK*&& TM    CUSTAT,CUSDDS       IS IT A DDS TERMINAL?                        
*&&UK*&& BZ    *+8                                                              
         BRAS  RE,TESTCARD                                                      
*&&UK                                                                           
         CLI   TESTCHR,C' '                                                     
         JE    GETUSR25                                                         
         CLI   APPARM+4,3                                                       
         BE    GETUSR90                                                         
                                                                                
GETUSR20 AHI   R8,SCBLKLQ                                                       
GETUSR25 BRAS  RE,VALPIN                                                        
         BNE   GETUSERN                                                         
*&&                                                                             
GETUSR90 TM    INWHEN,MIXIOKS      Are we sooning?                              
         JZ    GETUSERY            No                                           
         CLC   =C'DDS',INUSER      Yes, so DDS is not allowed                   
         JNE   GETUSERY                                                         
GETUSR95 MVC   FVXTRA(L'INUSER),INUSER                                          
         MVC   FVMSGNO,=AL2(ACEPRSN)                                            
                                                                                
GETUSERN LTR   RB,RB                                                            
         J     XIT                                                              
GETUSERY CR    RB,RB                                                            
         J     XIT                                                              
***********************************************************************         
* See if T=A, B, C or S was used and set TESTCHR. R8 = BLOCK entry    *         
***********************************************************************         
TESTCARD CLI   SC1STLEN,3          See if it could be T=A                       
         JNE   TESTCRDX            No                                           
         CLC   =C'T=',SCONEFLD                                                  
         JNE   TESTCRDX            No                                           
         CLI   SCONEFLD+2,C'S'     Saved version                                
         JE    TESTCRD8                                                         
         CLI   SCONEFLD+2,C'A'                                                  
         JL    TESTCRDX                                                         
         CLI   SCONEFLD+2,C'C'                                                  
         JH    TESTCRDX                                                         
TESTCRD8 MVC   TESTCHR,SCONEFLD+2  Set value                                    
                                                                                
TESTCRDX BR    RE                                                               
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
* Validate PIN/PID if entered.                                        *         
***********************************************************************         
VALPIN   ST    RE,SVRE                SAVE RE                                   
         CLC   SCONEFLD(4),=C'PIN='   DO WE HAVE A PIN?                         
         BE    VALPIN30                                                         
         CLC   SCONEFLD(2),=C'N='     N=PIN                                     
         BE    VALPIN31                                                         
         CLC   SCONEFLD(4),=C'PID='   DO WE HAVE A PID?                         
         BE    VALPIN10                                                         
         CLC   SCONEFLD(2),=C'I='     I=PID                                     
         BE    VALPIN12                                                         
         B     VALPINY                                                          
                                                                                
VALPIN10 CLI   SC1STLEN,5                                                       
         BNE   *+12                                                             
         CLI   SCONEFLD+4,C'*'                                                  
         BE    VALPIN20                                                         
         CLI   SC1STLEN,12                                                      
         BH    INVPID                                                           
         IC    R6,SC1STLEN                                                      
         AHI   R6,-4                                                            
         BCTR  R6,0                                                             
         MVC   PERSON,SPACES                                                    
         MVC   PERSON(0),SCONEFLD+4                                             
         EX    R6,*-6                                                           
         B     VALPIN15                                                         
                                                                                
VALPIN12 CLI   SC1STLEN,3                                                       
         BNE   *+12                                                             
         CLI   SCONEFLD+2,C'*'                                                  
         BE    VALPIN20                                                         
         CLI   SC1STLEN,10                                                      
         BH    INVPID                                                           
         IC    R6,SC1STLEN                                                      
         AHI   R6,-2                                                            
         BCTR  R6,0                                                             
         MVC   PERSON,SPACES                                                    
         MVC   PERSON(0),SCONEFLD+2                                             
         EX    R6,*-6                                                           
         B     VALPIN15                                                         
                                                                                
         USING SAPEREC,R6                                                       
VALPIN15 LA    R6,IOKEY            VERIFY AND GET BINARY PID                    
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,CUAGYSEC    SECURITY ALPHA ID                            
         MVC   SAPEPID,PERSON                                                   
         GOTO1 AIO,IORD+IOCTFILE+IO2                                            
         GOTO1 AIO,IOHI+IOCTFILE+IO2                                            
         L     R6,AIOAREA2                                                      
         BNE   INVPID                                                           
         CLC   SAPEPID,PERSON                                                   
         BNE   INVPID                                                           
         SR    RF,RF                                                            
         LA    R1,SAPEDATA                                                      
         USING SAPWDD,R1                                                        
                                                                                
VALPIN17 CLI   0(R1),0                                                          
         BE    INVPID                                                           
         CLI   0(R1),SAPWDELQ                                                   
         BE    VALPIN19                                                         
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VALPIN17                                                         
                                                                                
VALPIN19 MVC   REQPID,SAPWDNUM                                                  
         B     VALPIN28                                                         
                                                                                
VALPIN20 XC    REQPID,REQPID       OBTAIN CONNECTED USERS PID                   
         USING COMFACSD,R1                                                      
         L     R1,ACOM                                                          
         ICM   RF,15,CXTRAINF                                                   
         BZ    INVPID                                                           
         USING XTRAINFD,RF                                                      
         OC    XIPID,XIPID                                                      
         BZ    INVPID                                                           
         MVC   REQPID,XIPID                                                     
         B     VALPIN28                                                         
         DROP  RF,R1                                                            
                                                                                
VALPIN28 MVI   RQHFLG1,RQHFPID     SAVE PID                                     
         MVC   RQHSECD(2),CUAGYSEC                                              
         MVC   RQHSECD+2(2),REQPID                                              
         B     VALPINY                                                          
                                                                                
VALPIN30 CLI   SC1STLEN,8                                                       
         BNE   INVPIN                                                           
         MVC   PINNUM,SCONEFLD+4                                                
         B     VALPIN32                                                         
                                                                                
VALPIN31 CLI   SC1STLEN,6                                                       
         BNE   INVPIN                                                           
         MVC   PINNUM,SCONEFLD+2                                                
                                                                                
VALPIN32 LA    R6,PINNUM                                                        
         LA    RF,4                                                             
                                                                                
VALPIN33 CLI   0(R6),C'A'          CHECK ALPHANUMERIC INPUT                     
         BL    INVPIN                                                           
         CLI   0(R6),C'I'                                                       
         BNH   VALPIN35                                                         
         CLI   0(R6),C'J'                                                       
         BL    INVPIN                                                           
         CLI   0(R6),C'R'                                                       
         BNH   VALPIN35                                                         
         CLI   0(R6),C'S'                                                       
         BL    INVPIN                                                           
         CLI   0(R6),C'Z'                                                       
         BNH   VALPIN35                                                         
         CLI   0(R6),C'0'                                                       
         BL    INVPIN                                                           
         CLI   0(R6),C'9'                                                       
         BH    INVPIN                                                           
                                                                                
VALPIN35 LA    R6,1(R6)                                                         
         BCT   RF,VALPIN33                                                      
         CLC   PINNUM,=C'0000'     CANNOT BE ALL ZEROS                          
         BE    INVPIN                                                           
         MVI   RQHFLG1,RQHFPIN                                                  
         MVC   RQHSECD,PINNUM                                                   
         B     VALPINY                                                          
                                                                                
INVPIN   MVC   FVMSGNO,=AL2(ACEINPIN)  SET INVALID PIN                          
         B     VALPINN                                                          
                                                                                
INVPID   MVC   FVMSGNO,=AL2(ACEINPID)   SET INVALID PID                         
         B     VALPINN                                                          
                                                                                
VALPINN  CR    RB,RD                                                            
         L     RE,SVRE                                                          
         BR    RE                                                               
VALPINY  CR    RB,RB                                                            
         L     RE,SVRE                                                          
         BR    RE                                                               
*&&                                                                             
         DROP  R3,R5,R8                                                         
         LTORG                                                                  
***********************************************************************         
*  Display description of field stored in X'B0' element               *         
***********************************************************************         
         SPACE 1                                                                
         USING TFDELD,R1                                                        
DISPFLD  NTR1  BASE=*,LABEL=*                                                   
         MVC   APBYTE,0(R1)                                                     
         L     R5,0(,R1)                                                        
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,TFDELQ     X'B0' TEXT FIELD                             
         GOTO1 GETEL                                                            
*                                                                               
DISPFLD2 BNE   DISPFLDX                                                         
         CLC   TFDSEQ,APBYTE       SEQUENCE NUMBER PASSED IN APBYTE             
         BE    DISPFLD5                                                         
         GOTO1 NEXTEL                                                           
         B     DISPFLD2                                                         
*                                                                               
DISPFLD5 SR    RF,RF                                                            
         IC    RF,TFDLN            LENGTH OF DESCRIPTION                        
*                                  GET  DATA LENGTH                             
         AHI   RF,-(TFDTEXT-TFDELD)                                             
         SR    RE,RE                                                            
         IC    RE,0(,R5)                                                        
         AHI   RE,-8                                                            
         TM    1(R5),FVAXTND                                                    
         BZ    *+8                                                              
         AHI   RE,-8               GET  FIELD LENGTH                            
         LR    R2,RE                                                            
         BCTR  R2,0                SUBTRACT ONE FOR EXECUTE                     
         EX    R2,*+4                                                           
         MVC   8(0,R5),SPACES     CLEAR FIELD FIRST                             
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                SUBTRACT ONE FOR EXECUTE                     
         EX    RF,*+4                                                           
         MVC   8(0,R5),TFDTEXT                                                  
*                                                                               
DISPFLDX XIT1                                                                   
         LTORG                                                                  
         DROP  R1                                                               
         EJECT ,                                                                
*                                                                               
***********************************************************************         
* CHKRFILL -   THIS ROUTINE MAY BE USED TO DETERMINE IF WE MAY REFILL *         
*              A FIELD FROM THE USER'S PROFILE; E.G. WE SHOULD NOT BE *         
*              REFILLING THE FIELD IN SOME CASES WHEN RECAPPING IS IN *         
*              EFFECT AND THE REPFRNRF IS ON.                         *         
*                                                                     *         
* NOTE - ON INPUT R2 POINTS TO THE THE REPORT FIELD DSECT             *         
*                                                                     *         
* ON EXIT:                                                            *         
*    CONDITION CODE:                                                  *         
*        EQ -  REFILL THE FIELD                                       *         
*        NE -  DO NOT REFILL THE FIELD                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
         USING REPFD,R2                                                         
CHKRFILL TM    REPFIND2,REPFRNRF   Do not refill when recap is on ?             
         JZ    CHKRFLY             No this is ok to re-fill                     
         TM    RPTIND,RPTFILL      Override from REPORT/REQUEST screen          
         JO    CHKRFLY             Yes, so use value                            
         CLI   RECAP#,0            Any recap requested ?                        
         JE    CHKRFLY             No, so ok to re-fill                         
         CLI   REPFFLDN,FLDNUNLG   Unit/Ledger field ?                          
         JNE   CHKRF10             No                                           
         TM    RCAPSW,RCAPMIXL     Mixed ledgers                                
         JO    CHKRFLN             Yes, so don't refill                         
*                                                                               
CHKRF10  TM    RCAPSW,RCAPPROF     If recapped, do any use inividual            
         JZ    CHKRFLY                profiles ? (No to bracnh here)            
*                                                                               
CHKRFLN  CR    RB,RD               Yes, so do not refill                        
         BR    RE                                                               
*                                                                               
CHKRFLY  CR    RB,RB                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*                                                                               
***********************************************************************         
*  WILDCARD - DETERMINE IF A WILDCARD CHARACTER "?" IS PRESENT        *         
*                                                                     *         
*  ON EXIT:                                                           *         
*     REGISTER 15 HAS:                                                *         
*         -1 = ERROR -> WILDCARD FOUND IN UNIT/LEDGER                 *         
*          0 = NO WILDCARD CHARACTERS                                 *         
*          4 = WILDCARD CHARACTERS FOUND                              *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
WILDCARD NTR1  BASE=*,LABEL=*                                                   
         L     RF,0(,R1)           START OF FIELD                               
         SR    R0,R0                                                            
         ICM   R0,1,7(R1)          FIELD LENGTH                                 
*                                                                               
         CLI   4(R1),1             SPECIAL UNIT/LEDGER REQUEST ?                
         BNE   WILD200             NO,  SKIP                                    
         LA    R1,2                CHECK FIRST 2 BYTES OF UNIT/LEDGER           
WILD100  DS    0H                                                               
         CLI   0(RF),C'?'          UNIT/LEDGER HAS WILDCARD ?                   
         BE    WILDER              WILDCARD INVALID                             
         LA    RF,1(,RF)           NEXT CHARACTER                               
         BCTR  R0,0                SUBTRACT ONE FROM OVERALL LENGTH             
         BCT   R1,WILD100          LOOP                                         
*                                                                               
WILD200  DS    0H                  GENERAL TEST FOR WILDCARD                    
         CLI   0(RF),C'?'          WILDCARD ?                                   
         BE    WILDYES             WILDCARD FOUND                               
         LA    RF,1(,RF)           NEXT CHARACTER                               
         BCT   R0,WILD200                                                       
*                                                                               
WILDNO   DS    0H                  NO   WILDCARD CHARACTER FOUND                
         SR    RF,RF               RETURN  0 IN REGISTER 15                     
         B     WILDEX              EXIT                                         
*                                                                               
WILDYES  DS    0H                  WILDCARD CHARACTER FOUND                     
         LA    RF,4                RETURN  4 IN REGISTER 15                     
         B     WILDEX              EXIT                                         
*                                                                               
WILDER   DS    0H                  WILDCARD CHARACTER FOUND IN U/L              
         LHI   RF,-1               RETURN -1 IN REGISTER 15                     
*                                                                               
WILDEX   LTR   RF,RF                                                            
         XIT1  REGS=(RF)                                                        
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  ADD APG U/L ELEMENT                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2                                                        
ELM      USING RFLELD,APELEM                                                    
         SPACE 1                                                                
ADDAPGUL NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOAREA1                                                      
         AH    R2,DATADISP         GET    TO   1ST  ELEMENT                     
         SR    RF,RF               CLEAR  REGISTER                              
*                                                                               
*                                  ************************************         
*                                  * DELETE OLD COPIES OF THE ELEMENT *         
*                                  ************************************         
ADDAPG10 DS    0H                                                               
         CLI   0(R2),0             END    OF   RECORD ?                         
         BE    ADDAPG30            YES,   NO   MORE ELEMENTS TO DELETE          
         CLI   0(R2),RFLELQ        X'C5'  ELEMENT ?                             
         BNE   ADDAPG20            NO,    LOOP TO   NEXT ELEMENT                
         CLI   RFLTYPE,RFLLDG      X'01'  LEDGER ?                              
         BNE   ADDAPG20            NO,    LOOP TO   NEXT ELEMENT                
         MVI   0(R2),X'FF'         MARK   FOR  DELETION                         
*                                                                               
ADDAPG20 DS    0H                                                               
         IC    RF,1(,R2)           GET    LENGTH                                
         AR    R2,RF               BUMP   TO   NEXT ELEMENT                     
         B     ADDAPG10            LOOP                                         
*                                                                               
ADDAPG30 DS    0H                                                               
         MVI   APELCODE,X'FF'      DELETE X'FF'     ELEMENTS                    
         GOTO1 DELEL,(R2)                                                       
         DROP  R2                                                               
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   ELM.RFLEL,RFLELQ    X'C5' SIMULATE SCRIBE ELEMENT                
         MVI   ELM.RFLTYPE,RFLLDG  X'01' LEDGER                                 
         SR    RF,RF                                                            
         ICM   RF,1,LDGR#                                                       
         BZ    ADDAPG50                                                         
         LA    RE,ELM.RFLDATA                                                   
         LA    R1,LDGLIST                                                       
*                                                                               
ADDAPG40 MVC   0(2,RE),0(R1)                                                    
         MVC   2(1,RE),SCCOMMA                                                  
         LA    R1,2(,R1)           BUMP TO NEXT LEDGER                          
         LA    RE,3(,RE)                                                        
         BCT   RF,ADDAPG40                                                      
         BCTR  RE,0                                                             
         LA    RF,ELM.RFLEL        FIGURE OUT LENGTH OF ELEMENT                 
         SR    RE,RF                                                            
         STC   RE,ELM.RFLLN                                                     
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
*                                                                               
ADDAPG50 XIT1                      RETURN                                       
         DROP  ELM                                                              
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE OFFICE                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
         USING RTND,R6                                                          
VALOFF   NTR1  BASE=*,LABEL=*                                                   
         L     R3,4(,R1)           R3 = ACQD requst card field                  
         L     R4,0(,R1)           R4 = Field header                            
         L     R6,8(,R1)           R6 = Routine table                           
         MVC   CUROFF,SPACES                                                    
         CLI   FVILEN,0            Any data ?                                   
         BE    VALOFFOK            No, exit                                     
         CLI   FVIFLD,C'('         Profile on record ?                          
         BNE   VALOFF05            No, skip                                     
         CLI   RECAP#,0            Yes. Recapping ?                             
         BE    VALOFF50            No, verify the offices                       
         TM    RCAPSW,RCAPPROF     Yes. Use recap's profile ?                   
         BZ    VALOFF50            No, verify the offices                       
         LA    RF,CUROFF           Set CUROFF to (O                             
         GOTOR GDEFTEXT,APPARM,(RF),RR=APRELO                                   
         B     VALOFF50            Verify the offices                           
*                                                                               
VALOFF05 DS    0H                                                               
         MVI   BYTE,NO             Assume included                              
         ZIC   R5,FVXLEN                                                        
         LA    R9,FVIFLD                                                        
         CLI   FVIFLD,C'*'         Is it exclude this office/list ?             
         BE    VALOFF10            Yes, process exclude                         
         CLI   FVILEN,2            Input length > 2 ?                           
         BH    VALOFFN             Yes, invalid input                           
         B     VALOFF20            Save the office ID                           
*                                                                               
VALOFF10 DS    0H                  Exclude office(s)                            
         TM    REPFIND1,REPFXLD    Exclude supported ?                          
         BZ    VALOFFN             No, error                                    
         MVI   BYTE,YES            Set to exclude                               
         CLI   FVILEN,3            Input length > 3 ?                           
         BH    VALOFFN             Yes, invalid input                           
         LA    R9,1(,R9)           Bump past '*' to office code                 
         BCTR  R5,0                Adjust length                                
*                                                                               
VALOFF20 DS    0H                                                               
         EX    R5,*+4                  Save office in CUROFF                    
         MVC   CUROFF(0),0(R9)                                                  
         LA    R5,1(,R5)               Adjust to full length                    
         GOTO1 VOFFICE,APPARM,('VOFTYPE1',CUROFF),(BYTE,(R5))                   
         BNE   VALOFFEX                Error                                    
*                                                                               
         CLI   FVIFLD,C'*'             Exclude requested ?                      
         BNE   VALOFFOK                No, offices is okay                      
         NI    CUROFF,TURNOFF-X'40'    Set to exclude offices                   
         B     VALOFFOK                                                         
*                                                                               
VALOFFN  MVC   FVMSGNO,=AL2(ACEIVOF)   Invalid office message                   
         B     VALOFFEX                                                         
*                                                                               
VALOFF50 SR    R5,R5                                                            
         IC    R5,RTNPROFN                                                      
         GOTO1 VOFFICE,APPARM,('VOFTYPER',AIOAREA1),                   X        
               ('MAXPARM',BLOCK),(R5)                                           
         BNE   VALOFFEX                Error                                    
*                                                                               
VALOFFOK MVC   0(2,R3),CUROFF      Move data to request card field              
*                                                                               
VALOFFEX XIT1                                                                   
         DROP  R2,R6                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  SEE IF FIELD WAS BUILT (SPECIAL RULES FOR FIELD)                   *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
         USING ACQD,R3                                                          
FLDCNTL  NTR1  BASE=*,LABEL=*                                                   
         CLI   REPFFLDN,FLDNBUDG   BUDGET ?                                     
         BNE   FLDCNT10                                                         
         SR    R1,R1                                                            
         ICM   R1,3,REPFDDSP                                                    
         AHI   R1,-(ACQAPPL-ACQD-1)                                             
         CLM   R1,1,BUD#                                                        
         BH    FLDCNT98            SKIP  FIELD                                  
*                                                                               
FLDCNT10 CLI   APGFLAG,YES                                                      
         BNE   FLDCNT30            NO,   TEST   SOME MORE                       
         CLI   REPFFLDN,FLDNOPT6                                                
*&&UK*&& BE    FLDCNT90            YES,  FIELD  ON   SCREEN                     
*&&US                                                                           
         BNE   FLDCNT30            NO,   TEST   SOME MORE                       
         TM    SVCMPST5,CPYAPGS                                                 
         BZ    FLDCNT98            FIELD NOT    ON   SCREEN                     
         CLI   APMODE,APMVALR                                                   
         BNE   FLDCNT20                                                         
         SR    RE,RE                                                            
         ICM   RE,3,REPFDDSP                                                    
         AR    RE,R3                                                            
         MVI   0(RE),C'N'          SET   DEFAULT                                
*                                                                               
FLDCNT20 TM    CUAUTH,X'20'        ARE   THEY   AUTHORIZED ?                    
         BZ    FLDCNT98            NO,   FIELD  NOT  ON   SCREEN                
         B     FLDCNT90            YES,  FIELD  ON   SCREEN                     
*&&                                                                             
*                                                                               
FLDCNT30 CLI   REPFFLDN,FLDNRCAP   RECAP FIELD ?                                
         BNE   FLDCNT90            NO,   FIELD  ON   SCREEN                     
         CLC   RECAP#,REPFDDSP+1   RECAP FORMAT EXISTS ?                        
         BL    FLDCNT98            NO,   FIELD  NOT  ON   SCREEN                
*                                                                               
FLDCNT90 SR    RE,RE               FIELD ON     SCREEN                          
*                                                                               
FLDCNT98 LTR   RE,RE               IS    FIELD  ON   SCREEN ?                   
         XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
* CHKSPC - SEE IF SCREEN FIELD HAS SPACES; IF YES, THEN INSERT FIELD  *         
*          FROM RECORD ELEMENTS                                       *         
*                                                                     *         
* NOTE - ON INPUT R6 POINTS TO THE FLDHDRD                            *         
*                                                                     *         
* ON EXIT:                                                            *         
*    CONDITION CODE:                                                  *         
*        EQ - FIELD MAY HAVE BEEN SUPPLIED BY RECORD ELEMENTS         *         
*        NE - +/- LIST(S) OR DATA SUPPLIED                            *         
*                                                                     *         
*    APPARM FIRST BYTE (NOT USABLE FOR TRANSACTION TYPE):             *         
*        0  - NO INPUT - INITIAL SET ('(XXXX') MAY HAVE BEEN CLEARED  *         
*        1  - DATA SUPPLIED                                           *         
*        2  - +/- LIST(S) SUPPLIED                                    *         
*        3  - MULTIPLE ITEMS OR LISTS SUPPLIED                        *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
CHKSPC   NTR1  BASE=*,LABEL=*                                                   
         MVI   APPARM,0            SET   TO    NO     INPUT                     
         CLI   FVILEN,0            ANY   INPUT ?                                
         BE    CHKSPC10            NO,   CHECK PROFILE       DATA               
         CLC   FVIFLD,SPACES       ONLY  BLANKS       ENTERED ?                 
         BE    CHKSPC05            YES,  RESET FVILEN                           
         CLI   FVILEN,1            Length 1 is not a list                       
         BE    CHKSPC02                                                         
         MVI   APPARM,2            SET   TO    +/-    LIST                      
         CLI   FVIFLD,C'+'                                                      
         BE    CHKSPC90            YES,  VALIDATE +                             
         CLI   FVIFLD,C'-'                                                      
         BE    CHKSPC90            YES,  VALIDATE -                             
                                                                                
CHKSPC02 MVI   APPARM,1            SET   TO    SINGLE                           
         CLI   FVIFLD,C'('         USE   PROFILE      DATA ?                    
         BNE   CHKSPC95            HAS   INPUT DATA - SO     USE  IT            
         SR    R1,R1               NOT   INPUT DATA - USE    PROFILE            
         IC    R1,FVXLEN                 CLEAR LEFT   OVER   DATA               
         EX    R1,*+4                                                           
         MVC   8(0,R6),SPACES      WIPE  OUT   TO     RESET                     
         MVI   FVILEN,0            RESET FIELD LENGTH                           
         MVI   FVXLEN,0            RESET EXECUTE      FIELD  LENGTH             
         B     CHKSPC20            FILL  IN    FROM   PROFILE                   
*                                                                               
CHKSPC05 DS    0H                  ONLY  BLANKS       ENTERED                   
         MVI   FVILEN,0            RESET FIELD LENGTH                           
         MVI   FVXLEN,0            RESET EXECUTE      FIELD LENGTH              
*                                                                               
CHKSPC10 DS    0H                                                               
         BRAS  RE,CHKRFILL         SHOULD      WE     REFILL THIS FLD ?         
         BE    CHKSPC20            YES,  CONTINUE                               
*                                  NO,   DO    NOT    REFILL                    
         MVI   FVILEN,0            RESET FIELD LENGTH                           
         MVI   FVXLEN,0            RESET EXECUTE      FLD  LENGTH               
         MVI   APPARM,0            SAY   NO    INPUT  FOUND                     
         SR    RE,RE               SAY   NOT   +/-LST NOR  DATA SUPPL'D         
         B     CHKSPC95            RETURN                                       
*                                                                               
CHKSPC20 SR    R5,R5                                                            
         TM    REPFIND1,REPFNME    DO WE HAVE A NAME FIELD (DESC FLD) ?         
         BZ    CHKSPC30            NO, SKIP                                     
         IC    R5,0(,R6)           GET THIS FIELD'S LENGTH                      
         AR    R5,R6               POINT TO NEXT FIELD'S HEADER                 
*                                  (FOUND ADDRESS OF DESRIPTION FIELD)          
         USING RTND,R4             RTND ADDRESSABILITY                          
CHKSPC30 L     R4,ARTND            ->   RTND                                    
         SR    RF,RF                                                            
         ICM   RF,3,RTNGTXNO       FOR GOTO1 FILLFLD -                          
         ST    RF,APPARM+12            PROFILE NUMBER FOR GETTEXT               
         GOTOR FILLFLD,APPARM,AIOAREA1,(RTNPROFN,(R6)),(R5),RR=APRELO           
         GOTO1 AFVAL,(R6)          RETURN THE INSERTED VALUES IN FVAREA         
         SR    RE,RE                                                            
         B     CHKSPC95            EXIT                                         
*                                                                               
CHKSPC90 DS    0H                  +/- LISTS NOT VALID FOR APG                  
         CLI   APGFLAG,NO          APG  REPORT ?                                
         BNE   ERRIVPUT            INVALID INPUT                                
*                                                                               
CHKSPC95 LTR   RE,RE                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY COST LOCATION STATUS                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT     FIELD     DSECT                   
DISLOCS  NMOD1 0,**DLOC**                                                       
         L     RC,APALOCAL                                                      
         LR    R6,R1               SAVE  HEADER    ADDRESS                      
         L     R2,AIOAREA1         ->    RECORD                                 
         AH    R2,DATADISP         ->    ELEMENT                                
         SR    RF,RF                                                            
*                                                                               
         USING RFLELD,R2                                                        
DISLOC10 DS    0H                                                               
         CLI   0(R2),0             END   OF   RECORD ?                          
         BE    DISLOCSX                                                         
         CLI   0(R2),RFLELQ        X'C5'                                        
         BNE   DISLOC15                                                         
         CLI   RFLSEQ,0            MAIN  PROFILE                                
         BNE   DISLOCSX                                                         
         CLI   RFLTYPE,RFLLOCS     MATCH ON   LOCATION STATUS ?                 
         BE    DISLOC20                                                         
*                                                                               
DISLOC15 DS    0H                  FIND  NEXT ELEMENT                           
         IC    RF,RFLLN                                                         
         AR    R2,RF                                                            
         B     DISLOC10                                                         
*                                                                               
DISLOC20 DS    0H                                                               
         L     R5,ACLOSTAB         LOCATION   STATUS TABLE                      
*                                                                               
DISLOC25 DS    0H                                                               
         CLI   0(R5),EOT           END   OF   TABLE ?                           
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   RFLDATA(1),0(R5)    FOUND MATCH ?                                
         BE    DISLOC30            YES,  CONTINUE                               
*                                                                               
         LA    R5,LLOSTATU+1(,R5)  NEXT  TABLE     ENTRY                        
         B     DISLOC25            TRY   AGAIN                                  
*                                                                               
DISLOC30 DS    0H                  FOUND MATCH                                  
         MVC   LOSTATUS,1(R5)      MOVE  LOCATION STATUS TO WORK AREA           
         CLI   LOSTATUS,ESCHIGHQ   LOCATION STATUS A CONSTANT ?                 
         BNL   DISLOC35            YES,  SKIP                                   
*                                  GET   LOCATION STATUS VALUE                  
         GOTO1 VDICTAT,APPARM,C'SU  ',('LLOSTATU',LOSTATUS),0                   
*                                                                               
DISLOC35 DS    0H                                                               
         LA    R1,8(,R6)           ->    FIELD     DATA                         
         TM    RFLIND,RFLXCLD      EXCLUDE    BIT  ON ?                         
         BZ    DISLOC40            NO,   SKIP                                   
         MVI   8(R6),C'*'          INSERT     '*'                               
         LA    R1,9(,R6)                                                        
*                                                                               
DISLOC40 DS    0H                  INSERT LOCATION STATUS                       
         MVC   0(LLOSTATU,R1),LOSTATUS                                          
*                                                                               
DISLOCSX XIT                       EXIT                                         
         DROP  R2                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* CHKLIST - CHECK FOR OFFICE GROUP, MEDIA, ...                        *         
*           PASS (TYPE,FLDHDR) TO PARM 1                              *         
***********************************************************************         
         SPACE 1                                                                
CHKLIST  NMOD1 0,**CHKL**                                                       
         L     RC,APALOCAL                                                      
         LR    R3,R1                                                            
         L     R6,0(,R1)           GET ADDRESS OF FIELD HEADER                  
         GOTO1 AFVAL,(R6)                                                       
         BNE   CHKCCEQ                                                          
         LA    R6,FVIFLD                                                        
         CLI   FVIFLD,C'('         STRING LIST                                  
         BE    CHKCCEQ                                                          
         L     R2,=A(CHKTABL)                                                   
         A     R2,APRELO                                                        
         CLI   FVIFLD,C'*'                                                      
         BNE   CHKL005                                                          
         LA    R6,FVIFLD+1                                                      
         SR    R1,R1                                                            
         IC    R1,FVILEN           DECREASE LENGTH BY ONE                       
         BCTR  R1,R0                                                            
         STC   R1,FVILEN                                                        
         BCTR  R1,R0                                                            
         STC   R1,FVXLEN                                                        
*                                                                               
         USING CHKTABD,R2          MATCH FIELD NUMBER                           
CHKL005  CLI   CHKTYPE,EOT                                                      
         BE    CHKCCNE                                                          
         CLC   CHKTYPE,0(R3)                                                    
         BE    CHKL010                                                          
         LA    R2,CHKTABLN(R2)                                                  
         B     CHKL005                                                          
*                                                                               
CHKL010  CLC   FVILEN,CHKMLN       MAKE SURE LENGTHS ARE SAME                   
         BH    CHKL090                                                          
*                                                                               
CHKL030  XC    IOKEY,IOKEY                                                      
         TM    CHKFIND,CHKFSPC     Space filled key?                            
         BZ    *+10                No                                           
         MVC   IOKEY,SPACES        Yes                                          
*                                                                               
         MVC   IOKEY(L'CHKKEY),CHKKEY        BUILD KEY                          
         ZIC   RE,CHKCPY                                                        
         LA    RE,IOKEY(RE)        ->   TO   COMPANY CODE LOCATION              
         MVC   0(1,RE),CUABIN                                                   
         ZIC   RE,CHKDSP                                                        
         LA    RE,IOKEY(RE)        ->   DATA              LOCATION              
         ZIC   R1,FVXLEN           LENGTH OF FIELD                              
         EX    R1,*+4                                                           
         MVC   0(0,RE),0(R6)                                                    
         LA    R1,IOHIGH+IOACCFIL+IO2                                           
         GOTO1 AIO                                                              
         BNE   CHKL090             NO   GOOD, SHOW INPUT                        
         L     RF,AIOAREA2         CHECK IF KEYS ARE THE SAME                   
         CLC   IOKEY(ACCORLEN),0(RF)                                            
         BE    CHKCCEQ                                                          
*                                                                               
CHKL090  ZIC   R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   FVXTRA(0),0(R6)                                                  
         B     CHKCCNE                                                          
*                                                                               
CHKCCEQ  SR    RE,RE                                                            
*                                                                               
CHKCCNE  LTR   RE,RE                                                            
         XIT1                                                                   
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE TRANSACTION TYPE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD DSECT                           
VALTTYP  NMOD1 0,**TTYP**                                                       
         L     RC,APALOCAL                                                      
         L     R3,0(,R1)           DATA FIELD     HEADER                        
         L     R4,4(,R1)           REQUEST   CARD AREA                          
         CLI   FVILEN,0            ANY  DATA ?                                  
         BE    VALTTYX             NO,  EXIT                                    
         CLI   FVIFLD,C'('         PROFILE   LIST ?                             
         BNE   VALTTY05            NO,  SKIP                                    
         CLI   RECAP#,0            RECAPPING ?                                  
         BE    VALTTYX             NO,  LEAVE     REQUEST   CARD BLANK          
         TM    RCAPSW,RCAPPROF     FOUND     USE  RECAP'S   PROFILE ?           
         BZ    VALTTYX             NO,  LEAVE     REQUEST   CARD BLANK          
*                                  INSERT    (T                                 
         GOTOR GDEFTEXT,APPARM,(R4),RR=APRELO                                   
         B     VALTTYX             RETURN                                       
*                                                                               
VALTTY05 DS    0H                  NOT  '('  IN   1ST  BYTE                     
         MVC   DUMTRNTH,0(R3)      PICK UP   THE  DUMMY     FLD  HEADER         
         MVC   DUMTRNT,SPACES      CLEAR     THE  DUMMY     FIELD               
         LA    RE,FVIFLD           ->   DATA FIELD                              
         ZIC   RF,FVXLEN           GET  DATA LENGTH                             
         CLI   FVIFLD,C'*'         EXCLUDE REQUESTED ?                          
         BNE   VALTTY10            NO,  SKIP                                    
         STC   RF,DUMTRNTH+5       SAVE FIELD LENGTH WITHOUT THE '*'            
         LA    RE,1(,RE)           YES, ADD 1 TO DATA START                     
         AHI   RF,-1               SUBTRACT ONE FROM LENGTH                     
         BM    VALTTYER            ONLY C'*', INVALID TRANSACTION TYPE          
*                                                                               
VALTTY10 DS    0H                                                               
         EX    RF,*+4                                                           
         MVC   DUMTRNT(0),0(RE)    MOVE TRANSACTION TYPE                        
*                                  SET  UP SCANNER BLOCK                        
         GOTO1 VSCANNER,APPARM,DUMTRNTH,(2,BLOCK),SCNP3NEQ                      
         MVC   NPARMS,APPARM+4     GET  NUMBER OF FIELDS FOUND                  
         CLI   NPARMS,1            MORE THAN ONE PARAMETER ?                    
         BNE   VALTTYER            YES, INVALID TRANSACTION TYPE                
*                                                                               
*                                  CONVERT TRANSACTION TYPE TO A NUMBER         
         GOTO1 CNVTTYPE,APPARM,(C'N',BLOCK),(NPARMS,BYTE)                       
         BNE   VALTTYER            NOT  GOOD, INVALID TRANSACTION TYPE          
*                                                                               
         CLI   BYTE,TY06MN         IS   IT MANUAL BILLING TYPE ?                
         BNE   VALTTY20            NO,  SKIP                                    
         MVC   0(3,R4),=C'M  '     INSERT SPECIAL CODE                          
         B     VALTTY80            COMPLETE THE GOOD TRANSACTION                
*                                                                               
VALTTY20 DS    0H                                                               
         ZIC   RE,BYTE             GET  TRANSACTION TYPE                        
         CVD   RE,DWORK            CONVERT TO DECIMAL                           
         UNPK  WORK(3),DWORK+6(2)  UNPACK IT                                    
         OI    WORK+2,C'0'         FIX  SIGN BYTE                               
         MVC   0(3,R4),WORK        FILL IN NNN                                  
*                                                                               
VALTTY80 DS    0H                                                               
         CLI   FVIFLD,C'*'         EXCLUDE TRANS. TYPE REQUESTED ?              
         BNE   VALTTYX                                                          
         NI    0(R4),TURNOFF-X'40' MAKE SMALL CASE LETTER                       
         B     VALTTYX                                                          
*                                                                               
VALTTYER DS    0H                  INVALID TRANSACTION TYPE                     
         MVC   FVMSGNO,=AL2(ACEIVTY)                                            
*                                                                               
VALTTYX  DS    0H                                                               
         XIT1                      EXIT                                         
         DROP  R2                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALLST - READ LIST TO VALIDATE AND GET NAME                        *         
*                                                                     *         
*  ON INPUT:                                                          *         
*     P1  BYTE  0   = FIELD NUMBER                                    *         
*         BYTES 1-3 = HEADER FIELD FOR POSSIBLE LIST NAME             *         
*     P2  BYTES 0-3 = ADDRESS OF HEADER FIELD FOR NEXT (DETAIL) FIELD *         
*                     NAME OR A(0) IF NO DETAIL FIELD                 *         
*                                                                     *         
*  USES:                                                              *         
*     APBYTE   = SAVE AREA FOR FIELD NUMBER                           *         
*     LEDGLIST = CORRELATES FIELD NUMBER DATA WITH VALID LEDGERS      *         
*     REPTABD  = REPORT TYPE DEFINITION DSECT                         *         
*                                                                     *         
*  CALLS:                                                             *         
*     AFVAL    - FOR LIST NAME                                        *         
*     AIO      - FOR LIST RECORD (USES IOAREA2)                       *         
*                                                                     *         
*  ON EXIT:                                                           *         
*     THE FIRST BYTE OF THE PARAMETER LIST:                           *         
*       0 - VALID LIST                                                *         
*       1 - NOT A LIST                                                *         
*       2 - BAD   LIST                                                *         
*       3 - NO    INPUT                                               *         
*       4 - NOT   VALID INPUT                                         *         
*                                                                     *         
*     IF P1 = 0, THEN THE LIST NAME (TEXT) IS MOVED TO THE DETAIL AREA*         
*                                                                     *         
*  NOTES:                                                             *         
*     KEEP CHANGES TO THIS ROUTINE IN SYNC WITH THE VALLIST ROUTINE   *         
*     IN ACSCR00.                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALLST   NMOD1 0,**VLST**                                                       
         L     RC,APALOCAL                                                      
         L     R3,0(,R1)           POSSIBLE     LIST DATA                       
         L     R4,4(,R1)           NAME    FIELD     (DETAIL   FIELD)           
         MVC   APBYTE,0(R1)        SAVE    FIELD     NUMBER                     
         LR    R6,R1               SAVE    THE  PARM LIST                       
         GOTO1 AFVAL,(R3)          VALID   INPUT     ?                          
         MVI   0(R6),3             NO      INPUT                                
         BL    VALLST98            NO      INPUT,    EXIT                       
         MVI   0(R6),4             NOT     VALID     INPUT                      
         BH    VALLST98            NOT     VALID     INPUT,    EXIT             
         CLI   FVILEN,1                                                         
         BE    VALLST02                                                         
         CLI   FVIFLD,C'+'         INCLUDE LIST TYPE ?                          
         BE    VALLST05            YES,    CONITNUE                             
         CLI   FVIFLD,C'-'         EXCLUDE LIST TYPE ?                          
         BE    VALLST05            YES,    CONTINUE                             
VALLST02 MVI   0(R6),1             NOT     A    LIST                            
         B     VALLST98            NOT     A    LIST,     EXIT                  
*                                                                               
         USING LSTRECD,R3                                                       
VALLST05 MVI   0(R6),2             BAD     LIST                                 
         CLI   FVXLEN,5            TOO     LONG  FOR ONE  LIST ITEM ?           
         BH    VALLST98            YES,    EXIT                                 
         CLI   FVXLEN,0            TOO     SHORT FOR ONE  LIST ITEM ?           
         BE    VALLST98            YES,    EXIT                                 
         LA    R3,IOKEY                                                         
         MVC   LSTKEY,SPACES                                                    
         MVI   LSTKTYP,LSTKTYPQ    X'1D'                                        
         MVC   LSTKCPY,CUABIN      COMPANY CODE                                 
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         AHI   RF,-1               LESS    THE  C"+" OR   C"-"                  
         BM    VALLST98                                                         
         EX    RF,*+4                                                           
         MVC   LSTKLST(0),FVIFLD+1                                              
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   VALLST98                                                         
*                                                                               
         USING LITELD,R3                                                        
         L     R3,AIOAREA2         FIND LIST TYPE ELEMENT                       
         AH    R3,DATADISP                                                      
*                                                                               
VALLST10 DS    0H                                                               
         CLI   LITEL,0                                                          
         BE    VALLST98            INVALID LIST REC                             
         CLI   LITEL,LITELQ                                                     
         BE    VALLST15                                                         
         SR    RF,RF                                                            
         IC    RF,LITLN                                                         
         AR    R3,RF                                                            
         B     VALLST10                                                         
*                                                                               
VALLST15 DS    0H                                                               
         CLI   LITTYPE,LITTWRK     TEST WORKCODE TYPE LIST                      
         BNE   VALLST20                                                         
         CLI   APBYTE,FLDNWKCD     TEST WORKCODE REQUESTED                      
         BE    VALLST80            YES - GOOD LIST                              
         B     VALLST98            NO - EXIT                                    
*                                                                               
VALLST20 DS    0H                                                               
         MVC   BYTE,LITTYPE        ELSE SAVE LIST TYPE                          
*                                                                               
         USING LIDELD,R3                                                        
         L     R3,AIOAREA2         FIND LIST DATA ELEMENT                       
         AH    R3,DATADISP                                                      
*                                                                               
VALLST30 DS    0H                                                               
         CLI   LIDEL,0                                                          
         BE    VALLST98            EOR                                          
         CLI   LIDEL,LIDELQ        TEST LIST DATA ELEMENT                       
         BNE   VALLST75                                                         
*                                                                               
         CLI   APBYTE,FLDNCNTR     CONTRA  ACCOUNT ?                            
         BE    VALLST80            YES,    FOR  NOW, ASSUME   GOOD LIST         
*                                                                               
         LA    RE,LIDDLEDG-LIDELD                                               
*                                                                               
VALLST35 DS    0H                                                               
         L     RF,=A(LEDGLIST)                                                  
         CLI   APBYTE,FLDNACCT     ACCOUNT FIELD   ?                            
         BNE   VALLST50            NO,     GO   PROCESS  BILLING SOURCE         
*                                                                               
         L     RF,ACTYPTAB         THIS    IS   BY   REPORT TYPE                
         CLI   MULTIUL,NO          ARE     THERE     MULTIPLE LEDGERS ?         
         BNE   VALLST40            YES,    BRANCH                               
         CLC   LIDDLEDG,APREPUL    DEFAULT LEDGER ?                             
         BE    VALLST80            YES,    GOOD LIST                            
         B     VALLST70            NO,     TRY  NEXT ELEMENT                    
*                                                                               
         USING REPTABD,RF          REPORT TYPE DEFINITION DSECT                 
VALLST40 CLI   0(RF),EOT           END     OF   TABLE     ?                     
         BE    VALLST70            NOT     FOUND,    TRY  NEXT ELEMENT          
         CLI   REPNUM,C'M'         MULTIPLE     ENTRY     ?                     
         BE    VALLST45            YES,    SKIP THIS ENTRY                      
         CLC   APREPJCL,REPJCLID   REPORT  JCL  ID   MATCH     ?                
         BNE   VALLST45            NO,     SKIP THIS ENTRY                      
         CLC   LIDDLEDG,REPUL      LEDGER  ALSO MATCHES   ?                     
         BE    VALLST80            YES,    GOOD LIST                            
*                                                                               
VALLST45 DS    0H                                                               
         LA    RF,REPLNQ(,RF)      BUMP    TO   NEXT ENTRY                      
         B     VALLST40            LOOP    BACK                                 
         DROP  RF                                                               
*                                                                               
VALLST50 DS    0H                  PROCESS BILLING   SOURCE                     
         A     RF,APRELO           ADD     REALLOCATION   FACTOR                
         CLI   APBYTE,FLDNPART     PARTICIPANT  ?                               
         BNE   VALLST60            NO,     SKIP                                 
         CLI   LIDDLEDG,C'3'       UNIT    CODE =    3    ?                     
         BE    VALLST80            YES,    GOOD LIST                            
         B     VALLST60            NO,     TRY  NEXT                            
*                                                                               
VALLST55 CLI   APBYTE,FLDNBSRC     BILLING SOURCE    ?                          
         BNE   VALLST60            NO,     SKIP                                 
         CLC   LIDDLEDG,=C'ME'     UNIT/LEDGER  =    'ME' ?                     
         BE    VALLST80            YES,    GOOD BILLING   SOURCE   LIST         
*                                                                               
VALLST60 CLI   0(RF),X'FF'         END     OF   TABLE     ?                     
         BE    VALLST70            NOT     FOUND,    TRY  NEXT ELEMENT          
         CLC   APBYTE,0(RF)        FIELD   NUMBER    MATCH     ?                
         BNE   VALLST65            NO,     SKIP                                 
         CLC   LIDDLEDG,1(RF)      AND     UNIT/LEDGER    MATCH    ?            
         BE    VALLST80            YES,    GOOD LIST                            
*                                                                               
VALLST65 LA    RF,3(,RF)           NEXT    IN   TABLE                           
         B     VALLST55                                                         
*                                                                               
VALLST70 DS    0H                                                               
         CLI   BYTE,LITTLDG        TEST LEDGER LIST                             
         BNE   VALLST75            NO                                           
         LA    RE,L'LIDDLEDG(,RE)  BUMP TO NEXT UNIT/LEDGER                     
         CLM   RE,1,LIDLN          TEST END OF EL                               
         BL    VALLST35                                                         
*                                                                               
VALLST75 DS    0H                  FIND NEXT LIDEL ELEMENT                      
         SR    RF,RF                                                            
         IC    RF,LIDLN                                                         
         AR    R3,RF                                                            
         B     VALLST30                                                         
*                                                                               
VALLST80 MVI   0(R6),0             GOOD    LIST                                 
         LTR   R4,R4               FIELD   NAME (DETAIL) ?                      
         BZ    VALLST98            NO,     SKIP                                 
*                                  LIST    NAME (TEXT)   TO DETAIL AREA         
         GOTO1 GETNAME,TMPPARM,AIOAREA2,(R4)                                    
                                                                                
         CLI   BYTE,LITTACT        TEST ACCOUNT LIST                            
         BNE   VALLST98                                                         
         CLC   LIDDLEDG,APREPUL    TEST LIST LEDGER MATCHES REQUEST             
         BNE   VALLST98                                                         
L        USING ACTRECD,IOKEY                                                    
         MVC   L.ACTKEY,SPACES       READ ACCOUNT RECS IN LIST                  
         MVC   L.ACTKCPY,CUABIN                                                 
         MVC   L.ACTKUNT(2),LIDDLEDG                                            
         LA    R8,LIDDACCS         R8=A(CURRENT LIST ITEM)                      
VALLST82 SR    RF,RF                                                            
         IC    RF,LIDITLN                                                       
         AHI   RF,-1                                                            
         MVC   L.ACTKACT(0),0(R8)                                               
         EX    RF,*-6                                                           
         DROP  L                                                                
         GOTO1 AIO,IORD+IOACCFIL+IO3                                            
         BNE   VALLST98                                                         
*                                                                               
         USING ABLELD,R1                                                        
         L     R1,AIOAREA3                                                      
         AH    R1,DATADISP                                                      
         SR    R0,R0                                                            
VALLST84 CLI   ABLEL,0                                                          
         BE    VALLST94                                                         
         CLI   ABLEL,ABLELQ        FIND BALANCE EL (=LOW-LVL A/C)               
         BNE   VALLST86                                                         
         CLI   ABLLN,ABLLN3Q                                                    
         BL    VALLST94            SAFETY:SHOULDN'T HAPPEN                      
         ICM   RE,15,ABLTXS                                                     
         B     VALLST90                                                         
         USING NUMELD,R1                                                        
VALLST86 CLI   NUMEL,NUMELQ        ELSE FIND NUMEL (=HI-LVL A/C)                
         BNE   VALLST88                                                         
         CLI   NUMTYPE,NUMTYHIQ                                                 
         BNE   VALLST88                                                         
         ICM   RE,15,NUM#TRX                                                    
         B     VALLST90                                                         
*                                                                               
VALLST88 IC    R0,NUMLN                                                         
         AR    R1,R0                                                            
         B     VALLST84                                                         
*                                                                               
VALLST90 ICM   RF,15,ACAC#TRX                                                   
         AR    RF,RE                                                            
         STCM  RF,15,ACAC#TRX      ACCUMULATE TRANSACTION COUNT                 
*                                                                               
VALLST94 SR    RF,RF               BUMP TO NEXT LIDEL ITEM                      
         IC    RF,LIDITLN                                                       
         AR    R8,RF               R8=NEXT ACCOUNT IN LIDEL                     
         IC    RF,LIDLN            CALC/TEST END OF ELEMENT                     
         AR    RF,R3               RF=END OF LIDEL                              
         CR    R8,RF                                                            
         BL    VALLST82            READ NEXT ACCOUNT, THIS LIDEL                
         B     VALLST75            LOOK FOR NEXT LIDEL                          
*                                                                               
VALLST98 XIT1                                                                   
         DROP  R1,R3                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  FILLFLD - INSERT ELEMENT DATA                                      *         
*                                                                     *         
*  ON INPUT:                                                          *         
*     PARM LIST:                                                      *         
*        P1  = A(IO AREA OF RECORD)                                   *         
*        P2  = AL1(FIELD TYPE # OF X'C5'ELEMENT)                      *         
*              A(HEADER FIELD TO FILL)                                *         
*        P3  = A(HEADER FIELD FOR NEXT (DETAIL) FIELD NAME) OR A(0)   *         
*        P4  = A(PROFILE NUMBER FOR GETTEXT)                          *         
*                                                                     *         
* ON EXIT:                                                            *         
*    THE FIRST BYTE OF P1:                                            *         
*        00 = NOT FOUND                                               *         
*        01 = FOUND AN ITEM                                           *         
*        02 = FOUND A LIST                                            *         
*        03 = FOUND MULTIPLE ITEMS OR LISTS                           *         
***********************************************************************         
         SPACE 1                                                                
FILLFLD  NMOD1 0,**FILL**                                                       
         L     RC,APALOCAL                                                      
         L     R2,0(,R1)           A(IO AREA)                                   
         ICM   R4,15,8(R1)         CLEAR NAME FIELD IN ANY                      
         BZ    FILL10              NOTHING TO PUT                               
         SR    RF,RF                                                            
         IC    RF,0(,R4)           GET LENGTH OF FIELD                          
         AHI   RF,-9               MINUS FIELD HEADER                           
         TM    1(R4),FVAXTND       EXTENDED HEADER?                             
         BZ    *+8                                                              
         AHI   RF,-8               MINUS EXTENDED FIELD HEADER                  
         EX    RF,*+4                                                           
         MVC   8(0,R4),SPACES      CLEAR FIELD                                  
*                                                                               
FILL10   L     R4,4(,R1)           A(FIELD HEADER)                              
         LA    R4,0(,R4)           CLEAR HOB                                    
         MVC   TYPEC5,4(R1)        SAVE TYPE # FOR X'C5' ELEMENT                
         XC    0(4,R1),0(R1)                                                    
         MVI   EXTFLD,NO                                                        
*                                                                               
         USING RFLELD,R2                                                        
         SR    R6,R6                                                            
         AH    R2,DATADISP                                                      
*                                                                               
FILL20   IC    R6,RFLLN                                                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    FILL90                                                           
         CLI   0(R2),RFLELQ        X'C5' FILTER ELEMENT                         
         BNE   FILL25                                                           
         CLI   RFLSEQ,0            HIGH LEVEL FILTER?                           
         BNE   FILL25              NO , COLUMN FILTER (SKIP)                    
         CLC   RFLTYPE,TYPEC5      MATCH TYPE                                   
         BE    FILL40                                                           
*                                                                               
FILL25   AR    R2,R6                                                            
         B     FILL20                                                           
*                                  ************************************         
FILL40   DS    0H                  * GOT X'C5' FILTER ELEMENT         *         
*                                  ************************************         
         AHI   R6,-(RFLLNQ+1)      EXMVC LENGTH                                 
         TM    RFLIND,RFLXCLD      SHOW EXCLUDE?                                
         BZ    *+8                                                              
         LA    R6,1(,R6)           ADD ONE FOR C'*'                             
         MVI   0(R1),3             MARK AS MULTIPLE ACCOUNTS                    
         CLI   RFLTYPE,RFLTTYPE    TRANS TYPE?                                  
         BNE   FILL42                                                           
*                                                                               
         BAS   RE,FILLTTY          R3=NEW EXMVC LENGTH, R2=NEW ELEMENT          
*                                                                               
         LR    R6,R3               NEW LENGTH                                   
         BH    FILL50              MARK AS MULTIPLE TYPES                       
         BE    FILL90              NO OUTPUT                                    
*                                  ************************************         
FILL42   SR    RF,RF               * DETERMINE DATA LENGTH + IS IT    *         
*                                  *   ONE ELEMENT OR                 *         
*                                  *   ONE LIST    OR                 *         
*                                  *   MULTIPLE ELEMENTS OR LISTS     *         
*                                  * R6 = EXMVC LENGTH OF DATA        *         
*                                  * RF = FIELD SIZE                  *         
*                                  ************************************         
         IC    RF,0(,R4)           GET LENGTH OF FIELD                          
         SHI   RF,8                MINUS FIELD HEADER                           
         TM    1(R4),FVAXTND       EXTENDED HEADER?                             
         BZ    *+8                                                              
         SHI   RF,8                MINUS EXTENDED FIELD HEADER                  
         LR    R3,RF                                                            
         BCTR  R3,0                SUBTRACT ONE FOR EXECUTE                     
         EX    R3,*+4                                                           
         MVC   8(0,R4),SPACES      CLEAR FIELD                                  
         CR    R6,RF                                                            
         BNL   FILL50              TOO BIG A FIELD                              
*                                                                               
         LR    RF,R6               NOW RF BECOMES EXMVC DATA LENGTH             
         TM    RFLIND,RFLXCLD      EXCLUDE ON IN RECORD ?                       
         BZ    *+6                                                              
         BCTR  RF,0                                                             
         LTR   RF,RF               If zero length is 1                          
         BZ    FILL70              Not a list if one character                  
         BM    FILL90              No data                                      
*                                                                               
FILL45   LA    RE,RFLDATA(RF)      POINT TO END OF DATA                         
         CLC   SCCOMMA,0(RE)       LOOK FOR DELIMITER (COMMA)                   
         BE    FILL50              ONLY ONE FIELD ALLOWED                       
         BCT   RF,FILL45           LOOP                                         
         B     FILL60                                                           
*                                  ************************************         
FILL50   DS    0H                  * TOO BIG OR MULTIPLE ITEMS/LISTS  *         
*                                  ************************************         
         ICM   RF,15,12(R1)        GET PROFILE TEXT #                           
         BZ    FILL51                                                           
         LR    R3,R1               SAVE A(PARM LIST)                            
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
*                                  INSERT TEXT FIELD ONTO SCREEN                
         GOTO1 TEXTGET,TMPPARM,(RF),(R4),0                                      
         LR    R1,R3               RESTORE A(PARM LIST)                         
*                                                                               
FILL51   ICM   R4,15,8(R1)         HEADER FIELD FOR DETAIL                      
         BZ    FILL90              NOTHING TO PUT                               
         SR    RF,RF                                                            
         IC    RF,0(,R4)           GET LENGTH OF FIELD                          
         AHI   RF,-8               MINUS FIELD HEADER                           
         TM    1(R4),FVAXTND       EXTENDED HEADER?                             
         BZ    *+8                                                              
         AHI   RF,-8               MINUS EXTENDED FIELD HEADER                  
         BCTR  RF,0                MINUS ONE FOR POSSIBLE EXECUTE               
         CR    R6,RF               RF=LENGTH OF FIELD VS FIELD SIZE             
         BNH   FILL80              TOO BIG A FIELD?                             
         MVI   EXTFLD,YES          YES                                          
         LR    R6,RF               USE RF INSTEAD                               
         B     FILL80                                                           
*                                                                               
*                                  ************************************         
FILL60   DS    0H                  * ONE ITEM OR ONE LIST             *         
*                                  ************************************         
         MVI   0(R1),2             MARK AS A LIST                               
         CLI   RFLDATA,C'+'        IS IT A INCLUDE LIST TYPE                    
         BE    FILL62                                                           
         CLI   RFLDATA,C'-'        IS IT A EXCLUDE LIST TYPE                    
         BNE   FILL70                                                           
*                                  ************************************         
FILL62   DS    0H                  * ONE LIST                         *         
*                                  ************************************         
         USING LSTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   LSTKEY,SPACES                                                    
         MVI   LSTKTYP,LSTKTYPQ    X'1D'                                        
         MVC   LSTKCPY,CUABIN      COMPANY CODE                                 
         LR    RF,R6                                                            
         BCTR  RF,0                LESS THE C"+" OR C"-"                        
         EX    RF,*+4                                                           
         MVC   LSTKLST(0),RFLDATA+1                                             
         LR    R8,R4               SAVE CURRENT FIELD                           
*                                  ************************************         
*                                  *** IN THIS CASE R4 CHANGES TO BE            
*                                  *** THE ADDRESS OF THE DETAIL FIELD          
*                                  ************************************         
         ICM   R4,15,8(R1)         NAME FIELD                                   
         BZ    FILL65                                                           
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   FILL65                                                           
*                                  LIST NAME (TEXT) TO DETAIL AREA              
         GOTO1 GETNAME,TMPPARM,AIOAREA2,(R4)                                    
*                                                                               
FILL65   LR    R4,R8               RESTORE DATA FIELD                           
         B     FILL80                                                           
         DROP  R3                                                               
*                                  ************************************         
FILL70   DS    0H                  * ONE ITEM                         *         
*                                  ************************************         
         MVI   0(R1),1             MARK AS AN ACCOUNT                           
         CLI   RFLTYPE,RFLACC                                                   
         BNE   FILL80                                                           
         CLI   APMLDGR,YES                                                      
         BNE   FILL80                                                           
         L     RF,AFLDUNLG         POINT TO U/L HEADER FIELD                    
         A     RF,ATWA                                                          
         CLI   MULTIUL,YES         HAVE MULTIPLE UNIT LEDGERS                   
         BE    *+10                                                             
         CLC   RFLDATA(2),8(RF)    USER OVER-RIDE INPUT?                        
         BNE   FILL90              YES, SO DON'T FILL IN ACCOUNT                
         MVC   8(2,RF),RFLDATA                                                  
         OI    6(RF),FVOXMT        TRANSMIT FIELD                               
         TM    RFLIND,RFLXCLD      EXCLUDE DATA TYPE?                           
         BZ    FILL75              NO,  SKIP                                    
         AHI   R6,-3               SUBTRACT 1 FOR "*" AND 2 FOR U/L             
         BM    FILL78                                                           
         MVI   8(R4),C'*'          YES, SHOW AS EXCLUDE                         
         EX    R6,*+4                                                           
         MVC   9(0,R4),RFLDATA+2   MOVE DATA TO SCREEN                          
         LA    RF,2(,R6)           GET  REAL LENGTH                             
         STC   RF,7(,R4)           SAVE IN FIELD HEADER                         
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         B     FILL90                                                           
*                                                                               
FILL75   DS    0H                                                               
         AHI   R6,-2               SUBTRACT 2 FOR UNIT/LEDGER                   
         BM    FILL78                                                           
         EX    R6,*+4                                                           
         MVC   8(0,R4),RFLDATA+2   MOVE DATA TO SCREEN                          
         LA    RF,1(,R6)           GET  REAL LENGTH                             
         STC   RF,7(,R4)           SAVE IN FIELD HEADER                         
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         B     FILL90                                                           
*                                                                               
FILL78   MVI   0(R1),0                                                          
         B     FILL90                                                           
*                                  ************************************         
FILL80   DS    0H                  * MOVE DATA TO SCREEN              *         
*                                  ************************************         
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         LA    R8,8(,R4)           POINT PAST FIELD HEADER                      
         LR    RF,R6               SAVE  EXMVC LENGTH                           
         TM    RFLIND,RFLXCLD      EXCLUDE DATA TYPE?                           
         BZ    FILL88                                                           
         MVI   0(R8),C'*'          SHOW AS EXCLUDE                              
         AHI   RF,-1               SUBTRACT ONE                                 
         BM    FILL90                                                           
         LA    R8,1(,R8)           BUMP UP BECAUSE OF C'*'                      
*                                                                               
FILL88   EX    RF,*+4                                                           
         MVC   0(0,R8),RFLDATA     MOVE IN DATA                                 
         LA    RF,1(,R6)           GET  REAL LENGTH                             
         STC   RF,7(,R4)           SAVE IN FIELD HEADER                         
         CLI   EXTFLD,YES                                                       
         BNE   FILL90                                                           
         LA    RE,8(R6,R4)                                                      
         MVI   0(RE),C'>'          INDICATE TRUNCATED DATA                      
*                                                                               
FILL90   XIT1                                                                   
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* FILL IN THE TRANSACTION TYPE                                        *         
*                                                                     *         
* CONCODE   MEANS              EXIT CONDITION CODE                    *         
*    0      SINGLE   ELEMENT        LOW                               *         
*    1      NO DATA                 EQUAL                             *         
*    2      MULTIPLE ELEMENTS       HIGH                              *         
*                                                                     *         
* ON EXIT:                                                            *         
*    R2 = ADDRESS OF DUMMY ELEMENT                                    *         
*    R3 = EXMVC LENGTH OF ELEMENT (WITHIN THE MODULE R1 HAS EXMVC LNG)*         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2                                                        
FILLTTY  NTR1  ,                                                                
         MVI   CONCODE,1           NO DATA                                      
         MVC   APWORK,SPACES                                                    
         GOTO1 CNVTTYPE,APPARM,(C'S',(R2)),APWORK                               
         MVC   APELEM(RFLLNQ),RFLEL                                             
         LA    R1,40               MAX  FIELD SIZE                              
         LA    R8,APWORK-1(R1)     POINT TO THE LAST BYTE OF THE FIELD          
*                                                                               
FILLTY05 DS    0H                                                               
         CLI   0(R8),C' '          IS   THIS BYTE BLANK ?                       
         BH    FILLTY10            NO,  WE GOT THE FIELD LENGTH !               
         BCTR  R8,0                YES, SUBTRACT ONE AND                        
         BCT   R1,FILLTY05         TRY  THE PREVIOUS CHARACTER                  
         B     FILLTY90            SET  CC = EQUAL - NO DATA FOUND              
*                                                                               
NEW      USING RFLELD,R3                                                        
*                                                                               
FILLTY10 LA    R3,APELEM           CREATE NEW ELEMENT                           
         LA    RF,APWORK                                                        
         CLI   APWORK,C'*'         WAS IT EXCLUDE                               
         BNE   FILLTY15                                                         
         LA    RF,APWORK+1                                                      
         BCTR  R1,0                                                             
*                                  R1   IS THE REAL LENGTH                      
FILLTY15 DS    0H                  RF   POINTS TO DATA WITH NO "*"              
         BCTR  R1,0                GET  MOVE LENGTH                             
         EX    R1,*+4                                                           
         MVC   NEW.RFLDATA(0),0(RF)     INSERT INTO DUMMY RCD AREA              
         LA    RF,RFLLNQ+1(,R1)    GET  LENGTH OF DUMMY RECORD                  
         STC   RF,NEW.RFLLN        SAVE LENGTH IN DUMMY RECORD                  
*                                                                               
         DROP  NEW                                                              
*                                  BACK TO THE ORIGINAL INPUT                   
         MVI   CONCODE,2                                                        
         CLI   RFLLN,RFLLNQ+1      ALLOW ONLY ONE                               
         BH    FILLTY90            MULTIPLE, SKIP                               
*        CLI   RFLDATA,TY30DI      IS SPECIAL TYPE ?                            
*        BNL   FILLTY90            YES, TREAT AS MULTIPLE                       
         MVI   CONCODE,0           SET TO SINGLE                                
*                                                                               
FILLTY90 LA    R2,APELEM           USE NEW ELEMENT                              
         LR    R3,R1                                                            
         TM    RFLIND,RFLXCLD                                                   
         BZ    *+8                                                              
         LA    R3,1(,R3)                                                        
         CLI   CONCODE,1           SET CON CODE                                 
         XIT1  REGS=(R2,R3)        BE = NO DATA, BL = SINGLE, BH = MULT         
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY OPTION LIST ON SCREEN                                      *         
***********************************************************************         
         SPACE 1                                                                
VALOPT   NMOD1 0,**VOPT**                                                       
         L     RC,APALOCAL                                                      
         L     R3,0(,R1)           LOAD  ADDRESS   OF   FIELD                   
         L     R4,4(,R1)           LOAD  ADDRESS   OF   NAME FIELD              
         MVC   OPTIDNUM,0(R1)      SAVE  ID   OPTION    FIELD                   
         MVC   FVMSGNO,=AL2(FVFNOTV)     NOT  VALID     INPUT                   
         MVC   FVXTRA,SPACES                                                    
         LA    R5,FVXTRA           BUILD LIST OF   VALID     OPTIONS            
*                                                                               
         SR    RF,RF                                                            
         L     R2,=A(ADOPTTAB)     ALTERNATE  DATE OPTIONS   TABLE              
         A     R2,APRELO                                                        
*                                                                               
         USING ADOPTD,R2           ALTERNATE  DATE OPTIONS   DSECT              
VALOPT02 DS    0H                                                               
         CLI   0(R2),EOT           END   OF   TABLE ?                           
         BE    VALOPT10            VALID OPTION    LIST BUILT                   
         LA    R6,ADORTYPE         ->    LIST OF   REPORT    TYPES              
         LA    R0,L'ADORTYPE       MAX   NUM  OF   REPORT    TYPES              
*                                                                               
VALOPT05 DS    0H                                                               
         CLI   0(R6),C' '          END   OF   REPORT    TYPES ?                 
         BE    VALOPT08            YES,  GET  NEXT OPTION                       
         CLC   APREPJCL,0(R6)      MATCH REPORT    TYPE ?                       
         BE    VALOPT06            YES,  SKIP                                   
         LA    R6,1(,R6)           GET   NEXT REPORT    TYPE                    
         BCT   R0,VALOPT05         LOOP  FOR  NEXT REPORT    TYPE               
         B     VALOPT08            ENTRY NOT  VALID     FOR  THIS REP           
*                                                                               
VALOPT06 DS    0H                                                               
         CLC   OPTIDNUM,ADOOPT#    MATCH OPTION    BEING      CHECKED ?         
         BNE   VALOPT08            NO,   GET  NEXT ENTRY                        
         ZIC   R6,CULANG           GET   CURRENT   LANGUAGE                     
         AHI   R6,-1               MINUS ONE                                    
         BNM   *+6                 LANGUAGE   =    ENG  AND  ZERO ?             
         DC    H'0'                YES,  ABEND                                  
*                                  LANGUAGE   NUM  >    14   ?                  
         CHI   R6,L'ADOLCODE-1                                                  
         BNH   *+6                 NO,   CONTINUE                               
         DC    H'0'                YES,  ABEND                                  
         LA    R6,ADOLCODE(R6)     CODE  IN   USER'S    LANGUAGE                
         CLC   FVIFLD(1),0(R6)     MATCHING   USER'S    INPUT     ?             
         BNE   *+6                 NO,   SKIP                                   
         LR    RF,R2               YES,  SAVE ADDR ENTRY                        
         MVC   0(1,R5),0(R6)       MOVE  IN   VALID     USER OPTION             
         MVC   1(1,R5),SCCOMMA     ADD   COMMA     BETWEEN   OPTIONS            
         LA    R5,2(,R5)           MOVE  UP   IN   MSG  STRING                  
*                                                                               
VALOPT08 LA    R2,ADOPTLNQ(,R2)    NEXT  ENTRY                                  
         B     VALOPT02                                                         
*                                                                               
VALOPT10 DS    0H                                                               
         CLC   FVXTRA,SPACES       ANY   VALID     ENTRIES   AVAILABLE?         
         BNH   VALOPT99            NO,   EXIT                                   
         BCTR  R5,0                BUMP  BACK ONE                               
         MVI   0(R5),C' '          BLANK OUT  EXTRA     COMMA                   
         MVC   FVMSGNO,=AL2(1440)  VALID OPTIONS   ARE  &T                      
*                                                                               
         LTR   RF,RF               ANY   OPTION    MATCHED   ?                  
         BZ    VALOPT99            NO,   EXIT                                   
         LR    R2,RF               RESTORE    GOOD ENTRY                        
*                                                                               
VALOPT30 DS    0H                                                               
         MVC   OPTVALUE,FVIFLD     SAVE  USER'S    INPUT                        
         MVC   OPTEQU,ADODTYPE     SAVE  DATE TYPE                              
         MVC   OPTNME,ADODESCR     GET   DICTIONARY     TRANSLATION             
         LTR   R4,R4               ANY   NAME FIELD     ?                       
         BZ    VALOPT90            NO,   SKIP                                   
         OI    6(R4),FVOXMT        TRANSMIT   NAME OF   OPTION                  
         CLI   OPTNME,ESCHIGHQ     HARD  CODED     NAME ?                       
         BNL   VALOPT31            YES,  SKIP                                   
         GOTO1 VDICTAT,APPARM,C'SL  ',OPTNME,0                                  
*                                                                               
VALOPT31 SR    R1,R1                                                            
         IC    R1,0(,R4)           GET   LENGTH                                 
         AHI   R1,-8                                                            
         TM    1(R4),FVAXTND       EXTENDED   FIELD     HEADER    ?             
         BZ    *+8                                                              
         AHI   R1,-8                                                            
*                                  SCREEN     FLD  HOLDS >   16   CHARS         
         CLM   R1,1,=AL1(L'ADODESCR)                                            
         BNH   VALOPT32            NO,   SKIP                                   
         LA    R1,L'ADODESCR       MOVE  MAX  OF   16   CHARACTERS              
*                                                                               
VALOPT32 BCTR  R1,0                INSERT     NAME FIELD                        
         EX    R1,*+4                                                           
         MVC   8(0,R4),OPTNME                                                   
*                                                                               
VALOPT90 DS    0H                  PROCESSING COMPLETE                          
         MVC   FVMSGNO,=AL2(FVFOK) OPTION     OK                                
         MVC   FVXTRA,SPACES       CLEAR MSG  INFORMATION                       
         SR    RE,RE               FOR   CONDITION CODE                         
*                                                                               
VALOPT99 DS    0H                                                               
         LTR   RE,RE               SET   CONDITION CODE                         
         XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  GET METHOD                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R1                                                        
GETMTHD  NMOD1 0,**MTHD**                                                       
         L     RC,APALOCAL                                                      
         L     R1,AIOAREA1                                                      
         MVI   SAVEMTHD,C' '                                                    
         MVI   APELCODE,RPFELQ     X'C4' PROFILE ELEMEMT                        
         GOTO1 GETEL                                                            
         BNE   GETMHD05                                                         
         CLI   RPFMETHD,C' '                                                    
         BNH   *+10                                                             
         MVC   SAVEMTHD,RPFMETHD                                                
*                                                                               
         USING RFLELD,R1                                                        
GETMHD05 SR    RF,RF                                                            
         L     R1,AIOAREA1                                                      
         AH    R1,DATADISP                                                      
*                                                                               
GETMHD10 CLI   0(R1),0                                                          
         BE    GETMHD30                                                         
         CLI   0(R1),RFLELQ        X'C5'                                        
         BNE   GETMHD20                                                         
         CLI   RFLSEQ,0                                                         
         BNE   GETMHD30                                                         
         CLI   RFLTYPE,RFLMTHD     METHOD ONLY                                  
         BNE   GETMHD20                                                         
         MVC   SAVEMTHD,RFLDATA                                                 
*                                                                               
GETMHD20 IC    RF,RFLLN                                                         
         AR    R1,RF                                                            
         B     GETMHD10                                                         
*                                                                               
GETMHD30 CLI   SAVEMTHD,C' '                                                    
         BE    *+6                 IF EQUAL THEN MAKE CC=NOT EQUAL              
         SR    R1,R1               VISA VERSA                                   
         LTR   R1,R1                                                            
         XIT1                                                                   
         DROP  R1                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  GET RECAP DETAILS (DURING VALKEY)                                  *         
***********************************************************************         
         SPACE 1                                                                
GETRCAP  NMOD1 0,**RCAP**                                                       
         L     RC,APALOCAL                                                      
         L     R1,AIOAREA1         A(Format record)                             
         MVI   RECAP#,0            Set to no recapping                          
         MVI   RCAPSW,0                                                         
         XC    RCAPFMTS(MAXRCP#*L'RCAPFMTS),RCAPFMTS                            
         MVC   RQRCAPSW,SPACES     Clear recap request card info field          
*                                                                               
         USING RCPELD,R1           Recap element                                
         USING RECAPD,R2           Recap table                                  
         LA    R2,RCAPFMTS         A(Saved recap codes)                         
         MVI   APELCODE,RCPELQ     X'C8'  Recap element                         
         GOTO1 GETEL               Get first one                                
*                                                                               
GETRCP10 BNE   GETRCPEX            None or no more, so finished                 
         CLI   RCPSEQ,0            Main format ?                                
         BE    GETRCP30            Yes, skip it then                            
         CLI   RCPSEQ,MAXRCP#      Too many recaps ?                            
         BNH   *+6                 No                                           
         DC    H'0'                Yes                                          
*                                                                               
         MVC   RECAP#,RCPSEQ       Save highest recap #                         
         MVC   RECAPFMT,RCPCODE    Save format code                             
         MVC   RECAPIND,RCPOPT1         Save profile info                       
         MVC   RECAPERR,=AL2(FVFOK)     Set to ok                               
         LA    R2,RECAPLNQ(,R2)         Next recap format                       
         TM    RCPOPT1,RCPPROF     Use recap's profile ?                        
         BZ    GETRCP30            No                                           
         OI    RCAPSW,RCAPPROF     Yes                                          
*                                                                               
GETRCP30 GOTO1 NEXTEL              Get next recap element                       
         B     GETRCP10                                                         
*                                                                               
GETRCPEX XMOD1 ,                                                                
         DROP  R1                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  GET APG DETAILS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
GETAPG   NMOD1 0,**GAPG**                                                       
         L     RC,APALOCAL                                                      
         LA    R2,APRECKEY                                                      
         L     R3,AAPGIO                                                        
         MVC   APDUB,=CL8'AC'                                                   
         MVC   APDUB+2(2),APGKRTY                                               
         MVC   APDUB+4(2),CUAALF                                                
         MVC   APDUB+6(1),APGKFMT                                               
         GOTO1 ALOADER,APPARM,APDUB,AAPGIO,(C'M',AAPGIO)                        
         OC    APPARM+4(4),APPARM+4                                             
         BNZ   *+6                                                              
         DC    H'00'                                                            
         DROP  R2                                                               
*                                                                               
         MVI   BUD#,0                                                           
         MVI   OPT1#,0                                                          
         MVI   OPT3#,0                                                          
         MVI   OPT4#,0                                                          
         MVI   LDGR#,0                                                          
         XC    BUDLIST,BUDLIST                                                  
         MVC   LDGLIST,SPACES                                                   
         XC    OPT1LST,OPT1LST                                                  
         XC    OPT3LST,OPT3LST                                                  
         XC    OPT4LST,OPT4LST                                                  
*                                                                               
GETAPG10 CLI   0(R3),X'FF'         END OF REPORT                                
         BE    GETAPG90                                                         
         CLI   0(R3),CMREAD        SINGLE LEDGER                                
         BE    GETAPG20                                                         
         CLI   0(R3),CMRLST        MULTIPLE LEDGER                              
         BE    GETAPG20                                                         
         CLI   0(R3),CMBGDT        BUDGETS                                      
         BE    GETAPG40                                                         
         CLI   0(R3),CMIF          CONDITIONAL IF                               
         BE    GETAPG60                                                         
         CLI   0(R3),CMOR          CONDITIONAL OR                               
         BE    GETAPG60                                                         
         CLI   0(R3),CMAND         CONDITIONAL AND                              
         BE    GETAPG60                                                         
*                                                                               
GETAPG15 SR    R1,R1                                                            
         IC    R1,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R1                                                            
         B     GETAPG10                                                         
         EJECT ,                                                                
***********************************************************************         
* GET LEDGER INFO                                                     *         
***********************************************************************         
         SPACE 1                                                                
GETAPG20 SR    R1,R1                                                            
         IC    R1,1(,R3)                                                        
         AHI   R1,-2                                                            
         LA    RE,2(,R3)                                                        
         CLI   0(R3),CMREAD                                                     
         BE    GETAPG25                                                         
         BCTR  R1,0                                                             
         LA    RE,3(,R3)                                                        
*                                                                               
GETAPG25 LA    R6,LDGLIST                                                       
         SR    RF,RF                                                            
         ICM   RF,1,LDGR#                                                       
         BZ    GETAPG30                                                         
*                                                                               
GETAPG28 CLC   0(2,RE),0(R6)       CHECK FOR DUPLICATES                         
         BE    GETAPG35                                                         
         LA    R6,2(,R6)           NEXT LEDGER                                  
         BCT   RF,GETAPG28                                                      
*                                                                               
GETAPG30 IC    RF,LDGR#                                                         
         LA    RF,1(,RF)                                                        
         STC   RF,LDGR#                                                         
         MVC   0(2,R6),0(RE)                                                    
*                                                                               
GETAPG35 LA    RE,3(,RE)                                                        
         AHI   R1,-3                                                            
         BZ    GETAPG15            FINISHED                                     
         B     GETAPG25            GET ANOTHER                                  
         EJECT ,                                                                
***********************************************************************         
*  GET BUDGET INFO                                                    *         
***********************************************************************         
         SPACE 1                                                                
GETAPG40 SR    R6,R6                                                            
         IC    R6,2(R3)            BUDGET TO PROCESS                            
         LA    R6,BUDLIST-1(R6)                                                 
         CLC   BUD#,2(R3)          SAVE HIGHEST BUD NUMBER                      
         BH    *+10                                                             
         MVC   BUD#,2(R3)                                                       
         OC    3(2,R3),3(R3)       ANY BUDGET?                                  
         BZ    GETAPG45                                                         
         MVC   0(1,R6),4(R3)       SUPPORT BUD# 1-207                           
         B     GETAPG15            FINISHED                                     
*                                                                               
         USING BUDRECD,R2                                                       
GETAPG45 LA    R2,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN      COMPANY CODE                                 
         MVC   BUDKCOD,SPACES                                                   
         SR    RF,RF                                                            
         IC    RF,1(,R3)                                                        
         AHI   RF,-6                                                            
         BM    GETAPG15                                                         
         EX    RF,*+4                                                           
         MVC   BUDKCOD(0),5(R3)                                                 
         MVC   SAVBUD,BUDKCOD                                                   
         GOTO1 AIO,IO3+IOACCDIR+IOHI                                            
         CLC   BUDKCOD,SAVBUD                                                   
         BNE   GETAPG15            BUDGET MISSING                               
         MVC   0(1,R6),BUDKNO2+1                                                
         B     GETAPG15            FINISHED                                     
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  GET OPTION INFO                                                    *         
***********************************************************************         
         SPACE 1                                                                
GETAPG60 LA    R1,OPT1#                                                         
         LA    R2,OPT1LST                                                       
         CLI   3(R3),FRQ1          OPTION 1                                     
         BE    GETAPG65                                                         
         LA    R1,OPT3#                                                         
         LA    R2,OPT3LST                                                       
         CLI   3(R3),FRQ3          OPTION 3                                     
         BE    GETAPG65                                                         
         LA    R1,OPT4#                                                         
         LA    R2,OPT4LST                                                       
         CLI   3(R3),FRQ4          OPTION 4                                     
         BNE   GETAPG15            LOOP                                         
*                                                                               
GETAPG65 MVC   APBYTE,7(R3)                                                     
         CLI   1(R3),8                                                          
         BNH   *+8                                                              
         MVI   APBYTE,C' '                                                      
         SR    RF,RF                                                            
         ICM   RF,1,0(R1)          NUMBER OF OPTIONS SO FAR                     
         BZ    GETAPG70                                                         
         LR    RE,R2                                                            
         AR    R2,RF                                                            
*                                                                               
GETAPG67 CLC   APBYTE,0(RE)                                                     
         BE    GETAPG15            DUPLICATE OPTION                             
         LA    RE,1(,RE)                                                        
         BCT   RF,GETAPG67                                                      
*                                                                               
GETAPG70 MVC   0(1,R2),APBYTE                                                   
         IC    RF,0(,R1)                                                        
         LA    RF,1(,RF)                                                        
         STC   RF,0(,R1)                                                        
         B     GETAPG15            FINISHED                                     
         EJECT ,                                                                
GETAPG90 CLI   BUD#,12                                                          
         BNH   *+8                                                              
         MVI   BUD#,12             IS THE HIGHEST SUPPORTED                     
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  CHECK MESSAGE ELEMENTS FOR ERROR TYPE MESSAGES                     *         
*                                                                     *         
*  USES:                                                              *         
*    APELCODE - MESSAGE ELEMENT (In APELEM)                           *         
*    ELEMSEQ  - COLUMN  NUMBER                                        *         
*    SVMSGNO  - SAVE    FVMSGNO                                       *         
*    IOKEY    - IO      KEY                                           *         
*    IOAREA2  - SECOND  IO AREA                                       *         
*    APPARM   - PARM    LIST                                          *         
*                                                                     *         
*  CALLS:                                                             *         
*    CKRMSGER - CHECK   FORMAT FOR MESSAGE ELEMENTS WITH ERROR TYPES  *         
*    CKRCPMSG - CHECK   FOR  RECAP ERRORS                             *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    CONDITION CODE SET:                                              *         
*      EQUAL     - 'E' TYPE MESSAGE FOUND                             *         
*      NOT EQUAL - 'E' TYPE MESSAGE NOT   FOUND                       *         
***********************************************************************         
         EJECT                                                                  
         USING TWAD,R5                                                          
         USING RESRECD,R4                                                       
VALFRMT  NMOD1 0,**VFMT**                                                       
         L     RC,APALOCAL                                                      
         L     R3,0(,R1)                    Format code                         
                                                                                
         USING PITCHD,RE                                                        
         LA    RE,PITCHTAB         Set default values                           
         MVC   MAXWDPT,PITWDPT     Max width for pitch portrait                 
         MVC   MAXWDLS,PITWDLS     Max width for pitch landscape                
         DROP  RE                                                               
                                                                                
         LA    R4,IOKEY                                                         
         MVI   FFOUND,0                                                         
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ             X'2D'   record type                 
         MVI   RESKSUB,RESKSUBQ             X'02'   record sub type             
         MVC   RESKCPY,CUABIN               Company code                        
         MVC   RESKFORM(L'RCPCODE),0(R3)    Set key                             
*                                                                               
         L     R4,AIOAREA1                                                      
         LA    R3,IO1+IORDD+IOACCFIL                                            
         TM    0(R1),IO1           Use format area 1                            
         BO    VALFRM10                                                         
         LA    R3,IO2+IORDD+IOACCFIL                                            
         L     R4,AIOAREA2                                                      
         TM    0(R1),IO2           Use format area 2                            
         BO    VALFRM10                                                         
         LA    R3,IO3+IORDD+IOACCFIL                                            
         L     R4,AIOAREA3                                                      
         TM    0(R1),IO3           Use format area 3                            
         BO    VALFRM10                                                         
         DC    H'0'                                                             
*                                                                               
VALFRM10 CLC   RESKEY(RESKFRML),IOKEY    Do we already have in io area?         
         BE    VALFRM11                  Yes                                    
*                                                                               
         GOTO1 AIO,(R3)            No, so read record                           
         BNE   VALFERR8            Bad Read, record not found                   
         TM    IOERR,IOEDEL        Is it deleted ?                              
         BO    VALFERR7            Yes                                          
*                                                                               
VALFRM11 LR    R3,R4                                                            
         AH    R3,DATADISP                                                      
*&&UK                                                                           
         XC    XDATCO#,XDATCO#                                                  
         MVI   XDATCO#+L'XDATCO#-1,X'FF'                                        
         LA    RE,XDATCO#                                                       
         ST    RE,AXDCOL#                                                       
*&&                                                                             
VALFRM12 CLI   0(R3),0             EOR ?                                        
         BE    VALFRM80                                                         
         CLI   0(R3),STYELQ        x'25' Scribe type information                
         BE    VALFRM15                                                         
*&&UK*&& CLI   0(R3),RRWELQ        x'C2' Check for row                          
*&&UK*&& BE    VALFRM18                                                         
         CLI   0(R3),RCLELQ        x'C3' Check for columns                      
         BE    VALFRM20                                                         
         CLI   0(R3),RPFELQ        x'C4' Check profile information              
         BE    VALFRM30                                                         
         CLI   0(R3),MNOELQ        x'C9' Error message                          
         BE    VALFRM50                                                         
*                                                                               
         USING RFLELD,R3                                                        
         CLI   0(R3),RFLELQ        x'C5' Filter element                         
         BNE   VALFRM14                                                         
         CLI   RFLSEQ,0            Must be global filter                        
         BE    VALFRM60                                                         
*&&UK                                                                           
         CLI   RFLTYPE,RFLXDATN                                                 
         BNE   VALFRM14                                                         
         OC    XDATCO#(L'XDATCO#-1),XDATCO#                                     
         BZ    VALFRM14                                                         
         LA    RE,XDATCO#                                                       
VALFRM13 CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                there should be a match                      
         CLC   0(L'RFLSEQ,RE),RFLSEQ                                            
         BE    *+12                                                             
         AHI   RE,L'RFLSEQ                                                      
         B     VALFRM13                                                         
         MVI   0(RE),0                                                          
*&&                                                                             
VALFRM14 SR    RF,RF                                                            
         IC    RF,1(,R3)           Next element                                 
         AR    R3,RF                                                            
         B     VALFRM12                                                         
***********************************************************************         
*        Check for report type and security                           *         
***********************************************************************         
         SPACE 1                                                                
         USING STYELD,R3                                                        
VALFRM15 OI    FFOUND,FSTYELQ      Found STYELQ element                         
         MVC   RPTWIDTH,STYWIDTH   Set width                                    
         CLC   STYNAME,APREPCDE    Must be same report type for now             
         BNE   VALFERR1            Was not so error                             
*                                                                               
         OC    TWASAGN,TWASAGN     New security ?                               
         BZ    VALFRM14            No, skip                                     
         SR    RF,RF                                                            
         IC    RF,STYSEC#1         Any keyword security on format ?             
         GOTO1 AFMTSEC,APPARM,(RF),1                                            
         CLI   APPARM,0            Any problems found ?                         
         BNE   VALFERR2            Yes, exit                                    
         SR    RF,RF                                                            
         IC    RF,STYSEC#5         Any security set up ?                        
         GOTO1 AFMTSEC,APPARM,(RF),33                                           
         CLI   APPARM,0            Any problems found ?                         
         BNE   VALFERR2            Yes, exit                                    
         B     VALFRM14            Next element                                 
***********************************************************************         
*        Check for row on format                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RRWELD,R3                                                        
VALFRM18 TM    RRWOPT,RRWPRDTY     Period type keyword?                         
         BZ    VALFRM14            No - Next element                            
         OI    FFOUND,FPERIOD      Yes - must enter period range                
         B     VALFRM14                                                         
***********************************************************************         
*        Check for columns on format                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R3                                                        
VALFRM20 DS    0H                                                               
*&&UK                                                                           
         TM    RCLOPT4,RCLPRDTY    Period type keyword?                         
         BZ    *+8                 No - OK                                      
         OI    FFOUND,FPERIOD      Yes - must enter period range                
*                                                                               
         XR    RE,RE                                                            
         IC    RE,RCLDATLN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   VALFRM22                                                         
         CLC   RCLXDATA+1(0),AC@XDATA   XDATA KEYWORD?                          
         L     RF,AXDCOL#                                                       
         MVC   0(L'RCLSEQ,RF),RCLSEQ                                            
         AHI   RF,L'RCLSEQ                                                      
         ST    RF,AXDCOL#                                                       
*&&                                                                             
VALFRM22 CLI   RCLWDTH,0           Is column width zero ?                       
         BE    VALFRM14            Yes, column not printed, don't count         
         TM    RCLOPT,RCLHIDE      Hidden column ?                              
         BO    VALFRM14            Yes, column not printed, don't count         
         CLI   RCLSTACK,0          Stacked under type column ?                  
         BNE   VALFRM14            No, good column (shouldn't happen)           
         OI    FFOUND,FCOLUMN      Found good column                            
         B     VALFRM14                                                         
***********************************************************************         
*        Check for forced down-load                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R3                                                        
         USING PITCHD,RE                                                        
VALFRM30 LA    RF,MAX#PTCH                                                      
         LA    RE,PITCHTAB                                                      
         CLI   RPFLN,RPFLNQ        Which version of element is this             
         BNH   VALFRM38            Old so no pitch setting (Auto)               
VALFRM32 CLC   RPFPITCH,PITCH#                                                  
         BNE   VALFRM34                                                         
         MVC   MAXWDPT,PITWDPT     Max width for pitch portrait                 
         MVC   MAXWDLS,PITWDLS     Max width for pitch landscape                
VALFRM34 AHI   RE,PITCHLNQ                                                      
         BCT   RF,VALFRM32                                                      
         DROP  RE                                                               
                                                                                
VALFRM36 IC    RF,RPFPITCH                                                      
         CVD   RF,APDUB            CONVERT IT   TO       PACKED DECIMAL         
         OI    APDUB+7,X'0F'       SETUP   FOR  DISPLAY                         
         UNPK  PITCH,APDUB                                                      
                                                                                
VALFRM38 TM    RPFDNOPT,RPFDDOWN   Forced down-load ?                           
         BZ    VALFRM14                                                         
         OI    PRFFLAG,PRFDOWN     Force download option if RPFDDOWN            
         B     VALFRM14                                                         
***********************************************************************         
*        Check for error message elements                             *         
***********************************************************************         
         SPACE 1                                                                
         USING MNOEL,R3                                                         
VALFRM50 SR    RF,RF                                                            
         IC    RF,MNOLN            Get length of total element                  
         LA    RE,0(RF,R3)         Point to end of element                      
         LA    R2,MNOMLN                                                        
*                                                                               
VALFRM52 CLI   MNOMTYPE,MNOMTERR   Error                                        
         BE    VALFERR6            Format has error messages                    
         IC    RF,MNOMLN           Get to next sub element                      
         AR    R2,RF                                                            
         CR    R2,RE               See if at end                                
         BL    VALFRM52            No, so check if error type                   
         B     VALFRM14                                                         
***********************************************************************         
*        Check for consistent ledgers and recap profile conflicts     *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R3                                                        
VALFRM60 CLI   RFLTYPE,RFLLDG      Must be unit/ledger                          
         BNE   VALFRM14            Next element                                 
         SR    RF,RF                                                            
         ICM   RF,1,RECAP#                                                      
         BZ    VALFRM14            No recaps                                    
         CLC   RESKFORM,SAVFORM    Main format ?                                
         BE    VALFRM14            Yes, so skip                                 
*                                                                               
         USING RECAPD,R6                                                        
         LA    R6,RCAPFMTS         List of formats                              
VALFRM61 CLC   RESKFORM,RECAPFMT   Find format in recap table                   
         BE    VALFRM62            Found                                        
         LA    R6,RECAPLNQ(,R6)    Try next                                     
         BCT   RF,VALFRM61         Next entry in table                          
         DC    H'00'               It has to be here                            
*                                                                               
VALFRM62 IC    RF,RFLLN            Match on ledgers                             
         SHI   RF,RFLDATA-RFLELD                                                
         LA    RE,RFLDATA          Point to list of ledgers                     
         SR    R1,R1                                                            
*                                                                               
VALFRM65 IC    R1,APLDGR#          # of ledgers on MAIN in RFLELD               
         LA    R2,APLDGRS          List of ledgers on MAIN format               
                                                                                
VALFRM67 CLC   0(2,R2),0(RE)       Do they match                                
         BE    VALFRM68            This one is ok                               
         LA    R2,2(,R2)           Next in list of MAIN format                  
         BCT   R1,VALFRM67                                                      
*                                                                               
         OI    RCAPSW,RCAPMIXL     Mixed Ledgers being used                     
         TM    RECAPIND,RCPPROF    Recap profile used ?                         
         BO    VALFRM14            Yes, so doesn't matter                       
         B     VALFERR5            Error, ledger not found                      
*                                                                               
VALFRM68 LA    RE,3(,RE)           Next in Recap formats RFLELD                 
         SHI   RF,3                Any more to compare ?                        
         BP    VALFRM65            Yes                                          
         B     VALFRM14            No, finished. Next element                   
         DROP  R6                                                               
***********************************************************************         
*        Errors to set                                                *         
***********************************************************************         
         SPACE 1                                                                
VALFRM80 DS    0H                                                               
*&&UK                                                                           
         OC    XDATCO#(L'XDATCO#-1),XDATCO#                                     
         BNZ   VALFERR9                                                         
*&&                                                                             
         TM    FFOUND,FSTYELQ      Needs this element                           
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         TM    FFOUND,FCOLUMN      Find a valid column ?                        
         BZ    VALFERR4            No                                           
         B     VALFRM90            Yes                                          
*                                                                               
VALFERR1 MVC   FVMSGNO,=AL2(ACEMRSRT)       Must have same type                 
         B     VALFRM90                                                         
*                                                                               
VALFERR2 MVC   FVMSGNO,=AL2(ACEIVSEC)       Security violation                  
         B     VALFRM90                                                         
*                                                                               
VALFERR4 MVC   FVMSGNO,=AL2(ACEFMTNC)       Format has no columns               
         B     VALFRM90                                                         
*                                                                               
*        Recap ledgers don't match MAIN. Set profile to REPORT                  
VALFERR5 MVC   FVMSGNO,=AL2(2232)                                               
         B     VALFRM90                                                         
*                                                                               
VALFERR6 MVC   FVMSGNO,=AL2(2096)           Format has error messages           
         B     VALFRM90                                                         
*                                                                               
VALFERR7 MVC   FVMSGNO,=AL2(0077)           Format is marked deleted            
         B     VALFRM90                                                         
*                                                                               
VALFERR8 MVC   FVMSGNO,=AL2(2105)           Format not found                    
         B     VALFRM90                                                         
*&&UK                                                                           
VALFERR9 MVC   FVMSGNO,=AL2(ACEXDVC)   XDATA column is missing colfilt          
*&&                                         input in Data name field            
VALFRM90 CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1                                                                   
         DROP  R3,R4                                                            
                                                                                
PITCHTAB DC    AL1(00),AL2(999,999)                                             
         DC    AL1(10),AL2(085,110)                                             
         DC    AL1(12),AL2(102,132)                                             
         DC    AL1(15),AL2(127,198)                                             
         DC    AL1(18),AL2(153,198)                                             
         DC    AL1(20),AL2(198,198)                                             
         DC    AL1(24),AL2(198,198)                                             
MAX#PTCH EQU   (*-PITCHTAB)/PITCHLNQ                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  GET DEFAULT TEXT                                                   *         
*                                                                     *         
*  USES:                                                              *         
*    ARTND    - ADDR      OF   RTND DSECT                             *         
*    DUMFLD   - DUMMY     FIELD                                       *         
*    TMPPARM  - TEMPORARY PARM AREA                                   *         
*                                                                     *         
*  INPUT:                                                             *         
*    PARM 1 BYTES 1-3 -   ADDR TO   STORE THE DATA                    *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    DATA FIELD UPDATED                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING RTND,R4             Routine table DSECT                          
GDEFTEXT NMOD1 0,**DFTX**                                                       
         L     RC,APALOCAL                                                      
         L     R3,0(,R1)           A(Field)                                     
         LA    R3,0(,R3)           Clear high order byte                        
*                                                                               
         L     R4,ARTND            A(Table entry)                               
         ZIC   R2,RTNFLDLN         Get size of field                            
         BCTR  R2,0                                                             
         EXMVC R2,0(R3),SPACES     Clear output field                           
         NC    RTNGTXNO,RTNGTXNO   Any value to put for table entry ?           
         BZ    GDEFTEX             No, exit                                     
*                                                                               
         CHI   R2,L'DUMFLDH-1      Make sure field is large enough              
         BH    GDEFTEX             No, exit                                     
         MVI   0(R3),C'('          Initialize output field                      
         CLI   RTNFLDLN,1          One byte field ?                             
         BNH   GDEFTEX             Yes, can't fit anything else                 
         SR    R6,R6               Get message number                           
         ICM   R6,3,RTNGTXNO                                                    
         XC    DUMFLDH,DUMFLDH     Clear dummy field header                     
         LA    RF,L'DUMFLDH+1(,R2) Get field length plus header length          
         STC   RF,DUMFLDH          Save length                                  
         LA    RF,DUMFLDH                    A(Dummy field header)              
         GOTO1 TEXTGET,TMPPARM,(R6),(RF),0   Expand text                        
         EXMVC R2,0(R3),DUMFLD               Move text to passed field          
*                                                                               
GDEFTEX  XMOD1 ,                                                                
         DROP  R4                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
       ++INCLUDE ACSCRREQ                                                       
         EJECT ,                                                                
***********************************************************************         
* Valid ledger based on field processing                              *         
***********************************************************************         
LEDGLIST DS    0C                                                               
         DC    AL1(FLDNCLNT),CL2'SJ'      Client                                
         DC    AL1(FLDNVNDR),CL2'SV'      Vendor SV                             
         DC    AL1(FLDNVNDR),CL2'SX'      Vendor SX                             
         DC    AL1(FLDNVNDR),CL2'SY'      Vendor SY                             
         DC    AL1(FLDNCSTA),CL2'1C'      Cost account                          
         DC    AL1(FLDNDEXP),CL2'13'      Direct expense                        
         DC    AL1(FLDNDEPT),CL2'2D'      Department                            
         DC    AL1(FLDNPRSN),CL2'2P'      Person                                
         DC    AL1(FLDNBSRC),CL2'  '      Billing source                        
         DC    AL1(FLDNPART),CL2'3 '      Participant                           
         DC    X'FF'                                                            
         SPACE 2                                                                
***********************************************************************         
* Field to build special record key to validate with                  *         
***********************************************************************         
CHKTABL  DC    AL1(FLDNOGRP),AL1(1,2,5),X'2C0200E2D14000'      OFF GRP          
         DC    AL1(0)                                                           
         DC    AL1(FLDNOFFC),AL1(2,2,5),X'2C0400E2D14040'      OFFICE           
         DC    AL1(0)                                                           
         DC    AL1(FLDNMGRP),AL1(1,2,5),X'2C0600E2D14000'      MED GRP          
         DC    AL1(0)                                                           
         DC    AL1(FLDNMEDA),AL1(1,1,2),X'09004040404040'      MEDIA            
         DC    AL1(CHKFSPC)                                                     
         DC    AL1(FLDNWCGP),AL1(1,2,5),X'2C0800E2D14000'      WC GRP           
         DC    AL1(0)                                                           
         DC    AL1(FLDNSTTY),AL1(4,2,3),X'2C230040404040'      STUDIO           
         DC    AL1(0)                                                           
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* Valid Billing type values                                           *         
***********************************************************************         
BILLTY   DC    C'ACEPSTU1',AL1(EOT)                                             
         EJECT ,                                                                
       ++INCLUDE ACSCROTYP                                                      
         EJECT ,                                                                
       ++INCLUDE ACSCRRCLS                                                      
         EJECT ,                                                                
***********************************************************************         
*  Routine Table for displaying and validating of the Screen fields   *         
***********************************************************************         
         SPACE 1                                                                
ROUTINES DS    0F                                                               
* Unit/Ledger                                                                   
         DC    AL1(FLDNUNLG,LUNLG,RFLLDG),AL2(1688)                             
         DC    AL1(RTDULAC),AL1(RTVUNLG)                                        
* Account                                                                       
         DC    AL1(FLDNACCT,L'ACQACT,RFLACC),AL2(1680)                          
         DC    AL1(RTDULAC),AL1(RTVACNT)                                        
* Billing Group                                                                 
         DC    AL1(FLDNBGRP,L'ACQBILGP,RFLBLGP),AL2(0)                          
         DC    AL1(RTDGENC),AL1(RTVGENR)                                        
* Office Group                                                                  
         DC    AL1(FLDNOGRP,L'ACQOFGRP,RFLOFGP),AL2(1687)                       
         DC    AL1(RTDGENC),AL1(RTVOFGP)                                        
* Office Code                                                                   
         DC    AL1(FLDNOFFC,L'ACQOFFFL,RFLOFF),AL2(1687)                        
         DC    AL1(RTDGENC),AL1(RTVOFFC)                                        
* Media Group                                                                   
         DC    AL1(FLDNMGRP,L'ACQMEDGP,RFLMDGP),AL2(1693)                       
         DC    AL1(RTDGENC),AL1(RTVOFGP)                                        
* Media                                                                         
         DC    AL1(FLDNMEDA,L'ACQMEDFL,RFLMED),AL2(1693)                        
         DC    AL1(RTDGENC),AL1(RTVOFGP)                                        
* Workcode Group                                                                
         DC    AL1(FLDNWCGP,L'ACQWCGRP,RFLWCGP),AL2(1692)                       
         DC    AL1(RTDGENC),AL1(RTVOFGP)                                        
* Workcode                                                                      
         DC    AL1(FLDNWKCD,L'ACQWRKLS,RFLWC),AL2(1696)                         
         DC    AL1(RTDGENC),AL1(RTVWKCD)                                        
* Start Date                                                                    
         DC    AL1(FLDNSTDT,L'ACQSTART,0),AL2(MSG#STRT)                         
         DC    AL1(RTDDTES),AL1(RTVDTES)                                        
* End Date                                                                      
         DC    AL1(FLDNENDT,L'ACQEND,0),AL2(MSG#END)                            
         DC    AL1(RTDDTES),AL1(RTVDTES)                                        
* MOA Range                                                                     
         DC    AL1(FLDNMOAR,L'ACQMOSPD,0),AL2(MSG#MOA)                          
         DC    AL1(RTDDTES),AL1(RTVMOAR)                                        
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
* Media MOS (range)                                                             
*&&US*&& DC    AL1(FLDNMMOS,L'ACQSTART+L'ACQEND,0),AL2(0)                       
*&&US*&& DC    AL1(RTDDTES),AL1(RTVRNGE)                                        
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
* Activity Start Date                                                           
         DC    AL1(FLDNACTS,L'ACQACTST,0),AL2(MSG#ASTR)                         
         DC    AL1(RTDNONE),AL1(RTVDTES)                                        
* Activity End Date                                                             
         DC    AL1(FLDNACTE,L'ACQACTND,0),AL2(MSG#AEND)                         
         DC    AL1(RTDNONE),AL1(RTVDTES)                                        
* Alternate Start Date                                                          
         DC    AL1(FLDNALTS,L'ACQDTSTR,0),AL2(MSG#ALTS)                         
         DC    AL1(RTDNONE),AL1(RTVDTES)                                        
* Alternate End Date                                                            
         DC    AL1(FLDNALTE,L'ACQDTEND,0),AL2(MSG#ALTE)                         
         DC    AL1(RTDNONE),AL1(RTVDTES)                                        
* Over-ride Current Date                                                        
         DC    AL1(FLDNOCUR,L'ACQACTND,0),AL2(MSG#END)                          
         DC    AL1(RTDNONE),AL1(RTVDTES)                                        
* ?                                                                             
         DC    AL1(FLDNTYPE,L'ACQDTTYP,0),AL2(0)                                
         DC    AL1(RTDNONE),AL1(RTVALDT)                                        
* Transaction type                                                              
         DC    AL1(FLDNTRNT,L'ACQTTYPE,RFLTTYPE),AL2(1689)                      
         DC    AL1(RTDGENC),AL1(RTVTRNT)                                        
* Billing Type ?                                                                
         DC    AL1(FLDNBILT,L'ACQBILTY,RFLBTYP),AL2(1697)                       
         DC    AL1(RTDGENC),AL1(RTVBILT)                                        
* Estimate Status                                                               
         DC    AL1(FLDNESTS,L'ACQOPT7,0),AL2(0)                                 
         DC    AL1(RTDESTS),AL1(RTVESTS)                                        
* Studio Type                                                                   
         DC    AL1(FLDNSTTY,L'ACQSTUTY,RFLSTTY),AL2(0)                          
         DC    AL1(RTDGENC),AL1(RTVOFGP)                                        
* Userfield                                                                     
         DC    AL1(FLDNUSRF,L'ACQUSFLD,RFLUFLD),AL2(0)                          
         DC    AL1(RTDGENC),AL1(RTVGENR)                                        
* Contra Account                                                                
         DC    AL1(FLDNCNTR,LULACNT,RFLCNTR),AL2(1695)                          
         DC    AL1(RTDACCT),AL1(RTVCNTR)                                        
* Client Account                                                                
         DC    AL1(FLDNCLNT,LACCOUNT,RFLCLI),AL2(1681)                          
         DC    AL1(RTDACCT),AL1(RTVCLIA)                                        
* Cost Account                                                                  
         DC    AL1(FLDNCSTA,LACCOUNT,RFLCOST),AL2(1685)                         
         DC    AL1(RTDACCT),AL1(RTVCLIA)                                        
* Billing Source                                                                
         DC    AL1(FLDNBSRC,LACCOUNT,RFLBSR),AL2(1694)                          
         DC    AL1(RTDACCT),AL1(RTVCNTR)                                        
* Participant Account                                                           
         DC    AL1(FLDNPART,LULACNT,RFLPRTP),AL2(0)                             
         DC    AL1(RTDACCT),AL1(RTVCLIA)                                        
* Department Account                                                            
         DC    AL1(FLDNDEPT,LACCOUNT,RFLDEPT),AL2(1682)                         
         DC    AL1(RTDACCT),AL1(RTVCLIA)                                        
* Person Account                                                                
         DC    AL1(FLDNPRSN,LACCOUNT,RFLPRSN),AL2(1683)                         
         DC    AL1(RTDACCT),AL1(RTVCLIA)                                        
* Direct Expense Account                                                        
         DC    AL1(FLDNDEXP,LACCOUNT,RFLXCAT),AL2(1686)                         
         DC    AL1(RTDACCT),AL1(RTVCLIA)                                        
* Vendor Account                                                                
         DC    AL1(FLDNVNDR,LULACNT,RFLVNDR),AL2(1684)                          
         DC    AL1(RTDACCT),AL1(RTVCLIA)                                        
* Analysis office                                                               
         DC    AL1(FLDNAOFF,L'ACQANOF,RFLAOFF),AL2(0)                           
         DC    AL1(RTDGENC),AL1(RTVOFFC)                                        
* Client office                                                                 
         DC    AL1(FLDNOFCC,L'ACQANOF,RFLCOFF),AL2(1687)                        
         DC    AL1(RTDGENC),AL1(RTVOFFC)                                        
* Filter 1 - 5                                                                  
         DC    AL1(FLDNFLT1,L'ACQACTF1,0),AL2(0)                                
         DC    AL1(RTDFLTR),AL1(RTVGENR)                                        
         DC    AL1(FLDNFLT2,L'ACQACTF2,0),AL2(0)                                
         DC    AL1(RTDFLTR),AL1(RTVGENR)                                        
         DC    AL1(FLDNFLT3,L'ACQACTF3,0),AL2(0)                                
         DC    AL1(RTDFLTR),AL1(RTVGENR)                                        
         DC    AL1(FLDNFLT4,L'ACQACTF4,0),AL2(0)                                
         DC    AL1(RTDFLTR),AL1(RTVGENR)                                        
         DC    AL1(FLDNFLT5,L'ACQACTF5,0),AL2(0)                                
         DC    AL1(RTDFLTR),AL1(RTVGENR)                                        
* Contra Filter 1 - 5                                                           
         DC    AL1(FLDNFLC1,L'ACQCFLT1,RFLCFLT1),AL2(0)                         
         DC    AL1(RTDGENC),AL1(RTVGENR)                                        
         DC    AL1(FLDNFLC2,L'ACQCFLT2,RFLCFLT2),AL2(0)                         
         DC    AL1(RTDGENC),AL1(RTVGENR)                                        
         DC    AL1(FLDNFLC3,L'ACQCFLT3,RFLCFLT3),AL2(0)                         
         DC    AL1(RTDGENC),AL1(RTVGENR)                                        
         DC    AL1(FLDNFLC4,L'ACQCFLT4,RFLCFLT4),AL2(0)                         
         DC    AL1(RTDGENC),AL1(RTVGENR)                                        
         DC    AL1(FLDNFLC5,L'ACQCFLT5,RFLCFLT5),AL2(0)                         
         DC    AL1(RTDGENC),AL1(RTVGENR)                                        
* Select field (APG)                                                            
         DC    AL1(FLDNSELT,L'ACQSEL,0),AL2(0)                                  
         DC    AL1(RTDSELT),AL1(RTVGENR)                                        
* Option 1 - 7                                                                  
         DC    AL1(FLDNOPT1,L'ACQOPT1,0),AL2(0)                                 
         DC    AL1(RTDOPTS),AL1(RTVOPTS)                                        
         DC    AL1(FLDNOPT2,L'ACQOPT2,0),AL2(0)                                 
         DC    AL1(RTDOPTS),AL1(RTVOPTS)                                        
         DC    AL1(FLDNOPT3,L'ACQOPT3,0),AL2(0)                                 
         DC    AL1(RTDOPTS),AL1(RTVOPTS)                                        
         DC    AL1(FLDNOPT4,L'ACQOPT4,0),AL2(0)                                 
         DC    AL1(RTDOPTS),AL1(RTVOPTS)                                        
         DC    AL1(FLDNOPT5,L'ACQOPT5,0),AL2(0)                                 
         DC    AL1(RTDOPTS),AL1(RTVOPTS)                                        
         DC    AL1(FLDNOPT6,L'ACQOPT6,0),AL2(0)                                 
         DC    AL1(RTDOPTS),AL1(RTVOPTS)                                        
         DC    AL1(FLDNOPT7,L'ACQOPT7,0),AL2(0)                                 
         DC    AL1(RTDOPTS),AL1(RTVOPTS)                                        
* ? (APG)                                                                       
         DC    AL1(FLDNNARA,L'ACQCOMNT,0),AL2(0)                                
         DC    AL1(RTDGENC),AL1(RTVNARR)                                        
* Budgets                                                                       
         DC    AL1(FLDNBUDG,1,0),AL2(0)                                         
         DC    AL1(RTDBUDG),AL1(RTVBUDG)                                        
* Format detail report ?                                                        
         DC    AL1(FLDNFRPT,LENFFRPT,0),AL2(0)                                  
         DC    AL1(RTDFRPT),AL1(RTVRYNO)                                        
* Recap                                                                         
         DC    AL1(FLDNRCAP,LENFRCAP,0),AL2(0)                                  
         DC    AL1(RTDRECAP),AL1(RTVRECAP)                                      
* Method                                                                        
         DC    AL1(FLDNMTHD,L'ACQMTHD,0),AL2(0)                                 
         DC    AL1(RTDMTHD),AL1(RTVMTHD)                                        
* Curency code                                                                  
         DC    AL1(FLDNCURC,L'ACQBILGP,0),AL2(0)                                
         DC    AL1(RTDGENC),AL1(RTV2DEC)                                        
*  ?                                                                            
         DC    AL1(FLDNOVHD,LENFOVHD,0),AL2(0)                                  
         DC    AL1(RTDGENC),AL1(RTV2DEC)                                        
*  Period range                                                                 
         DC    AL1(FLDNPRDR,L'ACQSTART+L'ACQEND,0),AL2(0)                       
         DC    AL1(RTDDTES),AL1(RTVRNGE)                                        
*  Calendar range                                                               
         DC    AL1(FLDNCNDR,L'ACQSTART+L'ACQEND,0),AL2(0)                       
         DC    AL1(RTDDTES),AL1(RTVRNGE)                                        
*  Locations                                                                    
         DC    AL1(FLDNLOCS,L'ACQLOCS,0),AL2(0)                                 
         DC    AL1(RTDLOCS),AL1(RTVLOCS)                                        
* ?                                                                             
         DC    AL1(FLDNRCON,L'ACQLOCS,0),AL2(0)                                 
         DC    AL1(RTDRCON),AL1(RTVRYNO)                                        
* ?                                                                             
         DC    AL1(FLDNPRSC,LENFPRSC,0),AL2(1683)                               
         DC    AL1(RTDNONE),AL1(RTVPRSC)                                        
* Authorization                                                                 
         DC    AL1(FLDNAUTH,LENFAUTH,0),AL2(0)                                  
         DC    AL1(RTDNONE),AL1(RTVAUTH)                                        
*                                                                               
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   20                                                               
BLOCK    DS    (MAXPARM)CL32       DATA BLOCK FOR SCANNER                       
DWORK    DS    D                                                                
TMPPARM  DS    8F                                                               
APID     DS    A                   A(ACQPID area) in 3rd card                   
SVRE     DS    A                                                                
SVRF     DS    A                                                                
SVIO3    DS    A                                                                
AFLDUNLG DS    A                   DISP TO UNIT/LEDGER FIELD HEADER             
ASCRTAB  DS    A                   A(Current screen table entry)                
AREQLOC  DS    A                   A(Current request card location)             
ATEXTFLD DS    A                   TEXT FIELD TO INPUT FIELD                    
ALOADER  DS    A                                                                
AFAPQSEC DS    A                                                                
AAPGIO   DS    A                                                                
AACTHDRH DS    A                   A(Account code field header)                 
ARTND    DS    A                   A(Current routine table entry)               
SVSTARTH DS    A                   A(Start Date header)                         
SVACTSTH DS    A                   A(Activity start date header)                
SVPERDAH DS    A                   A(Person date header)                        
*&&UK                                                                           
AXDCOL#  DS    A                   A(Current XDATA col # table entry)           
*&&                                                                             
FFOUND   DS    XL1                 Format information found                     
FSTYELQ  EQU   X'80'               .   Found STYELQ element (x'25')             
FCOLUMN  EQU   X'40'               .   Found displayed column                   
FPERIOD  EQU   X'20'               .   Found PERIOD TYPE KEYWORD                
*RECAP   EQU   X'10'               .   Found recap to process                   
FMAIN    EQU   X'01'               .   Processing main format if on             
RUNJOB   DS    CL1                 L - Long, M - Med, S - Short                 
RUNLONG  EQU   C'L'                .   Long   running                           
RUNMED   EQU   C'M'                .   Medium running                           
RUNSHORT EQU   C'S'                .   Short  running                           
CONCODE  DS    XL1                                                              
NEWOPUT  DS    CL(L'REQOTYP)                                                    
SAVBUD   DS    CL10                                                             
SAVEUNL  DS    CL2                                                              
SAVEMTHD DS    CL1                                                              
SAVPFKEY DS    AL1                                                              
BYTE     DS    XL1                                                              
EXCLUDE  DS    XL1                 EXCLUDE YES OR NO                            
TYPEC5   DS    XL1                                                              
EXTFLD   DS    XL1                                                              
RPTIND   DS    XL1                 REPORT INDICTOR                              
RPTFILL  EQU   X'80'               .   FILL IN DATA FROM REPORT SCREEN          
RLPFLAG  DS    XL1                 RFP     INDICATOR                            
RLPFON   EQU   X'80'               .       RFP   IS   ON                        
RLPFALT  EQU   X'40'               .       ALTERNATE  DATE SYMBOL USED          
DTESWS   DS    XL1                 DATES   SWITCHES                             
DTESWSTA EQU   X'80'               .       START DATE                           
DTESW1DY EQU   X'40'               .       ONLY  ONE  DATE                      
CHGKEY   DS    CL1                                                              
CUROFF   DS    CL2                                                              
MOASTR   DS    CL4                 YYMM    MOA START                            
MOAEND   DS    CL4                 YYMM    MOA END                              
MMOSRNG  DS    CL12                YYMM    Media MOS range                      
ALTSDTE  DS    CL6                 YYMMDD  ALT START DATE                       
ALTEDTE  DS    CL6                 YYMMDD  ALT END   DATE                       
OCURDTE  DS    CL6                 YYMMDD OVERRIDE CURRENT DATE                 
NOPTS    DS    AL1                 NUMBER OF OPTIONS TO PROCESS                 
BUDOK    DS    CL1                 BUDGET NUMBER IS OKAY SWITCH                 
BUD#     DS    AL1                                                              
LBUDG#   EQU   5                   MAXIMUM LENGTH FOR NUMERIC BUDGET #          
LDGR#    DS    AL1                                                              
OPT1#    DS    AL1                                                              
OPT3#    DS    AL1                                                              
OPT4#    DS    AL1                                                              
BUDLIST  DS    CL16                                                             
PITCH    DS    CL2                                                              
RPTWIDTH DS    AL2                                                              
MAXWDPT  DS    AL2                 Max width for pitch                          
MAXWDLS  DS    AL2                 Max width for pitch                          
*&&UK                                                                           
PERSON   DS    CL8                 PERSON                                       
REQPID   DS    XL2                 BINARY PID                                   
PINNUM   DS    CL4                 PIN NUMBER                                   
XDATCO#  DS    XL(MAXCOLS+1)       SAVED XDATA COLUMN NUMBERS                   
*&&                                                                             
*                                                                               
RECAP#   DS    AL1                       Highest recap number                   
RCAPFMTS DS    (MAXRCP#)CL(RECAPLNQ)     Recap information table                
RQRCAPSW DS    CL(MAXRCP#)               ACQTYP1 recap flags Y's or N's         
*                                                                               
RCAPSW   DS    XL1                 Recap switch                                 
RCAPPROF EQU   X'40'               . At least 1 recap uses it's profile         
RCAPMIXL EQU   X'20'               . Mixed ledgers on recaps                    
*                                                                               
LDGLIST  DS    CL20                                                             
OPT1LST  DS    CL10                                                             
OPT3LST  DS    CL10                                                             
OPT4LST  DS    CL10                                                             
OPTNME   DS    CL16                OPTION NAME TO DISPLAY                       
OPTIDNUM DS    AL1                 OPTION ID   NUMBER                           
OPTEQU   DS    AL1                 OPTION TYPE EQUATE                           
OPTVALUE DS    AL1                 VALUE  IN   OPTION                           
MULTIUL  DS    CL1                 HAVE MULTIPLE U/LS  - YES OR NO              
*                                                                               
*EPIND   DS    CL1                                                              
*EPBEFR  EQU   C'B'                                                             
*EPAFTR  EQU   C'A'                                                             
*EPNULL  EQU   C'N'                                                             
*                                                                               
SVRECON  DS    CL1                 RECONCILED ITEMS (Y,N,O)                     
*                                                                               
SVFRPT   DS    CL1                 SAVE FORMAT REPORT                           
*                                                                               
SAVELOCS DS    CL1                 SAVE AREA FOR LOCATION STATUS VALUE          
LOSTATUS DS    CL(LENFLOCS-1)      LOCATION STATUS WORK AREA                    
LLOSTATU EQU   L'LOSTATUS          LENGTH OF LOCATION STATUS WORK AREA          
*                                                                               
DUMTRNTH DS    CL8                 DUMMY TRANSACTION TYPE HEADER                
DUMTRNT  DS    CL(LENFTTYP)        DUMMY TRANSACTION TYPE FIELD                 
*                                                                               
DUMFLDH  DS    CL8                 DUMMY FIELD HEADER FOR GDEFTEXT              
DUMFLD   DS    CL8                 DUMMY FIELD        FOR GDEFTEXT              
*                                                                               
REQREC   DS    0CL(80*9)           REQUEST RECORD                               
REQHDR   DS    0CL80                                                            
REQRECN  DS    CL54                REQUEST EXTENDED HEADER                      
REQHEAD  DS    0CL26               REQUEST HEAD                                 
         SPACE 3                                                                
       ++INCLUDE DMREQHDR                                                       
*&&UK                                                                           
         ORG   REQDATE                                                          
RQHFLG1  DS    XL1                 DEFINES TYPE OF DATA IN NEXT FIELD           
RQHFPIN  EQU   2                   FIELD CONTAINS A PIN                         
RQHFPID  EQU   3                   FIELD CONTAINS A PID                         
RQHSECD  DS    XL5                 SECURITY DATA DEFINED BY RQHFLG1             
         ORG                                                                    
*&&                                                                             
         SPACE 3                                                                
REQCARDS DS    0C                                                               
REQCARD1 DS    CL80                                                             
REQCARD2 DS    CL80                                                             
REQCARD3 DS    CL80                                                             
REQCARD4 DS    CL80                                                             
REQCARD5 DS    CL80                                                             
REQCARD6 DS    CL80                                                             
REQCARD7 DS    CL80                                                             
REQCARD8 DS    CL80                                                             
DUB      DS    D                                                                
WORK     DS    CL20                                                             
TESTCHR  DS    CL1                 "A", "B", "C" or "S"                         
SOFBLOCK DS    XL(SOFDATL)         BLOCK FOR SOFDAT CALLS                       
LWSX     DS    0C                                                               
         EJECT ,                                                                
RTND     DSECT                     ** ROUTINE TABLE DSECT **                    
RTNFLDN  DS    XL1                 ROUTINE FIELD NUMBER                         
RTNFLDLN DS    XL1                 ROUTINE FIELD LENGTH W/O EXCLUDE             
RTNPROFN DS    XL1                 FIELD PROFILE NUMBER                         
RTNGTXNO DS    XL2                 GETTEXT NUMBER                               
*                                                                               
RTNDDISP DS    AL1                 DISPLAY  ROUTINE #                           
RTDNONE  EQU   0                                                                
RTDULAC  EQU   1                                                                
RTDGENC  EQU   2                                                                
RTDBUDG  EQU   3                                                                
RTDOPTS  EQU   4                                                                
RTDSELT  EQU   5                                                                
RTDESTS  EQU   6                                                                
RTDACCT  EQU   7                                                                
RTDFLTR  EQU   8                                                                
RTDRECAP EQU   9                   DISPLAY RECAP FIELD                          
RTDMTHD  EQU   10                                                               
RTDDTES  EQU   11                                                               
RTDLOCS  EQU   12                                                               
RTDRCON  EQU   13                                                               
RTDFRPT  EQU   14                                                               
*                                                                               
RTNVDISP DS    AL1                 VALIDATE ROUTINE #                           
RTVUNLG  EQU   1                                                                
RTVACNT  EQU   2                                                                
RTVCNTR  EQU   3                                                                
RTVOFGP  EQU   4                                                                
RTVGENR  EQU   5                                                                
RTVWKCD  EQU   6                                                                
RTVESTS  EQU   7                                                                
RTVDTES  EQU   8                                                                
RTVMOAR  EQU   9                                                                
RTVALDT  EQU   10                                                               
RTVTRNT  EQU   11                                                               
RTVCLIA  EQU   12                                                               
RTVBUDG  EQU   13                                                               
RTVOPTS  EQU   14                                                               
RTV2DEC  EQU   15                                                               
RTVNARR  EQU   16                                                               
RTVMTHD  EQU   17                                                               
RTVRNGE  EQU   18                                                               
RTVLOCS  EQU   19                                                               
RTVPRSC  EQU   20                                                               
RTVBILT  EQU   21                  Vaildate billing type                        
RTVRECAP EQU   22                  Validate recap field                         
RTVRYNO  EQU   23                  Validate YES, NO or ONLY                     
RTVAUTH  EQU   24                  Validate Authorization                       
RTVOFFC  EQU   25                  Validate office                              
*                                                                               
RTNLNQ   EQU   *-RTND                                                           
         EJECT ,                                                                
RECAPD   DSECT                                                                  
RECAPFMT DS    CL8                 Recap format                                 
RECAPIND DS    XL1                 Recap indicators, see RCPPROF                
RECAPERR DS    AL2                 Recap error                                  
RECAPLNQ EQU   *-RECAPD                                                         
         SPACE 1                                                                
CHKTABD  DSECT                                                                  
CHKTYPE  DS    AL1                 Field # to process                           
CHKMLN   DS    XL1                 Maximum length                               
CHKCPY   DS    XL1                 Displacement to company in key               
CHKDSP   DS    XL1                 Displacement to data    in key               
CHKKEY   DS    CL7                 Static data of key                           
CHKFIND  DS    XL1                 Indicators                                   
CHKFSPC  EQU   X'10'               .   Space filled key                         
CHKTABLN EQU   *-CHKTYPE                                                        
                                                                                
PITCHD   DSECT                                                                  
PITCH#   DS    AL1                 Pitch number - 0,10,12,15,18,20,24           
PITWDPT  DS    AL2                 Portriat  max width for pitch                
PITWDLS  DS    AL2                 Landscape max width for pitch                
PITCHLNQ EQU   *-PITCHD                                                         
         EJECT ,                                                                
       ++INCLUDE ACSCROTYPD                                                     
         EJECT ,                                                                
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*ACAPGEQU                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACAPGEQU                                                       
         PRINT ON                                                               
*DDSOFDATD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSOFDATD                                                      
         PRINT ON                                                               
*ACSCRWRK                                                                       
       ++INCLUDE ACSCRWRK                                                       
         EJECT ,                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRE8D                                                       
*DDTWABLDD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072ACSCR14   10/10/19'                                      
         END                                                                    
