*          DATA SET ACSCR07    AT LEVEL 117 AS OF 09/02/15                      
*PHASE T60C07A                                                                  
*&&ONLIN SET   Y                                                                
*INCLUDE LOADER                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 117 AS OF 11/19/12        *         
*                                                                     *         
***********************************************************************         
         TITLE 'LIST OUT FORMAT/REQUEST AND MULTIPLE REQUEST'                   
                                                                                
* PID  LVl DATE    COMMENTS                                                     
* ---------------------------------                                             
* TFRY 25SEP09 113 <LO01-9062> SECURE REPORT BY PIN/PID                         
* TFRY 17NOV09 114 <BR14890D>  PREVENT USE OF , AS DELIMITER IN GERMANY         
*                              IN REQUESTOR NAME                                
* SMAN ?????10 115 <PR000414> UK/NY SCRIBE MERGE                                
* SMAN 16DEC11 116 <PR002242> UK/NY SCRIBE MERGE                                
* JFOS 17MAY12 117 <PR002375> SET RUNJOB ACCORDING TO LGR/ACC TRX COUNT         
                                                                                
T60C07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C07,RA,R9,RR=RE                                              
         SPACE 1                                                                
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         L     RF,=V(LOADER)                                                    
         AR    RF,RE                                                            
         ST    RF,ALOADER                                                       
         MVC   DMRDIR,=CL8'DMRDIR'                                              
         MVC   DMWRT,=CL8'DMWRT'                                                
*                                                                               
         USING RESRECD,R2                                                       
SCR30    CLI   APMODE,APMVALR      VALIDATE RECORD MODE?                        
         BNE   SCR50               NO                                           
         CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APPFKEY,PFKEXIT     RETURN?                                      
         BNE   SCR50               NO                                           
         TM    TWASWPST,TWASWAP    SHOULD WE SWAP?                              
         BZ    EXIT                                                             
         MVI   APPFKEY,0                                                        
         MVI   APMODE,APMSWP       SWAP                                         
         MVC   APPARM(1),TWASWPRE                                               
         MVC   APPARM+1(1),TWASWPAC                                             
         B     EXIT                                                             
*                                                                               
SCR50    LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE           GET MODE                                     
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     EXIT                VALIDATE KEY                                 
         B     EXIT                VALIDATE VALREQ (ACTUALLY VALREC)            
         B     EXIT                DISPLAY KEY                                  
         B     EXIT                DISPLAY REQUEST                              
         B     EXIT                DELETE RECORD                                
         B     EXIT                RESTORE RECORD                               
         B     VALSEL              VALSEL                                       
         B     GETSEL              GETSEL                                       
         B     DISSEL              DISSEL                                       
         B     EXIT                FIRST FOR LIST SELECT SCREEN                 
         B     EXIT                                                             
         B     PROCLST             PROCESS LIST/SELECT                          
*&&UK*&& B     SELSCRF             FIRST TIME FOR LIST SCREEN                   
*&&US*&& B     EXIT                FIRST TIME FOR LIST SCREEN                   
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ  (NOT USED)                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                COPY RECORD                                  
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
XIT      DS    0H                                                               
*                                                                               
EXIT99   XIT1                                                                   
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO DISPLAY SELECT RECORD                             *         
*        ENTER FROM --> REPORT LIST                                   *         
*              VALIDATE ORIENTATION LANDSCAPE/PORTRAIT                *         
*              VALIDATE OUTPUT                                        *         
*              VALIDATE DESTINATION                                   *         
*              VALIDATE MOA RANGE                                     *         
*              VALIDATE START DATE                                    *         
*              VALIDATE END DATE                                      *         
*              REQUESTOR                                              *         
*              VALIDATE FORMAT REPORT                                 *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
VALSEL   MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLC   FLTFORM,SPACES                                                   
         BE    VALSEL01                                                         
         MVC   RUNCODE,FLTFORM                                                  
         OI    RUNCODEH+6,FVOXMT                                                
         MVC   FLTFORM,SPACES                                                   
*                                                                               
VALSEL01 TM    RUNCODEH+4,FVITHIS                                               
         BZ    *+8                                                              
         MVI   PTRELNRQ,0                                                       
         MVC   RQOUTP,SPACES                                                    
         MVC   RQDEST,SPACES                                                    
         MVC   RQUL,SPACES                                                      
         MVC   RQACC,SPACES                                                     
         MVI   RQCURCY,C'1'                                                     
         XC    RQSTR,RQSTR                                                      
         XC    RQEND,RQEND                                                      
         XC    RQMOAS,RQMOAS                                                    
         XC    RQMOAE,RQMOAE                                                    
*&&US                                                                           
         GOTO1 AFVAL,RUNOPT2H                                                   
         BNE   VALSEL05                                                         
         CLI   RUNOPT2,C'L'        LANDSCAPE?                                   
         BE    VALSEL05                                                         
         CLI   RUNOPT2,C'P'        PORTRAIT?                                    
         BNE   IVALIPUT                                                         
*&&                                                                             
*                                                                               
VALSEL05 GOTO1 AFVAL,RUNOUTH       ANY  OUTPUT TYPE ?                           
         BNE   VALSEL20            NO,  SKIP                                    
         CLI   RUNCODE,C'@'        SQL  ?                                       
         BE    VALSEL20            YES, SKIP                                    
*                                                                               
         USING OUTTYPD,RE                                                       
         LA    R0,OUTTYPQ          NUMBER TO CHECK FOR                          
         L     RE,=A(OUTTYPES)     OUTPUT TYPES CONVERSION TABLE                
         A     RE,APRELO                                                        
*                                                                               
VALSEL10 CLC   OUTTYOLD,FVIFLD     OLD  TYPE ?                                  
         BE    VALSEL15            YES, CONVERT                                 
         CLC   OUTTYNEW,FVIFLD     NEW  TYPE                                    
         BE    VALSEL20            YES, USE  TYPE                               
         LA    RE,OUTTYLNQ(,RE)    GET  NEXT TABLE ENTRY                        
         BCT   R0,VALSEL10                                                      
         MVC   FVMSGNO,=AL2(FVFEOUT) SET INVALID OUTPUT TYPE                    
         B     VALSEL90                                                         
*                                                                               
VALSEL15 MVC   RUNOUT,SPACES                  CLEAR TYPE                        
         MVC   RUNOUT(L'OUTTYNEW),OUTTYNEW    USE   NEW TYPE                    
         OI    RUNOUTH+6,FVOXMT               TRANSMIT                          
         DROP  RE                                                               
*                                                                               
VALSEL20 GOTO1 AVALOTYP,RUNOUTH               VALIDATE TYPE                     
         BNE   VALSEL90                                                         
         MVC   RQOUTP,FVIFLD       SAVE OFF OUTPUT TYPE                         
         GOTO1 AVALDEST,RUNDESTH                                                
         BNE   VALSEL90                                                         
         MVC   RQDEST,FVIFLD                                                    
         GOTO1 AFVAL,RUNUNLH                                                    
         BNE   *+10                                                             
         MVC   RQUL,FVIFLD                                                      
         GOTO1 AFVAL,RUNACCH                                                    
         BNE   VALSEL22                                                         
         LA    RE,FVIFLD                                                        
         CLI   FVIFLD,C'*'         EXLCLUDE ?                                   
         BNE   VALSEL21                                                         
         LA    RE,FVIFLD+1                                                      
         NI    0(RE),TURNOFF-X'40'                                              
*                                                                               
VALSEL21 MVC   RQACC,0(RE)                                                      
*                                                                               
VALSEL22 GOTO1 VALMOA,RUNMOAH                                                   
         BNE   VALSEL90                                                         
         CLC   MOASTR,SPACES                                                    
         BNH   VALSEL24                                                         
         MVC   APWORK(L'MOASTR),MOASTR                                          
         MVC   APWORK+4(2),=C'01'                                               
         GOTO1 VDATCON,APPARM,(0,APWORK),(1,APWORK)                             
         MVC   RQMOAS,APWORK                                                    
*                                                                               
VALSEL24 CLC   MOAEND,SPACES                                                    
         BNH   VALSEL25                                                         
         MVC   APWORK(L'MOAEND),MOAEND                                          
         MVC   APWORK+4(2),=C'01'                                               
         GOTO1 VDATCON,APPARM,(0,APWORK),(1,APWORK)                             
         MVC   RQMOAE,APWORK                                                    
*                                                                               
VALSEL25 GOTO1 VALDTES,APPARM,RUNSTDH,STRDTE                                    
         BNE   VALSEL90                                                         
*                                                                               
         GOTO1 VALDTES,APPARM,RUNEDDH,ENDDTE                                    
         BNE   VALSEL90                                                         
         OC    STRDTE,STRDTE                                                    
         BZ    VALSEL30                                                         
         GOTO1 VDATCON,APPARM,(0,STRDTE),(1,RQSTR)                              
*                                                                               
VALSEL30 OC    ENDDTE,ENDDTE       IS   THERE AN   END    DATE ?                
         BZ    VALSEL40            NO,  SKIP                                    
         GOTO1 VDATCON,APPARM,(0,ENDDTE),(1,RQEND)                              
*                                                                               
         OC    STRDTE,STRDTE       IS   THERE A    START  DATE ?                
         BZ    VALSEL40            NO,  SKIP                                    
         CLC   STRDTE,ENDDTE       IS   START DATE BEFORE END DATE ?            
         BH    IVALDATE            NO,  ERROR                                   
*                                                                               
VALSEL40 GOTO1 AFVAL,RUNPERH                                                    
         BE    VALSEL50                                                         
         MVC   RUNPER(L'TWAPERSN),TWAPERSN                                      
         OI    RUNPERH+6,FVOXMT                                                 
*                                                                               
VALSEL50 DS    0H                                                               
         CLC   TWAPERSN,FVIFLD                                                  
         BE    *+10                DEFAULT SET                                  
         MVC   RQNAME,FVIFLD                                                    
*&&UK                                                                           
         TM    GENIND,GENEURO                                                   
         BZ    VALSEL53                                                         
         CLI   RUNRCU,C' '         ANY  CURRENCY # ?                            
         BH    *+8                                                              
         MVI   RUNRCU,ACQC1ST      DEFAULT TO C'1'                              
         CLI   RUNRCU,ACQC1ST      C'1'                                         
         BE    VALSEL52                                                         
         CLI   RUNRCU,ACQC2ND      C'2'                                         
         BNE   IVALIPUT                                                         
*                                                                               
VALSEL52 MVC   RQCURCY,RUNRCU                                                   
*                                                                               
VALSEL53 CLI   CUCTRY,X'03'        PREVENT INPUT OF \ OR , AS DELIMITER         
         BNE   VALSEL55            FOR GERMANY                                  
         LA    R8,RUNPERH          (ONLY # IS VALID)                            
         LA    R6,L'RUNPER                                                      
VALSEL54 CLI   0(R8),C','                                                       
         BE    IVALIPUT                                                         
         CLI   0(R8),C'\'                                                       
         BE    IVALIPUT                                                         
         LA    R8,1(R8)                                                         
         BCT   R6,VALSEL54                                                      
*&&                                                                             
         USING SCANBLKD,R8                                                      
VALSEL55 LA    R8,BLOCK                                                         
*&&US*&& GOTO1 VSCANNER,APPARM,RUNPERH,(2,BLOCK),SCNP3NEQ                       
                                                                                
*&&UK                                                                           
         GOTO1 VSCANNER,APPARM,RUNPERH,(3,BLOCK),SCNP3NEQ                       
         CLC   SCONEFLD(3),=C'PIN'                                              
         BE    IVALUSER                                                         
         CLC   SCONEFLD(3),=C'PID'                                              
         BE    IVALUSER                                                         
         CLC   SCONEFLD(2),=C'I='                                               
         BE    IVALUSER                                                         
         CLC   SCONEFLD(2),=C'N='                                               
         BE    IVALUSER                                                         
*&&                                                                             
         MVC   SVNAME,SCONEFLD                                                  
         MVC   INUSER,SCONEFLD                                                  
*&&UK                                                                           
         CLI   APPARM+4,2                                                       
         BL    VALSEL56                                                         
         AHI   R8,SCBLKLQ                                                       
         BRAS  RE,VALPIN                                                        
         BNE   IVALEXIT                                                         
         CLI   REQBYTE,0                                                        
         BNE   VALSEL56                                                         
         MVC   INUSER,SCONEFLD                                                  
         CLI   APPARM+4,3                                                       
         BL    VALSEL56                                                         
         AHI   R8,SCBLKLQ                                                       
         BRAS  RE,VALPIN                                                        
         BNE   IVALEXIT                                                         
*&&                                                                             
*&&US                                                                           
         CLI   APPARM+4,2                                                       
         BNE   *+10                                                             
         MVC   INUSER,SCONEFLD                                                  
*&&                                                                             
VALSEL56 MVI   RQFRPT,C' '         CLEAR FORMAT REPORT                          
         GOTO1 AFVAL,RUNFRPH       ANY   FORMAT REPORT ?                        
         BNE   VALSEL80            NO,   SKIP                                   
         CLI   APGFLAG,YES         IS IT APG ?                                  
         BE    IVALIPUT            YES,  INVALID                                
         CLC   RUNFRP,APNO         'NO'  (SKIP FORMAT REPORT) ?                 
         BNE   VALSEL60                                                         
         MVI   RQFRPT,NO                                                        
         B     VALSEL80                                                         
*                                                                               
VALSEL60 CLC   RUNFRP,APYES        'YES' (GEN  FORMAT REPORT) ?                 
         BNE   VALSEL65                                                         
         CLC   RUNOUT,=CL8'DOWN'   DOWN-LOAD   ALSO ?                           
         BE    IVALFRPT            YES,   DOWN-LOAD & FORMAT REPORT NG          
         MVI   RQFRPT,YES                                                       
         B     VALSEL80                                                         
*                                                                               
VALSEL65 CLC   RUNFRP,APONLY       'ONLY' (ONLY FORMAT REPORT) ?                
         BNE   IVALIPUT                                                         
         MVI   RQFRPT,ONLY                                                      
*                                                                               
         USING RESRECD,R2                                                       
VALSEL80 LA    R0,RUNSEL1H                                                      
         ST    R0,APPARM                                                        
         LA    R1,RUNSEL2H                                                      
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,11         ONLY 11 LINES OF SELECT                      
         LA    R2,APRECKEY                                                      
*                                                                               
         CLI   APGFLAG,YES         IS IT APG?                                   
         BE    VALSEL85                                                         
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         MVI   KEYSUB,RESKSUBQ                                                  
         B     VALSEL90                                                         
*                                                                               
         USING APGRECD,R2                                                       
VALSEL85 MVC   APGKEY,SPACES                                                    
         MVI   APGKTYP,APGKTYPQ    X'2D'                                        
         MVI   APGKSUB,APGKSUBQ    X'07'                                        
         MVC   APGKCPY,CUABIN                                                   
         MVI   KEYSUB,APGKSUBQ                                                  
*                                                                               
VALSEL90 B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
* Routine to validate pin/pid entered                                 *         
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
                                                                                
VALPIN28 MVI   REQBYTE,RQHFPID     SAVE PID                                     
         MVC   REQSEC(2),CUAGYSEC                                               
         MVC   REQSEC+2(2),REQPID                                               
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
         MVI   REQBYTE,RQHFPIN                                                  
         MVC   REQSEC,PINNUM                                                    
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
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*---------------------------------------------------------------------*         
*        ROUTINE TO GET SELECT RECORD                                 *         
*        ENTER FROM --> REPORT LIST                                   *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
GETSEL   LA    R2,IOKEY                                                         
         MVC   RESKEY,APRECKEY                                                  
         MVC   SAVLSTKY,APRECKEY            SAVE CURRENT KEY                    
         TM    INOPTA,INOAEXPY+INOAEXPO+INOAEXPN  EXPAND REQUESTS?              
         BZ    GETSEL07                                                         
         CLI   PTRELNRQ,0                                                       
         BE    GETSEL07                                                         
         CLC   RESKEY+RESKFRML+1(1),PTRELNRQ      FINISHED REQUESTS?            
         BL    GETSEL03                                                         
         BH    GETSEL02                                                         
         CLI   APINDS,APILNLST                                                  
         BE    GETSEL93                                                         
*                                                                               
GETSEL02 MVC   RESKEY+RESKFRML(2),SPACES          RESET THE FORMAT              
         MVI   PTRELNRQ,0                                                       
         OI    APINDS,APILRERD+APILNSEQ           RE-READ RECORD & SEQ          
         B     GETSEL07                                                         
*                                                                               
GETSEL03 ZIC   RF,RESKEY+RESKFRML+1                                             
         CLI   APINDS,APILNLST     FIRST ON NEXT SCREEN                         
         BE    *+8                                                              
         LA    RF,1(,RF)                                                        
         STC   RF,RESKEY+RESKFRML+1                                             
         MVI   RESKEY+RESKFRML,0                                                
         B     GETSEL93                                                         
*                                                                               
GETSEL07 TM    SCRTYPH+4,FVITHIS   SAME AS BEFORE?                              
         BNZ   GETSEL08            NO, SO RESET                                 
         TM    RUNCODEH+4,FVITHIS                                               
         BNZ   GETSEL08            NO, SO RESET                                 
         TM    RUNOWNH+4,FVITHIS                                                
         BZ    *+8                                                              
*                                                                               
GETSEL08 MVI   RESKEY,0            NO, RESET TO START OVER                      
         NI    SCRTYPH+4,TURNOFF-FVITHIS                                        
         NI    RUNCODEH+4,TURNOFF-FVITHIS                                       
         NI    RUNOWNH+4,TURNOFF-FVITHIS                                        
         CLI   RESKEY,RESKTYPQ     X'2D'                                        
         BNE   GETSEL10                                                         
         CLC   RESKSUB,KEYSUB      X'02' OR X'07'                               
         BNE   GETSEL10                                                         
         CLC   RESKCPY,CUABIN      COMPANY CODE                                 
         BE    GETSEL20                                                         
*                                                                               
GETSEL10 MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVC   RESKSUB,KEYSUB      X'02' OR X'07'                               
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,RUNCODEH      ANY     FORMAT CODE ?                        
         BNE   GETSEL60            READ    FIRST  RECORD                        
         SR    R6,R6                                                            
         IC    R6,FVXLEN           GET     FORMAT CODE EX   LENGTH              
         LA    RE,FVIFLD(R6)       ->      FORMAT CODE LAST CHARACTER           
         CLI   0(RE),C'*'          USER    WANTS  STARTING  FROM CODE ?         
         BNE   *+6                 NO,     SKIP                                 
         BCTR  R6,0                YES,    DON'T  MOVE THE  C"*"                
         EXMVC R6,RESKFORM,FVIFLD  MOVE    FORMAT CODE                          
         B     GETSEL60                                                         
*                                                                               
GETSEL20 TM    APINDS,APILRERD     RE-READ RECORD, IO OCCURED                   
         BZ    GETSEL40                                                         
         GOTO1 AIO,IORD+IOACCFIL+IO1                                            
         BE    GETSEL80                                                         
         B     GETSEL95                                                         
*                                                                               
GETSEL40 TM    APINDS,APILNSEQ     CONTINUE WITH SEQUENTIAL READ?               
         BNZ   GETSEL80                                                         
*                                                                               
GETSEL60 LA    R1,IOHI+IOACCFIL+IO1                                             
         B     *+8                                                              
*                                                                               
GETSEL80 LA    R1,IOSQ+IOACCFIL+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSEL95                                                         
         L     R2,AIOAREA1                                                      
         CLI   RESKEY,RESKTYPQ      X'2D'                                       
         BNE   GETSEL95                                                         
         CLC   RESKSUB,KEYSUB       X'02' OR X'07'                              
         BNE   GETSEL95                                                         
         CLC   RESKCPY,CUABIN       COMPANY                                     
         BNE   GETSEL95                                                         
***********************************************************************         
*  REPORT TYPE FILTER                                                 *         
***********************************************************************         
         SPACE 1                                                                
         CLI   RESKSEQ,RESKSREG    REGULAR FORMAT RECORD?                       
         BNE   GETSEL80            NO SO GET NEXT                               
         L     R6,AIOAREA1                                                      
         MVI   APELCODE,STYELQ     X'25'                                        
         GOTO1 GETEL,(R6)                                                       
         BNE   GETSEL80                                                         
         CLC   APREPCDE,STYNAME-STYELD(R1)                                      
         BNE   GETSEL80                                                         
***********************************************************************         
*  FILTER OUT FORMAT CODES                                            *         
***********************************************************************         
         SPACE 1                                                                
GETSEL84 GOTO1 AFVAL,RUNCODEH                                                   
         BNE   GETSEL85            NO INPUT IN OPTION                           
         SR    R6,R6                                                            
         CLI   APGFLAG,YES         NOT APPLICABLE FOR APG                       
         BE    GETSEL85                                                         
         IC    R6,FVXLEN                                                        
         LA    R1,FVIFLD(R6)                                                    
         LA    RF,X'70'            BRANCH NOT EQUAL                             
         CLI   0(R1),C'*'                                                       
         BNE   *+10                                                             
         LA    RF,X'40'            BRANCH LOW                                   
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   RESKFORM(0),FVIFLD                                               
*&&UK*&& EX    RF,*+4                                                           
*&&US                                                                           
         EX    RF,*+6                                                           
         NOPR  0                                                                
*&&                                                                             
         BC    0,GETSEL80                                                       
***********************************************************************         
*  FILTER PERSON                                                      *         
***********************************************************************         
         SPACE 1                                                                
GETSEL85 GOTO1 AFVAL,RUNOWNH                                                    
         BNE   GETSEL90            NO INPUT IN OPTION                           
         SR    R6,R6                                                            
         IC    R6,FVXLEN                                                        
         GOTO1 GETPER,APPARM,(R2),(L'TEMPOWN,TEMPOWN)                           
         BNE   GETSEL80                                                         
         EXCLC R6,TEMPOWN,FVIFLD                                                
         BNE   GETSEL80                                                         
***********************************************************************         
*  FILTER REQUEST DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING DTSELD,R3                                                        
GETSEL90 MVI   APELCODE,DTSELQ     Find time date stamp                         
         MVI   ELEMSEQ,DTSTREQ     For request only                             
         XC    RQDATE,RQDATE                                                    
         L     R6,AIOAREA1                                                      
         GOTO1 GETEL,(R6)                                                       
         BNE   GETSL90A                                                         
         LR    R3,R1                                                            
         GOTO1 VDATCON,APPARM,(2,DTSDATE),(1,RQDATE)                            
         DROP  R3                                                               
*                                                                               
GETSL90A TM    INOPT1,INORDTE                                                   
         BZ    GETSL90C                                                         
         TM    INOPTA,INOADTES     WAS A DATE INPUT ?                           
         BZ    GETSL90C            NO                                           
         CLC   RQDATE,OPTDTE1                                                   
         BL    GETSEL80            FILTER OUT THIS ONE                          
         CLC   RQDATE,OPTDTE2                                                   
         BH    GETSEL80            FILTER OUT THIS ONE                          
*                                                                               
GETSL90C TM    INOPTA,INOAEXPY+INOAEXPO+INOAEXPN       EXPAND REQUESTS?         
         BZ    GETSEL93                                                         
         MVI   PTRELNRQ,0          INITIALIZE # OF OVERNIGHT REQUESTS           
         OC    RQDATE,RQDATE       ANY REQUESTED DATE ?                         
         BZ    GETSEL91            NO                                           
         CLC   RQDATE,ASPDAT       DO WE HAVE ONE FOR TODAY?                    
         BNE   GETSEL91                                                         
         MVI   APELCODE,PTRELQ     FIND THE REQUESTS                            
         GOTO1 GETEL,(R6)                                                       
         BE    GETSEL92                                                         
*                                                                               
GETSEL91 TM    INOPTA,INOAEXPO     SHOW ONLY ONE'S WITH REQUEST                 
         BNZ   GETSEL80                                                         
         B     GETSEL93                                                         
*                                                                               
         USING PTRELD,R1                                                        
GETSEL92 TM    INOPTA,INOAEXPN                                                  
         BNZ   GETSEL80            DON'T PROCESS REQUESTED FORMATS              
         CLI   PTRTYPE,PTRTREQ     ARE THEY OVERNIGHT REQUESTS?                 
         BNE   GETSL92A                                                         
         ZIC   RF,PTRLN            CALCULATE # OF REQUESTS                      
         SHI   RF,PTRLN1Q                                                       
         SRL   RF,2                                                             
         STC   RF,PTRELNRQ         SAVE IT IN PTRELNRQ                          
         XC    RESKEY+RESKFRML(2),RESKEY+RESKFRML                               
*                                                                               
GETSL92A GOTO1 NEXTEL,(R1)                                                      
         BNE   GETSEL93                                                         
         CLI   PTRTYPE,PTRTREQ     ARE THEY OVERNIGHT REQUESTS?                 
         BNE   GETSL92A                                                         
         ZIC   RE,PTRLN            CALCULATE # OF REQUESTS                      
         SHI   RE,PTRLN1Q                                                       
         SRL   RE,2                                                             
         ZIC   RF,PTRELNRQ         GET IT FROM PTRELNRQ                         
         AR    RF,RE                                                            
         STC   RF,PTRELNRQ         SAVE IT IN PTRELNRQ                          
         B     GETSL92A                                                         
*                                                                               
GETSEL93 MVC   APRECKEY(L'RESKEY),RESKEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSEL99                                                         
*                                                                               
GETSEL95 MVI   APMODE,APMEOFS      END OF SELECT RECORDS                        
*                                                                               
GETSEL99 B     EXIT                                                             
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  LIST SCREEN FIRST TIME IN                                          *         
***********************************************************************         
         SPACE 1                                                                
*&&UK                                                                           
SELSCRF  CLI   RUNRCU,C' '         ANY  CURRENCY # ?                            
         BH    *+8                                                              
         MVI   RUNRCU,ACQC1ST              DEFAULT TO C'1'                      
         TM    GENIND,GENEURO      2ND CURRENCY CODE IN USE ?                   
         BO    SELSCRF9                                                         
         MVC   RUNRCF,SPACES                                                    
         OI    RUNRCFH+6,FVOXMT            XMIT FIELD                           
         MVI   RUNRCU,C' '                 REMOVE FIELD                         
         OI    RUNRCUH+6,FVOXMT+X'20'      PROTECT & XMIT FIELD                 
*                                                                               
SELSCRF9 B     EXIT                                                             
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO DISPLAY SELECT RECORD, ENTER FROM --> REPORT LIST       *         
***********************************************************************         
         SPACE 1                                                                
         USING LISTD,R4                                                         
DISSEL   DS    0H                                                               
         L     R2,AIOAREA1                                                      
         LA    R1,RESKFRML-1                                                    
         CLI   APGFLAG,YES                                                      
         BNE   *+8                                                              
         LA    R1,APGKFMTL-1                                                    
         EXCLC R1,RESKEY,APRECKEY                                               
         BE    DISSEL05                                                         
         MVC   IOKEY,APRECKEY                                                   
         MVC   IOKEY+RESKFRML(2),SPACES                                         
         GOTO1 AIO,IORD+IOACCFIL+IO1                                            
*                                                                               
DISSEL05 L     R4,APPARM           R4=A(LIST/SELECT LINE)                       
         TWAXC LISTDATH,LISTDATH                                                
         LA    R2,APRECKEY                                                      
         CLC   RESKEY+RESKFRML(2),SPACES     NO OVERNIGHT REQUESTS              
         BE    DISSEL10                                                         
         OC    RESKEY+RESKFRML(2),RESKEY+RESKFRML                               
         BNZ   DISSEL20                                                         
*                                                                               
DISSEL10 L     R2,AIOAREA1                                                      
         NI    LISTDATH+1,TURNOFF-FVAHIGH        UN-HIGHLIGHT                   
         MVC   LISTFMT(1),APGKFMT-APGRECD(R2)                                   
         CLI   APGFLAG,YES                                                      
         BE    *+10                                                             
         MVC   LISTFMT,RESKFORM                                                 
         MVC   LISTTYP,SCRTYP                                                   
         GOTO1 GETPER,APPARM,(R2),(L'LISTOWN,LISTOWN)                           
         GOTO1 GETNAME,APPARM,(R2),(L'LISTNME,LISTNME)                          
         XC    APBYTE,APBYTE                                                    
         TM    INOPT1,INORDTE                                                   
         BZ    *+8                                                              
         MVI   APBYTE,C'R'                                                      
         GOTO1 GETACT,APPARM,(APBYTE,(R2)),(L'LISTADTE,LISTADTE)                
         B     EXIT                                                             
*                                                                               
         USING PTRELD,R1                                                        
DISSEL20 DS    0H                                                               
         ZIC   R6,RESKEY+RESKFRML+1              REQUEST NUMBER                 
         EDIT  (R6),(3,LISTFMT)                                                 
         L     R2,AIOAREA1                                                      
         MVI   APELCODE,PTRELQ                                                  
         GOTO1 GETEL,(R2)          DID WE SAVE REQUEST ADDRESS?                 
         BNE   EXIT                                                             
*                                                                               
DISEL20A CHI   R6,63               MAX AMT OF REQ ADDRESSES ONE 'FA'            
         BNH   DISEL20B            ELEMENT CAN HOLD                             
         SHI   R6,63                                                            
         MVI   APELCODE,PTRELQ                                                  
         GOTO1 NEXTEL,(R1)                                                      
         BNE   EXIT                                                             
         B     DISEL20A                                                         
*                                                                               
DISEL20B LA    RE,PTRCODE                                                       
         LR    RF,R6                                                            
         BCTR  RF,0                                                             
         SLL   RF,2                MULTIPLY BY 4                                
         AR    RE,RF               POINT TO ADDRESS IN ELEMENT                  
*        LTR   RF,RF                                                            
*        BZ    DISSEL23                                                         
*                                                                               
*ISSEL22 LA    RE,4(,RE)                                                        
*        BCT   RF,DISSEL22                                                      
*                                                                               
         USING ACQD,R3                                                          
DISSEL23 MVC   ADDR,0(RE)          GET REQUEST ADDRESS                          
         GOTO1 VDMGR,APPARM,(X'20',DMRDIR),REQUEST,ADDR,AIOAREA3,DWORK          
         L     RE,AIOAREA3                                                      
         LA    RF,2*80             HEADER + ONE CARD                            
         LA    R3,80(,RE)                                                       
         CLI   ACQCONT1,C'C'       COUNT NUMBER OF CARDS                        
         BNE   DISSEL28                                                         
         LA    R0,6                                                             
         LA    RF,80(,RF)          INCREASE LENGTH                              
*                                                                               
DISSEL25 CLI   ACQCONT2,C'C'                                                    
         BNE   DISSEL28            FINISHED                                     
         LA    RF,80(,RF)          INCREASE LENGTH                              
         LA    R3,80(,R3)                                                       
         BCT   R0,DISSEL25                                                      
*                                                                               
DISSEL28 LA    R0,REQHDR                                                        
         LA    R1,9*80                                                          
         ICM   RF,8,=X'40'         PAD WITH SPACES                              
         MVCL  R0,RE                                                            
*                                                                               
DISSEL30 LA    R3,REQCARDS                                                      
         NI    LISTDATH+1,TURNOFF-X'08'                                         
         CLC   ACQPROG,=C'99'      REQUEST CANCELLED?                           
         BE    DISSEL35                                                         
         OI    LISTDATH+1,X'08'                                                 
         TM    ACQACT,X'40'                       UPPER  CASE ?                 
         BZ    DISSEL32                           NO,    MOVE WITH "*"          
         MVC   LISTNME1(LULACNT),ACQUNT           MOVE   DATA                   
         B     DISSEL40                           CONTINUE                      
*                                                                               
DISSEL32 DS    0H                                 EXCLUDE REQUESTED             
         MVC   LISTNME1(LUNLG),ACQUNT             INSERT UNIT/LEDGER            
         MVI   LISTNME1+LUNLG,C'*'                INSERT C"*"                   
         MVC   LISTNME1+LUNLG+1(LACCOUNT),ACQACT  INSERT ACCOUNT                
         OI    LISTNME1+LUNLG+1,X'40'             MAKE   UPPER CASE             
         B     DISSEL40                           CONTINUE                      
*                                                                               
DISSEL35 GOTO1 TEXTGET,APPARM,1601,(L'LISTNME,LISTNME),0                        
         B     DISSEL50            SINCE CANCELLED, DO NOT FILTER               
*                                                                               
DISSEL40 DS    0H                                                               
         CLI   ACQFLT2,C'('                       PROFILE REQUESTED ?           
         BE    DISSEL42                           YES,   JUST MOVE              
         TM    ACQFLT2,X'40'                      EXCLUDE REQUESTED ?           
         BO    DISSEL42                           NO,    JUST MOVE              
         MVI   LISTNME2,C'*'                      INSERT C"*"                   
         MVC   LISTNME2+1(LULACNT),ACQFLT2        INSERT UNIT/LEDGER            
*                                                 AND    ACCOUNT                
         OI    LISTNME2+1,X'40'                   MAKE   UPPER CASE             
         B     DISSEL50                           CONTINUE                      
*                                                                               
DISSEL42 DS    0H                                 INSERT DATA                   
         MVC   LISTNME2(L'ACQFLT2),ACQFLT2                                      
         CLI   ACQTYP2,ACQANAL                    ANALYSIS ?                    
         BNE   DISSEL43                           NO,    SKIP                   
         CLI   ACQFLT2,C'('                       LIST ?                        
         BNE   DISSEL43                           NO,    SKIP                   
         MVI   LISTNME2+13,C' '                   CLEAR  LAST  BYTE             
*                                                                               
DISSEL43 DS    0H                                                               
         CLI   ACQTYP2,ACQDATE                    DATE   FIELD ?                
         BNE   DISSEL50                           NO,    SKIP                   
*                                                 FROM   YEAR   ....            
         CLI   LISTNME2+1,C'9'                    YEAR   > 1999 ?               
         BNH   DISSEL45                           NO,    YEAR  OKAY             
         ZIC   R1,LISTNME2+1                      GET    H.O.  BYTE             
         SHI   R1,10                              SUBTRACT     X'0A'            
         STC   R1,LISTNME2+1                      SAVE   BYTE                   
*                                                                               
DISSEL45 DS    0H                                 TO     YEAR   ....            
         CLI   LISTNME2+1+L'ACQDTSTR,C'9'         YEAR   > 1999 ?               
         BNH   DISSEL50                           NO,    YEAR  OKAY             
         ZIC   R1,LISTNME2+1+L'ACQDTSTR           GET    H.O.  BYTE             
         SHI   R1,10                              SUBTRACT     X'0A'            
         STC   R1,LISTNME2+1+L'ACQDTSTR           SAVE   BYTE                   
*                                                                               
DISSEL50 DS    0H                                                               
         MVC   LISTOWN(L'ACQESTOR),ACQESTOR                                     
         B     EXIT                                                             
         DROP  R1,R2,R4                                                         
         EJECT ,                                                                
***********************************************************************         
*        ROUTINE TO PROCESS SELECTION FROM LIST                       *         
*        ENTER FROM --> REPORT LIST                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
         USING ACQD,R3                                                          
         USING LISTD,R4                                                         
PROCLST  XC    REQHDR,REQHDR       FIRST 80 BYTES                               
         MVC   REQCARD1,SPACES     2ND 80 BYTES, 1ST CARD                       
         MVC   REQCARD2,SPACES     3RD 80 BYTES, 2ND CARD                       
         MVC   REQCARD3,SPACES     4TH 80 BYTES, 3RD CARD                       
         MVC   REQCARD4,SPACES     5TH 80 BYTES, 4TH CARD                       
         MVC   REQCARD5,SPACES     6TH 80 BYTES, 5TH CARD                       
         MVC   REQCARD6,SPACES     7TH 80 BYTES, 6TH CARD                       
         MVC   REQCARD7,SPACES     8TH 80 BYTES, 7TH CARD                       
         MVC   REQCARD8,SPACES     9TH 80 BYTES, 8TH CARD                       
*                                                                               
         MVI   FAKEFLDH+5,L'FAKEFLD                                             
         MVI   FAKEFLDH,L'FAKEFLDH+L'FAKEFLD                                    
         MVI   FAKEDETH+5,L'FAKEDET                                             
         MVI   FAKEDETH,L'FAKEDETH+L'FAKEDET                                    
*                                                                               
         MVI   KEYSUB,RESKSUBQ     X'02'                                        
         CLI   APGFLAG,YES                                                      
         BNE   *+8                                                              
         MVI   KEYSUB,APGKSUBQ     X'07'                                        
         LA    R3,REQCARDS                                                      
*                                                                               
         GOTO1 GETFFLD,APPARM,(R3)                                              
         L     R8,APPARM           AVAILABLE LOCATION IN REQ CARD               
         MVI   0(R8),ACQPID        SET TO PERSONAL ID                           
         MVC   1(L'TWAPERSN,R8),TWAPERSN                                        
*                                                                               
         MVC   FVXTRA,SPACES                                                    
         MVI   PRFFLAG,0           RESET                                        
         MVI   ACQREVOP,YES        DEFAULT TO INCLUDE REVERSALS                 
         MVI   ACQDRFOP,NO         DEFAULT TO EXCLUDE DRAFT ITEMS               
         MVI   PRTSTYLE,C'L'       DEFAULT TO LANDSCAPE                         
         MVI   RUNJOB,C' '                                                      
*                                                                               
         MVC   RESKEY,APRECKEY                                                  
         OC    RESKEY+RESKFRML(2),RESKEY+RESKFRML                               
         BNZ   *+10                                                             
         MVC   RESKEY+RESKFRML(2),SPACES                                        
*                                                                               
         CLI   RESKEY+RESKFRML,C' '       IS IT A NORMAL FORMAT                 
         BE    PRCLST03                   YES                                   
         CLI   APACTN,ACTCNCL      CANCEL REQUEST?                              
         BE    PRCLST02                                                         
         CLI   APACTN,ACTRES       RESTORE REQUEST?                             
         BE    PRCLST02                                                         
         CLI   APACTN,ACTDIS       DISPLAY REQUEST?                             
         BE    PRCLST95                                                         
         B     IVALFMT             ALL OTHER ACTIONS ARE FOR FORMATS            
*                                                                               
PRCLST02 GOTO1 =A(CNRSREQ),APPARM,RR=APRELO                                     
         B     PRCLST95                                                         
***********************************************************************         
*        REQUEST PROCESS, SELECT = 'S' OR 'O' OR 'Q'                  *         
***********************************************************************         
         SPACE 1                                                                
PRCLST03 L     R4,APLSTADD         LOAD LIST LINE                               
         MVC   SAVFORM,SPACES                                                   
         MVC   SAVFORM(1),APGKFMT-APGRECD(R2)                                   
         CLI   RESKTYP,RESKTYPQ    X'02'                                        
         BNE   *+10                                                             
         MVC   SAVFORM,RESKFORM                                                 
*                                                                               
         GOTO1 AIO,IORD+IOACCFIL+IO1                                            
         BE    *+6                                                              
         DC    H'0'                VALID FORMAT NOW NOT VALID?                  
*                                                                               
         L     R1,AIOAREA1                                                      
         GOTO1 GETTYPE                                                          
         CLI   APACTN,ACTREQ       SELECT=Q, REQUEST                            
         BE    PRCLST4B                                                         
         CLI   APACTN,ACTOVER      SELECT=O, OVERNIGHT, NO RESTICTIONS          
         BE    PRCLST4B                                                         
         CLI   APACTN,ACTSOON      SELECT=S, SOON, RESTICTIONS                  
         BNE   IVALREQ             NOT VALID REQUEST                            
         CLI   SOONVLUE,SOONNOT    OK TO SOON IF > 0                            
         BE    IVALREQ                                                          
***********************************************************************         
*  KEYWORD AND PROFILE SECURITY WHEN REQUESTING                       *         
***********************************************************************         
         SPACE 1                                                                
PRCLST4B OC    TWASAGN,TWASAGN                                                  
         BZ    PRCLST4C            NO SECURITY , MUST BE DDS TERMINAL           
         SR    R3,R3               KEYWORD (1-8)                                
         ICM   R3,1,APSECKYW       SECURITY SET UP?                             
         GOTO1 AFMTSEC,APPARM,(R3),1                                            
         CLI   APPARM,0                                                         
         BNE   IVALSEC                                                          
*                                                                               
         SR    R3,R3               PROFILE (33-40)                              
         ICM   R3,1,APSECPRF       SECURITY SET UP?                             
         GOTO1 AFMTSEC,APPARM,(R3),33                                           
         CLI   APPARM,0                                                         
         BNE   IVALSEC                                                          
*                                                                               
PRCLST4C LA    R3,REQCARDS         RESTORE R3                                   
*&&UK                                                                           
         TM    GENIND,GENEURO      1ST OR 2ND CURRENCY POSSIBLE ?               
         BZ    *+10                NO                                           
         MVC   ACQCURR,RUNRCU      YES, SO MOVE INTO CARD                       
*&&                                                                             
         CLI   APACTN,ACTREQ       REQUEST?                                     
         BE    PRCLST95                                                         
*                                  ANY  ERROR   MESSAGES ?                      
         GOTO1 =A(CKMSGERR),RR=APRELO                                           
         BE    IVALEXIT            YES, FORMAT  CONTAINS ERROR MSGS             
*                                                                               
         MVI   LDGUSED,0           INITIALIZE NUMBER OF LEDGERS USED            
*                                  CLEAR LEDGER SAVE AREA                       
         MVC   LDGSAVE(LDGSAVEL),SPACES                                         
         MVC   FAKEDET,SPACES      CLEAR  DETAIL FIELD AREA                     
         MVI   MULTLDGR,YES                                                     
         CLI   APREPJCL,REPJCLX    EXPENSE                                      
         BE    PRCLST4E                                                         
         CLI   APREPJCL,REPJCLP    PAYABLES                                     
         BE    PRCLST4E                                                         
         CLI   APREPJCL,REPJCLG    GENERAL LEDGERS                              
         BE    PRCLST4E                                                         
         MVI   MULTLDGR,NO                                                      
*                                  CLEAR LEDGER SAVE AREA                       
PRCLST4E L     R1,AIOAREA1                                                      
         CLI   APGFLAG,YES                                                      
         BNE   PRCLST05                                                         
         GOTO1 VCOLY,APPARM,('OVLYBBLK',0),0,0                                  
         CLI   4(R1),X'FF'                                                      
         BE    IVALOVLY            ERROR ON LOADING OVERLAY                     
*                                                                               
PRCLST4G MVC   AAPGIO,0(R1)        SAVE  ADDRESS OF AREA                        
         BAS   RE,GETAPG                                                        
**&UK                                                                           
*        TM    GENIND,GENEURO      1ST OR 2ND CURRENCY POSSIBLE ?               
*        BZ    *+10                NO                                           
*        MVC   ACQCURR,RUNRCU      YES, SO MOVE INTO CARD                       
**&                                                                             
         GOTO1 AFVAL,RUNSTDH       DID   THEY INPUT START DATE ?                
         BNE   IVALMISS            NO,   MISSING    INPUT FIELD                 
         GOTO1 AFVAL,RUNEDDH       DID   THEY INPUT END   DATE ?                
         BNE   IVALMISS            NO,   MISSING    INPUT FIELD                 
         GOTO1 AFVAL,RUNUNLH       DID   THEY INPUT UNIT  LEDGER ?              
         BNE   PRCLST10            NO,   CHECK      ACCOUNT                     
         MVC   RQUL,FVIFLD                                                      
         CLI   MULTLDGR,YES        ARE   MULTIPLE   U/LS  VALID ?               
         BE    PRCLST4J            YES,  VALIDATE   U/L                         
         CLC   ACQUNT(2),FVIFLD    NO,   SAME UNIT/LEDGER ?                     
         BE    PRCLST8A            YES,  CHECK      ACCOUNT                     
         B     IVALUNLD            INVALID U/L                                  
*                                                                               
PRCLST4J MVC   ACQUNT(2),FVIFLD    SAVE  UNIT/LEDGER                            
         B     PRCLST8A                                                         
*                                                                               
PRCLST05 CLI   RCAP#,0             ANY  RECAP REQUESTED ?                       
         BNE   *+10                YES, SKIP                                    
         MVC   ACQUNT(2),APREPUL   USE  DEFAULT LEDGER                          
         GOTO1 AFVAL,RUNUNLH       DID  THEY INPUT ONE ON SCREEN ?              
         BNE   PRCLST08            NO,  FILL IN  UNIT LEDGER                    
         MVC   ACQUNT(2),FVIFLD                                                 
*                                                                               
         USING REPTABD,R6                                                       
         L     R6,ACTYPTAB         TYPE TABLE WITH LEDGERS                      
PRCLST06 CLI   REPCODE,EOT         END OF TABLE?                                
         BE    IVALULFA            INVALID U/L FOR ACTION                       
         CLC   APREPJCL,REPJCLID   SAME TYPE?                                   
         BNE   PRCLST07            NEXT                                         
         TM    REPFLAG,REPSJCL     APG, NO UNIT LEDGER TO MATCH                 
         BO    PRCLST07            NOT AN APG REQUEST, SO SKIP                  
         CLC   REPUL,FVIFLD        MATCH UNIT/LEDGER                            
         BE    PRCLST8A            OK SO FAR                                    
*                                                                               
PRCLST07 LA    R6,REPLNQ(,R6)      BUMP TO NEXT ENTRY                           
         B     PRCLST06                                                         
         DROP  R6                                                               
*                                                                               
PRCLST08 DS    0H                  FILL IN THE UNIT/LEDGER                      
         MVC   FAKEFLD,SPACES      CLEAR  THE  FIELD                            
         GOTO1 FILLFLD,APPARM,AIOAREA1,('RFLLDG',FAKEFLDH),FAKEDETH,0           
*                                                                               
         MVC   ACQUNT(2),SPACES    LIST OF LEDGERS SO SET TO NONE               
         CLI   APPARM,1            ONLY ONE LEDGER ?                            
         BNE   PRCLST8B            NO,  SKIP                                    
         MVC   ACQUNT(2),FAKEFLD   INSERT THE UNIT/LEDGER                       
*                                                                               
PRCLST8A LA    R1,ACQUNT                                                        
         GOTO1 VALLEDG             VALIDATE   U/L                               
         BNE   EXIT                NO,   EXIT (VALLEDG    SET ERR CODE)         
         MVC   FVXTRA(2),0(R1)                                                  
         CLC   CUAUTH+1(1),ACLEDSEC+1                                           
         BL    IVALSEC                                                          
         MVC   FVXTRA(2),SPACES                                                 
         CLI   APACTN,ACTSOON                                                   
         BNE   PRCLST10                                                         
         ICM   RF,15,ACLE#TRX      TEST TRX COUNTER SET ON LEDGER               
         BZ    PRCLST10                                                         
         MVI   RUNJOB,RUNSHORT                                                  
         C     RF,=A(MAXSHORT)     TEST MAX COUNT FOR SHORT SOON                
         BNH   *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
         B     PRCLST10                                                         
*                                                                               
PRCLST8B CLI   APPARM,0            ANY  LEDGERS ?                               
         BNE   PRCLST8C            YES, CONTINUE                                
         CLI   RCAP#,0             NO,  ANY  RECAP REQUESTED ?                  
         BE    IVALNOUL            NO,  ERROR NO LEDGERS                        
         B     PRCLST10            PROCESS ACCOUNT DATA                         
*                                                                               
PRCLST8C CLI   APPARM,2            LIST OF LEDGERS ?                            
         BE    IVALLEDG            YES, ERROR INVALID LEDGER FIELD              
*                                                                               
         CLI   APPARM,3            MULTIPLE UNIT/LEDGERS FOUND ?                
         BNE   PRCLST10            NO,  SKIP                                    
         LA    R6,1                INIT UNIT/LEDGER COUNTER                     
         LA    R1,LDGSAVE          ->   LEDGER SAVE AREA                        
         LA    R8,FAKEDET          ->   FAKE DETAIL LEDGER LIST                 
         SR    R0,R0                                                            
*                                                                               
PRCLST09 DS    0H                                                               
         MVC   0(LUNLG,R1),0(R8)   MOVE UNIT/LEDGER                             
         MVC   FVXTRA(2),0(R1)                                                  
         GOTO1 VALLEDG             VALIDATE   U/L                               
         BNE   EXIT                NO,   EXIT (VALLEDG SET ERR CODE)            
         CLC   CUAUTH+1(1),ACLEDSEC+1    CHECK LEDGER SECURITY                  
         BL    IVALSEC                                                          
         MVC   FVXTRA(2),SPACES                                                 
         CLC   2(1,R8),SCCOMMA     ANY  MORE UNIT/LEDGERS ?                     
         BNE   PRCLST9A            NO,  GOT ALL THE UNIT/LEDGERS                
         CLI   LDGUSED,MAXLDG      ANY  SPACE LEFT ?                            
         BL    *+6                 YES, SKIP                                    
         DC    H'0'                NO,  DUMP - SHOULD NOT OCCUR                 
*                                                                               
         CLI   APACTN,ACTSOON                                                   
         BNE   *+10                                                             
         ICM   RF,15,ACLE#TRX      TRX COUNTER ON LEDGER OR 0                   
         AR    R0,RF               RUNNING TOTAL                                
*                                                                               
         LA    R1,LUNLG(,R1)       BUMP TO NEXT UNIT/LEDGER                     
         LA    R8,LUNLG+1(,R8)     BUMP TO NEXT UNIT/LEDGER + COMMA             
         STC   R6,LDGUSED          SAVE THE COUNTER                             
         LA    R6,1(,R6)           BUMP UNIT/LEDGER COUNTER                     
         B     PRCLST09            LOOP TO PROCESS NEXT LEDGER                  
                                                                                
PRCLST9A CLI   APACTN,ACTSOON                                                   
         BNE   PRCLST10                                                         
         LTR   R0,R0               TOTAL TRX FOR ALL LEDGERS IN REQ             
         BZ    PRCLST10            NONE SET                                     
         MVI   RUNJOB,RUNSHORT                                                  
         C     R0,=A(MAXSHORT)     TEST MAX COUNT FOR SHORT SOON                
         BNH   *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
*                                                                               
PRCLST10 GOTO1 PRCACNT             PROCESS ACCOUNT DATA                         
         CLC   FVMSGNO,=AL2(FVFOK) ANY     ERRORS  ?                            
         BNE   XIT                 YES,    EXIT                                 
*                                                                               
         MVI   OFFFLDH,L'OFFFLDH+L'OFFFLD                                       
         MVI   OFFFLDH+5,L'OFFFLD                                               
         MVC   OFFFLD,SPACES       FAKE SCREEN HDR+FIELD                        
         GOTO1 FILLFLD,APPARM,AIOAREA1,('RFLOFF',OFFFLDH),0,1687                
         CLI   APPARM,1                                                         
         BL    PRCLST12                                                         
         LA    RF,RFLOFF                                                        
         GOTO1 VOFFICE,APPARM,('VOFTYPER',AIOAREA1),(20,BLOCK),(RF)             
         BNE   XIT                                                              
*                                                                               
PRCLST12 L     R2,AIOAREA1                                                      
         MVI   APELCODE,RPFELQ     PROFILE ELEMENT X'C4'                        
         GOTO1 GETEL,(R2)                                                       
         BNE   PRCLST20                                                         
*                                                                               
         USING RPFELD,R1                                                        
*&&US                                                                           
         TM    RPFPOPT,RPFPORT     USE PORTRAIT ?                               
         BZ    *+8                                                              
         MVI   PRTSTYLE,C'P'       YES                                          
*&&                                                                             
*&&UK                                                                           
         MVI   ACQALOCK,C'N'       INCLUDE UNLOCKED ONLY                        
         TM    RPFROPT,RPFXLOCK                                                 
         BO    *+20                                                             
         MVI   ACQALOCK,C'O'       INCLUDE LOCKED ONLY                          
         TM    RPFROPT,RPFOLOCK                                                 
         BO    *+8                                                              
         MVI   ACQALOCK,C'Y'       INCLUDE ALL ACCOUNTS                         
*&&                                                                             
*                                                                               
         TM    RPFOPT1,RPFIDRFT    INCLUDED DRAFT ITEMS ?                       
         BZ    *+8                                                              
         MVI   ACQDRFOP,YES        YES, INCLUDED THEM                           
         TM    RPFOPT1,RPFODRFT    DRAFT ITEMS ONLY?                            
         BZ    *+8                                                              
         MVI   ACQDRFOP,ONLY       YES, ONLY THEM                               
*                                                                               
         TM    RPFROPT,RPFXRVRS    EXCLUDED REVERSALS?                          
         BZ    *+8                                                              
         MVI   ACQREVOP,NO         YES, EXCLUDED THEM                           
         TM    RPFROPT,RPFORVRS    REVERSALS ONLY?                              
         BZ    *+8                                                              
         MVI   ACQREVOP,ONLY       YES, ONLY THEM                               
*                                                                               
*&&US                                                                           
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
         CLI   RPFBLTRN,YES                                                     
         BE    PRCLST14                                                         
         CLI   RPFBLTRN,C' '                                                    
         BNH   PRCLST14                                                         
         MVC   ACQOPT4,RPFBLTRN    MOVE IN UTILIZE OPTION                       
*                                                                               
PRCLST14 TM    RPFDNOPT,RPFDDOWN   FORCE     DOWNLOAD                           
         BZ    *+8                                                              
         OI    PRFFLAG,PRFDOWN     NORMAL    DOWNLOAD                           
*                                                                               
         CLI   RCAP#,0             ANY  RECAP     REQUESTED ?                   
         BNE   PRCLST50            YES, SKIP                                    
*                                                                               
         CLI   APREPJCL,REPJCLV    PRODUCTION ?                                 
         BNE   PRCLST16            NO,  SKIP                                    
         CLI   RPFESTST,C' '       ANY  ESTIMATE  STATUS ?                      
         BNH   PRCLST16            NO,  SKIP                                    
         MVC   ACQOPT7,RPFESTST    MOVE IN   ESTIMATE  STATUS                   
*                                                                               
PRCLST16 CLI   RPFFLT1,C' '        GET FILTERS 1-5                              
         BNH   *+10                                                             
         MVC   ACQACTF1,RPFFLT1                                                 
         CLI   RPFFLT2,C' '                                                     
         BNH   *+10                                                             
         MVC   ACQACTF2,RPFFLT2                                                 
         CLI   RPFFLT3,C' '                                                     
         BNH   *+10                                                             
         MVC   ACQACTF3,RPFFLT3                                                 
         CLI   RPFFLT4,C' '                                                     
         BNH   *+10                                                             
         MVC   ACQACTF4,RPFFLT4                                                 
         CLI   RPFFLT5,C' '                                                     
         BNH   *+10                                                             
         MVC   ACQACTF5,RPFFLT5                                                 
         DROP  R1                                                               
*                                                                               
         USING FL2LISTD,R2         MAP   FILTER    LIST 2                       
*                                                                               
PRCLST20 DS    0H                  PROCESS    SPECIAL X'C5' ELEMENTS            
         LA    R2,FLTLIST2         ->    FILTER    LIST                         
*                                                                               
PRCLST21 DS    0H                  CHECK NEXT ELEMENT                           
         CLI   FL2RFLT#,EOT        END   OF   LIST ?                            
         BE    PRCLST30            YES,  PROCESS   FLTLIST                      
         MVC   FAKEFLD,SPACES      CLEAR THE  FIELD                             
*                                  FIND  THE  DATA                              
         GOTO1 FILLFLD,APPARM,AIOAREA1,(FL2RFLT#,FAKEFLDH),0,0                  
         CLI   APPARM,0            ANY   DATA FOUND ?                           
         BE    PRCLST29            YES,  GET  NEXT TBL  ENTRY                   
         CLI   APPARM,3            ANY   STRING    FOUND ?                      
         BE    PRCLST29            YES,  GET  NEXT TBL  ENTRY                   
*                                                                               
         CLI   APPARM,1            ANY   ITEM FOUND ?                           
         BE    PRCLST22            YES,  PROCESS   ITEM                         
*                                  LIST  FOUND                                  
         ZIC   RE,FL2ACQLN         GET   ENTRY     LENGTH                       
         BCTR  RE,0                MINUS ONE  FOR  EXECUTE                      
         LH    RF,FL2ACQDS         GET   DISPLACEMENT   IN   ACQD               
         AR    RF,R3               GET   ADDRESS   IN   ACQD                    
         EXMVC RE,0(RF),FAKEFLD    MOVE  THE  ELEMENT                           
         B     PRCLST29            GET   NEXT TBL  ENTRY                        
*                                                                               
PRCLST22 DS    0H                  FOUND AN   ITEM                              
         CLI   FL2RFLT#,RFLWC      WORK  CODE ?                                 
         BE    PRCLST25            YES,  PROCESS   WORK CODE                    
         CLI   FL2RFLT#,RFLTTYPE   TRANSACTION     TYPE ?                       
         BNE   PRCLST26            NO,   DO   STANDARD  PROCESSING              
*                                  YES,  TRANSACTION    TYPE                    
*                                  CLEAR DUMMY     FIELD                        
         XC    DUMTRNTH(L'DUMTRNTH+L'DUMTRNT),DUMTRNTH                          
*                                  DUMMY AREA SIZE                              
         MVI   DUMTRNTH,L'DUMTRNTH+L'DUMTRNTH                                   
         LA    RE,FAKEFLD          ->    TRANSACTION    TYPE                    
         ZIC   RF,FAKEFLDH+7       GET   LENGTH    OF   TRAN TYPE               
         CLI   FAKEFLD,C'*'        EXCLUDE    REQUESTED ?                       
         BNE   PRCLST23            NO,   SKIP                                   
         LA    RE,FAKEFLD+1        SKIP  THE  '*'                               
         BCTR  RF,0                                                             
*                                                                               
PRCLST23 DS    0H                                                               
         STC   RF,DUMTRNTH+5       SAVE  THE  FIELD     LENGTH                  
         BCTR  RF,0                GET   EX   LENGTH                            
         EXMVC RF,DUMTRNT,0(RE)    MOVE  THE  TRANSACTION    TYPE               
*                                  SET   UP   SCANNER   BLOCK                   
         GOTO1 VSCANNER,APPARM,DUMTRNTH,(2,BLOCK),SCNP3NEQ                      
         CLI   APPARM+4,1          ONLY  ONE  PARM ?                            
         BE    *+6                 YES,  CONTINUE                               
         DC    H'0'                NO,   ABEND                                  
*                                                                               
*                                  CONVERT    TRANSACTION    TYPE               
*                                        TO   A    NUMBER                       
         GOTO1 CNVTTYPE,APPARM,(C'N',BLOCK),(1,APBYTE)                          
         BE    *+6                 OKAY, CONTINUE                               
         DC    H'0'                NO,   ABEND                                  
*                                                                               
         LH    RF,FL2ACQDS         GET   DISPLACEMENT   IN   ACQD               
         AR    RF,R3               GET   ADDRESS   IN   ACQD                    
         CLI   APBYTE,TY06MN       IS    IT   MANUAL    BILLING ?               
         BNE   PRCLST24            NO,   SKIP                                   
         MVC   0(3,RF),=CL3'M  '   INSERT     SPECIAL   CODE                    
         B     PRCLS24A            COMPLETE   THE  TRANSACTION    TYPE          
*                                                                               
PRCLST24 DS    0H                                                               
         ZIC   RE,APBYTE           GET   THE  TRANSACTION    TYPE               
         CVD   RE,DUB              CONVERT    TO   DECIMAL                      
         UNPK  0(3,RF),DUB+6(2)    UNPACK     IT                                
         OI    2(RF),C'0'          FIX   THE  SIGN                              
*                                                                               
PRCLS24A DS    0H                  PROCESS    EXCLUDE                           
         CLI   FAKEFLD,C'*'        EXCLUDE    TRAN TYPE REQUESTED ?             
         BNE   PRCLST29            FINISHED   THIS FIELD                        
         NI    0(RF),TURNOFF-X'40' MAKE  LOWER     CASE                         
         B     PRCLST29            FINISHED   THIS FIELD                        
*                                                                               
PRCLST25 DS    0H                  PROCESS    WORK CODE                         
         LA    RE,1                EXECUTE    LENGTH                            
         LH    RF,FL2ACQDS         GET   DISPLACEMENT   IN   ACQD               
         AR    RF,R3               GET   ADDRESS   IN   ACQD                    
         CLI   FAKEFLDH+7,2        EXCLUDE    REQUESTED ?                       
         BE    PRCLST27            NO,   MOVE THE  ELEMENT                      
         B     PRCLST28            YES,  MOVE THE  ELEMENT   AND                
*                                        TURN ON   EXCLUDE                      
*                                                                               
PRCLST26 DS    0H                  STANDARD   RFL  PROCESSING                   
         ZIC   RE,FL2ACQLN         GET   ENTRY     LENGTH                       
         BCTR  RE,0                MINUS ONE  FOR  EXECUTE                      
         LH    RF,FL2ACQDS         GET   DISPLACEMENT   IN   ACQD               
         AR    RF,R3               GET   ADDRESS   IN   ACQD                    
         CLI   FAKEFLD,C'*'        FIRST CHAR '*' ?                             
         BE    PRCLST28            YES,  PROCESS   EXCLUDE                      
*                                                                               
PRCLST27 DS    0H                                                               
         EXMVC RE,0(RF),FAKEFLD    MOVE  THE  ELEMENT                           
         B     PRCLST29            GET   NEXT TBL  ENTRY                        
*                                                                               
PRCLST28 DS    0H                  STANDARD   EXCLUDE   PROCESSING              
         EXMVC RE,0(RF),FAKEFLD+1  MOVE  THE  ELEMENT                           
         NI    0(RF),TURNOFF-X'40' MAKE  LOWER     CASE                         
*                                                                               
PRCLST29 DS    0H                  GET   NEXT TBL  ENTRY                        
         LA    R2,FL2LNQ(,R2)      ->    NEXT ENTRY                             
         B     PRCLST21            CHECK NEXT ENTRY                             
         DROP  R2                                                               
*                                                                               
PRCLST30 LA    R6,FLTLIST                                                       
*                                                                               
PRCLST32 CLI   0(R6),X'FF'         END OF TABLE?                                
         BE    PRCLST50            YES                                          
         LA    RE,FAKEFLDH                                                      
         ST    RE,APPARM+4                                                      
         SR    RF,RF                                                            
         IC    RF,0(,R6)                                                        
         STC   RF,APPARM+4                                                      
         SR    RF,RF                                                            
         ICM   RF,3,1(R6)                                                       
         ST    RF,APPARM+12                                                     
         MVC   FAKEFLD,SPACES      CLEAR  THE  FIELD                            
         GOTO1 FILLFLD,APPARM,AIOAREA1,,0                                       
         MVC   APBYTE,APPARM       SAVE OFF RETURN CODE                         
         CLI   APPARM,1                                                         
         BL    PRCLST40            NONE FOUND SO NEXT                           
*                                                                               
         GOTO1 GETFFLD,APPARM,REQCARDS                                          
         ICM   RE,15,APPARM                                                     
         BZ    PRCLST50            NO PLACE IN REQUEST CARD TO PUT              
         MVI   0(RE),ACQANAL       ACCOUNT FILTER                               
         LA    RF,LULACNT-1        MAX  FIELD DATA MINUS ONE                    
         CLI   0(R6),RFLCNTR       IS IT CONTRA ACCOUNT ?                       
         BE    *+8                 YES, SET CONTR FILTER                        
         CLI   0(R6),RFLBSR        IS IT BILLING SOURCE LIST ?                  
         BNE   *+8                 NO,  SKIP                                    
         MVI   0(RE),ACQCNTR       CONTRA FILTER                                
         LA    RE,1(,RE)           BUMP TO DATA AREA                            
         CLI   APBYTE,2            IS IT A +/- LIST OR LIST OF ACCTS?           
         BH    PRCLST36            LIST OF ACCOUNTS                             
         LA    RF,LLIST                                                         
         BE    PRCLST36            +/- LIST                                     
*                                                                               
         CLC   3(2,R6),SPACES      ANY UNIT LEDGER IN ACCOUNT SUPPLIED?         
         BL    PRCLST33            YES                                          
         LA    RF,LACCOUNT-1       EXMVC LENGTH (12 - 1)                        
         MVC   0(2,RE),3(R6)       MOVE IN UNIT/LEDGER                          
         LA    RE,2(,RE)                                                        
*                                                                               
PRCLST33 DS    0H                                                               
         CLI   FAKEFLD,C'*'        EXCLUDE REQUESTED ?                          
         BNE   PRCLST36            NO,  SKIP                                    
         EXMVC RF,0(RE),FAKEFLD+1  INSERT THE FIELD                             
         L     RE,APPARM           FIND START OF LOCATION IN CARD               
         NI    1(RE),TURNOFF-X'40' MAKE LOWER CASE                              
         B     PRCLST40            CONTINUE THE LOOP                            
*                                                                               
PRCLST36 EXMVC RF,0(RE),FAKEFLD    INSERT THE FIELD                             
*                                                                               
PRCLST40 LA    R6,5(,R6)           BUMP UP TO NEXT ENTRY                        
         B     PRCLST32            LOOP                                         
*                                                                               
PRCLST50 MVI   INWHEN,MIXIOKO      OVER NIGHT                                   
         CLI   APACTN,ACTSOON                                                   
         BNE   *+8                                                              
         MVI   INWHEN,MIXIOKS      SOON                                         
         MVC   ACQESTOR,SVNAME                                                  
         MVC   ACQCPY,CUABIN       REQUEST CARD COMPANY                         
         LA    R2,APRECKEY                                                      
*                                                                               
         USING APGRECD,R2                                                       
         CLI   APGFLAG,YES                                                      
         BNE   PRCLST51                                                         
         MVC   ACQSRTAR(2),=C'P='                                               
         MVC   ACQSRTAR+2(2),APREPCDE                                           
         MVC   ACQSRTAR+4(1),APGKFMT                                            
         MVC   ACQOPT2,APGKFMT                                                  
         B     PRCLST52                                                         
*                                                                               
         USING RESRECD,R2                                                       
PRCLST51 MVC   ACQAPPL(L'RESKFORM),RESKFORM                                     
         DROP  R2                                                               
*                                                                               
PRCLST52 CLI   RUNJOB,RUNSHORT                                                  
         BNE   PRCLST53                                                         
         CLC   STRDTE,SPACES       START DATE?                                  
         BNH   *+10                                                             
         MVC   ACQSTART,STRDTE                                                  
         CLC   ENDDTE,SPACES       END DATE?                                    
         BNH   *+10                                                             
         MVC   ACQEND,ENDDTE                                                    
         B     PRCLST54                                                         
*                                                                               
PRCLST53 CLC   STRDTE,SPACES       START DATE?                                  
         BH    *+12                                                             
         MVI   RUNJOB,RUNLONG                                                   
         B     *+10                                                             
         MVC   ACQSTART,STRDTE                                                  
         CLC   ENDDTE,SPACES       END DATE?                                    
         BH    *+12                                                             
         MVI   RUNJOB,RUNLONG                                                   
         B     *+10                                                             
         MVC   ACQEND,ENDDTE                                                    
*                                                                               
         CLI   RUNJOB,RUNLONG                                                   
         BNE   PRCLST54                                                         
         CLC   MOASTR,SPACES       MOA START DATE?                              
         BNH   PRCLST54                                                         
         CLC   MOAEND,SPACES       MOA END DATE?                                
         BNH   PRCLST54                                                         
         MVC   RUNJOB,SPACES                                                    
PRCLST54 MVC   ACQMOSST,MOASTR                                                  
         MVC   ACQMOSND,MOAEND                                                  
*                                                                               
         MVC   REQOUT,INOTYP       BUILD REQUEST HEADER                         
         MVC   INREPORT+2(1),APREPJCL                                           
         MVC   INREPORT+4(1),APREPJCL                                           
*&&US                                                                           
         GOTO1 AFVAL,RUNOPT2H                                                   
         BNE   *+10                                                             
         MVC   PRTSTYLE,RUNOPT2                                                 
         MVC   INREPORT+3(1),PRTSTYLE                                           
         MVC   INREPORT+5(1),PRTSTYLE                                           
*&&                                                                             
*&&UK*&& MVI   INREPORT+3,C'L'                                                  
*&&UK*&& MVI   INREPORT+5,C'L'                                                  
         CLI   APGFLAG,YES                                                      
         BNE   *+14                                                             
         MVI   PRTSTYLE,C'L'                                                    
         MVC   INREPORT+2(2),APREPCDE                                           
         MVC   REQNUMB,APREPLD#    $REQ ID # FOR LANDSCAPE                      
*&&US                                                                           
         CLI   PRTSTYLE,C'L'                                                    
         BE    *+10                                                             
         MVC   REQNUMB,APREPPT#    $REQ ID # FOR PORTRAIT                       
*&&                                                                             
         MVC   ACQPROG,INJCLID                                                  
         CLI   APGFLAG,YES                                                      
         BNE   *+10                                                             
         MVC   ACQPROG,APREPCDE                                                 
         MVC   REQDEST,INDEST                                                   
         MVC   REQORIG,CUUSER                                                   
         MVC   ACQLANG,CULANG                                                   
*&&UK                                                                           
         MVC   RQHFLG1,REQBYTE                                                  
         MVC   RQHSECD,REQSEC                                                   
*&&                                                                             
         MVC   ACQOPT8,RQFRPT      FORMAT REPORT                                
*                                                                               
**********************************************************************          
*  COUNT # OF REQUEST CARDS USED                                     *          
**********************************************************************          
*                                                                               
         LA    RE,ACQCARD4                                                      
         LA    RF,4                COUNT CARD                                   
*                                                                               
PRCLST55 CLC   0(L'ACQCARD1,RE),SPACES                                          
         BNE   PRCLST56                                                         
         SH    RE,=Y(L'ACQCARD1)                                                
         BCT   RF,PRCLST55                                                      
         B     PRCLST99            EXIT NO REQUEST DATA                         
*                                                                               
PRCLST56 MVI   REQFLAG,X'01'       SET AS LINKED REQUEST                        
         SH    RF,=H'01'                                                        
         BZ    PRCLST58                                                         
         SLL   RF,4                # OF REQUEST CARDS-1 IN HO NIBBLE            
         LA    RF,3(,RF)           TURN ON X'01' AND X'02'                      
         STC   RF,REQFLAG                                                       
         SRL   RF,4                SHIFT IT BACK                                
         MVI   ACQCONT1,C'C'       CONTINUATION MARKER                          
*                                                                               
PRCLST57 SH    RF,=H'01'           MARK CARDS 2-7 ONLY                          
         BZ    PRCLST58            IF ZERO NO MORE                              
         SH    RE,=Y(L'ACQCARD1)                                                
         MVI   ACQCONT2-ACQCARD2(RE),C'C'                                       
         B     PRCLST57                                                         
*                                                                               
PRCLST58 GOTO1 AFVAL,RUNOUTH                                                    
         TM    PRFFLAG,PRFDOWN     FORCE DOWNLOAD?                              
         BO    PRCLST59            YES,  SKIP                                   
         CLC   AC@DOWN,FVIFLD                                                   
         BNE   PRCLST60                                                         
*                                                                               
PRCLST59 CLI   APGFLAG,YES                                                      
         BNE   *+8                                                              
         MVI   ACQOPT7,YES                                                      
*                                                                               
         MVI   ACQOPT1,YES                                                      
         CLI   ACQOPT8,YES         FORMAT  REPORT ALSO ?                        
*                                  YES,    DOWN-LOAD AND FORMAT REPORT          
         BE    IVALFRPT                    CONFLICT                             
         TM    INOPT1,INOSEAR      SPECIAL DOWNLOAD?                            
         BZ    *+8                                                              
         MVI   ACQOPT1,C'T'                                                     
         TM    INOPT1,INOYMD       SPECIAL DOWNLOAD YMD?                        
         BZ    *+8                                                              
         MVI   ACQOPT1,C'D'                                                     
         TM    INOPT1,INOAIU       SPECIAL AIU DOWNLOAD?                        
         BZ    *+8                                                              
         MVI   ACQOPT1,C'A'                                                     
         MVC   REQOUT,AC@DOWN                                                   
*                                                                               
PRCLST60 TM    INWHEN,MIXIOKO      OVER NIGHT?                                  
         BZ    PRCLST61                                                         
         MVC   ADDR,=XL4'AAAAAAAA'                                              
         GOTO1 VDMGR,APPARM,(X'20',DMADD),REQUEST,ADDR,REQREC,,L'REQREC         
         CLI   APPARM+8,0                                                       
         BNE   IVALREQ                                                          
         B     PRCLST90                                                         
*                                                                               
PRCLST61 LA    R2,APELEM           BUILD SPOOK                                  
*                                                                               
         USING SPOOK,R2                                                         
         XC    SPOOK(SPOOKXL),SPOOK                                             
         MVC   SPOOKUID,CUUSER                                                  
         MVC   SPOOKDES,INDEST                                                  
         MVC   SPOOKTID,CUTRMN                                                  
         CLI   RUNJOB,RUNLONG      POSSIBLE LONG RUNNING JOB                    
         BNE   PRCLST70            NO                                           
*&&US                                                                           
         L     RE,=A(AGYTAB)       YES BUT IF AGENCY IS LISTED IN THIS          
         A     RE,APRELO           TABLE THEN DON'T PUT AS A LONG               
PRCLST65 CLI   0(RE),X'FF'         RUNNING SOON                                 
         BE    PRCLST68                                                         
         CLC   0(2,RE),CUAALF                                                   
         BE    PRCLST70                                                         
         AHI   RE,2                                                             
         B     PRCLST65                                                         
*&&                                                                             
PRCLST68 MVC   SPOOKSML,RUNJOB     YES                                          
PRCLST70 MVC   SPOOKAGY,CUAALF                                                  
         MVC   SPOOKAGX,CUABIN                                                  
         MVC   SPOOKDID,INUSER                                                  
         MVC   SPOOKSYS,=C'AC'                                                  
         MVC   SPOOKEOD,INPRGID                                                 
         MVC   SPOOKJCL,INJCLID                                                 
         MVC   SPOOKPR1,INPRTY1                                                 
         MVC   SPOOKPR2,INPRTY2                                                 
         MVC   SPOOKWEN,INWHEN                                                  
         CLI   ACQOPT1,C' '        DOWNLOADABLE?                                
         BNH   *+8                                                              
         OI    SPOOKTY,X'10'       (REMOTDLQ)                                   
         MVC   SPOOKXT(3),=C'XT='                                               
*                                                                               
         GOTO1 VREQTWA,APPARM,(5,(R5)),REQHEAD,VDMGR,ACOM,(R2)                  
         MVC   FVMSGNO,=AL2(FVFTFUL)                                            
         CLI   8(R1),X'FE'         TEST TERMINAL QUEUE FULL?                    
         BE    PRCLST99                                                         
         MVC   FVMSGNO,=AL2(FVFQFUL)                                            
         BH    PRCLST99                                                         
         MVC   FVMSGNO,=AL2(FVFEJCL)                                            
         L     RE,8(,R1)                                                        
         OC    0(7,RE),0(RE)                                                    
         BZ    PRCLST99                                                         
         MVC   FVMSGNO,=AL2(ACIACTN)     FORCE A MSG (ACTION FINISHED)          
         MVI   FVOMTYP,GTMINF                                                   
         SR    RF,RF                                                            
         ICM   RF,3,6(RE)                    GET SOON NUMBER                    
         CVD   RF,APDUB                                                         
*                                                                               
         USING GETTXTD,R6                                                       
PRCLST90 SR    RF,RF                                                            
         TM    INWHEN,MIXIOKO      OVERNIGHT?                                   
         BZ    *+8                                                              
         ICM   RF,15,ADDR                                                       
         GOTO1 DTESTAMP,APPARM,AIOAREA1,(RF)                                    
*&&UK*&& TM    GENIND,GENREADO                                                  
*&&UK*&& BO    PRCLST92                                                         
         GOTO1 AIO,IOSOXWRT+IOACCFIL+IO1                                        
                                                                                
PRCLST92 LA    R6,APPARM                                                        
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   ERRORMSG,SPACES                                                  
         OI    APINDS,APILRERD        RE-READ RECORD, IO WILL OCCURE            
         OI    LISTDATH+1,FVAHIGH     HIGHLIGHT                                 
         OI    LISTDATH+6,FVOXMT      TRANSMIT                                  
         MVC   LISTNME,SPACES                                                   
         MVI   GTMAXL,L'LISTNME                                                 
         MVI   GTMSYS,X'FF'                                                     
         LA    R1,ERRORMSG                                                      
         STCM  R1,7,GTAOUT                                                      
         MVI   GTMTYP,GTMINF                                                    
         OI    GT1INDS,GT1NOREF+GT1OWRK                                         
         MVC   GTMSGNO,=AL2(INFREP1)      MSG FOR OVER NIGHT PROCESSING         
         TM    INWHEN,MIXIOKO             OVERNIGHT?                            
         BNZ   PRCLST94                   YES                                   
*                                                                               
         MVC   LISTOWN,SPACES             RE-CLEAR NAME                         
         MVC   GTMSGNO,=AL2(INFREP2)      SOON                                  
         MVI   GTMAXL,L'LISTNME+2+L'LISTOWN                                     
         MVC   XTRA_MSG,SPACES                                                  
         MVC   XTRA_MSG(L'INUSER),INUSER                                        
         MVC   XTRA_MSG+3(1),SCCOMMA                                            
         MVC   XTRA_MSG+4(6),=XL6'402020202020'                                 
         LA    R1,XTRA_MSG+4                                                    
         EDMK  XTRA_MSG+4(7),APDUB+5                                            
         MVC   XTRA_MSG+4(6),0(R1)           SQUISH                             
         LA    R0,L'XTRA_MSG                 FIGURE OUT LENGTH                  
         LA    R1,XTRA_MSG+L'XTRA_MSG-1                                         
         CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         DC    H'0'                BAD SOON REQUEST ID                          
         STC   R0,GTLTXT                                                        
         LA    R1,XTRA_MSG                                                      
         STCM  R1,7,GTATXT                                                      
         MVI   LISTACT,C'*'                                                     
         MVC   LISTACT+1(L'LISTACT-1),SPACES                                    
*                                                                               
PRCLST94 OI    LISTACTH+1,FVAHIGH                                               
         OI    LISTACTH+6,FVOXMT                                                
         NI    TWALSCTL,TURNOFF-TWALSHLD                                        
         MVI   GTMSGNO,0           FROM GENERAL INFO                            
         GOTO1 VGETTXT,GETTXTD                                                  
         SR    R1,R1                                                            
         ICM   R1,1,GTMAXL                                                      
         BZ    PRCLST97                                                         
         BCTR  R1,0                                                             
         EXMVC R1,LISTNME,ERRORMSG                                              
         MVC   FVMSGNO,=AL2(ACIENREQ)                                           
         TM    INWHEN,MIXIOKS      SOONING?                                     
         BO    EXIT                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     PRCLST97                                                         
*                                                                               
PRCLST95 NI    TWALSCTL,TURNOFF-TWALSHLD                                        
*                                                                               
PRCLST97 MVI   APMODE,APMFMOK                                                   
*                                                                               
PRCLST99 B     EXIT99                                                           
         DROP  R2,R3,R4,R6                                                      
         EJECT ,                                                                
***********************************************************************         
*  PROCESS ACCOUNT DATA                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACQD,R3                                                          
PRCACNT  NTR1  ,                                                                
         MVC   ACQACT,SPACES       CLEAR  ACQACT                                
         MVC   SVRCAP#,RCAP#       SAVE   RECAP   NUMBER                        
         GOTO1 AFVAL,RUNACCH       DID    THEY INPUT ONE ON SCREEN ?            
         BNE   PRCACN52            NO,    SEE  IF ONE IS STORED                 
         CLI   FVIFLD,C'('         MULTIPLE    REQUESTED ?                      
         BE    PRCACN50            YES,   SEE  IF ONE IS STORED                 
         CLI   FVIFLD,C'*'         EXCLUDE     REQUESTED ?                      
         BE    PRCACN05            YES,   SKIP                                  
         CLI   FVIFLD,C'+'         IS     IT   +LIST                            
         BE    PRCACN10            YES,   PROCESS THE LIST                      
         CLI   FVIFLD,C'-'         IS     IT   -LIST                            
         BE    PRCACN10            YES,   PROCESS THE LIST                      
*                                  NOT    EXCLUDE ITEM ENTERED                  
         CLI   FVILEN,LACCOUNT     IS     THE  LENGTH > 12 ?                    
         BH    PRCACNER            YES,   INVALID ACCOUNT                       
         MVC   ACQACT,FVIFLD       SAVE   ACCOUNT                               
         B     PRCACN30            GOTO   COMMON CODE                           
*                                                                               
PRCACN05 DS    0H                  EXCLUDE       WAS ENTERED                    
         CLI   FVILEN,LACCOUNT+1   IS     THE  LENGTH > 13 ?                    
         BH    PRCACNER            YES,   INVALID ACCOUNT                       
*                                                                               
PRCACN08 DS    0H                                                               
         CLI   APGFLAG,YES         APG    FORMAT    ?                           
         BE    PRCACNER            YES,   INVALID   ACCOUNT                     
         MVC   ACQACT,FVIFLD+1     SAVE   ACCOUNT                               
         B     PRCACN30            GOTO   COMMON CODE                           
*                                                                               
PRCACN10 DS    0H                  LIST   WAS  ENTERED                          
         CLI   APGFLAG,YES         APG    FORMAT    ?                           
         BE    PRCACNER            YES,   INVALID   ACCOUNT                     
         MVI   RUNJOB,RUNLONG                                                   
         CLC   ACQUNT(2),SPACES    ANY    UNIT/LEDGER DATA ?                    
         BH    PRCACN15            YES,   SKIP                                  
         CLI   LDGUSED,0           ANY    UNIT/LEDGER LIST ?                    
         BNE   IVALMIS1            NO,    SHOULD NOT  HAPPEN                    
*                                                                               
PRCACN15 DS    0H                                                               
         CLI   LDGUSED,0           ANY    UNIT/LEDGER LIST ?                    
         BNE   PRCACN20            YES,   SKIP                                  
*                                  NO,    USE  THE  LEDGER                      
         MVC   LDGSAVE(LUNLG),ACQUNT                                            
*                                                                               
PRCACN20 DS    0H                  VALIDATE    THE LIST                         
         GOTO1 VALLIST,APPARM,(LDGUSED,LDGSAVE),(FVILEN,FVIFLD),1,TX#           
*                                                                               
         LTR   RF,RF               VALID  LIST ?                                
         BNZ   IVALLIST            NO,    ERROR                                 
         MVC   ACQACT,FVIFLD       INSERT LIST                                  
         B     PRCACN45            TEST TRX COUNT                               
*                                                                               
         USING ACTRECD,R6                                                       
PRCACN30 DS    0H                  ITEM   WAS  ENTERED                          
         CLC   ACQUNT(2),SPACES    ANY    UNIT/LEDGER DATA ?                    
         BE    IVALMIS1            NO,    EITHER U/L  TOTALLY MISSING           
*                                         OR   USER DID  NOT ENTER U/L          
         CLI   APGFLAG,YES         APG    FORMAT    ?                           
         BE    PRCACN35            YES,   SKIP WILDCARD  TEST                   
*                                                                               
*        CLC   ACQACT,SPACES       NO ACCOUNT?                                  
*        BH    *+12                                                             
*        MVI   RUNJOB,RUNLONG                                                   
*        B     PRCACN35                                                         
*                                  CHECK  FOR  WILDCARD CHARACTER               
         GOTO1 WILDCARD,APPARM,ACQUNT,(1,L'ACQACT+2)                            
         LTR   RF,RF               ANY    WILDCARD CHARACTERS ?                 
         BM    PRCACNER            INVALID,    EXIT                             
         BZ    PRCACN35            NO, VALIDATE ACCOUNT                         
         CLI   ACQACT,C'?'         COULD IT BE A LONG RUNNING JOB ?             
         BNE   PRCACN47            NO,  SKIP VALIDATION                         
         MVI   RUNJOB,RUNLONG                                                   
         B     PRCACN47            YES, SKIP VALIDATION                         
*                                                                               
PRCACN35 DS    0H                                                               
         LA    R6,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY     CODE                             
         MVC   ACTKUNT(2),ACQUNT   UNIT/LEDGER                                  
         MVC   ACTKACT,ACQACT      ACCOUNT                                      
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   PRCACNER            ERROR, INVALID ACCOUNT                       
         DROP  R6                                                               
*                                                                               
         DS    0H                                                               
         USING ACTRECD,RF                                                       
         SR    R0,R0                                                            
         L     RF,AIOAREA2                                                      
         AH    RF,DATADISP                                                      
         USING ABLELD,RF                                                        
PRCACN40 CLI   ABLEL,0                                                          
         BE    PRCACN47                                                         
         CLI   ABLEL,NUMELQ                                                     
         BE    PRCACN43                                                         
         CLI   ABLEL,ABLELQ                                                     
         BE    *+14                                                             
PRCACN42 IC    R0,ABLLN                                                         
         AR    RF,R0                                                            
         B     PRCACN40                                                         
                                                                                
         CLI   ABLLN,ABLLN3Q                                                    
         BL    PRCACN47            NO TRX COUNT - USE OLD-STYLE TESTS           
         ICM   RE,15,ABLTXS                                                     
         B     PRCACN44                                                         
*                                                                               
         USING NUMELD,RF                                                        
PRCACN43 CLI   NUMLN,NUMLN2Q                                                    
         BL    PRCACN42                                                         
         CLI   NUMTYPE,NUMTYHIQ    TEST TRX COUNT EL, HIGH LVL A/C              
         BNE   PRCACN42                                                         
         ICM   RE,15,NUM#TRX                                                    
         DROP  RF                                                               
PRCACN44 STCM  RE,15,ACAC#TRX                                                   
                                                                                
PRCACN45 ICM   RE,15,ACAC#TRX                                                   
         CLI   FVIFLD,C'*'         TEST SINGLE -VE ACCOUNT                      
         BE    *+8                                                              
         CLI   FVIFLD,C'-'         OR -VE A/C LIST                              
         BE    *+10                                                             
         LR    RF,RE               NO, SIMPLE TOTAL                             
         B     PRCACN46                                                         
         ICM   RF,15,ACLE#TRX      TRX COUNTER ON LEDGER                        
         SR    RF,RE               MINUS A/C OR A/C LIST TOTAL                  
         BM    PRCACN47            SOMETHING WRONG - SKIP                       
PRCACN46 MVI   RUNJOB,RUNSHORT                                                  
         C     RF,=A(MAXSHORT)     TEST MAX TRX COUNT FOR SHORT SOON            
         BNH   *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
         B     PRCACN49                                                         
*                                                                               
PRCACN47 DS    0H                                                               
         CLI   APREPJCL,REPJCLV    PRODUCTION?                                  
         BNE   *+14                                                             
         CLC   APREPUL,=C'SJ'      U/L = SJ?                                    
         BE    PRCACN48                                                         
         GOTO1 GETLEDG,APREPUL     GET LEVEL STRUCTURE                          
         CLI   ACLEV1,12           ONLY ONE LEVEL?                              
         BE    PRCACN48                                                         
         LA    R1,ACQACT                                                        
         ZIC   R0,ACLEV2                                                        
         AR    R1,R0               POINT TO THIRD LEVEL                         
         CLI   0(R1),X'40'                                                      
         BH    PRCACN48                                                         
         CLI   ACLEV3,0            ONLY 2 LEVELS?                               
         BNE   PRCACN49                                                         
         SR    R1,R0                                                            
         IC    R0,ACLEV1                                                        
         AR    R1,R0               POINT TO SECOND LEVEL                        
         CLI   0(R1),X'40'                                                      
         BNH   PRCACN49                                                         
*                                                                               
PRCACN48 MVI   RUNJOB,RUNSHORT                                                  
*                                                                               
PRCACN49 CLI   FVIFLD,C'*'         EXCLUDE     REQUESTED ?                      
         BNE   PRCACNEX            YES,   EXIT                                  
         NI    ACQACT,TURNOFF-X'40'       MAKE LOWER CASE                       
         B     PRCACNEX            EXIT                                         
*                                                                               
PRCACN50 DS    0H                  FOUND  "("                                   
         CLI   APGFLAG,YES         APG    FORMAT   ?                            
         BE    PRCACNER            YES,   INVALID  ACCOUNT                      
         MVC   RCAP#,=F'0'         TEMPORARILY SAY NO RECAP REQUESTED           
*                                                                               
PRCACN52 DS    0H                  FOUND  NULL                                  
         CLI   APGFLAG,YES         APG    FORMAT   ?                            
         BE    PRCACNEX            YES,   OKAY AS  IS                           
         MVC   FAKEFLD,SPACES      CLEAR  THE  FIELD                            
         GOTO1 FILLFLD,APPARM,AIOAREA1,('RFLACC',FAKEFLDH),0,1680               
         CLI   APPARM,1            TEST SINGLE ACCOUNT                          
         BNE   PRCACN62            NO                                           
****                                                                            
**** SINGLE ACCOUNT, READ IT HERE FOR TRX COUNT ****                            
**** A/C WAS INPUT FROM REPORT LIST SCREEN ***                                  
****                                                                            
         USING ACTRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY     CODE                             
         LA    RF,FAKEFLD                                                       
         CLI   FAKEFLD,C'*'                                                     
         BNE   *+8                                                              
         LA    RF,FAKEFLD+1                                                     
         MVC   ACTKULA,0(RF)       U/L/ACCOUNT                                  
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   PRCACNER            ERROR, INVALID ACCOUNT                       
         DROP  R6                                                               
*                                                                               
         DS    0H                                                               
         USING ACTRECD,RF                                                       
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         L     RF,AIOAREA2                                                      
         AH    RF,DATADISP                                                      
         USING ABLELD,RF                                                        
PRCACN54 CLI   ABLEL,0                                                          
         BE    PRCACN60                                                         
         CLI   ABLEL,NUMELQ                                                     
         BE    PRCACN58                                                         
         CLI   ABLEL,ABLELQ                                                     
         BE    *+14                                                             
PRCACN56 IC    R0,ABLLN                                                         
         AR    RF,R0                                                            
         B     PRCACN54                                                         
                                                                                
         CLI   ABLLN,ABLLN3Q                                                    
         BL    PRCACN60            NO TRX COUNT, TREAT AS ZERO                  
         ICM   RE,15,ABLTXS                                                     
         B     PRCACN60                                                         
*                                                                               
         USING NUMELD,RF                                                        
PRCACN58 CLI   NUMLN,NUMLN2Q                                                    
         BL    PRCACN56                                                         
         CLI   NUMTYPE,NUMTYHIQ    TEST TRX COUNT EL, HIGH LVL A/C              
         BNE   PRCACN56                                                         
         ICM   RE,15,NUM#TRX                                                    
         DROP  RF                                                               
PRCACN60 STCM  RE,15,ACAC#TRX                                                   
         CLI   FAKEFLD,C'*'        TEST SINGLE -VE ACCOUNT                      
         BE    *+10                                                             
         LR    RF,RE               ELSE SIMPLE ACCOUNT                          
         B     PRCACN61                                                         
         ICM   RF,15,ACLE#TRX      TRX COUNT ON LEDGER                          
         SR    RF,RE               ...MINUS A/C TOTAL                           
         BNM   PRCACN61            OK                                           
         MVI   RUNJOB,RUNLONG      SOMETHING WRONG - DEFAULT TO LONG            
         B     PRCACN73                                                         
PRCACN61 MVI   RUNJOB,RUNSHORT                                                  
         C     RF,=A(MAXSHORT)     TEST MAX TRX COUNT FOR SHORT SOON            
         BNH   *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
         B     PRCACN73                                                         
                                                                                
PRCACN62 CLI   APPARM,0            TEST MULTIPLE ENTRIES                        
         BH    PRCACN64            YES                                          
         MVI   RUNJOB,RUNLONG      ELSE NO DATA                                 
         CLI   SOONVLUE,1          NO DATA                                      
         BH    PRCACNEX            IF > 1 THEN OK TO SOON OR OVERNIGHT          
         CLI   RQFRPT,ONLY         FORMAT ONLY REPORT ?                         
         BE    PRCACNEX            YES,   OKAY AS   IS                          
*&&US*&& CLI   APACTN,ACTSOON      ARE    WE   SOONING                          
*&&US*&& BE    IVALSOON            YES,   ERROR                                 
         B     PRCACNEX            NO,    OKAY AS  IS                           
*                                                                               
PRCACN64 DS    0H                                                               
         CLI   RQFRPT,ONLY         FORMAT ONLY REPORT ?                         
         BE    PRCACN66                                                         
*&&US*&& CLI   APACTN,ACTSOON      ARE    WE   SOONING                          
*&&US*&& BE    IVALSOON            YES,   ERROR                                 
*                                                                               
PRCACN66 DS    0H                                                               
         CLI   APPARM,2            +/-LIST     ?                                
         BNE   PRCACN75            NO,    INSERT   MULTIPLE DATA                
*                                  YES,   +/-LIST                               
         MVI   RUNJOB,RUNLONG                                                   
         CLC   ACQUNT(2),SPACES    ANY    UNIT/LEDGER DATA ?                    
         BH    PRCACN68            YES,   SKIP                                  
         CLI   LDGUSED,0           ANY    UNIT/LEDGER LIST ?                    
         BNE   IVALMIS1            NO,    SHOULD NOT  HAPPEN                    
*                                                                               
PRCACN68 DS    0H                                                               
         CLI   LDGUSED,0           ANY    UNIT/LEDGER LIST ?                    
         BNE   *+10                YES,   SKIP                                  
*                                  NO,    USE  THE  LEDGER                      
         MVC   LDGSAVE(LUNLG),ACQUNT                                            
*                                  VALIDATE    THE LIST                         
         GOTO1 VALLIST,APPARM,(LDGUSED,LDGSAVE),(FAKEFLDH+7,FAKEFLD),  >        
               1,TX#                                                            
         LTR   RF,RF               VALID  LIST ?                                
         BNZ   IVALLIST            NO,    ERROR                                 
         ICM   RE,15,ACAC#TRX      A/C LIST'S TRX COUNT                         
         CLI   FAKEFLD,C'-'        TEST -VE LIST                                
         BE    *+10                                                             
         LR    RF,RE               ELSE +VE LIST                                
         B     PRCACN70                                                         
         ICM   RF,15,ACLE#TRX      TRX COUNT ON LEDGER                          
         SR    RF,RE               ...MINUS A/C LIST TOTAL                      
         BNM   PRCACN70            OK                                           
         MVI   RUNJOB,RUNLONG      SOMETHING WRONG - DEFAULT TO LONG            
         B     PRCACN75                                                         
PRCACN70 MVI   RUNJOB,RUNSHORT                                                  
         C     RF,=A(MAXSHORT)     TEST MAX TRX COUNT FOR SHORT SOON            
         BNH   *+8                                                              
         MVI   RUNJOB,RUNLONG                                                   
         B     PRCACN75                                                         
*                                                                               
PRCACN73 DS    0H                  SINGLE ACCOUNT                               
         LA    RE,ACQUNT           MOVE   IN   UNIT/LEDGER/ACCOUNT              
         LA    RF,LULACNT-1        LENGTH OF   UNIT/LEDGER/ACCOUNT - 1          
         CLI   MULTLDGR,YES        MULTIPLE    LEDGER TYPE?                     
         BE    PRCACN80                                                         
*                                                                               
PRCACN75 DS    0H                                                               
         LA    RE,ACQACT           ADDR   OF   ACCOUNT IN CARD                  
         LA    RF,LACCOUNT-1       LENGTH OF   ACCOUNT - 1                      
*                                                                               
PRCACN80 DS    0H                                                               
         CLI   FAKEFLD,C'*'        EXCLUDE     SPECIFIED ?                      
         BE    PRCACN90            YES,   PROCESS  EXCLUDE                      
         EXMVC RF,0(RE),FAKEFLD    INSERT FIELD                                 
         B     PRCACNEX            EXIT                                         
*                                                                               
PRCACN90 DS    0H                                                               
         EXMVC RF,0(RE),FAKEFLD+1  INSERT FIELD                                 
         NI    ACQACT,TURNOFF-X'40'       MAKE LOWER CASE                       
*                                                                               
PRCACNEX DS    0H                                                               
         MVC   RCAP#,SVRCAP#       RESTORE     RECAP NUMBER                     
         B     XIT                 EXIT                                         
*                                                                               
PRCACNER DS    0H                  INVALID     ACCOUNT                          
         B     IVALACCT            ISSUE  THE  MESSAGE                          
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  GET PHASE OFF OF LOADLIB TO SCAN FOR REPORT DETAILS                *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
         USING ACQD,R3                                                          
GETAPG   NTR1  ,                                                                
         LA    R2,APRECKEY                                                      
         LA    R3,REQCARDS                                                      
         L     R4,AAPGIO                                                        
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
GETAPG10 CLI   0(R4),X'FF'         END OF PHASE?                                
         BE    GETAPG90                                                         
         CLI   0(R4),CMREAD        SINGLE LEDGER?                               
         BE    GETAPG20                                                         
         CLI   0(R4),CMRLST        MULTI LEDGER                                 
         BE    GETAPG30                                                         
*                                                                               
GETAPG15 SR    RF,RF                                                            
         IC    RF,1(,R4)                                                        
         AR    R4,RF                                                            
         B     GETAPG10                                                         
*                                                                               
GETAPG20 MVI   MULTLDGR,NO         SINGLE LEDGER TYPE?                          
         CLC   ACQUNT(2),SPACES                                                 
         BNE   GETAPG30                                                         
         MVC   ACQUNT(2),2(R4)     MOVE IN UNIT LEDGER                          
         MVI   APPARM,0                                                         
         B     GETAPG15                                                         
*                                                                               
GETAPG30 MVI   MULTLDGR,YES        MULTIPLE LEDGER TYPE?                        
         MVI   APPARM,1            SET TO MULTI                                 
         MVC   ACQUNT(2),SPACES                                                 
         B     GETAPG15                                                         
*                                                                               
GETAPG90 B     XIT                                                              
         EJECT ,                                                                
*&&DO                                                                           
***********************************************************************         
*  VALIDATE OFFICE                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING OFFALD,R1                                                        
VALOFF   NTR1  ,                                                                
         LR    R4,R1               R1 = FIELD SCREEN HEADER                     
         MVC   CUROFF,SPACES       CLEAR    FIELD                               
         GOTO1 AFVAL                                                            
         BL    VALOFF98            NO       INPUT                               
         BH    VALOFF99            NOT      VALID INPUT                         
         CLI   FVIFLD,C'('         SPECIAL  LIST?                               
         BE    VALOFF98                                                         
         L     R1,AOFFBLK                                                       
         TM    OFFACST4,X'01'      TEST     NEW OFFICE                          
         BO    VALOFF20                                                         
*                                  ONE      BYTE OFFICES                        
         MVC   OFFAOFFC(1),FVIFLD  INSERT   OFFICE                              
         CLI   FVILEN,1            INPUT    LENGTH 1 ?                          
         BE    VALOFF05            YES,     SKIP                                
         CLI   FVIFLD,C'*'         EXCLUDE  OFFICE DATA                         
         BNE   VALOFF99            NO,      ERROR                               
         CLI   FVILEN,2            INPUT    LENGTH 2 ?                          
         BNE   VALOFF99            NO,      ERROR                               
         MVC   OFFAOFFC(1),FVIFLD+1 INSERT  OFFICE                              
*                                                                               
VALOFF05 MVI   OFFAACT,OFFAPST                                                  
         MVC   CUROFF(1),OFFAOFFC  SAVE     OFFICE ID                           
         B     VALOFF30            VALIDATE                                     
*                                                                               
VALOFF20 DS    0H                  TWO      BYTE OFFICES                        
         LA    RF,FVIFLD           INPUT    AREA                                
         LA    RE,2                EXPECTED DATA LENGTH                         
         CLI   FVIFLD,C'*'         EXCLUDE  REQUESTED ?                         
         BNE   VALOFF25            NO,      SKIP                                
         LA    RF,1(,RF)           START    OF   DATA                           
         LA    RE,3                EXPECTED DATA LENGTH                         
*                                                                               
VALOFF25 DS    0H                                                               
         CLM   RE,1,FVILEN         LENGTH   OK   ?                              
         BNE   VALOFF99            NO,      ERROR                               
         MVC   OFFAOFFC,0(RF)      OFFICE                                       
         MVC   CUROFF,0(RF)        SAVE     OFFICE                              
         MVC   OFFAREQL,AREQOFFL                                                
         MVI   OFFAACT,OFFAREQ                                                  
*                                                                               
VALOFF30 DS    0H                                                               
         GOTO1 VOFFAL              VALIDATE OFFICE                              
         BNE   VALOFF99            NOT      VALID                               
         CLI   FVIFLD,C'*'         EXCLUDE  REQUESTED ?                         
         BNE   VALOFF98            NO,      OFFICE OK SO EXIT                   
         NI    CUROFF,TURNOFF-X'40' EXCLUDE OFFICE                              
*                                                                               
VALOFF98 SR    RE,RE               SET CC = OK                                  
*                                                                               
VALOFF99 LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R1                                                               
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE DATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALDTES  NTR1  ,                                                                
         L     R4,0(,R1)           ->    INPUT FIELD       HEADER               
         L     R2,4(,R1)           ->    OUTPUT     DATE                        
         XC    0(L'STRDTE,R2),0(R2)                                             
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALDTE98            NO INPUT,  EXIT OK                           
         CLI   APREPJCL,REPJCL2    P&L ?                                        
         BNE   VALDTE10                                                         
         MVC   FVMSGNO,=AL2(1835)  DATES NOT ALLOWED                            
         B     VALDTE99                                                         
*                                                                               
         USING SOFDATD,R1                                                       
VALDTE10 LA    R1,SOFBLOCK         ->    SOFDAT     BLOCK                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R4,SOFAINP                                                       
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVC   SOFLANG,CULANG                                                   
         MVC   SOFACFST,FISCALMO                                                
         MVI   SOFITYPE,SOFITYMD                                                
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT+SOFIIONE+SOFIIRES+SOFIISLH            
         MVI   SOFOTYPE,SOFOTSD2                                                
         ST    R2,SOFAOUT                                                       
         GOTO1 ASOFDAT                                                          
         BZ    VALDTE98                                                         
         MVC   FVMSGNO,SOFERROR    SET ERROR NUMBER                             
         B     VALDTE99                                                         
*                                                                               
VALDTE98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALDTE99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE MOA RANGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALMOA   NTR1  ,                                                                
         LR    R4,R1               R1 = FIELD SCREEN HEADER                     
         MVC   MOASTR,SPACES                                                    
         MVC   MOAEND,SPACES                                                    
         GOTO1 AFVAL                                                            
         BNE   VALMOA98            NO INPUT, SO VALID                           
         LA    R1,SOFBLOCK                                                      
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R4,SOFAINP                                                       
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVC   SOFLANG,CULANG                                                   
         MVC   SOFACFST,FISCALMO                                                
         MVI   SOFITYPE,SOFITYM                                                 
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT+SOFIIF1O+SOFIIRES                     
         MVI   SOFOTYPE,SOFOTSD1                                                
         LA    R0,MOASTR                                                        
         ST    R0,SOFAOUT                                                       
         GOTO1 ASOFDAT                                                          
         BZ    VALMOA98                                                         
         MVC   FVMSGNO,SOFERROR    SET ERROR NUMBER                             
         B     VALMOA99                                                         
*                                                                               
VALMOA98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALMOA99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R1                                                               
         EJECT ,                                                                
         USING ACQD,R3                                                          
GETFFLD  NTR1  ,                                                                
         LA    R0,8                                                             
         L     R3,0(,R1)                                                        
         LA    R4,ACQTYP1                                                       
*                                                                               
GETFFLD4 CLI   0(R4),C' '                                                       
         BNH   GETFFLD9                                                         
         LA    R4,ACQTYP2-ACQTYP1(,R4)  BUMP TO NEXT FIELD                      
         CLM   R0,1,=AL1(4)             GO TO 2ND CARD?                         
         BNE   *+8                                                              
         LA    R4,ACQTYP5          POINT TO 1ST FIELD 4TH CARD                  
         BCT   R0,GETFFLD4                                                      
         SR    R4,R4               SET TO ZERO TO SAY NO                        
*                                                                               
GETFFLD9 ST    R4,0(,R1)           SAVE ADDRESS                                 
         B     XIT                                                              
         DROP  R3                                                               
         EJECT ,                                                                
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
WILDCARD NTR1  ,                                                                
         L     RF,0(,R1)           START OF FIELD                               
         SR    R2,R2                                                            
         ICM   R2,1,7(R1)          FIELD LENGTH                                 
*                                                                               
         CLI   4(R1),1             SPECIAL UNIT/LEDGER REQUEST ?                
         BNE   WILD200             NO,  SKIP                                    
*                                                                               
         LA    R3,2                CHECK FIRST 2 BYTES OF UNIT/LEDGER           
*                                                                               
WILD100  DS    0H                                                               
         CLI   0(RF),C'?'          UNIT/LEDGER HAS WILDCARD ?                   
         BE    WILDER              WILDCARD INVALID                             
         LA    RF,1(,RF)           NEXT CHARACTER                               
         BCTR  R2,0                SUBTRACT ONE FROM OVERALL LENGTH             
         BCT   R3,WILD100          LOOP                                         
*                                                                               
WILD200  DS    0H                  GENERAL TEST FOR WILDCARD                    
         CLI   0(RF),C'?'          WILDCARD ?                                   
         BE    WILDYES             WILDCARD FOUND                               
         LA    RF,1(,RF)           NEXT CHARACTER                               
         BCT   R2,WILD200                                                       
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
         L     RF,=F'-1'           RETURN -1 IN REGISTER 15                     
*        B     WILDEX              EXIT                                         
*                                                                               
WILDEX   DS    0H                  WILDCARD EXIT                                
         XIT1  REGS=(RF)                                                        
         EJECT ,                                                                
***********************************************************************         
* ERROR TO DISPLAY ON TOP OF SCREEN                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING ACQD,R3                                                          
         SPACE 1                                                                
IVALOVLY MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     EXIT99                                                           
         SPACE 3                                                                
IVALSEC  MVC   FVMSGNO,=AL2(ACEIVSEC)                                           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALOFF  MVC   FVMSGNO,=AL2(ACEIVOF)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALACT  MVC   FVMSGNO,=AL2(12)    INVALID ACTION                               
         B     IVALEXIT                                                         
         SPACE 1                                                                
*                                  LEDGER NOT SET UP                            
IVALNOUL MVC   FVXTRA(LUNLG),ACQUNT                                             
         MVC   FVMSGNO,=AL2(110)                                                
         B     IVALEXIT            NOT SET UP FOR COMPANY                       
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALMIS1 DS    0H                  ERROR  MISSING U/L INPUT                     
         LA    RE,RUNUNLH                                                       
         ST    RE,FVADDR                                                        
*                                  MISSING INPUT FIELD                          
IVALMISS MVC   FVMSGNO,=AL2(FVFMISS)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALMRSP MVC   FVMSGNO,=AL2(136)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALNUM  MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALFMT  MVC   FVMSGNO,=AL2(ACEIVFT)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
*                                  INVALID ACCOUNT                              
IVALACCT MVC   FVMSGNO,=AL2(ACEACCT)                                            
*                                  INSERT  UNIT LEDGER                          
         MVC   FVXTRA(LUNLG),ACQUNT                                             
*                                  INSERT  ACCOUNT                              
         MVC   FVXTRA+LUNLG(20),FVIFLD                                          
         B     IVALEXIT                                                         
         SPACE 1                                                                
*                                  INVALID LEDGER                               
IVALLEDG MVC   FVXTRA(LULACNT),FAKEFLD                                          
         MVC   FVMSGNO,=AL2(ACELEDG)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALULFA MVC   FVMSGNO,=AL2(108)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALSTUP MVC   FVMSGNO,=AL2(ACESETUP)                                           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALRQUL MVC   FVMSGNO,=AL2(ACEULCFL)                                           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALSIGN MVC   FVMSGNO,=AL2(ACELTSG)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALLIST MVC   FVMSGNO,=AL2(ACEIVLT)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALREQ  MVC   FVMSGNO,=AL2(ACEIVRQ)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALDATE MVC   FVMSGNO,=AL2(ACEIVDT)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALUNLD MVC   FVMSGNO,=AL2(ACEIVUL)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALSOON MVC   FVMSGNO,=AL2(ACESOON)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALLST  MVC   FVMSGNO,=XL2'FF09'                                               
         MVI   FVOMTYP,GTMINF                                                   
         LA    RE,RUNSEL1H                                                      
         ST    RE,FVADDR                                                        
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEMSG MVC   FVMSGNO,=AL2(2096)          FORMAT CONTAINS ERROR MSGS           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALFRPT MVC   FVMSGNO,=AL2(ACEDWFRC)      DOWN-LOAD AND FORMAT REPORT          
         B     IVALEXIT                         CONFLICT                        
*&&UK                                                                           
         SPACE 1                                                                
IVALUSER MVC   FVXTRA(L'INUSER),INUSER                                          
         MVC   FVMSGNO,=AL2(1653)                                               
         B     IVALEXIT                                                         
*&&                                                                             
         SPACE 3                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT ,                                                                
REQUEST  DC    CL8'ACCREQ'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMGETR   DC    CL8'GETREC  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
TEMPSTR  DC    CL8'TEMPSTR '                                                    
         EJECT ,                                                                
*                                  IN FLTLIST X'0000' MEANS THAT THE            
*                                  FIELD IS 14 BYTES LONG OR 15 BYTES           
*                                  LONG IF/WHEN EXCLUDE IS SUPPORTED            
FLTLIST  DC    AL1(RFLBSR),AL2(1694),C'  '                                      
         DC    AL1(RFLCLI),AL2(1681),C'SJ'                                      
         DC    AL1(RFLDEPT),AL2(1682),C'2D'                                     
         DC    AL1(RFLPRSN),AL2(1683),C'2P'                                     
         DC    AL1(RFLVNDR),AL2(1684),X'0000'                                   
         DC    AL1(RFLCOST),AL2(1685),C'1C'                                     
         DC    AL1(RFLXCAT),AL2(1686),C'13'                                     
         DC    AL1(RFLPRTP),AL2(1688),X'0000'                                   
         DC    AL1(RFLCNTR),AL2(1695),X'0000'                                   
         DC    X'FF'                                                            
         EJECT ,                                                                
FLTLIST2 DS    0C                                                               
         DC    AL1(RFLBLGP,L'ACQBILGP)                 BILLING  GROUP           
         DC    AL2(ACQBILGP-ACQD)                                               
         DC    AL1(RFLWCGP,L'ACQWCGRP)                 WORKCODE GROUP           
         DC    AL2(ACQWCGRP-ACQD)                                               
         DC    AL1(RFLWC,L'ACQWRKLS)                   WORK     CODE            
         DC    AL2(ACQWRKLS-ACQD)                                               
         DC    AL1(RFLBTYP,L'ACQBILTY)                 BILLING  TYPE            
         DC    AL2(ACQBILTY-ACQD)                                               
         DC    AL1(RFLOFGP,L'ACQOFGRP)                 OFFICE   GROUP           
         DC    AL2(ACQOFGRP-ACQD)                                               
         DC    AL1(RFLMDGP,L'ACQMEDGP)                 MEDIA    GROUP           
         DC    AL2(ACQMEDGP-ACQD)                                               
         DC    AL1(RFLMED,L'ACQMEDFL)                  MEDIA                    
         DC    AL2(ACQMEDFL-ACQD)                                               
         DC    AL1(RFLSTTY,L'ACQSTUTY)                 STUDIO   TYPE            
         DC    AL2(ACQSTUTY-ACQD)                                               
         DC    AL1(RFLUFLD,L'ACQUSFLD)                 USER     FIELD           
         DC    AL2(ACQUSFLD-ACQD)                                               
         DC    AL1(RFLTTYPE,L'ACQTTYPE)                TRANSACT TYPE            
         DC    AL2(ACQTTYPE-ACQD)                                               
         DC    AL1(RFLAOFF,L'ACQANOF)                  ANALYSIS OFFICE          
         DC    AL2(ACQANOF-ACQD)                                                
         DC    AL1(RFLMTHD,L'ACQMTHD)                  COST     METHOD          
         DC    AL2(ACQMTHD-ACQD)                                                
         DC    AL1(RFLLOCS,L'ACQLOCS)                  LOCATION STATUS          
         DC    AL2(ACQLOCS-ACQD)                                                
         DC    AL1(RFLCFLT1,L'ACQCFLT1)                CONTRA   FILTER          
         DC    AL2(ACQCFLT1-ACQD)                               1               
         DC    AL1(RFLCFLT2,L'ACQCFLT2)                CONTRA   FILTER          
         DC    AL2(ACQCFLT2-ACQD)                               2               
         DC    AL1(RFLCFLT3,L'ACQCFLT3)                CONTRA   FILTER          
         DC    AL2(ACQCFLT3-ACQD)                               3               
         DC    AL1(RFLCFLT4,L'ACQCFLT4)                CONTRA   FILTER          
         DC    AL2(ACQCFLT4-ACQD)                               4               
         DC    AL1(RFLCFLT5,L'ACQCFLT5)                CONTRA   FILTER          
         DC    AL2(ACQCFLT5-ACQD)                               5               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
         LTORG                                                                  
         SPACE 3                                                                
         DROP  R5,R7,R9,RA                                                      
         EJECT ,                                                                
***********************************************************************         
*  FILLFLD - INSERT ELEMENT DATA                                      *         
*                                                                     *         
*  ON INPUT:                                                          *         
*     PARM LIST:                                                      *         
*        P1  = A(IO AREA OF RECORD)                                   *         
*        P2  = AL1(FIELD TYPE # OF X'C5' ELEMENT)                     *         
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
         USING WORKD,R7                                                         
         SPACE 1                                                                
FILLFLD  NMOD1 0,**FILL**                                                       
         L     RC,APALOCAL                                                      
         L     R2,0(,R1)           A(IO AREA)                                   
         ICM   R4,15,8(R1)         CLEAR NAME FIELD IN ANY                      
         BZ    FILL10              NOTHING TO PUT                               
         SR    RF,RF                                                            
         IC    RF,0(,R4)           GET LENGTH OF FIELD                          
         SHI   RF,9                MINUS FIELD HEADER                           
         TM    1(R4),FVAXTND       EXTENDED HEADER?                             
         BZ    *+8                                                              
         SHI   RF,8                MINUS EXTENDED FIELD HEADER                  
         EXMVC RF,8(R4),SPACES     CLEAR FIELD                                  
*                                                                               
FILL10   L     R4,4(,R1)           A(FIELD HEADER)                              
         LA    R4,0(,R4)           CLEAR HOB                                    
         MVC   TYPEC5,4(R1)        SAVE TYPE # FOR X'C5' ELEMENT                
         XC    0(8,R1),0(R1)                                                    
         MVI   EXTFLD,NO                                                        
         CLI   RCAP#,0             ANY  RECAP  REQUESTED ?                      
         BE    FILL15              NO,  CONTINUE                                
         CLI   TYPEC5,RFLLDG       UNIT LEDGER REQUESTED ?                      
         BNE   FILL90              NO,  DO     NOT  FILL THIS FIELD             
         CLI   APREPJCL,REPJCL2    PROFIT      AND  LOSS ?                      
         BE    FILL15              YES, CONTINUE                                
         CLI   APREPJCL,REPJCLB    CASH ?                                       
         BNE   FILL90              NO,  DO     NOT  FILL THIS FIELD             
*                                                                               
         USING RFLELD,R2                                                        
*                                                                               
FILL15   SR    R6,R6                                                            
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
         SHI   R6,(RFLLNQ+1)       EXMVC LENGTH                                 
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
         BH    FILL50              MARK AS MULTIPLE ITEMS                       
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
         BCTR  R3,0                MINUS ONE FOR MOVE                           
         EXMVC R3,8(R4),SPACES     CLEAR FIELD                                  
         CR    R6,RF                                                            
         BNL   FILL50              TOO BIG A FIELD                              
*                                                                               
         LR    RF,R6               LENGTH                                       
         TM    RFLIND,RFLXCLD      EXCLUDE ON IN RECORD ?                       
         BZ    *+6                                                              
         BCTR  RF,0                                                             
         LTR   RF,RF               If zero then data is one character           
         BZ    FILL70              Can't be a list                              
         BM    FILL90              NO DATA                                      
*                                                                               
FILL45   LA    RE,RFLDATA(RF)      POINT TO END OF DATA                         
         CLC   SCCOMMA,0(RE)       LOOK FOR DELIMITER (COMMA)                   
         BE    FILL47              ONLY ONE FIELD ALLOWED                       
         BCT   RF,FILL45           LOOP                                         
         B     FILL60                                                           
*                                                                               
FILL47   LR    RF,R6                                                            
         LA    RE,RFLDATA(RF)      POINT TO END OF DATA AGAIN                   
         LA    RF,RFLDATA          POINT TO START OF DATA                       
         CLI   0(RF),C'?'          STARTS WITH WILDCARD?                        
         BE    FILL49                                                           
*                                                                               
FILL48   CR    RE,RF               END OF LIST?                                 
         BE    FILL50                                                           
         CLC   SCCOMMA,0(RF)       LOOK FOR COMMA                               
         BE    *+12                                                             
         AHI   RF,1                                                             
         B     FILL48                                                           
         CLI   1(RF),C'?'          STARTS WITH WILDCARD?                        
         BE    FILL49                                                           
         AHI   RF,1                                                             
         B     FILL48                                                           
*                                                                               
FILL49   MVI   4(R1),1             LIST STARTS WITH WILDCARDS                   
*                                                                               
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
         SHI   RF,8                MINUS FIELD HEADER                           
         TM    1(R4),FVAXTND       EXTENDED HEADER?                             
         BZ    *+8                                                              
         SHI   RF,8                MINUS EXTENDED FIELD HEADER                  
         BCTR  RF,0                MINUS ONE FOR POSSIBLE EXECUTE               
         CR    R6,RF               RF=LENGTH OF FIELD VS FIELD SIZE             
         BNH   FILL80              TOO BIG A FIELD?                             
         MVI   EXTFLD,YES          YES                                          
         LR    R6,RF               USE R0 INSTEAD                               
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
*                                                                               
         LA    R3,IOKEY                                                         
         MVC   LSTKEY,SPACES                                                    
         MVI   LSTKTYP,LSTKTYPQ    X'1D'                                        
         MVC   LSTKCPY,CUABIN      COMPANY CODE                                 
         LR    RF,R6                                                            
         BCTR  RF,0                LESS THE C"+" OR C"-"                        
         EX    RF,*+8                                                           
         B     *+10                                                             
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
*                                                                               
         DROP  R3                                                               
*                                                                               
*                                  ************************************         
FILL70   DS    0H                  * ONE ITEM                         *         
*                                  ************************************         
         MVI   0(R1),1             MARK AS AN ACCOUNT                           
*                                                                               
********************************** NOT  APPLICABLE ********************         
*                                                                               
*                                  ************************************         
*                                  * IN THIS MODULE, IN CASE OF       *         
*                                  * PAYABLES OR EXPENSE FOR ACCOUNT, *         
*                                  * THE WHOLE FIELD (INCLUDING THE   *         
*                                  * UNIT/LEDGER) IS MOVED TO THE     *         
*                                  * FAKE SCREEN FIELD) !  THEREFORE, *         
*                                  * THE FOLLOWING STANDARD CODE IS   *         
*                                  * COMMENTED OUT.                   *         
*                                  ************************************         
*        CLI   RFLTYPE,RFLACC                                                   
*        BNE   FILL80                                                           
*        CLI   APREPJCL,REPJCLP    PAYABLES                                     
*        BE    *+8                                                              
*        CLI   APREPJCL,REPJCLX    EXPENSE                                      
*        BNE   FILL80                                                           
*        L     RF,AFLDUNLG         POINT TO U/L HEADER FIELD                    
*        A     RF,ATWA                                                          
*        MVC   8(2,RF),RFLDATA                                                  
*        OI    6(RF),FVOXMT        TRANSMIT FIELD                               
*        TM    RFLIND,RFLXCLD      EXCLUDE DATA TYPE?                           
*        BZ    FILL75              NO,  SKIP                                    
*        MVI   8(R4),C'*'          YES, SHOW AS EXCLUDE                         
*        SH    R6,=H'03'           SUBTRACT 1 FOR "*" AND 2 FOR U/L             
*        EXMVC R6,9(R4),RFLDATA+2  MOVE DATA TO SCREEN                          
*        OI    6(R4),FVOXMT        TRANSMIT FIELD                               
*        LA    RF,2(,R6)           GET  REAL LENGTH                             
*        STC   RF,7(,R4)           SAVE IN FIELD HEADER                         
*        B     FILL90                                                           
*                                                                               
*FILL75  DS    0H                                                               
*        SH    R6,=H'02'           SUBTRACT 2 FOR UNIT/LEDGER                   
*        EXMVC R6,8(R4),RFLDATA+2  MOVE DATA TO SCREEN                          
*        LA    RF,1(,R6)           GET  REAL LENGTH                             
*        STC   RF,7(,R4)           SAVE IN FIELD HEADER                         
*        OI    6(R4),FVOXMT        TRANSMIT FIELD                               
*        B     FILL90                                                           
********************************** NOT  APPLICABLE END ****************         
*                                  ************************************         
FILL80   DS    0H                  * MOVE DATA TO SCREEN              *         
*                                  ************************************         
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         LA    R8,8(,R4)           POINT PAST FIELD HEADER                      
         LR    RF,R6               SAVE  EXMVC LENGTH                           
         TM    RFLIND,RFLXCLD      EXCLUDE DATA TYPE?                           
         BZ    FILL88                                                           
         MVI   0(R8),C'*'          SHOW AS EXCLUDE                              
         SH    RF,=H'01'           SUBTRACT ONE                                 
         BM    FILL90                                                           
         LA    R8,1(,R8)           BUMP UP BECAUSE OF C'*'                      
*                                                                               
FILL88   EXMVC RF,0(R8),RFLDATA    MOVE IN DATA                                 
         LA    RF,1(,R6)           GET  REAL LENGTH                             
         STC   RF,7(,R4)           SAVE IN FIELD HEADER                         
         CLI   EXTFLD,YES                                                       
         BNE   FILL90                                                           
         LA    RE,8(R6,R4)                                                      
         MVI   0(RE),C'>'          INDICATE TRUNCATED DATA                      
*                                                                               
FILL90   XIT1                                                                   
*                                                                               
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
         SPACE 1                                                                
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
         BNE   FILLTY15                                              *          
         LA    RF,APWORK+1                                                      
         BCTR  R1,0                                                             
*                                  R1   IS THE REAL LENGTH                      
FILLTY15 DS    0H                  RF   POINTS TO DATA WITH NO "*"              
         BCTR  R1,0                GET  MOVE LENGTH                             
         EXMVC R1,NEW.RFLDATA,0(RF)     INSERT INTO DUMMY RCD AREA              
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
*                                                                               
         DROP  R2,R7                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  CHECK MESSAGE ELEMENTS FOR ERROR TYPE MESSAGES                     *         
*                                                                     *         
*  USES:                                                              *         
*    APELCODE - MESSAGE ELEMENT (IN    APELEM)                        *         
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
*    RCAP#       - NUMBER   OF      RECAP PROGRAMS                              
*    RCAPSW      - RECAP    SWITCHES                                            
***********************************************************************         
         SPACE 1                                                                
         USING RCPELD,R3           RECAP   ELEMENT                              
         USING WORKD,R7                                                         
         SPACE 1                                                                
CKMSGERR NMOD1 0,**CMNO**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVMSGNO,FVMSGNO     SAVE    FVMSGNO                              
         SR    R6,R6               NO      ERRORS  FOUND                        
         MVI   RCAP#,0             NO      RECAP   RECORDS                      
         MVI   RCAPSW,0            CLEAR   RECAP   SWITCHES                     
         L     R1,AIOAREA1         ->      RECORD                               
*                                  CHECK   THIS    RECORD  FOR  ERRORS          
         GOTO1 =A(CKRMSGER),(R1),RR=APRELO                                      
         BE    CK1MSGE1            ON      ERROR,  FORMAT  HAS  ERRORS          
*                                                                               
         L     R1,AIOAREA1         ->      RECORD                               
         MVI   APELCODE,RCPELQ     FIND    RECAP   RECORDS                      
         GOTO1 GETEL               FIND    MESSAGE ELEMENT                      
         BNE   CK1MSGOK            NONE    FOUND,  EXIT    OKAY                 
         LR    R3,R1               ->      ELEMENT                              
*                                  CHECK   FOR     RECAP   ERRORS               
         GOTO1 =A(CKRCPMSG),APPARM,AIOAREA1,RCPCODE,RR=APRELO                   
         BE    CK1MSGNG            ON      ERROR,  EXIT    NOT  GOOD            
*                                                                               
CK1MSG10 DS    0H                  FIND    NEXT    RECAP   ELEMENT              
         LR    R1,R3               ->      ELEMENT                              
         MVI   APELCODE,RCPELQ     FIND    RECAP   RECORDS                      
         GOTO1 NEXTEL              FIND    IT                                   
         BNE   CK1MSGOK            ELEMENT NOT     FOUND,  EXIT OKAY            
         LR    R3,R1               ->      ELEMENT                              
         MVC   RCAP#,RCPSEQ        SAVE    NUMBER  OF      RECAP   PGMS         
         TM    RCPOPT1,RCPPROF     USE     RECAP'S PROFILE ?                    
         BO    CK1MSG20            YES,    TURN    ON      RECAP   PROF         
         OI    RCAPSW,RCAPFUMP     NO,     FOUND   USE     MAIN'S  PROF         
         B     CK1MSG30            CONTINUE                                     
*                                                                               
CK1MSG20 DS    0H                  TURN    ON      RECAP   PROFILE              
         OI    RCAPSW,RCAPFURP     NO,     FOUND   USE     RECAP'S PROF         
*                                                                               
CK1MSG30 DS    0H                  GET     RECAP   RECORD                       
*                                                                               
         USING RESRECD,R2          MAP     SCRIBE  RECORDS                      
*                                                                               
         LA    R2,IOKEY            ->      I/O     KEY                          
         MVC   RESKEY,SPACES       CLEAR   KEY                                  
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY  CODE                                
*                                  FORMAT   CODE                                
         MVC   RESKFORM(L'RCPCODE),RCPCODE                                      
         LA    R1,IORDD+IOACCFIL+IO2        READ   THE     RECORD               
         GOTO1 AIO                                                              
         BNE   CK1MSGE2            BAD      READ,  RCD     NOT  FOUND           
         L     R4,AIOAREA2         ->       RECORD                              
         DROP  R2                                                               
*                                                                               
*                                  CHECK   THIS    RECORD  FOR  ERRORS          
         GOTO1 =A(CKRMSGER),(R4),RR=APRELO                                      
         BE    CK1MSGE3            ON      ERROR,  FORMAT  HAS  ERRORS          
*                                                                               
*                                  CHECK   FOR     RECAP   ERRORS               
         GOTO1 =A(CKRCPMSG),APPARM,(R4),RCPCODE,RR=APRELO                       
         BE    CK1MSGNG            ON      ERROR,  EXIT    NOT  GOOD            
*                                                                               
         B     CK1MSG10            OKAY,   CHECK   NEXT    FORMAT               
*                                                                               
CK1MSGE1 DS    0H                                                               
         MVC   FVMSGNO,=AL2(2096)  FORMAT  CONTAINS        ERRORS               
         B     CK1MSGNG            EXIT NOT     GOOD                            
*                                                                               
CK1MSGE2 DS    0H                                                               
         MVC   FVMSGNO,=AL2(2105)  RECAP   REPORT  NOT  FOUND                   
         MVC   FVXTRA(L'RCPCODE),RCPCODE                                        
         B     CK1MSGNG            EXIT                                         
*                                                                               
CK1MSGE3 DS    0H                                                               
         MVC   FVMSGNO,=AL2(2106)  RECAP   REPORT  HAS  ERRORS                  
         MVC   FVXTRA(L'RCPCODE),RCPCODE                                        
*        B     CK1MSGNG            EXIT    NOT     GOOD                         
*                                                                               
CK1MSGNG DS    0H                  EXIT    NOT     GOOD                         
         LA    R6,1                SAY     ERRORS  FOUND                        
         B     CK1MSGEX            EXIT                                         
*                                                                               
CK1MSGOK DS    0H                                                               
         MVC   FVMSGNO,SVMSGNO     RESTORE FVMSGNO                              
         CLI   RCAP#,0             ANY     RECAP   REQUESTS ?                   
         BE    CK1MSGEX            NO,     EXIT                                 
         TM    RCAPSW,RCAPFURP     ANYBODY USING   RECAP'S   PROFILE ?          
         BO    CK1MSGEX            YES,    EXIT                                 
         MVI   RCAP#,0             TREAT   AS      RECAP NOT REQUESTED          
*                                                                               
CK1MSGEX DS    0H                                                               
         C     R6,=F'1'            SET     CONDITION    CODE                    
         XMOD1 ,                   RETURN                                       
*                                                                               
         DROP  R3,R7                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  CHECK MESSAGE ELEMENTS FOR ERROR TYPE MESSAGES FOR THIS RECORD     *         
*                                                                     *         
*  USES:                                                              *         
*    APELCODE - MESSAGE ELEMENT (IN    APELEM)                        *         
*    ELEMSEQ  - COLUMN  NUMBER                                        *         
*                                                                     *         
*  INPUT:                                                             *         
*    R1       - INPUT   RECORD ADDRESS                                *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    CONDITION CODE SET:                                              *         
*      EQUAL     - 'E' TYPE MESSAGE FOUND                             *         
*      NOT EQUAL - 'E' TYPE MESSAGE NOT   FOUND                       *         
***********************************************************************         
         SPACE 1                                                                
         USING MNOELD,R1           MESSAGE ELEMENT                              
WARNP    USING MNOMLN,R2           MESSAGE NUMBER  PORTION                      
         USING WORKD,R7                                                         
         SPACE 1                                                                
CKRMSGER NMOD1 0,**CRMN**                                                       
         L     RC,APALOCAL                                                      
         SR    R6,R6               NO      ERRORS  FOUND                        
*                                  R1      ->      RECORD                       
         MVI   APELCODE,MNOELQ     GET     MESSAGE ELEMENT                      
         GOTO1 GETEL               FIND    MESSAGE ELEMENT                      
         B     CKMSG20             CHECK   MESSAGE ELEMENT                      
*                                                                               
CKMSG10  DS    0H                  FIND    NEXT    MESSAGE ELEMENT              
         GOTO1 NEXTEL              FIND    IT                                   
*                                                                               
CKMSG20  DS    0H                                                               
         BNE   CKMSGEX             ELEMENT NOT     FOUND,  EXIT                 
         LA    R2,MNOLN1Q(,R1)     SET     UP      WARNP                        
         SR    R3,R3               CLEAR   REGISTER                             
         LA    R4,R1               ->      MESSAGE ELEMENT                      
         ZIC   RF,MNOLN            MESSAGE ELEMENT LENGTH                       
         AR    R4,RF               ->      NEXT    ELEMENT                      
*                                                                               
CKMSG40  DS    0H                  CHECK   FOR     MESSAGE TYPE ERROR           
         CLI   WARNP.MNOMTYPE,MNOMTERR                                          
         BNE   CKMSG50             NO,     GET     NEXT    MSG  ID              
         LA    R6,1                SAY     ERRORS  FOUND                        
         B     CKMSGEX             EXIT                                         
*                                                                               
CKMSG50  DS    0H                                                               
         IC    R3,WARNP.MNOMLN     GET     MESSAGE LENGTH                       
         AR    R2,R3               ->      NEXT    MESSAGE                      
         CR    R2,R4               FOUND   NEXT    MESSAGE                      
         BL    CKMSG40             YES,    CHECK   IT                           
         B     CKMSG10             NOT     FOUND,  PROCESS NEXT ELEMENT         
*                                                                               
CKMSGEX  DS    0H                                                               
         C     R6,=F'1'            SET     CONDITION       CODE                 
         XMOD1 ,                   RETURN                                       
*                                                                               
         DROP  R1,R7,WARNP                                                      
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  CHECK MESSAGE ELEMENTS FOR RECAP TYPE ERRORS                       *         
*                                                                     *         
*  USES:                                                              *         
*    APELCODE - MESSAGE ELEMENT (IN    APELEM)                        *         
*    ELEMSEQ  - COLUMN  NUMBER                                        *         
*                                                                     *         
*  INPUT:                                                             *         
*    PARM 1   - INPUT   RECORD ADDRESS                                *         
*    PARM 2   - FORMAT  CODE                                          *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    CONDITION CODE SET:                                              *         
*      EQUAL     - 'E' TYPE MESSAGE FOUND                             *         
*      NOT EQUAL - 'E' TYPE MESSAGE NOT   FOUND                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         SPACE 1                                                                
CKRCPMSG NMOD1 0,**CRMG**                                                       
         L     RC,APALOCAL                                                      
         L     R5,ATWA             ->      TWA                                  
         SR    R6,R6               NO      ERRORS  FOUND                        
         L     R4,0(,R1)           R4      ->      RECORD                       
         L     R2,4(,R1)           R2      ->      FORMAT CODE                  
*                                                                               
         LR    R1,R4               R1      ->      RECORD                       
         MVI   APELCODE,STYELQ     FREE FORM SCRIBE    ELEMENT  (X'25')         
         GOTO1 GETEL               GET  THE  ELEMENT                            
         BE    *+6                 FOUND,    CONTINUE                           
         DC    H'00'               ELSE,     ABEND                              
*                                                                               
         USING STYELD,R3           MAP  FREE FORM ELEMENT                       
         LR    R3,R1               SAVE ADDR OF   ELEMENT                       
         CLC   STYNAME,APREPCDE    SAME REPORT    TYPE ?                        
         BNE   CKRCPNST            NO,  NOT  SAME REPORT    TYPE                
*                                                                               
         OC    TWASAGN,TWASAGN     NEW  SECURITY ?                              
         BZ    CKRCPEX             NO,  EXIT                                    
         SR    RF,RF                                                            
         IC    RF,STYSEC#1         ANY  KEYWORD   SECURITY  SET  UP ?           
         GOTO1 AFMTSEC,APPARM,(RF),1                                            
         CLI   APPARM,0            ANY  PROBLEMS  FOUND ?                       
         BNE   CKRCPSEC            YES, EXIT                                    
         SR    RF,RF                                                            
         IC    RF,STYSEC#5         ANY  OFFICE    SECURITY  SET  UP ?           
         GOTO1 AFMTSEC,APPARM,(RF),33                                           
         CLI   APPARM,0            ANY  PROBLEMS  FOUND ?                       
         BNE   CKRCPSEC            YES, EXIT                                    
         B     CKRCPEX             EXIT OKAY                                    
         DROP  R3                                                               
*                                                                               
CKRCPNST DS    0H                  MAIN AND  RECAP     MUST HAVE SAME           
*                                       REPORT    TYPE                          
         MVC   FVMSGNO,=AL2(ACEMRSRT)                                           
         MVC   FVXTRA(L'RCPCODE),0(R2)                                          
         LA    R6,1                SAY  ERORS     FOUND                         
         B     CKRCPEX             RETURN                                       
*                                                                               
CKRCPSEC DS    0H                  INVALID   SECURITY                           
         MVC   FVMSGNO,=AL2(ACEIVSEC)                                           
         MVC   FVXTRA(L'RCPCODE),0(R2)                                          
         LA    R6,1                SAY  ERORS     FOUND                         
         B     CKRCPEX             RETURN                                       
*                                                                               
CKRCPEX  DS    0H                                                               
         C     R6,=F'1'            SET     CONDITION       CODE                 
         XMOD1 ,                   RETURN                                       
*                                                                               
         DROP  R5,R7                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*              CANCEL & RESTORE REQUESTS                              *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING PTRELD,R1                                                        
         USING RESRECD,R2                                                       
         USING ACQD,R3                                                          
         USING LISTD,R4                                                         
         USING TWAD,R5                                                          
         USING REPTABD,R6                                                       
         USING WORKD,R7                                                         
         SPACE 1                                                                
CNRSREQ  NMOD1 0,**CRREQ**                                                      
         L     RC,APALOCAL                                                      
         L     R4,APLSTADD         LOAD LIST LINE                               
*                                                                               
         MVC   RESKEY,APRECKEY     COPY REQUEST                                 
         ZIC   R3,RESKEY+RESKFRML+1         GET REQUEST NUMBER                  
         MVC   RESKEY+RESKFRML(2),=C'  '    ORIGINAL FORMAT KEY                 
         GOTO1 AIO,IORD+IOACCFIL+IO1                                            
         BE    *+6                                                              
         DC    H'0'                VALID FORMAT NOW INVALID!!!!!                
*                                                                               
         L     R2,AIOAREA1                                                      
         MVI   APELCODE,PTRELQ     GET REQUEST ADDRESS                          
         GOTO1 GETEL,(R2)                                                       
         BNE   CRR999                                                           
*                                                                               
CRR250   CH    R3,=H'63'           MAX AMT OF REQ ADDRESSES ONE 'FA'            
         BNH   CRR270              ELEMENT CAN HOLD                             
         SH    R3,=H'63'                                                        
         GOTO1 NEXTEL,(R1)                                                      
         BNE   CRR999                                                           
         B     CRR250                                                           
*                                                                               
CRR270   LA    RE,PTRCODE                                                       
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         LA    RE,4(,RE)                                                        
         BCT   RF,*-4                                                           
         MVC   ADDR,0(RE)                                                       
*                                                                               
         GOTO1 VDMGR,APPARM,(X'20',DMRDIR),REQUEST1,ADDR,AIOAREA3,DWORK         
         CLI   APPARM+8,0                                                       
         BNE   CRR999                                                           
*                                                                               
         L     R3,AIOAREA3                                                      
         CLI   APACTN,ACTCNCL      CANCEL REQUEST?                              
         BE    CRR500                                                           
*                                                                               
         L     R6,ACTYPTAB         RESTORE REQUEST                              
*                                                                               
CRR300   CLI   REPCODE,EOT         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVI   ACQPROG+L'REQHDR+1,C'L'      LANDSCAPE                           
         CLC   REPLDNO,REQNUMB-REQHDR(R3)                                       
         BE    CRR420                                                           
         MVI   ACQPROG+L'REQHDR+1,C'P'      PORTRAIT                            
         CLC   REPPTNO,REQNUMB-REQHDR(R3)                                       
         BE    CRR420                                                           
         LA    R6,REPLNQ(,R6)                                                   
         B     CRR300                                                           
*                                                                               
CRR420   MVC   ACQPROG+L'REQHDR(1),REPJCLID                                     
         OI    LISTDATH+1,X'08'    HIGH INTENSITY                               
         MVC   LISTNME,SPACES                                                   
*                                                                               
         TM    ACQACT+L'REQHDR,X'40'              UPPER CASE ?                  
         BZ    CRR430                             NO,    MOVE WITH "*"          
         MVC   LISTNME1(LULACNT),ACQUNT+L'REQHDR  MOVE   DATA                   
         B     CRR440                                                           
*                                                                               
CRR430   DS    0H                                 EXCLUDE REQUESTED             
         MVC   LISTNME1(LUNLG),ACQUNT+L'REQHDR    INSERT UNIT/LEDGER            
         MVI   LISTNME1+LUNLG,C'*'                INSERT "*"                    
*                                                 INSERT ACCOUNT                
         MVC   LISTNME1+LUNLG+1(LACCOUNT),ACQACT+L'REQHDR                       
         OI    LISTNME1+LUNLG+1,X'40'             MAKE   UPPER CASE             
*                                                                               
CRR440   DS    0H                                                               
         CLI   ACQCONT1+L'REQHDR,C'C'             ANY    CARD 3 ?               
         BNE   CRR460                             NO,    SKIP LISTNME2          
         CLI   ACQFLT2+L'REQHDR,C'('              PROFILE REQUESTED ?           
         BE    CRR450                             YES,   JUST MOVE              
         TM    ACQFLT2+L'REQHDR,X'40'             EXCLUDE REQUESTED ?           
         BO    CRR450                             NO,    JUST MOVE              
         MVI   LISTNME2,C'*'                      INSERT C"*"                   
*                                                 INSERT UNIT/LEDGER            
         MVC   LISTNME2+1(LULACNT),ACQFLT2+L'REQHDR      AND  ACCOUNT           
         OI    LISTNME2+1,X'40'                   MAKE   UPPER CASE             
         B     CRR460                             CONTINUE                      
*                                                                               
CRR450   DS    0H                                 INSERT DATA                   
         MVC   LISTNME2(L'ACQFLT2),ACQFLT2+L'REQHDR                             
         CLI   ACQTYP2+L'REQHDR,ACQANAL           ANALYSIS ?                    
         BNE   CRR452                             NO,    SKIP                   
         CLI   ACQFLT2+L'REQHDR,C'('              LIST ?                        
         BNE   CRR452                             NO,    SKIP                   
         MVI   LISTNME2+13,C' '                   CLEAR  LAST  BYTE             
*                                                                               
CRR452   DS    0H                                                               
         CLI   ACQTYP2+L'REQHDR,ACQDATE           DATE   FIELD ?                
         BNE   CRR460                             NO,    SKIP                   
*                                                 FROM   YEAR   ....            
         CLI   LISTNME2+1,C'9'                    YEAR   > 1999 ?               
         BNH   CRR455                             NO,    YEAR  OKAY             
         ZIC   R1,LISTNME2+1                      GET    H.O.  BYTE             
         SH    R1,=H'10'                          SUBTRACT     X'0A'            
         STC   R1,LISTNME2+1                      SAVE   BYTE                   
*                                                                               
CRR455   DS    0H                                 TO     YEAR   ....            
         CLI   LISTNME2+1+L'ACQDTSTR,C'9'         YEAR   > 1999 ?               
         BNH   CRR460                             NO,    YEAR  OKAY             
         ZIC   R1,LISTNME2+1+L'ACQDTSTR           GET    H.O.  BYTE             
         SH    R1,=H'10'                          SUBTRACT     X'0A'            
         STC   R1,LISTNME2+1+L'ACQDTSTR           SAVE   BYTE                   
*                                                                               
CRR460   DS    0H                                 CONTINUE                      
         B     CRR900                                                           
*                                                                               
CRR500   MVC   ACQPROG+L'REQHDR(2),=C'99'   CANCEL REQUEST                      
         NI    LISTDATH+1,TURNOFF-X'08'     NORMAL INTENSITY                    
         GOTO1 TEXTGET,APPARM,1601,(L'LISTNME,LISTNME),0                        
*                                                                               
CRR900   GOTO1 VDMGR,APPARM,(X'20',DMWRT),REQUEST1,ADDR,AIOAREA3,DWORK          
         CLI   APPARM+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    TWALSCTL,TURNOFF-TWALSHLD                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     CRR999X                                                          
*                                                                               
CRR999   MVC   FVMSGNO,=AL2(ACEIVRQN)       INVALID REQUEST NUMBER              
*                                                                               
CRR999X  XIT1                                                                   
*                                                                               
         DROP  R1,R2,R3,R4,R5,R6,R7                                             
*                                                                               
REQUEST1 DC    CL8'ACCREQ'                                                      
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
       ++INCLUDE ACSCROTYP                                                      
         EJECT ,                                                                
       ++INCLUDE ACSCRRCLS                                                      
         EJECT                                                                  
                                                                                
*              DSECT FOR LOCAL WORKING STORAGE                                  
LWSD     DSECT                                                                  
*&&UK                                                                           
REQBYTE  DS    XL1                 PIN OR PID REQUEST?                          
REQSEC   DS    XL5                 REQUEST SECURITY                             
*&&                                                                             
DUB      DS    D                                                                
DWORK    DS    12D                                                              
*                                                                               
ALOADER  DS    A                                                                
AAPGIO   DS    A                                                                
ADDR     DS    F                                                                
SVR1     DS    A                   SAVE AREA FOR R1                             
SVRE     DS    A                   SAVE AREA FOR RE                             
TMPPARM  DS    8F                                                               
*&&UK                                                                           
PERSON   DS    CL8                 PERSON                                       
REQPID   DS    XL2                 BINARY PID                                   
PINNUM   DS    CL4                 PIN NUMBER                                   
*&&                                                                             
DMRDIR   DS    CL8                                                              
DMWRT    DS    CL8                                                              
ERRORMSG DS    CL60                                                             
SELKEY   DS    CL(L'RESKEY)                                                     
SELOWN   DS    CL8                 OWNER                                        
TEMPOWN  DS    CL8                 TEMPORAY STORAGE FOR OWNER                   
KEYSUB   DS    XL1                 SUB TYPE OF KEY                              
MULTLDGR DS    XL1                 YES/NO (MULTILEDGER REQUEST)                 
RCAP#    DS    AL1                 HIGHEST RECAP REPORT    NUMBER               
SVRCAP#  DS    AL1                 SAVE    HIGHEST    RCAP RPT  NUMBER          
*                                                                               
RCAPSW   DS    XL1                 RECAP   SWITCH                               
RCAPFUMP EQU   X'80'               . FOUND USE   MAIN'S    PROFILE              
RCAPFURP EQU   X'40'               . FOUND USE   RECAP'S   PROFILE              
*                                                                               
RFPFLAG  DS    XL1                 RFP     INDICATOR                            
RFPFON   EQU   X'80'               .       RFP   IS   ON                        
*                                                                               
SOONIND  DS    XL1                 SOON    INDICATOR                            
SOONIUL  EQU   X'80'               .       VALID UNIT/LEDGER                    
*                                                                               
RUNJOB   DS    CL1                 RUN JOB RATE                                 
RUNLONG  EQU   C'L'                .   LONG   RUNNING                           
RUNMED   EQU   C'M'                .   MEDIUM RUNNING                           
RUNSHORT EQU   C'S'                .   SHORT  RUNNING                           
*                                                                               
EXTFLD   DS    XL1                                                              
CONCODE  DS    XL1                 CONDITION CODE (SUBR INTERNAL WORK)          
*                                                                               
MOASTR   DS    CL4                 YYMM      MOA START                          
MOAEND   DS    CL4                 YYMM      MOA END                            
STRDTE   DS    CL6                 YYMMDD    START DATE                         
ENDDTE   DS    CL6                 YYMMDD    END DATE                           
RQDATE   DS    XL3                 YYMMDD    LAST REQUESTED DATE                
SVNAME   DS    CL12                                                             
*                                                                               
XTRA_MSG DS    CL15                                                             
OPTNME   DS    CL15                OPTION NAME TO DISPLAY                       
OPTIDNUM DS    AL1                                                              
OPTEQU   DS    AL1                 TYPE EQUATE                                  
TEMPACC  DS    CL12                TEMPORARY STORAGE FOR ACCOUNT                
TEMPLIST DS    CL6                 TEMPORARY STORAGE FOR BILLING LIST           
*                                                                               
OFFFLDH  DS    CL8                 FAKE OFFICE FIELD HEADER                     
OFFFLD   DS    CL2                 FAKE OFFICE FIELD                            
*                                                                               
FAKEFLDH DS    CL8                 FAKE FIELD  HEADER                           
FAKEFLD  DS    CL15                FAKE FIELD                                   
*                                                                               
FAKEDETH DS    CL8                 FAKE DETAIL FIELD HEADER                     
FAKEDET  DS    CL40                FAKE DETAIL FIELD                            
*                                                                               
DUMTRNTH DS    CL8                 FAKE TRANSACTION  HEADER                     
DUMTRNT  DS    CL4                 FAKE TRANSACTION  FIELD                      
*                                                                               
PRTSTYLE DS    CL1                 PORTRAIT/LANDSCAPE                           
CUROFF   DS    CL2                                                              
*                                                                               
TYPEC5   DS    XL1                 X'C5' ELEMENT TYPE                           
*                                                                               
BLOCK    DS    20CL32              DATA BLOCK FOR SCANNER                       
*                                                                               
REQREC   DS    0CL(80*9)           REQUEST RECORD                               
REQHDR   DS    0CL80               REQUEST HEADER RECORD                        
REQRECN  DS    CL54                REQUEST EXTENDED HEADER                      
REQHEAD  DS    0CL26               REQUEST HEAD                                 
       ++INCLUDE DMREQHDR                                                       
*&&UK                                                                           
         ORG   REQDATE                                                          
RQHFLG1  DS    XL1                 DEFINES TYPE OF DATA IN NEXT FIELD           
RQHFPIN  EQU   2                   FIELD CONTAINS A PIN                         
RQHFPID  EQU   3                   FIELD CONTAINS A PID                         
RQHSECD  DS    XL5                 SECURITY DATA DEFINED BY RQHFLG1             
         ORG                                                                    
         SPACE 3                                                                
*&&                                                                             
REQCARDS DS    0C                                                               
REQCARD1 DS    CL80                                                             
REQCARD2 DS    CL80                                                             
REQCARD3 DS    CL80                                                             
REQCARD4 DS    CL80                                                             
REQCARD5 DS    CL80                                                             
REQCARD6 DS    CL80                                                             
REQCARD7 DS    CL80                                                             
REQCARD8 DS    CL80                                                             
*                                                                               
MAXLDG   EQU   10                  MAXIMUM NUMBER OF LEDGERS                    
LDGUSED  DS    XL1                 NUMBER OF LEDGERS SPECIFIED                  
LDGSAVE  DS    (MAXLDG)CL(LUNLG)   SAVE AREA FOR LEDGERS SPECIFIED              
LDGSAVEL EQU   *-LDGSAVE           LENGTH OF LDGSAVE AREA                       
*                                                                               
WORK     DS    CL20                                                             
SOFBLOCK DS    XL(SOFDATL)                                                      
*                                                                               
LWSX     DS    0C                                                               
         EJECT ,                                                                
SCRD     DSECT                                                                  
SCRTYPE  DS    CL1                 REPORT TYPE                                  
SCRUL    DS    CL2                 UNIT/LEDGER                                  
         DS    CL1                 N/D                                          
SCRROUT  DS    AL1                 VALIDATION BRANCH ROUTINE #                  
SCRFLAG  DS    AL1                 INDICATORS                                   
SCRLIST  EQU   X'01'                  VALIDATION FOR LIST                       
SCRNOUL  EQU   X'02'                  NO UNIT LEDGER USED MUST SUPPLY           
SCRNOVAL EQU   X'04'                  DON'T VALIDATE                            
SCRID#   DS    AL1                 ID # FOR ELEMENT                             
         DS    AL1                 N/D                                          
SCRTEXT  DS    A                   SCREEN FIELD FOR TEXT                        
SCRFLD   DS    A                   ADDRESS OF FIELD TO VALIDATE                 
SCRFLDNM DS    A                   ADDRESS OF FIELD TO NAME                     
SCRTEXT# DS    A                   DISCRIPTION TEXT NUMBER FOR GETTEXT          
SCRPROF# DS    A                   PROFILE TEXT NUMBER                          
SCRLNQ   EQU   *-SCRD                                                           
         EJECT ,                                                                
LISTD    DSECT                                                                  
LISTACTH DS    CL8                 HEADER FOR NUMBER                            
LISTACT  DS    CL3                 SELECTION FIELD                              
LISTDATH DS    CL8                 HEADER FOR DATA                              
LISTDAT  DS    CL73                DATA                                         
         ORG   LISTDAT                                                          
         DS    CL1                                                              
LISTTYP  DS    CL4                 TYPE                                         
         DS    CL2                                                              
LISTFMT  DS    CL8                 FORMAT                                       
         DS    CL2                                                              
LISTNME  DS    CL36                NAME                                         
         ORG   LISTNME                                                          
LISTNME1 DS    CL20                                                             
LISTNME2 DS    CL16                                                             
         DS    CL2                                                              
LISTOWN  DS    CL8                 OWNER                                        
         DS    CL2                                                              
LISTADTE DS    CL8                 ACTIVITY DATE                                
         ORG                                                                    
LISTLNQ  EQU   *-LISTD                                                          
         EJECT ,                                                                
FL2LISTD DSECT                                                                  
FL2RFLT# DS    AL1                 RFLTYPE      NUMBER                          
FL2ACQLN DS    AL1                 LENGTH       OF   ENTRY IN ACQD              
FL2ACQDS DS    AL2                 DISPLACEMENT OF   ENTRY IN ACQD              
FL2LNQ   EQU   *-FL2LISTD          LENGTH       OF   ENTRY IN TABLE             
         EJECT ,                                                                
       ++INCLUDE ACSCROTYPD                                                     
         EJECT ,                                                                
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*DDSOFDATD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSOFDATD                                                      
         PRINT ON                                                               
*ACAPGEQU                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACAPGEQU                                                       
         PRINT ON                                                               
*ACSCRWRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
*                                                                               
*              DSECT FOR HEADLINE DEFINITIONS                                   
*                                                                               
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRF5D                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117ACSCR07   09/02/15'                                      
         END                                                                    
