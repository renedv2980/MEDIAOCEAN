*          DATA SET FATABSCLR  AT LEVEL 041 AS OF 05/22/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE TABSCLRA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE BINSR31                                                                
*INCLUDE SCANNER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE ARREDIT                                                                
*INCLUDE LOADER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'FATABSCLR - CLEARS DATA SPACE TABLES'                           
         PRINT NOGEN                                                            
TABSCLR  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKL,*TABSCLR,=A(WORKAREA),RA,R9                                
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         USING DMSPACED,DSPHD                                                   
*                                                                               
         BRAS  RE,INIT             INITIALISE                                   
*&&US*&& BRAS  RE,DEMOS            CLEAR AND REBUILD DEMO TABLES                
*&&US*&& BRAS  RE,CDARE            CLEAR DARE TABLES                            
         BRAS  RE,CJOBS            CLEAR JOB TABLE                              
         B     XBASE                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
*                                                                               
         LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         LA    R1,FULL                                                          
         L     R2,0(R1)                                                         
         LOCASCB ASID=(R2)         GET ASCB ADDRESS INTO R1                     
         L     R2,ASCBASSB-ASCB(R1) R2 = A(ASSB)                                
         SAM31                     SWITCH TO 31-BIT MODE                        
         L     R1,ASSBJSAB-ASSB(R2) R1 = A(JSAB)                                
         MVC   MAILJOBN,JSABJBID-JSAB(R1)  JOBID (E.G., JOB12345)               
         MVC   MAILJBN2,JSABJBID-JSAB(R1)  JOBID (E.G., JOB12345)               
         SAM24                     SWITCH BACK TO 24-BIT MODE                   
         LA    R2,FULL                                                          
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,FULL                                                          
         MVC   MAILJOB#,0(R2)      JOBNAME                                      
         MVC   MAILJB#2,0(R2)                                                   
*                                                                               
         MVC   IOH,=CL8'IO1*IO1*'                                               
         MVC   IO2H,=CL8'IO2*IO2*'                                              
         MVC   PTITLE,SPACES                                                    
         MVC   TITLED(L'CTITLE),CTITLE   SET UP TITLE                           
         BRAS  RE,PRINTI           INIT PRINTING                                
*                                                                               
         MVC   PLINE,SPACES                                                     
         MVC   PLINED(L'CTITLEU),CTITLEU SET UP TITLE UNDERLINE                 
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LHI   R1,1                BEGINNING INPUT CARD VALIDATION              
         BRAS  RE,DOMSG                                                         
*                                                                               
         LA    R2,CARD                                                          
INIT02   GOTO1 VCARDS,DMCB,(R2),=C'RE00'                                        
         CLC   =C'/*',0(R2)        END OF CARDS?                                
         BE    INIT04              YES                                          
*                                                                               
         MVC   PLINE+20(L'CARD),CARD                                            
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R2               VALIDATE KEYWORD=VALUE                       
         BAS   RE,CARDVAL                                                       
         BE    INIT02                                                           
*                                                                               
         LHI   R1,3                CARD VALIDATION ERROR                        
         BRAS  RE,DOMSG                                                         
         MVC   RETCODE,=F'8'       SET CC=8                                     
         B     XBASE                                                            
*                                                                               
INIT04   LHI   R1,2                COMPLETED INPUT CARD VALIDATION              
         BRAS  RE,DOMSG                                                         
         BRAS  RE,GETSPC           GET ADDRESS OF TABS DSPACE                   
*                                                                               
         LHI   R1,8                BEGAN OPENING FILES                          
         BRAS  RE,DOMSG                                                         
         GOTO1 VDMGR,DMCB,DMOPEN,DMSYS,DMFLIST                                  
         LHI   R1,9                ENDED OPENING FILES                          
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* CLEAR AND REBUILD DEMO TABLES                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMOS    NTR1  ,                                                                
         LHI   R1,10               BEGAN LOADING DEMADDR/DEMDISP                
         BRAS  RE,DOMSG                                                         
*                                                                               
         GOTO1 VLOADER,DMCB,DEMADDR,0,0                                         
         OC    DMCB+4(4),DMCB+4    (POSSIBLE) TEST T00ADE PHASE FOUND?          
         BNZ   DEMOS10             YES                                          
         MVC   MAILMS2P,DEMADDR                                                 
         GOTO1 VDMGR,DMCB,=C'OPMSG',('MAILMS2Q',MAILMSG2)                       
         GOTO1 VLOADER,DMCB,=CL8'T00ADE',0,0                                    
         OC    DMCB+4(4),DMCB+4    LIVE PHASE FOUND?                            
         JZ    *+2                 NO ?!?                                       
*                                                                               
DEMOS10  DS    0H                                                               
         MVC   VDEMADDR,4(R1)                                                   
         GOTO1 VLOADER,DMCB,DEMDISP,0,0                                         
         OC    DMCB+4(4),DMCB+4    (POSSIBLE) TEST T00AD0 PHASE FOUND?          
         BNZ   DEMOS20             YES                                          
         MVC   MAILMS2P,DEMDISP                                                 
         GOTO1 VDMGR,DMCB,=C'OPMSG',('MAILMS2Q',MAILMSG2)                       
         GOTO1 VLOADER,DMCB,=CL8'T00AD0',0,0                                    
         OC    DMCB+4(4),DMCB+4    LIVE PHASE FOUND?                            
         JZ    *+2                 NO ?!?                                       
*                                                                               
DEMOS20  DS    0H                                                               
         MVC   VDEMDISP,4(R1)                                                   
         GOTO1 VLOADER,DMCB,DEMTABOF,0,0                                        
         OC    DMCB+4(4),DMCB+4    (POSSIBLE) TEST T00AD2 PHASE FOUND?          
         BNZ   DEMOS30             YES                                          
         MVC   MAILMS2P,DEMTABOF                                                
         GOTO1 VDMGR,DMCB,=C'OPMSG',('MAILMS2Q',MAILMSG2)                       
         GOTO1 VLOADER,DMCB,=CL8'T00AD2',0,0                                    
         OC    DMCB+4(4),DMCB+4    LIVE PHASE FOUND?                            
         JZ    *+2                 NO ?!?                                       
*                                                                               
DEMOS30  DS    0H                                                               
         MVC   VDEMTBOF,4(R1)                                                   
*                                                                               
         LHI   R1,11               ENDED LOADING DEMADDR/DEMDISP                
         BRAS  RE,DOMSG                                                         
*                                                                               
         LHI   R1,12               BEGAN REBUILD OF DEMO TABLES                 
         BRAS  RE,DOMSG                                                         
         BRAS  RE,PRINTL           PRINT CLEAR LINE UNDERNEATH                  
*                                                                               
         BRAS  RE,DEMTABS          CLEAR & REBUILD DEMO TABLES                  
*                                                                               
         LHI   R1,13               ENDED REBUILD OF DEMO TABLES                 
         BRAS  RE,DOMSG                                                         
         BRAS  RE,PRINTL           PRINT CLEAR LINE UNDERNEATH                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DEMTABS - CLEAR AND REBUILD DEMO TABLES                             *         
***********************************************************************         
         SPACE 1                                                                
DEMTABS  NTR1  ,                                                                
         MVI   OKFREE,C'Y'                                                      
         LA    R3,DEMTABLE         CLEAR ALL DEMO TABLES                        
         USING DEMTABD,R3                                                       
*                                                                               
DEMT02   CLI   0(R3),EOT                                                        
         BE    DEMT10                                                           
*                                                                               
         CLI   DCLEAR,C'Y'         CLEAR TABLE?                                 
         BNE   DEMT03              NO                                           
         TIME  DEC                                                              
         ST    R0,TIME1            SAVE TIME OF START OF THIS CLEAR             
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),DLOCKID                                                   
         GOTO1 VLOCKSPC,DUB        LOCK CORRECT TABLE                           
         ICM   RF,15,4(R1)                                                      
         BZ    DRERR                                                            
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
*                                                                               
         BRAS  RE,ON31                                                          
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DSPTFRST                                                   
         SAC   512                                                              
         XC    0(16,R2),0(R2)       CLEAR 16 BYTE TABLE HEADER                  
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         BRAS  RE,OFF31                                                         
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),DLOCKID                                                   
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB        UNLOCK TABLE                                 
*                                                                               
         TIME  DEC                                                              
         ST    R0,TIME2            SAVE TIME TABLE CLEARED                      
*                                                                               
         XC    BLDNUM,BLDNUM       NOW RE-BUILD THE TABLE                       
         MVC   BLDNUM(1),DTABNUM                                                
         MVI   BLDNUM+4,X'FF'                                                   
         GOTO1 VDEMADDR,DMCB,(X'FF',BLDNUM),COMFACS,,C'$BLD'                    
*                                                                               
DEMT03   CLI   DREP,C'Y'           REPORT ON SIZE REQUIRED?                     
         BNE   DEMT05              No: Get next table                           
*                                                                               
         CLI   DCLEAR,C'Y'         Did we just clear the table?                 
         BE    DEMT04              Yes: Go report on size                       
         TIME  DEC                 No: Need to get some information             
         ST    R0,TIME1                                                         
         XC    DUB,DUB                                                          
         MVC   DUB(4),DLOCKID                                                   
         GOTO1 VLOCKSPC,DUB        LOCK CORRECT TABLE                           
         ICM   RF,15,4(R1)                                                      
         BZ    DRERR                                                            
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
         XC    DUB,DUB                                                          
         MVC   DUB(4),DLOCKID                                                   
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB        UNLOCK TABLE                                 
*                                                                               
DEMT04   SAM31                                                                  
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DSPTFRST                                                   
         SAC   512                                                              
         MVC   SZNOW,08(R2)        GET CURRENT TABLE SIZE                       
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         BRAS  RE,OFF31                                                         
*                                                                               
         TIME  DEC                                                              
         ST    R0,TIME3            SAVE TIME TABLE REBUILD                      
         MVC   TABLE,DEMNAME                                                    
         MVC   CLEARD,DCLEAR                                                    
         BRAS  RE,DOTIME           OUTPUT TIMING INFORMATION                    
*                                                                               
DEMT05   AHI   R3,DEMTABLN                                                      
         B     DEMT02                                                           
*                                                                               
DEMT10   CLI   OKFREE,C'Y'                                                      
         BE    EXITOK                                                           
         CLI   OKFREE,C'F'         FATAL ERROR                                  
         BE    DRERR                                                            
         BRAS  RE,NOTEOUTD                                                      
         B     EXITOK                                                           
*                                                                               
DRERR    XC    DUB,DUB                                                          
         MVC   DUB(4),DLOCKID                                                   
         GOTO1 VLOCKSPC,DUB        LOCK CORRECT TABLE                           
         MVC   TBLID,SPACES                                                     
         MVC   TBLID(8),TABLE                                                   
         B     FATALITY                                                         
         EJECT                                                                  
***********************************************************************         
* DEMO TABLES TO BE CLEARED                                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'****DEMTABLE****'                                           
*                                                                               
DEMTABLE DC    CL8'DBOOK   ',AL4(DTDBOOK),X'D1',C'YY'                           
         DC    CL8'DSTATION',AL4(DTDSTN),X'D2',C'YY'                            
         DC    CL8'DMASTER ',AL4(DTDMSTR),X'D3',C'YY'                           
         DC    CL8'DNAME   ',AL4(DTDNAME),X'D5',C'YY'                           
         DC    CL8'DCODE   ',AL4(DTDCODE),X'D6',C'YY'                           
         DC    CL8'DCONTROL',AL4(DTDCNTRL),X'D7',C'YY'                          
         DC    CL8'DADJUST ',AL4(DTDADJST),X'D8',C'YY'                          
         DC    CL8'DFMTAB  ',AL4(DTDFMTAB),X'E2',C'YN'                          
         DC    CL8'DALPHMKT',AL4(DTDAMKT),X'E3',C'YY'                           
         DC    CL8'DNADUNV ',AL4(DTDNUNV),X'E4',C'YY'                           
         DC    CL8'DFORMULA',AL4(DTDFRMLA),X'D4',C'NN'                          
         DC    CL8'DFUSION ',AL4(DTFUSN),X'E5',C'YY'                            
         DC    CL8'DMKNAME ',AL4(DTDMKTN),X'E6',C'YY'                           
         DC    CL8'DNFORMS ',AL4(DTDNFOR),X'E7',C'YY'                           
         DC    CL8'DRENTRK ',AL4(DTDRNTRK),X'E8',C'NY'                          
         DC    CL8'DRENTNET',AL4(DTDRNTNE),X'E9',C'NY'                          
         DC    CL8'DCOMSNAT',AL4(DTDCSNAT),X'EA',C'NY'                          
         DC    CL8'DCOMSLON',AL4(DTDCSLON),X'EB',C'NY'                          
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DEMO TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
DEMTABD  DSECT                                                                  
DEMNAME  DS    CL8                 NAME OF TABLE BEING CLEARED                  
DLOCKID  DS    AL4                 LOCKSPACE ID                                 
DTABNUM  DS    XL1                 TABLE NUMBER                                 
DCLEAR   DS    XL1                 TABLE IS TO BE CLEARED?                      
*                                   C'N' = DO NOTHING                           
*                                   C'Y' = COMPLETE TABLE REBUILD               
DREP     DS    XL1                 REPORT ON SIZE REQUIRED?                     
DEMTABLN EQU   *-DEMTABD                                                        
*                                                                               
TABSCLR  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CLRDARE - CLEAR DARE TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
CDARE    NTR1  ,                                                                
         LHI   R1,14               BEGAN CLEARING DARE TABLES                   
         BRAS  RE,DOMSG                                                         
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDARE)                                              
         GOTO1 VLOCKSPC,DUB        LOCK CORRECT TABLE                           
         ICM   RF,15,4(R1)                                                      
         BZ    DAERR                                                            
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
*                                                                               
         BRAS  RE,ON31                                                          
*                                                                               
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DSPTFRST      A(DARE TABLE)                                
         USING TABDARED,R2                                                      
         SAC   512                                                              
         ICM   RF,15,TBDMAX        MAX # SORTED RECDS                           
         MHI   RF,TBDDARL                                                       
         CPYA  ARE,AR2                                                          
         LA    RE,TBDSORT                                                       
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0               CLEAR ALL SORTED RECORDS                     
         JNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
         LAM   ARE,ARE,ARZERO                                                   
*                                                                               
         USING BSPARA,TBDBSP   *** RESET PARAMETERS FOR BINSRCH                 
         XC    BSPAREC,BSPAREC                                                  
         XC    BSPNOR,BSPNOR                                                    
*                                                                               
         BRAS  RE,OFF31                                                         
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDARE)                                              
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB        UNLOCK CORRECT TABLE                         
*                                                                               
         LHI   R1,15               ENDED CLEARING DARE TABLE                    
         BRAS  RE,DOMSG                                                         
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTASS)                                               
         GOTO1 VLOCKSPC,DUB        LOCK CORRECT TABLE                           
         ICM   RF,15,4(R1)                                                      
         BZ    DAERR                                                            
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
*                                                                               
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DSPTFRST      R2=A(DARE ASSIST TABLE)                      
         ICM   R3,15,DSPTEND                                                    
         AHI   R3,1                                                             
         SR    R3,R2               R3=L'DARE ASSIST TABLE                       
         XR    RF,RF                                                            
*                                                                               
         BRAS  RE,ON31                                                          
         SAC   512                                                              
         MVCL  R2,RE               CLEAR WHOLE TABLE                            
         JNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         BRAS  RE,OFF31                                                         
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTASS)                                               
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB        UNLOCK CORRECT TABLE                         
*                                                                               
         LHI   R1,16               ENDED CLEARING DARE ASSIST TABLE             
         BRAS  RE,DOMSG                                                         
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
*                                                                               
DAERR    SAC   0                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDARE)                                              
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB                                                     
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTASS)                                               
         MVI   DUB,X'10'                                                        
         GOTO1 (RF),(R1)           CALL LOCKSPC                                 
         MVC   TBLID,=CL16'DARE ASSIST'                                         
         B     FATALITY                                                         
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* CJOBS - CLEAR JOB TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
CJOBS    NTR1  ,                                                                
         LHI   R1,21               BEGAN CLEARING JOB TABLE                     
         BRAS  RE,DOMSG                                                         
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         GOTO1 VLOCKSPC,DUB        LOCK CORRECT TABLE                           
         ICM   RF,15,4(R1)                                                      
         BZ    JOERR                                                            
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
*                                                                               
         BRAS  RE,ON31                                                          
*                                                                               
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DSPTFRST      A(JOB TABLE)                                 
         ICM   RF,15,DSPTEND                                                    
         SR    RF,R2                                                            
         LA    RF,1(RF)            LEN JOB TABLE                                
         SAC   512                                                              
         CPYA  ARE,AR2                                                          
         LR    RE,R2                                                            
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0               CLEAR WHOLE JOBTAB                           
         JNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
         LAM   ARE,ARE,ARZERO                                                   
*                                                                               
         L     R2,DSPTFRST         GO TO START OF TABLE                         
         MVC   4(2,R2),=Y(L'TBJOBHDR)  SET HEADER LENGTH                        
         MVC   6(2,R2),=Y(L'TBJNTRY)   SET ENTRY LENGTH                         
*                                                                               
         BRAS  RE,OFF31                                                         
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB        UNLOCK JOB TABLE                             
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDCLASS)                                            
         GOTO1 VLOCKSPC,DUB        LOCK CLASS TABLE                             
         ICM   RF,15,4(R1)                                                      
         BZ    JOERR                                                            
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
         ICM   R0,15,DSPTFRST                                                   
         BZ    CJ02                OLD DATASPACE                                
*                                                                               
         BRAS  RE,ON31                                                          
*                                                                               
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DSPTFRST      A(JOB TABLE)                                 
         ICM   RF,15,DSPTEND                                                    
         SR    RF,R2                                                            
         LA    RF,1(RF)            LEN JOB TABLE                                
         SAC   512                                                              
         CPYA  ARE,AR2                                                          
         LR    RE,R2                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0               CLEAR WHOLE CLASS TABLE                      
         JNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
         LAM   ARE,ARE,ARZERO                                                   
*                                                                               
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         BRAS  RE,OFF31                                                         
*                                                                               
CJ02     XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDCLASS)                                            
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB        UNLOCK CLASS TABLE                           
*                                                                               
         LHI   R1,22               ENDED CLEARING JOB TABLE                     
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
*                                                                               
JOERR    SAC   0                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB                                                     
         B     FATALITY                                                         
         EJECT                                                                  
***********************************************************************         
* TIMING AND SIZE MESSAGE OUTPUT                                      *         
* NTRY: TABLE   NAME OF TABLE OR ZEROS (USE UPRMSG)                   *         
*       CLEARD  WAS TABLE CLEARED OR NOT (DON'T SHOW CLEAR/BUILT TIME)*         
*       TIME1   START TIME                                            *         
*       TIME2   CLEAR TIME                                            *         
*       TIME3   BUILT TIME                                            *         
*       SZNOW   CURRENT TABLE SIZE                                    *         
*       DSPHD   DATASPACE INFORMATION                                 *         
*                                                                     *         
* FORMAT OF MESSAGE IS AS SHOWN                                       *         
* LINE  INFORMATION                                                   *         
* 1     CLRTABS DEMO TABLE XXXXXXXX TIMING/SIZE INFORMATION           *         
* 2     ROUTINE STARTED AT     HH:MM:SS:XX                            *         
* 3     TABLE CLEARED AT       HH:MM:SS:XX                            *         
* 4     TABLE REBUILT AT       HH:MM:SS:XX                            *         
* 5     TABLE SIZE AT ALLOCATION  XXXXXXXX BYTES                      *         
* 6     AMOUNT OF TABLE IN USE    XXXXXXXX BYTES                      *         
* 7     PERCENTAGE OF TABLE FREE     XX.XX %                          *         
***********************************************************************         
DOTIME   NTR1  ,                                                                
         MVC   DMSGT,DEMMSG                                                     
         MVC   DMSGN,TABLE                                                      
         CLI   TABLEID,C'U'                                                     
         BNE   *+10                                                             
         MVC   DMSGT,UPRMSG                                                     
         CLI   TABLEID,C'I'                                                     
         BNE   *+10                                                             
         MVC   DMSGT,IDRMSG                                                     
         CLI   TABLEID,C'5'                                                     
         BNE   *+10                                                             
         MVC   DMSGT,SARMSG                                                     
         CLI   TABLEID,C'0'                                                     
         BNE   *+10                                                             
         MVC   DMSGT,OARMSG                                                     
         CLI   TABLEID,C'F'                                                     
         BNE   *+10                                                             
         MVC   DMSGT,FARMSG                                                     
*                                                                               
         L     R0,TIME1            START TIME                                   
         BRAS  RE,TIMER                                                         
         MVC   DMSGT1,TIME                                                      
         L     R0,TIME2            CLEAR TIME                                   
         BRAS  RE,TIMER                                                         
         MVC   DMSGT2,TIME                                                      
         L     R0,TIME3            REBUILT TIME                                 
         BRAS  RE,TIMER                                                         
         MVC   DMSGT3,TIME                                                      
*                                                                               
         CLI   CLEARD,C'Y'         WAS TABLE ACTUALLY CLEARED?                  
         BE    DOTM01              YES                                          
         MVC   DMSGT2,NOCMSG       NO: Say so in the report                     
         MVC   DMSGT3,NOBMSG                                                    
*                                                                               
DOTM01   ICM   R0,15,DSPTEND       MAX SIZE OF TABLE                            
         AHI   R0,1                                                             
         ICM   RF,15,DSPTFRST                                                   
         N     RF,=XL4'0FFFFFFF'                                                
         SR    R0,RF               R0 = MAX SIZE                                
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DMSGMAX,DUB                                                      
*                                                                               
         ICM   RF,15,SZNOW         CURRENT SIZE OF TABLE                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DMSGNOW,DUB                                                      
*                                                                               
         MVC   DMSGPCT,=CL3'???'                                                
         LR    RE,R0               (MAX-NOW)/MAX*100                            
         SR    RE,RF                                                            
         BP    *+12                                                             
         MVI   OKFREE,C'F'         TABLE HAS OVERFLOWED                         
         B     DOTM06                                                           
*                                                                               
         SRDL  RE,32                                                            
         M     RE,=F'100'                                                       
         DR    RE,R0                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)            ROUND UP                                     
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DMSGPCT,DUB                                                      
         B     DOTM02                                                           
*                                                                               
DOTM02   OC    TABLE,TABLE         DEMO TABLE?                                  
         BZ    DOTM04              NO                                           
         CHI   RF,15               MORE THAN 15% STILL UNUSED?                  
         BH    *+8                 YES                                          
         MVI   OKFREE,C'N'         DEMO TABLE NEEDS CHANGING                    
         B     DOTM06                                                           
*                                                                               
DOTM04   CHI   RF,10               MORE THAN 10% STILL UNUSED?                  
         BH    DOTM06              YES                                          
         LR    RE,R0                                                            
         ICM   RF,15,SZNOW         CURRENT SIZE OF TABLE                        
         SR    RE,RF               RE = AMOUNT REMAINING                        
         C     RE,=A(128*1024)     IF MORE THAN 128K THEN IGNORE                
         BH    DOTM06                                                           
         MVI   OKFREE,C'N'         TABLE SIZE NEEDS CHANGING                    
*                                                                               
DOTM06   XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE(L'TIME),TIME                                               
         MVC   PLINE+L'TIME+1(L'DMSG1),DMSG1                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+1(L'DMSG2),DMSG2                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+1(L'DMSG3),DMSG3                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+1(L'DMSG4),DMSG4                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+1(L'DMSG5),DMSG5                                    
         BRAS  RE,PRINTL                                                        
*                                                                               
         BRAS  RE,PRINTL           PRINT CLEAR LINE UNDERNEATH                  
         B     EXITOK                                                           
*                                                                               
DEMMSG   DC    CL19'Demo table XXXXXXXX'                                        
UPRMSG   DC    CL19'User Profile table '                                        
IDRMSG   DC    CL19'ID Record table    '                                        
SARMSG   DC    CL19'System Auth Table  '                                        
OARMSG   DC    CL19'Personal Auth Table'                                        
FARMSG   DC    CL19'New Security Table'                                         
*                                                                               
NOCMSG   DC    CL11'Not Cleared'                                                
NOBMSG   DC    CL11'Not Rebuilt'                                                
*                                                                               
DMSG1    DC    CL60' '                                                          
         ORG   DMSG1                                                            
         DC    C'CLRTABS '                                                      
DMSGT    DS    0CL19                                                            
         DC    CL11' '                                                          
DMSGN    DC    CL08' '                                                          
         DC    C' Timing/Size Information'                                      
         ORG   DMSG1+L'DMSG1                                                    
*                                                                               
DMSG2    DC    CL60' '                                                          
         ORG   DMSG2                                                            
         DC    C'Routine started at '                                           
DMSGT1   DC    CL11' '                                                          
         DC    C'  '                                                            
         ORG   DMSG2+L'DMSG2                                                    
*                                                                               
DMSG3    DC    CL110' '                                                         
         ORG   DMSG3                                                            
         DC    C'Table cleared at   '                                           
DMSGT2   DC    CL11' '                                                          
         DC    C'     '                                                         
         DC    C'Table rebuilt at   '                                           
DMSGT3   DC    CL11' '                                                          
         ORG   DMSG3+L'DMSG3                                                    
*                                                                               
DMSG4    DC    CL110' '                                                         
         ORG   DMSG4                                                            
         DC    C'Allocated size     '                                           
DMSGMAX  DC    CL8' '                                                           
         DC    C' bytes  '                                                      
         DC    C'Amount in use      '                                           
DMSGNOW  DC    CL8' '                                                           
         DC    C' bytes'                                                        
         ORG   DMSG4+L'DMSG4                                                    
*                                                                               
DMSG5    DC    CL60' '                                                          
         ORG   DMSG5                                                            
         DC    C'Percentage free    '                                           
DMSGPCT  DC    CL3' '                                                           
         DC    C'%'                                                             
         ORG   DMSG5+L'DMSG5                                                    
         EJECT                                                                  
***********************************************************************         
* OUTPUT TIME FROM MVS TIME MACRO                                     *         
* NTRY R0 = ZERO USE CURRENT TIME (FROM MVS TIME MACRO)               *         
*      R0 = NZ   USE TIME IN HHMMSSXX IN R0                           *         
* EXIT TIME HOLDS HH:MM:SS:XX WHERE XX IS 1/100 SECS                  *         
***********************************************************************         
         SPACE 1                                                                
TIMER    NTR1  ,                                                                
         LTR   R0,R0                                                            
         BNZ   TIME02                                                           
*                                                                               
         TIME  DEC                 R0=TIME                                      
*                                                                               
TIME02   STC   R0,TIME+10                                                       
         OI    TIME+10,X'F0'                                                    
         SRL   R0,4                                                             
         STC   R0,TIME+9           XX PORTION                                   
         OI    TIME+9,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+7                                                        
         OI    TIME+7,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+6           SS PORTION                                   
         OI    TIME+6,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+4                                                        
         OI    TIME+4,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+3           MM PORTION                                   
         OI    TIME+3,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+1                                                        
         OI    TIME+1,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+0           HH PORTION                                   
         OI    TIME+0,X'F0'                                                     
*                                                                               
         MVI   TIME+2,C':'                                                      
         MVI   TIME+5,C':'                                                      
         MVI   TIME+8,C':'                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATASPACE ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETSPC   NTR1  ,                                                                
         LHI   R1,4                                                             
         BRAS  RE,DOMSG                                                         
*                                                                               
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    GETSPC2                                                          
*                                                                               
         LHI   R1,6                                                             
         BRAS  RE,DOMSG                                                         
         ABEND 911,DUMP                                                         
*                                                                               
GETSPC2  MVC   DMOFFS,WORK+20      EXTRACT VALUES                               
         MVC   ALET,WORK+24                                                     
         MVC   DMTOKN,WORK+28                                                   
         OC    ALET,ALET                                                        
         BNZ   GETSPC4                                                          
*                                                                               
         LHI   R1,7                                                             
         BRAS  RE,DOMSG                                                         
         ABEND 911,DUMP                                                         
*                                  SET IT IN SSB ALSO                           
GETSPC4  MVC   SSB+SSODSPAC-SSOOFF(1),DSPACE+3                                  
         MVC   SSB+SSOTBLET-SSOOFF(4),ALET                                      
         MVC   SSOMASTC-SSOOFF+SSB,=A(MASTC)                                    
         LHI   R1,5                                                             
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SEND AN E-MAIL IF AVAILABLE DEMO TABLE SIZE IS BELOW THRESHOLD      *         
***********************************************************************         
         SPACE 1                                                                
NOTEOUTD NTR1  ,                                                                
*                                                                               
         WTO   'Demo size problem found - sending note'                         
*                                                                               
         GOTO1 VDMGR,DMCB,=C'OPMSG',('MAILMS1Q',MAILMSG1)                       
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
         SPACE 1                                                                
CARDVAL  NTR1  ,                                                                
         ST    RD,CARDRD                                                        
         LR    R2,R1               R2=A(CARD START)                             
         LA    R1,79(R2)                                                        
         ST    R1,CARDEND          SAVE A(LAST CHAR)                            
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 =V(SCAN31),DMCB,(R2),SCNBLK,0,(1,SCICARD),20                     
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         LA    R3,CARDTAB                                                       
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,*+8                                                           
         BE    CARDV03                                                          
         CLC   SC1STFLD(0),CNAME                                                
         LA    R3,CARDTABL(R3)                                                  
         B     CARDV02                                                          
*                                                                               
CARDV03  DS    0H                                                               
         CLC   CNAME,=C'DEMCLR  '  IS THIS A DEMCLR PARAMETER CARD?             
         BNE   CARDV06             NO                                           
*                                                                               
         MVI   BYTE,C'N'           YES: ASSUME TABLE NAME IS INVALID            
         LA    RF,DEMTABLE         TABLE OF DEMO TABLE NAMES                    
         USING DEMTABD,RF                                                       
CARDV04  DS    0H                                                               
         MVI   DCLEAR,C'N'         ASSUME WE'RE *NOT* CLEARING THIS ONE         
         CLC   DEMNAME,SC2NDFLD    MATCH ON TABLE NAME?                         
         BNE   *+12                                                             
         MVI   BYTE,C'Y'           YES: PARAMETER CARD IS VALID                 
         MVI   DCLEAR,C'Y'         THIS IS THE ONE TO CLEAR!                    
*                                                                               
         LA    RF,DEMTABLN(RF)     BUMP TO NEXT DEMO TABLE                      
         CLI   0(RF),EOT           ANY MORE DEMO TABLES?                        
         BNE   CARDV04             YES: LOOP BACK                               
*                                                                               
         CLI   BYTE,C'Y'           NO: DID WE MATCH ON ANY TABLE NAME?          
         BNE   CENOINP             NO: INVALID TABLE NAME GIVEN                 
         DROP  RF                                                               
*                                                                               
CARDV06  CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CARDV08             NO                                           
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
CARDV08  CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CARDV10             NO                                           
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
         EX    RF,*+8                                                           
         B     EXITOK                                                           
         MVC   0(0,RE),SC2NDFLD                                                 
*                                                                               
CARDV10  DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,=CL40'INVALID LINE FORMAT'                                    
         B     CERR                                                             
*                                                                               
CEINVKEY LA    R1,=CL40'INVALID KEYWORD'                                        
         B     CERR                                                             
*                                                                               
CENOTNUM LA    R1,=CL40'VALUE NOT A VALID NUMBER'                               
         B     CERR                                                             
*                                                                               
CENOTCHR LA    R1,=CL40'VALUE NOT A VALID CHARACTER STRING'                     
         B     CERR                                                             
*                                                                               
CETOOSHT LA    R1,=CL40'LENGTH OF INPUT STRING TOO SHORT'                       
         B     CERR                                                             
*                                                                               
CETOOLNG LA    R1,=CL40'LENGTH OF INPUT STRING TOO LONG'                        
         B     CERR                                                             
*                                                                               
CETOOLOW LA    R1,=CL40'NUMERIC VALUE TOO SMALL'                                
         B     CERR                                                             
*                                                                               
CETOOBIG LA    R1,=CL40'NUMERIC VALUE TOO LARGE'                                
         B     CERR                                                             
*                                                                               
CENOINP  LA    R1,=CL40'INVALID/MISSING VALUE'                                  
         B     CERR                                                             
*                                                                               
CERR     L     RD,CARDRD                                                        
         MVC   PLINE,SPACES                                                     
         MVC   PLINE(15),=CL15' *** ERROR ***'                                  
         MVC   PLINE+15(40),0(R1)                                               
         BAS   RE,PRINTL                                                        
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINTER                                                  *         
***********************************************************************         
         SPACE 1                                                                
PRINTI   NTR1  ,                                                                
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         PUT   SYSPRINT,PTITLE     PRINT TITLES                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRINTL   NTR1  ,                                                                
         PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXIT                EXIT                                         
         EJECT                                                                  
***********************************************************************         
* CLOSE PRINTER                                                       *         
***********************************************************************         
         SPACE 1                                                                
PRINTX   NTR1  ,                                                                
         CLOSE SYSPRINT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FATAL ERROR ON BUILD - HAVE TO PUT OUT WTOR AND ABEND               *         
***********************************************************************         
         SPACE 1                                                                
FATALITY SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         MVC   ERR1TB,TBLID                                                     
*                                                                               
         XR    R0,R0               MUST BE CLEAR FOR WTOR                       
         WTOR  TEXT=(ERR1L,RESPND,12,REPLECB)                                   
         WAIT  ECB=REPLECB                                                      
         ABEND 911,DUMP                                                         
*                                                                               
ERR1L    DC    AL2(65)                                                          
         DC    CL25'CLRTABS - Error building '                                  
ERR1TB   DC    CL16' '                                                          
         DC    CL25'Recycle tabs dataspace'                                     
*                                                                               
REPLECB  DC    F'0'                                                             
RESPND   DC    CL12' '                                                          
         EJECT                                                                  
***********************************************************************         
* OUTPUT INFORMATION MESSAGE                                          *         
* NTRY: R1  NZ      INDEX TO MESSAGE                                  *         
*       R1  ZERO    MESSAGE IS ALREADY ON PRINT LINE                  *         
***********************************************************************         
         SPACE 1                                                                
DOMSG    NTR1  ,                                                                
         LTR   R1,R1                                                            
         BZ    DOMSG02                                                          
*                                                                               
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE(L'TIME),TIME                                               
*                                                                               
         BCTR  R1,0                                                             
         MHI   R1,L'MESSTAB                                                     
         A     R1,AMESSTAB                                                      
         MVC   PLINE+L'TIME+1(L'MESSTAB),0(R1)                                  
*                                                                               
DOMSG02  BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EXITS AND OTHER USEFUL ROUTINES                                     *         
***********************************************************************         
         SPACE 1                                                                
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    BRAS  RE,PRINTX           CLOSE PRINTING AND EXIT TO MVS               
         L     RD,SAVERD                                                        
*                                                                               
         XBASE RC=RETCODE                                                       
*                                                                               
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'CARDTAB*CARDTAB*'                                           
         SPACE 1                                                                
CARDTAB  DC    CL8'RUN     ',F'001',F'0000010'                                  
         DC    AL1(02,CTCHR,L'RUN,0),AL4(RUN)                                   
         DC    CL8'DSPACE  ',F'001',F'0000012'                                  
         DC    AL1(05,CTCHR,L'DSPACE,0),AL4(DSPACE)                             
         DC    CL8'DDSIO   ',F'001',F'0000012'                                  
         DC    AL1(04,CTCHR,8,0),V(DDSIO)                                       
         DC    CL8'DEMADDR ',F'006',F'0000008'                                  
         DC    AL1(06,CTCHR,L'DEMADDR,0),A(DEMADDR)                             
         DC    CL8'DEMDISP ',F'006',F'0000008'                                  
         DC    AL1(06,CTCHR,L'DEMDISP,0),A(DEMDISP)                             
         DC    CL8'DEMTBOF ',F'006',F'0000008'                                  
         DC    AL1(06,CTCHR,L'DEMTABOF,0),A(DEMTABOF)                           
         DC    CL8'DEMCLR  ',F'001',F'0000008'                                  
         DC    AL1(05,CTCHR,L'DEMCLRNM,0),A(DEMCLRNM)                           
         DC    CL8'DEMFLAGS',F'008',F'0000008'                                  
         DC    AL1(07,CTCHR,L'MCFFPARM,0),A(MCFFPARM)                           
CARDTABX DC    AL1(CARDEOT)                                                     
         DS    0L                                                               
         DC    CL16'CARDTABXCARDTABX'                                           
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
TABSCLR  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
EOT      EQU   X'FF'                                                            
CTMAXLEN EQU   1024                                                             
CTMAXID  EQU   1000                                                             
IOL      EQU   2000                                                             
ARZERO   DC    16F'0'                                                           
RETCODE  DC    F'0'                                                             
*                                                                               
ISREAD   DC    F'1'                                                             
ISRDSEQ  DC    F'2'                                                             
*                                                                               
VHEXOUT  DC    V(HEXOUT)                                                        
VISDDS   DC    V(ISDDS)                                                         
VCARDS   DC    V(CARDS)                                                         
VARREDIT DC    V(ARREDIT)                                                       
VLOCKSPC DC    V(LOCKSPC)                                                       
VREMOTEC DC    V(REMOTEC)                                                       
VDMGR    DC    V(DATAMGR)                                                       
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VHELLO   DC    V(HELLO)                                                         
VLOADER  DC    V(LOADER)                                                        
*                                                                               
AMESSTAB DC    A(MESSTAB)                                                       
*                                                                               
DMOPEN   DC    CL8'DMOPEN'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DTFAD    DC    CL8'DTFAD '                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
*&&US                                                                           
DMSYS    DC    CL8'SPOT'                                                        
*&&                                                                             
*&&UK                                                                           
DMSYS    DC    CL8'CONTROL'                                                     
*&&                                                                             
*&&US                                                                           
DMFLIST  DS    0C                                                               
         DC    C'NDEMDIRN'                                                      
         DC    C'NL=DEMFN'                                                      
         DC    C'NDEMDIRA'                                                      
         DC    C'NL=DEMFA'                                                      
         DC    C'NDEMDIRR'                                                      
         DC    C'NL=DEMFR'                                                      
         DC    C'NNTIDIR '                                                      
         DC    C'NL=NTIFL'                                                      
         DC    C'NPAVDIR '                                                      
         DC    C'NL=PAVFL'                                                      
         DC    C'NCTFILE X'                                                     
*&&                                                                             
*&&UK                                                                           
DMFLIST  DC    C'NCTFILE X'                                                     
*&&                                                                             
*                                                                               
CTITLE   DC    C'CLRTABS Output Information'                                    
CTITLEU  DC    C'--------------------------'                                    
*                                                                               
MAILMSG1 DC    C'AUTONOTE*DEISN,WHOAN,BPOON,AHYDN:'                             
MAILJOBN DS    CL8                 MVS JOBNAME                                  
         DC    C'('                                                             
MAILJOB# DS    CL8                 MVS JOB NUMBER                               
         DC    C') '                                                            
         DC    CL50'WARNING: Demo tables approaching maximum size'              
MAILMS1Q EQU   *-MAILMSG1                                                       
*                                                                               
MAILMSG2 DC    C'AUTONOTE*DEISN,WHOAN,BPOON,AHYDN:'                             
MAILJBN2 DS    CL8                 MVS JOBNAME                                  
         DC    C'('                                                             
MAILJB#2 DS    CL8                 MVS JOB NUMBER                               
         DC    C') '                                                            
         DC    C'WARNING: Missing phase '                                       
MAILMS2P DS    CL8                                                              
         DC    C'. Live substituted.'                                           
MAILMS2Q EQU   *-MAILMSG2                                                       
*                                                                               
         EJECT                                                                  
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
         EJECT                                                                  
***********************************************************************         
* COMMON FACILITIES LIST FOR DEMO LOOK-UPS                                      
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'COMFACS*COMFACS*'                                           
COMFACS  DS    0D                                                               
         DC    V(DATAMGR)                                                       
         DC    23A(0)                                                           
VDEMADDR DC    A(0)                                                             
VDEMDISP DC    A(0)                                                             
         DC    A(0)                                                             
VDEMTBOF DC    A(0)                                                             
         DC    35A(0)                                                           
         DC    V(BINSRCH)                                                       
         DC    V(PROTON)                                                        
         DC    V(PROTOFF)                                                       
         DC    3A(0)                                                            
         DC    V(LOCKUP)                                                        
         DC    V(MASTC)            MASTC                                        
         DC    V(LOCKSPC)          LOCKSPACE                                    
         DC    8A(0)               SPARE                                        
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(132)          
*                                                                               
AOPERECB DC    A(0)                                                             
ACOMM    DC    A(0)                                                             
*                                                                               
RUN      DC    CL4'DSP'                                                         
TEST     DC    C'N'                                                             
DSPACE   DC    CL12' '                                                          
DEMADDR  DC    CL8'T00ADE'                                                      
DEMDISP  DC    CL8'T00AD0'                                                      
DEMTABOF DC    CL8'T00AD2'                                                      
DEMCLRNM DS    CL8                 NAME OF SPECIFIC DEMO TABLE TO BUILD         
         EJECT                                                                  
***********************************************************************         
* UTL AND SSB FOR DDSIO                                               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'UTL*UTL*UTL*UTL*'                                           
UTL      DC    F'0'                                                             
*&&US*&& DC    X'02'                                                            
*&&UK*&& DC    X'0A'                                                            
         DC    XL251'00'                                                        
*                                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      DC    H'0'                                                             
         DC    X'FF'                                                            
         DC    X'02'               SUPPRESS RECOVERY                            
         DC    1020X'00'                                                        
         EJECT                                                                  
***********************************************************************         
* MESSAGES                                                            *         
***********************************************************************         
MESSTAB  DS    0CL60                                                            
  DC CL60'Began processing input parameters from cards                '         
  DC CL60'Ended processing input parameters from cards                '         
  DC CL60'Card validation error - Application terminating             '         
  DC CL60'Attempting Dataspace binds                                  '         
  DC CL60'Completed  Dataspace binds                                  '         
  DC CL60'Unable to bind to TABS dataspace - Application terminating  '         
  DC CL60'No ALET for TABS dataspace- Application terminating         '         
  DC CL60'Began opening files                                         '         
  DC CL60'Ended opening files                                         '         
  DC CL60'Began loading current DEMADDR and DEMDISP                   '         
  DC CL60'Ended loading current DEMADDR and DEMDISP                   '         
  DC CL60'Began rebuild of DEMO tables                                '         
  DC CL60'Ended rebuild of DEMO tables                                '         
  DC CL60'Began clearing DARE tables                                  '         
  DC CL60'Ended clearing DARE table                                   '         
  DC CL60'Ended clearing DARE ASSIST table                            '         
  DC CL60'UNUSED learing User Profile table                           '         
  DC CL60'UNUSED learing User Profile table                           '         
  DC CL60'UNUSED learing ID record table                              '         
  DC CL60'UNUSED learing ID record table                              '         
  DC CL60'Began clearing Job table                                    '         
  DC CL60'Ended clearing Job table                                    '         
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DC                                                  *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'WORKAREAWORKAREA'                                           
WORKAREA DC    60000X'00'                                                       
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                              *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DSPHD    DS    XL64                                                             
*                                                                               
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDEND  DS    A                                                                
*                                                                               
FULL     DS    F                                                                
*                                                                               
ACTDTF   DS    A                                                                
*                                                                               
TIME1    DS    F                                                                
TIME2    DS    F                                                                
TIME3    DS    F                                                                
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
GETLEN   DS    F                   LENGTH RETURNED IF GETMAIN                   
ADSDATA  DS    A                   ADDRESS OF DS BLOCK                          
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
*                                                                               
WAITER   DS    A                                                                
*                                                                               
DMOFFS   DS    A                   DATASPACE OFFSET                             
ALET     DS    A                   ALET                                         
DMTOKN   DS    CL8                 TOKEN                                        
*                                                                               
SZNOW    DS    F                                                                
TABLE    DS    CL8                                                              
TBLID    DS    CL16                                                             
HALF     DS    H                                                                
BYTE     DS    X                                                                
OKFREE   DS    X                                                                
TABLEID  DS    X                                                                
CLEARD   DS    X                                                                
         DS    X                                                                
*                                                                               
TIME     DS    CL11                                                             
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
*                                                                               
PLINE    DS    0CL132                                                           
         DS    XL1                                                              
PLINED   DS    CL131                                                            
*                                                                               
PTITLE   DS    0CL132                                                           
         DS    XL1                                                              
TITLED   DS    CL131                                                            
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
IDKLIST  DS    XL(2+L'CTIKID)                                                   
*                                                                               
KEY      DS    XL25                                                             
BLDNUM   DS    CL5                 BUILD ONE TABLE AT A TIME                    
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
*                                                                               
IOH      DS    CL8                                                              
IO       DS    (IOL)C                                                           
IO2H     DS    CL8                                                              
IO2      DS    (IOL)C                                                           
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
         TITLE 'MASTER CONTROL - STORAGE AND DSECTS'                            
         EJECT                                                                  
***********************************************************************         
* MASTC CSECT                                                         *         
***********************************************************************         
         SPACE 1                                                                
MASTC    CSECT                                                                  
         PRINT  OFF                                                             
       ++INCLUDE DDMASTC                                                        
         PRINT  ON                                                              
*                                                                               
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         EJECT                                                                  
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* FATABSDMP                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDMP                                                      
         PRINT ON                                                               
* FATABSJOB                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSJOB                                                      
         PRINT ON                                                               
* FATABSZIP                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSZIP                                                      
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
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* DDASSISTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDASSISTD                                                      
         PRINT ON                                                               
* FADARETABD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDAR                                                      
         PRINT ON                                                               
* DDBSPARA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         PRINT ON                                                               
* DDREMOTED                                                                     
*        PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
*        PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DDARREDITD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDARREDITD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041FATABSCLR 05/22/17'                                      
         END                                                                    
